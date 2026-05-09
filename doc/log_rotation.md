# Log Rotation

`fortran-logger` rotates log files automatically, in the same style as the
Linux `logrotate` utility. Rotation prevents a single log file from growing
without bound — a runaway log of several gigabytes can fill a disk and
make recent entries hard to read.

This document describes how the rotation mechanism works, when it triggers,
how to configure it, and what files end up on disk.

## TL;DR

By default, every log file managed by this logger is automatically capped:

- **Trigger** — when the file reaches **1 MB**, it rotates on the next write.
- **Backups kept** — **7** historical files (`.log.1` … `.log.7`).
- **Compression** — `.log.2` through `.log.7` are gzipped (delaycompress:
  `.log.1` stays uncompressed for easy `tail` / `less`).
- **Worst-case disk** — roughly `max_size + max_backups × max_size` per log,
  i.e. **~8 MB** per logger instance with the defaults.

No code change is needed in calling applications — rotation is on by default.

## How it works

Each call to `logger%write(...)` performs one extra step before opening the
file in append mode:

1. `inquire(file=this%file, exist=lr_exists, size=lr_size)`
2. If `lr_exists` and `lr_size >= this%max_size`, call `rotate_log_file`.
3. Open the file (now empty after rotation) in `access='append'` mode and
   write the new line.

The `inquire` is a single, cheap syscall. It runs on every write but its
overhead is negligible compared to disk I/O.

The logger uses an **open–append–close** pattern (it does not hold the file
descriptor between writes). This is what makes rename-based rotation safe:
no process is holding `foo.log` open while we rename it, so the rename
takes effect immediately and the next write opens a fresh empty file.

### The rotation algorithm

Given `base = this%file`, `N = this%max_backups`, and `C = this%compress_backups`:

```
1. Drop the oldest backup
       rm -f base.N.gz
       rm -f base.N
2. Shift each backup one slot older (high N to low):
       for i = N-1 down to 1:
           mv -f base.i.gz   →   base.(i+1).gz   (if exists)
           mv -f base.i      →   base.(i+1)      (if exists)
3. Promote the current file:
       mv -f base            →   base.1
4. Compress everything from .2 onward (delaycompress: leave .1 plain):
       for i = 2..N:
           gzip -f base.i    (if exists)
```

Step 4 mirrors `logrotate`'s `delaycompress` behaviour: the most recent
rotated file (`.log.1`) is left uncompressed so that follow-up debugging
(e.g. `tail -f base.log.1`) does not require `zcat`. Older backups are gzipped
to save disk space.

The `mv`/`gzip`/`rm` calls are issued via `execute_command_line` for
portability. Paths are single-quoted (`quote()` helper) so spaces and shell
metacharacters in log paths are handled safely.

### Concurrency note

When multiple coarray images log to the same file, they each call `inquire`
and may decide to rotate at the same instant. In the worst case this can
produce one extra backup shift; the log content is not lost (it is in `.log.1`
or `.log.2`). Because applications using this logger typically set
`FOR_COARRAY_NUM_IMAGES=1` for the logging path, this race is rarely hit in
practice.

## Configuration

All four rotation knobs are fields on `logger_ty`. The defaults are sensible
for production; you can override them at `init` time using keyword
arguments:

```fortran
integer(int64) :: max_size         = 1*1024*1024  ! bytes; default 1 MB
integer        :: max_backups      = 7            ! files .1 .. .N
logical        :: compress_backups = .true.       ! gzip .2..N
logical        :: enable_rotation  = .true.       ! global on/off switch
```

Override at init:

```fortran
use, intrinsic :: iso_fortran_env, only : int64
type(logger_ty) :: logger

call logger%init( file        = '/var/log/myapp.log',  &
                  max_size    = 10_int64*1024*1024,    &  ! 10 MB cap
                  max_backups = 14,                    &  ! 2 weeks daily
                  compress_backups = .true. )
```

To disable rotation entirely (e.g. when running under an external rotator
like `logrotate(8)`):

```fortran
call logger%init( file = 'foo.log', enable_rotation = .false. )
```

To rotate without keeping any history (`/dev/null`-style truncation):

```fortran
call logger%init( file = 'foo.log', max_backups = 0 )
```

When `max_backups <= 0`, the rotation step simply deletes the current file;
no backups are retained.

## Choosing `max_size`

| Scenario | Suggested `max_size` |
|---|---|
| Production server, low log volume, want recent context | **1 MB** (default) |
| Production server, high log volume, willing to lose older history sooner | 1–10 MB |
| Embedded / disk-constrained | 64 KB – 256 KB |
| Debugging with very chatty output | 100 MB+ (or disable rotation) |

The default 1 MB is the **practical minimum** that still keeps useful
context. Go smaller only when disk is tight; go larger only when you really
need long unrotated runs of recent logs.

## Files on disk

After multiple rotations, a typical log directory looks like this:

```
foo.log         ← current, plain text, actively written
foo.log.1       ← previous, plain text (delaycompress)
foo.log.2.gz    ← older, gzipped
foo.log.3.gz
foo.log.4.gz
foo.log.5.gz
foo.log.6.gz
foo.log.7.gz    ← oldest kept; will be deleted on next rotation
```

You can read them with the usual tools:

```bash
tail foo.log          # most recent
tail foo.log.1        # what was rotated out last
zcat foo.log.2.gz     # older, decompress on the fly
zless foo.log.5.gz    # paged with decompression
```

To find the absolute oldest entry kept across all backups:

```bash
zcat -f foo.log.7.gz foo.log.6.gz foo.log.5.gz foo.log.4.gz \
        foo.log.3.gz foo.log.2.gz foo.log.1   foo.log | head -1
```

(`zcat -f` falls back to plain `cat` for non-gzip files.)

## Comparison with logrotate(8)

| Feature | This logger | logrotate(8) |
|---|---|---|
| Trigger | size only (`>= max_size`) | size, time, or both |
| Compression | gzip, delaycompress | gzip/bzip2/xz, delaycompress, dateext |
| Retention | rolling N (default 7) | rolling N, age-based, dateext |
| Service reload | none (open–append per write) | `postrotate` hook (e.g. SIGHUP) |
| Where it runs | inside the application | external, via cron |
| Config | Fortran source (`logger_ty` fields or `init` args) | `/etc/logrotate.d/*` files |

When **both** are configured for the same path, prefer one or the other —
running both can produce duplicate rotations. To delegate to `logrotate(8)`
entirely, pass `enable_rotation = .false.` at init.

## Troubleshooting

**Q: My log keeps growing past `max_size`. Why isn't it rotating?**

- Confirm `enable_rotation = .true.` (the default).
- Ensure a `logger%write(...)` is actually being called — rotation only runs
  inside `write_log`. If your code writes directly to the file unit instead
  of through the logger, rotation is bypassed.
- Check disk permissions: rotation runs `mv` and `gzip` shell commands; if
  the process cannot write to the log directory, rotation will fail
  silently (the original `inquire` is read-only and won't error out).

**Q: I see `.log.8.gz` files. Wasn't the cap supposed to be 7?**

- The cap counts only files that go through the rotation algorithm. If a
  pre-existing rotation left files numbered higher, they are not removed
  retroactively. Delete them by hand or let the next rotation cycles drop
  the orphans (the algorithm shifts `.6 → .7` and then deletes anything at
  `.8` only on the run that creates them).

**Q: Can I disable compression for fast disks?**

- Yes: `compress_backups = .false.`. Backups will be kept as plain
  `.log.1` … `.log.N` files. Useful when CPU is precious or when you want
  to grep across history without `zgrep`.

**Q: How can I trigger a rotation manually for testing?**

- The simplest way is to set a tiny `max_size` (e.g. 1024 bytes) at init,
  then call `logger%write(...)` enough times to exceed it. The next write
  after the threshold will rotate.

## Implementation pointers

- All rotation logic lives in `src/logger_mo.f90`:
  - `subroutine rotate_log_file(base, max_backups, compress_backups)` — the algorithm.
  - `pure function quote(s)` — single-quotes shell paths safely.
  - The size check is at the top of `subroutine write_log`, gated by
    `this%enable_rotation` and `this%max_size > 0`.
- The rotation test in `test/unit_test.f90` exercises the logic end-to-end
  with `max_size = 200` bytes and `max_backups = 3`. Run it with
  `make test` from the repo root.
