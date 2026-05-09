module logger_mo

  use, intrinsic :: iso_fortran_env, only : stdin  => input_unit,  &
                                            stdout => output_unit, &
                                            stderr => error_unit,  &
                                            int64
  implicit none
  private
  public :: logger_ty, paste

  type logger_ty
    character(255) :: file        = 'fortran-logger.log' ! Log file path
    character(255) :: app         = 'MyApp' ! Application name
    character(255) :: email       = 'NA'    ! Email address
    character(255) :: args        = 'NA'    ! Debug arguments
    logical        :: colored     = .false. ! Use ANSI terminal colors
    integer        :: debuglevel  = 1       ! Debug level (0: No logging)
    integer        :: this_image  = 1       ! Coarray image id number
    integer        :: num_images  = 1       ! Coarray number of images
    logical        :: print_image = .false. ! Coarray print
    ! Log rotation (logrotate-style: rotate when file >= max_size)
    integer(int64) :: max_size         = 1_int64*1024_int64*1024_int64 ! 1 MB (practical minimum)
    integer        :: max_backups      = 7       ! file.log.1 .. file.log.7
    logical        :: compress_backups = .true.  ! gzip .2..N (delaycompress)
    logical        :: enable_rotation  = .true.  ! disable for tests/debug
  contains
    procedure :: init  => init_logger
    procedure :: write => write_log
    procedure :: open  => open_file_with_logger
    procedure :: exec  => execute_with_logger
  end type

contains

  pure function paste ( x1, x2, x3, x4, x5, x6, x7, x8, x9, x10 ) result ( args )

    class(*), optional, intent(in) :: x1, x2, x3, x4, x5, x6, x7, x8, x9, x10
    character(:), allocatable      :: c1, c2, c3, c4, c5, c6, c7, c8, c9, c10
    character(:), allocatable      :: args

    c1  = write_x( x1  )
    c2  = write_x( x2  )
    c3  = write_x( x3  )
    c4  = write_x( x4  )
    c5  = write_x( x5  )
    c6  = write_x( x6  )
    c7  = write_x( x7  )
    c8  = write_x( x8  )
    c9  = write_x( x9  )
    c10 = write_x( x10 )

    args = trim(c1)//' '//trim(c2)//' '//trim(c3)//' '//trim(c4)//' '//trim(c5)//' '//&
      trim(c6)//' '//trim(c7)//' '//trim(c8)//' '//trim(c9)//' '//trim(c10)
  end function

  pure elemental function write_x ( x ) result ( c )
    class(*), intent(in), optional :: x
    character(255)                 :: c
    c = ''
    if ( .not. present( x ) ) return
    select type ( y => x )
      type is ( character(*) )
        c = y
      type is ( logical )
        write ( c, '(l)' ) y
      type is ( integer )
        write ( c, '(i0)' ) y
      type is ( real )
        write ( c, '(f7.2)' ) y
      class default
        c = '(Unknown type)'
    end select
  end function

  subroutine open_file_with_logger ( this, file_macro, line_macro, newunit, file, status, access, form )

    class(logger_ty), intent(inout)        :: this
    character(*),     intent(in)           :: file_macro
    integer,          intent(in)           :: line_macro
    integer,          intent(out)          :: newunit
    character(*),     intent(in)           :: file
    character(*),     intent(in), optional :: status
    character(*),     intent(in), optional :: access
    character(*),     intent(in), optional :: form
    character(20)                          :: status_
    character(20)                          :: access_
    character(20)                          :: form_
    character(200)                         :: iomsg
    integer                                :: iostat

    if ( present( status ) ) then
      status_ = trim(status)
    else
      status_ = 'unknown'
    end if

    if ( present( access ) ) then
      access_ = trim(access)
    else
      access_ = 'sequential'
    end if

    if ( present( form ) ) then
      form_ = trim(form)
    else
      form_ = 'formatted'
    end if

    open ( newunit = newunit, &
      file = trim(file), status = trim(status_), access = trim(access_), form = trim(form_), &
      iomsg = iomsg, iostat = iostat )

    if ( iostat /= 0 ) then
      call this%write( file_macro, line_macro, '*** Erorr:', trim(iomsg) )
      stop '*** Erorr: '//trim(iomsg)
    end if

  end subroutine

  subroutine init_logger ( this, file, app, email, colored, debuglevel, this_image, num_images, &
                           max_size, max_backups, compress_backups, enable_rotation )

    class(logger_ty),         intent(inout) :: this
    character(*),   optional, intent(in)  :: file
    character(*),   optional, intent(in)  :: app
    character(*),   optional, intent(in)  :: email
    integer,        optional, intent(in)  :: this_image, num_images
    logical,        optional, intent(in)  :: colored
    integer,        optional, intent(in)  :: debuglevel
    integer(int64), optional, intent(in)  :: max_size
    integer,        optional, intent(in)  :: max_backups
    logical,        optional, intent(in)  :: compress_backups
    logical,        optional, intent(in)  :: enable_rotation

    if ( present( file ) ) then
      this%file = trim( file )
    else
      this%file = './logger.log'
    end if

    if ( present( app ) ) then
      this%app = trim(app)
    end if

    if ( present( email ) ) then
      this%email = trim(email)
    end if

    if ( present( colored ) ) then
      this%colored = colored
    end if

    if ( present( debuglevel ) ) then
      this%debuglevel = debuglevel
    end if

    if ( present( this_image ) .and. present( num_images ) ) then
      this%print_image = .true.
      this%this_image  = this_image
      this%num_images  = num_images
    end if

    if ( present( max_size         ) ) this%max_size         = max_size
    if ( present( max_backups      ) ) this%max_backups      = max_backups
    if ( present( compress_backups ) ) this%compress_backups = compress_backups
    if ( present( enable_rotation  ) ) this%enable_rotation  = enable_rotation

    ! Skip file truncation to avoid potential coarray issues
    ! (Rotation handles size; init no longer truncates)

    call this%write ( __FILE__, __LINE__, '*** Info: file = ',       this%file       )
    call this%write ( __FILE__, __LINE__, '*** Info: email = ',      this%email      )
    call this%write ( __FILE__, __LINE__, '*** Info: colored = ',    this%colored    )
    call this%write ( __FILE__, __LINE__, '*** Info: debuglevel = ', this%debuglevel )

  end subroutine

  subroutine write_log ( this, file, line, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10 )

    class(logger_ty),   intent(inout) :: this
    character(*),       intent(in)    :: file
    integer,            intent(in)    :: line
    class(*), optional, intent(in)    :: x1, x2, x3, x4, x5, x6, x7, x8, x9, x10
    character(255)                    :: prefix
    character(20)                     :: cimage = ''
    character(:), allocatable         :: args, args_ansi
    character(8)                      :: date
    character(10)                     :: time
    character(19)                     :: datetime
    character(1000)                   :: iomsg, cmdmsg
    integer u, iostat, cmdstat, exitstat
    integer i

    ! ANSI Console Colors
    !character(7) :: BLACK   = achar(27)//'[1;40m'
    character(7) :: RED     = achar(27)//'[1;41m'
    character(7) :: GREEN   = achar(27)//'[1;42m'
    character(7) :: YELLOW  = achar(27)//'[1;43m'
    character(7) :: BLUE    = achar(27)//'[1;44m'
    !character(7) :: MAGENTA = achar(27)//'[1;45m'
    character(7) :: CYAN    = achar(27)//'[1;46m'
    !character(7) :: WHITE   = achar(27)//'[1;47m'
    character(4) :: CLEAR   = achar(27)//'[0m'

    if ( this%debuglevel < 1 ) return

    !
    ! Timestamp
    !
    call date_and_time( date, time )
    datetime = date(1:4)//'-'//date(5:6)//'-'//date(7:8)//' '//&
               time(1:2)//':'//time(3:4)//':'//time(5:6)

    !
    ! Coarray Images
    !
    if ( this%print_image ) then
      write ( cimage, '("[", i0, "/", i0, "]")' ) this%this_image, this%num_images
    end if

    !
    ! Prefix
    !
    write ( prefix, '(a, i0, a1)' ) &
      '['//datetime//']'//trim(cimage)//'['//trim(basename(file))//':', line, ']'
      !'['//trim(this%app)//']'//'['//datetime//']'//trim(cimage)//'['//trim(basename(file))//':', line, ']'

    args = paste( x1, x2, x3, x4, x5, x6, x7, x8, x9, x10 )

    args_ansi = args

    !
    ! Keyword Coloring
    !
    if ( index( args, 'Log' ) > 0 ) then
      if ( this%colored ) then
        args_ansi = CYAN//trim(args)//CLEAR
      end if
      if ( this%debuglevel >= 1 ) then
        write ( stderr, '(a)' ) trim(prefix)//' '//trim(args_ansi)
      end if
    end if

    if ( index( args, 'Error' ) > 0 .or. index( args, 'Fatal' ) > 0 ) then
      if ( this%colored ) then
        args_ansi = RED//trim(args)//CLEAR
      end if
      if ( this%debuglevel >= 1 ) then
        write ( stderr, '(a)' ) trim(prefix)//' '//trim(args_ansi)
      end if
    end if

    if ( index( args, 'Warn' ) > 0 ) then
      if ( this%colored ) then
        args_ansi = YELLOW//trim(args)//CLEAR
      end if
      if ( this%debuglevel >= 2 ) then
        write ( stderr, '(a)' ) trim(prefix)//' '//trim(args_ansi)
      end if
    end if

    if ( index( args, 'Debug' ) > 0 ) then
      if ( this%colored ) then
        args_ansi = GREEN//trim(args)//CLEAR
      end if
      if ( this%debuglevel >= 3 ) then
        write ( stderr, '(a)' ) trim(prefix)//' '//trim(args_ansi)
      end if
    end if

    if ( index( args, 'Info' ) > 0 ) then
      if ( this%colored ) then
        args_ansi = BLUE//trim(args)//CLEAR
      end if
      if ( this%debuglevel >= 4 ) then
        write ( stderr, '(a)' ) trim(prefix)//' '//trim(args_ansi)
      end if
    end if

    !critical ! N.B. Place critical before email, or multiple email will be sent.

    !
    ! Email Sending
    !
    i = index( args, 'sendmail' )
    if ( i > 0 .and. this%email /= 'NA' ) then
      if ( this%email == 'NA' ) then
        write ( stderr, '(a)' ) '*** Erorr: Email address is required for sending email.'
      else
        args = args(1:i-1)//args(i+8:)
        call execute_command_line( 'echo "'//trim(args)//        &
             '" | neomutt -s "'//trim(prefix)//trim(args)//      &
             '" -i '//trim(this%file)//' -- '//trim(this%email), &
          exitstat = exitstat, &
          cmdstat  = cmdstat,  &
          cmdmsg   = cmdmsg)
        if ( exitstat /= 0 ) then
          write ( stderr, '(a, i0, a, i0, a)' ) &
            '*** Error: exitstat=', exitstat, ', cmdstat=', cmdstat, ', cmdmsg: '//trim(cmdmsg)
        end if
      end if
    end if

    !
    ! Log Rotation (logrotate-style, size-based)
    !
    if ( this%enable_rotation .and. this%max_size > 0_int64 ) then
      block
        logical        :: lr_exists
        integer(int64) :: lr_size
        lr_size = -1_int64
        inquire( file = trim(this%file), exist = lr_exists, size = lr_size )
        if ( lr_exists .and. lr_size >= this%max_size ) then
          call rotate_log_file( trim(this%file), this%max_backups, this%compress_backups )
        end if
      end block
    end if

    !
    ! Write Logging Message
    !
    open ( newunit = u, file = this%file, access = 'append', iomsg = iomsg, iostat = iostat )
    if ( iostat /= 0 ) then
      write ( stderr, '(a)' ) trim(iomsg)
      return
    end if
    write ( u, '(a)' ) trim(prefix)//' '//trim(args)
    close ( u )

    !end critical

  end subroutine

  subroutine rotate_log_file ( base, max_backups, compress_backups )
    !
    ! Logrotate-style rotation:
    !   delete  base.<N>[.gz]
    !   rename  base.<N-1>[.gz] -> base.<N>[.gz]   for N = max_backups..2
    !   rename  base            -> base.1
    !   gzip    base.2 .. base.<N>                 (delaycompress: .1 stays plain)
    !
    character(*), intent(in) :: base
    integer,      intent(in) :: max_backups
    logical,      intent(in) :: compress_backups

    character(20) :: csrc, cdst
    character(:), allocatable :: src, dst
    integer :: i
    logical :: exists

    if ( max_backups <= 0 ) then
      ! No backups kept: just delete the current file
      call execute_command_line( 'rm -f -- '//quote(base), wait = .true. )
      return
    end if

    ! Step 1: drop the oldest backup (compressed or plain)
    write ( cdst, '(i0)' ) max_backups
    call execute_command_line( 'rm -f -- '//quote(base//'.'//trim(cdst)//'.gz'), wait = .true. )
    call execute_command_line( 'rm -f -- '//quote(base//'.'//trim(cdst)),        wait = .true. )

    ! Step 2: shift backwards .N-1 -> .N (handle both plain and .gz forms)
    do i = max_backups - 1, 1, -1
      write ( csrc, '(i0)' ) i
      write ( cdst, '(i0)' ) i + 1
      src = base//'.'//trim(csrc)//'.gz'
      dst = base//'.'//trim(cdst)//'.gz'
      inquire( file = src, exist = exists )
      if ( exists ) call execute_command_line( 'mv -f -- '//quote(src)//' '//quote(dst), wait = .true. )
      src = base//'.'//trim(csrc)
      dst = base//'.'//trim(cdst)
      inquire( file = src, exist = exists )
      if ( exists ) call execute_command_line( 'mv -f -- '//quote(src)//' '//quote(dst), wait = .true. )
    end do

    ! Step 3: rename current file to .1
    inquire( file = base, exist = exists )
    if ( exists ) then
      call execute_command_line( 'mv -f -- '//quote(base)//' '//quote(base//'.1'), wait = .true. )
    end if

    ! Step 4: compress .2 .. .max_backups (delaycompress: leave .1 uncompressed)
    if ( compress_backups ) then
      do i = 2, max_backups
        write ( csrc, '(i0)' ) i
        src = base//'.'//trim(csrc)
        inquire( file = src, exist = exists )
        if ( exists ) then
          call execute_command_line( 'gzip -f -- '//quote(src), wait = .true. )
        end if
      end do
    end if

  end subroutine

  pure function quote ( s ) result ( q )
    ! Single-quote a path for safe shell interpolation.
    ! Embedded single quotes are escaped as '\'' (close, escaped, reopen).
    character(*), intent(in)  :: s
    character(:), allocatable :: q
    integer :: i
    q = "'"
    do i = 1, len_trim(s)
      if ( s(i:i) == "'" ) then
        q = q//"'\''"
      else
        q = q//s(i:i)
      end if
    end do
    q = q//"'"
  end function

  subroutine execute_with_logger ( this, file_macro, line_macro, cmd, stat )

    class(logger_ty),  intent(inout) :: this
    character(*),      intent(in)    :: file_macro
    integer,           intent(in)    :: line_macro
    character(*),      intent(in)    :: cmd
    integer, optional, intent(inout) :: stat
    integer                          :: cmdstat, exitstat
    character(1000)                  :: cmdmsg

    cmdmsg = 'NA'

    call execute_command_line( trim(cmd), exitstat = exitstat, cmdstat = cmdstat, cmdmsg = cmdmsg )

    if ( cmdstat > 0 ) then ! Command execution failed with error
      call this%write ( file_macro, line_macro, &
      '*** Error: cmdstat=', cmdstat, ', cmdmsg:', trim(cmdmsg), ', Command:', trim(cmd) )
      if ( present( stat ) ) then
         stat = cmdstat
         return
      else
        stop 1
      end if
    else if ( cmdstat < 0 ) then ! Command execution not supported
      call this%write ( file_macro, line_macro, &
        '*** Error: cmdstat=', cmdstat, ', cmdmsg:', trim(cmdmsg), ', Command:', trim(cmd) )
      if ( present( stat ) ) then
         stat = cmdstat
         return
      else
        stop 1
      end if
    else ! Command successfully completed with cmdstat == 0
      if ( exitstat /= 0 ) then ! Command completed with non-zero exitstat
        call this%write ( file_macro, line_macro, &
        '*** Error: exitstat=', exitstat, ', cmdstat=0', ', cmdmsg:', trim(cmdmsg), ', Command:', trim(cmd) )
        if ( present( stat ) ) then
           stat = exitstat
           return
        else
          stop 1
        end if
      else
        call this%write ( file_macro, line_macro, '*** Info: Command (successful): ', trim(cmd) )
        if ( present(stat) ) stat = 0
      end if
    end if

  end subroutine

  pure elemental character(255) function basename ( path )
    character(*), intent(in) :: path
    integer                  :: p_sep, p_comma
    p_sep   = index(path, '/', back = .true.)
    p_comma = index(path, '.', back = .true.)
    basename = path(p_sep + 1:p_comma - 1)
  end function

end module
