module logger_mo
  use, intrinsic :: iso_fortran_env, only : stdin  => input_unit, &
                                            stdout => output_unit, &
                                            stderr => error_unit
  implicit none
  private
  public :: logger_ty, exec

  type logger_ty
    character(255) :: file  = 'NA'
    character(255) :: email = 'NA'
    character(255) :: msg
    integer        :: debuglevel = 1 ! 0: No logging
    integer        :: id
  contains
    procedure :: init  => init_logger
    procedure :: write => write_log
    procedure :: open  => open_file_with_logger
  end type

contains

  subroutine open_file_with_logger ( this, file_macro, line_macro, u, file, status, access )

    class(logger_ty), intent(inout)        :: this
    character(20),    intent(in)           :: file_macro
    integer,          intent(in)           :: line_macro
    integer,          intent(out)          :: u
    character(*),     intent(in)           :: file
    character(*),     intent(in), optional :: status
    character(*),     intent(in), optional :: access
    character(20)                          :: status_
    character(20)                          :: access_
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
    open( newunit = u, &
      file = trim(file), status = trim(status_), access = trim(access_), &
      iomsg = iomsg, iostat = iostat )
    if ( iostat /= 0 ) then
      call this.write ( file_macro, line_macro, '*** Erorr on opening file:', trim(iomsg) )
      error stop 1
    end if
  end subroutine

  subroutine init_logger ( this, file, email, debuglevel )
    class(logger_ty),       intent(out) :: this
    character(*), optional, intent(in)  :: file
    character(*), optional, intent(in)  :: email
    integer,      optional, intent(in)  :: debuglevel
    if ( this_image() == 1 ) then
      call exec ( 'rm -f '//trim(file) )
    end if
    if ( present( file ) ) then
      this.file = trim(file)
    end if
    if ( present( email ) ) then
      this.email = trim(email)
    end if
    if ( present( debuglevel ) ) then
      this.debuglevel = debuglevel
    end if
  end subroutine

  subroutine write_log ( this, file, line, x1, x2, x3, x4, x5, x6 )

    class(logger_ty), intent(inout)        :: this
    character(*),     intent(in)           :: file
    integer,          intent(in)           :: line
    class(*),         intent(in), optional :: x1, x2, x3, x4, x5, x6
    character(:), allocatable              :: c1, c2, c3, c4, c5, c6
    character(255)                         :: prefix
    character(20)                          :: cimage
    character(:), allocatable              :: msg, msg_ansi
    character(8)   :: date
    character(10)  :: time
    character(19)  :: datetime
    character(255) iomsg
    integer u, iostat
    !character(7) :: BLACK   = achar(27)//'[1;40m'
    character(7) :: RED     = achar(27)//'[1;41m'
    character(7) :: GREEN   = achar(27)//'[1;42m'
    character(7) :: YELLOW  = achar(27)//'[1;43m'
    character(7) :: BLUE    = achar(27)//'[1;44m'
    !character(7) :: MAGENTA = achar(27)//'[1;45m'
    !character(7) :: CYAN    = achar(27)//'[1;46m'
    !character(7) :: WHITE   = achar(27)//'[1;47m'
    character(4) :: CLEAR   = achar(27)//'[0m'

    if ( this.debuglevel < 1 ) return

    call date_and_time ( date, time )
    datetime = date(1:4)//'-'//date(5:6)//'-'//date(7:8)//' '//time(1:2)//':'//time(3:4)//':'//time(5:6)

    if ( num_images() > 1 ) then
      write ( cimage, '("[", i3, "/", i3, "]")' ) this_image(), num_images()
    else
      cimage = ''
    end if

    write ( prefix, '(a, a15, a1, i4, a1)' ) '['//datetime//']'//trim(cimage)//'[', trim(basename(file)), ':', line, ']'

    c1 = write_x ( x1 )
    c2 = write_x ( x2 )
    c3 = write_x ( x3 )
    c4 = write_x ( x4 )
    c5 = write_x ( x5 )
    c6 = write_x ( x6 )

    msg = trim(c1)//' '//trim(c2)//' '//trim(c3)//' '//trim(c4)//' '//trim(c5)//' '//trim(c6)

    if ( index( msg, 'Error' ) > 0  .or. index( msg, 'Fatal' ) > 0 ) then
      msg_ansi = RED//trim(msg)//CLEAR
    end if

    if ( index( msg, 'Warn' ) > 0 ) then
      msg_ansi = YELLOW//trim(msg)//CLEAR
    end if

    if ( index( msg, 'Debug' ) > 0 ) then
      msg_ansi = GREEN//trim(msg)//CLEAR
    end if

    if ( index( msg, 'Info' ) > 0 ) then
      msg_ansi = BLUE//trim(msg)//CLEAR
    end if

    critical ! N.B. Place critical before email, or multiple email will be sent.

    !write ( stderr, * ) trim(prefix)//' '//trim(msg_ansi)

    if ( this.file /= 'NA' ) then
      open ( newunit = u, file = this.file, access = 'append', iomsg = iomsg, iostat = iostat ) 
      if ( iostat /= 0 ) then
        write (*, *) trim(iomsg)
      end if
      write ( u, * ) trim(prefix)//' '//trim(msg)
      close ( u )
    end if

    if ( this.email /= 'NA' ) then
      call exec ( 'echo "'//trim(msg)//&
        '" | neomutt -s "[fortran-logger]'//trim(prefix)//'" '//trim(this.email) )
    end if

    end critical

  end subroutine

  subroutine exec ( cmd, u )
    character(*)                  :: cmd
    integer, intent(in), optional :: u
    integer                       :: u_
    integer                       :: cmdstat, exitstat
    character(255)                :: cmdmsg
    if ( present (u) ) then
      u_ = u
    else
      u_ = stderr
    end if
    call execute_command_line (trim(cmd),&
      exitstat = exitstat, &
      cmdstat  = cmdstat,  &
      cmdmsg   = cmdmsg)
    if ( cmdstat > 0 ) then
      write ( u_, * ) '******************************************************' 
      write ( u_, * ) 'Command: '
      write ( u_, * ) trim(cmd)
      write ( u_, * ) 'Command execution failed with error: '
      write ( u_, * ) trim(cmdmsg)
      write ( u_, * ) '******************************************************' 
      error stop 1
    else if ( cmdstat < 0 ) then
      write ( u_, * ) '******************************************************'
      write ( u_, * ) 'Command: '
      write ( u_, * ) trim(cmd)
      write ( u_, * ) 'Command execution not supported'
      write ( u_, * ) '******************************************************'
      error stop 1
    else ! cmdstat == 0
      !write ( u_, * ) '******************************************************'
      !write ( u_, * ) trim(cmd)
      !write ( u_, * ) "Command successfully completed with status ", exitstat
      !write ( u_, * ) '******************************************************'
      if ( exitstat /= 0 ) then 
        write ( u_, * ) '******************************************************'
        write ( u_, * ) 'Command: '
        write ( u_, * ) trim(cmd)
        write ( u_, * ) "Command completed with status ", exitstat
        write ( u_, * ) '******************************************************'
        error stop 1
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

  pure elemental function write_x ( x ) result ( c )
    class(*), intent(in), optional :: x
    character(255)                 :: c
    c = ''
    if ( .not. present( x ) ) return
    select type ( y => x )
      type is (character(*))
        c = y
      type is (logical)
        write ( c, '(l)' ) y
      type is (integer)
        write ( c, '(i0)' ) y
      type is (real)
        write ( c, '(f7.2)' ) y
      class default
        c = '(Unknown type)'
    end select
  end function

end module
