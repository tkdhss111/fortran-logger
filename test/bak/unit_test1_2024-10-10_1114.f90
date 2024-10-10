#define MYLOG(x1, x2, x3) call logger.write ( __FILE__, __LINE__, x1, x2, x3 )

program unit_test1
  use logger_mo
  implicit none
  type(logger_ty) :: logger
  integer i, u

  print *, __FILE__, __LINE__
  print *, write_msg ( 'write_msg test *** Error:', 1, '2', .true., -999.0, i  )
  print *, write_macro ( __FILE__, __LINE__, 'write_macro test *** Error:', 1, '2', .true., -999.0, i  )

  ! Test: debuglevel
  call logger.init ( file = 'test.log', debuglevel = 1 )
  call logger.init ( file = 'test.log', debuglevel = 0 )
  call logger.init ( file = 'test.log', debuglevel = 4 )

  ! Macro
  MYLOG( 'Macro test *** Error:', i )

  ! Test: Time stamp
  call sleep(5)
  call logger.write ( __FILE__, __LINE__, '*** Error:', 1, '2', .true., -999.0, i )
  call logger.write ( __FILE__, __LINE__, '*** Warning:', 1, '2', .true., -999.99, i )
  call logger.write ( __FILE__, __LINE__, '*** Debug:', 1, '2', .true., -999.99, i )
  call logger.write ( __FILE__, __LINE__, '*** Info:', 1, '2', 'Yokadesu', -999.99, i )

  ! Test: Execute command line with logger
  call logger.exec ( __FILE__, __LINE__,  'touch exec_test.txt' )
  !call logger.exec ( __FILE__, __LINE__,  'rm non-existed.txt' ) ! will stop

  ! Test: Open file with logger (run twice to cause an error)
  !call logger.exec ( __FILE__, __LINE__, 'rm test.txt' )
  !call logger.open ( __FILE__, __LINE__, u, 'test.txt', status = 'new', access = 'append' ) ! will stop
  !write ( u, '(a)' ) 'Test line'
  !close ( u )

end program
