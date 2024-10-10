program unit_test1
  use logger_mo
  implicit none
  type(logger_ty) :: logger
  integer i, u

  print *, __FILE__, __LINE__

  ! Test: debuglevel
  call logger.init ( file = 'test.log', debuglevel = 1 )
  call logger.init ( file = 'test.log', debuglevel = 0 )
  call logger.init ( file = 'test.log', debuglevel = 4 )

  ! Macro
 ! LOG( 'Macro test *** Error:', 1, '2', .true., -999.0, i )

  ! Test: Time stamp
  call sleep(5)
  call logger.write ( __FILE__, __LINE__, '*** Error:', 1, '2', .true., -999.0, i )

  call logger.write ( __FILE__, __LINE__, '*** Warning:', 1, '2', .true., -999.99, i )
  call logger.write ( __FILE__, __LINE__, '*** Debug:', 1, '2', .true., -999.99, i )
  call logger.write ( __FILE__, __LINE__, '*** Info:', 1, '2', 'Yokadesu', -999.99, i )

  ! Test: Execute command line with logger
  call logger.exec ( __FILE__, __LINE__,  'touch exec_test.txt' )
  call logger.exec ( __FILE__, __LINE__,  'rm non-existed.txt' )

  ! Test: Arguments
  i = 3
  call logger.write ( __FILE__, __LINE__, '*** Fatal:', 1, '2', .true., 8.0, i )

  ! Test: Open file with logger (run twice to cause an error)
  !call exec ( 'rm test.txt' )
  call logger.open ( __FILE__, __LINE__, u, 'test.txt', status = 'new', access = 'append' ) 
  write ( u, '(a)' ) 'Test line'
  close ( u )

end program
