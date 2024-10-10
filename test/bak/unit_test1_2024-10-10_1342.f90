#include "macro.fi"

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

  TEST('*** Debug')

  ! Test: Time stamp
  call sleep(1)
  call logger.write ( __FILE__, __LINE__, '*** Error:', 1, '2', .true., -999.0, i )
  call logger.write ( __FILE__, __LINE__, '*** Warning:', 1, '2', .true., -999.99, i )
  call logger.write ( __FILE__, __LINE__, '*** Debug:', 1, '2', .true., -999.99, i )
  call logger.write ( __FILE__, __LINE__, '*** Info:', 1, '2', 'Yokadesu', -999.99, i )

  ! Macro for only one argument
  FATAL( 'Macro test: fatal' )
  ERROR( 'Macro test: error' )
  WARNING( 'Macro test: warning' )
  DEBUG( 'Macro test: debug' )
  INFO( 'Macro test: info' )

  ! Test: Execute command line with logger
  call logger.exec ( __FILE__, __LINE__,  'touch exec_test.txt' )
  !call logger.exec ( __FILE__, __LINE__,  'rm non-existed.txt' ) ! will stop

  ! Test: Open file with logger (run twice to cause an error)
  !call logger.exec ( __FILE__, __LINE__, 'rm test.txt' )
  !call logger.open ( __FILE__, __LINE__, u, 'test.txt', status = 'new', access = 'append' ) ! will stop
  !write ( u, '(a)' ) 'Test line'
  !close ( u )

end program
