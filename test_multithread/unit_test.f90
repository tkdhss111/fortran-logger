#include "macro.fi"

program unit_test

  use logger_mo

  implicit none

  type(logger_ty) :: logger

  call logger%init ( file = 'test.log', this_image = this_image(), num_images = num_images() )

  ! Macro for an argument
  __LOG__( 'Macro test: log' )
  __FATAL__( 'Macro test: fatal' )
  __ERROR__( 'Macro test: error' )
  __WARNING__( 'Macro test: warning' )
  __DEBUG__( 'Macro test: debug' )
  __INFO__( 'Macro test: info' )

end program
