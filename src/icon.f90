program main

use module_geometry
use module_logo
implicit none

type(logo_type) :: logo
integer :: i

call logo%read_parameters('parameters.nml')
call logo%compute_curve()
call logo%blueprint()
call logo%blueprint(dark_mode=.false.)

i = 64
do while (i <= 2048)
  call logo%draw(i)
  i = i*2
end do

end program main
