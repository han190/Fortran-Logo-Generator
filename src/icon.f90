program main

use module_geometry
use module_logo
implicit none

type(logo_type) :: logo
integer :: i

call logo%read('parameters.nml')
call logo%compute()
call logo%blueprint()
call logo%blueprint(dark_mode=.false.)

i = 160
do while (i <= 1000)
  call logo%draw(i)
  i = i*2
end do

print *, centroid(logo%letter_F)

end program main
