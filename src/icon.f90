program main

use module_geometry
use module_icon
implicit none

type(icon_type) :: icon

call icon%read_parameters()
call icon%compute_curve()
call icon%blue_print()
call icon%blue_print(dark_mode=.false.)

end program main
