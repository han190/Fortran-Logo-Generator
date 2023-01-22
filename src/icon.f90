program main

use module_driver
implicit none

call get_blueprint(.false.)
call get_blueprint(.true.)
call get_logo(256, 'svg')
call get_logo(256, 'png')

end program main
