program main

use module_geometry
use module_logo
implicit none

type(logo_type) :: logo

call logo%initialize('parameters.nml')
call logo%compute()
call logo%blueprint()
call logo%blueprint(dark_mode=.false.)
call logo%draw(256)

end program main
