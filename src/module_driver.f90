module module_driver

use module_logo
implicit none

contains

  subroutine get_blueprint(dark_mode)
    logical, intent(in) :: dark_mode
    type(logo_type) :: logo

    call logo%initialize()
    call logo%compute()
    call logo%blueprint(dark_mode)
  end subroutine get_blueprint

  subroutine get_logo(size, ext)
    integer, intent(in) :: size
    type(logo_type) :: logo
    character(*), intent(in) :: ext
    character(*), parameter :: file = 'parameters.nml'

    call logo%initialize(file)
    call logo%compute()
    call logo%draw(size, ext)
  end subroutine

end module module_driver