module module_driver

use module_logo
implicit none

public :: get_arguments
private

contains

subroutine get_blueprint()
  type(logo_type) :: logo

  call logo%initialize('./nml/parameters.nml')
  call logo%compute()
  call logo%blueprint()
end subroutine get_blueprint

subroutine get_arguments()
  character(len=500), allocatable :: args(:)
  integer :: num_args, i
  type(logo_type) :: logo

  num_args = command_argument_count()
  if (num_args >= 9 .or. num_args < 1) then
    error stop "Invalid argument count!"
  end if

  allocate (args(num_args))
  do i = 1, num_args
    call get_command_argument(i, args(i))
  end do

  i = 1
  select case (trim(args(i)))
  case ("-b", "--blueprint")
    call get_blueprint()
    return
  case ("-l", "--logo")
    call logo%initialize('./nml/parameters.nml')
    call logo%compute()
    call logo%draw()
    return
  end select

end subroutine get_arguments

end module module_driver
