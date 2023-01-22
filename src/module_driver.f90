module module_driver

use module_logo
implicit none

public :: get_arguments
private

contains

subroutine get_blueprint()
  type(logo_type) :: logo

  call logo%initialize()
  call logo%compute()
  call logo%blueprint(.true.)
  call logo%blueprint(.false.)
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

subroutine get_arguments()
  character(len=500), allocatable :: args(:)
  character(:), allocatable :: ext
  integer :: num_args, logo_size, i

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
    read (args(i + 1), *) logo_size
    allocate (character(len=10) :: ext)
    read (args(i + 2), *) ext
    call get_logo(logo_size, trim(ext))
    return
  end select
  
end subroutine get_arguments

end module module_driver
