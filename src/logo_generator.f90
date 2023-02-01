program main

use module_logo
implicit none
call dirver()

contains

subroutine dirver()
  character(len=500), allocatable :: arg
  integer :: num_args
  type(logo_type) :: logo
  integer :: unit
  character(:), allocatable :: file

  num_args = command_argument_count()
  if (num_args >= 2) error stop "Invalid argument!"

  if (num_args == 0) then
    file = './nml/parameters.nml'
  else
    call get_command_argument(1, arg)
    file = trim(arg)
  end if

  open (newunit=unit, file='./nml/parameters.nml')
  read (unit, *) logo
  close (unit)
  call draw(logo)
end subroutine dirver

end program main
