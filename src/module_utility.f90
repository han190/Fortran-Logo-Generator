module module_utility

public :: str
public :: write_strs
private

interface str
  module procedure :: real_to_str
  module procedure :: int_to_str
end interface str

contains

subroutine write_strs(unit, messages)
  integer, intent(in) :: unit
  character(*), intent(in) :: messages(:)
  integer :: i

  do i = 1, size(messages)
    write (unit, "(a)") trim(messages(i))
  end do
end subroutine write_strs

elemental function num_digits(val) result(ret)
  integer, intent(in) :: val
  integer :: ret

  ret = floor(log10(real(val))) + 1
end function num_digits

pure function int_to_str(val) result(ret)
  integer, intent(in) :: val
  character(:), allocatable :: ret

  allocate (character(len=num_digits(val)) :: ret)
  write (ret, "(i0)") val
end function int_to_str

pure function real_to_str(val) result(ret)
  real, intent(in) :: val
  character(:), allocatable :: ret

  allocate (character(len=100) :: ret)
  write (ret, "(f0.6)") val
  ret = trim(ret)
end function real_to_str

end module module_utility
