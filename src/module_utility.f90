module module_utility

public :: str
public :: write_strs
private

contains

elemental function num_digits(value) result(ret)
  integer, intent(in) :: value
  integer :: ret

  ret = floor(log10(real(value))) + 1
end function num_digits

pure function int2char(value) result(ret)
  integer, intent(in) :: value
  character(:), allocatable :: ret

  allocate (character(len=num_digits(value)) :: ret)
  write (ret, "(i0)") value
end function int2char

pure function str(value, width, decimal) result(ret)
  class(*), intent(in) :: value
  integer, intent(in), optional :: width
  integer, intent(in), optional :: decimal
  character(:), allocatable :: ret
  integer :: length_
  character(:), allocatable :: format_
  character(:), allocatable :: width_
  character(:), allocatable :: decimal_

  select type (value_ => value)
  type is (integer)

    if (present(width) .and. &
      & present(decimal)) then

      width_ = int2char(width)
      decimal_ = int2char(decimal)
      format_ = "(i"//width_//"."//decimal_//")"
      length_ = width

    else if (present(width) .and. &
      & .not. present(decimal)) then

      width_ = int2char(width)
      format_ = "(i"//width_//")"
      length_ = width

    else if (.not. present(width) .and. &
      & .not. present(decimal)) then

      format_ = "(i0)"
      length_ = num_digits(value_)

    else
      error stop "Invalid width."
    end if

    allocate (character(len=length_) :: ret)
    write (ret, format_) value_

  type is (real)

    if (present(width) .and. &
      & present(decimal)) then

      width_ = int2char(width)
      decimal_ = int2char(decimal)
      format_ = "(f"//width_//"."//decimal_//")"
      length_ = width

    else if (.not. present(width) .and. &
      & present(decimal)) then

      decimal_ = int2char(decimal)
      format_ = "(f0."//decimal_//")"
      length_ = 80

    else if (.not. present(width) .and. &
      & .not. present(decimal)) then

      decimal_ = int2char(6)
      format_ = "(f0."//decimal_//")"
      length_ = 80

    else
      error stop "Invalid width."
    end if

    allocate (character(len=length_) :: ret)
    write (ret, format_) value_
    ret = trim(ret)

  class default
    error stop "Invalid type"
  end select

end function str

subroutine write_strs(unit, messages)
  integer, intent(in) :: unit
  character(*), intent(in) :: messages(:)
  integer :: i

  do i = 1, size(messages)
    write (unit, "(a)") trim(messages(i))
  end do
end subroutine write_strs

end module module_utility
