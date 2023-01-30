module module_xml

use module_utility
implicit none

public :: operator(.pair.)
public :: assignment(=)
public :: xml_type
public :: attribute_type
public :: characters_type
private

!> Key value type
type :: attribute_type
  character(:), allocatable :: key
  class(*), allocatable :: value
end type attribute_type

!> Character type
type :: character_type
  character(:), allocatable :: value
  integer :: length = 0
end type character_type

!> Characters type
type :: characters_type
  type(character_type), allocatable :: array(:)
  integer :: length = 0
end type characters_type

!> XML type
type :: xml_type
  integer :: unit
  character(:), allocatable :: file
contains
  procedure :: open => open_xml
  procedure :: write_element
  procedure :: write_prolog
  procedure :: write_attribute
  procedure :: close_attribute
  procedure :: close => close_xml
end type xml_type

!> Interface for key value pair.
interface operator(.pair.)
  module procedure :: initiate_attribue
end interface operator(.pair.)

!> Assignment (=)
interface assignment(=)
  module procedure :: initiate_character
  module procedure :: initiate_characters
end interface assignment(=)

contains

!> Initialize a key value pair.
pure function initiate_attribue(key, value) result(ret)
  character(*), intent(in) :: key
  class(*), intent(in) :: value
  type(attribute_type) :: ret

  ret%key = key
  if (allocated(ret%value)) deallocate (ret%value)
  allocate (ret%value, source=value)
end function initiate_attribue

pure subroutine initiate_character(character_, string)
  type(character_type), intent(out) :: character_
  character(*), intent(in) :: string

  character_%value = string
  character_%length = len(string)
end subroutine initiate_character

pure subroutine initiate_characters(characters, array)
  type(characters_type), intent(out) :: characters
  character(*), intent(in) :: array(:)
  integer :: i, n

  allocate (characters%array(size(array)))
  n = 0
  do i = 1, size(array)
    characters%array(i) = trim(array(i))
    n = n + characters%array(i)%length
  end do
  characters%length = n
end subroutine initiate_characters

!> A private function that convert
!> a key-value pair into an allocatable string.
elemental function attribute_to_characters(attribute) result(characters)
  type(attribute_type), intent(in) :: attribute
  type(characters_type) :: characters
  character(:), allocatable :: temp

  if (allocated(characters%array)) &
    & deallocate (characters%array)
  temp = attribute%key//'="'

  select type (x => attribute%value)
  type is (real)
    characters = [temp//str(x)//'"']
  type is (integer)
    characters = [temp//str(x)//'"']
  type is (character(*))
    characters = [temp//trim(x)//'"']
  type is (characters_type)

    block
      integer :: i, n, m

      n = size(x%array)
      m = 0

      allocate (characters%array(n + 2))
      characters%array(1) = temp
      m = m + characters%array(1)%length

      do i = 2, n + 1
        characters%array(i) = &
          & trim(x%array(i - 1)%value)
        m = m + characters%array(i)%length
      end do

      characters%array(n + 2) = '"'
      m = m + characters%array(n + 2)%length

      characters%length = m
    end block

  end select
end function attribute_to_characters

pure function attributes_to_characters(attributes) result(characters)
  type(attribute_type), intent(in) :: attributes(:)
  type(characters_type) :: characters
  type(characters_type), allocatable :: temp(:)
  integer :: i, size_, num_

  size_ = size(attributes)

  select case (size_)
  case (0)
    characters = ['']
  case (1:)

    allocate (temp(size_))
    num_ = 0
    do i = 1, size_
      temp(i) = attribute_to_characters(attributes(i))
      num_ = num_ + temp(i)%length
    end do

    characters%length = num_
    characters%array = [(temp(i)%array, i=1, size_)]

  case default
    error stop 'Invalid attributes.'
  end select
end function attributes_to_characters

pure function characters_to_character(characters) result(message)
  type(characters_type), intent(in) :: characters
  character(:), allocatable :: message
  integer :: len_, size_
  integer :: i, n

  if (allocated(message)) deallocate (message)
  size_ = size(characters%array)
  len_ = characters%length + size_
  allocate (character(len=len_) :: message)

  n = 1
  do i = 1, size_
    message(n:n + characters%array(i)%length) = &
      & " "//characters%array(i)%value
    n = n + characters%array(i)%length + 1
  end do
  message = adjustl(trim(message))
end function characters_to_character

!> XML element
!> example: <tag>text</tag>
pure function xml_element(tag, element) result(ret)
  character(*), intent(in) :: tag
  character(*), intent(in) :: element
  character(:), allocatable :: ret
  integer :: len_
  character(*), parameter :: format_ = &
    & "('<', a, '>', a, '</', a, '>')"

  len_ = 2*len(tag) + len(element) + 5
  if (allocated(ret)) deallocate (ret)
  allocate (character(len=len_) :: ret)
  write (ret, format_) tag, element, tag
end function xml_element

!> XML Prolog
pure function xml_prolog(attributes) result(ret)
  type(attribute_type), intent(in) :: attributes(:)
  character(:), allocatable :: ret
  type(characters_type) :: temp

  if (allocated(ret)) deallocate (ret)
  temp = attributes_to_characters(attributes)
  ret = trim('<?xml '//characters_to_character(temp)//'?>')
end function xml_prolog

!> XML attribute
pure function xml_attribute(tag, attributes, inline) result(message)
  character(*), intent(in) :: tag
  type(attribute_type), intent(in) :: attributes(:)
  logical, intent(in) :: inline
  type(characters_type) :: message
  type(characters_type) :: temp
  integer :: i, n

  select case (size(attributes))
  case (0)

    if (inline) then
      message = ["<"//tag//"/>"]
    else
      message = ["<"//tag//">"]
    end if

  case (1:)

    temp = attributes_to_characters(attributes)
    n = size(temp%array)
    allocate (message%array(n + 2))

    message%array(1) = "<"//tag
    do i = 2, n + 1
      message%array(i) = temp%array(i - 1)
    end do

    if (inline) then
      message%array(n + 2) = "/>"
    else
      message%array(n + 2) = ">"
    end if

    message%length = temp%length + &
      & message%array(1)%length + &
      & message%array(n + 2)%length
  end select
end function xml_attribute

subroutine open_xml(self, file)
  class(xml_type), intent(out) :: self
  character(*), intent(in) :: file

  open (newunit=self%unit, file=file)
end subroutine open_xml

subroutine write_prolog(self, attributes)
  class(xml_type), intent(in) :: self
  type(attribute_type), intent(in) :: attributes(:)

  !> Has to be the first line so no indent.
  write (self%unit, "(a)") xml_prolog(attributes)
end subroutine write_prolog

subroutine write_element(self, tag, text)
  class(xml_type), intent(inout) :: self
  character(*), intent(in) :: tag
  character(*), intent(in) :: text

  write (self%unit, "(a)") xml_element(tag, text)
end subroutine write_element

subroutine write_attribute(self, tag, attributes, inline, line_break)
  class(xml_type), intent(inout) :: self
  character(*), intent(in) :: tag
  type(attribute_type), intent(in) :: attributes(:)
  logical, intent(in), optional :: inline
  logical, intent(in), optional :: line_break
  type(characters_type) :: message
  logical :: inline_, break_
  integer :: i

  if (present(inline)) then
    inline_ = inline
  else
    inline_ = .true.
  end if

  if (present(line_break)) then
    break_ = line_break
  else
    break_ = .true.
  end if

  message = xml_attribute(tag, attributes, inline_)
  if (break_) then
    do i = 1, size(message%array)
      write (self%unit, "(a)") &
        & message%array(i)%value
    end do
  else
    write (self%unit, "(a)") &
      characters_to_character(message)
  end if
end subroutine write_attribute

subroutine close_attribute(self, tag)
  class(xml_type), intent(inout) :: self
  character(*), intent(in) :: tag

  write (self%unit, "(a)") "</"//tag//">"
end subroutine close_attribute

subroutine close_xml(self)
  class(xml_type), intent(in) :: self

  close (self%unit)
end subroutine close_xml

end module module_xml
