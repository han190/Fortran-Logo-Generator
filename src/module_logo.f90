module module_logo

use module_geometry
use module_xml
use module_utility
implicit none

!> Icon type
type :: logo_type
  !> Number of points used when
  !> drawing curves.
  integer :: num_curves
  integer :: num_rounded
  real :: rounded_radius
  !> Side length of the logo.
  !> (Assuming square canvas)
  real :: side_length(2)
  !> Radius of rounded corners.
  real :: corner(4)
  !> A reference point
  type(point_type) :: reference_point
  !> Offset for hook of "F"
  real :: hook_offset
  !> Essential horiozntal coordinates.
  real, allocatable :: x(:, :)
  !> Essential vertical coordinates
  real, allocatable :: y(:, :)
  !> Piles for letter "F"
  type(point_type), allocatable :: letter_piles(:)
  !> The letter "F"
  type(point_type), allocatable :: letter_F(:)
  !> Piles for the rounded corner square
  type(point_type), allocatable :: boundary_piles(:)
  !> The rounded corner square
  type(point_type), allocatable :: boundary(:)
  !> Blueprint parameters
  character(:), allocatable :: color
  real :: circle_radius
  real :: dash_width
  real :: line_width
  character(:), allocatable :: font_family
  character(:), allocatable :: font_size
contains
  !> Initialization
  procedure :: initialize
  !> Compute the logo
  procedure :: compute
  !> Draw a blueprint based on
  !> default parameters.
  procedure :: blueprint
  !> Draw logo
  procedure :: draw
end type logo_type

contains

!> Initialization
subroutine initialize(self, filename)
  !> Type-bound variable
  class(logo_type), intent(inout) :: self
  !> Namelist filename
  character(*), intent(in), optional :: filename

  !> Namelist variables.
  integer :: num_curves
  integer :: num_rounded
  real :: rounded_radius
  real :: side_length(2)
  real :: corner(4)
  real :: hook_offset
  real :: reference_point(2)
  real :: x(3, 3), y(3, 3)
  namelist /parameters/ &
    & num_curves, num_rounded, &
    & side_length, rounded_radius, &
    & corner, hook_offset, &
    & reference_point, x, y

  character(len=7) :: color
  character(len=80) :: font_family, font_size
  real :: circle_radius
  real :: dash_width
  real :: line_width
  namelist /blueprint/ &
    & color, font_family, font_size, &
    & circle_radius, line_width, dash_width

  !> Local variables
  real :: factor(2)
  integer :: unit
  logical :: exist

  !> Inquire filename
  inquire (file=filename, exist=exist)
  if (.not. exist) error stop "namelist not found."

  open (newunit=unit, file=filename)
  read (unit=unit, nml=parameters)
  read (unit=unit, nml=blueprint)
  close (unit)

  factor = (side_length/2.)/(1.+2.*corner(1:2))
  self%num_curves = num_curves
  self%num_rounded = num_rounded
  self%side_length = side_length
  self%rounded_radius = rounded_radius*side_length(1)
  self%corner = [side_length*corner(1:2), &
    & side_length/(1.+2.*corner(3:4))*corner(3:4)]
  self%hook_offset = hook_offset*factor(2)
  self%reference_point = reference_point*factor
  self%x = x*factor(1)
  self%y = y*factor(2)

  self%color = trim(color)
  self%font_family = trim(font_family)
  self%font_size = trim(font_size)
  self%circle_radius = circle_radius
  self%line_width = line_width
  self%dash_width = dash_width
end subroutine initialize

!> Compute all "Points" required.
subroutine compute(self)
  !> Type-bound variable
  class(logo_type), intent(inout) :: self
  !> Local variables
  type(point_type), allocatable :: temp(:), temp2(:)
  type(point_type) :: point

  allocate (self%letter_piles(18))
  allocate (self%boundary_piles(12))
  associate ( &
    c => self%reference_point, &
    n => self%num_curves, &
    x => self%x, &
    y => self%y, &
    s => self%side_length, &
    r => self%corner, &
    F1 => self%letter_piles(1:6), &
    F2 => self%letter_piles(7:12), &
    F3 => self%letter_piles(13:18), &
    B => self%boundary_piles, &
    m => self%num_rounded, &
    rr => self%rounded_radius)

    F1 = [ &
      & point_type(x(3, 1), y(1, 1)), &
      & point_type(x(1, 1), y(1, 1)), &
      & point_type(x(1, 1), y(2, 1)), &
      & point_type(x(2, 1), y(2, 1)), &
      & point_type(x(3, 1), y(2, 1)), &
      & point_type(x(3, 1), y(3, 1))]
    temp = [F1(1), &
      & rounded_corner(F1(2), rr, [+1, +1], m, .true.), &
      & rounded_corner(F1(3), rr, [+1, -1], m, .true.), &
      & bezier_curve(F1(4:6), n)]
    self%letter_F = [ &
      & mirror(temp, 1, .true., c%x), &
      & temp, mirror(temp, 2, .true., 0.0)]

    point = self%letter_F(1)
    F2 = [ &
      & point_type(x(1, 2), y(1, 2)), &
      & point_type(x(1, 2), y(2, 2)), &
      & point_type(x(2, 2), y(2, 2)), &
      & point_type(x(2, 2), y(3, 2)), &
      & point_type(x(3, 2), y(3, 2)), &
      & point_type(point%x, y(3, 2))]

    temp2 = rounded_corner(F2(3), rr, [+1, +1], m, .true.)
    temp = [ &
      & rounded_corner(F2(1), rr, [-1, -1], m, .true.), &
      & rounded_corner(F2(2), rr, [-1, +1], m, .true.), &
      & temp2, &
      & bezier_curve([temp2(size(temp2)), F2(4:5)], n), &
      & rounded_corner(F2(6), rr, [+1, -1], m, .false.)]
    self%letter_F = [self%letter_F, temp]

    point = self%letter_F(size(self%letter_F))
    F3 = [ &
      & point_type(point%x, y(1, 3)), &
      & point_type(x(1, 3), y(1, 3)), &
      & point_type(x(2, 3), y(1, 3)), &
      & point_type(x(2, 3), y(2, 3)), &
      & point_type(x(2, 3), y(3, 3)), &
      & point_type(x(3, 3), y(3, 3))]

    temp2 = [ &
      & rounded_corner(F3(1), rr, [+1, +1], m, .false.), &
      & bezier_curve(F3(2:4), n), &
      & rounded_corner(F3(5), rr, [+1, -1], m, .true.) - &
      & [0., self%hook_offset], &
      & rounded_corner(F3(6), rr, [-1, -1], m, .true.) - &
      & [0., self%hook_offset]]

    temp = [ &
      & rounded_corner(F3(1), rr, [+1, +1], m, .false.), &
      & bezier_curve(F3(2:4), n), &
      & rounded_corner(F3(5), rr, [+1, -1], m, .true.), &
      & rounded_corner(F3(6), rr, [-1, -1], m, .true.)]
    temp = [temp2, mirror(temp, 2, .true., c%y), &
      & self%letter_F(1)]
    self%letter_F = [self%letter_F, temp]

    self%boundary_piles = [ &
      & point_type(-s(1)/2 + r(3), -s(2)/2), &
      & point_type(-s(1)/2, -s(2)/2), &
      & point_type(-s(1)/2, -s(2)/2 + r(4)), &
      & point_type(-s(1)/2, +s(2)/2 - r(4)), &
      & point_type(-s(1)/2, +s(2)/2), &
      & point_type(-s(1)/2 + r(3), +s(2)/2), &
      & point_type(+s(1)/2 - r(3), +s(2)/2), &
      & point_type(+s(1)/2, +s(2)/2), &
      & point_type(+s(1)/2, +s(2)/2 - r(4)), &
      & point_type(+s(1)/2, -s(2)/2 + r(4)), &
      & point_type(+s(1)/2, -s(2)/2), &
      & point_type(+s(1)/2 - r(3), -s(2)/2)]

    self%boundary = [ &
      & bezier_curve(B(1:3), n), &
      & bezier_curve(B(4:6), n), &
      & bezier_curve(B(7:9), n), &
      & bezier_curve(B(10:12), n), B(1)]
  end associate
end subroutine compute

subroutine draw(self)
  class(logo_type), intent(in) :: self
  type(xml_type) :: svg
  type(attribute_type), allocatable :: attributes(:)
  character(:), allocatable :: width, height
  real, parameter :: canvas_ratio = 1.1
  real :: line_width, offset(2)
  character(:), allocatable :: color

  line_width = 1.0
  ! color = '#796e58'
  color = '#6d5192'
  offset = 0.5*self%side_length*canvas_ratio

  call svg%open('./data/logo.svg')
  attributes = [ &
    & 'version'.pair.'1.0', &
    & 'encoding'.pair.'utf-8', &
    & 'standalone'.pair.'no']
  call svg%write_prolog(attributes)

  width = str(self%side_length(1)*canvas_ratio)
  height = str(self%side_length(2)*canvas_ratio)
  attributes = [ &
    & 'xmlns'.pair.'http://www.w3.org/2000/svg', &
    & 'width'.pair.width, 'height'.pair.height]

  call svg%write_attribute( &
    & 'svg', attributes, inline=.false., line_break=.false.)
  call svg%write_element('title', 'Fortran Logo')
  call svg%write_element('desc', 'Generated by Fortran Logo Generator')

  attributes = [ &
    & 'id'.pair.'diagonalFill', &
    & 'width'.pair.1.0, &
    & 'height'.pair.1.0, &
    & 'patternUnits'.pair.'userSpaceOnUse', &
    & 'patternTransform'.pair.'rotate(47)']
  call svg%write_attribute('pattern', attributes, &
    & inline=.false.)

  attributes = [ &
    & 'x'.pair.0., 'y'.pair.0., 'width'.pair.1, &
    & 'height'.pair.1, 'fill'.pair.color]

  call svg%write_attribute('rect', attributes)
  call svg%close_attribute('pattern')

  associate ( &
    F => self%letter_F, &
    B => self%boundary)
    call path_fill(F, B)
  end associate

  call svg%close_attribute('svg')
  call svg%close()
contains
  subroutine path_fill(ps1, ps2)
    type(point_type), intent(in) :: ps1(:), ps2(:)
    type(characters_type) :: message
    character(len=80), allocatable :: strings(:)
    integer :: i_, j_

    allocate (strings(size(ps1) + size(ps2)))
    do i_ = 1, size(ps1)
      j_ = size(ps1) - i_ + 1
      write (strings(i_), "('L', 2(1x, f12.6))") &
        & ps1(j_)%x + offset(1), -ps1(j_)%y + offset(2)
    end do
    strings(1) (1:1) = 'M'
    strings(size(ps1)) (29:29) = 'z'

    do i_ = size(ps1) + 1, size(ps1) + size(ps2)
      j_ = i_ - size(ps1)
      write (strings(i_), "('L', 2(1x, f12.6))") &
        & ps2(j_)%x + offset(1), -ps2(j_)%y + offset(2)
    end do
    strings(size(ps1) + 1) (1:1) = 'M'
    strings(size(ps1) + size(ps2)) (29:29) = 'z'

    message = strings
    attributes = [ &
      & 'fill'.pair.'url(#diagonalFill)', &
      & 'stroke'.pair.color, &
      & 'stroke-width'.pair.line_width, &
      & 'd'.pair.message]
    call svg%write_attribute('path', attributes)
  end subroutine path_fill
end subroutine draw

!> Draw blueprint.
subroutine blueprint(self)
  class(logo_type), intent(in) :: self
  type(xml_type) :: svg
  type(attribute_type), allocatable :: attributes(:)
  character(:), allocatable :: width, height
  type(point_type) :: pt(2)
  real :: offset(2)
  integer :: i, j

  character(:), allocatable :: color, font_family, font_size
  real :: dash_width
  real :: line_width
  real :: circle_radius
  real, parameter :: ampli_factor = 1.2

  color = self%color
  font_family = self%font_family
  font_size = self%font_size
  circle_radius = self%circle_radius
  line_width = self%line_width
  dash_width = self%dash_width

  call svg%open('./data/blueprint.svg')
  attributes = [ &
    & 'version'.pair.'1.0', &
    & 'encoding'.pair.'utf-8', &
    & 'standalone'.pair.'no']
  call svg%write_prolog(attributes)

  width = str(self%side_length(1)*ampli_factor)
  height = str(self%side_length(2)*ampli_factor)
  attributes = [ &
    & 'xmlns'.pair.'http://www.w3.org/2000/svg', &
    & 'width'.pair.width, 'height'.pair.height]

  call svg%write_attribute( &
    & 'svg', attributes, inline=.false., line_break=.false.)
  call svg%write_element('title', 'Fortran Logo')
  call svg%write_element('desc', 'Generated by Fortran Logo Generator')

  attributes = [ &
    & 'id'.pair.'diagonalFill', &
    & 'width'.pair.9, &
    & 'height'.pair.2, &
    & 'patternUnits'.pair.'userSpaceOnUse', &
    & 'patternTransform'.pair.'rotate(45)']
  call svg%write_attribute('pattern', attributes, &
    & inline=.false.)

  attributes = [ &
    & 'x'.pair.0., 'y'.pair.0., 'width'.pair.1., &
    & 'height'.pair.1., 'fill'.pair.color]

  call svg%write_attribute('rect', attributes)
  call svg%close_attribute('pattern')

  offset = 0.5*self%side_length*ampli_factor
  associate ( &
    F => self%letter_F, &
    B => self%boundary, &
    R => self%reference_point, &
    G => centroid(self%letter_F), &
    FP => self%letter_piles, &
    BP => self%boundary_piles)

    do j = 1, 3
      do i = 1, 3

        pt(1) = point_type(self%x(i, j), BP(2)%y)
        pt(2) = point_type(self%x(i, j), BP(5)%y)
        call dashed_line(pt(1), pt(2))

        call dashed_line( &
          & point_type(BP(2)%x, self%y(i, j)), &
          & point_type(BP(8)%x, self%y(i, j)))
      end do
    end do

    call dashed_line( &
      & point_type(R%x, BP(2)%y), &
      & point_type(R%x, BP(5)%y))

    call dashed_line( &
      & point_type(BP(2)%x, R%y), &
      & point_type(BP(8)%x, R%y))

    call dashed_line(BP(2), BP(5))
    call dashed_line(BP(5), BP(8))
    call dashed_line(BP(8), BP(11))
    call dashed_line(BP(11), BP(2))

    call path_fill(F, B)

    do i = 1, size(FP)
      call circle(FP(i))
    end do

    do i = 1, size(BP)
      call circle(BP(i))
    end do

    call circle(R)
  end associate

  call svg%close_attribute('svg')
  call svg%close()

contains
  subroutine path_fill(ps1, ps2)
    type(point_type), intent(in) :: ps1(:), ps2(:)
    type(characters_type) :: message
    character(len=80), allocatable :: strings(:)
    integer :: i_, j_

    allocate (strings(size(ps1) + size(ps2)))
    do i_ = 1, size(ps1)
      j_ = size(ps1) - i_ + 1
      write (strings(i_), "('L', 2(1x, f12.6))") &
        & ps1(j_)%x + offset(1), -ps1(j_)%y + offset(2)
    end do
    strings(1) (1:1) = 'M'
    strings(size(ps1)) (29:29) = 'z'

    do i_ = size(ps1) + 1, size(ps1) + size(ps2)
      j_ = i_ - size(ps1)
      write (strings(i_), "('L', 2(1x, f12.6))") &
        & ps2(j_)%x + offset(1), -ps2(j_)%y + offset(2)
    end do
    strings(size(ps1) + 1) (1:1) = 'M'
    strings(size(ps1) + size(ps2)) (29:29) = 'z'

    message = strings
    attributes = [ &
      & 'fill'.pair.'url(#diagonalFill)', &
      ! & 'fill-rule'.pair.'url(#diagonalFill)', &
      & 'stroke'.pair.color, &
      & 'stroke-width'.pair.line_width, &
      & 'd'.pair.message]
    call svg%write_attribute('path', attributes)
  end subroutine path_fill

  subroutine path(ps)
    type(point_type), intent(in) :: ps(:)
    type(characters_type) :: message
    character(len=80), allocatable :: strings(:)
    integer :: i_

    allocate (strings(size(ps)))
    do i_ = 1, size(ps)
      write (strings(i_), "('L', 2(1x, f12.6))") &
        & ps(i_)%x + offset(1), -ps(i_)%y + offset(2)
    end do
    strings(1) (1:1) = 'M'
    message = strings

    attributes = [ &
      & 'fill'.pair.'none', &
      & 'stroke'.pair.color, &
      & 'stroke-width'.pair.line_width, &
      & 'd'.pair.message]
    call svg%write_attribute('path', attributes)
  end subroutine path

  subroutine circle(p)
    type(point_type), intent(in) :: p

    attributes = [ &
      & 'fill'.pair.'none', &
      & 'stroke'.pair.color, &
      & 'cx'.pair.p%x + offset(1), &
      & 'cy'.pair.-p%y + offset(2), &
      & 'r'.pair.str(circle_radius)]
    call svg%write_attribute('circle', attributes)
  end subroutine circle

  subroutine text(p, message, tuned)
    type(point_type), intent(in) :: p
    character(*), intent(in) :: message
    real, intent(in) :: tuned(:)

    attributes = [ &
      & 'x'.pair.p%x + offset(1) + tuned(1), &
      & 'y'.pair.-p%y + offset(2) + tuned(2), &
      & 'fill'.pair.color, &
      & 'font-size'.pair.font_size, &
      & 'font-family'.pair.font_family]

    call svg%write_attribute('text', attributes, &
      & inline=.false., line_break=.true.)
    write (svg%unit, "(a)") message
    call svg%close_attribute('text')
  end subroutine text

  subroutine dashed_line(p1, p2)
    type(point_type), intent(in) :: p1, p2

    attributes = [ &
      & 'stroke'.pair.color, &
      & 'x1'.pair.p1%x + offset(1), &
      & 'y1'.pair.-p1%y + offset(2), &
      & 'x2'.pair.p2%x + offset(1), &
      & 'y2'.pair.-p2%y + offset(2), &
      & 'stroke-dasharray'.pair.'2 2', &
      & 'stroke-width'.pair.dash_width]
    call svg%write_attribute('line', attributes)
  end subroutine dashed_line
end subroutine blueprint

end module module_logo
