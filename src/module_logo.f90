module module_logo

use module_geometry
use module_xml
use module_utility
implicit none

!> Icon type
type :: logo_type
  !> Number of points used when
  !> drawing curves.
  integer :: num_points(2)
  !> Radius of rounded corners
  real :: rad_corners
  !> Side length of the logo.
  !> (Assuming square canvas)
  real :: width, height
  !> Margin
  real :: margin(4)
  !> Corner
  real :: corner(2)
  !> A reference point
  type(point_type) :: reference_point
  !> Offset for hook of "F"
  real :: bracket_offset
  !> Essential horiozntal coordinates.
  real, allocatable :: hori_anchors(:, :)
  !> Essential vertical coordinates
  real, allocatable :: vert_anchors(:, :)
  !> Piles for letter "F"
  type(point_type), allocatable :: letter_piles(:)
  !> The letter "F"
  type(point_type), allocatable :: letter_F(:)
  !> Piles for the frame
  type(point_type), allocatable :: boundary_piles(:)
  !> Frame
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
  integer :: num_points(2)
  real :: rad_corners
  real :: width, height
  real :: margin(4), corner(2)
  real :: frame_paras(4)
  real :: bracket_offset
  real :: reference_point(2)
  real :: hori_anchors(3, 3), vert_anchors(3, 3)
  namelist /parameters/ &
    & num_points, width, height, margin, corner, frame_paras

  namelist /letter_paras/ &
    & rad_corners, bracket_offset, &
    & reference_point, hori_anchors, vert_anchors

  character(len=7) :: color
  character(len=80) :: font_family, font_size
  real :: circle_radius
  real :: dash_width
  real :: line_width
  namelist /blueprint/ &
    & color, font_family, font_size, &
    & circle_radius, line_width, dash_width

  !> Local variables
  integer :: unit
  logical :: exist

  !> Inquire filename
  inquire (file=filename, exist=exist)
  if (.not. exist) error stop "namelist not found."

  open (newunit=unit, file=filename)
  read (unit=unit, nml=parameters)
  read (unit=unit, nml=letter_paras)
  read (unit=unit, nml=blueprint)
  close (unit)

  self%num_points = num_points
  self%width = width
  self%height = height
  ! self%rad_corners = rad_corners*width

  self%corner = corner

  associate ( &
    w => width, h => height, m => margin, &
    r => reference_point, &
    x => hori_anchors, y => vert_anchors)

    self%rad_corners = 0.5*((h - sum(m(3:4))) + (w - sum(m(1:2))))*rad_corners
    self%bracket_offset = (h - sum(m(3:4)))*bracket_offset

    self%reference_point = [ &
      & (-w/2. + m(1)) + (w - sum(m(1:2)))*r(1), &
      & (-h/2. + m(3)) + (h - sum(m(3:4)))*r(2)]

    self%hori_anchors = (-w/2. + m(1)) + (w - sum(m(1:2)))*x
    self%vert_anchors = (-h/2. + m(3)) + (h - sum(m(3:4)))*y
  end associate

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
  type(point_type), allocatable :: temp1(:), temp2(:)
  type(point_type) :: point
  integer :: i

  allocate (self%letter_piles(18))
  allocate (self%boundary_piles(12))
  associate ( &
    ref => self%reference_point, &
    num => self%num_points, &
    rad => self%rad_corners, &
    c => self%corner, &
    x => self%hori_anchors, &
    y => self%vert_anchors, &
    w => self%width, &
    h => self%height, &
    F => self%letter_piles, &
    B => self%boundary_piles)

    F(1:6) = [ &
      & point_type(x(3, 1), y(1, 1)), &
      & point_type(x(1, 1), y(1, 1)), &
      & point_type(x(1, 1), y(2, 1)), &
      & point_type(x(2, 1), y(2, 1)), &
      & point_type(x(3, 1), y(2, 1)), &
      & point_type(x(3, 1), y(3, 1))]
    temp1 = [F(1), &
      & rounded_corner(F(2), rad, [+1, +1], num(2), .true.), &
      & rounded_corner(F(3), rad, [+1, -1], num(2), .true.), &
      & bezier_curve(F(4:6), num(1))]
    self%letter_F = [ &
      & mirror(temp1, 1, .true., ref%x), &
      & temp1, mirror(temp1, 2, .true., 0.0)]

    point = self%letter_F(1)
    F(7:12) = [ &
      & point_type(x(1, 2), y(1, 2)), &
      & point_type(x(1, 2), y(2, 2)), &
      & point_type(x(2, 2), y(2, 2)), &
      & point_type(x(2, 2), y(3, 2)), &
      & point_type(x(3, 2), y(3, 2)), &
      & point_type(point%x, y(3, 2))]

    temp2 = rounded_corner(F(9), rad, [+1, +1], num(2), .true.)
    temp1 = [ &
      & rounded_corner(F(7), rad, [-1, -1], num(2), .true.), &
      & rounded_corner(F(8), rad, [-1, +1], num(2), .true.), &
      & temp2, &
      & bezier_curve([temp2(size(temp2)), F(10:11)], num(1)), &
      & rounded_corner(F(12), rad, [+1, -1], num(2), .false.)]
    self%letter_F = [self%letter_F, temp1]

    point = self%letter_F(size(self%letter_F))
    F(13:18) = [ &
      & point_type(point%x, y(1, 3)), &
      & point_type(x(1, 3), y(1, 3)), &
      & point_type(x(2, 3), y(1, 3)), &
      & point_type(x(2, 3), y(2, 3)), &
      & point_type(x(2, 3), y(3, 3)), &
      & point_type(x(3, 3), y(3, 3))]

    temp2 = [ &
      & rounded_corner(F(13), rad, [+1, +1], num(2), .false.), &
      & bezier_curve(F(14:16), num(1)), &
      & rounded_corner(F(17), rad, [+1, -1], num(2), .true.) - &
      & [0., self%bracket_offset], &
      & rounded_corner(F(18), rad, [-1, -1], num(2), .true.) - &
      & [0., self%bracket_offset]]

    temp1 = [ &
      & rounded_corner(F(13), rad, [+1, +1], num(2), .false.), &
      & bezier_curve(F(14:16), num(1)), &
      & rounded_corner(F(17), rad, [+1, -1], num(2), .true.), &
      & rounded_corner(F(18), rad, [-1, -1], num(2), .true.)]
    temp1 = [temp2, mirror(temp1, 2, .true., ref%y)]
    self%letter_F = [self%letter_F, temp1]

    self%boundary_piles = [ &
      & point_type(-w/2 + c(1), -h/2), &
      & point_type(-w/2, -h/2), &
      & point_type(-w/2, -h/2 + c(2)), &
      & point_type(-w/2, +h/2 - c(2)), &
      & point_type(-w/2, +h/2), &
      & point_type(-w/2 + c(1), +h/2), &
      & point_type(+w/2 - c(1), +h/2), &
      & point_type(+w/2, +h/2), &
      & point_type(+w/2, +h/2 - c(2)), &
      & point_type(+w/2, -h/2 + c(2)), &
      & point_type(+w/2, -h/2), &
      & point_type(+w/2 - c(1), -h/2)]

    self%boundary = [(bezier_curve(B(i:i + 2), num(1)), i=1, 10, 3)]
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
  offset = 0.5*[self%width, self%height]*canvas_ratio

  call svg%open('./data/logo.svg')
  attributes = [ &
    & 'version'.pair.'1.0', &
    & 'encoding'.pair.'utf-8', &
    & 'standalone'.pair.'no']
  call svg%write_prolog(attributes)

  width = str(self%width*canvas_ratio)
  height = str(self%height*canvas_ratio)
  attributes = [ &
    & 'xmlns'.pair.'http://www.w3.org/2000/svg', &
    & 'width'.pair.width, 'height'.pair.height]

  call svg%write_attribute( &
    & 'svg', attributes, inline=.false.)
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
  real, parameter :: ampli_factor = 1.1

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

  width = str(self%width*ampli_factor)
  height = str(self%height*ampli_factor)
  offset = 0.5*[self%width, self%height]*ampli_factor
  attributes = [ &
    & 'xmlns'.pair.'http://www.w3.org/2000/svg', &
    & 'width'.pair.width, 'height'.pair.height]

  call svg%write_attribute( &
    & 'svg', attributes, inline=.false.)
  call svg%write_element('title', 'Fortran Logo')
  call svg%write_element('desc', 'Generated by Fortran Logo Generator')

  !> Fill Pattern
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

  !> Image
  ! attributes = [ &
  !   & 'transform'.pair.'translate('// &
  !   & str(self%hori_anchors(1, 1) + offset(1))//","// &
  !   & str(self%vert_anchors(1, 1) + offset(2))//")"]
  ! call svg%write_attribute('g', attributes, inline=.false.)
  ! attributes = [ &
  !   & 'href'.pair.'F1954.png', &
  !   & 'width'.pair.abs(self%hori_anchors(1, 2) - self%hori_anchors(1, 1)), &
  !   & 'height'.pair.abs(self%vert_anchors(1, 2) - self%vert_anchors(1, 1))]
  ! call svg%write_attribute('image', attributes, inline=.true.)
  ! call svg%close_attribute('g')

  associate ( &
    F => self%letter_F, &
    B => self%boundary, &
    R => self%reference_point, &
    G => centroid(self%letter_F), &
    FP => self%letter_piles, &
    BP => self%boundary_piles)

    do j = 1, 3
      do i = 1, 3

        pt(1) = point_type(self%hori_anchors(i, j), BP(2)%y)
        pt(2) = point_type(self%hori_anchors(i, j), BP(5)%y)
        call dashed_line(pt(1), pt(2))

        call dashed_line( &
          & point_type(BP(2)%x, self%vert_anchors(i, j)), &
          & point_type(BP(8)%x, self%vert_anchors(i, j)))
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
    ! call path_fill(F)

    do i = 1, size(FP)
      call circle(FP(i))
      call text(FP(i), str(i))
    end do

    do i = 1, size(BP)
      call circle(BP(i))
      call text(BP(i), achar(64 + i))
    end do

    call circle(R)
    call text(R, "R")

    !> Uncomment to draw origin and
    !> centroid of "F"

    ! call circle(point_type(0., 0.))
    ! call text(point_type(0., 0.), "O")

    ! call circle(centroid(F))
    ! call text(centroid(F), "G")
  end associate

  call svg%close_attribute('svg')
  call svg%close()

contains
  subroutine path_fill(ps1, ps2)
    type(point_type), intent(in) :: ps1(:)
    type(point_type), intent(in), optional :: ps2(:)
    type(characters_type) :: message
    character(len=80), allocatable :: strings(:)
    integer :: i_, j_

    if (present(ps2)) then
      allocate (strings(size(ps1) + size(ps2)))
    else
      allocate (strings(size(ps1)))
    end if

    do i_ = 1, size(ps1)
      j_ = size(ps1) - i_ + 1
      write (strings(i_), "('L', 2(1x, f12.6))") &
        & ps1(j_)%x + offset(1), -ps1(j_)%y + offset(2)
    end do
    strings(1) (1:1) = 'M'
    strings(size(ps1)) (29:29) = 'z'

    if (present(ps2)) then
      do i_ = size(ps1) + 1, size(ps1) + size(ps2)
        j_ = i_ - size(ps1)
        write (strings(i_), "('L', 2(1x, f12.6))") &
          & ps2(j_)%x + offset(1), -ps2(j_)%y + offset(2)
      end do
      strings(size(ps1) + 1) (1:1) = 'M'
      strings(size(ps1) + size(ps2)) (29:29) = 'z'
    end if

    message = strings
    attributes = [ &
      & 'fill'.pair.'url(#diagonalFill)', &
      ! & 'fill'.pair.'none', &
      & 'stroke'.pair.color, &
      & 'stroke-width'.pair.line_width, &
      & 'd'.pair.message]
    call svg%write_attribute('path', attributes)
  end subroutine path_fill

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

  subroutine text(p, message)
    type(point_type), intent(in) :: p
    character(*), intent(in) :: message

    attributes = [ &
      & 'x'.pair.p%x + offset(1), &
      & 'y'.pair.-p%y + offset(2), &
      & 'text-anchor'.pair.'middle', &
      & 'alignment-baseline'.pair.'central', &
      & 'fill'.pair.color, &
      & 'font-size'.pair.font_size, &
      & 'font-family'.pair.font_family]

    call svg%write_attribute('text', attributes, &
      & inline=.false.)
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
