module module_logo

use module_geometry
use module_xml
use module_utility
implicit none

public :: logo_type
public :: draw
public :: read (formatted)
private

!> Icon type
type :: logo_type
  !> Number of points used when
  !> drawing curves.
  integer :: num_points(2)
  !> Radius of rounded corners
  real :: rad_corners
  !> Width and height of the logo.
  real :: width, height
  !> Margin
  real :: margin(4)
  !> Corner
  real :: corner(2)
  !> A reference point
  type(point_type) :: reference_point
  !> Offset for hook of "F"
  real :: bracket_offset(2)
  !> Essential horiozntal coordinates.
  real, allocatable :: hori_anchors(:, :)
  !> Essential vertical coordinates
  real, allocatable :: vert_anchors(:, :)
  !> Piles for letter "F"
  type(point_type), allocatable :: letter_piles(:)
  !> The letter "F"
  type(point_type), allocatable :: letter(:)
  !> Piles for the frame
  type(point_type), allocatable :: boundary_piles(:)
  !> Frame
  type(point_type), allocatable :: boundary(:)
  !> Output filename
  character(:), allocatable :: file
  !> Logo without frame
  logical :: letter_only
  !> Ratio of canvas (> 1)
  real :: canvas_ratio
  !> Color of the logo
  character(:), allocatable :: color
  !> Font family
  character(:), allocatable :: font_family
  !> Font size
  character(:), allocatable :: font_size
  !> Radius of piles
  real :: pile_radius
  !> Whether to fill piles
  character(:), allocatable :: pile_fill
  !> Line width
  real :: line_width
  !> Dashed line width
  real :: dash_width
  !> Dashed line type
  character(:), allocatable :: dash_array
  !> Fill pattern
  real :: fill_pattern(4)
  !> Whether to compare the logo with an image
  logical :: compare
  !> The image to be compared.
  character(:), allocatable :: compare_image
end type logo_type

interface read (formatted)
module procedure :: read_formatted
end interface read (formatted)

contains

subroutine read_namelist(unit, logo)
  integer, intent(in) :: unit
  type(logo_type), intent(out) :: logo

  !> local
  integer, parameter :: len_ = 80

  integer :: num_points(2)
  real :: rad_corners
  real :: width, height
  real :: margin(4)
  real :: corner(2)
  real :: reference_point(2)
  real :: bracket_offset(2)
  real :: hori_anchors(3, 3)
  real :: vert_anchors(3, 3)
  character(len=len_) :: file
  logical :: letter_only
  real :: canvas_ratio
  character(len=len_) :: color
  character(len=len_) :: font_family
  character(len=len_) :: font_size
  real :: pile_radius
  character(len=len_) :: pile_fill
  real :: line_width
  real :: dash_width
  character(len=len_) :: dash_array
  real :: fill_pattern(4)
  logical :: compare
  character(len=len_) :: compare_image

  namelist /parameters/ &
    & num_points, &
    & rad_corners, &
    & width, height, &
    & margin, &
    & corner, &
    & reference_point, &
    & bracket_offset, &
    & hori_anchors, &
    & vert_anchors, &
    & file, &
    & letter_only, &
    & canvas_ratio, &
    & color, &
    & font_family, &
    & font_size, &
    & pile_radius, &
    & pile_fill, &
    & line_width, &
    & dash_width, &
    & dash_array, &
    & fill_pattern, &
    & compare, &
    & compare_image
  read (unit=unit, nml=parameters)

  logo%num_points = num_points
  logo%width = width
  logo%height = height
  logo%corner = corner
  logo%margin = margin

  !> Compute anchors
  associate ( &
    w => logo%width, &
    h => logo%height, &
    mw => sum(logo%margin(1:2)), &
    mh => sum(logo%margin(3:4)), &
    ml => logo%margin(1), &
    mb => logo%margin(3), &
    r => reference_point, &
    x => hori_anchors, &
    y => vert_anchors)

    logo%rad_corners = &
      & 0.5*((h - mh) + (w - mw))*rad_corners
    logo%bracket_offset = (h - mh)*bracket_offset
    logo%reference_point = [ &
      & (-w/2.+ml) + (w - mw)*r(1), &
      & (-h/2.+mb) + (h - mh)*r(2)]
    logo%hori_anchors = (-w/2.+ml) + (w - mw)*x
    logo%vert_anchors = (-h/2.+mb) + (h - mh)*y
  end associate

  !> Compute piles
  call compute_piles(logo)

  logo%file = trim(file)
  logo%letter_only = letter_only
  logo%canvas_ratio = canvas_ratio
  logo%color = trim(color)
  logo%font_family = trim(font_family)
  logo%font_size = trim(font_size)
  logo%pile_radius = pile_radius
  logo%line_width = line_width
  logo%dash_width = dash_width
  logo%dash_array = trim(dash_array)
  logo%fill_pattern = fill_pattern
  logo%compare = compare
  logo%compare_image = trim(compare_image)
end subroutine read_namelist

!> Compute all "Points" required.
subroutine compute_piles(logo)
  !> Type-bound variable
  type(logo_type), intent(inout) :: logo
  !> Local variables
  type(point_type), allocatable :: temp1(:), temp2(:)
  type(point_type) :: point
  integer :: i

  allocate (logo%letter_piles(18))
  allocate (logo%boundary_piles(12))

  associate ( &
    ref => logo%reference_point, &
    num => logo%num_points, &
    rad => logo%rad_corners, &
    c => logo%corner, &
    x => logo%hori_anchors, &
    y => logo%vert_anchors, &
    w => logo%width, &
    h => logo%height, &
    F => logo%letter_piles, &
    B => logo%boundary_piles)

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
    temp2 = [F(1), &
      & rounded_corner(F(2), rad, [+1, +1], num(2), .true.) + &
        & point_type(-logo%bracket_offset(1), 0.), &
      & rounded_corner(F(3), rad, [+1, -1], num(2), .true.) + &
        & point_type(-logo%bracket_offset(1), 0.), &
      & bezier_curve(F(4:6), num(1))]
    logo%letter = [ &
      & mirror(temp2, 1, .true., ref%x), &
      & temp1, mirror(temp1, 2, .true., 0.0)]

    point = logo%letter(1)
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
    logo%letter = [logo%letter, temp1]

    point = logo%letter(size(logo%letter))
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
      & rounded_corner(F(17), rad, [+1, -1], num(2), .true.) + &
        & point_type(0., logo%bracket_offset(2)), &
      & rounded_corner(F(18), rad, [-1, -1], num(2), .true.) + &
        & point_type(0., logo%bracket_offset(2))]

    temp1 = [ &
      & rounded_corner(F(13), rad, [+1, +1], num(2), .false.), &
      & bezier_curve(F(14:16), num(1)), &
      & rounded_corner(F(17), rad, [+1, -1], num(2), .true.), &
      & rounded_corner(F(18), rad, [-1, -1], num(2), .true.)]
    temp1 = [temp2, mirror(temp1, 2, .true., ref%y)]
    logo%letter = [logo%letter, temp1]

    logo%boundary_piles = [ &
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

    logo%boundary = [(bezier_curve(B(i:i + 2), num(1)), i=1, 10, 3)]
  end associate
end subroutine compute_piles

subroutine read_formatted(logo, unit, iotype, v_list, iostat, iomsg)
  !> Bounded type
  class(logo_type), intent(inout) :: logo
  !> Unit
  integer, intent(in) :: unit
  !> IO type
  character(len=*), intent(in) :: iotype
  !> variable list
  integer, intent(in) :: v_list(:)
  !> IO status
  integer, intent(out) :: iostat
  !> IO message
  character(len=*), intent(inout) :: iomsg

  if (iotype == 'DT') then
    error stop 'Use * instead.'
  else
    call read_namelist(unit, logo)
  end if
end subroutine read_formatted

subroutine draw(logo)
  type(logo_type), intent(inout) :: logo
  type(xml_type) :: svg
  type(attribute_type), allocatable :: attrs(:)
  integer :: i, j

  real :: offset(2)

  !> Prolog
  call svg%open(logo%file)
  attrs = [ &
    & 'version'.pair.'1.0', &
    & 'encoding'.pair.'utf-8', &
    & 'standalone'.pair.'no']
  call svg%write_prolog(attrs)

  !> Determine offset
  offset = 0.5*[logo%width, logo%height]*logo%canvas_ratio
  attrs = [ &
    & 'xmlns'.pair.'http://www.w3.org/2000/svg', &
    & 'x'.pair.0., 'y'.pair.0., &
    & 'width'.pair.logo%width*logo%canvas_ratio, &
    & 'height'.pair.logo%height*logo%canvas_ratio]

  call svg%write_attribute('svg', attrs, inline=.false.)
  call svg%write_element('title', 'Fortran Logo')
  call svg%write_element('desc', &
    & 'Generated by Fortran Logo Generator')

  !> Fill Pattern
  attrs = [ &
    & 'id'.pair.'diagonalFill', &
    & 'width'.pair.logo%fill_pattern(1), &
    & 'height'.pair.logo%fill_pattern(2), &
    & 'patternUnits'.pair.'userSpaceOnUse', &
    & 'patternTransform'.pair.'rotate(45)']
  call svg%write_attribute('pattern', attrs, &
    & inline=.false.)
  attrs = [ &
    & 'x'.pair.0., 'y'.pair.0., &
    & 'width'.pair.logo%fill_pattern(3), &
    & 'height'.pair.logo%fill_pattern(4), &
    & 'fill'.pair.logo%color]
  call svg%write_attribute('rect', attrs)
  call svg%close_attribute('pattern')

  if (logo%compare) then
    ! attrs = [ &
    !   & 'transform'.pair.'translate('// &
    !   & str(0.+0.5*logo%width*(logo%canvas_ratio - 1))//","// &
    !   & str(0.+0.5*logo%height*(logo%canvas_ratio - 1))//")"]
    ! call svg%write_attribute('g', attrs, inline=.false.)
    attrs = [ &
      & 'href'.pair.logo%compare_image, &
      & 'x'.pair.str(0.5*logo%width*(logo%canvas_ratio - 1)), &
      & 'y'.pair.str(0.5*logo%height*(logo%canvas_ratio - 1)), &
      & 'width'.pair.logo%width, &
      & 'height'.pair.logo%height]
    call svg%write_attribute('image', attrs, inline=.true.)
    ! call svg%close_attribute('g')
  end if

  associate ( &
    F => logo%letter, &
    B => logo%boundary, &
    R => logo%reference_point, &
    FP => logo%letter_piles, &
    BP => logo%boundary_piles, &
    X => logo%hori_anchors, &
    Y => logo%vert_anchors)

    if (logo%dash_width > 0.) then
      do j = 1, 3
        do i = 1, 3
          call dashed( &
            & point_type(X(i, j), BP(2)%y), &
            & point_type(X(i, j), BP(5)%y))

          call dashed( &
            & point_type(BP(2)%x, Y(i, j)), &
            & point_type(BP(8)%x, Y(i, j)))
        end do
      end do

      call dashed( &
        & point_type(R%x, BP(2)%y), &
        & point_type(R%x, BP(5)%y))

      call dashed( &
        & point_type(BP(2)%x, R%y), &
        & point_type(BP(8)%x, R%y))

      call dashed(BP(2), BP(5))
      call dashed(BP(5), BP(8))
      call dashed(BP(8), BP(11))
      call dashed(BP(11), BP(2))
    end if

    if (logo%letter_only) then
      call path_fill(F)
    else
      call path_fill(F, B)
    end if

    if (logo%pile_radius > 0.) then
      do i = 1, size(FP)
        call circle(FP(i))
      end do

      do i = 1, size(BP)
        call circle(BP(i))
      end do

      call circle(R)
      call circle(point_type(0., 0.))
      call circle(centroid(F))
    end if

    select case (logo%font_size)
    case ("0", "0%")
    case default
      do i = 1, size(FP)
        call text(FP(i), str(i))
      end do

      do i = 1, size(BP)
        call text(BP(i), achar(64 + i))
      end do

      call text(R, "R")
      ! call text(point_type(0., 0.), "O")
      ! call text(centroid(F), "G")
    end select
  end associate

  call svg%close_attribute('svg')
  call svg%close()

contains
  function svg_coordinate(point) result(converted)
    type(point_type), intent(in) :: point
    type(point_type) :: converted

    converted%x = point%x + offset(1)
    converted%y = -point%y + offset(2)
  end function svg_coordinate

  subroutine path_fill(path1, path2)
    type(point_type), intent(in) :: path1(:)
    type(point_type), intent(in), optional :: path2(:)
    type(characters_type) :: message
    character(len=80), allocatable :: strings(:)
    integer :: i_, j_

    if (present(path2)) then
      allocate (strings(size(path1) + size(path2)))
    else
      allocate (strings(size(path1)))
    end if

    do i_ = 1, size(path1)
      j_ = size(path1) - i_ + 1
      write (strings(i_), "('L', 2(1x, f12.6))") &
        & svg_coordinate(path1(j_))
    end do
    strings(1) (1:1) = 'M'
    strings(size(path1)) (29:29) = 'z'

    if (present(path2)) then
      do i_ = size(path1) + 1, size(path1) + size(path2)
        j_ = i_ - size(path1)
        write (strings(i_), "('L', 2(1x, f12.6))") &
          & svg_coordinate(path2(j_))
      end do
      strings(size(path1) + 1) (1:1) = 'M'
      strings(size(path1) + size(path2)) (29:29) = 'z'
    end if

    message = strings
    attrs = [ &
      & 'fill'.pair.'url(#diagonalFill)', &
      & 'stroke'.pair.logo%color, &
      & 'stroke-width'.pair.logo%line_width, &
      & 'd'.pair.message]
    call svg%write_attribute('path', attrs)
  end subroutine path_fill

  subroutine circle(point)
    type(point_type), intent(in) :: point
    type(point_type) :: converted

    converted = svg_coordinate(point)
    attrs = [ &
      & 'fill'.pair.'none', &
      & 'stroke'.pair.logo%color, &
      & 'cx'.pair.converted%x, &
      & 'cy'.pair.converted%y, &
      & 'r'.pair.str(logo%pile_radius)]
    call svg%write_attribute('circle', attrs)
  end subroutine circle

  subroutine text(point, message, rotate)
    type(point_type), intent(in) :: point
    character(*), intent(in) :: message
    real, intent(in), optional :: rotate
    type(point_type) :: converted
    real :: tuned(2), rot

    if (present(rotate)) then
      rot = rotate
    else
      rot = 0.
    end if

    tuned(1) = logo%width*logo%canvas_ratio/70.
    tuned(2) = -logo%height*logo%canvas_ratio/70.
    converted = svg_coordinate(point)
    attrs = [ &
      & 'x'.pair.point%x + offset(1) + tuned(1), &
      & 'y'.pair.-point%y + offset(2) + tuned(2), &
      & 'text-anchor'.pair.'start', &
      & 'alignment-baseline'.pair.'central', &
      & 'fill'.pair.logo%color, &
      & 'font-size'.pair.logo%font_size, &
      & 'font-family'.pair.logo%font_family, &
      & 'transform'.pair."rotate("//str(rot)//")"]

    call svg%write_attribute('text', attrs, &
      & inline=.false.)
    write (svg%unit, "(a)") message
    call svg%close_attribute('text')
  end subroutine text

  subroutine dashed(point1, point2)
    type(point_type), intent(in) :: point1, point2
    type(point_type) :: converted1, converted2

    converted1 = svg_coordinate(point1)
    converted2 = svg_coordinate(point2)

    attrs = [ &
      & 'stroke'.pair.logo%color, &
      & 'x1'.pair.converted1%x, &
      & 'y1'.pair.converted1%y, &
      & 'x2'.pair.converted2%x, &
      & 'y2'.pair.converted2%y, &
      & 'stroke-dasharray'.pair.logo%dash_array, &
      & 'stroke-width'.pair.logo%dash_width]
    call svg%write_attribute('line', attrs)
  end subroutine dashed
end subroutine draw

end module module_logo
