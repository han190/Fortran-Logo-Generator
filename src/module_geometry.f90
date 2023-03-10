module module_geometry

use module_utility, only: str
implicit none

public :: point_type
public :: points_type
public :: assignment(=)
public :: operator(+)
public :: operator(-)
public :: operator(*)
public :: bezier_curve
public :: mirror
public :: centroid
public :: rounded_corner
private

!> Point type
type :: point_type
  real :: x
  real :: y
end type point_type

!> Points type
type :: points_type
  type(point_type), allocatable :: points(:)
end type points_type

interface assignment(=)
  module procedure :: init_point_real
  module procedure :: init_point_integer
end interface assignment(=)

interface operator(+)
  module procedure :: add_point
end interface operator(+)

interface operator(-)
  module procedure :: subtract_point
  module procedure :: subtract_points
end interface operator(-)

interface operator(*)
  module procedure :: multiply_point_real
  module procedure :: multiply_point_real_re
  module procedure :: multiply_point_integer
  module procedure :: multiply_point_integer_re
end interface operator(*)

interface mirror
  module procedure :: mirror_xy
end interface mirror

!> An abstract interface for Bezier curves
abstract interface
  pure function bezier_proc(p, t) result(b)
    import :: point_type

    type(point_type), intent(in) :: p(0:)
    real, intent(in) :: t
    type(point_type) :: b
  end function bezier_proc
end interface

contains

!> Initialize point with real
pure subroutine init_point_real(point, arr)
  type(point_type), intent(out) :: point
  real, intent(in) :: arr(:)

  point = point_type(arr(1), arr(2))
end subroutine init_point_real

pure subroutine init_point_integer(point, arr)
  type(point_type), intent(out) :: point
  integer, intent(in) :: arr(:)

  point = point_type(real(arr(1)), real(arr(2)))
end subroutine init_point_integer

!> Addition
elemental function add_point(point1, point2) result(point)
  type(point_type), intent(in) :: point1, point2
  type(point_type) :: point

  point = point_type(point1%x + point2%x, &
    & point1%y + point2%y)
end function add_point

!> Subtraction
elemental function subtract_point(point1, point2) result(point)
  type(point_type), intent(in) :: point1, point2
  type(point_type) :: point

  point = point_type(point1%x - point2%x, &
    & point1%y - point2%y)
end function subtract_point

!> Multiply
elemental function multiply_point_real(point1, a) result(point)
  type(point_type), intent(in) :: point1
  real, intent(in) :: a
  type(point_type) :: point

  point = point_type(point1%x*a, point1%y*a)
end function multiply_point_real

elemental function multiply_point_real_re(a, point1) result(point)
  real, intent(in) :: a
  type(point_type), intent(in) :: point1
  type(point_type) :: point

  point = multiply_point_real(point1, a)
end function multiply_point_real_re

elemental function multiply_point_integer(point1, a) result(point)
  type(point_type), intent(in) :: point1
  integer, intent(in) :: a
  type(point_type) :: point

  point = point_type(point1%x*a, point1%y*a)
end function multiply_point_integer

elemental function multiply_point_integer_re(a, point1) result(point)
  integer, intent(in) :: a
  type(point_type), intent(in) :: point1
  type(point_type) :: point

  point = multiply_point_integer(point1, a)
end function multiply_point_integer_re

pure function linear_bezier(p, t) result(b)
  type(point_type), intent(in) :: p(0:)
  real, intent(in) :: t
  type(point_type) :: b

  b = p(0) + t*(p(1) - p(0))
end function linear_bezier

pure function quadratic_bezier(p, t) result(b)
  type(point_type), intent(in) :: p(0:)
  real, intent(in) :: t
  type(point_type) :: b

  b = (1 - t)**2*p(0) + 2*(1 - t)*t*p(1) + t**2*p(2)
end function quadratic_bezier

pure function cubic_bezier(p, t) result(b)
  type(point_type), intent(in) :: p(0:)
  real, intent(in) :: t
  type(point_type) :: b

  b = (1 - t)**3*p(0) + 3*(1 - t)**2*t*p(1) + &
    & 3*(1 - t)*t*p(2) + t**3*p(3)
end function cubic_bezier

elemental function factorial(n) result(ret)
  integer, intent(in) :: n
  integer :: ret

  ret = int(gamma(real(n) + 1.0))
  ! ret = gamma(n + 1)
end function factorial

elemental function bionomial(n, k) result(ret)
  integer, intent(in) :: n, k
  integer :: ret

  ret = factorial(n)/(factorial(k)*factorial(n - k))
end function bionomial

pure function general_bezier(p, t) result(b)
  type(point_type), intent(in) :: p(0:)
  real, intent(in) :: t
  type(point_type) :: b
  real :: c
  integer :: n, i

  n = size(p) - 1
  b = [0., 0.]

  do i = 0, n
    c = bionomial(n, i)*(1 - t)**(n - i)*t**i
    b = b + c*p(i)
  end do
end function general_bezier

pure function bezier_curve(control_points, num, end_point) result(curve)
  type(point_type), intent(in) :: control_points(0:)
  integer, intent(in) :: num
  logical, intent(in), optional :: end_point
  type(point_type), allocatable :: curve(:)
  logical :: end_point_
  procedure(bezier_proc), pointer :: bezier
  integer :: i, n

  n = size(control_points)
  select case (n)
  case (0:1)
    error stop "Invalid control points."
  case (2)
    bezier => linear_bezier
  case (3)
    bezier => quadratic_bezier
  case (4)
    bezier => cubic_bezier
  case (5:12)
    bezier => general_bezier
  case default
    error stop "size(control_points) > 12."
  end select

  curve = [(bezier(control_points, 1.0/num*i), i=0, num)]
  nullify (bezier)

  if (present(end_point)) then
    end_point_ = end_point
  else
    end_point_ = .false.
  end if
  if (end_point_) curve = curve(:num - 1)
end function bezier_curve

pure function mirror_xy(points, dim, reverse, level) result(mirrored)
  type(point_type), intent(in) :: points(:)
  integer, intent(in) :: dim
  logical, intent(in), optional :: reverse
  real, intent(in), optional :: level
  type(point_type), allocatable :: mirrored(:)
  real :: level_
  logical :: reverse_
  integer :: i

  if (present(level)) then
    level_ = level
  else
    level_ = 0.
  end if

  if (present(reverse)) then
    reverse_ = reverse
  else
    reverse_ = .false.
  end if

  allocate (mirrored, mold=points)
  select case (dim)
  case (1) !> x
    do i = 1, size(mirrored)
      mirrored(i)%x = points(i)%x + &
        & (level_ - points(i)%x)*2
      mirrored(i)%y = points(i)%y
    end do
  case (2) !> y
    do i = 1, size(mirrored)
      mirrored(i)%x = points(i)%x
      mirrored(i)%y = points(i)%y + &
        & (level_ - points(i)%y)*2
    end do
  case default
    error stop "Invalid dim."
  end select

  if (reverse_) mirrored &
    & = mirrored(size(mirrored):1:-1)
end function mirror_xy

pure function centroid(points) result(center)
  type(point_type), intent(in) :: points(0:)
  type(point_type) :: center
  real :: a
  integer :: n, i, j

  associate (p => points)
    n = size(p) - 1
    a = 0.0
    do i = 0, n - 1
      a = a + p(i)%x*p(i + 1)%y - p(i + 1)%x*p(i)%y
    end do
    a = 0.5*a

    associate ( &
      x => center%x, &
      y => center%y)

      x = 0.0
      y = 0.0

      do i = 0, n - 1
        j = i + 1
        x = x + (p(i)%x + p(j)%x)* &
          & (p(i)%x*p(j)%y - p(j)%x*p(i)%y)
        y = y + (p(i)%y + p(j)%y)* &
          & (p(i)%x*p(j)%y - p(j)%x*p(i)%y)
      end do

      x = x/(6.0*a)
      y = y/(6.0*a)
    end associate
  end associate
end function centroid

pure function rounded_corner(point, radius, quadrant, &
  & num_points, clockwise) result(points)

  type(point_type), intent(in) :: point
  real, intent(in) :: radius
  integer, intent(in) :: quadrant(:)
  integer, intent(in) :: num_points
  logical, intent(in) :: clockwise
  type(point_type), allocatable :: points(:)
  type(point_type) :: init, finl

  if (all(quadrant == [+1, +1])) then

    if (clockwise) then
      init = point_type(point%x + radius, point%y)
      finl = point_type(point%x, point%y + radius)
    else
      init = point_type(point%x, point%y + radius)
      finl = point_type(point%x + radius, point%y)
    end if

  else if (all(quadrant == [-1, +1])) then

    if (clockwise) then
      init = point_type(point%x, point%y + radius)
      finl = point_type(point%x - radius, point%y)
    else
      init = point_type(point%x - radius, point%y)
      finl = point_type(point%x, point%y + radius)
    end if

  else if (all(quadrant == [-1, -1])) then

    if (clockwise) then
      init = point_type(point%x - radius, point%y)
      finl = point_type(point%x, point%y - radius)
    else
      init = point_type(point%x, point%y - radius)
      finl = point_type(point%x - radius, point%y)
    end if

  else if (all(quadrant == [+1, -1])) then

    if (clockwise) then
      init = point_type(point%x, point%y - radius)
      finl = point_type(point%x + radius, point%y)
    else
      init = point_type(point%x + radius, point%y)
      finl = point_type(point%x, point%y - radius)
    end if

  else
    error stop "Invalid quadrant"
  end if
  points = bezier_curve([init, point, finl], num_points)
end function rounded_corner

pure function subtract_points(points, offset) result(subtracted)
  type(point_type), intent(in) :: points(:)
  real, intent(in) :: offset(:)
  type(point_type), allocatable :: subtracted(:)
  integer :: i

  if (allocated(subtracted)) deallocate (subtracted)
  allocate (subtracted(size(points)))
  do i = 1, size(points)
    subtracted(i)%x = points(i)%x - offset(1)
    subtracted(i)%y = points(i)%y - offset(2)
  end do
end function subtract_points

end module module_geometry
