module module_icon

   use module_geometry
   implicit none

   !> Icon type
   type :: icon_type
      integer :: num
      type(point_type) :: center
      real, allocatable :: x(:, :)
      real, allocatable :: y(:, :)
   end type icon_type

contains

   !> Initialization
   function get_parameters(filename) result(icon)
      character(*), intent(in), optional :: filename
      type(icon_type) :: icon
      real :: c(2), x(3, 3), y(3, 3)
      integer :: n
      namelist /parameters/ n, c, x, y
      integer :: unit
      character(:), allocatable :: file
      logical :: exist

      if (present(filename)) then
         file = filename
      else
         file = 'parameters.nml'
      end if

      inquire (file=file, exist=exist)
      if (.not. exist) error stop "Parameters.nml not found."

      open (newunit=unit, file='parameters.nml')
      read (unit=unit, nml=parameters)
      close (unit)

      icon%num = n
      icon%center = c
      icon%x = x
      icon%y = y
   end function get_parameters

   pure subroutine get_curve(icon, piles, curve)
      type(icon_type), intent(in) :: icon
      type(points_type), intent(out) :: piles(3)
      type(point_type), allocatable, intent(out) :: curve(:)
      type(point_type), allocatable :: temp(:)
      type(point_type) :: point

      associate ( &
         c => icon%center, n => icon%num, &
         x => icon%x, y => icon%y)
         
         piles(1)%points = [ &
            & point_type(x(3, 1), y(1, 1)), &
            & point_type(x(1, 1), y(1, 1)), &
            & point_type(x(1, 1), y(2, 1)), &
            & point_type(x(2, 1), y(2, 1)), &
            & point_type(x(3, 1), y(2, 1)), &
            & point_type(x(3, 1), y(3, 1))]
         temp = [piles(1)%points(1:3), &
            & bezier_curve(piles(1)%points(4:6), n)]
         curve = [mirror(temp, dim=1, level=c%x, reverse=.true.), &
            & temp, mirror(temp, dim=2, reverse=.true.)]

         point = curve(1)
         piles(2)%points = [ &
            & point_type(x(1, 2), y(1, 2)), &
            & point_type(x(1, 2), y(2, 2)), &
            & point_type(x(2, 2), y(2, 2)), &
            & point_type(x(2, 2), y(3, 2)), &
            & point_type(x(3, 2), y(3, 2)), &
            & point_type(point%x, y(3, 2))]
         temp = [piles(2)%points(1:2), &
            & bezier_curve(piles(2)%points(3:5), n), &
            & piles(2)%points(6)]
         curve = [curve, temp]

         point = curve(size(curve))
         piles(3)%points = [ &
            & point_type(point%x, y(1, 3)), &
            & point_type(x(1, 3), y(1, 3)), &
            & point_type(x(2, 3), y(1, 3)), &
            & point_type(x(2, 3), y(2, 3)), &
            & point_type(x(2, 3), y(3, 3)), &
            & point_type(x(3, 3), y(3, 3))]
         temp = [piles(3)%points(1), &
            & bezier_curve(piles(3)%points(2:4), n), &
            & piles(3)%points(5:6)]
         curve = [curve, temp, &
            & mirror(temp, dim=2, level=c%y, reverse=.true.), curve(1)]
      end associate
   end subroutine get_curve

end module module_icon
