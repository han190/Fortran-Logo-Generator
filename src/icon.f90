program main

   use module_geometry
   implicit none

   type(point_type), allocatable :: piles(:), curve(:), temp(:)
   type(point_type) :: point, c
   integer :: n, u
   real :: x(3, 3), y(3, 3)

   c = point_type(-1.2800, +0.1247)
   n = 50

   x = reshape([[-3.00, -2.30, -1.90], &
      & [+3.00, +2.30, +1.25], &
      & [+0.00, +0.60, +1.25]], [3, 3])

   y = reshape([[-3.00, -2.30, -1.75], &
      & [+3.00, +0.60, +2.30], &
      & [+0.45, +1.50, +1.75]], [3, 3])

   piles = [ &
      & point_type(x(3, 1), y(1, 1)), &
      & point_type(x(1, 1), y(1, 1)), &
      & point_type(x(1, 1), y(2, 1)), &
      & point_type(x(2, 1), y(2, 1)), &
      & point_type(x(3, 1), y(2, 1)), &
      & point_type(x(3, 1), y(3, 1))]
   temp = [piles(1:3), bezier_curve(piles(4:6), n)]
   curve = [mirror(temp, dim=1, level=c%x, reverse=.true.), &
      & temp, mirror(temp, dim=2, reverse=.true.)]

   point = curve(1)
   piles = [ &
      & point_type(x(1, 2), y(1, 2)), &
      & point_type(x(1, 2), y(2, 2)), &
      & point_type(x(2, 2), y(2, 2)), &
      & point_type(x(2, 2), y(3, 2)), &
      & point_type(x(3, 2), y(3, 2)), &
      & point_type(point%x, y(3, 2))]
   temp = [piles(1:2), bezier_curve(piles(3:5), n), piles(6)]
   curve = [curve, temp]

   point = curve(size(curve))
   piles = [ &
      & point_type(point%x, y(1, 3)), &
      & point_type(x(1, 3), y(1, 3)), &
      & point_type(x(2, 3), y(1, 3)), &
      & point_type(x(2, 3), y(2, 3)), &
      & point_type(x(2, 3), y(3, 3)), &
      & point_type(x(3, 3), y(3, 3))]
   temp = [piles(1), bezier_curve(piles(2:4), n), piles(5:6)]
   curve = [curve, temp, &
      & mirror(temp, dim=2, level=c%y, reverse=.true.), curve(1)]

   open (newunit=u, file='./data/icon.dat')
   write (u, "('#', a11, 1x, a12)") "x", "y"
   write (u, "(2(f12.6, 1x))") curve
   close (u)

   call execute_command_line("mkdir -p ./data/")
   call execute_command_line("gnuplot ./src/plot_icon.plt")

end program main
