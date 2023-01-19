program main

   use module_geometry
   implicit none

   type(point_type), allocatable :: piles(:), curve(:), temp(:)
   type(point_type) :: point, center
   integer :: n, u
   real :: x(3, 3), y(3, 3), c(2)
   namelist /parameters/ n, c, x, y

   open (newunit=u, file='parameters.nml')
   read (unit=u, nml=parameters)
   close (u)

   center = c
   piles = [ &
      & point_type(x(3, 1), y(1, 1)), &
      & point_type(x(1, 1), y(1, 1)), &
      & point_type(x(1, 1), y(2, 1)), &
      & point_type(x(2, 1), y(2, 1)), &
      & point_type(x(3, 1), y(2, 1)), &
      & point_type(x(3, 1), y(3, 1))]
   temp = [piles(1:3), bezier_curve(piles(4:6), n)]
   curve = [mirror(temp, dim=1, level=center%x, reverse=.true.), &
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
      & mirror(temp, dim=2, level=center%y, reverse=.true.), curve(1)]

   open (newunit=u, file='./data/icon.dat')
   write (u, "('#', a11, 1x, a12)") "x", "y"
   write (u, "(2(f12.6, 1x))") curve
   close (u)

   call execute_command_line("mkdir -p ./data/")
   open (newunit=u, file='./data/plot_icon.plt')
   write (u, *) "set terminal png size 1000, 500 transparent truecolor"
   write (u, *) "set size 1, 1"
   write (u, *) "set key noautotitle"
   write (u, *) "light_color = '#6d5192'"
   write (u, *) "dark_color = '#d79921'"
   write (u, *) "color = light_color"
   write (u, *) "set xrange [-3.5:3.5]"
   write (u, *) "set yrange [-3.5:3.5]"
   write (u, *) "set style line 1 linewidth 3 linecolor rgb dark_color"
   write (u, *) "set style line 2 linewidth 3 linecolor rgb light_color"
   write (u, *) "set tics format ''"
   write (u, *) "set pixmap 1 './data/f1954.png' at first -3, -3 size first 6, 6 behind"
   write (u, "('set xtics', 1x, '(', 9(f6.2, ','), tl1, ')')") x
   write (u, "('set ytics', 1x, '(', 9(f6.2, ','), tl1, ')')") y

   write (u, *) "set output './data/icon_dark.png'"
   write (u, *) "set multiplot layout 1, 2"
   write (u, *) "set border linestyle 1"
   write (u, *) "set grid linecolor rgb dark_color"
   write (u, *) "plot './data/icon.dat' with lines linestyle 1"
   ! write (u, *) "unset grid"
   write (u, *) "set tics scale 0"
   write (u, *) "plot './data/icon.dat' with lines linestyle 1"
   write (u, *) "unset multiplot"

   write (u, *) "set output './data/icon_light.png'"
   write (u, *) "set multiplot layout 1, 2"
   write (u, *) "set border linestyle 2"
   write (u, *) "set grid linecolor rgb light_color"
   write (u, *) "plot './data/icon.dat' with lines linestyle 2"
   ! write (u, *) "unset grid"
   write (u, *) "set tics scale 0"
   write (u, *) "plot './data/icon.dat' with lines linestyle 2"
   write (u, *) "unset multiplot"

   write (u, *) "set terminal svg size 1000, 1000 dynamic enhanced"
   write (u, *) "set output './data/icon.svg'"
   write (u, *) "unset pixmap"
   write (u, *) "unset border"
   write (u, *) "unset tics"
   write (u, *) "plot './data/icon.dat' with lines linestyle 2"
   close (u)

   call execute_command_line("gnuplot ./data/plot_icon.plt")

end program main
