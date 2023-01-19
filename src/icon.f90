program main

   use module_geometry
   implicit none

   type(point_type), allocatable :: piles(:), curve(:), temp(:)
   type(point_type) :: point, center
   integer :: n, u, i, j
   character(:), allocatable :: fmt(:)
   character(:), allocatable :: msg
   real :: bdy
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
   write (u, "(a)") "set terminal png size 750, 750 transparent truecolor"
   write (u, "(a)") "set key noautotitle"
   write (u, "(a)") "light_color = '#6d5192'"
   write (u, "(a)") "dark_color = '#d79921'"
   write (u, "(a)") "color = dark_color"
   write (u, "(a)") "set xrange [-3.5:3.5]"
   write (u, "(a)") "set yrange [-3.5:3.5]"
   write (u, "(a)") "set style line 1 linewidth 3 linecolor rgb color"
   write (u, "(a)") "set style line 2 linewidth 1 linecolor rgb color linetype 0"
   write (u, "(a)") "set style line 3 linewidth 1.5 linecolor rgb color linetype 0"
   write (u, "(a)") "set tics format ''"
   ! write (u, "(a)") "set pixmap 1 './data/f1954.png' at first -3, -3 size first 6, 6 behind"

   write (u, "(a)") "set output './data/icon_dark.png'"
   write (u, "(a)") "unset border"
   write (u, "(a)") "set tics scale 0"

   allocate (character(len=500) :: fmt(3))
   fmt(1) = "('set label', a, 1x, 'at', f6.2, ',', f6.2, "// &
      & "1x, 'center front textcolor rgb color')"
   fmt(2) = "('set arrow', 1x, 2(a, 1x, f6.2, ',', f6.2, 1x), 'nohead linestyle 2')"
   fmt(3) = "('set arrow', 1x, 2(a, 1x, f6.2, ',', f6.2, 1x), 'nohead linestyle 3')"

   allocate (character(len=9) :: msg)
   do j = 1, 3
      do i = 1, 3

         bdy = 3.1
         write (u, trim(fmt(2))) 'from', x(i, j), -bdy, 'to', x(i, j), bdy
         write (u, trim(fmt(2))) 'from', -bdy, y(i, j), 'to', bdy, y(i, j)
         write (msg, "(a, i0, ',', i0, a)") '"x_{', i, j, '}"'

         bdy = merge(-3.3, 3.3, mod((j - 1)*3 + i, 2) == 1)
         write (u, trim(fmt(1))) msg, x(i, j), bdy
         msg(2:2) = 'y'
         write (u, trim(fmt(1))) msg, bdy, y(i, j)

      end do
   end do

   bdy = 3.1
   write (u, trim(fmt(3))) 'from', center%x, -bdy, 'to', center%x, bdy
   write (u, trim(fmt(3))) 'from', -bdy, center%y, 'to', bdy, center%y

   bdy = 3.3
   msg = "'x_c'"
   write (u, trim(fmt(1))) msg, center%x, -bdy
   msg = "'y_c'"
   write (u, trim(fmt(1))) msg, -bdy, center%y

   write (u, "(a)") "plot './data/icon.dat' with lines linestyle 1"

   write (u, "(a)") "set output './data/icon_light.png'"
   write (u, "(a)") "color = light_color"
   write (u, "(a)") "replot"

   write (u, "(a)") "set terminal svg size 1000, 1000 dynamic enhanced"
   write (u, "(a)") "set output './data/icon.svg'"
   write (u, "(a)") "unset pixmap"
   write (u, "(a)") "unset border"
   write (u, "(a)") "unset tics"
   write (u, "(a)") "plot './data/icon.dat' with lines linestyle 1"
   close (u)

   call execute_command_line("gnuplot ./data/plot_icon.plt")

end program main
