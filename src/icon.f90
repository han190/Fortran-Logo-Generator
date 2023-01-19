program main

   use module_geometry
   use module_icon
   implicit none

   type(icon_type) :: icon
   type(points_type) :: piles(3)
   type(point_type), allocatable :: curve(:)
   integer :: u, i, j, k
   real :: offset(2)
   character(:), allocatable :: fmt(:)
   character(:), allocatable :: msg
   real :: bdy

   icon = get_parameters()
   call get_curve(icon, piles, curve)

   open (newunit=u, file='./data/curve.dat')
   write (u, "('#', a11, 1x, a12)") "x", "y"
   write (u, "(2(f12.6, 1x))") curve
   close (u)

   open (newunit=u, file='./data/pile.dat')
   write (u, "('#', a11, 1x, a12, 1x, a12)") "x", "y", "label"
   j = 1
   k = 1
   do j = 1, 3
      do i = 1, size(piles(j)%points)
         write (u, "(2(f12.6, 1x), i12)") piles(j)%points(i), k
         k = k + 1
      end do
   end do
   close (u)

   associate ( &
      n => icon%num, &
      c => icon%center, &
      x => icon%x, &
      y => icon%y)

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
      write (u, "(a)") "set style line 4 linecolor rgb color pointtype 6 pointsize 2"
      write (u, "(a)") "set tics format ''"
      ! write (u, "(a)") "set pixmap 1 './data/f1954.png' at first -3, -3 size first 6, 6 behind"

      write (u, "(a)") "set output './data/icon_dark.png'"
      write (u, "(a)") "unset border"
      write (u, "(a)") "set tics scale 0"

      allocate (character(len=500) :: fmt(4))
      fmt(1) = "('set label', a, 1x, 'at', f6.2, ',', f6.2, "// &
         & "1x, 'center front textcolor rgb color')"
      fmt(2) = "('set arrow', 1x, 2(a, 1x, f6.2, ',', f6.2, 1x), 'nohead linestyle 2')"
      fmt(3) = "('set arrow', 1x, 2(a, 1x, f6.2, ',', f6.2, 1x), 'nohead linestyle 3')"
      fmt(4) = "(a, f6.2, ',', f6.2, ' textcolor rgb dark_color offset ', f6.2, ',', f6.2)"

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
      write (u, trim(fmt(3))) 'from', c%x, -bdy, 'to', c%x, bdy
      write (u, trim(fmt(3))) 'from', -bdy, c%y, 'to', bdy, c%y

      bdy = 3.3
      msg = "'x_c'"
      write (u, trim(fmt(1))) msg, c%x, -bdy
      msg = "'y_c'"
      write (u, trim(fmt(1))) msg, -bdy, c%y

      write (u, "(a)") "plot './data/curve.dat' with lines linestyle 1, \"
      write (u, "(a)") "'./data/pile.dat' using 1:2:3 with point linestyle 4"

      deallocate (msg)
      allocate (character(len=20) :: msg)
      k = 1
      do i = 1, 3
         do j = 1, size(piles(1)%points)

            select case (k)
            case (7)
               offset = [-1.8, -1.5]
            case (8)
               offset = [-1.8, 1.0]
            case (4, 13:16)
               offset = [1.0, -1.2]
            case default
               offset = [1, 1]
            end select

            write (msg, "(a, i0, a)") "set label '", k, "' at "
            write (u, fmt(4)) msg, piles(i)%points(j), offset
            k = k + 1

         end do
      end do

      write (u, "(a)") "set output './data/icon_light.png'"
      write (u, "(a)") "color = light_color"
      write (u, "(a)") "replot"

      write (u, "(a)") "set terminal svg size 1000, 1000 dynamic enhanced"
      write (u, "(a)") "set output './data/icon.svg'"
      write (u, "(a)") "unset pixmap"
      write (u, "(a)") "unset border"
      write (u, "(a)") "unset tics"
      write (u, "(a)") "plot './data/curve.dat' with lines linestyle 1"
      close (u)

      call execute_command_line("gnuplot ./data/plot_icon.plt")
   end associate

end program main
