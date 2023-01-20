module module_icon

use module_geometry
implicit none

!> Icon type
type :: icon_type

  integer :: num
  type(point_type) :: center
  real, allocatable :: x(:, :), y(:, :)
  type(points_type) :: piles(3)
  type(point_type), allocatable :: curve(:)

contains

  procedure :: read_parameters
  procedure :: compute_curve
  procedure :: blue_print

end type icon_type

contains

!> Initialization
subroutine read_parameters(self, filename)
  class(icon_type), intent(inout) :: self
  character(*), intent(in), optional :: filename
  real :: c(2), x(3, 3), y(3, 3)
  integer :: n
  namelist /parameters/ n, c, x, y
  integer :: unit
  logical :: exist

  if (present(filename)) then

    inquire (file=filename, exist=exist)
    if (.not. exist) error stop "Parameters.nml not found."

    open (newunit=unit, file=filename)
    read (unit=unit, nml=parameters)
    close (unit)

    self%num = n
    self%center = c
    self%x = x
    self%y = y

  else

    self%num = 50
    self%center = [-1.2800, +0.1247]
    self%x = reshape([ &
       & -3.00, -2.30, -1.90, &
       & +3.00, +2.30, +1.25, &
       & +0.00, +0.60, +1.25], [3, 3])

    self%y = reshape([ &
       & -3.00, -2.30, -1.75, &
       & +3.00, +0.60, +2.30, &
       & +0.45, +1.50, +1.75], [3, 3])

  end if
end subroutine read_parameters

subroutine compute_curve(self, output)
  class(icon_type), intent(inout) :: self
  logical, intent(in), optional :: output
  type(point_type), allocatable :: temp(:)
  type(point_type) :: point
  logical :: output_
  character(:), allocatable :: fmt
  integer :: unit, i

  associate ( &
    c => self%center, n => self%num, &
    x => self%x, y => self%y)

    self%piles(1)%points = [ &
       & point_type(x(3, 1), y(1, 1)), &
       & point_type(x(1, 1), y(1, 1)), &
       & point_type(x(1, 1), y(2, 1)), &
       & point_type(x(2, 1), y(2, 1)), &
       & point_type(x(3, 1), y(2, 1)), &
       & point_type(x(3, 1), y(3, 1))]
    temp = [self%piles(1)%points(1:3), &
       & bezier_curve(self%piles(1)%points(4:6), n)]
    self%curve = [mirror(temp, dim=1, level=c%x, reverse=.true.), &
       & temp, mirror(temp, dim=2, reverse=.true.)]

    point = self%curve(1)
    self%piles(2)%points = [ &
       & point_type(x(1, 2), y(1, 2)), &
       & point_type(x(1, 2), y(2, 2)), &
       & point_type(x(2, 2), y(2, 2)), &
       & point_type(x(2, 2), y(3, 2)), &
       & point_type(x(3, 2), y(3, 2)), &
       & point_type(point%x, y(3, 2))]
    temp = [self%piles(2)%points(1:2), &
       & bezier_curve(self%piles(2)%points(3:5), n), &
       & self%piles(2)%points(6)]
    self%curve = [self%curve, temp]

    point = self%curve(size(self%curve))
    self%piles(3)%points = [ &
       & point_type(point%x, y(1, 3)), &
       & point_type(x(1, 3), y(1, 3)), &
       & point_type(x(2, 3), y(1, 3)), &
       & point_type(x(2, 3), y(2, 3)), &
       & point_type(x(2, 3), y(3, 3)), &
       & point_type(x(3, 3), y(3, 3))]
    temp = [self%piles(3)%points(1), &
       & bezier_curve(self%piles(3)%points(2:4), n), &
       & self%piles(3)%points(5:6)]
    self%curve = [self%curve, temp, &
       & mirror(temp, dim=2, level=c%y, reverse=.true.), &
       & self%curve(1)]
  end associate

  if (present(output)) then
    output_ = output
  else
    output_ = .true.
  end if

  if (output_) then
    fmt = "(2(f12.6, 1x))"
    open (newunit=unit, file='./data/curve.dat')
    write (unit, "('#', a11, 1x, a12)") "x", "y"
    write (unit, fmt) self%curve
    close (unit)

    open (newunit=unit, file='./data/pile.dat')
    write (unit, "('#', a11, 1x, a12)") "x", "y"
    do i = 1, size(self%piles)
      write (unit, fmt) self%piles(i)%points
    end do
    write (unit, fmt) self%center
    close (unit)
  end if
end subroutine compute_curve

subroutine blue_print(self, dark_mode)
  class(icon_type), intent(in) :: self
  logical, intent(in), optional :: dark_mode
  real :: offset(2)
  integer :: unit
  character(:), allocatable :: fmt(:)
  character(:), allocatable :: msg, ls
  real :: bdy
  integer :: i, j, k
  character(:), allocatable :: mode

  if (present(dark_mode)) then
    if (dark_mode) then
      mode = 'dark'
    else
      mode = 'light'
    end if
  else
    mode = 'dark'
  end if

  associate ( &
    n => self%num, &
    c => self%center, &
    x => self%x, &
    y => self%y)

    open (newunit=unit, file='./data/plot_icon.plt')

    call write_("set terminal svg size 700, 700 font 'Cascadia Mono, 14'")
    call write_("set key noautotitle")
    call write_("light_color = '#6d5192'")
    call write_("dark_color = '#d79921'")
    call write_("color = "//mode//"_color")
    call write_("set xrange [-3.5:3.5]")
    call write_("set yrange [-3.5:3.5]")

    ls = 'set style line '
    call write_(ls//"1 linewidth 2 linecolor rgb color")
    call write_(ls//"2 linewidth 1 linecolor rgb color linetype 0")
    call write_(ls//"3 linewidth 1.5 linecolor rgb color linetype 0")
    call write_(ls//"4 linecolor rgb color pointtype 7 pointsize .75")
    call write_("set tics format ''")
    call write_("unset border")
    call write_("set tics scale 0")
    call write_("set output './data/icon_"//mode//".svg'")

    allocate (character(len=500) :: fmt(4))
    fmt(1) = "('set label', a, 1x, 'at', f6.2, ',', f6.2, "// &
       & "1x, 'center front textcolor rgb color')"
    fmt(2) = "('set arrow', 1x, "// &
       & "2(a, 1x, f6.2, ',', f6.2, 1x), 'nohead linestyle 2')"
    fmt(3) = "('set arrow', 1x, "// &
       & "2(a, 1x, f6.2, ',', f6.2, 1x), 'nohead linestyle 3')"
    fmt(4) = "(a, f6.2, ',', f6.2, "// &
       & "' textcolor rgb color offset ', f6.2, ',', f6.2, ' center')"

    allocate (character(len=9) :: msg)
    do j = 1, 3
      do i = 1, 3

        bdy = 3.1
        write (unit, trim(fmt(2))) 'from', x(i, j), -bdy, 'to', x(i, j), bdy
        write (unit, trim(fmt(2))) 'from', -bdy, y(i, j), 'to', bdy, y(i, j)
        write (msg, "(a, i0, ',', i0, a)") '"x_{', i, j, '}"'

        bdy = merge(-3.3, 3.3, mod((j - 1)*3 + i, 2) == 1)
        write (unit, trim(fmt(1))) msg, x(i, j), bdy
        msg(2:2) = 'y'
        write (unit, trim(fmt(1))) msg, bdy, y(i, j)

      end do
    end do

    bdy = 3.1
    write (unit, trim(fmt(3))) 'from', c%x, -bdy, 'to', c%x, bdy
    write (unit, trim(fmt(3))) 'from', -bdy, c%y, 'to', bdy, c%y

    bdy = 3.3
    msg = "'x_c'"
    write (unit, trim(fmt(1))) msg, c%x, -bdy
    msg = "'y_c'"
    write (unit, trim(fmt(1))) msg, -bdy, c%y

    write (unit, fmt(4)) "set label 'C' at ", c, 1.2, -0.6
    deallocate (msg)
    allocate (character(len=20) :: msg)

    k = 1
    do i = 1, 3
      do j = 1, size(self%piles(1)%points)

        select case (k)
        case (7)
          offset = [-1.2, -0.6]
        case (8)
          offset = [-1.2, +0.6]
        case (4:5, 13:16)
          offset = [+1.2, -0.6]
        case default
          offset = [+1.2, +0.6]
        end select

        write (msg, "(a, i0, a)") "set label '", k, "' at "
        write (unit, fmt(4)) msg, self%piles(i)%points(j), offset
        k = k + 1

      end do
    end do

    call write_("plot './data/curve.dat' with lines linestyle 1, \")
    call write_("     './data/pile.dat' using 1:2 with point linestyle 4")
  end associate

  close (unit)
  call execute_command_line("gnuplot ./data/plot_icon.plt")

contains
  subroutine write_(message_)
    character(*), intent(in) :: message_

    write (unit, "(a)") message_
  end subroutine write_
end subroutine blue_print

end module module_icon
