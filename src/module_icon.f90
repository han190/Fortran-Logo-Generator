module module_icon

use module_geometry
implicit none

!> Icon type
type :: icon_type

  integer :: num_curves
  real :: side_length
  real :: corner_radius
  type(point_type) :: center
  real, allocatable :: x(:, :)
  real, allocatable :: y(:, :)
  type(points_type) :: letter_piles(3)
  type(point_type), allocatable :: letter_F(:)
  type(point_type), allocatable :: boundary_piles(:)
  type(point_type), allocatable :: boundary(:)

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
  real :: c(2), x(3, 3), y(3, 3), s, r
  integer :: n
  namelist /parameters/ n, s, r, c, x, y
  integer :: unit
  logical :: exist

  if (present(filename)) then

    inquire (file=filename, exist=exist)
    if (.not. exist) error stop "Parameters.nml not found."

    open (newunit=unit, file=filename)
    read (unit=unit, nml=parameters)
    close (unit)

    self%num_curves = n
    self%side_length = s
    self%corner_radius = r
    self%center = c
    self%x = x
    self%y = y

  else

    self%num_curves = 50
    self%side_length = 7.0
    self%corner_radius = 0.5
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
    c => self%center, &
    n => self%num_curves, &
    x => self%x, &
    y => self%y)

    associate (p => self%letter_piles(1))
      p%points = [ &
        & point_type(x(3, 1), y(1, 1)), &
        & point_type(x(1, 1), y(1, 1)), &
        & point_type(x(1, 1), y(2, 1)), &
        & point_type(x(2, 1), y(2, 1)), &
        & point_type(x(3, 1), y(2, 1)), &
        & point_type(x(3, 1), y(3, 1))]
      temp = [p%points(1:3), &
        & bezier_curve(p%points(4:6), n)]
      self%letter_F = [ &
        & mirror(temp, 1, .true., c%x), &
        & temp, mirror(temp, 2, .true., 0.0)]
    end associate

    associate (p => self%letter_piles(2))
      point = self%letter_F(1)
      p%points = [ &
        & point_type(x(1, 2), y(1, 2)), &
        & point_type(x(1, 2), y(2, 2)), &
        & point_type(x(2, 2), y(2, 2)), &
        & point_type(x(2, 2), y(3, 2)), &
        & point_type(x(3, 2), y(3, 2)), &
        & point_type(point%x, y(3, 2))]
      temp = [p%points(1:2), &
        & bezier_curve(p%points(3:5), n), &
        & p%points(6)]
      self%letter_F = [self%letter_F, temp]
    end associate

    associate (p => self%letter_piles(3))
      point = self%letter_F(size(self%letter_F))
      p%points = [ &
        & point_type(point%x, y(1, 3)), &
        & point_type(x(1, 3), y(1, 3)), &
        & point_type(x(2, 3), y(1, 3)), &
        & point_type(x(2, 3), y(2, 3)), &
        & point_type(x(2, 3), y(3, 3)), &
        & point_type(x(3, 3), y(3, 3))]
      temp = [p%points(1), &
        & bezier_curve(p%points(2:4), n), &
        & p%points(5:6)]
      self%letter_F = [self%letter_F, temp, &
        & mirror(temp, 2, .true., c%y), &
        & self%letter_F(1)]
    end associate

    associate ( &
      s => self%side_length, &
      r => self%corner_radius)

      self%boundary_piles = [ &
        & point_type(-s/2 + r, -s/2), &
        & point_type(-s/2, -s/2), &
        & point_type(-s/2, -s/2 + r), &
        & point_type(-s/2, +s/2 - r), &
        & point_type(-s/2, +s/2), &
        & point_type(-s/2 + r, +s/2), &
        & point_type(+s/2 - r, +s/2), &
        & point_type(+s/2, +s/2), &
        & point_type(+s/2, +s/2 - r), &
        & point_type(+s/2, -s/2 + r), &
        & point_type(+s/2, -s/2), &
        & point_type(+s/2 - r, -s/2)]
    end associate

    associate (p => self%boundary_piles)
      self%boundary = [ &
        & bezier_curve(p(1:3), n), &
        & bezier_curve(p(4:6), n), &
        & bezier_curve(p(7:9), n), &
        & bezier_curve(p(10:12), n), p(1)]
    end associate
  end associate

  if (present(output)) then
    output_ = output
  else
    output_ = .true.
  end if

  if (output_) then
    fmt = "(2(f12.6, 1x))"
    open (newunit=unit, file='./data/letter_F.dat')
    write (unit, "('#', a11, 1x, a12)") "x", "y"
    write (unit, fmt) self%letter_F
    close (unit)

    open (newunit=unit, file='./data/letter_pile.dat')
    write (unit, "('#', a11, 1x, a12)") "x", "y"
    do i = 1, size(self%letter_piles)
      write (unit, fmt) self%letter_piles(i)%points
    end do
    write (unit, fmt) self%center
    close (unit)

    open (newunit=unit, file='./data/boundary_pile.dat')
    write (unit, "('#', a11, 1x, a12)") "x", "y"
    write (unit, fmt) self%boundary_piles
    close (unit)

    open (newunit=unit, file='./data/boundary.dat')
    write (unit, "('#', a11, 1x, a12)") "x", "y"
    write (unit, fmt) self%boundary
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
    n => self%num_curves, c => self%center, &
    x => self%x, y => self%y)

    open (newunit=unit, file='./data/.plot.plt')

    call write_("set terminal svg size 900, 900 font 'Cascadia Mono, 15'")
    call write_("set key noautotitle")
    call write_("light_color = '#6d5192'")
    call write_("dark_color = '#d79921'")
    call write_("color = "//mode//"_color")
    call write_("set xrange [-4.5:4.5]")
    call write_("set yrange [-4.5:4.5]")

    ls = 'set style line '
    call write_(ls//"1 linewidth 2 linecolor rgb color")
    call write_(ls//"2 linewidth 1 linecolor rgb color linetype 0")
    call write_(ls//"3 linewidth 1.5 linecolor rgb color linetype 0")
    call write_(ls//"4 linecolor rgb color pointtype 7 pointsize .75")
    call write_("set tics format ''")
    call write_("unset border")
    call write_("set tics scale 0")
    call write_("set output './data/blueprint_"//mode//".svg'")

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

        bdy = self%side_length/2
        write (unit, trim(fmt(2))) 'from', x(i, j), -bdy, 'to', x(i, j), bdy
        write (unit, trim(fmt(2))) 'from', -bdy, y(i, j), 'to', bdy, y(i, j)
        write (msg, "(a, i0, ',', i0, a)") '"X(', i, j, ')"'

        bdy = merge(-3.85, 3.85, mod((j - 1)*3 + i, 2) == 1)
        write (unit, trim(fmt(1))) msg, x(i, j), bdy
        bdy = merge(-4.05, 4.05, mod((j - 1)*3 + i, 2) == 1)
        msg(2:2) = 'Y'
        write (unit, trim(fmt(1))) msg, bdy, y(i, j)

      end do
    end do

    bdy = self%side_length/2
    write (unit, trim(fmt(3))) 'from', -bdy, -bdy, 'to', -bdy, +bdy
    write (unit, trim(fmt(3))) 'from', +bdy, -bdy, 'to', +bdy, +bdy
    write (unit, trim(fmt(3))) 'from', -bdy, -bdy, 'to', +bdy, -bdy
    write (unit, trim(fmt(3))) 'from', -bdy, +bdy, 'to', +bdy, +bdy

    write (unit, trim(fmt(3))) 'from', c%x, -bdy, 'to', c%x, bdy
    write (unit, trim(fmt(3))) 'from', -bdy, c%y, 'to', bdy, c%y

    bdy = 3.85
    msg = "'XC'"
    write (unit, trim(fmt(1))) msg, c%x, -bdy
    msg = "'YC'"
    write (unit, trim(fmt(1))) msg, -bdy, c%y

    write (unit, fmt(4)) "set label 'C' at ", c, 1.2, -0.6
    deallocate (msg)
    allocate (character(len=20) :: msg)

    k = 1
    do i = 1, 3
      do j = 1, size(self%letter_piles(i)%points)

        select case (k)
        case (7)
          offset = [-1.4, -0.6]
        case (8)
          offset = [-1.4, +0.6]
        case (4:5, 13:16)
          offset = [+1.4, -0.6]
        case default
          offset = [+1.4, +0.6]
        end select

        write (msg, "(a, i0, a)") "set label 'P", k, "' at "
        write (unit, trim(fmt(4))) msg, &
          & self%letter_piles(i)%points(j), offset
        k = k + 1

      end do
    end do

    do i = 1, size(self%boundary_piles)
      select case (i)
      case (2)
        offset = [-1.4, -0.6]
      case (4:6)
        offset = [-1.4, +0.6]
      case (7, 9)
        offset = [-1.4, -0.6]
      case (10:12)
        offset = [+1.8, -0.6]
      case default
        offset = [+1.4, +0.6]
      end select

      write (msg, "(a, i0, a)") "set label 'B", i, "' at "
      write (unit, trim(fmt(4))) msg, &
        & self%boundary_piles(i), offset
    end do

    call write_("plot './data/letter_F.dat' with lines linestyle 1, \")
    call write_("     './data/boundary.dat' with lines linestyle 1, \")
    call write_("     './data/letter_pile.dat' using 1:2 with point linestyle 4, \")
    call write_("     './data/boundary_pile.dat' using 1:2 with point linestyle 4")
  end associate

  close (unit)
  call execute_command_line("gnuplot ./data/.plot.plt")

contains
  subroutine write_(message_)
    character(*), intent(in) :: message_

    write (unit, "(a)") message_
  end subroutine write_
end subroutine blue_print

end module module_icon
