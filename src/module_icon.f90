module module_logo

use module_geometry
use module_utility
implicit none

!> Icon type
type :: logo_type
  !> Number of points used when
  !> drawing curves.
  integer :: num_curves
  !> Side length of the logo.
  !> (Assuming square canvas)
  real :: side_length
  !> Radius of rounded corners.
  real :: corner_radius
  !> A reference point
  type(point_type) :: reference_point
  !> Offset for hook of "F"
  real :: hook_offset
  !> Essential horiozntal coordinates.
  real, allocatable :: x(:, :)
  !> Essential vertical coordinates
  real, allocatable :: y(:, :)
  !> Piles for letter "F"
  type(point_type), allocatable :: letter_piles(:)
  !> The letter "F"
  type(point_type), allocatable :: letter_F(:)
  !> Piles for the rounded corner square
  type(point_type), allocatable :: boundary_piles(:)
  !> The rounded corner square
  type(point_type), allocatable :: boundary(:)
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
  class(logo_type), intent(inout) :: self
  character(*), intent(in), optional :: filename
  real :: reference_point(2), x(3, 3), y(3, 3)
  real :: side_length, corner_radius, hook_offset
  integer :: num_curves
  namelist /parameters/ num_curves, side_length, &
    & corner_radius, hook_offset, reference_point, x, y
  integer :: unit
  logical :: exist

  if (present(filename)) then

    inquire (file=filename, exist=exist)
    if (.not. exist) error stop "Parameters.nml not found."

    open (newunit=unit, file=filename)
    read (unit=unit, nml=parameters)
    close (unit)

    self%num_curves = num_curves
    self%side_length = side_length
    self%corner_radius = corner_radius
    self%hook_offset = hook_offset
    self%reference_point = reference_point
    self%x = x
    self%y = y

  else

    !> IF namelist is not provided, use
    !> use default a parameter set.
    self%num_curves = 100
    self%side_length = 185
    self%corner_radius = 36
    self%hook_offset = 2.5
    self%reference_point = [-29.45, +3.15]
    self%x = reshape([ &
       & -69.00, -53.00, -43.7, &
       & +69.00, +53.00, +28.75, &
       & +00.00, +13.80, +28.75], [3, 3])

    self%y = reshape([ &
       & -75.00, -57.50, -43.75, &
       & +75.00, +15.00, +57.50, &
       & +11.25, +37.50, +43.75], [3, 3])

  end if
end subroutine initialize

subroutine compute(self, output)
  class(logo_type), intent(inout) :: self
  logical, intent(in), optional :: output
  type(point_type), allocatable :: temp(:)
  type(point_type) :: point
  logical :: output_
  character(:), allocatable :: fmt
  integer :: unit, i

  allocate (self%letter_piles(18))
  associate ( &
    c => self%reference_point, &
    n => self%num_curves, &
    x => self%x, &
    y => self%y)

    associate (F => self%letter_piles(1:6))
      F = [point_type(x(3, 1), y(1, 1)), &
        & point_type(x(1, 1), y(1, 1)), &
        & point_type(x(1, 1), y(2, 1)), &
        & point_type(x(2, 1), y(2, 1)), &
        & point_type(x(3, 1), y(2, 1)), &
        & point_type(x(3, 1), y(3, 1))]
      temp = [F(1:3), bezier_curve(F(4:6), n)]
      self%letter_F = [ &
        & mirror(temp, 1, .true., c%x), &
        & temp, mirror(temp, 2, .true., 0.0)]
    end associate

    associate (F => self%letter_piles(7:12))
      point = self%letter_F(1)
      F = [point_type(x(1, 2), y(1, 2)), &
        & point_type(x(1, 2), y(2, 2)), &
        & point_type(x(2, 2), y(2, 2)), &
        & point_type(x(2, 2), y(3, 2)), &
        & point_type(x(3, 2), y(3, 2)), &
        & point_type(point%x, y(3, 2))]
      temp = [F(1:2), bezier_curve(F(3:5), n), F(6)]
      self%letter_F = [self%letter_F, temp]
    end associate

    associate (F => self%letter_piles(13:18))
      point = self%letter_F(size(self%letter_F))
      F = [ &
        & point_type(point%x, y(1, 3)), &
        & point_type(x(1, 3), y(1, 3)), &
        & point_type(x(2, 3), y(1, 3)), &
        & point_type(x(2, 3), y(2, 3)), &
        & point_type(x(2, 3), y(3, 3)), &
        & point_type(x(3, 3), y(3, 3))]
      temp = [F(1), bezier_curve(F(2:4), n), F(5:6)]
      temp = [temp, mirror(temp, 2, .true., c%y), &
        & self%letter_F(1)]
      temp(n + 3)%y = temp(n + 3)%y - self%hook_offset
      temp(n + 4)%y = temp(n + 4)%y - self%hook_offset
      self%letter_F = [self%letter_F, temp]
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

    associate (B => self%boundary_piles)
      self%boundary = [ &
        & bezier_curve(B(1:3), n), &
        & bezier_curve(B(4:6), n), &
        & bezier_curve(B(7:9), n), &
        & bezier_curve(B(10:12), n), B(1)]
    end associate
  end associate

  if (present(output)) then
    output_ = output
  else
    output_ = .true.
  end if

  if (output_) then
    fmt = "(2(f12.6, 1x))"
    open (newunit=unit, file='./data/logo.dat')
    write (unit, "('#', a11, 1x, a12)") "x", "y"
    write (unit, fmt) self%letter_F(2:size(self%letter_F))
    write (unit, fmt) self%x(2, 3), self%y(2, 1)
    write (unit, fmt) self%x(2, 3), self%y(1, 1)
    write (unit, fmt) self%boundary(size(self%boundary):1:-1)
    write (unit, fmt) self%x(2, 3), self%y(1, 1)
    write (unit, fmt) self%x(2, 3), self%y(2, 1)
    write (unit, fmt) self%letter_F(size(self%letter_F))
    close (unit)

    open (newunit=unit, file='./data/letter_F.dat')
    write (unit, "('#', a11, 1x, a12)") "x", "y"
    write (unit, fmt) self%letter_F
    close (unit)

    open (newunit=unit, file='./data/letter_piles.dat')
    write (unit, "('#', a11, 1x, a12)") "x", "y"
    do i = 1, size(self%letter_piles)
      write (unit, fmt) self%letter_piles
    end do
    write (unit, fmt) self%reference_point
    close (unit)

    open (newunit=unit, file='./data/boundary_piles.dat')
    write (unit, "('#', a11, 1x, a12)") "x", "y"
    write (unit, fmt) self%boundary_piles
    close (unit)

    open (newunit=unit, file='./data/boundary.dat')
    write (unit, "('#', a11, 1x, a12)") "x", "y"
    write (unit, fmt) self%boundary
    close (unit)
  end if
end subroutine compute

subroutine draw(self, size, ext)
  class(logo_type), intent(in) :: self
  integer, intent(in) :: size
  character(*), intent(in) :: ext
  integer :: unit
  type(point_type) :: center
  character(:), allocatable :: &
    & messages(:), size_, set_output, &
    & xrange_(:), yrange_(:), set_terminal

  size_ = str(size)
  center = point_type( &
    & (self%x(1, 1) + self%x(1, 2))/2, &
    & (self%y(1, 1) + self%y(1, 2))/2)
  xrange_ = [character(len=10) :: &
    & str(center%x - self%side_length/2), &
    & str(center%x + self%side_length/2)]
  yrange_ = [character(len=10) :: &
    & str(center%y - self%side_length/2), &
    & str(center%y + self%side_length/2)]

  set_output = "set output './data/fortran_logo"
  select case (ext)
  case ('png')
    set_terminal = "set terminal png size "// &
      & size_//","//size_//" transparent truecolor"
  case ('svg')
    set_terminal = "set terminal svg size "// &
      & size_//","//size_//" dynamic"
  case default
    error stop "Invalid extension."
  end select

  messages = [character(len=80) :: &
    & set_terminal, &
    & "unset key", &
    & "fortran_purple1 = '#a07fda'", &
    & "fortran_purple2 = '#6d5192'", &
    & "fortran_purple3 = '#42355b'", &
    & "set xrange ["//xrange_(1)//":"//xrange_(2)//"]", &
    & "set yrange ["//yrange_(1)//":"//yrange_(2)//"]", &
    & "unset border", &
    & "unset tics"//new_line("(a)"), &
    & "set style line 1 lw 2 lc rgb fortran_purple1", &
    & "set style line 2 lw 2 lc rgb fortran_purple2", &
    & "set style line 3 lw 2 lc rgb fortran_purple3"//new_line("(a)"), &
    & set_output//"."//ext//"'", &
    & "plot './data/logo.dat' with filledcurves closed ls 2, \", &
    & "     './data/letter_F.dat' with lines ls 3, \", &
    & "     './data/boundary.dat' with lines ls 3", &
    & set_output//"_inverted."//ext//"'", &
    & "plot './data/letter_F.dat' with filledcurves ls 2, \", &
    & "     './data/letter_F.dat' with lines ls 3"]

  open (newunit=unit, file='./data/.plot.plt')
  call write_strs(unit, messages)
  close (unit)

  call execute_command_line("gnuplot ./data/.plot.plt")
end subroutine draw

subroutine blueprint(self, dark_mode)
  class(logo_type), intent(in) :: self
  logical, intent(in), optional :: dark_mode
  real :: offset(2)
  integer :: unit
  character(:), allocatable :: fmt(:)
  character(:), allocatable :: msg
  real :: bdy, fac
  integer :: i, j
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
    c => self%reference_point, &
    n => self%num_curves, &
    x => self%x, &
    y => self%y)

    open (newunit=unit, file='./data/.plot.plt')

    call write_("set terminal svg size 600, 600 font ', 12'")
    call write_("set key noautotitle")
    call write_("light_color = '#6d5192'")
    call write_("dark_color = '#d79921'")
    call write_("color = "//mode//"_color")
    call write_("set xrange [-110:110]")
    call write_("set yrange [-110:110]"//new_line("(a)"))

    call write_("set style line 1 lw 2 lc rgb color")
    call write_("set style line 2 lw 1 lc rgb color lt 0")
    call write_("set style line 3 lw 1.5 lc rgb color lt 0")
    call write_("set style line 4 lc rgb color pt 7 ps .75")
    call write_("set tics format ''")
    call write_("unset border")
    call write_("set tics scale 0"//new_line("(a)"))
    call write_("set output './data/blueprint_"//mode//".svg'")
    ! call write_("set pixmap 1 './data/F1954.png' "// &
    !   & "at first -69, -75 size 138, 150 back")

    allocate (character(len=500) :: fmt(4))
    fmt(2) = "('set arrow', 1x, "// &
       & "2(a, 1x, f6.2, ',', f6.2, 1x), 'nohead ls 2')"
    fmt(3) = "('set arrow', 1x, "// &
       & "2(a, 1x, f6.2, ',', f6.2, 1x), 'nohead ls 3')"
    fmt(4) = "(a, f6.2, ',', f6.2, "// &
       & "' tc rgb color offset ', f6.2, ',', f6.2, 1x, a)"

    allocate (character(len=9) :: msg)
    do j = 1, 3
      do i = 1, 3

        bdy = self%side_length/2
        write (unit, trim(fmt(2))) 'from', x(i, j), -bdy, 'to', x(i, j), bdy
        write (unit, trim(fmt(2))) 'from', -bdy, y(i, j), 'to', bdy, y(i, j)

        offset = [-1.4, 0.0]
        msg = 'right'
        write (unit, trim(fmt(4))) &
          & "set label 'Y("//str(i)//","//str(j)//")' at ", &
          & self%boundary_piles(2)%x, y(i, j), offset, msg

        msg = 'right rotate by 90'
        if (i == 3 .and. j == 3) then
          offset = [1.4, -0.7]
          write (unit, trim(fmt(4))) &
            & "set label '=X("//str(i)//","//str(j)//")' at ", &
            & x(i, j), self%boundary_piles(2)%y, offset, msg
        else
          offset = [0.0, -0.7]
          write (unit, trim(fmt(4))) &
            & "set label 'X("//str(i)//","//str(j)//")' at ", &
            & x(i, j), self%boundary_piles(2)%y, offset, msg
        end if
      end do
    end do

    bdy = self%side_length/2
    write (unit, trim(fmt(3))) 'from', -bdy, -bdy, 'to', -bdy, +bdy
    write (unit, trim(fmt(3))) 'from', +bdy, -bdy, 'to', +bdy, +bdy
    write (unit, trim(fmt(3))) 'from', -bdy, -bdy, 'to', +bdy, -bdy
    write (unit, trim(fmt(3))) 'from', -bdy, +bdy, 'to', +bdy, +bdy

    write (unit, trim(fmt(3))) 'from', c%x, -bdy, 'to', c%x, bdy
    write (unit, trim(fmt(3))) 'from', -bdy, c%y, 'to', bdy, c%y

    fac = 0.7
    write (unit, trim(fmt(4))) &
      "set label '(RX, RY)' at", c, fac, -fac, 'left'

    do i = 1, size(self%letter_piles)
      msg = 'left'
      select case (i)
      case (1:2)
        offset = [+fac, -fac]
      case (4, 13:16)
        offset = [+fac, -fac]
      case default
        offset = [+fac, +fac]
      end select

      write (unit, trim(fmt(4))) &
        & "set label 'B"//str(i)//"' at ", &
        & self%letter_piles(i), offset, msg
    end do

    do i = 1, size(self%boundary_piles)
      msg = 'left'
      select case (i)
      case (2)
        offset = [+fac, -fac]
      case (4)
        offset = [+fac, -fac]
      case (10:11)
        offset = [+fac, -fac]
      case (12)
        offset = [-fac, +fac]
        msg = 'right'
      case default
        offset = [+fac, +fac]
      end select

      write (unit, trim(fmt(4))) &
        & "set label 'B"//str(i)//"' at ", &
        & self%boundary_piles(i), offset, msg
    end do

    call write_("plot './data/letter_F.dat' w lines ls 1, \")
    call write_("     './data/boundary.dat' w lines ls 1, \")
    call write_("     './data/letter_piles.dat' u 1:2 w point ls 4, \")
    call write_("     './data/boundary_piles.dat' u 1:2 w point ls 4")
  end associate

  close (unit)
  call execute_command_line("gnuplot ./data/.plot.plt")

contains
  subroutine write_(message_)
    character(*), intent(in) :: message_

    write (unit, "(a)") message_
  end subroutine write_
end subroutine blueprint

end module module_logo
