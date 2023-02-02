<p align="center">
  <img src="./data/logo_green_letter.svg" style="width:20%">
  <img src="./data/logo_red_rounded.svg" style="width:20%">
  <img src="./data/logo_blue_chobby.svg" style="width:20%">
  <img src="./data/logo_purple_regular.svg" style="width:20%">
</p>

<h1 align="center">
  Fortran Logo Generator
</h1>

This project has two driving factors:

* To provide a portable Fortran Logo Generator written in modern Fortran,
* To polish the current Fortran logo.

## Quick start

To compile and execute the program, navigate to the project directory and type
```bash
fpm build
fpm run -- --blueprint
fpm run -- --logo
```
The program is tested under WSL only but it should also work on all linux/mac. Please report an issue if you spotted one, and pull requests are welcome too!

## Parameterization

An example of `parameters.nml`

```
&parameters
  num_curves        = 20
  num_rounded       = 10
  side_length       = 500, 535
  rounded_radius    = 0.009
  corner            = 0.107, 0.1, 0.107, 0.1
  hook_offset       = 0.04
  reference_point   = -0.43, +0.04
  x                 = -1.00, -0.8, -0.633333, 
                      +1.00, +0.766666, +0.416666, 
                      +0.00, +0.2, +0.416666
  y                 = -1.00, -0.766666, -0.6, 
                      +1.00, +0.2, +0.766666, 
                      +0.15, +0.5, +0.583333
/

&blueprint
  ! color             = '#6d5192' ! Purple
  color             = '#ca9b3d' ! Yellow
  font_family       = 'Cascadia Mono'
  font_size         = '80%'
  circle_radius     = 5.0
  line_width        = 2.0
  dash_width        = 1.2
/
```
