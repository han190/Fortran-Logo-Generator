<p align="center">
  <img src="./data/logo_green_letter.svg" style="width:20%">
  <img src="./data/logo_red_rounded.svg" style="width:20%">
  <img src="./data/logo_blue_chobby.svg" style="width:20%">
  <img src="./data/logo_yellow_regular.svg" style="width:20%">
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
fpm run # Use default parameters.nml
fpm run -- /your/own/namelist/parameters.nml
```
The program is tested under WSL only but it should also work on all linux/mac. Please report an issue if you spotted one, and pull requests are welcome too!

## Parameterization

<p align="center">
  <img width="80%" src="./data/logo_blueprint.svg">
</p>

As shown in the diagram above, parameters `hori_anchors` and `vert_anchors` are converted into 18 coordinates (1-18) and a reference point (R) for the letter "F" and 12 coordinates for the rounded-corner boundary (A-L). They are called "piles" in the source code (empty circles). The "F" points can be further categorized into three groups: (1) 1 - 6, (2) 7 - 12, and (3) 13 - 18. The whole logo could be quickly drawn by mirroring group 1 with respect to RX and RY, and mirroring group 3 with respect to RX. `bracket_offset` are used to fine tune the brackets. Notice that all curves are quadratic bezier curves. The advantage of the parameterization is that, by changing parameters, users could create their own varient of the "F" logo.

## Parameters

| Parameter Name | |
|:-----|:-------|
| `integer :: num_points(2)` | Number of points used to draw (1) curves and (2) smoothed corners |
| `real :: rad_corners` | Radius of smoothed corners (suggested: < 0.05) |
| `real :: width` | Width (px) |
| `real :: height` | Height (px) |
| `real :: margin(4)` | <left, right, bottom, top> margin (px) from letter "F" to the boundary|
| `real :: corner(2)` | Radius (px) of the rounded boundary |
| `real :: reference_point(2)` | The reference point (from 0 to 1) |
| `real :: bracket_offset(2)` | The bracket offset (from 0 to 1) |
| `real :: hori_anchors(3, 3)` | Horizontal anchors (from 0 to 1) |
| `real :: vert_anchors(3, 3)` | Vertical anchors (from 0 to 1) |
| `character(len=80) :: file` | Ouptut filename |
| `logical :: letter_only` | Draw "F" only|
| `real :: canvas_ratio` | Width of canvas/width of boundary (so the actual size of the svg file is 382.5px x 382.5px) |
| `character(len=80) :: color` | Color of the logo |
| `character(len=80) :: font_family` | Font of lables (debug) |
| `character(len=80) :: font_size` | Font size of labels (debug) |
| `real :: pile_radius` | Radius of piles (debug) |
| `real :: line_width` | Line width of logo |
| `real :: dash_width` | Line width of dashed lines (debug) |
| `character(len=80) :: dash_array` | Dashed type (debug) |
| `real :: fill_pattern(4)` | Fill pattern |
| `logical :: compare` | Compare with another image (debug) |
| `character(len=80) :: compare_image` | The image to be compared (debug) |
