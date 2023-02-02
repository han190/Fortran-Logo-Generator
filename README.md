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
  <img src="./data/logo_blueprint.svg">
</p>

As shown in the diagram above, parameters `hori_anchors` and `vert_anchors` are converted into 18 coordinates (1-18) and a reference point (R) for the letter "F" and 12 coordinates for the rounded-corner boundary (A-L). They are called "piles" in the source code (empty circles). The "F" points can be further categorized into three groups: (1) 1 - 6, (2) 7 - 12, and (3) 13 - 18. The whole logo could be quickly drawn by mirroring group 1 with respect to RX and RY, and mirroring group 3 with respect to RX. `bracket_offset` are used to fine tune the brackets. Notice that all curves are quadratic bezier curves. The advantage of the parameterization is that, by changing parameters, users could create their own varient of the "F" logo.

| Parameter Name | Example | Comment |
|:-----|:-------|:---------|
| num_points | 20, 5 | Number of points used to draw (1) curves and (2) smoothed corners |
| rad_corners | 0.01 | Radius of smoothed corners |
| width | 255 | With in pixel |
| height | 255 | Height in pixel |
| margin | 35, 35, 28, 28 | <left, right, bottom, top> margin from letter "F" to the boundary|
| corner | 55, 55 | Radius of the rounded boundary |
| reference_point | 0.278, 0.525 | The reference point |
| bracket_offset | 0.016, -0.028 | The bracket offset |
| hori_anchors | - | Horizontal anchors |
| vert_anchors | - | Vertical anchors |
| file | 'logo.svg' | Ouptut filename |
| letter_only | .false. | Draw "F" only|
| canvas_ratio | 1.5 | Width of canvas/width of boundary (so the actual size of the svg file is 382.5px x 382.5px) |
| color | '#6d5192' ! Purple | Color of the logo |
| font_family | 'Cascadia Code' | Font of lables (debug) |
| font_size | '0' | Font size of labels (debug) |
| pile_radius | 0 | Radius of piles (debug) |
| line_width | 4.0 | Line width of logo |
| dash_width | 0.0 | Line width of dashed lines (debug) |
| dash_array | '2 2' | Dashed type (debug) |
| fill_pattern | 9., 1., 4., 1.  | Fill pattern |
| compare | .false. | Compare with another image (debug) |
| compare_image | './data/fortran_logo_512x512.png' | The image to be compared |
