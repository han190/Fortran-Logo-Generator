set terminal svg size 900, 900 font 'Cascadia Mono, 15'
set key noautotitle
light_color = '#6d5192'
dark_color = '#d79921'
color = light_color
set xrange [-4.5:4.5]
set yrange [-4.5:4.5]
set style line 1 linewidth 2 linecolor rgb color
set style line 2 linewidth 1 linecolor rgb color linetype 0
set style line 3 linewidth 1.5 linecolor rgb color linetype 0
set style line 4 linecolor rgb color pointtype 7 pointsize .75
set tics format ''
unset border
set tics scale 0
set output './data/blueprint_light.svg'
set arrow from  -3.00, -3.70 to  -3.00,  3.70 nohead linestyle 2
set arrow from  -3.70, -3.00 to   3.70, -3.00 nohead linestyle 2
set label"X(1,1)"  at -3.00, -3.85 center front textcolor rgb color
set label"Y(1,1)"  at -4.05, -3.00 center front textcolor rgb color
set arrow from  -2.30, -3.70 to  -2.30,  3.70 nohead linestyle 2
set arrow from  -3.70, -2.30 to   3.70, -2.30 nohead linestyle 2
set label"X(2,1)"  at -2.30,  3.85 center front textcolor rgb color
set label"Y(2,1)"  at  4.05, -2.30 center front textcolor rgb color
set arrow from  -1.90, -3.70 to  -1.90,  3.70 nohead linestyle 2
set arrow from  -3.70, -1.75 to   3.70, -1.75 nohead linestyle 2
set label"X(3,1)"  at -1.90, -3.85 center front textcolor rgb color
set label"Y(3,1)"  at -4.05, -1.75 center front textcolor rgb color
set arrow from   3.00, -3.70 to   3.00,  3.70 nohead linestyle 2
set arrow from  -3.70,  3.00 to   3.70,  3.00 nohead linestyle 2
set label"X(1,2)"  at  3.00,  3.85 center front textcolor rgb color
set label"Y(1,2)"  at  4.05,  3.00 center front textcolor rgb color
set arrow from   2.30, -3.70 to   2.30,  3.70 nohead linestyle 2
set arrow from  -3.70,  0.60 to   3.70,  0.60 nohead linestyle 2
set label"X(2,2)"  at  2.30, -3.85 center front textcolor rgb color
set label"Y(2,2)"  at -4.05,  0.60 center front textcolor rgb color
set arrow from   1.25, -3.70 to   1.25,  3.70 nohead linestyle 2
set arrow from  -3.70,  2.30 to   3.70,  2.30 nohead linestyle 2
set label"X(3,2)"  at  1.25,  3.85 center front textcolor rgb color
set label"Y(3,2)"  at  4.05,  2.30 center front textcolor rgb color
set arrow from   0.00, -3.70 to   0.00,  3.70 nohead linestyle 2
set arrow from  -3.70,  0.45 to   3.70,  0.45 nohead linestyle 2
set label"X(1,3)"  at  0.00, -3.85 center front textcolor rgb color
set label"Y(1,3)"  at -4.05,  0.45 center front textcolor rgb color
set arrow from   0.60, -3.70 to   0.60,  3.70 nohead linestyle 2
set arrow from  -3.70,  1.50 to   3.70,  1.50 nohead linestyle 2
set label"X(2,3)"  at  0.60,  3.85 center front textcolor rgb color
set label"Y(2,3)"  at  4.05,  1.50 center front textcolor rgb color
set arrow from   1.25, -3.70 to   1.25,  3.70 nohead linestyle 2
set arrow from  -3.70,  1.75 to   3.70,  1.75 nohead linestyle 2
set label"X(3,3)"  at  1.25, -3.85 center front textcolor rgb color
set label"Y(3,3)"  at -4.05,  1.75 center front textcolor rgb color
set arrow from  -3.70, -3.70 to  -3.70,  3.70 nohead linestyle 3
set arrow from   3.70, -3.70 to   3.70,  3.70 nohead linestyle 3
set arrow from  -3.70, -3.70 to   3.70, -3.70 nohead linestyle 3
set arrow from  -3.70,  3.70 to   3.70,  3.70 nohead linestyle 3
set arrow from  -1.28, -3.70 to  -1.28,  3.70 nohead linestyle 3
set arrow from  -3.70,  0.12 to   3.70,  0.12 nohead linestyle 3
set label'XC' at -1.28, -3.85 center front textcolor rgb color
set label'YC' at -3.85,  0.12 center front textcolor rgb color
set label 'C' at  -1.28,  0.12 textcolor rgb color offset   1.20, -0.60 center
set label 'P1' at    -1.90, -3.00 textcolor rgb color offset   1.40,  0.60 center
set label 'P2' at    -3.00, -3.00 textcolor rgb color offset   1.40,  0.60 center
set label 'P3' at    -3.00, -2.30 textcolor rgb color offset   1.40,  0.60 center
set label 'P4' at    -2.30, -2.30 textcolor rgb color offset   1.40, -0.60 center
set label 'P5' at    -1.90, -2.30 textcolor rgb color offset   1.40, -0.60 center
set label 'P6' at    -1.90, -1.75 textcolor rgb color offset   1.40,  0.60 center
set label 'P7' at     3.00,  3.00 textcolor rgb color offset  -1.40, -0.60 center
set label 'P8' at     3.00,  0.60 textcolor rgb color offset  -1.40,  0.60 center
set label 'P9' at     2.30,  0.60 textcolor rgb color offset   1.40,  0.60 center
set label 'P10' at    2.30,  2.30 textcolor rgb color offset   1.40,  0.60 center
set label 'P11' at    1.25,  2.30 textcolor rgb color offset   1.40,  0.60 center
set label 'P12' at   -0.66,  2.30 textcolor rgb color offset   1.40,  0.60 center
set label 'P13' at   -0.66,  0.45 textcolor rgb color offset   1.40, -0.60 center
set label 'P14' at    0.00,  0.45 textcolor rgb color offset   1.40, -0.60 center
set label 'P15' at    0.60,  0.45 textcolor rgb color offset   1.40, -0.60 center
set label 'P16' at    0.60,  1.50 textcolor rgb color offset   1.40, -0.60 center
set label 'P17' at    0.60,  1.75 textcolor rgb color offset   1.40,  0.60 center
set label 'P18' at    1.25,  1.75 textcolor rgb color offset   1.40,  0.60 center
set label 'B1' at    -3.00, -3.70 textcolor rgb color offset   1.40,  0.60 center
set label 'B2' at    -3.70, -3.70 textcolor rgb color offset  -1.40, -0.60 center
set label 'B3' at    -3.70, -3.00 textcolor rgb color offset   1.40,  0.60 center
set label 'B4' at    -3.70,  3.00 textcolor rgb color offset  -1.40,  0.60 center
set label 'B5' at    -3.70,  3.70 textcolor rgb color offset  -1.40,  0.60 center
set label 'B6' at    -3.00,  3.70 textcolor rgb color offset  -1.40,  0.60 center
set label 'B7' at     3.00,  3.70 textcolor rgb color offset  -1.40, -0.60 center
set label 'B8' at     3.70,  3.70 textcolor rgb color offset   1.40,  0.60 center
set label 'B9' at     3.70,  3.00 textcolor rgb color offset  -1.40, -0.60 center
set label 'B10' at    3.70, -3.00 textcolor rgb color offset   1.80, -0.60 center
set label 'B11' at    3.70, -3.70 textcolor rgb color offset   1.80, -0.60 center
set label 'B12' at    3.00, -3.70 textcolor rgb color offset   1.80, -0.60 center
plot './data/letter_F.dat' with lines linestyle 1, \
     './data/boundary.dat' with lines linestyle 1, \
     './data/letter_pile.dat' using 1:2 with point linestyle 4, \
     './data/boundary_pile.dat' using 1:2 with point linestyle 4
