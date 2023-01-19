set terminal png size 1000, 1000 transparent truecolor
set size 1, 1
set key noautotitle
light_color = '#6d5192'
dark_color = '#d79921'
color = light_color

set tics format ''
set xtics (-3.00, -2.30, -1.90, 0.00, 0.60, 1.25, 2.30, 3.00)
set ytics (-3.00, -2.30, -1.75, 0.45, 0.60, 1.50, 1.75, 2.30, 3.00)
set grid linecolor rgb color
# set pixmap 1 "./data/f1954.png" at first -3, -3 size first 6, 6 behind

set xrange [-3.5:3.5]
set yrange [-3.5:3.5]
set style line 2 linewidth 4

set output './data/icon_dark.png'
set border linewidth 4 linecolor rgb dark_color
plot './data/icon.dat' with lines linestyle 2 linecolor rgb dark_color

set output './data/icon_light.png'
set border linewidth 4 linecolor rgb light_color
plot './data/icon.dat' with lines linestyle 2 linecolor rgb light_color