set terminal png size 1000, 1000
set size 1, 1
set key noautotitle
orange = '#fabd2f'
purple = '#6d5192'
color = orange
set output './data/icon.png'
set border linewidth 4 linecolor rgb color
set tics format ''
set xtics (-3.00, -2.30, -1.90, 0.00, 0.60, 1.25, 2.30, 3.00)
set ytics (-3.00, -2.30, -1.75, 0.45, 0.60, 1.50, 1.75, 2.30, 3.00)
set grid linecolor rgb color
set pixmap 1 "./data/f1954.png" at first -3, -3 size first 6, 6 behind

set xrange [-3.5:3.5]
set yrange [-3.5:3.5]
set style line 1 linecolor rgb color \
    linetype 1 linewidth 3 pointtype 6 pointsize 2
set style line 2 linecolor rgb purple \
    linewidth 3

plot './data/icon.dat' with linespoints linestyle 1
# plot './data/icon.dat' with lines linestyle 2