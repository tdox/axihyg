set title "w"
set xlabel "s"
set ylabel "w"
plot 'w1.dat' using 2:3 t "theory 1" with lines, \
     'w8.dat' using 2:3 t "theory 8", \
     'w10.dat' using 2:3 t "theory 10"

