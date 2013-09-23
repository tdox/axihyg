set title "w"
set xlabel "s"
set ylabel "w"
plot 'w8.dat' using 2:3 t "theory 8" with lines, \
     'w7.dat' using 2:3 t "theory 7", \
     'w1.dat' using 2:3 t "theory 1", \
     'w10.dat' using 2:3 t "theory 10"

