set title "w"
set xlabel "s"
set ylabel "w"
plot 'w1.dat' using 2:3 t "theory 1" with lines, \
     'w9.dat' using 2:3 t "theory 9", \
     'w10.dat' using 2:3 t "theory 10"
