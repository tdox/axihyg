set title "w"
set xlabel "s"
set ylabel "w"
plot 'w1.dat' using 2:3 t "theory 1" with lines, \
     'w2.dat' using 2:3 t "theory 2", \
     'w3.dat' using 2:3 t "theory 3", \
     'w4.dat' using 2:3 t "theory 4", \
     'w5.dat' using 2:3 t "theory 5", \
     'w6.dat' using 2:3 t "theory 6", \
     'w7.dat' using 2:3 t "theory 7", \
     'w8.dat' using 2:3 t "theory 8"
