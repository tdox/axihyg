set title "b"
set xlabel "s"
set ylabel "b"
plot 'b8.dat' using 2:3 t "theory 8" with lines, \
     'b7.dat' using 2:3 t "theory 7", \
     'b1.dat' using 2:3 t "theory 1", \
     'b10.dat' using 2:3 t "theory 10"

