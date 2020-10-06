#set xrange[5578:5588]
#set yrange[5e-11:5e-10]
set xlabel "velocity"
set ylabel "Intensity"
set zero 1e-30
plot 'shellspectrum' using 2:3 with lines,\
'shellspectrumold39a' using 2:($3+0.) with points
pause-1 "Press Enter"

plot 'shellspectrum' using 2:5 with lines,\
'shellspectrumold39a' using 2:($5+0.) with lines
pause-1 "Press Enter"

set xlabel "lambda"
plot 'shellspectrum' using 1:5 with lines,\
'shellspectrumold39a' using 1:($5+0.) with lines

pause-1 "Press Enter"

#set xlabel "phase"
#set ylabel "velocity"
#set zlabel "magnitude"
#set hidden3d
#set grid
#set view 40,0
#splot 'lightcurve' using ($1+0.25):2:3 with lines
#pause-1 "Press Enter"

#set view 40,80
#splot 'lightcurve' using 1:2:4 with lines
#pause-1 "Press Enter"

set xlabel "x"
set ylabel "y"
set zlabel "Intensity"
#set logscale z
set hidden3d
set grid
set view 40,7
splot 'fort.21' using 1:2:3 with lines
pause-1 "Press Enter"
#splot 'fort.22' using 1:2:3 with lines
#pause-1 "Press Enter"
#splot 'fort.23' using 1:2:3 with lines
#pause-1 "Press Enter"
#splot 'fort.24' using 1:2:3 with lines
#pause-1 "Press Enter"
#splot 'fort.25' using 1:2:3 with lines
#pause-1 "Press Enter"
#splot 'fort.26' using 1:2:3 with lines
#pause-1 "Press Enter"
#splot 'fort.27' using 1:2:3 with lines
#pause-1 "Press Enter"
#splot 'fort.28' using 1:2:3 with lines
#pause-1 "Press Enter"
#splot 'fort.29' using 1:2:3 with lines
#pause-1 "Press Enter"
#splot 'fort.30' using 1:2:3 with lines
#pause-1 "Press Enter"
#splot 'fort.31' using 1:2:3 with lines
#pause-1 "Press Enter"
#splot 'fort.32' using 1:2:3 with lines
#pause-1 "Press Enter"
quit
