set xlabel "phase"
set ylabel "lambda[A]"
set zlabel "flux"
set ticslevel 0.
set hidden3d
set view 52,2
splot \
'lightcurve' using ($1-0.25):4:($5) with lines,\
'lightcurveold39e' using ($1-0.25):4:($5) 
pause-1 "Press Enter"
quit
