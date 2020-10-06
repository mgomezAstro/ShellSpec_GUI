set xlabel "phase"
set ylabel "lambda[mic]"
set zlabel "flux"
set logscale z
#set ticslevel 0.
set hidden3d
#set view 52,2
splot \
'lightcurve' using ($1-0.75):($4*1.e-4):($5) with lines,\
'lightcurveold39f' using ($1-0.75):($4*1.e-4):($5)
pause-1 "Press Enter"
quit
