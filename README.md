# ShellSpec_GUI
ShellSpec_GUI is a Qt UI created to manage the code shellspec; designed to calculate lightcurves, spectra and images of interacting binaries and extrasolar planets immersed in a moving circumstellar matter (CM). The main program "shellspec" is maintined by Dr. Jan Budaj (ref; https://www.ta3.sk/~budaj/shellspec/). Please read the notes and instructions of the variables being used in ShellSpec_GUI (ref to the manual here). Also, take notes about preliminar files you may require to run the program using atmosphere models.

### Pre-requisites (Linux)

So far I have tested the installation procedure in CentOS 7 and in Ubuntu 20.04 LTS.

* gfortran
* qmake
* make
* qt5 (e.g., in Ubuntu "apt get install qt5-default")

### Installation

git clone https://github.com/mgomezAstro/ShellSpec_GUI.git

Then run qmake in the Top Level directory (e.g., ShellSpec). This will create the Makefile with the paths to qt5 headers found in your machine. Finally, run "make" to create the executable (e.g., ShellSpec.sh).

### Warranty

ShellSpec_GUI is provided as it is. No Warranty at all, you cannot complain if it runs models which are not fitting your data.

### Notes

This is a beta release. It works well although there are some "to do" things in the graphics part.

### Acknowledgements

This program was supported by the ERASMUS+ asu mobility project in 2019 as part of my PhD prgram; in colaboration between the Instituto de Astrofísica de Canarias and the Astronomical Institute of the Slovak Academy of Sciences.

### Troubleshooting

* If for some reason the "make" procedure fails, run "make clean" and run "make" again.
* If you run the program in terminal, just ignore most of the comments.
