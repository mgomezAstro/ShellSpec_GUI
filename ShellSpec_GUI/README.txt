####################
#### Description:###
####################

Program SHELLSPEC is designed to calculate lightcurves, spectra and images 
of interacting binaries and extrasolar planets immersed in a moving circumstellar
matter (CM). It solves a simple radiative transfer along the line of sight in
3D moving media. Roche model and synthetic spectra from the stellar atmosphere
models can be used as a boundary condition for the radiative transfer. 

Please visit: http://www.ta3.sk/~budaj/shellspec/ for more information about the program.

###################
## Installation:###
###################

There are two kind of files. The RELEASE version, in which you can only execute
ShellSpec.sh in the running directory defined by you (you can add ShellSpec.sh
directly to your /usr/loca/bin or /usr/bin/).

If the first step doesn't work, then you must compile using qmake (with qt>=5.12)
and then make inside the binary file.

cd SHELLSPEC/dir/binary/
qmake -config release
make
./ShellSpec (you can put this directly in /usr/bin or /usr/local/bin).

Note however, that you must have the latest verison of GCC compilers (such as gfortran).

#####################
#Installation of qt:#
#####################

Ubuntu
sudo apt install qt5-default

Fedora
sudo dnf install qt5-defualt

Windows
Not needed.

###################
### Warranty: #####
###################

ShellSpec_GUI and shellspec is provided as it is. No Warranty at all, you cannot
complain if it runs models which are not fitting your data!

###################
### License: ######
###################

The GNUv3 license : https://www.gnu.org/licenses/gpl-3.0.html .

All users have the rights to obtain, modify and redistribute the full source code
of this application. Users are granted rights founded on the four freedoms of the
GNU General Public License.



PROBLEMS:
-- problem:Internal Error: get_unit(): Bad internal unit KIND
Solution: Install the latest version of gfortran as this errors comes from old libraries.
