c
c        program shellrun
c        call shellspec()
c        stop
c        end program shellrun
c!
        subroutine shellspec()
c	Program SHELLSPEC
c
c	is designed to calculate lightcurves, spectra and images of 
c	interacting binaries and extrasolar planets immersed in a moving
c       circumstellar matter (CM). 
c       It solves simple radiative transfer along the line of sight 
c       in 3D moving media. Roche model can be used as a boundary 
c       condition for the radiative tranfer. The scattered light from 
c       the two stars can be taken into account assuming that CM is 
c       optically thin.
c	The assumptions include LTE and optional known state quantities
c       and velocity field in 3D. These can be taken from 
c       the 3D hydrodynamical simulations. Alternatively,
c	optional (non)transparent objects such as:
c	a central star, companion star, envelope, spot, stream, ring,
c       disk, nebula, flow, jet, ufo, shell or an empty space 
c       may be defined in 3D and their composite synthetic spectrum 
c       calculated. The stars may have either the Roche or spherical 
c       geometry, optional velocity or rotation. They are subject
c       to the gravity darkening, limb darkening, and irradiation effect
c	including the heating, reflection and day-night heat 
c       redistribution. They may be ascribed a precalculated spectrum.
c	Synthetic light curves or trailing spectrograms can be produced
c	by changing your view points on the 3D object.
c	Opacities: lines, HI (bound-free, free-free), 
c               H- (bound-free, free-free),
c		Thomson scattering, Rayleigh scattering, 
c               Mie absorption and scattering on dust. 
c	Emissivities: thermal from the gas, 
c               Thomson and Rayleigh scattering from the stars, 
c               thermal from the dust and Mie scattering from stars 
c               on dust
c
c       INPUT:
c		shellspec.in -(9) main input (geometry, objects...)
c		line.dat - (8) atomic data for the sp. lines
c                       (optional if iline=1)
c               shellspec.mod - (10) input 3D model of the shell
c			(optional if imodel=2)
c		abundances  - (7) abundances 
c			(optional if ichemc=1 or ielnd=1)
c		phases - (15) orbital phases to calculate
c			(optional if nphase=0)
c		starspec1 - (12) spectrum of the primary star
c			(optional if lunt1>0)
c		starspec2 - (13) spectrum of the secondary
c			(optional if lunt2>0)
c		starspec3 - (14) spectrum of the third body 
c			(optional if lunt3>0)
c		albedo1 - (12) monochromatic albedo of the primary star
c			(optional if ialbst=1 and irrst=1)
c		albedo2 - (13) monochromatic albedo of the secondary
c			(optional if ialbcp=1 and irrcp=1)
c               dust_opac - (12) dust opacities (optional if imie>0)
c		mie_phase - (13) dust phase func. (optional if imiepf=1)
c               gas_opac - (16) molec x-sections (optional if iopac=1)
c               chem_eq_tab - (16) molec popul tab (optional if iopac=1)
c               wind_prof -(17) vertic. density wind prof (if idennb=1)
c
c       OUTPUT:
c		shellspec.out - (2) more detailed output
c		fort.xx - (21,21+iang) 2D images at some frequency
c		shellspectrum - (4) spectrum of the shell
c		lightcurve - (11) light curve or trailed spectrogram
c               sh.txt -(18) for Slavek and AWUMA
c               shx.txt -(19) for Slavek and AWUMA
c
c	COMMENT: CGS units are used if not specified otherwise
c		 Cartesian 3D coordinates are used
c		 implicit double precision is used and 
c		 use e.g. 1.d40 and not 1.e40
c		 ?? - BE CAUTIOUS
c
c	Non original routines: 
c	 	pfdwor  -from UCLSYN	
c		voigt, state0, gaunt, gfree   -from SYNSPEC
c		hunt,locate    -from Numerical recipes
c
c	Author/contact: Jan Budaj, Mercedes Richards
c			http://www.ta3.sk/~budaj
c			budaj@ta3.sk
c
c	Reference:
c		Budaj J., Richards M.T., 2004, 
c		Contrib. Astron. Obs. Skalnate Pleso, 34, 167
c
c	Things to be improved - added:
c	-introduce a new subroutine with the checks not goto 590
c       -calculate H opacity at each lambda in case of long SED??
c        (this is slow so uncomment lines if necessary)
c	-introduce sets with continuum opacity at the edges
c	-take the rotation of star1 into account in the scattered 
c	light (interpolate in precalculated rotationally broadened  
c	intensity for several inclinations),
c	-allow finner grid along the line of sight, step op.dep.<0.05
c       -irradiation (heating) of the companion in case icomp=1
c	(reflection is already included)
c	-gravity reddening of the primary star
c	-check H- opacity
c	-fix bug at the Roche surface at L1 point for f=1.
c       -elnd has slow or no convergence for T approx< 90 K
c	-make the code both F77 and F90: place & on 73-rd character
c	 and on 6-th character, use 'space' instead of 'tab'
c
c		ndimf1,2,3 -main space xyz dimension parameters
c                     for the body frozen grid
c               ndim1,2,3 - main space xyz dimension parameters
c                      for the line of sight grid
c               mion - max. number of ions dimension (fixed=9)
c		matom - highest possible atom number 
c		mfreq - max. number of frequnces
c		mline - max. number of sp. lines considered
c		mphase - max. number of grid rotations
c		mstarx - max. number of frequences for albedo or 
c			for star1, star2, star3 spectra
c		mspecx - max. number of precalculated spectra/files
c			for star1 for mspecx different temperatures
c               npfang -number of angles for the Mie phase function
c		mtemp,mdens,melm -number of temperatures, densities,
c			and molecules in the chemistry table
c
	implicit double precision (a-h,o-z)
	include 'param.inc'
	integer, parameter:: mtemp=57,mdens=73,melm=11
c	parameter (mtemp=57,mdens=73,melm=11)
        character*4 dyp
        character*11 forts
	dimension ar(ndim1),at(ndim2),az(ndim3),azn(ndim3)
        dimension far(ndimf1),fat(ndimf2),faz(ndimf3)
        dimension atemp(ndim3),adens(ndim3)
        dimension ane(ndim3),avr(ndim3)
        dimension avt(ndim3),avz(ndim3)
        dimension avtrb(ndim3)
        dimension adustd(ndim3),adustt(ndim3)
        dimension ftemp(ndimf1,ndimf2,ndimf3)
        dimension fdens(ndimf1,ndimf2,ndimf3),fne(ndimf1,ndimf2,ndimf3)
        dimension fvr(ndimf1,ndimf2,ndimf3),fvt(ndimf1,ndimf2,ndimf3)
        dimension fvz(ndimf1,ndimf2,ndimf3),fvtrb(ndimf1,ndimf2,ndimf3)
        dimension fdustd(ndimf1,ndimf2,ndimf3)
        dimension fdustt(ndimf1,ndimf2,ndimf3)
        dimension aint(ndim1,ndim2,mfreq)     
        dimension vr(ndim3),vt(ndim3),vz(ndim3),vtrb(ndim3)
        dimension dustd(ndim3),dustt(ndim3)
        dimension temp(ndim3),dens(ndim3),ekonc(ndim3),akonc(ndim3)
        dimension alam(mfreq),flux(mfreq,mphase),area(ndim1,ndim2)
        dimension fluxn(mfreq,mphase)
        dimension alpha(mphase)
        dimension sta(6,mion),d(3,matom),xi(8,matom),dyp(matom)
        dimension necod(matom),rr(ndim3,mion),stavs(ndim3,mion)
        dimension denxnb(mstarx),denznb(mstarx)
c	H+He
        dimension hi(ndim3),hneg(ndim3),h2(ndim3),hipf(ndim3,2)
        dimension hei(ndim3)
        dimension ophbf(ndim3),ophbf1(ndim3),ophbf2(ndim3)
        dimension ophff(ndim3),ophff1(ndim3),ophff2(ndim3)
        dimension ophrs(ndim3),ophrs1(ndim3),ophrs2(ndim3)
        dimension ophn(ndim3),ophn1(ndim3),ophn2(ndim3)
c	dust+gas
        dimension opmiex(mstarx,mspecx)
        dimension opmies(mstarx,mspecx),opmiea(mstarx,mspecx)
	dimension dtlow(mspecx),dthig(mspecx),drmf(mspecx),nmie(mspecx)
	dimension pmiex(mstarx),pmies(mstarx),pmiea(mstarx)
	dimension opmis(mspecx),opmia(mspecx)
        dimension pfmiex(mstarx),pfang(npfang),pfmie(mstarx,npfang)
	dimension pfmief(npfang)
        dimension xsecx(mstarx),xsecy(mstarx,mspecx)
        dimension txsec(mspecx),amixf(ndim3)
	dimension popt(mtemp),popd(mdens),pop(mtemp,mdens,melm)
c	stellar spectra+albedos
        dimension xstar1(mstarx,mspecx),star1(mstarx,mspecx)
        dimension nstar1(mspecx),tspec1(mspecx)
        dimension wstar1(mstarx),fstar1(mstarx)
	dimension xstar2(mstarx,mspecx),star2(mstarx,mspecx)
	dimension nstar2(mspecx),tspec2(mspecx)
	dimension wstar2(mstarx),fstar2(mstarx) 
        dimension xstar3(mstarx),star3(mstarx)
        dimension alb1x(mstarx),alb1y(mstarx)
        dimension alb2x(mstarx),alb2y(mstarx)
c        
        dimension iat(mline),iion(mline),wlab(mline)
        dimension elo(mline),eup(mline),glo(mline),bij(mline)
        dimension gr0(mline),gs0(mline),gw0(mline)
        dimension gr(mline),gs(mline),gw(mline)
        dimension aa(ndim3,mline),dop(ndim3,mline),popul(ndim3,mline)
        dimension zip(mion-1,mline),zipp(mion-1)
        dimension nion(mline),abund(mline),hmc(mline)
c-----------------------------------------------------------------------
        write(6,'(A)')
        write(6,'(A)')'                       Shellspec'
        write(6,'(A)')
        write(6,'(A)')'       INPUT: '
        write(6,'(A)')' shellspec.in - main input (geometry,objects...)'
        write(6,'(A)')' line.dat - atomic data for the sp. lines'
        write(6,'(A)')'                          (optional if iline=1)'        
        write(6,'(A)')' shellspec.mod - 3D model of the shell '
        write(6,'(A)')'                          (optional if imodel=2)'
        write(6,'(A)')' abundances  - element abundances'
        write(6,'(A)')'                          (optional if ichemc=1)'
        write(6,'(A)')' phases - orbital phases (optional if nphase=0)'  
        write(6,'(A)')' starspec1 - star spectrum (optional if lunt1>0)'
        write(6,'(A)')' starspec2 - star spectrum (optional if lunt2>0)'
        write(6,'(A)')' starspec3 - star spectrum (optional if lunt3>0)'
        write(6,'(A)')' albedo1 - albedo (optional if ialbst,irrst=1)'
        write(6,'(A)')' albedo2 - albedo (optional if ialbcp,irrcp=1)'        
        write(6,'(A)')' dust_opac - dust opacity (optional if imie>0)'                
        write(6,'(A)')' mie_phase - dust phase f.(optional if imiepf=1)'
        write(6,'(A)')' gas_opac - molec x-sectio (optional if iopac=1)'
        write(6,'(A)')' chem_eq_tab - molec popul (optional if iopac=1)'
	write(6,'(A)')' wind_prof - wind density (optional if idennb=1)'
        write(6,'(A)')'       OUTPUT: '
        write(6,'(A)')' shellspectrum - spectrum of the shell'
        write(6,'(A)')' lightcurve - LC or trailed spectrogram'        
        write(6,'(A)')' shellspec.out - more detail output'
        write(6,'(A)')' fort.xx - 2D images at some frequency'
	write(6,*)
	open(2,file='shellspec.out',status='unknown')
	open(4,file='shellspectrum',status='unknown')
	open(11,file='lightcurve',status='unknown')
cfor Slavek	
c	open(18,file='sh.txt',status='unknown')
c	open(19,file='shx.txt',status='unknown')
c	constants and units from NIST(2002)
	bol=1.3806503d-16
	clight=2.99792458d10
	hjed=1.66053873d-24
	plank=6.62606876d-27
	grav=6.673d-8
	rydb=109737.31568549d0
	rydbh=109677.585d0
	pi=3.1415926535897931d0
c	pi=dacos(-1.d0) seems to have the same precission
	elmass=9.10938188d-28
	stefb=5.670400d-5
c	1EV=X ERG, X CM^-1; 1ERG= X CM^-1 
	everg=1.602176462d-12
	evcm=8.06554477d3
	ergcm=5.03411762d15
c	Sun
c	Lang 1992
c	rsol=6.9599d10
c	Cox 2000
	rsol=6.95508d10
c	emsol=1.989d33
	emsol=1.9891d33
	pc=3.0857d18
	au=1.4960d13
c-----------------------------------------------------------------------
c       Definition of the input quantities:                             
c       alam1, alamn, alams -start,end and step of wavelength in [A]    
c       loglam=0 equidistant step im lambda
c       loglam=1 equidistant step in log(lambda), the number of steps
c               will be the same as for loglam=0
c       cutoff - extension of the <alam1,alamn> interval in [A] when
c               reading the gas_opac table. Assuming that 
c               broadening by the velocity field dominates:
c               cutoff>maximal radial velocity/c*lambda 
c       imodel=1  calculate your own input shell model                  
c       imodel=2  read input shell model from `shellspec.mod'.          
c               You can ignore most of input below defining geometry,   
c               the velocity field and state quantities of objects but
c               you must still input the data for the scattering:           
c                 rstar,tstar,vxst,vyst,vzst                              
c               for the coordinate rotation:                        
c                 temp0,ane0,xcp,ycp,zcp 
c               and for the limb darkening:
c                 istar,rstar,tstar,dlst,dlst2,
c                 icomp,rcp,tempcp,dlcp,dlcp2,xcp,qq
c               and switches: lunt1,lunt2,lunt3,ithom,irayl,
c                 imie,imiepf,ihyd,iopac,iline,eps
c       irotat -option of interpolation from the body frozen grid
c               to the line of sight grid during the coord. rotation
c               0=linear interpolation, good for continuous fields,
c                 otherwise the result may depend on discontinuities
c                 or background (temp0,ane0,...)
c               1=nearest neighbour approximation, may be less smooth
c                 but can handle discontinuities
c       ipart  -option of partition functions 
c               [1-built in Dworetsky & Smalley, 2-Irwin]
c               (only ipart=1 is implemented so far)
c       ichemc -option of abundances, if ielnd=1 then ichemc=1                                     
c               [0-default solar, 1-read from file `abundances']       
c       ielnd=1 electron number densities provided in the input model
c               are ignored and code calculates el.num.dens. 
c               assuming LTE, from known temperature, density and 
c               chemical composition. File 'abundances' is read and must
c               contain 3.column which specifies which elements are
c               considered in Ne calculations, this sets ichemc=1
c       ielnd=0 electron number densities are known apriori and are
c               specified in the input model
c       ithom=0 Thomson scattering is off
c       ithom=1 Thomson scattering from stars is on 
c               (assumes optically thin environment)
c       irayl=0 Rayleigh scattering on neutral hydrogen is off.
c               If Lyman lines are treated explicitely in the linelist
c               set irayl=0 not to count the contribution twice
c       irayl=1 Rayleigh scattering from stars on neutral hydrogen is on 
c               (assuming optically thin environment)
c       imie=0  Mie scattering and absorption on dust is off
c       imie=1  Mie scattering+absorption opacity is on.
c               Several species or input files can be included.
c               dust_opac file with tables must be provided.
c               Mie thermal and scattering emissivity on dust is on.
c               It is scattering of light from the stars assuming
c               optically thin medium.
c               Scattering emission can be isotropic or  
c               non-isotropic (see imiepf).
c       imie=2  Mie scattering+absorption opacity is on
c               Mie thermal emissivity is on, but
c               Mie scattering emissivity is off
c       imie=3  Mie scattering+absorption opacity is on
c               Mie thermal emissivity is on
c               Mie scattering emissivity is on but is isotropic and
c               assumes J=B(T) i.e. it is not scattered light from stars
c       imiepf  angular dependence of the scattered light from stars,
c               has an effect only if imie=1 
c       imiepf=1 angular dependent scattering emissivity, 
c               reads extra table with phase functions (mie_phase),
c               otherwise it is isotropic
c               In case there are several species in dust_opac 
c               this will redistribute the total scattering opacity. 
c       ihyd=1  hydrogen bound-free and free-free opacity is turned on
c               assuming only atomic H (no molecules)
c       iopac=1 additional tabulated gas true opacity is added
c               reads extra table with gas opacities (no scattering)
c       iline=0 No line opacity   
c       iline=1 line opacity is included. Spectral line parameters must
c               be specified in the file 'line.dat'
c       eps -artificial number <0.,1.> for test purpose which splits    
c               the line opacity (emissivity) into the true             
c               absorbtion (eps->1.) and coherent scattering (eps->0.). 
c               In LTE eps=1. ( S=eps*B+(1-eps)*J )  
c               If ithom=irayl=0 set also eps=1. for consistency                   
c       ionu, ior, iot -sequential indexes of frequency, x, and y point 
c               for which you want a more detailed output along the line
c               of sight (specified by x,y) 
c       offset -vertical shift applied to the normalized spectra output
c               to plot many spectra from different rotation phases        
c       phase1, phasen - start, end of the phase interval you want 
c               to cover [deg] (e.g. if xcp>0,ycp=zcp=0, dinc=90 then
c               phase1=-90 will start from the primary eclipse)
c       nphase -number of rotations (different view points) within 
c               the interval above
c               if nphase=0 then phase1 and phasen are ignored, and it
c               reads one column from the file `phases' with phases. 
c               These are values <0,1> and count from the x axis 
c               so that phase=0.0 or 1.0 is primary eclipse 
c               if xcp>0,ycp=zcp=0, dinc=90
c       dinc   -angle between rotation axis of the model and the line
c               of sight [deg], dinc=90.0 is edge on.
c       dd   -distance from the Earth in [pc]
c----------------intrinsic spectra specifications:
c       lunt1=0 all objects with density from <dcut1,dcut2> interval are 
c               nontransparent blackbodies with the same temperatures as
c               in the case of transparency. 
c       lunt1>0 all objects with density within <dcut1,dcut2> are 
c               nontransparent and have an intrinsic intensity spectrum.
c               The spectrum is read from file `starspec1'.
c       lunt1=1 the x,y column input required with wavelength [A] and
c               H_lambda flux [erg/cm^2/s/A] (as an output of SYNSPEC)
c       lunt1=2 the x,y column input required with wavelength [A] and
c               I_nu intensity [erg/cm^2/s/Hz/sterad] 
c       lunt1=3 the 4 column input required with idummy,frequency [Hz],
c               dummy, F_nu flux [erg/cm^2/s/Hz]
c               (output of coolTlusty, unit 21, first 2 rows are dummy) 
c       xunt1  -multiplication factor applied to starspec1 x-column
c               if it is not in the correct-required units
c               (otherwise set it =1.)
c       yunt1  -multiplication factor applied to starspec1 y-column
c               if it is not in the correct-required units
c               (otherwise set it =1.)
c       lunt2,xunt2,yunt2 -the same meaning as above except that these
c               deal with density interval <dcut2,dcut3> and
c               the spectrum is read from file `starspec2'. 
c       lunt3,xunt3,yunt3 -the same meaning as above except that these
c               deal with density interval <dcut3,dcutn> and
c               the spectrum is read from file `starspec3'.          
c-----------definitions of grids:
c       rmdfx1<rmdfx2, rmdfy1<rmdfy2, rmdfz1<rmdfz2 - define 
c         the box of the body frozen frame (if imodel=1)
c       rmdfx3<rmdfx4 -define an additional box on the x-coordinate 
c         It will have the same (rmdfy1,rmdfy2,rmdfz1,rmdfz2) dimension.
c         rmdfx1<rmdfx2<rmdfx3<rmdfx4
c       stepf -is a mean distance between the x,y grid points [R_sol]
c       stepfz -is a mean distance between the z grid points [R_sol]
c         They determine the number of grid points:
c       nbodf1, nbodf2, nbodf3 -number of grid points in x, y, z (>=1)
c         direction in body frozen coordinates of the model.
c         (Points are overridden 
c         by the values from `shellspec.mod' if imodel=2)
c       gainfx, gainfy, gainfz -grid step multiplication factors
c         of the body frozen grid to allow for logarithmic grid 
c         [gainfx=(x_{i+1}-x_{i})/(x_{i}-x_{i-1})]
c         e.g. gainfx=1. for equidistant step
c         gainfx>1. step increases symetrically from the middle to 
c         the left and to the right
c       rmdx1<rmdx2, rmdy1<rmdy2, rmdz1<rmdz2 - define the box
c         of the observer's line of sight frame. 
c       rmdz3<rmdz4 -define an additional box on the z-coordinate.
c         rmdz1<rmdz2<rmdz3<rmdz4
c         Observer looks along the opposite z-direction.
c       steps -is a mean distance between the x,y grid points [R_sol]
c       stepsz -is a mean distance between the z grid points [R_sol]
c         They determine the number of grid points:
c       nbod1, nbod2, nbod3 -number of grid points in x, y, z (>=1)  
c         in the line of sight observer's frame.
c       gainx, gainy, gainz -grid step multiplication factors
c         (common ratio of the geometric sequence) of the line of sight
c         grid    [gainx=(x_{i+1}-x_{i})/(x_{i}-x_{i-1})]
c         e.g. gainx=1. for equidistant step
c         gainx>1. step increases symetrically from the middle to 
c         the left and to the right
c-----------------object definitions:
c       istar,icomp,ienv,ispot,ism,iring,idisc,
c       inebl,iflow,ijet,iufo,ishell
c                 see below
c-------primary star (central object)----------
c       istar=0 accompanied by rstar=0 will switch off the primary
c       istar=1 central object is a nontransparent uniformly rotating
c         sphere. Its density is set to <dcut1,dcut2>. It can be either 
c         black body with T=tstar if lunt1=0 or has its intrinsic 
c         intensity spectrum if lunt1>0. In case of scattering or 
c         reflection of its light by other objects its rotation is 
c         ignored.
c         Code ignores: dgst,ffst,qq                 
c       istar=2 central object is a detached component of a binary.       
c         It has a Roche shape defined by ffst<=1, synchronous rotation,
c         is nonstrasparent with density within <dcut1,dcut2>.  
c         It can be either black body with T=tstar if lunt1=0 or 
c         has its intrinsic intensity spectrum if lunt1>0. 
c         You must also set: xcp>0,qq>0
c         Code also calculates/ignores: xstar,ystar,zstar,vrotst
c         ,drotst,hst,rstar
c       istar=3 central object is a figure 8 contact system. It has
c         a Roche shape defined by 1<ffst<=2, synchronous rotation,
c         is nonstrasparent with density within <dcut1,dcut2>.  
c         It can be either black body with T=tstar if lunt1=0 or 
c         has its intrinsic intensity spectrum if lunt1>0. 
c         You must also set: xcp>0,qq>0
c         Code also calculates/ignores: 
c           xstar,ystar,zstar,vrotst,drotst,hst,rstar,icomp
c       if istar>1 or icomp>1 or (istar>0 and icomp>0 and vxst>clight)
c         then code calculates (from emstar,xcp,qq):
c         ycp,zcp,vxst,vyst,vzst,vxcp,vycp,vzcp
c         assuming circular orbit.
c       rstar -radius of the central star in [R_sol] 
c         if istar>1 (Roche Geometry) this value will be used for
c         scattering in the circumstellar matter and irradiation effect 
c         on the companion which use spherical approximation
c       tstar -effective temperature of the central star in [K]
c         without gravity darkening and irradiation. This value will 
c         be used for scattering in the circumstellar matter (in case 
c         of black body) and irradiation effect on the companion
c         if istar=2 it is the temperature at the rotation pole 
c         if istar=3 it is the temperature at the rotation pole of
c         the more massive star
c       emstar -mass of the central star in [M_sol]
c       xstar,ystar,zstar -define unit aiming vector of the rotational        
c         axis of the central star         
c       vrotst -equatorial rotation velocity of the central star [km/s]
c         in case istar=1 corresponding to the equatorial angular vel.  
c       idifst -on/off differential rotation only for istar=1
c       idifst=0 no differential rotation
c       idifst=1 smooth differential rotation
c         omega(phi)=omega_eq-(omega_eq-omega_pol)*sin(phi)**2
c       idifst=2 step function differential rotation
c         omega(phi)=omega_eq  for z/rstar<hst
c         omega(phi)=omega_pol for z/rstar>hst
c       drotst - the ratio of angular velocity at the rotation pole to
c         the angular vel. at the equator: drotst=omega_pol/omega_eq.
c       hst -break in the step function =z/rstar for idifst=2
c       vxst, vyst, vzst -net velocity components                 
c         of the center of mass of the central star [km/s]
c         (if vxst>clight and istar>0 and icomp>0 then see istar)
c       dlst -limb darkening coefficient of the central star
c       dlst2 -second limb darkening coefficient 
c            I=1-dlst*(1-mu)-dlst2*(1-mu)**2
c       dgst -gravity darkening coefficient (beta) of the central star
c         (0.25 for radiative, 0.08 for convective atmospheres)
c         It is dummy if istar=1.
c       ffst<=1 -Roche lobe fill-in factor of the primary. Its is
c         the distance of the inner substellar point of the primary
c         (between the stars) from the center of the primary relative
c         to the distance to L1, the Roche lobe is reproduced if ff=1
c       1<ffst<=2 -Roche lobe fill-out factor of the contact system
c               ffst=(C1-C)/(C1-C2)+1
c               It is dummy if istar=1.
c       irrst=0 -irradiation and reflection effect is off
c           (ialbst,albst,htst,htsta have no meaning in this case)
c       irrst=1 -irradiation of the object from the companion is on.
c           Irradiation (heating) applies only if istar=1,2.
c           Reflection of the sp. of companion applies if istar=1,2
c           (rcp,tempcp>0 ... are presumed).
c       ialbst=1 monochomatic albedo is red from file=albedo1 
c           (if irrst=1). It should be compatible with Bond albedo.
c       albst  -Bond albedo <0,1>
c       htst   -heat redistribution parameter in case of irradiation, 
c           fraction of the heat absorbed on the day side which is 
c           redistributed over the day-night sides, <0,1>, 
c           0-nothing is redistributed and nothing goes to the night, 
c           1-all the energy (which is not reflected) impinging on 
c           the planet is evenly distributed over the day-night sides.
c           It is analoguous to the so called Pn parameter of A.Burrows
c           (a fraction of the irradiating energy impinging on 
c           the day side which is transfered to and irradiated from 
c           the night side), Pn=(1-albst)*htst/2
c       htsta  -degree of the inhomegenity of the heat transport, <0,1>.
c           1-homegeneous, 0-cosine dependence
c           T**4=T0**4(htsta+4(1-htsta)/pi*cos_latitude)
c       ispst=1/0 will turn on/off a spot on the star if istar=1
c           (it has the shape of a circle)
c       xspst,yspst,zspst -define unit aiming vector of the location
c           of the spot center on the surface 
c       aspst -angular radius of the spot in [deg]
c       tspst -ratio of the spot temperature to the ambient temperature
c           (i.e. temperature accounted for e.g. the reflection effect)
c-------
c       temp*,dens*,ane* - state quantities in various objects   
c               temperature, density, electron number density [K,CGS]
c       vtrb* - microturbulent velocity in various objects [km/s],
c               it does not apply to nontransparent objects
c       dstd* - density of dust in various objects [g/cm^3]
c               you must also set dens*>0. to have an effect
c       dstt* - temperature of dust in various objects [K]
c               it must be higher than the condensation temperature
c               of some species (see dust_opac) to have an effect
c-------companion or secondary star
c       icomp=0 secondary off
c       icomp=1 secondary on, it is a uniformly rotating nontransparent
c               sphere. It may be a blackbody with T=tempcp if lunt2=0
c               or has its own spectrum if lunt2>0. Its density is set 
c               to <dcut2,dcut3>. Code ignores: dgcp,ffcp,qq
c       icomp=2 secondary is a detached component of a binary.
c         It has a Roche shape defined by ffcp<=1, synchronous rotation,
c         is nonstrasparent with density within <dcut2,dcut3>.  
c         It can be either black body with T=tempcp if lunt2=0 or 
c         has its intrinsic intensity spectrum if lunt2>0. 
c         You must set: xcp>0,qq>0,emstar>0
c         Code also calculates/ignores: vrxcp,vrycp,vrzcp,vrotcp,rcp
c       rcp  -radius of the spherical companion [R_sol],
c         if icomp=2 this input is used only for the scattering 
c         and irradiation from the object otherwise it is superfluous 
c       tempcp -see primary star above, this value is used for 
c         the scattering on the circumstellar material and irradiation 
c         of the primary
c       qq -mass ratio (companion/star), important only for Roche geom.
c           if istar>1 or icomp>1
c       vrxcp, vrycp, vrzcp -define unit aiming vector of the rotational      
c         axis of the secondary star (companion)
c       vrotcp -equatorial rotation velocity of the companion [km/s]
c       xcp,ycp,zcp -location of the center (of mass) of 
c               the companion [R_sol]
c       vxcp,vycp,vzcp -components of the velocity vector of the center 
c               (of mass) of the companion [km/s]
c       dlcp -limb darkening coefficient of the secondary star
c       dlcp2 -second limb darkening coefficient (the same as dlst2)
c       dgcp -gravity darkening coefficient (beta) of the secondary
c       ffcp<=1 -Roche lobe filling factor of the secondary is 
c         the distance of the inner substellar point of the secondary
c         from the center of the secondary relative to 1-L1, 
c         the Roche lobe is reproduced if ffcp=1 
c       irrcp=0 -irradiation and reflection effect is off
c           (ialbcp,albcp,htcp,htcpa have no meaning in this case)
c       irrcp=1 -irradiation of the secondary from the primary is on.
c           Irradiation (heating) applies only if icomp=2.
c           Reflection (of the spectrum of primary) applies if icomp=1,2
c           (istar=1,2 and rstar,tstar>0 are presumed)
c       ialbcp=1  monochomatic albedo is red from file=albedo2
c           (if irrcp=1). It should be compatible with the Bond albedo.
c       albcp  -Bond albedo <0,1>
c       htcp   -heat transport parameter in case of the irradiation. 
c           The same as htst, <0,1>.
c       htcpa  -degree of the inhomegenity of the heat transport, <0,1>. 
c           1-homegeneous, 0-cosine dependence, the same as htsta.
c-------envelope around the primary star
c       ienv,emen,ggen,ffen have similar meaning to istar,emstar,qq,ffst
c       ienv=2 envelope is on, has a detached Roche shape
c       ienv=3 envelope is on, has a contact Roche shape
c           (common envelope)
c       emen -mass of the central star [M_sol]
c       qqen -mass ratio (companion/star)
c       ffen<=1 -Roche lobe fill-in factor of the detached envelope. 
c         Its is radius of the substellar point of the envelope
c         relative to the radius of the L1. Roche lobe has ffen=1.
c       1<ffen<=2 -Roche lobe fill-out factor of the contact envelope
c               ffen=(C1-C)/(C1-C2)+1
c       hen -vertical limit [R_sol], limits the envelope in 
c         the direction perpendicular to the orbital plane to z<+-hen
c       tempen -constant temperature [K]
c       densen -constant gas density [g/cm^3]
c       aneen -constant electron number density [cm^-3]
c       dstden -constant dust density [g/cm^3]
c               you must also set densen>0. to have an effect
c       dstten -constant dust temperature [K]
c               it must be higher than the condensation temperature
c               of some species (see dust_opac) to have an effect
c       vtrben -microturbulence [km/s]
c-------spot or third star
c       ispot=0 spot is off
c       ispot=1 spot is on, it is a uniformly rotating sphere
c       vrxsp, vrysp, vrzsp -define unit aiming vector of the rotational      
c               axis of the spot
c       vrotsp -equatorial rotation velocity of the spot [km/s]
c       rsp  -radius of the spherical spot [R_sol]
c       xsp,ysp,zsp -location of the center of the spot [R_sol]
c       vxsp,vysp,vzsp -components of the velocity vector of the center 
c               of the spot [km/s]
c       tempsp -constant temperature [K]
c       denssp -gas density [g/cm^3]
c       anesp -electron number density [cm^-3]
c       dstdsp -dust density [g/cm^3]
c               you must also set denssp>0. to have an effect
c       dsttsp -dust temperature [K]
c               it must be higher than the condensation temperature
c               of some species (see dust_opac) to have an effect
c       vtrbsp -microturbulence [km/s]
c-------stream
c       ism=0/1  -stream off/on
c       v1sm   -stream velocity at the beginnig of stream [km/s]
c       v2sm   -stream velocity at the end of stream [km/s]
c         velocity is directed from beginning to end 
c       r1sm    -radius of the stream at the beginning [R_sol]
c       r2sm    -radius of the stream at the end [R_sol]
c         notice that although the radius changes the streamlines 
c         are parallel (contrary to jet) 
c       x1sm,y1sm,z1sm -position of the beginning of the stream [R_sol]
c       x2sm,y2sm,z2sm -position of the end of the stream [R_sol]
c       vxsm, vysm, vzsm -net velocity [km/s]
c         you can use it also to mimic orbital drag or if the center
c         of rotation is not at the center of coordinates
c       xsm,ysm,zsm -rotational vector of stream
c       psm -rotational period of stream in days
c       tempsm -temperature [K], constant along the stream 
c       denssm - is density at the beginning [g/cm^3] and scales along
c         the stream to satisfy the continuity equation: 
c         density=denssm*v1sm*r1sm**2/(vsm*rsm**2)*exp(t/rsol*edensm)
c         where t is distance along the stream and exp term allows
c         e.g. for a dust destruction
c       anesm - electron number density at the beginning [cm^-3],
c         similar to the density but if ielnd=1 then it is overriden by
c         the calculation from the state quantities
c       edensm -density dependence exponent to enable the modeling 
c         of additional phenomena
c       dstdsm -dust density [g/cm^3], it changes along the stream like 
c         the gas density
c         you must also set denssm>0. to have an effect
c       dsttsm -dust temperature [K], constant along the stream
c               it must be higher than the condensation temperature
c               of some species (see dust_opac) to have an effect
c       vtrbsm -microturbulence velocity [km/s]
c-------ring
c       iring>0 ring is on
c       rrg -radius of the ring [R_sol]
c       emrg -mass in its center to calculate velocities [Msol]
c       b1rg, b2rg -specifies the arc from-to in [deg], b1rg><b2rg
c         The location of the zero angle is not simple to explain so 
c         test it first or consult subroutine trans. In many cases
c         it will be along the x axis.
c       a1rg,a2rg -vertical half width of the ring at the beginning
c         and end of the arc in [R_sol]
c       dr1rg, dr2rg -horizontal half thickness the ring at 
c         the beginning and end of the arc in [R_sol]
c         The crosssection, C, of the ring may vary along the arc and
c         is C1=4*a1rg*dr1rg at the beginning.
c       xrg, yrg, zrg -location of the center in [R_sol]
c       xpolrg, ypolrg, zpolrg -orientation of the polar axis
c       vxrg, vyrg, vzrg -net overall space velocity [km/s]
c       edenrg, ede2rg -density dependence exponent to enable 
c         the modeling of additional phenomena. Density, dust density 
c         and electron number density change along the ring (arc) 
c         to safisfy continuity equation+additional phenomenon 
c         e.g. destruction (lifetime) of dust grains along the arc. 
c       dstdrg, dst2rg -dust density at the beginning (b1rg). 
c               you must also set densrg>0. to have an effect
c       If itrg=1 then
c         gas density=densrg*C1/C*[|t-b1rg|/pi+1]**edenrg
c         electron num. density=anerg*C1/C*[|t-b1rg|/pi+1]**edenrg
c         dust density=dstdrg*C1/C*[|t-b1rg|/pi+1]**edenrg+
c                 dst2rg*C1/C*[|t-b1rg|/pi+1]**ede2rg
c       itrg>or< 1 then
c         gas density=densrg*C1/C*dexp[|t-b1rg|/pi]**edenrg
c         electron num. density=anerg*C1/C*dexp[|t-b1rg|/pi]**edenrg
c         dust density=dstdrg*C1/C*dexp[|t-b1rg|/pi]**edenrg+
c                dst2rg*C1/C*dexp[|t-b1rg|/pi]**ede2rg
c         where t-is angle along the arc.
c       densrg -gas density at b1rg
c       anerg -electron number density at b1rg
c       temprg -constant gas temperature [K]
c       dsttrg -constant dust temperature [K]
c               it must be higher than the condensation temperature
c               of some species (see dust_opac) to have an effect
c       vtrbrg -microturbulence
c-------disk (accretion disk around some object)
c       idisc=0 switch off the disc		
c       idisc=1 disc has the shape of a rotating wedge
c               limited by inner and outer radii (spherical surfaces)
c       idisc=2 disc has the shape of a slab
c               limited by inner and outer radii (spherical surfaces)
c       idisc=3 disc has the shape of a rotating ellipsoid
c               limited by inner spherical and outer ellipsoidal surface
c       adisc -angular halfwidth of the disc wedge [deg] 
c              (if idisc=1)
c             -half of the thickness of the disc slab [R_sol] 
c              (if idisc=2)
c             -semiaxis of the ellipsoid along the rotational axis 
c              [R_sol] (if idisc=3)	
c       rindc -inner radius of the disc [R_sol]
c       routdc -outer radius of the disc [R_sol] or
c              -semiaxis of the ellipsoid perpendicular to the rotation 
c               axis, if idisc=3, [R_sol]
c       emdc -mass of the object in the disk center [M_sol]
c         it determines its Keplerian velocity
c       rdc  -radius of the object in the disk center [R_sol]
c         it determines its temperature structure if itdc=2
c       xdc,ydc,zdc -location of the disk center in [R_sol]
c       xdisc,ydisc,zdisc -components of the unit aiming vector of 
c               the rotational axis of the Keplerian disc around emstar
c       vxdc, vydc, vzdc -net velocity components                 
c               of the center of the disc [km/s]
c       densdc -gas density at rindc 
c       anedc  -electron num. density at rindc 
c       tempdc -characteristic gas temperature, see below
c       edendc -radial density dependence exponent 
c               (dens, ane and dust density are a function of r)
c               Rho(r) ~ Ne(r) ~ densdc*(r/rindc)**edendc	                        
c       itdc=1  disc temperature is constant (=tempdc)
c       itdc=2  disc temperature is a function of r (accretion discs)
c               T(r)=tempdc*(rdc/r)**0.75*(1-(rdc/r)**0.5)**0.25       
c       itdc=3  disc temperature as a power law (e.g. protopl. discs)
c               T(r)=tempdc*(r/rindc)**etmpdc
c       etmpdc  -exponent of the radial temperature dependence
c       dstddc  -dust density at rindc
c               you must also set densdc>0. to have an effect
c       dsttdc  -characteristic dust temperature 
c               dust temperatures behave like gas temperatures for
c               different itdc but with dsttdc instead of tempdc,
c               it must be higher than the condensation temperature
c               of some species (see dust_opac) to have an effect
c       vtrbdc -microturbulence [km/s]
c-------nebula (protoplanetary disk/nebula around central object)
c              it is defined in cylindrical coordinates (r,z)
c       inebl not=4  -nebula off
c       inebl=4 flared protoplanetary disk
c               vertical scale height is H(r)=(gamma*k*T_gas/m)**0.5
c             vertical structure:
c               fdens=dens0*dexp(-erz**2/hscale**2/2.d0)
c               gas temperature may have temperature inversion
c             radial structure:
c               surface density decreases ~(r/rinnb)**edennb
c               dust dens & electron num. dens are ~ density
c               temperatures change with radius
c       aneb    -vertical extension of nebula at particular r in [H]
c               extension(r)=+-aneb* H(r)
c       rinnb   -inner radius of the nebula [R_sol]
c       routnb  -outer radius of the nebula [R_sol]
c       emnb    -mass of the object in the nebula center [M_sol]
c       rnb     -radius of the object in the ufo center [R_sol]
c       hinvnb  start of vertical gas temp. inversion in [H]
c               for z(r)>hinvnb*H(r) if itnb=3
c       tinvnb  temperature multiplication factor in the inversion
c	        gas temp(z,r)=temp0(r)*tinvnb
c	hwindnb -vertical scale-height of the wind region
c               rho(z)=rho(0)*dexp(-erz**2/hscale**2/2.d0)
c               but for z>hwindnb*H
c               rho(z)=rho(0)*dexp(-hwindnb**2/2.d0)
c               i.e. rho(z)=rho(hwindnb*H)= const  
c               electron n.d. and dust density are proportional to gas
c               and thus will also have wind region
c       idennb=1 reads file wind_prof with rho=f(z)
c       xneb,yneb,zneb -components of the unit aiming vector of 
c               the rotational axis of the Keplerian disc around emnb
c       vxnb, vynb, vznb -net velocity components                 
c               of the center of the nebula [km/s]
c       tempnb  -characteristic gas temperature [K]
c       itnb=1  nebula gas and dust temperatures are constant
c       itnb=2  nebula gas and dust temp. are a function of r only
c               T(r)=tempnb*(Rnb/r)**0.75*(1-(Rnb/r)**0.5)**0.25
c               T(r)=dsttuf*(Rnb/r)**0.75*(1-(Rnb/r)**0.5)**0.25       
c       itnb=3  disc temperature as a power law (e.g. protopl. discs)
c               T(r)=tempnb*(r/rinnb)**etmpnb
c               there may be a gas temperature inversion in z
c       etmpnb  -exponent of radial temperature dependence
c       densnb -gas density at rinnb (at midplane)
c       anenb  -electron num. density at rinnb (at midplane)
c       edennb -radial density dependence exponent of surface density
c               (dens, ane and dust density are a function of r)
c               Ne(r,z) ~ Rho_dust(r,z) ~ Rho_gas(r,z)
c       dstdnb -dust density at rinnb [g/cm^3] (at midplane)
c               you must also set densnb>0. to have an effect
c       dsttnb -characteristic dust temperature [K]
c               it must be higher than the condensation temperature
c               of some species (see dust_opac) to have an effect
c       vtrbuf -microturbulence [km/s]
c-------flow
c         it is identical to the stream but lower priority
c       iflow=0/1  -stream off/on
c       v1fw   -stream velocity at the beginnig of stream [km/s]
c       v2fw   -stream velocity at the end of stream [km/s]
c         velocity is directed from beginning to end 
c       r1fw    -radius of the stream at the beginning [R_sol]
c       r2fw    -radius of the stream at the end [R_sol]
c         notice that although the radius changes the streamlines 
c         are made paralel (contrary to jet) 
c       x1fw,y1fw,z1fw -position of the beginning of the stream [R_sol]
c       x2fw,y2fw,z2fw -position of the end of the stream [R_sol]
c       vxfw, vyfw, vzfw -net velocity [km/s]
c         you can use it also to mimic orbital drag or if the center
c         of rotation is not at the center of coordinates
c       xfw,yfw,zfw -rotational vector of stream
c       pfw -rotational period of stream in days
c       tempfw -temperature [K], constant along the stream 
c       densfw - is density at the beginning and scales along the stream
c         to satisfy the continuity equation: 
c         density=densfw*v1fw*r1fw**2/(vfw*rfw**2)*exp(t/rsol*edenfw)
c         where t is distance along the stream
c       anefw - electron number density at the beginning, similar to 
c         the density but if ielnd=1 then it is overriden by 
c         the calculation from the state quantities
c       edenfw -density dependence exponent to enable the modeling 
c         of additional phenomena
c       dstdfw -dust density [g/cm^3], it changes along the stream like 
c         the gas density,
c               you must also set densfw>0. to have an effect
c       dsttfw -dust temperature [K], constant along the stream 
c               it must be higher than the condensation temperature
c               of some species (see dust_opac) to have an effect
c       vtrbfw -microturbulence velocity [km/s]
c-------jet
c       ijet=0   switch off the jet
c       ijet=1  jet has only one -primary cone	
c       ijet=2  jet has two cones: primary cone and opposite one
c       ajet -angle halfwidth of the jet cones [deg]
c         streamlines flare according to the opening angle
c       rinjt, routjt -radius boundaries of the jet cones [R_sol]
c       vjt  -radial (expanding) velocity of the jet [km/s]
c       xjet,yjet,zjet -components of the unit aiming vector 
c               of the primary jet cone
c       vxjt, vyjt, vzjt -net velocity component [km/s] 
c       tempjt -temperature [K], constant in the jet
c       densjt -gas density [g/cm**3] at rinjt, it scales along the jet
c         to satisfy the continuity equation
c         density=densjt*rinjt**2/routjt**2
c       anejt - electron number density [cm**-3] at rinjt. It changes
c         along the jet like the gas density but if ielnd=1 then 
c         it is overriden by the calculation from the state quantities
c       dstdjt -dust density [g/cm**3] at rinjt, changes along the jet 
c         like the gas density
c               you must also set densjt>0. to have an effect
c       dsttjt -dust temperature [K], constant in the jet
c               it must be higher than the condensation temperature
c               of some species (see dust_opac) to have an effect
c       vtrbjt -microturbulence [km/s]
c-------ufo 
c         it is identical to DISK (same subroutine) but lower priority
c       iufo=0 switch off the ufo		
c       iufo=1 ufo has the shape of a rotating wedge
c               limited by inner and outer radii (spherical surfaces)
c       iufo=2 ufo has the shape of a slab
c               limited by inner and outer radii (spherical surfaces)
c       iufo=3 ufo has the shape of a rotating ellipsoid
c               limited by inner spherical and outer ellipsoidal surface
c       aufo -angular halfwidth of the ufo wedge [deg] 
c              (if iufo=1)
c             -half of the thickness of the ufo slab [R_sol] 
c              (if iufo=2)
c             -semiaxis of the ellipsiod along the rotational axis 
c              [R_sol] (if iufo=3)
c       rinuf -inner radius of the ufo [R_sol]
c       routuf -outer radius of the ufo [R_sol] or
c              -semiaxis of the ellipsoid perpendicular to the rotation 
c               axis, if iufo=3, [R_sol]
c       emuf -mass of the object in the ufo center [M_sol]
c       ruf  -radius of the object in the ufo center [R_sol]
c       xuf,yuf,zuf -location of the disk center in [R_sol]
c       xufo,yufo,zufo -components of the unit aiming vector of 
c               the rotational axis of the Keplerian disc around emuf
c       vxuf, vyuf, vzuf -net velocity components                 
c               of the center of the ufo [km/s]
c       tempuf  -temperature [K]
c       ituf=1  ufo gas and dust temperatures are constant
c       ituf=2  ufo gas and dust temperatures are a function of r
c               T(r)=tempuf*(Ruf/r)**0.75*(1-(Ruf/r)**0.5)**0.25
c               T(r)=dsttuf*(Ruf/r)**0.75*(1-(Ruf/r)**0.5)**0.25       
c       ituf=3  disc temperature as a power law (e.g. protopl. discs)
c               T(r)=tempdc*(r/rindc)**etmpuf
c       etmpuf  -exponent of radial temperature dependence
c       densuf -gas density at rinuf 
c       aneuf  -electron num. density at rinuf 
c       edenuf -radial density dependence exponent 
c               (dens, ane and dust density are a function of r)
c               Rho(r) ~ Ne(r) ~ densuf*(r/rinuf)**edenuf
c       dstduf -dust density at rinuf [g/cm^3] 
c               you must also set densuf>0. to have an effect
c       dsttuf -dust temperature [K]
c               it must be higher than the condensation temperature
c               of some species (see dust_opac) to have an effect
c       vtrbuf -microturbulence [km/s]
c-------shell
c       ishell=0   switch off the shell
c       ishell=1   velocity, dens, temp, ane are constant
c       ishell=2   radial velocity is v(r)=vsh*(r/rinsh)**evelsh
c           Ne(r)~Rho(r)=denssh*(rinsh/r)**2*vsh/v(r)), temp=const.
c       ishell=3   radial velocity is v(r)=vsh*(1-rcsh/r)**evelsh
c           Ne(r)~Rho(r)=denssh*(rinsh/r)**2*v(rinsh)/v(r)),temp=const.
c       rinsh, routsh -inner, outer radius of the shell in [R_sol]     
c       vsh -velocity of the uniformly expanding shell [km/s]           
c       evelsh -exponent of velocity dependence
c       rcsh - core/photospheric radius of the star in shell [R_sol]
c       vxsh, vysh, vzsh -net velocity [km/s]
c       tempsh -temperature [K]
c       denssh -gas density at rinsh
c       anesh -electron number density at rinsh
c       dstdsh -dust density at rinsh [g/cm^3], 
c         it changes as the gas density
c               you must also set denssh>0. to have an effect
c       dsttsh -dust temperature [K]
c               it must be higher than the condensation temperature
c               of some species (see dust_opac) to have an effect
c       vtrbsh -microturbulence [km/s]
c-------background
c       v0 -constant uniformly expanding velocity of background [km/s]
c       temp0 -temperature [K]
c       dens0 -gas density [g/cm^3](note dust density is =0 in the code)
c       ane0 -electron number density [cm^-3]
c-----------------------------------------------------------------------                                                                       
c       If the objects happen to overlap, priority is given by the order
c       of 'if'-blocks in the subroutine smod1 and it is as follows:         
c               star,companion,spot,stream,ring,disc,nebula,flow,
c               jet,ufo,shell,and background.
c       temp and ane are assumed to have reasonable values all along 
c       the beam. An empty space can be defined as dens<denvac.         
c       Four types of nontransparent objects can be defined as:         
c       dcut1<dens<dcut2 -central star,                                 
c       dcut2<dens<dcut3 -secondary star(=companion), 
c       dcut3<dens<dcutn -3.body (it can be anything)
c       dcutn<dens -any opaque dark matter.                              
c       These objects are allowed to make eclipses along the line of     
c       sight but cast no shadows into other directions (i.e. are       
c       transparent when considering scattered light from               
c       the central star).                                               
c       Note that lunt1, lunt2, lunt3 are in fact associated with 
c       density intervals (<dcut1,dcut2>, <dcut2,dcut3>, <dcut3,dcutn>) 
c       rather then with objects (star,companion,...) and thus can be 
c       used to ascribe the spectrum to any nontransparent object 
c       setting its density within a particular density interval.
c       However, limb darkening is applied to star and companion only
c       and it must be switched off (dlst=dlcp=0.) if you want to use 
c       these density intervals for other objects (without limb dark.).
c       Roche geometry assumes synchronous rotation around z axis with 
c       star in the center and companion at xcp>0,ycp=zcp=0 revolving 
c       towards (0,1,0).
c       Input variables which are supposed to be components of a unit 
c       vector do not need to be normalized.
        dcut1=0.5d15
        dcut2=1.5d15
        dcut3=2.5d15
        dcutn=3.5d15
        denvac=1.d-50
	open(9,file='shellspec.in',status='old')
	read(9,*)
	read(9,*)alam1,alamn,alams,loglam,cutoff
	read(9,*)
	read(9,*)imodel,irotat,ipart,ichemc,ielnd
	read(9,*)
	read(9,*)ithom,irayl,imie,imiepf,ihyd,iopac,iline,eps	
	read(9,*)
	read(9,*)ionu,ior,iot,offset
	read(9,*)
	read(9,*)phase1,phasen,nphase,dinc
	read(9,*)
	read(9,*)dd
	read(9,*)
	read(9,*)
	read(9,*)lunt1,xunt1,yunt1
	read(9,*)
	read(9,*)lunt2,xunt2,yunt2
	read(9,*)
	read(9,*)lunt3,xunt3,yunt3
	read(9,*)
	read(9,*)
	read(9,*)rmdfx1,rmdfx2,rmdfy1,rmdfy2,rmdfz1,rmdfz2,rmdfx3,rmdfx4
	read(9,*)
	read(9,*)stepf,stepfz,gainfx,gainfy,gainfz
	read(9,*)
	read(9,*)
	read(9,*)rmdx1,rmdx2,rmdy1,rmdy2,rmdz1,rmdz2,rmdz3,rmdz4
	read(9,*)
        read(9,*)steps,stepsz,gainx,gainy,gainz
        read(9,*)
        read(9,*)
c
        read(9,*)istar,icomp,ienv,ispot,ism,iring,idisc
	read(9,*)
        read(9,*)inebl,iflow,ijet,iufo,ishell
        read(9,*)
        read(9,*)
	read(9,*)rstar,tstar,emstar
        read(9,*)
        read(9,*)xstar,ystar,zstar,vrotst
        read(9,*)
        read(9,*)idifst,drotst,hst
        read(9,*)
        read(9,*)vxst,vyst,vzst
        read(9,*)
        read(9,*)dlst,dlst2,dgst,ffst
        read(9,*)
        read(9,*)irrst,ialbst,albst,htst,htsta        
	read(9,*)
	read(9,*)ispst,xspst,yspst,zspst,aspst,tspst
	read(9,*)
	read(9,*)
	read(9,*)rcp,tempcp,qq
	read(9,*)
	read(9,*)vrxcp,vrycp,vrzcp,vrotcp
	read(9,*)
	read(9,*)xcp,ycp,zcp
	read(9,*)
	read(9,*)vxcp,vycp,vzcp	
	read(9,*)
	read(9,*)dlcp,dlcp2,dgcp,ffcp
	read(9,*)
	read(9,*)irrcp,ialbcp,albcp,htcp,htcpa	
	read(9,*)
	read(9,*)
	read(9,*)emen,qqen,aen,ffen,hen
	read(9,*)
	read(9,*)tempen,densen,aneen,vtrben,dstden,dstten
	read(9,*)
	read(9,*)
	read(9,*)vrxsp,vrysp,vrzsp,vrotsp,rsp
	read(9,*)
	read(9,*)xsp,ysp,zsp,vxsp,vysp,vzsp
	read(9,*)
	read(9,*)tempsp,denssp,anesp,vtrbsp,dstdsp,dsttsp
	read(9,*)
        read(9,*)
        read(9,*)v1sm,v2sm,r1sm,r2sm
        read(9,*)
        read(9,*)x1sm,y1sm,z1sm
        read(9,*)
        read(9,*)x2sm,y2sm,z2sm
        read(9,*)
        read(9,*)vxsm,vysm,vzsm
        read(9,*)
        read(9,*)xsm,ysm,zsm,psm
        read(9,*)
        read(9,*)tempsm,denssm,anesm,vtrbsm,edensm,dstdsm,dsttsm
	read(9,*)
	read(9,*)
	read(9,*)rrg,emrg
	read(9,*)
	read(9,*)b1rg,b2rg
	read(9,*)
	read(9,*)a1rg,a2rg,dr1rg,dr2rg
	read(9,*)
	read(9,*)xrg,yrg,zrg
	read(9,*)
	read(9,*)xpolrg,ypolrg,zpolrg
	read(9,*) 
	read(9,*)vxrg,vyrg,vzrg
	read(9,*) 
	read(9,*)temprg,densrg,anerg,vtrbrg,itrg
	read(9,*)
	read(9,*)edenrg,dstdrg,ede2rg,dst2rg,dsttrg
	read(9,*)	
	read(9,*)
        read(9,*)adisc,rindc,routdc,emdc,rdc
	read(9,*)
        read(9,*)xdc,ydc,zdc
	read(9,*)
        read(9,*)xdisc,ydisc,zdisc
        read(9,*)
        read(9,*)vxdc,vydc,vzdc
        read(9,*)
        read(9,*)tempdc,densdc,anedc,vtrbdc,edendc,itdc,etmpdc
        read(9,*)
        read(9,*)dstddc,dsttdc
	read(9,*)	
	read(9,*)
        read(9,*)aneb,rinnb,routnb,emnb,rnb
        read(9,*)
        read(9,*)hinvnb,tinvnb,hwindnb,idennb
	read(9,*)
        read(9,*)xneb,yneb,zneb
        read(9,*)
        read(9,*)vxnb,vynb,vznb
        read(9,*)
        read(9,*)tempnb,densnb,anenb,vtrbnb,edennb,itnb,etmpnb
        read(9,*)
        read(9,*)dstdnb,dsttnb
        read(9,*)
        read(9,*)
        read(9,*)v1fw,v2fw,r1fw,r2fw
        read(9,*)
        read(9,*)x1fw,y1fw,z1fw
        read(9,*)
        read(9,*)x2fw,y2fw,z2fw
        read(9,*)
        read(9,*)vxfw,vyfw,vzfw
        read(9,*)
        read(9,*)xfw,yfw,zfw,pfw        
        read(9,*)
        read(9,*)tempfw,densfw,anefw,vtrbfw,edenfw,dstdfw,dsttfw
	read(9,*)
	read(9,*)
        read(9,*)ajet,rinjt,routjt,vjt
        read(9,*)
        read(9,*)xjet,yjet,zjet
        read(9,*)
        read(9,*)vxjt,vyjt,vzjt
        read(9,*)
        read(9,*)tempjt,densjt,anejt,vtrbjt,dstdjt,dsttjt
	read(9,*)
	read(9,*)
        read(9,*)aufo,rinuf,routuf,emuf,ruf
	read(9,*)
        read(9,*)xuf,yuf,zuf
	read(9,*)
        read(9,*)xufo,yufo,zufo
        read(9,*)
        read(9,*)vxuf,vyuf,vzuf
        read(9,*)
        read(9,*)tempuf,densuf,aneuf,vtrbuf,edenuf,ituf,etmpuf
        read(9,*)
        read(9,*)dstduf,dsttuf
	read(9,*)
	read(9,*)
	read(9,*)rinsh,routsh,vsh
	read(9,*)
	read(9,*)evelsh,rcsh
        read(9,*)
        read(9,*)vxsh,vysh,vzsh	
	read(9,*)
	read(9,*)tempsh,denssh,anesh,vtrbsh,dstdsh,dsttsh
        read(9,*)
        read(9,*)
        read(9,*)temp0,dens0,ane0,v0
c	only ipart=1 is suppoted at the moment
	ipart=1
	nfreq=nint((alamn-alam1)/alams)+1
	write(*,*)' nfreq=',nfreq
	nbodfa=nint((rmdfx2-rmdfx1)/stepf)+1
	nbodfb=nint((rmdfx4-rmdfx3)/stepf)+1
	nbodf2=nint((rmdfy2-rmdfy1)/stepf)+1
	nbodf3=nint((rmdfz2-rmdfz1)/stepfz)+1
	nbod1=nint((rmdx2-rmdx1)/steps)+1
	nbod2=nint((rmdy2-rmdy1)/steps)+1
	nboda=nint((rmdz2-rmdz1)/stepsz)+1
	nbodb=nint((rmdz4-rmdz3)/stepsz)+1
	if(nbodfa.lt.3.or.nboda.lt.3)then
          write(*,*)' error: nbodfa,nboda, stop'
          goto 590
        endif
c       make the number of points odd
	if(nbodfa/2.eq.(nbodfa+1)/2)nbodfa=nbodfa-1
	if(nbodfb/2.eq.(nbodfb+1)/2)nbodfb=nbodfb-1
	if(nbodf2/2.eq.(nbodf2+1)/2)nbodf2=nbodf2-1
	if(nbodf3/2.eq.(nbodf3+1)/2)nbodf3=nbodf3-1
	if(nbod1/2.eq.(nbod1+1)/2)nbod1=nbod1-1
	if(nbod2/2.eq.(nbod2+1)/2)nbod2=nbod2-1
	if(nboda/2.eq.(nboda+1)/2)nboda=nboda-1
	if(nbodb/2.eq.(nbodb+1)/2)nbodb=nbodb-1
	if(nbodfb.lt.3)nbodfb=0
	if(nbodb.lt.3)nbodb=0	
	nbodf1=nbodfa+nbodfb
	nbod3=nboda+nbodb
	write(*,*)'nbodfa nbodf2 nbodf3 nbodfb  (body frozen grid)'
	write(*,'(4i5)')nbodfa,nbodf2,nbodf3,nbodfb
	write(*,*)'nbod1 nbod2 nboda  nbodb  (line of sight grid)'
	write(*,'(4i5)')nbod1,nbod2,nboda,nbodb
	if(nbodf1.gt.ndimf1.or.nbodf2.gt.ndimf2.or.nbodf3.gt.ndimf3)then
	  write(*,*)' error: bf space dimension exceeded, stop'
	  write(*,*)'ndimf1 ndimf2 ndimf3  (body frozen grid)'
	  write(*,'(4i5)')ndimf1,ndimf2,ndimf3
	  goto 590
	endif
	if(nbod1.gt.ndim1.or.nbod2.gt.ndim2.or.nbod3.gt.ndim3)then
	  write(*,*)' error: ls space dimension exceeded, stop'
 	  write(*,*)'ndim1 ndim2 ndim3  (line of sight grid)'
	  write(*,'(4i5)')ndim1,ndim2,ndim3
	  goto 590
	endif	
	if(nfreq.gt.mfreq)then
	  write(*,*)' error: nfreq>mfreq dimension exceeded, stop'
	  write(*,*)' nfreq, mfreq=',nfreq,mfreq
	  goto 590
	endif		
	if(loglam.eq.0)then
	  do i=1,nfreq
	    alam(i)=alam1+dble(i-1)*alams
          enddo
        else
          alams=(dlog10(alamn)-dlog10(alam1))/dble(nfreq-1)
          do i=1,nfreq
             alam(i)=dlog10(alam1)+dble(i-1)*alams
             alam(i)=10**(alam(i))
          enddo   
        endif
	if(nphase.eq.0)then
	  write(*,*)' reading file phases'
	  open(15,file='phases',status='old')
	  i=1
15	  read(15,*,err=17,end=17)alpha(i)
	  alpha(i)=(alpha(i)-0.25d0)*2.d0*pi
	  i=i+1
	  goto 15
17	  nphase=i-1
	  close(15)
	elseif(nphase.eq.1)then
	  alpha(1)=phase1/180.d0*pi
	elseif(nphase.gt.1)then
	  alphas=(phasen-phase1)/dble(nphase-1)
	  do 20 i=1,nphase
	    alpha(i)=(phase1+dble(i-1)*alphas)/180.d0*pi
20 	  continue
	endif
	write(*,*)' nphase=',nphase
c	conversion of time
	psm=psm*24.d0*3600.d0
	pfw=pfw*24.d0*3600.d0
c	conversion of masses	
	emstar=emstar*emsol
	emen=emen*emsol
	emdc=emdc*emsol
	emnb=emnb*emsol
	emuf=emuf*emsol
	emrg=emrg*emsol
c	conversion of length	
	rstar=rstar*rsol
	rcp=rcp*rsol
        xcp=xcp*rsol
        ycp=ycp*rsol
        zcp=zcp*rsol
        aen=aen*rsol
        hen=hen*rsol
	rsp=rsp*rsol
        xsp=xsp*rsol
        ysp=ysp*rsol
        zsp=zsp*rsol
	rinsh=rinsh*rsol
	routsh=routsh*rsol
	rcsh=rcsh*rsol
	rindc=rindc*rsol
	routdc=routdc*rsol
	rdc=rdc*rsol
	xdc=xdc*rsol
	ydc=ydc*rsol
	zdc=zdc*rsol
	rinnb=rinnb*rsol
	routnb=routnb*rsol
	rnb=rnb*rsol
	rinuf=rinuf*rsol
	routuf=routuf*rsol
	ruf=ruf*rsol
	xuf=xuf*rsol
	yuf=yuf*rsol
	zuf=zuf*rsol
	rinjt=rinjt*rsol
	routjt=routjt*rsol
	x1sm=x1sm*rsol
	y1sm=y1sm*rsol
	z1sm=z1sm*rsol
	x2sm=x2sm*rsol
	y2sm=y2sm*rsol
	z2sm=z2sm*rsol
	r1sm=r1sm*rsol
	r2sm=r2sm*rsol
	x1fw=x1fw*rsol
	y1fw=y1fw*rsol
	z1fw=z1fw*rsol
	x2fw=x2fw*rsol
	y2fw=y2fw*rsol
	z2fw=z2fw*rsol
	r1fw=r1fw*rsol
	r2fw=r2fw*rsol
	rrg=rrg*rsol
	a1rg=a1rg*rsol
	a2rg=a2rg*rsol
	dr1rg=dr1rg*rsol
	dr2rg=dr2rg*rsol
        xrg=xrg*rsol
        yrg=yrg*rsol
        zrg=zrg*rsol
        rmdfx1=rmdfx1*rsol
	rmdfx2=rmdfx2*rsol
	rmdfx3=rmdfx3*rsol
	rmdfx4=rmdfx4*rsol
	rmdfy1=rmdfy1*rsol
	rmdfy2=rmdfy2*rsol
	rmdfz1=rmdfz1*rsol
	rmdfz2=rmdfz2*rsol
	rmdx1=rmdx1*rsol
	rmdx2=rmdx2*rsol	
	rmdy1=rmdy1*rsol
	rmdy2=rmdy2*rsol	
	rmdz1=rmdz1*rsol
	rmdz2=rmdz2*rsol	
	rmdz3=rmdz3*rsol
	rmdz4=rmdz4*rsol
c	conversion of velocities
	vrotst=1.d5*vrotst
	vxst=1.d5*vxst
        vyst=1.d5*vyst
        vzst=1.d5*vzst
	vxcp=1.d5*vxcp
        vycp=1.d5*vycp
        vzcp=1.d5*vzcp
        vrotcp=1.d5*vrotcp
        vtrben=1.d5*vtrben
	vxsp=1.d5*vxsp
        vysp=1.d5*vysp
        vzsp=1.d5*vzsp
        vrotsp=1.d5*vrotsp
	vtrbsp=1.d5*vtrbsp
        vxrg=1.d5*vxrg        
        vyrg=1.d5*vyrg
        vzrg=1.d5*vzrg
	vtrbrg=1.d5*vtrbrg
	vxdc=1.d5*vxdc
        vydc=1.d5*vydc
        vzdc=1.d5*vzdc
	vtrbdc=1.d5*vtrbdc
	vxnb=1.d5*vxnb
        vynb=1.d5*vynb
        vznb=1.d5*vznb
	vtrbnb=1.d5*vtrbnb
	vxuf=1.d5*vxuf
        vyuf=1.d5*vyuf
        vzuf=1.d5*vzuf
	vtrbuf=1.d5*vtrbuf
	vjt=1.d5*vjt
	vxjt=1.d5*vxjt
	vyjt=1.d5*vyjt
	vzjt=1.d5*vzjt
	vtrbjt=1.d5*vtrbjt
	vsh=1.d5*vsh
	vtrbsh=1.d5*vtrbsh
	vxsh=1.d5*vxsh
	vysh=1.d5*vysh
	vzsh=1.d5*vzsh
	v0=1.d5*v0
	v1sm=1.d5*v1sm
	v2sm=1.d5*v2sm
	vtrbsm=1.d5*vtrbsm
	vxsm=1.d5*vxsm
	vysm=1.d5*vysm
	vzsm=1.d5*vzsm
	v1fw=1.d5*v1fw
	v2fw=1.d5*v2fw
	vtrbfw=1.d5*vtrbfw
	vxfw=1.d5*vxfw
	vyfw=1.d5*vyfw
	vzfw=1.d5*vzfw
c	conversion of angles
	aspst=aspst/180.d0*pi
	dinc=dinc/180.d0*pi
	b1rg=b1rg/180.d0*pi
	b2rg=b2rg/180.d0*pi
	if(idisc.eq.1)then
	  adisc=adisc/180.d0*pi
	else
 	  adisc=adisc*rsol
	endif
	if(iufo.eq.1)then
	  aufo=aufo/180.d0*pi
	else
 	  aufo=aufo*rsol
	endif	
	ajet=ajet/180.d0*pi
	dd=dd*pc
	close(9)
c	check for most common errors in input	
        if(istar.gt.1.or.icomp.gt.1)then
          if(xcp.le.0.d0.or.qq.le.0.d0.or.emstar.le.0.d0)then
            write(*,*)' error: inconsistent star/companion input data'
            goto 590  
          endif
        endif
	if(densdc.gt.dcut1.and.edendc.ne.0.d0)then
          write(*,*)' error: wrong densdc or edendc, stop'
          goto 590
	endif
	if(densnb.gt.dcut1.and.edennb.ne.0.d0)then
          write(*,*)' error: wrong densnb or edennb, stop'
          goto 590
	endif
	if(densuf.gt.dcut1.and.edenuf.ne.0.d0)then
          write(*,*)' error: wrong densuf or edenuf, stop'
          goto 590
	endif	
	if(ishell.gt.1.and.denssh.gt.dcut1)then
          write(*,*)' error: wrong ishell or denssh, stop'
          goto 590
	endif
	if((rstar.le.0.d0.or.tstar.le.0.d0)
     &	.and.(istar.gt.0.or.icomp.eq.2))then
          write(*,*)' error: rstar-tstar, stop'
          goto 590
	endif
	if((rcp.le.0.d0.or.tempcp.le.0.d0)
     &	.and.(istar.gt.0.or.icomp.gt.0))then
          write(*,*)' error: rcp-tempcp, stop'
          goto 590
	endif	
	if(rindc.le.rdc.and.itdc.eq.2.and.idisc.gt.0)then
          write(*,*)' error: rdc, rindc, itdc, stop (T-singularity)'
          goto 590
	endif
	if(rinuf.le.ruf.and.ituf.eq.2.and.iufo.gt.0)then
          write(*,*)' error: ruf, rinuf, ituf, stop (T-singularity)'
          goto 590
	endif	
	if(rmdx2.le.rmdx1.or.rmdy2.le.rmdy1.or.rmdz2.le.rmdz1)then
          write(*,*)' error: rmd(xyz)(21)'
          goto 590
	endif	
	if(rmdfx2.le.rmdfx1.or.rmdfy2.le.rmdfy1.or.rmdfz2.le.rmdfz1)then
          write(*,*)' error: rmdf(xyz)(21)'
          goto 590
	endif		
	if(vrxsp**2+vrysp**2+vrzsp**2.le.0.d0)then
	  write(*,*)' error: unit vector'
	  goto 590
	endif		
	if((dlst+dlst2).gt.1.d0)then
	  write(*,*)' error: limb darkening'
	  goto 590
	endif		
	if((dlcp+dlcp2).gt.1.d0)then
	  write(*,*)' error: limb darkening'
	  goto 590
	endif		
	if(v1sm.le.0.d0.or.v2sm.le.0.d0)then
	  write(*,*)' error: v1sm or v2sm<=0'
	  goto 590
	endif  
	if(imiepf.gt.0.and.imie.lt.1)then
	  write(*,*)' error: Mie scattering'
	  goto 590
	endif		
c	reads monochromatic albedos	
	nalb1=0
	nalb2=0
	if(ialbst.eq.1.and.irrst.eq.1)then
	  call albedo(alb1x,alb1y,nalb1,1)
	  write(*,*)' albedo1',nalb1
	endif
	if(ialbcp.eq.1.and.irrcp.eq.1)then
	  call albedo(alb2x,alb2y,nalb2,2)
	  write(*,*)' albedo2',nalb2	  
	endif	
	if(albcp.lt.0.d0.or.albcp.gt.1.d0.or.
     &	htcp.lt.0.d0.or.htcp.gt.1.d0.or.
     &  htcpa.lt.0.d0.or.htcpa.gt.1.d0)then
          write(*,*)' error: alb**, ht**, stop (albedo-heat transport)'
          goto 590
	endif
	if(albst.lt.0.d0.or.albst.gt.1.d0.or.
     &	htst.lt.0.d0.or.htst.gt.1.d0.or.
     &  htsta.lt.0.d0.or.htsta.gt.1.d0)then
          write(*,*)' error: alb**, ht**, stop (albedo-heat transport)'
          goto 590
	endif	
c	reads opacities and phase functions for Mie scattering on dust
	if(imie.gt.0)then
	  call mie(opmiex,opmies,opmiea,nmie,pfmiex,pfang,pfmie
     &	  ,npfmie,imie,imiepf,ndust,dtlow,dthig,drmf)     
	endif
c		xsec reads extra gas opacity tables
c       xsecx - freq [Hz]
c       xsecy - cross-section [cm^2]
c	amixr -molecule abundance relative to H nuclei
c		chem reads chemistry table
c	pop -dlog10 of molecule population
	if(iopac.eq.1)then
	  call xsec(alam1,alamn,cutoff,xsecx,xsecy,txsec,nxsec,ntxsec
     &    ,amixr)
          call chem(pop,popt,popd,mtemp,mdens,melm)
	endif	
c		reading the input spectra of nontransparent objects
c	if lunt123>0 and converts them to central intensity
	call untsp(xstar1,star1,xstar2,star2,xstar3,star3
     &  ,wstar1,fstar1,tstar,jstar1,wstar2,fstar2,tempcp,jstar2
     &  ,xunt1,yunt1,xunt2,yunt2,xunt3,yunt3
     &  ,nstar1,nstar2,nstar3,nspec1,nspec2,tspec1,tspec2
     &  ,lunt1,lunt2,lunt3,dlst,dlst2,dlcp,dlcp2)
c	reads vertical density profile for nebula
	ndennb=0
	if(idennb.eq.1)then
	  write(*,*)' reading file wind_prof'
	  open(17,file='wind_prof',status='old')
	  read(17,*)
	  read(17,*)
	  i=1
30	  read(17,*,end=40,err=40)denxnb(i),pom,pom,denznb(i)
	  i=i+1
	  goto 30
40	  ndennb=i-1	  
	  write(*,'(a,i6)')' wind_prof:ndennb=',ndennb
	  close(17)	
	endif
c-----------------------------------------------------------------------
c	The shell (input) is defined in its body frozen cartesian 
c	coordinates (far,fat,faz). These may rotate relative to your 
c	steady cartesian laboratory frame (ar,at,az) -line of sight 
c	grid. The later has the same center of coordinates but its	
c	'z' axis is always pointed towards the observer so that 
c	the radial velocity of approaching object is positive. 
c	Radiative transfer is then solved in this line of sight grid.
c	Shell is first inclined around 'ar' by dinc degrees and 
c	then rotated around 'faz'-its rotational axis.
c	Nbod3 should be large enough so that a huge leaps in opt. depth
c	do not occure because of large velocity and opacity change 
c	between the grid points.
c	ftemp,fdens,fne,fvr,fvt,fvz,fvtrb,fdustd,fdustt are 
c	the following quantities in body frozen grid:
c	  ftemp -gas temperature
c	  fdens -gas density
c	  fne -electron number density 
c	  fvr -x velocity
c	  fvt -y velocity
c	  fvz -z velocity (radial velocity)
c	  fvtrb -turbulent velocity 
c	  fdustd -density of dust
c         fdustt -dust temperature
c	atemp,adens,ane,avr,avt,avz,avtrb,adustd,adustt are 
c	corresponding quantities in the line of sight grid.
	if (imodel.eq.1) then
          call smod1(nbodf1,nbodf2,nbodf3,nbodfa,nbodfb
     &   ,rmdfx1,rmdfx2,rmdfy1,rmdfy2,rmdfz1,rmdfz2,rmdfx3,rmdfx4
     &   ,gainfx,gainfy,gainfz
     &   ,rstar,tstar,emstar,xstar,ystar,zstar,vrotst,idifst,drotst,hst
     &   ,istar,vxst,vyst,vzst,dgst,ffst,irrst,albst,htst,htsta
     &   ,ispst,xspst,yspst,zspst,aspst,tspst
     &   ,icomp,dgcp,ffcp,qq,vrxcp,vrycp,vrzcp,vrotcp,rcp
     &   ,xcp,ycp,zcp,vxcp,vycp,vzcp,tempcp,irrcp,albcp,htcp,htcpa
     &   ,ienv,emen,qqen,aen,ffen,hen
     &   ,tempen,densen,aneen,vtrben,dstden,dstten     
     &   ,ispot,vrxsp,vrysp,vrzsp,vrotsp,rsp
     &   ,xsp,ysp,zsp,vxsp,vysp,vzsp,tempsp,denssp,anesp,vtrbsp
     &   ,dstdsp,dsttsp
     &   ,iring,rrg,emrg,b1rg,b2rg,a1rg,a2rg,dr1rg,dr2rg
     &   ,xrg,yrg,zrg,xpolrg,ypolrg,zpolrg,vxrg,vyrg,vzrg
     &   ,temprg,densrg,anerg,vtrbrg,itrg
     &   ,edenrg,dstdrg,ede2rg,dst2rg,dsttrg
     &   ,idisc,adisc,rindc,routdc,emdc,rdc
     &   ,xdc,ydc,zdc,xdisc,ydisc,zdisc,vxdc,vydc,vzdc
     &   ,tempdc,densdc,anedc,vtrbdc,edendc,itdc,etmpdc,dstddc,dsttdc
     &   ,inebl,aneb,rinnb,routnb,emnb,rnb
     &   ,hinvnb,tinvnb,hwindnb,ndennb,denxnb,denznb
     &   ,xneb,yneb,zneb,vxnb,vynb,vznb
     &   ,tempnb,densnb,anenb,vtrbnb,edennb,itnb,etmpnb,dstdnb,dsttnb
     &   ,x1sm,y1sm,z1sm,x2sm,y2sm,z2sm,v1sm,v2sm,r1sm,r2sm
     &   ,ism,vxsm,vysm,vzsm,xsm,ysm,zsm,psm
     &   ,tempsm,denssm,anesm,vtrbsm,edensm,dstdsm,dsttsm
     &   ,x1fw,y1fw,z1fw,x2fw,y2fw,z2fw,v1fw,v2fw,r1fw,r2fw
     &   ,iflow,vxfw,vyfw,vzfw,xfw,yfw,zfw,pfw
     &   ,tempfw,densfw,anefw,vtrbfw,edenfw,dstdfw,dsttfw
     &   ,iufo,aufo,rinuf,routuf,emuf,ruf
     &   ,xuf,yuf,zuf,xufo,yufo,zufo,vxuf,vyuf,vzuf
     &   ,tempuf,densuf,aneuf,vtrbuf,edenuf,ituf,etmpuf,dstduf,dsttuf
     &   ,ajet,rinjt,routjt,vjt,xjet,yjet,zjet,vxjt,vyjt,vzjt
     &   ,tempjt,densjt,anejt,vtrbjt,ijet,dstdjt,dsttjt
     &   ,rinsh,routsh,vsh,vxsh,vysh,vzsh
     &   ,tempsh,denssh,anesh,vtrbsh,ishell,evelsh,rcsh,dstdsh,dsttsh
     &   ,v0,temp0,dens0,ane0,dcut1,dcut2,dcut3,dcutn
     &   ,far,fat,faz,ftemp,fdens,fne,fvr,fvt,fvz,fvtrb,fdustd,fdustt)
	endif
c	be cautious imodel=2 does not support the gap in the coordinates
c       and use only nbodfb=0 before this option	
	if (imodel.eq.2.and.nbodfb.ne.0)then
           write(*,*)' error: imodel2 vs nbodfb'
           goto 590
        endif        
	if (imodel.eq.2) then
	  write(*,*)' reading file shellspec.mod'
	  open(10,file='shellspec.mod',status='old')
          call smod2(ndimf1,ndimf2,ndimf3,nbodf1,nbodf2,nbodf3
     &    ,far,fat,faz,ftemp,fdens,fne,fvr,fvt,fvz,fvtrb,fdustd,fdustt)
	  close(10)
        endif
	write(*,*)'nbodf1 nbodf2 nbodf3  (body frozen grid)'
	write(*,'(3i5)')nbodf1,nbodf2,nbodf3
c		definition of the line of sight grid
	call deflsg(ndim1,ndim2,ndim3,nbod1,nbod2,nboda,nbodb
     &  ,rmdx1,rmdx2,rmdy1,rmdy2,rmdz1,rmdz2,rmdz3,rmdz4
     &  ,gainx,gainy,gainz,ar,at,az)
	write(*,*)'nbod1 nbod2 nbod3  (line of sight grid)'
        write(*,'(3i5)')nbod1,nbod2,nbod3
c		reading the atomic data for spectral lines
	if(iline.eq.1)then
	  call lindat(iat,iion,wlab,elo,eup,glo,gr0,gs0,gw0,bij
     &    ,nline)
        else
          nline=0
        endif
c		reads the atomic data for all elements and ions
c		dyp(*)-name of the element
c		d(1,*)-atom weight
c		d(2,*)-solar abundance relative to hydrogen
c		d(3,*)-number of ions considered
c		xi(*,*)-first 8 ionization potentials
	call state0(dyp,d,xi)
c		extract data for our particular elements of sp.lines 
c		(hmc,nion,abund,zip) and change abundances: 
	call eldat(ichemc,iat,iion,dyp,d,xi,abhyd,abhel,wm
     &  ,hmc,nion,abund,zip,nline,ielnd,necod)
c		calculation of the electron number density
	if(ielnd.eq.1) call elnd(d,xi,necod,wm,abhyd,ftemp,fdens,fne
     &  ,nbodf1,nbodf2,nbodf3,denvac,dcut1,ane0)
	write(2,130)nbod1,nbod2,nbod3,nfreq
130	format(' nbod1=',i4,' nbod2=',i4,' nbod3=',i4,' nfreq=',i4)
	write(2,'(3(a,i3))')' output for:  ionu=',ionu
     &  ,' ior=',ior,' iot=',iot	
c		area(i,j)-area of a projected surface element
	call surf(ndim1,ndim2,nbod1,nbod2,ar,at,area)
c-----------------------------------------------------------------------
c		grand cycle through: angles, x,y, frequencies, and z
c	aint0-incident intesity from behind
	do 570 iang=1,nphase
          write(6,'(2(a,f7.2))')'i=',dinc*180.d0/pi
     &    ,'  alpha=',alpha(iang)*180.d0/pi
          write(2,'(2(a,f7.2))')'i=',dinc*180.d0/pi
     &    ,'  alpha=',alpha(iang)*180.d0/pi
c		cycle through parallel lines of sight (x,y)
	  opdepm=0.d0
	  do 430 ii=1,nbod1
	  do 420 jj=1,nbod2
c	    rotation of the object or ray casting:
c		linear interpolation
	    if(irotat.eq.0)then
	      call rot1d1(dinc,alpha(iang),dcut1,temp0,ane0,ii,jj
     &        ,vxst,vyst,vzst,vxstr,vystr,vzstr
     &        ,xcp,ycp,zcp,xcpr,ycpr,zcpr
     &        ,vxcp,vycp,vzcp,vxcpr,vycpr,vzcpr
     &        ,ndimf1,ndimf2,ndimf3,nbodf1,nbodf2,nbodf3
     &        ,ndim1,ndim2,ndim3,nbod1,nbod2,nbod3
     &        ,ar,at,az,far,fat,faz
     &        ,atemp,adustt,adustd,adens,ane,avr,avt,avz,avtrb
     &        ,ftemp,fdustt,fdustd,fdens,fne,fvr,fvt,fvz,fvtrb)
	    else
c		nearest neighbour approximation
	      call rot1d2(dinc,alpha(iang),dcut1,temp0,ane0,ii,jj
     &        ,vxst,vyst,vzst,vxstr,vystr,vzstr
     &        ,xcp,ycp,zcp,xcpr,ycpr,zcpr
     &        ,vxcp,vycp,vzcp,vxcpr,vycpr,vzcpr
     &        ,ndimf1,ndimf2,ndimf3,nbodf1,nbodf2,nbodf3
     &        ,ndim1,ndim2,ndim3,nbod1,nbod2,nbod3
     &        ,ar,at,az,far,fat,faz
     &        ,atemp,adustt,adustd,adens,ane,avr,avt,avz,avtrb
     &        ,ftemp,fdustt,fdustd,fdens,fne,fvr,fvt,fvz,fvtrb)
	    endif
	    if(ii.eq.ior.and.jj.eq.iot)then
	      iprint=1
	    else
	      iprint=0
	    endif		
c		check for an empty space
	    ivac1=0
	    aint0=0.d0	
	    do 140 kk=1,nbod3
	      if(adens(kk).gt.denvac)then
		ivac1=kk
		goto 150
	      endif
140	    continue	
150         ivacn=0
	    do 160 kk=nbod3,1,-1
	      if(adens(kk).gt.denvac)then
		ivacn=kk
		goto 170
	      endif	
160	    continue   
170	    if(ivac1.eq.0)then
	      do 180 nu=1,nfreq
	        aint(ii,jj,nu)=aint0
180	      continue	
	      if(ii.eq.ior.and.jj.eq.iot)then
	        write(2,*)' ii=',ii,'   jj=',jj,
     & 	        '   this ray goes through an empty space'
	      endif	
	      goto 420	
	    else
	      do 190 kk=ivac1,ivacn
	        temp(kk-ivac1+1)=atemp(kk)
	        ekonc(kk-ivac1+1)=ane(kk)
	        akonc(kk-ivac1+1)=adens(kk)/wm/hjed
	        dens(kk-ivac1+1)=adens(kk)
	        dustd(kk-ivac1+1)=adustd(kk)
	        dustt(kk-ivac1+1)=adustt(kk)
	        azn(kk-ivac1+1)=az(kk)
	        vr(kk-ivac1+1)=avr(kk)
	        vt(kk-ivac1+1)=avt(kk)
	        vz(kk-ivac1+1)=avz(kk)
	        vtrb(kk-ivac1+1)=avtrb(kk)
190	      continue
	      nivac=ivacn-ivac1+1
	    endif
c
c		you can possibly speed up if you seach for 
c		untransparent object before do 190 cycle??
c           search for the last untransparent point along the ray
c	    and calculation of the limb darkening factor (iunt,darkl)	
	    call bcond1(iunt,darkl,cdelta,vdif,nivac,dens
     &	    ,dcut1,dcut2,dcut3,dcutn,vr,vt,vz
     &      ,ar(ii),at(jj),azn,dinc,alpha(iang)
     &      ,istar,rstar,vxstr,vystr,vzstr,dlst,dlst2,irrst
     &      ,icomp,rcp,xcpr,ycpr,zcpr,vxcpr,vycpr,vzcpr
     &      ,dlcp,dlcp2,irrcp,xcp,qq,iprint)
c
c		hneg - H- hydrogen number density
c		h2 - H2 number density 
c	    	hi -neutral hydrogen number density
c	    	hei -neutral helium number density
c	    	hipf -HI partition function	
c		akonc -number density of all atoms
	    call hihei(ipart,nivac,temp,ekonc,akonc
     &	    ,hipf,hi,hneg,h2,hei,abhyd,abhel)     
c		interpolates amixf from the pop table
            if(iopac.eq.1)then
              call abnd(pop,popt,popd,mtemp,mdens,melm
     &        ,temp,dens,nivac,ndim3,iopac,amixr,amixf)
            endif
c           ophyd calculates at alam1,alamn:     
c	    	ophbf -HI bound-free opacity
c	    	ophff -HI free-free opacity
c	    	ophrs -HI Rayleigh scattering
c		ophn  -H(-) opacity
            call ophyd(nivac,temp,ekonc,akonc,hi,hneg,abhyd
     &      ,ophbf1,ophff1,ophrs1,ophn1,hipf,alam1)
            call ophyd(nivac,temp,ekonc,akonc,hi,hneg,abhyd
     &      ,ophbf2,ophff2,ophrs2,ophn2,hipf,alamn)
	    if (iprint.eq.1)then        
              write(2,*)' iunt=',iunt
              write(2,*)' ivac1=',ivac1,'ivacn=',ivacn,' nivac=',nivac
	      call tlac1(ndim3,nivac,azn,vr,vt,vz
     &        ,temp,ekonc,akonc,dens,dustd,dustt,hi,hneg,h2,hei,hipf)
	    endif
c
c		calculation of level population and damping for 
c	    the sp. lines (0,nline)
c	    partition functions of the element at different depth
c	    stavs(i,j) :  i-depth point, j-ion
	    if(iline.eq.1)then
	      do 230 ilin=1,nline
	        if(ipart.eq.1)then
	          call pfdwor(ndim3,nivac,mion,nion(ilin),iat(ilin)
     &            ,temp,ekonc,stavs)
                else
                  call pfirw(ndim3,nivac,mion,nion(ilin),sta,temp,stavs)
                endif
	        do 200 ll=1,mion-1
	          zipp(ll)=zip(ll,ilin)
200	        continue
c		rr(i,j)- popul of the j-th ion/total element population
c		at i-th depth
	        call zastup(temp,ekonc,zipp,stavs,rr,nivac,nion(ilin))
	        do 210 kk=1,nivac
	          popul(kk,ilin)=bolt(temp(kk),stavs(kk,iion(ilin))
     &            ,glo(ilin),elo(ilin),1)
     &            *rr(kk,iion(ilin))*abund(ilin)*abhyd*akonc(kk)
210	        continue
c		calculates damping constants
c	    	aa-combined damping constant
c	    	dop-Doppler halfwidth	
                do 220 kk=1,nivac
	          call utlm(wlab(ilin),eup(ilin),iat(ilin),hmc(ilin)
     &            ,iion(ilin),zipp(iion(ilin)),hi(kk),hei(kk)
     &            ,ekonc(kk),temp(kk),vtrb(kk),gr(ilin),gr0(ilin)
     &            ,gs(ilin),gs0(ilin),gw(ilin),gw0(ilin)  
     &            ,aa(kk,ilin),dop(kk,ilin))
220             continue
c		prints various quantities	    
	        if (iprint.eq.1)then        
	          write(2,*)' ilin=',ilin	
	          call tlac2(ndim3,nivac,mion
     &            ,nion(ilin),iion(ilin),azn,zipp,eup(ilin)
     &            ,temp,stavs,rr,gr(ilin),gs(ilin),gw(ilin))
	        endif
230	      continue
	    endif
c		frequency cycle
	    do 410 nu=1,nfreq
	      if(nu.eq.ionu.and.ii.eq.ior.and.jj.eq.iot)then
	        iprint=1
	      else
	        iprint=0
	      endif		
c		This interpolates continuum opacity to alam(nu)
c		it is 2x faster then calculating continuum opacity
c               directly at alam(nu)
c               It presumes that <alam1,alamn> is short enough.??
	      call medop(ndim3,nivac,alam1,alamn,alam(nu)
     &        ,ophbf,ophbf1,ophbf2,ophff,ophff1,ophff2
     &        ,ophrs,ophrs1,ophrs2,ophn,ophn1,ophn2)
c              This calculates ophbf,ophff,ophrs at each alam(nu):
c	      call ophyd(nivac,temp,ekonc,akonc,hi,hneg,abhyd
c     &       ,ophbf,ophff,ophrs,ophn,hipf,alam(nu))
c		wlab-laboratory lambda of the line center in [A]
c		alam(nu)-solving lambda in [A]
c		freq-frequency of alam(nu)
	      freq=clight/alam(nu)*1.d8
c             	interpolates dust opacity to freq (opmis,opmia)
	      if(imie.gt.0)then
		do idst=1,ndust
		  do ij=1,nmie(idst)
		    pmiex(ij)=opmiex(ij,idst)
		    pmies(ij)=opmies(ij,idst)
		    pmiea(ij)=opmiea(ij,idst)
		  enddo  
	          call intrp(pmiex,pmies,nmie(idst),freq,opmis(idst))
	          call intrp(pmiex,pmiea,nmie(idst),freq,opmia(idst))
	        enddo  
	      else
		do idst=1,ndust
	          opmis(idst)=0.d0
	          opmia(idst)=0.d0
	        enddo  
	      endif  
c             	interpolates dust phase function to freq	      
	      if(imiepf.eq.1)then
	        call intrp2(pfmiex,pfmie,npfmie,freq,pfmief)
	      else
	        do ipfang=1,npfang
		  pfmief(ipfang)=0.d0
	        enddo
	      endif  
c
c		calculation of aintb- boundary condition for intensity
c		behind the last untransparent object along the ray
	      call bcond2(iunt,dens,temp,vz,vdif,freq
     &        ,dcut1,dcut2,dcut3,dcutn,darkl,cdelta
     &        ,lunt1,nstar1,xstar1,star1,nspec1,tspec1,alb1x,alb1y,nalb1
     &        ,tstar,dlst,dlst2,irrst,albst,wstar1,fstar1,jstar1     
     &        ,lunt2,nstar2,xstar2,star2,nspec2,tspec2,alb2x,alb2y,nalb2
     &        ,tempcp,dlcp,dlcp2,irrcp,albcp,wstar2,fstar2,jstar2
     &        ,lunt3,nstar3,xstar3,star3,aint0,aintb)
              if(iprint.eq.1)then
                write(2,*)' iunt=',iunt,' aintb=',aintb
              endif
c
c	        this is the 1D radiative trasfer routine
	      call rte(nivac,alam(nu),aintb,aint(ii,jj,nu),opdep
     &        ,iunt,wlab,bij,aa,dop,popul,nline
     &        ,istar,tstar,rstar,wstar1,fstar1,jstar1,lunt1
     &        ,vxstr,vystr,vzstr
     &        ,icomp,tempcp,rcp,wstar2,fstar2,jstar2,lunt2
     &        ,vxcpr,vycpr,vzcpr,xcpr,ycpr,zcpr
     &        ,xsecx,xsecy,txsec,nxsec,ntxsec,amixf,amixr,iopac
     &        ,ophbf,ophff,ophrs,ophn,ithom,irayl,ihyd
     &        ,opmis,opmia,dtlow,dthig,drmf,ndust,imie
     &        ,imiepf,pfmief,pfang
     &        ,temp,dens,ekonc,akonc,dustd,dustt,azn,vr,vt,vz
     &        ,ar(ii),at(jj),denvac,iprint,eps)
              if(opdep.gt.opdepm)opdepm=opdep
410	    continue
420	  continue
430	  continue
	  write(2,*)' max optical depth for this view angle'
	  write(2,'(a,es10.2)')' is: ',opdepm	  
c
c          plocha=0.d0
          write(forts,'(A8,I0.3)')'2Dimage_',iang
          open(20+iang,file=forts,status='unknown')
	  do 450 i=1,nbod1
            write(20+iang,*)
	    do 440 j=1,nbod2
              write(20+iang,455)ar(i),at(j),aint(i,j,ionu)
c	      do 435 k=nbod3,1,-1
c	        if (adens(k).gt.dcut1)then
c		  plocha=area(i,j)+plocha
c		  goto 440
c		endif
c435	      continue
440         continue
450       continue
          close(20+iang)
455       format(2e12.4,e14.5e3)
c	  if(rstar.gt.0.d0)then
c	    ratio=plocha/pi/rstar/rstar
c            write(2,*)' star disk',pi*rstar**2
c            write(2,*)' sum_ij untranarea_ij/star disk',ratio            
c	  endif
	  do nu=1,nfreq
            flux(nu,iang)=0.d0
	    do i=1,nbod1
	    do j=1,nbod2
              flux(nu,iang)=aint(i,j,nu)*area(i,j)/dd/dd+flux(nu,iang)
	    enddo
	    enddo
	  enddo
	  der=(flux(nfreq,iang)-flux(1,iang))/(alam(nfreq)-alam(1))
	  do nu=1,nfreq
c	    flux in [erg/cm^2/s/Hz], fluxl in [erg/cm^2/s/cm], 
c	    fluxn -normalized flux 
c	    fluxs -shifted fluxn	
	    cont=flux(1,iang)+der*(alam(nu)-alam(1))
	    fluxl=flux(nu,iang)*clight/alam(nu)/alam(nu)*1.d16	
	    fluxn(nu,iang)=flux(nu,iang)/cont
	    fluxs=fluxn(nu,iang)-dble(iang-1)*offset
	    if(nline.ge.1)then
	      refw=wlab(1)	
	    else
              refw=alam1 
	    endif
	    dvel=(alam(nu)-refw)/refw*clight*1.d-5
	    write(4,510)alam(nu),dvel,flux(nu,iang),fluxl,fluxn(nu,iang)
     &	    ,fluxs
c	    write(100+iang,'(f10.3,f10.1,5e14.7)')alam(nu),dvel
c     &      ,flux(nu,iang),fluxl,fluxn(nu,iang),fluxs
	    write(11,'(f7.3,es11.3,f10.4,es12.4,es18.9e3)')alpha(iang)/
     &      2.d0/pi,dvel,-2.5d0*dlog10(fluxl),alam(nu),flux(nu,iang)
          enddo
	  write(11,*)
	  write(4,*)
	  write(2,*)
510     format(f12.3,1x,f10.1,5es14.5e3)	  
c	  end of rotation cycle
570	continue
c	for Slavek fluxn was defined a field
c	do nu=1,nfreq
c	  write(18,520)(idint(fluxn(nu,iang)*1.d4),iang=1,nphase)
c	  write(19,'(f12.4)')alam(nu)/1.d1
c520       format(300i6)	  
c	enddo
	close(2)
c	close(3)
	close(4)
	close(11)
c	close(18)
c	close(19)
590	continue
c	end
	end subroutine shellspec
c-----------------------------------------------------------------------
	subroutine rte(nbod,alam,aintb,aint,opdep
     &  ,iunt,wlab,bij,aa,dop,popul,nline
     &  ,istar,tstar,rstar,xstar1,star1,nstar1,lunt1
     &  ,vxstr,vystr,vzstr
     &  ,icomp,tempcp,rcp,xstar2,star2,nstar2,lunt2
     &  ,vxcpr,vycpr,vzcpr,xcpr,ycpr,zcpr
     &  ,xsecx,xsecy,txsec,nxsec,ntxsec,amixf,amixr,iopac
     &  ,ophbf,ophff,ophrs,ophn,ithom,irayl,ihyd
     &  ,opmis,opmia,dtlow,dthig,drmf,ndust,imie
     &  ,imiepf,pfmief,pfang     
     &  ,temp,dens,ekonc,akonc,dustd,dustt,az,vr,vt,vz
     &  ,ari,ati,denvac,iprint,eps)
c	to solve the radiative tranfer along the line of sight
c	at the wavelength -alam
c	input:
c	  nbod -number of depth points
c	  alam -wavelength [A]
c	  aintb -boundary condition for intensity 
c	  iunt -last untransparent point
c	  wlab -laboratory lambda of the line center [A]
c	  bij - Einstein coefficient B_lu per unit solid angle
c	  aa - combined damping constant
c	  dop - Doppler halfwidth
c	  popul - lower level population of spectral line
c	  nline - number of spectral lines
c	  istar,icomp -on/off switch of the scattered light
c	  tstar,rstar -temperature,radius of the spherical 
c			central star
c	  xstar1,star1,nstar1,lunt1 -spectrum of the central star
c	  vxstr,vystr,vzstr -velocity of the central star
c	  tempcp,rcp -temperature,radius of the spherical 
c			secondary star
c	  xstar2,star2,nstar2,lunt2 -spectrum of the second star
c	  vxcpr,vycpr,vzcpr -velocity of the second star
c	  xcpr,ycpr,zcpr -position of the second star
c	  xsecx,xsecy,txsec,nxsec,ntxsec -extra gas opacity table	
c	  amixf -molecule number density
c	  amixr -constant molecule abundance (mixing ratio)
c               (relative to H nuclei)
c	  iopac -extra gas opacity table switch
c	  ophbf,ophff,ophrs - opacity hydrogen 
c			(bound-free, free-free, Rayleigh scattering)
c         ophn -opacity of H- (bound-fee, free-free)
c	  opmis,opmia - dust opacity (scattering, absorption)
c		    per 1g of dust material for different species
c         dtlow,dthig -temperature interval for different dust species
c	  drmf -relative mas fraction for different dust species
c	  ndust -number of different dust species
c	  imie,imiepf -switch for Mie scattering
c	  pfmief -phase function for dust, function of angles 
c	  pfang -cos(angle) for phase function of dust
c	  ithom,irayl -switch for Thompson & Rayleigh scattering
c         ihyd -switch for hydrogen bf, ff opacity
c	  temp,dens - temperature, density
c	  ekonc,akonc - electron and atom number densities
c	  dustd,dustt - dust density and temperature 
c	  vr,vt,vz - x,y,z velocity along the ray
c         az - z coordinate along the ray
c	  ari,ati - x,y coordinates defining the line 
c			of sight along z
c	  denvac - limiting density of vacuum
c	  iprint - signal to print
c	  eps - In LTE eps=1. ( S=eps*B+(1-eps)*J ) 
c	output: 
c	  aint-emerging intensity [erg/cm^2/s/Hz/rad]
c	  opdep -optical depth along the ray after 
c                 	the possible non-transparent object
c
c       temp, ane -are assumed to have reasonable values all along
c       the beam. An empty space can be identified as dens<denvac.
c	Integration starts behind the last untransparent object.
	implicit double precision (a-h,o-z)
        include 'param.inc'
	parameter(modp=2000)
c	modp -maximum optical dept points to break a single huge step	
	dimension aa(ndim,mline),dop(ndim,mline),popul(ndim,mline)
        dimension flab(mline),wlab(mline),bij(mline),bij2(mline)
        dimension ophbf(ndim),ophff(ndim),ophrs(ndim),ophn(ndim)
        dimension temp(ndim),dens(ndim),ekonc(ndim),akonc(ndim)
        dimension dustd(ndim),dustt(ndim),amixf(ndim)
        dimension vr(ndim),vt(ndim),vz(ndim),az(ndim)
        dimension bnu(ndim),bnud(ndim),ejnu1(ndim),ejnu2(ndim)
        dimension sf2(ndim),opac2(ndim),emis2(ndim),opdept(ndim)
	dimension dtlow(mspecx),dthig(mspecx),drmf(mspecx)
	dimension opmis(mspecx),opmia(mspecx)
        dimension pfang(npfang),pfmief(npfang)
        dimension xstar1(mstarx),star1(mstarx)
        dimension xstar2(mstarx),star2(mstarx)
	dimension xsecx(mstarx),xsecy(mstarx,mspecx)
        dimension txsec(mspecx)
        dimension sf2i(modp),opdi(modp),cfi(modp)
c	CONSTANTS AND UNITS FROM NIST(2002)
	bol=1.3806503d-16
	clight=2.99792458d10
	hjed=1.66053873d-24
	plank=6.62606876d-27
	pi=3.1415926535897931d0
C	1EV=X ERG, X CM^-1; 1ERG= X CM^-1 
	EVERG=1.602176462D-12
	EVCM=8.06554477D3
	ERGCM=5.03411762D15
c		flab -frequency [1/s] of wlab
c		flabs-frequency of the line center in comoving frame
c		alam-solving lambda in [A]
c		freq-frequency of alam
	freq=clight/alam*1.D8
	if(nline.gt.0)then
	  do 10 ll=1,nline
	    flab(ll)=clight/wlab(ll)*1.D8
	    bij2(ll)=bij(ll)*plank*flab(ll)/dsqrt(pi)
10	  continue
	endif
	if (nbod-iunt.lt.2)then
c         aint-is set to boundary condition for intensity
	  aint=aintb
	  opdep=0.d0
          goto 100
        endif  
c		popul-population of the lower level
c		opac-opacity
c		emis-emisivity
c		sf-source function
        if (iprint.eq.1)then
	  write(2,*)' lambda=',alam
	  write(2,'(a,a,a)')' id  '      
     &    ,'ophbf     ophff     opth      ophrs     ophn      '
     &    ,'opmieb    opmiej    oplin     opmol'
	endif
c		initialize the first value of jj for hunt
	jj1=1
	jj2=1
c		BEGINNING OF THE DEPTH CYCLE ---------------------------
	do 60 id1=iunt+1,nbod
	  if(dens(id1).lt.denvac)then
c	    icheck=0
	    sf2(id1)=0.d0
	    emis2(id1)=0.d0
	    opac2(id1)=0.d0
            if (iprint.eq.1)then
	      write(2,'(i3,a)')id1,' empty space-skipped'
	    endif
	    goto 60
	  endif
c		Line opacity (takes into account the velocity field)	  
	  oplin=0.d0
	  if(nline.gt.0)then
	    do 20 ll=1,nline
	      flabs=vz(id1)/clight*flab(ll)+flab(ll)
	      dfreqd=(freq-flabs)/dop(id1,ll)
c	  	\INT(VOIGT F.)dFREQ 
c		HAS NORMALIZATION= SQRT(PI)*DOP.W.[1/S]
	      vprof=voigt(dfreqd,aa(id1,ll))
	      oplin=(1.d0-dexp(-4.7992375d-11*flab(ll)/temp(id1)))
     &	      *bij2(ll)*popul(id1,ll)*vprof/dop(id1,ll)+oplin
20	    continue
	  endif
c		extra true absorption gas opacity (xsection) from table
c               (takes into account the velocity field)
	  if(iopac.eq.1.and.nxsec.gt.1)then
	    flabs=-vz(id1)/clight*freq+freq
	    call int2d(xsecx,txsec,xsecy,nxsec,ntxsec,flabs,temp(id1)
     &      ,xsecs)
            opmol=amixf(id1)*xsecs
c            opmol=akonc(id1)*amixr*xsecs
	  else
	    opmol=0.d0
	  endif
c                Hydrogen bf, ff, H- opacity (ignores velocity field)
	  if(ihyd.ne.1)then
	    ophbf(id1)=0.d0
	    ophff(id1)=0.d0
	    ophn(id1)=0.d0
	  endif  
c		Thomson scattering opacity (no velocity dependence)
          if(ithom.eq.1)then
	    opth=ekonc(id1)*6.65d-25
	  else
	    opth=0.d0
	  endif
c               dust opacity (Mie scattering+absorption)
c               (ignores velocity field)
	  opmiej=0.d0
	  opmieb=0.d0
          if(imie.gt.0)then
	    do idst=1,ndust
	      if(dustt(id1).gt.dtlow(idst).and.
     &	      dustt(id1).le.dthig(idst))then
                opmiej=dustd(id1)*drmf(idst)*opmis(idst)+opmiej
                opmieb=dustd(id1)*drmf(idst)*opmia(idst)+opmieb
              endif
            enddo  
          endif
c		Rayleigh scattering on HI opacity
c               (ignores velocity field)
          if(irayl.ne.1)ophrs(id1)=0.d0
c		total opacity
	  opac2(id1)=oplin+opth+ophbf(id1)+ophff(id1)+ophrs(id1)
	  opac2(id1)=opac2(id1)+ophn(id1)+opmiej+opmieb+opmol
	  if(opac2(id1).le.0.d0)then
	    write(*,*)'error: zero opacity'
	    write(*,*)'id1   dens    dustd   dustt    opac'
	    write(*,30)id1,dens(id1),dustd(id1),dustt(id1),opac2(id1)
30	    format(i4,4es9.1)
	    stop
	  endif  
	  bnu(id1)=planck(freq,temp(id1),1)
	  bnud(id1)=planck(freq,dustt(id1),1)
c		HI, H-  bound-free and free-fee emissivity
	  emhibf=ophbf(id1)*bnu(id1)
	  emhiff=ophff(id1)*bnu(id1)
	  emhn=ophn(id1)*bnu(id1)
c		scattering from stars
c		ejnu1,2 - mean intensity from the star and companion
c		emth,emra - Thompson and Rayleigh scattering emissivity
c		emmij - Mie scattering emissivity
c               emmib - Mie absorption emissivity
          if(ithom.eq.1.or.irayl.eq.1.or.imie.eq.1)then
	    if(istar.gt.0)then	
     	      call jnu(rstar,tstar,ari,ati,az(id1)
     &	      ,vr(id1),vt(id1),vz(id1),freq
     &        ,xstar1,star1,nstar1,lunt1
     &        ,vxstr,vystr,vzstr,jj1,ejnu1(id1),iprint,0.d0,0.d0,0.d0)
	      er2=ari*ari+ati*ati+az(id1)*az(id1)
	      uhcos=az(id1)/dsqrt(er2)	
	      if(er2.gt.(rstar*rstar*9.d0))then
c		dipol phase function scattering
	        dippf1=0.75d0*(1.d0+uhcos**2)
c		dust phase function		
		if(imiepf.eq.1)then
		  call locate(pfang,npfang,uhcos,iuh)
		  deruh=(pfmief(iuh+1)-pfmief(iuh))
		  deruh=deruh/(pfang(iuh+1)-pfang(iuh))
		  dstpf1=deruh*(uhcos-pfang(iuh))+pfmief(iuh)
		else
		  dstpf1=1.d0
		endif
	      else
	        dippf1=1.d0
	        dstpf1=1.d0
	      endif    
	      emth1=dippf1*ejnu1(id1)*opth
	      emra1=dippf1*ejnu1(id1)*ophrs(id1)
	      if(imie.eq.1)then	
	        emmij1=dstpf1*opmiej*ejnu1(id1)
	      else
	        emmij1=0.d0
	      endif  
     	    else
     	      ejnu1(id1)=0.d0
     	      emth1=0.d0
	      emra1=0.d0
	      emmij1=0.d0
     	    endif
     	    if(icomp.gt.0)then  	
       	      call jnu(rcp,tempcp,ari,ati,az(id1)
     &        ,vr(id1),vt(id1),vz(id1),freq
     &        ,xstar2,star2,nstar2,lunt2
     &        ,vxcpr,vycpr,vzcpr,jj2,ejnu2(id1),iprint,xcpr,ycpr,zcpr)
	      er2=(ari-xcpr)**2+(ati-ycpr)**2+(az(id1)-zcpr)**2
	      uhcos=(az(id1)-zcpr)/dsqrt(er2)
	      if(er2.gt.(rcp*rcp*9.d0))then
c		dipol phase function scattering
	        dippf2=0.75d0*(1.d0+uhcos**2)
c               dust phase function   
		if(imiepf.eq.1)then
                  call locate(pfang,npfang,uhcos,iuh)    
                  deruh=(pfmief(iuh+1)-pfmief(iuh))      
                  deruh=deruh/(pfang(iuh+1)-pfang(iuh))		
                  dstpf2=deruh*(uhcos-pfang(iuh))+pfmief(iuh)
                else
                  dstpf2=1.d0
                endif  
	      else
	        dippf2=1.d0
	        dstpf2=1.d0
	      endif    
	      emth2=dippf2*ejnu2(id1)*opth
	      emra2=dippf2*ejnu2(id1)*ophrs(id1)
	      if(imie.eq.1)then
	        emmij2=dstpf2*opmiej*ejnu2(id1)
	      else
	        emmij2=0.d0
	      endif  
     	    else
     	      ejnu2(id1)=0.d0
     	      emth2=0.d0
	      emra2=0.d0
	      emmij2=0.d0
     	    endif     
	  else
	    ejnu1(id1)=0.d0
	    ejnu2(id1)=0.d0
     	    emth1=0.d0
	    emra1=0.d0
	    emth2=0.d0
	    emra2=0.d0
	    emmij1=0.d0
	    emmij2=0.d0	
	  endif
	  emth=emth1+emth2
	  emra=emra1+emra2
	  emmij=emmij1+emmij2
c		line emissivity
	  emlinb=eps*oplin*bnu(id1)
	  emlinj=(1.d0-eps)*oplin*(ejnu1(id1)+ejnu2(id1))
c		Mie emissivity from dust (thermal)
	  emmib=opmieb*bnud(id1)
c		test for disk	  
	  if(imie.eq.3)emmij=opmiej*bnud(id1)
c		molecule emissivity
	  emmol=opmol*bnu(id1)	  
c		total emissivity
	  emis2(id1)=emlinb+emlinj+emth+emhibf+emhiff+emra+emhn
	  emis2(id1)=emis2(id1)+emmib+emmij+emmol
c		total source function
	  sf2(id1)=emis2(id1)/opac2(id1)
	  if (iprint.eq.1)then
	    write(2,'(i3,9es10.2)')id1
     &      ,ophbf(id1),ophff(id1),opth,ophrs(id1),ophn(id1)
     &      ,opmieb,opmiej,oplin,opmol
	  endif
60      continue
C               END OF THE DEPTH CYCLE----------------------------------
c		solver1: for opt thin medium, integrates aint and opdep
c		opdep- optical depth as measured from behind the shell
c		      or from an untransparent object
c	aint-is set to boundary condition for intensity
c	aint=aintb
c        if (iprint.eq.1)then
c	  write(2,'(a)')' id    opdep      aint '  
c	endif
c	opdep=0.d0
c	icheck=0
c        do 65 id1=iunt+1,nbod
c	  if(dens(id1).lt.denvac)then
c	    icheck=0
c	    goto 65
c	  endif
c	  if (icheck.gt.0)then
c	    opac12=(opac1+opac2(id1))*0.5d0
c	    sf12=(sf1+sf2(id1))*0.5d0
c	    dz12=(az(id1)-az(id1-1))
c	    alph=1.d0/opac12/dz12
c	    aint=(aint*(alph-0.5d0)+sf12)/(alph+0.5d0)
c	    opdep=opac12*dz12+opdep
c            if (iprint.eq.1)then
c              write(2,70)id1,opdep,aint
c            endif
c	  endif
c	  opac1=opac2(id1)
c	  sf1=sf2(id1)
c	  icheck=icheck+1
c65	continue
c
c               solver2: formal solution, integrates aint and opdep
c		opdept- optical depth as measured from the observer to
c        behind the shell or up to an untransparent object
        if (iprint.eq.1)then
	  write(2,'(a)')' id   opdep     contrib.f   aint '  
	endif
c	calculate op.depth
	opdept(nbod)=0.d0
        do id1=nbod-1,iunt+1,-1
          dzz=az(id1+1)-az(id1) 
          opdepd=(opac2(id1+1)+opac2(id1))*0.5d0*dzz
          opdept(id1)=opdepd+opdept(id1+1)
        enddo
	aint=0.d0
        do id1=nbod-1,iunt+1,-1
          opdepd=(opdept(id1)-opdept(id1+1))
c	  break a huge leap in opt. depth into nodp steps
	  opdlim=10.d0
	  opslim=0.3d0
	  if(opdept(id1+1).lt.opdlim.and.opdepd.gt.opslim)then
  	    nodp=opdepd/opslim+1
	    if(nodp.gt.modp)then
	      write(6,*)'stop nodp>modp in rte',nodp,modp
	      write(6,*)'steep/high density gradient on the edge'
	      stop
	    endif  
	    opstep=opdepd/dble(nodp)
	    dersf=(sf2(id1)-sf2(id1+1))/opdepd
	    do id2=1,nodp+1
              sf2i(id2)=dersf*opstep*dble(id2-1)+sf2(id1+1)
              opdi(id2)=opstep*dble(id2-1)+opdept(id1+1)
	      cfi(id2)=sf2i(id2)*dexp(-opdi(id2))
	    enddo
	    cf12=0.d0
	    do id2=1,nodp
	      cf12=cf12+cfi(id2)+cfi(id2+1)
	    enddo 
	    cf12=cf12/2.d0*opstep
	  else
            cf1=sf2(id1+1)*dexp(-opdept(id1+1))
            cf2=sf2(id1)*dexp(-opdept(id1))
            cf12=(cf1+cf2)*0.5d0*opdepd
          endif  
          aint=cf12+aint
          if (iprint.eq.1)then
            write(2,70)id1,opdept(id1),cf12,aint
          endif
        enddo
	aintbt=aintb*dexp(-opdept(iunt+1))
	aint=aint+aintbt
	opdep=opdept(iunt+1)
        if(iprint.eq.1)then
          write(2,*)'iunt+1  opdep   aintbt    aint'
          write(2,70)iunt+1,opdep,aintbt,aint
        endif  
70      format(i3,3es11.3)        
c
        if (iprint.eq.1)then
	  write(2,'(a,a,a)')' id    opac2     emis2     bnu       bnud'
     &	  ,'      ejnu1     ejnu2     sf2       az        vz'
	  do id1=iunt+1,nbod
	    if(dens(id1).gt.denvac)then
	      write(2,'(i3,10es10.2)')id1,opac2(id1),emis2(id1)
     &	      ,bnu(id1),bnud(id1),ejnu1(id1),ejnu2(id1),sf2(id1)
     &        ,az(id1),vz(id1)
	    endif
          enddo
	endif
100	continue
	return
	end
C-----------------------------------------------------------------------
	DOUBLE PRECISION FUNCTION VOIGT(X,Y)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C      IMPLICIT REAL*8 (A-H,O-Z)
C      REAL*8 VOIGT
C      REAL*4 X,Y
c	taken from the old Synspec code:
c Hubeny I., Lanz T., Jeffery C.S., 1994, in Newsletter on Analysis
c of Astronomical spectra No.20, ed. C.S. Jeffery (CCP7; St. Andrews:
c St. Andrews Univ.), 30
C   	HUMLICEK VOIGT FUNCTION ALGORITHM  (LOWER PRECISION VERSION)
        DIMENSION T(6),C(6),S(6)
	DATA T/.314240376,.947788391,1.59768264,2.27950708,
     &  3.02063703,3.8897249/, 
     &  C/1.01172805,-.75197147,1.2557727E-2,
     &  1.00220082E-2,-2.42068135E-4,5.00848061E-7/,
     &  S/1.393237,.231152406,-.155351466,6.21836624E-3,
     &  9.19082986E-5,-6.27525958E-7/
      WR=0.
      Y1=Y+1.5
      Y2=Y1*Y1
      DO 3 I=1,6
      R=X-T(I)
      D=1./(R*R+Y2)
      D1=Y1*D
      D2=R*D
      R=X+T(I)
      D=1./(R*R+Y2)
      D3=Y1*D
      D4=R*D
      WR=WR+C(I)*(D1+D3)-S(I)*(D2-D4)
    3 CONTINUE
      VOIGT=DBLE(WR)
      RETURN
      END
C-----------------------------------------------------------------------
	DOUBLE PRECISION FUNCTION BOLT(TEMP,G1,G2,POT,IRIAD)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C	      TEMP -TEMPERATURE [KELVIN]			       C
C	      G1   -STATISTICAL WEIGHT OF THE LOWER LEVEL-m	       C
C	      G2   -STATISTICAL WEIGHT OF THE UPPER LEVEL-n	       C
C	      POT  -ENERGY DIFFERENCE (UPPER-LOWER LEVEL) [EV], POT>0  C
C	      OUTPUT -POPULATION RATIO N(n)/N(m) 		       C
C---------------------OR IF:            -------------------------------C
C	      TEMP -TEMPERATURE [KELVIN]			       C
C	      G2   -STATISTICAL WEIGHT OF THE LEVEL-n		       C
C	      G1   -PARTITION FUNCTION OF THE ION                      C
C	      POT  -EXCITATION POTENCIAL OF THE n-TH LEVEL [EV]	       C
C		   (WITH RESPECT TO GROUND STATE), POT>0	       C
C	      OUTPUT -RATIO N(n)/N, (n-TH LEVEL POPULATION TO	       C
C		    TO THE ION POPULATION			       C
C-----------------------OR IF------------------------------------------C
C	      IRIAD=1  POT IS IN [1/CM]         		       C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	EVCM=8.06554477D3
	IF(IRIAD.EQ.1)THEN
	  BOLT=G2/(G1*DEXP(2.302585D0*5039.77D0/TEMP*POT/EVCM))
	ELSE
	  BOLT=G2/(G1*DEXP(2.302585D0*5039.77D0/TEMP*POT))
	ENDIF
	RETURN
	END
C-----------------------------------------------------------------------
	SUBROUTINE UTLM(DL0,EP,IAT,AM,IION,CHI,HI,HEI,EKONC,TEMP,VT
     &  ,GR,GR0,GS,GS0,GW,GW0,A,DOP)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C	     DL0   -LINE CENTER [ANGSTROM]			       C
C	     EP    -EXCIT.POTENTIAL OF THE UPPER LEVEL [1/CM]	       C
C	     IAT   -ATOMIC NUMBER 				       C
C	     AM    -MASS NUMBER 				       C
C	     IION  -DEGREE OF IONIZATION, IION=1 FOR NEUTRAL...	       C
C	     CHI   -IONIZ. POTENTIAL OF THE ION [EV] 		       C
C	     HI    -NEUTR. HYDROGEN NUMBER DENSITY [CGS]	       C
C	     HEI   -NEUTR. HELIUM NUMBER DENSITY [CGS]		       C
C	     EKONC -ELECTRON NUMBER DENSITY [CGS]		       C
C	     TEMP   TEMPERATURE [KELVIN]			       C
C	     VT    -TURBULENCE VELOCITY [CGS]    		       C
C	     GR    -RADIATIVE DAMPING				       C
C	     GS    -STARK DAMPING				       C
C	     GW    -VAN DER WAALS DAMPING			       C
C								       C
C	     OUTPUT:                                                   C
C		A   - DAMPING PARAMETER                                C
C		DOP - DOPPLER. HALF-WIDTH [1/S]                        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	PI=3.1415926535897931d0
	EVCM=8.06554477D3
	CLIGHT=2.99792458D10
	DLL0=1.E-8*DL0
	BOL=1.380622D-16
	S=IAT
	Z=IION
C	     RADIATIVE DAMPING
	IF(GR0.GT.0.D0)THEN
	  GR=DEXP(2.302585D0*GR0)
	ELSE
	  GR=2.4734D-22*clight*clight/DLL0/DLL0
C	     V PRIPADE AK POZNAS SIRKU AUTOIONIZACNEJ HLADINY GRA
C	     V [EV] => GR=1.52E12*GRA
	ENDIF
	EFF2=Z*Z*13.595D0/(CHI-EP/EVCM)
	IF(EFF2.LT.0.D0.OR.EFF2.GT.25.D0)EFF2=25.D0
C	     STARK DAMPING
C	 IF(CHI.LE.EP/EVCM) THEN
C	     AUTOIONIZ. HORNA HLADINA ? =>BERIEME JU AKO EFF2=25
C            AJ VO VAN DER WAALSOVOM PRE S<20 (AT.CISLO)
C	   EFF2=0.D0
C	 ENDIF
	IF(GS0.NE.0.D0)THEN
	  GS=DEXP(2.302585D0*GS0)
	ELSE
	  GS=1.0D-8*EFF2**2.5D0
	ENDIF
C	     VAN DER WAALS DAMPING
	IF (INT(S).LT.21.OR.INT(S).GT.28)THEN
          R2=2.5D0*EFF2*EFF2/Z/Z
	ELSE
	  R2=(45.D0-S)/Z
	ENDIF
	IF(GW0.NE.0.D0)THEN
	  GW=DEXP(2.302585D0*GW0)
	ELSE
	  GW=4.5D-9*R2**0.4D0
	ENDIF
C	     DOPPLER halfwidth in frequency
	DOP=DSQRT(2.D0*BOL*TEMP/(AM*1.672614D-24)+VT*VT)/DLL0
C	     FRAME DAMPING PARAMETER
	A=(GR+GS*EKONC+GW*(HI+0.42D0*HEI)*(TEMP/1.D4)**0.3D0)
        A=A/(4.D0*PI*DOP)
	RETURN
	END
C-----------------------------------------------------------------------
	SUBROUTINE PFIRW(NDIM,NBOD,mion,NION,STA,TEMP,STAVS)
c	partition functions after Irwin
c	input: sta -Irwin's tabulated coeficients for each ion
c		of an atom
c	output: stavs(i,j)-partition function of j-th ion at i-th depth
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C	PARAMETER(mion=9)
        DIMENSION STA(6,mion),STAVS(NDIM,mion),TEMP(NDIM)
        DO 80 I=1,NBOD
	  DO 70 J=1,NION
	    IF (ABS(STA(1,J)-0.D0).LT.1.D-30) GO TO 60
	    SALA=DLOG(TEMP(I))
	    ARG=STA(1,J)+STA(2,J)*SALA+STA(3,J)*SALA*SALA
            ARG=ARG+STA(4,J)*SALA**3+STA(5,J)*SALA**4+STA(6,J)*SALA**5
	    STAVS(I,J)=DEXP(ARG)
	    GO TO 70
60	    STAVS(I,J)=1.D0
70	  CONTINUE
80      CONTINUE
c        WRITE (2,'(A)')' PARTITION FUNCTIONS'
c        WRITE (2,'(A,20I10)')' TEMP',(I,I=1,NION)
c	 DO 140 I=1,NBOD
c	  WRITE(2,130)TEMP(I),(STAVS(I,J),J=1,NION)
c130       FORMAT(F7.0,20E10.3) 
c140     CONTINUE
	RETURN
	END
C-----------------------------------------------------------------------
        subroutine potlow(temp,ane,eplow)
        implicit double precision (a-h,o-z)
c       temp - Temperature[K]
c       ane - electron number density [cm^{-3}]
c       lowering of the ionization potential for neutrals, z=1 in [eV]
c       for ions(z):      eplow(z)=z*eplow(1)
        bol=1.3806503d-16
	pi=3.1415926535897931d0
        e=4.80325d-10
        pom=bol*temp/8.d0/pi/ane
c       debye lenght in [CGS]
        debye=dsqrt(pom)/e
        eplow=1.44d-7/debye
        return
        end
C-----------------------------------------------------------------------
	SUBROUTINE PFDWOR(NDIM,NBOD,mion,NION,IAT,TEMP,EKONC,STAVS)
c
c       partition functions from UCLSYN:
c Smith K.C., Dworetsky M.M., 1988, In: Adelman S.J., Lanz T., eds,
c Elemental Abundance Analyses.  Institut d'Astronomie de l'Univ.  de
c Lausanne, Switzerland, p. 32
c	Budaj J., Dworetsky M.M., Smalley B, 2002,
c	Comm. Univ. London Obs. No. 82
c       URL=http://www.ulo.ucl.ac.uk/ulo_comms/82/index.html
c
c	output: stavs(i,j)-partition function of j-th ion at i-th depth
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	DIMENSION STAVS(NDIM,mion),EKONC(NDIM),TEMP(NDIM),U(5)
        DO 80 I=1,NBOD
          CALL POTLOW(TEMP(I),EKONC(I),EPLOW)
          THETA=5039.77D0/TEMP(I)
C		PARTFN IS IN 'pfdwor.inc'
          CALL PARTFN(IAT,THETA,U,EPLOW)
          DO 70 J=1,NION
	    IF(J.LE.5)THEN
	      STAVS(I,J)=U(J)
	    ELSE
	      STAVS(I,J)=1.D0
	    ENDIF
70        CONTINUE
80      CONTINUE
c        WRITE (2,'(A)')' PARTITION FUNCTIONS'
c        WRITE (2,'(A,20I10)')' TEMP',(I,I=1,NION)
c        DO 140 I=1,NBOD
c          WRITE(2,130)TEMP(I),(STAVS(I,J),J=1,NION)
c130       FORMAT(F7.0,20E10.3)
c140     CONTINUE
        RETURN
        END
C-----------------------------------------------------------------------
	SUBROUTINE ZASTUP(TEMP,EKONC,ZIP,STAVS,RR,NBOD,NION)
c	input:  temp,ekonc,nbod,nion
c		zip-ionization potentials
c		stavs-partition functions
c	output:
C       RR(I,J)- POPULATION OF THE J-TH ION/TOTAL ELEMENT POPULATION
C		 AT I-TH DEPTH
C	R1(J)= N(J)/N(1)
C	RION(J)= N(J)/N(J-1)
C	SAH	-CALLING FUNCTION
C	
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	include 'param.inc'
	DIMENSION ZIP(mion-1),STAVS(NDIM,mion),TEMP(NDIM),EKONC(NDIM)
        dimension RION(mion),R1(mion),RR(NDIM,mion)
        DO 40 I=1,NBOD
	  RION(1)=1.d0
	  R1(1)=1.d0
	  SUM1=1.D0
	  DO 20 J=2,NION
	    RION(J)=SAH(TEMP(I),EKONC(I),ZIP(J-1),STAVS(I,J-1)
     &      ,STAVS(I,J))
            R1(J)=R1(J-1)*RION(J)
	    SUM1=SUM1+R1(J)
20	  CONTINUE
	  DO 30 J=1,NION
	    RR(I,J)=R1(J)/SUM1
30	  CONTINUE
40	CONTINUE
c         WRITE(2,*)'   N(J)/N  ION POPULATIONS'
c         WRITE(2,'(A,40I11)')' DM',(I,I=1,NION)
c	DO 50 I=1,NBOD
c	  WRITE(2,130)DM(I),(RR(I,J),J=1,NION)
c130	  FORMAT(40(D10.3,1X))
c50	CONTINUE
	RETURN
	END
C-----------------------------------------------------------------------
        DOUBLE PRECISION FUNCTION SAH(TE,EK,ZP,S1,S2)
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C             SAHA EQUATION (RATIO OF THE NUMBER DENSITIES OF
C		TWO ADJACENT IONS)
C             ZP -in [EV], TE -in [K], EK -in [cm^-3]
        SAH=4.8293745D15*S2/S1*DSQRT(TE)**3
        SAH=SAH*DEXP(-1.16045059D4/TE*ZP)/EK
        RETURN   
        END      
C-----------------------------------------------------------------------
        DOUBLE PRECISION FUNCTION SAHAN(TE,ZP,S1,S2)
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C             PART OF SAHA EQUATION WITHOUT ELECTRON NUMBER DENSITY
C             ZP -in [EV], TE -in [K]
        SAHAN=4.8293745D15*S2/S1*DSQRT(TE)**3
        SAHAN=SAHAN*DEXP(-1.16045059D4/TE*ZP)
        RETURN   
        END      
C-----------------------------------------------------------------------
        DOUBLE PRECISION FUNCTION SAH2(TE)
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C             PART OF SAHA EQUATION FOR H2 MOLECULE: N(H2)/N(HI)^2
C       FROM KURUCZ, TE -in [K]
	bol=1.3806503d-16
	everg=1.602176462d-12
	bolte=bol*te/everg
        SAH2=4.477/BOLTE-4.6628D1+1.8031D-3*TE-5.0739D-7*TE*TE
        SAH2=SAH2+8.1424D-11*TE**3-5.0501D-15*TE**4-1.5D0*DLOG(TE)
        SAH2=DEXP(SAH2)
        RETURN   
        END      
c-----------------------------------------------------------------------
	DOUBLE PRECISION FUNCTION PLANCK(FREQ,TEP,IER)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C	CALCULATES INTENSITY OF THE BLACK BODY RADIATION
C		IF   IER<0  FREQ[CM], PLANCK[ERG/S/CM**2/CM]
C		     IER=0  FREQ [ANGST.], PLANCK [ERG/S/CM**2/ANGSTROM]
C		     IER>0  FREQ [1/S], PLANCK [ERG/S/CM**2/HZ]
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	IF(IER)10,20,30
C10	 WRITE(*,*)'EXP',1.4D0/FREQ/TEP
10       PLANCK=1.1904397D-5/FREQ**5/(DEXP(1.438769D0/FREQ/TEP)-1.)
	 RETURN
C20	 WRITE(*,*)'EXP',1.4D8/FREQ/TEP
20	 PLANCK=1.1904397D27/FREQ**5/(DEXP(1.438769D8/FREQ/TEP)-1.)
	 RETURN
C30	 WRITE(*,*)'EXP',4.7D-11*FREQ/TEP
30	 PLANCK=1.474501D-47*FREQ**3/(DEXP(4.799216D-11*FREQ/TEP)-1.)
	 RETURN
	END
c-----------------------------------------------------------------------
      SUBROUTINE STATE0(Adyp,Ad,Axi)
c		taken from the Synspec code:
c 	Hubeny I., Lanz T., Jeffery C.S., 1994, 
c	in: Newsletter on Analysis of Astronomical spectra No.20, 
c	ed. C.S. Jeffery (CCP7; St. Andrews: St. Andrews Univ.), 30
c
C     Initialization of the basic parameters for the Saha equation
C
	implicit double precision(a-h,o-z)
        include 'param.inc'
c	parameter(matom=99)
      character*4 DYP,ADYP
      DIMENSION D(3,MATOM),XI(8,MATOM),DYP(MATOM)
      DIMENSION AD(3,MATOM),AXI(8,MATOM),ADYP(MATOM)
C
      DATA DYP/' H  ',' He ',' Li ',' Be ',' B  ',' C  ',
     &         ' N  ',' O  ',' F  ',' Ne ',' Na ',' Mg ',
     &         ' Al ',' Si ',' P  ',' S  ',' Cl ',' Ar ',
     &         ' K  ',' Ca ',' Sc ',' Ti ',' V  ',' Cr ',
     &         ' Mn ',' Fe ',' Co ',' Ni ',' Cu ',' Zn ',
     &         ' Ga ',' Ge ',' As ',' Se ',' Br ',' Kr ',
     &         ' Rb ',' Sr ',' Y  ',' Zr ',' Nb ',' Mo ',
     &         ' Tc ',' Ru ',' Rh ',' Pd ',' Ag ',' Cd ', 
     &         ' In ',' Sn ',' Sb ',' Te ',' I  ',' Xe ',
     &         ' Cs ',' Ba ',' La ',' Ce ',' Pr ',' Nd ', 
     &         ' Pm ',' Sm ',' Eu ',' Gd ',' Tb ',' Dy ', 
     &         ' Ho ',' Er ',' Tm ',' Yb ',' Lu ',' Hf ', 
     &         ' Ta ',' W  ',' Re ',' Os ',' Ir ',' Pt ', 
     &         ' Au ',' Hg ',' Tl ',' Pb ',' Bi ',' Po ', 
     &         ' At ',' Rn ',' Fr ',' Ra ',' Ac ',' Th ', 
     &         ' Pa ',' U  ',' Np ',' Pu ',' Am ',' Cm ', 
     &         ' Bk ',' Cf ',' Es '/
C
C    Standard atomic constants for first 99 species 
C      Abundances for the first 30 from Grevesse & Sauval,
C         (1998, Space Sci. Rev. 85, 161)
C
C            Element Atomic  Solar    Std.
C                    weight abundance highest 
C 
C                                     ionization stage 
      DATA D/ 1.008, 1.00E 0, 2.,
     &        4.003, 1.00D-1, 3.,
     &        6.941, 1.26D-11, 4.,
     &        9.012, 2.51D-11, 5.,
     &       10.810, 5.0D-10, 5.,
     &       12.011, 3.31D-4, 5.,
     &       14.007, 8.32D-5, 5.,
     &       16.000, 6.76D-4, 5.,
     &       18.918, 3.16D-8, 5.,
     &       20.179, 1.23D-4, 5.,
     &       22.990, 2.14D-6, 5.,
     &       24.305, 3.80D-5, 5.,
     &       26.982, 2.95D-6, 5.,
     &       28.086, 3.55D-5, 5.,
     &       30.974, 2.82D-7, 5.,
     &       32.060, 2.14D-5, 5.,
     &       35.453, 3.16D-7, 5.,
     &       39.948, 2.52D-6, 5.,
     &       39.098, 1.32D-7, 5.,
     &       40.080, 2.29D-6, 5.,
     &       44.956, 1.48D-9, 5.,
     &       47.900, 1.05D-7, 5.,
     &       50.941, 1.00D-8, 5.,
     &       51.996, 4.68D-7, 5.,
     &       54.938, 2.45D-7, 5.,
     &       55.847, 3.16D-5, 5.,
     &       58.933, 8.32D-8, 5.,
     &       58.700, 1.78D-6, 5.,
     &       63.546, 1.62D-8, 5.,
     &       65.380, 3.98D-8, 5.,
     &       69.72 ,   1.34896324e-09  ,  3.,  
     &       72.60 ,   4.26579633e-09  ,  3.,  
     &       74.92 ,   2.34422821e-10  ,  3.,  
     &       78.96 ,   2.23872066e-09  ,  3.,  
     &       79.91 ,   4.26579633e-10  ,  3.,  
     &       83.80 ,   1.69824373e-09  ,  3.,  
     &       85.48 ,   2.51188699e-10  ,  3.,  
     &       87.63 ,   8.51138173e-10  ,  3.,  
     &       88.91 ,   1.65958702e-10  ,  3.,  
     &       91.22 ,   4.07380181e-10  ,  3.,  
     &       92.91 ,   2.51188630e-11  ,  3.,   
     &       95.95 ,   9.12010923e-11  ,  3.,   
     &       99.00 ,   1.00000000e-24  ,  3.,   
     &       101.1 ,   6.60693531e-11  ,  3.,   
     &       102.9 ,   1.23026887e-11  ,  3.,   
     &       106.4 ,   5.01187291e-11  ,  3.,   
     &       107.9 ,   1.73780087e-11  ,  3.,   
     &       112.4 ,   5.75439927e-11  ,  3.,   
     &       114.8 ,   6.60693440e-12  ,  3.,   
     &       118.7 ,   1.38038460e-10  ,  3.,   
     &       121.8 ,   1.09647810e-11  ,  3.,   
     &       127.6 ,   1.73780087e-10  ,  3.,   
     &       126.9 ,   3.23593651e-11  ,  3.,   
     &       131.3 ,   1.69824373e-10  ,  3.,   
     &       132.9 ,   1.31825676e-11  ,  3.,   
     &       137.4 ,   1.62181025e-10  ,  3.,   
     &       138.9 ,   1.58489337e-11  ,  3.,   
     &       140.1 ,   4.07380293e-11  ,  3.,   
     &       140.9 ,   6.02559549e-12  ,  3.,   
     &       144.3 ,   2.95120943e-11  ,  3.,   
     &       147.0 ,   1.00000000e-24  ,  3.,   
     &       150.4 ,   9.33254366e-12  ,  3.,   
     &       152.0 ,   3.46736869e-12  ,  3.,   
     &       157.3 ,   1.17489770e-11  ,  3.,   
     &       158.9 ,   2.13796216e-12  ,  3.,   
     &       162.5 ,   1.41253747e-11  ,  3.,   
     &       164.9 ,   3.16227767e-12  ,  3.,   
     &       167.3 ,   8.91250917e-12  ,  3.,   
     &       168.9 ,   1.34896287e-12  ,  3.,   
     &       173.0 ,   8.91250917e-12  ,  3.,   
     &       175.0 ,   1.31825674e-12  ,  3.,   
     &       178.5 ,   5.37031822e-12  ,  3.,   
     &       181.0 ,   1.34896287e-12  ,  3.,   
     &       183.9 ,   4.78630102e-12  ,  3.,   
     &       186.3 ,   1.86208719e-12  ,  3.,   
     &       190.2 ,   2.39883290e-11  ,  3.,   
     &       192.2 ,   2.34422885e-11  ,  3.,   
     &       195.1 ,   4.78630036e-11  ,  3.,   
     &       197.0 ,   6.76082952e-12  ,  3.,   
     &       200.6 ,   1.23026887e-11  ,  3.,   
     &       204.4 ,   6.60693440e-12  ,  3.,   
     &       207.2 ,   1.12201834e-10  ,  3.,   
     &       209.0 ,   5.12861361e-12  ,  3.,   
     &       210.0 ,   1.00000000e-24  ,  3.,   
     &       211.0 ,   1.00000000e-24  ,  3.,   
     &       222.0 ,   1.00000000e-24  ,  3.,   
     &       223.0 ,   1.00000000e-24  ,  3.,   
     &       226.1 ,   1.00000000e-24  ,  3.,   
     &       227.1 ,   1.00000000e-24  ,  3.,   
     &       232.0 ,   1.20226443e-12  ,  3.,   
     &       231.0 ,   1.00000000e-24  ,  3.,  
     &       238.0 ,   3.23593651e-13  ,  3.,  
     &       237.0 ,   1.00000000e-24  ,  0.,  
     &       244.0 ,   1.00000000e-24  ,  0.,  
     &       243.0 ,   1.00000000e-24  ,  0.,  
     &       247.0 ,   1.00000000e-24  ,  0.,  
     &       247.0 ,   1.00000000e-24  ,  0.,  
     &       251.0 ,   1.00000000e-24  ,  0.,  
     &       254.0 ,   1.00000000e-24  ,  0./
C
C
C     Ionization potentials for first 99 species:
      DATA XI/
C
C     Element Ionization potentials (eV) 
C              I     II      III     IV       V     VI     VII    VIII
C
     &       13.595,  0.   ,  0.   ,  0.   ,  0.  ,  0.  ,  0.  ,  0.  ,
     &       24.580, 54.400,  0.   ,  0.   ,  0.  ,  0.  ,  0.  ,  0.  ,
     &        5.392, 75.619,122.451,  0.   ,  0.  ,  0.  ,  0.  ,  0.  ,
     &        9.322, 18.206,153.850,217.713,  0.  ,  0.  ,  0.  ,  0.  ,
     &        8.296, 25.149, 37.920,259.298,340.22,  0.  ,  0.  ,  0.  ,
     &       11.264, 24.376, 47.864, 64.476,391.99,489.98,  0.  ,  0.  ,
     &       14.530, 29.593, 47.426, 77.450, 97.86,551.93,667.03,  0.  ,
     &       13.614, 35.108, 54.886, 77.394,113.87,138.08,739.11,871.39,
     &       17.418, 34.980, 62.646, 87.140,114.21,157.12,185.14,953.6 ,
     &       21.559, 41.070, 63.500, 97.020,126.30,157.91,207.21,239.0 ,
     &        5.138, 47.290, 71.650, 98.880,138.37,172.09,208.44,264.16,
     &        7.664, 15.030, 80.120,102.290,141.23,186.49,224.9 ,265.96, 
     &        5.984, 18.823, 28.440,119.960,153.77,190.42,241.38,284.53, 
     &        8.151, 16.350, 33.460, 45.140,166.73,205.11,246.41,303.07, 
     &       10.484, 19.720, 30.156, 51.354, 65.01,220.41,263.31,309.26,
     &       10.357, 23.400, 35.000, 47.290, 72.50, 88.03,280.99,328.8 ,
     &       12.970, 23.800, 39.900, 53.500, 67.80, 96.7 ,114.27,348.3 ,
     &       15.755, 27.620, 40.900, 59.790, 75.00, 91.3 ,124.0 ,143.46,
     &        4.339, 31.810, 46.000, 60.900, 82.6 , 99.7 ,118.0 ,155.0 ,
     &        6.111, 11.870, 51.210, 67.700, 84.39,109.0 ,128.0 ,147.0 ,
     &        6.560, 12.890, 24.750, 73.900, 92.0 ,111.1 ,138.0 ,158.7 ,
     &        6.830, 13.630, 28.140, 43.240, 99.8 ,120.0 ,140.8 ,168.5 ,
     &        6.740, 14.200, 29.700, 48.000, 65.2 ,128.9 ,151.0 ,173.7 ,
     &        6.763, 16.490, 30.950, 49.600, 73.0 , 90.6 ,161.1 ,184.7 ,
     &        7.432, 15.640, 33.690, 53.000, 76.0 , 97.0 ,119.24,196.46,
     &        7.870, 16.183, 30.652, 54.800, 75.0 , 99.1 ,125.0 ,151.06,
     &        7.860, 17.060, 33.490, 51.300, 79.5 ,102.0 ,129.0 ,157.0 ,
     &        7.635, 18.168, 35.170, 54.900, 75.5 ,108.0 ,133.0 ,162.0 ,
     &        7.726, 20.292, 36.830, 55.200, 79.9 ,103.0 ,139.0 ,166.0 ,
     &        9.394, 17.964, 39.722, 59.400, 82.6 ,108.0 ,134.0 ,174.0 ,
     &        6.000,  20.509,   30.700, 99.99,99.99,99.99,99.99,99.99,  
     &        7.89944,15.93462, 34.058, 45.715,99.99,99.99,99.99,99.99,    
     &        9.7887, 18.5892,  28.351, 99.99,99.99,99.99,99.99,99.99,    
     &        9.750,21.500, 32.000, 99.99,99.99,99.99,99.99,99.99,    
     &       11.839,21.600, 35.900, 99.99,99.99,99.99,99.99,99.99,    
     &       13.995,24.559, 36.900, 99.99,99.99,99.99,99.99,99.99,    
     &        4.175,27.500, 40.000, 99.99,99.99,99.99,99.99,99.99,    
     &        5.692,11.026, 43.000, 99.99,99.99,99.99,99.99,99.99,    
     &        6.2171,12.2236, 20.5244,60.607,99.99,99.99,99.99,99.99,    
     &        6.63390,13.13,23.17,34.418,80.348,99.99,99.99,99.99,    
     &        6.879,14.319, 25.039, 99.99,99.99,99.99,99.99,99.99,   
     &        7.099,16.149, 27.149, 99.99,99.99,99.99,99.99,99.99,   
     &        7.280,15.259, 30.000, 99.99,99.99,99.99,99.99,99.99,   
     &        7.364,16.759, 28.460, 99.99,99.99,99.99,99.99,99.99,   
     &        7.460,18.070, 31.049, 99.99,99.99,99.99,99.99,99.99,   
     &        8.329,19.419, 32.920, 99.99,99.99,99.99,99.99,99.99,   
     &        7.574,21.480, 34.819, 99.99,99.99,99.99,99.99,99.99,   
     &        8.990,16.903, 37.470, 99.99,99.99,99.99,99.99,99.99,   
     &        5.784,18.860, 28.029, 99.99,99.99,99.99,99.99,99.99,   
     &        7.342,14.627, 30.490,72.3,99.99,99.99,99.99,99.99,   
     &        8.639,16.500, 25.299,44.2,55.7,99.99,99.99,99.99,   
     &        9.0096,18.600, 27.96, 37.4,58.7,99.99,99.99,99.99,   
     &       10.454,19.090, 32.000, 99.99,99.99,99.99,99.99,99.99,   
     &       12.12984,20.975,31.05,45.,54.14,99.99,99.99,99.99,   
     &        3.893,25.100, 35.000, 99.99,99.99,99.99,99.99,99.99,   
     &        5.210,10.000, 37.000, 99.99,99.99,99.99,99.99,99.99,   
     &        5.580,11.060, 19.169, 99.99,99.99,99.99,99.99,99.99,   
     &        5.650,10.850, 20.080, 99.99,99.99,99.99,99.99,99.99,   
     &        5.419,10.550, 23.200, 99.99,99.99,99.99,99.99,99.99,   
     &        5.490,10.730, 20.000, 99.99,99.99,99.99,99.99,99.99,   
     &        5.550,10.899, 20.000, 99.99,99.99,99.99,99.99,99.99,   
     &        5.629,11.069, 20.000, 99.99,99.99,99.99,99.99,99.99,   
     &        5.680,11.250, 20.000, 99.99,99.99,99.99,99.99,99.99,   
     &        6.159,12.100, 20.000, 99.99,99.99,99.99,99.99,99.99,   
     &        5.849,11.519, 20.000, 99.99,99.99,99.99,99.99,99.99,   
     &        5.930,11.670, 20.000, 99.99,99.99,99.99,99.99,99.99,   
     &        6.020,11.800, 20.000, 99.99,99.99,99.99,99.99,99.99,   
     &        6.099,11.930, 20.000, 99.99,99.99,99.99,99.99,99.99,   
     &        6.180,12.050, 23.700, 99.99,99.99,99.99,99.99,99.99,   
     &        6.250,12.170, 20.000, 99.99,99.99,99.99,99.99,99.99,   
     &        6.099,13.899, 19.000, 99.99,99.99,99.99,99.99,99.99,   
     &        7.000,14.899, 23.299, 99.99,99.99,99.99,99.99,99.99,   
     &        7.879,16.200, 24.000, 99.99,99.99,99.99,99.99,99.99,   
     &        7.86404,17.700, 25.000, 99.99,99.99,99.99,99.99,99.99,   
     &        7.870,16.600, 26.000, 99.99,99.99,99.99,99.99,99.99,   
     &        8.500,17.000, 27.000, 99.99,99.99,99.99,99.99,99.99,   
     &        9.100,20.000, 28.000, 99.99,99.99,99.99,99.99,99.99,   
     &        8.95868,18.563,33.227, 99.99,99.99,99.99,99.99,99.99,   
     &        9.220,20.500, 30.000, 99.99,99.99,99.99,99.99,99.99,   
     &       10.430,18.750, 34.200, 99.99,99.99,99.99,99.99,99.99,   
     &        6.10829,20.4283,29.852,50.72,99.99,99.99,99.99,99.99,   
     &        7.416684,15.0325,31.9373,42.33,69.,99.99,99.99,99.99,   
     &        7.285519,16.679, 25.563,45.32,56.0,88.,99.99,99.99,   
     &        8.430,19.000, 27.000, 99.99,99.99,99.99,99.99,99.99,   
     &        9.300,20.000, 29.000, 99.99,99.99,99.99,99.99,99.99,   
     &       10.745,20.000, 30.000, 99.99,99.99,99.99,99.99,99.99,   
     &        4.000,22.000, 33.000, 99.99,99.99,99.99,99.99,99.99,   
     &        5.276,10.144, 34.000, 99.99,99.99,99.99,99.99,99.99,   
     &        6.900,12.100, 20.000, 99.99,99.99,99.99,99.99,99.99,   
     &        6.000,12.000, 20.000, 99.99,99.99,99.99,99.99,99.99,   
     &        6.000,12.000, 20.000, 99.99,99.99,99.99,99.99,99.99,    
     &        6.000,12.000, 20.000, 99.99,99.99,99.99,99.99,99.99,    
     &        6.000,12.000, 20.000, 99.99,99.99,99.99,99.99,99.99,    
     &        6.000,12.000, 20.000, 99.99,99.99,99.99,99.99,99.99,    
     &        6.000,12.000, 20.000, 99.99,99.99,99.99,99.99,99.99,    
     &        6.000,12.000, 20.000, 99.99,99.99,99.99,99.99,99.99,    
     &        6.000,12.000, 20.000, 99.99,99.99,99.99,99.99,99.99,    
     &        6.000,12.000, 20.000, 99.99,99.99,99.99,99.99,99.99,     
     &        6.000,12.000, 20.000, 99.99,99.99,99.99,99.99,99.99/
c
	do 20 i=1,matom
	  adyp(i)=dyp(i)
	  do 10 j=1,8
	    axi(j,i)=xi(j,i)	
10	  continue
	  do 15 j=1,3
	    ad(j,i)=d(j,i)	
15	  continue
20	continue
        return
        end
c-----------------------------------------------------------------------
	subroutine eldat(ichemc,iat,iion,dyp,d,xi,abhyd,abhel,wm
     &  ,hmc,nion,abund,zip,nline,ielnd,necod)
c		extract data for our particular elements of sp. lines
c	and change abundances
c	input: 
c		ichemc-switch to change built in chemical composition
c		iat-atomic number of the sp. line
c		iion-ionization degree of the sp.line
c		dyp,d,xi 
c		nline -number of sp. lines considered
c		ielnd-switch to calculate el.num.d. and read 3.column
c	output: 
c		d -weight,abundance, max ionization stage
c       	abhyd - H abundance N/N(TOT)
c		abhel - He abundance, N(EL)/N(H) 
c		wm -mean molecular weight= sum ai*mi/sum ai
c 		hmc - mass number
c		nion - number of ions of our element considered 
c		abund - abundance, N(EL)/N(H)
c		zip - first 8 ionization potentials [EV]
c		necod - code to include element into the el.num.dens. 
 	implicit double precision (a-h,o-z)
 	include 'param.inc'
 	character*4 dyp,elem
 	dimension d(3,matom),xi(8,matom),dyp(matom),zip(mion-1,mline)
        dimension elem(mline),nion(mline),iat(mline),iion(mline)
        dimension abund(mline),hmc(mline),necod(matom)
	if(ielnd.eq.1)ichemc=1
	do i=1,matom
	  necod(i)=0
	enddo
	if(ichemc.eq.1)then
	  write(*,*)' reading file abundances'
	  open(7,file='abundances',status='old')
          read(7,*)nichem
c	?? potential bug , initialize necod=0 for all=matom elements          
	  do 80 i=1,nichem
	    if(ielnd.eq.1)then	
              read(7,*)ii,abii,necdi
              necod(i)=necdi
	    else
              read(7,*)ii,abii
	    endif
	    d(2,ii)=abii
80 	  continue
	  close(7)	
	endif
	abhyd=0.d0
	wm=0.d0
	do 90 i=1,matom
	  abhyd=abhyd+d(2,i)
	  wm=wm+d(1,i)*d(2,i)
90 	continue
	abhyd=1.d0/abhyd
	wm=wm*abhyd
	abhel=d(2,2)
   	write(2,*)' No. of sp. lines considered:',nline
	if(nline.gt.0)then
	  do 110 i=1,nline
	    nion(i)=int(d(3,iat(i))+0.5d0)
	    if(iion(i).gt.nion(i).or.iion(i).gt.mion)then
	      write(*,*)i,'-th sp. line cannot be calculated,'
              write(*,*)' error iion>nion'
	      stop
	    endif
	    if(iion(i).gt.5)then
	      write(*,*)i,'-th sp. line, partition func. not available'
              write(*,*)', p.f. will be set=1, warning'
	    endif
	    abund(i)=d(2,iat(i))
	    hmc(i)=d(1,iat(i))
	    do 100 j=1,8
	      zip(j,i)=xi(j,iat(i))	
100	    continue
	    elem(i)=dyp(iat(i))
	    write(2,120)elem(i),iat(i),hmc(i),nion(i),abund(i)
110	  continue
	endif
	write(2,130)abhyd,abhel,wm
120	format(' elem=',a4,' iat=',i3,' hmc=',f8.4
     &  ,' nion=',i3,' abund=',e9.3)
130	format(' abhyd=',f5.3,' abhel=',f5.3,' wm=',e9.3)
	return
	end
c-----------------------------------------------------------------------
	subroutine ophyd(nbod,temp,ekonc,akonc,hi,hneg,abhyd
     &  ,ophbf,ophff,ophrs,ophn,hipf,wlab)
c	calculates HI bound-free, free-free, Rayleigh opacity
c	input: nbod,temp,ekonc,akonc,hi,hneg,abhyd,hipf,wlab[A]
c	output: ophbf,ophff,ophrs,ophn
        implicit double precision (a-h,o-z)
	include 'param.inc'
	dimension temp(ndim),ekonc(ndim),akonc(ndim),hipf(ndim,2)
        dimension hi(ndim),hneg(ndim)
        dimension ophbf(ndim),ophff(ndim),ophrs(ndim),ophn(ndim)
        bol=1.3806503d-16
        clight=2.99792458d10
        hjed=1.66053873d-24
        plank=6.62606876d-27
	rydbh=109677.585d0
	freq0=clight/wlab*1.D8
	pot=clight*plank*rydbh
	frray=2.463d15
	do 10 i=1,16
	  frjump=clight*rydbh*1.d0/dble(i*i)
	  if(frjump.lt.freq0)goto 20
10	continue
20	in=i
	do 70 i=1,nbod
c		HI bound-free opacity
	  hii=akonc(i)*abhyd-hi(i)
	  tk=bol*temp(i)
	  secor=(1.d0-dexp(-plank*freq0/tk))
	  if(in.lt.16)then
	    ophbf(i)=0.d0
	    do 60 j=1,3
	      en=dble(in+j-1)
	      chi=pot*(1.d0-1.d0/(en*en))
	      en3=en*en*en
	      gau=gaunt(in+j-1,freq0)
	      ophbf(i)=ophbf(i)+gau/en3*dexp(-chi/tk)
60          continue
	    en=dble(in+3)
	    chi=pot*(1.d0-1.d0/(en*en))
	    ophbf(i)=tk/pot*0.5d0*(dexp(-chi/tk)-dexp(-pot/tk))
     &      +ophbf(i)
c	    ophbf(i)=ophbf(i)*1.044d-26*wlab*wlab*wlab*2.d0/hipf(i,1)
c     &      *secor*hi(i)
	    ophbf(i)=ophbf(i)*2.8154d29/freq0/freq0/freq0*2.d0
	    ophbf(i)=ophbf(i)/hipf(i,1)*secor*hi(i)
	  else
	    ophbf(i)=0.d0
	  endif
c               HI free-free opacity
	  gau3=gfree(temp(i),freq0)
	  ophff(i)=3.69d8*gau3/(freq0*freq0*freq0*dsqrt(temp(i)))
	  ophff(i)=ophff(i)*ekonc(i)*hii*secor
c               HI Rayleigh scattering on neutral hydrogen
c		Kurucz R.L., 1970, SAO Special Report 309
c		note that scattering has no stimulated emission
	  xe=1.d-16*(min(freq0,frray)/clight)**2
	  ophrs(i)=(5.799d-13+(1.422d-6+2.784d0*xe)*xe)*xe*xe
	  ophrs(i)=ophrs(i)*2.d0*hi(i)/hipf(i,1)
c		H- bound-free opacity
	  ophnbf=hnbf(freq0)*hneg(i)*secor
c		H- free-free opacity
	  ophnff=hnff(freq0,ekonc(i),temp(i))*hi(i)*2.d0/hipf(i,1)
	  ophn(i)=ophnbf+ophnff	  
70	continue
	return
	end
c-----------------------------------------------------------------------
	double precision function hnbf(x)
	implicit double precision (a-h,o-z)
c	H- bound-free x-section from Kurucz 
c	without the stimulated emission
c	input: x-frequency [Hz]
c	output: cross-section [cm**2] per H- particle
c	works OK
	if(x.gt.2.111d14)then
	  pom1=(-5.519d27+4.808d41/x)
	  pom2=(1.481d13+pom1/x)
	  pom3=(5.358d-3+pom2/x)
	  hnbf=6.801d-20+pom3/x
c	elseif(x.gt.1.8259d14)then
c	this original Kurucz expression has a small bug 
c	it produces negative values at about 16400A 
c	which is near the ionization limit that is why I cut-off
c	before at 1.834d14
	elseif(x.gt.1.834d14)then
	  pom1=-1.251d-1+1.052d13/x
	  hnbf=3.695d-16+pom1/x
	else  
	  hnbf=0.d0
	endif  
	return
	end
c-----------------------------------------------------------------------
	double precision function hnff(x,en,t)
	implicit double precision (a-h,o-z)
c	H- free-free opacity from Kurucz
c	with the stimulated emission
c	input: 
c	x -frequency [Hz], en -electron num. density [cm^-3]
c	t -temperature[K]
c	output: cross-section [cm**2] per HI (in ground state)
c	works OK
	if(t.lt.1200.d0)then
	  hnff=0.d0
	elseif(t.gt.2.d4)then
	  hnff=0.d0
	elseif(x.lt.1.5d12)then
c       artificial constraint at 200mic to reach agreement with Gray 
c	within a factor of 2	
	  hnff=0.d0
	elseif(x.gt.4.d15)then
c	artificial constraint since it is already in Lyman cont.	
	  hnff=0.d0
	else  
	  pom1=4.3748d-10-2.5993d-7/t
	  hnff=(1.3727d-25+pom1/x)/x*en
	endif
	return
	end
c-----------------------------------------------------------------------
	subroutine hihei(ipart,nbod,temp,ekonc,akonc
     &	,hipf,hi,hneg,h2,hei,abhyd,abhel)
c	calculates H, He number densities and partition functions
c	input: ipart,nbod,temp,ekonc,akonc
c       	abhyd - H abundance N/N(TOT)
c		abhel - He abundance, N(EL)/N(H) 
c	output: hipf - HI partition function
c		hi,hneg,h2,hei - HI, H-, H2, HeI number densities
        implicit double precision (a-h,o-z) 
        include 'param.inc'
        dimension hipf(ndim,2),hepf(ndim,3),hi(ndim),hneg(ndim),h2(ndim)
        dimension HEI(NDIM),STAHI(6,2),STAHEI(6,3)
        dimension TEMP(NDIM),EKONC(NDIM),AKONC(NDIM)
C		POLYNOMIAL PARTITION FUNCTION COEFFICIENTS FOR 
C		HI,HII   A    HEI,HEII,HEIII 
C		AFTER IRWIN, 1981, ApJS 45,621	
        DATA STAHI/-2.61655891D2,1.63428326D2,-4.06133526D1,5.03282928D0
     &	 ,-3.10998364D-1,7.66654594D-3,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0
     &	 ,0.0D0/
        DATA STAHEI/-3.76575219D-1,2.33951687D-1,-5.79755525D-2
     &	 ,7.16333160D-3,-4.41302573D-4,1.08442997D-5,6.93147179D-1
     &	 ,9.29636701D-10,-2.30049742D-10,2.83829746D-11,-1.74590774D-12
     &	 ,4.28355287D-14,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0/
	if(ipart.eq.1)then
          CALL PFDWOR(NDIM,NBOD,2,2,1,TEMP,EKONC,hipf)
          CALL PFDWOR(NDIM,NBOD,3,3,2,TEMP,EKONC,hepf)
	else
	  do I=1,NBOD
	    SALA=DLOG(TEMP(I))
C		HIPF - H PARTITION FUNCTION
	    do J=1,2
	      HIPF(i,j)=DEXP(STAHI(1,J)+STAHI(2,J)*SALA
     &        +STAHI(3,J)*SALA*SALA+STAHI(4,J)*SALA**3
     &        +STAHI(5,J)*SALA**4+STAHI(6,J)*SALA**5)
	    enddo
C               HEPF - HE PARTITION FUNCTION
	    do J=1,3
	      HEPF(i,j)=DEXP(STAHEI(1,J)+STAHEI(2,J)*SALA+STAHEI(3,J)
     &	      *SALA*SALA+STAHEI(4,J)*SALA**3+STAHEI(5,J)*SALA**4
     &	      +STAHEI(6,J)*SALA**5)
	    enddo
	  enddo
	endif
C		HNEG - H- NUMBER DENSITY
C               HI - HI NUMBER DENSITY
C               HEI - HeI NUMBER DENSITY
C               H2 - H2 NUMBER DENSITY	
        pothm=0.7552d0
        stavhm=1.d0
        pothi=13.598d0
	do i=1,nbod
	  if(temp(i).lt.1.d2)then
c  	    H2
	    hi(i)=0.d0
	    hneg(i)=0.d0
	    h2(i)=akonc(i)*abhyd/2.d0
 	  elseif(temp(i).lt.2.d4)then
c	    HII+HI+H-+H2 	  
            rt3=sahan(temp(i),pothi,hipf(i,1),hipf(i,2))
            rt2=sahan(temp(i),pothm,stavhm,hipf(i,1))          
	    rt1=sah2(temp(i))
	    b=1.d0+ekonc(i)/rt2+rt3/ekonc(i)
	    b=b/2.d0/rt1
	    c=-akonc(i)*abhyd/2.d0/rt1
	    b2=b*b
	    d=b2-4.d0*c
	    if(-4.d0*c/b2.lt.1.d-10)then
c           then avoid substracting two similar numbers -b+b
              hi(i)=-c/b
            else
              hi(i)=(-b+dsqrt(d))/2.d0
            endif
	    hneg(i)=hi(i)*ekonc(i)/rt2
	    h2(i)=rt1*hi(i)*hi(i)
	  else
c	    HII+HI+H-	  	  
            rt2=sahan(temp(i),pothm,stavhm,hipf(i,1))
            rt3=sahan(temp(i),pothi,hipf(i,1),hipf(i,2))
            bot=1.d0+ekonc(i)/rt2+rt3/ekonc(i)
            hi(i)=akonc(i)*abhyd/bot
            hneg(i)=hi(i)*ekonc(i)/rt2
            h2(i)=0.d0
	  endif
	  HEI(I)=1.D0/(1.D0+SAH(TEMP(I),EKONC(I),24.587D0,HEPF(i,1)
     &	  ,HEPF(i,2))*(1.D0+SAH(TEMP(I),EKONC(I),54.416D0,HEPF(i,2)
     &	  ,HEPF(i,3))))*AKONC(I)*ABHEL*abhyd
	enddo
	return
	end
c-----------------------------------------------------------------------
      SUBROUTINE HUNT(XX,N,X,JLO)
c	taken from Numerical recipes
c	given field XX and a value X subroutine returns JLO such that
c	XX(JLO)<X<XX(JLO+1) 
c	JLO on input is taken as the intial guess for JLO on output
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION XX(N)
      LOGICAL ASCND
      ASCND=XX(N).GT.XX(1)
      IF(JLO.LE.0.OR.JLO.GT.N)THEN
        JLO=0
        JHI=N+1
        GO TO 3
      ENDIF
      INC=1
      IF(X.GE.XX(JLO).EQV.ASCND)THEN
1       JHI=JLO+INC
        IF(JHI.GT.N)THEN
          JHI=N+1
        ELSE IF(X.GE.XX(JHI).EQV.ASCND)THEN
          JLO=JHI
          INC=INC+INC
          GO TO 1
        ENDIF
      ELSE
        JHI=JLO
2       JLO=JHI-INC
        IF(JLO.LT.1)THEN
          JLO=0
        ELSE IF(X.LT.XX(JLO).EQV.ASCND)THEN
          JHI=JLO
          INC=INC+INC
          GO TO 2
        ENDIF
      ENDIF
3     IF(JHI-JLO.EQ.1)RETURN
      JM=(JHI+JLO)/2
      IF(X.GT.XX(JM).EQV.ASCND)THEN
        JLO=JM
      ELSE
        JHI=JM
      ENDIF
      GO TO 3
      END
c-----------------------------------------------------------------------
	subroutine rot1d1(theta,alpha,dcut1,temp0,ane0,iii,jjj
     &  ,vxst,vyst,vzst,vxstr,vystr,vzstr
     &  ,xcp,ycp,zcp,xcpr,ycpr,zcpr
     &  ,vxcp,vycp,vzcp,vxcpr,vycpr,vzcpr
     &  ,ndimf1,ndimf2,ndimf3,nbodf1,nbodf2,nbodf3
     &  ,ndim1,ndim2,ndim3,nbod1,nbod2,nbod3
     &  ,ar,at,az,far,fat,faz
     &  ,atemp,adustt,adustd,adens,ane,avr,avt,avz,avtrb
     &  ,ftemp,fdustt,fdustd,fdens,fne,fvr,fvt,fvz,fvtrb)
c	linear interpolation in the transparent object
c	nearest neighbour in the nontransparent object     
c	to transform (rotate) the line of sight grid (x,y,z) to the body
c	frozen grid (x'',y'',z'') and interpolate state quantities from 
c	the rotating body frozen coordinates (RBFC) to the line of sight 
c	coordinates (LSC) and transform the velocity field vectors 
c	from RBFC to LSC
c	input: 	
c         theta,alpha,dcut1
c	  temp0,ane0 - out of grid (sphere) reasonable values
c	  iii,jjj -identify x,y the ray
c         vxst,vyst,vzst - net velocity of the primary
c	  xcp,ycp,zcp - position of the secondary
c	  vxcp,vycp,vzcp - velocity of the secondary
c	  ar,at,az -coord. in which the model is necessary (LSC)
c	  far,fat,faz -coord. in which the model is defined (RBFC)
c	  ftemp,fdustt,fdustd,fdens,fne,fvr,fvt,fvz,fvtrb -model
c	output:
c	  atemp,adustt,adustd,adens,ane,avr,avt,avz,avtrb- rotated model
c         vxstr,vystr,vzstr- rotated net velocity of the primary
c	  xcpr,ycpr,zcpr -rotated position of the secondary
c	  vxcpr,vycpr,vzcpr - rotated velocity of the secondary 
c	parameter (ndim1=100,ndim2=100,ndim3=200)
	implicit double precision (a-h,o-z)
	dimension ar(ndim1),at(ndim2),az(ndim3)
        dimension far(ndimf1),fat(ndimf2),faz(ndimf3)
        dimension atemp(ndim3),adustt(ndim3),adustd(ndim3)
        dimension adens(ndim3),ane(ndim3)
        dimension avr(ndim3),avt(ndim3)
        dimension avz(ndim3),avtrb(ndim3)
        dimension ftemp(ndimf1,ndimf2,ndimf3)
        dimension fdustt(ndimf1,ndimf2,ndimf3)
        dimension fdustd(ndimf1,ndimf2,ndimf3)
        dimension fdens(ndimf1,ndimf2,ndimf3),fne(ndimf1,ndimf2,ndimf3)
        dimension fvr(ndimf1,ndimf2,ndimf3),fvt(ndimf1,ndimf2,ndimf3)
        dimension fvz(ndimf1,ndimf2,ndimf3),fvtrb(ndimf1,ndimf2,ndimf3)
	ctheta=dcos(theta)
	stheta=dsin(theta)
	calpha=dcos(alpha)
	salpha=dsin(alpha)
c	initialization of first values to ii,jj,kk (index in RBFC)
	ii=1
	jj=1
	kk=1
	i=iii
	j=jjj
	do 10 k=1,nbod3
c	  calculation of RBFC coordinates corresponding to the grid 
c	  points of the line of sight grid 
          call rotz(ar(i),at(j),az(k),fx,fy,fz
     &    ,ctheta,stheta,calpha,salpha)
c	  	interpolation from body frozen grid to required 
c	  line of sight grid  :
	  if(fx.gt.far(1).and.fx.lt.far(nbodf1).and.
     &    fy.gt.fat(1).and.fy.lt.fat(nbodf2).and.
     &    fz.gt.faz(1).and.fz.lt.faz(nbodf3))then
	    call hunt(far,nbodf1,fx,ii)
	    if(ii.lt.1.or.ii.ge.nbodf1)then
	      if(ii.eq.nbodf1.and.fx.eq.far(nbodf1))then
		ii=nbodf1-1
	      else		
	        write(*,*)' compiler/maschine dependent error'
                write(*,'(a,4i4)')' -out of grid at ii,i,j,k',ii,i,j,k
	      endif
	    endif
	    call hunt(fat,nbodf2,fy,jj)
	    if(jj.lt.1.or.jj.ge.nbodf2)then
	      if(jj.eq.nbodf2.and.fy.eq.fat(nbodf2))then
		jj=nbodf2-1
	      else		
	        write(*,*)' compiler/maschine dependent error'
                write(*,'(a,4i4)')' -out of grid at jj,i,j,k',jj,i,j,k
	      endif
	    endif
	    call hunt(faz,nbodf3,fz,kk)
	    if(kk.lt.1.or.kk.ge.nbodf3)then
	      if(kk.eq.nbodf3.and.fz.eq.faz(nbodf3))then
		kk=nbodf3-1
	      else		
	        write(*,*)' compiler/maschine dependent error'
                write(*,'(a,4i4)')' -out of grid at kk,i,j,k',kk,i,j,k
	      endif
	    endif
c		abnormal interpolation-nearest neighbour value
c		in the untransparent object or on its edge.
c		density of an untransparent object dens>dcut1
	    if(fdens(ii,jj,kk).gt.dcut1
     &      .or.fdens(ii,jj,kk+1).gt.dcut1
     &      .or.fdens(ii,jj+1,kk).gt.dcut1
     &      .or.fdens(ii,jj+1,kk+1).gt.dcut1
     &      .or.fdens(ii+1,jj,kk).gt.dcut1
     &      .or.fdens(ii+1,jj,kk+1).gt.dcut1
     &      .or.fdens(ii+1,jj+1,kk).gt.dcut1
     &      .or.fdens(ii+1,jj+1,kk+1).gt.dcut1)then
	      dist1=1.d60
	      do 5 ii1=ii,ii+1
              do 4 jj1=jj,jj+1
              do 3 kk1=kk,kk+1	
		dx=fx-far(ii1)
                dy=fy-fat(jj1)
                dz=fz-faz(kk1)
		dist=dx*dx+dy*dy+dz*dz
		if(dist.lt.dist1)then
	          atemp(k)=ftemp(ii1,jj1,kk1)
	          adustt(k)=fdustt(ii1,jj1,kk1)
	          adustd(k)=fdustd(ii1,jj1,kk1)
	          adens(k)=fdens(ii1,jj1,kk1)
	          ane(k)=fne(ii1,jj1,kk1)
	          avr(k)=fvr(ii1,jj1,kk1)
	          avt(k)=fvt(ii1,jj1,kk1)
	          avz(k)=fvz(ii1,jj1,kk1)
	          avtrb(k)=fvtrb(ii1,jj1,kk1)
		  dist1=dist
	        endif
3	      continue
4             continue
5             continue
	      goto 10	
	    endif
c		normal interpolation
	    tt=(fx-far(ii))/(far(ii+1)-far(ii))
	    uu=(fy-fat(jj))/(fat(jj+1)-fat(jj))
	    vv=(fz-faz(kk))/(faz(kk+1)-faz(kk))
	    tt1=1.d0-tt
	    uu1=1.d0-uu
	    vv1=1.d0-vv	
	    atemp(k)=tt1*uu1*vv1*ftemp(ii,jj,kk)
     &        +tt*uu1*vv1*ftemp(ii+1,jj,kk)
     &	      +tt*uu*vv1*ftemp(ii+1,jj+1,kk)
     &	      +tt1*uu*vv1*ftemp(ii,jj+1,kk)
     &        +tt1*uu1*vv*ftemp(ii,jj,kk+1)
     &        +tt*uu1*vv*ftemp(ii+1,jj,kk+1)
     &        +tt*uu*vv*ftemp(ii+1,jj+1,kk+1)
     &        +tt1*uu*vv*ftemp(ii,jj+1,kk+1)
	    adustt(k)=tt1*uu1*vv1*fdustt(ii,jj,kk)
     &        +tt*uu1*vv1*fdustt(ii+1,jj,kk)
     &	      +tt*uu*vv1*fdustt(ii+1,jj+1,kk)
     &	      +tt1*uu*vv1*fdustt(ii,jj+1,kk)
     &        +tt1*uu1*vv*fdustt(ii,jj,kk+1)
     &        +tt*uu1*vv*fdustt(ii+1,jj,kk+1)
     &        +tt*uu*vv*fdustt(ii+1,jj+1,kk+1)
     &        +tt1*uu*vv*fdustt(ii,jj+1,kk+1)
	    adustd(k)=tt1*uu1*vv1*fdustd(ii,jj,kk)
     &        +tt*uu1*vv1*fdustd(ii+1,jj,kk)
     &	      +tt*uu*vv1*fdustd(ii+1,jj+1,kk)
     &	      +tt1*uu*vv1*fdustd(ii,jj+1,kk)
     &        +tt1*uu1*vv*fdustd(ii,jj,kk+1)
     &        +tt*uu1*vv*fdustd(ii+1,jj,kk+1)
     &        +tt*uu*vv*fdustd(ii+1,jj+1,kk+1)
     &        +tt1*uu*vv*fdustd(ii,jj+1,kk+1)
	    adens(k)=tt1*uu1*vv1*fdens(ii,jj,kk)
     &        +tt*uu1*vv1*fdens(ii+1,jj,kk)
     &	      +tt*uu*vv1*fdens(ii+1,jj+1,kk)
     &	      +tt1*uu*vv1*fdens(ii,jj+1,kk)
     &        +tt1*uu1*vv*fdens(ii,jj,kk+1)
     &        +tt*uu1*vv*fdens(ii+1,jj,kk+1)
     &        +tt*uu*vv*fdens(ii+1,jj+1,kk+1)
     &        +tt1*uu*vv*fdens(ii,jj+1,kk+1)
	    ane(k)=tt1*uu1*vv1*fne(ii,jj,kk)
     &        +tt*uu1*vv1*fne(ii+1,jj,kk)
     &	      +tt*uu*vv1*fne(ii+1,jj+1,kk)
     &	      +tt1*uu*vv1*fne(ii,jj+1,kk)
     &        +tt1*uu1*vv*fne(ii,jj,kk+1)
     &        +tt*uu1*vv*fne(ii+1,jj,kk+1)
     &        +tt*uu*vv*fne(ii+1,jj+1,kk+1)
     &        +tt1*uu*vv*fne(ii,jj+1,kk+1)
	    avr(k)=tt1*uu1*vv1*fvr(ii,jj,kk)
     &        +tt*uu1*vv1*fvr(ii+1,jj,kk)
     &	      +tt*uu*vv1*fvr(ii+1,jj+1,kk)
     &	      +tt1*uu*vv1*fvr(ii,jj+1,kk)
     &        +tt1*uu1*vv*fvr(ii,jj,kk+1)
     &        +tt*uu1*vv*fvr(ii+1,jj,kk+1)
     &        +tt*uu*vv*fvr(ii+1,jj+1,kk+1)
     &        +tt1*uu*vv*fvr(ii,jj+1,kk+1)
	    avt(k)=tt1*uu1*vv1*fvt(ii,jj,kk)
     &        +tt*uu1*vv1*fvt(ii+1,jj,kk)
     &	      +tt*uu*vv1*fvt(ii+1,jj+1,kk)
     &	      +tt1*uu*vv1*fvt(ii,jj+1,kk)
     &        +tt1*uu1*vv*fvt(ii,jj,kk+1)
     &        +tt*uu1*vv*fvt(ii+1,jj,kk+1)
     &        +tt*uu*vv*fvt(ii+1,jj+1,kk+1)
     &        +tt1*uu*vv*fvt(ii,jj+1,kk+1)
	    avz(k)=tt1*uu1*vv1*fvz(ii,jj,kk)
     &        +tt*uu1*vv1*fvz(ii+1,jj,kk)
     &	      +tt*uu*vv1*fvz(ii+1,jj+1,kk)
     &	      +tt1*uu*vv1*fvz(ii,jj+1,kk)
     &        +tt1*uu1*vv*fvz(ii,jj,kk+1)
     &        +tt*uu1*vv*fvz(ii+1,jj,kk+1)
     &        +tt*uu*vv*fvz(ii+1,jj+1,kk+1)
     &        +tt1*uu*vv*fvz(ii,jj+1,kk+1)
	    avtrb(k)=tt1*uu1*vv1*fvtrb(ii,jj,kk)
     &        +tt*uu1*vv1*fvtrb(ii+1,jj,kk)
     &	      +tt*uu*vv1*fvtrb(ii+1,jj+1,kk)
     &	      +tt1*uu*vv1*fvtrb(ii,jj+1,kk)
     &        +tt1*uu1*vv*fvtrb(ii,jj,kk+1)
     &        +tt*uu1*vv*fvtrb(ii+1,jj,kk+1)
     &        +tt*uu*vv*fvtrb(ii+1,jj+1,kk+1)
     &        +tt1*uu*vv*fvtrb(ii,jj+1,kk+1)
	  else
	    atemp(k)=temp0
	    adustt(k)=temp0
	    adustd(k)=0.d0
	    adens(k)=0.d0
	    ane(k)=ane0
	    avr(k)=0.d0
	    avt(k)=0.d0
	    avz(k)=0.d0
	    avtrb(k)=0.d0
	  endif
10	continue
c	back transform of vector quantities
	do k=1,nbod3
 	  call rotzb(avr(k),avt(k),avz(k)
     &    ,avr(k),avt(k),avz(k),ctheta,stheta,calpha,salpha)
	enddo
c	back transform of the velocity of the center of mass 
c	of the primary explicitely because of the scattered light 
c	treatment
  	call rotzb(vxst,vyst,vzst,vxstr,vystr,vzstr
     &  ,ctheta,stheta,calpha,salpha)
c	back transform of the center of the secondary explicitely 
c	because	of the limb darkening treatement
	call rotzb(xcp,ycp,zcp,xcpr,ycpr,zcpr
     &  ,ctheta,stheta,calpha,salpha)
c       back transform of the velocity of the secondary explicitely 
c	because of the scattered light treatment
        call rotzb(vxcp,vycp,vzcp,vxcpr,vycpr,vzcpr
     &  ,ctheta,stheta,calpha,salpha)
        return
        end
c-----------------------------------------------------------------------
	subroutine rot1d2(theta,alpha,dcut1,temp0,ane0,iii,jjj
     &  ,vxst,vyst,vzst,vxstr,vystr,vzstr
     &  ,xcp,ycp,zcp,xcpr,ycpr,zcpr
     &  ,vxcp,vycp,vzcp,vxcpr,vycpr,vzcpr
     &  ,ndimf1,ndimf2,ndimf3,nbodf1,nbodf2,nbodf3
     &  ,ndim1,ndim2,ndim3,nbod1,nbod2,nbod3
     &  ,ar,at,az,far,fat,faz
     &  ,atemp,adustt,adustd,adens,ane,avr,avt,avz,avtrb
     &  ,ftemp,fdustt,fdustd,fdens,fne,fvr,fvt,fvz,fvtrb)
c	nearest neighbour in transparent and nontransparent objects     
c	to transform (rotate) the line of sight grid (x,y,z) to the body
c	frozen grid (x'',y'',z'') and interpolate state quantities from 
c	the rotating body frozen coordinates (RBFC) to the line of sight 
c	coordinates (LSC) and transform the velocity field vectors 
c	from RBFC to LSC
c	input: 	
c         theta,alpha,dcut1
c	  temp0,ane0 - out of grid (sphere) reasonable values
c         iii,jjj -identify x,y the ray
c         vxst,vyst,vzst - net velocity of the primary
c	  xcp,ycp,zcp - position of the secondary
c	  vxcp,vycp,vzcp - velocity of the secondary
c	  ar,at,az -coord. in which the model is necessary (LSC)
c	  far,fat,faz -coord. in which the model is defined (RBFC)
c	  ftemp,fdustt,fdustd,fdens,fne,fvr,fvt,fvz,fvtrb -model
c	output:
c         atemp,adustt,adustd,adens,ane,avr,avt,avz,avtrb- rotated model
c         vxstr,vystr,vzstr-rotated net velocity of the primary
c	  xcpr,ycpr,zcpr -rotated position of the secondary
c	  vxcpr,vycpr,vzcpr - rotated velocity of the secondary 
c	parameter (ndim1=100,ndim2=100,ndim3=200)
	implicit double precision (a-h,o-z)
	dimension ar(ndim1),at(ndim2),az(ndim3)
        dimension far(ndimf1),fat(ndimf2),faz(ndimf3)
        dimension atemp(ndim3),adustt(ndim3),adustd(ndim3)
        dimension adens(ndim3),ane(ndim3)
        dimension avr(ndim3),avt(ndim3)
        dimension avz(ndim3),avtrb(ndim3)
        dimension ftemp(ndimf1,ndimf2,ndimf3)
        dimension fdustt(ndimf1,ndimf2,ndimf3)
        dimension fdustd(ndimf1,ndimf2,ndimf3)
        dimension fdens(ndimf1,ndimf2,ndimf3),fne(ndimf1,ndimf2,ndimf3)
        dimension fvr(ndimf1,ndimf2,ndimf3),fvt(ndimf1,ndimf2,ndimf3)
        dimension fvz(ndimf1,ndimf2,ndimf3),fvtrb(ndimf1,ndimf2,ndimf3)
	ctheta=dcos(theta)
	stheta=dsin(theta)
	calpha=dcos(alpha)
	salpha=dsin(alpha)
c	initialization of first values to ii,jj,kk
	ii=1
	jj=1
	kk=1
	i=iii
	j=jjj
	do k=1,nbod3
c	  calculation of RBFC coordinates corresponding to the grid 
c	  points of the line of sight grid 
          call rotz(ar(i),at(j),az(k),fx,fy,fz
     &    ,ctheta,stheta,calpha,salpha)
c	  	interpolation from body frozen grid to required 
c	  line of sight grid  :
	  if(fx.gt.far(1).and.fx.lt.far(nbodf1).and.
     &    fy.gt.fat(1).and.fy.lt.fat(nbodf2).and.
     &    fz.gt.faz(1).and.fz.lt.faz(nbodf3))then
	    call hunt(far,nbodf1,fx,ii)
	    if(ii.lt.1.or.ii.ge.nbodf1)then
	      write(*,*)' compiler/maschine dependent error'
              write(*,'(a,4i4)')' -out of grid at ii,i,j,k',ii,i,j,k
	    endif
	    call hunt(fat,nbodf2,fy,jj)
	    if(jj.lt.1.or.jj.ge.nbodf2)then
	      write(*,*)' compiler/maschine dependent error'
              write(*,'(a,4i4)')' -out of grid at jj,i,j,k',jj,i,j,k
	    endif
	    call hunt(faz,nbodf3,fz,kk)
	    if(kk.lt.1.or.kk.ge.nbodf3)then
	      write(*,*)' compiler/maschine dependent error'
              write(*,'(a,4i4)')' -out of grid at kk,i,j,k',kk,i,j,k
	    endif
c		nearest neighbour value approximation
	    dist1=1.d60
	    do ii1=ii,ii+1
            do jj1=jj,jj+1
            do kk1=kk,kk+1	
	      dx=fx-far(ii1)
              dy=fy-fat(jj1)
              dz=fz-faz(kk1)
	      dist=dx*dx+dy*dy+dz*dz
	      if(dist.lt.dist1)then
	        atemp(k)=ftemp(ii1,jj1,kk1)
	        adustt(k)=fdustt(ii1,jj1,kk1)
	        adustd(k)=fdustd(ii1,jj1,kk1)
	        adens(k)=fdens(ii1,jj1,kk1)
	        ane(k)=fne(ii1,jj1,kk1)
	        avr(k)=fvr(ii1,jj1,kk1)
	        avt(k)=fvt(ii1,jj1,kk1)
	        avz(k)=fvz(ii1,jj1,kk1)
	        avtrb(k)=fvtrb(ii1,jj1,kk1)
		dist1=dist
	      endif
	    enddo
            enddo
            enddo
	  else
	    atemp(k)=temp0
	    adustt(k)=temp0
	    adustd(k)=0.d0
	    adens(k)=0.d0
	    ane(k)=ane0
	    avr(k)=0.d0
	    avt(k)=0.d0
	    avz(k)=0.d0
	    avtrb(k)=0.d0
	  endif
	enddo
c	back transform of vector quantities
	do k=1,nbod3
 	  call rotzb(avr(k),avt(k),avz(k)
     &    ,avr(k),avt(k),avz(k),ctheta,stheta,calpha,salpha)
	enddo
c	back transform of the velocity of the center of mass 
c	of the primary explicitely because of the scattered light 
c	treatment
  	call rotzb(vxst,vyst,vzst,vxstr,vystr,vzstr
     &  ,ctheta,stheta,calpha,salpha)
c	back transform of the center of the secondary explicitely 
c	because	of the limb darkening treatement
	call rotzb(xcp,ycp,zcp,xcpr,ycpr,zcpr
     &  ,ctheta,stheta,calpha,salpha)
c       back transform of the velocity of the secondary explicitely 
c	because of the scattered light treatment
        call rotzb(vxcp,vycp,vzcp,vxcpr,vycpr,vzcpr
     &  ,ctheta,stheta,calpha,salpha)
        return
        end
c-----------------------------------------------------------------------
        subroutine rotzb(xpp,ypp,zpp,x,y,z,ctheta,stheta,calpha,salpha) 
c       transformation from body frozen (x'',y'',z'') to line of sight  
c       coordinates (x,y,z) 
c       i.e. rotation about  z'=z'' and then about x=x'
c       input: xpp,ypp,zpp   
c       output:x,y,z
        implicit double precision (a-h,o-z)
        zp=zpp
        xp=xpp*calpha-ypp*salpha
        yp=ypp*calpha+xpp*salpha
        x=xp
        y=yp*ctheta+zp*stheta
        z=zp*ctheta-yp*stheta
        return
        end   
c-----------------------------------------------------------------------
        subroutine rotz(x,y,z,xpp,ypp,zpp,ctheta,stheta,calpha,salpha)  
c       transformation from line of sight (x,y,z) to body frozen 
c       coordinates (x'',y'',z'')
c       i.e. rotation about x=x' and then about z'=z''
c       input: x,y,z   
c       output:xpp,ypp,zpp
        implicit double precision (a-h,o-z)
        xp=x
        yp=y*ctheta-z*stheta
        zp=z*ctheta+y*stheta
        zpp=zp
        xpp=xp*calpha+yp*salpha
        ypp=yp*calpha-xp*salpha
        return
        end   
c-----------------------------------------------------------------------
	subroutine surf(ndim1,ndim2,nbod1,nbod2,ar,at,area)
c	to calculate area(i,j)-area of a projected surface element
c	input: ar,at
c	output: area
	implicit double precision (a-h,o-z)
	dimension ar(ndim1),at(ndim2),area(ndim1,ndim2)
	pi=3.1415926535897931d0
	do i=2,nbod1-1
	do j=2,nbod2-1
	  area(i,j)=(ar(i+1)-ar(i-1))*(at(j+1)-at(j-1))*0.25d0
	enddo
	enddo
	pomi12=(ar(2)-ar(1))*0.5d0
	pominn=(ar(nbod1)-ar(nbod1-1))*0.5d0
	do j=2,nbod2-1
	  area(1,j)=pomi12*(at(j+1)-at(j-1))*0.5d0
	  area(nbod1,j)=pominn*(at(j+1)-at(j-1))*0.5d0
	enddo
	do i=2,nbod1-1
          dar=ar(i+1)-ar(i-1)
	  area(i,1)=dar*(at(2)-at(1))*0.25d0
	  area(i,nbod2)=dar*(at(nbod2)-at(nbod2-1))*0.25d0
	enddo
	area(1,1)=pomi12*(at(2)-at(1))*0.5d0
        area(1,nbod2)=pomi12*(at(nbod2)-at(nbod2-1))*0.5d0
        area(nbod1,1)=pominn*(at(2)-at(1))*0.5d0
        area(nbod1,nbod2)=pominn*(at(nbod2)-at(nbod2-1))*0.5d0
	return
	end
c-----------------------------------------------------------------------       
	subroutine tlac1(ndim3,nbod3,az,vr,vt,vz
     &  ,temp,ekonc,akonc,dens,dustd,dustt,hi,hneg,h2,hei,hipf)
c	output of selected quantities for selected ray
c	along the light of sight
	implicit double precision (a-h,o-z)
	dimension az(ndim3),hei(ndim3)
	dimension hipf(ndim3,2),hi(ndim3),hneg(ndim3),h2(ndim3)
        dimension vr(ndim3),vt(ndim3),vz(ndim3)
        dimension temp(ndim3),ekonc(ndim3),akonc(ndim3)
        dimension dens(ndim3),dustd(ndim3),dustt(ndim3)
        write(2,'(a,a)')' vel.  vx        vy        vz        dens'
     &  ,'      dustd     dustt'
        do kk=1,nbod3
          write(2,20)kk,vr(kk),vt(kk),vz(kk),dens(kk)
     &    ,dustd(kk),dustt(kk)
	enddo
20      format(i4,6es10.2)
        write (2,'(a,a)')'       akonc      ekonc      temp'
     &        ,'       az'
        do kk=1,nbod3
          write(2,40)kk,akonc(kk),ekonc(kk),temp(kk),az(kk)
	enddo
40      format(i4,4es11.3)
        write(2,'(a,a)')'       AKONC      HI         H-         H2'
     &  ,'         HEI        HIPF       HIIPF'
        do kk=1,nbod3
	  write(2,60)kk,akonc(kk),hi(kk),hneg(kk),h2(kk),hei(kk)
     &	  ,hipf(kk,1),hipf(kk,2) 
	enddo
60      format(i4,7es11.3)	
c        write(2,'(a)')'        ophbf1     ophff1     ophrs1'
c        do kk=1,nbod3
c	  write(2,'(i4,3es11.3)')kk,ophbf(kk),ophff(kk),ophrs(kk)
c	enddo
	return
	end
c-----------------------------------------------------------------------
	subroutine tlac2(ndim3,nbod3,mion
     &  ,nion,iion,az,zip,eup
     &  ,temp,stavs,rr,gr,gs,gw)
c	output of selected quantities for selected ray
c	along the light of sight
	implicit double precision (a-h,o-z)
	dimension az(ndim3),temp(ndim3)
        dimension stavs(ndim3,mion),RR(ndim3,mion),zip(mion-1)
        EVCM=8.06554477D3
        WRITE (2,'(A)')' PARTITION FUNCTIONS'
        WRITE (2,'(A,20I10)')' TEMP',(kk,kk=1,NION)
        do kk=1,nbod3
          WRITE(2,240)TEMP(kk),(STAVS(kk,jj1),jj1=1,NION)
        enddo
240     FORMAT(F7.0,20E10.3)
        WRITE(2,*)'   N(J)/N  ION POPULATIONS'
        WRITE(2,'(A,40I11)')' z',(kk,kk=1,NION)
        do kk=1,nbod3
          WRITE(2,260)az(kk),(RR(kk,jj1),jj1=1,NION)
        enddo
260     FORMAT(40(D10.3,1X))
	write(2,'(a,a)')' i   GR        GS        GW        '
        DO kk=1,nbod3
          IF(ZIP(IION).LT.EUP/EVCM)THEN
	    WRITE(2,310)kk,GR,GS,GW
	  ELSE
	    WRITE(2,320)kk,GR,GS,GW
	  ENDIF
        enddo
310     FORMAT(i3,3E10.3,' AUTO')
320     FORMAT(i3,3E10.3)
	return
	end
c-----------------------------------------------------------------------
	subroutine smod1(nbodf1,nbodf2,nbodf3,nbodfa,nbodfb
     &  ,rmdfx1,rmdfx2,rmdfy1,rmdfy2,rmdfz1,rmdfz2,rmdfx3,rmdfx4
     &  ,gainfx,gainfy,gainfz
     &  ,rstar,tstar,emstar,xstar,ystar,zstar,vrotst,idifst,drotst,hst
     &  ,istar,vxst,vyst,vzst,dgst,ffst,irrst,albst,htst,htsta
     &  ,ispst,xspst,yspst,zspst,aspst,tspst
     &  ,icomp,dgcp,ffcp,qq,vrxcp,vrycp,vrzcp,vrotcp,rcp
     &  ,xcp,ycp,zcp,vxcp,vycp,vzcp,tempcp,irrcp,albcp,htcp,htcpa
     &  ,ienv,emen,qqen,aen,ffen,hen
     &  ,tempen,densen,aneen,vtrben,dstden,dstten     
     &  ,ispot,vrxsp,vrysp,vrzsp,vrotsp,rsp
     &  ,xsp,ysp,zsp,vxsp,vysp,vzsp,tempsp,denssp,anesp,vtrbsp
     &  ,dstdsp,dsttsp
     &  ,iring,rrg,emrg,b1rg,b2rg,a1rg,a2rg,dr1rg,dr2rg
     &  ,xrg,yrg,zrg,xpolrg,ypolrg,zpolrg,vxrg,vyrg,vzrg
     &  ,temprg,densrg,anerg,vtrbrg,itrg
     &  ,edenrg,dstdrg,ede2rg,dst2rg,dsttrg
     &  ,idisc,adisc,rindc,routdc,emdc,rdc
     &  ,xdc,ydc,zdc,xdisc,ydisc,zdisc,vxdc,vydc,vzdc
     &  ,tempdc,densdc,anedc,vtrbdc,edendc,itdc,etmpdc,dstddc,dsttdc
     &  ,inebl,aneb,rinnb,routnb,emnb,rnb
     &  ,hinvnb,tinvnb,hwindnb,ndennb,denxnb,denznb
     &  ,xneb,yneb,zneb,vxnb,vynb,vznb
     &  ,tempnb,densnb,anenb,vtrbnb,edennb,itnb,etmpnb,dstdnb,dsttnb
     &  ,x1sm,y1sm,z1sm,x2sm,y2sm,z2sm,v1sm,v2sm,r1sm,r2sm
     &  ,ism,vxsm,vysm,vzsm,xsm,ysm,zsm,psm
     &  ,tempsm,denssm,anesm,vtrbsm,edensm,dstdsm,dsttsm
     &  ,x1fw,y1fw,z1fw,x2fw,y2fw,z2fw,v1fw,v2fw,r1fw,r2fw
     &  ,iflow,vxfw,vyfw,vzfw,xfw,yfw,zfw,pfw
     &  ,tempfw,densfw,anefw,vtrbfw,edenfw,dstdfw,dsttfw
     &  ,iufo,aufo,rinuf,routuf,emuf,ruf
     &  ,xuf,yuf,zuf,xufo,yufo,zufo,vxuf,vyuf,vzuf
     &  ,tempuf,densuf,aneuf,vtrbuf,edenuf,ituf,etmpuf,dstduf,dsttuf
     &  ,ajet,rinjt,routjt,vjt,xjet,yjet,zjet,vxjt,vyjt,vzjt
     &  ,tempjt,densjt,anejt,vtrbjt,ijet,dstdjt,dsttjt
     &  ,rinsh,routsh,vsh,vxsh,vysh,vzsh
     &  ,tempsh,denssh,anesh,vtrbsh,ishell,evelsh,rcsh,dstdsh,dsttsh
     &  ,v0,temp0,dens0,ane0,dcut1,dcut2,dcut3,dcutn
     &  ,far,fat,faz,ftemp,fdens,fne,fvr,fvt,fvz,fvtrb,fdustd,fdustt)
        implicit double precision (a-h,o-z)
        include 'param.inc'
	dimension far(ndimf1),fat(ndimf2),faz(ndimf3)
        dimension ftemp(ndimf1,ndimf2,ndimf3)
        dimension fdens(ndimf1,ndimf2,ndimf3),fne(ndimf1,ndimf2,ndimf3)
        dimension fvr(ndimf1,ndimf2,ndimf3),fvt(ndimf1,ndimf2,ndimf3)
        dimension fvz(ndimf1,ndimf2,ndimf3),fvtrb(ndimf1,ndimf2,ndimf3)
        dimension fdustd(ndimf1,ndimf2,ndimf3)
        dimension fdustt(ndimf1,ndimf2,ndimf3)
        dimension rosup(ndimf1,ndimf2),rosus(ndimf1,ndimf2)
        dimension rotemp(ndimf1,ndimf2),rotems(ndimf1,ndimf2)
	dimension rosue(ndimf1,ndimf2),roteme(ndimf1,ndimf2)
        dimension trmt(3,3),trmtnb(3,3),trmtuf(3,3)
        dimension trmtst(3,3),trmtrg(3,3)
        dimension denxnb(mstarx),denznb(mstarx)
c	Calculates model of a shell (behaviour of state quantities and
c	velocity field in Cartesian coordinates x,y,z=far,fat,faz. 
c	fvr,fvt,fvz are corresponding components of velocity vector.
c	ftemp,fdens,fne are temperature,density
c	and electron number density of gas , respectively.
c	fvtrb-microturbulence, 
c       fdustd - dust density, fdustt - dust temperature.
c	Reasonable vaules of temp and ekonc must be defined 
c	everywhere but you can use dens<=0.d0 to define empty space
c	or dcut1<dens<dcut2 for central nontransparent objects (star),
c	dcut2<dens<dcut3 for second nontransparent object (companion),
c       dcut3<dens<dcutn for 3. nontransparent object (3.body),
c	dcutn<dens for any dark nontransparent object.
	clight=2.99792458d10
	pi=3.1415926535897931d0
        grav=6.673d-8
        stefb=5.670400d-5
c        rsol=6.9599d10
        rsol=6.95508d10
c	nbodf1,nbodf2,nbodf3>=1
	dunt1=(dcut1+dcut2)*0.5d0
	dunt2=(dcut2+dcut3)*0.5d0
c		definition of the model body frozen grid
	call defbfg(ndimf1,ndimf2,ndimf3,nbodfa,nbodf2,nbodf3,nbodfb
     &  ,rmdfx1,rmdfx2,rmdfy1,rmdfy2,rmdfz1,rmdfz2,rmdfx3,rmdfx4
     &  ,gainfx,gainfy,gainfz,far,fat,faz)
	if(istar.gt.1.or.icomp.gt.1.or.
     &	(istar.gt.0.and.icomp.gt.0.and.vxst.gt.clight))then
	  a1=xcp*qq/(1.d0+qq)
	  a2=xcp-a1
	  porbst=2.d0*pi*dsqrt(xcp**3/(grav*emstar*(1.d0+qq)))
c	  a1 is local auxiliary variable	  
c         omgst for istar=1 is temporary and will be overwritten
c	  in the subroutine star
	  omgst=2.d0*pi/porbst
	  omgcp=omgst
	  vxst=0.d0
	  vyst=-omgst*a1
	  vzst=0.d0
   	  vxcp=0.d0
   	  vycp=omgst*a2
   	  vzcp=0.d0
	  ycp=0.d0
	  zcp=0.d0
	else
	  porbst=0.d0  
	endif
	if(istar.eq.1)then
	  unstar=xstar*xstar+ystar*ystar+zstar*zstar
	  unstar=dsqrt(unstar)
	  xstar=xstar/unstar
	  ystar=ystar/unstar
	  zstar=zstar/unstar
	  unspst=xspst*xspst+yspst*yspst+zspst*zspst
	  unspst=dsqrt(unspst)
	  xspst=xspst/unspst
	  yspst=yspst/unspst
	  zspst=zspst/unspst
	  aspst=dcos(aspst)
c	  omgst=vrotst/rstar
c		calculates rotation matrix trmtst
	  call trans(xstar,ystar,zstar,trmtst)
	elseif(istar.eq.2)then
	  xstar=0.d0
	  ystar=0.d0
	  zstar=1.d0
c		rosup>=0 -Roche surface of the primary
     	  call roche(rosup,rotemp,areasp,teffp
     &    ,rfront,rback,rpole,rside,rmean 
     &    ,tstar,rstar,dgst,irrst,albst,htst,htsta
     &    ,tempcp,rcp,dgcp,irrcp,albcp,htcp,htcpa
     &    ,far,fat,xcp,qq,ffst,nbodf1,nbodf2,1)
	elseif(istar.eq.3)then
	  xstar=0.d0
	  ystar=0.d0
	  zstar=1.d0
	  icomp=0
c		rosup>=0 -Roche surface of the contact system
     	  call roche(rosup,rotemp,areasp,teffp
     &    ,rfront,rback,rpole,rside,rmean 
     &    ,tstar,rstar,dgst,irrst,albst,htst,htsta
     &    ,tempcp,rcp,dgcp,irrcp,albcp,htcp,htcpa
     &    ,far,fat,xcp,qq,ffst,nbodf1,nbodf2,3)     
c          do 34 i=1,nbodf1
c          do 32 j=1,nbodf2
c            write(19,*)far(i),fat(j),rosup(i,j)
c32	  continue
c	  write(19,*)
c34	  continue
	endif
	if(icomp.eq.2)then
	  vrxcp=0.d0
	  vrycp=0.d0
	  vrzcp=1.d0
c  	        rosus>=0 -Roche surface of the secondary
     	  call roche(rosus,rotems,areass,teffs
     &    ,rfront,rback,rpole,rside,rmean 
     &    ,tstar,rstar,dgst,irrst,albst,htst,htsta
     &    ,tempcp,rcp,dgcp,irrcp,albcp,htcp,htcpa
     &    ,far,fat,xcp,qq,ffcp,nbodf1,nbodf2,2)          
	endif
	if(ienv.eq.2.or.ienv.eq.3)then
	  a1=aen*qqen/(1.d0+qqen)
	  porb=2.d0*pi*dsqrt(aen**3/(grav*emen*(1.d0+qqen)))
	  omgen=2.d0*pi/porb
	  vxen=0.d0
	  vyen=-omgen*a1
	  vzen=0.d0
	  xen=0.d0
	  yen=0.d0
	  zen=1.d0
	endif
	if(ienv.eq.2)then
c		rosue>=0 -Roche surface of the envelope
     	  call roche(rosue,roteme,arease,teffe
     &    ,rfront,rback,rpole,rside,rmean 
     &    ,tempen,0.d0,0.d0,0,0.d0,1.d0,1.d0
     &    ,tempen,0.d0,0.d0,0,0.d0,1.d0,1.d0
     &    ,far,fat,aen,qqen,ffen,nbodf1,nbodf2,1)
	elseif(ienv.eq.3)then
c	  icomp=0
c		rosue>=0 -Roche surface of the common envelope
     	  call roche(rosue,roteme,arease,teffe
     &    ,rfront,rback,rpole,rside,rmean 
     &    ,tempen,0.d0,0.d0,0,0.d0,1.d0,1.d0
     &    ,tempen,0.d0,0.d0,0,0.d0,1.d0,1.d0
     &    ,far,fat,aen,qqen,ffen,nbodf1,nbodf2,3)     
     	endif
	if(ism.eq.1)then
	  unsm=xsm*xsm+ysm*ysm+zsm*zsm
	  unsm=dsqrt(unsm)
	  xsm=xsm/unsm
	  ysm=ysm/unsm
	  zsm=zsm/unsm
	  dxsm=x2sm-x1sm
	  dysm=y2sm-y1sm
	  dzsm=z2sm-z1sm	
	  dsm=dxsm*dxsm+dysm*dysm+dzsm*dzsm
	  dsm=dsqrt(dsm)
	  dxsm=dxsm/dsm
	  dysm=dysm/dsm
	  dzsm=dzsm/dsm	
          omgsm=2.d0*pi/psm
	endif
	if(iring.gt.0)then
	  unrg=xpolrg*xpolrg+ypolrg*ypolrg+zpolrg*zpolrg
	  unrg=dsqrt(unrg)
	  xpolrg=xpolrg/unrg
	  ypolrg=ypolrg/unrg
	  zpolrg=zpolrg/unrg
c		calculates rotation matrix trmtrg
	  call trans(xpolrg,ypolrg,zpolrg,trmtrg)
          vrg=dsqrt(grav*emrg/rrg)/rrg
	endif		
	if(idisc.gt.0)then
	  undisc=xdisc*xdisc+ydisc*ydisc+zdisc*zdisc
	  undisc=dsqrt(undisc)
	  xdisc=xdisc/undisc
	  ydisc=ydisc/undisc
	  zdisc=zdisc/undisc
c		calculates rotation matrix trmt
	  call trans(xdisc,ydisc,zdisc,trmt)
	endif
	if(inebl.eq.4)then
	  unneb=xneb*xneb+yneb*yneb+zneb*zneb
	  unneb=dsqrt(unneb)
	  xneb=xneb/unneb
	  yneb=yneb/unneb
	  zneb=zneb/unneb
c		calculates rotation matrix trmtnb
	  call trans(xneb,yneb,zneb,trmtnb)
	endif
	if(iflow.eq.1)then
	  unfw=xfw*xfw+yfw*yfw+zfw*zfw
	  unfw=dsqrt(unfw)
	  xfw=xfw/unfw
	  yfw=yfw/unfw
	  zfw=zfw/unfw
	  dxfw=x2fw-x1fw
	  dyfw=y2fw-y1fw
	  dzfw=z2fw-z1fw	
	  dfw=dxfw*dxfw+dyfw*dyfw+dzfw*dzfw
	  dfw=dsqrt(dfw)
	  dxfw=dxfw/dfw
	  dyfw=dyfw/dfw
	  dzfw=dzfw/dfw	
          omgfw=2.d0*pi/pfw
	endif
	if(iufo.gt.0)then
	  unufo=xufo*xufo+yufo*yufo+zufo*zufo
	  unufo=dsqrt(unufo)
	  xufo=xufo/unufo
	  yufo=yufo/unufo
	  zufo=zufo/unufo
c		calculates rotation matrix trmtuf
	  call trans(xufo,yufo,zufo,trmtuf)
	endif
        if(ijet.eq.1.or.ijet.eq.2)then	
	  unjet=xjet*xjet+yjet*yjet+zjet*zjet
	  unjet=dsqrt(unjet)
	endif
	write(2,*)' Porbst[d]  istar  icomp'
	write(2,*)porbst/8.64d4,istar,icomp
	write(2,'(a,a)')'     xstar      ystar      zstar      '
     &  ,'vxst       vyst       vzst'
        write(2,'(6e11.3)')xstar,ystar,zstar,vxst,vyst,vzst
	write(2,'(a,a)')'     vrxcp      vrycp      vrzcp      '
     &  ,'vxcp       vycp       vzcp'
        write(2,'(6e11.3)')vrxcp,vrycp,vrzcp,vxcp,vycp,vzcp
        write(2,'(a,a)')'       xcp        ycp        zcp      '
        write(2,'(3e11.3)')xcp,ycp,zcp
        if(istar.gt.1)then
          write(2,'(a,a)')' Roche S.  Teffprim '
          write(2,'(2e12.4)')areasp,teffp
          write(2,'(a,a)')'  Rsub     Rback    Rpole    Rside    Rmean'
     &    ,'    Rsub/Rpole in units of a'
          write(2,'(6f9.5)')rfront,rback,rpole,rside,rmean,rfront/rpole
        endif  
        if(icomp.eq.2)then
          write(2,'(a,a)')' Roche S. Teffsec'
          write(2,'(2e12.4)')areass,teffs        
          write(2,'(a,a)')'  Rsub     Rback    Rpole    Rside    Rmean'
     &    ,'    Rsub/Rpole in units of a'
          write(2,'(6f9.5)')rfront,rback,rpole,rside,rmean,rfront/rpole          
        endif
c        WRITE(2,'(A,A)')'  i  j  k      temp     dustd      dens'
c     &  ,'       ane       avr       avt       avz     avtrb'
c	The model with a star, companion, and possible: 
c	spot, stream, ring, disc, nebula, flow, jet, ufo, shell, 
c       and background
	do 60 i=1,nbodf1
	  do 50 j=1,nbodf2
	    do 40 k=1,nbodf3
	      inx=0
	      er2=far(i)*far(i)+fat(j)*fat(j)+faz(k)*faz(k)
	      er=dsqrt(er2)
c	      		inside the star
              call star(istar,rstar,tstar,xstar,ystar,zstar
     &        ,vxst,vyst,vzst,rosup(i,j),rotemp(i,j),trmtst
     &        ,irrst,htst,htsta,htstb,albst
     &        ,icomp,xcp,ycp,zcp,rcp,tempcp
     &        ,idifst,vrotst,drotst,hst
     &        ,ispst,xspst,yspst,zspst,aspst,tspst
     &        ,dunt1,ane0,er,er2,porbst
     &        ,far(i),fat(j),faz(k),inx
     &        ,ftemp(i,j,k),fdens(i,j,k),fne(i,j,k)
     &        ,fdustd(i,j,k),fdustt(i,j,k)
     &        ,fvr(i,j,k),fvt(i,j,k),fvz(i,j,k),fvtrb(i,j,k))
              if(inx.eq.1)goto 40
c	      		inside the companion
	      if(icomp.eq.1)then
	        farcp=far(i)-xcp		
	        fatcp=fat(j)-ycp
	        fazcp=faz(k)-zcp
	        ermcp=farcp*farcp+fatcp*fatcp+fazcp*fazcp
	        ermcp=dsqrt(ermcp)	
                if(ermcp.lt.rcp)then
	 	  unspot=vrxcp*vrxcp+vrycp*vrycp+vrzcp*vrzcp
		  unspot=dsqrt(unspot)
		  omgcp=vrotcp/rcp/unspot
	          ftemp(i,j,k)=tempcp
	          fdustt(i,j,k)=tempcp
	          fdustd(i,j,k)=0.d0
	          fdens(i,j,k)=dunt2
	          fne(i,j,k)=ane0
	          fvr(i,j,k)=omgcp*(vrycp*fazcp-vrzcp*fatcp)+vxcp
	          fvt(i,j,k)=omgcp*(vrzcp*farcp-vrxcp*fazcp)+vycp
	          fvz(i,j,k)=omgcp*(vrxcp*fatcp-vrycp*farcp)+vzcp
	          fvtrb(i,j,k)=0.d0
	          goto 40
	        endif	      
	      elseif(icomp.eq.2)then
   	        if(dabs(faz(k)).lt.rosus(i,j))then
   	          farcp=far(i)-xcp                
   	          fatcp=fat(j)-ycp
   	          fazcp=faz(k)-zcp
   	          ftemp(i,j,k)=rotems(i,j)
	          fdustt(i,j,k)=rotems(i,j)
	          fdustd(i,j,k)=0.d0
	          fdens(i,j,k)=dunt2
	          fne(i,j,k)=ane0
	          fvr(i,j,k)=omgcp*(vrycp*fazcp-vrzcp*fatcp)+vxcp
	          fvt(i,j,k)=omgcp*(vrzcp*farcp-vrxcp*fazcp)+vycp
	          fvz(i,j,k)=omgcp*(vrxcp*fatcp-vrycp*farcp)+vzcp
	          fvtrb(i,j,k)=0.d0
	          goto 40
		endif
	      endif	      
c			inside the envelope
	      call envelope(ienv,rosue(i,j),hen,xen,yen,zen
     &        ,vxen,vyen,vzen,omgen,inx
     &        ,tempen,dstden,dstten,densen,aneen,vtrben
     &        ,far(i),fat(j),faz(k)
     &        ,ftemp(i,j,k),fdens(i,j,k),fne(i,j,k)
     &        ,fdustd(i,j,k),fdustt(i,j,k)
     &        ,fvr(i,j,k),fvt(i,j,k),fvz(i,j,k),fvtrb(i,j,k))
	      if(inx.eq.1)goto 40
c	      		inside the spot
	      if(ispot.eq.1)then
	        farsp=far(i)-xsp		
	        fatsp=fat(j)-ysp
	        fazsp=faz(k)-zsp
	        ermsp=farsp*farsp+fatsp*fatsp+fazsp*fazsp
	        ermsp=dsqrt(ermsp)	
                if(ermsp.lt.rsp)then
	 	  unspot=vrxsp*vrxsp+vrysp*vrysp+vrzsp*vrzsp
		  unspot=dsqrt(unspot)
		  omega=vrotsp/rsp/unspot
	          ftemp(i,j,k)=tempsp
	          fdens(i,j,k)=denssp
	          fne(i,j,k)=anesp
	          fdustd(i,j,k)=dstdsp
	          fdustt(i,j,k)=dsttsp
	          fvr(i,j,k)=omega*(vrysp*fazsp-vrzsp*fatsp)+vxsp
	          fvt(i,j,k)=omega*(vrzsp*farsp-vrxsp*fazsp)+vysp
	          fvz(i,j,k)=omega*(vrxsp*fatsp-vrysp*farsp)+vzsp
	          fvtrb(i,j,k)=vtrbsp
	          goto 40
	        endif	      
	      endif	      
c	      		in the stream
	      call stream(ism,dxsm,dysm,dzsm,x1sm,y1sm,z1sm
     &        ,r1sm,r2sm,v1sm,v2sm,vxsm,vysm,vzsm
     &        ,omgsm,xsm,ysm,zsm,dsm
     &        ,vtrbsm,tempsm,dstdsm,dsttsm,denssm,anesm,edensm
     &        ,far(i),fat(j),faz(k),inx
     &        ,ftemp(i,j,k),fdens(i,j,k),fne(i,j,k)
     &        ,fdustd(i,j,k),fdustt(i,j,k)
     &        ,fvr(i,j,k),fvt(i,j,k),fvz(i,j,k),fvtrb(i,j,k))
	      if(inx.eq.1)goto 40
c	      		in the ring
	      call ring(iring,xrg,yrg,zrg
     &	      ,a1rg,a2rg,b1rg,b2rg,dr1rg,dr2rg,rrg,vrg,itrg
     &        ,vxrg,vyrg,vzrg,xpolrg,ypolrg,zpolrg,trmtrg,dcut1
     &        ,vtrbrg,temprg,densrg,anerg
     &        ,dstdrg,dst2rg,edenrg,ede2rg,dsttrg
     &        ,far(i),fat(j),faz(k),inx
     &        ,ftemp(i,j,k),fdens(i,j,k),fne(i,j,k)
     &        ,fdustd(i,j,k),fdustt(i,j,k)
     &        ,fvr(i,j,k),fvt(i,j,k),fvz(i,j,k),fvtrb(i,j,k))
     	      if(inx.eq.1)goto 40
c	      		in the disc
	      call ufo(idisc,xdisc,ydisc,zdisc
     &	      ,adisc,rindc,routdc,emdc,rdc,xdc,ydc,zdc
     &        ,vxdc,vydc,vzdc,trmt
     &        ,tempdc,densdc,anedc,vtrbdc,edendc,itdc,etmpdc
     &        ,dstddc,dsttdc
     &        ,far(i),fat(j),faz(k),inx
     &        ,ftemp(i,j,k),fdens(i,j,k),fne(i,j,k)
     &        ,fdustd(i,j,k),fdustt(i,j,k)
     &        ,fvr(i,j,k),fvt(i,j,k),fvz(i,j,k),fvtrb(i,j,k))
     	      if(inx.eq.1)goto 40
c	      		in the nebula
	      call nebula(inebl,xneb,yneb,zneb
     &	      ,aneb,rinnb,routnb,emnb,rnb
     &        ,hinvnb,tinvnb,hwindnb,ndennb,denxnb,denznb
     &        ,vxnb,vynb,vznb,trmtnb
     &        ,tempnb,densnb,anenb,vtrbnb,edennb,itnb,etmpnb
     &        ,dstdnb,dsttnb
     &        ,far(i),fat(j),faz(k),er,inx
     &        ,h00nb,h01nb,si00nb,si01nb,ss01nb
     &        ,ftemp(i,j,k),fdens(i,j,k),fne(i,j,k)
     &        ,fdustd(i,j,k),fdustt(i,j,k)
     &        ,fvr(i,j,k),fvt(i,j,k),fvz(i,j,k),fvtrb(i,j,k))
     	      if(inx.eq.1)goto 40
c	      		in the flow
	      call stream(iflow,dxfw,dyfw,dzfw,x1fw,y1fw,z1fw
     &        ,r1fw,r2fw,v1fw,v2fw,vxfw,vyfw,vzfw
     &        ,omgfw,xfw,yfw,zfw,dfw
     &        ,vtrbfw,tempfw,dstdfw,dsttfw,densfw,anefw,edenfw
     &        ,far(i),fat(j),faz(k),inx
     &        ,ftemp(i,j,k),fdens(i,j,k),fne(i,j,k)
     &        ,fdustd(i,j,k),fdustt(i,j,k)
     &        ,fvr(i,j,k),fvt(i,j,k),fvz(i,j,k),fvtrb(i,j,k))
	      if(inx.eq.1)goto 40
c	      		in the jet 
	      call sjet(ijet,ajet,rinjt,routjt,xjet,yjet,zjet
     &        ,vjt,vxjt,vyjt,vzjt,vtrbjt,tempjt,densjt,anejt
     &        ,dstdjt,dsttjt
     &        ,far(i),fat(j),faz(k),unjet,er,inx
     &        ,ftemp(i,j,k),fdens(i,j,k),fne(i,j,k)
     &        ,fdustd(i,j,k),fdustt(i,j,k)
     &        ,fvr(i,j,k),fvt(i,j,k),fvz(i,j,k),fvtrb(i,j,k))
              if(inx.eq.1)goto 40
c	      		in the ufo
	      call ufo(iufo,xufo,yufo,zufo
     &	      ,aufo,rinuf,routuf,emuf,ruf,xuf,yuf,zuf
     &        ,vxuf,vyuf,vzuf,trmtuf
     &        ,tempuf,densuf,aneuf,vtrbuf,edenuf,ituf,etmpuf
     &        ,dstduf,dsttuf
     &        ,far(i),fat(j),faz(k),inx
     &        ,ftemp(i,j,k),fdens(i,j,k),fne(i,j,k)
     &        ,fdustd(i,j,k),fdustt(i,j,k)
     &        ,fvr(i,j,k),fvt(i,j,k),fvz(i,j,k),fvtrb(i,j,k))
     	      if(inx.eq.1)goto 40
c	      		in the shell
	      call shell(ishell,rinsh,routsh,rcsh
     &        ,vsh,vxsh,vysh,vzsh,vtrbsh
     &        ,tempsh,dstdsh,dsttsh,denssh,anesh,evelsh
     &        ,far(i),fat(j),faz(k),er,inx
     &        ,ftemp(i,j,k),fdens(i,j,k),fne(i,j,k)
     &        ,fdustd(i,j,k),fdustt(i,j,k)
     &        ,fvr(i,j,k),fvt(i,j,k),fvz(i,j,k),fvtrb(i,j,k))
              if(inx.eq.1)goto 40
c	      		in the background	
	      ftemp(i,j,k)=temp0
	      fdens(i,j,k)=dens0
	      fne(i,j,k)=ane0
	      fdustd(i,j,k)=0.d0
	      fdustt(i,j,k)=temp0
	      fvr(i,j,k)=v0*far(i)/er
	      fvt(i,j,k)=v0*fat(j)/er
	      fvz(i,j,k)=v0*faz(k)/er
	      fvtrb(i,j,k)=0.d0	
c	      write(2,70)i,j,k,ftemp(i,j,k),fdustd(i,j,k),fdens(i,j,k)
c     &        ,fne(i,j,k),fvr(i,j,k),fvt(i,j,k),fvz(i,j,k)
c     &        ,fvtrb(i,j,k)
40	    continue
50 	  continue
60	continue
70 	FORMAT(3i3,8E10.3)
c	to write the model into the file readable by this code (smod2)
c	just uncomment the lines below
c        open(10,file='shellspec.mod',status='unknown')
c        write(10,'(1x,3i5)')nbodf1,nbodf2,nbodf3
c        write(10,'(5e12.4)')(far(i),i=1,nbodf1)
c        write(10,'(5e12.4)')(fat(i),i=1,nbodf2)
c        write(10,'(5e12.4)')(faz(i),i=1,nbodf3)
c        do 100 i=1,nbodf1
c        do 90 j=1,nbodf2
c        do 80 k=1,nbodf3
c          write(10,'(9es11.3)')ftemp(i,j,k),fdustt(i,j,k),fdustd(i,j,k)
c     &    ,fdens(i,j,k),fne(i,j,k)
c     &    ,fvr(i,j,k),fvt(i,j,k),fvz(i,j,k),fvtrb(i,j,k)
c80      continue
c90      continue
c100     continue
c        close(10)	
	if(inebl.eq.4)then
	  write(2,110)' inebl=4= pp-disk'
	  write(2,110)' H_scale_in H_scale_out [Rsol] sound_out [km/s]'	  
          write(2,'(4es10.2)')h00nb/rsol,h01nb/rsol,ss01nb/1.d5
	  write(2,110)' Surfdens_in Surfdens_out [g/cm^2]'	  
          write(2,'(4es11.2)')si00nb,si01nb
	endif
110	format(a)	
	return
	end
c-----------------------------------------------------------------------
        subroutine smod2(ndimf1,ndimf2,ndimf3,nbodf1,nbodf2,nbodf3
     &  ,far,fat,faz,ftemp,fdens,fne,fvr,fvt,fvz,fvtrb,fdustd,fdustt)
        implicit double precision (a-h,o-z)
	dimension far(ndimf1),fat(ndimf2),faz(ndimf3)
        dimension fdustd(ndimf1,ndimf2,ndimf3)
        dimension fdustt(ndimf1,ndimf2,ndimf3)
        dimension ftemp(ndimf1,ndimf2,ndimf3)
        dimension fdens(ndimf1,ndimf2,ndimf3),fne(ndimf1,ndimf2,ndimf3)
        dimension fvr(ndimf1,ndimf2,ndimf3),fvt(ndimf1,ndimf2,ndimf3)
        dimension fvz(ndimf1,ndimf2,ndimf3),fvtrb(ndimf1,ndimf2,ndimf3)
c	reads the input model of the shell from the file
c	nbodf1,nbodf2,nbodf3 -number of x,y,z grid points
c	far,fat,faz -define the x,y,z grid points [cm]
c	ftemp, fdens - gas temperature [K] and density [g/cm^3] 
c	fne -electron number density [cm^-3]
c       fdustd, fdustt -dust density [g/cm^3] and temperature [K]
c	fvr,fvt,fvz,fvtrb -x,y,z components of the velocity field [cm/s]
c	fvtrb -turbulence [cm/s]
	read(10,*)nbodf1,nbodf2,nbodf3
	if(nbodf1.gt.ndimf1.or.nbodf2.gt.ndimf2.or.nbodf3.gt.ndimf3)then
	  write(*,*)' error: space dimension exceeded, stop'
	  goto 100
	endif
	read(10,*)(far(i),i=1,nbodf1)
	read(10,*)(fat(i),i=1,nbodf2)
	read(10,*)(faz(i),i=1,nbodf3)
	do i=1,nbodf1
	do j=1,nbodf2
	do k=1,nbodf3
	  read(10,*)ftemp(i,j,k),fdustt(i,j,k),fdustd(i,j,k)
     &	  ,fdens(i,j,k),fne(i,j,k)
     &    ,fvr(i,j,k),fvt(i,j,k),fvz(i,j,k),fvtrb(i,j,k)
        enddo
        enddo
        enddo
100	return
	end
c-----------------------------------------------------------------------	
              subroutine star(istar,rstar,tstar,xstar,ystar,zstar
     &        ,vxst,vyst,vzst,rosup,rotemp,trmtst
     &        ,irrst,htst,htsta,htstb,albst
     &        ,icomp,xcp,ycp,zcp,rcp,tempcp
     &        ,idifst,vrotst,drotst,hst
     &        ,ispst,xspst,yspst,zspst,aspst,tspst
     &        ,dunt1,ane0,er,er2,porbst
     &        ,far,fat,faz,inst
     &        ,ftemp,fdens,fne
     &        ,fdustd,fdustt
     &        ,fvr,fvt,fvz,fvtrb)
              implicit double precision (a-h,o-z)
              dimension trmtst(3,3)
              pi=3.1415926535897931d0
              if(istar.eq.1)then
                if(er.lt.rstar)then
c			new coordinates aligned with the rot.axis		
		  ffx=trmtst(1,1)*far+trmtst(1,2)*fat
     & 		  +trmtst(1,3)*faz
		  ffy=trmtst(2,1)*far+trmtst(2,2)*fat
     &		  +trmtst(2,3)*faz
		  ffz=trmtst(3,1)*far+trmtst(3,2)*fat
     &		  +trmtst(3,3)*faz
                  if(idifst.eq.1)then
	            omgste=vrotst/rstar
		    omgstp=drotst*omgste
		    sinlat=ffz**2/(ffx**2+ffy**2+ffz**2)
		    omgst=omgste-(omgste-omgstp)*sinlat
		  elseif(idifst.eq.2)then
		    omgste=vrotst/rstar
		    omgstp=drotst*omgste
		    sinlat=ffz**2/(ffx**2+ffy**2+ffz**2)
		    if(sinlat.lt.hst**2)then
		      omgst=omgste
		    else
		      omgst=omgstp
		    endif  
		  else
		    omgst=vrotst/rstar
		  endif  
	          ftemp=tstar
		  fdustt=tstar
                  if(er2.le.0.d0)goto 35
c                 correct ftemp for irradiation effect                  
	          if(irrst.eq.1.and.icomp.gt.0)then
	            farcp=far-xcp
	            fatcp=fat-ycp
	            fazcp=faz-zcp
		    cosir=far*xcp+fat*ycp+faz*zcp
		    cosir=cosir/dsqrt(xcp**2+ycp**2+zcp**2)/er
     		    coslat=(ffx**2+ffy**2)/(ffx**2+ffy**2+ffz**2)
		    coslat=dsqrt(coslat)
    		    htstb=4.d0*(1.d0-htsta)/pi
    		    fnlat=htsta+htstb*coslat
    		    pom=rcp**2/(xcp**2+ycp**2+zcp**2)	
                    t04=0.25d0*htst*(1.d0-albst)*pom*tempcp**4
		    if(cosir.gt.0.d0)then
c		      dayside
		      pom=rcp**2/(farcp**2+fatcp**2+fazcp**2)	
	              firr=pom*tempcp**4*cosir
	              tirr4=(1.d0-htst)*(1.d0-albst)*firr
	              ftemp=(tirr4+t04*fnlat+tstar**4)**0.25d0
	            else  
c		      nightside	            
                      ftemp=(t04*fnlat+tstar**4)**0.25d0
		    endif
	          endif
c                 add a spot
                  if(ispst.eq.1)then
		    fxspst=trmtst(1,1)*xspst+trmtst(1,2)*yspst
     & 		    +trmtst(1,3)*zspst
		    fyspst=trmtst(2,1)*xspst+trmtst(2,2)*yspst
     &		    +trmtst(2,3)*zspst
		    fzspst=trmtst(3,1)*xspst+trmtst(3,2)*yspst
     &		    +trmtst(3,3)*zspst
                    cspst=fxspst*ffx+fyspst*ffy+fzspst*ffz
                    cspst=cspst/er
c                   inside the spot                    
                    if(cspst.gt.aspst)then
                      ftemp=ftemp*tspst
                    endif
                  endif
35	          fdustd=0.d0
	          fdens=dunt1
	          fne=ane0
	          fvr=omgst*(ystar*faz-zstar*fat)+vxst
	          fvt=omgst*(zstar*far-xstar*faz)+vyst
	          fvz=omgst*(xstar*fat-ystar*far)+vzst
	          fvtrb=0.d0
	          inst=1
                endif  
              elseif(istar.eq.2.or.istar.eq.3)then
                if(dabs(faz).lt.rosup)then
                  omgst=2.d0*pi/porbst
	          ftemp=rotemp
	          fdustt=rotemp
	          fdustd=0.d0
	          fdens=dunt1
	          fne=ane0
	          fvr=omgst*(ystar*faz-zstar*fat)+vxst
	          fvt=omgst*(zstar*far-xstar*faz)+vyst
	          fvz=omgst*(xstar*fat-ystar*far)+vzst
	          fvtrb=0.d0
	          inst=1
                endif  
	      endif	      
	      return
	      end
c-----------------------------------------------------------------------	
	subroutine envelope(ienv,rosue,hen,xen,yen,zen
     &  ,vxen,vyen,vzen,omgen,inen
     &  ,tempen,dstden,dstten,densen,aneen,vtrben
     &  ,far,fat,faz
     &  ,ftemp,fdens,fne
     &  ,fdustd,fdustt
     &  ,fvr,fvt,fvz,fvtrb)
c	envelope	      	     
     	implicit double precision (a-h,o-z)
c     	include 'param.inc'
c	dimension rosue(ndimf1,ndimf2)
        if(ienv.eq.2.or.ienv.eq.3)then
          if(dabs(faz).lt.rosue.and.
     &    dabs(faz).lt.hen)then
	    ftemp=tempen
	    fdustt=dstten
	    fdustd=dstden
	    fdens=densen
	    fne=aneen
	    fvr=omgen*(yen*faz-zen*fat)+vxen
	    fvt=omgen*(zen*far-xen*faz)+vyen
	    fvz=omgen*(xen*fat-yen*far)+vzen
	    fvtrb=vtrben
	    inen=1
          endif  
	endif	
	return
	end      
c-----------------------------------------------------------------------
	subroutine stream(ism,dxsm,dysm,dzsm,x1sm,y1sm,z1sm
     &  ,r1sm,r2sm,v1sm,v2sm,vxsm,vysm,vzsm
     &  ,omgsm,xsm,ysm,zsm,dsm
     &  ,vtrbsm,tempsm,dstdsm,dsttsm,denssm,anesm,edensm
     &  ,far,fat,faz,insm
     &  ,ftemp,fdens,fne
     &  ,fdustd,fdustt
     &  ,fvr,fvt,fvz,fvtrb)
c	stream
c	dxsm,dysm,dzsm,dsm -are unit vectors and length of the stream
c	notice that although stream radius changes the streamlines 
c       do not flare but are parallel in this approximation
	implicit double precision (a-h,o-z)	
c	rsol=6.9599d10
	rsol=6.95508d10
	if(ism.eq.1)then
c         tsm is projection of the point on the stream vector
	  tsm=dxsm*(far-x1sm)+dysm*(fat-y1sm)+dzsm*(faz-z1sm)
c	  point1/2 are at the beginning/end of stream	  
c	  point 3 is at the distance t on the axis
	  xsm3=dxsm*tsm+x1sm
	  ysm3=dysm*tsm+y1sm
	  zsm3=dzsm*tsm+z1sm
	  dxsm3=xsm3-far
	  dysm3=ysm3-fat
	  dzsm3=zsm3-faz
	  er2sm=dxsm3*dxsm3+dysm3*dysm3+dzsm3*dzsm3
	  rsm=(r2sm-r1sm)*tsm/dsm+r1sm
	  if(tsm.gt.0.d0.and.tsm.lt.dsm.and.er2sm.lt.rsm*rsm)then
c	    vsm \sim dsqrt(tsm)+v1sm     or
c           vsm \sim (2/r-1/a)          might be more physical
	    vsm=(v2sm-v1sm)*tsm/dsm+v1sm
	    fact=v1sm/vsm*r1sm**2/rsm**2*dexp(tsm/rsol*edensm)
	    ftemp=tempsm
	    fdustt=dsttsm
	    fdustd=dstdsm*fact
	    fdens=denssm*fact
	    fne=anesm*fact
c		internal velocity + rotational drag + net velocity
	    fvr=vsm*dxsm+omgsm*(ysm*faz-zsm*fat)+vxsm
	    fvt=vsm*dysm+omgsm*(zsm*far-xsm*faz)+vysm
	    fvz=vsm*dzsm+omgsm*(xsm*fat-ysm*far)+vzsm
	    fvtrb=vtrbsm
            insm=1
	  endif
        endif
	return
	end
c-----------------------------------------------------------------------
	subroutine ring(iring,xrg,yrg,zrg
     &  ,a1rg,a2rg,b1rg,b2rg,dr1rg,dr2rg,rrg,vrg,itrg
     &  ,vxrg,vyrg,vzrg,xpolrg,ypolrg,zpolrg,trmtrg,dcut1
     &  ,vtrbrg,temprg,densrg,anerg
     &  ,dstdrg,dst2rg,edenrg,ede2rg,dsttrg
     &  ,far,fat,faz,inrg
     &  ,ftemp,fdens,fne
     &  ,fdustd,fdustt
     &  ,fvr,fvt,fvz,fvtrb)
c	ring
c	b1rg, b2rg -beginning, end of arc
c	a1rg, a2rg -vertical half width at beginning, end
c	dr1rg, dr2rg -horizontal half thickness the ring at beg., end
c	trmtrg is transformation matrix
	implicit double precision (a-h,o-z)	
	dimension trmtrg(3,3)
	pi=3.1415926535897931d0
	grav=6.673d-8
	if(iring.gt.0)then	
	  dxrg=far-xrg
	  dyrg=fat-yrg
	  dzrg=faz-zrg
c	  rotation of the coordinates to align with the ring
	  ffx=trmtrg(1,1)*dxrg+trmtrg(1,2)*dyrg+trmtrg(1,3)*dzrg
	  ffy=trmtrg(2,1)*dxrg+trmtrg(2,2)*dyrg+trmtrg(2,3)*dzrg
	  ffz=trmtrg(3,1)*dxrg+trmtrg(3,2)*dyrg+trmtrg(3,3)*dzrg
	  ffxyrg=dsqrt(ffx*ffx+ffy*ffy)
c         you may try angle=dacos(ffy)
          angle=dacos(ffx/ffxyrg)
          if(ffy.lt.0.d0)angle=2.d0*pi-angle
          if((angle.ge.b1rg.and.angle.le.b2rg).or.
     &    (angle.le.b1rg.and.angle.ge.b2rg))then 
c	    within the arc
	    ffzrg=dsqrt(ffz*ffz)
	    trg=angle-b1rg
	    arg=(a2rg-a1rg)/(b2rg-b1rg)*trg+a1rg
	    if(ffzrg.lt.arg)then
c             within the vertical limit	    
	      drrg=(dr2rg-dr1rg)/(b2rg-b1rg)*trg+dr1rg
	      if(drrg.lt.rrg.and.drrg.gt.0.d0)then
	        if(ffxyrg.lt.rrg+drrg.and.ffxyrg.gt.rrg-drrg)then
c		  within the radial limit	        
	          ftemp=temprg
	          fdustt=dsttrg
		  if(densrg.lt.dcut1)then
		    trg=dsqrt(trg*trg)
		    if(itrg.eq.1)then
	             trgpi=trg/pi+1.d0
	             pomrg=dstdrg*trgpi**edenrg
	             pomrg=pomrg+dst2rg*trgpi**ede2rg
		     fdustd=pomrg*a1rg/arg*dr1rg/drrg
                     fdens=densrg*trgpi**edenrg
                     fdens=fdens*a1rg/arg*dr1rg/drrg
	             fne=anerg*trgpi**edenrg
	             fne=fne*a1rg/arg*dr1rg/drrg
	            else
	             trgpi=trg/pi 
	             pomrg=dstdrg*dexp(trgpi*edenrg)
	             pomrg=pomrg+dst2rg*dexp(trgpi*ede2rg)
		     fdustd=pomrg*a1rg/arg*dr1rg/drrg
                     fdens=densrg*dexp(trgpi*edenrg)
                     fdens=fdens*a1rg/arg*dr1rg/drrg
	             fne=anerg*dexp(trgpi*edenrg)
	             fne=fne*a1rg/arg*dr1rg/drrg
		    endif
	          else
	            fdustd=dstdrg
	            fdens=densrg
	            fne=anerg
	          endif  
	          fvr=vrg*(ypolrg*dzrg-zpolrg*dyrg)+vxrg
	          fvt=vrg*(zpolrg*dxrg-xpolrg*dzrg)+vyrg
	          fvz=vrg*(xpolrg*dyrg-ypolrg*dxrg)+vzrg
	          fvtrb=vtrbrg
	          inrg=1
c                 within the radial limit	          
	        endif
	      endif
c             within the vertical limit
	    endif      
c           within the arc	    
	  endif
	endif	
	return
	end
c-----------------------------------------------------------------------
	subroutine ufo(iufo,xufo,yufo,zufo
     &  ,aufo,rinuf,routuf,emuf,ruf,xuf,yuf,zuf
     &  ,vxuf,vyuf,vzuf,trmtuf
     &  ,tempuf,densuf,aneuf,vtrbuf,edenuf,ituf,etmpuf
     &  ,dstduf,dsttuf
     &  ,far,fat,faz,inuf
     &  ,ftemp,fdens,fne
     &  ,fdustd,fdustt
     &  ,fvr,fvt,fvz,fvtrb)
c	disk=ufo
c	trmtuf is transformation matrix
	implicit double precision (a-h,o-z)	
	dimension trmtuf(3,3)
	pi=3.1415926535897931d0
	grav=6.673d-8
        dxuf=far-xuf
	dyuf=fat-yuf
	dzuf=faz-zuf
	druf=dsqrt(dxuf*dxuf+dyuf*dyuf+dzuf*dzuf)
	if(iufo.eq.1)then
c         flared disk	      
c	  ffx=trmtuf(1,1)*dxuf+trmtuf(1,2)*dyuf+trmtuf(1,3)*dzuf
c	  ffy=trmtuf(2,1)*dxuf+trmtuf(2,2)*dyuf+trmtuf(2,3)*dzuf
	  ffz=trmtuf(3,1)*dxuf+trmtuf(3,2)*dyuf+trmtuf(3,3)*dzuf
	  angle=dabs(ffz)/druf
	  angle=dasin(angle)
          if(dabs(angle).lt.aufo
     &    .and.druf.gt.rinuf.and.druf.lt.routuf)then
	    vuf=dsqrt(grav*emuf/druf)/druf
	    if(ituf.eq.1)then
	      ftemp=tempuf
	      fdustt=dsttuf
	    elseif(ituf.eq.2)then
	      ratio=ruf/druf
	      pom=dsqrt(dsqrt(1.d0-dsqrt(ratio)))
	      ftemp=tempuf*ratio**0.75d0*pom
	      fdustt=dsttuf*ratio**0.75d0*pom
	    else
	      ratio=druf/rinuf
	      ftemp=tempuf*ratio**etmpuf
	      fdustt=dsttuf*ratio**etmpuf  	      
	    endif
	    ratio=druf/rinuf
	    fdens=densuf*ratio**edenuf
	    fne=aneuf*ratio**edenuf
	    fdustd=dstduf*ratio**edenuf
	    fvr=vuf*(yufo*dzuf-zufo*dyuf)+vxuf
	    fvt=vuf*(zufo*dxuf-xufo*dzuf)+vyuf
	    fvz=vuf*(xufo*dyuf-yufo*dxuf)+vzuf
	    fvtrb=vtrbuf
	    inuf=1
	  endif
	elseif(iufo.eq.2)then	
c         slab	      
c	  ffx=trmtuf(1,1)*dxuf+trmtuf(1,2)*dyuf+trmtuf(1,3)*dzuf
c	  ffy=trmtuf(2,1)*dxuf+trmtuf(2,2)*dyuf+trmtuf(2,3)*dzuf
	  ffz=trmtuf(3,1)*dxuf+trmtuf(3,2)*dyuf+trmtuf(3,3)*dzuf
          if(dabs(ffz).lt.aufo
     &    .and.druf.gt.rinuf.and.druf.lt.routuf)then
	    vuf=dsqrt(grav*emuf/druf)/druf
	    if(ituf.eq.1)then
	      ftemp=tempuf
	      fdustt=dsttuf
	    elseif(ituf.eq.2)then
	      ratio=ruf/druf
	      pom=dsqrt(dsqrt(1.d0-dsqrt(ratio)))
	      ftemp=tempuf*ratio**0.75d0*pom
	      fdustt=dsttuf*ratio**0.75d0*pom
	    else
	      ratio=druf/rinuf
	      ftemp=tempuf*ratio**etmpuf
	      fdustt=dsttuf*ratio**etmpuf  
	    endif
	    ratio=druf/rinuf
	    fdens=densuf*ratio**edenuf
	    fne=aneuf*ratio**edenuf
	    fdustd=dstduf*ratio**edenuf
	    fvr=vuf*(yufo*dzuf-zufo*dyuf)+vxuf
	    fvt=vuf*(zufo*dxuf-xufo*dzuf)+vyuf
	    fvz=vuf*(xufo*dyuf-yufo*dxuf)+vzuf
	    fvtrb=vtrbuf
	    inuf=1
	  endif
	elseif(iufo.eq.3)then	
c	  rotation ellipsoid	      
c	  rotation of the coordinates to align with the disc
	  ffx=trmtuf(1,1)*dxuf+trmtuf(1,2)*dyuf+trmtuf(1,3)*dzuf
	  ffy=trmtuf(2,1)*dxuf+trmtuf(2,2)*dyuf+trmtuf(2,3)*dzuf
	  ffz=trmtuf(3,1)*dxuf+trmtuf(3,2)*dyuf+trmtuf(3,3)*dzuf
	  er2uf=ffx*ffx/routuf/routuf+ffy*ffy/routuf/routuf
          er2uf=er2uf+ffz*ffz/aufo/aufo
          if(er2uf.lt.1.d0.and.druf.gt.rinuf)then
	    vuf=dsqrt(grav*emuf/druf)/druf
	    if(ituf.eq.1)then
	      ftemp=tempuf
	      fdustt=dsttuf
	    elseif(ituf.eq.2)then
	      ratio=ruf/druf
	      pom=dsqrt(dsqrt(1.d0-dsqrt(ratio)))
	      ftemp=tempuf*ratio**0.75d0*pom
	      fdustt=dsttuf*ratio**0.75d0*pom
	    else
	      ratio=druf/rinuf
	      ftemp=tempuf*ratio**etmpuf
	      fdustt=dsttuf*ratio**etmpuf
	    endif
	    ratio=druf/rinuf
	    fdens=densuf*ratio**edenuf
	    fne=aneuf*ratio**edenuf
	    fdustd=dstduf*ratio**edenuf
	    fvr=vuf*(yufo*dzuf-zufo*dyuf)+vxuf
	    fvt=vuf*(zufo*dxuf-xufo*dzuf)+vyuf
	    fvz=vuf*(xufo*dyuf-yufo*dxuf)+vzuf
	    fvtrb=vtrbuf
	    inuf=1
	  endif
	endif	                
	return
	end
c-----------------------------------------------------------------------??-
	subroutine nebula(iufo,xufo,yufo,zufo
     &  ,aufo,rinuf,routuf,emuf,ruf
     &  ,hinvnb,tinvnb,hwindnb,ndennb,denxnb,denznb
     &  ,vxuf,vyuf,vzuf,trmtuf
     &  ,tempuf,densuf,aneuf,vtrbuf,edenuf,ituf,etmpuf
     &  ,dstduf,dsttuf
     &  ,far,fat,faz,er,inuf
     &  ,hsca00,hsca01,sig00,sig01,soun01     
     &  ,ftemp,fdens,fne
     &  ,fdustd,fdustt
     &  ,fvr,fvt,fvz,fvtrb)
c	output: hsca00,hsca01,sig00,sig01,soun01,inuf
c	disk=ufo
c	trmtuf is transformation matrix
	implicit double precision (a-h,o-z)	
	include 'param.inc'
	dimension trmtuf(3,3),denxnb(mstarx),denznb(mstarx)
	pi=3.1415926535897931d0
	grav=6.673d-8
	bol=1.3806503d-16
	hjed=1.66053873d-24
	if(iufo.eq.4)then	
c	  protoplanetary disk
c	  rotation of the coordinates to align with the disc		
	  ffx=trmtuf(1,1)*far+trmtuf(1,2)*fat+trmtuf(1,3)*faz
	  ffy=trmtuf(2,1)*far+trmtuf(2,2)*fat+trmtuf(2,3)*faz
	  ffz=trmtuf(3,1)*far+trmtuf(3,2)*fat+trmtuf(3,3)*faz
          erxy=dsqrt(ffx*ffx+ffy*ffy)
          erz=dsqrt(ffz*ffz)
c	  define mid plane temperature temp0 depending on ituf          
          if(ituf.eq.1)then
	    temp0=tempuf
	  elseif(ituf.eq.2)then
	    ratio=ruf/erxy
	    pom=dsqrt(dsqrt(1.d0-dsqrt(ratio)))
	    temp0=tempuf*ratio**0.75d0*pom
	  else
	    ratio=erxy/rinuf
	    temp0=tempuf*ratio**etmpuf
	  endif
c         monoatommic=5/3, diatomic=7/5, m(H2)=2, m(He)=4
c         gamma=5.d0/3.d0
c         gamma=7.d0/5.d0
          gamma=1.d0
          em=(45.d0*2.d0+10.d0*4.d0)/(45.d0+10.d0)*hjed
	  sound=dsqrt(gamma*bol*temp0/em)        
          vuf0=dsqrt(grav*emuf/erxy)
c         scale height H(r)          
          hscale=sound/vuf0*erxy
c??	  z=hscale*4 corresponds to 10^-4 drop in density          
c         z=hscale*7 corresponds to 10^-10 drop in density
c         erz.lt.hscale*6.d0 may be required to reach wind (1d-8rho0)
c	  soun* -sound speed, hsca* -disk scale hight
c         *00 -quantity at rinuf, *01 -quantity at routuf
c	  you may slightly speed up if you take the calculations of
c         *00, *01 quantities out of this subroutine
          if(erxy.gt.rinuf.and.erxy.lt.routuf.and.erz.lt.hscale*aufo)
     &    then
	    if(ituf.eq.1)then
c		constant temperatures	    
	      ftem00=tempuf
	      ftem01=tempuf	
	      ftemp=tempuf
	      fdustt=dsttuf
	    elseif(ituf.eq.2)then
c		radially dependent temperatures	    
	      ratio=ruf/rinuf
	      pom=dsqrt(dsqrt(1.d0-dsqrt(ratio)))
	      ftem00=tempuf*ratio**0.75d0*pom
	      ratio=ruf/routuf
	      pom=dsqrt(dsqrt(1.d0-dsqrt(ratio)))
	      ftem01=tempuf*ratio**0.75d0*pom
	      ratio=ruf/erxy
	      pom=dsqrt(dsqrt(1.d0-dsqrt(ratio)))
	      ftemp=tempuf*ratio**0.75d0*pom
	      fdustt=dsttuf*ratio**0.75d0*pom
	    else
c	      radial & vertical temperature dependence	    
	      ftem00=tempuf
	      ratio=routuf/rinuf
	      ftem01=tempuf*ratio**etmpuf
	      ratio=erxy/rinuf
c	      mid-plane temperature	      
	      ftemp0=tempuf*ratio**etmpuf
c             gas temperature inversion	      
	      if(erz.gt.hscale*hinvnb)then	
c	        ftemp=2.d0*ftemp0/hscale*(erz-hscale*5.5d0)+ftemp0
	        ftemp=tinvnb*ftemp0
	      else
	        ftemp=ftemp0
	      endif  
	      fdustt=dsttuf*ratio**etmpuf
	    endif
	    soun00=dsqrt(gamma*bol*ftem00/em)
	    soun01=dsqrt(gamma*bol*ftem01/em)
	    vuf00=dsqrt(grav*emuf/rinuf)
	    vuf01=dsqrt(grav*emuf/routuf)
	    hsca00=soun00/vuf00*rinuf
	    hsca01=soun01/vuf01*routuf
	    sig00=densuf*hsca00*dsqrt(2.d0*pi)
	    sig01=sig00*(routuf/rinuf)**edenuf
	    ratio=(erxy/rinuf)**edenuf
            sig0=sig00*ratio
            dens0=sig0/hscale/dsqrt(2.d0*pi)
c	    wind region in densities
	    if(ndennb.eq.0)then
c             gaussian-like wind profile	    
              if(erz.lt.hscale*hwindnb)then
	        fdens=dens0*dexp(-erz**2/hscale**2/2.d0)
	      else
	        fdens=dens0*dexp(-hwindnb**2/2.d0)
	      endif  
	    else
c             precalculated wind profile	    
	      hh=hsca01/5.374d10
	      densw0=dens0*hh
	      call intrp(denxnb,denznb,ndennb,erz,fdens)
	      fdens=fdens/denznb(1)*densw0
	    endif  
	    fne=aneuf/densuf*fdens
	    fdustd=dstduf/densuf*fdens
c??	    write(19,*)
c	    write(19,'(6es10.2)')erxy,erz,rinuf,hscale,hsca00
c	    write(19,'(6es10.2)')dens0,sig0,sig00
c	    write(19,'(6es10.2)')fdens,fne,fdustd
	    vuf=vuf0/erxy
	    fvr=vuf*(yufo*faz-zufo*fat)+vxuf
	    fvt=vuf*(zufo*far-xufo*faz)+vyuf
	    fvz=vuf*(xufo*fat-yufo*far)+vzuf
	    fvtrb=vtrbuf
	    inuf=1
	  endif
	endif	                
	return
	end
c-----------------------------------------------------------------------
	subroutine sjet(ijet,ajet,rinjt,routjt,xjet,yjet,zjet
     &  ,vjt,vxjt,vyjt,vzjt,vtrbjt,tempjt,densjt,anejt
     &  ,dstdjt,dsttjt
     &  ,far,fat,faz,unjet,er,injt
     &  ,ftemp,fdens,fne
     &  ,fdustd,fdustt
     &  ,fvr,fvt,fvz,fvtrb)
c	jet with one or 2 cones
c	streamlines diverge
	implicit double precision (a-h,o-z)	
	pi=3.1415926535897931d0
	if(ijet.eq.1)then
	  angle=(xjet*far+yjet*fat+zjet*faz)/unjet/er
	  angle=dacos(angle)	
          if(dabs(angle-0.d0).lt.ajet
     &    .and.er.ge.rinjt.and.er.lt.routjt)then
            rr2=(rinjt/er)**2
	    ftemp=tempjt
	    fdustt=dsttjt
	    fdustd=dstdjt*rr2
	    fdens=densjt*rr2
	    fne=anejt*rr2
	    fvr=vjt*far/er+vxjt
	    fvt=vjt*fat/er+vyjt
	    fvz=vjt*faz/er+vzjt
	    fvtrb=vtrbjt
	    injt=1
	  endif
	elseif(ijet.eq.2)then
	  angle=(xjet*far+yjet*fat+zjet*faz)/unjet/er
	  angle=dacos(angle)
          if((dabs(angle-0.d0).lt.ajet.or.dabs(angle-pi).lt.ajet)
     &    .and.er.ge.rinjt.and.er.lt.routjt)then
            rr2=(rinjt/er)**2
	    ftemp=tempjt
	    fdustt=dsttjt
	    fdustd=dstdjt*rr2
	    fdens=densjt*rr2
	    fne=anejt*rr2
	    fvr=vjt*far/er+vxjt
	    fvt=vjt*fat/er+vyjt
	    fvz=vjt*faz/er+vzjt
	    fvtrb=vtrbjt
	    injt=1
	  endif
	endif	
	return
	end
c-----------------------------------------------------------------------
	subroutine shell(ishell,rinsh,routsh,rcsh
     &  ,vsh,vxsh,vysh,vzsh,vtrbsh
     &  ,tempsh,dstdsh,dsttsh,denssh,anesh,evelsh
     &  ,far,fat,faz,er,insh
     &  ,ftemp,fdens,fne
     &  ,fdustd,fdustt
     &  ,fvr,fvt,fvz,fvtrb)
c	shell
	implicit double precision (a-h,o-z)	
        if(ishell.eq.1.and.er.ge.rinsh.and.er.lt.routsh)then
          ftemp=tempsh
          fdustt=dsttsh
          fdustd=dstdsh
          fdens=denssh
          fne=anesh   
          fvr=vsh*far/er+vxsh
          fvt=vsh*fat/er+vysh
          fvz=vsh*faz/er+vzsh
          fvtrb=vtrbsh
          insh=1
        elseif(ishell.eq.2.and.er.ge.rinsh.and.er.lt.routsh)then
	  vvsh=(er/rinsh)**(-evelsh)
	  vshr=vsh/vvsh
	  rr2vv=rinsh*rinsh/(er*er)*vvsh
	  fvr=vshr*far/er+vxsh
	  fvt=vshr*fat/er+vysh
	  fvz=vshr*faz/er+vzsh
	  fvtrb=vtrbsh
	  ftemp=tempsh
	  fdustt=dsttsh
	  fdustd=dstdsh*rr2vv
	  fdens=denssh*rr2vv
	  fne=anesh*rr2vv
	  insh=1
	elseif(ishell.eq.3.and.er.ge.rinsh.and.er.lt.routsh)then
	  vshr=vsh*(1.d0-rcsh/er)**evelsh
	  vshin=vsh*(1.d0-rcsh/rinsh)**evelsh
	  rr2vv=rinsh*rinsh/(er*er)*vshin/vshr
	  fvr=vshr*far/er+vxsh
	  fvt=vshr*fat/er+vysh
	  fvz=vshr*faz/er+vzsh
	  fvtrb=vtrbsh
	  ftemp=tempsh
	  fdustt=dsttsh
	  fdustd=dstdsh*rr2vv
	  fdens=denssh*rr2vv
	  fne=anesh*rr2vv
	  insh=1
	endif	  
	return
	end
c-----------------------------------------------------------------------
	subroutine deflsg(ndim1,ndim2,ndim3,nbod1,nbod2,nboda,nbodb
     &  ,rmdx1,rmdx2,rmdy1,rmdy2,rmdz1,rmdz2,rmdz3,rmdz4
     &  ,gainx,gainy,gainz,ar,at,az)
c       definition of the line of sight grid
c	it allows to define/merge two grids along the z axis
c	gain?=1 grid is equidistant
c       gain?>1. step increases geometrically from the center 
c          of the interval to the left and to the right
        implicit double precision (a-h,o-z)
        dimension ar(ndim1),at(ndim2),az(ndim3)
c        rsol=6.9599d10
	rsol=6.95508d10
c	geom. sequence assumes that nbod1,nbod2,nboda,nbodb are odd        
c		x  
   	if(gainx.eq.1.d0)then
          r1=rmdx1
 	  rn=rmdx2 	
          rstep=(rn-r1)/dble(nbod1-1)
          do i=1,nbod1
            ar(i)=r1+rstep*dble(i-1)
          enddo
	else
   	  n2=nbod1/2
   	  st1=0.5d0*(rmdx2-rmdx1)*(1.d0-gainx)/(1.d0-gainx**n2)
   	  ar(n2+1)=0.5d0*(rmdx1+rmdx2)   	  
   	  do i=1,n2  
   	    ar(n2+1+i)=st1*(1.d0-gainx**i)/(1.d0-gainx)+ar(n2+1)
   	    ar(n2+1-i)=-st1*(1.d0-gainx**i)/(1.d0-gainx)+ar(n2+1)
 	  enddo
 	endif
c		y
        if(gainy.eq.1.d0)then
          t1=rmdy1
          tn=rmdy2
          tstep=(tn-t1)/dble(nbod2-1)
          do i=1,nbod2
            at(i)=t1+tstep*dble(i-1)
          enddo
	else
          n2=nbod2/2
          st1=0.5d0*(rmdy2-rmdy1)*(1.d0-gainy)/(1.d0-gainy**n2)
          at(n2+1)=0.5d0*(rmdy1+rmdy2)
          do i=1,n2  
            at(n2+1+i)=st1*(1.d0-gainy**i)/(1.d0-gainy)+at(n2+1)
            at(n2+1-i)=-st1*(1.d0-gainy**i)/(1.d0-gainy)+at(n2+1)
          enddo
	endif
c		z	
        if(gainz.eq.1.d0)then
          z1=rmdz1
          zn=rmdz2
          zstep=(zn-z1)/dble(nboda-1)
          do i=1,nboda
            az(i)=z1+zstep*dble(i-1)       
          enddo
	  if(nbodb.gt.2)then
	    z1=rmdz3
	    zn=rmdz4
	    zstep=(zn-z1)/dble(nbodb-1) 
            do i=1,nbodb
              az(i+nboda)=z1+zstep*dble(i-1)
            enddo
          endif
        else
          n2=nboda/2
          st1=0.5d0*(rmdz2-rmdz1)*(1.d0-gainz)/(1.d0-gainz**n2)
          az(n2+1)=0.5d0*(rmdz1+rmdz2)
          do i=1,n2  
            az(n2+1+i)=st1*(1.d0-gainz**i)/(1.d0-gainz)+az(n2+1)
            az(n2+1-i)=-st1*(1.d0-gainz**i)/(1.d0-gainz)+az(n2+1)
          enddo
	  if(nbodb.gt.2)then
            n2b=nbodb/2
            st1=0.5d0*(rmdz4-rmdz3)*(1.d0-gainz)/(1.d0-gainz**n2)
            az(n2b+1+nboda)=0.5d0*(rmdz3+rmdz4)
            do i=1,n2b  
              az(n2b+1+i+nboda)=st1*(1.d0-gainz**i)/(1.d0-gainz)
              az(n2b+1+i+nboda)=az(n2b+1+i+nboda)+az(n2b+1+nboda)
              az(n2b+1-i+nboda)=-st1*(1.d0-gainz**i)/(1.d0-gainz)
              az(n2b+1-i+nboda)=az(n2b+1-i+nboda)+az(n2b+1+nboda)
            enddo
          endif  
        endif
	return
	end
c-----------------------------------------------------------------------
	subroutine defbfg(ndim1,ndim2,ndim3,nboda,nbod2,nbod3,nbodb
     &  ,rmdx1,rmdx2,rmdy1,rmdy2,rmdz1,rmdz2,rmdx3,rmdx4
     &  ,gainx,gainy,gainz,ar,at,az)
c       definition of the body frozen grid
c	it allows to define/merge two grids along the x axis
c	gain?=1 grid is equidistant
c       gain?>1. step increases geometrically from the center 
c          of the interval to the left and to the right
        implicit double precision (a-h,o-z)
        dimension ar(ndim1),at(ndim2),az(ndim3)
c        rsol=6.9599d10
	rsol=6.95508d10
c	geom. sequence assumes that nbod2,nbod3,nboda,nbodb are odd        
c		x
   	if(gainx.eq.1.d0)then
          r1=rmdx1
 	  rn=rmdx2 	
          rstep=(rn-r1)/dble(nboda-1)
          do i=1,nboda
            ar(i)=r1+rstep*dble(i-1)
          enddo
          if(nbodb.gt.2)then
            r1=rmdx3
 	    rn=rmdx4 	
            rstep=(rn-r1)/dble(nbodb-1)
            do i=1,nbodb
              ar(i+nboda)=r1+rstep*dble(i-1)
            enddo           
          endif  
	else
   	  n2=nboda/2
   	  st1=0.5d0*(rmdx2-rmdx1)*(1.d0-gainx)/(1.d0-gainx**n2)
   	  ar(n2+1)=0.5d0*(rmdx1+rmdx2)   	  
   	  do i=1,n2  
   	    ar(n2+1+i)=st1*(1.d0-gainx**i)/(1.d0-gainx)+ar(n2+1)
   	    ar(n2+1-i)=-st1*(1.d0-gainx**i)/(1.d0-gainx)+ar(n2+1)
 	  enddo
	  if(nbodb.gt.2)then
   	    n2b=nbodb/2
   	    st1=0.5d0*(rmdx4-rmdx3)*(1.d0-gainx)/(1.d0-gainx**n2)
   	    ar(n2b+1+nboda)=0.5d0*(rmdx3+rmdx4)   	  
   	    do i=1,n2b  
   	      ar(n2b+1+i+nboda)=st1*(1.d0-gainx**i)/(1.d0-gainx)
              ar(n2b+1+i+nboda)=ar(n2b+1+i+nboda)+ar(n2b+1+nboda)
   	      ar(n2b+1-i+nboda)=-st1*(1.d0-gainx**i)/(1.d0-gainx)
              ar(n2b+1-i+nboda)=ar(n2b+1-i+nboda)+ar(n2b+1+nboda)
 	    enddo 	  
 	  endif  
 	endif
c		y 	
        if(gainy.eq.1.d0)then
          t1=rmdy1
          tn=rmdy2
          tstep=(tn-t1)/dble(nbod2-1)
          do i=1,nbod2
            at(i)=t1+tstep*dble(i-1)
          enddo
	else
          n2=nbod2/2
          st1=0.5d0*(rmdy2-rmdy1)*(1.d0-gainy)/(1.d0-gainy**n2)
          at(n2+1)=0.5d0*(rmdy1+rmdy2)
          do i=1,n2  
            at(n2+1+i)=st1*(1.d0-gainy**i)/(1.d0-gainy)+at(n2+1)
            at(n2+1-i)=-st1*(1.d0-gainy**i)/(1.d0-gainy)+at(n2+1)
          enddo
	endif
c		z	
        if(gainz.eq.1.d0)then
            z1=rmdz1
            zn=rmdz2
            zstep=(zn-z1)/dble(nbod3-1)
            do i=1,nbod3
              az(i)=z1+zstep*dble(i-1)       
            enddo
        else
          n2=nbod3/2
          st1=0.5d0*(rmdz2-rmdz1)*(1.d0-gainz)/(1.d0-gainz**n2)
          az(n2+1)=0.5d0*(rmdz1+rmdz2)
          do i=1,n2  
            az(n2+1+i)=st1*(1.d0-gainz**i)/(1.d0-gainz)+az(n2+1)
            az(n2+1-i)=-st1*(1.d0-gainz**i)/(1.d0-gainz)+az(n2+1)
          enddo
        endif
	return
	end	
c-----------------------------------------------------------------------
      FUNCTION GAUNT(I,FR)
c	taken from the Syspec code:
c Hubeny I., Lanz T., Jeffery C.S., 1994, in Newsletter on Analysis
c of Astronomical spectra No.20, ed. C.S. Jeffery (CCP7; St. Andrews:
c St. Andrews Univ.), 30
c
C     Hydrogenic bound-free Gaunt factor for the principal quantum
C     number I and frequency FR
C
        implicit double precision (a-h,o-z)
      X=FR/2.99793E14
      GAUNT=1.
      IF(I.GT.10) GO TO 16
      GO TO (1,2,3,4,5,6,7,8,9,10),I
    1 GAUNT=1.2302628+X*(-2.9094219E-3+X*(7.3993579E-6-8.7356966E-9*X))
     *+(12.803223/X-5.5759888)/X
      GO TO 16
    2 GAUNT=1.1595421+X*(-2.0735860E-3+2.7033384E-6*X)+(-1.2709045+
     *(-2.0244141/X+2.1325684)/X)/X
      GO TO 16
    3 GAUNT=1.1450949+X*(-1.9366592E-3+2.3572356E-6*X)+(-0.55936432+
     *(-0.23387146/X+0.52471924)/X)/X
      GO TO 16
    4 GAUNT=1.1306695+X*(-1.3482273E-3+X*(-4.6949424E-6+2.3548636E-8*X))
     *+(-0.31190730+(0.19683564-5.4418565E-2/X)/X)/X
      GO TO 16
    5 GAUNT=1.1190904+X*(-1.0401085E-3+X*(-6.9943488E-6+2.8496742E-8*X))
     *+(-0.16051018+(5.5545091E-2-8.9182854E-3/X)/X)/X
      GO TO 16
    6 GAUNT=1.1168376+X*(-8.9466573E-4+X*(-8.8393133E-6+3.4696768E-8*X))
     *+(-0.13075417+(4.1921183E-2-5.5303574E-3/X)/X)/X
      GO TO 16
    7 GAUNT=1.1128632+X*(-7.4833260E-4+X*(-1.0244504E-5+3.8595771E-8*X))
     *+(-9.5441161E-2+(2.3350812E-2-2.2752881E-3/X)/X)/X
      GO TO 16
    8 GAUNT=1.1093137+X*(-6.2619148E-4+X*(-1.1342068E-5+4.1477731E-8*X))
     *+(-7.1010560E-2+(1.3298411E-2 -9.7200274E-4/X)/X)/X
      GO TO 16
    9 GAUNT=1.1078717+X*(-5.4837392E-4+X*(-1.2157943E-5+4.3796716E-8*X))
     *+(-5.6046560E-2+(8.5139736E-3-4.9576163E-4/X)/X)/X
      GO TO 16
   10 GAUNT=1.1052734+X*(-4.4341570E-4+X*(-1.3235905E-5+4.7003140E-8*X))
     *+(-4.7326370E-2+(6.1516856E-3-2.9467046E-4/X)/X)/X
   16 RETURN
      END
c-----------------------------------------------------------------------
 	FUNCTION GFREE(T,FR)
c	taken from the Syspec code:
c Hubeny I., Lanz T., Jeffery C.S., 1994, in Newsletter on Analysis
c of Astronomical spectra No.20, ed. C.S. Jeffery (CCP7; St. Andrews:
c St. Andrews Univ.), 30
C
C	Hydrogenic free-free Gaunt factor, for temperature T and
C	frequency FR
C
        implicit double precision (a-h,o-z)
 	THET=5040.4/T
 	IF(THET.LT.4.E-2) THET=4.E-2
 	X=FR/2.99793E14
 	IF(X.GT.1) GO TO 10
 	IF(X.LT.0.2) X=0.2 
 	GFREE=(1.0823+2.98E-2/THET)+(6.7E-3+1.12E-2/THET)/X
 	RETURN
10 	C1=(3.9999187E-3-7.8622889E-5/THET)/THET+1.070192
      	C2=(6.4628601E-2-6.1953813E-4/THET)/THET+2.6061249E-1
      	C3=(1.3983474E-5/THET+3.7542343E-2)/THET+5.7917786E-1
      	C4=3.4169006E-1+1.1852264E-2/THET
      	GFREE=((C4/X-C3)/X+C2)/X+C1
      	RETURN
      	END   
c-----------------------------------------------------------------------
      SUBROUTINE LOCATE(XX,N,X,J)
c	taken from Numerical recipes, 
c	search an ordered table by bisection
c	given array xx and value x returns j such that 
c	x is between xx(j),xx(j+1)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c      include 'param.inc'
c      DIMENSION XX(mstarx)
      DIMENSION XX(N)
      JL=0
      JU=N+1
10    IF(JU-JL.GT.1)THEN
        JM=(JU+JL)/2
        IF((XX(N).GE.XX(1)).EQV.(X.GE.XX(JM)))THEN
          JL=JM
        ELSE
          JU=JM
        ENDIF
        GO TO 10
      ENDIF
      if(x.eq.xx(1))then
        j=1
      elseif(x.eq.xx(n))then
       j=n-1
      else
        J=JL
      endif  
      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE LOCATE2(XX,N,X,J,k)
c	taken from Numerical recipes, 
c	search an ordered table by bisection
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      include 'param.inc'
      dimension xx(mstarx,mspecx)
c      DIMENSION XX(N,k)
      JL=0
      JU=N+1
10    IF(JU-JL.GT.1)THEN
        JM=(JU+JL)/2
        IF((XX(N,k).GT.XX(1,k)).EQV.(X.GT.XX(JM,k)))THEN
          JL=JM
        ELSE
          JU=JM
        ENDIF
      GO TO 10
      ENDIF
      J=JL
      RETURN
      END      
c-----------------------------------------------------------------------
	subroutine bcond1(iunt,darkl,cdelta,vdif,nbod,dens
     &  ,dcut1,dcut2,dcut3,dcutn,vr,vt,vz
     &  ,ari,ati,azn,dinc,alpha
     &  ,istar,rstar,vxstr,vystr,vzstr,dlst,dlst2,irrst
     &  ,icomp,rcp,xcpr,ycpr,zcpr,vxcpr,vycpr,vzcpr
     &  ,dlcp,dlcp2,irrcp,xcp,qq,iprint)
c       Identifies the last untransparent point along the ray and
c	calculates quantities which are not frequency dependent:
c	limb darkening factor if applicable, cos of the irr. angle,
c	radial velocities.
c	The limb darkening applies only to 
c	'star' and 'companion' and you must set dlst=dlcp=0.d0 if you
c	want to use their density intervals for other objects
c	(without limb darkening). 
c       input:  nbod -number of points
c       	dens - density
c       	dcut1,2,3,n -density interval boundaries
c		vr,vt,vz -x,y,z velocity along the ray
c		ari,ati -x,y coordinates of the ray (line of sight)
c		azn - set of z coordinates
c		dinc - inclination
c		alpha - phase angle
c		istar - shape of the primary (1=sphere, 
c			2=Roche detached, 3=Roche contact)
c		rstar -radius of the 'star'
c               vxstr,vystr,vzstr -velocity of the center of 'star'
c		dlst -limb darkening coefficients of the 'star'
c		icomp - shape of the secondary (1=sphere,
c			2=Roche detached)
c		rcp -radius of the 'companion'
c		xcpr,ycpr,zcpr -position of the center of 'companion'
c			in the rotated line of sight coordinates
c		vxcpr,vycpr,vzcpr -velocity of the center of 'companion'
c		dlcp -limb darkening coeficients of the 'companion'
c		xcp - scaling factor (separation of stars)
c		qq - mass ratio
c		iprint - printing switch (if =1)
c       output: 
c	  iunt -number of the last untransparent point
c	  darkl -limb darkening factor
c	  cdelta -cos(irradiation angle)*R**2/separation**2
c	  vdif -differential radial velocity between the reflecting 
c		surface and the source of light
c              
        implicit double precision (a-h,o-z)
        include 'param.inc'
        dimension dens(ndim),azn(ndim),vr(ndim),vt(ndim),vz(ndim)
        iunt=0
        darkl=1.d0
        cdelta=0.d0
        vdif=0.d0
c       icut=0
c       grmin -optional small value??
	grmin=1.d-20
        do 10 i=nbod,1,-1
          if(dens(i).gt.dcut1.and.dens(i).le.dcut2)then
            iunt=i
	    if(istar.gt.1)then
              cdinc=dcos(dinc)
              sdinc=dsin(dinc)
              calpha=dcos(alpha)
              salpha=dsin(alpha)
c		coordinates of the last untransparent point 
c		in the body frozen frame
              call rotz(ari,ati,azn(iunt),fx,fy,fz
     &        ,cdinc,sdinc,calpha,salpha)
	      fx=fx/xcp
	      fy=fy/xcp
	      fz=fz/xcp	
c		line of sight coordinates in the body frozen frame
              call rotz(0.d0,0.d0,1.d0,xlos,ylos,zlos
     &        ,cdinc,sdinc,calpha,salpha)
c		gravity vector in the last untransparent point
	      call gravn(fx,fy,fz,qq,potx,poty,potz,grav)
	      if(grav.lt.grmin)then
		ctheta=0.d0
	      else
 	        ctheta=-(xlos*potx+ylos*poty+zlos*potz)/grav
		if(ctheta.lt.0.d0)ctheta=0.d0
	      endif
c 		reflection off the roche surface
	      if(irrst.eq.1.and.istar.eq.2)then
		rvec=(fx-1.d0)**2+fy**2+fz**2
		if(grav.lt.grmin)then
		  cdelta=1.d0
		else
		  cdelta=((fx-1.d0)*potx+fy*poty+fz*potz)
		  cdelta=cdelta/dsqrt(rvec)/grav
		endif
	        cdelta=cdelta*(rcp/xcp)**2/rvec
	      else
	        cdelta=0.d0
	      endif	
	      vdif=0.d0
	    else	
              sth2=(ari*ari+ati*ati)/(rstar*rstar)
              if(sth2.lt.1.d0)then
                ctheta=dsqrt(1.d0-sth2)
              else
                ctheta=0.d0
              endif
c		reflection off the sphere
	      if(irrst.eq.1.and.istar.eq.1)then
	        cdelta=ari*xcpr+ati*ycpr+azn(iunt)*zcpr
	        cdelta=cdelta/dsqrt(ari**2+ati**2+azn(iunt)**2)
	        cdelta=cdelta/dsqrt(xcpr**2+ycpr**2+zcpr**2)
	        rr2=(ari-xcpr)**2+(ati-ycpr)**2+(azn(iunt)-zcpr)**2
	        cdelta=cdelta*rcp**2/rr2
     	        vdif1=(ari-xcpr)*(vr(iunt)-vxcpr)
     	        vdif2=(ati-ycpr)*(vt(iunt)-vycpr)
     	        vdif3=(azn(iunt)-zcpr)*(vz(iunt)-vzcpr)
     	        vdif=-(vdif1+vdif2+vdif3)/dsqrt(rr2)	        
	      else
	        cdelta=0.d0
		vdif=0.d0
	      endif
	    endif
            darkl=1.d0-dlst*(1.d0-ctheta)-dlst2*(1.d0-ctheta)**2
	    goto 20
c           icut=1
          elseif(dens(i).gt.dcut2.and.dens(i).le.dcut3)then
            iunt=i
	    if(icomp.gt.1)then
              cdinc=dcos(dinc)
              sdinc=dsin(dinc)
              calpha=dcos(alpha)
              salpha=dsin(alpha)
c		coordinates of the last untransparent point 
c		in the body frozen frame
              call rotz(ari,ati,azn(iunt),fx,fy,fz
     &        ,cdinc,sdinc,calpha,salpha)
	      fx=fx/xcp
	      fy=fy/xcp
	      fz=fz/xcp	
c		line of sight coordinates in the body frozen frame
              call rotz(0.d0,0.d0,1.d0,xlos,ylos,zlos
     &        ,cdinc,sdinc,calpha,salpha)
c		gravity vector in the last untransparent point
              call gravn(fx,fy,fz,qq,potx,poty,potz,grav)
	      if(grav.lt.grmin)then
		ctheta=0.d0
	      else
 	        ctheta=-(xlos*potx+ylos*poty+zlos*potz)/grav
		if(ctheta.lt.0.d0)ctheta=0.d0
	      endif
c 		reflection off the roche surface
	      if(irrcp.eq.1.and.icomp.eq.2)then
		rvec=fx**2+fy**2+fz**2
		if(grav.lt.grmin)then
		  cdelta=1.d0
		else
		  cdelta=(fx*potx+fy*poty+fz*potz)/dsqrt(rvec)/grav
		endif
	        cdelta=cdelta*(rstar/xcp)**2/rvec
	      else
	        cdelta=0.d0
	      endif	
	      vdif=0.d0
	    else	
	      dari=ari-xcpr
	      dati=ati-ycpr
	      sth2=(dari*dari+dati*dati)/(rcp*rcp)
	      if(sth2.lt.1.d0)then
	        ctheta=dsqrt(1.d0-sth2)
	      else
	        ctheta=0.d0
	      endif
c		reflection off the sphere
	      if(irrcp.eq.1.and.icomp.eq.1)then
	        rvecx=ari-xcpr
	        rvecy=ati-ycpr
	        rvecz=azn(iunt)-zcpr
	        cdelta=-rvecx*xcpr-rvecy*ycpr-rvecz*zcpr
	        cdelta=cdelta/dsqrt(rvecx**2+rvecy**2+rvecz**2)
	        cdelta=cdelta/dsqrt(xcpr**2+ycpr**2+zcpr**2)
	        rr2=ari**2+ati**2+azn(iunt)**2	
	        cdelta=cdelta*rstar**2/rr2
     	        vdif1=ari*(vr(iunt)-vxstr)
     	        vdif2=ati*(vt(iunt)-vystr)
     	        vdif3=azn(iunt)*(vz(iunt)-vzstr)
     	        vdif=-(vdif1+vdif2+vdif3)/dsqrt(rr2)	      	        
	      else
	        cdelta=0.d0
	        vdif=0.d0
	      endif
	    endif
	    darkl=1.d0-dlcp*(1.d0-ctheta)-dlcp2*(1.d0-ctheta)**2
	    goto 20
c           icut=2
          elseif(dens(i).gt.dcut3.and.dens(i).le.dcutn)then
            iunt=i
	    darkl=1.d0
	    goto 20
c           icut=3
          elseif(dens(i).gt.dcutn)then
            iunt=i
	    darkl=1.d0
	    goto 20
c           icut=4
          endif
10      continue
20	return
	end
c-----------------------------------------------------------------------
	subroutine bcond2(iunt,dens,temp,vz,vdif,freq
     &  ,dcut1,dcut2,dcut3,dcutn,darkl,cdelta
     &  ,lunt1,nstar1,xstar1,star1,nspec1,tspec1,alb1x,alb1y,nalb1
     &  ,tstar,dlst,dlst2,irrst,albst,wstar1,fstar1,jstar1
     &  ,lunt2,nstar2,xstar2,star2,nspec2,tspec2,alb2x,alb2y,nalb2
     &  ,tempcp,dlcp,dlcp2,irrcp,albcp,wstar2,fstar2,jstar2
     &  ,lunt3,nstar3,xstar3,star3,aint0,aintb)
c	Calculation of the boundary condition of intensity behind 
c	the last untransparent object along the line of sight
c	input:  
c         iunt-last untransparent point
c	  dens,temp -density, temperature
c	  vz - radial velocity field
c         vdif -differential radial velocity between the reflecting 
c               surface and the source of light
c	  freq - frequency at which we are about to solve RTE
c	  dcut1,dcut2,dcut3,dcutn -boundaries of density intervals
c	  darkl-limb darkening factor of the particular ray
c         cdelta -cos(irradiation angle)*R**2/separation**2
c	  lunt1,nstar1,xstar1,star1,nspec1,tspec1 -refere to 
c		'star' i.e. <dcut1,dcut2>
c	  tstar -polar temperature of the 'star'
c	  dlst - limb darkening coefficients of the `star`
c	  irrst -irradiation switch
c	  albst -Bond albedo of the `star`
c	  alb1x,alb1y,nalb1 -monochromatic albedo of the 'star'
c	  wstar1,fstar1,jstar1 -spectrum of the star with temp=tstar
c	  lunt2,nstar2,xstar2,star2,nspec2,tspec2 -refere to 
c 		'companion' i.e. <dcut2,dcut3>
c	  tempcp -polar temperature of the 'companion'
c         dlcp - limb darkening coefficients of the 'companion'
c	  irrcp -irradiation switch
c         albcp -Bond albedo of the `companion`
c	  alb2x,alb2y,nalb2 -monochromatic albedo of the 'companion'
c         wstar2,fstar2,jstar2 -spectrum of `companion` with temp=tempcp
c	  lunt3,nstar3,xstar3,star3 -refere to <dcut3,dcutn>
c	  aint0 -incident intensity from behind the shell
c	output:
c	  aintb- boundary condition for intensity, intensity heading 
c	    towards you along the line of sight after the untransparent 
c	    object
        implicit double precision (a-h,o-z)
        include 'param.inc'
	dimension temp(ndim),dens(ndim),vz(ndim)
        dimension xstar1(mstarx,mspecx),star1(mstarx,mspecx)
        dimension xstar2(mstarx,mspecx),star2(mstarx,mspecx)
        dimension xstar3(mstarx),star3(mstarx)
        dimension nstar1(mspecx),tspec1(mspecx),yy(mspecx)
        dimension nstar2(mspecx),tspec2(mspecx)
        dimension wstar1(mstarx),fstar1(mstarx)
        dimension wstar2(mstarx),fstar2(mstarx)
        dimension alb1x(mstarx),alb1y(mstarx)
        dimension alb2x(mstarx),alb2y(mstarx)
	clight=2.99792458d10
	pi=3.1415926535897931d0
	aintb=aint0
c       Four types of untransparent objects can be recognized here:
c       dcut1<dens<dcut2 -central star,
c       dcut2<dens<dcut3 -secondary star(=companion) 
c       dcut3<dens<dcutn -3.body (it can be anything)
c       dcutn<dens -any opaque dark matter
c	Note that lunt1, lunt2, lunt3 are in fact associated with 
c	particular density intervals rather then with objects. 
c       They can be used to ascribe the spectrum to any untransparent 
c       object setting its density within a particular density interval.
c	Be cautious to turn off the limb darkening in that case.
	if(iunt.gt.0)then
  	  freqs=freq*(1.d0-vz(iunt)/clight)
	  freqr=freq*(1.d0-(vdif+vz(iunt))/clight)
c	  behind the primary
	  if(dens(iunt).gt.dcut1.and.dens(iunt).le.dcut2)then
	    if(lunt1.gt.0)then
	      alamf=clight/freqs*1.d8
	      do j=1,nspec1
	        call locate2(xstar1,nstar1(j),alamf,jj,j)
	        if(jj.lt.1)then
	          yy(j)=star1(1,j)
	        elseif(jj.ge.nstar1(j))then
	          yy(j)=star1(nstar1(j),j)
	        else
	          der=star1(jj+1,j)-star1(jj,j)
                  der=der/(xstar1(jj+1,j)-xstar1(jj,j))
	          yy(j)=(alamf-xstar1(jj,j))*der+star1(jj,j)
	        endif
              enddo
              call locate(tspec1,nspec1,temp(iunt),jj)
	      if(jj.lt.1)then
                bunt=yy(1)*darkl
              elseif(jj.ge.nspec1)then 
                bunt=yy(nspec1)*darkl
              else
                der=(yy(jj+1)-yy(jj))/(tspec1(jj+1)-tspec1(jj))
                bunt=(temp(iunt)-tspec1(jj))*der+yy(jj)
                bunt=bunt*darkl                
              endif
            else
              bunt=planck(freqs,temp(iunt),1)*darkl
              bunt=bunt/(1.d0-dlst/3.d0-dlst2/6.d0)
	    endif
c		reflection off the primary
	    if(irrst.eq.1.and.albst.gt.0.d0.and.cdelta.gt.0.d0)then
	      if(nalb1.gt.1)then
                alamf=clight/freqs*1.d8
c                write(*,*)'before locate alb1x',(alb1x(i),i=1,nalb1)
	        call locate(alb1x,nalb1,alamf,jj)
	        if(jj.lt.1)then
	          albnu=alb1y(1)
	        elseif(jj.ge.nalb1)then
	          albnu=alb1y(nalb1)
	        else
	          der=alb1y(jj+1)-alb1y(jj)
	          der=der/(alb1x(jj+1)-alb1x(jj))
	          albnu=(alamf-alb1x(jj))*der+alb1y(jj)
	        endif
	      else
	        albnu=albst
	      endif
	    endif
            if(lunt2.gt.0)then
              if(irrst.eq.1.and.albst.gt.0.d0.and.cdelta.gt.0.d0)then
	        alamr=clight/freqr*1.d8              
                call locate(wstar2,jstar2,alamr,jj)
                if(jj.lt.1)then
                  buntr=fstar2(1)
                elseif(jj.ge.jstar2)then
                  buntr=fstar2(jstar2)
                else
                  der=fstar2(jj+1)-fstar2(jj)
                  der=der/(wstar2(jj+1)-wstar2(jj))
                  buntr=(alamr-wstar2(jj))*der+fstar2(jj)
                endif
                buntr=albnu*cdelta*buntr*(1.d0-dlcp/3.d0-dlcp2/6.d0)
              else
                buntr=0.d0
              endif  
            else
              if(irrst.eq.1.and.albst.gt.0.d0.and.cdelta.gt.0.d0)then
                buntr=albnu*cdelta*planck(freqr,tempcp,1)
              else
                buntr=0.d0
              endif  
            endif
            bunt=bunt+buntr
c         behind the secondary
	  elseif(dens(iunt).gt.dcut2.and.dens(iunt).le.dcut3)then
	    if(lunt2.gt.0)then
	      alamf=clight/freqs*1.d8
c 	      darkg=planck(freqs,temp(iunt),1)/planck(freqs,tempcp,1)
              do j=1,nspec2
  	        call locate2(xstar2,nstar2(j),alamf,jj,j)
	        if(jj.lt.1)then
	          yy(j)=star2(1,j)
	        elseif(jj.ge.nstar2(j))then
	          yy(j)=star2(nstar2(j),j)
	        else
	          der=star2(jj+1,j)-star2(jj,j)
                  der=der/(xstar2(jj+1,j)-xstar2(jj,j))
	          yy(j)=(alamf-xstar2(jj,j))*der+star2(jj,j)
	        endif
              enddo
              call locate(tspec2,nspec2,temp(iunt),jj)
              if(jj.lt.1)then
c             if(temp(iunt).lt.tspec2(1))then              
                bunt=yy(1)*darkl
              elseif(jj.ge.nspec2)then
c             elseif(temp(iunt).ge.tspec2(nspec2))then              
                bunt=yy(nspec2)*darkl
              else
                der=(yy(jj+1)-yy(jj))/(tspec2(jj+1)-tspec2(jj))
                bunt=(temp(iunt)-tspec2(jj))*der+yy(jj)
                bunt=bunt*darkl
              endif              
	    else
              bunt=planck(freqs,temp(iunt),1)*darkl
              bunt=bunt/(1.d0-dlcp/3.d0-dlcp2/6.d0)
	    endif
c		reflection off the secoondary
	    if(irrcp.eq.1.and.albcp.gt.0.d0.and.cdelta.gt.0.d0)then
	      if(nalb2.gt.1)then
                alamf=clight/freqs*1.d8
	        call locate(alb2x,nalb2,alamf,jj)
	        if(jj.lt.1)then
	          albnu=alb2y(1)
	        elseif(jj.ge.nalb2)then
	          albnu=alb2y(nalb2)
	        else
	          der=alb2y(jj+1)-alb2y(jj)
	          der=der/(alb2x(jj+1)-alb2x(jj))
	          albnu=(alamf-alb2x(jj))*der+alb2y(jj)
	        endif
	      else
	        albnu=albcp
	      endif
	    endif
            if(lunt1.gt.0)then
              if(irrcp.eq.1.and.albcp.gt.0.d0.and.cdelta.gt.0.d0)then
	        alamr=clight/freqr*1.d8              
                call locate(wstar1,jstar1,alamr,jj)
                if(jj.lt.1)then
                  buntr=fstar1(1)
                elseif(jj.ge.jstar1)then
                  buntr=fstar1(jstar1)
                else
                  der=fstar1(jj+1)-fstar1(jj)
                  der=der/(wstar1(jj+1)-wstar1(jj))
                  buntr=(alamr-wstar1(jj))*der+fstar1(jj)
                endif
                buntr=albnu*cdelta*buntr*(1.d0-dlst/3.d0-dlst2/6.d0)
              else
                buntr=0.d0
              endif  
            else
              if(irrcp.eq.1.and.albcp.gt.0.d0.and.cdelta.gt.0.d0)then
                buntr=albnu*cdelta*planck(freqr,tstar,1)
              else
                buntr=0.d0
              endif  
            endif
            bunt=bunt+buntr
c         behind the third body
	  elseif(dens(iunt).gt.dcut3.and.dens(iunt).le.dcutn)then
	    if(lunt3.gt.0)then
	      alamf=clight/freqs*1.d8
	      call locate(xstar3,nstar3,alamf,jj)
	      if(jj.lt.1)then
	        bunt=star3(1)
	      elseif(jj.ge.nstar3)then
	        bunt=star3(nstar3)
	      else
	        der=(star3(jj+1)-star3(jj))/(xstar3(jj+1)-xstar3(jj))
	        bunt=(alamf-xstar3(jj))*der+star3(jj)
	      endif
	    else
              bunt=planck(freqs,temp(iunt),1)
	    endif
	  else
            bunt=0.d0
	  endif	
          aintb=bunt
	endif
	return
	end
c-----------------------------------------------------------------------
	subroutine jnu(rstar,tstar,ari,ati,az
     &  ,vr,vt,vz,freq,xstar1,star1,nstar1,lunt1
     &  ,vxstr,vystr,vzstr,jj,ejnu,iprint,xcpr,ycpr,zcpr)
c	calculation of the mean intensity
c	from a spherical object in the opt. thin medium
c	input:
c	  rstar,tstar -radius, temperature of the object
c         lunt1 -black body switch
c	  ari,ati,az -coordinates of the point
c	  vr,vt,vz -velocity at the point
c	  freq -frequency
c	  xstar1,star1,nstar1 -spectrum of the object
c	  vxstr,vystr,vzstr -velocity of the object (source)
c	  iprint -dummy
c	  xcpr,ycpr,zcpr - coordinates of the object (source)
c	output:
c		ejnu = mean intensity = I_nu*omega/4/pi
c		jj- input for subseqent call of hunt
	implicit double precision (a-h,o-z)
        include 'param.inc'
	dimension xstar1(mstarx),star1(mstarx)
	clight=2.99792458d10
	dx=ari-xcpr
	dy=ati-ycpr
	dz=az-zcpr
	er2=dx**2+dy**2+dz**2
	ratio2=rstar*rstar/er2
	if(ratio2.lt.1.d0)then	
	  omega4=0.5d0*(1.d0-dsqrt(1.d0-ratio2))
	else
          omega4=0.d0
	endif
	rdotv=(dx*(vr-vxstr)+dy*(vt-vystr)+dz*(vz-vzstr))/dsqrt(er2)
	dvel=-rdotv+vz
c		freqs -frequency of alam in comoving frame
	  freqs=-dvel/clight*freq+freq
c		in ejnu the rotation of the star is ignored so far
	  if(lunt1.gt.0)then
	    alamf=clight/freqs*1.d8	
            call hunt(xstar1,nstar1,alamf,jj)
	    if(jj.lt.1)then
	      bstar=star1(1)
	    elseif(jj.ge.nstar1)then
	      bstar=star1(nstar1)
	    else
	      der=(star1(jj+1)-star1(jj))/(xstar1(jj+1)-xstar1(jj))
	      bstar=(alamf-xstar1(jj))*der+star1(jj)
	    endif
	  else
	    bstar=planck(freqs,tstar,1)
	  endif
          ejnu=bstar*omega4
	  return
	  end
c-----------------------------------------------------------------------
	subroutine roche(rosu,rotem,areas,teff
     &  ,rfront,rback,rpole,rside,rmean
     &  ,tstar,rstar,dgst,irrst,albst,htst,htsta
     &  ,tempcp,rcp,dgcp,irrcp,albcp,htcp,htcpa
     &  ,x,y,xcomp,q,ff,nbod1,nbod2,ipsc)
c	Subroutine calculates the Roche surface (RS), normalized 
c       gravity on RS of the primary, secondary or contact system,
c	irradiation angle, and temperature on RS including gravity 
c	darkening and an approximate irradiation effect with
c	reflection, heating, and heat redistribution for detached 
c	components.
c	Code works in normalized scale where center of 
c	mass of the primary is at x=0, center of mass of the secondary 
c	at x=1 but the input/output is scaled/unscaled. 
c	CGS units are employed.
c		input: 
c	x,y -grid points in orbital plane
c	nbod1,nbod2 -number of grid points in x and y
c	xcomp -is the scale=x coordinate of the secondary 
c	q -mass ratio (q<1 -more massive star is in the center,
c		q>1 -less massive star is in the center)
c	ff -Roche lobe filling factor of the primary (if ipsc=1). It is
c	   the distance of the inner substellar point of the primary 
c	   (between the stars) from the center of the primary relative 
c	   to the distance to L1, 
c	   ff<=1, the Roche lobe is reproduced if ff=1 
c	ff -Roche lobe filling factor of the secondary (if ipsc=2).
c	   It is the radius of the secondary at the substellar point 
c	   relative to 1-L1, 
c	   ff<=1, the Roche lobe is reproduced if ff=1 
c	ff -Roche lobe fill-out factor of the contact system (if ipsc=3)
c		ff=(P1-P)/(P1-P2)+1   where P,P1,P2 are potentials
c	ipsc=1 -calculates RS of a detached central star (primary)
c	ipsc=2 -calculates RS of a detached secondary
c	ipsc=3 -calculates RS of a contact system
c       tstar -effective temperature of the central star in [K]      
c          in the absence of irradiation effect
c          if ipsc=1 it is the temperature at the rotation pole 
c          if ipsc=3 it is the temperature at the rotation pole of
c          the more massive star
c       rstar -radius of the central star used for the irradiation 
c		of other object
c       dgst -gravity darkening coefficient (beta) of the central star
c               (0.25 for radiative, 0.08 for convective atmospheres) 
c       irrst=0 -irradiation (reflection effect) is off
c           (albstp,htst,htsta have no meaning in this case)
c       irrst=1 -irradiation of the object from the companion is on.
c       albst  -Bond albedo <0,1> of the central star
c       htst   -heat redistribution parameter in case of the irradiation, 
c           <0,1>, 0-nothing is redistributed over the day side and 
c          nothing goes to the night, 1-all the heat is evenly 
c          redistributed over the day and night sides 
c       htsta  -zonal temperature redistribution parameter i.e.
c	   a degree of the homegenity of the heat transport  
c           <0,1>. 1-homegeneous, 0-cosine dependence (zonal)
c           T**4=T0**4(htsta+(1-htsta)*cos_latitude*constant)
c       tempcp -effective temperature at the rotation pole of 
c	   the secondary in [K] in the absence of irradiation effect
c       rcp -radius of the secondary used for the irradiation
c               of other object
c	irrcp,albcp,htcp,htcpa -the same as in the case of primary
c
c		output: 
c	rosu(x,y) -Roche surface
c	rotem(x,y) -surface temperature
c       areas -area of the RS
c	teff -effective temperature of the object including irradiation
c		and gravity darkening
c 	rfront,rback,rpole,rside,rmean -radius relative to the semimajor
c		axis at the substellar point, antistellar point, 
c		rotation pole, side point, and radius of a sphere with
c		 the same volume
c	rogrv(x,y) -normalized gravity on the RS relative to the gravity
c		on the rotation pole (of more massive star in case of
c		contact system)
c	rocos(x,y) -cosine of the angle between gravity vector
c		(-1*normal to the surface) and direction towards
c		towards the irradiating star i.e. cosine of the zenit 
c		distance of the irradiating star as seen from 
c		the irradiated surface). It is used for 
c		an approximate reflection effect (only for ipsc=1,2). 
c		rocos=0 on the night side.
c
c       Author/contact: Jan Budaj, http://www.ta3.sk/~budaj
c                       budaj@ta3.sk
c
	implicit double precision (a-h,o-z)
 	include 'param.inc'
c	parameter(ndimf1=6001,ndimf2=6001)
	dimension x(ndimf1),y(ndimf2),xn(ndimf1),yn(ndimf2)
        dimension rosuy(ndimf1),rosu(ndimf1,ndimf2),rogrv(ndimf1,ndimf2)
        dimension rocos(ndimf1,ndimf2),rotem(ndimf1,ndimf2)
        dimension area(ndimf1,ndimf2),areaz(ndimf1,ndimf2)
        dimension arear(ndimf1,ndimf2)
	external pot,dxpot,dypot,dzpot,dxxpot
	pi=3.1415926535897931d0
	stefb=5.670400d-5
	if((ipsc.eq.1.or.ipsc.eq.2).and.(ff.gt.1.d0.or.ff.lt.1.d-2))
     &  then
	  write(*,130)
	  stop
c	  goto 120
	endif
	if(ipsc.eq.3.and.(ff.le.1.d0.or.ff.gt.2.d0))then
	  write(*,130)
	  stop
c	  goto 120
	endif
	if(.not.(ipsc.eq.1.or.ipsc.eq.2.or.ipsc.eq.3))then
	  write(*,130)
	  stop
c	  goto 120
	endif
	if(htst.lt.0.d0.or.htst.gt.1.d0.or.
     &    htsta.lt.0.d0.or.htsta.gt.1.d0.or.
     &    htcp.lt.0.d0.or.htcp.gt.1.d0.or.
     &    htcpa.lt.0.d0.or.htcpa.gt.1.d0)then
	  write(*,130)
	  stop
c	  goto 120
	endif
c	precision required
	prc=1.d-7
	prc2=1.d-9
c	normalization of coordinates
        do 10 i=1,nbod1
          xn(i)=x(i)/xcomp
10      continue
        do 20 i=1,nbod2
          yn(i)=y(i)/xcomp
20      continue
c		el1- the x coordinate of the L1 point 
c	it may be quicker to start with 0.5 then with radlob function
	el1=raphx(dxpot,dxxpot,0.d0,q,0.5d0,prc2)
c       	el2,el3 -the L2,L3 points (they are reversed if q>1)
        el2=raphx(dxpot,dxxpot,0.d0,q,2.d0-el1,prc2)
        el3=raphx(dxpot,dxxpot,0.d0,q,-el1,prc2)
c
c			primary, ff<=1
	if(ipsc.eq.1.and.ff.gt.0.d0.and.ff.le.1.d0)then
	  pinn=ff*el1
	  pout=-pinn
	  rpot=pot(pinn,0.d0,0.d0,q)
c	  pinn-the inner substellar point of the surface of the primary
c	  pout-the outer point of the primary on the averted hemisphere
	  if(pout.gt.el3)then
   	    pout=pout
	  else
	    pout=el3*0.5d0
	  endif
	  pout=raphx(pot,dxpot,rpot,q,pout,prc2)
c	  rosuy(x)-the shape of the primary Roche surface for x=x(i),z=0
c	  ystart=el1*0.2d0
c	  expression below might help for small ff
	  ystart=pinn*0.5d0
	  do 50 i=1,nbod1
	    if(xn(i).gt.pout.and.xn(i).lt.pinn)then
	      rosuy(i)=dabs(raphy(pot,dypot,rpot,q,xn(i),ystart,prc))
	      ystart=rosuy(i)
	    else
	      rosuy(i)=0.d0
	    endif
50	  continue
c	  coordinates of the rotation pole
	  xpole=0.d0
	  ypole=0.d0
c	  zpole=raphz(pot,dzpot,rpot,q,xpole,ypole,radlob(1.d0/q),prc2)
	  zpole=raphz(pot,dzpot,rpot,q,xpole,ypole,dabs(pout),prc2)
	  rfront=pinn
	  rback=-pout
	  rpole=zpole
	  rside=dabs(raphy(pot,dypot,rpot,q,0.d0,pout,prc2))
	endif
c
c			contact, 1<ff<=2
	if(ipsc.eq.3.and.ff.gt.1.d0.and.ff.le.2.d0)then
	  potel1=pot(el1,0.d0,0.d0,q)	
	  potel2=pot(el2,0.d0,0.d0,q)
	  potel3=pot(el3,0.d0,0.d0,q)
	  if(potel2.lt.potel3)then
	    pom=potel2
	    potel2=potel3
	    potel3=pom
	  endif
	  rpot=potel1-(ff-1.d0)*(potel1-potel2)
	  pout=raphx(pot,dxpot,rpot,q,el3*0.5d0,prc2)
	  pinn=raphx(pot,dxpot,rpot,q,0.5d0+el2*0.5d0,prc2)
c	  pinn-the right hand side edge of the contact Roche surface
c	  pout-the left hand side edge of the contact Roche surface
c	  rosuy(x)-the shape of the contact Roche surface for x=x(i),z=0
	  ystart=radlob(1.d0/q)*0.2d0
	  do 70 i=1,nbod1
	    if(xn(i).gt.pout.and.xn(i).lt.pinn)then
	      rosuy(i)=dabs(raphy(pot,dypot,rpot,q,xn(i),ystart,prc))
	      ystart=rosuy(i)
	    else
	      rosuy(i)=0.d0
	    endif
c	    write(3,*)xn(i),rosuy(i),pout
70	  continue
c	  coordinates of the rotation pole of the more massive star
	  if(q.lt.1.d0)then
	  xpole=0.d0
	  ypole=0.d0
	  zpole=raphz(pot,dzpot,rpot,q,xpole,ypole,radlob(1.d0/q),prc2)
          rside=dabs(raphy(pot,dypot,rpot,q,xpole,radlob(1.d0/q),prc2))
	  else
	  xpole=1.d0
	  ypole=0.d0
	  zpole=raphz(pot,dzpot,rpot,q,xpole,ypole,radlob(q),prc2)
	  rside=dabs(raphy(pot,dypot,rpot,q,xpole,radlob(q),prc2))
	  endif
	  rfront=pinn
	  rback=-pout
	  rpole=zpole
	endif
c
c			secondary, ff<=1
	if(ipsc.eq.2.and.ff.gt.0.d0.and.ff.le.1.d0)then
	  sout=ff*(1.d0-el1)
	  sinn=1.d0-sout
	  rpot=pot(sinn,0.d0,0.d0,q)
c	  sinn-inner substellar point of the surface of the secondary
c	  sout-outer point of the secondary on the averted hemisphere
	  if((1.d0+sout).lt.el2)then
   	    sout=1.d0+sout
	  else
	    sout=1.d0+(el2-1.d0)*0.5d0
	  endif
	  sout=raphx(pot,dxpot,rpot,q,sout,prc2)
c	  rosuy(x)-the shape of the Roche surface for x=x(i),z=0
	  ystart=(sout-1.d0)*0.2d0
	  do 90 i=1,nbod1
	    if(xn(i).gt.sinn.and.xn(i).lt.sout)then
	      rosuy(i)=dabs(raphy(pot,dypot,rpot,q,xn(i),ystart,prc))
	      ystart=rosuy(i)
	    else
	      rosuy(i)=0.d0
	    endif
90	  continue
c	  coordinates of the rotation pole
	  xpole=1.d0
	  ypole=0.d0
	  zpole=radlob(q)*ff
	  zpole=raphz(pot,dzpot,rpot,q,xpole,ypole,zpole,prc2)
	  rfront=1.d0-sinn
	  rback=sout-1.d0
	  rpole=zpole
	  rside=dabs(raphy(pot,dypot,rpot,q,1.d0,zpole,prc2))
	endif
c	check mainly this
  	do 95 i=1,nbod1
	  if(rosuy(i).gt.0.d0.and.
     &    dypot(xn(i),rosuy(i),0.d0,q).ge.0.d0)then
	    write(*,*)' Error: Newton-Raphson slipped in subr.: Roche,',
     &      ' try to modify slightly: nbod,q,ff'
	  goto 120
	  endif
95	continue
c
c	gravnp-normalized gravity at the rotation pole
	call gravn(xpole,ypole,zpole,q,potx,poty,potz,gravnp)
c
c		rosu(x,y)-the shape of the Roche surface
c		rogrv(x,y) -normalized gravity relative to the pole
c		rocos(i,j) -cosine of the irradiation angle
	do  i=1,nbod1
          do  j=1,nbod2
            if(dabs(yn(j)).lt.rosuy(i))then
	      rosu(i,j)=raphz(pot,dzpot,rpot,q,xn(i),yn(j),rosuy(i)
     &        ,prc)
	      call gravn(xn(i),yn(j),rosu(i,j),q,potx,poty,potz,gravec)
	      rogrv(i,j)=gravec/gravnp
	      areaz(i,j)=dabs(potz/gravec)
	      if(ipsc.eq.1)then
	        rvec=dsqrt((xn(i)-1.d0)**2+yn(j)**2+rosu(i,j)**2)
                rocos(i,j)=((xn(i)-1.d0)*potx+yn(j)*poty+rosu(i,j)*potz)
     &          /rvec/gravec
       	        if(rogrv(i,j).lt.1.d-4)then
	          rogrv(i,j)=1.d-4
	          rocos(i,j)=0.84d0
	        endif
		rotem(i,j)=tstar*rogrv(i,j)**dgst
	      elseif(ipsc.eq.2)then
	        rvec=dsqrt(xn(i)*xn(i)+yn(j)*yn(j)+rosu(i,j)*rosu(i,j))
	        rocos(i,j)=(xn(i)*potx+yn(j)*poty+rosu(i,j)*potz)/rvec
     &          /gravec
c		take care of the unimportant case of singularity
     	        if(rogrv(i,j).lt.1.d-4)then
	          rogrv(i,j)=1.d-4
	          rocos(i,j)=0.84d0
	        endif
		rotem(i,j)=tempcp*rogrv(i,j)**dgcp
              else
	        if(rogrv(i,j).lt.1.d-4)rogrv(i,j)=1.d-4
                rocos(i,j)=0.d0
		rotem(i,j)=tstar*rogrv(i,j)**dgst
	      endif
    	      if(rocos(i,j).lt.0.d0)rocos(i,j)=0.d0
            else
	      rosu(i,j)=0.d0
	      rogrv(i,j)=0.d0
	      rocos(i,j)=0.d0
	      rotem(i,j)=0.d0
	      areaz(i,j)=0.d0
            endif
c		unscale the result
	    rosu(i,j)=xcomp*rosu(i,j)
	  enddo
	enddo
c	
c		irradiation and heat redistribution
	call surf(ndim1,ndim2,nbod1,nbod2,x,y,area)
c	area-projection of the surface element to xy
c	arear-area of the surface element
c	vol-volume under the Roche surface
        areas=0.d0
	vol=0.d0
        sum1=0.d0
        sum2=0.d0        
        do i=1,nbod1  
          do j=1,nbod2
            if(dabs(yn(j)).lt.rosuy(i))then  
              arear(i,j)=area(i,j)/areaz(i,j)
              if(irrst.eq.1.and.ipsc.eq.1)then
                fxy2=x(i)**2+y(j)**2
                coslat=fxy2/(fxy2+rosu(i,j)**2)
                coslat=dsqrt(coslat)
                surf2=coslat*arear(i,j)
                if(rocos(i,j).gt.0.d0)then
c                 dayside
                  rr2=rcp**2/((x(i)-xcomp)**2+y(j)**2+rosu(i,j)**2)
                  firr=stefb*rr2*tempcp**4*rocos(i,j)
		  surf1=firr*arear(i,j)
                endif
              endif
              if(irrcp.eq.1.and.ipsc.eq.2)then
                fxy2=(x(i)-xcomp)**2+y(j)**2
                coslat=fxy2/(fxy2+rosu(i,j)**2)
                coslat=dsqrt(coslat)
                surf2=coslat*arear(i,j)
                if(rocos(i,j).gt.0.d0)then
c                 day-side
                  rr2=rstar*rstar/(x(i)**2+y(j)**2+rosu(i,j)**2)
                  firr=stefb*rr2*tstar**4*rocos(i,j)
                  surf1=firr*arear(i,j)
                endif    
              endif              
              areas=areas+arear(i,j)
              vol=vol+area(i,j)*rosu(i,j)
	      sum1=sum1+surf1
	      sum2=sum2+surf2
            endif
          enddo
        enddo
        areas=areas*2.d0
        rmean=(vol*2.d0/(4.d0/3.d0*pi))**(1.d0/3.d0)/xcomp
        sum1=sum1*2.d0
        sum2=sum2*2.d0
        if(irrst.eq.1.and.ipsc.eq.1)then
          rr2=rcp**2/xcomp**2
          t04=0.25d0*htst*(1.d0-albst)*rr2*tempcp**4
          t04new=htst*(1.d0-albst)*sum1/areas/stefb
c            t04=htst*(1.d0-albst)*sum1/areas/stefb          
          htb=4.d0*(1.d0-htsta)/pi
          htbnew=(1.d0-htsta)*areas/sum2
c            htb=(1.d0-htsta)*areas/sum2          
        endif  
        if(irrcp.eq.1.and.ipsc.eq.2)then
          rr2=rstar*rstar/xcomp**2  
          t04=0.25d0*htcp*(1.d0-albcp)*rr2*tstar**4            
          t04new=htcp*(1.d0-albcp)*sum1/areas/stefb
c            t04=htcp*(1.d0-albcp)*sum1/areas/stefb          
          htb=4.d0*(1.d0-htcpa)/pi
          htbnew=(1.d0-htcpa)*areas/sum2
c            htb=(1.d0-htcpa)*areas/sum2          
        endif  
c	sum1,t04new may not be precise enough
c	(polar or cylindrical coordinates might be better).
c	However, if you want to use them simply uncomment 
c	the four lines above and replace
c	t04 by t04new and htb by htbnew.
c	write(*,*)'T0=',t04new**0.25d0,t04**0.25d0
c	write(*,*)'b=',htbnew,htb
c	write(*,*)'sum1,2=',sum1,sum2
	teff=0.d0
	do i=1,nbod1
          do j=1,nbod2
            if(dabs(yn(j)).lt.rosuy(i))then
              if(irrst.eq.1.and.ipsc.eq.1)then
                fxy2=x(i)**2+y(j)**2
                coslat=fxy2/(fxy2+rosu(i,j)**2)
                coslat=dsqrt(coslat)
                fnlat=htsta+htb*coslat
                if(rocos(i,j).gt.0.d0)then
c                 dayside
                  rr2=rcp**2/((x(i)-xcomp)**2+y(j)**2+rosu(i,j)**2)
                  firr=stefb*rr2*tempcp**4*rocos(i,j)
                  tirr4=(1.d0-htst)*(1.d0-albst)*firr/stefb
                  rotem(i,j)=(tirr4+t04*fnlat+rotem(i,j)**4)
		  rotem(i,j)=dsqrt(dsqrt(rotem(i,j)))
                else 
c                 nightside             
                  rotem(i,j)=(t04*fnlat+rotem(i,j)**4)
                  rotem(i,j)=dsqrt(dsqrt(rotem(i,j)))
                endif
              endif
              if(irrcp.eq.1.and.ipsc.eq.2)then
                fxy2=(x(i)-xcomp)**2+y(j)**2
                coslat=fxy2/(fxy2+rosu(i,j)**2)
                coslat=dsqrt(coslat)
                fnlat=htcpa+htb*coslat  
                if(rocos(i,j).gt.0.d0)then
c                 day-side
                  rr2=rstar*rstar/(x(i)**2+y(j)**2+rosu(i,j)**2)
                  firr=stefb*rr2*tstar**4*rocos(i,j)
                  tirr4=(1.d0-htcp)*(1.d0-albcp)*firr/stefb
                  rotem(i,j)=(tirr4+t04*fnlat+rotem(i,j)**4)
                  rotem(i,j)=dsqrt(dsqrt(rotem(i,j)))
                else
c                 night-side
                  rotem(i,j)=(t04*fnlat+rotem(i,j)**4)
		  rotem(i,j)=dsqrt(dsqrt(rotem(i,j)))
                endif    
              endif
              teff=teff+arear(i,j)*rotem(i,j)**4
            endif
	  enddo
	enddo
	teff=(teff*2.d0/areas)**0.25d0
c
c     	open(4,file='roche.out',status='unknown')
c	do i=1,nbod1
c         do j=1,nbod2
c            write(4,180)x(i),y(j),rosu(i,j),rogrv(i,j),rocos(i,j)
c     &      ,rotem(i,j)
c	  enddo
c	  write(4,*)
c	enddo
c180	format(6e14.5)
c	close(4)	
c
120	return
130	format(' error: inconsistent input in subroutine: roche')
	end
c-----------------------------------------------------------------------
        double precision function raphx(fun,dfun,fun0,q,xstart,xacc)
c	Newton-Raphson Method in 1D (x coordinate)
c	to solve the equation fun(x)=fun0
c	input:  fun-external function of x and parameters: y,z,q
c		dfun-external function, partial derivative df/dx
c		fun0-value of the function 
c		q-mass ratio
c	        xstart-first estimate
c		xacc-accuracy
c	output:  raphx-root for which fun=fun0
        implicit double precision (a-h,o-z)
	external fun,dfun
	raphx=xstart
	do i=1,22
	  dx=(fun(raphx,0.d0,0.d0,q)-fun0)/dfun(raphx,0.d0,0.d0,q)
	  raphx=raphx-dx
	  if(dabs(dx).lt.xacc)return
 	enddo
	return
	end
c-----------------------------------------------------------------------
        double precision function raphy(fun,dfun,fun0,q,x0,ystart,yacc)
c	Newton-Raphson Method in 1D (y coordinate)
c	to solve the equation fun(y)=fun0
c	input:  fun-external function of y and parameters: x,z,q
c		dfun-external function, partial derivative df/dy
c		fun0-value of the function 
c		q-mass ratio
c	        ystart-first estimate
c		yacc-accuracy
c	output:  raphy-root for which fun=fun0
        implicit double precision (a-h,o-z)
	external fun,dfun
	raphy=ystart
	do i=1,22
	  dy=(fun(x0,raphy,0.d0,q)-fun0)/dfun(x0,raphy,0.d0,q)
	  raphy=raphy-dy
	  if(dabs(dy).lt.yacc)return
 	enddo
	return
	end
c-----------------------------------------------------------------------
        double precision function raphz(fun,dfun,fun0,q,x0,y0,zstart
     &  ,zacc)
c	Newton-Raphson Method in 1D (z coordinate)
c	to solve the equation fun(z)=fun0
c	input:  fun-external function of z and parameters: x,y,q
c		dfun-external function, partial derivative df/dz
c		fun0-value of the function 
c		q-mass ratio
c	        zstart-first estimate
c		zacc-accuracy
c	output:  raphz-root for which fun=fun0
        implicit double precision (a-h,o-z)
	external fun,dfun
	raphz=zstart
	do i=1,22
	  dz=(fun(x0,y0,raphz,q)-fun0)/dfun(x0,y0,raphz,q)
	  raphz=raphz-dz
	  if(dabs(dz).lt.zacc)return
 	enddo
	return
	end
c-----------------------------------------------------------------------
	double precision function pot(x,y,z,q)
c	input: x,y,z,q
c	output: normalized Roche potential 
        implicit double precision (a-h,o-z)
	q1=1.d0+q
	x1=x-1.d0
	r1=dsqrt(x*x+y*y+z*z)
	r2=dsqrt(x1*x1+y*y+z*z)
	pot=2.d0/q1/r1+2.d0*q/q1/r2+(x-q/q1)*(x-q/q1)+y*y
	return
	end
c-----------------------------------------------------------------------
	double precision function dxpot(x,y,z,q)
c	input: x,y,z,q
c	output: partial derivation of the Roche potential after x
        implicit double precision (a-h,o-z)
	q1=1.d0+q
	x1=x-1.d0
	r1=dsqrt(x*x+y*y+z*z)
	r2=dsqrt(x1*x1+y*y+z*z)
	dxpot=-x/q1/r1**3-q*x1/q1/r2**3+x-q/q1
	dxpot=2.d0*dxpot
	return
	end
c-----------------------------------------------------------------------
	double precision function dypot(x,y,z,q)
c	input: x,y,z,q
c	output: partial derivation of the Roche potential after y
        implicit double precision (a-h,o-z)
	q1=1.d0+q
	x1=x-1.d0
	r1=dsqrt(x*x+y*y+z*z)
	r2=dsqrt(x1*x1+y*y+z*z)
	dypot=-2.d0/q1/r1**3-2.d0*q/q1/r2**3+2.d0
	dypot=dypot*y
	return
	end
c-----------------------------------------------------------------------
	double precision function dzpot(x,y,z,q)
c	input: x,y,z,q
c	output: partial derivation of the Roche potential after z
        implicit double precision (a-h,o-z)
	q1=1.d0+q
	x1=x-1.d0
	r1=dsqrt(x*x+y*y+z*z)
	r2=dsqrt(x1*x1+y*y+z*z)
	dzpot=-2.d0/q1/r1**3-2.d0*q/q1/r2**3
	dzpot=dzpot*z
	return
	end
c-----------------------------------------------------------------------
	double precision function dxxpot(x,y,z,q)
c	input: x,y,z,q
c	output: second partial derivation of the Roche potential after x
        implicit double precision (a-h,o-z)
	q1=1.d0+q
	x1=x-1.d0
	r1=dsqrt(x*x+y*y+z*z)
	r2=dsqrt(x1*x1+y*y+z*z)
	dxxpot=6.d0*x*x/q1/r1**5+6.d0*q*x1*x1/q1/r2**5-2.d0/q1/r1**3
        dxxpot=dxxpot-2.d0*q/q1/r2**3+2.d0
	return
	end
c-----------------------------------------------------------------------
	double precision function radlob(q)
c	returns the volumee radius of the Roche lobe, 0<q<infty
c	q<1 -radius of the less massive star
c	q>1 -radius of the more massive star
        implicit double precision (a-h,o-z)
	q13=q**(1.d0/3.d0)
	radlob=0.49d0*q13*q13/(0.69d0*q13*q13+dlog(1.d0+q13))
	return
	end
c-----------------------------------------------------------------------
        subroutine gravn(x,y,z,q,potx,poty,potz,pot)
c       calculates the normalized gravity vector
c       input: x,y,z,q
c       output: potx,poty,potz,pot
        implicit double precision (a-h,o-z)
        potx=dxpot(x,y,z,q)
        poty=dypot(x,y,z,q)
        potz=dzpot(x,y,z,q)
        pot=dsqrt(potx*potx+poty*poty+potz*potz)
        return
        end
c-----------------------------------------------------------------------
	subroutine untsp(xstar1,star1,xstar2,star2,xstar3,star3
     &  ,wstar1,fstar1,tstar1,jstar1,wstar2,fstar2,tstar2,jstar2 
     &  ,xunt1,yunt1,xunt2,yunt2,xunt3,yunt3
     &  ,nstar1,nstar2,nstar3,nspec1,nspec2,tspec1,tspec2
     &  ,lunt1,lunt2,lunt3,dlst,dlst2,dlcp,dlcp2)
c	reading the input spectra of nontransparent objects
c	input: lunt1,lunt2,lunt3,xunt1,yunt1,xunt2,yunt2,xunt3,yunt3,
c		dlst,dlst2,dlcp,dlcp2,tstar1,tstar2
c	output:xstar1,star1,xstar2,star2,xstar3,star3,
c		nstar1,nstar2,nstar3,nspec1,nspec2,tspec1,tspec2,
c		wstar1,fstar1,jstar1,wstar2,fstar2,jstar2
        implicit double precision (a-h,o-z)
	include 'param.inc'
	character*50 in1,in2
	dimension xstar1(mstarx,mspecx),star1(mstarx,mspecx)
        dimension wstar1(mstarx),fstar1(mstarx)
        dimension xstar2(mstarx,mspecx),star2(mstarx,mspecx)
        dimension wstar2(mstarx),fstar2(mstarx) 
        dimension xstar3(mstarx),star3(mstarx)
        dimension in1(mspecx),nstar1(mspecx),tspec1(mspecx)
        dimension in2(mspecx),nstar2(mspecx),tspec2(mspecx)     
        dimension fjj(mstarx),wjj1(mstarx),fjj1(mstarx),fjj1n(mstarx)
	pi=3.1415926535897931d0
	clight=2.99792458d10
c	our units will be:	
c 	star1 -intrinsic nonrotated spectrum of the central star
c		intensity for angle=0 in [erg/cm^2/s/Hz/sterad]
c	xstar1(i) -lambda in [A]
	if(lunt1.gt.0)then
	  write(*,*)' reading file starspec1'
	  open(12,file='starspec1',STATUS='OLD')
	  read(12,*)
	  read(12,*)nspec1
	  read(12,*)
	  do i=1,nspec1
	    read(12,*)tspec1(i)
	  enddo
	  read(12,*)
	  do i=1,nspec1
	    read(12,'(a)')in1(i)
	  enddo
	  close(12)
  	  if(nspec1.gt.1)then
	    do i=1,nspec1-1
	      if(tspec1(i+1).le.tspec1(i))then 
	        write(*,*)'error: tspec1 in starspec1, stop'
	        stop
	      endif  
            enddo
	  endif
	  do j=1,nspec1
	    open(12,file=in1(j),status='old')
	    i=1
            if(lunt1.eq.3)then
              read(12,*,end=20)
              read(12,*,end=20)
            endif
10	    if(lunt1.eq.1)then
	      read(12,*,end=20)xstar1(i,j),star1(i,j)
	      if(i.gt.1)then
c		fix synspec bug	      
	        if(xstar1(i,j).eq.xstar1(i-1,j))goto 10
	        if(.not.(xstar1(i,j).gt.xstar1(i-1,j)))then
                  write(*,*)' input error in starspec1 on line ',i,j
                endif
	        if(i+1.gt.mstarx)then
	          write(*,*)' error in starspec1: '
                  write(*,*)'dimension mstarx almost exceeded'
                endif
	      endif
c		convert input of Hlambda from synspec to our units
	      xstar1(i,j)=xstar1(i,j)*xunt1
	      star1(i,j)=star1(i,j)*yunt1/(1.d0-dlst/3.d0-dlst2/6.d0)
	      star1(i,j)=4.d-8*star1(i,j)*xstar1(i,j)**2/clight
	    endif 
 	    if(lunt1.eq.2)then
 	      read(12,*,end=20)xstar1(i,j),star1(i,j)
	      if(i.gt.1)then
	        if(.not.(xstar1(i,j).gt.xstar1(i-1,j)))then
                  write(*,*)' input error in starspec1 on line ',i,j
                endif
	        if(i+1.gt.mstarx)then
	          write(*,*)' error in starspec1: '
	          write(*,*)'dimension mstarx almost exceeded'
	        endif  
	      endif
	      xstar1(i,j)=xstar1(i,j)*xunt1
	      star1(i,j)=star1(i,j)*yunt1
	    endif
 	    if(lunt1.eq.3)then
 	      read(12,*,end=20)idummy,xstar1(i,j),dummy,star1(i,j)
	      if(i.gt.1)then
	        if(.not.(xstar1(i,j).gt.xstar1(i-1,j)))then
                  write(*,*)' input error in starspec1 on line ',i,j
                endif
	        if(i+1.gt.mstarx)then
	          write(*,*)' error in starspec1: '
                  write(*,*)'dimension mstarx almost exceeded'
                endif  
	      endif
c		convert input of nu, Fnu from coolTlusty.21 to our units
	      xstar1(i,j)=xunt1*clight/xstar1(i,j)*1.d8
	      star1(i,j)=star1(i,j)*yunt1
	      star1(i,j)=star1(i,j)/pi/(1.d0-dlst/3.d0-dlst2/6.d0)
	    endif
	    i=i+1
	    goto 10
20	    nstar1(j)=i-1
	    write(6,*)' nstar1=',j,nstar1(j)
	    close(12)
	  enddo  
c		an interpolation to tstar1 to get wstar1,fstar1 
c		these will be used e.g. for scattering or reflection
	  if(tstar1.ge.tspec1(nspec1))then
	    do i=1,nstar1(nspec1)
	      wstar1(i)=xstar1(i,nspec1)
	      fstar1(i)=star1(i,nspec1)
	      jstar1=nstar1(nspec1)
	    enddo  
	  elseif(tstar1.le.tspec1(1))then
	    do i=1,nstar1(1)
	      wstar1(i)=xstar1(i,1)
	      fstar1(i)=star1(i,1)
	      jstar1=nstar1(1)
            enddo
	  else
	    call locate(tspec1,nspec1,tstar1,jj)
	    jstar1=nstar1(jj)
	    do i=1,nstar1(jj)
	      wstar1(i)=xstar1(i,jj)
	      fjj(i)=star1(i,jj)
	    enddo
	    do i=1,nstar1(jj+1)
	      wjj1(i)=xstar1(i,jj+1) 
              fjj1(i)=star1(i,jj+1) 
            enddo
	    call interp(wjj1,fjj1,wstar1,fjj1n,nstar1(jj+1),nstar1(jj))
	    do i=1,nstar1(jj)
	      der=(fjj1n(i)-fjj(i))/(tspec1(jj+1)-tspec1(jj))
	      fstar1(i)=der*(tstar1-tspec1(jj))+fjj(i)
	    enddo
	  endif
	endif
c 	star2 -intrinsic nonrotated spectrum of the secondary star
c		intensity for angle=0 in erg/cm^2/s/Hz/sterad
c	xstar2(i) -lambda in [A]
	if(lunt2.gt.0)then
	  write(*,*)' reading file starspec2'
	  open(13,file='starspec2',status='old')
	  read(13,*)
	  read(13,*)nspec2
	  read(13,*)
	  do i=1,nspec2
	    read(13,*)tspec2(i)
	  enddo
	  read(13,*)
	  do i=1,nspec2
	    read(13,'(a)')in2(i)
	  enddo
	  close(13)
  	  if(nspec2.gt.1)then
	    do i=1,nspec2-1
	      if(tspec2(i+1).le.tspec2(i))then 
	        write(*,*)'error: tspec2 in starspec2, stop'
	        stop
	      endif  
            enddo
	  endif
          do j=1,nspec2
            open(13,file=in2(j),status='old')
  	    i=1
            if(lunt2.eq.3)then
              read(13,*,end=60)
              read(13,*,end=60)
            endif
50	    if(lunt2.eq.1)then
	      read(13,*,end=60)xstar2(i,j),star2(i,j)
	      if(i.gt.1)then
c		fix synspec bug
		if(xstar2(i,j).eq.xstar2(i-1,j))goto 50
	        if(.not.(xstar2(i,j).gt.xstar2(i-1,j)))then
                  write(*,*)' input error in starspec2 on line ',i,j
                endif  
	        if(i+1.gt.mstarx)then
	          write(*,*)' error in starspec2: '
                  write(*,*)'dimension mstarx almost exceeded'
                endif  
	      endif
c		convert input of Hlambda from synspec to our units
  	      xstar2(i,j)=xstar2(i,j)*xunt2
	      star2(i,j)=star2(i,j)*yunt2/(1.d0-dlcp/3.d0-dlcp2/6.d0)
	      star2(i,j)=4.d-8*star2(i,j)*xstar2(i,j)**2/clight
	    endif
	    if(lunt2.eq.2)then
	      read(13,*,end=60)xstar2(i,j),star2(i,j)
	      if(i.gt.1)then
	        if(.not.(xstar2(i,j).gt.xstar2(i-1,j)))then
                  write(*,*)' input error in starspec2 on line ',i,j
                endif  
	        if(i+1.gt.mstarx)then
	          write(*,*)' error in starspec2: '
                  write(*,*)'dimension mstarx almost exceeded'
                endif  
	      endif
	      xstar2(i,j)=xstar2(i,j)*xunt2
	      star2(i,j)=star2(i,j)*yunt2
	    endif
	    if(lunt2.eq.3)then
	      read(13,*,end=60)idummy,xstar2(i,j),dummy,star2(i,j)
	      if(i.gt.1)then
	        if(.not.(xstar2(i,j).gt.xstar2(i-1,j)))then
                  write(*,*)' input error in starspec2 on line ',i,j
                endif  
	        if(i+1.gt.mstarx)then
	          write(*,*)' error in starspec2: '
                  write(*,*)'dimension mstarx almost exceeded'
                endif  
	      endif
c               convert input of nu, Fnu from coolTlusty.21 to our units	    
	      xstar2(i,j)=xunt2*clight/xstar2(i,j)*1.d8
	      star2(i,j)=star2(i,j)*yunt2
              star2(i,j)=star2(i,j)/pi/(1.d0-dlcp/3.d0-dlcp2/6.d0)
	    endif
	    i=i+1
	    goto 50
60	    nstar2(j)=i-1
	    write(6,*)' nstar2=',j,nstar2(j)
	    close(13)
          enddo
c		an interpolation to tstar2
	  if(tstar2.ge.tspec2(nspec2))then
	    do i=1,nstar2(nspec2)
	      wstar2(i)=xstar2(i,nspec2)
	      fstar2(i)=star2(i,nspec2)
	      jstar2=nstar2(nspec2)
	    enddo  
	  elseif(tstar2.le.tspec2(1))then
	    do i=1,nstar2(1)
	      wstar2(i)=xstar2(i,1)
	      fstar2(i)=star2(i,1)
	      jstar2=nstar2(1)
            enddo
	  else
	    call locate(tspec2,nspec2,tstar2,jj)
	    jstar2=nstar2(jj)
	    do i=1,nstar2(jj)
	      wstar2(i)=xstar2(i,jj)
	      fjj(i)=star2(i,jj)
	    enddo
	    do i=1,nstar2(jj+1)
	      wjj1(i)=xstar2(i,jj+1) 
              fjj1(i)=star2(i,jj+1) 
            enddo
	    call interp(wjj1,fjj1,wstar2,fjj1n,nstar2(jj+1),nstar2(jj))
	    do i=1,nstar2(jj)
	      der=(fjj1n(i)-fjj(i))/(tspec2(jj+1)-tspec2(jj))
	      fstar2(i)=der*(tstar2-tspec2(jj))+fjj(i)
	    enddo
	  endif          
	endif
c 	star3 -intrinsic nonrotated 3. intensity spectrum 
c		in erg/cm^2/s/Hz/sterad
c	xstar3(i) -lambda in [A]
	if(lunt3.gt.0)then
	  write(*,*)' reading file starspec3'
	  OPEN(14,FILE='starspec3',STATUS='OLD')
	  i=1
          if(lunt3.eq.3)then
            read(14,*,end=100)
            read(14,*,end=100)
          endif
90	  if(lunt3.eq.1)then
	    read(14,*,end=100)xstar3(i),star3(i)
	    if(i.gt.1)then
c	      fix synspec bug		    	    
	      if(xstar3(i).eq.xstar3(i-1))goto 90
	      if(.not.(xstar3(i).gt.xstar3(i-1)))then
                write(*,*)' input error in starspec3 on line ',i
              endif  
	      if(i+1.gt.mstarx)then
	        write(*,*)' error in starspec3: '
                write(*,*)'dimension mstarx almost exceeded'
              endif  
	    endif
c		convert input of Hlambda from synspec to our units
c		with no limb darkening
            xstar3(i)=xstar3(i)*xunt3
	    star3(i)=star3(i)*yunt3*4.d-8
	    star3(i)=star3(i)*xstar3(i)*xstar3(i)/clight
	  endif
	  if(lunt3.eq.2)then
	    read(14,*,end=100)xstar3(i),star3(i)
	    if(i.gt.1)then
	      if(.not.(xstar3(i).gt.xstar3(i-1)))then
                write(*,*)' input error in starspec3 on line ',i
              endif  
	      if(i+1.gt.mstarx)then
	        write(*,*)' error in starspec3: '
                write(*,*)'dimension mstarx almost exceeded'
              endif  
	    endif
	    xstar3(i)=xstar3(i)*xunt3
	    star3(i)=star3(i)*yunt3
	  endif
	  if(lunt3.eq.3)then
	    read(14,*,end=100)idummy,xstar3(i),dummy,star3(i)
	    if(i.gt.1)then
	      if(.not.(xstar3(i).gt.xstar3(i-1)))then
                write(*,*)' input error in starspec3 on line ',i
              endif  
	      if(i+1.gt.mstarx)then
	        write(*,*)' error in starspec3: '
                write(*,*)'dimension mstarx almost exceeded'
              endif  
	    endif
c               convert input of nu, Fnu from coolTlusty.21 to our units
	    xstar3(i)=xunt3*clight/xstar3(i)*1.d8
	    star3(i)=star3(i)*yunt3/pi
	  endif
	  i=i+1
	  goto 90
100	  nstar3=i-1
	  write(6,*)' nstar3=',nstar3
	  close(14)
	endif
	return
	end
c-----------------------------------------------------------------------
	subroutine lindat(iat,iion,wlab,elo,eup,glo,gr0,gs0,gw0,bij
     &  ,nline)
c 		Line data as in Kurucz linelist or in the SYNSPEC code
c       dll -wavelength [nm]
c       cod -element.ion cod, e.g. 26.02. It is interpreted as:
c         26=atomic number=iron, 02=2xtimes ionized i.e. FeIII line
c       gf -log_10 (gf) 
c       elo,eup -energy of the lower and upper level in [1/cm]
c       qlo -quantum number -J of the lower level[=>stat.weight=2*J+1] 
c       qup -quantum number -J of the upper level[=>stat.weight=2*J+1] 
c       gr0,gs0,gw0-radiative, Stark, Van der Waals damping constants
c 	iat - element atomic number
c 	wlab - lambda [A] 
c	freq0 -frequency [1/s]
c 	stio-ionization degree of the ion [1.-neutral,...] 
c 	bij-transition probability [1/s]=Einstein coef. per unit solid
c		angle
        implicit double precision (a-h,o-z)
	include 'param.inc'
	dimension iat(mline),iion(mline),wlab(mline)
        dimension elo(mline),eup(mline),glo(mline)
        dimension gr0(mline),gs0(mline),gw0(mline),bij(mline)
        write(*,*)' reading file line.dat'
	open(8,file='line.dat',status='old')
	i=1
10	read(8,*,err=20,end=20)dll,cod,gf,elo(i),qlo,eup(i),qup
     &  ,gr0(i),gs0(i),gw0(i)
	wlab(i)=dll*10.d0
	iat(i)=int(cod)
	stio=(cod-dint(cod))*100.d0+1.d0
	iion(i)=int(stio+0.5d0)
	glo(i)=2.d0*qlo+1.d0
	bij(i)=dexp(2.302585d0*gf)/glo(i)*wlab(i)/7.484d-7
	i=i+1
	goto 10
20	nline=i-1
	close(8)
	write(*,*)' No. of sp. lines read from line.dat=',nline
	return
	end
c-----------------------------------------------------------------------
	subroutine medop(ndim3,nivac,alam1,alamn,alam
     &  ,ophbf,ophbf1,ophbf2,ophff,ophff1,ophff2
     &  ,ophrs,ophrs1,ophrs2,ophn,ophn1,ophn2)
c	calculates the continuum opacities at the particular wavelength
c	via a simple linear interpolation in between the boundaries
c	input:  ndim3,nivac,alam1,alamn
c		alam -actual value of lambda[A]
c		ophbf1,ophbf2,ophff1,ophff2,ophrs1,ophrs2,ophn1,ophn2
c	output: ophbf,ophff,ophrs,ophn
 	implicit double precision (a-h,o-z)
	dimension ophbf(ndim3),ophbf1(ndim3),ophbf2(ndim3)   
        dimension ophff(ndim3),ophff1(ndim3),ophff2(ndim3)
        dimension ophrs(ndim3),ophrs1(ndim3),ophrs2(ndim3)
        dimension ophn(ndim3),ophn1(ndim3),ophn2(ndim3)
	falam=(alam-alam1)/(alamn-alam1)
	do kk=1,nivac
	  ophbf(kk)=(ophbf2(kk)-ophbf1(kk))*falam+ophbf1(kk)
	  ophff(kk)=(ophff2(kk)-ophff1(kk))*falam+ophff1(kk)
	  ophrs(kk)=(ophrs2(kk)-ophrs1(kk))*falam+ophrs1(kk)
	  ophn(kk)=(ophn2(kk)-ophn1(kk))*falam+ophn1(kk)
        enddo
	return
	end
c-----------------------------------------------------------------------
	subroutine fend(temp,ekonc,entot,zip,stavs,nion,endf,endfp)
c	subroutine calculates the electron number density function
c	and its first parcial derivative (after electron num.dens.) 
c	for one chemical element (valid also for H without H-, H2)
c	input:  temp-temperature
c		ekonc-electron number density
c		entot-number density of particular chemical element
c		zip-ionization potentials
c		stavs-partition functions
c		nion-the highest ion of the element considered
c	output:
c		endf-electron number density function
c		endfp-  \parc endf / \parc Ne
c	notation:
c       rr(j)- population of the J-th ion/total element population
c	rt(j)= Ne*N(j)/N(j-1)
c	rt1(j)= Ne**(j-1)*N(j)/N(1)
c	sahan	-calling function 
c		(Saha equation without el.num. dens. term)
c	
 	implicit double precision (a-h,o-z)
	include 'param.inc'
	dimension zip(mion-1),stavs(mion),rt(mion),rt1(mion)
c	if(nion.lt.2)then
c	  write(*,*)'error in subroutine: fend'
c	  stop
c	endif
	rt1(1)=1.d0
	a=ekonc**(nion)
	b=dble(nion)*ekonc**(nion-1)
	do 10 j=2,nion
	  rt(j)=sahan(temp,zip(j-1),stavs(j-1),stavs(j))
          rt1(j)=rt1(j-1)*rt(j)
	  nj1=nion-j+1
	  a=a+ekonc**nj1*rt1(j)
	  b=b+dble(nj1)*ekonc**(nion-j)*rt1(j)
10	continue
	endf=0.d0
	endfp=0.d0
	do 20 j=2,nion
	  nj1=nion-j+1
	  rr=ekonc**nj1*rt1(j)/a
	  rrp=(dble(nj1)*ekonc**(nion-j)*rt1(j)-rr*b)/a
	  pom=dble(j-1)*entot
	  endf=endf+pom*rr
	  endfp=endfp+pom*rrp
20	continue
	return
	end
c-----------------------------------------------------------------------
	subroutine fendh(temp,ekonc,entot,zip,stavs,endfh,endfhp)
c	subroutine calculates the electron number density function
c	and its first parcial derivative (after electron num.dens.) 
c	for Hydrogen including H-
c	input:  temp -temperature
c		ekonc -electron number density
c		entot -number density of particular chemical element
c		zip -ionization potentials
c		stavs -partition functions
c	output:
c		endfh -electron number density function
c		endfhp -  \parc endfh / \parc Ne
c	notation:
c       rr(j) - population of the J-th ion/total element population
c	rt(j)= Ne*N(j)/N(j-1)
c	sahan	-calling function 
c		(Saha equation without el.num. dens. term)
c	
 	implicit double precision (a-h,o-z)
	include 'param.inc'
	dimension zip(mion-1),stavs(mion)
        endfh=0.d0
        endfhp=0.d0
c	H- ionization potential	and partition function
	pothm=0.7552d0
	stavhm=1.d0
	rt2=sahan(temp,pothm,stavhm,stavs(1))
	rt3=sahan(temp,zip(1),stavs(1),stavs(2))
	bot=ekonc*ekonc+ekonc*rt2+rt3*rt2
c       n(H-)=rr1*n(H), n(HI)=rr2*n(H), n(HII)=rr3*n(H)	
	rr1=ekonc*ekonc/bot
c	rr2=ekonc*rt2/bot
	rr3=rt3*rt2/bot
        endfh=(rr3-rr1)*entot
	rr1p=(ekonc**2*rt2+2.d0*ekonc*rt3*rt2)/bot**2
	rr3p=-rt3*rt2*(2.d0*ekonc+rt2)/bot**2
        endfhp=(rr3p-rr1p)*entot
	return
	end
c-----------------------------------------------------------------------
	subroutine fendh2 (temp,ekonc,entot,zip,stavs,endfh,endfhp)
c	subroutine calculates the electron number density function
c	and its first parcial derivative (after electron num.dens.) 
c	for Hydrogen including HII,HI,H-,H2
c	input:  temp-temperature
c		ekonc-electron number density
c		entot-number density of particular chemical element
c		zip-ionization potentials
c		stavs-partition functions
c	output:
c		endfh-electron number density function
c		endfhp-  \parc endfh / \parc Ne
c       rr(j)- population of the J-th ion/total element population
c	rt(j)= Ne*N(j)/N(j-1)
c	sahan	-calling function 
c		(Saha equation without el.num. dens. term)
c	
 	implicit double precision (a-h,o-z)
	include 'param.inc'
	dimension zip(mion-1),stavs(mion)
        endfh=0.d0
        endfhp=0.d0
c	H- ionization potential	and partition function
	pothm=0.7552d0
	stavhm=1.d0
	if(temp.lt.1.d2)then
c	  rt1 will exceed 1.d300 i.e. infti for T<70K	
	  return
	endif
	if(temp.lt.2.d4)then
c         HII+HI+H-+H2	
c	  for densities< 1.d-20 there are problems for 1d4<T<2d4 K
c	  so you may use if(temp.lt.1.d4)then
	  rt1=sah2(temp)
	  rt2=sahan(temp,pothm,stavhm,stavs(1))
	  rt3=sahan(temp,zip(1),stavs(1),stavs(2))
	  rt3ek=rt3/ekonc
c	  write(*,'(a,3es15.5)')'r123',rt1,rt2,rt3
	  ekrt2=ekonc/rt2
	  b=0.5d0/rt1*(1.d0+rt3ek+ekrt2)
	  c=-0.5d0*entot/rt1
	  b2=b*b
	  d=dsqrt(b2-4.d0*c)
c	  write(*,*)'-4.d0*c/b2',-4.d0/b*c/b
	  if(-4.d0/b*c/b.lt.1.d-10)then
c           then avoid substracting two similar numbers -b+b
	    hi=-c/b
	    endfh=(rt3ek-ekrt2)*hi
	    hib=(c/b)/b
	    bne=0.5d0/rt1*(1.d0/rt2-rt3ek/ekonc)
	    hine=hib*bne
	    endfhp=(-rt3ek/ekonc-1.d0/rt2)*hi+(rt3ek-ekrt2)*hine
	  else
	    hi=0.5d0*(-b+d)
            endfh=(rt3ek-ekrt2)*hi
c	    hib=parc HI / parc b,  bne= parc b / parc ne
c	    hine= parc HI / parc ne 	
	    hib=0.5d0*(-1.d0+b/d)
	    bne=0.5d0/rt1*(1.d0/rt2-rt3ek/ekonc)
	    hine=hib*bne
            endfhp=(-rt3ek/ekonc-1.d0/rt2)*hi+(rt3ek-ekrt2)*hine
          endif
        else
c         HII+HI+H-
c         n(H-)=rr1*n(H), n(HI)=rr2*n(H), n(HII)=rr3*n(H)
 	  rt2=sahan(temp,pothm,stavhm,stavs(1))
	  rt3=sahan(temp,zip(1),stavs(1),stavs(2))
	  bot=ekonc*ekonc+ekonc*rt2+rt3*rt2
	  rr1=ekonc*ekonc/bot
c	  rr2=ekonc*rt2/bot
	  rr3=rt3*rt2/bot
          endfh=(rr3-rr1)*entot
	  bot2=bot**2
	  rr1p=(ekonc**2*rt2+2.d0*ekonc*rt3*rt2)/bot2
	  rr3p=-rt3*rt2*(2.d0*ekonc+rt2)/bot2
          endfhp=(rr3p-rr1p)*entot
        endif  
	return
	end
c-----------------------------------------------------------------------
        subroutine elnd(d,xi,necod,wm,abhyd,ftemp,fdens,fne
     &  ,nbod1,nbod2,nbod3,denvac,dcut1,ane0)
c       calculates electron number density from the temperature, density
c	and abundances using Newton-Raphson iteration method 
c	input:  
c		d(2,*) -abundance with respect to H
c		xi -first 8 ionization potentials
c		necod=1 -code for taking the element into account
c		wm -mean molecular weight in [hjed]
c		abhyd -H abundance relative to total elem.num.dens.
c		ftemp,fdens -temperature,density
c		nbod1,nbod2,nbod3 -number of xyz grid points
c		denvac,dcut1 -density limits for vacuum and opaque obj.
c		ane0 -fake electron num.dens beyond the limits
c	output:
c		fne-electron number density
c	notation:
c		xacc-accuracy
        implicit double precision (a-h,o-z)
        include 'param.inc'     
        dimension zip(mion-1),stavs(mion),u(5)
        dimension d(3,matom),xi(8,matom),necod(matom)
        dimension ftemp(ndimf1,ndimf2,ndimf3)
        dimension fdens(ndimf1,ndimf2,ndimf3)
        dimension fne(ndimf1,ndimf2,ndimf3)
	hjed=1.66053873d-24
	wmhj=hjed*wm
	xacc=1.d-3
	niter=35
	do 70 l=1,nbod1
	do 60 m=1,nbod2
	do 50 n=1,nbod3
	  if(fdens(l,m,n).lt.denvac.or.fdens(l,m,n).gt.dcut1)then
	    fne(l,m,n)=ane0
	    goto 50
	  endif
	  temp=ftemp(l,m,n)
	  ekonc=fdens(l,m,n)/wmhj
	  pom=ekonc*abhyd
 	  theta=5039.77d0/temp
c	  	iteration cycle
	  do i=1,niter
 	    call potlow(temp,ekonc,eplow)
	    endf=0.d0
	    endfp=0.d0
	    do iat=1,matom
	      if(necod(iat).eq.1)then
	      nion=int(d(3,iat)+0.5d0)
              do j=1,8
  	        zip(j)=xi(j,iat)
	      enddo
c	        PARTFN is in 'pfdwor.inc'
c	      mind, for temperatures over a few 1.d5 deg. partition f.
c	      might do strange things 
 	      call partfn(iat,theta,u,eplow) 
 	      do j=1,nion
 	        if(j.le.5)then
 	          stavs(j)=u(j)
 	        else
 	          stavs(j)=1.d0
 	        endif
	      enddo
	      entot=pom*d(2,iat)
	      if(iat.eq.1)then
c	        call fendh(temp,ekonc,entot,zip,stavs,endfi,endfpi)
	        call fendh2(temp,ekonc,entot,zip,stavs,endfi,endfpi)
	      else
                call fend(temp,ekonc,entot,zip,stavs,nion,endfi,endfpi)
              endif
	      endf=endf+endfi
	      endfp=endfp+endfpi
	      endif
            enddo
c	  	heart of iteration
	    dx=(endf-ekonc)/(endfp-1.d0)
	    if((ekonc-dx).lt.1.d-3*ekonc)then
	      ekonc=1.d-3*ekonc
	    else
	      ekonc=ekonc-dx
	    endif
	    if(dabs(dx/ekonc).lt.xacc)goto 45
	    if(i.eq.niter)then
	      write(*,*)'stop, slow/no convergence in subroutine: elnd'
	      stop
	    endif
          enddo
45	  fne(l,m,n)=ekonc
50 	continue
60 	continue
70 	continue
	return
	end
c-----------------------------------------------------------------------
	subroutine trans(x,y,z,trmt)
c	Assumes coordinate system e1,e2,e3.
c	Defines new coordinate system ep1,ep2,ep3 such that ep(3)
c	is along the chosen direction ep3=(x,y,z).
c	Optionaly defines ep2 axes such that ep2 is perpendicular 
c	to e3,ep3 i.e. ep2=e3xep3.
c	Calculates the transformation matrix trmt between 
c	the old and new rotated coordinates such that the new 
c	coordinates (Vp) are related to old coordinates (V):
c	Vp=trmt \times V.  
c	input: x,y,z   (chosen direction)
c 	output:trmt
	implicit double precision (a-h,o-z)
	dimension trmt(3,3),e(3,3),ep(3,3)
	e(1,1)=1.d0
	e(1,2)=0.d0
	e(1,3)=0.d0
	e(2,1)=0.d0
	e(2,2)=1.d0
	e(2,3)=0.d0
	e(3,1)=0.d0
	e(3,2)=0.d0
	e(3,3)=1.d0
	pom1=x*x+y*y
	pom2=dsqrt(pom1)
	pom3=dsqrt(x*x*z*z+y*y*z*z+pom1*pom1)
	if(pom2.lt.1.d-30)then
	  ep(1,1)=1.d0
	  ep(1,2)=0.d0
	  ep(1,3)=0.d0
          ep(2,1)=0.d0
          ep(2,2)=1.d0
          ep(2,3)=0.d0
 	else
	  ep(1,1)=x*z/pom3
	  ep(1,2)=y*z/pom3
	  ep(1,3)=-pom1/pom3
	  ep(2,1)=-y/pom2
	  ep(2,2)=x/pom2
	  ep(2,3)=0.d0
	endif
	ep(3,1)=x
	ep(3,2)=y
	ep(3,3)=z
	do i=1,3
	  do j=1,3
	    trmt(i,j)=ep(i,1)*e(j,1)+ep(i,2)*e(j,2)+ep(i,3)*e(j,3)
 	  enddo
	enddo
	return
	end
c-----------------------------------------------------------------------
        subroutine albedo(albx,alby,nalb,ialb)
c	subroutine reads the albedo of the star and companion from 
c	the table
c	albx(i) -wavelength in Angstrom
c	alby(i) -monochromatic albedo
        implicit double precision (a-h,o-z)
        include 'param.inc'
        dimension albx(mstarx),alby(mstarx)
        if(ialb.eq.1)then
          write(*,*)'reading file albedo1'
          open(12,file='albedo1',status='old')
          i=1
10        read(12,*,err=20,end=20)albx(i),alby(i)
          i=i+1
          goto 10
20	  nalb=i-1          
          close(12)
        endif
        if(ialb.eq.2)then
          write(*,*)'reading file albedo2'
          open(13,file='albedo2',status='old')
          i=1
30        read(13,*,err=40,end=40)albx(i),alby(i)
          i=i+1
          goto 30
40	  nalb=i-1          
          close(13)
        endif        
        return   
        end          
c-----------------------------------------------------------------------
	subroutine mie(opmiex,opmies,opmiea,nmie,pfmiex,pfang,pfmie
     &  ,npfmie,imie,imiepf,ndust,dtlow,dthig,drmf)
c       subroutine reads the Mie data for the dust from the tables:
c       dust_opac:
c       ndust -number of dust species (or separate input files)
c       dtlow, dthig -temperature range of each species [K]
c       drmf -species relative mas fraction within the dust <0,1>
c       file -file names of the tables with: 
c       opmiex(i) -frequency in Hz
c       opmies(i) -scattering opacity per gram of dust material
c       opmiea(i) -absorption opacity per gram of dust material
c       mie_phase:
c	pfmiex(i) -frequency in Hz 
c	pfang(j)  -angle in deg. (cos of the angle on output)
c	pfmie(i,j) -phase function at frequency -i, angle -j,
c		normalised over all space angles to 4pi
c
c       nmie -number of frequencies in dust_opac files
c       npfmie -number of frequencies in mie_phase
c       npfang -number of angles for the Mie phase function 
c	imie -option of threating dust opacity & emissivity
c       imiepf -extra option for treating angle dependent scattering
        implicit double precision (a-h,o-z)
        include 'param.inc'
        character*50 file
        dimension dtlow(mspecx),dthig(mspecx),drmf(mspecx),file(mspecx)
        dimension nmie(mspecx)
	dimension opmiex(mstarx,mspecx)
	dimension opmies(mstarx,mspecx),opmiea(mstarx,mspecx) 
	dimension pfmiex(mstarx),pfang(npfang),pfmie(mstarx,npfang)
	pi=3.1415926535897931d0
	clight=2.99792458d10
	write(*,*)'reading file dust_opac'
	open(12,file='dust_opac',status='old')
	read(12,*)
	read(12,*)ndust,idtab
c	idtab -table format (1-Budaj, 2-Semenov)	
	read(12,*)
	do i=1,ndust
	  read(12,*)dtlow(i),dthig(i),drmf(i)
	enddo 
	read(12,*)
	do i=1,ndust
	  read(12,'(a)')file(i)
        enddo 
        close(12)
	if(idtab.eq.1)then
c	Budaj format, opacities [cm**2/g] per gram of dust
	  do j=1,ndust
            open(12,file=file(j),status='old')
            i=1
10    	    read(12,*,err=20,end=20)a,b,opmies(i,j),opmiea(i,j)
	    opmiex(i,j)=clight/b*1.d4
            i=i+1
            goto 10
20          nmie(j)=i-1 
            close(12)
          enddo
        else
c	Semenov format, opacities [cm**2/g] per gram of dust+gas        
c	multiply drmf by 100. (gas/dust ratio) in dust_opac
	  do j=1,ndust
            open(12,file=file(j),status='old')
            i=1
23    	    read(12,*,err=25,end=25)a,b,c,opmiea(i,j),opmies(i,j)
	    opmiex(i,j)=clight/a*1.d4
            i=i+1
            goto 23
25          nmie(j)=i-1 
            close(12)
          enddo
        endif  
        write(*,'(a,i3)')' dust species=',ndust
        do i=1,ndust
          write(*,'(a,i3,i5)')' freq. points=',i,nmie(i)
        enddo  
c        	phase functions
	if(imiepf.eq.1)then
	  write(*,*)'reading file mie_phase'
          open(13,file='mie_phase',status='old')
          i=1
30        read(13,*,err=40,end=40)pfmiex(i)
	  do j=1,npfang
	    read(13,*)pfang(j),pfmie(i,j)
	    pfang(j)=pi/180.d0*pfang(j)
	  enddo
	  sum=0.d0
	  do j=2,npfang
	    sump=pfmie(i,j-1)*dsin(pfang(j-1))
	    sump=sump+pfmie(i,j)*dsin(pfang(j))
	    sump=sump*(pfang(j)-pfang(j-1))
	    sum=sum+sump
	  enddo
	  sum=sum/2.d0
	  do j=1,npfang
	    pfang(j)=dcos(pfang(j))
	    pfmie(i,j)=2.d0*pfmie(i,j)/sum
	  enddo
          i=i+1
          goto 30
40        npfmie=i-1 
          close(13) 
        endif  
	return   
        end 
c-----------------------------------------------------------------------
	subroutine interp(x,y,xnew,ynew,ndat,nnew)
c	interpolates in y(x) and returns ynew=y(xnew) 	
c	assumes that x is monotonically increasing
c	input fields: x,y,xnew
c	output field: ynew
        implicit double precision (a-h,o-z)
        dimension x(ndat),y(ndat),xnew(nnew),ynew(nnew)
        jj=1
	do i=1,nnew
	  if(xnew(i).ge.x(ndat))then
	    ynew(i)=y(ndat)
	  elseif(xnew(i).le.x(1))then
	    ynew(i)=y(1)
	  else
	    call hunt(x,ndat,xnew(i),jj)
	    der=(y(jj+1)-y(jj))/(x(jj+1)-x(jj))
	    ynew(i)=der*(xnew(i)-x(jj))+y(jj)
	  endif  
	enddo
        return       
        end
c-----------------------------------------------------------------------
	subroutine intrp(x,y,ndat,xvalue,yvalue)
c	general subroutine for linear interpolation	
c	x may be increasing or decreasing
c	input: x, y, xvalue
c	output: yvalue
	implicit double precision (a-h,o-z)
	include 'param.inc'
	dimension x(mstarx),y(mstarx)
	if(x(1).lt.x(ndat).and.xvalue.le.x(1))then
	  yvalue=y(1)
	elseif(x(1).lt.x(ndat).and.xvalue.ge.x(ndat))then
          yvalue=y(ndat)
	elseif(x(1).gt.x(ndat).and.xvalue.ge.x(1))then
	  yvalue=y(1)
	elseif(x(1).gt.x(ndat).and.xvalue.le.x(ndat))then
	  yvalue=y(ndat)
	else
          call locate(x,ndat,xvalue,jj)
          der=(y(jj+1)-y(jj))/(x(jj+1)-x(jj))
          yvalue=der*(xvalue-x(jj))+y(jj)
	endif                         
	return     
        end 
c-----------------------------------------------------------------------
	subroutine intrp2(x,pfmie,ndat,xvalue,pfmief)
c	special linear interpolation
c	x may be increasing or decreasing
c	input: x, pfmie, xvalue
c	output: pfmief
	implicit double precision (a-h,o-z)
	include 'param.inc'
	dimension x(mstarx),pfmie(mstarx,npfang),pfmief(npfang)
	if(x(1).le.x(ndat).and.xvalue.le.x(1))then
	  do i=1,npfang
	    pfmief(i)=pfmie(1,i)
	  enddo  
	elseif(x(1).le.x(ndat).and.xvalue.ge.x(ndat))then
	  do i=1,npfang
            pfmief(i)=pfmie(ndat,i)
          enddo  
	elseif(x(1).ge.x(ndat).and.xvalue.ge.x(1))then
	  do i=1,npfang
             pfmief(i)=pfmie(1,i)
          enddo  
	elseif(x(1).ge.x(ndat).and.xvalue.le.x(ndat))then
          do i=1,npfang
            pfmief(i)=pfmie(ndat,i) 
          enddo         
	else
	  do i=1,npfang
            call locate(x,ndat,xvalue,jj)
            der=(pfmie(jj+1,i)-pfmie(jj,i))/(x(jj+1)-x(jj))
            pfmief(i)=der*(xvalue-x(jj))+pfmie(jj,i)
          enddo  
	endif                         
	return     
        end         
c-----------------------------------------------------------------------
	subroutine xsec(alam1,alamn,cutoff,xsecx,xsecy,txsec,nxsec,ny
     &  ,amixr)
c	reads extra cross-section from EXOMOL for gas molecules
c	x -wavenumber=1/lamda is increasing
c       y -cross-section for several temperatures
c       xsecx - frequency [Hz]
c       xsecy -cross-section [cm^2] for several temperatures
c       txsec -temperatures [K]
c	ny -muber of temperatures
c       nxsec -number of frequency points
c	amixr -abundance (mixing ratio) of the molecules 
c		relative to the H nuclei number density
c	input: 'gas_opac', alam1, alamn, cutoff [Ang]
c	output: xsecx,xsecy,txsec,nxsec,ny,amixr
	implicit double precision (a-h,o-z)
	include 'param.inc'
	dimension xsecx(mstarx),xsecy(mstarx,mspecx)
	dimension txsec(mspecx)
	clight=2.99792458d10
	xsec1=1.d8/(alam1-cutoff)
	xsecn=1.d8/(alamn+cutoff)
	write(*,*)'reading file gas_opac'
	open(16,file='gas_opac',status='old')
	read(16,*)nx,ny,amixr
	if(ny.lt.2.or.ny.gt.mspecx)then
	  stop'gas_opac: ny.lt.2.or.ny.gt.mspecx'
	endif
	if(xsecn.gt.xsec1)then
	  stop'gas_opac: xsecn.gt.xsec1'
        endif
	read(16,*)(txsec(i),i=1,ny)
c	write(*,*)(txsec(i),i=1,ny)
	nxsec=0
	i=1
	j=1
10	read(16,*,end=30,err=30)x
	if(j.eq.1.and.x.gt.xsec1)then
	  stop'gas_opac: x.gt.xsec1'
	endif  
	j=j+1
	if(x.ge.xsecn)then
          if(i.eq.1)then
	    if(j.gt.2)then
	      backspace(16)
              backspace(16)
            else
              backspace(16)
            endif
          endif  
20        read(16,*,end=30,err=30)xsecx(i),(xsecy(i,j),j=1,ny)
c		write(*,*)i,xsecx(i)
	  if(xsecx(i).gt.xsec1)then
            nxsec=i
	    goto 40
	  endif  
	  i=i+1
          goto 20
	endif
	goto 10
30      nxsec=i-1
40	write(6,*)'nxsec=',nxsec
	close(16)
	if(nxsec.gt.mstarx)then
	  stop'gas_opac: nxsec.gt.mstarx'
	endif
	do i=1,nxsec
	  xsecx(i)=xsecx(i)*clight
	enddo
	return     
        end         
c-----------------------------------------------------------------------
	subroutine int2d(x,y,z,nx,ny,xvalue,yvalue,zvalue)
c	subroutine for interpolation in 2D EXOMOL data	
c	x may be increasing or decreasing
c       y may be increasing or decreasing
c	input: x, y, z, xvalue, yvalue
c	output: zvalue
	implicit double precision (a-h,o-z)
	include 'param.inc'
	dimension x(mstarx),y(mspecx),z(mstarx,mspecx)
c	dimension x(nx),y(ny),z(nx,ny)
	if(x(1).lt.x(nx).and.xvalue.le.x(1))then
	  zvalue=0.d0
	elseif(x(1).lt.x(nx).and.xvalue.ge.x(nx))then
          zvalue=0.d0
	elseif(x(1).gt.x(nx).and.xvalue.ge.x(1))then
	  zvalue=0.d0
	elseif(x(1).gt.x(nx).and.xvalue.le.x(nx))then
	  zvalue=0.d0
	elseif(y(1).lt.y(ny).and.yvalue.le.y(1))then
	  zvalue=0.d0
	elseif(y(1).lt.y(ny).and.yvalue.ge.y(ny))then
          zvalue=0.d0
c		extend the table beyond the edge
c          call locate(x,nx,xvalue,i)
c          dz=(z(i+1,ny)-z(i,ny))
c          dx=(xvalue-x(i))/(x(i+1)-x(i))
c          zvalue=z(i,ny)+dz*dx
	elseif(y(1).gt.y(ny).and.yvalue.ge.y(1))then
	  zvalue=0.d0
	elseif(y(1).gt.y(ny).and.yvalue.le.x(ny))then
	  zvalue=0.d0
	else
          call locate(x,nx,xvalue,i)
          call locate(y,ny,yvalue,j)
c		linear interp., 30% faster
c          dzdx=(z(i+1,j)-z(i,j))/(x(i+1)-x(i))
c          dzdy=(z(i,j+1)-z(i,j))/(y(j+1)-y(j))
c          dx=xvalue-x(i)
c          dy=yvalue-y(j)
c          zvalue=z(i,j)+dzdx*dx+dzdy*dy
c		linear in lambda
	  dz=(z(i+1,j)-z(i,j))
	  dx=(xvalue-x(i))/(x(i+1)-x(i))
	  zz1=z(i,j)+dz*dx
	  dz=(z(i+1,j+1)-z(i,j+1))
	  zz2=z(i,j+1)+dz*dx 
c		exponential in temp
	  b=dlog(zz1/zz2)/(1.d0/y(j+1)-1.d0/y(j))
	  a=zz1*dexp(b/y(j))
	  zvalue=a*dexp(-b/yvalue)
	endif          
	return     
        end 
c-----------------------------------------------------------------------
 	subroutine chem(pop,popt,popd,mtemp,mdens,melm)
c	reads chemistry table with equilibrium molecular populations
c	pop,popt,popd -log10 of population, temperature, density
	implicit none
	integer, parameter:: i4=4,i8=8
c	integer, parameter:: mtemp=57,mdens=73,melm=11
	integer:: mtemp,mdens,melm
	integer:: i,j,k,l
	integer:: index(melm)
	character(18):: a18(melm)
	real(i8):: popt(mtemp),popd(mdens),pop(mtemp,mdens,melm)
	write(*,*)' reading file chem_eq_tab'
	open(16,file='chem_eq_tab',status='unknown')
        do i=1,mtemp
          read(16,*)
          read(16,30)popt(i),(popd(j),j=1,mdens)
30        format(6x,18x,f18.5,73f14.2)
          do k=1,melm
            read(16,40)index(k),a18(k),(pop(i,j,k),j=1,mdens)
          enddo  
        enddo
40      format(i6,18x,a18,73f14.5)
	close(16)
 	return
 	end
!-----------------------------------------------------------------------        
        subroutine abnd(pop,popt,popd,mtemp,mdens,melm                  &
     &  ,temp,dens,ndepth,mdepth,iopac,amixr,amixf)
!	CO molecule population from the table
!	input: amixr -molecule abundance relative to H nuclei
!         pop(mtemp,mdens,melm) -log10num.dens(log10temp,log10dens)
!	  popt -log10 temperature, popd -log10 density
!	output: amixf -number density of molecule imol in cm^-3
	implicit none
	integer, parameter:: i4=4,i8=8
	integer:: mtemp,mdens,melm,ndepth,mdepth,i,j,iopac,imol
	real(i8):: t,d,p
	real(i8):: hjed,wm,enh,amixr
	real(i8):: popt(mtemp),popd(mdens),pop(mtemp,mdens,melm)
	real(i8):: dens(mdepth),temp(mdepth)
	real(i8):: amixf(mdepth),popmol(mtemp,mdens)
!	index of HI,H2,CO,H2O	
!	ih=1 ih2=5 ico=6 iw=8
	imol=6
        hjed=1.66053873d-24
!       mean atomic weight for solar composition gas
        wm=1.3d0
!       calculate C,O abundance relative to H
!       abc=2.7d-4 abo=4.9d-4
	do i=1,mtemp
	do j=1,mdens
	  popmol(i,j)=pop(i,j,imol)
	enddo
	enddo
	do i=1,ndepth
	  if(temp(i).lt.255.d0)then
	    enh=0.9d0*dens(i)/wm/hjed
	    amixf(i)=amixr*enh
	  elseif(temp(i).gt.6300.d0)then
	    amixf(i)=0.d0
	  elseif(dens(i).lt.1.d-19.or.dens(i).gt.1.d-1)then
	    enh=0.9d0*dens(i)/wm/hjed
	    amixf(i)=amixr*enh
	  else
	    t=dlog10(temp(i))
	    d=dlog10(dens(i))
	    call int2b(popt,popd,popmol,mtemp,mdens,mtemp,mdens,t,d,p)
	    amixf(i)=10**p
	  endif  
	enddo
        return
        end
!-----------------------------------------------------------------------
	subroutine int2b(x,y,z,nx,ny,mx,my,xvalue,yvalue,zvalue)
!	subroutine for interpolation in 2D
!	x may be increasing or decreasing
!       y may be increasing or decreasing
!	input: x, y, z, xvalue, yvalue
!	output: zvalue
	implicit double precision (a-h,o-z)
	dimension x(mx),y(my),z(mx,my)
	if(x(1).lt.x(nx).and.xvalue.le.x(1))then
	  zvalue=0.d0
	elseif(x(1).lt.x(nx).and.xvalue.ge.x(nx))then
          zvalue=0.d0
	elseif(x(1).gt.x(nx).and.xvalue.ge.x(1))then
	  zvalue=0.d0
	elseif(x(1).gt.x(nx).and.xvalue.le.x(nx))then
	  zvalue=0.d0
	elseif(y(1).lt.y(ny).and.yvalue.le.y(1))then
	  zvalue=0.d0
	elseif(y(1).lt.y(ny).and.yvalue.ge.y(ny))then
          zvalue=0.d0
!		extend the table beyond the edge
!          call locate(x,nx,xvalue,i)
!          dz=(z(i+1,ny)-z(i,ny))
!          dx=(xvalue-x(i))/(x(i+1)-x(i))
!          zvalue=z(i,ny)+dz*dx
	elseif(y(1).gt.y(ny).and.yvalue.ge.y(1))then
	  zvalue=0.d0
	elseif(y(1).gt.y(ny).and.yvalue.le.x(ny))then
	  zvalue=0.d0
	else
          call locate(x,nx,xvalue,i)
          call locate(y,ny,yvalue,j)
c		linear interp.
          dzdx=(z(i+1,j)-z(i,j))/(x(i+1)-x(i))
          dzdy=(z(i,j+1)-z(i,j))/(y(j+1)-y(j))
          dx=xvalue-x(i)
          dy=yvalue-y(j)
          zvalue=z(i,j)+dzdx*dx+dzdy*dy
	endif          
	return     
        end 
c-----------------------------------------------------------------------
	include 'pfdwor.inc'
