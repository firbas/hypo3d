
		program HYPO
C(),<910712.1126>
c
c***************************************************************************
c
c             program   H Y P O 1 D  resp.  H Y P O 3 D
c
c     - locates hypocenter in 1-D model  resp. 3-D model
c
c     - iteration process is ended in three cases:
c       1/ difference in coord. of two following iterations is less then
c          min_shift parameters
c       2/ number of iterations is greater then nIter
c       3/ matrix C is singular
c       4/ break flag is set and operator choices end of iteration
c
c     - input data are taken from these files:
c       1) file containing crustal model and station list
c       2) file with known sources
c       3) file with start points
c       4) file with surface definition
c       5) xHEADER.DAT, xSTATIO.DAT
c       6) output hypofile from VISUAL, GRAF
c  ************
c  declarations
c  ************
c
		implicit none

c  local parameters
c
		integer maxIter
		real    max_change_z
c
		parameter (maxIter = 50)            !max. number of iterations
		parameter (max_change_z = 5.)       !max. change of z-coord. in 1 it

c  global parameters
c

      real    min_shift_epi               !minimum shift in epicenter coord.
      real    min_shift_depth             !minimum shift in depth
  
      parameter (min_shift_epi  =0.005)
      parameter (min_shift_depth=0.010)
		include 'param.fi'
		include 'pname.fi'
		include 'list.fi'
c
c  local variables
c
		logical loc_write                   !locfile was written in menu block?
		logical rms_on_sphere               !compute rms on sphere centered on hypocenter
		logical prt                         !printed?
		logical rp                          !repeat location flag
		real*8  dmin8                       !reference time in double real
		real    absd                        !last shift of hypocenter
		real    best_x0,best_y0,best_z0,best_t0
		character*1     old_it          !symbol for iteration with rms
						!of res greater then previous succ.
						!iteration
		real    c_hypo1(3)                  !epicenter local coordinates
		real    bx0, by0, bz0 !, bt0          !values of succesful iteration
		real    rmsresp                     !previous rms of residuals
		real*8  sum8                        !summa in real*8
		real    xp,yp,zp                    !auxiliary variables
		real    d(4)                        !shift vector
		real    zsurf                       !value of fix_surface
		real    vd(10)                      !auxiliary field
		integer n_increase                  !# of increasing in origin time norm
		integer n_of_location               !# of locations
		integer i_menu                      !selected way in menu
		integer ios                         !error variable
		integer n_recomp_it                 !number of recomputing of iter.
		integer i,j                         !aux. var.

		integer string_length
		character*255 string, hyponame, hyp3name
		character*255 ch_model_name   !name of crustal model

c
c  global variables ... common blocks
c
      real         model_error            !estimated error of model
                                          !in miliseconds
      real         reading_error          !estimated reading error in ms
                                          !(two sample intervals)
      common /err/ model_error,reading_error
 
c
c  common for space and rms data of hypocenter
c
		real           x_orig,y_orig,z_orig
		real           rms_orig
                integer        no_iter_orig
		common /org/   x_orig,y_orig,z_orig,rms_orig,no_iter_orig
c
c  common for damping factor
c
		real*8         sigma(4)
		common /sigm/  sigma
c
c  common for time data
c
		integer year
		integer month
		integer day
		integer hour
		integer minute
		real    t0
		common /otime/      year,month,day,hour,minute,t0
c
c  common for data of origin time
c
		integer             year_orig
		integer             month_orig
		integer             day_orig
		integer             hour_orig
		integer             minute_orig
		real                t_orig
		common /origin/     year_orig,month_orig,day_orig,hour_orig,
     >                    minute_orig,t_orig
c
c  common for coord. of trial hypocenter
c
		real                x0,y0,z0        !coord. of trial hypocenter
		common /centr/      x0,y0,z0
c
c  common for data for covariance matrix evaluation
c
		real            co(4,4)
		real            rmsres
		real            rmsres_co
		common /cov/    co,rmsres,rmsres_co
c
c  common for data of recording situ (space coordinates, delay; no. of arr.)
c
		integer             nrec            !no. of arrivals
		real                xstat(nStation) !\
		real                ystat(nStation) ! >coordinates of stations
		real                zstat(nStation) !/
		real                dly(nStation)   !stations delays for surf. events
		common /rec/        nrec,xstat,ystat,zstat,dly
c
c  common for evaluated beam data (travel time and its derivatives)
c
		real                tcal(nrec_max)  !calc. travel times
		real                xc(4,nrec_max)  !travel time derivatives
		common /cal_time/   tcal,xc
c
c  common for flags of fixed depth (fixed surface, fixed depth), no. of it.
c
		logical             fix_depth       !fixed depth?
		logical             fix_surface     !fixed surface?
		integer             i0              !no. of iter. cycle
		common /srfc/       fix_surface,fix_depth,i0
c
c  common of hypofile items ... noncharacter part, average weight
c
                logical             hyr
		real                trec(nrec_max)  !observed times
		real                wt(nrec_max)    !weight
		real                avwt            !average weight
                real                sumw,sumw2
		common /hyp/        hyr,trec,wt,avwt,sumw,sumw2
c
c  common for subprogram iteration_1 ... noncharacter part
c
		real            c_hypo(3)           !coord. of hypocenter
		integer         no_valid_arrivals   !no. of valid arrivals
		logical         t0_norm             !norm of origin time?
		logical         endit               !end of iteration process?
		common /it1/    t0_norm,c_hypo,no_valid_arrivals,endit
c
c  common for subprogram iteration_2
c
		real*8          c(4,4)              !Hessian matrix resp. inv. Hess. m.
		real*8          b(4)                !vector of right side
		real*8          det                 !determinant of matrix c
		real*8          scale(4)            !scale vector for Hessian matrix
		common /it2/    c,b,det,scale
c
c  common for scan depth mode
c
		logical         scan_depth
		real            scan_start
		real            scan_end
		real            scan_step
		common /scan/   scan_depth,scan_start,scan_end,scan_step
c
c  common for start point data
c
		real            x_start
		real            y_start
		real            z_start
		real            ot_start
		common /start/  x_start,y_start,z_start,ot_start
c
c  common for data of start (trial) point for scan depth mode
c
		real              x_start_trial
		real              y_start_trial
		common /t_start/  x_start_trial,y_start_trial
c
c  common for data for rms_on_net mode
c
		logical         rms_on_net
		real            start_x
		real            start_y
		real            start_depth
		real            start_otime
		real            end_y
		real            end_x
		real            end_depth
		real            end_otime
		real            step_x
		real            step_y
		real            step_depth
		real            step_otime
		common /rmsnet/ rms_on_net,
     >                start_x,end_x,step_x,
     >                start_y,end_y,step_y,
     >                start_depth,end_depth,step_depth,
     >                start_otime,end_otime,step_otime
c
c  common for flags of fixed coord.
c
		logical         fix_x
		logical         fix_y
		logical         fix_otime
		common /f_mode/ fix_x,fix_y,fix_otime
c
c  common for symbols for output
c
		character*1     symbol_x
		character*1     symbol_y
		character*1     symbol_depth
		character*1     symbol_otime
		common /symb/   symbol_x,symbol_y,symbol_depth,symbol_otime
c
c  common for flag of existence of wrong station name
c
		logical        err_stname
		logical        w_changes
		common /ernam/ err_stname,w_changes
c
c  common for flag for locfile writting
c
		logical       loc                   !was written locfile?
		common /wloc/ loc
c
c	common for file names - VD
c
		character*255 hypfn
		character*255 modfn
		common /hymofn/	hypfn,modfn
c  functions
c
		integer iargc, lnblnk
c
c common for ray profile coordinates
c
c		integer n_poi
c		real poi(2*z_layer)
c		real z_coor(2*z_layer)
c      common  /ray/ n_poi, poi, z_coor
c

c  *******************
c  end of declarations
c  *******************
c
c=============================================================================
c
c
c   get and decode command line arguments -VD
c
c  initialization of model_error variable (and other variables -VD)
c
      model_error=-1.0
	ch_model_name=' '
	hyponame=' '
	hyp3name=' '
c
c  for the first: get runstring
c
      	j=iargc()
c
c  for the second: decode parameters
c
      do i=1,j
          call getarg(i,string)
 	string_length=lnblnk(string)         
		if (string(1:2).eq.'-?'.or.string(1:2).eq.'-X'.or.
     *		string(1:2).eq.'-x') then
c
c  call runstring parameters information
c
      		write(*,*) 'Online information is not available'	
c          	else if (string(1:2).eq.'-D'.or.string(1:2).eq.'-d') then
c
          	else if (string(1:2).eq.'-i'.or.string(1:2).eq.'-I') then
c
c  name of first hypofile to location
c
			hyponame=string(3:lnblnk(string))

          	else if (string(1:2).eq.'-O'.or.string(1:2).eq.'-o') then
c
c  name of output file
c
			hyp3name=string(3:lnblnk(string))

c        
	 	else if (string(1:2).eq.'-M'.or.string(1:2).eq.'-m') then
c
c  name of model file
c
                  ch_model_name(1:string_length-2)=
     >                          string(3:string_length)
          else
              write (*,
     >        '(1x,a,": Error - wrong parameter ",a)')
     >         prog_name,string(1:2)
          endif
      end do  					! decoding of parameters
c
	if(hyponame.eq.' ') then
		write(*,*) 'Error: Hypofile name not given'
		write(*,*) 'Usage: hypo3d -ia001.hyp -oa001.hy3 -mkra_3d_a.mod'
		call exit
	endif
	if(hyp3name.eq.' ') then
		write(*,*) 'Error: Hypofile name not given'
		write(*,*) 'Usage: hypo3d -ia001.hyp -oa001.hy3 -mkra_3d_a.mod'
		call exit
	endif
	if (ch_model_name.eq.' ') then
c	complete path to hypofile probably defined => the velocity model
c 	name must NOT be derived => option -M required
		write(*,*) 'Error: Modelfile name not given'
		write(*,*) 'Usage: hypo3d -ia001.hyp -oa001.hy3 -mkra_3d_a.mod'
	   call exit
	endif
	hypfn=hyponame
	modfn=ch_model_name
c
c	
	write(*,*) 'Hypofile is  ',hypfn(1:lnblnk(hypfn))
	write(*,*) 'Model is  ',modfn(1:lnblnk(hypfn))
	rms_on_sphere = .false.
	loc_write     = .false.
	reading_error = 0.016 		!estimated reading error in ms
						!(two sample intervals)
c  header of the program
c
		write (*,'(1x,a)') long_prog_name
		write (*,'(1x,a,//)') prog_name2
c
c  init. loc-write flag
c
		loc=.false.
c  init. # of location
c
		n_of_location=0

c
c  increment number of locations
c
		n_of_location=n_of_location+1
					 
c  input of hypofile, crustal model, initialize of spline surface common
c
		call i_hyp_mod

		if (nrec .lt. 3) then
			 write (*,
     >    '(1x,a,": Error - no. of arrivals in hypofile ",
     >    " < 3.")') prog_name
			 call exit
		endif
		rp=.false.
90    continue
c
c  test on repeat of location
c
		if (rp) then
c
c  check datum, compute no_valid_arrival
c
			 call rec_time_name
     >    (.false.,rp,no_valid_arrivals,dmin8,n_increase,2)
		else
c
c  check names of recording stations, datum
c
			 call rec_time_name
     >    (.false.,rp,no_valid_arrivals,dmin8,n_increase,0)
c
c  test on number of arrivals
c
			 if (nrec.lt.3) then
c
c  go to the next hypofile if any
c
c			     go to 10
			    call exit
			 endif
		endif
c
c  interactive part ... choice of fixed coordinates, start point, location
c   mode (various starting points, rms of res. on net, scanned depth)
c
		call dialog_3
c
c  search the nearest station
c
		call rec_time_name
     >(.false.,rp,no_valid_arrivals,dmin8,n_increase,1)
c
c  test on scanned depth mode
c
		if (scan_depth) then
c  write header for scanned depth mode
c
			 call dialog_1 (dmin8)
		endif
c
c  number of valid arrivals must be 3 at least
c
		if (no_valid_arrivals  .lt. 3) then
			 write (*,'(1x,a,": # of arrivals in hypofile  <  3 "/
     >    "         Try another hypofile.")') prog_name
c
c  fatal situation in original hypofile
c
			     call abort
c
		else if (no_valid_arrivals.eq.3 .and. .not.fix_depth
     >       .and. .not.fix_surface  .and. .not.scan_depth
     >       .and. .not.fix_x  .and. .not.fix_y
     >       .and. .not.fix_otime) then
c
c  in the case of undetermined problem give a chance to location with fixed
c   depth
c
			 fix_depth=.true.
c
115       continue
			 write (*,'(1x,a,": Only 3 valid arrivals.",/,
     >                "         Enter value of fixed depth:_")')
     >    prog_name
c
c  reset value of z-coor. of start point
c
			 read (*,*,err=115,end=120,iostat=ios) z_start
			 go to 125
c
120       continue
c
c  end of program
c
			 call abort
c
125       continue
c
c  set value of fixed depth
c
			 z0=z_start
c
c  set graphics symbol for fixed depth
c
			 symbol_depth='*'
c
c  in this case reset rms_on_net mode
c
			 rms_on_net=.false.
c
		endif
c
c  test on rms_on_net mode
c
		if (rms_on_net) then
c  init. start of intervals, write header for rms_on_net mode
c
			 call rms_net_1(dmin8)
		endif
c
c  transform hypocenter coord. to Krovak coordinates for surface computing
c
		c_hypo1(1)=x0
		c_hypo1(2)=y0
		c_hypo1(3)=z0
c
c  give transformation
c
		call trans(c_hypo1(1),c_hypo1(2),c_hypo1(3),2)
c  z-coordinate of surface for epicenter coordinates c_hypo(1),c_hypo(2)
c  in computing of z-coordinate of surface ... z-axis is upward
c
		call spline_value (0,c_hypo1,vd)
		zsurf=-vd(1)
c
c  test whether given start point isn't above the surface
c
		if (z0.lt.zsurf) then
			 z0 = zsurf
		endif
c
c  test on fixed surface mode
c
		if (fix_surface) then
c
c  fixed surface mode
c
c  set value of z-coord. for surface in (x0,y0)
c
			 z0=zsurf
		endif
c
c  test on location mode
c
		if (.not.scan_depth .and. .not.rms_on_net) then
c
c  write header for succ. iterations
c
			 write(*,2030)
 2030     format(//'  SUCCESSIVE APPROXIMATIONS'//
     >'        |   LOCATION OF HYPOCENTER   | ORIGIN |   RMS   | SHIFT '
     >    ,'OF'/
     >' APROX. | X-COOR.   Y-COOR.   Z-COOR.|  TIME  | OF RES. | HYPO',
     >    'CENTER'/)
		endif
c
c  initialize derivative on time for origin time (is equal to one)
c   compute average weight
c
		avwt=0.0
                sumw=0.0
                sumw2=0.0
		do  i=1,nrec
			 sumw=sumw+wt(i)
			 sumw2=sumw2+wt(i)*wt(i)
		end do
		avwt=sumw/no_valid_arrivals
c ---------------------------------------------------------------------
c 2018-10 10.69
		do  i=1,nrec
			 xc(4,i)=1.0
                         wt(i)=wt(i)/avwt
		end do
c ---------------------------------------------------------------------
c
c  scanned depth
c
		if (scan_depth) then
c
c  init. start in depth for scanned depth mode
c
			 z0=scan_start
		endif
c
c  cycle for scaling depth
c
29    continue
c
c  initialization
c
		rmsresp= 1.0e+06
		rp = .false.
                no_iter_orig=i0
		i0 = 0
		absd = 0.
		endit=.false.
		prt=.false.
c
c  test on rms_on_sphere mode
c
		if (rms_on_sphere) then
			 call sphere_step (i0,rmsres,endit,t0_norm)
c
c  transform local to Krovak coordinates for surface computing
c
			 c_hypo1(1)=x0
			 c_hypo1(2)=y0
			 c_hypo1(3)=z0
c
			 call trans(c_hypo1(1),c_hypo1(2),c_hypo1(3),2)
c  z-coordinate of surface for epicenter coordinates c_hypo(1),c_hypo(2)
c  in computing of z-coordinate of surface ... z-axis is upward
c
			 call spline_value (0,c_hypo1,vd)
			 zsurf=-vd(1)
c
c  test on point in air
c
			 if (z0.lt.zsurf) then
			     z0=zsurf
			 endif
		endif
c
c  successive iteration cycle
c  **************************
c
30    continue
c
c  increment i0 ... variable for no. of iterations
c
		i0=i0+1
c
c  travel time and derivatives for hypocenter coordinate x0, y0, z0
c
		c_hypo(1)=x0
		c_hypo(2)=y0
		c_hypo(3)=z0
c  evaluate travel times and derivatives for every recording station
c
		call td_all (c_hypo)
c  first part of iteration process: init. t0, compute rmsres, rmsres_co
c
		call iter_1
c
c  test on rms_on_net mode
c
		if (rms_on_net) then
c  next step in coord., write output for previous point
c
			 call rms_net_2
c
c  test on end of rms_net_mode
c
			 if (endit) then
c
c  end of mode, show next menu
c
			     endit=.false.
			     go to 162
			 else
c
c  compute residuals
c
			     go to 30
			 endif
		else if (rms_on_sphere) then
c
c  load segment

			 call sphere_step (i0,rmsres,endit,t0_norm)
c
c  test on end of rms_sphere_mode
c
			 if (endit) then
c
c  load segment

			     c_hypo(1)=x_orig
			     c_hypo(2)=y_orig
			     c_hypo(3)=z_orig
c
c  evaluate travel times and derivatives for every recording station
c
			     call td_all (c_hypo)
c
c  end of mode, show next menu
c
			     go to 162
			 else
c
c  transform local to Krovak coordinates for surface computing
c
			     c_hypo1(1)=x0
			     c_hypo1(2)=y0
			     c_hypo1(3)=z0
c
			     call trans(c_hypo1(1),c_hypo1(2),c_hypo1(3),2)
c  z-coordinate of surface for epicenter coordinates c_hypo(1),c_hypo(2)
c  in computing of z-coordinate of surface ... z-axis is upward
c
			     call spline_value (0,c_hypo1,vd)
			     zsurf=-vd(1)
c
c  test on trial hypocenter in air
c
			     if (z0.lt.zsurf) then
						z0=zsurf
			     endif
c
c  compute next residual
c
			     go to 30
			 endif
		endif
c
c  test on end of iteration process
c
		if (endit) then
c
c  end of it. process ... skip writting for success. iteration and test
c   on decreasing in rms of res.
c
			 go to 35
		endif
c
		if (.not.scan_depth) then
			 xp=x0
			 yp=y0
			 zp=z0
c
c  transform local coord. to Krovak
c
		     call trans (xp,yp,zp,0)
c
c  set grafics symbol for successful and unsuccessful iteration
c
			 if (rmsresp.lt.rmsres) then
c
c  unsuccessful iteration
c
			     old_it='*'
			 else
c
c  successful iteration
c
			     old_it=' '
			 endif
c
c  not scanned depth mode ... detailed description of this iteration
c
			 write (*,
     >    '(i4,a1,3x,2(f8.3,a1,1x),f8.3,a1,1x,f8.3,a1,2x,f6.3,
     >    3x,f6.3)')
     >    i0,old_it,xp,symbol_x,yp,symbol_y,zp,symbol_depth,
     >    t0,symbol_otime,sqrt(rmsres),absd
		endif
c
		if (i0 .eq. 1) then
c
c  for the first iteration ... initialize rms of residuals
c
			 rmsresp = rmsres
			 best_x0=x0
			 best_y0=y0
			 best_z0=z0
			 best_t0=t0
			 n_recomp_it = 0
		else
c
c  not the first iteration
c
c
c  test on successful iteration
c
			 if (rmsresp.lt.rmsres) then
c
c  unsuccesful iteration
c
c  next iteration will not be a new iteration
c
			     i0 = i0 - 1
c
c  no. of recomputing of last computed adjustment of hypocenter
c
			     n_recomp_it = n_recomp_it + 1
c
			     if (n_recomp_it .eq. 1) then
c
c  for the first recomputing: store original coord. of hypocenter before
c   this unsuccesful iteration
c
						bx0=x0-d(1)
						by0=y0-d(2)
						bz0=z0-d(3)
			     endif
c
c  set new adjustment vector ... adjustment vector of unsuccessful iteration
c   divided by given parameter p_decrease
c
			     d(1) = d(1) / p_decrease
			     d(2) = d(2) / p_decrease
			     d(3) = d(3) / p_decrease
c
c  set new coord. of trial hypocenter
c
			     x0 = bx0 + d(1)
			     y0 = by0 + d(2)
			     z0 = bz0 + d(3)
c
c  test on fixed origin time
c
			     if (.not.fix_otime) then
c
c  origin time not fixed ... value for new value of origin time will be set
c   by the condition of minimal rms of residuals
c
						t0_norm=.true.
			     endif
c
c  set absolute value for last space shift of hypocenter
c
			     absd=sqrt(d(1)**2+d(2)**2+d(3)**2)
c
c  test on the end of iterative proccess
c
			     if (abs(d(1)).lt.min_shift_epi .and.
     >            abs(d(2)).lt.min_shift_epi .and.
     >            abs(d(3)).lt.min_shift_depth)  then
c
c  end of iterative procces
c  set the coord. of hypocenter to the best coord. of hypocenter found
c  in computation
c
						x0=best_x0
						y0=best_y0
						z0=best_z0
						t0=best_t0
c
c  set flag for error analysis
c
						endit=.true.
			     endif
			     go to 30
			 else
c
c  successful iteration
c
c
c  store coord. of this succesful trial hypocenter
c  set new value of rms of residuals
c
			     rmsresp = rmsres
			     best_x0=x0
			     best_y0=y0
			     best_z0=z0
			     best_t0=t0
c
c  reset variable for no. of recomputing of unsucc. iteration
c
			     n_recomp_it = 0
			 endif
		endif
c
35    continue
c
c  compute the right side, matrix C, eigenvalues, damping, scaling,
c  inversion matrix
c
		call iter_2
c
c  test on regularity of matrix C
c
		if (det.eq.0.) then
c
c  matrix is singular
c
			 write (*,'(1x,a,": Hessian matrix C is singular.")')
     >    prog_name
			 go to 160
		endif
c
c  test on computation for error analysis
c
		if (endit) then
c
c  estimated error of model ... model_error
c
			 if (rmsres_co.ne.9.99**2) then
c
c  test on damping in the last iteration
c
			     if (sigma(1).gt.0) then
						rmsres_co=9.99**2
			     else
c
c  degree of freedom is greater then 0
c  set estimate of error for error analysis
c
						rmsres_co=rmsres_co+model_error**2
			     endif
			 endif
c
c  test on evaluatimg of errors
c
			 if (rmsres_co.ne.9.99**2) then
c
c  compute covariance matrix  co: (A+&)INV * A * (A+&)INV
c   & ... damping
c
			     call cov_matrix
			 endif
c
			 go to 155
		else
c
c  compute adjustment vector
c
			 do i=1,4
			     sum8=0.0
			     do j=1,4
						sum8=sum8+c(i,j)*b(j)
			     end do
c
c  for scaled matrix C
c
			     d(i)=real(sum8/scale(i),4)
			 end do
		endif
c
		x0=x0+d(1)
		y0=y0+d(2)
c
c  test on too big change in depth
c
		if (abs(d(3)).gt.max_change_z) then
			 d(3)=sign(max_change_z,d(3))
c
			 if (.not.fix_otime) then
			     t0_norm=.true.
			 endif
c
		endif
c
c  transform local to Krovak coordinates for surface computing
c
		c_hypo1(1)=c_hypo(1)
		c_hypo1(2)=c_hypo(2)
		c_hypo1(3)=c_hypo(3)
c
		call trans(c_hypo1(1),c_hypo1(2),c_hypo1(3),2)
c
c  z-coordinate of surface for epicenter coordinates c_hypo(1),c_hypo(2)
c  in computing of z-coordinate of surface ... z-axis is upward
c
		call spline_value (0,c_hypo1,vd)
		zsurf=-vd(1)
c
c  test on hypocenter in air
c
		if (z0+d(3).lt.zsurf) then
			 d(3)=zsurf-z0
			 z0=zsurf
c
			 if (.not.fix_otime) then
			     t0_norm=.true.
			 endif
c
		else
			 z0=z0+d(3)
		endif
c
c  fixed surface
c
		if (fix_surface) then
			 d(3)=0.0
			 z0=zsurf
c
			 if (.not.fix_otime) then
			     t0_norm=.true.
			 endif
c
		endif
c
		t0=t0+d(4)
c
c  absolute value of adjusment vector
c
		absd=sqrt(d(1)**2+d(2)**2+d(3)**2)
c
c  test on end of iterative process
c
		if (abs(d(1)).lt.min_shift_epi .and.
     >    abs(d(2)).lt.min_shift_epi .and.
     >    abs(d(3)).lt.min_shift_depth)  then
c
c  end of iterative process
c
			 endit=.true.
			 i0=i0-1
			 go to 30
		endif
c
c  test on max. number of iterations
c
		if (i0 .lt. maxIter) then
c
c  max. number of iterations is maxIter
c
			 go to 30
		else
			 if (scan_depth) then
c
c  set values of coord. of epicenter
c
			     x0=999.99
			     y0=999.99
			     rmsres=9.99**2
			 else
c
c  write message for the case of end of the location
c
			     write (lulist,'(/,9x,"No convergent criterion satisfied "
     >        ,"in ",i2," iterations!!!",/,9x,
     >"---------------------------------------------------",/)')
     >maxIter
			 endif
		endif
155   continue
c
c  test on locmode
c
		if (.not.scan_depth) then
c  normalize orig. time
			 call origin_time (n_increase,dmin8)
c
c  magnitude estimation
c
			 call magni
c  output data
c
			 if (i0.lt.maxIter) then
			     call o_hy3(lulist)
			 else
c
c  no list to terminal in the case of no convergence
c
                             write(0,*) 'NO CONVERGENCE!'
			     call o_hy3(lulist)
			 endif
		endif
c
160   continue
		if (scan_depth) then
c
			 xp=x0
			 yp=y0
			 zp=z0
c
c  local to Krovak
c
			     call trans (xp,yp,zp,0)
c
c  scan depth mode ... write results
c
	       write (*,'(3x,i2,3x,f8.3,2x,3(1x,f7.3,2x),3x,f7.3)')
     >    i0,xp,yp,zp,t0,sqrt(rmsres)
c
cc	call pause
c
	       write (lulist,'(3x,i2,3x,f8.3,2x,3(1x,f7.3,2x),3x,f7.3)')
     >    i0,xp,yp,zp,t0,sqrt(rmsres)
			 z0=z0+scan_step
			 if (z0.le.scan_end) then
			     x0=x_start_trial
			     y0=y_start_trial
c
c  new cycle for new fixed depth
c
			     go to 29
			 endif
			 endit=.false.
		endif
c
162   continue
c
c  show menu with rms of res. on sphere centered on the hypocenter
c
		call dialog_2 (endit,scan_depth,rms_on_net,rms_on_sphere)
c
		if (rms_on_sphere) then
c
c  go on rms_on_sphere computing
c
			 go to 29
		endif
c
163   continue
c
c  show menu
c
			 call dialog_2_1
     >    (endit,prt,scan_depth,i0,maxIter,rms_on_net,
     >    loc_write,rp,i_menu)

c	
		if (i_menu.eq.1) then
c
c  new location
c
			 go to 90
		else if (i_menu.eq.2) then
			write(*,*) 'Writing dbfile' 
			call create_dbfile(hyp3name)
c
c  next menu
c
			 go to 163
		else if (i_menu.eq.3) then
c  next menu
c
			 go to 163
		else if (i_menu.eq.4) then

		endif
c ====================================================================
c      write(14,*) n_poi
c      write(14,*) 0,c_hypo(3),z0,best_z0
c      write(14,'(F7.3,F6.2)') (poi(j),z_coor(j),j=1,n_poi)
c ====================================================================
c
c  end of main program
c
		end
