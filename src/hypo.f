
      program HYPO
c
c***************************************************************************
c
c             program   H Y P O 3 D
c
c     - locates hypocenter in 3-D model
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
c       2) file with surface definition
c       3) hypfile containing seismic phase arrival data
c
c  ************
c  declarations
c  ************
c
         implicit none

c  local parameters
c
         integer maxIter
         parameter (maxIter = 50)            !max. number of iterations

         real    max_change_z
         parameter (max_change_z = 5.)       !max. change of z-coord. in 1 it

         real    p_decrease                  !parameter of decreasing of step in
         parameter (p_decrease = 2.)         !unsuccessful iteration

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
         character*1     old_it              !symbol for iteration with rms
                                             !of res greater then previous succ.
                                             !iteration
         real    c_hypo(3)                   !coord. of hypocenter
         real    c_hypo1(3)                  !epicenter local coordinates
         real    bx0, by0, bz0 !, bt0        !values of succesful iteration
         real    rmsresp                     !previous rms of residuals
         real*8  sum8                        !summa in real*8
         real    xp,yp,zp                    !auxiliary variables
         real    d(4)                        !shift vector
         real    zsurf                       !value of fix_surface
         real    vd(10)                      !auxiliary field
         integer n_increase                  !# of increasing in origin time norm
         integer i_menu                      !selected way in menu
         integer ios                         !error variable
         integer n_recomp_it                 !number of recomputing of iter.
         integer i,j                         !aux. var.

         integer string_length
         character*255 string, hypname, hy3name
         character*255 ch_model_name         !name of crustal model

c
c  global variables ... common blocks
c

         logical      ee3          !flag controlling error estimation 
         real         model_error
         real         reading_error
         common /err/ model_error,reading_error,ee3
c
c  common for space and rms data of hypocenter
c
         real           x_orig,y_orig,z_orig
         real           rms_orig
         integer        no_iter_orig
         common /org/   x_orig,y_orig,z_orig,rms_orig,no_iter_orig
c
c  common for time data
c
         real    t0
         common /otime/  t0
c
c  common for coord. of trial hypocenter
c
         real                x0,y0,z0        !coord. of trial hypocenter
         common /centr/      x0,y0,z0
c
c  common for data for covariance matrix evaluation
c
         real            rmsres
         real            rmsres_co
         common /cov/    rmsres,rmsres_co
c
c  common for data of recording situ (space coordinates, delay; no. of arr.)
c
         integer             nrec            !no. of arrivals
         common /rec/        nrec
c
c  common for evaluated beam data (travel time and its derivatives)
c
         real                tcal(nrec_max)  !calc. travel times
         real                xc(4,nrec_max)  !travel time derivatives
         common /cal_time/   tcal,xc
c
c  common of hypfile items ... noncharacter part, average weight
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
         integer         no_valid_arrivals   !no. of valid arrivals
         logical         t0_norm             !norm of origin time?
         logical         endit               !end of iteration process?
         common /it1/    t0_norm,endit,no_valid_arrivals
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
         common /rmsnet/ rms_on_net
c
         integer             i0
         common /citer/      i0
c
c  common for flags of fixed depth (fixed surface, fixed depth), no. of it.
c
         logical             fix_surface
         common /srfc/       fix_surface
c
c  common for flags of fixed coord.
c
         logical         fix_depth
         logical         fix_x
         logical         fix_y
         logical         fix_otime
         common /fix_mode/ fix_depth,fix_x,fix_y,fix_otime
c
c  common for symbols for output
c
         character*1     symbol_x
         character*1     symbol_y
         character*1     symbol_depth
         character*1     symbol_otime
         common /symb/   symbol_x,symbol_y,symbol_depth,symbol_otime
c
c      common for file names - VD
c
         character*255 hypfn
         character*255 modfn
         common /hymofn/  hypfn,modfn

c ----------------------------------------------------------------------
c 2024-04-22 pz
c flag controlling ray tracing, set by the --split_ray keyword
         logical split_rays
c pointer for swapping between two velocity models
         real v3
         pointer(ip_v3,v3(x_layer,y_layer,z_layer))
         common /model_stat/ ip_v3, split_rays

c   In the case of the hypo3d location program, the question is whether
c   to maintain independent ray tracing in each of the two velocity
c   models for P waves and S waves. Since we're using only an approximate
c   solution to the direct problem, and the perturbation procedure isn't
c   applied perfectly (ray tracing is in a 1-D model while time
c   integration is in a 3-D model), it suggests that it's unnecessary
c   to trace the ray separately in each model. Instead, we can use
c   a procedure where, in the first phase of the calculation,
c   the ray is traced only in the velocity model for P-waves and,
c   in the second phase of the calculation, this ray is used to calculate
c   the propagation time in both velocity models
c   for P-waves and for S-waves.
c
c   Choose how the ray is traced
c   by entering the command line keyword "--split_ray"
c   1. "--split_ray" not entered (default): set flag split_rays = .false.
c      The ray is traced only in the velocity model for P-waves.
c   2. "--split_ray" entered: set flag split_rays = .true.
c      The ray is traced in both velocity models independetly.
c ----------------------------------------------------------------------

c
c  get and decode command line arguments -VD
c
c  initialization of model_error variable (and other variables -VD)
c
         model_error=-1.0
         ch_model_name=' '
         hypname=' '
         hy3name=' '

         split_rays=.false.
         ee3=.false.
c
c  for the first: get runstring
c
         j=iargc()

         if ( j .lt. 3 .or. j .gt. 7 ) then
            call runstrinf
         endif 
c
c  for the second: decode parameters
c
         i = 1
         do while ( i .le. j)
            call getarg(i,string)
            i = i + 1
            string_length=lnblnk(string)
            if (string(1:2).eq.'-?') then
c
               call runstrinf
c
            else if (string(1:12).eq.'--split_rays') then
c
               split_rays=.true.
c
            else if (string(1:5).eq.'--ee3') then
c
               ee3=.true.
c
            else if (string(1:2).eq.'-I'.or.string(1:2).eq.'-i') then
c  name of input hypfile
c
               if (string_length .eq. 2) then
                 call getarg(i,string) 
                 if (string(1:1) .ne. '-') then
                    hypname=string(1:lnblnk(string))
                    i = i + 1
                 endif
               else
                 hypname=string(3:lnblnk(string))
               endif

            else if (string(1:2).eq.'-O'.or.string(1:2).eq.'-o') then
c  name of output hy3file
c
               if (string_length .eq. 2) then
                 call getarg(i,string)
                 if (string(1:1) .ne. '-') then
                    hy3name=string(1:lnblnk(string))
                    i = i + 1
                 endif
               else
                 hy3name=string(3:lnblnk(string))
               endif

c
            else if (string(1:2).eq.'-M'.or.string(1:2).eq.'-m') then
c  name of model file
c
               if (string_length .eq. 2) then
                 call getarg(i,string)
                 if (string(1:1) .ne. '-') then
                    ch_model_name=string(1:lnblnk(string))
                    i = i + 1
                 endif
               else
                 ch_model_name=string(3:lnblnk(string))
               endif

            else
c
               write(*,'(1x,a,": Error - wrong parameter ",a)')
     >           prog_name,string
               call runstrinf
            endif
         end do                                ! decoding of parameters
c
         if(hypname.eq.' ') then
            write(*,*) 'Error: Hypfile name not given'
            call runstrinf
         endif
         if(hy3name.eq.' ') then
            write(*,*) 'Error: Hy3file name not given'
            call runstrinf
         endif
         if (ch_model_name.eq.' ') then
            write(*,*) 'Error: Modelfile name not given'
            call runstrinf
         endif
         hypfn=hypname
         modfn=ch_model_name
c
c
         write(*,*) 'Hypfile is  ',trim(hypfn)
         write(*,*) 'Model is  ',trim(modfn)
         rms_on_sphere = .false.
         loc_write     = .false.
         reading_error = 0.016             !estimated reading error in ms
                                           !(two sample intervals)
c  header of the program
c
         write(*,'(1x,a)') long_prog_name
         write(*,'(1x,a,//)') prog_name2
c
c  input of hypfile, crustal model, initialize of spline surface common
c
         call i_hyp_mod

c         if (nrec .lt. 3) then
c            write (*,
c     >      '(1x,a,": Error - no. of arrivals in hypfile ",
c     >    " < 3.")') prog_name
         if (nrec .lt. 1) then
            write (*,'(1x,a,
     >         ": Error - there is no arrival in the hyp file")')
     >         prog_name
            call exit
         endif
c
         rp=.false.
90       continue
c
c  test on repeat of location
c
         if (rp) then
c
c  check datum, compute no_valid_arrival
c
            call rec_time_name
     >      (.false.,rp,no_valid_arrivals,dmin8,n_increase,2)
         else
c
c  check names of recording stations, datum
c
            call rec_time_name
     >      (.false.,rp,no_valid_arrivals,dmin8,n_increase,0)
c
c  test on number of arrivals
c
            if (nrec.lt.1) then
c
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
     >   (.false.,rp,no_valid_arrivals,dmin8,n_increase,1)
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
         if (no_valid_arrivals .lt. 3) then
            if (fix_x .and. fix_y .and.
     >          (fix_depth .or. fix_surface)) then
c              solve a forward problem
               continue
            else if (rms_on_net) then
               continue
            else
               write (*,'(1x,a,": # of arrivals in hypfile  <  3 "/
     >    " Try a different hypfile or",
     >    " enter the XYZ coordinates of the hypocenter.")') prog_name
               call exit
            endif
         endif
c
         if (no_valid_arrivals .eq. 3 .and.
     >       .not. (fix_depth .or. fix_surface .or. scan_depth
     >              .or. fix_x  .or. fix_y .or. fix_otime) ) then
c        in case of a poorly conditioned task, give a chance
c        to a location with a fixed depth
            fix_depth=.true.
c
115         continue
            write(*,'(1x,a,": Only 3 valid arrivals.",/,
     >                "         Enter value of fixed depth:_")')
     >      prog_name
c
c  reset value of z-coor. of start point
c
            read(*,*,err=115,end=120,iostat=ios) z_start
            go to 125
c
120         continue
c
c  end of program
c
            call abort
c
125         continue
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
         call spline_value(0,c_hypo1,vd)
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
 2030       format(//'  SUCCESSIVE APPROXIMATIONS'//
     >      '        |   LOCATION OF HYPOCENTER   | ORIGIN |   RMS   | SHIFT '
     >      ,'OF'/
     >      ' APROX. | X-COOR.   Y-COOR.   Z-COOR.|  TIME  | OF RES. | HYPO',
     >      'CENTER'/)
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
29       continue
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
            call sphere_step(i0,rmsres,endit,t0_norm)
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
            call spline_value(0,c_hypo1,vd)
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
30       continue
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
         call td_all(c_hypo)
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

            call sphere_step(i0,rmsres,endit,t0_norm)
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
               call td_all(c_hypo)
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
               call spline_value(0,c_hypo1,vd)
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
            call trans(xp,yp,zp,0)
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
            write(*,'(i4,a1,3x,2(f8.3,a1,1x),f8.3,a1,
     >            1x,f8.3,a1,2x,f6.3,3x,f6.3)')
     >      i0,old_it,xp,symbol_x,yp,symbol_y,zp,symbol_depth,
     >      t0,symbol_otime,sqrt(rmsres),absd
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
     >         abs(d(2)).lt.min_shift_epi .and.
     >         abs(d(3)).lt.min_shift_depth)  then
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
35       continue
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
            write(*,'(1x,a,": Hessian matrix C is singular.")')
     >      prog_name
            go to 160
         endif
c
c  test on computation for error analysis
c
         if (endit) then
c
c  estimated error of model ... model_error
c
                  rmsres_co=rmsres_co+model_error**2
c
c  compute covariance matrix  co: (A+&)INV * A * (A+&)INV
c   & ... damping
c
               call cov_matrix
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
         call spline_value(0,c_hypo1,vd)
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
     >   abs(d(2)).lt.min_shift_epi .and.
     >   abs(d(3)).lt.min_shift_depth)  then
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
            else
c
c  write message for the case of end of the location
c
               write(lulist,'(/,9x,"No convergent criterion satisfied "
     >        ,"in ",i2," iterations!!!",/,9x,
     >"---------------------------------------------------",/)')
     >         maxIter
            endif
         endif
155      continue
c
c  test on locmode
c
         if (.not.scan_depth) then
c  normalize orig. time
            call origin_time(n_increase,dmin8)
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
160      continue
         if (scan_depth) then
c
            xp=x0
            yp=y0
            zp=z0
c
c  local to Krovak
c
            call trans(xp,yp,zp,0)
c
c  scan depth mode ... write results
c
            write(*,'(3x,i2,3x,f8.3,2x,3(1x,f7.3,2x),3x,f7.3)')
     >      i0,xp,yp,zp,t0,sqrt(rmsres)
c
c
            write(lulist,'(3x,i2,3x,f8.3,2x,3(1x,f7.3,2x),3x,f7.3)')
     >      i0,xp,yp,zp,t0,sqrt(rmsres)
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
162      continue
c
c  show menu with rms of res. on sphere centered on the hypocenter
c
         call dialog_2(endit,scan_depth,rms_on_net,rms_on_sphere)
c
         if (rms_on_sphere) then
c
c  go on rms_on_sphere computing
c
            go to 29
         endif
c
163      continue
c
c  show menu
c
         call dialog_2_1(endit,
     >         prt,scan_depth,i0,maxIter,rms_on_net,loc_write,rp,i_menu)

c
         if (i_menu.eq.1) then
c
c  new location
c
            go to 90
         else if (i_menu.eq.2) then
            write(*,*) 'Writing dbfile'
            call create_dbfile(hy3name)
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
c
c  end of main program
c
      end program HYPO

      subroutine runstrinf
      write(*,*)
     >      'Usage: hypo3d [--split_ray] -i<input> -o<output> -m<model>'
      write(*,*) 'Example: hypo3d -ia001.hyp -oa001.hy3 -mkra_3d_a.mod'
      write(*,*) 'Keyword "--split_ray" sets, that the rays are traced'
      write(*,*) 'in both velocity models for v_p and v_s independetly.'
      write(*,*) 'The keyword "--ee3" sets the error estimation mode'
      write(*,*) 'in which the entire error ellipsoid is calculated'
c      write(*,*) 'even if some hypocenter coordinate is fixed.'
      write(*,*) 'even if hypocenter depth coordinate is fixed.'
      call exit
      end subroutine runstrinf
