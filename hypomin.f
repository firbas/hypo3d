	program HYPO
		implicit none

		integer maxIter
		real    max_change_z
		parameter (maxIter = 50)            !max. number of iterations
		parameter (max_change_z = 5.)       !max. change of z-coord. in 1 it
		include 'min_shift.fi'
		include 'error.fi'
		include 'param.fi'
		include 'pname.fi'
		include 'list.fi'
		include 'model_3d.fi'
		include 'onset.fi'
		include 'term.fi'
		logical correct                     !int. copy of hypofile corrected?
		logical loc_write                   !locfile was written in menu block?
		logical rms_on_sphere               !compute rms on sphere centered
														!on hypocenter
		logical prt                         !printed?
		logical rp                          !repair mode?
		real*8  dmin8                       !reference time in double real
		real    absd                        !last shift of hypocenter
		real    best_x0,best_y0,best_z0,best_t0
														!auxiliary var.
		real    c_hypo1(3)                  !epicenter local coordinates
		real    bx0, by0, bz0 !, bt0          !values of succesful iteration
		real    rmsresp                     !previous rms of residuals
		real    sum4                        !summa in real*4
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
		character*1 answer                  !      -"-      for input
		character*64 cwd		!VD -current working directory	
		character*4 site
		integer             nx,ny,nxs,nys,nws
		real                x,y,w,vx,vy,sigma
		common /sur/        nx(7),ny(7),nxs(7),nys(7),nws(7),x(48),
     >                    y(48),w(1024),vx(5,48),vy(5,48),sigma(7)
		real           x_orig,y_orig,z_orig
		real           rms_orig
		common /org/   x_orig,y_orig,z_orig,rms_orig
		real*8         sigma1(4)
		common /sigm/  sigma1
		real           s_point(10)
		common /point/ s_point
		real            avm             !average magnitude
		real            sdm             !std. dev. of magnitude
                real            xmag(nrec_max)
                common /mag/    xmag,avm,sdm
		character*64        ch_model_name   !name of crustal model for use
		common /chmodn/     ch_model_name
		character*1     interactive         !flag for interactive mode
		character*1     chfix_depth         !flag for fixed depth
		character*16    chfix_value         !value of fixed depth
		character*6     sname               !name of file with start coord.
		common /hnamch/ interactive,chfix_depth,chfix_value,sname
		integer             subdir_length   !length of subdir. name
		logical             source_flag
		common /hnami/      subdir_length,source_flag
		integer year
		integer month
		integer day
		integer hour
		integer minute
		real    t0
		common /otime/      year,month,day,hour,minute,t0
		integer             year_orig
		integer             month_orig
		integer             day_orig
		integer             hour_orig
		integer             minute_orig
		real                t_orig
		common /origin/     year_orig,month_orig,day_orig,hour_orig,
     >                    minute_orig,t_orig
		real                amp(nrec_max)   !array of amplitudes
		real                freq(nrec_max)  !array of frequencies
		common /ampli/      amp,freq
		real                x0,y0,z0        !coord. of trial hypocenter
		common /centr/      x0,y0,z0
		real            co(4,4)
		real            id(4,4)
		real            re(4,4)
		real            rmsres
		real            rmsres_co
		common /cov/    co,rmsres,rmsres_co,id,re
		integer             nrec            !no. of arrivals
		real                xstat(nStation) !\
		real                ystat(nStation) ! >coordinates of stations
		real                zstat(nStation) !/
		real                dly(nStation)   !stations delays for surf. events
		common /rec/        nrec,xstat,ystat,zstat,dly
		character*1         type(nrec_max)  !type of arrival
		common /chrec/      type
		real                tcal(nrec_max)  !calc. travel times
		real                xc(4,nrec_max)  !travel time derivatives
		common /cal_time/   tcal,xc
		real                toa(nrec_max)   !take-off angles
		common /toa/        toa
		logical             fix_depth       !fixed depth?
		logical             fix_surface     !fixed surface?
		integer             i0              !no. of iter. cycle
		common /srfc/       fix_surface,fix_depth,i0
		real                trec(nrec_max)  !observed times
		real                wt(nrec_max)    !weight
		real                avwt            !average weight
		common /hyp/        trec,wt,avwt

		real                wt1 (nrec_max)
		common /wt_1/       wt1
		character*4         rec_name(nrec_max) !name of recording site
		common /chhyp/      rec_name

      include 'stmod.fi'

		real*8              datum8(nrec_max)!whole date of arrival
		integer             ichan(nrec_max) !no. of channel
		common /dat8/       datum8,ichan

		integer             nstat           !no. of stations
		character*4         stat_name(nStation) !name of station
		common /stnam/      nstat,stat_name
		real            c_hypo(3)           !coord. of hypocenter
		integer         no_valid_arrivals   !no. of valid arrivals
		logical         t0_norm             !norm of origin time?
		logical         endit               !end of iteration process?
		common /it1/    t0_norm,c_hypo,no_valid_arrivals,endit
		character*1     old_it              !symbol for iteration with rms
														!of res greater then previous succ.
														!iteration
		common /ch_it1/ old_it

		real*8          c(4,4)              !Hessian matrix resp. inv. Hess. m.
		real*8          b(4)                !vector of right side
		real*8          det                 !determinant of matrix c
		real            scale(4)            !scale vector for Hessian matrix
		common /it2/    c,b,det,scale
		logical         scan_depth
		real            scan_start
		real            scan_end
		real            scan_step
		common /scan/   scan_depth,scan_start,scan_end,scan_step

		real            x_start
		real            y_start
		real            z_start
		real            ot_start
		common /start/  x_start,y_start,z_start,ot_start

		real              x_start_trial
		real              y_start_trial
		common /t_start/  x_start_trial,y_start_trial
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
		logical         fix_x
		logical         fix_y
		logical         fix_otime
		common /f_mode/ fix_x,fix_y,fix_otime

		character*1     symbol_x
		character*1     symbol_y
		character*1     symbol_depth
		character*1     symbol_otime
		common /symb/   symbol_x,symbol_y,symbol_depth,symbol_otime

		logical        err_stname
		logical        w_changes
		common /ernam/ err_stname,w_changes
		real            tid(z_layer,z_layer)
		real            did(z_layer,z_layer)
		common /trace/  tid,did
		logical       loc                   !was written locfile?
		common /wloc/ loc

		real            nangle
		common /nangl/  nangle

		character*64 hypofn
		character*64 modfn
		common /hymofn/	hypofn,modfn

		integer iargc, lnblnk, getcwd, status,string_length
		integer index
		character*32 subdir,string,hyponame
        model_error=-1.0
	status=getcwd(cwd)
	subdir=' '
	ch_model_name=' '
	hyponame=' '
      	j=iargc()

      do i=1,j
          call getarg(i,string)
 	string_length=lnblnk(string)         
		if (string(1:2).eq.'-?'.or.string(1:2).eq.'-X'.or.
     *		string(1:2).eq.'-x') then

      		write(*,*) 'Online information is not available'	
          	else if (string(1:2).eq.'-D'.or.string(1:2).eq.'-d') then
				
			subdir(1:string_length-2)=string(3:string_length)
			site=subdir(1:4)                  

          	else if (string(1:2).eq.'-S'.or. string(1:2).eq.'-s') then

              	subdir(1:string_length-2)=string(3:string_length)
 		  	site=subdir(1:4)             

          	else if (string(1:2).eq.'-F'.or.string(1:2).eq.'-f') then

			hyponame=string(3:lnblnk(string))//'.hyp'              

        
	 	else if (string(1:2).eq.'-M'.or.string(1:2).eq.'-m') then

                  ch_model_name(1:string_length-2)=
     >                          string(3:string_length)
          else
              write (*,
     >        '(1x,a,": Error - wrong parameter ",a)')
     >         prog_name,string(1:2)
          endif
      end do  					! decoding of parameters

	if(hyponame.eq.' ') then
		write(*,*) 'Error: Hypofile name not given'
		write(*,*) 'Usage: hypo3d -fa001'
		call exit
	endif
	ios=index(subdir,'/')
	if(subdir.eq.' '.and.ch_model_name.eq.' ') then
		subdir=cwd(12:lnblnk(cwd))
		site=cwd(7:10)
		write(*,*) 'Data subdirectory not given. Using default values:'
		write(*,*) 'Site:    ',site
		write(*,*) 'Subdir:  ',subdir
		hypofn='/data/'//site//'/'//subdir(1:lnblnk(subdir))//'/'
     *			//hyponame(1:lnblnk(hyponame))
		ch_model_name=site//'_3d_a.mod'
		modfn='/data/kras/'//site//'_3d_a.mod'	
	elseif(subdir.eq.' '.and.ch_model_name.ne.' ') then
		subdir=cwd(12:lnblnk(cwd))
		site=cwd(7:10)
		write(*,*) 'Data subdirectory not given. Using default values:'
		write(*,*) 'Site:    ',site
		write(*,*) 'Subdir:  ',subdir
		hypofn='/data/'//site//'/'//subdir(1:lnblnk(subdir))//'/'
     *			//hyponame(1:lnblnk(hyponame))
		modfn='/data/kras/'//ch_model_name	
	elseif (ios.eq.0.and.ch_model_name.eq.' ') then	
		site=subdir(1:4)
		hypofn='/data/'//site//'/'//subdir(1:lnblnk(subdir))//'/'
     *			//hyponame(1:lnblnk(hyponame))
		ch_model_name=site//'_3d_a.mod'	
		modfn='/data/kras/'//ch_model_name		
	elseif (ios.eq.0.and.ch_model_name.ne.' ') then	
		site=subdir(1:4)
		hypofn='/data/'//site//'/'//subdir(1:lnblnk(subdir))//'/'
     *			//hyponame(1:lnblnk(hyponame))	
		modfn='/data/kras/'//ch_model_name		
	elseif (ios.ne.0.and.ch_model_name.eq.' ') then
		write(*,*) 'Parameter -M required in this situation'
		call exit
	elseif (ios.ne.0.and.ch_model_name.ne.' ') then
		hypofn=subdir(1:lnblnk(subdir))//'/'
     *			//hyponame(1:lnblnk(hyponame))
		modfn=ch_model_name	
	else
		write(*,*) 'Unexpected situation with parameters'
		call exit
	endif
	write(*,*) 'Hypofile is  ',hypofn
	write(*,*) 'Model is  ',modfn
	rms_on_sphere = .false.
	loc_write     = .false.
	reading_error = 0.016 		!estimated reading error in ms
						!(two sample intervals)
		write (*,'(1x,a)') long_prog_name
		write (*,'(1x,a,//)') prog_name2

		loc=.false.

		n_of_location=0
10    continue

		n_of_location=n_of_location+1
		call inpt(n_of_location)

		if (nrec .lt. 3) then
			 write (*,
     >    '(1x,a,": Error - no. of arrivals in hypofile ",
     >    " < 3.")') prog_name
		endif
		rp=.false.
90    continue

		correct=.false.

		if (rp) then
			 call rec_time_name
     >    (correct,rp,no_valid_arrivals,dmin8,n_increase,2)
		else

			 call rec_time_name
     >    (correct,rp,no_valid_arrivals,dmin8,n_increase,0)

			 if (nrec.lt.3) then

			     go to 10
			 endif
		endif

		call dialog_3 (correct)

		if (correct) then
			 call rec_time_name
     >    (correct,rp,no_valid_arrivals,dmin8,n_increase,2)
		endif
		call rec_time_name
     >(correct,rp,no_valid_arrivals,dmin8,n_increase,1)
		if (scan_depth) then

			 call dialog_1 (dmin8)
		endif
		if (no_valid_arrivals  .lt. 3) then
			 write (*,'(1x,a,": # of arrivals in hypofile  <  3 "/
     >    "         Try another hypofile.")') prog_name
			 if (rp) then
			     scan_depth=.false.
			     endit=.false.
			     go to 162
			 else
			     call Abort
			 endif
		else if (no_valid_arrivals.eq.3 .and. .not.fix_depth
     >       .and. .not.fix_surface  .and. .not.scan_depth
     >       .and. .not.fix_x  .and. .not.fix_y
     >       .and. .not.fix_otime) then
			 fix_depth=.true.
115       continue
			 write (*,'(1x,a,": Only 3 valid arrivals.",/,
     >                "         Enter value of fixed depth:_")')
     >    prog_name
			 read (*,*,err=115,end=120,iostat=ios) z_start
			 go to 125

120       continue

			 call Abort

125       continue

			 z0=z_start
			 symbol_depth='*'
			 rms_on_net=.false.
		endif
		if (rms_on_net) then
			 call rms_net_1(dmin8)
		endif
		c_hypo1(1)=x0
		c_hypo1(2)=y0
		c_hypo1(3)=z0
		call trans(c_hypo1(1),c_hypo1(2),c_hypo1(3),2)
		call spline_value (0,c_hypo1,vd,0)
		zsurf=-vd(1)
		if (z0.lt.zsurf) then
			 z0 = zsurf
		endif
		if (fix_surface) then
			 z0=zsurf
		endif
		if (.not.scan_depth .and. .not.rms_on_net) then
			 write(*,2030)
 2030     format(//'  SUCCESSIVE APPROXIMATIONS'//
     >'        |   LOCATION OF HYPOCENTER   | ORIGIN |   RMS   | SHIFT '
     >    ,'OF'/
     >' APROX. | X-COOR.   Y-COOR.   Z-COOR.|  TIME  | OF RES. | HYPO',
     >    'CENTER'/)
		endif
		avwt=0.0
		do  i=1,nrec
			 xc(4,i)=1.0
			 avwt=avwt+wt(i)
		end do
		avwt=avwt/no_valid_arrivals
		if (scan_depth) then
			 z0=scan_start
		endif
29    continue

		rmsresp= 1.0e+06
		rp = .false.
		i0 = 0
		absd = 0.
		endit=.false.
		prt=.false.

		if (rms_on_sphere) then
			 call sphere_step (i0,rmsres,endit,t0_norm)

			 c_hypo1(1)=x0
			 c_hypo1(2)=y0
			 c_hypo1(3)=z0
			 call trans(c_hypo1(1),c_hypo1(2),c_hypo1(3),2)

			 call spline_value (0,c_hypo1,vd,0)
			 zsurf=-vd(1)

			 if (z0.lt.zsurf) then
			     z0=zsurf
			 endif
		endif
30    continue
		I0=I0+1
		c_hypo(1)=x0
		c_hypo(2)=y0
		c_hypo(3)=z0

		call td_all (c_hypo)

		call iter_1

		if (rms_on_net) then

			 call rms_net_2
			 if (endit) then
			     endit=.false.
			     go to 162
			 else
			     go to 30
			 endif
		else if (rms_on_sphere) then
			 call sphere_step (i0,rmsres,endit,t0_norm)
			 if (endit) then
			     c_hypo(1)=x_orig
			     c_hypo(2)=y_orig
			     c_hypo(3)=z_orig
			     call td_all (c_hypo)
			     go to 162
			 else
			     c_hypo1(1)=x0
			     c_hypo1(2)=y0
			     c_hypo1(3)=z0
			     call trans(c_hypo1(1),c_hypo1(2),c_hypo1(3),2)
			     call spline_value (0,c_hypo1,vd,0)
			     zsurf=-vd(1)
			     if (z0.lt.zsurf) then
						z0=zsurf
			     endif
			     go to 30
			 endif
		endif
		if (endit) then
			 go to 35
		endif
		if (.not.scan_depth) then
			 xp=x0
			 yp=y0
			 zp=z0
			 if (prog_name.eq.'HYPO3D') then
			     call trans (xp,yp,zp,0)
			 endif
			 if (rmsresp.lt.rmsres) then
			     old_it='*'
			 else
			     old_it=' '
			 endif

			 write (*,
     >    '(i4,a1,3x,2(f8.3,a1,1x),f8.3,a1,1x,f8.3,a1,2x,f6.3,
     >    3x,f6.3)')
     >    i0,old_it,xp,symbol_x,yp,symbol_y,zp,symbol_depth,
     >    t0,symbol_otime,sqrt(rmsres),absd
		endif
		if (i0 .eq. 1) then
			 rmsresp = rmsres
			 best_x0=x0
			 best_y0=y0
			 best_z0=z0
			 best_t0=t0
			 n_recomp_it = 0
		else
			 if (rmsresp.lt.rmsres) then
			     i0 = i0 - 1
			     n_recomp_it = n_recomp_it + 1
			     if (n_recomp_it .eq. 1) then
						bx0=x0-d(1)
						by0=y0-d(2)
						bz0=z0-d(3)
			     endif
			     d(1) = d(1) / p_decrease
			     d(2) = d(2) / p_decrease
			     d(3) = d(3) / p_decrease
			     x0 = bx0 + d(1)
			     y0 = by0 + d(2)
			     z0 = bz0 + d(3)
			     if (.not.fix_otime) then
						t0_norm=.true.
			     endif
			     absd=sqrt(d(1)**2+d(2)**2+d(3)**2)
			     if (abs(d(1)).lt.min_shift_epi .and.
     >            abs(d(2)).lt.min_shift_epi .and.
     >            abs(d(3)).lt.min_shift_depth)  then
						x0=best_x0
						y0=best_y0
						z0=best_z0
						t0=best_t0
						endit=.true.
			     endif
			     go to 30
			 else
			     rmsresp = rmsres
			     best_x0=x0
			     best_y0=y0
			     best_z0=z0
			     best_t0=t0
			     n_recomp_it = 0
			 endif
		endif
35    continue
		call iter_2
		if (det.eq.0.) then
			 write (*,'(1x,a,": Hessian matrix C is singular.")')
     >    prog_name
			 go to 160
		endif
		if (endit) then
			 if (rmsres_co.ne.9.99**2) then
			     if (sigma1(1).gt.0) then
						rmsres_co=9.99**2
			     else
						rmsres_co=rmsres_co+model_error**2
			     endif
			 endif
			 if (rmsres_co.ne.9.99**2) then
			     call cov_matrix
			 endif
			 go to 155
		else
			 do i=1,4
			     sum4=0.0
			     do j=1,4
						sum4=sum4+c(i,j)*b(j)
			     end do
			     d(i)=sum4/scale(i)
			 end do
		endif
		x0=x0+d(1)
		y0=y0+d(2)
		if (abs(d(3)).gt.max_change_z) then
			 d(3)=sign(max_change_z,d(3))
			 if (.not.fix_otime) then
			     t0_norm=.true.
			 endif
		endif
		c_hypo1(1)=c_hypo(1)
		c_hypo1(2)=c_hypo(2)
		c_hypo1(3)=c_hypo(3)
		call trans(c_hypo1(1),c_hypo1(2),c_hypo1(3),2)
		call spline_value (0,c_hypo1,vd,0)
		zsurf=-vd(1)
		if (z0+d(3).lt.zsurf) then
			 d(3)=zsurf-z0
			 z0=zsurf
			 if (.not.fix_otime) then
			     t0_norm=.true.
			 endif
		else
			 z0=z0+d(3)
		endif
		if (fix_surface) then
			 d(3)=0.0
			 z0=zsurf
			 if (.not.fix_otime) then
			     t0_norm=.true.
			 endif
		endif
		t0=t0+d(4)
		absd=sqrt(d(1)**2+d(2)**2+d(3)**2)
		if (abs(d(1)).lt.min_shift_epi .and.
     >    abs(d(2)).lt.min_shift_epi .and.
     >    abs(d(3)).lt.min_shift_depth)  then
			 endit=.true.
			 i0=i0-1
			 go to 30
		endif
		if (i0 .lt. maxIter) then
			 go to 30
		else
			 if (scan_depth .or. interactive.eq.'N') then
			     if (interactive.eq.'N') then
						z0=999.99
			     endif
			     x0=999.99
			     y0=999.99
			     rmsres=9.99**2
			 else
			     write (1     ,'(1x,a,": Hypocenter not found in ",i2,
     >        " iterations."/)') prog_name,maxIter
150           continue
			     write (1     ,'(1x,a,": Continue location with final",
     >        " coordinates",/,9x,"of trial hypocenter? (y/n) [n]:_")')
     >        prog_name
			     read (*,'(a)',end=150) answer
	 if(answer.eq.'y')answer='Y'
	 if(answer.eq.'n')answer='N'
			     if (answer.eq.'Y') then
						i0=0
						t0_norm=.true.
						go to 30
			     else if (answer.ne.' ' .and. answer.ne.'N') then
						go to 150
			     endif
			     write (lulist,'(/,9x,"No convergent criterion satisfied "
     >        ,"in ",i2," iterations!!!",/,9x,
     >"---------------------------------------------------",/)')
     >maxIter
			 endif
		endif
155   continue
		if (.not.scan_depth) then
			 call origin_time (n_increase,dmin8)
			 call magni
			 if (i0.lt.maxIter) then
			     call output(lulist,luterm)
			     if (interactive.ne.'N') then
						call nearest_source (source_flag,x0,y0,z0)
			     endif
			     if (.false.           ) then
						call show_matrix
			     endif
			 else
			     call output(lulist,-1)
			 endif
		endif
160   continue
		if (scan_depth) then
			 xp=x0
			 yp=y0
			 zp=z0
			 if (prog_name.eq.'HYPO3D') then
			     call trans (xp,yp,zp,0)
			 endif
	       write (*,'(3x,i2,3x,f8.3,2x,3(1x,f7.3,2x),3x,f7.3)')
     >    i0,xp,yp,zp,t0,sqrt(rmsres)
	       write (lulist,'(3x,i2,3x,f8.3,2x,3(1x,f7.3,2x),3x,f7.3)')
     >    i0,xp,yp,zp,t0,sqrt(rmsres)
			 z0=z0+scan_step
			 if (z0.le.scan_end) then
			     x0=x_start_trial
			     y0=y_start_trial
			     go to 29
			 endif
			 endit=.false.
		endif
162   continue
		if (interactive.eq.'N') then
			 go to 163
		endif
		call dialog_2 (endit,scan_depth,rms_on_net,rms_on_sphere)
		if (rms_on_sphere) then
			 go to 29
		endif
163   continue
		if (interactive.eq.'N') then
			 i_menu=2
		else
			 call dialog_2_1
     >    (endit,prt,scan_depth,i0,maxIter,rms_on_net,
     >    loc_write,rp,i_menu)
		endif	
		if (i_menu.eq.1) then
			 go to 90
		else if (i_menu.eq.2) then
			write(*,*) 'Writing dbfile' 
			call create_dbfile(n_of_location,hypofn(1:(lnblnk(hypofn)-4)))
			 if (interactive.eq.'N') then
			     go to 10
			 endif
			 go to 163
		else if (i_menu.eq.3) then
			 go to 163
		else if (i_menu.eq.4) then
		endif
		end
