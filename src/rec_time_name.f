c
C$ema /rec/,/hyp/,/stmod/
		subroutine rec_time_name
     >(correct,rp,no_valid_arrival,dmin8,n_increase,inumber)
c
c*****************************************************************************
c
c  subroutine REC_TIME_NAME
c
c-----------------------------------------------------------------------------
c
c  purpose:
c
c  there are three cases:
c     I) inumber equal to 1
c        set only the first approx. of hypocenter
c    II) inumber equal to 2
c        norm trec(), set no_valid_arr
c   III) switch inumber not equal to 1 and not equal to 2
c         test station names of recording stations with station list, take out
c         the wrong stations, set appropriate flags
c
c-----------------------------------------------------------------------------
c
c  formal parameters:
c
c     logical     RP               ... flag for repeat of location   I
c     logical     CORRECT          ... flag for correct. in hypofile I
c     integer     NO_VALID_ARRIVAL ... no. of valid arrivals         O
c     integer     N_INCREASE       ... no. of increasing in trec()   I/O
c     integer     INUMBER          ... switch for the run of progr.  I
c     real*8      DMIN8            ... double real repr. of minimum
c                                      datum of arrival              I
c
c----------------------------------------------------------------------------
c
c  calling convention:
c
c     call rec_time_name(correct,rp,no_valid_arrival,dmin8,n_increase,inumber)
c
c----------------------------------------------------------------------------
c
c  external references:
c
c     DATUM               mw subroutine
c     REAL8_TO_INT        mw subroutine
c     TRANS               mw subroutine
c
c----------------------------------------------------------------------------
c
c  programmed:  86-12  01.00  mw  original version
c               87-03  01.01  mw  switch for run of program
c
c*****************************************************************************
c
c
c  ************
c  declarations
c  ************
c
		implicit none
c
c  formal parameters
c
		logical rp
		logical correct
		integer no_valid_arrival
		integer n_increase
		integer inumber
		real*8  dmin8
c
c  local parameters  ...  none
c
c
c  global parameters
c
		include 'param.fi'
		include 'pname.fi'
c
c  local variables
c
                integer   i6a	
		integer isec
		integer indx
		integer n0
		integer i
		integer j
		integer k
		integer l
c		real    t0
		real    tmin
		real    x_temp
		real    y_temp
		real    z_temp
c
c  global variables
c
		character*4     rec_name(nrec_max)
		common /chhyp/  rec_name
c
		integer         nstat
		character*4     stat_name(nrec_max)
		common /stnam/  nstat,stat_name
c
		character*1     type    (nrec_max)
		common /chrec/  type
c
		integer         key     (nrec_max)
		common /stmod/  key
c
		real            trec    (nrec_max)
		real            wt      (nrec_max)
		real                avwt            !average weight
		common /hyp/        trec,wt,avwt
c
		real*8          datum8  (nrec_max)
		integer         ichan   (nrec_max)
		common /dat8/   datum8,ichan
c
		real            amp     (nrec_max)
		real            freq    (nrec_max)
		common /ampli/  amp,freq
c
		real            x0
		real            y0
		real            z0
		common /centr/  x0,y0,z0
c
		logical         fix_surface
		logical         fix_depth
		integer             i0              !no. of iter. cycle
		common /srfc/   fix_surface,fix_depth,i0
c
		integer year
		integer month
		integer day
		integer hour
		integer minute
                real    t0
		common /otime/  year,month,day,hour,minute,t0
c
		real            x_start
		real            y_start
		real            z_start
		real            ot_start
		common /start/  x_start,y_start,z_start,ot_start
c
		real              x_start_trial
		real              y_start_trial
		common /t_start/  x_start_trial,y_start_trial
c
		integer         nrec
		real            xstat(nstation)
		real            ystat(nstation)
		real            zstat(nstation)
		real                dly(nStation)
		common /rec/    nrec,xstat,ystat,zstat,dly
c
		real            c_hypo(3)           !coord. of hypocenter
		integer         no_valid_arrivals   !no. of valid arrivals
		logical         t0_norm             !norm of origin time?
		logical         endit               !end of iteration process?
		common /it1/    t0_norm,c_hypo,no_valid_arrivals,endit
c
		logical        err_stname
		logical        w_changes
		common /ernam/ err_stname,w_changes
c
c  functions  ...  none
c
c
c  *******************
c  end of declarations
c  *******************
c
c=============================================================================
c
c
c  test on the switch
c
		if (inumber.eq.1) then
c
c
			 go to 50
		else if (inumber.eq.2) then
c
c
			 go to 40
		endif
c
c
c
c  test on repeat of location
c
		if (.not.rp) then
c
c  initialize of flag for error in station names
c
			 err_stname=.false.
c
			 j = 0
			 k = 0
			 no_valid_arrival  = 0
			 do while (j .lt. nrec)
			     j = j + 1
			     do i=1,nstat
						if (rec_name(j) .eq. stat_name(i)) then
c
c  station found in the list of station
c
							 k = k + 1
							 key(k)=i
							 if (wt(j).gt.0.0) then
							     no_valid_arrival = no_valid_arrival + 1
							 endif
							 go to 110
						endif
			     end do
c
c  set flag for error in station names
c
			     err_stname=.true.
c
			     write (*,'(1x,a,": Error - station ",a4,
     >        " not found in station list -> will be ignored.")')
     >        prog_name,rec_name(j)
c
c  take out the wrong station
c
			     do l = j,nrec-1
						rec_name(l) = rec_name(l+1)
						type(l) = type(l+1)
						ichan(l) = ichan(l+1)
						trec(l) = trec(l+1)
						wt(l) = wt(l+1)
						amp(l) = amp(l+1)
						freq(l) = freq(l+1)
						datum8(l) = datum8(l+1)
			     end do
			     nrec = nrec - 1
			     j = j - 1
110           continue
			 end do
		endif
c
c  test on no. of recording
c
		if (nrec .lt. 3) then
			 write (*,
     >    '(1x,a,": Error - no. of arrivals in hypofile ",
     >    " < 3.")') prog_name
c
			 return
c
			 go to 100
		endif
c
40    continue
c
c  test on repeat of location or correct of hypofile
c
		if (rp .or. correct) then
c
c  search for minimum of trec
c
			 tmin=trec(1)
c
			 do i=2,nrec
			     if (trec(i).lt.tmin) then
						tmin=trec(i)
			     endif
			 end do
c
c  decode dmin8
c
			 call Real8_to_int (dmin8,year,month,day,hour,minute)
c
c  normalize trec
c
			 do while (tmin.lt.0.0)
			     tmin=tmin+60.0
			     minute=minute-1
			     do i=1,nrec
						trec(i)=trec(i)+60.0
			     end do
			 end do
c
c  encode dmin8
c
		i6a=0
			 call Datum (year,month,day,hour,minute,i6a)
			 dmin8=year*1.d08+month*1.d06+
     >    day*1.d04+hour*1.d02+minute*1.d00
c
c  init. variable
c
			 no_valid_arrival=0
		else
c
c  init. dmin8 variable by first datum8
c
			 dmin8=datum8(1)
c
c  search for minimum of datum8
c
			 do i=2,nrec
			     if (datum8(i).lt.dmin8) then
						dmin8=datum8(i)
			     endif
			 end do
		endif
c
c  cycle for norming of datum variables, evaluating of no_valid_arrival
c
		do i=1,nrec
c
c  test on repeat of location or correct hypofile
c
			 if (rp .or. correct) then
c
c  increase no_valid_arrival in the case of positive weight of arrival
c
			     if (wt(i).gt.0.0) then
						no_valid_arrival = no_valid_arrival + 1
			     endif
			 endif
c
			 if (inumber.eq.2) then
			     go to 45
			 endif
c
c  test on repeat of location
c
			 if (rp) then
c
c  decode datum8
c
			     call Real8_to_int
     >        (datum8(i),year,month,day,hour,minute)
c
c  cycle for decreasing of time of rec.
c
			     do j=1,n_increase
						trec(i)=trec(i)-60.0
						minute=minute-1
			     end do
c
c  norm. the datum variables
c
			     isec=0
			     call Datum(year,month,day,hour,minute,isec)
c
c  set datum8
c
			     datum8(i)=year*1.d08+month*1.d06+day*1.d04+
     >        hour*1.d02+minute*1.d00
c
c  reset n_increase variable
c
			     n_increase=0
			 else
c
c  cycle for norming of trec
c
			     do while(dmin8.lt.datum8(i))
c
c  decode datum8
c
						call Real8_to_int
     >            (datum8(i),year,month,day,hour,minute)
c
c  decrease trec
c
						minute=minute-1
						trec(i)=trec(i)+60.0
c
c  normalize datum variables
c
						isec=0
						call Datum(year,month,day,hour,minute,isec)
c
c  encode datum8
c
						datum8(i)=year*1.d08+month*1.d06+day*1.d04+
     >            hour*1.d02+minute*1.d00
			     end do
			 endif
45        continue
		end do
c
c  initialize of datum's variables for the case of identity of each datum8
c
		call Real8_to_int(dmin8,year,month,day,hour,minute)
c
		go to 100
c
50    continue
c
		if (abs(ot_start).lt.1E-7) then
c		if (ot_start.eq.0.0) then
			 t0_norm=.true.
		endif
c
c  set first approx. of origin time to rec. time on the nearest station
c      ----------------------------
c  in the case [0,0] set first epi. approx to coordinates of the nearest
c   station + 0.1
c
		indx = 1
		if (abs(x_start) .lt. 1E-7 .or. 
     >              abs(y_start) .lt. 1E-7 .or.
     >              abs(z_start) .lt. 1E-7      ) then
c		if (x_start .eq. 0. .or. y_start .eq. 0. .or.
c     >    z_start .eq. 0.) then
			 indx = 0                       !nejbl. stanice
		endif
c
c  the nearest station
c
		n0=1
		t0=1e20
		do i=1,nrec
			 if (type(i) .eq. 'S') then
			     go to 120
			 endif
			 if (trec(i) .gt. t0) then
			     go to 120
			 endif
			 t0=trec(i)
			 n0=i
120       continue
		end do
c
		if(indx.ne.1) then
c
c  Transf. local to Krovak of coord of the nearest station
c
			 x_temp=xstat(key(n0))
			 y_temp=ystat(key(n0))
			 z_temp=zstat(key(n0))
c
			 call Trans (x_temp,y_temp,z_temp,0)
c
c  compute x0, y0, z0
c
			 if (abs(x_start).lt.1E-7) then
c			 if (x_start.eq.0.0) then
			     x0 = x_temp + 0.1
			 else
			     x0 = x_start
			 endif
c
			 if (abs(y_start).lt.1E-7) then
c			 if (y_start.eq.0.0) then
			     y0 = y_temp + 0.1
			 else
			     y0 = y_start
			 endif
c
			 if (fix_depth) then
			     z0 = z_start
			 else
			     if (abs(z_start).lt.1E-7) then
c			     if (z_start.eq.0.0) then
						z0=z_temp + 0.1
			     else
						z0=z_start
			     endif
			 endif
		else
			 x0 = x_start
			 y0 = y_start
			 z0 = z_start
		endif
c
c  Transf. Krovak to local
c
	        call Trans (x0,y0,z0,1)
c
c  set trial start for scan depth usage
c
		x_start_trial=x0
		y_start_trial=y0
c
c  set values for year, ..., minute  - used in repair for relative time
c
		call Real8_to_int(dmin8,year,month,day,hour,minute)
c
c  return to main
c
100   continue
c
		return
		end
