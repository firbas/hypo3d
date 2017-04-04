c
C$ema /hyp/,/rec/,/stmod/
		subroutine o_hy3(lulist,lu1)
c
c*****************************************************************************
c
c  subroutine O_HY3
c
c-----------------------------------------------------------------------------
c
c  purpose:
c
c     output results to list devices/files (list-file, database-file)
c     lu1=0      ... output to db file
c     lu1=-1     ... output in the case of no convergence
c     lu1 .ne. 0 ... output to lu and to list file
c
c-----------------------------------------------------------------------------
c
c  formal parameters:
c
c     integer     LULIST      ...  lu for output                    I
c     integer     LU1         ...  lu for output resp.
c                                  switch of output format          I
c
c----------------------------------------------------------------------------
c
c  calling convention:
c
c     call o_hy3 (lulist,lu1)
c
c----------------------------------------------------------------------------
c
c  external references:
c
c     TRANS               mw subroutine
c     DATUM               mw subroutine
c     SORT_X              mw subroutine
c     EXEC                RL subroutine
c
c----------------------------------------------------------------------------
c
c  programmed:  87-07  01.00  mw OUTPUT.F
c  programmed:2017-04  10.59  pc O_HY3 cloned from OUTPUT.F
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
		integer lulist
		integer lu1
c
c  local parameters  ...  none
c
c
c  global parameters
c
		include 'param.fi'
		include 'pname.fi'
		include 'error.fi'
c
c  local variables
c
		character     fmt1*151,fmt2*36,fmt3*55,fmt4*64,fmt5*64,fmt6*64
		character     fmt7*59,fmt8*47,fmt9*52,fmt10*39,fmt11*48,fmt12*48
		character     fmt13*50
		character*22  whole_date
		integer it1,it2
      integer imt1,imt2
      integer minut1,minut2
      integer itime(5)
	integer ita(9)
	integer stime
      integer iyear
      integer lu
      integer iain
      integer temp1(nrec_max)
      integer temp2
      integer iaz
      integer i,j
      integer igap
      integer isec,msec
c
		integer  i1a
		integer  i2a
		integer	 i3a
		integer  i4a
c		
      real    delay
      real    coef
      real    dtemp
      real    deter
      real    d11,d21,d22
      real    theta
      real    al,bl
		real    l1,l2
      real    dxer,dyer,dzer,dter
      real    dx,dy
      real    temp(nrec_max)
      real    gap
      real    d_hypo(nrec_max)
      real    d_epi(nrec_max)
C      ema     d_hypo,d_epi
      real    az(nrec_max)
      logical no_convergence
      real    xp,yp,zp
      real*8  fi, rla
c
c  global variables
c
      integer             key(nrec_max)   !key field
      common /stmod/      key
c
	character*255      hypfn
	character*255      modfn
	common /hymofn/ hypfn,modfn
c
      real                amp(nrec_max)
      real                freq(nrec_max)
      common /ampli/      amp,freq
c
      integer             rok
      integer             mesic
      integer             den
      integer             hodina
      integer             minuta
      real                t0
      common /otime/      rok,mesic,den,hodina,minuta,t0
c
      real                x0
      real                y0
      real                z0
      common /centr/      x0,y0,z0
c
      integer             nrec
      real                xstat(nStation)
      real                ystat(nStation)
      real                zstat(nStation)
      real                dly(nStation)
      common /rec/        nrec,xstat,ystat,zstat,dly
c
      character*1         type(nrec_max)
      common /chrec/      type
c
      real            co(4,4)
      real            id(4,4)
      real            re(4,4)
      real            rmsres
      real            rmsres_co
      common /cov/    co,rmsres,rmsres_co,id,re
c
      real                trec(nrec_max)
      real                wt(nrec_max)
		real                avwt
      common /hyp/        trec,wt,avwt
c
      character*4         rec_name(nrec_max)
      common /chhyp/      rec_name
c
      real                tcal(nrec_max)
      real              xc  (4,nrec_max)
      common /cal_time/ tcal,xc
c
      real                toa(nrec_max)
      common /toa/        toa
c
      real                xmag(nrec_max)
      real                sdm
      real                avm
      common /mag/        xmag,avm,sdm
c
      logical             fix_surface
      logical             fix_depth
      integer             i0
      common /srfc/       fix_surface,fix_depth,i0
c
      character*255        ch_model_name
      common /chmodn/     ch_model_name
c
      real                x_start
      real                y_start
      real                z_start
      real                ot_start
      common /start/      x_start,y_start,z_start,ot_start
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
      logical         fix_x
      logical         fix_y
      logical         fix_otime
      common /f_mode/ fix_x,fix_y,fix_otime
c
		logical         scan_depth
		real            scan_start
		real            scan_end
		real            scan_step
		common /scan/   scan_depth,scan_start,scan_end,scan_step
c
      real            nangle
      common /nangl/  nangle

c
c  functions
c
       integer time
c
c
c  *******************
c  end of declarations
c  *******************
c
c=============================================================================
c
c
      xp=x0
      yp=y0
		zp=z0
c
c  test on 3D case
c
      if (prog_name.eq.'HYPO3D') then
          call trans (xp,yp,zp,0)
      endif
c
c  init. variable
c
      no_convergence=.false.
      lu=lu1
c
c  modify covariance matrix
c  for fixed coordinates
c
      if (fix_x) then
          do i=1,4
              co(1,i)=0.0
              co(i,1)=0.0
          end do
      endif
c
      if (fix_y) then
          do i=1,4
              co(2,i)=0.0
              co(i,2)=0.0
          end do
      endif
c
      if (fix_depth .or. scan_depth) then
			 do i=1,4
			     co(3,i)=0.0
			     co(i,3)=0.0
			 end do
		endif
c
		if (fix_otime) then
			 do i=1,4
			     co(4,i)=0.0
			     co(i,4)=0.0
			 end do
		endif
c
		if (.not.fix_x .and. .not.fix_y
     >  .and. rmsres_co.ne.9.99**2) then
c
c  error ellipse for epicenter
c
			 deter=co(1,1)*co(2,2)-co(1,2)*co(2,1)
			 d11=co(2,2)/deter
			 d22=co(1,1)/deter
			 d21=-co(2,1)/deter
			 theta=0.5*atan( 2.*d21/(d11-d22) )
			 al=d11*cos(theta)**2 + 2.*d21*cos(theta)*sin(theta) +
     >       d22*sin(theta)**2
			 bl=d11*sin(theta)**2 - 2.*d21*cos(theta)*sin(theta) +
     >       d22*cos(theta)**2
c
c  test on numerical errors
c
			 if (al.le.0.0) then
			     l1=999.99
			 else
			     l1=sqrt(1./al)
			 endif
c
			 if (bl.le.0.0) then
			     l2=999.99
			 else
			     l2=sqrt(1./bl)
			 endif
c
c  theta is the angle from x-axis to the semi-major axis of the error ellipse
c
			 theta = theta*180./pi
cc!!??    theta = 90.-theta
			 theta = mod(360.0-nangle+theta,360.0)
		else
			 l1=9.99
			 l2=9.99
			 theta=9.99
		endif
c
		if (rmsres_co.eq.9.99**2) then
c
c  no degree of freedom
c
			 dxer=9.99
			 dyer=9.99
			 dzer=9.99
			 dter=9.99
			 l1=9.99
			 l2=9.99
			 theta=9.0
		else
c
c  square root of diagonal elements of covariance matrix ... dispersion
c    of hypocenter coordinate estimates
c
			 dxer=sqrt(abs(co(1,1)))
			 dyer=sqrt(abs(co(2,2)))
			 dzer=sqrt(abs(co(3,3)))
			 dter=sqrt(abs(co(4,4)))
		endif
c
		j = 0
		do i = 1,nrec
			 dx = x0-xstat(key(i))
			 dy = y0-ystat(key(i))
c
c  az(i) ... angle between x-axis and direction recorder to source minus
c             angle between x-axis and north
c
			 if (dx.eq.0.0) then
			     az(i) = mod(360.0-nangle+90.0,360.0)
			 else
			     if (dx.gt.0.0) then
						if (dy.gt.0.0) then
							 temp2=atan(dy/dx)*57.29578
						else
							 dy=-dy
							 temp2=360.0-atan(dy/dx)*57.29578
						endif
			     else
						if (dy.gt.0.0) then
							 dx=-dx
							 temp2=180.0-atan(dy/dx)*57.29578
						else
							 dx=-dx
							 dy=-dy
							 temp2=180.0+atan(dy/dx)*57.29578
						endif
			     endif
c
			     az(i) = mod(360.0-nangle+temp2,360.0)
			 endif
c
			 if (wt(i).gt.0.0) then
			     j = j + 1
			     temp(j)=az(i)
			 endif
c
			 d_hypo(i) = sqrt( dx**2 + dy**2 + (zstat(key(i))-zp)**2 )
			 d_epi (i) = sqrt( dx**2 + dy**2 )
		end do
c
		call sort_x(temp,temp1,j)
		gap = temp(1)+360.-temp(j)
c
		do i=2,j
			 dtemp=temp(i)-temp(i-1)
			 if (dtemp.gt.gap) then
			     gap=dtemp
			 endif
		end do
c
		igap=gap+0.5
c
c provedeme transformaci data do tvaru rr-mm-dd  hh:mm:ss.ss
c
		isec=t_orig
		msec=(t_orig-isec)*1000.
		write (whole_date,
     >'(2(i2.2,"-"),i2.2,2x,2(i2.2,":"),i2.2,".",i3.3)')
     >year_orig,month_orig,day_orig,hour_orig,minute_orig,isec,msec
c
c  test on list mode
c
		if (lu.eq.0) then
c
c  lu=0 ... output to db file
c
			 go to 10
		else if (lu.eq.-1) then
c
c  lu=-1 ... output in the case of no convergence
c
			 no_convergence=.true.
			 lu=0
			 whole_date=' '
		endif
c
c no_convegence output deleted
c
c	label 10 - regular output to dbfile begins ###
c
10    continue
      write (lulist,'("program       :",a)')
     >prog_name1//prog_name2
      write (lulist,'("model         :",a)') modfn(1:lnblnk(modfn))       ! ch_model_name
      write (lulist,'("model error   :",f5.3," s")')
     >model_error
      write (lulist,'("reading error :",f5.3," s")')
     >reading_error
c
c  get system time
c
c  Following 7 lines replace call exec:
	stime = time ()
	call ltime (stime,ita)
	itime(2)=ita(1)
	itime(3)=ita(2)
	itime(4)=ita(3)
	itime(5)=ita(8)+1
	iyear = ita(6)+2000
c
	      itime(1)=1
      call datum (iyear,itime(1),itime(5),itime(4),itime(3),itime(2))
      iyear=iyear-(iyear/100)*100
      write (lulist,'("create time   :",i2.2,"-",i2.2,"-",i2.2,1x,
     >i2.2,":",i2.2,":",i2.2)') iyear,itime(1),itime(5),itime(4),
     >itime(3),itime(2)
      write (lulist,'("event",9x,":",a)') hypfn(1:lnblnk(hypfn))         ! lu21_data//
cc     >subdir(1:subdir_length)//'/'//letter_hp//ch_event_number
      write (lulist,'("start(x,y,z,t):(",
     >f7.2,",",f7.2,",",f7.2,",",f7.2,")",//)')
     >x_start,y_start,z_start,ot_start
      write (lulist,'("reference time:",
     >i2.2,"-",i2.2,"-",i2.2,1x,i2.2,":",i2.2)')
     >rok,mesic,den,hodina,minuta
c
c header for station data
c
      write (lulist,
     *'("----------------------------------------------------------",
     *  "------------")')
      write (lulist,'(" sta  |obs. t.|cal. t.|res. |amplitude"
     *,"|freq|w| epi |hypo |azm|ain|xmag")')
      write(lulist, '("      |  [s]  |  [s]  | [s] |  [m/s]  "
     *,"|[Hz]| |[km] |[km] |[o]|[o]|    ")')
      write (lulist,
     *'("----------------------------------------------------------",
     *  "------------")')
c
c cycle for writing station distances,etc.
c
      do i=1,nrec
          iaz=az(i)+0.5
          iain=toa(i)+0.5
          if (zp.lt.surf_ev) then
c
c  model for surface event ... with station delays
c
              delay=dly(key(i))
          else
              delay=0.0
          endif
c
          if (type(i).eq.'S') then
              coef=p_over_s
	      write (lulist,916) rec_name(i),type(i),trec(i),tcal(i)+t0,
     >    trec(i)-tcal(i)-t0-coef*delay,
     >    amp(i),freq(i),int(4.-4.*wt(i)),d_epi(i),d_hypo(i),iaz,
     >    int(toa(i)),xmag(i) 
         else
              coef=1.
              write (lulist,916) rec_name(i),type(i),trec(i),tcal(i)+t0,
     >    trec(i)-tcal(i)-t0-coef*delay,
     >    amp(i),freq(i),int(4.-4.*wt(i)),d_epi(i),d_hypo(i),iaz,
     >    int(toa(i))        
	 endif
c
c
 916      format (a4,' ',a1,'|',
     >    f7.2,   '|',f7.2,   '|',f5.3,'|',1pe9.2,'|',0pf4.1,
     >    '|',i1,'|',0pf5.1,'|',0pf5.1,
     >    '|',i3,'|',i3,'|',f4.1)
      end do
c
      call XY2FL (yp*1000, xp*1000, fi, rla)
      print *,xp*1000,yp*1000,fi,rla
      write (lulist,926) whole_date,dter,xp,dxer,fi,yp,dyer,rla,zp,dzer, avm,sdm,sqrt(rmsres),igap,i0,l1,l2,int(theta)
c
cc		write (lulist,*)'theta:',theta
c
926   format (//
     *"hypocenter data:",/,"----------------",/,
     *"origin time          t:",2x,a22,1x,"+-",1x,f6.3,/,
     *"x-coordinate         x:",2x,f7.2,1x,"+-",1x,f6.2,4x,"km",5x,"(fi:",f10.6," deg)",/,
     *"y-coordinate         y:",2x,f7.2,1x,"+-",1x,f6.2,4x,"km",1x,"(lambda:",f10.6," deg)"/,
     *"depth                z:",2x,f7.2,1x,"+-",1x,f6.2,4x,"km",/,
     *"magnitude           ml:",2x,f7.2,1x,"+-",1x,f6.2,4x,/,
     *"rms of time residuals :",8x,f6.2,9x,"s",/,
     *"angular gap           :",11x,i3,9x,"degrees",/,
     *"number of iterations  :",11x,i3,/,
     *"error ellipse axis l1 :",8x,f6.2,9x,"km",/,
     *"              axis l2 :",8x,f6.2,9x,"km",/,
     *"              theta   :",11x,i3,9x,"degrees")
c
20    continue
c
      return
      end
