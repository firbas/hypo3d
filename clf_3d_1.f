c
C$ema /rec/,/stmod/
		subroutine clf_1 (dmin8,n_loc)
c
c*****************************************************************************
c
c  subroutine CLF_1
c
c-----------------------------------------------------------------------------
c
c  purpose:
c
c     first part of locfile ... for arrivals contained in hypofile
c
c-----------------------------------------------------------------------------
c
c  formal parameters:
c
c     real*8      DMIN8       ...  minimum time of datum8           I
c
c----------------------------------------------------------------------------
c
c  calling convention:
c
c     call clf_1 (dmin8)
c
c----------------------------------------------------------------------------
c
c  external references:
c
c     DATUM               mw subroutine
c     REAL8_TO_INT        mw subroutine
c
c----------------------------------------------------------------------------
c
c  programmed:  87-05  01.00  mw
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
		real*8       dmin8
		integer      n_loc
c
c  local parameters  ...  none
c
c
c  global parameters
c
		include 'param.fi'
		include 'data_dir.fi'
c
c  local variables
c
cc		integer         number
		integer         isec
		integer         msec
		integer         minute1,minute2
		integer         hour1,hour2
      integer         day1,day2
      integer         month1,month2
      integer         year1,year2
      integer         i
      real            coef
      real            delay
cc      character*3     ch_event_number
cc      character*63    locnamr
		character*255 locfn
C Z
      real            cep(3)
      integer         no_valid_arrivals
      logical         t0_norm
      logical         endit
      common /it1/    t0_norm,cep,no_valid_arrivals,endit
c
c  global variables
c
      integer           minute
      integer           hour
		integer           day
      integer           month
      integer           year
      real              t0
      common /otime/    year,month,day,hour,minute,t0
c
      integer           nrec
      real              xstat(nStation)
      real              ystat(nStation)
      real              zstat(nStation)
      real              dly  (nStation)
      common /rec/      nrec,xstat,ystat,zstat,dly
c
      real*8            datum8(nrec_max)
      integer           ichan (nrec_max)
      common /dat8/     datum8,ichan
c
      real              tcal(nrec_max)
      real              xc  (4,nrec_max)
      common /cal_time/ tcal,xc
c
      integer           key(nrec_max)
      common /stmod/    key
c
      character*1       type(nrec_max)
      common /chrec/    type
c
      character*4       rec_name(nrec_max)
      common /chhyp/    rec_name
c
cc      character*10    subdir              !name of subdirectory
cc      character*1     letter_hp           !letter for hp tape
cc      character*3     ch_fevent_number    !first event to localize
cc      character*3     ch_levent_number    !last event to localize
cc      character*1     interactive         !flag for interactive mode
cc      character*1     chfix_depth         !flag for fixed depth
cc      character*16    chfix_value         !value of fixed depth
cc      character*6     sname               !name of file with start coord.
cc      common /hnamch/ subdir,letter_hp,ch_fevent_number,
cc     >                ch_levent_number,interactive,chfix_depth,
cc     >                chfix_value,sname
c
		integer             subdir_length   !length of subdir. name
		logical             source_flag
		common /hnami/      subdir_length,source_flag
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
c  evaluate ch_event_number
c
cc      read (ch_fevent_number,*) number
cc      number=number+n_loc-1
cc		write (ch_event_number,'(i3.3)') number
c
c  code locfile name
c
cc      locnamr=lu21_data//subdir(1:subdir_length)//'/'//
cc     >letter_hp//ch_event_number//'.LOC'
c
c  open loc-file
c
	call setfn('locfile',8,locfn)
      open (luloc,file=locfn)
c
c  decode datum in dmin8
c
      call real8_to_int(dmin8,year1,month1,day1,hour1,minute1)
c
c  cycle over arrivals in hypo-file
c
      do i=1,nrec
c
c  test on surface event (depth less the surf_ev) ... reasonable only
c  in 1D case
c
          if (cep(3).lt.surf_ev) then
c
c  model for surface event ... with station delays
c
              delay=dly(key(i))
          else
              delay=0.0
          endif
c
c  test on type of arrival ... set coef
c
          if (type(i).eq.'S') then
              coef=p_over_s
          else
              coef=1.
          endif
c
c  decode isec, msec from tcal
c
          isec=tcal(i)+t0+coef*delay+0.0005
          msec=(tcal(i)+t0+coef*delay+0.0005-float(isec))*1000.
c
c  normalize datum of arrival (year ... seconds)
c
          year2=year1
          month2=month1
          day2=day1
          hour2=hour1
          minute2=minute1
          call datum(year2,month2,day2,hour2,minute2,isec)
c
c  write one line for one arrival into the loc-file
c
          write (luloc,'(a4,1x,a1,7(1x,i2.2),1x,i3.3)')
     >    rec_name(i),type(i),ichan(i),year2,month2,day2,
     >    hour2,minute2,isec, msec
      end do
c
      return
      end
