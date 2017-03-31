c
		subroutine clf_2
c
c*****************************************************************************
c
c  subroutine CLF_2
c
c-----------------------------------------------------------------------------
c
c  purpose:
c
c     write second part of loc-file ... z-component stations from station
c     header file not included in hypofile
c
c-----------------------------------------------------------------------------
c
c  formal parameters:
c
c     none
c
c----------------------------------------------------------------------------
c
c  calling convention:
c
c     call clf_2
c
c----------------------------------------------------------------------------
c
c  external references:
c
c     DATUM               mw subroutine
c     RT_3D_L             mw subroutine
c
c----------------------------------------------------------------------------
c
c  programmed:  87-04  01.00  mw
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
c  formal parameters  ...  none
c
c
c  local parameters  ...  none
c
c
c  global parameters
c
		include 'param.fi'
		include 'onset.fi'
c
c  local variables
c
		integer         isec
		integer         msec
		integer         minute1
		integer         hour1
		integer         day1
		integer         month1
		integer         year1
		integer         i
      real            seconds
      real            c_hypo(3)
      real            travel_time
      character*1     type_loc
c
c  global variables
c
      real              x0
      real              y0
      real              z0
      common /centr/    x0,y0,z0
c
      integer           year
      integer           month
      integer           day
      integer           hour
      integer           minute
      real              t0
      common /otime/    year,month,day,hour,minute,t0
c
      integer           nstat
      character*4       stat_name(nstation)
      common /stnam/    nstat,stat_name
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
c  cycle over stations
c
      do i=1,nstat
          if (i_onset(i).eq.0) then
c
c  no arrival will be written for this station
c
              go to 100
          else
c
c  one (or more) line will be written into the loc-file
c
c  set value in coord. of hypocenter array
c
              c_hypo(1)=x0
              c_hypo(2)=y0
              c_hypo(3)=z0
c
c  compute travel time from hypocenter to this station (station no. i)
c
              call rt_3d_l(c_hypo,i,travel_time)
c
c  test on type of output
c
              if (i_onset(i).eq.1 .or. i_onset(i).eq.6) then
c
c  i_onset=1 ... only P_arrival will be written
c  i_onset=5 ... only S_arrival will be written
c  i_onset=6 ... P_arrival and S_arrival will be written
c
c
c  output for P_arrival
c
                  type_loc='P'
                  seconds=t0+travel_time
                  year1=year
                  month1=month
                  day1=day
                  hour1=hour
                  minute1=minute
                  isec=seconds
                  msec=nint( (seconds-isec)*1000. )
c
                  call datum (year1,month1,day1,hour1,minute1,isec)
c
                  write (luloc,'(a4,1x,a1,7(1x,i2.2),1x,i3.3)')
     >            stat_name(i),type_loc,i_channel(i)-1,
     >            year1,month1,day1,hour1,minute1,isec,msec
              endif
c
              if (i_onset(i).eq.5 .or. i_onset(i).eq.6) then
c
c  output for S_arrival
c
                  type_loc='S'
                  travel_time=travel_time*p_over_s
                  seconds=t0+travel_time
                  year1=year
                  month1=month
                  day1=day
                  hour1=hour
                  minute1=minute
                  isec=seconds
                  msec=nint( (seconds-isec)*1000. )
c
                  call datum (year1,month1,day1,hour1,minute1,isec)
c
                  write (luloc,'(a4,1x,a1,7(1x,i2.2),1x,i3.3)')
     >            stat_name(i),type_loc,i_channel(i)-1,
     >            year1,month1,day1,hour1,minute1,isec,msec
              endif
          endif
100       continue
c
      enddo
c
c  close the loc-file
c
      close (luloc,status='KEEP')
c
      return
      end
