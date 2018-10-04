c
		subroutine td_all (c_hypo)
c
c*****************************************************************************
c
c  subroutine TD_ALL
c
c-----------------------------------------------------------------------------
c
c  purpose:
c
c     travel times and derivatives for all stations
c
c-----------------------------------------------------------------------------
c
c  formal parameters:
c
c     real        C_HYPO(3)   ...  coordinates of trial hypocenter    I
c
c----------------------------------------------------------------------------
c
c  calling convention:
c
c     call td_all (c_hypo)
c
c----------------------------------------------------------------------------
c
c  external references:
c
c     LAYER               mw subroutine
c     VELOCITY            mw subroutine
c     RT_3D               mw subroutine
c
c----------------------------------------------------------------------------
c
c  programmed:  87-06  01.00  mw
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
		real              c_hypo(3)
c
c  local parameters  ...  none
c
c
c  global parameters
c
		include 'param.fi'
      include 'list.fi'
c
c  local variables
c
		logical           exchange
		real              temp
		real              c_stat(3)
		real              td(4)
		integer           i
c
c  global variables
c
      integer           key(nrec_max)
      common /stmod/    key
c
      character*1       type(nrec_max)   !characteristics of recorded arr.
      common /chrec/    type
c
      integer           nrec
      real              xstat(nstation)
      real              ystat(nstation)
      real              zstat(nstation)
		real                dly(nStation)
      common /rec/      nrec,xstat,ystat,zstat,dly
                                          !characteristics of rec. arrivals
c
      real              tcal(nrec_max)    !computed travel times
      real              xc(4,nrec_max)    !derivatives and residuals
      common /cal_time/ tcal,xc           !computed travel times, derivatives
                                          !and residuals
c
      real              toa(nrec_max)
      common /toa/      toa               !take-off angle
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
c  main cyclus over recorded arrivals
c
      do i = 1,nrec
c
c  initialize exchange variable
c
          exchange = .false.
c
c  test on elevation of first point (hypocenter) and second point (station)
c
          if (c_hypo(3).lt.zstat(key(i))) then
c
c  second point is below first point
c  exchange coordinates of first (hypocenter) and second (station) point
c
              temp=xstat(key(i))
              xstat(key(i))=c_hypo(1)
              c_hypo(1)=temp
  
              temp=ystat(key(i))
              ystat(key(i))=c_hypo(2)
              c_hypo(2)=temp
  
              temp=zstat(key(i))
              zstat(key(i))=c_hypo(3)
              c_hypo(3)=temp
  
              exchange=.true.
  
D     WRITE (LULIST,*) 'EXCHANGE IN COORDINATES'
  
          endif
c
c  initialisation of layer system for this station
c  z-axis is downward  ...  in subroutine layer is upward (according model)
c
          call layer((xstat(key(i))),(ystat(key(i))),(-zstat(key(i))))
c
c  calculate average velocities for this configuration of station, hypocenter
c  and layers
c
          call velocity
     >    (c_hypo(1),c_hypo(2),(xstat(key(i))),(ystat(key(i))))
c
c  set array of coordinates of station
c
          c_stat(1)=xstat(key(i))
          c_stat(2)=ystat(key(i))
          c_stat(3)=zstat(key(i))
c
c  travel time, derivatives and take off angle for two point tracing from
c  hypocenter to station
c
          call rt_3d (c_hypo,c_stat,td,toa(i),exchange)
c
          if (exchange) then
c
c  return coordinates of points to original status
c
              temp=c_hypo(1)
              c_hypo(1)=xstat(key(i))
              xstat(key(i))=temp
  
              temp=c_hypo(2)
              c_hypo(2)=ystat(key(i))
              ystat(key(i))=temp
  
              temp=c_hypo(3)
              c_hypo(3)=zstat(key(i))
              zstat(key(i))=temp
  
          endif
c
c  set arrays for following computing
c
c
c  1) travel time
c
          tcal(i)=td(1)
c
c  derivatives of travel time
c
c
c  2) derivative on the x
c
          xc(1,i)=td(2)
c
c  3) derivative on the y
c
          xc(2,i)=td(3)
c
c  4) derivative on the z
c
          xc(3,i)=td(4)
c
c  test on S-wave
c
          if (type(i) .eq. 'S') then
c
c  s-wave ... provide conversion
c
              tcal(i) =tcal(i)*p_over_s
              xc(1,i)=xc(1,i)*p_over_s
              xc(2,i)=xc(2,i)*p_over_s
              xc(3,i)=xc(3,i)*p_over_s
          endif
c
      end do                              !of main cycle
c
      return
      end
