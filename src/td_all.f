
      subroutine td_all (c_hypo)
c  travel times and derivatives for all stations
c----------------------------------------------------------------------------
c  programmed:  87-06  01.00  mw
c----------------------------------------------------------------------------

      implicit none

      real    c_hypo(3)   ! coordinates of trial hypocenter

c  global parameters
      include 'param.fi'

c  global variables
      integer key(nrec_max)
      common /stmod/    key

      character*1  phase(nrec_max)   !seismic phase of recorded arr.
      common /chrec/ phase

      integer nrec
      real    xstat(nstation)
      real    ystat(nstation)
      real    zstat(nstation)
      real    dly(nStation)
      real    dly_s(nStation)
      common /rec/ nrec,xstat,ystat,zstat,dly,dly_s

      real    tcal(nrec_max)    !computed travel times
      real    xc(4,nrec_max)    !derivatives and residuals
      common /cal_time/ tcal,xc

      real    toa(nrec_max)
      common /toa/ toa          !take-off angle

c ----------------------------------------------------------------------
c 2024-04-22 pz
c two velocity models
      real v3p(x_layer,y_layer,z_layer)
      real v3s(x_layer,y_layer,z_layer)
      common /model_3d/ v3p,v3s

c pointer for swapping between two velocity models
      logical split_rays
      real v3
      pointer(ip_v3,v3(x_layer,y_layer,z_layer))
      common /model_stat/ ip_v3, split_rays
c ----------------------------------------------------------------------

      logical           exchange
      real              temp
      real              c_stat(3)
      real              td(4)
      integer           i
      real              delay

c  main loop over recorded arrivals
      do i = 1,nrec

c  initialize exchange variable
         exchange = .false.

c  test on elevation of first point (hypocenter) and second point (station)
         if (c_hypo(3).lt.zstat(key(i))) then
c  second point is below first point
c  exchange coordinates of first (hypocenter) and second (station) point
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
         endif

c  initialisation of layer system for this station
c  z-axis is downward  ...  in subroutine layer is upward (according model)
         call layer(-zstat(key(i)))

         if (.not. split_rays) then
            ip_v3=loc(v3p)
c  1D velocity model for source-receiver profile
            call velocity
     >          (c_hypo(1),c_hypo(2),(xstat(key(i))),(ystat(key(i))))
         endif
        
c  swap P-velocity and S-velocity model
         if (phase(i) .eq. 'S') then
            ip_v3=loc(v3s)
            delay=dly_s(key(i))
         else
            ip_v3=loc(v3p)
            delay=dly(key(i))
         end if

         if (split_rays) then
c  1D velocity model for source-receiver profile
            call velocity
     >          (c_hypo(1),c_hypo(2),(xstat(key(i))),(ystat(key(i))))
         endif

c  set array of coordinates of station
         c_stat(1)=xstat(key(i))
         c_stat(2)=ystat(key(i))
         c_stat(3)=zstat(key(i))
c  travel time, derivatives and take off angle for two point tracing from
c  hypocenter to station
         call rt_3d (c_hypo,c_stat,td,toa(i),exchange)

         if (exchange) then
c  return coordinates of points to original status
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

c  1) travel time
         tcal(i)=td(1)
c  derivatives of travel time
c  2) derivative on the x
         xc(1,i)=td(2)
c  3) derivative on the y
         xc(2,i)=td(3)
c  4) derivative on the z
         xc(3,i)=td(4)
c  station delay
         tcal(i)=tcal(i)+delay

      end do  !loop over recorded arrivals

      return
      end subroutine td_all
