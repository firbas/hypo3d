c
      subroutine rms_net_2
c
c*****************************************************************************
c
c  subroutine RMS_NET_2
c
c-----------------------------------------------------------------------------
c
c  purpose:
c
c     provides steps in particular directions on net; writes results
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
c     call rms_net_2
c
c----------------------------------------------------------------------------
c
c  external references:
c
c     TRANS               mw subroutine
c     SPLINE_VALUE        mw subroutine
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
c  formal parameters  ...  none
c
c
c  local parameters  ...  none
c
c
c  global parameters
c
         include 'pname.fi'
         include 'list.fi'
c
c  local variables
c
         real            c_hypo1(3)
         real            zsurf
         real            vd(10)
c
c  global variables
c
c
         real            rmsres
         common /cov/    rmsres
c
         logical         t0_norm
         logical         endit
         common /it1/    t0_norm,endit
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
     >                   start_x,end_x,step_x,
     >                   start_y,end_y,step_y,
     >                   start_depth,end_depth,step_depth,
     >                   start_otime,end_otime,step_otime
c
         logical         fix_surface
         common /srfc/   fix_surface
c
         real            x0
         real            y0
         real            z0
         common /centr/  x0,y0,z0
c
         real            t0
         common /otime/  t0
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
c  local to Krovak
c
         call trans(x0,y0,z0,0)
c
c  write one line of results
c
         write (*,'(1x,4(f8.3,4x),f11.6,1x)')
     >   x0,y0,z0,t0,sqrt(rmsres)
c
         write (lulist,'(1x,4(f8.3,4x),f11.6,1x)')
     >   x0,y0,z0,t0,sqrt(rmsres)
c
c  increment coordinate x
c
         x0=x0+step_x
c
c x0,y0 (Krovak)
c It is not needed to transform coordinates for surface computing.
c
         c_hypo1(1)=x0
         c_hypo1(2)=y0
         c_hypo1(3)=z0
c
cc      call trans(c_hypo1(1),c_hypo1(2),c_hypo1(3),2)
c
c  z-coordinate of surface for epicenter coordinates c_hypo(1),c_hypo(2)
c  in computing of z-coordinate of surface ... z-axis is upward
c
         call spline_value (0,c_hypo1,vd)
         zsurf=-vd(1)
c
c  test on fixed surface
c
         if (fix_surface) then
c
c  z-coord. set to value of surface
c
            z0=zsurf
         endif
c
c  test on norm of origin time
c
         if (abs(start_otime).lt.1E-7) then
c      if (start_otime.eq.0.0) then
c
c  set flag for orig. time norm
c
            t0_norm=.true.
         endif
c
c  test on end of coordinate x
c
         if (x0-end_x .ge. step_x/2.) then
c
c  end of coordinate x:
c  set start of coordinate x, increment coordinate y
c
            x0=start_x
            y0=y0+step_y
c
c  test on end of coordinate y
c
            if (y0-end_y .ge. step_y/2.) then
c
c  end of coordinate y:
c  set start of coordinate y
c
               y0=start_y
c
c  test on fixed surface
c
               if (.not.fix_surface) then
c
c  not fixed surface ... increment depth
c
                  z0=z0+step_depth
c
c  test on end of depth
c
                  if (z0-end_depth .ge. step_depth/2.) then
c
c  end of depth:
c  set start value for depth
c
                     z0=start_depth
c
c  test on norm of orig. time
c
                     if (start_otime.ne.0.0) then
                        t0_norm=.false.
                        t0=t0+step_otime
c
c  test on end of orig. time
c
                        if (t0-end_otime .ge. step_otime/2.) then
c
c  end of orig. time: end of rms_on_net mode
c
                           endit=.true.
                        endif
                     else
                        endit=.true.
                     endif
                  endif
               else
c
c  fixed surface ... test orig. time
c
                  if (start_otime.ne.0.0) then
                     t0_norm=.false.
                     t0=t0+step_otime
                  else
                     endit=.true.
                  endif
               endif
            endif
         endif
c
c  Krovak to local
c
         call trans(x0,y0,z0,1)
c
         return
      end subroutine rms_net_2
