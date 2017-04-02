c
		subroutine rms_net_1(dmin8)
c
c*****************************************************************************
c
c  subroutine RMS_NET_1
c
c-----------------------------------------------------------------------------
c
c  purpose:
c
c     initialize start of net, write header for rms_on_net mode
c
c-----------------------------------------------------------------------------
c
c  formal parameters:
c
c     real*8      DMIN8       ...  minimum time from datum8         I
c
c----------------------------------------------------------------------------
c
c  calling convention:
c
c     call rms_net_1 (dmin8)
c
c----------------------------------------------------------------------------
c
c  external references:
c
c     TRANS               mw subroutine
c     REAL8_TO_INT        mw subroutine
c     SPLINE_VALUE        mw subroutine
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
c  formal parameters
c
		real*8          dmin8
c
c  local parameters  ...  none
c
c
c  global parameters
c
		character*255   ch_model_name
		common/chmodn/ ch_model_name
c
		include 'pname.fi'
		include 'list.fi'
c		include 'data_dir.fi'
c		include 'term.fi'
c
c  local variables
c
		real            c_hypo1(3)
		real            zsurf
		real            vd(10)
		character*255    hyponamr
c
c  global variables
c
      real            c_hypo(3)
      integer         no_valid_arrivals
      logical         t0_norm
      logical         endit
      common /it1/    t0_norm,c_hypo,no_valid_arrivals,endit
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
      logical         fix_x
      logical         fix_y
      logical         fix_otime
      common /f_mode/ fix_x,fix_y,fix_otime
c
      logical         fix_depth
      logical         fix_surface
	integer             i0              !no. of iter. cycle
      common /srfc/   fix_surface,fix_depth,i0
c
      real            x0
      real            y0
      real            z0
      common /centr/  x0,y0,z0
c
      integer         year
      integer         month
      integer         day
      integer         hour
      integer         minute
      real            t0
      common /otime/  year,month,day,hour,minute,t0
c
cc      character*10    subdir
cc      character*1     letter_hp
cc      character*3     ch_event_number
cc      common /hnamch/ subdir,letter_hp,ch_event_number
c
cc      integer         subdir_length
cc		logical             source_flag
cc		common /hnami/      subdir_length,source_flag
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
		hyponamr = 'hypofile'
c
c  initialize x0, y0
c
      x0=start_x
      y0=start_y
c
c  test on fix. surface
c
      if (start_depth.eq.999) then
          fix_surface=.true.
          z0=0
      else
          fix_surface=.false.
          z0=start_depth
      endif
c
c  Krovak to local
c
      call trans (x0,y0,z0,1)
c
c  transform to local coordinates for surface computing
c
      c_hypo1(1)=x0
      c_hypo1(2)=y0
      c_hypo1(3)=z0
c
      call trans(c_hypo1(1),c_hypo1(2),c_hypo1(3),2)
c
c  z-coordinate of surface for epicenter coordinates c_hypo(1),c_hypo(2)
c  in computing of z-coordinate of surface ... z-axis is upward
c
      call spline_value (0,c_hypo1,vd,0)
      zsurf=-vd(1)
c
      if (z0.lt.zsurf) then
          z0=zsurf
      endif
c
c  test on norming of start origin time
c
      if (start_otime.eq.0.0) then
          t0_norm=.true.
      else
          t0_norm=.false.
          t0=start_otime
      endif
c
c  write first part of header
c
      if (fix_surface) then
          write (*,
     >    '(/,1x,a,": Rms of residuals on net (surface)"/)')
     >    prog_name
          write (lulist,'(1x,a,
     >    "   location  ...  rms of residuals on net (surface)"/)')
     >    long_prog_name
      else
          write (*,'(/,1x,a,": Rms of residuals on net "/)')
     >    prog_name
          write (lulist,
     >    '(1x,a,"   location  ...  rms of residuals on net "/)')
     >    long_prog_name
      endif
c
c  decode datum
c
      call real8_to_int(dmin8,year,month,day,hour,minute)
c
c  code hypofile name
c
cc      hyponamr=lu21_data//subdir(1:subdir_length)//'/'//
cc     >letter_hp//ch_event_number
c
c  write second part of header
c
      write (lulist,'(" Name of used model        :",a)') ch_model_name
      write (lulist,'(" Name of hypofile          :",a)') hypoNamr
      write (lulist,'(" Minimal recorded arr. time:",
     >2(i2.2,"-"),i2.2,1x,i2.2,":",i2.2,/)')
     >year,month,day,hour,minute
c
      write (*,'(
     >"-----------------------------------------------------------",
     >"----",/,
     >"       x           y           z           t       rmsres",/,
     >"-----------------------------------------------------------",
     >"----")')
c
      write (lulist,'(
     >" ----------------------------------------------------------",
     >"----",/,
     >"       x           y           z           t       rmsres",/,
     >" ----------------------------------------------------------",
     >"----")')
c
      return
      end
