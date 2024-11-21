c
      subroutine sphere_step (i0,rmsres,endit,t0_norm)
c
c*****************************************************************************
c
c  subroutine SPHERE_STEP
c
c-----------------------------------------------------------------------------
c
c  purpose:
c
c     evalute and write rms of residuals on the sphere centered on hypocenter
c
c-----------------------------------------------------------------------------
c
c  formal parameters:
c
c     integer     I0          ...  number of step                   I
c     real        RMSRES      ...  rms of residuals                 I
c     logical     ENDIT       ...  flag for end of mode             O
c     logical     T0_NORM     ...  flag for norm of orig. time      I
c
c----------------------------------------------------------------------------
c
c  calling convention:
c
c     call sphere_step (i0,rmsres,endit,t0_norm)
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
         logical endit
         integer i0
         real    rmsres
         logical t0_norm
c
c  global parameters
c
         real       sphere_radius            !radius of sphere (centered on
                                             !hypocenter) on which rms of res.
                                             !will be computed (in kilometers)
         parameter (sphere_radius=1.0)

         real    half_side                   !half of side of cube in sphere
         parameter (half_side=sphere_radius/1.73205)

         include 'list.fi'
         include 'pname.fi'
c
c  local variables
c
         integer i
c
c  global variables
c
         real           x0,y0,z0
         common /centr/ x0,y0,z0
c
         real           x_orig,y_orig,z_orig
         real           rms_orig
         integer        no_iter_orig
         common /org/   x_orig,y_orig,z_orig,rms_orig, no_iter_orig
c
         real                t_orig
         common /origin/     t_orig
c
         real    t0
         common /otime/ t0
c
c  static variable
c
         real           s_point(10)
c     common /point/ s_point
         save s_point
c
c
c  *******************
c  end of declarations
c  *******************
c
c=============================================================================
c
c  set square root of rms of res.
c
         rmsres=sqrt(rmsres)
c
c  test on number of step
c
         if (i0.eq.0) then
c
c  store original value of hypocenter space coord. and rms of res.
c
            x_orig=x0
            y_orig=y0
            z_orig=z0
            rms_orig=rmsres
c
c  set coord. for the first point
c
            x0=x_orig
            y0=y_orig
            z0=z_orig-sphere_radius

         else if (i0.eq.1) then
c
c  set coord. for the second point
c
            x0=x_orig
            y0=y_orig
            z0=z_orig+sphere_radius
c
c  store value for first point on sphere
c
            s_point(1)=rmsres-rms_orig

         else if (i0.eq.2) then
c
c  set coord. for the third point
c
            x0=x_orig-half_side
            y0=y_orig-half_side
            z0=z_orig-half_side
c
c  store value for second point on sphere
c
            s_point(2)=rmsres-rms_orig

         else if (i0.eq.3) then
c
c  set coord. for the fourth point
c
            x0=x_orig+half_side
            y0=y_orig-half_side
            z0=z_orig-half_side
c
c  store value for third point on sphere
c
            s_point(3)=rmsres-rms_orig

         else if (i0.eq.4) then
c
c  set coord. for the fifth point
c
            x0=x_orig-half_side
            y0=y_orig+half_side
            z0=z_orig-half_side
c
c  store value for fourth point on sphere
c
            s_point(4)=rmsres-rms_orig

         else if (i0.eq.5) then
c
c  set coord. for the sixth point
c
            x0=x_orig+half_side
            y0=y_orig+half_side
            z0=z_orig-half_side
c
c  store value for fifth point on sphere
c
            s_point(5)=rmsres-rms_orig

         else if (i0.eq.6) then
c
c  set coord. for the seventh point
c
            x0=x_orig-half_side
            y0=y_orig-half_side
            z0=z_orig+half_side
c
c  store value for sixth point on sphere
c
            s_point(6)=rmsres-rms_orig

         else if (i0.eq.7) then
c
c  set coord. for the eighth point
c
            x0=x_orig+half_side
            y0=y_orig-half_side
            z0=z_orig+half_side
c
c  store value for seventh point on sphere
c
            s_point(7)=rmsres-rms_orig

         else if (i0.eq.8) then
c
c  set coord. for the nineth point
c
            x0=x_orig-half_side
            y0=y_orig+half_side
            z0=z_orig+half_side
c
c  store value for eighth point on sphere
c
            s_point(8)=rmsres-rms_orig

         else if (i0.eq.9) then
c
c  set coord. for the tenth point
c
            x0=x_orig+half_side
            y0=y_orig+half_side
            z0=z_orig+half_side
c
c  store value for nineth point on sphere
c
            s_point(9)=rmsres-rms_orig

         else if (i0.eq.10) then
c
c  store value for tenth point on sphere
c
            s_point(10)=rmsres-rms_orig
c
c  write 3D sphere (cube) projected on 2D paper with values s_point(1) ...(10)
c
            go to 10
         endif
c
c  set flag for t0 computing
c
         t0_norm=.true.
c
         go to 20
c
10       continue
c
c  round on values in s_point array to one hunderdth
c
         do i=1,10
            s_point(i)=nint(s_point(i)*100.)/100.
         end do
c
c  cursor home, clear display
c
cc            call clear_display
c
c  new page on the printer
c
         write (lulist,'("1")')
c
c  write header
c
         write (*,'(//,
     >"  Relative Rms Of Residuals On A Sphere Centered",
     >" On The Hypocenter")')
         write (lulist,'(//,8x,
     >"  Relative Rms Of Residuals On A Sphere Centered",
     >" On The Hypocenter",//)')
c
c  write 3D sphere (cube) projected on 2D paper
c
         write (*,'(37x,f5.2)')  s_point(1)
         write (lulist,'(45x,f5.2)')  s_point(1)
         write (*,'(1x,"X ****o",31x,"|")')
         write (lulist,'(9x,"X ****o",31x,"|")')
c
c  test on value of rms of res.
c
         if (s_point(3).lt.1.0 .and. s_point(3).ge.0.0) then
            write (*,'(6x,"/|",21x,f5.2,"*****","|",
     >           "********",f3.2)')
     >      s_point(4),s_point(3)
            write (lulist,'(14x,"/|",21x,f5.2,"*****","|",
     >           "********",f3.2)')
     >      s_point(4),s_point(3)
         else
            write (*,'(6x,"/|",21x,f5.2,"*****","|",
     >           "******",f5.2)')
     >      s_point(4),s_point(3)
            write (lulist,'(14x,"/|",21x,f5.2,"*****","|",
     >           "******",f5.2)')
     >      s_point(4),s_point(3)
         endif
         write (*,'(5x,"/ |",22x,"/|",7x,"|",8x,"/|")')
         write (lulist,'(13x,"/ |",22x,"/|",7x,"|",8x,"/|")')
         write (*,'(4x,"/  |",21x,"/ |",15x,"/",1x,"|")')
         write (lulist,'(12x,"/  |",21x,"/ |",15x,"/",1x,"|")')
c
c  test on value of rms of res.
c
         if (s_point(5).lt.1.0 .and. s_point(5).ge.0.0) then
            write (*,'(26x,f5.2,"***************",f3.2,"|")')
     >      s_point(6),s_point(5)
            write (lulist,'(34x,f5.2,"***************",f3.2,"|")')
     >      s_point(6),s_point(5)
         else
            write (*,'(26x,f5.2,"*************",f5.2,"|")')
     >      s_point(6),s_point(5)
            write (lulist,'(34x,f5.2,"*************",f5.2,"|")')
     >      s_point(6),s_point(5)
         endif
         write (*,'(2x,"Y",5x,"Z",19x,"|  |",14x,"|",2x,"|")')
         write (lulist,'(10x,"Y",5x,"Z",19x,"|  |",14x,"|",2x,"|")')
         write (*,'(28x,"|  |",14x,"|",2x,"|")')
         write (lulist,'(36x,"|  |",14x,"|",2x,"|")')
         write (*,'(28x,"|  |",14x,"|",2x,"|")')
         write (lulist,'(36x,"|  |",14x,"|",2x,"|")')
         write (*,'(28x,"|  |      0.00",4x,"|",2x,"|")')
         write (lulist,'(36x,"|  |      0.00",4x,"|",2x,"|")')
         write (*,'(28x,"|  |",14x,"|",2x,"|")')
         write (lulist,'(36x,"|  |",14x,"|",2x,"|")')
         write (*,'(28x,"|  |",14x,"|",2x,"|")')
         write (lulist,'(36x,"|  |",14x,"|",2x,"|")')
c
c  test on value of rms of res.
c
         if (s_point(7).lt.1.0 .and. s_point(7).ge.0.0) then
            write (*,'(28x,"|",f5.2,"************","|","**",f3.2)')
     >      s_point(8),s_point(7)
            write (lulist,'(36x,"|",f5.2,"************","|","**",f3.2)')
     >      s_point(8),s_point(7)
         else
            write (*,'(28x,"|",f5.2,"************","|",f5.2)')
     >      s_point(8),s_point(7)
            write (lulist,'(36x,"|",f5.2,"************","|",f5.2)')
     >      s_point(8),s_point(7)
         endif
         write (*,'(28x,"|",1x,"/",15x,"|",1x,"/")')
         write (lulist,'(36x,"|",1x,"/",15x,"|",1x,"/")')
         write (*,'(28x,"|/",16x,"|/")')
         write (lulist,'(36x,"|/",16x,"|/")')
c
c  test on value of rms of res.
c
         if (s_point(9).lt.1.0 .and. s_point(9).ge.0.0) then
            write (*,'(26x,f5.2,"***************",f3.2)')
     >      s_point(10),s_point(9)
            write (lulist,'(34x,f5.2,"***************",f3.2)')
     >      s_point(10),s_point(9)
         else
            write (*,'(26x,f5.2,"*************",f5.2)')
     >      s_point(10),s_point(9)
            write (lulist,'(34x,f5.2,"*************",f5.2)')
     >      s_point(10),s_point(9)
         endif
         write (*,'(39x,"|")')
         write (lulist,'(47x,"|")')
         write (*,'(37x,f5.2)') s_point(2)
         write (lulist,'(45x,f5.2)') s_point(2)
         write (lulist,
     >   '(8x,"Radius of the sphere:",f4.2," km")')
     >   sphere_radius
c
c  set flags for end of rms_on_sphere mode
c
         endit=.true.
c
c  reset flag for t0 computing by minim. procedure
c
         t0_norm=.false.
c
c  set the value of coord. of hypocenter
c  for the reason of locfile writting
c
         x0=x_orig
         y0=y_orig
         z0=z_orig
         t0=t_orig
c 2018-07-09 pz -----------------------------
         rmsres=rms_orig*rms_orig
         i0=no_iter_orig
c -------------------------------------------
20       continue
c
         return
      end subroutine sphere_step
