c
		subroutine velocity (x1,y1,x2,y2)
c
c*****************************************************************************
c
c  subroutine VELOCITY
c
c-----------------------------------------------------------------------------
c
c  purpose:
c
c     compute weighted average of velocity in given layers
c
c     z-section of model:
c
c                           layer nzl
c               ------------------------------   zl(nzl-1)
c                           layer nzl-1
c               ------------------------------   zl(nzl-2)
c           ^                 .
c          / \                .
c           |                 .
c           |   ------------------------------   zl(2)
c           |               layer 2
c           |   ------------------------------   zl(1)
c           |               layer 1
c         z axis
c
c-----------------------------------------------------------------------------
c
c  formal parameters:
c
c     real        X1          ...  x-coordinate of the first point    I
c     real        X2          ...  x-coordinate of the second point   I
c     real        Y1          ...  y-coordinate of the first point    I
c     real        Y2          ...  y-coordinate of the second point   I
c
c----------------------------------------------------------------------------
c
c  calling convention:
c
c     call velocity (x1,x2,y1,y2)
c
c----------------------------------------------------------------------------
c
c  external references:
c
c     VL                  mw subroutine
c     ABORT               mw subroutine
c
c----------------------------------------------------------------------------
c
c  programmed:  86-09  01.00  mw  box-car version
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
		real x1
		real y1
		real x2
		real y2
c
c  local parameters  ...  none
c
c
c  global parameters
c
		include 'param.fi'
c
c  local variables
c
		integer    i
		integer    j
		integer    nltemp
		real       voltemp
		real       vtemp
		real       s      (z_layer)
		real       svolume(z_layer)
		real       dtemp  (z_layer)
c
c  global variables
c
		real            v  (z_layer)
		real            vsq(z_layer)
		common /vlc/    v,vsq
c
		integer         nl
		real            d  (z_layer)
      real            thk(z_layer)
      common /zlayer/ d,nl,thk
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
c  store original d-layer system
c
c  1) no. of layers
c
      nltemp = nl
c
c  2) top of layers
c
      do  i = 1,nl
          dtemp(i) = d(i)
      end do
c
c  average velocities
c
      do i = 1,nl
c
c  test on equivalent points
c
          if (x1.eq.x2 .and. y1.eq.y2) then
c
c  average velocity in layer i
c
              call Vl (x1,y1,x2+0.001,y2+0.001,i,s(i),svolume(i))
          else
c
c  average velocity in layer i
c
              call Vl (x1,y1,x2,y2,i,s(i),svolume(i))
          endif
c
c  test on air
c
          if (svolume(i).eq.0.0) then
c
c  layer is in air
c
              v(i)=vair
          else
              v(i) = svolume(i)/s(i)
          endif
c
c  test on reasonable value of velocity in layer
c
          if (v(i).lt.0.) then
              write (*,*)
     >        'HYPO3D: Error - velocity in layer i < 0.'
              call EXIT(1)
          endif
c
c  set array of squared velocities
c
          vsq(i) = v(i) **2
      end do
c
c  in this revision of program HYPO should be velocities in d-layers in
c  increasing value  [ v(i) > v(i+1) ]
c
c  so check increasing order
c
5     continue
      do i=1,nl-1
D     WRITE (*,*) 'V(I)-V(I+1)=',V(I)-V(I+1)
          if ((v(i)-v(i+1)).ge.-0.001) then
              go to 10
          endif
      end do
c
c  all velocities O.K.
c
      go to 100
c
c  fuse layer i and i+1
c
10    continue
      voltemp = 0.
      vtemp = 0.
c
c  cycle over (maybe fused) layers
c
      do j=1,nltemp
          if (dtemp(j).ge.d(i) .and.
     >        dtemp(j).le.d(i+1) ) then
              vtemp = vtemp + s(j)
              voltemp = voltemp + svolume(j)
          endif
      end do
c
      vtemp = voltemp / vtemp
c
      v(i) = vtemp
      vsq(i) = vtemp**2
c
      if (i+1.eq.nl) then
c
c  fusing with last layer
c
          nl = nl - 1
          go to 5
      endif
c
c  set the new value of thickness of layer
c
      thk(i) = thk(i) + thk(i+1)
c
      do j=i+1,nl-1
          d(j) = d(j+1)
          v(j) = v(j+1)
          vsq(j) = vsq(j+1)
          if (j.lt.nl-1) then
              thk(j) = thk(j+1)
          endif
      end do
c
      nl = nl - 1
      go to 5
c
100   continue
      return
      end
