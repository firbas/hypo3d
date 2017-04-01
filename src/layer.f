c
		subroutine layer(xst,yst,zst)
c
c*****************************************************************************
c
c  subroutine LAYER
c
c-----------------------------------------------------------------------------
c
c  purpose:
c
c     initialize layer system for point (station) of coordinates xst,yst,zst
c
c-----------------------------------------------------------------------------
c
c  formal parameters:
c
c     real        XST         ...  x-coordinate of point            I
c     real        YST         ...  y-coordinate of point            I
c     real        ZST         ...  z-coordinate of point            I
c
c----------------------------------------------------------------------------
c
c  calling convention:
c
c     call layer (xst,yst,zst)
c
c----------------------------------------------------------------------------
c
c  external references:
c
c     none
c
c----------------------------------------------------------------------------
c
c  programmed:  86-09  01.00  mw
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
		real xst
		real yst
		real zst
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
		integer i                           !cycle variable
c
c  global variables
c
		real            v  (z_layer)
		real            vsq(z_layer)
		common /vlc/    v,vsq
c
		real            xl(x_layer-1)
		real            yl(y_layer-1)
		real            zl(z_layer-1)
		integer         nxl
		integer         nyl
		integer         nzl
		common /layers/ nxl,xl,nyl,yl,nzl,zl
c
		real            d  (z_layer)
		real            thk(z_layer)
		integer         nl
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
c  for the first: search for layer concerning the station
c
		do i = nzl-1,1,-1
c
c  test: does coord. of point (station) greater then interface no. i?
c   z-axis is downward
c
			 if (zst .gt. zl(i)) then
			     go to 10
			 endif
		end do
c
10    continue
c
c  number of d-layers
c
		nl = i+1
c
c  z-axis in d system is reversed (upward)
c
		d(1) = -zst
c
c  set value of d-interfaces
c
		do i = 2,nl
			 d(i) = -zl(nl+1-i)
		end do
c
c  set value for elements of thickness array
c
		do i = 1,nl-1
			 thk(i) = d(i+1) - d(i)
		end do
c
		return
		end
