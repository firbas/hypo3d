c
		subroutine trans (x1,x2,x3,type)
c
c*****************************************************************************
c
c  subroutine TRANS
c
c-----------------------------------------------------------------------------
c
c  purpose:
c
c     transformations of coordinates
c
c-----------------------------------------------------------------------------
c
c  formal parameters:
c
c     real     X1      ...  x-coordinate (old x new)         I/O
c     real     X2      ...  y-coordinate (old x new)         I/O
c     real     X3      ...  z-coordinate (old x new)         I/O
c     integer  TYPE    ...  type of transformation            I
c                           type=1   Krovak --> local
c                           type=0   local  --> Krovak
c                           type=2   local  --> Krovak (for surface computing)
c
c----------------------------------------------------------------------------
c
c  calling convention:
c
c     call trans (x1,x2,x3,type)
c
c
c  programmed:  87-02  01.00  mw  original version
c               87-07  01.01  mw  modified version
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
		integer          type
		real*4           x1
		real*4           x2
		real*4           x3
c
c  local parameters  ...  none
c
c
c  global parameters
c
      double precision p_fi, p_x_shift, p_y_shift
      common /p_posun/ p_fi, p_x_shift, p_y_shift
c		
		include 'pname.fi'
c
c  local variables
      double precision a,b,fi,x_stara,x_nova,y_stara,y_nova
      double precision x_shift, y_shift
c
c
c  global variables
c
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
	x_stara=x1
	y_stara=x2
	fi=p_fi/180.0*3.141592d00
	a=dsin(fi)
	b=dcos(fi)
	x_shift= p_x_shift
	y_shift= p_y_shift
c
	if (type.eq.1) then
c
c       K --> l
c
	   x_stara=x_stara-x_shift
	   y_stara=y_stara-y_shift
c       
	   y_nova=-x_stara*a + y_stara*b
	   x_nova= y_stara*a + x_stara*b
c       
	else
c
c       type=0, type=2       
c       l --> K
c       
	   y_nova= x_stara*a + y_stara*b + y_shift
	   x_nova=-y_stara*a + x_stara*b + x_shift
c
	endif
c
	x1=x_nova
	x2=y_nova
c	
	return
	end
