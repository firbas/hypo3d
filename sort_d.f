c
		subroutine sort_d(d,x,y,z,m,n)
c
c*****************************************************************************
c
c  subroutine SORT_D
c
c-----------------------------------------------------------------------------
c
c  purpose:
c
c     sort elements of arrays X(),Y(),Z(),M() according to the size
c     of elements in D()
c
c-----------------------------------------------------------------------------
c
c  formal parameters:
c
c     real        D(1)        ...  sorted array                         I/O
c     real        X(1)        ...  sorted array (in accordance to D())  I/O
c     real        Y(1)        ...  sorted array (in accordance to D())  I/O
c     real        Z(1)        ...  sorted array (in accordance to D())  I/O
c     integer     M(1)        ...  sorted array (in accordance to D())  I/O
c     integer     N           ...  no. of elements in arrays             I
c
c----------------------------------------------------------------------------
c
c  calling convention:
c
c     call sort_d(d,x,y,z,m,n)
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
		real d(1)
		real x(1)
		real y(1)
		real z(1)
		integer m(1)
		integer n
c
c  local parameters  ...  none
c
c
c  global parameters  ...  none
c
c
c  local variables
c
		real amin
		real ap
		integer mp
		integer kk
		integer k
c
c  global variables  ...  none
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
		do kk=1,n
			 amin=d(kk)
c
			 do k=kk,n
			     if(d(k).lt.amin) amin=d(k)
			 end do
c
			 do k=kk,n
			     if(d(k).eq.amin) go to 3
			 end do
c
3         d(k)=d(kk)
			 d(kk)=amin
c
			 ap=x(k)
			 x(k)=x(kk)
			 x(kk)=ap
c
			 ap=y(k)
			 y(k)=y(kk)
			 y(kk)=ap
c
			 ap=z(k)
			 z(k)=z(kk)
			 z(kk)=ap
c
			 mp=m(k)
			 m(k)=m(kk)
			 m(kk)=mp
c
		end do
c
		return
		end
