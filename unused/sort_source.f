c
		subroutine sort_source
c
c*****************************************************************************
c
c  subroutine SORT_SOURCE
c
c-----------------------------------------------------------------------------
c
c  purpose:
c
c     sort known sources in increasing hypocentral distance
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
c     call sort_source
c
c----------------------------------------------------------------------------
c
c  external references:
c
c     DATUM               mw subroutine
c     REAL8_TO_INT        mw subroutine
c
c----------------------------------------------------------------------------
c
c  programmed:  87-07  01.00  mw
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
c  formal parameters ... none
c
c
c  local parameters  ...  none
c
c
c  global parameters
c
      include 'source.fi'
c
c  local variables
c
		integer     ko
		integer     jo
		integer     i
		integer     mo
		integer     kemp
		real        key(max_source)
		real        temp
		character*30 chtemp
c
c  global variables
c
      real         x(max_source)
      real         y(max_source)
      real         z(max_source)
      real         delta(max_source)
      integer      nsource
      character*30 source_name(max_source)
      common /ns1/ x,y,z,delta,nsource
      common /ns2/ source_name
c
c  functions
c
c
c  *******************
c  end of declarations
c  *******************
c
c=============================================================================
c
c
c  sort in increasing order
c
C-----------------------------------------------------------------------
      DO 1 I=1,nsource
  1   KEY(I)=I
      MO=nsource
 2    IF (MO-15) 21,21,23
 21   IF (MO-1) 29,29,22
 22   MO=2*(MO/4)+1
      GO TO 24
 23   MO=2*(MO/8)+1
 24   KO=nsource-MO
      JO=1
 25   I=JO
 26   IF (delta(I)-delta(I+MO)) 28,28,27
 27   TEMP=delta(I)
      delta(I)=delta(I+MO)
      delta(I+MO)=TEMP
c
      temp=x(i)
      x(i)=x(i+mo)
      x(i+mo)=temp
c
      temp=y(i)
      y(i)=y(i+mo)
      y(i+mo)=temp
c
      temp=z(i)
      z(i)=z(i+mo)
      z(i+mo)=temp
c
      chtemp=source_name(i)
      source_name(i)=source_name(i+mo)
      source_name(i+mo)=chtemp
c
      KEMP=KEY(I)
      KEY(I)=KEY(I+MO)
      KEY(I+MO)=KEMP
      I=I-MO
      IF (I-1) 28,26,26
 28   JO=JO+1
      IF (JO-KO) 25,25,2
 29   RETURN
      END
