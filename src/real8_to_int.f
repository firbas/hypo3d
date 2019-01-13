c
		subroutine real8_to_int(real8,int1,int2,int3,int4,int5)
c
c*****************************************************************************
c
c  subroutine REAL8_TO_INT
c
c-----------------------------------------------------------------------------
c
c  purpose:
c
c     transfer real8 datum to integer representation
c
c-----------------------------------------------------------------------------
c
c  formal parameters:
c
c     real*8      REAL8       ...  datum                            I
c     integer     int1        ...  year                             O
c     integer     int2        ...  month                            O
c     integer     int3        ...  day                              O
c     integer     int4        ...  hour                             O
c     integer     int5        ...  minute                           O
c
c----------------------------------------------------------------------------
c
c  calling convention:
c
c     call real8_to_int(real8,int1,int2,int3,int4,int5)
c
c----------------------------------------------------------------------------
c
c  external references:
c
c     none
c
c----------------------------------------------------------------------------
c
c  programmed:  86-10  01.00  mw
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
		integer int1
		integer int2
		integer int3
		integer int4
		integer int5
		real*8  real8
c
c  local parameters  ...  none
c
c
c  global parameters  ...  none
c
c
c  local variables  ...  none
c
c
c  global variables ...  none
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
c
c  first two nonzero numbers ... year
c
		int1=int(real8/1.d08)
c
c  number 3 to 4 ... month
c
		int2=int((real8-int1*1.d08)/1.d06)
c
c  number 5 to 6 ... day
c
		int3=int((real8-int1*1.d08-int2*1.d06)/1.d04)
c
c  number 7 to 8 ... hour
c
		int4=int((real8-int1*1.d08-int2*1.d06-int3*1.d04)/1.d02)
c
c  number 9 to 10 ... minute
c
		int5=int(real8-int1*1.d08-int2*1.d06-int3*1.d04-int4*1.d02)
c
		return
		end
