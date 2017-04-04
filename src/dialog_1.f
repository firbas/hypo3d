c
		subroutine dialog_1(dmin8)
c
c*****************************************************************************
c
c  subroutine DIALOG_1
c
c-----------------------------------------------------------------------------
c
c  purpose:
c
c     header lines for scanned depth mode
c
c-----------------------------------------------------------------------------
c
c  formal parameters:
c
c     real*8      DMIN8       ...  minimum time of datum8           I
c
c----------------------------------------------------------------------------
c
c  calling convention:
c
c     call dialog_1 (dmin8)
c
c----------------------------------------------------------------------------
c
c  external references:
c
c     REAL8_TO_INT        mw subroutine
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
		real*8       dmin8
c
c  local parameters  ...  none
c
c
c  global parameters
c
		character*255         ch_model_name   !name of crustal model for use
		common /chmodn/     ch_model_name
c
		include 'list.fi'
		include 'pname.fi'
c
c  local variables
c
		character*255 hyponamr
		integer      year
		integer      month
		integer      day
		integer      hour
      integer      minute
c
c  global variables
c
      real            x_start
      real            y_start
      real            z_start
      real            ot_start
      common /start/  x_start,y_start,z_start,ot_start
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
c  for scanned depth
c
      write (*,'(1x,a,": Scanned depth mode:")') prog_name
      write (*,'(
     >"------------------------------------------------------------",/,
     >" # iter.    x0        y0        z0        t0        rmsres  ",/,
     >"------------------------------------------------------------")')
      write (lulist,'(1x,a,"   location ... scanned depth ",
     >"mode."/)') long_prog_name
c
c  decode datum
c
      call real8_to_int(dmin8,year,month,day,hour,minute)
c
c  write header
c
      write (lulist,'(" Name of used model        :",a)') ch_model_name
      write (lulist,'(" Name of hypofile          :",a)') hypoNamr
      write (lulist,'(" Minimal recorded arr. time:",
     >2(i2.2,"-"),i2.2,1x,i2.2,":",i2.2)') year,month,day,hour,minute
      write (lulist,'(" Starting point (x,y,t)    :(",f8.2,",",f8.2,",",
     >f8.2,")",/)') x_start,y_start,ot_start
      write (lulist,'(
     >" ------------------------------------------------------------",/,
     >"  # iter.    x0        y0        z0        t0        rmsres  ",/,
     >" ------------------------------------------------------------")')
c
c  return to main
c
      return
      end
