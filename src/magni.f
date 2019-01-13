c
		subroutine magni
c
c*****************************************************************************
c
c  subroutine MAGNI
c
c-----------------------------------------------------------------------------
c
c  purpose:
c
c     magnitude estimations
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
c     call magni
c
c----------------------------------------------------------------------------
c
c  external references:
c
c     none
c
c----------------------------------------------------------------------------
c
c  programmed:  86-11  01.00  mw original version
c               87-05  01.01  mw epi-distance over hypo-distance
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
c  formal parameters ...  none
c
c
c  local parameters  ...  none
c
		real       min_distance
		parameter (min_distance = 5.0)      !if hypocentral distance is less
														!then min_distance then magnitude
														!will be not evaluated
c
c  global parameters
c
      include 'param.fi'
c
c  local variables
c
		integer nm
		integer i
		real    delta
c
c  global variables
c
		integer         key(nrec_max)   !key field
		common /stmod/  key             !models for stations
c
		integer         nrec
		real            xstat(nStation)
		real            ystat(nStation)
		real            zstat(nStation)
		real            dly(nStation)   !stations delays for surf. events
		common /rec/    nrec,xstat,ystat,zstat,dly
c
		character*1     type(nrec_max)
      common /chrec/  type
c
      real            x0
      real            y0
      real            z0
      common /centr/  x0,y0,z0
c
      real            amp(nrec_max)
      real            freq(nrec_max)
      common /ampli/  amp,freq
c
      real            sdm
      real            avm
      real            xmag(nrec_max)
      common /mag/    xmag,avm,sdm
c
      character*5     rec_name(nrec_max)
      common /chhyp/  rec_name
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
c  init. variables
c
      nm=0
      avm=0.
      sdm=0.
c
c  cycle over recorded arrivals
c
      do i=1,nrec
c
c computing xmag according formula outlined in lennartz electronics
c sas-58000 user's guide and reference manual
c
          if (amp(i).eq.0.) then
c
c  evaluating only for amplitude greater then 0.0
c
              xmag(i)=-9.9
              go to 40
          endif
c

c          if (type(i).eq.'P' .and. i.lt.nrec) then
cc
cc  evaluating for S-arr., if only P-arrival then eval. for P-arr.
cc
c              if (rec_name(i).eq.rec_name(i+1) .and.
c     >           type(i+1).eq.'S' .and. amp(i+1).gt.0.0) then
c                  xmag(i)=-9.9
c                  go to 40
c              endif
c          endif

c
       xmag(i)=-9.9

       if (type(i).eq.'S' .and. amp(i)>0.0) then
c
c  distance hypocenter - record. station
c
          delta=sqrt( (xstat(key(i))-x0)**2+(ystat(key(i))-y0)**2
     >          +(zstat(key(i))-z0)**2 )
c
c  test on hypocentral distance of station
c
          if (delta.lt.min_distance) then
              xmag(i)=-9.9
              go to 40
          endif
c
c  formula for magnitude estimation (W.-A. equivalent)
c
          xmag(i)=
     >    log10(abs(amp(i)/6.283/freq(i))*2.8*1e6/0.6325)+
     >    0.1+1.4*log10(delta)
c
          nm=nm+1
          avm=avm+xmag(i)
          sdm=sdm+xmag(i)**2
       
       endif



   40     continue
      end do
c
      if (nm .eq. 0) then
c
c  no magnitude estimation
c
          go to 50
      endif
c
c  evaluate average magnitude ... AVM, standard deviation of magn. ... SDM
c
      avm=avm/nm
c
c  for the reasons of machine inaccuracy
c
      if (sdm/nm-avm**2 .lt. 0.0) then
          sdm=0.0
      else
          sdm=sqrt(sdm/nm-avm**2)
      endif
      go to 60
  
50    continue
      avm=-9.9
c
60    continue
      return
      end
