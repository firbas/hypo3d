C$ema /rec/,/hyp/
		subroutine valid_arrival
c
c*****************************************************************************
c
c  subroutine VALID_ARRIVAL
c
c-----------------------------------------------------------------------------
c
c  purpose:
c
c     search number of valid arrivals ... weight less then 4
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
c     call valid_arrival
c
c----------------------------------------------------------------------------
c
c  external references:
c
c     ABORT               mw subroutine
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
c  formal parameters
c
c
c  local parameters  ...  none
c
c
c  global parameters
c
		include 'pname.fi'
		include 'param.fi'
		include 'term.fi'
c
c  local variables
c
		integer n_valid_arrival
		integer i
c
c  global variables
c
		integer             nrec            !no. of arrivals
		real                xstat(nStation) !\
		real                ystat(nStation) ! >coordinates of stations
		real                zstat(nStation) !/
		real                dly(nStation)   !stations delays for surf. events
		common /rec/        nrec,xstat,ystat,zstat,dly
c
		real                trec(nrec_max)  !observed times
                real                wt(nrec_max)    !weight
		real                avwt            !average weight
		common /hyp/        trec,wt,avwt
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
c  init. variable
c
      n_valid_arrival=0
c
c  cycle for test on weight of arrival
c
      do i=1,nrec
          if (wt(i).gt.0.0) then
              n_valid_arrival=n_valid_arrival+1
          endif
      end do
c
c  test on reasonable value of n_valid_arrival
c
      if (n_valid_arrival .lt. 3) then
          write (*,
     >    '(1x,a,": Error - no. of valid arrivals",
     >    " < 3.")') prog_name
          call EXIT(1)
      endif
c
      return
      end
