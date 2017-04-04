c
		subroutine dialog_2
     >(endit,scan_depth,rms_on_net,rms_on_sphere)
c
c*****************************************************************************
c
c  subroutine DIALOG_2
c
c-----------------------------------------------------------------------------
c
c  purpose:
c
c     show menu with rms_on_sphere, resp. reset flag for rms_on_sphere mode
c
c-----------------------------------------------------------------------------
c
c  formal parameters:
c
c     logical     ENDIT         ...  end of iter. process             I
c     logical     SCAN_DEPTH    ...  scanned depth mode               I
c     logical     RMS_ON_NET    ...  rms_on_net mode                  I
c     logical     RMS_ON_SPHERE ...  rms_on_sphere mode             I/O
c
c----------------------------------------------------------------------------
c
c  calling convention:
c
c     subroutine dialog_2 (endit,scan_depth,rms_on_net,rms_on_sphere)
c
c----------------------------------------------------------------------------
c
c  external references:
c
c     CLEAR_DISPLAY       mw subroutine
c     ABORT               mw subroutine
c     CASEFOLD            RL subroutine
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
		logical scan_depth
		logical rms_on_net
		logical rms_on_sphere
c
c  local parameters  ...  none
c
c
c  global parameters
c
		include 'pname.fi'
c
c  local variables
c
		character*1 answer
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
c
c  show menu only in this situation:
c   1) end of iteration process and
c   2) rms of res. on net not selected and
c   3) scan depth mode not selected
c   4) in this location rms_on_sphere not yet selected
c
		if (rms_on_sphere) then
c
c  rms_on_sphere was selected
c  reset flag, skip this menu
c
          rms_on_sphere=.false.
          go to 20
      endif
  
      if (.not.endit .or. rms_on_net .or. scan_depth) then
c
c  conditions didn't satisfied
c  skip this menu
c
          go to 20
      endif
  
10    continue
  
      write (*,'(1x,a,": M e n u")') prog_name
      write (*,'(1x,a,
     >       ": Rms of res. on a sphere    -     ''R''  "/
     >"         End of program             -     ''E''  ",/
     >"         Next menu                  -     ''N'' [N]:_")')
     >prog_name
c
c  read answer
c
      read (*,'(a)',end=10) answer
  
	 if (answer.eq.'r')answer='R'
	 if (answer.eq.'e')answer='E'
	 if (answer.eq.'n')answer='N'
c  
      if (answer.eq.'R') then
c
c  rms of res. on sphere
c
          rms_on_sphere=.true.
      else if (answer.eq.'E') then
c
c  end of program
c
          call EXIT(0)
      else if (answer.eq.'N' .or. answer.eq.' ') then
c
c  next menu
c
          rms_on_sphere=.false.
      else
c
c  wrong answer
c
          go to 10
      endif
  
20    continue
 
      return
      end
