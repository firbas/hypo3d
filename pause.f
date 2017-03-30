c
		subroutine Pause
c
c*****************************************************************************
c
c  subroutine PAUSE
c
c-----------------------------------------------------------------------------
c
c  purpose:
c
c     wait on <return>
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
c     call Pause
c
c----------------------------------------------------------------------------
c
c  external references:
c
c     REAL8_TO_INT        mw subroutine
c
c----------------------------------------------------------------------------
c
c  programmed:  87-12  01.00  mw
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
c  formal parameters  ...  none
c
c
c  local parameters  ...  none
c
c
c  global parameters
c
		include 'pname.fi'
		include 'term.fi'
c
c  local variables
c
		character*01 answer
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
		write (*,'(1x,a,": Hit <RETURN> to continue._")')
     >prog_name
c
20    continue
c
		read (*,'(a)',end=20) answer
c
		if (answer.ne.' ') then
			 go to 20
		endif
c
      return
      end
