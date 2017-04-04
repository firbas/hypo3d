c
		SUBROUTINE ABORT
c
c-----------------------------------------------------------------------------
c
c  purpose:
c
c     treat end of program situation
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
c     call abort
c
c----------------------------------------------------------------------------
c
c  external references:
c
c     SEG_LOAD            mw subroutine
c     OPCLOSE             mw subroutine
c     PRTN                RL subroutine
c
c-----------------------------------------------------------------------------
c
c  programmed:  87-05  01.00  mw
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
c  global parameters
c
		include 'list.fi'
		include 'pname.fi'
c
c  local variables
c
cc		integer ierr
		integer ipram(5)                    !param. returned to father program
c
c  global variables
c
		logical         loc
		common /wloc/   loc
c
		logical         err_stname
		logical         w_changes
		common /ernam/  err_stname,w_changes
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
c  test on loc_file writting
c
		if (loc) then
c
c  loc-file was written
c
			 ipram(5)=1
		else
			 ipram(5)=0
		endif
c
		if (w_changes) then
			 ipram(5)=ipram(5)+10
		endif
c
c  write good bye message
c
		write (*,'(1x,a,": Program ended.")') prog_name
c
c  end of program
c
      stop
c
      end
