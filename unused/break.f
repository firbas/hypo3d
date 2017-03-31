c
		subroutine break (rms_on_net,n_iter,next_menu,endit)
c
c*****************************************************************************
c
c  subroutine BREAK
c
c-----------------------------------------------------------------------------
c
c  purpose:
c
c     show menu for break situation in different modes of location
c
c-----------------------------------------------------------------------------
c
c  formal parameters:
c
c     logical     RMS_ON_NET ... flag for rms_on_net mode               I
c     integer     N_ITER     ... no. of iteration                       I
c     logical     NEXT_MENU  ... flag for situation: stop computing,    O
c                                show next menu
c     logical     ENDIT      ... flag for situation: show results       O
c
c----------------------------------------------------------------------------
c
c  calling convention:
c
c     call break (rms_on_net,n_iter,next_menu,endit)
c
c----------------------------------------------------------------------------
c
c  external references:
c
c     CASEFOLD            RL subroutine
c     LOGLU               RL function
c
c----------------------------------------------------------------------------
c
c  programmed:  87-06  01.00  mw
c
c*****************************************************************************
c
c  ************
c  declarations
c  ************
c
		implicit none
c
c  formal parameters
c
		integer     n_iter
		logical     rms_on_net
		logical     next_menu
		logical     endit
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
cc		integer     lusys
		character*1 answer
c
c  global variables
c
cc		character*10    subdir              !name of subdirectory
cc		character*1     letter_hp           !letter for hp tape
cc		character*3     ch_fevent_number    !first event to localize
cc		character*3     ch_levent_number    !last event to localize
cc      character*1     interactive         !flag for interactive mode
cc      character*1     chfix_depth         !flag for fixed depth
cc      character*16    chfix_value         !value of fixed depth
cc      character*6     sname               !name of file with start coord.
c
c  functions
c
cc      integer LogLu
c
c  *******************
c  end of declarations
c  *******************
c
c=============================================================================
c
c
c  init. variable
c
      next_menu=.false.
c
c  test on interactive mode
c
cc      if (interactive.eq.'N') then
c
c  set lu of terminal
c
cc          luterm=Loglu(lusys)
cc          write (*,'(/,1x,a,": Operator break in ",i2,
cc     >    ". iteration.",/)')prog_name,n_iter
c
c  program ended
c
cc         call Abort
cc      endif
c
c  write break mode header
c
      if (rms_on_net) then
          write (*,'(/,1x,a,": Operator break.",/)') prog_name
      else
          write (*,'(/,1x,a,": Operator break in ",i2,
     >    ". iteration.",/)')prog_name,n_iter
      endif
c
10    continue
c
c  show menu
c
      if (rms_on_net) then
c
c  write menu for rms_on_net mode
c
          write (*,'(1x,a,": M e n u")') prog_name
          write (*,'(1x,a,
     >    ": Continue                                    ",
     >    "  -  ''C'',",/,7x,
     >    "  End of rms of res. on net mode, next menu   ",
     >    "  -  ''N''  [C]:_")') prog_name
      else
c
c  write menu
c
          write (*,'(1x,a,": M e n u")') prog_name
          write (*,'(1x
     >    ,a,": Continue iteration process",12x,
     >    "  -  ''C'',",/,
     >    7x,"  End of iteration process, show results",
     >    "  -  ''S'',",/,
     >    7x,"  End of iteration process, next menu   ",
     >    "  -  ''N''  [C]:_")') prog_name
      endif
c
      read (*,'(a)',end=10) answer
c
c  to big letters
c
cc      call CaseFold (answer)
	 if(answer.eq.'n')answer='N'
	 if(answer.eq.'s')answer='S'
	 if(answer.eq.'c')answer='C'
c
c  test on answer
c
      if (answer.eq.'N') then
c
c  set flag ... show next menu
c
          next_menu=.true.
      else if (answer.eq.'S' .and. .not.rms_on_net) then
c
c  set flag ... show results
c
          endit=.true.
      else if (answer.ne.'C' .and. answer.ne.' ') then
c
c  wrong answer
c
          go to 10
      endif
c
      return
      end
