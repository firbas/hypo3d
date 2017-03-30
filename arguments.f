c
		subroutine decod_run_string
c
c*****************************************************************************
c
c  subroutine DECOD_RUN_STRING
c
c-----------------------------------------------------------------------------
c
c  purpose:
c
c     decode runstring
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
c     call decod_run_string
c
c----------------------------------------------------------------------------
c
c  external references:
c
c     RUN_STRING_PAR      pf subroutine
c     ABORT               mw subroutine
c
c----------------------------------------------------------------------------
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
c
c  global parameters
c
		include 'pname.fi'
		include 'term.fi'
c
c  local variables
c
		integer      string_length
		integer      int_number
		integer      i
		integer      ios
		integer      n_of_parameters
		real         temp
		character*80 string
		character*20 ch_error
c
c  global variables
c
      include 'error.fi'
      character*64 ch_model_name
      common /chmodn/ ch_model_name
c
      character*10    subdir
      character*1     letter_hp
      character*3     ch_fevent_number
      character*3     ch_levent_number
      character*1     interactive
      character*1     chfix_depth
      character*16    chfix_value
      character*6     sname
      common /hnamch/ subdir,letter_hp,ch_fevent_number,
     >                ch_levent_number,interactive,chfix_depth,
     >                chfix_value,sname
c
      integer         subdir_length
      common /hnami/  subdir_length
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
c  initialization of model_error variable
c
      model_error=-1.0
c
c  for the first: get runstring
c
      call run_string_par(0,string,string_length,n_of_parameters)
c
c  for the second: decode parameters
c
      do i=1,n_of_parameters
          call run_string_par(i,string,string_length,n_of_parameters)
c
          if (string(1:2).eq.'-?' .or. string(1:2).eq.'-X') then
c
c  call runstring parameters information
c
              call Info
          else if (string(1:2).eq.'-D') then
c
c  name of subdirectory
c
              if (string_length.gt.8) then
c
c  error
c
                  write (*,
     >            '(1x,a,": Error in runstring parameter - ",
     >            "subdir length greater then 6.")')  prog_name
c
                  subdir_length=0
                  subdir=' '
              else if (string_length.gt.2) then
                  subdir(1:string_length-2)=string(3:string_length)
                  subdir_length=string_length-2
              else
                  subdir_length=0
              endif
c
          else if (string(1:2).eq.'-S') then
c
c  name of subdirectory
c
              if (string_length.gt.8) then
c
c  error
c
                  write (*,
     >            '(1x,a,": Error in runstring parameter - ",
     >            "name of start file greater then 6.")')  prog_name
c
c  program ended
c
                  call exit
              else if (string_length.gt.2) then
                  sname(1:string_length-2)=string(3:string_length)
              else
c
c  error
c
                  write (*,
     >            '(1x,a,": Error in runstring parameter - ",
     >            "no name of start file.")')  prog_name
c
                  call exit
c
              endif
c
          else if (string(1:2).eq.'-T') then
c
c  character of hp tape
c
              if (string_length.gt.3) then
c
c  error
c
                  write (*,
     >            '(1x,a,": Error in runstring parameter - ",
     >            "wrong enter of tape letter.")')  prog_name
c
                  letter_hp=' '
              else if (string_length.gt.2) then
                  letter_hp=string(3:3)
cc                  call CaseFold(letter_hp)
	if(letter_hp.eq.'a')letter_hp='A'
	if(letter_hp.eq.'z')letter_hp='Z'
c
c  test on valid value of hp letter
c
                  if (letter_hp.lt.'A' .or. letter_hp.gt.'Z') then
c
c  error
c
                      write (*,
     >                '(1x,a,": Error in runstring parameter - ",
     >                "wrong enter of tape letter.")')  prog_name
c
                      letter_hp=' '
                  endif
              else
                  letter_hp=' '
              endif
c
          else if (string(1:2).eq.'-F') then
c
c  no. of first hypofile to location
c
              if (string_length.gt.2) then
                  if (string_length.gt.5) then
c
c  error
c
                      write (*,
     >                '(1x,a,": Error in runstring ",
     >                "parameter - wrong enter",
     >                "of first event number.")')
     >                prog_name
c
                      ch_fevent_number=' '
                  else if (string_length.eq.3) then
                      ch_fevent_number='00'//string(3:string_length)
                  else if (string_length.eq.4) then
                      ch_fevent_number='0'//string(3:string_length)
                  else
                      ch_fevent_number=string(3:string_length)
                  endif
c
                  if (ch_fevent_number.ne.' ') then
c
c  test on valid value of last event number
c
                      read (ch_fevent_number,'(i3)',iostat=ios)
     >                int_number
c
                      if (ios.ne.0 .or. int_number.le.0 .or. int_number
     >                .gt.999) then
                          write (*,
     >                    '(1x,a,": Error in runstring ",
     >                    "parameter - wrong ",
     >                    "enter of first event number.")')
     >                    prog_name
                          ch_fevent_number=' '
                      endif
                  endif
              else
                  ch_fevent_number=' '
              endif
c
          else if (string(1:2).eq.'-L') then
c
c  no. of last hypofile to location
c
              if (string_length.gt.2) then
                  if (string_length.gt.5) then
c
c  error
c
                      write (*,
     >                '(1x,a,": Error in runstring ",
     >                "parameter - wrong ",
     >                "enter of last event number.")')
     >                prog_name
c
                      ch_levent_number=' '
                  else if (string_length.eq.3) then
                      ch_levent_number='00'//string(3:string_length)
                  else if (string_length.eq.4) then
                      ch_levent_number='0'//string(3:string_length)
                  else
                      ch_levent_number=string(3:string_length)
                  endif
c
                  if (ch_levent_number.ne.' ') then
c
c  test on valid value of last event number
c
                      read (ch_levent_number,'(i3)',iostat=ios)
     >                int_number
c
                      if (ios.ne.0 .or. int_number.le.0 .or. int_number
     >                .gt.999) then
                          write (*,
     >                    '(1x,a,": Error in runstring ",
     >                    "parameter - wrong ",
     >                    "enter of last event number.")')
     >                    prog_name
                          ch_levent_number=' '
                      endif
                  endif
              else
                  ch_levent_number=' '
              endif
c
          else if (string(1:2).eq.'-N') then
c
c  set flag for nonitaractive mode
c
              if (string_length.gt.2) then
c
c  error
c
                  write (*,
     >            '(1x,a,": Error in runstring ",
     >            "parameter - wrong enter",
     >            " of interactive choice.")')
     >            prog_name
c
c  program ended
c
                  call exit
              else
                  interactive='N'
              endif
c
          else if (string(1:2).eq.'-Z') then
c
c  fixed depth
c
              if (string_length.eq.2) then
c
c  error
c
                  write (*,
     >            '(1x,a,": Error in runstring ",
     >            "parameter - wrong enter",
     >            " of fixed depth.")')
     >            prog_name
c
c  program ended
c
                  call exit
              else
                  chfix_depth='Z'
                  chfix_value=string(3:string_length)
                  read (chfix_value,*,iostat=ios) temp
c
                  if (ios.ne.0) then
c
c  error
c
                      write (*,
     >                '(1x,a,": Error in runstring ",
     >                "parameter - wrong enter",
     >                " of fixed depth.")')
     >                prog_name
                  endif
              endif
c
          else if (string(1:2).eq.'-E') then
c
c  error of model in miliseconds
c
              if (string_length.eq.2) then
c
c  error
c
                  write (*,
     >            '(1x,a,": Error in runstring ",
     >            "parameter - wrong enter",
     >            " of model error.")')
     >            prog_name
c
c  program ended
c
                  call exit
              else
                  ch_error=string(3:string_length)
                  read (ch_error,*,iostat=ios) model_error
c
                  if (ios.ne.0 .or. model_error.lt.0.0) then
c
c  error
c
                      write (*,
     >                '(1x,a,": Error in runstring ",
     >                "parameter - wrong enter",
     >                " of model error.")')
     >                prog_name
                  endif
              endif
c
          else if (string(1:2).eq.'-M') then
c
c  name of model file
c
              if (string_length.gt.18) then
c
c  error
c
                  write (*,
     >            '(1x,a,": Error in runstring parameter - ",
     >            "model name length greater then 16.")') prog_name
c
c  program ended
c
                  call Abort
              else if (string_length.gt.2) then
                  ch_model_name(1:string_length-2)=
     >                                     string(3:string_length)
              endif
          else
              write (*,
     >        '(1x,a,": Error - wrong parameter ",a)')
     >         prog_name,string(1:2)
          endif
      end do  !of decoding of parameters
c
      if (ch_levent_number.eq.' ') then
c
c  default ... last event = first event
c
          ch_levent_number=ch_fevent_number
      endif
c
c  initialize minimal reading error ... two samples (2 * 8 ms)
c
      reading_error=0.016
c
      return
      end
