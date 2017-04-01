c
		subroutine create_dbfile (n_loc,namr)
c
c*****************************************************************************
c
c  subroutine CREATE_DBFILE
c
c-----------------------------------------------------------------------------
c
c  purpose:
c
c     creates and fills data base file
c
c-----------------------------------------------------------------------------
c
c  formal parameters:
c
c     integer N_LOC
c
c----------------------------------------------------------------------------
c
c  calling convention:
c
c     call create_dbfile (n_loc,namr)
c
c----------------------------------------------------------------------------
c
c  external references:
c
c     REPORTERROR         mw subroutine
c     ABORT               mw subroutine
c     OUTPUT              mw subroutine
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
c  formal parameters
c
		integer    n_loc
		character*255 namr
c
c  local parameters
c
		integer      ludb
		parameter (ludb=15)
c
c  global parameters
c
		include 'pname.fi'
c		include 'data_dir.fi'
c
c  local variables
c
cc		integer      number                 !# of event
cc		character*3  ch_event_number        !char. repr. # of event
		integer      ios,lnblnk             !error variable
		character*5  suffix                 !suffix for name of database file
		character*255 NamrDb                 !namr of data base file
c	        character*30 hy3fn
c
c  global variables
c
cc		character*10    subdir              !name of subdirectory
cc      character*1     letter_hp           !letter for hp tape
cc      character*3     ch_fevent_number    !first event to localize
cc      character*3     ch_levent_number    !last event to localize
cc      character*1     interactive         !flag for interactive mode
cc      character*1     chfix_depth         !flag for fixed depth
cc      character*16    chfix_value         !value of fixed depth
cc      character*6     sname               !name of file with start coord.
cc      common /hnamch/ subdir,letter_hp,ch_fevent_number,
cc     >                ch_levent_number,interactive,chfix_depth,
cc     >                chfix_value,sname
c
      integer             subdir_length       !length of subdir. name
		logical             source_flag
		common /hnami/      subdir_length,source_flag
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
c  code database file name
c
      if (prog_name.eq.'HYPO3D') then
          suffix='.hy3'
      else
          suffix='.hy1'
      endif
c
c  evaluate ch_event_number
c
cc      read (ch_fevent_number,*) number
cc      number=number+n_loc-1
cc      write (ch_event_number,'(i3.3)') number
c
cc     NamrDb=lu21_data//subdir(1:subdir_length)//'/'//
cc     >letter_hp//ch_event_number//suffix
c
c	call setfn('hy3file',7,hy3fn)
c	namrdb=namr(1:(lnblnk(namr)-4))//suffix
	namrdb=namr
 	write(*,*) 'CREATE:  ', namrdb    
	 open (ludb,file=namrdb,status='UNKNOWN',iostat=ios)
c
cc      if (ios.ne.0) then
cc          call ReportError (ios,NamrDb,'CRDB')
cc          call Abort
cc      endif
c
c  call output
c
      call output(ludb,0)
c
c  close the data base file
c
      close (ludb,status='KEEP')
c
      return
      end
