c
		subroutine create_dbfile (namr)
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
c     call create_dbfile (namr)
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
	namrdb=namr
 	write(*,*) 'CREATE:  ', namrdb    
	 open (ludb,file=namrdb,status='UNKNOWN',iostat=ios)
c
c
      call o_hy3(ludb)
c
c  close the data base file
c
      close (ludb,status='KEEP')
c
      return
      end
