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
c     integer LU
c
c----------------------------------------------------------------------------
c
c  calling convention:
c
c     call create_dbfile (namr)
c
c----------------------------------------------------------------------------
c
c  programmed:  87-05  01.00  mw
c
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
c
		integer      ios                    !error variable
		character*255 NamrDb                 !namr of data base file
c
c=============================================================================
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
