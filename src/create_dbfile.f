c
      subroutine create_dbfile (namr)
c
c     creates and fills data base file
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
         integer      ios                     !error variable
c
c=============================================================================
c
         write(*,*) 'CREATE:  ', namr
c
         open (ludb,file=namr,status='UNKNOWN',iostat=ios)
         call o_hy3(ludb)
         close (ludb,status='KEEP')
c
         return
      end subroutine create_dbfile
