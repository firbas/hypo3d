c
		subroutine show_matrix
c     program    crd_matrix
c
c*****************************************************************************
c
c  subroutine SHOW_MATRIX
c
c-----------------------------------------------------------------------------
c
c  purpose:
c
c     vizualize resolution and info-density matrix
c
c-----------------------------------------------------------------------------
c
c  formal parameters:
c
c     integer     N_INCREASE  ...  no. of increasing of t0          O
c     real*8      DMIN8       ...  minimum time of datum8           I
c
c----------------------------------------------------------------------------
c
c  calling convention:
c
c     call origin_time (n_increase,dmin8)
c
c----------------------------------------------------------------------------
c
c  external references:
c
c     DATUM               mw subroutine
c     REAL8_TO_INT        mw subroutine
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
		include 'list.fi'
		include 'term.fi'
c
c  local variables
c
		integer         n_matrix
		character*1     answer
		character*51    fmt(31)
		real            scale
		real            matrix(4,4)
		real            max
		integer         i,j
		integer         n_point
		integer         ivalue
c
c  global variables
c
		real            co(4,4)
		real            id(4,4)
		real            re(4,4)
		real            rmsres
		real            rmsres_co
		common /cov/    co,rmsres,rmsres_co,id,re
c
c  functions
c
c
c  *******************
c  end of declarations
c  *******************
c
c=============================================================================
c
		n_matrix=0
c
10    continue
c
		n_matrix=n_matrix+1
c
		if (n_matrix.eq.1) then
c
c 1) resolution matrix
c
15        continue
			 write (*,'(1x,a,": Show resolution and ",
     >    "info density matrix? (Y/N) [N]:_")')
     >    prog_name
c
			 read (*,'(a)',end=15) answer
c
cc			 call CaseFold (answer)
	 if(answer.eq.'y')answer='Y'
	 if(answer.eq.'n')answer='N'
c
			 if (answer.eq.'N' .or. answer.eq.' ') then
			     go to 999
			 else if (answer.ne.'Y') then
			     go to 15
			 endif
c
cc			 call Clear_Display
c
c  new page on the printer
c
			 write (lulist,'("1")')
c
c  write header
c
			 write (lulist,'(8x,"Resolution Matrix")')
			 write (*,'(8x,"Resolution Matrix")')
c
			 do i=1,4
			     do j=1,4
						matrix(4-i+1,j)=re(i,j)
			     end do
			 end do
		else if (n_matrix.eq.2) then
c
c  info density matrix
c
          write (*,'(1x,a,": Hit <RETURN> to continue.")')
     >    prog_name
c
20        continue
c
          read (*,'(a)',end=20) answer
c
          if (answer.ne.' ') then
              go to 20
          endif
c
cc          call Clear_Display
c
c  write header
c
          write (lulist,'(8x,"Information Density Matrix")')
          write (*,'(8x,"Information Density Matrix")')
c
          do i=1,4
              do j=1,4
                  matrix(4-i+1,j)=id(i,j)
              end do
          end do
      else
          go to 999
      endif
c
c  search max. abs.element of matrix
c
      do i=1,4
          do j=1,4
              if (i.eq.1 .and. j.eq.1) then
                  max=abs(matrix(i,j))
              else
                  if (max.lt.abs(matrix(i,j))) then
                      max=abs(matrix(i,j))
                  endif
              endif
          end do
      end do
c
      write (*,'(//,8x,"Maximum absolute value: ",e8.3)') max
c
c  search scale
c
      scale=9./max
c
c  initialize format array
c
      do i=1,9
          fmt(i)='8x,"                                              "'
      end do
c
      fmt(10)='8x,"            x -------- x -------- x -------- x"'
      fmt(11)='8x,"           /          /          /          / "'
      fmt(12)='8x,"          /          /          /          /  "'
      fmt(13)='8x,"         /          /          /          /   "'
      fmt(14)='8x,"        x -------- x -------- x -------- x    "'
      fmt(15)='8x,"       /          /          /          /     "'
      fmt(16)='8x,"      /          /          /          /      "'
      fmt(17)='8x,"     /          /          /          /       "'
      fmt(18)='8x,"    x -------- x -------- x -------- x        "'
      fmt(19)='8x,"   /          /          /          /         "'
      fmt(20)='8x,"  /          /          /          /          "'
      fmt(21)='8x," /          /          /          /           "'
      fmt(22)='8x,"x -------- x -------- x -------- x            "'
      fmt(23)='8x,"                                              "'
      fmt(24)='8x," X          Y          Z          T           "'
c
      do i=25,31
          fmt(i)='8x,"                                              "'
      end do
c
c  cycle over elements of matrix
c
      n_point=0
      do i=1,4
          do j=1,4
              n_point=n_point+1
              ivalue=matrix(i,j)*scale
c
              if (n_point.eq.1) then
                  if (ivalue.eq.0) then
                      fmt(10)(17:17) = 'u'
                  else if (ivalue.eq.  1) then
                      fmt( 9)(17:17) = 'u'
                  else if (ivalue.eq. -1) then
                      fmt(11)(17:17) = 'u'
                  else if (ivalue.eq.  2) then
                      fmt( 9)(17:17) = '|'
                      fmt( 8)(17:17) = 'u'
                  else if (ivalue.eq. -2) then
                      fmt(11)(17:17) = '|'
                      fmt(12)(17:17) = 'u'
                  else if (ivalue.eq.  3) then
                      fmt( 9)(17:17) = '|'
                      fmt( 8)(17:17) = '|'
                      fmt( 7)(17:17) = 'u'
                  else if (ivalue.eq. -3) then
                      fmt(11)(17:17) = '|'
                      fmt(12)(17:17) = '|'
                      fmt(13)(17:17) = 'u'
                  else if (ivalue.eq.  4) then
                      fmt( 9)(17:17) = '|'
                      fmt( 8)(17:17) = '|'
                      fmt( 7)(17:17) = '|'
                      fmt( 6)(17:17) = 'u'
                  else if (ivalue.eq. -4) then
                      fmt(11)(17:17) = '|'
                      fmt(12)(17:17) = '|'
                      fmt(13)(17:17) = '|'
                      fmt(14)(17:17) = 'u'
                  else if (ivalue.eq.  5) then
                      fmt( 9)(17:17) = '|'
                      fmt( 8)(17:17) = '|'
                      fmt( 7)(17:17) = '|'
                      fmt( 6)(17:17) = '|'
                      fmt( 5)(17:17) = 'u'
                  else if (ivalue.eq. -5) then
                      fmt(11)(17:17) = '|'
                      fmt(12)(17:17) = '|'
                      fmt(13)(17:17) = '|'
                      fmt(14)(17:17) = '|'
                      fmt(15)(17:17) = 'u'
                  else if (ivalue.eq.  6) then
                      fmt( 9)(17:17) = '|'
                      fmt( 8)(17:17) = '|'
                      fmt( 7)(17:17) = '|'
                      fmt( 6)(17:17) = '|'
                      fmt( 5)(17:17) = '|'
                      fmt( 4)(17:17) = 'u'
                  else if (ivalue.eq. -6) then
                      fmt(11)(17:17) = '|'
                      fmt(12)(17:17) = '|'
                      fmt(13)(17:17) = '|'
                      fmt(14)(17:17) = '|'
                      fmt(15)(17:17) = '|'
                      fmt(16)(17:17) = 'u'
                  else if (ivalue.eq.  7) then
                      fmt( 9)(17:17) = '|'
                      fmt( 8)(17:17) = '|'
                      fmt( 7)(17:17) = '|'
                      fmt( 6)(17:17) = '|'
                      fmt( 5)(17:17) = '|'
                      fmt( 4)(17:17) = '|'
                      fmt( 3)(17:17) = 'u'
                  else if (ivalue.eq. -7) then
                      fmt(11)(17:17) = '|'
                      fmt(12)(17:17) = '|'
                      fmt(13)(17:17) = '|'
                      fmt(14)(17:17) = '|'
                      fmt(15)(17:17) = '|'
                      fmt(16)(17:17) = '|'
                      fmt(17)(17:17) = 'u'
                  else if (ivalue.eq.  8) then
                      fmt( 9)(17:17) = '|'
                      fmt( 8)(17:17) = '|'
                      fmt( 7)(17:17) = '|'
                      fmt( 6)(17:17) = '|'
                      fmt( 5)(17:17) = '|'
                      fmt( 4)(17:17) = '|'
                      fmt( 3)(17:17) = '|'
                      fmt( 2)(17:17) = 'u'
                  else if (ivalue.eq. -8) then
                      fmt(11)(17:17) = '|'
                      fmt(12)(17:17) = '|'
                      fmt(13)(17:17) = '|'
                      fmt(14)(17:17) = '|'
                      fmt(15)(17:17) = '|'
                      fmt(16)(17:17) = '|'
                      fmt(17)(17:17) = 'u'
                  else if (ivalue.eq.  9) then
                      fmt( 9)(17:17) = '|'
                      fmt( 8)(17:17) = '|'
                      fmt( 7)(17:17) = '|'
                      fmt( 6)(17:17) = '|'
                      fmt( 5)(17:17) = '|'
                      fmt( 4)(17:17) = '|'
                      fmt( 3)(17:17) = '|'
                      fmt( 2)(17:17) = '|'
                      fmt( 1)(17:17) = 'u'
                  else if (ivalue.eq. -9) then
                      fmt(11)(17:17) = '|'
                      fmt(12)(17:17) = '|'
                      fmt(13)(17:17) = '|'
                      fmt(14)(17:17) = '|'
                      fmt(15)(17:17) = '|'
                      fmt(16)(17:17) = '|'
                      fmt(17)(17:17) = '|'
                      fmt(18)(17:17) = '|'
                      fmt(19)(17:17) = 'u'
                  endif
              else if (n_point.eq.2) then
                  if (ivalue.eq.0) then
                      fmt(10)(28:28) = 'u'
                  else if (ivalue.eq.  1) then
                      fmt( 9)(28:28) = 'u'
                  else if (ivalue.eq. -1) then
                      fmt(11)(28:28) = 'u'
                  else if (ivalue.eq.  2) then
                      fmt( 9)(28:28) = '|'
                      fmt( 8)(28:28) = 'u'
                  else if (ivalue.eq. -2) then
                      fmt(11)(28:28) = '|'
                      fmt(12)(28:28) = 'u'
                  else if (ivalue.eq.  3) then
                      fmt( 9)(28:28) = '|'
                      fmt( 8)(28:28) = '|'
                      fmt( 7)(28:28) = 'u'
                  else if (ivalue.eq. -3) then
                      fmt(11)(28:28) = '|'
                      fmt(12)(28:28) = '|'
                      fmt(13)(28:28) = 'u'
                  else if (ivalue.eq.  4) then
                      fmt( 9)(28:28) = '|'
                      fmt( 8)(28:28) = '|'
                      fmt( 7)(28:28) = '|'
                      fmt( 6)(28:28) = 'u'
                  else if (ivalue.eq. -4) then
                      fmt(11)(28:28) = '|'
                      fmt(12)(28:28) = '|'
                      fmt(13)(28:28) = '|'
                      fmt(14)(28:28) = 'u'
                  else if (ivalue.eq.  5) then
                      fmt( 9)(28:28) = '|'
                      fmt( 8)(28:28) = '|'
                      fmt( 7)(28:28) = '|'
                      fmt( 6)(28:28) = '|'
                      fmt( 5)(28:28) = 'u'
                  else if (ivalue.eq. -5) then
                      fmt(11)(28:28) = '|'
                      fmt(12)(28:28) = '|'
                      fmt(13)(28:28) = '|'
                      fmt(14)(28:28) = '|'
                      fmt(15)(28:28) = 'u'
                  else if (ivalue.eq.  6) then
                      fmt( 9)(28:28) = '|'
                      fmt( 8)(28:28) = '|'
                      fmt( 7)(28:28) = '|'
                      fmt( 6)(28:28) = '|'
                      fmt( 5)(28:28) = '|'
                      fmt( 4)(28:28) = 'u'
                  else if (ivalue.eq. -6) then
                      fmt(11)(28:28) = '|'
                      fmt(12)(28:28) = '|'
                      fmt(13)(28:28) = '|'
                      fmt(14)(28:28) = '|'
                      fmt(15)(28:28) = '|'
                      fmt(16)(28:28) = 'u'
                  else if (ivalue.eq.  7) then
                      fmt( 9)(28:28) = '|'
                      fmt( 8)(28:28) = '|'
                      fmt( 7)(28:28) = '|'
                      fmt( 6)(28:28) = '|'
                      fmt( 5)(28:28) = '|'
                      fmt( 4)(28:28) = '|'
                      fmt( 3)(28:28) = 'u'
                  else if (ivalue.eq. -7) then
                      fmt(11)(28:28) = '|'
                      fmt(12)(28:28) = '|'
                      fmt(13)(28:28) = '|'
                      fmt(14)(28:28) = '|'
                      fmt(15)(28:28) = '|'
                      fmt(16)(28:28) = '|'
                      fmt(17)(28:28) = 'u'
                  else if (ivalue.eq.  8) then
                      fmt( 9)(28:28) = '|'
                      fmt( 8)(28:28) = '|'
                      fmt( 7)(28:28) = '|'
                      fmt( 6)(28:28) = '|'
                      fmt( 5)(28:28) = '|'
                      fmt( 4)(28:28) = '|'
                      fmt( 3)(28:28) = '|'
                      fmt( 2)(28:28) = 'u'
                  else if (ivalue.eq. -8) then
                      fmt(11)(28:28) = '|'
                      fmt(12)(28:28) = '|'
                      fmt(13)(28:28) = '|'
                      fmt(14)(28:28) = '|'
                      fmt(15)(28:28) = '|'
                      fmt(16)(28:28) = '|'
                      fmt(17)(28:28) = 'u'
                  else if (ivalue.eq.  9) then
                      fmt( 9)(28:28) = '|'
                      fmt( 8)(28:28) = '|'
                      fmt( 7)(28:28) = '|'
                      fmt( 6)(28:28) = '|'
                      fmt( 5)(28:28) = '|'
                      fmt( 4)(28:28) = '|'
                      fmt( 3)(28:28) = '|'
                      fmt( 2)(28:28) = '|'
                      fmt( 1)(28:28) = 'u'
                  else if (ivalue.eq. -9) then
                      fmt(11)(28:28) = '|'
                      fmt(12)(28:28) = '|'
                      fmt(13)(28:28) = '|'
                      fmt(14)(28:28) = '|'
                      fmt(15)(28:28) = '|'
                      fmt(16)(28:28) = '|'
                      fmt(17)(28:28) = '|'
                      fmt(18)(28:28) = '|'
                      fmt(19)(28:28) = 'u'
                  endif
              else if (n_point.eq.3) then
                  if (ivalue.eq.0) then
                      fmt(10)(39:39) = 'u'
                  else if (ivalue.eq.  1) then
                      fmt( 9)(39:39) = 'u'
                  else if (ivalue.eq. -1) then
                      fmt(11)(39:39) = 'u'
                  else if (ivalue.eq.  2) then
                      fmt( 9)(39:39) = '|'
                      fmt( 8)(39:39) = 'u'
                  else if (ivalue.eq. -2) then
                      fmt(11)(39:39) = '|'
                      fmt(12)(39:39) = 'u'
                  else if (ivalue.eq.  3) then
                      fmt( 9)(39:39) = '|'
                      fmt( 8)(39:39) = '|'
                      fmt( 7)(39:39) = 'u'
                  else if (ivalue.eq. -3) then
                      fmt(11)(39:39) = '|'
                      fmt(12)(39:39) = '|'
                      fmt(13)(39:39) = 'u'
                  else if (ivalue.eq.  4) then
                      fmt( 9)(39:39) = '|'
                      fmt( 8)(39:39) = '|'
                      fmt( 7)(39:39) = '|'
                      fmt( 6)(39:39) = 'u'
                  else if (ivalue.eq. -4) then
                      fmt(11)(39:39) = '|'
                      fmt(12)(39:39) = '|'
                      fmt(13)(39:39) = '|'
                      fmt(14)(39:39) = 'u'
                  else if (ivalue.eq.  5) then
                      fmt( 9)(39:39) = '|'
                      fmt( 8)(39:39) = '|'
                      fmt( 7)(39:39) = '|'
                      fmt( 6)(39:39) = '|'
                      fmt( 5)(39:39) = 'u'
                  else if (ivalue.eq. -5) then
                      fmt(11)(39:39) = '|'
                      fmt(12)(39:39) = '|'
                      fmt(13)(39:39) = '|'
                      fmt(14)(39:39) = '|'
                      fmt(15)(39:39) = 'u'
                  else if (ivalue.eq.  6) then
                      fmt( 9)(39:39) = '|'
                      fmt( 8)(39:39) = '|'
                      fmt( 7)(39:39) = '|'
                      fmt( 6)(39:39) = '|'
                      fmt( 5)(39:39) = '|'
                      fmt( 4)(39:39) = 'u'
                  else if (ivalue.eq. -6) then
                      fmt(11)(39:39) = '|'
                      fmt(12)(39:39) = '|'
                      fmt(13)(39:39) = '|'
                      fmt(14)(39:39) = '|'
                      fmt(15)(39:39) = '|'
                      fmt(16)(39:39) = 'u'
                  else if (ivalue.eq.  7) then
                      fmt( 9)(39:39) = '|'
                      fmt( 8)(39:39) = '|'
                      fmt( 7)(39:39) = '|'
                      fmt( 6)(39:39) = '|'
                      fmt( 5)(39:39) = '|'
                      fmt( 4)(39:39) = '|'
                      fmt( 3)(39:39) = 'u'
                  else if (ivalue.eq. -7) then
                      fmt(11)(39:39) = '|'
                      fmt(12)(39:39) = '|'
                      fmt(13)(39:39) = '|'
                      fmt(14)(39:39) = '|'
                      fmt(15)(39:39) = '|'
                      fmt(16)(39:39) = '|'
                      fmt(17)(39:39) = 'u'
                  else if (ivalue.eq.  8) then
                      fmt( 9)(39:39) = '|'
                      fmt( 8)(39:39) = '|'
                      fmt( 7)(39:39) = '|'
                      fmt( 6)(39:39) = '|'
                      fmt( 5)(39:39) = '|'
                      fmt( 4)(39:39) = '|'
                      fmt( 3)(39:39) = '|'
                      fmt( 2)(39:39) = 'u'
                  else if (ivalue.eq. -8) then
                      fmt(11)(39:39) = '|'
                      fmt(12)(39:39) = '|'
                      fmt(13)(39:39) = '|'
                      fmt(14)(39:39) = '|'
                      fmt(15)(39:39) = '|'
                      fmt(16)(39:39) = '|'
                      fmt(17)(39:39) = 'u'
                  else if (ivalue.eq.  9) then
                      fmt( 9)(39:39) = '|'
                      fmt( 8)(39:39) = '|'
                      fmt( 7)(39:39) = '|'
                      fmt( 6)(39:39) = '|'
                      fmt( 5)(39:39) = '|'
                      fmt( 4)(39:39) = '|'
                      fmt( 3)(39:39) = '|'
                      fmt( 2)(39:39) = '|'
                      fmt( 1)(39:39) = 'u'
                  else if (ivalue.eq. -9) then
                      fmt(11)(39:39) = '|'
                      fmt(12)(39:39) = '|'
                      fmt(13)(39:39) = '|'
                      fmt(14)(39:39) = '|'
                      fmt(15)(39:39) = '|'
                      fmt(16)(39:39) = '|'
                      fmt(17)(39:39) = '|'
                      fmt(18)(39:39) = '|'
                      fmt(19)(39:39) = 'u'
                  endif
              else if (n_point.eq.4) then
                  if (ivalue.eq.0) then
                      fmt(10)(50:50) = 'u'
                  else if (ivalue.eq.  1) then
                      fmt( 9)(50:50) = 'u'
                  else if (ivalue.eq. -1) then
                      fmt(11)(50:50) = 'u'
                  else if (ivalue.eq.  2) then
                      fmt( 9)(50:50) = '|'
                      fmt( 8)(50:50) = 'u'
                  else if (ivalue.eq. -2) then
                      fmt(11)(50:50) = '|'
                      fmt(12)(50:50) = 'u'
                  else if (ivalue.eq.  3) then
                      fmt( 9)(50:50) = '|'
                      fmt( 8)(50:50) = 'u'
                  else if (ivalue.eq. -3) then
                      fmt(11)(50:50) = '|'
                      fmt(12)(50:50) = 'u'
                  else if (ivalue.eq.  4) then
                      fmt( 9)(50:50) = '|'
                      fmt( 8)(50:50) = '|'
                      fmt( 7)(50:50) = 'u'
                  else if (ivalue.eq. -4) then
                      fmt(11)(50:50) = '|'
                      fmt(12)(50:50) = '|'
                      fmt(13)(50:50) = 'u'
                  else if (ivalue.eq.  5) then
                      fmt( 9)(50:50) = '|'
                      fmt( 8)(50:50) = '|'
                      fmt( 7)(50:50) = '|'
                      fmt( 6)(50:50) = 'u'
                  else if (ivalue.eq. -5) then
                      fmt(11)(50:50) = '|'
                      fmt(12)(50:50) = '|'
                      fmt(13)(50:50) = '|'
                      fmt(14)(50:50) = 'u'
                  else if (ivalue.eq.  6) then
                      fmt( 9)(50:50) = '|'
                      fmt( 8)(50:50) = '|'
                      fmt( 7)(50:50) = '|'
                      fmt( 6)(50:50) = '|'
                      fmt( 5)(50:50) = 'u'
                  else if (ivalue.eq. -6) then
                      fmt(11)(50:50) = '|'
                      fmt(12)(50:50) = '|'
                      fmt(13)(50:50) = '|'
                      fmt(14)(50:50) = '|'
                      fmt(15)(50:50) = 'u'
                  else if (ivalue.eq.  7) then
                      fmt( 9)(50:50) = '|'
                      fmt( 8)(50:50) = '|'
                      fmt( 7)(50:50) = '|'
                      fmt( 6)(50:50) = '|'
                      fmt( 5)(50:50) = '|'
                      fmt( 4)(50:50) = 'u'
                  else if (ivalue.eq. -7) then
                      fmt(11)(50:50) = '|'
                      fmt(12)(50:50) = '|'
                      fmt(13)(50:50) = '|'
                      fmt(14)(50:50) = '|'
                      fmt(15)(50:50) = '|'
                      fmt(16)(50:50) = 'u'
                  else if (ivalue.eq.  8) then
                      fmt( 9)(50:50) = '|'
                      fmt( 8)(50:50) = '|'
                      fmt( 7)(50:50) = '|'
                      fmt( 6)(50:50) = '|'
                      fmt( 5)(50:50) = '|'
                      fmt( 4)(50:50) = '|'
                      fmt( 3)(50:50) = 'u'
                  else if (ivalue.eq. -8) then
                      fmt(11)(50:50) = '|'
                      fmt(12)(50:50) = '|'
                      fmt(13)(50:50) = '|'
                      fmt(14)(50:50) = '|'
                      fmt(15)(50:50) = '|'
                      fmt(16)(50:50) = 'u'
                  else if (ivalue.eq.  9) then
                      fmt( 9)(50:50) = '|'
                      fmt( 8)(50:50) = '|'
                      fmt( 7)(50:50) = '|'
                      fmt( 6)(50:50) = '|'
                      fmt( 5)(50:50) = '|'
                      fmt( 4)(50:50) = '|'
                      fmt( 3)(50:50) = '|'
                      fmt( 2)(50:50) = 'u'
                  else if (ivalue.eq. -9) then
                      fmt(11)(50:50) = '|'
                      fmt(12)(50:50) = '|'
                      fmt(13)(50:50) = '|'
                      fmt(14)(50:50) = '|'
                      fmt(15)(50:50) = '|'
                      fmt(16)(50:50) = '|'
                      fmt(17)(50:50) = '|'
                      fmt(18)(50:50) = 'u'
                  endif
              else if (n_point.eq.5) then
                  if (ivalue.eq.0) then
                      fmt(14)(13:13) = 'u'
                  else if (ivalue.eq.  1) then
                      fmt(13)(13:13) = 'u'
                  else if (ivalue.eq. -1) then
                      fmt(15)(13:13) = 'u'
                  else if (ivalue.eq.  2) then
                      fmt(13)(13:13) = '|'
                      fmt(12)(13:13) = 'u'
                  else if (ivalue.eq. -2) then
                      fmt(15)(13:13) = '|'
                      fmt(16)(13:13) = 'u'
                  else if (ivalue.eq.  3) then
                      fmt(13)(13:13) = '|'
                      fmt(12)(13:13) = '|'
                      fmt(11)(13:13) = 'u'
                  else if (ivalue.eq. -3) then
                      fmt(15)(13:13) = '|'
                      fmt(16)(13:13) = '|'
                      fmt(17)(13:13) = 'u'
                  else if (ivalue.eq.  4) then
                      fmt(13)(13:13) = '|'
                      fmt(12)(13:13) = '|'
                      fmt(11)(13:13) = '|'
                      fmt(10)(13:13) = 'u'
                  else if (ivalue.eq. -4) then
                      fmt(15)(13:13) = '|'
                      fmt(16)(13:13) = '|'
                      fmt(17)(13:13) = '|'
                      fmt(18)(13:13) = 'u'
                  else if (ivalue.eq.  5) then
                      fmt(13)(13:13) = '|'
                      fmt(12)(13:13) = '|'
                      fmt(11)(13:13) = '|'
                      fmt(10)(13:13) = '|'
                      fmt( 9)(13:13) = 'u'
                  else if (ivalue.eq. -5) then
                      fmt(15)(13:13) = '|'
                      fmt(16)(13:13) = '|'
                      fmt(17)(13:13) = '|'
                      fmt(18)(13:13) = '|'
                      fmt(19)(13:13) = 'u'
                  else if (ivalue.eq.  6) then
                      fmt(13)(13:13) = '|'
                      fmt(12)(13:13) = '|'
                      fmt(11)(13:13) = '|'
                      fmt(10)(13:13) = '|'
                      fmt( 9)(13:13) = '|'
                      fmt( 8)(13:13) = 'u'
                  else if (ivalue.eq. -6) then
                      fmt(15)(13:13) = '|'
                      fmt(16)(13:13) = '|'
                      fmt(17)(13:13) = '|'
                      fmt(18)(13:13) = '|'
                      fmt(19)(13:13) = '|'
                      fmt(20)(13:13) = 'u'
                  else if (ivalue.eq.  7) then
                      fmt(13)(13:13) = '|'
                      fmt(12)(13:13) = '|'
                      fmt(11)(13:13) = '|'
                      fmt(10)(13:13) = '|'
                      fmt( 9)(13:13) = '|'
                      fmt( 8)(13:13) = '|'
                      fmt( 7)(13:13) = 'u'
                  else if (ivalue.eq. -7) then
                      fmt(15)(13:13) = '|'
                      fmt(16)(13:13) = '|'
                      fmt(17)(13:13) = '|'
                      fmt(18)(13:13) = '|'
                      fmt(19)(13:13) = '|'
                      fmt(20)(13:13) = '|'
                      fmt(21)(13:13) = 'u'
                  else if (ivalue.eq.  8) then
                      fmt(13)(13:13) = '|'
                      fmt(12)(13:13) = '|'
                      fmt(11)(13:13) = '|'
                      fmt(10)(13:13) = '|'
                      fmt( 9)(13:13) = '|'
                      fmt( 8)(13:13) = '|'
                      fmt( 7)(13:13) = '|'
                      fmt( 6)(13:13) = 'u'
                  else if (ivalue.eq. -8) then
                      fmt(15)(13:13) = '|'
                      fmt(16)(13:13) = '|'
                      fmt(17)(13:13) = '|'
                      fmt(18)(13:13) = '|'
                      fmt(19)(13:13) = '|'
                      fmt(20)(13:13) = '|'
                      fmt(21)(13:13) = 'u'
                  else if (ivalue.eq.  9) then
                      fmt(13)(13:13) = '|'
                      fmt(12)(13:13) = '|'
                      fmt(11)(13:13) = '|'
                      fmt(10)(13:13) = '|'
                      fmt( 9)(13:13) = '|'
                      fmt( 8)(13:13) = '|'
                      fmt( 7)(13:13) = '|'
                      fmt( 6)(13:13) = '|'
                      fmt( 5)(13:13) = 'u'
                  else if (ivalue.eq. -9) then
                      fmt(15)(13:13) = '|'
                      fmt(16)(13:13) = '|'
                      fmt(17)(13:13) = '|'
                      fmt(18)(13:13) = '|'
                      fmt(19)(13:13) = '|'
                      fmt(20)(13:13) = '|'
                      fmt(21)(13:13) = '|'
                      fmt(22)(13:13) = '|'
                      fmt(23)(13:13) = 'u'
                  endif
              else if (n_point.eq.6) then
                  if (ivalue.eq.0) then
                      fmt(14)(24:24) = 'u'
                  else if (ivalue.eq.  1) then
                      fmt(13)(24:24) = 'u'
                  else if (ivalue.eq. -1) then
                      fmt(15)(24:24) = 'u'
                  else if (ivalue.eq.  2) then
                      fmt(13)(24:24) = '|'
                      fmt(12)(24:24) = 'u'
                  else if (ivalue.eq. -2) then
                      fmt(15)(24:24) = '|'
                      fmt(16)(24:24) = 'u'
                  else if (ivalue.eq.  3) then
                      fmt(13)(24:24) = '|'
                      fmt(12)(24:24) = '|'
                      fmt(11)(24:24) = 'u'
                  else if (ivalue.eq. -3) then
                      fmt(15)(24:24) = '|'
                      fmt(16)(24:24) = '|'
                      fmt(17)(24:24) = 'u'
                  else if (ivalue.eq.  4) then
                      fmt(13)(24:24) = '|'
                      fmt(12)(24:24) = '|'
                      fmt(11)(24:24) = '|'
                      fmt(10)(24:24) = 'u'
                  else if (ivalue.eq. -4) then
                      fmt(15)(24:24) = '|'
                      fmt(16)(24:24) = '|'
                      fmt(17)(24:24) = '|'
                      fmt(18)(24:24) = 'u'
                  else if (ivalue.eq.  5) then
                      fmt(13)(24:24) = '|'
                      fmt(12)(24:24) = '|'
                      fmt(11)(24:24) = '|'
                      fmt(10)(24:24) = '|'
                      fmt( 9)(24:24) = 'u'
                  else if (ivalue.eq. -5) then
                      fmt(15)(24:24) = '|'
                      fmt(16)(24:24) = '|'
                      fmt(17)(24:24) = '|'
                      fmt(18)(24:24) = '|'
                      fmt(19)(24:24) = 'u'
                  else if (ivalue.eq.  6) then
                      fmt(13)(24:24) = '|'
                      fmt(12)(24:24) = '|'
                      fmt(11)(24:24) = '|'
                      fmt(10)(24:24) = '|'
                      fmt( 9)(24:24) = '|'
                      fmt( 8)(24:24) = 'u'
                  else if (ivalue.eq. -6) then
                      fmt(15)(24:24) = '|'
                      fmt(16)(24:24) = '|'
                      fmt(17)(24:24) = '|'
                      fmt(18)(24:24) = '|'
                      fmt(19)(24:24) = '|'
                      fmt(20)(24:24) = 'u'
                  else if (ivalue.eq.  7) then
                      fmt(13)(24:24) = '|'
                      fmt(12)(24:24) = '|'
                      fmt(11)(24:24) = '|'
                      fmt(10)(24:24) = '|'
                      fmt( 9)(24:24) = '|'
                      fmt( 8)(24:24) = '|'
                      fmt( 7)(24:24) = 'u'
                  else if (ivalue.eq. -7) then
                      fmt(15)(24:24) = '|'
                      fmt(16)(24:24) = '|'
                      fmt(17)(24:24) = '|'
                      fmt(18)(24:24) = '|'
                      fmt(19)(24:24) = '|'
                      fmt(20)(24:24) = '|'
                      fmt(21)(24:24) = 'u'
                  else if (ivalue.eq.  8) then
                      fmt(13)(24:24) = '|'
                      fmt(12)(24:24) = '|'
                      fmt(11)(24:24) = '|'
                      fmt(10)(24:24) = '|'
                      fmt( 9)(24:24) = '|'
                      fmt( 8)(24:24) = '|'
                      fmt( 7)(24:24) = '|'
                      fmt( 6)(24:24) = 'u'
                  else if (ivalue.eq. -8) then
                      fmt(15)(24:24) = '|'
                      fmt(16)(24:24) = '|'
                      fmt(17)(24:24) = '|'
                      fmt(18)(24:24) = '|'
                      fmt(19)(24:24) = '|'
                      fmt(20)(24:24) = '|'
                      fmt(21)(24:24) = 'u'
                  else if (ivalue.eq.  9) then
                      fmt(13)(24:24) = '|'
                      fmt(12)(24:24) = '|'
                      fmt(11)(24:24) = '|'
                      fmt(10)(24:24) = '|'
                      fmt( 9)(24:24) = '|'
                      fmt( 8)(24:24) = '|'
                      fmt( 7)(24:24) = '|'
                      fmt( 6)(24:24) = '|'
                      fmt( 5)(24:24) = 'u'
                  else if (ivalue.eq. -9) then
                      fmt(15)(24:24) = '|'
                      fmt(16)(24:24) = '|'
                      fmt(17)(24:24) = '|'
                      fmt(18)(24:24) = '|'
                      fmt(19)(24:24) = '|'
                      fmt(20)(24:24) = '|'
                      fmt(21)(24:24) = '|'
                      fmt(22)(24:24) = '|'
                      fmt(23)(24:24) = 'u'
                  endif
              else if (n_point.eq.7) then
                  if (ivalue.eq.0) then
                      fmt(14)(35:35) = 'u'
                  else if (ivalue.eq.  1) then
                      fmt(13)(35:35) = 'u'
                  else if (ivalue.eq. -1) then
                      fmt(15)(35:35) = 'u'
                  else if (ivalue.eq.  2) then
                      fmt(13)(35:35) = '|'
                      fmt(12)(35:35) = 'u'
                  else if (ivalue.eq. -2) then
                      fmt(15)(35:35) = '|'
                      fmt(16)(35:35) = 'u'
                  else if (ivalue.eq.  3) then
                      fmt(13)(35:35) = '|'
                      fmt(12)(35:35) = '|'
                      fmt(11)(35:35) = 'u'
                  else if (ivalue.eq. -3) then
                      fmt(15)(35:35) = '|'
                      fmt(16)(35:35) = '|'
                      fmt(17)(35:35) = 'u'
                  else if (ivalue.eq.  4) then
                      fmt(13)(35:35) = '|'
                      fmt(12)(35:35) = '|'
                      fmt(11)(35:35) = '|'
                      fmt(10)(35:35) = 'u'
                  else if (ivalue.eq. -4) then
                      fmt(15)(35:35) = '|'
                      fmt(16)(35:35) = '|'
                      fmt(17)(35:35) = '|'
                      fmt(18)(35:35) = 'u'
                  else if (ivalue.eq.  5) then
                      fmt(13)(35:35) = '|'
                      fmt(12)(35:35) = '|'
                      fmt(11)(35:35) = '|'
                      fmt(10)(35:35) = '|'
                      fmt( 9)(35:35) = 'u'
                  else if (ivalue.eq. -5) then
                      fmt(15)(35:35) = '|'
                      fmt(16)(35:35) = '|'
                      fmt(17)(35:35) = '|'
                      fmt(18)(35:35) = '|'
                      fmt(19)(35:35) = 'u'
                  else if (ivalue.eq.  6) then
                      fmt(13)(35:35) = '|'
                      fmt(12)(35:35) = '|'
                      fmt(11)(35:35) = '|'
                      fmt(10)(35:35) = '|'
                      fmt( 9)(35:35) = '|'
                      fmt( 8)(35:35) = 'u'
                  else if (ivalue.eq. -6) then
                      fmt(15)(35:35) = '|'
                      fmt(16)(35:35) = '|'
                      fmt(17)(35:35) = '|'
                      fmt(18)(35:35) = '|'
                      fmt(19)(35:35) = '|'
                      fmt(20)(35:35) = 'u'
                  else if (ivalue.eq.  7) then
                      fmt(13)(35:35) = '|'
                      fmt(12)(35:35) = '|'
                      fmt(11)(35:35) = '|'
                      fmt(10)(35:35) = '|'
                      fmt( 9)(35:35) = '|'
                      fmt( 8)(35:35) = '|'
                      fmt( 7)(35:35) = 'u'
                  else if (ivalue.eq. -7) then
                      fmt(15)(35:35) = '|'
                      fmt(16)(35:35) = '|'
                      fmt(17)(35:35) = '|'
                      fmt(18)(35:35) = '|'
                      fmt(19)(35:35) = '|'
                      fmt(20)(35:35) = '|'
                      fmt(21)(35:35) = 'u'
                  else if (ivalue.eq.  8) then
                      fmt(13)(35:35) = '|'
                      fmt(12)(35:35) = '|'
                      fmt(11)(35:35) = '|'
                      fmt(10)(35:35) = '|'
                      fmt( 9)(35:35) = '|'
                      fmt( 8)(35:35) = '|'
                      fmt( 7)(35:35) = '|'
                      fmt( 6)(35:35) = 'u'
                  else if (ivalue.eq. -8) then
                      fmt(15)(35:35) = '|'
                      fmt(16)(35:35) = '|'
                      fmt(17)(35:35) = '|'
                      fmt(18)(35:35) = '|'
                      fmt(19)(35:35) = '|'
                      fmt(20)(35:35) = '|'
                      fmt(21)(35:35) = 'u'
                  else if (ivalue.eq.  9) then
                      fmt(13)(35:35) = '|'
                      fmt(12)(35:35) = '|'
                      fmt(11)(35:35) = '|'
                      fmt(10)(35:35) = '|'
                      fmt( 9)(35:35) = '|'
                      fmt( 8)(35:35) = '|'
                      fmt( 7)(35:35) = '|'
                      fmt( 6)(35:35) = '|'
                      fmt( 5)(35:35) = 'u'
                  else if (ivalue.eq. -9) then
                      fmt(15)(35:35) = '|'
                      fmt(16)(35:35) = '|'
                      fmt(17)(35:35) = '|'
                      fmt(18)(35:35) = '|'
                      fmt(19)(35:35) = '|'
                      fmt(20)(35:35) = '|'
                      fmt(21)(35:35) = '|'
                      fmt(22)(35:35) = '|'
                      fmt(23)(35:35) = 'u'
                  endif
              else if (n_point.eq.8) then
                  if (ivalue.eq.0) then
                      fmt(14)(46:46) = 'u'
                  else if (ivalue.eq.  1) then
                      fmt(13)(46:46) = 'u'
                  else if (ivalue.eq. -1) then
                      fmt(15)(46:46) = 'u'
                  else if (ivalue.eq.  2) then
                      fmt(13)(46:46) = '|'
                      fmt(12)(46:46) = 'u'
                  else if (ivalue.eq. -2) then
                      fmt(15)(46:46) = '|'
                      fmt(16)(46:46) = 'u'
                  else if (ivalue.eq.  3) then
                      fmt(13)(46:46) = '|'
                      fmt(12)(46:46) = '|'
                      fmt(11)(46:46) = 'u'
                  else if (ivalue.eq. -3) then
                      fmt(15)(46:46) = '|'
                      fmt(16)(46:46) = '|'
                      fmt(17)(46:46) = 'u'
                  else if (ivalue.eq.  4) then
                      fmt(13)(46:46) = '|'
                      fmt(12)(46:46) = '|'
                      fmt(11)(46:46) = '|'
                      fmt(10)(46:46) = 'u'
                  else if (ivalue.eq. -4) then
                      fmt(15)(46:46) = '|'
                      fmt(16)(46:46) = '|'
                      fmt(17)(46:46) = '|'
                      fmt(18)(46:46) = 'u'
                  else if (ivalue.eq.  5) then
                      fmt(13)(46:46) = '|'
                      fmt(12)(46:46) = '|'
                      fmt(11)(46:46) = '|'
                      fmt(10)(46:46) = '|'
                      fmt( 9)(46:46) = 'u'
                  else if (ivalue.eq. -5) then
                      fmt(15)(46:46) = '|'
                      fmt(16)(46:46) = '|'
                      fmt(17)(46:46) = '|'
                      fmt(18)(46:46) = '|'
                      fmt(19)(46:46) = 'u'
                  else if (ivalue.eq.  6) then
                      fmt(13)(46:46) = '|'
                      fmt(12)(46:46) = '|'
                      fmt(11)(46:46) = '|'
                      fmt(10)(46:46) = '|'
                      fmt( 9)(46:46) = '|'
                      fmt( 8)(46:46) = 'u'
                  else if (ivalue.eq. -6) then
                      fmt(15)(46:46) = '|'
                      fmt(16)(46:46) = '|'
                      fmt(17)(46:46) = '|'
                      fmt(18)(46:46) = '|'
                      fmt(19)(46:46) = '|'
                      fmt(20)(46:46) = 'u'
                  else if (ivalue.eq.  7) then
                      fmt(13)(46:46) = '|'
                      fmt(12)(46:46) = '|'
                      fmt(11)(46:46) = '|'
                      fmt(10)(46:46) = '|'
                      fmt( 9)(46:46) = '|'
                      fmt( 8)(46:46) = '|'
                      fmt( 7)(46:46) = 'u'
                  else if (ivalue.eq. -7) then
                      fmt(15)(46:46) = '|'
                      fmt(16)(46:46) = '|'
                      fmt(17)(46:46) = '|'
                      fmt(18)(46:46) = '|'
                      fmt(19)(46:46) = '|'
                      fmt(20)(46:46) = '|'
                      fmt(21)(46:46) = 'u'
                  else if (ivalue.eq.  8) then
                      fmt(13)(46:46) = '|'
                      fmt(12)(46:46) = '|'
                      fmt(11)(46:46) = '|'
                      fmt(10)(46:46) = '|'
                      fmt( 9)(46:46) = '|'
                      fmt( 8)(46:46) = '|'
                      fmt( 7)(46:46) = '|'
                      fmt( 6)(46:46) = 'u'
                  else if (ivalue.eq. -8) then
                      fmt(15)(46:46) = '|'
                      fmt(16)(46:46) = '|'
                      fmt(17)(46:46) = '|'
                      fmt(18)(46:46) = '|'
                      fmt(19)(46:46) = '|'
                      fmt(20)(46:46) = '|'
                      fmt(21)(46:46) = 'u'
                  else if (ivalue.eq.  9) then
                      fmt(13)(46:46) = '|'
                      fmt(12)(46:46) = '|'
                      fmt(11)(46:46) = '|'
                      fmt(10)(46:46) = '|'
                      fmt( 9)(46:46) = '|'
                      fmt( 8)(46:46) = '|'
                      fmt( 7)(46:46) = '|'
                      fmt( 6)(46:46) = '|'
                      fmt( 5)(46:46) = 'u'
                  else if (ivalue.eq. -9) then
                      fmt(15)(46:46) = '|'
                      fmt(16)(46:46) = '|'
                      fmt(17)(46:46) = '|'
                      fmt(18)(46:46) = '|'
                      fmt(19)(46:46) = '|'
                      fmt(20)(46:46) = '|'
                      fmt(21)(46:46) = '|'
                      fmt(22)(46:46) = '|'
                      fmt(23)(46:46) = 'u'
                  endif
              else if (n_point.eq.9) then
                  if (ivalue.eq.0) then
                      fmt(18)(9:9) = 'u'
                  else if (ivalue.eq.  1) then
                      fmt(17)(9:9) = 'u'
                  else if (ivalue.eq. -1) then
                      fmt(19)(9:9) = 'u'
                  else if (ivalue.eq.  2) then
                      fmt(17)(9:9) = '|'
                      fmt(16)(9:9) = 'u'
                  else if (ivalue.eq. -2) then
                      fmt(19)(9:9) = '|'
                      fmt(20)(9:9) = 'u'
                  else if (ivalue.eq.  3) then
                      fmt(17)(9:9) = '|'
                      fmt(16)(9:9) = '|'
                      fmt(15)(9:9) = 'u'
                  else if (ivalue.eq. -3) then
                      fmt(19)(9:9) = '|'
                      fmt(20)(9:9) = '|'
                      fmt(21)(9:9) = 'u'
                  else if (ivalue.eq.  4) then
                      fmt(17)(9:9) = '|'
                      fmt(16)(9:9) = '|'
                      fmt(15)(9:9) = '|'
                      fmt(14)(9:9) = 'u'
                  else if (ivalue.eq. -4) then
                      fmt(19)(9:9) = '|'
                      fmt(20)(9:9) = '|'
                      fmt(21)(9:9) = '|'
                      fmt(22)(9:9) = 'u'
                  else if (ivalue.eq.  5) then
                      fmt(17)(9:9) = '|'
                      fmt(16)(9:9) = '|'
                      fmt(15)(9:9) = '|'
                      fmt(14)(9:9) = '|'
                      fmt(13)(9:9) = 'u'
                  else if (ivalue.eq. -5) then
                      fmt(19)(9:9) = '|'
                      fmt(20)(9:9) = '|'
                      fmt(21)(9:9) = '|'
                      fmt(22)(9:9) = '|'
                      fmt(23)(9:9) = 'u'
                  else if (ivalue.eq.  6) then
                      fmt(17)(9:9) = '|'
                      fmt(16)(9:9) = '|'
                      fmt(15)(9:9) = '|'
                      fmt(14)(9:9) = '|'
                      fmt(13)(9:9) = '|'
                      fmt(12)(9:9) = 'u'
                  else if (ivalue.eq. -6) then
                      fmt(19)(9:9) = '|'
                      fmt(20)(9:9) = '|'
                      fmt(21)(9:9) = '|'
                      fmt(22)(9:9) = '|'
                      fmt(23)(9:9) = '|'
                      fmt(24)(9:9) = 'u'
                  else if (ivalue.eq.  7) then
                      fmt(17)(9:9) = '|'
                      fmt(16)(9:9) = '|'
                      fmt(15)(9:9) = '|'
                      fmt(14)(9:9) = '|'
                      fmt(13)(9:9) = '|'
                      fmt(12)(9:9) = '|'
                      fmt(11)(9:9) = 'u'
                  else if (ivalue.eq. -7) then
                      fmt(19)(9:9) = '|'
                      fmt(20)(9:9) = '|'
                      fmt(21)(9:9) = '|'
                      fmt(22)(9:9) = '|'
                      fmt(23)(9:9) = '|'
                      fmt(24)(9:9) = '|'
                      fmt(25)(9:9) = 'u'
                  else if (ivalue.eq.  8) then
                      fmt(17)(9:9) = '|'
                      fmt(16)(9:9) = '|'
                      fmt(15)(9:9) = '|'
                      fmt(14)(9:9) = '|'
                      fmt(13)(9:9) = '|'
                      fmt(12)(9:9) = '|'
                      fmt(11)(9:9) = '|'
                      fmt(10)(9:9) = 'u'
                  else if (ivalue.eq. -8) then
                      fmt(19)(9:9) = '|'
                      fmt(20)(9:9) = '|'
                      fmt(21)(9:9) = '|'
                      fmt(22)(9:9) = '|'
                      fmt(23)(9:9) = '|'
                      fmt(24)(9:9) = '|'
                      fmt(25)(9:9) = 'u'
                  else if (ivalue.eq.  9) then
                      fmt(17)(9:9) = '|'
                      fmt(16)(9:9) = '|'
                      fmt(15)(9:9) = '|'
                      fmt(14)(9:9) = '|'
                      fmt(13)(9:9) = '|'
                      fmt(12)(9:9) = '|'
                      fmt(11)(9:9) = '|'
                      fmt(10)(9:9) = '|'
                      fmt( 9)(9:9) = 'u'
                  else if (ivalue.eq. -9) then
                      fmt(19)(9:9) = '|'
                      fmt(20)(9:9) = '|'
                      fmt(21)(9:9) = '|'
                      fmt(22)(9:9) = '|'
                      fmt(23)(9:9) = '|'
                      fmt(24)(9:9) = '|'
                      fmt(25)(9:9) = '|'
                      fmt(26)(9:9) = '|'
                      fmt(27)(9:9) = 'u'
                  endif
              else if (n_point.eq.10) then
                  if (ivalue.eq.0) then
                      fmt(18)(20:20) = 'u'
                  else if (ivalue.eq.  1) then
                      fmt(17)(20:20) = 'u'
                  else if (ivalue.eq. -1) then
                      fmt(19)(20:20) = 'u'
                  else if (ivalue.eq.  2) then
                      fmt(17)(20:20) = '|'
                      fmt(16)(20:20) = 'u'
                  else if (ivalue.eq. -2) then
                      fmt(19)(20:20) = '|'
                      fmt(20)(20:20) = 'u'
                  else if (ivalue.eq.  3) then
                      fmt(17)(20:20) = '|'
                      fmt(16)(20:20) = '|'
                      fmt(15)(20:20) = 'u'
                  else if (ivalue.eq. -3) then
                      fmt(19)(20:20) = '|'
                      fmt(20)(20:20) = '|'
                      fmt(21)(20:20) = 'u'
                  else if (ivalue.eq.  4) then
                      fmt(17)(20:20) = '|'
                      fmt(16)(20:20) = '|'
                      fmt(15)(20:20) = '|'
                      fmt(14)(20:20) = 'u'
                  else if (ivalue.eq. -4) then
                      fmt(19)(20:20) = '|'
                      fmt(20)(20:20) = '|'
                      fmt(21)(20:20) = '|'
                      fmt(22)(20:20) = 'u'
                  else if (ivalue.eq.  5) then
                      fmt(17)(20:20) = '|'
                      fmt(16)(20:20) = '|'
                      fmt(15)(20:20) = '|'
                      fmt(14)(20:20) = '|'
                      fmt(13)(20:20) = 'u'
                  else if (ivalue.eq. -5) then
                      fmt(19)(20:20) = '|'
                      fmt(20)(20:20) = '|'
                      fmt(21)(20:20) = '|'
                      fmt(22)(20:20) = '|'
                      fmt(23)(20:20) = 'u'
                  else if (ivalue.eq.  6) then
                      fmt(17)(20:20) = '|'
                      fmt(16)(20:20) = '|'
                      fmt(15)(20:20) = '|'
                      fmt(14)(20:20) = '|'
                      fmt(13)(20:20) = '|'
                      fmt(12)(20:20) = 'u'
                  else if (ivalue.eq. -6) then
                      fmt(19)(20:20) = '|'
                      fmt(20)(20:20) = '|'
                      fmt(21)(20:20) = '|'
                      fmt(22)(20:20) = '|'
                      fmt(23)(20:20) = '|'
                      fmt(24)(20:20) = 'u'
                  else if (ivalue.eq.  7) then
                      fmt(17)(20:20) = '|'
                      fmt(16)(20:20) = '|'
                      fmt(15)(20:20) = '|'
                      fmt(14)(20:20) = '|'
                      fmt(13)(20:20) = '|'
                      fmt(12)(20:20) = '|'
                      fmt(11)(20:20) = 'u'
                  else if (ivalue.eq. -7) then
                      fmt(19)(20:20) = '|'
                      fmt(20)(20:20) = '|'
                      fmt(21)(20:20) = '|'
                      fmt(22)(20:20) = '|'
                      fmt(23)(20:20) = '|'
                      fmt(24)(20:20) = '|'
                      fmt(25)(20:20) = 'u'
                  else if (ivalue.eq.  8) then
                      fmt(17)(20:20) = '|'
                      fmt(16)(20:20) = '|'
                      fmt(15)(20:20) = '|'
                      fmt(14)(20:20) = '|'
                      fmt(13)(20:20) = '|'
                      fmt(12)(20:20) = '|'
                      fmt(11)(20:20) = '|'
                      fmt(10)(20:20) = 'u'
                  else if (ivalue.eq. -8) then
                      fmt(19)(20:20) = '|'
                      fmt(20)(20:20) = '|'
                      fmt(21)(20:20) = '|'
                      fmt(22)(20:20) = '|'
                      fmt(23)(20:20) = '|'
                      fmt(24)(20:20) = '|'
                      fmt(25)(20:20) = 'u'
                  else if (ivalue.eq.  9) then
                      fmt(17)(20:20) = '|'
                      fmt(16)(20:20) = '|'
                      fmt(15)(20:20) = '|'
                      fmt(14)(20:20) = '|'
                      fmt(13)(20:20) = '|'
                      fmt(12)(20:20) = '|'
                      fmt(11)(20:20) = '|'
                      fmt(10)(20:20) = '|'
                      fmt( 9)(20:20) = 'u'
                  else if (ivalue.eq. -9) then
                      fmt(19)(20:20) = '|'
                      fmt(20)(20:20) = '|'
                      fmt(21)(20:20) = '|'
                      fmt(22)(20:20) = '|'
                      fmt(23)(20:20) = '|'
                      fmt(24)(20:20) = '|'
                      fmt(25)(20:20) = '|'
                      fmt(26)(20:20) = '|'
                      fmt(27)(20:20) = 'u'
                  endif
              else if (n_point.eq.11) then
                  if (ivalue.eq.0) then
                      fmt(18)(31:31) = 'u'
                  else if (ivalue.eq.  1) then
                      fmt(17)(31:31) = 'u'
                  else if (ivalue.eq. -1) then
                      fmt(19)(31:31) = 'u'
                  else if (ivalue.eq.  2) then
                      fmt(17)(31:31) = '|'
                      fmt(16)(31:31) = 'u'
                  else if (ivalue.eq. -2) then
                      fmt(19)(31:31) = '|'
                      fmt(20)(31:31) = 'u'
                  else if (ivalue.eq.  3) then
                      fmt(17)(31:31) = '|'
                      fmt(16)(31:31) = '|'
                      fmt(15)(31:31) = 'u'
                  else if (ivalue.eq. -3) then
                      fmt(19)(31:31) = '|'
                      fmt(20)(31:31) = '|'
                      fmt(21)(31:31) = 'u'
                  else if (ivalue.eq.  4) then
                      fmt(17)(31:31) = '|'
                      fmt(16)(31:31) = '|'
                      fmt(15)(31:31) = '|'
                      fmt(14)(31:31) = 'u'
                  else if (ivalue.eq. -4) then
                      fmt(19)(31:31) = '|'
                      fmt(20)(31:31) = '|'
                      fmt(21)(31:31) = '|'
                      fmt(22)(31:31) = 'u'
                  else if (ivalue.eq.  5) then
                      fmt(17)(31:31) = '|'
                      fmt(16)(31:31) = '|'
                      fmt(15)(31:31) = '|'
                      fmt(14)(31:31) = '|'
                      fmt(13)(31:31) = 'u'
                  else if (ivalue.eq. -5) then
                      fmt(19)(31:31) = '|'
                      fmt(20)(31:31) = '|'
                      fmt(21)(31:31) = '|'
                      fmt(22)(31:31) = '|'
                      fmt(23)(31:31) = 'u'
                  else if (ivalue.eq.  6) then
                      fmt(17)(31:31) = '|'
                      fmt(16)(31:31) = '|'
                      fmt(15)(31:31) = '|'
                      fmt(14)(31:31) = '|'
                      fmt(13)(31:31) = '|'
                      fmt(12)(31:31) = 'u'
                  else if (ivalue.eq. -6) then
                      fmt(19)(31:31) = '|'
                      fmt(20)(31:31) = '|'
                      fmt(21)(31:31) = '|'
                      fmt(22)(31:31) = '|'
                      fmt(23)(31:31) = '|'
                      fmt(24)(31:31) = 'u'
                  else if (ivalue.eq.  7) then
                      fmt(17)(31:31) = '|'
                      fmt(16)(31:31) = '|'
                      fmt(15)(31:31) = '|'
                      fmt(14)(31:31) = '|'
                      fmt(13)(31:31) = '|'
                      fmt(12)(31:31) = '|'
                      fmt(11)(31:31) = 'u'
                  else if (ivalue.eq. -7) then
                      fmt(19)(31:31) = '|'
                      fmt(20)(31:31) = '|'
                      fmt(21)(31:31) = '|'
                      fmt(22)(31:31) = '|'
                      fmt(23)(31:31) = '|'
                      fmt(24)(31:31) = '|'
                      fmt(25)(31:31) = 'u'
                  else if (ivalue.eq.  8) then
                      fmt(17)(31:31) = '|'
                      fmt(16)(31:31) = '|'
                      fmt(15)(31:31) = '|'
                      fmt(14)(31:31) = '|'
                      fmt(13)(31:31) = '|'
                      fmt(12)(31:31) = '|'
                      fmt(11)(31:31) = '|'
                      fmt(10)(31:31) = 'u'
                  else if (ivalue.eq. -8) then
                      fmt(19)(31:31) = '|'
                      fmt(20)(31:31) = '|'
                      fmt(21)(31:31) = '|'
                      fmt(22)(31:31) = '|'
                      fmt(23)(31:31) = '|'
                      fmt(24)(31:31) = '|'
                      fmt(25)(31:31) = 'u'
                  else if (ivalue.eq.  9) then
                      fmt(17)(31:31) = '|'
                      fmt(16)(31:31) = '|'
                      fmt(15)(31:31) = '|'
                      fmt(14)(31:31) = '|'
                      fmt(13)(31:31) = '|'
                      fmt(12)(31:31) = '|'
                      fmt(11)(31:31) = '|'
                      fmt(10)(31:31) = '|'
                      fmt( 9)(31:31) = 'u'
                  else if (ivalue.eq. -9) then
                      fmt(19)(31:31) = '|'
                      fmt(20)(31:31) = '|'
                      fmt(21)(31:31) = '|'
                      fmt(22)(31:31) = '|'
                      fmt(23)(31:31) = '|'
                      fmt(24)(31:31) = '|'
                      fmt(25)(31:31) = '|'
                      fmt(26)(31:31) = '|'
                      fmt(27)(31:31) = 'u'
                  endif
              else if (n_point.eq.12) then
                  if (ivalue.eq.0) then
                      fmt(18)(42:42) = 'u'
                  else if (ivalue.eq.  1) then
                      fmt(17)(42:42) = 'u'
                  else if (ivalue.eq. -1) then
                      fmt(19)(42:42) = 'u'
                  else if (ivalue.eq.  2) then
                      fmt(17)(42:42) = '|'
                      fmt(16)(42:42) = 'u'
                  else if (ivalue.eq. -2) then
                      fmt(19)(42:42) = '|'
                      fmt(20)(42:42) = 'u'
                  else if (ivalue.eq.  3) then
                      fmt(17)(42:42) = '|'
                      fmt(16)(42:42) = '|'
                      fmt(15)(42:42) = 'u'
                  else if (ivalue.eq. -3) then
                      fmt(19)(42:42) = '|'
                      fmt(20)(42:42) = '|'
                      fmt(21)(42:42) = 'u'
                  else if (ivalue.eq.  4) then
                      fmt(17)(42:42) = '|'
                      fmt(16)(42:42) = '|'
                      fmt(15)(42:42) = '|'
                      fmt(14)(42:42) = 'u'
                  else if (ivalue.eq. -4) then
                      fmt(19)(42:42) = '|'
                      fmt(20)(42:42) = '|'
                      fmt(21)(42:42) = '|'
                      fmt(22)(42:42) = 'u'
                  else if (ivalue.eq.  5) then
                      fmt(17)(42:42) = '|'
                      fmt(16)(42:42) = '|'
                      fmt(15)(42:42) = '|'
                      fmt(14)(42:42) = '|'
                      fmt(13)(42:42) = 'u'
                  else if (ivalue.eq. -5) then
                      fmt(19)(42:42) = '|'
                      fmt(20)(42:42) = '|'
                      fmt(21)(42:42) = '|'
                      fmt(22)(42:42) = '|'
                      fmt(23)(42:42) = 'u'
                  else if (ivalue.eq.  6) then
                      fmt(17)(42:42) = '|'
                      fmt(16)(42:42) = '|'
                      fmt(15)(42:42) = '|'
                      fmt(14)(42:42) = '|'
                      fmt(13)(42:42) = '|'
                      fmt(12)(42:42) = 'u'
                  else if (ivalue.eq. -6) then
                      fmt(19)(42:42) = '|'
                      fmt(20)(42:42) = '|'
                      fmt(21)(42:42) = '|'
                      fmt(22)(42:42) = '|'
                      fmt(23)(42:42) = '|'
                      fmt(24)(42:42) = 'u'
                  else if (ivalue.eq.  7) then
                      fmt(17)(42:42) = '|'
                      fmt(16)(42:42) = '|'
                      fmt(15)(42:42) = '|'
                      fmt(14)(42:42) = '|'
                      fmt(13)(42:42) = '|'
                      fmt(12)(42:42) = '|'
                      fmt(11)(42:42) = 'u'
                  else if (ivalue.eq. -7) then
                      fmt(19)(42:42) = '|'
                      fmt(20)(42:42) = '|'
                      fmt(21)(42:42) = '|'
                      fmt(22)(42:42) = '|'
                      fmt(23)(42:42) = '|'
                      fmt(24)(42:42) = '|'
                      fmt(25)(42:42) = 'u'
                  else if (ivalue.eq.  8) then
                      fmt(17)(42:42) = '|'
                      fmt(16)(42:42) = '|'
                      fmt(15)(42:42) = '|'
                      fmt(14)(42:42) = '|'
                      fmt(13)(42:42) = '|'
                      fmt(12)(42:42) = '|'
                      fmt(11)(42:42) = '|'
                      fmt(10)(42:42) = 'u'
                  else if (ivalue.eq. -8) then
                      fmt(19)(42:42) = '|'
                      fmt(20)(42:42) = '|'
                      fmt(21)(42:42) = '|'
                      fmt(22)(42:42) = '|'
                      fmt(23)(42:42) = '|'
                      fmt(24)(42:42) = '|'
                      fmt(25)(42:42) = 'u'
                  else if (ivalue.eq.  9) then
                      fmt(17)(42:42) = '|'
                      fmt(16)(42:42) = '|'
                      fmt(15)(42:42) = '|'
                      fmt(14)(42:42) = '|'
                      fmt(13)(42:42) = '|'
                      fmt(12)(42:42) = '|'
                      fmt(11)(42:42) = '|'
                      fmt(10)(42:42) = '|'
                      fmt( 9)(42:42) = 'u'
                  else if (ivalue.eq. -9) then
                      fmt(19)(42:42) = '|'
                      fmt(20)(42:42) = '|'
                      fmt(21)(42:42) = '|'
                      fmt(22)(42:42) = '|'
                      fmt(23)(42:42) = '|'
                      fmt(24)(42:42) = '|'
                      fmt(25)(42:42) = '|'
                      fmt(26)(42:42) = '|'
                      fmt(27)(42:42) = 'u'
                  endif
              else if (n_point.eq.13) then
                  if (ivalue.eq.0) then
                      fmt(22)(5:5) = 'u'
                  else if (ivalue.eq.  1) then
                      fmt(21)(5:5) = 'u'
                  else if (ivalue.eq. -1) then
                      fmt(23)(5:5) = 'u'
                  else if (ivalue.eq.  2) then
                      fmt(21)(5:5) = '|'
                      fmt(20)(5:5) = 'u'
                  else if (ivalue.eq. -2) then
                      fmt(23)(5:5) = '|'
                      fmt(24)(5:5) = 'u'
                  else if (ivalue.eq.  3) then
                      fmt(21)(5:5) = '|'
                      fmt(20)(5:5) = '|'
                      fmt(19)(5:5) = 'u'
                  else if (ivalue.eq. -3) then
                      fmt(23)(5:5) = '|'
                      fmt(24)(5:5) = '|'
                      fmt(25)(5:5) = 'u'
                  else if (ivalue.eq.  4) then
                      fmt(21)(5:5) = '|'
                      fmt(20)(5:5) = '|'
                      fmt(19)(5:5) = '|'
                      fmt(18)(5:5) = 'u'
                  else if (ivalue.eq. -4) then
                      fmt(23)(5:5) = '|'
                      fmt(24)(5:5) = '|'
                      fmt(25)(5:5) = '|'
                      fmt(26)(5:5) = 'u'
                  else if (ivalue.eq.  5) then
                      fmt(21)(5:5) = '|'
                      fmt(20)(5:5) = '|'
                      fmt(19)(5:5) = '|'
                      fmt(18)(5:5) = '|'
                      fmt(17)(5:5) = 'u'
                  else if (ivalue.eq. -5) then
                      fmt(23)(5:5) = '|'
                      fmt(24)(5:5) = '|'
                      fmt(25)(5:5) = '|'
                      fmt(26)(5:5) = '|'
                      fmt(27)(5:5) = 'u'
                  else if (ivalue.eq.  6) then
                      fmt(21)(5:5) = '|'
                      fmt(20)(5:5) = '|'
                      fmt(19)(5:5) = '|'
                      fmt(18)(5:5) = '|'
                      fmt(17)(5:5) = '|'
                      fmt(16)(5:5) = 'u'
                  else if (ivalue.eq. -6) then
                      fmt(23)(5:5) = '|'
                      fmt(24)(5:5) = '|'
                      fmt(25)(5:5) = '|'
                      fmt(26)(5:5) = '|'
                      fmt(27)(5:5) = '|'
                      fmt(28)(5:5) = 'u'
                  else if (ivalue.eq.  7) then
                      fmt(21)(5:5) = '|'
                      fmt(20)(5:5) = '|'
                      fmt(19)(5:5) = '|'
                      fmt(18)(5:5) = '|'
                      fmt(17)(5:5) = '|'
                      fmt(16)(5:5) = '|'
                      fmt(15)(5:5) = 'u'
                  else if (ivalue.eq. -7) then
                      fmt(23)(5:5) = '|'
                      fmt(24)(5:5) = '|'
                      fmt(25)(5:5) = '|'
                      fmt(26)(5:5) = '|'
                      fmt(27)(5:5) = '|'
                      fmt(28)(5:5) = '|'
                      fmt(29)(5:5) = 'u'
                  else if (ivalue.eq.  8) then
                      fmt(21)(5:5) = '|'
                      fmt(20)(5:5) = '|'
                      fmt(19)(5:5) = '|'
                      fmt(18)(5:5) = '|'
                      fmt(17)(5:5) = '|'
                      fmt(16)(5:5) = '|'
                      fmt(15)(5:5) = '|'
                      fmt(14)(5:5) = 'u'
                  else if (ivalue.eq. -8) then
                      fmt(23)(5:5) = '|'
                      fmt(24)(5:5) = '|'
                      fmt(25)(5:5) = '|'
                      fmt(26)(5:5) = '|'
                      fmt(27)(5:5) = '|'
                      fmt(28)(5:5) = '|'
                      fmt(29)(5:5) = 'u'
                  else if (ivalue.eq.  9) then
                      fmt(21)(5:5) = '|'
                      fmt(20)(5:5) = '|'
                      fmt(19)(5:5) = '|'
                      fmt(18)(5:5) = '|'
                      fmt(17)(5:5) = '|'
                      fmt(16)(5:5) = '|'
                      fmt(15)(5:5) = '|'
                      fmt(14)(5:5) = '|'
                      fmt(13)(5:5) = 'u'
                  else if (ivalue.eq. -9) then
                      fmt(23)(5:5) = '|'
                      fmt(24)(5:5) = '|'
                      fmt(25)(5:5) = '|'
                      fmt(26)(5:5) = '|'
                      fmt(27)(5:5) = '|'
                      fmt(28)(5:5) = '|'
                      fmt(29)(5:5) = '|'
                      fmt(30)(5:5) = '|'
                      fmt(31)(5:5) = 'u'
                  endif
              else if (n_point.eq.14) then
                  if (ivalue.eq.0) then
                      fmt(22)(16:16) = 'u'
                  else if (ivalue.eq.  1) then
                      fmt(21)(16:16) = 'u'
                  else if (ivalue.eq. -1) then
                      fmt(23)(16:16) = 'u'
                  else if (ivalue.eq.  2) then
                      fmt(21)(16:16) = '|'
                      fmt(20)(16:16) = 'u'
                  else if (ivalue.eq. -2) then
                      fmt(23)(16:16) = '|'
                      fmt(24)(16:16) = 'u'
                  else if (ivalue.eq.  3) then
                      fmt(21)(16:16) = '|'
                      fmt(20)(16:16) = '|'
                      fmt(19)(16:16) = 'u'
                  else if (ivalue.eq. -3) then
                      fmt(23)(16:16) = '|'
                      fmt(24)(16:16) = '|'
                      fmt(25)(16:16) = 'u'
                  else if (ivalue.eq.  4) then
                      fmt(21)(16:16) = '|'
                      fmt(20)(16:16) = '|'
                      fmt(19)(16:16) = '|'
                      fmt(18)(16:16) = 'u'
                  else if (ivalue.eq. -4) then
                      fmt(23)(16:16) = '|'
                      fmt(24)(16:16) = '|'
                      fmt(25)(16:16) = '|'
                      fmt(26)(16:16) = 'u'
                  else if (ivalue.eq.  5) then
                      fmt(21)(16:16) = '|'
                      fmt(20)(16:16) = '|'
                      fmt(19)(16:16) = '|'
                      fmt(18)(16:16) = '|'
                      fmt(17)(16:16) = 'u'
                  else if (ivalue.eq. -5) then
                      fmt(23)(16:16) = '|'
                      fmt(24)(16:16) = '|'
                      fmt(25)(16:16) = '|'
                      fmt(26)(16:16) = '|'
                      fmt(27)(16:16) = 'u'
                  else if (ivalue.eq.  6) then
                      fmt(21)(16:16) = '|'
                      fmt(20)(16:16) = '|'
                      fmt(19)(16:16) = '|'
                      fmt(18)(16:16) = '|'
                      fmt(17)(16:16) = '|'
                      fmt(16)(16:16) = 'u'
                  else if (ivalue.eq. -6) then
                      fmt(23)(16:16) = '|'
                      fmt(24)(16:16) = '|'
                      fmt(25)(16:16) = '|'
                      fmt(26)(16:16) = '|'
                      fmt(27)(16:16) = '|'
                      fmt(28)(16:16) = 'u'
                  else if (ivalue.eq.  7) then
                      fmt(21)(16:16) = '|'
                      fmt(20)(16:16) = '|'
                      fmt(19)(16:16) = '|'
                      fmt(18)(16:16) = '|'
                      fmt(17)(16:16) = '|'
                      fmt(16)(16:16) = '|'
                      fmt(15)(16:16) = 'u'
                  else if (ivalue.eq. -7) then
                      fmt(23)(16:16) = '|'
                      fmt(24)(16:16) = '|'
                      fmt(25)(16:16) = '|'
                      fmt(26)(16:16) = '|'
                      fmt(27)(16:16) = '|'
                      fmt(28)(16:16) = '|'
                      fmt(29)(16:16) = 'u'
                  else if (ivalue.eq.  8) then
                      fmt(21)(16:16) = '|'
                      fmt(20)(16:16) = '|'
                      fmt(19)(16:16) = '|'
                      fmt(18)(16:16) = '|'
                      fmt(17)(16:16) = '|'
                      fmt(16)(16:16) = '|'
                      fmt(15)(16:16) = '|'
                      fmt(14)(16:16) = 'u'
                  else if (ivalue.eq. -8) then
                      fmt(23)(16:16) = '|'
                      fmt(24)(16:16) = '|'
                      fmt(25)(16:16) = '|'
                      fmt(26)(16:16) = '|'
                      fmt(27)(16:16) = '|'
                      fmt(28)(16:16) = '|'
                      fmt(29)(16:16) = 'u'
                  else if (ivalue.eq.  9) then
                      fmt(21)(16:16) = '|'
                      fmt(20)(16:16) = '|'
                      fmt(19)(16:16) = '|'
                      fmt(18)(16:16) = '|'
                      fmt(17)(16:16) = '|'
                      fmt(16)(16:16) = '|'
                      fmt(15)(16:16) = '|'
                      fmt(14)(16:16) = '|'
                      fmt(13)(16:16) = 'u'
                  else if (ivalue.eq. -9) then
                      fmt(23)(16:16) = '|'
                      fmt(24)(16:16) = '|'
                      fmt(25)(16:16) = '|'
                      fmt(26)(16:16) = '|'
                      fmt(27)(16:16) = '|'
                      fmt(28)(16:16) = '|'
                      fmt(29)(16:16) = '|'
                      fmt(30)(16:16) = '|'
                      fmt(31)(16:16) = 'u'
                  endif
              else if (n_point.eq.15) then
                  if (ivalue.eq.0) then
                      fmt(22)(27:27) = 'u'
                  else if (ivalue.eq.  1) then
                      fmt(21)(27:27) = 'u'
                  else if (ivalue.eq. -1) then
                      fmt(23)(27:27) = 'u'
                  else if (ivalue.eq.  2) then
                      fmt(21)(27:27) = '|'
                      fmt(20)(27:27) = 'u'
                  else if (ivalue.eq. -2) then
                      fmt(23)(27:27) = '|'
                      fmt(24)(27:27) = 'u'
                  else if (ivalue.eq.  3) then
                      fmt(21)(27:27) = '|'
                      fmt(20)(27:27) = '|'
                      fmt(19)(27:27) = 'u'
                  else if (ivalue.eq. -3) then
                      fmt(23)(27:27) = '|'
                      fmt(24)(27:27) = '|'
                      fmt(25)(27:27) = 'u'
                  else if (ivalue.eq.  4) then
                      fmt(21)(27:27) = '|'
                      fmt(20)(27:27) = '|'
                      fmt(19)(27:27) = '|'
                      fmt(18)(27:27) = 'u'
                  else if (ivalue.eq. -4) then
                      fmt(23)(27:27) = '|'
                      fmt(24)(27:27) = '|'
                      fmt(25)(27:27) = '|'
                      fmt(26)(27:27) = 'u'
                  else if (ivalue.eq.  5) then
                      fmt(21)(27:27) = '|'
                      fmt(20)(27:27) = '|'
                      fmt(19)(27:27) = '|'
                      fmt(18)(27:27) = '|'
                      fmt(17)(27:27) = 'u'
                  else if (ivalue.eq. -5) then
                      fmt(23)(27:27) = '|'
                      fmt(24)(27:27) = '|'
                      fmt(25)(27:27) = '|'
                      fmt(26)(27:27) = '|'
                      fmt(27)(27:27) = 'u'
                  else if (ivalue.eq.  6) then
                      fmt(21)(27:27) = '|'
                      fmt(20)(27:27) = '|'
                      fmt(19)(27:27) = '|'
                      fmt(18)(27:27) = '|'
                      fmt(17)(27:27) = '|'
                      fmt(16)(27:27) = 'u'
                  else if (ivalue.eq. -6) then
                      fmt(23)(27:27) = '|'
                      fmt(24)(27:27) = '|'
                      fmt(25)(27:27) = '|'
                      fmt(26)(27:27) = '|'
                      fmt(27)(27:27) = '|'
                      fmt(28)(27:27) = 'u'
                  else if (ivalue.eq.  7) then
                      fmt(21)(27:27) = '|'
                      fmt(20)(27:27) = '|'
                      fmt(19)(27:27) = '|'
                      fmt(18)(27:27) = '|'
                      fmt(17)(27:27) = '|'
                      fmt(16)(27:27) = '|'
                      fmt(15)(27:27) = 'u'
                  else if (ivalue.eq. -7) then
                      fmt(23)(27:27) = '|'
                      fmt(24)(27:27) = '|'
                      fmt(25)(27:27) = '|'
                      fmt(26)(27:27) = '|'
                      fmt(27)(27:27) = '|'
                      fmt(28)(27:27) = '|'
                      fmt(29)(27:27) = 'u'
                  else if (ivalue.eq.  8) then
                      fmt(21)(27:27) = '|'
                      fmt(20)(27:27) = '|'
                      fmt(19)(27:27) = '|'
                      fmt(18)(27:27) = '|'
                      fmt(17)(27:27) = '|'
                      fmt(16)(27:27) = '|'
                      fmt(15)(27:27) = '|'
                      fmt(14)(27:27) = 'u'
                  else if (ivalue.eq. -8) then
                      fmt(23)(27:27) = '|'
                      fmt(24)(27:27) = '|'
                      fmt(25)(27:27) = '|'
                      fmt(26)(27:27) = '|'
                      fmt(27)(27:27) = '|'
                      fmt(28)(27:27) = '|'
                      fmt(29)(27:27) = 'u'
                  else if (ivalue.eq.  9) then
                      fmt(21)(27:27) = '|'
                      fmt(20)(27:27) = '|'
                      fmt(19)(27:27) = '|'
                      fmt(18)(27:27) = '|'
                      fmt(17)(27:27) = '|'
                      fmt(16)(27:27) = '|'
                      fmt(15)(27:27) = '|'
                      fmt(14)(27:27) = '|'
                      fmt(13)(27:27) = 'u'
                  else if (ivalue.eq. -9) then
                      fmt(23)(27:27) = '|'
                      fmt(24)(27:27) = '|'
                      fmt(25)(27:27) = '|'
                      fmt(26)(27:27) = '|'
                      fmt(27)(27:27) = '|'
                      fmt(28)(27:27) = '|'
                      fmt(29)(27:27) = '|'
                      fmt(30)(27:27) = '|'
                      fmt(31)(27:27) = 'u'
                  endif
              else if (n_point.eq.16) then
                  if (ivalue.eq.0) then
                      fmt(22)(38:38) = 'u'
                  else if (ivalue.eq.  1) then
                      fmt(21)(38:38) = 'u'
                  else if (ivalue.eq. -1) then
                      fmt(23)(38:38) = 'u'
                  else if (ivalue.eq.  2) then
                      fmt(21)(38:38) = '|'
                      fmt(20)(38:38) = 'u'
                  else if (ivalue.eq. -2) then
                      fmt(23)(38:38) = '|'
                      fmt(24)(38:38) = 'u'
                  else if (ivalue.eq.  3) then
                      fmt(21)(38:38) = '|'
                      fmt(20)(38:38) = '|'
                      fmt(19)(38:38) = 'u'
                  else if (ivalue.eq. -3) then
                      fmt(23)(38:38) = '|'
                      fmt(24)(38:38) = '|'
                      fmt(25)(38:38) = 'u'
                  else if (ivalue.eq.  4) then
                      fmt(21)(38:38) = '|'
                      fmt(20)(38:38) = '|'
                      fmt(19)(38:38) = '|'
                      fmt(18)(38:38) = 'u'
                  else if (ivalue.eq. -4) then
                      fmt(23)(38:38) = '|'
                      fmt(24)(38:38) = '|'
                      fmt(25)(38:38) = '|'
                      fmt(26)(38:38) = 'u'
                  else if (ivalue.eq.  5) then
                      fmt(21)(38:38) = '|'
                      fmt(20)(38:38) = '|'
                      fmt(19)(38:38) = '|'
                      fmt(18)(38:38) = '|'
                      fmt(17)(38:38) = 'u'
                  else if (ivalue.eq. -5) then
                      fmt(23)(38:38) = '|'
                      fmt(24)(38:38) = '|'
                      fmt(25)(38:38) = '|'
                      fmt(26)(38:38) = '|'
                      fmt(27)(38:38) = 'u'
                  else if (ivalue.eq.  6) then
                      fmt(21)(38:38) = '|'
                      fmt(20)(38:38) = '|'
                      fmt(19)(38:38) = '|'
                      fmt(18)(38:38) = '|'
                      fmt(17)(38:38) = '|'
                      fmt(16)(38:38) = 'u'
                  else if (ivalue.eq. -6) then
                      fmt(23)(38:38) = '|'
                      fmt(24)(38:38) = '|'
                      fmt(25)(38:38) = '|'
                      fmt(26)(38:38) = '|'
                      fmt(27)(38:38) = '|'
                      fmt(28)(38:38) = 'u'
                  else if (ivalue.eq.  7) then
                      fmt(21)(38:38) = '|'
                      fmt(20)(38:38) = '|'
                      fmt(19)(38:38) = '|'
                      fmt(18)(38:38) = '|'
                      fmt(17)(38:38) = '|'
                      fmt(16)(38:38) = '|'
                      fmt(15)(38:38) = 'u'
                  else if (ivalue.eq. -7) then
                      fmt(23)(38:38) = '|'
                      fmt(24)(38:38) = '|'
                      fmt(25)(38:38) = '|'
                      fmt(26)(38:38) = '|'
                      fmt(27)(38:38) = '|'
                      fmt(28)(38:38) = '|'
                      fmt(29)(38:38) = 'u'
                  else if (ivalue.eq.  8) then
                      fmt(21)(38:38) = '|'
                      fmt(20)(38:38) = '|'
                      fmt(19)(38:38) = '|'
                      fmt(18)(38:38) = '|'
                      fmt(17)(38:38) = '|'
                      fmt(16)(38:38) = '|'
                      fmt(15)(38:38) = '|'
                      fmt(14)(38:38) = 'u'
                  else if (ivalue.eq. -8) then
                      fmt(23)(38:38) = '|'
                      fmt(24)(38:38) = '|'
                      fmt(25)(38:38) = '|'
                      fmt(26)(38:38) = '|'
                      fmt(27)(38:38) = '|'
                      fmt(28)(38:38) = '|'
                      fmt(29)(38:38) = 'u'
                  else if (ivalue.eq.  9) then
                      fmt(21)(38:38) = '|'
                      fmt(20)(38:38) = '|'
                      fmt(19)(38:38) = '|'
                      fmt(18)(38:38) = '|'
                      fmt(17)(38:38) = '|'
                      fmt(16)(38:38) = '|'
                      fmt(15)(38:38) = '|'
                      fmt(14)(38:38) = '|'
                      fmt(13)(38:38) = 'u'
                  else if (ivalue.eq. -9) then
                      fmt(23)(38:38) = '|'
                      fmt(24)(38:38) = '|'
                      fmt(25)(38:38) = '|'
                      fmt(26)(38:38) = '|'
                      fmt(27)(38:38) = '|'
                      fmt(28)(38:38) = '|'
                      fmt(29)(38:38) = '|'
                      fmt(30)(38:38) = '|'
                      fmt(31)(38:38) = 'u'
                  endif
              endif
          end do
      end do
c
c  visualize matrix
c
      do i=1,31
          if (i.eq.10) then
              write (lulist,'("//fmt(i)//",12x,4f8.3)')
     >        matrix(1,1),matrix(1,2),matrix(1,3),matrix(1,4)
          else if (i.eq.14) then
              write (lulist,'("//fmt(i)//", 8x,4f8.3)')
     >        matrix(2,1),matrix(2,2),matrix(2,3),matrix(2,4)
          else if (i.eq.18) then
              write (lulist,'("//fmt(i)//", 4x,4f8.3)')
     >        matrix(3,1),matrix(3,2),matrix(3,3),matrix(3,4)
          else if (i.eq.22) then
              write (lulist,'("//fmt(i)//"    ,4f8.3)')
     >        matrix(4,1),matrix(4,2),matrix(4,3),matrix(4,4)
          else
              write (lulist,'("//fmt(i)//")')
          endif
c
          write (1,'("//fmt(i)//")')
c
      end do
c
c  next matrix
c
      go to 10
c
999   return
c
      end
