c
      subroutine iter_2
c
c*****************************************************************************
c
c  subroutine ITER_2
c
c-----------------------------------------------------------------------------
c
c  purpose:
c
c     evaluate matrix C, vector b, eigenvalues of C
c     matrix, damped matrix C, modified matrix C for fixed coord.,
c     scaled matrix C, inverted matrix C
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
c     call iter_2
c
c----------------------------------------------------------------------------
c
c  external references:
c
c     EIGEN               ssp subroutine
c     MINV                ssp subroutine
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
         real*8  damp_level
         parameter (damp_level=1.00d-03)
         real*8  kappa
         parameter (kappa=1.00d-08)
c
c  global parameters
c
         include 'param.fi'
c
c  local variables
c
         integer i,j,k
         real*8  sum8
         integer*4  l4(4) !for MINV
         integer*4  m4(4) !for MINV
         real*8  c1(16)
         real*8  r(4,4)  !work array for EIGEN
c
c  global variables
c
         real            t0
         common /otime/  t0
c
         real*8           sigma(4)
         common /sigm/    sigma
c
         real              tcal(nrec_max)
         real              xc(4,nrec_max)
         common /cal_time/ tcal,xc
c
         logical         hyr
         real            trec (nrec_max)
         real            wt   (nrec_max)
         common /hyp/    hyr,trec,wt
c
         logical         t0_norm
         logical         endit
         common /it1/    t0_norm,endit
c
         logical         fix_surface
         common /srfc/   fix_surface
c
         logical         fix_depth
         logical         fix_x
         logical         fix_y
         logical         fix_otime
         common /fix_mode/ fix_depth,fix_x,fix_y,fix_otime
c
         real*8          c(4,4)        !Hessian matrix resp. inv. Hess. m.
         real*8          b(4)          !vector of right side
         real*8          det           !determinant of matrix c
         real*8          scaling(4)    !scale vector for Hessian matrix
         common /it2/    c,b,det,scaling
c
         integer         nrec
         common /rec/    nrec
c
         logical         scan_depth
         real            scan_start
         real            scan_end
         real            scan_step
         common /scan/   scan_depth,scan_start,scan_end,scan_step
c
c  *******************
c  end of declarations
c  *******************
c
c
c  evaluate vector b ... right side times matrix A
c
         do j=1,4
            sum8=0.0
            do i=1,nrec
               if (hyr) then
                  sum8=sum8+xc(j,i)*wt(i)*(trec(i)-t0-tcal(i))*wt(i)
               else
                  sum8=sum8+xc(j,i)*wt(i)*(trec(i)-t0-tcal(i))
               endif    !hyr
            end do
            b(j)=sum8
         end do
c
c  compute matrix C
c
         do i=1,4
            do j=1,4
               sum8=0.0
               do k=1,nrec
                  if (hyr) then
                     sum8=sum8+xc(i,k)*wt(k)*xc(j,k)*wt(k)
                  else
                     sum8=sum8+xc(i,k)*wt(k)*xc(j,k)
                  endif    !hyr
               end do
               c(i,j)=sum8
            end do
         end do
c
      if (.not.endit) then
c
c  store upper triangular portion of symmetric matrix C to vector C1
c
         k=0
         do j=1,4
            do i=1,4
               if(i.le.j) then
                  k=k+1
                  c1(k)=c(i,j)
               endif
            end do
         end do
c
c  evaluate eigenvalues 
c  EIGEN storage mode code = 1 (COMPUTE EIGENVALUES ONLY)
c
         call EIGEN(c1,r,4,1)
c
c  eigenvalues are stored on diagonal in decreasing order
c  i.e. minimal value is in last element
c
c  test: minimal value of eigenvalues greater then damp level?
c
            if (abs(c1(10)).gt.damp_level) then
c
c  yes ... no damping
c
               do i=1,4
                  sigma(i)=0.0
               end do
            else
c
c  no ... switch on damping
c
               do i=1,4
                  sigma(i)=damp_level
               end do
            endif
c
c  set damped matrix C
c
            do i=1,4
               c(i,i)=c(i,i)+sigma(i)
            end do
c
c  modify matrix C & vector b
c  for fixed coordinates
c
            if (fix_x) then
               do i=1,4
                  c(1,i)=0.0
                  c(i,1)=0.0
               end do
               c(1,1)=1.0
               b(1)=0.0
            endif
c
            if (fix_y) then
               do i=1,4
                  c(2,i)=0.0
                  c(i,2)=0.0
               end do
               c(2,2)=1.0
               b(2)=0.0
            endif
c
            if (scan_depth) then
               do i=1,4
                  c(3,i)=0.0
                  c(i,3)=0.0
               end do
               c(3,3)=1.0
               b(3)=0.0
            endif
c
            if (fix_depth) then
               do i=1,4
                  c(3,i)=0.0
                  c(i,3)=0.0
               end do
               c(3,3)=1.0
               b(3)=0.0
            endif
c
            if (fix_otime) then
               do i=1,4
                  c(4,i)=0.0
                  c(i,4)=0.0
               end do
               c(4,4)=1.0
               b(4)=0.0
            endif
c
c  scaled matrix C
c
            do j=1,4
               scaling(j)=0.
               do i=1,4
                  scaling(j)=scaling(j)+c(i,j)**2
               end do
               scaling(j)=sqrt(scaling(j))
            end do
c
            do i=1,4
               do j=1,4
                  c(i,j)=c(i,j)/scaling(j)
               end do
            end do
c
c  matrix inversion
         call minv(c,4,det,l4,m4)
c
      endif ! .not.endit
c
      return
c
      end subroutine iter_2
