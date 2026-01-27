c subroutine to compute error ellipse / error ellipsoid

      subroutine eel

      implicit none

      real*8  kappa
      parameter (kappa=1.00d-08)

c  global parameters

         include 'param.fi'

c  local variables

      integer i,j,k
      real*8  c1(16)
      real*8  r(4,4)   !eigenvector matrix
      real*8  e(4)     !eigenvalues

c  global variables

      logical         fix_surface
      common /srfc/   fix_surface

      logical         fix_depth
      logical         fix_x
      logical         fix_y
      logical         fix_otime
      common /fix_mode/ fix_depth,fix_x,fix_y,fix_otime

      real*8          c(4,4)    !matrix of normal equations
      common /it2/    c

      logical         ee_nan(4) !flag array for error ellipse axes
      common /nan/    ee_nan

      logical      ee3          !flag controlling error estimation 
      real         model_error
      real         reading_error
      common /err/ model_error,reading_error,ee3

      real            rmsres
      real            rmsres_co
      real            co(4,4)
      common /cov/    rmsres,rmsres_co,co

      logical         hyr        !weighting mode flag
      real            trec (nrec_max)
      real            wt   (nrec_max)
      real            avwt       !average weight
      common /hyp/    hyr,trec,wt,avwt

c ===========================================================================
c 2019-05-31 pz v10.75

c In the case of coordinate fixation, the calculation of the covariance
c matrix is not reduced, except in the following cases:
      if (fix_surface .or. (fix_depth .and. .not. ee3 )) then
         do i=1,4
            c(3,i)=0.0
            c(i,3)=0.0
         end do
         c(3,3)=1.0
      endif

      if (fix_otime) then
         do i=1,4
            c(4,i)=0.0
            c(i,4)=0.0
         end do
         c(4,4)=1.0
      endif
c Test the error ellipse solvability before calculating the covariance 
c matrix using normal matrix eigenvector factorization.

c  store upper triangular portion of symmetric matrix C to vector C1
      k=0
      do j=1,4
         do i=1,4
            if(i.le.j) then
               k=k+1
               c1(k)=c(i,j)
            endif
         end do
      end do

c  EIGEN storage mode code = 0 (COMPUTE EIGENVALUES AND EIGENVECTORS)
      call EIGEN(c1,r,4,0)

      e(1)=c1(1)
      e(2)=c1(3)
      e(3)=c1(6)
      e(4)=c1(10)

C      write(*,'(1x,"Eigenvalues of C: ",4F14.8)') (e(i), i=1,4)
C      ! egenvectors are stored column-wise in r
C      write(*,'(1x,I5,"::",4F14.8)') (i,r(:,i), i=1,4)

c Compute inversion of c matrix
c using eigenvectors in matrix r and eigenvalues in e
c e ~ lambda, r ~ V, then C_inv = V * Lambda_inv * V_transpose
c Index i=1,4 corresponds to x, y, z, t and
c index j=1,4 corresponds to eigenvalues/vectors of the C matrix.
c Test if any eigenvalue is too small or negative
c and exclude it from inversion.

      do j=1,4
         do i=1,4
            c(i,j)=0d0
         end do
      end do
      do k=1,4
         if (e(k)/e(1).gt.kappa) then 
            do j=1,4
               do i=1,4
                  c(i,j)=c(i,j)+r(i,k)*(1.0/e(k))*r(j,k)
               end do
            end do
         endif
      end do

C      write(*,'(1x,"Covariance matrix C_inv:")')
C      write(*,'(1x,I5,"::",4F14.8)') (i,c(:,i), i=1,4)

c Set ee_nan flags for coordinates that was not solvable in error ell.
      do i=1,4
         if (c(i,i).le.kappa) then
            ee_nan(i) = .true.
         else
            ee_nan(i) = .false.
         endif
      end do
          
      write(*,'(1x,"ee_nan: ",4L2)') (ee_nan(i), i=1,4) 

      if (hyr) then
         do i=1,4
            do j=1,4
               co(i,j)=real(c(i,j)/(avwt*avwt),4)
            end do
         end do
      else
         do i=1,4
            do j=1,4
c compensate weights normed by avwt
               co(i,j)=real(c(i,j)/avwt*rmsres_co,4)
            end do
         end do
      endif    !hyr

      return
      end subroutine eel
