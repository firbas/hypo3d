c
      subroutine cov_matrix
c
c*****************************************************************************
c
c  subroutine COV_MATRIX
c
c-----------------------------------------------------------------------------
c
c  purpose:
c
c     evaluated covariance matrix for dumped least squares
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
c     call cov_matrix
c
c----------------------------------------------------------------------------
c
c  external references:
c
c     none
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
c  formal parameters  ...  none
c
c
c  local parameters  ...  none
c
c
c  global parameters
c
         include 'param.fi'
c
c  local variables
c
         integer         i,j,k,l,m
         real*8          sum8_co
c
c  global variables
c
         real*8          c(4,4)        !inversion of Hessian matrix
         common /it2/    c
c
         real            rmsres
         real            rmsres_co
         real            co(4,4)
         common /cov/    rmsres,rmsres_co,co
c
         logical         hyr
         real            trec (nrec_max)
         real            wt   (nrec_max)
         real            avwt
         common /hyp/    hyr,trec,wt,avwt
c
         real              tcal(nrec_max)
         real              xc  (4,nrec_max)
         common /cal_time/ tcal,xc
c
         integer             nrec            !no. of arrivals
         common /rec/        nrec
c
c  functions  ...  none
c
c
c  *******************
c  end of declarations
c  *******************
c
c         write(*,'(1x,I5,"c:",4F14.8)') (i,c(:,i), i=1,4)
c=============================================================================
         if (hyr) then
c
            do i=1,4
               do m=1,4
c
                  sum8_co=0.0
                  do l=1,nrec
                     do j=1,4
                        do k=1,4
                           sum8_co=sum8_co+c(i,j)*xc(j,l)*wt(l)
     >                             *wt(l)*xc(k,l)*c(m,k)
                        end do  !k
                     end do     !j
                  end do        !l
c
c  element i,m of cov. matrix
                  co(i,m)=real(sum8_co/(avwt*avwt),4)
               end do  !m
            end do     !i
c
c=============================================================================
         else       ! hyr .false.
c
            do i=1,4
               do m=1,4
c
                  sum8_co=0.0
                  do l=1,nrec
                     do j=1,4
                        do k=1,4
                           sum8_co=sum8_co+c(i,j)*xc(j,l)
     >                             *wt(l)*xc(k,l)*c(m,k)
                        end do  !k
                     end do     !j
                  end do        !l
c
c  element i,m of cov. matrix
c ---------------------------------------------------------------------
c 2020-02-01 pz v10.77
c compensate weights normed by avwt
                  co(i,m)=real(sum8_co/avwt*rmsres_co,4)
c ---------------------------------------------------------------------
               end do  !m
            end do     !i
c
         endif    !hyr
c ---------------------------------------------------------------------
c         write(*,'(1x,I5,"co:",4F14.8)') (i,co(:,i), i=1,4)
         return
      end subroutine cov_matrix
