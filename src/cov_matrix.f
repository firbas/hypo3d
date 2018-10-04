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
		real            sum4_co
c
c  global variables
c
		real*8          c(4,4)     !Hessian matrix resp. inv. Hess. m.
		real*8          b(4)        !vector of right side
		real*8          det          !determinant of matrix c
		real            scale(4)      !scale vector for Hessian matrix
		common /it2/    c,b,det,scale
c
		real            co(4,4)
		real            rmsres
		real            rmsres_co
		common /cov/    co,rmsres,rmsres_co
c
		real            trec (nrec_max)
		real            wt   (nrec_max)
		real            avwt
		common /hyp/    trec,wt,avwt
c
                logical             hyr
                real                wt1(nrec_max)
                common /wt_1/       hyr,wt1
c
		real              tcal(nrec_max)
                real              xc  (4,nrec_max)
                common /cal_time/ tcal,xc
c
		integer             nrec            !no. of arrivals
		real                xstat(nStation) !\
		real                ystat(nStation) ! >coordinates of stations
		real                zstat(nStation) !/
		real                dly(nStation)   !stations delays for surf. events
		common /rec/        nrec,xstat,ystat,zstat,dly
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
      do i=1,4
          do m=1,4
c
c  initialize summ
c
              sum4_co=0.0
              do l=1,nrec
                  do j=1,4
                      do k=1,4
                          sum4_co=sum4_co+c(i,j)*xc(j,l)*wt(l)
     >                            *wt(l)*xc(k,l)*c(m,k)
                      end do
                  end do
              end do
c
c  element i,m of cov. matrix
c
c ---------------------------------------------------------------------
c 2018-09 10.69
            if (hyr) then
              co(i,m)=sum4_co/(avwt*avwt)
c              co(i,m)=sum4_co
            else
              co(i,m)=sum4_co*rmsres_co
            endif
c ---------------------------------------------------------------------
          end do
      end do
c
      return
      end
