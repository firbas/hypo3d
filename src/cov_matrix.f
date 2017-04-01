c
C$ema /rec/,/hyp/
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
		real            sum4_re
		real            sum4_id
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
		real            id(4,4)
		real            re(4,4)
		real            rmsres
		real            rmsres_co
		common /cov/    co,rmsres,rmsres_co,id,re
c
		real            trec (nrec_max)
		real            wt   (nrec_max)
		real            avwt
		common /hyp/    trec,wt,avwt
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
      integer           i0
      logical           fix_depth
      logical           fix_surface
      common /srfc/     fix_surface,fix_depth,i0
c
      logical           fix_x
      logical           fix_y
      logical           fix_otime
      common /f_mode/   fix_x,fix_y,fix_otime
c
      logical           scan_depth
		real            scan_start
		real            scan_end
		real            scan_step
		common /scan/   scan_depth,scan_start,scan_end,scan_step
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
              sum4_id=0.0
              sum4_re=0.0
              do l=1,nrec
                  do j=1,4
                      do k=1,4
                          sum4_co=sum4_co+c(i,j)*xc(j,l)*xc(k,l)*c(m,k)
     >                        *wt(l)/avwt
                          if (i.eq.j) then
                              sum4_id=sum4_id+xc(j,l)*xc(k,l)*c(m,k)
     >                            *wt(l)/avwt
                          endif
c
                          if (m.eq.k) then
                              sum4_re=sum4_re+c(i,j)*xc(j,l)*xc(k,l)
     >                            *wt(l)/avwt
                          endif
                      end do
                  end do
              end do
c
c  element i,m of cov. matrix
c
              co(i,m)=sum4_co*rmsres_co
              id(i,m)=sum4_id
              re(i,m)=sum4_re
          end do
      end do
c
c  modify resolution & info density matrix
c  for fixed coordinates
c
      if (fix_x) then
          do i=1,4
              re(1,i)=0.0
              id(1,i)=0.0
              re(i,1)=0.0
              id(i,1)=0.0
          end do
          re(1,1)=1.0
          id(1,1)=1.0
      endif
c
      if (fix_y) then
          do i=1,4
              re(2,i)=0.0
              id(2,i)=0.0
              re(i,2)=0.0
              id(i,2)=0.0
          end do
          re(2,2)=1.0
          id(2,2)=1.0
      endif
c
      if (fix_depth .or. scan_depth) then
          do i=1,4
              re(3,i)=0.0
              id(3,i)=0.0
              re(i,3)=0.0
              id(i,3)=0.0
          end do
          re(3,3)=1.0
          id(3,3)=1.0
      endif
c
      if (fix_otime) then
          do i=1,4
              re(4,i)=0.0
              id(4,i)=0.0
              re(i,4)=0.0
              id(i,4)=0.0
          end do
          re(4,4)=1.0
          id(4,4)=1.0
      endif
c
      return
      end
