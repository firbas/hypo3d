c
C$ema /rec/,/stmod/,/hyp/
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
		real*8  lambda
		parameter (damp_level=1.00d-03)
		parameter (lambda    =1.00d+08)
c
c  global parameters
c
      include 'param.fi'
c
c  local variables
c
		integer i,j,k
		real*8  sum8
		integer*4  l4(4)
		integer*4  m4(4)
		real*8  c1(16)
		real*8  r(16)
		real    delay
		real    coef
c
c  global variables
c
		integer         year
		integer         month
		integer         day
		integer         hour
		integer         minute
      real            t0
      common /otime/  year,month,day,hour,minute,t0
c
      real*8           sigma(4)
      common /sigm/    sigma
c
      real              tcal(nrec_max)
      real              xc(4,nrec_max)
      common /cal_time/ tcal,xc
c
      character*1     type (nrec_max)
      common /chrec/  type
c
      integer         key  (nrec_max)
      common /stmod/  key
c
      real            trec (nrec_max)
      real            wt   (nrec_max)
      real            avwt
      common /hyp/    trec,wt,avwt
c
      real            cep(3)
      integer         no_valid_arrivals
      logical         t0_norm
      logical         endit
      common /it1/    t0_norm,cep,no_valid_arrivals,endit
c
      integer         i0
      logical         fix_depth
      logical         fix_surface
      common /srfc/   fix_surface,fix_depth,i0
c
      logical         fix_x
      logical         fix_y
      logical         fix_otime
      common /f_mode/ fix_x,fix_y,fix_otime
c
      real*8          c(4,4)
      real*8          b(4)
      real*8          det
      real            scale(4)
      common /it2/    c,b,det,scale
c
      real            xstat (nStation)
      real            ystat (nStation)
      real            zstat (nStation)
      real            dly   (nStation)
      integer         nrec
      common /rec/    nrec,xstat,ystat,zstat,dly
c
		logical         scan_depth
		real            scan_start
		real            scan_end
		real            scan_step
		common /scan/   scan_depth,scan_start,scan_end,scan_step
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
c
c  evaluate vector b ... right side times matrix A
c
      do j=1,4
          sum8=0.0
              do i=1,nrec
                  if (cep(3).lt.surf_ev) then
c
c  model for surface event ... with station delays
c
                      delay=dly(key(i))
                  else
                      delay=0.0
                  endif
c
                  if (type(i).eq.'S') then
                      coef=p_over_s
                  else
                      coef=1.
                  endif
                  sum8=sum8+xc(j,i)*(trec(i)-t0-tcal(i)-coef*delay)*
     >                wt(i)/avwt
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
                  sum8=sum8+xc(i,k)*xc(j,k)*wt(k)/avwt
              end do
              c(i,j)=sum8
          end do
      end do
c
c  store upper triangual part of symmetric matrix C to vector C1
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
c
      call EIGEN(c1,r,4,1)
c
c  eigenvalues are stored on diagonal in decreasing order
c  i.e. minimal value is in last element
c
      if (.not.endit) then
cc    if (.true.    ) then
c
c  test: minimal value of eigenvalues greater then damp level?
c
          if (c1(10).gt.damp_level) then
c
c  yes ... no damping
c
cc        if (c1(1)/c1(10).lt.lambda) then
              do i=1,4
                  sigma(i)=0.0
              end do
          else
c
c  no ... switch on damping
c
              do i=1,4
                  sigma(i)=damp_level
cc                sigma(i)=c1(1)/lambda
              end do
          endif
c
c  set damped matrix C
c
          do i=1,4
              c(i,i)=c(i,i)+sigma(i)
          end do
      endif
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
      if (fix_depth .or. scan_depth) then
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
      if (.not.endit) then
c
d    write (lulist,'(/" Matrix C")')
d    write (lulist,'(4f18.8)') c
c
c  scaled matrix C
c
          do j=1,4
              scale(j)=0.
              do i=1,4
                  scale(j)=scale(j)+c(i,j)**2
              end do
              scale(j)=sqrt(scale(j))
          end do
c
d        write (lulist,'(/" Scale factor")')
d        write (lulist,'(4f18.8)') scale
c
          do i=1,4
              do j=1,4
                  c(i,j)=c(i,j)/scale(j)
              end do
          end do
c
d         write (lulist,'(/" Matrix C - scaled")')
d         write (lulist,'(4f18.8)') c
d         write (lulist,'(/"Vector b")')
d         write (lulist,'(4f18.8)') b
c
      endif
c
c  matrix inversion
c
      call minv(c,4,det,l4,m4)
c
d     write (lulist,'(/" Matrix C - inverted")')
d     write (lulist,'(4(4f18.8,/))') c
d     write (lulist,'(" Det = ",f18.8,/)') det
c
      if (endit) then
cc    if (.false.) then
          if (c1(10).lt.1.e-07 .or. det.eq.0.0) then
c
c  no error estimation
c
              sigma(1)=1.0
              det=1.0
          else
              sigma(1)=0.0
          endif
      endif
c
      return
      end
