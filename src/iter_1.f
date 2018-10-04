c
		subroutine iter_1
c
c*****************************************************************************
c
c  subroutine ITER_1
c
c-----------------------------------------------------------------------------
c
c  purpose:
c
c     normalize origin time, evaluate rms of res., rms of res. for cov. matrix
c     evaluation
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
c     call iter_1
c
c----------------------------------------------------------------------------
c
c  external references:
c
c     none
c
c----------------------------------------------------------------------------
c
c  programmed:  87-02  01.00  mw
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
		include 'pname.fi'
		include 'error.fi'
c
c  local variables
c
		integer         i
		integer         n_constr            !# of constraints
		real            dtime
		real            dtime_co
		real            delay
		real            coef
      real*8          sum8
c
c  global variables
c
      real              tcal(nrec_max)
      real              xc  (4,nrec_max)
      common /cal_time/ tcal,xc
c
      integer         year
      integer         month
      integer         day
      integer         hour
      integer         minute
      real            t0
      common /otime/  year,month,day,hour,minute,t0
c
      character*1     type (nrec_max)
      common /chrec/  type
c
      real            xstat (nStation)
      real            ystat (nStation)
      real            zstat (nStation)
      real            dly   (nStation)
      integer         nrec
      common /rec/    nrec,xstat,ystat,zstat,dly
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
      character*1     old_it
      common /ch_it1/ old_it
c
      real            co(4,4)
      real            rmsres
      real            rmsres_co
      common /cov/    co,rmsres,rmsres_co
c
      real            x_start
      real            y_start
      real            z_start
      real            ot_start
      common /start/  x_start,y_start,z_start,ot_start
c
c  *******************
c  end of declarations
c  *******************
c
c=============================================================================
c
      if (i0.eq.1 .and. .not.t0_norm .and. .not.endit) then
c
c  1) ot_start .eq. 0.0
c     for the first iteration ... given value for start origin time
c  2) ot_start .ne. 0.0
c     for the first iter. ...  t0_norm=.true. ... value given by minimizing
c     procedure
c  3) endit=.true. for the first iteration (for instance in the case of
c     fixed x, y, depth) ... no changes for origin time (only calculat.
c     for error analysis will be performed ... covar. matrix etc.)
c
          t0=ot_start
      endif
c
      if (t0_norm) then
c
c  initialize of origin time
c
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
              sum8=sum8+(trec(i)-tcal(i)-coef*delay)*wt(i)
          end do
c
          t0=sum8/no_valid_arrivals
          t0_norm = .false.
      endif
c
c  compute rms of residuals
c
      rmsres=0.0
c
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
          dtime = trec(i) - t0 - tcal(i) - coef*delay  !casova rezidua
c
c  endit=true ... for covariance matrix
c
          if (endit) then
c
c  test on the start of cycle
c
              if (i.eq.1) then
c
c  initialize rmsres_co
c
                  rmsres_co=0.0
              endif
c
              dtime_co=dtime
c
              if (abs(dtime_co).lt.reading_error) then
c
c  if difference is less then reading error ... difference will be equal to
c  reading error
c
                  dtime_co = reading_error
              endif
c
c  rmsres for covariance matrix
c
              rmsres_co=rmsres_co+dtime_co*wt(i)*wt(i)*dtime_co
          endif
c
          rmsres=rmsres+dtime*wt(i)*wt(i)*dtime
      end do
c
c  evaluate no. of constraints
c
      n_constr=4
c
      if (fix_x) n_constr=n_constr-1
      if (fix_y) n_constr=n_constr-1
      if (fix_depth) n_constr=n_constr-1
      if (fix_otime) n_constr=n_constr-1
c
      rmsres=rmsres/no_valid_arrivals
      if (no_valid_arrivals-n_constr.le.0) then
          rmsres_co=9.99**2
      else
          rmsres_co=rmsres_co/(no_valid_arrivals-n_constr)
      endif
c
      return
      end
