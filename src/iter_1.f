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
c
c  local variables
c
         integer         i
         integer         n_constr            !# of constraints
         real            dtime
         real            dtime_co
         real*8          sum8
c
c  global variables
c
         real         model_error            !estimated error of model
                                             !in miliseconds
         real         reading_error          !estimated reading error in ms
                                             !(two sample intervals)
         common /err/ model_error,reading_error
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
         real            xstat (nStation)
         real            ystat (nStation)
         real            zstat (nStation)
         real            dly   (nStation)
         integer         nrec
         common /rec/    nrec,xstat,ystat,zstat,dly
c
         logical         hyr
         real            trec (nrec_max)
         real            wt   (nrec_max)
         real            avwt,sumw,sumw2
         common /hyp/    hyr,trec,wt,avwt,sumw,sumw2
c
         real            c_hypo(3)
         integer         no_valid_arrivals
         logical         t0_norm
         logical         endit
         common /it1/    t0_norm,c_hypo,no_valid_arrivals,endit
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
               sum8=sum8+(trec(i)-tcal(i))*wt(i)
            end do
c
            t0=real(sum8/no_valid_arrivals,4)
            t0_norm = .false.
         endif
c
c  compute rms of residuals
c
         rmsres=0.0
         rmsres_co=0.0
c
         do i=1,nrec
            dtime = trec(i) - t0 - tcal(i) !casova rezidua
c
c  endit=true ... for covariance matrix
c
            if (endit) then
c
               dtime_co=dtime
c
c ----------------------------------------------------------------
commented out 2018-12-04 pz v10.70
c
c              if (abs(dtime_co).lt.reading_error) then
cc  if difference is less then reading error ...
cc     difference will be equal to reading error
c                  dtime_co = reading_error
c              endif
c ----------------------------------------------------------------
c
c  rmsres for covariance matrix
c
               rmsres_co=rmsres_co+dtime_co*wt(i)*dtime_co
            endif    ! endit
c
            if (hyr) then
               rmsres=rmsres+dtime*wt(i)*wt(i)*dtime
            else
               rmsres=rmsres+dtime*wt(i)*dtime
            endif    !hyr
         end do       !i   nrec
c
         if (hyr) then
            rmsres=rmsres*avwt*avwt/sumw2
         else
            rmsres=rmsres/no_valid_arrivals
         endif
c
c  evaluate no. of constraints
c
         n_constr=4
c ----------------------------------------------------------------
c 2020-02-01 pz v10.77
c In the case of coordinate fixation, an unbiased estimate
c of the standard deviation is still calculated
c with four degrees of freedom
c         if (fix_x) n_constr=n_constr-1
c         if (fix_y) n_constr=n_constr-1
c         if (fix_depth) n_constr=n_constr-1
c         if (fix_otime) n_constr=n_constr-1
c
         if (no_valid_arrivals-n_constr.le.0) then
c            rmsres_co=9.99**2
             rmsres_co=0.0
         else
            rmsres_co=rmsres_co/(no_valid_arrivals-n_constr)
         endif
c
c ----------------------------------------------------------------
c 2018-12-04 pz v10.70
         if (rmsres_co .lt. reading_error**2) then
            rmsres_co = reading_error**2
         endif
c ----------------------------------------------------------------
         return
      end subroutine iter_1
