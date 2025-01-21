c
      subroutine origin_time (n_increase,dmin8)
c
c*****************************************************************************
c
c  subroutine ORIGIN_TIME
c
c-----------------------------------------------------------------------------
c
c  purpose:
c
c     normalize origin time; called in HYPO main program
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
         integer n_increase
         real*8  dmin8
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
         integer i
         integer isec
c
c  global variables
c
         integer         year
         integer         month
         integer         day
         integer         hour
         integer         minute
         real            t0
         common /otime/  t0,year,month,day,hour,minute
c
         integer             year_orig
         integer             month_orig
         integer             day_orig
         integer             hour_orig
         integer             minute_orig
         real                t_orig
         common /origin/     t_orig,year_orig,month_orig,day_orig,
     >                              hour_orig,minute_orig
c
         integer             nrec            !no. of arrivals
         common /rec/        nrec
c
         logical             hyr
         real                trec(nrec_max)  !observed times
         common /hyp/        hyr,trec
c
c  *******************
c  end of declarations
c  *******************
c
c=============================================================================
c
c
c  init. variable
c
         n_increase=0
c
c  decode dmin8 to year,month,day,hour,minute
c
         call real8_to_int(dmin8,year,month,day,hour,minute)
c
c  initialize variables for origin time
c
         year_orig=year
         month_orig=month
         hour_orig=hour
         day_orig=day
         minute_orig=minute
c
c  cycle for norm. of orig. time
c
c
c  in the case of origin time greater then any of rec. times, can be orig.
c   time greater then 60.0
c
         if (t0.ge.60.0) then
            t_orig=t0
c
            do while (t_orig.ge.60.0)
               t_orig=t_orig-60.0
               minute_orig=minute_orig+1
            end do
c
c  normalize datum variables
c
            isec=0
            call datum
     >      (year_orig,month_orig,day_orig,hour_orig,minute_orig,isec)
c
         else if (t0.lt.0.0) then
c
            do while (t0.lt.0.0)
               n_increase=n_increase+1
               t0=t0+60.0
               minute=minute-1
               minute_orig=minute_orig-1
               do i=1,nrec
                  trec(i)=trec(i)+60.0
               end do
            end do
c
c  dmin8 ... reference time for arrivals
c
c
c  normalize datum variables
c
            isec=0
            call datum(year,month,day,hour,minute,isec)
            isec=0
            call datum
     >      (year_orig,month_orig,day_orig,hour_orig,minute_orig,isec)
c
c  encode dmin8
c
            dmin8=year*1.d08+month*1.d06+day*1.d04+hour*1.d02+
     >      minute*1.d00
c
c  set t_orig
c
            t_orig=t0
         else
c
c  set t_orig
c
            t_orig=t0
         endif

         return
      end subroutine origin_time

