c
		subroutine datum (year,month,day,hour,minute,second)
c
c*****************************************************************************
c
c  subroutine DATUM
c
c-----------------------------------------------------------------------------
c
c  purpose:
c
c     - checks datum for 0 < second < 60, 0 < minute < 60,
c       0 < hours < 24, 0 < days < n, 0 < month < 13;
c       n ... no. of days in month + 1
c     - in the case of wrong values of items, converts the datum
c       to the valid values
c
c-----------------------------------------------------------------------------
c
c  formal parameters:
c
c     integer     YEAR        ...  year                             I/O
c     integer     MONTH       ...  month                            I/O
c     integer     DAY         ...  day                              I/O
c     integer     HOUR        ...  hour                             I/O
c     integer     MINUTE      ...  minute                           I/O
c     integer     SECOND      ...  second                           I/O
c
c----------------------------------------------------------------------------
c
c  calling convention:
c
c     call datum (year,month,day,hour,minute,second)
c
c----------------------------------------------------------------------------
c
c  external references:
c
c     DAYS                mw function
c
c----------------------------------------------------------------------------
c
c  programmed:  85-07  01.00  mw
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
		integer year
		integer month
		integer day
		integer hour
		integer minute
		integer second
c
c  local parameters  ...  none
c
c
c  global parameters
c
c
c  local variables
c
		integer sday                    !true no. of days in month
		integer flag
c
c  global variables  ...  none
c
c
c  functions
c
      integer days
c
c  *******************
c  end of declarations
c  *******************
c
c=============================================================================
c
c
c  init. flag
c
      flag = 0
c
c  normalize seconds
c
      do while (second.ge.60)
          second=second-60
          minute=minute+1
      end do
c
      do while (second.lt.0)
          second=second+60
          minute=minute-1
      end do
c
c  normalize minutes
c
      do while (minute.ge.60)
          minute=minute-60
          hour=hour+1
      end do
c
      do while (minute.lt.0)
          minute=minute+60
          hour=hour-1
      end do
c
c  normalize hours
c
      do while (hour.ge.24)
          hour=hour-24
          day=day+1
      end do
c
      do while (hour.lt.0)
          hour=hour+24
          day=day-1
      end do
c
c  normalize days ... lower limit
c
      do while (day.lt.1)          !cycle for no. of days
          month = month - 1
c
c  normalize months
c
          do while (month .lt. 1)      !cycle for month
              month = month + 12
              year = year - 1
              if (year .lt. 0) then
                  write (*,'("DATUM: Warning - year<0.")')
              endif
          end do
c
          do while (month .gt. 12)
              month = month - 12
              year = year + 1
          end do
c
          day = day + days(month,year)
          flag = 1
      end do
c
c  test on flag
c
      if (flag .eq. 1) then
c
c  months are normalized
c
          go to 1
      endif
c
c  normalize months
c
      do while (month .lt. 1)
          month = month + 12
          year = year - 1
          if (year .lt. 0) then
              write (*,'("DATUM: Warning - year<0.")')
          endif
      end do
c
      do while (month .gt. 12)
          month = month - 12
          year = year + 1
      end do
c
      sday = days(month,year)
c
c  normalize days ... upper limit
c
      do while (day .gt. sday)          !cycle for no. of days
          day = day - sday
          month = month + 1
          sday = days(month,year)
c
          if (sday .eq. 0) then
              month = month - 12
              year = year + 1
              sday = days(month,year)
          endif
c
      end do
c
1     continue
c
      return
      end
