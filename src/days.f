c
		integer function days(month,year)
c
c*****************************************************************************
c
c  function DAYS
c
c-----------------------------------------------------------------------------
c
c  purpose:
c
c     search no. of days in month, in the case of not valid month (greater
c     then 12 or less then 1) return functional value 0
c
c-----------------------------------------------------------------------------
c
c  formal parameters:
c
c     integer     MONTH       ...  month                            I
c     integer     YEAR        ...  year                             I
c
c----------------------------------------------------------------------------
c
c  calling convention:
c
c     integer_variable=days(month,year)
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
		integer month
		integer year
c
c  local parameters described by data statement
c
c
c  global parameters  ...  none
c
c
c  local variables
c
		integer days_in_month(12)
c
c  global variables  ...  none
c
c
c  functions  ...  none
c
c
c  data statement
c
		data days_in_month/31,28,31,30,31,30,31,31,30,31,30,31/
c
c  *******************
c  end of declarations
c  *******************
c
c=============================================================================
c
c
c  init. functional value
c
		days=0
c
c  test on valid value of month
c
		if (month.lt.1 .or. month.gt.12) then
c
c  return functional value = 0
c
			 return
		endif
c
c  no. of days in month
c
		days=days_in_month(month)
c
c  test on leap year
c
		if (year/4*4.eq.year .and. month.eq.2) then
c
c  february of leap year
c
			 days=29
		endif
c
		return
		end
