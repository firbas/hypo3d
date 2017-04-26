c
		subroutine dialog_3
c
c*****************************************************************************
c
c  subroutine DIALOG_3
c
c-----------------------------------------------------------------------------
c
c  purpose:
c
c     menu of computing modes, input of init. variables for space coordinates
c     and origin time
c
c-----------------------------------------------------------------------------
c
c  formal parameters:
c
c
c----------------------------------------------------------------------------
c
c  calling convention:
c
c     call dialog_3
c
c----------------------------------------------------------------------------
c
c  external references:
c
c     CLEAR_DISPLAY       mw subroutine
c     ABORT               mw subroutine
c    (( REPAIR              mw subroutine)) option removed ! 96-09
c     VALID_ARRIVAL       mw subroutine
c     CASEFOLD            RL subroutine
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
c
c  local parameters  ...  none
c
c
c  global parameters
c
		include 'pname.fi'
		include 'param.fi'
c
c  local variables
c
		real         temp_real
		real         temp_array(4)
		character*1  answer
		character*4  answer4
		character*255 line
		integer      i
		integer      ios
		integer      n_fix_coord
c
c  global variables
c
      logical         scan_depth
      real            scan_start
      real            scan_end
      real            scan_step
      common /scan/   scan_depth,scan_start,scan_end,scan_step
c
      real            x_start
      real            y_start
      real            z_start
      real            ot_start
      common /start/  x_start,y_start,z_start,ot_start
c
      logical         rms_on_net
      real            start_x
      real            start_y
      real            start_depth
      real            start_otime
      real            end_y
      real            end_x
      real            end_depth
      real            end_otime
      real            step_x
      real            step_y
      real            step_depth
      real            step_otime
      common /rmsnet/ rms_on_net,
     >                start_x,end_x,step_x,
     >                start_y,end_y,step_y,
     >                start_depth,end_depth,step_depth,
     >                start_otime,end_otime,step_otime
c
      logical         fix_surface
      logical         fix_depth
	integer             i0            !no. of iter. cycle
      common /srfc/   fix_surface,fix_depth,i0
c
      logical         fix_x
      logical         fix_y
      logical         fix_otime
      common /f_mode/ fix_x,fix_y,fix_otime
c
      character*1     symbol_x
      character*1     symbol_y
      character*1     symbol_depth
      character*1     symbol_otime
      common /symb/   symbol_x,symbol_y,symbol_depth,symbol_otime
c
      integer year
      integer month
      integer day
      integer hour
      integer minute
      real    t0
      common /otime/      year,month,day,hour,minute,t0
c
c  functions ... none
c
c
c  *******************
c  end of declarations
c  *******************
c
c=============================================================================
c
c
c  init choice variable
c
      scan_depth    =.false.
      rms_on_net    =.false.
      fix_x         =.false.
      fix_y         =.false.
      fix_depth     =.false.
      fix_otime     =.false.
      fix_surface   =.false.
c
c  show menu
c
110   continue
c
          write (*,'(1x,a,": M e n u")') prog_name
          write (*,'(1x,a,": ",
     >    "End of program            -  ''E''",/,9x,
     >    "Next menu                 -  ''N''   [N]:_")')
     >    prog_name
c
          read (*,'(a)',err=110,end=110) answer
c
	 if(answer.eq.'n')answer='N'
	 if(answer.eq.'e')answer='E'

c
      if (answer.eq.'E') then
c
c  end of program
c
          call exit
      else if (answer.eq.'N' .or. answer.eq.' ') then
c
c  next menu
c
          go to 10
c
      else
c
c  wrong answer
c
          go to 110
      endif
c
c=============================================================================
c
c  next menu
c
c=============================================================================
c
10    continue
c
          write (*,'(1x,a,": M e n u")') prog_name
          write (*,'(1x,a,": ",
     >    "Scanned depth             -  ''S''",/,9x,
     >    "Fixed coordinates mode    -  ''F''",/,9x,
     >    "Rms of res. on given net  -  ''R''",/,9x,
     >    "No fixed coordinate       -  ''N''   [N]:_")')
     >    prog_name
c
          read (*,'(a)',err=10,end=10) answer
c
	 if(answer.eq.'s')answer='S'
	 if(answer.eq.'f')answer='F'
	 if(answer.eq.'r')answer='R'
	 if(answer.eq.'n')answer='N'
c
c
      if (answer.eq.'S') then
c
c  scanned depth
c
          go to 20
c
      else if (answer.eq.'F') then
c
c  fixed coordinates mode
c
          go to 50
      else if (answer.eq.'R') then
c
c  rms of residuals on given net
c
          go to 70
      else if (answer.eq.' ' .or. answer.eq.'N') then
c
c  no fixed coordinate
c
          go to 100
      else
c
c  wrong answer
c
          go to 10
      endif
c
c=============================================================================
c
c  scanned depth
c
c=============================================================================
c
20    continue
c
c  set flag for scanned depth
c
      scan_depth = .true.
c
30    continue
      write (*,'(1x,a,": Start of scan:_")') prog_name
      read (*,*,err=30,end=30) scan_start
32    continue
      write (*,'(1x,a,": End of scan:_")') prog_name
      read (*,*,err=32,end=32) scan_end
c
      if (scan_end.lt.scan_start) then
c
c  scan_start <--> scan_end
c
          temp_real=scan_start
          scan_start=scan_end
          scan_end=temp_real
      endif
c
34    continue
      write (*,'(1x,a,": Scan step:_")') prog_name
      read (*,*,err=34,end=34) scan_step
c
      if (scan_step.lt.0.0) then
          go to 34
      endif
c
40    continue
      write (*,'(1x,a,": Starting point ... X,Y,ORIG. TIME",/
     >            ,7x,"   space coord. = 0 ... value of the nearest ",
     >            "station",/
     >            ,7x,"   orig. time   = 0 ... value given by ",
     >            "minimizing procedure",/,7x,"   [0,0,0]:_")')
     >prog_name
      read (*,'(a)',end=40) line
      if (line.eq.' ') then
          x_start=0.0
          y_start=0.0
          ot_start=0.0
      else
          read (line,*,err=45,end=45) x_start,y_start,ot_start
      endif
c
      go to 1000
c
45    continue
      write (*,'(1x,a,": Wrong input data, try again."/)')
     >prog_name
      go to 40
c
c=============================================================================
c
c  fixed coordinates mode
c
c=============================================================================
c
50    continue
c
c  init. number of fixed coordinates
c
      n_fix_coord=0
c
c  show menu
c
      write (*,'(1x,a,": M e n u")') prog_name
      write (*,'(1x,a,": ",
     >"Enter combination of following letters",/,9x,
     >"(Examle: for fixed org. time and depth enter ''TZ'' etc.)",/,9x,
     >"Fixed coordinate X        -  ''X''",/,9x,
     >"Fixed coordinate Y        -  ''Y''",/,9x,
     >"Fixed depth               -  ''Z''",/,9x,
     >"Fixed origin time         -  ''T''   [Z]:_")')
     >prog_name
c
      read (*,'(a)',err=50,end=50) answer4
c
	if ( index(answer4,'X').ne.0.or.index(answer4,'x').ne.0) then
c
c  fixed coordinate x
c
          fix_x=.true.
          n_fix_coord=n_fix_coord+1
52        continue
          write (*,'(1x,a,": ",
     >    "Value of fixed coordinate X:_")')
     >    prog_name
          read (*,*,err=52,end=52) x_start
      endif
c
      if ( index(answer4,'Y').ne.0.or.index(answer4,'y').ne.0) then
c
c  fixed coordinate y
c
          fix_y=.true.
          n_fix_coord=n_fix_coord+1
54        continue
          write (*,'(1x,a,": ",
     >    "Value of fixed coordinate Y:_")')
     >    prog_name
          read (*,*,err=54,end=54) y_start
      endif
c
      if ( index(answer4,'Z').ne.0.or.index(answer4,'z').ne.0
     *					.or. answer4.eq.' ') then
c
c  fixed depth
c
          fix_depth=.true.
          n_fix_coord=n_fix_coord+1
56        continue
          write (*,'(1x,a,": ",
     >    "Value of fixed depth (999 for fixed surface) [999]:_")')
     >    prog_name
          read (*,'(a)',end=56) line
c
          if (line.eq.' ') then
c
c  set default value
c
              z_start=999.0
          else
              read (line,*,err=56,end=56) z_start
          endif
c
          if (z_start.eq.999.0) then
              fix_surface=.true.
          endif
      endif
c
      if ( index(answer4,'T').ne.0.or.index(answer4,'t').ne.0 ) then
c
c  fixed origin time
c
          fix_otime=.true.
          n_fix_coord=n_fix_coord+1
58        continue
          write (*,'(1x,a,": ",
     >    "Value of fixed origin time (only seconds, e.g. 28.340)",/,
     >    9x,"(reference time ",
     >    i2.2,"-",i2.2,"-",i2.2,1x,i2.2,":",i2.2,"):_")')
     >    prog_name,
     >    year,month,day,hour,minute
          read (*,*,err=58,end=58) ot_start
c
          if (abs(ot_start).lt.1E-7) then
c          if (ot_start.eq.0.0) then
c
c  set ot_start to 0.000001 for the reason of automatic norming of origin
c  time for the value exactly equals 0.0
c
              ot_start=0.000001
          endif
      endif
c
      if (n_fix_coord.eq.0) then
c
c  no choice of fix coordinate
c
          go to 50
      endif
c
60    continue
      if (n_fix_coord.lt.4) then
          write (*,'(1x,a,
     >    ": Enter")')
     >    prog_name
      endif
c
      if (.not.fix_x) then
          write (*,'(14x,"X  ",7x,
     >    "  (0 for value of the nearest station)")')
      endif
c
      if (.not.fix_y) then
          write (*,'(14x,"Y  ",7x,
     >    "  (0 for value of the nearest station)")')
      endif
c
      if (.not.fix_depth) then
          write (*,'(14x,"DEPTH",5x,
     >    "  (0 for value of the nearest station)")')
      endif
c
      if (.not.fix_otime) then
          write (*,'(14x,"ORIG. TIME",
     >    "  (0 for value given by minimizing procedure)")')
      endif
c
      if (n_fix_coord.eq.3) then
          write (*,'(14x,
     >    "[0]:_")')
      else if (n_fix_coord.eq.2) then
          write (*,'(14x,
     >    "[0,0]:_")')
      else if (n_fix_coord.eq.1) then
          write (*,'(14x,
     >    "[0,0,0]:_")')
      endif
c
      if (n_fix_coord.lt.4) then
c
          read (*,'(a)',end=60) line
c
          if (line.eq.' ') then
c
c  set default values
c
              do i=1,n_fix_coord
                  temp_array(i)=0.0
              end do
          else
c
c  read starting values for free variables into the temporary array
c  no. of degrees of freedom is 4-n_fix_coord
c
              read (line,*,err=60,end=60)
     >        (temp_array(i),i=1,4-n_fix_coord)
          endif
c
          if (.not.fix_x) then
c
c  'X'
c
              x_start=temp_array(1)
              if (.not.fix_y) then
c
c  'XY'
c
                  y_start=temp_array(2)
                  if (.not.fix_depth) then
c
c  'XYZ'
c
                      z_start=temp_array(3)
                      if (.not.fix_otime) then
c
c  'XYZT'
c
                          ot_start=temp_array(4)
                      endif
                  else
                      if (.not.fix_otime) then
c
c  'XYT'
c
                          ot_start=temp_array(3)
                      endif
                  endif
              else
                  if (.not.fix_depth) then
c
c  'XZ'
c
                      z_start=temp_array(2)
                      if (.not.fix_otime) then
c
c  'XZT'
c
                          ot_start=temp_array(3)
                      endif
                  else
                      if (.not.fix_otime) then
c
c  'XT'
c
                          ot_start=temp_array(2)
                      endif
                  endif
              endif
          else
              if (.not.fix_y) then
c
c  'Y'
c
                  y_start=temp_array(1)
                  if (.not.fix_depth) then
c
c  'YZ'
c
                      z_start=temp_array(2)
                      if (.not.fix_otime) then
c
c  'YZT'
c
                          ot_start=temp_array(3)
                      endif
                  else
                      if (.not.fix_otime) then
c
c  'YT'
c
                          ot_start=temp_array(2)
                      endif
                  endif
              else
                  if (.not.fix_depth) then
c
c  'Z'
c
                      z_start=temp_array(1)
                      if (.not.fix_otime) then
c
c  'ZT'
c
                          ot_start=temp_array(2)
                      endif
                  else
                      if (.not.fix_otime) then
c
c  'T'
c
                          ot_start=temp_array(1)
                      endif
                  endif
              endif
          endif
  
      endif
 
      go to 1000
c
c=============================================================================
c
c  rms of residuals on given net
c
c=============================================================================
c
70    continue
c
c  set flag for rms of res. on given net
c
      rms_on_net=.true.
      fix_x=.true.
      fix_y=.true.
c
c  coordinate x
c
75    continue
      write (*,'(1x,a,
     >": Start of coordinate X:_")')
     >prog_name
      read (*,*,err=75,end=75) start_x
c
77    continue
      write (*,'(1x,a,
     >": End of coordinate X:_")')
     >prog_name
      read (*,*,err=77,end=77) end_x
c
      if (start_x.gt.end_x) then
          temp_real=start_x
          start_x=end_x
          end_x=temp_real
      endif
c
79    continue
      write (*,'(1x,a,
     >": Step:_")')
     >prog_name
      read (*,*,err=79,end=79) step_x
c
      if (step_x.lt.0.0) then
          go to 79
      endif
c
c  coordinate y
c
81    continue
      write (*,'(1x,a,
     >": Start of coordinate Y:_")')
     >prog_name
      read (*,*,err=81,end=81) start_y
c
83    continue
      write (*,'(1x,a,
     >": End of coordinate Y:_")')
     >prog_name
      read (*,*,err=83,end=83) end_y
c
      if (start_y.gt.end_y) then
          temp_real=start_y
          start_y=end_y
          end_y=temp_real
      endif
c
85    continue
      write (*,'(1x,a,
     >": Step:_")')
     >prog_name
      read (*,*,err=85,end=85) step_y
c
      if (step_y.lt.0.0) then
          go to 85
      endif
c
87    continue
      write (*,'(1x,a,
     >": Start of depth (999 for fixed surface):_")')
     >prog_name
      read (*,*,err=87,end=87) start_depth
c
      if (start_depth.eq.999.0) then
c
c  fixed surface
c
          go to 93
      endif
89    continue
      write (*,'(1x,a,
     >": End of depth:_")')
     >prog_name
      read (*,*,err=89,end=89) end_depth
c
      if (start_depth.gt.end_depth) then
          temp_real=start_depth
          start_depth=end_depth
          end_depth=temp_real
      endif
c
91    continue
      write (*,'(1x,a,
     >": Step:_")')
     >prog_name
      read (*,*,err=91,end=91) step_depth
c
      if (step_depth.lt.0.0) then
          go to 91
      endif
c
93    continue
      write (*,'(1x,a,": ",
     >"Start of origin time (only seconds, e.g. 28.340)",/,
     >9x,"reference time ",
     >i2.2,"-",i2.2,"-",i2.2,1x,i2.2,":",i2.2,/,
     >9x,"(0 for value given by minimizing procedure) [0]:_")')
     >prog_name,
     >year,month,day,hour,minute
c
      read (*,'(a)',err=93,end=93) line
c
      if (line.eq.' ') then
c
c  set default value
c
          start_otime=0.0
      else
          read (line,*,err=93,end=93) start_otime
      endif
c
      if (abs(start_otime).lt.1E-7) then
c      if (start_otime.eq.0.0) then
          go to 1000
      endif
c
95    continue
      write (*,'(1x,a,
     >": End of origin time:_")')
     >prog_name
      read (*,*,err=95,end=95) end_otime
c
      if (start_otime.gt.end_otime) then
          temp_real=start_otime
          start_otime=end_otime
          end_otime=temp_real
      endif
c
97    continue
      write (*,'(1x,a,
     >": Step:_")')
     >prog_name
      read (*,*,err=97,end=97) step_otime
c
      if (step_otime.lt.0.0) then
          go to 97
      endif
c
      go to 1000
c
c=============================================================================
c
c
c no fixed coordinate
c
c
c=============================================================================
c
100   continue
      write (*,'(1x,a,
     >": Starting point ... X,Y,DEPTH,ORIG. TIME "/
     >,7x,"   space coord. = 0 ... value of the nearest ",
     >"station",/
     >,7x,"   orig. time   = 0 ... value given by minimizing ",
     >"procedure",/,7x,"   [0,0,0,0]:_")')
     >prog_name
      read (*,'(a)',end=100) line
      if (line.eq.' ') then
          x_start=0.0
          y_start=0.0
          z_start=0.0
          ot_start=0.0
      else
          read (line,*,err=105,end=105)
     >    x_start,y_start,z_start,ot_start
          go to 106
105       continue
          write (*,'(1x,a,": Wrong input data, try again."/)')
     >    prog_name
          go to 100
106       continue
      endif
c
c=============================================================================
c
c
c  end of menu
c
c
c=============================================================================
c
1000  continue
c
c  set grafic symbols for output
c
      if (fix_x) then
          symbol_x = '*'
      else
          symbol_x = ' '
      endif
c
      if (fix_y) then
          symbol_y = '*'
      else
          symbol_y = ' '
      endif
c
      if (fix_depth .or. scan_depth .or. fix_surface) then
          symbol_depth = '*'
      else
          symbol_depth = ' '
      endif
c
      if (fix_otime) then
          symbol_otime = '*'
      else
          symbol_otime = ' '
      endif
c
      return
      end
