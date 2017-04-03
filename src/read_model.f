c
		subroutine read_model(luhypo,lucrmod,chname,surname)
c
c*****************************************************************************
c
c  subroutine READ_MODEL
c
c-----------------------------------------------------------------------------
c
c  purpose:
c
c     read crustal model
c
c-----------------------------------------------------------------------------
c
c  formal parameters:
c
c     integer       LUHYPO      ...  lu for hypofile                  I
c     integer       LUCRMOD     ...  lu for crustal model file        I
c     character*16  CHNAME      ...  name of crustal model or no.
c                                    of cr. model or blanks          I/O
c     character*16  SURNAME     ...  name of surface file for this
c                                    model                            O
c
c----------------------------------------------------------------------------
c
c  calling convention:
c
c     call read_model(luhypo,lucrmod,chname,surname)
c
c----------------------------------------------------------------------------
c
c  external references:
c
c     ABORT               mw subroutine
c     TRANS               mw subroutine
c
c----------------------------------------------------------------------------
c
c  programmed:  87-02  01.00  mw  original version SEARCH_CR_MODEL.F
c               87-07  01.01  mw  slightly modified version
c             2017-04  10.59  pz  READ_MODEL.F cloned from SEARCH_CR_MODEL.F
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
c
c  formal parameters
c
		integer         lucrmod
		integer         luhypo
		character*255    chname
		character*255    surname
c
c  local parameters
c
c  global parameters
c
		include 'param.fi'
		include 'pname.fi'
c		include 'model_dir.fi'
c		include 'term.fi'
		include 'error.fi'
c
c  local variables
c
		real            temp
		real            x_temp
		real            y_temp
		real            z_temp
cc		integer         model_number
cc		integer         len
cc		integer         ierr
		integer         ios
cc		integer         nl
		integer         i
		integer         j
cc		integer         k

c parameter model_error,reading_error reading  
      character*81 line
      real         t_errparam(2)
      integer      itep
c
c  global variables
c
      integer         nrec
      real            xstat(nStation)
      real            ystat(nStation)
      real            zstat(nStation)
      real            dly  (nStation)
      common /rec/    nrec,xstat,ystat,zstat,dly
c
      integer         nstat
      character*4     stat_name(nStation)
      common /stnam/  nstat,stat_name
c
      real            nangle
      common /nangl/  nangle
c	
      double precision p_fi, p_x_shift, p_y_shift
      common /p_posun/ p_fi, p_x_shift, p_y_shift
c	
c
	character*255      hypfn
	character*255      modfn
	common /hymofn/ hypfn,modfn
c
c  functions
c
c  *******************
c  end of declarations
c  *******************
c
c  open crustal model file
c
         open (lucrmod,file=modfn,iostat=ios,status='OLD')
c
c  test on error
c
          if (ios.ne.0) then
              call Abort
          endif
c
c  cte posun lokalnich souradnici vuci Krovakovi
c
	  read (lucrmod,*,iostat=ios) p_over_s
c	  
          if (ios.ne.0) then
   	      write (*,'(" ... Abort 666")')
              call Abort
          endif
	  write (*,*) p_over_s	  
          read (lucrmod,*,iostat=ios) p_fi
c
c  test on error in reading
c
          if (ios.ne.0) then
   	      write (*,'(" ... Abort 2")')
              call Abort
          endif
c	  
          read (lucrmod,*,iostat=ios) p_x_shift
c
c  test on error in reading
c
          if (ios.ne.0) then
              call Abort
          endif
c	  
          read (lucrmod,*,iostat=ios) p_y_shift
c
c  test on error in reading
c
          if (ios.ne.0) then
              call Abort
          endif
c
c  read in estimated model error
c
c          read (lucrmod,*,iostat=ios) temp
c
c  test on error in reading
c
c          if (ios.ne.0) then
c              call Abort
c          endif

c =============================================================
c 2017-03-28
c Extended by the ability to read parameter reading_error
c and override its default value.
c The reading_error parameter value can optionally be specified
c by editing the velocity model line number 5th.
c On line 5, you can enter two parameters:
c         model_error as the first mandatory argument 
c         and reading_error as second optional argument.
c Both parameters are entered in seconds.

          line=""
          t_errparam(1)=-1
          t_errparam(2)=-1
          read (lucrmod,'(A)',iostat=ios) line
          if (len_trim(line)==0) then
              write(*,*) 'Invalid model file.'//
     >       ' Empty line for model_error and reading_error parameter.'
              call Abort
          else
              read (line,*,end=210, iostat=ios) 
     >             (t_errparam(itep),itep=1,2)
          endif
          goto 215
210       continue
          if (ios.ne.-1) then
          write(*,*) 'Error while loading model file, model_error'//
     >               ' or reading_error parameter.'
          call Abort 
          endif
          itep=1
215       continue
c temp: model_error
          temp=0
          if (t_errparam(1) .gt. 0) then
               temp=t_errparam(1)
          endif
c reading_error
          if (t_errparam(2) .ge. 0) then
               reading_error=t_errparam(2) 
           endif
c          write(*,*) 'model_err=',temp,'reading_err=',reading_error
c =============================================================

c
c  test on meaningful value of model error
c
          if (temp.lt.0.0) then
c
c  wrong choice of model error value
c
              call Abort
          endif
c
          if (model_error.lt.0.0) then
c
c  no runstring choice of model error
c
              model_error=temp
          endif
c
c  Read in value of angle between x-axis and north
c
          read (lucrmod,*,iostat=ios) nangle
c
c  test on error in reading
c
          if (ios.ne.0) then
              call Abort
          endif
c
c  read in number of stations
c
          read (lucrmod, * ,iostat=ios,err=50,end=50) nstat
c
c  do a test on allowed number of stations:
c  nstat requested; nStation dimensioned
c
          if(nstat.gt.nStation)then
              write (*,'(1x,a,": Too many stations. Max:",i3)')
     *        nStation
              call Abort
          endif
c
c  continue
c
          go to 70
c
c  label for error
c
50        continue
          call Abort
c
c  read in station names
c
70        continue
c
c  cycle over stations
c
          do j = 1, nstat
              read (lucrmod, * ,iostat=ios,err=90,end=90)
     >        stat_name(j),xstat(j),ystat(j),zstat(j),dly(j)
              go to 110
c
c  label for error
c
90            continue
              call Abort
c
110           continue
c
c  z-coordinate in model file is upward
c  --> set to downward
c
              zstat(j)=-zstat(j)
c
c  transform station coordinates
c  Krovak to local
c
              x_temp=xstat(j)
              y_temp=ystat(j)
              z_temp=zstat(j)
              call Trans (x_temp,y_temp,z_temp,1)
              xstat(j)=x_temp
              ystat(j)=y_temp
              zstat(j)=z_temp
          end do
c
c  read in name of SURFACE file
c
          read (lucrmod, * ,iostat=ios,err=115,end=115) surname
          go to 120
c
c  label for error
c
115       continue
          call Abort
c
120       continue
c
      return
      end
