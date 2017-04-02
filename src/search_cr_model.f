c
C$ema /rec/
		subroutine search_cr_model(luhypo,lucrmod,chname,surname)
c
c*****************************************************************************
c
c  subroutine SEARCH_CR_MODEL
c
c-----------------------------------------------------------------------------
c
c  purpose:
c
c     search crustal model name, default crustal model for array
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
c     call search_cr_model(luhypo,lucrmod,chname,surname)
c
c----------------------------------------------------------------------------
c
c  external references:
c
c     ABORT               mw subroutine
c     REPORTERROR         mw subroutine
c     TRANS               mw subroutine
c     CASEFOLD            RL subroutine
c     TRIMLEN             RL subroutine
c
c----------------------------------------------------------------------------
c
c  programmed:  87-02  01.00  mw  original version
c               87-07  01.01  mw  slightly modified version
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
		integer   nmodels
		parameter (nmodels=7)               !number of crustal models
c
c  global parameters
c
		character*255    ch_model_name
		common/chmodn/  ch_model_name
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
cc      character*63    namr
cc      character*16    crmod_default(max_models)
      character*255    crmod_name   (nmodels)
      character*4     rec_name
c parameter_model_error,reading_error reading  
      character*81 line
      real         t_errparam(2)
      integer      itep
c
c  global variables
c
cc      character*1     letter_hp
cc      character*3     ch_event_number
cc      character*10    subdir
cc     common /hnamch/ subdir,letter_hp,ch_event_number
c
cc      integer         subdir_length
cc		logical             source_flag
cc		common /hnami/      subdir_length,source_flag
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
cc     integer         lnblnk
c
c  *******************
c  end of declarations
c  *******************
c
c  models for user choice
c
      data crmod_name(1)/'BLAHUTOVICE_B'/
      data crmod_name(2)/'BLAHUTOVICE_C'/
      data crmod_name(3)/'BLAHUTOVICE_D'/
      data crmod_name(4)/'KECEROVCE_B'/
      data crmod_name(5)/'KECEROVCE_C'/
      data crmod_name(6)/'KECEROVCE_D'/
      data crmod_name(7)/'KECEROVCE_E'/
c
c=============================================================================
c
cc      open(100,file=lu13_proc_models//'DEFAULT_3D_MOD.LST',status='OLD',
cc     >    iostat=ierr)
C use='NONEXCLUSIVE'
c
cc      if (ierr.ne.0) then
cc          crmod_default(1)='BLA_3D_A'
cc          crmod_default(2)='KEC_3D_A'
cc          crmod_default(3)='BOH_3D_A'
cc         crmod_default(4)='DOB_3D_B'
cc          crmod_default(5)='KRA_3D_A'
cc      else
cc          do i=1,5
cc              read (100,'(a)',iostat=ierr,end=1111) crmod_default(i)
c
cc              if (ierr.ne.0) then
cc                  go to 1111
cc              else
cc                  call CaseFold(crmod_default(i))
cc              endif
cc          end do
c
cc          go to 1112
cc1111      continue
cc          crmod_default(1)='BLA_3D_A'
cc          crmod_default(2)='KEC_3D_A'
cc          crmod_default(3)='BOH_3D_A'
cc         crmod_default(4)='DOB_3D_B'
cc          crmod_default(5)='KRA_3D_A'
cc112      continue
cc      endif
c
cc      close (100)
c
c  test on chname
c
      if (chname.ne.' ') then
c
c  chname not blanks ... no default model
c
c  attempt to read model number from chname
c
cc          read (chname,*,err=1) model_number
cc          chname=crmod_name(model_number)
  
cc1         continue
c
c  name of model to big letters
c
cc          call CaseFold(chname)
c
c  create namr for crustal model file
c
cc          if (index(chname,'.MOD').ne.0) then
cc              namr=lu13_proc_models//chname
cc          else
cc              len=lnblnk(chname)
cc              namr=lu13_proc_models//chname(1:len)//'.MOD'
cc          endif
c
c  open crustal model file
c
c	call setfn('model',5,modfn)
         open (lucrmod,file=modfn,iostat=ios,status='OLD')
c
c  test on error
c
          if (ios.ne.0) then
cc              call ReportError(ios,namr,'SEARCH_CR_MODEL')
              call Abort
          endif
c
c  cte posun lokalnich souradnici vuci Krovakovi
c
	  read (lucrmod,*,iostat=ios) p_over_s
c	  
          if (ios.ne.0) then
cc              call ReportError(ios,namr,'SEARCH_CR_MODEL')
   	      write (*,'(" ... Abort 666")')
              call Abort
          endif
	  write (*,*) p_over_s	  
          read (lucrmod,*,iostat=ios) p_fi
c
c  test on error in reading
c
          if (ios.ne.0) then
cc              call ReportError(ios,namr,'SEARCH_CR_MODEL')
   	      write (*,'(" ... Abort 2")')
              call Abort
          endif
c	  
          read (lucrmod,*,iostat=ios) p_x_shift
c
c  test on error in reading
c
          if (ios.ne.0) then
cc              call ReportError(ios,namr,'SEARCH_CR_MODEL')
              call Abort
          endif
c	  
          read (lucrmod,*,iostat=ios) p_y_shift
c
c  test on error in reading
c
          if (ios.ne.0) then
cc              call ReportError(ios,namr,'SEARCH_CR_MODEL')
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
cc              call ReportError(ios,namr,'SEARCH_CR_MODEL')
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
              write(*,*) 'Invalid model file. Empty line for model_error and reading_error parameter.'
              call Abort
          else
              read (line,*,end=210, iostat=ios) (t_errparam(itep),itep=1,2)
          endif
          goto 215
210       continue
          if (ios.ne.-1) then
          write(*,*) 'Error while loading model file, model_error or reading_error parameter.'
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
cc              call ReportError(999,namr,'SEARCH_CR_MODEL')
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
cc              call ReportError(ios,namr,'SEARCH_CR_MODEL')
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
cc          call ReportError (ios,namr,'SEARCH_CR_MODEL')
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
cc              call ReportError (ios,namr,'SEARCH_CR_MODEL')
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
c
c  station name to big letters
c
cc              call CaseFold( stat_name(j) )
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
cc          call ReportError (ios,namr,'SEARCH_CR_MODEL')
          call Abort
c
120       continue
      else
c
c  chname is equal to blanks ... search default crustal model for array
c
c  for the first: read in name of first station in hypofile
c
          read (luhypo,'(a4)',iostat=ios,err=2,end=2) rec_name
          go to 3
c
c  label for error
c
2         continue
cc          call ReportError(ios,'HYPOFILE','SEARCH_CR_MODEL')
          call Abort
c
3         continue
c
c  name of recording station to big letters
c
cc          call CaseFold(rec_name)
c
c  rewind the hypofile
c
          rewind (luhypo)
c
c  test the default models:
c                    the name of recording station is equal to name
c                    of station from station list in crustal model file?
c
c
c  cycle over default models
c
          do i=1,max_models
c
c  set the name of default crustal model
c
cc              chname=crmod_default(i)
cc             ch_model_name=chname
cc              len=lnblnk(crmod_default(i))
c
c  create namr of default crustal model
c
cc              namr=lu13_proc_models//
cc     >             crmod_default(i)(1:len)//'.MOD'
c
c  open i-th crustal model file
c
c		call setfn('model',5,modfn)
  		write(*,*) 'SEARCH: Modfn is  ',modfn            
		open (lucrmod,file=modfn,iostat=ios,status='OLD')
c
c  cte posun lokalnich souradnici vuci Krovakovi
c
          read (lucrmod,*,iostat=ios) p_fi
c
c  test on error in reading
c
          if (ios.ne.0) then
cc              call ReportError(ios,namr,'SEARCH_CR_MODEL')
   	      write (*,'(" ... Abort 2")')
              call Abort
          endif
c	  
          read (lucrmod,*,iostat=ios) p_x_shift
c
c  test on error in reading
c
          if (ios.ne.0) then
cc              call ReportError(ios,namr,'SEARCH_CR_MODEL')
              call Abort
          endif
c	  
          read (lucrmod,*,iostat=ios) p_y_shift
c
c  test on error in reading
c
          if (ios.ne.0) then
cc              call ReportError(ios,namr,'SEARCH_CR_MODEL')
              call Abort
          endif
c
c  test on error
c
              if (ios.ne.0) then
                  go to 111
ccc               call ReportError(ios,namr,'SEARCH_CR_MODEL')
ccc               call Abort
              endif
c
              read (lucrmod,*,iostat=ios) temp
c
c  test on error in reading
c
              if (ios.ne.0) then
cc                  call ReportError(ios,namr,'SEARCH_CR_MODEL')
                  call Abort
              endif
c
c  test on meaningful value of model error
c
              if (temp.lt.0.0) then
c
c  wrong choice of model error value
c
cc                  call ReportError(999,namr,'SEARCH_CR_MODEL')
                  call Abort
              endif
c
c  Read in value of angle between x-axis and north
c
              read (lucrmod,*,iostat=ios) nangle
c
c  test on error in reading
c
              if (ios.ne.0) then
cc                  call ReportError(ios,namr,'SEARCH_CR_MODEL')
                  call Abort
              endif
c
c  read in no. of stations (from station list in crustal model file)
c
              read (lucrmod, * ,iostat=ios,err=5,end=5) nstat
              go to 7
c
c  label for error
c
5             continue
cc              call ReportError (ios,namr,'SEARCH_CR_MODEL')
              call Abort
c
7             continue
c
c  read in station names from station list in crustal model file
c
              do j = 1, nstat
                  read (lucrmod, * ,iostat=ios,err=9,end=9)
     >            stat_name(j),xstat(j),ystat(j),zstat(j),dly(j)
                  go to 11
c
c  label for error
c
9                 continue
cc                  call ReportError (ios,namr,'SEARCH_CR_MODEL')
                  call Abort
c
11                continue
c
c  transform coordinates of red in stations
c
c
c  z-coordinate in model file is upward
c  --> set to downward
c
                  zstat(j)=-zstat(j)
c
c  Krovak to local
c
                  x_temp=xstat(j)
                  y_temp=ystat(j)
                  z_temp=zstat(j)
                  call Trans (x_temp,y_temp,z_temp,1)
                  xstat(j)=x_temp
                  ystat(j)=y_temp
                  zstat(j)=z_temp
c
c  station name to big letters
c
cc                  call CaseFold( stat_name(j) )
              end do
c
c  search for equal names of record site and station site
c
              do j=1,nstat
                  if (rec_name.eq.stat_name(j)) then
c
c  equal names found
c
                      go to 130
                  endif
              end do
111           continue
          end do
c
c  valid crustal model not found
c
          write (*,'(1x,a,": Valid crustal model not found.")')
     >    prog_name
c
c  Reset number of records in hypofile
c
          nrec=0
          call Abort
c
130       continue
c
c  valid crustal model found ... set model error from this model
c   if no choice in rustring
c
          if (model_error.lt.0.0) then
c
c  no runstring choice of model error
c
              model_error=temp
          endif
c
c  read in name of SURFACE file
c
          read (lucrmod, * ,iostat=ios,err=135,end=135) surname
          go to 140
c
c  label for error
c
135       continue
cc          call ReportError (ios,namr,'SEARCH_CR_MODEL')
          call Abort
c
140       continue
      endif
c
      return
      end
