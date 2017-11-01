c
		subroutine I_HYP_MOD
c
c*****************************************************************************
c
c  subroutine I_HYP_MOD
c
c-----------------------------------------------------------------------------
c
c  purpose:
c
c     read crustal model and hypofile
c
c-----------------------------------------------------------------------------
c
c  formal parameters:
c
c     integer N_LOC
c
c----------------------------------------------------------------------------
c
c  calling convention:
c
c     call i_hyp_mod
c
c----------------------------------------------------------------------------
c
c  external references:
c
c     ABORT               mw subroutine
c     READ_MODEL          pz subroutine
c     lnblnk              RL subroutine
c     SPLINE_IN           spline subroutine
c
c----------------------------------------------------------------------------
c
c  programmed:  87-07  01.00  mw INPT_3D.F
c  programmed:2017-04  10.59  pz I_HYP_MOD.F cloned of INPT_3D.F
c  programmed:  18-05  10.66  pz Weights in quadratic scale
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
c  local parameters
c
		integer       luhypo,lucrmod,lu_surface
		parameter    (luhypo=11)
		parameter    (lucrmod=12)
		parameter    (lu_surface=15)
c
c  global parameters
c
		include 'param.fi'
		include 'pname.fi'
		include 'model_3d.fi'
c
c  local variables
c
		integer     len1
		integer     event_number
		integer     ix
		integer     iy
		integer     iz
		integer     np
		integer     i
		integer     j
		integer     iwt
		integer     isign
		integer     ios
      integer     len
      integer     micros
      integer     msec
      integer     isec
      integer     minute
      integer     hour
      integer     day
      integer     month
      integer     year
      real        ampcon
      real        period
      character*1  answer
      character*255 line
      character*255 surname

      real   pwt
c
c  global variables
c
		integer             nx(7),ny(7),nxs(7),nys(7),nws(7)
		real                x(48),y(48),w(1024)
		real                vx(5,48),vy(5,48),sigma(7)
		common /sur/        nx,ny,nxs,nys,nws,x,y,w,vx,vy,sigma
c
		character*255        ch_model_name   !name of crustal model for use
		common /chmodn/     ch_model_name
c
		integer             nrec
		real                xstat(nStation)
		real                ystat(nStation)
		real                zstat(nStation)
		real                dly  (nStation)
		common /rec/        nrec,xstat,ystat,zstat,dly
c
		character*1         type(nrec_max)
		common /chrec/      type
c
		integer             key (nrec_max)
cc		real                modl(nStation,z_layer,2)
cc		integer             nli (nStation)
		common /stmod/      key                !,modl,nli 
c
		real                amp (nrec_max)
		real                freq(nrec_max)
		common /ampli/      amp,freq
c
		integer             nstat
		character*4         stat_name(nStation)
		common /stnam/      nstat,stat_name
c
		real*8              datum8(nrec_max)
		integer             ichan (nrec_max)
		common /dat8/       datum8,ichan
c
		real                trec(nrec_max)
		real                wt  (nrec_max)
		real                avwt            !average weight
		common /hyp/        trec,wt,avwt
c
		real                wt1(nrec_max)
		common /wt_1/       wt1
c
		character*4         rec_name(nrec_max)
		common /chhyp/      rec_name
c
	character*255      hypfn
	character*255      modfn
	common /hymofn/ hypfn,modfn
c
c  functions
c
		integer             lnblnk
cc		logical             IfBrk
c
c  *******************
c  end of declarations
c  *******************
c
c=============================================================================
c
		write(*,*) 'INP: Hypofile is  ', hypfn(1:lnblnk(hypfn))

		ios = 0
		event_number = 1
c
c  open the hypofile ... it must exist! (status=old)
c
		open (luhypo,file=hypfn,iostat=ios,status='OLD')
c
c  test on error
c
		if (ios.ne.0) then
c
c  reset number of records in hypofile
c
			 nrec=0
			 call EXIT(1)
		endif
c
70    continue

c
c  read crustal model file, read in station data
c
		call read_model(luhypo,lucrmod,ch_model_name,surname)
c
c  initialize spline common for surface computing
c
		call spline_in (lu_surface,surname)
c
c  write the message  ...  read crustal model
c
		len=lnblnk(ch_model_name)
		if (index(ch_model_name,'.MOD').eq.0 .and.
     *		    index(ch_model_name,'.mod').eq.0) then
			 write (*,'(1x,a,": Reading ",a)')
     >    prog_name,
     >    ch_model_name(1:len)//'.mod'
		else
			 write (*,'(1x,a,": Reading ",a)')
     >    prog_name,
     >    ch_model_name(1:len)
		endif
c
c  skip one comment line
c
		read (lucrmod,*,end=80)
c
c  read in number of layers in direction x, y, z
c
		read (lucrmod, * ,err=80,iostat=ios,end=80) nxl,nyl,nzl
		np=nxl-1
c
c  skip one comment line
c
		read (lucrmod,*,end=80)
c
c  read in value of interfaces in directions x, y, z
c
c  direction x:
c
		read(lucrmod, * ,err=80,iostat=ios,end=80)(xl(j),j=1,np)
		np=nyl-1
c
c  skip one comment line
c
		read (lucrmod,*,end=80)
c
c  direction y:
c
		read(lucrmod, * ,err=80,iostat=ios,end=80)(yl(j),j=1,np)
		np=nzl-1
c
c  skip one comment line
c
		read (lucrmod,*,end=80)
c
c  direction z:
c
		read(lucrmod, * ,err=80,iostat=ios,end=80)(zl(j),j=1,np)
c
c  following nested cycles are for reading in values in blocks
c
		do iy = 1,nyl
c
c  skip one comment line
c
			 read (lucrmod,*)
			 do ix = 1,nxl
c
c  skip one comment line
c
			     read (lucrmod,*,end=80)
			     read (lucrmod,*,err=80,iostat=ios,end=80)
     >        (v3(ix,iy,iz),iz = 1,nzl)
			 end do
		end do
		go to 85
c
c  part for errors in reading of crustal model
c
80    continue
c
c  error in reading of crustal models
c
		write (*,'(" ... [failed]")')
c
		if (ios.eq.0) then
c
c  end of file in unformatted reading from file
c
			 ios=-1
		endif
c
c
c  close crustal model file
c
		close (lucrmod,status='KEEP')
c
c  end of program
c
		call EXIT(0)
c
85    continue
c
c  crustal model is OK
c
		write (*,'(" ... [ok]")')
c
c  close the crustal model file
c
		close (lucrmod,status='KEEP')
c
c  read in data from hypofile
c
		i = 0
		j = 0
		do while (.true.)
			 i = i + 1
c
c  read in one line from hypofile
c
			 read (luhypo,'(a)',end=87) line
c
c  test on overflow of dimension
c
			 if (i.gt.nrec_max) then
c
c  exceeds dimension
c
			     write (*,
     >        '(1x,a," : No. of arrivals is greater then ",i2)')
     >        prog_name,nrec_max
c
c  end of program
c
			     call EXIT(0)
			 endif
c
c  decode read in line: chars 1 to 6 are station name(4chars), one arbitrary
c  character and type of arrival(1char)
c
			 read (line(1:6),'(a4,1x,a1)',iostat=ios,err=86,end=86)
     >             rec_name(i),type(i)
c
c  characters 7 to end of line are next data for arrival in free format
c
			 read (line(7:),*,iostat=ios,err=86,end=86)
     >    ichan(i),year,month,day,hour,minute,isec,msec,micros,
     >    iwt,ampcon,isign,amp(i),period
c
c  evaluate frequency
c
			 freq(i)=1./period
c
c  test on meaningfull value of frequencies
c
			 if (freq(i).gt.100.0) then
			     freq(i) = 99.9
			 endif
c
c  code datum in one double real element
c
			 datum8(i)=year*1.d08+month*1.d06+day*1.d04+hour*1.d02+
     >    minute*1.d00
c
c  remember original weights
c
			 wt1(i)=iwt
c
c
c  compute weights
c
			 pwt=(4.-float(iwt))/4.
c =====================================================================
c 2018-05  10.66  Weights in quadratic scale
                         wt(i)=pwt*pwt
c
c =====================================================================
c
c  test on type of arrival
c
			 if (type(i) .eq. 'S') then
			     j = j + 1
			 else if (type(i) .ne. 'P') then
			     write (*,
     >        '(1x,a," : Wrong type of arrival in record ",i2)')
     >        prog_name,i
			 endif
c
c  code recorded time
c
			 trec(i) = (isec * 1000. + msec)*1e-3
		end do
c
c  part for error in reading of hypofile
c
86    continue
c
c  close hypofile
c
		close (luhypo,status='KEEP')
		call EXIT(0)
c
87    continue
c
c  hypofile is OK
c
		close (luhypo,status='KEEP')
c
c  set number of recorded arrivals
c
		nrec = i - 1
c
c  write input data to terminal
c
		write (*,'(1x,a,": Phase data from ",a)')
     >prog_name,hypfn(1:lnblnk(hypfn))
c
c  write header
c
		write (*,'(
     >/" Recorded Times Of Arrivals On Particular Stations")')
		write(*,'(/
     >"       Station     Type    Rel. Time   Weight",/,
     >"         Name    Of Wave   Of Arrival        "/)')
c
c  cycle over recorded arrivals
c
		do i=1,nrec
c
c  one line with data for ane arrival
c
			 write(*,'(9x,a4,7x,a1,4x,f8.2,7x,f4.2)')
     >    rec_name(i),type(i),trec(i),wt(i)          
		end do
c
c  one empty line
c
		write (*,'(" ")')
c
c  test on number of p-arrivals
c
		if (j.eq.nrec) then
c
c  all arrival are s-arrivals
c
			 write (*,'(1x,a," : Warning ... no P_arrival.")')
     >    prog_name
		endif
c
999   continue
c
		return
		end
