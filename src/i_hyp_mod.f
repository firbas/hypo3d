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
c     read crustal model and hypfile
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
c     READ_MODEL          pz subroutine
c     SPLINE_IN           spline subroutine
c
c----------------------------------------------------------------------------
c
c  programmed:  87-07  01.00  mw INPT_3D.F
c  programmed:2017-04  10.59  pz I_HYP_MOD.F cloned of INPT_3D.F
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
c
c  global variables
c
c
         integer             nrec
         common /rec/        nrec
c
         character*1         phase(nrec_max)
         common /chrec/      phase
c
         real                amp (nrec_max)
         real                freq(nrec_max)
         common /ampli/      amp,freq
c
         real*8              datum8(nrec_max)
         integer             ichan (nrec_max)
         common /dat8/       datum8,ichan
c
         logical             hyr
         real                trec(nrec_max)
         real                wt  (nrec_max)
         real                avwt             !average weight
         common /hyp/        hyr,trec,wt,avwt
c
         real                wt1(nrec_max)
         common /wt_1/       wt1
c
         character*5         rec_name(nrec_max)
         common /chhyp/      rec_name
c
         character*255      hypfn
         character*255      modfn
         common /hymofn/ hypfn,modfn
c
c  local variables
c
         integer     i
         integer     j
         integer     psign
         integer     ios
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
         character*255 line
         character*255 surname

         character*10 ph
         real   pwt, mwt

c ----------------------------------------------------------------------
c 2024-04-22 pz
c two velocity models
         real v3p(x_layer,y_layer,z_layer)
         real v3s(x_layer,y_layer,z_layer)
         common /model_3d/ v3p,v3s

c=============================================================================
c        ip_v3=loc(v3p)
c
         write(*,*) 'INP: Hypfile is  ', trim(hypfn)
c ----------------------------------------------------------------------------
c 2018-09 10.69
         hyr = .false.
         if ( hypfn(lnblnk(hypfn)-3:lnblnk(hypfn)) .eq. '.hyr' .or.
     *        hypfn(lnblnk(hypfn)-3:lnblnk(hypfn)) .eq. '.HYR'     ) then
            hyr = .true.
         endif
c ----------------------------------------------------------------------------
         ios = 0
c
c  open the hypfile ... it must exist! (status=old)
         open (luhypo,file=hypfn,iostat=ios,status='OLD')
         if (ios.ne.0) then
            write (*,'(1x,a,": File ",a," failed to read.")')
     >           prog_name, trim(hypfn)
            call EXIT(1)
         endif
c
c  open crustal model file
         open (lucrmod,file=modfn,iostat=ios,status='OLD')
         if (ios.ne.0) then
            write (*,'(1x,a,": File ",a," failed to read.")')
     >           prog_name, trim(modfn)
            call EXIT(1)
         endif
c
c  read crustal model file
         write (*,'(1x,a,": Reading ",a)') prog_name, trim(modfn)
c  read header data
         call read_model_header(lucrmod)
c  read station data
         call read_model_sta(lucrmod)
c  read name of SURFACE file
         read (lucrmod, * ,iostat=ios,err=115,end=115) surname
         go to 120
c
c  label for error
115      continue
         call abort
c
120      continue
c
c read velocities
         call read_model_vel(lucrmod)
c
c  initialize spline common for surface computing
         call spline_in (lu_surface,surname)
c
         write (*,'(" ... [ok]")')
c
c  close the crustal model file
         close (lucrmod,status='KEEP')
c
c  read in data from hypfile
c
         i = 0
         j = 0
         do while (.true.)
            i = i + 1
c
c  read in one line from hypfile
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
     >         '(1x,a," : No. of arrivals is greater then ",i2)')
     >         prog_name,nrec_max
c
c  end of program
c
               call EXIT(0)
            endif
c

c decode read in line: chars 1 to 5 are station name(5chars)
            read (line,'(a5)') rec_name(i)
c decode read in line: arrival data in free format
            read (line(6:),*,iostat=ios,err=86,end=86)
     >      ph,
     >      ichan(i),year,month,day,hour,minute,isec,msec,micros,
     >      pwt,ampcon,psign,amp(i),period
c decode read in line: arrival phase
            if (ph(1:1) .eq. 'P' .or. ph(1:1) .eq. 'p') then
               phase(i) = 'P'
            else if (ph(1:1) .eq. 'S' .or.
     >               ph(1:1) .eq. 's' .or. ph(1:1) .eq. 'L') then
               phase(i) = 'S'
            else
               i = i-1
               goto 71
            endif
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
     >      minute*1.d00
c
c  remember original weights
c
            wt1(i)=pwt
c
c
c  test on phase of arrival
c
            if (phase(i) .eq. 'S') then
               j = j + 1
            else if (phase(i) .ne. 'P') then
               write (*,
     >         '(1x,a," : Wrong type of arrival in record ",i2)')
     >         prog_name,i
            endif
c
c  code recorded time
c
            trec(i) = (isec * 1000. + msec)*1e-3

71          continue
         end do
c
c  part for error in reading of hypfile
c
86       continue
c
c  close hypfile
c
         close (luhypo,status='KEEP')
         call EXIT(0)
c
87       continue
c
c  hypfile is OK
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
     >   prog_name, trim(hypfn)
c
c ---------------------------------------------------------------------
c 2018-09  10.69
c compute weights
         mwt = 0.0
         avwt = 0.0
         do i=1,nrec
            pwt = wt1(i)
            if (hyr) then
c Inverse-variance weighing
c weights refer (are proportional) to the standard deviation
               if (pwt .gt. 0) then
                  pwt = pwt/1000.0
                  pwt = 1. / pwt
               else if (pwt .lt. 0) then
                  pwt = 0.0
               else
                  pwt = 0.0
               endif
            else
               pwt=(4.-pwt)/4.
c                   pwt=(4.-float(iwt))/4.
            endif
            wt(i) = pwt
            if (pwt > mwt) mwt = pwt
         end do   ! nrec
c ---------------------------------------------------------------------
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
            write(*,'(9x,a5,6x,a1,4x,f8.2,7x,f5.3)')
     >      rec_name(i),phase(i),trec(i),wt(i)/mwt
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
     >      prog_name
         endif
c
         return
      end subroutine I_HYP_MOD
