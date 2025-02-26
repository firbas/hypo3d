c
      subroutine o_hy3(lulist)
c
c*****************************************************************************
c
c  subroutine O_HY3
c
c-----------------------------------------------------------------------------
c
c  purpose:
c
c     output results to list devices/files (list-file, database-file)
c
c-----------------------------------------------------------------------------
c
c  formal parameters:
c
c     integer     LULIST      ...  lu for output                    I
c
c----------------------------------------------------------------------------
c
c  calling convention:
c
c     call o_hy3 (lulist)
c
c----------------------------------------------------------------------------
c
c  external references:
c
c     TRANS               mw subroutine
c     DATUM               mw subroutine
c     SORT_X              mw subroutine
c     XY2FL               pv subroutine
c----------------------------------------------------------------------------
c
c  programmed:  87-07  01.00  mw OUTPUT.F
c  programmed:2013-07  10.10  pv XY2FL subroutine, Krovak to WGS84 conversion
c  programmed:2017-04  10.59  pz O_HY3 cloned from OUTPUT.F
c  programmed:2017-04  10.62  pz Rotation of the diagonal elements
c                                in the co matrix from local to Krovak
c  programmed:2017-04  10.64  pz function mconvergence
c  programmed:2018-10  10.69  pz hyr
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
         integer lulist
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
         character*22  whole_date
         integer itime(5)
         integer ita(9)
         integer stime
         integer iyear
         integer ktemp(nrec_max)
         real temp2
         integer i,j
         integer isec,msec
c
         real    dtemp
         real    deter
         real    d11,d21,d22
         real    theta, az_theta
         real    al,bl
         real    l1,l2,tl
         real    dxer,dyer,dzer,dter
         real    dx,dy,dz
         real    temp(nrec_max)
         real    gap
         real    d_hypo(nrec_max)
         real    d_epi(nrec_max)

         real    az(nrec_max)
         real    xp,yp,zp
         real*8  fi, rla
         real    meridian_con

         character*7 strFixX, strFixY, strFixZ, strFixT 

c
         double precision c,s,PI_D,RAD2DEG,DEG2RAD

c
c  global variables
c
         logical      ee3                    !full error elipsoid
         real         model_error            !estimated error of model [ms]
         real         reading_error          !estimated reading error [ms]
         common /err/ model_error,reading_error,ee3
c
         integer             key(nrec_max)   !key field
         common /stmod/      key
c
         character*255      hypfn
         character*255      modfn
         common /hymofn/ hypfn,modfn
c
         real                amp(nrec_max)
         real                freq(nrec_max)
         common /ampli/      amp,freq
c
         integer             rok
         integer             mesic
         integer             den
         integer             hodina
         integer             minuta
         real                t0
         common /otime/      t0,rok,mesic,den,hodina,minuta
c
         real                x0
         real                y0
         real                z0
         common /centr/      x0,y0,z0
c
         integer             nrec
         real                xstat (nStation)
         real                ystat (nStation)
         real                zstat (nStation)
         common /rec/        nrec,xstat,ystat,zstat
c
         character*1         phase(nrec_max)
         common /chrec/      phase
c
         real                rmsres
         real                rmsres_co
         real                co(4,4)
         common /cov/        rmsres,rmsres_co,co
c
         logical             hyr
         real                trec(nrec_max)
         real                wt(nrec_max)
         common /hyp/        hyr,trec,wt
c
         character*5         rec_name(nrec_max)
         common /chhyp/      rec_name
c
         real                tcal(nrec_max)
         real                xc(4,nrec_max)
         common /cal_time/   tcal,xc
c
         real                toa(nrec_max)
         common /toa/        toa
c
         real                xmag(nrec_max)
         real                sdm
         real                avm
         common /mag/        xmag,avm,sdm
c
         real                x_start
         real                y_start
         real                z_start
         real                ot_start
         common /start/      x_start,y_start,z_start,ot_start
c
         integer             year_orig
         integer             month_orig
         integer             day_orig
         integer             hour_orig
         integer             minute_orig
         real                t_orig
         common /origin/     t_orig,year_orig,month_orig,day_orig,
     >                              hour_orig, minute_orig
c
         integer             i0
         common /citer/      i0
c
         logical             fix_surface
         common /srfc/       fix_surface
c
         logical         fix_depth
         logical         fix_x
         logical         fix_y
         logical         fix_otime
         common /fix_mode/ fix_depth,fix_x,fix_y,fix_otime
c
         real            nangle
         common /nangl/  nangle

         real             p_fi
         real             p_x_shift, p_y_shift
         common /p_posun/ p_fi, p_x_shift, p_y_shift
c
         real                wt1(nrec_max)
         common /wt_1/       wt1
c
         logical         ee_nan(4)
         common /nan/    ee_nan
c
c  functions
c
         integer time
         real mconvergence
c
c
c  *******************
c  end of declarations
c  *******************
c
         PI_D=4.D0*datan(1.D0)
         RAD2DEG=180.D0/PI_D
         DEG2RAD=PI_D/180.D0
c
c --------------------------------------------------------------------
c
c error ellipse for epicenter
c computed in local coordinates
         deter=co(1,1)*co(2,2)-co(1,2)*co(2,1)
         d11=co(2,2)/deter
         d22=co(1,1)/deter
         d21=-co(2,1)/deter
         theta=0.5*atan( 2.*d21/(d11-d22) )
         al=d11*cos(theta)**2 + 2.*d21*cos(theta)*sin(theta) +
     >   d22*sin(theta)**2
         bl=d11*sin(theta)**2 - 2.*d21*cos(theta)*sin(theta) +
     >   d22*cos(theta)**2
c
         l1=sqrt(1./al)
         l2=sqrt(1./bl)
c
c theta is the angle from x-axis to the semi-major axis of the error ellipse
c
c local coordinates
         theta = real(theta*RAD2DEG,4)
c l1 is major axis
         if (l2 .gt. l1) then
            tl=l1
            l1=l2
            l2=tl
            theta=theta+90.0
         endif
c
c ====================================================================
c 2017-04-08 pz
c The covariance matrix co was computed in local coord. To compensate this,
c the diagonal elements in the co matrix are rotated from the local to Krovak.
         c=dcos(p_fi*DEG2RAD)
         s=dsin(p_fi*DEG2RAD)
         dxer=real(c*c*co(1,1)-s*c*co(1,2)-s*c*co(2,1)+s*s*co(2,2),4)
         dyer=real(s*s*co(1,1)+s*c*co(1,2)+s*c*co(2,1)+c*c*co(2,2),4)
c ====================================================================
         dxer=sqrt(abs(dxer))
         dyer=sqrt(abs(dyer))
         dzer=sqrt(abs(co(3,3)))
         dter=sqrt(abs(co(4,4)))
c --------------------------------------------------------------------
c In the case of coordinate fixation, the calculation of the error ell
c is not reduced, except in the following cases:

c modify for fixed coordinates
         if (fix_surface .or. (fix_depth .and. .not. ee3 )) then
            dzer=0.0
         endif

         if (fix_otime) then
            dter=0.0
         endif
c --------------------------------------------------------------------
c hypocenter to Krovak
         xp=x0
         yp=y0
         zp=z0
c
c local to Krovak
         call trans (xp,yp,zp,0)
         meridian_con = mconvergence(xp,yp)
c      write(*,*) "m.k: ",meridian_con
c
c coordinates local --> Krovak
         theta = mod(360.0+theta+p_fi,360.0)
c coordinates Krovak --> geofraphic
         az_theta = theta-180.0-meridian_con
         az_theta = mod(360.0+az_theta,360.0)
c --------------------------------------------------------------------
         j = 0
         do i = 1,nrec
            dx = x0-xstat(key(i))
            dy = y0-ystat(key(i))
            dz = z0-zstat(key(i))
c
c  az(i) ... angle between x-axis and recorder to source direction minus
c            angle between x-axis and north
c
            temp2=real(atan2(dy,dx)*RAD2DEG,4)
c                   az(i) = mod(720.0-nangle+temp2,360.0)
            az(i) = mod(720.0+temp2-180.0-meridian_con+p_fi,360.0)
c
            if (wt(i).gt.0.0) then
               j = j + 1
               temp(j)=az(i)
            endif
c
            d_hypo(i) = sqrt( dx**2 + dy**2 + dz**2 )
            d_epi (i) = sqrt( dx**2 + dy**2 )
         end do
c
         call sort_x(temp,ktemp,j)
         gap = temp(1)+360.-temp(j)
c
         do i=2,j
            dtemp=temp(i)-temp(i-1)
            if (dtemp.gt.gap) then
               gap=dtemp
            endif
         end do
c
c --------------------------------------------------------------------
c provedeme transformaci data do tvaru rr-mm-dd  hh:mm:ss.ss
c
         isec=int(t_orig)
         msec=int((t_orig-isec)*1000.0)
         write (whole_date,
     >   '(2(i2.2,"-"),i2.2,2x,2(i2.2,":"),i2.2,".",i3.3)')
     >   year_orig,month_orig,day_orig,hour_orig,minute_orig,isec,msec
c
c --------------------------------------------------------------------
c

         write (lulist,'("program       :",a)') prog_name1//prog_name2
         write (lulist,'("model         :",a)') trim(modfn)
         if (hyr) then
            write (lulist,'("model error   :")')
            write (lulist,'("reading error :")')
         else
            write (lulist,'("model error   :",f5.3," s")') model_error
            write (lulist,'("reading error :",f5.3," s")') reading_error
         endif
c
c  get system time
c
         stime = time ()
         call ltime (stime,ita)
         itime(2)=ita(1)
         itime(3)=ita(2)
         itime(4)=ita(3)
         itime(5)=ita(8)+1
         iyear = ita(6)+2000
c
         itime(1)=1
         call datum (iyear,itime(1),itime(5),itime(4),itime(3),itime(2))
         iyear=iyear-(iyear/100)*100
         write (lulist,'("create time   :",i2.2,"-",i2.2,"-",i2.2,1x,
     >i2.2,":",i2.2,":",i2.2)') iyear,itime(1),itime(5),itime(4),
     >   itime(3),itime(2)
         write (lulist,'("event",9x,":",a)') trim(hypfn)

         write (lulist,'("start(x,y,z,t):   (",
     >f7.2,",",f7.2,",",f7.2,",",f7.2,")")')
     >   x_start,y_start,z_start,ot_start
         if (fix_x) then
             strFixX=" fix X "
         else
             strFixX="       "
         endif
         if (fix_y) then
             strFixY=" fix Y "
         else
             strFixY="       "
         endif
         if (fix_depth) then
             strFixZ=" fix Z "
         else
             strFixZ="       "
         endif
         if (fix_otime) then
             strFixT=" fix T "
         else
             strFixT="       "
         endif
         
         write (lulist,
     >    '("fixed coordinates:(",a7,",",a7,",",a7,",",a7,")")')
     >    strFixX, strFixY, strFixZ, strFixT 
         write (lulist,*)
         write (lulist,
     >    '("reference time:",i2.2,"-",i2.2,"-",i2.2,1x,i2.2,":",i2.2)')
     >    rok,mesic,den,hodina,minuta
c
c header for station data
c
         if(hyr) then
            write (lulist,
     *      '("------------------------------------------------------",
     *  "---------------------")')
            write (lulist,'(" sta   |obs. t.|cal. t.|res. |amplitude"
     *,"|freq|   w | epi |hypo |azm|ain|xmag")')
            write(lulist, '("       |  [s]  |  [s]  | [s] |  [m/s]  "
     *,"|[Hz]| [ms]|[km] |[km] |[o]|[o]|    ")')
            write (lulist,
     *      '("------------------------------------------------------",
     *  "---------------------")')
         else
            write (lulist,
     *      '("------------------------------------------------------",
     *  "-----------------")')
            write (lulist,'(" sta   |obs. t.|cal. t.|res. |amplitude"
     *,"|freq|w| epi |hypo |azm|ain|xmag")')
            write(lulist, '("       |  [s]  |  [s]  | [s] |  [m/s]  "
     *,"|[Hz]| |[km] |[km] |[o]|[o]|    ")')
            write (lulist,
     *      '("------------------------------------------------------",
     *  "-----------------")')
         endif
c
c cycle for writing station distances,etc.
c
         do i=1,nrec

            if (hyr) then
               if (phase(i).eq.'S') then
                  write (lulist,917) rec_name(i),phase(i),trec(i),
     >               tcal(i)+t0,trec(i)-tcal(i)-t0,amp(i),freq(i),
     >               int(wt1(i)),d_epi(i),d_hypo(i),int(az(i)+0.5),
     >               int(toa(i)+0.5),xmag(i)
               else
                  write (lulist,917) rec_name(i),phase(i),trec(i),
     >               tcal(i)+t0,trec(i)-tcal(i)-t0,amp(i),freq(i),
     >               int(wt1(i)),d_epi(i),d_hypo(i),int(az(i)+0.5),
     >               int(toa(i)+0.5)
               endif
c
            else      ! hyr == .false.
c
               if (phase(i).eq.'S') then
                  write (lulist,916) rec_name(i),phase(i),trec(i),
     >               tcal(i)+t0,trec(i)-tcal(i)-t0,amp(i),freq(i),
     >               int(wt1(i)),d_epi(i),d_hypo(i),int(az(i)+0.5),
     >               int(toa(i)+0.5),xmag(i)
               else
                  write (lulist,916) rec_name(i),phase(i),trec(i),
     >               tcal(i)+t0,trec(i)-tcal(i)-t0,amp(i),freq(i),
     >               int(wt1(i)),d_epi(i),d_hypo(i),int(az(i)+0.5),
     >               int(toa(i)+0.5)
               endif
c
            endif     ! hyr
c
 916        format (a5,' ',a1,'|',
     >      f7.2,   '|',f7.2,   '|',f5.3,'|',1pe9.2,'|',0pf4.1,
     >      '|',i1,'|',0pf5.1,'|',0pf5.1,
     >      '|',i3,'|',i3,'|',f4.1)
 917        format (a5,' ',a1,'|',
     >      f7.2,   '|',f7.2,   '|',f5.3,'|',1pe9.2,'|',0pf4.1,
     >      '|',i5,'|',0pf5.1,'|',0pf5.1,
     >      '|',i3,'|',i3,'|',f4.1)
         end do
c
         call XY2FL (yp*1000, xp*1000, fi, rla)
         print *,xp*1000,yp*1000,fi,rla

      write(lulist,'(//,"hypocenter data:",/,"----------------")')
      write(lulist,'("origin time          t:",2x,a22,$)') whole_date
      if (ee_nan(4)) then
         write(lulist,'(1x,"+-    NaN")')
      else
         write(lulist,'(1x,"+-",1x,f6.3)') dter
      endif
      write(lulist,'("x-coordinate         x:",2x,f7.2,$)') xp
      if (ee_nan(1)) then
         write(lulist,'(1x,"+-    NaN   km",$)')
      else
         write(lulist,'(1x,"+-",1x,f6.2,3x,"km",$)') dxer
      endif
      write(lulist,'(7x,"(fi:",f10.6," deg)")') fi
      write(lulist,'("y-coordinate         y:",2x,f7.2,$)') yp
      if (ee_nan(2)) then
         write(lulist,'(1x,"+-    NaN   km",$)')
      else
         write(lulist,'(1x,"+-",1x,f6.2,3x,"km",$)') dyer
      endif
      write(lulist,'(3x,"(lambda:",f10.6," deg)")') rla
      write(lulist,'("depth                z:",2x,f7.2,$)') zp
      if (ee_nan(3)) then
         write(lulist,'(1x,"+-    NaN   km")')
      else
         write(lulist,'(1x,"+-",1x,f6.2,3x,"km")') dzer
      endif
      write(lulist,
     >        '("magnitude           ml:",2x,f7.2,1x,"+-",1x,f6.2)')
     >         avm, sdm
      write(lulist,'("rms of time residuals :",8x,f6.2,8x,"s")')
     >        sqrt(rmsres)
      write(lulist,'("angular gap           :",11x,i3,8x,"deg")')
     >        int(gap+0.5)
      write(lulist,'("number of iterations  :",11x,i3)') i0
      if(ee_nan(1) .or. ee_nan(2)) then
         write(lulist,'("error ellipse axis l1 :",10x," NaN",8x,"km")')
         write(lulist,'("              axis l2 :",10x," NaN",8x,"km")')
      else
         write(lulist,'("error ellipse axis l1 :",8x,f6.2,8x,"km")') l1
         write(lulist,'("              axis l2 :",8x,f6.2,8x,"km")') l2
      endif
c      write(lulist,'("              theta   :",8x,f6.1,9x,"deg")') az_theta
      if(ee_nan(1) .or. ee_nan(2)) then
         write(lulist,
     >      '("              theta   :",2x,"NaN deg (to grid)",$)')
         write(lulist,'(4x,"(azimuth:",2x,"NaN deg)")')
      else
         write(lulist,
     >      '("              theta   :",2x,f6.1," deg (to grid)",$)')
     >      theta
         write(lulist,'(4x,"(azimuth:",f6.1," deg)")') az_theta
      endif
c
         return
      end subroutine o_hy3
c


      real function mconvergence(X, Y)
c
c X,Y [km]
c
         real X
         real Y


         mconvergence = 0.008257*Y+2.373*Y/X
         return

      end function mconvergence
