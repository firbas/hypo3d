
      subroutine split( line, vals, n )
c https://stackoverflow.com/questions/32022062/reading-a-sequence-of-integer-in-a-line-with-unknown-bound-in-fortran
      implicit none
      character(*) line
      real  vals(*)
      integer n

      real  buf( 10 ) 

      n = 1
      do
         read(line,*, end=100, err=100) buf(1:n)
         vals(1:n) = buf(1:n)
         n = n + 1
      end do
100   continue
      n = n - 1
      end subroutine split

      subroutine read_model_header(lu)
c
      implicit none
      integer         lu

c  global variables
      real  model_error
      real  reading_error
      common /err/ model_error, reading_error

      real            nangle
      common /nangl/  nangle

      real             p_fi
      real             p_x_shift, p_y_shift
      common /p_posun/ p_fi, p_x_shift, p_y_shift

      real    p_over_s   !p_velocity / s_velocity = sqrt(3.)
      real    vpA, vpB
      common /p_over_s/   p_over_s, vpA, vpB

c local variables
      real          l_param(5)
      integer       n_param
      integer         ios
      character(256)  iom
      character(100)  line

c line number 1st
c vP_over_vS, vpA (optional), vpB (optional)
      p_over_s=1.70
      vpA=0.0
      vpB=1.0

      l_param=-1.0
      line=""
      read (lu,'(A)',err=90,iostat=ios,end=90,iomsg=iom) line
      call split(trim(line), l_param, n_param)
      if (n_param .gt. 0) then
        p_over_s = l_param(1)
        vpA= 0.0
        vpB= 1.0
      end if
      if (n_param .eq. 3) then
        vpA=l_param(2)
        vpB=l_param(3)
      end if
   
c line number 2nd
c mode coordinates rotation to Krovak coordinates
      read (lu,*,err=90,iostat=ios,end=90,iomsg=iom) p_fi

c line number 3th, 4th
c origin of model coordinates relative to Krovak coordinates
      read (lu,*,err=90,iostat=ios,end=90,iomsg=iom) p_x_shift
      read (lu,*,err=90,iostat=ios,end=90,iomsg=iom) p_y_shift

c line number 5th.
c model_error, reading_error (optional)
c On line 5, you can enter two parameters:
c         model_error as the first mandatory argument
c         and reading_error as second optional argument.
c Both parameters are entered in seconds.

      model_error=0.0        ! default 0.0 sec
      reading_error=0.016    ! default 0.016 sec

      l_param=-1.0
      line=""
      read (lu,'(A)',err=90,iostat=ios,end=90,iomsg=iom) line
      call split(trim(line), l_param, n_param)
      if (n_param .gt. 0) then
         model_error=l_param(1)
      end if
      if (n_param .eq. 2) then
         reading_error=l_param(2)
      end if
   
c angle between x-axis and north
      read (lu,*,err=90,iostat=ios,end=90,iomsg=iom) nangle
   
      return

c  error in reading
90    continue
      if (ios/=0) then
         write(*,*) 'iostat = ', ios
         write(*,*) 'iomsg: '//trim(iom)
         stop
      end if
      end subroutine read_model_header


      subroutine read_model_sta(lu)
c
      implicit none
      integer         lu

c  global parameters
      include 'param.fi'
      include 'pname.fi'

c  global variables
      integer         nrec
      real            xstat(nStation)
      real            ystat(nStation)
      real            zstat(nStation)
      real            dly(nStation)
      real            dly_s(nStation)
      common /rec/    nrec,xstat,ystat,zstat,dly,dly_s

      integer         nstat
      character*5     stat_name(nStation)
      common /stnam/  nstat,stat_name

c  local variables
      real            x_temp
      real            y_temp
      real            z_temp
      integer         j
      integer         ios
      character(256)  iom
      character(100)  line
      real            delay_p,delay_s
c  read number of stations
      read (lu,*,err=90,iostat=ios,end=90,iomsg=iom) nstat
c  test on number of stations:  nstat requested; nStation dimensioned
      if(nstat.gt.nStation)then
         write (*,'(1x,a,": Too many stations. Max:",i3)')  nStation
         call abort
      endif

c  load station list
      do j = 1, nstat
         read(lu,'(A)') line
c  to handle the case where delay values are not enntered
         line=trim(line)//" -99.9 -99.9"
         read (line,*,err=90,iostat=ios,end=90,iomsg=iom)
     >         stat_name(j),xstat(j),ystat(j),zstat(j),delay_p,delay_s
         if (delay_p<-99.0) then
            delay_p=0.0
            delay_s=0.0
         else if (delay_s<-99.0) then
            delay_s=delay_p
         end if
         dly(j)=delay_p
         dly_s(j)=delay_s
         write(*,*) stat_name(j),dly(j),dly_s(j)
c  z-coordinate in model file is upward
c  --> set to downward
         zstat(j)=-zstat(j)

c  transform station coordinates  Krovak to local
         x_temp=xstat(j)
         y_temp=ystat(j)
         z_temp=zstat(j)
         call Trans (x_temp,y_temp,z_temp,1)
         xstat(j)=x_temp
         ystat(j)=y_temp
         zstat(j)=z_temp
      end do

      return

c  error in reading
90    continue
      if (ios/=0) then
         write(*,*) 'iostat = ', ios
         write(*,*) 'iomsg: '//trim(iom)
         stop
      end if
      end subroutine read_model_sta

      function vs_vp(vp)
c
      implicit none
      real vs_vp
      real vp
c global variables 
      real    p_over_s                    !p_velocity / s_velocity = sqrt(3.)
      real    vpA, vpB
      common /p_over_s/   p_over_s, vpA, vpB
c local
      real vpovs
      
      vpovs=p_over_s

      if (vp .ge. vpB) then
         vs_vp=vp/vpovs
      else
         vs_vp=(vp-vpA)/(vpB-vpA)*vpB/vpovs
      end if

      end function vs_vp


      subroutine read_model_vel(lu)

c ----------------------------------------------------------------------
c  With the update to version 10.79, the program now allows the input
c  of two velocity models for P-wave and S-wave velocities.
c  The velocity model v_p is entered explicitly and the velocity model
c  v_s can be entered:
c   1. By constant velocity ratio v_s/v_p, as before.
c   2. By entering the parameters of a function representing
c      the relationship between velocity v_s and velocity v_p.
c   3. By explicitly entering two velocity models for v_s and v_p.
c
c  The segments of the velocity model are labelled as "wave S".
c  However, the first segment corresponding to the P-waves is
c  not labelled as such for compatibility with older versions.
c ----------------------------------------------------------------------
      implicit none
      integer lu

      include 'param.fi'
c global
      real                xl(x_layer-1)
      real                yl(y_layer-1)
      real                zl(z_layer-1)
      integer             nxl
      integer             nyl
      integer             nzl
      common /layers/     nxl,xl,nyl,yl,nzl,zl

c ----------------------------------------------------------------------
c 2024-04-22 pz
c two velocity models
         real v3p(x_layer,y_layer,z_layer)
         real v3s(x_layer,y_layer,z_layer)
         common /model_3d/ v3p,v3s
c ----------------------------------------------------------------------

c local
      integer     ix
      integer     iy
      integer     iz
      integer     np
      integer     j
      integer         ios
      character(256)  iom
      character(100)  line
c functions
      real vs_vp

c  number of layers
      !skip one comment line
      !read (lu,'(A)',err=90,iostat=ios,end=90,iomsg=iom) line
      read (lu,*,err=90,iostat=ios,end=90,iomsg=iom)
      read (lu,*,err=90,iostat=ios,end=90,iomsg=iom) nxl,nyl,nzl

c  x-interfaces
      !skip one comment line
      read (lu,*,err=90,iostat=ios,end=90,iomsg=iom)
      np=nxl-1
      read(lu,*,err=90,iostat=ios,end=90,iomsg=iom)(xl(j),j=1,np)

c  y-interfaces
      !skip one comment line
      read (lu,*,err=90,iostat=ios,end=90,iomsg=iom)
      np=nyl-1
      read(lu,*,err=90,iostat=ios,end=90,iomsg=iom)(yl(j),j=1,np)

c  z-interfaces
      !skip one comment line
      read (lu,*,err=90,iostat=ios,end=90,iomsg=iom)
      np=nzl-1
      read(lu,*,err=90,iostat=ios,end=90,iomsg=iom)(zl(j),j=1,np)

c  read P velocities
      do iy = 1,nyl
        !skip one comment line
        read (lu,*,err=90,iostat=ios,end=90,iomsg=iom)
        do ix = 1,nxl
          read (lu,*,err=90,iostat=ios,end=90,iomsg=iom)
          read (lu,*,err=90,iostat=ios,end=90,iomsg=iom)
     >        (v3p(ix,iy,iz),iz = 1,nzl)
        end do
      end do

      read (lu,'(A)',err=90,iostat=ios,end=70,iomsg=iom) line
      if ((index(line,"wave") .eq. 1) .and. (index(line,"S") .gt. 0)) then 
c  read S velocities
      do iy = 1,nyl
         !skip one comment line
         read (lu,*,err=90,iostat=ios,end=90,iomsg=iom)
         do ix = 1,nxl
            !skip one comment line
            read (lu,*,err=90,iostat=ios,end=90,iomsg=iom)
            read (lu,*,err=90,iostat=ios,end=90,iomsg=iom)
     >          (v3s(ix,iy,iz),iz = 1,nzl)
         end do
      end do
   
      return
      end if

70    continue
c  calculate S velocities
c   v3s=v3p/p_over_s
   
      do iy = 1,nyl
         do ix = 1,nxl
             do iz = 1,nzl
c               v3s(ix,iy,iz)=v3p(ix,iy,iz)/p_over_s
                v3s(ix,iy,iz)=vs_vp(v3p(ix,iy,iz))
             end do
         end do
      end do

      return

c  error in reading
90    continue
      if (ios/=0) then
         write(*,*) 'iostat = ', ios
         write(*,*) 'iomsg: '//trim(iom)
         stop
      end if
      end subroutine read_model_vel
