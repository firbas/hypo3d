c
		subroutine vl(xs,ys,xo,yo,n,v_prum,svolume)
c
c     vl       ... velocity layers
c     xs,ys    ... source coordinates
c     xo,yo    ... station coordinates
c     n        ... layer number
c     v_prum   ... (reverse value of mean velocity)*svolume
c     svolume  ... total volume
c
c     progr.: 01.00 86-09-29 mw  original version
c
		implicit none
c
		include 'param.fi'
c
		integer i,j
		integer nl
		integer n
		integer ind
		integer np
		integer nr
		integer mx,my,mz
		integer nxl,nyl,nzl
		integer nx,ny,nz
		integer nxo,nyo,nzo
		integer nxs,nys,nzs
		integer indx(x_layer)
		integer indy(y_layer)
		integer indz(z_layer)
		integer mpr((x_layer-1)+(y_layer-1)+(z_layer-1))

cc		real tm
		real pom
		real v3(x_layer,y_layer,z_layer)
		real xl(x_layer-1)
		real yl(y_layer-1)
		real zl(z_layer-1)
		real v_prum
		real volume
		real svolume
      real volume_xy
      real volume_z
      real xs,ys,zs
      real xo,yo,zo
      real so,sox,soy,soz
      real xpr((x_layer-1)+(y_layer-1)+(z_layer-1))
      real ypr((x_layer-1)+(y_layer-1)+(z_layer-1))
      real zpr((x_layer-1)+(y_layer-1)+(z_layer-1))
      real dist((x_layer-1)+(y_layer-1)+(z_layer-1))
      real d(z_layer)
      real thk(z_layer)
 
      common/model_3d/ v3
      common/layers/ nxl,xl,nyl,yl,nzl,zl
      common/zlayer/ d,nl,thk
  
      logical        exchange
      real           zair
      real           vtemp
      common/surf/   exchange,zair,vtemp
  
      svolume = 0.
      v_prum = 0.
  
      if (n.eq.nl) then
          thk(n)=1.
      endif
c
c  d-layer system is reverse
c
      zs = -d(n)
      zo = -d(n)
c.....indexy vrstev zdroje(s) a stanice(o)
      call search_l(xl,nxl,xs,nxs)
      call search_l(yl,nyl,ys,nys)
      call search_l(zl,nzl,zs,nzs)
      call search_l(xl,nxl,xo,nxo)
      call search_l(yl,nyl,yo,nyo)
      call search_l(zl,nzl,zo,nzo)
c.....smernice paprsku   ...   smerove cosiny
      sox=xo-xs
      soy=yo-ys
      soz=zo-zs
      so=sqrt(sox*sox+soy*soy+soz*soz)
      sox=sox/so
      soy=soy/so
      soz=soz/so
c.....indexy zasazenych vrstev
      np=nxo-nxs          !\
      if(np)1,2,3         ! \
    3 nx=np+1             !  \
      do 4 j=1,nx         !   \
    4 indx(j)=nxs+j-1     !    \
      go to 5             !     \ 
    2 nx=1                !      > pocet zasazenych vrstev ve smeru x 
      indx(1)=nxs         !     /  indx(j) ... indexy x zasazenych vrstev 
      go to 5             !    /
    1 nx=-np+1            !   / 
      do 6 j=1,nx         !  /
    6 indx(j)=nxs-j+1     ! / 
    5 continue            !/
      np=nyo-nys          !\
      if(np)7,8,9         ! \ 
    9 ny=np+1             !  \
      do 10 j=1,ny        !   \ 
   10 indy(j)=nys+j-1     !    \
      go to 11            !     \ 
    8 ny=1                !      >       y vrstvy 
      indy(1)=nys         !     / 
      go to 11            !    /
    7 ny=-np+1            !   / 
      do 12 j=1,ny        !  /
   12 indy(j)=nys-j+1     ! / 
   11 continue            !/
      np=nzo-nzs          !\
      if(np)13,14,15      ! \ 
   15 nz=np+1             !  \
      do 16 j=1,nz        !   \ 
   16 indz(j)=nzs+j-1     !    \
      go to 17            !     \ 
   14 nz=1                !      >      z vrstvy
      indz(1)=nzs         !     / 
      go to 17            !    /
   13 nz=-np+1            !   / 
      do 18 j=1,nz        !  /
   18 indz(j)=nzs-j+1     ! /
   17 continue            !/
c.....hledani pruseciku paprsku s hranicemi vrstev
      nr=0                !  ++++         nr bude obsah. pocet prus. s kvadry
      np=nx-1             !  @@@ pruseciky se zadanymy rozhranimi x @@@
      if(np)19,19,20      !
   20 do 21 j=1,np        !
      nr=nr+1             !
      ind=indx(j)         !
      if(sox.lt.0.) ind=ind-1
      xpr(nr)=xl(ind)     ! x souradnice  nr. pruseciku s rozhranim x
      pom=(xl(ind)-xs)/sox!
      ypr(nr)=ys+soy*pom  ! y souradnice     - " -
      zpr(nr)=zs+soz*pom  ! z souradnice     - " -
      dist(nr)=sqrt((xpr(nr)-xs)**2+(ypr(nr)-ys)**2+(zpr(nr)-zs)**2)
   21 mpr(nr)=ind         !
   19 continue            !  ----
      np=ny-1             !  ++++
      if(np)22,22,23      !  @@@ pruseciky se zadanymi rozhranimi y @@@
   23 do 24 j=1,np        !
      nr=nr+1             ! 
      ind=indy(j)         ! 
      if(soy.lt.0.) ind=ind-1 
      ypr(nr)=yl(ind)     ! 
      pom=(yl(ind)-ys)/soy! 
      xpr(nr)=xs+sox*pom  ! 
      zpr(nr)=zs+soz*pom  ! 
      dist(nr)=sqrt((xpr(nr)-xs)**2+(ypr(nr)-ys)**2+(zpr(nr)-zs)**2)
   24 mpr(nr)=ind+100     ! 
   22 continue            !  ---- 
      np=nz-1             !  ++++
      if(np)25,25,26      !  @@@ pruseciky se zadanymi rozhranimi z @@@
   26 do 27 j=1,np        !
      nr=nr+1             !
      ind=indz(j)         !
      if(soz.lt.0.) ind=ind-1
      zpr(nr)=zl(ind)     !
      pom=(zl(ind)-zs)/soz!
      xpr(nr)=xs+sox*pom  !
      ypr(nr)=ys+soy*pom  !
      dist(nr)=sqrt((xpr(nr)-xs)**2+(ypr(nr)-ys)**2+(zpr(nr)-zs)**2)
   27 mpr(nr)=ind+200     !
   25 continue            !  ----
      if(nr.eq.0) go to 37
c.....srovnani pruseciku podle vzdalenosti od zdroje
      call sort_d(dist,xpr,ypr,zpr,mpr,nr)
c.....hledani indexu vrstev, sumace
      do 29 i=1,nr
          np=mpr(i)-100
          if(np)30,30,31
   30     call search_l(yl,nyl,ypr(i),my)     !pro prusecik s plochou x=const.
          call search_l(zl,nzl,zpr(i),mz)     !dohledava indexy vrstev y,z
          mx=mpr(i)
          if(sox.lt.0.) mx=mx+1
          go to 32
   31     np=mpr(i)-200
          if(np)33,33,34
   33     call search_l(xl,nxl,xpr(i),mx)
          call search_l(zl,nzl,zpr(i),mz)
          my=mpr(i)-100
          if(soy.lt.0.) my=my+1
          go to 32
   34     call search_l(xl,nxl,xpr(i),mx)
          call search_l(yl,nyl,ypr(i),my)
          mz=mpr(i)-200
          if(soz.lt.0.) mz=mz+1
   32     continue
          if(i-1)35,35,36
   35     continue
  
          volume_xy = dist(1)
  
          if (v3(mx,my,mz) .eq. vair) then
              volume_xy=0.0
          endif
 
          volume_z = thk(n)
  
          volume = volume_xy * volume_z
          v_prum = v_prum + volume / v3(mx,my,mz)
          svolume = svolume + volume
          go to 29
   36     continue
  
          volume_xy = dist(i) - dist(i-1)
	  
          if (v3(mx,my,mz) .eq. vair) then
              volume_xy=0.0
          endif
 
          volume_z = thk(n)
  
          volume = volume_xy * volume_z
          svolume = svolume + volume
          v_prum = v_prum + volume / v3(mx,my,mz)
   29 continue
  
      volume_xy = so - dist(nr)
  
      if (v3(nxo,nyo,nzo) .eq. vair) then
          volume_xy = 0.0
      endif
  
      volume_z = thk(n)
  
      volume = volume_xy * volume_z
      svolume = svolume + volume
      v_prum = v_prum + volume / v3(nxo,nyo,nzo)
  
      return
  
   37 continue                            ! s i o jsou v jedinem bloku
      volume_xy = so
      volume_z = thk(n)
      volume = volume_xy * volume_z
      v_prum = v_prum + volume / v3(nxs,nys,nzs)
      svolume = svolume + volume
      return
      end
