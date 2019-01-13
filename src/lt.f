c
c
      subroutine lt(xs,ys,zs,xo,yo,zo,tm,imode)
c
c
c     lt       ... linear tracing
c
c     xs,ys,zs ... vstupni souradnice vychoziho bodu (vb) primkoveho paprsku
c     xo,yo,zo ... vstupni souradnice koncoveho bodu (kb) primkoveho paprsku
c     tm       ... vystupni cas pruchodu paprsku z vychoziho bodu do bodu
c                  koncoveho
c     imode    ... =0   vystupem je cas pruchodu paprsku
c                  =1   vystupem je rychlost v bode urcenem souradnicemi
c                       xs,ys,zs
c
c     max. pocet deleni v jednom smeru: 99
c
c
c     progr.: 01.00 86-09-29 mw upravena verze podle js
c             02.00 87-05-22 mw vypocet rychlosti v bode xs,ys,zs
c
         implicit none
c
         include 'param.fi'
c
         integer i,j
         integer imode
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

         real tm
         real pom
         real v3(x_layer,y_layer,z_layer)
         real xl(x_layer-1)
         real yl(y_layer-1)
         real zl(z_layer-1)
         real xs,ys,zs
         real xo,yo,zo
         real so,sox,soy,soz
         real xpr((x_layer-1)+(y_layer-1)+(z_layer-1))
         real ypr((x_layer-1)+(y_layer-1)+(z_layer-1))
         real zpr((x_layer-1)+(y_layer-1)+(z_layer-1))
         real dist((x_layer-1)+(y_layer-1)+(z_layer-1))

         common/model_3d/ v3
         common/layers/ nxl,xl,nyl,yl,nzl,zl

c
c  layer indexes of first point
c
         call search_l(xl,nxl,xs,nxs)
         call search_l(yl,nyl,ys,nys)
         call search_l(zl,nzl,zs,nzs)
c
c  test on computing mode
c
         if (imode.eq.1) then
c
c  only velocity in the first point
c
            tm=v3(nxs,nys,nzs)
c
c  test on reasonable value of velocity
c
            if (tm.eq.0.3) then
c
c  first point is in the air ... error
c
               write (*,'(" LT: Error ... velocity=0.3!")')
               call abort
            endif
            return
         endif
c
c  layer indexes of second point
c
         call search_l(xl,nxl,xo,nxo)
         call search_l(yl,nyl,yo,nyo)
         call search_l(zl,nzl,zo,nzo)
c
c   directione cosines ... sox, soy, soz
c
         sox=xo-xs
         soy=yo-ys
         soz=zo-zs
         so=sqrt(sox*sox+soy*soy+soz*soz)
         sox=sox/so
         soy=soy/so
         soz=soz/so
c
c  indexes of affected layers will be in indx(),indy(),indz()
c
c  x-direction:
c
         np=nxo-nxs
c
         if (np.gt.0) then
            nx=np+1
            do j=1,nx
               indx(j)=nxs+j-1
            end do
         else if (np.eq.0) then
            nx=1
            indx(1)=nxs
         else
            nx=-np+1
            do j=1,nx
               indx(j)=nxs-j+1
            end do
         endif
c
c  y-direction:
c
         np=nyo-nys
c
         if (np.gt.0) then
            ny=np+1
            do j=1,ny
               indy(j)=nys+j-1
            enddo
         else if (np.eq.0) then
            ny=1
            indy(1)=nys
         else
            ny=-np+1
            do j=1,ny
               indy(j)=nys-j+1
            end do
         endif
c
c  z-direction:
c
         np=nzo-nzs
c
         if (np.gt.0) then
            nz=np+1
            do j=1,nz
               indz(j)=nzs+j-1
            end do
         else if (np.eq.0) then
            nz=1
            indz(1)=nzs
         else
            nz=-np+1
            do j=1,nz
               indz(j)=nzs-j+1
            end do
         endif
c
c  search point of intersection of rays and interfaces of layers
c
c
c  initialize no. of points of intersection with model blocks
c
         nr=0
c
c  poi with x-interfaces
c
         np=nx-1
c
c  test on existence of poi with x-interfaces
c
         if (np.gt.0) then
c
c  exist ... do cycle over poi
c
            do j=1,np
               nr=nr+1
               ind=indx(j)
               if (sox.lt.0.) then
c
c  xs > xo ... poi with interfaces between blocks  belongs to block with
c  the less no. so decrease index
c
                  ind=ind-1
               endif
c
c  x-coord. of nr poi with interface x
c
               xpr(nr)=xl(ind)
               pom=(xl(ind)-xs)/sox
c
c  y-coord.
c
               ypr(nr)=ys+soy*pom
c
c  z-coord.
c
               zpr(nr)=zs+soz*pom
c
c  evaluate distance from first point (source) to poi nr
c
               dist(nr)=
     >         sqrt((xpr(nr)-xs)**2+(ypr(nr)-ys)**2+(zpr(nr)-zs)**2)
               mpr(nr)=ind         !
            end do
         endif
         np=ny-1                !  ++++
         if(np.le.0) go to 22   !  @@@ pruseciky se zadanymi rozhranimi y @@@
         do j=1,np              !
            nr=nr+1             !
            ind=indy(j)         !
            if(soy.lt.0.) ind=ind-1
            ypr(nr)=yl(ind)     !
            pom=(yl(ind)-ys)/soy!
            xpr(nr)=xs+sox*pom  !
            zpr(nr)=zs+soz*pom  !
            dist(nr)=sqrt((xpr(nr)-xs)**2+(ypr(nr)-ys)**2+(zpr(nr)-zs)**2)
            mpr(nr)=ind+100     !
         end do
   22    continue               !  ----
         np=nz-1                !  ++++
         if(np.le.0) go to 25   !  @@@ pruseciky se zadanymi rozhranimi z @@@
         do j=1,np              !
            nr=nr+1             !
            ind=indz(j)         !
            if(soz.lt.0.) ind=ind-1
            zpr(nr)=zl(ind)     !
            pom=(zl(ind)-zs)/soz!
            xpr(nr)=xs+sox*pom  !
            ypr(nr)=ys+soy*pom  !
            dist(nr)=sqrt((xpr(nr)-xs)**2+(ypr(nr)-ys)**2+(zpr(nr)-zs)**2)
            mpr(nr)=ind+200     !
         end do
   25    continue               !  ----
         if(nr.eq.0) go to 37
c.....srovnani pruseciku podle vzdalenosti od zdroje
         call sort_d(dist,xpr,ypr,zpr,mpr,nr)
c.....hledani indexu vrstev, sumace
         do i=1,nr
            np=mpr(i)-100
            if(np.gt.0) go to 31
            call search_l(yl,nyl,ypr(i),my)         !pro prusecik s plochou x=co
            call search_l(zl,nzl,zpr(i),mz)         !dohledava indexy vrstev y,z
            mx=mpr(i)
            if(sox.lt.0.) mx=mx+1
            go to 32
31          continue
            np=mpr(i)-200
            if(np.gt.0) go to 34
            call search_l(xl,nxl,xpr(i),mx)
            call search_l(zl,nzl,zpr(i),mz)
            my=mpr(i)-100
            if(soy.lt.0.) my=my+1
            go to 32
34          continue
            call search_l(xl,nxl,xpr(i),mx)
            call search_l(yl,nyl,ypr(i),my)
            mz=mpr(i)-200
            if(soz.lt.0.) mz=mz+1
32          continue
            if(i-1.gt.0) go to 36
            tm=dist(1)/v3(mx,my,mz)
            go to 29
36          continue
            tm=tm+(dist(i)-dist(i-1))/v3(mx,my,mz)
29          continue
         end do
         tm=tm+(so-dist(nr))/v3(nxo,nyo,nzo)
         return

37       continue                            ! s i o jsou v jedinem bloku
         tm=so/v3(nxs,nys,nzs)
         return
      end subroutine lt
