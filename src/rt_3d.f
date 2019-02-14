c
      subroutine rt_3d (c_hypo,c_stat,td,toa,exchange)

c*************************************************************************
c
c                             R T _ 3 D
c
c Compute travel time, derivatives and take off angle for given two points
c and crustal model by means of linearization approach
c
c There are two stages in computing:
c
c 1) search path in 1D model (given by some averaging of 3D model)
c 2) take into account realistic 3D model and path search out in point 1)
c
c-----------------------------------------------------------------------------
c
c input parameters:
c -----------------
c
c     C_HYPO(3) ... array of coordinates of first point (hypocenter)
c     C_STAT(3) ... array of coordinates of second point (station)
c     EXCHANGE  ... logical varible - exchange of station and hypocenter
c
c---------------------------------------------------------------------------
c
c output parameters:
c ------------------
c
c     TD(4)     ... output array: TD(1) .. travel time from point one
c                                          to point two
c                                 TD(2) .. derivative on x of travel time
c                                          in point one (hypocenter)
c                                 TD(3) .. derivative on y of travel time
c                                          in point one (hypocenter)
c                                 TD(4) .. derivative on z of travel time
c                                          in point one (hypocenter)
c     TOA       ... take off angle of ray from point one (hypocenter) with
c                   respect to z-axis
c-----------------------------------------------------------------------------
c external references:
c --------------------
c function isnan() - GNU extension
c          LT
c          TERM 
c-----------------------------------------------------------------------------
c progr.: 01.00               original version
c         02.00     05.86 mw  improved version
c         02.00  19.06.86 mw  'spy on' travel path in 3d model
c         03.00  29.09.86 mw  general positions of stations in 3d model
c         03.01  17.12.86 mw  computed derivatives for general positions
c                             of stations in 3d
c         04.00  12.02.87 mw  streamlined computation of derivatives
c         05.00  16.06.87 mw  new version ... input,output parameters
c        10.72 2019-02-10 pz  toas=sin(take-off angle), test isnan()
c-----------------------------------------------------------------------------
c 2019-02-10 pz
c This subroutine has been identified as a modification of TRVDRV
c writen by J.P. Eaton (HYPOLAYR, 1969), 
c which is part of  HYPO71, LEE AND LAHR (USGS OPEN-FILE REPORT 75-311, 1975).
c**************************************************************************

         implicit none
c
c  include files ... global constants, variables
c
         include 'param.fi'
c
c  input parameters
c
         real    c_hypo(3)
         real    c_stat(3)
         logical exchange
c
c  output parameters
c
         real    td(4)
         real    toa
c
c  local variables
c
      real(8) toas  
      real(8) toac 

         real x_temp,y_temp,z_temp
         real xovmax
         real delxtr
         real dellit
         real delbig
         real xtest
         real delta
         real tdir
         real tmin
         real tdc
         real tdj1
         real usq
         real sqt
         real ub
         real ubsq
         real ul
         real ulsq
         real tkj
         real tkjsq
         real xlit
         real xbig
         real dx
         real dy
         real u
         real u1
c
c
         integer n_poi
         real poi(2*z_layer)
         real z_coor(2*z_layer)
c
         real x_sour1,y_sour1
         real x_sour2,y_sour2
         real t
         integer ii,j,jj,l,k,ll,m
         integer j1
         integer jl
         integer type_of_wave                !type of wave attaching station

c-----------------------------------------------------------------c
c     type_of_wave  =  1 ... refracted wave                       c
c                      2 ... direct wave in first layer           c
c                      3 ... head wave along top of layer JL      c
c                      4 ... direct wave below first layer        c
c-----------------------------------------------------------------c

         real   tinj(z_layer+1),didj(z_layer+1),tr(z_layer+1)
         real   v_hypo                       !velocity in first point (hypoc.)
                                             !surface coordinate and deriv.
c
c-----------------------------------------------------------------------------
c
c  common for velocity in 1D model
c
         real   v(z_layer),vsq(z_layer)      !velocity in d-layers
         common /vlc/      v,vsq             !velocity in d-layers
c
c-----------------------------------------------------------------------------
c
c  common for structure of 1D model
c
         integer nl
         real   d(z_layer),thk(z_layer)      !structure of d-layers
         common /zlayer/   d,nl,thk          !structure of d-layers
c
c-----------------------------------------------------------------------------
c
c  common for terms tid, did
c
         real   tid(z_layer,z_layer),did(z_layer,z_layer)
         common /trace/    tid,did           !travel times from top
                                             !of layer one, ..., of layer nl
c
c===========================================================================
c
c
c  velocity in hypocenter
c
         call lt(c_hypo(1),c_hypo(2),-c_hypo(3),0.0,0.0,0.0,v_hypo,1)
c
c  partial distances dx,dy; total 2D distance delta
c
         dx = c_stat(1)-c_hypo(1)
         dy = c_stat(2)-c_hypo(2)
         delta = sqrt( dx**2 + dy**2 )+1.e-6

         if (nl.eq.1) then
            jl=nl
            tmin=999.99
            go to 90
         endif

         do l=1,nl
            if (d(l) .gt. c_hypo(3)) go to  2
         end do
c
c  first point is under all layers
c
         jl=nl
         go to  3

2        continue
c
c  first point is in layer l
c
         jj=l
         jl=l-1

3        continue
         tkj=c_hypo(3)-d(jl)                 !tkj ... distance from focal to the
                                             !nearest higher interface
         tkjsq=tkj**2+0.000001
c
c  calculate TID, DID
c
         call term
c
c  test for refracted wave existence
c
         if (jl .eq. nl) then
c
c  only direct wave exist
c
            go to 100
         endif

         do l=jj,nl
            sqt=sqrt(vsq(l)-vsq(jl))
            tinj(l)=tid(jl,l)-tkj*sqt/(v(l)*v(jl))
            didj(l)=did(jl,l)-tkj*v(jl)/sqt
         end do
         xovmax=v(jj)*v(jl)*(tinj(jj)-tid(jl,jl))/(v(jj)-v(jl))
         do m=jj,nl
            tr(m)=tinj(m)+delta/v(m)
         end do
c
c  searching for the minimum arrival time for refracted wave
c
         tmin=999.99
         do m=jj,nl
            if (tr(m) .gt. tmin) go to 70
            if (didj(m) .gt. delta) go to 70
            k=m
            tmin=tr(m)
70          continue
         end do
c
c  on distance less then XOVMAX may be a direct wave arrival
c
         if (delta .lt. xovmax) go to 90
c
c  travel time & derivatives for refracted wave
c
80       continue

c
c  set type of wave
c
         type_of_wave = 1
c
c  no. of points of intersection (of linear sections)
c
         n_poi = 2 * k - jj
c
c  set coordinates of linear sections; poi    ... profil coordinate
c                                      z_coor ... z coordinate
c
         poi(1) = (thk(jl) - tkj) * v(jl) / sqrt(vsq(k) - vsq(jl))
         z_coor(1) = -d(jl+1)
c
c  travel from focal to the top of layer k
c
         do ii = 2,k-jj+1
            poi(ii) = poi(ii-1) + thk(jl+ii-1)
     >                     * v(jl+ii-1) /
     >                    sqrt(vsq(k) - vsq(jl+ii-1))
            z_coor(ii) = -d(jl+ii)
         end do
c
c  travel along the top of layer k
c
         poi(k-jj+2) = poi(k-jj+1) + delta - didj(k)
         z_coor(k-jj+2) = -d(k)
c
c  from the top of layer k to the station
c
         do ii = k-jj+3,2*k-jj
            poi(ii) = poi(ii-1) + thk(2*k-jj+2-ii) *
     >                    v(2*k-jj+2-ii) / sqrt(vsq(k)
     >                                       - vsq(2*k-jj+2-ii))
            z_coor(ii) = -d(2*k-jj+2-ii)
         end do

         if (exchange) then
            toas=-v(1)/v(k)
         else
            toas=-v(jl)/v(k)
         endif
         go to 260
c
c  calculation for direct wave
c
90       if (jl .ne. 1) go to 100
c
c  direct wave in first layer
c
         sqt=sqrt((c_hypo(3)-d(1))**2+delta**2)
         tdj1=sqt/v(1)
         if (tdj1 .ge. tmin) then
c
c  refracted wave is faster
c
            go to 80
         endif
c
c  travel time & derivatives for direct wave in first layer
c

         type_of_wave = 2
         n_poi=0

         if (exchange) then
            toas=-delta/sqt
         else
            toas=delta/sqt
         endif

         go to 260
c
c  find a direct wave that will emerge at the station
c
100      continue
         xbig=delta
         xlit=delta*tkj/(c_hypo(3)-d(1))
         ub=xbig/sqrt(xbig**2+tkjsq)     !sin alfa_max ... maximal angle
                                         !                 of incidence
         ul=xlit/sqrt(xlit**2+tkjsq)     !sin alfa_min ... minimal angle
         ubsq=ub**2
         ulsq=ul**2
c
c  distance of arrival of direct wave with angle of incidence
c  arcsin(ub) [resp. arcsin(ul)]
c
         delbig=tkj*ub/sqrt(1.000001-ubsq)
         dellit=tkj*ul/sqrt(1.000001-ulsq)
         j1=jl-1
         do l=1,j1
            delbig=delbig+(thk(l)*ub)/sqrt(vsq(jl)/vsq(l)-ubsq)
            dellit=dellit+(thk(l)*ul)/sqrt(vsq(jl)/vsq(l)-ulsq)
         end do
c
c  delbig < delta ... only direct wave along top of layer jl
c
         if (delbig.lt.delta) then
            u=1.
            go to 190
         endif
c
c  iteration cyclus for searching appropriate angle of incidence
c  (wave hit station with selected error {+- 0.001 km})
c
         do ll=1,30
            if (delbig-dellit .lt. 0.001) go to 180
            u=(ub+ul)/2.
            usq=u**2
            delxtr=tkj*u/sqrt(1.000001-usq)
            do  l=1,j1
               delxtr=delxtr+(thk(l)*u)/sqrt(vsq(jl)/vsq(l)-usq)
            end do
            xtest=delta-delxtr
            if (abs(xtest) .le. 0.001) go to 190
            if (xtest) 140,190,150
c
c  delta < delxtr
c
140         ub=u
            delbig=delxtr
            go to 160
c
c  delta > delxtr
c
150         ul=u
            dellit=delxtr

160         if (ll .lt. 10) go to 170
            if (1.0-u .lt. 0.00001) go to 190
170         continue
         end do

180      u=(ub+ul)/2.
         usq=u**2
190      if (1.0-u .gt. 0.00001) go to 220
c
c  if u is too near 1, compute tdir as wave along the top of layer jl
c
         tdc=tid(jl,jl)+delta/v(jl)
         if (jl .eq. nl) go to 210       !focal in the lowest layer ...
                                         !only direct wave exist
         if (tdc .ge. tmin) go to 80     !refracted wave is faster
210      continue

c
c  along top of layer jl
c
         type_of_wave = 3
         n_poi = jl - 1
         poi(1) = delta - did(jl,jl)
c
c
         z_coor(1) = -d(jl)
         do ii = jl-1,2,-1
            poi(jl+1-ii) = poi(jl-ii) + thk(ii) *
     >                         v(ii) / sqrt(vsq(jl) - vsq(ii))
            z_coor(jl+1-ii) = -d(ii)
         end do

         if (exchange) then
            toas=-v(1)/v(jl)
         else
            toas=1D0
         endif

         go to 260
c
c  travel time & derivatives for direct wave below first layer
c

220      continue
         tdir=tkj/(v(jl)*sqrt(1.0-usq))  !travel time in layer concerning
                                         !focal
         do l=1,j1
            tdir=tdir+(thk(l)*v(jl))/(vsq(l)*sqrt(vsq(jl)/vsq(l)-usq))
         end do
         if (jl .eq. nl) go to 245       !only direct wave exist
         if (tdir .ge. tmin) go to 80    !refracted wave is faster
245      continue

         type_of_wave = 4
         n_poi = jl - 1
         poi(1) = tkj * u / sqrt(1.000001 - usq)
         z_coor(1) = -d(jl)

         do ii = jl-1,2,-1
            poi(jl+1-ii) = poi(jl-ii) + (thk(ii) * u) /
     >                         sqrt(vsq(jl) / vsq(ii) - usq)
            z_coor(jl+1-ii) = -d(ii)
         end do

         if (exchange) then
            u1=v(1)/v(jl)*u
            toas=-u1
         else
            toas=u
         endif

260      continue
c
c-------------------------------------------------------------------------
c
c  computation of improved time for 3D-block model
c

         td(1) = 0.
         if (type_of_wave .eq. 2) then
c
c  improved arrival time for direct wave in the first layer
c  z-axis is downward  ...  in LT is upward (according model)
c
            x_temp=c_stat(1)
            y_temp=c_stat(2)
            z_temp=-c_stat(3)
c
c  path from first to second point is in one layer (one linear section)
c
            call lt (c_hypo(1),c_hypo(2),-c_hypo(3),
     >               x_temp,y_temp,z_temp,td(1),0)
         else
c
c  path from first to second point concern more then one linear section
c
            x_sour2 = (c_stat(1) - c_hypo(1)) / delta * poi(1)
     >                  + c_hypo(1)
            y_sour2 = (c_stat(2) - c_hypo(2)) / delta * poi(1)
     >                  + c_hypo(2)
c
c  z-axis is downward  ...  in LT is upward (according model)
c
c
c  travel time for one linear section
c
            call lt(c_hypo(1),c_hypo(2),-c_hypo(3),
     >              x_sour2,y_sour2,z_coor(1),t,0)
            td(1) =td(1) + t
            x_sour1 = x_sour2
            y_sour1 = y_sour2

            do j = 2, n_poi
               x_sour2 = (c_stat(1) - c_hypo(1)) / delta * poi(j)
     >                     + c_hypo(1)
               y_sour2 = (c_stat(2) - c_hypo(2)) / delta * poi(j)
     >                     + c_hypo(2)
c
c  z-axis is downward  ...  in LT is upward (according model)
c
               call lt(x_sour1,y_sour1,z_coor(j-1),
     >                 x_sour2,y_sour2,z_coor(j)  ,t,0)
               x_sour1 = x_sour2
               y_sour1 = y_sour2
               td(1) = td(1) + t
            end do
c
c  z-axis is downward  ...  in LT is upward (according model)
c
            x_temp=c_stat(1)
            y_temp=c_stat(2)
            z_temp=-c_stat(3)
            call lt (x_sour1,y_sour1,z_coor(n_poi),
     >               x_temp,y_temp,z_temp,t,0)
            td(1) =td(1) + t
         endif

c
c  derivative on x  of travel time
c
         td(2)=real(-1./v_hypo*(c_stat(1)-c_hypo(1))/delta*abs(toas))
c
c  derivative on y
c
         td(3)=real(-1./v_hypo*(c_stat(2)-c_hypo(2))/delta*abs(toas))
c
c  test on exchange of station and hypocenter
c
         if (exchange) then
c
c  derivatives on x and on y will be changed to opposite!
c
            td(2)=-td(2)
            td(3)=-td(3)
         endif
c
c === 2019-02-10 v10.72 pz ===========================================
c  derivative on z
         td(4)=0.0
         toac = dsqrt(1D0-toas*toas)
         if(.not. isnan(toac)) then
            td(4)=real(1.D0/v_hypo*dsign(toac,toas))
         endif

c  conversion to degrees
         toa=real(asin(-toas)/(4.0*atan(1.0))*180.0)
         if (toa < 0.0) toa = toa+180
c ====================================================================

c  Convention:                                                        c
c          if  toa < 0.0  then take-off angle alfa                    c
c                                 is given by equation                c
c                                                                     c
c              alfa=pi-arcsin(abs(toa))                               c
c---------------------------------------------------------------------c
         return
      end subroutine rt_3d
