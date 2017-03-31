c
C$ema /trace/,/rec/
		subroutine rt_3d_l(c_hypo,n_stat,travel_time)

c*************************************************************************
c
c compute travel time for given station no. and coordinates of hypocenter
c  from crustal model
c
c-----------------------------------------------------------------------------
c
c input parameters:
c
c  C_HYPO(3) ... coordinates of hypocenter
c  N_STAT    ... no. of station
c
c-----------------------------------------------------------------------------
c
c output parameters:
c
c  TRAVEL_TIME ... travel time from hypocenter to station no. n_stat
c
c-----------------------------------------------------------------------------
c
c progr.: 01.00  05-25-87 mw  derived from rt_3d
c
c**************************************************************************

		implicit none
c
		include 'param.fi'
		include 'list.fi'
		include 'term.fi'
c
		real travel_time
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
      real poi(2*z_layer)
      real z_coor(2*z_layer)
      real x_sour1,y_sour1
      real x_sour2,y_sour2
      real t
      real temp
      integer n_stat,ii,j,jj,l,k,ll,m
      integer j1
      integer jl
      integer nl
      integer n_poi
      integer type_of_wave                !type of wave attaching station
  
c-----------------------------------------------------------------c
c     type_of_wave  =  1 ... vlna refragovana                     c
c                      2 ... prima vlna v prvni vrstve            c
c                      3 ... vlna vedena po vrcholu vrstvy JL     c
c                      4 ... prima vlna niz nez v prvni vrstve    c
c-----------------------------------------------------------------c
  
      real   xstat(nstation),ystat(nstation),zstat(nstation)
      real   v(z_layer),vsq(z_layer)      !velocity in d-layers
      real   d(z_layer),thk(z_layer)      !structure of d-layers
      real   tid(z_layer,z_layer),did(z_layer,z_layer)
                                          !travel times from top
                                          !of layer one ... of layer nl
      real   tinj(z_layer+1),didj(z_layer+1),tr(z_layer+1)
      real   c_hypo(3)                       !x,y,z -coord. of epicenter
 
      logical exchange
  
      integer           nrec
		real                dly(nStation) 
      common /rec/      nrec,xstat,ystat,zstat,dly
                                          !characteristics of rec. arrivals
      common /vlc/      v,vsq             !velocity in d-layers
      common /zlayer/   d,nl,thk          !structure of d-layers
      common /trace/    tid,did           !travel times from top
                                          !of layer one, ..., of layer nl
c
c=============================================================================
c
c  initialize exchange variable
c
      exchange = .false.
c
c  initialization for each station
c
      if (c_hypo(3).lt.zstat(n_stat)) then
c
c  exchange coordinates of station and epicenter
c
  
d     WRITE (LULIST,*)
d     WRITE (LULIST,*) 'EXCHANGE IN ',n_stat,' STATION.'
  
          temp=xstat(n_stat)
          xstat(n_stat)=c_hypo(1)
          c_hypo(1)=temp
  
          temp=ystat(n_stat)
          ystat(n_stat)=c_hypo(2)
          c_hypo(2)=temp
  
          temp=zstat(n_stat)
          zstat(n_stat)=c_hypo(3)
          c_hypo(3)=temp
  
          exchange=.true.
  
      endif
c
c  partial distances dx,dy; total 2D distance delta
c
      dx = xstat(n_stat)-c_hypo(1)
      dy = ystat(n_stat)-c_hypo(2)
      delta = sqrt( dx**2 + dy**2 )+1.e-6
c
c  initialisation of layer system for this station
c  z-axis is downward  ...  in LAYER is upward (according model)
c
      call layer((xstat(n_stat)),(ystat(n_stat)),(-zstat(n_stat)))
c
c  calculate average velocities for this configuration of station,
c  focal and layers
c
      call velocity
     >(c_hypo(1),c_hypo(2),(xstat(n_stat)),(ystat(n_stat)))
 
cd     write (lulist,'(/" nl=",i3,/)') nl
cd     write (lulist,'(" velocity:")')
cd     write (lulist,'(10f8.3)') v
  
      if (nl.eq.1) then
          jl=nl
          tmin=999.99
          go to 90
      endif
  
      do l=1,nl
          if (d(l) .gt. c_hypo(3)) go to  2
      end do
      jl=nl
      go to  3
  
2     continue
      jj=l
      jl=l-1
  
3     continue
      tkj=c_hypo(3)-d(jl)                    !tkj ... distance from focal to the
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
70        continue
      end do
c
c  on distance less then XOVMAX may be a direct wave arrival
c
      if (delta .lt. xovmax) go to 90
c
c  travel time & derivatives for refracted wave
c
80    travel_time=tr(k)
      type_of_wave = 1
      n_poi = 2 * k - jj
      poi(1) = (thk(jl) - tkj) * v(jl) / sqrt(vsq(k) - vsq(jl))
      z_coor(1) = -d(jl+1)
c
c  travel from focal to the top of layer k
c
      do ii = 2,k-jj+1
          poi(ii) = poi(ii-1) + thk(jl+ii-1)
     >                   * v(jl+ii-1) /
     >                  sqrt(vsq(k) - vsq(jl+ii-1))
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
     >                  v(2*k-jj+2-ii) / sqrt(vsq(k)
     >                                     - vsq(2*k-jj+2-ii))
          z_coor(ii) = -d(2*k-jj+2-ii)
      end do
  
          go to 260
c
c  calculation for direct wave
c
90    if (jl .ne. 1) go to 100
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
      travel_time=tdj1
      type_of_wave = 2
  
      go to 260
c
c  find a direct wave that will emerge at the station
c
100   continue
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
140       ub=u
          delbig=delxtr
          go to 160
c
c  delta > delxtr
c
150       ul=u
          dellit=delxtr
  
160       if (ll .lt. 10) go to 170
          if (1.0-u .lt. 0.00001) go to 190
170       continue
      end do
  
180   u=(ub+ul)/2.
      usq=u**2
190   if (1.0-u .gt. 0.00001) go to 220
c
c  if u is too near 1, compute tdir as wave along the top of layer jl
c
      tdc=tid(jl,jl)+delta/v(jl)
      if (jl .eq. nl) go to 210       !focal in the lowest layer ...
                                      !only direct wave exist
      if (tdc .ge. tmin) go to 80     !refracted wave is faster
210   travel_time=tdc
c
c  along top of layer jl
c
      type_of_wave = 3
      n_poi = jl - 1
      poi(1) = delta - did(jl,jl)
c
c  test on resonable value
c
      if (poi(1).lt.0.0) then
          write(*,*) ' Error in computing of direct wave!'
          write(lulist,*) ' Error in computing of direct wave!'
          write(lulist,*)
     >    ' n_stat,delta,did,jl=',n_stat,delta,did(jl,jl),jl
          stop 999
      endif
  
      z_coor(1) = -d(jl)
      do ii = jl-1,2,-1
          poi(jl+1-ii) = poi(jl-ii) + thk(ii) *
     >                       v(ii) / sqrt(vsq(jl) - vsq(ii))
          z_coor(jl+1-ii) = -d(ii)
      end do
  
      go to 260
c
c  travel time & derivatives for direct wave below first layer
c
220   continue
      tdir=tkj/(v(jl)*sqrt(1.0-usq))  !travel time in layer concerning
                                      !focal
      do l=1,j1
          tdir=tdir+(thk(l)*v(jl))/(vsq(l)*sqrt(vsq(jl)/vsq(l)-usq))
      end do
      if (jl .eq. nl) go to 245       !only direct wave exist
      if (tdir .ge. tmin) go to 80    !refracted wave is faster
245   travel_time=tdir
  
      type_of_wave = 4
      n_poi = jl - 1
      poi(1) = tkj * u / sqrt(1.000001 - usq)
      z_coor(1) = -d(jl)
      do ii = jl-1,2,-1
          poi(jl+1-ii) = poi(jl-ii) + (thk(ii) * u) /
     >                       sqrt(vsq(jl) / vsq(ii) - usq)
          z_coor(jl+1-ii) = -d(ii)
      end do
  
260   continue
d     WRITE (LULIST,*)
d     WRITE (LULIST,*) 'TYPE OF WAVE IN STATION ',n_stat,' IS ',TYPE_OF_WAVE
d     IF (TYPE_OF_WAVE.EQ.1) THEN
d         WRITE (LULIST,*) 'REFRACTING LAYER: ',K
d     ENDIF
  
c
c-------------------------------------------------------------------------
c
c  computation of improved time for 3D-block model
c
d         WRITE (LULIST,*) 'TCAL(',n_stat,') = ',travel_time
      travel_time = 0.
      if (type_of_wave .eq. 2) then
c
c  improved arrival time for direct wave in the first layer
c  z-axis is downward  ...  in LT is upward (according model)
c
          x_temp=xstat(n_stat)
          y_temp=ystat(n_stat)
          z_temp=-zstat(n_stat)
          call lt (c_hypo(1),c_hypo(2),-c_hypo(3),
     >             x_temp,y_temp,z_temp,travel_time,0)
      else
          x_sour2 = (xstat(n_stat) - c_hypo(1)) / delta * poi(1)
     >                + c_hypo(1)
          y_sour2 = (ystat(n_stat) - c_hypo(2)) / delta * poi(1)
     >                + c_hypo(2)
c
c  z-axis is downward  ...  in LT is upward (according model)
c
          call lt(c_hypo(1),c_hypo(2),-c_hypo(3),
     >            x_sour2,y_sour2,z_coor(1),t,0)
          travel_time =travel_time + t
          x_sour1 = x_sour2
          y_sour1 = y_sour2
  
          do j = 2, n_poi
              x_sour2 = (xstat(n_stat) - c_hypo(1)) / delta * poi(j)
     >                    + c_hypo(1)
              y_sour2 = (ystat(n_stat) - c_hypo(2)) / delta * poi(j)
     >                    + c_hypo(2)
c
c  z-axis is downward  ...  in LT is upward (according model)
c
              call lt(x_sour1,y_sour1,z_coor(j-1),
     >                x_sour2,y_sour2,z_coor(j)  ,t,0)
              x_sour1 = x_sour2
              y_sour1 = y_sour2
              travel_time = travel_time + t
          end do
c
c  z-axis is downward  ...  in LT is upward (according model)
c
          x_temp=xstat(n_stat)
          y_temp=ystat(n_stat)
          z_temp=-zstat(n_stat)
          call lt (x_sour1,y_sour1,z_coor(n_poi),
     >             x_temp,y_temp,z_temp,t,0)
          travel_time =travel_time + t
      endif
  
D         WRITE (LULIST,*) '3D TCAL(',n_stat,') = ',travel_time
  
  
      if (exchange) then
  
          temp=c_hypo(1)
          c_hypo(1)=xstat(n_stat)
          xstat(n_stat)=temp
  
          temp=c_hypo(2)
          c_hypo(2)=ystat(n_stat)
          ystat(n_stat)=temp
  
          temp=c_hypo(3)
          c_hypo(3)=zstat(n_stat)
          zstat(n_stat)=temp
  
      endif
  
      return
      end
