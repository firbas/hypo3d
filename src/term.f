c
		subroutine term
c
c computation if TID, DID
c
c     progr.: 01.00 86-09-29 mw original version derived from HYPO71
c
		implicit none
c
      include 'param.fi'
c
c  local variables
c
		real f(z_layer,z_layer)
		real tim
		real dim
		real sqt
		integer j,l,m
c
c  global variables ... common blocks
c
		real            v(z_layer)
		real            vsq(z_layer)
		common /vlc/    v,vsq
c
		integer         nl
      real            d(z_layer)
      real            thk(z_layer)
      common /zlayer/ d,nl,thk
c
      real            tid(z_layer,z_layer)
      real            did(z_layer,z_layer)
      common /trace/  tid,did
c
c=============================================================================
c
c  compute tid and did
c
      do j=1,nl
          do l=1,nl
              f(l,j)=1.
              if (l .ge. j) f(l,j)=2.
          end do
      end do
c
      do j=1,nl
          do m=1,nl
              tid(j,m)=0.
              did(j,m)=0.
          end do
      end do
c
      do j=1,nl
          do m=j,nl
              if (m .eq. 1) go to 10
              do l=1,m-1
                  sqt=sqrt(vsq(m)-vsq(l))
                  tim=thk(l)*sqt/(v(l)*v(m))
                  dim=thk(l)*v(l)/sqt
                  tid(j,m)=tid(j,m)+f(l,j)*tim
                  did(j,m)=did(j,m)+f(l,j)*dim
              end do
  10          continue
          end do
      end do
c
      return
      end
