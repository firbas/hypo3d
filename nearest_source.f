c
		subroutine nearest_source(source_flag,x_hypo,y_hypo,z_hypo)
c
c*****************************************************************************
c
c  subroutine NEAREST_SOURCE
c
c-----------------------------------------------------------------------------
c
c  purpose:
c
c     search for the sources given in user file, which are close to hypocenter
c
c-----------------------------------------------------------------------------
c
c  formal parameters:
c
c     logical     SOURCE_FLAG ...  file successfully open           I
c     real*4      X_HYPO      ...  x coord. of hypocenter           I
c     real*4      Y_HYPO      ...  y coord. of hypocenter           I
c     real*4      Z_HYPO      ...  z coord. of hypocenter           I
c
c----------------------------------------------------------------------------
c
c  calling convention:
c
c     call nearest_source (source_flag,x_hypo,y_hypo,z_hypo)
c
c----------------------------------------------------------------------------
c
c  external references:
c
c     DATUM               mw subroutine
c     REAL8_TO_INT        mw subroutine
c
c----------------------------------------------------------------------------
c
c  programmed:  87-07  01.00  mw
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
		real    x_hypo
		real    y_hypo
		real    z_hypo
		logical source_flag
c
c  local parameters
c
		real    near_level
		integer nsource_list
		parameter (nsource_list=3)
		parameter (near_level=10.0)
c
c  global parameters
c
		include 'source.fi'
		include 'pname.fi'
c
c  local variables
c
		integer ios
		integer i
		real    xp
		real    yp
		real    zp
c
c  global variables
c
		real         x(max_source)
		real         y(max_source)
		real         z(max_source)
		real         delta(max_source)
		integer      nsource
		character*30 source_name(max_source)
		common /ns1/ x,y,z,delta,nsource
      common /ns2/ source_name
c
c  functions
c
c
c  *******************
c  end of declarations
c  *******************
c
c=============================================================================
c
c  test on successfully opened source file
c
      if (source_flag) then
c
c  transform local coord. to Krovak
c
          xp=x_hypo
          yp=y_hypo
          zp=z_hypo
c
c  test on 3D case
c
          if (prog_name.eq.'HYPO3D') then
              call trans (xp,yp,zp,0)
          endif
c
c  read in sources names, sources coordinates
c   evaluate distances to hypocenter
c
          i=0
          do while(.true.)
              i=i+1
              read (lusource,*,iostat=ios,end=10)
     >        x(i),y(i),z(i),source_name(i)
c
c  test on error
c
              if (ios.ne.0) then
cc                  call ReportError(ios,'SOURCE','NEAREST_SOURCE')
c
c  no list of sources
c
                  go to 20
              endif
c
c  evaluate distance
c
              delta(i)=sqrt( (xp-x(i))**2+(yp-y(i))**2+
     >        (zp-z(i))**2 )
          end do
      else
          go to 20
      endif
c
10    continue
c
      nsource=i-1
c
c  sort sources by hypocentral distance
c
      call sort_source
c
      if (delta(1).lt.near_level) then
c
c  pause on return
c
cc         call Pause
c
c  clear display
c
cc          call clear_display
c
          write (1,'(1x,a,":   Known sources nearby the hypocenter",/)')
     >    prog_name
          write (1,'(11x,"Delta      X      Y      Z      Name")')
          write (1,'(11x," [km]     [km]   [km]   [km]")')
          write (1,'(11x,"------------------------------------")')
          do i=1,nsource_list
              if (delta(i).lt.near_level) then
                  write (1,'(11x,f5.1,4x,3(f5.1,2x),1x,a)')
     >            delta(i),x(i),y(i),z(i),source_name(i)
              endif
          end do
c
          write (1,'(/)')
c
c  pause on return
c
cc          call Pause
c
c  clear display
c
cc          call clear_display
      endif
c
20    continue
c
c  rewind the source file
c
      rewind (lusource)
c
      return
      end
