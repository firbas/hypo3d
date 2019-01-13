c
      subroutine search_l(sl,nsl,sf,nsf)
c
c*****************************************************************************
c
c  subroutine SEARCH_L
c
c-----------------------------------------------------------------------------
c
c  purpose:
c
c     given  ... 1) layer system (NSL layers, coordinates in SL array)
c                2) point of coordinate SF
c     search ... NSF - number of layer concerning point with coord. SF
c
c-----------------------------------------------------------------------------
c
c  formal parameters:
c
c     integer     SL(1)       ...  array with coord. of layers      I
c     integer     NSL         ...  no. of layers                    I
c     integer     SF          ...  coord. of point                  I
c     integer     NSF         ...  no. of layer conc. point         O
c
c----------------------------------------------------------------------------
c
c  calling convention:
c
c     call search_l(sl,nsl,sf,nsf)
c
c----------------------------------------------------------------------------
c
c  external references:
c
c     none
c
c----------------------------------------------------------------------------
c
c  programmed:  86-09  01.00  mw
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
         real sl(100)                    !sl(1)
         real sf
         integer nsl
         integer nsf
c
c  local parameters  ...  none
c
c
c  global parameters  ...  none
c
c
c  local variables
c
         integer n1
         integer n2
         integer np
c
c  global variables  ...  none
c
c
c  functions  ...  none
c
c
c  *******************
c  end of declarations
c  *******************
c
c=============================================================================
c
         if (sf .le. sl(1)) then
c
c  point is in the first layer
c
            nsf = 1
            go to 10
         endif
c
         if (sf .gt. sl(nsl-1)) then
c
c  point is in the last layer
c
            nsf = nsl
            go to 10
         endif
c
c     search by the method of halved intervals
c
c     point on the interfaces between the layers belongs to layer
c     with the less index
c
         n1 = 1
         n2 = nsl
         do while (n2-n1-1 .gt. 0)
            np = (n1 + n2) / 2
            if (sf .le. sl(np)) then
               n2 = np
            else
               n1 = np
            endif
         end do
         nsf = n2
c
10       continue
         return
      end subroutine search_l
