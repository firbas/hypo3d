c
      subroutine spline_value (IN,R,ZD)
c            subroutine spline_value (IN,R,ZD,NDER)
c
c
c  Parameter IN has in program HYPO3D value 0 ... input parameter
c  Parameter R  ... input array with coordinates x, y
c  Parameter ZD ... output parameter ... ZD(1) is z-coordinate of surface
c                   in the point with coordinates given in R
c  Parameter NDER is not used ... it's value is constantly 0 (in HYPO3D)
c
c
c  calling convention:   CALL SPLINE_VALUE (0,R,ZD,0)
c
c
c  87-02  01.00 lk original version
c  87-06  01.01 mw modified version
c
c
         DIMENSION   R(2),ZD(6)
         COMMON/SUR/ NX(7),NY(7),NXS(7),NYS(7),NWS(7),X(48),
     *          Y(48),W(1024),VX(5,48),VY(5,48),SIGMA(7)
c
c mw
c |
c |
c
c  test on presence of epicenter in array
c
         if ( r(1).lt.x(1) ) then
            r(1)=x(1)
         else if ( r(1).gt.x(nx(1)) ) then
            r(1)=x(nx(1))
         endif
c
         if ( r(2).lt.y(1) ) then
            r(2)=y(1)
         else if ( r(2).gt.y(ny(1)) ) then
            r(2)=y(ny(1))
         endif
c |
c |
c mw
         IM = IN+1
         MXS= NXS(IM)
         MYS= NYS(IM)
         MWS= NWS(IM)
         DO 1 I=2,6
1        ZD(I)= 0.
         IF (NY(IM)-1)  2, 2, 3
2        IF (NX(IM)-1) 11,11,12
3        IF (NX(IM)-1) 13,13,14
11       ZD(1)= W(MWS)
         RETURN
12       write (*,'("Spline_value: Error - 1D surface.")')
         call abort
13       write (*,'("Spline_value: Error - 1D surface.")')
         call abort
14       CALL SURFBD(R(1),R(2),ZD(1),ZD(2),ZD(3),ZD(4),ZD(5),ZD(6),
     *               NX(IM),NY(IM),X(MXS),Y(MYS),W(MWS),
     *               VX(1,MXS),VY(1,MYS),SIGMA(IM))
         RETURN
      END subroutine spline_value
