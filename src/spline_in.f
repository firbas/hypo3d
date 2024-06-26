c
      subroutine spline_in (lu1,namr)
c
c  input of data for spline surface
c  written by lk
c
c
         character*(*) namr
         COMMON/SUR/ NX(7),NY(7),NXS(7),NYS(7),NWS(7),X(48),
     *         Y(48),W(1024),VX(5,48),VY(5,48),SIGMA(7)
         COMMON/TEM/ TEMP(48,3)
C
         write(*,*) 'SPLINE: Surface is  ', namr
         open (lu1,file=namr,status='OLD',iostat=ios)
C
         if (ios.ne.0) then
            call abort
         endif
C
         nm=0
C         lu2=0
         MM    = NM+1
         NXS(1)= 1
         NYS(1)= 1
         NWS(1)= 1
C         IF(LU2.GT.0) WRITE(LU2,90)
C
         DO 21 IM=1,MM
            READ (LU1,91) NX(IM),NY(IM)
C            IF(LU2.GT.0) WRITE(LU2,92) NX(IM),NY(IM)
C            IF (IM-1) 2,2,1
C1           CONTINUE
            IF (IM .LT. 2) GOTO 2
            NXS(IM)= NXS(IM-1)+NX(IM-1)
            NYS(IM)= NYS(IM-1)+NY(IM-1)
            NWS(IM)= NWS(IM-1)+NX(IM-1)*NY(IM-1)
2           CONTINUE
            MXS= NXS(IM)
            MYS= NYS(IM)
            MWS= NWS(IM)
            MX = NX(IM)
            MY = NY(IM)
C
            J= MXS+MX-1
            READ (LU1,93) (X(I),I=MXS,J)
C            IF(LU2.GT.0) WRITE(LU2,94) (X(I),I=MXS,J)
            J= MYS+MY-1
            READ (LU1,93) (Y(I),I=MYS,J)
C            IF(LU2.GT.0) WRITE(LU2,94) (Y(I),I=MYS,J)
            READ (LU1,97) SIGMA(IM)
C            IF(LU2.GT.0) WRITE(LU2,98) SIGMA(IM)
C
            MWS= MWS+MX*MY
            DO 3 IY=1,MY
               J= MWS-1
               MWS= MWS-MX
               READ (LU1,95) (W(I),I=MWS,J)
C            IF(LU2.GT.0) WRITE(LU2,96) (W(I),I=MWS,J)
3           CONTINUE
            READ (LU1,93,end=100)
100         CONTINUE
C100         IF(LU2.GT.0) WRITE(LU2,94)
C
            MWS= NWS(IM)
            IF (MY-1) 10, 4, 5
4           IF (MX-1) 10,21,12
5           IF (MX-1) 10,13,14
10          STOP 13
12          continue
            write (*,
     >      '("Spline_in: Error 1D surface /INTFD/.")')
            call abort
13          continue
            write (*,
     >      '("Spline_in: Error 1D surface /INTFD/.")')
            call abort
14          CALL SURFB1(MX,MY,X(MXS),Y(MYS),W(MWS),MX,W(MWS),
     *                  VX(1,MXS),VY(1,MYS),TEMP,SIGMA(IM),IERR)
            IF(IERR.EQ.1) STOP 22
            IF(IERR.EQ.2) STOP 33
21       CONTINUE
c
         close (lu1,status='KEEP')
c
         RETURN
C
C90       FORMAT(/' STRUCTURAL INTERFACES - INPUT DATA'
C     *          /' ----------------------------------')
91       FORMAT(       16I5)
C92       FORMAT(4H IF*,31I5)
93       FORMAT(       13F6.0)
C94       FORMAT(4H IF*,31F6.0)
95       FORMAT(       16F5.1)
C96       FORMAT(4H IF*,31F5.2)
97       FORMAT(       16F5.2)
C98       FORMAT(4H IF*,31F5.2)
      END subroutine spline_in
