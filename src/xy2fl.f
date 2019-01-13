C PREVOD Y,X V S-JTSK NA ETRF89 (=WGS84) POMOCI POLYNOMU
C UPLNY 3. ST (10+10 KOEFICIENTU)
C RKOEFF OBSAHUJE KOEF. PRO FI SOURADNICI, RKOEFL PRO
C LAMBDA SOURADNICI
C PRESNOST CCA 3M

      SUBROUTINE XY2FL(Y,X,FI,RLA)
         IMPLICIT REAL*8 (A-H,O-W)
         DIMENSION RKOEFF(10),RKOEFL(10)
         FI0=50D0
         RL0=15D0
         Y0= 703000D0
         X0=1058000D0

         RKOEFF(1)= .1325132993E-02
         RKOEFF(2)=-.8916429099E-05
         RKOEFF(3)=-.1156917384E-05
         RKOEFF(4)=-.2298750250E-13
         RKOEFF(5)= .2087176527E-12
         RKOEFF(6)=-.8219794748E-12
         RKOEFF(7)= .2191874854E-19
         RKOEFF(8)= .5305545189E-20
         RKOEFF(9)= .1760134043E-18
         RKOEFF(10)=.6270628603E-20

         RKOEFL(1)=-.1019442857E-03
         RKOEFL(2)= .1794902692E-05
         RKOEFL(3)=-.1383338939E-04
         RKOEFL(4)=-.3294257309E-12
         RKOEFL(5)= .2506009659E-11
         RKOEFL(6)= .3291143794E-12
         RKOEFL(7)= .4567560092E-19
         RKOEFL(8)=-.4843979237E-18
         RKOEFL(9)=-.1182561606E-18
         RKOEFL(10)=.1641107774E-18

         DX=X-X0
         DY=Y-Y0
         FI=FI0+RKOEFF(1)+RKOEFF(2)*DX+RKOEFF(3)*DY+RKOEFF(4)*DX*DX+
     .         RKOEFF(5)*DX*DY+RKOEFF(6)*DY*DY+RKOEFF(7)*DX**3+
     .         RKOEFF(8)*DX**2*DY+RKOEFF(9)*DX*DY**2+RKOEFF(10)*DY**3
         RLA=RL0+RKOEFL(1)+RKOEFL(2)*DX+RKOEFL(3)*DY+RKOEFL(4)*DX*DX+
     .         RKOEFL(5)*DX*DY+RKOEFL(6)*DY*DY+RKOEFL(7)*DX**3+
     .         RKOEFL(8)*DX**2*DY+RKOEFL(9)*DX*DY**2+RKOEFL(10)*DY**3
         RETURN
      END subroutine XY2FL
