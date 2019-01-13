C      LB:[34,13]EIGEN.FTN;1
C     TITLE 780001-126  IBM - SUBROUTINE EIGEN
C
C     ..................................................................
C
C        SUBROUTINE EIGEN
C
C        PURPOSE
C           COMPUTE EIGENVALUES AND EIGENVECTORS OF A REAL SYMMETRIC
C           MATRIX
C
C        USAGE
C           CALL EIGEN(A,R,N,MV)
C
C        DESCRIPTION OF PARAMETERS
C           A - ORIGINAL MATRIX (SYMMETRIC), DESTROYED IN COMPUTATION.
C               RESULTANT EIGENVALUES ARE DEVELOPED IN DIAGONAL OF
C               MATRIX A IN DESCENDING ORDER.
C           R - RESULTANT MATRIX OF EIGENVECTORS (STORED COLUMNWISE,
C               IN SAME SEQUENCE AS EIGENVALUES)
C           N - ORDER OF MATRICES A AND R
C           MV- INPUT CODE
C                   0   COMPUTE EIGENVALUES AND EIGENVECTORS
C                   1   COMPUTE EIGENVALUES ONLY (R NEED NOT BE
C                       DIMENSIONED BUT MUST STILL APPEAR IN CALLING
C                       SEQUENCE)
C
C        REMARKS
C           ORIGINAL MATRIX A MUST BE REAL SYMMETRIC (STORAGE MODE=1)
C           MATRIX A CANNOT BE IN THE SAME LOCATION AS MATRIX R
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           DIAGONALIZATION METHOD ORIGINATED BY JACOBI AND ADAPTED
C           BY VON NEUMANN FOR LARGE COMPUTERS AS FOUND IN 'MATHEMATICAL
C           METHODS FOR DIGITAL COMPUTERS', EDITED BY A. RALSTON AND
C           H.S. WILF, JOHN WILEY AND SONS, NEW YORK, 1962, CHAPTER 7
C
C     ..................................................................
C
      SUBROUTINE EIGEN(A,R,N,MV)
         DIMENSION A(N*N),R(N*N)
C
C        ...............................................................
C
C        IF A DOUBLE PRECISION VERSION OF THIS ROUTINE IS DESIRED, THE
C        C IN COLUMN 1 SHOULD BE REMOVED FROM THE DOUBLE PRECISION
C        STATEMENT WHICH FOLLOWS.
C
         DOUBLE PRECISION A,R,ANORM,ANRMX,THR,X,Y,SINX,SINX2,COSX,
     1                    COSX2,SINCS,RANGE
C
C        THE C MUST ALSO BE REMOVED FROM DOUBLE PRECISION STATEMENTS
C        APPEARING IN OTHER ROUTINES USED IN CONJUNCTION WITH THIS
C        ROUTINE.
C
C        THE DOUBLE PRECISION VERSION OF THIS SUBROUTINE MUST ALSO
C        CONTAIN DOUBLE PRECISION FORTRAN FUNCTIONS.  SQRT IN STATEMENTS
C        40, 68, 75, AND 78 MUST BE CHANGED TO DSQRT.  ABS IN STATEMENT
C        62 MUST BE CHANGED TO DABS. THE CONSTANT IN STATEMENT 5 SHOULD
C        BE CHANGED TO 1.0D-12.
C
C        ...............................................................
C
C        GENERATE IDENTITY MATRIX
C
c mw    5 RANGE=1.0E-6
         range=1.0d-12
         IF(MV-1) 10,25,10
   10    IQ=-N
         DO 20 J=1,N
            IQ=IQ+N
            DO 20 I=1,N
               IJ=IQ+I
               R(IJ)=0.0
               IF(I-J) 20,15,20
   15          R(IJ)=1.0
   20    CONTINUE
C
C        COMPUTE INITIAL AND FINAL NORMS (ANORM AND ANORMX)
C
   25    ANORM=0.0
         DO 35 I=1,N
            DO 35 J=I,N
               IF(I-J) 30,35,30
   30          IA=I+(J*J-J)/2
               ANORM=ANORM+A(IA)*A(IA)
   35    CONTINUE
         IF(ANORM) 165,165,40
cc   40 ANORM=1.414*SQRT(ANORM)
   40    ANORM=1.414*dSQRT(ANORM)
         ANRMX=ANORM*RANGE/FLOAT(N)
C
C        INITIALIZE INDICATORS AND COMPUTE THRESHOLD, THR
C
         IND=0
         THR=ANORM
   45    THR=THR/FLOAT(N)
   50    L=1
   55    M=L+1
C
C        COMPUTE SIN AND COS
C
   60    MQ=(M*M-M)/2
         LQ=(L*L-L)/2
         LM=L+MQ
cc   62 IF( ABS(A(LM))-THR) 130,65,65
         IF( dABS(A(LM))-THR) 130,65,65
   65    IND=1
         LL=L+LQ
         MM=M+MQ
         X=0.5*(A(LL)-A(MM))
cc   68 Y=-A(LM)/ SQRT(A(LM)*A(LM)+X*X)
         Y=-A(LM)/ dSQRT(A(LM)*A(LM)+X*X)
         IF(X) 70,75,75
   70    Y=-Y
cc   75 SINX=Y/ SQRT(2.0*(1.0+( SQRT(1.0-Y*Y))))
   75    SINX=Y/ dSQRT(2.0*(1.0+( dSQRT(1.0-Y*Y))))
         SINX2=SINX*SINX
cc   78 COSX= SQRT(1.0-SINX2)
         COSX= dSQRT(1.0-SINX2)
         COSX2=COSX*COSX
         SINCS =SINX*COSX
C
C        ROTATE L AND M COLUMNS
C
         ILQ=N*(L-1)
         IMQ=N*(M-1)
         DO 125 I=1,N
            IQ=(I*I-I)/2
            IF(I-L) 80,115,80
   80       IF(I-M) 85,115,90
   85       IM=I+MQ
            GO TO 95
   90       IM=M+IQ
   95       IF(I-L) 100,105,105
  100       IL=I+LQ
            GO TO 110
  105       IL=L+IQ
  110       X=A(IL)*COSX-A(IM)*SINX
            A(IM)=A(IL)*SINX+A(IM)*COSX
            A(IL)=X
  115       IF(MV-1) 120,125,120
  120       ILR=ILQ+I
            IMR=IMQ+I
            X=R(ILR)*COSX-R(IMR)*SINX
            R(IMR)=R(ILR)*SINX+R(IMR)*COSX
            R(ILR)=X
  125    CONTINUE
         X=2.0*A(LM)*SINCS
         Y=A(LL)*COSX2+A(MM)*SINX2-X
         X=A(LL)*SINX2+A(MM)*COSX2+X
         A(LM)=(A(LL)-A(MM))*SINCS+A(LM)*(COSX2-SINX2)
         A(LL)=Y
         A(MM)=X
C
C        TESTS FOR COMPLETION
C
C        TEST FOR M = LAST COLUMN
C
  130    IF(M-N) 135,140,135
  135    M=M+1
         GO TO 60
C
C        TEST FOR L = SECOND FROM LAST COLUMN
C
  140    IF(L-(N-1)) 145,150,145
  145    L=L+1
         GO TO 55
  150    IF(IND-1) 160,155,160
  155    IND=0
         GO TO 50
C
C        COMPARE THRESHOLD WITH FINAL NORM
C
  160    IF(THR-ANRMX) 165,165,45
C
C        SORT EIGENVALUES AND EIGENVECTORS
C
  165    IQ=-N
         DO 185 I=1,N
            IQ=IQ+N
            LL=I+(I*I-I)/2
            JQ=N*(I-2)
            DO 185 J=I,N
               JQ=JQ+N
               MM=J+(J*J-J)/2
               IF(A(LL)-A(MM)) 170,185,185
  170          X=A(LL)
               A(LL)=A(MM)
               A(MM)=X
               IF(MV-1) 175,185,175
  175          DO 180 K=1,N
                  ILR=IQ+K
                  IMR=JQ+K
                  X=R(ILR)
                  R(ILR)=R(IMR)
  180          R(IMR)=X
  185    CONTINUE
         RETURN
      END subroutine EIGEN
