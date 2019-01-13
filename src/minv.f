C      LB:[34,13]MINV.FTN;1
C     TITLE 780001-100  IBM - SUBROUTINE MINV
C
C     ..................................................................
C
C        SUBROUTINE MINV
C
C        PURPOSE
C           INVERT A MATRIX
C
C        USAGE
C           CALL MINV(A,N,D,L,M)
C
C        DESCRIPTION OF PARAMETERS
C           A - INPUT MATRIX, DESTROYED IN COMPUTATION AND REPLACED BY
C               RESULTANT INVERSE.
C           N - ORDER OF MATRIX A
C           D - RESULTANT DETERMINANT
C           L - WORK VECTOR OF LENGTH N
C           M - WORK VECTOR OF LENGTH N
C
C        REMARKS
C           MATRIX A MUST BE A GENERAL MATRIX
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           THE STANDARD GAUSS-JORDAN METHOD IS USED. THE DETERMINANT
C           IS ALSO CALCULATED. A DETERMINANT OF ZERO INDICATES THAT
C           THE MATRIX IS SINGULAR.
C
C     ..................................................................
C
      SUBROUTINE MINV(A,N,D,L,M)
         DIMENSION A(N*N),L(N),M(N)
C
C        ...............................................................
C
C        IF A DOUBLE PRECISION VERSION OF THIS ROUTINE IS DESIRED, THE
C        C IN COLUMN 1 SHOULD BE REMOVED FROM THE DOUBLE PRECISION
C        STATEMENT WHICH FOLLOWS.
C
         DOUBLE PRECISION A,D,BIGA,HOLD
C
C        THE C MUST ALSO BE REMOVED FROM DOUBLE PRECISION STATEMENTS
C        APPEARING IN OTHER ROUTINES USED IN CONJUNCTION WITH THIS
C        ROUTINE.
C
C        THE DOUBLE PRECISION VERSION OF THIS SUBROUTINE MUST ALSO
C        CONTAIN DOUBLE PRECISION FORTRAN FUNCTIONS.  ABS IN STATEMENT
C        10 MUST BE CHANGED TO DABS.
C
C        ...............................................................
C
C        SEARCH FOR LARGEST ELEMENT
C
         D=1.0
         NK=-N
         DO 80 K=1,N
            NK=NK+N
            L(K)=K
            M(K)=K
            KK=NK+K
            BIGA=A(KK)
            DO 20 J=K,N
               IZ=N*(J-1)
               DO 20 I=K,N
                  IJ=IZ+I

                  IF( DABS(BIGA)- DABS(A(IJ))) 15,20,20
   15             BIGA=A(IJ)
                  L(K)=I
                  M(K)=J
   20       CONTINUE
C
C        INTERCHANGE ROWS
C
            J=L(K)
            IF(J-K) 35,35,25
   25       KI=K-N
            DO 30 I=1,N
               KI=KI+N
               HOLD=-A(KI)
               JI=KI-K+J
               A(KI)=A(JI)
   30       A(JI) =HOLD
C
C        INTERCHANGE COLUMNS
C
   35       I=M(K)
            IF(I-K) 45,45,38
   38       JP=N*(I-1)
            DO 40 J=1,N
               JK=NK+J
               JI=JP+J
               HOLD=-A(JK)
               A(JK)=A(JI)
   40       A(JI) =HOLD
C
C        DIVIDE COLUMN BY MINUS PIVOT (VALUE OF PIVOT ELEMENT IS
C        CONTAINED IN BIGA)
C
   45       IF(BIGA) 48,46,48
   46       D=0.0
            RETURN
   48       DO 55 I=1,N
               IF(I-K) 50,55,50
   50          IK=NK+I
               A(IK)=A(IK)/(-BIGA)
   55       CONTINUE
C
C        REDUCE MATRIX
C
            DO 65 I=1,N
               IK=NK+I
               HOLD=A(IK)
               IJ=I-N
               DO 65 J=1,N
                  IJ=IJ+N
                  IF(I-K) 60,65,60
   60             IF(J-K) 62,65,62
   62             KJ=IJ-I+K
                  A(IJ)=HOLD*A(KJ)+A(IJ)
   65       CONTINUE
C
C        DIVIDE ROW BY PIVOT
C
            KJ=K-N
            DO 75 J=1,N
               KJ=KJ+N
               IF(J-K) 70,75,70
   70          A(KJ)=A(KJ)/BIGA
   75       CONTINUE
C
C        PRODUCT OF PIVOTS
C
            D=D*BIGA
C
C        REPLACE PIVOT BY RECIPROCAL
C
            A(KK)=1.0/BIGA
   80    CONTINUE
C
C        FINAL ROW AND COLUMN INTERCHANGE
C
         K=N
  100    K=(K-1)
         IF(K) 150,150,105
  105    I=L(K)
         IF(I-K) 120,120,108
  108    JQ=N*(K-1)
         JR=N*(I-1)
         DO 110 J=1,N
            JK=JQ+J
            HOLD=A(JK)
            JI=JR+J
            A(JK)=-A(JI)
  110    A(JI) =HOLD
  120    J=M(K)
         IF(J-K) 100,100,125
  125    KI=K-N
         DO 130 I=1,N
            KI=KI+N
            HOLD=A(KI)
            JI=KI-K+J
            A(KI)=-A(JI)
  130    A(JI) =HOLD
         GO TO 100
  150    RETURN
      END subroutine MINV

