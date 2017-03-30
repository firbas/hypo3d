c
		SUBROUTINE sort_x(X,KEY,NO)
c
c  HYPO71 routine ... sort array X in increasing order
c
		DIMENSION X(NO),KEY(NO)
C-----------------------------------------------------------------------
		DO 1 I=1,NO
  1   KEY(I)=I
		MO=NO
 2    IF (MO-15) 21,21,23
 21   IF (MO-1) 29,29,22
 22   MO=2*(MO/4)+1
		GO TO 24
 23   MO=2*(MO/8)+1
 24   KO=NO-MO
		JO=1
 25   I=JO
 26   IF (X(I)-X(I+MO)) 28,28,27
 27   TEMP=X(I)
		X(I)=X(I+MO)
		X(I+MO)=TEMP
		KEMP=KEY(I)
		KEY(I)=KEY(I+MO)
		KEY(I+MO)=KEMP
		I=I-MO
		IF (I-1) 28,26,26
 28   JO=JO+1
		IF (JO-KO) 25,25,2
 29   RETURN
		END
