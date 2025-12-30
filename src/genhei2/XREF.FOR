C------------------------------------------------------------------------------
C
C       CROSS REFERENCES, ORGAN DOSE TO RISK COEFFICIENTS
C
C       BAN  5 NOV 98
C
C------------------------------------------------------------------------------
      SUBROUTINE XREF(J,KK)
	IF (J .EQ. 1) KK=6
	IF (J .EQ. 2) KK=8
	IF (J .EQ. 3) KK=10
	IF (J .EQ. 4) KK=12
	IF (J .EQ. 5) KK=13
	IF (J .EQ. 6) KK=3
	IF (J .EQ. 7) KK=18
	IF (J .EQ. 8) KK=5
	IF (J .EQ. 9) KK=15
	IF (J .EQ. 10) KK=2
	IF (J .EQ. 11) KK=11
	IF (J .EQ. 12) KK=22
	IF (J .EQ. 13) KK=17
	IF (J .EQ. 14) KK=14
	IF (J .EQ. 15) KK=24
	RETURN
	END