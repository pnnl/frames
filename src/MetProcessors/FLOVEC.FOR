      SUBROUTINE FLOVEC( ihr )
C***********************************************************************
C     FLOVEC.FOR 
C
C     Christian J Fosmire
C     Pacific Northwest National Lab
C     P.O. Box 999
C     Richland, WA 99352
C
C     Created:    1/16/96
C
C     Description: This Routine Converts The Meteorological Wind 
C        Direction To Flow Vector and Also Calculates Random Flow 
C        Vector.  Adapted from FLOVEC subroutine in PCRAMMET
C
C***********************************************************************
      
      IMPLICIT    NONE

      INCLUDE     'hrly.inc'
      
      REAL        U01
      REAL        fv, rndnum, xdir
      
      
      INTEGER     ihr, krand
      INTEGER*2   ih, im, is, i1
            
C*    Calculate flow vector and random flow vector only if the
C*    wind direciton is missing.

C*    Only winds from CD144 format need to be multiplied by 10;
C*    the SAMSON data are reported to the nearest 10 degrees, but
C*    in whole degrees.

      IF( idir(ihr) .LT. 900 ) THEN

         IF( sfctyp.EQ.'CD144' .OR. sfctyp.EQ.'SCRAM' ) THEN
            xdir = idir(ihr)*10.0
         ELSEIF( sfctyp.EQ.'SAMSON' ) THEN
            xdir = idir(ihr)
         ENDIF

         IF( xdir.GT.180.0 ) THEN
            fv = xdir - 180.0
         ELSE
            fv = xdir + 180.0
         ENDIF
         
         afv(ihr) = fv

C*       CALCULATE A RANDOM NUMBER 0-9

         rndnum = U01( rndseed )
c         rndnum = .57383857         
         krand = NINT( rndnum*10 - 0.5 )
         fvr(ihr) = fv + krand - 4.0
         IF( fvr(ihr).GT.360.0 ) fvr(ihr) = fvr( ihr ) - 360.0

      ELSE
         
         fvr(ihr) = 999.0

      ENDIF

      RETURN
      END
