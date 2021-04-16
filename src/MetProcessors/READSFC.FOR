      SUBROUTINE READSFC( nread, eofsfc )
C***********************************************************************
C     READSFC.FOR
C 
C     Christian J Fosmire
c     Pacific Northwest National Lab
c     P.O. Box 999
C     Richland, WA 99352
c
c     Created:  1/15/96
c
c     Description:  Reads 24 hours of Surface Data from either CD144 or
C        SAMSON format.  Adapted from PCRAMMET READSF subroutine
c
C***********************************************************************
      IMPLICIT    NONE

      INCLUDE     'hrly.inc'
      
      INTEGER     nread
      
      LOGICAL     eofsfc
      
      IF( sfctyp.EQ.'CD144' ) THEN

C*       DATA TYPE IS CD144

         CALL CD144( iosfc, acd144, eofsfc, lwd, nread )

      ELSEIF( sfctyp.EQ.'SCRAM' ) THEN

C*       DATA TYPE IS SCRAM

c********NEED TO CHANGE************************************

c         CALL SCRAM( iosfc, acd144, eofsfc, lwd, nread )

      ELSEIF( sfctyp.EQ.'SAMSON' ) THEN

C*       DATA TYPE IS SAMSON

**********NEED TO CHANGE***********************************
         CALL SAMSON( nvars, idvar, jsamsn, timzon, lat, long, iosfc, 
     &                eofsfc, nread, tilde )

      ENDIF

      RETURN

      END