      SUBROUTINE FILMET
C***********************************************************************
C     FILLMET.FOR
C
c     Christian J Fosmire
C     Pacific Northwest National Lab
C     P.O. Box 999
C     Richland, WA 99352
C
C     Created:    1/16/96
C
C     Description: Fills in the Appropriate Weather Variable Arrays,
C        Translating Character to Numeric.  Adapted from FILMET 
c        subroutine in PCRAMMET
c
C***********************************************************************
      IMPLICIT    NONE
      
      INCLUDE     'hrly.inc'

      INTEGER     ih

      DO ih = 1,24
             
C*       Fill in Surface Variables

         IF( sfctyp.EQ.'SAMSON' ) THEN 

C*          From SAMSON...

c     ****Change Later*****
            CALL FILSAM( ih )

         ELSEIF( sfctyp.EQ.'CD144' .OR. sfctyp.EQ.'SCRAM' ) THEN

C*          Or From CD144 or SCRAM

            CALL FIL144( ih ) 
         
         ENDIF
            
C*       Check the wind direction:

         IF( idir(ih).NE.0  .AND. idir(ih).NE.999 ) THEN

C*          It is not a calm hour and not missing - save the direction

            lwd = idir(ih)

         ELSEIF( idir(ih).EQ.0 ) THEN

C*          The direction is zero (calm hour), set the direction to the
C           previous valid wind direction

            idir(ih) = lwd

         ENDIF
            
C*      If the Surface Data is SAMSON         
      
         IF( sfctyp.EQ.'SAMSON' ) THEN                

C*       SAMSON Precip (in PRECIP(IHR)) May have been Read With
C*       Surface Variables

C*       If Supplementing with TD3240 Data (PPTFLG = .T.), Then
C*       Check to See if SAMSON Precip Was Missing.  If So, '
C*       Substitute With TD3240 If Non-Zero, Else Use SAMSON

c     ******Change Later********        
            IF( td32flg ) THEN
               IF( precip(ih).GT.9999.0 ) THEN
                  IF( p3240(ih).GE.0.0 ) THEN
                     precip(ih) = p3240(IH)
                     WRITE( IDIAG,500 ) isyr(ih),ismo(ih),isdy(ih),ih
c                     nowarn = .FALSE.
                  ENDIF
               ENDIF
            ENDIF
c
C*       Otherwise, if it is CD144 or SCRAM
      
         ELSEIF( sfctyp.EQ.'CD144' .OR. sfctyp.EQ.'SCRAM' ) THEN
            
C*             Get Precip Variable From 3240 Data                  
      
            precip(ih) = p3240(ih)
      
         ENDIF

      ENDDO
   
      RETURN

  500 FORMAT( ' FILMET: TD-3240 precip substituted for SAMSON',
     &        ' on (yy/mm/dd/hh): ', 4(i2.2:,'/') )

      END
