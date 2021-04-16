      INTEGER FUNCTION PRCPCODE( lpc, fpc )
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     
C     PRCPCODE
C
C     J. V. Ramsdell
C     Pacific Northwest Laboratory
C     P.O. Box 999
C     Richland, WA 99352
C
C     Created:    3/29/90
C
C     Description: PRCPCODE converts the liquid and frozen precipitation
c                  codes in Hanford Meteorological Station Surface
C                  observations to a precipitation code for use in the
C                  MESOI/MESORAD suite of computer codes.
C
C     Input Variables:  LPC is the liquid precipitation code
C                       FPC is the frozen precipitation code
C
C     Output Variables: PRCPCODE is the MESOI precipitation code, which
C                       returned via tghe function name
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT    NONE
      
      INTEGER     lcode(10), mcode(4,4)
      INTEGER     findx, fpc, if1, if2, if3, if4, if5, lindx, lpc
      
      DATA lcode  / 2, 2, 3, 4, 2, 3, 4, 2, 3, 4 /
      
      DATA mcode  / 0, 1, 2, 3, 4, 1, 2, 3, 5, 5, 2, 3, 6, 6, 6, 3 /

      lindx = 1
      findx = 1
      
      IF( lpc.eq.0 .AND. fpc.eq.0 ) THEN

C     No Precipitation
      
         PRCPCODE = 0
         
      ELSE
      
C     Precipitation of some form
         
         IF( lpc.eq.0 ) THEN
 
C     No Liquid Precipitation

            lindx = 1
           
         ELSE
         
C     Liquid Precipitation

            lindx = lcode(lpc/10 + 1)
            
         ENDIF
         
         IF( fpc.eq.0 ) THEN
            
c     No Frozen Precipitation
            
            findx = 1
         
         ELSE
         
c     Frozen Precipitation

            if1 = fpc / 100
            if2 = MOD( if1, 3 )
            
            IF( if1.GT.0 .AND. if2.EQ.0 ) THEN
            
c     Heavy Frozen Precipitation

               findx = 4
               
            ELSE
            
               if3 = MOD( fpc, 100 ) 
               IF( if1.GT.0 .AND. if3.EQ.0 ) THEN
               
                  IF( if2.eq.1 ) THEN
                  
c                 Light Frozen Precip.

                     findx = 2
                     
                  ELSE
                  
c                 Moderate Frozen Precip

                     findx = 3
                     
                  ENDIF
                  
               ELSE
               
                  if4 = if3 / 10
                  if5 = MOD( if3, 10 ) 
                  IF( if4.eq.3 .OR. if4.eq.9 .OR. if5.eq.3 ) THEN
                  
C                    Heavy Frozen Precip

                     findx = 4
                     
                  ELSEIF( if4.eq.2 .OR. if4.eq.5. OR. if5.EQ.2 .OR.
     &                     if5.eq.8 ) THEN
                
C                    Moderate Frozen Precip

                     findx = 3
                     
                  ELSE

C                    Light Frozen Precip

                     findx = 2
                  
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      
         PRCPCODE = mcode(lindx,findx)
      ENDIF
      
      RETURN
      
      END          