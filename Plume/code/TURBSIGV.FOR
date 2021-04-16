C***********************************************************************
C
C     TURBSIGV
C     
C     J.V. Ramsdell 
C     Pacific Northwest Laboratory
C     P.O. Box 999
C     Richland, Washington  99352
C
C     Created: 11/8/91
C     Updated: 4/21/92
C
C     Description:  Computes lateral turbulence velocity (sigv)
C                   using the procedures recommended by the participants
C                   in the atmospheric modeling working meeting in March 
C                   1991 (Ramsdell 1991)
C
C     Input:   stability class                   ==> IST
C              friction velocity (m/s)           ==> USTR
C              effective release height (m)      ==> ERHT
C              mixing layer depth (m)            ==> DEPTH
C              inverse Monin-Obukhov length (1/m)==> MOLINV
C
C     Required Modules: NONE
C
C***********************************************************************
      REAL FUNCTION TURBSIGV( ist, ustr, erht, depth, molinv ) 

      IMPLICIT NONE

      INTEGER  ist      

      REAL     coriolis, depth, erht, molinv, sigv, ustr
      
      coriolis = 1.0605E-4

      IF( (ist .LT. 1) .OR. (ist .GT. 7) )
     +     STOP 'STABILITY CLASS ERROR IN TURBSIG'

      IF( ist .GE. 5 ) THEN       ! Stable Conditions
         IF( erht .LT. (0.9*depth) ) THEN
            sigv = ustr * 1.3 * ( 1.0 - erht / depth )    
         ELSE
            sigv = ustr * 0.13
         ENDIF
      
      ELSE IF( ist .EQ. 4 ) THEN  ! Neutral Conditions
        
         IF( erht .LE. depth ) THEN
            sigv = ustr * 1.3 * EXP( -2.0 * coriolis * erht / ustr )
            sigv = AMAX1( sigv, 0.13*ustr)
         ELSE
            sigv = ustr * 0.13
         ENDIF

      ELSE                        ! Unstable Conditions

         IF( erht .LE. depth ) THEN
            sigv = ustr * ( 12 - 0.5 * depth * molinv )**(1./3.)
            sigv = AMAX1( sigv, 0.13*ustr)
         ELSE
            sigv = ustr * 0.13
         ENDIF

      ENDIF

      TURBSIGV = AMAX1(sigv, 0.01)
      
      RETURN                
      
      END