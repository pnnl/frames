C***********************************************************************
C
C     TURBSIGW
C     
C     J.V. Ramsdell 
C     Pacific Northwest Laboratory
C     P.O. Box 999
C     Richland, Washington  99352
C
C     Created: 11/8/91
C     Updated: 4/21/92
C
C     Description:  Computes turbulence velocities (sigv and sigw) 
C                   using the procedures recommended by the participants
C                   in the atmospheric modeling working meeting in March 
C                   1991 (Ramsdell 1991)
C
C     Input:   stability class                       ==> IST
C              friction velocity (m/s)               ==> USTR
C              effective release height (m)          ==> ERHT
C              mixing layer depth (m)                ==> DEPTH
C              inverse Monin-Obukhov length (1/m)    ==> MOLINV
C              horizontal turbulance velocity (m/s)  ==> SIGV
C
C     Relationship to other modules:
C
C         Makes calls to:  NONE
C
C
C***********************************************************************
      REAL FUNCTION TURBSIGW( ist, ustr, erht, depth, molinv, sigv )

      IMPLICIT NONE

      REAL     coriolis, depth, erht, molinv, sigv, sigw, ustr

      INTEGER  ist      
      
      coriolis = 1.0605E-4

      IF( (ist .LT. 1) .OR. (ist .GT. 7) )
     +     STOP 'STABILITY CLASS ERROR IN TURBSIGW'

      IF( (ist .GE. 4) .AND. (sigv .GT. 0.0)) THEN
         TURBSIGW = sigv
         RETURN
      ENDIF

      IF( ist .GE. 5 ) THEN       ! Stable Conditions
         IF( erht .LT. (0.9*depth) ) THEN
            sigw = ustr * 1.3 * ( 1.0 - erht / depth )    
         ELSE
            sigw = ustr * 0.13
         ENDIF
      
      ELSE IF( ist .EQ. 4 ) THEN  ! Neutral Conditions

         IF( erht .LE. depth ) THEN
            sigw = ustr * 1.3 * EXP( -2.0 * coriolis * erht / ustr )
            sigw = AMAX1( sigw, 0.13*ustr )
         ELSE
            sigw = ustr * 0.13
         ENDIF

      ELSE                        ! Unstable Conditions

         IF( erht .LE. (0.5*depth) ) THEN
            sigw = ustr * 1.3 * ( 1.0 - 3.0 * erht * molinv )**(1./3.)
            sigw = AMAX1( sigw, 0.13*ustr )
         ELSE IF( erht .LE. depth) THEN
            sigw = ustr * 1.3 * ( 1.0 - 1.5 * depth * molinv )**(1./3.)
            sigw = AMAX1( sigw, 0.13*ustr )
         ELSE
            sigw = ustr * 0.13 
         ENDIF

      ENDIF

      TURBSIGW = AMAX1(sigw, 0.01)

      RETURN

      END