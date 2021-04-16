      REAL FUNCTION TURBSIGW( ist, ustr, erht, depth, molinv, sigv, 
     &                        acute, PrgStat )

C-----------------------------------------------------------------------
c
c     TURBSIGW
c     
c     Date:              April 5, 1996
c     Updated:           March 14, 2000
c
c     Description:       Computes turbulence velocities (sigv and sigw) 
c                        from stability, surface roughness, and wind
c                        speed
c
c     Required modules:  None
c
C-----------------------------------------------------------------------

      IMPLICIT       NONE

      REAL           coriolis, depth, erht, molinv, sigv, sigw, ustr

      INTEGER        ist      

      CHARACTER*2    prgid
      CHARACTER*50   PrgStat

      LOGICAL        acute
      
      coriolis = 1.0605E-4

      prgid = 'CR'
      IF ( acute ) prgid = 'AC'

      IF ( (ist .LT. 1) .OR. (ist .GT. 7) ) THEN
         WRITE ( PrgStat(1:2),'(a2)'  )    prgid
         WRITE ( PrgStat(4:13),'(a10)' )  'TURBSIGW'
         WRITE ( PrgStat(15:18),'(i4)' )   9999
         WRITE ( PrgStat(31:46),'(a16)' ) 'stab range error' 
         WRITE ( PrgStat(48:50),'(i3)' )   ist
         RETURN
      ENDIF 
      
      IF ( (ist .GE. 5) .AND. (sigv .GT. 0.0) ) THEN
         TURBSIGW = sigv
         RETURN
      ENDIF   

      IF ( ist .GE. 5 ) THEN       ! Stable Conditions
         IF ( erht .LT. (0.9*depth) ) THEN
            sigw = ustr * 1.3 * ( 1.0 - erht / depth )    
         ELSE
            sigw = ustr * 0.13
         ENDIF
      
      ELSE IF ( ist .EQ. 4 ) THEN  ! Neutral Conditions

         IF ( erht .LE. depth ) THEN
            sigw = ustr * 1.3 * EXP( -2.0 * coriolis * erht / ustr )
            sigw = AMAX1( sigw,(0.13*ustr) )
         ELSE
            sigw = ustr * 0.13
         ENDIF

      ELSE                        ! Unstable Conditions

         IF ( erht .LE. (0.5*depth) ) THEN
            sigw = ustr * 1.3 * ( 1.0 - 3.0 * erht * molinv )**(1./3.)
            sigw = AMAX1 ( sigw,(0.13*ustr) )
         ELSE IF ( erht .LE. depth ) THEN
            sigw = ustr * 1.3 * ( 1.0 - 1.5 * depth * molinv )**(1./3.)
            sigw = AMAX1 ( SIGW,(0.13*ustr) )
         ELSE
            sigw = ustr * 0.13 
         ENDIF

      ENDIF

      TURBSIGW = AMAX1 ( sigw, 0.005 )

      RETURN                         
      
      END
