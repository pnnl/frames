C---------------------------------------------------------------------------
c
c     TURBSIGV
c     
c     Date:         August 16, 1995
c
c     Description:  Computes lateral turbulence velocity (sigv)
c                   using the procedures recommended by the participants
c                   in the atmospheric modeling working meeting in March 
c                   1991 (Ramsdell 1991)
c
c     Required modules:  None
c
C---------------------------------------------------------------------------

      REAL FUNCTION TURBSIGV ( ist, ustr, erht, depth, molinv, acute,
     &                           PrgStat ) 

      IMPLICIT      NONE

      REAL          coriolis, depth, erht, molinv, sigv, ustr

      INTEGER       ist      
              
      CHARACTER*2   prgid
      CHARACTER*50  PrgStat  

      LOGICAL       acute

      coriolis = 1.0605E-4

      prgid = 'CR'
      IF ( acute ) prgid = 'AC'

      IF ( (ist .LT. 1) .OR. (ist .GT. 7) ) THEN
         WRITE ( PrgStat(1:2),'(a2)'  )    prgid
         WRITE ( PrgStat(4:13),'(a10)' )  'TURBSIGV'
         WRITE ( PrgStat(15:18),'(i4)' )   9999
         WRITE ( PrgStat(31:46),'(a16)' ) 'stab range error' 
         WRITE ( PrgStat(48:48),'(i1)' )   ist
         RETURN
      ENDIF

      IF ( ist .GE. 5 ) THEN       ! Stable Conditions
         IF ( erht .LT. (0.9 * depth) ) THEN
            sigv = ustr * 1.3 * ( 1.0 - erht / depth )    
         ELSE
            sigv = ustr * 0.13
         ENDIF
      
      ELSE IF ( ist .EQ. 4 ) THEN  ! Neutral Conditions
        
         IF ( erht .LE. depth ) THEN
            sigv = ustr * 1.3 * EXP ( -2.0 * coriolis * erht / ustr )
            sigv = AMAX1 ( sigv, (0.13 * ustr) )
         ELSE
            sigv = ustr * 0.13
         ENDIF

      ELSE                        ! Unstable Conditions

         IF ( erht .LE. depth ) THEN
            sigv = ustr * ( 12 - 0.5 * depth * molinv )**(1./3.)
            sigv = AMAX1 ( sigv, (0.13 * ustr) )
         ELSE
            sigv = ustr * 0.13
         ENDIF

      ENDIF

      TURBSIGV = AMAX1 ( sigv,0.01 )
      
      RETURN
      END                                    