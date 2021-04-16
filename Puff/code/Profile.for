C---------------------------------------------------------------------------
c
c     PROFILE
c     
c     Date:              August 16, 1995
c     Updated:           March 14, 2000
c
c     Description:
c
c        PROFILE computes the wind at any height in the atmospheric
c        boundary layer using a diabatic wind profile.  (See Panofsky 
c        and Dutton, ATMOSPHERIC TURBULENCE, 1984, Section 6.5 for 
c        details.)   
c
c     Required modules:  None
c
C---------------------------------------------------------------------------

      REAL FUNCTION PROFILE( z0, ustr, molinv, ist, hght, acute,
     &                       PrgStat )

      IMPLICIT      NONE
        
      REAL          hght, ht1, mol, molinv, pi, psi, x, ustr, z0
      
      INTEGER       ist
      
      CHARACTER*2   prgid
      CHARACTER*50  PrgStat
      
      LOGICAL        acute

      prgid = 'CR'
      IF ( acute ) prgid = 'AC'

      pi = 3.14159
      x = 0.0

      IF ( hght .LT. 0.1 ) THEN 
         WRITE ( PrgStat(1:2),'(a2)'  )     prgid
         WRITE ( PrgStat(4:13),'(a10)' )   'PROFILE'
         WRITE ( PrgStat(15:18),'(i4)' )    9999
         WRITE ( PrgStat(31:44),'(a14)' )  'wnd ht too low' 
         WRITE ( PrgStat(46:50),'(f5.3)' )  hght
      ENDIF
 
      IF ( (ist .LT. 0) .OR. (ist .GT. 7)  ) THEN 
         WRITE ( PrgStat(1:2),'(a2)'  )     prgid
         WRITE ( PrgStat(4:13),'(a10)' )   'PROFILE'
         WRITE ( PrgStat(15:18),'(i4)' )    9999
         WRITE ( PrgStat(31:46),'(a16)' )  'stab range error' 
         WRITE ( PrgStat(48:48),'(i1)' )    ist
      ENDIF

      IF ( PrgStat .NE. ' ' ) RETURN

C **  LIMIT PROFILE TO LOWEST 100M ... OR 3 TIMES M-O LENGTH IN STABLE
C     CONDITIONS

      ht1 = hght
      IF ( ist .GT. 4 ) THEN
         IF ( molinv .LE. 0.0 ) THEN 
            WRITE ( PrgStat(1:2),'(a2)'  )    prgid
            WRITE ( PrgStat(4:13),'(a10)' )  'PROFILE'
            WRITE ( PrgStat(15:18),'(i4)' )   9999
            WRITE ( PrgStat(31:47),'(a17)' ) 'stable molinv <= 0'  
            WRITE ( PrgStat(49:50),'(i2)' )   ist
            RETURN
         ENDIF
         mol = 1.0 / molinv             
         ht1 = AMIN1 ( (3 * mol),ht1 )
      ENDIF
      
      IF ( hght .GT. 100. ) ht1 = 100.

C ** Estimate wind speed at height

      IF ( ist .LE. 3 ) THEN  ! Unstable Conditions

         IF ( molinv .GT. 0.0 ) THEN 
            WRITE ( PrgStat(1:2),'(a2)'  )    prgid
            WRITE ( PrgStat(4:13),'(a10)' )  'PROFILE'
            WRITE ( PrgStat(15:18),'(i4)' )   9999
            WRITE ( PrgStat(31:47),'(a17)' ) 'unstable molinv > 0'  
            WRITE ( PrgStat(49:50),'(i2)' )   ist
            RETURN
         ENDIF

         x = (1.0 - 16.0 * ht1 * molinv )**0.25
         psi = ALOG( (1+X**2)/2. * ((1.+X)/2)**2) - 2. * ATAN(X) + pi/2

      ELSE IF ( ist .GE. 5 ) THEN ! Stable Conditions
      
         IF ( molinv .LE. 0.0 ) THEN 
            WRITE ( PrgStat(1:2),'(a2)'  )    prgid
            WRITE ( PrgStat(4:13),'(a10)' )  'PROFILE'
            WRITE ( PrgStat(15:18),'(i4)' )   9999
            WRITE ( PrgStat(31:47),'(a17)' ) 'stable molinv <= 0' 
            WRITE ( PrgStat(49:50),'(i2)' )   ist
            RETURN
         ENDIF
      
          psi = -5.0 * ht1 * molinv  
          
      ELSE  ! Neutral 
           
          psi = 0.0
      
      ENDIF

      PROFILE = ustr / 0.4 * ( ALOG( ht1/z0 ) - psi )

      RETURN 
      
      END