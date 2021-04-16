
C---------------------------------------------------------------------------
c
c     USTAR
c     
c     Date:              August 16. 1995
c     Updated:           March 14, 2000  (revised PrgStat)
c
c     Description:
c         
c        USTAR computes the friction velocity in the atmospheric
c        boundary layer from a measured wind using a diabatic wind
c        profile assumption.  (See Panofsky and Dutton, ATMOSPHERIC
c        TURBULENCE, 1984, Section 6.5 for details.)   
c
c     Required modules:  None
c
C---------------------------------------------------------------------------

      REAL FUNCTION USTAR( ht, spd, z0, molinv, ist, acute, PrgStat )

      IMPLICIT          NONE

      REAL              ht, molinv, pi, psi, spd, x, z0 

      INTEGER           ist

      LOGICAL           acute
      
      CHARACTER*2       prgid
      CHARACTER*50      PrgStat
      
      prgid = 'CR'
      IF ( acute ) prgid = 'AC'
 
      pi = 3.14159
      x = 0.0

      IF ( ht .LT. 0.1 ) THEN 
         WRITE ( PrgStat(1:2),'(a2)'  )     prgid
         WRITE ( PrgStat(4:13),'(a10)' )   'USTAR'
         WRITE ( PrgStat(15:18),'(i4)' )    9999
         WRITE ( PrgStat(31:44),'(a14)' )  'wndhgt too low' 
         WRITE ( PrgStat(46:50),'(f5.3)' )  ht
      ENDIF
 
      IF ( (ist .LT. 0) .OR. (ist .GT. 7)  ) THEN 
         WRITE ( PrgStat(1:2),'(a2)'  )     prgid
         WRITE ( PrgStat(4:13),'(a10)' )   'USTAR'
         WRITE ( PrgStat(15:18),'(i4)' )    9999
         WRITE ( PrgStat(31:46),'(a16)' )  'stab range error' 
         WRITE ( PrgStat(48:48),'(i1)' )    ist
      ENDIF
      
      IF ( PrgStat .NE. ' ' ) RETURN

C  ESTIMATE  USTAR BASED ON STABILITY

      IF ( ist .LE. 3 ) THEN                        !  Unstable

         IF ( molinv .GT. 0.0) THEN 
            WRITE ( PrgStat(1:2),'(a2)'  )    prgid
            WRITE ( PrgStat(4:13),'(a10)' )  'USTAR'
            WRITE ( PrgStat(15:18),'(i4)' )   9999
            WRITE ( PrgStat(31:47),'(a17)' ) 'unstable molinv > 0'  
            WRITE ( PrgStat(49:50),'(i2)' )   ist
            RETURN
         ENDIF

         x = (1.0 - 16.0 * ht * molinv )**0.25
         psi = ALOG ( (1+x**2)/2. * ((1.+x)/2)**2 ) - 2. * ATAN ( x )
     &                + pi/2 
     
      ELSE IF ( ist .GE. 5 ) THEN                   !  Stable

         IF ( molinv .LE. 0.0 ) THEN 
            WRITE ( PrgStat(1:2),'(a2)'  )    prgid
            WRITE ( PrgStat(4:13),'(a10)' )  'USTAR'
            WRITE ( PrgStat(15:18),'(i4)' )   9999
            WRITE ( PrgStat(31:47),'(a17)' ) 'stable molinv < 0'  
            WRITE ( PrgStat(49:50),'(i2)' )   ist
            RETURN
         ENDIF
      
         psi = -5.0 * ht * molinv

      ELSE                                          !  Neutral

         psi = 0.0

      ENDIF                
       
      USTAR = 0.4 * spd / ( ALOG ( ht/z0 ) - psi ) 

c  set minimum value for low wind speed diffusion  

      IF ( USTAR .LT. 0.05 ) USTAR = 0.05
     
      RETURN
      
      END
