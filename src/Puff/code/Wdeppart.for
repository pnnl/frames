C---------------------------------------------------------------------------
c
c     WDEPPART
c
c     Date:             August 16, 1995
c     Updated:          March 14, 2000
c
c     Description:
c
c        WDEPPART estimates a washout coefficient for use in wet 
c        deposition calculations.   The washout coefficient is used 
c        to estimate the scavenging of particles by rain and snow.  
c        It is calculated from the precipitation type and rate.  
c        The equations used to estimate the washout coefficient are 
c        discussed in Slinn (1984).  
c 
c     Required modules:  None
c
C---------------------------------------------------------------------------

      REAL FUNCTION WDEPPART( precip, pr_rate, acute, PrgStat )

      IMPLICIT       NONE
       
      INTEGER        precip 
       
      REAL           cnst, ebar, pr_rate, rm, wcoef 
      
      CHARACTER*2    prgid
      CHARACTER*50   PrgStat
      
      LOGICAL        acute

      prgid = 'CR' 
      IF ( acute ) prgid = 'AC'

      wcoef = 0.0
      cnst = 0.5
      ebar = 1.0
      WDEPPART = 0.0

C **  Check range of precip 
     
      IF ( precip .EQ. 0 ) RETURN

      IF ( (precip .LE. 0) .OR. (precip .GT. 6) ) THEN
         WRITE ( PrgStat(1:2),'(a2)'  )    prgid
         WRITE ( PrgStat(4:13),'(a10)' )  'WDEPPART' 
         WRITE ( PrgStat(15:18),'(i4)' )   9999
         WRITE ( PrgStat(31:46),'(a16)' ) 'prcp range error' 
         WRITE ( PrgStat(48:50),'(i3)' )   precip
         RETURN
      ENDIF

      IF ( precip .LE. 3 ) THEN
         rm = 0.35 * pr_rate**0.25
         wcoef = cnst * ebar * pr_rate / rm
      ELSE IF ( precip .LE. 6 ) THEN
          wcoef = 0.2 * pr_rate
      ENDIF

C  Change washout coefficient to 1/sec from 1/hr

      wcoef = wcoef / 3600.      
        
      WDEPPART = wcoef
       
      RETURN
      END                            