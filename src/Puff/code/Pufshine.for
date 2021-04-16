      SUBROUTINE PUFSHINE ( acute, PrgStat )           
C---------------------------------------------------------------------------
c
c     PUFSHINE
c     
c     Date:         November 16, 1998
c     Updated:      September 13, 1999
c                   March 14, 2000 
c
c     Description:  Calculates the external dose from puffs using
c                   a discrete point approximation. 
c
c     Required modules:    
c
c           Subroutine:  R2QUAD
c
c           Functions:   None
c
C---------------------------------------------------------------------------
                                                                            
      IMPLICIT      NONE                                                                        
                                                                            
      INCLUDE       'parm.inc'
      INCLUDE       'shine.inc'
      INCLUDE       'pf_shine.inc'

      REAL          rsult, test, xout(20), z1, z2
      
      INTEGER       i, indx, i1

      CHARACTER*2   prgid
      CHARACTER*50  PrgStat 

      LOGICAL       acute

      DATA xout /     0.0,   2.0,     5.0,   10.0,   25.0,   50.0,  
     &              100.0,  150.0,  200.0,  300.0,  400.0,  500.0,  
     &              750.0, 1000.0, 1500.0, 2000.0, 2500.0, 3000.0, 
     &             4000.0, 5000.0 /


      prgid = 'CR' 
      IF ( acute ) prgid = 'AC'

C  SET LOWER EDGE OF PUFF TO >= 1 METER

      IF ( zi1 .LT. 0. ) zi1 = 0.0
      z1 = zi1
      z2 = zi2 
      
      IF ( ( z2-z1 ) .LE. 0.0 )  THEN
         WRITE ( PrgStat(1:2),'(a2)'  )    prgid
         WRITE ( PrgStat(4:13),'(a10)' )  'PUFSHINE'
         WRITE ( PrgStat(15:18),'(i4)' )   9999
         WRITE ( PrgStat(31:50),'(a)' )   'z1 - z2 <= 0.0' 
         RETURN
      ENDIF

      mdist = 1
      drate0 = 0.0
      dd = 0.0
      pdx(1) = 0.0 

c  set initial number of volume elements
     
      ntheta = 3
      tindx = 1
      nr = 3
      rindx = 1

      IF ( sigy .GE. 10.0 ) THEN
      
c  reset numberof volume elements
      
         nr = 8
         rindx = 2 
         ntheta = 8
         tindx = 2 
      ENDIF

c  calculate dose rate under center of puff

      CALL R2QUAD( rsult )

      IF ( rsult .GE. 1.0E-30 ) THEN

c  dose rate is  significant

         IF ( sigy .GE. chgmod ) THEN
          
c  use semi-inifinite cloud approximation... dose rate under center is enuf
          
            drate0 = rsult
            RETURN
         ELSE
          
c  save dose rate under center of puff as initial pt in dose rate v. dist.          

            pdy(1) = rsult 
            test = AMAX1(1E-6 * pdy(1),1E-20)
         ENDIF
      ELSE
        
c  dose rate insignificant       

          pdy(1) = 0.
          mdist = 0
          RETURN
      ENDIF

c  determine distances for dose rate calculations
 
      indx = maxn
      DO i = 2,20
         IF ( xout(i) .LE. rdx(indx) ) THEN
            dd = xout(i)
            pdx(I) = dd
         ELSE
            dd = rdx( indx )
            pdx(i) = dd
            DO i1 = i+1,20
               pdx(i1) = 0.0
            ENDDO
         ENDIF
     
c  calculate dose rate at distance dd        
        
         CALL R2QUAD( rsult )

         IF ( rsult .LT. test ) THEN
            pdy(i) = rsult
            mdist = mdist + 1
            DO i1 = i+1,20
               pdy(i1) = 0.0               
            ENDDO                     
            EXIT 
         ELSE
            pdy(i) = AMIN1(rsult,pdy(i-1)) 
            mdist = mdist + 1
         ENDIF

         IF( dd .EQ. rdx( indx ) ) EXIT 

      ENDDO       

      RETURN 
      
      END