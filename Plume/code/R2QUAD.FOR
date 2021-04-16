
      SUBROUTINE R2QUAD( dose )  
      
C---------------------------------------------------------------------------
c
c     R2QUAD                                         
c     
c     Date:              August 17, 1995
c
c     Description:  computes the dose from a puff starting from 
c                   individual points 
c
c     Required modules:
c
c          Subroutines:  None
c
c            Functions:  QINTERP
c
C---------------------------------------------------------------------------
      
      IMPLICIT       NONE
      
      INCLUDE        'parm.inc'
      INCLUDE        'shine.inc'
              
      REAL           QINTERP        
              
      REAL           costh(8,2), hexpy(8,3), rarea(8,3), rmass(8,3)
      
      REAL           dose, phdens, pi, ptdos, rd, rho, rhosqr, secdos,
     &               sphdiv 
      
      INTEGER        ir, it, iz, nz

C  COSINE VALUES FOR COMPUTING THE DISTANCE BETWEEN A POINT IN THE
C  PUFF AND A RECEPTOR

      DATA   costh/ 0.8660254, 0.0, -0.8660254, 5*0.0,
     &              0.9807853,  0.8314696,  0.5555702,  0.1950903,
     &             -0.1950903, -0.5555702, -0.8314696, -0.9807853 /

C  HORizONTAL EXPONENTIAL TERM FOR POINTS IN THE PUFF

      DATA   hexpy/ 0.9459595, 0.6065307, 0.2493522, 5*0.0,
     &              0.9801987, 0.8352702, 0.6065307, 0.3753111, 
     &              0.1978987, 3*0.0,
     &              0.9922179, 0.9321025, 0.8225776, 0.6819408,
     &              0.5310960, 0.3885581, 0.2670518, 0.1724216 /

C  HORizONTAL AREAS OF FINitE PUFF VOLUME ELEMENTS NORMALizED BY
C  SIGMA Y SQUARED

      DATA   rarea/ 0.2327106, 0.6981317, 1.1635528, 5*0.0,
     &              0.0314159, 0.0942478, 0.1570796, 0.2199115,
     &              0.2827433, 3*0.0,
     &              0.0122718, 0.0365155, 0.0613592, 0.0859029,
     &              0.1104466, 0.1349903, 0.1595340, 0.1840777 /

      pi = 3.14159
      nz = 3

C  COMPUTE THE RELATIVE MASS REPRESENTED BY EACH POINT IN 
C  A SECTOR

      DO iz = 1,3
         DO ir = 1,nr
            rmass(ir,iz) = hexpy(ir,rindx) * vexpz(iz) 
     &                   * rarea(ir,rindx) * sigy**2 * cylzd(iz)
         ENDDO   
      ENDDO

C  DO OUTER LOOP COMPUTES THE DOSE BY SUMMING CONTRIBUTIONS
C  OF THE INDIVIDUAL LAYERS -- STARTING WitH LOWEST LAYER 

      dose = 0.0
      DO it = 1,ntheta
         secdos = 0.0
         DO iz = 1,nz
            DO ir = 1,nr 
               ptdos = 0.0
               rd = ( 2 * ir - 1 ) * sigy / nr
               rhosqr = Z(iz)**2 + DD**2 + rd**2 
     +               - 2.0 * DD *rd * costh(it,tindx)
               IF ( rhosqr .LT. 0.0001 ) THEN
                  rhosqr = 0.0001
                  rd = 0.01
                  rho = 0.01
               ELSE
                  rho = SQRT( rhosqr )
               ENDIF
               sphdiv = 1.0 / ( 4.0 * pi * rhosqr )  
               phdens = QINTERP( ndxt, rho, rdxl, rdyc )
               ptdos = 592.0 * phdens * rmass( ir,iz ) * sphdiv
               secdos = secdos + ptdos
            ENDDO  
         ENDDO

         IF ( secdos .LT. 1E-10 ) THEN

C  DOSE CONTRIBUTION BELOW THRESHOLD       

              RETURN

         ELSE IF ( DD .EQ. 0.0 ) THEN

C  POINT DIRECTLY BELOW CENTER OF PUFF TAKE ADVANTAGE OF SYMMETRY NOT
C  A FUNCTION OF THETA ( MULTIPLY BY TOTAL NUMBER OF SECTORS )

            dose = 2 * ntheta * secdos
              
            RETURN 
         ELSE 

C   POINT NOT BELOW CENTER OF PUFF TAKE ADVANTAGE OF SYMMETRY (MULTIPLY
C   BY 2 TO ACCOUNT FOR 2 HALVES OF PUFF)

            dose = dose + 2 * secdos
         ENDIF         

      ENDDO 

      RETURN
      
      END 
