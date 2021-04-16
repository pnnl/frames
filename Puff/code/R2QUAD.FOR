      SUBROUTINE R2QUAD( dose )  
C---------------------------------------------------------------------------
c
c     R2QUAD                                         
c     
c     Date:              October 16,1997
c
c     Description:       computes the dose from a puff starting from 
c                        individual points 
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
              
      REAL           costh(8,2), hexpy(8,2), rdis(8,2), rarea(8,2), 
     &               vract(8,4)
      
      REAL           dose,  pi, ptdos, rd, rho, rhosqr, secdos
      
      INTEGER        ir, it, iz, nz

C  COSINE VALUES FOR COMPUTING THE DISTANCE BETWEEN A POINT IN THE
C  PUFF AND A RECEPTOR

      DATA   costh/ 0.8660254, 0.0, -0.8660254, 5*0.0,
     &              0.9807853,  0.8314696,  0.5555702,  0.1950903,
     &             -0.1950903, -0.5555702, -0.8314696, -0.9807853 /

C ** r/sigma y for points in puff

	DATA   rdis /  0.467, 1.037, 1.669, 5*0.0,
     &               0.177, 0.404, 0.636, 0.892, 
     &               1.130, 1.378, 1.626, 1.875 / 

C  HORizONTAL EXPONENTIAL TERM FOR POINTS IN THE PUFF

      DATA   hexpy/ 0.89669, 0.58410, 0.24838, 5*0.0,
     &              0.98446, 0.92163, 0.81689, 0.67178,
     &              0.52811, 0.38696, 0.26662, 0.17242 /

C  HORizONTAL AREAS OF FINitE PUFF VOLUME ELEMENTS NORMALizED BY
C  SIGMA Y SQUARED

      DATA   rarea/ 0.2327106, 0.6981317, 1.1635528, 5*0.0,
     &              0.0122718, 0.0365155, 0.0613592, 0.0859029,
     &              0.1104466, 0.1349903, 0.1595340, 0.1840777 /

c	sub_id = 'Subroutine R2QUAD'

      pi = 3.14159
      nz = 4

C  COMPUTE THE RELATIVE MASS REPRESENTED BY EACH POINT IN 
C  A SECTOR

      DO iz = 1,nz
         DO ir = 1,nr
            vract(ir,iz) = dpufchi * hexpy(ir,rindx) * vexpz(iz) 
     &                     * rarea(ir,rindx) * sigy**2 * cylzd(iz)
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
               rd = rdis(ir,rindx) * sigy 
               rhosqr = z(iz)**2 + dd**2 + rd**2 
     +               - 2.0 * dd * rd * costh(it,tindx)

               IF ( rhosqr .LT. 1.0 ) THEN
                  rhosqr = 1.0
                  rd = 1.0
                  rho = 1.0
               ELSE
                  rho = SQRT( rhosqr )
               ENDIF

               ptdos = QINTERP(maxn,rho,rdxl,rdyc)
     &                         * vract(ir,iz)

               secdos = secdos + ptdos

            ENDDO  
         ENDDO

         IF ( secdos .LT. 1E-30 ) THEN

C  DOSE CONTRIBUTION BELOW THRESHOLD       

              RETURN

         ELSE IF ( dd .EQ. 0.0 ) THEN

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
