
      SUBROUTINE VERDIST ( m )
C-----------------------------------------------------------------------
c
c     VERDIST
c
c     Date:              April 3, 1996
c
c     Description:       This subroutine computes the terms associated 
c                        with the vertical distribution in the puffs.  
c 
c     Required modules:  None
c       
C-----------------------------------------------------------------------

      IMPLICIT     NONE

      INCLUDE      'parm.inc'
      INCLUDE      'const.inc'
      INCLUDE      'difter.inc'
      INCLUDE      'met_data.inc'
      INCLUDE      'puffs.inc'

      REAL         exparg, udepth, zratio

      INTEGER      m

      IF ( sigmaz(m) .GE. sigzlim ) THEN

c  uniform mixing  
 
         udepth = sigmaz(m) / 1.05    
         pufchi = dt / (twopi * sigysq * udepth)
         rpew = sigmay(m) * radcnst / delxy
         vexp = 1.0
              
      ELSE IF ( zp(m) .GT. ldepth ) THEN
 
c  not uniformly mixed, puff center above mixing layer

         pufchi = 2.0 * dt / ( twopi**1.5 * sigysq * sigmaz(m) )
         rpew = sigmay(m) * radcnst / delxy   
         zratio = zp(m)**2 / sigzsq
         IF ( zratio .GT. 46.0 ) THEN
            vexp = 0.0
         ELSE
            vexp = EXP(-0.5 * zratio)
         ENDIF

      ELSE
 
c  not uniformly mixed, puff center at top of or within mixing layer
 
         pufchi = 2.0 * dt / ( twopi**1.5 * sigysq * sigmaz(m) )
         rpew = sigmay(m) * radcnst / delxy
         exparg = 0.5 * zp(m)**2 / sigzsq
         IF ( exparg .LT. 15 ) THEN
            vexp = EXP ( -exparg )
            exparg = 0.5 * (2*ldepth - zp(m))**2 / sigzsq
            IF ( ABS ( exparg ) .LT. 15 )
     &               vexp = vexp + EXP ( -exparg )
            exparg = 0.5 * (2*ldepth + zp(m))**2 / sigzsq
            IF ( ABS ( exparg ) .LT. 15 )
     &               vexp = vexp + EXP ( -exparg )
         ENDIF

      ENDIF

c      IF ( m .EQ. 1 ) WRITE ( 25,'(a, 4f10.2,1pe10.2)' ) 
c     & 'Subroutine VerDist', age(m), dt, sigmay(m), sigmaz(m), pufchi

      RETURN

      END
