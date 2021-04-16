     
      SUBROUTINE SHINEPRP( zp, sigmay, sigy2, sigz, sigz2, sigzlim, 
     &                     ldepth )
     
C---------------------------------------------------------------------------
c
c     SHINEPRP                                          
c
c     Date:              March 19, 1998 
c
c     Description:  This subroutine calculates various height parameters 
c                   and vertical distribution for the cloud shine
c                   calculations.
c
c     Required modules:  None
c
C---------------------------------------------------------------------------

      IMPLICIT        NONE

      INCLUDE         'parm.inc'
      INCLUDE         'shine.inc'

      REAL             cylz(5), cylzm(4,3)  
      
      REAL             cyldz, ldepth, pi,  sigmay, sigy2, sigz, sigz2, 
     &                 sigzlim, twopi, udepth, zp
     
      INTEGER          nn, zindx

      DATA             cylz / 0.0, 0.1, 0.3, 0.6,  1.0 /
      DATA             cylzm / 0.05, 0.19, 0.41, 0.70,
     &                         0.06, 0.24, 0.47, 0.68,
     &                         0.05, 0.20, 0.45, 0.80  /

      pi = 3.14159
      twopi = 2 * pi
      udepth = 0.0
      
      sigy = sigmay

c  bottom of puff
       
      zi1 = MAX ( (zp - 3.0 * sigz),0.0 )

      IF ( sigz .GT. sigzlim ) THEN

C  **  WITH UNIFORM DISTRIBUTION IN THE VERTICAL

         udepth = sigz / 1.05
         dpufchi = 1.0 / ( twopi * sigy2 * udepth )
         zi2 = udepth 
         cyldz = zi2 - zi1
         DO nn = 1,4
            cylzd(nn) = ( cylz(nn+1) - cylz(nn) ) * cyldz
            vexpz(nn) = 1.0
            z(nn) = zi1 + cylzm(nn,3) * cyldz
         ENDDO

      ELSE

C  **  REFLECTED GAUSSIAN DISTRIBUTION IN THE VERTICAL

         dpufchi = 1.0 / ( twopi**1.5 * sigy2 * sigz )
         zi2 = zp + 3.0 * sigz
         cyldz = zi2 - zi1                   
         zindx = 1
         IF( zp .GT. 10.0 ) zindx = 2
         DO nn = 1,4
             cylzd(nn) = ( cylz(nn+1) - cylz(nn) ) * cyldz
             z(nn) = zi1 + cylzm(nn,zindx) * cyldz
             vexpz(nn) = EXP (-0.5 * ( zp-z(nn) )**2 / sigz2 ) 
     &                +  EXP (-0.5 * (-zp-z(nn) )**2 / sigz2 ) 
     &                +  EXP (-0.5 * (2.0*ldepth+zp-z(nn))**2 / sigz2 )
     &                +  EXP (-0.5 * (-2.0*ldepth-zp-z(nn))**2 / sigz2 )
     &                +  EXP (-0.5 * (4.0*ldepth+zp-z(nn))**2 / sigz2 )
     &                +  EXP (-0.5 * (-4.0*ldepth-zp-z(nn))**2 / sigz2 )
             IF ( zp .LE. ldepth ) 
     &          vexpz(nn) = vexpz(nn) 
     &                +  EXP (-0.5 * (2.0*ldepth-zp-z(nn))**2 / sigz2 )
     &                +  EXP (-0.5 * (-2.0*ldepth+zp-z(nn))**2 / sigz2 )
     &                +  EXP (-0.5 * (4.0*ldepth-zp-z(nn))**2 / sigz2 )
     &                +  EXP (-0.5 * (-4.0*ldepth+zp-z(nn))**2 / sigz2 )  
          ENDDO

      ENDIF

      RETURN
      END 
