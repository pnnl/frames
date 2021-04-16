      SUBROUTINE NRCDIST( stab, sigmay, sigmaz, disty, distz )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     NRCDist.FOR
c     Christian J Fosmire
c     Pacific Northwest Laboratories
c     P.O. Box 999
c     Richland, WA 99352
c
c     Created: 6/28/95
c
c     Description:  Calculates the distances (disty distz) for which
c      sigy and sigz equal the given sigy and sigz using the NRC
c      method for calculating sigy and sigz
c
c     Required Modules: None
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc      
      IMPLICIT    NONE
      
      REAL        ay(7), az(7,3), bz(7,3), cz(7,3)
      REAL        disty, distz, sigmay, sigmaz
      
      INTEGER     i, stab      
      
      DATA ay/ 0.3658, 0.2751, 0.2089, 0.1471, 0.1046, 0.0722, 0.0481 /
      DATA az/ 0.19200,  0.156,  0.116, 0.079, 0.063, 0.053, 0.032,
     +         0.00066,  0.0382, 0.113, 0.222, 0.211, 0.086, 0.052,
     +         0.00024,  0.055,  0.113, 1.26,  6.73, 18.05, 10.83  /
      DATA bz/ 0.936,  0.922, 0.905, 0.881, 0.871, 0.814, 0.814,
     +         1.941,  1.149, 0.911, 0.725, 0.678, 0.74,  0.74,
     +         2.094,  1.098, 0.911, 0.516, 0.305, 0.18,  0.18  /
      DATA cz/ 0.0,    0.0,   0.0,   0.0,   0.0,   0.0,   0.0,
     +         9.27,   3.3,   0.0,  -1.7,  -1.3,  -0.35, -0.21,
     +        -9.6,    2.0,   0.0, -13.0, -34.0, -48.6, -29.2  /
      
      IF( sigmaz .LE. 100.0 ) THEN
         DO i = 1, 3
            distz = ((sigmaz - cz(stab,i))/az(stab,i))**(1.0/bz(stab,i))
            IF( distz .LE. 10**(i+1) ) EXIT
         ENDDO
      ELSE IF( sigmaz .LE. 1000.0 ) THEN
         DO i = 2, 3
            distz = ((sigmaz - cz(stab,i))/az(stab,i))**(1.0/bz(stab,i))
            IF( distz .LE. 1000 ) EXIT
         ENDDO
      ELSE
         distz = ((sigmaz - cz(stab,3))/az(stab,3))**(1.0/bz(stab,3))
      ENDIF
      
      IF (sigmay .GT. 0.0) disty = (sigmay / ay(stab))**(1/0.9031)
      
      RETURN 
      
      END