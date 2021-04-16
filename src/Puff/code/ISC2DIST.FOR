      SUBROUTINE ISC2DIST( stab, sigy, sigz, disty, distz )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     ISC2Dist.FOR
c     Christian J Fosmire
c     Pacific Northwest Laboratories
c     P.O. Box 999
c     Richland, WA 99352
c
c     created: 6/28/95
c
c     description:  Calculates the distances (distz and disty) at which 
c       sigy and sigz are equal to the inputted values.  The distz 
c       calculations are analytical solution to the equation for sigz.
c       The disty calculations are taken directly from the ISC2 code 
c       (subroutine XYV); the sigy equation for ISC2 do not have 
c       analytical solutions for x.
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc      
      IMPLICIT    NONE
      
      REAL        a(10,6), b(10,6), cdist(9,6), sp(6), sq(6)
      REAL        distkm, disty, distz, sigy, sigz
      
      INTEGER     numdist(6)
      INTEGER     i, stab      
      
      DATA sp/0.004781486, 0.006474168, 0.009684292, 0.014649868,
     &        0.019584802, 0.029481132/
      
      DATA sq/1.1235955, 1.1086475, 1.0905125, 1.0881393, 1.0857763,
     &        1.0881393/   
      
      DATA numdist /8, 2, 1, 5, 8, 9/ 
      
      DATA cdist/0.10, 0.15, 0.20, 0.25, 0.30, 0.40, 0.50, 3.11, 0.0,
     &   0.20, 0.40,  0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,
     &   0.0,  0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0, 
     &   0.30, 1.00, 3.00, 10.00, 30.00,  0.0,   0.0,   0.0,   0.0, 
     &   0.10, 0.30, 1.00,  2.00,  4.00, 10.00, 20.00, 40.00,  0.0,
     &   0.20, 0.70, 1.00,  2.00,  3.00,  7.00, 15.00, 30.00, 60.00/ 
      
      DATA a /122.80,  158.08,  170.22,  179.52, 217.41, 
     &        258.89,  346.75,  453.85,  453.85,   0.0,
     &         90.673,  98.483, 109.300, 7*0.0,
     &         61.141,  61.141, 8*0.0,
     &         34.459,  32.093,  32.093, 33.504, 36.650,
     &         44.053, 4*0.0,
     &         24.260,  23.331,  21.628, 21.628, 22.534,
     &         24.703,  26.907,  35.420, 47.618,  0.0,
     &         15.209,  14.457,  13.953, 13.953, 14.823,
     &         16.187,  17.836,  22.651, 27.074, 34.219/
      
      DATA b /0.9447,    1.0542,   1.0932,   1.1262,  1.2644, 
     &        1.4094,    1.7283,   2.1166,   2.1166,  0.0,
     &        0.93198,   0.98332,  1.0971, 7*0.0,
     &        0.91465,   0.91465,8*0.0,
     &        0.86974,   0.81066,  0.64403,  0.60486, 0.56589,
     &        0.51179, 4*0.0,
     &        0.8366,    0.81956,  0.7566,   0.63077, 0.57154,
     &        0.50527,   0.46713,  0.37615,  0.29592, 0.0,
     &        0.81558,   0.78407,  0.68465,  0.63227, 0.54503,
     &        0.46490,   0.41507,  0.32681,  0.27436, 0.21716/

      IF( stab.gt.6 ) stab = 6 
       
      DO i = 1, numdist(stab)
         distkm = (sigz/a(i,stab))**(1.0/b(i,stab))
         IF( distkm.lt.cdist(i,stab) ) THEN
            EXIT
         ENDIF
      ENDDO
      
      distz = distkm * 1000.
      
      disty = (sigy*sp(stab))**sq(stab) * 1000.
      
      RETURN
      END      