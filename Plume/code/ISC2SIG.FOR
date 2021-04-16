      SUBROUTINE ISC2SIG( dist, stabin, sigy, sigz )
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     ISC2SIG.FOR
c     Christian J Fosmire
c     Pacific Northwest Lab
c     P.O. Box 999
C     Richland, WA 99352
c
c     Created: 4/20/95
c
c     Description:  This subroutine calculates the dispersion 
c        coefficients used in the ISCST2 code.  These are approximated 
c        fits to the Pasquill-Gifford curves using the form:
c           sigy = 465.11628 * x * tan(TH)
c              where TH = 0.017453293[c - d * ln(x)]
c           and
c              sigz = ax^b
c
c           x is the distance (km), 
c           sigy and sigz (m)
c           c and d depend upon stability
c           a and b depend upon stability and distance
c        for unstable cases (A-C), sigz is limited to 5,000 meters 
c
c     Required Modules:  None  
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT    NONE
      
      REAL        a(10,6), b(10,6), c(6), cdist(9,6), d(6)  
      REAL        dist, distkm, sigy, sigycon, sigz, th, thcon
      
      INTEGER     numdist(6)
      INTEGER     i, stab, stabin
      
      DATA numdist / 8, 2, 1, 5, 8, 9 / 
      
      DATA cdist
     & / 0.10, 0.15, 0.20,  0.25,  0.30,  0.40,  0.50,  3.11,  0.0,
     &   0.20, 0.40, 0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.0,
     &   0.0,  0.00, 0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.0, 
     &   0.30, 1.00, 3.00, 10.00, 30.00,  0.00,  0.00,  0.00,  0.0, 
     &   0.10, 0.30, 1.00,  2.00,  4.00, 10.00, 20.00, 40.00,  0.0,
     &   0.20, 0.70, 1.00,  2.00,  3.00,  7.00, 15.00, 30.00, 60.0 / 
      
      DATA a / 122.800,  158.080,  170.220,  179.520, 217.410, 
     &         258.890,  346.750,  453.850,  453.850,   0.000,
     &          90.673,   98.483,  109.300,  7*0.000,
     &          61.141,   61.141,  8*0.000,
     &          34.459,   32.093,   32.093,   33.504,  36.650,
     &          44.053,  4*0.000,
     &          24.260,   23.331,   21.628,   21.628,  22.534,
     &          24.703,   26.907,   35.420,   47.618,   0.000,
     &          15.209,   14.457,   13.953,   13.953,  14.823,
     &          16.187,   17.836,   22.651,   27.074,  34.219 /
      
      DATA b / 0.94470,   1.05420,   1.09320,   1.12620, 1.26440, 
     &         1.40940,   1.72830,   2.11660,   2.11660, 0.00000,
     &         0.93198,   0.98332,   1.09710, 7*0.0,
     &         0.91465,   0.91465, 8*0.00000,
     &         0.86974,   0.81066,   0.64403,   0.60486, 0.56589,
     &         0.51179, 4*0.00000,
     &         0.83660,   0.81956,   0.75660,   0.63077, 0.57154,
     &         0.50527,   0.46713,   0.37615,   0.29592, 0.00000,
     &         0.81558,   0.78407,   0.68465,   0.63227, 0.54503,
     &         0.46490,   0.41507,   0.32681,   0.27436, 0.21716 /
      
      DATA c / 24.167, 18.333, 12.5, 8.333, 6.25, 4.1667 /
      
      DATA d / 2.5334, 1.8096, 1.0857, 0.72382, 0.54287, 0.36191 / 
      
      
      distkm = dist / 1000.
      stab = stabin
      
      IF( stab .GT. 6 ) stab = 6
      
      thcon = 0.017453293
      sigycon = 465.11628
      
      IF( distkm .LE. 0 ) THEN
         sigy = 0
         sigz = 0 
         RETURN
      ENDIF
      
      th = thcon * (c(stab) - d(stab) * LOG(distkm))
      sigy = sigycon * distkm * TAN(th)
      
      IF( distkm .GT. cdist(numdist(stab),stab) ) THEN
         sigz = a(numdist(stab)+1,stab) * distkm**
     &                                    (b(numdist(stab)+1,stab))
      ELSE
         DO i = 1, numdist(stab)
            IF( distkm .LT. cdist(i,stab) ) THEN
               sigz = a(i,stab) * distkm**(b(i,stab))
               EXIT
            ENDIF
         ENDDO
      ENDIF 
      
      IF( sigz .GT. 5000 ) sigz = 5000.
      
      RETURN
      
      END                    