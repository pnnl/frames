C-----------------------------------------------------------------------
C
      SUBROUTINE SWCAL
C                                                                             
C     SWCAL computes dilution factors for flow through river and 
C     near-shore lake environments, surface water concentration, and
C     decay during transit to irrigation withdrawl site.  Parts of
C     this module were adapted from the LADTAPII computer program.
C                                                                             
C     Module of Program SWATER of the GENII Software Package
C     Pacific Northwest National Laboratory as revised for EPA.
C
C     Last Modification: 29-Oct-96 DLS
C     
C                                                                             
C-----------------------------------------------------------------------
C                                                                             
C     D         - Dispersion factor, M**5/S**2         
C     DY        - Y-direction dispersion, M**2/S         
C     DZ        - Z-direction dispersion, M**2/S         
C     F         - Term in series expansion              
C     F1        - Integral value in series              
C     Q         - Total river flow rate, M**3/S        
C     SWDZI     - Depth of effluent discharge point used in lake calculation
C     SWOSYI    - Input point used in river calculation
C     VELOC     - Velocity used for concentration calculation, m3/sec
C                                                                             
C-----------------------------------------------------------------------------

c      INCLUDE 'CONC.CMN'
      INCLUDE 'OPT.CMN'
c      INCLUDE 'RAD.CMN'
      INCLUDE 'SWPAR.CMN'
c      INCLUDE 'TIMES.CMN'

      REAL VELOC

      IF (MIXFLG .GT. 0) THEN

C       Set initial parameter values--
        DY = 0.06 * SWDPTH * SWFLOW                                                               
        D = 0.06 * SWDPTH ** 3.0 * SWFLOW ** 2.0                                                        
        R = D * SWLSX                                                                     

      ENDIF
                                                                            
                                                                            
C---- River solution ---------------------------------------------------
      IF (MIXFLG .EQ. 1) THEN
                                                                             
C       Limit intake point to width of the river--

        SWOSYI = SWOSY                                                                            
        IF (SWOSY .GT. SWIDTH) SWOSYI = SWIDTH                                                    

        RJY=SWOSYI/SWIDTH                                                                
        Q=SWFLOW*SWDPTH*SWIDTH                                                                
        F1=0.                                                                  
        F=0.                                                                   
                                                                            
C       Series expansion limited to 100 terms--
        N=101                                                                  
  620   N=N-1                                                                  
        IF(N .LE. 0) GO TO 790                                                 
        E1=-(N*P/Q)**2.*R                                                      
        IF(ABS(E1) .GT. 20.) GO TO 620                                         
        E3=N*P*RJY                                                             
                                                                            
C       Terms of series--
  760   F=EXP(E1)*COS(E3)                                                      
        F1=F1+F                                                                
        GO TO 620                                                              
  790   A=1.+2.*F1                                                             
                                                                            
C       Limit normalized conc and dilution--
        IF(A .LT. 1.00E-20) THEN                                          
          A=0.                                                                   
          GO TO 93
        ELSE                                                                            
          MIXR=(SWQB*A)/Q                                                          
        ENDIF

C---- Lake Solution ---------------------------------------------------------
      
      ELSEIF (MIXFLG .EQ. 2) THEN

        SWDZI = SWDZ
        IF (SWDZ .GT. SWDPTH) SWDZI = SWDPTH                                                      
        DZ=0.0059*SWDPTH*SWFLOW                                                          
        M=SQRT(0.295*SWLSX/SWDPTH) + SWDZI/2./SWDPTH                                               

C       Failure--
        IF (M .LT. 1) GO TO 93

        DZXU=4.*DZ*SWLSX/SWFLOW                                                         
        DYXU=4.*DY*SWLSX/SWFLOW                                                         

C       Divide-by-zero check--
        IF (DZXU .EQ. 0.0) GOTO 91
        IF (DYXU .EQ. 0.0) GOTO 92

        AA=(2.*M*SWDPTH-SWDZI)**2./DZXU + SWOSY**2./DYXU                                    

C       Failure--
        IF (AA .GE. 50) GO TO 93
                                                                            
C       Series expansion limited to M terms (i.e., EXP(-50)>solution)--
                                                                            
        MM=-M-1                                                                
        M=2*M+1                                                                
        F1=0.                                                                  
        FY=0.                                                                  
        FZ=0.                                                                  
        AA=0.                                                                  

        DO 123 I1=1,M                                                          
          I=MM+I1                                                                
          AA=(2.*I*SWDPTH-SWDZI)**2./DZXU                                                 
          AA=EXP(-AA)                                                            
          FZ=FZ+2.*AA                                                            
  123   CONTINUE

        AA=SWOSY**2./DYXU                                                          
        FY=2.*EXP(-AA)                                                         
        F1=SWQB/4./P/SWLSX/SQRT(DY*DZ)                                               
        MIXR=F1*FY*FZ                                                          

      ENDIF

C----- Calculate surface water concentrations---------------------------
 
C     Apply mixing ratio to release term--

      IF (MIXFLG .EQ. 1 .OR. MIXFLG .EQ. 2) THEN
        VELOC = SWQB
      ELSE
        VELOC = SWFLOW
      ENDIF

      DO 102 IN = 1, NONUC
        SWCON(IN) = QSW(IN) * MIXR / VELOC * M3L * YRSEC
  102 CONTINUE

C     Decay surface water for transit time to irrigation withdrawl
C     location--
      IF (QSW(1) .GT. 0.0) THEN
        T = SWTT * YRHR
        CALL CHAIN (T, DUMMY, SWCON, SWIRR, 0)
      ENDIF

      RETURN                                                                  

C---- Error Messages ---------------------------------------------------

   91 WRITE (*,*) ' SWCAL:  DZXU = 0.0, invalid (divide by zero)' 
      CALL EXIT (1)

   92 WRITE (*,*) ' SWCAL: DYXU = 0.0, invalid (divide by zero)' 
      CALL EXIT (1)

   93 MIXR=1.E-20                                                                
      WRITE (*,*)      ' SWCAL:  The dilution calculation has failed;'
      WRITE (*,'(2A)') '         A value of 1E-20 will be returned ',
     .                 'for the mixing ratio.'         
      RETURN
                                                             
C-----------------------------------------------------------------------
      END
