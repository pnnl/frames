C   SWCHRON.FOR                        Version Date: 08-Jan-98               *
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.     *
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE SWCHRON                               *
C                                                                            *
C  Subroutine SWCHRON evaluates water concentration at a location for one    *
C             decay chain for release to a river or near-shore lake          *
C
C  Written by:       RA Peloquin/Dennis Strenge                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    31-Oct-1984                                             *
C  Last Modified:    17-Aug-09  BAN                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: GENII/SWATER
C     Called by: SWATER
C     Calls: NONE
C     Common blocks referenced: DEVICE, SWPAR, SWINFO, SWCON, OPT
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
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
C     VELOC     - Flow dilution model rate (m3/sec)                                                                        
C-----------------------------------------------------------------------------
C     
C==== Modification History ===================================================
C
C   Date      Who  Modification Description
C  ---------  ---  ------------------------------------------------------------
C  25-Feb-97  DLS  Added common bocks OPT
C  28-Jul-97  DLS  Parameter DUMMY made a real array(9)
C  08-Jan-98  DLS  Revised for addition of flow dilution model
C  21-Aug-08  BAN  Removed (what i hope is) unnecessary underflow trap
C  17-Aug-09  BAN  Add NRC Recirculation models
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE SWCHRON(IL)
C
C==== COMMON Block Definitions ===============================================
C
        INCLUDE 'DEVICE.CMN'
        include 'SWPAR.CMN'
        INCLUDE 'SWINFO.CMN'
        INCLUDE 'SWCON.CMN'
        INCLUDE 'DECAY.CMN'
        INCLUDE 'OPT.CMN'
C
C==== DIMENSION Statements ===================================================
C
      REAL DUMMY(9)
C 
C==== Variable Declarations ==================================================
C
      CHARACTER*15  RIVER,NSLAKE
c
      REAL VELOC
C
C==== DATA Statements ========================================================
C
      DATA RIVER/'River          '/,  NSLAKE/'Near-shore lake'/
C
C---- Calculate water dispersion factor --------------------------------------
C
C-----  Set initial parameter values--
C                                     
        DUMMY = 0.0
	  VELOC = SWFLOW(IL)
        DY = 0.06 * SWDPTH(IL) * VELOC
        D = 0.06 * SWDPTH(IL) ** 3.0 * VELOC ** 2.0                               
        R = D * SWLSX(IL)
	DO I=1,9
	  QSWR(I) = QSW(I)
	ENDDO

	CALL RECIRC
C                                                                            
C---- River solution ---------------------------------------------------
C
      IF(SWMODL.EQ.RIVER) THEN
C
C---- Check for flow dilution model or river dispersion model
C
       IF(KSWTYPE.EQ.1) THEN    ! river dispersion model

c
C       Limit intake point to width of the river--
c
        SWOSYI = SWOSY(IL)                                                                            
        IF (SWOSY(IL) .GT. SWIDTH(IL)) SWOSYI = SWIDTH(IL)                                                    

        RJY=SWOSYI/SWIDTH(IL)                                                                
        Q=VELOC*SWDPTH(IL)*SWIDTH(IL)                                                                
        F1=0.                                                                  
        F=0.                                                                   
                                                                            
C       Series expansion limited to 100 terms--
        N=101                                                                  
  620   N=N-1                                                                  
        IF(N .LE. 0) GO TO 790                                                 
        E1=-(N*P/Q)**2.*R                                                      
        IF(ABS(E1) .GT. 20.) GO TO 620                                         
        E3=N*P*RJY                                                             
C
C------ Terms of series--
C
  760   F=EXP(E1)*COS(E3)                                                      
        F1=F1+F                                                                
        GO TO 620                                                              
  790   A=1.+2.*F1                                                             
C                                                                           
C------ Limit normalized conc and dilution--
C
        IF(A .LT. 1.00E-20) THEN
          A=0.                                                                   
          GO TO 93
        ELSE                                                                            
          MIXR=A/Q                                                          
        ENDIF
        DO IN = 1, NUC
          SWCON(IN) = QSWR(IN) * MIXR * M3L * YRSEC
        END DO
C
       ELSE    !flow dilution model
        DO IN = 1, NUC
          SWCON(IN) = QSWR(IN) * M3L * YRSEC / VELOC
        END DO
C
       ENDIF
C
C---- Lake Solution ---------------------------------------------------------
C      
      ELSEIF (SWMODL.EQ.NSLAKE) THEN
        SWDZI = SWDZ
        IF (SWDZ .GT. SWDPTH(IL)) SWDZI = SWDPTH(IL)                                                      
        DZ=0.0059*SWDPTH(IL)*VELOC
        M=SQRT(0.295*SWLSX(IL)/SWDPTH(IL)) + SWDZI/2./SWDPTH(IL)                                               
C
C-----  Failure ---
C
        IF (M .LT. 1) GO TO 93
        DZXU=4.*DZ*SWLSX(IL)/VELOC
        DYXU=4.*DY*SWLSX(IL)/VELOC
C
C------ Divide-by-zero check--
C
        IF (DZXU .EQ. 0.0) GOTO 91
        IF (DYXU .EQ. 0.0) GOTO 92
C the following 2 executable lines were taken out by BAN 21 Aug 2008
c we'll let the "M" number of loops go.  As far as numeric underflows, the
C newer compliers and 32 bit machines can take care of themselves...

c        AA=(2.*M*SWDPTH(IL)-SWDZI)**2./DZXU + SWOSY(IL)**2./DYXU                                    
C
C------ Failure
C
c        IF (AA .GE. 50) GO TO 93
C                                                                           
C------ Series expansion limited to M terms (i.e., EXP(-50)>solution)--
C
        MM=-M-1                                                                
        M=2*M+1                                                                
        F1=0.                                                                  
        FY=0.                                                                  
        FZ=0.                                                                  
        AA=0.                                                                  
        DO 123 I1=1,M                                                          
          I=MM+I1                                                                
          AA=(2.*I*SWDPTH(IL)-SWDZI)**2./DZXU                                                 
          AA=EXP(-AA)                                                            
          FZ=FZ+2.*AA                                                            
  123   CONTINUE
        AA=SWOSY(IL)**2./DYXU                                                          
        FY=2.*EXP(-AA)                                                         
        F1=1./(4.*P*SWLSX(IL)*SQRT(DY*DZ))
        MIXR=F1*FY*FZ                                                          
        DO IN = 1, NUC
          SWCON(IN) = QSWR(IN) * MIXR * M3L * YRSEC
        END DO
C
      ENDIF
C
C-----  Decay surface water for transit time to withdrawl location.
C
      IF (QSWR(1) .GT. 0.0) THEN
        T = SWTT(IL) * DAYHR
        CALL CHAIN (T, DUMMY, SWCON, SWCON, 0)
      ENDIF
      IF(DEBUG) write(NSLS,101) nuc,(swcon(i),i=1,nuc)
 101  format(' Chronic water conc. nuc =',I3,' Water Conc:'/
     .       1p9e9.1)
      RETURN                                                                  
C
C---- Error Messages ---------------------------------------------------
C
   91 WRITE (NERR ,*) ' SWCHRON:  DZXU = 0.0, invalid (divide by zero)'
      NERROR = NERROR + 1
      RETURN        
C
   92 WRITE (NERR,*) ' SWCHRON: DYXU = 0.0, invalid (divide by zero)'
      NERROR = NERROR + 1
      RETURN
C
   93 MIXR=1.E-20  
c---ADDED BY BAN ON 11 AUG 2008
         DO IN = 1, NUC
          SWCON(IN) = QSW(IN) * MIXR * M3L * YRSEC
        END DO
c                                                                
      WRITE (NSLS,*)   ' SWCHRON:  The dilution calculation has failed;'
      WRITE (NSLS,'(2A)') '         A value of 1E-20 will be returned ',
     .                 'for the mixing ratio.'         
 99   RETURN
      END
C
C====================== END OF MODULE SWCHRON ============================
C
