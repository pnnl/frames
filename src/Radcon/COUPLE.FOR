C     MEPAS RADCON: COUPLE.FOR            Version Date: 12-22-1994
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                            SUBROUTINE COUPLE                               *
C                                                                            *
C  Subroutine COUPLE computes the boundary conditions to be passed to the    *
C  exposure module.  The concentration and time values are written to unit   *
C  7.  Subroutine COUPLE is called by the main programs and calls            *
C  subroutine INTERP.                                                        *
C                                                                            *
C  Written by:            Gene Whelan                                        *
C                         Battelle Pacific Northwest Laboratories            *
C                         P.O. Box 999                                       *
C                         Richland, WA  99352                                *
C                                                                            *
C  Creation Date:         01/19/91 (Converted to PC)                         *
C  Last Modified:         12/22/94 - JPM                                     *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C      Module of: RADCON
C      Called by: RADCONPC
C      Calls to: SUBROUTINE INTERP
C      Common blocks referenced: INTER, TIME, LIST, INDEX3, INDEX5, INDEX6,   
C                                COM21                                        
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter      Set/                Location
C     Name           Used      Type      Export/Import  Parameter Description 
C     -------------  ------  --------   --------------- ----------------------
C     CS             USED      REAL      COMMON - EXP   BECAUSE IT IS THE LAST
C                                                       MEDIA, THE VALUE IN CS
C                                                       IS THE CONCENTRATION  
C                                                       OF DECAYED CONTAMINANT
C     JJFLAG         SET/USED  INTEGER   LOCAL          OFF (EQUAL TO ZERO)
C                                                       UNTIL THE FIRST 70-YEAR
C                                                       AVERAGE CONCENTRATION
C                                                       GREATER THAN ZERO IS
C                                                       FOUND.  USED TO AVOID
C                                                       LEADING ZERO'S IN THE
C                                                       *.WAT FILE
C     M1             S/U       INTEGER   LOCAL          INDEX FOR # OF PARENT 
C                                                       CONTAMINANTS          
C     M3             S/U       INTEGER   LOCAL          INDEX FOR TOTAL # OF  
C                                                       CONSTITUENTS          
C                                                       PARENTS PLUS DAUGHTERS
C     NDS            USED      INTEGER   ARG - IMPORT   TOTAL # IN CONSTITUENT
C                                                       CHAIN                 
C                                                       PARENT PLUS DAUGHTERS 
C     NUM            USED      INTEGER   COMMON - IMP   BECAUSE IT IS LAST    
C                                                       MEDIA, NUM HAS BEEN   
C                                                       SET TO NTIMES + 1     
C     NUMCON         USED      INTEGER   COMMON - IMP   TOTAL NUMBER OF PARENT
C                                                       CONTAMINANTS          
C     PNT            USED      REAL      COMMON - IMP   FINAL CONCENTRATIONS  
C                                                       FOR THE LAST MEDIA    
C                                                       TO BE SENT TO THE     
C                                                       EXPOSURE MODULE       
C     PNT1           SET       REAL      LOCAL          TEMPORARY HOLDING FOR 
C                                                       PARENT CONCENTRATIONS 
C     TSTRT1         USED      REAL      COMMON - IMP   TIME OF THE BEGINNING 
C                                                       OF PREVIOUS MEDIA     
C                                                       SOURCE TERM           
C     TEND1          USED      REAL      COMMON - INP   TIME OF THE END OF    
C                                                       PREVIOUS MEDIA        
C                                                       SOURCE TERM           
C     T2             USED      REAL      COMMON - INP   MAXIMUM TIME UNDER    
C                                                       CONSIDERATION FOR     
C                                                       CURRENT MEDIA         
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     01/26/89     JWB  Maximum conc. for contituents is computed (CMAX)      
C     03/24/89     JWB  Decay-product information modified and Logical unit 7 
C                       used to transfer this information to HAZ              
C     11/24/91     JWB  Added required comments and header for MEPAS QA plan  
C     07/23/92     JWB  Modified DO LOOP 987 to compute # of daughters right  
C     08/10/92     JWB  Modified calculation of TIMAX (loop by NTIMES,not III)
C     08/12/92     JWB  Modified logic associated with INTERP for I=1 or III  
C     11/08/92     JWB  TSTRT1 equation modified to compute 70-years different
C     11/24/92     JWB  Modifications to put real time in WAT file.  Also     
C                       correction to Do Loop 988 (moved 399 within 989 Loop  
C     09/01/93     JLS  Introduced TDELTA to allow time discounting to begin
C                       at times earlier than the start of risk calculations.
C     09/13/93     JLS  Replaced the INTERP() subroutine with the AVG70Y()
C                       function to find true 70 yr. averaging.
C     05/31/94  AdH/JWB Changed computation so that more than 200 70.yr 
C                       averages can be written out to the WLS.file.
C     06/03/94     JPM  Made changes so concentrations at specific times
C                       are written to the *.WAT file as if they were
C                       70 year averages.
C    06/15/94      JPM  Function AVG70Y checks the start time for the 70 yr
C                       interval, and if it is less than the start time for
C                       the PNT array, then AVG70Y returns a zero for the
C                       70 yr average.  For the first 70 yr interval, the
C                       start time of the interval should equal the start time
C                       of the PNT array.  However, precision problems can
C                       result in the interval start time being a rounded
C                       start time of the PNT array.  In the case where it
C                       is rounded down, AVG70Y return a zero.  So, if the
C                       70 yr interval being computed is the first one, pass
C                       the start time of the PNT array (TSTRT1) to AVG70Y.
C    07/18/94      JPM  Made changes so the 70-year averages always begin
C                       at the start time for the risk calculations.  Added
C                       JJFLAG so leading zero average concentrations are
C                       not written to the *.WAT file.
C    07/21/94      JPM  Changed how the CS_SPEC array is accessed, since the
C                       concentration at time zero is no longer stored in
C                       this array (see comment in WLIST.FOR).
C    12/22/94      JPM  Writing out a record to the *.WAT file for year 35.0,
C                       average conc = 0.0, if all the average concentrations
C                       for a given constituent are zero.
C
C==== SUBROUTINE CALL ========================================================
C 
      SUBROUTINE COUPLE (UNITS,VI,INDXWU,NDS,CO,MSOURC,QOLD) 
C 
C==== COMMON Block Definitions ===============================================
C
      INCLUDE 'MAXIMUM.WTN'
      INCLUDE 'INDEX1.WTN'
      INCLUDE 'INDEX3.WTN'
      INCLUDE 'INDEX5.WTN'
      INCLUDE 'INDEX6.WTN'
      INCLUDE 'COM21.WTN'
      INCLUDE 'CTIME.WTN'
      INCLUDE 'LIST.WTN'
      INCLUDE 'INTER.WTN'

C
C==== DIMENSION Statements ===================================================
C
      DIMENSION VI(MAXCON),NDS(MAXCON),PNT1(MAXNTI),CO(MAXCON)
C
C==== Variable Declaration ===================================================
C
C     None
C
C==== DATA STATEMENT =========================================================
C
C     None
C
      IJK=-88
      INDEX=140
      DT=70./FLOAT(INDEX)
C*****************************************************************************
C     MTOT IS THE TOTAL NUMBER OF PARENT AND DAUGHTER PRODUCTS  JWB 3-24-1989 
C     M3 IS THE NUMBER OF CONSTITUENTS AND DAUGHTERS (M3 = M1 + DAUGHTERS)    
C*****************************************************************************
      MTOT = 0
      DO M1=1,NUMCON
        MTOT = MTOT + NDS(M1)
        NUM(M1) = NTIMES + 1
      enddo  
      IF (NTIMSPEC.EQ.0) THEN
        DO M1=1,NUMCON
          Do III=1,NUM(M1)
c
c pass time=time+travel instead of time and travel  JLS 12-11-92
c         TIME(III,M1) = TIME(III,M1) + TRAVEL(M1) - TDIFF(M1)
c TDIFF is the difference between when the waste unit started and the time
c at which risk calculations started (commonly present time).
c
            TIME(III,M1) = TIME(III,M1) - TDIFF(M1)
            IF( TIME(III,M1) .GE. TFINAL ) GOTO 399
          end do
C*****************************************************************************
C     Stay within first Loop after passing TFINAL to set TIME for rest of NUMCON
C                                   JWB - 11/24/92                            
C*****************************************************************************
399       continue
        end do
      ENDIF
      M3=0
      WRITE(7,692) INDXWU,LOCNUM,LOCMED
692   FORMAT (2X,3I5)
C*****************************************************************************
C     LOGICAL UNIT 7 IS USED TO TRANSFER INFORMATION TO THE EXPOSURE
C     ASSESSMENT COMPONENT (*.WAT FILE).  JWB 3-24-1989
C*****************************************************************************
      WRITE(7,692) MTOT
      DO M1=1,NUMCON
C*****************************************************************************
C     TEMPORARILY SAVES THE PARENT IN PNT1 SO DAUGHTER CONCENTRATIONS CAN BE  
C     OBTAINED FROM CS, PRINTED AND THEN SENT TO *.WAT FILE.  JWB 3-24-1989   
C*****************************************************************************
        IF(NDS(M1).GT.1 .AND. NTIMSPEC.EQ.0) THEN
          DO NUMI=1,NUM(M1)
            PNT1(NUMI) = PNT(NUMI,M1)
          end do
        ENDIF
        DO MM=1,NDS(M1)
          JJFLAG = 0
          M3 = M3 + 1
          IF(MM.GT.1 .AND. NTIMSPEC.EQ.0) THEN
            DO NUMI=1,NUM(M1)
              PNT(NUMI,M1) = CS(NUMI,M3)
            end do
          ENDIF
          IF (NTIMSPEC.EQ.0) THEN
            DDT=TIME(2,M1)-TIME(1,M1)
            TSTRT1(M1)=0.
            TEND1(M1)=TIME(NUM(M1),M1)
            IF (TEND1(M1) .LT. 0.) TEND1(M1)=0.
            IF (TSTRT1(M1) .GT. TFINAL) TSTRT1(M1)=TFINAL+1.
            IF (TEND1(M1) .GT. TFINAL) TEND2(M1)=TFINAL+2.
            VI(M1)=(TEND1(M1)-TSTRT1(M1)+1.)/70.+1
10          IMULR(M1) = VI(M1)
            IJ=0
            IK=0
            III=VI(M1)
C*****************************************************************************
C     CSMAX IS THE MAXIMUM CONCENTRATION FOR A CONSTITUENT.  JWB 1-26-1989    
C     DO LOOP 557 modified to loop for NTIMES+1, not III     JWB 8-10-1992    
C*****************************************************************************
            CSMAX = 0.0
            TIMAX = 0.0
            DO IX=1,ntimes+1
              IF( CSMAX .LT. CS(IX,M3) ) CSMAX = CS(IX,M3)
              IF( TIMAX .LT. TIME(IX,M1) ) TIMAX = TIME(IX,M1)          
            end do
          ELSE
            III = NTIMSPEC
          ENDIF
          DO I=1,III
            IF (NTIMSPEC.EQ.0) THEN
              TAVG = TSTRT1(M1) + FLOAT(I-1) * 70.0 + 35.0
              T3 = TAVG
              if( msourc .gt. 0 ) then
                CAVG = (co(m1)/UNITS) / (QNEW/QOLD)
              else
C*****************************************************************************
c JLS call new function to get the 70yr average
c (the function is passed the starting time of the 70 year period and
c the constituent number, M1)
C*****************************************************************************
C     Pass the start time of the PNT array if this is the first interval,
C     else pass the start time for the interval.  See note above in the
C     modification description section of the QA header.  JPM 06/15/94.
C*****************************************************************************
                IF (I.EQ.1) THEN
                  W1 = AVG70Y(TSTRT1(M1),M1)
                ELSE
                  W1 = AVG70Y(T3 - 35.0, M1)
                ENDIF
c
c Add TDELTA so that we get the proper time discounting JLS 
c TDELTA is the difference between  the current time and the time to start
c discounting from (ie posative for discounting times before present)
                tAVG = tAVG + TDELTA(M1)
                IF( NPRINT .gt. 2 ) then
                  WRITE(6,999) I,M1,DDT,DT,T3,TIME(I,M1),PNT(I,M1),
     1                       TRAVEL(M1)
999               FORMAT (2I5,1P,6E10.2)
                endif
                CAVG = W1
              endif
            ELSE
              TAVG = TIMSPEC(I)
              CAVG = CS_SPEC(I,M3)
            ENDIF
C*****************************************************************************
C     CHECK FOR MAXIMUM CONCENTRATION, IF CSMAX IS <OR= TO 0.0, A WARNING     
C     MESSAGE IS WRITTEN OUT CONCENTRATIONS WILL BE PRINTED OUT. JWB 1-26-1989
C*****************************************************************************
            IF (CAVG.GT.0.0.OR.JJFLAG.EQ.1) THEN
              JJFLAG = 1
              IF (NTIMSPEC.GT.0) THEN
                WRITE (7,994) I,M3,IVS(M3),CASN2(M3),TAVG,CAVG,UNITS
                INDEX2 = I
              ELSE IF( csmax .le. 0.0 .or. TIMAX .LE. 0.0 ) THEN
                WRITE(7,994) I,M3,IVS(M3),CASN2(M3),TAVG,CAVG,UNITS
                WRITE(7,692) IJK
                IMULR(M1)=I
                goto 119
              ELSE IF ( CSMAX .GT. 0.0 .AND. TIMAX .GT. 0.0 ) THEN
                IF (TAVG .gt. tfinal) then 
                  write(7,692) ijk
                  imulr(m1) = i
                  goto 119
                elseif( tavg .gt. 0.0 ) then
                  IF (CAVG .GT. 0.) then
                    ik = 1
                    WRITE(7,994) I,M3,IVS(M3),CASN2(M3),TAVG,CAVG,UNITS
                    INDEX2=I
                  else
                    if( ik .gt. 0 ) then
                      ij = ij + 1
                      if( ij .gt. 3 ) then
                        WRITE(7,994) I,M3,IVS(M3),CASN2(M3),TAVG,CAVG,
     1                               UNITS
                        WRITE(7,692) IJK
                        IMULR(M1)=I
                        ij = 0
                        ik = 0
                        GO TO 119
994                     FORMAT (2X,2I5,2X,A18,2X,A18,2X,1P3E10.3)
                      else
                        WRITE(7,994) I,M3,IVS(M3),CASN2(M3),TAVG,CAVG,
     1                               UNITS
                        INDEX2=I
                      endif
                    else
                      WRITE(7,994) I,M3,IVS(M3),CASN2(M3),TAVG,CAVG,
     1                             UNITS
                      INDEX2=I
                    endif
                    IF(I.EQ.III.AND.MM.GT.1.AND.MM.EQ.NDS(M1) ) THEN
                      DO NUMI=1,NUM(M1)
                        PNT(NUMI,M1) = PNT1(NUMI)
                      enddo
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF 
            ENDIF
          ENDDO
C
C***********************************************************************
C     If a 70-yr interval with an average concentration greater than
C     zero was NOT found (i.e., JJFLAG = 0), then write out one record
C     for the current constituent with time = 35.0yrs, and conc = 0.0.
C     JPM  12/22/94
C***********************************************************************
C 
          IF (JJFLAG.EQ.0) WRITE (7,994) 1,M3,IVS(M3),CASN2(M3),35.0,
     1                                   0.0,UNITS
          WRITE (7,692) IJK
          IMULR(M1)=INDEX2
119     ENDDO  
      ENDDO  
      RETURN
      END
