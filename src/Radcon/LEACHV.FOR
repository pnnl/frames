C     MEPAS RADCON: LEACHV.FOR            Version Date: 11-17-1995
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.
C*****************************************************************************
C                                                                            *
C                    SUBROUTINE LEACHV                                       *
C                                                                            *
C  Subroutine LEACHV calculates the leaching rate (velocity) from            *
C  a source.  If the leaching rate is not known and is not entered as        *
C  an input parameter, it is calculated in subroutine LEACH1 (FPSUM).        *
C  Subroutine LEACHV is called from subroutine SOURC.                        *
C                                                                            *
C  Written by:      Gene Whelan                                              *
C                   Battelle Pacific Northwest Laboratories                  *
C                   P.O. Box 999                                             *
C                   Richland, WA  99352                                      *
C                                                                            *
C  Creation Date:   01/19/89 (Converted to PC)                               *
C  Last Modifed:    11/17/95 - JPM                                           *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C    Module of:  RADCONPC
C    Called by:  SUBROUTINE SOURC
C    Calls to:  SUBROUTINE LEACH1, PNTTIIM, S_TERM
C    Common blocks referenced: INTER, INIT, VLEA1, CONC1, INDEX1, INDEX2,
C                              INDEX3, EXTFLUX, isv, ERROR, MAXIMUM, TECH,
C
C==== Significant Parameter Designation and Description ======================
C
C    Parameter Set/           Location
C    Name      Used  Type     Export/Import  Parameter Description
C    --------- ----- -------  -------------- ----------------------
C     AL1      USED REAL      COMMON - IMP   LENGTH OF OVERLAND FLOW SEGMENT
C                                            ACTING AS A SOURCE TO A RIVER
C     AN5      SET  REAL      ARG - EXPORT   SOURCE TERM MOISTURE CONTENT
C     FPSUM    USED REAL      COMMON - IMP   TOTAL INFILTRATION RATE, IF SITE
C                                            IS PONDED, FPSUM IS TOTAL YEARLY
C                                            INFILTRATION RATE.  IF NONPONDED,
C                                            FPSUM IS BASED ON EVAPOTRANSIR-
C                                            ATION AND SOIL MOISTURE CONDITION
C     IHW      SET  INTEGER   COMMON - EXP   INDEX IF WASTE UNIT PENETRATES
C                                            THE WATER TABLE SURFACE:
C                                              = 0; DOES NOT PENETRATE WATER
C                                                   TABLE,(HW MUST BE = 0)
C                                              = 1; DOES PENETRATE WATERTABLE,
C                                                   (HW MUST BE > 0)
C     IPOND    USED INTEGER   COMMON - IMP   INDEX ON LIQUID IMPOUNDMENT AS
C                                            SOURCE TERM:
C                                              = 1; PONDED SITE               
C                                              = 2; NOT PONDED SITE           
C     R2       SET  REAL      ARG - EXPORT   SOURCE TERM BULK DENSITY         
C     VLEACH   SET  REAL      ARG - EXPORT   INFILTRATION RATE; CAN BE KNOWN  
C                                            INFILTRATION RATE FROM A PONDED   
C                                            SITE, KNOWN INFILTRATION RATE    
C                                            FROM A LANDFILL OR CALCULATED    
C                                            RATE BASE ON SOIL MOISTURE AND   
C                                            EVAPOTRANSPIRATION               
C                                            CONDITIONS                       
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/25/91     JWB  Added required comments and header for MEPAS QA plan
C     11/14/92     JWB  Total rewite of subroutine by G Whelan
C     02/08/93     JWB  Added include statement EXTFLUX and modified code to
C                       call S_TERM to read in external fluxes if the file
C                       (*.MAS) exists or compute fluxes based on ISS using
C                       *.SSS file.
C     02/28/93     KJC  The variable ANALYSIS is passed through from tech.wtn
C     05/26/93     JLS  Removed goto so that AN5 and R2 will be read in
C     01/27/94     KJC  Added lines so no stop is used ERRORSIG is set instead
C     04/07/94     JPM  Uncommented the statements that read in IHW, VLEACH
C                       and calls PNTTIM to read in fluxes from the *.WIN
C                       file.  Set up a conditional such that either these
C                       statements are executed, or S_TERM is called,
C                       using the variables EXT_FLUX and ISSSFL.
C     04/20/94     JPM  Removed commented out write statements so AN5 and
C                       R2 are written under the same conditions in which
C                       they are read.  Commented the write statement under
C                       the ANALYSIS.EQ.0 conditional, so AN5 and R2 are not
C                       written twice.
C     07/28/95     JPM  Writing error messages to the screen and the *.WRN
C                       file in addition to the *.WLS file.
C     11/17/95     JPM  Allowing for the ISCONF = 5 condition when reading
C                       in the source term moisture content and bulk density.
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE LEACHV (ILEACH,VLEACH,BLEN,BWIDTH,TLIFE,SEDFLX,CO,AN5,
     1                   R2,UNITS,B1,CLEN,III)
C
C==== COMMON Block Definitions ===============================================
C
CKJC  Added for dll compilation
      INCLUDE 'MAXIMUM.WTN'
      INCLUDE 'ERROR.WTN'
      INCLUDE 'INDEX1.WTN'
      INCLUDE 'INDEX2.WTN'
      INCLUDE 'INDEX3.WTN'
      INCLUDE 'CONC1.WTN'
      INCLUDE 'INIT.WTN'
      INCLUDE 'INTER.WTN'
      INCLUDE 'VLEA1.WTN'
      INCLUDE 'extflux.WTN'
      include 'tech.wtn'
      include 'isv.wtn'
C
C==== DIMENSION Statements ===================================================
C
      DIMENSION CO(MAXCON),TLIFE(MAXCON)
C
C==== Variable Declarations ==================================================
C
C     None
      CHARACTER W_NAME*15
C
C==== DATA Statements ========================================================
C
C     None
C
C*****************************************************************************
C ILEACH=1 STORES THE AVERAGE-ANNUAL OVERLAND FLOW VOLUME IN PNT FOR
C          ISOURC=1 AND ISTYPE>2; STORES THE AVERAGE-ANNUAL LANDFILL
C          INFILTRATION RATE TO GROUNDWATER IN PNT FOR ISOURC=1 AND ISTYPE<3.
C
C ILEACH=2 STORES KNOWN SOURCE-TERM MASS FLUXES IN PNT FOR ISOURC=2 AND
C          ISOURC=3.
C*****************************************************************************
C
C*****************************************************************************
C CHECK TO SEE IF THE SCENARIO IS ACCEPTABLE FOR INPUTING DATA
C*****************************************************************************
c JLS removed check on IHW since it is not read in at this point
CKJC      IF( ISTYPE.EQ.2 .AND. ISOURC.EQ.2 .AND. IPOND.EQ.2 .AND.
CKJC     1    ISCONF.LT.4 ) THEN
CKJC       WRITE(6,5) ISTYPE,ISOURC,IPOND,ISCONF,IHW
CKJC        STOP ' Error 1 Detected in SUROUTINE LEACHV'
CKJC      ENDIF
      IF( ISTYPE.EQ.2 .AND. ISOURC.EQ.3 .AND. IPOND.EQ.2 ) THEN
        IF( ISCONF.LT.4 ) THEN
           WRITE(6,5) ISTYPE,ISOURC,IPOND,ISCONF,IHW
           WRITE(11,5) ISTYPE,ISOURC,IPOND,ISCONF,IHW
           CALL MESSAGE ()
CKJC  Added for dll compilation
           ERRORSIG=.TRUE.
           goto 9999
CKJC           STOP' Error 2 Detected in SUROUTINE LEACHV'
        ENDIF
        IF( ISCONF.EQ.4 .AND. IHW.EQ.1 ) THEN
          WRITE(6,5) ISTYPE,ISOURC,IPOND,ISCONF,IHW
          WRITE(11,5) ISTYPE,ISOURC,IPOND,ISCONF,IHW
          CALL MESSAGE ()
CKJC  Added for dll compilation
           ERRORSIG=.TRUE.
           goto 9999
CKJC          STOP' Error 3 Detected in SUROUTINE LEACHV'
        ENDIF
      ENDIF
C*****************************************************************************
C   varaible ANALYSIS indicates the type of analysis is being conducted
C                      ANALYSIS = 0 ; baseline
C                      ANALYSIS = 1 ; remediation
C                      ANALYSIS = 2 ; residual
C                      ANALYSIS = 3 ; failure
C                      ANALYSIS = 4 ; other
C*****************************************************************************
C*******  remediation case  *************
        if( analysis.eq.1 .AND. tech_type .eq. 47 ) then
          READ(5,149) AN5,R2,glass_lr,waste_vol
149       format(4e10.3)
          WRITE(6,152) AN5,R2,glass_lr,waste_vol,tech_type
152   FORMAT (/,
     1'Source-Term Moisture Content (AN5) = ',F6.2,' %',/,
     2'Source-Term Bulk Density (R2)      = ',1p,E10.2,' g/cm3',/,
     3'Glass Leach Rate (GLASS_LR)        = ',1p,E10.2,' g/cm2/yr',/,
     4'Volume of Waste (WVOLUME)          = ',1p,E10.2,' cm3',//,
     5'Technology types (TECH_TYPE)       = ',i3,/)
          read(5,153) (raas_cs(nn), nn=1,numcon)
153   format(8e10.3)
          write(6,154)
154   format(/,'RAAS Provided Soil Concentrations from ISV',/)
          do ijk = 1, numcon
            write(6,155) ijk, raas_cs(ijk)
155         format(5x,i3,5x,1p,e10.3)
          end do
        else
C*****************************************************************************
C READS IN MOISTURE CONTENT (AN5) AND BULK DENSITY (R2) OF SOURCE TERM
C IF IPOND = 2 (NOT POND)
C*****************************************************************************
          IF( IPOND.EQ.2 ) then
            IF( ISTYPE.EQ.1 ) THEN
              IF( ISOURC.EQ.1 .AND. ISCONF.EQ.4 ) THEN
                READ(5,147) AN5,R2
                WRITE(6,111) AN5,R2
              ENDIF
              IF( ISOURC.GE.2 .AND. ISCONF.LT.4 ) THEN
                READ(5,147) AN5,R2
                WRITE(6,111) AN5,R2
              ENDIF
            elseIF( ISTYPE.EQ.2 ) THEN
              IF( ISOURC.LE.2 .AND. ISCONF.GE.4 ) THEN
                READ(5,147) AN5,R2
                WRITE(6,111) AN5,R2
               ENDIF
              IF( ISOURC.EQ.2 .AND. ISCONF.LT.4 ) THEN
                READ(5,147) AN5,R2
                WRITE(6,111) AN5,R2
               ENDIF
            elseIF( ISTYPE.GE.3 .AND. ISOURC.EQ.1 ) THEN
              READ(5,147) AN5,R2
              WRITE(6,111) AN5,R2
            ENDIF
CKJC fix for designated pond
          elseif (ipond.eq.1 )then
            if (istype.le.2 .and. isourc.eq.1 .and. isconf.eq.4) then
              READ(5,147) AN5,R2
              WRITE(6,111) AN5,R2
            endif
          ENDIF
          if (analysis .eq. 0) then
CJPM            WRITE(6,111) AN5,R2
111   FORMAT (/,
     1'Source-Term Moisture Content (AN5) = ',F6.2,' %',/,
     2'Source-Term Bulk Density (R2)      = ',1p,E10.2,' g/cm**3')
          else
            WRITE(6,112) AN5,R2,tech_type
112   FORMAT (/,
     1'Source-Term Moisture Content (AN5) = ',F6.2,' %',/,
     2'Source-Term Bulk Density (R2)      = ',1p,E10.2,' g/cm**3',/,
     3'Technology types (TECH_TYPE)       = ',i3,/)
          endif
        endif
      AN5=AN5/100.
C*****************************************************************************
C COMPUTER-CALCULATED SOURCE-TERM COMPUTATIONS (ISOURC=1)
C*****************************************************************************
100   IF( ISOURC.EQ.1 ) THEN
        IF( IPOND.EQ.2 ) THEN
          CALL LEACH1( FPSUM,BLEN,BWIDTH,VLECH1,SEDFLX)
        endif
        ILEACH=1
      IF( ISTYPE.LE.2 .AND. ISOURC.EQ.1 .AND. IPOND.EQ.1 .AND.
     1    ISCONF.EQ.4 ) THEN
        READ(5,147) VLEACH
        WRITE(6,201) VLEACH
        FPSUM=VLEACH*365.25
      ENDIF
        CALL PNTTIM( FPSUM,TLIFE,ILEACH,CO,AN5,R2,SEDFLX,BLEN,BWIDTH,
     1               UNITS,AL1,B1,CLEN,III)
CKJC  Added for dll compilation
      IF (ERRORSIG) goto 9999
C*****************************************************************************
C USER-SUPPLIED SOURCE-TERM CALCULATIONS (ISOURC=2 OR 3)
C
C     S_TERM is called when either EXT_FLUX or ISSSFL is equal to zero.
C     Thus, if both of the variables are nonzero and this is a known flux
C     case (ISOURC = 2), then call PNTTIM with ILEACH=2 so fluxes are read
C     from the *.WIN file.  JPM  04/07/94
C
C*****************************************************************************
C
      else IF (ISOURC.GE.2) THEN
        READ(5,1) IHW
        IF ((EXT_FLUX.NE.0).AND.(ISSSFL.NE.0)) THEN
           READ(5,2) (NUM(I), I=1,NUMCON)
           WRITE(6,3)
           DO I=1,NUMCON
             WRITE(6,4) I,IV(I,1),NUM(I)
           ENDDO
           DO I=1,NUMCON
             IF( NUM(I).LT.2 ) then
               WRITE(6,8)
               WRITE(11,8)
               CALL MESSAGE ()
CJPM  Added for dll compilation
               ERRORSIG=.TRUE.
               goto 9999
CJPM               STOP' Error 4 Detected in SUROUTINE LEACHV'
             ENDIF
           ENDDO
        ENDIF
        ILEACH=2
        IF ((EXT_FLUX.NE.0).AND.(ISSSFL.NE.0)) THEN
           CALL PNTTIM( FPSUM,TLIFE,ILEACH,CO,AN5,R2,SEDFLX,BLEN,BWIDTH,
     1                  UNITS,AL1,B1,CLEN,III)
C*****************************************************************************
C READS IN DARCIAN INFILTRATION RATE (VLEACH)
C*****************************************************************************
           IF( ISTYPE.EQ.1 .AND. ISOURC.EQ.2 ) THEN
             READ(5,147) VLEACH
             WRITE(6,201) VLEACH
           ENDIF
           IF( ISTYPE.EQ.1 .AND. ISOURC.EQ.3 .AND. IPOND.EQ.2 ) THEN
             READ(5,147) VLEACH
             WRITE(6,201) VLEACH
           ENDIF
           IF( ISTYPE.EQ.2 .AND. ISOURC.EQ.3 .AND. IPOND.EQ.2 .AND.
     1         ISCONF.EQ.4 .AND. IHW.EQ.0 ) THEN
             READ(5,147) VLEACH
             WRITE(6,201) VLEACH
           ENDIF
        ENDIF
C*****************************************************************************
        IF( ISTYPE.EQ.1 .AND. ISOURC.EQ.3 .AND. IPOND.EQ.2) CALL LEACH1(
     1      FPSUM,BLEN,BWIDTH,VLECH1,SEDFLX)
        IF( ISTYPE.EQ.2 .AND. ISOURC.EQ.3 .AND. IPOND.EQ.2 .AND.
     1      IHW.EQ.0 ) CALL LEACH1( FPSUM,BLEN,BWIDTH,VLECH1,SEDFLX)
        IF( ISTYPE.EQ.5 .AND. ISOURC.EQ.2 ) THEN
          CALL LEACH1 (FPSUM,BLEN,BWIDTH,VLECH1,SEDFLX)
          ILEACH=1
          CALL PNTTIM( FPSUM,TLIFE,ILEACH,CO,AN5,R2,SEDFLX,BLEN,BWIDTH,
     1                 UNITS,AL1,B,CLEN,III)
CKJC  Added for dll compilation
          if (ERRORSIG) goto 9999
        ENDIF
      ENDIF
C*****************************************************************************
C     IF( ISTYPE.EQ.1 .AND. ISOURC.EQ.2 .AND. IPOND.EQ.2 .AND.
C    1    ISCONF.EQ.4 ) THEN
C       READ(5,147) VLEACH
C       WRITE(6,201) VLEACH
C     ENDIF
C*****************************************************************************
C READ THE LENGTH OF THE RIVER BANK THAT CONTAMINANTS CROSS DUE TO OVERLAND
C RUNOFF (RIVLEN)
C*****************************************************************************
      IF( ISTYPE.EQ.3 .OR. ISTYPE.EQ.5 ) THEN
        READ(5,147) AL1
        IF (AL1 .EQ. 0) AL1=1.
        WRITE(6,203) AL1
      ENDIF
C*****************************************************************************
C   If ext_flux=0 then Mass Balance input file exists (*.MAS)
C                          JWB - 2/7/93
C*****************************************************************************
      if( ext_flux.eq.0 .or. isssfl.eq.0 )
     1	  call s_term(istype,vleach,W_NAME,TLIFE)
C*****************************************************************************
C FORMAT STATEMENTS
C*****************************************************************************
1     FORMAT (6I5,2E10.3)
2     FORMAT (16I5)
3     FORMAT (/'  #',T15,'Constituent',T35,'NUM'/)
4     FORMAT (I5,T16,A18,T32,I5)
5     FORMAT (
     C /2x,'ERROR --> This scenario is unacceptable.  Values for',
     1 /2x,'the parameters are as follows:',/T10,'ISTYPE =',T20,I2,/,
     2        T10,'ISOURC =',T20,I2,/,T10,'IPOND =',T20,I2,/,
     3        T10,'ISCONF =',T20,I2,/,T10,'IHW =',T20,I2)
8     FORMAT (
     C /2x,'ERROR --> NUM cannot be less than 2')
147   FORMAT (8E10.3)
201   FORMAT (/'Known Darcian Infiltration Rate from Site (VLEACH)',
     1        '   =',1PE10.3,' cm/day',/)
203   FORMAT ('Length of Surface Water Source Term (AL1)',T43,' = ',
     1        1PE10.3, ' cm')
C
 9999 RETURN
      END
