C   EPFNUM.FOR INTAKE               Version Date: 09-Jun-97               
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE EPFNUM                                *
C                                                                            *
C  Subroutine EPFNUM reads data set heading record with number of media      *
C                                                                            *
C                                                                            *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    07-Jan-97                                               *
C  Last Modified:    09-Jun-97      DLS                                      *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: EDUP/INTAKE
C     Called by: INTAKE
C     Calls: NONE  
C     Common blocks referenced: NONE
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C     NUMEXP     S      Int    Number of media included in this data set
C     RELTYPE    S    Char*14  Release type (acute or chronic)
C==== Modification History ===================================================
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C   09-Jun-97      DLS  Initial programming from CHRONIC EPFDSET
C    26-OCT-98      BAN  MOVE ACUTE/CHRONIC DETERMINATION TO EPFRCP
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE EPFNUM(NUMEXP)
C
C---- Include statements -----------------------------------------------------
c
      INCLUDE 'DEVICE.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
c      LOGICAL ACUTE
      INTEGER NUMEXP
c      CHARACTER*14 RELTYPE
      CHARACTER*80 TEXT
C
C---- Data Statements --------------------------------------------------------
C
C     NONE
C
C---- Read text heading information ------------------------------------------
C
      READ(NEPF,*) NTEXT
      DO IT = 1,NTEXT
         READ(NEPF,*) TEXT
         WRITE(NRLS,*) TEXT
      ENDDO
C
C---- Read first line of data set: number of data sets and chronic ----------
C
C      READ(NEPF,*) NUMEXP,RELTYPE      
	READ(NEPF,*) NUMEXP
C  ALL OF THIS STUFF IS USELESS HERE!
C---- Determine exposure type ------------------------------------------------
C
C      ACUTE = .FALSE.
C      IF(RELTYPE.EQ."acute") THEN
C        WRITE(NRLS,100) NUMEXP,RELTYPE
C 100    FORMAT(' There is (are)',I3,' data set(s) for type ',a7)
C        ACUTE = .TRUE.
C      ELSEIF(RELTYPE.EQ."chronic") THEN
C        WRITE(NRLS,100) NUMEXP,RELTYPE                          
C      ELSE
C        WRITE(NERR,1000) RELTYPE
C 1000   FORMAT(' Exposure type not recognized for EPF data set:',a14)
C        NERROR = NERROR + 1
C      ENDIF
C
      RETURN
C
C----- END OF MODULE EPFNUM -------------------------------------------------
C
      END

