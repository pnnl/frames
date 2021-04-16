C    GETSET.FOR                      Version Date: 28-Oct-96               
C   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE GETSET                                *
C                                                                            *
C  Subroutine GETSET reads a set of lines from the GID file                  *
C                                                                            *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    28-Oct-96                                               *
C  Last Modified:    28-Oct-96      DLS                                      *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: GENERAL USE
C     Called by: ...
C     Calls: NONE  
C     Common blocks referenced: NONE
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C     NGID       U      INT    Argument  Logical unit of the .GID file    
C     NERR       U      INT    Argument  Logical unit for writing error messages
C     ***THE FILES MUST ALREADY BE OPEN***
C     NLINES     U      INT    Argument  Number of lines to read 
C     SETDATA    S      CHR    GID file  Storage array for GID data set 
C==== Modification History ===================================================
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
c
C==== SUBROUTINE CALL ========================================================
C
       SUBROUTINE GETSET(NGID,NERR,NAME,NLINES,SETDATA,NSITE,FILTER)
C      
C---- Variable Type Declarations ---------------------------------------------
C
      PARAMETER (LINEMAX=10000)
      LOGICAL MOVESET, FILTER
      INTEGER NGID,NLINES,NERR,NGOOD
      CHARACTER*120 SETDATA(linemax),SETLINE
      CHARACTER*14 NAME,PNAM
C
C---- Data Statements --------------------------------------------------------
C
C
C---- Start of Analysis
C    MOVESET finds the location of the first record on the set, and the 
C    number of lines to be read (NLINES)
C
      IF (MOVESET(NGID,NAME,NLINES)) THEN
C      
C----- Set name was found in the .GID file.  Read a data set ---------------
C
        IF(NLINES.LE.0.OR.NLINES.GT.linemax) THEN   ! Test NLINES for range
          WRITE(NERR,100) NLINES
 100      FORMAT(' Error in GID specification of NLINES: ',I5)
        ELSE                            ! Read data set
          NGOOD = 0
          DO ILN = 1,NLINES
            IF(FILTER) THEN
              READ(NGID,'(A80)',ERR=300,END=400) SETLINE
              READ(SETLINE,*) PNAM,ISITE
                IF(ISITE.EQ.NSITE) THEN
                  NGOOD = NGOOD + 1
                  SETDATA(NGOOD) = SETLINE
                ENDIF
            ELSE 
              READ(NGID,'(A80)',ERR=300,END=400) SETDATA(ILN)
            ENDIF
          END DO
          IF(FILTER) NLINES = NGOOD
        ENDIF
      ELSE
        WRITE(NERR,105) NAME
 105    FORMAT(' Error in GID read.  Set name not found. ',A14)
      ENDIF
      PNAM = PNAM
      RETURN
 300  WRITE(NERR,101)ILN
 101  FORMAT(' Error in GID set read at line ILN: ',I5)
      RETURN
 400  WRITE(NERR,102) ILN, NLINES
 102  FORMAT(' Error in GID set read. EOF after reading ',I4,' of',I4,
     .       ' lines')
      RETURN
C----- END OF MODULE GETSET -------------------------------------------------
      END

