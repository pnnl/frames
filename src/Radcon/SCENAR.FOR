C     MEPAS RADCON: SCENAR.FOR            Version Date: 05-15-1998
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                            SUBROUTINE SCENAR                               *
C                                                                            *
C  Subroutine SCENAR reads the number of transport scenarios (NUMSCE)        *
C  to be addressed per waste unit.  The subroutine is called by the          *
C  main program.                                                             *
C                                                                            *
C  Written by:       Gene Whelan                                             *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    01/19/89 (Converted to PC)                              *
C  Last Modified:    05/15/98 - PDM                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of:  RADCON
C     Called by:  RADCONPC
C     Calls to:  None
C     Common blocks referenced: INDEX3
C
C==== Significant Parameter Designation and Descriptions =====================
C
C     Parameter Set/           Location
C     Name      Used   Type    Export/Import  Parameter Description
C     --------- -----  ------- -------------- --------------------------------
C     NUMSCE    USED   INTEGER  ARG - IMPORT   NUMBER OF TRANSPORT SCENARIOS  
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/26/91     JWB  Added required comments and header for MEPAS QA plan
C     05/15/98     PDM  Reading TREMED from the WIN file if the multiple
C                       infiltration rate option is being used.  Writing an
C                       appropriate message to the WLS file (RAD-E054).
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE SCENAR
C
C==== COMMON Block Definitions ===============================================
C
      INCLUDE 'MAXIMUM.WTN'
      INCLUDE 'INDEX3.WTN'
      INCLUDE 'CAP.WTN'
C
C==== DIMENSION Statements ===================================================
C
C     None
C
C==== Variable Declarations ==================================================
C
C     None
C
C==== DATA Statements ========================================================
C
C     None
C
cpdm modified read, to obtain remediation data
c
      READ(5,'(I5,8e10.3)') NUMSCE, (TREMED(I),I=1,NUMITR-1)
cpdm      READ(5,1) NUMSCE
1     FORMAT (I5)
      IF (NUMSCE .LE. 0) NUMSCE=1
c
      if (numitr.gt.1) then
        write(6,999)numitr,(tremed(i),i=1,numitr-1)
      endif
 999  format(/,' Multiple infiltration rate option being used.',
     &       /,1x,i5,' infiltration rates beginning at years',
     &       /,' 0.0 ',1P,8e10.3)
      RETURN
      END

