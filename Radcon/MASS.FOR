C     MEPAS RADCON: MASS.FOR              Version Date: 05-15-1998
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                          SUBROUTINE MASS                                   *
C                                                                            *
C  Subroutine MASS ensures that more mass than which is available at the     *
C  source is not released into the environment.  It is called by subroutine  *
C  INITIN.                                                                   *
C                                                                            *
C  Written by:       Gene Whelan                                             *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA  99352                                     *
C                                                                            *
C  Creation Date:    01/19/89 (Converted to PC)                              *
C  Last Modified:    05/15/98 - PDM                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of:  RADCON
C     Called by:  SUBROUTINE INITIN
C     Calls to: None
C     Common blocks referenced: INTER, INDEX3, MASSS
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/           Location
C     Name      Used   Type    Export/Import  Parameter Description
C     --------- ----- ------  --------------- --------------------------------
C     DT        SET    REAL    LOCAL          TIMESTEP SIZE FOR ADDING UP MASS
C     CMASS2    SET    REAL    LOCAL          CUMMULATIVE MASS VALUE
C     FLUX1     SET    REAL    LOCAL          EQUAL TO FIRST PNT VALUE
C     FLUX2     SET    REAL    LOCAL          INTERPOLATED VALUE
C     INDEX     SET    INTEGER LOCAL          LOCAL COUNTER SET TO IDENTIFY
C                                             FIRST PASS THROUGH LOOP
C     PNT       SET    REAL    COMMON - EXP   INTERPOLATED MASS VALUES
C     TLIFE     SET    REAL    ARG - EXPORT   TIME OF RELEASE INTO ENVIRONMENT
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     04/10/89     JWB  Assure that LTIFE is a minimum of 1 year
C     11/25/91     JWB  Added required comments and header for MEPAS QA plan
C     11/14/92     JWB  Restructured subroutine based on G Whelan comments
C     01/25/93     JWB  Modifications made to reflect CAP changes from JLS
C     01/31/93     JWB  If MUNITR = 1, then GO TO 666 (skip cap loop)
C     04/17/94     JPM  Added new arrays to save the total mass released
C                       and total time at the source for the instantaneous
C                       solution.
C     09/09/94     JPM  Saving the mass released from the source in the
C                       SMASS array.
C     04/21/97     JPM  CINVEN is set to zero for permuted decay products
C                       (i.e., decay products occurring as a parent).  In this
C                       case, this routine will not compare the fluxes to the
C                       inventory, but instead will set the inventory based
C                       on the fluxes (PH1-P008).
C     05/15/98     PDM  Replaced TCAP and TFAIL with TREMED (RAD-E054).
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE MASS (TLIFE)
C
C==== COMMON Block Definitions ===============================================
C
      INCLUDE 'MAXIMUM.WTN'
      INCLUDE 'INDEX3.WTN'
      INCLUDE 'INTER.WTN'
      INCLUDE 'MASSS.WTN'
      INCLUDE 'CAP.WTN'
C
C==== DIMENSION Statements ===================================================
C
      DIMENSION TLIFE(MAXCON)
C
C==== Variable Declarations ==================================================
C
      LOGICAL DPROD
C
C==== DATA Statements ========================================================
C
C     None
C
      DO I=1,NUMCON
        IF (CINVEN(I).LE.0.0) THEN
          DPROD = .TRUE.
        ELSE
          DPROD = .FALSE.
        ENDIF
        INDEX=0
        CMASS1=0.
        FLUX1=PNT(1,I)
        T1=TIME(1,I)
        NI=NUM(I)
        DO J=2,NI
          IF( INDEX.EQ.0 ) THEN
            FLUX2=PNT(J,I)
            T2=TIME(J,I)
            DT=T2-T1
            CMASS2=(FLUX2+FLUX1)*DT/2.+CMASS1
            TOTMAS(I)=CMASS2
            SMASS(I)=TOTMAS(I)
            IF(CMASS2.LT.CINVEN(I).OR.DPROD) THEN
              CMASS1=CMASS2
              FLUX1=FLUX2
              T1=T2
            ELSE
              T=(CINVEN(I)-CMASS1)*DT/(CMASS2-CMASS1)+T1
              CALL INTERP(T,PNT(J,I),I)
              TIME(J,I)=T
              TLIFE(I)=T-TIME(1,I)
              INDEX=1
              TOTMAS(I)=CINVEN(I)
              SMASS(I)=TOTMAS(I)
              DO J2=J+1,NI
                PNT(J2,I)=0.
                TIME(J2,I)=0.
              ENDDO
              NUM(I)=J
            ENDIF
          ENDIF
        ENDDO
        IF (DPROD) THEN
          CINVEN(I) = SMASS(I)
          TLIFE(I) = TIME(NUM(I),I) - TIME(1,I)
        ENDIF
        TLIF(I)=TLIFE(I)
C****************************************************************************
C    I hate GO TOs but it was the easiest change to skip over the CAP changes
C    to the inventory if baseline run is being made.  01/31/93 - JWB         
C****************************************************************************
        if( numitr .eq. 1 ) go to 666
c pdm
        if (incr .eq. 1) then
          cutoff = tremed(1)
        else if (incr .lt. numitr) then
          cutoff = tremed(incr) - tremed(incr-1)
        else
c
c        if (incr .eq. 1) then
c          cutoff = tcap
c        else if (incr .eq. 2) then
c          cutoff = tfail - tcap
c        else
C set cutoff to some value which will fail the conditional (time > cutoff)
          cutoff = time(num(i),i)*1.5
        endif
c pdm

        if((time(num(i),i) .gt. cutoff) .and. (numitr. gt. 1)) then
          time(num(i),i) = cutoff
          tlife(i)  = time(num(i),i) - time(1,i)
          invlos = 0.0
          do k=2,num(i),2
            invlos = invlos + pnt(k,i)*(time(k,i) - time(k-1,i))
          end do
          cinven(i) = cinven(i) - invlos
        else
          cinven(i) = 0.0
        end if
666     continue
      ENDdO
      RETURN
      END
