C------------------------------------- Version:  08-Sep-97 -------------------
C
C      SUBROUTINE CANDH 
C
C     This module calculates environmental concentrations of C-14 and
C     H-3 using simplified specific-activity models for acute and chronic
C     exposures. 
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System
C
C     Initial GENII Version:  17-Apr-90 RAP
C     Reviewed and Approved:  3-Apr-90  BA Napier
C     Last Modification:  08-Sep-97  DL Strenge
C
C-----------------------------------------------------------------------------
C
C     AIRBOX()  - air concentration for each radionuclide in the 
C                 current chain, set based on whether acute or chronic
C                 exposure 
C     ANFLAG    - option flag for each of the feed and fodder types
C     ANMFR     - fraction of animal diet for the current animal feed 
C                 type, set based on whether acute or chronic exposure 
C     DW1()     - Intake via drinking water, used for tritium
C     DW2()     - Concentration in drinking water, used for tritium 
C     EDBL()    - Concentration in animals from the six feed categories,
C                 used for carbon
C     EDBL1()   - Intake via food, used for tritium
C     EDBL2()   - Concentration in food, used for tritium
C     LEAF()    - Total leaf concentration, each radionuclide, UoA/kg
C     LEAF1a    - Leaf concentration from air, UoA/kg 
C     LEAF2     - Leaf concentration from irrigation, UoA/kg
C     WATCON()  - Water concentration used for each radionuclide in the
C                 current chain, either ground water or surface water
C
C     Assumptions:
C
C     1) Animals drink irrigation water if irrigation is considered,
C        otherwise animals assumed to drink surface water
C
C---- Modification History ---------------------------------------------------
C    Date     Who  Modification Summary
C  ---------  ---  -----------------------------------------------------------
C  08-Sep-97  DLS  Initial modification of old GENII
C  08-Sep-97  DLS  All pathwayw disappear when there is no air or water
C                  contamination.  Subroutine sets results to zero for C and H.
C-----------------------------------------------------------------------------
C    Subroutine Call Statement
C
      SUBROUTINE CANDH 
C
C---- Include Statements -----------------------------------------------------
C
      INCLUDE 'CONC.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'RAD.CMN'
C
C---- Type/dimension Statements ----------------------------------------------
C
C
C---- Data Statements --------------------------------------------------------
C
C
C---- Start of Analysis ------------------------------------------------------
C
C   For NEAR-FIELD analysis, all C14 and H3 results are zero: no soil to plant
C   transport.
C
C-----      Calculate human uptake from crops --------------------------------
C
         DO ITF = 1,NTF
            DO IN = 1, NONUC
              EXPOS(IN,3+ITF) = 0.0
            END DO  
         END DO
C
C-----      Human uptake from contaminated animal products -------------------
C
         DO IAN = 1,NAN
            DO IN = 1, NONUC
              EXPOS(IN,7+IAN) = 0.0
            END DO
         END DO
C
      RETURN
      END
C
C---- End of Module CANDH ----------------------------------------------------
C
