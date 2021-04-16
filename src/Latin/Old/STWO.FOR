      SUBROUTINE STWO
C*....SUBROUTINE STWO
C***********************************************************************
C*
C*  Purpose:
C*
C*    This subroutine writes an opening and closing screen to the
C*    terminal.
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Version 1.0
C*    Paul W. Eslinger : 23 Oct 1992 : Version 1.11
C*      Add copyright notice
C*    Paul W. Eslinger :  3 Feb 1993 : Version 1.11.1
C*      Modify copyright notice
C*
C***********************************************************************
C
      INCLUDE 'SPINATTR.ins'
      INCLUDE 'SCREENS.ins'
      INCLUDE 'IDENC.ins'
C
C *** Clear the screen
C
      CALL CLS( 0,0, 24,79 )
C
C *** Closing screen
C
      IWFIN = 9998
      IR1 =  0
      IR2 = 13
      IC1 =  0
      IC2 = 79
      CALL DEFWIND(IWFIN, IR1, IC1, IR2, IC2, DBLBDR, ICDEF )
      CALL CLRB(IWFIN)
C
      CALL BLDB(IWFIN,1,10,
     * 'Multimedia Environmental Pollutant Assessment System (MEPAS)',
     * ICDEF )
      CALL BLDB(IWFIN,2,10,
     * '     Pacific Northwest Laboratory, Richland, Washington',
     * ICDEF )
      CALL BLDB(IWFIN,3,10,
     * '             LATIN Hypercube Sample Generation',
     * ICDEF )
C
      CALL BLDB(IWFIN, 5, 10, PRGNAM,  NORMAL)
      CALL BLDB(IWFIN, 5, 19, 'Version', NORMAL )
      CALL BLDB(IWFIN, 5, 27, PRGVER, NORMAL )
      CALL BLDB(IWFIN, 5, 42, 'Last modified on ', NORMAL )
      CALL BLDB(IWFIN, 5, 59, PRGDAT, NORMAL )
C
      CALL BLDB(IWFIN, 6, 22, 'Run started', NORMAL )
      CALL BLDB(IWFIN, 6, 34, 'at', NORMAL )
      CALL BLDB(IWFIN, 6, 37, SYSTIM, NORMAL )
      CALL BLDB(IWFIN, 6, 46, 'on', NORMAL )
      CALL BLDB(IWFIN, 6, 49, SYSDAT, NORMAL )
C
C *** Copyright notice
C
      CALL BLDB(IWFIN, 8, 25,'    (C) Copyright 1992     ', NORMAL )
      CALL BLDB(IWFIN, 9, 25,'Battelle Memorial Institute', NORMAL )
      CALL BLDB(IWFIN,10, 25,'    All Rights Reserved    ', NORMAL )
C
C *** Put the cursor in the proper place for exit
C
      CALL LOCATEW( 0, 13, 79 )
C
      RETURN
      END
