      REAL FUNCTION CORRUSTR( ustr, z0old, z0new, stab, acute, PrgStat )
C----------------------------------------------------------------------
C
C     CorrUstr.FOR
C
C     Christian J Fosmire
C
C     Pacific Northwest National Lab
C     P.O. Box 999
C     Richland, WA 99352
C
C     Date:          July 19, 1996
c     Updated:       March 14, 2000
C
C     Description:   This routine calculates a new ustar given 
C                    an old sfc roughness (z0old) and a new 
C                    sfc roughness (z0new) and the exisiting
C                    stability
C
C----------------------------------------------------------------------
      IMPLICIT       NONE
      
      REAL           INVMOL
      REAL           PROFILE
      REAL           USTAR
      REAL           fhgt, molen, molnew, molold, ustr, ws100, z0old,
     &               z0new
      
      INTEGER        stab

      CHARACTER*2    prgid      
      CHARACTER*50   PrgStat

      LOGICAL        acute

      prgid = 'CR'
      IF ( acute ) prgid = 'AC'

c  Check if sfc roughness are different
      
      IF( z0old .EQ. z0new ) THEN
         CORRUSTR = ustr 
         RETURN
      ENDIF
      
      IF( (z0old.LE.0.0) .OR. (z0new.LE.0.0) ) THEN
         WRITE ( PrgStat(1:2),'(a2)'  )      prgid
         WRITE ( PrgStat(4:13),'(a10)' )    'CORRUSTR'
         WRITE ( PrgStat(15:18),'(i4)' )     9999
         WRITE ( PrgStat(31:38),'(a8)' )    'z0 <= 0' 
         WRITE ( PrgStat(39:50),'(2f6.3)' )  z0old, z0new
         RETURN
      ENDIF         
      
c  Find old and new inverse Monin-Obh. lengths
      
      molold = INVMOL( stab, z0old )
      molnew = INVMOL( stab, z0new )
               
c     Estimate the winds at free height assumed to be 100 meters
c     or if stable (kst(ihr)>4), minimum of 3*MO length or 100 meters

      IF ( stab.GT.4 ) THEN
         molen = 1.0/molold
         fhgt = 3.0*molen
         fhgt = AMIN1(fhgt,100.)
      ELSE
         fhgt = 100.
      ENDIF               
               
               
      ws100 = PROFILE( z0old,ustr,molold,stab,fhgt,acute,PrgStat )

      IF( PrgStat .NE. ' ' ) RETURN

      CORRUSTR = USTAR( fhgt,ws100,z0new,molnew,stab,acute,PrgStat )

      RETURN

      END            