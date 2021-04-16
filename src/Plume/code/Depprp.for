
      SUBROUTINE DEPPRP( isrc )

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     DEPPRP.FOR
c
c     Christian J Fosmire
c     Pacific Northwest Lab
c     P.O. Box 999
c     Richland, WA 99352
c
c     Created: 5/2/95
c
c     Description:  This subroutine calculates the dry deposition 
c        velocity, the wet gas deposition velocity, and the washout 
c        coefficient for particles.  The settling velocity for 
c        large particles is also calculated.
c
c     Functions:  DDEPVEL, PROFILE, WDEPPART, WDEP_I2
c
c
c     26 Jan 2012  BAN  revised indexing on setvel
c
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      IMPLICIT    NONE                               
                             
      INCLUDE     'parm.inc'                       
      INCLUDE     'depos.inc'
      INCLUDE     'metdata.inc'
      
      REAL        DDEPVEL
      REAL        PROFILE
      REAL        WDEP_I2
      REAL        WDEPPART
      REAL        depspd 
      
      INTEGER     i, isrc
      
      CHARACTER*50   PrgStat

c     Zero out the washout coefficients
      
      wcoef_I2 = 0.0
      wcoef_p = 0.0
      
c     Calculate the speed at the ground for deposition (at 10m )

      depspd = PROFILE( z0, ustr, molinv, stab, 10.0 )

c     Calculate the dry deposition velocity
 
      dv_I2 = DDEPVEL ( ustr, depspd, rtx(1), 0.0 )
      
      DO i = 1, numpardis(isrc)
C revision of 1/26/2012 BAN  Invert indices on setvel (Christian had them wrong)
C         dv_p(i) = DDEPVEL ( ustr, depspd, rtx(2), setvel(i,isrc) )
         dv_p(i) = DDEPVEL ( ustr, depspd, rtx(2), setvel(isrc,i) )
      ENDDO
      
      IF ( (ptyp .GE. 1) .AND. (ptyp .LE. 6)) THEN
         wcoef_I2 = WDEP_I2( ptyp, prate, PrgStat )
         wcoef_p = WDEPPART( ptyp, prate, PrgStat )
      ENDIF

c      WRITE (25, '(a)') ' Subroutine DepPrp  '  
c      WRITE(25,'(a,1pe10.2)' ) 'Dry Deposition Vel for I2 ', dv_I2
c      WRITE(25,'(a,1p,10e10.2)') 'Dry Deposition vel for particles is ', 
c     &            (dv_p(i), i = 1, numpardis(isrc))
c      WRITE(25,'(a,1pe10.2)') 'Washout Coef for I2 ', wcoef_I2
c      WRITE(25,'(a,1pe10.2)') 'Washout Coef for part is ', wcoef_p
      
      RETURN

      END      