      SUBROUTINE SetPlMet( MetInp, amhgt )
C-----------------------------------------------------------------------
C
C     SETPLMET
C
C     Date: 3/16/1998
c     revision 30 Mar 2006 to use MINWIND
c     Revision 5 Feb 2008 to adapt for Droppo's binning problem
C
C     Description:   Puts the new Meteorological Variables into
C                    the common block variables for use in the Plume
C                    models
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      
      INCLUDE  'parm.inc'
      INCLUDE  'metdata.inc' 
      INCLUDE  'srcrec.inc' 
      INCLUDE  'output.inc'
      INCLUDE  'metinput.inc'
      
      RECORD   /metinput/ MetInp

      REAL        INVMOL             
      REAL        USTAR
      REAL        U01
      REAL        degrad, ht, minwin, prob, secwnd, amhgt
      
      INTEGER     i
      
      stab = MetInp.stab
      wnddir = MetInp.wnddir
      atemp = MetInp.tempc
      mixhgt = MetInp.mixhgt
      ptyp = MetInp.pcode
      prate = MetInp.prate
      wght = MetInp.wght

c  Get the inverse M-O length

      molinv = INVMOL(stab,z0)
      
c  Check if calm wind [Need to check if 0.8 m/s is correct or not!!]
c
c      IF( MetInp.wndspd .LT. 0.8 ) THEN
c
c  Assume a 1 m/s wind at anemonometer height
c
c         minwin = 0.8

C
C  Modification to use input maximum windspeed for "calm"
C  BAN 30 March 2006
C
c  Check if calm wind 

      minwin = 0.8
	If (MINWIND .GT. 0.0) minwin = MINWIND

      IF( MetInp.wndspd .LT. minwin ) THEN
c
c  Assume a 1 m/s wind at anemonometer height
c

         ht = amhgt
         ustr = USTAR( ht, minwin, z0, molinv, stab )
c    BAN experiment 3/9/2012
         MetInp.wndspd = minwin

c  Calculate a direction

         prob = U01( rndseed )

c  Check if user defined low wind distribution
         
         IF ( usercalm ) THEN
            DO i = 1, recnum
               IF( prob .LE. calmdis(i) ) THEN
                  EXIT
               ENDIF
            ENDDO
            wndsec = i
         ELSE
            wndsec = INT( prob*recnum )
            IF( wndsec.EQ.0 ) wndsec = recnum
         ENDIF
         
         wnddir = wndsec*(360./recnum)
                                        
      ELSE                          

c        Calculate ustar
      
         ustr = USTAR( amhgt, MetInp.wndspd, z0, molinv, stab )
         
c  Compute the sector in which the wind is blowing towards

c  Fix the Droppo STAR problem with a minor wind dir variation
         wnddir = wnddir + 5.0*(U01(rndseed)*2.0 - 1.0)
c---end fix

         secwnd = wnddir*(recnum/360.0)
         wndsec = INT(secwnd + .499999 )
         IF ( wndsec.eq.0 ) wndsec = recnum
      
      ENDIF   
c  Add up the weights
      
      tot_wght = tot_wght + wght
      
      IF( INT(tot_wght/100) .NE. INT((tot_wght - wght)/100) ) THEN
         WRITE(*,*) 'Have Processed ',tot_wght,' Hours.'
      ENDIF
      
      RETURN
      
      END
