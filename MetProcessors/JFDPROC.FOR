CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     JFDPROC
C
C     Christian J Fosmire
C     Pacific Northwest National Laboratory
C     P.O. Box 999
C     Richland, WA 99352
C
C     Created:    1/19/96
C
C     Description:  This program reads in a joint frequency distribution
C        of wind speed, wind direction, and stability and creates
C        psuedo hourly data for input into CHRONSRC and ACUTESRC
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   Change history
C     25 June 2003  BAN  Divided precipitation probabilities by 100.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      
      INCLUDE  'jfdproc.inc'

      REAL     INVMOL
      REAL     MIX_HT
      REAL     PROFILE
      REAL     USTAR
      REAL     prate(4)
      REAL     molmeas, molappl, ustrm, molen, fhgt, ws100, ustr, 
     &         mixhgt, fv, wght, tempk, tempf, pwght, sumcheck, sumhr,
     &         wndspd
      
      INTEGER  irec, statin, rio, iprcp, ist, isp, iwd, imo, np, nst
      
      CHARACTER*80   rsfile

C     Data for precipitation rate (rain and snow)

      DATA prate / 1, 6, 0.25, 1.8 /
      
C     Get Name of Run File

      CALL GETARG(1,rsfile,statin)
      IF( statin.gt.0 ) THEN

         rio = 5
         OPEN(UNIT = rio, FILE = rsfile, STATUS = 'old' )
     
      ELSE
         
         WRITE(*,*) 'Error reading run file'
         STOP 1
      ENDIF

c     Read in the Parameter File

      CALL RDPRMJFD( rio )

c     Read in JFD
      
      IF ( jfdtype .EQ. 0 ) THEN
         CALL READJFD
      ELSE
         CALL READGENI
      ENDIF
      
c     Write out surface roughness used
      
      irec = 1
      
      WRITE( oio, '(f7.3,a1,f5.1)', REC = irec ) z0appl,',',amhgt
      
c     Do by Stability, Wind Speed, and Wind Direction

      sumcheck = 0
      sumhr = 0
      
      DO ist = 1, nstab

c        Calculate 1/L

         molmeas = INVMOL( ist, z0meas )
         molappl = INVMOL( ist, z0appl )
      
         DO isp = 1, nwspd
            
            
c        Calculate U*

            ustrm = USTAR( amhgt, wspd(isp), z0meas, molmeas, ist )

            IF( z0meas .NE. z0appl ) THEN
            
               IF ( ist.GT.4 ) THEN
                  molen = 1/molmeas
                  fhgt = AMIN1(3*molen,100.)
               ELSE
                  fhgt = 100.
               ENDIF
            
               ws100 = PROFILE( z0meas, ustrm, molmeas, ist, fhgt )
            
               ustr = USTAR( fhgt, ws100, z0appl, molappl, ist )       
            
               wndspd = PROFILE( z0appl, ustr, molappl, ist, amhgt )
            
            ELSE
               
               ustr = ustrm
               wndspd = wspd(isp)
            
            ENDIF
                                
            mixhgt = MIX_HT( ustr, molappl, ist, lat )
            
            DO iwd = 1, ndir

c     Determine flow vector (wind direction + 180)

               fv = wdir(iwd) + 180.
               IF( fv.gt.360 ) fv = fv - 360.
               IF( fv.le.0 ) fv = fv + 360.

c     Calculate the weight for psuedo data ( have 12 months )
            
               wght = freq(ist,isp,iwd) / 12.0 
            
c              WRITE(*,*) 
c     &'for stab =',ist,' spd = ',isp,' wnddir = ',iwd,' wght = ',wght
            
c     Do by month

               DO imo = 1, 12               

c     Get the air temperature
                  
                  IF ( ist.lt.3 ) THEN
                     tempk = (5*(maxtmp(imo)-32.)/9) + 273.15
                  ELSEIF ( ist.gt.5 ) THEN
                     tempk = (5*(mintmp(imo)-32.)/9) + 273.15
                  ELSE
                     tempf = (maxtmp(imo) + mintmp(imo))/2.0
                     tempk = (5*(tempf - 32 ) /9 ) + 273.15
                  ENDIF


                  If ( doprcp ) THEN

c     If precip, then determine the weight with and without precip
                     
                     IF( ist .EQ. 1 ) THEN
                        nst = 2
                     ELSEIF( ist .EQ. 7 ) THEN
                        nst = 6
                     ELSE
                        nst = ist
                     ENDIF
                     
                     nst = nst - 1

c     Assume No heavy rain or heavy snow
                     
                     DO iprcp = 1, 4
                        
                        IF( iprcp .GT. 2 ) THEN
                           np = iprcp + 1
                        ELSE
                           np = iprcp
                        ENDIF
                      
CCC                      pwght = wght * prcpprob(iprcp,nst)
CCC            BAN thinks that Christian lost percents here
                        pwght = wght * prcpprob(iprcp,nst)/100.
                        
                        CALL OUPSDATA( oio, irec, ist, fv, wndspd, 
     &                                 tempk, mixhgt, np, 
     &                                 Prate(iprcp), pwght * numhr )
                        sumcheck = sumcheck + pwght
                        sumhr = sumhr + pwght * numhr
                     ENDDO                         
                     CALL OUPSDATA( oio, irec, ist, fv, wndspd, tempk,
     &                              mixhgt, 0, 0.0, 
     &                              wght * nprcp_prob(nst) * numhr )
                     sumcheck = sumcheck + wght*nprcp_prob(nst)
                     sumhr = sumhr + wght * nprcp_prob(nst) * numhr 
                  ELSE
                     CALL OUPSDATA( oio, irec, ist, fv, wndspd, tempk,
     &                              mixhgt, 0, 0.0, wght * numhr )
                     sumcheck = sumcheck + wght 
                     sumhr = sumhr + wght * numhr
                  ENDIF
               ENDDO
            ENDDO
         ENDDO
      ENDDO
      
C      WRITE(oio,*) 'Sum of all weights = ', sumcheck
C      WRITE(oio,*) 'Sum of all hours = ', sumhr
      
      END            
