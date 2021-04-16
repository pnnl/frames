      SUBROUTINE CALCCONC( indx, isrc, concrec, rq, rqp, ddep, wdep, 
     &                     ddep_p, wdep_p, ttime, wght, sigin, relspd,
     &                     arearad, IsSector, IsArea )
C-----------------------------------------------------------------------
C     CalcConc
C
C     Christian J Fosmire
C     Pacific Northwest National Lab
C     P.O. Box 999
C     Richland, WA 99352
C
C     Date:       7/26/96
C     Updated:    8/24/99 JV Ramsdell add q_line
C
C     Description:   Calculates the Concentration and dry and wet 
C                    depositions  
C
C     Required Models:  None
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      
      INCLUDE  'parm.inc'
      INCLUDE  'depos.inc'
      INCLUDE  'nuc_data.inc'
      INCLUDE  'srcrec.inc' 
      INCLUDE  'output.inc'
      INCLUDE  'sigin.inc'
      
      REAL     ddep_p(MaxPBins), wdep_p(MaxPBins)
      REAL     arearad, ddep, wdep, relspd
      REAL     concrec, ttime, wght, totfrac
      REAL     rq(2), rqp(MaxPBins), qold(MaxNucs), q_line(MaxNucs),
     &         qprt(MaxNucs,MaxPBins)  
     
      INTEGER  i, indx, isrc, istp, k, n, nn, idis, nstep     
 
      LOGICAL  IsArea, IsSector
       
c  Check if concentration is above zero

      IF( concrec .LE. 0 ) THEN
         RETURN
      ENDIF   
      
d      WRITE(25,*) 'rqp = ', rqp
d     WRITE(25,*) 'MassFrac = ',MassFrac(isrc,1)
      
c  Do Decay and Ingrowth

c  Determine the number of step required to do decay

      nstep = INT( ttime/900.0 )
      
      DO n = 1, nnucs
         qold(n) = RelRate(isrc, n)
         DO idis = 1, numpardis(isrc)
            qprt(n,idis) = qold(n) * MassFrac(isrc,idis)
         ENDDO
      ENDDO
      
      IF (nstep .LT. 1 ) THEN
         DO n = 1, nnucs
            IF( deptype(n) .LT. 2 ) THEN
               qold(n) = qold(n) * rq(deptype(n) + 1)
            ELSE
               totfrac = 0.0
               DO idis = 1, numpardis(isrc)
                  qprt(n,idis) = qold(n) * rqp(idis) 
     &                                * MassFrac(isrc,idis)
                  totfrac = totfrac + rqp(idis) * MassFrac(isrc,idis)
               ENDDO
d              WRITE(25,*) 'totfrac = ',totfrac                 
               qold(n) = qold(n) * totfrac
                  
            ENDIF
         ENDDO
      ELSE
      
         DO istp = 1, nstep

c  Start with last daughter in chain up to parent
         
            DO n = nnucs, 1, -1
         
c  First do decay, then ingrowth from other nuclides

               qold(n) = qold(n) * dkfract(n,n)

c  If last step, include depletion

               IF( istp .EQ. nstep ) THEN 
d                 WRITE(25,*) 'deptype(n) = ',deptype(n)
                  IF( deptype(n) .LT. 2 ) THEN
                 
                     qold(n) = qold(n) * rq(deptype(n)+1)
               
                  ELSE
                 
                     totfrac = 0.0
                     DO idis = 1, numpardis(isrc)
                        qprt(n,idis) = qold(n) * rqp(idis) 
     &                              * MassFrac(isrc,idis)
                        totfrac = totfrac + rqp(idis) * 
     &                                 MassFrac(isrc,idis)
                     ENDDO
d                    WRITE(25,*) 'totfrac = ',totfrac                 
                     qold(n) = qold(n) * totfrac
                  
                  ENDIF
               ENDIF        
         
               DO nn = n-1, 1, -1
               
                  IF( dkfract(nn,n) .EQ. 0 ) CYCLE
               
                  qold(n) = qold(n) + qold(nn) * dkfract(nn,n)
            
               ENDDO
            ENDDO
         ENDDO
      ENDIF

      DO n = 1,nnucs
         q_line(n) = qold(n) / relspd
      ENDDO
      
c  Calculate the Cloud Shine
      
      CALL newCLCSHINE( indx, qold, q_line, sigin, arearad, IsSector, 
     &               IsArea, wght )      

d      WRITE(25,*)'q = ',qold                   
d      WRITE(25,*)'qprt = ',qprt
      
      
c  Calculate the hourly values
      
      DO n = 1, nnucs
      
         k = deptype(n) + 1
         
         EOQ_hr(indx,n) = concrec * qold(n)
         
         IF( k .EQ. 2 ) THEN
            
            Dry_hr(indx,n) = ddep * qold(n)
            Wet_hr(indx,n) = wdep * qold(n)
         ELSEIF( k .EQ. 1 ) THEN
            Dry_hr(indx,n) = 0
            Wet_hr(indx,n) = 0            
         ELSEIF( k .EQ. 3 ) THEN
            DO i = 1, numpardis(isrc)
               Dry_hr(indx,n) = Dry_hr(indx,n) + 
     &                           ddep_p(i) * qprt(n,i)
               Wet_hr(indx,n) = Wet_hr(indx,n) + 
     &                           wdep_p(i) * qprt(n,i)
            ENDDO       
         ENDIF

d        WRITE(25,*) 'Dry_hr = ',dry_hr(indx,n)
d        WRITE(25,*) 'Wet_hr = ',wet_hr(indx,n)
               
c     Do Cummulative Values

         EOQ_ch(indx,n) = EOQ_ch(indx,n) + EOQ_hr(indx,n) * wght
         Dry_ch(indx,n) = Dry_ch(indx,n) + Dry_hr(indx,n) * wght
         Wet_ch(indx,n) = Wet_ch(indx,n) + Wet_hr(indx,n) * wght
      
      ENDDO
      
      RETURN
      
      END                                       