      SUBROUTINE newCALCCONC( indx, isrc, concrec, rq, rqp, ddep, wdep, 
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
C                 2/14/00 JV Ramsdell add species
C                 12/5/2001 BAN dimension rg_fract, ng_fract, part_fract
C                 12/10/2001 BAN repartition progeny during decay
C
C     Description:   Calculates the Concentration and dry and wet 
C                    deposition  
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
      REAL     rq(2), rqp(MaxPBins), qold(MaxNucs,3), q_line(MaxNucs),
     &         qprt(MaxNucs,MaxPBins), tot_qold(MaxNucs)
     
      INTEGER  i, indx, isrc, istp,j, n, nn, idis, nstep     
 
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
         qold(n,1) = RelRate(isrc, n) * ng_fract(n)
         qold(n,2) = RelRate(isrc, n) * rg_fract(n)
         qold(n,3) = RelRate(isrc, n) * part_fract(n)
         DO idis = 1, numpardis(isrc)
            qprt(n,idis) = qold(n,3) * MassFrac(isrc,idis)
         ENDDO
      ENDDO
      
c      WRITE (25,'(a,i5,10(1pe10.3))')  ' newCalcConc, inital q ', nstep,
c     &    (qold(1,n),n=1,3), (qprt(1,idis),idis=1,numpardis(isrc) )

      IF (nstep .LT. 1 ) THEN

         DO n = 1, nnucs
            qold(n,2) = qold(n,2) * rq(2)
            totfrac = 0.0
            DO idis = 1, numpardis(isrc)
               qprt(n,idis) = qold(n,3) * rqp(idis) 
     &                                  * MassFrac(isrc,idis)
               totfrac = totfrac + rqp(idis) * MassFrac(isrc,idis)
            ENDDO
            qold(n,3) = qold(n,3) * totfrac
         ENDDO

      ELSE
      
         DO istp = 1, nstep

c  Start with last daughter in chain up to parent
         
            DO n = nnucs, 1, -1
         
c  First do decay, then ingrowth from other nuclides

               DO j = 1,3
                  qold(n,j) = qold(n,j) * dkfract(n,n)
               ENDDO

c  If last step, include depletion

               IF( istp .EQ. nstep ) THEN 
               
                  qold(n,2) = qold(n,2) * rq(2)
                  totfrac = 0.0
                  DO idis = 1, numpardis(isrc)
                     qprt(n,idis) = qold(n,3) * rqp(idis) 
     &                                        * MassFrac(isrc,idis)
                     totfrac = totfrac + rqp(idis) * MassFrac(isrc,idis)
                  ENDDO
d                    WRITE(25,*) 'totfrac = ',totfrac                 
                  qold(n,3) = qold(n,3) * totfrac

               ENDIF 
       
               IF ( n .GT. 1) THEN
                  DO nn = n-1, 1, -1
                   
                   IF( dkfract(nn,n) .EQ. 0 ) CYCLE
C                     DO j = 1,3            
C                        qold(n,j) = qold(n,j) + 
C     &                                  qold(nn,j)*dkfract(nn,n)
C                      ENDDO
C           Partition Progeny here, BAN 10 Dec 2001
                    qold(n,1) = qold(n,1) + dkfract(nn,n)*ng_fract(n)*
     &                            (qold(nn,1) + qold(nn,2) + qold(nn,3))
                    qold(n,2) = qold(n,2) + dkfract(nn,n)*rg_fract(n)*
     &                            (qold(nn,1) + qold(nn,2) + qold(nn,3))
                    qold(n,3) = qold(n,3) + dkfract(nn,n)*part_fract(n)*
     &                            (qold(nn,1) + qold(nn,2) + qold(nn,3))
                  ENDDO
               ENDIF
            ENDDO
         ENDDO
      ENDIF

c      WRITE (25,'(a,10(1pe10.3))')  ' newCalcConc, final q ', 
c     &     (qold(1,n),n=1,3),(qprt(1,idis),idis=1,numpardis(isrc))

      DO n = 1,nnucs
         q_line(n) = 0.0
         tot_qold(n) = 0.0           
         DO j = 1,3
            q_line(n) = q_line(n) + qold(n,j) / relspd
            tot_qold(n) = tot_qold(n) + qold(n,j) 
         ENDDO
      ENDDO

c      WRITE (25,'(a,10(1pe10.3))')  ' newCalcConc, line q ', 
c     &              tot_qold(1), q_line(1)
      
c  Calculate the Cloud Shine
      
      CALL newCLCSHINE( indx, tot_qold, q_line, sigin, arearad,  
     &               IsSector, IsArea, wght )      

d      WRITE(25,*)'q = ',qold                   
d      WRITE(25,*)'qprt = ',qprt
      
c  Calculate the hourly values
      
      DO n = 1, nnucs
        
         EOQ_hr(indx,n) = concrec * Tot_qold(n)
         
         IF( (qold(n,2)+qold(n,3)) .GT. 0.0 ) THEN
            
            Dry_hr(indx,n) = ddep * qold(n,2)
            Wet_hr(indx,n) = wdep * qold(n,2)
            DO i = 1, numpardis(isrc)
               Dry_hr(indx,n) = Dry_hr(indx,n) + 
     &                           ddep_p(i) * qprt(n,i)
               Wet_hr(indx,n) = Wet_hr(indx,n) + 
     &                           wdep_p(i) * qprt(n,i)
            ENDDO 
         ELSE
            Dry_hr(indx,n) = 0
            Wet_hr(indx,n) = 0            
         ENDIF

d        WRITE(25,*) 'Dry_hr = ',dry_hr(indx,n)
d        WRITE(25,*) 'Wet_hr = ',wet_hr(indx,n)
               
c     Do Cumulative Values

         EOQ_ch(indx,n) = EOQ_ch(indx,n) + EOQ_hr(indx,n) * wght
         Dry_ch(indx,n) = Dry_ch(indx,n) + Dry_hr(indx,n) * wght
         Wet_ch(indx,n) = Wet_ch(indx,n) + Wet_hr(indx,n) * wght
      
      ENDDO
      
      RETURN
      
      END                                       