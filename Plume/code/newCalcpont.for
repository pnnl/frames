
      SUBROUTINE newCALCPONT( isrc, Do_Sect )

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     CALCPONT.FOR
c
c     Christian J Fosmire
c     Pacific Northwest National Laboratory
C     P.O. Box 999
c     Richland, WA 99352
c
c     Created:  8/1/96
c     Updated:  8/24/99 J.V. Ramsdell
c     Corrected 3/28/2006 BANapier plume depletion call
c
c     Description:  This subroutine calculates the concentration, 
c     using the Guassian Plume equation for a point source.  Can do 
c     either sector-averaged or centerline depending upon Do_Sect 
c
c     Subroutines: CALCCONC, CALCDPL, CALCRISE, CALCSIG, DEPPLUM, 
c                  MODSIGS, PRE_PLUM
c     FUNCTIONS:   PNTCONC, PTSECONC
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      IMPLICIT    NONE      

      INCLUDE     'parm.inc'
      INCLUDE     'depos.inc'
      INCLUDE     'srcrec.inc'
      INCLUDE     'metdata.inc' 
c      INCLUDE     'conc_inp.inc'
      INCLUDE     'ptsrcin.inc'
      INCLUDE     'sigin.inc'
      INCLUDE     'iscwkin.inc'
      
      REAL        dry_fract_p(MaxPBins), rq(2), rqp(MaxPBins)
      REAL        dry_fract_I2, wet_fract_I2, wet_fract_p 
      
      REAL        PNTCONC
      REAL        PTSECONC 
      REAL        ddep_p(MaxPBins), wdep_p(MaxPBins)
      REAL        ddep, wdep, ndist, nsigz, nhpeff, nsigy
      REAL        concrec, crtdist, dist, frise,
     &            hpeff, odist, ohpeff, osigz, relhgt, sigbouy,
     &            sigy, sigz, sigzlim, ttime, udepth, y,
     &            horzdisp, vertdisp

      INTEGER     idis, indx, irec, isrc, plmtyp
      
      LOGICAL     Do_Sect, IsArea

c     Set the release rate, release height, and the 
c     plume type and old dist (for deposition)

      rq = 1
      rqp = 1
      oq_dpl_I2 = 1
      oq_dpl_p = 1
      q_dpl_I2 = 1
      q_dpl_p = 1
      relhgt = Src_Var1(isrc)
      rechgt = 1
      odist = 100
      osigz = 0      
      IsArea = .FALSE.

c     Set flag for deposition

      IF ( Do_Sect ) THEN
         plmtyp = 0
      ELSE
         plmtyp = 1
      ENDIF

c     Set Plume Variables

      CALL PRE_PLUM( isrc, frise, crtdist, udepth, iscwkin, sigin )
      
      sigzlim = 1.2 * udepth
         
c     To do depletion, calculate plume height and sigz at odist

      IF( plmflg(isrc) ) THEN

         CALL CALCRISE( isrc, odist, frise, iscwkin, ohpeff, sigbouy )
         
      ENDIF
            
c     Call routine to calculate the sigz at odist
              
      sigin.dist = odist
      sigin.dodist = .FALSE.
              
      CALL CALCSIG( sigin )
              
      osigz = sigin.sigz

c     Alter sigy and sigz for ISC wakes or Low and High wind
c     speed phenonmenons...also bouyance induced dispersion
      
      sigy = 0.0
                       
      CALL MODSIGS( sigin, iscwkin, odist, isrc, wndsec, sigbouy, 
     &              sigy, osigz )      
      
c     Check sigma z aganist the limit

      IF ( osigz .GT. sigzlim ) osigz = sigzlim 

c     Scan through the receptors 

d     WRITE(25,*) 'wndsec = ', wndsec
      
      DO irec = 1, inum

c     Check if receptor is same sector as wind is blowing towards

         IF ( strsec(irec,isrc) .EQ. wndsec ) THEN

c     Set the downwind and crosswind directions and the index of recpt.

            dist = strdist(irec,isrc)
            indx = stridx(irec,isrc)

c            IF( Do_Sect ) THEN
            
               y = 0
            
c            ELSE
               
c               angdiff = 360. / FLOAT(recnum)
c               ang = FLOAT(wndsec) * angdiff
c               diffang = ABS(wnddir - ang) * 3.14159 / 180.
c               y = dist * ATAN(diffang)
c              Always assume under the centerline if in sector

c               y = 0
            
c            ENDIF

c            WRITE (25,'(a)') ' Subroutine newCalcPont'
c            WRITE (25,'(a,L4)') ' Do_sect = ', Do_sect             
c            WRITE(25,'(2(a,i4))') ' For Receptor # ', irec, 
c     &                           '  Receptor index ', indx

c     Set the effective heights to the release height

            hpeff = relhgt

c     Calculate Effective Release Height due plume rise, if required
             
 
            IF( plmflg(isrc) ) CALL CALCRISE( isrc, dist, frise,  
     &                                        iscwkin, hpeff,sigbouy )
                     
c     Call routine to calculate sigz and sigy
              
cccc   A small change to capture nhpeff for cases when plume rise flag is turned off
cccc    BAN 16 JAN 2007
            nhpeff = hpeff
cccc
            sigin.dist = dist
            sigin.dodist = .FALSE.
              
            CALL CALCSIG( sigin )
              
            sigy = sigin.sigy
            sigz = sigin.sigz

c     Alter sigy and sigz for ISC wakes or Low and High wind
c     speed phenonmenons...also bouyancy induced dispersion
              
            CALL MODSIGS( sigin, iscwkin, dist, isrc, wndsec, sigbouy, 
     &                    sigy, sigz )
              
c     Call routine to calculate the concentration for receptor irec
              
            ptsrcin.q_src = 1
            ptsrcin.wndspd = relspd
            ptsrcin.relhgt = hpeff
            ptsrcin.rechgt = rechgt
            ptsrcin.mixhgt = mixhgt
            ptsrcin.dwdist = dist
            ptsrcin.cwdist = y
            ptsrcin.udepth = udepth
            ptsrcin.sigy = sigy
            ptsrcin.sigz = sigz
            ptsrcin.numrec = recnum

          
c            WRITE (25,'( 2(a,f7.2) )') ' wndspd = ',relspd,
c     &                                 ' hpeff = ',hpeff
c            WRITE (25,'( a,f7.2,a,f7.0 )') ' rechgt = ',rechgt,
c     &                                    ' mixhgt = ',mixhgt
c            WRITE (25,'( 3(a,f7.0) )') ' dist = ',dist,' y = ', y,
c     &                                 ' udepth =',udepth
c            WRITE (25,'( 2(a,f7.2) )') ' sigy= ',sigy,' sigz = ',sigz
              
            
            IF( Do_Sect ) THEN
               
               concrec = PTSECONC( ptsrcin )
               
            ELSE   
               
               concrec = PNTCONC( ptsrcin, horzdisp, vertdisp )
c                CALL PNTCONC( ptsrcin, concrec, horzdisp, vertdisp )
           
            ENDIF

c            WRITE (25,'(3f7.2,2(1pe11.3))') 
c     &             relspd, sigy, sigz, horzdisp, vertdisp 
c  
c            WRITE(25,'( a,1pe10.2) )') 'concrec = ',concrec  
          
c     If stack flow, then correct for it
                            
            IF( (st_flow(isrc) .GT. 0.0) .AND. (concrec .GT. 0.0) )
     &          concrec = 1.0 / ((1.0 / concrec) + st_flow(isrc))

c            WRITE(25,'( a,1pe10.2) )') 'concrec = ',concrec 

c     If doing ISC wakes, then if distance is less than 3*MIN(building
c       height, building width), then set concentration to zero
                  
            IF( (iscwkin.doiscwk) .AND. (dist .LT.
     &           3 * AMIN1(iscwkin.bldhgt,iscwkin.bldwth)) ) THEN

               concrec = 0.0

            ENDIF  

c     Calculate depleted x/q and deposition
            
            dry_fract_I2 = 1.0
            dry_fract_p = 1.0
            wet_fract_I2 = 1.0
            wet_fract_p = 1.0
                        
            ttime = dist / relspd
            
            IF( ptyp .GT. 0 ) THEN
               wet_fract_I2 = EXP( -wcoef_I2 * ttime )
               wet_fract_p = EXP( -wcoef_p * ttime )
            ENDIF
            
            IF( dist .GT. odist ) THEN
                        
c     Calculate the depleted release rate

c               CALL DPLSRC( dist, odist, dv, relspd, wcoef, 
c     &sigparm, stab, rechgt, hpeff, mixhgt, crtdist, sigbouy, 
c     &sigv, sigw, udepth, q_dpl, oq_dpl, sigz )


               DO WHILE( dist .GT. 2.0 * odist )

c     calculate plume height and sigz at distance between dist and odist

                  ndist = 2.0 * odist

                  IF( plmflg(isrc) ) THEN

                     CALL CALCRISE( isrc, ndist, frise, iscwkin, nhpeff, 
     &                              sigbouy )
         
                  ENDIF
            
c     Call routine to calculate the sigz
              
                  sigin.dist = ndist
                  sigin.dodist = .FALSE.
              
                  CALL CALCSIG( sigin )
              
                  nsigz = sigin.sigz

c     Alter sigy and sigz for ISC wakes or Low and High wind
c     speed phenonmenons...also bouyance induced dispersion
      
                  nsigy = 0.0
                       
                  CALL MODSIGS( sigin, iscwkin, ndist, isrc, wndsec, 
     &                          sigbouy, nsigy, nsigz )      
      
c     Check sigma z aganist the limit

                  IF ( nsigz .GT. sigzlim ) nsigz = sigzlim 
c
c  BAN 28 March 2006  change dist to ndist
c
c                  CALL CALCDPL( isrc, odist, dist, ohpeff, nhpeff, 
                  CALL CALCDPL( isrc, odist, ndist, ohpeff, nhpeff, 
     &                          osigz, nsigz, crtdist, mixhgt, udepth, 
     &                          relspd ) 

                  oq_dpl_I2 = q_dpl_I2
                  oq_dpl_p = q_dpl_p
                  ohpeff = nhpeff
                  osigz = nsigz
               ENDDO
             
               CALL CALCDPL( isrc, odist, dist, ohpeff, hpeff, osigz, 
     &                       sigz, crtdist, mixhgt, udepth, relspd )
               
               dry_fract_I2 = q_dpl_I2
               dry_fract_p = q_dpl_p                        
            ENDIF
            
c     Set the depleted source and get the deposition
c

            rq(2) = wet_fract_I2 * dry_fract_I2

            CALL DEPPLUM( plmtyp, dv_I2, wcoef_I2, concrec, relspd, 
     &                    dist, y, sigy, 0., ddep, wdep)
            

            DO idis = 1, numpardis(isrc)    
               rqp(idis) = wet_fract_p * dry_fract_p(idis)
               CALL  DEPPLUM( plmtyp, dv_p(idis), wcoef_p, concrec, 
     &                        relspd, dist, y, sigy, 0., ddep_p(idis),
     &                        wdep_p(idis) )   
            ENDDO
            
c            WRITE (25, '(a,10f7.4)') ' Depletion Fractions ',
c     &                     rq(2),(rqp(idis),idis=1,numpardis(isrc)) 

            sigin.dist = dist
            sigin.sigy = sigy
            sigin.sigz = sigz
            
            CALL newCALCCONC( indx, isrc, concrec, rq, rqp, ddep, wdep, 
     &                     ddep_p, wdep_p, ttime, wght, sigin, relspd,
     &                     0.0, Do_Sect, IsArea )


c            WRITE (25, '(a,1pe11.3)') 'Final Concentration = ', concrec

         ELSE IF( strsec(irec,isrc) .GT. wndsec ) THEN

c     If receptor is in a sector larger than the wind sector then can
c      stop searching 

            EXIT
         
         ENDIF


      
      ENDDO
      
      RETURN
      
      END      