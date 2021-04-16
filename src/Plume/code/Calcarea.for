      SUBROUTINE CALCAREA( isrc, Do_Sect )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     CALCARAC.FOR
c     
c     Christian J Fosmire
c     Pacific Northwest National Laboratory
c     P.O. Box 999
c     Richland, WA 99352
c
c     Created:  8/1/96
c
c     Description:  This subroutine calculates the concentration
c        using the Guassian Plume equation for an area source.  
c        Will calculate either centerline or sector-averaged 
c        depending upon Do_Sect 
c
c     Subroutines:   CALCCONC, CALCDPL, CALCSIG, DEPPLUM, PRE_AREA
c     FUNCTIONS:     AREACONC, ARSECONC
c      
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT    NONE
      
      INCLUDE     'parm.inc'
      INCLUDE     'depos.inc'
      INCLUDE     'srcrec.inc'
      INCLUDE     'metdata.inc'
      INCLUDE     'conc_inp.inc'
      INCLUDE     'ptsrcin.inc'
      INCLUDE     'sigin.inc'
      
      REAL        dry_fract_p(MaxPBins), rq(2), rqp(MaxPBins)
      REAL        ddep_p(MaxPBins), wdep_p(MaxPBins)
      REAL        dry_fract_I2, wet_fract_I2, wet_fract_p
      
      REAL        AREACONC
      REAL        ARSECONC
      REAL        ddep, wdep, ndist, nsigz
      REAL        ang, angdiff, concrec, crtdist, diffang, dist,
     &            hpeff, odist, ohpeff, osigz, sigy, sigz, 
     &            ttime, udepth, y
     
      INTEGER     i, indx, irec, isrc, plmtyp, rvwndsec
      
      LOGICAL     Do_Sect, IsArea     
                
c     Set the variables

      CALL PRE_AREA( sigin, conc_inp, crtdist, isrc )
      
      IsArea = .TRUE.
      udepth = conc_inp.udepth
      conc_inp.numrec = recnum  
      
c     Set Variables for deposition and depletion
      
      q_dpl_I2 = 4.0 * conc_inp.lenside**2 * conc_inp.q_area
      q_dpl_p = 4.0 * conc_inp.lenside**2 * conc_inp.q_area
      oq_dpl_I2 = q_dpl_I2
      oq_dpl_p = q_dpl_p
      odist = MAX(100.0, 5.0 * conc_inp.lenside)
      osigz = 0
      hpeff = conc_inp.rel_hgt
      ohpeff = conc_inp.rel_hgt
      rechgt = conc_inp.rel_hgt
      rq = 1
      rqp = 1
      
c     Set Flag for deposition (0 - sector area, 2 - centerline area)
      
      IF( Do_Sect ) THEN
         plmtyp = 0
      ELSE
         plmtyp = 2
      ENDIF
            

c     Calculate osigz at odist

      sigin.dist = odist
      sigin.dodist = .FALSE.
      
      CALL CALCSIG( sigin )
      
      osigz = sigin.sigz    

d      WRITE(25,*)'for sigz = ',sigz,' crtdist = ',crtdist
                        
c     Have to scan all sectors as need to worry about receptors in 
c     the direction of the wind and 180 degree (for any source sitting
c     in the area)
          
      rvwndsec = wndsec + recnum / 2
      IF( rvwndsec .GT. recnum ) rvwndsec = rvwndsec - recnum
          
      DO irec = 1, inum

c     Check if receptor is same sector as wind is blowing towards

         IF( strsec(irec,isrc) .EQ. wndsec ) THEN

c     Set downwind and the index of recpt.

            dist = strdist(irec,isrc)
            indx = stridx(irec,isrc)

d        WRITE(25,*) 'Irec = ', irec
d        WRITE(25,*) 'indx = ', indx

c     If Centerline, then calculate the crosswind distance

            IF( Do_Sect ) THEN
               y = 0
            ELSE
c               angdiff = 360. / FLOAT(recnum)
c               ang = FLOAT(wndsec) * angdiff 
c               diffang = ABS(wnddir - ang) * 3.14159 / 180.
c               y = dist * ATAN(diffang)
c              Always assume under the centerline if in the sector
               y = 0
            ENDIF

            conc_inp.dwdist = dist
            conc_inp.cwdist = y

                     
c     Call routine to calculate the concentration for receptor irec
                  
            
            IF( Do_Sect ) THEN
               
               concrec = ARSECONC( conc_inp )
            
            ELSE
                  
               concrec = AREACONC( conc_inp )
            
            ENDIF
            
d     WRITE(25,*) 'concrec = ', concrec

c     Calculate travel time

            ttime = dist / relspd

c     Calculate sigy, sigz for deposition calculation

            sigin.dist = dist
            sigin.dodist = .FALSE.
            
            CALL CALCSIG ( sigin )
            
            sigy = sigin.sigy
            sigz = sigin.sigz
                     
c     calculate deposition and depeleted conc
c     no deposition until we reach the sector width

            dry_fract_I2 = 0.0
            dry_fract_p = 0.0
            wet_fract_I2 = 1
            wet_fract_p = 1

            IF( ptyp .GT. 0 ) THEN
               wet_fract_I2 = EXP( - wcoef_I2 * ttime )
               wet_fract_p = EXP( - wcoef_p * ttime )
            ENDIF

            IF( dist .GE. odist ) THEN

c     Old version*****                        
c               CALL DPLSRC( dist, odist, dv, relspd, wcoef, sigparm,
c     &stab, rechgt, hpeff, mixhgt, crtdist, 0., sigv, sigw, 
c     &conc_inp.udepth, q_dpl, oq_dpl, sigz)

               DO WHILE ( dist .GT. 2.0*odist )
                  
                  ndist = 2.0*odist
                  
c     Calculate sigz

                  sigin.dist = odist
                  sigin.dodist = .FALSE.
      
                  CALL CALCSIG( sigin )
      
                  nsigz = sigin.sigz    
                  
                  CALL CALCDPL( isrc, odist, ndist, hpeff, hpeff, osigz, 
     &                          nsigz, crtdist, mixhgt, udepth, relspd )        
               
                  odist = ndist
                  osigz = nsigz
                  oq_dpl_I2 = q_dpl_I2
                  oq_dpl_p = q_dpl_p
               
               ENDDO
               
               CALL CALCDPL ( isrc, odist, dist, hpeff, hpeff, osigz, 
     &                        sigz, crtdist, mixhgt, udepth, relspd )
               
               dry_fract_I2 = q_dpl_I2
               dry_fract_p = q_dpl_p
             
               rq(2) = wet_fract_I2 * dry_fract_I2
               CALL DEPPLUM( plmtyp, dv_I2, wcoef_I2, concrec, 
     &                       relspd, dist, y, sigy, conc_inp.lenside, 
     &                       ddep, wdep )
               DO i = 1, numpardis(isrc)
                  rqp(i) = wet_fract_p * dry_fract_p(i)
                  CALL DEPPLUM( plmtyp, dv_p(i), wcoef_p, concrec,
     &                          relspd, dist, y, sigy, conc_inp.lenside,
     &                          ddep_p(i), wdep_p(i) )
               
               ENDDO
            
            ELSE
               ddep = 0
               wdep = 0
               ddep_p = 0
               wdep_p = 0            
            ENDIF
            
            sigin.dist = dist
            sigin.sigy = sigy
            sigin.sigz = sigz          
 
            
            CALL newCALCCONC( indx, isrc, concrec, rq, rqp, ddep, wdep, 
     &                     ddep_p, wdep_p, ttime, wght, sigin, relspd,
     &                     conc_inp.lenside, Do_Sect, IsArea )            
               
         ELSE IF( strsec(irec,isrc) .EQ. rvwndsec ) THEN

c     If in opposite sector, then calculate concentrations if less 
c     than the lenside ( note:  no depeletion or deposition )
                  
            dist = strdist(irec,isrc)
               
            IF( dist .LT. conc_inp.lenside ) THEN

               conc_inp.dwdist = -dist
               indx = stridx(irec,isrc)
               
               IF ( Do_Sect ) THEN
                  y  = 0
               ELSE               
                  angdiff = 360. / FLOAT(recnum)
                  ang = FLOAT(wndsec - 1) * angdiff
                  diffang = ABS(wnddir - ang) * 3.14159 / 180.
                  y = dist * ATAN(diffang)
               ENDIF
               
               conc_inp.cwdist = y
                  
                     
               IF ( Do_Sect ) THEN
                  concrec = ARSECONC( conc_inp )
               ELSE
                  concrec = AREACONC( conc_inp )
               ENDIF
               
               ddep = 0
               wdep = 0
               ddep_p = 0
               wdep_p = 0
               rq = 1
               rqp = 1               
               ttime = 0
               
               sigin.sigy = 0
               sigin.sigz = 0
               sigin.dist = 0

c     Calculate sigy, sigz for cloud Shine calculation

               sigin.dist = dist
               sigin.dodist = .FALSE.
            
               CALL CALCSIG ( sigin )
            
               sigy = sigin.sigy
               sigz = sigin.sigz
               
               CALL newCalcConc( indx, isrc, concrec, rq, rqp, ddep, 
     &                      wdep, ddep_p, wdep_p, ttime, wght, sigin, 
     &                      relspd, conc_inp.lenside, Do_Sect, IsArea )
                        
            ENDIF     
         ENDIF
      ENDDO ! Receptors
      
d      WRITE(25,*) 'Finished with area source'
      
      RETURN                 
      
      END