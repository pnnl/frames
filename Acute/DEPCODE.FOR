c         
C----- Calculate leaf deposition rate from rain ------------------------------
c
              LEAF2 = 0.0
              IF(.NOT.WETSET) THEN
                IF(.NOT.ANION(IN)) THEN           ! Cations/particles
                  DEPFR2 =2.95*BIOMAS(ITF)*DRYFAC(ITF)*RAIN**(-0.191)
C  Cations - old  DEPFR2 =3.25*BIOMAS(ITF)*DRYFAC(ITF)*RAIN**(-0.294)
                ELSE                           ! Anions
                  DEPFR2 = 2.30*BIOMAS(ITF)*DRYFAC(ITF)*RAIN**(-0.92)
                ENDIF
                IF(DEPFR2.GT.1.) DEPFR2 = 1.0
              ENDIF
              LEAF1A = LEAF1A + WETDEP(IN) * DEPFR2
             END IF


C
C----- Calculate leaf deposition rate from irrigation ------------------------
C
              IF (TPATH.LE.2.AND.IRRST(ITF) .GT. 0) THEN
                IF(.NOT.WETSET) THEN
                RATEIRR = RIRR(ITF) * MMIN / (IRTIMT(ITF) * DAYMO)
C                DEPFR2 = 0.0
                 IF(.NOT.ANION(IN)) THEN           ! Cations/particles
                  DEPFR2 =2.95*BIOMAS(ITF)*DRYFAC(ITF)*RATEIRR**(-0.191)
C  Cations - old  DEPFR2 =3.25*BIOMAS(ITF)*DRYFAC(ITF)*RATEIRR**(-0.294)
                 ELSE                           ! Anions
                  DEPFR2 = 2.30*BIOMAS(ITF)*DRYFAC(ITF)*RATEIRR**(-0.92)
                 ENDIF
                IF(DEPFR2.GT.1.) DEPFR2 = 1.0
                ENDIF ! End if on WETSET
                IF (IRRST(ITF) .EQ. 1) THEN
                  WATCON = GWCON(IN)             ! Groundwater source
                ELSE
                  WATCON = SWCON(IN)             ! Surface water source
                ENDIF
                IF (IRTIMT(ITF) .GT. 0.0) THEN
                  LEAF2 = WATCON * RIRR(ITF) * LM2IN / IRTIMT(ITF) 
     .                    * MOYR * DEPFR2
                ENDIF
              ENDIF

