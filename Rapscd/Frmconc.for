c        
C     FRMCONC.FOR          Version Date:  June 11, 1997
C     Copyright 1997 by Battelle Memorial Institute. All rights reserved.
C     
C               May 25, 1998 /cjf/ Correct distances out of valley for
C                                  channel flow.
C               July 17, 1998 /cjf/ Allow DET and AFF file to have
C                                   different number of constituents and
C                                   be in a different order.      
C               September 14, 1998 /cjf/ Reformatted output line for
C                                     progenies so that it can handle 
C                                     larger number of releases (>=100) 
C               September 16, 1998 /cjf/ add new area source code to 
C                                        FRMCONC
C               March 10, 2001 /JGD/ FRAMES IO updates
c                                    1) ATO - exclude null arrays, ok
C                                    2) ATO - add ^'s in units format, ok
c                                    3) Use AFF specified particle parm
C               March 15, 2001 /JGD/ FRAMES IO updates
C                                    4) ambient particle (supports item 3)
C               March 20, 2001 /JGD/ 5) more code for item 4)
c
c               March 23, 2001 /jgd/ 6) still more updates for 4)
c               May 7, 2001 /JGD/ null AFF values for progeny enabled
c                          (patch to make exposure code work...)
c               May 8, 2001 /jgd/ incorrect writes to aff file taken out
c                                 New version date
c
c$debug
      PROGRAM FRMCONC
c     subroutine AIRCONC
C
C======================================================================
C
C       ATMOSPHERIC PATHWAY COMPONENT
C       ------------------------------------
C       Written by J.G.Droppo
c       Modified for Framework Project by Christian Fosmire
C
c
      IMPLICIT NONE
      
      INCLUDE  'parm.inc'
      INCLUDE  'apath.inc'
      
c      INTEGER  NARGS, NUMPARTS, ISD ! 20 March 2001 JGD
      INTEGER  NARGS, NUMPARTS ! 8 May 2001 JGD
      INTEGER  io, nfid, ndet, naff, npol, numwu, numhead, i, numflux,
     &         numparent, poltyp, poldf, irel, souleng, nsou, pgileng, 
     &         numrel, numprog, iclass, iprog, nu, np, glyleng, pleng,
     &         facleng, ier, next_pos, iflux, numarg, pgleng, pileng,
c     added new variables / July 17, 1998 CJF/March 10, 2001 JGD
     &         numpol, ipol, numflx, promax
      INTEGER  numdum    
      INTEGER
     & fluxclass [ALLOCATABLE](:), fxparn [ALLOCATABLE](:,:),
     & fxprog [ALLOCATABLE](:,:,:),numfpar [ALLOCATABLE](:),
     & numfpro [ALLOCATABLE](:,:)
c     added next variable / September 16, 1998 CJF
c     added new variable / July 17, 1998 CJF
      INTEGER  In_Poldf [ALLOCATABLE](:)
c     added next variable / September 16, 1998 CJF
      INTEGER  endpos

      REAL     dist(numdist)
      REAL*4   flux [ALLOCATABLE](:),
     & emission [ALLOCATABLE](:),
     & fdata [ALLOCATABLE](:),
     & partdia [ALLOCATABLE](:),
     & gasres [ALLOCATABLE](:),
     & gassvr [ALLOCATABLE](:)
      REAL     airconc(numdir,numdist), depconc(numdir,numdist)
      REAL     reltim, erat, unitconv, partdmr
c              jgd/partdia to partdmr/march 10, 2001
c     added next variable/ September 16, 1998 CJF
      REAL     areasize
      
      CHARACTER*8
     & uemiss [ALLOCATABLE](:),
     & ufdata [ALLOCATABLE](:),
     & upartdia [ALLOCATABLE](:),
     & unotdef [ALLOCATABLE](:),
     & ugassvr [ALLOCATABLE](:)
        
      CHARACTER*12 fluxnam [ALLOCATABLE](:) 
c     added next two variables / July 17, 1998
      CHARACTER*12 In_PolCas [ALLOCATABLE](:)
      CHARACTER*20 In_PolNam [ALLOCATABLE](:)
      CHARACTER Recin*80, RecTyp*1, FacFnm*8, SouFnm*8, MetFnm*8,
c     &          dum*1, PolCas*12, PolNam*20, ParNam*20,
c     &          dum*1, ParNam*20, LABEL*70,  !JGD March 15, 2001
     &          dum*1, ParNam*20, !JGD May 08 2001
     &          ParID*12, funit*4, unit*8, relunit*2, PrgNam*20, 
     &          PrgID*12, teststr*100, glypnm*33, airglyph*33
c     added next variables /September 16, 1998 CJF
      CHARACTER*10 s_type,chktype
      
      LOGICAL   COMPSTR
c     added next variable / September 16, 1998 CJF
      LOGICAL   Is_Area
      
c     distances are a combination of what HAZ uses and others...
      DATA dist/0.10, 0.20,0.50,0.81, 1.00,2.00, 2.41, 4.02, 5.63,7.24,
     +          10.0,12.10,15.0,20.0,24.10,30.0,40.20,56.30,72.40,75.0/

c     Set Unit Numbers for Files

      NFID = 2
      NAIN = 3
      NAFF = 5
      NDET = 7
      io   = 9
      NSOU = 17

c     define array size for max # of progeny for any parnet
      promax=12     ! 22 March 01 / JGD

C     Get Name of Air Glyph from Command Line
      numarg = NARGS()
      IF ( numarg .LE. 1 ) THEN
         WRITE(*,*) 'Error - No Air Glyph Name Given!'
         STOP 1
      ENDIF
      
      CALL GETARG(1,airglyph,ier)
      IF( ier .LE. 0 ) THEN
         WRITE(*,*) 'Error reading Air Glyph Name from Command Line!'
         STOP 1
      ENDIF
      
C.......OPEN AND READ INPUT FILES

C --- Get AIR waste unit count and filenames

      OPEN (UNIT=NFID,FILE='FACIL.ID',STATUS='OLD')

10    READ (NFID,4000) RECIN
      READ (RECIN,4001) RECTYP
      IF (RECTYP.NE.'0') GOTO 10
      READ (RECIN,4010) FACFNM   
      facleng = LEN_TRIM(Facfnm)

 4000 FORMAT (A80)
 4001 FORMAT (1x,A1)
 4002 FORMAT (3X,I3)
 4003 FORMAT (3X,A8,2X,A8)
 4010 FORMAT (3X,A8)
 4011 FORMAT (I3)
 4012 FORMAT (A12,A20,I3,3X,I3)
 4013 FORMAT (A12,10X,7G10.3)
c 4014 format (1X,i2,a,f6.2,a,3(1pe9.2,a))
c 4015 FORMAT (I2,A,A,A,A,A,I2,A,I3,A,G9.2)
      REWIND (NFID)
   20 READ (NFID,4000) RECIN
      READ (RECIN,4001) RECTYP
      IF (RECTYP.NE.'2') GOTO 20
      READ (RECIN,4002) NUMWU

      DO nu = 1,numwu
         READ (NFID,4003) SOUFNM, METFNM 
         souleng = LEN_TRIM(soufnm)
         OPEN(io,FILE=SOUFNM(1:souleng)//'.ATO',STATUS='UNKNOWN')

C     WRITE Header
          
         CALL WR_HEAD( io, soufnm, souleng )

C     Write out number of data set (will always be one)         
         WRITE( io, '(i2,a1)')1,','         
                  
C       READ FROM OPTIONAL FILES FOR DISTANCES/JGD/October 16, 1994
C       Looks for SOURFNM.XKM first, then 'AIRDIST.KM'
c        OPEN(NDET,FILE=SOUFNM//'.XKM',STATUS='OLD',IOSTAT=IER)
c        IF(IER.NE.0) THEN
cx          OPEN(NDET,FILE='AIRDIST.KM',STATUS='OLD',IOSTAT=IER)
cx          IF(IER.EQ.0) THEN
cx             WRITE(9,*) ' "Distances read from AIRDIST.KM"'
cx          ENDIF
c        ELSE
c          WRITE(9,*) '" Distances read from ',SOUFNM//'.XKM"'
c        ENDIF
cx         IF(IER.NE.0) THEN
cx           WRITE(9,*) ' Default distances used for listing'
cx           READ(NDET,'(A70)') LABEL
cx           READ(NDET,*) (DIST(isd),isd=1,10)
cx           READ(NDET,*) (DIST(isd),isd=11,20)
cx           WRITE(9,*) '"',LABEL,'"'
cx          ENDIF  !cx -> reads to unsupported file commented out
c        CLOSE(NDET)

         OPEN (UNIT=NDET, FILE=FACFNM(1:facleng)//'.DET', STATUS='OLD')
         OPEN (UNIT=NSOU, FILE=SOUFNM(1:souleng)//'.SOU', STATUS='OLD')
c        OPEN (UNIT=NCNC, FILE=SOUFNM//'.CNC', STATUS='OLD')
         OPEN (UNIT=NAIN, FILE=SOUFNM(1:souleng)//'.AIR', STATUS='OLD')
         OPEN (UNIT=NAFF, FILE=SOUFNM(1:souleng)//'.AFF', STATUS='OLD')


c     Added code to read in pollutant information from DET file
c     into allocated arrays /July 17, 1998 cjf

c     Get number of pollutants from DET file
        
         READ (NDET,4011) NumPol

c     Allocate Arrays for pollutant from DET file

         ALLOCATE( IN_PolCas(NumPol), IN_PolNam(NumPol), 
     &             IN_Poldf(NumPol), STAT = ier )
         
         IF ( ier .NE. 0 ) THEN
            WRITE(*,*) 'Error trying to ALLOCATE DET arrays'
            WRITE(*,*) 'Error number = ', ier
            STOP 1
         ENDIF

c     Read in the pollutant information from the DET file
         
         DO i = 1, NumPol

            READ (NDET,4012) IN_PolCas(i), IN_PolNam(i), POLTYP, 
     &                       IN_Poldf(i)
         ENDDO

c     Finished with DET File
      
         CLOSE(NDET)

c     Get Glyph Name from Source File

         READ (NSOU,40, IOSTAT = ier) NPOL
 40   FORMAT (2x,I2) 
         IF( ier .NE. 0 ) THEN
            WRITE(*,*) 'ERROR reading Source File ', 
     &                  soufnm(1:souleng)//'.SOU'
            WRITE(*,*) 'Error Number - ', ier
            STOP 1
         ENDIF           

c        Skip the pollutant information

         DO i = 1, NPOL
            READ(NSOU,'(a1)')dum
         ENDDO

c        Read in Glyph Name
         READ( NSOU, '(a)' )glypnm         
                  
         CLOSE(NSOU)

c        Find correct Section in AFF file
         
         CALL FINDGLYP(NAFF,glypnm,ier)
         
         IF( ier .NE. 0 ) THEN
            WRITE(*,*) 'Unable to find Glyph Name ',glypnm
            WRITE(*,*) 'In AFF file ', soufnm(1:souleng)//'.AFF'
            STOP 1
         ENDIF
        
c     Read past header info
        
         READ( NAFF, * )numhead
         DO i = 1, numhead
            READ( NAFF, '(a1)' )dum
         ENDDO
         
C     Read past number of flux sets and flux names
c     Correct logic/ September 16, 1998 CJF
         READ( NAFF, * ) numdum
         DO i = 1, numdum
            READ( NAFF, '(a1)') dum
         ENDDO                        

c     Read in type of source and size of area / September 16, 1998 CJF
         READ( NAFF, '(a)' ) s_type
         CALL READSTR(s_type,1,chktype,endpos)         
         IF ( COMPSTR(chktype,'AREA') ) THEN
            Is_Area = .TRUE.
         ELSE  
            Is_Area = .FALSE.
         ENDIF
         READ( NAFF, * ) Areasize
         
         
c     Read past Source Info

         DO i = 1, 5
            READ( NAFF, '(a1)' )dum
         ENDDO

c     Read in Number of fluxes

         READ( NAFF, *) numflux
         glyleng = LEN_TRIM(airglyph)
         WRITE(io,'(i3,a2,a,a2)')numflux,',"',airglyph(1:glyleng),'",'
         
         ALLOCATE( flux(numflux), fluxclass(numflux), fluxnam(numflux), 
     &          gasres(numflux), unotdef(numflux), 
     &          emission(numflux), fdata(numflux), gassvr(numflux),
     &          uemiss(numflux), ufdata(numflux), ugassvr(numflux),
     &          partdia(numflux), upartdia(numflux),
     &          STAT = ier )
     
         IF ( ier .NE. 0 ) THEN
            WRITE(*,*) 'Error trying to ALLOCATE flux arrays'
            WRITE(*,*) 'Error number = ', ier
            STOP 1
         ENDIF

c     Create an array that defines the non-zero flux types for this run

c        Determine which flux goes with each type (gas or particles)
c        New Code Section Created March 10, 2001 JGD
c

         DO i = 1, numflux
            READ( NAFF, '(a)') teststr
            WRITE(io,'(a)')teststr
            CALL READSTR(teststr,1,fluxnam(i),next_pos)
            IF( INDEX(fluxnam(i),'Gas') .NE. 0 ) THEN
               fluxclass(i) = 0
               CALL READREAL(teststr,next_pos,gasres(i),next_pos)
               CALL READSTR(teststr,next_pos,unotdef(i),next_pos)
               CALL READREAL(teststr,next_pos,fdata(i),next_pos)
               CALL READSTR(teststr,next_pos,ufdata(i),next_pos)
              ELSE
               IF( fluxnam(i)(1:8) .NE. 'Particle' ) THEN
                  WRITE(*,*) 'ERROR reading AFF file'
                  WRITE(*,*) 'Flux Type is not defined correctly'
                  STOP 1
               ENDIF
               CALL READREAL(teststr,next_pos,partdia(i),next_pos)
               CALL READSTR(teststr,next_pos,upartdia(i),next_pos)
               CALL READREAL(teststr,next_pos,fdata(i),next_pos)
               CALL READSTR(teststr,next_pos,ufdata(i),next_pos)
            ENDIF
         ENDDO           

c        Read in the number of parent consitutents from AFF file

         READ( NAFF, * ) numparent

         ALLOCATE(numfpar(numparent))
         ALLOCATE(numfpro(numparent,promax))
         ALLOCATE(fxparn(numparent,numflux),
     &    fxprog(numparent,numflux,promax))
c        Loop over parents
         DO np = 1, numparent 

            DO i = 1, numflux
               fxparn(np,i)=0
               do iprog=1,promax
                 fxprog(np,i,iprog)=0
               enddo
            ENDDO

            READ (NAFF,'(a)') teststr
            CALL READSTR(teststr,1,ParNam,next_pos)
            CALL READSTR(teststr,next_pos,ParID,next_pos)
            CALL READSTR(teststr,next_pos,funit,next_pos)
            CALL READSTR(teststr,next_pos,unit,next_pos)
            CALL READINT4(teststr,next_pos,numrel,next_pos)
            CALL READINT4(teststr,next_pos,numprog,next_pos)

            IF(numprog.gt.promax) THEN
               WRITE(*,*) 'ERROR data in AFF file'
               WRITE(*,*) 'More progeny than array size(',promax,')'
               STOP 1
            ENDIF

c           Loop over the fluxes
            DO irel = 1, numrel
c           Read time and fluxes from AFF file

               READ( NAFF, * )reltim, (flux(i), i = 1, numflux)

               DO iflux = 1, numflux
                  if (flux(iflux).gt.0.0) then
                     fxparn(np,iflux)=1
                  endif
               ENDDO !Number of Fluxes

c              define number of non-zero flux classes/jgd march 10, 2001
               numfpar(np)=0
               do iflux = 1, numflux
                 if (fxparn(np,iflux).ne.0) then
                    numfpar(np)=numfpar(np)+1
                 endif
               enddo

c              code to always putout one array, even if null
c              code enabled/May 7, 2001 /JGD
               if (numfpar(np).eq.0) then
                    numfpar(np) =1
                    fxparn(np,1)=1
               endif

            ENDDO ! Number of Releases
            
c           Loop over each Progeny
         
            DO iprog = 1, numprog

c           Read Constituent Line from AFF file

               READ (NAFF,'(a)') teststr
               CALL READSTR(teststr,1,PrgNam,next_pos)
               CALL READSTR(teststr,next_pos,PrgID,next_pos)
               CALL READSTR(teststr,next_pos,funit,next_pos)
               CALL READSTR(teststr,next_pos,unit,next_pos)
               CALL READINT4(teststr,next_pos,numrel,next_pos)
               CALL READSTR(teststr,next_pos,ParNam,next_pos)
               CALL READSTR(teststr,next_pos,ParID,next_pos)


c               READ (NAFF,*) PrgNam, PrgID, dum, unit, numrel, ParNam, 
c     &                       ParID


c              Loop over the fluxes
         
               DO irel = 1, numrel


c                 Read time and fluxes from AFF file

                  READ( NAFF, * )reltim, (flux(i), i = 1, numflux)

                  DO iflux = 1, numflux
                    if (flux(iflux).gt.0.0) then
                     fxprog(np,iflux,iprog)=1
                    endif
                  ENDDO !Number of Fluxes

c                 define number of non-zero flux classes/jgd march 10, 2001
                  numfpro(np,iprog)=0
                  do iflux = 1, numflux
                    if (fxprog(np,iflux,iprog).ne.0) then
                      numfpro(np,iprog)=numfpro(np,iprog)+1
                    endif
                  enddo

c                 code to always put out one array, even if null
c                 enabled May 7, 2001/jgd
                  if (numfpro(np,iprog).eq.0) then
                     numfpro(np,iprog) =1
                     fxprog(np,1,iprog)=1
                  endif

               ENDDO ! Number of Releases
            ENDDO ! Progeny  
         ENDDO  ! Parents
c
c        Refind Start of Flux Values in AFF File
           close(NAFF)
           OPEN (UNIT=NAFF,FILE=SOUFNM(1:souleng)//'.AFF',STATUS='OLD')
c          Refind correct Section in AFF file
           CALL FINDGLYP(NAFF,glypnm,ier)
c          Read past header info
           READ( NAFF, * )numhead
           DO i = 1, numhead
            READ( NAFF, '(a1)' )dum
           ENDDO
C          Read past number of flux sets and flux names
           READ( NAFF, '(a1)') dum
           READ( NAFF, '(a1)') dum
c          Read past Source Info
           DO i = 1, 7
            READ( NAFF, '(a1)' )dum
           ENDDO
c          Read in Number of fluxes
           READ( NAFF, *) numflx
           IF ( numflux.ne.numflx) THEN
              WRITE(*,*) 'Error finding flux location in AFF file'
              WRITE(*,*) '(FRMCONC.FOR)'
              STOP 1
           ENDIF

c        Determine which flux goes with each type (gas or particles)
         NUMPARTS=0
         DO i = 1, numflux
            READ( NAFF, '(a)' )teststr                         
            CALL READSTR(teststr,1,fluxnam(i),next_pos)
            
            IF( COMPSTR(fluxnam(i),'GAS') ) THEN
               fluxclass(i) = 0
            ELSE
               IF( .NOT.COMPSTR(fluxnam(i),'PARTICLE')  ) THEN
                  WRITE(*,*) 'ERROR reading AFF file'
                  WRITE(*,*) 'Flux Type is not defined correctly'
                  STOP 1
               ENDIF
               
               CALL READREAL(teststr,next_pos,partdmr,next_pos)
c               IF( partdmr .LE. .03 ) THEN ! 20 March 2001 /JGD
c                  fluxclass(i) = 3   ! part. class now matches flux 
c               ELSEIF( partdmr .LE. 3.0 ) THEN
c                  fluxclass(i) = 2
c               ELSE
c                  fluxclass(i) = 1
c               ENDIF
                numparts=numparts+1  ! 20 March 2001 /JGD
                IF(numparts.gt.3) then
                  WRITE(*,*) 'ERROR reading AFF file'
                  WRITE(*,*) 'Two many particle flux types'
                  STOP 1
                ELSE
                  fluxclass(i)=numparts! 20 March 2001 /JGD
                ENDIF
            ENDIF
         ENDDO
         
c        Read in the number of parent consitutents from AFF file

         READ( NAFF, * ) numparent

c        WRITE to Output File

         WRITE( io, '(a,a,a,i2,a1)')
     &   '"chronic", ','"polar",','"grid",', numparent, ','
 
C        INITIALIZE AIR ARRAYS
         CALL AIRH(0,0.,0.,0.,0.,0.,0.,0.)

c        Loop over parents

         DO np = 1, numparent 

c        Get Pollutant Type from DET file
c        Read Constituent Line from AFF file

            READ (NAFF,'(a)') teststr
            CALL READSTR(teststr,1,ParNam,next_pos)
            CALL READSTR(teststr,next_pos,ParID,next_pos)
            CALL READSTR(teststr,next_pos,funit,next_pos)
            CALL READSTR(teststr,next_pos,unit,next_pos)
            CALL READINT4(teststr,next_pos,numrel,next_pos)
            CALL READINT4(teststr,next_pos,numprog,next_pos)

c           Check if DET and AFF file match 
            
            poldf = -1
            DO ipol = 1, NumPol
               IF( COMPSTR(IN_PolCas(ipol), ParID) .AND. 
     &              COMPSTR(IN_PolNam(ipol), ParNam) ) THEN
                  poldf = IN_Poldf(ipol)
                  EXIT
               ENDIF
            ENDDO 
                        
            IF( poldf .LT. 0 ) THEN
               WRITE(*,*) 
     &         'Error - Constitutent in AFF does not match any in DET'
               WRITE(*,*) 'Constitutent is (Name,Cas-ID) ',ParNam, ParID
               STOP 1
            ENDIF


c           Determine units of output products
         
            IF( COMPSTR(unit,'PCI')) THEN
               relunit = 'Bq'
               unitconv = 1.172E-9 !From pCi/yr to Bq/s
               
            ELSEIF( COMPSTR(unit,'G/Y')) THEN
               relunit = 'kg'       
               unitconv = 3.169E-11 !From g/yr to kg/s
            
            ELSEIF( COMPSTR(unit,'BQ') ) THEN
               relunit = 'Bq'
               unitconv = 1
            ELSEIF ( COMPSTR(unit,'KG') ) THEN
               relunit = 'kg'
               unitconv = 1
            ELSE
               WRITE(*,*) 'ERROR - unknown flux units '
               WRITE(*,*) 'Given unit of flux is ', unit
               STOP 1   
            ENDIF

            pleng = LEN_TRIM(ParNam)
            pileng = LEN_TRIM(ParID)
            
            WRITE( io,'(2(a1,a,a2),2(i6,a1))')
     &      '"', ParNam(1:pleng), '",', '"', ParID(1:pileng), '",',
     &       numrel,',', numprog, ','

c           Loop over the fluxes
         
            DO irel = 1, numrel


c              Read time and fluxes from AFF file

               READ( NAFF, * )reltim, (flux(i), i = 1, numflux)

               WRITE( io, '(1p,e9.3,a,i1,a1)')
c     &            reltim, ',"yr",', 2*numflux, ','
     &            reltim, ',"yr",', 2*numfpar(np), ','
c                                            ! JGD march 10, 2001
             
               DO iflux = 1, numflux
c              jgd add loop to filter zero values/march 10, 2001
               IF(fxparn(np,iflux).gt.0) then
c              Zero Out Concentrations and Depositions

                  airconc = 0
                  depconc = 0

C                 Determine if flux class is gas or particle
               
                  IF( fluxclass(iflux) .EQ. 0 ) THEN
                                 
c                    Gas - Check if consitutent is a gas (poldf>3)
c                    or particle (poldf<=3)
            
                     IF( poldf .LT. 4 ) THEN
c                       Gas as a paricle/Ambient Aerosol
                        iclass = 6
                     ELSE
C                       constituent is a gas, use class from DET file
                        iclass = poldf
                     ENDIF
                  ELSE
c                    particle - deposition class is fluxclass
                     iclass = fluxclass(iflux)
                  ENDIF 
                  erat = flux(iflux) * unitconv
                  IF( erat .NE. 0 ) THEN
c                 Change arguements for new area / September 16, 1998
                     CALL CALCPROD( iclass, numdist, numdir, dist, 
c     &                              erat, airconc, depconc )
     &                              erat, airconc, depconc, Is_Area, 
     &                              Areasize )
                  ENDIF                        

                                                      
c                 Write Concentration to Output File

                  CALL WR_OPROD( io, numdist, numdir, dist, airconc, 
     &                           depconc, relunit, fluxnam(iflux) )
               endif ! JGD mach 10, 2001 - filter zero cases      
               ENDDO !Number of Fluxes
            ENDDO ! Number of Releases
            
c           Loop over each Progeny
         
            DO iprog = 1, numprog

c               Get Pollutant Type from DET file
c               From Progeny Line from AFF file
c               Read Constituent Line from AFF file

                READ (NAFF,'(a)') teststr
                CALL READSTR(teststr,1,PrgNam,next_pos)
                CALL READSTR(teststr,next_pos,PrgID,next_pos)
                CALL READSTR(teststr,next_pos,funit,next_pos)
                CALL READSTR(teststr,next_pos,unit,next_pos)
                CALL READINT4(teststr,next_pos,numrel,next_pos)
                CALL READSTR(teststr,next_pos,ParNam,next_pos)
                CALL READSTR(teststr,next_pos,ParID,next_pos)

c               READ (NAFF,*) PrgNam, PrgID, dum, unit, numrel, ParNam, 
c     &                       ParID
               
c               added new logic to check DET vs AFF/ July 17, 1998 CJF
c               Check if DET and AFF file match 
            
                poldf = -1
                DO ipol = 1, NumPol
                  IF( COMPSTR(IN_PolCas(ipol), ParID) .AND. 
     &               COMPSTR(IN_PolNam(ipol), ParNam) ) THEN
                     poldf = IN_Poldf(ipol)
                     EXIT
                  ENDIF
                ENDDO 
                        
                IF( poldf .LT. 0 ) THEN
                  WRITE(*,*) 
     &          'Error - Constitutent in AFF does not match any in DET'
                  WRITE(*,*) 'Constitutent is (Name,Cas-ID) ',ParNam,
     &                        ParID
                  STOP 1
                ENDIF


c              Determine units of output products
         
               IF( COMPSTR(unit,'PCI')) THEN
                  relunit = 'Bq'
                  unitconv = 1.172E-9 !From pCi/yr to Bq/s
               
               ELSEIF( COMPSTR(unit,'G/Y') ) THEN
                  relunit = 'kg'       
                  unitconv = 3.169E-11 !From g/yr to kg/s
            
               ELSEIF( COMPSTR(unit,'BQ') ) THEN
                  relunit = 'Bq'
                  unitconv = 1
               ELSEIF ( COMPSTR(unit,'KG') ) THEN
                  relunit = 'kg'
                  unitconv = 1
               ELSE
                  WRITE(*,*) 'ERROR - unknown flux units '
                  WRITE(*,*) 'Given unit of flux is ', flux
                  STOP 1   
               ENDIF
               
               pgleng = LEN_TRIM(PrgNam)
               pgileng = LEN_TRIM(PrgID)
               pleng = LEN_TRIM(ParNam)
               Pileng = LEN_TRIM(ParID)

c              Reformatted for number of releases (emissions)
c              to handle more than 2 digits /September 14, 1998 CJF                            
c               WRITE( io,'(2(a1,a,a2),i2,a1,2(a1,a,a2))')
               WRITE( io,'(2(a1,a,a2),i6,a1,2(a1,a,a2))')
     &         '"', PrgNam(1:pgleng), '",', '"', PrgID(1:pgileng), '",',
     &          numrel, ',','"', ParNam(1:pleng), '",', '"', 
     &         ParID(1:pileng), '",' 
                                      
c              Loop over the fluxes
         
               DO irel = 1, numrel


c              Read time and fluxes from AFF file

                  READ( NAFF, * )reltim, (flux(i), i = 1, numflux)

                  WRITE( io, '(1p,e9.3,a,i1,a1)')
c       &               reltim, ',"yr",', 2*numflux, ','
     &               reltim, ',"yr",', 2*numfpro(np,iprog), ','

               
                  DO iflux = 1, numflux
c                 jgd add loop to filter zero values/march 10, 2001
                  IF(fxprog(np,iflux,iprog).gt.0) then

c                    Zero Out Concentrations and Depositions

                     airconc = 0
                     depconc = 0

C                    Determine if flux class is gas or particle
               
                     IF( fluxclass(iflux) .EQ. 0 ) THEN
                                 
c                    Check if consitutent is a gas (poldf>3)
c                    or particle (poldf<=3)
            
                        IF( poldf .LT. 4 ) THEN
c                       particle decay products are deposited as
c                       ambient aerosols
                           iclass = 6
                        ELSE
                           iclass = poldf
                        ENDIF

                     ELSE

c                       determine deposition type from DET file
               
                        iclass = fluxclass(iflux)
            
                     ENDIF 
                  
                     erat = flux(iflux) * unitconv
                     IF( erat .NE. 0 ) THEN
c                       Change arguements for new area / September 16, 1998
                        CALL CALCPROD( iclass, numdist, numdir, dist, 
c     &                              erat, airconc, depconc )
     &                              erat, airconc, depconc, Is_Area, 
     &                              Areasize )
                                             
                     ENDIF
                                                      
c                    Write Concentration to Output File

                     CALL WR_OPROD( io, numdist, numdir, dist, airconc, 
     &                              depconc, relunit, fluxnam(iflux) )
                  endif ! filter zero fluxes   

                  ENDDO !Number of Fluxes
               ENDDO ! Number of Releases
            ENDDO ! Progeny  
         ENDDO  ! Parents

         DEALLOCATE ( flux, fluxnam, fluxclass, STAT = ier )
      
         IF( ier .NE. 0 ) THEN
            WRITE(*,*) 'Error Deallocating Flux arrays'
            WRITE(*,*) 'Error Number - ', ier
         ENDIF

         DEALLOCATE ( IN_PolCas, IN_PolNam, IN_Poldf, STAT = ier )
      
         IF( ier .NE. 0 ) THEN
            WRITE(*,*) 'Error Deallocating Flux arrays'
            WRITE(*,*) 'Error Number - ', ier
         ENDIF

            
      ENDDO ! Wasteunits      

      CLOSE( NAFF )
      CLOSE( NDET )
      CLOSE( NAIN )
      CLOSE( IO )
     
      END


C     Change subroutine for new area source /September 16, 1998 CJF                    
      SUBROUTINE CALCPROD( iclass, ndist, ndir, dist, erat, airc, 
C    &                     depc )
     &                     depc, Is_Area, Areasize ) 
C-----------------------------------------------------------------------
C     CONCPROD
C
C     Christian J Fosmire
C
C     Date: 8/16/96
C
C     Description:   This subroutine calculates the output products
C                    which are air concentrations and surface depostions
C
C-----------------------------------------------------------------------
      IMPLICIT NONE

c     add in parameter common block /September 16, 1998
      INCLUDE  'parm.inc'

c     change numdist and numdir to ndist and ndir
c     INTEGER  iclass, numdist, numdir, jdir, isd            
      INTEGER  iclass, ndist, ndir, jdir, isd
     
c     REAL     airc(numdir, numdist), depc(numdir, numdist)      
c     REAL     dist(numdist)
      REAL     airc(ndir, ndist), depc(ndir, ndist)
      REAL     dist(ndist)                      
c     add next two variables/ September 16, 1998 CJF
      REAL     con_cir(numdir), dep_cir(numdir)      
      REAL     erat, xdir, xdis, dcon, airt, bld, soil
c     add next four variables/ September 16, 1998 CJF
      REAL     Areasize, radius, radkm                        
      LOGICAL  Is_Area

c     If area calculate radius /September 16, 1998 CJF
      IF( Is_Area ) THEN
         radius = SQRT(Areasize/3.14159)
         radkm = radius/1000.0
      ELSE  
         radius = 0.0
         radkm = radius/1000.0
      ENDIF

c     Calculate for each direction and distance      
      
      DO jdir = 1, numdir
         xdir = jdir

c     Calculate concentration on circle if one or more distances 
c     less than radius of circle /September 16, 1998 CJF
         IF( dist(1).LT. radkm ) THEN
            
            dcon = 0.0
            airt = 0.0
            bld = 0.0
            soil = 0.0
            
            CALL AIRH( iclass, erat, radkm, xdir, dcon, airt, bld, 
     &                    soil) 
            con_cir(xdir) = airt
            dep_cir(xdir) = soil
         ENDIF
         
         DO isd = 1, numdist
            xdis = dist(isd)   


c     Initialize variables            
            
            dcon = 0.0
            airt = 0.
            bld = 0.
            soil = 0.

c     Calculate Air concentration and sfc deposition
            
            CALL AIRH( iclass, erat, xdis, xdir, dcon, airt, bld, soil )
            
            airc(jdir,isd) = airt
            depc(jdir,isd) = soil

         ENDDO

      ENDDO

c     Added Calls for new area source/ September 16, 1998 CJF
      IF( Is_Area ) THEN

c     Add call to inner area calculation if needed /September 16, 1998 CJF
         IF( dist(1) .LT. radkm ) THEN
           CALL INN_AREA( numdist, numdir, dist, airc, depc, con_cir,
     &                     dep_cir, radius )
         ENDIF

         CALL LAT_INTG( numdist, numdir, dist, airc, depc, Areasize )
         CALL VERT_INT( numdist, numdir, dist, airc, depc, Areasize )
      ENDIF
            
      RETURN                                               
      
      END

      SUBROUTINE WR_OPROD( io, numdist, numdir, dist, airconc, depconc,
     &                     unit, fluxtype )
C-----------------------------------------------------------------------
C     WR_OPROD
C
C     Christian J Fosmire
C
C     Date:    6/11/97
C
C     Description:   This program write the output products to the
C                    output file
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      
      INTEGER  io, numdist, numdir, i, idir, flxleng, unileng, prtzro
      
      REAL     airconc(numdir, numdist), depconc( numdir, numdist)
      REAL     dist(numdist)
      REAL     dir
      REAL     sumtst ! JGD
      
      CHARACTER*2 unit      
      CHARACTER*12 airunit, depunit
      CHARACTER*72 fmt, fmt1, fmt2 ! JGD March 10, 2001
      CHARACTER*(*) fluxtype
      

c     PRTZRO controls output to the ATO..
c      PRTZRO=1 ! prints out zero lines
c      PRTZRO=0 ! skips all zero lines
      PRTZRO=1

c     Determine units of products

      IF( unit .EQ. 'Bq' ) THEN
c         airunit = '"Bq/m3"' !March 10, 2001 jgd units format correction
c         depunit = '"Bq/m2/yr"' !March 10, 2001 jgd units format correction
         airunit = '"Bq/m^3"'
         depunit = '"Bq/m^2/yr"'
      ELSE
c         airunit = '"kg/m3"' !March 10, 2001 jgd units format correction
c         depunit = '"kg/m2/yr"' !March 10, 2001 jgd units format correction
         airunit = '"kg/m^3"'
         depunit = '"kg/m^2/yr"'
      ENDIF      

      WRITE(fmt,101)numdist
      WRITE(fmt1,102)numdist
      WRITE(fmt2,103)numdist
101   FORMAT('( 9X,', i3, '(f9.1,a1))')   ! Frm
102   FORMAT('( f9.1, a1, 1p, ', i3, '(G9.3,a1))')! FRM1
103   FORMAT('( f9.1, a1, 1p, ', i3, '(a2))') ! FRM2
      
      
      flxleng = LEN_TRIM(fluxtype)
      unileng = LEN_TRIM(airunit)
      
c     Write Out Air Concentrations

      WRITE( io, '(a,a1,a,a5,a,a1,i3,a,i3,a)')
     &'"Air Concentration",','"',fluxtype(1:flxleng),'","",',
     & airunit(1:unileng),',', numdist,',"m",', numdir,',"deg",'      
      WRITE( io, fmt )(dist(i)*1000,',', i = 1, numdist)
      DO idir = 1, numdir
         dir = (360./numdir) * (idir-1)
         sumtst=0.0
         do i = 1,numdist
         sumtst=sumtst+airconc(idir,i)
         enddo
         if(sumtst.gt.0.0) then
          WRITE(io, fmt1)dir,',',(airconc(idir,i),',', i = 1, numdist)
         else
           IF(prtzro.eq.1) then
           WRITE(io, fmt2)dir,',',('0,', i = 1, numdist)
           endif
         endif
      ENDDO

c     Write Out Deposition Rate
      unileng = LEN_TRIM(depunit)
      
      WRITE( io, '(a,a1,a,a12,a,a1,i3,a,i3,a)')
     &'"Deposition Rate",','"',fluxtype(1:flxleng),'","total",', 
     & depunit(1:unileng),',', numdist, ',"m",', numdir,',"deg",'      
      WRITE( io, fmt )(dist(i)*1000,',', i = 1, numdist)
      DO idir = 1, numdir
         dir = (360./numdir) * (idir-1)
         sumtst=0.0
         do i = 1,numdist
         sumtst=sumtst+depconc(idir,i)
         enddo
         if(sumtst.gt.0.0) then
          WRITE( io, fmt1 )dir,',',(depconc(idir,i),',', i=1, numdist)
         else
          IF(prtzro.eq.1) then
          WRITE( io, fmt2 )dir,',',('0,', i=1, numdist)
          endif
         endif
      ENDDO

c     Write Out External Dose (Not Calculated

c      WRITE( io, '(a)')
c     &'"External Dose","Sv",0,"m",0,"m",'
      
      RETURN
      
      END
            
      
C
C     AIRH.FOR
C
C
C       RAPS ATMOSPHERIC PATHWAY RUNTIME COMPONENT 
C       ------------------------------------------
C       Developed by J.G.Droppo, Version May 12, 1994
C       Modified for Framework by Chrisitan Fosmire August 16, 1996 
C       ------------------------------------------
C      IONS
C     May 12, 1994 / Smooth dispersion curve interpolation /JGD
c     July 3, 1994 / Change name for HAZ interface with no
c                    code changed/jgd
C     October 21, 1995 / define PL for pol. case 6
C
C
C     INPUTS:
C
C     ICLASS =   CONTAMINANT CLASS (1 TO 7)               (I)
C     ICLASS =   SURFACE EMISSION CONTAMINANT CLASS (1 TO 7)  (I)
C  0 -  initialize arrays
C  1 -  particles (radius = 7.5), in surface soil   
C  2 -  particles (radius = 3.0), in surface soil   
C  3 -  particles (radius = 0.3), in surface soil   
C  4 -  depositing gas, moderate surface resistance of gas             
C  5 -  non-depositing gas of gas             
C  6 -  gas emission/particle deposition of gas (same as 3)
C  7 -  depositing gas, zero surface resistance. of gas
C             
C     RATE  =   EMISSION FACTOR OF CONTAMINANT IN ICLASS       (F)
C     RAPS/MEPAS UNITS input    output   
C     radioactive..... pCi/s .. pCi/m3
C     non-radioactive.. mg/s ... mg/m3
C
C     DIST  =   DOWNWIND DISTANCE (KM)                      (F)
C     IDIR  =   DIRECTION INDEX (1-16 FOR CLOCKWISE FROM N) (I)
C     DCON  =   DECAY CONSTANT (PER YEAR)                   (F)
C
C     OUTPUTS OF CONTAMINANT CONCENTRATIONS:
C
C     ACONC =   OUTDOOR AIR 
C     BCONC =   INDOOR AIR
C     SCONC =   SURFACE DEPOSITION

C      SUBROUTINE AIRA (ICLASS,ERAT,XDIS,XDIR,ACONC,BCONC,SCONC)
C      /JGD/July 3, 1994
      SUBROUTINE AIRH  (ICLASS,ERAT,XDIS,XDIR,DCON,ACONC,BCONC,SCONC)

c     Changed includes and variable definitions
      
      IMPLICIT NONE
      
      INCLUDE  'parm.inc'
      INCLUDE  'apath.inc'
      
      INTEGER  iclass, idir, wd, pl, kds, kdb
      REAL     xdis, dist, xdir, dism, aconc, sconc, erat, bconc,
     &         dcon
      
      
      IF( ICLASS .NE. 0 ) THEN
         DIST = XDIS
         IF( DIST .LT. XMIN ) DIST = XMIN
         IF( DIST .GT. XMAX ) DIST = XMAX
         IDIR=XDIR
         WD = IDIR
         PL = ity(iclass)

C        DISTANCE INTEPOLATION
         CALL DISTI( DIST, KDS, KDB, DISM )
         IF ( KDS .LT. 1 ) KDS = 1
         IF ( KDS .GE. NDIST )  KDS = NDIST
         IF ( KDS .GE. NDIST ) THEN
            KDS = NDIST
            ACONC = AIRC(WD,KDS,PL)
            SCONC = SURC(WD,KDS,PL)
         ELSE
            CALL DISINT( AIRC(WD,KDS,PL), AIRC(WD,KDS+1,PL),
     +                   XKM(KDS), XKM(KDS+1), DISM, ACONC)
            CALL DISINT( SURC(WD,KDS,PL), SURC(WD,KDS+1,PL),
     +                   XKM(KDS), XKM(KDS+1), DISM, SCONC)
         ENDIF

         ACONC = ERAT * ACONC
         SCONC = ERAT * SCONC
c        decay in transit not in this version/jgd/July 3, 1994
         dcon=dcon * 1.0
C        ACCOUNT FOR DECAY DURING TRAVEL
c        commented out / jgd November 22, 1992 because
c        ubr, the mean wind speed is not being written to "air" file
c        IF( DCON.GT.0.0 .AND. UBR.GT.0.0 ) THEN
c             ACONC=ACONC*EXP(DIST/1000./UBR*DCON)
c             SCONC=SCONC*EXP(DIST/1000./UBR*DCON)
c        ENDIF
C        IF (KDB.GE.NDIST) THEN
C           KBS = NDIST
C           BCONC = ERAT*AIRB(WD,KDB,PL)
C        ELSE
C           A = (DIST-XKM(KDB))/(XKM(KDB+1)-XKM(KDB)) 
C           BCONC = ERAT*(AIRB(WD,KDB,PL)*(1-A)+AIRB(WD,KDB+1,PL)*A)
C        ENDIF
         IF (KDB .LT. 1) KDB = 1
         IF (KDB .GE. NDIST) KDB = NDIST
         IF (KDB .GE. NDIST) THEN
            KDB = NDIST
            BCONC = AIRB(WD,KDB,PL)
         ELSE
            CALL DISINT( AIRB(WD,KDB,PL), AIRB(WD,KDB+1,PL),
     +                   XKM(KDB), XKM(KDB+1), DIST, BCONC)
         ENDIF
         BCONC = ERAT*BCONC
      ELSE
C........CALL TO INITIALIZE AIR PATH
         CALL INIT
      
      ENDIF
      
      RETURN
      
      END

      SUBROUTINE DISTI(DIST,KDS,KDB,DISM)
c  
      IMPLICIT NONE
      
      INCLUDE  'parm.inc'
      INCLUDE  'apath.inc'
      
      INTEGER  kdb, id, kds
      REAL     dist, dism     


      IF( NUMC .EQ. 0) THEN
         KDB = ALOG((DIST-XB1)/XC1)
      ELSE

c             IF(DIST.LE.XMID) THEN
c                  KDB = ALOG((DIST-XB1)/XC1)
c             ELSE
c                  KDB = ALOG((DIST-XB)/XC) + NUMC
c             ENDIF

         kdb=1
         DO id = ndist, 1, -1
            IF( dist .LE. xkm(id) ) kdb = id
         ENDDO

         IF( dist .gt. xkm(12) ) kdb = 12

      ENDIF

      IF( KDB .LT. 1 ) KDB = 1
      
      DISM = 0.100
      
      IF ( DIST .LT. DISM ) THEN
         
         IF(NUMC .EQ. 0 ) THEN
            KDS = ALOG((DISM - XB1) / XC1)

c             ELSE
c                  IF(DISM.LE.XMID) THEN
c                      KDS = ALOG((DISM-XB1)/XC1)
c                  ELSE
c                      KDS = ALOG((DISM-XB)/XC) + NUMC
c                  ENDIF

         ENDIF
      ELSE
         DISM = DIST
         KDS = KDB
      ENDIF
      
      IF( KDS .LT. 1 ) KDS = 1

      RETURN

      END
c
c
        SUBROUTINE INIT
c
C       RAPS ATMOSPHERIC PATHWAY RUNTIME SUBROUTINE 
C       -------------------------------------------
C       Developed by J.G.Droppo, Version 01-19-88
c       Modified for Framework by Christian Fosmire, August 16, 1996 
C       -------------------------------------------
c
      IMPLICIT NONE
      
      INCLUDE  'parm.inc'
      INCLUDE  'apath.inc'
      
      INTEGER  i, j, numr, pl, ds, idist, wd
      
      CHARACTER*4 Atitl1(20)
      
C.......READ DATA FROM INPUT FILE 
      READ(NAIN,200) (ATITL1(I),I=1,20)
c        READ(NAIN,201) NDIST,NUMC,NSC,NTY,UBR
c       ubr not in current version / jgd November 22, 1992
      READ(NAIN,201) NDIST,NUMC,NSC,NTY
      READ(NAIN,202) XMIN,XMID,XMAX
      READ(NAIN,202) (QNN(I),I=1,NTY)
c      READ(NAIN,203) ((AVD(I,J),J=1,6),I=1,4)
      READ(NAIN,203) ((AVD(I,J),J=1,6),I=1,5)
 200  FORMAT(20A4)
 201  FORMAT(4I5)
 202  FORMAT(8G10.3)
 203  FORMAT(1X,6G10.3)
c
C.......COMPUTE DISTANCE CONSTANTS FOR XMAX, XMID, AND XMIN . . .
      CALL DEFDIST( XMAX, XMIN, NDIST, XB1, XC1 )
CWW        WRITE(*,*)' DISTANCES (KM)'
      IF(NUMC.LE.0) THEN
C.......NO VALLEY FLOW
         DO 5 I = 1,NDIST
            XKM(I) = XC1 * EXP(I * 1.) + XB1
CWW             WRITE(*,*) I,XKM(I)
5        CONTINUE
      ELSE
C.......VALLEY CHANNEL
         NUMR = NDIST - NUMC
         CALL DEFDIST( XMID, XMIN, NUMC, XB1, XC1 )
CWW             WRITE(*,*)' WITHIN VALLEY DISTANCES (KM)'
         DO 6 I = 1, NUMC
            XKM(I) = XC1 * EXP(I * 1.) + XB1
CWW                  WRITE(*,*) I,XKM(I)
 6       CONTINUE
CWW             WRITE(*,*)' BEYOND VALLEY DISTANCES (KM)'
c        Correct calculating distance beyond valley /cjf/ May 25, 1998
c         CALL DEFDIST( XMAX, XMID, NUMR, XB, XC )
         CALL DEFDIST( XMAX, XMID, NUMR+1, XB, XC)
c         DO 7 I = NUMC, NDIST
         DO 7 I = 2, NUMR+1
c            XKM(I) = XC * EXP(I * 1.) + XB
            XKM(numc+i-1) = XC*EXP(i*1.) + XB
CWW                  WRITE(*,*) I,XKM(I)
 7       CONTINUE
      ENDIF
c
C.......NOTE: THIS VERSION FILLS THE ZERO DEPOSITION VALUES AND
C.......Earlier Version EQUATEd CLASSES 3 AND 6, JGD 011788
C.......This version has separate Class 6, Ambient Particle Deposition
c       ! March 15, 2001 JGD

      DO 18 PL = 1, NTY
c         IF( PL .NE. 6 ) THEN ! March 15, 2001 JGD
            DO 12 DS = 1, NDIST
               READ(NAIN,100,END=12,ERR=12)
     +              ITY(PL),(AIRC(WD,DS,PL),WD=1,8)
               READ(NAIN,100,END=12,ERR=12)
     +              IDIST,(AIRC(WD,DS,PL),WD=9,16)
12          CONTINUE
            DO 13 DS = 1, NDIST
               READ(NAIN,100,END=13,ERR=13)
     +              ITY(PL),(AIRB(WD,DS,PL),WD=1, 8)
               READ(NAIN,100,END=13,ERR=13)
     +              IDIST,  (AIRB(WD,DS,PL),WD=9,16)
13          CONTINUE
            DO 15 DS = 1, NDIST
               IF( PL .EQ. 5) THEN
                  DO 14 WD = 1, 16
14                SURC(WD,DS,PL)=-99.
               ELSE
                  READ(NAIN,100,END=15,ERR=15)
     +                 ITY(PL),(SURC(WD,DS,PL),WD=1,8)
                  READ(NAIN,100,END=15,ERR=15)
     +                 IDIST,(SURC(WD,DS,PL),WD=9,16)
               ENDIF
15          CONTINUE
c         ELSE     ! March 15, 2001 JGD
c                next line added/enddo structure/jgd/October 21, 1995
c            ity(6)=3 ! March 15, 2001 JGD
c            DO DS = 1, NDIST   ! March 15, 2001 JGD
c               DO WD = 1, 16  ! March 15, 2001 JGD
c                  AIRC(WD,DS,6) = AIRC(WD,DS,3)   ! March 15, 2001 JGD
c                  AIRB(WD,DS,6) = AIRB(WD,DS,3) ! March 15, 2001 JGD
c                  SURC(WD,DS,6) = SURC(WD,DS,3)  ! March 15, 2001 JGD
c               ENDDO  ! March 15, 2001 JGD
c            ENDDO ! March 15, 2001 JGD
c         ENDIF    ! March 15, 2001 JGD
18    CONTINUE
      DO 20 WD =  1, 16
         DO 20 DS = 1, NDIST
            DO 20 PL = 1, NTY
               AIRC(WD,DS,PL)=10.**(AIRC(WD,DS,PL))
               AIRB(WD,DS,PL)=10.**(AIRB(WD,DS,PL))
20    SURC(WD,DS,PL)=10.**(SURC(WD,DS,PL))
      ICALL =  1

      RETURN

c100     FORMAT(1X,I2,8F7.3)
c       jgd/May 11, 1994
100     FORMAT(2X,I1,8F7.3)

      END

      SUBROUTINE DEFDIST(XMAX,XMIN,NUMD,XB,XC)
      IMPLICIT NONE
      
      INTEGER  numd
      REAL     xb, xmax, xmin, xc

      XB = (XMIN * EXP(NUMD - 1.) - XMAX) / (EXP(NUMD - 1.) - 1)
      XC = (XMAX - XB) / EXP(NUMD * 1.)
      
      RETURN
      
      END

C     MEPAS VERSION 3:  DISINT.FOR          Version Date: May 11, 1994
C     Copyright 1994 by Battelle Memorial Institute. All rights reserved.
C
      SUBROUTINE DISINT(C1,C2,X1,X2,XN,CN)
      IMPLICIT NONE
      
      REAL  c1, c2, x1, x2, xn, cn, a, b, c

      IF( (C1. EQ. 0.0) .OR. (C2 .EQ. 0.0) ) THEN
         CN = 0
      ELSEIF( XN .EQ. X1 ) THEN
         CN = C1
      ELSEIF( XN .EQ. X2 ) THEN
         CN = C2
      ELSE
         A = ALOG10(X2) / ALOG10(X1)
         B = (ALOG10(C2) - ALOG10(C1) * A) / (1.0 - A)
         C = (ALOG10(C1) - B) / ALOG10(X1)
         CN = 10.**(C * ALOG10(XN) + B)
      ENDIF
      
      RETURN
      
      END

      SUBROUTINE WR_HEAD( io, soufn, souleng )
C----------------------------------------------------------------------
C     WR_HEAD
C     
C     Christian J Fosmire
C
C     Date: 8/16/96
C     
C     Description:   Write a header for the output file
C
C----------------------------------------------------------------------
c     Revisons
c     March 10, 2001 JGD new version header..
c
      IMPLICIT NONE
      
      CHARACTER*8 soufn
      CHARACTER*12 outname
            
      INTEGER  iyr, imon, iday, ihr, imin, isec, ihun, io, souleng

c     Get output name

      outname = soufn(1:souleng)//'.ATO'
      
c     Write out number of lines in header

      WRITE( io, '(i1,a1)' )6,','
      
c     Get Time and Date of Run

      CALL GETDAT(IYR,IMON,IDAY)
      CALL GETTIM(IHR,IMIN,ISEC,IHUN)

c     Write Out Header

      WRITE(io,'(a)')
     &'"=============================================================",'
      WRITE(io,'(a)')
     &'" Rapscd and FrmConc Modules, Version - 8 May 2001",'
      WRITE(io,'(a,a,a2)')
     &'" Run: ","',soufn,'",'
      WRITE(io,101)
     &'" Run Performed:",',imon,iday,iyr,ihr,imin,isec
      WRITE( io, '(a,a,a,a)')
     &'" Output Filename:",', '"', outname,'",'
      WRITE( io, '(a)')
     &'"=============================================================",'

101   FORMAT(a,'"',2(i2,'/'),i4,'",','"',2(i2,':'),i2,'",')
      
      RETURN
      
      END
      
      LOGICAL FUNCTION COMPSTR ( oldstr, newstr)
C-----------------------------------------------------------------------
C     COMPSTR
C
C     Christian Fosmire
C     Pacific Northwest National Lab
C
C     Date: 9/10/97
C
C     Description:   Determines if two strings are equal without
C                    regard to case (upper or lower)
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      
      CHARACTER*(*) oldstr, newstr
      INTEGER*4   lenstr, lenstr1, charindex, i, charindex1
      CHARACTER*1 chkchar, chkchar1
      
      
      lenstr = LEN_TRIM(oldstr)
      lenstr1 = LEN_TRIM(newstr)
      
      COMPSTR = .TRUE.
      
c     Check each character in string                                
      DO i = 1, MIN0(lenstr,lenstr1)
                                           
         READ(oldstr(i:i),'(a1)') chkchar
         READ(newstr(i:i),'(a1)') chkchar1
         charindex = ICHAR(chkchar)
         charindex1 = ICHAR(chkchar1)                      
         
         IF ( charindex .EQ. charindex1 ) THEN
            CYCLE
         ELSE
c     Determine if lower case letter
         
            IF( charindex .GE. 97 .AND. charindex .LE. 122 ) THEN

c     Convert to upper case letter

               chkchar = CHAR(charindex - 32 )
            ENDIF
            
            IF( charindex1 .GE. 97 .AND. charindex .LE. 122 ) THEN
               chkchar1 = CHAR(charindex1 - 32 )
            ENDIF
            
            IF( chkchar .NE. chkchar1 ) THEN
               COMPSTR = .FALSE.
               EXIT
            ENDIF
         ENDIF
      ENDDO                       


      RETURN
      
      END
      
c     New Subroutine / September 16, 1998 CJF
      SUBROUTINE LAT_INTG( numdist, numdir, dist, airc, depc, areasize )
C-----------------------------------------------------------------------
C     LAT_INTG
C
C     Christian Fosmire
C     Pacific Northwest National Lab
C     
C     Date: September 16, 1998
C
C     Description:   Calculates the Laterial Integration portion of the
C                    New Area Source 
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      
      INTEGER  numdist, numdir
      INTEGER  isd, jdir, jdp1, jdp2, jdp3, jdm1, jdm2, jdm3, intw
      INTEGER  ier, strtdist
      
      REAL     dist(numdist), airc(numdir, numdist), 
     &         depc(numdir,numdist), Area_Wght1(4), Area_Wght2(4),
     &         Area_Wght3(4), Area_Wght4(4)
      REAL     airt [ALLOCATABLE](:,:)
      REAL     dept [ALLOCATABLE](:,:)

      REAL     areasize, rad, facintr, Wght1, Wght2, Wght3, Wght4, 
     &         disnorm
      

c     Allocate space for temporary arrays
      ALLOCATE (airt(numdir,numdist), dept(numdir,numdist), STAT = ier)
      IF( ier .NE. 0 ) THEN
         WRITE(*,*) 'Unable to Allocate Arrays in LAT_INTG'
         WRITE(*,*) 'Error Number = ', ier
         WRITE(*,*) 
     &      'Insufficient storage space for data; aborting program'
         STOP 1
      ENDIF
                  
c     Set up Area Weights Factors for a circle
      Area_Wght1(1) = 0.248
      Area_Wght2(1) = 0.212
      Area_Wght3(1) = 0.124
      Area_Wght4(1) = 0.04
      
      Area_Wght1(2) = 0.295
      Area_Wght2(2) = 0.242
      Area_Wght3(2) = 0.11
      Area_Wght4(2) = 0.0
      
      Area_Wght1(3) = 0.427
      Area_Wght2(3) = 0.286
      Area_Wght3(3) = 0.0
      Area_Wght4(3) = 0.0
      
      Area_Wght1(4) = 1.0
      Area_Wght2(4) = 0.0
      Area_Wght3(4) = 0.0
      Area_Wght4(4) = 0.0
      
      rad = SQRT(Areasize/3.14159)

c     Loop through all distances and directions
      strtdist = 0
      
      DO isd = 1, numdist 

c        Skip all distance less than the radius as already done         
         IF( dist(isd)*1000 .LT. rad ) THEN
            strtdist = isd
            CYCLE
         ENDIF
         
         DO jdir = 1, numdir

            jdp1 = jdir + 1
            IF( jdp1 .GT. 16 ) jdp1 = jdp1 - 16
            jdp2 = jdir + 2
            IF( jdp2 .GT. 16 ) jdp2 = jdp2 - 16
            jdp3 = jdir + 3
            IF( jdp3 .GT. 16 ) jdp3 = jdp3 - 16
            
            jdm1 = jdir -1
            IF( jdm1 .LT. 1 ) jdm1 = jdm1 + 16
            jdm2 = jdir - 2
            IF( jdm2 .LT. 1 ) jdm2 = jdm2 + 16
            jdm3 = jdir - 3
            IF( jdm3 .LT. 1 ) jdm3 = jdm3 + 16
            
c           determine distance over radius of source
            disnorm = dist(isd)*1000/rad
            
            IF( disnorm .LT. 1 ) THEN

C              Point is within the area
               intw = 1
            
            ELSEIF( disnorm .LT. 1.2 ) THEN

C              close to the area is the source is spilt into four sections
               intw = 1
               facintr = (disnorm - 1.0)/(1.2 - 1.0)
               
            ELSEIF( disnorm .LT. 1.8 ) THEN

C              farther away there are less sections of the source
               intw = 2
               facintr = (disnorm - 1.2)/(1.8 - 1.2)

            ELSEIF( disnorm .LT. 5.1 ) THEN

C              area is spilt into two sections
               intw = 3
               facintr = (disnorm - 1.8)/(5.1 - 1.8)
           
            ELSE

C              area is within one sector

               intw = 3
               facintr = 1
 
            ENDIF
            
c           Calculate area weights for each sector
            Wght1 = Area_Wght1(intw) * (1.0 - facintr) + 
     &               Area_Wght1(intw+1) * facintr
            Wght2 = Area_Wght2(intw) * (1.0 - facintr) + 
     &               Area_Wght2(intw+1) * facintr
            Wght3 = Area_Wght3(intw) * (1.0 - facintr) +
     &               Area_Wght3(intw+1) * facintr
            Wght4 = Area_Wght4(intw) * (1.0 - facintr) + 
     &               Area_Wght4(intw+1) * facintr
                                        
            airt(jdir,isd) = Wght1 * airc(jdir,isd) + 
     &       Wght2 * airc(jdp1,isd) + Wght2 * airc(jdm1,isd) +
     &       Wght3 * airc(jdp2,isd) + Wght3 * airc(jdm2,isd) +
     &       Wght4 * airc(jdp3,isd) + Wght4 * airc(jdm3,isd)
     
            dept(jdir,isd) = Wght1 * depc(jdir,isd) + 
     &       Wght2 * depc(jdp1,isd) + Wght2 * depc(jdm1,isd) +
     &       Wght3 * depc(jdp2,isd) + Wght3 * depc(jdm2,isd) +
     &       Wght4 * depc(jdp3,isd) + Wght4 * depc(jdm3,isd)
         
         ENDDO
      ENDDO

c     Move to Save Array from Temporary Array
      
      DO isd = strtdist+1, numdist
         DO jdir = 1, numdir
            airc(jdir,isd) = airt(jdir,isd)
            depc(jdir,isd) = dept(jdir,isd)
         ENDDO
      ENDDO                                  
      
      DEALLOCATE( airt, dept, STAT = ier )
      IF ( ier .NE. 0 ) THEN
         WRITE(*,*) 
     &     'Error occurred trying to deallocate arrays in LAT_INTG'
         WRITE(*,*) 'Error Number - ', ier
         WRITE(*,*)
     &       'Aborting program'
         STOP 1
      ENDIF
      
      RETURN
      
      END
      
c     New suboroutine added /September 16, 1998 CJF
      SUBROUTINE VERT_INT( numdist, numdir, dist, airc, depc, areasize ) 
C-----------------------------------------------------------------------
C     VERT_INT
C
C     Christian Fosmire
C     Pacific Northwest National Lab
C
C     Date: September 16, 1998
C
C     Description:   Calculates the Vertical Integration portion of the
C                    New Area Source
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      
      INTEGER  numdist, numdir
      INTEGER  jdir, isd, istab
      
      REAL     dist(numdist), airc(numdir,numdist), depc(numdir,numdist)
      REAL     rad, acfac, areasize
      
c     Assume stability class D for vertical correction      
      istab = 4                                       

      rad = SQRT(areasize/3.14159)
      
      DO jdir = 1, numdir
         DO isd = 1, numdist
            
            IF( dist(isd)*1000 .GE. rad ) THEN
               
               acfac = 1.0
               CALL VERTAREA( dist(isd), rad, istab, acfac )
               airc(jdir,isd) = airc(jdir,isd) * acfac
               depc(jdir,isd) = depc(jdir,isd) * acfac
               
            ENDIF
         
         ENDDO
      ENDDO
      
      RETURN
      
      END
      
c     added subroutine /September 16, 1998 CJF
      SUBROUTINE  VERTAREA( xklm, rad, stab, acfac )
C-----------------------------------------------------------------------
C     VERTAREA
C
C     Christian Fosmire, J.G. Droppo
C     Pacific Northwest National Lab
C
C     Date:    September 16, 1998
C
C     Description:   Returns the integration correction factor for an
C                    area source assuming a 1) box model for minimum
C                    vertical dispersion and 2) dispersion occurs as 1/x
C                    within the area (where X is distance to point of
C                    emission).
C     Revision Log: 
C     24 Mar 2001 JGD changed variable names to make code more understandable
C                     corrected and added comment notes at end of code
C
C
C
C
C
C-----------------------------------------------------------------------
      IMPLICIT NONE                
      
      INTEGER  stab, ifst, pts_on_diam, ini
      
      REAL xminc(7)
      REAL xklm, rad, acfac, xloc, radcc, dist_edge, sum_invx, xt, xi 

c     Xminc is the approx. distance at which sigma z = 1 m using
c     Brigg's rural dispersion coefficients
      
      xminc(1) = 5.0
      xminc(2) = 8.3
      xminc(3) = 12.5
      xminc(4) = 16.7
      xminc(5) = 33.3
      xminc(6) = 62.5
      xminc(7) = 62.5

C     Convert distance to m
      xloc = xklm* 1000
      
C     Treat a source with a radius < 5 m as a point
      IF( rad .LT. 5.0 ) THEN
         acfac = 1.0
      ELSE
c        Determine radius and distance from edge of source
         IF( xloc .LT. rad ) THEN
            !Return already done ealier
            RETURN
         ELSE
            radcc = rad
            dist_edge = xloc - radcc
         ENDIF
         
c        Define variables for integration loop based on 
c        summing points at 1 meter spacing across the area
      
         ifst = 0
         pts_on_diam = int(radcc*2 - 0.5)
         sum_invx = 0
         xt = xminc(stab)
         
c        Integrate across the area
         
         DO ini = ifst, pts_on_diam
            xi = ini + dist_edge + 0.5
            IF( xi .LT. xt ) xi = xt
            sum_invx = sum_invx + 1/xi
         ENDDO

c     Value that RAPSCD computes:      Conca = factors/(dist_edge + radcc)
c     Value with vertical area corr:   Concb = factors*(sum(1/x)/Npts)
c     Derive concentration correction factor:
c           acfac = concb/conca
c           acfac = (sum(1/x)/Npts*(dist_edge + radcc)

      acfac=sum_invx/(pts_on_diam+1)*(dist_edge+(pts_on_diam+1)/2.)

c     notes:
c     +1's are needed because count starts at zero
c     radius is defined as a real 1/2 of the interger diameter used
c     in integration because a relative factor is computed for that
c     specific diameter 24Mar2001 JGD
c     
      ENDIF
      
      RETURN
      
      END     

c     New Subroutine added/ September 16, 1998 CJF      
      SUBROUTINE INN_AREA( numdist, numdir, dist, airc, depc, con_cir, 
     &                     dep_cir, radius )
C-----------------------------------------------------------------------
C
C     INN_AREA
C
C     Christian Fosmire
C
C     Date: September 16, 1998
C
C     Description:   Calculates the concentration and deposition for 
C                    receptors located within the area.
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      
      INTEGER  numdist, numdir
      INTEGER  idis, idir, jdir
      
      REAL     dist(numdist), airc(numdir, numdist), 
     &         depc(numdir, numdist), con_cir(numdir), dep_cir(numdir)
      REAL     radius          
      REAL     CIR_ANG, LAT_FACT, VER_FACT 
      REAL     del_dir, new_theta, lat_wght, vert_wght, new_conc,
     &         new_dep, dtr
     
      dtr = 3.14159/180.0
      
      DO idis = 1, numdist
         
c        Exit Subroutine when distance beyond radius
         
         IF( dist(idis)*1000.0 .GE. radius ) THEN
            RETURN
         ENDIF

c        Loop through the directions
         
         DO idir = 1, numdir

c        zero out concentration for this receptor
            airc(idir,idis) = 0.0
	      depc(idir,idis) = 0.0
            
            DO jdir = 1, numdir
               
               del_dir = FLOAT(idir - jdir) * 22.5
               IF( del_dir .LT. 0 ) THEN
                  del_dir = 360.0 + del_dir
               ENDIF
               
               new_theta = CIR_ANG( dist(idis), radius, del_dir )
               
               lat_wght = LAT_FACT( new_theta )
               
               vert_wght = VER_FACT( dist(idis), radius, del_dir, 
     &                               new_theta )
               
               new_conc = con_cir(jdir) * lat_wght * vert_wght
               new_dep = dep_cir(jdir) * lat_wght * vert_wght
               
               airc(idir,idis) = airc(idir,idis) + new_conc
               depc(idir,idis) = depc(idir,idis) + new_dep
            
            ENDDO
         ENDDO
      ENDDO
      
      RETURN
      
      END 

c     New Function added/ September 16, 1998 CJF
      REAL FUNCTION CIR_ANG( dist, radius, ang )
C-----------------------------------------------------------------------
C
C     CIR_ANG
C
C     Christian Fosmire
C
C     Date: September 16, 1998
C
C     Description:   Calculates the angle (CIR_ANG) on the edge of a 
C                    circular area for a line that runs north-south 
C                    through a point within the area that is at 
C                    distance dist from the center of the area and at 
C                    angle ang from north.  
C                                           
C-----------------------------------------------------------------------
      IMPLICIT NONE
      
      REAL  dist, radius, ang, theta, dtr, distm

c     variable to convert degrees to radians
      dtr = 3.14159/180.0
      
C     covert distance to meter
      distm = dist*1000.0
      
C     check to make sure valid angle            
      
      IF( ang .LT. 0 .OR. ang .GT. 360 ) THEN
         WRITE(*,*) 'Error in FUNCTION CIR_ANG - invalid angle'
         WRITE(*,*) 'Angle must be between 0 and 360 degrees.'
         WRITE(*,*) 'Angle given is ',ang
         STOP 1
      ENDIF
            
c     Calculate angle 
      
      theta = ASIN(distm*SIN(ang*dtr)/radius) / dtr
      
      IF( ang .GE. 0 .AND. ang .LE. 90 ) THEN
         CIR_ANG = theta
      ELSEIF( ang .GT. 90 .AND. ang .LE. 270 ) THEN
         CIR_ANG = 180 - theta
      ELSE
         CIR_ANG = 360 + theta
      ENDIF
      
      RETURN
      
      END   
      
      
c     New Function added/ September 16, 1998      
      REAL FUNCTION LAT_FACT( ang )
C-----------------------------------------------------------------------
C     
C     LAT_FACT
C
C     Christian Fosmire
C
C     Date: September 16, 1998
C
C     Description:   Calculates the lateral fraction of the area at the
C              edge of the area.  Fraction is determined by the 
C              angle from the direction the wind is blowing 
C              towards.  Laterial fraction is based on a best-fit
C              to a third order polynomial to the fraction associated 
C              with 0, 22.5, 45, 67.5, and 90 degrees.  
C
C              Equation is y = 7E-7x^3 - 1E-4x^2 + 0.0001x + 0.2482
C
C              R^2 for fit is 0.9999
C
C-----------------------------------------------------------------------  
      IMPLICIT NONE

      REAL  ang, x

c     Modify the angle so between 0 and 90
      
      IF( ang .GT. 90 .AND. ang .LE. 270 ) THEN
         x = ABS(180 - ang)
      ELSEIF( ang .GT. 270 .AND. ang .LE. 360 ) THEN
         x = ABS(360 - ang)
      ELSEIF( ang .GE. 0 .AND. ang .LE. 90 ) THEN
         x = ang
      ELSE
         WRITE(*,*) 'Invalid angle sent to LAT_FACT'
         WRITE(*,*) 'Angle is ', ang
         WRITE(*,*) 'Must be between 0 and 360 degrees.'
         STOP 1
      ENDIF
        
c     Apply equation

      LAT_FACT = 7.0E-7 * x*x*x - 1.0E-4 * x*x + 0.0006 * x + 0.248
         
      RETURN
               
      END

c     New function added/ September 16, 1998 CJF      
      REAL FUNCTION VER_FACT( dist, radius, ang, theta )
C-----------------------------------------------------------------------
C
C     VER_FACT
C
C     Christian Fosmire
C
C     Date: September 16, 1998
C
C     Description:   Calculates the vertical integration term for a
C                    point within the area which is located at distance
C                    dist from the center of the source and angle ang 
C                    from the direction of interest.  Theta is the angle
C                    at the edge of the (circular) area of the line 
C                    parrellel to the direction of interest that passes
C                    through the point at distance dist and angle ang.
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      
      REAL  dist, radius, ang, theta, leng, xlim, distm, dtr
      
c     variable to convert degrees to radians
      dtr = 3.14159/180.0      
      
c     xlim is the approximate distance at which sigma z = 1 m using
c     Brigg's rural dispersion coefficient.  At this time only use
C     D stability

      xlim = 16.7

c     convert distance to meters
      distm = dist * 1000.0
      
c     calculate length of line from upwind edge to point

      IF ( COS(ang*dtr) .GT. 0 ) THEN
         leng = radius * COS(theta*dtr) + distm * COS(ang*dtr)               
      ELSEIF( COS(ang*dtr) .LT. 0 ) THEN
         leng = distm * COS(ang*dtr) - radius * COS(theta*dtr) 
      ELSE 
         leng = ABS(radius*COS(theta*dtr))
      ENDIF
      
c     Determine vertical integration based on length
      
      IF( leng .LT. 0 ) THEN
         WRITE(*,*) 'Error in Function VER_FACT.'
         WRITE(*,*) 'Negative length of line calculated.'
         WRITE(*,*) 'Length = ', leng
         STOP 1
      ENDIF
      
      IF( leng .LE. xlim ) THEN
         VER_FACT = 0.5 * leng / xlim
      ELSE
         VER_FACT = 0.5 * LOG(leng) - 0.9076
      ENDIF
      
      RETURN
      
      END
                                           
