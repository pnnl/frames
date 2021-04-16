      SUBROUTINE  RdRSPuff( acute, metyr, metmo, metda, methr, PrgStat )
C-----------------------------------------------------------------------
C     RdRSPuff
C
C     Christian J Fosmire
C     Pacific Northwest National Lab
C     P.O. Box 999
C     Richland, WA 99352
C
C     Date:             March 14, 2000
C
C     Description:      This routine reads in the various variables from 
C                       the run specification file    
C
C     Required Modules: NONE
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      
      INCLUDE        'parm.inc'
      INCLUDE        'const.inc'
      INCLUDE        'difter.inc'
      INCLUDE        'files.inc'
      INCLUDE        'puffs.inc'
      INCLUDE        'shine.inc'          
      INCLUDE        'depos.inc'
      INCLUDE        'rel.inc'
      
      REAL           SETLVEL
      REAL           distb(36), partdia(MaxPBins), partdens(MaxPBins)
      REAL           xcent, ycent, xsourc, ysourc
      
      INTEGER        isrc, i, j, rio, ier 
      INTEGER        i1, is, im, ih
      INTEGER        metyr, metmo, metda, methr
	INTEGER        kind
      
      CHARACTER*2    prgid
      CHARACTER*50   PrgStat      

      LOGICAL        partdisflg, acute

      prgid = 'CR'
      IF ( acute ) prgid = 'AC'
 
c      WRITE(25,*) 'Subroutine Rdrspuff '
     
c     Open Run Specification File

      rio = 55
      
      OPEN( rio,File=rsfile,STATUS='OLD',IOSTAT=ier )
         
      IF (ier .NE. 0 ) THEN   
         WRITE(25,*) 'Error opening File ', rsfile
         WRITE(25,*) 'Error Number - ', ier
         STOP 1
      ENDIF
      
c     Read in minimum wind speed for plume rise

      READ( rio, * ) wndmin    
c      WRITE(25,*)'Minimum Wind Speed for Plume Rise is ', wndmin

c  Sigma y for shift to semi-inifinite cloud shine model

      READ ( rio, * ) chgmod
c      WRITE(25,*)'The shift for semi-infinite cloud is ', chgmod

c  Read in the Tranfer resistances for gas and particle

      READ ( rio, * ) (rtx(i), i = 1, 2)
c      WRITE ( 25, * ) 'The transfer resistances are ', rtx
      
c  Read in the sigma parameterization

      READ ( rio, * ) sigparm
c      WRITE ( 25, * ) 'The sigma parameterization scheme is ',sigparm

c  Read in Number of nodes in E-W, nodes in N-S, and 
c  distance between nodes

      READ( rio, *, IOSTAT = ier) numx, numy, delxy
      
      IF ( ier.NE.0 ) THEN
         WRITE ( PrgStat(1:2), '(a2)'  )    prgid
         WRITE ( PrgStat(4:13), '(a10)' )   'RDRSPuff'
         WRITE ( PrgStat(15:18), '(i4)' )   ier
         WRITE ( PrgStat(20:29), '(a10)' )  'rsfile'
         WRITE ( PrgStat(31:50), '(a20)' )  'variable numx'
      ENDIF

c      WRITE(25,*) 'The number of nodes in the east-west = ',numx
c      WRITE(25,*) 'The number of nodes in the north-south = ', numy
c      WRITE(25,*) 'The distance bewteen nodes is ',delxy
               
c  Normalized maximum distance for puff concentration > 0

      READ ( rio, *, IOSTAT = ier ) radcnst

      IF( ier.NE.0 ) THEN
         WRITE ( PrgStat(1:2),'(a2)'  )   prgid
         WRITE ( PrgStat(4:13),'(a10)' )  'RDRSPuff'
         WRITE ( PrgStat(15:18),'(i4)' )  ier
         WRITE ( PrgStat(20:29),'(a10)' ) 'rsfile'
         WRITE ( PrgStat(31:50),'(a20)' ) 'variable radcnst'
      ENDIF

c      WRITE(25,*) 'Max Puff Radius Constant = ',radcnst     

c  number of puffs per hour

      READ ( rio, *, IOSTAT = ier ) nph      

      IF( ier.NE.0 ) THEN
         WRITE ( PrgStat(1:2),'(a2)'  )    prgid
         WRITE ( PrgStat(4:13),'(a10)' )  'RDRSPuff'
         WRITE ( PrgStat(15:18),'(i4)' )   ier
         WRITE ( PrgStat(20:29),'(a10)' ) 'rsfile'
         WRITE ( PrgStat(31:50),'(a20)' ) 'variable nph'
      ENDIF

c      WRITE(25,*)'The number of puffs per hr = ',nph

c  leading coef for time-based sigma y calculations for t > 30 min

      READ ( rio, *, IOSTAT = ier ) sy_cnst
      
      IF( ier.NE.0 ) THEN
         WRITE ( PrgStat(1:2),'(a2)'  )    prgid
         WRITE ( PrgStat(4:13),'(a10)' )  'RDRSPuff'
         WRITE ( PrgStat(15:18),'(i4)' )   ier
         WRITE ( PrgStat(20:29),'(a10)' ) 'rsfile'
         WRITE ( PrgStat(31:50),'(a20)' ) 'variable sy_cnst'
      ENDIF

c      WRITE(25,*)'The leading coef. for time-based sigy is ',sy_cnst

c  maximum number of integration steps per interval (1-8,10,15,30,60) 

      READ ( rio, *, IOSTAT = ier ) iopdta

      IF( ier.NE.0 ) THEN
         WRITE ( PrgStat(1:2),'(a2)'  )    prgid
         WRITE ( PrgStat(4:13),'(a10)' )  'RDRSPuff'
         WRITE ( PrgStat(15:18),'(i4)' )   ier
         WRITE ( PrgStat(20:29),'(a10)' ) 'rsfile'
         WRITE ( PrgStat(31:50),'(a20)' ) 'variable iopdta'
      ENDIF

c      WRITE(25,*)'The max number of integration steps = ',IOPDTA

c  puff consolidation controls

      READ ( rio, *, IOSTAT = ier )  cln_flg

      IF( ier.NE.0 ) THEN
         WRITE ( PrgStat(1:2),'(a2)'  )    prgid
         WRITE ( PrgStat(4:13),'(a10)' )  'RDRSPuff'
         WRITE ( PrgStat(15:18),'(i4)' )   ier
         WRITE ( PrgStat(20:29),'(a10)' ) 'rsfile'
         WRITE ( PrgStat(31:50),'(a20)' ) 'variable cln_flg'
      ENDIF

      READ ( rio, *, IOSTAT = ier )  cln_crit

      IF( ier.NE.0 ) THEN
         WRITE ( PrgStat(1:2),'(a2)'  )    prgid
         WRITE ( PrgStat(4:13),'(a10)' )  'RDRSPuff'
         WRITE ( PrgStat(15:18),'(i4)' )   ier
         WRITE ( PrgStat(20:29),'(a10)' ) 'rsfile'
         WRITE ( PrgStat(31:50),'(a20)' ) 'variable cln_crit'
      ENDIF

c      WRITE(25,*) 'The Flag for doing puff consolidation = ',cln_flg
c      WRITE(25,*) 'The consolidating puff seperated by ', cln_crit,
c     &' sigmays'       

c  tracking region scale factor

      READ ( rio, *, IOSTAT = ier ) gscale

      IF( ier.NE.0 ) THEN
         WRITE ( PrgStat(1:2),'(a2)'  )    prgid
         WRITE ( PrgStat(4:13),'(a10)' )  'RDRSPuff'
         WRITE ( PrgStat(15:18),'(i4)' )   ier
         WRITE ( PrgStat(20:29),'(a10)' ) 'rsfile'
         WRITE ( PrgStat(31:50),'(a20)' ) 'variable gscale'
      ENDIF

c      WRITE(25,*) 'The tracking region scale factor is ',gscale
                
c  Minimum Concentration constant
      
      READ( rio, *, IOSTAT = ier ) chimin

      IF( ier.NE.0 ) THEN
         WRITE ( PrgStat(1:2),'(a2)'  )    prgid
         WRITE ( PrgStat(4:13),'(a10)' )  'RDRSPuff'
         WRITE ( PrgStat(15:18),'(i4)' )   ier
         WRITE ( PrgStat(20:29),'(a10)' ) 'rsfile'
         WRITE ( PrgStat(31:50),'(a20)' ) 'variable chimin'
      ENDIF

c      WRITE(25,*)'The minimum concentration of interest is ',chimin

c  Determine the minimum and maximum x and y for tracking region
               
      xcent = float(numx+1)/2.0
      ycent = float(numy+1)/2.0

      XSMIN = 1 - (xcent - 1) * Gscale
      YSMIN = 1 - (ycent - 1) * Gscale
      XSMAX = numx + (xcent - 1) * Gscale
      YSMAX = numy + (ycent - 1) * Gscale

c      WRITE(25,*)'The minimum x of tracking region is ',xsmin
c      WRITE(25,*)'The minimum y of tracking region is ',ysmin
c      WRITE(25,*)'The maximum x of tracking region is ',xsmax
c      WRITE(25,*)'The maximum y of tracking region is ',ysmax

c     Number of sources

      READ ( rio, *, IOSTAT = ier ) nsrc
      
      IF ( ier.NE.0 ) THEN
         WRITE ( PrgStat(1:2), '(a2)'  )    prgid
         WRITE ( PrgStat(4:13), '(a10)' )  'RDRSPuff'
         WRITE ( PrgStat(15:18), '(i4)' )   ier
         WRITE ( PrgStat(20:29), '(a10)' ) 'rsfile'
         WRITE ( PrgStat(31:50), '(a20)' ) 'variable nsrc'
      ENDIF

      IF ( nsrc.gt.MaxSrcs ) THEN
         WRITE ( PrgStat(1:2), '(a2)' )     prgid
         WRITE ( PrgStat(4:13), '(a10)' )  'RDRSPuff'
         WRITE ( PrgStat(15:18), '(i4)' )   9999
         WRITE ( PrgStat(31:44), '(a14)' ) 'nsrc too large'
         WRITE ( PrgStat(46:50), '(i5)' )   nsrc
      ENDIF

c      WRITE(25,*)'The total number of sources are ',nsrc

c      WRITE(25,*)'For Each Source'
c      WRITE(25,201)'X','Y','IX','IY','Z','Do Rise','Stk Rad',
c     &             'Stk Flow','ETemp','SigYin','SigZin'
  201 FORMAT(10(1x,a8))
               
      DO isrc = 1, nsrc
ccc   added "kind" to the following list for puff inputs, BAN 30 May 2008
         READ ( rio, *, IOSTAT = ier) 
     &      kind,
     &      xsourc, ysourc, zsourc(isrc), dorise(isrc), 
     &      rstack(isrc), sflow(isrc), stempc(isrc), sigyin(isrc),
     &      sigzin(isrc)                        
      
         IF ( ier .NE. 0 ) THEN
            WRITE ( PrgStat(1:2),'(a2)'  )    prgid
            WRITE ( PrgStat(4:13),'(a10)' )  'RDRSPuff'
            WRITE ( PrgStat(15:18),'(i4)' )   ier
            WRITE ( PrgStat(20:29),'(a10)' ) 'rsfile'
            WRITE ( PrgStat(31:50),'(a20)' ) 'stack parmeters'
         ENDIF

c  Determine x and y position on the grid   

         xsrc(isrc) = (xsourc/delxy) + xcent
         ysrc(isrc) = (ysourc /delxy) + ycent

c         WRITE (25,205) 
c     &      xsourc, ysourc, xsrc(isrc), ysrc(isrc), 
c     &      zsourc(isrc), dorise(isrc), 
c     &      rstack(isrc), sflow(isrc), stempc(isrc), sigyin(isrc),
c     &      sigzin(isrc)                        
  205    FORMAT(5(1x,f7.2),7x,L1,3(1x,f7.2),2(1x,f7.2))

c     Check if src has particle size distribution

         READ( rio, * ) partdisflg
         
c         WRITE(25,*) 'Particle size distribution ', partdisflg

c     Check if reading in distribution

         meandia = 0
         meanden = 0
         IF( partdisflg ) THEN

            READ( rio, * )numpardis(isrc)
c            WRITE(25,*) 'Number of particle classes = ', numpardis(isrc)
         
            READ( rio, * )
     &            (partdia(i), i = 1, numpardis(isrc))
c            WRITE(25,*)'Particle Diameters:'
c            WRITE(25,*) (partdia(i), i = 1, numpardis(isrc))
            
            READ( rio, * )
     &            (partdens(i), i = 1, numpardis(isrc))
c            WRITE(25,*)'Particle Density:'
c            WRITE(25,*)(partdens(i), i = 1, numpardis(isrc))            
            
c     Calculate settling velocities
            
            DO i = 1, numpardis(isrc)
               setvel(isrc,i) =  SETLVEL( partdia(i), partdens(i) )
               meandia = meandia + (partdia(i)/FLOAT(numpardis(isrc)))
               meanden = meanden + (partdens(i)/FLOAT(numpardis(isrc)))
            ENDDO

c            WRITE(25,*)'Settling Velocities:'
c            WRITE(25,*)(setvel(isrc,i), i = 1, numpardis(isrc))
        
         ELSE
         
            numpardis(isrc) = 1
            setvel(isrc,1) = 0.0
            massfrac(isrc,1) = 1.0
         
         ENDIF

      ENDDO 

      IF( acute ) THEN
         READ( rio, * ) metyr, metmo, metda, methr
c         WRITE(25, *) 'Starting date and time: ', metmo,'/',metda,
c     &                '/',metyr,' ',methr,':00'      
      ENDIF
                        
      CLOSE( rio )
      
      RETURN                 
      
      END      