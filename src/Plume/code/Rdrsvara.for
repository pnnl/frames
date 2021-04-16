      SUBROUTINE  RdRSVarA( metyr, metmo, metda, methr, do_sect )
C-----------------------------------------------------------------------
C     RdRSVarA
C
C     Christian J Fosmire
C     Pacific Northwest National Lab
C     P.O. Box 999
C     Richland, WA 99352
C
C     Date:    7/29/96
c     Revision 30 March 2006 BAN to read MINWIND
C
C     Description:   This routine reads in the various variables from 
C                    the run specification file    
C
C     Required Modules: NONE
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      
      INCLUDE 'parm.inc'
      INCLUDE 'depos.inc'
      INCLUDE 'files.inc'
      INCLUDE 'metdata.inc'
      INCLUDE 'nuc_data.inc'
      INCLUDE 'shine.inc'
      INCLUDE 'srcrec.inc'
      
      REAL     SETLVEL
      REAL     distb(36), partdia(MaxPBins), partdens(MaxPBins)
      
      INTEGER  isrc, i, j, rio, ier, metmo, metyr, metda, methr 
c      INTEGER*2        i1, is, im, ih

      LOGICAL     partdisflg, do_sect

c      WRITE (25,'(a)') ' RDRSVARA'
      
c     Open Run Specification File

      rio = 55
      
      OPEN( rio, File = rsfile, STATUS = 'OLD', IOSTAT = ier )
         
      IF (ier .NE. 0 ) THEN   
         WRITE(25,*) 'Error opening File ', rsfile
         WRITE(25,*) 'Error Number - ', ier
         STOP 1
      ENDIF
      
c     Read in minimum wind speed for plume rise

      READ( rio, * ) wndmin    

d     WRITE(25,*)'Minimum Wind Speed for Plume Rise is ', wndmin

c  Sigma y for shift to semi-inifinite cloud shine model

      READ ( rio, * ) chgmod
c
c  addition to bring in ARMINWIND  BAN 30 Mar 2006
      READ ( rio, * ) minwind
c
 

d     WRITE(25,*)'The shift for semi-infinite cloud is ', chgmod

      
c     Set Random seed

c      CALL GETTIM( ih, im, is, i1 )
                  
c      rndseed = 1.0D+4 * DBLE(i1) + 1.0D+3 * DBLE(i1*2) + 
c     &             1.0D+2 * DBLE(is) + DBLE(im)

      
c  Read in the number of receptors

      READ( rio, * ) numradii
      
c  Check if too many receptor rings
      
      IF( numradii .GT. MaxRadii ) THEN
         WRITE(25,*) 'ERROR...Too many receptor rings'
         WRITE(25,*) 'Only allowed ',MaxRadii,' receptor rings'
         STOP 1
      ENDIF

d      WRITE(25,*)'The number of receptor radii is ',numradii

c  Read in Radii
      
      READ( rio, * ) (recradii(i), i = 1, numradii)

d      WRITE(25,*)'The radii of receptor rings are ',recradii

c  Read in number of receptors (sectors) per ring

      READ( rio, * )recnum
      
c  Check if too many receptors
      
      IF( (recnum .NE. 16) .AND. (recnum .NE. 36) ) THEN
         WRITE(25,*) 'ERROR-can only have 16 or 36 sectors'
         WRITE(25,*) 'Not ',recnum,' sectors'     
         STOP 1
      ENDIF
      
d      WRITE(25,*)'The number of receptors per ring is ',recnum

c  Read in the Tranfer resistances for gas and particle

      READ ( rio, * ) (rtx(i), i = 1, 2)
      
d      WRITE ( 25, * ) 'The transfer resistances are ', rtx
      
c  Read in the sigma parameterization

      READ ( rio, * ) sigparm
      
d      WRITE ( 25, * ) 'The sigma parameterization scheme is ',sigparm

c  Read in flag for low wind direction distribution

      READ( rio, * ) usercalm

d     WRITE(25,*)'Flag for Calm directions is ', usercalm
      
c  Read in distribution, if exist
C
C   14 Dec 2009  BAN  Correct one-sector-off read of distb
C
      IF ( usercalm ) THEN
	   READ( rio, * ) distb(recnum), (distb(i), i = 1, recnum-1)

c      IF ( usercalm ) THEN
c         READ( rio, * )(distb(i), i = 1, recnum)

c  Create cumulative distribution

         calmdis(1) = distb(1)
         DO i = 2, recnum
            calmdis(i) = calmdis(i-1) + distb(i)
         ENDDO

d        WRITE(25,*)'Distribution of Calm Directions'
d        WRITE(25,*) (i, i = 1, recnum)
d        WRITE(25,*)(calmdis(i), i = 1, recnum)

      ENDIF
         
c  Get the number of sources

      READ ( rio, * ) numsrc
      
c     Check the number of sources      
      
      IF( numsrc .GT. Maxsrcs ) THEN
         WRITE(25,*) 'ERROR...Too Many Sources.'
         WRITE(25,*) 'Can only have ',Maxsrcs,' sources.'
         STOP 1
      ENDIF
      
      IF( numsrc .LE. 0 ) THEN
         WRITE(25,*) 'ERROR...No Sources.'
         WRITE(25,*) 'Must have a source'
         STOP 1
      ENDIF
       
d     WRITE(25,*)'The number of sources is ',numsrc

c  Read in source information for each source     
      
      DO isrc = 1, numsrc
         READ( rio, * )SrcTyp(isrc), Src_Rad(isrc), Src_Ang(isrc), 
     &                 Src_Var1(isrc), PlmFlg(isrc), St_rad(isrc), 
     &                 St_flow(isrc), Etemp(isrc)

c  Read in the flags for doing wake or enchanced sigmas

         READ( rio, * )iscwakflg(isrc), sslowbnd(isrc), bidflg(isrc), 
     &                 hgwndflg(isrc), lwwndflg(isrc)

c     If doing ISC wakes then read in building height and building
c     widths for all sectors. If doing high wind speed sigmas
c     then read in the area for all sectors.
C
C    14 Dec 2009 BAN correct 1-sector-off read of bldhgt and bldwth:
C
         IF( iscwakflg(isrc) ) THEN
	      READ( rio, * )  bldhgt(recnum,isrc),(bldhgt(j,isrc),
     .                       j = 1, recnum-1)
	      READ( rio, * )  bldwth(recnum,isrc),(bldwth(j,isrc),
     .                       j = 1, recnum-1) 
         ELSE IF( hgwndflg(isrc) ) THEN
	      READ( rio, * )  area(recnum,isrc),(area(j,isrc),
     .                       j = 1, recnum-1)
         ENDIF
c         IF( iscwakflg(isrc) ) THEN
c            READ( rio, * ) (bldhgt(j,isrc),j = 1, recnum)
c            READ( rio, * ) (bldwth(j,isrc),j = 1, recnum)
c         ELSE IF( hgwndflg(isrc) ) THEN
c            READ( rio, * ) (area(j,isrc),j = 1, recnum)
c         ENDIF
d         IF( SrcTyp(isrc) .NE. 1 ) THEN
d       WRITE(25,*)'Num  Type  Radii  Angle  Hgt Flag SRad SFlow Etemp'
d       WRITE(25,*)i,SrcTyp(isrc),Src_Rad(isrc),Src_Ang(isrc),
d    &      Src_Var1(isrc), PlmFlg(isrc), St_rad(isrc),St_Flow(isrc),
d    &      Etemp(isrc)
d         ELSE
d       WRITE(25,*)'Num Type  Radii Angle    Area Size'
d       WRITE(25,*)isrc,SrcTyp(isrc),Src_Rad(isrc),Src_Ang(isrc),
d    &             Src_Var1(isrc)
d         ENDIF       

c     Check if src has particle size distribution

         READ( rio, * ) partdisflg
         
d     WRITE(25,*) 'Distribution Flag for Source is ', partdisflg

c     Check if reading in distribution

         meandia = 0
         meanden = 0
         IF( partdisflg ) THEN
            READ( rio, * )numpardis(isrc)
            

d     WRITE(25,*) 'Number of particles classes is ', numpardis(isrc)
         
            READ( rio, * )
     &            (partdia(i), i = 1, numpardis(isrc))
            

d     WRITE(25,*)'Particle Diameters:'
d     WRITE(25,*) (partdia(i), i = 1, numpardis(isrc))
            
            READ( rio, * )
     &            (partdens(i), i = 1, numpardis(isrc))
         

d     WRITE(25,*)'Particle Density:'
d     WRITE(25,*)(partdens(i), i = 1, numpardis(isrc))            
            

c     Calculate settling velocities
            
            DO i = 1, numpardis(isrc)
               setvel(isrc,i) =  SETLVEL( partdia(i), partdens(i) )
               meandia = meandia + (partdia(i)/FLOAT(numpardis(isrc)))
               meanden = meanden + (partdens(i)/FLOAT(numpardis(isrc)))
            ENDDO

c            WRITE(25,'(a,10f10.4)') 'Settling Velocities:', 
c     &                 (setvel(isrc,i), i = 1, numpardis(isrc))
        
         ELSE
         
            numpardis(isrc) = 1
            setvel(isrc,1) = 0.0
            massfrac(isrc,1) = 1.0
         
         ENDIF

      ENDDO

c  Read in flag for doing centerline or sector average

      READ( rio, * ) do_sect  

c  Read in Date and Time of Met Data
      
      READ( rio, * ) metyr, metmo, metda, methr
      
      CLOSE( rio )
      
      RETURN

      END   