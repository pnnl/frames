      Subroutine ReSort(nhr)
C-----------------------------------------------------------------------
c     ReSort
c
c     Date:                1/22/2008
c     Rev.                 2/15/2008 BAN : to incorporate PWE's more efficient sorts
c
c     Description:         This routine writes out the output for the acute
c                          plume 95th Percentile module  
c
c     Required Modules:    None
c
C-----------------------------------------------------------------------
      IMPLICIT NONE
      
      INCLUDE  'parm.inc'
      INCLUDE  'files.inc'
      INCLUDE  'srcrec.inc'
      INCLUDE  'output.inc'
      INCLUDE  'nuc_data.inc'
    
      INTEGER  i,j,irec,k,numout,oio,outtyp,nhr,ndist,nnuks,icnt,m,jcnt
      
      REAL*4   ang, angdif, EQall, EQ95, DDall, DD95,
     &  WDall, WD95, EDall, ED95, Dir, dir2, dir3,DR,EQ,DD,WD,ED
	Dimension Dir(88000), dir3(16), DR(88000)
      Dimension EQall(88000,10), EQ(88000)
	Dimension EQ95(16,10)
	Dimension DDall(88000,10), DD(88000)
	Dimension DD95(16,10)
	Dimension WDall(88000,10), WD(88000)
	Dimension WD95(16,10)
	Dimension EDall(88000,10), ED(88000)
	Dimension ED95(16,10)
	Dimension icnt(16)
      
      CHARACTER*72   fmt, fmt1
	Character*8 NName
	Character*14 EQname
   
c      LOGICAL  
	
c     Set Formats for output
      
      WRITE( fmt, 101 ) numradii
101   FORMAT( '(1p,',i2,'(e10.3,a1))')

      WRITE( fmt1, 104) numradii
104   FORMAT( '(f5.1,a1,1p,',i2,'(e10.3,a1))')      

      oio = 30

      DO k = 1, nnucs
	DR=0.0
      EQall = 0.0
	EQ95 = 0.0
	DDall = 0.0
	DD95 = 0.0
      WDall = 0.0
	WD95 = 0.0
	EDall = 0.0
	ED95 = 0.0
c
c   This is all for one chain, therefore 1 parent and up to 8 progeny
c   The 95th percentile arrays must be filled from the intermediate files
c      prior to doing any output
c
c   So - do the sorting and storing in intermediate arrays here, then dump them below
c
      Rewind(51)
      Read(51,*) NName, ndist, nnuks
c
c set up for progeny(on first pass read 0, second pass 1, 3rd 2, etc)
      do j=1,k-1
	  read(51,*) dir2
	enddo
c
c
      do i=1,nhr-1
	Read(51,*)Dir(i), (EQall(i,j),j=1,numradii),
     & (DDall(i,j), j=1,numradii), (WDall(i,j), j=1,numradii),
     & (EDall(i,j), j=1,numradii)
	 If (nnuks .gt. 1 .and. i .lt. nhr-1) then
	  do j=1,nnuks-1
	    read(51,*)dir2
	  enddo
	 endif
	enddo
C the sort goes here
c
c  loop on distances
c
      Do j=1,numradii
	  do I =1,nhr-1
	    DR(I) = DIR(i)
	    EQ(I) = EQall(i,j)
          DD(I) = DDall(i,j)
	    WD(I) = WDall(i,j)
          ED(I) = EDall(i,j)
	  enddo

C   First, sort on directions
      CALL S1Sort2(DR, EQ, 
     & DD, WD, ED,
     & nhr-1)
c
C
C   Now, within direction, sort on magnitude
      CALL S2SORT2(DR, EQ, 
     & DD, WD, ED,
     & nhr-1, icnt) 
C
c
 	  do I =1,nhr-1
	    EQall(i,j) = EQ(I)
          DDall(i,j) = DD(I)
	    WDall(i,j) = WD(I)
          EDall(i,j) = ED(I)
	  enddo
	enddo      

c
c  This is where things get into the standard output here
c
      jcnt = 0
	Do i=1,16
	jcnt = jcnt + 0.95*icnt(i)
	dir3(i) = dr(jcnt)
	do j=1,numradii
	eq95(i,j) = eqall(jcnt,j)
      DD95(I,J) = DDall(JCNT,J)
	WD95(I,J) = WDall(JCNT,J)
	ED95(I,J) = EDall(JCNT,J)
      enddo
      jcnt = jcnt - 0.95*icnt(i) + icnt(i)+1
	enddo
c     If Parent, write out the number of progenies and external dose

         IF( k .EQ. 1 ) THEN
            WRITE( oio, '(2(a1,a,a2),2(i1,a1))')
     &      '"',NucFulName(k),'",','"',NucName(k),'",',1,',',nnucs-1,','          

               numout = 4
                     
         ELSE
            WRITE( oio, '(2(a1,a,a2),i1,a1,2(a1,a,a2))')
     &      '"',NucFulName(k),'",','"',NucName(k),'",',1,',',
     &      '"',NucFulName(1),'",','"',NucName(1),'",'
 
               numout = 3
             
         ENDIF
         
c     Write out time information and number of outputs

         WRITE( oio, '(i1,a,i1,a1)')
     &      1,',"yr",',numout,','

         DO outtyp = 1, numout

            SELECT CASE(outtyp)
         
               CASE(1) ! Air Concentration
               

                        WRITE( oio, '(a,i2,a,i2,a)')
     &                     '"Air Exposure","all","","(Bq-s)/m^3",',
     &                     numradii,',"m",',recnum,',"deg",'
            WRITE( oio, fmt) (RecRadii(i),',', i = 1, numradii )
C	       write(oio,*) (icnt(m), m=1,36)
		DO I = 1,16
		WRITE(OIO,FMT1) DIR3(I),',',(EQ95(I,J),',',J=1,NUMRADII)
		ENDDO  
               CASE(2) ! Dry Deposition
 
                        WRITE( oio, '(a,i2,a,i2,a)')
     &                     '"Total Deposition","all","dry","Bq/m^2",',
     &                     numradii,',"m",',recnum,',"deg",'   
                    WRITE( oio, fmt) (RecRadii(i),',', i = 1, numradii )
C             write(oio,'(10(1pE10.3))')((DD95(i,j),j=1,numradii),i=1,16)
 		DO I = 1,16
		WRITE(OIO,FMT1) DIR3(I),',',(DD95(I,J),',',J=1,NUMRADII)
		ENDDO       
               CASE(3) ! Wet Depostion

                        WRITE( oio, '(a,i2,a,i2,a)')
     &                     '"Total Deposition","all","wet","Bq/m^2",',
     &                     numradii,',"m",',recnum,',"deg",'      
                 WRITE( oio, fmt) (RecRadii(i),',', i = 1, numradii )
C             write(oio,'(10(1pE10.3))')((WD95(i,j),j=1,numradii),i=1,16)
		DO I = 1,16
		WRITE(OIO,FMT1) DIR3(I),',',(WD95(I,J),',',J=1,NUMRADII)
		ENDDO  
               CASE(4) ! External Dose

                  WRITE( oio, '(a,i2,a,i2,a)')
     &               '"External Dose","all"," ","Sv",',
     &               numradii,',"m",',recnum,',"deg",'
                 WRITE( oio, fmt) (RecRadii(i),',', i = 1, numradii )
C             write(oio,'(10(1pE10.3))')((ED95(i,j),j=1,numradii),i=1,16)
		DO I = 1,16
		WRITE(OIO,FMT1) DIR3(I),',',(ED95(I,J),',',J=1,NUMRADII)
		ENDDO  
            END SELECT                      

           
         ENDDO ! outtype
      ENDDO  !  nnucs
	Return
	End
