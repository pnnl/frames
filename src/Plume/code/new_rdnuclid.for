      Subroutine new_RDNUCLID( nio )
C-----------------------------------------------------------------------
C     RDNUCLID
C
C     Christian Fosmire
C     Pacific Northwest National Lab
C     P.O. Box 999
C     Richland, WA 99352
C
C     Date: 9/26/97
C           2/15/00  Add species information to input (ng_fract, rg_fract,
C                    part_fract
C           12/5/2001 dimension ng_fract, rg_fract, part_fract
C
C     Description:   Read in information about nuclides and 
C                    release rates and mass fraction information
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      
      INCLUDE  'parm.inc'
      INCLUDE  'depos.inc'
      INCLUDE  'nuc_data.inc'
      INCLUDE  'srcrec.inc'

      INTEGER  nio, nuc, isrc, ibn, idx
      
      REAL     HalfLife(MaxNucs)
      REAL     ttime

c      CHARACTER*80    inline

c  Read in Nuclear Data Info

      READ( nio, * ) nnucs

      WRITE(25,*)'The Number of Nuclides is ', nnucs
      WRITE(25,'(8a9)')'   Number','     Name',' DK Const','    Index',
     &                 ' Par_Idx1',' Br_Frac1',' Par_Idx2',' Br_Frac2'
      
      DO nuc = 1, nnucs
         READ (nio,*) NucFulName(nuc), NucName(nuc), HalfLife(nuc), 
     &   idx, Par_Indx(1,nuc), Br_Fract(1,nuc), Par_Indx(2,nuc), 
     &   Br_Fract(2,nuc)
            
c     Half Life is in day(s), convert to decay constant in 1/sec

         Dk_Const(nuc) = Log(2.0) / (HalfLife(nuc) * 24. * 3600.)      
      
         WRITE(25,'(i9,a9,1pe9.2, i9,2(i9,1pe9.2))')
     &      nuc, NucName(nuc), Dk_Const(nuc), idx, Par_indx(1, nuc), 
     &      Br_Fract(1, nuc), Par_indx(2, nuc), Br_Fract(2, nuc)

c     &      deptype(nuc)
      
c     Get Release Information and mass fraction
         
         DO isrc = 1, numsrc 
            IF ( idx .EQ. 1 ) THEN

               READ(nio,*) relrate(isrc, nuc), ng_fract(nuc),  
     &                     rg_fract(nuc), part_fract (nuc)
               READ(nio,*) (massfrac(isrc,ibn),ibn = 1,numpardis(isrc)) 
        
               WRITE( 25, *) NucName(nuc), RelRate(isrc,nuc), 
     &                    ng_fract(nuc),rg_fract(nuc), part_fract(nuc) 
               WRITE(25,*)'Mass Fraction:'
               WRITE(25,*)(massfrac(isrc,ibn), ibn = 1, numpardis(isrc))            

            ELSE

               READ(nio,*) relrate(isrc,nuc),
     &                    ng_fract(nuc),rg_fract(nuc), part_fract(nuc) 
               WRITE( 25, *) NucName(nuc), RelRate(isrc,nuc), 
     &                    ng_fract(nuc),rg_fract(nuc), part_fract(nuc) 

            ENDIF

c           A temporary fix:
c            if(relrate(isrc,nuc) .le. 0.0) relrate(isrc,nuc) = 1.0E-30
c
c   BAN revised this line 16 NOv 2009; it is causing non-zero outputs
            if(relrate(isrc,nuc) .lt. 0.0) relrate(isrc,nuc) = 0.0            
		  
         ENDDO
      ENDDO

c     Calculate the fractions at 15 minutes (900 sec)
      
      ttime = 900.0
      
      CALL C_DkFact( ttime )                         

      RETURN
      
      END