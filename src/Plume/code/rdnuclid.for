      Subroutine RDNUCLID( nio )
C-----------------------------------------------------------------------
C     RDNUCLID
C
C     Christian Fosmire
C     Pacific Northwest National Lab
C     P.O. Box 999
C     Richland, WA 99352
C
C     Date: 9/26/97
C           2/9/00  activate debug output
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

c  Read in Nuclear Data Info

      READ( nio, * ) nnucs

      WRITE(25,*)'The Number of Nuclides is ', nnucs
      WRITE(25,'(9a9)')'Number','Name','DK Const','Par_Idx1',
     &                 'Br_Frac1','Par_Idx2','Br_Frac2'
      
      DO nuc = 1, nnucs
         READ(nio, * ) NucFulName(nuc), NucName(nuc), HalfLife(nuc), 
     &   idx, Par_Indx(1,nuc), Br_Fract(1,nuc), Par_Indx(2,nuc), 
     &   Br_Fract(2,nuc)
            
c     Half Life is in day(s), convert to decay constant in 1/sec

         Dk_Const(nuc) = Log(2.0) / (HalfLife(nuc) * 24 * 3600)      
      
         WRITE(25,'(i9,a9,1p,e9.2,2(i9,e9.2))')
     &      nuc, NucName(nuc), Dk_Const(nuc), Par_indx(1, nuc), 
     &      Br_Fract(1, nuc), Par_indx(2, nuc), Br_Fract(2, nuc)
      
c     Get Release Information and mass fraction
         
         DO isrc = 1, numsrc 
            IF ( nuc .EQ. 1 ) THEN
               READ(nio,*) relrate(isrc, nuc),
     &                  (massfrac(isrc,ibn), ibn = 1, numpardis(isrc))      

               WRITE(25,*)'Mass Fraction:'
               WRITE(25,*)(massfrac(isrc,ibn), ibn = 1, numpardis(isrc))            

            ELSE
               READ(nio,*) relrate(isrc,nuc)
            ENDIF

            WRITE(25,*) 'Release Rates:'         
            WRITE( 25, *) NucName(nuc), RelRate(isrc,nuc)
         ENDDO
      ENDDO

c     Calculate the fractions at 15 minutes (900 sec)
      
      ttime = 900.0
      
      CALL C_DkFact( ttime )                         

      RETURN
      
      END