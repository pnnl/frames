      Subroutine RDNUCLPF( nio, acute, PrgStat )
C-----------------------------------------------------------------------
C     RDNUCLPF
C
C     Christian Fosmire
C     Pacific Northwest National Lab
C     P.O. Box 999
C     Richland, WA 99352
C
C     Date:          September 26, 1997
c     Updated:       April 26, 2000
c                    14 Dec 2001 BAN to include ng_, rg_, and part_fract
C
C     Description:   Read in information about nuclides and 
C                    release rates and mass fraction information for
C                    Puff Model
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      
      INCLUDE        'parm.inc'  
      INCLUDE        'const.inc'
      INCLUDE        'depos.inc'
      INCLUDE        'nuc_data.inc'
      INCLUDE        'rel.inc'

      INTEGER        nio, nuc, isrc, ibn, idx, ier
      
      REAL           HalfLife(MaxNucs)
      REAL           ttime

      CHARACTER*2    prgid
      CHARACTER*50   PrgStat

      LOGICAL        acute      

c      WRITE (25, '(a)' ) 'Subroutine RdNuclPf'

c  Read in Nuclear Data Info

      READ( nio, *, IOSTAT=ier ) nnucs

      IF ( ier .NE. 0 ) THEN
         WRITE ( PrgStat(1:2),'(a2)'  )     prgid
         WRITE ( PrgStat(4:13),'(a10)' )   'RdNuclPf'
         WRITE ( PrgStat(15:18),'(i4)' )    ier
         WRITE ( PrgStat(20:29),'(a10)' )  'nuclfile'
         WRITE ( PrgStat(31:50), '(a20)' ) '   nnucs'
      ENDIF                                  

c      WRITE(25,*)'The Number of Nuclides is ', nnucs
c      WRITE(25,'(9a9)')'Number','Name','DK Const','Par_Idx1',
c     &                 'Br_Frac1','Par_Idx2','Br_Frac2'
      
      DO nuc = 1, nnucs
         READ(nio, *, IOSTAT = ier) NucFulName(nuc), NucName(nuc), 
     &   HalfLife(nuc), idx, Par_Indx(1,nuc), Br_Fract(1,nuc),  
     &   Par_Indx(2,nuc), Br_Fract(2,nuc)

         IF ( ier .NE. 0 ) THEN
            WRITE ( PrgStat(1:2),'(a2)'  )     prgid
            WRITE ( PrgStat(4:13),'(a10)' )   'RdNuclPf'
            WRITE ( PrgStat(15:18),'(i4)' )    ier
            WRITE ( PrgStat(20:29),'(a10)' )  'nuclfile'
            WRITE ( PrgStat(31:50), '(a20)' ) 'Reading nuclide data'
         ENDIF                                  

            
c     Half Life is in day(s), convert to decay constant in 1/sec

         IF ( HalfLife(nuc) .GT. 0.0 ) THEN
            Dk_Const(nuc) = Log(2.0) / (HalfLife(nuc) * 24 * 3600)      
         ELSE      
            Dk_Const(nuc) = 0.0
         ENDIF

c         WRITE(25,'(i9,a9,1p,e9.2,2(i9,e9.2))')
c     &      nuc, NucName(nuc), Dk_Const(nuc), Par_indx(1, nuc), 
c     &      Br_Fract(1, nuc), Par_indx(2, nuc), Br_Fract(2, nuc)
      
c     Get Release Information and mass fraction
         
         DO isrc = 1, nsrc 
            IF ( idx .EQ. 1 ) THEN

               READ (nio,*,IOSTAT=ier) relrate(isrc, nuc),ng_fract(nuc), 
     &                     rg_fract(nuc), part_fract(nuc)
               IF ( ier .NE. 0 ) THEN
                  WRITE ( PrgStat(1:2),'(a2)'  )     prgid
                  WRITE ( PrgStat(4:13),'(a10)' )   'RdNuclPf'
                  WRITE ( PrgStat(15:18),'(i4)' )    ier
                  WRITE ( PrgStat(20:29),'(a10)' )  'nuclfile'
                  WRITE ( PrgStat(31:50), '(a20)' ) 'Release rate data'
               ENDIF                                  

               READ  (nio,*,IOSTAT=ier) 
     &            (massfrac(isrc,ibn),ibn = 1,numpardis(isrc))      
               IF ( ier .NE. 0 ) THEN
                  WRITE ( PrgStat(1:2),'(a2)'  )     prgid
                  WRITE ( PrgStat(4:13),'(a10)' )   'RdNuclPf'
                  WRITE ( PrgStat(15:18),'(i4)' )    ier
                  WRITE ( PrgStat(20:29),'(a10)' )  'nuclfile'
                  WRITE ( PrgStat(31:50), '(a20)' ) 'Particle size data'
               ENDIF                                  

c               WRITE ( 25,* )'Release data'
c               WRITE ( 25,* ) relrate(isrc,nuc), ng_fract, rg_fract, 
c     &                        part_fract 
c               WRITE ( 25,* ) (massfrac(isrc,ibn), 
c     &                        ibn = 1, numpardis(isrc))            

            ELSE

               READ( nio,*,IOSTAT=ier ) relrate(isrc,nuc),ng_fract(nuc), 
     &                     rg_fract(nuc), part_fract(nuc)
               IF ( ier .NE. 0 ) THEN
                  WRITE ( PrgStat(1:2),'(a2)'  )     prgid
                  WRITE ( PrgStat(4:13),'(a10)' )   'RdNuclPf'
                  WRITE ( PrgStat(15:18),'(i4)' )    ier
                  WRITE ( PrgStat(20:29),'(a10)' )  'nuclfile'
                  WRITE ( PrgStat(31:50), '(a20)' ) 'Release rate n>1'
               ENDIF                                  

c               WRITE ( 25,* )'Release data'
c               WRITE ( 25,* ) relrate(isrc,nuc)
 
            ENDIF
         ENDDO

      ENDDO

c     Calculate the fractions at 15 minutes (900 sec)
      
      ttime = 3600.0/FLOAT(nph)
      
      CALL C_DkFact( ttime )                         

      RETURN
      
      END