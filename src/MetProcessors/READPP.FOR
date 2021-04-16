      SUBROUTINE READPP
C***********************************************************************
C*    READPPT Module of PCRAMMET Meteorological Pre-processor 
C*
C*    PURPOSE:    Reads Hourly Preciptitation Data from the 
C*                TD3240 File
C*
C*    PROGRAMMER: Jayant Hardikar
C*                PES Inc.
C*
C*    DATE:       February 14, 1995
C*     
C*    INPUTS:     TD3240 Precip 
C*
C*    OUTPUTS:    24 Hours of Precip Data 
C*        
C*    CALLED FROM: MAIN program
C***********************************************************************
C*
C*    Variable Declarations
      INCLUDE 'hrly.inc'

C     If td32flb Is TRUE, Read From TD3240 
     
      IF( td32flg ) THEN
      
C*       Loop Over 24 Hours for This Day      

         DO ihr = 1,24

C*          Determine The Period For Which Data Is Needed
C*          Using Surface Data 

            IF( sfctyp.EQ.'CD144' ) THEN

               READ( acd144(2,ihr), '(I5)' ) iy
               READ( acd144(3,ihr), '(I5)' ) im
               READ( acd144(4,ihr), '(I5)' ) id
               READ( acd144(5,ihr), '(I5)' ) ih

               IF( ih.EQ.0 ) THEN
                  
                  CALL JULIAN( iy, im, id, jd, 0 )
                  CALL HR0024( iy, jd, ih )
                  CALL JULIAN( iy, im, id, jd, 1 )
               
               ENDIF

            ELSEIF( sfctyp.EQ.'SAMSON' ) THEN
               
c     ***ADD LATER****
c               READ( jsamsn(1,ihr), '(I2)' ) iy
c               READ( jsamsn(2,ihr), '(I2)' ) im
c               READ( jsamsn(3,ihr), '(I2)' ) id
c               READ( jsamsn(4,ihr), '(I2)' ) ih

            ENDIF

C*          Code Date Variable
            
            CALL JULIAN( iy, im, id, jd, 0 )
            ndate = iy * 100000 + jd * 100 + ih

C*          Get an Hour of Precip Data

            CALL UNCDP( iotd, ndate, 24, 1, pmm, icode, idiag )
            
            p3240(ihr) = pmm
c            iy3240(ihr) = iy
c            im3240(ihr) = im
c            id3240(ihr) = id

         ENDDO
      ENDIF
      
      RETURN
      
      END