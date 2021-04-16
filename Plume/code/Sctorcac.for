      SUBROUTINE SCTORCAC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SrcToRec.For
c
C       C J J Fosmire
C       Pacific Northwest Lab
C       P O Box 999
C       Richland, WA 99352
C
C       Created : 5/8/95
C
C       Description:  For a given set of receptors on a polar grid
c        this program computes for multiple sources the distance and 
c        angle to the receptors.  The program orders receptors by sector
c        index then distances (assumes all sources at center of grids)
c     
c      Input is : 
c         numradii  - Number of receptor rings
c         recpnum   - Number of receptors per ring
c         recpang   - The angle between receptors on a ring (deg)
c         recpradii - An Array of the receptor ring radii (m)
c         numsrc    - The number of sources
c         srcang    - An array of the angle of the source from the 
c                      center of the grid (deg)
c         srcradii  - An array of distance of the source from the 
c                      center of the grid (m)
c
c     Output is:
c      The following are 2-d arrays with the first dimension being the 
c         number of the receptor with sector 1 and smallest distance from 
c         the source and the second dimension being the number of the source:
c         stridx    - the number of the receptor with respect to the center of 
c                      the grid (i.e. the receptor on the first ring with angle 0
c                      has index of 1, the next receptor on the ring has 
c                      index 2, etc.)
c         strsec    - The sector that the receptor is located in
c         strang    - The angle between the receptor and the source
c         strdist   - The radial distance between the receptor and source
c
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT none
        
      INCLUDE 'parm.inc'
      INCLUDE 'srcrec.inc'  
        
      REAL     angdiff, degrad, recang, recrad, tmp, tmp1 
     
      INTEGER  i, idist, irec, isrc, itmp, itmp1, j
      
c      CHARACTER*72   fmt
        
c  Constant to convert sectors to radians(ptrad) and 
c  constant to convert degrees to radians (degrad)

      ptrad = 2.0*3.14159/FLOAT(recnum)
      degrad = 2.0*3.14159/360.

c  Set angular difference and starting angle
      
      angdiff = 360./FLOAT(recnum)
      
c       Calculate for each other source the distance and angle of
c         the receptors

      DO isrc = 1, numsrc
            
c       Reset the record number, get the source radius and angle            

         inum = 1
            
         DO idist = 1, numradii

c       Get radius of receptor              
              
            recrad =  recradii(idist)
            DO irec = 1, recnum
               
c       Get the angle of receptor
                
               recang = float(irec)*angdiff 

c       Set distance between receptor and source
                
               strdist(inum,isrc) = recrad
               
c       Set angle and sector 

               strang(inum,isrc) = recang
               strsec(inum,isrc) = irec
               stridx(inum,isrc) = inum
               inum = inum+1
             
            ENDDO
         ENDDO
           
c       Order the data by sector index then distance

         DO i = 1, inum - 2
            DO j = i+1, inum - 1 
               IF( strsec(j,isrc).lt.strsec(i,isrc) ) THEN
                  tmp = strang(i,isrc) 
                  tmp1 = strdist(i,isrc)
                  itmp = strsec(i,isrc)
                  itmp1 = stridx(i,isrc) 
                  strang(i,isrc) = strang(j,isrc)
                  strdist(i,isrc) = strdist(j,isrc)
                  stridx(i,isrc) = stridx(j,isrc)
                  strsec(i,isrc) = strsec(j,isrc)
                  strang(j,isrc) = tmp
                  strdist(j,isrc) = tmp1
                  strsec(j,isrc) = itmp
                  stridx(j,isrc) = itmp1
               ELSE IF( strsec(j,isrc).eq.strsec(i,isrc) ) THEN
                  IF( strdist(j,isrc).lt.strdist(i,isrc) ) THEN
                     tmp1 = strdist(i,isrc)
                     tmp = strang(i,isrc)
                     itmp1 = stridx(i,isrc)
                     strdist(i,isrc) = strdist(j,isrc)
                     strang(i,isrc) = strang(j,isrc)
                     stridx(i,isrc) = stridx(j,isrc)
                     strdist(j,isrc) = tmp1
                     strang(j,isrc) = tmp
                     stridx(j,isrc) = itmp1
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
      ENDDO
        
      inum = inum - 1
        
c       Write stuff to file (debug)
    
d      OPEN(12,FILE='distosrc.dbg',STATUS = 'unknown')
        
d      WRITE(12,*)'By Source....'         
d      WRITE(fmt,100)numsrc
d100   FORMAT('(',i3,'(1x,a3,1x,a3,3x,a3,6x,a4))')        
d      WRITE(12,fmt)('NUM','IDX','ANG','DIST', i = 1, numsrc)
d      WRITE(12,'(a)')
d      WRITE(fmt,120)numsrc
d      DO j = 1,inum
d         WRITE(12,fmt)(stridx(j,i),strsec(j,i),strang(j,i),
d    &      strdist(j,i), i = 1, numsrc)
d      ENDDO
d120   FORMAT('(',i3,'(i4,i4,F8.1,F8.1))')
d      CLOSE(12)
      
      RETURN
      END