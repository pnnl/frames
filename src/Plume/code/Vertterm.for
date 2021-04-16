      REAL FUNCTION VERTTERM( h_e, z, mix_hgt, sigz )
 
c      SUBROUTINE VERTTERM( h_e, z, mix_hgt, sigz, vertdisp )
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     VertTerm.FOR
c     Christian J Fosmire
c     Pacific Northwest Lab
c     P.O. Box 999
c     Richland, WA 99352
c
c     Created: 6/14/95
c
c     Description:  This function computes the vertical dispersion term
c                   for the Gaussian plume equation.  
c                                                                       
c     Required Modules:    NONE
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT    NONE
       
      REAL        exparg, h_e, mix_hgt, refhgt, sigz, vertdisp, z 
      
      INTEGER     nvert, nvrterm

c     Set the number of vertical terms to calculate
    
      nvrterm = 2

c     Calculate the vertical terms

      vertdisp = 0.0

      IF( h_e .LT. mix_hgt ) THEN

c     If h_e is below the mixing height then have both a virtual below
c     ground source and a virtual elevated source

         DO nvert = -nvrterm, nvrterm

            refhgt = (2 * FLOAT(nvert) * mix_hgt - h_e - z) / sigz
            exparg = 0.5 * refhgt*refhgt

            IF( (exparg .GT. 1e-3) .AND. (exparg .LT. 15) ) THEN
               vertdisp = vertdisp + EXP(-exparg)
            ELSE IF( exparg .LE. 1e-3 ) THEN
               vertdisp = vertdisp + 1.0
            ENDIF

            refhgt = (2 * FLOAT(nvert) * mix_hgt + h_e - z) / sigz
            exparg = 0.5 * (refhgt*refhgt)

            IF( (exparg .GT. 1e-3) .AND. (exparg .LT. 15) ) THEN
               vertdisp = vertdisp + EXP(-exparg)
            ELSEIF( exparg .LE. 1e-3 ) THEN
               vertdisp = vertdisp + 1.0
            ENDIF

         ENDDO   

      ELSE

c     If h_e is above the mixing height, then only have a virtual below
c     ground source...i.e., the mixing layer allows the plume to disperse
c     to the ground but not through the mixing layer

         DO nvert = -nvrterm, nvrterm
         
            IF( nvert .LT. 0 ) THEN
               refhgt = (2 * FLOAT(nvert) * mix_hgt - h_e - z) / sigz
               exparg = 0.5 * refhgt*refhgt

               IF( (exparg .GT. 1E-3) .AND. (exparg .LT. 15) ) THEN
                  vertdisp = vertdisp + EXP(-exparg)
               ELSE IF( exparg .LE. 1e-3 ) THEN
                  vertdisp = vertdisp + 1
               ENDIF

            ELSE IF( nvert .GT. 0 ) THEN

               refhgt = (2 * FLOAT(nvert) * mix_hgt + h_e - z) / sigz
               exparg = 0.5 * refhgt*refhgt

               IF( (exparg .GT. 1E-3) .AND. (exparg .LT. 15) ) THEN
                  vertdisp = vertdisp + EXP(-exparg)
               ELSE IF( exparg .LE. 1e-3 ) THEN
                  vertdisp = vertdisp + 1
               ENDIF

            ELSE

               refhgt = (2 * FLOAT(nvert) * mix_hgt - h_e - z) / sigz
               exparg = 0.5 * refhgt*refhgt

               IF( (exparg .GT. 1E-3) .AND. (exparg .LT. 15) ) THEN
                  vertdisp = vertdisp + EXP(-exparg)
               ELSE IF( exparg .LE. 1e-3 ) THEN
                  vertdisp = vertdisp + 1
               ENDIF

               refhgt = (2 * float(nvert) * mix_hgt + h_e - z) / sigz
               exparg = 0.5 * refhgt*refhgt

               IF( (exparg .GT. 1E-3) .AND. (exparg .LT. 15) ) THEN
                  vertdisp = vertdisp + EXP(-exparg)
               ELSEIF( exparg .LE. 1e-3 ) THEN
                  vertdisp = vertdisp + 1
               ENDIF

            ENDIF
         ENDDO
      ENDIF
      
      VERTTERM = vertdisp
      
      RETURN                        
      
      END