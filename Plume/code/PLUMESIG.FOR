
C-----------------------------------------------------------------------
c
c     PLUMESIG
c     
c     Date:              August 25, 1995
c
c     Description:       Compute diffusion coefficients for plume model
c                        using the RATCHET diffusion coefficient models.
c
c     Required modules:  None
c
c-----------------------------------------------------------------------
      SUBROUTINE PLUMESIG ( age, erht, ist, depth, sigv, sigw, sigy_in, 
     &                      sigz_in, sigzlim, sigy, sigz )
                                      
      IMPLICIT        NONE                           
      
      INTEGER         ist

      REAL            age, b, c, depth, erht, sigv, sigw, sigy, sigy_in,
     &                sigz, sigz_in, sigzlim, time, t0_y, t0_z

c  calculate the effective time for sigmas to grow from 0 to sigy_in,
c  sigz_in... to be added to age
      
      t0_y = 0.0
      t0_z = 0.0
      IF ( sigy_in .GT. 0.0 ) THEN
         c = sigy_in / sigv
         b = c * 0.9 / SQRT ( 1000. )
         t0_y = (b**2)/2.0 + c + ( b/4.0 ) * SQRT ( b**2 + 4.0 * c )
         IF ( t0_y .GT. 1800. )  t0_y = sigy_in / ( 0.453 * sigv ) 
      ENDIF     
      
      IF ( sigz_in .GT. 0.0 ) THEN 
         IF ( (erht .GT. depth) .OR. (ist .GT. 4) ) THEN
            c = sigz_in / sigw
            b = c * 0.9 / SQRT ( 50. )
            t0_z = (b**2)/2.0 + c + ( b/4.0 ) * SQRT ( b**2 + 4.0 * c )
         ELSE
            t0_z =  sigz_in / sigw 
         ENDIF
      ENDIF                                                
                                                 
c  sigma y
      
      time = age + t0_y
      IF( time .LT. 1800. ) THEN
         sigy = sigv * time / ( 1.0 + 0.9 * SQRT ( time / 1000. ) )
      ELSE
         sigy = 0.453 * sigv * time
      ENDIF
          
c  sigma z          
          
      IF ( (erht .GT. depth) .OR. (ist .GT. 4) ) THEN

c  stable conditions ... assume slow vertical diffusion 

         time = age + t0_z
         sigz = sigw * time / ( 1.0 + 0.9 * SQRT ( time / 50 ) )
                                              
      ELSE

c  neutral or unstable

         sigz =  sigw * ( age + t0_z )

      ENDIF

c  limit sigma z if appropriate 
      
      IF ( sigz .GT. sigzlim ) sigz = sigzlim
       
      RETURN 
      
      END       