      SUBROUTINE NSIG ( dsmtri, stab, szlim, sigmaz, sigmay )

C---------------------------------------------------------------------------
c
c     NSIG                                         
c
c     Date:              August 16, 1995
c     Updated:           March 14, 2000
c
c     Description:       Computes new diffusion coefficients given the
c                        last values, atmospheric stability, mixing layer
c                        thickness and distance moved.
c
c     Required modules:  None
c
C---------------------------------------------------------------------------

      IMPLICIT      NONE
  
      REAL          ay(7), az(7,3), bz(7,3), cz(7,3)
                    
      REAL          dsmtri, sigmay, sigmaz, szlim, xey, xez, xvy, xvz              
                    
      INTEGER       stab
  
      DATA ay/ 0.3658, 0.2751,0.2089,0.1471,0.1046,0.0722,0.0481/
      
      DATA az/ 0.192,  0.156, 0.116, 0.079, 0.063, 0.053, 0.032,
     &         0.00066,0.0382,0.113, 0.222, 0.211, 0.086, 0.052,
     &         0.00024,0.055, 0.113, 1.26,  6.73, 18.05, 10.83 /
      
      DATA bz/ 0.936, 0.922, 0.905, 0.881, 0.871, 0.814, 0.814,
     &         1.941,  1.149, 0.911, 0.725, 0.678, 0.74,  0.74,
     &         2.094,  1.098, 0.911, 0.516, 0.305, 0.18,  0.18 /
     
      DATA cz/ 0.0,    0.0,   0.0,   0.0,   0.0,   0.0,   0.0,
     &         9.27,   3.3,   0.0,  -1.7,  -1.3,  -0.35, -0.21,
     &        -9.6,    2.0,   0.0, -13.,  -34.0, -48.6, -29.2  /


      xvy = 0.0
      IF ( sigmay .GT. 1.0 ) xvy = ( sigmay/ay(stab) )**(1.0/0.9031)      
      xey = xvy + dsmtri
      sigmay = ay(stab) * xey**0.9031
  
C  SIGMA Z COMPUTATIONS
C  CHECK INITIAL SIGMA Z SIZE AGAINST MAXIMUM
  
      IF ( sigmaz .GE. szlim)  RETURN
  
C  COMPUTE VIRTUAL DISTANCE
      
      xvz = 0
      
      IF ( sigmaz .GT. 1.0 ) THEN 
       
         xvz = ( sigmaz/az(stab,1) )**(1.0 / bz(stab,1)) 
      
         IF ( ( (xvz+dsmtri) .GT. 100.0 ) .AND. 
     &        ( sigmaz .GT. CZ(stab,2) ) ) THEN 
     
            xvz = ( ( sigmaz - cz(stab,2) ) / az(stab,2) )**
     &             (1.0/bz(stab,2))
     
            IF ( ( (xvz+dsmtri) .GT. 1000.0 ) .AND.
     &           (sigmaz .GT. cz(stab,3) ) ) THEN
               xvz = ( ( sigmaz - cz(stab,3) ) / az(stab,3) )**
     &                (1.0/bz(stab,3))
            ENDIF
      
         ENDIF
      
      ENDIF 
      
      xez = xvz + dsmtri
      
      IF ( xez .LE. 100.0 ) THEN
        sigmaz = az(stab,1) * xez**bz(stab,1)
      ELSE IF ( xez .LE. 1000.0 ) THEN
        sigmaz = az(stab,2) * xez**bz(stab,2) + cz(stab,2)
      ELSEIF ( xez .GT. 1000.0 ) THEN
        sigmaz = az(stab,3) * xez**bz(stab,3) + cz(stab,3)
      ENDIF                                       
      
      IF ( sigmaz .GT. szlim )  sigmaz = szlim
      
      RETURN
      
      END  
