C-----------------------------------------------------------------------
c
c     DDEPVEL
c
c     Date:              August 16, 1995
c
c     Description:
c
c        DDEPVEL estimates the dry deposition velocity for particles and
c        gases using the resistance analogy.  The analogy is described 
c        in Seinfeld (1986).  The deposition velocity is the reciprocal 
c        of the total resistance to deposition.  The total resistance is 
c        made up of three components in series-- aerodynamic, surface, 
c        and transfer.  The aerodynamic and surface resistances are 
c        function of the wind speed, surface roughness, and stability.
c        The stability, height and roughness length dependence enters 
c        through the friction velocity.  The transfer resistance is a 
c        function of the characteristics of the surface and depositing
c        material. 
c 
c     Required modules:  None
c
C-----------------------------------------------------------------------
      REAL FUNCTION DDEPVEL( ustr, wspd, rtx, setvel )

      IMPLICIT    NONE
                               
      REAL        ra, rs, rtx, u, ustr, wspd, setvel  

      DDEPVEL = 0.0

      u = wspd

      IF ( u .LT. 1.0 ) u = 1.0
      
      ra = u / ustr**2
      rs = 2.6 / ( 0.4 * ustr )

C  Add transfer resistance and compute deposition velocity

      DDEPVEL = 1.0 / ( ra + rs + rtx + (ra * rs * setvel) )
      
      DDEPVEL = DDEPVEL + setvel         
      
      RETURN 
      
      END
