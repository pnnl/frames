      REAL FUNCTION SETLVEL( partdia, partrho )
C-----------------------------------------------------------------------
C
C     SETLVEL
C
C     Christian J Fosmire
C     Pacific Northwest National Lab
C     P.O. Box 999
C     Richland, WA 99352
C
C     Created:    8/23/96
C
C     Description:  Calculate the settling velocity using the equation
C                   found in the EPA's ISC3 dispersion models, where the
C                   inputs are:
C                       partdia - particle diameter (microns)
C                       partrho - particle density (g/m3)
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      
      REAL  partdia, partrho, rhoair, grav, corr, Scf, visc, x2, a1, a2,
     &      a3, vel_set
     
c     Initialize the variables
c     grav = gravity (cm/s), corr - correction for units
c     visc = absoulte viscoisty of air (cm2/micron2)
c     rhoair - density of air ( g/cm3)

      grav = 981.0
      corr = 1.0E-8
      visc = 1.81E-4
      rhoair = 1.2E-3
      
c     Calculate the slip correction factor

      x2 = 6.5E-6
      a1 = 1.257
      a2 = 0.4
      a3 = 0.55E-4
      
      Scf = 1.0 + ((2 * x2 * (a1 + a2 * EXP(-a3*partdia/x2)))/
     &             (partdia * 1.0E-4))


c     Calculate settling velocity (cm/s)

      vel_set = (partrho - rhoair) * grav * partdia**2 * corr * Scf / 
     &          (18 * visc)
     
c     Convert to m/s
      
      SETLVEL = vel_set/100.0
      
      RETURN
      
      END           
                      