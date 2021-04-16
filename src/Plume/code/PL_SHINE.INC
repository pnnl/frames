C-----------------------------------------------------------------------
C     Shine_pl.INC
C
C     Description:   Shine variables for the plume model include file
C
C------------------------------------------------------------------------
      
      REAL     pgdrate0, pgpdx(15), pgpdy(15)
      
      INTEGER  pgmdist
      
      COMMON /Shine_pl/
     &         pgdrate0, pgpdx, pgpdy,
     &         pgmdist     