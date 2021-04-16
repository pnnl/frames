      REAL FUNCTION SOLALT( jdate, hour, lat )
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     
C     SOLALT
C
C     J.V. Ramsdell
C     Pacific Northwest Laboratory
C     P.O. Box 999
C     Richland, WA 99352
C
C     Created:    4/28/92
C
C     Description:  Computes the solar altitude for the given hour
C
C     Input:   Julian date                      ===>     jdate
C              Hour of the day                  ===>     hour
C              Latitude                         ===>     lat
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      
      IMPLICIT    NONE
      
      REAL        ang, cl, declin, hrang, lat, sar, sl
      
      INTEGER     hour, jdate
      
      cl = COS( lat/57.2958 )
      sl = SIN( lat/57.2958 )
      
c     Compute the local hour angle of the sun

      hrang = ( 180. - (hour / 12.) * 180. ) / 57.2958
      
c     Compute the declination of the sun

      ang = ( ( jdate/173. ) * 180. ) / 57.2958
      
      declin = ( -23.5 * COS( ang ) ) / 57.2958
      
c     Compute the solar altitude

      sar = ASIN( sl*SIN( declin ) + cl*COS( declin )*COS( hrang ) )
      
      SOLALT = sar * 57.2985
      
      RETURN
      
      END
      