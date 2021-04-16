cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     test
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
      INTEGER     imo(12), ida(14), ihr(14)
      
      REAL        ang, lat, long, solalt, tcorr
      
      DATA imo / 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 /
      DATA ida / 1, 7, 24, 11, 1, 12, 29, 4, 16, 21, 27, 30, 26, 31 /
      DATA ihr / 1, 21, 12, 17, 20, 23, 6, 10, 3, 13, 9, 13, 21, 23 /
      
      iyr = 88
      
      lat = 37.39
      long = 97.25
      tcorr = 6
      
      DO i = 1, 14
      
         IF ( i.lt.3 ) THEN
            im = imo(1)
         ELSEIF( i.gt.12 ) THEN
            im = imo(12)
         ELSE
            im = imo(i-1)
         ENDIF
         id = ida(i)
         ih = ihr(i)
         
         CALL JULIAN( iyr, im, id, jd, 0 )
         
         CALL SUNTIME( jd, lat, long, trise, tset )
         
         trise = trise - 6
         tset = tset - 6
         
         ang = SOLALT( jd, ih, lat)
         
         WRITE(*,*) im, id, ih, trise, tset, ang
         
      ENDDO
      
      END