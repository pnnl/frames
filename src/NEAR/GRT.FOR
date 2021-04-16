      PROGRAM GRT
      INCLUDE 'DEVICE.CMN'
      DIMENSION RR(9), TT(9), XX(3), YY(3)
      DATA XX/100.,0.,-100./
      DATA YY/100.,  0.,-100./
      nerr = 6
C
      DO IX = 1,3
        WRITE(*,100) XX(IX)
 100    FORMAT(' Results for X =',F7.1)
        DO IY = 1,3
          X = XX(IX)
          Y = YY(IY)
          CALL GETRT(X,Y,R,T)
          IF(NERROR.GT.0) THEN
            WRITE(*,500) NERROR,X,Y
 500        FORMAT(' Error return from GETRT.'/
     .        ' NERROR, X, Y =',I3,2F9.1)
          endif
          RR(IY) = R
          TT(IY) = T
          nerror = 0
        END DO
        WRITE(*,200) (YY(I),I=1,3)
        WRITE(*,300) (RR(I),I=1,3)
        WRITE(*,400) (TT(I),I=1,3)
 200    FORMAT(' YY ',9F7.0)
 300    FORMAT(' RR ',9F7.0)
 400    FORMAT(' TT ',9F7.0)
      END DO
      END
