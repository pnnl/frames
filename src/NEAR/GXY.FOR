      PROGRAM GXY
      DIMENSION RR(3), TT(9), XX(9), YY(9)
      DATA RR/1.,100.,1000./
      DATA TT/0.,45.,90.,135.,180.,225.,270.,315.,360./
C
      DO IR = 1,3
        WRITE(*,100) RR(IR)
 100    FORMAT(' Results for R =',F7.1)
        DO IT = 1,9
          R = RR(IR)
          T = TT(IT)
          CALL GETXY(R,T,X,Y)
          XX(IT) = X
          YY(IT) = Y
        END DO
        WRITE(*,200) (TT(I),I=1,9)
        WRITE(*,300) (XX(I),I=1,9)
        WRITE(*,400) (YY(I),I=1,9)
 200    FORMAT(' TT ',9F7.0)
 300    FORMAT(' XX ',9F7.0)
 400    FORMAT(' YY ',9F7.0)
      END DO
      END
