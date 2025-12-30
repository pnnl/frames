C
      CHARACTER*7 FUNCTION HYPHEN(NUKE)
C
      CHARACTER*7 NUKE,NU
      NU(1:1) = NUKE(1:1)
      NU(2:2) = NUKE(2:2)
      IF(NUKE(2:2).LE.'9') THEN
        IHY = 2
      ELSE
        IHY = 3
      ENDIF
      DO I = IHY,6
        NU(I+1:I+1) = NUKE(I:I)
      END DO
      NU(IHY:IHY) = '-'
      HYPHEN = NU
      RETURN
      END
