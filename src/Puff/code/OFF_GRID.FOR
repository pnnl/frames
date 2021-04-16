      LOGICAL FUNCTION OFF_GRID ( x, y, xmin, xmax, ymin, ymax )

C---------------------------------------------------------------------------
c
c     OFF_GRID
c
c     Date:        August 21, 1995
c
c     Description:  This program determines if position ( x, y ) is 
c                   outside the area defined by xmin, xmax, ymin, ymax.  
c                   If either x or y is beyond the boundary the 
c                   function is .TRUE.
c
c     Required Modules:  None
c
C---------------------------------------------------------------------------

      IMPLICIT    NONE

      REAL        x, xmax, xmin, y, ymax, ymin
      
      OFF_GRID = .FALSE.
      
      IF ( (x .LT. xmin) .OR. (x .GT. xmax)  .OR.
     &     (y .LT. ymin) .OR. (y .GT. ymax) )  OFF_GRID = .TRUE.
      
      RETURN
      
      END
