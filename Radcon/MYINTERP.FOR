C
C    Function to interpolate between two points
C
C
C     time      time at which we evaluate the function, f(t), at.
C     newtim    time stored from three runs, pre-cap, cap, post-cap
C     newcon    concentration stored from three runs.
C     parent    parent contaminant number
C     contam    contaminant number
C     ntimes    number of time vs. concentration pairs
C
C
C     LOCAL VARIABLES
C
C     incr      the run to use for interpolation
C     x_high    x-axis variable (time) above the desired time
C     x_low     x-axis variable (time) below the desired time
C     y_high    y-axis variable (conc.) associated with x_high
C     y_low     y-axis variable (conc.) associated with x_low
C     temp      temporary variable
C     result    variable to sum and store the return result
C

      REAL FUNCTION MYINTR (time, newtim, newcon,parent,contam,ntimes,
     +                      co, numitr)
c pdm+                      co)
      INCLUDE 'MAXIMUM.WTN'
      REAL newtim,newcon
      INTEGER parent, contam
c pdm DIMENSION newcon(3,MAXNTI,MAXCON), newtim(3,MAXNTI,MAXCON)
      DIMENSION newcon(MAXITR,MAXNTI,MAXCON)
      DIMENSION newtim(MAXITR,MAXNTI,MAXCON)
      DIMENSION co(MAXCON)

      result = 0.0
c pdm do incr=1,3
      do incr=1,numitr
        if( (time.ge.newtim(incr,1,parent)) .AND.
     +      (time.lt.newtim(incr,ntimes+1,parent)) ) then

          i = 1

          do while ( (newtim(incr,i,parent) .lt. time) .AND.
     +               (i .le. ntimes+1) )
            i = i+1
          end do

          x_high = newtim(incr,i,parent)
          x_low  = newtim(incr,i-1,parent)
          y_high = newcon(incr,i,contam)
          y_low  = newcon(incr,i-1,contam)

          temp = ( (y_high - y_low)/(x_high - x_low) )*(time - x_low) +
     +           y_low

          result = result + temp
        end if
      end do

cjpm      if( result .gt. co(parent) ) result = co(parent)

      myintr = result

      END
