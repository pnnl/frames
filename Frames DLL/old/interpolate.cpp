#include "interpolate.h"
#include "math.h"

///interpolate the value at x, y using the values at (listX, listY) and return
///the result
float interpolate(float x, float y, float *listX, float *listY, float *values, int pointCount)
{
  if(pointCount < 1)
    return 0.0;

  float distance = 0.0, weight = 0.0, weightValue = 0.0;
  float totalWeightValue = 0.0;
  float totalWeight = 0.0;

  for(int i = 0; i < pointCount; i++)
  {
    ///now currX and currY are in cartesian, find difference from x,y
    distance = sqrt(pow(listX[i]-x,2.0) + pow(listY[i]-y,2.0));
    if(distance == 0) return values[i];
    weight = 1.0 / (pow(distance, 2.0));
    weightValue = weight * values[i];

    totalWeight += weight;
    totalWeightValue += weightValue;
  }

  return totalWeightValue / totalWeight;

}

float interpolate(float x1, float y1, float x2, float y2, float targetX)
{
    float slope = (y2 - y1) / (x2 - x1);
    float intercept = y2 - (x2 * slope);

    ///thee ol slope intercept formula
    return targetX*slope + intercept;
}
