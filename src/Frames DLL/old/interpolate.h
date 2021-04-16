#ifndef INTERPOLATE_H
#define INTERPOLATE_H

/**
 * 3d interpolate the value at x, y using the values at (listX, listY) and return
 * the result.  pointCount should specify the size of the listX, listY, and
 * values lists (other words: how many points will be used to interpolate x,y)
 * the algorithm used by this function is a x-point inverse squared distance
 * weighted interpolation.  this means:
 * distance1 = (x1, y1) - (x, y)
 * distance2 = (x2, y2) - (x, y)
 * ...
 * weight1 = 1/(distance1^2)
 * weight2 = 1/(distance2^2)
 * ...
 * weightvalue1 = weight1 * values[1]
 * weightvalue2 = weight2 * values[2]
 * ...
 * pointvalue = sum(weightvaluesAll) / sum(weight)
 */
float interpolate(float x, float y, float *listX, float *listY, float *values, int pointCount=3);

/**
 * estimate the y value at x coordinate targetX, according to the
 * line defined by the points (x1, y1) - (x2, y2)
 */
float interpolate(float x1, float y1, float x2, float y2, float targetX);

#endif