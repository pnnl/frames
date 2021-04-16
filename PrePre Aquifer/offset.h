#ifndef OFFSETH
#define OFFSETH
#include "PolyClass.h"

PolygonClass *CreatePolyPlume(Segment *seg, int numseg, double width);
int getoffset(PolygonClass *plume, Segment *seg, int numseg,
              Point *pt, double *dist, double *offset);

#endif