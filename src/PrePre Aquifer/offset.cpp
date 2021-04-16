#include "offset.h"

PolygonClass *CreatePolyPlume(Segment *seg, int numseg, double width)
{
  int i;
  double totlen, theta, offset;
  Point p1,p2,p3;
  PolygonClass *myPoly;
  PointClass tpt;

  totlen = 0.0;
  myPoly = new PolygonClass[numseg*2];
  offset = 0.5 * width;

  for (i=0; i<numseg; i++)
  {
    totlen += seg[i].len;
    theta = -asin((seg[i].p2.y-seg[i].p1.y) / seg[i].len);

//    if (i==0)
//      myPoly[i].addPoint(seg[i].p1.x,seg[i].p1.y);
//    else
//    {
      p1.x = 0;
      p1.y = 0.4 * (totlen-seg[i].len) + offset;
      p2.x = p1.x * cos(theta) + p1.y * sin(theta) + seg[i].p1.x;
      p2.y = p1.x * -sin(theta) + p1.y * cos(theta) + seg[i].p1.y;
      myPoly[i].addPoint(p2.x,p2.y);

      p1.x = 0;
      p1.y = -0.4 * (totlen-seg[i].len) - offset;
      p2.x = p1.x * cos(theta) + p1.y * sin(theta) + seg[i].p1.x;
      p2.y = p1.x * -sin(theta) + p1.y * cos(theta) + seg[i].p1.y;
      myPoly[i].addPoint(p2.x,p2.y);
//    }

    p1.x = seg[i].len;
    p1.y = -0.4 * totlen - offset;
    p2.x = p1.x * cos(theta) + p1.y * sin(theta) + seg[i].p1.x;
    p2.y = p1.x * -sin(theta) + p1.y * cos(theta) + seg[i].p1.y;
    myPoly[i].addPoint(p2.x,p2.y);

    p1.x = seg[i].len;
    p1.y = 0.4 * totlen + offset;
    p2.x = p1.x * cos(theta) + p1.y * sin(theta) + seg[i].p1.x;
    p2.y = p1.x * -sin(theta) + p1.y * cos(theta) + seg[i].p1.y;
    myPoly[i].addPoint(p2.x,p2.y);



    if (i)
      if (PointInsidePoly(myPoly[i-1].points[2],&myPoly[i]))
      {
        myPoly[i+numseg-1].addPoint(myPoly[i].points[0]);
        myPoly[i+numseg-1].addPoint(myPoly[i-1].points[3]);
        tpt.x = seg[i].p1.x;
        tpt.y = seg[i].p1.y;
        myPoly[i+numseg-1].addPoint(&tpt);
      }
      else
      {
        myPoly[i+numseg-1].addPoint(myPoly[i].points[1]);
        myPoly[i+numseg-1].addPoint(myPoly[i-1].points[2]);
        tpt.x = seg[i].p1.x;
        tpt.y = seg[i].p1.y;
        myPoly[i+numseg-1].addPoint(&tpt);
      }
  }
  return myPoly;
}

int getoffset(PolygonClass *plume, Segment *seg, int numseg, Point *pt, double *dist, double *offset)
{
  int cnt=0,i;
  int *idx;
  double *len;
  double myoff;
  double c, totlen, totlen1;
  PointClass tpt;

  *dist = 0.0;
  *offset = 0.0;
  totlen = 0.0;
  totlen1 = 0.0;
  tpt.x = pt->x;
  tpt.y = pt->y;

  idx = new int[numseg];
  len = new double[numseg];

  for (i=0; i<numseg; i++)
  {
    if (PointInsidePoly(&tpt,&plume[i]))
    {
      idx[cnt] = i;
      len[cnt] = totlen;
      cnt++;
    }
    totlen+=seg[i].len;
  }

  switch (cnt)
  {
    case 0:
      for (i=numseg; i<numseg*2; i++)
      {
        totlen1+=seg[i-numseg].len;
        if (PointInsidePoly(&tpt,&plume[i]))
        {
          *dist = totlen1;
          *offset = pt->DistanceTo((Line *)&(seg[i-numseg]));
          cnt++;
          break;
        }
      }
      break;
    case 1:
      myoff = pt->DistanceTo(&(seg[idx[0]]));
      c = pt->DistanceTo(&(seg[idx[0]].p1));
      *offset = myoff;
      if (c - myoff < 1.0e-2)
        *dist = len[0];
      else
        *dist = sqrt(c*c - myoff*myoff) + len[0];
      break;

    default:
      myoff = 0;
      for (i=0; i<2; i++)
        myoff += pt->DistanceTo(&(seg[idx[i]]));
      *offset = myoff / 4;
      *dist = len[1];
      break;
  }

  delete[] idx;
  delete[] len;
  if (cnt)
    return 1;
  else
    return 0;
}


