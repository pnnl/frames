#include "line.h"

/*
class Point
public:
float x,y;
*/
Point::Point()
{
  x = 0;
  y = 0;
}

Point::Point(float x, float y)
{
  this->x = x;
  this->y = y;
}

double Point::DistanceTo(Point *p)
{
  double dif_x, dif_y;
  dif_x = (p->x - x);
  dif_y = (p->y - y);
  
  return sqrtl((long double)dif_x*(long double)dif_x + (long double)dif_y*(long double)dif_y);
}

double Point::DistanceTo(Line *z)
{                                    // Distance from point to a line
  double temp;
  if (z->vertical)
    return fabs(z->xequal - x);
  else
  {
    
    temp = sqrt(z->m*z->m + 1);
    return fabs(z->m*x - y + z->b) / temp;
  }
}

double Point::DistanceTo(Segment *z)
{                                    // Distance from point to a segment
  double a,b,c;
  
  a = z->len;
  b = DistanceTo((Line *)z);
  c = DistanceTo(&(z->p1));
  if (a*a < c*c - b*b)
    return 0;
  return b;
}

Point& Point::operator =(Point &p)
{
  this->x = p.x;
  this->y = p.y;
  return *this;
}

/*
class Line
public:
double m;
double b;
*/

Line::Line()
{
  m = 0.0;             // define x axis as the default line
  b = 0.0;
  xequal = 0.0;
  vertical = 0;
  parallel = 0;
}

Line::Line(float x)
{
  m = 0.0;       // no meaning when vertical
  b = 0.0;       // no meaning when vertical
  xequal = x;
  vertical = 1;
  parallel = 0;
}

Line::Line(float m, float b)
{
  this->m = m;
  this->b = b;
  vertical = 0;
  parallel = 0;
}

Line::Line(Point p1, Point p2)
{
  Line(p1.x,p1.y,p2.x,p2.y);
}

Line::Line(float x1,float y1,float x2,float y2)
{
  xequal = 0.0;
  parallel = 0;
  
  if (fabs(x2 - x1)<=0)
  {
    //      Line::Line(x1);
    m = 0.0;       // no meaning when vertical
    b = 0.0;       // no meaning when vertical
    xequal = x1;
    vertical = 1;
    parallel = 0;
    return;
  }
  else
    vertical = 0;
  
  m = (y2 - y1)/(x2 - x1);
  b = y2 - (m * x2);
}
void Line::lineinit(float x1,float y1,float x2,float y2)
{
  if (fabs(x2 - x1)<=0)
  {
    //      Line::Line(x1);
    m = 0.0;       // no meaning when vertical
    b = 0.0;       // no meaning when vertical
    xequal = x1;
    vertical = 1;
    parallel = 0;
    return;
  }
  else
    vertical = 0;
  
  m = (y2 - y1)/(x2 - x1);
  b = y2 - (m * x2);
}

Point *Line::Intersect(Line *myLine)
{
  if (vertical && myLine->vertical)                          // both lines vertical?
  {
    parallel = 1;
    if (fabs(xequal-myLine->xequal) <= fabs(1.0E-4*xequal))  // x intercept equal?
    {
      a.x = xequal;                                          // same line
      a.y = 0.0;
      return &a;
    }
    else
      return NULL;                                           // no intersetion
  }
  if (vertical)                                              // this line vertical?
  {
    if ((-1.0E-20 < myLine->m) && (myLine->m < 1E-20))     // my line horizontal?
    {
      a.x = xequal;                                          // perpendicular
      a.y = myLine->b;
      parallel = 0;
      return &a;
    }
  }
  if (myLine->vertical)                                      // my line vertical?
  {
    if ((-1.0E-300 < m) && (m < 1E-300))                     // this line horizontal?
    {
      a.x = myLine->xequal;                                  // perpendicular
      a.y = b;
      parallel = 0;
      return &a;
    }
  }
  if (fabs(m - myLine->m) <= fabs(1.0E-4*m))                 // slopes equal?
  {
    parallel = 1;
    if (fabs(b - myLine->b) <= fabs(1.0E-4*b))               // y intercept equal?
    {
      a.x = 0.0;                                             // same line
      a.y = b;
      return &a;
    }
    else
      return NULL;                                           // no intersetion
  }
  
  if (fabs(m - myLine->m) <= fabs(1.0E-4*m))                 // assigned no slope
  {
    parallel = 1;
    if (fabs(b - myLine->b) <= fabs(1.0e-4*b))                // y intercept equal?
    {
      a.x = 0.0;
      a.y = b;
      return &a;
    }
    else
      return NULL;
  }
  
  if (((-1.0E-20 < m) && (m < 1E-20)) || m == 0.0)                       // line parallel to x axis?
  {
    if  (((-1.0E-20 < myLine->m) && (myLine->m < 1E-20)) || myLine->m == 0.0)
    {
      parallel = 1;
      if (fabs(b - myLine->b) <= fabs(1.0e-4*b))                // y intercept equal?
      {
        a.x = 0.0;
        a.y = b;
        return &a;
      }
      return NULL;
    }
    else
    {
      a.y = ((myLine->m*b) - (m*myLine->b)) / (myLine->m-m);
      a.x = (a.y - myLine->b) / myLine->m;                   //
    }
  }
  else
  {
    a.y = ((m*myLine->b) - (myLine->m*b)) / (m-myLine->m);
    a.x = (a.y - b) / m;
  }
  return &a;
}


/*
class Segment:public Line
public:
Point p1,p2;
*/

Segment::Segment(): Line() { }

Segment::Segment(Point *pt1, Point *pt2):
Line(p1,p2)
{
  p1.x = pt1->x;
  p1.y = pt1->y;
  p2.x = pt2->x;
  p2.x = pt2->y;
  len = p1.DistanceTo(&p2);
}

Segment::Segment(float x1,float y1,float x2,float y2):
Line(x1,y1,x2,y2)
{     
  p1.x = x1;
  p1.y = y1;
  p2.x = x2;
  p2.y = y2;
  len = p1.DistanceTo(&p2);
}
void Segment::segmentinit(float x1,float y1,float x2,float y2)
{
  lineinit(x1,y1,x2,y2);
  p1.x = x1;
  p1.y = y1;
  p2.x = x2;
  p2.y = y2;
  len = p1.DistanceTo(&p2);
}

Point *Segment::Intersect(Segment *mySegment)
{
/*
maxx = (p2.x > p1.x) ? p2.x : p1.x;
smaxx = (mySegment->p2.x > mySegment->p1.x) ? mySegment->p2.x : mySegment->p1.x;
minx = (p2.x < p1.x) ? p2.x : p1.x;
sminx = (mySegment->p2.x < mySegment->p1.x) ? mySegment->p2.x : mySegment->p1.x;

  maxy = (p2.y > p1.y) ? p2.y : p1.y;
  smaxy = (mySegment->p2.y > mySegment->p1.y) ? mySegment->p2.y : mySegment->p1.y;
  miny = (p2.y < p1.y) ? p2.y : p1.y;
  sminy = (mySegment->p2.y < mySegment->p1.y) ? mySegment->p2.y : mySegment->p1.y;
  */
  Line *l = (Line*)this;
  Point *p = l->Intersect((Line *)mySegment);
  if (l->parallel)
  {
    return NULL;
  }
  if (p != NULL)
  {
    if (p->x > p1.x && p->x > p2.x) return NULL;
    if (p->x < p1.x && p->x < p2.x) return NULL;
    if (p->y > p1.y && p->y > p2.y) return NULL;
    if (p->y < p1.y && p->y < p2.y) return NULL;
    if (p->x > mySegment->p1.x && p->x > mySegment->p2.x) return NULL;
    if (p->x < mySegment->p1.x && p->x < mySegment->p2.x) return NULL;
    if (p->y > mySegment->p1.y && p->y > mySegment->p2.y) return NULL;
    if (p->y < mySegment->p1.y && p->y < mySegment->p2.y) return NULL;
    return p;
  }
  else
    return NULL;
}

