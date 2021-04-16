#ifndef LINEH
#define LINEH

#include <stdlib.h>
#include <math.h>

class Line;
class Segment;

class Point
{
 public:
  float x,y;
  Point();
  Point(float x, float y);
  double DistanceTo(Point *p);
  double DistanceTo(Line *z);
  double DistanceTo(Segment *z);
  Point& operator =(Point &p);
};

class Line
{
 private:
  Point a;
 public:
  float m;
  float b;
  float xequal;
  int vertical;
  int parallel;

  Line();
  Line(float x);
  Line(float m, float b);
  Line(Point p1, Point p2);
  Line(float x1,float y1,float x2,float y2);
  void lineinit(float x1,float y1,float x2,float y2);

  Point *Intersect(Line *myLine);
};

class Segment:public Line
{
 public:
  double len;
  Point p1,p2;

  Segment();
  Segment(Point *p1, Point *p2);
  Segment(float x1,float y1,float x2,float y2);
  void segmentinit(float x1,float y1,float x2,float y2);

  Point *Intersect(Segment *mySegment);
};

#endif
