//---------------------------------------------------------------------------

#pragma hdrstop

#include "PolyClass.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)
//------------------------------------------------------------------------------
PointClass::PointClass()
{
  x = 0;
  y = 0;
}
//------------------------------------------------------------------------------
PointClass::~PointClass()
{
}
//------------------------------------------------------------------------------
void PointClass::operator= (PointClass* pointb)
{
  this->x = pointb->x;
  this->y = pointb->y;
}
//------------------------------------------------------------------------------
int PointClass::operator== (PointClass* pointb)
{
  if(this->x != pointb->x) return 0;
  if(this->y != pointb->y) return 0;
  return 1;
}
//------------------------------------------------------------------------------
int PointClass::compare(PointClass* pointb)
{
  if(x != pointb->x) return 0;
  if(y != pointb->y) return 0;
  return 1;
}
//------------------------------------------------------------------------------
int PointClass::compare(double xcoord,double ycoord)
{
  if(x != xcoord)return 0;
  if(y != ycoord)return 0;
  return 1;
}
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
LineClass::LineClass()
{
  begin = new PointClass();
  end = new PointClass();
}
//------------------------------------------------------------------------------
LineClass::~LineClass()
{
  if(begin)delete begin;
  if(end)delete end;
}
//------------------------------------------------------------------------------
void LineClass::operator=(LineClass* lineb)
{
  this->begin->x = lineb->begin->x;
  this->begin->y = lineb->begin->y;
  this->end->x = lineb->end->x;
  this->end->y = lineb->end->y;
}
//------------------------------------------------------------------------------
void LineClass::SetLine(double x0,double y0,double x1,double y1)
{
  begin->x = x0;
  begin->y = y0;
  end->x = x1;
  end->y = y1;
}
//------------------------------------------------------------------------------
int LineClass::operator==(LineClass* lineb)
{
  if(this->begin->x==lineb->begin->x && this->begin->y==lineb->begin->y && this->end->x==lineb->end->x && this->end->y==lineb->end->y)
    return 1;
  else
    return 0;
}
//------------------------------------------------------------------------------

PolygonClass::PolygonClass()
{
  vpoint = new PointClass();
  points = NULL;
  size = 0;
}
//------------------------------------------------------------------------------
PolygonClass::~PolygonClass()
{
  if(vpoint) delete vpoint;
  if(points)
  {
    for(int i=0;i<size;i++)
      delete points[i];
    delete[] points;
  }
}
//------------------------------------------------------------------------------
void PolygonClass::addPoint(PointClass* point)
{
  int i;
  PointClass **temppoints;
  temppoints = new PointClass*[size];
  
  for(i=0;i<size;i++)
    temppoints[i] = new PointClass();
  for(i=0;i<size;i++)
  {
    temppoints[i]->x = points[i]->x;
    temppoints[i]->y = points[i]->y;
  }
  if(size > 0)
  {
    for(i=0;i<size;i++)
      delete points[i];
    delete[] points;
  }
  points = new PointClass*[size+1];
  for(i=0;i<size+1;i++)
    points[i] = new PointClass();
  for(i=0;i<size;i++)
  {
    points[i]->x = temppoints[i]->x;
    points[i]->y = temppoints[i]->y;
  }
  points[size]->x = point->x;
  points[size]->y = point->y;
  size++;
  delete temppoints;
}
//------------------------------------------------------------------------------
void PolygonClass::addPoint(double x,double y)
{
  int i;
  PointClass **temppoints;
  temppoints = new PointClass*[size];
  for(i=0;i<size;i++)
    temppoints[i] = new PointClass();
  for(i=0;i<size;i++)
  {
    temppoints[i]->x = points[i]->x;
    temppoints[i]->y = points[i]->y;
  }
  if(size > 0)
  {
    for(i=0;i<size;i++)
      delete points[i];
    delete[] points;
  }
  points = new PointClass*[size+1];
  for(i=0;i<size+1;i++)
    points[i] = new PointClass();
  for(i=0;i<size;i++)
  {
    points[i]->x = temppoints[i]->x;
    points[i]->y = temppoints[i]->y;
  }
  points[size]->x = x;
  points[size]->y = y;
  size++;
  delete temppoints;
}

//------------------------------------------------------------------------------
int PolygonClass::checkpoint(PointClass* point)
{
  int check = 0;
  int tempcheck;
  for(int i=0;i<size;i++)
  {
    tempcheck = points[i]->compare(point);
    if(tempcheck)
    {
      check = 1;
      break;
    }
  }
  return check;
}
//------------------------------------------------------------------------------
int PolygonClass::checkpoint(double xcoord,double ycoord)
{
  int check = 0;
  int tempcheck;
  for(int i=0;i<size;i++)
  {
    tempcheck = points[i]->compare(xcoord,ycoord);
    if(tempcheck)
    {
      check = 1;
      break;
    }
  }
  return check;
}
//------------------------------------------------------------------------------
void PolygonClass::assign(PolygonClass* poly)
{
  int i;
  this->vpoint = new PointClass();
  this->points = new PointClass*[poly->size];
  for(i=0;i<poly->size;i++)
    this->points[i] = new PointClass();
  this->vpoint->x = poly->vpoint->x;
  this->vpoint->y = poly->vpoint->y;
  for(i=0;i<poly->size;i++)
    this->addPoint(poly->points[i]);
  size = poly->size;
}
//------------------------------------------------------------------------------
double getAngle(PointClass* point,PointClass* centerpoint)
{
  double radius;
  double ydist;
  double xdist;
  double tempangle;
  xdist = point->x - centerpoint->x;
  ydist = point->y - centerpoint->y;
  radius = sqrt(pow(point->x - centerpoint->x,2)+pow(point->y - centerpoint->y,2));
  tempangle = acos(xdist / radius);
  if((xdist<0.0 && ydist<0.0)||(xdist>0.0 && ydist<0.0))
  {
    if(tempangle<0.0)
      tempangle = 6.28 + tempangle;
    else
      tempangle = 6.28 - tempangle;
  }
  return tempangle;
}
//------------------------------------------------------------------------------
double TriangleArea(PointClass* p1,PointClass* p2,PointClass* p3)
{
  double sidea;
  double sideb;
  double sidec;
  double semiperimeter;
  double area;
  sidea = sqrt(pow(p1->x - p2->x,2)+pow(p1->y - p2->y,2));
  sideb = sqrt(pow(p2->x - p3->x,2)+pow(p2->y - p3->y,2));
  sidec = sqrt(pow(p3->x - p1->x,2)+pow(p3->y - p1->y,2));
  semiperimeter = 0.5 * (sidea + sideb + sidec);
  area = sqrt(semiperimeter * (semiperimeter - sidea) * (semiperimeter - sideb) * (semiperimeter - sidec));
  return area;
}
//------------------------------------------------------------------------------
double PolygonClass::getarea()
{
  int i;
  double area;
  area = TriangleArea(points[0],points[1],points[2]);
  for(i=3;i<size;i++)
  {
    area += TriangleArea(points[0],points[i-1],points[i]);
  }
  return area;
}
//------------------------------------------------------------------------------
double PolygonClass::height()
{
  int i;
  double ht;
  ht = 0.0;
  for(i=0;i<size;i++)
    if(points[i]->y>ht) ht = points[i]->y;
    return ht;
}
//------------------------------------------------------------------------------
double PolygonClass::farright()
{
  int i;
  double rt;
  rt = 0.0;
  for(i=0;i<size;i++)
    if(points[i]->x>rt) rt = points[i]->x;
    return rt;
}
//------------------------------------------------------------------------------
int PointOnLine(double xc,double yc,double xa,double ya,double xb,double yb)
{
  double len;
  double r;
  double s;
  len = sqrt(pow(xb-xa,2)+pow(yb-ya,2));
  r = ((ya-yc)*(ya-yb)-(xa-xc)*(xb-xa))/(pow(len,2));
  s = ((ya-yc)*(xb-xa)-(xa-xc)*(yb-ya))/(pow(len,2));
  if(0.0<=r&&r<=1&&s==0)
    return 1;
  else
    return 0;
}
//------------------------------------------------------------------------------
int PointOnPolygonLine(PointClass* pnt,PolygonClass* poly,PolygonClass* fracpoly)
{
  int i;
  int count;
  count = 0;
  for(i=1;i<poly->size;i++)
  {
    if(PointOnLine(pnt->x,pnt->y,poly->points[i-1]->x,poly->points[i-1]->y,poly->points[i]->x,poly->points[i]->y))
    {
      count++;
      if(fracpoly->checkpoint(pnt)==0)fracpoly->addPoint(pnt);
    }
  }
  if(PointOnLine(pnt->x,pnt->y,poly->points[poly->size-1]->x,poly->points[poly->size-1]->y,poly->points[0]->x,poly->points[0]->y))
  {
    count++;
    if(fracpoly->checkpoint(pnt)==0)fracpoly->addPoint(pnt);
  }
  
  return count;
}
//------------------------------------------------------------------------------
int PointOnPolygonLine(double pntx,double pnty,PolygonClass* poly,PolygonClass* fracpoly)
{
  int i;
  int count;
  PointClass* pnt = new PointClass();
  pnt->x = pntx;
  pnt->y = pnty;
  count = 0;
  for(i=1;i<poly->size;i++)
  {
    if(PointOnLine(pnt->x,pnt->y,poly->points[i-1]->x,poly->points[i-1]->y,poly->points[i]->x,poly->points[i]->y))
    {
      count++;
      if(fracpoly->checkpoint(pnt)==0)fracpoly->addPoint(pnt);
    }
  }
  if(PointOnLine(pnt->x,pnt->y,poly->points[poly->size-1]->x,poly->points[poly->size-1]->y,poly->points[0]->x,poly->points[0]->y))
  {
    count++;
    if(fracpoly->checkpoint(pnt)==0)fracpoly->addPoint(pnt);
  }
  delete pnt;
  return count;
}
//------------------------------------------------------------------------------
int PointInsidePoly(PointClass* pnt,PolygonClass* poly,PolygonClass* fracpoly)
{
  int i, j, c = 0;
  for (i = 0, j = poly->size-1; i < poly->size; j = i++)
  {
    if ((((poly->points[i]->y<=pnt->y) && (pnt->y<poly->points[j]->y)) ||((poly->points[j]->y<=pnt->y) && (pnt->y<poly->points[i]->y))) &&
      (pnt->x <= (poly->points[j]->x - poly->points[i]->x) * (pnt->y - poly->points[i]->y) / (poly->points[j]->y - poly->points[i]->y) + poly->points[i]->x))
      c = !c;
  }
  if(c)
    if(fracpoly->checkpoint(pnt) == 0) fracpoly->addPoint(pnt);
    return c;
}
//------------------------------------------------------------------------------
int PointInsidePoly(PointClass* pnt,PolygonClass* poly)
{
  int i, j, c = 0;
  for (i = 0, j = poly->size-1; i < poly->size; j = i++)
  {
    if ((((poly->points[i]->y<=pnt->y) && (pnt->y<poly->points[j]->y)) ||((poly->points[j]->y<=pnt->y) && (pnt->y<poly->points[i]->y))) &&
      (pnt->x <= (poly->points[j]->x - poly->points[i]->x) * (pnt->y - poly->points[i]->y) / (poly->points[j]->y - poly->points[i]->y) + poly->points[i]->x))
      c = !c;
  }
  return c;
}
//------------------------------------------------------------------------------
Point* LineIntersectsPoly(PointClass* pnt1,PointClass* pnt2,PolygonClass* poly)
{
  int i;
  Segment* seg1;
  Segment* seg2;
  Point* intpnt;
  seg1 = new Segment(pnt1->x,pnt1->y,pnt2->x,pnt2->y);
  for(i=1;i<poly->size;i++)
  {
    seg2 = new Segment(poly->points[i-1]->x,poly->points[i-1]->y,poly->points[i]->x,poly->points[i]->y);
    intpnt = seg1->Intersect(seg2);
    delete seg2;
    if(intpnt)
      return intpnt;
  }
  seg2 = new Segment(poly->points[poly->size-1]->x,poly->points[poly->size-1]->y,poly->points[0]->x,poly->points[0]->y);
  intpnt = seg1->Intersect(seg2);
  delete seg1;
  delete seg2;
  if(intpnt)
    return intpnt;
  return NULL;
}
//------------------------------------------------------------------------------
int PointEqualsVertex(PointClass* pnt,PolygonClass* poly,PolygonClass* fracpoly)
{
  int i;
  double eps = 1.0E-04;
  for(i=0;i<poly->size;i++)
  {
    if(fabs(pnt->x - poly->points[i]->x)<eps*fabs(poly->points[i]->x) && fabs(pnt->y - poly->points[i]->y)<eps*fabs(poly->points[i]->y))
    {
      if(fracpoly->checkpoint(pnt) == 0) fracpoly->addPoint(pnt);
      return 1;
    }
  }
  return 0;
}
//------------------------------------------------------------------------------
void PolygonClass::findpointinside(PolygonClass* thispoly,PolygonClass* poly)
{
  int i,j;
  int goodx;
  int goody;
  int inpoly1;
  int inpoly2;
  double eps = 1.0E-04;
  
  for(i=1;i<size;i++)
  {
    //        vpoint->x = points[0]->x + ((points[i]->x - points[i-1]->x)/2);
    //        vpoint->y = points[0]->y + ((points[i]->y - points[i-1]->y)/2);
    vpoint->x = points[i]->x-((points[i]->x - points[i-1]->x)/2);
    vpoint->y = points[i]->y-((points[i]->y - points[i-1]->y)/2);
    goodx = 1;
    goody = 1;
    for(j=0;j<size;j++)
    {
      if(fabs(vpoint->x - points[j]->x)<eps*fabs(points[j]->x)) goodx = 0;
      if(fabs(vpoint->y - points[j]->y)<eps*fabs(points[j]->y)) goody = 0;
    }
    if(goodx==1 && goody==1)
    {
      inpoly1 = PointInsidePoly(vpoint,thispoly);
      inpoly2 = PointInsidePoly(vpoint,poly);
      if(inpoly1!=0 && inpoly2!=0)
      {
        i = size;
      }
    }
  }
}
//------------------------------------------------------------------------------
double PolygonClass::overlapfraction(PolygonClass* temppoly)
{
  int i;
  int insidepoints;
  int interceptcount;
  int *thisinsidecheck;
  int *temppolyinsidecheck;
  int equalcheck;
  int startindex;
  int *thisindices;
  int noinsidepoints;
  PolygonClass* fracpoly;
  Point* temppoint;
  double area1;
  double area2;
  double area3;
  int onlinecount;
  area1 = this->getarea();
  area2 = temppoly->getarea();
  insidepoints = 0;
  thisinsidecheck = new int[size];
  temppolyinsidecheck = new int[temppoly->size];
  fracpoly = new PolygonClass();
  noinsidepoints = 0;
  for(i=0;i<size;i++)
    thisinsidecheck[i] = 0;
  for(i=0;i<temppoly->size;i++)
    temppolyinsidecheck[i] = 0;
  for(i=0;i<size;i++)
  {
    onlinecount = PointOnPolygonLine(points[i],temppoly,fracpoly);
    interceptcount = PointInsidePoly(points[i],temppoly,fracpoly);
    equalcheck = PointEqualsVertex(points[i],temppoly,fracpoly);
    if(interceptcount % 2 != 0 || equalcheck == 1||onlinecount)
    {
      thisinsidecheck[i] = 1;
      insidepoints++;
    }
  }
  //all of this polygon is located in the temppoly polygon
  if(insidepoints == size)
  {
    delete[] thisinsidecheck;
    delete[] temppolyinsidecheck;
    return 1.0;
  }
  if(insidepoints > 0) noinsidepoints = 1;
  //all of this polygon is either outside of the temppoly polygon or else
  //all of the temppoly polygon is located within the polygon
  insidepoints = 0;
  for(i=0;i<temppoly->size;i++)
  {
    onlinecount = PointOnPolygonLine(temppoly->points[i],this,fracpoly);
    interceptcount = PointInsidePoly(temppoly->points[i],this,fracpoly);
    equalcheck = PointEqualsVertex(temppoly->points[i],this,fracpoly);
    if(interceptcount % 2 != 0 || equalcheck == 1 ||onlinecount)
    {
      temppolyinsidecheck[i] = 1;
      insidepoints++;
    }
  }
  if(insidepoints == temppoly->size)
  {
    delete[] thisinsidecheck;
    delete[] temppolyinsidecheck;
    return area2/area1;
  }
  if(insidepoints == 0 && noinsidepoints == 0)
  {
    return 0.0;
  }
  //walk the perimeters of the two polygons to get the intersection
  //of the polygons and return the percentage of the area walk this polygon
  startindex = 0;
  thisindices = new int[size];
  for(i=0;i<size;i++)
  {
    if(thisinsidecheck[i]==0)
    {
      startindex = i;
      break;
    }
  }
  for(i=startindex;i<size;i++)
    thisindices[i-startindex] = i;
  for(i=0;i<startindex;i++)
    thisindices[i+startindex] = i;
  for(i=0;i<size-1;i++)
  {
    temppoint = LineIntersectsPoly(points[i],points[i+1],temppoly);
    if(temppoint)
    {
      onlinecount = PointOnPolygonLine(temppoint->x,temppoint->y,temppoly,fracpoly);
    }
    if(temppoint&&onlinecount)
    {
      if(fracpoly->checkpoint(temppoint->x,temppoint->y)==0)fracpoly->addPoint(temppoint->x,temppoint->y);
    }
  }
  temppoint = LineIntersectsPoly(points[size-1],points[0],temppoly);
  if(temppoint)
  {
    onlinecount = PointOnPolygonLine(temppoint->x,temppoint->y,temppoly,fracpoly);
  }
  if(temppoint&&onlinecount)
  {
    if(fracpoly->checkpoint(temppoint->x,temppoint->y)==0)fracpoly->addPoint(temppoint->x,temppoint->y);
  }
  
  fracpoly->findpointinside(this,temppoly);
  fracpoly->sortvertices();
  if(fracpoly->points)
    area3 = fracpoly->getarea();
  else
    area3 = 0.0;
  delete[] thisinsidecheck;
  delete[] temppolyinsidecheck;
  if(fracpoly)
    delete fracpoly;
  return area3/area2;
}
//------------------------------------------------------------------------------
void PolygonClass::sortvertices()
{
  int i,j;
  int lastexchangeindex;
  double *angles;
  double tempangle;
  PointClass* temppoint;
  PointClass** vertices;
  angles = new double[size];
  temppoint = new PointClass();
  vertices = new PointClass*[size];
  for(i=0;i<size;i++)
    vertices[i] = new PointClass();
  for(i=0;i<size;i++)
    angles[i] = getAngle(points[i],vpoint);
  i = size-1;
  while(i>0)
  {
    lastexchangeindex = 0;
    for(j=0;j<i;j++)
    {
      if(angles[j+1] < angles[j])
      {
        tempangle = angles[j+1];
        angles[j+1] = angles[j];
        angles[j] = tempangle;
        temppoint->x = points[j+1]->x;
        temppoint->y = points[j+1]->y;
        points[j+1]->x = points[j]->x;
        points[j+1]->y = points[j]->y;
        points[j]->x = temppoint->x;
        points[j]->y = temppoint->y;
        lastexchangeindex = j;
      }
    }
    i = lastexchangeindex;
  }
  delete temppoint;
  delete[] angles;
  for(i=0;i<size;i++)
    delete vertices[i];
  delete[] vertices;
}
//------------------------------------------------------------------------------
