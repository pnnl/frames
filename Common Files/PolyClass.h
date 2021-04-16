//---------------------------------------------------------------------------
#ifndef PolyClassH
#define PolyClassH

#include "Line.h"

//------------------------------------------------------------------------------
class PointClass
{
    private:
    public:
        double x;
        double y;

        PointClass();
        ~PointClass();
        void operator= (PointClass* pointb);
        int operator== (PointClass* pointb);
        int compare(PointClass* pointb);
        int compare(double xcoord,double ycoord);
};
//------------------------------------------------------------------------------
class LineClass
{
    private:
    public:
        PointClass* begin;
        PointClass* end;

        LineClass();
        ~LineClass();
        void operator= (LineClass* lineb);
        int operator== (LineClass* lineb);
        void SetLine(double x0,double y0,double x1,double y1);
};
//------------------------------------------------------------------------------
class LineVector
{
    private:
    public:
        LineClass **lines;
        int size;
        LineVector();
        ~LineVector();
        void addElement(LineClass *line);
        LineClass* elementAt(int index);
};
//------------------------------------------------------------------------------
class PolygonClass
{
    private:
    public:
        PointClass *vpoint;
        PointClass **points;
        int size;
        PolygonClass();
        ~PolygonClass();
        void addPoint(PointClass* point);
        void addPoint(double x,double y);
        void assign(PolygonClass* poly);
        int checkpoint(PointClass* point);
        void sortvertices();
        double getarea();
        double overlapfraction(PolygonClass* temppoly);
        double height();
        double farright();
        void findpointinside(PolygonClass* thispoly,PolygonClass* poly);
        int checkpoint(double xcoord,double ycoord);
};
//------------------------------------------------------------------------------
int PointInsidePoly(PointClass* pnt,PolygonClass* poly);
#endif
