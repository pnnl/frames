/*______________________________________________________________________________

   Date:       1993 - 2004
   Company:    Pacific Northwest National Laboratories
               Battelle Corporation
________________________________________________________________________________
__Modifiication  History________________________________________________________

  DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#ifndef series_h
#define series_h

#include "csv.h"
#include "gid.h"

class Series
{
  public:
  int count;
  int whole;
  int decimal;
  int exponent;
  double *xValues;
  double *yValues;
  double xInterval;
  double yInterval;
  char Id[SMALLSTRING];
  char Name[SMALLSTRING];
  char xUnits[SMALLSTRING];
  char yUnits[SMALLSTRING];

    ~Series();
    Series();
    Series(int n);
    void Init(int n);
    void Init(int n, double *Xvals, double *Yvals);
    void Copy(Series *T);
    void CopyTo(Series *T);
    Series* Clone();

    void Read(icsv *fle, int n, int valueIndex=1);
    void Read(fcsv *fle, int n, int valueIndex=1);
    void Read(fcsv *fle, int n);
    void ReadSeries(fcsv *fle);
    void Read(fcsv *fle);                                      //from timeseries
    void ReadTWI(GIDFILE *f, element *cas, element *time, element *dose, int org, int chm, int med);
    void ReadFreqGID(GIDFILE *f,int lifeidx);
    void ReadGID(GIDFILE *f,int source, int lifeidx);
    void WriteSeries(fcsv *fle);
    void PrintSeries();
    int XNotLinear();
    int YNotLinear();
    double MaxY(int begIdx, int endIdx);
    double MinY(int begIdx, int endIdx);
    double MinX();
    double MaxX();
    double MinY();
    double MaxY();
    void MinXwithY(double *x, double *y);
    void MaxXwithY(double *x, double *y);
    void MinYwithX(double *x, double *y);
    void MaxYwithX(double *x, double *y);

    double DiscreteInterpY(double x);
    double InterpY(double x);                                  //ValueAt
    double InterpX(double y, int idx);                         //InterpTime

    int Find(double x);
    void Insert(int idx, double x, double y);
    void AppendEntry(double x, double y);                      //addentry used by sync only
    void PrependEntry(double x, double y);
    void Replace(int n, double x, double y);                   //add
    void InsertEnds(Series *T);
    void InsertXpts(double y);                                 //Break
    int InsertXpts(Series *T);                                 //InsertTimes

    double ExceedDelta(double y, int idx);                          //GetIntervalDuration int double
    double ExceedDelta(double y, int begIdx, int endIdx);
    Series *MakeExceed(int probability, int resolution);
    Series *MakeInterval(int year, double offX, double interval, int size, double error);
    void Rankorder();

    void Div(double scalar);
    void Mult(double scalar);
    void Mult(Series *T);
    double Integrate(double begX, double endX);
    double Average(double begX, double endX);
//    void RunAvg(double Period, int *NumRes, double *TimesRes, double *ValuesRes);

    void Adjust(Series *T);
    void AddIn(Series *T);
    int Add(Series *T);
};

#endif

