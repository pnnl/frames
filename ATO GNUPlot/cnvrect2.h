#ifndef CNVRT_H
#define CNVRT_H

#include <fstream.h>
#include <new.h>
#include "..\common files\csv.h"

void newHandler() throw(xalloc);
void setHandler();

class Memory
{
 private:
  Memory *Pred,*Succ;
  void *Pointer;
  char File[20];
  int  Line;
  static int on;

 public:
  void Dump(char *title);
  void Off();
  void On();
  Memory(void *ref=NULL,Memory *p=NULL,Memory *s=NULL,char *f=NULL,int l=0);
  void New(void *p,char *file,int line);
  // a more convienient format of New
  #define Newed(a) New(a,__FILE__,__LINE__)
  void Deleting(void *p);
};

extern Memory mem;

class ConvertData
{
 public:
  double CenterX,CenterY,TimeSpan,TimeInterval;
  char Source[30];
  char fname[128];

  ConvertData(char *GIDFilepath,char *Glyph);
  ~ConvertData();
};

class ATOContaminant
{
 public:
  int NumX,NumY;
  double Time,*X,*Y,**Conc; // Concentration and Deposition Rates are stored in Conc
  char Type[30],Particle[20],ConName[50],ConId[20],Units[20];

  ATOContaminant();
  ~ATOContaminant();
  void Destroy();
  long Read(icsv &f,char *name,char *id,double time);
  double CalcX(double X,double Y,double CenterX,long polar); // km
  double CalcY(double X,double Y,double CenterY,long polar); // km
  int Rad();
  double ChmConc(int i,int j,double min=0.0);
  double RadConc(int i,int j,double min=0.0);
  void Write(ocsv &f,int numtimes,ATOContaminant *air,ATOContaminant *dep,
             double CenterX,double CenterY,long polar,int AirConc);
  void XExtrema(double &min,double &max);
  void YExtrema(double &min,double &max);
  void ConcExtrema(double &min,double &max);
  void ConcSeriesExtrema(int numtimes,ATOContaminant *a,double &Min,double &Max);
  void WriteGNU(ofstream &d,char *dname,ofstream &o,int numtimes,ATOContaminant *air,ATOContaminant *dep,
                double CenterX,double CenterY,long polar);
  long operator ==(ATOContaminant &a);
  ATOContaminant & operator +=(ATOContaminant &a);
  ATOContaminant & operator /=(double divisor);
  ATOContaminant & operator =(ATOContaminant &a);
  long Empty();
  long AirConc();
  void Integrate(ATOContaminant &a);
  void Integrate(ATOContaminant &total,ATOContaminant &a,ATOContaminant &b);
  double getTime();
  double Interp(double y1,double y2,double x1,double x2,double x);
  void Average(double start,double end,int numtimes,ATOContaminant *values);
};

class ATOEntry
{
 private:
  int NumTimes;
  ATOContaminant **result;

 public:
  char PConName[50];
  char PConId[20];
  char ConName[50];
  char ConId[20];

  ATOEntry();
  ~ATOEntry();
  void Destroy();
  void Read(icsv &f,char *pname,char *pid,char *name,char *id,int numtimes);
  void IntegrateDeposition();
  void Write(ocsv &air,ocsv &dep,double cx,double cy,long gridtype);
  void WriteGNU(ofstream &data,char *dname,ofstream &script,double cx,double cy,long gridtype);
  void Average(double timespan,double timeinterval,ATOEntry *instantaneous);
};

class Grid
{
 private:
  double CX,CY;
  int Type;
  char DName[128];
  ocsv *air,*dep;
  ofstream *d,*s;

 public:
  Grid(char *GridFilePath,double cx,double cy,char *gridtype);
  ~Grid();
  void Write(ATOEntry &ae);
  void WriteGNU(ATOEntry &ae);
};

class ATO
{
 private:
  int NumDataSets,NumCon,NumProg,NumTimes,CurrentCon,CurrentProg,Ok;
  double CurrentTime;
  char ResultType[10],CoordinateType[10];
  icsv *f;

 public:
  char PConName[50];
  char PConId[20];

  ATO(char *ATOFilepath,char *Glyph);
  ~ATO();
  long Read(ATOEntry &ae);
  char *GridType();
  int NumberConstituent();
  int CurrentConstituent();
};

#endif

