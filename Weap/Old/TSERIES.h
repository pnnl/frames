#ifndef TSERIES_H
#define TSERIES_H
#include "consts.h"


int GCD(int n, int m);
int isequal(const void *a, const void *b);

class TimeSeries
{
 public:
  int whole;
  int decimal;
  int exponent;
  float *Time;
  float *Value;
  float TotalTime;
  char IDName[MaxName];             // casid
  char Name[MaxName];               // constituent name
  char TimeUnits[MaxUnits];
  char Units[MaxUnits];
  int NumSteps;
  TimeSeries();
  ~TimeSeries();

  // different funtion to read in from different sources
  void Read(fcsv *TVC);
  void ReadGID(GIDFILE *f, int source, int lifeidx);
  void ReadFreqGID(GIDFILE *f, int lifeidx);
  void ReadTWI(GIDFILE *f, element *cas, element *time, element *dose, int org, int chm, int med);

  // available functions
  float InterpTime(float CurrentConc,int count);
  float GetIntervalDuration(int t, float valu);
  float GetTotDuration(float conc);
  int GetDuration(int *count,float CurrentConc,float *Duration);
  void Init(int numsteps);
  void InsertTimes(TimeSeries *T);
  void Adjust(TimeSeries *T, int locidx);
  void Mult(TimeSeries *T);
  void AddIn(TimeSeries *T);
  float DiscreteInterp(float t);
  float ContinInterp(float t);
  float GetMinValue();
  float GetMaxValue();
  void Break(float conc);
  void MakeConsistant(TimeSeries *T);
  void Insert(int count,float t,float v);
  void AddEntry(float t,float v);
  void Copy(TimeSeries *T);
  void CopyTo(TimeSeries *T);
  void Div(float scalor);
  void Rankorder();
  void WriteSeries(fcsv *fle);
  void PrintSeries();
};

#endif


