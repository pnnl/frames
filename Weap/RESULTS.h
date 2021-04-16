#ifndef RESULTS_H
#define RESULTS_H

//#include "consts.h"
#include "toxdata.h"
#include "cseries.h"
#include "eseries.h"
#include "line.h"

const int resolution = 10;

class Results
{
  double RegionA,RegionB,RegionC;
  double RegionD,RegionE,RegionF;
  double DurationTotal;
  Series *CvsD;
  void Regions(Series *S,ToxData *T);
 public:
  ~Results();
  Results();
  void SumRegions(int cnt, double y, Point **lclist, ToxData *T, double *A, double *B, double *C, double *D);
  void CalcRegions(ConData *tox,ConSeries *CS,char *Name,int ConNum);
  void WriteRegions(fcsv *resout,char *Id,char *Name, char *effect);
  void WriteProb(fcsv *resout);
};

#endif
