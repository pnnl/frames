#ifndef LSERIES_H
#define LSERIES_H

#include "consts.h"
#include "tseries.h"
#include "cseries.h"

class LocSeries
{
 public:
  int NumLoc;
  int NumLocFreq;
  TimeSeries LocFreq;
  ConSeries *Loc;
  LocSeries();
  ~LocSeries();
  void InitConc(int numloc);
  void ReadConcWCF(char *ConFile,char *locname);
  void Combine(GIDFILE *f,ConSeries *CS);
};

#endif


