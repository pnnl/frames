#ifndef CSERIES_H
#define CSERIES_H
#include "series.h"
#include <math.h>
#include <stdio.h>
#include <string.h>

class ConSeries
{
  public:
  int NumCon;
  char Name[SMALLSTRING];        // exposing medium id in bbf, twi, location name in scf
  char Type[SMALLSTRING];        // scientific name in bbf, twi
  Series *Con;
  ConSeries();
  ~ConSeries();
//  void Read(char *LocFile,char *name);
  void Init(int numcon);
  int GetIndex(char *name);
  void ReadTWI(GIDFILE *fle, element *cas, element *time, element *dose, int num, int org, int med);
  void ReadBBF(fcsv *ConTVC, int numc, int numv, int numu);
  void Read(fcsv *ConTVC, int num);
  void Adjust(Series *T);
  void Mult(Series *T);
  void AddIn(ConSeries *C);
  void FindBounds(float *min,float *max,int first);
  void SetTime(float *min,float *max);
  void SetNames(int NumLoc,ConSeries *CS);
};

#endif


