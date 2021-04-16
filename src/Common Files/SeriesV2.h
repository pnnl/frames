#ifndef SeriesV2_H
#define SeriesV2_H

#include "Series.h"
#include "typedefs.h"

class Series_v2:public Series
{
private:
public:
  double eco;
  double man;
  char *measure;
  char msgeco[MEDSTRING];
  char msgman[MEDSTRING];
  char vartime[SMALLSTRING];   // variable name for times
  char varvalu[SMALLSTRING];   // variable name for values

  Series_v2(char *tunit, char *funit);
  Series_v2(char *vtime, char *tunit, char *vflux, char *funit, char *meas);

  int ReadTimes(long pid, char *setname, int *tIdx, int tI);  
  int ReadTimes(long pid, char *setname, char *_vartime, int *tIdx, int tI);
  int ReadDatasets(long pid, char *setname, int *tIdx, int tI, int *vIdx = NULL, int vI = 0);
  int ReadDatasets(long pid, char *setname, char *_vartime, char *_varvalu, int *tIdx, int tI, int *vIdx = NULL, int vI = 0);
  int WriteDatasets(long pid, icsv *inf, long numsteps, char *setname, int *idx, int ii);
  int WriteFile(ocsv *fp);
};



#endif