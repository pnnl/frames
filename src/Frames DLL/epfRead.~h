/*______________________________________________________________________________

   Date:       1993 - 2004
   Company:    Pacific Northwest National Laboratories
               Battelle Corporation
________________________________________________________________________________
__Modifiication  History________________________________________________________

  DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#ifndef epfReadH
#define epfReadH

#include "csv.h"
#include "gid.h"

class EPFCon
{
  public:
  int nProg;
  int nTime;
  char casid[SMALLSTRING];
  char name[SMALLSTRING];
  fpos_t fpos;
  fpos_t *time;
  EPFCon **prog;

  ~EPFCon();
  EPFCon(int numTime);
  EPFCon(icsv *inf);
  double getTime(int timeIdx);
  int LoadExposuresByTime(char *cas, int timeIdx, expRoute **exp);

};

class EPFSet
{
  private:
  fpos_t *fpos;

  public:
  char locExp[SMALLSTRING];  // acute or chronic
  char locType[SMALLSTRING]; // file  extension(s) indicates location type (scf=soil,wcf=water,ato=air)
  char locMedia[SMALLSTRING]; // file  qualifier(s) indicates location media (aquifer, surface water, sediment ,soil)

  // chemicals for this dataset
  int nChem;
  EPFCon **chem;

  // locations for this dataset
  int nLoc;
  double *x;
  double *y;

  // stuff for use of a current chemical exposures
  int nExp;
  expRoute *exp;

  // stuff for roll up
  // list of chems from the CON in GID file
  double ***tempValues;
  _EXPLIST erp;

  // stuff for use of a current time series
  int nTime;
  double *rtime;
  double *rvalue;
  double acuteduration;
  double duration;

  EPFSet();
  ~EPFSet();
  void epfRead(icsv *inf);
  int getDatasetInfo(int *numLoc, char *typ, char *med, char *exp);
  int getExposurePoint(int locIdx, double *xp, double *yp);
  int getTimeCount(char *parentCAS);
  double getTime(char *parentCAS, int timeIdx);

  // In the next series of calls
  // if *cas is the parent then *parentCAS should be the same
  // if *cas is the progeny then *parentCAS should be the cas's parent

  // functions to load exposures and then get the values
  // read functions repopulates exp and sets nExp
  int LoadExposuresByTime(char *cas, char *parentCAS, int timeIdx);
  void getExposure(int expIdx, char *path, char *route, char *unit);

  // functions to load a time series and then get the values
  // read function repopulates the rtime and rvalue and sets nTIme
  int LoadTimeSeries(int locIdx, char *cas, char *parentCAS, char *path, char *route);
  int getTimeSeries(double *stime, double *svalue);
  void getTimeAndValue(int timeIdx, double *stime, double *svalue);

  // functions to convert an EPF dataset to a dataset without progenies
  int convertEPF(char *casList);
  void aggregateExposures(char *cas, char *parentCas, int numTimes, bool newAggregate);
  int writeConstituent(char *cas, char *name, int numTimes);
};

/*
class EPF
{
  char epfname[SMALLSTRING]; // file name
  char source[SMALLSTRING];  // module id
};
*/

#endif
