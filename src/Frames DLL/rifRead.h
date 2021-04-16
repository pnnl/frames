/*______________________________________________________________________________

   Date:       1993 - 2004
   Company:    Pacific Northwest National Laboratories
               Battelle Corporation
________________________________________________________________________________
__Modifiication  History________________________________________________________

  DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#ifndef rifReadH
#define rifReadH

#include "csv.h"
#include "gid.h"

class RIFCon
{
  public:
  int nProg;
  int nTime;
  char casid[SMALLSTRING];
  char name[SMALLSTRING];
  fpos_t fpos;
  fpos_t *time;
  RIFCon **prog;

  ~RIFCon();
  RIFCon(int numTime);
  RIFCon(icsv *inf, int numLoc);
  double getTime(int timeIdx);
  int LoadExposuresByTime(char *cas, int timeIdx, int numLoc, expRoute **exp);
};

//------------------------------------------------------------------------------
class RIFAge
{
  public:
  fpos_t fpos;
  double agemin;
  double agemax;
  int nChem;
  RIFCon **chem;

  ~RIFAge();
  RIFAge(icsv *inf, int numpt, int numcon);
//  int LoadStartTimes(char *cas, int len, char *timestring);
//  double getOption1Data(double start, char *cas, char *parentCAS, char *path, char *route, int locIdx, char *ep, int organ, int numLoc);
//  double getHealthImpactsByTime(int ageIdx, int orgIdx, int timeIdx, char *cas, char *parentCAS, char *path, char *route, char *measure, char *unit, int numLoc, char *expdur);
};

//------------------------------------------------------------------------------
class RIFSet
{
  private:
  fpos_t fpos;

  public:
  char locExp[SMALLSTRING];  // acute or chronic
  char locType[SMALLSTRING]; // file  extension(s) indicates location type (scf=soil,wcf=water,ato=air)
  char locMedia[SMALLSTRING]; // file  qualifier(s) indicates location media (aquifer, surface water, sediment ,soil)

  // age groups for this dataset
  int nAge;
  RIFAge **age;

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
  double duration;
  double population;

  ~RIFSet();
  RIFSet();
  void read(icsv *inf);
  int getDatasetInfo(int *numLoc, int *numAge, char *typ, char *med, char *exp);
  int getExposurePoint(int locIdx, double *xp, double *yp);
  int getTimeCount(int ageIdx, char *parentCAS);
  double getTime(int ageIdx, char *parentCAS, int timeIdx);
  int getAgeGroup(int ageIdx, double *min, double *max);

  // In the next series of calls
  // if *cas is the parent then *parentCAS should be the same
  // if *cas is the progeny then *parentCAS should be the cas's parent

  // functions to load exposures and then get the values
  // read functions repopulates exp and sets nExp
  int LoadExposuresByTime(int ageIdx, char *cas, char *parentCAS, int timeIdx);
  void getExposure(int expIdx, char *path, char *route, char *measure, char *unit);

  // functions to load a time series and then get the values
  // read function repopulates the rtime and rvalue and sets nTIme
  int LoadTimeSeries(int locIdx, int ageIdx, char *cas, char *parentCAS, char *path, char *route, char *measure, char *unit);
  int getTimeSeries(double *stime, double *svalue);
  void getTimeAndValue(int idx, double *stime, double *svalue);

  // functions to convert an EPF dataset to a dataset without progenies
  int convertRIF(char *casList);
  void aggregateExposures(int ageIdx, char *cas, char *parentCas, int numTimes, bool newAggregate);
  int writeConstituent(char *cas, char *name, int numTimes);
};

#endif