/*______________________________________________________________________________

   Date:       1993 - 2004
   Company:    Pacific Northwest National Laboratories
               Battelle Corporation
________________________________________________________________________________
__Modifiication  History________________________________________________________

  DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#ifndef affClassH
#define affClassH

#include "csv.h"
#include "series.h"

class AFFCon
{
  public:
  char name[SMALLSTRING];
  char cas[SMALLSTRING];
  char tunit[SMALLSTRING];
  char vunit[SMALLSTRING];
  int numStep;
  int numProg;
  fpos_t filepos;
  fpos_t xypos;
  AFFCon **prog;

  void Init();
  AFFCon();
  AFFCon(icsv *inf, bool isProg);
  AFFCon (char *_name, char *_cas, int count);
  ~AFFCon();
};

class AFFSet
{
  public:
  char name[SMALLSTRING];
  char type[SMALLSTRING];
  double area;
  double extHt;
  double adjHt;
  double extVel;
  double extTmp;
  double ambTmp;
  fpos_t filepos;
  fpos_t fluxpos;
  int numCon;
  int numFluxTypes;
  AFFCon **pCon;


  void Init();
  AFFSet();
  AFFSet(icsv *inf);
  ~AFFSet();
  int convertAFF(icsv *inf, ocsv *eOut, char *casList);
  int unconvertAFF(icsv *inf, ocsv *eOut, char *casList, char *nameList, char *degList, char *secList, int branching);
};

class AFF
{
  public:
  icsv *inf;
  int numSet;
  long affHead;
  char name[SMALLSTRING];
  AFFSet **set;

  void Init();
  AFF();
  AFF(char *fuiname, char *modId);
  ~AFF();
};

#endif
