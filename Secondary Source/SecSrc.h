//---------------------------------------------------------------------------
#ifndef SecSrcH
#define SecSrcH
//---------------------------------------------------------------------------

#ifndef scfClassH
#define scfClassH
//---------------------------------------------------------------------------

#include "frames.h"

class AGGcon {
  public:
  char name[64];
  char cas[32];
  int numTimes;
  int numProgeny;
  fpos_t filepos;
  fpos_t concpos;
  AGGcon **prog;

  AGGcon();
  AGGcon(icsv *inf);
  AGGcon(icsv *inf, int nprog);
  ~AGGcon();
};

class AGG {
  public:
  icsv *inf;
  int numCon;
  AGGcon **pCon;

  AGG();
  ~AGG();
};

#endif
