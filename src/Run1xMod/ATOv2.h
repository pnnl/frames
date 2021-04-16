/*______________________________________________________________________________

   Date:       1993 - 2004
   Company:    Pacific Northwest National Laboratories
               Battelle Corporation
________________________________________________________________________________
__Modifiication  History________________________________________________________

  DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#ifndef ATOv2_H
#define ATOv2_H

#include "typedefs.h"
#include "Glyph.h"
#include "atoClass.h"
#include "PolyClass.h"

class ATOGrid_v2;

struct ATOState
{
  int conidx;
  int fluxidx;
  int timeidx;
  char prefix[SMALLSTRING];
  char name[SMALLSTRING];

  char casid[SMALLSTRING];
  char chemname[SMALLSTRING];

  char prodname[SMALLSTRING];
  char outprodname[SMALLSTRING];
  char moist[SMALLSTRING];
  char phase[SMALLSTRING];

  char releasetype[SMALLSTRING];
  char coordtype[SMALLSTRING];
  char spatialtype[SMALLSTRING];

  char axis1units[8];   //"m"
  char axis2units[8];   //"deg" | "m"
  ATOGrid_v2 *grid;

  int ngas;
  int nsol;
  int nliq;

  int yr;
  int mon;
  int dy;
  int hr;

  double dist;
  double radius;
  double density;
  char fluxtype[SMALLSTRING];
  char radunits[SMALLSTRING];
  char denunits[SMALLSTRING];

  double time;

  char tunits[SMALLSTRING];
  char vunits[SMALLSTRING];
};

//---------------------------------------------------------------------------------
class ATOGrid_v2: public ATOGrid
{
private:
public:

  int WriteFile(long pid, ocsv *fp, Glyph *g, ATOState *t, int fIdx);

  char *getDictionaryName(ATOState *t);
  char *getVariableName(ATOState *t);
  int WriteDataset(long pid, icsv *inf, Glyph *g, ATOState *t);
};

//---------------------------------------------------------------------------------
class ATOTimePeriod_v2: public ATOTimePeriod
{
private:
public:

  int WritePoints(long pid, ocsv *fp, Glyph *g, ATOState *t, int fIdx, char *ID);
  int forEachPhase(long pid, ocsv *fp, Glyph *g, ATOState *t, int i, char *ID);
  int forEachOutProd(long pid, ocsv *fp, Glyph *g, ATOState *t, char *ID);
  int WriteFile(long pid, ocsv *fp, Glyph *g, char *modId, ATOState *t, char *ID);
  int WriteDataset(long pid, icsv *inf, Glyph *g, ATOState *t);
};

//---------------------------------------------------------------------------------
class ATOCon_v2: public ATOCon
{
private:
public:

  int WriteDataset(long pid, icsv *inf, Glyph *g, ATOState *t, STR_VEC_PTR_VEC *conlist);
  int WriteFile(long pid, ocsv *fp, Glyph *g, char *modId, ATOState *t, STR_VEC *conlist, char *ID);
};

//---------------------------------------------------------------------------------
class ATOFluxType_v2: public ATOFluxType
{
private:
public:

  int WriteDataset(long pid, icsv *inf, Glyph *g, int &ngas, int &nliq, int &nsol);
  int WriteFile(long pid, ocsv *fp, Glyph *g, char *modId, int num, STR_VEC_PTR_VEC *conlist, char *ID);
};

//---------------------------------------------------------------------------------
class ATOSet_v2: public ATOSet
{
private:
public:

  int WriteDataset(long pid, icsv *inf, Glyph *g, STR_VEC_PTR_VEC *conlist);
  int WriteFile(long pid, ocsv *fp, Glyph *g, char *modId, STR_VEC_PTR_VEC *conlist, char *ID);
};

//---------------------------------------------------------------------------------
class ATO_v2: public ATO
{
private:
public:
  ocsv *outf;

  int WriteDataset(long pid, char *fuiname, Glyph *g, STR_VEC_PTR_VEC *conlist);
  int WriteFile(long pid, char *outpath, Glyph *g, STR_VEC_PTR_VEC *conlist, char *fqual, char *ID, char *ID2x);
};
//---------------------------------------------------------------------------------
#endif


