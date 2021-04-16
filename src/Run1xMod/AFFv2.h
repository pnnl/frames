/*______________________________________________________________________________

   Date:       1993 - 2004
   Company:    Pacific Northwest National Laboratories
               Battelle Corporation
________________________________________________________________________________
__Modifiication  History________________________________________________________

  DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#ifndef affV2_H
#define affV2_H

#include "typedefs.h"
#include "glyph.h"
#include "affclass.h"

class AFFCon_v2: public AFFCon
{
private:
public:

  int WriteDataset(long pid, icsv *inf, int nm, Glyph *g, int numFluxTypes, STR_VEC_PTR_VEC *conlist, STR_VEC *vset);
  int WriteFile(long pid, ocsv *fp, int nm, Glyph *g, STR_VEC *conlist, int ngas, int nsol, int nliq, char *ID);
};

class AFFSet_v2: public AFFSet
{
private:
public:

  int WriteDataset(long pid, icsv *inf, int nm, Glyph *g, STR_VEC_PTR_VEC *conlist);
  int WriteFile(long pid, ocsv *fp, int nm, Glyph *g, STR_VEC_PTR_VEC *conlist, char *ID);
};

class AFF_v2: public AFF
{
private:
public:
  ocsv *outf;

  int WriteDataset(long pid, char *fuiname, Glyph *g, STR_VEC_PTR_VEC *conlist);
  int WriteFile(long pid, char *outpath, Glyph *g, STR_VEC_PTR_VEC *conlist, char *fqual, char *ID, char *ID2x);
};

#endif
