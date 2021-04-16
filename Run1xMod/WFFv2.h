/*______________________________________________________________________________

   Date:       1993 - 2004
   Company:    Pacific Northwest National Laboratories
               Battelle Corporation
________________________________________________________________________________
__Modifiication  History________________________________________________________

  DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#ifndef wffV2_H
#define wffV2_H

#include "typedefs.h"
#include "glyph.h"
#include "wffClass.h"

class WFFCon_v2: public WFFCon
{
private:
public:
  Series_v2 *flux1;
  Series_v2 *flux2;

  int WriteDataset(long pid, icsv *inf, int nm, Glyph *g, STR_VEC_PTR_VEC *conlist);
  int WriteFile(long pid, ocsv *fp, int nm, Glyph *g, STR_VEC *conlist, char *ID);
};

class WFFSet_v2: public WFFSet
{
private:
public:

  int WriteDataset(long pid, icsv *inf, int nm, Glyph *g, STR_VEC_PTR_VEC *conlist);
  int WriteFile(long pid, ocsv *fp, int nm, Glyph *g, STR_VEC_PTR_VEC *conlist, char *ID);
};

class WFF_v2: public WFF
{
private:
public:
  ocsv *outf;

  int WriteDataset(long pid, char *fuiname, Glyph *g, STR_VEC_PTR_VEC *conlist);
  int WriteFile(long pid, char *outpath, Glyph *g, STR_VEC_PTR_VEC *conlist, char *fqual, char *ID, char *ID2x);
};

#endif
