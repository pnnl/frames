/*______________________________________________________________________________

   Date:       1993 - 2004
   Company:    Pacific Northwest National Laboratories
               Battelle Corporation
________________________________________________________________________________
__Modifiication  History________________________________________________________

  DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#ifndef wcfV2_H
#define wcfV2_H

#include "typedefs.h"
#include "glyph.h"
#include "wcfClass.h"

class WCFCon_v2: public WCFCon
{
private:
public:

  int WriteDataset(long pid, icsv *inf, int nm, Glyph *g, STR_VEC_PTR_VEC *conlist, char *type);
  int WriteFile(long pid, ocsv *fp, int nm, Glyph *g, STR_VEC *conlist, char *ID);
};

class WCFSet_v2: public WCFSet
{
private:
public:

  int WriteDataset(long pid, icsv *inf, int nm, Glyph *g, STR_VEC_PTR_VEC *conlist);
  int WriteFile(long pid, ocsv *fp, int nm, Glyph *g, STR_VEC_PTR_VEC *conlist, char *fqual, char *ID);
};

class WCF_v2: public WCF
{
private:
public:
  ocsv *outf;

  int WriteDataset(long pid, char *fuiname, Glyph *g, STR_VEC_PTR_VEC *conlist);
  int WriteFile(long pid, char *outpath, Glyph *g, STR_VEC_PTR_VEC *conlist, char *fqual, char *ID, char *ID2x);
};

#endif