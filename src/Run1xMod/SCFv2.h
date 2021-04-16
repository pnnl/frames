/*______________________________________________________________________________

   Date:       1993 - 2004
   Company:    Pacific Northwest National Laboratories
               Battelle Corporation
________________________________________________________________________________
__Modifiication  History________________________________________________________

  DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#ifndef scfV2_H
#define scfV2_H

#include "typedefs.h"
#include "glyph.h"
#include "scfClass.h"

class SCFCon_v2: public SCFCon
{
private:
public:

  int WriteDataset(long pid, icsv *inf, int nm, Glyph *g, STR_VEC_PTR_VEC *conlist, char *type);
  int WriteFile(long pid, ocsv *fp, int nm, Glyph *g, STR_VEC *conlist, char *ID);
};

class SCFSet_v2: public SCFSet
{
private:
public:

  int WriteDataset(long pid, icsv *inf, int nm, Glyph *g, STR_VEC_PTR_VEC *conlist);
  int WriteFile(long pid, ocsv *fp, int nm, Glyph *g, STR_VEC_PTR_VEC *conlist, char *fqual, char *ID);
};

class SCF_v2: public SCF
{
private:
public:
  ocsv *outf;

  int WriteDataset(long pid, char *fuiname, Glyph *g, STR_VEC_PTR_VEC *conlist);
  int WriteFile(long pid, char *outpath, Glyph *g, STR_VEC_PTR_VEC *conlist, char *fqual, char *ID, char *ID2x);
};

#endif