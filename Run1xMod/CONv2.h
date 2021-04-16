/*______________________________________________________________________________

   Date:       1993 - 2004
   Company:    Pacific Northwest National Laboratories
               Battelle Corporation
________________________________________________________________________________
__Modifiication  History________________________________________________________

  DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#ifndef CON_v2_H
#define CON_v2_H

#include "typedefs.h"
#include "Glyph.h"


class CON_v2
{
private:
  PM_PTR_MAP conVarMap;
  char uidic[SMALLSTRING];
public:

  STR_VEC_PTR_VEC conList;

  CON_v2(STR_SET dicList, char *_uidic);
  ~CON_v2();

  // loaded by run1xmod.
  void ConListClear();
  int GetConstituentList(STR_VEC_MAP setlist, char *ID);

  void ConVarMapClear();
  int GetMappingOfConstituentParameters(STR_SET dicList);

  // special handeling of constituent output
  int ReadConstituentDatasets(Glyph *g);
  int ReadConstituentProperties(Glyph *g, char *dicprefix, int *v2idx, int *v1idx, int depidx);

  int WriteConstituentDatasets(Glyph *g);
  void WriteConstituentProperties(Glyph *g, char *dicprefix, int *v1idx, int *v2idx, int depidx);
};

#endif