/*______________________________________________________________________________

   Date:       1993 - 2004
   Company:    Pacific Northwest National Laboratories
               Battelle Corporation
________________________________________________________________________________
__Modifiication  History________________________________________________________

  DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#ifndef OS_v2_H
#define OS_v2_H

#include "typedefs.h"
#include "Glyph.h"


class OS_v2
{
private:
  PM_PTR_MAP VarMap;
  char mapname[SMALLSTRING];
  char prefix[SMALLSTRING];
  char uidic[SMALLSTRING];
public:
 
  STR_VEC_PTR_VEC keyList;

  OS_v2(STR_SET setList, char *_mapname, char *_prefix, char *_uidic);
  ~OS_v2();

  // loaded by run1xmod.
  void KeyListClear();
  int GetKeyList(STR_VEC_MAP setlist, char *ID);

  void VarMapClear();
  int GetMappingOfSectionParameters(STR_SET dicList);

  // special handeling of constituent output
  int ReadDatasets(Glyph *g);
  int ReadProperties(Glyph *g, char *dicprefix, int *v2idx, int *v1idx, int depidx);

  int WriteDatasets(Glyph *g);
  void WriteProperties(Glyph *g, char *dicprefix, int *v1idx, int *v2idx, int depidx);
};

#endif