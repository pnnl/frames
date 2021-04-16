/*______________________________________________________________________________

   Date:       1993 - 2004
   Company:    Pacific Northwest National Laboratories
               Battelle Corporation
________________________________________________________________________________
__Modifiication  History________________________________________________________

  DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#ifndef TB_v2_H
#define TB_v2_H

#include "typedefs.h"
#include "Glyph.h"


class TB_v2
{
private:
  PM_PTR_MAP VarMap;
  char mapname[SMALLSTRING];
  char prefix[SMALLSTRING];
  char uidic[SMALLSTRING];
public:
 
  STR_VEC_PTR_VEC cKeyList; //constituent
  STR_VEC_PTR_VEC oKeyList; //organism
  STR_VEC_PTR_VEC jKeyList; //jurisdiction 

  TB_v2(STR_SET setList, char *_mapname, char *_prefix, char *_uidic);
  ~TB_v2();

  // loaded by run1xmod.
  void KeyListClear(STR_VEC_PTR_VEC keyList);
  int GetKeyList(STR_VEC_MAP setlist, char *pfix, char *mname, char *vname, char *name);

  void VarMapClear();
  int GetMappingOfSectionParameters(STR_SET dicList);

  // special handeling of constituent output
  int ReadDatasets(Glyph *g);
  int ReadProperties(Glyph *g, char *dicprefix, int *v2idx, int *v1idx, int depidx);

  int WriteDatasets(Glyph *g);
  void WriteProperties(Glyph *g, char *dicprefix, int *v1idx, int *v2idx, int depidx);
};

#endif