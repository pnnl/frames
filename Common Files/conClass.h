//---------------------------------------------------------------------------
#ifndef conClassH
#define conClassH

#include "gid.h"
#include "series.h"

class CON {
  public:

  char name[64];
  char cas[32];
  int type;

  CON *parent;

  int numProgeny;
  CON **prog;

  Series *TS;

  CON();
  CON(CON *pcon);
  ~CON();
  void Init(int siteidx, int ncon, int nprog, element *fscname, element *fscasid, element *nds, element *ktype);
  void AddSeries(char *units, char *timeunits, Series *addTS);
  void Write(fcsv *fout);
  void Read(fcsv *fin);
};


//---------------------------------------------------------------------------
#endif
