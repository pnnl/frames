//---------------------------------------------------------------------------
#ifndef atoSSrcH
#define atoSSrcH

#include "Series.h"
#include "gid.h"

class atoSSrc
{
public:
  GIDFILE *G;
  int siteidx;
  Series *TS;
  float length;
  float width;
  float area;
  element *modid;
  int srcidx;
  int atoidx;
  float srcLocX;
  float srcLocY;
  float atoLocX;
  float atoLocY;
  double dist;
  float loc[3][4];
  void (*AddSeries) (char*,char *,Series*);

  ~atoSSrc();
  atoSSrc(GIDFILE *g, int idx, void (*pt2func)(char*, char*, Series*));
  void getSeries(char *fname, char *srcid, char *atoid);
  void getThreeClosestPoints(int ds, int ncon, int nprog, int ntime, int nout);
  void getThreeClosestGridPoints(char *coord, int ds, int ncon, int nprog, int ntime, int nout);
};

//---------------------------------------------------------------------------
#endif
