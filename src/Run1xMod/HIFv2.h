//---------------------------------------------------------------------------
#ifndef HIFClass2H
#define HIFClass2H

#include "typedefs.h"
#include "glyph.h"

extern void SetMedia(long pid, int numpt, GLYPH_PTR_MAP_ITR ig, char *dicname, char *dstype, char *locname, char *medtype);

class HIFPathway_v2;
class HIFRoute_v2;

typedef std::vector<HIFPathway_v2 *>              HIF_VEC_PATH;
typedef HIF_VEC_PATH::iterator                    HIF_VEC_PATH_ITR;
typedef std::map<std::string, HIFRoute_v2 *>      HIF_MAP_FILE;
typedef HIF_MAP_FILE::iterator                    HIF_MAP_FILE_ITR;

class HIFPathway_v2
{
public:
  bool hit;
  bool hq;
  int mIdx;
  int pIdx;

  char media[SMALLSTRING];
  char path[SMALLSTRING];
  char rte[SMALLSTRING];

  HIFPathway_v2(long pid, char *setname, char *Media, char *_rte, int nmi, int npi);
  HIFPathway_v2(long pid, char *setname, char *Media, int nmi, char *Path, char *_rte);
  void WriteDataset(long pid, icsv *inf, char *setname, char *_unit, int na, int nc, int nt, int numpt, char *Xtype);
};

class HIFRoute_v2
{
public:
  bool hit;
  char pwdic[SMALLSTRING];
  char pwset[SMALLSTRING];
  char xtype[SMALLSTRING];
  char unit[SMALLSTRING];

  int numOrgans;

  STR_VEC vecMedia;
  STR_VEC vecMediaSet;
  HIF_VEC_PATH *PathwayVec;

  HIFRoute_v2(char *dicname, char *setname);
  void WriteDataset(long pid, icsv *inf, int na, int nc, int nt, int numpt, char *rte, char *Path, char *Xtype);
  void PutTime(long pid, int nm, int na, int nc, int nt, float sttime, float expdur);
};

class HIFCon_v2
{
public:
  int nprog;
  int ntimes;
  char casid[SMALLSTRING];

  HIFCon_v2() { nprog = 0; ntimes = 0; }
  int WriteFile(long pid, ocsv *fp, int nm, int na, int numpt, Glyph *g, STR_VEC *conlist, char *ID);
  int WriteDataset(long pid, icsv *inf, int nm, int na, int numpt, Glyph *g, STR_VEC_PTR_VEC *conlist);
};

class HIFSet_v2
{
  public:
  char hifname[SMALLSTRING];
  char dstype[SMALLSTRING];
  char locname[SMALLSTRING];
  char medtype[SMALLSTRING];
  int numcon;

  int numpt;
  double x;
  double y;

  int numage;
  double agemin;
  double agemax;

  int WriteFile(long pid, ocsv *outf, int ds, Glyph *g, STR_VEC_PTR_VEC *conlist, char *ID);
  int WriteDataset(long pid, icsv *inf, int ds, Glyph *g, STR_VEC_PTR_VEC *conlist);
};

class HIF_v2
{
  public:
  icsv *inf;
  ocsv *outf;

  int WriteDataset(long pid, char *fuiname, Glyph *g, STR_VEC_PTR_VEC *conlist);
  int WriteFile(long pid, char *outpath, Glyph *g, STR_VEC_PTR_VEC *conlist, char *ID, char *ID2x);
};

#endif