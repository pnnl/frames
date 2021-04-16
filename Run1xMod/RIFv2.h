//---------------------------------------------------------------------------
#ifndef rifClass2H
#define rifClass2H

#include "typedefs.h"
#include "glyph.h"

extern void SetMedia(long pid, int numpt, GLYPH_PTR_MAP_ITR ig, char *dicname, char *dstype, char *locname, char *medtype);

class RIFPathway_v2;
class RIFRoute_v2;

typedef std::vector<RIFPathway_v2 *>                 RIF_VEC_PATH;
typedef RIF_VEC_PATH::iterator                    RIF_VEC_PATH_ITR;
typedef std::map<std::string, RIFRoute_v2 *>         RIF_MAP_FILE;
typedef RIF_MAP_FILE::iterator                    RIF_MAP_FILE_ITR;

class RIFPathway_v2
{
public:
  bool hit;
  bool hq;
  int mIdx;
  int pIdx;
  bool carc;
  bool nonc;

  char media[SMALLSTRING];
  char path[SMALLSTRING];
  char xtype[SMALLSTRING];

  RIFPathway_v2(long pid, char *setname, char *Media, int nmi, int npi, char *Xtype);
  RIFPathway_v2(long pid, char *setname, char *Media, int nmi, char *Path, char *Xtype);
  void WriteDataset(long pid, icsv *inf, char *setname, char *varname, char *unit, int na, int nc, int nt, int numpt, char *Xtype);
};

class RIFRoute_v2
{
public:
  bool hit;
  char pwdic[SMALLSTRING];
  char pwset[SMALLSTRING];
  char pwvar[SMALLSTRING];
  char rte[SMALLSTRING];
  char unit[SMALLSTRING];

  STR_VEC vecMedia;
  STR_VEC vecMediaSet;
  RIF_VEC_PATH *PathwayVec;

  RIFRoute_v2(char *dicname, char *setname);
  void WriteDataset(long pid, icsv *inf, int na, int nc, int nt, int numpt, char *Path, char *Xtype);
};

class RIFCon_v2
{
public:
  int nprog;
  int ntimes;
  char casid[SMALLSTRING];

  RIFCon_v2() { nprog = 0; ntimes = 0; }
  int WriteDataset(long pid, icsv *inf, int nm, int na, int numpt, Glyph *g, STR_VEC_PTR_VEC *conlist);
  int WriteFile(long pid, ocsv *fp, int nm, int na, int numpt, Glyph *g, STR_VEC *conlist, char *ID);
};

class RIFSet_v2
{
public:
  char rifname[SMALLSTRING];    // exp5, air9, ...
  char dstype[SMALLSTRING];     // acute or chronic
  char locname[SMALLSTRING];    // wcf:scf
  char medtype[SMALLSTRING];    // Surface Water:Soil
  int numcon;

  int numpt;
  double x;
  double y;

  int numage;
  double agemin;
  double agemax;

  int WriteDataset(long pid, icsv *inf, int nm, Glyph *g, STR_VEC_PTR_VEC *conlist);
  int WriteFile(long pid, ocsv *outf, int nm, Glyph *g, STR_VEC_PTR_VEC *conlist, char *ID);
};

class RIF_v2
{
public:
  icsv *inf;
  ocsv *outf;

  int WriteDataset(long pid, char *fuiname, Glyph *g, STR_VEC_PTR_VEC *conlist);
  int WriteFile(long pid, char *outpath, Glyph *g, STR_VEC_PTR_VEC *conlist, char *ID, char *ID2x);
};

#endif