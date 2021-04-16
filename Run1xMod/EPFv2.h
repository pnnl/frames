//---------------------------------------------------------------------------
#ifndef epfClass2H
#define epfClass2H

#include "typedefs.h"
#include "glyph.h"

class EPFRoute_v2;
class EPFPathway_v2;

typedef std::vector<EPFPathway_v2 *>                 EPF_VEC_PATH;
typedef EPF_VEC_PATH::iterator                       EPF_VEC_PATH_ITR;
typedef std::map<std::string, EPFRoute_v2 *>         EPF_MAP_FILE;
typedef EPF_MAP_FILE::iterator                       EPF_MAP_FILE_ITR;

class EPFPathway_v2
{
public:
  bool hit;
  int mIdx;
  int pIdx;

  char media[SMALLSTRING];
  char path[SMALLSTRING];

  EPFPathway_v2(long pid, char *setname, char *Media, int nmi, int npi);
  EPFPathway_v2(long pid, char *setname, char *Media, int nmi, char *Path);
  void WriteDataset(long pid, icsv *inf, char *setname, char *varname, char *unit, int nc, int nt, int numpt);
};

class EPFRoute_v2
{
public:
  bool hit;
  char pwdic[SMALLSTRING];
  char pwset[SMALLSTRING];
  char pwvar[SMALLSTRING];
  char rte[SMALLSTRING];        //dermal, ingestion, inhalation, external
  char unit[SMALLSTRING];

  STR_VEC vecMedia;
  STR_VEC vecMediaSet;
  EPF_VEC_PATH *PathwayVec;

  EPFRoute_v2(char *dicname, char *setname);
  void WriteDataset(long pid, icsv *inf, int nc, int nt, int numpt, char *Path);
};

class EPFCon_v2
{
public:
  int nprog;
  int ntimes;
  char casid[SMALLSTRING];

  EPFCon_v2() { nprog = 0; ntimes = 0; }
  int WriteDataset(long pid, icsv *inf, int nm, int numpt, Glyph *g, STR_VEC_PTR_VEC *conlist);
  int WriteFile(long pid, ocsv *fp, int nm, int numpt, Glyph *g, STR_VEC *conlist, char *ID);
};

class EPFSet_v2
{
public:
  char epfname[SMALLSTRING];    // exp5, air9, ...
  char dstype[SMALLSTRING];     // acute or chronic
  char locname[SMALLSTRING];    // wcf:scf
  char medtype[SMALLSTRING];    // Surface Water:Soil
  int numcon;

  int numpt;
  double x;
  double y;

  int WriteDataset(long pid, icsv *inf, int nm, Glyph *g, STR_VEC_PTR_VEC *conlist);
  int WriteFile(long pid, ocsv *fp, int nm, Glyph *g, STR_VEC_PTR_VEC *conlist, char *ID);
};

class EPF_v2
{
public:
  icsv *inf;
  ocsv *outf;

  int WriteDataset(long pid, char *fuiname, Glyph *g, STR_VEC_PTR_VEC *conlist);
  int WriteFile(long pid, char *outpath, Glyph *g, STR_VEC_PTR_VEC *conlist, char *ID, char *ID2x);
};

#endif