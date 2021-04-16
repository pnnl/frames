#ifndef GLYPH_H
#define GLYPH_H

#include "typedefs.h"
#include "Param.h"
#include "gid.h"

#define PI 3.1415926535897932384626433832795

//unit constants for mappings
extern char *UNIT;
extern char *NA;
extern char *HOUR;
extern char *YEAR;
extern char *M;
extern char *KM;
extern char *UM;
extern char *G;
extern char *PCI;
extern char *MYR;
extern char *GYR;
extern char *GHR;
extern char *GML;
extern char *MGKG;
extern char *MGL;
extern char *GCM3;
extern char *PCIKG;
extern char *PCIL;
extern char *PCIML;
extern char *PCIYR;
extern char *PCIHR;
extern char *FRACTION;

// measure
extern char *cmWCF;
extern char *rmWCF;
extern char *cmSCF;
extern char *rmSCF;
extern char *cmFF;
extern char *rmFF;

//Dictionary Name constants for file mappings
extern char *_GID;
extern char *_MSG;
extern char *_WRN;
extern char *_ERR;
extern char *_AFF;
extern char *_WFF;
extern char *_WCF;
extern char *_SCF;
extern char *_ATO;
extern char *_EPF;
extern char *_RIF;
extern char *_HIF;
extern char *_BBF;
extern char *_EPA;
extern char *_HQF;
extern char *_EXF;
extern char *_TWI;
extern char *_FLX;
extern char *_AC2;
extern char *_RDF;
extern char *_DOT;

extern char *FEATURE;
extern char *FEATUREPTS;
extern char *ATTRIBUTEDES;
extern char *SETATTRIBUTEDES;
extern char *ATTRIBUTES;
extern char *SETATTRIBUTES;
extern char *MEDIA;
extern char *MEDIASET;
extern char *POPFEATURE;

//GIS Feature type constants
extern char *PTS;
extern char *POLYS;
extern char *VOLUMES;

//point styles
extern char *GRID;
extern char *POLAR;
extern char *CARTESIAN;

//specific GIS feature types
extern char *MEDIUM;
extern char *AIR;
extern char *SOIL;
extern char *VADOSE;
extern char *AQUIFER;
extern char *SURFACEWATER;
extern char *SURFACE_WATER;
extern char *SEDIMENT;
extern char *ACUTEEXP;
extern char *CHRONICEXP;
extern char *POPULATION;

//Time
extern char *TIME;
extern char *ACUTE;
extern char *CHRONIC;
extern char *DURATION;
extern char *DUR;

//Constituent
extern char *LIST;
extern char *CASID;
extern char *NAME;
extern char *CHEM;
extern char *RAD;

//Measurements
extern char *MODIFIER;
extern char *TIMEPTS;
extern char *FLUX;
extern char *CONC;
extern char *DEP;
extern char *DOSE;
extern char *INTAKE;
extern char *WATERFLUX;
extern char *TOTALFLUX;
extern char *ADSORBEDFLUX;
extern char *DISSOLVEDFLUX;
extern char *WATERCONC;
extern char *TOTALCONC;
extern char *ADSORBEDCONC;
extern char *DISSOLVEDCONC;
extern char *EXTERNALDOSE;

extern char *MIDSIZE;
extern char *DENSITY;
extern char *REACTIVE;

extern char *ALL;     // all states
extern char *GAS;     // reactive fractions
extern char *SOLID;   // particle range what is given are mid points
extern char *LIQUID;  // vapor

extern char *WET;     // Deposition types
extern char *DRY;
extern char *TOTAL;

extern char *GASEMISSION;  // reactive fractions
extern char *SOLEMISSION;  // particle range what is given are mid points
extern char *LIQEMISSION;  // vapor

extern char *PATHWAY;
extern char *AGEGROUP;
extern char* STARTAGE;
extern char* ENDAGE;

//DB UI Datasets(_GID) that contain Boundary Conditions
// con, aos, tos, ebf, cct, ssl, saf

extern char Media[SMALLSTRING];
extern char MediaSet[SMALLSTRING];
extern char setAgeGroup[SMALLSTRING];
extern char setPopulation[SMALLSTRING];
extern char varPopFeature[SMALLSTRING];
extern int  population;
extern int  popFeatureIdx;
extern double duration;

class Glyph
{
private:
  int numreads;
  int numwrites;
  string DesPath;
  string Class;
  string Group;
  STR_VEC Reads;
  STR_VEC Writes;

public:
  int des;
  string Prefix;
  string UIExe;
  string ModelExe;

  iGid gid;

  char uiset[SMALLSTRING];
  char uidic[SMALLSTRING];
  char ftype[SMALLSTRING];
  char fqual[SMALLSTRING];

  char *ID1x;
  char *ID2x;
  char *Label;
  char *Model;
  double x;
  double y;
  double z;
  int sx;
  int sy;
  int State;
  int siteidx;
  int glyphidx;

  STR_VEC_MAP_MAP imodlist;
  STR_VEC_MAP_MAP omodlist;
  STR_VEC_MAP isetlist;
  STR_VEC_MAP osetlist;

  GLYPH_PTR_MAP sinks;
  GLYPH_PTR_MAP sources;
  GLYPH_PTR_MAP vwrobjects;
  Glyph *vwrobject;
  Glyph *PDCFglyph;

  Glyph(char *simname, char *modid);
  ~Glyph();

  int ReadDes(const char *path);

  bool Source(char *modid);
  bool Sink(char *modid);
  bool FindWriteType(char *type, char *qual);
  bool HaveFileType(char *ftype, char *fqual, STR_VEC_MAP setlist);
  int GetInputModuleList();
  int GetOutputModuleList();
  int GetSourcesAndSinks(GLYPH_PTR_MAP glyphlist);
  int GetFileTypeAndQualifier(STR_VEC_MAP setlist, char *ctype, char *cqual);

  // writes the modules fui and csm info only
  int WriteFUI(oGid *gid, char *prefix, int siteIdx, int glyphIdx);
  int WriteCSM(oGid *gid, char *prefix, int siteIdx, int glyphIdx, char *modid);
  int WriteProperties(oGid *gid, char *prefix, int siteIdx, int glyphIdx);

  int ReadUIDataset();
  int WriteGIDSection(oGid *gid, char *section);

  int WriteDatasetsFromPDCF(char *outPath, STR_VEC_PTR_VEC conList);
  int WritePDCFFromDatasets(char *outPath, STR_VEC_PTR_VEC conList, Glyph *g);

  // should be called from write pdcf
  int WriteDatasetsFromGID(char *outpath);
  void WriteToFileType(char *outpath, char *ext, char *dname, char *vname);
  bool WriteFromFileType(char *outpath, char *ext, char *dname, char *vname, char *ID1x, char *ID2x);
};
#endif