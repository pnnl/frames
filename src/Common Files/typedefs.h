#ifndef TYPEDEFS_H
#define TYPEDEFS_H

#define MICROSOFT

#pragma warning(disable:4786)
#pragma warning(disable:4503)

#include <io.h>
#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <set>
#include <map>
#include <vector>
#include <algorithm>

// For compatibilty to FRAMES v2
union dtVariant
{
  double Float;
  int    Integer;
  int    Logical;
  char   *String;
};

#define SUCCESS               0
#define FAILURE              -1

#define VARIABLERANGE         -1004

#define dsFLOAT                   0
#define dsINTEGER                 1
#define dsLOGICAL                 2
#define dsSTRING                  3
#define dsINVALID                 4

#define GID_MAX_DIM 6
#define MMF_MAX_DIM 8
extern int _Idx[8];
int *SetIdx(int _1=0, int _2=0, int _3=0, int _4=0, int _5=0, int _6=0, int _7=0, int _8=0);

#include "Test.h"
#include "robust.h"
#include "ErrorC.h"
#include "DatasetC.h"
#include "SystemDevC.h"
#include "ConversionC.h"
#include "F2ModuleDevC.h"
#include "F2EnumerateC.h"
#include "F2ModuleDevC.h"
#include "F2SystemDevC.h"
#include "StringParser.h"
#include "SeriesV2.h"

class FileType;
class Glyph;
class ParamValue;
class Param;
class ParamMap;
class Series_v2;
 
using namespace std;

typedef std::map<std::string, std::string>      STR_MAP;
typedef STR_MAP::iterator                       STR_MAP_ITR;

typedef std::set<std::string>                   STR_SET;
typedef STR_SET::iterator                       STR_SET_ITR;

typedef std::vector<std::string>                STR_VEC;
typedef STR_VEC::iterator                       STR_VEC_ITR;

typedef std::map<std::string, STR_VEC>          STR_VEC_MAP;
typedef STR_VEC_MAP::iterator                   STR_VEC_MAP_ITR;

typedef std::map<std::string, STR_VEC_MAP>      STR_VEC_MAP_MAP;
typedef STR_VEC_MAP_MAP::iterator               STR_VEC_MAP_MAP_ITR;

typedef std::vector<STR_VEC *>                  STR_VEC_PTR_VEC;
typedef STR_VEC_PTR_VEC::iterator               STR_VEC_PTR_VEC_ITR;

typedef std::map<std::string, STR_VEC *>        STR_VEC_PTR_MAP;
typedef STR_VEC_PTR_MAP::iterator               STR_VEC_PTR_MAP_ITR;

typedef std::vector<ParamValue *>               VALUE_VEC;
typedef VALUE_VEC::iterator                     VALUE_VEC_ITR;

typedef std::vector<Param *>                    PARAM_PTR_VEC;
typedef PARAM_PTR_VEC::iterator                 PARAM_PTR_VEC_ITR;

typedef std::vector<ParamMap *>                 PM_PTR_VEC;
typedef PM_PTR_VEC::iterator                    PM_PTR_VEC_ITR;

typedef std::map<std::string, PM_PTR_VEC>       PM_PTR_VEC_MAP;
typedef PM_PTR_VEC_MAP::iterator                PM_PTR_VEC_MAP_ITR;

typedef std::map<std::string, ParamMap *>       PM_PTR_MAP;
typedef PM_PTR_MAP::iterator                    PM_PTR_MAP_ITR;

typedef std::map<std::string, Glyph *>          GLYPH_PTR_MAP;
typedef GLYPH_PTR_MAP::iterator                 GLYPH_PTR_MAP_ITR;

typedef std::map<std::string, FileType *>       FILETYPE_PTR_MAP;
typedef FILETYPE_PTR_MAP::iterator              FILETYPE_PTR_MAP_ITR;

// 1.x to 2x file type mapping structure
class FileType
{
private:
public:
  string ftype;
  string fqual;
  STR_VEC    dicreq;
  STR_VEC    dicopt;
  FileType(char *type, char *qual, int ix1, int ix2);
};

extern char *FILETYPE;
extern char *DICLIST;

// defined in run1xmod
extern void Log(char *s);
extern void CheckErrors();
extern char *CheckUnit(long pid, char *setname, char *var, char *vunit);
extern int GetSetName(char *dicname, char *setname, char *ID, STR_VEC_MAP setlist);
extern int newGetSetName(char *modId, char *dicname, STR_VEC_MAP setlist, char *setname);
extern void FindBenchmarks(char *type, char *cas, Series_v2 *conc);

extern bool doBench;
extern long pid;
extern char setname[SMALLSTRING];
extern char iniset[SMALLSTRING];  // maintian.import
extern FILETYPE_PTR_MAP mapFileType;

#endif