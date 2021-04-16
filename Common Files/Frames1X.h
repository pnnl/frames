#ifndef FRAMES1X_H
#define FRAMES1X_H


#include "typedefs.h"

extern void WriteToDataset(long pid, char *setname, char *vname, char *vunit, int *idx, int itype, char *val);
extern void ReadFromDataset(long pid, char *setname, char *vname, char *vunit, int *idx, char *vtype, char *val);
extern int TypeString2Int(string val);
extern int TypeStr2Int(char *val);
extern char *cstr(const char *s);
extern char *CheckUnit(long pid, char *setname, char *var, char *vunit);

#include "Param.h"

extern char iniset[MAXFIELD];
extern char inidic[MAXFIELD];

extern DIC_LIST conDicList;
extern MAP_LIST conVarList;
extern CON_LIST conList;

class Glyph;

extern char *NA;
extern char *blank;
extern char *chemprefix;
extern char *radprefix;
extern char *dkprefix;

#endif
