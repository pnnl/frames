#ifndef PARAM_H
#define PARAM_H

#include "typedefs.h"

extern int TypeStr2Int(char *val);
extern int TypeString2Int(string val);
extern char *DataTypeToStr(int val);
extern void WriteToDataset(char *setname, char *vname, char *vunit, int *idx, int itype, char *val);

class iGid;
class oGid;

class ParamValue {
private:
public:
  int rec;
  int ref;
  int idx[MMF_MAX_DIM];
  char key[16];
  char fmt[SMALLSTRING];
  char uunt[SMALLSTRING];
  char vunt[SMALLSTRING];
  char sval[LARGESTRING];

  double dval;
  int ival;
  int lval;
  ParamValue();
  ParamValue(int dtype, int *_idx, dtVariant *var);
};

class Param {
private:
public:

  // dictionary properties
  int    itype;
  string vname;
  string vunit;
  string vtype;
  double vmin;
  double vmax;

  // gid properties
  string iunit;
  string uunit;
  string logfmt;

  VALUE_VEC values;

  Param(char *_pname = NULL, char *_vtype = NULL);
  ~Param();

  int ReadValues(char *_setname, char *_v2var, int *v2idx, int *v1idx); // from gid
  int PutValues(oGid *gid, int recnum);                                 // to gid

  int ReadValues(char *_setname, int idx); // from dataset
  int PutValues(char *_setname, int idx);  // to dataset

  int AddEmptyValue();
  ParamValue *Value(int idx);
  bool Param::ValidRange(ParamValue *);
};

class ParamMap {
private:
public:
  char *prefix[2];

  char *v1var;
  char *v1dic;
  char *v1unt;
  char *v1typ;

  char *v2dic[2];
  char *v2var;
  char *v2unt[2];
  char *v2typ[2];

  int ndc;            // number of indices for v2 parameter
  char *v2depdic[6];
  char *v2depvar[6];  // may have up to 6 dependend variables
  char *v2depval[6];
  int  v2depidx[2][6];   // first index indicate chem or rad

  ParamMap();
  ~ParamMap();
  void Initialize(char *_v1dic, char *_v1var, char *_v2var, char *_v2dic, char *_prefix0, char *_prefix1);
  int  GetDependentValueIndex(char *dicprefix, STR_VEC_MAP osetlist, bool write, int idx);
};

class iGid
{
private:
public:
  PARAM_PTR_VEC params;

  void ParamsClear();
  int ReadGID(char *outpath, char *uidic);
  Param *ParamsAdd(char *_dic, char *_name, char *v2dic = NULL, char *v2var = NULL);
  int AddRecToParams(char *_dic, paramrec *curr, int recnum);
  Param *ParamsFind(char *_name);
  ParamValue *ParamsFind(char *_name, int *_idx);

  bool GetParamInteger(char *_name, int *_idx, int *_val);
  bool GetParamLogical(char *_name, int *_idx, int *_val);
  bool GetParamString(char *_name, int *_idx, char *_val);
  bool GetParamFloat(char *_name, int *_idx, double *_val);
  bool GetParamValue(char *_name, int *_idx, int dtype, dtVariant *_val);
  void SetParamValue(char *_name, int *_idx, dtVariant *_val, char *_dic, int recnum = 0);

};

class oGid
{
private:
public:
  ocsv *fp;
  long nrec;

  oGid(char *filename);
  ~oGid();

  void PutValue(const char *pname, int *idx, int ref, const char *uunit, const char *iunit, int val);
  void PutValue(const char *pname, int *idx, int ref, const char *uunit, const char *iunit, float val);
  void PutValue(const char *pname, int *idx, int ref, const char *uunit, const char *iunit, double val);
  void PutValue(const char *pname, int *idx, int ref, const char *uunit, const char *iunit, const char *val);
  void PutValue(const char *pname, int *idx, int ref, const char *uunit, const char *iunit, string val);
};


#endif