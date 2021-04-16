#include "Param.h"
#include "F2ErrorC.h"

typedef int ItemRange(dtVariant *A, double min, double max);

int RangeCheckFloat(dtVariant *val, double min, double max)
{
  if(min == max) return SUCCESS;
  if(!(((val->Float > min) ||(fabs(min - val->Float) <= (fabs(val->Float)*eps)))
    &&((val->Float < max) ||(fabs(max - val->Float) <= (fabs(val->Float)*eps)))))
    return VARIABLERANGE;
  return SUCCESS;
}

int RangeCheckInteger(dtVariant *val, double min, double max)
{
  if(min == max) return 0;
  if(val->Integer < min || val->Integer > max)
    return VARIABLERANGE;
  return SUCCESS;
}

int RangeCheckLogical(dtVariant *val, double min, double max)
{
  if(val->Logical > 1 || val->Logical < 0)
    return VARIABLERANGE;
  return SUCCESS;
}

int RangeCheckString(dtVariant *val, double min, double max)
{
  if(min == max) return 0;
  int len = strlen(val->String);
  if(len < min || max < len)
    return VARIABLERANGE;
  return SUCCESS;
}

ItemRange *RangeCheck[4] =
{
  RangeCheckFloat,
    RangeCheckInteger,
    RangeCheckLogical,
    RangeCheckString
};

char *strType[5] =
{
  "FLOAT",
    "INTEGER",
    "LOGICAL",
    "STRING",
    "INVALID"
};

char *strLogical[2] =
{
  "FALSE",
    "TRUE"
};

int TypeStr2Int(char *val)
{
  if(strcmpi(val, strType[0]) == 0)    return 0;
  if(strcmpi(val, strType[1]) == 0)    return 1;
  if(strcmpi(val, strType[2]) == 0)    return 2;
  if(strcmpi(val, strType[3]) == 0)    return 3;
  return 4;
}

char *cstr(const char *s)
{ return(char *)s; }

int TypeString2Int(string val)
{  return TypeStr2Int(cstr(val.c_str())); }

char *DataTypeToStr(int val)
{
  if(val > 3 || val < 0) return strType[4];
  return strType[val];
}

void WriteToDataset(char *setname, char *vname, char *vunit, int *idx, int itype, char *val)
{
  if(dsFLOAT == itype)
    WriteReal6(pid, setname, vname, vunit, idx[0], idx[1], idx[2], idx[3], idx[4], idx[5], atof(val));
  else if(dsINTEGER == itype)
    WriteInt6(pid, setname, vname, vunit, idx[0], idx[1], idx[2], idx[3], idx[4], idx[5], atoi(val));
  else if(dsSTRING == itype)
    WriteString6(pid, setname, vname, vunit, idx[0], idx[1], idx[2], idx[3], idx[4], idx[5], val);
  else if(dsLOGICAL == itype)
    WriteLog6(pid, setname, vname, vunit, idx[0], idx[1], idx[2], idx[3], idx[4], idx[5], atoi(val));
}

ParamValue::ParamValue()
{
  for(int i = 0; i<MMF_MAX_DIM; i++) idx[i] = 0;
  dval = 0.0;
  ival = 0;
  lval = 0;
  ref = 0;
  rec = 0;
  rstrcpy(key, blank);
  rstrcpy(fmt, blank);
  rstrcpy(uunt, blank);
  rstrcpy(vunt, blank);
  rstrcpy(sval, blank);
}

ParamValue::ParamValue(int dtype, int *_idx, dtVariant *var)
{
  for(int i = 0; i<MMF_MAX_DIM; i++) idx[i] = *(_idx+i);
  if(dsSTRING == dtype)
    rstrcpy(sval, var->String);
  else
  {
    rstrcpy(sval, "#");
    if(dsFLOAT == dtype)
      dval = var->Float;
    else if(dsINTEGER == dtype)
      ival = var->Integer;
    else if(dsLOGICAL == dtype)
      lval = var->Logical;
  }
  ref = 0;
  sprintf(key, "%d, %d, %d, %d, %d, %d", idx[0], idx[1], idx[2], idx[3], idx[4], idx[5]);
  rstrcpy(fmt, blank);
  rstrcpy(uunt, blank);
  rstrcpy(vunt, blank);
}

Param::Param(char *_pname, char *_vtype)
{
  vname = blank;
  iunit = blank;
  uunit = blank;
  vunit = blank;
  vtype = blank;
  vmin = 0.0;
  vmax = 0.0;
  itype = TypeString2Int("STRING");

  if(_pname == NULL) return;
  vname = string(_pname);

  if(_vtype == NULL) return;
  vtype = string(_vtype);
  itype = TypeString2Int(_vtype);
}

Param::~Param()
{
  for(VALUE_VEC_ITR it = values.begin(); it!= values.end(); ++it)
  {
    ParamValue *pv = *it;
    TrackDelete(pv);
  }
  values.clear();
}

int Param::ReadValues(char *_setname, char *_v2var, int *v2idx, int *v1idx)
{
  int i;
  int dim;
  int ErrorCode;
  char _vunit[SMALLSTRING];

  dim = 0;
  rstrcpy(_vunit, vunit.c_str());
  ErrorCode = GetVarDimSize(pid, _setname, _v2var, v2idx, &dim);

  if(ErrorCode == 0 && dim>0)
  {
    for(VALUE_VEC_ITR it = values.begin(); it!= values.end(); ++it)
    {
      ParamValue *pv = *it;
      int ct = 0;
      for(int j = 0; j<GID_MAX_DIM; j++)
      {  if(pv->idx[j] == v1idx[j]) ct++; }
      if(ct == GID_MAX_DIM)
        return 0;
    }
    ParamValue *p = new ParamValue; TrackLocation(p);
    dtVariant var;

    int dtype = TypeString2Int(vtype);

    for(i = 0; i<MMF_MAX_DIM; i++)
      p->idx[i] = *(v2idx+i);

    if(dsSTRING == dtype)
    {
      ReadString6(pid, _setname, _v2var, _vunit, p->idx[0], p->idx[1], p->idx[2], p->idx[3], p->idx[4], p->idx[5], p->sval);
      ErrorCode = F2IOOk(pid);
      if(ErrorCode == SUCCESS)
        var.String = p->sval;
    }
    else
    {
      rstrcpy(p->sval, "#");
      if(dsFLOAT == dtype)
      {
        p->dval = ReadReal6(pid, _setname, _v2var, _vunit, p->idx[0], p->idx[1], p->idx[2], p->idx[3], p->idx[4], p->idx[5]);
        ErrorCode = F2IOOk(pid);
        if(ErrorCode == SUCCESS)
          var.Float = p->dval;
      }
      else if(dsINTEGER == dtype)
      {
        p->ival = ReadInt6(pid, _setname, _v2var, _vunit, p->idx[0], p->idx[1], p->idx[2], p->idx[3], p->idx[4], p->idx[5]);
        ErrorCode = F2IOOk(pid);
        if(ErrorCode == SUCCESS)
          var.Integer = p->ival;
      }
      else if(dsLOGICAL == dtype)
      {
        p->lval = ReadLog6(pid, _setname, _v2var, _vunit, p->idx[0], p->idx[1], p->idx[2], p->idx[3], p->idx[4], p->idx[5]);
        ErrorCode = F2IOOk(pid);
        if(ErrorCode == SUCCESS)
          var.Logical = p->lval;
      }
    }
    if(ErrorCode == SUCCESS)
    {
      if(RangeCheck[dtype](&var, vmin, vmax) == SUCCESS)
      {
        for(i = 0; i<MMF_MAX_DIM; i++) p->idx[i] = *(v1idx+i);
        if(0 == strlen(p->uunt))
          rstrcpy(p->uunt, this->vunit.c_str());
        if(0 == strlen(p->vunt))
          rstrcpy(p->vunt, this->vunit.c_str());
        values.push_back(p);
      }
      else
        TrackDelete(p);
    }
    else
    {
      TrackDelete(p);
    }
    return ErrorCode;
  }
  return ErrorCode;
}

int Param::AddEmptyValue()
{
  ParamValue *p = new ParamValue; TrackLocation(p);
  values.push_back(p);
  return SUCCESS;
}

ParamValue *Param::Value(int idx)
{
  int i = 0;
  for(VALUE_VEC_ITR it = values.begin(); it!= values.end(); ++it)
    if(idx == ++i)
      return *it;
  return NULL;
}

// Writes parameter values to the given Gid.
// Returns the number of value output in _GID,
int Param::PutValues(oGid *gid, int recnum)
{
  int cnt;
  bool ok;
  int idx[MMF_MAX_DIM];
  char dummy[SMALLSTRING];
  char _pname[SMALLSTRING];

  int itype = TypeString2Int(vtype);
  rstrcpy(_pname, vname.c_str());
  cnt = 0;
  for(VALUE_VEC_ITR it = values.begin(); it!= values.end(); ++it)
  {
    ParamValue *pv = *it;

    // the following line allows for the parameters to be written out in the order they were read in
    // as long as the recnum matches if rec=0 then we write the record anyway.
    if (pv->rec != recnum && pv->rec != 0) continue;

    if(0 == strlen(pv->uunt))
      rstrcpy(pv->uunt, this->vunit.c_str());
    if(0 == strlen(pv->vunt))
      rstrcpy(pv->vunt, this->vunit.c_str());

    sscanf(pv->key, "%d, %d, %d, %d, %d, %d", &pv->idx[0], &pv->idx[1], &pv->idx[2], &pv->idx[3], &pv->idx[4], &pv->idx[5]);
    ok = true;
    if(itype == dsSTRING)
    {
      gid->PutValue(_pname, pv->idx, pv->ref, pv->uunt, pv->vunt, pv->sval);
    }
    else if(itype == dsFLOAT)
    {
      gid->PutValue(_pname, pv->idx, pv->ref, pv->uunt, pv->vunt, pv->dval);
    }
    else if(itype == dsINTEGER)
    {
      gid->PutValue(_pname, pv->idx, pv->ref, pv->uunt, pv->vunt, pv->ival);
    }
    else if(itype == dsLOGICAL)
    {
      ok = (pv->lval == 0 || pv->lval == 1);
      if(!ok) return cnt;
      if(0 == strcmp(logfmt.c_str(), "0") || 0 == strcmp(logfmt.c_str(), "1"))
        gid->PutValue(_pname, pv->idx, pv->ref, pv->uunt, pv->vunt, pv->lval);
      else
      {
        rstrcpy(dummy, "false"); // default logical value
        if(0 == strcmpi(logfmt.c_str(), "True") || 0 == strcmpi(logfmt.c_str(), "False"))
        { if(pv->lval == 1) rstrcpy(dummy, "True"); else rstrcpy(dummy, "False"); }
        else if(0 == strcmpi(logfmt.c_str(), "T") || 0 == strcmpi(logfmt.c_str(), "F"))
        { if(pv->lval == 1) rstrcpy(dummy, "T"); else rstrcpy(dummy, "F"); }
        else if(0 == strcmpi(logfmt.c_str(), "Yes") || 0 == strcmpi(logfmt.c_str(), "No"))
        { if(pv->lval == 1) rstrcpy(dummy, "Yes"); else rstrcpy(dummy, "No"); }
        gid->PutValue(_pname, idx, pv->ref, pv->uunt, pv->vunt, dummy);
      }
    }
    else
      Log("Param::PutValues Unknown data type error!");
    if(!ok) return cnt;
    cnt++;
  }
  return cnt;
}

// Reads parameter values from the given setname and idx.
// The set must contain the parameter name and
// the 'zz' vars for the conversion.
// Returns 0 on success,
int Param::ReadValues(char *_setname, int idx)
{
  int i;
  int dim;
  int ErrorCode;
  char _vname[SMALLSTRING];
  char _vunit[SMALLSTRING];

  int dtype = TypeString2Int(vtype);

  dim = 0;
  rstrcpy(_vname, vname.c_str());
  rstrcpy(_vunit, vunit.c_str());
  ErrorCode = GetVarDimSize(pid, _setname, _vname, SetIdx(), &dim);
  for(i = 1; i<= dim; i++)
  {
    ParamValue *p = new ParamValue; TrackLocation(p);
    GetString(pid, _setname, "zzIDX", blank, SetIdx(idx, i), p->key);
    GetString(pid, _setname, "zzUUNT", blank, SetIdx(idx, i), p->uunt);
    GetString(pid, _setname, "zzUNT", blank, SetIdx(idx, i), p->vunt);
    GetInteger(pid, _setname, "zzREF", blank, SetIdx(idx, i), &p->ref);
    GetInteger(pid, _setname, "zzREC", blank, SetIdx(idx, i), &p->rec);
    // GetInteger(pid, _setname, "zzFMT", blank, SetIdx(idx, i), p->fmt);

    dtVariant var;
    if(dsSTRING == dtype)
    {
      ReadString1(pid, _setname, _vname, _vunit, i, p->sval);
      var.String = p->sval;
    }
    else
    {
      rstrcpy(p->sval, "#");
      if(dsFLOAT == dtype)
      {
        p->dval = ReadReal1(pid, _setname, _vname, _vunit, i);
        var.Float = p->dval;
      }
      else if(dsINTEGER == dtype)
      {
        p->ival = ReadInt1(pid, _setname, _vname, _vunit, i);
        var.Integer = p->ival;
      }
      else if(dsLOGICAL == dtype)
      {
        p->lval = ReadLog1(pid, _setname, _vname, _vunit, i);
        var.Logical = p->lval;
      }
    }
    if(ErrorCode == 0)
    {
      if(RangeCheck[dtype](&var, vmin, vmax) == SUCCESS)
      {
        sscanf(p->key, "%d, %d, %d, %d, %d, %d", &p->idx[0], &p->idx[1], &p->idx[2], &p->idx[3], &p->idx[4], &p->idx[5]);
        values.push_back(p);
      }
    }
    else
      TrackDelete(p);
  }
  return ErrorCode;
}

// Stores parameter values to the given setname and idx.
// The set must contain the parameter name and
// the ZZ vars for the conversion. Returns 0 on success,
Param::PutValues(char *_setname, int idx)
{
  int i, dtype, code;
  char _vname[SMALLSTRING];
  char _vunit[SMALLSTRING];

  i = 0;
  dtype = TypeString2Int(vtype);
  rstrcpy(_vname, vname.c_str());
  rstrcpy(_vunit, vunit.c_str());
  for(VALUE_VEC_ITR it = values.begin(); it!= values.end(); ++it)
  {
    ParamValue *p = (*it);
    i++;

    sprintf(p->key, "%d, %d, %d, %d, %d, %d", p->idx[0], p->idx[1], p->idx[2], p->idx[3], p->idx[4], p->idx[5]);
    SetIdx(idx, i);
    code = PutString(pid, _setname, "zzIDX", blank, _Idx, p->key);
    code += PutString(pid, _setname, "zzUUNT", blank, _Idx, p->uunt);
    code += PutString(pid, _setname, "zzUNT", blank, _Idx, p->vunt);
    code += PutInteger(pid, _setname, "zzREF", blank, _Idx, p->ref);
    code += PutInteger(pid, _setname, "zzREC", blank, _Idx, p->rec);
    //  code += PutInteger(pid, _setname, "zzFMT", blank, _Idx, p->fmt);
    SetIdx(i);
    if(dsSTRING == dtype)
    {
      code += PutString(pid, _setname, _vname, _vunit, _Idx, p->sval);
    }
    if(0<strlen(p->sval))
    {
      if(dsFLOAT == dtype)
      {
        code += PutFloat(pid, _setname, _vname, _vunit, _Idx, p->dval);
      }
      else if(dsINTEGER == dtype)
      {
        code += PutInteger(pid, _setname, _vname, _vunit, _Idx, p->ival);
      }
      else if(dsLOGICAL == dtype)
      {
        code += PutLogical(pid, _setname, _vname, _vunit, _Idx, p->lval);
      }
    }
  }
  return code;
}

//------------------------------------------------------------------------------
int iGid::AddRecToParams(char *_dic, paramrec *curr, int rec)
{
  int ErrorCode;

  Param *p = ParamsFind(curr->name);
  if(p == NULL)
  {
    p = new Param; TrackLocation(p);
    params.push_back(p);
    ErrorCode = GetVarType(pid, _dic, curr->name, dummy);
    p->vname = curr->name;
    p->vtype = dummy;
    GetVarMin(pid, _dic, curr->name, &p->vmin);
    GetVarMax(pid, _dic, curr->name, &p->vmax);
  }
  ParamValue *pv = new ParamValue(); TrackLocation(pv);

  pv->idx[0] = curr->cnt1;
  pv->idx[1] = curr->cnt2;
  pv->idx[2] = curr->cnt3;
  pv->idx[3] = curr->cnt4;
  pv->idx[4] = curr->cnt5;
  pv->idx[5] = curr->cnt6;
  pv->ref = curr->ref;
  pv->rec = rec;
  rstrcpy(pv->fmt, curr->fmt);
  rstrcpy(pv->uunt, curr->uunit);
  rstrcpy(pv->vunt, curr->punit);
  rstrcpy(pv->sval, curr->valu);

  int dtype = TypeString2Int(p->vtype);
  if(dsFLOAT == dtype)
    pv->dval = ratof(pv->sval);
  else if(dsINTEGER == dtype)
    pv->ival = ratoi(pv->sval);
  else if(dsLOGICAL == dtype)
  {
    if(0 == strnicmp(pv->sval, "T", 1) ||
       0 == strnicmp(pv->sval, "Y", 1) ||
       0 == strnicmp(pv->sval, "1", 1))
      pv->lval = 1;
    else
      pv->lval = 0;
  }

  p->values.push_back(pv);
  return 0;
}

void iGid::ParamsClear()
{
  for(PARAM_PTR_VEC_ITR it = params.begin(); it!= params.end(); ++it)
  {
    Param *p = (*it); // .second;
    TrackDelete(p);
  }
  params.clear();
}

Param *iGid::ParamsAdd(char *_dic, char *_name, char *v2dic, char *v2var)
{
  Param *prop = new Param; TrackLocation(prop);
  prop->vname = string(_name);
  rstrcpy(dummy, blank);
  int ErrorCode = GetVarUnit(pid, _dic, _name, dummy);
  if(ErrorCode == SUCCESS && strlen(dummy)) prop->vunit = string(dummy);
  ErrorCode = GetVarType(pid, _dic, _name, dummy);
  if(ErrorCode == SUCCESS && strlen(dummy))
    prop->vtype = string(dummy);
  else
  {
    sprintf(dummy, "ERROR in dictionary %s, variable %s, MISSING DATA TYPE - VARIABLE specification is INCOMPLETE!", v2dic, v2var);
    Log(dummy);
    if(v2dic!= NULL && v2var!= NULL)
    {
      ErrorCode = GetVarUnit(pid, v2dic, v2var, dummy);
      if(ErrorCode == SUCCESS && strlen(dummy))
        prop->vunit = string(dummy);
      ErrorCode = GetVarType(pid, v2dic, v2var, dummy);
      if(ErrorCode == SUCCESS && strlen(dummy))
        prop->vtype = string(dummy);
      GetVarMin(pid, v2dic, v2var, &prop->vmin);
      GetVarMax(pid, v2dic, v2var, &prop->vmax);
    }
    else
      prop->vtype = string("FLOAT");
  }
  GetVarMin(pid, _dic, _name, &prop->vmin);
  GetVarMax(pid, _dic, _name, &prop->vmax);

  params.push_back(prop); // params[prop->pname] = prop;
  return prop;
}

Param *iGid::ParamsFind(char *_name)
{
  char varname[SMALLSTRING];
  for(PARAM_PTR_VEC_ITR it = params.begin(); it!= params.end(); ++it)
  {
    Param *p = (*it);
    rstrcpy(varname, p->vname.c_str());
    if(0 == rstrcmpi(varname, _name)) return p;
  }
  return NULL;
}

ParamValue *iGid::ParamsFind(char *_name, int *_idx)
{
  char varname[SMALLSTRING];
  for(PARAM_PTR_VEC_ITR it = params.begin(); it!= params.end(); ++it)
  {
    Param *p = (*it);
    rstrcpy(varname, p->vname.c_str());
    if(0 == rstrcmpi(varname, _name))
    {
      for(VALUE_VEC_ITR iv = p->values.begin(); iv!= p->values.end(); ++iv)
      {
        ParamValue *pv = *iv;
        int ct = 0;
        for(int j = 0; j<GID_MAX_DIM; j++)
        { if(pv->idx[j] == _idx[j]) ct++; }
        if(ct == GID_MAX_DIM)
          return pv;
      }
    }
  }
  return NULL;
}

bool iGid::GetParamInteger(char *_name, int *_idx, int *_val)
{
  ParamValue *pv = ParamsFind(_name, _idx);
  if(pv) { *_val = atoi(pv->sval); return true; }
  return false;
}

bool iGid::GetParamLogical(char *_name, int *_idx, int *_val)
{
  ParamValue *pv = ParamsFind(_name, _idx);
  if(pv) { *_val = atoi(pv->sval); return true; }
  return false;
}

bool iGid::GetParamString(char *_name, int *_idx, char *_val)
{
  ParamValue *pv = ParamsFind(_name, _idx);
  if(pv) { rstrcpy(_val, pv->sval) ; return true; }
  return false;
}

bool iGid::GetParamFloat(char *_name, int *_idx, double *_val)
{
  ParamValue *pv = ParamsFind(_name, _idx);
  if(pv) { *_val = atof(pv->sval); return true; }
  return false;
}

bool iGid::GetParamValue(char *_name, int *_idx, int dtype, dtVariant *_val)
{
  ParamValue *pv = ParamsFind(_name, _idx);
  if(pv)
  {
    if(dtype == dsSTRING)  {_val->String = pv->sval;  return true; }
    if(dtype == dsFLOAT)   {_val->Float = pv->dval;   return true; }
    if(dtype == dsINTEGER) {_val->Integer = pv->ival; return true; }
    if(dtype == dsLOGICAL) {_val->Logical = pv->lval; return true; }
  }
  return false;
}

void iGid::SetParamValue(char *_name, int *_idx, dtVariant *_val, char *_dic, int rec)
{
  Param *prop = ParamsFind(_name);
  if(!prop)
  {
    prop = new Param; TrackLocation(prop);
    prop->vname = string(_name);
    int ErrorCode = GetVarUnit(pid, _dic, _name, dummy);
    prop->vunit = string(dummy);
    ErrorCode = GetVarType(pid, _dic, _name, dummy);
    prop->vtype = string(dummy);
    prop->itype = TypeString2Int(dummy);
    GetVarMin(pid, _dic, _name, &prop->vmin);
    GetVarMax(pid, _dic, _name, &prop->vmax);
    params.push_back(prop);
  }
  if(!ParamsFind(_name, _idx))
  {
    int dtype = TypeString2Int(prop->vtype);
    ParamValue *pv = new ParamValue(dtype, _idx, _val);  TrackLocation(pv);
    pv->rec = rec;
    prop->values.push_back(pv);
  }
  if(0 == prop->vunit.length())
    prop->vunit = string("n/a");
}

int iGid::ReadGID(char *outpath, char *uidic)
{
  int rec = 0;
  int ErrorCode = 0;
  bool AnError = false;

  ParamsClear();
  // check that gid output exists
  if(0!= access(AddExtension(outpath, "gid"), 0)) return -1;
  // read gid file contents into parameter list
  GIDFILE *G = Open_GID(AddExtension(outpath, "gid"));
  if(G == NULL) return -1;
  paramrec *pa;
  do
  {
    rec++;
    pa = readparamrec(G);
    AddRecToParams(uidic, pa, rec);
  }
  while(!G->f.eof());
  Close_GID(G);
  return rec;
}

//------------------------------------------------------------------------------
oGid::oGid(char *filename)
{
  if(0 == access(filename, 0))
    unlink(filename);
  fp = new ocsv(filename, '"', ',', _CREATE_); TrackLocation(fp);
  fp->alwaysQuote();
  nrec = 0;
}

oGid::~oGid()
{
  if(fp) TrackDelete(fp);
  nrec = 0;
}

void oGid::PutValue(const char *pname, int *idx, int ref, const char *uunit, const char *iunit, int val)
{
  if(!fp) return;
  *fp <<(char *)pname;
  *fp << idx[0] << idx[1] << idx[2] << idx[3] << idx[4] << idx[5] << ref;
  *fp <<(char *)uunit <<(char *)iunit << val;
  *fp << NewLn;
  nrec++;
}

void oGid::PutValue(const char *pname, int *idx, int ref, const char *uunit, const char *iunit, float val)
{
  if(!fp) return;
  *fp <<(char *)pname;
  *fp << idx[0] << idx[1] << idx[2] << idx[3] << idx[4] << idx[5] << ref;
  *fp <<(char *)uunit <<(char *)iunit << val;
  *fp << NewLn;
  nrec++;
}

void oGid::PutValue(const char *pname, int *idx, int ref, const char *uunit, const char *iunit, double val)
{
  if(!fp) return;
  *fp <<(char *)pname;
  *fp << idx[0] << idx[1] << idx[2] << idx[3] << idx[4] << idx[5] << ref;
  *fp <<(char *)uunit <<(char *)iunit << val;
  *fp << NewLn;
  nrec++;
}

void oGid::PutValue(const char *pname, int *idx, int ref, const char *uunit, const char *iunit, const char *val)
{
  if(!fp) return;
  *fp <<(char *)pname;
  *fp << idx[0] << idx[1] << idx[2] << idx[3] << idx[4] << idx[5] << ref;
  *fp <<(char *)uunit <<(char *)iunit;
  if(0 == strcmpi(val, "true") || 0 == strcmpi(val, "false"))
  {
    char sdelim = fp->sdelim;
    fp->sdelim = ' ';
    *fp << (char *)val;
    fp->sdelim = sdelim;
  }
  else
    *fp <<(char *)val;
  *fp << NewLn;
  nrec++;
}

void oGid::PutValue(const char *pname, int *idx, int ref, const char *uunit, const char *iunit, string val)
{
  PutValue(pname, idx, ref, uunit, iunit, val.c_str());
}
//----------------------------------------------------------------------------------------------------
ParamMap::ParamMap()
{
  v1dic = blank;
  v1var = blank;
  v1unt = blank;
  v1typ = blank;
  v2var = blank;
  for (int i=0; i<2; i++)
  {
    prefix[i] = NULL;
    v2dic[i] = NULL;
    v2unt[i] = NULL;
    v2typ[i] = NULL;
    for (int j=0; j<6; j++)
      v2depidx[i][j] = 0;
  }
  for (int j=0; j<6; j++)
  {
    v2depdic[j] = blank;
    v2depvar[j] = blank;
    v2depval[j] = blank;
  }
}

ParamMap::~ParamMap()
{
  int i;

  if(v1dic && v1dic != blank)         TrackDelete(v1dic);
  if(v1var && v1var != blank)         TrackDelete(v1var);
  if(v1unt && v1unt != blank)         TrackDelete(v1unt);
  if(v1typ && v1typ != blank)         TrackDelete(v1typ);
  if(v2var && v2var != blank)         TrackDelete(v2var);
  for (i=0; i<2; i++)
  {
    if(prefix[i] && prefix[i] != blank)     TrackDelete(prefix[i]);
    if(v2dic[i] && v2dic[i] != blank)       TrackDelete(v2dic[i]);
    if(v2unt[i] && v2unt[i] != blank)       TrackDelete(v2unt[i]);
    if(v2typ[i] && v2typ[i] != blank)       TrackDelete(v2typ[i]);
  }
  for (i=0; i<6; i++)
  {
    if(v2depdic[i] && v2depdic[i] != blank)   TrackDelete(v2depdic[i]);
    if(v2depvar[i] && v2depvar[i] != blank)   TrackDelete(v2depvar[i]);
    if(v2depval[i] && v2depval[i] != blank)   TrackDelete(v2depval[i]);
  }
}

int ParamMap::GetDependentValueIndex(char *dicprefix, STR_VEC_MAP osetList, bool write, int idx)
{
  int dim = 0;
  int ktype = -1;
  char v2dset[SMALLSTRING];
  char v2ddic[SMALLSTRING];
  char vunit[SMALLSTRING];
  long sethandle;
  long varhandle;
  STR_VEC_MAP_ITR it;

  if(0 == strlen(v2depval[idx])) return 0;
  if(0 == strlen(v2depvar[idx])) return 0;
  if(0 == strlen(v2depdic[idx])) return 0;

  //switch between chem and rad map settings
  if(0 == rstrcmpi(dicprefix, prefix[0]))  ktype = 0;
  if(0 == rstrcmpi(dicprefix, prefix[1]))  ktype = 1;
  if(ktype == -1) return -1;

  // already been here once before
  // this may be incorrect as the dep value may have changed
  if(v2depidx[ktype][idx]>0) return v2depidx[ktype][idx];

  sprintf(dummy, "GetDependentValueIndex %s%s %s %s %s", dicprefix, v2dic[ktype], v2var, v2depvar[idx], v2depval[idx]);  Log(dummy);

  // try with prefix
  it = osetList.find(concat(dicprefix,v2depdic[idx]));
  // if that fails try witout prefix
  if(it == osetList.end())
    it = osetList.find(v2depdic[idx]);

  if(it != osetList.end())
  {
    rstrcpy(v2ddic,(*it).first.c_str());
    rstrcpy(v2dset,(*it).second[0].c_str());
    sethandle = DataSetGetHandle(pid, v2dset);
    varhandle = VariableGetHandleByDataSet(pid, sethandle, v2depvar[idx]);
    v2depidx[ktype][idx] = VariableLookUp(pid, sethandle, varhandle, v2depval[idx], SetIdx());
    if(0 == v2depidx[ktype][idx] && write)
    {
      dim = VariableDimensionCount(pid, sethandle, varhandle, SetIdx());
      dim = dim + 1;
      int rc = GetVarUnit(pid, v2ddic, v2depvar[idx], vunit);
      WriteString1(pid, v2dset, v2depvar[idx], vunit, dim, v2depval[idx]);
      v2depidx[ktype][idx] = dim;
    }
    if(0 < v2depidx[ktype][idx]) return v2depidx[ktype][idx];
  }
  return -1;
}

void ParamMap::Initialize(char *_v1dic, char *_v1var, char *_v2var, char *_v2dic, char *_prefix0, char *_prefix1)
{
  int i;
  char *idxTemp;
  CStringParser v2args;
  CStringParser v2Idx;

  // copy strings for map
  v1dic = strdup(_v1dic); TrackLocation(v1dic);
  v1var = strdup(_v1var); TrackLocation(v1var);
  if(0 == GetVarUnit(pid, v1dic, v1var, dummy))
  {  v1unt = strdup(dummy);  TrackLocation(v1unt);  }

  prefix[0] = strdup(_prefix0);  TrackLocation(prefix[0]);
  prefix[1] = strdup(_prefix1);  TrackLocation(prefix[1]);
  v2dic[0] = strdup(concat(_prefix0,_v2dic));  TrackLocation(v2dic[0]);
  v2dic[1] = strdup(concat(_prefix1,_v2dic));  TrackLocation(v2dic[1]);

  //  [Variable] ~ [Index Dictionary] . [Index Variable] . [Index Value]
  //  SaltwaterFaunaBAF~BAF.AquaticFauna.Crustacea

  v2args.Parse(_v2var, CParseOptions('~'));
  v2var = strdup(v2args.GetCount()>0 ? v2args.GetAt(0) : blank);  TrackLocation(v2var);
  for (i=1; i< v2args.GetCount(); i++)
  {
    idxTemp = strdup(v2args.GetCount()>i ? v2args.GetAt(i) : blank);
    v2Idx.Parse(idxTemp, CParseOptions('.'));
    v2depdic[i] = strdup(v2Idx.GetCount()>0 ? v2Idx.GetAt(0) : blank);  TrackLocation(v2depdic[i]);
    v2depvar[i] = strdup(v2Idx.GetCount()>1 ? v2Idx.GetAt(1) : blank);  TrackLocation(v2depvar[i]);
    v2depval[i] = strdup(v2Idx.GetCount()>2 ? v2Idx.GetAt(2) : blank);  TrackLocation(v2depval[i]);
    if(idxTemp && idxTemp != blank) delete idxTemp;
  }

  // Dual role parameters found in _GID - CON section only,
  // TOS and AOS are in seperate sections but have similar dic
  // known prefixes are "Chem, Rad, Aquatic, Terrestrial"
  for (i=0; i<2; i++)
    if(strlen(prefix[i])>0)
    {
      if(0 == GetVarType(pid, v2dic[i], v2var, dummy))
      {  v2typ[i] = strdup(dummy);  TrackLocation(v2typ[i]);  }
      if(0 == GetVarUnit(pid, v2dic[i], v2var, dummy))
      {  v2unt[i] = strdup(dummy);  TrackLocation(v2unt[i]);  }
    }
}

/*
void ParamMap::oldInitialize(char *_v1dic, char *_v1var, char *_v2var, char *_v2dic, char *_prefix0, char *_prefix1)
{
  int i;
  CStringParser v2args;

  // copy strings for map
  v1dic = strdup(_v1dic); TrackLocation(v1dic);
  v1var = strdup(_v1var); TrackLocation(v1var);
  if(0 == GetVarUnit(pid, v1dic, v1var, dummy))
  {  v1unt = strdup(dummy);  TrackLocation(v1unt);  }

  prefix[0] = strdup(_prefix0);  TrackLocation(prefix[0]);
  prefix[1] = strdup(_prefix1);  TrackLocation(prefix[1]);
  v2dic[0] = strdup(concat(_prefix0,_v2dic));  TrackLocation(v2dic[0]);
  v2dic[1] = strdup(concat(_prefix1,_v2dic));  TrackLocation(v2dic[1]);

  v2args.Parse(_v2var, CParseOptions('.'));
  v2var = strdup(v2args.GetCount()>0 ? v2args.GetAt(0) : blank);  TrackLocation(v2var);
  v2depdic = strdup(v2args.GetCount()>1 ? v2args.GetAt(1) : blank);  TrackLocation(v2depdic);
  v2depvar = strdup(v2args.GetCount()>2 ? v2args.GetAt(2) : blank);  TrackLocation(v2depvar);
  v2depval = strdup(v2args.GetCount()>3 ? v2args.GetAt(3) : blank);  TrackLocation(v2depval);

  // ====== dual role parameters found in _GID
  for (i=0; i<2; i++)
    if(strlen(prefix[i])>0)
      if(0 == GetVarType(pid, v2dic[i], v2var, dummy))
      {
        if(v2typ[i]==NULL)
        {  v2typ[i] = strdup(dummy);  TrackLocation(v2typ[i]);  }
        if(0 == GetVarUnit(pid, v2dic[i], v2var, dummy))
        {  v2unt[i] = strdup(dummy);  TrackLocation(v2unt[i]);  }
      }
}

int ParamMap::oldGetDependentValueIndex(char *dicprefix, STR_VEC_MAP isetList, STR_VEC_MAP osetList, bool write)
{
  int dim = 0;
  int ktype = -1;
  char v2depset[SMALLSTRING];
  char v2ddic[SMALLSTRING];
  char vunit[SMALLSTRING];
  long sethandle;
  long varhandle;
  STR_VEC_MAP_ITR it;

  if(0 == strlen(v2depval)) return 0;
  if(0 == strlen(v2depvar)) return 0;
  if(0 == strlen(v2depdic)) return 0;

  if(0 == rstrcmpi(dicprefix, prefix[0]))  ktype = 0;
  if(0 == rstrcmpi(dicprefix, prefix[1]))  ktype = 1;
  if(ktype == -1) return -1;

  // already been here once before
  if(v2depidx[ktype]>0) return v2depidx[ktype];

  sprintf(dummy, "GetDependentValueIndex %s%s %s %s %s", dicprefix, v2dic[ktype], v2var, v2depvar, v2depval);  Log(dummy);

  // try with prefix
  it = osetList.find(concat(prefix[ktype],v2depdic));
  // if that fails try witout prefix
  if(it == osetList.end())
    it = osetList.find(v2depdic);

  if(it != osetList.end())
  {
    rstrcpy(v2ddic,(*it).first.c_str());
    rstrcpy(v2depset,(*it).second[0].c_str());
    sethandle = DataSetGetHandle(pid, v2depset);
    varhandle = VariableGetHandleByDataSet(pid, sethandle, v2depvar);
    v2depidx[ktype] = VariableLookUp(pid, sethandle, varhandle, v2depval, SetIdx());
    if(0 == v2depidx[ktype] && write)
    {
      dim = VariableDimensionCount(pid, sethandle, varhandle, SetIdx());
      dim = dim + 1;
      int rc = GetVarUnit(pid, v2ddic, v2depvar, vunit);
      WriteString1(pid, v2depset, v2depvar, vunit, dim, v2depval);
      v2depidx[ktype] = dim;
    }
    if(0 < v2depidx[ktype]) return v2depidx[ktype];
  }
  return -1;
}
*/