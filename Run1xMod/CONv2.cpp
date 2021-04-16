
#include "CONv2.h"

extern char *RAD;
extern char *CHEM;

CON_v2::CON_v2(STR_SET setList, char *_uidic)
{
  rstrcpy(uidic,_uidic);
  GetMappingOfConstituentParameters(setList);
}

CON_v2::~CON_v2()
{
  ConListClear();
  ConVarMapClear();
}

void CON_v2::ConListClear()
{
  for(STR_VEC_PTR_VEC_ITR it = conList.begin(); it<conList.end(); ++it)
  {
    STR_VEC *st = (*it);
    TrackDelete(st);
  }
  conList.clear();
}

int CON_v2::GetConstituentList(STR_VEC_MAP setlist, char *ID)
{
  int i;
  int ncon;
  int ntype;
  char *prefix;
  string cname;
  STR_VEC *v;
  STR_VEC_PTR_MAP mapNames;
  STR_VEC_PTR_MAP_ITR mapIter;

  Log("Get RAD and CHEM Constituent Lists");
  // this logic provides for constituent list in name order
  // == == == == == ==  which is consistent with 1X == == == == == == =
  for(ntype = 0; ntype<2; ntype++)
  {
    if(ntype == 0)     prefix = CHEM;
    else               prefix = RAD;
    if(0<GetSetName(concat(prefix, LIST), setname, blank, setlist))
    {
      ncon = 0;
      if(0 == GetVarDimSize(pid, setname, CASID, SetIdx(), &ncon))
        for(i = 0; i<ncon; i++)
        {
          v = new STR_VEC; TrackLocation(v);
          v->push_back(string(prefix));
          if(0 == GetString(pid, setname, CASID, blank, SetIdx(i+1), dummy))
          {
            v->push_back(string(dummy));
            GetString(pid, setname, NAME, blank, SetIdx(i+1), dummy);
            
            // order is important for autotesting
            cname = dummy;
            transform(cname.begin(),cname.end(),cname.begin(),(int(*)(int))tolower);
            mapNames[cname] = v;
          }
        }
      else
        Log("ERROR getting var dim size");
    }
  }
  // reoder list
  ConListClear();
  for(mapIter = mapNames.begin(); mapIter!= mapNames.end(); ++mapIter)
  {
    rstrcpy(dummy,(*mapIter).first.c_str());
    v = (*mapIter).second;
    conList.push_back(v);
  }
  mapNames.clear();
  return 0;
}

void CON_v2::ConVarMapClear()
{
  for(PM_PTR_MAP_ITR it = conVarMap.begin(); it!= conVarMap.end(); ++it)
  {
    ParamMap *pm = (*it).second;
    TrackDelete(pm);
  }
  conVarMap.clear();
}

int CON_v2::GetMappingOfConstituentParameters(STR_SET dicList)
{
  int i;
  int j;
  int ndic;
  int nvar;
  int ErrorCode;
  char suffix[SMALLSTRING];
  char rad[SMALLSTRING];
  char chem[SMALLSTRING];
  char v1var[SMALLSTRING];
  char v2var[SMALLSTRING];
  STR_SET_ITR iv;
  STR_SET_ITR ix;
  ParamMap *v2map;

  // This function creates a mapping of old names to new
  // Building conVarMap which is a map of V1 to V2 chemical properties
  Log("Get Constituent Parameter Mapping");

  ndic = 0;
  ConVarMapClear();
  ErrorCode = GetVarDimSize(pid, iniset, "ConstituentDic", SetIdx(), &ndic);

  for(i=1; i <= ndic; i++)
  {
    ErrorCode = GetString(pid, iniset, "ConstituentDic", blank, SetIdx(i), suffix);
    rstrcpy(rad, RAD);
    rstrcpy(chem, CHEM);

    iv = dicList.find(concat(RAD,suffix));
    if(iv == dicList.end())
      //no dictionary no mapping for rad
      rstrcpy(rad, blank);
    ix = dicList.find(concat(CHEM,suffix));
    if(ix == dicList.end())
      //no dictionary no mapping for chem
      rstrcpy(chem, blank); 

    // chem or rad find suceeds get var dim size
    nvar = 0;
    if(iv!= dicList.end() || ix!= dicList.end())  
      ErrorCode = GetVarDimSize(pid, iniset, "ConstituentV1Var", SetIdx(i), &nvar);

    for(j=1; j <= nvar; j++)
    {
      GetString(pid, iniset, "ConstituentV1Var", blank, SetIdx(i, j), v1var);
      GetString(pid, iniset, "ConstituentV2Var", blank, SetIdx(i, j), v2var);
      v2map = new ParamMap; TrackLocation(v2map);
      v2map->Initialize(uidic, v1var, v2var, suffix, chem, rad);
      conVarMap[v1var] = v2map;
    }
  }
  return 0;
}

int CON_v2::ReadConstituentDatasets(Glyph *g)  //assumes glyph is a con producer
{
  int j;
  int casidx, casidx2;
  int ErrorCode;
  int v1idx[MMF_MAX_DIM] = {0};
  int v2idx[MMF_MAX_DIM] = {0};
  int clktype = 0, ocon = 0, nds = 0;
  bool done;
  char cbuf[SMALLSTRING];
  char cas[SMALLSTRING];
  char dicprefix[SMALLSTRING];
  dtVariant var;
  STR_VEC_MAP_ITR it;
  PM_PTR_MAP_ITR ivar;
  STR_VEC_PTR_VEC_ITR ic;

  Log("Read Constituent Datasets");
  // clears parameters even though they may have been loaded
  // if the the con glyph was a one 1x module we will
  // do a check here to save loading from datasets.
  // all data could have been loaded from input dataset
  if (g->gid.params.size() > 0) return 0;
//  g->gid.ParamsClear();

  // see if we have dependent dics might need to add plant and animal dics
  it = g->osetlist.find("ChemList");
  if(it == g->osetlist.end())
  {
    it = g->osetlist.find("RadList");
    if(it == g->osetlist.end())
      return -1;
  }

  // set the number of constituents
  var.Integer = conList.size();
  g->gid.SetParamValue("NUMCON", SetIdx(1), &var, uidic, 1);

  ocon = 0;
  for(ic = conList.begin(); ic!= conList.end(); ++ic)
  {
    // get set name
    STR_VEC_ITR iv = (*ic)->begin();
    rstrcpy(dicprefix, iv->c_str());
    ++iv; rstrcpy(cas, iv->c_str());
    STR_VEC_MAP_ITR it = g->osetlist.find(concat(dicprefix, LIST));
    rstrcpy(setname,(*it).second[0].c_str());
    if(0 == strcmpi(dicprefix, CHEM)) clktype = 0;
    else clktype = 1;

    // lookup casid and get v2idx
    for(j = 0; j<MMF_MAX_DIM; j++)  v1idx[j] = v2idx[j] = 0; 
    ErrorCode = VarLookUp(pid, setname, "CASID", cas, v2idx);
    casidx = v2idx[0];
    if(ErrorCode == 0 && v2idx[0] > 0)
    {
      ocon++;
      v1idx[0] = 1;  // always only one site
      v1idx[1] = ocon;
      v1idx[2] = 0;
      ReadConstituentProperties(g, dicprefix, v2idx, v1idx, 1);

      nds = 0;
      done = false;
      while (!done)
      {
        rstrcpy(setname,(*it).second[0].c_str());
        ErrorCode = GetString(pid, setname, "DKPCASID", blank, v2idx, cbuf);
        if (ErrorCode==0 && 0<strlen(cbuf))
        {
          for(j = 0; j<MMF_MAX_DIM; j++) v2idx[j] = 0;
          ErrorCode = VarLookUp(pid, setname, "CASID", cbuf, v2idx);
          if (ErrorCode == 0 && v2idx[0] > 0)
          {
            nds++;
            v1idx[2] = nds;
            casidx2 = v2idx[0];
            ReadConstituentProperties(g, dicprefix, v2idx, v1idx, 1);

            if (nds==1)
            {
              rstrcpy(setname,(*it).second[0].c_str());
              if (0 == GetFloat(pid, setname, "DKPFRACTION", "fraction", SetIdx(casidx, 1), &var.Float))
                g->gid.SetParamValue("FSFRACTION", SetIdx(1, ocon, 1), &var, uidic);
              if (0 == GetFloat(pid, setname, "DKSFRACTION", "fraction", SetIdx(casidx, 1), &var.Float))
                g->gid.SetParamValue("SSFRACTION", SetIdx(1, ocon, 1), &var, uidic);
              if (0 == GetString(pid, setname, "DKSCASID", blank, SetIdx(casidx), cbuf))
              {
                var.String = cbuf;
                if (ErrorCode==0 && 0!=strlen(cbuf))
                {
                  g->gid.SetParamValue("SSCASID", SetIdx(1, ocon, 1), &var, uidic);
                  for(j = 0; j<MMF_MAX_DIM; j++) v2idx[j] = 0;
                  if(0 == VarLookUp(pid, setname, "CASID", cbuf, v2idx))
                  {
                    GetString(pid, setname, NAME, blank, v2idx, cbuf);
                    var.String = cbuf;
                    g->gid.SetParamValue("SSCNAME", SetIdx(1, ocon, 1), &var, uidic);
                  }
                }
              }
            }
            v2idx[0] = casidx2;
          }
          else
            done = true;
        }
        else
          done = true;
      }
      var.Integer = nds;
      v1idx[2] = 0;
      g->gid.SetParamValue("NDS", v1idx, &var, uidic);
    }
  }
  return 0;
}

int CON_v2::ReadConstituentProperties(Glyph *g, char *dicprefix, int *v2idx, int *v1idx, int depidx)
{
  int i;
  int dim;
  int dep;
  int ktype;
  int ErrorCode;
  int inpidx[MMF_MAX_DIM];
  int outidx[MMF_MAX_DIM];
  char name[SMALLSTRING];
  char dicname[SMALLSTRING];
  dtVariant var;
  Param *p = NULL;
  ParamMap *pm = NULL;
  ParamValue *pv = NULL;
  STR_VEC_MAP_ITR it;
  PM_PTR_MAP_ITR ivar;

  Log("ReadConstituentProperties");

  if(!rstrcmpi(dicprefix, CHEM)) 
    ktype = 0; 
  else 
    ktype = 1;

  // copy indices so we don't change what is passed in
  for(i = 0; i<MMF_MAX_DIM; i++) { inpidx[i] = v2idx[i]; outidx[i] = v1idx[i]; }

  it = g->osetlist.find(concat(dicprefix, LIST));
  if(it!= g->osetlist.end())
  {
    rstrcpy(dicname,(*it).first.c_str());
    rstrcpy(setname,(*it).second[0].c_str());

    ErrorCode = GetString(pid, setname, "CASID", blank, inpidx, name);
    var.String = name;
    g->gid.SetParamValue("FSCASID", outidx, &var, uidic);

    ErrorCode = GetString(pid, setname, NAME, blank, inpidx, name);
    var.String = name;
    g->gid.SetParamValue("FSCNAME", outidx, &var, uidic);

    var.Integer = ktype;
    g->gid.SetParamValue("CLKTYPE", outidx, &var, uidic);
  }

  for(ivar = conVarMap.begin(); ivar!= conVarMap.end(); ++ivar)
  {
    dim = 0;
    pm = (*ivar).second;
    it = g->osetlist.find(pm->v2dic[ktype]); // make sure dictionary exists
    if(it!= g->osetlist.end())
    {
      dim = 0;
      rstrcpy(setname,(*it).second[0].c_str());
      ErrorCode = GetVarDimSize(pid, setname, pm->v2var, inpidx, &dim);
      if(ErrorCode == 0 && dim> 0)
      {
        dep = pm->GetDependentValueIndex(dicprefix, g->osetlist, false, depidx);
        if(dep>=0)
          inpidx[depidx] = dep;
        p = g->gid.ParamsFind(pm->v1var);
        if(!p)
          p = g->gid.ParamsAdd(g->uidic, pm->v1var, pm->v2dic[ktype], pm->v2var);
        ErrorCode = p->ReadValues(setname, pm->v2var, inpidx, outidx);
      }
    }
  }
  return 0;
}

int CON_v2::WriteConstituentDatasets(Glyph *g)
{
  int i;
  int n;
  int ktype;
  int ocon;
  int nrad;
  int nchem;
  int numcon;
  int nds;
  int vct;
  int ErrorCode;
  int v1idx[MMF_MAX_DIM];
  int v2idx[MMF_MAX_DIM];
  char *dicprefix;
  char vname[SMALLSTRING];
  char dicname[SMALLSTRING];
  char casid[SMALLSTRING];
  char cbuf[MEDSTRING];
  STR_VEC_MAP_ITR it;
  PM_PTR_MAP_ITR ivar;
  Param *p;
  ParamMap *pm;
  ParamValue *pv;

  for(i = 0; i<MMF_MAX_DIM; i++)  v1idx[i] = v2idx[i] = 0; 

  Log("WriteConstituentDatasets");

  for(ivar = conVarMap.begin(); ivar!= conVarMap.end(); ++ivar)
  {
    pm = (*ivar).second;
    for(i=0; i<6; i++)
    {
      pm->v2depidx[0][i] = 0;
      pm->v2depidx[1][i] = 0;
    }
  }

  // clear output datasets
  if(g->gid.params.size() == 0) return 0;
  for(it = g->osetlist.begin(); it!= g->osetlist.end(); ++it)
  {
    rstrcpy(dicname,(*it).first.c_str());
    rstrcpy(setname,(*it).second[0].c_str());
    ErrorCode = GetVarCount(pid, dicname, &vct);
    GetSetName(dicname, setname, g->ID2x, g->osetlist);
    for(int i = 1; i<= vct; i++)
    {
      ErrorCode = GetVarName(pid, dicname, i, vname);
      ErrorCode = ClearVariable(pid, setname, vname, v1idx);
    }
  }

  // It could be the DCE will provide the constituent datasets
  // and no conversion from gid will be necessary.
  // In this case it is assumed that the module UI dictionary is 
  // consistent with the generic 1xcon.dic and ImportDes.ini mapping file  
  p = g->gid.ParamsFind("NUMCON"); 
  pv = p->Value(1);
  numcon = pv->ival;
  nrad = 0;
  nchem = 0;
  for(n=1; n <= numcon; n++)
  {
    for(i=0; i < MMF_MAX_DIM; i++)
    {
      v1idx[i] = 0;
      v2idx[i] = 0;
    }

    v1idx[0] = 1;
    v1idx[1] = n;

    if(g->gid.GetParamString("FSCASID", v1idx, casid))
    {
      if(g->gid.GetParamInteger("CLKTYPE", v1idx, &ktype))
      {
        if(ktype == 1)
        { dicprefix = RAD;  nrad++;  ocon = nrad; }
        else
        { dicprefix = CHEM; nchem++; ocon = nchem; }

        it = g->osetlist.find(concat(dicprefix, LIST));
        if(it!= g->osetlist.end())
        {
          v2idx[0] = ocon;
          rstrcpy(setname,(*it).second[0].c_str());
          WriteToDataset(setname, "CASID", blank, v2idx, dsSTRING, casid);
          if (g->gid.GetParamString("FSCNAME", v1idx, cbuf))
            WriteToDataset(setname, NAME, blank, v2idx, dsSTRING, cbuf);
          WriteConstituentProperties(g, dicprefix, v1idx, v2idx, 1);
          if(g->gid.GetParamInteger("NDS", v1idx, &nds))
          {
            if(nds>0)
            {
              double fraction = 0.0;
              if(g->gid.GetParamString("FSCASID", SetIdx(1, n, 1), casid))
                WriteToDataset(setname, "DKPCASID", blank, SetIdx(ocon, 1), dsSTRING, casid);
              if(g->gid.GetParamString("FSFRACTION", SetIdx(1, n, 1), cbuf))
                WriteToDataset(setname, "DKPFRACTION", "fraction", SetIdx(ocon, 1), dsFLOAT, cbuf);
              if(g->gid.GetParamString("SSCASID", SetIdx(1, n, 1), casid))
                WriteToDataset(setname, "DKSCASID", blank, SetIdx(ocon, 1), dsSTRING, casid);
              if(g->gid.GetParamString("SSFRACTION", SetIdx(1, n, 1), cbuf))
                WriteToDataset(setname, "DKSFRACTION", "fraction", SetIdx(ocon, 1), dsFLOAT, cbuf);
            }
          }
        }
      }
    }
  }
  return 0;
}

void CON_v2::WriteConstituentProperties(Glyph *g, char *dicprefix, int *v1idx, int *v2idx, int depidx)
{
  int i;
  int dep;
  int ktype;
  int inpidx[MMF_MAX_DIM];
  int outidx[MMF_MAX_DIM];
  char v2set[SMALLSTRING];
  char cstr[MAXSTRING];
  Param *p;
  ParamMap *pm;
  PM_PTR_MAP_ITR ivar;
  STR_VEC_MAP_ITR it;

  Log("Write constituent properties");

  if(!rstrcmpi(dicprefix, CHEM)) 
    ktype = 0; 
  else 
    ktype = 1;

  for(i = 0; i<MMF_MAX_DIM; i++) { inpidx[i] = v1idx[i]; outidx[i] = v2idx[i]; }

  // process all constituent datasets in the conVarMap
  for(ivar = conVarMap.begin(); ivar!= conVarMap.end(); ++ivar)
  {
    pm = (*ivar).second;
    outidx[depidx] = pm->v2depidx[ktype][depidx];
    it = g->osetlist.find(pm->v2dic[ktype]);
    if(it!= g->osetlist.end())
    {
      rstrcpy(v2set,(*it).second[0].c_str());
      p = g->gid.ParamsFind(pm->v1var);
      if(p!= NULL)
      {
        if(g->gid.GetParamString(pm->v1var, inpidx, cstr))
        {
          dep = pm->GetDependentValueIndex(dicprefix, g->osetlist, true, depidx);
          if(dep>=0)
            outidx[depidx] = dep;
          WriteToDataset(v2set, pm->v2var, pm->v2unt[ktype], outidx, TypeString2Int(pm->v2typ[ktype]), cstr);
          CheckErrors();
        }
      }
    }
  }
}

