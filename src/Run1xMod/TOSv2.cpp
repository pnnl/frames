
#include "OSv2.h"

OS_v2::OS_v2(STR_SET setList, char *_mapname, char *_prefix, char *_uidic)
{
  rstrcpy(mapname,_mapname);
  rstrcpy(prefix,_prefix);
  rstrcpy(uidic,_uidic);

  GetMappingOfSectionParameters(setList);
}

OS_v2::~OS_v2()
{
  KeyListClear();
  VarMapClear();
}

// getting and clearing the keys for which to write the data
// it was never written that the order was the same in 1.x most of
// the time I think people programmed it that way, 2x is though
// I'm sure we will find out.  Bugs! I hate the ones you can't
// see until you run a very particular combination
void OS_v2::KeyListClear()
{
  for(STR_VEC_PTR_VEC_ITR it = keyList.begin(); it<keyList.end(); ++it)
  {
    STR_VEC *st = (*it);
    TrackDelete(st);
  }
  keyList.clear();
}

int OS_v2::GetKeyList(STR_VEC_MAP setlist, char *ID)
{
  int i;
  int nkey;
  string cname;
  STR_VEC *v;
  STR_VEC_PTR_MAP mapNames;
  STR_VEC_PTR_MAP_ITR mapIter;


  Log("Get Key Lists");
  if(0<GetSetName(concat(prefix, mapname), setname, blank, setlist))
  {
    nkey = 0;
    if(0 == GetVarDimSize(pid, setname, "ScientificName", SetIdx(), &nkey))
      for(i = 0; i<nkey; i++)
      {
        v = new STR_VEC; TrackLocation(v);
        v->push_back(string(prefix));
        if(0 == GetString(pid, setname, "ScientificName", blank, SetIdx(i+1), dummy))
        {
          v->push_back(string(dummy));
          GetString(pid, setname, "CommonName", blank, SetIdx(i+1), dummy);

          // order is important for autotesting
          cname = dummy;
          transform(cname.begin(),cname.end(),cname.begin(),(int(*)(int))tolower);
          mapNames[cname] = v;
        }
      }
    else
      Log("ERROR getting var dim size");
  }
  // reoder list
  KeyListClear();
  for(mapIter = mapNames.begin(); mapIter!= mapNames.end(); ++mapIter)
  {
    rstrcpy(dummy,(*mapIter).first.c_str());
    v = (*mapIter).second;
    keyList.push_back(v);
  }
  mapNames.clear();
  return 0;
}

void OS_v2::VarMapClear()
{
  for(PM_PTR_MAP_ITR it = VarMap.begin(); it!= VarMap.end(); ++it)
  {
    ParamMap *pm = (*it).second;
    TrackDelete(pm);
  }
  VarMap.clear();
}
                                                          
// mapname - Organism | Chem
// prefix - Terrestrial, Aquatic | Chem, Rad

int OS_v2::GetMappingOfSectionParameters(STR_SET dicList)
{
  int i;
  int j;
  int ndic;
  int nvar;
  int ErrorCode;
  char suffix[SMALLSTRING];
  char v1var[SMALLSTRING];
  char v2var[SMALLSTRING];
  STR_SET_ITR iv;
  ParamMap *v2map;

  // This function creates a mapping of old names to new
  // Building VarMap which is a map of V1 properties to V2 properties
  Log("Get Key Parameter Mapping");

  ndic = 0;
  VarMapClear();
  ErrorCode = GetVarDimSize(pid, iniset, concat(mapname,"dic"), SetIdx(), &ndic);

  for(i=1; i <= ndic; i++)
  {
    ErrorCode = GetString(pid, iniset, concat(mapname,"dic"), blank, SetIdx(i), suffix);
    iv = dicList.find(concat(prefix,suffix));
    // if find suceeds get count of vars
    nvar = 0;
    if(iv!= dicList.end())        
      ErrorCode = GetVarDimSize(pid, iniset, concat(mapname,"V1Var"), SetIdx(i), &nvar);

    for(j=1; j <= nvar; j++)
    {
      GetString(pid, iniset, concat(mapname,"V1Var"), blank, SetIdx(i, j), v1var);
      GetString(pid, iniset, concat(mapname,"V2Var"), blank, SetIdx(i, j), v2var);
      v2map = new ParamMap; TrackLocation(v2map);
      v2map->Initialize(uidic, v1var, v2var, suffix, prefix, blank);
      VarMap[v1var] = v2map;
    }
  }
  return 0;
}

int OS_v2::ReadDatasets(Glyph *g)  
{
  int j;
  int okey;
  int ErrorCode;
  int v1idx[MMF_MAX_DIM] = {0};
  int v2idx[MMF_MAX_DIM] = {0};
  char id[SMALLSTRING];
  char dicprefix[SMALLSTRING];
  dtVariant var;
  STR_VEC_ITR iv;
  STR_VEC_MAP_ITR it;
  STR_VEC_PTR_VEC_ITR ic ;

  Log("Read Key Datasets");
  // clears parameters even though they may have been loaded
  // if the the con glyph was a one 1x module we could
  // do a check here to save loading from datasets. all
  // data could have been loaded from input dataset
  g->gid.ParamsClear();

  // set the number of keys
  var.Integer = keyList.size();
  if (!rstrcmpi(uidic,"1xaos"))
    g->gid.SetParamValue("NumLifeForm", SetIdx(1), &var, uidic);
  else
    g->gid.SetParamValue("NumScientificName", SetIdx(1), &var, uidic);

  okey = 0;
  for(ic = keyList.begin(); ic!= keyList.end(); ++ic)
  {
    // get set name
    iv = (*ic)->begin();
    rstrcpy(dicprefix, iv->c_str());
    ++iv; rstrcpy(id, iv->c_str());
    it = g->osetlist.find(concat(dicprefix, mapname));
    rstrcpy(setname,(*it).second[0].c_str());

    // lookup casid and get v2idx
    for(j = 0; j<MMF_MAX_DIM; j++)  v1idx[j] = v2idx[j] = 0;
    ErrorCode = VarLookUp(pid, setname, "ScientificName", id, v2idx);
    if(ErrorCode == 0 && v2idx[0] > 0)
    {
      okey++;
      v1idx[0] = 1;
      v1idx[1] = okey;
      v1idx[2] = 0;
      ReadProperties(g, dicprefix, v2idx, v1idx, 1);
    }
  }
  return 0;
}

int OS_v2::ReadProperties(Glyph *g, char *dicprefix, int *v2idx, int *v1idx, int depidx)
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

  ktype = 0; 

  // copy indices so we don't change wha tis passed in
  for(i = 0; i<MMF_MAX_DIM; i++) { inpidx[i] = v2idx[i]; outidx[i] = v1idx[i]; }

  it = g->osetlist.find(concat(dicprefix, mapname));
  if(it!= g->osetlist.end())
  {
    rstrcpy(dicname,(*it).first.c_str());
    rstrcpy(setname,(*it).second[0].c_str());

    ErrorCode = GetString(pid, setname, "ScientificName", blank, inpidx, name);
    var.String = name;
    g->gid.SetParamValue("ScientificName", outidx, &var, uidic);
  }

  for(ivar = VarMap.begin(); ivar!= VarMap.end(); ++ivar)
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
        dep = pm->GetDependentValueIndex(dicprefix, g->osetlist, false);
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

int OS_v2::WriteDatasets(Glyph *g)
{
  int i;
  int n;
  int vct;
  int numlife;
  int ErrorCode;
  int v1idx[MMF_MAX_DIM];
  int v2idx[MMF_MAX_DIM];
  char vname[SMALLSTRING];
  char dicname[SMALLSTRING];
  char id[SMALLSTRING];
  STR_VEC_MAP_ITR it;
  PM_PTR_MAP_ITR ivar;
  Param *p;
  ParamMap *pm;
  ParamValue *pv;

  for(i=0; i < MMF_MAX_DIM; i++)  v1idx[i] = v2idx[i] = 0; 

  Log("WriteKeyDatasets");

  for(ivar = VarMap.begin(); ivar!= VarMap.end(); ++ivar)
  {
    pm = (*ivar).second;
    pm->v2depidx[0] = 0;
    pm->v2depidx[1] = 0;
  }

  // clear output datasets
  if(g->gid.params.size() == 0) return 0;
  for(it = g->osetlist.begin(); it!= g->osetlist.end(); ++it)
  {
    rstrcpy(dicname,(*it).first.c_str());
    rstrcpy(setname,(*it).second[0].c_str());
    ErrorCode = GetVarCount(pid, dicname, &vct);
    GetSetName(dicname, setname, g->ID2x, g->osetlist);
    for(i = 1; i<= vct; i++)
    {
      ErrorCode = GetVarName(pid, dicname, i, vname);
      ErrorCode = ClearVariable(pid, setname, vname, v1idx);
    }
  }

  // It could be the DCE will provide the constituent datasets 
  // and no conversion from gid will be necessary.
  // In this case it is assumed that the module UI dictionary is 
  // consistent with the generic 1xcon.dic and ImportDes.ini mapping file
  p = g->gid.ParamsFind("NumLifeForm"); 
  if (p==NULL)
    p = g->gid.ParamsFind("NumScientificName"); 

  pv = p->Value(1);
  numlife = pv->ival;
  for(n=1; n <= numlife; n++)
  {
    for(int i=0; i < MMF_MAX_DIM; i++)   v1idx[i] = v2idx[i] = 0;    
    v1idx[0] = n;
    v2idx[0] = n;

    if(g->gid.GetParamString("ScientificName", v1idx, id))
    {
      it = g->osetlist.find(concat(prefix, mapname));
      if(it!= g->osetlist.end())
      {
        rstrcpy(setname,(*it).second[0].c_str());
        WriteToDataset(setname, "ScientificName", blank, v2idx, dsSTRING, id);
        WriteProperties(g, prefix, v1idx, v2idx, 1);
      }
    }
  }
  return 0;
}

void OS_v2::WriteProperties(Glyph *g, char *dicprefix, int *v1idx, int *v2idx, int depidx)
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
  
  Log("Write organism properties");

  ktype = 0;
  for(i = 0; i<MMF_MAX_DIM; i++) { inpidx[i] = v1idx[i]; outidx[i] = v2idx[i]; }

  // process all constituent datasets in the VarMap
  for(ivar = VarMap.begin(); ivar!= VarMap.end(); ++ivar)
  {
    pm = (*ivar).second;
    outidx[depidx] = pm->v2depidx[ktype];
    it = g->osetlist.find(pm->v2dic[ktype]);
    if(it!= g->osetlist.end())
    {
      rstrcpy(v2set,(*it).second[0].c_str());
      p = g->gid.ParamsFind(pm->v1var);
      if(p!= NULL)
      {
        if(g->gid.GetParamString(pm->v1var, inpidx, cstr))
        {
          dep = pm->GetDependentValueIndex(dicprefix, g->osetlist, true);
          if(dep>0)
            outidx[depidx] = dep;
          WriteToDataset(v2set, pm->v2var, pm->v2unt[ktype], outidx, TypeString2Int(pm->v2typ[ktype]), cstr);
        }
      }
    }
  }
}