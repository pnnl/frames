//---------------------------------------------------------------------------
#include "HIFv2.h"
//---------------------------------------------------------------------------

HIF_MAP_FILE HIFfiles;
STR_MAP hifMedia;
STR_VEC *vecCancerOrgans;
STR_VEC *vecDoseOrgans;

int numCancerOrgans = 0;  // exposure risk unit = "Risk"
int numDoseOrgans = 0;    // exposure risk unit = "Sv"
int numHQOrgans = 1;      // exposure risk unit = "HI"

char *HIFGetBaseName(char *exptype, char *expunit)
{
  char dicname[SMALLSTRING];
  rstrcpy(dicname, blank);
  if(!rstrcmpi(exptype, "Risk"))
  {
    if(!rstrcmpi(expunit, "carcinogenic")) strcat(dicname, "Carcinogenic");
    else if(!rstrcmpi(expunit, "cancer incidence")) strcat(dicname, "CancerIncidence");
    else if(!rstrcmpi(expunit, "cancer fatalities")) strcat(dicname, "CancerFatalities");
    else if(rstrstr(expunit, "cancer plus")) strcat(dicname, "CancerHereditaryEffects");
  }
  else if(0 == rstrcmpi(exptype, "HI"))
  {
    if(!rstrcmpi(expunit, "noncarcinogenic")) strcat(dicname, "NonCarcinogenic");
  }
  else if(0 == rstrcmpi(exptype, DOSE))
  {
    if(!rstrcmpi(expunit, "Sv")) strcat(dicname, "OrganDose");
  }
  return concat(blank, dicname);
}

HIFRoute_v2 *HIFFileFind(char *_prefix, char *_xtype, char *_unit)
{
  char name[SMALLSTRING];
  rstrcpy(name, _prefix, HIFGetBaseName(_xtype, _unit));
  HIF_MAP_FILE_ITR it = HIFfiles.find(name);
  if(it == HIFfiles.end()) return NULL;
  return (*it).second;
}

void HIFPathway_v2VecClear()
{
  for(HIF_MAP_FILE_ITR it = HIFfiles.begin(); it!= HIFfiles.end(); ++it)
  {
    HIFRoute_v2 *hif = (*it).second;
    HIF_VEC_PATH *v = hif->PathwayVec;
    for(HIF_VEC_PATH_ITR iv = v->begin(); iv!= v->end(); ++iv)
    {
      HIFPathway_v2 *pw = *iv;
      TrackDelete(pw);
    }
    v->clear();
    TrackDelete(v);
    hif->vecMedia.clear();
    hif->vecMediaSet.clear();
    TrackDelete(hif);
  }
  HIFfiles.clear();
}

void HIFInitializePathwayMap(STR_VEC_MAP setlist)
{
  int i, ct, np;
  int ErrorCode;
  int nummed = 0;
  int numpath = 0;
  int idx[MMF_MAX_DIM];
  char rte[SMALLSTRING];
  char Media[SMALLSTRING];
  char MediaSet[SMALLSTRING];
  char dicname[SMALLSTRING];

  HIFRoute_v2 *ef = NULL;
  HIFPathway_v2 *pw = NULL;
  HIF_VEC_PATH *v = NULL;

  Log("HIFInitializePathwayMap");
  HIFPathway_v2VecClear();

  for(i = 0; i<MMF_MAX_DIM; i++) idx[i] = 0;
  ErrorCode = VarLookUp(pid, iniset, FILETYPE, _HIF, idx);
  ct = 0;
  idx[1] = 1;
  ErrorCode = GetVarDimSize(pid, iniset, DICLIST, idx, &ct);
  for(i = 1; i<= ct; i++)
  {
    idx[2] = i;
    ErrorCode = GetString(pid, iniset, DICLIST, blank, idx, dicname);
    if(!strnicmp(dicname, CHEM, 4) || !strnicmp(dicname, RAD, 3))
    {
      STR_VEC_MAP_ITR it = setlist.find(dicname);
      if(it!= setlist.end())
      {
        // multiples of same dictionary
        STR_VEC v = (*it).second;
        for(int iv = 0; iv<v.size(); iv++)
        {
          rstrcpy(setname, v[iv].c_str());
          // dicname gives chem/rad and Impact
          ef = new HIFRoute_v2(dicname, setname); TrackLocation(ef);
          if(!HIFfiles[ef->pwdic])
          {
            nummed = 0;
            HIFfiles[ef->pwdic] = ef;
            ErrorCode = GetVarDimSize(pid, setname, MEDIA, SetIdx(), &nummed);
            for(int nm = 1; nm<= nummed; nm++)
            {
              ErrorCode = GetString(pid, setname, MEDIA, blank, SetIdx(nm), Media);
              ErrorCode = GetString(pid, setname, MEDIASET, blank, SetIdx(nm), MediaSet);

              ef->vecMedia.push_back((string)Media);
              ef->vecMediaSet.push_back((string)MediaSet);
              hifMedia[MediaSet] = string(Media);

              for(int nr = 1; nr<= 4; nr++)
              {
                if(nr == 1)      rstrcpy(rte, "Dermal");
                else if(nr == 2) rstrcpy(rte, "Ingestion");
                else if(nr == 3) rstrcpy(rte, "Inhalation");
                else if(nr == 4 && !strnicmp(dicname, RAD, 3))  rstrcpy(rte, "External");
                else break;
                numpath = 0;
                ErrorCode = GetVarDimSize(pid, setname, concat(rte, PATHWAY), SetIdx(nm), &numpath);
                for(np = 1; np<= numpath; np++)
                {
                  pw = new HIFPathway_v2(pid, setname, Media, rte, nm, np); TrackLocation(pw);
                  ef->PathwayVec->push_back(pw);
                }
              }
            }
          }
          else
          {
            TrackDelete(ef->PathwayVec);
            TrackDelete(ef);
          }
        }
      }
    }
  }
}

HIFPathway_v2::HIFPathway_v2(long pid, char *setname, char *Media, char *_rte, int nmi, int npi)
{
  hit = false;
  mIdx = nmi;
  pIdx = npi;
  rstrcpy(media, Media);
  rstrcpy(rte, _rte);
  int ErrorCode = GetString(pid, setname, concat(rte, PATHWAY), blank, SetIdx(nmi, npi), path);
}

HIFPathway_v2::HIFPathway_v2(long pid, char *setname, char *Media, int nmi, char *Path, char *_rte)
{
  int ErrorCode;
  int idx[MMF_MAX_DIM];

  hq = false;
  hit = false;
  mIdx = nmi;
  rstrcpy(path, Path);
  rstrcpy(media, Media);
  rstrcpy(rte, _rte);

  pIdx=0;
  for(int i = 0; i<MMF_MAX_DIM; i++) idx[i] = 0;
  idx[0] = mIdx;
  ErrorCode = VarLookUp(pid, setname, concat(rte, PATHWAY), path, idx);
  pIdx = idx[1];
  if(pIdx == 0)
  {
    ErrorCode = GetVarDimSize(pid, setname, concat(rte, PATHWAY), SetIdx(mIdx), &pIdx);
    pIdx++;
    ErrorCode = PutString(pid, setname, concat(rte, PATHWAY), blank, SetIdx(mIdx, pIdx), path);
  }
}

void HIFPathway_v2::WriteDataset(long pid, icsv *inf, char *setname, char *_unit, int na, int nc, int nt, int numpt, char *Xtype)
{
  float conc;
  int ErrorCode = 0;
  int numOrgans = 0;
  int idx[MMF_MAX_DIM];

  idx[0] = mIdx;
  idx[1] = nc;
  idx[2] = na;
  if(0 == rstrcmpi(Xtype, "HI"))
  {
    hq = true;
    numOrgans = 1;
    idx[4] = pIdx;
    idx[5] = nt;
  }
  else
  {
    if(0 == rstrcmpi(Xtype, "Risk"))       numOrgans = vecCancerOrgans->size();
    else if(0 == rstrcmpi(Xtype, DOSE))  numOrgans = vecDoseOrgans->size();
    idx[4] = pIdx;
    idx[5] = nt;
  }

  for (int pt = 1; pt<=numpt; pt++)
  {
    idx[6] = pt;
    for(int no = 1; no<= numOrgans; no++)
    {
      idx[3] = no;
      *inf >> conc;
      ErrorCode = PutFloat(pid, setname, rte, _unit, idx, conc);
    }
    *inf >> NewLn;
  }
}

HIFRoute_v2::HIFRoute_v2(char *dicname, char *setname)
{
  hit = false;
  rstrcpy(pwdic, dicname);
  rstrcpy(pwset, setname);

  if(rstrstr(pwdic, DOSE))
  {
    rstrcpy(xtype, DOSE);
    rstrcpy(unit, "Sv");
    numOrgans = numDoseOrgans;
  }
  else if(rstrstr(pwdic, "NonCarcinogenic"))
  {
    rstrcpy(xtype, "HI");
    rstrcpy(unit, "noncarcinogenic");
    numOrgans = 1;
  }
  else if(rstrstr(pwdic, "Carcinogenic"))
  {
    rstrcpy(xtype, "Risk");
    rstrcpy(unit, "carcinogenic");
    numOrgans = numCancerOrgans;
  }
  else if(rstrstr(pwdic, "Cancer"))
  {
    rstrcpy(xtype, "Risk");
    numOrgans = numCancerOrgans;
    if(rstrstr(pwdic, "Fatalities"))
    {  rstrcpy(unit, "cancer fatalities");  }
    else if(rstrstr(pwdic, "Incidence"))
    {  rstrcpy(unit, "cancer incidence");   }
    else if(rstrstr(pwdic, "Hereditary"))
    {  rstrcpy(unit, "cancer plus severe hereditary effects"); }
  }

  PathwayVec = new HIF_VEC_PATH; TrackLocation(PathwayVec);
}

void HIFRoute_v2::WriteDataset(long pid, icsv *inf, int na, int nc, int nt, int numpt, char *rte, char *Path, char *Unit)
{
  int mIdx;
  int idx[MMF_MAX_DIM];

  HIFPathway_v2 *pw = NULL;
  for(HIF_VEC_PATH_ITR iv = PathwayVec->begin(); iv!= PathwayVec->end(); ++iv)
  {
    HIFPathway_v2 *t = *iv;
    if(!rstrcmpi(t->path, Path) && !rstrcmpi(t->rte, rte) && !rstrcmpi(t->media, Media) && !rstrcmpi(unit, Unit))
    {
      pw = t;
      break;
    }
  }
  if(pw == NULL)
  {
    mIdx = 0;
    for(int i = 0; i<MMF_MAX_DIM; i++) idx[i] = 0;
    int ErrorCode = VarLookUp(pid, pwset, MEDIA, Media, idx);
    mIdx = idx[0];
    if(mIdx == 0)
    {
      ErrorCode = GetVarDimSize(pid, pwset, MEDIA, SetIdx(), &mIdx);
      mIdx++;
      ErrorCode = PutString(pid, pwset, MEDIA, blank, SetIdx(mIdx), Media);
      ErrorCode = PutString(pid, pwset, MEDIASET, blank, SetIdx(mIdx), MediaSet);
    }
    pw = new HIFPathway_v2(pid, pwset, Media, mIdx, Path, rte); TrackLocation(pw);
    PathwayVec->push_back(pw);
  }
  pw->WriteDataset(pid, inf, pwset, unit, na, nc, nt, numpt, xtype);
  pw->hit = true;
}

int HIFCon_v2::WriteDataset(long pid, icsv *inf, int nm, int na, int numpt, Glyph *g, STR_VEC_PTR_VEC *conlist)
{
  int i, nc, nt, no;
  int numpath;
  int idx[MMF_MAX_DIM];
  char setprefix[8];
  char exppath[SMALLSTRING];
  char exprte[SMALLSTRING];
  char expunit[SMALLSTRING];
  char exptype[SMALLSTRING];
  char tunit[SMALLSTRING];
  char dunit[SMALLSTRING];

  STR_VEC_ITR iv;
  HIFRoute_v2 *ef;
  HIFPathway_v2 *pw;
  HIF_MAP_FILE_ITR it;

  *inf >> dummy >> casid >> nprog >> ntimes >> NewLn;
  if(ntimes<= 0) return 0;

  for(STR_VEC_PTR_VEC_ITR ic = conlist->begin(); ic!= conlist->end(); ++ic)
  {
    STR_VEC_ITR iv = (*ic)->begin();
    rstrcpy(setprefix, iv->c_str());
    ++iv;
    rstrcpy(dummy, iv->c_str());
    if(!rstrcmpi(casid, dummy)) break;
  }

  GetSetName(concat(setprefix, LIST), setname, blank, g->isetlist);

  for(i = 0; i<MMF_MAX_DIM; i++) idx[i] = 0;
  int ErrorCode = VarLookUp(pid, setname, "CASID", casid, idx);
  nc = idx[0];

  //clear hits
  pw = NULL;
  for(it = HIFfiles.begin(); it!= HIFfiles.end(); ++it)
  {
    ef = (*it).second;
    ef->hit = false;
    for(HIF_VEC_PATH_ITR ip = ef->PathwayVec->begin(); ip!= ef->PathwayVec->end(); ++ip)
    {
      pw = *ip;
      pw->hit = false;
    }
  }

  float *times = new float[ntimes];  TrackLocation(times);
  for(nt = 1; nt<= ntimes; nt++)
  {
    *inf >> times[nt-1] >> tunit >> duration >> dunit >> numpath >> NewLn;
    for(i = 1; i<= numpath; i++)
    {
      *inf >> population >> exppath >> exprte >> exptype >> expunit >> NewLn;
      ef = HIFFileFind(setprefix, exptype, expunit);
      if(ef)
      {
        ef->hit = true;
        ef->WriteDataset(pid, inf, na, nc, nt, numpt, exprte, exppath, expunit);
        CheckErrors();
      }
      else
      {
        sprintf(dummy, "_HIF dataset not found for %s path:%s route:%s unit:%s", casid, exppath, exprte, expunit); Log(dummy);
        *inf >> NewLn;
      }
    }
  }

  for(it = HIFfiles.begin(); it!= HIFfiles.end(); ++it)
  {
    ef = (*it).second;
    if(ef->hit)
    {
      for(nt = 1; nt<= ntimes; nt++)
        ErrorCode = PutFloat(pid, ef->pwset, TIMEPTS, YEAR, SetIdx(nm, nc, nt), times[nt-1]);
      if(0 == rstrcmpi(ef->xtype, "Risk"))
      {
        no = 0;
        for(iv = vecCancerOrgans->begin(); iv!= vecCancerOrgans->end(); ++iv)
        {
          no++;
          rstrcpy(dummy,(*iv).c_str());
          ErrorCode = PutString(pid, ef->pwset, "EffectedOrgan", blank, SetIdx(nm, no), dummy);
        }
      }
      else if(0 == rstrcmpi(ef->xtype, DOSE))
      {
        no = 0;
        for(iv = vecDoseOrgans->begin(); iv!= vecDoseOrgans->end(); ++iv)
        {
          no++;
          rstrcpy(dummy,(*iv).c_str());
          ErrorCode = PutString(pid, ef->pwset, "EffectedOrgan", blank, SetIdx(nm, no), dummy);
        }
      }
      ef->hit = false;
    }
  }
  TrackDelete(times);
  return 0;
}

int HIFSet_v2::WriteDataset(long pid, icsv *inf, int nm, Glyph *g, STR_VEC_PTR_VEC *conlist)
{
  int na;
  int ErrorCode;
  int idx[MMF_MAX_DIM];
  char dummy[SMALLSTRING];
  char dicname[SMALLSTRING];

  *inf >> dstype >> locname >> medtype >> numpt >> numage >> numcon >> numCancerOrgans >> numDoseOrgans >> NewLn;

  rstrcpy(Media, blank);
  rstrcpy(MediaSet, blank);
  if(!rstrcmpi(SURFACE_WATER, medtype))
    rstrcpy(dicname,SURFACEWATER,PTS);
  else if(!rstrcmpi(SOIL, medtype) || !rstrcmpi(SEDIMENT, medtype))
    rstrcpy(dicname,medtype,VOLUMES);
  else
    rstrcpy(dicname,medtype,PTS);

  // set media set name from source of source sources
  for(GLYPH_PTR_MAP_ITR jg = g->sources.begin(); jg!= g->sources.end(); ++jg)
  {
    Glyph *sg = (*jg).second;
    sg->GetInputModuleList();
    for(GLYPH_PTR_MAP_ITR ig = sg->sources.begin(); ig!= sg->sources.end(); ++ig)
    {
      SetMedia(pid,numpt,ig,dicname,dstype,locname,medtype);
      if(strlen(MediaSet)) break;
    }
  }

  // set media set name from source sinks
  if(!strlen(MediaSet))
    for(GLYPH_PTR_MAP_ITR ig = g->sources.begin(); ig!= g->sources.end(); ++ig)
    {
      SetMedia(pid,numpt,ig,dicname,dstype,locname,medtype);
      if(strlen(MediaSet)) break;
    }

  vecCancerOrgans->clear();
  vecDoseOrgans->clear();
  for(na = 1; na<= numCancerOrgans; na++)
  {
    *inf >> dummy;
    vecCancerOrgans->push_back(string(dummy));
  }
  *inf >> NewLn;

  for(na = 1; na<= numDoseOrgans; na++)
  {
    *inf >> dummy;
    vecDoseOrgans->push_back(string(dummy));
  }
  *inf >> NewLn;

  if(!strlen(MediaSet))
  {
    GetSetName(dicname,setname, g->ID2x, g->osetlist);
    for(int np = 1; np<= numpt; np++)
    {
    // if mediaset is blank then must be known _EPF concentrations
    // a media set must be produced for known modules
    // the name of this dictionary is determined by medtype
    // locations will always be of type point
      *inf >> x >> dummy >> y >> dummy >> NewLn;
      ErrorCode = PutString(pid, setname, FEATURE, blank, SetIdx(np), locname);
      ErrorCode = PutFloat(pid, setname, FEATUREPTS, KM, SetIdx(np,1,1), x);
      ErrorCode = PutFloat(pid, setname, FEATUREPTS, KM, SetIdx(np,2,1), y);
      ErrorCode = PutFloat(pid, setname, FEATUREPTS, KM, SetIdx(np,3,1), 0.0);

      ErrorCode = PutString(pid, setname, ATTRIBUTEDES, blank, SetIdx(1), "Type of medium");
      ErrorCode = PutString(pid, setname, ATTRIBUTES, blank, SetIdx(np, 1), medtype);
      rstrcpy(MediaSet,setname);
      sprintf(Media, "%s.%s.%s.%s", g->ID1x, dstype, locname, medtype);
    }
  }
  else
    inf->Skip(numpt);

  // for 1.x module all modules deal with only one population center
  na = 0;
  for(int i = 0; i<MMF_MAX_DIM; i++) idx[i] = 0;
  ErrorCode = GetVarDimSize(pid, setPopulation, FEATURE, idx, &na);
  if(na==0)
  {
    ErrorCode = PutString(pid, setPopulation, FEATURE,  blank, SetIdx(1), g->ID1x);
    ErrorCode = PutFloat(pid, setPopulation, FEATUREPTS, KM, SetIdx(1,1,1), g->x);
    ErrorCode = PutFloat(pid, setPopulation, FEATUREPTS, KM, SetIdx(1,2,1), g->y);
    ErrorCode = PutFloat(pid, setPopulation, FEATUREPTS, KM, SetIdx(1,3,1), 0.0);
  }

  for(int n = 1; n<= numage; n++)
  {
    *inf >> agemin >> agemax >> dummy >> NewLn;
    sprintf(dummy, "Group %d",n);
    for(int i = 0; i<MMF_MAX_DIM; i++) idx[i] = 0;
    ErrorCode = VarLookUp(pid, setAgeGroup, NAME, dummy, idx);
    na = idx[0];
    if(idx[0] == 0)
    {
      ErrorCode = GetVarDimSize(pid, setAgeGroup, NAME, idx, &na);
      na++;
      ErrorCode = PutString(pid, setAgeGroup, NAME, blank, SetIdx(na), dummy);
      ErrorCode = PutFloat(pid,  setAgeGroup, STARTAGE, YEAR, SetIdx(na), agemin);
      ErrorCode = PutFloat(pid,  setAgeGroup, ENDAGE, YEAR, SetIdx(na), agemax);
    }

    for(int nc = 1; nc<= numcon; nc++)
    {
      HIFCon_v2 *p = new HIFCon_v2(); TrackLocation(p);
      p->WriteDataset(pid, inf, nm, na, numpt, g, conlist);
      TrackDelete(p);
    }
    ErrorCode = PutFloat(pid, setAgeGroup, DURATION, YEAR, SetIdx(na), duration);
    ErrorCode += PutString(pid, setAgeGroup, POPFEATURE, blank, SetIdx(na), g->ID1x);
    ErrorCode += PutInteger(pid, setAgeGroup, POPULATION, YEAR, SetIdx(na), population);
  }

  return numpt;
}

int HIF_v2::WriteDataset(long pid, char *fuiname, Glyph *g, STR_VEC_PTR_VEC *conlist)
{
  int i;
  int numds;
  int numInfo;

  vecCancerOrgans = new STR_VEC;
  vecDoseOrgans = new STR_VEC;

  Log("_HIF::WriteDataset");
  // this will create an empty map that will be filled
  // by _HIF datasets as pathways are encountered (why?)
  HIFInitializePathwayMap(g->osetlist);

  // if these are blank then must create them when writing datasets
  GetSetName(AGEGROUP, setAgeGroup, "", g->isetlist);
  GetSetName(concat(POPULATION,POLYS), setPopulation, blank, g->isetlist);

  rstrcpy(dummy, fuiname, _DOT, _HIF);
  inf = new icsv(dummy, '\"'); TrackLocation(inf);
  if(inf->ok())
  {
    *inf >> numInfo >> NewLn;
    for(i = 0; i< numInfo; i++)
      *inf >> dummy >> NewLn;

    *inf >> numds >> NewLn;
    for(i = 0; i< numds; i++)
    {
      HIFSet_v2 *ds = new HIFSet_v2(); TrackLocation(ds);
      ds->WriteDataset(pid, inf, i+1,  g, conlist);
      TrackDelete(ds);
    }
  }
  TrackDelete(inf);
  HIFPathway_v2VecClear();
  hifMedia.clear();
  vecCancerOrgans->clear();
  vecDoseOrgans->clear();
  delete vecCancerOrgans;
  delete vecDoseOrgans;
  return 0;
}

//======================================================================================
int HIFCon_v2::WriteFile(long pid, ocsv *fp, int nm, int na, int numpt, Glyph *g, STR_VEC *conlist, char *ID)
{
  int nc, nt, nval, nrec = 0;
  int ErrorCode;
  double ftime, fval;
  char setprefix[8];
  int idx[MMF_MAX_DIM] = {0};
  char cas[SMALLSTRING];
  char name[SMALLSTRING];

  HIF_MAP_FILE_ITR it;
  HIF_VEC_PATH_ITR ip;
  HIFRoute_v2 *eftime = NULL;
  HIFPathway_v2 *pw = NULL;
  HIFPathway_v2 *pwtime = NULL;

  STR_VEC_ITR iv = conlist->begin();
  rstrcpy(setprefix, iv->c_str());
  ++iv;
  rstrcpy(cas, iv->c_str());

  GetSetName(concat(setprefix, LIST), setname, blank, g->isetlist);
  ErrorCode = VarLookUp(pid, setname, CASID, cas, idx);
  nc = idx[0];
  ErrorCode = GetString(pid, setname, NAME, blank, idx, name);

  eftime = NULL;
  for(it = HIFfiles.begin(); it!= HIFfiles.end(); ++it)
  {
    HIFRoute_v2 *ef = (*it).second;
    if(rstrstr(ef->pwdic, setprefix))
    {
      for(ip = ef->PathwayVec->begin(); ip!= ef->PathwayVec->end(); ++ip)
      {
        pw = (*ip);
        if(!rstrcmpi(pw->media, Media))
        {
          ntimes = 0;
          ErrorCode = GetVarDimSize(pid, ef->pwset, TIMEPTS, SetIdx(pw->mIdx, nc), &ntimes);
          if(ntimes>0)
          {
            eftime = ef;
            pwtime = pw;
            break;
          }
        }
      }
    }
    if(eftime) break;
  }

  if(eftime == NULL)
  {
    *fp << name << cas << 0 << 0 << NewLn; nrec++;
    return nrec;
  }
  else
    *fp << name << cas << 0 << ntimes << NewLn; nrec++;

  ErrorCode = GetFloat(pid, setAgeGroup, DURATION, YEAR, SetIdx(na), &duration);
  for(nt = 1; nt<= ntimes; nt++)
  {
    ErrorCode = GetFloat(pid, eftime->pwset, TIMEPTS, YEAR, SetIdx(pwtime->mIdx, nc, nt), &ftime);
    // count paths, including carcinogenic and noncarcinogenic
    int numpath = 0;
    for(it = HIFfiles.begin(); it!= HIFfiles.end(); ++it)
    {
      HIFRoute_v2 *ef = (*it).second;
      int x = ef->PathwayVec->size();
      if(rstrstr(ef->pwdic, setprefix) && (x>0))
      {
        for(ip = ef->PathwayVec->begin(); ip!= ef->PathwayVec->end(); ++ip)
        {
          pw = (*ip);
          if(!rstrcmpi(pw->media, Media))
          {
            nval = 0;
            ErrorCode = GetVarDimSize(pid, ef->pwset, pw->rte, SetIdx(pw->mIdx), &nval);
            if(ErrorCode == 0 && nval>= nc)
            {
              ef->hit = true;
              pw->hit = true;
              numpath++;
            }
          }
        }
      }
    }

    *fp << ftime << YEAR << duration << YEAR << numpath << NewLn; nrec++;

    if(numpath)
      for(it = HIFfiles.begin(); it!= HIFfiles.end(); ++it)
      {
        HIFRoute_v2 *ef = (*it).second;
        if(ef->hit)
        {
          int x = ef->PathwayVec->size();
          for(ip = ef->PathwayVec->begin(); ip!= ef->PathwayVec->end(); ++ip)
          {
            pw = (*ip);
            if(pw->hit)
            {
              *fp << 1.0 << pw->path << pw->rte << ef->xtype << ef->unit << NewLn; nrec++;
              for(int np = 1; np<= numpt; np++)
              {
                for(int no = 1; no<= ef->numOrgans; no++)
                {
                  ErrorCode = GetFloat(pid, ef->pwset, pw->rte, ef->unit, SetIdx(pw->mIdx, nc, na, no, pw->pIdx, nt, np), &fval);
                  if (ErrorCode)
                    printf("Missing %s %s %s %s %s\n",ef->pwset, name, pw->path, pw->rte, ef->unit);
                  *fp << fval;
                }
                *fp << NewLn; nrec++;
              }
              pw->hit = false;
            }
          }
          ef->hit = false;
        }
      }
  }
  return nrec;
}

int HIFSet_v2::WriteFile(long pid, ocsv *fp, int nm, Glyph *g, STR_VEC_PTR_VEC *conlist, char *ID)
{
  int no;
  int nrec = 0;
  int ErrorCode;
  int featureIdx;
  int idx[MMF_MAX_DIM];
  HIFRoute_v2 *ef;

  CStringParser args;
  args.Parse(Media, CParseOptions('.'));
  rstrcpy(hifname, args.GetAt(0)); // location name
  rstrcpy(dstype, args.GetAt(1));
  rstrcpy(locname, args.GetAt(2));
  rstrcpy(medtype, args.GetAt(3)); // Media

  vecCancerOrgans->clear();
  vecDoseOrgans->clear();

  int nmed = 0;
  for(HIF_MAP_FILE_ITR it = HIFfiles.begin(); it!= HIFfiles.end(); ++it)
  {
    ef = (*it).second;
    for(HIF_VEC_PATH_ITR iv = ef->PathwayVec->begin(); iv!= ef->PathwayVec->end(); ++iv)
    {
      HIFPathway_v2 *pw = (*iv);
      if(0 == strcmpi(pw->media, Media))
      {
        nmed = pw->mIdx;
        if(rstrstr(ef->pwdic, DOSE))
        {
          if(0 == vecDoseOrgans->size())
          {
            numDoseOrgans = 0;
            ErrorCode = GetVarDimSize(pid, ef->pwset, "EffectedOrgan", SetIdx(nmed), &numDoseOrgans);
            for(no = 1; no<= numDoseOrgans; no++)
            {
              ErrorCode = GetString(pid, ef->pwset, "EffectedOrgan", blank, SetIdx(nmed, no), dummy);
              vecDoseOrgans->push_back(string(dummy));
            }
          }
          ef->numOrgans = vecDoseOrgans->size();
        }
        if(rstrstr(ef->pwdic, "Cancer"))
        {
          if(0 == vecCancerOrgans->size())
          {
            numCancerOrgans = 0;
            ErrorCode = GetVarDimSize(pid, ef->pwset, "EffectedOrgan", SetIdx(nmed), &numCancerOrgans);
            for(no = 1; no<= numCancerOrgans; no++)
            {
              ErrorCode = GetString(pid, ef->pwset, "EffectedOrgan", blank, SetIdx(nmed, no), dummy);
              vecCancerOrgans->push_back(string(dummy));
            }
          }
          ef->numOrgans = vecCancerOrgans->size();
        }
        if(rstrstr(ef->pwdic, "Carcinogenic"))
          ef->numOrgans = 1;
      }
    }
  }

  if(0 == vecCancerOrgans->size())
  {
    vecCancerOrgans->push_back("all sites");
    numCancerOrgans = 1;
  }

  if(0 == vecDoseOrgans->size())
  {
    vecDoseOrgans->push_back("total body");
    numDoseOrgans = 1;
  }

  sprintf(dummy, "Writing _HIF from datasets for %s, %s, %s", dstype, locname, medtype); Log(dummy);

  for(int i = 0; i<MMF_MAX_DIM; i++) idx[i] = 0;

  // air has multiple location in a set
  // this is not consistant with other
  // transport files (scf,wff,wcf,aff)
  numpt=0;
  if (!rstrcmpi(medtype,AIR) && !rstrcmpi(hifname,AIR))  // check for single air point must be one or all
  {
    // feature are points for air
    ErrorCode = GetVarDimSize(pid, MediaSet, FEATURE, SetIdx(), &numpt);
  }
  else
  {
    // could be volume or point but always just one per set
    ErrorCode = VarLookUp(pid, MediaSet, FEATURE, hifname, idx);
    featureIdx = idx[0];
  }
  numage = 0;
  ErrorCode = GetVarDimSize(pid, setAgeGroup, NAME, SetIdx(), &numage);
  numcon = conlist->size();
  *fp << dstype << locname << medtype;
  if(numpt)
    *fp << numpt;
  else
    *fp << 1;
  *fp << numage << numcon << numCancerOrgans << numDoseOrgans << NewLn;  nrec++;

  for(no = 0; no< numCancerOrgans; no++)
  {
    rstrcpy(dummy, vecCancerOrgans->at(no).c_str());
    *fp << dummy;
  }
  *fp << NewLn; nrec++;

  for(no = 0; no< numDoseOrgans; no++)
  {
    rstrcpy(dummy, vecDoseOrgans->at(no).c_str());
    *fp << dummy;
  }
  *fp << NewLn; nrec++;

  sprintf(dummy, "Writing _HIF from datasets for %s, %s, %s", dstype, locname, medtype); Log(dummy);

  if(numpt)
  {
    for(int np = 1; np<= numpt; np++)
    {
      ErrorCode = GetFloat(pid, MediaSet, FEATUREPTS, KM, SetIdx(np, 1, 1), &x);
      ErrorCode = GetFloat(pid, MediaSet, FEATUREPTS, KM, SetIdx(np, 2, 1), &y);
      //ErrorCode = GetFloat(pid, MediaSet, FEATUREPTS, KM, SetIdx(np, 3, 1), &z);
      *fp << x << KM << y << KM << NewLn;  nrec++;
    }
  }
  else
  {
    numpt=1;
    *fp << g->PDCFglyph->x << KM << g->PDCFglyph->y << KM << NewLn;  nrec++;
  }

  if(numcon == 0) return nrec;
  if(numage == 0) return nrec;

  for(int na = 1; na<= numage; na++)
  {
    ErrorCode = GetFloat(pid, setAgeGroup, STARTAGE, YEAR, SetIdx(na), &agemin);
    ErrorCode = GetFloat(pid, setAgeGroup, ENDAGE, YEAR, SetIdx(na), &agemax);
    ErrorCode = GetFloat(pid, setAgeGroup, DURATION, YEAR, SetIdx(na), &duration);
    ErrorCode = GetInteger(pid, setAgeGroup, POPULATION, blank, SetIdx(na), &population);

    *fp << agemin << agemax << YEAR << NewLn; nrec++;

    for(int nc = 1; nc<= numcon; nc++)
    {
      HIFCon_v2 *p = new HIFCon_v2();  TrackLocation(p);
      nrec += p->WriteFile(pid, fp, nm, na, numpt, g,(*conlist)[nc-1], ID);
      TrackDelete(p);
    }
  }
  numcon = 0;
  return nrec;
}

int HIF_v2::WriteFile(long pid, char *outpath, Glyph *g, STR_VEC_PTR_VEC *conlist, char *ID, char *ID2x)
{
  int i;
  int numds;
  long nrec = 0;
  long posbeg = 0;
  long posend = 0;

  Log("_HIF::WriteFile");

  vecCancerOrgans = new STR_VEC;
  vecDoseOrgans = new STR_VEC;

  GetSetName(AGEGROUP, setAgeGroup, blank, g->isetlist);
  GetSetName(concat(POPULATION,POLYS), setPopulation, ID2x, g->isetlist);
  HIFInitializePathwayMap(g->isetlist);
  numds = hifMedia.size();

  // check whether to create hif or append existing hif
  if(0 == access(AddExtension(outpath, _HIF), 0))
  { outf = new ocsv(AddExtension(outpath, _HIF), '"', ',', _APPEND_); TrackLocation(outf); }
  else
  { outf = new ocsv(AddExtension(outpath, _HIF), '"', ',', _CREATE_); TrackLocation(outf); }
  if(!outf->ok()) return 0;

  posbeg = outf->getpos();
  outf->smartQuote();
  *outf << ID << "         " << NewLn;
  outf->alwaysQuote();
  *outf << 1 << NewLn; nrec++;
  *outf << "_HIF generated by 1X Wrapper in Frames V2" << NewLn; nrec++;
  *outf << numds << NewLn; nrec++;

  i = 0;
  for(STR_MAP_ITR is = hifMedia.begin(); is!= hifMedia.end(); ++is)
  {
    i++;
    rstrcpy(Media,(*is).second.c_str());
    rstrcpy(MediaSet,(*is).first.c_str());
    HIFSet_v2 *p = new HIFSet_v2(); TrackLocation(p);
    nrec += p->WriteFile(pid, outf, i, g, conlist, ID2x);
    TrackDelete(p);
  }
  outf->setpos(posbeg);
  *outf << ID;
  fprintf(outf->fptr,"%ld",nrec);
  TrackDelete(outf);
  HIFPathway_v2VecClear();
  hifMedia.clear();
  vecCancerOrgans->clear();
  vecDoseOrgans->clear();
  delete vecCancerOrgans;
  delete vecDoseOrgans;
  return 0;
}