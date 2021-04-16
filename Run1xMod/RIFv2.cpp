//---------------------------------------------------------------------------
#include "RIFv2.h"
//---------------------------------------------------------------------------

RIF_MAP_FILE RIFfiles;
STR_MAP rifMedia;

char *RIFGetBaseName(char *exprte, char *expunit)
{
  char unit[SMALLSTRING];
  char rte[SMALLSTRING];

  rstrcpy(unit, expunit);   strlwr(unit);
  rstrcpy(rte, exprte);     strlwr(rte);
  rstrcpy(dummy, blank);

  if(!rstrcmpi(unit, "Sv"))                       rstrcpy(dummy, "ExternalDose");
  else if(rstrstr(unit, "Bq"))
  {
    if(!rstrcmpi(unit, "Bq"))
    {
      if(!rstrcmpi(exprte, "ingestion"))          rstrcpy(dummy, "IngestIntake");
      else if(!rstrcmpi(exprte, "dermal"))        rstrcpy(dummy, "DermalIntake");
      else if(!rstrcmpi(exprte, "inhalation"))    rstrcpy(dummy, "InhaleIntake");
    }
    else if(!rstrcmpi(unit, "Bq/kg"))             rstrcpy(dummy, "ExternalSoilConc");
    else if(!rstrcmpi(unit, "Bq/m^3"))            rstrcpy(dummy, "ExternalAirConc");
    else if(!rstrcmpi(unit, "Bq/L"))              rstrcpy(dummy, "ExternalWaterConc");
  }
  else if(rstrstr(unit, "mg"))
  {
    if(!rstrcmpi(unit, "mg/m^3"))                 rstrcpy(dummy, "InhaleConc");
    else
    {
      if(!rstrcmpi(exprte, "ingestion"))          rstrcpy(dummy, "IngestIntake");
      else if(!rstrcmpi(exprte, "dermal"))        rstrcpy(dummy, "DermalIntake");
      else if(!rstrcmpi(exprte, "inhalation"))    rstrcpy(dummy, "InhaleIntake");
    }
  }
  return dummy;
}

RIFRoute_v2 *RIFFileFind(char *_prefix, char *_rte, char *_unit)
{
  char name[SMALLSTRING];
  rstrcpy(name, _prefix, RIFGetBaseName(_rte, _unit));
  RIF_MAP_FILE_ITR it = RIFfiles.find(name);
  if(it == RIFfiles.end()) return NULL;
  return(*it).second;
}

void RIFPathwayVecClear()
{
  for(RIF_MAP_FILE_ITR it = RIFfiles.begin(); it!= RIFfiles.end(); ++it)
  {
    RIFRoute_v2 *rif = (*it).second;
    RIF_VEC_PATH *v = rif->PathwayVec;
    for(RIF_VEC_PATH_ITR iv = v->begin(); iv!= v->end(); ++iv)
    {
      RIFPathway_v2 *pw = *iv;
      TrackDelete(pw);
    }
    v->clear();
    rif->vecMedia.clear();
    rif->vecMediaSet.clear();
    TrackDelete(v);
    TrackDelete(rif);
  }
  RIFfiles.clear();
}

void RIFInitializePathwayMap(STR_VEC_MAP setlist)
{
  int i,ct,np;
  int ErrorCode;
  int nummed = 0;
  int numpath = 0;
  int idx[MMF_MAX_DIM];

  char Xtype[SMALLSTRING];
  char Media[SMALLSTRING];
  char MediaSet[SMALLSTRING];
  char dicname[SMALLSTRING];

  RIFRoute_v2 *ef = NULL;
  RIFPathway_v2 *pw = NULL;
  RIF_VEC_PATH *v = NULL;

  Log("RIFInitializePathwayMap");
  RIFPathwayVecClear();

  for(i = 0; i<MMF_MAX_DIM; i++) idx[i] = 0;
  ErrorCode = VarLookUp(pid, iniset, FILETYPE, _RIF, idx);
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
        STR_VEC v = (*it).second;
        for(int iv = 0; iv<v.size(); iv++)
        {
          rstrcpy(setname, v[iv].c_str());
          ef = new RIFRoute_v2(dicname, setname); TrackLocation(ef);
          if(!RIFfiles[ef->pwdic])
          {
            RIFfiles[ef->pwdic] = ef;
            if (rstrstr(ef->pwdic,INTAKE))
              rstrcpy(Xtype,"intake");
            else if (rstrstr(ef->pwdic,CONC))
              rstrcpy(Xtype,"concentration");
            else if (rstrstr(ef->pwdic,DOSE))
              rstrcpy(Xtype,"radiation dose");
            else
              rstrcpy(Xtype,blank);
            nummed = 0;
            ErrorCode = GetVarDimSize(pid, setname, MEDIA, SetIdx(), &nummed);
            for(int nm = 1; nm<= nummed; nm++)
            {
              ErrorCode = GetString(pid, setname, MEDIA, blank, SetIdx(nm), Media);
              ErrorCode = GetString(pid, setname, MEDIASET, blank, SetIdx(nm), MediaSet);

              ef->vecMedia.push_back((string)Media);
              ef->vecMediaSet.push_back((string)MediaSet);
              rifMedia[MediaSet] = string(Media);
              numpath = 0;
              ErrorCode = GetVarDimSize(pid, setname, PATHWAY, SetIdx(nm), &numpath);
              for(np = 1; np<= numpath; np++)
              {
                pw = new RIFPathway_v2(pid, setname, Media, nm, np, Xtype); TrackLocation(pw);
                ef->PathwayVec->push_back(pw);
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

RIFPathway_v2::RIFPathway_v2(long pid, char *setname, char *Media, int nmi, int npi, char *Xtype)
{
  hit = false;
  carc = false;
  nonc = false;
  mIdx = nmi;
  pIdx = npi;
  rstrcpy(media, Media);
  rstrcpy(xtype, Xtype);
  int ErrorCode = GetString(pid, setname, PATHWAY, blank, SetIdx(nmi, npi), path);
}

RIFPathway_v2::RIFPathway_v2(long pid, char *setname, char *Media, int nmi, char *Path, char *Xtype)
{
  int ErrorCode;
  int idx[MMF_MAX_DIM];

  carc = false;
  nonc = false;
  hit = false;
  mIdx = nmi;
  rstrcpy(path, Path);
  rstrcpy(media, Media);
  rstrcpy(xtype, Xtype);

  pIdx=0;
  for(int i = 0; i<MMF_MAX_DIM; i++) idx[i] = 0;
  idx[0] = mIdx;
  ErrorCode = VarLookUp(pid, setname, PATHWAY, path, idx);
  pIdx = idx[1];
  if(pIdx == 0)
  {
    ErrorCode = GetVarDimSize(pid, setname, PATHWAY, SetIdx(mIdx), &pIdx);
    pIdx++;
    ErrorCode = PutString(pid, setname, PATHWAY, blank, SetIdx(mIdx, pIdx), path);
  }
}

void RIFPathway_v2::WriteDataset(long pid, icsv *inf, char *setname, char *varname, char *unit, int na, int nc, int nt, int numpt, char *Xtype)
{
  float conc;
  int ErrorCode;
  int idx[MMF_MAX_DIM];
  char vname[SMALLSTRING];

  rstrcpy(vname,varname);
  if(rstrstr(setname, CHEM))
  {
    if(!rstrcmpi(Xtype, "carcinogenic")) strcat(vname, "Carc");
    if(!rstrcmpi(Xtype, "noncarcinogenic")) strcat(vname, "NonC");
  }

  idx[0] = mIdx;
  idx[1] = nc;
  idx[2] = na;
  idx[3] = pIdx;
  idx[4] = nt;
  for(int n = 1; n<= numpt; n++)
  {
    *inf >> conc;
    idx[5] = n;
    idx[6] = 0;
    ErrorCode = PutFloat(pid, setname, vname, unit, idx, conc);
  }
  if(!inf->eof())
    *inf >> NewLn;
}

RIFRoute_v2::RIFRoute_v2(char *dicname, char *setname)
{
  hit = false;
  rstrcpy(pwdic, dicname);
  rstrcpy(pwset, setname);

  if(rstrstr(pwdic, "Inhale"))         rstrcpy(rte, "Inhalation");
  else if(rstrstr(pwdic, "Ingest"))    rstrcpy(rte, "Ingestion");
  else if(rstrstr(pwdic, "Dermal"))    rstrcpy(rte, "Dermal");
  else if(rstrstr(pwdic, "External"))  rstrcpy(rte, "External");

  if(rstrstr(pwdic, CONC))
  {
    if(rstrstr(pwdic, CHEM))         rstrcpy(unit, "mg/m^3");
    else
    {
      if(rstrstr(pwdic, "Water"))    rstrcpy(unit, "Bq/L");
      else if(rstrstr(pwdic, SOIL))  rstrcpy(unit, "Bq/kg");
      else if(rstrstr(pwdic, AIR))   rstrcpy(unit, "Bq/m^3");
    }
  }
  else if(rstrstr(pwdic, "Intake"))
  {
    if(rstrstr(pwdic, CHEM))         rstrcpy(unit, "mg/kg/day");
    else                            rstrcpy(unit, "Bq");
  }
  else if(rstrstr(pwdic, DOSE))    rstrcpy(unit, "Sv");


  if(rstrstr(dicname, CHEM))    rstrcpy(pwvar, dicname+4);
  else                         rstrcpy(pwvar, dicname+3);

  PathwayVec = new RIF_VEC_PATH; TrackLocation(PathwayVec);
}

void RIFRoute_v2::WriteDataset(long pid, icsv *inf, int na, int nc, int nt, int numpt, char *Path, char *Xtype)
{
  int mIdx;
  int idx[MMF_MAX_DIM];

  RIFPathway_v2 *pw = NULL;
  for(RIF_VEC_PATH_ITR iv = PathwayVec->begin(); iv!= PathwayVec->end(); ++iv)
  {
    RIFPathway_v2 *t = *iv;
    if(!rstrcmpi(t->path, Path) && !rstrcmpi(t->media, Media) && !rstrcmpi(t->xtype, Xtype))
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
    pw = new RIFPathway_v2(pid, pwset, Media, mIdx, Path, Xtype); TrackLocation(pw);
    PathwayVec->push_back(pw);
  }
  pw->WriteDataset(pid, inf, pwset, pwvar, unit, na, nc, nt, numpt, Xtype);
  pw->hit = true;
}

int RIFCon_v2::WriteDataset(long pid, icsv *inf, int nm, int na, int numpt, Glyph *g, STR_VEC_PTR_VEC *conlist)
{
  int i, nc, nt;
  int numpath;
  int idx[MMF_MAX_DIM];
  char setprefix[8];
  char exppath[SMALLSTRING];
  char exprte[SMALLSTRING];
  char expunit[SMALLSTRING];
  char exptype[SMALLSTRING];
  char tunit[SMALLSTRING];
  char dunit[SMALLSTRING];

  RIFRoute_v2 *ef;
  RIFPathway_v2 *pw;
  RIF_VEC_PATH_ITR ip;
  RIF_MAP_FILE_ITR it;

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
  int ErrorCode = VarLookUp(pid, setname, CASID, casid, idx);
  nc = idx[0];

  //clear hits
  pw = NULL;
  for(it = RIFfiles.begin(); it!= RIFfiles.end(); ++it)
  {
    ef = (*it).second;
    ef->hit = false;
    for(ip = ef->PathwayVec->begin(); ip!= ef->PathwayVec->end(); ++ip)
    {
      pw = *ip;
      pw->hit = false;
    }
  }

  float *times = new float[ntimes];
  for(nt = 1; nt<= ntimes; nt++)
  {
    *inf >> times[nt-1] >> tunit >> duration >> dunit >> numpath >> NewLn;
    for(i = 1; i<= numpath; i++)
    {
      *inf >> population >> exppath >> exprte >> expunit >> exptype >> NewLn;
      ef = RIFFileFind(setprefix, exprte, expunit);
      if(ef)
      {
        ef->hit = true;
        ef->WriteDataset(pid, inf, na, nc, nt, numpt, exppath, exptype);
      }
      else
      {
        sprintf(dummy, "_RIF dataset not found for %s path:%s route:%s unit:%s", casid, exppath, exprte, expunit); Log(dummy);
        *inf >> NewLn;
      }
    }
  }
  for(it = RIFfiles.begin(); it!= RIFfiles.end(); ++it)
  {
    ef = (*it).second;
    if(ef->hit)
    {
      for(ip = ef->PathwayVec->begin(); ip!= ef->PathwayVec->end(); ++ip)
      {
        pw = (*ip);
        if(pw->hit)
        {
          for(nt = 1; nt<= ntimes; nt++)
            ErrorCode = PutFloat(pid, ef->pwset, TIMEPTS, YEAR, SetIdx(pw->mIdx, nc, nt), times[nt-1]);
          pw->hit = false;
        }
      }
      ef->hit = false;
    }
  }
  delete[] times;
  return 0;
}

int RIFSet_v2::WriteDataset(long pid, icsv *inf, int nm, Glyph *g, STR_VEC_PTR_VEC *conlist)
{
  int na;
  int ErrorCode;
  int idx[MMF_MAX_DIM];
  char dicname[SMALLSTRING];

  *inf >> dstype >> locname >> medtype >> numpt >> numage >> numcon >> NewLn;

  rstrcpy(Media, blank);
  rstrcpy(MediaSet, blank);
  if(!rstrcmpi(SURFACE_WATER, medtype))
    rstrcpy(dicname,SURFACEWATER,PTS);
  else if(!rstrcmpi(SOIL, medtype) || !rstrcmpi(SEDIMENT, medtype))
    rstrcpy(dicname,medtype,VOLUMES);
  else
    rstrcpy(dicname,medtype,PTS);

  // set media set name from sources
  for(GLYPH_PTR_MAP_ITR ig = g->sources.begin(); ig!= g->sources.end(); ++ig)
  {
    SetMedia(pid,numpt,ig,dicname,dstype,locname,medtype);
    if(strlen(MediaSet)) break;
  }

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
  for(int i = 0; i<MMF_MAX_DIM; i++) idx[i] = 0;
  na = 0;
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
      RIFCon_v2 *p = new RIFCon_v2(); TrackLocation(p);
      p->WriteDataset(pid, inf, nm, na, numpt, g, conlist);
      TrackDelete(p);
    }
    ErrorCode = PutFloat(pid, setAgeGroup, DURATION, YEAR, SetIdx(na), duration);
    ErrorCode = PutString(pid, setAgeGroup, POPFEATURE, blank, SetIdx(na), g->ID1x);
    ErrorCode = PutInteger(pid, setAgeGroup, POPULATION, blank, SetIdx(na), population);
  }
  return numpt;
}

int RIF_v2::WriteDataset(long pid, char *fuiname, Glyph *g, STR_VEC_PTR_VEC *conlist)
{
  int i;
  int numds;
  int numInfo;

  Log("_RIF::WriteDataset");
  // this will create an empty map that will be filled
  // by _HIF datasets as pathways are encountered (why?)
  RIFInitializePathwayMap(g->osetlist);

  // if these are blank then must create them when writing datasets
  GetSetName(AGEGROUP, setAgeGroup, g->ID2x, g->osetlist);
  GetSetName(concat(POPULATION,POLYS), setPopulation, g->ID2x, g->osetlist);

  rstrcpy(dummy, fuiname, _DOT, _RIF);
  inf = new icsv(dummy, '\"'); TrackLocation(inf);
  if(inf->ok())
  {
    *inf >> numInfo >> NewLn;
    for(i = 0; i< numInfo; i++)
      *inf >> dummy >> NewLn;

    *inf >> numds >> NewLn;
    for(i = 0; i< numds; i++)
    {
      RIFSet_v2 *ds = new RIFSet_v2(); TrackLocation(ds);
      ds->WriteDataset(pid, inf, i+1,  g, conlist);
      TrackDelete(ds);
    }
  }
  TrackDelete(inf);
  RIFPathwayVecClear();
  rifMedia.clear();
  return 0;
}

//======================================================================================
int RIFCon_v2::WriteFile(long pid, ocsv *fp, int nm, int na, int numpt, Glyph *g, STR_VEC *conlist, char *ID)
{
  int i, nc, nt, nval, nrec = 0;
  int ErrorCode;
  double ftime, fval;
  char setprefix[8];
  int idx[MMF_MAX_DIM] = {0};
  char cas[SMALLSTRING];
  char name[SMALLSTRING];

  RIF_MAP_FILE_ITR it;
  RIF_VEC_PATH_ITR ip;
  RIFRoute_v2 *eftime = NULL;
  RIFPathway_v2 *pw = NULL;
  RIFPathway_v2 *pwtime = NULL;

  STR_VEC_ITR iv = conlist->begin();
  rstrcpy(setprefix, iv->c_str());
  ++iv;
  rstrcpy(cas, iv->c_str());

  GetSetName(concat(setprefix, LIST), setname, blank, g->isetlist);
  ErrorCode = VarLookUp(pid, setname, CASID, cas, idx);
  nc = idx[0];
  ErrorCode = GetString(pid, setname, NAME, blank, idx, name);

  eftime = NULL;
  for(it = RIFfiles.begin(); it!= RIFfiles.end(); ++it)
  {
    RIFRoute_v2 *ef = (*it).second;
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
    ErrorCode = GetFloat(pid, eftime->pwset, TIMEPTS, YEAR, SetIdx(pw->mIdx, nc, nt), &ftime);
    // count paths, including carcinogenic and noncarcinogenic
    int numpath = 0;
    for(it = RIFfiles.begin(); it!= RIFfiles.end(); ++it)
    {
      RIFRoute_v2 *ef = (*it).second;
      if(rstrstr(ef->pwdic, setprefix))
      {
        for(ip = ef->PathwayVec->begin(); ip!= ef->PathwayVec->end(); ++ip)
        {
          pw = (*ip);
          if(!rstrcmpi(pw->media, Media))
          {
            nval = 0;
            ErrorCode = GetVarDimSize(pid, ef->pwset, TIMEPTS, SetIdx(pw->mIdx), &nval);
            if(ErrorCode == 0 && nval>= nc)
            {
              if(0 == rstrcmpi(setprefix, CHEM))
              {
                pw->carc = false;
                pw->nonc = false;
                // for each pathway, count number of timepts, if >0 numpath++
                nval = 0;
                ErrorCode = GetVarDimSize(pid, ef->pwset, concat(ef->pwvar, "Carc"), SetIdx(pw->mIdx, nc, na, pw->pIdx), &nval);
                if(ErrorCode == 0)     pw->carc = (nval>0);
                nval = 0;
                ErrorCode = GetVarDimSize(pid, ef->pwset, concat(ef->pwvar, "NonC"), SetIdx(pw->mIdx, nc, na, pw->pIdx), &nval);
                if(ErrorCode == 0)     pw->nonc = (nval>0);
                nval = 0;
                if(pw->carc) nval++;
                if(pw->nonc) nval++;
              }
              else
              {
                nval = 0;
                ErrorCode = GetVarDimSize(pid, ef->pwset, ef->pwvar, SetIdx(pw->mIdx, nc, na, pw->pIdx), &nval);
                if(ErrorCode == 0 && nval>0)
                  nval = 1;
              }
              if(nval>0)
              {
                ef->hit = true;
                pw->hit = true;
                numpath += nval;
              }
            }
          }
        }
      }
    }

    *fp << ftime << YEAR << duration << YEAR << numpath << NewLn; nrec++;

    if(numpath)
      for(it = RIFfiles.begin(); it!= RIFfiles.end(); ++it)
      {
        RIFRoute_v2 *ef = (*it).second;
        if(ef->hit)
        {
          for(ip = ef->PathwayVec->begin(); ip!= ef->PathwayVec->end(); ++ip)
          {
            pw = (*ip);
            if(pw->hit)
            {
              if(0 == strcmpi(setprefix, CHEM))
              {
                if(pw->carc)
                {
                  *fp << 1 << pw->path << ef->rte << ef->unit << "carcinogenic" << NewLn; nrec++;
                  for(i = 1; i<= numpt; i++)
                  {
                    ErrorCode = GetFloat(pid, ef->pwset, concat(ef->pwvar, "Carc"), ef->unit, SetIdx(pw->mIdx, nc, na, pw->pIdx, nt, i), &fval);
                    *fp << fval;
                  }
                  *fp << NewLn; nrec++;
                }
                if(pw->nonc)
                {
                  *fp << 1 << pw->path << ef->rte << ef->unit << "noncarcinogenic" << NewLn; nrec++;
                  for(i = 1; i<= numpt; i++)
                  {
                    ErrorCode = GetFloat(pid, ef->pwset, concat(ef->pwvar, "NonC"), ef->unit, SetIdx(pw->mIdx, nc, na, pw->pIdx, nt, i), &fval);
                    *fp << fval;
                  }
                  *fp << NewLn; nrec++;
                }
              }
              else
              {
                *fp << 1 << pw->path << ef->rte << ef->unit << pw->xtype << NewLn; nrec++;
                for(i = 1; i<= numpt; i++)
                {
                  ErrorCode = GetFloat(pid, ef->pwset, ef->pwvar, ef->unit, SetIdx(pw->mIdx, nc, na, pw->pIdx, nt, i), &fval);
                  *fp << fval;
                }
                *fp << NewLn; nrec++;
              }
              pw->hit = false;
              pw->carc = false;
              pw->nonc = false;
            }
          }
          ef->hit = false;
        }
      }
  }
  return nrec;
}

int RIFSet_v2::WriteFile(long pid, ocsv *fp, int nm, Glyph *g, STR_VEC_PTR_VEC *conlist, char *ID)
{
  int nrec = 0;
  int ErrorCode;
  int featureIdx;
  int idx[MMF_MAX_DIM];

  CStringParser args;
  args.Parse(Media, CParseOptions('.'));
  rstrcpy(rifname, args.GetAt(0)); // location name
  rstrcpy(dstype, args.GetAt(1));  // acute or chronic
  rstrcpy(locname, args.GetAt(2)); // file extension
  rstrcpy(medtype, args.GetAt(3)); // file qualifier .aka. media

  sprintf(dummy, "Writing _RIF from datasets for %s, %s, %s", dstype, locname, medtype); Log(dummy);

  for(int i = 0; i<MMF_MAX_DIM; i++) idx[i] = 0;

  // air has multiple location in a set
  // this is not consistant with other
  // transport files (scf,wff,wcf,aff)
  numpt = 0;
  if (!rstrcmpi(medtype,AIR) && !rstrcmpi(rifname,AIR))  // check for single air point must be one or all
  {
    // feature are points for air
    ErrorCode = GetVarDimSize(pid, MediaSet, FEATURE, SetIdx(), &numpt);
  }
  else
  {
    // could be volume or point but always just one per set
    ErrorCode = VarLookUp(pid, MediaSet, FEATURE, rifname, idx);
    featureIdx = idx[0];
  }
  numage = 0;
  ErrorCode = GetVarDimSize(pid, setAgeGroup, NAME, SetIdx(), &numage);
  numcon = conlist->size();
  *fp << dstype << locname << medtype;

  if(numpt)
  {
    *fp << numpt << numage << numcon << NewLn; nrec++;
    for(int np = 1; np<= numpt; np++)
    {
      ErrorCode = GetFloat(pid, MediaSet, FEATUREPTS, KM, SetIdx(np, 1, 1), &x);
      ErrorCode = GetFloat(pid, MediaSet, FEATUREPTS, KM, SetIdx(np, 2, 1), &y);
//      ErrorCode = GetFloat(pid, MediaSet, FEATUREPTS, KM, SetIdx(np, 3, 1), &z);
      *fp << x << KM << y << KM << NewLn;  nrec++;
    }
  }
  else
  {
    numpt=1;
    *fp << numpt << numage << numcon << NewLn; nrec++;
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

    *fp << agemin << agemax << YEAR << NewLn;  nrec++;

    for(int nc = 1; nc<= numcon; nc++)
    {
      RIFCon_v2 *p = new RIFCon_v2();  TrackLocation(p);
      nrec += p->WriteFile(pid, fp, nm, na, numpt, g,(*conlist)[nc-1], ID);
      TrackDelete(p);
    }
  }
  numcon = 0;
  return nrec;
}

int RIF_v2::WriteFile(long pid, char *outpath, Glyph *g, STR_VEC_PTR_VEC *conlist, char *ID, char *ID2x)
{
  int i;
  int numds;
  long nrec = 0;
  long posbeg = 0;
  long posend = 0;

  Log("_RIF::WriteFile");
  GetSetName(AGEGROUP, setAgeGroup, ID2x, g->isetlist);
  GetSetName(concat(POPULATION,POLYS), setPopulation, ID2x, g->isetlist);
  RIFInitializePathwayMap(g->isetlist);
  numds = rifMedia.size();

  // check whether to create rif or append existing rif
  if(0 == access(AddExtension(outpath, _RIF), 0))
  { outf = new ocsv(AddExtension(outpath, _RIF), '"', ',', _APPEND_); TrackLocation(outf); }
  else
  { outf = new ocsv(AddExtension(outpath, _RIF), '"', ',', _CREATE_); TrackLocation(outf); }
  if(!outf->ok()) return 0;

  posbeg = outf->getpos();
  outf->smartQuote();
  *outf << ID << "         " << NewLn;
  outf->alwaysQuote();
  *outf << 1 << NewLn; nrec++;
  *outf << "_RIF generated by 1X Wrapper in Frames V2" << NewLn; nrec++;
  *outf << numds << NewLn; nrec++;

  i = 0;
  for(STR_MAP_ITR is = rifMedia.begin(); is!= rifMedia.end(); ++is)
  {
    i++;
    rstrcpy(Media,(*is).second.c_str());
    rstrcpy(MediaSet,(*is).first.c_str());
    RIFSet_v2 *p = new RIFSet_v2(); TrackLocation(p);
    nrec += p->WriteFile(pid, outf, i, g, conlist, ID2x);
    TrackDelete(p);
  }
  outf->setpos(posbeg);
  *outf << ID;
  fprintf(outf->fptr,"%ld",nrec);
  TrackDelete(outf);
  RIFPathwayVecClear();
  rifMedia.clear();
  return 0;
}