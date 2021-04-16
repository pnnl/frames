//---------------------------------------------------------------------------
#include "EPFv2.h"
//---------------------------------------------------------------------------

EPF_MAP_FILE EPFfiles;
STR_MAP epfMedia;

void SetMedia(long pid, int numpt, GLYPH_PTR_MAP_ITR ig, char *dicname, char *dstype, char *locname, char *medtype)
{
  Glyph *tg = (*ig).second;
  STR_VEC_MAP_ITR it = tg->osetlist.find(dicname);
  if(it == tg->osetlist.end())
    it = tg->isetlist.find(dicname);
      if(it == tg->isetlist.end()) return;
  STR_VEC sv = (*it).second;
  // what to do for multiples, for now just using the first one
  for(int j = 0; j<sv.size(); j++)
  {
    rstrcpy(MediaSet,(char*)sv[j].c_str());
    int ErrorCode = GetString(pid, MediaSet, FEATURE, blank, SetIdx(1), dummy);
    if (!rstrcmpi(AIR, medtype))
      if (numpt != 1)
        sprintf(Media, "%s.%s.%s.%s", AIR, dstype, locname, medtype);
      else
        // need to look up point index based on coordinates
        // right now code works but uses wrong coordinates for air
        // unless it happens to be the first location
        sprintf(Media, "%s.%s.%s.%s", dummy, dstype, locname, medtype);
    else
      sprintf(Media, "%s.%s.%s.%s", dummy, dstype, locname, medtype);
    break;
  }
}

char *GetBaseName(char *exprte, char *expunit)
{
  char rte[SMALLSTRING];
  char unit[SMALLSTRING];

  rstrcpy(unit, expunit);  strlwr(unit);
  rstrcpy(rte, exprte);    strlwr(rte);
  rstrcpy(dummy, blank);

  if(!rstrcmpi(rte, "Inhalation"))
  {
    rstrcpy(dummy, "Inhale");
    strcat(dummy, "Gas");
  }
  else
  {
    if(!rstrcmpi(rte, "ingestion"))        rstrcpy(dummy, "Ingest");
    else if(!rstrcmpi(rte, "dermal"))      rstrcpy(dummy, "Dermal");
    else if(!rstrcmpi(rte, "external"))    rstrcpy(dummy, "External");
    else if(!rstrcmpi(rte, "dose rate"))   rstrcpy(dummy, "BiotaDoseRate");

    if(rstrstr(unit, "/l"))                strcat(dummy, "Liquid");
    else if(rstrstr(unit, "kg"))           strcat(dummy, "Solid");
    else if(rstrstr(unit, "m^3"))          strcat(dummy, "Gas");
  }
  if(!rstrstr(expunit, "Sv") && !rstrstr(unit, "rad/day"))
    strcat(dummy, CONC);
  return dummy;
}

EPFRoute_v2 *EPFFileFind(char *_prefix, char *_rte, char *_unit)
{
  char name[SMALLSTRING];
  rstrcpy(name, _prefix, GetBaseName(_rte, _unit));
  EPF_MAP_FILE_ITR it = EPFfiles.find(name);
  if(it == EPFfiles.end()) return NULL;
  return(*it).second;
}

void EPFPathwayVecClear()
{
  for(EPF_MAP_FILE_ITR it = EPFfiles.begin(); it!= EPFfiles.end(); ++it)
  {
    EPFRoute_v2 *epf = (*it).second;
    EPF_VEC_PATH *PathwayVec = epf->PathwayVec;
    for(EPF_VEC_PATH_ITR iv = PathwayVec->begin(); iv!= PathwayVec->end(); ++iv)
    {
      EPFPathway_v2 *pw = *iv;
      TrackDelete(pw);
    }
    PathwayVec->clear();
    epf->vecMedia.clear();
    epf->vecMediaSet.clear();
    TrackDelete(PathwayVec);
    TrackDelete(epf);
  }
  EPFfiles.clear();
}

void EPFInitializePathwayMap(STR_VEC_MAP setlist, int fileIdx)
{
  int i, ct, np;
  int ErrorCode;
  int nummed = 0;
  int numpath = 0;
  int idx[MMF_MAX_DIM];
  char Media[SMALLSTRING];
  char MediaSet[SMALLSTRING];
  char dicname[SMALLSTRING];
  bool found;

  found = false;
  EPFRoute_v2 *ef = NULL;
  EPFPathway_v2 *pw = NULL;
  EPF_VEC_PATH *v = NULL;

  Log("EPFInitializePathwayMap");
  EPFPathwayVecClear();

  for(i = 0; i<MMF_MAX_DIM; i++) idx[i] = 0;
  ErrorCode = VarLookUp(pid, iniset, FILETYPE, _EPF, idx);
  ErrorCode = GetVarDimSize(pid, iniset, DICLIST, idx, &ct);
  if (fileIdx > ct) return;
  ct = 0;
  idx[1] = fileIdx;
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
        found = true;
        STR_VEC v = (*it).second;
        for(int iv = 0; iv<v.size(); iv++)
        {
          rstrcpy(setname, v[iv].c_str());
          ef = new EPFRoute_v2(dicname, setname); TrackLocation(ef);
          if(!EPFfiles[ef->pwdic])
          {
            nummed = 0;
            EPFfiles[ef->pwdic] = ef;
            ErrorCode = GetVarDimSize(pid, setname, MEDIA, SetIdx(), &nummed);
            for(int nm = 1; nm<= nummed; nm++)
            {
              ErrorCode = GetString(pid, setname, MEDIA, blank, SetIdx(nm), Media);
              ErrorCode = GetString(pid, setname, MEDIASET, blank, SetIdx(nm), MediaSet);

              ef->vecMedia.push_back((string)Media);
              ef->vecMediaSet.push_back((string)MediaSet);
              epfMedia[MediaSet] = string(Media);

              numpath = 0;
              ErrorCode = GetVarDimSize(pid, setname, PATHWAY, SetIdx(nm), &numpath);
              for(np = 1; np<= numpath; np++)
              {
                pw = new EPFPathway_v2(pid, setname, Media, nm, np); TrackLocation(pw);
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
  // Move to next qualifyer if no matches
  if (!found) EPFInitializePathwayMap(setlist, fileIdx+1);
}

EPFPathway_v2::EPFPathway_v2(long pid, char *setname, char *Media, int nmi, int npi)
{
  hit = false;
  mIdx = nmi;
  pIdx = npi;
  rstrcpy(media, Media);
  int ErrorCode = GetString(pid, setname, PATHWAY, blank, SetIdx(nmi, npi), path);
}

EPFPathway_v2::EPFPathway_v2(long pid, char *setname, char *Media, int nmi, char *Path)
{
  int ErrorCode;
  int idx[MMF_MAX_DIM];

  hit = false;
  mIdx = nmi;
  rstrcpy(path, Path);
  rstrcpy(media, Media);

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

void EPFPathway_v2::WriteDataset(long pid, icsv *inf, char *setname, char *varname, char *unit, int nc, int nt, int numpt)
{
  double conc;
  int ErrorCode;
  int idx[MMF_MAX_DIM];

  idx[0] = mIdx;
  idx[1] = nc;
  idx[2] = pIdx;
  idx[3] = nt;
  for(int n = 1; n<= numpt; n++)
  {
    *inf >> conc;
    idx[4] = n;
    idx[5] = 0;
    ErrorCode = PutFloat(pid, setname, varname, unit, idx, conc);
  }
  if(!inf->eof())
    *inf >> NewLn;
}

EPFRoute_v2::EPFRoute_v2(char *dicname, char *setname)
{
  hit = false;
  rstrcpy(pwdic, dicname);
  rstrcpy(pwset, setname);

  if(rstrstr(pwdic, "Inhale"))         rstrcpy(rte, "Inhalation");
  else if(rstrstr(pwdic, "Ingest"))    rstrcpy(rte, "Ingestion");
  else if(rstrstr(pwdic, "Dermal"))    rstrcpy(rte, "Dermal");
  else if(rstrstr(pwdic, "External"))  rstrcpy(rte, "External");
  else if(rstrstr(pwdic, "Biota"))     rstrcpy(rte, "Dose Rate");

  if(rstrstr(pwdic, CHEM))
  {
    if(rstrstr(pwdic, LIQUID))       rstrcpy(unit, "mg/L");
    else if(rstrstr(pwdic, SOLID))   rstrcpy(unit, "mg/kg");
    else if(rstrstr(pwdic, GAS))     rstrcpy(unit, "mg/m^3");
  }
  else
  {
    if(rstrstr(pwdic, LIQUID))        rstrcpy(unit, "Bq/L");
    else if(rstrstr(pwdic, SOLID))    rstrcpy(unit, "Bq/kg");
    else if(rstrstr(pwdic, GAS))      rstrcpy(unit, "Bq/m^3");
    else if(rstrstr(pwdic, "External")) rstrcpy(unit, "Sv");
    else if(rstrstr(pwdic, "DoseRate")) rstrcpy(unit, "rad/day");
  }

  if(rstrstr(dicname, CHEM))    rstrcpy(pwvar, dicname+4);
  else                          rstrcpy(pwvar, dicname+3);
  PathwayVec = new EPF_VEC_PATH; TrackLocation(PathwayVec);
}

void EPFRoute_v2::WriteDataset(long pid, icsv *inf, int nc, int nt, int numpt, char *path)
{
  int mIdx;
  int idx[MMF_MAX_DIM];

  EPFPathway_v2 *pw = NULL;
  for(EPF_VEC_PATH_ITR iv = PathwayVec->begin(); iv!= PathwayVec->end(); ++iv)
  {
    EPFPathway_v2 *t = *iv;
    if(!rstrcmpi(t->path, path) && !rstrcmpi(t->media, Media))
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
    pw = new EPFPathway_v2(pid, pwset, Media, mIdx, path); TrackLocation(pw);
    PathwayVec->push_back(pw);
  }
  pw->WriteDataset(pid, inf, pwset, pwvar, unit, nc, nt, numpt);
  pw->hit = true;
}

int EPFCon_v2::WriteDataset(long pid, icsv *inf, int nm, int numpt, Glyph *g, STR_VEC_PTR_VEC *conlist)
{
  int i, nc, nt;
  int numpath;
  int idx[MMF_MAX_DIM];
  char setprefix[8];
  char exppath[SMALLSTRING];
  char exprte[SMALLSTRING];
  char expunit[SMALLSTRING];
  char dunit[SMALLSTRING];
  char tunit[SMALLSTRING];

  EPFRoute_v2 *ef;
  EPFPathway_v2 *pw;
  EPF_VEC_PATH_ITR ip;
  EPF_MAP_FILE_ITR it;

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
  for(it = EPFfiles.begin(); it!= EPFfiles.end(); ++it)
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
  float *durations = new float[ntimes];
  for(nt = 1; nt<= ntimes; nt++)
  {
    *inf >> times[nt-1] >> tunit >> durations[nt-1] >> dunit >> numpath >> NewLn;
    for(i = 1; i<= numpath; i++)
    {
      *inf >> exppath >> exprte >> expunit >> NewLn;
      ef = EPFFileFind(setprefix, exprte, expunit);
      if(ef)
      {
        ef->hit = true;
        ef->WriteDataset(pid, inf, nc, nt, numpt, exppath);
      }
      else
      {
        sprintf(dummy, "_EPF dataset not found for %s path:%s route:%s unit:%s", casid, exppath, exprte, expunit); Log(dummy);
        *inf >> NewLn;
      }
    }
  }

  for(it = EPFfiles.begin(); it!= EPFfiles.end(); ++it)
  {
    ef = (*it).second;
    if(ef->hit)
    {
      for(ip = ef->PathwayVec->begin(); ip!= ef->PathwayVec->end(); ++ip)
      {
        pw = *ip;
        if(pw->hit)
        {
          for(nt = 1; nt<= ntimes; nt++)
          {
            ErrorCode = PutFloat(pid, ef->pwset, TIMEPTS, YEAR, SetIdx(pw->mIdx, nc, nt), times[nt-1]);
            ErrorCode = PutFloat(pid, ef->pwset, concat(ef->pwvar, DUR), YEAR, SetIdx(pw->mIdx), durations[nt-1]);
          }
          pw->hit = false;
        }
      }
      ef->hit = false;
    }
  }
  delete[] times;
  delete[] durations;
  return 0;
}

int EPFSet_v2::WriteDataset(long pid, icsv *inf, int nm, Glyph *g, STR_VEC_PTR_VEC *conlist)
{
  int ErrorCode;
  char dicname[SMALLSTRING];

  *inf >> dstype >> locname >> medtype >> numpt >> numcon >> NewLn;

  rstrcpy(Media, blank);
  rstrcpy(MediaSet, blank);

  if(!rstrcmpi(SURFACE_WATER, medtype))
    rstrcpy(dicname,SURFACEWATER,PTS);
  else if(!rstrcmpi(SOIL, medtype) || !rstrcmpi(SEDIMENT, medtype))
    rstrcpy(dicname,medtype,VOLUMES);
  else
    rstrcpy(dicname,medtype,PTS);

  // set media set name
  for(GLYPH_PTR_MAP_ITR ig = g->sources.begin(); ig!= g->sources.end(); ++ig)
  {
    SetMedia(pid,numpt,ig,dicname,dstype,locname,medtype);
    if(strlen(MediaSet)) break;
  }

  if(!strlen(MediaSet))
  {
    GetSetName(dicname, setname, g->ID2x, g->osetlist);
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

  for(int nc = 1; nc<= numcon; nc++)
  {
    EPFCon_v2 *p = new EPFCon_v2();  TrackLocation(p);
    p->WriteDataset(pid, inf, nm, numpt, g, conlist);
    TrackDelete(p);
  }
  return numpt;
}

int EPF_v2::WriteDataset(long pid, char *fuiname, Glyph *g, STR_VEC_PTR_VEC *conlist)
{
  int i;
  int numds;
  int numInfo;

  Log("EPF_v2::WriteDataset");
  // this will create an empty map that will be filled
  // by EPF_v2::WriteDataset as pathways are encountered
  EPFInitializePathwayMap(g->osetlist,1);

  rstrcpy(dummy, fuiname, _DOT, _EPF);
  inf = new icsv(dummy, '\"'); TrackLocation(inf);
  if(inf->ok())
  {
    *inf >> numInfo >> NewLn;
    for(i = 0; i< numInfo; i++)
      *inf >> dummy >> NewLn;

    *inf >> numds >> NewLn;
    for(i = 0; i< numds; i++)
    {
      EPFSet_v2 *p = new EPFSet_v2(); TrackLocation(p);
      p->WriteDataset(pid, inf, i+1, g, conlist);
      TrackDelete(p);
    }
  }
  TrackDelete(inf);
  EPFPathwayVecClear();
  epfMedia.clear();
  return 0;
}

//======================================================================================
int EPFCon_v2::WriteFile(long pid, ocsv *fp, int nm, int numpt, Glyph *g, STR_VEC *conlist, char *ID)
{
  int i, nc, nt, nval, nrec = 0;
  int ErrorCode;
  double ftime, fdur, fval;
  char setprefix[8];
  int idx[MMF_MAX_DIM] = {0};
  char cas[SMALLSTRING];
  char name[SMALLSTRING];

  EPF_MAP_FILE_ITR it;
  EPF_VEC_PATH_ITR ip;
  EPFRoute_v2 *eftime = NULL;
  EPFPathway_v2 *pw = NULL;
  EPFPathway_v2 *pwtime = NULL;

  STR_VEC_ITR iv = conlist->begin();
  rstrcpy(setprefix, iv->c_str());
  ++iv;
  rstrcpy(cas, iv->c_str());

  GetSetName(concat(setprefix, LIST), setname, blank, g->isetlist);
  ErrorCode = VarLookUp(pid, setname, CASID, cas, idx);
  nc = idx[0];
  ErrorCode = GetString(pid, setname, NAME, blank, idx, name);

  eftime = NULL;
  for(it = EPFfiles.begin(); it!= EPFfiles.end(); ++it)
  {
    EPFRoute_v2 *ef = (*it).second;
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

  ErrorCode = GetFloat(pid, eftime->pwset, concat(eftime->pwvar, DUR), YEAR, SetIdx(pwtime->mIdx), &fdur);
  for(nt = 1; nt<= ntimes; nt++)
  {
    ErrorCode = GetFloat(pid, eftime->pwset, TIMEPTS, YEAR, SetIdx(pwtime->mIdx, nc, nt), &ftime);
    int numpath = 0;
    for(it = EPFfiles.begin(); it!= EPFfiles.end(); ++it)
    {
      EPFRoute_v2 *ef = (*it).second;
      int x = ef->PathwayVec->size();
      if(rstrstr(ef->pwdic, setprefix) && (x>0))
      {
        for(ip = ef->PathwayVec->begin(); ip!= ef->PathwayVec->end(); ++ip)
        {
          pw = (*ip);
          if(!rstrcmpi(pw->media, Media))
          {
            // for each pathway, count number of timepts, if >0 numpath++
            nval = 0;
            ErrorCode = GetVarDimSize(pid, ef->pwset, ef->pwvar, SetIdx(pw->mIdx, nc, pw->pIdx), &nval);
            if(nval>0)
            {
              ef->hit = true;
              pw->hit = true;
              numpath ++;
            }
          }
        }
      }
    }

    *fp << ftime << YEAR << fdur << YEAR << numpath << NewLn; nrec++;

    if(numpath)
      for(it = EPFfiles.begin(); it!= EPFfiles.end(); ++it)
      {
        EPFRoute_v2 *ef = (*it).second;
        if(ef->hit)
        {
          for(ip = ef->PathwayVec->begin(); ip!= ef->PathwayVec->end(); ++ip)
          {
            pw = (*ip);
            if(pw->hit)
            {
              *fp << pw->path << ef->rte << ef->unit << NewLn; nrec++;
              for(i = 1; i<= numpt; i++)
              {
                ErrorCode = GetFloat(pid, ef->pwset, ef->pwvar, ef->unit, SetIdx(pw->mIdx, nc, pw->pIdx, nt, i), &fval);
                *fp << fval;
              }
              *fp << NewLn; nrec++;
              pw->hit = false;
              ++iv;
            }
          }
          ef->hit = false;
        }
      }
  }
  return nrec;
}

int EPFSet_v2::WriteFile(long pid, ocsv *fp, int nm, Glyph *g, STR_VEC_PTR_VEC *conlist, char *ID)
{
  int nrec = 0;
  int ErrorCode;
  int featureIdx;
  int idx[MMF_MAX_DIM];

  CStringParser args;
  args.Parse(Media, CParseOptions('.'));
  rstrcpy(epfname, args.GetAt(0)); // location name
  rstrcpy(dstype, args.GetAt(1));  // acute or chronic
  rstrcpy(locname, args.GetAt(2)); // file extension
  rstrcpy(medtype, args.GetAt(3)); // file qualifier .aka. media

  sprintf(dummy, "Writing _EPF from datasets for %s, %s, %s", dstype, locname, medtype); Log(dummy);

  for(int i = 0; i<MMF_MAX_DIM; i++) idx[i] = 0;

  // air has multiple location in a set
  // this is not consistant with other
  // transport files (scf,wff,wcf,aff)
  numpt = 0;
  if (!rstrcmpi(medtype,AIR) && !rstrcmpi(epfname,AIR))  // check for single air point must be one or all
  {
    // feature are points for air
    ErrorCode = GetVarDimSize(pid, MediaSet, FEATURE, SetIdx(), &numpt);
  }
  else
  {
    // could be volume or point but always just one per set
    ErrorCode = VarLookUp(pid, MediaSet, FEATURE, epfname, idx);
    featureIdx = idx[0];
  }

  numcon = conlist->size();
  *fp << dstype << locname << medtype;

  if(numpt)
  {
    *fp << numpt << numcon << NewLn; nrec++;
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
    *fp << numpt << numcon << NewLn; nrec++;
    *fp << g->PDCFglyph->x << KM << g->PDCFglyph->y << KM << NewLn;  nrec++;
  }

  if(numcon == 0) return nrec;

  for(int nc = 1; nc<= numcon; nc++)
  {
    EPFCon_v2 *p = new EPFCon_v2();  TrackLocation(p);
    nrec += p->WriteFile(pid, fp, nm, numpt, g,(*conlist)[nc-1], ID);
    TrackDelete(p);
  }
  numcon = 0;
  return nrec;
}

int EPF_v2::WriteFile(long pid, char *outpath, Glyph *g, STR_VEC_PTR_VEC *conlist, char *ID, char *ID2x)
{
  int i;
  int numds;
  long nrec = 0;
  long posbeg = 0;
  long posend = 0;

  Log("EPF_v2::WriteFile");
  EPFInitializePathwayMap(g->isetlist,1);
  numds = epfMedia.size();

  // check whether to create _EPF or append existing _EPF
  if(0 == access(AddExtension(outpath, _EPF), 0))
  { outf = new ocsv(AddExtension(outpath, _EPF), '"', ',', _APPEND_); TrackLocation(outf); }
  else
  { outf = new ocsv(AddExtension(outpath, _EPF), '"', ',', _CREATE_); TrackLocation(outf); }
  if(!outf->ok()) return 0;

  posbeg = outf->getpos();
  outf->smartQuote();
  *outf << ID << "         " << NewLn;
  outf->alwaysQuote();
  *outf << 1 << NewLn; nrec++;
  *outf << "_EPF generated by 1X Wrapper in Frames V2" << NewLn; nrec++;
  *outf << numds << NewLn; nrec++;

  i = 0;
  for(STR_MAP_ITR is = epfMedia.begin(); is!= epfMedia.end(); ++is)
  {
    i++;
    rstrcpy(Media,(*is).second.c_str());
    rstrcpy(MediaSet,(*is).first.c_str());
    EPFSet_v2* p = new EPFSet_v2(); TrackLocation(p);
    nrec += p->WriteFile(pid, outf, i, g, conlist, ID2x);
    TrackDelete(p);
  }
  outf->setpos(posbeg);
  *outf << ID;
  fprintf(outf->fptr,"%ld",nrec);
  TrackDelete(outf);
  EPFPathwayVecClear();
  epfMedia.clear();
  return 0;
}