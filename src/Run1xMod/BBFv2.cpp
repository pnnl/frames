#include "BBFv2.h"

Series_v2 *BBFInitializeNames(char *contype, STR_VEC_MAP setlist, char *ftype)
{
  Series_v2 *flux = NULL;

  rstrcpy(dummy, contype, MEDIUM, "Emission");
  GetSetName(dummy, setname, 1, setlist);
  if(!rstrcmpi(CHEM, contype))
    flux = new Series_v2(TIMEPTS, YEAR, ftype, GYR);
  else
    flux = new Series_v2(TIMEPTS, YEAR, ftype, PCIYR);
  return flux;
}

int BBFCon_v2::WriteDataset(long pid, icsv *inf, int nm, int no, Glyph *g, int numLvl,  STR_VEC_PTR_VEC *conlist)
{
  int i;
  int j;
  int ErrorCode;
  int idx[MMF_MAX_DIM];
  double year;
  double conc;
  char setprefix[8];
  STR_VEC_ITR iv;

  Log("BBFCon_v2::WriteDataset");
  *inf >> name >> cas >> tunit >> vunit >> numStep >> numProg >> NewLn;

  for(STR_VEC_PTR_VEC_ITR ic = conlist->begin(); ic!= conlist->end(); ++ic)
  {
    STR_VEC_ITR iv = (*ic)->begin();
    rstrcpy(setprefix, iv->c_str());
    ++iv;
    rstrcpy(dummy, iv->c_str());
    if(0 == rstrcmpi(cas, dummy)) break;
  }
  for(i = 0; i<MMF_MAX_DIM; i++) idx[i] = 0;
  GetSetName(concat(setprefix, LIST), setname, blank, g->isetlist);
  ErrorCode = VarLookUp(pid, setname, CASID, cas, idx);

  if(ErrorCode == 0)
  {
    rstrcpy(dummy, setprefix, "BurdenWet");
    GetSetName(dummy, setname, g->ID2x, g->osetlist);
    CheckUnit(pid, setname, TIMEPTS, tunit);

    idx[2] = no;
    idx[1] = idx[0];
    idx[0] = nm;
    for(i = 1; i<=numStep; i++)
    {
      *inf >> year;
      idx[4] = 0;
      idx[3] = i;
      ErrorCode = PutFloat(pid , setname, TIMEPTS, tunit, idx, year);
      idx[4] = i;
      for(j = 1; j=<numLvl; j++)
      {
        *inf >> conc;
        idx[3] = j;
        ErrorCode = PutFloat(pid, setname, "BodyBurden", vunit, idx, conc);
      }
      *inf >> NewLn;
    }
  }
  return 0;
}

int BBFCon_v2::WriteFile(long pid, ocsv *fp, int nm, Glyph *g, STR_VEC *conlist, int n, char *ID)
{
  int i;
  int nstep;
  int nflux;
  int nrec;
  int ErrorCode;
  char setprefix[8];
  int idx[MMF_MAX_DIM];
  int vidx[MMF_MAX_DIM];
  Series_v2 **flux;
  Series_v2 *tflux;

  Log("BBFCon_v2::WriteFile");
  nrec = 0;
  numProg = 0;
  STR_VEC_ITR iv = conlist->begin();
  rstrcpy(setprefix, iv->c_str());
  ++iv;
  rstrcpy(cas, iv->c_str());

  for(i = 0; i<MMF_MAX_DIM; i++) idx[i] = vidx[0] = 0;
  GetSetName(concat(setprefix, LIST), setname, blank, g->isetlist);
  ErrorCode = VarLookUp(pid, setname, CASID, cas, idx);
  ErrorCode = GetString(pid, setname, NAME, blank, idx, name);
  if(ErrorCode == 0)
  {
    *fp << name << cas;
    *fp << YR;
    *fp << MGKG;
    *fp << numStep;
    *fp << numProg;
    *fp << NewLn;
    nrec++;

    for(nstep = 0; nstep<numStep; nstep++)
    {
      nflux = 0;
      for(i = 0; i<ngas+nsol+nliq; i++)
      {
        if(nflux == 0) *fp << flux[i]->xValues[nstep];
        *fp << flux[i]->yValues[nstep];
        nflux++;
      }
      *fp << NewLn;
      nrec++;
    }
    for(i = 0; i<(ngas+nsol+nliq); i++)
    {
      tflux = flux[i];
      TrackDelete(tflux);
    }
    TrackDelete(flux);
  }
  else
  {
    *fp << name << cas << YEAR << MGKG << 0 << 0 <<NewLn;
    nrec++;
  }
  return nrec;
}

int BBFSet_v2::WriteDataset(long pid, icsv *inf, int nm, Glyph *g, STR_VEC_PTR_VEC *conlist)
{
  int i;
  int j;
  int ErrorCode;
  double dumFloat;
  double dumFloat2;
  char level[SMALLSTRING];
  char name[SMALLSTRING];
  char type[SMALLSTRING];
  char organism[SMALLSTRING];
  int numOrg;
  int numCon;
  int numVLvl;
  int numULvl;
  STR_VEC v;
  STR_VEC u;

  Log("BBFSet_v2::WriteDataset");
  *inf >> name >> type >> numOrg >> numVLvl >> numULvl >> NewLn;

  GetSetName("ChemBurdenWet", setname, g->ID2x, g->osetlist);

  for(i = 0; i<numVLvl; i++)
  {
    *inf >> level;
    v.push_back(string(level));
  }
  *inf >> NewLn;

  for(i = 0; i<numVLvl; i++)
  {
    *inf >> level;
    u.push_back(string(level));
  }
  *inf >> NewLn;

  j=0;
  for(STR_VEC_ITR iv=v.begin(); iv!=v.end(); ++iv)
    for(STR_VEC_ITR iu=u.begin(); iu!=u.end(); ++iu)
    {
      j++;
      ErrorCode = PutString(pid, setname, "VULevels", blank, SetIdx(j), concat((*iv).c_str(), (*iu).c_str());
    }

  for(i = 0; i<numOrg; i++)
  {
    *inf >> organism >> numCon >> NewLn;
    for(i = 0; i<MMF_MAX_DIM; i++) idx[i] = 0;
    GetSetName("AquaticOrganism", setname, ID, g->isetlist);
    ErrorCode = VarLookUp(pid, setname, "ScientificName", orgism, idx);

    for(i = 0; i<numCon; i++)
    {
      BBFOrg_v2 *p = new BBFOrg_v2(); TrackLocation(p);
      p->WriteDataset(pid, inf, nm, no, g, j, conlist);
      TrackDelete(p);
    }
  }
  return 0;
}

int BBFSet_v2::WriteFile(long pid, ocsv *fp, int nm, Glyph *g, STR_VEC_PTR_VEC *conlist, char *ID)
{
  int i;
  int nc;
  int nrec;
  int numOrg;
  int ErrorCode;
  char organism[SMALLSTRING];

  Log("BBFSet_v2::WriteFile");
  nrec = 0;
  numCon = conlist->size();
  numOrg = 0;
  GetSetName("AquaticOrganism", setname, ID, g->isetlist);
  ErrorCode = GetVarDimSize(pid, setname, "ScientificName", SetIdx(), &numOrg);

  *fp << _WCF << SURFACE_WATER << numOrg << numVLvl << numULvl << NewLn;
  nrec++;

  *fp << VLvl << NewLn;
  *fp << ULvl << NewLn;
  nrec+=2;

  if(numOrg>0)
    for(no = 1; no<=numOrg; no++)
    {
      GetSetName("AquaticOrganism", setname, ID, g->isetlist);
      ErrorCode = GetString(pid, setname, "ScientificName", blank, SetIdx(no), organism);
      *fp << organism << numCon << NewLn;
      nrec++;
      if(numCon>0)
        for(nc = 0; nc<numCon; nc++)
        {
          BBFCon_v2 *p = new BBFCon_v2();  TrackLocation(p);
          nrec += p->WriteFile(pid, fp, nm, no, g, (*conlist)[nc]);
          TrackDelete(p);
        }
    }

  numCon = 0;
  return nrec;
}

int BBF_v2::WriteDataset(long pid, char *fuiname, Glyph *g, STR_VEC_PTR_VEC *conlist)
{
  int i;
  int numInfo;

  Log("BBF_v2::WriteDataset");
  MEDIUM = SURFACEWATER;
  rstrcpy(dummy, fuiname, _DOT, _BBF);
  inf = new icsv(dummy, '\"');  TrackLocation(inf);
  if(inf->ok())
  {
    *inf >> numInfo >> NewLn;
    for(i = 0; i<numInfo; i++)
      *inf >> dummy >> NewLn;

    *inf >> numSet >> NewLn;
    for(i = 0; i<numSet; i++)
    {
      BBFSet_v2 *p = new BBFSet_v2(); TrackLocation(p);
      p->WriteDataset(pid, inf, i+1, g, conlist);
      TrackDelete(p);
    }
  }
  TrackDelete(inf);
  return 0;
}

int BBF_v2::WriteFile(long pid, char *outpath, Glyph *g, STR_VEC_PTR_VEC *conlist, char *fqual, char *ID)
{
  int i, ErrorCode;
  long nrec = 0, posbeg = 0, posend = 0;

  Log("BBF_v2::WriteFile");
  MEDIUM = SURFACEWATER;

  // check whether to create BBF or append existing BBF
  if(0 == access(AddExtension(outpath, "bbf"), 0))
    outf = new ocsv(AddExtension(outpath, "bbf"), '"', ',', _APPEND_);
  else
    outf = new ocsv(AddExtension(outpath, "bbf"), '"', ',', _CREATE_);
  if(!outf->ok()) return 0;

  posbeg = outf->getpos();
  outf->smartQuote();
  *outf << ID << "         " << NewLn;
  outf->alwaysQuote();

  numSet = 0;
  GetSetName(concat(MEDIUM, POLYS), setname, ID, g->isetlist);
  ErrorCode = GetVarDimSize(pid, setname, FEATURE, SetIdx(), &numSet);

  *outf << 1 << NewLn; nrec++;
  *outf << "BBF generated by 1X Wrapper in Frames V2" << NewLn; nrec++;
  *outf << numSet << NewLn; nrec++;
  for(i = 0; i<numSet; i++)
  {
    BBFSet_v2* p = new BBFSet_v2();  TrackLocation(p);
    nrec += p->WriteFile(pid, outf, i+1, g, conlist);
    TrackDelete(p);
  }

  outf->setpos(posbeg);
  *outf << ID << nrec ;
  delete outf;
  outf = NULL;
  return 0;
}