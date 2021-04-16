#include "AFFv2.h"

Series_v2 *AFFInitializeNames(char *contype, STR_VEC_MAP setlist, char *ftype, char *ID)
{
  Series_v2 *flux = NULL;

  rstrcpy(dummy, contype, MEDIUM, "Emission");
  GetSetName(dummy, setname, ID, setlist);
  if(!rstrcmpi(CHEM, contype))
  {  flux = new Series_v2(TIMEPTS, YEAR, ftype, GYR, cmFF);  TrackLocation(flux);  }
  else
  {  flux = new Series_v2(TIMEPTS, YEAR, ftype, PCIYR, rmFF);  TrackLocation(flux);  }
  return flux;
}

int AFFCon_v2::WriteDataset(long pid, icsv *inf, int nm, Glyph *g, int numFluxTypes,  STR_VEC_PTR_VEC *conlist, STR_VEC *vars)
{
  int i;
  int j;
  int ErrorCode;
  int idx[MMF_MAX_DIM];
  double year;
  double flux;
  char setprefix[8];
  char varname[SMALLSTRING];
  STR_VEC_ITR iv;

  Log("AFFCon_v2::WriteDataset");
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
    rstrcpy(dummy, setprefix, AIR, "Emission");
    GetSetName(dummy, setname, g->ID2x, g->osetlist);
    CheckUnit(pid, setname, TIMEPTS, tunit);
    CheckUnit(pid, setname, GASEMISSION, vunit);
    CheckUnit(pid, setname, SOLEMISSION, vunit);
    CheckUnit(pid, setname, LIQEMISSION, vunit);

    idx[1] = idx[0]; idx[0] = nm;
    for(i = 0; i<numStep; i++)
    {
      *inf >> year;
      for(j = 0; j<numFluxTypes; j++)
      {
        idx[2] = i+1;
        idx[3] = 0;
        ErrorCode = PutFloat(pid , setname, TIMEPTS, tunit, idx, year);
        if(j == 0) iv = vars->begin();
        else ++iv;
        idx[2] = atoi((*iv).c_str());
        ++iv;
        rstrcpy(varname,(*iv).c_str());
        idx[3] = i+1;
        *inf >> flux;
        ErrorCode = PutFloat(pid, setname, varname, vunit, idx, flux);
      }
      *inf >> NewLn;
    }
  }
  return 0;
}

int AFFCon_v2::WriteFile(long pid, ocsv *fp, int nm, Glyph *g, STR_VEC *conlist, int ngas, int nsol, int nliq, char *ID)
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

  Log("AFFCon_v2::WriteFile");
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
    nflux = 0;
    numStep = 0;
    idx[1] = vidx[1] = idx[0];
    idx[0] = vidx[0] = nm;
    idx[2] = 1;
    flux = new Series_v2 *[ngas+nsol+nliq]; TrackLocation(flux);
    for(i = 0; i<ngas; i++)
    {
      vidx[2] = i+1;
      flux[nflux] = AFFInitializeNames(setprefix, g->isetlist, GASEMISSION, ID);
      flux[nflux]->ReadDatasets(pid, setname, idx, 3, vidx, 4);
      if(numStep < flux[nflux]->count)
        numStep = flux[nflux]->count;
      tflux = flux[nflux];
      TrackLocation(tflux);
      nflux++;
    }
    idx[2] = 1;
    for(i = 0; i<nsol; i++)
    {
      vidx[2] = i+1;
      flux[nflux] = AFFInitializeNames(setprefix, g->isetlist, SOLEMISSION, ID);
      tflux = flux[nflux];
      TrackLocation(tflux);
      flux[nflux]->ReadDatasets(pid, setname, idx, 3, vidx,  4);
      if(numStep < flux[nflux]->count)
        numStep = flux[nflux]->count;
      tflux = flux[nflux];
      TrackLocation(tflux);
      nflux++;
    }
    for(i = 0; i<nliq; i++)
    {
      vidx[2] = i+1;
      flux[nflux] = AFFInitializeNames(setprefix, g->isetlist, LIQEMISSION, ID);
      flux[nflux]->ReadDatasets(pid, setname, idx, 3, vidx, 4);
      if(numStep < flux[nflux]->count)
        numStep = flux[nflux]->count;
      tflux = flux[nflux];
      TrackLocation(tflux);
      nflux++;
    }

    *fp << name << cas;
    *fp << flux[0]->xUnits;       // yr
    *fp << flux[0]->yUnits;       // g/yr or pCi/yr
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
    *fp << name << cas << YEAR << blank << 0 << 0 <<NewLn;
    nrec++;
  }
  return nrec;
}

int AFFSet_v2::WriteDataset(long pid, icsv *inf, int nm, Glyph *g, STR_VEC_PTR_VEC *conlist)
{
  int i;
  int ErrorCode;
  double dumFloat;
  double dumFloat2;
  double len;
  char vunit[SMALLSTRING];
  char runit[SMALLSTRING];
  STR_VEC v;

  Log("AFFSet_v2::WriteDataset");
  *inf >> name >> NewLn; // name
  *inf >> type >> NewLn; // point or area
  *inf >> area >> NewLn; // exit are
  *inf >> extHt >> NewLn; // exit height
  *inf >> adjHt >> NewLn; // adj struct height
  *inf >> extVel >> NewLn; // exit velocity
  *inf >> extTmp >> NewLn; // exit temp
  *inf >> ambTmp >> NewLn; // amb air temp
  *inf >> numFluxTypes >> NewLn; // number of flux types

  GetSetName(concat(MEDIUM, POLYS), setname, g->ID2x, g->osetlist);
  ErrorCode = PutString(pid, setname, FEATURE, blank, SetIdx(nm), name);
  ErrorCode = PutString(pid, setname, ATTRIBUTEDES, blank, SetIdx(1), "Source Type");
  ErrorCode = PutString(pid, setname, ATTRIBUTEDES, blank, SetIdx(2), "Area (m^2)");
  ErrorCode = PutString(pid, setname, ATTRIBUTEDES, blank, SetIdx(3), "Exit Height (m)");
  ErrorCode = PutString(pid, setname, ATTRIBUTEDES, blank, SetIdx(4), "Adj. Building height (m)");
  ErrorCode = PutString(pid, setname, ATTRIBUTEDES, blank, SetIdx(5), "Exit velocity (m/s)");
  ErrorCode = PutString(pid, setname, ATTRIBUTEDES, blank, SetIdx(6), "Exit temp. (C)");
  ErrorCode = PutString(pid, setname, ATTRIBUTEDES, blank, SetIdx(7), "Ambient air temp. (C)");

  ErrorCode = PutString(pid, setname, ATTRIBUTES, blank, SetIdx(nm, 1), type);
  sprintf(dummy, "%20.12f", area);   ErrorCode = PutString(pid, setname, ATTRIBUTES, blank, SetIdx(nm, 2), dummy);
  sprintf(dummy, "%20.12f", extHt);  ErrorCode = PutString(pid, setname, ATTRIBUTES, blank, SetIdx(nm, 3), dummy);
  sprintf(dummy, "%20.12f", adjHt);  ErrorCode = PutString(pid, setname, ATTRIBUTES, blank, SetIdx(nm, 4), dummy);
  sprintf(dummy, "%20.12f", extVel); ErrorCode = PutString(pid, setname, ATTRIBUTES, blank, SetIdx(nm, 5), dummy);
  sprintf(dummy, "%20.12f", extTmp); ErrorCode = PutString(pid, setname, ATTRIBUTES, blank, SetIdx(nm, 6), dummy);
  sprintf(dummy, "%20.12f", ambTmp); ErrorCode = PutString(pid, setname, ATTRIBUTES, blank, SetIdx(nm, 7), dummy);

  // create polygon for flux
  if (area > 0)
    len = sqrt(area) / 2.0;
  else
    len = 0.5;
  ErrorCode = PutFloat(pid, setname, FEATUREPTS, M, SetIdx(nm,1,1), g->x-len);
  ErrorCode = PutFloat(pid, setname, FEATUREPTS, M, SetIdx(nm,1,2), g->x+len);
  ErrorCode = PutFloat(pid, setname, FEATUREPTS, M, SetIdx(nm,1,3), g->x+len);
  ErrorCode = PutFloat(pid, setname, FEATUREPTS, M, SetIdx(nm,1,4), g->x-len);
  ErrorCode = PutFloat(pid, setname, FEATUREPTS, M, SetIdx(nm,1,5), g->x-len);
  ErrorCode = PutFloat(pid, setname, FEATUREPTS, M, SetIdx(nm,2,1), g->y+len);
  ErrorCode = PutFloat(pid, setname, FEATUREPTS, M, SetIdx(nm,2,2), g->y+len);
  ErrorCode = PutFloat(pid, setname, FEATUREPTS, M, SetIdx(nm,2,3), g->y-len);
  ErrorCode = PutFloat(pid, setname, FEATUREPTS, M, SetIdx(nm,2,4), g->y-len);
  ErrorCode = PutFloat(pid, setname, FEATUREPTS, M, SetIdx(nm,2,5), g->y+len);
  ErrorCode = PutFloat(pid, setname, FEATUREPTS, M, SetIdx(nm,3,1), g->z);
  ErrorCode = PutFloat(pid, setname, FEATUREPTS, M, SetIdx(nm,3,2), g->z);
  ErrorCode = PutFloat(pid, setname, FEATUREPTS, M, SetIdx(nm,3,3), g->z);
  ErrorCode = PutFloat(pid, setname, FEATUREPTS, M, SetIdx(nm,3,4), g->z);
  ErrorCode = PutFloat(pid, setname, FEATUREPTS, M, SetIdx(nm,3,5), g->z);

  int ngas = 0;
  int nsol = 0;
  int nliq = 0;
  for(i = 0; i<numFluxTypes; i++)
  {
    *inf >> dummy >> dumFloat >> runit >> dumFloat2 >> vunit >> NewLn;
    if(rstrstr(dummy, GAS))
    {
      GetSetName("SuspendedGasesDef", setname, g->ID2x, g->osetlist);
      ngas++;
      ErrorCode = PutString(pid, setname, NAME, blank, SetIdx(ngas), dummy);
      ErrorCode = PutFloat(pid, setname, REACTIVE, runit, SetIdx(ngas), dumFloat);
      ErrorCode = PutFloat(pid, setname, DENSITY, vunit, SetIdx(ngas), dumFloat2);
      sprintf(dummy, "%d", ngas);       v.push_back(string(dummy));
      rstrcpy(dummy, GASEMISSION);      v.push_back(string(dummy));
    }
    else if(rstrstr(dummy, "Particle"))
    {
      GetSetName("SuspendedSolidsDef", setname, g->ID2x, g->osetlist);
      nsol++;
      ErrorCode = PutString(pid, setname, NAME, blank, SetIdx(nsol), dummy);
      ErrorCode = PutFloat(pid, setname, MIDSIZE, runit, SetIdx(nsol), dumFloat);
      ErrorCode = PutFloat(pid, setname, DENSITY, vunit, SetIdx(nsol), dumFloat2);
      sprintf(dummy, "%d", nsol);       v.push_back(string(dummy));
      rstrcpy(dummy, SOLEMISSION);      v.push_back(string(dummy));
    }
    else if(rstrstr(dummy, LIQUID))
    {
      GetSetName("SuspendedLiquidsDef", setname, g->ID2x, g->osetlist);
      nliq++;
      ErrorCode = PutString(pid, setname, NAME, blank, SetIdx(nliq), dummy);
      ErrorCode = PutFloat(pid, setname, MIDSIZE, vunit, SetIdx(nliq), dumFloat);
      sprintf(dummy, "%d", nsol);       v.push_back(string(dummy));
      rstrcpy(dummy, LIQEMISSION);      v.push_back(string(dummy));
    }
  }
  *inf >> numCon >> NewLn;

  for(i = 0; i<numCon; i++)
  {
    AFFCon_v2 *p = new AFFCon_v2(); TrackLocation(p);
    p->WriteDataset(pid, inf, nm, g, numFluxTypes, conlist, &v);
    TrackDelete(p);
  }
  return 0;
}

int AFFSet_v2::WriteFile(long pid, ocsv *fp, int nm, Glyph *g, STR_VEC_PTR_VEC *conlist, char *ID)
{
  int i;
  int nc;
  int nrec;
  int ErrorCode;
  double dumFloat;

  Log("AFFSet_v2::WriteFile");
  nrec = 0;
  numCon = conlist->size();
  GetSetName(concat(MEDIUM, POLYS), setname, ID, g->isetlist);
  ErrorCode = GetString(pid, setname, FEATURE, blank, SetIdx(nm), name);
  ErrorCode = GetString(pid, setname, ATTRIBUTES, blank, SetIdx(nm, 1), type);
  ErrorCode = GetString(pid, setname, ATTRIBUTES, blank, SetIdx(nm, 2), dummy); area = ratod(dummy);
  ErrorCode = GetString(pid, setname, ATTRIBUTES, blank, SetIdx(nm, 3), dummy); extHt = ratod(dummy);
  ErrorCode = GetString(pid, setname, ATTRIBUTES, blank, SetIdx(nm, 4), dummy); adjHt = ratod(dummy);
  ErrorCode = GetString(pid, setname, ATTRIBUTES, blank, SetIdx(nm, 5), dummy); extVel = ratod(dummy);
  ErrorCode = GetString(pid, setname, ATTRIBUTES, blank, SetIdx(nm, 6), dummy); extTmp = ratod(dummy);
  ErrorCode = GetString(pid, setname, ATTRIBUTES, blank, SetIdx(nm, 7), dummy); ambTmp = ratod(dummy);

  *fp << name << NewLn;
  *fp << type << NewLn;
  *fp << area << "m^2" << NewLn;
  *fp << extHt << M << NewLn;
  *fp << adjHt << M << NewLn;
  *fp << extVel << "m/sec" << NewLn;
  *fp << extTmp << "C" << NewLn;
  *fp << ambTmp << "C" << NewLn;
  nrec+= 8;

  int ngas = 0;
  int nsol = 0;
  int nliq = 0;
  GetSetName("SuspendedGasesDef", setname, ID, g->isetlist);
  ErrorCode = GetVarDimSize(pid, setname, NAME, SetIdx(), &ngas);
  GetSetName("SuspendedSolidsDef", setname, ID, g->isetlist);
  ErrorCode = GetVarDimSize(pid, setname, NAME, SetIdx(), &nsol);
  GetSetName("SuspendedLiquidsDef", setname, ID, g->isetlist);
  ErrorCode = GetVarDimSize(pid, setname, NAME, SetIdx(), &nliq);

  numFluxTypes = nliq+nsol+ngas;
  *fp << numFluxTypes << NewLn;
  nrec++;

  for(i = 0; i<ngas; i++)
  {
    GetSetName("SuspendedGasesDef", setname, ID, g->isetlist);
    ErrorCode = GetString(pid, setname, NAME, blank, SetIdx(i+1), dummy); *fp << dummy;
    ErrorCode = GetFloat(pid, setname, REACTIVE, FRACTION, SetIdx(i+1), &dumFloat);
    *fp << dumFloat << FRACTION;
    ErrorCode = GetFloat(pid, setname, DENSITY, GCM3, SetIdx(i+1), &dumFloat);
    *fp << dumFloat << GCM3 << NewLn;
    nrec++;
  }
  for(i = 0; i<nsol; i++)
  {
    GetSetName("SuspendedSolidsDef", setname, ID, g->isetlist);
    ErrorCode = GetString(pid, setname, NAME, blank, SetIdx(i+1), dummy); *fp << dummy;
    ErrorCode = GetFloat(pid, setname, MIDSIZE, UM, SetIdx(i+1), &dumFloat);
    *fp << dumFloat << UM;
    ErrorCode = GetFloat(pid, setname, DENSITY, GCM3, SetIdx(i+1), &dumFloat);
    *fp << dumFloat << GCM3 << NewLn;
    nrec++;
  }
  for(i = 0; i<nliq; i++)
  {
    GetSetName("SuspendedLiquidsDef", setname, ID, g->isetlist);
    ErrorCode = GetString(pid, setname, NAME, blank, SetIdx(i+1), dummy); *fp << dummy;
    ErrorCode = GetFloat(pid, setname, MIDSIZE, UM, SetIdx(i+1), &dumFloat);
    *fp << dumFloat << UM << NewLn;
    nrec++;
  }
  *fp << numCon << NewLn;
  nrec++;

  if(numCon>0)
    for(nc = 0; nc<numCon; nc++)
    {
      AFFCon_v2 *p = new AFFCon_v2();  TrackLocation(p);
      nrec += p->WriteFile(pid, fp, nm, g, (*conlist)[nc], ngas, nsol, nliq, ID);
      TrackDelete(p);
    }

  numCon = 0;
  return nrec;
}

int AFF_v2::WriteDataset(long pid, char *fuiname, Glyph *g, STR_VEC_PTR_VEC *conlist)
{
  int i;
  int numInfo;

  Log("AFF_v2::WriteDataset");
  MEDIUM = AIR;
  rstrcpy(dummy, fuiname, _DOT, _AFF);
  inf = new icsv(dummy, '\"');  TrackLocation(inf);
  if(inf->ok())
  {
    *inf >> numInfo >> NewLn;
    for(i = 0; i<numInfo; i++)
      *inf >> dummy >> NewLn;

    *inf >> numSet >> NewLn;
    for(i = 0; i<numSet; i++)
    {
      AFFSet_v2 *p = new AFFSet_v2(); TrackLocation(p);
      p->WriteDataset(pid, inf, i+1, g, conlist);
      TrackDelete(p);
    }
  }
  TrackDelete(inf);
  return 0;
}

int AFF_v2::WriteFile(long pid, char *outpath, Glyph *g, STR_VEC_PTR_VEC *conlist, char *fqual, char *ID, char *ID2x)
{
  int i, ErrorCode;
  long nrec = 0, posbeg = 0, posend = 0;

  Log("AFF_v2::WriteFile");
  MEDIUM = AIR;

  // check whether to create _AFF or append existing _AFF
  if(0 == access(AddExtension(outpath, _AFF), 0))
    outf = new ocsv(AddExtension(outpath, _AFF), '"', ',', _APPEND_);
  else
    outf = new ocsv(AddExtension(outpath, _AFF), '"', ',', _CREATE_);
  if(!outf->ok()) return 0;

  posbeg = outf->getpos();
  outf->smartQuote();
  *outf << ID << "         " << NewLn;
  outf->alwaysQuote();

  numSet = 0;
  GetSetName(concat(MEDIUM, POLYS), setname, ID2x, g->isetlist);
  ErrorCode = GetVarDimSize(pid, setname, FEATURE, SetIdx(), &numSet);

  *outf << 1 << NewLn; nrec++;
  *outf << "_AFF generated by 1X Wrapper in Frames V2" << NewLn; nrec++;
  *outf << numSet << NewLn; nrec++;
  for(i = 0; i<numSet; i++)
  {
    AFFSet_v2* p = new AFFSet_v2();  TrackLocation(p);
    nrec += p->WriteFile(pid, outf, i+1, g, conlist, ID2x);
    TrackDelete(p);
  }

  outf->setpos(posbeg);
  *outf << ID;
  fprintf(outf->fptr,"%ld",nrec);
  delete outf;
  outf = NULL;
  return 0;
}