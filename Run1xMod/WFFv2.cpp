#include "WFFv2.h"

Series_v2 *WFFInitializeNames(char *contype, STR_VEC_MAP isetlist, char *ftype, char *ID)
{
  Series_v2 *flux = NULL;

  sprintf(dummy, "%s%s%s", contype, MEDIUM, ftype);
  GetSetName(dummy, setname, ID, isetlist);
  if(!rstrcmpi(CHEM, contype))
     flux = new Series_v2(TIMEPTS, YEAR, ftype, GYR, cmFF);
  else
    flux = new Series_v2(TIMEPTS, YEAR, ftype, PCIYR, rmFF);
  return flux;
}

int WFFCon_v2::WriteDataset(long pid, icsv *inf, int nm, Glyph *g, STR_VEC_PTR_VEC *conlist)
{
  int i;
  int ErrorCode;
  int idx[MMF_MAX_DIM];
  double year;
  double flux;
  char setprefix[8];
  char setAdsorbed[SMALLSTRING];
  char setDissolved[SMALLSTRING];
  char setTotal[SMALLSTRING];

  Log("WFFCon_v2::WriteDataset");
  *inf >> name >> cas >> tunit >> vunit >> numStep >> numXYSeries >> numProg >> NewLn;

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
    if(numXYSeries == 2)
    {
      rstrcpy(dummy, setprefix, MEDIUM);
      GetSetName(concat(dummy, ADSORBEDFLUX), setAdsorbed, g->ID2x , g->osetlist);
      CheckUnit(pid, setAdsorbed, TIMEPTS, tunit);
      CheckUnit(pid, setAdsorbed, ADSORBEDFLUX, vunit);
      rstrcpy(dummy, setprefix, MEDIUM);
      GetSetName(concat(dummy, DISSOLVEDFLUX), setDissolved, g->ID2x, g->osetlist);
      CheckUnit(pid, setDissolved, TIMEPTS, tunit);
      CheckUnit(pid, setDissolved, DISSOLVEDFLUX, vunit);
    }
    else
    {
      rstrcpy(dummy, setprefix, MEDIUM);
      GetSetName(concat(dummy, TOTALFLUX), setTotal, g->ID2x, g->osetlist);
      CheckUnit(pid, setTotal, TIMEPTS, tunit);
      CheckUnit(pid, setTotal, TOTALFLUX, vunit);
    }

    idx[1] = idx[0]; idx[0] = nm;
    for(i = 0; i<numStep; i++)
    {
      idx[2] = i+1;
      *inf >> year >> flux;
      if(numXYSeries == 2)
      {
        ErrorCode = PutFloat(pid, setAdsorbed, TIMEPTS, tunit, idx, year);
        ErrorCode = PutFloat(pid, setAdsorbed, ADSORBEDFLUX, vunit, idx, flux);
        *inf >> flux;
        ErrorCode = PutFloat(pid, setDissolved, TIMEPTS, tunit, idx, year);
        ErrorCode = PutFloat(pid, setDissolved, DISSOLVEDFLUX, vunit, idx, flux);
      }
      else
      {
        ErrorCode = PutFloat(pid, setTotal, TIMEPTS, tunit, idx, year);
        ErrorCode = PutFloat(pid, setTotal, TOTALFLUX, vunit, idx, flux);
      }
      *inf >> NewLn;
    }
  }
  return 0;
}

int WFFCon_v2::WriteFile(long pid, ocsv *fp, int nm, Glyph *g, STR_VEC *conlist, char *ID)
{
  int i;
  int nf;
  int nrec;
  int ErrorCode;
  char setprefix[8];
  int idx[MMF_MAX_DIM];

  Log("WFFCon_v2::WriteFile");
  nrec = 0;
  numProg = 0;
  numXYSeries = 1;
  if(MEDIUM == SURFACEWATER) numXYSeries = 2;
  STR_VEC_ITR iv = conlist->begin();
  rstrcpy(setprefix, iv->c_str());
  ++iv;
  rstrcpy(cas, iv->c_str());

  for(i = 0; i<MMF_MAX_DIM; i++) idx[i] = 0;
  GetSetName(concat(setprefix, LIST), setname, blank, g->isetlist);
  ErrorCode = VarLookUp(pid, setname, CASID, cas, idx);
  ErrorCode = GetString(pid, setname, NAME, blank, idx, name);
  if(ErrorCode == 0)
  {
    idx[1] = idx[0]; idx[0] = nm;
    if(numXYSeries == 2)
    {
      flux1 = WFFInitializeNames(setprefix, g->isetlist, ADSORBEDFLUX, ID); TrackLocation(flux1);
      flux1->ReadDatasets(pid, setname, idx, 3);
      flux2 = WFFInitializeNames(setprefix, g->isetlist, DISSOLVEDFLUX,ID); TrackLocation(flux2);
      flux2->ReadDatasets(pid, setname, idx, 3);
    }
    else
    {
      flux1 = WFFInitializeNames(setprefix, g->isetlist, TOTALFLUX, ID); TrackLocation(flux1);
      flux1->ReadDatasets(pid, setname, idx, 3);
    }

    *fp << name << cas;
    *fp << flux1->xUnits; // yr
    *fp << flux1->yUnits;     // g/yr or pCi/yr
    *fp << flux1->count;
    *fp << numXYSeries;
    *fp << numProg;
    *fp << NewLn;
    nrec+= 1;

    for(nf = 0; nf<flux1->count; nf++)
    {
      *fp << flux1->xValues[nf] << flux1->yValues[nf];
      if(numXYSeries == 2)
        *fp << flux2->yValues[nf];
      *fp << NewLn;
      nrec+= 1;
    }
    TrackDelete(flux1);
    if(numXYSeries == 2)
      TrackDelete(flux2);
  }
  else
  {
    *fp << name << cas << YEAR << blank << 0 << 0 << 0 <<NewLn;
    nrec++;
  }
  return nrec;
}

int WFFSet_v2::WriteDataset(long pid, icsv *inf, int nm, Glyph *g, STR_VEC_PTR_VEC *conlist)
{
  int nc;
  int nf;
  int ErrorCode;
  char tunit[SMALLSTRING];
  char vunit[SMALLSTRING];
  double year;
  double flux;
  double len;

  Log("WFFSet_v2::WriteDataset");
  *inf >> name >> type;       // name ~ Transmitting medium type
  *inf >> width >> dummy;     // width, units(m) of flux pane
  *inf >> length >> dummy;    // length/height, units(m) of flux pane
  *inf >> distance >> dummy;  // distance, units(m) from water table
  *inf >> recharge >> dummy;  // natural recharge rate, units(m/yr)
  *inf >> numCon >> NewLn;
  *inf >> tunit >> vunit >> numFlux >> NewLn;

  MEDIUM = NULL;
  if(!rstrcmpi(VADOSE, type))         MEDIUM = VADOSE;
  if(!rstrcmpi(AQUIFER, type))        MEDIUM = AQUIFER;
  if(!rstrcmpi(SURFACE_WATER, type))  MEDIUM = SURFACEWATER;
  if(MEDIUM == NULL) return 0;

  GetSetName(concat(MEDIUM, POLYS), setname, g->ID2x, g->osetlist);
  ErrorCode = PutString(pid, setname, FEATURE, blank, SetIdx(nm), name);
  ErrorCode = PutString(pid, setname, ATTRIBUTEDES, blank, SetIdx(1), "Type of medium");
  ErrorCode = PutString(pid, setname, ATTRIBUTEDES, blank, SetIdx(2), "Width of flux plane (m)");
  ErrorCode = PutString(pid, setname, ATTRIBUTEDES, blank, SetIdx(3), "Length of flux plane (m)");
  ErrorCode = PutString(pid, setname, ATTRIBUTEDES, blank, SetIdx(4), "Distance below water table");

  ErrorCode = PutString(pid, setname, ATTRIBUTES, blank, SetIdx(nm, 1), type);
  sprintf(dummy, "%20.12f", width);     ErrorCode = PutString(pid, setname, ATTRIBUTES, blank, SetIdx(nm, 2), dummy);
  sprintf(dummy, "%20.12f", length);    ErrorCode = PutString(pid, setname, ATTRIBUTES, blank, SetIdx(nm, 3), dummy);
  sprintf(dummy, "%20.12f", distance);  ErrorCode = PutString(pid, setname, ATTRIBUTES, blank, SetIdx(nm, 4), dummy);

  // create polygon for flux 
  // although the polygon WIDTH is assumed to be oriented perpendicular to the
  // flow direction we will assume for now flow is oriented along the Y axis
  len = width/2.0;
  ErrorCode = PutFloat(pid, setname, FEATUREPTS, M, SetIdx(nm,1,1), g->x-len);
  ErrorCode = PutFloat(pid, setname, FEATUREPTS, M, SetIdx(nm,1,2), g->x+len);
  ErrorCode = PutFloat(pid, setname, FEATUREPTS, M, SetIdx(nm,1,3), g->x+len);
  ErrorCode = PutFloat(pid, setname, FEATUREPTS, M, SetIdx(nm,1,4), g->x-len);
  ErrorCode = PutFloat(pid, setname, FEATUREPTS, M, SetIdx(nm,1,5), g->x-len);
  len = length/2.0;
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

  GetSetName(concat(MEDIUM, FLUX), setname, g->ID2x, g->osetlist);
  ErrorCode = PutFloat(pid, setname, "RegionalRecharge", MYR, SetIdx(nm), recharge);
  CheckUnit(pid, setname, TIMEPTS, tunit);
  CheckUnit(pid, setname, WATERFLUX, vunit);
  for(nf = 0; nf<numFlux; nf++)
  {
    *inf >> year >> flux >> NewLn;
    ErrorCode = PutFloat(pid, setname, TIMEPTS, tunit, SetIdx(nm, nf+1), year);
    ErrorCode = PutFloat(pid, setname, WATERFLUX, vunit, SetIdx(nm, nf+1), flux);
  }

  for(nc = 0; nc<numCon; nc++)
  {
    WFFCon_v2 *p = new WFFCon_v2(); TrackLocation(p);
    p->WriteDataset(pid, inf, nm, g, conlist);
    TrackDelete(p); p = NULL;
  }
  return 0;
}

int WFFSet_v2::WriteFile(long pid, ocsv *fp, int nm, Glyph *g, STR_VEC_PTR_VEC *conlist, char *ID)
{
  int nf;
  int nc;
  int nrec;
  int ErrorCode;

  Log("WFFSet_v2::WriteFile");
  nrec = 0;
  numCon = conlist->size();
  GetSetName(concat(MEDIUM, POLYS), setname, ID, g->isetlist);
  ErrorCode = GetString(pid, setname, FEATURE, blank, SetIdx(nm), name);
  ErrorCode = GetString(pid, setname, ATTRIBUTES, blank, SetIdx(nm, 1), type); ;
  ErrorCode = GetString(pid, setname, ATTRIBUTES, blank, SetIdx(nm, 2), dummy); width = ratod(dummy);
  ErrorCode = GetString(pid, setname, ATTRIBUTES, blank, SetIdx(nm, 3), dummy); length = ratod(dummy);
  ErrorCode = GetString(pid, setname, ATTRIBUTES, blank, SetIdx(nm, 4), dummy); distance = ratod(dummy);

  GetSetName(concat(MEDIUM, FLUX), setname, ID, g->isetlist);
  ErrorCode = GetFloat(pid, setname, "RegionalRecharge", MYR, SetIdx(nm), &recharge);

  // check to make sure MEDIUM matches type
  if(rstrcmpi(SURFACE_WATER, type) && rstrcmpi(MEDIUM, type))  return nrec;

  *fp << name << type;
  *fp << width << M;
  *fp << length << M;
  *fp << distance << M;
  *fp << recharge << MYR;
  *fp << numCon << NewLn;
  nrec++;

  Series_v2 *flux1 = new Series_v2(TIMEPTS, YEAR, WATERFLUX, "m^3/yr", "Volume/Time"); TrackLocation(flux1);
  int idx[MMF_MAX_DIM] = {nm, 0, 0, 0, 0, 0};
  flux1->ReadDatasets(pid, setname, idx, 2);
  *fp << flux1->xUnits << flux1->yUnits << flux1->count << NewLn;
  nrec++;
  for(nf = 1; nf<= flux1->count; nf++)
  {
    *fp << flux1->xValues[nf-1] << flux1->yValues[nf-1] << NewLn;
    nrec++;
  }
  TrackDelete(flux1);

  if(numCon>0)
    for(nc = 0; nc<numCon; nc++)
    {
      WFFCon_v2 *p = new WFFCon_v2(); TrackLocation(p);
      nrec += p->WriteFile(pid, fp, nm, g,(*conlist)[nc], ID);
      TrackDelete(p);
    }
    numCon = 0;
    return nrec;
}

int WFF_v2::WriteDataset(long pid, char *fuiname, Glyph *g, STR_VEC_PTR_VEC *conlist)
{
  int i;
  int numInfo;

  Log("WFF_v2::WriteDataset");
  rstrcpy(dummy, fuiname, _DOT, _WFF);
  inf = new icsv(dummy, '\"');  TrackLocation(inf);
  if(inf->ok())
  {
    *inf >> numInfo >> NewLn;
    for(i = 0; i<numInfo; i++)
      *inf >> dummy >> NewLn;

    *inf >> numSet >> NewLn;
    for(i = 0; i<numSet; i++)
    {
      WFFSet_v2 *p = new WFFSet_v2; TrackLocation(p);
      p->WriteDataset(pid, inf, i+1, g, conlist);
      TrackDelete(p);
    }
  }
  TrackDelete(inf);
  return 0;
}

int WFF_v2::WriteFile(long pid, char *outpath, Glyph *g, STR_VEC_PTR_VEC *conlist, char *fqual, char *ID, char *ID2x)
{
  int i, ErrorCode, s1=0;
  long nrec = 0, posbeg = 0;

  Log("WFF_v2::WriteFile");

  MEDIUM = NULL;
  if(!rstrcmpi(VADOSE, fqual))           MEDIUM = VADOSE;
  if(!rstrcmpi(AQUIFER, fqual))          MEDIUM = AQUIFER;
  if(!rstrcmpi(SURFACE_WATER, fqual))    MEDIUM = SURFACEWATER;
  if(MEDIUM == NULL) return 0;

  // check whether to create _WFF or append existing _WFF
  if(0 == access(AddExtension(outpath, _WFF), 0))
    outf = new ocsv(AddExtension(outpath, _WFF), '"', ',', _APPEND_);
  else
    outf = new ocsv(AddExtension(outpath, _WFF), '"', ',', _CREATE_);
  if(!outf->ok()) return 0;

  posbeg = outf->getpos();
  outf->smartQuote();
  *outf << ID << "         " << NewLn;
  outf->alwaysQuote();

  GetSetName(concat(MEDIUM, POLYS), setname, ID2x, g->isetlist);
  ErrorCode = GetVarDimSize(pid, setname, FEATURE, SetIdx(), &numSet);
  for (int j=1; j<=numSet; j++) {
    ErrorCode = GetString(pid, setname, FEATURE, blank, SetIdx(j), name);
    if (strlen(name)) s1++;
  }
  
//  numSet = 0;
//  GetSetName(concat(MEDIUM, POLYS), setname, ID2x, g->isetlist);
//  ErrorCode = GetVarDimSize(pid, setname, FEATURE, SetIdx(), &numSet);

  *outf << 1 << NewLn; nrec++;
  *outf << "_WFF generated by 1X Wrapper in Frames V2" << NewLn; nrec++;
  *outf << s1 << NewLn; nrec++;
//  *outf << numSet << NewLn; nrec++;
  for(i = 0; i<numSet; i++)
  {
    WFFSet_v2* p = new WFFSet_v2();  TrackLocation(p);
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