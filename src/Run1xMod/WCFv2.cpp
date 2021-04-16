#include "WCFv2.h"

Series_v2 *WCFInitializeNames(char *contype, STR_VEC_MAP setlist, char *ftype, char *ID)
{
  Series_v2 *conc;

  sprintf(dummy, "%s%s%s", contype, MEDIUM, MODIFIER);
  GetSetName(dummy, setname, ID, setlist);
  if(!rstrcmpi(CHEM, contype))
    conc = new Series_v2(TIMEPTS, YEAR, ftype, GML, cmWCF);
  else
    conc = new Series_v2(TIMEPTS, YEAR, ftype, PCIML, rmWCF);
  return conc;
}

int WCFCon_v2::WriteDataset(long pid, icsv *inf, int nm, Glyph *g, STR_VEC_PTR_VEC *conlist, char *type)
{
  int i;
  int ErrorCode;
  int idx[MMF_MAX_DIM];
  char setprefix[8];

  Log("WCFCon_v2::WriteDataset");
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
    idx[1] = idx[0];
    idx[0] = nm;
    Series_v2 *conc = WCFInitializeNames(setprefix, g->osetlist, CONC, g->ID2x);    TrackLocation(conc);

    FindBenchmarks(type, cas, conc);

    conc->WriteDatasets(pid, inf, numStep, setname, idx, 2);
    TrackDelete(conc);
  }
  return 0;
}

int WCFCon_v2::WriteFile(long pid, ocsv *fp, int nm, Glyph *g, STR_VEC *conlist, char *ID)
{
  int i;
  int nrec;
  int ErrorCode;
  char setprefix[8];
  int idx[MMF_MAX_DIM];

  Log("WCFCon_v2::WriteFile");
  nrec = 0;
  numProg = 0;
  STR_VEC_ITR iv = conlist->begin();
  rstrcpy(setprefix, iv->c_str());
  ++iv;
  rstrcpy(cas, iv->c_str());

  for(i = 0; i<MMF_MAX_DIM; i++) idx[i] = 0;
  GetSetName(concat(setprefix, LIST), setname, blank, g->isetlist);
  ErrorCode = VarLookUp(pid, setname, CASID, cas, idx);
  ErrorCode += GetString(pid, setname, NAME, blank, idx, name);
  if(ErrorCode == 0)
  {
    idx[1] = idx[0]; idx[0] = nm;
    Series_v2 *conc = WCFInitializeNames(setprefix, g->isetlist, CONC, ID);    TrackLocation(conc);
    conc->ReadDatasets(pid, setname, idx, 3);

    *fp << name << cas;
    *fp << conc->xUnits;     // yr
    *fp << conc->yUnits;     // g/yr | pCi/yr
    *fp << conc->count;
    *fp << numProg;
    *fp << NewLn;
    nrec++;
    nrec += conc->WriteFile(fp);
    TrackDelete(conc);
  }
  else
  {
    *fp << name << cas << YEAR << blank << 0 << 0 <<NewLn;
    nrec++;
  }
  return nrec;
}

int WCFSet_v2::WriteDataset(long pid, icsv *inf, int nm, Glyph *g, STR_VEC_PTR_VEC *conlist)
{
  int nc;
  int ErrorCode;

  Log("WCFSet_v2::WriteDataset");
  *inf >> name >> type >> numCon >> x >> dummy >> y >> dummy >> z >> dummy >> NewLn;

  MEDIUM = NULL;
  if(!rstrcmpi(AQUIFER, type))                 { MEDIUM = AQUIFER;      MODIFIER = DISSOLVEDCONC; }
  if(!rstrcmpi(SURFACE_WATER, type))           { MEDIUM = SURFACEWATER; MODIFIER = DISSOLVEDCONC; }
  if(!rstrcmpi("Aquifer-Total", type))         { MEDIUM = AQUIFER;      MODIFIER = TOTALCONC; }
  if(!rstrcmpi("Surface Water-Total", type))   { MEDIUM = SURFACEWATER; MODIFIER = TOTALCONC; }
  if(MEDIUM == NULL) return 0;

  GetSetName(concat(MEDIUM, PTS), setname, g->ID2x, g->osetlist);
  ErrorCode = PutString(pid, setname, FEATURE, blank, SetIdx(nm), name);
  ErrorCode = PutFloat(pid, setname, FEATUREPTS, M, SetIdx(nm, 1, 1), x);
  ErrorCode = PutFloat(pid, setname, FEATUREPTS, M, SetIdx(nm, 2, 1), y);
  ErrorCode = PutFloat(pid, setname, FEATUREPTS, M, SetIdx(nm, 3, 1), z);

  ErrorCode = PutString(pid, setname, ATTRIBUTEDES, blank, SetIdx(1), "Type of medium");
  ErrorCode = PutString(pid, setname, ATTRIBUTES, blank, SetIdx(nm, 1), type);

  for(nc = 0; nc<numCon; nc++)
  {
    WCFCon_v2 *p = new WCFCon_v2(); TrackLocation(p);
    p->WriteDataset(pid, inf, nm, g, conlist, type);
    TrackDelete(p);
  }
  return 0;
}

int WCFSet_v2::WriteFile(long pid, ocsv *fp, int nm, Glyph *g, STR_VEC_PTR_VEC *conlist, char *fqual, char *ID)
{
  int nc;
  int nrec;
  int ErrorCode;

  Log("WCFSet_v2::WriteFile");
  nrec = 0;
  numCon = conlist->size();
  GetSetName(concat(MEDIUM, PTS), setname, ID, g->isetlist);
  ErrorCode = GetString(pid, setname, FEATURE, blank, SetIdx(nm), name);
  ErrorCode = GetString(pid, setname, ATTRIBUTES, blank, SetIdx(nm, 1), type);
  ErrorCode = GetFloat(pid, setname, FEATUREPTS, M, SetIdx(nm, 1, 1), &x);
  ErrorCode = GetFloat(pid, setname, FEATUREPTS, M, SetIdx(nm, 2, 1), &y);
  ErrorCode = GetFloat(pid, setname, FEATUREPTS, M, SetIdx(nm, 3, 1), &z);

  if(!rstrcmpi(AQUIFER, type))                 { MODIFIER = DISSOLVEDCONC; }
  if(!rstrcmpi(SURFACE_WATER, type))           { MODIFIER = DISSOLVEDCONC; }
  if(!rstrcmpi("Aquifer-Total", type))         { MODIFIER = TOTALCONC; }
  if(!rstrcmpi("Surface Water-Total", type))   { MODIFIER = TOTALCONC; }
  // check to make sure MEDIUM matches type
//  if(rstrcmpi(fqual, type)) return nrec;
//  if(rstrcmpi(MEDIUM, type)) return nrec;

  *fp << name << type << numCon << x << M << y << M << z << M << NewLn;
  nrec++;

  if(numCon>0)
    for(nc = 0; nc<numCon; nc++)
    {
      WCFCon_v2 *p = new WCFCon_v2();  TrackLocation(p);
      nrec += p->WriteFile(pid, fp, nm, g,(*conlist)[nc], ID);
      TrackDelete(p);
    }
  numCon = 0;
  return nrec;
}

int WCF_v2::WriteDataset(long pid, char *fuiname, Glyph *g, STR_VEC_PTR_VEC *conlist)
{
  int i;
  int numInfo;

  Log("WCF_v2::WriteDataset");
  rstrcpy(dummy, fuiname, _DOT, _WCF);
  inf = new icsv(dummy, '\"'); TrackLocation(inf);
  if(inf->ok())
  {
    *inf >> numInfo >> NewLn;
    for(i = 0; i<numInfo; i++)
      *inf >> dummy >> NewLn;

    *inf >> numSet >> NewLn;
    for(i = 0; i<numSet; i++)
    {
      WCFSet_v2 *p = new WCFSet_v2(); TrackLocation(p);
      p->WriteDataset(pid, inf, i+1, g, conlist);
      TrackDelete(p);
    }
  }
  TrackDelete(inf);
  return 0;
}

int WCF_v2::WriteFile(long pid, char *outpath, Glyph *g, STR_VEC_PTR_VEC *conlist, char *fqual, char *ID, char *ID2x)
{
  int i;
  int ErrorCode;
  int setCt=0;
  long nrec = 0, posbeg = 0;

  Log("WCF_v2::WriteFile");
  // Make sure valid FRAMES 1x qualifier
  MEDIUM = NULL;
//  if(!rstrcmpi(AQUIFER, fqual))                 { MEDIUM = AQUIFER;      MODIFIER = DISSOLVEDCONC; }
//  if(!rstrcmpi(SURFACE_WATER, fqual))           { MEDIUM = SURFACEWATER; MODIFIER = DISSOLVEDCONC; }
//  if(!rstrcmpi("Aquifer-Total", fqual))         { MEDIUM = AQUIFER;      MODIFIER = TOTALCONC; }
//  if(!rstrcmpi("Surface Water-Total", fqual))   { MEDIUM = SURFACEWATER; MODIFIER = TOTALCONC; }
  if(!rstrcmpi(AQUIFER, fqual))                 { MEDIUM = AQUIFER;      }
  if(!rstrcmpi(SURFACE_WATER, fqual))           { MEDIUM = SURFACEWATER; }
  if(!rstrcmpi("Aquifer-Total", fqual))         { MEDIUM = AQUIFER;      }
  if(!rstrcmpi("Surface Water-Total", fqual))   { MEDIUM = SURFACEWATER; }
  if(MEDIUM == NULL) return 0;

  // check whether to create _WCF or append existing _WCF
  if(0 == access(AddExtension(outpath, _WCF), 0))
    outf = new ocsv(AddExtension(outpath, _WCF), '"', ',', _APPEND_);
  else
    outf = new ocsv(AddExtension(outpath, _WCF), '"', ',', _CREATE_);
  if(!outf->ok()) return 0;

  posbeg = outf->getpos();
  outf->smartQuote();
  *outf << ID << "         " << NewLn;
  outf->alwaysQuote();

  numSet = 0;
  GetSetName(concat(MEDIUM, PTS), setname, ID2x, g->isetlist);
  ErrorCode = GetVarDimSize(pid, setname, FEATURE, SetIdx(), &numSet);

  *outf << 1 << NewLn; nrec++;
  *outf << "_WCF generated by 1X Wrapper in Frames V2" << NewLn; nrec++;
  *outf << numSet << NewLn; nrec++;
  for(i = 0; i<numSet; i++)
  {
    WCFSet_v2* p = new WCFSet_v2(); TrackLocation(p);
    nrec += p->WriteFile(pid, outf, i+1, g, conlist, fqual, ID2x);
    TrackDelete(p);
  }

  outf->setpos(posbeg);
  *outf << ID;
  fprintf(outf->fptr,"%ld",nrec);
  delete outf;
  outf = NULL;
  return 0;
}