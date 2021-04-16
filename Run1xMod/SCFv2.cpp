#include "SCFv2.h"

Series_v2 *SCFInitializeNames(char *contype, STR_VEC_MAP setlist, char *ftype, char *ID)
{
  Series_v2 *conc;

  sprintf(dummy, "%s%s%s", contype, MEDIUM, MODIFIER);
    Log(dummy);
  GetSetName(dummy, setname, ID, setlist);
    Log(setname);

  if (!rstrcmpi(MODIFIER, DISSOLVEDCONC))
    if(!rstrcmpi(CHEM, contype))
      conc = new Series_v2(TIMEPTS, YEAR, ftype, MGL, cmWCF);
    else
      conc = new Series_v2(TIMEPTS, YEAR, ftype, PCIL, rmWCF);
  else
    if(!rstrcmpi(CHEM, contype))
      conc = new Series_v2(TIMEPTS, YEAR, ftype, MGKG, cmSCF);
    else
      conc = new Series_v2(TIMEPTS, YEAR, ftype, PCIKG, rmSCF);
  return conc;
}

int SCFCon_v2::WriteDataset(long pid, icsv *inf, int nm, Glyph *g, STR_VEC_PTR_VEC *conlist, char *type)
{
  int i;
  int ErrorCode;
  int idx[MMF_MAX_DIM];
  char setprefix[8];

  Log("SCFCon_v2::WriteDataset");
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
    Series_v2 *conc = SCFInitializeNames(setprefix, g->osetlist, CONC, g->ID2x);    TrackLocation(conc);

    FindBenchmarks(type, cas, conc);
    
    conc->WriteDatasets(pid, inf, numStep, setname, idx, 2);
    TrackDelete(conc);
  }
  return 0;
}

int SCFCon_v2::WriteFile(long pid, ocsv *fp, int nm, Glyph *g, STR_VEC *conlist, char *ID)
{
  int i;
  int nrec;
  int ErrorCode;
  char setprefix[8];
  int idx[MMF_MAX_DIM];

  Log("SCFCon_v2::WriteFile");
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
    Series_v2 *conc = SCFInitializeNames(setprefix, g->isetlist, CONC, ID);   TrackLocation(conc);
    conc->ReadDatasets(pid, setname, idx, 3);

    *fp << name << cas;
    *fp << conc->xUnits; // YEAR;
    *fp << conc->yUnits;     // "g/yr" | pCi/yr;
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

int SCFSet_v2::WriteDataset(long pid, icsv *inf, int nm, Glyph *g, STR_VEC_PTR_VEC *conlist)
{
  int nc;
  int ErrorCode;

  Log("SCFSet_v2::WriteDataset");
  *inf >> name >> type >> x >> dummy >> y >> dummy >> z >> dummy >> numCon >> NewLn;

  MEDIUM = NULL;
  if(!rstrcmpi(SOIL, type))                  { MEDIUM = SOIL;     MODIFIER = TOTALCONC; }
  if(!rstrcmpi(SEDIMENT, type))              { MEDIUM = SEDIMENT; MODIFIER = TOTALCONC; }
  if(!rstrcmpi("Soil-Dissolved", type))      { MEDIUM = SOIL;     MODIFIER = DISSOLVEDCONC; }
  if(!rstrcmpi("Sediment-Dissolved", type))  { MEDIUM = SEDIMENT; MODIFIER = DISSOLVEDCONC; }
  if(MEDIUM == NULL) return 0;

  GetSetName(concat(MEDIUM, VOLUMES), setname, g->ID2x, g->osetlist);
  ErrorCode = PutString(pid, setname, FEATURE, blank, SetIdx(nm), name);
  ErrorCode = PutString(pid, setname, ATTRIBUTEDES, blank, SetIdx(1), "Type of medium");
  ErrorCode = PutString(pid, setname, ATTRIBUTEDES, blank, SetIdx(2), "x Dimension of volume (m)");
  ErrorCode = PutString(pid, setname, ATTRIBUTEDES, blank, SetIdx(3), "y Dimension of volume (m)");
  ErrorCode = PutString(pid, setname, ATTRIBUTEDES, blank, SetIdx(4), "z Dimension of volume (m)");

  ErrorCode = PutString(pid, setname, ATTRIBUTES, blank, SetIdx(nm, 1), type);
  sprintf(dummy, "%20.12f", x);  ErrorCode = PutString(pid, setname, ATTRIBUTES, blank, SetIdx(nm, 2), dummy);
  sprintf(dummy, "%20.12f", y);  ErrorCode = PutString(pid, setname, ATTRIBUTES, blank, SetIdx(nm, 3), dummy);
  sprintf(dummy, "%20.12f", z);  ErrorCode = PutString(pid, setname, ATTRIBUTES, blank, SetIdx(nm, 4), dummy);

  for(nc = 0; nc<numCon; nc++)
  {
    SCFCon_v2 *p = new SCFCon_v2();  TrackLocation(p);
    p->WriteDataset(pid, inf, nm, g, conlist, type);
    TrackDelete(p);
  }
  return 0;
}

int SCFSet_v2::WriteFile(long pid, ocsv *fp, int nm, Glyph *g, STR_VEC_PTR_VEC *conlist, char *fqual, char *ID)
{
  int nc;
  int nrec;
  int ErrorCode;

  Log("SCFSet_v2::WriteFile");
  nrec = 0;
  numCon = conlist->size();
  GetSetName(concat(MEDIUM, VOLUMES), setname, ID, g->isetlist);
  ErrorCode = GetString(pid, setname, FEATURE, blank, SetIdx(nm), name);
  ErrorCode = GetString(pid, setname, ATTRIBUTES, blank, SetIdx(nm, 1), type);
  ErrorCode = GetString(pid, setname, ATTRIBUTES, blank, SetIdx(nm, 2), dummy);  x = ratod(dummy);
  ErrorCode = GetString(pid, setname, ATTRIBUTES, blank, SetIdx(nm, 3), dummy);  y = ratod(dummy);
  ErrorCode = GetString(pid, setname, ATTRIBUTES, blank, SetIdx(nm, 4), dummy);  z = ratod(dummy);

  if(!rstrcmpi(SOIL, type))                  { MODIFIER = TOTALCONC; }
  if(!rstrcmpi(SEDIMENT, type))              { MODIFIER = TOTALCONC; }
  if(!rstrcmpi("Soil-Dissolved", type))      { MODIFIER = DISSOLVEDCONC; }
  if(!rstrcmpi("Sediment-Dissolved", type))  { MODIFIER = DISSOLVEDCONC; }
  // check to make sure MEDIUM matches type
//  if(rstrcmpi(MEDIUM, type)) return nrec;
//  if(rstrcmpi(MODIFIER, type)) return nrec;


  *fp << name << type;
  *fp << x << M;
  *fp << y << M;
  *fp << z << M;
  *fp << numCon << NewLn;
  nrec++;

  if(numCon>0)
    for(nc = 0; nc<numCon; nc++)
    {
      SCFCon_v2 *p = new SCFCon_v2(); TrackLocation(p);
      nrec += p->WriteFile(pid, fp, nm, g,(*conlist)[nc], ID);
      TrackDelete(p);
    }
  numCon = 0;
  return nrec;
}

int SCF_v2::WriteDataset(long pid, char *fuiname, Glyph *g, STR_VEC_PTR_VEC *conlist)
{
  int i;
  int numInfo;

  Log("SCF_v2::WriteDataset");
  rstrcpy(dummy, fuiname, _DOT, _SCF);
  inf = new icsv(dummy, '\"'); TrackLocation(inf);
  if(inf->ok())
  {
    *inf >> numInfo >> NewLn;
    for(i = 0; i<numInfo; i++)
      *inf >> dummy >> NewLn;

    *inf >> numSet >> NewLn;
    for(i = 0; i<numSet; i++)
    {
      SCFSet_v2 *p = new SCFSet_v2(); TrackLocation(p);
      p->WriteDataset(pid, inf, i+1, g, conlist);
      TrackDelete(p);
    }
  }
  TrackDelete(inf);
  return 0;
}

int SCF_v2::WriteFile(long pid, char *outpath, Glyph *g, STR_VEC_PTR_VEC *conlist, char *fqual, char *ID, char *ID2x)
{
  int i;
  int ErrorCode;
  long nrec = 0, posbeg = 0;

  Log("SCF_v2::WriteFile");
  // Make sure valid FRAMES 1x qualifier
  MEDIUM = NULL;
//  if(!rstrcmpi(SOIL, fqual))                  { MEDIUM = SOIL;     MODIFIER = TOTALCONC; }
//  if(!rstrcmpi(SEDIMENT, fqual))              { MEDIUM = SEDIMENT; MODIFIER = TOTALCONC; }
//  if(!rstrcmpi("Soil-Dissolved", fqual))      { MEDIUM = SOIL;     MODIFIER = DISSOLVEDCONC; }
//  if(!rstrcmpi("Sediment-Dissolved", fqual))  { MEDIUM = SEDIMENT; MODIFIER = DISSOLVEDCONC; }
  if(!rstrcmpi(SOIL, fqual))                  { MEDIUM = SOIL;     }
  if(!rstrcmpi(SEDIMENT, fqual))              { MEDIUM = SEDIMENT; }
  if(!rstrcmpi("Soil-Dissolved", fqual))      { MEDIUM = SOIL;     }
  if(!rstrcmpi("Sediment-Dissolved", fqual))  { MEDIUM = SEDIMENT; }
  if(MEDIUM == NULL) return 0;

  // check whether to create _SCF or append existing _SCF
  if(0 == access(AddExtension(outpath, _SCF), 0))
    outf = new ocsv(AddExtension(outpath, _SCF), '"', ',', _APPEND_);
  else
    outf = new ocsv(AddExtension(outpath, _SCF), '"', ',', _CREATE_);
  if(!outf->ok()) return 0;

  posbeg = outf->getpos();
  outf->smartQuote();
  *outf << ID << "         " << NewLn;
  outf->alwaysQuote();

  numSet = 0;
  GetSetName(concat(MEDIUM, VOLUMES), setname, ID2x, g->isetlist);
  ErrorCode = GetVarDimSize(pid, setname, FEATURE, SetIdx(), &numSet);

  *outf << 1 << NewLn; nrec++;
  *outf << "_SCF generated by 1X Wrapper in Frames V2" << NewLn; nrec++;
  *outf << numSet << NewLn; nrec++;
  for(i = 0; i<numSet; i++)
  {
    SCFSet_v2* p = new SCFSet_v2(); TrackLocation(p);
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