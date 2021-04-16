/*______________________________________________________________________________

  Date:       1993 - 2004
  Company:    Pacific Northwest National Laboratories
  Battelle Corporation
  ________________________________________________________________________________
  __Modifiication  History________________________________________________________

    DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#include "frames.h"
#include "exfClass.h"

static EXF *exf = NULL;

void EXFDes::Init()
{
  rstrcpy(vlbl,"");
  rstrcpy(tunit,"");
  rstrcpy(vunit,"");
  filepos = 0;
  xypos = 0;
  numStep = 0;
}

EXFDes::EXFDes(icsv *inf)
{
  Init();
  filepos = inf->getpos();
  *inf >> vlbl >> NewLn;
  *inf >> numStep >> tunit >> vunit >> NewLn;
  xypos = inf->getpos();
  inf->Skip(numStep);
}

EXFDes::~EXFDes(){}

void EXFCon::Init()
{
  rstrcpy(name,"");
  rstrcpy(cas,"");
  filepos = 0;
  numDes = 0;
  des = NULL;
}

EXFCon::EXFCon()
{ Init(); }

EXFCon::EXFCon (icsv *inf)
{
  Init();
  filepos = inf->getpos();
  *inf >> name >> cas >> numDes >> NewLn;
  if (numDes>0)
  {
    des = new EXFDes*[numDes];
    for (int i = 0; i<numDes; i++)
      des[i] = new EXFDes(inf);
  }
}

EXFCon::~EXFCon()
{
  for (int i = 0; i<numDes; i++)
    if (des[i]) delete des[i];
  if (des) delete[] des;
  numDes = 0;
  des = NULL;
}

void EXFOrg::Init()
{
  filepos = 0;
  rstrcpy(name,"");
  rstrcpy(id,"");
  numCon = 0;
  pCon = NULL;
  filepos = 0;
}

EXFOrg::EXFOrg()
{
  Init();
}

EXFOrg::EXFOrg (icsv *inf)
{
  int i;
  filepos = inf->getpos();
  *inf >> name >> id >> numCon >> NewLn;
  if (numCon > 0)
  {
    pCon = new EXFCon*[numCon];
    for (i = 0; i<numCon; i++)
      pCon[i] = new EXFCon(inf);
  }
}

EXFOrg::~EXFOrg()
{
  if (pCon)
  {
    for (int i = 0; i<numCon; i++)
      if (pCon[i]) delete pCon[i];
    delete[] pCon;
  }
  numCon = 0;
  pCon = NULL;
}

void EXFSet::Init()
{
  filepos = 0;
  rstrcpy(name,"");
  rstrcpy(type,"");
  numOrg = 0;
  pOrg = NULL;
  filepos = 0;
}

EXFSet::EXFSet()
{
  Init();
}

EXFSet::EXFSet (icsv *inf)
{
  int i;
  filepos = inf->getpos();
  *inf >> type >> name >> numOrg >> NewLn;
  if (numOrg > 0)
  {
    pOrg = new EXFOrg*[numOrg];
    for (i = 0; i<numOrg; i++)
      pOrg[i] = new EXFOrg(inf);
  }
}

EXFSet::~EXFSet()
{
  if (pOrg)
  {
    for (int i = 0; i<numOrg; i++)
      if (pOrg[i]) delete pOrg[i];
    delete[] pOrg;
  }
  numOrg = 0;
  pOrg = NULL;
}

void EXF::Init()
{
  inf = NULL;
  set = NULL;
  numSet = 0;
}

EXF::EXF()
{
  Init();
}

EXF::EXF (char *fuiname, char *modId)
{
  char dummy[MAXPATH];

  Init();
  numSet = 0;
  set = NULL;

  sprintf(dummy,"%s.hqf",fuiname);
  inf = new icsv(dummy,'\"');
  if (!inf->ok()) return;
  if (0 != inf->SeekSection(modId,&exfHead)) return;
  *inf >> numSet >> NewLn;
  if (numSet>0)
  {
    set = new EXFSet*[numSet];
    for (int i = 0; i<numSet; i++)
      set[i] = new EXFSet(inf);
  }
}

EXF::~EXF()
{
  if (set)
  {
    for (int i = 0; i<numSet; i++)
      if (set[i]) delete set[i];
    delete[] set;
  }
  numSet = 0;
  set = NULL;
  delete inf;
}

//------------------------------------------------------------------------------
// EXF API ---------------------------------------------------------------------
//------------------------------------------------------------------------------
void FRAMES_API exfOpen(char *fuiname, char *modId)
{
  exfClose();
  exf = new EXF(fuiname, modId);
}

void FRAMES_API exfClose()
{
  if (exf) delete exf;
  exf = NULL;
}

int FRAMES_API exfGetNumRunInfo()
{
  int num;
  if (exf == NULL) return 0;
  if (exf->numSet<= 0) return 0;
  exf->inf->setpos(exf->exfHead);
  *exf->inf >> num >> NewLn;
  return num;
}

int FRAMES_API exfGetRunInfo(int Idx, char *info)
{
  int num = exfGetNumRunInfo();
  if (num == 0 || Idx>num) return 0;
  for (int i = 1; i<Idx; i++)
    *exf->inf >> NewLn;
  *exf->inf >> info >> NewLn;
  return 1;
}

int FRAMES_API exfGetNumSets()
{
  if (!exf) return 0;
  if (!exf->set) return 0;
  return exf->numSet;
}

int FRAMES_API exfGetSetInfo(int dsIdx, char *name, char *type)
{
  if (!exf) return 0;
  if (!exf->set) return 0;
  if (dsIdx<1 || dsIdx>exf->numSet) return 0;

  rstrcpy(name, exf->set[dsIdx-1]->name);
  rstrcpy(type, exf->set[dsIdx-1]->type);
  return 1;
}

int FRAMES_API exfGetNumOrganism(int dsIdx)
{
  if (!exf) return 0;
  if (!exf->set) return 0;
  if (dsIdx<1 || dsIdx>exf->numSet) return 0;
  EXFSet *set = exf->set[dsIdx-1];
  return set->numOrg;
}

int FRAMES_API exfGetNumLocation(int dsIdx)
{  return exfGetNumOrganism(dsIdx); }

EXFOrg *GetOrg(int dsIdx, int orgIdx)
{
  if (!exf) return NULL;
  if (!exf->set) return NULL;
  if (dsIdx<1 || dsIdx>exf->numSet) return NULL;
  EXFSet *set = exf->set[dsIdx-1];
  if (orgIdx<1 || orgIdx>set->numOrg) return NULL;
  return set->pOrg[orgIdx-1];
}

int FRAMES_API exfGetOrganismName(int dsIdx, int orgIdx, char *name, char *id)
{
  EXFOrg *org = GetOrg(dsIdx,orgIdx);
  if (org == NULL) return 0;
  rstrcpy(name, org->name);
  rstrcpy(id, org->id);
  return 1;
}

int FRAMES_API exfGetLocationName(int dsIdx, int locIdx, char *name, char *id)
{ return exfGetOrganismName(dsIdx, locIdx, name, id); }

int FRAMES_API exfGetNumContam(int dsIdx, int orgIdx)
{
  EXFOrg *org = GetOrg(dsIdx,orgIdx);
  if (org == NULL) return 0;
  return org->numCon;
}

int FRAMES_API exfGetContamName(int dsIdx, int orgIdx, int conIdx, char *name, char *cas)
{
  EXFOrg *org = GetOrg(dsIdx,orgIdx);
  if (org == NULL) return 0;
  if (conIdx<1 || conIdx>org->numCon) return 0;
  EXFCon *con = org->pCon[conIdx-1];

  rstrcpy(name, con->name);
  rstrcpy(cas, con->cas);
  return 1;
}

int FRAMES_API exfGetNumEffects(int dsIdx, int orgIdx, int conIdx)
{
  EXFOrg *org = GetOrg(dsIdx,orgIdx);
  if (org == NULL) return 0;
  if (conIdx<1 || conIdx>org->numCon) return 0;
  EXFCon *con = org->pCon[conIdx-1];
  return con->numDes;
}

int FRAMES_API exfGetSeriesProperties(int dsIdx, int orgIdx, int conIdx, int efxIdx, char *efxDes, char *efxunits, char *timeunits)
{
  int count = 0;
  EXFOrg *org = GetOrg(dsIdx,orgIdx);
  if (org == NULL) return 0;
  if (conIdx<1 || conIdx>org->numCon) return 0;
  EXFCon *con = org->pCon[conIdx-1];
  if (efxIdx<1 || efxIdx>con->numDes) return 0;
  EXFDes *des = con->des[efxIdx-1];

  exf->inf->setpos(des->filepos);
  *exf->inf >> efxDes >> NewLn;
  *exf->inf >> count >> timeunits >> efxunits >> NewLn;
  return count;
}

int FRAMES_API exfGetSeriesValues(int dsIdx, int orgIdx, int conIdx, int efxIdx, int count, double *times, double *values)
{
  char tmp[LARGESTRING];
  int cnt = exfGetSeriesProperties(dsIdx, orgIdx, conIdx, efxIdx, tmp, tmp, tmp);
  if (count < cnt) return 0;
  for (int i = 0; i<cnt; i++)
    *exf->inf >> times[i] >> values[i] >> NewLn;
  return cnt;
}

