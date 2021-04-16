/*______________________________________________________________________________

  Date:       1993 - 2004
  Company:    Pacific Northwest National Laboratories
  Battelle Corporation
  ________________________________________________________________________________
  __Modifiication  History________________________________________________________

    DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#include "frames.h"
#include "bbfClass.h"

static BBF *bbf = NULL;

void BBFCon::Init()
{
  rstrcpy(name,"");
  rstrcpy(cas,"");
  rstrcpy(tunit,"");
  rstrcpy(vunit,"");
  filepos = 0;
  xypos = 0;
  numStep = 0;
  numProg = 0;
  prog = NULL;
}

BBFCon::BBFCon()
{ Init(); }

BBFCon::BBFCon (icsv *inf, bool isProg = false)
{
  Init();
  filepos = inf->getpos();
  *inf >> name >> cas >> tunit >> vunit >> numStep;
  if (isProg)      *inf >> NewLn;
  else             *inf >> numProg >> NewLn;
  xypos = inf->getpos();
  inf->Skip(numStep);
  if (numProg>0)
  {
    prog = new BBFCon*[numProg];
    for (int i = 0; i<numProg; i++)
      prog[i] = new BBFCon(inf, true);
  }
}

BBFCon::BBFCon (char *_name, char *_cas, int count)
{
  Init();
  rstrcpy(name,_name);
  rstrcpy(cas,_cas);
  numProg = count;
  if (numProg>0) prog = new BBFCon*[numProg];
}

BBFCon::~BBFCon()
{
  for (int i = 0; i<numProg; i++)
    if (prog[i]) delete prog[i];
  if (prog) delete[] prog;
  numProg = 0;
  prog = NULL;
}

void BBFOrg::Init()
{
  filepos = 0;
  rstrcpy(name,"");
  rstrcpy(id,"");
  numCon = 0;
  pCon = NULL;
  filepos = 0;
}

BBFOrg::BBFOrg()
{
  Init();
}

BBFOrg::BBFOrg (icsv *inf)
{
  int i;
  filepos = inf->getpos();
//  *inf >> name >> id >> numCon >> NewLn;
  *inf >> name >> numCon >> NewLn;

  pCon = new BBFCon*[numCon];
  for (i = 0; i<numCon; i++)
    pCon[i] = new BBFCon(inf);
}

BBFOrg::~BBFOrg()
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

void BBFSet::Init()
{
  filepos = 0;
  rstrcpy(name,"");
  rstrcpy(type,"");
  numOrg = 0;
  pOrg = NULL;
  filepos = 0;
  certpos = 0;
  varypos = 0;
  numCertTypes = 0;
  numVaryTypes = 0;
}

BBFSet::BBFSet()
{
  Init();
}

BBFSet::BBFSet (icsv *inf)
{
  int i;
  filepos = inf->getpos();
  *inf >> name >> type >> numOrg >> numVaryTypes >> numCertTypes >> NewLn;
  varypos = inf->getpos();
  *inf >> NewLn;
  certpos = inf->getpos();
  *inf >> NewLn;

  pOrg = new BBFOrg*[numOrg];
  for (i = 0; i<numOrg; i++)
    pOrg[i] = new BBFOrg(inf);
}

BBFSet::~BBFSet()
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

void BBF::Init()
{
  inf = NULL;
  set = NULL;
  numSet = 0;
}

BBF::BBF()
{
  Init();
}

BBF::BBF (char *fuiname, char *modId)
{
  char dummy[MAXPATH];

  Init();
  numSet = 0;
  set = NULL;

  sprintf(dummy,"%s.bbf",fuiname);
  inf = new icsv(dummy,'\"');
  if (!inf->ok()) return;
  if (0 != inf->SeekSection(modId,&bbfHead)) return;
  *inf >> numSet >> NewLn;
  set = new BBFSet*[numSet];
  for (int i = 0; i<numSet; i++)
    set[i] = new BBFSet(inf);
}

BBF::~BBF()
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
// BBF API ---------------------------------------------------------------------
//------------------------------------------------------------------------------
void FRAMES_API bbfOpen(char *fuiname, char *modId)
{
  bbfClose();
  bbf = new BBF(fuiname, modId);
}

void FRAMES_API bbfClose()
{
  if (bbf) delete bbf;
  bbf = NULL;
}

int FRAMES_API bbfGetNumRunInfo()
{
  int num;
  if (bbf == NULL) return 0;
  if (bbf->numSet<= 0) return 0;
  bbf->inf->setpos(bbf->bbfHead);
  *bbf->inf >> num >> NewLn;
  return num;
}

int FRAMES_API bbfGetRunInfo(int Idx, char *info)
{
  int num = bbfGetNumRunInfo();
  if (num == 0 || Idx>num) return 0;
  for (int i = 1; i<Idx; i++)
    *bbf->inf >> NewLn;
  *bbf->inf >> info >> NewLn;
  return 1;
}

int FRAMES_API bbfGetNumSets()
{
  if (!bbf) return 0;
  if (!bbf->set) return 0;
  return bbf->numSet;
}

int FRAMES_API bbfGetSetInfo(int dsIdx, char *name, char *type)
{
  if (!bbf) return 0;
  if (!bbf->set) return 0;
  if (dsIdx<1 || dsIdx>bbf->numSet) return 0;

  rstrcpy(name, bbf->set[dsIdx-1]->name);
  rstrcpy(type, bbf->set[dsIdx-1]->type);
  return 1;
}

int FRAMES_API bbfGetNumVULevels(int dsIdx)
{
  if (!bbf) return 0;
  if (!bbf->set) return 0;
  if (dsIdx<1 || dsIdx>bbf->numSet) return 0;
  return bbf->set[dsIdx-1]->numCertTypes * bbf->set[dsIdx-1]->numVaryTypes;
}

int FRAMES_API bbfGetVULevel(int dsIdx, int vuIdx, char *variability, char *uncertainty)
{
  int i;
  int vIdx;
  int uIdx;

  if (!bbf) return 0;
  if (!bbf->set) return 0;
  if (dsIdx<1 || dsIdx>bbf->numSet) return 0;
  for (i = 0, vIdx = 0; vIdx<bbf->set[dsIdx-1]->numVaryTypes; vIdx++)
  {
    for (uIdx = 0; uIdx<bbf->set[dsIdx-1]->numCertTypes; uIdx++)
    {
      i++;
      if (i == vuIdx) break;
    }
    if (i == vuIdx) break;
  }

  bbf->inf->setpos(bbf->set[dsIdx-1]->varypos);
  for (i = 0; i<= vIdx; i++)
    *bbf->inf >> variability;
  bbf->inf->setpos(bbf->set[dsIdx-1]->certpos);
  for (i = 0; i<= uIdx; i++)
    *bbf->inf >> uncertainty;
  return 1;
}

int FRAMES_API bbfGetNumOrganism(int dsIdx)
{
  if (!bbf) return 0;
  if (!bbf->set) return 0;
  if (dsIdx<1 || dsIdx>bbf->numSet) return 0;
  return bbf->set[dsIdx-1]->numOrg;
}

int FRAMES_API bbfGetOrganismName(int dsIdx, int orgIdx, char *name, char *id)
{
  if (!bbf) return 0;
  if (!bbf->set) return 0;
  if (dsIdx<1 || dsIdx>bbf->numSet) return 0;
  BBFSet *set = bbf->set[dsIdx-1];
  if (orgIdx<1 || orgIdx>set->numOrg) return 0;
  BBFOrg *org = set->pOrg[orgIdx-1];
  rstrcpy(name, org->name);
  rstrcpy(id, org->id);
  return 1;
}

int FRAMES_API bbfGetNumContam(int dsIdx, int orgIdx)
{
  if (!bbf) return 0;
  if (!bbf->set) return 0;
  if (dsIdx<1 || dsIdx>bbf->numSet) return 0;
  BBFSet *set = bbf->set[dsIdx-1];
  if (orgIdx<1 || orgIdx>set->numOrg) return 0;
  BBFOrg *org = set->pOrg[orgIdx-1];
  return org->numCon;
}

int FRAMES_API bbfGetNumProgeny(int dsIdx, int orgIdx, int conIdx)
{
  if (!bbf) return 0;
  if (!bbf->set) return 0;
  if (dsIdx<1 || dsIdx>bbf->numSet) return 0;
  BBFSet *set = bbf->set[dsIdx-1];
  if (orgIdx<1 || orgIdx>set->numOrg) return 0;
  BBFOrg *org = set->pOrg[orgIdx-1];
  if (conIdx<1 || conIdx>org->numCon) return 0;
  BBFCon *con = org->pCon[conIdx-1];
  return con->numProg;
}

int FRAMES_API bbfGetContamName(int dsIdx, int orgIdx, int conIdx, int progIdx, char *name, char *cas)
{
  if (!bbf) return 0;
  if (!bbf->set) return 0;
  if (dsIdx<1 || dsIdx>bbf->numSet) return 0;
  BBFSet *set = bbf->set[dsIdx-1];
  if (orgIdx<1 || orgIdx>set->numOrg) return 0;
  BBFOrg *org = set->pOrg[orgIdx-1];
  if (conIdx<1 || conIdx>org->numCon) return 0;
  BBFCon *con = org->pCon[conIdx-1];
  if (progIdx>con->numProg) return 0;
  if (progIdx>0) con = con->prog[progIdx-1];

  rstrcpy(name, con->name);
  rstrcpy(cas, con->cas);
  return 1;
}

int FRAMES_API bbfGetSeriesProperties(int dsIdx, int orgIdx, int conIdx, int progIdx, char *concunits, char *timeunits)
{
  int count = 0;
  char tmp1[SMALLSTRING];

  if (!bbf) return 0;
  if (!bbf->set) return 0;
  if (dsIdx<1 || dsIdx>bbf->numSet) return 0;
  BBFSet *set = bbf->set[dsIdx-1];
  if (orgIdx<1 || orgIdx>set->numOrg) return 0;
  BBFOrg *org = set->pOrg[orgIdx-1];
  if (conIdx<1 || conIdx>org->numCon) return 0;
  BBFCon *con = org->pCon[conIdx-1];
  if (progIdx>con->numProg) return 0;
  if (progIdx>0) con = con->prog[progIdx-1];

  bbf->inf->setpos(con->filepos);
  *bbf->inf >> tmp1 >> tmp1 >> timeunits >> concunits >> count >> NewLn;
  return count;
}

int FRAMES_API bbfGetSeriesValues(int dsIdx, int orgIdx, int conIdx, int progIdx, int vuIdx, int count, double *times, double *values)
{
  char tmp[SMALLSTRING];
  int cnt = bbfGetSeriesProperties(dsIdx, orgIdx, conIdx, progIdx, tmp, tmp);
  if (count < cnt) return 0;
  for (int i = 0; i<cnt; i++)
  {
    *bbf->inf >> times[i];
    for (int j = 0; j<vuIdx; j++) *bbf->inf >> values[i];
    *bbf->inf >> NewLn;
  }
  return cnt;
}

