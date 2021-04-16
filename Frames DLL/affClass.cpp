/*______________________________________________________________________________

  Date:       1993 - 2004
  Company:    Pacific Northwest National Laboratories
  Battelle Corporation
  ________________________________________________________________________________
  __Modifiication  History________________________________________________________

    DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#include "frames.h"
#include "affClass.h"

static AFF *aff = NULL;

void AFFCon::Init()
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

AFFCon::AFFCon()
{ Init(); }

AFFCon::AFFCon (icsv *inf, bool isProg = false)
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
    prog = new AFFCon*[numProg];
    for (int i = 0; i<numProg; i++)
      prog[i] = new AFFCon(inf, true);
  }
}

AFFCon::AFFCon (char *_name, char *_cas, int count)
{
  Init();
  rstrcpy(name,_name);
  rstrcpy(cas,_cas);
  numProg = count;
  if (numProg>0) prog = new AFFCon*[numProg];
}

AFFCon::~AFFCon()
{
  for (int i = 0; i<numProg; i++)
    if (prog[i]) delete prog[i];
    if (prog) delete[] prog;
    numProg = 0;
    prog = NULL;
}

void AFFSet::Init()
{
  filepos = 0;
  rstrcpy(name,"");
  rstrcpy(type,"");
  numCon = 0;
  pCon = NULL;
  filepos = 0;
  numFluxTypes = 0;
  area = 0.0;
  extHt = 0.0;
  adjHt = 0.0;
  extVel = 0.0;
  extTmp = 0.0;
  ambTmp = 0.0;
}

AFFSet::AFFSet()
{
  Init();
}

AFFSet::AFFSet (icsv *inf)
{
  int i;
  filepos = inf->getpos();
  *inf >> name >> NewLn; // name
  *inf >> type >> NewLn; // point or area
  *inf >> area >> NewLn; // exit area
  *inf >> extHt >> NewLn; // exit height
  *inf >> adjHt >> NewLn; // adj struct height
  *inf >> extVel >> NewLn; // exit velocity
  *inf >> extTmp >> NewLn; // exit temp
  *inf >> ambTmp >> NewLn; // amb air temp
  *inf >> numFluxTypes >> NewLn; // number of flux types
  fluxpos = inf->getpos();
  inf->Skip(numFluxTypes);
  *inf >> numCon >> NewLn;
  pCon = new AFFCon*[numCon];
  for (i = 0; i<numCon; i++)
    pCon[i] = new AFFCon(inf);
}

AFFSet::~AFFSet()
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

int AFFSet::convertAFF(icsv *inf, ocsv *eOut, char *casList)
{
  int i, j, p, m;
  double size;
  double density;
  char sunit[SMALLSTRING];
  char dunit[SMALLSTRING];
  AFFCon *con;
  bool found;

  char *mylist = strdup(casList);
  splitList(mylist);
  // dataset info
  *eOut << name << NewLn; // name
  *eOut << type << NewLn; // point or area
  *eOut << area << "m^2" << NewLn; // exit area
  *eOut << extHt << "m" << NewLn; // exit height
  *eOut << adjHt << "m" << NewLn; // adj struct height
  *eOut << extVel << "m/sec" << NewLn; // exit velocity
  *eOut << extTmp << "C" << NewLn; // exit temp
  *eOut << ambTmp << "C" << NewLn; // amb air temp
  *eOut << numFluxTypes << NewLn; // number of flux types
  // flux info
  inf->setpos(fluxpos);
  for (i = 0; i<numFluxTypes; i++)
  {
    *inf >> type >> size >> sunit >> density >> dunit >> NewLn;
    *eOut << type << size << sunit << density << dunit << NewLn;
  }
  *eOut << (long)rList.size() << NewLn;

  for (p = 0; p< (long)rList.size(); p++)
  {
    // loading chemical names
    con = NULL;
    Series **xy = new Series*[numFluxTypes];
    for (m = 0; m<numFluxTypes; m++) xy[m] = NULL;

    for (i = 0; i<numCon; i++)
    {
      found = !rstrcmpi(rList[p], pCon[i]->cas);
      if (found)
        con = pCon[i];
      else
        for (j = 0; j<pCon[i]->numProg; j++)
        {
          found = !rstrcmpi(rList[p], pCon[i]->prog[j]->cas);
          if (found)
          {
            con = pCon[i]->prog[j];
            break;
          }
        }
      if (found)
        for (m = 1; m<= numFluxTypes; m++)
        {
          inf->setpos(con->xypos);
          if (xy[m-1] == NULL)
          {
            xy[m-1] = new Series();
            xy[m-1]->Read(inf, con->numStep, m);
          }
          else
          {
            Series *other = new Series();
            other->Read(inf, con->numStep, m);
            xy[m-1]->Add(other);
            delete other;
          }
        }
    }

    if (xy[0] == NULL)
      *eOut << "Not found" << rList[p] << "yr" << "?" << 0 << 0 << NewLn;
    else
    {
      *eOut << con->name << rList[p] << con->tunit << con->vunit << xy[0]->count << 0 << NewLn;
      for (i = 0; i<xy[0]->count; i++)
      {
        *eOut << xy[0]->xValues[i];
        for (m = 0; m<numFluxTypes; m++)
          *eOut << (double)xy[m]->yValues[i];
        *eOut << NewLn;
      }
    }
    for (m = 0; m<numFluxTypes; m++) delete xy[m];
    delete[] xy;
  }

  delete mylist;
  return 0;
}

// this routine will add liked named parents in the caslist
// then write them with degradation product that have zero time steps
// ???List are comma seperated and should be one to one with cas as primary key
// all degradation products must be listed as parents if not they are ignored
int AFFSet::unconvertAFF(icsv *inf, ocsv *eOut, char *casList, char *nameList, char *degList, char *secList, int branching)
{
  int i, j, p, m, last, lcnt;
  double size;
  double density;
  char sunit[SMALLSTRING];
  char dunit[SMALLSTRING];
  AFFCon *con;
  bool found;

  OpenDecayChain(casList, nameList, degList, secList, branching);

  // dataset info
  *eOut << name << NewLn; // name
  *eOut << type << NewLn; // point or area
  *eOut << area << "m^2" << NewLn; // exit area
  *eOut << extHt << "m" << NewLn; // exit height
  *eOut << adjHt << "m" << NewLn; // adj struct height
  *eOut << extVel << "m/sec" << NewLn; // exit velocity
  *eOut << extTmp << "C" << NewLn; // exit temp
  *eOut << ambTmp << "C" << NewLn; // amb air temp
  *eOut << numFluxTypes << NewLn; // number of flux types
  lcnt = 9;
  // flux info
  inf->setpos(fluxpos);
  for (i = 0; i<numFluxTypes; i++)
  {
    *inf >> type >> size >> sunit >> density >> dunit >> NewLn;
    *eOut << type << size << sunit << density << dunit << NewLn;
    lcnt+= 2;
  }

  *eOut << (long)cList.size() << NewLn;
  lcnt++;

  for (p = 0; p< (long)cList.size(); p++)
  {
    // loading chemical names
    con = NULL;
    Series **xy = new Series*[numFluxTypes];
    for (m = 0; m<numFluxTypes; m++) xy[m] = NULL;

    for (i = 0; i<numCon; i++)
    {
      found = !rstrcmpi(cList[p], pCon[i]->cas);
      if (found)
        con = pCon[i];
      else
        for (j = 0; j<pCon[i]->numProg; j++)
        {
          found = !rstrcmpi(cList[p], pCon[i]->prog[j]->cas);
          if (found)
          {
            con = pCon[i]->prog[j];
            break;
          }
        }
      if (found)
      {
        for (m = 1; m<= numFluxTypes; m++)
        {
          inf->setpos(con->xypos);
          if (xy[m-1] == NULL)
          {
            xy[m-1] = new Series();
            xy[m-1]->Read(inf, con->numStep, m);
          }
          else
          {
            Series *other = new Series();
            other->Read(inf, con->numStep, m);
            xy[m-1]->Add(other);
            delete other;
          }
        }
      }
    }

    if (xy[0] == NULL)
      *eOut << "Not found" << cList[p] << "yr" << "?" << 0 << 0 << NewLn;
    else
    {

      ClearDecayFlags();
      GetDecayChain(p);
      // write out consituent and time series
      *eOut << con->name << cList[p] << con->tunit << con->vunit << xy[0]->count << (long)d3List.size() << NewLn;
      lcnt++;
      for (i = 0; i<xy[0]->count; i++)
      {
        *eOut << xy[0]->xValues[i];
        for (m = 0; m<numFluxTypes; m++)
          *eOut << (double)xy[m]->yValues[i];
        *eOut << NewLn;
        lcnt++;
      }
      // write out progeny list with no value pts
      last = p;
      for (i = 0; i< (long)d3List.size(); i++)
        for (j = 0; j< (long)cList.size(); j++)
          if (!rstrcmpi(d3List[i], cList[j]))
          {
            *eOut << nList[j] << cList[j] << con->tunit << con->vunit << 2 << nList[last] << cList[last] << NewLn;
            *eOut << 0;
            for (m = 0; m<numFluxTypes; m++)
              *eOut << 0.0;
            *eOut << NewLn << 1;
            for (m = 0; m<numFluxTypes; m++)
              *eOut << 0.0;
            *eOut << NewLn;
            last = j;
            lcnt+=2;
            break;
          }
    }
    for (m = 0; m<numFluxTypes; m++) delete xy[m];
    delete[] xy;
  }

  CloseDecayChain();
  return lcnt;
}

void AFF::Init()
{
  inf = NULL;
  set = NULL;
  numSet = 0;
}

AFF::AFF()
{
  Init();
}

AFF::AFF(char *fuiname, char *modId)
{
  char dummy[MAXPATH];

  Init();
  rstrcpy(name,modId);
  sprintf(dummy,"%s.aff",fuiname);
  inf = new icsv(dummy,'\"');
  if (!inf->ok()) return;
  if (0 != inf->SeekSection(modId,&affHead)) return;
  *inf >> numSet >> NewLn;
  set = new AFFSet*[numSet];
  for (int i = 0; i<numSet; i++)
    set[i] = new AFFSet(inf);
}

AFF::~AFF()
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
// AFF API ---------------------------------------------------------------------
//------------------------------------------------------------------------------
void FRAMES_API affOpen(char *fuiname, char *modId)
{
  affClose();
  aff = new AFF(fuiname, modId);
}

void FRAMES_API affClose()
{
  if (aff) delete aff;
  aff = NULL;
}

int FRAMES_API affGetNumRunInfo()
{
  int num;
  if (aff == NULL) return 0;
  if (aff->numSet<= 0) return 0;
  aff->inf->setpos(aff->affHead);
  *aff->inf >> num >> NewLn;
  return num;
}

int FRAMES_API affGetRunInfo(int Idx, char *info)
{
  int num = affGetNumRunInfo();
  if (num == 0 || Idx>num) return 0;
  for (int i = 1; i<Idx; i++)
    *aff->inf >> NewLn;
  *aff->inf >> info >> NewLn;
  return 1;
}

int FRAMES_API affGetNumSets()
{
  if (!aff) return 0;
  if (!aff->set) return 0;
  return aff->numSet;
}

int FRAMES_API affGetSetInfo(int dsIdx, char *name, char *type)
{
  if (!aff) return 0;
  if (!aff->set) return 0;
  if (dsIdx<1 || dsIdx>aff->numSet) return 0;

  rstrcpy(name, aff->set[dsIdx-1]->name);
  rstrcpy(type, aff->set[dsIdx-1]->type);
  return 1;
}

int FRAMES_API affGetDimensions(int dsIdx, double *area, double *eht, double *aht, double *vel, double *etmp, double *atmp)
{
  if (!aff) return 0;
  if (!aff->set) return 0;
  if (dsIdx<1 || dsIdx>aff->numSet) return 0;
  *area = aff->set[dsIdx-1]->area;
  *eht = aff->set[dsIdx-1]->extHt;
  *aht = aff->set[dsIdx-1]->adjHt;
  *vel = aff->set[dsIdx-1]->extVel;
  *etmp = aff->set[dsIdx-1]->extTmp;
  *atmp = aff->set[dsIdx-1]->ambTmp;
  return 1;
}

int FRAMES_API affGetNumFluxTypes(int dsIdx)
{
  if (!aff) return 0;
  if (!aff->set) return 0;
  if (dsIdx<1 || dsIdx>aff->numSet) return 0;
  return aff->set[dsIdx-1]->numFluxTypes;
}

int FRAMES_API affGetFluxType(int dsIdx, int nflux, char *type, double *size, char *sunt, double *density, char *dunt )
{
  int i;
  if (!aff) return 0;
  if (!aff->set) return 0;
  if (dsIdx<1 || dsIdx>aff->numSet) return 0;
  aff->inf->setpos(aff->set[dsIdx-1]->filepos);
  for (i = 0; i<9; i++) *aff->inf >> NewLn;
  for (i = 0; i<nflux; i++) *aff->inf >> type >> *size >> sunt >> *density >> dunt >> NewLn;
  return 1;
}

int FRAMES_API affGetNumContam(int dsIdx)
{
  if (!aff) return 0;
  if (!aff->set) return 0;
  if (dsIdx<1 || dsIdx>aff->numSet) return 0;
  return aff->set[dsIdx-1]->numCon;
}

int FRAMES_API affGetNumProgeny(int dsIdx, int conIdx)
{
  if (!aff) return 0;
  if (!aff->set) return 0;
  if (dsIdx<1 || dsIdx>aff->numSet) return 0;
  AFFSet *set = aff->set[dsIdx-1];
  if (conIdx<1 || conIdx>set->numCon) return 0;
  return set->pCon[conIdx-1]->numProg;
}

int FRAMES_API affGetContamName(int dsIdx, int conIdx, int progIdx, char *name, char *cas)
{
  if (!aff) return 0;
  if (!aff->set) return 0;
  if (dsIdx<1 || dsIdx>aff->numSet) return 0;
  AFFSet *set = aff->set[dsIdx-1];
  if (conIdx<1 || conIdx>set->numCon) return 0;
  AFFCon *con = set->pCon[conIdx-1];
  if (progIdx>con->numProg) return 0;
  if (progIdx>0) con = con->prog[progIdx-1];

  rstrcpy(name, con->name);
  rstrcpy(cas, con->cas);
  return 1;
}

int FRAMES_API affGetSeriesProperties(int dsIdx, int conIdx, int progIdx, char *fluxunits, char *timeunits)
{
  int count = 0;
  char tmp1[SMALLSTRING];

  if (!aff) return 0;
  if (!aff->set) return 0;
  if (dsIdx<1 || dsIdx>aff->numSet) return 0;
  AFFSet *set = aff->set[dsIdx-1];
  if (conIdx<1 || conIdx>set->numCon) return 0;
  AFFCon *con = set->pCon[conIdx-1];
  if (progIdx>con->numProg) return 0;
  if (progIdx>0) con = con->prog[progIdx-1];

  aff->inf->setpos(con->filepos);
  *aff->inf >> tmp1 >> tmp1 >> timeunits >> fluxunits >> count >> NewLn;
  return count;
}

int FRAMES_API affGetSeriesValues(int dsIdx, int conIdx, int progIdx, int fluxIdx, int count, double *times, double *values)
{
  char tmp[SMALLSTRING];
  int cnt = affGetSeriesProperties(dsIdx, conIdx, progIdx, tmp, tmp);
  if (count < cnt) return 0;
  for (int i = 0; i<cnt; i++)
  {
    *aff->inf >> times[i];
    for (int j = 0; j<fluxIdx; j++) *aff->inf >> values[i];
    *aff->inf >> NewLn;
  }
  return cnt;
}

void FRAMES_API affAggregate(char *filename, char *casList)
{
  int i;
  char *dummy = new char[MAXPATH];

  if (aff == NULL) return;
  ocsv *eOut = new ocsv(filename, '"', ',', _CREATE_);
  eOut->alwaysQuote();
  *eOut << (affGetNumRunInfo()+1) << NewLn;
  *eOut << "This file was modified by wrapspec.exe /out" << NewLn;
  for (i = 0; i<affGetNumRunInfo(); i++)
  {
    affGetRunInfo(i+1, dummy);
    *eOut << dummy << NewLn;
  }
  *eOut << aff->numSet << NewLn;
  for (i = 0; i<aff->numSet; i++)
    aff->set[i]->convertAFF(aff->inf, eOut, casList);

  delete eOut;
  delete [] dummy;
  return;
}

void FRAMES_API affInsert(char *filename, char *casList, char *nameList, char *degList, char *secList, int branching)
{
  int i;
  int cnt;
  long pos;
  char *dummy = new char[MAXPATH];

  if (aff == NULL) return;
  ocsv *eOut;
  if (0 == access(filename, 0))
    eOut = new ocsv(filename, '"', ',', _APPEND_);
  else
    eOut = new ocsv(filename, '"', ',', _CREATE_);
  eOut->alwaysQuote();
  *eOut << aff->name;
  pos = eOut->getpos();
  eOut->smartQuote();
  *eOut << "          " << NewLn;
  eOut->alwaysQuote();
  *eOut << (affGetNumRunInfo()+1) << NewLn;
  *eOut << "This file was modified by wrapspec.exe /in" << NewLn;
  cnt = 2;
  for (i = 0; i<affGetNumRunInfo(); i++)
  {
    affGetRunInfo(i+1,dummy);
    *eOut << dummy << NewLn;
    cnt++;
  }
  *eOut << aff->numSet << NewLn;
  cnt++;
  for (i = 0; i<aff->numSet; i++)
    cnt += aff->set[i]->unconvertAFF(aff->inf, eOut, casList, nameList, degList, secList, branching);

  eOut->setpos(pos);
  eOut->delim = ' ';
  *eOut << cnt;

  delete eOut;
  delete [] dummy;
  return;
}

