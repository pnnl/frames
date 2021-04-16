/*______________________________________________________________________________

  Date:       1993 - 2004
  Company:    Pacific Northwest National Laboratories
  Battelle Corporation
  ________________________________________________________________________________
  __Modifiication  History________________________________________________________

    DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#include "frames.h"
#include "wcfClass.h"

static WCF *wcf = NULL;

void WCFCon::Init()
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
  xy = NULL;
}

WCFCon::WCFCon()
{ Init(); }

WCFCon::WCFCon (icsv *inf, bool isProg = false)
{
  Init();
  filepos = inf->getpos();
  *inf >> name >> cas >> tunit >> vunit >> numStep;
  if (isProg)      *inf >> NewLn;
  else             *inf >> numProg >> NewLn;
  xypos = inf->getpos();
  xy = new Series(numStep);
  xy->Read(inf, numStep);
  if (numProg>0)
  {
    prog = new WCFCon*[numProg];
    for (int i = 0; i<numProg; i++)
      prog[i] = new WCFCon(inf, true);
  }
}

WCFCon::WCFCon (char *_name, char *_cas, int count)
{
  Init();
  rstrcpy(name,_name);
  rstrcpy(cas,_cas);
  numProg = count;
  if (numProg>0) prog = new WCFCon*[numProg];
}

WCFCon::~WCFCon()
{
  for (int i = 0; i<numProg; i++)
  {
    if (prog[i])
      delete prog[i];
  }
  if (prog) delete[] prog;
  numProg = 0;
  prog = NULL;
  if (xy) delete xy;
  xy = NULL;
}

void WCFCon::setSeriesCount(int count)
{
  if (xy) delete xy;
  xy = new Series(count);
}

void WCFCon::setSeriesValue(int idx, double time, double value)
{
  if (!xy) return;
  xy->Replace(idx,time,value);
}

void WCFCon::wcfWrite(ocsv *outf)
{
  int i;
  *outf << name << cas << tunit << vunit << xy->count << numProg << NewLn;
  for (i = 0; i<xy->count; i++)
    *outf << xy->xValues[i] << (double)xy->yValues[i] << NewLn;
  for (i = 0; i<numProg; i++)
    prog[i]->wcfWrite(outf,name,cas);
}

void WCFCon::wcfWrite(ocsv *outf, char *pname, char *pcas)
{
  *outf << name << cas << tunit << vunit << xy->count << pname << pcas << NewLn;
  for (int j = 0; j<xy->count; j++)
    *outf << xy->xValues[j] << xy->yValues[j] << NewLn;
}

bool WCFCon::add(WCFCon *other)
{
  ///make sure name and qualifier match
  if (rstrcmpi(cas, other->cas)) return false;

  ///keeps track of if one of the adds failed
  bool failed = false;

  ///go through the progenies in the other object
  ///and add them all to this one.  if this one does
  ///not have a progeny from the other create one so
  //  hey can be added
  for (int i = 0; i < other->numProg; i++)
  {
    bool found = false;
    int j;

    ///search for the name/id in these datasets for a match
    for (j = 0; j < numProg; j++)
      if (!rstrcmpi(prog[j]->cas, other->prog[i]->cas))
      {
        found = true;
        break;
      }

      ///if these datasets don't match with the
      ///current dataset from other, create one
      if (!found)
      {
        WCFCon *newprog = other->prog[i]->copy();
        appendProgeny(newprog);
      }
      else
      {
        ///add the current progeny from other to
        //  he dataset that matches one of this
        ///classes progenies
        if (!prog[j]->add(other->prog[i]))
          failed = true;
      }
  }

  xy->Add(other->xy);
  return !failed;
}

void WCFCon::appendProgeny(WCFCon *progeny)
{
  WCFCon **temp = prog;

  prog = new WCFCon*[numProg+1];
  for (int i = 0; i < numProg; i++)
    prog[i] = temp[i];

  prog[numProg] = progeny;
  numProg++;

  delete [] temp;
}

WCFCon* WCFCon::copy()
{
  WCFCon *con = new WCFCon();
  rstrcpy(con->name, name);
  rstrcpy(con->cas, cas);
  rstrcpy(con->tunit, tunit);
  rstrcpy(con->vunit, vunit);
  con->filepos = filepos;
  con->xypos = xypos;
  con->numStep = numStep;
  if (xy) con->xy = xy->Clone();
  if (numProg>0)    //copy over all progenies
  {
    con->numProg = numProg;
    prog = new WCFCon*[numProg];
    for (int i = 0; i < numProg; i++)
      con->prog[i] = prog[i]->copy();
  }
  return con;
}

void WCFSet::Init()
{
  filepos = 0;
  rstrcpy(name,"");
  rstrcpy(type,"");
  numCon = 0;
  pCon = NULL;
  x = 0.0;
  y = 0.0;
  z = 0.0;
}

WCFSet::WCFSet ()
{
  Init();
}

WCFSet::WCFSet (icsv *inf)
{
  Init();
  filepos = inf->getpos();
  *inf >> name >> type >> numCon >> x >> dummy >> y >> dummy >> z >> dummy  >> NewLn;
  pCon = new WCFCon*[numCon];
  for (int i = 0; i<numCon; i++)
    pCon[i] = new WCFCon(inf);
}

WCFSet::WCFSet (char *lname, char *ltype)
{
  Init();
  rstrcpy(name,lname);
  rstrcpy(type,ltype);
}

WCFSet::~WCFSet()
{
  if (pCon)
  {
    for (int i = 0; i<numCon; i++)
      if (pCon[i]) delete pCon[i];
      delete[] pCon;
  }
  Init();
}

void WCFSet::setNumCon(int count)
{
  if (pCon)
  {
    for (int i = 0; i<numCon; i++)
      if (pCon[i]) delete pCon[i];
      delete[] pCon;
  }
  Init();
  numCon = count;
  if (numCon>0)
    pCon = new WCFCon*[numCon];
}

void WCFSet::setContaminant(int conIdx, char *cname, char *cas, int progIdx)
{
  if (!pCon) return;
  if (conIdx<numCon)
    pCon[conIdx] = new WCFCon(cname,cas,progIdx);
}

void WCFSet::setSeriesCount(int conIdx, int count)
{
  if (!pCon) return;
  if (conIdx<numCon)
  {
    if (!pCon[conIdx]) return;
    pCon[conIdx]->setSeriesCount(count);
  }
}

void WCFSet::setSeriesValue(int conIdx, int idx, double time, double value)
{
  if (!pCon) return;
  if (conIdx<numCon)
  {
    if (!pCon[conIdx]) return;
    pCon[conIdx]->setSeriesValue(idx,time,value);
  }
}

bool WCFSet::add(WCFSet *other)
{
  ///make sure name and qualifier match
  if (rstrcmpi(type, other->type))  return false;

  ///keeps track of if one of the adds failed
  bool failed = false;

  ///go through the constituents in the other object
  ///and add them all to this one.  if this one does
  ///not have a constituent from the other create
  ///one so they can be added
  for (int i = 0; i < other->numCon; i++)
  {
    bool found = false;
    int j;

    ///search for the name/id in these datasets for a match
    for (j = 0; j < numCon; j++)
      if (!rstrcmpi(pCon[j]->name, other->pCon[i]->name) &&
         !rstrcmpi(pCon[j]->cas,  other->pCon[i]->cas))
      {
        found = true;
        break;
      }

      ///if these datasets don't match with the
      ///current dataset from other, create one
      if (!found)
      {
        WCFCon * newcon = other->pCon[i]->copy();
        append(newcon);
      }
      else
      {
        // add the current dataset from other to the dataset
        // that matches one of this classes datasets
        if (!pCon[j]->add(other->pCon[i]))
          failed = true;
      }
  }
  return !failed;
}

void WCFSet::append(WCFCon *con)
{
  WCFCon **temp = pCon;

  pCon = new WCFCon*[numCon+1];
  for (int i = 0; i < numCon; i++)
    pCon[i] = temp[i];

  pCon[numCon] = con;
  numCon++;

  delete [] temp;
}

void WCFSet::wcfWrite(ocsv *outf)
{
  *outf << name << type << numCon
        << x << "m" << y << "m" << z << "m" << NewLn;
  for (int i = 0; i<numCon; i++)
    pCon[i]->wcfWrite(outf);
}

WCFSet* WCFSet::copy()
{
  WCFSet *newloc = new WCFSet(name, type);
  newloc->filepos = filepos;
  newloc->numCon = numCon;
  newloc->x = x;
  newloc->y = y;
  newloc->z = z;
  rstrcpy(newloc->name, name);
  rstrcpy(newloc->type, type);
  ///copy over all constituents
  if (numCon > 0) newloc->pCon = new WCFCon*[numCon];
  for (int i = 0; i < numCon; i++)
    newloc->pCon[i] = pCon[i]->copy();
  return newloc;
}

int WCFSet::convertWCF(icsv *inf, ocsv *eOut, char *casList)
{
  char *mylist = strdup(casList);
  WCFCon *con;
  bool found;

  splitList(mylist);
  // dataset info
  *eOut << name << type << (long)rList.size()
        << x << "m" << y << "m" << z << "m" << NewLn;

  for (int k = 0; k< (long)rList.size(); k++)
  {
    con = NULL;
    Series *xy = NULL;
    for (int i = 0; i<numCon; i++)
    {
      found = !rstrcmpi(rList[k], pCon[i]->cas);
      if (found)
        con = pCon[i];
      else
        for (int j = 0; j<pCon[i]->numProg; j++)
        {
          found = !rstrcmpi(rList[k], pCon[i]->prog[j]->cas);
          if (found)
          {
            con = pCon[i]->prog[j];
            break;
          }
        }
      if (found)
      {
        inf->setpos(con->xypos);
        if (xy == NULL)
        {
          xy = new Series();
          xy->Read(inf, con->numStep);
        }
        else
        {
          Series *other = new Series();
          other->Read(inf, con->numStep);
          xy->Add(other);
          delete other;
        }
      }
    }

    if (xy == NULL)
      *eOut << "Not found" << rList[k] << "yr" << "?" << 0 << 0 << NewLn;
    else
    {
      *eOut << con->name << rList[k] << con->tunit << con->vunit << xy->count << 0 << NewLn;
      for (int i = 0; i<xy->count; i++)
        *eOut << xy->xValues[i] << (double)xy->yValues[i] << NewLn;
    }
    delete xy;
  }

  delete mylist;
  return 0;
}

// this routine will add liked named parents in the caslist
// then write them with degradation product that have zero time steps
// ???List are comma seperated and should be one to one with cas as primary key
// all degradation products must be listed as parents if not they are ignored
int WCFSet::unconvertWCF(icsv *inf, ocsv *eOut, char *casList, char *nameList, char *degList, char *secList, int branching)
{
  int i, j, p, last, lcnt;
  WCFCon *con;
  bool found;

  OpenDecayChain(casList, nameList, degList, secList, branching);

  // dataset info
  *eOut << name << type << (long)cList.size()
        << x << "m" << y << "m" << z << "m" << NewLn;
  lcnt = 1;

  for (p = 0; p< (long)cList.size(); p++)
  {
    // loading chemical names
    con = NULL;
    Series *xy = NULL;

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
        inf->setpos(con->xypos);
        if (xy == NULL)
        {
          xy = new Series();
          xy->Read(inf, con->numStep, 1);
        }
        else
        {
          Series *other = new Series();
          other->Read(inf, con->numStep, 1);
          xy->Add(other);
          delete other;
        }
      }
    }

    if (xy == NULL)
      *eOut << "Not found" << cList[p] << "yr" << "?" << 0 << 0 << NewLn;
    else
    {

      ClearDecayFlags();
      GetDecayChain(p);
      // write out consituent and time series
      *eOut << con->name << cList[p] << con->tunit << con->vunit << xy->count << (long)d3List.size() << NewLn;
      lcnt++;
      for (i = 0; i<xy->count; i++)
      {
        *eOut << xy->xValues[i] << (double)xy->yValues[i] << NewLn;
        lcnt++;
      }
      // write out progeny list with no time pts
      last = p;
      for (i = 0; i< (long)d3List.size(); i++)
        for (j = 0; j< (long)cList.size(); j++)
          if (!rstrcmpi(d3List[i], cList[j]))
          {
            *eOut << nList[j] << cList[j] << con->tunit << con->vunit << 2 << nList[last] << cList[last] << NewLn;
            *eOut << 0 << 0.0 << NewLn;
            *eOut << 1 << 0.0 << NewLn;
            last = j;
            lcnt+=3;
            break;
          }
    }
    delete xy;
  }

  CloseDecayChain();
  return lcnt;
}

void WCF::Init()
{
  inf = NULL;
  set = NULL;
  numSet = 0;
}

WCF::WCF()
{
  Init();
}

WCF::WCF(char *fuiname, char *modId)
{
  char dummy[MAXPATH];

  Init();
  rstrcpy(name,modId);
  sprintf(dummy,"%s.wcf",fuiname);
  inf = new icsv(dummy,'\"');
  if (!inf->ok()) return;
  if (0 != inf->SeekSection(modId,&wcfHead)) return;
  *inf >> numSet >> NewLn;
  set = new WCFSet*[numSet];
  for (int i = 0; i<numSet; i++)
    set[i] = new WCFSet(inf);
}

WCF::~WCF()
{
  if (set)
  {
    for (int i = 0; i<numSet; i++)
      if (set[i]) delete set[i];
      delete[] set;
  }
  if (inf) delete inf;
  Init();
}

void WCF::setName(char *lname)
{
  for (int i = 0; i<numSet; i++)
    rstrcpy(set[i]->name,lname);
}

void WCF::setNumSet(int count)
{
  if (numSet>0)
  {
    for (int i = 0; i<numSet; i++)
      delete set[i];
    delete[] set;
  }
  set = NULL;
  if (count>0)
  {
    set = new WCFSet*[count];
    numSet = count;
  }
}

void WCF::setLocation(int dsIdx, char *lname, char *ltype)
{
  if (!set) return;
  set[dsIdx] = new WCFSet(lname,ltype);
}

void WCF::setNumCon(int dsIdx, int count)
{
  if (!set) return;
  if (!set[dsIdx]) return;
  set[dsIdx]->setNumCon(count);
}

void WCF::setContaminant(int dsIdx, int conIdx, char *cname, char *cas, int progIdx)
{
  if (!set) return;
  if (!set[dsIdx]) return;
  set[dsIdx]->setContaminant(conIdx, cname, cas, progIdx);
}

void WCF::setSeriesCount(int dsIdx, int conIdx, int count)
{
  if (!set) return;
  if (!set[dsIdx]) return;
  set[dsIdx]->setSeriesCount(conIdx, count);
}

void WCF::setSeriesValue(int dsIdx, int conIdx, int idx, double time, double value)
{
  if (!set) return;
  if (!set[dsIdx]) return;
  set[dsIdx]->setSeriesValue(conIdx, idx, time, value);
}

bool WCF::add(WCF *other, char *id)
{
  char  msg[SMALLSTRING];
  ///go through the datasets in the other object
  ///and add them all to this one.  if this one
  ///doesn't have a dataset from the other,
  ///create so that they can be added
  for (int i = 0; i < other->numSet; i++)
  {
    int j;
    bool typematch;
    ///search for the name/qualifier in these datasets for a match
    if (!rstrcmpi(other->set[i]->name,id) || !rstrcmpi("all", other->set[i]->name))
    {
      typematch = false;
      for (j = 0; j < numSet; j++)
      {
        // add only qualifier type matches
        if (!rstrcmpi(set[j]->type, other->set[i]->type))
        {
          typematch = true;
          if (!set[j]->add(other->set[i]))
          {
            sprintf(msg,"%s.%s and %s.%s locations failed to add!",
                      name, set[j]->name, other->name, other->set[i]->name);
            writeError(msg);
            return false;
          }
        }
      }
      // if these datasets don't match with the
      // current dataset and id from other, create one
      // must be another type at this point
      if (!typematch)
      {
        WCFSet *newmedialoc = other->set[i]->copy();
        append(newmedialoc);
      }
    }
    //else no name match disregrard location
  }
  return true;
}

void WCF::wcfWrite (char *tmpname)
{
  char dummy[MAXPATH];
  ocsv *outf;

  sprintf(dummy,"%s.wcf",tmpname);
  outf = new ocsv(dummy,'\"');
  if (!outf->ok()) return;
  outf->alwaysQuote();
  *outf << 1 << NewLn << "File created with WCF I/O tools" << NewLn;
  *outf << numSet << NewLn;
  for (int i = 0; i<numSet; i++)
    if (set[i]) set[i]->wcfWrite(outf);
    delete outf;
}

void WCF::append(WCFSet *loc)
{
  WCFSet **temp = set;

  set = new WCFSet*[numSet+1];
  for (int i = 0; i < numSet; i++)
    set[i] = temp[i];

  set[numSet] = loc;
  numSet++;

  delete [] temp;
}

//------------------------------------------------------------------------------
// WCF API ---------------------------------------------------------------------
//------------------------------------------------------------------------------

void FRAMES_API wcfOpen(char *fuiname, char *modId)
{
  wcfClose();
  wcf = new WCF(fuiname, modId);
}

void FRAMES_API wcfClose()
{
  if (wcf) delete wcf;
  wcf = NULL;
}

int FRAMES_API wcfGetNumRunInfo()
{
  int num;
  if (wcf == NULL) return 0;
  if (wcf->numSet<= 0) return 0;
  wcf->inf->setpos(wcf->wcfHead);
  *wcf->inf >> num >> NewLn;
  return num;
}

int FRAMES_API wcfGetRunInfo(int Idx, char *info)
{
  int num = wcfGetNumRunInfo();
  if (num == 0 || Idx>num) return 0;
  for (int i = 1; i<Idx; i++)
    *wcf->inf >> NewLn;
  *wcf->inf >> info >> NewLn;
  return 1;
}

int FRAMES_API wcfGetNumSets()
{
  if (!wcf) return 0;
  if (!wcf->set) return 0;
  return wcf->numSet;
}

int FRAMES_API wcfGetSetInfo(int dsIdx, char *name, char *type)
{
  if (!wcf) return 0;
  if (!wcf->set) return 0;
  if (dsIdx<1 || dsIdx>wcf->numSet) return 0;
  rstrcpy(name, wcf->set[dsIdx-1]->name);
  rstrcpy(type, wcf->set[dsIdx-1]->type);
  return 1;
}

int FRAMES_API wcfGetLocation(int dsIdx, double *x, double *y, double *z)
{
  if (!wcf) return 0;
  if (!wcf->set) return 0;
  if (dsIdx<1 || dsIdx>wcf->numSet) return 0;
  *x = wcf->set[dsIdx-1]->x;
  *y = wcf->set[dsIdx-1]->y;
  *z = wcf->set[dsIdx-1]->z;
  return 1;
}

int FRAMES_API wcfGetNumContam(int dsIdx)
{
  if (!wcf) return 0;
  if (!wcf->set) return 0;
  if (dsIdx<1 || dsIdx>wcf->numSet) return 0;
  return wcf->set[dsIdx-1]->numCon;
}

int FRAMES_API wcfGetNumProgeny(int dsIdx, int conIdx)
{
  if (!wcf) return 0;
  if (!wcf->set) return 0;
  if (dsIdx<1 || dsIdx>wcf->numSet) return 0;
  WCFSet *set = wcf->set[dsIdx-1];
  if (conIdx<1 || conIdx>set->numCon) return 0;
  return set->pCon[conIdx-1]->numProg;
}

int FRAMES_API wcfGetContamName(int dsIdx, int conIdx, int progIdx, char *name, char *cas)
{
  if (!wcf) return 0;
  if (!wcf->set) return 0;
  if (dsIdx<1 || dsIdx>wcf->numSet) return 0;
  WCFSet *set = wcf->set[dsIdx-1];
  if (conIdx<1 || conIdx>set->numCon) return 0;
  WCFCon *con = set->pCon[conIdx-1];
  if (progIdx>con->numProg) return 0;
  if (progIdx>0) con = con->prog[progIdx-1];

  rstrcpy(name, con->name);
  rstrcpy(cas, con->cas);
  return 1;
}

int FRAMES_API wcfGetSeriesProperties(int dsIdx, int conIdx, int progIdx, char *concunits, char *timeunits)
{
  int count = 0;
  char tmp[SMALLSTRING];

  if (!wcf) return 0;
  if (!wcf->set) return 0;
  if (dsIdx<1 || dsIdx>wcf->numSet) return 0;
  WCFSet *set = wcf->set[dsIdx-1];
  if (conIdx<1 || conIdx>set->numCon) return 0;
  WCFCon *con = set->pCon[conIdx-1];
  if (progIdx>con->numProg) return 0;
  if (progIdx>0) con = con->prog[progIdx-1];

  wcf->inf->setpos(con->filepos);
  *wcf->inf >> tmp >> tmp >> timeunits >> concunits >> count >> NewLn;
  return count;
}

int FRAMES_API wcfGetSeriesValues(int dsIdx, int conIdx, int progIdx, int count, double *times, double *values)
{
  char tmp[SMALLSTRING];
  int cnt = wcfGetSeriesProperties(dsIdx, conIdx, progIdx, tmp, tmp);
  if (count < cnt) return 0;
  for (int i = 0; i<cnt; i++)
    *wcf->inf >> times[i] >> values[i] >> NewLn;
  return cnt;
}

void FRAMES_API wcfAggregate(char *filename, char* casList)
{
  int i;
  char *dummy = new char[MAXPATH];

  if (wcf == NULL) return;
  ocsv *eOut = new ocsv(filename, '"', ',' , _CREATE_);
  eOut->alwaysQuote();
  *eOut << (wcfGetNumRunInfo()+1) << NewLn;
  *eOut << "This file was modified by wrapspec.exe /out" << NewLn;
  for (i = 0; i<wcfGetNumRunInfo(); i++)
  {
    wcfGetRunInfo(i+1, dummy);
    *eOut << dummy << NewLn;
  }
  *eOut << wcf->numSet << NewLn;
  for (i = 0; i<wcf->numSet; i++)
    wcf->set[i]->convertWCF(wcf->inf, eOut, casList);

  delete eOut;
  delete [] dummy;
  return;
}

void FRAMES_API wcfInsert(char *filename, char *casList, char *nameList, char *degList, char *secList, int branching)
{
  int i;
  int cnt;
  long pos;
  char *dummy = new char[MAXPATH];

  if (wcf == NULL) return;
  ocsv *eOut;
  if (0 == access(filename, 0))
    eOut = new ocsv(filename, '"', ',', _APPEND_);
  else
    eOut = new ocsv(filename, '"', ',', _CREATE_);
  eOut->alwaysQuote();
  *eOut << wcf->name;
  pos = eOut->getpos();
  eOut->smartQuote();
  *eOut << "          " << NewLn;
  eOut->alwaysQuote();
  *eOut << (wcfGetNumRunInfo()+1) << NewLn;
  *eOut << "This file was modified by wrapspec.exe /in" << NewLn;
  cnt = 2;
  for (i = 0; i<wcfGetNumRunInfo(); i++)
  {
    wcfGetRunInfo(i+1,dummy);
    *eOut << dummy << NewLn;
    cnt++;
  }
  *eOut << wcf->numSet << NewLn;
  cnt++;
  for (i = 0; i<wcf->numSet; i++)
    cnt += wcf->set[i]->unconvertWCF(wcf->inf, eOut, casList, nameList, degList, secList, branching);

  eOut->setpos(pos);
  eOut->delim = ' ';
  *eOut << cnt;

  delete eOut;
  delete [] dummy;
  return;
}

