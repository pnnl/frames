/*______________________________________________________________________________

  Date:       1993 - 2004
  Company:    Pacific Northwest National Laboratories
  Battelle Corporation
  ________________________________________________________________________________
  __Modifiication  History________________________________________________________

    DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#include "frames.h"
#include "scfClass.h"

static SCF *scf = NULL;

void SCFCon::Init()
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

SCFCon::SCFCon()
{ Init(); }

SCFCon::SCFCon (icsv *inf, bool isProg = false)
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
    prog = new SCFCon*[numProg];
    for (int i = 0; i<numProg; i++)
      prog[i] = new SCFCon(inf, true);
  }
}

SCFCon::SCFCon (char *_name, char *_cas, int count)
{
  Init();
  rstrcpy(name,_name);
  rstrcpy(cas,_cas);
  numProg = count;
  if (numProg>0) prog = new SCFCon*[numProg];
}

SCFCon::~SCFCon()
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

void SCFCon::setSeriesCount(int count)
{
  if (xy) delete xy;
  xy = new Series(count);
}

void SCFCon::setSeriesValue(int idx, double time, double value)
{
  if (!xy) return;
  xy->Replace(idx,time,value);
}

void SCFCon::scfWrite(ocsv *outf)
{
  int i;
  *outf << name << cas << tunit << vunit << xy->count << numProg << NewLn;
  for (i = 0; i<xy->count; i++)
    *outf << xy->xValues[i] << (double)xy->yValues[i] << NewLn;
  for (i = 0; i<numProg; i++)
    prog[i]->scfWrite(outf,name,cas);
}

void SCFCon::scfWrite(ocsv *outf, char *parentname, char *parentcas)
{
  *outf << name << cas << tunit << vunit << xy->count << parentname << parentcas << NewLn;
  for (int j = 0; j<xy->count; j++)
    *outf << xy->xValues[j] << xy->yValues[j] << NewLn;
}

bool SCFCon::add(SCFCon *other)
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
      if (rstrcmpi(prog[j]->cas, other->prog[i]->cas))
      {
        found = true;
        break;
      }

      ///if these datasets don't match with the
      ///current dataset from other, create one
      if (!found)
      {
        SCFCon *newprog = other->prog[i]->copy();
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

void SCFCon::appendProgeny(SCFCon *progeny)
{
  SCFCon **temp = prog;

  prog = new SCFCon*[numProg+1];
  for (int i = 0; i < numProg; i++)
    prog[i] = temp[i];

  prog[numProg] = progeny;
  numProg++;

  delete [] temp;
}

SCFCon* SCFCon::copy()
{
  SCFCon *con = new SCFCon();
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
    prog = new SCFCon*[numProg];
    for (int i = 0; i < numProg; i++)
      con->prog[i] = prog[i]->copy();
  }
  return con;
}

void SCFSet::Init()
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

SCFSet::SCFSet()
{
  Init();
}

SCFSet::SCFSet (icsv *inf)
{
  Init();
  filepos = inf->getpos();
  *inf >> name >> type >> x >> dummy >> y >> dummy >> z >> dummy >> numCon >> NewLn;
  pCon = new SCFCon*[numCon];
  for (int i = 0; i<numCon; i++)
    pCon[i] = new SCFCon(inf);
}

SCFSet::SCFSet (char *lname, char *ltype)
{
  Init();
  rstrcpy(name,lname);
  rstrcpy(type,ltype);
}

SCFSet::~SCFSet()
{
  if (pCon)
  {
    for (int i = 0; i<numCon; i++)
      if (pCon[i]) delete pCon[i];
      delete[] pCon;
  }
  Init();
}

void SCFSet::setNumCon(int count)
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
    pCon = new SCFCon*[numCon];
}

void SCFSet::setContaminant(int conIdx, char *cname, char *cas, int progIdx)
{
  if (!pCon) return;
  if (conIdx<numCon)
    pCon[conIdx] = new SCFCon(cname,cas,progIdx);
}

void SCFSet::setSeriesCount(int conIdx, int count)
{
  if (!pCon) return;
  if (conIdx<numCon)
  {
    if (!pCon[conIdx]) return;
    pCon[conIdx]->setSeriesCount(count);
  }
}

void SCFSet::setSeriesValue(int conIdx, int idx, double time, double value)
{
  if (!pCon) return;
  if (conIdx<numCon)
  {
    if (!pCon[conIdx]) return;
    pCon[conIdx]->setSeriesValue(idx,time,value);
  }
}

bool SCFSet::add(SCFSet *other)
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
        SCFCon * newcon = other->pCon[i]->copy();
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

void SCFSet::append(SCFCon *con)
{
  SCFCon **temp = pCon;

  pCon = new SCFCon*[numCon+1];
  for (int i = 0; i < numCon; i++)
    pCon[i] = temp[i];

  pCon[numCon] = con;
  numCon++;

  delete [] temp;
}

void SCFSet::scfWrite(ocsv *outf)
{
  *outf << name << type <<  x << "m" << y << "m" << z << "m"
        << numCon << cx << "m" << cy << "m" << cz << "m" << NewLn;
  for (int i = 0; i<numCon; i++)
    pCon[i]->scfWrite(outf);
}

SCFSet* SCFSet::copy()
{
  SCFSet *newloc = new SCFSet();
  newloc->filepos = filepos;
  newloc->numCon = numCon;
  newloc->x = x;
  newloc->y = y;
  newloc->z = z;
  rstrcpy(newloc->name, name);
  rstrcpy(newloc->type, type);
  ///copy over all constituents
  if (numCon > 0) newloc->pCon = new SCFCon*[numCon];
  for (int i = 0; i < numCon; i++)
    newloc->pCon[i] = pCon[i]->copy();
  return newloc;
}

int SCFSet::convertSCF(icsv *inf, ocsv *eOut, char *casList)
{
  char *mylist = strdup(casList);
  SCFCon *con;
  bool found;

  splitList(mylist);
  // dataset info
  *eOut << name << type <<  x << "m" << y << "m" << z << "m"
        << (long)rList.size() << cx << "m" << cy << "m" << cz << "m" << NewLn;

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
int SCFSet::unconvertSCF(icsv *inf, ocsv *eOut, char *casList, char *nameList, char *degList, char *secList, int branching)
{
  int i, j, p, last, lcnt;
  SCFCon *con;
  bool found;

  OpenDecayChain(casList, nameList, degList, secList, branching);

  // dataset info
  *eOut << name << type << x << "m" << y << "m" << z << "m"
        << (long)cList.size() << cx << "m" << cy << "m" << cz << "m" << NewLn;
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

void SCF::Init()
{
  inf = NULL;
  set = NULL;
  numSet = 0;
}

SCF::SCF()
{
  Init();
}

SCF::SCF(char *fuiname, char *modId)
{
  char dummy[MAXPATH];

  Init();
  rstrcpy(name,modId);
  sprintf(dummy,"%s.scf",fuiname);
  inf = new icsv(dummy,'\"');
  if (!inf->ok()) return;
  if (0 != inf->SeekSection(modId,&scfHead)) return;
  *inf >> numSet >> NewLn;
  set = new SCFSet*[numSet];
  for (int i = 0; i<numSet; i++)
    set[i] = new SCFSet(inf);
}

SCF::~SCF()
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

void SCF::setName(char *lname)
{
  for (int i = 0; i<numSet; i++)
    rstrcpy(set[i]->name,lname);
}

void SCF::setNumSet(int count)
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
    set = new SCFSet*[count];
    numSet = count;
  }
}

void SCF::setLocation(int dsIdx, char *lname, char *ltype)
{
  if (!set) return;
  set[dsIdx] = new SCFSet(lname,ltype);
}

void SCF::setNumCon(int dsIdx, int count)
{
  if (!set) return;
  if (!set[dsIdx]) return;
  set[dsIdx]->setNumCon(count);
}

void SCF::setContaminant(int dsIdx, int conIdx, char *name, char *cas, int progIdx)
{
  if (!set) return;
  if (!set[dsIdx]) return;
  set[dsIdx]->setContaminant(conIdx, name, cas, progIdx);
}

void SCF::setSeriesCount(int dsIdx, int conIdx, int count)
{
  if (!set) return;
  if (!set[dsIdx]) return;
  set[dsIdx]->setSeriesCount(conIdx, count);
}

void SCF::setSeriesValue(int dsIdx, int conIdx, int idx, double time, double value)
{
  if (!set) return;
  if (!set[dsIdx]) return;
  set[dsIdx]->setSeriesValue(conIdx, idx, time, value);
}

bool SCF::add(SCF *other, char *id)
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
        double vol1 = set[j]->x * set[j]->y * set[j]->z;
        double vol2 = other->set[i]->x * other->set[i]->y * other->set[i]->z;
        // ********** highly unlikely as coded
        // if both name(location) and
        // type(soil, sediment, ...) and volumes the same, add,
        // else error
        if (!rstrcmpi(set[j]->type, other->set[i]->type) && (vol1 == vol2))
        {
          typematch = true;
          if (!set[j]->add(other->set[i]))
          {
            sprintf(msg,"%s and %s locations failed to add!",
                     set[j]->name, other->set[i]->name);
            writeError(msg);
            return false;
          }
        }
      }
      ///if these datasets don't match with the
      ///current dataset from other, create one
      // must be another type at this point
      if (!typematch)
      {
        SCFSet *newmedialoc = other->set[i]->copy();
        append(newmedialoc);
      }
    }
    //else no name match disregrard location
  }
  return true;
}

void SCF::scfWrite(char *tmpname)
{
  char dummy[MAXPATH];
  ocsv *outf;

  sprintf(dummy,"%s.scf",tmpname);
  outf = new ocsv(dummy,'\"');
  if (!outf->ok()) return;
  outf->alwaysQuote();
  *outf << 1 << NewLn << "File created with SCF I/O tools" << NewLn;
  *outf << numSet << NewLn;
  for (int i = 0; i<numSet; i++)
    if (set[i]) set[i]->scfWrite(outf);
    delete outf;
}

void SCF::append(SCFSet *loc)
{
  SCFSet **temp = set;

  set = new SCFSet*[numSet+1];
  for (int i = 0; i < numSet; i++)
    set[i] = temp[i];

  set[numSet] = loc;
  numSet++;

  delete [] temp;
}

//------------------------------------------------------------------------------
// SCF API ---------------------------------------------------------------------
//------------------------------------------------------------------------------

void FRAMES_API scfOpen(char *fuiname, char *modId)
{
  scfClose();
  scf = new SCF(fuiname,modId);
}

void FRAMES_API scfClose()
{
  if (scf) delete scf;
  scf = NULL;
}

int FRAMES_API scfGetNumRunInfo()
{
  int num;
  if (scf == NULL) return 0;
  if (scf->numSet<= 0) return 0;
  scf->inf->setpos(scf->scfHead);
  *scf->inf >> num >> NewLn;
  return num;
}

int FRAMES_API scfGetRunInfo(int Idx, char *info)
{
  int num = scfGetNumRunInfo();
  if (num == 0 || Idx>num) return 0;
  for (int i = 1; i<Idx; i++)
    *scf->inf >> NewLn;
  *scf->inf >> info >> NewLn;
  return 1;
}

int FRAMES_API scfGetNumSets()
{
  if (!scf) return 0;
  if (!scf->set) return 0;
  return scf->numSet;
}

int FRAMES_API scfGetSetInfo(int dsIdx, char *name, char *type)
{
  if (!scf) return 0;
  if (!scf->set) return 0;
  if (dsIdx<1 || dsIdx>scf->numSet) return 0;
  rstrcpy(name, scf->set[dsIdx-1]->name);
  rstrcpy(type, scf->set[dsIdx-1]->type);
  return 1;
}

int FRAMES_API scfGetDimensions(int dsIdx, double *width, double *length, double *depth)
{
  if (!scf) return 0;
  if (!scf->set) return 0;
  if (dsIdx<1 || dsIdx>scf->numSet) return 0;
  *width = scf->set[dsIdx-1]->x;
  *length = scf->set[dsIdx-1]->y;
  *depth = scf->set[dsIdx-1]->z;
  return 1;
}

int FRAMES_API scfGetNumContam(int dsIdx)
{
  if (!scf) return 0;
  if (!scf->set) return 0;
  if (dsIdx<1 || dsIdx>scf->numSet) return 0;
  return scf->set[dsIdx-1]->numCon;
}

int FRAMES_API scfGetNumProgeny(int dsIdx, int conIdx)
{
  if (!scf) return 0;
  if (!scf->set) return 0;
  if (dsIdx<1 || dsIdx>scf->numSet) return 0;
  SCFSet *set = scf->set[dsIdx-1];
  if (conIdx<1 || conIdx>set->numCon) return 0;
  return set->pCon[conIdx-1]->numProg;
}

int FRAMES_API scfGetContamName(int dsIdx, int conIdx, int progIdx, char *name, char *cas)
{
  if (!scf) return 0;
  if (!scf->set) return 0;
  if (dsIdx<1 || dsIdx>scf->numSet) return 0;
  SCFSet *set = scf->set[dsIdx-1];
  if (conIdx<1 || conIdx>set->numCon) return 0;
  SCFCon *con = set->pCon[conIdx-1];
  if (progIdx>con->numProg) return 0;
  if (progIdx>0) con = con->prog[progIdx-1];

  rstrcpy(name, con->name);
  rstrcpy(cas, con->cas);
  return 1;
}

int FRAMES_API scfGetSeriesProperties(int dsIdx, int conIdx, int progIdx, char *vunits, char *tunits)
{
  int count = 0;
  char tmp[SMALLSTRING];

  if (!scf) return 0;
  if (!scf->set) return 0;
  if (dsIdx<1 || dsIdx>scf->numSet) return 0;
  SCFSet *set = scf->set[dsIdx-1];
  if (conIdx<1 || conIdx>set->numCon) return 0;
  SCFCon *con = set->pCon[conIdx-1];
  if (progIdx>con->numProg) return 0;
  if (progIdx>0) con = con->prog[progIdx-1];

  scf->inf->setpos(con->filepos);
  *scf->inf >> tmp >> tmp >> tunits >> vunits >> count >> NewLn;
  return count;
}

int FRAMES_API scfGetSeriesValues(int dsIdx, int conIdx, int progIdx, int count, double *times, double *values)
{
  char tmp[SMALLSTRING];
  int cnt = scfGetSeriesProperties(dsIdx, conIdx, progIdx, tmp, tmp);
  if (count < cnt) return 0;
  for (int i = 0; i<cnt; i++)
    *scf->inf >> times[i] >> values[i] >> NewLn;
  return cnt;
}

void FRAMES_API scfAggregate(char *filename, char* casList)
{
  int i;
  char *dummy = new char[MAXPATH];

  if (scf == NULL) return;
  ocsv *eOut = new ocsv(filename, '"', ',', _CREATE_);
  eOut->alwaysQuote();
  *eOut << (scfGetNumRunInfo()+1) << NewLn;
  *eOut << "This file was modified by wrapspec.exe /out" << NewLn;
  for (i = 0; i<scfGetNumRunInfo(); i++)
  {
    scfGetRunInfo(i+1, dummy);
    *eOut << dummy << NewLn;
  }
  *eOut << scf->numSet << NewLn;
  for (i = 0; i<scf->numSet; i++)
    scf->set[i]->convertSCF(scf->inf, eOut, casList);

  delete eOut;
  delete [] dummy;
  return;
}

void FRAMES_API scfInsert(char *filename, char *casList, char *nameList, char *degList, char *secList, int branching)
{
  int i;
  int cnt;
  long pos;
  char *dummy = new char[MAXPATH];

  if (scf == NULL) return;
  ocsv *eOut;
  if (0 == access(filename, 0))
    eOut = new ocsv(filename, '"', ',', _APPEND_);
  else
    eOut = new ocsv(filename, '"', ',', _CREATE_);
  eOut->alwaysQuote();
  *eOut << scf->name;
  pos = eOut->getpos();
  eOut->smartQuote();
  *eOut << "          " << NewLn;
  eOut->alwaysQuote();
  *eOut << (scfGetNumRunInfo()+1) << NewLn;
  *eOut << "This file was modified by wrapspec.exe /in" << NewLn;
  cnt = 2;
  for (i = 0; i<scfGetNumRunInfo(); i++)
  {
    scfGetRunInfo(i+1,dummy);
    *eOut << dummy << NewLn;
    cnt++;
  }
  *eOut << scf->numSet << NewLn;
  cnt++;
  for (i = 0; i<scf->numSet; i++)
    cnt += scf->set[i]->unconvertSCF(scf->inf, eOut, casList, nameList, degList, secList, branching);

  eOut->setpos(pos);
  eOut->delim = ' ';
  *eOut << cnt;

  delete eOut;
  delete [] dummy;
  return;
}

