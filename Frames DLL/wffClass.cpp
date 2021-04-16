/*______________________________________________________________________________

  Date:       1993 - 2004
  Company:    Pacific Northwest National Laboratories
  Battelle Corporation
  ________________________________________________________________________________
  __Modifiication  History________________________________________________________

    DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#include "frames.h"
#include "wffClass.h"

static WFF *wff = NULL;

void WFFCon::Init()
{
  rstrcpy(name, "");
  rstrcpy(cas, "");
  rstrcpy(tunit, "");
  rstrcpy(vunit, "");
  filepos = 0;
  xypos = 0;
  numStep = 0;
  numProg = 0;
  prog = NULL;
  xy = NULL;
  xy2 = 0;
  numXYSeries = 0;
}

WFFCon::WFFCon()
{ Init(); }

WFFCon::WFFCon(icsv *inf, bool isProg = false)
{
  Init();
  filepos = inf->getpos();
  *inf >> name >> cas >> tunit >> vunit >> numStep >> numXYSeries;
  if (isProg)      *inf >> NewLn;
  else             *inf >> numProg >> NewLn;
  xypos = inf->getpos();
  xy = new Series();
  xy->Read(inf, numStep);
  if (numXYSeries > 1)
  {
    inf->setpos(xypos);
    xy2 = new Series();
    xy2->Read(inf, numStep, 2);
  }
  if (numProg>0)
  {
    prog = new WFFCon*[numProg];
    for (int i = 0; i<numProg; i++)
      prog[i] = new WFFCon(inf, true);
  }
  else
    prog = NULL;
}

WFFCon::WFFCon (char *_name, char *_cas, int count)
{
  Init();
  rstrcpy(name, _name);
  rstrcpy(cas, _cas);
  numProg = count;
  if (numProg>0) prog = new WFFCon*[numProg];
}

WFFCon::~WFFCon()
{
  if (prog != NULL)
  {
    for (int i = 0; i<numProg; i++)
      if (prog[i] != NULL) delete prog[i];
      delete[] prog;
  }
  if (xy) delete xy;
  if (xy2) delete xy2;
  Init();
}

void WFFCon::wffWrite(ocsv *outf)
{
  int i;
  *outf << name << cas << tunit << vunit << xy->count << numXYSeries << numProg << NewLn;
  for (i = 0; i < xy->count; i++)
  {
    *outf << xy->xValues[i] << xy->yValues[i];
    if (numXYSeries > 1 && xy2)
      *outf << xy2->yValues[i];
    *outf << NewLn;
  }
  for (i = 0; i<numProg; i++)
  {
    (prog[i])->wffWrite(outf, name, cas);
  }
}

void WFFCon::wffWrite(ocsv *outf, char *pname, char *pcas)
{
  int i;

  *outf << name << cas << tunit << vunit << xy->count << numXYSeries << pname << pcas << NewLn;
  for (i = 0; i < xy->count; i++)
  {
    *outf << xy->xValues[i] << xy->yValues[i];
    if (numXYSeries > 1 && xy2)
      *outf << xy2->yValues[i];
    *outf << NewLn;
  }
}

bool WFFCon::add(WFFCon *other, Series *fluxes1, Series *fluxes2, Series *flux)
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
        WFFCon *newprog = other->prog[i]->copy();
        appendProgeny(newprog);
      }
      else
      {
        ///add the current progeny from other to
        //  he dataset that matches one of this
        ///classes progenies
        if (!prog[j]->add(other->prog[i], fluxes1, fluxes2, flux))
          failed = true;
      }
  }

  if (numXYSeries > 1)
  {
    ///make sure they have starts ends and zeros, and both column
    ///values match up (ie have the same number of times)
    Series *other1 = other->xy->Clone();
    Series *other2 = other->xy2->Clone();
    //      syncSeries(other1, other2);

    if (!xy->Add(other1) || !xy2->Add(other2))
        failed = true;

    delete other1;
    delete other2;
  }
  else
  {
    if (!xy->Add(other->xy))
        failed = true;
    /*  the formula according to gene's document, for
    adding non surface water flux:
    C1 = Qc1 / Q1
    C2 = Qc2 / Q2

      CT = C1 + C2

        QcT = QT CT.

          int i;
          //divide xy by fluxes1 (C1 = Qc1 / Q1)
          for (i = 0; i < xy->numStep; i++)
          xy->yValues[i] = xy->yValues[i] / fluxes1->ContinInterp(xy->xValues[i]);
          //divide other->xy by fluxes2 (C2 = Qc2 / Q2)
          for (i = 0; i < other->xy->numStep; i++)
          other->xy->yValues[i] = other->xy->yValues[i] / fluxes2->ContinInterp(other->xy->xValues[i]);
          ///superposition series (CT = C1 + C2)
          xy->AddSeries(other->xy);
          //multiply by flux(QcT = QT CT)
          for (i = 0; i < xy->numStep; i++)
          xy->yValues[i] = xy->yValues[i] * flux->ContinInterp(xy->xValues[i]);
    */
  }
  return !failed;
}

void WFFCon::appendProgeny(WFFCon *progeny)
{
  WFFCon **temp = prog;

  prog = new WFFCon*[numProg+1];
  for (int i = 0; i < numProg; i++)
    prog[i] = temp[i];

  prog[numProg] = progeny;
  numProg++;

  delete [] temp;
}

WFFCon *WFFCon::copy()
{
  WFFCon *con = new WFFCon();
  rstrcpy(con->name, name);
  rstrcpy(con->cas, cas);
  rstrcpy(con->tunit, tunit);
  rstrcpy(con->vunit, vunit);
  con->filepos = filepos;
  con->xypos = xypos;
  con->numStep = numStep;
  con->numXYSeries = numXYSeries;
  if (xy)   con->xy = xy->Clone();
  if (xy2)  con->xy2 = xy2->Clone();
  if (numProg>0)    //copy over all progenies
  {
    con->numProg = numProg;
    prog = new WFFCon*[numProg];
    for (int i = 0; i < numProg; i++)
      con->prog[i] = prog[i]->copy();
  }
  return con;
}

void WFFSet::Init()
{
  filepos = 0;
  rstrcpy(name, "");
  rstrcpy(type, "");
  numCon = 0;
  pCon = NULL;
  filepos = 0;
  numFlux = 0;
  flux = NULL;
  width = 0.0;
  length = 0.0;
  distance = 0.0;
  recharge = 0.0;
}

WFFSet::WFFSet()
{
  Init();
}

WFFSet::WFFSet (icsv *inf)
{
  char dummy[SMALLSTRING];

  Init();
  filepos = inf->getpos();
  *inf >> name >> type; // name ~ Transmitting dsIdx type
  *inf >> width >> dummy; // width, units(m) of flux pane
  *inf >> length >> dummy; // length/height, units(m) of flux pane
  *inf >> distance >> dummy; // distance, units(m) from water table
  *inf >> recharge >> dummy; // natural recharge rate, units(m/yr)
  *inf >> numCon >> NewLn;
  *inf >> dummy >> dummy >> numFlux >> NewLn;
  fluxpos = inf->getpos();
  flux = new Series();
  flux->Read(inf, numFlux);
  pCon = new WFFCon*[numCon];
  for (int i = 0; i<numCon; i++)
    pCon[i] = new WFFCon(inf);
}

WFFSet::WFFSet (char *lname, char *ltype)
{
  Init();
  rstrcpy(name, lname);
  rstrcpy(type, ltype);
}

WFFSet::~WFFSet()
{
  if (pCon)
  {
    for (int i = 0; i<numCon; i++)
      if (pCon[i]) delete pCon[i];
      delete[] pCon;
  }
  Init();
}

bool WFFSet::add(WFFSet *other)
{
  ///constituent needs to know it's water flux before it was super positioned.
  Series *fluxes1 = 0;
  ///keeps track of if one of the adds failed
  bool failed = false;
  if (!rstrcmpi(type, "surface water"))
  {
    ///calculate new area
    double area1 = width * length;
    double area2 = other->width * other->length;
    double tallest = (width > other->width) ? width : other->width;
    if (tallest != width)
      length = (area1 / tallest) + other->length;
    else
      length = (area2 / tallest) + length;
    width = tallest;
  }
  else
    ///vadose and aquifer need old flux before its superpositioned
    fluxes1 = flux->Clone();

  ///superposition water rates
  if (!flux->Add(other->flux))
    failed = true;



  for (int i = 0; i < other->numCon; i++)
  {
    bool found = false;
    int j;

    ///search for the name/id in these datasets for a match
    for (j = 0; j < numCon; j++)
      //Change SJC 9/7/10
      //Old
      /*if (!rstrcmpi(pCon[j]->name, other->pCon[i]->name) &&
         !rstrcmpi(pCon[j]->cas, other->pCon[i]->cas))
      {
        found = true;
        break;
      }*/
      //New
      if (!rstrcmpi(pCon[j]->cas, other->pCon[i]->cas))
      {
        found = true;
        if (strlen(pCon[j]->name) < strlen(other->pCon[i]->name))
          for(int k=0; k < SMALLSTRING; k++)
            pCon[j]->name[k] = other->pCon[i]->name[k];
        break;
      }
      //End Change

    ///if these datasets don't match with the
    ///current dataset from other, create one
    if (!found)
    {
      WFFCon *newcon = other->pCon[i]->copy();
      append(newcon);
    }
    else
    {
      // add the current dataset from other to the dataset
      // that matches one of this classes datasets
      if (!pCon[j]->add(other->pCon[i], fluxes1, other->flux, flux))
        failed = true;
    }
  }
  if (fluxes1)
    delete fluxes1;
  return !failed;
}

void WFFSet::append(WFFCon *con)
{
  WFFCon **temp = pCon;

  pCon = new WFFCon*[numCon+1];
  for (int i = 0; i < numCon; i++)
    pCon[i] = temp[i];

  pCon[numCon] = con;
  numCon++;

  delete [] temp;
}

void WFFSet::wffWrite(ocsv *outf)
{
  int i;

  *outf << name << type << width << "m" << length << "m"
        << distance << "m" << recharge << "m/yr"
        << numCon << NewLn;

  *outf << "yr" << "m^3/yr" << flux->count << NewLn;
  for (i = 0; i < flux->count; i++)
    *outf << flux->xValues[i] << flux->yValues[i] << NewLn;

  for (i = 0; i < numCon; i++)
    pCon[i]->wffWrite(outf);
}

WFFSet *WFFSet::copy()
{
  WFFSet* newloc = new WFFSet(name, type);
  newloc->filepos = filepos;
  newloc->numCon = numCon;
  newloc->width = width;
  newloc->length = length;
  newloc->distance = distance;
  newloc->recharge = recharge;
  newloc->fluxpos = fluxpos;
  newloc->numFlux = numFlux;
  newloc->flux = flux->Clone();
  ///copy over all constituents
  if (numCon > 0) newloc->pCon = new WFFCon*[numCon];
  for (int i = 0; i < numCon; i++)
    newloc->pCon[i] = pCon[i]->copy();
  return newloc;
}

int WFFSet::convertWFF(icsv *inf, ocsv *eOut, char *casList)
{
  char *mylist = strdup(casList);
  WFFCon *con;
  bool found;
  int m;

  splitList(mylist);
  // dataset info
  *eOut << name << type << width << "m" << length << "m"
    << distance << "m" << recharge << "m/yr"
    << (long)rList.size() << NewLn;

  *eOut << "yr" << "m^3/yr" << flux->count << NewLn;
  for (int i = 0; i < flux->count; i++)
    *eOut << flux->xValues[i] << flux->yValues[i] << NewLn;

  for (int k = 0; k< (long)rList.size(); k++)
  {
    con = NULL;
    Series **xy = new Series*[2];
    for (m = 0; m<2; m++) xy[m] = NULL;

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
        for (m = 1; m<= con->numXYSeries; m++)
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
      *eOut << "Not found" << rList[k] << "yr" << "?" << 0 << 0 << 0 << NewLn;
    else
    {
      *eOut << con->name << rList[k] << con->tunit << con->vunit << xy[0]->count << con->numXYSeries << 0 << NewLn;
      for (int i = 0; i<xy[0]->count; i++)
      {
        *eOut << xy[0]->xValues[i];
        for (int m = 0; m<con->numXYSeries; m++)
          *eOut << (double)xy[m]->yValues[i];
        *eOut << NewLn;
      }
    }
    for (m = 1; m<= 2; m++) delete xy;
    delete[] xy;
  }

  delete mylist;
  return 0;
}

// this routine will add liked named parents in the caslist
// then write them with degradation product that have zero time steps
// ???List are comma seperated and should be one to one with cas as primary key
// all degradation products must be listed as parents if not they are ignored
int WFFSet::unconvertWFF(icsv *inf, ocsv *eOut, char *casList, char *nameList, char *degList, char *secList, int branching)
{
  int i, j, p, m, last, lcnt;
  int numFluxtypes;
  WFFCon *con;
  bool found;

  OpenDecayChain(casList, nameList, degList, secList, branching);

  numFluxtypes = 1;
  if (!rstrcmpi(type, "Surface Water")) numFluxtypes = 2;

  // dataset info
  *eOut << name << type << width << "m" << length << "m"
        << distance << "m" << recharge << "m/yr"
        << (long)cList.size() << NewLn;

  lcnt = 1;

  *eOut << "yr" << "m^3/yr" << flux->count << NewLn;
  lcnt++;
  for (i = 0; i < flux->count; i++)
    *eOut << flux->xValues[i] << flux->yValues[i] << NewLn;
  lcnt+= flux->count;


  for (p = 0; p<(long)cList.size(); p++)
  {
    // loading chemical names
    con = NULL;
    Series **xy = new Series*[2];
    for (m = 0; m<2; m++) xy[m] = NULL;

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
        for (m = 1; m<= 2; m++)
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
      *eOut << con->name << cList[p] << con->tunit << con->vunit << xy[0]->count << numFluxtypes << (long)d3List.size() << NewLn;
      lcnt++;
      for (i = 0; i<xy[0]->count; i++)
      {
        *eOut << xy[0]->xValues[i];
        for (m = 0; m<numFluxtypes; m++)
          *eOut << (double)xy[m]->yValues[i];
        *eOut << NewLn;
        lcnt++;
      }
      // write out progeny list with no time pts
      last = p;
      for (i = 0; i< (long)d3List.size(); i++)
        for (j = 0; j< (long)cList.size(); j++)
          if (!rstrcmpi(d3List[i], cList[j]))
          {
            *eOut << nList[j] << cList[j] << con->tunit << con->vunit << 2 << numFluxtypes << nList[last] << cList[last] << NewLn;
            *eOut << 0;
            for (m = 0; m<numFluxtypes; m++)
              *eOut << 0.0;
            *eOut << NewLn << 1;
            for (m = 0; m<numFluxtypes; m++)
              *eOut << 0.0;
            *eOut << NewLn;
            last = j;
            lcnt+=3;
            break;
          }
    }
    for (m = 0; m<2; m++) delete xy[m];
    delete[] xy;
  }

  CloseDecayChain();
  return lcnt;
}

void WFF::Init()
{
  inf = NULL;
  set = NULL;
  numSet = 0;
}

WFF::WFF()
{
  Init();
}

WFF::WFF(char *fuiname, char *modId)
{
  char filename[MAXPATH];

  Init();
  rstrcpy(name,modId);
  sprintf(filename, "%s.wff", fuiname);
  inf = new icsv(filename, '\"');
  if (!inf->ok()) return;
  if (0 != inf->SeekSection(modId, &wffHead)) return;
  *inf >> numSet >> NewLn;
  set = new WFFSet*[numSet];
  for (int i = 0; i<numSet; i++)
    set[i] = new WFFSet(inf);
}

WFF::~WFF()
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

void WFF::setName(char *lname)
{
  for (int i = 0; i<numSet; i++)
    rstrcpy(set[i]->name, lname);
}

bool WFF::add(WFF *other, char *id)
{
  double k;
  char  msg[SMALLSTRING];

  ///go through the datasets in the other object
  ///and add them all to this one.  if this one
  ///doesn't have a dataset from the other,
  ///create so that they can be added
  for (int i = 0; i < other->numSet; i++)
  {
    int j;
    bool sizematch;
    bool typematch;
    ///add only those that match the id
    if (!rstrcmpi(other->set[i]->name,id) || !rstrcmpi("all", other->set[i]->name))
    {
      typematch = false;
      for (j = 0; j < numSet; j++)
      {
        // add only qualifier type matches
        if (!rstrcmpi(set[j]->type, other->set[i]->type))
        {
          typematch = true;
        ///if both surface, add if areas not equal set k (esp) to -1
        ///if both vadose or both aquifer and equal areas, add
        ///else error can't because of size match

           // **** surface water does not need same flux plane area ****
          if (rstrcmpi(set[j]->type, "surface water"))
          {
           // **** aquifer and vadose need the same area within an epsilon area to be added ****
            sizematch = false;
            for (k = eps; k<8; k*= 10)
              if (isEqual((set[j]->length*set[j]->width), (other->set[i]->length*other->set[i]->width), k))
                sizematch = true;
          }
          else
          {
            sizematch = true;
            k = -1;
          }
          if (sizematch)
          {
            if (!set[j]->add(other->set[i]))
            {
              sprintf(msg,"%s.%s location failed to add!", other->name, other->set[i]->name);
              writeError(msg);
              return false;
            }
            else
            {
              if (k != -1)
              {
                sprintf(msg,"%s.%s and %s.%s location's area are within %f percent of each other!",
                     name,set[j]->name, other->name, other->set[i]->name);
                writeWarn(msg);
              }
              //Added 11/6/09 SJC
              else
              {
                //  Times must match in each series
                //  because of the way wff files are outputed
                //  This only effects surface water types
                for (int n=0; n<set[j]->numCon; n++){
                  set[j]->pCon[n]->xy->InsertXpts(set[j]->pCon[n]->xy2);
                  set[j]->pCon[n]->xy2->InsertXpts(set[j]->pCon[n]->xy);
                }
              }
              //END
            }
          }
          else
          {
            sprintf(msg,"%s.%s and %s.%s locations failed to match on area!",
                     name,set[j]->name, other->name, other->set[i]->name);
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
        WFFSet *newmedialoc = other->set[i]->copy();
        append(newmedialoc);
      }
    }
    //else no name match disregrard location
  }
  return true;
}

void WFF::wffWrite (char *tmpname)
{
  char dummy[MAXPATH];
  ocsv *outf;

  sprintf(dummy, "%s.wff", tmpname);
  outf = new ocsv(dummy, '\"');
  if (!outf->ok()) return;
  outf->alwaysQuote();
  *outf << 1 << NewLn;
  *outf << "File created with WFF I/O tools"  << NewLn;
  *outf << numSet << NewLn;
  for (int i = 0; i<numSet; i++)
    if (set[i]) set[i]->wffWrite(outf);
    delete outf;
}

void WFF::append(WFFSet *loc)
{
  WFFSet **temp = set;

  set = new WFFSet*[numSet+1];
  for (int i = 0; i < numSet; i++)
    set[i] = temp[i];

  set[numSet] = loc;
  numSet++;

  delete [] temp;
}

//------------------------------------------------------------------------------
// WCF API ---------------------------------------------------------------------
//------------------------------------------------------------------------------

void FRAMES_API wffOpen(char *fuiname, char *modId)
{
  wffClose();
  wff = new WFF(fuiname, modId);
}

void FRAMES_API wffClose()
{
  if (wff) delete wff;
  wff = NULL;
}

int FRAMES_API wffGetNumRunInfo()
{
  int num;
  if (wff == NULL) return 0;
  if (wff->numSet<= 0) return 0;
  wff->inf->setpos(wff->wffHead);
  *wff->inf >> num >> NewLn;
  return num;
}

int FRAMES_API wffGetRunInfo(int Idx, char *info)
{
  int num = wffGetNumRunInfo();
  if (num == 0 || Idx>num) return 0;
  for (int i = 1; i<Idx; i++)
    *wff->inf >> NewLn;
  *wff->inf >> info >> NewLn;
  return 1;
}

int FRAMES_API wffGetNumSets()
{
  if (!wff) return 0;
  if (!wff->set) return 0;
  return wff->numSet;
}

int FRAMES_API wffGetSetInfo(int dsIdx, char *name, char *type)
{
  if (!wff) return 0;
  if (!wff->set) return 0;
  if (dsIdx<1 || dsIdx>wff->numSet) return 0;

  rstrcpy(name, wff->set[dsIdx-1]->name);
  rstrcpy(type, wff->set[dsIdx-1]->type);
  return 1;
}

int FRAMES_API wffGetDimensions(int dsIdx, double *width, double *length, double *dist, double *recharge)
{
  if (!wff) return 0;
  if (!wff->set) return 0;
  if (dsIdx<1 || dsIdx>wff->numSet) return 0;
  *width = wff->set[dsIdx-1]->width;
  *length = wff->set[dsIdx-1]->length;
  *dist = wff->set[dsIdx-1]->distance;
  *recharge = wff->set[dsIdx-1]->recharge;
  return 1;
}

int FRAMES_API wffGetNumContam(int dsIdx)
{
  if (!wff) return 0;
  if (!wff->set) return 0;
  if (dsIdx<1 || dsIdx>wff->numSet) return 0;
  return wff->set[dsIdx-1]->numCon;
}

int FRAMES_API wffGetNumProgeny(int dsIdx, int conIdx)
{
  if (!wff) return 0;
  if (!wff->set) return 0;
  if (dsIdx<1 || dsIdx>wff->numSet) return 0;
  WFFSet *set = wff->set[dsIdx-1];
  if (conIdx<1 || conIdx>set->numCon) return 0;
  return wff->set[dsIdx-1]->pCon[conIdx-1]->numProg;
}

int FRAMES_API wffGetContamName(int dsIdx, int conIdx, int progIdx, char *name, char *cas)
{
  if (!wff) return 0;
  if (!wff->set) return 0;
  if (dsIdx<1 || dsIdx>wff->numSet) return 0;
  WFFSet *set = wff->set[dsIdx-1];
  if (conIdx<1 || conIdx>set->numCon) return 0;
  WFFCon *con = set->pCon[conIdx-1];
  if (progIdx>con->numProg) return 0;
  if (progIdx>0) con = con->prog[progIdx-1];

  rstrcpy(name, con->name);
  rstrcpy(cas, con->cas);
  return 1;
}

int FRAMES_API wffGetNumFluxTypes(int dsIdx)
{
  if (!wff) return 0;
  if (!wff->set) return 0;
  if (dsIdx<1 || dsIdx>wff->numSet) return 0;
  WFFSet *set = wff->set[dsIdx-1];
  if (1>set->numCon) return 0;
  WFFCon *con = set->pCon[0];
  return con->numXYSeries;
}

int FRAMES_API wffGetWaterFluxSeries(int dsIdx, int count, double *times, double *values)
{
  if (!wff) return 0;
  if (!wff->set) return 0;
  if (dsIdx<1 || dsIdx>wff->numSet) return 0;
  WFFSet *set = wff->set[dsIdx-1];

  if (count != set->numFlux) return 0;
  wff->inf->setpos(set->fluxpos);
  for (int i = 0; i<set->numFlux; i++)
    *wff->inf >> times[i] >> values[i] >> NewLn;
  return set->numFlux;
}

int FRAMES_API wffGetSeriesProperties(int dsIdx, int conIdx, int progIdx, char *fluxunits, char *timeunits)
{
  int count = 0;
  char tmp[SMALLSTRING];

  if (!wff) return 0;
  if (!wff->set) return 0;
  if (dsIdx<1 || dsIdx>wff->numSet) return 0;
  WFFSet *set = wff->set[dsIdx-1];
  if (conIdx<1 || conIdx>set->numCon) return 0;
  WFFCon *con = set->pCon[conIdx-1];
  if (progIdx>con->numProg) return 0;
  if (progIdx>0) con = con->prog[progIdx-1];

  wff->inf->setpos(con->filepos);
  *wff->inf >> tmp >> tmp >> timeunits >> fluxunits >> count >> NewLn;
  return count;
}

int FRAMES_API wffGetSeriesValues(int dsIdx, int conIdx, int progIdx, int fluxIdx, int count, double *times, double *values)
{
  char tmp[SMALLSTRING];
  int cnt = wffGetSeriesProperties(dsIdx, conIdx, progIdx, tmp, tmp);
  if (count < cnt) return 0;
  for (int i = 0; i<cnt; i++)
  {
    *wff->inf >> times[i];
    for (int j = 0; j<fluxIdx; j++)
      *wff->inf >> values[i];
    *wff->inf >> NewLn;
  }
  return cnt;
}

int FRAMES_API wffGetSeriesValuesSum(int dsIdx, int conIdx, int progIdx, int count, double *times, double *values)
{
  char tmp[SMALLSTRING];
  if (count != wffGetSeriesProperties(dsIdx, conIdx, progIdx, tmp, tmp)) return 0;

  WFFSet *set = wff->set[dsIdx-1];
  WFFCon *con = set->pCon[conIdx-1];
  if (progIdx>0) con = con->prog[progIdx-1];

  double flux1 = 0.0;
  double flux2 = 0.0;
  for (int i = 0; i<count; i++)
  {
    *wff->inf >> times[i];
    *wff->inf >> flux1;
    if (con->numXYSeries == 2) *wff->inf >> flux2;
    values[i] = flux1+flux2;
    *wff->inf >> NewLn;
  }
  return count;
}

void FRAMES_API wffAggregate(char *filename, char *casList)
{
  int i;
  char *dummy = new char[MAXPATH];

  if (wff == NULL) return;
  ocsv *eOut = new ocsv(filename, '"', ',', _CREATE_);
  eOut->alwaysQuote();
  *eOut << (wffGetNumRunInfo()+1) << NewLn;
  *eOut << "This file was modified by wrapspec.exe /out" << NewLn;
  for (i = 0; i<wffGetNumRunInfo(); i++)
  {
    wffGetRunInfo(i+1, dummy);
    *eOut << dummy << NewLn;
  }
  *eOut << wff->numSet << NewLn;
  for (i = 0; i<wff->numSet; i++)
    wff->set[i]->convertWFF(wff->inf, eOut, casList);

  delete eOut;
  delete [] dummy;
  return;
}

void FRAMES_API wffInsert(char *filename, char *casList, char *nameList, char *degList, char *secList, int branching)
{
  int i;
  int cnt;
  long pos;
  char *dummy = new char[MAXPATH];

  if (wff == NULL) return;
  ocsv *eOut;
  if (0 == access(filename, 0))
    eOut = new ocsv(filename, '"', ',', _APPEND_);
  else
    eOut = new ocsv(filename, '"', ',', _CREATE_);
  eOut->alwaysQuote();
  *eOut << wff->name;
  pos = eOut->getpos();
  eOut->smartQuote();
  *eOut << "          " << NewLn;
  eOut->alwaysQuote();
  *eOut << (wffGetNumRunInfo()+1) << NewLn;
  *eOut << "This file was modified by wrapspec.exe /in" << NewLn;
  cnt = 2;
  for (i = 0; i<wffGetNumRunInfo(); i++)
  {
    wffGetRunInfo(i+1,dummy);
    *eOut << dummy << NewLn;
    cnt++;
  }
  *eOut << wff->numSet << NewLn;
  cnt++;
  for (i = 0; i<wff->numSet; i++)
    cnt += wff->set[i]->unconvertWFF(wff->inf, eOut, casList, nameList, degList, secList, branching);

  eOut->setpos(pos);
  eOut->delim = ' ';
  *eOut << cnt;

  delete eOut;
  delete [] dummy;
  return;
}

