/*______________________________________________________________________________

   Date:       1993 - 2004
   Company:    Pacific Northwest National Laboratories
               Battelle Corporation
________________________________________________________________________________
__Modifiication  History________________________________________________________

  DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#include "frames.h"
#include "epfread.h"

icsv *eIn = NULL;
ocsv *eOut = NULL;
int numepf = 0;
long epfHead = 0;
static EPFSet *epf = NULL;


//------------------------------------------------------------------------------
//class EPFCon
EPFCon::~EPFCon()
{
  if (time) delete[] time;
  time = NULL;
  if (prog)
  {
    for (int i = 0; i<nProg; i++) delete prog[i];
    delete[] prog;
  }
  prog = NULL;
  nProg = 0;
}

EPFCon::EPFCon(int numTime)
{
  nProg = 0;
  prog = NULL;
  time = new fpos_t[numTime];
  nTime = numTime;
}

EPFCon::EPFCon(icsv *eIn)
{
  int np,nt;
  int numExp;
  char dummy[SMALLSTRING];

  nProg = 0;
  nTime = 0;
  prog = NULL;
  time = NULL;
  fpos = eIn->getpos();
  *eIn >> name >> casid >> nProg >> nTime >> NewLn;

  prog = new EPFCon*[nProg];
  time = new fpos_t[nTime];

  for (np = 0; np<nProg; np++)
    prog[np] = new EPFCon(nTime);

  for (nt = 0; nt<nTime; nt++)
  {
    time[nt] = eIn->getpos();
    *eIn >> dummy >> dummy >> dummy >> dummy >> numExp >> NewLn;
    eIn->Skip(numExp*2);
    for (np = 0; np<nProg; np++)
    {
      prog[np]->time[nt] = eIn->getpos();
      prog[np]->fpos = eIn->getpos();
      *eIn >> prog[np]->name >> prog[np]->casid >> numExp >> NewLn ;
      eIn->Skip(numExp*2);
    }
  }
}

double EPFCon::getTime(int timeIdx)
{
  double tmp = 0.0;
  if (nTime<= timeIdx || timeIdx<0) return 0;
  eIn->setpos(time[timeIdx]);
  *eIn >> tmp;
  return tmp;
}

int EPFCon::LoadExposuresByTime(char *cas, int timeIdx, expRoute **exp)
{
  int i;
  int numExp;
  char dummy[SMALLSTRING];
  char duration[SMALLSTRING];

  numExp = 0;
  eIn->setpos(time[timeIdx]);
  *eIn >> dummy >> dummy >> duration >> dummy >> numExp >> NewLn;
  if (rstrcmpi(casid,cas))
    for (i = 0; i<nProg; i++)
      if (!rstrcmpi(prog[i]->casid,cas))
      {
        eIn->setpos(prog[i]->time[timeIdx]);
        *eIn >> dummy >> dummy >> numExp >> NewLn;
        break;
      }

  if (numExp>0)
  {
    *exp = new expRoute[numExp];
    for (i = 0; i<numExp; i++)
      *eIn >> (*exp)[i].path >> (*exp)[i].route >> (*exp)[i].unit >> NewLn >> NewLn;
  }
  return numExp;
}

//------------------------------------------------------------------------------
//class EPFSet
EPFSet::EPFSet()
{
  nExp = 0;
  exp = NULL;
  nChem = 0;
  chem = NULL;
  fpos = NULL;
}

EPFSet::~EPFSet()
{
  if (x) delete[] x;
  if (y) delete[] y;
  if (rtime) delete[] rtime;
  if (rvalue) delete[] rvalue;
  if (exp) delete exp;

  if (chem)
  {
    for (int i = 0; i<nChem; i++) delete chem[i];
    delete[] chem;
  }
}

void EPFSet::epfRead(icsv *eIn)
{
  int i;
  char dummy[SMALLSTRING];

  *eIn >> locExp >> locType >> locMedia >> nLoc >> nChem >> NewLn;

  x = new double[nLoc];
  y = new double[nLoc];

  for (i = 0; i<nLoc; i++)
    *eIn >> x[i] >> dummy >> y[i] >> dummy >> NewLn;

  chem = new EPFCon*[nChem];
  fpos = new fpos_t[nChem];

  for (i = 0; i<nChem; i++)
  {
    fpos[i] = eIn->getpos();
    chem[i] = new EPFCon(eIn);
  }
}

int EPFSet::getDatasetInfo(int *numLoc, char *typ, char *med, char *exp)
{
  *numLoc = nLoc;
  rstrcpy(typ, locType);
  rstrcpy(med, locMedia);
  rstrcpy(exp, locExp);
  return 1;
}

int EPFSet::getExposurePoint(int locIdx, double *xp, double *yp)
{
  if (nLoc<= locIdx || locIdx<0) return 0;
  *xp = x[locIdx];
  *yp = y[locIdx];
  return 1;
}

int EPFSet::getTimeCount(char *cas)
{
  int nc,j;
  int cnt1, cnt2;

  cnt1 = 0;
  cnt2 = 0;
  for (nc = 0; nc<nChem; nc++)
  {
    if (!rstrcmpi(chem[nc]->casid,cas))
      cnt1 = chem[nc]->nTime;
    for (j = 0; j<chem[nc]->nProg; j++)
      if (!rstrcmpi(chem[nc]->prog[j]->casid,cas))
        cnt2 = chem[nc]->prog[j]->nTime;
  }
  if (cnt2 > cnt1) return cnt2;
  if (cnt1 >= cnt2) return cnt1;
  return 0;
}

double EPFSet::getTime(char *parentCAS, int timeIdx)
{
  for (int nc = 0; nc<nChem; nc++)
    if (!rstrcmpi(chem[nc]->casid,parentCAS))
      return chem[nc]->getTime(timeIdx);
  return 0;
}

int EPFSet::LoadExposuresByTime(char *cas, char *parentCAS, int timeIdx)
{
  int i;

  if (exp != NULL)    delete[] exp;
  exp = NULL;

  nExp = 0;
  for (i = 0; i<nChem; i++)
    if (!rstrcmpi(chem[i]->casid,parentCAS))
    {
      nExp = chem[i]->LoadExposuresByTime(cas,timeIdx,&exp);
      return nExp;
    }
  return 0;
}

void EPFSet::getExposure(int expIdx, char *path, char *route, char *unit)
{
  if (exp != NULL && expIdx<nExp && expIdx>-1)
  {
    rstrcpy(path, exp[expIdx].path);
    rstrcpy(route, exp[expIdx].route);
    rstrcpy(unit, exp[expIdx].unit);
  }
  else
  {
    rstrcpy(path, "");
    rstrcpy(route, "");
    rstrcpy(unit, "");
  }
}

int EPFSet::LoadTimeSeries(int locIdx, char *cas, char *parentCAS, char *path, char *route)
{
  double value, start;
  int i, nc, np, nt, ne;
  int ct, ot;
  int numTime, numProg, numExp;
  bool found;

  char dummy[SMALLSTRING];
  char casid[SMALLSTRING];
  char prog[SMALLSTRING];
  char tPath[SMALLSTRING];
  char tRoute[SMALLSTRING];
  char tUnit[SMALLSTRING];

  if (rtime != NULL) delete[] rtime;
  if (rtime != NULL) delete[] rvalue;
  nTime = 0;
  rtime = NULL;
  rvalue = NULL;
  if (nLoc<= locIdx || locIdx<0) return 0;

  found = false;
  for (nc = 0; nc<nChem; nc++)
  {
    eIn->setpos(fpos[nc]);
    *eIn >> dummy >> casid >> numProg >> numTime >> NewLn;
    if (!rstrcmpi(casid, parentCAS))
    {
      rvalue = new double[(int)(numTime+1)];
      rtime = new double[(int)(numTime+1)];
      *rvalue = 0.0;
      *rtime = 0.0;
      ot = 0;
      ct = 0;
      for (nt = 0; nt<numTime; nt++)
      {
        value = 0.0;
        *eIn >> start >> dummy >> duration >> dummy >> numExp >> NewLn;
        if (nt == 0 && !rstrcmpi(locExp,"acute"))  acuteduration = duration;
        if (!rstrcmpi(casid, cas))
          // scan parent for value
          for (np = 0; np<numExp; np++)
          {
            *eIn >> tPath >> tRoute >> tUnit >> NewLn;
            if  (!rstrcmpi(path,tPath) && !strcmpi(route,tRoute))
            {
              value = 0.0;
              found = true;
              for (i = 0; i<= locIdx && i<nLoc && nLoc>locIdx; i++)
                *eIn >> value;
            }
            *eIn >> NewLn;
          }
        else
          eIn->Skip(2*numExp);
        for (np = 0; np<numProg; np++)
        {
          *eIn >> dummy >> prog >> numExp >> NewLn;
          if (!rstrcmpi(prog,cas))
           // scan progeny for value
            for (ne = 0; ne<numExp; ne++)
            {
              *eIn >> tPath >> tRoute >> tUnit >> NewLn;
              if (!rstrcmpi(route,tRoute) && !strcmpi(path,tPath))
              {
                value = 0.0;
                found = true;
                for (i = 0; i<= locIdx && i<nLoc && nLoc>locIdx; i++)
                  *eIn >> value;
              }
              *eIn >> NewLn;
            }
          else
            eIn->Skip(2*numExp);
        }
        // not sure why first time must be 0.0
        // but the next two lines below assure time[0] = 0
        // commented out becaues we think it's not right BLH.MAP
        // if (start != 0.0 && nt == 0) ot = 1;
        ct = nt+ot;
        rvalue[ct] = value;
        rtime[ct] = start;
      }
      if (found)
        nTime = ct+1;
      else
        nTime = 0;
      break;
    }
  }
  return nTime;
}

int EPFSet::getTimeSeries(double *stime, double *svalue)
{
  int i;
  for (i = 0; i<nTime; i++)
  {

    stime[i] = rtime[i];
    svalue[i] = rvalue[i];
  }
  return nTime;
}

void EPFSet::getTimeAndValue(int timeIdx, double *stime, double *svalue)
{
  if (nTime<= timeIdx || timeIdx<0) return;
  *stime = rtime[timeIdx];
  *svalue = rvalue[timeIdx];
}

void EPFSet::aggregateExposures(char *cas, char *parentCas, int numTimes, bool newAggregate)
{
  if (newAggregate)  // delete exposures
  {
    for (_EXPLIST_IT it = erp.begin(); it != erp.end(); ++it)
    {
      expRoute *tmp = (*it).second; ;
      if (tmp != NULL) delete tmp;
    }
    erp.clear();
  }

  for (int t = 0; t<numTimes; t++)
  {
    LoadExposuresByTime(cas, parentCas, t);
    for (int m = 0; m< nExp; m++)
    {
      std::string key = exp[m].path;
      key += exp[m].route;
      key += exp[m].unit;
      expRoute *myexp = erp[key];
      if (myexp == NULL)
      {
        expRoute *tmp = new expRoute;
        rstrcpy(tmp->path, exp[m].path);
        rstrcpy(tmp->route, exp[m].route);
        rstrcpy(tmp->measure, exp[m].measure);
        rstrcpy(tmp->unit, exp[m].unit);
        erp[key] = tmp;
      }
    }
  }
}

int EPFSet::writeConstituent(char *cas, char *name, int numTimes)
{
  double dur;
  if (rtime)
  {
    //write chemheader
    //ChemName, ChemCAS, NumProg = 0, numtimes
    *eOut << name << cas << 0 << numTimes << NewLn;

    //loop times
    for (int t = 0; t<numTimes; t++)
    {
      // load exposures for current chemical
      if (t == 0 && !rstrcmpi(locExp,"acute"))
        dur = acuteduration; // used later for writing constituents
      else
        dur = duration;

      *eOut << rtime[t] << "yr" << dur << "yr" << (long)erp.size() << NewLn;
      //loop exposures
      int m = 0;
      for (_EXPLIST_IT it = erp.begin(); it != erp.end(); ++it)
      {
        //write pathname, routename, unitname
        *eOut << (*it).second->path
              << (*it).second->route
              << (*it).second->unit << NewLn;
        //loop locations
        for (int l = 0; l < nLoc; l++)
          *eOut << tempValues[l][t][m];
        *eOut << NewLn;
        m++;
      }
    }
  }
  else
    //ChemName, ChemCAS, NumProg = 0, numtimes = 0
    *eOut << name << cas << 0 << 0 << NewLn;

  return 0;
}

int EPFSet::convertEPF(char *casList)
{
  char *mylist = strdup(casList);
  char *chemName;
  int numTimes;

  splitList(mylist);
  // dataset info
  *eOut << locExp << locType << locMedia << nLoc << (long)rList.size() << NewLn;
  // location info
  for (int l = 0; l < nLoc; l++)
    *eOut << x[l] << "km" << y[l] << "km" << NewLn;
  // loop progenies
  for (int k = 0; k< (long)rList.size(); k++)
  {
    // loading all exposure pathways
    aggregateExposures(rList[k],rList[k], getTimeCount(rList[k]), true);
    // loading chemical names
    for (int i = 0; i<nChem; i++)
    {
      if (!rstrcmpi(rList[k], chem[i]->casid))
        chemName = chem[i]->name;
      for (int j = 0; j<chem[i]->nProg; j++)
        if (!rstrcmpi(chem[i]->prog[j]->casid,rList[k]))
        {
          chemName = chem[i]->prog[j]->name;
          aggregateExposures(chem[i]->prog[j]->casid, chem[i]->casid, getTimeCount(rList[k]), false);
        }
    }
    //loading times and initializing
    numTimes = getTimeCount(rList[k]);
    tempValues = new double**[nLoc];
    for (int l = 0; l<nLoc; l++)
    {
      tempValues[l] = new double*[numTimes];
      for (int t = 0; t<numTimes; t++)
      {
        tempValues[l][t] = new double[erp.size()];
        for (int m = 0; m< (long)erp.size(); m++)
          tempValues[l][t][m] = 0.0;
      }
    }

    //loop exposures
    int m = 0;
    for (_EXPLIST_IT it = erp.begin(); it != erp.end(); ++it)
    {
      for (int l = 0; l<nLoc; l++)
      {
        int numTimes = LoadTimeSeries(l, rList[k], rList[k], (*it).second->path, (*it).second->route);
        for (int t = 0; t<numTimes; t++)
          tempValues[l][t][m] = rvalue[t];

        for (int i = 0; i<nChem; i++)
          for (int j = 0; j<chem[i]->nProg; j++)
            if (!rstrcmpi(chem[i]->prog[j]->casid,rList[k]))
            {
              int numTimes = LoadTimeSeries(l, rList[k], chem[i]->casid, (*it).second->path, (*it).second->route);
              for (int t = 0; t<numTimes; t++)
                tempValues[l][t][m] += rvalue[t];
            }
      }
      m++;
    }
    // write constituent/chemical/progeny
    writeConstituent(rList[k], chemName, numTimes);

    for (int l = 0; l<nLoc; l++)
    {
      for (int t = 0; t<numTimes; t++)
        delete[]  tempValues[l][t];
      delete[] tempValues[l];
    }
    delete[] tempValues;
  }

  delete mylist;
  return 0;
}

//------------------------------------------------------------------------------
// EPF API ---------------------------------------------------------------------
//------------------------------------------------------------------------------
bool FRAMES_API epfChecksum()
{  return (eIn->recordsCount == eIn->recordsRead); }

int FRAMES_API epfOpen(char *fuiname, char *modId)
{
  int i;
  char EPFName[MAXPATH];

  numepf = 0;
  sprintf(EPFName,"%s.epf",fuiname);
  eIn = new icsv(EPFName);
  if (!eIn->ok()) return 0;
  if (0 != eIn->SeekSection(modId, &epfHead)) return 0;
  *eIn >> numepf >> NewLn;
  epf = new EPFSet[numepf];
  for (i = 0; i<numepf; i++)
    epf[i].epfRead(eIn);
  return numepf;
}

void FRAMES_API epfClose()
{
  if (eIn != NULL) delete eIn;
  if (epf != NULL) delete[] epf;
  numepf = 0;
  epf = NULL;
  eIn = NULL;
}

int FRAMES_API epfGetNumRunInfo()
{
   int num = 0;
   if (numepf<= 0) return 0;
   eIn->setpos(epfHead);
   *eIn >> num >> NewLn;
   return num;
}

int FRAMES_API epfGetRunInfo(int Idx, char *info)
{
  int num = epfGetNumRunInfo();
  if (num == 0) return 0;
   for (int i = 1; i<Idx; i++)
     *eIn >> NewLn;
   *eIn >> info >> NewLn;
   return 1;
}

int FRAMES_API epfGetSetInfo(int dsIdx, int *numLoc, char *typ, char *med, char*exp)
{  if (numepf<= dsIdx || dsIdx<0) return 0;
   return epf[dsIdx].getDatasetInfo(numLoc, typ, med, exp); }

int FRAMES_API epfGetExposurePoint(int dsIdx, int locIdx, double *xp, double *yp)
{  if (numepf<= dsIdx || dsIdx<0) return 0;
   return epf[dsIdx].getExposurePoint(locIdx, xp, yp); }

int FRAMES_API epfGetTimeCount(int dsIdx, char *parentCAS)
{  if (numepf<= dsIdx || dsIdx<0) return 0;
   return (epf[dsIdx].getTimeCount(parentCAS)); }

double FRAMES_API epfGetTime(int dsIdx, char *parentCAS, int timeIdx)
{  if (numepf<= dsIdx || dsIdx<0) return 0.0;
   return (epf[dsIdx].getTime(parentCAS, timeIdx)); }

int FRAMES_API epfLoadRoutesAndPathwaysByTime(int dsIdx, char *cas, char *parentCAS, int timeIdx)
{  if (numepf<= dsIdx || dsIdx<0) return 0;
   return (epf[dsIdx].LoadExposuresByTime(cas, parentCAS, timeIdx)); }

void FRAMES_API epfGetRouteAndPathway(int dsIdx, int expIdx, char *path, char *route, char *unit)
{  if (numepf<= dsIdx || dsIdx<0) return;
   epf[dsIdx].getExposure(expIdx, path, route, unit); }

int FRAMES_API epfLoadTimeSeries(int dsIdx, int locIdx, char *cas, char *parentCAS, char *path, char *route)
{  if (numepf<= dsIdx || dsIdx<0) return 0;
   return (epf[dsIdx].LoadTimeSeries(locIdx, cas, parentCAS, path, route)); }

void FRAMES_API epfGetTimeSeries(int dsIdx, double *stime, double *svalue)
{  if (numepf<= dsIdx || dsIdx<0) return;
   epf[dsIdx].getTimeSeries(stime, svalue); }

void FRAMES_API epfGetTimeAndValue(int dsIdx, int timeIdx, double *stime, double *svalue)
{  if (numepf<= dsIdx || dsIdx<0) return;
   epf[dsIdx].getTimeAndValue(timeIdx, stime, svalue); }

double FRAMES_API epfGetDuration(int dsIdx)
{  if (numepf<= dsIdx || dsIdx<0) return 0.0;
   return epf[dsIdx].duration; }

void FRAMES_API epfAggregate(char *filename, char* casList)
{
  char *dummy = new char[MAXPATH];

  eOut = new ocsv(filename, '"', ',', _CREATE_);
  eOut->alwaysQuote();
  *eOut << (epfGetNumRunInfo()+1) << NewLn;
  *eOut << "This file was modified by wrapspec.exe /out" << NewLn;
  for (int i = 0; i < epfGetNumRunInfo(); i++)
  {
    epfGetRunInfo(i+1,dummy);
    *eOut << dummy << NewLn;
  }
  *eOut << numepf << NewLn;
  for (int i = 0; i<numepf; i++)
  {
    epf[i].convertEPF(casList);
  }
  delete eOut;
  delete [] dummy;
  eOut = NULL;
  return;
}
