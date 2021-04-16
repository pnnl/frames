/*______________________________________________________________________________

   Date:       1993 - 2004
   Company:    Pacific Northwest National Laboratories
               Battelle Corporation
________________________________________________________________________________
__Modifiication  History________________________________________________________

  DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#include "frames.h"
#include "rifread.h"

icsv *rIn = NULL;
ocsv *rOut = NULL;
int numrif = 0;
long rifHead = 0;
static RIFSet *rif = NULL;

//------------------------------------------------------------------------------
//class RIFCon
RIFCon::~RIFCon()
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

RIFCon::RIFCon(int numTime)
{
   nProg = 0;
   prog = NULL;
   time = new fpos_t[numTime];
   nTime = numTime;
}

//------------------------------------------------------------------------------
RIFCon::RIFCon(icsv *rIn, int numLoc)
{
  int np,nt,numExp;
  char dummy[SMALLSTRING];

  nProg = 0;
  nTime = 0;
  prog = NULL;
  time = NULL;
  fpos = rIn->getpos();
  *rIn >> name >> casid >> nProg >> nTime >> NewLn;

  prog = new RIFCon*[nProg];
  time = new fpos_t[nTime];

  for (np = 0; np<nProg; np++)
    prog[np] = new RIFCon(nTime);

  for (nt = 0; nt<nTime; nt++)
  {
    time[nt] = rIn->getpos();
    *rIn >> dummy >> dummy >> dummy >> dummy >> numExp >> NewLn;
    rIn->Skip(numExp*2);
    for (np = 0; np<nProg; np++)
    {
      prog[np]->time[nt] = rIn->getpos();
      prog[np]->fpos = rIn->getpos();
      *rIn >> prog[np]->name >> prog[np]->casid >> numExp >> NewLn ;
      rIn->Skip(numExp*2);
    }
  }
}

double RIFCon::getTime(int timeIdx)
{
  double tmp = 0.0;
  if (nTime<= timeIdx || timeIdx<0) return 0;
  rIn->setpos(time[timeIdx]);
  *rIn >> tmp;
  return tmp;
}

int RIFCon::LoadExposuresByTime(char *cas, int timeIdx, int numLoc, expRoute **exp)
{
  int i;
  int numExp;
  char dummy[SMALLSTRING];
  char duration[SMALLSTRING];
  char population[SMALLSTRING];

  numExp = 0;
  rIn->setpos(time[timeIdx]);
  *rIn >> dummy >> dummy >> duration >> dummy >> numExp >> NewLn;
  if (rstrcmpi(casid,cas))
    for (i = 0; i<nProg; i++)
      if (!rstrcmpi(prog[i]->casid,cas))
      {
        rIn->setpos(prog[i]->time[timeIdx]);
        *rIn >> dummy >> dummy >> numExp >> NewLn;
        break;
      }

  if (numExp>0)
  {
    *exp = new expRoute[numExp];
    for (i = 0; i<numExp; i++)
      *rIn >> population >> (*exp)[i].path >> (*exp)[i].route >> (*exp)[i].unit >> (*exp)[i].measure >> NewLn >> NewLn;
  }
  return numExp;
}

//------------------------------------------------------------------------------
//class RIFAge
RIFAge::RIFAge(icsv *rIn, int numLoc, int numChem)
{
  int i;
  char dummy[SMALLSTRING];

  chem = NULL;
  fpos = rIn->getpos();
  *rIn >> agemin >> agemax >> dummy >> NewLn;
  nChem = numChem;
  chem = new RIFCon*[nChem];
  for (i = 0; i<nChem; i++)
    chem[i] = new RIFCon(rIn,numLoc);
}

RIFAge::~RIFAge()
{
  int i;

  if (chem)
  {
    for (i = 0; i<nChem; i++)
      delete chem[i];
    delete[] chem;
  }
  chem = NULL;
  nChem = 0;
}

//------------------------------------------------------------------------------
//class RIFSet
RIFSet::~RIFSet()
{
  if (x) delete[] x;
  if (y) delete[] y;

  if (rtime) delete[] rtime;
  if (rvalue) delete[] rvalue;

  if (exp) delete[] exp;

  if (age)
  {
    for (int i = 0; i<nAge; i++) delete age[i];
    delete[] age;
  }
  age = NULL;
  nAge = 0;
}

//------------------------------------------------------------------------------
RIFSet::RIFSet()
{
  nLoc = 0;
  nAge = 0;
  x = NULL;
  y = NULL;
  age = NULL;
  nExp = 0;
  exp = NULL;
  nTime = 0;
  rtime = NULL;
  rvalue = NULL;
}

void RIFSet::read(icsv *rIn)
{
  int j;
  int numChem;
  char dummy[1024];

  fpos = rIn->getpos();
  *rIn >> locExp >> locType >> locMedia >> nLoc >> nAge >> numChem >> NewLn;

  x = new double[nLoc];
  y = new double[nLoc];

  for (j = 0; j<nLoc; j++)
    *rIn >> x[j] >> dummy >> y[j] >> NewLn;

  age = new RIFAge*[nAge];
  for (j = 0; j<nAge; j++)
    age[j] = new RIFAge(rIn,nLoc,numChem);
}

int RIFSet::getDatasetInfo(int *numLoc, int *numAge, char *typ, char *med, char *exp)
{
  *numLoc = nLoc;
  *numAge = nAge;
  rstrcpy(typ, locType);
  rstrcpy(med, locMedia);
  rstrcpy(exp, locExp);
  return 1;
}

int RIFSet::getExposurePoint(int locIdx, double *xp, double *yp)
{
  if (nLoc<= locIdx || locIdx<0) return 0;
  *xp = x[locIdx];
  *yp = y[locIdx];
  return 1;
}

int RIFSet::getTimeCount(int ageIdx, char *parentCAS)
{
  int nc,j;
  int cnt1, cnt2;

  cnt1 = 0;
  cnt2 = 0;
  if (nAge<= ageIdx || ageIdx<0) return 0;
  for (nc = 0; nc<age[ageIdx]->nChem; nc++)
  {
    if (!rstrcmpi(age[ageIdx]->chem[nc]->casid,parentCAS))
      cnt1 = age[ageIdx]->chem[nc]->nTime;
    for (j = 0; j<age[ageIdx]->chem[nc]->nProg; j++)
      if (!rstrcmpi(age[ageIdx]->chem[nc]->prog[j]->casid,parentCAS))
        cnt2 = age[ageIdx]->chem[nc]->prog[j]->nTime;
  }
  if (cnt2 > cnt1) return cnt2;
  if (cnt1 >= cnt2) return cnt1;
  return 0;
}

double RIFSet::getTime(int ageIdx, char *parentCAS, int timeIdx)
{
  if (nAge<= ageIdx || ageIdx<0) return 0;
  for (int nc = 0; nc<age[ageIdx]->nChem; nc++)
    if (!rstrcmpi(age[ageIdx]->chem[nc]->casid,parentCAS))
      return age[ageIdx]->chem[nc]->getTime(timeIdx);
  return 0;
}

int RIFSet::getAgeGroup(int ageIdx, double *min, double *max)
{
  if (nAge<= ageIdx || ageIdx<0) return 0;
  *min = age[ageIdx]->agemin;
  *max = age[ageIdx]->agemax;
  return 1;
}

int RIFSet::LoadExposuresByTime(int ageIdx, char *cas, char *parentCAS, int timeIdx)
{
  int i;
  if (exp != NULL)
  {
    delete[] exp;
    exp = NULL;
  }
  nExp = 0;
  if (nAge<= ageIdx || ageIdx<0) return 0;

  for (i = 0; i<age[ageIdx]->nChem; i++)
    if (!rstrcmpi(age[ageIdx]->chem[i]->casid,parentCAS))
    {
      nExp = age[ageIdx]->chem[i]->LoadExposuresByTime(cas,timeIdx,nLoc,&exp);
      return nExp;
    }
  return 0;
}

void RIFSet::getExposure(int expIdx, char *path, char *route, char *measure, char *unit)
{
  if (exp != NULL && expIdx<nExp && expIdx>-1)
  {
    rstrcpy(path, exp[expIdx].path);
    rstrcpy(route, exp[expIdx].route);
    rstrcpy(measure, exp[expIdx].measure);
    rstrcpy(unit, exp[expIdx].unit);
  }
  else
  {
    rstrcpy(path, "");
    rstrcpy(route, "");
    rstrcpy(unit, "");
    rstrcpy(measure,  "");
  }
}

int RIFSet::LoadTimeSeries(int locIdx, int ageIdx, char *cas, char *parentCAS, char *path, char *route, char *measure, char *unit)
{
  double value, tot, start;
  int i, nc, np, nt, ne;
  int ct, ot;
  int numProg, numExp;
  bool found;

  char dummy[SMALLSTRING];
  char casid[SMALLSTRING];
  char prog[SMALLSTRING];
  char tPath[SMALLSTRING];
  char tRoute[SMALLSTRING];
  char tUnit[SMALLSTRING];
  char tMeasure[SMALLSTRING];

  if (rtime != NULL)  delete[] rtime;
  if (rvalue != NULL) delete[] rvalue;
  rtime = NULL;
  rvalue = NULL;
  nTime = 0;

  if (nAge<= ageIdx || ageIdx<0) return 0;
  if (nLoc<= locIdx || locIdx<0) return 0;

  found = true;
  for (nc = 0; nc<age[ageIdx]->nChem; nc++)
  {
    rIn->setpos(age[ageIdx]->chem[nc]->fpos);
    *rIn >> dummy >> casid >> numProg >> nTime >> NewLn;
    if (!rstrcmpi(casid, parentCAS))
    {
      rvalue = new double[(int)(nTime+1)];
      rtime = new double[(int)(nTime+1)];
      *rvalue = 0.0;
      *rtime = 0.0;
      ot = 0;
      ct = 0;
      for (nt = 0; nt<nTime; nt++)
      {
        tot = 0.0;
        *rIn >> start >> dummy >> duration >> dummy >> numExp >> NewLn;
        if (!rstrcmpi(casid,cas))
          for (np = 0; np<numExp; np++)
          {
            *rIn >> population >> tPath >> tRoute >> tUnit >> tMeasure >> NewLn;
            if (!rstrcmpi(measure,tMeasure) && !rstrcmpi(unit,tUnit) && !rstrcmpi(route,tRoute) &&
               (!rstrcmpi(path,"all") || !rstrcmpi(path,tPath)))
            {
              value = 0.0;
              found = true;
              for (i = 0; i<= locIdx && i<nLoc && nLoc>locIdx; i++)
                *rIn >> value;
              tot += value;
            }
            *rIn >> NewLn;
          }
        else
          rIn->Skip(2*numExp);
        for (np = 0; np<numProg; np++)
        {
          *rIn >> dummy >> prog >> numExp >> NewLn;
          if (!rstrcmpi(prog,cas))
            for (ne = 0; ne<numExp; ne++)
            {
              *rIn >> population >> tPath >> tRoute >> tUnit >> tMeasure >> NewLn;
              if (!rstrcmpi(measure,tMeasure) && !rstrcmpi(unit,tUnit) && !rstrcmpi(route,tRoute) &&
                 (!rstrcmpi(path,"all") || !rstrcmpi(path,tPath)))
              {
                value = 0.0;
                found = true;
                for (i = 0; i<= locIdx && i<nLoc && nLoc>locIdx; i++)
                  *rIn >> value;
                tot += value;
              }
              *rIn >> NewLn;
            }
          else
            rIn->Skip(2*numExp);
        }
        // not sure why first time must be 0.0
        // but the next two lines below assure time[0] = 0
        // commented out becaues we think it's not right BLH.MAP
        //if (start != 0.0 && nt == 0) ot = 1;
        ct = nt+ot;
        rvalue[ct] = tot;
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

int RIFSet::getTimeSeries(double *stime, double *svalue)
{
  int i;
  for (i = 0; i<nTime; i++)
  {
    stime[i] = rtime[i];
    svalue[i] = rvalue[i];
  }
  return nTime;
}

void RIFSet::getTimeAndValue(int timeIdx, double *stime, double *svalue)
{
  if (nTime<= timeIdx || timeIdx<0) return;
  *stime = rtime[timeIdx];
  *svalue = rvalue[timeIdx];
}

//------------------------------------------------------------------------------
// RIF API ---------------------------------------------------------------------
//------------------------------------------------------------------------------
bool FRAMES_API rifChecksum()
{  return (rIn->recordsCount == rIn->recordsRead); }

int FRAMES_API rifOpen(char *fuiname, char *modId)
{
  int i;
  char RIFName[MAXPATH];

  numrif = 0;
  sprintf(RIFName,"%s.rif",fuiname);
  rIn = new icsv(RIFName,'\"');
  if (!rIn->ok()) return 0;
  if (0 != rIn->SeekSection(modId, &rifHead)) return 0;
  *rIn >> numrif >> NewLn;
  rif = new RIFSet[numrif];
  for (i = 0; i<numrif; i++)
    rif[i].read(rIn);
  return numrif;
}

void FRAMES_API rifClose()
{
  if (rif) delete[] rif;
  if (rIn) delete rIn;
  numrif = 0;
  rif = NULL;
  rIn = NULL;
}

int FRAMES_API rifGetNumRunInfo()
{
   int num = 0;
   if (numrif<= 0) return 0;
   rIn->setpos(rifHead);
   *rIn >> num >> NewLn;
   return num;
}

int FRAMES_API rifGetRunInfo(int Idx, char *info)
{
   int num = rifGetNumRunInfo();
   if (num == 0) return 0;
   for (int i = 1; i<Idx; i++)
     *rIn >> NewLn;
   *rIn >> info >> NewLn;
   return 1;
}

int FRAMES_API rifGetSetInfo(int dsIdx, int *numLoc, int *numAge, char *typ, char *med, char *exp)
{  if (numrif<= dsIdx || dsIdx<0) return 0;
   return rif[dsIdx].getDatasetInfo(numLoc, numAge, typ, med, exp); }

int FRAMES_API rifGetExposurePoint(int dsIdx, int locIdx, double *xp, double *yp)
{  if (numrif<= dsIdx || dsIdx<0) return 0;
   return rif[dsIdx].getExposurePoint(locIdx, xp, yp); }

int FRAMES_API rifGetTimeCount(int dsIdx, int ageIdx, char *parentCAS)
{  if (numrif<= dsIdx || dsIdx<0) return 0;
   return (rif[dsIdx].getTimeCount(ageIdx,parentCAS)); }

double FRAMES_API rifGetTime(int dsIdx, int ageIdx, char *parentCAS, int timeIdx)
{  if (numrif<= dsIdx || dsIdx<0) return 0.0;
   return (rif[dsIdx].getTime(ageIdx, parentCAS, timeIdx)); }

int FRAMES_API rifGetAgeGroup(int dsIdx, int ageIdx, double *min, double *max)
{  if (numrif<= dsIdx || dsIdx<0) return 0;
   return rif[dsIdx].getAgeGroup(ageIdx, min, max); }

int FRAMES_API rifLoadRoutesAndPathwaysByTime(int dsIdx, int ageIdx, char *cas, char *parentCAS, int timeIdx)
{  if (numrif<= dsIdx || dsIdx<0) return 0;
   return rif[dsIdx].LoadExposuresByTime(ageIdx, cas, parentCAS, timeIdx); }

void FRAMES_API rifGetRouteAndPathway(int dsIdx, int expIdx, char *path, char *route, char *measure, char *unit)
{  if (numrif<= dsIdx || dsIdx<0) return;
   rif[dsIdx].getExposure(expIdx, path, route, measure, unit); }

int FRAMES_API rifLoadTimeSeries(int dsIdx, int locIdx, int ageIdx, char *cas, char *parentCAS, char *path, char *route, char *measure, char *unit)
{  if (numrif<= dsIdx || dsIdx<0) return 0;
   return rif[dsIdx].LoadTimeSeries(locIdx, ageIdx, cas, parentCAS, path, route, measure, unit); }

void FRAMES_API rifGetTimeSeries(int dsIdx, double *stime, double *svalue)
{  if (numrif<= dsIdx || dsIdx<0) return;
   rif[dsIdx].getTimeSeries(stime, svalue); }

void FRAMES_API rifGetTimeAndValue(int dsIdx, int timeIdx, double *stime, double *svalue)
{  if (numrif<= dsIdx || dsIdx<0) return;
   rif[dsIdx].getTimeAndValue(timeIdx, stime, svalue); }

double FRAMES_API rifGetDuration(int dsIdx)
{  if (numrif<= dsIdx || dsIdx<0) return 0.0;
   return rif[dsIdx].duration; }

double FRAMES_API rifGetPopulation(int dsIdx)
{  if (numrif<= dsIdx || dsIdx<0) return 0.0;
   return rif[dsIdx].population; }

void FRAMES_API rifAggregate(char *filename, char* casList)
{
  char *dummy = new char[MAXPATH];

  rOut = new ocsv(filename, '"', ',', _CREATE_);
  rOut->alwaysQuote();
  *rOut << rifGetNumRunInfo() << NewLn;
  for (int i = 0; i < rifGetNumRunInfo(); i++)
  {
    rifGetRunInfo(i+1,dummy);
    *rOut << dummy << NewLn;
  }
  *rOut << numrif << NewLn;
  for (int i = 0; i<numrif; i++)
  {
    rif[i].convertRIF(casList);
  }
  delete rOut;
  delete [] dummy;
  rOut = NULL;
 return;
}

void RIFSet::aggregateExposures(int ageIdx, char *cas, char *parentCas, int numTimes, bool newAggregate)
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
    LoadExposuresByTime(ageIdx, cas, parentCas, t);
    for (int m = 0; m< nExp; m++)
    {
      std::string key = exp[m].path;
      key += exp[m].route;
      key += exp[m].measure;
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

int RIFSet::convertRIF(char *casList)
{
  char *mylist = strdup(casList);
  char *chemName;
  int numTimes;

  splitList(mylist);
  // dataset info
  *rOut << locExp << locType << locMedia << nLoc << nAge << (long)rList.size() << NewLn;
  // location info
  for (int l = 0; l < nLoc; l++)
    *rOut << x[l] << "km" << y[l] << "km" << NewLn;

  for (int a = 0; a<nAge; a++)
  {
    // age range info
    *rOut << age[a]->agemin << age[a]->agemax << "yr" << NewLn;
    // loop constituents
    for (int k = 0; k< (long)rList.size(); k++)
    {
      aggregateExposures(a,rList[k],rList[k],getTimeCount(a,rList[k]),true);
      for (int i = 0; i<age[a]->nChem; i++)
      {
        if (!rstrcmpi(rList[k], age[a]->chem[i]->casid))
          chemName = age[a]->chem[i]->name;
        for (int j = 0; j<age[a]->chem[i]->nProg; j++)
          if (!rstrcmpi(age[a]->chem[i]->prog[j]->casid,rList[k]))
          {
            chemName = age[a]->chem[i]->prog[j]->name;
            aggregateExposures(a,age[a]->chem[i]->prog[j]->casid,
                                 age[a]->chem[i]->casid,
                                 getTimeCount(a,age[a]->chem[i]->prog[j]->casid),false);
          }
      }
      numTimes = getTimeCount(a,rList[k]);
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
          numTimes = LoadTimeSeries(l, a, rList[k], rList[k],
             (*it).second->path, (*it).second->route,
             (*it).second->measure, (*it).second->unit);
          for (int t = 0; t<numTimes; t++)
            tempValues[l][t][m] = rvalue[t];

          for (int i = 0; i<age[a]->nChem; i++)
            for (int j = 0; j<age[a]->chem[i]->nProg; j++)
              if (!rstrcmpi(age[a]->chem[i]->prog[j]->casid,rList[k]))
              {
                int numTimes = LoadTimeSeries(l, a, rList[k], age[a]->chem[i]->casid,
                  (*it).second->path, (*it).second->route,
                  (*it).second->measure, (*it).second->unit);
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
          delete[] tempValues[l][t];
        delete[] tempValues[l];
      }
      delete[] tempValues;
    }
  }

  delete mylist;
  return 0;
}

int RIFSet::writeConstituent(char *cas, char *name, int numTimes)
{
  if (rtime)
  {
    //write chemheader
    //ChemName, ChemCAS, NumProg = 0, numtimes
    *rOut << name<< cas  << 0 << numTimes << NewLn;
    //loop times
    for (int t = 0; t<numTimes; t++)
    {
      // load exposures for current chemical
      *rOut << rtime[t] << "yr" << duration << "yr" << (long)erp.size() << NewLn;
      //loop exposures
      int m = 0;
      for (_EXPLIST_IT it = erp.begin(); it != erp.end(); ++it)
      {
        //write pathname, routename, unitname
        *rOut <<  population
              << (*it).second->path
              << (*it).second->route
              << (*it).second->unit
              << (*it).second->measure << NewLn;
        //loop locations
        for (int l = 0; l < nLoc; l++)
          *rOut << tempValues[l][t][m];
        *rOut << NewLn;
        m++;
      }
    }
  }
  else
    //ChemName, ChemCAS, NumProg = 0, numtimes = 0
    *rOut << name<< cas  << 0 << numTimes << NewLn;
  return 0;
}
