#include "sumdata.h"
#include "atoclass.h"

bool Result::ATOReadSet(icsv *inf)
{
  int i,j;
  int ntime;
  double x,y;
  double ttime,tvalue;
  char output[SMALLSTRING];
  char moist[SMALLSTRING];
  char flux[SMALLSTRING];
  char delim[SMALLSTRING];
  bool found;
  ATOSet *data;

  clearfluxlist();
  data = new ATOSet();
  if (!data->Read(inf))
  {
    delete data;
    return false;
  }

  if (producer) delete producer;
  if (qual) delete qual;

  sscanf(OrgID,"%lf %c %lf",&y,delim,&x);
  if (!rstrcmpi(data->coordtype,"polar"))
    convertToCompass(x,y);

  if (!rstrncmpi(Type,"con",3))
  {
    rstrcpy(output,"air concentration");
    rstrcpy(moist,"");
    if (!rstrncmpi(&Type[18],"gas",3))
    { strncpy(flux,&Type[18],5);
      flux[5] = 0; }
    else
    { strncpy(flux,&Type[18],10);
      flux[10] = 0; }
  }
  else
  {
    rstrcpy(output,"deposition rate");
    if (!rstrncmpi(Type,"wet",3))
    { strncpy(moist,Type,3);
      moist[3] = 0; }
    else if (!rstrncmpi(Type,"dry",3))
    { strncpy(moist,Type,3);
      moist[3] = 0; }
    else if (!rstrncmpi(Type,"tot",3))
    { strncpy(moist,Type,5);
      moist[5] = 0; }

    if (!rstrncmpi(&Type[19],"gas",3))
    { strncpy(flux,&Type[19],5);
      flux[5] = 0; }
    else if (!rstrncmpi(&Type[21],"gas",3))
    { strncpy(flux,&Type[21],5);
      flux[5] = 0; }
    else if (!rstrncmpi(&Type[19],"par",3))
    { strncpy(flux,&Type[19],10);
      flux[10] = 0; }
    else
    { strncpy(flux,&Type[21],10);
      flux[10] = 0; }
  }

  ntime = 0;
  found = false;
  if (xunit) delete xunit;
  if (yunit) delete yunit;

  for (i = 0; i<data->numcons; i++)
    if (!strcmpi(ParentID,data->cons[i]->casid))
    {
      if (!rstrcmpi(CASID,data->cons[i]->casid))
      {
        ntime = data->cons[i]->getTimeSeries(output,flux,moist,x,y);
        found = true;
      }
      else
        for (j = 0; j<data->cons[i]->numprogs; j++)
          if (!rstrcmpi(CASID,data->cons[i]->progs[j]->casid))
          {
            ntime = data->cons[i]->progs[j]->getTimeSeries(output,flux,moist,x,y);
            found = true;
            break;
          }
      if (found)
      {
        producer = strdup(data->name);
        qual = new char[SMALLSTRING];
        sprintf(qual,"%s %s air",data->releasetype,data->coordtype);
        if (ntime > 0)
        {
          xunit = strdup(data->cons[i]->times[0]->unit);
          yunit = strdup(data->cons[i]->units);
        }
        else
        {
          xunit = strdup("");
          yunit = strdup("");
        }
        found = true;
      }
      break;
    }
  if (dtime) delete[] dtime;
  if (dvalue) delete[] dvalue;
  dtime = new double[(int)ntime+1];
  dvalue = new double[(int)ntime+1];
  *dtime = 0.0;
  *dvalue = 0.0;

  if (i < data->numcons)
    for (j = 0, num = 0; j<ntime; j++, num++)
    {
      ttime = data->cons[i]->getTimeSeriesTime(j);
      tvalue = data->cons[i]->getTimeSeriesValue(j);
      dtime[num] = ttime;
      dvalue[num] = tvalue;
    }
  if (num == 0) num++;
  computeResults();

  delete data;
  clearfluxlist();
  return found;
}

// handles wff, scf, wcf, bbf
//------------------------------------------------------------------------------
bool Result::CFReadSeries(icsv *inf, int ntime, int validx, char *xunt, char *yunt)
{
  int j,k;
  double ttime,tvalue;

  if (xunit) delete xunit;
  if (yunit) delete yunit;
  xunit = strdup(xunt);
  yunit = strdup(yunt);

  if (dtime) delete[] dtime;
  if (dvalue) delete[] dvalue;
  dtime = new double[(int)ntime+1];
  dvalue = new double[(int)ntime+1];
  *dtime = 0.0;
  *dvalue = 0.0;

  for (j = 0, num = 0; j<ntime; j++, num++)
  {
    *inf >> ttime;
    for (k = 0; k<validx; k++) *inf >> tvalue;
    *inf >> NewLn;

    dtime[num] = ttime;
    dvalue[num] = tvalue;
  }
  computeResults();
  return true;
}

bool Result::CFReadSet(icsv *inf)
{
  int h,i,j,validx;
  int ntime,norg,nchem,nprog,nflux;
  char cas[SMALLSTRING];
  char name[SMALLSTRING];
  char orgid[SMALLSTRING];
  //  char orgname[SMALLSTRING];
  char temp1[SMALLSTRING];
  char temp2[SMALLSTRING];
  bool found;

  // dataset intendee and file qualifier
  *inf >> temp1 >> temp2;

  if (consumer) delete consumer;
  if (qual) delete qual;
  consumer = strdup(temp1);
  qual = strdup(temp2);

  //scan off extra info for wff scf sets
  if (!rstrcmpi(ext,"wff"))
    for (i = 0; i<8; i++) *inf >> temp1; // extra info about source
  if (!rstrcmpi(ext,"scf"))
    for (i = 0; i<6; i++) *inf >> temp1; // extra info about source

  //set organism indice if any
  norg = 1;
  orgid[0] = '\0';
  //  orgname[0] = '\0';
  if (rstrcmpi(OrgID, "0") && strlen(OrgID) > 1)
    *inf >> norg >> NewLn;
  else
    OrgID[0] = 0;

  validx = 1;
  if (!rstrcmpi(Type,"Dissolved constituent flux"))
    validx = 2;

  if (!rstrcmpi(ext,"aff"))
  {
    if (qual) delete qual;
    qual = strdup("Air");
    inf->Skip(8); // extra info about source
    *inf >> nflux >> NewLn;
    validx = 0;
    for (i = 0; i<nflux; i++)
    {
      *inf >> temp1 >> NewLn;
      if (!rstrncmpi(temp1,Type,strlen(temp1)))
        validx = i+1;
    }
  }

  for (h = 0; h<norg; h++)
  {
    if (!rstrcmpi(ext,"bbf")) *inf >> orgid; // >> orgname;

    //read number of constituents
    *inf >> nchem >> NewLn;

    if (!rstrcmpi(ext,"wff"))
    {
      *inf >> temp1 >> temp2 >> ntime >> NewLn;
      if (!rstrcmpi(Type,"Water flux"))
        found = CFReadSeries(inf,ntime,validx,temp1,temp2);
      else
        inf->Skip(ntime);
    }

    for (i = 0; i<nchem; i++)
    {
      *inf >> name >> cas >> temp1 >> temp2 >> ntime;
      if (!rstrcmpi(ext,"wff")) *inf >> nflux;
      *inf >> nprog >> NewLn;
      if (!rstrcmpi(cas,ParentID) && !rstrcmpi(orgid,OrgID))
      {
        if (!rstrcmpi(cas,CASID))
          found = CFReadSeries(inf,ntime,validx,temp1,temp2);
        else
        {
          inf->Skip(ntime);
          for (j = 0; j<nprog; j++)
          {
            *inf >> name >> cas >> temp1 >> temp2 >> ntime;
            if (!rstrcmpi(ext,"wff")) *inf >> nflux;
            *inf >> NewLn;
            if (!rstrcmpi(cas,CASID))
              found = CFReadSeries(inf,ntime,validx,temp1,temp2);
            else
              inf->Skip(ntime);
          }
        }
      }
      else
      {
        inf->Skip(ntime);
        for (j = 0; j<nprog; j++)
        {
          *inf >> temp1 >> temp1 >> temp1 >> temp1 >> ntime >> NewLn;
          inf->Skip(ntime);
        }
      }
    }
  }
  return found;
}
//------------------------------------------------------------------------------


