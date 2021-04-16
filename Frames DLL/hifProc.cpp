#include "sumdata.h"

//hif only
//------------------------------------------------------------------------------
double Result::HIFReadPath(icsv *inf, int npt, int npath, int epindex)
{
  int i,j,k;
  double risk;
  double value = 0.0;
  char temp1[SMALLSTRING];
  char temp2[SMALLSTRING];
  char temp3[SMALLSTRING];
  char temp4[SMALLSTRING];
  char type1[MEDSTRING];
  char type2[MEDSTRING];
  char type3[MEDSTRING];


  for (i = 0; i<npath; i++)
  {
    *inf >> temp1 >> temp1 >> temp2 >> temp3 >> temp4 >> NewLn;
    sprintf(type1,"%s %s", temp2, temp4);
    sprintf(type2,"%s %s %s", temp1, temp2, temp4);
    sprintf(type3,"summed %s",temp4);
    if ((!rstrncmpi(Type,type1,strlen(type1)) && !rstrncmpi(OrgID,temp1,strlen(temp1))) ||
         !rstrncmpi(Type,type2,strlen(type2)) ||
         !rstrncmpi(Type,type3,strlen(type3)))
    {
      if (yunit == NULL)  yunit = strdup(temp4);
      for (j = 0; j<npt; j++)
      {
        // only add first media point
        // not enough info to use other points
        if (j == 0)
        {
          for (k = 0; k<=epindex; k++)
            *inf >> risk;
          value += risk;
        }
        *inf >> NewLn;
      }
    }
    else
      inf->Skip(npt);
  }
  return value;
}
//------------------------------------------------------------------------------
Result *Result::HIFReadSet(icsv *inf)
{
  int i,j,k,m,n;
  int ncancer, ndose, nage, ncon;
  int ntime, nprog, npt, npath, oindex;
  double stime;
  double startage;
  double endage;
  bool firsttime;
  bool found = false;
  char pcas[SMALLSTRING];
  char cas[SMALLSTRING];
  char name[SMALLSTRING];
  char temp1[SMALLSTRING];
  char temp2[SMALLSTRING];
  char *typ;
  char *grp = "Age Group";

  bool sumAll;
  Result *curr = this;
  typ = strdup(Des);
  sumAll = !rstrcmpi("",CASID);

  // dataset type, producer and file qualifier
  *inf >> temp1 >> temp1 >> temp2 >> npt;
  *inf >> nage >> ncon >> ncancer >> ndose >> NewLn;

  if (consumer) delete consumer;
  if (qual) delete qual;
  producer = strdup(temp1);
  qual = strdup(temp2);

  oindex = 0;
  if (rstrcmpi(Type,"summed Sv") && rstrcmpi(Type,"Sv"))
    // names of cancer effect organs
    for (i = 0; i<ncancer; i++)
    {
      *inf >> temp1;
      if (!rstrcmpi(temp1,OrgID)) oindex = i;
    }
  *inf >> NewLn;

  if (strstr(Type," Sv"))
    // names radiation dose organs
    for (i = 0; i<ndose; i++)
    {
      *inf >> temp1;
      if (!rstrcmpi(temp1,OrgID)) oindex = i;
    }
  *inf >> NewLn;

  // scan off coordinates currently only first location used
  // see HIRReadPath
  for (i = 0; i<npt; i++) *inf >> NewLn;

  // accounting for all age groups by adding a new result
  // should identify ages in UI by reading discreet example
  for (i = 0; i<nage; i++)
  {
    firsttime = true;
    // would check for age here add or add new result
    *inf >> startage >> endage >> NewLn;
    if (i != 0)
    {
      if (curr->next == NULL)
      {
        try
        {
          curr->next = new Result(home);
          if (curr->next->consumer) delete curr->next->consumer;
          if (curr->next->qual) delete curr->next->qual;
          curr->next->producer = strdup(producer);
          curr->next->qual = strdup(qual);
        }
        catch(...)
        {
          writeError("Unable to create new result!");
          delete typ;
          curr->next = NULL;
          return NULL;
        }
      }
      curr = curr->next;

      sprintf(temp1, "%s%2d",grp, i+1);
      if (rstrncmpi(curr->Labels[0], temp1, strlen(temp1)))
      {
        for (j = 0; j<NumValues; j++)
        {
          sprintf(temp1, "%s%2d, %s",grp, i+1, curr->Labels[j] + 13);
          delete curr->Labels[j];
          curr->Labels[j] = strdup(temp1);
        }
        sprintf(temp1, "%s, %s%2d", typ, grp, i+1);
        delete curr->Des;
        curr->Des = strdup(temp1);
      }
    }

    if (rstrncmpi(curr->Labels[0], grp, 9))
    {
      for (j = 0; j<NumValues; j++)
      {
        sprintf(temp1, "%s%2d, %s", grp, i+1, curr->Labels[j]);
        delete curr->Labels[j];
        curr->Labels[j] = strdup(temp1);
      }
      sprintf(temp1, "%s, %s%2d", typ, grp, i+1);
      delete curr->Des;
      curr->Des = strdup(temp1);
    }

    for (j = 0; j<ncon; j++)
    {
      *inf >> name >> pcas >> nprog >> ntime >> NewLn;
      if (!rstrcmpi(pcas,ParentID) || sumAll)  // && i == 0)
      {
        for (k = 0, curr->num = 0; k<ntime; k++, curr->num++)
        {
          rstrcpy(cas,pcas);
          *inf >> stime >> temp1 >> temp2 >> temp2 >> npath >> NewLn;
          for (m = 0; m<= nprog; m++)
          {
            if (m>0) *inf >> name >> cas >> npath >> NewLn;
            if (!rstrcmpi(cas,curr->CASID) || sumAll)
            {
              if (k == 0)
              {
                found = true;
                if (curr->xunit) delete curr->xunit;
                if (curr->yunit) delete curr->yunit;
                curr->xunit = strdup(temp1);
                curr->yunit = NULL;
                // this is the duration unit
                // the value is read in HIFReadPath

                if (curr->dtime) delete[] curr->dtime;
                curr->dtime = new double[(int)ntime+1];
                curr->dtime[0] = 0.0;

                if (firsttime || !sumAll)
                {
                  if (curr->dvalue) delete[] curr->dvalue;
                  curr->dvalue = new double[(int)ntime+1];
                  for (n = 0; n<= ntime; n++)
                    curr->dvalue[n] = 0.0;
                  firsttime = false;
                }
              }
              curr->dtime[curr->num] = stime;

              if (sumAll)
                curr->dvalue[curr->num] += curr->HIFReadPath(inf, npt, npath, oindex);
              else
                curr->dvalue[curr->num] = curr->HIFReadPath(inf, npt, npath, oindex);
            }
            else  // skip progeny
              inf->Skip(npath*(npt+1));
          }
        }
      }
      else
      {
        for (k = 0; k<ntime; k++)
        {
          *inf >> temp1 >> temp1 >> temp1 >> temp1 >> npath >> NewLn;
          for (m = 0; m<= nprog; m++)
          {
            if (m>0) *inf >> temp1 >> temp1 >> npath >> NewLn;
            inf->Skip(npath*(npt+1));
          }
        }
      }
    }
    curr->computeResults();
  }
  delete typ;
  if (found) return curr;
  return NULL;
}
//------------------------------------------------------------------------------

