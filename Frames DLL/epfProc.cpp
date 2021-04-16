#include "sumdata.h"

//hif only
//------------------------------------------------------------------------------
double Result::EPFReadPath(icsv *inf, int npt, int npath)
{
  int i,j;
  double risk;
  double value = 0.0;
  char temp1[SMALLSTRING];
  char temp2[SMALLSTRING];
  char temp3[SMALLSTRING];

  for (i = 0; i<npath; i++)
  {
    *inf >> temp1 >> temp2 >> temp3 >> NewLn;
    if (!rstrncmpi(Type,temp2,strlen(temp2)) && !rstrncmpi(OrgID,temp1,strlen(temp1)))
    {
      if (yunit == NULL)  yunit = strdup(temp3);
      for (j = 0; j<npt; j++)
      {
        // only add first media point
        // not enough info to use other points
        if (j == 0)
        {
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
bool Result::EPFReadSet(icsv *inf)
{
  int i,j,k,m,n;
  int ncon, ntime, nprog, npt, npath;
  double stime;
  bool found = false;
  char pcas[SMALLSTRING];
  char cas[SMALLSTRING];
  char name[SMALLSTRING];
  char temp1[SMALLSTRING];
  char temp2[SMALLSTRING];

  bool firsttime = true;


  // dataset type, producer and file qualifier
  *inf >> temp1 >> temp1 >> temp2 >> npt >> ncon >> NewLn;

  if (consumer) delete consumer;
  if (qual) delete qual;
  producer = strdup(temp1);
  qual = strdup(temp2);

  // scan off coordinates
  for (i = 0; i<npt; i++) *inf >> NewLn;

  // only accounting for last age group
  // should identify ages in UI by reading discreet example
  for (j = 0; j<ncon; j++)
  {
    *inf >> name >> pcas >> nprog >> ntime >> NewLn;
    if (!rstrcmpi(pcas,ParentID))
    {
      for (k = 0, num = 0; k<ntime; k++, num++)
      {
        rstrcpy(cas,pcas);
        *inf >> stime >> temp1 >> temp2 >> temp2 >> npath >> NewLn;
        for (m = 0; m<= nprog; m++)
        {
          if (m>0) *inf >> name >> cas >> npath >> NewLn;
          if (!rstrcmpi(cas,CASID))
          {
            if (k == 0)
            {
              found = true;
              if (xunit) delete xunit;
              if (yunit) delete yunit;
              xunit = strdup(temp1);
              yunit = NULL;
              // this is the duration unit
              // the value unit is read in HIFReadPath

              if (dtime) delete[] dtime;
              dtime = new double[(int)ntime+1];
              *dtime = 0.0;

              if (firsttime)
              {
                if (dvalue) delete[] dvalue;
                dvalue = new double[(int)ntime+1];
                for (n = 0; n<= ntime; n++)
                  dvalue[n] = 0.0;
                firsttime = false;
              }
            }
            dtime[num] = stime;
            dvalue[num] = EPFReadPath(inf, npt, npath);
          }
          else  // skip progeny
            inf->Skip(npath*(npt+1));
        }
      }
      computeResults();
    }
    else
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
  return found;
}
//------------------------------------------------------------------------------

