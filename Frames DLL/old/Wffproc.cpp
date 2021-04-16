#include "sumdata.h"

const int VADOSE=0;
const int AQUIFER=1;
const int SURFACEWATER=2;
const int OVERLAND=3;

//------------------------------------------------------------------------------
int Result::readWFFseries(icsv *fp, int set, int n, int FCount)
{
  int j;
  double ttime,tflux1,tflux2;
  double *ddvalue;

  if (dtime != NULL) delete[] dtime;
  if (dvalue != NULL) delete[] dvalue;
  dtime = new double[n+1];
  dvalue = new double[n+1];
  ddvalue = new double[n+1];
  dtime[0] = 0.0;
  dvalue[0] = 0.0;
  ddvalue[0] = 0.0;

  /* process water flux */
  for (j=0, num=0; j<n; j++, num++)
  {
    *fp >> ttime >> tflux1 >> tflux2 >> NewLn;
    // make sure the first time pt is zero
    if (ttime!=0.0 && j==0) num++;
    dtime[num] = ttime;
    dvalue[num] = tflux1;
    ddvalue[num] = tflux2;
  }

  computeResults();
  if (FCount==2)
  {
    delete[] dvalue;
    dvalue = ddvalue;
    computeResults();
  }
  else
    delete[] ddvalue;
  return 1;
}


int Result::procWFF(char *fuiname)
{
  int c,i,m,p;
  int numSet,numCon,numProg,n,FCount;
  char temp[100],tcasid[100],dummy[100];
  icsv *inf;

  vIdx = 0;
  sprintf(dummy,"%s.wff",fuiname);
  inf=new icsv(dummy,'\"');
  if (!inf->ok())
  {
    writeError("Error opening",dummy);
    return 0;
  }
  if (0!=inf->SeekSection(Source))
  {
    writeError(Source, "data not found in", dummy);
    delete inf;
    return 0;
  }
  *inf >> numSet >> NewLn;
  for (m=0; m<numSet; m++)
  {
    *inf >> dummy >> dummy;
    // scan off dimensions
    for (i=0;i<8;i++) *inf >> dummy;
    *inf >> numCon >> NewLn >> dummy;
    // attach units to labels
    sprintf(temp,"%s in %s",Labels[0],dummy);
    delete Labels[0];
    Labels[0] = strdup(temp);
    *inf >> dummy >> n >> NewLn;
    sprintf(temp,"%s in %s",Labels[1],dummy);
    delete Labels[1];
    Labels[1] = strdup(temp);

    if (0==strlen(CASID))
    {
      /* process water flux */
      if (Values==NULL) Values = new double[numSet*NumLabels];
      readWFFseries(inf,m,n,1);
    }
    else
    {
      /* skip water flux */
      inf->Skip(n);
      /* process contaminant flux */
      for (c=0; c<numCon; c++)
      {
        *inf >> dummy >> tcasid >> dummy >> dummy;
        *inf >> n >> FCount >> numProg >> NewLn;
        if (Values==NULL) Values = new double[numSet*NumLabels*FCount];
        if (0==rstrcmpi(tcasid,ParentID))
        {
          if (0==rstrcmpi(tcasid,CASID))
          {
            readWFFseries(inf,m,n,FCount);
          }
          else
          {
            inf->Skip(n);
            for (p=0; p<numProg; p++)
            {
              *inf >> dummy >> tcasid >> dummy >> dummy;
              *inf >> n >> FCount >> NewLn;
              if (0==rstrcmpi(tcasid,CASID))
                readWFFseries(inf,m,n,FCount);
              else
                inf->Skip(n);
            }
          }
        }
        else
        {
          /* skip parent contaminant */
          inf->Skip(n);
          /* process progeny */
          for (p=0; p<numProg; p++)
          {
            *inf >> dummy >> dummy >> dummy >> dummy >> n >> NewLn;
            inf->Skip(n);
          }
        }
      }
    }
  }
  delete inf;
  return 1;
}


