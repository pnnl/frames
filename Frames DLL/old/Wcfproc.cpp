#include "sumdata.h"

//------------------------------------------------------------------------------
void Result::procWCFloc(icsv *inf)
{
  int i,j,k,ot=0,ct,numcon,ntimes,nprog;
  double ttime,tflux;
  char msg[64];
  char locname[32], loctype[32], name[32], cas[32];
  char timeunit[32],concunit[32];
  char parentname[32], parentcas[32];
  char dummy[100];

  *inf >> locname >> loctype >> numcon >> NewLn;
  sprintf(this->descrip,"%s.%s",locname,loctype);
  for (i=0;i<numcon;i++)
  {
    *inf >> name >> cas >> timeunit >> concunit >> ntimes >> nprog >> NewLn;
    if (!rstrcmpi(cas,ParentID))
    {
      if (!rstrcmpi(cas,CASID))
      {
        dvalue = new double[(int)ntimes+1];
        dtime = new double[(int)ntimes+1];
        *dvalue=0.0;
        *dtime=0.0;
        /* process water flux */
        for (j=0;j<ntimes;j++)
        {
          *inf >> ttime >> tflux >> NewLn;
          if (ttime!=0.0 && j==0) ot=1;
          ct = j+ot;
          *(dtime+ct)=ttime;
          *(dvalue+ct)=tflux;
        }
        num=ntimes+ot;
        return;
        }
      else {
        inf->Skip(ntimes);
        for (k=0;k<nprog;k++) {
          *inf >> name >> cas >> timeunit >> concunit >> ntimes ;
          *inf >> parentname >> parentcas >> NewLn;
          if (!rstrcmpi(cas,CASID)) {
            dvalue = new double[(int)ntimes+1];
            dtime = new double[(int)ntimes+1];
            *dvalue=0.0;
            *dtime=0.0;
            /* process water flux */
            for (j=0;j<ntimes;j++)
              {
              *inf >> ttime >> tflux >> NewLn;
              if (ttime!=0.0 && j==0) ot=1;
              ct = j+ot;
              *(dtime+ct)=ttime;
              *(dvalue+ct)=tflux;
              }
            num=ntimes+ot;
            return;
          }
        else
          inf->Skip(ntimes);
        }
      }
    }
    else
      {
      inf->Skip(ntimes);
      for (j=0;j<nprog;j++)
        {
        *inf >> dummy >> dummy >> dummy >> dummy >> ntimes >> NewLn;
        inf->Skip(ntimes);
        }
      }
    }
  sprintf(msg,"WCF Read Error: Chemical CASID=\"%s\" was never found!",CASID);
  writeError(msg);
}
//------------------------------------------------------------------------------
int Result::procWCF(char *fuiname)
{
  int i,numSet;
  char WCFName[MAXPATH];
  icsv *inf;
  Result *head=NULL;
  Result *tail=NULL;

  sprintf(WCFName,"%s.wcf",fuiname);
  inf=new icsv(WCFName,'\"');
  if (!inf->ok())
  {
    writeError("Error opening",WCFName);
    return 0;
  }
  if (0!=inf->SeekSection(Source))
  {
    writeError(Source,"data not found in",WCFName);
    return 0;
  }
  *inf >> numSet >> NewLn;
  for (i=0;i<numSet;i++)
  {
    if (i==0)
    {
      head=this;
      tail=this;
      procWCFloc(inf);
    }
    else
    {
      tail->next = new Result(head);
      tail = tail->next;
      tail->procWCFloc(inf);
    }
  }
  delete inf;
  return head->num;
}

