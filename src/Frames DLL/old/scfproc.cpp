#include "sumdata.h"

//------------------------------------------------------------------------------
void Result::procSCFloc(icsv *inf, char *dsname, char *dstype)
{
  int i,j,k,ot=0,ct,numcon,ntimes,nprog;
  double ttime,tconc;
  char msg[64];
  char locname[32], name[32], cas[32];
  char timeunit[32], concunit[32];
  char parentname[32], parentcas[32];

//  *inf >> locname;

  sprintf(this->descrip,"%s.%s.%s",dsname,dstype,locname);

  for (i=0;i<6;i++) *inf >> msg;
  *inf >> numcon >> NewLn;
  for (i=0; i<numcon; i++)
  {
    *inf >> name >> cas >> timeunit >> concunit >> ntimes >> nprog >> NewLn;
    if (!rstrcmpi(cas,Parent))
    {
      if (!rstrcmpi(cas,Contam))
      {
        dvalue = new double[(int)ntimes+1];
        dtime = new double[(int)ntimes+1];
        *dvalue=0.0;
        *dtime=0.0;
        /* process time,concentration pairs */
        for (j=0;j<ntimes;j++)
        {
          *inf >> ttime >> tconc >> NewLn;
          if (ttime!=0.0 && j==0) ot=1;
          ct = j+ot;
          *(dtime+ct)=ttime;
          *(dvalue+ct)=tconc;
        }
        num=ntimes+ot;
        return;
      }
      else {
        inf->Skip(ntimes);
        for (k=0;k<nprog;k++) {
          *inf >> name >> cas >> timeunit >> concunit >> ntimes ;
          *inf >> parentname >> parentcas >> NewLn;
          if (!rstrcmpi(cas,Contam)) {
            dvalue = new double[(int)ntimes+1];
            dtime = new double[(int)ntimes+1];
            *dvalue=0.0;
            *dtime=0.0;
            /* process water flux */
            for (j=0;j<ntimes;j++)
              {
              *inf >> ttime >> tconc >> NewLn;
              if (ttime!=0.0 && j==0) ot=1;
              ct = j+ot;
              *(dtime+ct)=ttime;
              *(dvalue+ct)=tconc;
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
        *inf >> name >> cas >> timeunit >> concunit >> ntimes ;
        *inf >> parentname >> parentcas >> NewLn;
        inf->Skip(ntimes);
      }
    }
  }
  sprintf(msg,"SCF Read Error: Chemical CASID=\"%s\" was never found.  Read CASID=\"%s\" last.",Contam,cas);
  writeError(msg);
}
//------------------------------------------------------------------------------
int Result::procSCF(char *fuiname)
{
  int i,numloc,nd,numds;
  char SCFName[MAXPATH],dsname[256],dstype[256];
  icsv *inf;
  Result *head=NULL;
  Result *tail=NULL;
  sprintf(SCFName,"%s.scf",fuiname);
  inf=new icsv(SCFName,'\"');
  if (!inf->ok())
  {
    writeError("Error opening",SCFName);
    return 0;
  }
  if (0!=inf->SeekSection(Source))
  {
    writeError(Source,"data not found in",SCFName);
    delete inf;
    return 0;
  }
  *inf >> numds >> NewLn;
  for (nd=0; nd<numds; nd++)
  {
    *inf >> dsname >> dstype; //  >> numloc >> NewLn;  locations are differenet datasets
    numloc = 1;
    for (i=0;i<numloc;i++)
    {
      if (nd==0 && i==0)
      {
        head=this;
        tail=this;
        procSCFloc(inf,dsname,dstype);
      }
      else
      {
        tail->next = new Result(head);
        tail = tail->next;
        tail->procSCFloc(inf,dsname,dstype);
      }
    }
  }
  delete inf;
  return head->num;
}

