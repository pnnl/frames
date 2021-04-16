#include "error.h"
#include "fcsv.h"

fcsv *errout;
char errname[1024];
char errtemp[1024];
int errcnt;

fcsv *wrnout;
char wrnname[1024];
char wrntemp[1024];

void ErrOpen(char *fname)
{
  rstrcpy(errname,fname);
  strcat(errname,".err");
  try
  {
    errout = new fcsv;
    if (!errout->open(errname,WRITE,','))
      throw;
  }
  catch(...)
  {
    printf("Can't create error file!");
    printf("Press a key to exit");
    getch();
    exit(0);
  }
}

void Error(char *s1,char *s2,char *s3)
{
  sprintf(errtemp,"%s%s%s",s1,s2,s3);
  if (errout!=NULL)
  {
    errout->write(errtemp);
    errout->writeln();
    delete errout;
  }
  exit(0);
}

void ErrClose()
{
  errout->close();
  delete errout;
  unlink(errname);
}

void WrnOpen(char *fname)
{
  rstrcpy(wrnname,fname);
  strcat(wrnname,".wrn");
  try
  {
    wrnout = new fcsv;
    if (!wrnout->open(wrnname,WRITE,','))
      throw;
  }
  catch(...)
  {
    printf("Can't create warning file!");
    printf("Press a key to continue");
    getch();
  }
}

void Warning(char *s1, char *s2, char *s3)
{
  sprintf(wrntemp,"%s%s%s",s1,s2,s3);
  if (wrnout!=NULL)
  {
    wrnout->write(wrntemp);
    wrnout->writeln();
  }
  else              printf(wrntemp);
}

void WrnClose()
{
  wrnout->close();
  delete wrnout;
  wrnout = NULL;
}
