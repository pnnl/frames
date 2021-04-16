#include "newinfo.h"

FILE *mlog;

int ecount;
char epath[MAXFILE];

void LogStart(char *fname)
  {
    ecount=0;
    strcpy(epath,fname);
    strcat(epath,".err");
    mlog=fopen(epath,"a+");
  }

void LogStop()
  {
    fclose(mlog);
    if (ecount == 0)
      unlink(epath);
  }

char *infol(GIDFILE *p,char *s,int c1,int c2,
            int c3,int c4,int c5,int c6)
  {
    char *temp;
    temp=info(p,s,c1,c2,c3,c4,c5,c6);
    if (temp==NULL)
    {
      fprintf(mlog,"Parameter needed ");
      fprintf(mlog,"%s,%d,%d,%d,%d,%d,%d\n",s,c1,c2,c3,c4,c5,c6);
      ecount++;
    }
    return temp;
  }
