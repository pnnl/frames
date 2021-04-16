#include "fileio.h"
#include <stdlib.h>
#include <stdio.h>
#include <io.h>
#include <fcntl.h>
#include "memhand.h"
#include <string.h>
#include <dos.h>
#include <direct.h>

static char tempstr[64];

int copy(char *d,char *s)
  {
    char line[255];
    int t;
    FILE *in,*out;
    in=fopen(s,"rb");
      if (in==NULL) return 0;
    out=fopen(d,"wb");
    do
      {
	t=fread(line,sizeof(char),255,in);
	fwrite(line,sizeof(char),t,out);
      }
    while (t==255);
    fclose(in);
    fclose(out);
    return 1;
  }

int exist(char *s)
  {
   // FILE *f;
    int handle;
    handle=open(s,O_RDONLY);
    if (handle==-1)
      return 0;
    else
      {
        close(handle);
        return 1;
      }
  }

static char **names;

char **FindFiles(char *search,int *i)
  {
    int t,j;
    struct find_t *f,fblk;

    f=&fblk;
    t=_dos_findfirst(search,0,f);
    j=0;
    while (t==0)
      {
        t=_dos_findnext(f);
        j++;
      }
    names=(char **)getmem(sizeof(char *)*j);
    t=_dos_findfirst(search,0,f);
    j=0;
    while (t==0)
      {
        names[j]=strdup(f->name);
        t=_dos_findnext(f);
        j++;
      }
    *i=j;
    return names;
  }

char **FindPRMFiles(int *i)
  {
    int t,j;
    struct find_t *f,fblk;
 //   char *ext;

    f=&fblk;
    t=_dos_findfirst("*.prm",0,f);
    j=0;
    while (t==0)
      {
        j++;
        t=_dos_findnext(f);
      }
    names=(char **)getmem(sizeof(char *)*j);
    t=_dos_findfirst("*.prm",0,f);
    j=0;
    while (t==0)
      {
        names[j]=strdup(f->name);
        j++;
	      t=_dos_findnext(f);
      }
    *i=j;
    return names;
  }

void FreeFiles(int i,char **names)
  {
    int j;
    for (j=0;j<i;j++)
      free(names[j]);
    putmem(names);
  }

void filenameparse(char *line,char *drive,char *directory,char *name)
  {
    char fname[9],ext[5],tempdir[64];
    _splitpath(line,drive,directory,fname,ext);
    sprintf(name,"%s%s",fname,ext);
    if (strlen(drive)==0)
      sprintf(drive,"%c:",'A'+(char)_getdrive()-1);
    if (drive[0]>=97) drive[0]=drive[0]-(char)32;
    if (strlen(directory)==0)
      {
        _getdcwd((int)(drive[0]-'A'+1),tempdir,64);
        strcpy(directory,&tempdir[2]);
      }
    if (directory[strlen(directory)-1]=='\\')
      directory[strlen(directory)-1]='\0';
  }

char *pathfile(char *s1,char *s2)
  {
    sprintf(tempstr,"%s\\%s",s1,s2);
    return tempstr;
  }

char *drvpathfile(char *s1,char *s2,char *s3)
  {
    sprintf(tempstr,"%s%s\\%s",s1,s2,s3);
    return tempstr;
  }

void current(char *filepath,char *drive)
  {
    char buffer[64];

    getcwd(buffer,64);
    strncpy(drive,buffer,2);
    drive[2]='\0';
    strcpy(filepath,&(buffer[2]));
  }

int changeto(char *filepath,char *drive)
  {
    int t;
    unsigned ndrives;
    _dos_setdrive((int)(drive[0]-'A'+1),&ndrives);
    if (strcmp(filepath,""))
      t=chdir(filepath); /* can't find equivalent */
    else
      t=0;
    return !t;
  }

unsigned int crc(char *filename)
  {
    FILE *f;
    char buffer[512];
    int n,total,i;
    f=fopen(filename,"rb");
    if (f==NULL) return 0;
    total=0;
    do
      {
        n=fread(buffer,1,512,f);
        for (i=0;i<n;i++)
          total+=buffer[i];
      }
    while (n==512);
    fclose(f);
    return total;
  };
