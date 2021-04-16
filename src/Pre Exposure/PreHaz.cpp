#pragma hdrstop
#include <condefs.h>

//---------------------------------------------------------------------------
USEUNIT("..\Common Files\robust.cpp");
USEUNIT("..\Common Files\fcsv.cpp");
USEUNIT("..\common files\gid.cpp");
USEUNIT("memhand.cpp");
USEUNIT("CHM.CPP");
USEUNIT("DFL.CPP");
USERES("PreHaz.res");
//---------------------------------------------------------------------------
#pragma argsused

#include "memhand.h"
#include "gid.h"
#include "dfl.h"
#include "chm.h"

fcsv err;
fcsv wrn;
GIDFILE *gid;

int SI=0;
int MI=0;
char fui[128]="";
char run[128]="";
char mod[40]="";

char errname[128]="";
char wrnname[128]="";
char gidname[128]="";

int model=1;

#define FGV(a,b) ratof(Get_Element_Value(a,b))

int Make_ID()
{
/*
  char *s;
  char temp[128];

  s = &run[strlen(run)-1];
  while (s>run && *s!='\\')
    s--;
  if (s[0] == '\\')
    s++;
  strncpy(temp,run,(int)(s-run));
  temp[(int)(s-run)] = 0;
  strcat(temp,"facil.id");
*/
  FILE *facil;

  if ((facil=fopen("facil.id","wt")) == NULL) return 0;
//  fprintf(facil," 1 1\n");
  fprintf(facil," 1 %s\n",fui);
//  fprintf(facil," 2 1\n");
  fprintf(facil," 2 %s\n",run);
//  fprintf(facil," 3 1\n");
  fprintf(facil," 3 %d\n",SI);
//  fprintf(facil," 4 1\n");
  fprintf(facil," 4 %d\n",MI);
//  fprintf(facil," 5 1\n");
  fprintf(facil," 5 %s\n",mod);
  fclose(facil);
  return 1;
}

void Make_POP()
{
  FILE *f;
  char fname[128];
  int i,j;
  paramrec p = {false,0,0,0,0,0,0,0,"","","","",""};

  rstrcpy(fname,run);
  strcat(fname,".pop");
  if ((f=fopen(fname,"wt")) == NULL) return;
  fprintf(f,"%-20s\n",info(gid,"sitename",SI,0,0,0,0,0));

  element *pop = Get_Element(gid,"pop");
  for (i=0; i<16; i++)
  {
    for (j=0; j<10; j++)
    {
      p.cnt1 = i+j*8;
      fprintf(f,"%8.0f",FGV(pop,&p));
    }
    fprintf(f,"\n");
  }
  fclose(f);
}

int preprocessor()
{
  if ((gid = Open_GID(gidname)) != NULL)
    if (Load_GID(gid,SI,mod))
    {
      Make_ID();
//      Make_DFL(gid,run);
      Make_CHM(gid,run,SI);
//      Make_POP();
      Close_GID(gid);
      return 1;
    }
  return 0;
}

int main (int argc,char **argv)
{
  if (argc < 6)
  {
    printf("Not enough parameters passed to program!\n");
    printf("Contact Battelle, Pacific Northwest National Labs.\n");
    return 0;
  }
  else
  {
    starthandler();
    rstrcpy(fui,argv[1]);
    rstrcpy(run,argv[2]);
    rstrcpy(errname,argv[2]);
    rstrcpy(wrnname,argv[2]);
    rstrcpy(gidname,argv[1]);
    SI = atoi(argv[3]);
    MI = atoi(argv[4]);
    rstrcpy(mod,argv[5]);

    strcat(errname,".err");
    strcat(wrnname,".wrn");
    strcat(gidname,".gid");
    err.open(errname,WRITE);
    err.write("Exposure Model");
    err.writeln();
    err.write("Error file for pre processing.");
    err.writeln();

    wrn.open(wrnname,WRITE);
    wrn.write("Warnings for AHAZ Model");
    wrn.writeln();

    if (!preprocessor())
    {
      err.write("Preprocessor failure!");
      err.writeln();
      err.close();
      wrn.close();
      exit(EXIT_FAILURE);
    }
    err.close();
    wrn.close();
    unlink(errname);
  }
  return 1;
}
