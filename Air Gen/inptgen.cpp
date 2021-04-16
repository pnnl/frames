#include <dir.h>
#include <stdlib.h>
#include <string.h>
#include "robust.h"
#include "memhand.h"
#include "gid.h"
#include "newinfo.h"
#include "met2.h"
#include "top2.h"
#include "jfd2.h"
#include "fileio.h"
#include "p2met.h"
#include "p2top.h"
#include "p2jfd.h"
#include "menusmal.h"

extern char *logfilename(char*);

extern unsigned _stklen = 16384U;
GIDFILE *Param;

char filename[MAXPATH]="None";

void prmtoid(char *s)
  {
    FILE *facil;

    facil=fopen("facil.id","wt");
    fprintf(facil," 5 Mepas Shell 1992\n");
    fprintf(facil," 6 %s.chm\n",s);
    fprintf(facil," 7 %s.chm\n",s);
    fprintf(facil," 0 %-8s  %s%s\n",s,s,s);
    fprintf(facil," 3  1,Number of waste units\n");
    fprintf(facil," 3 %-8s\n",s);
    fprintf(facil," 2  1,\n");
    fprintf(facil," 2 %-8s  %-8s  %-8s\n",s,s,s);
    fprintf(facil," 4  1,\n");
    fprintf(facil," 4 %-8s  ZEROES    %-8s  %-8s \n",s,s,s);
    fclose(facil);
  }

void prmtomet(GIDFILE *PF,int Site,int Air,char *fname)
  {
    char temp[MAXPATH];
		char drive[MAXDRIVE];
		char dir[MAXDIR];
		char file[MAXFILE];
		char ext[MAXEXT];

		METFILE *mf;

		fnsplit(fname,drive,dir,file,ext);

		strcpy(temp,fname);
    strcat(temp,".met");
    mf=openmet(temp,"wt");
    testmem(mf);
		makemet(PF,mf,Site,Air,fname);
    writemet(mf);
    closemet(mf);
  }

void prmtodet(GIDFILE *PF,int Site,char *fname)
  {
		char temp[MAXPATH];
    char *cname;
    FILE *f;
    int i,j,t,NumCon,Total;

    strcpy(temp,fname);
    strcat(temp,".det");
    f=fopen(temp,"wt");
    NumCon=ratoi(infol(PF,"NumCon",Site));
    Total=NumCon;
    for (i=0;i<NumCon;i++)
      Total+=ratoi(infol(PF,"NDS",Site,i+1));
    fprintf(f,"%3d\n",Total);
    for (i=0;i<NumCon;i++)
      {
        fprintf(f,"%-12s",infol(PF,"FSCASID",Site,i+1));
        cname=infol(PF,"FSCNAME",Site,i+1);
        cname[20]='\0';
        fprintf(f,"%-20s",cname);
        fprintf(f,"%3d   ",ratoi(infol(PF,"CLKTYPE",Site,i+1)));
        fprintf(f,"%3d\n",ratoi(infol(PF,"CLCLASS",Site,i+1)));
        t=ratoi(infol(PF,"NDS",Site,i+1));
        for (j=0;j<t;j++)
          {
            fprintf(f,"%-12s",infol(PF,"FSCASID",Site,i+1,j+1));
            cname=infol(PF,"FSCNAME",Site,i+1,j+1);
            cname[20]='\0';
            fprintf(f,"%-20s",cname);
            fprintf(f,"%3d   ",ratoi(infol(PF,"CLKTYPE",Site,i+1,j+1)));
            fprintf(f,"%3d\n",ratoi(infol(PF,"CLCLASS",Site,i+1,j+1)));
          }
        }
    fclose(f);
  }

void prmtosou(GIDFILE *PF,int Site,int Air,char *fname,char *gname)
  {
		char temp[MAXPATH];
    char sname[32];
    int NumCon,ANumCon,NDS;
    FILE *f;
    int i,j;

    strncpy(sname,info(PF,"AirSrcName",Site,Air,1),32);
    sname[32]='\0';
    NumCon=ratoi(infol(PF,"NumCon",Site));
    ANumCon=NumCon;
    for (i=0;i<NumCon;i++)
      ANumCon+=ratoi(infol(PF,"NDS",Site,i+1));
    strcpy(temp,fname);
    strcat(temp,".sou");
    f=fopen(temp,"wt");
    fprintf(f,"%2d%2d%2d%2d%2d%2d\n",7,ANumCon,4,4,0,0);
    for (i=0;i<NumCon;i++)
      {
        fprintf(f,"%-12s",infol(PF,"FSCASID",Site,i+1));
        fprintf(f,"%2d",7);
        fprintf(f,"%1d",ratoi(infol(PF,"CLCLASS",Site,i+1)));
        fprintf(f,"%10.3g",0.0);
        fprintf(f,"%10.3g",0.0);
        fprintf(f,"%10.3g",0.0);
        fprintf(f,"%10.3g",0.0);
        fprintf(f,"%10.3g\n",0.0);
        NDS=ratoi(infol(PF,"NDS",Site,i+1));
        for (j=0;j<NDS;j++)
          {
            fprintf(f,"%-12s",infol(PF,"FSCASID",Site,i+1,j+1));
            fprintf(f,"%2d",-1);
            fprintf(f,"%1d",ratoi(infol(PF,"CLCLASS",Site,i+1,j+1)));
            fprintf(f,"%10.3g",0.0);
            fprintf(f,"%10.3g",0.0);
            fprintf(f,"%10.3g\n",0.0);
          }
      }
    fprintf(f,"%s\n",sname);
    fprintf(f,"\n");
    fprintf(f,"%2d\n",9);
    fclose(f);
  }

void prmtotop(GIDFILE *PF,int Site,int Air,char *fname)
  {
    char temp[MAXPATH];
    TOPFILE *tf;

    strcpy(temp,fname);
    strcat(temp,".top");
    tf=opentop(temp,"wt");
    testmem(tf);
    maketop(PF,tf,Site,Air);
    writetop(tf);
    closetop(tf);
  }

void prmtojfd(GIDFILE *PF,int Site,int Air,char *fname)
  {
    char temp[MAXPATH];
    JFDFILE *jf;

    strcpy(temp,fname);
    strcat(temp,".jfd");
    jf=openjfd(temp,"wt");
    testmem(jf);
    makejfd(PF,jf,Site,Air);
    writejfd(jf);
    closejfd(jf);
  }

void exitmessage(void)
  {
		char *errmes[4]=
      {
        "Usage: airgen FUIName RunName Site# Air# GlyphName",
        "See framework specification Call Arquments for ",
        "description of these parameters.",
        "Press any key to exit"
      };
    Message(4,errmes);
    ExitApplication(1);
  }

void memhand()
  {
    char *outmemmes[2]=
      {
        "Out of Memory",
        "Press any key to Exit Inputgen"
      };
    Close_GID(Param);
    Message(2,outmemmes);
    ExitApplication(1);
  }

void nullhand()
  {
    char *memmes[2]=
      {
        "NULL memory pointer",
        "Press any key to Exit Inputgen"
      };
    Close_GID(Param);
    Message(2,memmes);
    ExitApplication(1);
  }

int any(char *s,char c)
  {
		while (*s!=c && *s!='\0')
			s++;
    if (*s==c) return 1;
    else return 0;
  }

void copyaff(char *oldn,char *newn)
  {
    char sourc[MAXPATH],dest[MAXPATH];
    sprintf(sourc,"%s.aff",oldn);
    sprintf(dest,"%s.aff",newn);
    copy(dest,sourc);
  }

int  main(int argc,char **argv)
  {
    int t,Site,Air;
    char GIDName[MAXPATH];
    char *myname = "~mepair";
    char *cantfilemes[4]=
      {
        "File",
        NULL,
        "Could not be found",
        "Press any key to exit"
      };
     char *openmes[2]=
       {
         "Opening",
         "Global Input Data file"
       };
     char *genmes[2]=
       {
         "Generating",
         "Air input files"
       };
     char *closemes[2]=
       {
         "Closing ",
         "Parameter file"
       };

    InitApplication();
    readyhandler(memhand,nullhand);
		if (argc<6)
      exitmessage();
    sprintf(GIDName,"%s.gid",argv[1]);
    Site=ratoi(argv[3]);
    Air=ratoi(argv[4]);
    if (!exist(GIDName))
      {
        cantfilemes[1]=GIDName;
        Message(4,cantfilemes);
        ExitApplication(1);
      }
    LogStart(argv[2]);
    t=TempMessage(2,openmes);
    Param=Open_GID(GIDName);
    Load_GID(Param,Site,argv[5]);  /* argv[5] is glyphname */
    copyaff(argv[1],myname);
    ClearTempMessage(t);
    t=TempMessage(2,genmes);
    prmtodet(Param,Site,myname);  /* argv[2] is runname */
    prmtosou(Param,Site,Air,myname,argv[5]);
    prmtomet(Param,Site,Air,myname);
    prmtojfd(Param,Site,Air,myname);
    prmtotop(Param,Site,Air,myname);
    prmtoid(myname);
    ClearTempMessage(t);
    t=TempMessage(2,closemes);
    Close_GID(Param);
    ClearTempMessage(t);
    LogStop();
    ExitApplication(0);
    return 0;
  }
