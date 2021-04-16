#pragma hdrstop
#include <condefs.h>
USEUNIT("..\Common Files\robust.cpp");
USEUNIT("..\Common Files\fcsv.cpp");
USEUNIT("..\Common Files\gid.cpp");
USEUNIT("reduce.cpp");
USEUNIT("siteflux.cpp");
USEUNIT("riv2.cpp");
USERES("PreRiver.res");
//---------------------------------------------------------------------------
#include "riv.h"

void dofacil(char *path)
  {
    char s[128],*p;
    FILE *f;
    // path has addition name that needs to be taken off
    rstrcpy(s,path);
    p=&(s[strlen(path)-1]);
    while (p>s && *p!='\\')
      p--;
    if (*p=='\\') *(p+1)='\0';
    else s[0]='\0';
    strcat(s,"facil.id");
    f=fopen(s,"wt");
    fprintf(f," 5 Mepas Shell 1992\n");
    fprintf(f," 1  1,\n");
    fprintf(f," 1 %-8s\n","MEPAS_RV");
    fclose(f);
  }

char gfile[128],ffile[128],wfile[128];

#pragma argsused
int main(int argc,char **argv)
  {
    GIDFILE *gidf;
    FILE *winf;
    fcsv *wff;
    int site,aqu,retval=0;
    if (argc<6)
      {
        printf("Usage: preaqu <FUIName> <RunName> <Site#>\n");
        printf("              <Aquifer#> <GlyphName>\n");
        retval=1;
      }
    else
      {
         sprintf(gfile,"%s.gid",argv[1]);
         sprintf(ffile,"%s.wff",argv[1]);
         sprintf(wfile,"%s.win","MEPAS_RV");
         site=atoi(argv[3]);
         aqu=atoi(argv[4]);
         gidf=Open_GID(gfile);
         if (Load_GID(gidf,site,argv[5]))
           {
             wff=new fcsv;
             dofacil("MEPAS_RV");
             winf=fopen(wfile,"wt");
             wff->open(ffile,READ);
             if (!dowin(wff,winf,gidf,site,aqu,argv[5])) retval=1;
             Close_GID(gidf);
             fclose(winf);
             wff->close();
             delete wff;
           }
         else
           {
             retval=1;
             printf("Error opening GID file\n");
           }
      }
    return retval;
  }
