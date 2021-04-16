#pragma hdrstop
#include <conio.h>
#include "..\Common Files\frames.h"
#include "..\Common Files\gid.h"
#include "..\Common Files\robust.h"
#include "..\Common Files\error.h"
#include <except.h>
#include <condefs.h>

//---------------------------------------------------------------------------
USEUNIT("..\Common Files\Error.cpp");
USEUNIT("..\Common Files\gid.cpp");
USEUNIT("..\Common Files\Fcsv.cpp");
USEUNIT("..\Common Files\robust.cpp");
USERES("eco.res");
//---------------------------------------------------------------------------
#pragma argsused

int siteidx,modidx,numlife,numcon;
struct ffblk ff;
GIDFILE *G=NULL;

element *LifeForm;
element *LifeName;
element *ChemId;/// = Get_Element(G,"FSCASID");
element *ChemName;/// = Get_Element(G,"FSCNAME");


char *AddExten(char *f,char *e)
{
  static char fname[MAXSTRING];
  sprintf(fname,"%s.%s",f,e);
  return fname;
}


int main(int argc,char **argv)
{
  char des[MAXSTRING];

  if (argc<6)
  {
    printf("USAGE: <FUIFile> <RunFile> <SiteIndex> <ModelIndex> <ModelID>\n");
  }
  else
  {
    siteidx = ratoi(argv[3]);
    modidx = ratoi(argv[4]);

    ErrOpen(argv[2]);
    WrnOpen(argv[2]);
    G=Open_GID(AddExtension(argv[1],"gid"));
    if (G==NULL)
      Error("GID file not found");
    Load_GID(G,siteidx,argv[5]);

    int cnt1 = ratoi(info(G,"nummod",siteidx));
    element *modid = Get_Element(G,"ModID");
    element *modnumsrc = Get_Element(G,"modsrcnum");
    element *modsrcid = Get_Element(G,"modsrcid");
    element *modsrcqual = Get_Element(G,"modsrcqual");
    for (int i=1; i<=cnt1; i++)
    {
      if (!strcmpi(argv[5],getvalu(modid,siteidx,i)))
      {
        int cnt2 = ratoi(getvalu(modnumsrc,siteidx,i));
        for (int j=1; j<=cnt2; j++)
        {
          strcpy(des,getvalu(modsrcqual,siteidx,i,j));
          if (!rstrcmpi("Eco Aquatic Benchmarks",des) ||
              !rstrcmpi("Aquatic Organism",des) ||
              !rstrcmpi("Aquatic Benchmarks",des) ||
              !rstrcmpi("Terrestrial Organism",des) ||
              !rstrcmpi("Chemical Terrestrial TRVs",des) ||
              !rstrcmpi("Chemical SSLs",des))
            Load_GIDSection(G,getvalu(modsrcid,siteidx,i,j));
        }
        break;
      }
    }

    //if (aquatic)
    LifeForm = Get_Element(G,"LifeFormSci");
    LifeName = Get_Element(G,"LifeFormName");
    numlife = ratoi(info(G,"numlife"));

    if (LifeForm == NULL)    // then terrestrial
    {
      LifeForm = Get_Element(G,"ScientificName");
      LifeName = Get_Element(G,"CommonName");
      if (numlife == 0) numlife = ratoi(info(G,"numScientificName"));
    }

    // All gid information is loaded for SSL, TRV organism list and properties
    // as well as the modules input from placed into the gid

    // use frames DLL calls to read SCF,WCF,ATO files frames.h for declarations

    //***
    // insert code here
    //***



    WrnClose();
    ErrClose();
    Close_GID(G);
  }
  findclose(&ff);
  return 0;
}

