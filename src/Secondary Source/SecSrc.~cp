
#pragma hdrstop
#include <condefs.h>
#include <values.h>
#include <math.h>

#include "gid.h"
#include "Series.h"
#include "error.h"
#include "conClass.h"
#include "frames.h"
#include "atoSSrc.h"
//#include "wffClass.h"


//---------------------------------------------------------------------------
USEUNIT("..\Common Files\robust.cpp");
USEUNIT("..\Common Files\csv.cpp");
USEUNIT("..\Common Files\fcsv.cpp");
USEUNIT("..\Common Files\gid.cpp");
USEUNIT("..\Common Files\series.cpp");
USEUNIT("..\Common Files\conClass.cpp");
USEUNIT("atoSSrc.cpp");
USERES("SecSrc.res");
USELIB("..\Frames DLL\frames.lib");
//---------------------------------------------------------------------------
#pragma argsused


int siteidx,modidx,numcon;
int numsrc,numlife,numlocs,numecos;
int bbfcheck;
char Title[1024];
struct ffblk ff;
GIDFILE *G=NULL;
fcsv *fout=NULL;
ocsv *fdbg=NULL;
CON **cont=NULL;

void wffSecSrc(char *fname, char *wffid);
void atoSecSrc(char *fname, char *srcid, char *atoid, char *dbgfile);
void AddSeries(char *casid, char *pcas, Series *TS);

int main(int argc, char **argv)
{
  char filename[MAXPATH];
  char drive[MAXDRIVE];
  char dir[MAXDIR];
  char file[MAXFILE];
  char ext[MAXEXT];
  char dbgfile[MAXPATH];

  if (argc<6)
  {
    printf("USAGE: <FUIFile> <RunFile> <SiteIndex> <ModelIndex> <ModelID>\n");
    return 0;
  }

  siteidx = atoi(argv[3]);
  modidx = atoi(argv[4]);
  rstrcpy(ERRName,argv[2],".err");

  G=Open_GID(AddExtension(argv[1],"gid"));
  if (G==NULL)
  {
    writeError("GID file not found");
    exit(0);
  }
  Load_GID(G,siteidx,argv[5]); // loads FUI, CSM and module

  fnsplit(argv[1],drive,dir,file,ext);
  sprintf(dbgfile,"%s\%s\secsrc.dbg",drive,dir);
  rstrcpy(dbgfile,"secsrc.dbg");

  Series *TS = new Series(2);
  TS->Replace(0,0.0,0.0);
  TS->Replace(1,7000.0,0.0);

  element *fscname = Get_Element(G,"FSCNAME");
  element *fscasid = Get_Element(G,"FSCASID");
  element *nds = Get_Element(G,"NDS");
  element *ktype = Get_Element(G,"CLKTYPE");

  numcon = ratoi(info(G,"numcon",siteidx));
  cont = new CON*[numcon];
  for (int i=1; i<=numcon; i++)
  {
    cont[i-1] = new CON();
    cont[i-1]->Init(siteidx, i, 0, fscname, fscasid, nds, ktype);

    cont[i-1]->TS->Copy(TS);
    for (int j=0; j<cont[i-1]->numProgeny; j++)
      cont[i-1]->prog[j]->TS->Copy(TS);
  }

  int nummod = ratoi(info(G,"nummod",siteidx));

  element *modid = Get_Element(G,"ModID");
  element *modnumsrc = Get_Element(G,"modsrcnum");
  element *modsrcid = Get_Element(G,"modsrcid");
  element *modsrctype = Get_Element(G,"modsrctype");

  for (int nmod=1; nmod<=nummod; nmod++)
  {
    if (0==rstrcmpi(argv[5],getvalu(modid,siteidx,nmod)))
    {
      int numsrc = atoi(getvalu(modnumsrc,siteidx,nmod));
      for (int nsrc=1; nsrc<=numsrc; nsrc++)
      {
        char *srctype = getvalu(modsrctype,siteidx,nmod,nsrc);
        if (0==rstrncmpi("wff",srctype,3))
          wffSecSrc(argv[1],
                  getvalu(modsrcid,siteidx,nmod,nsrc));
        if (0==strncmpi("ato",srctype,3))
        {
          atoSSrc *ato = new atoSSrc(G, siteidx, AddSeries);
          ato->getSeries(argv[1],argv[5],
                       getvalu(modsrcid,siteidx,nmod,nsrc));
          delete ato;
        }
      }
      break;
    }
  }
  Close_GID(G);

  rstrcpy(filename,AddExtension(argv[2],"dat"));

  try
  {      fout = new fcsv;    }
  catch(...)
  {
     writeError("Can't allocate memory for output file");
     exit(0);
  }
  if (!fout->open(filename,WRITE))
  {
    writeError("Can't open output file!");
    exit(0);
  }
  fout->write(0); // header - to be supplied later
  fout->writeln();
  fout->write(argv[5]);
  fout->write(numcon);
  fout->writeln();
  for (int ncon=1;ncon<=numcon;ncon++)
    cont[ncon-1]->Write(fout);
  fout->close();

  return 0;
}

void wffSecSrc(char *fname, char *wffid)
{
  Series *TS = new Series();
  char mname[SMALLSTRING];
  char mtype[SMALLSTRING];
  double wffwidth, length, distance, recharge;

  double width = atof(info(G,"stwidth",siteidx)) / 100.0;  // cm -> m

  wffOpen(fname,wffid);
  int wffNumMed = wffGetNumSets();
  for (int wffNMed=1; wffNMed<=wffNumMed; wffNMed++)
  {
    wffGetSetInfo(wffNMed,mname,mtype);
    if (0==strcmpi(mtype,"Surface Water"))
    {
      wffGetDimensions(wffNMed, &wffwidth, &length, &distance, &recharge);
      float ratio = 1.0;
      if (width < wffwidth) ratio = width / wffwidth;
      int wffNumCon = wffGetNumContam(wffNMed);
      for (int wffNCon=1; wffNCon<=wffNumCon; wffNCon++)
      {
        char cname[SMALLSTRING], pcas[SMALLSTRING], casid[SMALLSTRING], units[SMALLSTRING], timeunits[SMALLSTRING];
        wffGetContamName(wffNMed,wffNCon,0,cname,pcas);
        int wffNumProg = wffGetNumProgeny(wffNMed, wffNCon);
        for (int wffNProg=0; wffNProg<=wffNumProg; wffNProg++)
        {
          wffGetContamName(wffNMed,wffNCon,wffNProg,cname,casid);
          int count = wffGetSeriesProperties(wffNMed,wffNCon,wffNProg,units,timeunits);
          TS->Init(count);
          strcpy(TS->xUnits,timeunits);
          strcpy(TS->yUnits,units);
          wffGetSeriesValuesSum(wffNMed,wffNCon,wffNProg,count,TS->xValues,TS->yValues);
          for (int n=0;n<count;n++)
          {
            if (0==strncmpi(units,"pCi",3))
              TS->yValues[n] *= 1.0e-12; // pCi --> Ci
            TS->yValues[n] *= ratio;
          }
          AddSeries(casid, pcas, TS);
        }
      }
    }
  }
  wffClose();
  delete TS;
}



void AddSeries(char *casid, char *pcas, Series *TS)
{
  CON *con=NULL;
  for (int ncon=0; ncon<numcon; ncon++)
  {
    if (0==strcmpi(cont[ncon]->cas,pcas))
    {
      if (0==strcmpi(cont[ncon]->cas,casid))
        con = cont[ncon];
      else
        for (int nprog=0; nprog<cont[ncon]->numProgeny; nprog++)
          if (0==strcmpi(cont[ncon]->prog[nprog]->cas,casid))
          {
            con = cont[ncon]->prog[nprog];
            break;
          }
    }
    if (con) break;
  }
  if (con)
  {
    if (con->type==1)
      con->AddSeries("Ci/yr","yr",TS);
    else
      con->AddSeries("g/yr","yr",TS);
  }
}

























