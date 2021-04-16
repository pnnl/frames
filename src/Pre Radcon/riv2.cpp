#include "riv.h"
#include "robust.h"
#include "siteflux.h"

void do1(FILE *wf,GIDFILE *pf,int *NUMWU)
{
  int i,NumFlux,NumConc;
  NumFlux=ratoi(info(pf,"wwnumflux"));
  NumConc=ratoi(info(pf,"wwnumconc"));
  for (i=0;i<NumFlux+NumConc;i++)
    if (ratoi(info(pf,"wwkind",i+1))==1)
      Usage.Add(info(pf,"wwname",i+1),FLUX,i+1);
  for (i=0;i<NumFlux+NumConc;i++)
    if (ratoi(info(pf,"wwkind",i+1))==0)
      Usage.Add(info(pf,"wwname",i+1),CONC,i+1);
  fprintf(wf," %4i %4i %4i\n",
    Usage.NumEndPoints,
    0,
    NumFlux);
  *NUMWU=Usage.NumEndPoints;
}

void do2(FILE *wf,int NumCon)
   {
    fprintf(wf,"%5i\n",NumCon);
   }

void do3(FILE *wf)
  {
    fprintf(wf,"%-32s\n","Facility Name");
  }

void do4(FILE *wf)
  {
    fprintf(wf,"%-64s\n","Ranking Unit Name");
  }

void do5(FILE *wf)
  {
    fprintf(wf,"%-64s\n","Transport Scenario");
  }

void do6(FILE *wf)
  {
    fprintf(wf,"%-32s\n","Exposure Scenario");
  }

void do7(FILE *wf,int Rcp)
  {
    fprintf(wf,"%5i%5i     %-24s\n",
      Rcp+1,
      3,
      Usage.All[Rcp].Name);
  }

void do9(FILE *wf,GIDFILE *pf,int NumCon)
  {
    char *str;
    float tfinal;
    str=info(pf,"tfinal");
    if (str==NULL) tfinal=0.0;
    else tfinal=ratof(str);
    fprintf(wf,"%5i%5i%5i%5i%5i%5i%5i%5i%5i\n",
      3,
      2,
      2,
      2,
      1,
      0,
      1+NumCon,
      0,
      0);
    fprintf(wf,"%#10.3G%#10.3G%#10.3G\n",
      tfinal,
      0.0,
      30.0);
  }

void do10(FILE *wf,int con)
  {
    fprintf(wf,"%-18s  %-18s  %#10.3G%5i%#10.3G%5i%#10.3G%#10.3G%#10.3G\n",
      Flux[con].fscname,
      Flux[con].fscasid,
      Flux[con].al/365.25, // days -> years
      Flux[con].nds+1,
      -99.0,
      -99,
      -99.0,
      Flux[con].sollim,
      RoundUp(Flux[con].wsinvent));
    fprintf(wf,"          %#10.3G%#10.3G%#10.3G%5i\n",
      Flux[con].wstlife,
      0.0,
      0.0,
      Flux[con].Rad);
  }

void do11(FILE *wf,int con,int sub)
  {
    fprintf(wf,"%-18s  %-18s  %#10.3G\n",
      Flux[con+sub+1].fscname,
      Flux[con+sub+1].fscasid,
      Flux[con+sub+1].al/365.25); // days -> years
  }

void do16(FILE *wf)
  {
    fprintf(wf,"%5i\n",0);
  }

void do17(FILE *wf,int Start,int NumCon)
  {
    int i;
    for (i=0;i<NumCon;i++)
      {
        if (i%16==0 && i!=0) fprintf(wf,"\n");
        if (!i)  fprintf(wf,"%5i",Flux[Start+i].wsnum);
        else    fprintf(wf,"%5i",2);
      }
    fprintf(wf,"\n");
  }

void do18(FILE *wf,int con, int i)
  {
    if (!i)
      for (int j=0;j<Flux[con].wsnum;j++)
      {
        if (j!=0 && j%4==0) fprintf(wf,"\n");
        fprintf(wf,"%#10.5G%#10.5G", Flux[con].wsflux[j], Flux[con].wstime[j]);
      }
    else
    {
      fprintf(wf,"%#10.5G%#10.5G", 0.0, Flux[con-i].wstime[0]);
      fprintf(wf,"%#10.5G%#10.5G", 0.0, Flux[con-i].wstime[Flux[con-i].wsnum-1]);
    }
    fprintf(wf,"\n");
  }

void do25(FILE *wf)
  {
    fprintf(wf,"%#10.3G\n",Site.wswidth);
  }

void do26(FILE *wf,GIDFILE *pf)
  {
    char *str;
    int ntimes;
    str=info(pf,"ntimes");
    if (str==NULL) ntimes=40;
    else ntimes=ratoi(str);
    fprintf(wf,"%5i%5i%5i%5i%#10.3G\n",
      0,
      1,
      3,
      ntimes,
      0.0);
  }

void do27(FILE *wf,int Rcp)
  {
    int media;

    if (Usage.All[Rcp].Type==FLUX) media=6;
    else media=5;
	  fprintf(wf,"%5i\n",media);
  }

void do33(FILE *wf,GIDFILE *pf,int Rcp)
  {
    float wwveloc,wwdepth,wwwidth,wwdist,wwdischg;
    float wwx,wwy,wwz;

    int num;
    num=Usage.All[Rcp].Index;
    wwveloc=ratof(info(pf,"wwveloc"));
    wwdepth=ratof(info(pf,"wwdepth"));
    wwwidth=ratof(info(pf,"wwwidth"));
//    wwdischg=ratof(info(pf,"wwdischg",num));
    wwdischg=wwveloc*wwdepth*wwwidth;
    wwdist=ratof(info(pf,"wwdist",num));
    fprintf(wf,"%5i%#10.3G%#10.3G%#10.3G%#10.3G%#10.3G\n",
      1,
      wwveloc,
      wwdepth,
      wwwidth,
      wwdist,
      wwdischg);

    wwx=ratof(info(pf,"wwx",num));
    wwy=ratof(info(pf,"wwy",num));
    wwz=ratof(info(pf,"wwz",num));
    fprintf(wf,"%#15.3f%#15.3f%#15.3f\n",wwy,wwx,wwz);
  }


int GetCons(fcsv  *wff,GIDFILE *pf,int snum,char *sglyph,char *mglyph)
  {
    int NumCon,i,n,nds,j;
    NumCon=ratoi(info(pf,"NUMCON",snum));
    n=0;
    for (i=0;i<NumCon;i++)
      {
        nds=ratoi(info(pf,"NDS",snum,i+1));
        Flux[n].SetInfo(pf,i+1,0,snum,nds,SURFACEWATER);
        Flux[n].Read(wff,&Site,sglyph,mglyph,SURFACEWATER,AQUIFER,AQUIFER);
        n++;
        for (j=0;j<nds;j++)
          {
            Flux[n].SetInfo(pf,i+1,j+1,snum,nds-j-1,SURFACEWATER);
            Flux[n].Read(wff,&Site,sglyph,mglyph,SURFACEWATER,AQUIFER,AQUIFER);
            n++;
          }
      }
    return n;
  }

int dowin(fcsv *wff,FILE *wf,GIDFILE *pf,int Site,int Aqu,char *mglyph)
  {
   int NumCon,i,j,done,nds,NumWU,TotalDaugh,Con;

   NumCon=GetCons(wff,pf,Site,info(pf,"RivSrcName",Site,Aqu,1),mglyph);
   if (!Ok(NumCon)) return 0;
   TotalDaugh=0;
   for (Con=0;Con<NumCon;Con++)
     {
       TotalDaugh+=Flux[Con].nds;
       Con+=Flux[Con].nds;
     }
	 do1(wf,pf,&NumWU);
   for (done=0;done<NumWU;done++)
     {
       do2(wf,NumCon-TotalDaugh);
       for (Con=0;Con<NumCon;Con++)
         {
           do3(wf);
           do4(wf);
           do5(wf);
           do6(wf);
           do7(wf,done);
           nds=Flux[Con].nds;
           do9(wf,pf,nds);
           for (i=0;i<nds+1;i++)
             {
               do10(wf,Con+i);
               for (j=0;j<nds-i;j++)
                 do11(wf,Con+i,j);
             }
           do16(wf);
           do17(wf,Con,nds+1);
           for (i=0;i<nds+1;i++)
             do18(wf,Con+i,i);
           do25(wf);
           do26(wf,pf);
           do27(wf,done);
           do33(wf,pf,done);
           Con+=nds;
         }
     }
    return 1;
  }

