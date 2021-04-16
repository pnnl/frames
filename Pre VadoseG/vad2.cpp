#include "vad.h"
#include "dates.h"
#include "siteflux.h"

void do1(FILE *wf,int *NumWU)
   {
      fprintf(wf," %4i %4i %4i\n",
        1,
        0,
        1);
      *NumWU=1;
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

void do7(FILE *wf,char *mglyph)
  {
    fprintf(wf,"%5i%5i    %-24s\n",
      1,
      1,
      mglyph);
  }

void do9(FILE *wf,GIDFILE *pf,int NumCon)
  {
    char *str;
    float tfinal;
    str=info(pf,"tfinal");
    if (str==NULL) tfinal=0.0;
    else tfinal=ratof(str);
    fprintf(wf,"%5i%5i%5i%5i%5i%5i%5i%5i%5i\n",
      1,
      2,
      2,
      2,
      0,
      0,
      1+NumCon,
      4,
      0);
    fprintf(wf,"%#10.3G%#10.3G%#10.3G\n",
      tfinal,
      0.0,
      30.0);
  }

void do10(FILE *wf,int con)
  {
    fprintf(wf,"%-8s  %-8s  %#10.3G%5i%#10.3G%5i%#10.3G%#10.3G%#10.3G\n",
      Flux[con].fscname,
      Flux[con].fscasid,
      Flux[con].al/365.25, //days -> years
      Flux[con].nds+1,
      Flux[con].conc,
      3,
      Flux[con].kd,
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
    fprintf(wf,"%-8s  %-8s  %#10.3G\n",
      Flux[con+sub+1].fscname,
      Flux[con+sub+1].fscasid,
      Flux[con+sub+1].al/365.25); // days-> years
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
     	  fprintf(wf,"%5i",Flux[Start+i].wsnum);
      }
    fprintf(wf,"\n");
  }

void do18(FILE *wf,int con)
  {
    int j;
    for (j=0;j<Flux[con].wsnum;j++)
      {
        if (j!=0 && j%4==0) fprintf(wf,"\n");
        fprintf(wf,"%#10.5G%#10.5G",
          Flux[con].wsflux[j],
          Flux[con].wstime[j]);
      }
    fprintf(wf,"\n");
  }

void do19(FILE *wf)
  {
    fprintf(wf,"%#10.3G\n",Site.wsleachv);
  }

void do26(FILE *wf,GIDFILE *pf)
  {
    char *str;
    int ntimes;
    str=info(pf,"ntimes");
    if (str==NULL) ntimes=40;
    else ntimes=ratoi(str);
    fprintf(wf,"%5i%5i%5i%5i%#10.3G\n",
      4,
      1,
      3,
      ntimes,
      0.0);
  }

void do27(FILE *wf)
  {
  
	  fprintf(wf,"%5i\n",1);
  }

void do28(FILE *wf)
  {
    fprintf(wf,"%#10.3G%#10.3G\n",
      Site.wslength,
      Site.wswidth);
  }

void do30(FILE *wf,GIDFILE *pf)
  {
    float wpthick,wpbulkd,wptotpor,wpfieldc,wpldisp,wpconduc,a2;

    a2=ratof(info(pf,"WPSOILCOEF"));
    wpconduc=ratof(info(pf,"WPCONDUC"));
    wpldisp=ratof(info(pf,"WPLDISP"));
    wpfieldc=ratof(info(pf,"WPFIELDC"));
    wptotpor=ratof(info(pf,"WPTOTPOR"));
    wpbulkd=ratof(info(pf,"WPBULK"));
    wpthick=ratof(info(pf,"WPTHICK"));
    fprintf(wf,"%#10.3G%#10.3G%#10.3G%#10.3G%#10.3G%#10.3G%#10.3G\n",
      wpthick,
      wpbulkd,
      wptotpor,
      wpfieldc,
      wpldisp,
      wpconduc,
      a2);
  }

void do32(FILE *wf,int Start,int NumCon)
  {
    int i;
    for (i=0;i<NumCon;i++)
      {
        if (i%8==0 && i!=0) fprintf(wf,"\n");
        fprintf(wf,"%#10.3G",Flux[Start+i].kd);
      }
    fprintf(wf,"\n");
  }

int GetCons(fcsv  *wff,GIDFILE *pf,int snum,char *sglyph,char *mglyph)
  {
    int NumCon,i,n,nds,j;
    NumCon=ratoi(info(pf,"NUMCON",snum));
    n=0;
    for (i=0;i<NumCon;i++)
      {
        nds=ratoi(info(pf,"NDS",snum,i+1));
        Flux[n].SetInfo(pf,i+1,0,snum,nds,VADOSE);
        Flux[n].Read(wff,&Site,sglyph,mglyph,VADOSE,SURFACEWATER,VADOSE);
        n++;
        for (j=0;j<nds;j++)
          {
            Flux[n].SetInfo(pf,i+1,j+1,snum,nds-j-1,VADOSE);
            Flux[n].Read(wff,&Site,sglyph,mglyph,VADOSE,SURFACEWATER,VADOSE);
            n++;
          }
      }
    return n;
  }

int dowin(fcsv *wff,FILE *wf,GIDFILE *pf,int Site,int Aqu,char *mglyph)
  {
   int NumCon,i,j,done,nds,NumWU,Con,TotalDaugh;

   NumCon=GetCons(wff,pf,Site,info(pf,"VadSrcName",Site,Aqu,1),mglyph);
   if (!Ok(NumCon)) return 0;
   TotalDaugh=0;
   for (Con=0;Con<NumCon;Con++)
     {
       TotalDaugh+=Flux[Con].nds;
       Con+=Flux[Con].nds;
     }
	 do1(wf,&NumWU);
   for (done=0;done<NumWU;done++)
     {
	     do2(wf,NumCon-TotalDaugh);
       for (Con=0;Con<NumCon;Con++)
         {
           do3(wf);
           do4(wf);
           do5(wf);
           do6(wf);
           do7(wf,mglyph);
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
             do18(wf,Con+i);
           do19(wf);
           do26(wf,pf);
           do27(wf);
           do28(wf);
           do30(wf,pf);
           do32(wf,Con,nds+1);
           Con+=nds;
         }
     }
    return 1;
  }


