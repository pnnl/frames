#include "aqu.h"
#include "robust.h"
#include "siteflux.h"

void do1(FILE *wf,GIDFILE *pf,int *NUMWU)
   {
     int i,NumFlux,NumConc;
     NumFlux=ratoi(info(pf,"wzfnum"));
     for (i=0;i<NumFlux;i++)
       Usage.Add(info(pf,"wzfname",i+1),FLUX,i+1);
     NumConc=ratoi(info(pf,"wzcnum"));
     for (i=0;i<NumConc;i++)
       Usage.Add(info(pf,"wzcname",i+1),CONC,i+1);
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
      1,
      Usage.All[Rcp].Name);
  }

void do9(FILE *wf,GIDFILE *pf,int Rcp,int *ISCONF,int NDS)
  {
    int isconf;
    float wzfract;

    char *str;
    float tfinal;
    str=info(pf,"tfinal");
    if (str==NULL) tfinal=0.0;
    else tfinal=ratof(str);
    if (Site.Type==AQUIFER) isconf=5;
    else isconf=6;
    if (Usage.All[Rcp].Type==FLUX)
      wzfract=ratof(info(pf,"WZRFract",Usage.All[Rcp].Index));
    else
      wzfract=ratof(info(pf,"WZEFract",Usage.All[Rcp].Index));

// different method of determining configuration
    fprintf(wf,"%5i%5i%5i%5i%5i%5i%5i%5i%5i\n",
      2,
      2,
      2,
      2,
      0,
      0,
      1+NDS,
      isconf,
      0);
    fprintf(wf,"%#10.3G%#10.3G%#10.3G\n",
      tfinal,
      wzfract,
      30.0);
    *ISCONF=isconf;
  }

void do10(FILE *wf,int con)
  {
    fprintf(wf,"%-18s  %-18s  %#10.3G%5i%#10.3G%5i%#10.3G%#10.3G%#10.3G\n",
      Flux[con].fscname,
      Flux[con].fscasid,
      Flux[con].al/365.25, // days -> years
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
    fprintf(wf,"%-18s  %-18s  %#10.3G\n",
      Flux[con+sub+1].fscname,
      Flux[con+sub+1].fscasid,
      Flux[con+sub+1].al/365.25); // days -> years
  }

void do15(FILE *wf)
  {
    fprintf(wf,"%#10.5G%#10.3G\n",
      -99.0,
      -99.0);
  }

void do16(FILE *wf)
  {
    fprintf(wf,"%5i\n",0);
  }

void do17(FILE *wf,int start,int NumCon)
  {
    int i;
    for (i=0;i<NumCon;i++)
      {
        if (i%16==0 && i!=0) fprintf(wf,"\n");
        if (!i)  fprintf(wf,"%5i",Flux[start+i].wsnum);
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

int do26(FILE *wf,GIDFILE *pf,int ISCONF)
  {
    int media;
    char *str;
    int ntimes;
    media=ratoi(info(pf,"WZDIV"));
    str=info(pf,"ntimes");
    if (str==NULL) ntimes=40;
    else ntimes=ratoi(str);
    fprintf(wf,"%5i%5i%5i%5i%#10.3G\n",
      ISCONF,
      media,
      3,
      ntimes,
      0.0);
    return media;
  }

void do27(FILE *wf,int media,int Rcp)
  {
    int i;
    for (i=0;i<media-1;i++)
  	  fprintf(wf,"%5i",4);
    if (Usage.All[Rcp].Type==FLUX)
     	fprintf(wf,"%5i\n",4);
    else
    	fprintf(wf,"%5i\n",3);
  }

void do28(FILE *wf,GIDFILE *pf,int ISCONF)
   {
    float zbot,zthick,zwidth,zveloc;
    if (ISCONF==5)
      {
        fprintf(wf,"%#10.3G%#10.3G%#10.3G\n",
          Site.wswidth,
          Site.wstop,
          Site.wstop+Site.wslength);
      }
    else if (ISCONF==6)
      {
        zveloc=ratof(info(pf,"wzpveloc"));
        zwidth=Site.wswidth;
        zbot=Site.wsleachv*Site.wslength/zveloc;
        zthick=ratof(info(pf,"wzthick"));
        if (zbot>zthick)
          {
            zbot=zthick;
            zwidth=Site.wsleachv*Site.wslength*Site.wswidth/(zveloc*zthick);
          }
        fprintf(wf,"%#10.3G%#10.3G%#10.3G%#10.3G\n",
          Site.wslength,
          zwidth,
          0.0,
          zbot);
      }
    else
      {
        fprintf(wf,"%#10.3G%#10.3G\n",
          Site.wslength,
          Site.wswidth);
      }
  }

void do31(FILE *wf,GIDFILE *pf,int Rcp,int wzdiv,int layer)
  {
    float wzpveloc,wzthick,wzbulkd,wztotpor,wzeffpor,wzldisp,wztdisp,
		       wzvdisp,wzdist,wsdepth,wzydist,wzaqdepth;

    int num;
    wztotpor=ratof(info(pf,"WZTOTPOR"));
    wzeffpor=ratof(info(pf,"WZEFFPOR"));
    wzbulkd=ratof(info(pf,"WZBULK"));
    wzthick=ratof(info(pf,"WZTHICK"));
    wzpveloc=ratof(info(pf,"WZPVELOC"));
    wsdepth=Site.wstop;
    num=Usage.All[Rcp].Index;
    if (Usage.All[Rcp].Type==FLUX)
      {
        wzvdisp=ratof(info(pf,"WZFVDISP",num));
        wztdisp=ratof(info(pf,"WZFTDISP",num));
        wzldisp=ratof(info(pf,"WZFLDISP",num));
        wzdist=ratof(info(pf,"WZFDIST",num));
        wzydist=ratof(info(pf,"WZFYDIST",num));
        wzaqdepth=ratof(info(pf,"WZFAQDEPTH",num));
      }
    else
      {
        wzvdisp=ratof(info(pf,"WZCVDISP",num));
        wztdisp=ratof(info(pf,"WZCTDISP",num));
        wzldisp=ratof(info(pf,"WZCLDISP",num));
        wzdist=ratof(info(pf,"WZCDIST",num));
        wzydist=ratof(info(pf,"WZCYDIST",num));
        wzaqdepth=ratof(info(pf,"WZCAQDEPTH",num));
      }
    fprintf(wf,"%#10.3G%#10.3G%#10.3G%#10.3G%#10.3G%#10.3G%#10.3G%#10.3G\n",
      wzpveloc,
      wzthick,
      wzbulkd,
      wztotpor,
      wzeffpor,
      wzldisp,
      wztdisp,
      wzvdisp);
    if (wzdiv-1 != layer)
      {
        wzydist=0.0;
        wzaqdepth=0.0;
      }
    fprintf(wf,"%#10.3G%#10.3G%#10.3G%#10.3G%#10.3G\n",
      wzdist/(float)wzdiv,
      wsdepth,
      0.0,
      wzydist,
      wzaqdepth);
  }

void do32(FILE *wf,int start,int NumCon)
  {
    int i;
    for (i=0;i<NumCon;i++)
      {
        if (i%8==0 && i!=0) fprintf(wf,"\n");
        fprintf(wf,"%#10.3G",Flux[start+i].kd);
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
        Flux[n].SetInfo(pf,i+1,0,snum,nds,AQUIFER);
        Flux[n].Read(wff,&Site,sglyph,mglyph,AQUIFER,VADOSE,SURFACEWATER);
        n++;
        for (j=0;j<nds;j++)
          {
            Flux[n].SetInfo(pf,i+1,j+1,snum,nds-j-1,AQUIFER);
            Flux[n].Read(wff,&Site,sglyph,mglyph,AQUIFER,VADOSE,SURFACEWATER);
            n++;
          }
      }
    return n;
  }

int dowin(fcsv *wff,FILE *wf,GIDFILE *pf,int Site,int Aqu,char *mglyph)
  {
   int NumCon,i,j,done,nds,isconf,media,NumWU,Con,TotalDaugh;

   NumCon=GetCons(wff,pf,Site,info(pf,"AquSrcName",Site,Aqu,1),mglyph);
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
           do9(wf,pf,done,&isconf,nds);
           for (i=0;i<nds+1;i++)
             {
               do10(wf,Con+i);
               for (j=0;j<nds-i;j++)
                 do11(wf,Con+i,j);
             }
           do15(wf);
           do16(wf);
           do17(wf,Con,nds+1);
           for (i=0;i<nds+1;i++)
             do18(wf,Con+i,i);
           media=do26(wf,pf,isconf);
           do27(wf,media,done);
           do28(wf,pf,isconf); // use first length width and depth
           for (i=0;i<media;i++)
             {
               do31(wf,pf,done,media,i);
               do32(wf,Con,nds+1);
             }
           Con+=nds;
         }
     }
    return 1;
  }

