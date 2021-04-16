  /*ษออออออออออออออออออออออออออออออออออออออออออัอออออออออออออออออออออออออออป
    บ                    DATE         TIME     ณ Written by Mitch Pelton   บ
    บ Written      : August,1996               ณ For Pacific Northwest     บ
    บ Received     :                           ณ     National Laboratories บ
    บ Modified     :                           ณ     Battelle Corporation  บ
    ศออออออออออออออออออออออออออออออออออออออออออฯอออออออออออออออออออออออออออผ*/

#include "dfl.h"

void writedflrec(FILE *f,dflrec d)
{
  if (d.iwdat == 38 || d.iwdat == 42)
    fprintf(f,"%-3d,%d,%d,%d,%d,%d,%s\n",d.iwdat,
          d.val[0],d.val[1],d.val[2],
          d.val[3],d.val[4],d.text);
  else
    fprintf(f,"%-3d,%.3f,%.3f,%.3f,%.3f,%.3f,%s\n",d.iwdat,
          d.val[0],d.val[1],d.val[2],
          d.val[3],d.val[4],d.text);
}

void iwdat1d(GIDFILE *pf,FILE *f, int iwdt, char *n1, int idx, char *comment)
{
  element *list;
  dflrec d;
  paramrec p ={false,0,0,0,0,0,0,0,"","","","",""};

  rstrcpy(p.name,n1);

  if ((list = Get_Element(pf,p.name)) != NULL)
  {
    d.iwdat = iwdt;
    p.cnt1 = idx;   d.val[0] = ratof(Get_Element_Value(list,&p));
    p.cnt1++;       d.val[1] = ratof(Get_Element_Value(list,&p));
    p.cnt1++;       d.val[2] = ratof(Get_Element_Value(list,&p));
    p.cnt1++;       d.val[3] = ratof(Get_Element_Value(list,&p));
    p.cnt1++;       d.val[4] = ratof(Get_Element_Value(list,&p));
    rstrcpy(d.text,comment);
    writedflrec(f,d);
  }
}

void iwdat2d(GIDFILE *pf,FILE *f, int iwdt, char *n1,int idx1, int idx2, char *comment)
{
  element *list;
  dflrec d;
  paramrec p ={false,0,0,0,0,0,0,0,"","","","",""};

  rstrcpy(p.name,n1);
  p.cnt1 = idx1;
  if ((list = Get_Element(pf,p.name)) != NULL)
  {
    d.iwdat = iwdt;
    p.cnt2 = idx2;   d.val[0] = ratof(Get_Element_Value(list,&p));
    p.cnt2++;        d.val[1] = ratof(Get_Element_Value(list,&p));
    p.cnt2++;        d.val[2] = ratof(Get_Element_Value(list,&p));
    p.cnt2++;        d.val[3] = ratof(Get_Element_Value(list,&p));
    p.cnt2++;        d.val[4] = ratof(Get_Element_Value(list,&p));
    rstrcpy(d.text,comment);
    writedflrec(f,d);
  }
}

void iwdat19(GIDFILE *pf,FILE *f)
{
  dflrec d;

  d.iwdat = 19;
  d.val[0] = ratof(info(pf,"ffc",1,0,0,0,0,0));
  d.val[1] = ratof(info(pf,"ffc",2,0,0,0,0,0));
  d.val[2] = ratof(info(pf,"fwc",1,0,0,0,0,0));
  d.val[3] = ratof(info(pf,"fwc",2,0,0,0,0,0));
  d.val[4] = 0;
  rstrcpy(d.text,"Fractions of feed & water contamination");
  if (d.val[0]+d.val[1]+d.val[2]+d.val[3] > 0)
    writedflrec(f,d);
}

void iwdat66(GIDFILE *pf,FILE *f)
{
  dflrec d;

  d.iwdat = 66;
  d.val[0] = ratof(info(pf,"askin",1,0,0,0,0,0));
  d.val[1] = ratof(info(pf,"askin",2,0,0,0,0,0));
  d.val[2] = ratof(info(pf,"askin",3,0,0,0,0,0));
  d.val[3] = ratof(info(pf,"adfac",0,0,0,0,0,0));
  d.val[4] = ratof(info(pf,"evsoil",0,0,0,0,0,0));
  rstrcpy(d.text,"Skin exposure, soil adherance & freq");
  if (d.val[0]+d.val[1]+d.val[2]+d.val[3]+d.val[4] > 0)
    writedflrec(f,d);
}

void iwdat69(GIDFILE *pf,FILE *f)
{
  dflrec d;

  d.iwdat = 69;
  d.val[0] = ratof(info(pf,"teshor",0,0,0,0,0,0));
  d.val[1] = ratof(info(pf,"evshor",0,0,0,0,0,0));
  d.val[2] = ratof(info(pf,"used",0,0,0,0,0,0));
  d.val[3] = ratof(info(pf,"askin",4,0,0,0,0,0));
  d.val[4] = 0;
  rstrcpy(d.text,"Shore time, freq, ingestion & skin area");
  if (d.val[0]+d.val[1]+d.val[2]+d.val[3] > 0)
    writedflrec(f,d);
}

void iwdatloc(GIDFILE *pf,FILE *f,int iwdat,int idx,char *comment,char *n1)
{
  dflrec d = {0,{0,0,0,0,0},""};
  d.iwdat = iwdat;
  d.val[0] = ratof(info(pf,n1,idx,0,0,0,0,0));
  rstrcpy(d.text,comment);
  if (d.val[0] > 0) writedflrec(f,d);
}

void iwdat(GIDFILE *pf, FILE *f, int iwdat, char *comment,
           char *n1=NULL,char *n2=NULL,char *n3=NULL,
           char *n4=NULL,char *n5=NULL)
{
  dflrec d = {0,{0,0,0,0,0},""};
  d.iwdat = iwdat;
  if (n1) d.val[0] = ratof(info(pf,n1,0,0,0,0,0,0));
  if (n2) d.val[1] = ratof(info(pf,n2,0,0,0,0,0,0));
  if (n3) d.val[2] = ratof(info(pf,n3,0,0,0,0,0,0));
  if (n4) d.val[3] = ratof(info(pf,n4,0,0,0,0,0,0));
  if (n5) d.val[4] = ratof(info(pf,n5,0,0,0,0,0,0));;
  rstrcpy(d.text,comment);
  if (d.val[0]+d.val[1]+d.val[2]+d.val[3]+d.val[4] > 0)
    writedflrec(f,d);
}

void Make_DFL(GIDFILE *pf,char *runname)
{
  FILE *out;
  char fname[128];

  rstrcpy(fname,runname);
  strcat(fname,".dfl");
  if ((out = fopen(fname,"wt")) == NULL) return;
  iwdat1d(pf,out,3,"yld",1,"Yields of veg & meat/milk animal feed");
  iwdat(pf,out,11,"Finfish intake delay time","tfsh");
  iwdat(pf,out,13,"Shellfish intake delay time","tinv");
  iwdat1d(pf,out,15,"tcrp",1,"Harvest delays vegies & meat/milk feed");
  iwdat(pf,out,16,"Meat & milk animal feed & water intake","umt","umk","wmt","wmk");
  iwdat(pf,out,17,"Ag soil desity & retension fraction","den","ret");
  iwdat1d(pf,out,18,"trn",1,"Transloc factors veg & meat/milk feed");
  iwdat19(pf,out);
  iwdatloc(pf,out,20,1,"Irrigation rate GW","cirr");
  iwdat1d(pf,out,22,"usl",1,"Soil intake meat/milk animal");
  iwdatloc(pf,out,24,2,"Irrigation rate SW","cirr");
  iwdat1d(pf,out,26,"firr",1,"Irrigation fraction of GW & SW used");
  iwdat2d(pf,out,30,"tgrw",1,1,"GW Grow periods veg & meat/meat feed");
  iwdat2d(pf,out,34,"tgrw",3,1,"SW Grow periods veg & meat/meat feed");
  iwdat2d(pf,out,36,"tgrw",4,1,"Air Grow periods veg & meat/meat feed");
  iwdat2d(pf,out,37,"tgrw",5,1,"MS Grow periods veg & meat/meat feed");
  iwdatloc(pf,out,38,1,"GW treatment flag","ltrtl");
  iwdatloc(pf,out,39,2,"SW treatment flag","ltrtl");
  iwdatloc(pf,out,44,1,"Water transit time for GW","twtr");
  iwdatloc(pf,out,48,2,"Water transit time for SW","twtr");
  iwdatloc(pf,out,50,1,"Drinking water intake rate GW","udw");
  iwdatloc(pf,out,54,2,"Drinking water intake rate SW","udw");
  iwdat(pf,out,56,"Misc. see documentation","ubrsh","ubw","ubr","usw","usoil");
  iwdat(pf,out,57,"Time and frequency of boating","teboat","evboat");
  iwdat1d(pf,out,58,"uag",1,"Ingestion rates for vegies,meat & milk");
  iwdat1d(pf,out,59,"uaq",1,"Ingestion rates for shell & finfish");
  iwdat(pf,out,61,"Rfd ratio cutoff","rfdlim");
  iwdat(pf,out,62,"Cancer fatality & incidence conversions","hcom","hinc");
  iwdat(pf,out,63,"Thickness of skin layer","skinl");
  iwdat(pf,out,64,"Time and frequency of showering","teshwr","evshwr");
  iwdat(pf,out,65,"Time and frequency of swimming","teswim","evswim");
  iwdat66(pf,out);
  iwdat(pf,out,67,"Daily exposure time to direct raiadtion","tedird");
  iwdat(pf,out,68,"Andelman indoor air factors","andfc","andfr");
  iwdat69(pf,out);
  iwdat(pf,out,70,"Misc. soil factors","tss","rhoss","tas","rhoas");
  iwdat(pf,out,71,"Resuspension factor & inhalation rate","resfac","ubres");
  iwdat(pf,out,72,"MSoil thinkness & density, mass factor","tms","rhoms","cml");
  iwdat(pf,out,73,"External rad exp time to air & msoil","uext");
  iwdat(pf,out,74,"Misc shore, boat, & reduction factors","swf","sbrf","ssrf");
  iwdat(pf,out,75,"Sheilding & time for outdoors & indoors","sho","fto","shi","fti");
  iwdat1d(pf,out,76,"freq",1,"Misc annual factors");
  iwdat1d(pf,out,76,"freq",6,"Misc annual factors");
  iwdat1d(pf,out,76,"freq",11,"Misc annual factors");
  iwdat1d(pf,out,76,"freq",16,"Misc annual factors");
  iwdat1d(pf,out,76,"freq",21,"Misc annual factors");
  fprintf(out,"999");
  fclose(out);
}
