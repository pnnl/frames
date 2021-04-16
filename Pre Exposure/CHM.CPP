/*========================================================================
  º                    DATE         TIME     º Written by Mitch Pelton   º
  º Written      : August,1996               º For Pacific Northwest     º
  º Received     :                           º     National Laboratories º
  º                                          º     Battelle Corporation  º
  ========================================================================*/
#include "chm.h"

#define IGV(a,b) ratoi(GetEV(a,b))
#define FGV(a,b) ratof(GetEV(a,b))
#define GV GetEV
#define GR Get_Element_Ref

char *GetEV(element *e, paramrec *p)
{
  char *val;
  char tmp[256];

  val = Get_Element_Value(e,p);
  if (val==NULL && e!=NULL)
  {
    sprintf(tmp,"%s \[%d,%d,%d,%d,%d,%d\] variable not found defaulting to zero or blank!",e->parent->name,p->cnt1,p->cnt2,p->cnt3,p->cnt4,p->cnt5,p->cnt6);
    wrn.write(tmp);
    wrn.writeln();
    return NULL;
  }
  return val;
}

element *GetE(GIDFILE *f,char *pname)
{
  char tmp[256];

  element *e = Get_Element(f,pname);
  if (e==NULL)
  {
    sprintf(tmp,"%s variable not found",pname);
    wrn.write(tmp);
    wrn.writeln();
  }
  return e;
}


void Make_CHM(GIDFILE *gid, char *run, int SIdx)
{
  bool shift = false;

  wrn.writeln();
  wrn.write("***** NOTE ******");
  wrn.writeln();
  wrn.write("Radionuclide half-lives entered through the user interface are not currently used.");
  wrn.writeln();
  wrn.write("Values obtained from the file rmdlibm.dat are used.");
  wrn.writeln();

  /* Record 1 */
  element *fscasid = GetE(gid,"fscasid");
  element *fscname = GetE(gid,"fscname");
  /* Record 2 */
  element *clktype= GetE(gid,"clktype");
  element *clwm   = GetE(gid,"clwm");
  element *clvap  = GetE(gid,"clvap");
  element *clhlc  = GetE(gid,"clhlc");
  element *clsol  = GetE(gid,"clsol");
  element *clkoc  = GetE(gid,"clkoc");
  element *clkow  = GetE(gid,"clkow");
  element *clkperm= GetE(gid,"clkperm");
  /* Record 3 - References */
  /* Record 4 */
  element *clthalf = Get_Element(gid,"ecthalf");
  if (clthalf == NULL) { element *clthalf = GetE(gid,"clthalf"); shift = true; }
  element *clghalf = Get_Element(gid,"ecghalf");
  if (clghalf == NULL) { element *clghalf = GetE(gid,"clghalf"); shift = true; }
  element *clwhalf = Get_Element(gid,"ecwhalf");
  if (clwhalf == NULL) { element *clwhalf = GetE(gid,"clwhalf"); shift = true; }
  element *clshalf = Get_Element(gid,"ecshalf");
  if (clshalf == NULL) { element *clshalf = GetE(gid,"clshalf"); shift = true; }

  element *clrdfgi = GetE(gid,"clrdfgi");   // was element *cldfg
  element *cldfad  = GetE(gid,"cldfad");    // was element *cldfa
  element *cldex   = GetE(gid,"cldex");
  element *cldsh   = GetE(gid,"cldsh");
  element *cldimr  = GetE(gid,"cldimr");
  /* Record 5 - References */
  /* Record 6 */
  element *clbff   = GetE(gid,"clbff");
  element *clbfi   = GetE(gid,"clbfi");
  element *clbfv   = GetE(gid,"clbvlv");  // used oldname
  element *clbmt   = GetE(gid,"clfmt");   // used oldname
  element *clbmk   = GetE(gid,"clfmk");   // used oldname
  element *clwpf   = GetE(gid,"clwpf");
  element *clvdp   = GetE(gid,"clvd");
  element *clkcls  = GetE(gid,"clclass");
  /* Record 7 - References */
  /* Record 8 */
  element *clcpfh  = GetE(gid,"clcpfh");
  element *clcpfg  = GetE(gid,"clcpfg");
  element *clrfdh  = GetE(gid,"clrfdh");
  element *clrfdg  = GetE(gid,"clrfdg");   // chem reference dose
  element *clrdfs  = GetE(gid,"clrdfs");   // rads rem/pCi
  element *clitxih = GetE(gid,"clitxih");
  element *clitxig = GetE(gid,"clitxig");
  element *clihisi = GetE(gid,"clihisi");
  element *clippi  = GetE(gid,"clippi");
  element *clitfi  = GetE(gid,"clitfi");
  element *clfone  = GetE(gid,"clfonei");
  element *clabskn = GetE(gid,"clabskn");
  element *cldifco = GetE(gid,"cldifco");
  /* Record 9  - Includes some references also */
  element *clkcpfh = GetE(gid,"clkcpfh");
  element *clkcpfg = GetE(gid,"clkcpfg");
  element *clkrfdh = GetE(gid,"clkrfdh");
  element *clkrfdg = GetE(gid,"clkrfdg");
  element *nds     = GetE(gid,"nds");

  paramrec p = {false,SIdx,0,0,0,0,0,0,"","","","",""};
  char fname[128];
  int i,j,tmp;
  FILE *f;

  rstrcpy(fname,run);
  strcat(fname,".chm");
  if ((f=fopen(fname,"wt")) == NULL) return;
  int numcon = ratoi(info(gid,"numcon",SIdx,0,0,0,0,0));
  int npol = numcon;
  for (i=1; i<=numcon; i++)
    npol += ratoi(info(gid,"nds",SIdx,i,0,0,0,0));
  fprintf(f,"%4dChemical Database from GID file\n",npol);
  for (i=1; i<=numcon; i++)
  {
    p.cnt2 = i;
    p.cnt3 = 0;
    int numds = IGV(nds,&p);
    for (j=0; j<=numds; j++)
    {
      p.cnt3 = j;
      /* Record 1 */
      fprintf(f,"%-12s%-67s\n",GV(fscasid,&p),GV(fscname,&p));

      /* Record 2 */
      rstrcpy(fname,GV(fscname,&p));
      fname[18] = '\0';
      fprintf(f,"%2d%-18s%#10.4G%#10.4G%#10.4G%#10.4G%#10.4G%#10.4G%#10.4G\n",
      IGV(clktype,&p), fname,           FGV(clwm,&p),    FGV(clvap,&p),
      FGV(clhlc,&p),   FGV(clsol,&p),   FGV(clkoc,&p),   FGV(clkow,&p),
      FGV(clkperm,&p));

      /* Record 3 */
      fprintf(f,"%30d%10d%10d%10d%10d%10d%10d\n",
      GR(clwm,&p),   GR(clvap,&p),    GR(clhlc,&p),  GR(clsol,&p),
      GR(clkoc,&p),  GR(clkow,&p),    GR(clkperm,&p));

      /* Record 4 */
      if (shift)
      {
        tmp = p.cnt1;
        p.cnt1 = p.cnt2;
        p.cnt2 = p.cnt3;
        p.cnt3 = p.cnt4;
      }
      fprintf(f,"%#10.4G%#10.4G%#10.4G%#10.4G",
      FGV(clthalf,&p),  FGV(clghalf,&p),  FGV(clwhalf,&p),  FGV(clshalf,&p));
      if (shift)
      {
        p.cnt3 = p.cnt2;
        p.cnt2 = p.cnt1;
        p.cnt1 = tmp;
      }
      fprintf(f,"%#10.4G%#10.4G%#10.4G%#10.4G%#10.4G\n",
      FGV(clrdfgi,&p),  FGV(cldfad,&p),   FGV(cldex,&p),    FGV(cldsh,&p),
      FGV(cldimr,&p));
//                      FGV(cldfg,&p),   now clrfdgi
//                      FGV(cldfa,&p),   now cldfad

      /* Record 5 */
      fprintf(f,"%10d%10d%10d%10d%10d%10d%10d%10d%10d\n",
      GR(clthalf,&p),  GR(clghalf,&p),  GR(clwhalf,&p),  GR(clshalf,&p),
      GR(clrdfgi,&p),  GR(cldfad,&p),   GR(cldex,&p),    GR(cldsh,&p),
      GR(cldimr,&p));

      /* Record 6 */
      fprintf(f,"%#10.4G%#10.4G%#10.4G%#10.4G%#10.4G%#10.4G%#10.4G%5d\n",
      FGV(clbff,&p),  FGV(clbfi,&p),  FGV(clbfv,&p),  FGV(clbmt,&p),
      FGV(clbmk,&p),  FGV(clwpf,&p),  FGV(clvdp,&p),  IGV(clkcls,&p));

      /* Record 7 */
      fprintf(f,"%10d%10d%10d%10d%10d%10d%10d%5d\n",
      GR(clbff,&p),  GR(clbfi,&p),  GR(clbfv,&p),  GR(clbmt,&p),
      GR(clbmk,&p),  GR(clwpf,&p),  GR(clvdp,&p),  GR(clkcls,&p));

      if (IGV(clktype,&p) != 1)
      {
        /* Record 8 chem*/
        fprintf(f,"%#10.4G%#10.4G%#10.4G%#10.4G%5d%5d%5d%5d%5d%#10.4G%#10.4G%#10.4G\n",
        FGV(clcpfh,&p),   FGV(clcpfg,&p),   FGV(clrfdh,&p),   FGV(clrfdg,&p),
        IGV(clitxih,&p),  IGV(clitxig,&p),  IGV(clihisi,&p),  IGV(clippi,&p),
        IGV(clitfi,&p),   FGV(clfone,&p),   FGV(clabskn,&p),  FGV(cldifco,&p));
      }
      else
      {
        /* Record 8 rads*/
        fprintf(f,"%#10.4G%#10.4G%#10.4G%#10.4G%5d%5d%5d%5d%5d%#10.4G%#10.4G%#10.4G\n",
        FGV(clcpfh,&p),   FGV(clcpfg,&p),   FGV(clrfdh,&p),   FGV(clrdfs,&p),
        IGV(clitxih,&p),  IGV(clitxig,&p),  IGV(clihisi,&p),  IGV(clippi,&p),
        IGV(clitfi,&p),   FGV(clfone,&p),   FGV(clabskn,&p),  FGV(cldifco,&p));
      }
      /* Record 9 */
      fprintf(f,"%3d%7d%3d%7d%3d%7d%3d%7d%35d%10d%10d\n",
      IGV(clkcpfh,&p),   GR(clcpfh,&p),   IGV(clkcpfg,&p),   GR(clcpfg,&p),
      IGV(clkrfdh,&p),   GR(clrfdh,&p),   IGV(clkrfdg,&p),   GR(clrfdg,&p),
      GR(clfone,&p),    GR(clabskn,&p),  GR(cldifco,&p));
    }
  }
  fclose(f);
}


