/*______________________________________________________________________________

   Date:       1993 - 2004
   Company:    Pacific Northwest National Laboratories
               Battelle Corporation
________________________________________________________________________________
__Modifiication  History________________________________________________________

  DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#include "distribution.h"

//------------------------------------------------------------------------------
void Distribution::DispTrun(FILE *k)
{
  int Limits;
  Limits = 0;
  if (strlen(Upper)>0)
    Limits+= 2;
  if (strlen(Lower)>0)
    Limits+= 1;
  fprintf(k,"  Truncation   %d ",Limits);
  if (Limits  == 1 || Limits  == 3)
    fprintf(k,"%9.3E ",ratof(Lower));
  else
    fprintf(k,"          ");
  if (Limits  == 2 || Limits  == 3)
    fprintf(k,"%9.3E\n",ratof(Upper));
  else
    fprintf(k,"\n");
}
//------------------------------------------------------------------------------
void Distribution::DispUnif (FILE *k)
{
  fprintf(k,"1 Uniform\n");
  fprintf(k,"  Truncation   0\n");
  fprintf(k,"  Parameters   %E  %E\n", ratof(Lower), ratof(Upper));
}
//------------------------------------------------------------------------------
void Distribution::DispLU(FILE *k)
{
  // Karl Changed ! Base to (e) values 11-27-01
  if (Base != 0)
    fprintf(k,"3 Log Uniform (e)\n");
  else
    fprintf(k,"2 Log Uniform (ten)\n");
  fprintf(k,"  Truncation   0\n");
  fprintf(k,"  Parameters   %E %E\n", ratof(Lower),ratof(Upper));
}
//------------------------------------------------------------------------------
void Distribution::DispNorm(FILE *k)
{
  fprintf(k,"4 Normal\n");
  DispTrun(k);
  fprintf(k,"  Parameters   %E  %E\n",Mean,Std);
}
//------------------------------------------------------------------------------
void Distribution::DispLN(FILE *k)
{
  // Karl Changed ! Base to (e) values 11-27-01
  if (Base != 0)
    fprintf(k,"6 Log Normal (e)\n");
  else
    fprintf(k,"5 Log Normal (ten)\n");
  DispTrun(k);
  fprintf(k,"  Parameters   %E  %E\n",Mean,Std);
}
//------------------------------------------------------------------------------
void Distribution::DispExp(FILE *k)
{
  fprintf(k,"7 Exponential\n");
  DispTrun(k);
  fprintf(k,"  Parameters   %E  %E  %E  %E\n",ratof(Lower),ratof(Upper),Mean,Scale);
}
//------------------------------------------------------------------------------
void Distribution::DispTri(FILE *k)
{
  fprintf(k,"8 Triangular\n");
  DispTrun(k);
  fprintf(k,"  Parameters   %E  %E  %E\n",ratof(Lower),ratof(Upper),Mode);
}
//------------------------------------------------------------------------------
void Distribution::DispGam(FILE *k)
{
  fprintf(k,"9 Gamma\n");
  DispTrun(k);
  fprintf(k,"  Parameters   %E  %E  %E  %E\n",ratof(Lower),ratof(Upper),Mean,Std);
}
//------------------------------------------------------------------------------
void Distribution::DispBet(FILE *k)
{
  fprintf(k,"10 Beta\n");
  DispTrun(k);
  fprintf(k,"  Parameters   %E  %E  %E  %E\n",ratof(Lower),ratof(Upper),Mean,Std);
}
//------------------------------------------------------------------------------
void Distribution::DispWei(FILE *k)
{
  fprintf(k,"11 Weibull\n");
  DispTrun(k);
  fprintf(k,"  Parameters   %E  %E  %E  %E  %E\n",ratof(Lower),ratof(Upper),Scale,Shift,Shift);
  /* something up with this distribution*/
}
//------------------------------------------------------------------------------
void Distribution::DispLog(FILE *k)
{
  fprintf(k,"12 Logistic\n");
  DispTrun(k);
  fprintf(k,"  Parameters   %E  %E  %E  %E\n",ratof(Lower),ratof(Upper),Mean,Scale);
}
//------------------------------------------------------------------------------
int Distribution::HasDist()
{
  return rstrcmpi(Type,"None");
}
//------------------------------------------------------------------------------
int Distribution::HasEquation()
{
  return strlen(Equation)>0;
}
//------------------------------------------------------------------------------
int Distribution::writeCor(FILE *k, Distribution *var, int idx)
{
  int i;
  int j;
  double *cors;

  if (!HasDist()) return 1;
  cors = new double[idx+1];
  cors[idx] = 1.0;
  for (i = 0; i<idx; i++)
  {
    cors[i] = 0.0;
    if (var[i].HasDist())
      for (j = 0; j<CorNum; j++)
        if (!rstrcmpi(CorAlias[j], var[i].Alias))
          cors[i] = Cor[j];
  }
  for (i = 0; i<idx+1; i++)
    if (var[i].HasDist())
      fprintf(k, "%#15.3G\n", cors[i]);
  delete[] cors;
  return 1;
}
//------------------------------------------------------------------------------
void Distribution::writeSUFLabel(fcsv *f)
{
  f->write(Alias);
}
//------------------------------------------------------------------------------
void Distribution::writeSUFValue(fcsv *f)
{
  f->write(p.valu);
}
//------------------------------------------------------------------------------
int Distribution::read(GIDFILE *g,int count)
{
  int i;
  char *temp = NULL;
  parameter *pm;

  if (!readStr(g,"suName",&temp,count)) return 0;

  strncpy(p.name,temp,SMALLSTRING-1); // prevent to long of string copy
  p.name[SMALLSTRING-1] = '\0';
  p.uunit[0] = '\0';
  p.punit[0] = '\0';
  p.ref = 0;
  delete temp;

  if (!readStr(g,"suAlias",&Alias,count)) return 0;
  if (!readStr(g,"suDes",&Des,count)) return 0;
  if (!readStr(g,"suExe",&Exe,count)) return 0;
  if (!readStr(g,"suMod",&Glyph,count)) return 0;

  if (!readStr(g,p.name,&temp,count))
  {
    if (!Load_GIDSection(g,Glyph))
      return 0;
  }
  else
    delete temp;

  pm = Get_Parameter(g,p.name);
  if (pm == NULL) return 1;
  Units = strdup(pm->punit);

  // need to read past if suFmt for backward compatibility
  if (!readStr(g,"suFmt",&Fmt,count)) rstrcpy(Fmt,"");
  rstrcpy(p.fmt, Fmt);
  if (!readStr(g,"suDistrib",&Type,count)) return 0;
  if (!readStr(g,"suUpper",&Upper,count)) return 0;
  if (!readStr(g,"suLower",&Lower,count)) return 0;
  if (!readDouble(g,"suMean",&Mean,count)) return 0;
  if (!readDouble(g,"suStd",&Std,count)) return 0;
  if (!readDouble(g,"suScale",&Scale,count)) return 0;
  if (!readDouble(g,"suShift",&Shift,count)) return 0;
  if (!readDouble(g,"suMode",&Mode,count)) return 0;
  if (!readInt(g,"suBase",&Base,count)) return 0;
  if (!readStr(g,"suEqu",&Equation,count)) return 0;
  if (!readInt(g,"suIndex1",&p.cnt1,count)) return 0;
  if (!readInt(g,"suIndex2",&p.cnt2,count)) return 0;
  if (!readInt(g,"suIndex3",&p.cnt3,count)) return 0;
  if (!readInt(g,"suIndex4",&p.cnt4,count)) return 0;
  if (!readInt(g,"suIndex5",&p.cnt5,count)) return 0;
  if (!readInt(g,"suIndex6",&p.cnt6,count)) return 0;
  if (!readInt(g,"suCorNum",&CorNum,count)) return 0;
//     if (!Get_Element_Units(g, &p)) return 0;
  if (CorNum>0)
  {
    CorAlias = new char*[CorNum];
    for (i = 0; i<CorNum; i++)
      CorAlias[i] = NULL;
    Cor = new double[CorNum];
    for (i = 0; i<CorNum; i++)
    {
      if (!readStr(g,"suCorAlias",&(CorAlias[i]),count,i+1)) return 0;
        if (!readDouble(g,"suCor",&(Cor[i]),count,i+1)) return 0;
    }
  }

  return 1;
}
//------------------------------------------------------------------------------
int Distribution::write(FILE *k,int i)
{
  static char *Space = "/   ";
  if (HasDist())
  {
    fprintf(k,"VARIABLE   %d   %d   ",i,i);
    if (!rstrcmpi(Type,"Uniform"))              DispUnif (k);
    else if (!rstrcmpi(Type,"Log Uniform"))     DispLU(k);
    else if (!rstrcmpi(Type,"Log Normal"))      DispLN(k);
    else if (!rstrcmpi(Type,"Normal"))          DispNorm(k);
    else if (!rstrcmpi(Type,"Triangular"))      DispTri(k);
    else if (!rstrcmpi(Type,"Gamma"))           DispGam(k);
    else if (!rstrcmpi(Type,"Beta"))            DispBet(k);
    else if (!rstrcmpi(Type,"Weibull"))         DispWei(k);
    else if (!rstrcmpi(Type,"Exponential"))     DispExp(k);
    else if (!rstrcmpi(Type,"Logistic"))        DispLog(k);

    if (strlen(Alias)>9) Alias[9] = '\0';
    if (strlen(Des)>60) Des[60] = '\0';

    fprintf(k,"LABEL      %d   %d \"%-9s\"\n",i,i,Alias);
    fprintf(k,"UNIQUE     %d   %d \"V%04d\"\n",i,i,i);
    fprintf(k,"DESCRIBE   %d   %d \"%s\"\n",i,i,Des);
    fprintf(k, "%-121s\n", Space);
    return 1;
  }
  else
    return 0;
}
//------------------------------------------------------------------------------
Distribution::Distribution()
{
  Alias = NULL;
  Des = NULL;
  Exe = NULL;
  Glyph = NULL;
  Fmt = NULL;
  Type = NULL;
  Equation = NULL;
  Upper = NULL;
  Lower = NULL;
  CorNum = 0;
}
//------------------------------------------------------------------------------
Distribution::~Distribution()
{
  int i;
  if (Alias != NULL) delete Alias;
  if (Des != NULL) delete Des;
  if (Exe != NULL) delete Exe;
  if (Fmt != NULL) delete Fmt;
  if (Glyph != NULL) delete Glyph;
  if (Type != NULL) delete Type;
  if (Upper != NULL) delete Upper;
  if (Lower != NULL) delete Lower;
  if (Equation != NULL) delete Equation;
  if (Units != NULL) delete Units;
  if (CorNum>0)
  {
    for (i = 0; i<CorNum; i++)
      if (CorAlias[i] != NULL) delete CorAlias;
    delete[] CorAlias;
    delete[] Cor;
  }
}
//------------------------------------------------------------------------------

