#include <math.h>
#include <stdlib.h>
#include "sumdata.h"
#include "gid.h"
#include "readfunc.h"

extern SUM App;
//------------------------------------------------------------------------------
int seekGlyph(fcsv *fp, char *fuiname, char *sglyph)
{
  char glyph[30];
  int i,n;
  fp->open(fuiname,_READ);
  do
  {
    fp->read(glyph);
    fp->read(&n);
    fp->readln();
    if (strcmpi(glyph,sglyph)!=0)
        for (i=0;i<n;i++)
          fp->readln();
  }
  while (strcmpi(glyph,sglyph)!=0 && !fp->eof());
  if (0==fp->eof())
  {
    fp->read(&n); // number of lines of header
    fp->readln();
    for (i=0;i<n;i++)  // read header lines
      fp->readln();
  }
  return fp->eof();
}
//------------------------------------------------------------------------------
void skipRecords(fcsv *fp, int n)
{
  int i;
  for (i=0;i<n;i++)
    fp->readln();
}
//------------------------------------------------------------------------------
void Variable::DispTrun(FILE *k)
{
  int Limits;
  Limits=0;
  if (strlen(Upper)>0)
    Limits+=2;
  if (strlen(Lower)>0)
    Limits+=1;
  fprintf(k,"  Truncation   %d ",Limits);
  if (Limits ==1 || Limits ==3)
    fprintf(k,"%9.3E ",atof(Lower));
  else
    fprintf(k,"          ");
  if (Limits ==2 || Limits ==3)
    fprintf(k,"%9.3E\n",atof(Upper));
  else
    fprintf(k,"\n");
}
//------------------------------------------------------------------------------
void Variable::DispUnif(FILE *k)
{
  fprintf(k,"1 Uniform\n");
  fprintf(k,"  Truncation   0\n");
  fprintf(k,"  Parameters   %E  %E\n", atof(Lower),atof(Upper));
}
//------------------------------------------------------------------------------
void Variable::DispLU(FILE *k)
{
  // Karl Changed ! Base to (e) values 11-27-01
  if (Base!=0)
    fprintf(k,"3 Log Uniform (e)\n");
  else
    fprintf(k,"2 Log Uniform (ten)\n");
  fprintf(k,"  Truncation   0\n");
  fprintf(k,"  Parameters   %E %E\n", atof(Lower),atof(Upper));
}
//------------------------------------------------------------------------------
void Variable::DispNorm(FILE *k)
{
  fprintf(k,"4 Normal\n");
  DispTrun(k);
  fprintf(k,"  Parameters   %E  %E\n",Mean,Std);
}
//------------------------------------------------------------------------------
void Variable::DispLN(FILE *k)
{
  // Karl Changed ! Base to (e) values 11-27-01
  if (Base!=0)
    fprintf(k,"6 Log Normal (e)\n");
  else
    fprintf(k,"5 Log Normal (ten)\n");
  DispTrun(k);
  fprintf(k,"  Parameters   %E  %E\n",Mean,Std);
}
//------------------------------------------------------------------------------
void Variable::DispExp(FILE *k)
{
  fprintf(k,"7 Exponential\n");
  DispTrun(k);
  fprintf(k,"  Parameters   %E  %E  %E  %E\n",atof(Lower),atof(Upper),Mean,Scale);
}
//------------------------------------------------------------------------------
void Variable::DispTri(FILE *k)
{
  fprintf(k,"8 Triangular\n");
  DispTrun(k);
  fprintf(k,"  Parameters   %E  %E  %E\n",atof(Lower),atof(Upper),Mode);
}
//------------------------------------------------------------------------------
void Variable::DispGam(FILE *k)
{
  fprintf(k,"9 Gamma\n");
  DispTrun(k);
  fprintf(k,"  Parameters   %E  %E  %E  %E\n",atof(Lower),atof(Upper),Mean,Std);
}
//------------------------------------------------------------------------------
void Variable::DispBet(FILE *k)
{
  fprintf(k,"10 Beta\n");
  DispTrun(k);
  fprintf(k,"  Parameters   %E  %E  %E  %E\n",atof(Lower),atof(Upper),Mean,Std);
}
//------------------------------------------------------------------------------
void Variable::DispWei(FILE *k)
{
  fprintf(k,"11 Weibull\n");
  DispTrun(k);
  fprintf(k,"  Parameters   %E  %E  %E  %E  %E\n",atof(Lower),atof(Upper),Scale,Shift,Shift);
  /* something up with this distribution*/
}
//------------------------------------------------------------------------------
void Variable::DispLog(FILE *k)
{
  fprintf(k,"12 Logistic\n");
  DispTrun(k);
  fprintf(k,"  Parameters   %E  %E  %E  %E\n",atof(Lower),atof(Upper),Mean,Scale);
}
//------------------------------------------------------------------------------
int Variable::HasDist()
{
  return strcmpi(Distribution,"None")!=0;
}
//------------------------------------------------------------------------------
int Variable::HasEquation()
{
  return strlen(Equation)>0;
}
//------------------------------------------------------------------------------
int Variable::writeCor(FILE *k,int n,Variable *var,int idx)
{
  int i,j;
  if (!HasDist()) return 1;
  float *cors;
  cors=new float[idx+1];
  cors[idx]=1.0;
  for (i=0;i<idx;i++)
  {
    cors[i]=0.0;
    if (var[i].HasDist())
      for (j=0;j<CorNum;j++)
        if (strcmpi(CorAlias[j],var[i].Alias)==0)
          cors[i]=Cor[j];
  }
  for (i=0;i<idx+1;i++)
    if (var[i].HasDist())
      fprintf(k,"%#15.3G\n",cors[i]);
  delete[] cors;
  return 1;
}
//------------------------------------------------------------------------------
Variable::Variable()
{
  Des=NULL;
  Alias=NULL;
  Glyph=NULL;
  Distribution=NULL;
  Equation=NULL;
  Upper=NULL;
  Lower=NULL;
  CorNum=0;
}
//------------------------------------------------------------------------------
int Variable::read(GIDFILE *g,int count)
{
  int i;
  char *temp;
  if (!readStr(g,count,"suName",&temp)) return 0;
  strncpy(p.name,temp,NAMEsize-1);
  p.name[NAMEsize-1]='\0';
  p.uunit[0]='\0';
  p.punit[0]='\0';
  p.ref=0;
  if (!readStr(g,count,"suAlias",&Alias)) return 0;
  if (!readStr(g,count,"suDes",&Des)) return 0;
  if (!readStr(g,count,"suMod",&Glyph)) return 0;
  if (!readStr(g,count,"suDistrib",&Distribution)) return 0;
  if (!readStr(g,count,"suUpper",&Upper)) return 0;
  if (!readStr(g,count,"suLower",&Lower)) return 0;
  if (!readFloat(g,count,"suMean",&Mean)) return 0;
  if (!readFloat(g,count,"suStd",&Std)) return 0;
  if (!readFloat(g,count,"suScale",&Scale)) return 0;
  if (!readFloat(g,count,"suShift",&Shift)) return 0;
  if (!readFloat(g,count,"suMode",&Mode)) return 0;
  if (!readInt(g,count,"suBase",&Base)) return 0;
  if (!readStr(g,count,"suEqu",&Equation)) return 0;
  if (!readInt(g,count,"suIndex1",&p.cnt1)) return 0;
  if (!readInt(g,count,"suIndex2",&p.cnt2)) return 0;
  if (!readInt(g,count,"suIndex3",&p.cnt3)) return 0;
  if (!readInt(g,count,"suIndex4",&p.cnt4)) return 0;
  if (!readInt(g,count,"suIndex5",&p.cnt5)) return 0;
  if (!readInt(g,count,"suIndex6",&p.cnt6)) return 0;
  if (!readInt(g,count,"suCorNum",&CorNum)) return 0;
//     if (!Get_Element_Units(g, &p)) return 0;
  if (CorNum>0)
  {
    CorAlias=new char*[CorNum];
    for (i=0;i<CorNum;i++)
      CorAlias[i]=NULL;
    Cor=new float[CorNum];
    for (i=0;i<CorNum;i++)
    {
      if (!readStr(g,count,"suCorAlias",&(CorAlias[i]),i+1)) return 0;
        if (!readFloat(g,count,"suCor",&(Cor[i]),i+1)) return 0;
    }
  }
  return 1;
}
//------------------------------------------------------------------------------
Variable::~Variable()
{
  int i;
  if (Alias!=NULL) delete Alias;
  if (Des!=NULL) delete Des;
  if (Glyph!=NULL) delete Glyph;
  if (Distribution!=NULL) delete Distribution;
  if (Upper!=NULL) delete Upper;
  if (Lower!=NULL) delete Lower;
  if (Equation!=NULL) delete Equation;
  if (CorNum>0)
  {
    for (i=0;i<CorNum;i++)
      if (CorAlias[i]!=NULL) delete CorAlias;
    delete[] CorAlias;
    delete[] Cor;
  }
}
//------------------------------------------------------------------------------
int Variable::write(FILE *k,int i)
{
  static char *Space="/   ";
  if (HasDist())
  {
    fprintf(k,"VARIABLE   %d   %d   ",i,i);
    if (strcmpi(Distribution,"Uniform")==0)
      DispUnif(k);
    else if (strcmpi(Distribution,"Log Uniform")==0)
      DispLU(k);
    else if (strcmpi(Distribution,"Log Normal")==0)
      DispLN(k);
    else if (strcmpi(Distribution,"Normal")==0)
      DispNorm(k);
    else if (strcmpi(Distribution,"Triangular")==0)
      DispTri(k);
    else if (strcmpi(Distribution,"Gamma")==0)
      DispGam(k);
    else if (strcmpi(Distribution,"Beta")==0)
      DispBet(k);
    else if (strcmpi(Distribution,"Weibull")==0)
      DispWei(k);
    else if (strcmpi(Distribution,"Exponential")==0)
      DispExp(k);
    else if (strcmpi(Distribution,"Logistic")==0)
      DispLog(k);
    if (strlen(Alias)>9) Alias[9]='\0';
    fprintf(k,"LABEL      %d   %d \"%-9s\"\n",i,i,Alias);
//      fprintf(k,"UNIQUE     %d   %d \"%-9s\"\n",i,i,Alias);
    fprintf(k,"UNIQUE     %d   %d \"V%04d\"\n",i,i,i);
    if (strlen(Des)>60) Des[60]='\0';
    fprintf(k,"DESCRIBE   %d   %d \"%s\"\n",i,i,Des);
    fprintf(k, "%-121s\n", Space);
    return 1;
  }
  else
    return 0;
}
//------------------------------------------------------------------------------
void Variable::writeSUFHeader(fcsv *f)
{
  f->write(Alias);
}
//------------------------------------------------------------------------------
void Variable::writeSUF(fcsv *f)
{
  f->write(p.valu);
}
//------------------------------------------------------------------------------
int SUMData::read(GIDFILE *gid,int Scenario)
{
  int i,j,num,found;
  char pre[8];
  paramrec pa;
  char *val;
  if (!readFloat(gid,0,"suSeed",&Seed))return 0;
  if (!readInt(gid,0,"suIter",&Iteration))return 0;
  if (!readInt(gid,0,"suVarNum",&VarCount)) return 0;
  Vars=new Variable[VarCount];
  for (i=0;i<VarCount;i++)
  {
    if (!Vars[i].read(gid,i+1)) return 0;
  }
  if (!readInt(gid,0,"suOutNum",&ResCount)) return 0;
  Res=new Result[ResCount];
  for (i=0;i<ResCount;i++)
    if (!Res[i].read(gid,i+1)) return 0;
    /* need to find and modify xxxModelStat for each Glyph */
  pa.cnt1 = Scenario; pa.cnt2 = 0; pa.cnt3 = 0;
  pa.cnt4 = 0; pa.cnt5 = 0; pa.cnt6 = 0;
  pa.ref=0; strcpy(pa.uunit,""); strcpy(pa.punit,"");
  Fui=new paramrec[VarCount];
  for (i=0;i<VarCount;i++)
  {
    sscanf(Vars[i].Glyph,"%[^0-9]",&pre);
//    sprintf(pa.name,"%s%s",pre,"Num");
    sprintf(pa.name,"NumMod");
    found=0;
    if (!readInt(gid,Scenario,pa.name,&num)) return 0;
    for (j=0;j<num;j++)
    {
      if (found) break;
//    sprintf(pa.name,"%s%s",pre,"Name");
      sprintf(pa.name,"ModId");
      val=NULL;
      if (!readStr(gid,Scenario,pa.name,&val,j+1)) return 0;
      if (0==strcmpi(val,Vars[i].Glyph))
      {
        pa.cnt2=j+1;
//      sprintf(pa.name,"%s%s",pre,"ModelStat");
        sprintf(pa.name,"ModState");
        strcpy(pa.valu,"1");
        copyparam(&(Fui[i]),&pa);
        found=1;
      }
      if (val!=NULL) delete val;
    }
  }
  return 1;
}
//------------------------------------------------------------------------------
int SUMData::writeKey(FILE *k)
{
  int i,j;
  j=1;
  for (i=0;i<VarCount;i++)
    if (Vars[i].write(k,j))
      j++;
  if (j>1) return 1;
  else return 0;
}
//------------------------------------------------------------------------------
int SUMData::writeCorMat(FILE *k)
{
  static char *Space="/   ";
  int i,count;
  i=0;
  while (i<VarCount && Vars[i].CorNum==0)
    i++;
  if (i==VarCount)
    fprintf(k,"CORRELATION Identity\n");
  else
  {
    count=0;
    for (i=0;i<VarCount;i++)
      if(Vars[i].HasDist()) count++;
        fprintf(k,"CORRELATION for %4d variables\n",count);
    for (i=0;i<VarCount;i++)
      Vars[i].writeCor(k,VarCount,Vars,i);
    fprintf(k, "/\n", Space);
  }
  return 1;
}
//------------------------------------------------------------------------------
int SUMData::readLHO(char *name,int current)
{
  char temp[80],spaces[80];
  char varname[80], alias[80];
  FILE *f;
  int i,j,iter,varcount,k;
  f=fopen(name,"rt");
  if (f!=NULL)
  {
    fscanf(f,"%[^\n]%c",spaces,&spaces[0]);
    fscanf(f,"%[^\n]%c",spaces,&spaces[0]);
    fscanf(f,"%[ ]%[^ ]",spaces,temp);
    varcount=atoi(temp);
    fscanf(f,"%[ ]%[^ ]",spaces,temp);
    iter=atoi(temp);
    fscanf(f,"%[^\n]%c",spaces,&spaces[0]);
    for (i=0;i<varcount;i++)
    {
      fscanf(f,"%[^ ]%[ ]%[^ ]%[^\n]%c",alias,spaces,varname,spaces,&spaces[0]);
      fscanf(f,"%[^\n]%c",spaces,&spaces[0]);
      for (j=0;j<iter;j++)
      {
        fscanf(f,"%[ ]%[^ ]%[ ]%[^ \n]%[^\n]%c",spaces,spaces,spaces,temp,spaces,&spaces[0]);
        if (j==current)
        {
          k=0; // Find the alias in the Variables Array
          while (k<VarCount && strcmpi(Vars[k].Alias,alias))
            k++;
          if (k<VarCount)
            strcpy(Vars[k].p.valu,temp);
        }
      }
    }
    fclose(f);
    return 1;
  }
  else
   return 0;
}
//------------------------------------------------------------------------------
int SUMData::writeTempGID(char *oldname,char *newname)
{
  char **glyphs;
  paramrec *parms;
  int i, rc;
  glyphs=new char*[VarCount];
  parms=new paramrec[VarCount];
  for (i=0;i<VarCount;i++)  // make copy of glyph names and paramrecs
  {
    glyphs[i]=Vars[i].Glyph;
    copyparam(&(parms[i]),&(Vars[i].p));
  }
  rc = Copy_Update_GID(oldname,newname,VarCount,glyphs,parms,Fui);
  if (rc)
    for (i=0;i<VarCount;i++)  // get results out of copy of paramrecs
      copyparam(&(Vars[i].p),&(parms[i]));
  delete[] glyphs;
  delete[] parms;
  return rc;
}
//------------------------------------------------------------------------------
int SUMData::readResults(char *fuiname)
{
  int i;
  writeError("readResults(",fuiname,")");
  writeError("ReadResults entered");
  for (i=0;i<ResCount;i++)
  {
    writeError("Alias: ",Res[i].Alias);
    if (!Res[i].read(fuiname)) return 0;
  }
  return 1;
}
//------------------------------------------------------------------------------
int SUMData::writeSUF(fcsv *f,int current,int total)
{
  int i,j,k,rcount;
  Result *res;
  char cbuf[256];

  if (current==0)
  {
    f->write(2);
    f->writeln();
    f->write("Sensitivity/Uncertainty Module for FRAMES");
    f->writeln();
    f->write("Beta Test version");
    f->writeln();
    rcount=0;
    for (i=0;i<ResCount;i++) {
      res = Res[i].home;
      while (res!=NULL) {
        rcount++;
        res = res->next;
        }
    }
//  for (i=0;i<ResCount;i++)
//    rcount+=Res[i].NumValues;
    f->write(VarCount);
//  f->write(ResCount);
    f->write(rcount);
    f->writeln();
    for (i=0;i<VarCount;i++)
      { f->write(Vars[i].Alias); f->write(Vars[i].Des); f->writeln();  }
    for (i=0;i<ResCount;i++)
    {
      res = Res[i].home;
      k=0;
      while (res!=NULL) {
        if ((res->next!=NULL) || (res!=res->home)) {
//      if (0<strlen(res->loctype)) {
          sprintf(cbuf,"%s[%d]",res->Alias,++k);
          f->write(cbuf);
          }
        else {
          f->write(res->Alias);
          }
        if (0<strlen(res->descrip))
          sprintf(cbuf,"%s [%s]",res->Des,res->descrip);
        else
          strcpy(cbuf,res->Des);
        f->write(cbuf);
        for (j=0;j<res->NumYears;j++)
          f->write(res->Years[j]);
        f->writeln();
        res = res->next;
      }
    }
    f->write(Iteration);  f->writeln();
    f->write("realizations");
    for (i=0;i<VarCount;i++)
      Vars[i].writeSUFHeader(f);
    for (i=0;i<ResCount;i++)
      Res[i].writeSeriesHeader(f);
    for (i=0;i<ResCount;i++)
      Res[i].writeSUFHeader(f);
    f->writeln();
  }
  f->write(current+1);
  for (i=0;i<VarCount;i++)
  {
    writeError("Vars[i].writeSUF(f)"); //should not be seen
    Vars[i].writeSUF(f);
  }
  for (i=0;i<ResCount;i++)
  {
    writeError("Res[i].writeSUF(f)"); //should not be seen
    Res[i].writeSUF(f);
    Res[i].reset();
  }
  f->writeln();
  return 1;
}

