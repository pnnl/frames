//------------------------------------------------------------------------------
#include "sumdata.h"

//------------------------------------------------------------------------------
int SUMData::read(GIDFILE *gid,int SiteIdx)
{
  int i,j,num;
  paramrec pa;
  char *val;

  if (!readDouble(gid,"suSeed",&Seed)) return 0;
  if (!readInt(gid,"suIter",&Iteration)) return 0;
  if (!readInt(gid,"suVarNum",&VarCount)) return 0;
  Vars = new Distribution[VarCount];
  for (i = 0; i<VarCount; i++)
    if (!Vars[i].read(gid,i+1))
      return 0;
  if (!readInt(gid,"suOutNum",&ResCount)) return 0;
  Res = new Result[ResCount];
  for (i = 0; i<ResCount; i++)
    if (!Res[i].read(gid,i+1)) return 0;
    /* need to find and modify xxxModelStat for each Glyph */
  pa.cnt1 = SiteIdx;
  pa.cnt2 = 0;
  pa.cnt3 = 0;
  pa.cnt4 = 0;
  pa.cnt5 = 0;
  pa.cnt6 = 0;
  pa.ref = 0;
  rstrcpy(pa.uunit,"");
  rstrcpy(pa.punit,"");
  sprintf(pa.name,"NumMod");
  if (!readInt(gid,pa.name,&num,SiteIdx)) return 0;
  Fui = new paramrec[VarCount];
  for (j = 0; j<num; j++)
  {
    sprintf(pa.name,"ModId");
    val = NULL;
    if (!readStr(gid,pa.name,&val,SiteIdx,j+1)) return 0;
    for (i = 0; i<VarCount; i++)
    {
      if (!rstrcmpi(val,Vars[i].Glyph))
      {
        if (strlen(Vars[i].Exe) > 0)
        {
          pa.cnt2 = j+1;
          sprintf(pa.name,"ModState");
          rstrcpy(pa.valu,"1");
          copyparamrec(&(Fui[i]),&pa);
        }
      }
    }
    if (val != NULL) delete val;
  }
  return 1;
}
//------------------------------------------------------------------------------
int SUMData::writeKey(FILE *k)
{
  int i,j;
  j = 1;
  for (i = 0; i<VarCount; i++)
    if (Vars[i].write(k,j))
      j++;
  if (j>1) return 1;
  else return 0;
}
//------------------------------------------------------------------------------
int SUMData::writeCorMat(FILE *k)
{
  static char *Space = "/   ";
  int i,count;
  i = 0;
  while (i<VarCount && Vars[i].CorNum == 0) i++;
  if (i == VarCount)
    fprintf(k,"CORRELATION Identity\n");
  else
  {
    count = 0;
    for (i = 0; i<VarCount; i++)
      if (Vars[i].HasDist()) count++;
        fprintf(k,"CORRELATION for %4d variables\n",count);
    for (i = 0; i<VarCount; i++)
      Vars[i].writeCor(k,Vars,i);
    fprintf(k, "/\n", Space);
  }
  return 1;
}
//------------------------------------------------------------------------------
int SUMData::readLHO(char *name,int current)
{
  FILE *f;
  char temp[80],spaces[80];
  char varname[80], alias[80];
  int i,j,iter,varcount,k;

  f = fopen(name,"rt");
  if (f != NULL)
  {
    fscanf(f,"%[^\n]%c",spaces,&spaces[0]);
    fscanf(f,"%[^\n]%c",spaces,&spaces[0]);
    fscanf(f,"%[ ]%[^ ]",spaces,temp);
    varcount = ratoi(temp);
    fscanf(f,"%[ ]%[^ ]",spaces,temp);
    iter = ratoi(temp);
    fscanf(f,"%[^\n]%c",spaces,&spaces[0]);
    for (i = 0; i<varcount; i++)
    {
      fscanf(f,"%[^ ]%[ ]%[^ ]%[^\n]%c",alias,spaces,varname,spaces,&spaces[0]);
      fscanf(f,"%[^\n]%c",spaces,&spaces[0]);
      for (j = 0; j<iter; j++)
      {
        fscanf(f,"%[ ]%[^ ]%[ ]%[^ \n]%[^\n]%c",spaces,spaces,spaces,temp,spaces,&spaces[0]);
        if (j == current)
        {
          k = 0; // Find the alias in the Variables Array
          while (k<VarCount && rstrcmpi(Vars[k].Alias,alias))
            k++;
          if (k<VarCount)
            rstrcpy(Vars[k].p.valu,temp);
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
int SUMData::writeTempGID(char *sname,char *dname)
{
  int i,j,k;
  bool usedglyph;
  paramrec *pa, *p;
  GIDFILE *s,*d;

  s = Open_GID(sname);
  if (s == NULL) return 0;
  d = new GIDFILE;
  if (d->f.open(dname,WRITE) != 1)
  {
    Close_GID(s);
    delete d;
    return 0;
  }
  do
    if (readglyph(s))
    {
      writeglyph(d);
      usedglyph = !rstrcmpi("CSM",glyph.name);
      for (j = 0; j<VarCount && !usedglyph; j++)
        if (!rstrcmpi(Vars[j].Glyph,glyph.name))
          usedglyph = true;
      for (i = 0; i<glyph.cnt; i++)
      {
        pa = readparamrec(s);
        for (j = 0; j<VarCount && usedglyph; j++)
          if (!rstrcmpi(Vars[j].Glyph,glyph.name))
          {
            p = &(Vars[j].p);
            if (paramequal(p,pa))
            {
              if (strlen(p->fmt))
                sprintf(p->valu,p->fmt,ratof(p->valu));
              for (k = 0; k<LARGESTRING; k++)
                pa->valu[k] = p->valu[k];
            }
          }
          else if (!rstrcmpi("CSM",glyph.name))
            if (paramequal(&(Fui[j]),pa))
              for (k = 0; k<LARGESTRING; k++)
                pa->valu[k] = Fui[j].valu[k];
        writeparamrec(d,pa);
      }
    }
    else
    {
      Close_GID(s);
      d->f.close();
      delete d;
      return 0;
    }
  while (!s->f.eof());
  Close_GID(s);
  d->f.close();
  delete d;
  return 1;
}
//------------------------------------------------------------------------------
int SUMData::readResults(char *fuiname)
{
  int i;

  for (i = 0; i<ResCount; i++)
    if (!Res[i].ProcessPDCF(fuiname))
    {
      writeError("Alias:",Res[i].Alias, "in error");
//      return 0;
    }
  return 1;
}
//------------------------------------------------------------------------------
int SUMData::writeSUF(fcsv *f,int current,int total, bool errfile)
{
  int i,j,rcount;
  Result *res;
  char cbuf[MEDSTRING];

  // write info the the first
  if (current == 0)
  {
    f->write(2);
    f->writeln();
    f->write("Sensitivity/Uncertainty Module for FRAMES");
    f->writeln();
    f->write("=========================================");
    f->writeln();

    // count actual number of result outputs
    rcount = 0;
    for (i = 0; i<ResCount; i++)
    {
      res = Res[i].home;
      while (res != NULL)
      {
        rcount++;
        res = res->next;
      }
    }
    f->write(VarCount);
    f->write(rcount);
    f->writeln();
    for (i = 0; i<VarCount; i++)
    {
      f->write(Vars[i].Alias);
      sprintf(cbuf,"%s (%s)",Vars[i].Des, Vars[i].Units);
      f->write(cbuf);
      f->writeln();
    }

    for (i = 0; i<ResCount; i++)
    {
      res = Res[i].home;
      while (res != NULL)
      {
        if (res->consumer != NULL)
          sprintf(cbuf,"%s.%s.%s",res->Alias,res->consumer,res->qual);
        else
          sprintf(cbuf,"%s.%s.%s",res->Alias,res->producer,res->qual);
        f->write(cbuf);
        sprintf(cbuf,"%s (%s)",res->Des, res->yunit);
        f->write(cbuf);

        // need count of
        if (!rstrcmpi(res->Time,"peak"))
        {
          f->write(2);
        }
        else if (!rstrcmpi(res->Time,"average years # to #"))
        {
          f->write(1);
        }
        else
          f->write(res->NumYears);

        f->write(res->xunit);
        for (j = 0; j<res->NumYears; j++)
          f->write(res->Years[j]);
        f->writeln();
        res = res->next;
      }
    }
    f->write(Iteration);
    f->writeln();
    f->write("realizations");
    for (i = 0; i<VarCount; i++)
      Vars[i].writeSUFLabel(f);
    for (i = 0; i<ResCount; i++)
      Res[i].writeSUFLabels(f);
    f->writeln();
  }
  // write variables and monitored outputs
  f->write(current+1);
  for (i = 0; i<VarCount; i++)
    Vars[i].writeSUFValue(f);
  if (!errfile)
    for (i = 0; i<ResCount; i++)
    {
      Res[i].writeSUFValues(f);
      Res[i].reset();
    }
  f->writeln();
  return 1;
}
//------------------------------------------------------------------------------
void SUMData::cleanup()
{
  for (int i = 0; i<ResCount; i++)
    Res[i].reset();
}
//------------------------------------------------------------------------------
SUMData::~SUMData()
{
  if (Vars) delete[] Vars;
  if (Res) delete[] Res;
  if (Fui) delete[] Fui;
}

