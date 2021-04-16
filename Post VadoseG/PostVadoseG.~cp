#pragma hdrstop
#include <condefs.h>
USEUNIT("..\Common Files\csv.cpp");
USEUNIT("..\Common Files\robust.cpp");
USEUNIT("..\Common Files\Line.cpp");
USEUNIT("..\Common Files\PolyClass.cpp");
USEUNIT("gid.cpp");
USERES("PostVadoseG.res");
//---------------------------------------------------------------------------
#include "..\Common Files\robust.h"
#include "gid.h"

icsv *in;
ocsv *out;
Section *gis;
GIDfile *gid;

long GetGeoObjIdx(char *modid,char *label)
{
  long i;
  long nummod;
  long numobj;
  Parameter* p;
  Parameter* gidx;
  long idx[6] = {0,0,0,0,0,0};

  gidx = gis->GetParameter("GeoObjIndex");
  p = gis->GetParameter("NumModules");
  nummod = p->GetEntry(idx)->Long();
  p = gis->GetParameter("ModuleId");
  for (i=1; i<=nummod; i++)
  {
    idx[0] = i;
    if (rstrcmpi(p->GetEntry(idx)->value,modid) == 0)
    {
      p = gis->GetParameter("NumModGeoObj");
      numobj = p->GetEntry(idx)->Long();
      if (numobj !=1) return -1;
      p = gis->GetParameter("ModGeoObjIndex");
      idx[1] = 1;
      idx[0] = p->GetEntry(idx)->Long();
      idx[1] = 0;
      return gidx->GetEntry(idx)->Long();
    }
  }
  return -1;
}

Section *GetGIS(long siteidx, char *modid)
{
  int notfound;
  long i,j;
  long nummod,modsrcnum;
  Section *fui;
  Section *g;
  Parameter *p;
  Entry *e;
  long idx[6] = {0,0,0,0,0,0};

  fui = gid->GetSection("CSM");
  p = fui->GetParameter("NumMod");
  idx[0]= siteidx;
  nummod = p->GetEntry(idx)->Long();
  p = fui->GetParameter("ModId");
  for (i=1, notfound=1; i<=nummod && notfound; i++)
  {
    idx[1] = i;
    if (rstrcmpi(p->GetEntry(idx)->value,modid) == 0)
    {
      p = fui->GetParameter("ModSrcNum");
      modsrcnum = p->GetEntry(idx)->Long();
      p = fui->GetParameter("ModSrcId");
      for (j=1; j<=modsrcnum && notfound; j++)
      {
        idx[2] = j;
        e = p->GetEntry(idx);
        if (rstrncmpi(e->value,"gis",3) == 0)
        {
          g = gid->GetSection(e->value);
          notfound = 0;
        }
      }
    }
  }
  if (notfound)
    return NULL;
  return g;
}

void WritePoly(Section *gis, long pidx)
{
  long i,j;
  long numpts;
  long idx[6] = {pidx,0,0,0,0,0};
  Parameter *p;

  p = gis->GetParameter("NumPolygonPts");

  numpts = p->GetEntry(idx)->Long();
  *out << numpts << NewLn;
  p = gis->GetParameter("PolygonPts");
  for (i=1; i<=numpts; i++)
  {
    idx[1] = i;
    for (j=1; j<=2; j++)
    {
      idx[2] = j;
      *out << p->GetEntry(idx)->Double();
    }
    *out << 0 << NewLn;
  }
}

void WriteValues(long count, long line)
{
  char dummy[256];
  for (long j=0; j<count; j++)
  {
    *in >> dummy;
    *out << dummy;
  }
  if (line)
  {
    *in >> NewLn;
    *out << NewLn;
  }
}

long main(long argc, char **argv)
{
  long i,j,k,pidx;
  long numlines;
  long numset;
  long numcon;
  long numtime;
  long numtype;
  long numprog;

  gid = new GIDfile(argv[1]);
  gid->Read();
  gis = GetGIS(ratoi(argv[3]),argv[5]);
  pidx = GetGeoObjIdx(argv[5],"boundary");
  if (pidx < 1)
  {
    delete gis;
    delete gid;
    printf("Error in reading FUI or GIS sections\n");
    return 1;
  }

  in = new icsv(argv[6]);
  out = new ocsv(argv[2]);
//changed by FCR so it reads in the section name
//  *in >> sname >> numslines >> NewLn;
  *in >> numlines >> NewLn;
  *out << numlines << NewLn;
  for (i=0; i<numlines; i++)
    WriteValues(1,1);
  *in >> numset >> NewLn;
  *out << numset << NewLn;

  for (i=0; i<numset; i++)
  {
    WriteValues(10,0);
    *in >> numcon >> NewLn;
    *out << numcon << NewLn;
    WritePoly(gis,pidx);
    WriteValues(2,0);
    *in >> numlines >> NewLn;
    *out << numlines << NewLn;
    for (j=0; j<numlines; j++)
      WriteValues(2,1);
    for (j=0; j<numcon; j++)
    {
      WriteValues(4,0);
      *in >> numtime >> numtype >> numprog >> NewLn;
      *out << numtime << numtype << numprog << NewLn;
      for (k=0; k<numtime; k++)
        WriteValues(numtype+1,1);
      for (k=0; k<numprog; k++)
      {
        WriteValues(4,0);
        *in >> numtime >> numtype;
        *out << numtime << numtype;
        WriteValues(2,1);
        for (int m=0; m<numtime; m++)
          WriteValues(numtype+1,1);
      }
    }
  }

  delete gid;
  //delete in;
  delete out;

  return 0;
}
