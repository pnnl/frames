#ifndef SITEFLUX_H
#define SITEFLUX_H

#include "gid.h"
#include "PolyClass.h"
#include "robust.h"

const int VADOSE=0;
const int AQUIFER=1;
const int SURFACEWATER=2;
const int OVERLAND=3;
extern PolygonClass *vadpoly;
extern int AQUSRC;

PolygonClass *GetPolygon(GIDFILE *pf, char *modid);
float RoundUp(float a);

class SiteInfo
  {
    public:
    int Type;
    float wsleachv,wslength,wswidth,wstop;
  };

extern SiteInfo Site;

class FluxInfo
  {
    public:
    int nds,Rad;
    char casid[20],pcasid[20];
    char fscname[9];
    char fscasid[9];
    float sollim,al,kd,conc;
    float wstlife,wsinvent,*wsflux,*wstime;
    int wsnum;
   ~FluxInfo();
    FluxInfo();
    void SetInfo(GIDFILE *pf,int pid,int did,int Site,int NDS,int Type);
    void ReduceCount();
    void ReduceCountFractMass(float fract);
    void Read(fcsv *wff,SiteInfo *s,char *sglyph,char *mglyphm,int Type1,int Type2,int Type3);
    void Clear();
  };

extern FluxInfo Flux[200];
const int FLUX=0;
const int CONC=1;


class Endpoint
  {
    public:
    int Type;
    int Index;
    char Name[30];
  };

class Endpoints
  {
    public:
    int NumEndPoints;
    Endpoint *All;
    Endpoints();
    void Add(char *name,int type,int index);
  };

extern Endpoints Usage;

int Ok(int count);
#endif