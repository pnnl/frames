#ifndef SITEFLUX_H
#define SITEFLUX_H

#include "gid.h"
#include "fcsv.h"

const int FLUX=0;
const int CONC=1;

const int VADOSE=0;
const int AQUIFER=1;
const int SURFACEWATER=2;

const int RadconTimeSize=5001;


class SiteInfo
  {
    public:
    int Type;
    float wsleachv,wslength,wswidth,wstop;
  };

class FluxInfo
  {
    public:
    int nds,Rad;
    char casid[19],pcasid[19];
    char fscname[19];
    char fscasid[19];
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


extern SiteInfo Site;
extern FluxInfo Flux[200];
extern Endpoints Usage;

float RoundUp(float a);
int Ok(int count);
#endif