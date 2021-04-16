#include "siteflux.h"
#include "robust.h"
#include "reduce.h"
#include <math.h>

SiteInfo Site;
FluxInfo Flux[200];
Endpoints Usage;

#define df_dt(i) (wsflux[i+1]-wsflux[i])/((wstime[i+1]-wstime[i]))
#define d2f_dt2(i) (df_dt(i)-df_dt(i-1))/((wstime[i]-wstime[i-1]))
#define swap(a,b) a^=b; b^=a; a^=b

float RoundUp(float a)
  {
    float loga,siga,expa;
    if (a==0.0) return 0.0;
    loga=log(a)/log(10.0);
    expa=(float)((int)loga);
    loga-=expa;
    siga=pow(10.0,loga);
    siga+=0.01;
    loga=log(siga)/log(10.0);
    loga+=expa;
    return pow(10.0,loga);
  }

int Ok(int count)
  {
    return 1;
  }

int UnInteresting(int n,float *time,float *flux)
  {
    int i;
    float total;
    if (n<2) return 1;
    total=0.0;
    for (i=1;i<n;i++)
      total+=(flux[i]+flux[i-1])*(time[i]-time[i-1])*0.5;
    if (total<1e-30) return 1;  // total mass is less than 1e-30
    i=1;
    while (i<n && fabs(time[i-1]-time[i])>fabs(time[i])*1e-5)
      // times are more four orders of magnatude different
      i++;
    if (i<n) return 1; // times are very close together
    else return 0;
  }

void FluxInfo::ReduceCount()
  {
    int i,j,peak,tagged;
    float *deriv;
    int   *use;
    int   *map;
    float *newtime,*newflux;

    if (wsnum<=RadconTimeSize) return;
    if (UnInteresting(wsnum,wstime,wsflux))
      {
        wsnum=2;
        return;
      }
    newtime=new float[RadconTimeSize];
    newflux=new float[RadconTimeSize];
    deriv=new float[wsnum];
    use=new int[wsnum];
    map=new int[wsnum];
    peak=0;
    for (i=0;i<wsnum;i++)
      {
        if (wsflux[i]>wsflux[peak]) peak=i;  // find peak
        if (i==0)
          deriv[i]=fabs(d2f_dt2(1)); // two forward differences
        else if (i==wsnum-1)
          deriv[i]=fabs(d2f_dt2(wsnum-2)); // two backward differences
        else
          deriv[i]=fabs(d2f_dt2(i));  //centered difference
        use[i]=0;
        map[i]=i;
      }
    if (peak==0 || peak==wsnum-1) tagged=2;
    else tagged=3;
    use[0]=1;       // mark first time zero to be used
    use[wsnum-1]=1; // mark last time to be used
    use[peak]=1;    // mark peak time to be used
    for (i=0;i<wsnum;i++)  // simple bubble sort on map values
      for (j=i+1;j<wsnum;j++)
        if (deriv[map[i]]<deriv[map[j]])
          {
            swap(map[i],map[j]);
          }
    j=0;
    for (i=0;i<wsnum;i++)  // mark the largest RadconSize-tagged derivatives
      if (j<RadconTimeSize-tagged)
        if (!use[map[i]])
          {
            use[map[i]]=1;
            j++;
          }
    j=0;
    for (i=0;i<wsnum;i++) // move data into new arrays
      if (use[i])
        {
          newflux[j]=wsflux[i];
          newtime[j]=wstime[i];
          j++;
        }
    delete[] deriv;
    delete[] use;
    delete[] map;
    delete[] wstime;  wstime=NULL;
    delete[] wsflux;  wsflux=NULL;
    wstime=newtime;
    wsflux=newflux;
    wsnum=RadconTimeSize;
  }

void FluxInfo::ReduceCountFractMass(float fract)
  {
    int i,last;
    float *total;
    if (wsnum==1) return;
    if (wsnum==2) return;
    if (wsnum<=RadconTimeSize) return;
    total=new float[wsnum];
    total[0]=0.0;
    for (i=1;i<wsnum;i++)
      total[i]=total[i-1]+0.5*(wsflux[i]+wsflux[i-1])*(wstime[i]-wstime[i-1]);
    if (total[wsnum-1]==0.0) last=2;
    else
      {
        last=0;
        while (total[last]<total[wsnum-1]*fract)
          last++;
        if (last<wsnum) last++;
      }
    delete[] total;
    wsnum=last;
  }
/* reduce count below used an averaging technique to reduce count.
  it does not work well for the case were 41 point are reduced.  41 points
  would be reduced to 21 and the peak is reduced.
void FluxInfo::ReduceCount()
  {
    int i,j,step,count;
    float totaltime,totalmass,*newtime,*newflux,dt;
    if (wsnum<=41) return;
    step=wsnum/40+1;
    count=wsnum/step;
    newtime=new float[count+1];
    newflux=new float[count+1];
    newtime[0]=0.0;
    newflux[0]=0.0;
    for (i=1;i<wsnum;i+=step)
      {
        totaltime=0.0;
        totalmass=0.0;
        for (j=0;j<step;j++)
          {
            if (i+j<wsnum)
              {
                dt=wstime[i+j]-wstime[i+j-1];
                totaltime+=dt;
                totalmass+=0.5*(wsflux[i+j]+wsflux[i+j-1])*dt;
              }
          }
        newtime[i/step+1]=wstime[i-1]+totaltime*0.5; // mid point time reported
        newflux[i/step+1]=totalmass/totaltime; // averaged flux for interval
      }
    wstime=newtime;
    wsflux=newflux;
    wsnum=count+1;
  }
*/
void FluxInfo::SetInfo(GIDFILE *pf,int pid,int did,int Site,int NDS,int Type)
{
  char *inf;
  if (ratoi(info(pf,"CLKTYPE",Site,pid,did))==1) Rad=1;
  else Rad=0;
  kd=ratof(info(pf,"WASUBKD",pid,did));
  nds=NDS;
  rstrcpy(pcasid,info(pf,"FSCASID",Site,pid));
  rstrcpy(casid,info(pf,"FSCASID",Site,pid,did));
  strncpy(fscname,info(pf,"FSCNAME",Site,pid,did),18);
  fscname[18]='\0';
  strncpy(fscasid,casid,18);
  fscasid[18]='\0';

  //  Get apporpriate half life
  al = ratof(info(pf,"CLGHALF",Site,pid,did));
  if (Type==AQUIFER)
  {
    inf=info(pf,"WZGHALF",pid,did);
    if (inf!=NULL) al=ratof(inf);
  }
  else if (Type==VADOSE)
  {
    inf=info(pf,"WPGHALF",pid,did);
    if (inf!=NULL) al=ratof(inf);
  }
  else // river
  {
    inf=info(pf,"WWSHALF",pid,did);
    if (inf!=NULL) al=ratof(inf);
  }

  //  Get apporpriate solubility limit
  sollim = ratof(info(pf,"CLSOL",Site,pid,did));
  if (Type==AQUIFER)
  {
    if (Rad) inf=info(pf,"WZRSOL",pid,did);
    else     inf=info(pf,"WZSOL",pid,did);
    if (inf!=NULL) sollim=ratof(inf);
  }
  else if (Type==VADOSE)
  {
    if (Rad) inf=info(pf,"WPRSOL",pid,did);
    else     inf=info(pf,"WPSOL",pid,did);
    if (inf!=NULL) sollim=ratof(inf);
  }
  else // river
  {
    if (Rad) inf=info(pf,"WWRSOL",pid,did);
    else     inf=info(pf,"WWSOL",pid,did);
    if (inf!=NULL) sollim=ratof(inf);
  }
  if (!Rad) sollim*=1e-6; // mg/l -> g/ml
  }

FluxInfo::FluxInfo()
  {
    wsflux=NULL;
    wstime=NULL;
  }

void FluxInfo::Read(fcsv *wff,SiteInfo *s,char *sglyph,char *mglyph,int Type1,int Type2,int Type3)
{
  int i,j,k,m,n,numcon,nds,done,media,peak,ot,FCount;
  char glyph[30],units[30],tcasid[30],dummy[100],rglyph[30];
  float leachv,time,ttime,tflux,tflux2;

  done=0;
  wff->rewind();
  do
  {
    wff->read(glyph);
    wff->read(&n);
    wff->readln();
    if (strcmpi(glyph,sglyph)!=0)
      for (i=0;i<n;i++)
        wff->readln();
  }
  while (strcmpi(glyph,sglyph)!=0 && !wff->eof());
  // number of lines of header
  wff->read(&n);
  wff->readln();
  // read header lines
  for (i=0;i<n;i++) wff->readln();
  wff->read(&media);
  wff->readln();
  for (m=0;m<media && !done;m++)
  {
    wff->read(rglyph);
    if (strcmpi(rglyph,"all") && strcmpi(rglyph,mglyph))
    {
      //need to read past this dataset
      for (i=0;i<9;i++) wff->read(dummy);
      wff->read(&numcon);
      wff->readln();
      for (i=0;i<2;i++) wff->read(dummy);
      wff->read(&n);
      for (i=0;i<=n;i++) wff->readln();
      for (i=0;i<numcon;i++)
      {
        for (j=0;j<4;j++) wff->read(dummy);
        wff->read(&n);
        wff->read(dummy);
        wff->read(&nds);
        for (j=0;j<=n;j++) wff->readln();
        for (j=0;j<nds;j++)
        {
          for (k=0;k<4;k++) wff->read(dummy);
          wff->read(&n);
          for (k=0;k<=n;k++) wff->readln();
        }
      }
    }
    else
    {
      wff->read(dummy);
      if (strncmpi(dummy,"Vadose",6)==0)              s->Type=VADOSE;
      else if (strncmpi(dummy,"Aquifer",7)==0)        s->Type=AQUIFER;
      else if (strncmpi(dummy,"Surface Water",13)==0) s->Type=SURFACEWATER;
      else /*assume a horizontal flux plane */        s->Type=VADOSE;
      wff->read(&s->wswidth);s->wswidth*=100.0;   // m -> cm
      wff->read(units);
      wff->read(&s->wslength);s->wslength*=100.0; // m -> cm
      wff->read(units);
      wff->read(&s->wstop);s->wstop*=100.0;       // m -> cm
      wff->read(units);
      wff->read(&s->wsleachv); s->wsleachv*=100.0/365.25; // m/yr -> cm/day
      wff->read(units);
      wff->read(&numcon);
      wff->readln();
      wff->read(units);
      wff->read(units);
      wff->read(&n);
      wff->readln();
      for (i=0;i<n;i++)
      {
        if (i==0)
        {
          wff->read(&time);
          wff->read(&leachv); // m^3/yr -> cm/day
          if (s->wswidth*s->wslength > 0)
            s->wsleachv=(leachv*1e6/(s->wswidth*s->wslength))/365.25;
        }
        wff->readln();
      }
      wsnum=0;
      for (i=0;i<numcon && !done;i++)
      {
        wff->read(dummy);
        wff->read(tcasid);
        wff->read(units);
        wff->read(units);
        wff->read(&n);
        wff->read(&FCount);
        wff->read(&nds);
        wff->readln();
        if (strncmpi(pcasid,tcasid,strlen(pcasid))==0 && (s->Type==Type1 || s->Type==Type2 || s->Type==Type3))
        {
          if (strcmpi(casid,pcasid)==0) // grab parent flux rates
          {
            ot=0;
            wsnum=n;
            wsflux=new float[n+1];
            wstime=new float[n+1];
            wstime[0]=0.0;
            wsflux[0]=0.0;
            for (j=0;j<n;j++)
            {
              wff->read(&ttime);  // yr
              wff->read(&tflux);  // pCi/yr or g/yr
              if (FCount==2)
              {
                wff->read(&tflux2);
                tflux+=tflux2;
              }
// temporary removal fix in radcon of following line
//              if (ttime!=0.0 && j==0) ot=1;
              wstime[j+ot]=ttime;
              wsflux[j+ot]=tflux;
              wff->readln();
            }
            wsnum+=ot;
            done=1;
          }
          else
          {
            // skip parent flux rates
            for (j=0;j<n;j++) wff->readln();
            for (j=0;j<nds && !done;j++)
            {
              wff->read(dummy);
              wff->read(tcasid);
              wff->read(units);
              wff->read(units);
              wff->read(&n);
              wff->read(&FCount);
              wff->read(dummy);
              wff->read(dummy);
              wff->readln();
              if (strncmpi(tcasid,casid,strlen(casid))==0) // read progeny flux rates
              {
                ot=0;
                wsnum=n;
                wsflux=new float[n+1];
                wstime=new float[n+1];
                wstime[0]=0.0;
                wsflux[0]=0.0;
                for (k=0;k<n;k++)
                {
                  wff->read(&ttime);  // yr
                  wff->read(&tflux);  // pCi/yr or g/yr
                  if (FCount==2)
                  {
                    wff->read(&tflux2);
                    tflux+=tflux2;
                  }
                  if (ttime!=0.0 && k==0) ot=1;
                  wstime[k+ot]=ttime;
                  wsflux[k+ot]=tflux;
                  wff->readln();
                }
                wsnum+=ot;
                done=1;
              }
              else
                for (k=0;k<n;k++)
                  wff->readln();
            }
          }
        }
        else
        {
          // skip parent and child flux rates
          for (j=0;j<n;j++) wff->readln();
          for (j=0;j<nds;j++)
          {
            for (k=0;k<4;k++) wff->read(dummy);
            wff->read(&n);
            for (k=0;k<=n;k++) wff->readln();
          }
        }
      }
    }
  }
  if (done)
  {
//    ReduceCount();
//    ReduceCountFractMass(0.999); // Only worry about 99.9% of mass
    double ratio=.0001;
    Reduce(&wsnum,&wstime,&wsflux,&ratio);
    wstlife=wstime[wsnum-1];
    wsinvent=0.0;
    peak=0;
    for (i=1;i<wsnum;i++)
    {
//      if (wsflux[i]!=0.0 && wsflux[i-1]!=0.0)
      if (i>1 || ot==0) // add in all times > 1 and when time 0 is given
        wsinvent+=0.5*(wstime[i]-wstime[i-1])*(wsflux[i]+wsflux[i-1]);
      if (wsflux[peak]<wsflux[i]) peak=i;
    }
    if (leachv==0.0 || wsflux[peak]==0.0)
      conc=0.0;
    else if (log10(wsflux[peak]) - log10(leachv) > 30.0)
      conc=0.0;
    else
      conc=wsflux[peak]/leachv; // pCi/m^3 of water or g/m^3 of water
    if (Rad)
      conc*=1e-6; //pCi/cm^3 of water = pCi/ml
    else
      conc*=1e-6; // g/cm^3 of water = g/ml
    if (wsinvent==0.0) wsinvent=1e-30;
  }
}

FluxInfo::~FluxInfo()
  {
    if (wsflux!=NULL)
        delete[] wsflux;
    if (wstime!=NULL)
      delete[] wstime;
    wsnum=0;
  }

Endpoints::Endpoints()
  {
    NumEndPoints=0;
    All=NULL;
  }

void Endpoints::Add(char *name,int type,int index)
  {
    Endpoint *p;
    int i;

    p=new Endpoint[NumEndPoints+1];
    for (i=0;i<NumEndPoints;i++)
      p[i]=All[i];
    rstrcpy(p[i].Name,name);
    p[i].Type=type;
    p[i].Index=index;
    if (All!=NULL) delete[] All;
    All=p;
    NumEndPoints++;
  }


