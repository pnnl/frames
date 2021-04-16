#include <math.h>
#include <windows.h>


int NextUsed(int Start,int Max,int *Used)
  {
    int i;
    for (i=Start;i<Max && !Used[i];i++);
    return i;
  }

float Integrate(int Num,float *Times,float *Flux,int *Used)
  {
    int i,nexti;
    float NewMass;
    NewMass=0.0;
    i=NextUsed(0,Num,Used);
    while (i<Num)
      {
        nexti=NextUsed(i+1,Num,Used);
        if (nexti<Num)
          NewMass+=0.5*(Flux[i]+Flux[nexti])*(Times[nexti]-Times[i]);
        i=nexti;
      }
    return NewMass;
  }

int NumUsed(int Num,int *Used)
  {
    int i, ct=0;
    for (i=0;i<Num;i++)
      if (Used[i]) ct++;
    return ct;
  }

int NumCriticalPoints(int Num, int *Used)
  {
    int i, ct=0;
    for (i=0;i<Num;i++)
      if (Used[i]>=2) ct++;
    return ct;
  }

void Reduce(int *NumFlux,float **Times,float **Fluxes, double *oratio)
  {
    #define df_dt(i,j) (OldFluxes[i+1][j]-OldFluxes[i][j])/((OldTimes[i+1]-OldTimes[i]))
    float *OldFluxes,*OldTimes;
    double NewMass;
    double TotalMass=0.0;
    double ratio;
    double dy1, dy2, eps;
    int OldNum,i,j,k,*Used,Done,Ok,n;
    OldFluxes=*Fluxes; // get copy of old data
    OldTimes=*Times;
    OldNum=(*NumFlux);
    Used=new int[OldNum];
    for (i=0;i<OldNum;i++)
      {
        Used[i]=1;
        if (i==0)
          TotalMass=0.0;
        else
          TotalMass+=0.5*(OldFluxes[i-1]+OldFluxes[i])*(OldTimes[i]-OldTimes[i-1]);
      }
    if (OldNum>0) {
      Used[0]=2;          // keep first point
      Used[OldNum-1]=2;   // keep last point
      if (!(0.0<OldFluxes[0])) {
        for (i=1;i<OldNum;i++) {
          if (0.0<OldFluxes[i]) {
            Used[i-1]=2;
            Used[i]=2;    // keep first non-zero point if the first point is zero
            break;
          }
        }
      }
      if (!(0.0<OldFluxes[OldNum-1])) {
        for (i=OldNum-1;i>=0;i--) {
          if (0.0<OldFluxes[i]) {
            Used[i]=2;  // keep last non-zero point if the last point is zero
            break;
          }
        }
      }
      for (i=1;i<OldNum-1;i++)
        {
        if ((OldFluxes[i]>OldFluxes[i-1] && OldFluxes[i]>OldFluxes[i+1]) ||
            (OldFluxes[i]<OldFluxes[i-1] && OldFluxes[i]<OldFluxes[i+1])) {
          Used[i]=3; // critical point (minimum or maximum)
          }
        else {
          if ((OldFluxes[i]>0.0 || OldFluxes[i]<0.0) &&
             (OldFluxes[i]-OldFluxes[i-1] == OldFluxes[i+1]-OldFluxes[i])) {
              Used[i]=0;  // eliminate consecutive equal values
            }
          }
        }
      }

    n = NumCriticalPoints(OldNum,Used);
    if (n>5001) {
//    MessageBox(NULL, "Initial Critical Points > 5001","Reduce",MB_OK);
      for (i=1;i<OldNum-1;i++)
        {
        if (Used[i]==3) {
          eps = OldFluxes[i]*.0001;
          dy1=fabs(OldFluxes[i]-OldFluxes[i-1]);
          dy2=fabs(OldFluxes[i]-OldFluxes[i+1]);
          if (fabs(dy1-dy2)>eps)
            Used[i]=2; // keep this significant critical point
          else
            Used[i]=1; // restore insignificant critical point
          }
        }
    }
    n = NumCriticalPoints(OldNum,Used);
    if (n>5001)
      // this is a big problem
      MessageBox (NULL,"Final Number of Critical Points > 5001","Reduce",MB_OK);

    // Now loop through removing points until new mass <> 0.0001*total mass
    ratio = *oratio;
    n = NumUsed(OldNum,Used);
    if (5001>=n)
      Done=1;
    else
      Done=0;
    while (!Done)
      {
        Done=1;
        for (k=1;k<OldNum-1;k++) {
          i = rand() % (OldNum+1);
          if (Used[i]==1)
            {
              Used[i]=0;
              Ok=1;
              NewMass=Integrate(OldNum,OldTimes,OldFluxes,Used);
              if (fabs(NewMass-TotalMass)>(ratio*TotalMass))
                Ok=0;
              if (!Ok) Used[i]=1;
              else {
                Done=0;
                n = NumUsed(OldNum,Used);
                if (5001>=n) {
                  Done=1;
                  break;
                }
              }
           }
        }
        if (Done)  {
          n=NumUsed(OldNum,Used);
          if (5001<n)   {
            ratio = ratio * 2.;
            if (ratio>=1.0) {
              ratio = .99;
              MessageBox(NULL,"Significant loss of mass required to reduce curve to 5001 points","Reduce",MB_OK);
              }
            Done=0;
            }
          }
      }
    (*NumFlux)=0;
    for (i=0;i<OldNum;i++)
      if (Used[i]) (*NumFlux)++;
    *Times=new float[*NumFlux];
    *Fluxes=new float[*NumFlux];
    j=0;
    for (i=0;i<OldNum;i++)
      {
      if (Used[i])
        {
          (*Fluxes)[j]=OldFluxes[i];
          (*Times)[j]=OldTimes[i];
          j++;
        }
      }
    *oratio = ratio;
    delete[] OldFluxes;
    delete[] OldTimes;
    delete[] Used;
  }

