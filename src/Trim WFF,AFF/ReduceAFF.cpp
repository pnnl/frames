#include <math.h>



int NextUsed(int Start,int Max,int *Used)
{
  int i;
  for (i=Start;i<Max && !Used[i];i++);
  return i;
}

float Integrate(int Num,int Index,float *Times,float **Flux,int *Used)
{
  int i,nexti;
  float NewMass;
  NewMass=0.0;
  i=NextUsed(0,Num,Used);
  while (i<Num)
    {
      nexti=NextUsed(i+1,Num,Used);
      if (nexti<Num)
        NewMass+=0.5*(Flux[i][Index]+Flux[nexti][Index])*(Times[nexti]-Times[i]);
      i=nexti;
    }
  return NewMass;
}

void Reduce(int NumType,int *NumFlux,float **Times,float ***Fluxes,float ratio)
{
  #define df_dt(i,j) (OldFluxes[i+1][j]-OldFluxes[i][j])/((OldTimes[i+1]-OldTimes[i]))
  float **OldFluxes,*OldTimes;
  float *TotalMass;
  float *NewMass;
  int OldNum,i,j,k,*Used,Done,Ok;
  OldFluxes=(*Fluxes); // get copy of old data
  OldTimes=(*Times);
  OldNum=(*NumFlux);
  Used=new int[OldNum];
  TotalMass=new float[NumType];
  NewMass=new float[NumType];
  for (i=0;i<OldNum;i++)
    {
      Used[i]=1;
      for (j=0;j<NumType;j++)
        if (i==0)
          TotalMass[j]=0.0;
        else
          TotalMass[j]+=0.5*(OldFluxes[i-1][j]+OldFluxes[i][j])*(OldTimes[i]-OldTimes[i-1]);
    }
  // Now loop through removing a points until new mass <> 0.0001*total mass
  Done=0;
  while (!Done)
    {
      Done=1;
      for (i=1;i<OldNum-1;i++)
        if (Used[i]==1)
          {
            Used[i]=0;
            Ok=1;
            for (j=0;j<NumType;j++)
              {
                NewMass[j]=Integrate(OldNum,j,OldTimes,OldFluxes,Used);
                if (fabs(NewMass[j]-TotalMass[j])>(ratio*TotalMass[j]))
                  Ok=0;
                NewMass[j]=Integrate(OldNum,j,OldTimes,OldFluxes,Used);
              }
            if (!Ok) Used[i]=1;
            else Done=0;
         }
    }
  (*NumFlux)=0;
  for (i=0;i<OldNum;i++)
    if (Used[i]) (*NumFlux)++;
  *Times=new float[*NumFlux];
  *Fluxes=new float*[*NumFlux];
  j=0;
  for (i=0;i<OldNum;i++)
    {
      if (Used[i])
        {
          (*Fluxes)[j]=new float[NumType];
          for (k=0;k<NumType;k++)
            (*Fluxes)[j][k]=OldFluxes[i][k];
          (*Times)[j]=OldTimes[i];
          j++;
        }
    }
  for (i=0;i<OldNum;i++)
    delete[] OldFluxes[i];
  delete[] OldFluxes;
  delete[] OldTimes;
  delete[] Used;
  delete[] TotalMass;
}

/*
void Sort(int Count,int *Map,float *Values)
{
  register int i,j,t;
  for (i=0;i<Count;i++)  // simple bubble sort on map values
    for (j=i+1;j<Count;j++)
      if (Values[Map[i]]<Values[Map[j]])
        {
          t=Map[i];
          Map[i]=Map[j];
          Map[j]=t;
        }
}

void Reduce(int NumType,int *NumFlux,float **Times,float ***Fluxes)
{
  #define df_dt(i,j) (OldFluxes[i+1][j]-OldFluxes[i][j])/((OldTimes[i+1]-OldTimes[i]))
  float **OldFluxes,*OldTimes;
  float *Deriv,*TotalMass;
  float NewMass;
  int OldNum,i,nexti,j,*Map,*Used,Done,NewCount;
  OldFluxes=(*Fluxes); // get copy of old data
  OldTimes=(*Times);
  OldNum=(*NumFlux);
  Deriv=new float[OldNum];
  Map=new int[OldNum];
  Used=new int[OldNum];
  TotalMass=new float[NumType];
  for (i=0;i<OldNum;i++)  // calculate absolute values of the total second derivatives
    {
      Deriv[i]=0.0;
      Map[i]=i;
      Used[i]=0;
      for (j=0;j<NumType;j++)
        {
          if (i==0)
            Deriv[i]+=fabs(df_dt(i,j));// 1 Foward Differences
          else if (i==OldNum-1)
            Deriv[i]+=fabs(df_dt(i-1,j)); // 1 Backward Differences
          else
            Deriv[i]+=0.5*fabs(df_dt(i,j)+df_dt(i-1,j)); // average of Backward and Forward
          if (i==0)
            TotalMass[j]=0.0;
          else
            TotalMass[j]+=0.5*(OldFluxes[i-1][j]+OldFluxes[i][j])*(OldTimes[i]-OldTimes[i-1]);
        }
    }
  Sort(OldNum,Map,Deriv); // sort second derivatives in order
  // Mark first and last times as used
  Used[0]=1; Used[OldNum-1]=1;
  // Now loop through adding a new derivative until total mass for all rates
  // is 99.9% of the original mass
  Done=0;
  for (NewCount=0;NewCount<OldNum && !Done;NewCount++)
    {
      if (Used[Map[NewCount]]==0)
        {
          Used[Map[NewCount]]=1; // Turn on Next Highest derivative
          Done=1; // Assume that we are done
          for (j=0;j<NumType && Done;j++)
            {
              NewMass=0.0;
              i=NextUsed(0,OldNum,Used);
              while (i<OldNum)
                {
                  nexti=NextUsed(i+1,OldNum,Used);
                  if (nexti<OldNum)
                    NewMass+=0.5*(OldFluxes[i][j]+OldFluxes[nexti][j])*(OldTimes[nexti]-OldTimes[i]);
                  i=nexti;
                }
              if (fabs(NewMass-TotalMass[j])>(0.0001*TotalMass[j])) Done=0; // cannot be done yet
            }
        }
    }
  (*NumFlux)=0;
  for (i=0;i<OldNum;i++)
    if (Used[i]) (*NumFlux)++;
  *Times=new float[*NumFlux];
  *Fluxes=new float*[*NumFlux];
  nexti=NextUsed(0,OldNum,Used);
  for (i=0;i<*NumFlux;i++)
    {
      (*Fluxes)[i]=new float[NumType];
      for (j=0;j<NumType;j++)
        (*Fluxes)[i][j]=OldFluxes[nexti][j];
      (*Times)[i]=OldTimes[nexti];
      nexti=NextUsed(nexti+1,OldNum,Used);
    }
  for (i=0;i<OldNum;i++)
    delete[] OldFluxes[i];
  delete[] OldFluxes;
  delete[] OldTimes;
  delete[] Deriv;
  delete[] Map;
  delete[] Used;
  delete[] TotalMass;
}
*/
