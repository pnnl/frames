#include "tseries.h"
#include <ctype.h>

//routine provided to qsort to sort floats
int isequal(const void *a, const void *b)
{
  if (*((float *)a) == *((float *)b))
    return 0;
  if (*((float *)a) < *((float *)b))
    return -1;
  else
    return 1;
}

TimeSeries::TimeSeries()
{
  NumSteps=0;
  TotalTime=0;
  Name[0]=0;
  IDName[0]=0;
  Units[0]=0;
  TimeUnits[0]=0;

  Time=NULL;
  Value=NULL;
}

TimeSeries::~TimeSeries()
{
  if (Time!=NULL)     delete[] Time;
  if (Value!=NULL)    delete[] Value;
}

void TimeSeries::Read(fcsv *TVC)
{
  TVC->read(IDName);
  TVC->read(Name);
  TVC->read(TimeUnits);
  TVC->read(Units);
  TVC->read(&NumSteps);
  Init(NumSteps);
  TVC->readln();
  for (int i=0;i<NumSteps;i++)
  {
    TVC->read(&Time[i]);
    TVC->read(&Value[i]);
    TVC->readln();
  }
  TotalTime = Time[NumSteps-1]-Time[0];
}

void TimeSeries::ReadFreqGID(GIDFILE *f,int lifeidx)
{
  int i;
  paramrec p;

//    rstrcpy(Name,info(f,"LocId",lifeidx));
//    rstrcpy(Name,info(f,"LocName",source+1));
//    rstrcpy(TimeUnits,info(f,"TUnits",source+1));
//    rstrcpy(Units,info(f,"Units",source+1));
  NumSteps=ratoi(info(f,"NumSteps",lifeidx));
  Init(NumSteps);
  p.cnt1=lifeidx;
  p.cnt3=0;
  p.cnt4=0;
  p.cnt5=0;
  p.cnt6=0;
  for (i=0;i<NumSteps;i++)
  {
    rstrcpy(p.name,"Time");
    p.cnt2=i+1;
    if (Get_Value(f,&p)!=NULL)
    {
      Time[i]=ratof(p.valu);
      rstrcpy(TimeUnits,p.punit);
    }
    else Time[i]=0.0;
    rstrcpy(p.name,"Value");
    if (Get_Value(f,&p)!=NULL)
    {
      Value[i]=ratof(p.valu);
      rstrcpy(Units,p.punit);
    }
    else Value[i]=0.0;
  }
  TotalTime= Time[NumSteps-1]-Time[0];
}

void TimeSeries::ReadTWI(GIDFILE *f, element *cas, element *time, element *dose, int org, int chm, int med)
{
  rstrcpy(Name,getvalu(cas,1,org,chm));
  rstrcpy(TimeUnits,time->parent->punit);
  rstrcpy(Units,dose->parent->punit);
  NumSteps=ratoi(info(f,"NumTimePoints",1,org,chm,med));
  Init(NumSteps);
  for (int i=1; i<=NumSteps; i++)
  {
    Time[i-1] = ratof(getvalu(time,1,org,chm,med,i));
    Value[i-1] = ratof(getvalu(dose,1,org,chm,med,i));
  }
}

void TimeSeries::ReadGID(GIDFILE *f,int source, int lifeidx)
{
  int i;
  paramrec p;

  rstrcpy(Name,info(f,"LocId",lifeidx,source+1));
//    rstrcpy(Name,info(f,"LocName",source+1));
//    rstrcpy(TimeUnits,info(f,"TUnits",source+1));
//    rstrcpy(Units,info(f,"Units",source+1));
  NumSteps=ratoi(info(f,"NumSteps",lifeidx,source+1));
  Init(NumSteps);
  p.cnt1=lifeidx;
  p.cnt2=source+1;
  p.cnt4=0;
  p.cnt5=0;
  p.cnt6=0;
  for (i=0;i<NumSteps;i++)
  {
    rstrcpy(p.name,"Time");
    p.cnt3=i+1;
    if (Get_Value(f,&p)!=NULL)
    {
      Time[i]=ratof(p.valu);
      rstrcpy(TimeUnits,p.punit);
    }
    else Time[i]=0.0;
    rstrcpy(p.name,"Value");
    if (Get_Value(f,&p)!=NULL)
    {
      Value[i]=ratof(p.valu);
      rstrcpy(Units,p.punit);
    }
    else Value[i]=0.0;
  }
  TotalTime= Time[NumSteps-1]-Time[0];
}

float TimeSeries::InterpTime(float valu,int count)
{
  float f,NewTime;
  f = Value[count+1]-Value[count];
  if (fabs(f) > 0)
  {
    f = (valu-Value[count]) / f;
    NewTime = Time[count] + f * (Time[count+1]-Time[count]);
    return NewTime;
  }
  return Time[count];
}

float TimeSeries::GetIntervalDuration(int t, float valu)
{
  if (Time[t] == Time[t+1])                               // vertical segment
    return 0.0;
  if (Value[t] < valu && valu > Value[t+1])               // above segment
    return 0.0;
  if (Value[t] >= valu && valu <= Value[t+1])             // below segment
    return Time[t+1] - Time[t];
  if (Value[t] <= valu && valu <= Value[t+1])             // in ascending segment
    return Time[t+1] - InterpTime(valu,t);
  if (Value[t] >= valu && valu >= Value[t+1])             // in descending segment
    return InterpTime(valu,t) - Time[t];
  return 0.0;
}

float TimeSeries::GetTotDuration(float valu)
{
  float duration = 0.0;

  for (int t=0; t<NumSteps-1; t++)
    duration += GetIntervalDuration(t,valu);
  return duration;
}

void TimeSeries::Init(int numsteps)
{
  try
  {
    NumSteps=numsteps;
    if (Time != NULL) delete[] Time;
    if (Value != NULL) delete[] Value;
    Time=new float[NumSteps];
    Value=new float[NumSteps];
  }
  catch(...)
  {
//    Error("Out of Memory in Timeseries::Init");
     printf("Out of Memory in Timeseries::Init");
     NumSteps=0;
     Time=NULL;
     Value=NULL;
     return;
  }
  for (int i=0;i<NumSteps;i++)
  {
    Time[i]=0.0;
    Value[i]=0.0;
  }
}

void TimeSeries::InsertTimes(TimeSeries *T)
{
  int i,j,add;
  float v;

  i=0;
  while (T->Time[i] < Time[0] && i < T->NumSteps) i++;
  if (i)
  {
    Insert(-1,Time[0]+(Time[0]-T->Time[i-1])/10000.0,0.0);
    Insert(-1,T->Time[0],0.0);
  }

  i=1;
  while (T->Time[T->NumSteps-i] > Time[NumSteps-1] && i >= 0) i++;
  if (i > 1)
  {
    Insert(NumSteps,Time[NumSteps-1]+(T->Time[T->NumSteps-i+1]-Time[NumSteps-1])/10000.0,0.0);
    Insert(NumSteps,T->Time[T->NumSteps-1],0.0);
  }

  for (i=0; i<T->NumSteps; i++)
  {
    add = 1;
    for (j=0; j<NumSteps; j++)
      if (fabs(T->Time[i]-Time[j]) <= 1.0E-6*fabs(Time[j]))
      {
        add = 0;
        break;
      }
    if (add)
    {
      j = 0;
      while (T->Time[i] > Time[j] && j < NumSteps) j++;
      if (fabs(Value[j-1] - Value[j]) < 1.0E-4*fabs(Value[j]))
        v = Value[j];
      else
        v = Value[j-1]+((T->Time[i]-Time[j-1])/(Time[j]-Time[j-1]))*(Value[j]-Value[j-1]);
      Insert(j-1,T->Time[i],v);
    }
  }
}

float TimeSeries::DiscreteInterp(float t)
{
  int i=0;
  if (t<Time[0]) return 0.0;
  if (t>Time[NumSteps-1]) return 0.0;
  while (t>Time[i+1] && i<NumSteps) i++;
  return Value[i];
}

float TimeSeries::ContinInterp(float t)
{
  int i=0;
  if (t<Time[0]) return 0.0;
  if (t>Time[NumSteps-1]) return 0.0;
  while (t>Time[i+1] && i<NumSteps) i++;
  return Value[i]+((t-Time[i])/(Time[i+1]-Time[i]))*(Value[i+1]-Value[i]);
}

void TimeSeries::Break(float conc)
{
  int broke;
  float t;

  do {
    broke = 0;
    for (int j=0;j<NumSteps-1;j++)
      if ((Value[j]<conc && conc<Value[j+1]) ||
          (Value[j]>conc && conc>Value[j+1]))
      {
        t=InterpTime(conc,j);
        Insert(j,t,conc);
        broke = 1;
        break;
      }
  } while (broke);
}

void TimeSeries::Adjust(TimeSeries *T, int locidx)                  //
{
  int i,j;

  InsertTimes(T);
  for (i=0;i<T->NumSteps;i++)
  {
    j = 0;
    while ((T->Time[i] >= Time[j]) && j < NumSteps) j++;
    while ((T->Time[i+1] >= Time[j]) && j < NumSteps)
    {
// new replaces commented line
      if (fabs(locidx - T->Value[i]) > 0)
        Value[j] = 0;
//      Value[j] *= T->Value[i];
      j++;
    }
  }
}

void TimeSeries::Mult(TimeSeries *T)
{
  for (int i=0;i<NumSteps;i++)
    Value[i]*=T->DiscreteInterp(Time[i]);
}

void TimeSeries::AddIn(TimeSeries *T)
{
  if (Value == NULL)
    Copy(T);
  else
    for (int i=0;i<NumSteps;i++)
      Value[i]+=T->ContinInterp(Time[i]);
}

float TimeSeries::GetMinValue()
{
  float value = MAXFLOAT;
  for (int i=0; i<NumSteps; i++)
    if (value > Value[i])
      value = Value[i];
  return value;
}

float TimeSeries::GetMaxValue()
{
  float value = -MINFLOAT;
  for (int i=0; i<NumSteps; i++)
    if (value < Value[i])
      value = Value[i];
  return value;
}

void TimeSeries::Insert(int count,float t,float v)
{
  int i,oi;
  float *TempTime,*TempValue;

  try
  {
    TempTime=new float[NumSteps+1];
    TempValue=new float[NumSteps+1];
  }
  catch(...)
  {
    printf("Out of Memory in TimeSeries::Insert");
    NumSteps=0;
    return;
  }
  oi=0;
  for (i=0;i<NumSteps;i++)
  {
    if (i==count+1)
    {
      TempTime[i]=t;
      TempValue[i]=v;
      oi=1;
    }
    TempTime[i+oi]=Time[i];
    TempValue[i+oi]=Value[i];
  }
  if (count==NumSteps)
  {
    TempTime[count]=t;
    TempValue[count]=v;
  }
  delete[] Time;
  delete[] Value;
  Time=TempTime;
  Value=TempValue;
  TotalTime= Time[NumSteps]-Time[0];
  NumSteps++;
}

void TimeSeries::Copy(TimeSeries *T)
{
  Init(T->NumSteps);
  for (int i=0;i<NumSteps;i++)
  {
    Time[i]=T->Time[i];
    Value[i]=T->Value[i];
  }
  TotalTime=T->TotalTime;
  rstrcpy(IDName,T->IDName);
  rstrcpy(Name,T->Name);
  rstrcpy(TimeUnits,T->TimeUnits);
  rstrcpy(Units,T->Units);
}

void TimeSeries::CopyTo(TimeSeries *T)
{
  T->Init(NumSteps);
  for (int i=0;i<NumSteps;i++)
  {
    T->Time[i]=Time[i];
    T->Value[i]=Value[i];
  }
  T->TotalTime = TotalTime;
  rstrcpy(T->IDName,IDName);
  rstrcpy(T->Name,Name);
  rstrcpy(T->TimeUnits,TimeUnits);
  rstrcpy(T->Units,Units);
}

void TimeSeries::Div(float scalor)
{
  for (int i=0;i<NumSteps;i++)
    Value[i]=Value[i]/scalor;
}

void TimeSeries::Rankorder()
{
  qsort(Value,NumSteps,sizeof(float),isequal);
  for (int i=0;i<NumSteps;i++)
    Time[i] = ((float)(NumSteps-i)) / ((float)NumSteps + 1.0) * 100;
}

void TimeSeries::WriteSeries(fcsv *fle)
{
  fle->write(NumSteps);
  fle->writeln();
  for (int i=0;i<NumSteps;i++)
  {
    fle->write(Value[i],"%G");
    fle->write(Time[i],"%G");
    fle->writeln();
  }
}
void TimeSeries::PrintSeries()
{
  printf("%d\n",NumSteps);
  for (int i=0;i<NumSteps;i++)
    printf("%G , %G\n", Value[i],Time[i]);
}


