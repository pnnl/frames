/*______________________________________________________________________________

  Date:       1993 - 2004
  Company:    Pacific Northwest National Laboratories
  Battelle Corporation
  ________________________________________________________________________________
  __Modifiication  History________________________________________________________

    DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#include "frames.h"
#include "series.h"

//------------------------------------------------------------------------------
//routine provided to qsort to sort doubles
int isequal(const void *a, const void *b)
{
  if (*((double *)a) == *((double *)b))
    return 0;
  if (*((double *)a) < *((double *)b))
    return -1;
  else
    return 1;
}
//------------------------------------------------------------------------------
// works really well, but ...
// absolutly no checking done here other than not to exceed given nums
// NumRes is set to the proper length when finished
void addSeries(int Num1, double *Times1, double *Values1,
               int Num2, double *Times2, double *Values2,
               int *NumRes, double *TimesRes, double *ValuesRes)
{
  int i,j,c;
  i=0; j=0; c=0;
  while (i<Num1 && j<Num2)
    if (Times1[i]<Times2[j])
    {
      // bump times1 to next interval
      TimesRes[c]=Times1[i];
      ValuesRes[c]=Values1[i];
      if (j>0)
        ValuesRes[c]+=interpolate(Times2[j-1], Values2[j-1],
                                  Times2[j], Values2[j], TimesRes[c]);
      i++;
      c++;
    }
    else if (Times2[j]<Times1[i])
    {
      // bump times2 to next interval
      TimesRes[c]=Times2[j];
      ValuesRes[c]=Values2[j];
      if (i>0)
        ValuesRes[c]+=interpolate(Times1[i-1], Values1[i-1],
                                  Times1[i], Values1[i], TimesRes[c]);
      j++;
      c++;
    }
    else  // the time steps are equal
    { // bump both to next time interval
      TimesRes[c]=Times2[j];
      ValuesRes[c]=Values2[j]+Values1[i];
      i++;
      j++;
      c++;
    }
  // add on rest of first series if any is left
  if (i<Num1)
    for (;i<Num1;i++)
    {
      TimesRes[c] =Times1[i];
      ValuesRes[c]=Values1[i];
      c++;
    }
  // add on rest of second series if any is left
  if (j<Num2)
    for (;j<Num2;j++)
    {
      TimesRes[c]=Times2[j];
      ValuesRes[c]=Values2[j];
      c++;
    }
  (*NumRes)=c;
}
//------------------------------------------------------------------------------
Series::~Series()
{
  if (xValues!=NULL) delete[] xValues; xValues=NULL;
  if (yValues!=NULL) delete[] yValues; xValues=NULL;
}
//------------------------------------------------------------------------------
Series::Series()
{
  Id[0]=0;
  Name[0]=0;
  xUnits[0]=0;
  yUnits[0]=0;
  count=0;
  xInterval=0;
  yInterval=0;
  whole=0;
  decimal=0;
  exponent=0;
  xValues=NULL;
  yValues=NULL;
}
//------------------------------------------------------------------------------
Series::Series(int n)
{
  Id[0]=0;
  Name[0]=0;
  yUnits[0]=0;
  xUnits[0]=0;
  count=0;
  xInterval=0;
  yInterval=0;
  whole=0;
  decimal=0;
  exponent=0;
  xValues=NULL;
  yValues=NULL;
  Init(n);
}
//------------------------------------------------------------------------------
void Series::Init(int n)
{
  if (n==0) return;
  try
  {
    count=n;
    if (xValues != NULL) delete[] xValues;
    if (yValues != NULL) delete[] yValues;
    xValues=new double[count];
    yValues=new double[count];
  }
  catch(...)
  {
    printf("Out of Memory in Series::Init");
    count=0;
    xValues=NULL;
    yValues=NULL;
    return;
  }
  for (int i=0;i<count;i++)
  {
    xValues[i]=0.0;
    yValues[i]=0.0;
  }
}
//------------------------------------------------------------------------------
void Series::Init(int n, double *Xvals, double *Yvals)
{
  Init(n);
  for (int i=0;i<count;i++)
  {
    xValues[i]=Xvals[i];
    yValues[i]=Yvals[i];
  }
}
//------------------------------------------------------------------------------
// next three function copy to, from and clone
void Series::Copy(Series *T)
{
  Init(T->count);
  for (int i=0;i<count;i++)
  {
    xValues[i]=T->xValues[i];
    yValues[i]=T->yValues[i];
  }
  whole = T->whole;
  decimal = T->decimal;
  exponent = T->exponent;
  xInterval = T->xInterval;
  rstrcpy(Id,T->Id);
  rstrcpy(Name,T->Name);
  rstrcpy(xUnits,T->xUnits);
  rstrcpy(yUnits,T->yUnits);
}
//------------------------------------------------------------------------------
void Series::CopyTo(Series *T)
{
  T->Init(count);
  for (int i=0;i<count;i++)
  {
    T->xValues[i]=xValues[i];
    T->yValues[i]=yValues[i];
  }
  T->whole = whole;
  T->decimal = decimal;
  T->exponent = exponent;
  T->xInterval = xInterval;
  rstrcpy(T->Id,Id);
  rstrcpy(T->Name,Name);
  rstrcpy(T->xUnits,xUnits);
  rstrcpy(T->yUnits,yUnits);
}
//------------------------------------------------------------------------------
Series* Series::Clone()
{
  Series *clone = new Series(count);
  CopyTo(clone);
  return clone;
}
//------------------------------------------------------------------------------
// read write functions for specific file types
//
void Series::Read(icsv *fle, int n, int valueIndex)
{
  Init(n);
  for (int i=0;i<count;i++)
  {
    *fle >> xValues[i];
    for(int j = 0; j < valueIndex; j++)
      *fle >> yValues[i];
    *fle >> NewLn;
  }
  if (count) xInterval = xValues[count-1]-xValues[0];
}
//------------------------------------------------------------------------------
// read write functions for specific file types
//
void Series::Read(fcsv *fle, int n, int valueIndex)
{
  Init(n);
  for (int i=0;i<count;i++)
  {
    fle->read(&xValues[i]);
    for(int j = 0; j < valueIndex; j++)
      fle->read(&yValues[i]);
    fle->readln();
  }
  if (count) xInterval = xValues[count-1]-xValues[0];
}
//------------------------------------------------------------------------------
void Series::Read(fcsv *fle, int n)
{
  Init(n);
  for (int i=0;i<count;i++)
  {
    fle->read(&xValues[i]);
    fle->read(&yValues[i]);
    fle->readln();
  }
  if (count) xInterval = xValues[count-1]-xValues[0];
}
//------------------------------------------------------------------------------
void Series::ReadSeries(fcsv *fle)
{
  fle->read(&count);
  fle->read(xUnits);
  fle->read(yUnits);
  fle->readln();
  Init(count);
  for (int i=0;i<count;i++)
  {
    fle->read(&xValues[i]);
    fle->read(&yValues[i]);
    fle->readln();
  }
  if (count) xInterval = xValues[count-1]-xValues[0];
}
//------------------------------------------------------------------------------
void Series::Read(fcsv *fle)  //from timeseries
{
  fle->read(Id);
  fle->read(Name);
  fle->read(xUnits);
  fle->read(yUnits);
  fle->read(&count);
  fle->readln();
  Init(count);
  for (int i=0;i<count;i++)
  {
    fle->read(&xValues[i]);
    fle->read(&yValues[i]);
    fle->readln();
  }
  if (count) xInterval = xValues[count-1]-xValues[0];
}
//------------------------------------------------------------------------------
void Series::ReadTWI(GIDFILE *f, element *cas, element *time, element *dose, int org, int chm, int med)
{
  rstrcpy(Name,getvalu(cas,1,org,chm));
  rstrcpy(xUnits,time->parent->punit);
  rstrcpy(yUnits,dose->parent->punit);
  count=ratoi(info(f,"NumTimePoints",1,org,chm,med));
  Init(count);
  for (int i=1; i<=count; i++)
  {
    xValues[i-1] = ratof(getvalu(time,1,org,chm,med,i));
    yValues[i-1] = ratof(getvalu(dose,1,org,chm,med,i));
  }
  if (count) xInterval = xValues[count-1]-xValues[0];
}
//------------------------------------------------------------------------------
void Series::ReadFreqGID(GIDFILE *f,int lifeidx)
{
  int i;
  paramrec p;

  count=ratoi(info(f,"NumSteps",lifeidx));
  Init(count);
  p.cnt1=lifeidx;
  p.cnt3=0;
  p.cnt4=0;
  p.cnt5=0;
  p.cnt6=0;
  for (i=0;i<count;i++)
  {
    rstrcpy(p.name,"Time");
    p.cnt2=i+1;
    if (Get_Value(f,&p)!=NULL)
    {
      xValues[i]=ratof(p.valu);
      if (xUnits[0]==0) rstrcpy(xUnits,p.punit);
    }
    else xValues[i]=0.0;
    rstrcpy(p.name,"Value");
    if (Get_Value(f,&p)!=NULL)
    {
      yValues[i]=ratof(p.valu);
      if (yUnits[0]==0) rstrcpy(yUnits,p.punit);
    }
    else yValues[i]=0.0;
  }
  if (count) xInterval = xValues[count-1]-xValues[0];
}
//------------------------------------------------------------------------------
void Series::ReadGID(GIDFILE *f,int source, int lifeidx)
{
  int i;
  paramrec p;

  rstrcpy(Name,info(f,"LocId",lifeidx,source+1));
  count=ratoi(info(f,"NumSteps",lifeidx,source+1));
  Init(count);
  p.cnt1=lifeidx;
  p.cnt2=source+1;
  p.cnt4=0;
  p.cnt5=0;
  p.cnt6=0;
  for (i=0;i<count;i++)
  {
    rstrcpy(p.name,"Time");
    p.cnt3=i+1;
    if (Get_Value(f,&p)!=NULL)
    {
      xValues[i]=ratof(p.valu);
      if (xUnits[0]==0) rstrcpy(xUnits,p.punit);
    }
    else xValues[i]=0.0;
    rstrcpy(p.name,"Value");
    if (Get_Value(f,&p)!=NULL)
    {
      yValues[i]=ratof(p.valu);
      if (yUnits[0]==0) rstrcpy(yUnits,p.punit);
    }
    else yValues[i]=0.0;
  }
  if (count) xInterval = xValues[count-1]-xValues[0];
}
//------------------------------------------------------------------------------
void Series::WriteSeries(fcsv *fle)
{
  fle->write(count);
  fle->write(xUnits);
  fle->write(yUnits);
  fle->writeln();
  for (int i=0;i<count;i++)
  {
    fle->write(xValues[i]);
    fle->write(yValues[i]);
    fle->writeln();
  }
}
//------------------------------------------------------------------------------
void Series::PrintSeries()
{
  printf("%d\n",count);
  for (int i=0;i<count;i++)
    printf("%G , %G\n", yValues[i],xValues[i]);
}
//------------------------------------------------------------------------------
// returns 1 or true if
//     Xvalues are not in acending order,
//     no repeats on Xvalues
//     less than one value
int Series::XNotLinear()
{
  if (count==1)
  { writeError("Series::TestX - 2 Values are required in xValues series");
    return 1;                   // pair is not a series
  }
  for (int i=1; i<count; i++)
    if (xValues[i-1]>xValues[i])
    { writeError("Series::TestX - xValues out of order in xValues series");
      return 1;
    }
    else if (isEqual(xValues[i-1],xValues[i]))
    { writeError("Series::TestX - Two xValues are equal in xValues series");
      return 1;
    }
  return 0;
}
//------------------------------------------------------------------------------
// returns 1 or true if
//     Yvalues are not in acending order,
//     no repeats on Yvalues
//     less than one value
int Series::YNotLinear()
{
  if (count==1)
  { writeError("Series::TestY - 2 Values are required in yValues series");
    return 1;                   // pair is not a series
  }
  for (int i=1; i<count; i++)
    if (yValues[i-1]>yValues[i])
    { writeError("Series::TestY - yValues out of order in yValues series");
      return 1;
    }
    else if (isEqual(yValues[i-1],yValues[i]))
    { writeError("Series::TestY - Two yValues are equal in yValues series");
      return 1;
    }
  return 0;
}
//------------------------------------------------------------------------------
// Min and Max functions
double Series::MaxY(int begIdx, int endIdx)
{
  int i,t;

  if (count < endIdx || count-1 < begIdx || begIdx > endIdx || begIdx < 0)
    return mMAXDOUBLE;
  t=begIdx;
  for (i=begIdx+1; i<=endIdx; i++)
    if (yValues[i]>yValues[t])
      t = i;
  return yValues[t];
}
//------------------------------------------------------------------------------
double Series::MinY(int begIdx, int endIdx)
{
  int i,t;

  if (count < endIdx || count-1 < begIdx || begIdx > endIdx || begIdx < 0)
    return mMINDOUBLE;
  t=begIdx;
  for (i=begIdx+1; i<=endIdx; i++)
    if (yValues[i]<yValues[t])
      t = i;
  return yValues[t];
}
//------------------------------------------------------------------------------
double Series::MinX()
{
  double x,y=mMINDOUBLE;
  MinXwithY(&x,&y);
  return x;
}
//------------------------------------------------------------------------------
double Series::MaxX()
{
  double x,y=mMAXDOUBLE;
  MaxXwithY(&x,&y);
  return x;
}
//------------------------------------------------------------------------------
double Series::MinY()
{
  double x,y=mMINDOUBLE;
  MinYwithX(&x,&y);
  return y;
}
//------------------------------------------------------------------------------
double Series::MaxY()
{
  double x,y=mMAXDOUBLE;
  MaxYwithX(&x,&y);
  return y;
}
//------------------------------------------------------------------------------
void Series::MinXwithY(double *x, double *y)
{
  int i,t;

  if (count==0) return;
  t=0;
  for (i=1; i<count; i++)
    if (xValues[i]<xValues[t])
      t=i;
  *x=xValues[t];
  *y=yValues[t];
}
//------------------------------------------------------------------------------
void Series::MaxXwithY(double *x, double *y)
{
  int i,t;

  if (count==0) return;
  t=0;
  for (i=1;i<count;i++)
    if (xValues[i]>xValues[t])
      t=i;
  *x=xValues[t];
  *y=yValues[t];
}
//------------------------------------------------------------------------------
void Series::MinYwithX(double *x, double *y)
{
  int i,t;

  if (count==0) return;
  t=0;
  for (i=1; i<count; i++)
    if (yValues[i]<yValues[t])
      t=i;
  *x=xValues[t];
  *y=yValues[t];
}
//------------------------------------------------------------------------------
void Series::MaxYwithX(double *x, double *y)
{
  int i,t;

  if (count==0) return;
  t=0;
  for (i=1;i<count;i++)
    if (yValues[i]>yValues[t])
      t=i;
  *x=xValues[t];
  *y=yValues[t];
}
//------------------------------------------------------------------------------
// find the interval index over which x occurs ie xVaules[i] <= x < xVaules[i+1]
int Series::Find(double x)
{
  int i;
  if (count==0) return -1;
  if (x<xValues[0]) return -1;
  if (x>xValues[count-1]) return -1;
  for (i=1;i<count;i++)
    if (xValues[i-1]<=x && x<xValues[i])
      return i-1;
  return count-1;
}
//------------------------------------------------------------------------------
// interpolation functions
double Series::DiscreteInterpY(double x)
{
  int i = Find(x);
  if (i==-1) return 0.0;
  return yValues[i];
}
//------------------------------------------------------------------------------
double Series::InterpY(double x) //ValueAt
{
  int i = Find(x);
  if (i==-1) return 0.0;
  return interpolate(xValues[i],yValues[i],xValues[i+1],yValues[i+1],x);
}
//------------------------------------------------------------------------------
// NOTE: This routine assumes that y lies between the interval
// yValues[idx] - yValues[idx+1], used by InsertXpts, ExceedDelta only
// which ensures the assumption, though we check for an interval here
double Series::InterpX(double y, int idx)   //InterpTime
{
  double interval;

  if (count==0) return 0.0;
  interval = yValues[idx+1]-yValues[idx];
  if (fabs(interval) > 0.0)
    return xValues[idx] + (y-yValues[idx]) * (xValues[idx+1]-xValues[idx]) / interval;
  else
    return xValues[idx];
}

//------------------------------------------------------------------------------
// important note!!
//   these insert routines do not check for continuity on either axis
void Series::Insert(int idx, double x, double y)
{
  int i,oi;
  double *xTemp = NULL;
  double *yTemp = NULL;

  try
  {
    xTemp=new double[count+1];
    yTemp=new double[count+1];
  }
  catch(...)
  {
    printf("Series::Insert - Out of Memory in Series::Insert");
    count=0;
    if (xTemp) delete xTemp;
    if (yTemp) delete yTemp;
    xTemp=NULL;
    yTemp=NULL;
    return;
  }
  oi=0;
  for (i=0;i<count;i++)
  {
    if (i==idx+1)
    {
      xTemp[i]=x;
      yTemp[i]=y;
      oi=1;
    }
    xTemp[i+oi]=xValues[i];
    yTemp[i+oi]=yValues[i];
  }
  if (count<=idx)
  {
    xTemp[count]=x;
    yTemp[count]=y;
  }
  if (xValues) delete xValues;
  if (xValues) delete yValues;
  xValues=xTemp;
  yValues=yTemp;
  count++;
  xInterval = MaxX()-MinX();
  yInterval = MaxY()-MinY();
}
//------------------------------------------------------------------------------
void Series::AppendEntry(double x, double y)  //addentry used by sync only
{
  Insert(count, x, y);
}
//------------------------------------------------------------------------------
void Series::PrependEntry(double x, double y)
{
  Insert(-1, x, y);
}
//------------------------------------------------------------------------------
// use init and then load with this routine
void Series::Replace(int n, double x, double y)   //was add
{
  xValues[n]=x;
  yValues[n]=y;
}
//------------------------------------------------------------------------------
// inserts y=0 points for each end
void Series::InsertEnds(Series *T)
{
  if (yValues[0]!=0.0 && xValues[0] > 0.0  && T->xValues[0] < xValues[0])
//    Insert(-1, xValues[0]*(1.0-_eps), 0.0);    //backup a little
    Insert(-1, xValues[0] - _eps, 0.0);    //backup a little

  if (yValues[count-1]!=0.0 && T->xValues[T->count-1] > xValues[count-1])
//    Insert(count, xValues[count-1]*(1.0+_eps), 0.0);  //forward a little
    Insert(count, xValues[count-1] + _eps, 0.0);  //forward a little
}
//------------------------------------------------------------------------------
// inserts points for each rise or fall of the curve passing over the given y
void Series::InsertXpts(double y)  //Break
{
  int j, notDone;
  double t;

  j=0;
  do {
    notDone = 0;
    for (;j<count-1;j++)
      if ((yValues[j]<y && y<yValues[j+1]) ||
          (yValues[j]>y && y>yValues[j+1]))
      {
        t=InterpX(y,j);
        Insert(j,t,y);
        notDone = 1;
        break;            //break for because count changes, j need not be reset
      }
  } while (notDone);
}

//------------------------------------------------------------------------------
// insert x axis yValues from T series into this series by linear interpolating y
// an assumption is made that both series are continuous along the x axis
// A value of zero for y is given if T.xValues[i] not in range of xValues, also
// inserts a zero y value just before and after this series if x overlap occurs
int Series::InsertXpts(Series *T)  //InsertTimes
{
  int i,j,k;
  double y;

  if(T->count>1)
    if(T->XNotLinear()) return 0;
  if(count>1)
    if(XNotLinear()) return 0;
  //find and set tips
  i=0;
  while (T->xValues[i] < xValues[0] && i < T->count) i++;
  if (i)
  {
    // insert 0 just before begX of this series
//    Insert(-1,xValues[0]*(1.0-_eps),0.0);
    Insert(-1,xValues[0] - _eps,0.0);
    // insert 0 using begX of other series
    Insert(-1,T->xValues[0],0.0);
  }
  //find and set tails
  i=1;
  while (T->xValues[T->count-i] > xValues[count-1] && i <= T->count) i++;
  if (i > 1)
  {
    // insert 0 just after endX of this series
//    Insert(count,xValues[count-1]*(1.0+_eps),0.0);
    Insert(count,xValues[count-1] + _eps,0.0);
    // insert 0 using endX of other series
    Insert(count,T->xValues[T->count-1],0.0);
  }
  //insert rest of the points
  for (i=0; i<T->count; i++)
  {
    j=0;
    for (k=0; k<count; k++)
    {
      if (isEqual(T->xValues[i], xValues[k]))        // check for duplicate
        break;
      if (T->xValues[i] > xValues[k])         // set insert idx as well
        j++;
    }
    if (k==count)                             // no duplicate, add x
    {
      if (isEqual(yValues[j-1], yValues[j]))
        y = yValues[j];
      else
        y = yValues[j-1]+((T->xValues[i]-xValues[j-1])/(xValues[j]-xValues[j-1]))*(yValues[j]-yValues[j-1]);
      Insert(j-1,T->xValues[i],y);
    }
  }
  return 1;
}
//------------------------------------------------------------------------------
// x equal or exceed over x range routines, used to help calculate
// the actual probaility of exceedence of the y axis yValues
double Series::ExceedDelta(double y, int idx)      //GetIntervalDuration int double
{
  if (idx > count-2 || idx < 0)                            // no segments
    return 0.0;
  if (isEqual(xValues[idx],xValues[idx+1]))                // vertical segment
    return 0.0;
  if (yValues[idx] >= y && yValues[idx+1] >= y)            // segment above y
    return xValues[idx+1] - xValues[idx];
  if (yValues[idx] < y && yValues[idx+1] < y)              // segment below y
    return 0.0;

  if (yValues[idx] <= y && y < yValues[idx+1])             // y intersects segment
      return xValues[idx+1] - InterpX(y,idx);              // in ascending segment

  if (yValues[idx] > y && y >= yValues[idx+1])             // y intersects segment
      return InterpX(y,idx) - xValues[idx];                // in descending segment
  return 0.0;
}
//------------------------------------------------------------------------------
// 0 based array index
double Series::ExceedDelta(double y, int begIdx, int endIdx)   //GetTotDuration
{
  int n;
  double deltaX = 0.0;

  if (endIdx > count-1 || begIdx < 0)
    return 0.0;
  for (n=begIdx; n<endIdx; n++)
    deltaX += ExceedDelta(y,n);
  return deltaX;
}
//------------------------------------------------------------------------------
Series *Series::MakeExceed(int probability, int resolution)
{
  double minY, maxY;
  double y;

  if (XNotLinear()) return NULL;
  if (count<1 || resolution<1) return NULL;
  minY = MinY();
  maxY = MaxY();
  Series *T = new Series();
  T->Init(resolution+1);
  double step = (maxY - minY) / resolution;
  double Interval = xValues[count-1] - xValues[0];

  if (step==0.0)                            //
  {
    minY = maxY * (1.0-_eps);
    step = (maxY - minY)/resolution;
    y = minY;
    for (int i=0;i<=resolution; i++, y+=step)
    {
      T->xValues[i] = Interval - (i*(Interval/resolution));
      if (probability)
        T->xValues[i] = (T->xValues[i] / Interval) * 100.0;
      T->yValues[i] = y;
    }
    return T;
  }

  y = minY;
  for (int i=0; i<=resolution; i++, y+=step)     // create curve
  {
    if (i == resolution) y = maxY;
    T->xValues[i] = ExceedDelta(y, 0, count-1);
    if (probability)
      T->xValues[i] = (T->xValues[i] / Interval) * 100.0;
    T->yValues[i] = y;
  }
  return T;
}
//------------------------------------------------------------------------------
// this routine is used exclusively by sync operator
// the idea here is to create a new series from this series
// that is shifted and contains the desired time point intervals

Series *Series::MakeInterval(int year, double offX, double interval, int size, double error)
{
  int i;
  double area;
  double areaT;
  Series *T = new Series();

  T->Init(size+1);
  for (i=0;i<=size;i++)
  {
    T->xValues[i] = interval * i;
    T->yValues[i] = InterpY(T->xValues[i] - offX);
  }
  areaT = T->Integrate(T->xValues[0], T->xValues[size]);
  if (offX > 0)
    area = Integrate(xValues[0]+offX, xValues[0]+interval*size);
  else
    area = Integrate(xValues[0]-offX, xValues[0]-offX+interval*size);

  for (i=0;i<=size;i++)
    T->xValues[i] += year;

  if (isEqual(areaT,area))
    return T;
  if (area!=0)
  {
    double percent = areaT/area;
    if (percent < 1.0+error && percent > 1.0-error)
      return T;
  }
  sprintf(dummy,"Integrated sync area -> %f, Integrated acutal area - %f",areaT, area);
  writeError("");
  writeError(dummy);
  return NULL;
}
//------------------------------------------------------------------------------
void Series::Rankorder()
{
  qsort(yValues,count,sizeof(double),isequal);
  for (int i=0;i<count;i++)
    xValues[i] = ((double)(count-i)) / ((double)count + 1.0) * 100;
}
//------------------------------------------------------------------------------
void Series::Div(double scalar)
{
  for (int i=0;i<count;i++)    yValues[i]/=scalar;
}
//------------------------------------------------------------------------------
void Series::Mult(double scalar)
{
  for (int i=0;i<count;i++)    yValues[i]*=scalar;
}
//------------------------------------------------------------------------------
void Series::Mult(Series *T)
{
  for (int i=0;i<count;i++)    yValues[i]*=T->DiscreteInterpY(xValues[i]);
}
//------------------------------------------------------------------------------
double Series::Integrate(double begX, double endX)
{
  int i,si,ei;
  double total,begY,endY;

  if (begX>=endX)
  {
    total = begX;
    begX = endX;
    endX = total;
  }
  if (XNotLinear())            return 0.0;   // writes error to error file
  if (xValues[0]>endX)         return 0.0;   // perhaps a need to write warning
  if (xValues[count-1]<begX)   return 0.0;

  if (begX<=xValues[0])
  {
    begX=xValues[0];
    si=0;
  }
  else
    si=Find(begX);
  if (endX>=xValues[count-1])
  {
    endX=xValues[count-1];
    ei=count-2;
  }
  else
    ei=Find(endX);
  begY=interpolate(xValues[si],yValues[si],xValues[si+1],yValues[si+1],begX);
  endY=interpolate(xValues[ei],yValues[ei],xValues[ei+1],yValues[ei+1],endX);
  total=0.0;
  if (ei-si==0)                               // all in one interval
    total+=(endY+begY)*(endX-begX);
  else                                        // two or more intervals
  {
    for (i=si+1;i<ei;i++)
      total+=(yValues[i]+yValues[i+1])*(xValues[i+1]-xValues[i]);
    // add ends
    total+=(begY+yValues[si+1])*(xValues[si+1]-begX)+
           (yValues[ei]+endY)*(endX-xValues[ei]);
  }
  return total*0.5;
}
//------------------------------------------------------------------------------
double Series::Average(double begX, double endX)
{
  if (isEqual(begX,endX)) return 0.0;
  return Integrate(begX,endX)/fabs(endX-begX);
}
/*/------------------------------------------------------------------------------
void Series::RunAvg(double Period, int *NumRes, double *TimesRes, double *ValuesRes)
{
  // Compute NumRes Running Averages icremented by 1.0 in the xValues scale
  int i,idelta;
  double delta;

  if (count==2) return;
  delta = xValues[count-1]-xValues[0]-Period+1.0;
  if (delta>(float)((int)delta)) delta+=1.0;
  idelta=(int)delta;
  if (*NumRes< idelta)    writeError("Series::RunAvg - Array passed is not large enough");
  else if (Period==0.0)   writeError("Series::RunAvg - Period passed to is zero");
  else if (idelta<=0)     writeError("Series::RunAvg - Period is longer than supplied time interval");
  else if (*NumRes<=0)    writeError("Series::RunAvg - NumRes should be greater than or equal to zero.");
  else
  {
    *NumRes=idelta;
    for (i=0;i<(*NumRes);i++)
    {
      TimesRes[i]=xValues[0]+(double)i;
      ValuesRes[i]=Avgerage(TimesRes[i],TimesRes[i]+Period);
    }
  }
}
*/
//------------------------------------------------------------------------------
// T is supposed to be a binary series - 0 and 1
void Series::Adjust(Series *T)
{
  int i,j;

  if (!InsertXpts(T)) return;
  j = 0;

  while ((T->xValues[0] >= xValues[j]) && j < count)
  {
    yValues[j] = 0.0;
    j++;
  }

  for (i=0;i<T->count;i++)
  {
    //set default use frequency to 0
    while ((T->xValues[i+1] >= xValues[j]) && j < count)
    {
      yValues[j] *= T->yValues[i];
      j++;
    }
  }

  for (;j<count;j++)
    yValues[j] *= T->yValues[T->count-1];

}
//------------------------------------------------------------------------------
void Series::AddIn(Series *T)
{
  if (count < 1)
    Copy(T);
  else
    for (int i=0;i<count;i++)
      yValues[i]+=T->InterpY(xValues[i]);
}

//------------------------------------------------------------------------------
int Series::Add(Series *T)
{
  Series *tmp = T->Clone();
  if (XNotLinear()) return 0;
  if (tmp->XNotLinear()) return 0;
  InsertEnds(T);
  tmp->InsertEnds(this);
  Series *res = new Series(count+T->count+4);
  addSeries(count,xValues,yValues,tmp->count,tmp->xValues,tmp->yValues,&res->count,res->xValues,res->yValues);
  delete xValues;
  delete yValues;
  xValues = res->xValues;
  yValues = res->yValues;
  count = res->count;
  res->xValues = NULL;
  res->yValues = NULL;
  delete tmp;
  delete res;
  return 1;
}
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// 0 is succesfull 1 if falied to make exceedence series
int FRAMES_API GetProbability(int includezero, int resolution, int count, double *intimes, double *invalues,
                              double *minY, double *maxY, int *xBegIdx, int *xEndIdx, double *sprob, double *svalu)
{
  int i;
  int begIdx, endIdx;

  if (!includezero)
  {
    for (endIdx=count-1; endIdx>0; endIdx--)
    { if (invalues[endIdx]>0.0) break; }
    for (begIdx=0; begIdx<count && begIdx<endIdx; begIdx++)
    { if (invalues[begIdx]>0.0) break; }
  }
  else
  {
    begIdx = 0;
    endIdx = count-1;
  }

  Series *T = new Series();
  if (!T) return 1;
  T->Init(endIdx-begIdx+1,intimes+begIdx,invalues+begIdx);
  Series *P = T->MakeExceed(1,resolution);
  if (P)
  {
    for (i=0;i<P->count;i++)
    {
      sprob[i]=P->xValues[i];
      svalu[i]=P->yValues[i];
    }
    
    *xBegIdx = begIdx;
    *minY = T->MinY(begIdx,endIdx);
    *xEndIdx = endIdx;
    *maxY = T->MaxY(begIdx,endIdx);

    delete T;
    delete P;
    return 0;
  }
  delete T;
  return 1;
}
// API call will run the data though the series class to check integrity
// Numres will return a zero if the series can not be added together
void FRAMES_API AddSeries(int Num1, double *Times1, double *Values1,
                          int Num2, double *Times2, double *Values2,
                          int *NumRes, double *TimesRes, double *ValuesRes)
{
  int i = *NumRes;
  *NumRes = 0;
  Series *S1 = new Series();
  if (!S1) return;
  S1->Init(Num1,Times1,Values1);

  Series *S2 = new Series();
  if (!S2) return;
  S2->count = Num2;
  S2->xValues = Times2;
  S2->yValues = Values2;

  S1->Add(S2);
  if (i >= S1->count)
  {
    *NumRes = S1->count;
    for (int i=0;i<S1->count;i++)
    {
      TimesRes[i]=S1->xValues[i];
      ValuesRes[i]=S1->yValues[i];
    }
  }
  S2->count = 0;
  S2->xValues = NULL;
  S2->yValues = NULL;
  delete S1;
  delete S2;
}
