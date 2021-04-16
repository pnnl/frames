#include "hwirst.h"
#include <stdio.h>
#include <iostream.h>
#include <fstream.h>
#include <stdlib.h>

void Error(char *s)
  {
    ofstream *out;
    out=new ofstream("error.");
    *out << s << "\n\n";
    out->flush();
    delete out;
    exit(0);
  }

char *toString(Descriptor *d,char *comment)
{
	static char info[1000];
	sprintf(info,"%s\n"
                 "Base:   %ld\n"
		         "Size:   %ld\n"
				 "Offset: %ld\n"
				 "Defined:%ld\n"
				 "Dim:    %ld\n"
				 "NumElem:%ld\n"
				 "Dist:   %ld\n"
				 "Lower:  %ld\n",
				 comment,
				 d->Base,d->Size,d->Offset,d->Defined,
				 d->Dim,d->NumElem,d->Dist,d->Lower);
    return info;
}

double *Convert(Descriptor *d)
{
  if (d->Dist!=8) Error(toString(d,"ST: A Real*8 Array was not handed in by Digital Fortran"));
  return (double *)(d->Base+d->Offset+d->Size);
}


double PntInterp(double t1,double y1,double t2,double y2,double t)
  {
    return y1+(y2-y1)*(t-t1)/(t2-t1);
  }
  
void TestTimes(int Num,double *Times)
  {
    int i;
    if (Num==1) return;
    for (i=1;i<Num;i++)
      if (Times[i-1]>Times[i])
        Error("ST: Times out of order in time series");
      else if (Times[i-1]==Times[i])
        Error("ST: Two times are equal in time series");
  }

void FPASS fMult(int Num,double *Times,double *Values,double Scalar)
  {
    Mult(Num,Times,Values,Scalar);
  }

//void DFPASS dfMult(int Num,double *Times,double *Values,double Scalar)
void DFPASS dfMult(int Num,Descriptor *Times,Descriptor *Values,double Scalar)
  {
    Mult(Num,Convert(Times),Convert(Values),Scalar);
//    Mult(Num,Times,Values,Scalar);
  }

void CPASS Mult(int Num,double *Times,double *Values,double Scalar)
  {
    register int i;
    TestTimes(Num,Times);
    for (i=0;i<Num;i++)
      Values[i]*=Scalar;
  }

void FPASS fAdd(int Num1,double *Times1,double *Values1,
               int Num2,double *Times2,double *Values2,
               int *NumRes,double *TimesRes,double *ValuesRes)
  {
    Add(Num1,Times1,Values1,
        Num2,Times2,Values2,
        NumRes,TimesRes,ValuesRes);
  }

//void DFPASS dfAdd(int Num1,double *Times1,double *Values1,
//               int Num2,double *Times2,double *Values2,
//               int *NumRes,double *TimesRes,double *ValuesRes)
void DFPASS dfAdd(int Num1 ,Descriptor *Times1  ,Descriptor *Values1,
               int Num2   ,Descriptor *Times2  ,Descriptor *Values2,
               int *NumRes,Descriptor *TimesRes,Descriptor *ValuesRes)
  {
    Add(Num1,Convert(Times1),Convert(Values1),
        Num2,Convert(Times2),Convert(Values2),
        NumRes,Convert(TimesRes),Convert(ValuesRes));
//    Add(Num1,Times1,Values1,
//        Num2,Times2,Values2,
//        NumRes,TimesRes,ValuesRes);
  }


void quickAdd(int Num1,double *Times1,double *Values1,
              int Num2,double *Times2,double *Values2,
              int *NumRes,double *TimesRes,double *ValuesRes)
  {
    int i,j,c;
    i=0; j=0; c=0;
    while (i<Num1 && j<Num2)
      if (Times1[i]<Times2[j])
        { // bump times1 to next interval
          TimesRes[c]=Times1[i];
          ValuesRes[c]=Values1[i];
          if (j>0)
            ValuesRes[c]+=PntInterp(Times2[j-1],Values2[j-1],
                                   Times2[j]  ,Values2[j],
                                   TimesRes[c]);
          i++;
          c++;
        }
      else if (Times2[j]<Times1[i])
        { // bump times2 to next interval
          TimesRes[c]=Times2[j];
          ValuesRes[c]=Values2[j];
          if (i>0)
            ValuesRes[c]+=PntInterp(Times1[i-1],Values1[i-1],
                                    Times1[i]  ,Values1[i],
                                    TimesRes[c]);
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

int zeroStartsEnd(int Num1,double *Times1,double *Values1,
                   double **Times,double **Values)
  {
    // insure time series has zero starts and ends
    static double epsilon=1e-3;
    int i,n1,soffset,eoffset;
    n1=Num1;
    soffset=0;
    eoffset=0;
//  if (Values1[0]!=0.0)
    // in the case of the HIF where the value is nonzero at time zero
    //   this test produced an extra time zero and at the next iteration
    //   TestTimes terminates because the first 2 times are equal (zero).
    if (Values1[0]!=0.0 && Times1[0]!=0.0) // BLH nov-997
      {
        n1++;
        soffset=1;
      }
    if (Values1[Num1-1]!=0.0 && Times1[Num1-1]!=0.0) // same comment as above
      {
        n1++;
        eoffset=1;
      }
    (*Times)=new double[n1];
    (*Values)=new double[n1];
    for (i=0;i<Num1;i++)
      {
        (*Times)[i+soffset]=Times1[i];
        (*Values)[i+soffset]=Values1[i];
      }
    if (soffset)
      {
        (*Times)[0]=Times1[0]*(1.0-epsilon);
        (*Values)[0]=0.0;
      }
    if (eoffset)
      {
        (*Times)[i+soffset]=Times1[i-1]*(1.0+epsilon);
        (*Values)[i+soffset]=0.0;
      }
    return n1;
  }

void CPASS Add(int Num1,double *Times1,double *Values1,
               int Num2,double *Times2,double *Values2,
               int *NumRes,double *TimesRes,double *ValuesRes)
  {
    int n1,n2;
    double *t1,*t2,*v1,*v2;
    TestTimes(Num1,Times1);
    TestTimes(Num2,Times2);
    if (*NumRes<Num1+Num2+4)
      Error("ST: Array passed to Add is not large enough");
    n1=zeroStartsEnd(Num1,Times1,Values1,&t1,&v1);  // news t1,v1
    n2=zeroStartsEnd(Num2,Times2,Values2,&t2,&v2);  // new t2,v2
    quickAdd(n1,t1,v1,n2,t2,v2,NumRes,TimesRes,ValuesRes);
    delete t1;
    delete t2;
    delete v1;
    delete v2;
  }

void FPASS fMax(int Num,double *Times,double *Values,
               double *MaxTime,double *MaxValue)
  {
    Max(Num,Times,Values,MaxTime,MaxValue);
  }
//void DFPASS dfMax(int Num,double *Times,double *Values,
//               double *MaxTime,double *MaxValue)
void DFPASS dfMax(int Num,Descriptor *Times,Descriptor *Values,
               double *MaxTime,double *MaxValue)
  {
    Max(Num,Convert(Times),Convert(Values),MaxTime,MaxValue);
//    Max(Num,Times,Values,MaxTime,MaxValue);
  }
void CPASS Max(int Num,double *Times,double *Values,
               double *MaxTime,double *MaxValue)
  {
    register int i,t;
    if (Num==0) return;
//  TestTimes(Num,Times); BLH 3/26/99
    t=0;
    for (i=1;i<Num;i++)
      if (Values[i]>Values[t]) t=i;
    *MaxTime=Times[t];
    *MaxValue=Values[t];
  }

void FPASS fMin(int Num,double *Times,double *Values,
               double *MinTime,double *MinValue)
  {
    Min(Num,Times,Values,MinTime,MinValue);
  }
//void DFPASS dfMin(int Num,double *Times,double *Values,
//               double *MinTime,double *MinValue)
void DFPASS dfMin(int Num,Descriptor *Times,Descriptor *Values,
                  double *MinTime,double *MinValue)
  {
    Min(Num,Convert(Times),Convert(Values),MinTime,MinValue);
//    Min(Num,Times,Values,MinTime,MinValue);
  }
void CPASS Min(int Num,double *Times,double *Values,
               double *MinTime,double *MinValue)
  {
    register int i,t;
    if (Num==0) return;
    TestTimes(Num,Times);
    t=0;
    for (i=1;i<Num;i++)
      if (Values[i]<Values[t]) t=i;
    *MinTime=Times[t];
    *MinValue=Values[t];
  }

int Find(int Num,double *Times,double Time)
  {
    register int i;
    if (Num==0) return -1;
    if (Time<Times[0]) return -1;
    if (Time>Times[Num-1]) return -1;
    for (i=1;i<Num;i++)
      if (Times[i-1]<=Time && Time<Times[i]) return i-1;
    return Num-1;
  }

double Integrate(int Num,double *Times,double *Values,
                 double Start,double End)
  {
    register int s,e,i;
    double total,vs,ve;
    if (Start<Times[0])
      {
        Start=Times[0];
        s=0;
      }
    else
      s=Find(Num,Times,Start);
    if (End>Times[Num-1])
      {
        End=Times[Num-1];
        e=Num-2;
      }
    else
      e=Find(Num,Times,End);
    vs=PntInterp(Times[s],Values[s],Times[s+1],Values[s+1],Start);
    ve=PntInterp(Times[e],Values[e],Times[e+1],Values[e+1],End);
    total=0.0;
    if (e-s==0) // all in one interval
      total+=(ve+vs)*(End-Start);
    else           // two or more
      {
        for (i=s+1;i<e;i++)
          total+=(Values[i]+Values[i+1])*(Times[i+1]-Times[i]);
//        cout << total << "\n";
        total+=(Values[s+1]+vs)*(Times[s+1]-Start)+
               (ve+Values[e])*(End-Times[e]);
      }
    return total*0.5;
  }

double FPASS fAvg(int Num,double *Times,double *Values,
                 double Start,double End)
  {
    return Avg(Num,Times,Values,Start,End);
  }
//double DFPASS dfAvg(int Num,double *Times,double *Values,
//                 double Start,double End)
double DFPASS dfAvg(int Num,Descriptor *Times,Descriptor *Values,
                    double Start,double End)
  {
    return Avg(Num,Convert(Times),Convert(Values),Start,End);
//    return Avg(Num,Times,Values,Start,End);
  }
double CPASS Avg(int Num,double *Times,double *Values,
                 double Start,double End)
  {
    if (Start>=End)
      Error("ST: Incorrect boundary definition in call to Avg");
    TestTimes(Num,Times);
    if (Times[Num-1]<Start) return 0.0;
    if (Times[0]>End)return 0.0;
    return Integrate(Num,Times,Values,Start,End)/(End-Start);
  }

void FPASS fRunAvg(int Num,double *Times,double *Values,
               double Period,
               int *NumRes,double *TimesRes,double *ValuesRes)
  {
    RunAvg(Num,Times,Values,Period,NumRes,TimesRes,ValuesRes);
  }
//void DFPASS dfRunAvg(int Num,double *Times,double *Values,
//               double Period,
//               int *NumRes,double *TimesRes,double *ValuesRes)
void DFPASS dfRunAvg(int Num,Descriptor *Times,Descriptor *Values,
                     double Period,
                     int *NumRes,Descriptor *TimesRes,Descriptor *ValuesRes)
  {
    RunAvg(Num,Convert(Times),Convert(Values),Period,
		   NumRes,Convert(TimesRes),Convert(ValuesRes));
//    RunAvg(Num,Times,Values,Period,
//		   NumRes,TimesRes,ValuesRes);
  }
void CPASS RunAvg(int Num,double *Times,double *Values,
               double Period,
               int *NumRes,double *TimesRes,double *ValuesRes)
  {
    // Compute NumRes Running Averages icremented by 1.0 in the times scale
    register int i,idelta;
    double delta;
    if (Num==2) return;
    TestTimes(Num,Times);
    delta=Times[Num-1]-Times[0]-Period+1.0;
    if (delta>(float)((int)delta)) delta+=1.0;
    idelta=(int)delta;
    if (*NumRes< idelta)
      Error("ST: Array passed to RunAvg is not large enough");
    if (Period==0.0)
      Error("ST: Period passed to RunAvg is zero");
    if (idelta<=0)
      Error("ST: Period is longer than supplied time interval");
    if (*NumRes<=0)
      Error("ST: NumRes in RunAvg should be greater than or equal to zero.");
    *NumRes=idelta;
    for (i=0;i<(*NumRes);i++)
      {
        TimesRes[i]=Times[0]+(double)i;
        ValuesRes[i]=Avg(Num,Times,Values,TimesRes[i],TimesRes[i]+Period);
      }
  }

double FPASS fInterp(int Num,double *Times,double *Values,
                 double Time)
  {
    return Interp(Num,Times,Values,Time);
  }
//double DFPASS dfInterp(int Num,double *Times,double *Values,
//                 double Time)
double DFPASS dfInterp(int Num,Descriptor *Times,Descriptor *Values,
                       double Time)
  {
    return Interp(Num,Convert(Times),Convert(Values),Time);
//    return Interp(Num,Times,Values,Time);
  }
double CPASS Interp(int Num,double *Times,double *Values,
                 double Time)
  {
    register int i;
    if (Time<Times[0]) return 0.0;
    if (Time>Times[Num-1]) return 0.0;
    TestTimes(Num,Times);
    i=Find(Num,Times,Time);
    if (Times[i]==Time) return Values[i];
    return Values[i]+(Time-Times[i])*(Values[i+1]-Values[i])/
                     (Times[i+1]-Times[i]);
  }
