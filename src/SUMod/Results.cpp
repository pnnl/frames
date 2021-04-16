#include "sumdata.h"
#include "gid.h"
//#include "hifread.h"
#include "hwirst.h"
#include "ReadFunc.h"
#include <math.h>
#include <stdlib.h>

//------------------------------------------------------------------------------
//int readStr(GIDFILE *g,int count,char *pname,char **str,int idx2);
//int readFloat(GIDFILE *g,int count,char *pname,float *f,int idx2);
//int readDouble(GIDFILE *g,int count,char *pname,double *f,int idx2);
//int readInt(GIDFILE *g,int count,char *pname,int *i,int idx2);
//------------------------------------------------------------------------------
void Result::reset()
{
  if(next) next->reset();
  if(dtime) delete[] dtime;
  if(dvalue) delete[] dvalue;
  dtime=NULL;
  dvalue=NULL;
  num=0;
}
//------------------------------------------------------------------------------
void Result::Peak(double *year, double *val)
{ // HWIRST
  Max(num,dtime,dvalue,year,val);

  char msg[MAXPATH];
  sprintf(msg,"returned from peak %d %g\n",(int)*year,(double)*val);
  writeError(msg);
  return;
}
//------------------------------------------------------------------------------
double Result::ValueAt(double year)
{
  // HWIRST
  return Interp(num,dtime,dvalue,year);
}
//------------------------------------------------------------------------------
double Result::Average(double year1, double year2)
{
  // HWIRST
  return Avg(num,dtime,dvalue,year1,year2);
}
//------------------------------------------------------------------------------
int Result::read(GIDFILE *g,int count)
{
   int i;
   char temp[30];
   if(!readStr(g,count,"suOutAlias",&Alias)) return 0;
   if(!readStr(g,count,"suOutDes",&Des)) return 0;
   if(!readStr(g,count,"suOutType",&Type)) return 0;
//   if(!readStr(g,count,"suOutSubType",&SubType)) return 0;
   if(!readStr(g,count,"suOutTime",&Time)) return 0;
   NumYears=0;
   if((!strcmpi(Time,"average years # to #")) || (!strcmpi(Time,"at year(s) #(,#...)")))
     if(!readInt(g,count,"suOutNumYear",&NumYears)) return 0;
   if(NumYears>0)
     Years=new double[NumYears];
   for(i=0;i<NumYears;i++)
     if(!readDouble(g,count,"suOutYear",&(Years[i]),i+1)) return 0;
   if (!readStr(g,count,"suOutContam",&Contam)) return 0;
   if (!readStr(g,count,"suOutParentContam",&Parent)) Parent = strdup(Contam);
//   if(!readStr(g,count,"suOutProgeny",&Progeny)) return 0;
   if(!readStr(g,count,"suOutSource",&Source)) return 0;
   if(!strcmpi(Time,"peak"))
     if(strcmpi(Type,"Air concentration")==0||strcmpi(Type,"Air deposition rate")==0||strcmpi(Type,"External dose")==0)
       NumValues = 4;
     else
       NumValues=2;
   else if(!strcmpi(Time,"at year(s) #(,#...)"))
     NumValues=NumYears;
   else
     NumValues=1;
   Labels = new char*[NumValues];
   Values = new double[NumValues];
   if (!strcmpi(Time,"peak"))
   {
     sprintf(temp,"%s peak time",Alias);
     Labels[0]=strdup(temp);
     Labels[1]=strdup(Alias);
     Values[0]=0.0;
     Values[1]=0.0;
     //added to handle the ATO file
     if(strcmpi(Type,"Air concentration")==0||strcmpi(Type,"Air deposition rate")==0||strcmpi(Type,"External dose")==0)
     {
       sprintf(temp,"%s axis 1 coordinate",Alias);
       Labels[2] = strdup(temp);
       Values[2] = 0.0;
       sprintf(temp,"%s axis 2 coordinate",Alias);
       Labels[3] = strdup(temp);
       Values[3] = 0.0;
     }
   }
   else if(!strcmpi(Time,"at year(s) #(,#...)"))
   {
     for(i=0;i<NumYears;i++)
     {
       sprintf(temp,"%s (%10.3g)",Alias,Years[i]);
       Labels[i]=strdup(temp);
     }
   }
   else
     Labels[0]=strdup(Alias);
   return 1;
}
//------------------------------------------------------------------------------
Result::Result()
{
  Alias=NULL;
  Des=NULL;
  Type=NULL;
  Time=NULL;
  Contam=NULL;
  Progeny=NULL;
  SubType=NULL;
  Source=NULL;
  Sink=NULL;
  Parent=NULL;
  Labels=NULL;

  NumYears=0;
  NumValues=0;
  NumLoc=0;
  num=0;
  Years=NULL;
  Values=NULL;
  dvalue=NULL;
  dtime=NULL;
  home=this;
  next=NULL;

  xcoordinate=0.0;
  ycoordinate=0.0;

  strcpy(descrip,"");
}
//------------------------------------------------------------------------------
Result::Result(Result *head)
{
  Alias = head->Alias;
  Des = head->Des;
  Type = head->Type;
  Time = head->Time;
  Contam = head->Contam;
  Progeny = head->Progeny;
  SubType = head->SubType;
  Source = head->Source;
  Sink = head->Sink;
  Parent = head->Parent;
  Labels = head->Labels;

  xcoordinate=0.0;
  ycoordinate=0.0;

  NumYears=head->NumYears;
  NumValues=head->NumValues;
  Years=head->Years;
  Values=head->Values;
  NumLoc=0;
  num=0;
  dtime=NULL;
  dvalue=NULL;
  home=head;
  next=NULL;
}
//------------------------------------------------------------------------------
Result::~Result()
{
  int i;
  
/*
  if (Alias) {delete Alias; Alias=NULL; }
  if (Des) { delete Des; Des=NULL; }
  if (Type) {delete Type; Type=NULL; }
  if (Time) { delete Time; Time=NULL; }
  if (Contam) { delete Contam; Contam=NULL; }
  if (Progeny) { delete Progeny; Progeny=NULL; }
  if (SubType) { delete SubType; SubType=NULL; }
  if (Source) { delete Source; Source=NULL; }
  if (Sink) { delete Sink; Sink=NULL; }
  if (Parent) { delete Parent; Parent=NULL; }
*/

  if(NumYears>0 && Years) {
    delete[] Years;
    Years=NULL;
    NumYears=0;
    }
  if(NumValues>0)
  {
    for(i=0;i<NumValues;i++)
      free((char*)Labels[i]);
    delete[] Labels;
    delete[] Values;
    Labels=NULL;
    Values=NULL;
    NumValues=0;
  }
  if (num>0)
  {
    delete[] dtime;
    delete[] dvalue;
    dtime=NULL;
    dvalue=NULL;
    num=0;
  }
  next=NULL;
}
//------------------------------------------------------------------------------
int Result::read(char *fuiname)
{
//  if (strcmpi(Type,"Radiological carcinogenic risk")==0 ||
//      strcmpi(Type,"Chemical carcinogenic risk")==0 ||
//      strcmpi(Type,"Chemical hazard index")==0 )
  if(strcmpi(Type,"Radiation dose")==0||strcmpi(Type,"Cumulative dose")==0||
      strcmpi(Type,"Cancer incidence")==0||strcmpi(Type,"Cancer fatalities")==0||
      strcmpi(Type,"Cancer+hereditary effects")==0||
      strcmpi(Type,"Carcinogenic risk")==0||strcmpi(Type,"Noncarcinogenic HI")==0)
    return readHIF(fuiname);
  else if(strcmpi(Type,"Ingestion")==0||strcmpi(Type,"Inhalation")==0||
           strcmpi(Type,"External exposure")==0||strcmpi(Type,"Dermal exposure")==0)
    return readRIF(fuiname);
  else if(strcmpi(Type,"Water concentration")==0)
    return readWCF(fuiname);
  else if(strcmpi(Type,"Air concentration")==0||strcmpi(Type,"Air deposition rate")==0||
           strcmpi(Type,"External dose")==0)
    return readATO(fuiname);
  else if(strcmpi(Type,"Water flux")==0||strcmpi(Type,"Water contaminant flux")==0)
    return readWFF(fuiname);
  else if(strcmpi(Type,"Suspension release")==0||strcmpi(Type,"Volatilization release")==0)
    return readAFF(fuiname);
  else if(strcmpi(Type,"Soil concentration")==0)
    return readSCF(fuiname);
  return 0;
}
//------------------------------------------------------------------------------
int Result::readHIF(char *fuiname)
{
  return procHIF(fuiname);
}
//------------------------------------------------------------------------------
int Result::readRIF(char *fuiname)
{
  return 1;
}
//------------------------------------------------------------------------------
int Result::readWCF(char *fuiname)
{
//return readWCF(fuiname,Source,Contam);
  return procWCF(fuiname);
}
//------------------------------------------------------------------------------
int Result::readATO(char *fuiname)
{
  return procATO(fuiname);
}
//------------------------------------------------------------------------------
int Result::readWFF(char *fuiname)
{
  return procWFF(fuiname);
}
//------------------------------------------------------------------------------
int Result::readAFF(char *fuiname)
{
  return procAFF(fuiname);
}
//------------------------------------------------------------------------------
int Result::readSCF(char *fuiname)
{
  return procSCF(fuiname);
}
//------------------------------------------------------------------------------
/*
void Result::writeSeriesHeader(fcsv *f)
  {
    int i,j;
    for (i=0;i<NumSeries;i++)
      for (j=0;j<NumValues;j++)
        f->write(series[i]->label);
  }
void Result::writeSUFHeader(fcsv *f)
  {
    int i;
    for (i=0;i<NumValues;i++)
      f->write(Labels[i]);
  }
void Result::writeSUF(fcsv *f)
  {
    int i,j;
    for (j=0;j<NumSeries;j++)
      {
      computeResults(j);
      for (i=0;i<NumValues;i++)
        f->write(Values[i]);
      }
  }

void Result::computeResults(int i)
  {
    int y;
    if (!strcmpi(Time,"peak"))
      series[i]->Peak(&Values[0], &Values[1]);
    else if (!strcmpi(Time,"average years # to #"))
      Values[0] = series[i]->Average(Years[0], Years[1]);
    else
      for (y=0;y<NumYears;y++)
        Values[y] = series[i]->ValueAt(Years[y]);
  }
*/
//------------------------------------------------------------------------------
void Result::writeSeriesHeader(fcsv *f)
{
  return;
}
//------------------------------------------------------------------------------
void Result::writeSUFHeader(fcsv *f)
{
  int i,j=0;
  Result *res;
  char cbuf[256];

  res = this;
  while (res!=NULL) {
    if ((res->next!=NULL) || (res!=res->home)) j++;
//  if (0<strlen(loctype)) j++;
    for(i=0;i<NumValues;i++) {
      if (j>0)
        sprintf(cbuf,"%s[%d]",Labels[i],j);
      else
        strcpy(cbuf,Labels[i]);
      f->write(cbuf);
    }
    res = res->next;
  }

/*
    for(i=0;i<NumValues;i++) {
      if (0<strlen(loctype))
        sprintf(cbuf,"%s[%s]",Labels[i],loctype);
      else
        strcpy(cbuf,Labels[i]);
      f->write(cbuf);
      }
    if(next)
      next->writeSUFHeader(f);
*/
}
//------------------------------------------------------------------------------
void Result::writeSUF(fcsv *f)
{
  int i;
  computeResults(0);
  for(i=0;i<NumValues;i++)
    f->write(Values[i]);
  if(next)
    next->writeSUF(f);
}
//------------------------------------------------------------------------------
void Result::computeResults(int i)
{
  int y;
  if(!strcmpi(Time,"peak"))
  {
    Peak(&Values[0], &Values[1]);
    //added for the ATO file
    if(strcmpi(Type,"Air concentration")==0||strcmpi(Type,"Air deposition rate")==0||strcmpi(Type,"External dose")==0)
    {
      Values[2] = xcoordinate;
      Values[3] = ycoordinate;
    }
  }
  else if(!strcmpi(Time,"average years # to #"))
    Values[0] = Average(Years[0], Years[1]);
  else
    for(y=0;y<NumYears;y++)
      Values[y] = ValueAt(Years[y]);
}

