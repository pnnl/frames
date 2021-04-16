#include "toxdata.h"
#include "error.h"

ConData::ConData()
{
  NumCon=0;
  Con=NULL;
}

ConData::~ConData()
{
  if (Con!=NULL) delete[] Con;
}

void ConData::Init(int numcon)
{
  try
  {
    NumCon=numcon;
    Con=new ToxData[NumCon];
  }
  catch(...)
  {
    Error("Out of Memory in TimeSeries::AddEntry");
    NumCon=0;
    Con=NULL;
  }
}

void ConData::Read(char *toxfile)
{
  fcsv intox;
  int i,NumLines;

  intox.open(toxfile,READ);
  intox.read(&NumLines);
  intox.readln();
  for (i=0;i<NumLines;i++)
    intox.readln();
  intox.read(&NumCon);
  Init(NumCon);
  intox.readln();
  for (i=0;i<NumCon;i++)
    Con[i].Read(&intox);
  intox.close();
}

void ConData::ReadGID(GIDFILE *f,int site,int lifeidx)
{
  NumCon=ratoi(info(f,"NumCon",site));
  Init(NumCon);
  for (int i=0;i<NumCon;i++)
    Con[i].ReadGID(f,site,i,lifeidx);
}

void ToxData::Read(fcsv *intox)
{
  intox->read(IDName);
  intox->read(Name);
  intox->read(Species);
  intox->read(TimeUnits);
  intox->read(ConcUnits);
  intox->read(&NumLC);
  intox->readln();
  intox->read(&CCCvalue);
  intox->readln();
  intox->read(&AcuteTime);
  intox->readln();
  for (int i=0;i<NumLC;i++)
  {
    intox->read(&LCdur[i]);
    intox->read(&LCconc[i]);
    intox->readln();
  }
}

void ToxData::ReadGID(GIDFILE *f,int site,int con, int lifeidx)
{
  int i;
  paramrec p;

  rstrcpy(IDName,info(f,"FSCASID",site,con+1));
  rstrcpy(Name,info(f,"FSCNAME",site,con+1));
  rstrcpy(Species,info(f,"Species",lifeidx));
  NumLC=ratoi(info(f,"NumLC",lifeidx,con+1));
  CCCvalue=ratof(info(f,"CCCValue",lifeidx,con+1));
  AcuteTime=ratof(info(f,"AcuteTime",lifeidx,con+1));
  p.cnt1=lifeidx;
  p.cnt2=con+1;
  p.cnt4=0;
  p.cnt5=0;
  p.cnt6=0;
  for (i=0;i<NumLC;i++)
  {
    rstrcpy(p.name,"LCDur");
    p.cnt3=i+1;
    if (Get_Value(f,&p)!=NULL)
    {
      LCdur[i]=ratof(p.valu);
      rstrcpy(TimeUnits,p.punit);
    }
    rstrcpy(p.name,"LCConc");
    if (Get_Value(f,&p)!=NULL)
    {
      LCconc[i]=ratof(p.valu);
      rstrcpy(ConcUnits,p.punit);
    }
  }
}

/*
float ToxData::GetLCDuration(float conc)  //duration, concentration
{
 int i=0;

 if (conc<LCconc[NumLC-1]) return MAXFLOAT;
 if (conc>LCconc[0]) return 0;
 while (conc<LCconc[i+1] && i<NumLC) i++;
 return InterpDuration(conc,i);
}

int ToxData::CCCcheck(float conc)
{
  if (conc > CCCvalue)    return 1;
  else                    return 0;
}

float ToxData::InterpDuration(float conc,int count)
{
  float toxduration,f;

  if (LCconc[count+1] == LCconc[count]) return LCdur[count];
  f = (conc-LCconc[count])/(LCconc[count+1]-LCconc[count]);
  toxduration = LCdur[count] + f * (LCdur[count+1]-LCdur[count]);
  return toxduration;
}
*/
