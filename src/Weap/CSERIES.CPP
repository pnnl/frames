#include "cseries.h"
#include "error.h"
#include <except.h>

ConSeries::ConSeries()
{
  NumCon=0;
  Name[0]=0;
  Type[0]=0;
  Con=NULL;
}

ConSeries::~ConSeries()
{
  if (Con!=NULL) delete[] Con;
}

int ConSeries::GetIndex(char *IDNm)
{
 int i=0;
 while (strcmpi(IDNm,Con[i].Id) && i<NumCon)  i++;
 if (i<NumCon)   return i;
 else            return -1;
}

void ConSeries::ReadTWI(GIDFILE *fle, element *cas, element *time, element *dose, int num, int org, int med)
{
  Init(num);
  for (int i=1; i<=NumCon; i++)
    Con[i-1].ReadTWI(fle,cas,time,dose,org,i,med);
}

void ConSeries::ReadBBF(fcsv *ConTVC, int numc, int numv, int numu)
{
  int cnt,k;
  fpos_t pos;
  char Id[SMALLSTRING];
  char Name[SMALLSTRING];
  char xUnits[SMALLSTRING];
  char yUnits[SMALLSTRING];

  k=0;
  Init(numc*numv*numu);
  for (int i=0; i<numc; i++)
  {
    ConTVC->read(Id);
    ConTVC->read(Name);
    ConTVC->read(xUnits);
    ConTVC->read(yUnits);
    ConTVC->read(&cnt);
    ConTVC->readln();
    pos = ConTVC->getfpos();
    for (int j=1; j<=numv*numu; j++)
    {
      ConTVC->setfpos(pos);
      rstrcpy(Con[k].Id,Id);
      rstrcpy(Con[k].Name,Name);
      rstrcpy(Con[k].xUnits,xUnits);
      rstrcpy(Con[k].yUnits,yUnits);
//      Con[i].count = cnt;
      Con[k].Read(ConTVC,cnt,j);
      k++;
    }
  }
}

void ConSeries::Read(fcsv *ConTVC, int num)
{
  Init(num);
  for (int i=0; i<NumCon; i++)
    Con[i].Read(ConTVC);
}

void ConSeries::Adjust(Series *T)
{
  for (int i=0;i<NumCon;i++)
    Con[i].Adjust(T);
}

void ConSeries::Mult(Series *T)
{
  for (int i=0;i<NumCon;i++)
    Con[i].Mult(T);
}

void ConSeries::Init(int numcon)
{
  try
  {
    NumCon=numcon;
    Con=new Series[NumCon];
  }
  catch(...)
  {
    Error("Out of Memory in ConSeries::Init");
    NumCon=0;
    Con=NULL;
  }
}

void ConSeries::AddIn(ConSeries *C)
{
  int i,j,*map;
  if (Con==NULL)
  {
    Init(C->NumCon);
    for (i=0;i<NumCon;i++)
      Con[i].Copy(&(C->Con[i]));
  }
  else
  {
    try
    {
      map=new int[NumCon];
    }
    catch(...)
    {
      Error("Out of Memory in ConSeries::AddIn");
      return;
    }

    for (i=0;i<NumCon;i++)
      map[i]=-1;
    for (i=0;i<NumCon;i++)
      for (j=0;j<C->NumCon;j++)
        if (!rstrcmpi(Con[i].Id,C->Con[j].Id))
          map[i]=j;
    for (i=0;i<NumCon;i++)
      if (map[i]==-1)
        Error("Constituent: ",Con[i].Name," missing in Concentration Information");
    for (i=0;i<NumCon;i++)
      Con[i].AddIn(&(C->Con[map[i]]));
    delete[] map;
  }
}
