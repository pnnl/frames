#include "lseries.h"
#include "error.h"
#include <except.h>

LocSeries::LocSeries()
{
  NumLoc=0;
  NumLocFreq=0;
  Loc=NULL;
}

LocSeries::~LocSeries()
{
  if (Loc!=NULL)     delete[] Loc;
}

void LocSeries::InitConc(int numloc)
{
  try
  {
    NumLoc=numloc;
    Loc = new ConSeries[NumLoc];
  }
  catch(...)
  {
    Error("Out of Memory in LocSeries::InitConc");
    NumLoc=0;
    Loc=NULL;
  }
}

void LocSeries::ReadConcWCF(char *ConFile, char *locname)
{
  int i,j,k,numcon,numtime,progeny,numprog;
  int NumSectLines,NumConLines,locnum,loccnt;
  char GlyphName[40],dummy[40];
  float conc;
  fcsv *ConTVC;

  try
  { ConTVC = new fcsv;  }
  catch(...)
  {
    Error("Out of Memory in LocSeries::ReadConc");
    return;
  }

  ConTVC->open(ConFile,READ);
  loccnt = 0;
  while(!ConTVC->eof())
  {
    ConTVC->read(GlyphName);
    ConTVC->read(&NumSectLines);
    ConTVC->readln();
    ConTVC->read(&NumConLines);
    for (i=0;i<=NumConLines;i++)
      ConTVC->readln();
    ConTVC->read(&locnum);
    ConTVC->readln();
    for (i=0;i<locnum;i++)
    {
      ConTVC->read(dummy);
      if ((!strcmpi(locname,dummy) || !strcmpi(dummy,"all")) && loccnt < NumLoc)
      {
        Loc[loccnt].Read(ConTVC,GlyphName);
        loccnt++;
      }
      else
      {
        ConTVC->read(dummy);
        ConTVC->read(&numcon);
        ConTVC->readln();
        progeny = 0;
        for (j=0; j<numcon; j++)
        {
          ConTVC->read(dummy);
          ConTVC->read(dummy);
          ConTVC->read(dummy);
          ConTVC->read(dummy);
          ConTVC->read(&numtime);
          if (progeny)
          {
            numprog = 0;
            progeny--;
          }
          else
          {
            ConTVC->read(&numprog);
            progeny = numprog;
            j -= progeny;
          }
          ConTVC->readln();
          for (k=0; k<numtime; k++)
          {
            ConTVC->read(&conc);
            ConTVC->read(&conc);
            ConTVC->readln();
          }
        }
      }
    }
  }
  ConTVC->close();
  delete ConTVC;
}

void LocSeries::Combine(GIDFILE *f,ConSeries *CS)
{
  int i;

  for (i=0;i<NumLoc;i++)
    Loc[i].Adjust(&LocFreq);
  for (i=0;i<NumLoc;i++)
    CS->AddIn(&Loc[i]);
}
