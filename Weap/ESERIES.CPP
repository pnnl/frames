#include "eseries.h"
#include "error.h"
#include <except.h>

EcoSeries::EcoSeries()
{
  NumEco=0;
  Eco=NULL;
}

EcoSeries::~EcoSeries()
{
  if (Eco!=NULL)     delete[] Eco;
}

void EcoSeries::InitConc(int num)      // numloc = numlife * numcon
{
  try
  {
    NumEco=num;
    Eco=new ConSeries[NumEco];
  }
  catch(...)
  {
    Error("Out of Memory in EcoSeries::InitConc");
    NumEco=0;
    Eco=NULL;
  }
}

int EcoSeries::ReadIntakeTWI(GIDFILE *f)
{
  int aoc,i,j,numcon,numlife,numset;

    aoc = 1;  //there is always only one aoc

    element *cas = Get_Element(f,"ChemicalCASID");
    element *species = Get_Element(f,"OrganismSpecies");
    element *genus = Get_Element(f,"OrganismGenus");

    element *med = Get_Element(f,"MediaName");
    element *time = Get_Element(f,"TimePoint");
    element *dose = Get_Element(f,"Dose");

    numset = ratoi(info(f,"NumMedia",aoc,1,1));
    numlife = ratoi(info(f,"NumOrganism",aoc));

    // process all exposure media datasets
    int setcnt = 0;
    for (i=1; i<=numset; i++)
    {
      for (j=1; j<=numlife; j++)
      {
        // name of exposing intake media
        rstrcpy(Eco[setcnt].Name,getvalu(med,aoc,j,1,i));
        // number of chemicals for this lifeform
        numcon = ratoi(info(f,"NumChemical",aoc,j));
        // create scientific name
        rstrcpy(Eco[setcnt].Type,getvalu(genus,aoc,j));
        strcat(Eco[setcnt].Type," ");
        strcat(Eco[setcnt].Type,getvalu(species,aoc,j));
        // read time series
        Eco[setcnt].ReadTWI(f,cas,time,dose,numcon,j,i);
        setcnt++;
      }

    }
  NumEco = setcnt;
  return setcnt;
}

int EcoSeries::ReadConcBBF(GIDFILE *G, char *ConFile)
{
  int i,j,numcon,numsrc,numlife;
  int NumSectLines,NumComLines,numset,setcnt;
  int numv, numu;
  fcsv *bbf;
  char GlyphName[40];
  char dummy1[40];

  try
  { bbf = new fcsv;  }
  catch(...)
  {
    Error("Out of Memory in EcoSeries::ReadConcBBF");
    return 0;
  }

  bbf->open(ConFile,READ);
  setcnt = 0;
  numsrc = ratoi(info(G,"efxsrcnum",siteidx,modidx));

  while(!bbf->eof())
  {
    bbf->read(GlyphName);
    bbf->read(&NumSectLines);
    bbf->readln();

    // check if a source continue while if not
    for (i=1; i<=numsrc; i++)
      if (!strcmpi(GlyphName,info(G,"efxsrcname",siteidx,modidx,i)))
        break;
    if (i > numsrc)
    {
      for (j=0; j<NumSectLines; j++)
        bbf->readln();
      continue;
    }

    // must be source if we got here
    bbf->read(&NumComLines);
    for (i=0;i<=NumComLines;i++)
      bbf->readln();
    bbf->read(&numset);
    bbf->readln();

    // process all exposure media datasets
    for (i=0;i<numset;i++)
    {
      // get media name
      bbf->read(dummy1);
      bbf->read(dummy1);
      bbf->read(&numlife);
      bbf->read(&numv);
      bbf->read(&numu);
      bbf->readln();
      bbf->readln(vary);               // read off variability percentages
      bbf->readln(uncr);               // read off uncertainty percentages
      for (j=0; j<numlife; j++)
      {
        rstrcpy(Eco[setcnt].Name,dummy1);
        bbf->read(Eco[setcnt].Type);
        bbf->read(&numcon);
        bbf->readln();
        Eco[setcnt].ReadBBF(bbf,numcon,numv,numu);
        setcnt++;
      }
    }
  }
  bbf->close();
  delete bbf;
  return setcnt;
}

int EcoSeries::ReadConcWCF(GIDFILE *G, char *ConFile, char *modname)
{
  int i,j,k,m,numcon,numsrc;
  int k1,m1;
  int NumSectLines,NumComLines,numset,setcnt;
  fcsv *wcf;
  char GlyphName[40];
  char dummy1[40];
  char dummy2[40];

  try
  { wcf = new fcsv;  }
  catch(...)
  {
    Error("Out of Memory in EcoSeries::ReadConcWCF");
    return 0;
  }

  wcf->open(ConFile,READ);
  setcnt = 0;
  numsrc = ratoi(info(G,"efxsrcnum",siteidx,modidx));

  while(!wcf->eof())
  {

    wcf->read(GlyphName);
    wcf->read(&NumSectLines);
    wcf->readln();

    for (i=1; i<=numsrc; i++)
      if (!rstrcmpi(GlyphName,info(G,"efxsrcname",siteidx,modidx,i)))
        break;

    if (i > numsrc) // skip section
    {
      for (j=0; j<NumSectLines; j++)
        wcf->readln();
      continue;
    }

    wcf->read(&NumComLines);
    for (i=0;i<=NumComLines;i++)
      wcf->readln();
    wcf->read(&numset);
    wcf->readln();

    for (i=0;i<numset;i++)
    {
      wcf->read(dummy1);
      wcf->read(dummy2);
      wcf->read(&numcon);
      wcf->readln();

      if ((!rstrcmpi(modname,dummy1) ||
           !rstrcmpi("All",dummy1)) &&
           !rstrcmpi("Surface Water",dummy2))
      {
        Eco[setcnt].Read(wcf,numcon);
        setcnt++;
      }
      else
      {

        for (j=0; j<numcon; j++)
        {
          for (k=0; k<4; k++)
            wcf->read(dummy1);
          wcf->read(&k1);         // num time pairs
          wcf->read(&m1);         // num progeny
          wcf->readln();
          for (k=0; k<k1; k++)
            wcf->readln();
          for (k=0; k<m1; k++)
          {
            for (m=0; m<4; m++)
              wcf->read(dummy1);
            wcf->read(&k1);        // num time pairs
            wcf->readln();
            for (m=0; m<k1; m++)
              wcf->readln();
          }
        }
      }
    }
  }
  wcf->close();
  delete wcf;
  return setcnt;
}

int EcoSeries::ReadConcSCF(GIDFILE *G, char *ConFile, char *modname)
{
  int i,j,k,m,numcon,numsrc;
  int k1,m1;
  int NumSectLines,NumComLines,numset,setcnt;
  fcsv *scf;
  char GlyphName[40];
  char dummy[40];
  char dummy1[40];
  char dummy2[40];

  try
  { scf = new fcsv;  }
  catch(...)
  {
    Error("Out of Memory in EcoSeries::ReadConcSCF");
    return 0;
  }

  scf->open(ConFile,READ);
  setcnt = 0;
  numsrc = ratoi(info(G,"efxsrcnum",siteidx,modidx));

  while(!scf->eof())
  {
    scf->read(GlyphName);
    scf->read(&NumSectLines);
    scf->readln();

    for (i=1; i<=numsrc; i++)
      if (!strcmpi(GlyphName,info(G,"efxsrcname",siteidx,modidx,i)))
        break;

    if (i > numsrc) // skip section
    {
      for (j=0; j<NumSectLines; j++)
        scf->readln();
      continue;
    }

    scf->read(&NumComLines);
    for (i=0;i<=NumComLines;i++)
      scf->readln();
    scf->read(&numset);
    scf->readln();
    int loccnt = 0;

    for (i=0;i<numset;i++)
    {
      scf->read(dummy1);
      scf->read(dummy2);
      for (j=0; j<6; j++)
        scf->read(dummy);
      scf->read(&numcon);
      scf->readln();

      // check if intended consumer
      if ((!rstrcmpi(modname,dummy1) ||
           !rstrcmpi("All",dummy1)) &&
           !rstrncmpi("Soil",dummy2,4))
      {
        rstrcpy(Eco[setcnt].Name,GlyphName);
        sprintf(Eco[setcnt].Type,"Location#%d",loccnt+1);
        Eco[setcnt].Read(scf,numcon);
        setcnt++;
        loccnt++;
      }
      else
      {                           // scan off dataset
        for (j=0; j<numcon; j++)
        {
          for (k=0; k<4; k++)
            scf->read(dummy1);
          scf->read(&k1);         // num time pairs
          scf->read(&m1);         // num progeny
          scf->readln();
          for (k=0; k<k1; k++)
            scf->readln();
          for (k=0; k<m1; k++)
          {
            for (m=0; m<4; m++)
              scf->read(dummy1);
            scf->read(&k1);        // num time pairs
            scf->readln();
            for (m=0; m<k1; m++)
              scf->readln();
          }
        }
      }
    }
  }
  scf->close();
  delete scf;
  return setcnt;
}

void EcoSeries::Combine(ConSeries *CS, Series *Frequency)
{
  int i;

  for (i=0;i<NumEco;i++)
    Eco[i].Adjust(Frequency);
  for (i=0;i<NumEco;i++)
    CS->AddIn(&Eco[i]);
}
