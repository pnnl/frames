
#pragma hdrstop
#include <condefs.h>
#include <conio.h>
#include "csv.h"
#include "ReduceWff.h"

//#define NewLn NewLn

//---------------------------------------------------------------------------
USEUNIT("..\Common Files\robust.cpp");
USEUNIT("..\Common Files\csv.cpp");
USEUNIT("ReduceWff.cpp");
USERES("WFFTrim.res");
//---------------------------------------------------------------------------
class WFFFlux
  {
  int NumFlux;
  char Units[64],dummy[64];
  float *Times;
  float *Fluxes;
  public:
  void Read(icsv *in)
    {
    int i;
/*  Line with:Water flux time units-"yr"
    Water flux units -"m^3/yr"
    Number of water flux time-pairs-(integer)
 For each Water flux time-pair
   Water Flux time-(float)
   Water Flux-(float)
 For each constituent
*/
    *in >> dummy >> Units >> NumFlux >> NewLn;
    Times = new float[NumFlux];
    Fluxes = new float[NumFlux];
    for (i=0;i<NumFlux;i++)
      *in >> Times[i] >> Fluxes[i] >> NewLn;
    }
  void Write(ocsv *out)
    {
    int i;
    *out << "yr" << Units << NumFlux << NewLn;
    for (i=0;i<NumFlux;i++)
      *out << Times[i] << Fluxes[i] << NewLn;
    delete[] Times;
    delete[] Fluxes;
    }
  };

class WFFProg
  {
    char *Name,*Id,*Units,*Parent,*ParentId;
    int NumFlux,NumTypes;
    float *Times;
    float **Fluxes;
/*
     Line with: Progeny Name-(string)
       Progeny ID-(string)
       Time units-"yr"
       Flux units-"pCi/y" or "g/y"
       Number of fluxes-(integer)
       Number of flux types-(integer)
         ;Number of flux types will be 2 for source module
         ;to surface water and for overland flow, it will be
         ;1 for all other situations
       Parent Name-(string)
       Parent ID-(string)
   For each flux
     Line with: Media Time-(float)
       Flux1-(float)
         ;adsorbed flux if 2 types, otherwise total
       Flux2-(float)
         ;dissolved flux if 2 types, otherwise omit
*/
    public:
    void Read(icsv *in)
      {
        int i,j;
        char line[256];
        *in >> line; Name=strdup(line);
        *in >> line; Id=strdup(line);
        *in >> line;
        *in >> line; Units=strdup(line);
        *in >> NumFlux;
        *in >> NumTypes;
        *in >> line; Parent=strdup(line);
        *in >> line; ParentId=strdup(line);
        *in >> NewLn;
        sprintf(line,"Reading %s (%s)",Name,Id); puts(line);
        Times=new float[NumFlux];
        Fluxes=new float*[NumFlux];
        for (i=0;i<NumFlux;i++)
          {
            *in >> Times[i] ;
            Fluxes[i]=new float[NumTypes];
            for (j=0;j<NumTypes;j++)
              *in >> Fluxes[i][j];
            *in >> NewLn;
          }
      }
    void Reduce(float ratio, int numpts)
      {
        int i,Before;
        char line[256];
        Before=NumFlux;
        ::Reduce(NumTypes,&NumFlux,&Times,&Fluxes,ratio,numpts);
        sprintf(line, "Reducing %s (%s) to %f%s of original size",Name,Id,
                    ((float)NumFlux*100.0/(float)Before),"%"); puts(line);
      }
    void Write(ocsv *out)
      {
        int i,j;
        *out << Name << Id << "yr" << Units << NumFlux << NumTypes << Parent << ParentId << NewLn;
        delete Name;
        delete Id;
        delete Units;
        delete Parent;
        delete ParentId;
        for (i=0;i<NumFlux;i++)
          {
            *out << Times[i] ;
            for (j=0;j<NumTypes;j++)
              *out << Fluxes[i][j];
            delete[] Fluxes[i];
            *out <<  NewLn;
          }
        delete[] Times;
        delete[] Fluxes;
      }
  };

class WFFCon
  {
/*
For each constituent
   Line with: Constituent Name-(string)
     Constituent ID-(string)
     Time units-"yr"
     Flux units-"pCi/y" or "g/y"
     Number of time-fluxes pairs-(integer)
     Number of flux types-(integer)
     Number of progeny-(integer)
   For each flux
     Line with: Time-(float)
       Flux1-(float)
         ;adsorbed flux if 2 types, otherwise total
       Flux2-(float)
         ;dissolved flux if 2 types, otherwise omit
   For each progeny
*/
    char *Name,*Id,*Units;
    int NumFlux,NumProg,NumTypes;
    float *Times;
    float **Fluxes;
    WFFProg *Prog;
    public:
    void Read(icsv *in)
      {
        int i,j;
        char line[256];
        *in >> line; Name=strdup(line);   // Constituent Name
        *in >> line; Id=strdup(line);     // Constituent Id
        *in >> line;                      // Time unit (yr)
        *in >> line; Units=strdup(line);  // Flux unit

        sprintf(line,"Reading %s (%s)",Name,Id); puts(line);

        *in >> NumFlux >> NumTypes >> NumProg >> NewLn;
        Times=new float[NumFlux];
        Fluxes=new float*[NumFlux];
        for (i=0;i<NumFlux;i++)
          {
            *in >> Times[i] ;
            Fluxes[i]=new float[NumTypes];
            for (j=0;j<NumTypes;j++)
              *in >> Fluxes[i][j];
            *in >> NewLn;
          }
        Prog=new WFFProg[NumProg];
        for (i=0;i<NumProg;i++)
          Prog[i].Read(in);
      }
    void Reduce(float ratio, int numpts)
      {
        int i,Before;
        char line[256];
        Before=NumFlux;
        ::Reduce(NumTypes,&NumFlux,&Times,&Fluxes,ratio,numpts);
        sprintf(line,"Reducing %s (%s) to %f%s of original size",Name,Id,((float)NumFlux*100.0/(float)Before),"%"); puts(line);
        for (i=0;i<NumProg;i++)
          Prog[i].Reduce(ratio, numpts);
      }
    void Write(ocsv *out)
      {
        int i,j;
        char line[256];
        *out << Name << Id << "yr" << Units << NumFlux << NumTypes << NumProg << NewLn;
        sprintf(line,"Writing %s (%s)",Name,Id); puts(line);
        delete Name;
        delete Id;
        delete Units;
        for (i=0;i<NumFlux;i++)
          {
            *out << Times[i] ;
            for (j=0;j<NumTypes;j++)
              *out << Fluxes[i][j];
            delete[] Fluxes[i];
            *out <<  NewLn;
          }
        delete[] Times;
        delete[] Fluxes;
        for (i=0;i<NumProg;i++)
          Prog[i].Write(out);
        delete[] Prog;
      }
  };

class WFFFluxSet
  {
/*
For each medium
  Line with: Medium Name-(string)
    Medium Type-(string)
    Rectangular area width-(float)
       Width units-"m"
    Rectangular area Length-(float)
    Length or Height Units-"m",
    Distance from Water Table -(float)
    Distance from water table units-"m",
    Natural Recharge Rate-(float)
    Natural Recharge Rate Units "m/yr"
    Number of constituents-(integer)
*/
    char *Name,*Type;
    float Width, Length, Dist, Rech;
    int NumCon;

    WFFCon *ConFlux;
    WFFFlux *WaterFlux;
    public:
    void Read(icsv *in)
      {
        int i;
        char line[256],dummy[256];
        *in >> line; Name=strdup(line); // Medium Name - (string)
        *in >> line; Type=strdup(line); // Medium Type - (string)
        *in >> Width >> dummy;    // Rectangular area width-(float)
        *in >> Length >> dummy;   // Rectangular area Length-(float)
        *in >> Dist >> dummy;     // Distance from Water Table -(float)
        *in >> Rech >> dummy;     // Natural Recharge Rate-(float)
        *in >> NumCon >> NewLn; // Number of constituents-(integer)

        WaterFlux = new WFFFlux;
        WaterFlux->Read(in);

        ConFlux=new WFFCon[NumCon];
        for (i=0;i<NumCon;i++)
          ConFlux[i].Read(in);
      }
    void Reduce(float ratio, int numpts)
      {
        int i;
        for (i=0;i< NumCon; i++)
          ConFlux[i].Reduce(ratio,numpts);
      }
    void Write(ocsv *out)
      {
        int i;
        *out << Name; delete Name;
        *out << Type; delete Type;
        *out << Width << "m";
        *out << Length << "m";
        *out << Dist << "m";
        *out << Rech << "m/yr" << NumCon << NewLn;

        WaterFlux->Write(out);
        delete WaterFlux;

        for (i=0;i<NumCon;i++)
          ConFlux[i].Write(out);
        delete[] ConFlux;
      }
  };

class WFFData
  {
/*
Line with: Number of Lines of Header Information-(integer)
For each
  Line with: Run information-(string)
Line with: Number of Media-(integer)
*/
    int RunCount;
    char **RunInfo;
    int FluxSetCount;
    WFFFluxSet *fluxes;
    public:
    void Read(icsv *in)
      {
        char line[256];
        int i;
        /* header information */
        *in >> RunCount >> NewLn;
        RunInfo=new char*[RunCount];
        for (i=0;i<RunCount;i++)
          {
            *in >> line >> NewLn;
            RunInfo[i]=strdup(line);
          }
        /* media */
        *in >> FluxSetCount >> NewLn;
       fluxes=new WFFFluxSet[FluxSetCount];
        for (i=0;i<FluxSetCount;i++)
          fluxes[i].Read(in);

      }
    void Reduce(float ratio,int numpts)
      {
        int i;
        for (i=0;i<FluxSetCount;i++)
          fluxes[i].Reduce(ratio,numpts);
      }
    void Write(ocsv *out)
      {
        int i;
        *out << RunCount << NewLn;
        for (i=0;i<RunCount;i++)
          {
            *out << RunInfo[i] << NewLn;
            delete RunInfo[i];
          }
        delete[] RunInfo;
        *out << FluxSetCount << NewLn;
        for (i=0;i<FluxSetCount;i++)
          fluxes[i].Write(out);
        delete[] fluxes;
      }
  };

//---------------------------------------------------------------------------
//#pragma argsused
int main(int argc, char **argv)
{
  icsv *Original;
  ocsv *Reduced;
  WFFData WFFD;
  int maxpt;
  if (argc<3)
    puts("Usage:  WFFTrim.exe <file> <ratio> <points>\n"
         "Where: <file> file to read, reduce and rewrite\n"
         "       <ratio> 0.0001 normal for 99.99% of original mass\n"
         "       <points> maximum points in reduced curve\n");
  else
    {
      if (argc<4)
        {
        puts("\nThe missing argument <maximum points> set to 41\n");
        maxpt = 41;
        }
      else maxpt=ratoi(argv[3]);

      Original=new icsv(argv[1]);
      WFFD.Read(Original);
      delete Original;
      WFFD.Reduce(atof(argv[2]),maxpt);
      Reduced=new ocsv(argv[1]);
      WFFD.Write(Reduced);
      delete Reduced;
    }
  return 0;
}
