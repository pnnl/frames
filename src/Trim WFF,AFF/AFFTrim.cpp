#pragma hdrstop
#include <condefs.h>
#include <iostream.h>

#include "csv.h"
#include "ReduceAFF.h"

//---------------------------------------------------------------------------
USEUNIT("..\Common Files\robust.cpp");
USEUNIT("..\Common Files\csv.cpp");
USEUNIT("ReduceAFF.cpp");
USERES("AFFTrim.res");
//---------------------------------------------------------------------------
class AFFProg
  {
    char *Name,*Id,*Units,*ParentName,*ParentID;
    int NumFlux,NumTypes;
    float *Times;
    float **Fluxes;
/*
         For each progeny
         Line with: Progeny Name-(string)
           Progeny ID-(string)
           Media Time units-"yr"
           Flux units-"pCi/y" or "g/y"
           Number of fluxes-(integer)
        Parent Name-(string)
          Parent ID-(string)
          For each flux
            Line with: Media Time-(float)
              Flux for 1st flux entry-(float)
              Flux for 2nd flux entry-(float)
              Flux for 3rd flux entry-(float)
              Flux for 4th flux entry-(float)
*/
    public:
    void Read(icsv *in,int NumFluxTypes)
      {
        int i,j;
        char line[256];
        NumTypes=NumFluxTypes;
        *in >> line; Name=strdup(line);
        *in >> line; Id=strdup(line);
        *in >> line;
        *in >> line; Units=strdup(line);
        cout << "\tReading " << Name << "(" << Id << ")\n";
        *in >> NumFlux ;
        *in >> line; ParentName=strdup(line);
        *in >> line; ParentID=strdup(line);
        *in >> NewLn;
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
    void Reduce(float ratio)
      {
        int Before;
        Before=NumFlux;
        cout << "\tReducing " << Name << "(" << Id << "):";
        ::Reduce(NumTypes,&NumFlux,&Times,&Fluxes,ratio);
        cout << " to " << ((float)NumFlux*100.0/(float)Before) << "% of original size\n";
      }
    void Write(ocsv *out)
      {
        int i,j;
        cout << "\tWriting " << Name << "(" << Id << ")\n";
        *out << Name << Id << "yr" << Units << NumFlux << ParentName << ParentID << NewLn;
        delete Name;
        delete Id;
        delete ParentName;
        delete ParentID;
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
      }
  };

class AFFCon
  {
/*
        Line with: Constituent Name-(string)
          Constituent ID-(string)
          Media Time units-"yr"
          Flux units-"pCi/yr" or "g/yr"
          Number of fluxes-(integer)
          Number of progeny-(integer)
        For each flux
          Line with: Media Time-(float)
            Flux for 1st flux entry-(float)
            Flux for 2nd flux entry-(float)
            Flux for 3rd flux entry-(float)
            Flux for 4th flux entry-(float)
*/
    char *Name,*Id,*Units;
    int NumFlux,NumProg,NumTypes;
    float *Times;
    float **Fluxes;
    AFFProg *Prog;
    public:
    void Read(icsv *in,int NumFluxTypes)
      {
        int i,j;
        char line[256];
        NumTypes=NumFluxTypes;
        *in >> line; Name=strdup(line);
        *in >> line; Id=strdup(line);
        *in >> line;
        *in >> line; Units=strdup(line);
        cout << "Reading " << Name << "(" << Id << ")\n";
        *in >> NumFlux >> NumProg >> NewLn;
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
        Prog=new AFFProg[NumProg];
        for (i=0;i<NumProg;i++)
          Prog[i].Read(in,NumFluxTypes);
      }
    void Reduce(float ratio)
      {
        int i,Before;
        Before=NumFlux;
        cout << "Reducing " << Name << "(" << Id << "):";
        ::Reduce(NumTypes,&NumFlux,&Times,&Fluxes,ratio);
        cout << " to " << ((float)NumFlux*100.0/(float)Before) << "% of original size\n";
        for (i=0;i<NumProg;i++)
          Prog[i].Reduce(ratio);
      }
    void Write(ocsv *out)
      {
        int i,j;
        *out << Name << Id << "yr" << Units << NumFlux << NumProg << NewLn;
        cout << "Writing " << Name << "(" << Id << ")\n";
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

class AFFFluxSet
  {
/*
    Line with:Name of flux rate set-(string)
    Line with:Air Source Type Name-(string)
      ;If Stack, vent, etc.     "POINT"
      ;If Landfill, pond, etc.   "AREA"
    Line with:Exit Area of Source-(float)
      Units-"m2"
    Line with:Exit Height-(float)
      ;If AREA fixed value = 0
      Units-"m"
    Line with:Adjacent Structure Height-(float)
      ;If AREA, fixed value = 0
      Units-"m"
    Line with:Exit Velocity-(float)
      ;If AREA, fixed value = 0
      Units-"m/s"
    Line with:Exit Temperature-(float)
      Units-"Deg C"
    Line with:Ambient Air Temperature-(float)
      Units-"Deg C"
    Line with:Number of Flux Types-(integer)
    For each Flux type
      Line with: Flux Type Name-(string)
        ; If Gas "Gas 1"
        ; If particle 1 used "Particle 1
        ; If particle 2 used "Particle 2"
        ; If particle 3 used "Particle 3"
      Flux Type Parameter-(float)
        ; If Gas, not defined  (=0.0)
        ; If Particle, radius   (mm)
      Units-"um"
      Flux Type Density-(float)
      Units - "g/cm3"
    Line with: Number., of constituents-(integer)
      For each constituent
*/
    char *Name,*Type;
    float Area,Height,ASHeight,ExitVel,ExitTemp,AmbientAirTemp;
    int NumFluxTypes;
    char **Names;
    float *Radius;
    float *Density;
    char **RadiusUOM;
    char **DensityUOM;
    int NumCon;
    AFFCon *ConFlux;
    public:
    void Read(icsv *in)
      {
        int i;
        char line[256],dummy[256];
        *in >> line >> NewLn; Name=strdup(line);
        *in >> line >> NewLn; Type=strdup(line);
        *in >> Area >> NewLn;
        *in >> Height >> NewLn;
        *in >> ASHeight >> NewLn;
        *in >> ExitVel >> NewLn;
        *in >> ExitTemp >> NewLn;
        *in >> AmbientAirTemp >> NewLn;
        *in >> NumFluxTypes >> NewLn;
        Names=new char*[NumFluxTypes];
        Radius=new float[NumFluxTypes];
        Density=new float[NumFluxTypes];
        RadiusUOM=new char*[NumFluxTypes];
        DensityUOM=new char*[NumFluxTypes];
        for (i=0;i<NumFluxTypes;i++)
          {
            *in >> line;                        Names[i]=strdup(line);
            *in >> Radius[i] >> dummy;          RadiusUOM[i]=strdup(dummy);
            *in >> Density[i] >> dummy;         DensityUOM[i]=strdup(dummy);
            *in >> NewLn;
          }
        *in >> NumCon >> NewLn;
        ConFlux=new AFFCon[NumCon];
        for (i=0;i<NumCon;i++)
          ConFlux[i].Read(in,NumFluxTypes);
      }
    void Reduce(float ratio)
      {
        int i;
        for (i=0;i< NumCon; i++)
          ConFlux[i].Reduce(ratio);
      }
    void Write(ocsv *out)
      {
        int i;
        *out << Name << NewLn; delete Name;
        *out << Type << NewLn; delete Type;
        *out << Area << NewLn;
        *out << Height << "m2" << NewLn;
        *out << ASHeight << "m" << NewLn;
        *out << ExitVel << "m/s" << NewLn;
        *out << ExitTemp << "Deg C" << NewLn;
        *out << AmbientAirTemp << "Deg C" << NewLn;
        *out << NumFluxTypes << NewLn;
        for (i=0;i<NumFluxTypes;i++)
          {
            *out << Names[i] << Radius[i] << RadiusUOM[i];
            *out << Density[i] << DensityUOM[i] << NewLn;
            delete Names[i];
            delete RadiusUOM[i];
            delete DensityUOM[i];
          }
        delete[] Names;
        delete[] Radius;
        delete[] Density;
        delete[] DensityUOM;
        delete[] RadiusUOM;
        *out << NumCon << NewLn;
        for (i=0;i<NumCon;i++)
          ConFlux[i].Write(out);
        delete[] ConFlux;
      }
  };

class AFFData
  {
/*
Line with:Number of Lines of Header Information-(integer)
  For each
    Line with: Run information-(string)
Line with: Number of Air flux rate sets
    For each Flux rate set
*/
    int RunCount;
    char **RunInfo;
    int FluxSetCount;
    AFFFluxSet *fluxes;
    public:
    void Read(icsv *in)
      {
        char line[256];
        int i;
        *in >> RunCount >> NewLn;
        RunInfo=new char*[RunCount];
        for (i=0;i<RunCount;i++)
          {
            *in >> line >> NewLn;
            RunInfo[i]=strdup(line);
          }
        *in >> FluxSetCount >> NewLn;
        fluxes=new AFFFluxSet[FluxSetCount];
        for (i=0;i<FluxSetCount;i++)
          fluxes[i].Read(in);
      }
    void Reduce(float ratio)
      {
        int i;
        for (i=0;i<FluxSetCount;i++)
          fluxes[i].Reduce(ratio);
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
  AFFData AFFD;
  if (argc<3)
    cout << "Usage:  AFFTrim.exe <file> <ratio>\n"
            "Where: <file> file to read, reduce and rewrite\n"
            "       <ratio> 0.0001 normal for 99.99% of original mass\n";
  else
    {
      Original=new icsv(argv[1]);
      AFFD.Read(Original);
      delete Original;
      AFFD.Reduce(atof(argv[2]));
      Reduced=new ocsv(argv[1]);
      AFFD.Write(Reduced);
      delete Reduced;
    }
  return 0;
}
