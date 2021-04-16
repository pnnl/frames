#ifndef CPPDLL_H
#define CPPDLL_H

#ifdef __cplusplus
extern "C" {
#endif

void __declspec(dllimport) _ChemSSF(char *fname);
// This function creates a HWIRIO SSF file at the designated path

void __declspec(dllimport) _ChemDebug(int state);
// This function turns debugging on and off in the hwircp.dll

void __declspec(dllimport) _ChemPath(char *path);
// This function set the path the chemical properties data table will be read from


//---------------------------------------------------------------------------
void __declspec(dllimport) _ChemEnv(double Temperature,double pH,char *Media,double FOC);
//	This function initializes the environmental parameters for the Chemical
// Property Processor DLL.  If the pH is less than 6 acidic conditions will be
// assumed. A pHof greater than 8 is assumed to be basic.  Between 6 and 8
// inclusively will be assumed neutral conditions.  For functions that need to
// make assumptions about the acidity, alkalinity, and neutral conditions of
// the media.
//		Media is one of "Soil","Sediment", or "Surface Water"

void __declspec(dllimport) _ChemCASID(char *CASID);
//	This function initializes the CPP to retreive values for a selected chemical.

int __declspec(dllimport) _NumChem();
//	This function returns the integer value for the number of valid chemicals
// that the CPP has properties for.  If a chemical is missing part of its
// required data it will not be included in this count.

void __declspec(dllimport) _ChemInfo(int Index,char *Name,char *CASID,int *ChemType);
//	This subroutine is used in conjunction with NumChem() to create a list of
// the available chemicals in the database the CPP.DLL is reading.   ChemType
// is set to an integer value which represents the type of the chemical.
// 1=Organic and 2=Metal/Inorganic

int __declspec(dllimport) _ChemNumSolCw();
//	This functions return the number of Solid Waste Cws Specified
int __declspec(dllimport) _ChemNumLiqCw();
//	This functions return the number of Liquid Waste Cws Specified
double __declspec(dllimport) _ChemSolCw(int Index);
//	This functions return the Index'th Solid Waste Cw in ug/g
double __declspec(dllimport) _ChemLiqCw(int Index);
//	This functions return the Index'th Liquid Waste Cw in mg/L

void __declspec(dllimport) _ChemSMILES(char *SMILES);
//	This function returns the SMILES string for Organic Chemcials

double __declspec(dllimport) _ChemMolWt();
//	This functions returns the moleculare weight for Organic and Inorganic chemicals
// in g/mole

double __declspec(dllimport) _ChemADiff();
//	This functions computes the Air Diffusion coefficient for Organic Chemcials
// in cm^2/s

double __declspec(dllimport) _ChemVol();
//	This functions computes the Volume for Organic Chemcials in mL

double __declspec(dllimport) _ChemDen();
//	This functions computes the density for Organic Chemcials in g/mL

double __declspec(dllimport) _ChemWDiff();
//	This functions computes the Water Diffusion coefficient for Organic
// Chemcials in cm^2/s

double __declspec(dllimport) _ChemVP();
//	This functions computes the Vapor Pressure for Organic Chemcials in torr

double __declspec(dllimport) _ChemSol();
//	This functions computes the Solubility Limit for Organic Chemcials in mg/L.
//  It samples from a distribution for metals and inorganics.

double __declspec(dllimport) _ChemHLC();
//	This functions computes the Henry's Law Constant for Organic Chemcials in
// (atm m^3 / mol).

double __declspec(dllimport) _ChemKow();
//	This functions computes the Kow for Organic Chemcials in {?? mL/mL ??}

double __declspec(dllimport) _ChemKoc();
//	This functions computes the Koc for Organic Chemcials in {?? mL/g ??}

double __declspec(dllimport) _ChemKDoc();
//	This functions computes the Koc for Organic Chemcials in {?? mL/g ??}

double __declspec(dllimport) _ChemKd();
//	This functions computes the partition coefficient for Metals/Inorganic
// Chemcials in L/kg.

void __declspec(dllimport) _ChemHyd(double *Rate,int *NumProd);
//	This subroutine returns the catalyzed hydrolysis rate constant(Rate) and
// number of reaction products(NumProd).  The determination of Acid, Neutral or
// Base conditions is made from the value set for pH.  See description of
// ChemEnv.

void __declspec(dllimport) _ChemPHyd(int Index,char *Name,char *PCASID,double *YCoef);
//	This subroutine returns data about a particular product that is associated
// with the catalyzed hydrolysis rate constant. The name(Name), product
// CAS ID(PCASID), and molar yield coefficient for reaction(YCoef) are returned.
//   The determination of Acid, Neutral or Base conditions is made from the
// value set for pH.  See description of ChemEnv.

void __declspec(dllimport) _ChemAerBio(double *Rate,int *NumProd);
//	This subroutine returns the aerobic biodegradation rate constant (Rate) and
// number of reaction products(NumProd).

void __declspec(dllimport) _ChemPAerBio(int Index,char *Name,char *PCASID,double *YCoef);
//	This subroutine returns data about a particular product that is associated
// with the aerobic biodegradation rate constant. The name(Name), product
// CAS ID(PCASID), and molar yield coefficient for reaction(YCoef) are returned.

void __declspec(dllimport) _ChemActBio(double *Rate,int *NumProd);
//  This subroutine returns the activated biodegradation rate constant(Rate) and
//  number of reaction products(NumProd).

void __declspec(dllimport) _ChemPActBio(int Index,char *Name,char *PCASID,double *YCoef);
//	This subroutine returns data about a particular product that is associated
// with the activated biodegradation rate constant. The name(Name), product
// CAS ID(PCASID), and molar yield coefficient for reaction(YCoef) are returned.

void __declspec(dllimport) _ChemAnaRed(double *Rate,int *NumProd);
//	This subroutine returns the anaerobic reduction rate constant(Rate) and
// number of reaction products(NumProd).

void __declspec(dllimport) _ChemPAnaRed(int Index,char *Name,char *PCASID,double *YCoef);
//	This subroutine returns data about a particular product that is associated
// with the anaerobic reduction rate constant. The name(Name), product
// CAS ID(PCASID), and molar yield coefficient for reaction(YCoef) are returned.

void __declspec(dllimport) _ChemAnaBio(double *Rate,int *NumProd);
//	This subroutine returns the anaerobic biodegradation rate constant(Rate) and
// number of reaction products(NumProd).  The determination of Acid, Neutral or
// Base conditions is made from the value set for pH.  See description of
// ChemEnv.

void __declspec(dllimport) _ChemPAnaBio(int Index,char *Name,char *PCASID,double *YCoef);
// 	This subroutine returns data about a particular product that is associated
// with the anaerobic biodegradation rate constant. The name(Name), product
// CAS ID(PCASID), and molar yield coefficient for reaction(YCoef) are returned.
//   The determination of Acid, Neutral or Base conditions is made from the
// value set for pH.  See description of ChemEnv.

void __declspec(dllimport) _ChemSO4Bio(double *Rate,int *NumProd);
//  This subroutine returns the SO4 reducing biodegradation rate constant(Rate)
// and number of reaction products(NumProd).  The determination of Acid, Neutral
//  or Base conditions is made from the value set for pH.  See description of
// ChemEnv.

void __declspec(dllimport) _ChemPSO4Bio(int Index,char *Name,char *PCASID,double *YCoef);
//  This subroutine returns data about a particular product that is associated
// with the SO4 reducing biodegradation rate constant. The name(Name), product
// CAS ID(PCASID), and molar yield coefficient for reaction(YCoef) are returned.
//   The determination of Acid, Neutral or Base conditions is made from the
// value set for pH.  See description of ChemEnv.

void __declspec(dllimport) _ChemMetBio(double *Rate,int *NumProd);
//	This subroutine returns the methanogenic biodegradation rate constant(Rate)
// and number of reaction products(NumProd).  The determination of Acid, Neutral
//  or Base conditions is made from the value set for pH.  See description of
// ChemEnv.

void __declspec(dllimport) _ChemPMetBio(int Index,char *Name,char *PCASID,double *YCoef);
//	This subroutine returns data about a particular product that is associated
// with the methanogenic biodegradation rate constant. The name(Name), product
// CAS ID(PCASID), and molar yield coefficient for reaction(YCoef) are returned.
//    The determination of Acid, Neutral or Base conditions is made from the
// value set for pH.  See description of ChemEnv.

int __declspec(dllimport) _ChemHum();
//	This subroutine returns the human health benchmarks for the given CASID.
int __declspec(dllimport) _ChemEco();
//	This subroutine returns the ecological benchmarks for the given CASID.

int __declspec(dllimport) _ChemEB(char *Species,double *Sed,double *Soil,double *WaterDis,double *WaterTot,double *EBRec);
//  This function will return the ecological benchmarks for mamals.  If the
// variable Mamal does not contain a recognized mamal the function will return
// .false. In FORTRAN and 0 in C/C++.
//		Weight  is one of -1=low, -2=medium, and -3=high

int __declspec(dllimport) _ChemSoilTo(char *Species,double *BCF);
//	This function will return the biological accumulation and concentration
// factor for soil to the defined species.  If the variable Species does not
// contain a recognized Species the function will return .false. In FORTRAN and
// 0 in C/C++.
//		Selected from	"exveg", "proveg", "exfruit", "profruit" ,"root", "grain" ,
// "silage", "forage", "earthworm", "inverterbrate", "small mammal",
// "other vertebrate"

int __declspec(dllimport) _ChemAirTo(char *Plant,double *BTF);
//	This function will return the biological accumulation and concentration
// factor for air to the defined plant.  If the variable Plant does not
// contain a recognized Plant the function will return .false. In FORTRAN
// and 0 in C/C++.
//		Selected from	"exveg", "exfruit", "silage", "forage"

double __declspec(dllimport) _ChemRCF();
//	This function will return the root concentration factor for soil to the
// root.

double __declspec(dllimport) _ChemBs();
//  This function will return the bioavailablility fraction for contaminanted
// soil.

int __declspec(dllimport) _ChemBTF(char *Medium,double *Ba);
//	This function will return the biotransfer factor for the given medium.  If
// the variable Medium does not contain a recognized Medium the function will
//  return .false. In FORTRAN and 0 in C/C++.
//		Selected from "milk","beef","water"

double __declspec(dllimport) _ChemMT();
//  This function will return the metabolic transformation rate.

int __declspec(dllimport) _ChemABF(char *Species,double *BCF);
//	This function will returns the aquatic bioconcentration factor for the given
//  species.  If the variable species does not contain a recognized species the
// function will return .false. In FORTRAN and 0 in C/C++.
//		Selected from	 "plant", "finfish", "shellfish", "phytoplankton",
// "zooplankton", "benthos category 1", "benthos category 2", "invertebrates",
// "T3 finfish","T4 finfish"
#ifdef __cplusplus
}
#endif

#define ChemEnv _ChemEnv
#define ChemCASID _ChemCASID
#define NumChem _NumChem
#define ChemNumSolCw _ChemNumSolCw
#define ChemNumLiqCw _ChemNumLiqCw
#define ChemSolCw _ChemSolCw
#define ChemLiqCw _ChemLiqCw
#define ChemInfo _ChemInfo
#define ChemSMILES _ChemSMILES
#define ChemMolWt _ChemMolWt
#define ChemADiff _ChemADiff
#define ChemVol _ChemVol
#define ChemDen _ChemDen
#define ChemWDiff _ChemWDiff
#define ChemVP _ChemVP
#define ChemSol _ChemSol
#define ChemHLC _ChemHLC
#define ChemKow _ChemKow
#define ChemKoc _ChemKoc
#define ChemKDoc _ChemKDoc
#define ChemKd _ChemKd
#define ChemHyd _ChemHyd
#define ChemPHyd _ChemPHyd
#define ChemAerBio _ChemAerBio
#define ChemPAerBio _ChemPAerBio
#define ChemActBio _ChemActBio
#define ChemPActBio _ChemPActBio
#define ChemAnaRed _ChemAnaRed
#define ChemPAnaRed _ChemPAnaRed
#define ChemAnaBio _ChemAnaBio
#define ChemPAnaBio _ChemPAnaBio
#define ChemSO4Bio _ChemSO4Bio
#define ChemPSO4Bio _ChemPSO4Bio
#define ChemMetBio _ChemMetBio
#define ChemPMetBio _ChemPMetBio
#define ChemHum _ChemHum
#define ChemEco _ChemEco
#define ChemEB _ChemEB
#define ChemSoilTo _ChemSoilTo
#define ChemAirTo _ChemAirTo
#define ChemRCF _ChemRCF
#define ChemBs _ChemBs
#define ChemBTF _ChemBTF
#define ChemMT _ChemMT
#define ChemABF _ChemABF
#define ChemPath _ChemPath
#define ChemSSF _ChemSSF
#define ChemDebug _ChemDebug

#endif

