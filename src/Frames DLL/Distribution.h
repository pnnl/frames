/*______________________________________________________________________________

   Date:       1993 - 2004
   Company:    Pacific Northwest National Laboratories
               Battelle Corporation
________________________________________________________________________________
__Modifiication  History________________________________________________________

  DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#ifndef distributionH
#define distributionH

#include "gid.h"

//------------------------------------------------------------------------------
class Distribution
{
    void DispTrun(FILE *k);
    void DispUnif(FILE *k);
    void DispLU(FILE *k);
    void DispNorm(FILE *k);
    void DispLN(FILE *k);
    void DispExp(FILE *k);
    void DispTri(FILE *k);
    void DispGam(FILE *k);
    void DispBet(FILE *k);
    void DispWei(FILE *k);
    void DispLog(FILE *k);

  public:
    int Base;
    int CorNum;
    double Mean;
    double Std;
    double Scale;
    double Shift;
    double Mode;
    double *Cor;
    char *Alias;
    char *Des;
    char *Exe;
    char *Fmt;
    char *Glyph;
    char *Type;
    char *Upper;
    char *Lower;
    char *Equation;
    char *Units;
    char **CorAlias;
    paramrec p;

    int HasDist();
    int HasEquation();
    int writeCor(FILE *k, Distribution *var, int idx);
    void writeSUFLabel(fcsv *f);
    void writeSUFValue(fcsv *f);
    int read(GIDFILE *g, int count);
    int write(FILE *k, int i);
    Distribution();
    ~Distribution();
};
//---------------------------------------------------------------------------
#endif
