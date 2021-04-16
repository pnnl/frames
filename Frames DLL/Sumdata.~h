//------------------------------------------------------------------------------
#ifndef sumdata_h
#define sumdata_h

#include "distribution.h"
#include "results.h"

//------------------------------------------------------------------------------
class SUMData
{
  public:
    double Seed;
    int Iteration;
    int VarCount;
    Distribution *Vars;
    int ResCount;
    Result *Res;
    paramrec *Fui;

    int read(GIDFILE *gid,int SiteIdx);
    int writeKey(FILE *k);
    int writeCorMat(FILE *k);
    int readLHO(char *name,int current);
    int readResults(char *fuiname);
    int writeTempGID(char *sname,char *dname);
    int writeSUF(fcsv *f,int current,int total, bool errfile);
    void cleanup();
    ~SUMData();
};
//------------------------------------------------------------------------------
#endif
