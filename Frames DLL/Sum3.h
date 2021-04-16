//------------------------------------------------------------------------------
#ifndef sum3_h
#define sum3_h

#include "sumdata.h"

//------------------------------------------------------------------------------
class SUM
{
  private:
    int Ok;
    int Current;
    int Count;
    int SiteIdx;
    int ModIdx;
    char ModId[SMALLSTRING];
    char AppPath[MAXPATH];
    char DoAllPath[MAXPATH];
    char FUIName[MAXPATH];
    char RunName[MAXPATH];
    char GIDName[MAXPATH];
    char SUFName[MAXPATH];
    char sensin[MAXPATH];   // ="~sensi.gid";
    char sensout[MAXPATH];  // ="~senso.*";
    char workdir[MAXPATH];

    GIDFILE *GID;
    SUMData *Data;
    fcsv SUF;

    int LaunchProcess(char *cmdline, char *apppath);
    int writeId();
    int writeKey();
    int writeGID();
    int spawnLatin();
    int spawnDoAll();

  public:
    int Initialize(char *argv0, char *argv1, char *argv2, char *argv3, char *argv4, char *argv5);
    int Iterate(int iteration);
    int Finalize();
};

#endif
