/*______________________________________________________________________________

   Date:       1993 - 2004
   Company:    Pacific Northwest National Laboratories
               Battelle Corporation
________________________________________________________________________________
__Modifiication  History________________________________________________________

  DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#ifndef results_h
#define results_h

#include "csv.h"
#include "gid.h"


//------------------------------------------------------------------------------
class Result
{
    int kind;
    bool firstIteration;

  public:
    // variables attribute keys keys
    char *Alias;
    char *Des;
    char *Type;
    char *Time;
    char *Source;     // aka the producer of the file (modid)
    char *CASID;
    char *ParentID;
    char *OrgID;      // doubles as organism, effect endpoint, affected organs
    int NumYears;
    double *Years;

    // monitored output
    int NumValues;
    double *Values;
    char **Labels;

    // dataset info
    char *ext;
    char *qual;
    char *consumer;
    char *producer;

    // timeseries
    int num;
    double *dvalue;
    double *dtime;
    char *xunit;
    char *yunit;

    // allows for multiple datasets
    Result *home;
    Result *next;

    // class methods
    int read(GIDFILE *g,int count);

    int ProcessPDCF(char *fuiname);

    // Readset for scf,wff,aff,wcf all qualifier types
    bool CFReadSet(icsv *inf);
    bool CFReadSeries(icsv *inf, int ntime, int validx, char *xunt, char *yunt);
    // Readset for ato: all qualifier
    bool ATOReadSet(icsv *inf);
    // Readset for epf: exposure pathway concentration
    bool EPFReadSet(icsv *inf);
    double EPFReadPath(icsv *inf, int npt, int npath);
    // Readset for rif: receptor intakes
    Result *RIFReadSet(icsv *inf);
    double RIFReadPath(icsv *inf, int npt, int npath);
    // Readset for hif: health impacts
    Result *HIFReadSet(icsv *inf);
    double HIFReadPath(icsv *inf, int npt, int npath, int epindex);
    // Readset for twi: Terrestrial Wildlife Intakes
    bool TWIReadSet(GIDFILE *GID, int setidx);

    void computeResults();
    void writeSUFLabels(fcsv *f);
    void writeSUFValues(fcsv *f);

    Result();
    Result(Result *);
    ~Result();
    void reset();
};

#endif