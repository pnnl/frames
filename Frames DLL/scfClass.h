/*______________________________________________________________________________

   Date:       1993 - 2004
   Company:    Pacific Northwest National Laboratories
               Battelle Corporation
________________________________________________________________________________
__Modifiication  History________________________________________________________

  DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#ifndef scfClassH
#define scfClassH

#include "series.h"

class SCFCon
{
  public:
  char name[SMALLSTRING];
  char cas[SMALLSTRING];
  char tunit[SMALLSTRING];
  char vunit[SMALLSTRING];
  int numStep;
  int numProg;
  fpos_t filepos;
  fpos_t xypos;
  SCFCon **prog;
  Series *xy;

  void Init();
  SCFCon();
  ~SCFCon();
  SCFCon(icsv *inf, bool isProg);
  SCFCon (char *_name, char *_cas, int count);
  /**
    * read in all concentrations to memory.
    */
  void setConcentration(int count, double time, double value);
  void setSeriesCount(int count);
  void setSeriesValue(int idx, double time, double value);
  void scfWrite(ocsv *outf);
  void scfWrite(ocsv *outf, char *parentname, char *parentcas);
  bool add(SCFCon *other);
  void appendProgeny(SCFCon *progeny);
  SCFCon* copy();
};

class SCFSet
{
  public:
  fpos_t filepos;
  char name[SMALLSTRING];
  char type[SMALLSTRING];
  int numCon;
  SCFCon **pCon;

  double x;
  double y;
  double z;

  double cx;
  double cy;
  double cz;

  void Init();
  SCFSet();
  SCFSet(icsv *inf);
  SCFSet(char *lname, char *ltype);
  ~SCFSet();
  void setNumCon(int count);
  void setContaminant(int conIdx, char *cname, char *cas, int progIdx);
  void setSeriesCount(int conIdx, int count);
  void setSeriesValue(int conIdx, int idx, double time, double xy);
  void scfWrite(ocsv *outf);
  bool add(SCFSet *other);
  void append(SCFCon *con);
  SCFSet* copy();
  int convertSCF(icsv *inf, ocsv *eOut, char *casList);
  int unconvertSCF(icsv *inf, ocsv *eOut, char *casList, char *nameList, char *degList, char *secList, int branching);
};

class SCF
{
  public:
  icsv *inf;
  int numSet;
  long scfHead;
  char name[SMALLSTRING];
  SCFSet **set;

  void Init();
  SCF();
  SCF(char *fuiname, char *modId);
  ~SCF();
  void setName(char *lname);
  void setNumSet(int count);
  void setLocation(int dsIdx, char *lname, char *ltype);
  void setNumCon(int dsIdx, int count);
  void setContaminant(int dsIdx, int conIdx, char *cname, char *cas, int nprog);
  void setSeriesCount(int dsIdx, int conIdx, int count);
  void setSeriesValue(int dsIdx, int conIdx, int idx, double time, double value);
  bool add(SCF *other, char *id);
  void append(SCFSet *set);
  void scfWrite(char *tmpname);
};

#endif

