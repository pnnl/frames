/*______________________________________________________________________________

   Date:       1993 - 2004
   Company:    Pacific Northwest National Laboratories
               Battelle Corporation
________________________________________________________________________________
__Modifiication  History________________________________________________________

  DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#ifndef wcfClassH
#define wcfClassH

#include "series.h"

class WCFCon
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
  WCFCon **prog;
  Series *xy;

  void Init();
  WCFCon();
  ~WCFCon();
  WCFCon(icsv *inf, bool isProg);
  WCFCon(char *_name, char *_cas, int count);
   /*\
    * read in all concentrations to memory.
   \*/
  void setConcentration(int count, double time, double value);
  void setSeriesCount(int count);
  void setSeriesValue(int idx, double time, double value);
  void wcfWrite(ocsv *outf);
  void wcfWrite(ocsv *outf, char *parentname, char *parentcas);
  bool add(WCFCon *other);
  void appendProgeny(WCFCon *progeny);
  WCFCon* copy();
};

class WCFSet
{
  public:
  fpos_t filepos;
  char name[SMALLSTRING];
  char type[SMALLSTRING];
  int numCon;
  WCFCon **pCon;

  double x;
  double y;
  double z;

  void Init();
  ~WCFSet();
  WCFSet();
  WCFSet(icsv *inf);
  WCFSet(char *lname, char *ltype);
  void setNumCon(int numCon);
  void setContaminant(int conIdx, char *name, char *cas, int progIdx);
  void setSeriesCount(int conIdx, int count);
  void setSeriesValue(int conIdx, int idx, double time, double value);
  bool add(WCFSet *other);
  void append(WCFCon *con);
  void wcfWrite(ocsv *outf);
  WCFSet* copy();
  int convertWCF(icsv *inf, ocsv *eOut, char *casList);
  int unconvertWCF(icsv *inf, ocsv *eOut, char *casList, char *nameList, char *degList, char *secList, int branching);
};

class WCF
{
  public:
  icsv *inf;
  int numSet;
  long wcfHead;
  char name[SMALLSTRING];
  WCFSet **set;

  void Init();
  WCF();
  WCF(char *fuiname, char *modId);
  ~WCF();
  void setName(char *lname);
  void setNumSet(int count);
  void setLocation(int dsIdx, char *lname, char *ltype);
  void setNumCon(int dsIdx, int count);
  void setContaminant(int dsIdx, int conIdx, char *name, char *cas, int progIdx);
  void setSeriesCount(int dsIdx, int conIdx, int count);
  void setSeriesValue(int dsIdx, int conIdx, int idx, double time, double value);
  bool add(WCF *other, char *id);
  void append(WCFSet *set);
  void wcfWrite(char *tmpname);
};

#endif

