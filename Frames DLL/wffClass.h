/*______________________________________________________________________________

   Date:       1993 - 2004
   Company:    Pacific Northwest National Laboratories
               Battelle Corporation
________________________________________________________________________________
__Modifiication  History________________________________________________________

  DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#ifndef wffClassH
#define wffClassH

#include "series.h"

class WFFCon
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
  WFFCon **prog;

  int numXYSeries;
  Series *xy;
  Series *xy2;

  void Init();
  WFFCon();
  ~WFFCon();
  WFFCon(icsv *inf, bool isProg);
  WFFCon (char *_name, char *_cas, int count);
   /*\
    * read in all concentrations to memory.
   \*/
  void wffWrite(ocsv *outf);
  void wffWrite(ocsv *outf, char *parentname, char *parentcas);
  bool add(WFFCon *other, Series *fluxes1, Series *fluxes2, Series *flux);
  void appendProgeny(WFFCon *progeny);
  WFFCon *copy();
};
               
class WFFSet
{
  public:
  char name[SMALLSTRING];
  char type[SMALLSTRING];
  fpos_t filepos;
  int numCon;
  WFFCon **pCon;

  double width;
  double length;
  double distance;
  double recharge;
  fpos_t fluxpos;
  int numFlux;
  Series *flux;

  void Init();
  WFFSet();
  WFFSet(icsv *inf);
  WFFSet(char *lname, char *ltype);
  ~WFFSet();

  bool add(WFFSet *other);
  void append(WFFCon *con);
  void wffWrite(ocsv *outf);
  WFFSet *copy();
  int convertWFF(icsv *inf, ocsv *eOut, char *casList);
  int unconvertWFF(icsv *inf, ocsv *eOut, char *casList, char *nameList, char *degList, char *secList, int branching);
};

class WFF
{
  public:
  icsv *inf;
  int numSet;
  long wffHead;
  char name[SMALLSTRING];
  WFFSet **set;

  void Init();
  WFF();
  WFF(char *fuiname, char *modId);
  ~WFF();
  void setName(char *lname);
  bool add(WFF *other, char *id);
  void append(WFFSet *media);
  void wffWrite(char *tmpname);
};

#endif
