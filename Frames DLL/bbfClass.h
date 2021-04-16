/*______________________________________________________________________________

   Date:       1993 - 2004
   Company:    Pacific Northwest National Laboratories
               Battelle Corporation
________________________________________________________________________________
__Modifiication  History________________________________________________________

  DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#ifndef bbfClassH
#define bbfClassH

#include "csv.h"
#include "series.h"

class BBFCon
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
  BBFCon **prog;

  void Init();
  BBFCon();
  BBFCon(icsv *inf, bool isProg);
  BBFCon (char *_name, char *_cas, int count);
  ~BBFCon();
};

class BBFOrg
{
  public:
  char name[SMALLSTRING];
  char id[SMALLSTRING];
  int numCon;
  fpos_t filepos;
  BBFCon **pCon;

  void Init();
  BBFOrg();
  BBFOrg(icsv *inf);
  ~BBFOrg();
};

class BBFSet
{
  public:
  char name[SMALLSTRING];
  char type[SMALLSTRING];
  fpos_t filepos;
  fpos_t varypos;
  fpos_t certpos;
  int numOrg;
  int numVaryTypes;
  int numCertTypes;
  BBFOrg **pOrg;


  void Init();
  BBFSet();
  BBFSet(icsv *inf);
  ~BBFSet();
};

class BBF
{
  public:
  icsv *inf;
  int numSet;
  long bbfHead;
  BBFSet **set;

  void Init();
  BBF();
  BBF(char *fuiname, char *modId);
  ~BBF();
};

#endif
