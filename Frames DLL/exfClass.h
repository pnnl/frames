/*______________________________________________________________________________

   Date:       1993 - 2004
   Company:    Pacific Northwest National Laboratories
               Battelle Corporation
________________________________________________________________________________
__Modifiication  History________________________________________________________

  DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#ifndef exfClassH
#define exfClassH

#include "csv.h"
#include "series.h"

class EXFDes
{
  public:
  char vlbl[LARGESTRING];
  char tunit[SMALLSTRING];
  char vunit[SMALLSTRING];
  int numStep;
  fpos_t filepos;
  fpos_t xypos;

  void Init();
  EXFDes(icsv *inf);
  ~EXFDes();
};

class EXFCon
{
  public:
  char name[SMALLSTRING];
  char cas[SMALLSTRING];
  int numDes;
  fpos_t filepos;
  EXFDes **des;

  void Init();
  EXFCon();
  EXFCon(icsv *inf);
  ~EXFCon();
};

class EXFOrg  // doubles as orgin (location) or organism for HQ
{
  public:
  char name[SMALLSTRING];
  char id[SMALLSTRING];
  int numCon;
  fpos_t filepos;
  EXFCon **pCon;

  void Init();
  EXFOrg();
  EXFOrg(icsv *inf);
  ~EXFOrg();
};

class EXFSet
{
  public:
  char name[SMALLSTRING];
  char type[SMALLSTRING];
  fpos_t filepos;
  int numOrg;
  EXFOrg **pOrg;

  void Init();
  EXFSet();
  EXFSet(icsv *inf);
  ~EXFSet();
};

class EXF
{
  public:
  icsv *inf;
  int numSet;
  long exfHead;
  EXFSet **set;

  void Init();
  EXF();
  EXF(char *fuiname, char *modId);
  ~EXF();
};

#endif

