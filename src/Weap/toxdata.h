#ifndef TOXDATA_H
#define TOXDATA_H

#include "gid.h"

const int MaxPoints = 8;
const int MaxLoc = 5;

class ToxData
{
 public:
  char IDName[SMALLSTRING];
  char Name[SMALLSTRING];
  char Species[SMALLSTRING];
  int NumLC;
  float CCCvalue;
  float AcuteTime;
  float LCconc[MaxPoints];
  float LCdur[MaxPoints];
  char TimeUnits[SMALLSTRING];
  char ConcUnits[SMALLSTRING];
  void Read(fcsv *intox);
  void ReadGID(GIDFILE *f,int site,int con,int lifeidx);
  float GetLCDuration(float conc);
  int CCCcheck(float conc);
  float InterpDuration(float conc,int count);
};

class ConData
{
 public:
  int NumCon;
  ToxData *Con;
  ConData();
  ~ConData();
  void Init(int numcon);
  void Read(char *toxfile);
  void ReadGID(GIDFILE *f,int site,int lifeidx);
  int getindex();
};

#endif
