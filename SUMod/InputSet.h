#ifndef INPUTSET_H
#define INPUTSET_H

#include <iostream>

#include "FramesLib.h"
#include "Assertions.h";
#include "Aliases.h"

//#include "Equation.h"
//#include "sumdata2.h"

using namespace std;


class InputSet
{
 public:
  int Seed;
  int LastIter;
  int CurrIter;
  long hndSeed;
  long hndIter;
  long hndAlias;
  IAliases Ins;
  OAliases Outs;

  InputSet();
  // void read(int pid,long dset);
  long readSeed(int pid, char *modid);
  long readIter(int pid, char *modid);
  long readInputs(int pid, char *modid);
  long readOutputs(int pid, char *modid);
  void copyToLatin(int pid);
  void performEquations();
  long saveOriginalValues(pid, char *modid);
  long restoreOriginalValues(pid, char *modid);
};

#endif

