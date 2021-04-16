#ifndef ITERATOR_H
#define ITERATOR_H

#include <vector>
#include <string>
#include <map>
#include "FramesLib.h"
#include "Iterator.h"
#include "Assertions.h"
#include "OutputDataSet.h"

using namespace std;

// Dictionary
// Indices,,3,STRING,FALSE,TRUE,0,0,,,FALSE,,IteratorConf.SelectedMod,IteratorConf.SelectedDictionary,IteratorConf.Variable
// SelectedDataSet,,2,STRING,FALSE,FALSE,0,0,,,FALSE,,IteratorConf.SelectedMod
// SelectedDictionary,,2,STRING,FALSE,FALSE,0,0,,,FALSE,,IteratorConf.SelectedMod
// SelectedMod,,1,STRING,FALSE,FALSE,0,0,,,FALSE,
// Step,,3,STRING,FALSE,TRUE,0,0,,,FALSE,,IteratorConf.SelectedMod,IteratorConf.SelectedDictionary,IteratorConf.Variable
// Value,,3,STRING,FALSE,TRUE,0,0,,,FALSE,,IteratorConf.SelectedMod,IteratorConf.SelectedDictionary,IteratorConf.Variable
// Variable,,3,STRING,FALSE,FALSE,0,0,,,FALSE,,IteratorConf.SelectedMod,IteratorConf.SelectedDictionary
// VarType,,3,STRING,FALSE,TRUE,0,0,,,FALSE,,IteratorConf.SelectedMod,IteratorConf.SelectedDictionary,IteratorConf.Variable


class Module
{
  public:
  string mclass;
  string modid;
  string mpath;
  string margs;
  long handle;
  bool isaSampler;
  bool isaSummarizer;
  bool isAffected;
  bool Init(int pid, char *name, long h);
};

class Iterator
{
  public:

  int Seed;
  int LastIter;
  int CurrIter;

  long hndSeed;
  long hndIter;
  long hndConf;
  long handle;

  char *getDictionaryName() { return "IteratorConf"; }
  int Init(int pid, long h);
  bool Run();
  void RunIconModel();
  void RunSamplers();
  void RunSummarizers();
  void RunAffectedModules();

};

#endif