
#include "InputSet.h"

//------------------------------------------------------------------------------
// Dictionary
// LastIter,Last iteration for the looping,0,INTEGER,FALSE,TRUE,0,99999,,,TRUE,
// Seed,Seed value for random number generator,0,INTEGER,FALSE,TRUE,-2147483648,2147483647,,,TRUE,

//------------------------------------------------------------------------------
InputSet::InputSet()
{
// public:
//  int Seed;
//  int LastIter;
//  int CurrIter;
//  long hndSeed;
//  long hndIter;
//  long hndAlias;
//  IAliases Ins;
//  OAliases Outs;
}

/*
void InputSet::read(int pid,long dset)
{
  assert(dset>0);
  Ins.read(pid,dset);
  assert(Ins.values.size()>=0);
  Outs.read(pid,dset);
  assert(Outs.values.size()>=0);
  Seed=DataSetReadInt(pid,dset,"Seed","");
  assert(Seed>0);
  LastIter=DataSetReadInt(pid,dset,"LastIter","");
  assert(LastIter>0);
  CurrIter=DataSetReadInt(pid,dset,"CurrIter","");
  assert(CurrIter>0);
  copyToLatin(pid);
}
*/

long InputSet::readSeed(int pid, char *modid)
{
  hndSeed=IconGetUIDataSet(pid,modid,"Seed",1);
  assert(hndSeed>0);
  Seed=DataSetReadInt(pid,hndSeed,"Seed","");
  assert(Seed>0);
  return hndSeed;
}

long InputSet::readIter(int pid, char *modid)
{
  hndIter=IconGetUIDataSet(pid,modid,"Iteration",1);
  assert(hndIter>0);
  LastIter=DataSetReadInt(pid,hndIter,"LastIter","");
  assert(LastIter>0);
  CurrIter=DataSetReadInt(pid,hndIter,"CurrIter","");
  assert(CurrIter>=-1);
  return hndIter;
}

long InputSet::readInputs(int pid, char *modid)
{
  hndAlias=IconGetUIDataSet(pid,modid,"Aliases",1);
  assert(hndAlias>0);
  Ins.read(pid,hndAlias);
  assert(Ins.values.size()>=0);
  return hndAlias;
}

long InputSet::readOutputs(int pid, char *modid)
{
  hndAlias=IconGetUIDataSet(pid,modid,"Aliases",1);
  assert(hndAlias>0);
  Outs.read(pid,hndAlias);
  assert(Outs.values.size()>=0);
  return hndAlias;
}

void InputSet::copyToLatin(int pid)
{
  assert(Ins.values.size()>0);
  LatinInterface.setControl(Seed,LastIter);
  for(unsigned i=0;i<Ins.values.size();i++) {
    //LatinInterface.add(Ins.values[i]);
    Ins.values[i].copyToLatin(pid,hndAlias);
  }
}

void InputSet::performEquations()
{
  for (int i=0;i<LastIter;i++)
  {
    ValueMap t=LatinInterface.loadMap(i);
    for (vector<IAlias>::iterator it=Ins.values.begin();it!=Ins.values.end();++it)
    {
      it->performEquations(t);
    }
//      LatinInterface.getFromMap(t,i);
  }
}

long InputSet::saveOriginalValues(int pid, char *modid)
{
  int idx[6] = {0,0,0,0,0,0};
  long dset = IconGetUIDataSet(pid, modid, "OriginalValues", 1);
  assert(dset>0);
  int i = DataSetDimensionCount(pid,dset,"Variables",idx);
  if (i > 0) return dset;
  int pos=0;
  for(i=0;i<Ins.values.size();i++) {
    pos = Ins.values[i].saveOriginalValue(pid, dset, pos+1);
  }
  return dset;
}

long InputSet::restoreOriginalValues(int pid, char *modid)
{
  long dset = IconGetUIDataSet(pid, modid, "OriginalValues", 1);
  assert(dset>0);
  for(int i=0;i<Ins.values.size();i++) {
    Ins.values[i].restoreOriginalValue(pid,dset);
  }
  return dset;
}


