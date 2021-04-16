#ifndef OUTPUTDATASET_H
#define OUTPUTDATASET_H

#include "FramesLib.h"
#include "sumdata2.h"
#include "InputSet.h"

class OutputDataSet
{
  protected:
  virtual char* getDictionaryName()=0;
  virtual void write(int pid,long odset)=0;
  public:
  long getDataSetHandle(int pid, char *modid);
  long createOutput(int pid,char *modid);
};

class SampledValuesDataset: public OutputDataSet
{
  virtual char *getDictionaryName() { return "SampledValues"; }
  virtual void write(int pid,long odset) {LatinInterface.writeSampledOutputs(pid,odset);}
  virtual void read(int pid,long odset,int iteration) {LatinInterface.readSampledValues(pid,odset,iteration); }
};

//int current;
class IterationDataSet:public OutputDataSet
{
  virtual char *getDictionaryName() { return "Iterations"; }
  virtual void write(int pid,long odset);
};

class SummaryValuesDataset: public OutputDataSet
{
  virtual char *getDictionaryName() { return "SummaryValues"; }
  virtual void write(int pid,long odset);
};

class AffectedModulesDataset: public OutputDataSet
{
  virtual char *getDictionaryName() { return "AffectedModules"; }
  public:
  virtual void write(int pid,long odset);
};


#endif