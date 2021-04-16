#include <iostream>

#include "sumdata2.h"
#include "OutputDataSet.h"

extern InputSet is;
extern Extractions es;

  long OutputDataSet::getDataSetHandle(int pid, char *modid)
  {
    long odataset = IconGetOutputDataSet(pid,modid,getDictionaryName(),1);
    return odataset;
  }

  long OutputDataSet::createOutput(int pid,char *modid)
  {
    long odataset = IconGetOutputDataSet(pid,modid,getDictionaryName(),1);
    if (odataset>0)
      write(pid,odataset);
    return odataset;
  }

  void IterationDataSet::write(int pid,long odset)
  {
    cout << "CurrIter " << is.CurrIter << " LastIter " << is.LastIter << endl;
    if (is.CurrIter==1)
      DataSetClear(pid,odset);
    DataSetWriteInt(pid,odset,"CurrIter","",is.CurrIter);
    DataSetWriteInt(pid,odset,"LastIter","",is.LastIter);
    DataSetWriteInt1(pid,odset,"Iterations","",is.CurrIter,is.CurrIter);
  }

   void SummaryValuesDataset::write(int pid,long odset)
  {
    es.readValues(pid);
    if (is.CurrIter==1)
      DataSetClear(pid,odset);
    es.summarize(pid,odset,is.CurrIter);
  }

  void AffectedModulesDataset::write(int pid,long odset)
  {
    map<string,string> modules;
    for (vector<IAlias>::iterator it=is.Ins.values.begin();it!=is.Ins.values.end();++it)
    {
      string set=it->dataset;
      string mod=set.substr(0,set.find(".",0));
      modules[mod]=mod;
    }
    for (vector<OAlias>::iterator it=is.Outs.values.begin();it!=is.Outs.values.end();++it)
    {
      string set=it->dataset;
      string mod=set.substr(0,set.find(".",0));
      modules[mod]=mod;
    }

    DataSetClear(pid,odset);
    int i=1;
    for (map<string,string>::iterator it=modules.begin();it!=modules.end();++it)
    {
      char *s = (char *)(*it).second.c_str();
      DataSetWriteString1(pid,odset,"ModID","",i,s);
      i++;
    }
  }
  /*
  int AffectedModulesDataset::readAffectedModules(int pid, map<string,Module>mmap)
  {
  }
  */
