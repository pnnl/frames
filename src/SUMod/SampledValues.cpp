
#include "SampledValues.h"

//------------------------------------------------------------------------------
// Dictionary
// Indices,Dimension indices for sampled variables.,2,INTEGER,FALSE,FALSE,0,32767,,,FALSE,,SampledValues.variables
// Value,Sampled value of variable,2,FLOAT,FALSE,TRUE,0,0,,,FALSE,,SampledValues.variables,SampledValues.Iterations
// Set,Data set for sampled variables,1,STRING,FALSE,TRUE,0,0,,,FALSE,,SampledValues.variables
// Iterations,Set of iterations that have been sampled,1,INTEGER,FALSE,FALSE,0,0,,,FALSE,for
// ShortName,Alias name for sampled variables,1,STRING,FALSE,TRUE,0,0,,,FALSE,,SampledValues.variables
// Variables,variable names that have been sampled,1,STRING,FALSE,FALSE,0,0,,,FALSE,for
// VarUnits,Units for sampled variables,1,STRING,FALSE,TRUE,0,0,,,FALSE,,SampledValues.variables

//------------------------------------------------------------------------------
SampledValue::SampledValue()
{
// public:
//  double old;
//  long sethandle;
//  long varhandle;
//  string type;
//  string units;
//  string alias;
//  string dataset;
//  string variable;
//  string description;
//  string equation;
//  vector<int> indices;
//  vector<double> values;
//  IAlias *palias;
}

SampledValue::SampledValue(IAlias &p)
{
  type=p.type;
  alias=p.alias;
  units=p.units;
  dataset=p.dataset;
  variable=p.varname;
  description=p.describe();
}

void SampledValue::clear()
{  values.clear();  }

void SampledValue::add(double d)
{  values.insert(values.end(),d);  }

void SampledValue::changeInputs(int pid,int idx)
{
  // Index of 0 saves old values
  // Index of -1 replaces old values

  int iold=0;
  int code=0;
  double value=0;
  int index[6]={0,0,0,0,0,0,0};

  if (idx==-1)
    value=old;
  else
    value=values[idx];
  for (unsigned i=0;i<6;i++)
    if (i<indices.size())
      index[i]=indices[i];
  if (type=="FLOAT")
  {
    if (idx==0)
      code = GetFloat(pid,(char*)dataset.c_str(),(char*)variable.c_str(),(char*)units.c_str(),index,&old);
    if (code!=0)
      cout << "Error GetFloat " << dataset << " " << variable << endl;
    code = PutFloat(pid,(char*)dataset.c_str(),(char*)variable.c_str(),(char*)units.c_str(),index,value);
    if (code!=0)
      cout << "Error PutFloat " << dataset << " " << variable << " " << value << endl;
  }
  else if (type=="INTEGER")
  {
    if (idx==0)
      code = GetInteger(pid,(char*)dataset.c_str(),(char*)variable.c_str(),(char*)units.c_str(),index,&iold);
    if (code!=0)
      cout << "Error GetInteger " << dataset << " " << variable << endl;
    old = iold;
    code = PutInteger(pid,(char*)dataset.c_str(),(char*)variable.c_str(),(char*)units.c_str(),index,(int)value);
    if (code!=0)
      cout << "Error PutInteger " << dataset << " " << variable << " " << value << endl;
  }
}

void SampledValue::write(int pid,char *dset,int idx)
{
  unsigned i;

  cout << " SampledValue write "<< dset << endl;
  for (i=0;i<indices.size();i++)
    WriteInt2(pid,dset,"Indices","",idx,i+1,indices[i]);
  for (i=0;i<values.size();i++)
    WriteReal2(pid,dset,"Value","",idx,i+1,values[i]);
  WriteString1(pid,dset,"Set","",idx,(char*)dataset.c_str());
  WriteString1(pid,dset,"ShortName","",idx,(char*)alias.c_str());
  WriteString1(pid,dset,"Variables","",idx,(char*)variable.c_str());
  WriteString1(pid,dset,"VarUnits","",idx,(char*)units.c_str());
  if (idx==1)
    for (i=0;i<values.size();i++)
      WriteInt1(pid,dset,"Iterations","",i+1,i+1);
}

//------------------------------------------------------------------------------
void SampledValues::SampledValues()
{
// public:
//  map<string,SampledValue> values;
}

void SampledValues::addVar(IAlias &p)
{
  SampledValue s;
  s.type=p.type;
  s.units=p.units;
  s.alias=p.alias;
  s.dataset=p.dataset;
  s.variable=p.varname;
  s.description=p.describe();
  s.indices=p.indexs;
  values[p.alias]=s;
}

void SampledValues::write(int pid,char *dset)
{
  int count=1;
  int indices[6]={0,0,0,0,0,0};

  ClearVariable(pid,dset,"Indices",indices);
  ClearVariable(pid,dset,"Iterations",indices);
  ClearVariable(pid,dset,"Set",indices);
  ClearVariable(pid,dset,"ShortName",indices);
  ClearVariable(pid,dset,"Value",indices);
  ClearVariable(pid,dset,"Variables",indices);
  ClearVariable(pid,dset,"VarUnits",indices);
  for (map<string,SampledValue>::iterator it=values.begin();it!=values.end();it++)
  {
    (*it).second.write(pid,dset,count);
    count++;
  }
}

bool SampledValues::readLHO(char *name)
{
  FILE *f;
  char temp[80],spaces[80];
  char varname[80], alias[80];
  unsigned i,j,iter,varcount;

  f=fopen(name,"rt");
  if (f!=NULL)
  {
    fscanf(f,"%[^\n]%c",spaces,&spaces[0]);
    fscanf(f,"%[^\n]%c",spaces,&spaces[0]);
    fscanf(f,"%[ ]%[^ ]",spaces,temp);
    varcount=atoi(temp);
    fscanf(f,"%[ ]%[^ ]",spaces,temp);
    iter=atoi(temp);
    fscanf(f,"%[^\n]%c",spaces,&spaces[0]);
    for (i=0;i<varcount;i++)
    {
      fscanf(f,"%[^ ]%[ ]%[^ ]%[^\n]%c",alias,spaces,varname,spaces,&spaces[0]);
      fscanf(f,"%[^\n]%c",spaces,&spaces[0]);
      for (j=0;j<iter;j++)
      {
        fscanf(f,"%[ ]%[^ ]%[ ]%[^ \n]%[^\n]%c",spaces,spaces,spaces,temp,spaces,&spaces[0]);
        values[string(alias)].add(atof(temp));
      }
    }
    fclose(f);
    return true;
  }
  else
    return false;
}

ValueMap SampledValues::loadMap(int idx)
{
  static ValueMap temp;
  for (map<string,SampledValue>::iterator it=values.begin();it!=values.end();++it)
  {
    float value=0.0;
    if (idx<(int)(*it).second.values.size()) value=(*it).second.values[idx];
    temp[(*it).second.alias]=value;
  }
//    assert(temp.size()==values.size());
  return temp;
}
void SampledValues::getFromMap(ValueMap valmap,int idx)
{
  double d;
  for (ValueMap::iterator it=valmap.begin();it!=valmap.end();it++)
  {
    d=valmap[(*it).first]; // The value for a given alias in the value map
    if (idx>=(int)values[(*it).first].values.size())
      values[(*it).first].values.insert(values[(*it).first].values.end(),d);
    else
      values[(*it).first].values[idx]=d;
  }
}

void SampledValues::changeInputs(int pid,int idx)
{
  for (map<string,SampledValue>::iterator it=values.begin();it!=values.end();it++)
    (*it).second.changeInputs(pid,idx);
}


//------------------------------------------------------------------------------

//extern void Iterate(char *argv1, char *argv2, char *argv3);

/*
class OutputDataSet
{
  protected:
  virtual char* getDictionaryName()=0;
  virtual void write(int pid,long odset)=0;
  public:
  long getDataSetHandle(int pid, char *modid)
  {
    long odataset = IconGetOutputDataSet(pid,modid,getDictionaryName(),1);
    return odataset;
  }

  long createOutput(int pid,char *modid)
  {
    long odataset = IconGetOutputDataSet(pid,modid,getDictionaryName(),1);
    if (odataset>0)
      write(pid,odataset);
    return odataset;
  }
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
  virtual void write(int pid,long odset)
  {
    cout << "CurrIter " << is.CurrIter << " LastIter " << is.LastIter << endl;
    if (is.CurrIter==1)
      DataSetClear(pid,odset);
    DataSetWriteInt(pid,odset,"CurrIter","",is.CurrIter);
    DataSetWriteInt(pid,odset,"LastIter","",is.LastIter);
    DataSetWriteInt1(pid,odset,"Iterations","",is.CurrIter,is.CurrIter);
  }
};

class SummaryValuesDataset: public OutputDataSet
{
  virtual char *getDictionaryName() { return "SummaryValues"; }
  virtual void write(int pid,long odset)
  {
    es.readValues(pid);
    if (is.CurrIter==1)
      DataSetClear(pid,odset);
    es.summarize(pid,odset,is.CurrIter);
  }
};

class AffectedModulesDataset: public OutputDataSet
{
  virtual char *getDictionaryName() { return "AffectedModules"; }
  virtual void write(int pid,long odset)
  {
    map<string,string> modules;
    for (vector<IAlias>::iterator it=is.Ins.values.begin();it!=is.Ins.values.end();++it)
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
  int readAffectedModules(int pid, long hset)
  {
    return 0;
  }
};
*/
