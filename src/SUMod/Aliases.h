#ifndef ALIASES_H
#define ALIASES_H

#include <vector>
#include <string>

#include "Equation.h"
#include "Enumerate.h"
#include "InputSet.h"

using namespace std;

class Variable;

class Alias
{
  protected:
  Enumerate allIndices;
  public:
  int dimension;
  long sethandle;
  long varhandle;
  string dataset;
  string varname;
  string alias;
  vector<string> indices;
  vector<int> indexs;
  string units;
  string type;
  string describe();
  string describe(string altalias);
  string getAlias();
  Enumerate *getIndices();
  virtual Alias *getNew()=0;
  virtual char *getPrefix()=0;
  virtual void read(int idx,int pid,long dset);
  void clearIndexs();
  void add(int index);
  string indicesStr();
  void expandAllIndices(int pid,Enumerate e);
  void evalIndices(int pid, int row, int pos, int index[]);
};

class OAlias:public Alias
{
  public:
  string distType;
  vector<double> params;
  virtual Alias *getNew();
  virtual char *getPrefix();
  virtual void read(int idx,int pid,long dset);
};

class Extraction:public OAlias
{
  vector<double> values;
  int summarizeNormal(int pid,long outgroup,int curIndex,int iteration,bool dolog);
  int summarizeUniform(int pid,long outgroup,int curIndex,int iteration,bool dolog);
  int summarizeAll(int pid,long outgroup,int curIndex,int iteration);
  int summarizePercentiles(int pid,long outgroup,int curIndex,int iteration,vector<double> percentiles);
  public:
  Extraction();
  Extraction(OAlias &oa);
  int writeValue(int pid,long outgroup,int curIndex,int iteration,string label,double value,int epos);
  bool readValues(int pid);
  int summarize(int pid,long outgroup,int curIndex,int iteration);
};

class Corr
{
  public:
  double Correlation;
  string OtherAliases;
  void read(int idx1,int idx2,int pid,long dset);
};

class IAlias:public OAlias
{
  public:
  string distType;
  vector<double> params;
  string Equation;
  bool forEach;
  vector<Corr> Corrs;
  virtual Alias *getNew();
  virtual char *getPrefix();
  virtual void read(int idx,int pid,long dset);
  void copyToLatin(int pid,long dset);
  void performEquations(ValueMap &valmap);
  /*static IAlias test1Parameter();
  static IAlias test2Parameter();
  static IAlias test3Parameter();
  static IAlias test4Parameter();
  */
  Variable copy(string newalias);
  int saveOriginalValue(int pid, long dset, int start);
  void restoreOriginalValue(int pid, long dset);
};

typedef vector<IAlias> IAliasSet;
typedef vector<OAlias> OAliasSet;

class IAliases
{
  public:
  vector<IAlias> values;
  virtual void read(int pid,long dset);
};

class OAliases
{
  public:
  vector<OAlias> values;
  virtual void read(int pid,long dset);
};

class Extractions
{
  public:
  vector<Extraction> extractions;
  bool read(OAliases &oa);
  bool readValues(int pid);
  bool summarize(int pid,long odataset,int iteration);
};

#endif
