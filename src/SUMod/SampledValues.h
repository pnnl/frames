#ifndef SAMPLEDVALUES_H
#define SAMPLEDVALUES_H

#include <map>
#include <vector>
#include <string>
#include <iostream>

using namespace std;

#include "FramesLib.h"
#include "Equation.h"
#include "Aliases.h"
#include "Assertions.h"

class SampledValue
{
 public:
  double old;
  long sethandle;
  long varhandle;
  string type;
  string units;
  string alias;
  string dataset;
  string variable;
  string description;
  string equation;
  vector<int> indices;
  vector<double> values;
  IAlias *palias;

  SampledValue();
  SampledValue(IAlias &p);
  void clear();
  void add(double d);
  void changeInputs(int pid,int idx);
  void write(int pid,char *dset,int idx);

  // void changeInputs(int pid,long dset,int iteration);
  // void setValue(int pid,int idx, int index[]);
  // void read(int pid,long dset,int idx,int iter);
  // bool write(int pid,long dset,int idx);
  // void recurse(int pid, int idx, int* index, int pos, Enumerate e);
};

class SampledValues
{
 public:
  map<string,SampledValue> values;

  SampledValues();
  void addVar(IAlias &p);
  void write(int pid,char *dset);
  bool readLHO(char *fname);
  ValueMap loadMap(int idx);
  void getFromMap(ValueMap valmap,int idx);
  void changeInputs(int pid,int idx);

  // void readValues(int pid,long dset,int iteration);
  // void addVar(IAlias *p, string alias);
  // void write(int pid,long dset);
  // void addVar(Parameter p);
  // void changeInputs(int pid,long dset,int iteration);
};

#endif
