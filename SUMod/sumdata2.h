#ifndef sumdata_h
#define sumdata_h

#include <map>
#include <vector>
#include <string>
#include <dir.h>
#include <iostream>
#include <math.h>
#include <stdlib.h>
#include <windows.h>
#include <process.h>



#include "SampledValues.h"
#include "Equation.h"

using namespace std;
//------------------------------------------------------------------------------

  class CorInfo
  {
    public:
    string Alias;
    double Cor;
  };

  class Variable
  {
    int Base;
    void DispTrun(FILE *k);
    void DispUnif(FILE *k);
    void DispLU(FILE *k);
    void DispNorm(FILE *k);
    void DispLN(FILE *k);
    void DispExp(FILE *k);
    void DispTri(FILE *k);
    void DispGam(FILE *k);
    void DispBet(FILE *k);
    void DispWei(FILE *k);
    void DispLog(FILE *k);
    vector<CorInfo> Cors;

   public:
    double value;
    int numCor() {return Cors.size();}
    string Alias,Des,LatinId,Distribution;
    string Equation;
    float Mean,Std,Scale,Shift,Mode,Upper,Lower;
    Variable() { Base=0; }
    void Add(CorInfo c) {Cors.insert(Cors.begin(),c); }
    bool writeCor(FILE *k,vector<Variable> &vars,unsigned idx);
    bool HasDist();
    bool HasEquation();
    bool write(FILE *k,int i);

    static Variable testNormal();
    static Variable testUniform();
    static Variable testLogNormal();
    static Variable testLogUniform();
  };
  //------------------------------------------------------------------------------


  class Latin
  {
    static char *keyname;
    static char sensin[MAXPATH];   // ="~sensi.gid";
    static char sensout[MAXPATH];  // ="~senso.*";
    static char workdir[MAXPATH];
    vector<Variable> Vars;

   public:
    SampledValues ins;
    SampledValues outs;
    float Seed;
    void add(IAlias *ia);
    static char fuiAppPath[MAXPATH];
    int Iteration;
    void  add(Variable v,IAlias *p);
    void clear();
    ValueMap loadMap(int idx);
    void getFromMap(ValueMap valMap,int idx);
    bool writeKey(FILE *k);
    bool writeAllKey();
    char* Time();
    char* Date();
    bool writeSens_Id(char *keyname);
    bool writeCorMat(FILE *k);
    bool spawnLatin(char *path);
    bool readLHO(char *path);
    bool writeSampledOutputs(int pid,long odset);
    bool writeSummaryOutputs(int pid,long odset);
    void readSampledValues(int pid,long odset,int iteration);
    void setControl(int seed,int iteration);
    void changeInputs(int pid,int idx);
    //void changeInputs(int pid,long dset,int iteration);
    void readOutputs(int pid,int idx);
    static bool test1(int pid,long svdset);
    static bool passedTest(int pid,long svdset);
  };

extern Latin LatinInterface;
#endif
