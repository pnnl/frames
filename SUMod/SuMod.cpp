#pragma hdrstop
#include <condefs.h>

//------------------------------------------------------------------------------
USEUNIT("Assertions.cpp");
USEUNIT("Equation.cpp");
USEUNIT("Series.cpp");
USEUNIT("Sort.cpp");
USEUNIT("Hwirst.cpp");
USEUNIT("Aliases.cpp");
USEUNIT("SampledValues.cpp");
USEUNIT("sumdata2.cpp");
USEUNIT("InputSet.cpp");
USEUNIT("Extraction.cpp");
USEUNIT("Enumerate.cpp");
USELIB("..\..\Program Files\FramesV2\Developer\OMFsystemio.lib");
//---------------------------------------------------------------------------
#include <map>
#include <set>
#include <vector>
#include <iostream>
//#include "\source\kingcounty\frames2x\sensitivity module V2\modelvc\SystemSU.h"
#include "FramesLib.h"
#include "InputSet.h"
#include "Assertions.h"
#include "Equation.h"
#include "SumData2.h"
#include "Iterator.h"
#include "OutputDataSet.h"

#pragma hdrstop

//------------------------------------------------------------------------------
int pid;
char path[4096];
char apppath[4096];
char modid[4096];
char simfile[4096];

long handle;
//long dataset;
long hndalias;
long hndseed;
long hnditer;
long hmodid;

bool sample = false;
bool summarize = false;
bool iterate = false;

InputSet is;
Extractions es;

/*
void IncrementChildren(map<string,int> &depths,string mod,int current)
{
  int modcount,i;
  char imodid[4096];
  if (depths[mod]<current) // If the child says it is deeper
    depths[mod]=current;
  assert(NumOMod(pid,(char*)mod.c_str(),&modcount)>=0);
  cout << "Number of outputs " << mod <<" " <<  modcount << endl;
  for (i=1;i<=modcount;i++)
  {
    assert(GetOModId(pid, (char*)mod.c_str(), i, imodid)>=0);
    cout << "Outputs to " << imodid << endl;
    IncrementChildren(depths,string(imodid),current+1);
  }
}
*/
/*
bool findAffectedModules(OAliases oa)
{
     affectedModules.clear();
     for (vector<OAlias>::iterator it=oa.values.begin();it!=oa.values.end();++it)
    {
      string set=it->dataset;
      string mod=set.substr(0,set.find(".",0)-1);
      affectedModules.insert(affectedModules.end(),mod);
    }
    return true;
}

bool findAffectedModules(IAliases ia)
{
     affectedModules.clear();
     for (vector<IAlias>::iterator it=ia.values.begin();it!=ia.values.end();++it)
    {
      string set=it->dataset;
      string mod=set.substr(0,set.find(".",0)-1);
      affectedModules.insert(affectedModules.end(),mod);
    }
    return true;
}
*/
bool FindBounds(char *modid,long *start,long *end)
{
    int i,modcount;
    vector<string> ids;
    map <string,int> depths;
    static char imodid[4096];

    *start=0L;
    *end=0L;
  /*
    assert(NumIMod(pid, modid, &modcount)>=0);
    assert(modcount>0);
    for (i=1;i<=modcount;i++)
    {
      assert(GetIModId(pid, modid, i, imodid)>=0);
      assert(strlen(imodid)>0);
      cout << "imodid "<< imodid << endl;
      SetState(pid,imodid,2,2);
      cout << "After SetState"<< endl;
      ids.insert(ids.end(),string(imodid));
      cout << "After ids.insert"<< endl;
//      IncrementChildren(depths,string(imodid),0);
    }
    start=ids[0];
    end=ids[0];
//    for (unsigned j=1;j<ids.size();j++)
//    {
//      if (depths[ids[j]]<depths[start]) start=ids[j];
//      if (depths[ids[j]]>depths[end]) end=ids[j];
//    }
    cout << "Start at "<<start << " end at " << end << endl;
    */
    return true;
}

#pragma argsused
void main(int argc, char* argv[])
{
  long start,end;
  char c;
  long oset, itset, sumset, odataset, itdataset, sumdataset;
  SampledValuesDataset svd;
  SummaryValuesDataset suvd;
  AffectedModulesDataset amd;

  IterationDataSet id;
  assert(Enumerate::passedTest());
  assert(evalPassedTest());
  if (argc<5)
  {
    cout << "Too few arguments" << endl;
    cout << "Usage: SUMod <op> <path> <simfile> <module id>" << endl;
    return;
  }
  summarize = (0==strcmpi(argv[1],"/su"));
  sample    = (0==strcmpi(argv[1],"/sa"));
  iterate   = (0==strcmpi(argv[1],"/it"));
  if (! (summarize || sample || iterate))
  {
    cout << "<op> must be /su or /sa or /it" << endl;
    return;
  }
  strncpy(path,argv[2],4096);
  strncpy(simfile,argv[3],4096);
  strncpy(modid,argv[4],4096);

  pid=ModuleDevOpen(path,simfile,modid);
  if (pid<0) SetWarning(pid,"Bad Module PID");

//  This should be done, if nothing else save as!
//  SystemDevSaveSimulation(pid);

  handle = ModuleGetHandle(pid,"Startup");

  assert(0<DataSetReadString(pid,handle,"AppPath","",apppath));

  hmodid = IconGetHandle(pid,modid);
  assert(hmodid>0);

  if (sample || summarize)
  {
    hndalias=IconGetUIDataSet(pid,modid,"Aliases",1);
    assert(hndalias>0);
    hnditer=is.readIter(pid,modid);
    assert(hnditer>0);

    if (sample) {

      hndalias = is.readInputs(pid,modid);
      assert(hndalias>0);
      is.copyToLatin(pid);

      if (is.CurrIter==-1)
      {
        odataset = is.restoreOriginalValues(pid,modid);
        assert(odataset>0);

        // write something to output to register success
        odataset = IconGetOutputDataSet(pid, modid, "Iterations", 1);
        assert (odataset>0);
        DataSetWriteInt(pid, odataset, "LastIter", "", is.LastIter);

        //SystemDevSaveSimulation(pid);
      }
      else
      {
        if (is.CurrIter==1)
        {


        hndseed=is.readSeed(pid,modid);
        assert(hndseed>0);
        LatinInterface.Seed = is.Seed;

        sprintf(Latin::fuiAppPath,"%s\\",path);
        sprintf(Latin::fuiAppPath,"%s\\",apppath);
        //sprintf(Latin::workdir,"%s\\",apppath);

        //  ????  this is done in is.readInputs... BLH
       //  is.copyToLatin(pid);



        // do this only on first iteration
        assert(LatinInterface.writeAllKey());
        assert(LatinInterface.spawnLatin(apppath));

        // is.copyToLatin();
        LatinInterface.readLHO(apppath);
        is.performEquations();

        // populate sampled values dataset
        odataset = svd.createOutput(pid,modid);
        assert(odataset>0);

        // assert(findAffectedModules());
        odataset = amd.createOutput(pid,modid);
        assert(odataset>0);
      }
      odataset = svd.getDataSetHandle(pid,modid);
      assert(odataset>0);

      // requires is.CopyToLatin executed beforehand to setup variables
      LatinInterface.readSampledValues(pid,odataset,is.CurrIter);  // read single value for currIter into values vector
      //is.readSampledValues(pid,odataset,is.CurrIter);

      itdataset = id.createOutput(pid,modid);
      assert(itdataset>0);

//      Iterate(path,simfile,modid);

      LatinInterface.changeInputs(pid,0);                 // index to currIter in values vector is 0
      }
    }
    if (summarize) {
      hndalias = is.readOutputs(pid,modid);
      es.read(is.Outs);

      sumdataset=suvd.createOutput(pid,modid);
      assert(sumdataset>0);
      //assert(findAffectedModules(is.Outs));
    }
//  assert(svd.createOutput(pid,hmodid,odataset));
  }


  ClearErrors(pid);
  ModuleDevClose(pid,0);
}
/*
void main(int argc, char* argv[])
{
  long start,end;
  char c;
  long oset, itset, sumset, odataset, itdataset, sumdataset;
  SampledValuesDataset svd;
  SummaryValuesDataset suvd;
  AffectedModulesDataset amd;

  IterationDataSet id;
  assert(Enumerate::passedTest());
  assert(evalPassedTest());
  if (argc<4)
  {
    cout << "Too few arguments" << endl;
    cout << "Usage: SUMod <path> <simfile> <module id>" << endl;
    return;
  }
  if (argc==5) {
    strncpy(path,argv[1],4096);
    strcat(path," ");
    strcat(path,argv[2]);
    strncpy(simfile,argv[3],4096);
    strncpy(modid,argv[4],4096);
  } else {
    strncpy(path,argv[1],4096);
    strncpy(simfile,argv[2],4096);
    strncpy(modid,argv[3],4096);
  }
  sprintf(Latin::fuiAppPath,"%s\\",path);
  pid=ModuleDevOpen(path,simfile,modid);
  if (pid<0) SetWarning(pid,"Bad Module PID");
  assert(pid>0);
  hmodid = IconGetHandle(pid,modid);
  assert(hmodid>0);
  dataset=IconGetUIDataSet(pid,modid,"StochIter",1);
  assert(dataset>0);
  is.read(pid,dataset);
  es.read(is.Outs);

  assert(findAffectedModules(is.Ins,is.Outs));

  //odataset = amd.createOutput(pid,modid);
  //assert(odataset>0);

  // do this only on first iteration
  assert(LatinInterface.writeAllKey());
  assert(LatinInterface.spawnLatin(path));
  LatinInterface.readLHO(path);
  is.performEquations();

  // do this for every iteration

  odataset = svd.createOutput(pid,modid);
  assert(odataset>0);

//  assert(svd.createOutput(pid,hmodid,odataset));
  assert(FindBounds(modid,&start,&end));
  if (ModuleDevOk(pid)) cout << "Error before Loop pid" << endl;
  for (current=1;current<=LatinInterface.Iteration;current++)
  {
    itdataset = id.createOutput(pid,modid);
    assert(itdataset>0);
    //assert(id.createOutput(pid,hmodid,itdataset));

    LatinInterface.changeInputs(pid,current-1);

    assert(FindBounds(modid,&start,&end));

    IconsRunBetween(pid,start,end);
    //assert(IconsRunBetween(pid,(char*)start.c_str(),(char*)end.c_str())>=0);

    sumdataset=suvd.createOutput(pid,modid);
    assert(sumdataset>0);
    //assert(suvd.createOutput(pid,hmodid,sumdataset));
  }
  LatinInterface.changeInputs(pid,-1); // Put back old values
  //RunBetween(pid,(char*)start.c_str(),(char*)end.c_str());
  IconsRunBetween(pid,start,end) ;
  ClearErrors(pid);
  ModuleDevClose(pid,0);
}
*/


//------------------------------------------------------------------------------
