#include <strstream>

using namespace std;

#include "Aliases.h"
#include "c:\program files\framesv2\Developer\F2ModuleDevC.h"
#include "c:\program files\framesv2\Developer\F2SystemDevC.h"
#include "c:\program files\framesv2\Developer\ErrorC.h"
#include "assertions.h"

  Extraction::Extraction()
  {
  }
  Extraction::Extraction(OAlias &oa)
  {
    *((OAlias *)this)=oa;
  }
  int Extraction::writeValue(int pid,long ogroup,int curIndex,int iteration,string label,double value,int epos=-1)
  {
    //char *ogroup=(char *)outgroup.c_str();
    if (iteration==1)
    {
      DataSetWriteString1(pid,ogroup,"ShortName","",curIndex,(char*)(alias+label).c_str());
      DataSetWriteString1(pid,ogroup,"Variables","",curIndex,(char*)(varname+label).c_str());
      if (label=="_count")
        DataSetWriteString1(pid,ogroup,"VarUnits","",curIndex,"");
      else
        DataSetWriteString1(pid,ogroup,"VarUnits","",curIndex,(char*)units.c_str());
      for (unsigned i=0;i<dimension;i++) {
        int idx=0;
        if (epos>=0)
          idx = allIndices.idx(epos,i);
        //  int idx=atoi(indices[i].c_str());
        DataSetWriteInt2(pid,ogroup,"Indices","",curIndex,i+1,idx);
      }
      DataSetWriteString1(pid,ogroup,"Set","",curIndex,(char*)dataset.c_str());
    }
    DataSetWriteReal2(pid,ogroup,"Value","",curIndex,iteration,value);
    return curIndex+1;
  }
  bool Extraction::readValues(int pid)
  {
    double val;
    int index[6]={0,0,0,0,0,0},ival,code;
    values.clear();
    allIndices.start();

    int ctx = allIndices.size();
    for (int ix=0;ix<ctx;ix++)
    {
//      cout << "Index ";
      for (int i=0;i<dimension;i++)
      {
        index[i]=allIndices.idx(ix,i);
//        cout << index[i] << " " ;
        }
//      cout << endl;
      if (type=="FLOAT") {
        val=DataSetGetReal(pid,sethandle,(char*)varname.c_str(),(char*)units.c_str(),index);
        //code=GetFloat(pid,(char*)dataset.c_str(),(char*)varname.c_str(),(char*)units.c_str(),index,&val);
        //cout << "readValues Float Value " <<pid << " " << dataset <<" " << varname << " " <<units << " "<<val << endl;
      }
      else if (type=="INTEGER")
      {
        ival=DataSetGetInteger(pid,sethandle,(char*)varname.c_str(),(char*)units.c_str(),index);
        //code=GetInteger(pid,(char*)dataset.c_str(),(char*)varname.c_str(),(char*)units.c_str(),index,&ival);
        val=ival;
        cout << "readValues Integer Value " << ival << endl;
      }
      code = ModuleDevOk(pid);
      if (code==0)
        values.insert(values.end(),val);
      else
        cout << "!!Error!! reading values " << endl;
    }
    return (code!=0);
  }
  int Extraction::summarize(int pid,long outgroup,int curIndex,int iteration)
  {
    if (distType=="Every Value")
      curIndex=summarizeAll(pid,outgroup,curIndex,iteration);
    else if (distType=="Log Uniform")
      curIndex=summarizeUniform(pid,outgroup,curIndex,iteration,true);
    else if (distType=="Uniform")
      curIndex=summarizeUniform(pid,outgroup,curIndex,iteration,false);
    else if (distType=="Log Normal")
      curIndex=summarizeNormal(pid,outgroup,curIndex,iteration,true);
    else if (distType=="Normal")
      curIndex=summarizeNormal(pid,outgroup,curIndex,iteration,false);
    else if (distType=="Percentiles")
      curIndex=summarizePercentiles(pid,outgroup,curIndex,iteration,params);
    return curIndex;
  }
  int Extraction::summarizeNormal(int pid,long outgroup,int curIndex,int iteration,bool dolog)
  {
    double average,sd,sse,se,v;
    int count=0;
    curIndex=summarizeUniform(pid,outgroup,curIndex,iteration,dolog);
    average=0.0;
    sd=0.0;
    sse=0.0;
    se=0.0;
    if (values.size()>0)
    {
      for (unsigned i=0;i<values.size();i++)
      {
        if (dolog && values[i]>0)
        {
          average+=log(values[i]);
          count++;
        }
        else
        {
          average+=values[i];
          count++;
        }
      }
      if (count>0) average/=(double)count;
      if (count>2)
      {
        for (unsigned i=0;i<values.size();i++)
        {
          if (dolog && values[i]>0)
          {
            v=log(values[i]);
            sse+=(v-average)*(v-average);
            se+=(v-average);
          }
          else
          {
            sse+=(values[i]-average)*(values[i]-average);
            se+=(values[i]-average);
          }
        }
        sd=sqrt(sse-se*se)/(double)(count-2);
      }
    }
    curIndex=writeValue(pid,outgroup,curIndex,iteration,"_mean",average);
    curIndex=writeValue(pid,outgroup,curIndex,iteration,"_std",sd);
    return curIndex;
  }
  int Extraction::summarizeUniform(int pid,long outgroup,int curIndex,int iteration,bool dolog)
  {
    double min,max;
    int count=0;
    min=0.0;
    max=0.0;
    bool first=false;
    if (values.size()>0)
    {
      for (unsigned i=0;i<values.size();i++)
      {
        if(!first && (!dolog || (dolog && values[i]>0)))
        {
          if (dolog)
          {
            min=log(values[i]);
            max=log(values[i]);
            count++;
          }
          else
          {
            min=values[i];
            max=values[i];
            count++;
          }
          first=true;
        }
        else
        {
          if (dolog)
          {
            if (values[i]>0)
            {
              if (log(values[i])<min) min=log(values[i]);
              if (log(values[i])>max) max=log(values[i]);
              count++;
            }
          }
          else
          {
            if (values[i]<min) min=values[i];
            if (values[i]>max) max=values[i];
            count++;
          }
        }
      }
    }
    curIndex=writeValue(pid,outgroup,curIndex,iteration,"_count",(double)count);
    curIndex=writeValue(pid,outgroup,curIndex,iteration,"_min",min);
    curIndex=writeValue(pid,outgroup,curIndex,iteration,"_max",max);
    return curIndex;
  }
  int Extraction::summarizeAll(int pid,long outgroup,int curIndex,int iteration)
  {
    for (unsigned i=0;i<values.size();i++)
    {
      strstream s;
      s << "_" << (i+1) << '\0';
      s.freeze();
      curIndex=writeValue(pid,outgroup,curIndex,iteration,s.str(),values[i],i);
    }
    return curIndex;
  }
  int Extraction::summarizePercentiles(int pid,long outgroup,int curIndex,int iteration,vector<double> percentiles)
  {
    int idx;
    sort(values.begin(),values.end());
    for(unsigned i=0;i<percentiles.size();i++)
    {
      strstream s;
      s << "_" << (i+1) << '\0';
      s.freeze();
      idx=(int)(percentiles[i]*(double)values.size()/100.0);
      curIndex=writeValue(pid,outgroup,curIndex,iteration,s.str(),values[idx]);
    }
    return curIndex;
  }

  bool Extractions::read(OAliases &oa)
  {
    bool anyFailed=false;
    assert(oa.values.size()>0);
    for(unsigned i=0;i<oa.values.size();i++)
      if (extractions.insert(extractions.end(),Extraction(oa.values[i])))
        anyFailed=true;
    return anyFailed;
  }
  bool Extractions::readValues(int pid)
  {
    bool anyFailed=false;
    cout << "extractions.size() " <<extractions.size() <<  endl;
    for (unsigned i=0;i<extractions.size();i++)
      if (extractions[i].readValues(pid)) {
        anyFailed=true;
      }
    return anyFailed;
  }
  bool Extractions::summarize(int pid,long odataset,int iteration)
  {
    int curIndex=1;
    bool anyFailed=false;
    DataSetWriteInt1(pid,odataset,"Iterations","",iteration,iteration);
    for(unsigned i=0;i<extractions.size();i++) {
//      cout << "curIndex " << curIndex << endl;
      curIndex=extractions[i].summarize(pid,odataset,curIndex,iteration);
    }
    return !anyFailed;
  }

