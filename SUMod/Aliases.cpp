#include "Aliases.h"
#include "Assertions.h"
#include "c:\program files\framesv2\Developer\F2DataSetC.h"
#include "c:\program files\framesv2\Developer\F2ModuleDevC.h"
#include "c:\program files\framesv2\Developer\F2SystemDevC.h"
#include "sumdata2.h"
#include <iostream>

using namespace std;

#include <stdlib.h>

//IAliases,Aliases of input variables,1,STRING,FALSE,FALSE,0,32,,,FALSE,for
//IDataSet,Input DataSet for an Alias,1,STRING,FALSE,TRUE,0,32,,,FALSE,,StochIter.IAliases
//IIndexs,Selected indices an Alias,2,STRING,FALSE,FALSE,0,32,,,FALSE,,StochIter.IAliases
//IVarName,Variable Name for and alias,1,STRING,FALSE,TRUE,0,32,,,FALSE,,StochIter.IAliases
//OAliases,Output Aliases,1,STRING,FALSE,FALSE,0,32,,,FALSE,for
//ODataSet,Output DataSet,1,STRING,FALSE,TRUE,0,32,,,FALSE,,StochIter.OAliases
//OIndexs,Output Indices for Aliases,2,STRING,FALSE,FALSE,0,32,,,FALSE,,StochIter.OAliases
//OVarName,Output Variable Name,1,STRING,FALSE,TRUE,0,32,,,FALSE,,StochIter.OAliases
  string Alias::describe()
  {
    return describe(alias);
  }

  string Alias::describe(string altalias)
  {
    assert(this!=NULL);
    vector<string>::iterator it;
    string temp=altalias+" "+varname+" "+dataset;
    for (it=indices.begin();it!=indices.end();++it)
    {
      if (it==indices.begin()) temp+=" ";
      if (it!=indices.begin()) temp+=", ";
      temp+=(*it);
    }
    return temp;
  }

  string Alias::getAlias()
  {
    assert(this!=NULL);
    return alias;
  }
  void Alias::read(int idx,int pid,long dset)
  {
    assert(this!=NULL);
    assert(idx>0);
    assert(pid>0);
    assert(dset>0);
    string dictionary;
    char temp[4096];
    char vname[32];
    long dic, var;
    int  count,flag;
    int idxs[6]={0,0,0,0,0,0};
    sprintf(vname,"%sAliases",getPrefix());
    DataSetReadString1(pid,dset,vname,"",idx,temp);
    alias=string(temp);
    sprintf(vname,"%sDataSet",getPrefix());
    DataSetReadString1(pid,dset,vname,"",idx,temp);
    dataset=string(temp);
    sethandle=DataSetGetHandle(pid,temp);
    sprintf(vname,"%sVarName",getPrefix());
    DataSetReadString1(pid,dset,vname,"",idx,temp);
    varname=string(temp);
    dic = DataSetGetDictionary(pid,sethandle);
    assert(dic>0);
    var = VariableGetHandleByDictionary(pid,dic,(char *)varname.c_str());
    assert(var>0);
    assert(VariableGetType(pid,dic,var,temp)>=0);
    type=string(temp);
    assert(VariableGetUnit(pid,dic,var,temp)>=0);
    units=temp;
    dimension = VariableGetDimension(pid,dic,var);
    assert(dimension>=0);
    flag = VariableGetScalar(pid,dic,var);
    assert(flag>=0);
    idxs[0]=idx;
    sprintf(vname,"%sIndexs",getPrefix());
    count = DataSetDimensionCount(pid,dset,vname,idxs);
    assert(count>=0);
    indices.clear();
    for (int i=0;i<count;i++)
    {
      DataSetReadString2(pid,dset,vname,"",idx,i+1,temp);
      indices.insert(indices.end(),string(temp));
    }
    Enumerate e;
    e.parseAllIndices(indices);
    expandAllIndices(pid,e);

  }

  Enumerate *Alias::getIndices()
  { return &allIndices; }

  void Alias::evalIndices(int pid, int row, int pos, int index[])
  {
   if (pos>=dimension)
   {
     indexs.clear();
     for (int k=0;k<dimension;k++)
        indexs.insert(indexs.end(),index[k]);
     allIndices.insert(indexs);
     return;
   }
   else if (index[pos]<0)
   {
      //index[pos]=0;
      int idx[8]={0,0,0,0,0,0,0,0};
      for (int i=0;i<pos;i++) idx[i]=index[i];
      int ct2 = DataSetDimensionCount(pid,sethandle,(char*)varname.c_str(),idx);
      for (int j=0;j<ct2;j++)
      {
        index[pos]=j+1;
        evalIndices(pid, row, pos+1, index);
      }
   }
   else
     evalIndices(pid, row, pos+1, index);
  }

  void Alias::expandAllIndices(int pid, Enumerate e)
  {
    int j;

    int index[8]={0,0,0,0,0,0,0,0};
    allIndices.clear();

      // copies enumerate into state
      e.start();
      while (!e.done())
      {
        for (j=0;j<dimension;j++) index[j]=e.idx(j);
        for (j=dimension;j<8;j++) index[j]=0;
        evalIndices(pid,0,0,index);
        e.next();
      }
//  cout << "expandAllIndices " << alias << endl;
    allIndices.debug();
  }

  void Alias::clearIndexs()
  {
    indexs.clear();
  }
  void Alias::add(int index)
  {
    indexs.insert(indexs.end(),index);
  }
  string Alias::indicesStr()
  {
    char temp[80];
    if (indexs.size()==0) return "";
    string s="{";
    for (unsigned i=0;i<indexs.size();i++)
    {
      if (i>0) s+=",";
      s+=itoa(indexs[i],temp,10);
    }
    s+="}";
    return s;
  }

  char* OAlias::getPrefix()
  {
     assert(this!=NULL);
     return "O";
  }
//ODistType,Distribution type to assume,1,STRING,FALSE,TRUE,0,32,,,FALSE,,StochIter.OAliases
//OParams,Output Parameters for distribution calculations,2,FLOAT,FALSE,FALSE,0,999999,,,TRUE,,StochIter.OAliases
//IDistType,Distribution types ,1,STRING,FALSE,TRUE,0,32,,,FALSE,,StochIter.IAliases
//IParams,Parameters for distribution,2,FLOAT,FALSE,FALSE,0,99999,,,TRUE,,StochIter.IAliases
  void OAlias::read(int idx,int pid,long dset)
  {
    assert(this!=NULL);
    assert(idx>0);
    assert(pid>0);
    assert(dset>0);
    //assert(dset!=NULL);
    //assert(strlen(dset)>0);
    char temp[4096];
    char vname[32];
    int count;
    int idxs[6]={0,0,0,0,0,0};
    Alias::read(idx,pid,dset);
    sprintf(vname,"%sDistType",getPrefix());
    DataSetReadString1(pid,dset,vname,"",idx,temp);
    distType=string(temp);
    sprintf(vname,"%sParams",getPrefix());
    idxs[0]=idx;
    count = DataSetDimensionCount(pid,dset,vname,idxs);
    if (count<0) return;
    for (int i=0;i<count;i++)
      params.insert(params.end(),DataSetReadReal2(pid,dset,vname,"",idx,i+1));
  }

  Alias* OAlias::getNew()
  {
    assert(this!=NULL);
    return new OAlias();
  }

//ICorrelation,Corrleration to other alias,2,FLOAT,FALSE,TRUE,0,1,,,TRUE,,StochIter.IAliases,StochIter.IOtherAliases
//IOtherAliases,Other aliases this alias is correlated with,2,STRING,FALSE,FALSE,0,32,,,FALSE,,StochIter.IAliases
  void Corr::read(int idx1,int idx2,int pid,long dset)
  {
    assert(this!=NULL);
    assert(idx1>0);
    assert(idx2>0);
    assert(pid>0);
    assert(dset>0);
    //assert(dset!=NULL);
    //assert(strlen(dset)>0);
    char temp[4096];
    Correlation=DataSetReadReal2(pid,dset,"ICorrelation","",idx1,idx2);
    DataSetReadString2(pid,dset,"IOtherAliases","",idx1,idx2,temp);
    OtherAliases=string(temp);
  }

  Alias* IAlias::getNew()
  {
    assert(this!=NULL);
    return new IAlias();
  }
  char* IAlias::getPrefix()
  {
     assert(this!=NULL);
     return "I";
  }
//IEquation,Equation to perform after sampling,1,STRING,FALSE,TRUE,0,32,,,FALSE,,StochIter.IAliases
  void IAlias::read(int idx,int pid,long dset)
  {
    assert(this!=NULL);
    assert(idx>0);
    assert(pid>0);
    assert(dset>0);
    //assert(dset!=NULL);
    //assert(strlen(dset)>0);
    int count;
    char temp[4096];
    int idxs[6]={0,0,0,0,0,0};
    Corr c;
    OAlias::read(idx,pid,dset);
    DataSetReadString1(pid,dset,"IEquation","",idx,temp);
    Equation=string(temp);
    forEach=(DataSetReadLog1(pid,dset,"IForEach","",idx)!=0);
    idxs[0]=idx;
    count=DataSetDimensionCount(pid,dset,"IOtherAliases",idxs);
    if (count<0) return;
    //if (GetVarDimSize(pid,dset,"IOtherAliases",idxs,&count)< 0) return;
    for (int i=0;i<count;i++)
    {
      c.read(idx,i+1,pid,dset);
      Corrs.insert(Corrs.begin(),c);
    }
    /*
    if (distType!="None")
      copyToLatin(pid,dset);
    */
  }

  void IAlias::copyToLatin(int pid,long dset)
  {
    assert(this!=NULL);
    assert(pid>0);
    assert(dset>0);
    Enumerate e = allIndices;
    Variable v;

    v.Alias=alias;
    v.Des=describe();
    v.Distribution=distType;
    v.Equation=Equation;
    if (distType=="Uniform" || distType=="Log Uniform")
    {
      v.Lower=params[0];
      v.Upper=params[1];
    }
    if (distType=="Log Normal" || distType=="Normal")
    {
      v.Mean=params[0];
      v.Std =params[1];
      v.Lower=params[2];
      v.Upper=params[3];
    }
    if (forEach)
    {
      e.start();  // initialize state
      int ctx = e.size();
      for (int ix=0;ix<ctx;ix++)
      {
        clearIndexs();
        for (int i=0;i<dimension;i++)
          add(e.idx(ix,i));
        v.Alias = alias+indicesStr();
        v.Des=describe(v.Alias);
        LatinInterface.add(v,this); // Add a distribition for each index set
      }
    }
    else
      LatinInterface.add(v,this);  // Add one distribution to Latin
  }
  void IAlias::performEquations(ValueMap &valmap)
  {
    char *line1,*line2,*line3;
    if (Equation.empty()) return;
    assert(eval(alias,"",Equation,valmap,&line1,&line2,&line3));
  }

  void IAliases::read(int pid,long dset)
  {
    assert(this!=NULL);
    assert(pid>0);
    assert(dset>0);
    char vname[32];
    int idxs[6]={0,0,0,0,0,0};
    int count;
    IAlias a;
    sprintf(vname,"IAliases");
    count = DataSetDimensionCount(pid,dset,vname,idxs);
    assert(count>=0);
    for (int i=0;i<count;i++)
    {
      a.read(i+1,pid,dset);
      values.insert(values.end(),a);
    }
  }

  Variable IAlias::copy(string newalias)
  {
    Variable v;

    v.Alias=newalias;
    v.Des=describe(newalias);
    v.Distribution=distType;
    v.Equation=Equation;
    if (distType=="Uniform" || distType=="Log Uniform")
    {
      v.Upper=params[1];
      v.Lower=params[0];
    }
    if (distType=="Log Normal" || distType=="Normal")
    {
      v.Mean=params[0];
      v.Std =params[1];
      v.Lower=params[2];
      v.Upper=params[3];
    }
    return v;
  }

  int IAlias::saveOriginalValue(int pid, long dset, int start)
  {
    int i;
    char temp[4096];
    int pos;

    DataSetGetName(pid,dset,temp);
    //cout << " saveOriginalValue write "<< alias << " to " << temp << endl;

    int index[6]={0,0,0,0,0,0};
    allIndices.start();  // initialize state
    int ctx = allIndices.size();
    for (int idx=0;idx<ctx;idx++)
    {
      clearIndexs();
      for (i=0;i<dimension;i++)
      {
        index[i]=allIndices.idx(idx,i);
        add(index[i]);
      }
      pos = start+idx;
      for (i=0;i<dimension;i++)
        DataSetWriteInt2(pid,dset,"Indices","",pos,i+1,index[i]);
      DataSetWriteString1(pid,dset,"Set","",pos,(char*)dataset.c_str());
      DataSetWriteString1(pid,dset,"ShortName","",pos,(char*)(alias+indicesStr()).c_str());
      DataSetWriteString1(pid,dset,"Variables","",pos,(char*)varname.c_str());
      DataSetWriteString1(pid,dset,"VarUnits","",pos,(char*)units.c_str());

      double value;
      if (type=="FLOAT")
        value=DataSetGetReal(pid,sethandle,(char*)varname.c_str(),(char*)units.c_str(),index);
      else if (type=="INTEGER")
      {
        int iold;
        iold=DataSetGetInteger(pid,sethandle,(char*)varname.c_str(),(char*)units.c_str(),index);
        value=iold;
      }
      DataSetWriteReal1(pid,dset,"Value","",pos,value);
    }
    return pos;
  }

  void IAlias::restoreOriginalValue(int pid, long dset)
  {
  double value;
  int index[6]={0,0,0,0,0,0};

    allIndices.start();  // initialize state
    int ctx = allIndices.size();
    for (int idx=0;idx<ctx;idx++)
    {
      clearIndexs();
      for (int i=0;i<dimension;i++)
      {
        add(allIndices.idx(idx,i));
        index[i]=0;
      }
      string a = alias+indicesStr();
      int pos = DataSetLookUp(pid,dset,"ShortName",(char*)(alias+indicesStr()).c_str(),index);
      assert(pos>0);

    for (int i=0;i<dimension;i++)
     index[i] = DataSetReadInt2(pid,dset,"Indices","",pos,i+1);
    value=DataSetReadReal1(pid,dset,"Value","",pos);
   if (type=="FLOAT")
      DataSetSetReal(pid,sethandle,(char*)varname.c_str(),(char*)units.c_str(),index,value);
    else if (type=="INTEGER")
      DataSetSetInteger(pid,sethandle,(char*)varname.c_str(),(char*)units.c_str(),index,(int)value);
    }
  }

  void OAliases::read(int pid,long dset)
  {
    assert(this!=NULL);
    assert(pid>0);
    assert(dset>0);
    char vname[32];
    int idxs[6]={0,0,0,0,0,0};
    int count;
    OAlias a;
    sprintf(vname,"OAliases");
    count=DataSetDimensionCount(pid,dset,vname,idxs);
    assert(count>=0);
    for (int i=0;i<count;i++)
    {
      a.read(i+1,pid,dset);
      values.insert(values.end(),a);
    }
  }

