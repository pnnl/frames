#include "Parameter.h"
#include "..\Developer\DataSetC.h"
#include "..\Developer\SystemDevC.h"

  void Parameter::read(int pid,string newDataSet,string newVarname,string newAlias)
  {
    char temp[4096];
    int flag;
    alias=newAlias;
    varname=newVarname;
    dataset=newDataSet;
    if(GetDicId(pid,(char *)dataset.c_str(),temp)<0) return;
    dictionary=string(temp);
    if(GetVarType(pid,(char *)dictionary.c_str(),(char *)varname.c_str(),temp)<0)
      return;
    type=string(temp);
    if(GetVarUnit(pid,(char *)dictionary.c_str(),(char *)varname.c_str(),temp)<0)
      return;
    units=temp;  
    if (GetVarIndexCount(pid,(char*)dictionary.c_str(),(char*)varname.c_str(), &dimension)<0)
      return;
    if (GetVarScalar(pid,(char*)dictionary.c_str(), (char*)varname.c_str(), &flag)<0)
      return;
    if (flag == 0) dimension++;
  }
  string Parameter::indicesStr()
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
  void Parameter::clearIndexs()
  {
    indexs.clear();
  }
  void Parameter::add(int index)
  {
    indexs.insert(indexs.end(),index);
  }
  string Parameter::describe()
  {
    return varname+indicesStr()+" from "+dataset;
  }

  Parameter Parameter::test1Parameter()
  {
    Parameter p;
    p.alias="Test1";
    p.varname="Ex1";
    p.units="";
    p.dataset="ds1.data";
    return p;
  }
  Parameter Parameter::test2Parameter()
  {
    Parameter p;
    p.alias="Test2";
    p.varname="Ex2";
    p.units="g/s";
    p.dataset="ds1.data";
    p.add(1);
    return p;
  }
  Parameter Parameter::test3Parameter()
  {
    Parameter p;
    p.alias="Test3";
    p.varname="Ex3";
    p.units="g";
    p.dataset="ds1.data";
    p.add(11);
    p.add(22);
    return p;
  }
  Parameter Parameter::test4Parameter()
  {
    Parameter p;
    p.alias="Test4";
    p.varname="Ex4";
    p.units="s";
    p.dataset="ds1.data";
    return p;
  }



 