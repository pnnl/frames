#include "SeriesV2.h"

/*
class Series_v2:public Series
{
private:
public:
char vartime[SMALLSTRING];
char varvalu[SMALLSTRING];
*/
//-----------------------------------------------------------------------------------
Series_v2::Series_v2(char *tunit, char *funit):Series()
{
  man = mMAXDOUBLE;
  eco = mMAXDOUBLE;
  rstrcpy(msgeco,"");
  rstrcpy(msgman,"");
  rstrcpy(vartime,"");
  rstrcpy(varvalu,"");
  rstrcpy(xUnits,tunit);
  rstrcpy(yUnits,funit);
}
//-----------------------------------------------------------------------------------
Series_v2::Series_v2(char *vtime, char *tunit, char *vvalu, char *funit, char *meas)
{
  man = mMAXDOUBLE;
  eco = mMAXDOUBLE;
  measure = meas;
  rstrcpy(msgeco,"");
  rstrcpy(msgman,"");
  rstrcpy(vartime,vtime);
  rstrcpy(varvalu,vvalu);
  rstrcpy(xUnits,tunit);
  rstrcpy(yUnits,funit);
}
//-----------------------------------------------------------------------------------
int Series_v2::ReadTimes(long pid, char *setname, int *tIdx, int tI)
{
  int i;

  int ErrorCode = GetVarDimSize(pid, setname, varvalu, tIdx, &count);
  if (count==0) return 0;
  Init(count);
  
  for (i=1;i<=count;i++)
  {
    tIdx[tI-1] = i;
    ErrorCode = GetFloat(pid, setname, vartime, xUnits, tIdx, &xValues[i-1]);
  }
  return 0;
}
//-----------------------------------------------------------------------------------
int Series_v2::ReadTimes(long pid, char *setname, char *_vartime, int *tIdx, int tI)
{
  rstrcpy(vartime, _vartime);
  int ErrorCode = GetVarDimSize(pid, setname, vartime, tIdx, &count);
  if (count==0) return 0;
  Init(count);
  
  CheckUnit(pid, setname, vartime, xUnits);
  
  for (int i=1;i<=count;i++)
  {
    tIdx[tI-1] = i;
    ErrorCode = GetFloat(pid, setname, vartime, xUnits, tIdx, &xValues[i-1]);
  }
  return 1;
}
//-----------------------------------------------------------------------------------
int Series_v2::ReadDatasets(long pid, char *setname, int *tIdx, int tI, int *vIdx, int vI)
{
  int v;
  int *Idx;

  int ErrorCode = GetVarDimSize(pid, setname, vartime, tIdx, &count);
  if (count==0) return 0;
  Init(count);
  
  CheckUnit(pid, setname, vartime, xUnits);
  CheckUnit(pid, setname, varvalu, yUnits);

  if (vIdx!=NULL) 
  { Idx = vIdx; v = vI;  }
  else 
  { Idx = tIdx; v = tI;  }

  for (int i=1;i<=count;i++)
  {
    tIdx[tI-1] = i;
    ErrorCode = GetFloat(pid, setname, vartime, xUnits, tIdx, &xValues[i-1]);
    Idx[v-1] = i;
    ErrorCode = GetFloat(pid, setname, varvalu, yUnits, Idx, &yValues[i-1]);
  }
  return 0;
}
//-----------------------------------------------------------------------------------
int Series_v2::ReadDatasets(long pid, char *setname, char *_vartime, char *_varvalu, int *tIdx, int tI, int *vIdx, int vI)
{
  rstrcpy(vartime, _vartime);
  rstrcpy(varvalu, _varvalu);
  return ReadDatasets(pid, setname, tIdx, tI, vIdx, vI);
}
//-----------------------------------------------------------------------------------
int Series_v2::WriteDatasets(long pid, icsv *inf, long numsteps, char *setname, int *idx, int ii)
{
  bool feco = true;
  bool fman = true;
  int i, ErrorCode;
  double year, flux;

  if (numsteps==0) return 0;
  CheckUnit(pid, setname, vartime, xUnits);
  CheckUnit(pid, setname, varvalu, yUnits);
  for (i=0;i<numsteps;i++)
  {
    idx[ii]=i+1;
    *inf >> year >> flux >> NewLn;
    ErrorCode = PutFloat(pid, setname, vartime, xUnits, idx, year);
    ErrorCode = PutFloat(pid, setname, varvalu, yUnits, idx, flux);
    if (0 < eco && eco < flux && feco)  { 
      SetWarning(pid, msgeco);  
      feco = false; }
    if (0 < man && man < flux && fman)  { 
      SetWarning(pid, msgman);  
      fman = false; }
  }
  return 0;
}
//-----------------------------------------------------------------------------------
int Series_v2::WriteFile(ocsv *fp)
{
  for (int i=0;i<count;i++)
  {
    *fp << xValues[i] << yValues[i] << NewLn;
  }
  return count;
}