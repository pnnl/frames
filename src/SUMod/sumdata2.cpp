#include "sumdata2.h"
#include "Assertions.h"

using namespace std;

//------------------------------------------------------------------------------
void Variable::DispTrun(FILE *k)
{
  int Limits;
  if (Upper!=Lower) Limits=3;
  else  Limits=0;

  fprintf(k,"  Truncation   %d ",Limits);
  if (Limits ==1 || Limits ==3)
    fprintf(k,"%9.3E ",Lower);
  else
    fprintf(k,"          ");
  if (Limits ==2 || Limits ==3)
    fprintf(k,"%9.3E\n",Upper);
  else
    fprintf(k,"\n");
}
//------------------------------------------------------------------------------
void Variable::DispUnif(FILE *k)
{
  fprintf(k,"1 Uniform\n");
  fprintf(k,"  Trucation    0\n");
  fprintf(k,"  Parameters   %E  %E\n",Upper,Lower);
}
//------------------------------------------------------------------------------
void Variable::DispLU(FILE *k)
{
  if (Base!=0)
    fprintf(k,"3 Log Uniform (e)\n");
  else
    fprintf(k,"2 Log Uniform (ten)\n");
  fprintf(k,"  Truncation   0\n");
  fprintf(k,"  Parameters   %E  %E\n",Lower,Upper);
}
//------------------------------------------------------------------------------
void Variable::DispNorm(FILE *k)
{
  fprintf(k,"4 Normal\n");
  DispTrun(k);
  fprintf(k,"  Parameters   %E  %E\n",Mean,Std);
}
//------------------------------------------------------------------------------
void Variable::DispLN(FILE *k)
{
  if (Base!=0)
    fprintf(k,"6 Log Normal (e)\n");
  else
    fprintf(k,"5 Log Normal (ten)\n");
  DispTrun(k);
  fprintf(k,"  Parameters   %E  %E\n",Mean,Std);
}
//------------------------------------------------------------------------------
void Variable::DispExp(FILE *k)
{
  fprintf(k,"7 Exponential\n");
  DispTrun(k);
  fprintf(k,"  Parameters   %E  %E  %E  %E\n",Lower,Upper,Mean,Scale);
}
//------------------------------------------------------------------------------
void Variable::DispTri(FILE *k)
{
  fprintf(k,"8 Triangular\n");
  DispTrun(k);
  fprintf(k,"  Parameters   %E  %E  %E\n",Lower,Upper,Mode);
}
//------------------------------------------------------------------------------
void Variable::DispGam(FILE *k)
{
  fprintf(k,"9 Gamma\n");
  DispTrun(k);
  fprintf(k,"  Parameters   %E  %E  %E  %E\n",Lower,Upper,Mean,Std);
}
//------------------------------------------------------------------------------
void Variable::DispBet(FILE *k)
{
  fprintf(k,"10 Beta\n");
  DispTrun(k);
  fprintf(k,"  Parameters   %E  %E  %E  %E\n",Lower,Upper,Mean,Std);
}
//------------------------------------------------------------------------------
void Variable::DispWei(FILE *k)
{
  fprintf(k,"11 Weibull\n");
  DispTrun(k);
  fprintf(k,"  Parameters   %E  %E  %E  %E  %E\n",Lower,Upper,Scale,Shift,Shift);
  /* something up with this distribution*/
}
//------------------------------------------------------------------------------
void Variable::DispLog(FILE *k)
{
  fprintf(k,"12 Logistic\n");
  DispTrun(k);
  fprintf(k,"  Parameters   %E  %E  %E  %E\n",Lower,Upper,Mean,Scale);
}
//------------------------------------------------------------------------------
bool Variable::HasEquation()
{
  return Equation.size()>0;
}
//------------------------------------------------------------------------------
bool Variable::writeCor(FILE *k,vector<Variable> &vars,unsigned idx)
{
  unsigned i,j;
  float *cors;
  cors=new float[idx+1];
  cors[idx]=1.0;
  for (i=0;i<idx;i++)
  {
    cors[i]=0.0;
    for (j=0;j<Cors.size();j++)
      if (Cors[j].Alias==vars[i].Alias)
        cors[i]=Cors[j].Cor;
  }
  for (i=0;i<idx+1;i++)
    fprintf(k,"%#15.3G\n",cors[i]);
  delete[] cors;
  return true;
}
//------------------------------------------------------------------------------
bool Variable::write(FILE *k,int i)
{
  static char *Space="/   ";
    fprintf(k,"VARIABLE   %d   %d   ",i,i);
    if (Distribution=="Uniform")
      DispUnif(k);
    else if (Distribution=="Log Uniform")
      DispLU(k);
    else if (Distribution=="Log Normal")
      DispLN(k);
    else if (Distribution=="Normal")
      DispNorm(k);
    else if (Distribution=="Triangular")
      DispTri(k);
    else if (Distribution=="Gamma")
      DispGam(k);
    else if (Distribution=="Beta")
      DispBet(k);
    else if (Distribution=="Weibull")
      DispWei(k);
    else if (Distribution=="Exponential")
      DispExp(k);
    else if (Distribution=="Logistic")
      DispLog(k);
//  if (Alias.size()>9) Alias=Alias.substr(0,9);
//  fprintf(k,"LABEL      %d   %d \"%-9s\"\n",i,i,Alias.c_str());

    char s[16];
    strncpy(s,Alias.substr(0,9).c_str(),10);
//  string s = Alias;
//  if (s.size()>9) t = s.substr(0,9);
    fprintf(k,"LABEL      %d   %d \"%-9s\"\n",i,i,s);

    sprintf(s,"V%04d",i);
    LatinId = string(s);

    fprintf(k,"UNIQUE     %d   %d \"V%04d\"\n",i,i,i);
    if (Des.size()>60) Des=Des.substr(0,60);
    fprintf(k,"DESCRIBE   %d   %d \"%s\"\n",i,i,Des.c_str());
    fprintf(k, "%-121s\n", Space);
    return true;
}

Variable Variable::testNormal()
{
  Variable v;
  v.Mean=2.0;
  v.Std=1.0;
  v.Upper=4.0;
  v.Lower=0.0;
  v.Alias="tNorm";
  v.Distribution="Normal";
  v.Des="Test Normal Distribution";
  return v;
}
Variable Variable::testUniform()
{
  Variable v;
  v.Upper=4.0;
  v.Lower=0.0;
  v.Alias="tUni";
  v.Distribution="Uniform";
  v.Des="Test Uniform Distribution";
  return v;
}
Variable Variable::testLogNormal()
{
  Variable v;
  v.Mean=2.0;
  v.Std=1.0;
  v.Upper=300.0;
  v.Lower=0.0001;
  v.Distribution="Log Normal";
  v.Alias="tLogNorm";
  v.Base=0;
  v.Des="Test Log Normal Distribution";
  return v;
}
Variable Variable::testLogUniform()
{
  Variable v;
  v.Upper=4.0;
  v.Lower=0.0001;
  v.Base=0;
  v.Distribution="Log Uniform";
  v.Alias="tLogUni";
  v.Des="Test Log Uniform Distribution";
  return v;
}
//------------------------------------------------------------------------------
bool Latin::writeKey(FILE *k)
{
  unsigned i;
  assert(Vars.size()>0);
  for (i=0;i<Vars.size();i++)
    Vars[i].write(k,i+1);
  return Vars.size()>0;
}
//------------------------------------------------------------------------------
bool Latin::writeCorMat(FILE *k)
{
  static char *Space="/   ";
  assert(Vars.size()>0);
  unsigned i;
  i=0;
  while (i<Vars.size() && Vars[i].numCor()==0)
    i++;
  if (i==Vars.size())
    fprintf(k,"CORRELATION Identity\n");
  else
  {
    fprintf(k,"CORRELATION for %4d variables\n",Vars.size());
    for (i=0;i<Vars.size();i++)
      Vars[i].writeCor(k,Vars,i);
    fprintf(k, "/\n", Space);
  }
  return true;
}

char* Latin::keyname="LATINKEY";
char Latin::sensin[MAXPATH];   // ="~sensi.gid";
char Latin::sensout[MAXPATH];  // ="~senso.*";
char Latin::workdir[MAXPATH];
char Latin::fuiAppPath[MAXPATH];

//------------------------------------------------------------------------------
char* Latin::Time()
{
  static char timestring[9];
  time_t ti;
  struct tm *t;
  ti=time(NULL);
  t=localtime(&ti);
  sprintf(timestring,"%02d:%02d:%02d",t->tm_hour,t->tm_min,t->tm_sec);
  return timestring;
}
//------------------------------------------------------------------------------
char* Latin::Date()
{
  static char daystring[11]="  /  /    ";
  time_t ti;
  struct tm *t;
  ti=time(NULL);
  t=localtime(&ti);
  sprintf(daystring,"%02d/%02d/%04d",t->tm_mon+1,t->tm_mday,t->tm_year+1900);
  return daystring;
}
//------------------------------------------------------------------------------
bool Latin::writeSens_Id(char *keyname)
{
  char tmp[MAXPATH];
  FILE *f;
//sprintf(tmp,"%s%s",workdir,"sens.id");
  sprintf(tmp,"%s%s",fuiAppPath,"sens.id");
  f=fopen(tmp,"wt");
  if (f!=NULL)
  {
    fprintf(f,"%s\n",keyname);
    fprintf(f,"%s\n",keyname);
    fprintf(f,"1\n");
    fprintf(f,"1\n");
    fclose(f);
    return true;
  }
  else
    return false;
}
//------------------------------------------------------------------------------
bool Latin::writeAllKey()
{
  FILE *k;
  char *Space="/ ";
  char tmp[128];

  // delete latin files in working directory
  sprintf(tmp,"%s%s",workdir,"LATINKEY.LER"); unlink(tmp);
  sprintf(tmp,"%s%s",workdir,"LATINKEY.KEY"); unlink(tmp);
  sprintf(tmp,"%s%s",workdir,"LATINKEY.LHO"); unlink(tmp);
  sprintf(tmp,"%s%s",workdir,"LATINKEY.LRP"); unlink(tmp);

  // delete latin files in frames directory
  sprintf(tmp,"%s%s",fuiAppPath,"LATINKEY.LER"); unlink(tmp);
  sprintf(tmp,"%s%s",fuiAppPath,"LATINKEY.KEY"); unlink(tmp);
  sprintf(tmp,"%s%s",fuiAppPath,"LATINKEY.LHO"); unlink(tmp);
  sprintf(tmp,"%s%s",fuiAppPath,"LATINKEY.LRP"); unlink(tmp);
  assert(writeSens_Id(keyname));
  sprintf(tmp,"%s%s.key",fuiAppPath,keyname);
  k=fopen(tmp, "wt");
  assert(k!=NULL);
  sprintf(tmp,"\ %s",keyname);
  fprintf(k, "%-121s\n", tmp);
  fprintf(k, "%-121s\n", Space);
  sprintf(tmp, "/       Input file written by FRAMES SUM at %s on %s", Time(), Date());
  fprintf(k, "%-121s\n", tmp);
  fprintf(k, "%-121s\n", "/ Program: FRAMES 1.0 beta version ");
  fprintf(k, "%-121s\n", Space);
  fprintf(k, "%-121s\n", "/    This program is experimental and has no been formally    ");
  fprintf(k, "%-121s\n", "/    tested according to project procedures.  All results are ");
  fprintf(k, "%-121s\n", "/    preliminary in nature.");
  fprintf(k, "%-121s\n", Space);
  fprintf(k, "%-121s\n", Space);
  fprintf(k, "%-121s\n", "/                          Review Signatures ");
  fprintf(k, "%-121s\n", Space);
  fprintf(k, "%-121s\n", "/ Input Prepared By: ________________________________ Date: _______________  ");
  fprintf(k, "%-121s\n", "/ Input Reviewed By: ________________________________ Date: _______________  ");
  fprintf(k, "%-121s\n", Space);
  fprintf(k, "%-121s\n", Space);
  sprintf(tmp, "TITLE \"Run Name %s\"",keyname);
  fprintf(k, "%-121s\n", tmp);
  sprintf(tmp, "USER \"FRAMES USER\"");
  fprintf(k, "%-121s\n", tmp);
  fprintf(k, "%-121s\n", Space);
  sprintf(tmp, "ITERATE %d",Iteration);
  fprintf(k, "%-121s\n", tmp);             
  sprintf(tmp, "SEED %E",Seed);
  fprintf(k, "%-121s\n", tmp);
  fprintf(k, "%-121s\n", Space);
  sprintf(tmp,"FILE REPORT \"%s.LRP\"",keyname);
  fprintf(k, "%-121s\n",tmp);
  sprintf(tmp,"FILE DATA   \"%s.LHO\"",keyname);
  fprintf(k, "%-121s\n",tmp);
  fprintf(k, "%-121s\n", Space);
  fprintf(k, "%-121s\n", "OUTPUT a DATA table");
  fprintf(k, "%-121s\n", Space);
  fprintf(k, "%-121s\n", "OUTPUT the CORRELATION matrix");
  fprintf(k, "%-121s\n", Space);
  assert(writeKey(k));
  assert(writeCorMat(k));
  fprintf(k, "%-121s\n", "EXECUTE");
  fprintf(k, "%-121s\n", "END");
  fclose(k);
  return true;
}


//------------------------------------------------------------------------------
bool Latin::spawnLatin(char *path)
{
  assert(Vars.size()>0);

  chdir(path);

  char _cmdline[MAXPATH];
  sprintf(_cmdline,"%s\\%s",path,"latin.exe");
  char *_path=NULL;

  // Note that spawn doesn't work with long file names
  //  return (spawnl(P_WAIT,tmp,tmp,NULL)!=-1);

  bool ErrorCode;
  DWORD                   rc;
  PROCESS_INFORMATION     procInfo;
  STARTUPINFO             startInfo;
  BOOL                    success;

  // Set up members of STARTUPINFO structure.
  memset(&startInfo, '\0', sizeof(startInfo));
  startInfo.cb          = sizeof(STARTUPINFO);
  startInfo.lpReserved  = NULL;
  startInfo.lpReserved2 = NULL;
  startInfo.cbReserved2 = 0;
  startInfo.lpReserved  = NULL;
  startInfo.lpDesktop   = NULL;
  startInfo.lpTitle     = NULL;
  startInfo.lpDesktop   = NULL;
  startInfo.dwFlags     = 0;
  startInfo.dwFillAttribute = 0;
  startInfo.dwFlags         = 0; // STARTF_USESHOWWINDOW | STARTF_USESTDHANDLES;

  success = CreateProcess(
      NULL,
      _cmdline,
      NULL,                 // process security attributes
      NULL,                 // primary thread security attributes
      true,                 // handles are inherited
      0,                    // creation flags
      NULL,                 // use parent's environment
      _path,                // use parent's current directory
      &startInfo,           // STARTUPINFO pointer
      &procInfo);           // receives PROCESS_INFORMATION

  // Wait for the processs to finish
  if (success)
  {
    ErrorCode=true;
 /////////////////////   *processId = procInfo.dwProcessId;
    // Wait until the given process is waiting for user input with no input pending,
    // or until the time-out interval has elapse.
    // This function only works with GUI applications.
    // If a console application calls the function, it returns immediately, with no wait.
    rc = WaitForInputIdle(procInfo.hProcess, INFINITE);
       // The WaitForSingleObject function checks the current state of the specified object.
        // If the object's state is nonsignaled, the calling thread enters an efficient wait state.
        // The thread consumes very little processor time while waiting for the object state
        // to become signaled or the time-out interval to elapse
    rc = WaitForSingleObject(procInfo.hProcess, INFINITE);
    if (rc == WAIT_FAILED)
      ErrorCode = false;
    CloseHandle(procInfo.hProcess);
    CloseHandle(procInfo.hThread);
  }
  else
    ErrorCode = false;
  return ErrorCode;
}

bool Latin::readLHO(char *path)
{
  char lhopath[256];
  assert(Vars.size()>0);
  sprintf(lhopath,"%s\\%s",path,"latinkey.lho");
  ins.readLHO(lhopath);
  return true;
}

bool Latin::writeSampledOutputs(int pid,long odset)
{
  ins.write(pid,odset);
  return true;
}

bool Latin::writeSummaryOutputs(int pid,long odset)
{
  outs.write(pid,odset);
  return true;
}

void Latin::readSampledValues(int pid,long odset,int iteration)
{
  ins.readValues(pid,odset,iteration);
}

void Latin::setControl(int seed,int iteration)
{
  Seed=seed;
  Iteration=iteration;
}
void Latin::changeInputs(int pid,int idx)
{
  ins.changeInputs(pid,idx);
}
/*
void Latin::changeInputs(int pid,long dset,int idx)
{
  ins.changeInputs(pid,dset,idx);
}
*/
void Latin::add(Variable v,IAlias *i)
{
  Vars.insert(Vars.end(),v);
  ins.addVar(i, v.Alias);
}

void Latin::clear()
{
  Vars.clear();
}

ValueMap Latin::loadMap(int idx)
{
  return ins.loadMap(idx);
}

void Latin::getFromMap(ValueMap valMap,int idx)
{
  ins.getFromMap(valMap,idx);
}

void Latin::add(IAlias *ia)
{
  ins.addVar(ia, (*ia).alias);
}


Latin LatinInterface;

