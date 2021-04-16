#pragma hdrstop
#include <dir.h>
//#include <owl/framewin.h>
//#include <owl/applicat.h>
//#include <owl/docmanag.h>
//#include <classlib/file.h>
//#include <system.h>
#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <direct.h>
#include <process.h>
#include <windows.h>
#include <string.h>
#include "sumdata.h"
#include "gid.h"
#include "equation.h"
#include "readfunc.h"
#include "launch.h"
//extern TApplication* Application;


char ERRName[256];
char msg[256];
char *fuiAppPath;

extern "C"
{
  int _export _stdcall sum3Init(char *, char *, char *, char *, char *);
  int _export _stdcall sum3RunIter(int iter);
  int _export _stdcall sum3Cleanup();
}
int CopyAll(char *TempLoc,char *PermLoc);
SUM App;
struct LOADPARMS
{
    WORD   segEnv;                  /* child environment  */
    LPSTR  lpszCmdLine;             /* child command tail */
    LPWORD lpwShow;                 /* how to show child  */
    LPWORD lpwReserved;             /* must be NULL       */
};
char *keyname="LATINKEY";
char sensin[MAXPATH];   // ="~sensi.gid";
char sensout[MAXPATH];  // ="~senso.*";
char workdir[MAXPATH];

//------------------------------------------------------------------------------
char *Time()
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
char *Date()
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
/*
void SUM::writeError(char *s,char *l2,char *l3)
  {
    FILE *f;
    f=fopen(ERRName,"wt");
    fprintf(f,"Error\n");
    fprintf(f,"%s\n",s);
    if (l2!=NULL) fprintf(f,"%s\n",l2);
    if (l3!=NULL) fprintf(f,"%s\n",l3);
    fflush(f);
    fclose(f);
    Ok=0;
  }
*/
//------------------------------------------------------------------------------
int writeSens_Id(char *keyname)
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
    return 1;
  }
  else
    return 0;
}
//------------------------------------------------------------------------------
void SUMData::cleanup()
{
  int i;
  for (i=0;i<ResCount;i++)
    Res[i].reset();
}
//------------------------------------------------------------------------------
SUMData::~SUMData()
{
  if (Vars) delete[] Vars;
  if (Res) delete[] Res;
  if (Fui) delete[] Fui;
}
//------------------------------------------------------------------------------
int SUM::writeKey()
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

  if (!writeSens_Id(keyname)) return 0;
  sprintf(tmp,"%s%s.key",fuiAppPath,keyname);
  k=fopen(tmp, "wt");
  if (k==NULL) return 0;
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
  sprintf(tmp, "ITERATE %d",Data->Iteration);
  fprintf(k, "%-121s\n", tmp);
  sprintf(tmp, "SEED %E",Data->Seed);
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
  if (!Data->writeKey(k)) return 0;
  if (!Data->writeCorMat(k)) return 0;
  fprintf(k, "%-121s\n", "EXECUTE");
  fprintf(k, "%-121s\n", "END");
  fclose(k);
  return 1;
}
//------------------------------------------------------------------------------
int SUM::spawnLatin()
{
//  int i;
  char tmp[MAXPATH]/*, cmd[MAXPATH]*/;
  struct ffblk ffblk;

  sprintf(tmp,"%s%s",fuiAppPath,"latin.exe");

//  i = launchProcess4(tmp, tmp);
  if (launchProcess4(tmp, tmp))
  {
      MessageBox(NULL, "Error launching latin.exe", "spawnLatin", MB_OK);
      return 0;
  }

  if (strcmpi(fuiAppPath,workdir))
  {
    sprintf(tmp,"xcopy /Y %slatinkey.* %s",fuiAppPath,workdir);
    if (launchProcess4(tmp, "xcopy"))
    {
      MessageBox(NULL, "Error launching xcopy", "spawnLatin", MB_OK);
      return 0;
    }
  }
  // check for latinkey.* files in working directory
  sprintf(tmp,"%slatinkey.lho",workdir);
  if (findfirst(tmp,&ffblk,0))
    return 0;
  return 1;
}

/*------------------------------------------------------------------------------
int SUM::spawnLatin()
{
  char tmp[MAXPATH]; // cmd[MAXPATH];
  STARTUPINFO StartupInfo;
  ZeroMemory(&StartupInfo, sizeof(STARTUPINFO));
  StartupInfo.cb = sizeof(STARTUPINFO);
  PROCESS_INFORMATION ProcessInfo;

  sprintf(tmp,"%s%s",fuiAppPath,"latin.exe");

  if (0==launchProcess4(tmp, tmp)) {
//
//  if (CreateProcess(tmp,NULL,NULL,NULL,TRUE,NORMAL_PRIORITY_CLASS,NULL,fuiAppPath,&StartupInfo,&ProcessInfo))
//    {
//    WaitForInputIdle(ProcessInfo.hProcess,INFINITE);
//    WaitForSingleObject(ProcessInfo.hProcess,INFINITE);
//    CloseHandle(ProcessInfo.hProcess);
//    CloseHandle(ProcessInfo.hThread);
//
    sprintf(tmp,"xcopy /Y %slatinkey.* %s",fuiAppPath,workdir);
//  launchProcess(tmp);
    if (0==launchProcess4(tmp, "xcopy")) {
      // check for latinkey.* files in working directory
      struct ffblk ffblk;
      int found;
      sprintf(tmp,"%slatinkey.lho",workdir);
      found = findfirst(tmp,&ffblk,0);
      if (0==found) return 1;
      }
    else {
      MessageBox(NULL, "Error launching xcopy", "spawnLatin", MB_OK);
      return 0;
      }
    }
  else {
      MessageBox(NULL, "Error launching latin.exe", "spawnLatin", MB_OK);
      return 0;
    }
  return 0;
  }
*/
//------------------------------------------------------------------------------
int SUM::writeTempGID()
{
  char tmp[MAXPATH],sen[MAXPATH],*line1,*line2,*line3;
  sprintf(tmp,"%s%s.lho",workdir,keyname);
//  sprintf(tmp,"%s%s.lho",fuiAppPath,keyname);
  Data->readLHO(tmp,Current);
  if (!Equation(Data,&line1,&line2,&line3))
  {
    writeError(line1,line2,line3);
    return 0;
  }//  sprintf(tmp,"%s.gid",RunName);
  sprintf(tmp,"%s.gid",FUIName);
  sprintf(sen,"%s.gid",sensin);
  Data->writeTempGID(tmp,sen);
  return 1;
}
//------------------------------------------------------------------------------
int SUM::spawnBatch(char *binpath)
  {
    char tmp[512]; // tmp2[512];
    bool rc;
    sprintf(tmp,"%s %s %s %s %d",this->binpath,Glyph,sensin,sensout,Scenario-1);
//  rc = launchProcess2(tmp, fuiAppPath);
    rc = launchProcess4(tmp, this->binpath);
    return rc;
}
//------------------------------------------------------------------------------
int SUM::processResults()
{
    if (!Data->readResults(sensin))
    {
       writeError("processResults(");
       writeError("Alias: ",Data->Res->Alias);
       writeError("ReadResults returned false");
       return 0;
    }
    if (!Data->writeSUF(&SUF,Current,Count)) return 0;
    return 1;
}
//------------------------------------------------------------------------------
int SUM::sum3Init(char *argv1, char *argv2, char *argv3, char *argv4, char *argv5)
{
    char fuiDrive[MAXDRIVE], fuiDir[MAXDIR], fuiName[MAXFILE], fuiExt[MAXEXT];

	// Create a large buffer.
        char* lpstrBuffer = new char[255+1];

	// Fill the buffer with the value, setting the default as an empty string.
	GetPrivateProfileString( "App Path", "Fui", "", lpstrBuffer, 255, "FramesUI.ini");

	// Create the buffer, using the strlen function. Adding one for the
	// Null-terminator.
	fuiAppPath = new char[strlen(lpstrBuffer)+2];

	// Copy the string from the buffer to the query string variable.
    sprintf(fuiAppPath,"%s",lpstrBuffer);
  //  fuiAppPath = "C:\\SOURCE\\Sens\\" ;

	// lpstrBuffer can now be used to get new values.



    strcpy(FUIName,argv1);
    strcpy(RunName,argv2);
    Scenario=atoi(argv3);
    SUMNumber=atoi(argv4);
    strcpy(Glyph,argv5);
    fnsplit(FUIName,fuiDrive,fuiDir,fuiName,fuiExt);
    fnmerge(sensin,fuiDrive,fuiDir,"~sens","");
    fnmerge(sensout,fuiDrive,fuiDir,"~senso","");
    fnmerge(workdir,fuiDrive,fuiDir,"","");

//  fnmerge(binpath,fuiDir,"DoAll","exe");

    sprintf(binpath,"%sDoAll.exe",fuiAppPath);

    sprintf(ERRName,"%s.err",RunName);
    writeError("Unknown Error Generated"); //should not be seen
    Ok=1;
    Data=new SUMData();
    sprintf(SUFName,"%s.SUF",RunName);    // output file for FUI

    SUF.open(SUFName,_WRITE);

    sprintf(GIDName,"%s.gid",FUIName);    // input GID file from FUI
    GID=Open_GID(GIDName);
    if (GID==NULL && Ok)
    {
      writeError("unable to open gid file");
      MessageBox(NULL, "unable to open gid file", "sum3Init", MB_OK);
      Ok=0;
    }
    if (Ok)
    {
      //  printf("Loading gid file\n");
      Ok = Load_GID(GID,Scenario,Glyph);
      if (!Ok)
      {
        writeError("unable to read gid section");
        MessageBox(NULL, "unable to read gid section", "sum3Init", MB_OK);
      }
    }
    if (Ok)
    {
      //  printf("Reading SUM data\n");
      Ok = Data->read(GID,Scenario);
      if (!Ok)
      {
        writeError("reading SUM data");
        MessageBox(NULL, "error reading SUM data", "sum3Init", MB_OK);
      }
    }
    if (Ok)
    {
      //  printf("Writing key file\n");
      Ok = writeKey();
      if (!Ok)
      {
        writeError("writing key file");
        MessageBox(NULL, "error writing key file", "sum3Init", MB_OK);
      }
    }
    if (Ok)
    {
      //  printf("Spawning Latin\n");
      Ok = spawnLatin();
      if (!Ok)
      {
        writeError("spawning Latin.exe");
        MessageBox(NULL, "error spawning Latin.exe", "sum3Init", MB_OK);
      }
    }
    SUF.close();
    if (Ok)
      return Data->Iteration;
    else
      return 0;
}
//------------------------------------------------------------------------------
int SUM::sum3RunIter(int iter)
{
    int i;
    char tmp[MAXPATH];
    char binpath[MAXPATH];
    struct ffblk ffblk;
    int done;
    for (i=2;i<=2;i++)
    {
      if (i==1)
        sprintf(tmp,"%s.*",sensin);
      else
        sprintf(tmp,"%s.*",sensout);

      done = findfirst(tmp,&ffblk,0);
      while (!done)
      {
        sprintf(tmp,"%s%s",workdir,ffblk.ff_name);
        unlink(tmp);
        done = findnext(&ffblk);
      }
    }
    Ok = 1;
    Current = iter-1;
    sprintf(tmp,"%s",SUFName);
    SUF.open(SUFName,APPEND);
    sprintf(tmp,"");
    Ok = writeTempGID();
    if (Ok)
    {
      Ok = spawnBatch(binpath);
      sprintf(msg,"\n#1 %d",Ok);
      writeError(msg);
      sprintf(tmp,"%s.err",sensout);
      writeError(tmp);
      Ok = findfirst(tmp,&ffblk,0);
      sprintf(msg,"\n#2 %d",Ok);
      writeError(msg);
    }
    else {
      sprintf(msg,"spawning %s",binpath);
      writeError(msg);
    }
    if (Ok)
    {
      Ok = processResults();
      sprintf(msg,"\n#3 %d",Ok);
      writeError(msg);
    }
    SUF.close();
    Data->cleanup();
    sprintf(msg,"\nreturn Ok %d",Ok);
    writeError(msg);
    return Ok;
}
//------------------------------------------------------------------------------
int SUM::testResults()
{
  processResults();
  SUF.close();
  return Ok;
}
//------------------------------------------------------------------------------
int SUM::sum3Cleanup()
{
//SUF.close();
  Close_GID(GID);
  if (Ok) unlink(ERRName);
  delete Data;
  return Ok;
}

/*int OwlMain(int argc,char **argv)
  {
  SUM App;
  MainWindow = new TListBoxWindow("ListBox Example");
  MainWindow->CmStandard();
  return App.Run(argc,argv);
  }
*/
//------------------------------------------------------------------------------
int _stdcall sum3Init(char *argv1, char *argv2, char *argv3, char *argv4, char *argv5)
{
//  unlink("bugfile.txt");
  return App.sum3Init(argv1, argv2, argv3, argv4, argv5);
}
//------------------------------------------------------------------------------
int _stdcall sum3Cleanup()
{
  return App.sum3Cleanup();
}
//------------------------------------------------------------------------------
int _stdcall sum3RunIter(int iter)
{
  int rc;
  rc = App.sum3RunIter(iter);
  if (rc)
    unlink (ERRName);
  return rc;
}
//------------------------------------------------------------------------------
int _stdcall sum3TestResults()
{
  return App.testResults();
}


