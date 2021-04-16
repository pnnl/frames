//---------------------------------------------------------------------------
#include <vcl.h>
#include <process.h>
#include <stdio.h>
#include <dir.h>
#pragma hdrstop

#include "launch.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)
int _export _stdcall launch(char *args)
{
  int result, i;
  char *ptr, *vargs[20];

  ptr = args;

  i=0;
  while (ptr && i<20)
    {
    vargs[i]=ptr;
    args=ptr;
    ptr = strchr(args,' ');
    if (ptr)
      {
      *ptr='\0';
      ptr++;
      i++;
      }
    }
    vargs[++i]='\0';

    result = spawnvp(P_NOWAIT, vargs[0], vargs);
    return result;
}
//------------------------------------------------------------------------------
int _export _stdcall launchProcess(char *args)
{
    STARTUPINFO StartupInfo;
    ZeroMemory(&StartupInfo, sizeof(STARTUPINFO));
    StartupInfo.cb = sizeof(STARTUPINFO);
    PROCESS_INFORMATION ProcessInfo;
    if (CreateProcess(NULL,args,NULL,NULL,TRUE,NORMAL_PRIORITY_CLASS,NULL,NULL,&StartupInfo,&ProcessInfo))
      {
      WaitForInputIdle(ProcessInfo.hProcess,INFINITE);
      WaitForSingleObject(ProcessInfo.hProcess,INFINITE);
      CloseHandle(ProcessInfo.hProcess);
      CloseHandle(ProcessInfo.hThread);
      return 0;
      }
    return 1;
}

int _export _stdcall launchProcess2(char *args, char *curDir)
{
    STARTUPINFO StartupInfo;
    ZeroMemory(&StartupInfo, sizeof(STARTUPINFO));
    StartupInfo.cb = sizeof(STARTUPINFO);
    PROCESS_INFORMATION ProcessInfo;
    if (CreateProcess(NULL,args,NULL,NULL,TRUE,NORMAL_PRIORITY_CLASS,NULL,curDir,&StartupInfo,&ProcessInfo))
      {
      WaitForInputIdle(ProcessInfo.hProcess,INFINITE);
      WaitForSingleObject(ProcessInfo.hProcess,INFINITE);
      CloseHandle(ProcessInfo.hProcess);
      CloseHandle(ProcessInfo.hThread);
      return 0;
      }
//  MessageBox(NULL,args,"launchProcess2 Failed",MB_OK);
    return 1;
}

long _export _stdcall launchProcess3(char *args, char *curDir, long *hProcess, long *hThread)
{
    STARTUPINFO StartupInfo;
    ZeroMemory(&StartupInfo, sizeof(STARTUPINFO));
    StartupInfo.cb = sizeof(STARTUPINFO);
    PROCESS_INFORMATION ProcessInfo;

    if (CreateProcess(NULL,args,NULL,NULL,TRUE,NORMAL_PRIORITY_CLASS,NULL,curDir,&StartupInfo,&ProcessInfo))
      {
      *hProcess = (long)ProcessInfo.hProcess;
      *hThread = (long)ProcessInfo.hThread;

      return 0L;
      }
    else {
      MessageBox(NULL,args,"launchProcess3 Failed",MB_OK);
      return 1L;
    }
}

int _export _stdcall launchProcess4(char *cmdline, char *apppath)
{
  int rc;
  rc = spawnlp (P_WAIT, apppath, cmdline, NULL);
  return rc;
}


