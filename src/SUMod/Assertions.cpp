#include "Assertions.h"
#include "c:\program files\framesv2\Developer\F2ModuleDevC.h"
#include "c:\program files\framesv2\Developer\F2SystemDevC.h"
#include "c:\program files\framesv2\Developer\ErrorC.h"
#include <stdio.h>

#include <iostream>
using namespace std;

extern int pid;

void _assert(bool check,char *strCheck,char *file,int line)
{
  static char msg[80];
//  char c;
  if (!check)
  {
    sprintf(msg,"%s %s %d",strCheck,file,line);
//    cout << msg <<endl;
//    cin >> c;
    SetWarning(pid,msg);
    ModuleDevClose(pid,0);
    exit(0);
  }
}

