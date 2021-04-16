//---------------------------------------------------------------------------
#ifndef launchH
#define launchH
//---------------------------------------------------------------------------

extern "C"
{
int _stdcall launch(char *args);
int _stdcall launchProcess(char *args);
int _stdcall launchProcess2(char *args, char *curDir);
long _stdcall launchProcess3(char *args, char *curDir, long *hProcess, long *hThread);
int _stdcall launchProcess4(char *args, char *curDir);
}
#endif

