//---------------------------------------------------------------------------
#include <vcl.h>
#include "frames.h"
#pragma hdrstop

//---------------------------------------------------------------------------
//   Important note about DLL memory management when your DLL uses the
//   static version of the RunTime Library:
//
//   If your DLL exports any functions that pass String objects (or structs/
//   classes containing nested Strings) as parameter or function results,
//   you will need to add the library MEMMGR.LIB to both the DLL project and
//   any other projects that use the DLL.  You will also need to use MEMMGR.LIB
//   if any other projects which use the DLL will be perfomring new or delete
//   operations on any non-TObject-derived classes which are exported from the
//   DLL. Adding MEMMGR.LIB to your project will change the DLL and its calling
//   EXE's to use the BORLNDMM.DLL as their memory manager.  In these cases,
//   the file BORLNDMM.DLL should be deployed along with your DLL.
//
//   To avoid using BORLNDMM.DLL, pass string information using "char *" or
//   ShortString parameters.
//
//   If your DLL uses the dynamic version of the RTL, you do not need to
//   explicitly add MEMMGR.LIB as this will be done implicitly for you
//---------------------------------------------------------------------------

USEUNIT("..\Common Files\robust.cpp");
USEUNIT("..\Common Files\csv.cpp");
USEUNIT("..\Common Files\fcsv.cpp");
USEUNIT("..\Common Files\series.cpp");
USEUNIT("..\Common Files\gid.cpp");
USEUNIT("atoClass.cpp");
USEUNIT("affClass.cpp");
USEUNIT("wffClass.cpp");
USEUNIT("wcfClass.cpp");
USEUNIT("scfClass.cpp");
USEUNIT("bbfClass.cpp");
USEUNIT("exfClass.cpp");
USEUNIT("epfRead.cpp");
USEUNIT("rifRead.cpp");
USEUNIT("hifRead.cpp");
USEUNIT("cfProc.cpp");
USEUNIT("rifProc.cpp");
USEUNIT("hifProc.cpp");
USEUNIT("epfProc.cpp");
USEUNIT("twiProc.cpp");
USEUNIT("Distribution.cpp");
USEUNIT("Results.cpp");
USEUNIT("Sumdata.cpp");
USEUNIT("Equation.cpp");
USEUNIT("Sum3.cpp");
USERES("frames.res");
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
  return 1;
}
//---------------------------------------------------------------------------



