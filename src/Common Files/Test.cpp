
#include "Test.h"
#include <sstream>
using namespace std;


_CrtMemState b;
Tests tests;
strstream debugMessages;
int LineInfo::count=0;

//#ifndef BUILD_DLL 
#ifdef TEST_MAIN

int MyReportHook( int reportType, char *message, int *returnValue )
{
  debugMessages << message ;
  return *returnValue;
}

void main() 
{
  PerformTests();
  tests.doTests();
  CleanUp();
}
#endif