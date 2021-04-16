
#ifndef Testing_H
#define Testing_H
#ifndef BUILD_DLL
  #define FRAMES_API __declspec(dllimport) __stdcall
#else
  #define FRAMES_API __declspec(dllexport) __stdcall
#endif
// API Name: FRAMES 2.0 System API
  #ifdef __cplusplus
    extern "C" {
  #else
    #define bool int
  #endif

   /*
   This is a set of function to facilitate writing testcases and test suites
   for all languages supported in FRAMES 2.0 or other systems.

   */
     
   /*-------------------------------------------------------------------------*/
   /* Documentation for: StartTestSuiteWLink
  A function to create a new test suite.  The file name created will be 
  <Path><Name>_plan.html and <Path><Name>_status.html
  A test suite is made up of a set of test cases.
  
   */

       void FRAMES_API _StartTestSuiteWLink (
       char *  Name, char *  Description, char *  Path, char *  Href);
       #ifndef BUILD_DLL
         #define StartTestSuiteWLink _StartTestSuiteWLink
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: StartTestSuite
  A function to create a new test suite.  The file name created will be 
  <Path><Name>_plan.html and <Path><Name>_status.html
  A test suite is made up of a set of test cases.
  
   */

       void FRAMES_API _StartTestSuite (
       char *  Name, char *  Description, char *  Path);
       #ifndef BUILD_DLL
         #define StartTestSuite _StartTestSuite
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: TestSuiteCompleteWMsg
  A function to signify the end of the test suite and returns whether the test
  was successful or not.  A return value of 1 indicates success and 0 indicates
  failure.  The details of the status are written to the status file.
  
   */

       int FRAMES_API _TestSuiteCompleteWMsg (
       char *  StmtOnSuccess);
       #ifndef BUILD_DLL
         #define TestSuiteCompleteWMsg _TestSuiteCompleteWMsg
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: TestSuiteComplete
  A function to signify the end of the test suite and returns whether the test
  was successful or not.  A return value of 1 indicates success and 0 indicates
  failure.  The details of the status are written to the status file.
  
   */

       int FRAMES_API _TestSuiteComplete (
       );
       #ifndef BUILD_DLL
         #define TestSuiteComplete _TestSuiteComplete
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: NewTestCase
  Add a new test case to the current test suite.
  
   */

       void FRAMES_API _NewTestCase (
       char *  Name, char *  Description);
       #ifndef BUILD_DLL
         #define NewTestCase _NewTestCase
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: TestCaseComplete
  A function to signify the end of the test case and returns whether the test
  was successful or not.  A return value of 1 indicates success and 0 indicates
  failure.  The details of the status are written to the status file.
  The next operation will occur on the testcase before this test case was started.
  
   */

       int FRAMES_API _TestCaseComplete (
       );
       #ifndef BUILD_DLL
         #define TestCaseComplete _TestCaseComplete
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: Status
    A function to directly write information to the status file.
  
   */

       void FRAMES_API _Status (
       char *  Text);
       #ifndef BUILD_DLL
         #define Status _Status
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: Plan
    A function to directly write information to the plan file.
  
   */

       void FRAMES_API _Plan (
       char *  Text);
       #ifndef BUILD_DLL
         #define Plan _Plan
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: Track
    A function to begining tracking a memory location to insure it is deallocated.
  
   */

       void FRAMES_API _Track (
       char *  Description, int location);
       #ifndef BUILD_DLL
         #define Track _Track
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: RemoveTrack
    A function to signify that a location has been deallocated.
  
   */

       void FRAMES_API _RemoveTrack (
       int location);
       #ifndef BUILD_DLL
         #define RemoveTrack _RemoveTrack
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: NoTrackedLeaks
    A function that checks that all allocations have been followed by deallocations.
    The plan and the status documents are automatically updated with this information.
    The state of the test (pass/fail) is also set by this function.
  
   */

       void FRAMES_API _NoTrackedLeaks (
       );
       #ifndef BUILD_DLL
         #define NoTrackedLeaks _NoTrackedLeaks
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: NoOSLeaks
    A function that checks that all allocations have been followed by deallocations 
    using Operating System functions from the start of NewTest.  
    Not Related to Track, RemoveTrack and NoTrackedLeaks.
    The plan and the status documents are automatically updated with this information.
    The state of the test (pass/fail) is also set by this function.
  
   */

       void FRAMES_API _NoOSLeaks (
       );
       #ifndef BUILD_DLL
         #define NoOSLeaks _NoOSLeaks
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: TestError
    A function to directly signify that the current test has failed.
  
   */

       void FRAMES_API _TestError (
       char *  Description);
       #ifndef BUILD_DLL
         #define TestError _TestError
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: Assert
    A function to test that something is true.  If it is not an TestError is 
    called with the description.
  
   */

       void FRAMES_API _Assert (
       bool condition, char *  Description);
       #ifndef BUILD_DLL
         #define Assert _Assert
       #endif

  
  #ifdef __cplusplus
  }
  #endif
#endif
