Attribute VB_Name = "TestingAPI"
Option Explicit
' API Name: FRAMES 2.0 System API
'-------------------------------------------------------------------------
' Documentation for: StartTestSuiteWLink
' A function to create a new test suite. The file name created will be <Path><Name>_plan.html and <Path><Name>_status.html A test suite is made up of a set of test cases.
DECLARE Sub StartTestSuiteWLink LIB "testing.dll" Alias "__StartTestSuiteWLink@16" (  ByVal Name as string,  ByVal Description as string,  ByVal Path as string,  ByVal Href as string ) 

'-------------------------------------------------------------------------
' Documentation for: StartTestSuite
' A function to create a new test suite. The file name created will be <Path><Name>_plan.html and <Path><Name>_status.html A test suite is made up of a set of test cases.
DECLARE Sub StartTestSuite LIB "testing.dll" Alias "__StartTestSuite@12" (  ByVal Name as string,  ByVal Description as string,  ByVal Path as string ) 

'-------------------------------------------------------------------------
' Documentation for: TestSuiteCompleteWMsg
' A function to signify the end of the test suite and returns whether the test was successful or not. A return value of 1 indicates success and 0 indicates failure. The details of the status are written to the status file.
DECLARE Function TestSuiteCompleteWMsg LIB "testing.dll" Alias "__TestSuiteCompleteWMsg@4" (  ByVal StmtOnSuccess as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: TestSuiteComplete
' A function to signify the end of the test suite and returns whether the test was successful or not. A return value of 1 indicates success and 0 indicates failure. The details of the status are written to the status file.
DECLARE Function TestSuiteComplete LIB "testing.dll" Alias "__TestSuiteComplete@0" (  )  as integer 

'-------------------------------------------------------------------------
' Documentation for: NewTestCase
' Add a new test case to the current test suite.
DECLARE Sub NewTestCase LIB "testing.dll" Alias "__NewTestCase@8" (  ByVal Name as string,  ByVal Description as string ) 

'-------------------------------------------------------------------------
' Documentation for: TestCaseComplete
' A function to signify the end of the test case and returns whether the test was successful or not. A return value of 1 indicates success and 0 indicates failure. The details of the status are written to the status file. The next operation will occur on the testcase before this test case was started.
DECLARE Function TestCaseComplete LIB "testing.dll" Alias "__TestCaseComplete@0" (  )  as integer 

'-------------------------------------------------------------------------
' Documentation for: Status
' A function to directly write information to the status file.
DECLARE Sub Status LIB "testing.dll" Alias "__Status@4" (  ByVal Text as string ) 

'-------------------------------------------------------------------------
' Documentation for: Plan
' A function to directly write information to the plan file.
DECLARE Sub Plan LIB "testing.dll" Alias "__Plan@4" (  ByVal Text as string ) 

'-------------------------------------------------------------------------
' Documentation for: Track
' A function to begining tracking a memory location to insure it is deallocated.
DECLARE Sub Track LIB "testing.dll" Alias "__Track@8" (  ByVal Description as string,  ByVal location as long ) 

'-------------------------------------------------------------------------
' Documentation for: RemoveTrack
' A function to signify that a location has been deallocated.
DECLARE Sub RemoveTrack LIB "testing.dll" Alias "__RemoveTrack@4" (  ByVal location as long ) 

'-------------------------------------------------------------------------
' Documentation for: NoTrackedLeaks
' A function that checks that all allocations have been followed by deallocations. The plan and the status documents are automatically updated with this information. The state of the test (pass/fail) is also set by this function.
DECLARE Sub NoTrackedLeaks LIB "testing.dll" Alias "__NoTrackedLeaks@0" (  ) 

'-------------------------------------------------------------------------
' Documentation for: NoOSLeaks
' A function that checks that all allocations have been followed by deallocations using Operating System functions from the start of NewTest. Not Related to Track, RemoveTrack and NoTrackedLeaks. The plan and the status documents are automatically updated with this information. The state of the test (pass/fail) is also set by this function.
DECLARE Sub NoOSLeaks LIB "testing.dll" Alias "__NoOSLeaks@0" (  ) 

'-------------------------------------------------------------------------
' Documentation for: TestError
' A function to directly signify that the current test has failed.
DECLARE Sub TestError LIB "testing.dll" Alias "__TestError@4" (  ByVal Description as string ) 

'-------------------------------------------------------------------------
' Documentation for: Assert
' A function to test that something is true. If it is not an TestError is called with the description.
DECLARE Sub Assert LIB "testing.dll" Alias "__Assert@8" (  ByVal condition as bool,  ByVal Description as string ) 
