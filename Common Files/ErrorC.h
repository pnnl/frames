
#ifndef Error_H
#define Error_H
#ifndef BUILD_DLL
  #define FRAMES_API __declspec(dllimport) __stdcall
#else
  #define FRAMES_API __declspec(dllexport) __stdcall
#endif
// API Name: FRAMES Error
  #ifdef __cplusplus
    extern "C" {
  #else
    #define bool int
  #endif

   /*
   These functions and subroutines provide access to the error and warning files.
   If programming in FORTRAN and a warning is larger than 80 characters then
   multiple calls to the subroutine will be required to include the entire 
   description. Errors will be limited to 128 characters in FORTRAN. 
   
   Functions return a success flag if there are no errors. 
 
   */
     
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SetError
    Set an error message. The Error subroutine will add the string value passed by the calling program to the Error File and then terminate the calling program. 
  
   */

       void FRAMES_API _SetError (
       int PID, char *  errorName);
       #ifndef BUILD_DLL
         #define SetError _SetError
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SetWarning
    Add a warning message to the list of messages. The Warning subroutine will add the string value passed by the calling program to the Warning File and then return control to the calling program.
  
   */

       void FRAMES_API _SetWarning (
       int PID, char *  warningName);
       #ifndef BUILD_DLL
         #define SetWarning _SetWarning
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: ReadError
    Get an error message using error code. The ReadError function call has a process id and code integer as input, and an empty msg string to output the error message. 
  
   */

       int FRAMES_API _ReadError (
       int PID, int code, char *  msg);
       #ifndef BUILD_DLL
         #define ReadError _ReadError
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: ReadWarning
    Get an warning message using error code. The ReadWarning function call has a process id and code integer as input, and an empty msg string to output the warning message. 
  
   */

       int FRAMES_API _ReadWarning (
       int PID, int code, char *  msg);
       #ifndef BUILD_DLL
         #define ReadWarning _ReadWarning
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: ClearErrors
    Clear error message list.
  
   */

       void FRAMES_API _ClearErrors (
       int PID);
       #ifndef BUILD_DLL
         #define ClearErrors _ClearErrors
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: ClearWarnings
    Clear warnings message list.
  
   */

       void FRAMES_API _ClearWarnings (
       int PID);
       #ifndef BUILD_DLL
         #define ClearWarnings _ClearWarnings
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetErrorList
    Get all the error messages as a single string with a delimiter. Using \n will cause line feeds to be put between the messages.
  
   */

       void FRAMES_API _GetErrorList (
       int PID, char *  delimiter, char *  list);
       #ifndef BUILD_DLL
         #define GetErrorList _GetErrorList
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: WriteErrorFile
    Get all the error messages as a single string with a delimiter. Using \n will cause line feeds to be put between the messages.
  
   */

       int FRAMES_API _WriteErrorFile (
       int PID, char *  errorFile);
       #ifndef BUILD_DLL
         #define WriteErrorFile _WriteErrorFile
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetWarningList
    Get all the warning messages as a single string with a delimiter. Using \n will cause line feeds to be put between the messages.
  
   */

       void FRAMES_API _GetWarningList (
       int PID, char *  delimiter, char *  list);
       #ifndef BUILD_DLL
         #define GetWarningList _GetWarningList
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: WriteWarningFile
    Get all the warning messages as a single string with a delimiter. Using \n will cause line feeds to be put between the messages.
  
   */

       int FRAMES_API _WriteWarningFile (
       int PID, char *  warningFile);
       #ifndef BUILD_DLL
         #define WriteWarningFile _WriteWarningFile
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetCountOfWarningsAndErrors
    Get all the count of errors and warnings for a given PID.
  
   */

       int FRAMES_API _GetCountOfWarningsAndErrors (
       int PID, int * warnings, int * errors);
       #ifndef BUILD_DLL
         #define GetCountOfWarningsAndErrors _GetCountOfWarningsAndErrors
       #endif

  
  #ifdef __cplusplus
  }
  #endif
#endif
