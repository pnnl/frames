#ifndef FRAMES2API_F2Error_H
#define FRAMES2API_F2Error_H

#ifndef BUILD_DLL
  #define FRAMES2API __declspec(dllimport) __stdcall
#else
  #define FRAMES2API __declspec(dllexport) __stdcall
#endif

#ifdef __cplusplus
  extern "C" {
#else
  #define bool int
#endif

  // API Name: FRAMES Error API
  /* 
       These functions and subroutines provide access to the error and 
       warning files.    If programming in FORTRAN and a warning is larger 
       than 80 characters then    multiple calls to the subroutine will be 
       required to include the entire     description. The F2Ok function 
       should be called after every call to check    that the function 
       succeeded.
  */
  /*============================================================================
     Documentation for: GetStatus
    ============================================================================
       Returns the status of the process. This will indicate whether the 
       process interacted completely with the system by calling 
       OpenIO/OpenINI and CloseIO/CloseINI.
  */
  int  FRAMES2API _GetStatus(int _PID);
    #ifndef BUILD_DLL
      #define GetStatus _GetStatus
    #endif

  /*============================================================================
     Documentation for: F2IOOk
    ============================================================================
       This function checks to see if the status of the Frame API system. A 
       value of SUCCESS is returned if the system is "Ok". Otherwise, the 
       last registered error for the PID is returned.
  */
  int  FRAMES2API _F2IOOk(int _PID);
    #ifndef BUILD_DLL
      #define F2IOOk _F2IOOk
    #endif

  /*============================================================================
     Documentation for: F2SetError
    ============================================================================
       Add a error message to the list of error messages. The F2SetError 
       subroutine will add the string value passed by the calling program to 
       the Error list and and then return control to the calling program.
  */
  void  FRAMES2API _F2SetError(int _PID,char* _errorName);
    #ifndef BUILD_DLL
      #define F2SetError _F2SetError
    #endif

  /*============================================================================
     Documentation for: F2SetWarning
    ============================================================================
       Add a warning message to the list of warning messages. The 
       F2SetWarning subroutine will add the string value passed by the 
       calling program to the Warning list and then return control to the 
       calling program.
  */
  void  FRAMES2API _F2SetWarning(int _PID,char* _warningName);
    #ifndef BUILD_DLL
      #define F2SetWarning _F2SetWarning
    #endif

  /*============================================================================
     Documentation for: F2ReadError
    ============================================================================
       Get an error message using error code. The ReadError function call 
       has a process id and code integer as input. It returns The message to 
       associated with the error code.
  */
  int  FRAMES2API _F2ReadError(int _PID,int _code,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2ReadError _F2ReadError
    #endif

  /*============================================================================
     Documentation for: F2ReadWarning
    ============================================================================
       Get an warning message using error code. The ReadWarning function 
       call has a process id and code integer as input. It returns The 
       message to associated with the warning code.
  */
  int  FRAMES2API _F2ReadWarning(int _PID,int _code,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2ReadWarning _F2ReadWarning
    #endif

  /*============================================================================
     Documentation for: F2ClearErrors
    ============================================================================
       Clear error message list.
  */
  void  FRAMES2API _F2ClearErrors(int _PID);
    #ifndef BUILD_DLL
      #define F2ClearErrors _F2ClearErrors
    #endif

  /*============================================================================
     Documentation for: F2ClearWarnings
    ============================================================================
       Clear warnings message list.
  */
  void  FRAMES2API _F2ClearWarnings(int _PID);
    #ifndef BUILD_DLL
      #define F2ClearWarnings _F2ClearWarnings
    #endif

  /*============================================================================
     Documentation for: F2ErrorCount
    ============================================================================
       Get the number of errors for the associated pid.
  */
  int  FRAMES2API _F2ErrorCount(int _PID);
    #ifndef BUILD_DLL
      #define F2ErrorCount _F2ErrorCount
    #endif

  /*============================================================================
     Documentation for: F2WarningCount
    ============================================================================
       Get the number of warnings for the associated pid.
  */
  int  FRAMES2API _F2WarningCount(int _PID);
    #ifndef BUILD_DLL
      #define F2WarningCount _F2WarningCount
    #endif

  /*============================================================================
     Documentation for: F2WriteErrorFile
    ============================================================================
       Write all the error messages to a file. Each error message will be 
       put on a newline.
  */
  void  FRAMES2API _F2WriteErrorFile(int _PID,char* _errorFile);
    #ifndef BUILD_DLL
      #define F2WriteErrorFile _F2WriteErrorFile
    #endif

  /*============================================================================
     Documentation for: F2WriteWarningFile
    ============================================================================
       Write all the warning messages to a file. Each warning message will 
       be put on a newline.
  */
  void  FRAMES2API _F2WriteWarningFile(int _PID,char* _warningFile);
    #ifndef BUILD_DLL
      #define F2WriteWarningFile _F2WriteWarningFile
    #endif

#ifdef __cplusplus
  }
#endif
#endif
