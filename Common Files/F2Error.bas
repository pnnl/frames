Attribute VB_Name = "F2ErrorAPI"
Option Explicit
' API Name: FRAMES Error API
'-------------------------------------------------------------------------
' Documentation for: GetStatus
' Returns the status of the process. This will indicate whether the process interacted completely with the system by calling OpenIO/OpenINI and CloseIO/CloseINI.
DECLARE Function GetStatus LIB "systemio.dll" Alias "__GetStatus@4" (  ByVal PID as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2IOOk
' This function checks to see if the status of the Frame API system. A value of SUCCESS is returned if the system is "Ok". Otherwise, the last registered error for the PID is returned.
DECLARE Function F2IOOk LIB "systemio.dll" Alias "__F2IOOk@4" (  ByVal PID as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2SetError
' Add a error message to the list of error messages. The F2SetError subroutine will add the string value passed by the calling program to the Error list and and then return control to the calling program.
DECLARE Sub F2SetError LIB "systemio.dll" Alias "__F2SetError@8" (  ByVal PID as long,  ByVal errorName as string ) 

'-------------------------------------------------------------------------
' Documentation for: F2SetWarning
' Add a warning message to the list of warning messages. The F2SetWarning subroutine will add the string value passed by the calling program to the Warning list and then return control to the calling program.
DECLARE Sub F2SetWarning LIB "systemio.dll" Alias "__F2SetWarning@8" (  ByVal PID as long,  ByVal warningName as string ) 

'-------------------------------------------------------------------------
' Documentation for: F2ReadError
' Get an error message using error code. The ReadError function call has a process id and code integer as input. It returns The message to associated with the error code.
DECLARE Function DLLF2ReadError LIB "systemio.dll" Alias "__F2ReadError@12" (  ByVal PID as long,  ByVal code as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2ReadWarning
' Get an warning message using error code. The ReadWarning function call has a process id and code integer as input. It returns The message to associated with the warning code.
DECLARE Function DLLF2ReadWarning LIB "systemio.dll" Alias "__F2ReadWarning@12" (  ByVal PID as long,  ByVal code as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2ClearErrors
' Clear error message list.
DECLARE Sub F2ClearErrors LIB "systemio.dll" Alias "__F2ClearErrors@4" (  ByVal PID as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2ClearWarnings
' Clear warnings message list.
DECLARE Sub F2ClearWarnings LIB "systemio.dll" Alias "__F2ClearWarnings@4" (  ByVal PID as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2ErrorCount
' Get the number of errors for the associated pid.
DECLARE Function F2ErrorCount LIB "systemio.dll" Alias "__F2ErrorCount@4" (  ByVal PID as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2WarningCount
' Get the number of warnings for the associated pid.
DECLARE Function F2WarningCount LIB "systemio.dll" Alias "__F2WarningCount@4" (  ByVal PID as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2WriteErrorFile
' Write all the error messages to a file. Each error message will be put on a newline.
DECLARE Sub F2WriteErrorFile LIB "systemio.dll" Alias "__F2WriteErrorFile@8" (  ByVal PID as long,  ByVal errorFile as string ) 

'-------------------------------------------------------------------------
' Documentation for: F2WriteWarningFile
' Write all the warning messages to a file. Each warning message will be put on a newline.
DECLARE Sub F2WriteWarningFile LIB "systemio.dll" Alias "__F2WriteWarningFile@8" (  ByVal PID as long,  ByVal warningFile as string ) 

Function F2ReadError (  ByVal PID as long,  ByVal code as long )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLF2ReadError(PID, code, retStr3)
      F2ReadError=StripTerminator(retStr3)
End  Function 

Function F2ReadWarning (  ByVal PID as long,  ByVal code as long )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLF2ReadWarning(PID, code, retStr3)
      F2ReadWarning=StripTerminator(retStr3)
End  Function 
