Attribute VB_Name = "ErrorAPI"
Option Explicit
' API Name: FRAMES Error
'-------------------------------------------------------------------------
' Documentation for: SetError
' Set an error message. The Error subroutine will add the string value passed by the calling program to the Error File and then terminate the calling program.
DECLARE Sub SetError LIB "systemio.dll" Alias "__SetError@8" (  ByVal PID as long,  ByVal errorName as string ) 

'-------------------------------------------------------------------------
' Documentation for: SetWarning
' Add a warning message to the list of messages. The Warning subroutine will add the string value passed by the calling program to the Warning File and then return control to the calling program.
DECLARE Sub SetWarning LIB "systemio.dll" Alias "__SetWarning@8" (  ByVal PID as long,  ByVal warningName as string ) 

'-------------------------------------------------------------------------
' Documentation for: ReadError
' Get an error message using error code. The ReadError function call has a process id and code integer as input, and an empty msg string to output the error message.
DECLARE Function DLLReadError LIB "systemio.dll" Alias "__ReadError@12" (  ByVal PID as long,  ByVal code as long,  ByVal msg as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ReadWarning
' Get an warning message using error code. The ReadWarning function call has a process id and code integer as input, and an empty msg string to output the warning message.
DECLARE Function DLLReadWarning LIB "systemio.dll" Alias "__ReadWarning@12" (  ByVal PID as long,  ByVal code as long,  ByVal msg as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ClearErrors
' Clear error message list.
DECLARE Sub ClearErrors LIB "systemio.dll" Alias "__ClearErrors@4" (  ByVal PID as long ) 

'-------------------------------------------------------------------------
' Documentation for: ClearWarnings
' Clear warnings message list.
DECLARE Sub ClearWarnings LIB "systemio.dll" Alias "__ClearWarnings@4" (  ByVal PID as long ) 

'-------------------------------------------------------------------------
' Documentation for: GetErrorList
' Get all the error messages as a single string with a delimiter. Using \n will cause line feeds to be put between the messages.
DECLARE Sub DLLGetErrorList LIB "systemio.dll" Alias "__GetErrorList@12" (  ByVal PID as long,  ByVal delimiter as string,  ByVal list as string ) 

'-------------------------------------------------------------------------
' Documentation for: WriteErrorFile
' Get all the error messages as a single string with a delimiter. Using \n will cause line feeds to be put between the messages.
DECLARE Function WriteErrorFile LIB "systemio.dll" Alias "__WriteErrorFile@8" (  ByVal PID as long,  ByVal errorFile as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetWarningList
' Get all the warning messages as a single string with a delimiter. Using \n will cause line feeds to be put between the messages.
DECLARE Sub DLLGetWarningList LIB "systemio.dll" Alias "__GetWarningList@12" (  ByVal PID as long,  ByVal delimiter as string,  ByVal list as string ) 

'-------------------------------------------------------------------------
' Documentation for: WriteWarningFile
' Get all the warning messages as a single string with a delimiter. Using \n will cause line feeds to be put between the messages.
DECLARE Function WriteWarningFile LIB "systemio.dll" Alias "__WriteWarningFile@8" (  ByVal PID as long,  ByVal warningFile as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetCountOfWarningsAndErrors
' Get all the count of errors and warnings for a given PID.
DECLARE Function GetCountOfWarningsAndErrors LIB "systemio.dll" Alias "__GetCountOfWarningsAndErrors@12" (  ByVal PID as long, warnings as long, errors as long )  as integer 

Function ReadError (  ByVal PID as long,  ByVal code as long, msg as string   )  as integer 
      Dim retStr3 as String * MAXFIELD
      ReadError = DLLReadError(PID, code, retstr3)
      msg=StripTerminator(retStr3)
End  Function 

Function ReadWarning (  ByVal PID as long,  ByVal code as long, msg as string   )  as integer 
      Dim retStr3 as String * MAXFIELD
      ReadWarning = DLLReadWarning(PID, code, retstr3)
      msg=StripTerminator(retStr3)
End  Function 

Sub GetErrorList (  ByVal PID as long,  ByVal delimiter as string  , list as string   ) 
      Dim retStr3 as String * MAXFIELD
      call DLLGetErrorList(PID, delimiter, retstr3)
      list=StripTerminator(retStr3)
End  Sub 

Sub GetWarningList (  ByVal PID as long,  ByVal delimiter as string  , list as string   ) 
      Dim retStr3 as String * MAXFIELD
      call DLLGetWarningList(PID, delimiter, retstr3)
      list=StripTerminator(retStr3)
End  Sub 
