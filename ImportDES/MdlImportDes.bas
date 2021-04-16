Attribute VB_Name = "MdlImportDes"
Option Explicit
Option Compare Text

Public Type STARTUPINFO
  cb As Long
  lpReserved As String
  lpDesktop As String
  lpTitle As String
  dwX As Long
  dwY As Long
  dwXSize As Long
  dwYSize As Long
  dwXCountChars As Long
  dwYCountChars As Long
  dwFillAttribute As Long
  dwFlags As Long
  wShowWindow As Integer
  cbReserved2 As Integer
  lpReserved2 As Long
  hStdInput As Long
  hStdOutput As Long
  hStdError As Long
End Type

Public Type PROCESS_INFORMATION
  hProcess As Long
  hThread As Long
  dwProcessId As Long
  dwThreadId As Long
End Type

'Const used by *Process functions
Public Const INFINITE = &HFFFF             '  Infinite timeout
Public Const WAIT_FAILED = &HFFFF          '  Wait failed
Public Const NORMAL_PRIORITY_CLASS = &H20

'Constants
Public Const TrueFlag As Long = 1
Public Const FalseFlag As Long = 0
Public Const BLANK As String = ""
Public Const SPACER As String = " "
Public Const WRAPPER As String = "\FramesV1\2xExport\Run1xMod.Exe" ' relative path
Public Const LookupDictionary As String = "Import"

Public Const INT_MAX As Long = 2000000000#
Public Const FLT_MAX As Single = 1E+38

Public Const dtSTRING As String = "STRING"
Public Const dtINTEGER As String = "INTEGER"
Public Const dtFLOAT As String = "FLOAT"
Public Const dtLOGICAL As String = "LOGICAL"

Public Declare Function LoadLibrary Lib "kernel32" Alias "LoadLibraryA" (ByVal lpLibFileName As String) As Long
Public Declare Function FreeLibrary Lib "kernel32" (ByVal hLibModule As Long) As Long
Public Declare Function GetModuleFileName Lib "kernel32" Alias "GetModuleFileNameA" (ByVal hModule As Long, ByVal lpFileName As String, ByVal nSize As Long) As Long
Public Declare Function WaitForSingleObject _
                                      Lib "kernel32" _
                                       (ByVal hHandle As Long, _
                                        ByVal dwMilliseconds As Long) As Long
Public Declare Function CloseHandle _
                                      Lib "kernel32" _
                                       (ByVal hObject As Long) As Long
Public Declare Function WaitForInputIdle _
                                      Lib "user32" _
                                       (ByVal hProcess As Long, _
                                        ByVal dwMilliseconds As Long) As Long
Public Declare Function CreateProcess _
                                      Lib "kernel32" Alias "CreateProcessA" _
                                       (ByVal lpApplicationName As String, _
                                        ByVal lpCommandLine As String, _
                                        lpProcessAttributes As Any, _
                                        lpThreadAttributes As Any, _
                                        ByVal bInheritHandles As Long, _
                                        ByVal dwCreationFlags As Long, _
                                        lpEnvironment As Any, _
                                        ByVal lpCurrentDriectory As String, _
                                        lpStartupInfo As STARTUPINFO, _
                                        lpProcessInformation As PROCESS_INFORMATION) As Long
Public Declare Function GetPrivateProfileString _
                                      Lib "kernel32" Alias "GetPrivateProfileStringA" _
                                       (ByVal lpApplicationName As String, _
                                        ByVal lpKeyName As Any, _
                                        ByVal lpDefault As String, _
                                        ByVal lpReturnedString As String, _
                                        ByVal nSize As Long, _
                                        ByVal lpFileName As String) As Long

'Global Params
Global FRAMES_INI As String
Public LookupDataSet As String
Public impDomain As String
Public AppPath As String
Public colUnits As Collection
Public fTmp As Long
Public PID As Long              'the process id for all systemio calls
Public SimPath As String        'the path to the simulation files
Public SimName As String        'the name of the simulation
Public modid As String          'the name of the module icon
Public argv() As String         'command line argument array
Public argc As Long             'command line argument count
Public hLibModule As Long

Public Sub GetCommandLine(Optional MaxArgs)
 
   Dim quote As String
   quote = Chr$(34)
  
   'Declare variables.
   Dim C, cmdline, CmdLnLen, InArg, i, quoted As Boolean
   'See if MaxArgs was provided.
   If IsMissing(MaxArgs) Then MaxArgs = 10
   'Make array of the correct size.
   ReDim argv(MaxArgs)
   argc = 0: InArg = False
   'Get command line arguments.
   cmdline = Command()
   CmdLnLen = Len(cmdline)
   'Go thru command line one character
   'at a time.
   For i = 1 To CmdLnLen
      C = Mid(cmdline, i, 1)
      'Test for space or tab.
      If (C <> " " And C <> vbTab) Or quoted Then
         'Neither space nor tab.
         'Test if already in argument.
         If Not InArg Then
         'New argument begins.
         'Test for too many arguments.
            If argc = MaxArgs Then Exit For
            argc = argc + 1
            InArg = True
         End If
         'Concatenate character to current argument.
         argv(argc) = argv(argc) & C
         If C = quote Then
           If Right$(argv(argc), 1) = quote Then
            quoted = IIf(quoted, False, True)
           End If
         End If
      Else
         'Found a space or tab.
         'Set InArg flag to False.
         InArg = False
      End If
   Next i
   'Resize array just enough to hold arguments.
   ReDim Preserve argv(argc)
   
   impDomain = "EARTH"
   FRAMES_INI = argv(argc) + "\\FramesUI.ini"

End Sub
  
Sub OpenReport()
  fTmp = FreeFile
  Open FormImportDes.dirDest.Path & "\DESImport.txt" For Output As #fTmp
End Sub

Sub CloseReport()
  Close #fTmp
End Sub

Public Function DisplayError(ByVal code As Long, ByVal API_CALL As String, Optional ByVal display As Boolean = False) As Long
Dim errstr As String
Dim errmsg As String

  DisplayError = code
  If code < 0 Then
    ReadError PID, code, errstr
    errmsg = "API Call : " + API_CALL + vbCrLf
    errmsg = errmsg + "Error Code : " + CStr(code) + vbCrLf
    errmsg = errmsg + "Error Description : " + errstr
    Debug.Print errmsg
    If display Then MsgBox errmsg, vbExclamation + vbOKOnly, "Import DES File Error"
'    If fStatus Is Nothing Then Exit Function
'    fStatus.Text1.Text = errmsg & vbCrLf & fStatus.Text1.Text
  Else
'    If fStatus Is Nothing Then Exit Function
'    fStatus.Text1.Text = "Success Code: " & CStr(code) & "  API call: " & API_CALL & vbCrLf & fStatus.Text1.Text
  End If
End Function

Function ReadIniString(AppName As String, KeyName As String, Default As String, value As String) As Boolean
Dim i As Long
Dim pszVal As String

  value = Default
  ReadIniString = False
  
  pszVal = String(255, 0)
  i = GetPrivateProfileString(AppName, KeyName, BLANK, pszVal, Len(pszVal), FRAMES_INI)
  If i > 0 Then
    ReadIniString = True
'    value = LCase$(Left$(pszVal, i))
    value = (Left$(pszVal, i))
  End If
End Function

Public Function FindObject(ByRef col As Collection, ByVal key As String) As Object
  Dim i As Long
  Dim obj As Object
  For i = 1 To col.count
    Set obj = col.item(i)
    
    If col.item(i).key = key Then
      Set FindObject = col.item(i)
      Exit Function
  End If
  Next i
  Set FindObject = Nothing
End Function
