Attribute VB_Name = "Execute"
Option Explicit
Option Compare Text

Type StartUpInfo
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
  wShowWindow As Long
  cbReserved2 As Long
  lpReserved2 As Long
  hStdInput As Long
  hStdOutput As Long
  hStdError As Long
End Type

Type Process_Information
  hProcess As Long
  hThread As Long
  dwProcessID As Long
  dwThreadID As Long
End Type

Declare Function GetPrivateProfileString Lib "kernel32" Alias "GetPrivateProfileStringA" (ByVal lpApplicationName As String, ByVal lpKeyName As Any, ByVal lpDefault As String, ByVal lpReturnedString As String, ByVal nSize As Long, ByVal lpFileName As String) As Long
Declare Function WaitForSingleObject Lib "kernel32" (ByVal hHandle As Long, ByVal dwMilliseconds As Long) As Long
Declare Function CreateProcessA Lib "kernel32" (ByVal lpApplicationName As String, ByVal lpCommandLine As String, ByVal lpProcessAttributes As Any, ByVal lpThreadAttributes As Any, ByVal bInheritHandles As Long, ByVal dwCreationFlags As Long, ByVal lpEnvironment As Any, ByVal lpCurrentDirectory As String, lpStartupInfo As StartUpInfo, lpProcessInformation As Process_Information) As Long
Declare Function CloseHandle Lib "kernel32" (ByVal hobject As Long) As Long
Declare Function GetLastError Lib "kernel32" () As Long

Const Normal_Priority_Class = &H20&
Const Infinite = -1&

Public Sub ExecCmd(cmdline As String)
  'Execute another program and waits till other program is finished
  Dim sNull As String
  Dim ret As Long
  Dim proc As Process_Information
  Dim start As StartUpInfo
  
  start.cb = Len(start)
  
  ret = CreateProcessA(sNull, cmdline, ByVal 0&, ByVal 0&, 1&, Normal_Priority_Class, ByVal 0&, sNull, start, proc)
'  MsgBox cmdline + ": " + GetLastError()
  ret = WaitForSingleObject(proc.hProcess, Infinite)
  ret = CloseHandle(proc.hProcess)
  
End Sub



