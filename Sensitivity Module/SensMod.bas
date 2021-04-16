Attribute VB_Name = "Module1"
Option Explicit

Public Const ITER_GO = 1
Public Const ITER_STOP = 2

'constants used to shorten the output strings
Public Const fmt As String = "###,###,###,###"
Public Const skb As String = " Kbyte"
Public Const nkb As Long = 1024

Public DEBUG_OUTPUT As Boolean ' see getargs() - flag to delete or leave temporary files

Public DotErrFile As Integer
Public DotMsgFile As Integer

Public status As Integer
Public haveError As Boolean

Public Iteration As Long
Public IterCount As Long

Type MEMORYSTATUS
  dwLength As Long
  dwMemoryLoad As Long
  dwTotalPhys As Long
  dwAvailPhys As Long
  dwTotalPageFile As Long
  dwAvailPageFile As Long
  dwTotalVirtual As Long
  dwAvailVirtual As Long
End Type

Declare Sub GlobalMemoryStatus Lib "kernel32" (lpBuffer As MEMORYSTATUS)
Declare Function GetShortPathName Lib "kernel32" Alias "GetShortPathNameA" (ByVal lpszLongPath As String, ByVal lpszShortPath As String, ByVal cchBuffer As Long) As Long
Declare Function Initialize Lib "frames.dll" (ByVal lp0 As String, ByVal lp1 As String, ByVal lp2 As String, ByVal lp3 As String, ByVal lp4 As String, ByVal lp5 As String) As Long
Declare Function Iterate Lib "frames.dll" (ByVal Iteration As Integer) As Long
Declare Function Finalize Lib "frames.dll" () As Long

Public Function GetShortName(sLongFileName As String) As String
  Dim lRetVal As Long
  Dim sShortPathName As String
  Dim iLen As Integer
  
  'Set up buffer area for API function call return
  sShortPathName = Space(255)
  iLen = Len(sShortPathName)

  'Call the function
  lRetVal = GetShortPathName(sLongFileName, sShortPathName, iLen)
  'Strip away unwanted characters.
  GetShortName = Left(sShortPathName, lRetVal)
End Function

