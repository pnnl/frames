Attribute VB_Name = "Module1"
Option Explicit

'Public Const SP_TITLE = 1
'Public Const SP_DIR = 2

Public Const ITER_GO = 1
Public Const ITER_STOP = 2

'constants used to shorten the output strings
Public Const fmt As String = "###,###,###,###"
Public Const skb As String = " Kbyte"
Public Const nkb As Long = 1024

Public DEBUG_OUTPUT As Boolean ' see getargs() - flag to delete or leave temporary files

'Public argv() As String
'Public argc As Integer
Public DotErrFile As Integer
Public DotWrnFile As Integer

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
Declare Function Initialize Lib "frames.dll" (ByVal lp1 As String, ByVal lp2 As String, ByVal lp3 As String, ByVal lp4 As String, ByVal lp5 As String) As Long
Declare Function Iterate Lib "frames.dll" (ByVal Iteration As Integer) As Long
Declare Function Finalize Lib "frames.dll" () As Long

'Function SplitPath(ByVal Path$, ByVal item%) As String
'Dim i As Integer
'Dim ct As Integer
'Dim done As Boolean
'
  'ct = Len(Path)
'  i = ct
'  Do While i > 0 And Not done
'    If Mid(Path, i, 1) = "\" Then
'      done = True
'    Else
'      i = i - 1
'    End If
'  Loop
'
'  If item = SP_DIR Then
'    SplitPath = Left$(Path, i)  'drive to slash inclusive (c:\dir1\ | \dir1\dir2\ | dir\ | "")
'  Else ' SP_TITLE
'    SplitPath = Right$(Path, Len(Path) - i)  'from slash to the end
'  End If
  
'End Function

'Sub getargs()
'  Dim args As String, arg As String
'  Dim pos As Integer
'
'  DEBUG_OUTPUT = False
'
'  argc = 0
'  args = Trim$(Command$)
'
'  If Len(args) > 0 Then
'    Do
'      pos = InStr(args, " ")
'      If pos > 0 Then
'        arg = Trim$(Left$(args, pos))
'        args = Trim$(Right$(args, Len(args) - pos))
'      Else
'        arg = Trim$(args)
'        args = ""
'      End If
'      If "/d" = Left(arg, 2) Then
'        DEBUG_OUTPUT = True
'      Else
'        argc = argc + 1
'        ReDim Preserve argv(argc)
'        argv(argc - 1) = arg
'      End If
'    Loop Until pos = 0
'  End If
'End Sub
