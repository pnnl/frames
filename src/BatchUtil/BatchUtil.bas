Attribute VB_Name = "BatchUtil"
Option Explicit
Option Compare Text

Public Declare Function SetEnvironmentVariable Lib "kernel32" Alias "SetEnvironmentVariableA" (ByVal lpName As String, ByVal lpValue As String) As Long

Sub Main()
Dim ok As Boolean
Dim fname As String
Dim fs, f, s
On Error Resume Next
  
  GetArguments
  If argc < 1 Then
    MsgBox "Invalid arguments passed to module" & Chr(10) & Command & Chr(10) & "Contact PNNL", 16, "Usage error!"
    End
  End If
  Select Case argv(0)
  Case "tmpRename"
    Set fs = CreateObject("Scripting.FileSystemObject")
    fname = SplitPath(argv(1), SP_DIR) & "(tmp)." & argv(2)
    Set f = fs.GetFile(fname)
    fname = SplitPath(argv(1), SP_TITLE) & "." & argv(2)
    f.name = fname
  End Select
  
End Sub
