Attribute VB_Name = "DesHelp"
Option Explicit
Dim Helpfile As String
Dim HelpAvailable As Boolean

Global des As csv                 'des file declaration
Global Const SCF As Integer = 1
Global Const WFF As Integer = 2
Global Const WCF As Integer = 3
Global Const AFF As Integer = 4
Global Const ATO As Integer = 5
Global Const EPF As Integer = 6
Global Const RIF As Integer = 7
Global Const HIF As Integer = 8
Dim descount As Integer
Dim deslist As New Collection

Declare Function FindExecutable Lib "shell32" Alias "FindExecutableA" (ByVal lpFile As String, ByVal lpDirectory As String, ByVal sResult As String) As Long
Declare Function GetPrivateProfileString Lib "kernel32" Alias "GetPrivateProfileStringA" (ByVal lpApplicationName As String, ByVal lpKeyName As Any, ByVal lpDefault As String, ByVal lpReturnedString As String, ByVal nSize As Long, ByVal lpFileName As String) As Long


Private Const MAX_PATH As Long = 260
Private Const ERROR_FILE_NO_ASSOCIATION As Long = 31
Private Const ERROR_FILE_NOT_FOUND As Long = 2
Private Const ERROR_PATH_NOT_FOUND As Long = 3
Private Const ERROR_FILE_SUCCESS As Long = 32 'my constant
Private Const ERROR_BAD_FORMAT As Long = 11

Sub SetRefFile(filename As String)
Dim temp As String
  RefFilename = filename
  temp = Dir(RefFilename)
  If Len(temp) > 0 Then
    RefAvailable = True
  Else
    RefAvailable = False
  End If
End Sub

Sub SetHelpFile(filename As String)
  Dim temp As String
  Helpfile = filename
  temp = Dir(Helpfile)
  If Len(temp) >= 0 Then
    HelpAvailable = True
  Else
    HelpAvailable = False
  End If
End Sub
 
Sub GetHelp(anchor As String)
Dim url As String
Dim success As Long
Dim pos As Long
Dim sResult As String
Dim msg As String

  If HelpAvailable Then
    sResult = Space$(MAX_PATH)

    ' lpFile: name of the file of interest
    ' lpDirectory: location of lpFile
    ' sResult: path and name of executable associated with lpFile
    success = FindExecutable(Helpfile, "", sResult)
      
    Select Case success
      Case ERROR_FILE_NO_ASSOCIATION: msg = "no association"
      Case ERROR_FILE_NOT_FOUND: msg = "file not found"
      Case ERROR_PATH_NOT_FOUND: msg = "path not found"
      Case ERROR_BAD_FORMAT:     msg = "bad format"
      Case Is >= ERROR_FILE_SUCCESS:
        pos = InStr(sResult, Chr$(0))
        If pos Then
          url = Left$(sResult, pos - 1)
          url = url + " " + Helpfile + "#" + UCase(anchor)
          Shell url, vbNormalFocus
          Exit Sub
        End If
        Exit Sub
    End Select
    MsgBox msg, vbOKOnly, Helpfile
  End If
End Sub

Function opendes(fname$)
Dim i As Integer
  opendes = open_csv(des, fname$, 1)
  For i = 1 To deslist.Count
    deslist.Remove 1
  Next
End Function

Sub closedes()
Dim i As Integer
  'put_val des, deslist.Count              'used to adjust count in des file
  'put_line des
  close_csv des
  For i = 1 To deslist.Count
    deslist.Remove 1
  Next
End Sub

Function IsUnique(s As String) As Boolean
On Error Resume Next
  Err.Clear
  deslist.Add s, s
  If Err.Number <> 0 Then
    IsUnique = False
  Else
    IsUnique = True
  End If
End Function

Sub putmeta(name$, typ$, unit$, descript$, cnt%, Optional min, Optional max)
  put_val des, name$
  put_val des, typ$
  put_val des, unit$
  If IsMissing(min) Then
    put_val des, ""
  Else
    put_val des, "MIN"
    put_val des, min
  End If
  If IsMissing(max) Then
    put_val des, ""
  Else
    put_val des, "MAX"
    put_val des, max
  End If
  put_val des, descript$
  put_val des, cnt%
  put_line des
End Sub

Sub putvariable(name$, i1$, Optional i2, Optional i3, Optional i4, Optional i5, Optional i6)
  put_val des, "Variable"
  put_val des, name$
  put_val des, i1
  If Not IsMissing(i2) Then put_val des, i2
  If Not IsMissing(i3) Then put_val des, i3
  If Not IsMissing(i4) Then put_val des, i4
  If Not IsMissing(i5) Then put_val des, i5
  If Not IsMissing(i6) Then put_val des, i6
  put_line des
End Sub

Sub putlabel(name$, i1$, Optional i2, Optional i3, Optional i4, Optional i5, Optional i6)
  put_val des, "Label"
  put_val des, name$
  put_val des, i1
  If Not IsMissing(i2) Then put_val des, i2
  If Not IsMissing(i3) Then put_val des, i3
  If Not IsMissing(i4) Then put_val des, i4
  If Not IsMissing(i5) Then put_val des, i5
  If Not IsMissing(i6) Then put_val des, i6
  put_line des
End Sub
