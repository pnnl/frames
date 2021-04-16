Attribute VB_Name = "htmhelp"
Option Explicit

Global HelpFileName As String   'the name of the reference file
Global HelpAvailable As Boolean 'does the help file exist
Global HelpAnchor As String     'the name of the help anchor

Private Declare Function FindExecutable Lib "shell32" _
   Alias "FindExecutableA" _
  (ByVal lpFile As String, _
   ByVal lpDirectory As String, _
   ByVal sResult As String) As Long

Private Const MAX_PATH As Long = 260
Private Const ERROR_FILE_NO_ASSOCIATION As Long = 31
Private Const ERROR_FILE_NOT_FOUND As Long = 2
Private Const ERROR_PATH_NOT_FOUND As Long = 3
Private Const ERROR_FILE_SUCCESS As Long = 32 'my constant
Private Const ERROR_BAD_FORMAT As Long = 11

Sub SetHelpFile(filename As String)
Dim temp As String
  
  HelpFileName = filename
  temp = Dir(HelpFileName)
  If Len(temp) >= 0 Then
    HelpAvailable = True
  Else
    HelpAvailable = False
  End If
End Sub
 
Public Function ConvertURL(URLSTRING As String) As String
    Dim i As Integer
    Dim tempchar As String
    Dim rstring As String
    rstring = ""
    For i = 1 To Len(URLSTRING)
        tempchar = Mid(URLSTRING, i, 1)
        If tempchar = "\" Then
            rstring = rstring + "/"
        Else
            rstring = rstring + tempchar
        End If
    Next i
    ConvertURL = "file:///" + rstring
End Function

Sub GetHelp()
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
    success = FindExecutable(HelpFileName, "", sResult)
      
    Select Case success
      Case ERROR_FILE_NO_ASSOCIATION: msg = "no association"
      Case ERROR_FILE_NOT_FOUND: msg = "file not found"
      Case ERROR_PATH_NOT_FOUND: msg = "path not found"
      Case ERROR_BAD_FORMAT:     msg = "bad format"
      Case Is >= ERROR_FILE_SUCCESS:
        pos = InStr(sResult, Chr$(0))
        If pos Then
          url = Left$(sResult, pos - 1)
          url = url + " " + ConvertURL(HelpFileName + "#" + UCase(HelpAnchor))
          Shell url, vbNormalFocus
          Exit Sub
        End If
        Exit Sub
    End Select
    MsgBox msg, vbOKOnly, HelpFileName
  End If
End Sub
          
