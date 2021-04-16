Attribute VB_Name = "FileUtil"
Option Explicit
Option Compare Text

Sub ChangeFilewithDialog(OpenFile As Control, oldname As String, pathway As String, filters As String, newname As String, fileexist As Boolean, errtext As String)
  Dim tmpfilename As String

On Local Error GoTo OpenFileError
  
  errtext = ""  
  'Set Dialog Box Title
  OpenFile.DialogTitle = "Edit/Change File"
  'Set Default Filename
  OpenFile.filename = oldname
  'Set Default Path
  OpenFile.InitDir = pathway
  'Set Default Filters
  OpenFile.Filter = filters
  'Enable Cancel button
  OpenFile.CancelError = True
  'Show Dialog Box
  OpenFile.Action = 1 

  tmpfilename = OpenFile.filename
  
  If tmpfilename = "" Then
    errtext = "Invalid file - No filename given."
    errtext = errtext + Chr$(10) + Chr$(13)
    Exit Sub
  End If
    
  If ChkFile(tmpfilename) Then
    fileexist = True
  Else
    fileexist = False
  End If
  newname = tmpfilename
  Exit Sub

OpenFileError:
  If err = cdlCancel Then
    errtext = "EXIT"
    Exit Sub
  End If
  errtext = "Error number " + Str$(err)
  errtext = errtext + Chr$(13) + Chr$(10)
  errtext = errtext + Error$
End Sub

Sub GetNamePathExt(inpfile As String, name As String, path As String, ext As String)
  'Determine the file name, path, and extension
  
  Dim teststring As String, namestring As String
  Dim lastpos As Long, fextpos As Long, lextpos As Long
  Dim fnamepos As Long, lnamepos As Long

  If inpfile = "" Then
    name = ""
    path = ""
    ext = ""
  Else
    'Get Extension
    teststring = LTrim$(RTrim$(inpfile))
    lastpos = Len(teststring)
    lextpos = 0
    fextpos = InStr(teststring, ".")
    Do While fextpos <> 0
      lextpos = fextpos
      fextpos = InStr(lextpos + 1, teststring, ".")
    Loop
    'MsgBox "Test String = " + teststring + ", LastPos = " + Str(lastpos)
    'MsgBox "lExtPos = " + Str(lextpos)
    
    If lextpos <= 0 Then
      ext = ""
      namestring = teststring
    Else
      namestring = Mid(teststring, 1, (lextpos - 1))
      ext = Mid(teststring, lextpos + 1)
    End If
    
    'MsgBox "Namestring = " + namestring
    'MsgBox "Ext = " + Ext
    'Get Name
    lastpos = Len(namestring)
    lnamepos = 0
    fnamepos = InStr(namestring, "\")
    Do While fnamepos <> 0
      lnamepos = fnamepos
      fnamepos = InStr(lnamepos + 1, namestring, "\")
    Loop
    
    If lnamepos <= 0 Then
      fnamepos = InStr(namestring, ":")
      If fnamepos <= 0 Then
        name = namestring
        path = 0
      Else
        path = Mid(namestring, 1, (fnamepos))
        name = Mid(namestring, fnamepos + 1)
      End If
    Else
      path = Mid(namestring, 1, (lnamepos))
      name = Mid(namestring, lnamepos + 1)
    End If
  End If
  'MsgBox "Path = " + Path + ", Name = " + Name + ", Ext = " + Ext
End Sub

Function ChkFile(filename As String) As Boolean
'Determines if filename is an existing file or not

On Local Error GoTo BadPath

  ChkFile = False
  If filename <> "" Then
    If Dir(filename) <> "" Then
      ChkFile = True
    End If
  End If

BadPath:

End Function

