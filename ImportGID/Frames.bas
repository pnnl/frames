Attribute VB_Name = "Frames"
Option Explicit
Option Compare Text

Type Point
  x As Long
  y As Long
End Type

Public Const MAX_SITES = 5              ' max number of sites
Public Const MAX_GLYPH = 100            ' max number of glyphs per siteIdx
Public Const MAX_RECENT = 3             ' max number of recent files
Public Const ISRC = 1
Public Const OSNK = 2
Public Const NOT_APP = "N/A"
Public Const BLANK = ""
Public Const FUI = "FUI"
Public Const CSM = "CSM"
Public Const TMP_NAME = "(tmp)"
Public Const BAK_NAME = "(bak)"
Public Const DOT_STAR = ".*"
Public Const DOT_DES = ".des"
Public Const DOT_GID = ".gid"
Public Const DOT_REF = ".ref"
Public Const DOT_TXT = ".txt"
Public Const DOT_ERR = ".err"
Public Const DOT_TMP = ".tmp"
Public Const DB = "Database"
Public Const MDL = "Model"
Public Const VWR = "Viewer"
Public Const SYS = "System"

Public Const NO_MODULE = -1      ' BLACK - no module selected
Public Const MODULE_OK = 0       ' RED -  module selected
Public Const INPUT_OK = 1        ' YELLOW - input complete
Public Const RUN_OK = 2          ' GREEN - run complete

Public Const SP_DRIVE = 0
Public Const SP_DIR = 1
Public Const SP_TITLE = 2
Public Const SP_EXT = 3

Public Const GID_OPEN = 0
Public Const GID_OPEN_NEW = 1
Public Const GID_SAVE_AS = 2

' All winAPI calls used by FRAMES User Interface

Public Declare Function GetPrivateProfileString Lib "kernel32" Alias "GetPrivateProfileStringA" (ByVal lpApplicationName As String, ByVal lpKeyName As Any, ByVal lpDefault As String, ByVal lpReturnedString As String, ByVal nSize As Long, ByVal lpFileName As String) As Long
Public Declare Function WritePrivateProfileString Lib "kernel32" Alias "WritePrivateProfileStringA" (ByVal lpApplicationName As String, ByVal lpKeyName As Any, ByVal lpString As Any, ByVal lpFileName As String) As Long
Public Declare Function WaitForSingleObject Lib "kernel32" (ByVal hHandle As Long, ByVal dwMilliseconds As Long) As Long
Public Declare Function CloseHandle Lib "kernel32" (ByVal hObject As Long) As Long

Public Declare Function WaitForInputIdle Lib "user32" (ByVal hProcess As Long, ByVal dwMilliseconds As Long) As Long
Public Declare Function SetWindowPos Lib "user32" (ByVal hWnd As Long, ByVal hWndInsertAfter As Long, ByVal x As Long, ByVal y As Long, ByVal cx As Long, ByVal cy As Long, ByVal wFlags As Long) As Long
Public Declare Function GetWindowThreadProcessId Lib "user32" (ByVal hWnd As Long, lpdwprocessid As Long) As Long

Public Declare Function launchProcess2 Lib "summm" (ByVal lpCommandLine As String, ByVal lpCurdir As String) As Long
Public Declare Function launchProcess3 Lib "summm" (ByVal lpCommandLine As String, ByVal lpCurdir As String, ByRef hProcess As Long, ByRef hThread As Long) As Long
Public Declare Function launchProcess4 Lib "summm" (ByVal lpCommandLine As String, ByVal lpCurdir As String) As Long

Type Glyph
  FuiId As Long           ' assigned id
  Name As String          ' Prefix & FuiId ex: aqu2 or con1
  label As String         ' user specified label
  model As String         ' model name as specified in the DES file
  modIdx As Long          ' index into module array
  grpIdx As Long          ' index into module grouping array
  x As Single             ' world X coordinate
  y As Single             ' world Y coordinate
  Z As Single             ' world Z coordinate
  sx As Single            ' x screen coordinate
  sy As Single            ' y screen coordinate
  State As Long           ' state of execution
  
  SIMid As String
  Modified As Boolean
End Type

Type Site
  Name As String
  NumGlyphs As Long
  Glyphs(MAX_GLYPH) As Glyph
  Connect(MAX_GLYPH, MAX_GLYPH) As Boolean
  ' SrcSnk(a,b)=ISRC indicates "b" is a source of "a"
  ' SrcSnk(a,b)=OSNK indicates "b" is a sink of "a" ("a" is a source of "b")
  SrcSnk(MAX_GLYPH, MAX_GLYPH) As Byte ' added 1/8/97 BLH
End Type

Public FRAMES_INI As String

'Public argv() As String
'Public argc As Long

Public NumSites As Long
Public Sites() As Site
Public frmSite As Integer
Public frmGlyph As Integer
Public CurrentGlyph As Integer
Public CurrentSite As Integer

Public AppPath As String
Public GidFile As String
Public GidDir As String
Public GidTitle As String
Public TmpTitle As String
Public savemychanges As Boolean

Public lastId As Integer
Public doMod As String
Public cdl As CommonDialog

' Reference varaiables also found in common.bas for mepas uis
Public RefFileName As String     'the name of the reference file
Public RefAvailable As Boolean   'does the reference file exist
Public RefMode As Long           'if 0 then select a reference else add a reference
Public RefIdx As Long            'the current selected reference
Public RefItem As Integer        'the current selected item index
 
' also found in common.bas for mepas uis
Sub GetArguments()
Dim args As String
Dim pos As Long
    
  argc = 0
  args = Trim$(Command$)
  If Len(args) > 0 Then
    Do
      pos = InStr(args, " ")
      argc = argc + 1
      If pos > 0 Then
        ReDim Preserve argv(argc) As String
        argv(argc - 1) = Trim$(Left$(args, pos))
        args = Trim$(Right$(args, Len(args) - pos))
      Else
        ReDim Preserve argv(argc)
        argv(argc - 1) = Trim$(args)
      End If
    Loop Until pos = 0
  End If
End Sub

Function GetListIndex(list As Control, Text As String) As Integer
Dim i As Integer
  
  For i = 0 To list.ListCount - 1
    If list.list(i) = Text Then
      GetListIndex = i
      Exit Function
    End If
  Next
  GetListIndex = -1
End Function

Function WriteIniLong(AppName As String, KeyName As String, value As Long) As Boolean
  WriteIniLong = WritePrivateProfileString(AppName, KeyName, CStr(value), FRAMES_INI)
End Function

Function ReadIniLong(AppName As String, KeyName As String, Default As Long, value As Long) As Boolean
Dim i As Long
Dim pszVal As String

  value = Default
  ReadIniLong = False
  
  pszVal = String(255, 0)
  i = GetPrivateProfileString(AppName, KeyName, BLANK, pszVal, Len(pszVal), FRAMES_INI)
  If i > 0 Then
    ReadIniLong = True
    value = val((Left$(pszVal, i)))
  End If
End Function

Function WriteIniString(AppName As String, KeyName As String, value As String) As Boolean
  WriteIniString = WritePrivateProfileString(AppName, KeyName, value, FRAMES_INI)
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
    value = LCase$(Left$(pszVal, i))
  End If
End Function

'Function getDesPath(modIdx As Long) As String
'Dim str As String
'
'  str = BLANK
'  If modIdx >= 0 Then str = AppPath & module(modIdx).FileName
'  str = Mid$(str, 1 + InStr(str, ":"))
'  getDesPath = str
'End Function

Function SplitPath(path As String, item As Integer) As String
Dim i As Integer
Dim dot As Integer
Dim slash As Integer
Dim slashnotfound As Boolean
Dim dotnotfound As Boolean

  slashnotfound = True
  dotnotfound = True
  dot = Len(path) + 1
  slash = 0
  i = dot
  While i > 0 Or (slashnotfound And dotnotfound)
    If Mid(path, i, 1) = "\" And slashnotfound Then
      slash = i
      slashnotfound = False
    Else
      If Mid(path, i, 1) = "." And dotnotfound Then
        dot = i
        dotnotfound = False
      End If
    End If
    i = i - 1
  Wend
  
  Select Case item
'    Case SP_DRIVE:
    Case SP_DIR:    SplitPath = Left(path, slash)
    Case SP_TITLE:  SplitPath = Mid(path, slash + 1, dot - slash - 1)
    Case SP_EXT:    SplitPath = Right(path, Len(path) - dot + 1)
  End Select
End Function

Sub ConnectGlyphs(siteIdx As Integer, glyphIdx As Integer, SrcName As String)
Dim i As Integer

  If SrcName = "" Then Exit Sub
  For i = 0 To Sites(siteIdx).NumGlyphs - 1
    If (Sites(siteIdx).Glyphs(i).Name = SrcName) Then
      Sites(siteIdx).Connect(i, glyphIdx) = True
      Sites(siteIdx).Connect(glyphIdx, i) = True
      Sites(siteIdx).SrcSnk(i, glyphIdx) = OSNK
      Sites(siteIdx).SrcSnk(glyphIdx, i) = ISRC
      Exit Sub
    End If
  Next
End Sub

Function Closest(siteIdx As Integer, x As Single, y As Single) As Integer
Dim i As Integer
Dim d1 As Single
Dim d2 As Single
Dim tx As Single
Dim ty As Single
  
  Closest = 0
  d1 = 10000000000#
  For i = 0 To Sites(siteIdx).NumGlyphs - 1
    tx = x - Sites(siteIdx).Glyphs(i).sx
    ty = y - Sites(siteIdx).Glyphs(i).sy
    d2 = tx * tx + ty * ty
    If d1 > d2 Then
      Closest = i
      d1 = d2
    End If
  Next
End Function

'Function getModulePrefixIndex(siteIdx As Integer, glyphIdx As Integer) As Integer
'Dim i As Integer
'Dim idx As Integer
'
'  idx = 0
'  For i = 0 To Sites(siteIdx).NumGlyphs - 1
'    If Sites(siteIdx).Glyphs(glyphIdx).grpIdx = Sites(siteIdx).Glyphs(i).grpIdx Then idx = idx + 1
'    If Sites(siteIdx).Glyphs(glyphIdx).Name = Sites(siteIdx).Glyphs(i).Name Then Exit For
'  Next
'  getModulePrefixIndex = idx
'End Function
'
'Function getmodIdxByName(Name As String) As Integer
'Dim i As Integer
'
'  getmodIdxByName = -1
'  For i = 0 To ModuleCount - 1
'    If module(i).Name = Name Then
'      getmodIdxByName = i
'      Exit For
'    End If
'  Next
'End Function

Function getSources(siteIdx As Integer, glyphIdx As Integer, srcs() As Integer) As Integer
Dim i As Integer
Dim cnt As Integer

  cnt = 0
  ReDim srcs(cnt)
  For i = 0 To Sites(siteIdx).NumGlyphs - 1
    If Sites(siteIdx).SrcSnk(i, glyphIdx) = OSNK Then
      cnt = cnt + 1
      ReDim Preserve srcs(cnt)
      srcs(cnt) = i
    End If
  Next i
  getSources = cnt
End Function

'Sub SetRefFile(FileName As String)
'Dim temp As String
'
'  RefFileName = FileName
'  temp = Dir(RefFileName)
'  If Len(temp) > 0 Then
'    RefAvailable = True
'  Else
'    RefAvailable = False
'  End If
'End Sub

Function ValidGlyphName(ftype As String, glyphIdx As String) As String
Dim i As Integer
Dim j As Integer

  ValidGlyphName = BLANK
  If FUI = glyphIdx Or CSM = glyphIdx Then
    ValidGlyphName = IIf(ftype = DOT_GID, glyphIdx, BLANK)
  Else
    For i = 0 To NumSites - 1
      For j = 0 To Sites(i).NumGlyphs - 1
        If glyphIdx = Sites(i).Glyphs(j).Name Then
          ValidGlyphName = Sites(i).Glyphs(j).Name
          Exit Function
        End If
      Next
    Next
  End If
End Function

Function GoodGidName(filename As String) As Boolean
  GoodGidName = False
  If "gid" <> LCase(Right(filename, 3)) Then
    MsgBox "Specified file name (" & filename & ") is invalid!" & Chr(10) & "File extension must be GID.", vbExclamation, "File Path\Name Error"
    Exit Function
  End If
  If InStr(filename, " ") > 0 Then
    MsgBox "Specified file path\name (" & filename & ") is invalid!" & Chr(10) & "File path\name.extension cannot contain spaces.", vbExclamation, "File Path\Name Error"
    Exit Function
  End If
  If Len(SplitPath(filename, SP_TITLE)) > 9 Then
    MsgBox "Specified file name (" & filename & ") is invalid!" & Chr(10) & "File names may only consist of 8 characters without an extension.", vbExclamation, "File Path\Name Error"
    Exit Function
  End If
  GoodGidName = True
End Function

Function GetFileName(Method As Integer) As String

  On Error Resume Next
  GetFileName = BLANK
  If cdl.InitDir = BLANK Then cdl.InitDir = AppPath
  cdl.CancelError = True
  cdl.Filter = "Global Input Data (*.gid)|*.gid"
  cdl.FilterIndex = 1
  cdl.DefaultExt = "gid"
  cdl.flags = cdlOFNHideReadOnly Or cdlOFNNoChangeDir Or cdlOFNExtensionDifferent Or cdlOFNNoLongNames Or cdlOFNPathMustExist Or cdlOFNNoReadOnlyReturn
  
  Select Case Method
  Case GID_OPEN
    cdl.DialogTitle = "Global Input Data Open"
    cdl.filename = "*.gid"
    cdl.flags = cdl.flags Or cdlOFNFileMustExist
    cdl.ShowOpen
  Case GID_OPEN_NEW
    cdl.DialogTitle = "Global Input Data Open New"
    cdl.filename = BLANK
    cdl.flags = cdl.flags Or cdlOFNOverwritePrompt
    cdl.ShowOpen
    If Dir(cdl.filename) <> "" Then
      If vbNo = MsgBox("The file (" & cdl.filename & ") already exists!" & Chr(10) & "Do want to replace the existing file?", vbExclamation Or vbYesNo, "File Path\Name Warning") Then
        cdl.filename = BLANK
      End If
    End If
  Case GID_SAVE_AS
    cdl.DialogTitle = "Global Input Data Save As"
    cdl.filename = "*.gid"
    cdl.flags = cdl.flags Or cdlOFNOverwritePrompt
    cdl.ShowSave
  End Select
  
  If Err.Number <> 0 Then Exit Function
  GetFileName = cdl.filename
End Function

Sub siteIdx_Clear(siteIdx As Integer, glyphIdx As Integer)
Dim i As Integer

  With Sites(siteIdx).Glyphs(glyphIdx)
    .FuiId = 0
    .grpIdx = -1
    .modIdx = -1
    .State = NO_MODULE
    .model = BLANK
    .Name = BLANK
    .label = BLANK
    .x = 0
    .y = 0
    .Z = 0
    .sx = 0
    .sy = 0
    .Modified = False
  End With
  For i = 0 To MAX_GLYPH
    Sites(siteIdx).Connect(i, glyphIdx) = False
    Sites(siteIdx).Connect(glyphIdx, i) = False
    Sites(siteIdx).SrcSnk(i, glyphIdx) = 0
    Sites(siteIdx).SrcSnk(glyphIdx, i) = 0
  Next
End Sub

Sub BackupSites()
Dim OldName As String
Dim BakName As String

' back up all files
  On Error Resume Next
  OldName = Dir(GidTitle & DOT_STAR)
  While (BLANK <> OldName)
    BakName = LCase(GidDir & BAK_NAME & OldName)
    SetAttr GidDir & OldName, vbNormal
    SetAttr BakName, vbNormal
    Kill BakName
    FileCopy GidDir & OldName, BakName
    OldName = Dir()
  Wend
  savemychanges = False
End Sub

Sub RestoreSites()
Dim newname As String
Dim BakName As String

' restore all files
  On Error Resume Next
  BakName = Dir(GidDir & BAK_NAME & SplitPath(GidTitle, SP_TITLE) & DOT_STAR)
  While (BLANK <> BakName)
    newname = LCase(GidTitle & SplitPath(BakName, SP_EXT))
    SetAttr newname, vbNormal
    SetAttr GidDir & BakName, vbNormal
    Kill newname
    Name GidDir & BakName As newname
    BakName = Dir()
  Wend
  savemychanges = False
End Sub

Function OpenSites(filename As String, AsNew As Boolean) As Boolean
  OpenSites = False
  If filename = BLANK Then Exit Function
  If GoodGidName(filename) Then
    CloseSites
    Set ConList = New Collection
    ReDim Sites(MAX_SITES)
    GidFile = filename
    GidDir = SplitPath(GidFile, SP_DIR)
    GidTitle = GidDir & SplitPath(GidFile, SP_TITLE)
    TmpTitle = GidDir & TMP_NAME
    If Not AsNew Then
      BackupSites
      Sites_Read GidFile
    Else
      savemychanges = True
    End If
    OpenSites = True
  End If
End Function

Function CloseSites() As Boolean
  RestoreSites
  GidFile = BLANK
  GidDir = BLANK
  GidTitle = BLANK
  TmpTitle = BLANK
  lastId = 0
  NumSites = 0
  CurrentSite = 0
  CurrentGlyph = 0
  Set ConList = Nothing
  ReDim Sites(0)
End Function

