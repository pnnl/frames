Attribute VB_Name = "Execution"
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
Public Const NONE = 0
Public Const OSNK = -1

Public Const NOT_APP = "N/A"
Public Const KM = "km"
'Public Const BLANK = ""
Public Const FUI = "FUI"
Public Const CSM = "CSM"
Public Const TMP_NAME = "(tmp)"
Public Const BAK_NAME = "(bak)"
Public Const dot = "."
Public Const DOT_STAR = ".*"
Public Const DOT_DES = ".des"
Public Const DOT_REF = ".ref"
Public Const DOT_GID = ".gid"
Public Const DOT_TXT = ".txt"
Public Const DOT_DSC = ".dsc"
Public Const DOT_TMP = ".tmp"
Public Const DOT_WRN = ".wrn"
Public Const DOT_ERR = ".err"
Public Const DB = "Database"
Public Const MDL = "Model"
Public Const VWR = "Viewer"
Public Const SYS = "System"

Public Const NO_MODULE = -1      ' BLACK - no module selected
Public Const MODULE_OK = 0       ' RED -  module selected
Public Const INPUT_OK = 1        ' YELLOW - input complete
Public Const RUN_OK = 2          ' GREEN - run complete

Public Const GID_OPEN = 0
Public Const GID_OPEN_NEW = 1
Public Const GID_SAVE_AS = 2

'FRAMES datatypes
Type Glyph
  id As Long              ' assigned id
  name As String          ' Prefix & Id ex: aqu2 or con1
  label As String         ' user supplied label
  Notes As String         ' user supplied notes
  Model As String         ' model name as specified in the DES file
  state As Long           ' state of execution
  modIdx As Long          ' index into module array
  GrpIdx As Long          ' index into module grouping array
  SchIdx As Long          ' index into the selected module scheme
  x As Single             ' world X coordinate
  y As Single             ' world Y coordinate
  z As Single             ' world Z coordinate
  sx As Single            ' x screen coordinate
  sy As Single            ' y screen coordinate
  modified As Boolean     ' been Modified?
End Type

Type Site
  name As String
  NumGlyphs As Long
  Glyphs(MAX_GLYPH) As Glyph
  connect(MAX_GLYPH, MAX_GLYPH) As Integer
  ' Connect(a,b)=NONE indicates no connection
  ' Connect(a,b)=ISRC indicates "b" is a source of "a" ("a" is a sink of "b")
  ' Connect(a,b)=OSNK indicates "b" is a sink of "a" ("a" is a source of "b")
  ' Connect is indexed by (0 to NumGlyphs-1, 0 to NumGlyphs-1)
End Type

Public FRAMES_INI As String
Public argv() As String
Public argc As Long
Public AppPath As String
Public doMod As String
Public lastId As Integer
Public connect As Boolean
Public connectingIcon As Boolean

Public NumSites As Long
Public Sites() As Site
Public frmSite As Long
Public frmGlyph As Long
Public currentSite As Long
Public currentGlyph As Long
Public oldVersion As Boolean
Public modified As Boolean

Public LongGidFile As String
Public LongGidDir As String
Public LongGidTitle As String

Public GidFile As String
Public GidDir As String
Public GidTitle As String
Public TmpTitle As String

Public cdl As CommonDialog

' Reference varaiables also found in common.bas for frames friendly uis
Public RefFileName As String     'the name of the reference file
Public RefAvailable As Boolean   'does the reference file exist
Public RefMode As Long           'if 0 then select a reference else add a reference
Public RefIdx As Long            'the current selected reference
Public RefItem As Integer        'the current selected item index

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
    value = Val((Left$(pszVal, i)))
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

Function getDesPath(modIdx As Long) As String
Dim str As String

  str = BLANK
  If modIdx >= 0 Then str = AppPath & Module(modIdx).filename
  str = Mid$(str, 1 + InStr(str, ":"))
  getDesPath = str
End Function

Sub ConnectGlyphs(siteIdx As Long, glyphIdx As Long, SrcName As String)
Dim i As Integer

  If SrcName = "" Then Exit Sub
  For i = 0 To Sites(siteIdx).NumGlyphs - 1
    If (Sites(siteIdx).Glyphs(i).name = SrcName) Then
      Sites(siteIdx).connect(i, glyphIdx) = OSNK
      Sites(siteIdx).connect(glyphIdx, i) = ISRC
      Exit Sub
    End If
  Next
End Sub

Function getModulePrefixIndex(siteIdx As Long, glyphIdx As Long) As Integer
Dim i As Integer
Dim idx As Integer

  idx = 0
  For i = 0 To Sites(siteIdx).NumGlyphs - 1
    If Sites(siteIdx).Glyphs(glyphIdx).GrpIdx = Sites(siteIdx).Glyphs(i).GrpIdx Then idx = idx + 1
    If Sites(siteIdx).Glyphs(glyphIdx).name = Sites(siteIdx).Glyphs(i).name Then Exit For
  Next
  getModulePrefixIndex = idx
End Function

Function getmodIdxByName(name As String) As Integer
Dim i As Integer
  
  getmodIdxByName = -1
  For i = 0 To ModuleCount - 1
    If Module(i).name = name Then
      getmodIdxByName = i
      Exit For
    End If
  Next
End Function

Function getCommandline(siteIdx As Long, glyphIdx As Long, idx As Integer, Model As String, cmd As String, cmdPath As String) As Boolean
Dim pos1 As Long
Dim pos2 As Long
Dim cmdArgs As String

  getCommandline = False
  ' strip off all module specific arguments
  pos1 = InStr(Model, " ")
  cmdPath = Left(Model, pos1 - 1)
  cmdArgs = Right(Model, Len(Model) - pos1)

  ' if no directory syntax, assume same as app.path
  pos1 = InStr(cmdPath, ":")
  pos2 = InStr(cmdPath, "\")
  If pos1 + pos2 = 0 Then
    cmdPath = AppPath & cmdPath
  End If
  
  ' convert to short name
  cmdPath = GetShortName(cmdPath)
  
  ' if file doesn't exist then exit
  If BLANK = Dir(cmdPath) Then
    MsgBox "File does not exist: " & cmdPath & Chr(10) & "Fix the description file: " & AppPath & Module(idx).filename, vbExclamation
    Exit Function
  End If
   
  ' executable Gid name no extension, Tmp file name, siteIdx index, glyphIdx index, glyphIdx name
  cmd = cmdPath & " " & cmdArgs & _
        " " & GidTitle & _
        " " & TmpTitle & _
        " " & CStr(siteIdx + 1) & _
        " " & CStr(getModulePrefixIndex(siteIdx, glyphIdx)) & _
        " " & Sites(siteIdx).Glyphs(glyphIdx).name
        
  getCommandline = True
End Function

Function getSources(siteIdx As Long, glyphIdx As Long, srcs() As Integer) As Integer
Dim i As Long
Dim cnt As Long

  cnt = 0
  ReDim srcs(cnt)
  For i = 0 To Sites(siteIdx).NumGlyphs - 1
    If Sites(siteIdx).connect(i, glyphIdx) = OSNK Then
      cnt = cnt + 1
      ReDim Preserve srcs(cnt)
      srcs(cnt) = i
    End If
  Next i
  getSources = cnt
End Function

Function hasPassive(siteIdx As Long, glyphIdx As Long) As Boolean
  hasPassive = False
  If Sites(siteIdx).Glyphs(glyphIdx).Model = BLANK Then Exit Function
  hasPassive = (BLANK <> Module(Sites(siteIdx).Glyphs(glyphIdx).modIdx).UIBat)
End Function

Function hasActive(siteIdx As Long, glyphIdx As Long) As Boolean
  hasActive = False
  If Sites(siteIdx).Glyphs(glyphIdx).Model = BLANK Then Exit Function
  hasActive = (BLANK <> Module(Sites(siteIdx).Glyphs(glyphIdx).modIdx).EXEBat)
End Function
      
Sub ConnectIcon(NextGlyph As Long)
  ' set flag connecting icon
  connectingIcon = True
  If anyApplicableModule(currentSite, currentGlyph, NextGlyph) Then
    ' set up connection matrix
    If Sites(currentSite).connect(currentGlyph, NextGlyph) = NONE Then
      Sites(currentSite).connect(currentGlyph, NextGlyph) = OSNK
      Sites(currentSite).connect(NextGlyph, currentGlyph) = ISRC
    Else
      Sites(currentSite).connect(currentGlyph, NextGlyph) = NONE
      Sites(currentSite).connect(NextGlyph, currentGlyph) = NONE
    End If
    
    UpdateDownStreamGlyphs currentSite, currentGlyph, Sites(currentSite).Glyphs(currentGlyph).state
    ' if no cyle connect will be true
    If connect Then
      If Group(Sites(currentSite).Glyphs(NextGlyph).GrpIdx).Type <> SYS Then
        If Sites(currentSite).Glyphs(currentGlyph).state >= MODULE_OK Then
          Sites(currentSite).Glyphs(currentGlyph).state = MODULE_OK
        Else
          Sites(currentSite).Glyphs(currentGlyph).state = NO_MODULE
        End If
      End If
      UpdateDownStreamGlyphs currentSite, currentGlyph, Sites(currentSite).Glyphs(currentGlyph).state
      Sites(currentSite).Glyphs(NextGlyph).state = NO_MODULE
      UpdateDownStreamGlyphs currentSite, NextGlyph, Sites(currentSite).Glyphs(NextGlyph).state
      modified = True
    Else
      If Sites(currentSite).connect(currentGlyph, NextGlyph) = NONE Then
        Sites(currentSite).connect(currentGlyph, NextGlyph) = OSNK
        Sites(currentSite).connect(NextGlyph, currentGlyph) = ISRC
      Else
        Sites(currentSite).connect(currentGlyph, NextGlyph) = NONE
        Sites(currentSite).connect(NextGlyph, currentGlyph) = NONE
      End If
    End If
  Else
    MsgBox "There are no models applicable for this '" & Group(Sites(currentSite).Glyphs(currentGlyph).GrpIdx).name & _
         "' to '" & Group(Sites(currentSite).Glyphs(NextGlyph).GrpIdx).name & "' connection." & Chr(10) & "Connection not allowed!"
  End If
  connectingIcon = False
End Sub

Sub InfoClick(siteIdx As Long, glyphIdx As Long)
  frmSite = siteIdx
  frmGlyph = glyphIdx
  frmGeneral.Show 1
  If Sites(siteIdx).Glyphs(glyphIdx).modified Then
    Sites(siteIdx).Glyphs(glyphIdx).state = MODULE_OK
    UpdateDownStreamGlyphs siteIdx, glyphIdx, INPUT_OK
    modified = True
  End If
End Sub

Function hasCompleteInfo(siteIdx As Long, glyphIdx As Long) As Boolean
Dim getInfo As Boolean

  hasCompleteInfo = False
  If 0 > Sites(siteIdx).Glyphs(glyphIdx).modIdx Then
    getInfo = True
    Sites(siteIdx).Glyphs(glyphIdx).Model = BLANK
    Sites(siteIdx).Glyphs(glyphIdx).state = NO_MODULE
  Else
    getInfo = (Sites(siteIdx).Glyphs(glyphIdx).state = NO_MODULE)
  End If
  If getInfo Then
    InfoClick siteIdx, glyphIdx
    If 0 > Sites(siteIdx).Glyphs(glyphIdx).modIdx Then
      Exit Function
    End If
  End If
  hasCompleteInfo = (Sites(siteIdx).Glyphs(glyphIdx).state >= MODULE_OK)
End Function

Function hasCompleteInfoRecurse(siteIdx As Long, glyphIdx As Long, recurse As Boolean) As Boolean
Dim i As Long

  If recurse Then
    For i = 0 To Sites(siteIdx).NumGlyphs - 1
      If Sites(siteIdx).connect(glyphIdx, i) = ISRC And glyphIdx <> i Then
        If Not hasCompleteInfoRecurse(siteIdx, i, True) Then Exit Function
      End If
    Next
  End If
  hasCompleteInfoRecurse = hasCompleteInfo(siteIdx, glyphIdx)
End Function

Function hasRequiredConnections(siteIdx As Long, glyphIdx As Long) As Boolean
Dim i As Long
Dim m As Long
Dim sel As Boolean
Dim fio() As fileIOStruct
Dim msrc() As Long
Dim gsrc() As Long
  
  hasRequiredConnections = False
  getSourceModules siteIdx, glyphIdx, gsrc(), msrc(), False
  buildFioA fio(), msrc(), False
  m = Sites(siteIdx).Glyphs(glyphIdx).modIdx
  If m >= 0 Then
    If Module(m).nvers >= 2# Then
      If (0 < UBound(fio)) Then
        For i = 1 To Module(m).nread
          sel = sel Or MatchProduceToConsume(Module(m).reads(i).Spec, fio(), True)
          If sel Then Exit For
        Next i
      Else
        sel = (0 = Module(m).nread) ' no input requirements
      End If
    Else
      sel = True
    End If
  End If
  
  If Not sel Then
    MsgBox "The connections are not complete for " & Sites(siteIdx).Glyphs(glyphIdx).label & " (" & Sites(siteIdx).Glyphs(glyphIdx).name & ")"
    Exit Function
  End If
  hasRequiredConnections = True
End Function

Function hasRequiredConnectionsRecurse(siteIdx As Long, glyphIdx As Long, recurse As Boolean) As Boolean
Dim i As Long
  
  hasRequiredConnectionsRecurse = False
  If recurse Then
    For i = 0 To Sites(siteIdx).NumGlyphs - 1
      If Sites(siteIdx).connect(glyphIdx, i) = ISRC And glyphIdx <> i Then
        If Not hasRequiredConnectionsRecurse(siteIdx, i, True) Then Exit Function
      End If
    Next
  End If
  hasRequiredConnectionsRecurse = hasRequiredConnections(siteIdx, glyphIdx)
End Function

Function RequisiteInputComplete(siteIdx As Long, glyphIdx As Long) As Boolean
Dim i As Long

  RequisiteInputComplete = True
  For i = 0 To Sites(siteIdx).NumGlyphs - 1
    If Sites(siteIdx).connect(i, glyphIdx) = OSNK Then
      If Sites(siteIdx).Glyphs(i).state = MODULE_OK And hasPassive(siteIdx, i) Then
        RequisiteInputComplete = False
        Exit Function
      End If
    End If
  Next
End Function

Function RequisiteRunsIncomplete(siteIdx As Long, glyphIdx As Long) As Integer
Dim i As Long
  
  RequisiteRunsIncomplete = -1
  For i = 0 To Sites(siteIdx).NumGlyphs - 1
    If Sites(siteIdx).connect(i, glyphIdx) = OSNK Then
      If Sites(siteIdx).Glyphs(i).state < RUN_OK Then
        RequisiteRunsIncomplete = i
        Exit Function
      End If
    End If
  Next
End Function

Sub SetRefFile(filename As String)
Dim temp As String

  RefFileName = filename
  temp = Dir(RefFileName)
  If Len(temp) > 0 Then
    RefAvailable = True
  Else
    RefAvailable = False
  End If
End Sub

Sub UpdateGlyphStatus(siteIdx As Long, glyphIdx As Long, state As Long, idx As Long)
Dim i As Long
Dim newstatus As Long

  For i = 0 To Sites(siteIdx).NumGlyphs - 1
    If Sites(siteIdx).connect(glyphIdx, i) = OSNK Then
      If i <> idx Then
        UpdateGlyphStatus siteIdx, i, state, idx
      Else
        MsgBox "CSM may not contain a loop, no connection established!"
        connect = False
        Exit Sub
      End If
    End If
  Next
  If Not connectingIcon Then
    If Sites(siteIdx).Glyphs(glyphIdx).state >= state Then
      newstatus = state
      Select Case state
        Case NO_MODULE:
        Case MODULE_OK:
        Case INPUT_OK:
          If Not hasPassive(siteIdx, glyphIdx) Then newstatus = MODULE_OK
        Case RUN_OK:
          If Not hasActive(siteIdx, glyphIdx) Then newstatus = INPUT_OK
      End Select
      Sites(siteIdx).Glyphs(glyphIdx).state = newstatus
    End If
  End If
End Sub

Sub UpdateDownStreamGlyphs(siteIdx As Long, glyphIdx As Long, state As Long)
Dim i As Long

  ' check for cycle
  connect = True
  For i = 0 To Sites(siteIdx).NumGlyphs - 1
    If i <> glyphIdx Then
      If Sites(siteIdx).connect(glyphIdx, i) = OSNK Then
        UpdateGlyphStatus siteIdx, i, state, glyphIdx
      End If
    End If
  Next
End Sub

Function ValidGlyphName(ftype As String, glyphIdx As String) As String
Dim i As Long
Dim j As Long

  ValidGlyphName = BLANK
  If FUI = glyphIdx Or CSM = glyphIdx Then
    ValidGlyphName = IIf(ftype = DOT_GID, glyphIdx, BLANK)
  Else
    For i = 0 To NumSites - 1
      For j = 0 To Sites(i).NumGlyphs - 1
        If glyphIdx = Sites(i).Glyphs(j).name Then
          ValidGlyphName = Sites(i).Glyphs(j).name
          Exit Function
        End If
      Next
    Next
  End If
End Function

Function GoodGidName(filename As String) As Boolean
  GoodGidName = False
  If DOT_GID <> LCase(Right(filename, 4)) Then
    MsgBox "Specified file name (" & filename & ") is invalid!" & Chr(10) & "File extension must be GID.", vbExclamation, "File Path\Name Error"
    Exit Function
  End If
  If InStr(filename, " ") > 0 Then
    MsgBox "Specified file name (" & filename & ") is invalid!" & Chr(10) & "File name can not contain spaces.", vbExclamation, "File Path\Name Error"
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
  cdl.Flags = cdlOFNHideReadOnly Or cdlOFNNoChangeDir Or cdlOFNExtensionDifferent Or cdlOFNNoLongNames Or cdlOFNPathMustExist Or cdlOFNNoReadOnlyReturn
  
  Select Case Method
  Case GID_OPEN
    cdl.DialogTitle = "Global Input Data Open"
    cdl.filename = "*.gid"
    cdl.Flags = cdl.Flags Or cdlOFNFileMustExist
    cdl.ShowOpen
  Case GID_OPEN_NEW
    cdl.DialogTitle = "Global Input Data Open New"
    cdl.filename = BLANK
    cdl.Flags = cdl.Flags Or cdlOFNOverwritePrompt
    cdl.ShowOpen
    If Dir(cdl.filename) <> "" And cdl.filename <> "" Then
      If vbNo = MsgBox("The file (" & cdl.filename & ") already exists!" & Chr(10) & "Do want to replace the existing file?", vbExclamation Or vbYesNo, "File Path\Name Warning") Then
        cdl.filename = BLANK
      End If
    End If
  Case GID_SAVE_AS
    cdl.DialogTitle = "Global Input Data Save As"
    cdl.filename = "*.gid"
    cdl.Flags = cdl.Flags Or cdlOFNOverwritePrompt
    cdl.ShowSave
  End Select
  
  If Err.Number <> 0 Then Exit Function
  GetFileName = cdl.filename
End Function
  
Sub SetLastId()
Dim i As Long
Dim j As Long
Dim k As Long
Dim id As String
  
  ' get last used glyphIdx id
  lastId = 0
  For i = 0 To NumSites - 1
    For j = 0 To Sites(i).NumGlyphs - 1
      id = Sites(i).Glyphs(j).name
      For k = 1 To Len(id)
        If IsNumeric(Mid(id, k, 1)) Then
          id = Mid(id, k)
          Exit For
        End If
      Next
      If IsNumeric(id) Then Sites(i).Glyphs(j).id = Val(id)
      If Sites(i).Glyphs(j).id > lastId Then lastId = Sites(i).Glyphs(j).id
    Next
  Next
End Sub

Function SiteIdx_Add(siteIdx As Long, GrpIdx As Long, sx As Single, sy As Single) As Long
Dim i As Long
  
  SetLastId
  lastId = lastId + 1
  i = Sites(siteIdx).NumGlyphs
  SiteIdx_Clear siteIdx, i
  With Sites(siteIdx).Glyphs(i)
    .id = lastId
    .GrpIdx = GrpIdx
    .sx = sx
    .sy = sy
    .name = Group(GrpIdx).Prefix & lastId
    .label = Group(GrpIdx).name
  End With
  Sites(siteIdx).NumGlyphs = Sites(siteIdx).NumGlyphs + 1
  SiteIdx_Add = i
End Function

Sub SiteIdx_Clear(siteIdx As Long, glyphIdx As Long)
Dim i As Long

  With Sites(siteIdx).Glyphs(glyphIdx)
    .id = 0
    .GrpIdx = -1
    .modIdx = -1
    .SchIdx = -1
    .state = NO_MODULE
    .Model = BLANK
    .name = BLANK
    .label = BLANK
    .Notes = BLANK
    .x = 0
    .y = 0
    .z = 0
    .sx = 0
    .sy = 0
    .modified = False
  End With
  For i = 0 To MAX_GLYPH
    Sites(siteIdx).connect(i, glyphIdx) = NONE
    Sites(siteIdx).connect(glyphIdx, i) = NONE
  Next
End Sub

Sub BackupSites()
Dim OldName As String
Dim BakName As String

' back up all files
  On Error Resume Next
  OldName = Dir(LongGidTitle & DOT_STAR)
  Kill LongGidDir & BAK_NAME & SplitPath(LongGidTitle, SP_TITLE) & DOT_STAR
  While (BLANK <> OldName)
    BakName = LCase(LongGidDir & BAK_NAME & OldName)
    SetAttr LongGidDir & OldName, vbNormal
    SetAttr BakName, vbNormal
    Kill BakName
    FileCopy LongGidDir & OldName, BakName
    OldName = Dir()
  Wend
  modified = False
End Sub

Sub RestoreSites()
Dim NewName As String
Dim BakName As String

' restore all files
  On Error Resume Next
  BakName = Dir(LongGidDir & BAK_NAME & SplitPath(LongGidTitle, SP_TITLE) & DOT_STAR)
  Kill LongGidTitle & DOT_STAR
  While (BLANK <> BakName)
    NewName = LCase(LongGidTitle & SplitPath(BakName, SP_EXT))
    SetAttr NewName, vbNormal
    SetAttr LongGidDir & BakName, vbNormal
    Kill NewName
    Name LongGidDir & BakName As NewName
    BakName = Dir()
  Wend
  modified = False
End Sub

Function OpenSites(filename As String, asNew As Boolean) As Boolean
Dim errfile As Boolean

  OpenSites = False
  If filename = BLANK Then Exit Function
  If GoodGidName(filename) Then
    CloseSites
    Set conList = New Collection
    ReDim Sites(MAX_SITES)
    LongGidFile = filename
    LongGidDir = SplitPath(LongGidFile, SP_DIR)
    LongGidTitle = LongGidDir & SplitPath(LongGidFile, SP_TITLE)
    TmpTitle = LongGidDir & TMP_NAME
    If Not asNew Then
      BackupSites
      If Not Sites_Read(LongGidFile) Then
        CloseSites
        Exit Function
      End If
    Else
      modified = True
    End If
    SetLastId
    SetRefFile LongGidTitle & DOT_REF
    Sites_Write_FUI TmpTitle & DOT_GID
    Commit_Writes FUI, errfile
    Sites_Write_CSM TmpTitle & DOT_GID
    Commit_Writes CSM, errfile
    Kill TmpTitle & DOT_GID

' fix for long filenames, must get short name after file creation
    GidFile = GetShortName(LongGidFile)
    GidDir = SplitPath(GidFile, SP_DIR)
    GidTitle = GidDir & SplitPath(GidFile, SP_TITLE)
    TmpTitle = GidDir & TMP_NAME
#If UI Then
    rtb.Text = ""
    tree.SetFocus
#End If
    OpenSites = True
  End If
End Function

Function CloseSites() As Boolean
  RestoreSites
  LongGidFile = BLANK
  LongGidDir = BLANK
  LongGidTitle = BLANK
  GidFile = BLANK
  GidDir = BLANK
  GidTitle = BLANK
  TmpTitle = BLANK
  NumSites = 0
  currentSite = 0
  currentGlyph = 0
  modified = False
  oldVersion = False
  Set conList = Nothing
  ReDim Sites(0)
#If UI Then
  rtb.Text = ""
  tree.SetFocus
  DrawSite Sites(currentSite), pic, False
#End If
End Function

Function Save_Changes() As Long
Dim answer As Integer

  If Not modified Or NumSites = 0 Then Exit Function
  answer = MsgBox("Do you want to save any changes?", vbYesNoCancel + vbQuestion)
  Save_Changes = answer
  If answer = vbCancel Then Exit Function
  If answer = vbYes Then SaveSites False, True
  If answer = vbNo Then CloseSites
End Function

Function SaveSites(asNew As Boolean, Permanent As Boolean) As Boolean
Dim i As Long
Dim j As Long
Dim state As Long
Dim errfile As Boolean
Dim NewFile As String
Dim NewTitle As String

  On Error Resume Next
  SaveSites = False
  If asNew Then
    ' Get new file name
    NewFile = LCase(GetFileName(GID_SAVE_AS))
    If NewFile <> LongGidFile Then
      If Not GoodGidName(NewFile) Then Exit Function
      NewTitle = SplitPath(NewFile, SP_DIR) & SplitPath(NewFile, SP_TITLE)
      Kill NewTitle & DOT_STAR
      FileCopy LongGidTitle & DOT_GID, NewTitle & DOT_GID
      FileCopy LongGidTitle & DOT_REF, NewTitle & DOT_REF
      FileCopy LongGidTitle & DOT_DSC, NewTitle & DOT_DSC
      RestoreSites
      
      LongGidFile = NewFile
      LongGidDir = SplitPath(LongGidFile, SP_DIR)
      LongGidTitle = LongGidDir & SplitPath(LongGidFile, SP_TITLE)
      
      SetRefFile LongGidTitle & DOT_REF
      GidFile = GetShortName(LongGidFile)
      GidDir = SplitPath(GidFile, SP_DIR)
      GidTitle = GidDir & SplitPath(GidFile, SP_TITLE)
      TmpTitle = GidDir & TMP_NAME
      
      ' Set any glyphs downsteam from a database back
      For i = 0 To NumSites - 1
        For j = 0 To Sites(i).NumGlyphs - 1
          If Group(Sites(i).Glyphs(j).GrpIdx).Type = DB And Sites(i).Glyphs(j).state > INPUT_OK Then
            ' save database State
            state = Sites(i).Glyphs(j).state
            UpdateDownStreamGlyphs i, j, INPUT_OK
            ' restore database State
            Sites(i).Glyphs(j).state = state
          End If
        Next
      Next
    End If
  End If
  
  Sites_Write_FUI TmpTitle & DOT_GID
  Commit_Writes FUI, errfile
  Sites_Write_CSM TmpTitle & DOT_GID
  Commit_Writes CSM, errfile
  Kill TmpTitle & DOT_GID
  If Permanent Then BackupSites
  SaveSites = True
End Function

Function LaunchProcess(cmdline As String, cmdPath As String, Optional wait As Boolean = True) As Long
Dim hWndw As Long
Dim retCode As Long
Dim sNull As String
Dim startInfo As STARTUPINFO
Dim procInfo As PROCESS_INFORMATION
  
  LaunchProcess = 0
#If UI Then
' Minimize FRAMES window
  frm.Hide
#End If
' Set up members of STARTUPINFO structure.
  startInfo.cb = Len(startInfo)
  retCode = CreateProcess(sNull, cmdline, ByVal 0&, ByVal 0&, 1&, NORMAL_PRIORITY_CLASS, ByVal 0&, sNull, startInfo, procInfo)
' Wait for the processs to finish
  If (retCode <> 0) Then
    ' Wait until the given process is waiting for user input with no input pending,
    ' or until the time-out interval has elapse.
    ' This function only works with GUI applications.
    ' If a console application calls the function, it returns immediately, with no wait.
    retCode = WaitForInputIdle(procInfo.hProcess, INFINITE)
    If (wait) Then
        ' The WaitForSingleObject function checks the current state of the specified object.
        ' If the object's state is nonsignaled, the calling thread enters an efficient wait state.
        ' The thread consumes very little processor time while waiting for the object state
        ' to become signaled or the time-out interval to elapse
        retCode = WaitForSingleObject(procInfo.hProcess, INFINITE)
        If (retCode = WAIT_FAILED) Then LaunchProcess = 411
#If UI Then
    If (retCode = WAIT_FAILED) Then rtb.Text = rtb.Text + "Failed to wait for process" + vbCrLf
#End If
    End If
    retCode = CloseHandle(procInfo.hThread)
#If UI Then
    If retCode = 0 Then rtb.Text = rtb.Text + "Failed to close process thread" + vbCrLf
#End If
    retCode = CloseHandle(procInfo.hProcess)
#If UI Then
    If retCode = 0 Then rtb.Text = rtb.Text + "Failed to close process" + vbCrLf
#End If
  Else
    LaunchProcess = 911
  End If
#If UI Then
' Restore FRAMES window
  If doMod <> FUI Then
    frm.Show
    SetForegroundWindow frm.hWnd
    pic.SetFocus
  End If
#End If
End Function

Function ExecuteModule(siteIdx As Long, glyphIdx As Long, mode As String, changes As Integer) As Boolean
Dim idx As Integer
Dim errfile As Boolean
Dim retCode As Long
Dim Model As String
Dim cmdline As String
Dim cmdPath As String
 
  On Error Resume Next
  Kill TmpTitle & DOT_STAR
  changes = 0
  ExecuteModule = False
  idx = Sites(siteIdx).Glyphs(glyphIdx).modIdx
  If idx < 0 Then
    MsgBox "Model does not exist: " & Sites(siteIdx).Glyphs(glyphIdx).Model
    Exit Function
  End If
  ' model path [drive:\path\filename] from module desription file
  If mode = "UI" Then
    Model = Module(idx).UIBat & " "
  Else
    Model = Module(idx).EXEBat & " "
  End If
  If Not getCommandline(siteIdx, glyphIdx, idx, Model, cmdline, cmdPath) Then Exit Function
  retCode = LaunchProcess(cmdline, cmdPath)
  If retCode <> 0 Then
    MsgBox "Error: " & CStr(retCode) & Chr(10) & "Trying to spawn: " & cmdline
  Else
    ' success is true if files other than ~tmp.err are processed 2/10/97
    ' the number returned from Commit_Writes is the number of files posted...
    changes = Commit_Writes(Sites(siteIdx).Glyphs(glyphIdx).name, errfile)
    If changes > 0 Then modified = True
    ExecuteModule = Not errfile
    
    'must update the FUI section if constituent module executed
    If InStr(Sites(siteIdx).Glyphs(glyphIdx).name, "con") = 1 Then
      Sites_Write_FUI TmpTitle & DOT_GID
      Commit_Writes FUI, errfile
    End If

#If UI Then
    Kill TmpTitle & DOT_STAR
#Else
    SetAttr TmpTitle & DOT_ERR, vbReadOnly
    Kill TmpTitle & DOT_STAR
    SetAttr TmpTitle & DOT_ERR, vbNormal
#End If
  
  End If
End Function

Function doUI(siteIdx As Long, glyphIdx As Long) As Boolean
Dim changes As Integer

  doUI = False
  If ExecuteModule(siteIdx, glyphIdx, "UI", changes) Then
    If changes > 0 Then
      If hasActive(siteIdx, glyphIdx) Then
        Sites(siteIdx).Glyphs(glyphIdx).state = INPUT_OK
        UpdateDownStreamGlyphs siteIdx, glyphIdx, INPUT_OK
      Else
        Sites(siteIdx).Glyphs(glyphIdx).state = RUN_OK
        UpdateDownStreamGlyphs siteIdx, glyphIdx, MODULE_OK
      End If
      doUI = True
    End If
  Else
    Sites(siteIdx).Glyphs(glyphIdx).state = MODULE_OK
    UpdateDownStreamGlyphs siteIdx, glyphIdx, MODULE_OK
  End If
End Function

Function doModule(siteIdx As Long, glyphIdx As Long) As Boolean
Dim changes As Integer

  doModule = False
  If ExecuteModule(siteIdx, glyphIdx, "EXE", changes) Then
    If changes > 0 Then
      Sites(siteIdx).Glyphs(glyphIdx).state = RUN_OK
      UpdateDownStreamGlyphs siteIdx, glyphIdx, INPUT_OK
      doModule = True
    End If
  Else
    Sites(siteIdx).Glyphs(glyphIdx).state = INPUT_OK
    UpdateDownStreamGlyphs siteIdx, glyphIdx, INPUT_OK
  End If
End Function

Sub doAll(mode As String, siteIdx As Long)
Dim i As Long
Dim j As Long
Dim nSnk As Long
Dim success As Boolean

  doMod = mode
        
  ' check that the glyphIdx has a module defined
  For j = 0 To Sites(siteIdx).NumGlyphs - 1
    ' find tails
    nSnk = 0
    For i = 0 To Sites(siteIdx).NumGlyphs - 1
      If Sites(siteIdx).connect(j, i) = OSNK Then nSnk = nSnk + 1
    Next i
    If 0 = nSnk Then
      If Not hasCompleteInfoRecurse(siteIdx, j, True) Then Exit Sub
      If Not hasRequiredConnectionsRecurse(siteIdx, j, True) Then Exit Sub
    End If
  Next j
  
  'need to refresh connection info
  'now that all modules have been selected
  Sites_Write_FUI TmpTitle & DOT_GID
  Commit_Writes FUI, success
  Sites_Write_CSM TmpTitle & DOT_GID
  Commit_Writes CSM, success
  Kill TmpTitle & DOT_GID

  'now lets execute the UIs
  success = False
  For j = 0 To Sites(siteIdx).NumGlyphs - 1
    If doMod = FUI Then
      nSnk = 0
      For i = 0 To Sites(siteIdx).NumGlyphs - 1
        If Sites(siteIdx).connect(j, i) = OSNK Then nSnk = nSnk + 1
      Next i
      If 0 = nSnk Then
        success = doModuleIONew(siteIdx, j, "UI")
        If Not success Then Exit For
      End If
    Else
      If Sites(siteIdx).Glyphs(j).name = doMod Then
        success = doModuleIONew(siteIdx, j, "UI")
        Exit For
      End If
    End If
  Next
  
  'now lets execute the Models
  If success Then
    For j = 0 To Sites(siteIdx).NumGlyphs - 1
      If doMod = FUI Then
        nSnk = 0
        For i = 0 To Sites(siteIdx).NumGlyphs - 1
          If Sites(siteIdx).connect(j, i) = OSNK Then nSnk = nSnk + 1
        Next i
        If 0 = nSnk Then
          success = doModuleIONew(siteIdx, j, "EXE")
          If Not success Then Exit For
        End If
      Else
        If Sites(siteIdx).Glyphs(j).name = doMod Then
          success = doModuleIONew(siteIdx, j, "EXE")
          Exit For
        End If
      End If
    Next j
  End If
  
  doMod = BLANK
End Sub

Function doModuleIONew(siteIdx As Long, glyphIdx As Long, ByVal mode As String) As Boolean
Dim i As Long
Dim inc As Long
Dim doInput As Boolean
Dim doRun As Boolean
Dim success As Boolean
Dim srcs() As Integer
Dim curStatus As Long

  'Recursive call to get all upstream modules complete
  For i = 0 To Sites(siteIdx).NumGlyphs - 1
    If Sites(siteIdx).connect(glyphIdx, i) = ISRC And glyphIdx <> i Then
      success = doModuleIONew(siteIdx, i, mode)
      If Not success Then
        doModuleIONew = False
        Exit Function
      End If
    End If
  Next
  
  ' do not execute vwr or system glyphs in doall execution
  If Group(Sites(siteIdx).Glyphs(glyphIdx).GrpIdx).Type = VWR Or Group(Sites(siteIdx).Glyphs(glyphIdx).GrpIdx).Type = SYS Then
    doModuleIONew = True
    Exit Function
  End If
  
  ' run user input if applicable
  success = True
  If mode = "UI" And hasPassive(siteIdx, glyphIdx) And Sites(siteIdx).Glyphs(glyphIdx).state < INPUT_OK Then
    ' do input
    If RequisiteInputComplete(siteIdx, glyphIdx) Then
      success = hasCompleteInfo(siteIdx, glyphIdx)
      curStatus = Sites(siteIdx).Glyphs(glyphIdx).state
      If success Then success = doUI(siteIdx, glyphIdx)
      ' if no change of state then this isn't completed
      If curStatus = Sites(siteIdx).Glyphs(glyphIdx).state Then success = False
    Else
      success = False
      If 1 = getSources(siteIdx, glyphIdx, srcs()) Then
        MsgBox "First complete input for object: " & Sites(siteIdx).Glyphs(inc).name
      Else
        ' if multiple sources then they will be completed by other paths
        doModuleIONew = True
        Exit Function
      End If
    End If
  End If
    
  ' run model if applicable
  If mode = "EXE" And hasActive(siteIdx, glyphIdx) And Sites(siteIdx).Glyphs(glyphIdx).state < RUN_OK Then
    If Not (doMod = Sites(siteIdx).Glyphs(glyphIdx).name) Then
      ' the above test should prevent a system module from calling itself
      inc = RequisiteRunsIncomplete(siteIdx, glyphIdx)
      If 0 > inc Then
        success = hasCompleteInfo(siteIdx, glyphIdx)
        curStatus = Sites(siteIdx).Glyphs(glyphIdx).state
        If success Then success = doModule(siteIdx, glyphIdx)
        If curStatus = Sites(siteIdx).Glyphs(glyphIdx).state Then success = False
      Else
        success = False
        If 1 = getSources(siteIdx, glyphIdx, srcs()) Then
          MsgBox "First complete the run for object: " & Sites(siteIdx).Glyphs(inc).name
        Else
          ' if multiple sources then they will be completed by other paths
          doModuleIONew = True
          Exit Function
        End If
      End If
    End If
  End If
  ' if no change in state then not done
  doModuleIONew = success
End Function
