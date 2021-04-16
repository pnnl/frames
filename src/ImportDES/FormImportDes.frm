VERSION 5.00
Begin VB.Form FormImportDes 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Import Module Description Files"
   ClientHeight    =   6795
   ClientLeft      =   45
   ClientTop       =   345
   ClientWidth     =   7905
   Icon            =   "FormImportDes.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   6795
   ScaleWidth      =   7905
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.TextBox Text3 
      Height          =   288
      Left            =   1680
      TabIndex        =   13
      Top             =   120
      Width           =   5772
   End
   Begin VB.TextBox Text2 
      Enabled         =   0   'False
      Height          =   288
      Left            =   1680
      TabIndex        =   8
      Top             =   6360
      Width           =   5772
   End
   Begin VB.Frame Frame2 
      Caption         =   "Destination Directory"
      Height          =   2772
      Left            =   120
      TabIndex        =   4
      Top             =   3480
      Width           =   7692
      Begin VB.CommandButton cmdClose 
         Caption         =   "Close"
         Height          =   372
         Left            =   5880
         TabIndex        =   12
         Top             =   2280
         Width           =   1572
      End
      Begin VB.CommandButton cmdModules 
         Caption         =   "&Import Modules"
         Enabled         =   0   'False
         Height          =   372
         Left            =   4080
         TabIndex        =   11
         Top             =   2280
         Width           =   1572
      End
      Begin VB.TextBox Text1 
         BackColor       =   &H80000004&
         Height          =   1932
         Left            =   3960
         MultiLine       =   -1  'True
         TabIndex        =   10
         Text            =   "FormImportDes.frx":030A
         Top             =   240
         Width           =   3612
      End
      Begin VB.DriveListBox driveDest 
         Height          =   315
         Left            =   120
         TabIndex        =   6
         Top             =   240
         Width           =   3615
      End
      Begin VB.DirListBox dirDest 
         Height          =   2016
         Left            =   120
         TabIndex        =   5
         Top             =   600
         Width           =   3615
      End
   End
   Begin VB.Frame Frame1 
      Caption         =   "Source Files"
      Height          =   2772
      Left            =   120
      TabIndex        =   0
      Top             =   600
      Width           =   7692
      Begin VB.DirListBox dirSource 
         Height          =   2016
         Left            =   120
         TabIndex        =   3
         Top             =   600
         Width           =   3612
      End
      Begin VB.FileListBox fileSource 
         Height          =   2235
         Left            =   3960
         MultiSelect     =   2  'Extended
         Pattern         =   "*.DES"
         TabIndex        =   2
         Top             =   240
         Width           =   3612
      End
      Begin VB.DriveListBox driveSource 
         Height          =   315
         Left            =   120
         TabIndex        =   1
         Top             =   240
         Width           =   3615
      End
   End
   Begin VB.ListBox List1 
      Height          =   1620
      Left            =   4080
      Sorted          =   -1  'True
      TabIndex        =   7
      Top             =   1200
      Width           =   3492
   End
   Begin VB.Label Label2 
      Caption         =   "Installation Name"
      Height          =   252
      Left            =   120
      TabIndex        =   14
      Top             =   120
      Width           =   1332
   End
   Begin VB.Label Label1 
      Caption         =   "Processing file ..."
      Height          =   252
      Left            =   120
      TabIndex        =   9
      Top             =   6360
      Width           =   1332
   End
End
Attribute VB_Name = "FormImportDes"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Public hLibModule As Long
Public fSuppress As Boolean
  
Sub Print_Conversion(fnum As Long)
Dim Measure As Variant
Dim unit As Variant
Dim Factor As Double
Dim measures As Collection
Dim units As Collection
  
  load_convert
  Set measures = get_conversion_measures
  For Each Measure In measures
    Set units = get_conversion_units(CStr(Measure))
    Measure = Trim(Replace(CStr(Measure), "liquid", ""))
    For Each unit In units
      Factor = 0#
      'Factor = CDbl(convert(CStr(unit), CStr(units(1)), 1#))
      Print #fnum, "inst.Unit(""" & CStr(Measure) & """,""" & CStr(unit) & """,""" & unit & """," & Factor & ",0.0);"
    Next
  Next
End Sub

Function LaunchProcess(cmdLine As String, Optional wait As Boolean = True) As Long
Dim hWndw As Long
Dim retCode As Long
Dim sNull As String
Dim startInfo As STARTUPINFO
Dim procInfo As PROCESS_INFORMATION
  
  LaunchProcess = 0
' Set up members of STARTUPINFO structure.
  startInfo.cb = Len(startInfo)
  retCode = CreateProcess(sNull, cmdLine, ByVal 0&, ByVal 0&, 1&, NORMAL_PRIORITY_CLASS, ByVal 0&, sNull, startInfo, procInfo)
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
      If (retCode = WAIT_FAILED) Then MsgBox "Failed to wait for process" + vbCrLf
    End If
    retCode = CloseHandle(procInfo.hThread)
    If retCode = 0 Then MsgBox "Failed to close process thread" + vbCrLf
    retCode = CloseHandle(procInfo.hProcess)
    If retCode = 0 Then MsgBox "Failed to close process" + vbCrLf
  Else
    LaunchProcess = 911
  End If
End Function

Private Sub dirSource_Change()
  On Error Resume Next
  If Not fSuppress Then
    fSuppress = True
    fileSource.Path = dirSource.Path
    fSuppress = False
    End If
End Sub

Private Sub driveSource_Change()
  On Error Resume Next
  If Not fSuppress Then
    fSuppress = True
    dirSource.Path = driveSource.list(driveSource.ListIndex)
    fileSource.Path = dirSource.Path
    fSuppress = False
  End If
End Sub

Private Sub driveDest_Change()
  On Error Resume Next
  If Not fSuppress Then
    fSuppress = True
    dirDest.Path = driveDest.list(driveDest.ListIndex)
    fSuppress = False
  End If
End Sub

Private Sub fileSource_Click()
  cmdModules.Enabled = True
End Sub

Private Sub cmdConversions()
Dim fnum As Long
Dim fName As String
Dim i As Long
Dim j As Long
Dim list As String
Dim vMeasure As Variant
Dim vUnit As Variant
Dim col As Collection
Dim ErrorCode As Long

  On Error GoTo ConvertErr
  
  Screen.MousePointer = vbHourglass
  fnum = FreeFile
  fName = dirDest.Path & "\Install 1x Units.txt"
  Open fName For Output As #fnum
  Print #fnum, "import FRAMES2API.*;"
  Print #fnum, "import Tools.*;"
  Print #fnum,
  Print #fnum, "File dir1 = new File (""."");"
  Print #fnum, "String path=dir1.getCanonicalPath();"
  Print #fnum, "System.out.println(""Path is {""+path+""}"");"
  Print #fnum,
  Print #fnum, "Install inst=new Install();"
  Print #fnum, "inst.Start(path);"
  Print #fnum,
  Print_Conversion fnum
  Print #fnum, ""
  Print #fnum, "System.out.println(""******** FINISHED ********"");"
  Print #fnum, "inst.End();"
  Print #fnum, "exit();"
  Close #fnum
  
  'Load v1 units through script into v2
  LaunchProcess """" & AppPath & "\RunScript.bat"" """ & fName & """"
  
  'Load v2 units from from system
  Set colUnits = New Collection
  ErrorCode = GetMeasureList(",", list)
  vMeasure = Split(list, ",")
  For i = LBound(vMeasure) To UBound(vMeasure)
    If 0 = InStr(vMeasure(i), "UNSPECIFIED") Then
      ErrorCode = sGetUnitList(vMeasure(i), ",", list)
      vUnit = Split(list, ",")
      For j = LBound(vUnit) To UBound(vUnit)
        On Error Resume Next
          Set col = colUnits(vUnit(j))
          If Err.Number <> 0 Then
            Set col = New Collection
            colUnits.add col, vUnit(j)
          End If
        On Error GoTo ConvertErr
        col.add vMeasure(i), vMeasure(i)
      Next
    End If
  Next

ConvertErr:
  Screen.MousePointer = vbNormal
  If Err.Number <> 0 Then MsgBox "Error conversions_click: " & Err.description
End Sub

Private Sub cmdModules_Click()
Dim objDes As ClassDESFile
Dim objMod As ClassMODFile
Dim objDic As ClassDICFile
Dim grp As New ClassModGroup
Dim grps As New Collection
Dim csv As Variant

Dim bImportDesForDic As Boolean
Dim bImportDesForMod As Boolean

Dim value As String
Dim instDic As String
Dim instMod As String
Dim instGrp As String
Dim stPrefix As String
Dim stFileName As String
Dim xpath As String
  
Dim i As Long
Dim ct As Long
Dim cnt As Long
Dim ipos As Long
Dim fnum As Long
Dim domhandle As Long
  
  
  If InStr(Text3.Text, "/") Or _
     InStr(Text3.Text, ".") Or _
     InStr(Text3.Text, """") Or _
     InStr(Text3.Text, ",") Or _
     InStr(Text3.Text, "\") Then
    MsgBox "Installation name contains one of the following unallowed characters \/.,""" & vbCrLf & "Please correct and try again.", vbCritical
    Exit Sub
  End If
  If Trim(Text3.Text) = "" Then
    MsgBox "Installation name is blank," & vbCrLf & "Please correct and try again.", vbCritical
    Exit Sub
  End If
  
  cmdModules.Enabled = False
  Screen.MousePointer = vbHourglass
            
  cmdConversions
  'Need to add impDomain domain if it doesn't already exist
  ' define in mdlImportDes
  If domhandle = 0 Then
    domhandle = DomainGetHandle(PID, impDomain)
    If (domhandle <= 0) Then domhandle = AddDomain(PID, impDomain)
  End If

  xpath = Replace(dirSource.Path, "\", "\\")
  xpath = Right(xpath, Len(xpath) - 2)

  On Error GoTo ModulesErr
  fnum = FreeFile
  Open dirDest.Path & "\" & Trim(Text3.Text) & ".txt" For Output As fnum
  
  Print #fnum, "import FRAMES2API.*;"
  Print #fnum, "import Tools.*;"
  Print #fnum,
  Print #fnum, "File dir1 = new File (""."");"
  Print #fnum, "String path = dir1.getCanonicalPath();"
  Print #fnum, "System.out.println(""Path is {""+path+""}"");"
  Print #fnum, "String DicPath = " + xpath + "\\2xExport\\"";"
  Print #fnum, "String ModPath = " + xpath + "\\2xExport\\"";"
  Print #fnum, "String ExePath = " + xpath + "\\2xExport\\"";"
  Print #fnum, "String AppPath = " + xpath + "\\"";"
  Print #fnum, "String IconPath = " + xpath + "\\Icons\\"";"
  Print #fnum, "String FramesV1Path = """ & xpath & """;"
  Print #fnum, "String Domain = """ + impDomain + """;"
  Print #fnum,
  Print #fnum, "Install inst=new Install();"
  Print #fnum, "inst.Start(path);"
  Print #fnum, ""
  Print #fnum, "//===================================="
  Print #fnum, "//======= Conversions ================"
  Print #fnum, "//===================================="
  Print_Conversion fnum
  Print #fnum, ""
  Print #fnum, "//===================================="
  Print #fnum, "//======= Module goupings ============"
  Print #fnum, "//===================================="
  Print #fnum, "inst.Domain(Domain,IconPath+""env.ico"");"
  
  ReadIniString "ModuleTypeInfo", "Count", "0", value
  ct = val(value)
  
  instGrp = "inst.DatabaseGroup(Domain,""GIS"",IconPath+""gis.ico"");"
  Print #fnum, instGrp
  For i = 2 To ct
    If ReadIniString("ModuleTypeInfo", "ModInfo" & i, BLANK, value) Then
      csv = Split(value, ",")
      If 0 <= UBound(csv) Then grp.ModuleType = csv(0)
      If 1 <= UBound(csv) Then grp.moduleName = csv(1)
      If 2 <= UBound(csv) Then grp.ModulePrefix = csv(2)
      If 3 <= UBound(csv) Then grp.ModuleIcoPath = csv(3)
      Select Case grp.ModuleType
       Case "Database":  instGrp = "inst.Database"
       Case "System":    instGrp = "inst.System"
       Case "Viewer":    instGrp = "inst.Viewer"
       Case Else:        instGrp = "inst.Model"
      End Select
      If instGrp = "inst.Model" Then
        On Error Resume Next
          grps.add grp.ModuleType, grp.ModuleType
          If Err = 0 Then Print #fnum, instGrp & "Group(Domain,""" & grp.ModuleType & """,IconPath+""ref.ico"");"
        On Error GoTo 0
        Print #fnum, instGrp & "SubGroup(Domain, """ & grp.ModuleType & """, """ & grp.moduleName & """, """ & Replace(grp.ModuleIcoPath, "\", "\\") & """); "
      Else
        If Err = 0 Then Print #fnum, instGrp & "Group(Domain,""" & grp.moduleName & """, """ & Replace(grp.ModuleIcoPath, "\", "\\") & """); "
      End If
    End If
  Next
  
  Print #fnum, ""
  Print #fnum, "//===================================="
  Print #fnum, "//======= Module and dictionaries ===="
  Print #fnum, "//===================================="
  instMod = "inst.FramesV1Mod(ModPath,""GeoReference.mod"",ExePath,ExePath,IconPath,FramesV1Path,Domain,""GIS"","""");"
  Print #fnum, instMod
  For i = 0 To fileSource.ListCount - 1
    If fileSource.Selected(i) Then
      Text2.Text = fileSource.list(i)
      DoEvents
      Set objDes = New ClassDESFile
      Set objMod = New ClassMODFile
      Set objDic = New ClassDICFile
      stFileName = fileSource.list(i)
      ipos = InStr(1, stFileName, ".des")
      If ipos > 1 Then
        stPrefix = Mid(stFileName, 1, ipos - 1)
      Else
        stPrefix = stFileName
      End If
      If objDes.ReadModuleDescription(stFileName, fileSource.Path & "\") Then
        If objDes.ModType <> "System" Then
          bImportDesForDic = objDic.ImportDesFile(objDes, stPrefix)
          bImportDesForMod = objMod.ImportDesFile(objDes, stPrefix)
          objMod.DeleteMod
          objDic.DeleteDic
          If bImportDesForDic And (objDes.Nvars > 0) Then
            objDic.WriteDic stPrefix & ".dic", dirDest.Path & "\"
            instDic = "inst.Dic(DicPath,""" & stPrefix & ".dic" & """,Domain);"
            Print #fnum, instDic
          End If
          If bImportDesForMod Then
            If objMod.WriteMod(stPrefix & ".mod", dirDest.Path & "\") Then
              ' MespasMod(path,mod,exe,ui,icon,cmdline,domain,group,subgroup)
              instMod = "inst.FramesV1Mod(ModPath,""" & objMod.mstName & ".mod" & """,ExePath,ExePath,IconPath,FramesV1Path,Domain,"""
              If objMod.mstGroup = "Database" Or objMod.mstGroup = "Model" Or _
                 objMod.mstGroup = "System" Then
                instMod = instMod & """,""" & objMod.mstSubGrp & """);"
              Else
                instMod = instMod & objMod.mstGroup & """,""" & objMod.mstSubGrp & """);"
              End If
              Print #fnum, instMod
            End If
          End If
        End If
      End If
    End If
  Next
  
ModulesErr:
  Screen.MousePointer = vbNormal
  If Err.Number <> 0 Then MsgBox "Error modules_click: " & Err.description
  Print #fnum, ""
  Print #fnum, "inst.End();"
  Print #fnum, ""
  Print #fnum, "System.out.println(""===================================="");"
  Print #fnum, "System.out.println(""FINISHED..."");"
  Print #fnum, "System.out.println(""===================================="");"
  Print #fnum, "System.out.println("""");"
  Print #fnum, "exit();"
  Close #fnum

  Set objMod = New ClassMODFile
  objMod.mstClass = "System"
  objMod.mstModelCmdLine = """" & dirDest.Path & "\" & Trim(Text3.Text) & ".txt"""
  objMod.mstName = Trim(Text3.Text)
  objMod.mstModelExe = AppPath & "\RunScript.bat"
  objMod.mstTool = True
  objMod.mbLoaded = True
  objMod.WriteMod Trim(Text3.Text) & ".mod", dirDest.Path & "\"

End Sub

Private Sub cmdClose_Click()
  Unload Me
End Sub

Private Sub Form_Load()
Dim lngCount As Long
Dim ErrorCode As Integer
Dim strPath As String
Dim strFileName As String
Dim dicname As String

  On Error GoTo Form_Load_Error
  
  GetCommandLine
  AppPath = Replace(argv(argc), """", "")
  
  fSuppress = False
  strFileName = String(255, 0)
  lngCount = GetModuleFileName(App.hInstance, strFileName, 255)
  strFileName = Left(strFileName, lngCount)
   
  If Right(strFileName, 7) = "VB6.EXE" Then
    strFileName = "C:\Framesv2"
    strFileName = Replace(strFileName, """", "")
    ChDir strFileName
    hLibModule = LoadLibrary(AppPath & "\systemio.dll")
  End If
  
  PID = OpenINI(AppPath)
  If PID <= 0 Then
    MsgBox "Invalid PID: " & PID
    Err.Raise vbObjectError + 513
  End If
  
  
  ReadIniString "App Path", "FUI", "", strPath
  dirSource.Path = strPath
  dirDest.Path = strPath & "2xExport"
  
  OpenReport
  
  ' Make sure import.dic is registered
  ErrorCode = AddOpenDictionary(PID, dirDest.Path & "\Import.dic", dicname)
  strPath = dirDest.Path & "\MaintainImporter.sim.mod1.Import"
  ErrorCode = AddOpenDataSet(PID, LookupDictionary, strPath, LookupDataSet)
  If (ErrorCode <> 0 And ErrorCode <> -7006) Then
    MsgBox "ERROR " & ErrorCode & " AddOpenDataSet " & strPath
    Err.Raise vbObjectError + 513
    GoTo Form_Load_Error
  End If
  ErrorCode = 0
 
Form_Load_Error:
  If (ErrorCode <> 0 Or Err.Number <> 0) Then
    DelDataSet PID, LookupDataSet
    End
  End If
End Sub

Private Sub Form_Unload(cancel As Integer)
  On Error Resume Next
  CloseReport
  CloseINI PID, 0
  If 0 <> hLibModule Then FreeLibrary hLibModule
  Close
  End
End Sub
