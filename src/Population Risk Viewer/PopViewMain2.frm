VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Object = "{F856EC8B-F03C-4515-BDC6-64CBD617566A}#7.0#0"; "FPSPR70.ocx"
Begin VB.Form PopViewMain2 
   Caption         =   "FRAMES Population Health Impacts Viewer"
   ClientHeight    =   6915
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   7500
   Icon            =   "PopViewMain2.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   461
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   500
   StartUpPosition =   2  'CenterScreen
   Begin FPSpreadADO.fpSpread Report 
      Height          =   2595
      Left            =   0
      TabIndex        =   27
      ToolTipText     =   "The Report for the current selection"
      Top             =   4080
      Width           =   7515
      _Version        =   458752
      _ExtentX        =   13256
      _ExtentY        =   4577
      _StockProps     =   64
      AutoCalc        =   0   'False
      AutoClipboard   =   0   'False
      ColHeaderDisplay=   0
      DisplayColHeaders=   0   'False
      DisplayRowHeaders=   0   'False
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      MaxCols         =   5
      MaxRows         =   63
      OperationMode   =   1
      RowHeaderDisplay=   0
      ScrollBars      =   2
      SpreadDesigner  =   "PopViewMain2.frx":030A
   End
   Begin VB.ComboBox Route 
      Height          =   315
      Left            =   1560
      Style           =   2  'Dropdown List
      TabIndex        =   9
      Top             =   3360
      Width           =   3750
   End
   Begin VB.ComboBox Organs 
      Height          =   315
      Left            =   1560
      Style           =   2  'Dropdown List
      TabIndex        =   6
      Top             =   2280
      Width           =   3750
   End
   Begin VB.ComboBox Sites 
      Height          =   315
      Left            =   1560
      Style           =   2  'Dropdown List
      TabIndex        =   5
      Top             =   1920
      Width           =   3750
   End
   Begin VB.ComboBox HealthImpact 
      Height          =   315
      ItemData        =   "PopViewMain2.frx":0579
      Left            =   1560
      List            =   "PopViewMain2.frx":057B
      Style           =   2  'Dropdown List
      TabIndex        =   10
      Top             =   3720
      Width           =   3750
   End
   Begin VB.CheckBox progeny 
      Caption         =   "and Progeny"
      Enabled         =   0   'False
      Height          =   195
      Left            =   5400
      TabIndex        =   13
      Top             =   960
      Width           =   1335
   End
   Begin MSComDlg.CommonDialog CommonDialog 
      Left            =   6960
      Top             =   600
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
      DefaultExt      =   "pop"
      FilterIndex     =   1
   End
   Begin VB.CommandButton writeHTML 
      Caption         =   "Write to HTML"
      Enabled         =   0   'False
      Height          =   255
      Left            =   6120
      TabIndex        =   14
      ToolTipText     =   "Writes the entry in the current table to PopView.html"
      Top             =   3720
      Width           =   1335
   End
   Begin VB.ComboBox AgeGroup 
      Height          =   315
      Left            =   1560
      Style           =   2  'Dropdown List
      TabIndex        =   4
      Top             =   1560
      Width           =   3750
   End
   Begin VB.ComboBox Path 
      Height          =   315
      Left            =   1560
      Style           =   2  'Dropdown List
      TabIndex        =   8
      Top             =   3000
      Width           =   3750
   End
   Begin VB.ComboBox ExposureMedium 
      Height          =   315
      Left            =   1560
      Style           =   2  'Dropdown List
      TabIndex        =   3
      Top             =   1200
      Width           =   3750
   End
   Begin VB.ComboBox Constituent 
      Height          =   315
      Left            =   1560
      Style           =   2  'Dropdown List
      TabIndex        =   2
      Top             =   840
      Width           =   3750
   End
   Begin VB.ComboBox ExposureStart 
      Height          =   315
      Left            =   1560
      Style           =   2  'Dropdown List
      TabIndex        =   7
      Top             =   2640
      Width           =   3750
   End
   Begin VB.ComboBox Site 
      Height          =   315
      Left            =   1560
      Style           =   2  'Dropdown List
      TabIndex        =   1
      Top             =   480
      Width           =   3750
   End
   Begin VB.CommandButton Command1 
      Caption         =   "..."
      Height          =   255
      Left            =   5880
      TabIndex        =   11
      ToolTipText     =   "Browse to a .pop file"
      Top             =   120
      Width           =   735
   End
   Begin VB.CommandButton cmdGo 
      Caption         =   "Read"
      Height          =   255
      Left            =   6720
      TabIndex        =   12
      ToolTipText     =   "Read the specified .pop file"
      Top             =   120
      Width           =   735
   End
   Begin VB.TextBox PopFilePath 
      Height          =   285
      Left            =   1080
      TabIndex        =   0
      Top             =   120
      Width           =   4695
   End
   Begin MSComctlLib.StatusBar StatusBar1 
      Align           =   2  'Align Bottom
      Height          =   255
      Left            =   0
      TabIndex        =   22
      ToolTipText     =   "Status"
      Top             =   6660
      Width           =   7500
      _ExtentX        =   13229
      _ExtentY        =   450
      Style           =   1
      SimpleText      =   "StatusBar1"
      _Version        =   393216
      BeginProperty Panels {8E3867A5-8586-11D1-B16A-00C0F0283628} 
         NumPanels       =   1
         BeginProperty Panel1 {8E3867AB-8586-11D1-B16A-00C0F0283628} 
         EndProperty
      EndProperty
   End
   Begin VB.Label Label7 
      Caption         =   "Route:"
      Height          =   255
      Left            =   0
      TabIndex        =   26
      Top             =   3360
      Width           =   1575
   End
   Begin VB.Label Label11 
      Caption         =   "Organs:"
      Height          =   255
      Left            =   0
      TabIndex        =   25
      Top             =   2280
      Width           =   1575
   End
   Begin VB.Label Label10 
      Caption         =   "Sites:"
      Height          =   255
      Left            =   0
      TabIndex        =   24
      Top             =   1920
      Width           =   1575
   End
   Begin VB.Label Label9 
      Caption         =   "Health Impact Metric:"
      Height          =   255
      Left            =   0
      TabIndex        =   23
      Top             =   3720
      Width           =   1575
   End
   Begin VB.Label Label6 
      Caption         =   "Age Group:"
      Height          =   255
      Left            =   0
      TabIndex        =   21
      Top             =   1560
      Width           =   855
   End
   Begin VB.Label Label5 
      Caption         =   "Path:"
      Height          =   255
      Left            =   0
      TabIndex        =   20
      Top             =   3000
      Width           =   1575
   End
   Begin VB.Label Label4 
      Caption         =   "Exposure Medium:"
      Height          =   255
      Left            =   0
      TabIndex        =   19
      Top             =   1200
      Width           =   1575
   End
   Begin VB.Label Label3 
      Caption         =   "Constituent:"
      Height          =   255
      Left            =   0
      TabIndex        =   18
      Top             =   840
      Width           =   855
   End
   Begin VB.Label Label2 
      Caption         =   "Exposure Start Time:"
      Height          =   255
      Left            =   0
      TabIndex        =   17
      Top             =   2640
      Width           =   1575
   End
   Begin VB.Label Label1 
      Caption         =   "Site Name:"
      Height          =   255
      Left            =   0
      TabIndex        =   16
      Top             =   480
      Width           =   855
   End
   Begin VB.Label lblPopFile 
      Caption         =   "Pop. file path:"
      Height          =   255
      Left            =   0
      TabIndex        =   15
      Top             =   120
      Width           =   1095
   End
End
Attribute VB_Name = "PopViewMain2"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private NumHIFDataSets As Long
Private NumAgeGroups As Long
Private NumPathways As Long
Private NumConstituent As Long
Private NumOrgans As Long
Private CCount As Integer
Private CList() As ContamList
Private ExpIdxMap() As Integer
Private ExpCount As Integer
Private Risk() As Single ' Result for a single constituent reused for progeny and parents
Private NumLines As Integer

Private Sub AnyChange()
  If (Len(PopFilePath.Text) = 0 Or Site.ListIndex < 0 Or _
      Constituent.ListIndex < 0 Or ExposureMedium.ListIndex < 0 Or _
      AgeGroup.ListIndex < 0 Or Sites.ListIndex < 0 Or _
      Organs.ListIndex < 0 Or ExposureStart.ListIndex < 0 Or _
      Path.ListIndex < 0 Or HealthImpact.ListIndex < 0) Then
    Call clearSpreadsheet
    writeHTML.Enabled = False
  End If
End Sub

Private Function SwitchToLabel(units As String) As String
  SwitchToLabel = units
  If (units = "Sv") Then
    SwitchToLabel = "Dose (Sv)"
  ElseIf (units = "HI") Then
    SwitchToLabel = "Hazard (HI)"
  End If
End Function

Private Function SwitchToUnit(label As String) As String
  SwitchToUnit = label
  If (label = "Dose (Sv)") Then
    SwitchToUnit = "Sv"
  ElseIf (label = "Hazard (HI)") Then
    SwitchToUnit = "HI"
  End If
End Function

Private Sub AgeGroup_Click()
  Dim i As Integer, j As Integer
  Dim times As String
  Dim t As Double
  Dim numtimes As Integer
  If (AgeGroup.ListIndex < 0) Then Exit Sub
  ExposureStart.Clear
  Path.Clear
  Route.Clear
  HealthImpact.Clear
'  MAP changed the following two lines
'  numtimes = hifLoadStartTimes(ExpIdxMap(ExposureMedium.ListIndex) - 1, AgeGroup.ItemData(AgeGroup.ListIndex) - 1, CList(Constituent.ListIndex + 1).cas, times)
  numtimes = hifGetTimeCount(ExpIdxMap(ExposureMedium.ListIndex) - 1, AgeGroup.ItemData(AgeGroup.ListIndex) - 1, CList(Constituent.ListIndex + 1).cas)
'  KJC added the lines below to read times into ExposureStart combobox
  Call ExposureStart.Clear
  For i = 0 To numtimes - 1
    t = hifGetTime(ExpIdxMap(ExposureMedium.ListIndex) - 1, AgeGroup.ItemData(AgeGroup.ListIndex) - 1, CList(Constituent.ListIndex + 1).cas, i)
    Call ExposureStart.AddItem(t)
  Next i
'  Call SetComboFromString(ExposureStart, times)
'  End Change by KJC
  If (numtimes = 1) Then
    ExposureStart.ListIndex = 0
  End If
  Path.Clear
  Path.AddItem ("all")
  Route.Clear
  HealthImpact.Clear
  Call GetUniqueRoutesAndPathways("hif", ExpIdxMap(ExposureMedium.ListIndex) - 1, AgeGroup.ItemData(AgeGroup.ListIndex) - 1, CList(Constituent.ListIndex + 1).cas, CList(Constituent.ListIndex + 1).cas)
  For i = 1 To UBound(expRoute)
    For j = 0 To Path.ListCount - 1
      If expRoute(i).Path = Path.List(j) Then Exit For
    Next j
    If j = Path.ListCount Then Path.AddItem (expRoute(i).Path)
  Next i
  AnyChange
End Sub

Private Sub cmdGo_Click()
  Dim i As Integer
  Dim count As Long
  Dim Location As String * 80
  Dim qualifier As String * 80
  Dim medium As String * 80
  Dim NumPathways, NumAgeGroups, NumOrgans, NumConstituent
  AnError = False
  StatusBar1.SimpleText = ReadPopFile(PopFilePath.Text)
  Site.Clear
  Site.AddItem (mstPopSiteName)
  If (POPStatus) Then
    Site.ListIndex = 0
  End If
  ExposureMedium.Clear
  ExpCount = 0
  ReDim ExpIdxMap(NumHIFDataSets)
  For i = 1 To NumHIFDataSets
    If (ds(i).medtype = "Air" And ds(i).numpt > 1) Then
      ExpIdxMap(ExpCount) = i
      ExpCount = ExpCount + 1
      ExposureMedium.AddItem (ds(i).locname + " " + ds(i).dstype + " " + ds(i).medtype)
    End If
  Next i
  If (ExpCount = 0) Then
    MsgBox "There are no air data set in this health impacts file. Or too few locations in the HIF file.", vbCritical, "Population Viewer"
    Call EndModule
  End If
  If (NumHIFDataSets = 1) Then
    ExposureMedium.ListIndex = 0
  End If
End Sub

Private Sub Command1_Click()
  CommonDialog.Flags = cdlOFNFileMustExist Or cdlOFNHideReadOnly Or cdlOFNNoChangeDir
  CommonDialog.ShowOpen
  If Err.Number <> 0 Then
    Err.Clear
    Exit Sub
  End If
  PopFilePath = CommonDialog.filename
  Call cmdGo_Click
End Sub

Private Sub Constituent_Click()
  Path.Clear
  Route.Clear
  HealthImpact.Clear
  HealthImpact.ListIndex = -1
  AgeGroup.ListIndex = -1
  ExposureStart.ListIndex = -1
  If (CList(Constituent.ListIndex + 1).nprog > 0) Then
    progeny.Enabled = True
  Else
    progeny.Enabled = False
  End If
  AnyChange
End Sub

Private Sub ExposureMedium_Click()
  Dim i As Integer, j As Integer
  Dim Location As String * 80
  Dim qualifier As String * 80
  Dim medium As String * 80
  Dim routestr As String * 80
  Dim pathstr As String * 80
  Dim Unit As String * 80
  Dim name As String * 80
  Dim count As Long
  Dim min As Single, max As Single
  Dim idx As Integer
  Dim popAge As String
  idx = ExpIdxMap(ExposureMedium.ListIndex)
  AgeGroup.Clear
  ExposureStart.Clear
  Path.Clear
  Route.Clear
  HealthImpact.Clear
  For j = 1 To mlNumPopAgeGroups
    popAge = mstPopAgeGroupName(j) + " " + _
           Str$(miPopAgeGroupLower(j)) + "-" + _
           Str$(miPopAgeGroupUpper(j)) + " " + _
           mstPopAgeGroupUnits(j)
    For i = 1 To ds(idx).numAge
      min = ds(idx).agemin(i)
      max = ds(idx).agemax(i)
      AgeGroup.AddItem (popAge + " using MEI for " + Str$(min) + "-" + Str$(max) + " years")
      AgeGroup.ItemData(AgeGroup.NewIndex) = i
    Next i
  Next j
  If (ds(idx).numAge * mlNumPopAgeGroups = 1) Then
    AgeGroup.ListIndex = 0
  End If
  Sites.Clear
  For i = 1 To ds(idx).nsites
    Sites.AddItem (ds(idx).Sites(i))
  Next i
  If (ds(idx).nsites = 1) Then
    Sites.ListIndex = 0
  End If
  Organs.Clear
  For i = 1 To ds(idx).norgans
    Organs.AddItem (ds(idx).Organs(i))
  Next i
  If (ds(idx).norgans = 1) Then
    Organs.ListIndex = 0
  End If
  AnyChange
End Sub

Private Sub ExposureStart_Click()
  HealthImpact.ListIndex = -1
  AnyChange
End Sub

Private Sub Form_load()
 Dim i As Integer
  Report.ColWidth(1) = 12
  Report.ColWidth(2) = 12
  Report.ColWidth(3) = 12
  Report.ColWidth(4) = 12
  Report.ColWidth(5) = 12
  Report.RowHeight(1) = 40
 Call StartModule(Me, Me.Caption, 5)
  MsgBox "The population viewer assumes that the air grid system is coincident with the population grid system, Found in the population file.", vbCritical, "Population Viewer"
  NumHIFDataSets = hifLoadDatasets()
  If (NumHIFDataSets = 0) Then
    Call PutError("No HIF Datasets read for this module")
    End
  End If
 CCount = getContamList(CList)
 If (CCount = 0) Then
   PutError ("No constituents handed to viewer")
   End
 End If
 Constituent.Clear
 For i = 1 To CCount
   Call Constituent.AddItem(CList(i).name + "(" + CList(i).cas + ")")
 Next i
 If (CCount = 1) Then
   Constituent.ListIndex = 0
 End If
End Sub

Private Sub Form_Unload(Cancel As Integer)
 Call hifClose
 Call EndModule
End Sub

Private Sub clearSpreadsheet()
  Dim i As Integer, j As Integer
  NumLines = 1
  For i = 1 To 5
    Report.col = i
    For j = 1 To 63
      Report.Row = j
      Report.Text = ""
    Next j
  Next i
End Sub
Private Sub AddLabels(label As String, Optional PopLabel As String = "")
  Call clearSpreadsheet
  Report.col = 1
  Report.Row = 1
  Report.TypeEditMultiLine = True
  Report.Text = label + " Range"
  Report.col = 2
  Report.TypeEditMultiLine = True
  Report.Text = "Number of People"
  Report.col = 3
  Report.TypeEditMultiLine = True
  Report.Text = "Number People in " + label + " Range or Greater"
  If (PopLabel <> "") Then
    Report.col = 4
    Report.TypeEditMultiLine = True
    Report.Text = PopLabel + " in this " + label + " Range"
    Report.col = 5
    Report.TypeEditMultiLine = True
    Report.Text = PopLabel + " in this " + label + " Range or Greater"
  End If
End Sub

Private Sub AddResult(doPop As Boolean, Constituent As String, RowOffset As Long, people() As Long, PopResult() As Single, RiskLabels() As String)
  Dim i As Long, j As Long
  Dim CDFPeople(5) As Long
  Dim CDFPopResult(5) As Single
  CDFPeople(0) = people(0)
  CDFPeople(1) = CDFPeople(0) + people(1)
  CDFPeople(2) = CDFPeople(1) + people(2)
  CDFPeople(3) = CDFPeople(2) + people(3)
  CDFPeople(4) = CDFPeople(3) + people(4)
  CDFPopResult(0) = PopResult(0)
  CDFPopResult(1) = CDFPopResult(0) + PopResult(1)
  CDFPopResult(2) = CDFPopResult(1) + PopResult(2)
  CDFPopResult(3) = CDFPopResult(2) + PopResult(3)
  CDFPopResult(4) = CDFPopResult(3) + PopResult(4)
  Report.col = 1
  Report.Row = RowOffset + 2
  Report.Text = Constituent
  NumLines = NumLines + 1
  For i = 0 To 4
    Report.Row = RowOffset + i + 3
    Report.col = 1
    Report.Text = RiskLabels(i)
    Report.col = 2
    Report.Text = people(i)
    Report.col = 3
    Report.Text = CDFPeople(i)
    If (doPop) Then
      Report.col = 4
      Report.Text = PopResult(i)
      Report.col = 5
      Report.Text = CDFPopResult(i)
    End If
    NumLines = NumLines + 1
  Next i
  NumLines = NumLines + 1
End Sub

Private Sub Update(people() As Long, PopResult() As Single, AgeIndex As Integer, Location As Long, Risk As Single, PathIdx As Long)
    Dim binIndex As Long
    Dim population As Long
    If (expRoute(PathIdx).name = "Carcinogen") Then
      If (Risk > 0.001) Then
        binIndex = 0
      ElseIf (Risk > 0.0001) Then
        binIndex = 1
      ElseIf (Risk > 0.00001) Then
        binIndex = 2
      ElseIf (Risk > 0.000001) Then
        binIndex = 3
      Else
        binIndex = 4
      End If
    Else
      If (Risk > 10) Then
        binIndex = 0
      ElseIf (Risk > 1) Then
        binIndex = 1
      ElseIf (Risk > 0.1) Then
        binIndex = 2
      ElseIf (Risk > 0.01) Then
        binIndex = 3
      Else
        binIndex = 4
      End If
    End If
    If (AgeIndex = 0) Then
      population = mlPopCountAG1(Location)
    ElseIf (AgeIndex = 1) Then
      population = mlPopCountAG2(Location)
    ElseIf (AgeIndex = 2) Then
      population = mlPopCountAG3(Location)
    ElseIf (AgeIndex = 3) Then
      population = mlPopCountAG4(Location)
    Else
      population = mlPopCountAG5(Location)
    End If
    people(binIndex) = people(binIndex) + population
    PopResult(binIndex) = PopResult(binIndex) + population * Risk
End Sub

Private Sub ReportRest()
  Dim i As Long, idx As Long, PathIdx As Long, j As Long
  Dim ntimes As Long
  Dim Message As String
  Dim indices() As Long
  Dim count As Long
  Dim pathChoice As Long
  Dim routeChoice As Long
  Dim Risk As Single
  Dim binIndex As Long
  Dim population As Long
  Dim people(5) As Long
  Dim PopResult(5) As Single
  Dim RiskLabels(5) As String
  Dim units As String
  Dim doPop As Boolean
  
  idx = ExpIdxMap(ExposureMedium.ListIndex)
  Call FindNearest(ds(idx).numpt, ds(idx).x, ds(idx).y)
  units = SwitchToUnit(HealthImpact.Text)
  PathIdx = findPathIndex(Path.Text, Route.Text, units)
  If (PathIdx < 0) Then Exit Sub
  If (units = "Sv" Or _
      units = "Risk") Then
    doPop = True
    RiskLabels(0) = ">1E-3"
    RiskLabels(1) = "1E-3 to 1E-4"
    RiskLabels(2) = "1E-4 to 1E-5"
    RiskLabels(3) = "1E-5 to 1E-6"
    RiskLabels(4) = "<1E-6"
  Else
    doPop = False
    RiskLabels(0) = ">10"
    RiskLabels(1) = "1.0 to 10"
    RiskLabels(2) = "0.1 to 1.0"
    RiskLabels(3) = "0.01 to 0.1"
    RiskLabels(4) = "<0.01"
  End If
  If (units = "HI") Then
    Call AddLabels("Hazard Index")
  ElseIf (units = "Risk") Then
    Call AddLabels("Risk", "Cancer Impacts")
  Else
    Call AddLabels("Dose", "Population Dose")
  End If
  For i = 0 To 5
    people(i) = 0
    PopResult(i) = 0
  Next i
  For i = 0 To ds(idx).numpt
    ntimes = readTimeSeries(CInt(ExpIdxMap(ExposureMedium.ListIndex)), _
                            AgeGroup.ItemData(AgeGroup.ListIndex), _
                            CList(Constituent.ListIndex + 1).cas, _
                            i, _
                            Organs.ListIndex + 1, _
                            expRoute(PathIdx).name, _
                            expRoute(PathIdx).Unit, _
                            expRoute(PathIdx).Route, _
                            expRoute(PathIdx).Path, _
                            "")
                            
    If (ntimes > ExposureStart.ListIndex + 1) Then
      Risk = rvalues(ExposureStart.ListIndex + 1)
      Call Update(people, PopResult, AgeGroup.ListIndex / ds(idx).numAge, i + 1, Risk, PathIdx)
    End If
  Next i
  Call AddResult(doPop, CList(Constituent.ListIndex + 1).name, 0, people, PopResult, RiskLabels)
  If (CList(Constituent.ListIndex + 1).nprog > 0 And progeny.Value = 1) Then
    For j = 1 To CList(Constituent.ListIndex + 1).nprog
      For i = 0 To 5
        people(i) = 0
        PopResult(i) = 0
      Next i
      For i = 0 To ds(idx).numpt
        ntimes = readTimeSeries(CInt(ExpIdxMap(ExposureMedium.ListIndex)), _
                                AgeGroup.ItemData(AgeGroup.ListIndex), _
                                CList(Constituent.ListIndex + 1).cas, _
                                i, _
                                Organs.ListIndex + 1, _
                                expRoute(PathIdx).name, _
                                expRoute(PathIdx).Unit, _
                                expRoute(PathIdx).Route, _
                                expRoute(PathIdx).Path, _
                                CList(Constituent.ListIndex + 1).pcas(j))
      If (ntimes > ExposureStart.ListIndex + 1) Then
        Risk = rvalues(ExposureStart.ListIndex + 1)
        Call Update(people, PopResult, AgeGroup.ListIndex / ds(idx).numAge, i + 1, Risk, PathIdx)
      End If
      Next i
    Call AddResult(doPop, CList(Constituent.ListIndex + 1).pname(j), 7 * j, people, PopResult, RiskLabels)
    Next j
  End If
End Sub

Private Sub HealthImpact_Click()
  If (HealthImpact.ListIndex < 0) Then
    AnyChange
    Exit Sub
  End If
  Call ReportRest
  writeHTML.Enabled = True
End Sub

Private Sub Organs_Click()
  HealthImpact.ListIndex = -1
  AnyChange
End Sub

Private Sub Path_Click()
  Dim i As Integer, j As Integer
  HealthImpact.Clear
  Route.Clear
  Route.AddItem ("all")
  For i = 1 To UBound(expRoute)
    If (expRoute(i).Path = Path.Text Or Path.ListIndex = 0) Then
      For j = 0 To Route.ListCount - 1
        If expRoute(i).Route = Route.List(j) Then Exit For
      Next j
      If j = Route.ListCount Then Route.AddItem (expRoute(i).Route)
    End If
  Next i
  AnyChange
End Sub


Private Sub PopFilePath_Change()
  AnyChange
End Sub

Private Sub progeny_Click()
  HealthImpact.ListIndex = -1
End Sub

Private Sub Route_Click()
  Dim i As Integer, j As Integer
  HealthImpact.Clear
  For i = 1 To UBound(expRoute)
    If (expRoute(i).Path = Path.Text Or Path.ListIndex = 0) And _
       (expRoute(i).Route = Route.Text Or Route.ListIndex = 0) Then
      For j = 0 To HealthImpact.ListCount - 1
        If SwitchToLabel(expRoute(i).Unit) = HealthImpact.List(j) Then Exit For
      Next j
      If j = HealthImpact.ListCount Then
        HealthImpact.AddItem (SwitchToLabel(expRoute(i).Unit))
      End If
    End If
  Next i
  AnyChange
End Sub

Private Sub Site_Click()
  ExposureMedium.Clear
  AgeGroup.Clear
  ExposureStart.Clear
  Path.Clear
  Route.Clear
  HealthImpact.Clear
  AnyChange
End Sub

Private Sub Sites_Click()
  HealthImpact.ListIndex = -1
  AnyChange
End Sub

Private Sub writeHTML_Click()
  Dim i As Integer, j As Integer
  Dim file As Integer
  file = FreeFile
  Open "PopView.html" For Output As file
  Print #file, "<html><head><title>Population Viewer Report</title></head><body>"
  Print #file, "<b>Site Name:</b>" + Site.Text + "<br/>"
  Print #file, "<b>Exposure Medium:</b>" + Site.Text + "<br/>"
  Print #file, "<b>Age Group:</b>" + AgeGroup.Text + "<br/>"
  Print #file, "<b>Site:</b>" + Sites.Text + "<br/>"
  Print #file, "<b>Organ:</b>" + Organs.Text + "<br/>"
  Print #file, "<b>Exposure Start:</b>" + ExposureStart.Text + "<br/>"
  Print #file, "<b>Path:</b>" + Path.Text + "<br/>"
  Print #file, "<b>Route:</b>" + Route.Text + "<br/>"
  Print #file, "<b>Health Impact:</b>" + HealthImpact.Text + "<br/>"
  Print #file, "<table width=100% border=1>"
  For j = 1 To NumLines
    If j = 1 Then
      Print #file, "<tr bgcolor=lightgrey>"
    Else
      Print #file, "<tr>"
    End If
    Report.Row = j
    For i = 1 To 5
      Report.col = i
      Print #file, "<td>" + Report.Text + "</td>"
    Next i
    Print #file, "</tr>"
  Next j
  Print #file, "</table></body></html>"
  Close file
End Sub
