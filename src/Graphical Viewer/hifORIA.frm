VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Object = "{F856EC8B-F03C-4515-BDC6-64CBD617566A}#7.0#0"; "FPSPR70.ocx"
Begin VB.Form hifORIA 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Form1"
   ClientHeight    =   7200
   ClientLeft      =   3075
   ClientTop       =   2880
   ClientWidth     =   9600
   Icon            =   "hifORIA.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   7200
   ScaleWidth      =   9600
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   0
      Top             =   0
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
      CancelError     =   -1  'True
      DefaultExt      =   ".txt"
      DialogTitle     =   "Save Table to File"
      Filter          =   "Tab Delimited Text File (*.txt) | *.txt"
   End
   Begin VB.Frame Frame3 
      Height          =   7095
      Left            =   105
      TabIndex        =   0
      Top             =   0
      Width           =   9420
      Begin VB.OptionButton optTable 
         Caption         =   "Option 4"
         Height          =   315
         Index           =   4
         Left            =   4920
         TabIndex        =   26
         TabStop         =   0   'False
         Top             =   2520
         Value           =   -1  'True
         Visible         =   0   'False
         Width           =   1000
      End
      Begin VB.ComboBox cboConst 
         Height          =   315
         Left            =   1380
         Style           =   2  'Dropdown List
         TabIndex        =   14
         Top             =   1575
         Width           =   3000
      End
      Begin VB.OptionButton optTable 
         Caption         =   "Option 4"
         Height          =   315
         Index           =   3
         Left            =   3780
         TabIndex        =   13
         TabStop         =   0   'False
         Top             =   2520
         Visible         =   0   'False
         Width           =   1000
      End
      Begin VB.OptionButton optTable 
         Caption         =   "Option 3"
         Height          =   315
         Index           =   2
         Left            =   2520
         TabIndex        =   12
         TabStop         =   0   'False
         Top             =   2520
         Visible         =   0   'False
         Width           =   1000
      End
      Begin VB.OptionButton optTable 
         Caption         =   "Option 2"
         Height          =   315
         Index           =   1
         Left            =   1365
         TabIndex        =   11
         TabStop         =   0   'False
         Top             =   2520
         Visible         =   0   'False
         Width           =   1000
      End
      Begin VB.OptionButton optTable 
         Caption         =   "Option 1"
         Height          =   315
         Index           =   0
         Left            =   210
         TabIndex        =   10
         TabStop         =   0   'False
         Top             =   2520
         Visible         =   0   'False
         Width           =   1000
      End
      Begin VB.ComboBox cboTime 
         Height          =   315
         ItemData        =   "hifORIA.frx":030A
         Left            =   1380
         List            =   "hifORIA.frx":030C
         Style           =   2  'Dropdown List
         TabIndex        =   9
         Top             =   1995
         Width           =   3000
      End
      Begin VB.ComboBox cboMeasure 
         Height          =   315
         Left            =   6210
         Style           =   2  'Dropdown List
         TabIndex        =   8
         Top             =   315
         Width           =   3000
      End
      Begin VB.ComboBox cboPath 
         Height          =   315
         Left            =   6210
         Style           =   2  'Dropdown List
         TabIndex        =   7
         Top             =   1155
         Width           =   3000
      End
      Begin VB.ComboBox cboRoute 
         Height          =   315
         ItemData        =   "hifORIA.frx":030E
         Left            =   6210
         List            =   "hifORIA.frx":0310
         Style           =   2  'Dropdown List
         TabIndex        =   6
         Top             =   735
         Width           =   3000
      End
      Begin VB.ComboBox cboAge 
         Height          =   315
         Left            =   1380
         Style           =   2  'Dropdown List
         TabIndex        =   5
         Top             =   1155
         Width           =   3000
      End
      Begin VB.ComboBox cboXY 
         Height          =   315
         Left            =   1380
         Style           =   2  'Dropdown List
         TabIndex        =   4
         Top             =   735
         Width           =   3000
      End
      Begin VB.ComboBox cboName 
         Height          =   315
         Left            =   1380
         Style           =   2  'Dropdown List
         TabIndex        =   3
         Top             =   315
         Width           =   3000
      End
      Begin VB.ComboBox cboOrgDose 
         Height          =   315
         Left            =   6210
         Style           =   2  'Dropdown List
         TabIndex        =   2
         Top             =   1995
         Width           =   3000
      End
      Begin VB.ComboBox cboOrgCancer 
         Height          =   315
         Left            =   6210
         Style           =   2  'Dropdown List
         TabIndex        =   1
         Top             =   1575
         Width           =   3000
      End
      Begin FPSpreadADO.fpSpread vaSpread1 
         Height          =   4260
         Left            =   240
         TabIndex        =   25
         Top             =   2496
         Width           =   9000
         _Version        =   458752
         _ExtentX        =   15875
         _ExtentY        =   7514
         _StockProps     =   64
         ArrowsExitEditMode=   -1  'True
         AutoCalc        =   0   'False
         ColHeaderDisplay=   0
         DisplayRowHeaders=   0   'False
         EditEnterAction =   5
         EditModeReplace =   -1  'True
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         FormulaSync     =   0   'False
         ScrollBarExtMode=   -1  'True
         ScrollBarShowMax=   0   'False
         SpreadDesigner  =   "hifORIA.frx":0312
         VisibleCols     =   500
         VisibleRows     =   500
      End
      Begin VB.Label Label1 
         Caption         =   "Time Point (yr)"
         Height          =   195
         Index           =   5
         Left            =   210
         TabIndex        =   24
         Top             =   2100
         Width           =   1200
      End
      Begin VB.Label Label1 
         Caption         =   "Measure"
         Height          =   195
         Index           =   6
         Left            =   4620
         TabIndex        =   23
         Top             =   420
         Width           =   1200
      End
      Begin VB.Label Label1 
         Caption         =   "Route"
         Height          =   195
         Index           =   7
         Left            =   4620
         TabIndex        =   22
         Top             =   840
         Width           =   1200
      End
      Begin VB.Label Label1 
         Caption         =   "Pathway"
         Height          =   195
         Index           =   8
         Left            =   4620
         TabIndex        =   21
         Top             =   1260
         Width           =   1200
      End
      Begin VB.Label Label1 
         Caption         =   "Dose organ"
         Height          =   195
         Index           =   10
         Left            =   4620
         TabIndex        =   20
         Top             =   2100
         Width           =   1395
         WordWrap        =   -1  'True
      End
      Begin VB.Label Label1 
         Caption         =   "Cancer organ"
         Height          =   195
         Index           =   9
         Left            =   4620
         TabIndex        =   19
         Top             =   1680
         Width           =   1200
         WordWrap        =   -1  'True
      End
      Begin VB.Label Label1 
         Caption         =   "Age Group"
         Height          =   195
         Index           =   3
         Left            =   210
         TabIndex        =   18
         Top             =   1260
         Width           =   1200
      End
      Begin VB.Label Label1 
         Caption         =   "Constituent"
         Height          =   195
         Index           =   4
         Left            =   210
         TabIndex        =   17
         Top             =   1680
         Width           =   1200
      End
      Begin VB.Label Label1 
         Caption         =   "Location"
         Height          =   195
         Index           =   2
         Left            =   210
         TabIndex        =   16
         Top             =   840
         Width           =   1200
         WordWrap        =   -1  'True
      End
      Begin VB.Label Label1 
         Caption         =   "Dataset"
         Height          =   195
         Index           =   1
         Left            =   210
         TabIndex        =   15
         Top             =   420
         Width           =   1200
         WordWrap        =   -1  'True
      End
   End
   Begin VB.Menu mnuPrint 
      Caption         =   "Print"
   End
   Begin VB.Menu mnuSave 
      Caption         =   "Save"
   End
   Begin VB.Menu help 
      Caption         =   "&Help"
      Begin VB.Menu howto 
         Caption         =   "How to..."
      End
      Begin VB.Menu about 
         Caption         =   "&About"
      End
   End
End
Attribute VB_Name = "hifORIA"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Dim loading As Boolean

Private Sub about_Click()
  frmAbout.picIcon = frmAbout.ImageList1.ListImages(1).Picture
  frmAbout.Show 0
End Sub

Private Sub howto_Click()
  GetHelp
End Sub

Private Sub mnuPrint_Click()
  vaSpread1.PrintUseDataMax = True
  vaSpread1.PrintGrid = False
  vaSpread1.PrintColHeaders = False
  vaSpread1.PrintRowHeaders = False
  vaSpread1.PrintBorder = False
  On Error GoTo PRINTERROR
  vaSpread1.Action = 13
  On Error GoTo 0
PRINTERROR:
End Sub

Private Sub mnuSave_Click()
Dim savecheck As Boolean
  savecheck = True
  On Error GoTo CANCELSAVE
  CommonDialog1.ShowSave
  On Error GoTo 0
  savecheck = vaSpread1.SaveTabFile(CommonDialog1.fileName)
  If Not savecheck Then MsgBox "Unable to save table to " + CommonDialog1.fileName, vbApplicationModal + vbExclamation + vbOKOnly, "Save Error"
CANCELSAVE:
End Sub

Private Sub Form_Load()
  Dim i As Integer
  
  Set activeForm = Me
  loading = True
  hifLoadDatasets
  If AnError Or numDS = 0 Then Unload Me
  cboName.Clear
  For i = 0 To numDS - 1
    cboName.AddItem ds(i).locName + ":" + ds(i).locType
  Next
  updateConstituentList
  cboUpdateConstituent cboConst, False
  loading = False
  cboName.ListIndex = 0
  Set frm = Me
  SetHelpFile App.Path + "\Summary.htm"
End Sub

Private Sub Form_Unload(Cancel As Integer)
  hifClose
  EndModule
End Sub

Private Sub updateSheet()
  If loading Then Exit Sub
  MousePointer = vbHourglass
  If optTable(0).value = True Then
    GenOpt1Table
  ElseIf optTable(1).value = True Then
    GenOpt2Table
  ElseIf optTable(2).value = True Then
    GenOpt3Table
  Else
    GenOpt4Table
  End If
  MousePointer = vbNormal
End Sub

Private Sub cboName_Click()
Dim dsIdx As Long

  dsIdx = cboName.ListIndex
  cboUpdateExpPT dsIdx, cboXY
  cboUpdateAgeGroup dsIdx, cboAge
  cboUpdateCancerOrgans dsIdx, cboOrgCancer
  cboUpdateDoseOrgans dsIdx, cboOrgDose
  cboAge_Click
End Sub

Private Sub cboAge_Click()
  If IgnoreEvents Then Exit Sub
  cboConst_Click
End Sub

Private Sub cboConst_Click()
Dim i As Long
Dim dsIdx As Long
Dim ageIdx As Long

  If IgnoreEvents Then Exit Sub
  dsIdx = cboName.ListIndex
  ageIdx = cboAge.ListIndex
  GetRoutesAndPaths dsIdx, ageIdx, cboConst.Text
  i = GetAllContIdx(cboConst.Text)
  If i < 1 Then i = 1
  cboUpdateStartTimes dsIdx, ageIdx, allCAS(i).pcas(0), cboTime
  cboUpdateMeasures cboMeasure
  cboMeasure_Click
End Sub

Private Sub cboMeasure_Click()
Dim i As Long
Dim j As Long

  If IgnoreEvents Then Exit Sub
  i = cboMeasure.ItemData(cboMeasure.ListIndex)
  cboUpdateRoutes uExposure(i).measure, uExposure(i).Unit, cboRoute
  j = cboRoute.ItemData(cboRoute.ListIndex)
  cboUpdatePaths uExposure(j).measure, uExposure(j).Unit, uExposure(j).Route, cboPath
  Select Case uExposure(i).measure
    Case "risk":
      Label1(9).Visible = True
      cboOrgCancer.Visible = True
      Label1(10).Visible = False
      cboOrgDose.Visible = False
    Case "dose":
      Label1(9).Visible = False
      cboOrgCancer.Visible = False
      Label1(10).Visible = True
      cboOrgDose.Visible = True
    Case Else: ' "hi" or not valid file
      Label1(9).Visible = False
      cboOrgCancer.Visible = False
      Label1(10).Visible = False
      cboOrgDose.Visible = False
  End Select
  updateSheet
End Sub

Private Sub cboRoute_Click()
Dim i As Long

  If IgnoreEvents Then Exit Sub
  i = cboRoute.ItemData(cboRoute.ListIndex)
  cboUpdatePaths uExposure(i).measure, uExposure(i).Unit, uExposure(i).Route, cboPath
  updateSheet
End Sub

Private Sub cboPath_Click()
  If IgnoreEvents Then Exit Sub
  updateSheet
End Sub

Private Sub cboXY_Click()
  If IgnoreEvents Then Exit Sub
  updateSheet
End Sub

Private Sub cboOrgCancer_Click()
  If IgnoreEvents Then Exit Sub
  updateSheet
End Sub

Private Sub cboOrgDose_Click()
  If IgnoreEvents Then Exit Sub
  updateSheet
End Sub

Private Sub cboTime_Click()
  If IgnoreEvents Then Exit Sub
  updateSheet
End Sub

Private Sub optTable_Click(Index As Integer)
  'tag used as flag for MAXIMUM or REPORTALL ...
  cboAge.Tag = False
  cboTime.Tag = False
  cboXY.Tag = False
  cboMeasure.Tag = False
  cboRoute.Tag = False
  cboPath.Tag = False
  cboOrgCancer.Tag = False
  cboOrgDose.Tag = False
  cboAge.Enabled = True
  cboTime.Enabled = True
  cboXY.Enabled = True
  cboMeasure.Enabled = True
  cboRoute.Enabled = True
  cboPath.Enabled = True
  cboOrgCancer.Enabled = True
  cboOrgDose.Enabled = True
  Select Case Index
  Case 0:
   cboAge.Tag = True
   cboRoute.Tag = True
   cboPath.Tag = True
   cboAge.Enabled = False
   cboRoute.Enabled = False
   cboPath.Enabled = False
  Case 1
   cboMeasure.Tag = True
   cboRoute.Tag = True
   cboPath.Tag = True
   cboMeasure.Enabled = False
   cboRoute.Enabled = False
   cboPath.Enabled = False
  Case 2
   cboAge.Tag = True
   cboOrgDose.Tag = True
   cboOrgCancer.Tag = True
   cboAge.Enabled = False
   cboOrgDose.Enabled = False
   cboOrgCancer.Enabled = False
  Case 3
   cboTime.Tag = True
   cboXY.Tag = True
   cboAge.Tag = True
   cboOrgDose.Tag = True
   cboOrgCancer.Tag = True
   cboTime.Enabled = False
   cboXY.Enabled = False
   cboAge.Enabled = False
   cboOrgDose.Enabled = False
   cboOrgCancer.Enabled = False
  End Select
  cboName_Click
End Sub

'Generates table 2 with option 1 selected
Private Sub GenOpt1Table()
Dim i As Long
Dim m As Long
Dim dsIdx As Long
Dim xyIdx As Long
Dim casid As String
Dim parentCAS As String
Dim timeIdx As Long
Dim orgIdx As Long
Dim timevalue As Double
Dim datavalue As Double
Dim tRow As Long
Dim rowcnt As Long

  StartSpread
  'set description info
  vaSpread1.SetText 1, 1, "Dataset"
  vaSpread1.SetText 2, 1, cboName
  vaSpread1.SetText 1, 2, "Location"
  vaSpread1.SetText 2, 2, cboXY
  vaSpread1.SetText 1, 3, "Constituent"
  vaSpread1.SetText 2, 3, cboConst.Text
  vaSpread1.SetText 1, 4, "Time"
  vaSpread1.SetText 2, 4, cboTime
  vaSpread1.SetText 1, 5, "Measure"
  vaSpread1.SetText 2, 5, cboMeasure
  If cboOrgCancer.Enabled And cboOrgCancer.Visible Then
    tRow = 8
    vaSpread1.SetText 1, 6, "Cancer Organ"
    vaSpread1.SetText 2, 6, cboOrgCancer.Text
    orgIdx = cboOrgCancer.ListIndex
  ElseIf cboOrgDose.Enabled And cboOrgDose.Visible Then
    tRow = 8
    vaSpread1.SetText 1, 6, "Dose Organ"
    vaSpread1.SetText 2, 6, cboOrgDose.Text
    orgIdx = cboOrgDose.ListIndex
  Else
    tRow = 7
    orgIdx = 0
  End If
  'set table heeaders
  vaSpread1.SetText 1, tRow, "Exposure Pathways"
  vaSpread1.SetText 2, tRow, "Exposure Routes"
    
  dsIdx = cboName.ListIndex
  casid = allCAS(cboConst.ListIndex + 1).cas
  parentCAS = allCAS(cboConst.ListIndex + 1).pcas(0)
  xyIdx = cboXY.ListIndex
  timeIdx = cboTime.ListIndex
  
  rowcnt = tRow + 1
  For i = 0 To ds(dsIdx).numAge - 1
    If rowcnt = tRow + 1 Then vaSpread1.SetText 3 + i, tRow, "Ages " + CStr(ds(dsIdx).ageMin(i)) + " to " + CStr(ds(dsIdx).ageMax(i))
    For m = 1 To UBound(uExposure)
      If cboMeasure.Text = (uExposure(m).measure & " (" & uExposure(m).Unit & ")") Then
        timevalue = hifLoadTimeSeries(dsIdx, xyIdx, i, orgIdx, casid, parentCAS, uExposure(m).Path, uExposure(m).Route, uExposure(m).measure, uExposure(m).Unit)
        If timevalue > 0 Then
          hifGetTimeAndValue dsIdx, timeIdx, timevalue, datavalue
        Else
          datavalue = 0
        End If
        vaSpread1.SetText 1, rowcnt, uExposure(m).Path
        vaSpread1.SetText 2, rowcnt, uExposure(m).Route
        vaSpread1.SetText 3 + i, rowcnt, Format(datavalue, "0.0##E+00")
        rowcnt = rowcnt + 1
      End If
    Next
  Next
  For i = 1 To ds(dsIdx).numAge + 2
    vaSpread1.ColWidth(i) = vaSpread1.MaxTextColWidth(i) + 2
  Next
  CompleteSpread i - 1, rowcnt - 1, tRow
End Sub

Private Sub GenOpt2Table()
Dim i As Long
Dim j As Long
Dim k As Long
Dim m As Long
Dim dsIdx As Long
Dim ageIdx As Long
Dim xyIdx As Long
Dim casid As String
Dim parentCAS As String
Dim timeIdx As Long
Dim orgIdx As Long
Dim timevalue As Double
Dim datavalue As Double
Dim tRow As Long
Dim rowcnt As Long
Dim active As Boolean

  StartSpread
  'set description info
  vaSpread1.SetText 1, 1, "Time"
  vaSpread1.SetText 2, 1, cboTime
  vaSpread1.SetText 1, 2, "Location"
  vaSpread1.SetText 2, 2, cboXY
  vaSpread1.SetText 1, 3, "Constituent"
  vaSpread1.SetText 2, 3, cboConst.Text
  vaSpread1.SetText 1, 4, "Dataset"
  vaSpread1.SetText 2, 4, cboName
  vaSpread1.SetText 1, 5, "Ages"
  vaSpread1.SetText 2, 5, cboAge.Text
  If cboOrgCancer.Enabled And cboOrgCancer.Visible Then
    tRow = 7
    vaSpread1.SetText 1, 6, "Cancer to"
    vaSpread1.SetText 2, 6, cboOrgCancer.Text
    orgIdx = cboOrgCancer.ListIndex
  ElseIf cboOrgDose.Enabled And cboOrgDose.Visible Then
    tRow = 7
    vaSpread1.SetText 1, 7, "Dose to"
    vaSpread1.SetText 2, 7, cboOrgDose.Text
    orgIdx = cboOrgDose.ListIndex
  Else
    tRow = 7
    orgIdx = 0
  End If
  'set table heeaders
  vaSpread1.SetText 1, tRow, "Exposure Pathways"
  vaSpread1.SetText 2, tRow, "Exposure Routes"
    
  dsIdx = cboName.ListIndex
  ageIdx = cboAge.ListIndex
  casid = allCAS(cboConst.ListIndex + 1).cas
  parentCAS = allCAS(cboConst.ListIndex + 1).pcas(0)
  xyIdx = cboXY.ListIndex
  timeIdx = cboTime.ListIndex
  
  rowcnt = tRow + 1
  For j = 1 To UBound(uRoutes)
    For k = 1 To UBound(uPaths)
      active = False
      For i = 1 To UBound(uMeasure)
        If rowcnt = tRow + 1 Then vaSpread1.SetText i + 2, tRow, uMeasure(i)
        For m = 1 To UBound(uExposure)
          If uRoutes(j) = uExposure(m).Route And uPaths(k) = uExposure(m).Path And _
             uMeasure(i) = (uExposure(m).measure & " (" & uExposure(m).Unit & ")") Then
            timevalue = hifLoadTimeSeries(dsIdx, xyIdx, ageIdx, orgIdx, casid, parentCAS, uPaths(k), uRoutes(j), uExposure(m).measure, uExposure(m).Unit)
            If timevalue > 0 Then
              hifGetTimeAndValue dsIdx, timeIdx, timevalue, datavalue
            Else
              datavalue = 0
            End If
            vaSpread1.SetText 1, rowcnt, uPaths(k)
            vaSpread1.SetText 2, rowcnt, uRoutes(j)
            vaSpread1.SetText i + 2, rowcnt, Format(datavalue, "0.0##E+00")
            active = True
          End If
        Next
      Next
      If active Then rowcnt = rowcnt + 1
    Next
  Next
  For i = 1 To UBound(uMeasure) + 2
    vaSpread1.ColWidth(i) = vaSpread1.MaxTextColWidth(i) + 2
  Next
  CompleteSpread i - 1, rowcnt - 1, tRow
End Sub

Private Sub GenOpt3Table()
Dim i As Long
Dim j As Long
Dim dsIdx As Long
Dim ageIdx As Long
Dim xyIdx As Long
Dim casid As String
Dim parentCAS As String
Dim timeIdx As Long
Dim Path As String
Dim Route As String
Dim measure As String
Dim Unit As String
Dim timevalue As Double
Dim datavalue As Double
Dim rowcnt As Long
Dim norg As Long

  StartSpread
  'set description info
  vaSpread1.SetText 1, 1, "Time"
  vaSpread1.SetText 2, 1, cboTime
  vaSpread1.SetText 1, 2, "Location"
  vaSpread1.SetText 2, 2, cboXY
  vaSpread1.SetText 1, 3, "Constituent"
  vaSpread1.SetText 2, 3, cboConst.Text
  vaSpread1.SetText 1, 4, "Dataset"
  vaSpread1.SetText 2, 4, cboName
  vaSpread1.SetText 1, 5, "Pathway"
  vaSpread1.SetText 2, 5, cboPath.Text
  vaSpread1.SetText 1, 6, "Route"
  vaSpread1.SetText 2, 6, cboRoute.Text
  vaSpread1.SetText 1, 7, "Measure"
  vaSpread1.SetText 2, 7, cboMeasure
    
  dsIdx = cboName.ListIndex
  ageIdx = cboAge.ListIndex
  casid = allCAS(cboConst.ListIndex + 1).cas
  parentCAS = allCAS(cboConst.ListIndex + 1).pcas(0)
  timeIdx = cboTime.ListIndex
  xyIdx = cboXY.ListIndex
  Route = cboRoute.Text
  Path = cboPath.Text
  measure = uExposure(cboMeasure.ItemData(cboMeasure.ListIndex)).measure
  Unit = uExposure(cboMeasure.ItemData(cboMeasure.ListIndex)).Unit
  
  Select Case measure
  Case "dose": norg = ds(dsIdx).ndOrgans
    vaSpread1.SetText 1, 9, "Dose Organ"
  Case "risk": norg = ds(dsIdx).ncOrgans
    vaSpread1.SetText 1, 9, "Cancer Organ"
  Case "hi":   norg = 1
    vaSpread1.SetText 1, 9, "Unit"
  End Select
  
  rowcnt = 10
  For i = 0 To ds(dsIdx).numAge - 1
    If i = 0 Then vaSpread1.SetText i + 2, 9, "Ages " + CStr(ds(dsIdx).ageMin(i)) + " to " + CStr(ds(dsIdx).ageMax(i))
    For j = 0 To norg - 1
      timevalue = hifLoadTimeSeries(dsIdx, xyIdx, i, j, casid, parentCAS, Path, Route, measure, Unit)
      If timevalue > 0 Then
        hifGetTimeAndValue dsIdx, timeIdx, timevalue, datavalue
      Else
        datavalue = 0
      End If
      If i = 0 Then
        Select Case measure
          Case "dose": vaSpread1.SetText 1, j + rowcnt, ds(dsIdx).dOrgans(j)
          Case "risk": vaSpread1.SetText 1, j + rowcnt, ds(dsIdx).cOrgans(j)
          Case "hi":   vaSpread1.SetText 1, j + rowcnt, Unit
        End Select
      End If
      vaSpread1.SetText i + 2, j + 10, Format(datavalue, "0.0##E+00")
     Next
  Next
  For i = 1 To ds(dsIdx).numAge + 1
    vaSpread1.ColWidth(i) = vaSpread1.MaxTextColWidth(i) + 2
  Next
  CompleteSpread i - 1, rowcnt + norg - 1, 9
End Sub

Private Sub GenOpt4Table()
Dim i As Long
Dim j As Long
Dim m As Long
Dim n As Long
Dim dsIdx As Long
Dim ageIdx As Long
Dim xyIdx As Long
Dim casid As String
Dim parentCAS As String
Dim Path As String
Dim Route As String
Dim Unit As String
Dim measure As String
Dim timevalue As Double
Dim datavalue As Double
Dim maxtime As Double
Dim maxdata As Double
Dim maxloc As Long
Dim rowcnt As Long
Dim colcnt As Long
Dim norg As Long
Dim ntime As Long
  
  StartSpread
  'set description info
  vaSpread1.SetText 1, 1, "Dataset"
  vaSpread1.SetText 2, 1, cboName
  vaSpread1.SetText 1, 2, "Location"
  vaSpread1.SetText 2, 2, cboXY
  vaSpread1.SetText 1, 3, "Constituent"
  vaSpread1.SetText 2, 3, cboConst.Text
  vaSpread1.SetText 1, 4, "Time"
  vaSpread1.SetText 2, 4, cboTime
  vaSpread1.SetText 1, 5, "Measure"
  vaSpread1.SetText 2, 5, cboMeasure
  vaSpread1.SetText 1, 6, "Route"
  vaSpread1.SetText 2, 6, cboRoute.Text
  vaSpread1.SetText 1, 7, "Pathway"
  vaSpread1.SetText 2, 7, cboPath.Text
    
  dsIdx = cboName.ListIndex
  ageIdx = cboAge.ListIndex
  xyIdx = cboXY.ListIndex
  casid = allCAS(cboConst.ListIndex + 1).cas
  parentCAS = allCAS(cboConst.ListIndex + 1).pcas(0)
  Route = cboRoute.Text
  Path = cboPath.Text
  measure = uExposure(cboMeasure.ItemData(cboMeasure.ListIndex)).measure
  Unit = uExposure(cboMeasure.ItemData(cboMeasure.ListIndex)).Unit
  
  Select Case measure
  Case "dose": norg = ds(dsIdx).ndOrgans
    vaSpread1.SetText 1, 9, "Dose Organ"
  Case "risk": norg = ds(dsIdx).ncOrgans
    vaSpread1.SetText 1, 9, "Cancer Organ"
  Case "hi":   norg = 1
    vaSpread1.SetText 1, 9, "Unit"
  End Select
  
  rowcnt = 10
  colcnt = 2
  For i = 0 To ds(dsIdx).numAge - 1
    For j = 0 To norg - 1
      If i = 0 Then
        Select Case measure
          Case "dose": vaSpread1.SetText 1, j + rowcnt, ds(dsIdx).dOrgans(j)
          Case "risk": vaSpread1.SetText 1, j + rowcnt, ds(dsIdx).cOrgans(j)
          Case "hi":   vaSpread1.SetText 1, j + rowcnt, Unit
        End Select
      End If
      maxdata = -1
      For n = 0 To cboXY.ListCount - 1
        ntime = hifLoadTimeSeries(dsIdx, n, i, j, casid, parentCAS, Path, Route, measure, Unit)
        For m = 0 To ntime - 1
          hifGetTimeAndValue dsIdx, m, timevalue, datavalue
          If datavalue > maxdata Then
            maxloc = n
            maxtime = timevalue
            maxdata = datavalue
          End If
        Next
      Next
      vaSpread1.SetText colcnt, 9, "Maximum for ages " & CStr(ds(dsIdx).ageMin(i)) + " to " + CStr(ds(dsIdx).ageMax(i))
      vaSpread1.SetText colcnt, j + rowcnt, Format(maxdata, "0.0##E+00")
      vaSpread1.SetText colcnt + 1, 9, "Location and time for ages " & CStr(ds(dsIdx).ageMin(i)) + " to " + CStr(ds(dsIdx).ageMax(i))
      vaSpread1.SetText colcnt + 1, j + rowcnt, "Max located at (" & ds(dsIdx).x(maxloc) & "," & ds(dsIdx).y(maxloc) & ") at time " & CStr(maxtime)
      colcnt = colcnt + 2
    Next
  Next
  For i = 1 To ds(dsIdx).numAge * 2 + 1
    vaSpread1.ColWidth(i) = vaSpread1.MaxTextColWidth(i) + 2
  Next
  CompleteSpread i - 1, rowcnt + norg - 1, 9
End Sub

Sub StartSpread()
Dim i As Long

  vaSpread1.Visible = False
  vaSpread1.MaxCols = 100
  vaSpread1.MaxRows = 100
  vaSpread1.row = -1
  vaSpread1.col = -1
  vaSpread1.row = -1
  vaSpread1.col = -1
  vaSpread1.BackColor = vbWhite
  vaSpread1.FontBold = False
  vaSpread1.TypeHAlign = 0
  vaSpread1.Action = 12
  For i = 1 To 100
    vaSpread1.ColWidth(i) = 10
  Next
End Sub

Sub CompleteSpread(col As Long, length As Long, row As Long)
  vaSpread1.MaxCols = col
  vaSpread1.MaxRows = length
  If length > 15 Then
    vaSpread1.VisibleRows = 15
  Else
    vaSpread1.VisibleRows = length
  End If
  'info labels
  vaSpread1.row = 1
  vaSpread1.row2 = row - 2
  vaSpread1.col = 1
  vaSpread1.col2 = 1
  vaSpread1.BlockMode = True
  vaSpread1.FontBold = True
  vaSpread1.BackColor = &HC0FFFF    ' vbYellow
  vaSpread1.BlockMode = False
  'field labels
  vaSpread1.row = row
  vaSpread1.row2 = row
  vaSpread1.col = 1
  vaSpread1.col2 = col
  vaSpread1.BlockMode = True
  vaSpread1.BackColor = vbCyan
  vaSpread1.BlockMode = False
  vaSpread1.AutoSize = True
  vaSpread1.ReDraw = True
  vaSpread1.Visible = True
End Sub

