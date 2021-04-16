VERSION 5.00
Object = "{FE0065C0-1B7B-11CF-9D53-00AA003C9CB6}#1.1#0"; "COMCT232.OCX"
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Object = "{F856EC8B-F03C-4515-BDC6-64CBD617566A}#7.0#0"; "FPSPR70.ocx"
Begin VB.Form frmSeries 
   Caption         =   "Probability of Exceedence"
   ClientHeight    =   8820
   ClientLeft      =   60
   ClientTop       =   630
   ClientWidth     =   6405
   Icon            =   "frmSeries.frx":0000
   LinkTopic       =   "Form1"
   MinButton       =   0   'False
   ScaleHeight     =   8820
   ScaleWidth      =   6405
   StartUpPosition =   2  'CenterScreen
   Begin VB.Frame Frame3 
      Height          =   615
      Left            =   120
      TabIndex        =   19
      Top             =   0
      Visible         =   0   'False
      Width           =   5805
      Begin VB.TextBox txtFile 
         Enabled         =   0   'False
         Height          =   288
         Index           =   1
         Left            =   240
         TabIndex        =   21
         Text            =   "Browse for benchmarks file"
         Top             =   240
         Visible         =   0   'False
         Width           =   4932
      End
      Begin VB.CommandButton btnBrowse 
         Caption         =   "..."
         Height          =   252
         Index           =   1
         Left            =   5280
         TabIndex        =   20
         Top             =   240
         Visible         =   0   'False
         Width           =   372
      End
   End
   Begin VB.Frame Frame1 
      Height          =   1840
      Left            =   120
      TabIndex        =   4
      Top             =   0
      Width           =   5805
      Begin VB.TextBox txtFile 
         Enabled         =   0   'False
         Height          =   288
         Index           =   0
         Left            =   240
         TabIndex        =   18
         Text            =   "Browse for benchmarks file"
         Top             =   1480
         Visible         =   0   'False
         Width           =   4932
      End
      Begin VB.CommandButton btnBrowse 
         Caption         =   "..."
         Height          =   252
         Index           =   0
         Left            =   5280
         TabIndex        =   17
         Top             =   1440
         Visible         =   0   'False
         Width           =   372
      End
      Begin VB.TextBox txtSecondBound 
         Alignment       =   2  'Center
         Enabled         =   0   'False
         Height          =   285
         Left            =   2760
         TabIndex        =   13
         Text            =   "95"
         Top             =   680
         Visible         =   0   'False
         Width           =   420
      End
      Begin ComCtl2.UpDown UpDown4 
         Height          =   288
         Left            =   3180
         TabIndex        =   14
         Top             =   684
         Visible         =   0   'False
         Width           =   240
         _ExtentX        =   450
         _ExtentY        =   503
         _Version        =   327681
         Value           =   95
         OrigLeft        =   3720
         OrigTop         =   120
         OrigRight       =   3960
         OrigBottom      =   495
         Increment       =   5
         Max             =   95
         Min             =   5
         SyncBuddy       =   -1  'True
         BuddyProperty   =   0
         Enabled         =   -1  'True
      End
      Begin VB.TextBox txtFirstBound 
         Alignment       =   2  'Center
         Enabled         =   0   'False
         Height          =   285
         Left            =   240
         TabIndex        =   11
         Text            =   "5"
         Top             =   680
         Visible         =   0   'False
         Width           =   420
      End
      Begin ComCtl2.UpDown UpDown3 
         Height          =   288
         Left            =   660
         TabIndex        =   12
         Top             =   684
         Visible         =   0   'False
         Width           =   252
         _ExtentX        =   450
         _ExtentY        =   503
         _Version        =   327681
         Value           =   5
         OrigLeft        =   660
         OrigTop         =   684
         OrigRight       =   912
         OrigBottom      =   972
         Increment       =   5
         Max             =   95
         Min             =   5
         Enabled         =   -1  'True
      End
      Begin VB.CheckBox Check1 
         Caption         =   "Use non-zero values only"
         Height          =   192
         Left            =   240
         TabIndex        =   9
         Top             =   360
         Width           =   5412
      End
      Begin VB.TextBox txtIntv 
         Alignment       =   2  'Center
         Enabled         =   0   'False
         Height          =   285
         Left            =   240
         TabIndex        =   6
         Text            =   "10"
         Top             =   1080
         Width           =   420
      End
      Begin ComCtl2.UpDown UpDown2 
         CausesValidation=   0   'False
         Height          =   288
         Left            =   660
         TabIndex        =   5
         Top             =   1080
         Width           =   252
         _ExtentX        =   450
         _ExtentY        =   503
         _Version        =   327681
         Value           =   10
         OrigLeft        =   2625
         OrigTop         =   345
         OrigRight       =   2865
         OrigBottom      =   690
         Increment       =   10
         Max             =   100
         Min             =   10
         Enabled         =   -1  'True
      End
      Begin VB.CheckBox Check2 
         Caption         =   "Use Kolmogorov-Smirnov Goodness-of-Fit confidence intervals"
         Height          =   192
         Left            =   240
         TabIndex        =   10
         Top             =   720
         Width           =   5412
      End
      Begin VB.Label lblUpperbound 
         Caption         =   "Second Bound"
         Height          =   252
         Left            =   3600
         TabIndex        =   16
         Top             =   720
         Width           =   1572
      End
      Begin VB.Label lblLowerbound 
         Caption         =   "First Bound"
         Height          =   252
         Left            =   1040
         TabIndex        =   15
         Top             =   720
         Width           =   1572
      End
      Begin VB.Label Label2 
         AutoSize        =   -1  'True
         Caption         =   "Number of exceedence intervals"
         Height          =   192
         Left            =   1044
         TabIndex        =   7
         Top             =   1110
         Width           =   2316
      End
   End
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
   Begin VB.Frame Frame2 
      Height          =   7140
      Left            =   120
      TabIndex        =   0
      Top             =   1480
      Width           =   5805
      Begin VB.TextBox txtCount 
         Alignment       =   2  'Center
         Enabled         =   0   'False
         Height          =   285
         Left            =   2415
         TabIndex        =   1
         Text            =   "0"
         Top             =   420
         Width           =   420
      End
      Begin ComCtl2.UpDown UpDown1 
         Height          =   285
         Left            =   2835
         TabIndex        =   2
         Top             =   420
         Width           =   240
         _ExtentX        =   450
         _ExtentY        =   503
         _Version        =   327681
         Value           =   1
         OrigLeft        =   3720
         OrigTop         =   120
         OrigRight       =   3960
         OrigBottom      =   495
         Max             =   25
         Min             =   1
         SyncBuddy       =   -1  'True
         BuddyProperty   =   0
         Enabled         =   -1  'True
      End
      Begin FPSpreadADO.fpSpread vaSpread1 
         Height          =   6000
         Left            =   240
         TabIndex        =   8
         Top             =   912
         Width           =   5340
         _Version        =   458752
         _ExtentX        =   9419
         _ExtentY        =   10583
         _StockProps     =   64
         AutoCalc        =   0   'False
         BackColorStyle  =   1
         ColHeaderDisplay=   0
         EditEnterAction =   7
         EditModeReplace =   -1  'True
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         FormulaSync     =   0   'False
         MaxCols         =   2
         ScrollBarExtMode=   -1  'True
         ScrollBarShowMax=   0   'False
         SpreadDesigner  =   "frmSeries.frx":030A
      End
      Begin VB.Label Label3 
         AutoSize        =   -1  'True
         Caption         =   "Number of charts (max 25):"
         Height          =   192
         Left            =   216
         TabIndex        =   3
         Top             =   420
         Width           =   1884
      End
   End
   Begin VB.Menu mnuShow 
      Caption         =   "Chart"
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
         Caption         =   "How to ..."
      End
      Begin VB.Menu about 
         Caption         =   "&About"
      End
   End
End
Attribute VB_Name = "frmSeries"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Private IgnoreEvents As Boolean
Private mConstitRowSize() As Long
Private mConstitRowSizeLength As Integer

Private Sub about_Click()
  frmAbout.picIcon = frmAbout.ImageList1.ListImages(1).Picture
  frmAbout.Show 0
End Sub
Private Sub btnBrowse_Click(Index As Integer)
    Dim result As String
    
    On Error Resume Next
    With CommonDialog1
        .Filter = "XML File (*.xml,*.xsl)|*.xml;*xsl"
        .ShowOpen
    End With
    
    If Err.Number > 0 Then
      Err.Clear
    Else
      If IsBenchmark(CommonDialog1.fileName, result) Then
        txtFile(Index).Text = CommonDialog1.fileName
        XMLUrl = CommonDialog1.fileName
      Else
        txtFile(Index).Text = XMLUrl
        If Len(result) > 0 Then txtFile(Index).Text = "Browse for benchmarks file"
        MsgBox result
      End If
    End If
End Sub


Private Sub Form_Load()
  Dim benchmark_message As String
  
  On Error GoTo ErrorHandler
  mConstitRowSizeLength = 1
  ReDim mConstitRowSize(0)
  IgnoreEvents = True
  
  ' miscellaneous initializations
  Set activeForm = Me
  Set activeSpread = vaSpread1
  
  txtCount = 1
  Set frm = Me
  If viewType = "PE" Then
    zeroes = 1
    resolution = 10
    txtIntv = resolution
    ReDim peArray(UpDown1.max)
    caption = " - Probability of Exceedence Series"
    SetHelpFile App.Path + "\Probability.htm"
    Frame1.Height = 1450
    Frame2.Top = 1480
  Else
    Select Case pdcfType
      Case "sufscf", "sufwcf"
        Check2.Visible = False
        Frame1.Height = 1840
        Frame2.Top = 1820
        btnBrowse(0).Visible = True
        txtFile(0).Visible = True
        txtFirstBound.Visible = True
        txtSecondBound.Visible = True
        UpDown3.Visible = True
        UpDown4.Visible = True
        SetHelpFile App.Path + "\Probability.htm"
        If IsBenchmark(FUIName + "_benchmarks.xml", benchmark_message) Then
          txtFile(0).Text = FUIName + "_benchmarks.xml"
          XMLUrl = txtFile(0).Text
        End If
      Case "mrkwcf", "mrkscf"
        Frame3.Height = 615
        Frame2.Top = 595
        btnBrowse(1).Visible = True
        txtFile(1).Visible = True
        Frame1.Visible = False
        Frame3.Visible = True
        SetHelpFile App.Path + "\Graphical.htm"
        If IsBenchmark(FUIName + "_benchmarks.xml", benchmark_message) Then
          txtFile(1).Text = FUIName + "_benchmarks.xml"
          XMLUrl = txtFile(1).Text
        End If
      Case Else
        Frame2.Top = Frame1.Top
        Frame1.Visible = False
        SetHelpFile App.Path + "\Graphical.htm"
    End Select
    caption = " - Time Series"
  End If

  IgnoreEvents = False
  
  Select Case pdcfType
    Case "scf", "sufscf", "mrkscf":
      caption = "Constituent Soil/Sediment Concentrations" & caption
      scfLoadDatasets
      If AnError Or numDS = 0 Then Unload Me
      InitSelect 1
      vaSpread1_Change 2, CBO_DS
    Case "aff":
      caption = "Constituent Air Flux" & caption
      affLoadDatasets
      If AnError Or numDS = 0 Then Unload Me
      InitSelect 1
      vaSpread1_Change 2, CBO_DS
    Case "ato"
      caption = "Constituent Air Concentration and Deposistion" & caption
      atoLoadDatasets
      If AnError Or numDS = 0 Then Unload Me
      'atoInitSelect 1
      InitSelect 1
      vaSpread1_Change 2, CBO_DS
    Case "wff":
      caption = "Constituent Water Flux" & caption
      wffLoadDatasets
      If AnError Or numDS = 0 Then Unload Me
      InitSelect 1
      vaSpread1_Change 2, CBO_DS
    Case "wcf", "sufwcf", "mrkwcf":
      caption = "Constituent Water Concentrations" & caption
      wcfLoadDatasets
      If AnError Or numDS = 0 Then Unload Me
      InitSelect 1
      vaSpread1_Change 2, CBO_DS
    Case "bbf"
      caption = "Aquatic Organism Constituent Body Burdens" & caption
      bbfLoadDatasets
      If AnError Or numDS = 0 Then Unload Me
      InitSelect 1
      vaSpread1_Change 2, CBO_DS
    Case "epf":
      caption = "Exposure Pathway Concentrations" & caption
      epfLoadDatasets
      If AnError Or numDS = 0 Then Unload Me
      InitSelect 1
      vaSpread1_Change 2, CBO_DS
    Case "rif":
      caption = "Receptor Intakes" & caption
      rifLoadDatasets
      If AnError Or numDS = 0 Then Unload Me
      InitSelect 1
      vaSpread1_Change 2, CBO_DS
    Case "hif":
      caption = "Health Impacts" & caption
      hifLoadDatasets
      If AnError Or numDS = 0 Then Unload Me
      InitSelect 1
      vaSpread1_Change 2, CBO_DS
    Case "hqf"
      caption = "Ecologcial Effects and Hazard Quotient" & caption
      exfLoadDatasets
      If AnError Or numDS = 0 Then Unload Me
      InitSelect 1
      vaSpread1_Change 2, CBO_DS
    Case Else:
      PutError "Unknown file type -> " & pdcfType
  End Select

ErrorHandler:
  If Err.Number <> 0 Then
    PutError Err.Description & " from frmSeries_Load"
    Unload Me
  End If
  Form_Resize
End Sub

Private Sub Form_Unload(Cancel As Integer)
  On Error Resume Next
  Select Case pdcfType
    Case "scf", "sufscf", "mrkscf":      scfClose
    Case "aff":      affClose
    Case "ato":      atoClose
    Case "wff":      wffClose
    Case "wcf", "sufwcf", "mrkwcf":      wcfClose
    Case "bbf":      bbfClose
    Case "epf":      epfClose
    Case "rif":      rifClose
    Case "hif":      hifClose
    Case "hqf":      exfClose
  End Select

  If main.savefile Then
    On Error Resume Next
    wbo.DisplayAlerts = False
    wso.Saveas FUIName & "." & modName & ".xls", 1
    wbo.DisplayAlerts = True
  End If
  
  Set wbo = Nothing
  Set cho = Nothing
  EndModule
End Sub

Private Sub Form_Resize()
  If IgnoreEvents Then Exit Sub
  If Me.ScaleWidth = 0 Or Me.ScaleHeight = 0 Then Exit Sub
  If Frame2.Visible Then
    If viewType = "PE" Or pdcfType = "sufscf" Or pdcfType = "sufwcf" Then
      If 50 < Me.ScaleHeight - (Frame1.Height + 50) Then
        Frame2.Height = Me.ScaleHeight - (Frame1.Height + 50)
      End If
    Else
      If 50 < Me.ScaleHeight Then
        Frame2.Height = Me.ScaleHeight
      End If
    End If
    If 50 < Me.ScaleWidth - 2 * Frame1.Left Then
      Frame2.width = Me.ScaleWidth - 2 * Frame1.Left
    End If
    If 50 < Frame2.Height - 1.2 * vaSpread1.Top Then
      vaSpread1.Height = Frame2.Height - 1.2 * vaSpread1.Top
    End If
    If 50 < Frame2.width - 2 * vaSpread1.Left Then
      vaSpread1.width = Frame2.width - 2 * vaSpread1.Left
    End If
  End If
  If Frame3.Visible Then
    If 50 < Me.ScaleHeight - (Frame3.Height + 50) Then
      Frame2.Height = Me.ScaleHeight - (Frame3.Height + 50)
    End If
    If 50 < Me.ScaleWidth - 2 * Frame1.Left Then
      Frame3.width = Me.ScaleWidth - 2 * Frame1.Left
    End If
    If 50 < Frame2.Height - 1.2 * vaSpread1.Top Then
      vaSpread1.Height = Frame2.Height - 1.2 * vaSpread1.Top
    End If
    If 50 < Frame3.width - 2 * vaSpread1.Left Then
      vaSpread1.width = Frame3.width - 2 * vaSpread1.Left
    End If
  End If
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

Private Sub mnuShow_Click()
  If viewType = "PE" Then
    peCreateChart CLng(txtCount.Text)
  Else
    CreateChart
  End If
End Sub

Sub CreateChart()
  Dim numRow As Long
  Dim row As Long
  Dim off As Long
  Dim vstr
  Dim nhdr As Long
  Dim ncol As Long
  Dim xrange As String
  Dim yrange As String
  Dim Title As String
  Dim Yaxis As String
  Dim ns As Object

  nhdr = numSel + 2
  numRow = activeSpread.DataRowCnt

  If numRow - nhdr = 0 Then
    MsgBox "There are no results." & vbCrLf & "Please make another selection.", , "Note"
    Exit Sub
  End If
  
  If Not GetWorkbookObject() Then
    MsgBox "Error getting Excel workbook object"
    Exit Sub
  End If
                            
  For ncol = 0 To numCol - 1
    Set wso = GetWorkSheetObject(WorkSheetName("Criteria #" + CStr(ncol + 1)))
    Set cho = GetChartObject(wso)
    wbo.Interactive = False
    'copy header info
    off = ncol * 2 + 1
    row = 1: xlsSetCell wso, 1, row, "File:": xlsSetCell wso, 2, row, FUIName + "." + pdcfType
    row = 2: xlsSetCell wso, 1, row, "Module:": xlsSetCell wso, 2, row, modLabel + " (" + modName + ")"
    For row = 3 To numRow + 3
      activeSpread.GetText off, row - 2, vstr:    xlsSetCell wso, 1, row, vstr
      activeSpread.GetText off + 1, row - 2, vstr: xlsSetCell wso, 2, row, vstr
    Next row
    xrange = excelRange(1, nhdr + 3, 1, numRow + 2)
    yrange = excelRange(2, nhdr + 3, 2, numRow + 2)
    Set ns = AddSeriesToChart(wso, cho, "Concentration Series", xrange, yrange)
    
    Select Case pdcfType
      Case "ato"
        activeSpread.GetText off + 1, CBO_DEP, vstr:   Title = ""
        If vstr <> "Not Available" Then Title = UCase(Left(vstr, 1)) + Right(vstr, Len(vstr) - 1)
        activeSpread.GetText off + 1, CBO_MSR, vstr:   Title = Title + " " + vstr + " for "
        activeSpread.GetText off + 1, CBO_CHM, vstr:   Title = Title + vstr
        activeSpread.GetText off + 1, CBO_FLX, vstr:   Title = Title + " from " + vstr + " flux"
        CompleteChartObject cho, Title, xlabel, ylabel
      Case "scf":
        activeSpread.GetText off + 1, CBO_DS, vstr:    Title = vstr
        Title = Title + " Constituent Concentration for "
        activeSpread.GetText off + 1, CBO_CHM, vstr:   Title = Title + vstr
        activeSpread.GetText off + 1, CBO_CHM + 2, vstr: ylabel = vstr
        CompleteChartObject cho, Title, xlabel, ylabel
      Case "aff":
        activeSpread.GetText off + 1, CBO_DS, vstr:    Title = vstr
        activeSpread.GetText off + 1, CBO_FLX, vstr:   Title = Title + " " + vstr
        Title = Title + " Constituent Flux for "
        activeSpread.GetText off + 1, CBO_CHM, vstr:   Title = Title + vstr
        CompleteChartObject cho, Title, xlabel, ylabel
      Case "wff":
        activeSpread.GetText off + 1, CBO_DS, vstr:    Title = vstr
        activeSpread.GetText off + 1, CBO_FLX, vstr:   Title = Title + " " + vstr
        Title = Title + " Constituent Flux for "
        activeSpread.GetText off + 1, CBO_CHM, vstr:   Title = Title + vstr
        CompleteChartObject cho, Title, xlabel, ylabel
      Case "wcf":
        activeSpread.GetText off + 1, CBO_DS, vstr:    Title = vstr
        Title = Title + " Constituent Concentration for "
        activeSpread.GetText off + 1, CBO_CHM, vstr:   Title = Title + vstr
        CompleteChartObject cho, Title, xlabel, ylabel
      Case "bbf":
        Title = " Aquatic Body Burdens for "
        activeSpread.GetText off + 1, CBO_OR, vstr:    Title = Title + vstr
        activeSpread.GetText off + 1, CBO_CHM, vstr:   Title = Title + " exposed to " + vstr
        activeSpread.GetText off + 1, CBO_DS, vstr:    Title = Title + " from " + vstr
        activeSpread.GetText off + 1, CBO_VU, vstr:    Title = Title + " having " + vstr
        CompleteChartObject cho, Title, xlabel, ylabel
      Case "epf":
        Title = " Exposure Concentrations for "
        activeSpread.GetText off + 1, nhdr, vstr:      Yaxis = "Concentration in " & vstr
        activeSpread.GetText off + 1, CBO_CHM, vstr:   Title = vstr + Title
        activeSpread.GetText off + 1, CBO_RTE, vstr:   Title = Title + vstr + " route(s) and "
        activeSpread.GetText off + 1, CBO_PTH, vstr:   Title = Title + vstr + " pathway(s)"
        CompleteChartObject cho, Title, "Start Time in " + xlabel, Yaxis
      Case "rif":
        Title = " Receptor Intakes for "
        activeSpread.GetText off + 1, CBO_MSR, vstr
        If vstr <> "intake" Then
          Yaxis = vstr & " Intake"
        Else
          Yaxis = vstr
        End If
        activeSpread.GetText off + 1, CBO_UNT, vstr:   Yaxis = Yaxis + " (" + vstr + ")"
        activeSpread.GetText off + 1, CBO_CHM, vstr:   Title = vstr + Title
        activeSpread.GetText off + 1, CBO_RTE, vstr:   Title = Title + vstr + " route(s) and "
        activeSpread.GetText off + 1, CBO_PTH, vstr:   Title = Title + vstr + " pathway(s)"
        CompleteChartObject cho, Title, "Start Time in " + xlabel, Yaxis
      Case "hif":
        Title = " Health Impacts for "
        activeSpread.GetText off + 1, CBO_MSR, vstr:   Yaxis = vstr
        activeSpread.GetText off + 1, CBO_UNT, vstr:   Yaxis = Yaxis + " (" + vstr + ")"
        activeSpread.GetText off + 1, CBO_CHM, vstr:   Title = vstr + Title
        activeSpread.GetText off + 1, CBO_RTE, vstr:   Title = Title + vstr + " route(s) and "
        activeSpread.GetText off + 1, CBO_PTH, vstr:   Title = Title + vstr + " pathway(s)"
        CompleteChartObject cho, Title, "Start Time in " + xlabel, Yaxis
      Case "hqf":
        Title = " Ecological Hazard Quotient for "
        activeSpread.GetText off + 1, CBO_OR, vstr:    Title = Title + vstr
        activeSpread.GetText off + 1, CBO_CHM, vstr:   Title = Title + " exposed to " + vstr
        activeSpread.GetText off + 1, CBO_DS, vstr:    Title = Title + " from " + vstr
        activeSpread.GetText off + 1, CBO_EFX, vstr:   Title = Title + " using " + vstr
        CompleteChartObject cho, Title, xlabel, ylabel
      Case "sufscf":
        activeSpread.GetText off + 1, CBO_DS, vstr:    Title = vstr
        Title = Title + " Bound Bars Constituent Concentration for "
        activeSpread.GetText off + 1, CBO_CHM, vstr:   Title = Title + vstr
        activeSpread.GetText off + 1, CBO_CHM + 2, vstr: ylabel = vstr
        ns.name = "Constituent Concentration"
        CompleteChartObject cho, Title, xlabel, ylabel
        confidenceIntervalChart cho, FUIName + ".suf"
      Case "sufwcf":
        activeSpread.GetText off + 1, CBO_DS, vstr:    Title = vstr
        Title = Title + " Bounding Bars Constituent Concentration for "
        activeSpread.GetText off + 1, CBO_CHM, vstr:   Title = Title + vstr
        ns.name = "Constituent Concentration"
        CompleteChartObject cho, Title, xlabel, ylabel
        confidenceIntervalChart cho, FUIName + ".suf"
      Case "mrkscf":
        activeSpread.GetText off + 1, CBO_DS, vstr:    Title = vstr
        Title = Title + " Constituent Concentration for "
        activeSpread.GetText off + 1, CBO_CHM, vstr:   Title = Title + vstr
        activeSpread.GetText off + 1, CBO_CHM + 2, vstr: ylabel = vstr
        CompleteChartObject cho, Title, xlabel, ylabel
        benchmarkChart cho
      Case "mrkwcf":
        activeSpread.GetText off + 1, CBO_DS, vstr:    Title = vstr
        Title = Title + " Constituent Concentration for "
        activeSpread.GetText off + 1, CBO_CHM, vstr:   Title = Title + vstr
        CompleteChartObject cho, Title, xlabel, ylabel
        benchmarkChart cho
    End Select
  Next
  wbo.Interactive = True
End Sub

Private Sub UpdateSpread(ByVal col As Long, ByVal row As Long, change As Boolean)
  Dim i As Integer
  Dim nrow As Integer
  Dim peIdx As Long
  Dim toprow As Long
  Dim retcode As Boolean
  
  On Error GoTo ErrorHandler
  
  If Not vaSpread1.Enabled Then Exit Sub
  toprow = vaSpread1.toprow
  activeForm.MousePointer = vbHourglass
  vaSpread1.ReDraw = False
  vaSpread1.Enabled = False
  vaSpread1.row = numSel + 2
  vaSpread1.row2 = activeSpread.DataRowCnt
  vaSpread1.col = col - 1
  vaSpread1.col2 = col
  vaSpread1.BlockMode = True
  vaSpread1.Action = 3 ' clear
  vaSpread1.BlockMode = False
  
  If change Then
    Select Case pdcfType
      'Sheetchange calls must change the
      'series xValues, yvalues and set labels
      Case "ato": retcode = atoSheetChange(col, row)
      Case "aff": retcode = affSheetChange(col, row)
      Case "wff": retcode = wffSheetChange(col, row)
      Case "bbf": retcode = bbfSheetChange(col, row)
      Case "epf": retcode = epfSheetChange(col, row)
      Case "rif": retcode = rifSheetChange(col, row)
      Case "hif": retcode = hifSheetChange(col, row)
      Case "hqf": retcode = exfSheetChange(col, row)
      Case "scf", "sufscf", "mrkscf":
                  retcode = scfSheetChange(col, row)
      Case "wcf", "sufwcf", "mrkwcf":
                  retcode = wcfSheetChange(col, row)
    End Select
  End If
  
  'place values
  If retcode Then
    If "PE" = viewType Then
      peIdx = col / 2
      If GetProbabilitys(peArray(peIdx)) <> 0 Then GoTo Leave
      nrow = numSel + 2
      vaSpread1.SetText col - 1, nrow, ylabel
      vaSpread1.SetText col, nrow, "(%)"
      peArray(peIdx).ylabel = "(%)"
      peArray(peIdx).xlabel = ylabel
      peArray(peIdx).Sheet = "Criteria #" & CStr(peIdx)
      nrow = nrow + 1
      With peArray(peIdx).series(0)
        For i = 0 To UBound(.sprob)
          vaSpread1.SetText col - 1, nrow + i, Format(.svalue(i), "0.0##E+00")
          vaSpread1.SetText col, nrow + i, Format(.sprob(i), "0.0")
        Next
      End With
    Else
      nrow = numSel + 2
      vaSpread1.SetText col - 1, nrow, xlabel
      vaSpread1.SetText col, nrow, ylabel
      nrow = nrow + 1
      mConstitRowSize((col / 2) - 1) = UBound(xValues) + nrow
      For i = 0 To (mConstitRowSizeLength - 1)
        If vaSpread1.MaxRows < mConstitRowSize(i) Then
          vaSpread1.MaxRows = mConstitRowSize(i)
        End If
      Next
      For i = 0 To UBound(xValues) - 1
        vaSpread1.SetText col - 1, nrow + i, CStr(xValues(i))
        vaSpread1.SetText col, nrow + i, Format(yValues(i), "0.0##E+00")
      Next
    End If
  End If
  
Leave:
  If toprow <= vaSpread1.DataRowCnt Then vaSpread1.toprow = toprow
  vaSpread1.Enabled = True
  vaSpread1.ReDraw = True
  activeForm.MousePointer = vbDefault

ErrorHandler:
  If Err.Number <> 0 Then ReportError task, "UpadteSpread"
End Sub

Private Sub UpDown3_Change()
  Me.txtFirstBound.Text = UpDown3.value
End Sub

Private Sub UpDown4_Change()
  Me.txtSecondBound.Text = UpDown4.value
End Sub

Private Sub vaSpread1_Change(ByVal col As Long, ByVal row As Long)
  If Not vaSpread1.Enabled Then Exit Sub
  MousePointer = vbHourglass
  UpdateSpread col, row, True
  MousePointer = vbNormal
End Sub

Private Sub txtCount_Change()
Dim c As Long
  
  If Not activeSpread.Enabled Then Exit Sub
  numCol = activeSpread.MaxCols / 2
  activeSpread.MaxCols = 2 * txtCount
  If numCol < txtCount Then
    For c = numCol + 1 To txtCount
      InitSelect c
      vaSpread1_Change c * 2, CBO_DS
    Next
  End If
  numCol = txtCount
End Sub

Sub InitSelect(col As Long)
  Dim i As Long
  Dim j As Long
  Dim c1 As Long
  Dim c2 As Long
  Dim lbl As String
  Dim cbolist As String
  Dim qualifier As String
  Dim implied As String
  Dim adder As String
  Dim fromto As Boolean
  Dim lbl2 As String
  
  On Error GoTo ErrorHandler:
  
  fromto = True
  adder = ""
  implied = ""
  qualifier = ""
  CBO_DS = 1
  Select Case pdcfType
    Case "wcf", "scf", "sufwcf", "sufscf", "mrkwcf", "mrkscf":
      CBO_CHM = 2
      numSel = CBO_CHM
      If InStr(pdcfType, "scf") > 0 Then
        qualifier = "-Dissolved"
        implied = "-Total"
      End If
      If InStr(pdcfType, "wcf") > 0 Then
        qualifier = "-Total"
        implied = "-Dissolved"
      End If
    Case "wff", "aff":
      CBO_CHM = 2
      CBO_FLX = 3
      numSel = CBO_FLX
    Case "ato":
      CBO_CHM = 2
      CBO_FLX = 3
      CBO_MSR = 4
      CBO_DEP = 5
      CBO_LOCX = 6
      CBO_LOCY = 7
      numSel = CBO_LOCY
    Case "hqf":
      CBO_OR = 2
      CBO_CHM = 3
      CBO_EFX = 4
      numSel = CBO_EFX
    Case "bbf":
      CBO_OR = 2
      CBO_CHM = 3
      CBO_VU = 4
      numSel = CBO_VU
    Case "epf":
      CBO_XY = 2
      CBO_CHM = 3
      CBO_RTE = 4
      CBO_PTH = 5
      numSel = CBO_PTH
      fromto = False
      lbl2 = "Exposures"
    Case "rif":
      CBO_XY = 2
      CBO_AGE = 3
      CBO_CHM = 4
      CBO_MSR = 5
      CBO_UNT = 6
      CBO_RTE = 7
      CBO_PTH = 8
      numSel = CBO_PTH
      fromto = False
      lbl2 = "Intakes"
    Case "hif"
      CBO_XY = 2
      CBO_AGE = 3
      CBO_CHM = 4
      CBO_MSR = 5
      CBO_UNT = 6
      CBO_RTE = 7
      CBO_PTH = 8
      CBO_ORG = 9
      numSel = CBO_ORG
      fromto = False
      lbl2 = "Impacts"
  End Select
  
  'Set col1 and col2
  c2 = (2 * col)
  c1 = c2 - 1
  'Set row labels
  If CBO_DS > 0 Then activeSpread.SetText c1, CBO_DS, "Dataset"
  If CBO_XY > 0 Then activeSpread.SetText c1, CBO_XY, "Location"
  If CBO_OR > 0 Then activeSpread.SetText c1, CBO_OR, "Organism"
  If CBO_VU > 0 Then activeSpread.SetText c1, CBO_VU, "Variability"
  If CBO_FLX > 0 Then activeSpread.SetText c1, CBO_FLX, "Flux"
  If CBO_EFX > 0 Then activeSpread.SetText c1, CBO_EFX, "Toxic Effect"
  If CBO_AGE > 0 Then activeSpread.SetText c1, CBO_AGE, "Age"
  If CBO_CHM > 0 Then activeSpread.SetText c1, CBO_CHM, "Constituent"
  If CBO_MSR > 0 Then activeSpread.SetText c1, CBO_MSR, "Measure"
  If CBO_UNT > 0 Then activeSpread.SetText c1, CBO_UNT, "Unit"
  If CBO_RTE > 0 Then activeSpread.SetText c1, CBO_RTE, "Route"
  If CBO_PTH > 0 Then activeSpread.SetText c1, CBO_PTH, "Pathway"
  If CBO_ORG > 0 Then activeSpread.SetText c1, CBO_ORG, "Organ"
  If CBO_DEP > 0 Then activeSpread.SetText c1, CBO_DEP, "Deposition"
  If CBO_LOCX > 0 Then activeSpread.SetText c1, CBO_LOCX, "X"
  If CBO_LOCY > 0 Then activeSpread.SetText c1, CBO_LOCY, "Y"
  
  
  'Set column labels
  activeSpread.SetText c1, 0, "Criteria"
  activeSpread.SetText c2, 0, "Selection #" & col
  'Set column widths
  activeSpread.ColWidth(c1) = activeSpread.MaxTextColWidth(c1)
  activeSpread.ColWidth(c2) = 15
  'Set cells to selection boxes
  activeSpread.col = c2
  For i = 1 To numSel
    activeSpread.row = i
    activeSpread.CellType = 8
  Next
  'set scroll lock on series
  activeSpread.RowsFrozen = numSel + 2
  'update the allCAS constituent list
  updateConstituentList
  'update dataset ids
  cbolist = ""
  For i = 0 To numDS - 1
    If ds(i).locName = "all" Then
      lbl = "all"
    Else
      For j = 1 To UBound(modIds)
        If modIds(j) = ds(i).locName Then
          lbl = modLbls(j) + ":" + modIds(j)
        End If
      Next
    End If
    If Len(qualifier) > 0 Then
      If InStr(ds(i).locType, qualifier) > 0 Then
        adder = ""
      Else
        adder = implied
      End If
    End If

'    cbolist = cbolist + IIf(cbolist <> "", Chr(9), "") + _
'                        IIf(ds(i).locName <> "all", ds(i).locName, "") + _
'                        IIf(ds(i).locName <> "" And ds(i).locName <> "all", ":", "") + ds(i).locType
    
    If fromto Then
      cbolist = cbolist + IIf(cbolist <> "", Chr(9), "") + ds(i).locType + adder + " to " + lbl
    Else
      cbolist = cbolist + IIf(cbolist <> "", Chr(9), "") + lbl2 + " from " + ds(i).locType + adder + " " + lbl
    End If
  Next
  SetSelList CBO_DS, c2, cbolist

ErrorHandler:
  If Err.Number <> 0 Then PutError Err.Description & " in InitSelect"
End Sub

Private Sub txtIntv_Change()
Dim i As Long
  If IgnoreEvents Then Exit Sub
  resolution = val(txtIntv)
  For i = 2 To 2 * txtCount.Text Step 2
    UpdateSpread i, 1, True
  Next
End Sub

Private Sub Check1_Click()
  If IgnoreEvents Then Exit Sub
  If Check1.value <> 1 Then
    zeroes = 1
  Else
    zeroes = 0
  End If
  txtIntv_Change
End Sub

Private Sub Check2_Click()
  If IgnoreEvents Then Exit Sub
  If Check2.value = 1 Then
    includeKS = 1
  Else
    includeKS = 0
  End If
End Sub

Private Sub Updown1_Downclick()
  If Not vaSpread1.Enabled Then Exit Sub
  mConstitRowSizeLength = mConstitRowSizeLength - 1
  ReDim Preserve mConstitRowSize(mConstitRowSizeLength - 1)
  txtCount.Text = UpDown1.value
End Sub

Private Sub Updown1_Upclick()
  If Not vaSpread1.Enabled Then Exit Sub
  mConstitRowSizeLength = mConstitRowSizeLength + 1
  ReDim Preserve mConstitRowSize(mConstitRowSizeLength - 1)
  txtCount.Text = UpDown1.value
End Sub

Private Sub UpDown2_DownClick()
  txtIntv.Text = UpDown2.value
End Sub

Private Sub UpDown2_UpClick()
  txtIntv.Text = UpDown2.value
End Sub

Private Sub vaSpread1_EditChange(ByVal col As Long, ByVal row As Long)
  If Not vaSpread1.Enabled Then Exit Sub
  MousePointer = vbHourglass
  UpdateSpread col, row, True
  MousePointer = vbNormal
End Sub


