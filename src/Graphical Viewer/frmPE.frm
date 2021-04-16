VERSION 5.00
Object = "{FE0065C0-1B7B-11CF-9D53-00AA003C9CB6}#1.1#0"; "comct232.ocx"
Object = "{B02F3647-766B-11CE-AF28-C3A2FBE76A13}#2.5#0"; "ss32x25.ocx"
Begin VB.Form frmPE 
   Caption         =   "Probability of Exceedence"
   ClientHeight    =   8310
   ClientLeft      =   60
   ClientTop       =   630
   ClientWidth     =   6090
   LinkTopic       =   "Form1"
   MinButton       =   0   'False
   ScaleHeight     =   8310
   ScaleWidth      =   6090
   StartUpPosition =   2  'CenterScreen
   Begin VB.Frame Frame1 
      Caption         =   "Probability of Exceedence"
      Height          =   972
      Left            =   105
      TabIndex        =   4
      Top             =   105
      Width           =   5805
      Begin VB.ComboBox Combo1 
         Height          =   315
         ItemData        =   "frmPE.frx":0000
         Left            =   3885
         List            =   "frmPE.frx":000A
         Style           =   2  'Dropdown List
         TabIndex        =   7
         Top             =   420
         Width           =   1680
      End
      Begin VB.TextBox txtIntv 
         Alignment       =   2  'Center
         Enabled         =   0   'False
         Height          =   285
         Left            =   1890
         TabIndex        =   6
         Text            =   "10"
         Top             =   420
         Width           =   420
      End
      Begin ComCtl2.UpDown UpDown2 
         CausesValidation=   0   'False
         Height          =   330
         Left            =   2310
         TabIndex        =   5
         Top             =   420
         Width           =   240
         _ExtentX        =   344
         _ExtentY        =   582
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
      Begin VB.Label Label2 
         AutoSize        =   -1  'True
         Caption         =   "Number of Intervals:"
         Height          =   195
         Left            =   210
         TabIndex        =   9
         Top             =   420
         Width           =   1440
      End
      Begin VB.Label Label1 
         AutoSize        =   -1  'True
         Caption         =   "deMinimus:"
         Height          =   195
         Left            =   2940
         TabIndex        =   8
         Top             =   420
         Width           =   840
      End
   End
   Begin VB.Frame Frame2 
      Caption         =   "Report Details"
      Height          =   7020
      Left            =   105
      TabIndex        =   0
      Top             =   1155
      Width           =   5805
      Begin VB.TextBox txtCount 
         Alignment       =   2  'Center
         Enabled         =   0   'False
         Height          =   285
         Left            =   2415
         TabIndex        =   1
         Text            =   "1"
         Top             =   420
         Width           =   420
      End
      Begin ComCtl2.UpDown UpDown1 
         Height          =   285
         Left            =   2835
         TabIndex        =   2
         Top             =   420
         Width           =   240
         _ExtentX        =   344
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
         Height          =   5880
         Left            =   240
         TabIndex        =   10
         Top             =   915
         Width           =   5340
         _Version        =   131077
         _ExtentX        =   9419
         _ExtentY        =   10372
         _StockProps     =   64
         BackColorStyle  =   1
         ColHeaderDisplay=   0
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         MaxCols         =   2
         SelectBlockOptions=   0
         SpreadDesigner  =   "frmPE.frx":002B
      End
      Begin VB.Label Label3 
         AutoSize        =   -1  'True
         Caption         =   "Number of criteria (max 25):"
         Height          =   195
         Left            =   210
         TabIndex        =   3
         Top             =   420
         Width           =   1935
      End
   End
   Begin VB.Menu mnuShow 
      Caption         =   "Show Charts"
   End
   Begin VB.Menu mnuPrint 
      Caption         =   "Print Series"
   End
End
Attribute VB_Name = "frmPE"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Private IgnoreEvents As Boolean

Private Sub form_Load()
  Dim task As String
  Dim numds As Long
  
  On Error GoTo ErrorHandler
  IgnoreEvents = True
  
  ' miscellaneous initializations
  Set activeForm = Me
  Set activeSpread = vaSpread1
  task = "ReDim Routes(0)"
  ReDim Routes(0)
  task = "ReDim exposure(0, 0)"
  ReDim Exposure(0, 0)
  txtCount = 1
  If viewType = "PE" Then
    Combo1.ListIndex = 0
    zeroes = Combo1.ItemData(0)
    resolution = 10
    txtIntv = resolution
    task = "ReDim peArray(UpDown1.max)"
    ReDim peArray(UpDown1.max)
    caption = " - Probability of Exceedence Series"
  Else
    Frame2.Top = Frame1.Top
    Frame1.Visible = False
    caption = " - Time Series"
  End If

  IgnoreEvents = False
  
  Select Case pdcfType
    Case "ato", "epf", "rif", "hif":
      Select Case pdcfType
        Case "ato"
          caption = "Air Concentration and Deposistion" & caption
          numds = atoLoadDatasets(pdcfName, ModName)
          atoInitSelect 1
        Case "epf":
          caption = "Exposure Pathway Concentrations" & caption
          numds = epfLoadDatasets
          expInitSelect 1
        Case "rif":
          caption = "Receptor Intakes" & caption
          numds = rifLoadDatasets
          expInitSelect 1
        Case "hif":
          caption = "Health Impacts" & caption
          numds = hifLoadDatasets
          expInitSelect 1
      End Select
      If AnError Or numds = 0 Then Unload Me
    Case "AFF":
      caption = "Air flux" & caption
      affOpen FUIName, ModName
      txtCount = peAFFGetAll
    Case "WCF":
      caption = "Water Concentrations" & caption
      wcfOpen FUIName, ModName
      txtCount = peWCFGetAll
    Case "WFF":
      caption = "Water Flux" & caption
      wffOpen FUIName, ModName
      txtCount = peWFFGetAll
    Case "SCF":
      caption = "Soil Concentrations" & caption
      scfOpen FUIName, ModName
      txtCount = peSCFGetAll
    Case Else:
  End Select

ErrorHandler:
  If Err.Number <> 0 Then
    MsgBox task & vbCrLf & Err.Description, vbOKOnly, "frmPE_Load"
  End If
  vaSpread1_Change 2, 2
  Form_Resize
End Sub

Private Sub Form_Unload(Cancel As Integer)
  On Error Resume Next
  Select Case pdcfType
    Case "scf":      scfClose
    Case "wcf":      wcfClose
    Case "wff":      wffClose
    Case "aff":      affClose
    Case "ato":      atoCloseDataset
    Case "epf":      epfCloseDataset
    Case "rif":      rifCloseDataset
    Case "hif":      hifCloseDataset
  End Select
  Close
  If Not AnError Then Kill RunName & ".ERR"
  Set wbo = Nothing
  Set cho = Nothing
  End
End Sub

Private Sub Form_Resize()
  If IgnoreEvents Then Exit Sub
  If Me.ScaleWidth = 0 Or Me.ScaleHeight = 0 Then Exit Sub
    Select Case pdcfType
      Case "ATO", "HIF", "EPF", "RIF":
        If viewType = "PE" Then
          If 50 < Me.ScaleHeight - (Frame1.Height + 3 * Frame1.Top) Then
            Frame2.Height = Me.ScaleHeight - (Frame1.Height + 3 * Frame1.Top)
          End If
        Else
          If 50 < Me.ScaleHeight - 2 * Frame1.Top Then
            Frame2.Height = Me.ScaleHeight - 2 * Frame1.Top
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
    End Select
End Sub

Private Sub Combo1_Click()
  If IgnoreEvents Then Exit Sub
  zeroes = Combo1.ItemData(Combo1.ListIndex)
  RecalculateProbability
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

Private Sub mnuShow_Click()
  If viewType = "PE" Then
    peCreateChart CLng(txtCount.Text)
  Else
    If pdcfType = "ato" Then
      atoCreateChart txtCount
    Else
      CreateChart
    End If
  End If
End Sub

Private Sub vaSpread1_Change(ByVal Col As Long, ByVal Row As Long)
  Select Case pdcfType
    Case "ato":
      atoSpreadsheetChange Col, Row
    Case "epf", "rif", "hif":
      mnuPrint.Enabled = expSpreadSheetChange(Col, Row)
  End Select
End Sub

Private Sub txtCount_Change()
  ChangeInCriteriaCount txtCount
End Sub

Private Sub txtIntv_Change()
  resolution = val(txtIntv)
  RecalculateProbability
End Sub

Private Sub Updown1_Downclick()
  If Not vaSpread1.Enabled Then Exit Sub
  txtCount.Text = UpDown1.Value
End Sub

Private Sub Updown1_Upclick()
  If Not vaSpread1.Enabled Then Exit Sub
  txtCount.Text = UpDown1.Value
End Sub

Private Sub UpDown2_DownClick()
  txtIntv.Text = UpDown2.Value
End Sub

Private Sub UpDown2_UpClick()
  txtIntv.Text = UpDown2.Value
End Sub
