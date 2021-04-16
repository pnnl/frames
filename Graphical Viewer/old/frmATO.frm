VERSION 5.00
Object = "{B02F3647-766B-11CE-AF28-C3A2FBE76A13}#2.5#0"; "ss32x25.ocx"
Object = "{FE0065C0-1B7B-11CF-9D53-00AA003C9CB6}#1.1#0"; "comct232.ocx"
Begin VB.Form frmATO 
   Caption         =   "frmATO"
   ClientHeight    =   6540
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   7815
   LinkTopic       =   "Form1"
   ScaleHeight     =   6540
   ScaleWidth      =   7815
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton Command1 
      Caption         =   "Print Tables"
      Height          =   405
      Left            =   6120
      TabIndex        =   2
      Top             =   120
      Width           =   1545
   End
   Begin VB.TextBox txtCount 
      Alignment       =   2  'Center
      Enabled         =   0   'False
      Height          =   285
      Left            =   2880
      TabIndex        =   1
      Text            =   "1"
      Top             =   240
      Width           =   300
   End
   Begin VB.CommandButton Command2 
      Caption         =   "Excel Chart"
      Height          =   405
      Left            =   4440
      TabIndex        =   0
      Top             =   120
      Width           =   1545
   End
   Begin ComCtl2.UpDown UpDown1 
      Height          =   285
      Left            =   3181
      TabIndex        =   3
      Top             =   240
      Width           =   240
      _ExtentX        =   423
      _ExtentY        =   503
      _Version        =   327681
      Value           =   1
      BuddyControl    =   "txtCount"
      BuddyDispid     =   196610
      OrigLeft        =   3360
      OrigTop         =   240
      OrigRight       =   3600
      OrigBottom      =   525
      Max             =   25
      Min             =   1
      SyncBuddy       =   -1  'True
      BuddyProperty   =   0
      Enabled         =   -1  'True
   End
   Begin FPSpreadADO.fpSpread vaSpread1 
      Height          =   5715
      Left            =   120
      TabIndex        =   4
      Top             =   720
      Width           =   7560
      _Version        =   131077
      _ExtentX        =   13335
      _ExtentY        =   10081
      _StockProps     =   64
      BackColorStyle  =   1
      ColHeaderDisplay=   0
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
      MaxCols         =   2
      RowHeaderDisplay=   0
      SelectBlockOptions=   0
      ShadowColor     =   12632064
      SpreadDesigner  =   "frmATO.frx":0000
   End
   Begin VB.Label Label1 
      AutoSize        =   -1  'True
      Caption         =   "Specify number of criteria (max 25) :"
      Height          =   195
      Left            =   120
      TabIndex        =   5
      Top             =   240
      Width           =   2520
   End
End
Attribute VB_Name = "frmATO"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text


Private Sub Command1_Click()
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

Private Sub Command2_Click()
  atoCreateChart txtCount
End Sub


Private Sub Form_Load()
  Set activeForm = Me
  'ReDim availableDatasets(0)
  caption = "Atmospheric Transport Output (ATO) Graphical Viewer"
  txtCount = 1
  InitSelect 1, vaSpread1, Me
End Sub

Private Sub Form_Resize()
  If Me.ScaleWidth = 0 Or Me.ScaleHeight = 0 Then Exit Sub
  vaSpread1.Move 400, 1000, Me.ScaleWidth - 700, Me.ScaleHeight - 1200
End Sub

Private Sub Form_Unload(Cancel As Integer)
  On Error Resume Next
  atoCloseDataset
  Close
  If Not AnError Then Kill RunName & ".ERR"
  Set wbo = Nothing
  Set cho = Nothing
  End
End Sub

Private Sub txtCount_Change()
  numcol = ChangeInCriteriaCount(txtCount)
End Sub
Private Sub UpDown1_Change()
  If Not vaSpread1.Enabled Then Exit Sub
  txtCount = UpDown1.Value
End Sub

Private Sub vaSpread1_Change(ByVal col As Long, ByVal Row As Long)
  If Not (col > 0 And col <= CInt(txtCount) * 2) Then Exit Sub
  atoSpreadsheetChange col, Row
End Sub

