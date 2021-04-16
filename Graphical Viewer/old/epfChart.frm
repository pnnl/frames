VERSION 5.00
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "Threed32.ocx"
Object = "{B02F3647-766B-11CE-AF28-C3A2FBE76A13}#2.5#0"; "ss32x25.ocx"
Begin VB.Form frmSel 
   Caption         =   "Exposure Pathway Concentrations Chart Viewer"
   ClientHeight    =   5460
   ClientLeft      =   4005
   ClientTop       =   840
   ClientWidth     =   8070
   ControlBox      =   0   'False
   BeginProperty Font 
      Name            =   "MS Sans Serif"
      Size            =   8.25
      Charset         =   0
      Weight          =   700
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "epfChart.frx":0000
   LinkTopic       =   "Form2"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   5460
   ScaleWidth      =   8070
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton Command2 
      Caption         =   "Close"
      Height          =   288
      Left            =   6336
      TabIndex        =   4
      Top             =   4740
      Width           =   1020
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Chart"
      Default         =   -1  'True
      Height          =   288
      Left            =   5172
      TabIndex        =   3
      Top             =   4740
      Width           =   1020
   End
   Begin Threed.SSPanel panCount 
      Height          =   285
      Left            =   3900
      TabIndex        =   2
      Top             =   4770
      Width           =   645
      _Version        =   65536
      _ExtentX        =   1143
      _ExtentY        =   508
      _StockProps     =   15
      BackColor       =   12632256
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
   End
   Begin Threed.SSPanel panSel 
      Height          =   3870
      Left            =   120
      TabIndex        =   5
      Top             =   735
      Width           =   7890
      _Version        =   65536
      _ExtentX        =   13928
      _ExtentY        =   6816
      _StockProps     =   15
      BackColor       =   12632256
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Begin FPSpreadADO.fpSpread sprSel 
         Height          =   3708
         Left            =   72
         TabIndex        =   6
         Top             =   72
         Width           =   7752
         _Version        =   131077
         _ExtentX        =   13674
         _ExtentY        =   6541
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
         GridShowHoriz   =   0   'False
         GridShowVert    =   0   'False
         GridSolid       =   0   'False
         MaxCols         =   4
         MaxRows         =   30
         NoBorder        =   -1  'True
         RowsFrozen      =   1
         ScrollBars      =   2
         SelectBlockOptions=   0
         ShadowColor     =   16776960
         SpreadDesigner  =   "epfChart.frx":030A
         UserResize      =   1
         VisibleRows     =   16
      End
   End
   Begin VB.Label Label3 
      Alignment       =   2  'Center
      Caption         =   "Note: All charts for a single constituent will appear on the same sheet."
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   276
      Left            =   120
      TabIndex        =   7
      Top             =   5112
      Width           =   7824
   End
   Begin VB.Label Label2 
      Alignment       =   1  'Right Justify
      Caption         =   "Potential Number of Charts Selected :"
      Height          =   240
      Left            =   228
      TabIndex        =   1
      Top             =   4812
      Width           =   3672
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      Caption         =   "Select Items to Chart"
      Height          =   228
      Left            =   144
      TabIndex        =   0
      Top             =   396
      Width           =   7788
   End
End
Attribute VB_Name = "frmSel"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub Command1_Click()
  If 0 < panCount.Caption Then
    MousePointer = vbHourglass
    epfChart
    MousePointer = vbDefault
  End If
End Sub

Private Sub Command2_Click()
' close_csv errfile
  If 0 < CInt(panCount.Caption) Then
    epfWriteSelection
' Else
'   Kill errfile.fname
  End If
  On Error Resume Next
  Kill RunName & ".ERR"
  Set wbo = Nothing
  Set cho = Nothing
  End
End Sub

Private Sub Form_Activate()
  CountCharts
End Sub

Private Sub form_Initialize()
  panSel.BevelWidth = 0
End Sub

Private Sub form_Load()
  Top = 0.5 * (Screen.Height - Height)
  Left = 0.5 * (Screen.Width - Width)
End Sub

Private Sub Form_Unload(Cancel As Integer)
  On Error Resume Next
  Close
  If Not AnError Then Kill RunName & ".ERR"
  Set wbo = Nothing
  Set cho = Nothing
  End

End Sub

Private Sub sprSel_ButtonClicked(ByVal col As Long, ByVal row As Long, ByVal ButtonDown As Integer)
  epfCBclick col, row, ButtonDown
End Sub


