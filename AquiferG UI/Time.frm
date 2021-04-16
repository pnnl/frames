VERSION 5.00
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "THREED32.OCX"
Object = "{B02F3647-766B-11CE-AF28-C3A2FBE76A13}#2.5#0"; "ss32x25.ocx"
Begin VB.Form Time 
   BorderStyle     =   3  'Fixed Dialog
   ClientHeight    =   5136
   ClientLeft      =   36
   ClientTop       =   36
   ClientWidth     =   6816
   ControlBox      =   0   'False
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   428
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   568
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.Timer Timer1 
      Interval        =   100
      Left            =   6360
      Top             =   600
   End
   Begin VB.TextBox mes 
      BackColor       =   &H00C0FFFF&
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.6
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   360
      Left            =   120
      Locked          =   -1  'True
      TabIndex        =   7
      TabStop         =   0   'False
      Top             =   4680
      Width           =   5280
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Done"
      Height          =   372
      Left            =   5520
      TabIndex        =   6
      Top             =   120
      Width           =   1212
   End
   Begin Threed.SSFrame SSFrame1 
      Height          =   4575
      Left            =   120
      TabIndex        =   0
      Top             =   0
      Width           =   5295
      _Version        =   65536
      _ExtentX        =   9340
      _ExtentY        =   8070
      _StockProps     =   14
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   7.8
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ShadowStyle     =   1
      Begin VB.OptionButton Option2 
         Caption         =   "User selected time points"
         Height          =   255
         Left            =   240
         TabIndex        =   13
         Top             =   1470
         Width           =   2850
      End
      Begin VB.OptionButton Option1 
         Caption         =   "Always model to choose time intervals"
         Height          =   255
         Left            =   210
         TabIndex        =   12
         Top             =   315
         Value           =   -1  'True
         Width           =   3495
      End
      Begin VB.ComboBox unit 
         Enabled         =   0   'False
         Height          =   315
         Index           =   1
         ItemData        =   "Time.frx":0000
         Left            =   4080
         List            =   "Time.frx":0002
         Style           =   2  'Dropdown List
         TabIndex        =   9
         Tag             =   "yr"
         Top             =   1800
         Width           =   1000
      End
      Begin FPSpreadADO.fpSpread vaSpread1 
         Height          =   1935
         Left            =   3480
         TabIndex        =   8
         Top             =   2400
         Width           =   1635
         _Version        =   131077
         _ExtentX        =   2794
         _ExtentY        =   3239
         _StockProps     =   64
         Enabled         =   0   'False
         AutoSize        =   -1  'True
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         MaxCols         =   1
         ScrollBarExtMode=   -1  'True
         ScrollBars      =   2
         SpreadDesigner  =   "Time.frx":0004
         UserResize      =   2
         VisibleCols     =   1
         VisibleRows     =   7
      End
      Begin VB.TextBox txt 
         BackColor       =   &H00C0FFC0&
         Enabled         =   0   'False
         Height          =   312
         Index           =   1
         Left            =   3120
         TabIndex        =   4
         TabStop         =   0   'False
         Tag             =   "ntimes"
         Text            =   "40"
         Top             =   960
         Width           =   1000
      End
      Begin VB.TextBox txt 
         BackColor       =   &H00C0FFC0&
         Height          =   312
         Index           =   0
         Left            =   3120
         TabIndex        =   2
         Tag             =   "tfinal"
         Text            =   "0.0"
         Top             =   600
         Width           =   1000
      End
      Begin VB.ComboBox unit 
         Height          =   315
         Index           =   0
         ItemData        =   "Time.frx":01AD
         Left            =   4080
         List            =   "Time.frx":01AF
         Style           =   2  'Dropdown List
         TabIndex        =   1
         Tag             =   "yr"
         Top             =   600
         Width           =   1000
      End
      Begin VB.Label Label1 
         Caption         =   "Enter the periods you wish the model to calculate flux and/or concentrations.  Time periods chould be consecutive."
         Height          =   1815
         Left            =   600
         TabIndex        =   11
         Top             =   2400
         Width           =   2535
      End
      Begin VB.Label lbl 
         Caption         =   "Time units"
         Height          =   255
         Index           =   3
         Left            =   600
         TabIndex        =   10
         Tag             =   "TFINAL"
         Top             =   1800
         Width           =   2385
      End
      Begin VB.Label lbl 
         Caption         =   "Set time steps - NTIMES"
         Height          =   255
         Index           =   1
         Left            =   480
         TabIndex        =   5
         Tag             =   "NTIMES"
         Top             =   990
         Width           =   2505
      End
      Begin VB.Label lbl 
         Caption         =   "Set ending time - TFINAL"
         Height          =   255
         Index           =   0
         Left            =   480
         TabIndex        =   3
         Tag             =   "TFINAL"
         Top             =   630
         Width           =   2505
      End
   End
End
Attribute VB_Name = "Time"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub Option1_Click()
  txt(0).Enabled = True
  unit(0).Enabled = True
  unit(1).Enabled = False
  vaSpread1.Enabled = False
End Sub

Private Sub Option2_Click()
  txt(0).Enabled = False
  unit(0).Enabled = False
  unit(1).Enabled = True
  vaSpread1.Enabled = True
End Sub

Private Sub Timer1_Timer()
  mes = Mid(mes, 2) & Mid(mes, 1, 1)
End Sub

Function er(Index As Long) As Boolean
  Dim tval As Double
  Dim t1 As String
  Dim t2 As String
  Dim m As String
  
  m = ""
  er = False
  If txt(Index).Text = "" Then er = True
  tval = Val(txt(Index).Text)
  Select Case Index
    Case 0, 1
      m = "Value must be greater than or equal to zero"
      If (tval < 0) Then er = True
    Case 2
      m = "Value must less than or equal to 40"
      If (tval > 40 Or tval < 1) Then er = True
  End Select
  mes = Space(100 - Len(m)) & m
  If er Then
    txt(Index).BackColor = &H8080FF
  Else
    txt(Index).BackColor = &HC0FFC0
  End If
End Function

Private Sub Command1_Click()
  Time.Hide
End Sub

Private Sub txt_Change(Index As Integer)
Dim chk As Double
On Error GoTo toolarge
  er CLng(Index)
  chk = CDbl(txt(Index).Text)
  Exit Sub
toolarge:
  txt(Index).BackColor = &H8080FF
End Sub

Private Sub unit_Click(Index As Integer)
  er CLng(Index)
End Sub

Private Sub unit_GotFocus(Index As Integer)
  er CLng(Index)
  HelpAnchor = txt(Index).Tag
End Sub

Private Sub txt_GotFocus(Index As Integer)
  er CLng(Index)
  HelpAnchor = txt(Index).Tag
End Sub

Private Sub form_KeyPress(KeyAscii As Integer)
  If KeyAscii = 13 Then KeyAscii = 0
End Sub

Private Sub form_KeyDown(KeyCode As Integer, Shift As Integer)
  Select Case KeyCode
  Case vbKeyF1:
    KeyCode = 0
    GetHelp
  Case vbKeyUp:
    KeyCode = 0
    SendKeys "+{TAB}"
  Case vbKeyDown:
    KeyCode = 0
    SendKeys "{TAB}"
  Case vbKeyReturn:
    KeyCode = 0
    SendKeys "{TAB}"
  End Select
End Sub


