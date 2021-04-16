VERSION 5.00
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "THREED32.OCX"
Begin VB.Form TimePts 
   BorderStyle     =   3  'Fixed Dialog
   ClientHeight    =   1785
   ClientLeft      =   30
   ClientTop       =   30
   ClientWidth     =   6540
   ControlBox      =   0   'False
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   119
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   436
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.Timer Timer1 
      Interval        =   100
      Left            =   0
      Top             =   0
   End
   Begin VB.TextBox mes 
      BackColor       =   &H00C0FFFF&
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
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
      Top             =   1320
      Width           =   4920
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Done"
      Height          =   372
      Left            =   5160
      TabIndex        =   6
      Top             =   120
      Width           =   1212
   End
   Begin Threed.SSFrame SSFrame1 
      Height          =   1212
      Left            =   120
      TabIndex        =   0
      Top             =   0
      Width           =   4932
      _Version        =   65536
      _ExtentX        =   8700
      _ExtentY        =   2138
      _StockProps     =   14
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   8.23
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ShadowStyle     =   1
      Begin VB.TextBox txt 
         Alignment       =   1  'Right Justify
         BackColor       =   &H00C0FFC0&
         Height          =   312
         Index           =   1
         Left            =   2760
         TabIndex        =   5
         TabStop         =   0   'False
         Tag             =   "ntimes"
         Text            =   "40"
         Top             =   705
         Width           =   1000
      End
      Begin VB.TextBox txt 
         Alignment       =   1  'Right Justify
         BackColor       =   &H00C0FFC0&
         Height          =   312
         Index           =   0
         Left            =   2760
         TabIndex        =   2
         Tag             =   "tfinal"
         Text            =   "0.0"
         Top             =   360
         Width           =   1000
      End
      Begin VB.ComboBox unit 
         Height          =   315
         Index           =   0
         ItemData        =   "Time.frx":0000
         Left            =   3750
         List            =   "Time.frx":0002
         Style           =   2  'Dropdown List
         TabIndex        =   3
         Tag             =   "yr"
         Top             =   360
         Width           =   1000
      End
      Begin VB.Label lbl 
         Caption         =   "Set time steps - NTIMES"
         Height          =   252
         Index           =   1
         Left            =   120
         TabIndex        =   4
         Tag             =   "NTIMES"
         Top             =   756
         Width           =   2500
      End
      Begin VB.Label lbl 
         Caption         =   "Set ending time - TFINAL"
         Height          =   255
         Index           =   0
         Left            =   120
         TabIndex        =   1
         Tag             =   "TFINAL"
         Top             =   375
         Width           =   2505
      End
   End
End
Attribute VB_Name = "TimePts"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

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
    Case 0
      m = "Value must be greater than or equal to zero"
      If (tval < 0) Then er = True
    Case 1
      m = "Value must less than or equal to 5000"
      If (tval > 5000 Or tval < 1) Then er = True
  End Select
  mes = Space(100 - Len(m)) & m
  If er Then
    txt(Index).BackColor = &H8080FF
  Else
    txt(Index).BackColor = &HC0FFC0
  End If
End Function

Private Sub Command1_Click()
  TimePts.Hide
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


