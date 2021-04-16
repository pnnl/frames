VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Begin VB.Form frmColors 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Colors"
   ClientHeight    =   2850
   ClientLeft      =   30
   ClientTop       =   270
   ClientWidth     =   5160
   ControlBox      =   0   'False
   Icon            =   "Colors.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   2850
   ScaleWidth      =   5160
   ShowInTaskbar   =   0   'False
   StartUpPosition =   3  'Windows Default
   Begin VB.CommandButton Command3 
      Caption         =   "Apply"
      Height          =   300
      Left            =   1536
      TabIndex        =   13
      Top             =   2496
      Width           =   972
   End
   Begin VB.CommandButton Command2 
      Caption         =   "Cancel"
      Height          =   300
      Left            =   4032
      TabIndex        =   12
      Top             =   2496
      Width           =   972
   End
   Begin VB.CommandButton Command1 
      Caption         =   "OK"
      Height          =   300
      Left            =   2784
      TabIndex        =   11
      Top             =   2496
      Width           =   972
   End
   Begin VB.Frame Frame1 
      Height          =   2412
      Left            =   96
      TabIndex        =   0
      Top             =   0
      Width           =   5004
      Begin VB.ListBox List1 
         Height          =   1230
         Index           =   2
         ItemData        =   "Colors.frx":030A
         Left            =   192
         List            =   "Colors.frx":030C
         TabIndex        =   10
         Top             =   480
         Width           =   1932
      End
      Begin VB.CheckBox Check1 
         Caption         =   "Visible"
         Height          =   204
         Left            =   2304
         TabIndex        =   9
         Top             =   1632
         Width           =   2028
      End
      Begin VB.ListBox List1 
         Height          =   1230
         Index           =   1
         ItemData        =   "Colors.frx":030E
         Left            =   192
         List            =   "Colors.frx":0310
         TabIndex        =   8
         Top             =   480
         Visible         =   0   'False
         Width           =   1932
      End
      Begin VB.ListBox List1 
         Height          =   1230
         Index           =   0
         ItemData        =   "Colors.frx":0312
         Left            =   192
         List            =   "Colors.frx":0314
         TabIndex        =   6
         Top             =   480
         Visible         =   0   'False
         Width           =   1932
      End
      Begin VB.Label Label6 
         Caption         =   "Click on color to change"
         Height          =   204
         Left            =   2304
         TabIndex        =   7
         Top             =   480
         Width           =   2508
      End
      Begin VB.Label Label5 
         BorderStyle     =   1  'Fixed Single
         Height          =   300
         Left            =   3648
         TabIndex        =   5
         Top             =   1248
         Width           =   1104
      End
      Begin VB.Label Label4 
         BorderStyle     =   1  'Fixed Single
         Height          =   300
         Left            =   3648
         TabIndex        =   4
         Top             =   864
         Width           =   1104
      End
      Begin VB.Label Label3 
         Caption         =   "Background"
         Height          =   204
         Left            =   2304
         TabIndex        =   3
         Top             =   1248
         Width           =   1296
      End
      Begin VB.Label Label2 
         Caption         =   "Foreground"
         Height          =   204
         Left            =   2304
         TabIndex        =   2
         Top             =   864
         Width           =   1296
      End
      Begin VB.Label Label1 
         Caption         =   "Item"
         Height          =   204
         Left            =   192
         TabIndex        =   1
         Top             =   288
         Width           =   1356
      End
   End
   Begin MSComDlg.CommonDialog ColorDialog 
      Left            =   4608
      Top             =   96
      _ExtentX        =   688
      _ExtentY        =   688
      _Version        =   393216
   End
End
Attribute VB_Name = "frmColors"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Dim listclick As Boolean

Sub addcolor(Name As String, f As Long, b As Long, v As Long)
  List1(0).AddItem Name
  List1(0).ItemData(List1(0).NewIndex) = f
  List1(1).AddItem Name
  List1(1).ItemData(List1(1).NewIndex) = b
  List1(2).AddItem Name
  List1(2).ItemData(List1(2).NewIndex) = v
End Sub

Private Sub Check1_Click()
  List1(2).ItemData(List1(2).ListIndex) = Check1.value
End Sub

Private Sub Command1_Click()
  updatecolor dfcolor, dbcolor, dvis, 0
  updatecolor mfcolor, mbcolor, mvis, 1
  updatecolor sfcolor, sbcolor, svis, 2
  updatecolor wfcolor, wbcolor, wvis, 3
  SetColors
  DrawSite Sites(frmSite), pic, False
  Unload Me
End Sub

Private Sub Command2_Click()
  Unload Me
End Sub

Private Sub Command3_Click()
  updatecolor dfcolor, dbcolor, dvis, 0
  updatecolor mfcolor, mbcolor, mvis, 1
  updatecolor sfcolor, sbcolor, svis, 2
  updatecolor wfcolor, wbcolor, wvis, 3
  SetColors
  DrawSite Sites(frmSite), pic, False
End Sub

Sub Form_Load()
  addcolor "Database Connection", dfcolor, dbcolor, dvis
  addcolor "Model Connection", mfcolor, mbcolor, mvis
  addcolor "System Connection", sfcolor, sbcolor, svis
  addcolor "Workspace", wfcolor, wbcolor, wvis
  List1(2).ListIndex = 0
End Sub

Sub updatecolor(f As Long, b As Long, v As Long, i As Long)
  f = List1(0).ItemData(i)
  b = List1(1).ItemData(i)
  v = List1(2).ItemData(i)
End Sub

Private Sub Label4_Click()
  On Error Resume Next
  ColorDialog.CancelError = True
  ColorDialog.ShowColor
  If Err Then Exit Sub
  Label4.BackColor = ColorDialog.Color
  List1(0).ItemData(List1(2).ListIndex) = ColorDialog.Color
End Sub

Private Sub Label5_Click()
  On Error Resume Next
  ColorDialog.CancelError = True
  ColorDialog.ShowColor
  If Err Then Exit Sub
  Label5.BackColor = ColorDialog.Color
  List1(1).ItemData(List1(2).ListIndex) = ColorDialog.Color
End Sub

Private Sub List1_Click(Index As Integer)
  ' prevent recursion
  If listclick Then Exit Sub
  listclick = True
  
  List1(0).ListIndex = List1(2).ListIndex
  List1(1).ListIndex = List1(2).ListIndex
  Label4.BackColor = List1(0).ItemData(List1(2).ListIndex)
  Label5.BackColor = List1(1).ItemData(List1(2).ListIndex)
  Check1.value = List1(2).ItemData(List1(2).ListIndex)
  listclick = False
End Sub

Private Sub List1_DblClick(Index As Integer)
  List1_Click Index
End Sub
