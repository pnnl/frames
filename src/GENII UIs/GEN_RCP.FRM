VERSION 5.00
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "THREED32.OCX"
Begin VB.Form gen_rcp 
   BorderStyle     =   1  'Fixed Single
   ClientHeight    =   5730
   ClientLeft      =   3840
   ClientTop       =   2685
   ClientWidth     =   7680
   Icon            =   "GEN_RCP.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   382
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   512
   StartUpPosition =   2  'CenterScreen
   Begin Threed.SSFrame SSFrame3 
      Height          =   612
      Left            =   120
      TabIndex        =   40
      Top             =   0
      Width           =   3612
      _Version        =   65536
      _ExtentX        =   6371
      _ExtentY        =   1080
      _StockProps     =   14
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ShadowStyle     =   1
      Begin VB.ComboBox NAGES 
         Height          =   288
         ItemData        =   "GEN_RCP.frx":08CA
         Left            =   2520
         List            =   "GEN_RCP.frx":08E0
         Style           =   2  'Dropdown List
         TabIndex        =   41
         Top             =   240
         Width           =   852
      End
      Begin VB.Label Label3 
         Caption         =   "Number of age groups"
         Height          =   252
         Left            =   240
         TabIndex        =   42
         Top             =   240
         Width           =   2292
      End
   End
   Begin Threed.SSFrame SSFrame1 
      Height          =   5052
      Left            =   120
      TabIndex        =   0
      Top             =   600
      Visible         =   0   'False
      Width           =   3612
      _Version        =   65536
      _ExtentX        =   6371
      _ExtentY        =   8911
      _StockProps     =   14
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ShadowStyle     =   1
      Begin VB.ComboBox vunt 
         Height          =   288
         Index           =   1
         Left            =   1440
         Style           =   2  'Dropdown List
         TabIndex        =   51
         Top             =   1440
         Width           =   1000
      End
      Begin VB.TextBox vtxt 
         Height          =   288
         Index           =   1
         Left            =   480
         TabIndex        =   48
         Top             =   1440
         Width           =   1000
      End
      Begin VB.ComboBox vunt 
         Height          =   288
         Index           =   0
         Left            =   1440
         Style           =   2  'Dropdown List
         TabIndex        =   43
         Top             =   840
         Width           =   1000
      End
      Begin VB.TextBox vtxt 
         Height          =   288
         Index           =   0
         Left            =   480
         TabIndex        =   39
         Top             =   840
         Width           =   1000
      End
      Begin VB.ComboBox Combo1 
         Height          =   288
         Left            =   2520
         TabIndex        =   4
         Text            =   "Combo1"
         Top             =   240
         Width           =   852
      End
      Begin VB.ListBox p 
         Height          =   2400
         ItemData        =   "GEN_RCP.frx":08F6
         Left            =   360
         List            =   "GEN_RCP.frx":0927
         TabIndex        =   1
         Top             =   2040
         Width           =   3012
      End
      Begin VB.Label vlbl 
         Caption         =   "lbl"
         Height          =   252
         Index           =   1
         Left            =   240
         TabIndex        =   50
         Top             =   1200
         Width           =   1692
      End
      Begin VB.Label vref 
         Caption         =   "Ref: 0"
         Height          =   252
         Index           =   1
         Left            =   2520
         TabIndex        =   49
         Top             =   1440
         Width           =   996
      End
      Begin VB.Label vref 
         Caption         =   "Ref: 0"
         Height          =   252
         Index           =   0
         Left            =   2520
         TabIndex        =   44
         Top             =   840
         Width           =   996
      End
      Begin VB.Label vlbl 
         Caption         =   "lbl"
         Height          =   252
         Index           =   0
         Left            =   240
         TabIndex        =   38
         Top             =   600
         Width           =   1692
      End
      Begin VB.Label Label1 
         Caption         =   "Age group selection"
         Height          =   252
         Left            =   240
         TabIndex        =   3
         Top             =   240
         Width           =   2292
      End
      Begin VB.Label Label2 
         Caption         =   "Pathway selection"
         Height          =   252
         Left            =   240
         TabIndex        =   2
         Top             =   1800
         Width           =   3132
      End
   End
   Begin Threed.SSFrame SSFrame2 
      Height          =   5652
      Left            =   3960
      TabIndex        =   5
      Top             =   0
      Visible         =   0   'False
      Width           =   3612
      _Version        =   65536
      _ExtentX        =   6371
      _ExtentY        =   9970
      _StockProps     =   14
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ShadowStyle     =   1
      Begin VB.CheckBox chk 
         Caption         =   "Check1"
         Height          =   285
         Index           =   0
         Left            =   240
         TabIndex        =   52
         Top             =   2880
         Width           =   2775
      End
      Begin VB.TextBox rtxt 
         Height          =   288
         Left            =   480
         TabIndex        =   47
         Text            =   "txt"
         Top             =   4920
         Width           =   972
      End
      Begin VB.ComboBox runt 
         Height          =   288
         Left            =   1440
         Style           =   2  'Dropdown List
         TabIndex        =   46
         Top             =   4920
         Width           =   1092
      End
      Begin VB.ComboBox cb 
         Height          =   315
         Left            =   1440
         Style           =   2  'Dropdown List
         TabIndex        =   45
         Top             =   4320
         Visible         =   0   'False
         Width           =   1095
      End
      Begin VB.ComboBox unt 
         Height          =   288
         Index           =   7
         Left            =   1440
         Style           =   2  'Dropdown List
         TabIndex        =   36
         Top             =   4920
         Width           =   1092
      End
      Begin VB.TextBox txt 
         Height          =   288
         Index           =   7
         Left            =   480
         TabIndex        =   35
         Text            =   "txt"
         Top             =   4920
         Width           =   972
      End
      Begin VB.ComboBox unt 
         Height          =   288
         Index           =   6
         Left            =   1440
         Style           =   2  'Dropdown List
         TabIndex        =   32
         Top             =   4320
         Width           =   1092
      End
      Begin VB.TextBox txt 
         Height          =   288
         Index           =   6
         Left            =   480
         TabIndex        =   31
         Text            =   "txt"
         Top             =   4320
         Width           =   972
      End
      Begin VB.ComboBox unt 
         Height          =   288
         Index           =   5
         ItemData        =   "GEN_RCP.frx":0AC0
         Left            =   1440
         List            =   "GEN_RCP.frx":0AC2
         Style           =   2  'Dropdown List
         TabIndex        =   28
         Top             =   3720
         Width           =   1092
      End
      Begin VB.TextBox txt 
         Height          =   288
         Index           =   5
         Left            =   480
         TabIndex        =   27
         Text            =   "txt"
         Top             =   3720
         Width           =   972
      End
      Begin VB.ComboBox unt 
         Height          =   288
         Index           =   4
         Left            =   1440
         Style           =   2  'Dropdown List
         TabIndex        =   24
         Top             =   3120
         Width           =   1092
      End
      Begin VB.TextBox txt 
         Height          =   288
         Index           =   4
         Left            =   480
         TabIndex        =   23
         Text            =   "txt"
         Top             =   3120
         Width           =   972
      End
      Begin VB.ComboBox unt 
         Height          =   288
         Index           =   3
         Left            =   1440
         Style           =   2  'Dropdown List
         TabIndex        =   20
         Top             =   2520
         Width           =   1092
      End
      Begin VB.TextBox txt 
         Height          =   288
         Index           =   3
         Left            =   480
         TabIndex        =   19
         Text            =   "txt"
         Top             =   2520
         Width           =   972
      End
      Begin VB.ComboBox unt 
         Height          =   288
         Index           =   2
         Left            =   1440
         Style           =   2  'Dropdown List
         TabIndex        =   16
         Top             =   1920
         Width           =   1092
      End
      Begin VB.TextBox txt 
         Height          =   288
         Index           =   2
         Left            =   480
         TabIndex        =   15
         Text            =   "txt"
         Top             =   1920
         Width           =   972
      End
      Begin VB.ComboBox unt 
         Height          =   288
         Index           =   1
         Left            =   1440
         Style           =   2  'Dropdown List
         TabIndex        =   12
         Top             =   1320
         Width           =   1092
      End
      Begin VB.TextBox txt 
         Height          =   288
         Index           =   1
         Left            =   480
         TabIndex        =   11
         Text            =   "txt"
         Top             =   1320
         Width           =   972
      End
      Begin VB.ComboBox unt 
         Height          =   288
         Index           =   0
         Left            =   1440
         Style           =   2  'Dropdown List
         TabIndex        =   8
         Top             =   720
         Width           =   1092
      End
      Begin VB.TextBox txt 
         Height          =   288
         Index           =   0
         Left            =   480
         TabIndex        =   7
         Text            =   "txt"
         Top             =   720
         Width           =   972
      End
      Begin VB.Label ref 
         Caption         =   "Ref: 0"
         Height          =   252
         Index           =   7
         Left            =   2640
         TabIndex        =   37
         Top             =   4920
         Width           =   852
      End
      Begin VB.Label lbl 
         Caption         =   "lbl"
         Height          =   252
         Index           =   7
         Left            =   120
         TabIndex        =   34
         Top             =   4680
         Width           =   3372
      End
      Begin VB.Label ref 
         Caption         =   "Ref: 0"
         Height          =   252
         Index           =   6
         Left            =   2640
         TabIndex        =   33
         Top             =   4320
         Width           =   852
      End
      Begin VB.Label lbl 
         Caption         =   "lbl"
         Height          =   252
         Index           =   6
         Left            =   120
         TabIndex        =   30
         Top             =   4080
         Width           =   3372
      End
      Begin VB.Label ref 
         Caption         =   "Ref: 0"
         Height          =   252
         Index           =   5
         Left            =   2640
         TabIndex        =   29
         Top             =   3720
         Width           =   852
      End
      Begin VB.Label lbl 
         Caption         =   "lbl"
         Height          =   252
         Index           =   5
         Left            =   120
         TabIndex        =   26
         Top             =   3480
         Width           =   3372
      End
      Begin VB.Label ref 
         Caption         =   "Ref: 0"
         Height          =   252
         Index           =   4
         Left            =   2640
         TabIndex        =   25
         Top             =   3120
         Width           =   852
      End
      Begin VB.Label lbl 
         Caption         =   "lbl"
         Height          =   252
         Index           =   4
         Left            =   120
         TabIndex        =   22
         Top             =   2880
         Width           =   3372
      End
      Begin VB.Label ref 
         Caption         =   "Ref: 0"
         Height          =   252
         Index           =   3
         Left            =   2640
         TabIndex        =   21
         Top             =   2520
         Width           =   852
      End
      Begin VB.Label lbl 
         Caption         =   "lbl"
         Height          =   252
         Index           =   3
         Left            =   120
         TabIndex        =   18
         Top             =   2280
         Width           =   3372
      End
      Begin VB.Label ref 
         Caption         =   "Ref: 0"
         Height          =   252
         Index           =   2
         Left            =   2640
         TabIndex        =   17
         Top             =   1920
         Width           =   852
      End
      Begin VB.Label lbl 
         Caption         =   "lbl"
         Height          =   252
         Index           =   2
         Left            =   120
         TabIndex        =   14
         Top             =   1680
         Width           =   3372
      End
      Begin VB.Label ref 
         Caption         =   "Ref: 0"
         Height          =   252
         Index           =   1
         Left            =   2640
         TabIndex        =   13
         Top             =   1320
         Width           =   852
      End
      Begin VB.Label lbl 
         Caption         =   "lbl"
         Height          =   252
         Index           =   1
         Left            =   120
         TabIndex        =   10
         Top             =   1080
         Width           =   3372
      End
      Begin VB.Label ref 
         Caption         =   "Ref: 0"
         Height          =   252
         Index           =   0
         Left            =   2640
         TabIndex        =   9
         Top             =   720
         Width           =   852
      End
      Begin VB.Label lbl 
         Caption         =   "lbl"
         Height          =   252
         Index           =   0
         Left            =   120
         TabIndex        =   6
         Top             =   480
         Width           =   3372
      End
   End
   Begin VB.Menu file 
      Caption         =   "&File"
      Begin VB.Menu save 
         Caption         =   "&Save and Exit"
      End
      Begin VB.Menu leave 
         Caption         =   "&Exit"
      End
   End
   Begin VB.Menu def 
      Caption         =   "&Defaults"
      Begin VB.Menu mi 
         Caption         =   "&Maximum Individual"
         Checked         =   -1  'True
      End
      Begin VB.Menu ai 
         Caption         =   "&Population Average"
      End
      Begin VB.Menu ud 
         Caption         =   "&User Defined"
      End
   End
   Begin VB.Menu mnuRef 
      Caption         =   "&Reference"
      Enabled         =   0   'False
      Begin VB.Menu addref 
         Caption         =   "&Add"
      End
      Begin VB.Menu selref 
         Caption         =   "Se&lect"
      End
   End
   Begin VB.Menu hlp 
      Caption         =   "&Help"
      Begin VB.Menu howto 
         Caption         =   "&General"
      End
      Begin VB.Menu contxt 
         Caption         =   "&Context"
         Shortcut        =   {F1}
      End
      Begin VB.Menu rng 
         Caption         =   "&Range"
         Shortcut        =   {F2}
      End
   End
End
Attribute VB_Name = "gen_rcp"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Dim uiobj As Object
Dim gn_sl(1) As New Selects
Dim gn_s(9) As New Float
Dim gn_t(6, 54) As New Float
Dim gn_d(6, 11) As New Float
Dim gn_l(6, 1) As New Logical
Dim loadng As Boolean
Dim rfs As Object
Dim pathway(6, 19) As New Collection
Dim f_numcon As Long, con() As parent
Dim m_numcon As Long, rads() As radparent
Dim ctext3 As String
Dim oldp As Long
Dim haveglyph As Boolean
Dim agecnt As Long

Private Sub WriteDesLabels(fl As parmfile)
Dim parm As parmrec
   set_parm parm, "ANFLabel", 1, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "meat animal"
   write_parmrec fle, parm
   set_parm parm, "ANFLabel", 2, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "poultry animal"
   write_parmrec fle, parm
   set_parm parm, "ANFLabel", 3, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "milk animal"
   write_parmrec fle, parm
   set_parm parm, "ANFLabel", 4, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "egg animal"
   write_parmrec fle, parm
   set_parm parm, "TFDLabel", 1, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "leafy vegetables"
   write_parmrec fle, parm
   set_parm parm, "TFDLabel", 2, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "root vegetables"
   write_parmrec fle, parm
   set_parm parm, "TFDLabel", 3, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "fruits"
   write_parmrec fle, parm
   set_parm parm, "TFDLabel", 4, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "grains"
   write_parmrec fle, parm
   set_parm parm, "AQFLabel", 1, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "fish"
   write_parmrec fle, parm
   set_parm parm, "AQFLabel", 2, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "mollusks"
   write_parmrec fle, parm
   set_parm parm, "AQFLabel", 3, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "crustacea"
   write_parmrec fle, parm
   set_parm parm, "AQFLabel", 4, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "aquatic plants"
   write_parmrec fle, parm
End Sub

Private Sub Combo1_GotFocus()
  mnuRef.Enabled = False
End Sub

Private Sub ai_Click()
  Loading.Show
  loaddef 2
  ai.Checked = True
  mi.Checked = False
  ud.Checked = False
  Combo1_Click
  Unload Loading
End Sub

Private Sub Form_Unload(Cancel As Integer)
 leave_Click
End Sub

Private Sub mi_Click()
  Loading.Show
  loaddef 1
  mi.Checked = True
  ai.Checked = False
  ud.Checked = False
  Combo1_Click
  Unload Loading
End Sub

Private Sub howto_Click()
  GetHelp
End Sub

Private Sub contxt_Click()
  GetHelp
End Sub

Private Sub NAGES_GotFocus()
  mnuRef.Enabled = False
End Sub

Private Sub p_GotFocus()
  mnuRef.Enabled = False
End Sub

Private Sub rng_Click()
  uiobj.Help
End Sub

Private Sub addref_Click()
  RefMode = 1
  GetRef rfs
  If RefIdx >= 0 Then pathway(Combo1.ListIndex + 1, p.ListIndex).item(rfs.index + 1).refnum = RefIdx
End Sub

Private Sub selref_Click()
  RefMode = 0
  GetRef rfs
  If RefIdx >= 0 Then pathway(Combo1.ListIndex + 1, p.ListIndex).item(rfs.index + 1).refnum = RefIdx
End Sub

Private Sub NAGES_Click()
  Dim i As Long
  If Val(NAGES.Text) > 0 Then
    SSFrame1.Visible = True
    SSFrame2.Visible = True
    Combo1.Clear
    For i = 1 To Val(NAGES.Text)
      Combo1.AddItem i
    Next
    Combo1.ListIndex = 0
  End If
End Sub

Private Sub Combo1_Click()
  If p.ListIndex = -1 Then p.ListIndex = 0
  p_Click
End Sub

Sub SetVisible(x As Long, v As Boolean)
  lbl(x).Visible = v
  txt(x).Visible = v
  unt(x).Visible = v
  ref(x).Visible = v
End Sub

Sub SetrVisible(v As Boolean)
  rtxt.Visible = v
  runt.Visible = v
End Sub

Private Sub p_Click()
Dim i As Long
Dim j As Long
Dim k As Long
  
  SSFrame2.Caption = p.list(p.ListIndex)
  For i = 0 To 7
    SetVisible i, False
  Next
'  If p.ListIndex = 15 Or p.ListIndex = 16 Then
'    ctext3 = ""
'    gn_s(8).Refresh
'    gn_sl(0).Refresh
'    gn_sl(0).Visible = True
'    If cb.ListIndex = -1 Then cb.ListIndex = 0
'    SetrVisible True
'  ElseIf p.ListIndex = 17 Or p.ListIndex = 18 Then
'    ctext3 = ""
'    gn_s(9).Refresh
'    gn_sl(1).Refresh
'    gn_sl(1).Visible = True
'    If cb.ListIndex = -1 Then cb.ListIndex = 0
'    SetrVisible True
'
' Inserted by BAN 10/10/01
'
  If p.ListIndex = 14 Then
'     pathway(Combo1.ListIndex + 1, 14).item(5).StatusListeners pathway(Combo1.ListIndex + 1, 14).item(5).Value
 '    chk(0).Visible = True
      chk(0).Visible = False
  Else
     chk(0).Visible = False
     gn_sl(1).Visible = False
     gn_sl(1).Visible = False
     SetrVisible False
  End If
  
  If Not loadng Then
    j = Combo1.ListIndex + 1
    k = p.ListIndex
    For i = 1 To pathway(j, k).Count
      pathway(j, k).item(i).Refresh
      pathway(j, k).item(i).Visible = True
    Next
    gn_t(j, 52).Refresh
    gn_t(j, 53).Refresh
  End If
End Sub

Private Sub Form_load()
Dim i As Long
Dim j As Long
 
  GetArguments
  If argc >= 5 Then
    Loading.Show
    loadng = True
    FUIName = argv(0) & ".GID"
    RunName = argv(1)
    siteIdx = Val(argv(2))
    modIdx = Val(argv(3))
    modName = argv(4)
    SetForm Me
    SetRefFile ReplaceExt(FUIName, "ref")
    SetHelpFile App.Path + "\genrcphp.htm"
    contxt.Enabled = HelpAvailable
    rng.Enabled = HelpAvailable
    gen_rcp.Caption = "GENII Intake Module - " + modName
    
    If open_csv(errfile, RunName & ".ERR", 1) Then
      put_val errfile, "Error report for Receptor Model"
      put_line errfile
    Else
      MsgBox "Unable to create file " & RunName & ".ERR" & Chr(10) & "Check directory permessions"
      End
    End If
  
  loadng = True
  load_convert

'    pathway(i, 15).Add gn_sl(0):    pathway(i, 17).Add gn_sl(1)
'    pathway(i, 16).Add gn_sl(0):    pathway(i, 18).Add gn_sl(1)
  gn_sl(0).Init "PERMK", "Radionuclide skin permeability constant", lbl(6), cb, ref(6)
  gn_sl(1).Init "ABSFAC", "Fraction radionuclide absorbed during event", lbl(6), cb, ref(6)
  
  gn_s(8).Init "PERM", "Radionuclide skin permeability constant", lbl(6), rtxt, runt, ref(6), "cm/hr", 0, 10, 0.001
  gn_s(9).Init "ABSF", "Fraction radionuclide absorbed during event", lbl(6), rtxt, runt, ref(6), "fraction", 0, 1, 0.001
  

  gn_s(0).Init "SHIN", "Indoor shielding factor", lbl(0), txt(0), unt(0), ref(0), "none", 0, 1, 1#
  gn_s(1).Init "SHOUT", "Outdoor shielding factor", lbl(1), txt(1), unt(1), ref(1), "none", 0, 1, 1#
  gn_s(2).Init "SFBOAT", "Shielding factor", lbl(0), txt(0), unt(0), ref(0), "none", 0, 1, 1#
  gn_s(3).Init "SWFAC", "Shoreline width factor", lbl(3), txt(3), unt(3), ref(3), "none", 0, 1, 0.2
  
  For i = 1 To 6
    pathway(i, 1).Add gn_s(0)
    pathway(i, 1).Add gn_s(1)
    pathway(i, 3).Add gn_s(2)
    
    gn_d(i, 0).Init "EVSWIM", "Frequency of swimming event", lbl(0), txt(0), unt(0), ref(0), "evt/day", 0, 10, 1, i
    gn_d(i, 1).Init "TESWIM", "Duration of swimming event", lbl(1), txt(1), unt(1), ref(1), "hr", 0, 24, 2, i
    gn_d(i, 2).Init "TSWIM", "Swimming days", lbl(2), txt(2), unt(2), ref(2), "day", 0, 365, 50, i
    gn_t(i, 35).Init "USWIM", "Ingestion rate of water while swimming", lbl(3), txt(3), unt(3), ref(3), "L/hr", 0, 10, 0.02, i
'    gn_t(i, 47).Init "ASKINSW", "Area of skin exposed while swimming", lbl(4), txt(4), unt(4), ref(4), "cm^2", 0, 50000, 20000, i
    
    pathway(i, 2).Add gn_d(i, 0):     pathway(i, 2).Add gn_d(i, 1):     pathway(i, 2).Add gn_d(i, 2)
    pathway(i, 9).Add gn_d(i, 0):     pathway(i, 9).Add gn_d(i, 1):     pathway(i, 9).Add gn_d(i, 2)
'    pathway(i, 16).Add gn_d(i, 0):    pathway(i, 16).Add gn_d(i, 1):    pathway(i, 16).Add gn_d(i, 2)
    pathway(i, 2).Add gn_t(i, 35)
    pathway(i, 9).Add gn_t(i, 35)
'    pathway(i, 16).Add gn_t(i, 35)
'    pathway(i, 2).Add gn_t(i, 47)
'    pathway(i, 9).Add gn_t(i, 47)
'    pathway(i, 16).Add gn_t(i, 47)
    
    gn_d(i, 3).Init "EVSHOR", "Frequency of shoreline use", lbl(0), txt(0), unt(0), ref(0), "evt/day", 0, 10, 1, i
    gn_d(i, 4).Init "TESHOR", "Duration of shoreline use events", lbl(1), txt(1), unt(1), ref(1), "hr", 0, 24, 5, i
    gn_d(i, 5).Init "TSHOR", "Shoreline days", lbl(2), txt(2), unt(2), ref(2), "day", 0, 365, 100, i
'    gn_t(i, 50).Init "ADHSED", "Shoreline sediment contact adherance factor", lbl(4), txt(4), unt(4), ref(4), "mg/cm^2", 0, 10, 0.5, i
'    gn_t(i, 51).Init "ASKINSD", "Area of skin exposed to shoreline sediment", lbl(5), txt(5), unt(5), ref(5), "cm^2", 0, 50000, 5000, i
    pathway(i, 4).Add gn_d(i, 3)
    pathway(i, 4).Add gn_d(i, 4)
    pathway(i, 4).Add gn_d(i, 5)
    pathway(i, 4).Add gn_s(3)
'    pathway(i, 4).Add gn_t(i, 50)
'    pathway(i, 4).Add gn_t(i, 51)
'    pathway(i, 18).Add gn_d(i, 3)
'    pathway(i, 18).Add gn_d(i, 4)
'    pathway(i, 18).Add gn_d(i, 5)
'    pathway(i, 18).Add gn_s(3)
'    pathway(i, 18).Add gn_t(i, 50)
'    pathway(i, 18).Add gn_t(i, 51)
    
    gn_d(i, 6).Init "EVSHWR", "Freqency of showering event", lbl(0), txt(0), unt(0), ref(0), "evt/day", 0, 10, 1, i
    gn_d(i, 7).Init "TESHWR", "Duration of showering event", lbl(1), txt(1), unt(1), ref(1), "hr", 0, 24, 0.167, i
    gn_d(i, 8).Init "TSHWR", "Showering days", lbl(2), txt(2), unt(2), ref(2), "day", 0, 365, 365, i
    pathway(i, 10).Add gn_d(i, 6):    pathway(i, 10).Add gn_d(i, 7):    pathway(i, 10).Add gn_d(i, 8)
    pathway(i, 15).Add gn_d(i, 6):    pathway(i, 15).Add gn_d(i, 7):    pathway(i, 15).Add gn_d(i, 8)
    
    
    gn_d(i, 11).Init "TSOIL", "Soil contact days", lbl(0), txt(0), unt(0), ref(0), "day", 0, 365, 365, i
    gn_t(i, 37).Init "USOIL", "Inadvertent soil ingestion rate", lbl(1), txt(1), unt(1), ref(1), "mg/day", 0, 15000, 100, i
    pathway(i, 11).Add gn_d(i, 11)
    pathway(i, 11).Add gn_t(i, 37)
    
    
    gn_t(i, 0).Init "UEXAIR", "Daily plume immersion exposure time", lbl(0), txt(0), unt(0), ref(0), "hr", 0, 24, 24, i
    gn_t(i, 1).Init "TEXAIR", "Yearly plume immersion exposure time", lbl(1), txt(1), unt(1), ref(1), "day", 0, 365, 365, i
    pathway(i, 0).Add gn_t(i, 0):    pathway(i, 0).Add gn_t(i, 1)
    
    gn_t(i, 2).Init "UEXGRD", "Daily external ground exposure time", lbl(2), txt(2), unt(2), ref(2), "hr", 0, 24, 24, i
    gn_t(i, 3).Init "TEXGRD", "Yearly external ground exposure time", lbl(3), txt(3), unt(3), ref(3), "day", 0, 365, 182.5, i
    gn_t(i, 4).Init "FTIN", "Fraction of time spent indoors", lbl(4), txt(4), unt(4), ref(4), "fraction", 0, 1, 0.7, i
    gn_t(i, 5).Init "FTOUT", "Fraction of time spent outdoors", lbl(5), txt(5), unt(5), ref(5), "fraction", 0, 1, 0.3, i
    pathway(i, 1).Add gn_t(i, 2):    pathway(i, 1).Add gn_t(i, 3)
    pathway(i, 1).Add gn_t(i, 4):    pathway(i, 1).Add gn_t(i, 5)
    
    gn_t(i, 6).Init "EVBOAT", "Frequency of boating event", lbl(1), txt(1), unt(1), ref(1), "evt/day", 0, 10, 1, i
    gn_t(i, 7).Init "TEBOAT", "Duration of boating event", lbl(2), txt(2), unt(2), ref(2), "hr", 0, 24, 2, i
    gn_t(i, 8).Init "TBOAT", "Boating days", lbl(3), txt(3), unt(3), ref(3), "day", 0, 365, 50, i
    pathway(i, 3).Add gn_t(i, 6):    pathway(i, 3).Add gn_t(i, 7)
    pathway(i, 3).Add gn_t(i, 8)
    
    gn_t(i, 9).Init "UCRP", "Leaf vegetables consumption rate", lbl(0), txt(0), unt(0), ref(0), "kg/day", 0, 10, 0.049, i, 1
    gn_t(i, 10).Init "TCRP", "Leafy vegetables consumption period", lbl(1), txt(1), unt(1), ref(1), "day/yr", 0, 365, 365, i, 1
    gn_t(i, 11).Init "UCRP", "Root vegetables consumption rate", lbl(2), txt(2), unt(2), ref(2), "kg/day", 0, 10, 0.482, i, 2
    gn_t(i, 12).Init "TCRP", "Root vegetables consumption period", lbl(3), txt(3), unt(3), ref(3), "day/yr", 0, 365, 365, i, 2
    gn_t(i, 13).Init "UCRP", "Fruit consumption rate", lbl(4), txt(4), unt(4), ref(4), "kg/day", 0, 10, 0.903, i, 3
    gn_t(i, 14).Init "TCRP", "Fruit consumption period", lbl(5), txt(5), unt(5), ref(5), "day/yr", 0, 365, 365, i, 3
    gn_t(i, 15).Init "UCRP", "Grain consumption rate", lbl(6), txt(6), unt(6), ref(6), "kg/day", 0, 10, 0.219, i, 4
    gn_t(i, 16).Init "TCRP", "Grain consumption period", lbl(7), txt(7), unt(7), ref(7), "day/yr", 0, 365, 365, i, 4
    pathway(i, 5).Add gn_t(i, 9):     pathway(i, 5).Add gn_t(i, 10)
    pathway(i, 5).Add gn_t(i, 11):    pathway(i, 5).Add gn_t(i, 12)
    pathway(i, 5).Add gn_t(i, 13):    pathway(i, 5).Add gn_t(i, 14)
    pathway(i, 5).Add gn_t(i, 15):    pathway(i, 5).Add gn_t(i, 16)
    
    gn_t(i, 17).Init "UANM", "Meat consumption rate", lbl(0), txt(0), unt(0), ref(0), "kg/day", 0, 10, 0.233, i, 1
    gn_t(i, 18).Init "TANM", "Meat consumption period", lbl(1), txt(1), unt(1), ref(1), "day/yr", 0, 365, 365, i, 1
    gn_t(i, 19).Init "UANM", "Poultry consumption rate", lbl(2), txt(2), unt(2), ref(2), "kg/day", 0, 10, 0.049, i, 2
    gn_t(i, 20).Init "TANM", "Poultry consumption period", lbl(3), txt(3), unt(3), ref(3), "day/yr", 0, 365, 365, i, 2
    gn_t(i, 21).Init "UANM", "Milk consumption rate", lbl(4), txt(4), unt(4), ref(4), "kg/day", 0, 10, 0.307, i, 3
    gn_t(i, 22).Init "TANM", "Milk consumption period", lbl(5), txt(5), unt(5), ref(5), "day/yr", 0, 365, 365, i, 3
    gn_t(i, 23).Init "UANM", "Egg consumption rate", lbl(6), txt(6), unt(6), ref(6), "kg/day", 0, 10, 0.032, i, 4
    gn_t(i, 24).Init "TANM", "Egg consumption period", lbl(7), txt(7), unt(7), ref(7), "day/yr", 0, 365, 365, i, 4
    pathway(i, 6).Add gn_t(i, 17):    pathway(i, 6).Add gn_t(i, 18)
    pathway(i, 6).Add gn_t(i, 19):    pathway(i, 6).Add gn_t(i, 20)
    pathway(i, 6).Add gn_t(i, 21):    pathway(i, 6).Add gn_t(i, 22)
    pathway(i, 6).Add gn_t(i, 23):    pathway(i, 6).Add gn_t(i, 24)
    
    gn_t(i, 25).Init "UAQU", "Fish consumption rate", lbl(0), txt(0), unt(0), ref(0), "kg/day", 0, 10, 0.11, i, 1
    gn_t(i, 26).Init "TAQU", "Fish consumption period", lbl(1), txt(1), unt(1), ref(1), "day/yr", 0, 365, 365, i, 1
    gn_t(i, 27).Init "UAQU", "Mollusc consumption rate", lbl(2), txt(2), unt(2), ref(2), "kg/day", 0, 10, 0.019, i, 2
    gn_t(i, 28).Init "TAQU", "Mollusc consumption period", lbl(3), txt(3), unt(3), ref(3), "day/yr", 0, 365, 365, i, 2
    gn_t(i, 29).Init "UAQU", "Crustacea consumption rate", lbl(4), txt(4), unt(4), ref(4), "kg/day", 0, 10, 0.019, i, 3
    gn_t(i, 30).Init "TAQU", "Crustacea consumption period", lbl(5), txt(5), unt(5), ref(5), "day/yr", 0, 365, 365, i, 3
    gn_t(i, 31).Init "UAQU", "Aquatic plants consumption rate", lbl(6), txt(6), unt(6), ref(6), "kg/day", 0, 10, 0.019, i, 4
    gn_t(i, 32).Init "TAQU", "Aquatic plants consumption period", lbl(7), txt(7), unt(7), ref(7), "day/yr", 0, 365, 365, i, 4
    pathway(i, 7).Add gn_t(i, 25):    pathway(i, 7).Add gn_t(i, 26)
    pathway(i, 7).Add gn_t(i, 27):    pathway(i, 7).Add gn_t(i, 28)
    pathway(i, 7).Add gn_t(i, 29):    pathway(i, 7).Add gn_t(i, 30)
    pathway(i, 7).Add gn_t(i, 31):    pathway(i, 7).Add gn_t(i, 32)
    
    gn_t(i, 33).Init "UDW", "Drinking water ingestion rate", lbl(0), txt(0), unt(0), ref(0), "L/day", 0, 10, 2, i
    gn_t(i, 34).Init "TDW", "Drinking water ingestion period", lbl(1), txt(1), unt(1), ref(1), "day/yr", 0, 365, 365, i
    pathway(i, 8).Add gn_t(i, 33)
    pathway(i, 8).Add gn_t(i, 34)
    
    
    gn_t(i, 36).Init "USHIN", "Ingestion rate of water while showering", lbl(3), txt(3), unt(3), ref(3), "L/hr", 0, 10, 0.06, i
    pathway(i, 10).Add gn_t(i, 36)
    
    gn_t(i, 38).Init "UINH", "Air inhalation rate", lbl(0), txt(0), unt(0), ref(0), "m^3/day", 0, 50, 23, i
    gn_t(i, 39).Init "TINH", "Air inhalation period", lbl(1), txt(1), unt(1), ref(1), "day/yr", 0, 365, 365, i
    gn_t(i, 48).Init "FRINH", "Fraction of a day outdoor inhalation occurs", lbl(2), txt(2), unt(2), ref(2), "fraction", 0, 1, 0.25, i
    pathway(i, 12).Add gn_t(i, 38):    pathway(i, 12).Add gn_t(i, 39)
    pathway(i, 12).Add gn_t(i, 48)
    
    gn_t(i, 40).Init "UINHR", "Resuspended soil inhalation rate", lbl(0), txt(0), unt(0), ref(0), "m^3/day", 0, 50, 23, i
    gn_t(i, 41).Init "TINHR", "Resuspended soil inhalation period", lbl(1), txt(1), unt(1), ref(1), "day/yr", 0, 365, 365, i
    gn_t(i, 42).Init "FRINHR", "Fraction of a day inhalation of resuspension occurs", lbl(2), txt(2), unt(2), ref(2), "fraction", 0, 1, 1, i
    pathway(i, 13).Add gn_t(i, 40):    pathway(i, 13).Add gn_t(i, 41)
    pathway(i, 13).Add gn_t(i, 42)
    
    gn_t(i, 43).Init "UINDRH", "Indoor inhalation rate", lbl(0), txt(0), unt(0), ref(0), "m^3/day", 0, 50, 23, i
    gn_t(i, 44).Init "TINDRH", "Indoor inhalation period", lbl(1), txt(1), unt(1), ref(1), "day/yr", 0, 365, 365, i
    gn_t(i, 45).Init "FRINDR", "Fraction of a day indoor inhalation occurs", lbl(2), txt(2), unt(2), ref(2), "fraction", 0, 1, 0.75, i
'    gn_t(i, 54).Init "EQFRAC", "Radon equilibrium fraction", lbl(3), txt(3), unt(3), ref(3), "fraction", 0, 1, 1#, i
'    gn_l(i, 0).Init "RNMODL", "Consider radon and progeny", chk(0), ref(3), i
    gn_l(i, 0).Add gn_t(i, 54)
    pathway(i, 14).Add gn_t(i, 43):    pathway(i, 14).Add gn_t(i, 44)
    pathway(i, 14).Add gn_t(i, 45):   ' pathway(i, 14).Add gn_t(i, 54)
 '   pathway(i, 14).Add gn_l(i, 0)
    
'    gn_t(i, 46).Init "ASKINSH", "Area of skin exposed while showering", lbl(3), txt(3), unt(3), ref(3), "cm^2", 0, 50000, 20000, i
'    pathway(i, 15).Add gn_t(i, 46)
    
    
'    gn_d(i, 9).Init "EVSOIL", "Frequency of soil contact", lbl(0), txt(0), unt(0), ref(0), "evt/day", 0, 10, 1, i
'    gn_d(i, 10).Init "TSOILD", "Soil contact days", lbl(1), txt(1), unt(1), ref(1), "day", 0, 365, 182.5, i
'    gn_t(i, 48).Init "ADHSOL", "Soil contact adherance factor", lbl(2), txt(2), unt(2), ref(2), "mg/cm^2", 0, 10, 0.5, i
'    gn_t(i, 49).Init "ASKINSL", "Area of skin exposed to soil", lbl(3), txt(3), unt(3), ref(3), "cm^2", 0, 50000, 5000, i
'    pathway(i, 17).Add gn_d(i, 9)
'    pathway(i, 17).Add gn_d(i, 10)
'    pathway(i, 17).Add gn_t(i, 48)
'    pathway(i, 17).Add gn_t(i, 49)
    
    
    gn_t(i, 52).Init "LOWAGE", "Age group lower bound", vlbl(0), vtxt(0), vunt(0), vref(0), "yr", 0, 110, 0, i
    gn_t(i, 53).Init "UPAGE", "Age group upper bound", vlbl(1), vtxt(1), vunt(1), vref(1), "yr", 0, 110, 70, i
  Next
  
  loadprm
  Unload Loading
  loadng = False
  If haveglyph Then NAGES.ListIndex = agecnt
Else
    End
  End If
End Sub
'
'Sub addrad()
'  If temp.idx1 > m_numcon Then
'    m_numcon = temp.idx1
'    ReDim Preserve rads(temp.idx1) As radparent
'  End If
'  If temp.idx2 > rads(temp.idx1).numprog Then
'    rads(temp.idx1).numprog = temp.idx2
'    ReDim Preserve rads(temp.idx1).progeny(temp.idx2) As raddaughter
'  End If
'End Sub
'
'Sub addcon()
'  If temp.idx2 > f_numcon Then
'    f_numcon = temp.idx2
'    ReDim Preserve con(f_numcon) As parent
'  End If
'  If temp.idx3 > con(temp.idx2).numprog Then
'    con(temp.idx2).numprog = temp.idx3
'    ReDim Preserve con(temp.idx2).progeny(temp.idx3) As daughter
'  End If
'End Sub

Private Sub loadprm()
  Dim i As Long
  Dim j As Long
  Dim k As Long
  Dim m As Long
  
  If open_parm(fle, FUIName, 2) Then
    Do Until EOCF(fle.file)
      If read_parmrec(fle, temp) Then
        Select Case LCase(temp.pName)
'          Case "fui"
'            For m = 1 To temp.idx1
'              If read_parmrec(fle, temp) Then
'                If temp.idx1 = siteIdx Then
'                  Select Case temp.pname
'                  Case "fscname"
'                    addcon
'                    If temp.idx3 > 0 Then
'                      con(temp.idx2).progeny(temp.idx3).name = temp.pval
'                    Else
'                      con(temp.idx2).name = temp.pval
'                    End If
'                  Case "fscasid"
'                    addcon
'                    If temp.idx3 > 0 Then
'                      con(temp.idx2).progeny(temp.idx3).cas = temp.pval
'                    Else
'                      con(temp.idx2).cas = temp.pval
'                    End If
'                  Case "clktype"
'                    addcon
'                    If temp.idx3 > 0 Then
'                      con(temp.idx2).progeny(temp.idx3).kind = CInt(temp.pval)
'                    Else
'                      con(temp.idx2).kind = CInt(temp.pval)
'                    End If
'                  Case "clabskn"
'                    addcon
'                    If temp.idx3 > 0 Then
'                      con(temp.idx2).progeny(temp.idx3).absf = Val(temp.pval)
'                    Else
'                      con(temp.idx2).perm = Val(temp.pval)
'                    End If
'                  Case "clkperm"
'                    addcon
'                    If temp.idx3 > 0 Then
'                      con(temp.idx2).progeny(temp.idx3).perm = Val(temp.pval)
'                    Else
'                      con(temp.idx2).perm = Val(temp.pval)
'                    End If'                  End Select
'                End If
'              End If
'            Next
          Case modName
            haveglyph = True
            Loading.Gauge1.Max = Val(temp.idx1)
            Loading.Gauge1.Value = 0
            For m = 1 To temp.idx1
              updategauge
              If read_parmrec(fle, temp) Then
                Select Case temp.pName
                  Case "nages": agecnt = Val(temp.pval) - 1
'                  Case "casid"
'                    addrad
'                    If temp.idx2 = 0 Then
'                      rads(temp.idx1).cas = temp.pval
'                    Else
'                      rads(temp.idx1).progeny(temp.idx2).cas = temp.pval
'                    End If
'                  Case "absfac"
'                    addrad
'                     If temp.idx2 = 0 Then
'                       rads(temp.idx1).idx = 0
'                       rads(temp.idx1).absf = temp.pval
'                       rads(temp.idx1).afunt = temp.uunit
'                       rads(temp.idx1).afref = temp.ref
'                     Else
'                       rads(temp.idx1).progeny(temp.idx2).idx = 0
'                       rads(temp.idx1).progeny(temp.idx2).absf = temp.pval
'                       rads(temp.idx1).progeny(temp.idx2).afunt = temp.uunit
'                       rads(temp.idx1).progeny(temp.idx2).afref = temp.ref
'                    End If
'                  Case "permk"
'                    addrad
'                     If temp.idx2 = 0 Then
'                       rads(temp.idx1).idx = 0
'                       rads(temp.idx1).perm = temp.pval
'                       rads(temp.idx1).pkunt = temp.uunit
'                       rads(temp.idx1).pkref = temp.ref
'                     Else
'                       rads(temp.idx1).progeny(temp.idx2).idx = 0
'                       rads(temp.idx1).progeny(temp.idx2).perm = temp.pval
'                       rads(temp.idx1).progeny(temp.idx2).pkunt = temp.uunit
'                       rads(temp.idx1).progeny(temp.idx2).pkref = temp.ref
'                    End If
                  Case Else
                    For i = 0 To 54
                      If gn_t(temp.idx1, i).Name = temp.pName And gn_t(temp.idx1, i).idx(0) = temp.idx1 And gn_t(temp.idx1, i).idx(1) = temp.idx2 Then
                        gn_t(temp.idx1, i).Text = convert(temp.cunit, temp.uunit, Val(temp.pval))
                        gn_t(temp.idx1, i).pUnit = temp.cunit
                        gn_t(temp.idx1, i).Unit = temp.uunit
                        gn_t(temp.idx1, i).refnum = temp.ref
                      End If
                    Next
                    For i = 0 To 11
                      If gn_d(temp.idx1, i).Name = temp.pName Then
                        gn_d(temp.idx1, i).Text = convert(temp.cunit, temp.uunit, Val(temp.pval))
                        gn_d(temp.idx1, i).pUnit = temp.cunit
                        gn_d(temp.idx1, i).Unit = temp.uunit
                        gn_d(temp.idx1, i).refnum = temp.ref
                      End If
                    Next
                    For i = 0 To 9
                      If gn_s(i).Name = temp.pName Then
                        gn_s(i).Text = convert(temp.cunit, temp.uunit, Val(temp.pval))
                        gn_s(i).pUnit = temp.cunit
                        gn_s(i).Unit = temp.uunit
                        gn_s(i).refnum = temp.ref
                      End If
                    Next
                End Select
              End If
            Next
          Case Else
            For m = 1 To temp.idx1
              get_line fle.file
            Next
        End Select
      End If
    Loop
    close_parm fle
  
'resolve contaminate differences          if contaminate no longer exists its index is 0
'    For i = 1 To f_numcon
'      For j = 1 To m_numcon
'        If rads(j).cas = con(i).cas Then
'          If con(i).kind = 1 Then rads(j).idx = i
'          Exit For
'       End If
'      Next
'      If j > m_numcon And con(i).kind = 1 Then
'        m_numcon = m_numcon + 1
'        ReDim Preserve rads(m_numcon) As radparent
'        rads(m_numcon).cas = con(i).cas
'        rads(m_numcon).idx = i
'        rads(m_numcon).afunt = "fraction"
'        rads(m_numcon).pkunt = "cm/hr"
'        rads(m_numcon).afref = 0
'        rads(m_numcon).pkref = 0
'      End If
'    Next
'    For i = 1 To m_numcon
'      If rads(i).idx > 0 Then cb.AddItem con(rads(i).idx).name
'    Next

'resolve progeny differences         if progeny no longer exists its index is 0
'    For i = 1 To m_numcon
'      If rads(i).idx <> 0 Then
'        For j = 1 To con(rads(i).idx).numprog
'          For k = 1 To rads(i).numprog
'            If rads(i).progeny(k).cas = con(rads(i).idx).progeny(j).cas Then
'              rads(i).progeny(k).idx = j
'              Exit For
'            End If
'          Next
'          If k > rads(i).numprog Then
'            rads(i).numprog = rads(i).numprog + 1
'            ReDim Preserve rads(i).progeny(rads(i).numprog) As raddaughter
'            rads(i).progeny(rads(i).numprog).cas = con(rads(i).idx).progeny(j).cas
'            rads(i).progeny(rads(i).numprog).idx = j
'            rads(i).progeny(rads(i).numprog).afunt = "fraction"
'            rads(i).progeny(rads(i).numprog).pkunt = "cm/hr"
'            rads(i).progeny(rads(i).numprog).afref = 0
'            rads(i).progeny(rads(i).numprog).pkref = 0
'          End If
'        Next
'      End If
'    Next
  
  Else
    put_val errfile, "Can't find or open file " & FUIName
    put_line errfile
    close_csv errfile
    End
  End If
End Sub

Private Sub leave_Click()
  close_csv errfile
  Kill RunName & ".ERR"
  End
End Sub

Private Sub save_Click()
  Dim i As Long
  Dim j As Long
  Dim k As Long
  Dim fName As String
  Dim tmp As parmfile
  Dim flag As Boolean
  Dim degree  As Double
  Dim parm As parmrec
  
  flag = False
  fName = RunName & ".GID"
  If open_parm(fle, fName, 1) Then
    set_parm parm, "NAGES", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CStr(NAGES.ListIndex + 1)
    write_parmrec fle, parm
    For i = 0 To NAGES.ListIndex
      If Not gn_t(i + 1, 52).Store Then flag = True
      If Not gn_t(i + 1, 53).Store Then flag = True
      ' did go to 18 for dermal exposures
      For j = 0 To 14
        For k = 1 To pathway(i + 1, j).Count
          If Not pathway(i + 1, j).item(k).Store Then flag = True
        Next
      Next
    Next
    
'    For i = 1 To m_numcon
'      If rads(i).idx <> 0 Then
'        set_parm parm, "CASID", rads(i).idx, 0, 0, 0, 0, 0, 0, "N/A", "N/A", rads(i).cas
'        write_parmrec fle, parm
'        If rads(i).perm <> "" Then
'          set_parm parm, "PERMK", rads(i).idx, 0, 0, 0, 0, 0, rads(i).pkref, rads(i).pkunt, "cm/hr", convert(rads(i).pkunt, "cm/hr", Val(rads(i).perm))
'          write_parmrec fle, parm
'        Else
'          flag = True
'        End If
'        If rads(i).absf <> "" Then
'          set_parm parm, "ABSFAC", rads(i).idx, 0, 0, 0, 0, 0, rads(i).afref, rads(i).afunt, "fraction", convert(rads(i).afunt, "fraction", Val(rads(i).absf))
'          write_parmrec fle, parm
'        Else
'          flag = True
'        End If
'      End If
'    Next
    WriteDesLabels fle
    close_parm fle
    If flag Then
      put_val errfile, "Incomplete data entry!"
      put_line errfile
      close_csv errfile
    Else
      close_csv errfile
      Kill RunName & ".ERR"
    End If
  Else
    put_val errfile, "Unable to create transaction file" & RunName & ".GID"
    put_line errfile
    close_csv errfile
  End If
  End
End Sub

Private Sub ud_Click()
  Loading.Show
  loaddef 3
  mi.Checked = False
  ai.Checked = False
  ud.Checked = True
  Combo1_Click
  Unload Loading
End Sub

Private Sub vunt_Click(index As Integer)
  If Not loadng Then
    gn_t(Combo1.ListIndex + 1, 52 + index).Click
  End If
End Sub

Private Sub unt_Click(index As Integer)
  If Not loadng Then
    pathway(Combo1.ListIndex + 1, p.ListIndex).item(index + 1).Click
  End If
End Sub

Private Sub chk_Click(index As Integer)
  If loadng Then Exit Sub
  pathway(Combo1.ListIndex + 1, p.ListIndex).item(5).Value = chk(index).Value
End Sub

Private Sub vtxt_KeyUp(index As Integer, KeyCode As Integer, Shift As Integer)
  If Not loadng Then
    If KeyCode = vbKeyF2 Then
      gn_t(Combo1.ListIndex + 1, 52 + index).Help
    Else
      gn_t(Combo1.ListIndex + 1, 52 + index).Change
    End If
  End If
End Sub

Private Sub txt_KeyUp(index As Integer, KeyCode As Integer, Shift As Integer)
  If Not loadng Then
    If KeyCode = vbKeyF2 Then
      pathway(Combo1.ListIndex + 1, p.ListIndex).item(index + 1).Help
    Else
      pathway(Combo1.ListIndex + 1, p.ListIndex).item(index + 1).Change
    End If
  End If
End Sub

Private Sub unt_GotFocus(index As Integer)
  Set rfs = ref(index)
  mnuRef.Enabled = True
  HelpAnchor = pathway(Combo1.ListIndex + 1, p.ListIndex).item(index + 1).Name
  Set uiobj = pathway(Combo1.ListIndex + 1, p.ListIndex).item(index + 1)
End Sub

Private Sub txt_GotFocus(index As Integer)
  Set rfs = ref(index)
  mnuRef.Enabled = True
  HelpAnchor = pathway(Combo1.ListIndex + 1, p.ListIndex).item(index + 1).Name
  Set uiobj = pathway(Combo1.ListIndex + 1, p.ListIndex).item(index + 1)
End Sub

Private Sub chk_GotFocus(index As Integer)
'  Set rfs = ref(index)
  mnuRef.Enabled = False
  HelpAnchor = pathway(Combo1.ListIndex + 1, p.ListIndex).item(index + 1).Name
  Set uiobj = pathway(Combo1.ListIndex + 1, p.ListIndex).item(index + 1)
End Sub

Private Sub vunt_GotFocus(index As Integer)
  Set rfs = vref(index)
  mnuRef.Enabled = False
  HelpAnchor = gn_t(Combo1.ListIndex + 1, 52 + index).Name
  Set uiobj = gn_t(Combo1.ListIndex + 1, 52 + index)
End Sub

Private Sub vtxt_GotFocus(index As Integer)
  'Set rfs = vref(index)
  mnuRef.Enabled = False
  HelpAnchor = gn_t(Combo1.ListIndex + 1, 52 + index).Name
  Set uiobj = gn_t(Combo1.ListIndex + 1, 52 + index)
End Sub

Sub loaddef(kind As Long)
  Dim i As Long
  Dim m As Long
  Dim fName As String
  Dim fle As parmfile
  
  Select Case kind
  Case 1
    fName = App.Path + "\gndflinm.def"
  Case 2
    fName = App.Path + "\gndflina.def"
  Case 3
    fName = App.Path + "\gndfliud.def"
  Case Else: Exit Sub
  End Select
  
  loadng = True
  If open_parm(fle, fName, 2) Then
    Do Until EOCF(fle.file)
      If read_parmrec(fle, temp) Then
        Loading.Gauge1.Max = Val(temp.idx1)
        Loading.Gauge1.Value = 0
        For m = 1 To temp.idx1
          updategauge
          If read_parmrec(fle, temp) Then
            For i = 0 To 53
              If gn_t(temp.idx1, i).Name = temp.pName And gn_t(temp.idx1, i).idx(0) = temp.idx1 And gn_t(temp.idx1, i).idx(1) = temp.idx2 Then
                gn_t(temp.idx1, i).Text = convert(temp.cunit, temp.uunit, Val(temp.pval))
                gn_t(temp.idx1, i).pUnit = temp.cunit
                gn_t(temp.idx1, i).Unit = temp.uunit
                gn_t(temp.idx1, i).refnum = temp.ref
              End If
            Next
            For i = 0 To 11
              If gn_d(temp.idx1, i).Name = temp.pName Then
                gn_d(temp.idx1, i).Text = convert(temp.cunit, temp.uunit, Val(temp.pval))
                gn_d(temp.idx1, i).pUnit = temp.cunit
                gn_d(temp.idx1, i).Unit = temp.uunit
                gn_d(temp.idx1, i).refnum = temp.ref
              End If
            Next
            For i = 0 To 9
              If gn_s(i).Name = temp.pName Then
                gn_s(i).Text = convert(temp.cunit, temp.uunit, Val(temp.pval))
                gn_s(i).pUnit = temp.cunit
                gn_s(i).Unit = temp.uunit
                gn_s(i).refnum = temp.ref
              End If
            Next
          End If
        Next
      End If
    Loop
    close_parm fle
    loadng = False

  Else
    put_val errfile, "Can't find or open file " & FUIName
    put_line errfile
    close_csv errfile
    End
  End If
End Sub

Private Sub form_KeyPress(KeyAscii As Integer)
  Select Case KeyAscii
    Case 48 To 57, 43, 45, 46, 69, 101, 8:
    Case Else: KeyAscii = 0
  End Select
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
