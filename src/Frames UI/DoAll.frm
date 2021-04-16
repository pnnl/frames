VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Object = "{6B7E6392-850A-101B-AFC0-4210102A8DA7}#1.3#0"; "COMCTL32.OCX"
Begin VB.Form frmFUI 
   Caption         =   "Form1"
   ClientHeight    =   1530
   ClientLeft      =   5670
   ClientTop       =   5625
   ClientWidth     =   6105
   Icon            =   "DoAll.frx":0000
   LinkTopic       =   "Form1"
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   1530
   ScaleWidth      =   6105
   Begin ComctlLib.StatusBar StatusBar1 
      Align           =   2  'Align Bottom
      Height          =   420
      Left            =   0
      TabIndex        =   1
      Top             =   1110
      Width           =   6105
      _ExtentX        =   10769
      _ExtentY        =   741
      SimpleText      =   ""
      _Version        =   327682
      BeginProperty Panels {0713E89E-850A-101B-AFC0-4210102A8DA7} 
         NumPanels       =   2
         BeginProperty Panel1 {0713E89F-850A-101B-AFC0-4210102A8DA7} 
            Key             =   ""
            Object.Tag             =   ""
         EndProperty
         BeginProperty Panel2 {0713E89F-850A-101B-AFC0-4210102A8DA7} 
            AutoSize        =   1
            Object.Width           =   7673
            Key             =   ""
            Object.Tag             =   ""
         EndProperty
      EndProperty
   End
   Begin VB.PictureBox picModel 
      Height          =   525
      Index           =   0
      Left            =   1440
      ScaleHeight     =   465
      ScaleWidth      =   465
      TabIndex        =   0
      Top             =   120
      Width           =   525
   End
   Begin MSComDlg.CommonDialog cdlAll 
      Left            =   840
      Top             =   120
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin ComctlLib.ImageList ImageList1 
      Left            =   120
      Top             =   120
      _ExtentX        =   1005
      _ExtentY        =   1005
      BackColor       =   -2147483643
      ImageWidth      =   32
      ImageHeight     =   32
      MaskColor       =   12632256
      _Version        =   327682
      BeginProperty Images {0713E8C2-850A-101B-AFC0-4210102A8DA7} 
         NumListImages   =   1
         BeginProperty ListImage1 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "DoAll.frx":030A
            Key             =   ""
         EndProperty
      EndProperty
   End
End
Attribute VB_Name = "frmFUI"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Dim argc As Long
Dim argv() As String

Sub GetArguments()
  Dim args As String
  Dim pos As Long
    
  FRAMES_INI = App.Path + "\\FramesUI.ini"
  argc = 0
  args = Trim$(Command$)
  If Len(args) > 0 Then
    Do
      pos = InStr(args, " ")
      argc = argc + 1
      If pos > 0 Then
        ReDim Preserve argv(argc) As String
        argv(argc - 1) = Trim$(Left$(args, pos))
        args = Trim$(Right$(args, Len(args) - pos))
      Else
        ReDim Preserve argv(argc)
        argv(argc - 1) = Trim$(args)
      End If
    Loop Until pos = 0
  End If
End Sub

Private Sub Form_load()
Dim SiteIndex As Long
Dim errfile As csv
Dim RunName As String
Dim FuiName As String

  GetArguments
  If argc < 4 Then
    MsgBox "Not enough arguments passed" & Chr(10) & "Contact PNNL"
    End
  End If
'  ChDir "c:\framesv1"
'  AppPath = "c:\framesv1"
  If Not (ReadIniString("App Path", "Fui", "none", AppPath)) Then AppPath = App.Path & "\"
  
  Set cdl = cdlAll
  ReDim Sites(MAX_SITES)
  GidFile = BLANK
  GidDir = BLANK
  GidTitle = BLANK
  TmpTitle = BLANK
  lastId = 0
  NumSites = 0
  currentSite = 0
  currentGlyph = 0
  ReadModules
  LongGidFile = argv(1) & DOT_GID
  LongGidDir = SplitPath(LongGidFile, SP_DIR)
  LongGidTitle = LongGidDir & SplitPath(LongGidFile, SP_TITLE)
  GidFile = argv(1) & DOT_GID
  GidDir = SplitPath(GidFile, SP_DIR)
  GidTitle = GidDir & SplitPath(GidFile, SP_TITLE)
  TmpTitle = argv(2)
  SiteIndex = Val(argv(3))
  If GoodGidName(GidFile) And "" <> Dir$(FuiName) Then
    Sites_Read GidFile
    doAll argv(0), SiteIndex
  End If
  Close 'any files that might be open
  End
End Sub
