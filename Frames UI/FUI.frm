VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "THREED32.OCX"
Object = "{3B7C8863-D78F-101B-B9B5-04021C009402}#1.2#0"; "RICHTX32.OCX"
Object = "{6B7E6392-850A-101B-AFC0-4210102A8DA7}#1.3#0"; "COMCTL32.OCX"
Begin VB.Form frmFUI 
   AutoRedraw      =   -1  'True
   Caption         =   "Framework for Risk Analysis in Multimedia Environmental Systems"
   ClientHeight    =   4965
   ClientLeft      =   450
   ClientTop       =   765
   ClientWidth     =   8310
   Icon            =   "FUI.frx":0000
   LinkTopic       =   "Form2"
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   4965
   ScaleWidth      =   8310
   Begin MSComDlg.CommonDialog cdlAll 
      Left            =   720
      Top             =   3720
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
      DefaultExt      =   "GID"
      DialogTitle     =   "Global Input File Save"
      Filter          =   "*.GID"
   End
   Begin ComctlLib.StatusBar StatusBar1 
      Align           =   2  'Align Bottom
      Height          =   372
      Left            =   0
      TabIndex        =   1
      Top             =   4596
      Width           =   8304
      _ExtentX        =   14658
      _ExtentY        =   661
      SimpleText      =   ""
      _Version        =   327682
      BeginProperty Panels {0713E89E-850A-101B-AFC0-4210102A8DA7} 
         NumPanels       =   5
         BeginProperty Panel1 {0713E89F-850A-101B-AFC0-4210102A8DA7} 
            AutoSize        =   1
            Object.Width           =   2963
            Key             =   ""
            Object.Tag             =   ""
         EndProperty
         BeginProperty Panel2 {0713E89F-850A-101B-AFC0-4210102A8DA7} 
            AutoSize        =   1
            Object.Width           =   2963
            Key             =   ""
            Object.Tag             =   ""
         EndProperty
         BeginProperty Panel3 {0713E89F-850A-101B-AFC0-4210102A8DA7} 
            AutoSize        =   1
            Object.Width           =   2963
            Key             =   ""
            Object.Tag             =   ""
         EndProperty
         BeginProperty Panel4 {0713E89F-850A-101B-AFC0-4210102A8DA7} 
            Style           =   6
            Alignment       =   1
            TextSave        =   "2/5/2013"
            Key             =   ""
            Object.Tag             =   ""
         EndProperty
         BeginProperty Panel5 {0713E89F-850A-101B-AFC0-4210102A8DA7} 
            Style           =   5
            Alignment       =   1
            TextSave        =   "1:26 PM"
            Key             =   ""
            Object.Tag             =   ""
         EndProperty
      EndProperty
   End
   Begin VB.PictureBox picIcon 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   540
      Left            =   1320
      ScaleHeight     =   480
      ScaleWidth      =   480
      TabIndex        =   0
      TabStop         =   0   'False
      Tag             =   "14"
      Top             =   3720
      Visible         =   0   'False
      Width           =   540
   End
   Begin Threed.SSPanel RPanel 
      Height          =   3645
      Left            =   0
      TabIndex        =   2
      Top             =   30
      Width           =   8250
      _Version        =   65536
      _ExtentX        =   14552
      _ExtentY        =   6429
      _StockProps     =   15
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BevelWidth      =   2
      BorderWidth     =   0
      BevelOuter      =   1
      Begin ComctlLib.TreeView TreeView1 
         DragIcon        =   "FUI.frx":030A
         Height          =   1455
         Left            =   75
         TabIndex        =   3
         Top             =   1980
         Width           =   3015
         _ExtentX        =   5318
         _ExtentY        =   2566
         _Version        =   327682
         HideSelection   =   0   'False
         Indentation     =   18
         LabelEdit       =   1
         Sorted          =   -1  'True
         Style           =   5
         ImageList       =   "ImageList1"
         Appearance      =   0
      End
      Begin Threed.SSPanel bar 
         Height          =   3612
         Left            =   3120
         TabIndex        =   5
         Top             =   0
         Visible         =   0   'False
         Width           =   132
         _Version        =   65536
         _ExtentX        =   233
         _ExtentY        =   6371
         _StockProps     =   15
         BackColor       =   0
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         BevelOuter      =   0
      End
      Begin Threed.SSPanel SSPanel1 
         Height          =   240
         Left            =   4560
         TabIndex        =   7
         Top             =   840
         Visible         =   0   'False
         Width           =   2040
         _Version        =   65536
         _ExtentX        =   3598
         _ExtentY        =   423
         _StockProps     =   15
         BackColor       =   12632256
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Autosize        =   2
      End
      Begin RichTextLib.RichTextBox RichTextBox1 
         Height          =   1365
         Left            =   3360
         TabIndex        =   8
         TabStop         =   0   'False
         Top             =   1995
         Width           =   4800
         _ExtentX        =   8467
         _ExtentY        =   2408
         _Version        =   393217
         BorderStyle     =   0
         Enabled         =   -1  'True
         ReadOnly        =   -1  'True
         ScrollBars      =   2
         AutoVerbMenu    =   -1  'True
         TextRTF         =   $"FUI.frx":0614
      End
      Begin VB.PictureBox picSite 
         AutoRedraw      =   -1  'True
         Height          =   1845
         Left            =   3360
         ScaleHeight     =   1785
         ScaleWidth      =   4740
         TabIndex        =   4
         Top             =   90
         Width           =   4800
      End
      Begin VB.Image Image1 
         Height          =   1815
         Left            =   45
         Stretch         =   -1  'True
         Top             =   120
         Width           =   2985
      End
   End
   Begin RichTextLib.RichTextBox RichTextBox2 
      Height          =   492
      Left            =   1920
      TabIndex        =   9
      TabStop         =   0   'False
      Top             =   3720
      Width           =   480
      _ExtentX        =   847
      _ExtentY        =   873
      _Version        =   393217
      BorderStyle     =   0
      Enabled         =   -1  'True
      ScrollBars      =   2
      AutoVerbMenu    =   -1  'True
      TextRTF         =   $"FUI.frx":0696
   End
   Begin VB.Label tmpLabel 
      Alignment       =   2  'Center
      BorderStyle     =   1  'Fixed Single
      Caption         =   "dummy"
      Height          =   255
      Left            =   6315
      TabIndex        =   6
      Top             =   3840
      Visible         =   0   'False
      Width           =   1545
   End
   Begin ComctlLib.ImageList ImageList1 
      Left            =   0
      Top             =   3720
      _ExtentX        =   1005
      _ExtentY        =   1005
      BackColor       =   -2147483643
      ImageWidth      =   32
      ImageHeight     =   32
      MaskColor       =   12632256
      _Version        =   327682
      BeginProperty Images {0713E8C2-850A-101B-AFC0-4210102A8DA7} 
         NumListImages   =   7
         BeginProperty ListImage1 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "FUI.frx":0718
            Key             =   "OpenFolder"
            Object.Tag             =   "open.ico"
         EndProperty
         BeginProperty ListImage2 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "FUI.frx":136A
            Key             =   "CloseFolder"
            Object.Tag             =   "close.ico"
         EndProperty
         BeginProperty ListImage3 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "FUI.frx":1FBC
            Key             =   "unknown"
         EndProperty
         BeginProperty ListImage4 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "FUI.frx":22D6
            Key             =   ""
         EndProperty
         BeginProperty ListImage5 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "FUI.frx":25F0
            Key             =   ""
         EndProperty
         BeginProperty ListImage6 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "FUI.frx":290A
            Key             =   ""
         EndProperty
         BeginProperty ListImage7 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "FUI.frx":2C24
            Key             =   ""
         EndProperty
      EndProperty
   End
   Begin VB.Menu mnuFile 
      Caption         =   "&File"
      Begin VB.Menu mnuNew 
         Caption         =   "&New"
         Shortcut        =   ^N
      End
      Begin VB.Menu mnuOpen 
         Caption         =   "&Open"
      End
      Begin VB.Menu mnuClose 
         Caption         =   "&Close"
      End
      Begin VB.Menu mnusep1 
         Caption         =   "-"
      End
      Begin VB.Menu mnuSave 
         Caption         =   "&Save"
         Shortcut        =   {F2}
      End
      Begin VB.Menu mnuSaveAs 
         Caption         =   "Save &As"
      End
      Begin VB.Menu mnuSep2 
         Caption         =   "-"
      End
      Begin VB.Menu mnuPrint 
         Caption         =   "&Print..."
         Shortcut        =   ^P
      End
      Begin VB.Menu mnuReference 
         Caption         =   "&References"
      End
      Begin VB.Menu mnuComment 
         Caption         =   "Project &Description"
      End
      Begin VB.Menu mnuSep3 
         Caption         =   "-"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFileN 
         Caption         =   ""
         Index           =   0
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFileN 
         Caption         =   ""
         Index           =   1
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFileN 
         Caption         =   ""
         Index           =   2
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFileN 
         Caption         =   ""
         Index           =   3
         Visible         =   0   'False
      End
      Begin VB.Menu mnuSep4 
         Caption         =   "-"
      End
      Begin VB.Menu mnuDesReport 
         Caption         =   "Module Description Report"
      End
      Begin VB.Menu mnuRefreshModules 
         Caption         =   "Refresh Module Descriptions"
      End
      Begin VB.Menu mnuExit 
         Caption         =   "E&xit"
         Shortcut        =   ^{F4}
      End
   End
   Begin VB.Menu mnuSite 
      Caption         =   "&Site"
      Enabled         =   0   'False
      Begin VB.Menu mnuNewSite 
         Caption         =   "&New"
      End
      Begin VB.Menu mnuRenSite 
         Caption         =   "&Rename"
      End
      Begin VB.Menu mnuDelSite 
         Caption         =   "&Delete"
      End
      Begin VB.Menu mnuSep5 
         Caption         =   "-"
      End
      Begin VB.Menu mnuSites 
         Caption         =   "Site 1"
         Index           =   0
      End
      Begin VB.Menu mnuSites 
         Caption         =   "Site 2"
         Index           =   1
         Visible         =   0   'False
      End
      Begin VB.Menu mnuSites 
         Caption         =   "Site 3"
         Index           =   2
         Visible         =   0   'False
      End
      Begin VB.Menu mnuSites 
         Caption         =   "Site 4"
         Index           =   3
         Visible         =   0   'False
      End
      Begin VB.Menu mnuSites 
         Caption         =   "Site 5"
         Index           =   4
         Visible         =   0   'False
      End
      Begin VB.Menu mnuSep6 
         Caption         =   "-"
      End
      Begin VB.Menu mnuPrintSite 
         Caption         =   "&Print Site"
      End
      Begin VB.Menu mnuPrintAllSites 
         Caption         =   "P&rint All Sites"
      End
   End
   Begin VB.Menu mnuCustomize 
      Caption         =   "&Customize"
      Begin VB.Menu mnuShowId 
         Caption         =   "&Show Object Id"
         Shortcut        =   ^O
      End
      Begin VB.Menu mnuShowIcon 
         Caption         =   "Show &Icons"
         Shortcut        =   ^I
      End
      Begin VB.Menu mnuPickFont 
         Caption         =   "&Font"
         Shortcut        =   ^F
      End
      Begin VB.Menu mnuPickColor 
         Caption         =   "&Colors"
      End
      Begin VB.Menu mnuLogo 
         Caption         =   "&Logo"
         Begin VB.Menu mnuShowLogo 
            Caption         =   "Show"
         End
         Begin VB.Menu mnuSep7 
            Caption         =   "-"
         End
         Begin VB.Menu mnuSelectLogo 
            Caption         =   "Select..."
         End
      End
      Begin VB.Menu mnuModIcon 
         Caption         =   "Use module &icons"
         Shortcut        =   ^T
      End
      Begin VB.Menu mnuShowMsg 
         Caption         =   "Show &Messages"
         Shortcut        =   ^M
      End
   End
   Begin VB.Menu mnuGO 
      Caption         =   "&GO..."
      Enabled         =   0   'False
   End
   Begin VB.Menu mnuHelp 
      Caption         =   "&Help"
      Begin VB.Menu mnuHowTo 
         Caption         =   "&How To"
      End
      Begin VB.Menu mnuSep8 
         Caption         =   "-"
      End
      Begin VB.Menu mnuAbout 
         Caption         =   "&About FRAMES"
      End
   End
   Begin VB.Menu mnuOptions 
      Caption         =   "Options"
      Visible         =   0   'False
      Begin VB.Menu mnuConnect 
         Caption         =   "Connect-Disconnect"
         Begin VB.Menu mnuIConnect 
            Caption         =   ""
            Index           =   1
         End
      End
      Begin VB.Menu mnuSep11 
         Caption         =   "-"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuInfo 
         Caption         =   "General Info"
         Enabled         =   0   'False
      End
      Begin VB.Menu mnuInput 
         Caption         =   "User Input"
         Enabled         =   0   'False
      End
      Begin VB.Menu mnuRun 
         Caption         =   "Run Model"
         Enabled         =   0   'False
      End
      Begin VB.Menu mnuSep9 
         Caption         =   "-"
      End
      Begin VB.Menu mnuRename 
         Caption         =   "Rename"
      End
      Begin VB.Menu mnuDelete 
         Caption         =   "Delete"
      End
      Begin VB.Menu mnuSep10 
         Caption         =   "-"
      End
      Begin VB.Menu mnuViewInput 
         Caption         =   "View/Print User Input"
         Enabled         =   0   'False
      End
      Begin VB.Menu mnuViewOutput 
         Caption         =   "View/Print Module Output"
         Enabled         =   0   'False
         Begin VB.Menu mnuViewOut 
            Caption         =   ""
            Index           =   1
         End
      End
   End
   Begin VB.Menu mnuTreeViewOptions 
      Caption         =   "TreeViewOptions"
      Visible         =   0   'False
      Begin VB.Menu mnuAddGlyph 
         Caption         =   "Add to Scenario"
      End
      Begin VB.Menu mnuSelectIcon 
         Caption         =   "Change Icon ..."
      End
   End
End
Attribute VB_Name = "frmFUI"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Dim FUI_Loading As Boolean
Dim DSX As Single
Dim DSY As Single
Dim DEX As Single
Dim DEY As Single
Dim lastX As Single
Dim lastY As Single
Dim Drag As Boolean
Dim vertical As Boolean
Dim horizontal As Boolean
Dim CurrentDir As String
Dim ModuleIsExecuting As Boolean

'===========================================================
' Event blocking routines
'===========================================================

Sub ShowWaitMessage()
Dim msg As String
  
  SSPanel1.Visible = ModuleIsExecuting
  If ModuleIsExecuting Then
    msg = "Waiting for module execution request:"
    msg = msg & Chr(13) & "Object Id: " & Sites(currentSite).Glyphs(currentGlyph).name
    msg = msg & Chr(13) & "Name: " & Sites(currentSite).Glyphs(currentGlyph).label
    msg = msg & Chr(13) & "Module: " & Sites(currentSite).Glyphs(currentGlyph).Model
    SSPanel1.Caption = msg
    SSPanel1.Left = rbar + barwidth + 0.5 * (pic.Width - SSPanel1.Width)
    SSPanel1.Top = 0.5 * (pic.Height - SSPanel1.Height)
  End If
End Sub

Sub EnableProcessEvents()
  ModuleIsExecuting = False
  ShowWaitMessage
  MousePointer = vbDefault
End Sub

Sub DisableProcessEvents()
  ModuleIsExecuting = True
  ShowWaitMessage
  MousePointer = vbHourglass
End Sub

Private Sub mnuDesReport_Click()
Dim i As Long
Dim j As Long
Dim k As Long
Dim f As Long
Dim r As Long
Dim t As Long
Dim rsel() As Boolean
Dim wsel() As Boolean
  
  ReDim rsel(BCFileCount) As Boolean
  ReDim wsel(BCFileCount) As Boolean
  For i = 0 To GroupCount - 1
    For j = 0 To BCFileCount
      rsel(j) = False
      wsel(j) = False
    Next
    
    k = 0
    rtb.Text = rtb.Text + vbCrLf + CStr(i + 1) & ") " & Group(i).name & " module group"
    
    For j = 0 To ModuleCount - 1
      If Module(j).GrpIdx = i Then
        k = k + 1
        For f = 0 To BCFileCount - 1
          For r = 1 To Module(j).nread
            For t = 0 To UBound(Module(j).reads(r).Spec)
              If Module(j).reads(r).Spec(t).Type = BCFile(f).Extension And _
                  Module(j).reads(r).Spec(t).Qual = BCFile(f).Qualifier Then
                rsel(f) = True
                rsel(BCFileCount) = True
              End If
            Next
          Next
          For r = 1 To Module(j).nwrite
            If Module(j).writes(r).Spec(1).Type = BCFile(f).Extension And _
                Module(j).writes(r).Spec(1).Qual = BCFile(f).Qualifier Then
              wsel(f) = True
              wsel(BCFileCount) = True
            End If
          Next
        Next
      End If
    Next
      
    If rsel(BCFileCount) Then
      rtb.Text = rtb.Text + vbCrLf + "    Types of boundary condition inputs"
    End If
    For j = 0 To BCFileCount - 1
      If rsel(j) Then
        rtb.Text = rtb.Text + vbCrLf + "        " & BCFile(j).Extension & ":" & BCFile(j).Qualifier
      End If
    Next
    If wsel(BCFileCount) Then
      rtb.Text = rtb.Text + vbCrLf + "    Types of boundary condition outputs"
    End If
    For j = 0 To BCFileCount - 1
      If wsel(j) Then
        rtb.Text = rtb.Text + vbCrLf + "        " & BCFile(j).Extension & ":" & BCFile(j).Qualifier
      End If
    Next
  Next
End Sub

'===========================================================
' File menu click events
'===========================================================

Private Sub mnuFile_Click()
  If ModuleIsExecuting Then Exit Sub
  mnuClose.Enabled = LongGidDir <> BLANK
  mnuSave.Enabled = mnuClose.Enabled
  mnuSaveAs.Enabled = mnuClose.Enabled
  mnuPrint.Enabled = mnuClose.Enabled
  mnuReference.Enabled = mnuClose.Enabled
  mnuComment.Enabled = mnuClose.Enabled
End Sub

Private Sub mnuNew_Click()
  If ModuleIsExecuting Then Exit Sub
  If vbCancel = Save_Changes() Then Exit Sub
  If OpenSites(LCase(GetFileName(GID_OPEN_NEW)), True) Then
    mnuNewSite_Click
    If NumSites = 0 Then
      CloseSites
    Else
      If Not RefAvailable Then Reference.new_ref
      UpdateSiteMenu 0
      UpdateFileMenu LongGidFile
      mnuSite.Enabled = True
      mnuGO.Enabled = True
      mnuRefreshModules.Enabled = False
      DrawSite Sites(currentSite), pic, False
    End If
  End If
End Sub

Private Sub mnuOpen_Click()
  If ModuleIsExecuting Then Exit Sub
  If vbCancel = Save_Changes() Then Exit Sub
  If OpenSites(LCase(GetFileName(GID_OPEN)), False) Then
    If Not RefAvailable Then Reference.new_ref
    UpdateSiteMenu 0
    UpdateFileMenu LongGidFile
    mnuSite.Enabled = True
    mnuGO.Enabled = True
    mnuRefreshModules.Enabled = False
    DrawSite Sites(currentSite), pic, False
  End If
End Sub

Private Sub mnuClose_Click()
  If ModuleIsExecuting Then Exit Sub
  If vbCancel = Save_Changes() Then Exit Sub
  CloseSites
  UpdateSiteMenu 0
  mnuSite.Enabled = False
  mnuGO.Enabled = False
  mnuRefreshModules.Enabled = True
  DrawSite Sites(currentSite), pic, False
End Sub

Private Sub mnuSave_Click()
  If ModuleIsExecuting Then Exit Sub
  SaveSites False, True
End Sub

Private Sub mnuSaveAs_Click()
  If ModuleIsExecuting Then Exit Sub
  If SaveSites(True, True) Then
    UpdateFileMenu LongGidFile
    DrawSite Sites(currentSite), pic, False
  End If
End Sub

Private Sub mnuReference_Click()
  If ModuleIsExecuting Then Exit Sub
  RefMode = 0
  Reference.Show 1, Me
  modified = True
End Sub

Private Sub mnuComment_Click()
  If ModuleIsExecuting Then Exit Sub
  frmComment.Show 1, Me
  modified = True
End Sub

Private Sub mnuPrint_Click()
  If ModuleIsExecuting Then Exit Sub
  frmPrint.Show 1
End Sub

Private Sub mnuFileN_Click(Index As Integer)
Dim asNew As Boolean
  
  asNew = False
  If ModuleIsExecuting Then Exit Sub
  If vbCancel = Save_Changes() Then Exit Sub

' dir errors when drive is not present (removeable drives)
On Error GoTo DriveNotFound
  If BLANK = Dir(mnuFileN(Index).Caption) Then
    If MsgBox("File not found!" & vbCrLf & "Create " & mnuFileN(Index).Caption, vbExclamation + vbYesNo) = vbYes Then
      asNew = True
    Else
      Exit Sub
    End If
  End If
  
  If OpenSites(LCase(mnuFileN(Index).Caption), asNew) Then
    If asNew Then mnuNewSite_Click
    If NumSites = 0 Then
      CloseSites
    Else
      If Not RefAvailable Then Reference.new_ref
      UpdateSiteMenu 0
      UpdateFileMenu LongGidFile
      mnuSite.Enabled = True
      mnuGO.Enabled = True
      mnuRefreshModules.Enabled = False
      DrawSite Sites(currentSite), pic, False
    End If
    Exit Sub
  Else
    Exit Sub
  End If
DriveNotFound:
    MsgBox "Failed to open file!" & vbCrLf & "Drive not found", vbExclamation
End Sub

Private Sub mnuRefreshModules_Click()
  ReadModules
  LoadModuleGroupIconInfo
  UpdateModuleGroupIconInfo
  UpdateFileTypeInfo
  LoadTreeView
End Sub

Private Sub mnuExit_Click()
  Unload Me
End Sub

'===========================================================
' siteIdx menu click events
'===========================================================

Private Sub mnuSite_Click()
  If ModuleIsExecuting Then Exit Sub
  mnuNewSite.Enabled = NumSites < 5
End Sub

Sub mnuNewSite_Click()
  If ModuleIsExecuting Then Exit Sub
  Sites(NumSites).name = InputBox("Site name", "Create new site", "Site " & CStr(NumSites + 1))
  If Sites(NumSites).name = "" Then Exit Sub
  mnuSites(NumSites).Caption = Sites(NumSites).name
  mnuSites(NumSites).Checked = True
  mnuSites(NumSites).Visible = True
  mnuSites(currentSite).Checked = False
  currentSite = NumSites
  NumSites = NumSites + 1
  currentGlyph = 0
  UpdateSiteMenu currentSite
  modified = True
  DrawSite Sites(currentSite), pic, False
End Sub

Private Sub mnuRenSite_Click()
Dim i As Integer
Dim answer As String

  If ModuleIsExecuting Then Exit Sub
  answer = InputBox("New site name", "Rename site", "Site " & CStr(currentSite + 1))
  If answer = "" Then Exit Sub
  Sites(currentSite).name = answer
  UpdateSiteMenu currentSite
  modified = True
  DrawSite Sites(currentSite), pic, False
End Sub

Private Sub mnuDelSite_Click()
Dim i As Integer
Dim answer As Integer

  If ModuleIsExecuting Then Exit Sub
  answer = MsgBox("Are you sure you want to delete " & Sites(currentSite).name & "?", vbYesNo Or vbQuestion)
  If answer = vbNo Then Exit Sub
  If answer = vbYes Then
    For i = currentSite To NumSites - 1
      Sites(i) = Sites(i + 1)
    Next
    currentSite = 0
    currentGlyph = 0
    NumSites = NumSites - 1
    UpdateSiteMenu currentSite
    modified = True
    DrawSite Sites(currentSite), pic, False
  End If
End Sub

Public Sub mnuSites_Click(Index As Integer)
  If ModuleIsExecuting Then Exit Sub
  If Index < 0 Then Exit Sub
  mnuSites(currentSite).Checked = False
  currentSite = Index
  currentGlyph = 0
  mnuSites(currentSite).Checked = True
  DrawSite Sites(currentSite), pic, False
End Sub

Private Sub mnuPrintSite_Click()
  If ModuleIsExecuting Then Exit Sub
  On Error Resume Next
  cdl.CancelError = True
  cdl.ShowPrinter
  If Err <> 0 Then Exit Sub
  DrawSite Sites(currentSite), Printer, True
End Sub

Private Sub mnuPrintAllSites_Click()
Dim i As Long

  If ModuleIsExecuting Then Exit Sub
  On Error Resume Next
  cdl.CancelError = True
  cdl.ShowPrinter
  If Err <> 0 Then Exit Sub
  For i = 0 To NumSites - 1
    DrawSite Sites(i), Printer, True
  Next
End Sub

'===========================================================
' Customize menu click events
'===========================================================

Private Sub mnuShowId_Click()
  mnuShowId.Checked = Not mnuShowId.Checked
  DrawSite Sites(currentSite), pic, False
End Sub

Private Sub mnuShowIcon_Click()
  mnuShowIcon.Checked = Not mnuShowIcon.Checked
  If mnuShowIcon.Checked Then
    tree.Style = 5
  Else
    tree.Style = 4
  End If
End Sub

Private Sub mnuPickFont_Click()
  cdl.CancelError = True
  On Error GoTo fonterror
  cdl.flags = cdlCFScreenFonts
  GetFont
  cdl.ShowFont
  SetFont
  DrawSite Sites(currentSite), pic, False
fonterror:
  cdl.CancelError = False
End Sub

Private Sub mnuPickColor_Click()
  frmSite = currentSite
  frmColors.Show vbModeless, Me
End Sub

Private Sub mnuShowLogo_Click()
  If Not mnuShowLogo.Checked Then
    If LogoFile = BLANK Then
      mnuSelectLogo_Click
      Exit Sub
    End If
    mnuShowLogo.Checked = True
  Else
    mnuShowLogo.Checked = False
  End If
  ResizeFormLayout
End Sub

Private Sub mnuSelectLogo_Click()
  On Error Resume Next
  ReadIniString "Options", "LogoFile", BLANK, LogoFile
  cdl.DialogTitle = "Select LOGO Graphic"
  cdl.flags = cdlOFNFileMustExist Or cdlOFNHideReadOnly Or cdlOFNNoChangeDir
  cdl.Filter = "JPEG file (*.jpg)|*.jpg|Windows bitmap (*.bmp)|*.bmp|Icon (*.ico)|*.ico|"
  cdl.FilterIndex = 1
  cdl.DefaultExt = "jpg"
  cdl.filename = LogoFile
  cdl.CancelError = True
  cdl.ShowOpen
  If Err.Number <> 0 Then
    Err.Clear
    Exit Sub
  End If
  Set logo.Picture = LoadPicture(cdl.filename)
  If Err.Number <> 0 Then
    mnuShowLogo.Checked = False
    MsgBox "Error loading logo image: " & LogoFile
    LogoFile = BLANK
    Err.Clear
    Exit Sub
  End If
  LogoFile = cdl.filename
  mnuShowLogo.Checked = True
  ResizeFormLayout
End Sub

Private Sub mnuModIcon_Click()
  mnuModIcon.Checked = Not mnuModIcon.Checked
  DrawSite Sites(currentSite), pic, False
End Sub

Private Sub mnuShowMsg_Click()
  mnuShowMsg.Checked = Not mnuShowMsg.Checked
  ResizeFormLayout
End Sub

'===========================================================
' Go menu click events
'===========================================================

Private Sub mnuGO_Click()
  If ModuleIsExecuting Then Exit Sub
  If NumSites < 1 Then Exit Sub
  If Sites(currentSite).NumGlyphs < 1 Then Exit Sub
  If SaveSites(False, False) Then
    DisableProcessEvents
    doAll FUI, currentSite
    EnableProcessEvents
    frm.Show
    SetForegroundWindow frm.hWnd
    frm.picSite.SetFocus
    DrawSite Sites(currentSite), pic, False
  End If
End Sub

'===========================================================
' Help menu click events
'===========================================================

Private Sub mnuHowTo_Click()
Dim link As New CHyperlink

  MousePointer = vbHourglass
  link.LinkURL = App.Path & "\fui.htm"
  link.OpenLink
  MousePointer = vbDefault
End Sub

Private Sub mnuAbout_Click()
  frmAbout.Show 1
End Sub

'===========================================================
' Options menu click events (right click off of workspace)
'===========================================================

Private Sub mnuIConnect_Click(Index As Integer)
  ConnectIcon mnuIConnect(Index).Tag
  DrawSite Sites(currentSite), pic, False
End Sub

Private Sub mnuInfo_Click()
  If SaveSites(False, False) Then
    InfoClick currentSite, currentGlyph
    DrawSite Sites(currentSite), pic, False
  End If
End Sub

Private Sub mnuInput_Click()
  If SaveSites(False, False) Then
    DisableProcessEvents
    doUI currentSite, currentGlyph
    EnableProcessEvents
    DrawSite Sites(currentSite), pic, False
  End If
End Sub

Private Sub mnuRun_Click()
  If SaveSites(False, False) Then
    DisableProcessEvents
    doModule currentSite, currentGlyph
    EnableProcessEvents
    DrawSite Sites(currentSite), pic, False
  End If
End Sub

Private Sub mnuRename_Click()
Dim result As String
  
  result = InputBox("Enter new label for icon", "Rename Module Icon", Sites(currentSite).Glyphs(currentGlyph).label)
  If result <> "" Then
    Sites(currentSite).Glyphs(currentGlyph).label = result
    modified = True
    DrawSite Sites(currentSite), pic, False
  End If
End Sub

Private Sub mnuDelete_Click()
Dim i As Integer
Dim j As Integer
Dim ct As Integer
Dim ans As Integer
  
  ans = MsgBox("Are you sure you want to delete object?", vbYesNo, "Delete Object")
  If ans = vbYes Then
    modified = True
    UpdateDownStreamGlyphs currentSite, currentGlyph, MODULE_OK
    ct = Sites(currentSite).NumGlyphs - 1
    
    'delete constituent section for this site if consituent icon
    If InStr(Sites(currentSite).Glyphs(currentGlyph).name, "con") = 1 Then
      On Error Resume Next
      conList.Remove Sites(currentSite).Glyphs(currentGlyph).name
      On Error GoTo 0
    End If
    
    ' remove the glyphIdx
    For i = currentGlyph + 1 To ct
      Sites(currentSite).Glyphs(i - 1) = Sites(currentSite).Glyphs(i)
    Next
    
    ' adjust other connections
    For i = currentGlyph + 1 To ct
      For j = 0 To ct
        Sites(currentSite).connect(i - 1, j) = Sites(currentSite).connect(i, j)
      Next j
    Next i
    For i = 0 To ct
      For j = currentGlyph + 1 To ct
        Sites(currentSite).connect(i, j - 1) = Sites(currentSite).connect(i, j)
      Next j
    Next i
    
    SiteIdx_Clear currentSite, Sites(currentSite).NumGlyphs - 1
    Sites(currentSite).NumGlyphs = Sites(currentSite).NumGlyphs - 1
    currentGlyph = 0
    DrawSite Sites(currentSite), pic, False
  End If
End Sub

Private Sub mnuViewInput_Click()
Dim retCode As Long
Dim cmd As String
Dim cmdPath As String
  
  If Not getCommandline(currentSite, currentGlyph, 0, "view.exe gid ", cmd, cmdPath) Then Exit Sub
  retCode = LaunchProcess(cmd, cmdPath)
  If retCode <> 0 Then MsgBox "Error: " & CStr(retCode) & Chr(10) & "Trying to spawn: " & cmd
End Sub

Private Sub mnuViewOut_Click(Index As Integer)
Dim idx As Integer
Dim retCode As Long
Dim fnum As Integer
Dim msg As String
Dim fname As String
Dim cmd As String
Dim cmdPath As String
 
  On Error Resume Next
  Clipboard.Clear
  Clipboard.SetData pic.Picture

  idx = getmodIdxByName(mnuViewOut(Index).Caption)
  If idx < 0 Then
    MsgBox "Model does not exist: " & mnuViewOut(Index).Caption
    Exit Sub
  End If
  
  If Not getCommandline(currentSite, currentGlyph, idx, Module(idx).EXEBat & " ", cmd, cmdPath) Then Exit Sub
  Kill TmpTitle & DOT_STAR
  retCode = LaunchProcess(cmd, cmdPath)
  If retCode <> 0 Then
    MsgBox "Error: " & CStr(retCode) & Chr(10) & "Trying to spawn: " & cmd
    Exit Sub
  End If
  fname = TmpTitle & DOT_ERR
  If BLANK <> Dir(fname) Then
    fnum = FreeFile
    Open fname For Input As #fnum
    MsgBox Input(FileLen(fname), #fnum), vbOKOnly, "Error Report from: " & Sites(currentSite).Glyphs(currentGlyph).label & " (" & Sites(currentSite).Glyphs(currentGlyph).name & ")"
    Close #fnum
  End If
  'no outputs are used from viewer
  Kill TmpTitle & DOT_STAR
End Sub

'===========================================================
' Treeview menu click events (right click off workspace)
'===========================================================

Private Sub mnuAddGlyph_Click()
  TreeView1_DblClick
End Sub

Private Sub mnuSelectIcon_Click()
Dim i As Long
Dim nod As Node

  On Error Resume Next
  Set nod = tree.SelectedItem
  If Err <> 0 Then Exit Sub
  If nod.Parent Is Nothing Then Exit Sub
  cdl.DialogTitle = "Select Icon for " & nod.Parent & ":" & nod
  cdl.flags = cdlOFNFileMustExist Or cdlOFNHideReadOnly Or cdlOFNNoChangeDir
  cdl.Filter = "Icon (*.ico)|*.ico|"
  cdl.FilterIndex = 1
  cdl.DefaultExt = "ico"
  cdl.CancelError = True
  cdl.ShowOpen
  If Err <> 0 Then Exit Sub
  Set picIcon.Picture = LoadPicture(cdl.filename)
  If Err <> 0 Then
    MsgBox "Error " & CStr(Err.Number) & " " & Err.Description
    Err.Clear
    Exit Sub
  End If
  Set picIcon.Picture = Nothing
  i = images.ListImages(nod.Key).Index
  images.ListImages.Remove i
  images.ListImages.Add i, nod.Key, LoadPicture(cdl.filename)
  UpdateModuleGroupIcon nod.Key, cdl.filename
  ResizeFormLayout
End Sub

Private Sub form_DragOver(source As Control, x As Single, y As Single, state As Integer)
' Change pointer to no drop.
  If state = 0 Then source.MousePointer = 12
' Use default mouse pointer.
  If state = 1 Then source.MousePointer = 0
End Sub

Private Sub form_GotFocus()
  DrawSite Sites(currentSite), pic, False
End Sub

Private Sub Form_load()
Dim rc As Boolean
Dim tmp As String
Dim cmdline As String
Dim shortPath As String
  
  If App.PrevInstance Then
    MsgBox "FRAMES is currently running!", , "Instance notification"
    End
  End If
  
' Save current directory
  CurrentDir = CurDir

'========================================================================
'========================================================================
' Get working directory ... set if debugging application
'  AppPath = "c:\framesv1\"
'  AppPath = "C:\ARAMSPRG\FRAMES\"
  AppPath = App.Path + "\"
'========================================================================
'========================================================================
  FRAMES_INI = App.Path + "\\FramesUI.ini"

' Set working directory
  ReadIniString "App Path", FUI, "", tmp
  If tmp <> AppPath Then IgnoreGroupInfo = True
  WriteIniString "App Path", FUI, AppPath
  shortPath = GetShortName(AppPath)
  WriteIniString "App Path", FUI & "short", shortPath
  
  ChDrive AppPath
  ChDir AppPath
  SetHelpFile AppPath & "fui.htm"
  FUI_Loading = True
  ReDim Sites(0)
  GetOptions Me
  mnuRefreshModules_Click
  FUI_Loading = False
  
  Show
  CloseSites
  ResizeFormLayout
  
' Show caveat
  If BLANK <> Dir(AppPath & "caveat.txt") Then
    rc = ReadIniString("Options", "ShowNotice", "true", tmp)
    If (Not rc) Or (rc And tmp = "true") Then
      frmCaveat.Show 1
    End If
  End If
  
' Load file if passed on command line
  cmdline = Command$
  If cmdline <> "" Then
    If Right(cmdline, 1) = """" Then cmdline = Mid(Command$, 2, Len(Command$) - 2) 'remove quotes if any
    tmp = SplitPath(cmdline, SP_DIR)
    If tmp = BLANK Or tmp = ".\" Then cmdline = CurrentDir & "\" & cmdline
    'identify if has path
    If GoodGidName(cmdline) Then
      UpdateFileMenu cmdline
      mnuFileN_Click 0
    End If
  End If
  
  tree.SetFocus
  SendKeys "{PGUP}"
End Sub

Private Sub Form_Resize()
  If ModuleIsExecuting Then Exit Sub
  If WindowState <> 1 Then ' minimized
    ResizeFormLayout
  End If
End Sub

Private Sub Form_Unload(Cancel As Integer)
Dim i As Long

  If ModuleIsExecuting Then
    Cancel = True
    Exit Sub
  End If
  If vbCancel = Save_Changes() Then
    Cancel = True
    Exit Sub
  End If
  CloseSites
  SetOptions
  For i = Forms.Count - 1 To 0 Step -1
    Unload Forms.item(i)
  Next
  
' restore working directory if not a network path indicated by starting "\\"
  If InStr(CurrentDir, "\\") <> 1 Then
    ChDrive CurrentDir
    ChDir CurrentDir
  End If
  Close 'any files that might be open
  End
End Sub

'======================================================
'  Logo - Image1 events
'======================================================

Private Sub Image1_DragOver(source As Control, x As Single, y As Single, state As Integer)
  source.MousePointer = ccNoDrop
End Sub

'======================================================
'  Domain Palette - Treeview1 events
'======================================================

Private Sub TreeView1_DblClick()
Dim i As Long
Dim GrpIdx As Long
Dim name As String
Dim sx As Single
Dim sy As Single
Dim nod As Node
  
  If GidFile = BLANK Then Exit Sub
  Set nod = tree.SelectedItem
  If nod.Parent Is Nothing Then Exit Sub
  GrpIdx = getgrpIdxByPrefix(nod.Key)
  If Group(GrpIdx).Type = DB Then
    For i = 0 To Sites(currentSite).NumGlyphs - 1
      If GrpIdx = Sites(currentSite).Glyphs(i).GrpIdx Then
        MsgBox "Only 1 instance of " & Group(GrpIdx).name & " " & Group(GrpIdx).Type & " can exist in a siteIdx."
        Exit Sub
      End If
    Next
  End If
  FindDropPosition sx, sy
  currentGlyph = SiteIdx_Add(currentSite, GrpIdx, sx, sy)
  DrawSite Sites(currentSite), pic, False
  modified = True
End Sub

Private Sub TreeView1_DragOver(source As Control, x As Single, y As Single, state As Integer)
  source.MousePointer = ccNoDrop
End Sub

Private Sub TreeView1_KeyPress(KeyAscii As Integer)
Dim xx As Single
Dim yy As Single
Dim hItem As Long
Dim lpRect As RECT
Dim nod As Node

  SSPanel1.Visible = False
  If KeyAscii = 13 Then
    If Not ProcessTreeMenuOptions Then Exit Sub
    Set nod = tree.SelectedItem
    hItem = GetTVItemFromNode(TreeView1.hWnd, nod)
    If TreeView_GetItemRect(TreeView1.hWnd, hItem, lpRect, True) = 0 Then lpRect.Left = 0
    xx = (lpRect.Right + 2 * GetSystemMetrics(SM_CXDLGFRAME)) * Screen.TwipsPerPixelX
    yy = TreeView1.Top + ((lpRect.Top) * Screen.TwipsPerPixelY) + ((0.25 * (lpRect.Bottom - lpRect.Top)) * Screen.TwipsPerPixelY)
    PopupMenu mnuTreeViewOptions, , xx, yy
  End If
'  KeyAscii = 0
End Sub

Private Sub TreeView1_MouseUp(Button As Integer, Shift As Integer, x As Single, y As Single)
  If Button = vbRightButton Then
    If Not ProcessTreeMenuOptions Then Exit Sub
    PopupMenu mnuTreeViewOptions, , x, y
  End If
End Sub

Private Sub picSite_DragDrop(source As Control, x As Single, y As Single)
  If Drag Then
    With Sites(currentSite).Glyphs(currentGlyph)
      .sx = x / dx
      .sy = y / dy
      If .sx > 925 Then .sx = 925
      If .sy > 925 Then .sy = 925
      If .sx < 0 Then .sx = 0
      If .sy < 0 Then .sy = 0
    End With
    SSPanel1.Visible = False
    modified = True
    Drag = False
  End If
  DrawSite Sites(currentSite), pic, False
End Sub

Private Sub picSite_DragOver(source As Control, x As Single, y As Single, state As Integer)
  source.MousePointer = ccCross
End Sub

Private Sub picSite_KeyPress(KeyAscii As Integer)
  If ModuleIsExecuting Then Exit Sub
  If Sites(currentSite).NumGlyphs < 1 Then Exit Sub
  If KeyAscii = 13 Then
    DEX = dx * Sites(currentSite).Glyphs(currentGlyph).sx + images.ImageWidth * 1.5 * Screen.TwipsPerPixelX
    DEY = dy * Sites(currentSite).Glyphs(currentGlyph).sy
    ProcessMenuOptions
    PopupMenu mnuOptions, , DEX + rbar + barwidth, DEY + barwidth * 2
  End If
  KeyAscii = 0
End Sub

Private Sub picSite_KeyDown(KeyCode As Integer, Shift As Integer)
Dim i As Long
Dim id As Long
  
  SSPanel1.Visible = False
  If ModuleIsExecuting Then Exit Sub
  If Sites(currentSite).NumGlyphs < 1 Then Exit Sub
  With Sites(currentSite).Glyphs(currentGlyph)
    Select Case KeyCode
      Case 32:
        If Shift = 0 Then  'forward spacebar between glyphs
          id = MAX_GLYPH
          For i = 0 To Sites(currentSite).NumGlyphs - 1
            If .id < Sites(currentSite).Glyphs(i).id Then
              If id > Sites(currentSite).Glyphs(i).id Then
                id = Sites(currentSite).Glyphs(i).id
                currentGlyph = i
              End If
            End If
          Next
          If id = MAX_GLYPH Then
            For i = 0 To Sites(currentSite).NumGlyphs - 1
              If id > Sites(currentSite).Glyphs(i).id Then
                id = Sites(currentSite).Glyphs(i).id
                currentGlyph = i
              End If
            Next
          End If
        Else            'reverse spacebar between glyphs
          id = 0
          For i = 0 To Sites(currentSite).NumGlyphs - 1
            If .id > Sites(currentSite).Glyphs(i).id Then
              If id < Sites(currentSite).Glyphs(i).id Then
                id = Sites(currentSite).Glyphs(i).id
                currentGlyph = i
              End If
            End If
          Next
          If id = 0 Then
            For i = 0 To Sites(currentSite).NumGlyphs - 1
              If id < Sites(currentSite).Glyphs(i).id Then
                id = Sites(currentSite).Glyphs(i).id
                currentGlyph = i
              End If
            Next
          End If
        End If
        DrawSite Sites(currentSite), pic, False
        modified = True
      Case 37:
        .sx = (.sx - 5)
        DrawSite Sites(currentSite), pic, False
        modified = True
      Case 38:
        .sy = (.sy - 5)
        DrawSite Sites(currentSite), pic, False
        modified = True
      Case 39:
        .sx = (.sx + 5)
        DrawSite Sites(currentSite), pic, False
        modified = True
      Case 40:
        .sy = (.sy + 5)
        DrawSite Sites(currentSite), pic, False
        modified = True
      Case 46:
        mnuDelete_Click
    End Select
    If .sx > 925 Then .sx = 925
    If .sy > 925 Then .sy = 925
    If .sx < 0 Then .sx = 0
    If .sy < 0 Then .sy = 0
  End With
End Sub

Private Sub picSite_MouseDown(Button As Integer, Shift As Integer, x As Single, y As Single)
Dim ClosestGlyph As Long

  If ModuleIsExecuting Then Exit Sub
  ClosestGlyph = Closest(currentSite, x * 1000# / pic.ScaleWidth, y * 1000# / pic.ScaleHeight)
  If ClosestGlyph > -1 Then
    SSPanel1.Visible = False
    currentGlyph = ClosestGlyph
    DrawSite Sites(currentSite), pic, False
    ' find center for line draw
    DSX = dx * Sites(currentSite).Glyphs(currentGlyph).sx + images.ImageWidth * 0.9 * Screen.TwipsPerPixelX
    DSY = dy * Sites(currentSite).Glyphs(currentGlyph).sy + images.ImageHeight * 0.5 * Screen.TwipsPerPixelY
    DEX = DSX
    DEY = DSY
    If Button = 2 And Shift = 0 Then
      ProcessMenuOptions
      PopupMenu mnuOptions
    Else
      Drag = True
    End If
  End If
End Sub

Private Sub picSite_MouseMove(Button As Integer, Shift As Integer, x As Single, y As Single)
Dim ClosestGlyph As Long
  
  If ModuleIsExecuting Then Exit Sub
  If Abs(lastX - x) < 20 And Abs(lastY - y) < 20 And Not Drag Then
    ClosestGlyph = Closest(currentSite, x * 1000# / pic.ScaleWidth, y * 1000# / pic.ScaleHeight)
    If ClosestGlyph > -1 Then
      DEX = dx * Sites(currentSite).Glyphs(ClosestGlyph).sx + images.ImageWidth * Screen.TwipsPerPixelX * 0.4
      DEY = dy * Sites(currentSite).Glyphs(ClosestGlyph).sy + images.ImageHeight * Screen.TwipsPerPixelY
      SSPanel1.Move DEX + rbar + barwidth, DEY + barwidth * 2
      SSPanel1.Caption = Sites(currentSite).Glyphs(ClosestGlyph).Model
      SSPanel1.Visible = SSPanel1.Caption <> "" And frm.hWnd = GetForegroundWindow()
      If SSPanel1.Visible Then picSite.SetFocus
    Else
      SSPanel1.Visible = False
    End If
  End If
  lastX = x
  lastY = y
  
  If Button = 1 And Shift = 1 And Drag Then
    ' hides previous line
    pic.DrawMode = 7
    pic.Line (DSX, DSY)-(DEX, DEY), wfcolor
    DEX = x
    DEY = y
    ' draws new line
    pic.Line (DSX, DSY)-(DEX, DEY), wfcolor
    pic.DrawMode = 13
  End If
  
  If Button = 1 And Shift = 0 And Drag Then
    DEX = dx * Sites(currentSite).Glyphs(currentGlyph).sx
    DEY = dy * Sites(currentSite).Glyphs(currentGlyph).sy
    ' move box releative icon
    tmpLabel.Move DEX + rbar + barwidth, DEY + barwidth * 2, 0, 0
    ' drag forces the mouse to move to the upper left
    tmpLabel.Drag
    ' adjust box to be full size
    tmpLabel.Move DEX + rbar + barwidth, DEY + barwidth * 2, images.ImageWidth * Screen.TwipsPerPixelX * 1.4, images.ImageHeight * Screen.TwipsPerPixelY
    ' now deag with full size box
    tmpLabel.Drag
  End If
End Sub

Private Sub picSite_MouseUp(Button As Integer, Shift As Integer, x As Single, y As Single)
Dim NextGlyph As Long

  If ModuleIsExecuting Then Exit Sub
  If Button = 1 And Shift = 1 And Drag Then 'connect modules
    NextGlyph = Closest(currentSite, x * 1000# / pic.ScaleWidth, y * 1000# / pic.ScaleHeight)
    If currentGlyph <> NextGlyph And NextGlyph > -1 Then
      ConnectIcon NextGlyph
    End If
    Drag = False
  End If
  DrawSite Sites(currentSite), pic, False
End Sub

Private Sub RPanel_MouseDown(Button As Integer, Shift As Integer, x As Single, y As Single)
  If Button = 1 And RPanel.MousePointer = vbSizeWE Then vertical = True
  If Button = 1 And RPanel.MousePointer = vbSizeNS Then horizontal = True
End Sub

Private Sub RPanel_MouseUp(Button As Integer, Shift As Integer, x As Single, y As Single)
  If Button = 1 Then
    If horizontal = True Then
      tbar(hbar) = bar.Top
      bbar(hbar) = bar.Top + barwidth
      bar.Visible = False
      horizontal = False
      ResizeFormLayout
    End If
    If vertical = True Then
      lbar = bar.Left
      rbar = bar.Left + barwidth
      bar.Visible = False
      vertical = False
      ResizeFormLayout
    End If
    RPanel.MousePointer = vbDefault
  End If
End Sub

Private Sub RPanel_MouseMove(Button As Integer, Shift As Integer, x As Single, y As Single)
  If Button = 0 Then
    If lbar < x And x < rbar Then
      RPanel.MousePointer = vbSizeWE
    ElseIf tbar(0) < y And y < bbar(0) Then
      hbar = 0
      RPanel.MousePointer = vbSizeNS
    ElseIf tbar(1) < y And y < bbar(1) Then
      hbar = 1
      RPanel.MousePointer = vbSizeNS
    End If
  End If
  
  If Button = 1 Then
    If horizontal Then
      If hbar = 0 Then
        If y < 0.1 * RPanel.Height Then
          bar.Top = 0.1 * RPanel.Height
        ElseIf y > 0.5 * RPanel.Height Then
          bar.Top = 0.5 * RPanel.Height
        Else
          bar.Top = y
        End If
        bar.Left = 0
        bar.Width = rbar
      End If
      If hbar = 1 Then
        If y < 0.6 * RPanel.Height Then
          bar.Top = 0.6 * RPanel.Height
        ElseIf y > 0.95 * RPanel.Height Then
          bar.Top = 0.95 * RPanel.Height
        Else
          bar.Top = y
        End If
        bar.Left = lbar
        bar.Width = RPanel.Width - lbar
      End If
      bar.Height = barwidth
      bar.Visible = True
    End If
    If vertical Then
      If x < 0.01 * RPanel.Width Then
        bar.Left = 0.01 * RPanel.Width
      ElseIf x > 0.5 * RPanel.Width Then
        bar.Left = 0.5 * RPanel.Width
      Else
        bar.Left = x
      End If
      bar.Top = 0
      bar.Width = barwidth
      bar.Height = RPanel.Height
      bar.Visible = True
    End If
  End If
End Sub

Sub ResizeFormLayout()
Dim yborder As Long
Dim xborder As Long

  If FUI_Loading Then Exit Sub
  xborder = RPanel.BevelWidth * Screen.TwipsPerPixelX
  yborder = RPanel.BevelWidth * Screen.TwipsPerPixelY
  
  RPanel.Visible = False
  If Me.Width < rbar + 128 * Screen.TwipsPerPixelX Then
    Me.Width = rbar + 128 * Screen.TwipsPerPixelX
  End If
  If Me.Height < 128 * Screen.TwipsPerPixelY Then
    Me.Height = 128 * Screen.TwipsPerPixelY
  End If
  
  RPanel.Width = Me.ScaleWidth
  RPanel.Height = Me.ScaleHeight - StatusBar1.Height
  
  If RPanel.Height * 0.1 > bbar(0) Or _
     RPanel.Height * 0.51 < bbar(0) Then
    tbar(0) = 0.2 * RPanel.Height
    bbar(0) = tbar(0) + barwidth
  End If
  If RPanel.Height * 0.6 > bbar(1) Or _
     RPanel.Height * 0.96 < bbar(1) Then
    tbar(1) = 0.8 * RPanel.Height
    bbar(1) = tbar(1) + barwidth
  End If

  logo.Left = xborder
  logo.Width = lbar - xborder
  logo.Top = yborder
  logo.Height = tbar(0) - yborder
  
  tree.Left = yborder
  tree.Width = lbar - xborder
  If mnuShowLogo.Checked Then
    tree.Top = bbar(0)
    tree.Height = RPanel.Height - (bbar(0) + yborder)
    logo.Visible = True
  Else
    tree.Top = yborder
    tree.Height = RPanel.Height - (2 * yborder)
    logo.Visible = False
  End If
    
  rtb.Left = rbar
  rtb.Width = RPanel.Width - (rbar + xborder)
  rtb.Top = bbar(1)
  rtb.Height = RPanel.Height - (bbar(1) + yborder)
  
  pic.Left = rbar
  pic.Width = RPanel.Width - (rbar + xborder)
  pic.Top = yborder
  If mnuShowMsg.Checked Then
    pic.Height = tbar(1) - yborder
  Else
    pic.Height = RPanel.Height - (2 * yborder)
  End If
  rtb.Visible = mnuShowMsg.Checked
  rtb.Enabled = mnuShowMsg.Checked
  
  DrawSite Sites(currentSite), pic, False
  RPanel.Visible = True
End Sub

Function ProcessTreeMenuOptions() As Boolean
Dim nod As Node

  ProcessTreeMenuOptions = False
  Set nod = tree.SelectedItem
  If nod.Parent Is Nothing Then Exit Function
  mnuAddGlyph.Visible = Not (GidFile = BLANK)
  mnuAddGlyph.Caption = "Add '" & nod & "' to site"
  mnuSelectIcon.Caption = "Change '" & nod & "' Icon"
  ProcessTreeMenuOptions = True
End Function

Sub ProcessMenuOptions()
Dim inc As Integer
Dim ctype As String
Dim imdl As Boolean  'indicates there is a UI
Dim rmdl As Boolean  'indicates there is a model
Dim cls As String
Dim msel() As Boolean
Dim mct As Long
Dim i As Long
  
  If ModuleIsExecuting Then Exit Sub
  mnuInfo.Enabled = True
  mnuInput.Enabled = False
  mnuRun.Enabled = False
  mnuViewInput.Enabled = False
  mnuViewOutput.Enabled = False
  
  For i = mnuIConnect.Count To 2 Step -1
    Unload mnuIConnect(i)
  Next i
  mct = 0
  For i = 0 To Sites(currentSite).NumGlyphs - 1
    If Sites(currentSite).Glyphs(currentGlyph).id <> Sites(currentSite).Glyphs(i).id Then
      mct = mct + 1
      If mct > 1 Then Load mnuIConnect(mct)
      mnuIConnect(mct).Visible = True
      mnuIConnect(mct).Caption = Sites(currentSite).Glyphs(i).label & " (" & Sites(currentSite).Glyphs(i).name & ")"
      mnuIConnect(mct).Tag = i
    End If
  Next i
  If mct > 0 Then mnuConnect.Enabled = True
  
  
  If Sites(currentSite).Glyphs(currentGlyph).state = NO_MODULE Then Exit Sub
  imdl = hasPassive(currentSite, currentGlyph)
  rmdl = hasActive(currentSite, currentGlyph)
  If imdl Then
    mnuInput.Enabled = RequisiteInputComplete(currentSite, currentGlyph)
  End If
  
  If rmdl And ((imdl And mnuInput.Enabled) Or (Not imdl)) Then
    inc = RequisiteRunsIncomplete(currentSite, currentGlyph)
    If inc < 0 Then
      mnuRun.Enabled = True
      If mnuInput.Enabled Then
        ' enable run only if input is required and complete
        mnuRun.Enabled = Sites(currentSite).Glyphs(currentGlyph).state >= INPUT_OK
      End If
    End If
  End If
  
  mnuViewInput.Enabled = Sites(currentSite).Glyphs(currentGlyph).state >= INPUT_OK
  If Sites(currentSite).Glyphs(currentGlyph).state >= RUN_OK Then
    GetMatchingViewers currentSite, currentGlyph, cls, msel()
    For i = mnuViewOut.Count To 2 Step -1
      Unload mnuViewOut(i)
    Next i
    mct = 0
    For i = 0 To UBound(msel)
      If msel(i) Then
        If Group(Module(i).GrpIdx).Prefix = cls Then
          mct = mct + 1
          If mct > 1 Then Load mnuViewOut(mct)
          mnuViewOut(mct).Visible = True
          mnuViewOut(mct).Caption = Module(i).name
        End If
      End If
    Next i
    If mct > 0 Then mnuViewOutput.Enabled = True
  End If
End Sub

Private Sub LoadTreeView()
Dim i As Long
Dim Typ As String
Dim imKey As String
Dim errmsg As String
Dim nodX As Node
  
  ' April 2001 - Viewers can now be accessed from the module popupmenu so they are excluded from the treeview list - BLH
  
  ' images is initialized with default icons for open/closed folder and unknown icon (?)
  ' replace default treeview icons with the icons from the .ini file by key (Group,type)

  'update imagelist for group icons
  tree.Visible = False
  errmsg = ""
  For i = 0 To GroupCount - 1
    If Group(i).icoPath <> BLANK Then

On Error GoTo IconNotFound1
      If Dir(Group(i).icoPath) <> BLANK Then

On Error Resume Next
        images.ListImages.Remove Group(i).Prefix
        If Err.Number > 0 Then Err.Clear
        images.ListImages.Add , Group(i).Prefix, LoadPicture(Group(i).icoPath)
        If Err.Number > 0 Then
          errmsg = errmsg & Err.Description & vbCrLf & "Unable to load icon " & Group(i).icoPath & vbCrLf
          Err.Clear
        End If
      Else
      
IconNotFound1:
        errmsg = errmsg & "Unable to locate icon " & Group(i).icoPath & vbCrLf
      End If
    End If
  Next
  
  'update imagelist for module icons
  For i = 0 To ModuleCount - 1
    If Module(i).icoPath <> BLANK Then

On Error GoTo IconNotFound2
      If Dir(Module(i).icoPath) <> BLANK Then

On Error Resume Next
        images.ListImages.Remove Module(i).filename
        If Err.Number > 0 Then Err.Clear
        images.ListImages.Add , Module(i).filename, LoadPicture(Module(i).icoPath)
        If Err.Number > 0 Then
          errmsg = errmsg & Err.Description & vbCrLf & "Unable to load icon " & Module(i).icoPath & " specified by " & Module(i).filename & vbCrLf
          Err.Clear
        End If
      Else
        
IconNotFound2:
        errmsg = errmsg & "Unable to locate icon " & Module(i).icoPath & " specified by " & Module(i).filename & vbCrLf
      End If
    End If
  Next
  
' Save out icons
'  For i = 1 To images.ListImages.Count
'    On Error Resume Next
'    SavePicture images.ListImages(i).Picture, images.ListImages(i).Tag
'    If Err Then MsgBox Error & ": " & images.ListImages(i).Tag
'  Next
  
  'Type = Database, Model, Viewer, System, any others are considered Model
  'Add types, as root nodes, to treeview from actual DES files
On Error Resume Next
  
  tree.Nodes.Clear
  For i = 0 To GroupCount - 1
    Typ = Group(i).Type
    If Typ <> VWR Then
      Set nodX = tree.Nodes.Add(, , Typ, Typ)
      If Err.Number = 0 Then
        nodX.Sorted = True
        nodX.Image = "CloseFolder"
        nodX.ExpandedImage = "OpenFolder"
        nodX.EnsureVisible
      Else
        'The only errors that can ever occur is duplicate key and out of memory
        'errmsg = errmsg & Err.Description & vbCrLf & _
        '         "Trying to add " & Group(i).Type & vbCrLf
        Err.Clear
      End If
    End If
  Next
  
  For i = 1 To GroupCount - 1
    If Group(i).Type <> VWR Then
      Set nodX = tree.Nodes.Add(Group(i).Type, tvwChild, Group(i).Prefix, Group(i).name, Group(i).Prefix)
      If Err.Number = 0 Then
        nodX.EnsureVisible
      Else
        ' if there was an error then try adding with a unknown icon [?]
        Err.Clear
        Set nodX = tree.Nodes.Add(Group(i).Type, tvwChild, Group(i).Prefix, Group(i).name, "unknown")
        If Err.Number = 0 Then
          nodX.EnsureVisible
        Else
          errmsg = errmsg & Err.Description & vbCrLf & _
                   "Trying to add " & Group(i).Type & ":" & Group(i).name & ":" & Group(i).Prefix & vbCrLf
          Err.Clear
        End If
      End If
    End If
  Next
  If errmsg <> "" Then MsgBox errmsg
  tree.Visible = True
End Sub
