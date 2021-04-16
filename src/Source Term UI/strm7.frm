VERSION 5.00
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "threed32.ocx"
Object = "{F856EC8B-F03C-4515-BDC6-64CBD617566A}#7.0#0"; "FPSPR70.ocx"
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "tabctl32.ocx"
Begin VB.Form frmMain 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Source Term Module Input"
   ClientHeight    =   6648
   ClientLeft      =   4428
   ClientTop       =   4020
   ClientWidth     =   9504
   ControlBox      =   0   'False
   Icon            =   "strm7.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   6648
   ScaleWidth      =   9504
   Begin Threed.SSPanel panLeft 
      Height          =   300
      Left            =   15
      TabIndex        =   11
      Top             =   6315
      Width           =   5520
      _Version        =   65536
      _ExtentX        =   9737
      _ExtentY        =   529
      _StockProps     =   15
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   7.8
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BevelOuter      =   1
      Alignment       =   1
   End
   Begin Threed.SSPanel panRight 
      Height          =   300
      Left            =   5595
      TabIndex        =   12
      Top             =   6315
      Width           =   3870
      _Version        =   65536
      _ExtentX        =   6826
      _ExtentY        =   529
      _StockProps     =   15
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   7.8
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BevelOuter      =   1
      Alignment       =   1
   End
   Begin TabDlg.SSTab SSTab1 
      Height          =   6240
      Left            =   60
      TabIndex        =   10
      Top             =   15
      Width           =   9420
      _ExtentX        =   16616
      _ExtentY        =   11007
      _Version        =   393216
      Tabs            =   10
      TabsPerRow      =   5
      TabHeight       =   794
      TabCaption(0)   =   "&Options"
      TabPicture(0)   =   "strm7.frx":030A
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "Picture1(0)"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).ControlCount=   1
      TabCaption(1)   =   "&Waste Zone"
      TabPicture(1)   =   "strm7.frx":0326
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "Picture1(1)"
      Tab(1).Control(0).Enabled=   0   'False
      Tab(1).ControlCount=   1
      TabCaption(2)   =   "O&verland"
      TabPicture(2)   =   "strm7.frx":0342
      Tab(2).ControlEnabled=   0   'False
      Tab(2).Control(0)=   "Picture1(2)"
      Tab(2).Control(0).Enabled=   0   'False
      Tab(2).ControlCount=   1
      TabCaption(3)   =   "&Suspension"
      TabPicture(3)   =   "strm7.frx":035E
      Tab(3).ControlEnabled=   0   'False
      Tab(3).Control(0)=   "Picture1(3)"
      Tab(3).Control(0).Enabled=   0   'False
      Tab(3).ControlCount=   1
      TabCaption(4)   =   "&Hydrology"
      TabPicture(4)   =   "strm7.frx":037A
      Tab(4).ControlEnabled=   0   'False
      Tab(4).Control(0)=   "Picture1(4)"
      Tab(4).Control(0).Enabled=   0   'False
      Tab(4).ControlCount=   1
      TabCaption(5)   =   "Monthly &Climatology"
      TabPicture(5)   =   "strm7.frx":0396
      Tab(5).ControlEnabled=   0   'False
      Tab(5).Control(0)=   "Picture1(5)"
      Tab(5).Control(0).Enabled=   0   'False
      Tab(5).ControlCount=   1
      TabCaption(6)   =   "&Kd's"
      TabPicture(6)   =   "strm7.frx":03B2
      Tab(6).ControlEnabled=   0   'False
      Tab(6).Control(0)=   "Picture1(6)"
      Tab(6).Control(0).Enabled=   0   'False
      Tab(6).ControlCount=   1
      TabCaption(7)   =   "Constituent &Properties"
      TabPicture(7)   =   "strm7.frx":03CE
      Tab(7).ControlEnabled=   0   'False
      Tab(7).Control(0)=   "Picture1(7)"
      Tab(7).Control(0).Enabled=   0   'False
      Tab(7).ControlCount=   1
      TabCaption(8)   =   "Known &Media Releases"
      TabPicture(8)   =   "strm7.frx":03EA
      Tab(8).ControlEnabled=   0   'False
      Tab(8).Control(0)=   "Picture1(8)"
      Tab(8).Control(0).Enabled=   0   'False
      Tab(8).ControlCount=   1
      TabCaption(9)   =   "Known Constituent F&lux"
      TabPicture(9)   =   "strm7.frx":0406
      Tab(9).ControlEnabled=   0   'False
      Tab(9).Control(0)=   "Picture1(9)"
      Tab(9).Control(0).Enabled=   0   'False
      Tab(9).ControlCount=   1
      Begin VB.PictureBox Picture1 
         Height          =   5190
         Index           =   9
         Left            =   -74925
         ScaleHeight     =   5148
         ScaleWidth      =   9072
         TabIndex        =   23
         TabStop         =   0   'False
         Top             =   960
         Width           =   9120
         Begin FPSpreadADO.fpSpread ssTab 
            Height          =   5040
            Index           =   9
            Left            =   45
            TabIndex        =   9
            Top             =   45
            Width           =   8970
            _Version        =   458752
            _ExtentX        =   15822
            _ExtentY        =   8890
            _StockProps     =   64
            AllowCellOverflow=   -1  'True
            BackColorStyle  =   1
            ColHeaderDisplay=   0
            DisplayRowHeaders=   0   'False
            EditModeReplace =   -1  'True
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            MaxCols         =   10
            MaxRows         =   50
            ScrollBars      =   2
            SelectBlockOptions=   8
            ShadowColor     =   8421376
            SpreadDesigner  =   "strm7.frx":0422
            VisibleCols     =   500
            VisibleRows     =   500
         End
      End
      Begin VB.PictureBox Picture1 
         Height          =   5190
         Index           =   8
         Left            =   -74925
         ScaleHeight     =   5148
         ScaleWidth      =   9072
         TabIndex        =   22
         TabStop         =   0   'False
         Top             =   960
         Width           =   9120
         Begin FPSpreadADO.fpSpread ssTab 
            Height          =   5040
            Index           =   8
            Left            =   45
            TabIndex        =   8
            Top             =   45
            Width           =   8970
            _Version        =   458752
            _ExtentX        =   15822
            _ExtentY        =   8890
            _StockProps     =   64
            AllowCellOverflow=   -1  'True
            BackColorStyle  =   1
            ColHeaderDisplay=   0
            DisplayRowHeaders=   0   'False
            EditModeReplace =   -1  'True
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            MaxCols         =   10
            MaxRows         =   50
            ScrollBars      =   2
            SelectBlockOptions=   8
            ShadowColor     =   8421376
            SpreadDesigner  =   "strm7.frx":394D
            VisibleCols     =   500
            VisibleRows     =   500
         End
      End
      Begin VB.PictureBox Picture1 
         Height          =   5190
         Index           =   7
         Left            =   -74925
         ScaleHeight     =   5148
         ScaleWidth      =   9072
         TabIndex        =   21
         TabStop         =   0   'False
         Top             =   960
         Width           =   9120
         Begin FPSpreadADO.fpSpread ssTab 
            Height          =   5040
            Index           =   7
            Left            =   45
            TabIndex        =   7
            Top             =   45
            Width           =   8970
            _Version        =   458752
            _ExtentX        =   15822
            _ExtentY        =   8890
            _StockProps     =   64
            AllowCellOverflow=   -1  'True
            BackColorStyle  =   1
            ColHeaderDisplay=   0
            DisplayRowHeaders=   0   'False
            EditModeReplace =   -1  'True
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            MaxCols         =   10
            MaxRows         =   50
            ScrollBars      =   2
            SelectBlockOptions=   8
            ShadowColor     =   8421376
            SpreadDesigner  =   "strm7.frx":6E78
            VisibleCols     =   500
            VisibleRows     =   500
         End
      End
      Begin VB.PictureBox Picture1 
         Height          =   5190
         Index           =   6
         Left            =   -74925
         ScaleHeight     =   5148
         ScaleWidth      =   9072
         TabIndex        =   19
         TabStop         =   0   'False
         Top             =   960
         Width           =   9120
         Begin FPSpreadADO.fpSpread ssTab 
            Height          =   4590
            Index           =   6
            Left            =   45
            TabIndex        =   6
            Top             =   495
            Width           =   8970
            _Version        =   458752
            _ExtentX        =   15822
            _ExtentY        =   8096
            _StockProps     =   64
            AllowCellOverflow=   -1  'True
            BackColorStyle  =   1
            ColHeaderDisplay=   0
            DisplayRowHeaders=   0   'False
            EditModeReplace =   -1  'True
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            MaxCols         =   10
            MaxRows         =   50
            ScrollBars      =   2
            ShadowColor     =   8421376
            SpreadDesigner  =   "strm7.frx":A3A3
            VisibleCols     =   500
            VisibleRows     =   500
         End
         Begin VB.CommandButton cmdEstKd 
            Caption         =   "Soil Properties"
            Height          =   312
            Left            =   6048
            TabIndex        =   20
            Top             =   132
            Width           =   2172
         End
      End
      Begin VB.PictureBox Picture1 
         Height          =   5190
         Index           =   5
         Left            =   -74925
         ScaleHeight     =   5148
         ScaleWidth      =   9072
         TabIndex        =   18
         TabStop         =   0   'False
         Top             =   960
         Width           =   9120
         Begin FPSpreadADO.fpSpread ssTab 
            Height          =   5040
            Index           =   5
            Left            =   45
            TabIndex        =   5
            Top             =   45
            Width           =   8970
            _Version        =   458752
            _ExtentX        =   15822
            _ExtentY        =   8890
            _StockProps     =   64
            AllowCellOverflow=   -1  'True
            BackColorStyle  =   1
            ColHeaderDisplay=   0
            DisplayRowHeaders=   0   'False
            EditModeReplace =   -1  'True
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            MaxCols         =   10
            MaxRows         =   50
            ScrollBars      =   2
            ShadowColor     =   8421376
            SpreadDesigner  =   "strm7.frx":D8C6
            VisibleCols     =   500
            VisibleRows     =   500
         End
      End
      Begin VB.PictureBox Picture1 
         Height          =   5190
         Index           =   4
         Left            =   -74925
         ScaleHeight     =   5148
         ScaleWidth      =   9072
         TabIndex        =   17
         TabStop         =   0   'False
         Top             =   960
         Width           =   9120
         Begin FPSpreadADO.fpSpread ssTab 
            Height          =   5040
            Index           =   4
            Left            =   45
            TabIndex        =   4
            Top             =   45
            Width           =   8970
            _Version        =   458752
            _ExtentX        =   15822
            _ExtentY        =   8890
            _StockProps     =   64
            AllowCellOverflow=   -1  'True
            BackColorStyle  =   1
            ColHeaderDisplay=   0
            DisplayRowHeaders=   0   'False
            EditModeReplace =   -1  'True
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            MaxCols         =   10
            MaxRows         =   50
            ScrollBars      =   2
            ShadowColor     =   8421376
            SpreadDesigner  =   "strm7.frx":10DE9
            VisibleCols     =   500
            VisibleRows     =   500
         End
      End
      Begin VB.PictureBox Picture1 
         Height          =   5190
         Index           =   3
         Left            =   -74925
         ScaleHeight     =   5148
         ScaleWidth      =   9072
         TabIndex        =   16
         TabStop         =   0   'False
         Top             =   960
         Width           =   9120
         Begin FPSpreadADO.fpSpread ssTab 
            Height          =   5040
            Index           =   3
            Left            =   45
            TabIndex        =   3
            Top             =   45
            Width           =   8970
            _Version        =   458752
            _ExtentX        =   15822
            _ExtentY        =   8890
            _StockProps     =   64
            AllowCellOverflow=   -1  'True
            BackColorStyle  =   1
            ColHeaderDisplay=   0
            DisplayRowHeaders=   0   'False
            EditModeReplace =   -1  'True
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            MaxCols         =   10
            MaxRows         =   50
            ScrollBars      =   2
            ShadowColor     =   8421376
            SpreadDesigner  =   "strm7.frx":1430C
            VisibleCols     =   500
            VisibleRows     =   500
         End
      End
      Begin VB.PictureBox Picture1 
         Height          =   5190
         Index           =   2
         Left            =   -74925
         ScaleHeight     =   5148
         ScaleWidth      =   9072
         TabIndex        =   15
         TabStop         =   0   'False
         Top             =   960
         Width           =   9120
         Begin FPSpreadADO.fpSpread ssTab 
            Height          =   5040
            Index           =   2
            Left            =   45
            TabIndex        =   2
            Top             =   45
            Width           =   8970
            _Version        =   458752
            _ExtentX        =   15822
            _ExtentY        =   8890
            _StockProps     =   64
            AllowCellOverflow=   -1  'True
            BackColorStyle  =   1
            ColHeaderDisplay=   0
            DisplayRowHeaders=   0   'False
            EditModeReplace =   -1  'True
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            MaxCols         =   10
            MaxRows         =   50
            ScrollBars      =   2
            ShadowColor     =   8421376
            SpreadDesigner  =   "strm7.frx":1782F
            VisibleCols     =   500
            VisibleRows     =   500
         End
      End
      Begin VB.PictureBox Picture1 
         Height          =   5190
         Index           =   1
         Left            =   -74925
         ScaleHeight     =   5148
         ScaleWidth      =   9072
         TabIndex        =   14
         TabStop         =   0   'False
         Top             =   960
         Width           =   9120
         Begin FPSpreadADO.fpSpread ssTab 
            Height          =   5040
            Index           =   1
            Left            =   45
            TabIndex        =   1
            Top             =   45
            Width           =   8970
            _Version        =   458752
            _ExtentX        =   15822
            _ExtentY        =   8890
            _StockProps     =   64
            AllowCellOverflow=   -1  'True
            BackColorStyle  =   1
            ColHeaderDisplay=   0
            DisplayRowHeaders=   0   'False
            EditModeReplace =   -1  'True
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            MaxCols         =   10
            MaxRows         =   50
            ScrollBars      =   2
            ShadowColor     =   8421376
            SpreadDesigner  =   "strm7.frx":1AD52
            VisibleCols     =   500
            VisibleRows     =   500
         End
      End
      Begin VB.PictureBox Picture1 
         Height          =   5190
         Index           =   0
         Left            =   75
         ScaleHeight     =   5148
         ScaleWidth      =   9072
         TabIndex        =   13
         TabStop         =   0   'False
         Top             =   960
         Width           =   9120
         Begin FPSpreadADO.fpSpread ssTab 
            Height          =   5040
            Index           =   0
            Left            =   45
            TabIndex        =   0
            Top             =   45
            Width           =   8970
            _Version        =   458752
            _ExtentX        =   15822
            _ExtentY        =   8890
            _StockProps     =   64
            AllowCellOverflow=   -1  'True
            BackColorStyle  =   1
            ColHeaderDisplay=   0
            DisplayRowHeaders=   0   'False
            EditModeReplace =   -1  'True
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            MaxCols         =   10
            MaxRows         =   50
            ScrollBars      =   2
            ShadowColor     =   8421376
            ShadowText      =   -2147483641
            SpreadDesigner  =   "strm7.frx":1E275
            VisibleCols     =   500
            VisibleRows     =   500
         End
      End
   End
   Begin VB.Label lblRef 
      BackColor       =   &H000000FF&
      Caption         =   "Label1"
      Height          =   450
      Left            =   15
      TabIndex        =   24
      Top             =   5805
      Visible         =   0   'False
      Width           =   6345
   End
   Begin VB.Menu mnuFile 
      Caption         =   "&File"
      NegotiatePosition=   3  'Right
      Begin VB.Menu mnuDiscard 
         Caption         =   "&Undo Changes"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuSep1 
         Caption         =   "-"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuExitSave 
         Caption         =   "&Save and Exit"
      End
      Begin VB.Menu mnuExitDiscard 
         Caption         =   "E&xit"
      End
   End
   Begin VB.Menu mnuRef 
      Caption         =   "&Reference"
      Begin VB.Menu mnuRefAdd 
         Caption         =   "&Add"
      End
      Begin VB.Menu mnuRefSel 
         Caption         =   "Se&lect"
      End
   End
   Begin VB.Menu NumFormat 
      Caption         =   "For&mat"
      Visible         =   0   'False
      Begin VB.Menu NumGeneral 
         Caption         =   "General"
      End
      Begin VB.Menu NumStandard 
         Caption         =   "Standard"
      End
      Begin VB.Menu NumFixed 
         Caption         =   "Fixed"
      End
      Begin VB.Menu NumScientific 
         Caption         =   "Scientific"
      End
      Begin VB.Menu NumUser 
         Caption         =   "User Defined"
      End
   End
   Begin VB.Menu mnuHelp 
      Caption         =   "&Help"
      NegotiatePosition=   3  'Right
      Begin VB.Menu howto 
         Caption         =   "How &to..."
      End
      Begin VB.Menu mnuSep2 
         Caption         =   "-"
      End
      Begin VB.Menu mnuHelpAbout 
         Caption         =   "&About"
      End
   End
   Begin VB.Menu SpreadEdit 
      Caption         =   "SpreadEdit"
      Visible         =   0   'False
      Begin VB.Menu SpreadCopy 
         Caption         =   "Copy         Ctrl+C"
      End
      Begin VB.Menu SpreadCut 
         Caption         =   "Cut             Ctrl+X"
      End
      Begin VB.Menu SpreadPaste 
         Caption         =   "Paste         Ctrl+V"
      End
   End
End
Attribute VB_Name = "frmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text
Dim FormLoaded As Boolean

Sub GetParamHelp(KeyCode As Integer)
  If KeyCode = vbKeyF1 Then
    howto_Click
  End If
End Sub

Sub PopulateForm()
  vfProcessUserInput = False
  SetTabEnabled 0, True
  Refresh
End Sub

Private Sub cmdEstKd_Click()
  MousePointer = vbHourglass
  EstimateKd
  MousePointer = vbDefault
End Sub

Private Sub Form_Activate()
Dim tmp As String

  Exit Sub
  ' evaluate options to activate remaining tabs
  On Error GoTo Activate_Error
  ErrorTag = "form_Activate::EvaluateOptions"
  EvaluateOptions
  vfProcessUserInput = True
  If Not FormLoaded Then
    FormLoaded = True
  End If

  MousePointer = vbDefault
  Exit Sub
Activate_Error:
  MsgBox "form_Activate::" & ErrorTag & ">> " & Error()
  Resume Next
End Sub

Private Sub Form_load()
Dim i As Integer, tmp As String, flag As Boolean

  MousePointer = vbHourglass
  CenterForm Me
  Set frm = Me
  GetArguments
  panLeft.Caption = Trim$(Command$)
  If argc >= 5 Then
    GlyphName = argv(argc - 1)
    GlyphIndex = Val(argv(argc - 2))
    siteIdx = Val(argv(argc - 3))
    RunName = argv(argc - 4)
    FUIName = argv(argc - 5) & ".GID"
    
    If argv(0) = "/c" Then
      Caption = Caption & " - COMPUTED"
      KnownFlux = False
      ExecMode = COMPUTE_ONLY
    ElseIf argv(0) = "/k" Then
      Caption = Caption & " - KNOWN FLUX"
      KnownFlux = True
      ExecMode = KNOWN_ONLY
    ElseIf argv(0) = "/b" Then
      ExecMode = COMBINED
    Else
      MsgBox "First argument is invalid"
      End
    End If
    
    If argv(1) = "/sw" Then
'     SourceMedia = "Surface Water|surface water"
'     SourceMedia = "Non-flowing Pond|surface water" ' Aug03
      SourceMedia = "Standing Surface Water|surface water" ' Dec03
      halfIndex = SWHALF
    ElseIf argv(1) = "/aq" Then
      SourceMedia = "Aquifer|aquifer"
      halfIndex = GWHALF
    ElseIf argv(1) = "/sl" Then
      SourceMedia = "Soil/Vadose|soil"
      halfIndex = SLHALF
    Else
'     SourceMedia = "Surface Water|surface water"
'     SourceMedia = "Non-flowing Pond|surface water" ' Aug03
      SourceMedia = "Standing Surface Water|surface water" ' Dec03
      SourceMedia = SourceMedia & "|Soil/Vadose|soil" ' vadose zone
      SourceMedia = SourceMedia & "|Aquifer|aquifer"
      halfIndex = GWHALF
    End If
    
    If open_csv(errfile, RunName & ".ERR", 1) Then
      put_val errfile, "Error report for Source Term Model"
      put_line errfile
      put_line errfile
    Else
      MsgBox "Unable to create file " & RunName & ".ERR" & Chr(10) & "Check directory permissions"
      End
    End If
  Else
    MsgBox "Not enough arguments passed" & Chr(10) & "Contact PNNL"
    close_csv errfile
    End
  End If
  
  SetRefFile ReplaceExt(FUIName, "ref")
  SetHelpFile App.Path + "\strm.htm"
  load_convert
  InitParamInfo
  InitParamStruct
  InitListCatStructs
  
  ReDim TabEnabled(SSTab1.Tabs - 1)
  ReDim ssTabCtl(SSTab1.Tabs - 1)
  For i = 0 To UBound(ssTabCtl)
   Set ssTabCtl(i) = ssTab(i)
   Picture1(i).BorderStyle = 0
'  SetTabEnabled i, False
   SetTabEnabled i, False
  Next
  
  LoadParameterInfo
  LoadParameters
  AdjustContamParams
  
 'SSTab1.TabCaption(TAB_REL) = "Known Media" + Chr(10) + "Releases"
'SSTab1.TabCaption(TAB_FLUX) = "Known Contaminant" & Chr(10) & "Flux"
 
  ConstructOptionsSS 0
  SetTabEnabled 0, True
  
  ConstructReleaseSiteSS 1
  SetTabEnabled 1, True
  
  SSTab1.Tab = 0
  SetMediaOptions
  
  For i = 0 To UBound(ssTabCtl)
    ssTab(i).Enabled = False
  Next
  For i = 0 To Picture1.count - 1
    Picture1.item(i).Enabled = False
  Next i
  
'  SSTab1.Tab = 0
  SSTab1_Click 0
  
  Me.Show
  Refresh
  
  If Not GetParamElem(i, "WARNED", 0, 0, E_USERVAL, tmp) Then
   mnuHelpAbout_Click
   SetParamElem i, "WARNED", 0, 0, E_USERVAL, True
  End If
  
' On Error GoTo Activate_Error
' ErrorTag = "form_Activate::EvaluateOptions"
  EvaluateOptions
  vfProcessUserInput = True
  If Not FormLoaded Then
    FormLoaded = True
  End If

  MousePointer = vbDefault

End Sub

Private Sub mnuDiscard_Click()
  MousePointer = vbHourglass
  LoadParameters
  AdjustContamParams
  
  ConstructOptionsSS 0
  SetTabEnabled 0, True
  
  ConstructReleaseSiteSS 1
  SetTabEnabled 1, True
  
  SSTab1.Tab = 0
  MousePointer = vbDefault
End Sub

Private Sub mnuExitDiscard_Click()
  On Error Resume Next
  close_csv errfile
  Kill RunName & ".gid"
  Kill RunName & ".err"
  End
End Sub

Private Sub mnuExitSave_Click()
Dim complete As Boolean
Dim ans As Integer, msg As String
Dim paramId As String

  msg = "Input is incomplete" & vbCrLf
  complete = IsInputComplete(-1, paramId, msg)
  
' If Not IsInputComplete(-1, paramId) Then
'   msg = "Input required for " + paramId ' + " " + CStr(vptParam(np).c1) + " " + CStr(vptParam(np).c2)
'   msg = msg + Chr(10) + "Do you want to save incomplete data?"
'   ans = MsgBox(msg, vbYesNoCancel + vbQuestion)
' End If

' Select Case ans
'   Case vbCancel
'     Exit Sub
'   Case vbNo
'     mnuExitDiscard_Click
'   Case vbYes
      MousePointer = vbHourglass
      SaveParameters
      
      On Error Resume Next
      If Not complete Then
        put_val errfile, msg
        put_line errfile
      End If
      close_csv errfile
      If complete Then Kill RunName & ".err"
      End
' End Select
End Sub

Private Sub mnuSave_Click()
Dim complete As Boolean
Dim ans As Integer, msg As String
Dim paramId As String
  ans = vbYes
  If Not IsInputComplete(-1, paramId, msg) Then
    msg = "Input required for " + paramId
    msg = msg + Chr(10) + "Do you want to save incomplete data?"
    ans = MsgBox(msg, vbYesNo + vbQuestion)
  End If
  Select Case ans
    Case vbNo
      Exit Sub
    Case vbYes
      MousePointer = vbHourglass
      SaveParameters
  End Select
  MousePointer = vbDefault
End Sub

Private Sub mnuHelpAbout_Click()
' frmAbout.lblDescription.AutoSize = True
' frmAbout.lblDescription.WordWrap = True
  frmAbout.lblDescription = "The MEPAS Computed Source Term Release Model (CSTRM) " & _
    "estimates time varying, constituent mass fluxes exiting the waste zone as a function " & _
    "of time.  Both radionuclides and chemicals are evaluated by this model. " & _
    "Constituent mass flux values are estimated for volatilization, infiltration, " & _
    "suspension, overland runoff, and decay/degradation loss routes for contaminated " & _
    "soil/vadose, aquifer, or surface impoundment waste zones.  " & _
    "This model links to airborne and waterborne transport models."

''rmAbout.lblDisclaimer.AutoSize = True
'frmAbout.lblDisclaimer.WordWrap = True
 frmAbout.lblDisclaimer = "TECHNICAL WARNING ---- Please note that this model has not been " & _
    "fully tested.  All test cases have been completed, except the test case that examines " & _
    "the interaction of  multiple organic compound releases.  No errors have been identified, " & _
    "but this case is very complicated and requires additional analysis.  The user can model " & _
    "multiple organic compounds but they should examine their results very closely. " & _
    "Users are encouraged to provide information on appropriate and/or inappropriate behavior " & _
    "of the model.  This test case will be completed in the next version of the model."
  Load frmAbout
  frmAbout.Caption = "About MEPAS CSTRM"
  frmAbout.Left = frmMain.Left + 0.5 * (frmMain.Width - frmAbout.Width)
  frmAbout.Top = frmMain.Top + 0.5 * (frmMain.Height - frmAbout.Height)
  frmAbout.Show 1
End Sub

Private Sub howto_Click()
Dim paramId
Dim Index As Integer
Dim rc As Boolean
  
  HelpAnchor = ""
  Index = SSTab1.Tab
  MousePointer = vbHourglass
  If ssTab(Index).ActiveRow > 0 Then
    If (Index = TAB_CLIM) Then
      rc = ssTab(Index).GetText(ssTab(Index).ActiveCol, ssTab(Index).MaxRows, paramId)
    Else
      rc = ssTab(Index).GetText(SS_PID, ssTab(Index).ActiveRow, paramId)
    End If
    If rc Then HelpAnchor = CStr(paramId)
  End If
  GetHelp
  MousePointer = vbDefault
End Sub

Private Sub mnuRef_Click()
  mnuRefAdd.Enabled = parSelect.pname <> ""
  mnuRefSel.Enabled = parSelect.pname <> ""
End Sub

Private Sub mnuRefAdd_Click()
  Dim ndx As Integer
  RefMode = 1
' RefIdx = parSelect.ref
  lblRef.Tag = parSelect.ref
  GetRef lblRef
  If RefIdx >= 0 Then
    SetParamElem3 ndx, parSelect.pname, parSelect.c1, parSelect.c2, _
      parSelect.c3, E_REF, RefIdx
     ssTab(parSelect.sprIndex).SetText parSelect.ssCol, parSelect.ssRow, RefIdx
  End If
End Sub

Private Sub mnuRefSel_Click()
  Dim ndx As Integer, i As Integer
  RefMode = 0
  lblRef.Tag = parSelect.ref
  RefIdx = parSelect.ref
  GetRef lblRef
  If RefIdx >= 0 And RefIdx <> parSelect.ref Then
    If parSelect.sprIndex <> TAB_CLIM Then
      SetParamElem3 ndx, parSelect.pname, parSelect.c1, parSelect.c2, _
        parSelect.c3, E_REF, RefIdx
       ssTab(parSelect.sprIndex).SetText parSelect.ssCol, parSelect.ssRow, RefIdx
    Else
      For i = 1 To 12
        SetParamElem3 ndx, parSelect.pname, i, 0, 0, E_REF, RefIdx
      Next
    End If
    ssTab(parSelect.sprIndex).SetText parSelect.ssCol, parSelect.ssRow, RefIdx
  End If
End Sub

'Subs of the Format menu
Private Sub NumFixed_Click()
    CVTFormat = "Fixed"
    FormatChecked (CVTFormat)
     'tell user how number will appear
     MsgBox "Displays at least one digit to the left and two " + vbCrLf + _
            "digits to the right of the decimal separator.", vbOKOnly, "Fixed Number Format Selected"
End Sub
Private Sub NumGeneral_Click()
    CVTFormat = "General Number"
    FormatChecked (CVTFormat)
    'tell user how number will appear
    MsgBox "Displays number with no thousand separator.", vbOKOnly, "General Number Format Selected"
End Sub
Private Sub NumScientific_Click()
    CVTFormat = "Scientific"
    FormatChecked (CVTFormat)
    'tell user how number will appear
    MsgBox "Uses standard scientific notation", vbOKOnly, "Scientific Number Format Selected"
End Sub
Private Sub NumStandard_Click()
    CVTFormat = "Standard"
    FormatChecked (CVTFormat)
    'tell user how number will appear
    MsgBox "Displays number with thousand separator, at least " + vbCrLf + _
           "one digit to the left and two digits to the " + vbCrLf + _
           "right of the decimal separator.", vbOKOnly, "Standard Number Format Selected"
End Sub
Private Sub NumUser_Click()
Dim UserStrg As String
Dim PreviousStrg As String
Dim OkayStr As Boolean
                
    UserStrg = InputBox("Enter your format in one of these styles:  " + vbCrLf + vbCrLf + _
        vbTab + "  ##0.0##  OR  0.0##E+00" + _
        vbCrLf + vbCrLf + "The '#' character may be used as many times as " + vbCrLf + _
        "desired (where indicated), including zero times.  " + vbCrLf + vbCrLf + _
        "Current format is " + CVTFormat, "User Defined Number Format")

    If UserStrg = "" Then Exit Sub
    UserStrg = UCase(UserStrg)
    OkayStr = CheckUserDefinedFormat(UserStrg)       'basic check of characters
    If OkayStr Then
        CVTFormat = UserStrg
        FormatChecked (CVTFormat)
    End If
End Sub
Private Sub FormatChecked(FormatStr As String)
    NumFixed.Checked = False
    NumGeneral.Checked = False
    NumStandard.Checked = False
    NumScientific.Checked = False
    NumUser.Checked = False
    Select Case FormatStr
        Case "Standard"
            NumStandard.Checked = True
        Case "Fixed"
            NumFixed.Checked = True
        Case "General Number"
            NumGeneral.Checked = True
        Case "Scientific"
            NumScientific.Checked = True
        Case Else
            NumUser.Checked = True
        End Select
End Sub
'end format menu

Private Sub SSTab_Change(Index As Integer, ByVal col As Long, ByVal Row As Long)
  If Not vfProcessUserInput Then Exit Sub
  MousePointer = vbHourglass
  vaSpreadChange Index, col, Row
  Picture1(Index).Refresh
  MousePointer = vbDefault
End Sub

Private Sub SSTab_Click(Index As Integer, ByVal col As Long, ByVal Row As Long)
   vaSpreadClick Index, col, Row, True
End Sub

Sub EvaluateOptions()
  EvaluatePathwayCompute
  EvaluatePathwayRelease
  EvaluatePathwayFlux
  End Sub

Private Sub ssTab_KeyDown(Index As Integer, KeyCode As Integer, Shift As Integer)
  If (KeyCode = 40 And Shift = 0) Then Picture1(Index).Refresh
End Sub

Private Sub SSTab_KeyUp(Index As Integer, KeyCode As Integer, Shift As Integer)
  Debug.Print "SStab_KeyUp", Index, KeyCode, Shift, ssTab(Index).ActiveRow, ssTab(Index).ActiveCol
  If KeyCode = 13 And Shift = 2 Then
        ssTab(Index).col = ssTabCtl(Index).ActiveCol
        ssTab(Index).Row = ssTabCtl(Index).ActiveRow
        If ssTab(Index).CellType = SS_CELL_TYPE_BUTTON Then
          SSTab_Click Index, ssTabCtl(Index).ActiveCol, ssTabCtl(Index).ActiveRow
        End If
  Else
  GetParamHelp KeyCode
  End If
End Sub

Private Sub SSTab_LeaveCell(Index As Integer, ByVal col As Long, ByVal Row As Long, ByVal NewCol As Long, ByVal NewRow As Long, Cancel As Boolean)
  If NewCol > 0 And NewRow > 0 Then
    vaSpreadClick Index, NewCol, NewRow, False
  End If

End Sub

'Copy, cut, paste -- jdn 6/2001
Private Sub ssTab_RightClick(Index As Integer, ByVal ClickType As Integer, ByVal col As Long, ByVal Row As Long, ByVal MouseX As Long, ByVal MouseY As Long)
    'Set global variables
    With ssTab(Index)
        BeginCol = .SelBlockCol
        EndCol = .SelBlockCol2
        BeginRow = .SelBlockRow
        EndRow = .SelBlockRow2
    End With
    PopupMenu SpreadEdit, vbPopupMenuLeftAlign
End Sub
'Copy, cut, paste procedures in invisible menu
Private Sub SpreadCopy_Click()
    Call vaSpreadCopy
End Sub
Private Sub SpreadCut_Click()
    Call vaSpreadCut
End Sub
'This feature may not work as expected in this module because
'of coding conflicts
Private Sub SpreadPaste_Click()
    Call vaSpreadPaste
End Sub
'end copy, cut, paste

Private Sub SSTab1_Click(PreviousTab As Integer)
  Picture1(PreviousTab).Enabled = False: ssTabCtl(PreviousTab).Enabled = False
  Picture1(SSTab1.Tab).Enabled = True: ssTabCtl(SSTab1.Tab).Enabled = True
End Sub

Private Sub form_KeyDown(KeyCode As Integer, Shift As Integer)
  Select Case KeyCode
  Case vbKeyF1:
    KeyCode = 0
    howto_Click
  End Select
End Sub



