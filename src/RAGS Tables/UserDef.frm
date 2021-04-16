VERSION 5.00
Object = "{6B7E6392-850A-101B-AFC0-4210102A8DA7}#1.3#0"; "COMCTL32.OCX"
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Object = "{86CF1D34-0C5F-11D2-A9FC-0000F8754DA1}#2.0#0"; "mscomct2.ocx"
Begin VB.Form frmUserDef 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "RAGS Criteria"
   ClientHeight    =   6345
   ClientLeft      =   45
   ClientTop       =   735
   ClientWidth     =   6615
   ControlBox      =   0   'False
   Icon            =   "UserDef.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   6345
   ScaleWidth      =   6615
   StartUpPosition =   2  'CenterScreen
   Begin VB.Frame frmMeasure 
      Caption         =   "Specify De-Minimus and Measure"
      ForeColor       =   &H00FF0000&
      Height          =   3660
      Left            =   120
      TabIndex        =   9
      Top             =   600
      Width           =   5505
      Begin VB.Frame Frame2 
         Caption         =   "Measure"
         Height          =   2175
         Left            =   210
         TabIndex        =   10
         Top             =   1200
         Width           =   5070
         Begin VB.TextBox Text2 
            Alignment       =   2  'Center
            Enabled         =   0   'False
            Height          =   285
            Left            =   915
            TabIndex        =   22
            Text            =   "95"
            Top             =   840
            Width           =   495
         End
         Begin VB.OptionButton Option2 
            Caption         =   "Probability of NOT exceeding measure (e.g. risk/hazard)"
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   480
            Index           =   5
            Left            =   435
            TabIndex        =   21
            Tag             =   "Perc"
            Top             =   270
            Value           =   -1  'True
            Width           =   3990
         End
         Begin VB.OptionButton Option2 
            Caption         =   "Midpoint"
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   270
            Index           =   4
            Left            =   3195
            TabIndex        =   15
            Tag             =   "Midpoint Exposure"
            Top             =   1290
            Width           =   1200
         End
         Begin VB.OptionButton Option2 
            Caption         =   "Median"
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   270
            Index           =   3
            Left            =   2100
            TabIndex        =   14
            Tag             =   "Median Exposure"
            Top             =   1695
            Width           =   1200
         End
         Begin VB.OptionButton Option2 
            Caption         =   "Mean"
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   270
            Index           =   2
            Left            =   2100
            TabIndex        =   13
            Tag             =   "Mean Exposure"
            Top             =   1305
            Width           =   975
         End
         Begin VB.OptionButton Option2 
            Caption         =   "Maximum"
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   270
            Index           =   1
            Left            =   435
            TabIndex        =   12
            Tag             =   "Maximum Exposure"
            Top             =   1710
            Width           =   1695
         End
         Begin VB.OptionButton Option2 
            Caption         =   "Minimum"
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   270
            Index           =   0
            Left            =   435
            TabIndex        =   11
            Tag             =   "Minimum Exposure"
            Top             =   1320
            Width           =   1575
         End
         Begin MSComCtl2.UpDown UpDown1 
            Height          =   285
            Left            =   1440
            TabIndex        =   19
            Top             =   840
            Width           =   705
            _ExtentX        =   1244
            _ExtentY        =   503
            _Version        =   393216
            Value           =   95
            BuddyControl    =   "Text2"
            BuddyDispid     =   196615
            OrigLeft        =   1335
            OrigTop         =   645
            OrigRight       =   2010
            OrigBottom      =   945
            Increment       =   5
            Max             =   99
            Min             =   1
            Orientation     =   1
            SyncBuddy       =   -1  'True
            BuddyProperty   =   65547
            Enabled         =   -1  'True
         End
         Begin VB.Label Label2 
            Caption         =   "( 1 - 99 %): "
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   300
            Left            =   2310
            TabIndex        =   20
            Top             =   870
            Width           =   1860
         End
      End
      Begin VB.Frame Frame1 
         Caption         =   "De-Minimus"
         Height          =   765
         Left            =   225
         TabIndex        =   16
         Top             =   315
         Width           =   5070
         Begin VB.OptionButton optDeMinimus 
            Caption         =   "All Values"
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   345
            Index           =   0
            Left            =   435
            TabIndex        =   18
            Tag             =   "All"
            Top             =   270
            Value           =   -1  'True
            Width           =   1305
         End
         Begin VB.OptionButton optDeMinimus 
            Caption         =   "Non-zero Values"
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   345
            Index           =   1
            Left            =   2100
            TabIndex        =   17
            Tag             =   "NonZero"
            Top             =   270
            Width           =   1950
         End
      End
   End
   Begin VB.Frame frmAdvise 
      Caption         =   "Advisory:"
      ForeColor       =   &H00C00000&
      Height          =   3660
      Left            =   120
      TabIndex        =   7
      Top             =   600
      Width           =   5505
      Begin VB.TextBox Text1 
         BackColor       =   &H80000001&
         BorderStyle     =   0  'None
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   3210
         Left            =   165
         MultiLine       =   -1  'True
         TabIndex        =   8
         Top             =   210
         Width           =   5205
      End
   End
   Begin VB.Frame Frame3 
      Caption         =   "COPC Thresholds for Table 10"
      ForeColor       =   &H00FF0000&
      Height          =   1305
      Left            =   120
      TabIndex        =   24
      Top             =   4320
      Width           =   5505
      Begin VB.TextBox txtNCthresh 
         Alignment       =   1  'Right Justify
         BackColor       =   &H00C0FFC0&
         Height          =   312
         Left            =   3735
         TabIndex        =   27
         Tag             =   "RFDLIM"
         Text            =   "0.0"
         Top             =   780
         Width           =   1000
      End
      Begin VB.TextBox txtCThresh 
         Alignment       =   1  'Right Justify
         BackColor       =   &H00C0FFC0&
         Height          =   312
         Left            =   3720
         TabIndex        =   25
         Tag             =   "RFDLIM"
         Text            =   "0.0"
         Top             =   330
         Width           =   1000
      End
      Begin VB.Label lbl 
         Caption         =   "Non-Carcinogenic Hazard Quotient"
         Height          =   255
         Index           =   0
         Left            =   405
         TabIndex        =   28
         Tag             =   "rfdlim"
         Top             =   765
         Width           =   3015
      End
      Begin VB.Label lbl 
         Caption         =   "Carcinogenic Risk"
         Height          =   255
         Index           =   3
         Left            =   390
         TabIndex        =   26
         Tag             =   "rfdlim"
         Top             =   375
         Width           =   3015
      End
   End
   Begin VB.ComboBox Combo1 
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   315
      ItemData        =   "UserDef.frx":030A
      Left            =   1320
      List            =   "UserDef.frx":0317
      Style           =   2  'Dropdown List
      TabIndex        =   5
      Top             =   120
      Width           =   4275
   End
   Begin MSComctlLib.StatusBar StatusBar1 
      Align           =   2  'Align Bottom
      Height          =   30
      Left            =   0
      TabIndex        =   4
      Top             =   6315
      Width           =   6615
      _ExtentX        =   11668
      _ExtentY        =   53
      _Version        =   393216
      BeginProperty Panels {8E3867A5-8586-11D1-B16A-00C0F0283628} 
         NumPanels       =   1
         BeginProperty Panel1 {8E3867AB-8586-11D1-B16A-00C0F0283628} 
         EndProperty
      EndProperty
   End
   Begin VB.CommandButton Command3 
      Caption         =   "Cancel Tables"
      Enabled         =   0   'False
      Height          =   420
      Left            =   2400
      TabIndex        =   3
      Top             =   5760
      Width           =   1500
   End
   Begin VB.CheckBox chkOther 
      Caption         =   "Probability of Exceedence"
      Height          =   345
      Index           =   5
      Left            =   2670
      TabIndex        =   1
      Top             =   75
      Value           =   1  'Checked
      Width           =   2160
   End
   Begin VB.CommandButton Command2 
      Caption         =   "Generate Tables"
      Default         =   -1  'True
      Height          =   420
      Left            =   4080
      TabIndex        =   0
      Top             =   5760
      Width           =   1500
   End
   Begin ComctlLib.ProgressBar ProgressBar1 
      Height          =   375
      Left            =   120
      TabIndex        =   2
      Top             =   5760
      Visible         =   0   'False
      Width           =   2160
      _ExtentX        =   3810
      _ExtentY        =   661
      _Version        =   327682
      Appearance      =   1
   End
   Begin MSComctlLib.TreeView TreeView1 
      Height          =   6015
      Left            =   6120
      TabIndex        =   23
      Top             =   120
      Width           =   4320
      _ExtentX        =   7620
      _ExtentY        =   10610
      _Version        =   393217
      Style           =   7
      Appearance      =   1
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      AutoSize        =   -1  'True
      Caption         =   "Select Output:"
      Height          =   195
      Left            =   165
      TabIndex        =   6
      Top             =   165
      Width           =   1020
   End
   Begin VB.Menu mnuExit 
      Caption         =   "E&xit"
   End
   Begin VB.Menu howto 
      Caption         =   "&Help"
   End
End
Attribute VB_Name = "frmUserDef"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Public ProcessEvents As Boolean

Const PE_INDEX = 5

Private Sub Combo1_Click()
  frmAdvise.Visible = Combo1.ListIndex < 2
  frmMeasure.Visible = Not frmAdvise.Visible
End Sub

Private Sub Command1_Click()
  If vbYes = MsgBox("Are you sure you want to quit?", vbQuestion + vbYesNo, "Quit") Then
    EndRAGS
  End If
End Sub

Private Sub Command2_Click()
  Dim i As Integer
  Dim cap As String
  Dim moi As Integer
  
  On Error GoTo ErrorHandler
  
  If MousePointer = vbHourglass Then Exit Sub
  
  Select Case Combo1.ListIndex
    Case 0: whichTables = "CT"
      moi = mct
      mTitle = TITLE_CT
      mTitle = UCase(mTitle)
    Case 1: whichTables = "RME"
      moi = mRME
      mTitle = TITLE_RME
      mTitle = UCase(mTitle)
    Case 2: whichTables = "OTHER"
      If optDeMinimus(0).value Then
        mfDeMin = optDeMinimus(0).Tag
      Else
        mfDeMin = optDeMinimus(1).Tag
      End If
      For i = Option2.LBound To Option2.UBound
        If Option2(i).value Then
          mTitle = Option2(i).Tag
          cap = Option2(i).Tag
          If i = PE_INDEX Then
            moi = mPE
            mfThreshold = CInt(Text2.Text) ' val(Text2.Text)
            mTitle = mfThreshold & "% " & Option2(i).Caption
            ' the 95th percentile is equivalent to exceeding a value 5% of the time
            '     if percentile is p then threshold = 100 - p
'''''''''            mfThreshold = 100# - mfThreshold
          End If
          If 0 < InStr(cap, "Min") Then moi = mMIN
          If 0 < InStr(cap, "Max") Then moi = mMAX
          If 0 < InStr(cap, "Mid") Then moi = mMID
          If 0 < InStr(cap, "Mean") Then moi = mMN
          If 0 < InStr(cap, "Med") Then moi = mMED
          Exit For
        End If
      Next i
      mTitle = UCase(mTitle)
      mTitle = mTitle & ", " & IIf(mfDeMin = "All", "All Values", "Non-zero Values")
  End Select
  measureOfInterest = moi
  Set t9 = TreeView1
  t9.Nodes.Clear
  MousePointer = vbHourglass
  Frame1.Enabled = False
  Frame2.Enabled = False
  Command3.Enabled = True
  Cancel = False
  ProgressBar1.Visible = True
  GenerateTables
  close_csv errfile
  Close 'all files
  If Not AnError Then Kill RunName & ".ERR"

ErrorHandler:
  If Err.Number <> 0 Then
    MsgBox "Exiting due to error: " & vbCrLf & Err.Description, vbOKOnly, "RAGS Report Generator"
  End If
  Frame1.Enabled = True
  Frame2.Enabled = True
  Command3.Enabled = False
  MousePointer = vbDefault
  ProgressBar1.Visible = False
  If Not Cancel Then
'     EndRAGS
  Else
    Cancel = False
  End If
End Sub

Private Sub Command3_Click()
  If vbYes = MsgBox("Cancel these tables?", vbQuestion + vbYesNo, "Cancel") Then
    Cancel = True
    MousePointer = Default
  End If
End Sub

Private Sub Form_Load()
  Dim wid As Long
  Dim ht As Long
  Dim cw As Long
  Dim lngCount As Long
  Dim advisory As String
  Dim strFileName As String
  
  Set mainform = Me
  
  strFileName = String(255, 0)
  lngCount = GetModuleFileName(App.hInstance, strFileName, 255)
  strFileName = Left(strFileName, lngCount)
  
  AppPath = Space$(256)
  GetPrivateProfileString "App Path", "FUI", App.Path, AppPath, 256, FRAMES_INI
  AppPath = StripTerminator(AppPath)

  If Right(strFileName, 7) = "VB6.EXE" Then
    hLibModule = LoadLibrary(AppPath & "\frames.dll")
  End If
  
  StartModule Me, "RAGS Table Generator", 5
  If Not areSourcesConnected() Then
    Dim msg As String
    msg = "The module sources from exposure to risk are not contiguous. "
    msg = msg & vbCrLf & "This configuration is not recommended for RAGS and may cause the viewer to fail."
    msg = msg & vbCrLf & "Do you want to continue?."
    Dim answer
    answer = MsgBox(msg, vbExclamation + vbYesNo, "RAGS Table Generator")
    If answer = vbNo Then
      Unload Me
    End If
  End If
  
  Set frm = Me
  Template = App.Path & "\rags_templates.xls"
  SetHelpFile App.Path + "\RAGS.htm"
  
  
  ReDim advise(3)
  advise(1) = "a.  The user is responsible to ensure that assumptions associated with choosing the Central Tendency (CT) or Reasonable Maximum Exposure (RME) are specified and adhered to in the assessment."
  advise(2) = "b.  The source term and all subsequent exposure concentrations and assessment results are assumed NOT to be a function of time."
  advise(3) = "c.  Because it is assumed that the assessment results do NOT vary in time, the first output value is used in developing the RAGS summary tables."

  advisory = vbCrLf & advise(1)
  advisory = advisory & vbCrLf & vbCrLf & advise(2)
  advisory = advisory & vbCrLf & vbCrLf & advise(3)
  
  frmMeasure.Move frmAdvise.Left, frmAdvise.Top, frmAdvise.width, frmAdvise.Height
  Me.width = Frame3.width + (3 * Frame3.Left)
  Text1.BackColor = Me.BackColor
  Text1.Text = advisory
  
  Combo1.ListIndex = 2
  
  Dim x As Long
  x = GetSystemMetrics(SM_CXVSCROLL)
  
  Dim txt As String
  Dim j As Integer
  ProcessEvents = True
  
End Sub

Private Sub Form_Unload(Cancel As Integer)
  EndRAGS
End Sub

Public Sub EndDialog()
'  Unload frmRAGS
End Sub

Private Sub mnuHelp_Click()
End Sub

Private Sub howto_Click()
  MousePointer = vbHourglass
  GetHelp
  MousePointer = vbDefault
End Sub

Private Sub mnuExit_Click()
  EndRAGS
End Sub

Private Sub Option2_Click(Index As Integer)
  UpDown1.Enabled = Option2(5).value
End Sub

