VERSION 5.00
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "THREED32.OCX"
Object = "{6B7E6392-850A-101B-AFC0-4210102A8DA7}#1.4#0"; "comctl32.Ocx"
Begin VB.Form frmView 
   Caption         =   "MEPAS Sensitivity/Uncertainty Module"
   ClientHeight    =   1320
   ClientLeft      =   5100
   ClientTop       =   4950
   ClientWidth     =   6495
   ControlBox      =   0   'False
   Icon            =   "Sens1.frx":0000
   LinkTopic       =   "Form1"
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   1320
   ScaleWidth      =   6495
   Begin VB.Timer Timer1 
      Interval        =   300
      Left            =   240
      Top             =   3240
   End
   Begin Threed.SSFrame SSFrame2 
      Height          =   1260
      Left            =   120
      TabIndex        =   0
      Top             =   0
      Width           =   6300
      _Version        =   65536
      _ExtentX        =   11112
      _ExtentY        =   2222
      _StockProps     =   14
      Caption         =   "Status"
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Begin ComctlLib.ProgressBar ProgressBar1 
         Height          =   255
         Left            =   360
         TabIndex        =   3
         Top             =   840
         Width           =   4215
         _ExtentX        =   7435
         _ExtentY        =   450
         _Version        =   327682
         Appearance      =   1
         Min             =   1
         Max             =   500
      End
      Begin VB.CommandButton cmdStop 
         Caption         =   "Stop"
         Height          =   375
         Left            =   4800
         TabIndex        =   1
         Top             =   360
         Width           =   1245
      End
      Begin VB.Label lblMemStat 
         Height          =   252
         Index           =   0
         Left            =   2760
         TabIndex        =   17
         Top             =   1320
         Width           =   1452
      End
      Begin VB.Label Label1 
         Alignment       =   1  'Right Justify
         Caption         =   "Percent of memory in use:"
         Height          =   252
         Index           =   0
         Left            =   360
         TabIndex        =   16
         Top             =   1320
         Width           =   2292
      End
      Begin VB.Label Label1 
         Alignment       =   1  'Right Justify
         Caption         =   "Bytes of physical memory:"
         Height          =   252
         Index           =   1
         Left            =   360
         TabIndex        =   15
         Top             =   1680
         Width           =   2292
      End
      Begin VB.Label Label1 
         Alignment       =   1  'Right Justify
         Caption         =   "Free physical memory:"
         Height          =   252
         Index           =   2
         Left            =   360
         TabIndex        =   14
         Top             =   2040
         Width           =   2292
      End
      Begin VB.Label Label1 
         Alignment       =   1  'Right Justify
         Caption         =   "Paging file (bytes):"
         Height          =   252
         Index           =   3
         Left            =   360
         TabIndex        =   13
         Top             =   2400
         Width           =   2292
      End
      Begin VB.Label Label1 
         Alignment       =   1  'Right Justify
         Caption         =   "Free paging file (bytes):"
         Height          =   252
         Index           =   4
         Left            =   360
         TabIndex        =   12
         Top             =   2760
         Width           =   2292
      End
      Begin VB.Label Label1 
         Alignment       =   1  'Right Justify
         Caption         =   "User bytes of address space:"
         Height          =   252
         Index           =   5
         Left            =   360
         TabIndex        =   11
         Top             =   3120
         Width           =   2292
      End
      Begin VB.Label Label1 
         Alignment       =   1  'Right Justify
         Caption         =   "Free user bytes:"
         Height          =   252
         Index           =   6
         Left            =   360
         TabIndex        =   10
         Top             =   3480
         Width           =   2292
      End
      Begin VB.Label lblMemStat 
         Height          =   252
         Index           =   1
         Left            =   2760
         TabIndex        =   9
         Top             =   1680
         Width           =   1452
      End
      Begin VB.Label lblMemStat 
         Height          =   252
         Index           =   2
         Left            =   2760
         TabIndex        =   8
         Top             =   2040
         Width           =   1452
      End
      Begin VB.Label lblMemStat 
         Height          =   252
         Index           =   3
         Left            =   2760
         TabIndex        =   7
         Top             =   2400
         Width           =   1452
      End
      Begin VB.Label lblMemStat 
         Height          =   252
         Index           =   4
         Left            =   2760
         TabIndex        =   6
         Top             =   2760
         Width           =   1452
      End
      Begin VB.Label lblMemStat 
         Height          =   252
         Index           =   5
         Left            =   2760
         TabIndex        =   5
         Top             =   3120
         Width           =   1452
      End
      Begin VB.Label lblMemStat 
         Height          =   252
         Index           =   6
         Left            =   2760
         TabIndex        =   4
         Top             =   3480
         Width           =   1452
      End
      Begin VB.Label StatusLabel 
         BorderStyle     =   1  'Fixed Single
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   375
         Left            =   360
         TabIndex        =   2
         Top             =   360
         Width           =   4215
      End
   End
End
Attribute VB_Name = "frmView"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim minutesDelay As Long

Private Sub Form_Load()
  Dim pfile As parmfile
  Dim startTime As Long
  Dim hours As Long
  Dim minutes As Long
  Dim currentTime As Single
  Dim endTime As Single
  Dim mode As String
  
  GetArguments
  If DEBUG_OUTPUT Then
    frmView.Height = 4340
    SSFrame2.Height = 3900
  Else
    frmView.Height = 1700
    SSFrame2.Height = 1260
  End If
  
  Refresh
  Call StartModule(frmView, "MEPAS Sensitivity/Uncertainty Module", argc)
  If (open_parm(pfile, FUIName, F_READ)) Then
    Call LoadSection(pfile, modName)
    minutesDelay = 0
    If (ReadStr(pfile, mode, "noDelay")) Then
      If (mode = "T") Then
        If (ReadLng(pfile, startTime, "StartTime")) Then
          If (startTime >= 0) Then
            hours = startTime
            currentTime = Timer / 60
            endTime = hours * 60
            If (currentTime > endTime) Then
              endTime = endTime + 24 * 60 ' minutes in a day
            End If
            minutesDelay = endTime - currentTime
            Timer1.Interval = 60000  '  delay for a minute
          Else
          
          End If
        End If
      End If
    End If
    Timer1.Enabled = True
  End If
  Call close_parm(pfile)
  cmdInitialize
End Sub

Private Sub cmdStop_Click()
  If cmdStop.Caption = "Stop" Then
    If (minutesDelay = 0) Then
      cmdStop.Caption = "Go"
      status = ITER_STOP
    Else
      Call PutError("Terminated by user")
      Call cmdFinalize
    End If
  Else
    cmdStop.Caption = "Stop"
    status = ITER_GO
    Timer1.Enabled = True
  End If
End Sub

Private Sub cmdInitialize()
Dim getshorty As String
  
  haveError = False
'  DotErrFile = FreeFile
'  On Error Resume Next
'  Kill argv(1) & ".err"
'  On Error GoTo 0
'  Open argv(1) & ".err" For Output As #DotErrFile
'  Write #DotErrFile, "Sensitivity/Uncertainty did not complete successfully!"
'  Call put_val(errfile, "Sensitivity/Uncertainty did not complete successfully!")
'  Close #DotErrFile
  
  DotMsgFile = FreeFile
  On Error Resume Next
  Kill argv(1) & ".msg"
  On Error GoTo 0
  Open argv(1) & ".msg" For Output As #DotMsgFile
  Write #DotMsgFile, "Sensitivity/Uncertainty messages"
  Close #DotMsgFile
  
  getshorty = GetShortName(App.Path)
  GetMemoryStatus
  IterCount = Initialize(getshorty, argv(0), argv(1), argv(2), argv(3), argv(4))
  GetMemoryStatus
  If IterCount = 0 Then cmdFinalize
  status = ITER_GO
  Iteration = 1
  ProgressBar1.Max = IterCount
  ProgressBar1.Value = Iteration
  StatusLabel.Caption = "Running iteration " & Iteration & " of " & IterCount
End Sub

Private Sub cmdFinalize()
Dim result As Long

  result = Finalize()
  If result = 1 And Not haveError Then AnError = False
  EndModule
  End
End Sub

Sub GetMemoryStatus()
Dim MS As MEMORYSTATUS
Dim msg As String
   
  If Not DEBUG_OUTPUT Then Exit Sub
  
  Open argv(1) & ".msg" For Append As #DotMsgFile
  MS.dwLength = Len(MS)
  GlobalMemoryStatus MS
  'divide the memory variables by 1024 (nkb)
  'to obtain the size in kilobytes
  lblMemStat(0) = Format$(MS.dwMemoryLoad, fmt) & " % used"
  msg = "Memory Load: " + lblMemStat(0)
  Write #DotMsgFile, msg
  lblMemStat(1) = Format$(MS.dwTotalPhys / nkb, fmt) & skb
  msg = "Total Physical Memory: " + lblMemStat(1)
  Write #DotMsgFile, msg
  lblMemStat(2) = Format$(MS.dwAvailPhys / nkb, fmt) & skb
  msg = "Available Physical Memory: " + lblMemStat(2)
  Write #DotMsgFile, msg
  lblMemStat(3) = Format$(MS.dwTotalPageFile / nkb, fmt) & skb
  msg = "Total Page Memory: " + lblMemStat(3)
  Write #DotMsgFile, msg
  lblMemStat(4) = Format$(MS.dwAvailPageFile / nkb, fmt) & skb
  msg = "Available Page Memory: " + lblMemStat(4)
  Write #DotMsgFile, msg
  lblMemStat(5) = Format$(MS.dwTotalVirtual / nkb, fmt) & skb
  msg = "Total Virtual Memory: " + lblMemStat(5)
  Write #DotMsgFile, msg
  lblMemStat(6) = Format$(MS.dwAvailVirtual / nkb, fmt) & skb
  msg = "Available Virtual Memory: " + lblMemStat(6)
  Write #DotMsgFile, msg
  Close #DotMsgFile
  frmView.Refresh
End Sub

Private Sub Timer1_Timer()
Dim i As Integer
Dim result As Long
  Timer1.Enabled = False
  If (minutesDelay = 0) Then
    StatusLabel.Caption = "Running iteration " & Iteration & " of " & IterCount
    cmdStop.Caption = "Stop"
    If Iteration < IterCount Then
      For i = Iteration To IterCount
        Iteration = i
        If status = ITER_STOP Then Exit Sub
        ProgressBar1.Value = i
        StatusLabel.Caption = "Running iteration " & i & " of " & IterCount
        GetMemoryStatus
        result = Iterate(Iteration)
        DoEvents
        StatusLabel.Caption = "Finished iteration " & i & " of " & IterCount
        GetMemoryStatus
        If result <> 1 Then haveError = True
      Next
    End If
    cmdFinalize
  Else
    StatusLabel.Caption = "Delayed for " & minutesDelay & " minutes"
    minutesDelay = minutesDelay - 1
    Timer1.Interval = 60000
    Timer1.Enabled = True
  End If
End Sub
