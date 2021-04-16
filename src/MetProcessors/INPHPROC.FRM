VERSION 4.00
Begin VB.Form Form1 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   Caption         =   "Input for HrlyProc"
   ClientHeight    =   5892
   ClientLeft      =   516
   ClientTop       =   624
   ClientWidth     =   7908
   BeginProperty Font 
      name            =   "MS Sans Serif"
      charset         =   1
      weight          =   700
      size            =   7.8
      underline       =   0   'False
      italic          =   0   'False
      strikethrough   =   0   'False
   EndProperty
   ForeColor       =   &H80000008&
   Height          =   6444
   Left            =   468
   LinkTopic       =   "Form1"
   ScaleHeight     =   5892
   ScaleWidth      =   7908
   Top             =   120
   Width           =   8004
   Begin VB.CommandButton QuitButton 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "Quit"
      Height          =   495
      Left            =   5040
      TabIndex        =   11
      Top             =   5160
      Width           =   1575
   End
   Begin VB.CommandButton RunButton 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "Run HrlyProc"
      Height          =   495
      Left            =   1200
      TabIndex        =   10
      Top             =   5160
      Width           =   1695
   End
   Begin VB.Frame TD32Frame 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "TD3240 Data"
      ForeColor       =   &H80000008&
      Height          =   1455
      Left            =   360
      TabIndex        =   20
      Top             =   3480
      Width           =   7335
      Begin VB.Frame Frame3 
         Appearance      =   0  'Flat
         BackColor       =   &H80000005&
         Caption         =   "Format"
         ForeColor       =   &H80000008&
         Height          =   615
         Left            =   1320
         TabIndex        =   22
         Top             =   720
         Width           =   4095
         Begin VB.OptionButton TDformOpt 
            Appearance      =   0  'Flat
            BackColor       =   &H80000005&
            Caption         =   "Variable"
            ForeColor       =   &H80000008&
            Height          =   255
            Index           =   1
            Left            =   2280
            TabIndex        =   9
            Top             =   240
            Width           =   1455
         End
         Begin VB.OptionButton TDformOpt 
            Appearance      =   0  'Flat
            BackColor       =   &H80000005&
            Caption         =   "Fixed"
            ForeColor       =   &H80000008&
            Height          =   255
            Index           =   0
            Left            =   600
            TabIndex        =   8
            Top             =   240
            Width           =   1215
         End
      End
      Begin VB.Label Td32FileLabel 
         Appearance      =   0  'Flat
         AutoSize        =   -1  'True
         BackColor       =   &H80000005&
         Caption         =   "Label14"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   120
         TabIndex        =   27
         Top             =   480
         Width           =   690
      End
      Begin VB.Label Label10 
         Appearance      =   0  'Flat
         AutoSize        =   -1  'True
         BackColor       =   &H80000005&
         Caption         =   "Name of Precp Data File"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   120
         TabIndex        =   21
         Top             =   240
         Width           =   2100
      End
   End
   Begin VB.CheckBox TD32Check 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "Use TD3240 Precp Data"
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   360
      TabIndex        =   7
      Top             =   3120
      Width           =   2535
   End
   Begin VB.Frame Frame1 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "Station Information"
      ForeColor       =   &H80000008&
      Height          =   1455
      Left            =   360
      TabIndex        =   13
      Top             =   1560
      Width           =   7335
      Begin VB.TextBox ApplZ0Text 
         Appearance      =   0  'Flat
         Height          =   285
         Left            =   6360
         TabIndex        =   6
         Text            =   " "
         Top             =   1080
         Width           =   855
      End
      Begin VB.TextBox MeasZ0Text 
         Appearance      =   0  'Flat
         Height          =   285
         Left            =   2640
         TabIndex        =   5
         Text            =   " "
         Top             =   1080
         Width           =   855
      End
      Begin VB.TextBox WndHgtText 
         Appearance      =   0  'Flat
         Height          =   285
         Left            =   6360
         TabIndex        =   4
         Text            =   " "
         Top             =   720
         Width           =   855
      End
      Begin VB.TextBox TimeZText 
         Appearance      =   0  'Flat
         Height          =   285
         Left            =   2640
         TabIndex        =   3
         Text            =   " "
         Top             =   720
         Width           =   855
      End
      Begin VB.TextBox LongText 
         Appearance      =   0  'Flat
         Height          =   285
         Left            =   6360
         TabIndex        =   2
         Text            =   " "
         Top             =   360
         Width           =   855
      End
      Begin VB.TextBox LatText 
         Appearance      =   0  'Flat
         Height          =   285
         Left            =   2640
         TabIndex        =   1
         Text            =   " "
         Top             =   360
         Width           =   855
      End
      Begin VB.Label Label9 
         Appearance      =   0  'Flat
         AutoSize        =   -1  'True
         BackColor       =   &H80000005&
         Caption         =   "z0 at Application Site (m)"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   3600
         TabIndex        =   19
         Top             =   1080
         Width           =   2145
      End
      Begin VB.Label Label8 
         Appearance      =   0  'Flat
         AutoSize        =   -1  'True
         BackColor       =   &H80000005&
         Caption         =   "z0 at Measurement Site (m)"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   120
         TabIndex        =   18
         Top             =   1080
         Width           =   2325
      End
      Begin VB.Label Label7 
         Appearance      =   0  'Flat
         AutoSize        =   -1  'True
         BackColor       =   &H80000005&
         Caption         =   "Wind Measurement Height (m)"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   3600
         TabIndex        =   17
         Top             =   720
         Width           =   2565
      End
      Begin VB.Label Label6 
         Appearance      =   0  'Flat
         AutoSize        =   -1  'True
         BackColor       =   &H80000005&
         Caption         =   "Hours from GMT"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   120
         TabIndex        =   16
         Top             =   720
         Width           =   1395
      End
      Begin VB.Label Label5 
         Appearance      =   0  'Flat
         AutoSize        =   -1  'True
         BackColor       =   &H80000005&
         Caption         =   "Longitude (deg)"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   3600
         TabIndex        =   15
         Top             =   360
         Width           =   1350
      End
      Begin VB.Label Label4 
         Appearance      =   0  'Flat
         AutoSize        =   -1  'True
         BackColor       =   &H80000005&
         Caption         =   "Latitude (deg)"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   120
         TabIndex        =   14
         Top             =   360
         Width           =   1200
      End
   End
   Begin MSComDlg.CommonDialog OpenDialog 
      Left            =   4920
      Top             =   480
      _version        =   65536
      _extentx        =   677
      _extenty        =   677
      _stockprops     =   0
   End
   Begin VB.Label SfcFileLabel 
      Appearance      =   0  'Flat
      AutoSize        =   -1  'True
      BackColor       =   &H80000005&
      Caption         =   "Label13"
      ForeColor       =   &H80000008&
      Height          =   195
      Left            =   360
      TabIndex        =   26
      Top             =   1320
      Width           =   690
   End
   Begin VB.Label OutFileLabel 
      Appearance      =   0  'Flat
      AutoSize        =   -1  'True
      BackColor       =   &H80000005&
      Caption         =   "Label12"
      ForeColor       =   &H80000008&
      Height          =   195
      Left            =   360
      TabIndex        =   25
      Top             =   840
      Width           =   690
   End
   Begin VB.Label ParmFileLabel 
      Appearance      =   0  'Flat
      AutoSize        =   -1  'True
      BackColor       =   &H80000005&
      Caption         =   "Label11"
      ForeColor       =   &H80000008&
      Height          =   195
      Left            =   360
      TabIndex        =   24
      Top             =   360
      Width           =   690
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      AutoSize        =   -1  'True
      BackColor       =   &H80000005&
      Caption         =   "Information Saved in File:"
      ForeColor       =   &H80000008&
      Height          =   195
      Left            =   360
      TabIndex        =   23
      Top             =   120
      Width           =   2190
   End
   Begin VB.Label Label3 
      Appearance      =   0  'Flat
      AutoSize        =   -1  'True
      BackColor       =   &H80000005&
      Caption         =   "Path and Name of Hourly Surface Data:"
      ForeColor       =   &H80000008&
      Height          =   195
      Left            =   360
      TabIndex        =   12
      Top             =   1080
      Width           =   3390
   End
   Begin VB.Label Label2 
      Appearance      =   0  'Flat
      AutoSize        =   -1  'True
      BackColor       =   &H80000005&
      Caption         =   "Path and Name of Output File:"
      ForeColor       =   &H80000008&
      Height          =   195
      Left            =   360
      TabIndex        =   0
      Top             =   600
      Width           =   2595
   End
   Begin VB.Menu RunMenu 
      Caption         =   "&Run HRLYPROC"
   End
   Begin VB.Menu FileMenu 
      Caption         =   "&Files"
      Begin VB.Menu ChgParmMenu 
         Caption         =   "Change &Info Saved File"
      End
      Begin VB.Menu ChgOutMenu 
         Caption         =   "Change &Output File"
      End
      Begin VB.Menu ChgSfcMenu 
         Caption         =   "Change &Hourly Surface Data File"
      End
      Begin VB.Menu ChgTd32Menu 
         Caption         =   "Change &TD3240 File"
      End
   End
   Begin VB.Menu QuitMenu 
      Caption         =   "&Quit"
   End
End
Attribute VB_Name = "Form1"
Attribute VB_Creatable = False
Attribute VB_Exposed = False
Option Explicit

Private Sub ChgOutFile_Click()
End Sub

Private Sub ChgOutMenu_Click()
    Dim tmp1$, tmp$, msg$, er$, m$
    Dim response, ibad, ms, mb


    On Local Error GoTo OpenFileError1

    'Set Dialog Box Title
    Opendialog.DialogTitle = "Open Input File"
    'Set Default Filename
    Opendialog.filename = OutFileName$
    'Set Default Path
    Opendialog.InitDir = App.Path
    'Set Default Filters
    Opendialog.Filter = "All Files (*.*)|*.*|Output Files (*.OUT)|*.OUT"
    'Enable Cancel button
    Opendialog.CancelError = True
    'Show Dialog Box
    Opendialog.Action = 1
    

    tmp1$ = Opendialog.filename
    tmp$ = Dir$(tmp1$)
    If tmp$ <> "" Then
      msg$ = "This information in this file may be overwritten."
      msg$ = msg$ + Chr$(10) + Chr$(13)
      msg$ = msg$ + "Do you wish to change the name of the file?"
      response = MsgBox(msg$, vbYesNo, "Change File?")
      If response = vbYes Then
        Call ChgOutMenu_Click
      End If
    End If
    
    Call CheckFile(tmp1$, ibad)

    If ibad <> 1 Then
        er$ = "Filename is incorrect; please re-enter"
        ms = MsgBox(er$, vbExclamation, "ERROR")
        Call ChgOutMenu_Click
    End If
    
    OutFileName$ = tmp1$
    
    'Call ChkInp(InpFileName$)
    
    Call Form_Activate

    Exit Sub

OpenFileError1:
    If Err = CDERR_CANCEL Then Exit Sub
    m$ = "Error number " + Str$(Err)
    m$ = m$ + Chr$(13) + Chr$(10)
    m$ = m$ + Error$
    mb = MsgBox(m$, vbExclamation, "Error:")




End Sub

Private Sub ChgParmMenu_Click()
    Dim tmp1$, tmp$, msg$, er$, m$
    Dim response, ibad, ms, mb


    On Local Error GoTo OpenFileError

    'Set Dialog Box Title
    Opendialog.DialogTitle = "Open Input File"
    'Set Default Filename
    Opendialog.filename = ParmFileName$
    'Set Default Path
    Opendialog.InitDir = App.Path
    'Set Default Filters
    Opendialog.Filter = "All Files (*.*)|*.*|Input Files (*.INP)|*.INP"
    'Enable Cancel button
    Opendialog.CancelError = True
    'Show Dialog Box
    Opendialog.Action = 1
    

    tmp1$ = Opendialog.filename
    tmp$ = Dir$(tmp1$)
    If tmp$ <> "" Then
      msg$ = "This information in this file may be overwritten."
      msg$ = msg$ + Chr$(10) + Chr$(13)
      msg$ = msg$ + "Do you wish to change the name of the file?"
      response = MsgBox(msg$, vbYesNo, "Change File?")
      If response = vbYes Then
        Call ChgParmMenu_Click
      End If
    End If
    
    Call CheckFile(tmp1$, ibad)

    If ibad <> 1 Then
        er$ = "Filename is incorrect; please re-enter"
        ms = MsgBox(er$, vbExclamation, "ERROR")
        Call ChgParmMenu_Click
    End If
    
    ParmFileName$ = tmp1$
    
    If Dir$(ParmFileName$) <> "" Then
        Call ChkParm(ParmFileName$)
    End If

    Call Form_Activate

    Exit Sub

OpenFileError:
    If Err = CDERR_CANCEL Then Exit Sub
    m$ = "Error number " + Str$(Err)
    m$ = m$ + Chr$(13) + Chr$(10)
    m$ = m$ + Error$
    mb = MsgBox(m$, vbExclamation, "Error:")


End Sub

Private Sub ChgSfcFile_Click()
End Sub

Private Sub ChgSfcMenu_Click()
    Dim tmp1$, tmp$, msg$, er$, m$
    Dim response, ibad, ms, mb


    On Local Error GoTo OpenFileError2

    'Set Dialog Box Title
    Opendialog.DialogTitle = "Open Surface Data File"
    'Set Default Filename
    Opendialog.filename = SfcFileName$
    'Set Default Path
    Opendialog.InitDir = App.Path
    'Set Default Filters
    Opendialog.Filter = "All Files (*.*)|*.*|Met Files (*.MET)|*.MET"
    'Enable Cancel button
    Opendialog.CancelError = True
    'Show Dialog Box
    Opendialog.Action = 1
    

    tmp1$ = Opendialog.filename
    tmp$ = Dir$(tmp1$)
    If tmp$ = "" Then
      msg$ = "This file does not exist ."
      msg$ = msg$ + Chr$(10) + Chr$(13)
      response = MsgBox(msg$, vbExclamation, "ERROR")
      Call ChgSfcMenu_Click
    End If
    
    Call CheckFile(tmp1$, ibad)

    If ibad <> 1 Then
        er$ = "Filename is incorrect; please re-enter"
        ms = MsgBox(er$, vbExclamation, "ERROR")
        Call ChgSfcMenu_Click
    End If
    
    SfcFileName$ = tmp1$
    
    'Call ChkInp(InpFileName$)
    
    Call Form_Activate

    Exit Sub

OpenFileError2:
    If Err = CDERR_CANCEL Then Exit Sub
    m$ = "Error number " + Str$(Err)
    m$ = m$ + Chr$(13) + Chr$(10)
    m$ = m$ + Error$
    mb = MsgBox(m$, vbExclamation, "Error:")




End Sub

Private Sub ChgTd32Menu_Click()
    Dim tmp1$, tmp$, msg$, er$, m$
    Dim response, ibad, ms, mb

    On Local Error GoTo OpenFileError3

    'Set Dialog Box Title
    Opendialog.DialogTitle = "Open Surface Data File"
    'Set Default Filename
    Opendialog.filename = td32file$
    'Set Default Path
    Opendialog.InitDir = App.Path
    'Set Default Filters
    Opendialog.Filter = "All Files (*.*)|*.*"
    'Enable Cancel button
    Opendialog.CancelError = True
    'Show Dialog Box
    Opendialog.Action = 1
    

    tmp1$ = Opendialog.filename
    tmp$ = Dir$(tmp1$)
    If tmp$ = "" Then
      msg$ = "This file does not exist ."
      msg$ = msg$ + Chr$(10) + Chr$(13)
      response = MsgBox(msg$, vbExclamation, "ERROR")
      Call ChgTd32Menu_Click
    End If
    
    Call CheckFile(tmp1$, ibad)

    If ibad <> 1 Then
        er$ = "Filename is incorrect; please re-enter"
        ms = MsgBox(er$, vbExclamation, "ERROR")
        Call ChgSfcMenu_Click
    End If
    
    td32file$ = tmp1$

    msg$ = "Is this file in Fixed Format (If not, assumed to be in Variable Format)? "
    response = MsgBox(msg$, vbYesNo, "Format?")
    If response = vbYes Then
        TdFormOpt(0).Value = True
        tdform$ = "F"
    Else
        TdFormOpt(1).Value = True
        tdform$ = "V"
    End If
    
    'Call ChkInp(InpFileName$)
    
    Call Form_Activate

    Exit Sub

OpenFileError3:
    If Err = CDERR_CANCEL Then Exit Sub
    m$ = "Error number " + Str$(Err)
    m$ = m$ + Chr$(13) + Chr$(10)
    m$ = m$ + Error$
    mb = MsgBox(m$, vbExclamation, "Error:")




End Sub

Private Sub ExitMenu_Click()
    QuitButton_Click
End Sub

Private Sub Form_Activate()

    ParmFileLabel.Caption = ParmFileName$
    OutFileLabel.Caption = OutFileName$
    SfcFileLabel.Caption = SfcFileName$

    LatText.Text = lat
    LongText.Text = lon
    TimeZText.Text = timez
    WndHgtText.Text = wndhgt
    MeasZ0Text.Text = z0meas
    Applz0Text.Text = z0appl
    If doTD$ = "T" Then
        TD32Check.Value = 1
        Td32FileLabel = td32file$
        
        ChgTd32Menu.Enabled = True

        If tdform$ = "F" Then
            TdFormOpt(0).Value = True
        Else
            TdFormOpt(1).Value = True
        End If

    Else
        
        TD32Check.Value = 0
        TD32Frame.Visible = False
        ChgTd32Menu.Enabled = False

    End If

End Sub

Private Sub Form_Load()
    Dim Path$

    Path$ = App.Path + "\"
    ParmFileName$ = Path$ + "HRLYPROC.INP"

    TD32Check.Value = 0
    TD32Frame.Enabled = False
    TD32Frame.Visible = False

    TdFormOpt(0).Value = -1
    
    If Dir$(ParmFileName$) <> "" Then
        Call ChkParm(ParmFileName$)
    End If
        
    Call Form_Activate

End Sub

Private Sub QuitButton_Click()
    Dim msg$
    Dim mb
    
    msg$ = "Do you wish to Quit?"
    mb = MsgBox(msg$, MB_YesNo, "Quit")
    If mb <> IDYES Then
       Exit Sub
    Else
       End
    End If

End Sub

Private Sub QuitMenu_Click()
    QuitButton_Click
End Sub

Private Sub RunButton_Click()
    Dim runfile$, m$, er$, errorfile$
    Dim sh%, mb, de, tmp, ms


    GoSub ChkVals

    lat = Val(LatText.Text)
    lon = Val(LongText.Text)
    timez = Val(TimeZText.Text)
    wndhgt = Val(WndHgtText.Text)
    z0meas = Val(MeasZ0Text.Text)
    z0appl = Val(Applz0Text.Text)
    If TD32Check.Value = 1 Then
        doTD$ = "T"
        
        'msg$ = "Fixed Value is " + TdFormOpt(0).Value
        'msg$ = msg$ + "Varib. Value is " + TdFormOpt(1).Value
        'mb = MsgBox(msg$)

        If TdFormOpt(1).Value = 0 Then
            tdform$ = "F"
        Else
            tdform$ = "V"
        End If
    Else
        doTD$ = "F"
    End If

    Call WriteRSFile(ParmFileName$)

    runfile$ = App.Path + "\runcode.bat " + App.Path + "\hrlyproc " + ParmFileName$

    sh% = Shell(runfile$, 1)
    If sh% <= 0 Then
      m$ = "Unable to Shell " + runfile$
      mb = MsgBox(m$, MB_ICONEXCLAMATION, "ERROR:")
      Exit Sub
    End If

    While GetModuleUsage(sh%) > 0
        de = DoEvents()
    Wend
    
    'Write out error if it occurs

    errorfile$ = App.Path + "\code.err"
    
    If Dir$(errorfile$) <> "" Then
        Call WRITEERR(errorfile$)
    End If


    Exit Sub

ChkVals:

    er$ = ""

    If OutFileName$ = "" Then
        er$ = er$ + "Need a file for Output"
        er$ = er$ + Chr$(13) + Chr$(10)
    End If

    If Dir$(SfcFileName$) = "" Then
        er$ = er$ + "Need an existing file for hourly data"
        er$ = er$ + Chr$(13) + Chr$(10)
    ElseIf SfcFileName$ = "" Then
        er$ = er$ + "Need an existing file for hourly data"
        er$ = er$ + Chr$(13) + Chr$(10)
    End If


    tmp = Val(WndHgtText.Text)
    If tmp <= 0 Then
        er$ = er$ + "The Wind Measurement Height must be greater than zero"
        er$ = er$ + Chr$(13) + Chr$(10)
    End If
    
    
    tmp = Val(MeasZ0Text.Text)

    If tmp <= 0 Then
        er$ = er$ + "The Surface Roughness at Measurement Site must be greater than zero"
        er$ = er$ + Chr$(13) + Chr$(10)
    End If

    tmp = Val(Applz0Text.Text)
        
    If tmp <= 0 Then
        er$ = er$ + "The Surface Roughness at the Application Site must be greater than zero"
        er$ = er$ + Chr$(13) + Chr$(10)
    End If


    If TD32Check.Value = 1 Then
        If Dir$(td32file$) = "" Then
            er$ = er$ + "TD3240 File Needed"
            er$ = er$ + Chr$(13) + Chr$(10)
        ElseIf td32file$ = "" Then
            er$ = er$ + "TD3240 File Needed"
            er$ = er$ + Chr$(13) + Chr$(10)
        End If
    End If

    If er$ <> "" Then
        ms = MsgBox(er$, MB_OK, "ERROR")
        Exit Sub
    End If

    Return
    
End Sub

Private Sub RunMenu_Click()
    Call RunButton_Click
End Sub

Private Sub TD32Check_Click()
    If TD32Check.Value = 0 Then
        TD32Frame.Enabled = False
        TD32Frame.Visible = False
        ChgTd32Menu.Enabled = False
    Else
        TD32Frame.Enabled = True
        TD32Frame.Visible = True
        ChgTd32Menu.Enabled = True
    End If
End Sub

