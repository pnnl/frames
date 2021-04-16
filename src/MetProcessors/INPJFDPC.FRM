VERSION 2.00
Begin Form InpJFDForm 
   BackColor       =   &H00C000C0&
   Caption         =   "Input for JFDPROC"
   ClientHeight    =   5745
   ClientLeft      =   1005
   ClientTop       =   945
   ClientWidth     =   7905
   Height          =   6435
   Left            =   945
   LinkTopic       =   "Form1"
   ScaleHeight     =   5745
   ScaleWidth      =   7905
   Top             =   315
   Width           =   8025
   Begin CommonDialog OpenDialog 
      Left            =   5400
      Top             =   480
   End
   Begin CommandButton SpdMonButton 
      Caption         =   "Set Wind Speeds and Monthly Values"
      Height          =   615
      Left            =   1320
      TabIndex        =   9
      Top             =   4200
      Width           =   4335
   End
   Begin CommandButton QuitButton 
      Caption         =   "Quit"
      Height          =   495
      Left            =   4800
      TabIndex        =   11
      Top             =   5040
      Width           =   1815
   End
   Begin CommandButton RunButton 
      Caption         =   "Run JFDPROC"
      Height          =   495
      Left            =   960
      TabIndex        =   10
      Top             =   5040
      Width           =   2055
   End
   Begin Frame ClassFrame 
      Caption         =   "Number of Classes"
      Height          =   855
      Left            =   360
      TabIndex        =   18
      Top             =   3240
      Width           =   7335
      Begin Frame Frame2 
         Height          =   615
         Left            =   5280
         TabIndex        =   24
         Top             =   120
         Width           =   1695
         Begin OptionButton WndDirOpt 
            Caption         =   "36"
            Height          =   255
            Index           =   1
            Left            =   960
            TabIndex        =   8
            Top             =   240
            Width           =   615
         End
         Begin OptionButton WndDirOpt 
            Caption         =   "16"
            Height          =   255
            Index           =   0
            Left            =   120
            TabIndex        =   7
            Top             =   240
            Width           =   735
         End
      End
      Begin TextBox StabClassText 
         Height          =   285
         Left            =   2160
         TabIndex        =   6
         Top             =   360
         Width           =   495
      End
      Begin Label Label10 
         Caption         =   "Wind Direction"
         Height          =   255
         Left            =   3600
         TabIndex        =   23
         Top             =   360
         Width           =   1575
      End
      Begin Label Label5 
         AutoSize        =   -1  'True
         Caption         =   "Stability Classes"
         Height          =   195
         Left            =   240
         TabIndex        =   22
         Top             =   360
         Width           =   1395
      End
   End
   Begin Frame Frame1 
      Caption         =   "Station Information"
      Height          =   1335
      Left            =   360
      TabIndex        =   13
      Top             =   1800
      Width           =   7335
      Begin TextBox HourJFDText 
         Height          =   285
         Left            =   3480
         TabIndex        =   5
         Text            =   "8760"
         Top             =   960
         Width           =   615
      End
      Begin TextBox ApplZ0Text 
         Height          =   285
         Left            =   6360
         TabIndex        =   4
         Text            =   " "
         Top             =   600
         Width           =   855
      End
      Begin TextBox MeasZ0Text 
         Height          =   285
         Left            =   2760
         TabIndex        =   3
         Text            =   " "
         Top             =   600
         Width           =   855
      End
      Begin TextBox WndHgtText 
         Height          =   285
         Left            =   6360
         TabIndex        =   2
         Text            =   " "
         Top             =   240
         Width           =   855
      End
      Begin TextBox LatText 
         Height          =   285
         Left            =   2760
         TabIndex        =   1
         Text            =   " "
         Top             =   240
         Width           =   855
      End
      Begin Label Label8 
         AutoSize        =   -1  'True
         Caption         =   "z0 At Measurement Site (m)"
         Height          =   195
         Left            =   120
         TabIndex        =   21
         Top             =   600
         Width           =   2340
      End
      Begin Label Label9 
         AutoSize        =   -1  'True
         Caption         =   "z0 at Application Site (m)"
         Height          =   195
         Left            =   3720
         TabIndex        =   17
         Top             =   600
         Width           =   2145
      End
      Begin Label Label7 
         AutoSize        =   -1  'True
         Caption         =   "Wind Measurement Height (m)"
         Height          =   195
         Left            =   3720
         TabIndex        =   16
         Top             =   240
         Width           =   2565
      End
      Begin Label Label6 
         AutoSize        =   -1  'True
         Caption         =   "Number of Hour in JFD"
         Height          =   195
         Left            =   1320
         TabIndex        =   15
         Top             =   960
         Width           =   1950
      End
      Begin Label Label4 
         AutoSize        =   -1  'True
         Caption         =   "Latitude (deg)"
         Height          =   195
         Left            =   120
         TabIndex        =   14
         Top             =   240
         Width           =   1200
      End
   End
   Begin Label JfdFileLabel 
      AutoSize        =   -1  'True
      Caption         =   "Label13"
      Height          =   195
      Left            =   360
      TabIndex        =   27
      Top             =   1560
      Width           =   690
   End
   Begin Label OutFileLabel 
      AutoSize        =   -1  'True
      Caption         =   "Label12"
      Height          =   195
      Left            =   360
      TabIndex        =   26
      Top             =   960
      Width           =   690
   End
   Begin Label ParmFileLabel 
      AutoSize        =   -1  'True
      Caption         =   "Label11"
      Height          =   195
      Left            =   360
      TabIndex        =   25
      Top             =   360
      Width           =   690
   End
   Begin Label SpdMonLabel 
      AutoSize        =   -1  'True
      Caption         =   "Input Required"
      Height          =   195
      Left            =   6000
      TabIndex        =   19
      Top             =   4320
      Width           =   1275
   End
   Begin Label Label1 
      AutoSize        =   -1  'True
      Caption         =   "Information Saved in File:"
      Height          =   195
      Left            =   360
      TabIndex        =   20
      Top             =   120
      Width           =   2190
   End
   Begin Label Label3 
      AutoSize        =   -1  'True
      Caption         =   "Path and Name of Joint Frequency Distribution File"
      Height          =   195
      Left            =   360
      TabIndex        =   12
      Top             =   1320
      Width           =   4335
   End
   Begin Label Label2 
      AutoSize        =   -1  'True
      Caption         =   "Path and Name of Output File:"
      Height          =   195
      Left            =   360
      TabIndex        =   0
      Top             =   720
      Width           =   2595
   End
   Begin Menu RunMenu 
      Caption         =   "&Run JFDPROC"
   End
   Begin Menu FileMenu 
      Caption         =   "&Files"
      Begin Menu ChgParmMenu 
         Caption         =   "Change &Info Saved File"
      End
      Begin Menu ChgOutMenu 
         Caption         =   "Change &Output File"
      End
      Begin Menu ChgJFDMenu 
         Caption         =   "Change &JFD File"
      End
   End
   Begin Menu QuitMenu 
      Caption         =   "&Quit"
   End
End
Option Explicit

Sub ChgJFDMenu_Click ()
    Dim tmp1$, tmp$, msg$, er$, m$
    Dim response, ibad, ms, mb


    On Local Error GoTo OpenFileError2

    'Set Dialog Box Title
    Opendialog.DialogTitle = "Open JFD File"
    'Set Default Filename
    Opendialog.Filename = JFDFileName$
    'Set Default Path
    Opendialog.InitDir = app.Path
    'Set Default Filters
    Opendialog.Filter = "All Files (*.*)|*.*|JFD Files (*.JFD)|*.JFD"
    'Enable Cancel button
    Opendialog.CancelError = True
    'Show Dialog Box
    Opendialog.Action = 1
    

    tmp1$ = Opendialog.Filename
    tmp$ = Dir$(tmp1$)
    If tmp$ = "" Then
      msg$ = "This file does not exist ."
      msg$ = msg$ + Chr$(10) + Chr$(13)
      response = MsgBox(msg$, MB_OK, "ERROR")
      Call ChgJFDMenu_Click
    End If
    
    Call CheckFile(tmp1$, ibad)

    If ibad <> 1 Then
        er$ = "Filename is incorrect; please re-enter"
        ms = MsgBox(er$, MB_OK, "ERROR")
        Call ChgJFDMenu_Click
    End If
    
    JFDFileName$ = tmp1$
    
    'Call ChkInp(InpFileName$)
    
    Call Form_Activate

    Exit Sub

OpenFileError2:
    If Err = CDERR_CANCEL Then Exit Sub
    m$ = "Error number " + Str$(Err)
    m$ = m$ + Chr$(13) + Chr$(10)
    m$ = m$ + Error$
    mb = MsgBox(m$, MB_OK + MB_ICONEXCLAMATION, "Error:")


End Sub

Sub ChgOutMenu_Click ()
    Dim tmp1$, tmp$, msg$, er$, m$
    Dim response, ibad, ms, mb

    On Local Error GoTo OpenFileError1

    'Set Dialog Box Title
    Opendialog.DialogTitle = "Open Input File"
    'Set Default Filename
    Opendialog.Filename = OutFileName$
    'Set Default Path
    Opendialog.InitDir = app.Path
    'Set Default Filters
    Opendialog.Filter = "All Files (*.*)|*.*|Output Files (*.OUT)|*.OUT"
    'Enable Cancel button
    Opendialog.CancelError = True
    'Show Dialog Box
    Opendialog.Action = 1
    

    tmp1$ = Opendialog.Filename
    tmp$ = Dir$(tmp1$)
    If tmp$ <> "" Then
      msg$ = "This information in this file may be overwritten."
      msg$ = msg$ + Chr$(10) + Chr$(13)
      msg$ = msg$ + "Do you wish to change the name of the file?"
      response = MsgBox(msg$, MB_YESNO, "Change File?")
      If response = IDYES Then
        Call ChgOutMenu_Click
      End If
    End If
    
    Call CheckFile(tmp1$, ibad)

    If ibad <> 1 Then
        er$ = "Filename is incorrect; please re-enter"
        ms = MsgBox(er$, MB_OK, "ERROR")
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
    mb = MsgBox(m$, MB_OK + MB_ICONEXCLAMATION, "Error:")


End Sub

Sub ChgParmMenu_Click ()
    Dim tmp1$, tmp$, msg$, er$, m$
    Dim response, ibad, ms, mb


    On Local Error GoTo OpenFileError

    'Set Dialog Box Title
    Opendialog.DialogTitle = "Open Input File"
    'Set Default Filename
    Opendialog.Filename = ParmFileName$
    'Set Default Path
    Opendialog.InitDir = app.Path
    'Set Default Filters
    Opendialog.Filter = "All Files (*.*)|*.*|Input Files (*.INP)|*.INP"
    'Enable Cancel button
    Opendialog.CancelError = True
    'Show Dialog Box
    Opendialog.Action = 1
    

    tmp1$ = Opendialog.Filename
    tmp$ = Dir$(tmp1$)
    If tmp$ <> "" Then
      msg$ = "This information in this file may be overwritten."
      msg$ = msg$ + Chr$(10) + Chr$(13)
      msg$ = msg$ + "Do you wish to change the name of the file?"
      response = MsgBox(msg$, MB_YESNO, "Change File?")
      If response = IDYES Then
        Call ChgParmMenu_Click
      End If
    End If
    
    Call CheckFile(tmp1$, ibad)

    If ibad <> 1 Then
        er$ = "Filename is incorrect; please re-enter"
        ms = MsgBox(er$, MB_OK, "ERROR")
        Call ChgParmMenu_Click
    End If
    
    ParmFileName$ = tmp1$
    
    If Dir$(ParmFileName$) <> "" Then
        Call ChkParm(ParmFileName$)
        LatText.Text = lat
        WndHgtText.Text = wndhgt
        MeasZ0Text.Text = z0meas
        ApplZ0Text.Text = z0appl
        StabClassText.Text = numstab
        HourJfdText.Text = hrjfd

    End If

    Call Form_Activate

    Exit Sub

OpenFileError:
    If Err = CDERR_CANCEL Then Exit Sub
    m$ = "Error number " + Str$(Err)
    m$ = m$ + Chr$(13) + Chr$(10)
    m$ = m$ + Error$
    mb = MsgBox(m$, MB_OK + MB_ICONEXCLAMATION, "Error:")


End Sub

Sub ExitMenu_Click ()
    QuitButton_Click
End Sub

Sub Form_Activate ()
    
    ParmFileLabel.Caption = ParmFileName$
    OutFileLabel.Caption = OutFileName$
    JFDFileLabel.Caption = JFDFileName$

    If NumWndSpd > 1 And NumWndSpd < 9 Then
        SpdMonLabel = "Done"
        RunButton.Enabled = True
        RunMenu.Enabled = True
    Else
        RunButton.Enabled = False
        RunMenu.Enabled = False
    End If

End Sub

Sub Form_Load ()
    Dim Path$
    
    Path$ = app.Path + "\"
    
    ParmFileName$ = Path$ + "JFDPROC.INP"
    OutFileName$ = ""
    JFDFileName$ = ""

    WndDirOpt(0).Value = True

    RunButton.Enabled = False
    RunMenu.Enabled = False

    SpdMonLabel.Caption = "Input Required"
        
    If Dir$(ParmFileName$) <> "" Then
        Call ChkParm(ParmFileName$)
    End If
    LatText.Text = lat
    WndHgtText.Text = wndhgt
    MeasZ0Text.Text = z0meas
    ApplZ0Text.Text = z0appl
    HourJfdText.Text = hrjfd
    StabClassText.Text = numstab
    If Numwnddir <> 36 Then
        WndDirOpt(0).Value = True
    Else
        WndDirOpt(1).Value = True
    End If

    Call Form_Activate
End Sub

Sub QuitButton_Click ()
    Dim msg$
    Dim mb

    msg$ = "Do you wish to Quit?"
    mb = MsgBox(msg$, MB_YESNO, "Quit")
    If mb <> IDYES Then
       Exit Sub
    Else
       End
    End If

End Sub

Sub QuitMenu_Click ()
    QuitButton_Click
End Sub

Sub RunButton_Click ()
    Dim er$, nwfile$, runfile$, errorfile$, m$
    Dim sh%, mb, de, tmp, ms

    GoSub ChkVals

    lat = Val(LatText.Text)
    wndhgt = Val(WndHgtText.Text)
    z0meas = Val(MeasZ0Text.Text)
    z0appl = Val(ApplZ0Text.Text)
    hrjfd = Val(HourJfdText.Text)
    numstab = Val(StabClassText.Text)
    

    If WndDirOpt(0).Value <> False Then
        Numwnddir = 16
    Else
        Numwnddir = 36
    End If

    nwfile$ = ParmFileName$

    Call WriteRSFile(nwfile$)

    runfile$ = app.Path + "\runcode.bat " + app.Path + "\jjfdproc " + nwfile$

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

    errorfile$ = app.Path + "\code.err"
    
    If Dir$(errorfile$) <> "" Then
        Call WRITEERR(errorfile$)
    End If

    Exit Sub

ChkVals:
    er$ = ""

    tmp = Val(WndHgtText.Text)
    If tmp <= 0 Then
        er$ = er$ + "The Wind Measurement Height must be greater than zero"
        er$ = er$ + Chr$(13) + Chr$(10)
    End If

    tmp = Val(MeasZ0Text.Text)
    If tmp <= 0 Then
        er$ = er$ + "The Measurement Surface Roughness must be greater than zero"
        er$ = er$ + Chr$(13) + Chr$(10)
    End If
        
    tmp = Val(ApplZ0Text.Text)
    If tmp <= 0 Then
        er$ = er$ + "The Application Surface Roughness must be greater than zero"
        er$ = er$ + Chr$(13) + Chr$(10)
    End If

    tmp = Val(HourJfdText.Text)
    If tmp <= 0 Then
        er$ = er$ + "The Hours in the JFD must be greater than zero"
        er$ = er$ + Chr$(13) + Chr$(10)
    End If

    tmp = Val(StabClassText.Text)
    If tmp < 1 Or tmp > 7 Then
        er$ = er$ + "The Number of Stability Classes must be between 1 and 7"
        er$ = er$ + Chr$(13) + Chr$(10)
    End If

    If er$ <> "" Then
        ms = MsgBox(er$, MB_OK, "ERROR")
        Exit Sub
    End If

    Return


End Sub

Sub RunMenu_Click ()
    Call RunButton_Click
End Sub

Sub SpdMonButton_Click ()
    Dim l$
    Dim i

    InpSpdMonForm.WSpdClassText.Text = NumWndSpd
    l$ = ""
    For i = 1 To NumWndSpd - 1
        l$ = l$ + MidVal(i) + ", "
    Next i
    l$ = l$ + MidVal(NumWndSpd)
    InpSpdMonForm.WSpdMidText.Text = l$

    'mg$ = "Do Prcp = " + DoPrcp$
    'mb = MsgBox(mg$)

    If DoPrcp$ = "T" Then
        InpSpdMonForm.DoPrcpCheck.Value = 1
    Else
        InpSpdMonForm.DoPrcpCheck.Value = 0
    End If

    For i = 1 To 12
        InpSpdMonForm.MonMaxTempText(i - 1).Text = MaxTemp(i)
        InpSpdMonForm.MonMinTempText(i - 1).Text = MinTemp(i)
    Next i
    For i = 1 To 5
        InpSpdMonForm.LgtRainText(i - 1).Text = LgtRain(i)
        InpSpdMonForm.ModRainText(i - 1).Text = ModRain(i)
        InpSpdMonForm.LgtSnowText(i - 1).Text = LgtSnow(i)
        InpSpdMonForm.ModSnowText(i - 1).Text = ModSnow(i)
    Next i
    
    If InpJFDForm.WindowState = 1 Then
        InpJFDForm.WindowState = 0
    End If
    InpJFDForm.Hide
    InpSpdMonForm.Show
    InpSpdMonForm.SetFocus

End Sub

Sub TD32Check_Click ()
End Sub

