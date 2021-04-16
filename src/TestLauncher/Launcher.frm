VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Begin VB.Form Form1 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "FATLaunch - FRAMES Auto Test Launcher"
   ClientHeight    =   3885
   ClientLeft      =   30
   ClientTop       =   420
   ClientWidth     =   8535
   Icon            =   "Launcher.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   ScaleHeight     =   3885
   ScaleWidth      =   8535
   StartUpPosition =   3  'Windows Default
   Begin VB.CommandButton Command6 
      BackColor       =   &H000080FF&
      Caption         =   "Restore FRAMES Configuration"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   372
      Left            =   4800
      MaskColor       =   &H008080FF&
      TabIndex        =   29
      Top             =   2880
      Width           =   3492
   End
   Begin VB.Frame Frame4 
      Caption         =   "Style of Windows"
      Height          =   1092
      Left            =   240
      TabIndex        =   26
      Top             =   120
      Width           =   1692
      Begin VB.OptionButton OptionA 
         Caption         =   "Classic Style"
         Height          =   372
         Index           =   1
         Left            =   240
         TabIndex        =   28
         Top             =   600
         Width           =   1200
      End
      Begin VB.OptionButton OptionA 
         Caption         =   "XP Style"
         Height          =   372
         Index           =   0
         Left            =   240
         TabIndex        =   27
         Top             =   240
         Width           =   1200
      End
   End
   Begin VB.TextBox Text5 
      Alignment       =   1  'Right Justify
      Height          =   288
      Left            =   1920
      TabIndex        =   24
      Top             =   3360
      Width           =   1000
   End
   Begin VB.Frame Frame2 
      Caption         =   "Version"
      Height          =   1092
      Left            =   1920
      TabIndex        =   16
      Top             =   120
      Width           =   1452
      Begin VB.OptionButton OptionV 
         Caption         =   "Frames 1x"
         Height          =   252
         Index           =   0
         Left            =   240
         TabIndex        =   18
         Top             =   240
         Width           =   1150
      End
      Begin VB.OptionButton OptionV 
         Caption         =   "Frames 2x"
         Height          =   252
         Index           =   1
         Left            =   240
         TabIndex        =   17
         Top             =   600
         Width           =   1150
      End
   End
   Begin VB.Frame Frame1 
      Caption         =   "Run to..."
      Height          =   1092
      Left            =   3360
      TabIndex        =   12
      Top             =   120
      Width           =   1332
      Begin VB.OptionButton OptionR 
         Caption         =   "Red"
         Height          =   252
         Index           =   0
         Left            =   240
         TabIndex        =   15
         Top             =   240
         Width           =   1000
      End
      Begin VB.OptionButton OptionR 
         Caption         =   "Yellow"
         Height          =   252
         Index           =   1
         Left            =   240
         TabIndex        =   14
         Top             =   480
         Width           =   1000
      End
      Begin VB.OptionButton OptionR 
         Caption         =   "Green"
         Height          =   252
         Index           =   2
         Left            =   240
         TabIndex        =   13
         Top             =   720
         Width           =   1000
      End
   End
   Begin MSComDlg.CommonDialog cdMain 
      Left            =   8160
      Top             =   0
      _ExtentX        =   688
      _ExtentY        =   688
      _Version        =   393216
   End
   Begin VB.CommandButton Command4 
      BackColor       =   &H000080FF&
      Caption         =   "Launch the test"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   372
      Left            =   4800
      MaskColor       =   &H008080FF&
      TabIndex        =   11
      Top             =   3360
      Width           =   3492
   End
   Begin VB.TextBox Text4 
      Alignment       =   1  'Right Justify
      Height          =   288
      Left            =   1920
      TabIndex        =   6
      Top             =   2880
      Width           =   1000
   End
   Begin VB.TextBox Text3 
      Height          =   288
      Left            =   1920
      TabIndex        =   5
      Top             =   2400
      Width           =   6100
   End
   Begin VB.TextBox Text2 
      Height          =   288
      Left            =   1920
      TabIndex        =   4
      Top             =   1920
      Width           =   6100
   End
   Begin VB.TextBox Text1 
      Height          =   288
      Left            =   1920
      TabIndex        =   3
      Top             =   1440
      Width           =   6100
   End
   Begin VB.CommandButton Command3 
      Caption         =   "..."
      Height          =   300
      Left            =   8040
      TabIndex        =   2
      Top             =   2400
      Width           =   300
   End
   Begin VB.CommandButton Command2 
      Caption         =   "..."
      Height          =   300
      Left            =   8040
      TabIndex        =   1
      Top             =   1920
      Width           =   300
   End
   Begin VB.CommandButton Command1 
      Caption         =   "..."
      Height          =   300
      Left            =   8040
      TabIndex        =   0
      Top             =   1440
      Width           =   300
   End
   Begin VB.Frame Frame3 
      Caption         =   "Stochastic Values"
      Height          =   1092
      Left            =   4680
      TabIndex        =   19
      Top             =   120
      Width           =   3612
      Begin VB.OptionButton OptionS 
         Caption         =   "Seeded random value"
         Height          =   372
         Index           =   1
         Left            =   240
         TabIndex        =   23
         Top             =   600
         Width           =   1600
      End
      Begin VB.OptionButton OptionS 
         Caption         =   "Maximum value"
         Height          =   252
         Index           =   3
         Left            =   1920
         TabIndex        =   22
         Top             =   600
         Width           =   1600
      End
      Begin VB.OptionButton OptionS 
         Caption         =   "Minimum value"
         Height          =   252
         Index           =   2
         Left            =   1920
         TabIndex        =   21
         Top             =   240
         Width           =   1600
      End
      Begin VB.OptionButton OptionS 
         Caption         =   "Default value"
         Height          =   252
         Index           =   0
         Left            =   240
         TabIndex        =   20
         Top             =   240
         Width           =   1600
      End
   End
   Begin VB.Label Label6 
      Caption         =   "Random  value seed"
      Height          =   252
      Left            =   240
      TabIndex        =   25
      Top             =   3360
      Width           =   1700
   End
   Begin VB.Label Label4 
      Caption         =   "Data input sleep factor"
      Height          =   252
      Left            =   240
      TabIndex        =   10
      Top             =   2880
      Width           =   1700
   End
   Begin VB.Label Label3 
      Caption         =   "Frames 2x directory"
      Height          =   252
      Left            =   240
      TabIndex        =   9
      Top             =   2400
      Width           =   1700
   End
   Begin VB.Label Label2 
      Caption         =   "Frames 1x directory"
      Height          =   252
      Left            =   240
      TabIndex        =   8
      Top             =   1920
      Width           =   1700
   End
   Begin VB.Label Label1 
      Caption         =   "Script/Batch file name"
      Height          =   252
      Left            =   240
      TabIndex        =   7
      Top             =   1440
      Width           =   1700
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim BLANK As String
Dim state As String
Dim FRAMES_INI As String

Private Sub Command1_Click()
'-- Initialize Common Dialog control
    With cdMain
        .Flags = cdlOFNPathMustExist
        .Flags = .Flags Or cdlOFNHideReadOnly
        .Flags = .Flags Or cdlOFNNoChangeDir
        .Flags = .Flags Or cdlOFNExplorer
        .Flags = .Flags Or cdlOFNNoValidate
        .Filter = "Script files|*.tst|Batch files|*.bat|All files|*.*"
        .FilterIndex = 2
        .FilterIndex = 1
    End With

    Dim x As Integer
    '-- Cheap way to use the common dialog box as a directory-picker
    x = 3

    cdMain.CancelError = True      'do not terminate on error

    On Error Resume Next           'I will hande errors

    cdMain.Action = 1              'Present "open" dialog

    '-- If FileTitle is null, user did not override the default (*.*)
    If cdMain.FileTitle <> "" Then x = Len(cdMain.FileTitle)

    If Err = 0 Then
        ChDrive cdMain.FileName
        Text1.Text = cdMain.FileName
    Else
    '-- User pressed "Cancel"
    End If

End Sub

Private Sub Command2_Click()
'-- Initialize Common Dialog control
    With cdMain
        .Flags = cdlOFNPathMustExist
        .Flags = .Flags Or cdlOFNHideReadOnly
        .Flags = .Flags Or cdlOFNNoChangeDir
        .Flags = .Flags Or cdlOFNExplorer
        .Flags = .Flags Or cdlOFNNoValidate
        .FileName = "*.*"
    End With

    Dim x As Integer
    '-- Cheap way to use the common dialog box as a directory-picker
    x = 3

    cdMain.CancelError = True      'do not terminate on error

    On Error Resume Next       'I will hande errors

    cdMain.Action = 1              'Present "open" dialog

    '-- If FileTitle is null, user did not override the default (*.*)
    If cdMain.FileTitle <> "" Then x = Len(cdMain.FileTitle)

    If Err = 0 Then
        ChDrive cdMain.FileName
        Text2.Text = Left(cdMain.FileName, (Len(cdMain.FileName) - x) - 1)
    Else
    '-- User pressed "Cancel"
    End If

End Sub

Private Sub Command3_Click()
'-- Initialize Common Dialog control
    With cdMain
        .Flags = cdlOFNPathMustExist
        .Flags = .Flags Or cdlOFNHideReadOnly
        .Flags = .Flags Or cdlOFNNoChangeDir
        .Flags = .Flags Or cdlOFNExplorer
        .Flags = .Flags Or cdlOFNNoValidate
        .FileName = "*.*"
    End With

    Dim x As Integer
    '-- Cheap way to use the common dialog box as a directory-picker
    x = 3

    cdMain.CancelError = True      'do not terminate on error

    On Error Resume Next       'I will hande errors

    cdMain.Action = 1              'Present "open" dialog

    '-- If FileTitle is null, user did not override the default (*.*)
    If cdMain.FileTitle <> "" Then x = Len(cdMain.FileTitle)

    If Err = 0 Then
        ChDrive cdMain.FileName
        Text3.Text = Left(cdMain.FileName, (Len(cdMain.FileName) - x) - 1)
    Else
    '-- User pressed "Cancel"
    End If

End Sub

Sub GetSettings()
Dim tmp As String
Dim path1x As String

  FRAMES_INI = App.Path + "\\FramesUI.ini"
  
  ReadIniString "App Path", "FUI", BLANK, path1x

  ReadIniString "CMD", "style", "0", tmp:       OptionA(CInt(tmp)).value = True
  ReadIniString "CMD", "version", "0", tmp:     OptionV(CInt(tmp)).value = True
  ReadIniString "CMD", "runto", "2", tmp:       OptionR(CInt(tmp)).value = True
  ReadIniString "CMD", "stoch", "1", tmp:       OptionS(CInt(tmp)).value = True
  ReadIniString "CMD", "testfile", path1x + "MEPAS_Tests\Test_Scripts\test.bat", tmp:    Text1.Text = tmp
  ReadIniString "CMD", "frames1x", path1x, tmp: Text2.Text = tmp
  ReadIniString "CMD", "frames2x", BLANK, tmp:  Text3.Text = tmp
  ReadIniString "CMD", "factor", "1.0", tmp:    Text4.Text = tmp
  ReadIniString "CMD", "seed", "7", tmp:        Text5.Text = tmp
  ReadIniString "CMD", "state", "0", state
  If state = "0" Then
    Command6.Caption = "Save FRAMES Configuration"
  Else
    Command6.Caption = "Restore FRAMES Configuration"
  End If
End Sub

Sub SetSettings()
  If OptionA(0).value Then WriteIniString "CMD", "style", "0"
  If OptionA(1).value Then WriteIniString "CMD", "style", "1"
  If OptionV(0).value Then WriteIniString "CMD", "version", "0"
  If OptionV(1).value Then WriteIniString "CMD", "version", "1"
  If OptionR(0).value Then WriteIniString "CMD", "runto", "0"
  If OptionR(1).value Then WriteIniString "CMD", "runto", "1"
  If OptionR(2).value Then WriteIniString "CMD", "runto", "2"
  If OptionS(0).value Then WriteIniString "CMD", "stoch", "0"
  If OptionS(1).value Then WriteIniString "CMD", "stoch", "1"
  If OptionS(2).value Then WriteIniString "CMD", "stoch", "2"
  If OptionS(3).value Then WriteIniString "CMD", "stoch", "3"
  
  WriteIniString "CMD", "testfile", Text1.Text
  WriteIniString "CMD", "frames1x", Text2.Text
  WriteIniString "CMD", "frames2x", Text3.Text
  If Val(Text4.Text) > 1 Then
    WriteIniString "CMD", "factor", CStr(Val(Text4.Text))
  Else
    Text4.Text = 1#
    WriteIniString "CMD", "factor", CStr(1#)
  End If
  If Val(Text5.Text) <> 0 Then
    WriteIniString "CMD", "seed", CStr(Val(Text5.Text))
  Else
    Text4.Text = 1#
    WriteIniString "CMD", "seed", CStr(7#)
  End If
  WriteIniString "CMD", "state", state
End Sub

Private Sub Command4_Click()
Dim runto As String
Dim style As String
Dim cmdline As String

  SetSettings
  If OptionA(0).value Then style = " 0 "
  If OptionA(1).value Then style = " 1 "
  
  If OptionR(0).value Then runto = " 0"
  If OptionR(1).value Then runto = " 1"
  If OptionR(2).value Then runto = " 2"
 
  If OptionS(0).value Then stoch = " 0 "
  If OptionS(1).value Then stoch = " 1 "
  If OptionS(2).value Then stoch = " 2 "
  If OptionS(3).value Then stoch = " 3 "
  
  If OptionV(0).value Then 'frames v1
    If Right(Text1.Text, 3) = "bat" Then
      cmdline = """" & Text1.Text & """ """ & Text2.Text & """" & style & Text4.Text & runto & stoch & Text5.Text
    Else
      On Error Resume Next
      ChDir Text2.Text
      If Err.Number = 0 Then
        cmdline = """" & Text2.Text & "\runscript.bat"" """ & Text1.Text & """ """ & Text2.Text & """" & style & Text4.Text & runto & stoch & Text5.Text
      Else
        On Error GoTo 0
        MsgBox Err.Description, vbCritical, Form1.Caption
        Exit Sub
      End If
      On Error GoTo 0
    End If
  End If
  If OptionV(1).value Then 'frames v2
    If Right(Text1.Text, 3) = "bat" Then
      cmdline = """" & Text1.Text & """ """ & Text3.Text & """" & style & Text4.Text & " """ & Text2.Text & """" & runto & stoch & Text5.Text
    Else
      On Error Resume Next
      ChDir Text3.Text
      If Err.Number = 0 Then
        cmdline = """" & Text3.Text & "\runscript.bat"" """ & Text1.Text & """ """ & Text3.Text & """" & style & Text4.Text & " """ & Text2.Text & """" & runto & stoch & Text5.Text
      Else
        On Error GoTo 0
        MsgBox Err.Description, vbCritical, Form1.Caption
        Exit Sub
      End If
      On Error GoTo 0
    End If
  End If
  
  On Error Resume Next
  Shell cmdline
  If Err.Number <> 0 Then
    If Right(Text1.Text, 3) = "bat" Then
       MsgBox "Failure to launch testing batch file", vbCritical
    Else
       MsgBox "Failure to launch runscript batch file", vbCritical
    End If
  End If
End Sub


Private Sub Command6_Click()
Dim tlabel As String
Dim tstate As String
Dim cmdline As String

  If state = "0" Then
    cmdline = """" & App.Path & "\save.bat"" """ & Text2.Text & """"
    tlabel = "Restore FRAMES Configuration"
    tstate = "1"
  Else
    cmdline = """" & App.Path & "\restore.bat"" """ & Text2.Text & """"
    tlabel = "Save FRAMES Configuration"
    tstate = "0"
  End If
  
  On Error Resume Next
  Shell cmdline, vbNormalFocus
  If Err.Number <> 0 Then
    MsgBox "Failure to launch 'restore' batch file", vbCritical
  Else
    state = tstate
    Command6.Caption = tlabel
    SetSettings
  End If
End Sub

Private Sub Form_Load()
  BLANK = ""
  GetSettings
End Sub

Function WriteIniString(AppName As String, KeyName As String, value As String) As Boolean
  WriteIniString = WritePrivateProfileString(AppName, KeyName, value, FRAMES_INI)
End Function

Function ReadIniString(AppName As String, KeyName As String, Default As String, value As String) As Boolean
Dim i As Long
Dim pszVal As String

  value = Default
  ReadIniString = False
  
  pszVal = String(255, 0)
  i = GetPrivateProfileString(AppName, KeyName, BLANK, pszVal, Len(pszVal), FRAMES_INI)
  If i > 0 Then
    ReadIniString = True
    value = LCase$(Left$(pszVal, i))
  End If
End Function

Private Sub OptionV_Click(Index As Integer)
  If Index = 0 Then
   Label3.Visible = False
   Text3.Visible = False
   Command3.Visible = False
  End If
  If Index = 1 Then
   Label3.Visible = True
   Text3.Visible = True
   Command3.Visible = True
  End If
End Sub
