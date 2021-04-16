VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Begin VB.Form Form1 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "JFD Generator"
   ClientHeight    =   6000
   ClientLeft      =   120
   ClientTop       =   804
   ClientWidth     =   8172
   Icon            =   "JFDGenerator.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   ScaleHeight     =   6000
   ScaleWidth      =   8172
   StartUpPosition =   3  'Windows Default
   Begin VB.Frame Frame1 
      Height          =   5892
      Left            =   120
      TabIndex        =   28
      Top             =   0
      Width           =   7932
      Begin VB.OptionButton Option1 
         Caption         =   "NOAA Met File [path\name.dat] "
         Height          =   192
         Index           =   1
         Left            =   120
         TabIndex        =   1
         Top             =   840
         Width           =   2652
      End
      Begin VB.OptionButton Option1 
         Caption         =   "NOAA Met File [path\name.sam] "
         Height          =   192
         Index           =   0
         Left            =   120
         TabIndex        =   0
         Top             =   360
         Value           =   -1  'True
         Width           =   2652
      End
      Begin VB.Frame Frame3 
         BorderStyle     =   0  'None
         Height          =   4452
         Left            =   120
         TabIndex        =   29
         Top             =   1320
         Width           =   7692
         Begin VB.CommandButton Command4 
            Caption         =   "..."
            Height          =   300
            Left            =   7320
            TabIndex        =   46
            Top             =   120
            Width           =   300
         End
         Begin VB.Frame Frame4 
            BorderStyle     =   0  'None
            Caption         =   "Station Info"
            Enabled         =   0   'False
            Height          =   2292
            Left            =   120
            TabIndex        =   38
            Top             =   2040
            Visible         =   0   'False
            Width           =   4812
            Begin VB.TextBox state 
               Height          =   288
               Left            =   3600
               TabIndex        =   10
               Top             =   480
               Width           =   1000
            End
            Begin VB.TextBox lng 
               Alignment       =   1  'Right Justify
               Height          =   288
               Left            =   3600
               TabIndex        =   13
               Top             =   1560
               Width           =   1000
            End
            Begin VB.TextBox lat 
               Alignment       =   1  'Right Justify
               Height          =   288
               Left            =   3600
               TabIndex        =   12
               Top             =   1200
               Width           =   1000
            End
            Begin VB.TextBox zone 
               Alignment       =   1  'Right Justify
               Height          =   288
               Left            =   3600
               TabIndex        =   11
               Top             =   840
               Width           =   1000
            End
            Begin VB.TextBox elevation 
               Alignment       =   1  'Right Justify
               Height          =   288
               Left            =   3600
               TabIndex        =   14
               Top             =   1920
               Width           =   1000
            End
            Begin VB.TextBox station 
               Height          =   288
               Left            =   2040
               TabIndex        =   9
               Top             =   120
               Width           =   2532
            End
            Begin VB.Label Label12 
               Caption         =   "State abbreviation"
               Height          =   252
               Left            =   360
               TabIndex        =   44
               Top             =   480
               Width           =   2496
            End
            Begin VB.Label Label11 
               Caption         =   "Longitude (xx.xx degrees)"
               Height          =   252
               Left            =   360
               TabIndex        =   43
               Top             =   1560
               Width           =   2496
            End
            Begin VB.Label Label10 
               Caption         =   "Latitude (xx.xx degrees)"
               Height          =   252
               Left            =   360
               TabIndex        =   42
               Top             =   1200
               Width           =   2496
            End
            Begin VB.Label Label9 
               Caption         =   "Time Zone"
               Height          =   252
               Left            =   360
               TabIndex        =   41
               Top             =   840
               Width           =   2496
            End
            Begin VB.Label Label5 
               Caption         =   "Elevation (m)"
               Height          =   252
               Left            =   360
               TabIndex        =   40
               Top             =   1920
               Width           =   2496
            End
            Begin VB.Label Label6 
               Caption         =   "Station name"
               Height          =   252
               Left            =   120
               TabIndex        =   39
               Top             =   120
               Width           =   1896
            End
         End
         Begin VB.TextBox surface 
            Alignment       =   1  'Right Justify
            Height          =   288
            Left            =   3720
            TabIndex        =   8
            Top             =   1680
            Width           =   1000
         End
         Begin VB.TextBox anom 
            Alignment       =   1  'Right Justify
            Height          =   288
            Left            =   3720
            TabIndex        =   7
            Top             =   1320
            Width           =   1000
         End
         Begin VB.TextBox designation 
            Height          =   288
            Left            =   3720
            TabIndex        =   5
            Top             =   600
            Width           =   1000
         End
         Begin VB.TextBox year 
            Alignment       =   1  'Right Justify
            Height          =   288
            Left            =   3720
            TabIndex        =   6
            Top             =   960
            Width           =   1000
         End
         Begin VB.TextBox Text2 
            Enabled         =   0   'False
            Height          =   288
            Left            =   2760
            TabIndex        =   4
            Top             =   120
            Width           =   4536
         End
         Begin VB.CommandButton Command3 
            Caption         =   "Generate JFD"
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   372
            Left            =   5160
            TabIndex        =   27
            Top             =   3960
            Width           =   2052
         End
         Begin VB.CheckBox Mon 
            Caption         =   "January"
            Height          =   192
            Index           =   0
            Left            =   5160
            TabIndex        =   15
            Top             =   840
            Value           =   1  'Checked
            Width           =   1500
         End
         Begin VB.CheckBox Mon 
            Caption         =   "Febuary"
            Height          =   192
            Index           =   1
            Left            =   5160
            TabIndex        =   16
            Top             =   1080
            Value           =   1  'Checked
            Width           =   1500
         End
         Begin VB.CheckBox Mon 
            Caption         =   "March"
            Height          =   192
            Index           =   2
            Left            =   5160
            TabIndex        =   17
            Top             =   1320
            Value           =   1  'Checked
            Width           =   1500
         End
         Begin VB.CheckBox Mon 
            Caption         =   "April"
            Height          =   192
            Index           =   3
            Left            =   5160
            TabIndex        =   18
            Top             =   1560
            Value           =   1  'Checked
            Width           =   1500
         End
         Begin VB.CheckBox Mon 
            Caption         =   "May"
            Height          =   192
            Index           =   4
            Left            =   5160
            TabIndex        =   19
            Top             =   1800
            Value           =   1  'Checked
            Width           =   1500
         End
         Begin VB.CheckBox Mon 
            Caption         =   "June"
            Height          =   192
            Index           =   5
            Left            =   5160
            TabIndex        =   20
            Top             =   2040
            Value           =   1  'Checked
            Width           =   1500
         End
         Begin VB.CheckBox Mon 
            Caption         =   "July"
            Height          =   192
            Index           =   6
            Left            =   5160
            TabIndex        =   21
            Top             =   2280
            Value           =   1  'Checked
            Width           =   1500
         End
         Begin VB.CheckBox Mon 
            Caption         =   "August"
            Height          =   192
            Index           =   7
            Left            =   5160
            TabIndex        =   22
            Top             =   2520
            Value           =   1  'Checked
            Width           =   1500
         End
         Begin VB.CheckBox Mon 
            Caption         =   "September"
            Height          =   192
            Index           =   8
            Left            =   5160
            TabIndex        =   23
            Top             =   2760
            Value           =   1  'Checked
            Width           =   1500
         End
         Begin VB.CheckBox Mon 
            Caption         =   "October"
            Height          =   192
            Index           =   9
            Left            =   5160
            TabIndex        =   24
            Top             =   3000
            Value           =   1  'Checked
            Width           =   1500
         End
         Begin VB.CheckBox Mon 
            Caption         =   "November"
            Height          =   192
            Index           =   10
            Left            =   5160
            TabIndex        =   25
            Top             =   3240
            Value           =   1  'Checked
            Width           =   1500
         End
         Begin VB.CheckBox Mon 
            Caption         =   "December"
            Height          =   192
            Index           =   11
            Left            =   5160
            TabIndex        =   26
            Top             =   3480
            Value           =   1  'Checked
            Width           =   1500
         End
         Begin VB.Label Label1 
            Caption         =   "Months to Include"
            Height          =   252
            Left            =   5040
            TabIndex        =   45
            Top             =   600
            Width           =   1572
         End
         Begin VB.Label Label8 
            Caption         =   "Surface roughness length (cm)"
            Height          =   252
            Left            =   240
            TabIndex        =   37
            Top             =   1680
            Width           =   2496
         End
         Begin VB.Label Label7 
            Caption         =   "Anenmometer height (m)"
            Height          =   252
            Left            =   240
            TabIndex        =   36
            Top             =   1320
            Width           =   2496
         End
         Begin VB.Label Label3 
            Caption         =   "NOAA designation"
            Height          =   252
            Left            =   240
            TabIndex        =   35
            Top             =   600
            Width           =   2496
         End
         Begin VB.Label Label4 
            Caption         =   "Starting year"
            Height          =   252
            Left            =   240
            TabIndex        =   34
            Top             =   960
            Width           =   2496
         End
         Begin VB.Label Label2 
            Caption         =   "Output File [path\name]"
            Height          =   252
            Left            =   240
            TabIndex        =   33
            Top             =   120
            Width           =   2052
         End
      End
      Begin VB.CommandButton Command1 
         Caption         =   "..."
         Height          =   300
         Left            =   7440
         TabIndex        =   3
         Top             =   360
         Width           =   300
      End
      Begin VB.TextBox Text1 
         Height          =   288
         Left            =   2880
         TabIndex        =   2
         Top             =   360
         Width           =   4536
      End
      Begin MSComDlg.CommonDialog cdMain 
         Left            =   7440
         Top             =   360
         _ExtentX        =   677
         _ExtentY        =   677
         _Version        =   393216
      End
      Begin VB.Line Line1 
         BorderWidth     =   2
         X1              =   120
         X2              =   7800
         Y1              =   1320
         Y2              =   1320
      End
   End
   Begin VB.Frame Frame2 
      Enabled         =   0   'False
      Height          =   5892
      Left            =   120
      TabIndex        =   30
      Top             =   0
      Width           =   7932
      Begin VB.TextBox Text3 
         Height          =   4812
         Left            =   120
         MultiLine       =   -1  'True
         TabIndex        =   32
         Text            =   "JFDGenerator.frx":0442
         Top             =   240
         Width           =   7692
      End
      Begin VB.CommandButton Command2 
         Caption         =   "Back to Generator"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   372
         Left            =   5280
         TabIndex        =   31
         Top             =   5280
         Width           =   2052
      End
   End
   Begin VB.Menu mnuHelp 
      Caption         =   "Help"
      Begin VB.Menu howto 
         Caption         =   "How to..."
      End
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim BLANK As String
Const JFDGEN_IN = "JFDGen.in"
Const JFDGEN_ERR = "JFD.err"
Const JFDGEN_OUT = "JFDGen.out"
Const JFD = "JFD"
Const STR = "STR"

Private Sub Command1_Click()
'-- Initialize Common Dialog control
    With cdMain
        .Flags = cdlOFNPathMustExist
        .Flags = .Flags Or cdlOFNHideReadOnly
        .Flags = .Flags Or cdlOFNNoChangeDir
        .Flags = .Flags Or cdlOFNExplorer
        If Option1(0).value Then
          .Filter = "SAM files (*.sam)|*.sam|All files (*.*)|*.*"
        Else
          .Filter = "DAT files (*.dat)|*.dat|All files (*.*)|*.*"
        End If
        .FilterIndex = 1
    End With

    cdMain.CancelError = True      'do not terminate on error
    On Error Resume Next           'I will hande errors
    cdMain.Action = 1              'Present "open" dialog

    If Err = 0 Then
        ChDrive cdMain.fileName
        Text1.Text = cdMain.fileName
    Else
    '-- User pressed "Cancel"
    End If

End Sub

Private Sub Command4_Click()
'-- Initialize Common Dialog control
    With cdMain
        .Flags = cdlOFNPathMustExist
        .Flags = .Flags Or cdlOFNHideReadOnly
        .Flags = .Flags Or cdlOFNNoChangeDir
        .Flags = .Flags Or cdlOFNExplorer
        .Filter = "Joint Frequency Data files (*.jfd)|*.jfd|All files (*.*)|*.*"
        .FilterIndex = 1
    End With

    cdMain.CancelError = True      'do not terminate on error
    On Error Resume Next           'I will hande errors
    cdMain.ShowOpen                'Present "open" dialog

    If Err = 0 Then
      Text2.Text = cdMain.fileName
    Else
    '-- User pressed "Cancel"
    End If
End Sub

Private Sub Command2_Click()
  Frame1.ZOrder 0
  Frame1.Enabled = True
  Text3.Text = ""
End Sub

Sub GetSettings()
Dim i As Long
Dim tmp As String
Dim path1x As String

  ReadIniString "App Path", "FUI", BLANK, path1x

  For i = 0 To 11
    ReadIniString JFD, "Month" & CStr(i), "1", tmp
    Mon(i).value = Val(tmp)
  Next
  ReadIniString JFD, "NOAAtype", "0", tmp:   Option1(Val(tmp)).value = True
  ReadIniString JFD, "inputfile", BLANK, tmp:   Text1.Text = tmp
  ReadIniString JFD, "outputfile", BLANK, tmp:  Text2.Text = tmp
  ReadIniString JFD, "designation", BLANK, tmp: designation.Text = tmp
  ReadIniString JFD, "year", BLANK, tmp:        year.Text = tmp
  ReadIniString JFD, "anom", "2.0", tmp:     anom.Text = tmp
  ReadIniString JFD, "surface", "0.0", tmp:  surface.Text = tmp
  
  ReadIniString JFD, "station", BLANK, tmp:  station.Text = tmp
  ReadIniString JFD, "state", "WA", tmp:     state.Text = tmp
  ReadIniString JFD, "zone", "7", tmp:       zone.Text = tmp
  ReadIniString JFD, "elevation", "0", tmp:  elevation.Text = tmp
  ReadIniString JFD, "lat", "7", tmp:        lat.Text = tmp
  ReadIniString JFD, "lng", "44.00", tmp:    lng.Text = tmp
End Sub

Sub SetSettings()
Dim i As Long
  
  On Error Resume Next
 
  i = 1
  If Option1(0).value Then i = 0
  WriteIniString JFD, "NOAAtype", CStr(i)
  WriteIniString JFD, "inputfile", Text1.Text
  WriteIniString JFD, "outputfile", Text2.Text
  WriteIniString JFD, "designation", designation.Text
  WriteIniString JFD, "year", CStr(Val(year.Text))
  WriteIniString JFD, "anom", CStr(Val(anom.Text))
  WriteIniString JFD, "surface", CStr(Val(surface.Text))
    
  WriteIniString JFD, "station", station.Text
  WriteIniString JFD, "state", state.Text
  WriteIniString JFD, "zone", CStr(Val(zone.Text))
  WriteIniString JFD, "elevation", CStr(Val(elevation.Text))
  WriteIniString JFD, "lat", CStr(Val(lat.Text))
  WriteIniString JFD, "lng", CStr(Val(lng.Text))
  For i = 0 To 11
    WriteIniString JFD, "Month" & CStr(i), CStr(Mon(i).value)
  Next
End Sub

Public Function GetShortName(sLongFileName As String) As String
  Dim lRetVal As Long
  Dim sShortPathName As String
  Dim iLen As Integer
  
  'Set up buffer area for API function call return
  sShortPathName = Space(255)
  iLen = Len(sShortPathName)

  'Call the function
  lRetVal = GetShortPathName(sLongFileName, sShortPathName, iLen)
  'Strip away unwanted characters.
  GetShortName = Left(sShortPathName, lRetVal)
End Function

Private Sub Command3_Click()
Dim fnum As Long
Dim runto As String
Dim style As String
Dim cmdline As String
Dim metname As String
Dim jfdname As String
Dim strname As String

'  If Option1(0).value Then
'    station.Text = ""
'    state.Text = ""
'    lat.Text = ""
'    lng.Text = ""
'    zone.Text = ""
'    elevation.Text = ""
'  End If
  
  SetSettings
  
  cmdline = ""
  For i = 0 To 11
    cmdline = cmdline & CStr(Mon(i).value) & ","
  Next
  
  cmdline = designation.Text & "," & _
            CStr(Val(year.Text)) & ",'" & _
            Trim(station.Text) & "','" & _
            Trim(state.Text) & "'," & _
            CStr(Val(lat.Text)) & "," & _
            CStr(Val(lng.Text)) & "," & _
            zone.Text & _
            ",3,1," & _
            cmdline & _
            CStr(Val(elevation.Text)) & "," & _
            CStr(Val(anom.Text)) & "," & _
            CStr(Val(surface.Text))

  fnum = FreeFile
  Open App.Path & "\" & JFDGEN_IN For Output As #fnum
  Print #fnum, cmdline
  Close #fnum
  
  metname = GetShortName(Text1.Text)
  jfdname = ReplaceExt(metname, JFD)
  strname = ReplaceExt(metname, STR)
  ChDir App.Path
  cmdline = """" & App.Path & "\starr.exe"" " & JFDGEN_IN & " " & metname & " " & JFDGEN_OUT & " " & jfdname & " " & strname
  On Error Resume Next
  Kill JFDGEN_OUT
  Kill JFDGEN_ERR
  ExecCmd cmdline
  
  Dim fle As csv
  Frame2.ZOrder 0
  Frame2.Enabled = True
  If Len(Dir(JFDGEN_ERR)) Then
    fnum = FreeFile
    open_csv fle, JFDGEN_ERR, F_READ
    While Not EOCF(fle)
      buffer = buffer & vbCrLf & fle.getbuff
      get_line fle
    Wend
    buffer = buffer & vbCrLf & fle.getbuff
    close_csv fle
    Text3.Text = jfdname & " file generation failed!" _
                 & vbCrLf & "The following describes the transformation error." & vbCrLf & buffer
  ElseIf Len(Dir(JFDGEN_OUT)) Then
    fnum = FreeFile
    open_csv fle, JFDGEN_OUT, F_READ
    While Not EOCF(fle)
      buffer = buffer & vbCrLf & fle.getbuff
      get_line fle
    Wend
    buffer = buffer & vbCrLf & fle.getbuff
    close_csv fle
    Text3.Text = jfdname & " file gereration succesfull!" _
                 & vbCrLf & "The following describes the transformation." & vbCrLf & buffer
  End If
  
End Sub

Private Sub form_KeyDown(KeyCode As Integer, Shift As Integer)
  Select Case KeyCode
  Case vbKeyF1:
    KeyCode = 0
    GetHelp
  Case vbKeyReturn:
    KeyCode = 0
    SendKeys "{TAB}"
  End Select
End Sub

Private Sub Form_Load()
  BLANK = ""
  GetSettings
  Set frm = Me
  SetHelpFile App.Path + "\JFDGenerator.htm"
  If Dir(App.Path + "\starr.exe") = "" Then
    MsgBox "Unable to locate Starr.exe!" & vbCrLf & _
    "JFDGenerator.exe and Starr.exe must be in the same directory!", vbCritical, "JFD Generator Error"
    End
  End If
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

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
  SetSettings
End Sub

Private Sub howto_Click()
  GetHelp
End Sub

Private Sub Option1_Click(Index As Integer)
'If sam files are read, then we do not need the following information
'Station Name, Two letter state abbreviation, Time Zone,
'Latitude, Longitude, and Elevation.
Text1.Text = ""
If Option1(1).value Then
  Frame4.Visible = True
  Frame4.Enabled = True
  Text1.Move 2880, 840
  Command1.Move 7440, 840
Else
  Frame4.Visible = False
  Frame4.Enabled = False
  Text1.Move 2880, 360
  Command1.Move 7440, 360
End If
End Sub

Private Sub Text1_Change()
  Text2.Text = ReplaceExt(Text1.Text, JFD)
End Sub

