VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Begin VB.Form Source 
   BorderStyle     =   1  'Fixed Single
   ClientHeight    =   2436
   ClientLeft      =   9120
   ClientTop       =   636
   ClientWidth     =   5988
   KeyPreview      =   -1  'True
   MaxButton       =   0   'False
   MinButton       =   0   'False
   NegotiateMenus  =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   203
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   499
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton Command2 
      Caption         =   "View"
      Height          =   252
      Left            =   2040
      TabIndex        =   18
      Top             =   360
      Visible         =   0   'False
      Width           =   852
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Browse"
      Height          =   252
      Left            =   2040
      TabIndex        =   17
      Top             =   1440
      Visible         =   0   'False
      Width           =   852
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   0
      Top             =   1680
      _ExtentX        =   677
      _ExtentY        =   677
      _Version        =   393216
   End
   Begin VB.TextBox txt 
      Height          =   288
      Index           =   4
      Left            =   3000
      TabIndex        =   15
      Tag             =   "reachback"
      Top             =   1440
      Visible         =   0   'False
      Width           =   2892
   End
   Begin VB.ComboBox Combo1 
      Height          =   288
      Index           =   0
      ItemData        =   "Source.frx":0000
      Left            =   3000
      List            =   "Source.frx":0016
      Style           =   2  'Dropdown List
      TabIndex        =   6
      Tag             =   "location"
      Top             =   360
      Visible         =   0   'False
      Width           =   1812
   End
   Begin VB.ComboBox Combo1 
      Height          =   288
      Index           =   1
      ItemData        =   "Source.frx":0038
      Left            =   3000
      List            =   "Source.frx":0042
      Style           =   2  'Dropdown List
      TabIndex        =   5
      Tag             =   "direction"
      Top             =   360
      Visible         =   0   'False
      Width           =   1812
   End
   Begin VB.TextBox mes 
      BackColor       =   &H00C0FFFF&
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.6
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   360
      Left            =   0
      Locked          =   -1  'True
      TabIndex        =   4
      TabStop         =   0   'False
      Top             =   2040
      Width           =   6000
   End
   Begin VB.TextBox txt 
      Height          =   288
      Index           =   2
      Left            =   3000
      TabIndex        =   3
      Tag             =   "endtime"
      Top             =   720
      Visible         =   0   'False
      Width           =   972
   End
   Begin VB.ComboBox unit 
      Height          =   288
      Index           =   2
      ItemData        =   "Source.frx":0052
      Left            =   3960
      List            =   "Source.frx":0054
      Style           =   2  'Dropdown List
      TabIndex        =   2
      Tag             =   "yr"
      Top             =   720
      Visible         =   0   'False
      Width           =   852
   End
   Begin VB.ComboBox unit 
      Height          =   288
      Index           =   3
      ItemData        =   "Source.frx":0056
      Left            =   3960
      List            =   "Source.frx":0058
      Style           =   2  'Dropdown List
      TabIndex        =   1
      Tag             =   "mg/L"
      Top             =   1080
      Visible         =   0   'False
      Width           =   852
   End
   Begin VB.TextBox txt 
      Height          =   288
      Index           =   3
      Left            =   3000
      TabIndex        =   0
      Tag             =   "threshold"
      Top             =   1080
      Visible         =   0   'False
      Width           =   972
   End
   Begin VB.Timer Timer1 
      Interval        =   100
      Left            =   0
      Top             =   1320
   End
   Begin VB.Label Label1 
      Caption         =   "Reachback Filename"
      Height          =   252
      Index           =   4
      Left            =   240
      TabIndex        =   16
      Top             =   1440
      Visible         =   0   'False
      Width           =   2052
   End
   Begin VB.Label Label1 
      Caption         =   "Release Location"
      Height          =   252
      Index           =   0
      Left            =   240
      TabIndex        =   14
      Top             =   360
      Visible         =   0   'False
      Width           =   2772
   End
   Begin VB.Label Label1 
      Caption         =   "Wind Direction"
      Height          =   252
      Index           =   1
      Left            =   240
      TabIndex        =   13
      Top             =   360
      Visible         =   0   'False
      Width           =   2772
   End
   Begin VB.Label ref 
      Caption         =   "Ref: 0"
      Height          =   252
      Index           =   0
      Left            =   5040
      TabIndex        =   12
      Tag             =   "0"
      Top             =   360
      Visible         =   0   'False
      Width           =   732
   End
   Begin VB.Label ref 
      Caption         =   "Ref: 0"
      Height          =   252
      Index           =   1
      Left            =   5040
      TabIndex        =   11
      Tag             =   "0"
      Top             =   360
      Visible         =   0   'False
      Width           =   732
   End
   Begin VB.Label Label1 
      Caption         =   "Ending Time"
      Height          =   252
      Index           =   2
      Left            =   240
      TabIndex        =   10
      Top             =   720
      Visible         =   0   'False
      Width           =   2772
   End
   Begin VB.Label ref 
      Caption         =   "Ref: 0"
      Height          =   252
      Index           =   2
      Left            =   5040
      TabIndex        =   9
      Tag             =   "0"
      Top             =   720
      Visible         =   0   'False
      Width           =   732
   End
   Begin VB.Label ref 
      Caption         =   "Ref: 0"
      Height          =   252
      Index           =   3
      Left            =   5040
      TabIndex        =   8
      Tag             =   "0"
      Top             =   1080
      Visible         =   0   'False
      Width           =   732
   End
   Begin VB.Label Label1 
      Caption         =   "Concentration Threshold"
      Height          =   252
      Index           =   3
      Left            =   240
      TabIndex        =   7
      Top             =   1080
      Visible         =   0   'False
      Width           =   2772
   End
   Begin VB.Menu file 
      Caption         =   "&File"
      WindowList      =   -1  'True
      Begin VB.Menu save 
         Caption         =   "&Save and Exit"
      End
      Begin VB.Menu leave 
         Caption         =   "E&xit"
      End
   End
   Begin VB.Menu noact 
      Caption         =   "&Reference"
      Begin VB.Menu addref 
         Caption         =   "&Add"
      End
      Begin VB.Menu selref 
         Caption         =   "Selec&t"
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
   Begin VB.Menu hlp 
      Caption         =   "&Help"
      Begin VB.Menu howto 
         Caption         =   "How to..."
         Enabled         =   0   'False
         Visible         =   0   'False
      End
      Begin VB.Menu about 
         Caption         =   "&About"
      End
   End
End
Attribute VB_Name = "Source"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text
Dim mode As String
Dim temp As parmrec

Private Const MAX_PATH As Long = 260
Private Const ERROR_FILE_NO_ASSOCIATION As Long = 31
Private Const ERROR_FILE_NOT_FOUND As Long = 2
Private Const ERROR_PATH_NOT_FOUND As Long = 3
Private Const ERROR_FILE_SUCCESS As Long = 32 'my constant
Private Const ERROR_BAD_FORMAT As Long = 11


Private Sub about_Click()
  frmAbout.Show 1, Source
End Sub

Private Sub Command1_Click()
  CommonDialog1.ShowOpen
  txt(4).Text = CommonDialog1.filename
End Sub

Private Sub Command2_Click()
Dim url As String
Dim success As Long
Dim pos As Long
Dim sResult As String
Dim msg As String

    sResult = Space$(1024)

    ' lpFile: name of the file of interest
    ' lpDirectory: location of lpFile
    ' sResult: path and name of executable associated with lpFile
    success = FindExecutable("source.xls", "", sResult)
    Select Case success
      Case ERROR_FILE_NO_ASSOCIATION: msg = "no association"
      Case ERROR_FILE_NOT_FOUND: msg = "file not found"
      Case ERROR_PATH_NOT_FOUND: msg = "path not found"
      Case ERROR_BAD_FORMAT:     msg = "bad format"
      Case Is >= ERROR_FILE_SUCCESS:
        pos = InStr(sResult, Chr$(0))
        If pos Then
          url = Left$(sResult, pos - 1)
          'url = url + " " + ConvertURL(HelpFileName + "#" + UCase(HelpAnchor))
          url = url + " " + "source.xls"
          Shell url, vbNormalFocus
          Exit Sub
        End If
        Exit Sub
    End Select
End Sub


Private Sub Form_Unload(Cancel As Integer)
  Dim answer As Long
  If Not called Then
    answer = MsgBox("Do you want to save changes?", 51, App.Title)
    If answer = 6 Then save_Click
    If answer = 7 Then EndModule
    If answer = 2 Then Cancel = 1
  End If
End Sub

Private Sub howto_Click()
  GetHelp
End Sub

Private Sub leave_Click()
  Form_Unload 0
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

Private Sub selref_Click()
  RefMode = 0
  GetRef ref(RefItem)
End Sub

Private Sub addref_Click()
  RefMode = 1
  GetRef ref(RefItem)
End Sub

Private Sub loadprm()
  Dim m As Long
  Dim fle As parmfile
  
  If open_parm(fle, FUIName, 2) Then
    Do Until EOCF(fle.file)
      If read_parmrec(fle, temp) Then
        Loading.Gauge1.Max = Val(temp.idx1)
        Loading.Gauge1.Value = 0
        Select Case temp.pname
          Case "fui"
            For m = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                If temp.idx1 = siteIdx Then
                  If temp.pname = "rivdespath" Then DesName = temp.pval
                End If
              End If
            Next
          
          Case ModName
            For m = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                Loading.update
                Select Case temp.pname
                  Case "CVTFormat":    CVTFormat = "General Number" 'CVTFormat = temp.pval
                  Case "Location":
                    Combo1(0).Text = temp.pval
                    ref(0).Tag = temp.ref
                    ref(0).Caption = "Ref:" & Str(temp.ref)
                  Case "Direction":
                    Combo1(1).Text = temp.pval
                    ref(1).Tag = temp.ref
                    ref(1).Caption = "Ref:" & Str(temp.ref)
                  Case "EndTime":
                    txt(2).Text = convert(temp.cunit, temp.uunit, Val(temp.pval))
                    unit(2).Text = temp.uunit
                    ref(2).Tag = temp.ref
                    ref(2).Caption = "Ref:" & Str(temp.ref)
                  Case "Threshold":
                    txt(3).Text = convert(temp.cunit, temp.uunit, Val(temp.pval))
                    unit(3).Text = temp.uunit
                    ref(3).Tag = temp.ref
                    ref(3).Caption = "Ref:" & Str(temp.ref)
                  Case "Reachback":
                    txt(4).Text = temp.pval
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
    SetFormat Me
  Else
    PutError "Can't find or open file " & FUIName
    EndModule
  End If
End Sub

Private Sub Form_load()
 Dim i As Integer
  StartModule Source, App.Title, 6
'  SetHelpFile App.Path + "\picksrc.htm"
  SetRefFile ReplaceExt(FUIName, "ref")
  
  mode = argv(0)
  If mode = "/L" Then
    Me.Caption = "Pick Location" + " - " + ModName
    Label1(0).Visible = True
    Combo1(0).Visible = True
    ref(0).Visible = True
    Combo1(0).ListIndex = 0
    Command2.Visible = True
  End If
  If mode = "/D" Then
    Me.Caption = "Pick Direction" + " - " + ModName
    Label1(1).Visible = True
    Combo1(1).Visible = True
    ref(1).Visible = True
    Combo1(1).ListIndex = 0
    
    For i = 2 To 3
      Label1(i).Visible = True
      txt(i).Visible = True
      unit(i).Visible = True
      ref(i).Visible = True
      get_conversion_items unit(i).Tag, unit(i)
    Next
  End If
  ' both have reachback
  Label1(4).Visible = True
  txt(4).Visible = True
  Command1.Visible = True
    
  
  Loading.Show
  loadprm
  Unload Loading
End Sub

Private Sub save_Click()
  Dim i As Integer
  Dim fname As String
  Dim parm As parmrec
  Dim fle As parmfile
  Dim UserFormat As String
  UserFormat = CVTFormat
  
  fname = RunName + ".GID"
  If open_parm(fle, fname, 1) Then
    set_parm parm, "CVTFormat", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CVTFormat
    write_parmrec fle, parm
    CVTFormat = "General Number"
    If mode = "/L" Then
      set_parm parm, Combo1(0).Tag, 0, 0, 0, 0, 0, 0, ref(0).Tag, "N/A", "N/A", Combo1(0).Text
      write_parmrec fle, parm
    End If
    If mode = "/D" Then
      set_parm parm, Combo1(1).Tag, 0, 0, 0, 0, 0, 0, ref(1).Tag, "N/A", "N/A", Combo1(1).Text
      write_parmrec fle, parm
      For i = 2 To 3
        set_parm parm, txt(i).Tag, 0, 0, 0, 0, 0, 0, Val(ref(i).Tag), unit(i).Text, unit(i).Tag, convert(unit(i).Text, unit(i).Tag, Val(txt(i).Text))
        write_parmrec fle, parm
      Next
    End If
    set_parm parm, txt(4).Tag, 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", txt(4).Text
    write_parmrec fle, parm
    close_parm fle
    CVTFormat = UserFormat
    SetFormat Me
  Else
    PutError "Unable to create transaction file" & RunName & ".GID"
  End If
  EndModule
End Sub

Private Sub Timer1_Timer()
  mes = Mid(mes, 2) & Mid(mes, 1, 1)
End Sub

Private Sub Combo1_GotFocus(Index As Integer)
Dim m As String
  m = "Select a " & Combo1(Index).Tag
  mes = Space(109 - Len(m)) & m
  RefItem = Index
  noact.Enabled = True
  HelpAnchor = Combo1(Index).Tag
End Sub

Private Sub form_KeyPress(KeyAscii As Integer)
  If KeyAscii = 13 Then KeyAscii = 0
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

