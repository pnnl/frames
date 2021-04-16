VERSION 5.00
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "TABCTL32.OCX"
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Begin VB.Form Source 
   BorderStyle     =   1  'Fixed Single
   ClientHeight    =   6855
   ClientLeft      =   9120
   ClientTop       =   630
   ClientWidth     =   9870
   Icon            =   "BiotaSource.frx":0000
   KeyPreview      =   -1  'True
   MaxButton       =   0   'False
   MinButton       =   0   'False
   NegotiateMenus  =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   6855
   ScaleWidth      =   9870
   StartUpPosition =   2  'CenterScreen
   Begin TabDlg.SSTab SSTab1 
      Height          =   6375
      Left            =   0
      TabIndex        =   1
      Top             =   0
      Width           =   9855
      _ExtentX        =   17383
      _ExtentY        =   11245
      _Version        =   393216
      Style           =   1
      Tabs            =   1
      TabsPerRow      =   1
      TabHeight       =   706
      TabCaption(0)   =   "Inputs"
      TabPicture(0)   =   "BiotaSource.frx":030A
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "Frame0"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).ControlCount=   1
      Begin VB.Frame Frame0 
         Height          =   5535
         Left            =   240
         TabIndex        =   2
         Top             =   600
         Width           =   9375
         Begin VB.ComboBox Combo 
            Height          =   315
            ItemData        =   "BiotaSource.frx":0326
            Left            =   2280
            List            =   "BiotaSource.frx":0336
            Style           =   2  'Dropdown List
            TabIndex        =   16
            Tag             =   "IremSv"
            Top             =   2400
            Width           =   1335
         End
         Begin VB.CheckBox IPathNuclide 
            Caption         =   "Provide results by pathway and by nuclide"
            Height          =   255
            Left            =   360
            TabIndex        =   15
            Tag             =   "IPthNuc"
            Top             =   1920
            Width           =   4815
         End
         Begin VB.OptionButton RType 
            Caption         =   "Include Facility Information"
            Height          =   195
            Index           =   1
            Left            =   360
            TabIndex        =   9
            Tag             =   "CETOXT"
            Top             =   960
            Value           =   -1  'True
            Width           =   8700
         End
         Begin VB.OptionButton RType 
            Caption         =   "Include Facility Information and Atmospheric Dispersion Outputs"
            Height          =   195
            Index           =   2
            Left            =   360
            TabIndex        =   8
            Tag             =   "CETOXT"
            Top             =   1440
            Width           =   8700
         End
         Begin VB.OptionButton RType 
            Caption         =   "Provide only minimal output"
            Height          =   195
            Index           =   0
            Left            =   360
            TabIndex        =   7
            Tag             =   "CETOXT"
            Top             =   480
            Width           =   8700
         End
         Begin VB.TextBox txt 
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   4
            Left            =   3120
            TabIndex        =   6
            Tag             =   "FacNam"
            Text            =   "Facility Name"
            Top             =   3360
            Width           =   5500
         End
         Begin VB.TextBox txt 
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   5
            Left            =   3120
            TabIndex        =   5
            Tag             =   "FacStrt"
            Text            =   "Street Address"
            Top             =   3840
            Width           =   5500
         End
         Begin VB.TextBox txt 
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   6
            Left            =   3120
            TabIndex        =   4
            Tag             =   "FacCity"
            Text            =   "City, State, ZIP"
            Top             =   4320
            Width           =   5500
         End
         Begin VB.TextBox txt 
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   7
            Left            =   3120
            TabIndex        =   3
            Tag             =   "UsrNam"
            Text            =   "User Name"
            Top             =   4800
            Width           =   5500
         End
         Begin VB.Label Label1 
            Caption         =   "Select reporting units"
            Height          =   255
            Index           =   6
            Left            =   360
            TabIndex        =   14
            Tag             =   "directions"
            Top             =   2400
            Width           =   1890
         End
         Begin VB.Label lbl 
            Caption         =   "Input Facility Name"
            Height          =   300
            Index           =   4
            Left            =   360
            TabIndex        =   13
            Top             =   3360
            Width           =   2700
         End
         Begin VB.Label lbl 
            Caption         =   "Input Facility Mailing Address"
            Height          =   300
            Index           =   5
            Left            =   360
            TabIndex        =   12
            Top             =   3840
            Width           =   2700
         End
         Begin VB.Label lbl 
            Caption         =   "Input Facility City, State, ZIP Code"
            Height          =   300
            Index           =   6
            Left            =   360
            TabIndex        =   11
            Top             =   4320
            Width           =   2700
         End
         Begin VB.Label lbl 
            Caption         =   "Input User Name"
            Height          =   300
            Index           =   7
            Left            =   360
            TabIndex        =   10
            Top             =   4800
            Width           =   2700
         End
         Begin VB.Line Line 
            Index           =   2
            X1              =   120
            X2              =   9000
            Y1              =   3000
            Y2              =   3000
         End
      End
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   9480
      Top             =   480
      _ExtentX        =   688
      _ExtentY        =   688
      _Version        =   393216
   End
   Begin VB.TextBox mes 
      BackColor       =   &H00C0FFFF&
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
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
      TabIndex        =   0
      TabStop         =   0   'False
      Top             =   6480
      Width           =   9840
   End
   Begin VB.Timer Timer1 
      Interval        =   100
      Left            =   9480
      Top             =   120
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

Dim temp As parmrec

Private Sub about_Click()
  frmAbout.Show 1, Source
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

Private Sub loadprm()
  Dim noair As Boolean
  Dim m As Long
  Dim fle As parmfile
  
  noair = False
  If open_parm(fle, FUIName, 2) Then
    Do Until EOCF(fle.file)
      If read_parmrec(fle, temp) Then
        Loading.Gauge1.Max = Val(temp.idx1)
        Loading.Gauge1.Value = 0
        Select Case temp.pName
          Case "fui"
            For m = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                If temp.idx1 = siteIdx And temp.idx2 = modIdx Then
                  If temp.pName = "nesdespath" Then
                    DesName = temp.pval
                  End If
                  If temp.pName = "nesType" And InStr(temp.pval, "air") > 0 Then
                    noair = True
                  End If
                End If
              End If
            Next
          
          Case modName
            For m = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                Loading.update
                Select Case temp.pName
                  Case "CVTFormat":    CVTFormat = "General Number" 'CVTFormat = temp.pval
                  Case "Method":       RType(CInt(temp.pval)).Value = True
                  Case "FacNam":       txt(4).Text = temp.pval
                  Case "FacStrt":      txt(5).Text = temp.pval
                  Case "FacCity":      txt(6).Text = temp.pval
                  Case "UsrNam":       txt(7).Text = temp.pval
                  Case "IremSv":       Combo.ListIndex = CInt(temp.pval)
                  Case "IPthNuc":      IPathNuclide.Value = CInt(temp.pval)
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
    If Not noair And RType(2).Value Then RType(1).Value = True
    RType(2).Visible = noair
    SetFormat Me
  Else
    PutError "Can't find or open file " & FUIName
    EndModule
  End If
End Sub

Private Sub Form_load()
Dim i As Long
  StartModule Me, App.Title, 5
'  SetHelpFile App.Path + "\xq.htm"
  
  Combo.ListIndex = 2
  
  RType(0).Value = True
  Loading.Show
  loadprm
  Unload Loading
End Sub

Private Sub RType_Click(Index As Integer)
  Select Case Index
  Case 0
    SetIdEnabled False
  Case 1
    SetIdEnabled True
  Case 2
    SetIdEnabled True
  End Select
End Sub

Sub SetIdEnabled(enable As Boolean)
Dim i As Long
  For i = 4 To 7
    lbl(i).Enabled = enable
    txt(i).Enabled = enable
  Next
End Sub

Private Sub save_Click()
  Dim i As Long
  Dim method As Long
  Dim fName As String
  Dim parm As parmrec
  Dim fle As parmfile
  Dim UserFormat As String
  UserFormat = CVTFormat
  
  fName = RunName + ".GID"
  If open_parm(fle, fName, 1) Then
    set_parm parm, "CVTFormat", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CVTFormat
    write_parmrec fle, parm
    CVTFormat = "General Number"
      
    If RType(0).Value Then method = 0
    If RType(1).Value Then method = 1
    If RType(2).Value Then method = 2
    
    set_parm parm, "Method", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CStr(method)
    write_parmrec fle, parm
    set_parm parm, IPathNuclide.Tag, 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", IPathNuclide.Value
    write_parmrec fle, parm
    set_parm parm, Combo.Tag, 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", Combo.ListIndex
    write_parmrec fle, parm
        
    If RType(1).Value = True Or RType(2).Value = True Then
      'save facitity info
      For i = 4 To 7
        set_parm parm, txt(i).Tag, 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", txt(i).Text
        write_parmrec fle, parm
      Next
    End If
    
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
