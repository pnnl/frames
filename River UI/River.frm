VERSION 5.00
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "TabCtl32.ocx"
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "threed32.ocx"
Begin VB.Form River 
   BorderStyle     =   1  'Fixed Single
   ClientHeight    =   5052
   ClientLeft      =   1020
   ClientTop       =   3816
   ClientWidth     =   7620
   Icon            =   "River.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   NegotiateMenus  =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   421
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   635
   StartUpPosition =   2  'CenterScreen
   Begin VB.Timer Timer1 
      Interval        =   100
      Left            =   7150
      Top             =   0
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
      TabIndex        =   53
      TabStop         =   0   'False
      Top             =   4680
      Width           =   7680
   End
   Begin TabDlg.SSTab SSTab1 
      Height          =   4692
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   8196
      _ExtentX        =   14436
      _ExtentY        =   8276
      _Version        =   393216
      Style           =   1
      Tabs            =   2
      TabHeight       =   524
      TabCaption(0)   =   "Dimensions"
      TabPicture(0)   =   "River.frx":030A
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "SSFrame1(0)"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).ControlCount=   1
      TabCaption(1)   =   "Constituent Properties"
      TabPicture(1)   =   "River.frx":0326
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "SSFrame1(1)"
      Tab(1).ControlCount=   1
      Begin Threed.SSFrame SSFrame1 
         Height          =   3960
         Index           =   0
         Left            =   240
         TabIndex        =   54
         Top             =   480
         Width           =   7200
         _Version        =   65536
         _ExtentX        =   12700
         _ExtentY        =   6985
         _StockProps     =   14
         ForeColor       =   16776960
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ShadowStyle     =   1
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   8
            Left            =   5280
            Style           =   2  'Dropdown List
            TabIndex        =   29
            Tag             =   "m"
            Top             =   3132
            Width           =   996
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   9
            Left            =   5280
            Style           =   2  'Dropdown List
            TabIndex        =   33
            Tag             =   "m"
            Top             =   3492
            Width           =   996
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   7
            Left            =   5280
            Style           =   2  'Dropdown List
            TabIndex        =   25
            Tag             =   "m"
            Top             =   2760
            Width           =   996
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   7
            Left            =   4320
            TabIndex        =   24
            Tag             =   "wwy"
            Top             =   2760
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   8
            Left            =   4320
            TabIndex        =   28
            Tag             =   "wwx"
            Top             =   3132
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   9
            Left            =   4320
            TabIndex        =   32
            Tag             =   "wwz"
            Top             =   3492
            Width           =   1000
         End
         Begin VB.ComboBox Combo2 
            Height          =   264
            Left            =   3024
            Style           =   2  'Dropdown List
            TabIndex        =   14
            Tag             =   "fslocate"
            Top             =   1548
            Width           =   3252
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   2
            Left            =   4320
            TabIndex        =   10
            Tag             =   "wwwidth"
            Top             =   1092
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   1
            Left            =   4320
            TabIndex        =   6
            Tag             =   "wwdepth"
            Top             =   732
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Enabled         =   0   'False
            Height          =   312
            Index           =   4
            Left            =   4320
            TabIndex        =   20
            Tag             =   "wwdischg"
            Top             =   2280
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   3
            Left            =   4320
            TabIndex        =   16
            Tag             =   "wwdist"
            Top             =   1920
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   0
            Left            =   4320
            TabIndex        =   2
            Tag             =   "wwveloc"
            Top             =   360
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   0
            Left            =   5280
            Style           =   2  'Dropdown List
            TabIndex        =   3
            Tag             =   "cm/day"
            Top             =   360
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   2
            Left            =   5280
            Style           =   2  'Dropdown List
            TabIndex        =   11
            Tag             =   "cm"
            Top             =   1092
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   1
            Left            =   5280
            Style           =   2  'Dropdown List
            TabIndex        =   7
            Tag             =   "cm"
            Top             =   732
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Enabled         =   0   'False
            Height          =   264
            Index           =   4
            Left            =   5280
            Style           =   2  'Dropdown List
            TabIndex        =   21
            Tag             =   "cm^3/day"
            Top             =   2280
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   264
            Index           =   3
            Left            =   5280
            Style           =   2  'Dropdown List
            TabIndex        =   17
            Tag             =   "cm"
            Top             =   1920
            Width           =   1000
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   7
            Left            =   6360
            TabIndex        =   26
            Top             =   2796
            Width           =   780
         End
         Begin VB.Label lbl 
            Caption         =   "Georeferenced  notrhing coordinate - WW-Y"
            Height          =   252
            Index           =   12
            Left            =   120
            TabIndex        =   23
            Top             =   2796
            Width           =   4104
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   8
            Left            =   6360
            TabIndex        =   30
            Tag             =   "0"
            Top             =   3168
            Width           =   780
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   9
            Left            =   6360
            TabIndex        =   34
            Tag             =   "0"
            Top             =   3528
            Width           =   780
         End
         Begin VB.Label lbl 
            Caption         =   "Elevation - WW-Z"
            Height          =   252
            Index           =   11
            Left            =   120
            TabIndex        =   31
            Top             =   3492
            Width           =   4104
         End
         Begin VB.Label lbl 
            Caption         =   "Georeferenced easting coordinate - WW-X"
            Height          =   252
            Index           =   10
            Left            =   120
            TabIndex        =   27
            Top             =   3132
            Width           =   4104
         End
         Begin VB.Label lbl 
            Caption         =   "Depth at constituent entry point - WW-DEPTH"
            Height          =   252
            Index           =   1
            Left            =   120
            TabIndex        =   5
            Top             =   732
            Width           =   4104
         End
         Begin VB.Label lbl 
            Caption         =   "Distance from source to location - WW-DIST"
            Height          =   252
            Index           =   3
            Left            =   120
            TabIndex        =   15
            Top             =   1920
            Width           =   4100
         End
         Begin VB.Label lbl 
            Caption         =   "Width at constituent entry point - WW-WIDTH"
            Height          =   252
            Index           =   2
            Left            =   120
            TabIndex        =   9
            Top             =   1092
            Width           =   4104
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   2
            Left            =   6360
            TabIndex        =   12
            Tag             =   "0"
            Top             =   1125
            Width           =   775
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   1
            Left            =   6360
            TabIndex        =   8
            Tag             =   "0"
            Top             =   765
            Width           =   775
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Enabled         =   0   'False
            Height          =   255
            Index           =   4
            Left            =   6360
            TabIndex        =   22
            Tag             =   "0"
            Top             =   2310
            Width           =   775
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   3
            Left            =   6360
            TabIndex        =   18
            Tag             =   "0"
            Top             =   1950
            Width           =   775
         End
         Begin VB.Label lbl 
            Caption         =   "Average annual discharge at location - WW-DISCHG"
            Enabled         =   0   'False
            Height          =   252
            Index           =   4
            Left            =   120
            TabIndex        =   19
            Top             =   2280
            Width           =   4100
         End
         Begin VB.Label lbl 
            Caption         =   "Usage Location"
            Height          =   252
            Index           =   5
            Left            =   120
            TabIndex        =   13
            Top             =   1524
            Width           =   2784
         End
         Begin VB.Label lbl 
            Caption         =   "Flow velocity at constituent entry point - WW-VELOC"
            Height          =   252
            Index           =   0
            Left            =   120
            TabIndex        =   1
            Top             =   396
            Width           =   4104
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   0
            Left            =   6360
            TabIndex        =   4
            Top             =   390
            Width           =   775
         End
      End
      Begin Threed.SSFrame SSFrame1 
         Height          =   3360
         Index           =   1
         Left            =   -74760
         TabIndex        =   55
         Top             =   480
         Width           =   7200
         _Version        =   65536
         _ExtentX        =   12700
         _ExtentY        =   5927
         _StockProps     =   14
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ShadowStyle     =   1
         Enabled         =   0   'False
         Begin VB.CommandButton SSCommand1 
            Caption         =   "<"
            Height          =   230
            Left            =   2885
            TabIndex        =   45
            Top             =   2250
            Width           =   250
         End
         Begin VB.CommandButton SSCommand4 
            Caption         =   "<"
            Height          =   230
            Left            =   2885
            TabIndex        =   36
            Top             =   510
            Width           =   250
         End
         Begin VB.CommandButton SSCommand5 
            Caption         =   ">"
            Height          =   230
            Left            =   3130
            TabIndex        =   37
            Top             =   510
            Width           =   230
         End
         Begin VB.CommandButton SSCommand2 
            Caption         =   ">"
            Height          =   230
            Left            =   3130
            TabIndex        =   46
            Top             =   2250
            Width           =   230
         End
         Begin VB.CommandButton Command4 
            Caption         =   "<"
            Height          =   230
            Left            =   6120
            TabIndex        =   48
            Top             =   2250
            Width           =   250
         End
         Begin VB.CommandButton Command3 
            Caption         =   ">"
            Height          =   230
            Left            =   6370
            TabIndex        =   49
            Top             =   2250
            Width           =   230
         End
         Begin VB.CommandButton Command2 
            Caption         =   "<"
            Height          =   230
            Left            =   6120
            TabIndex        =   39
            Top             =   510
            Width           =   250
         End
         Begin VB.CommandButton Command1 
            Caption         =   ">"
            Height          =   230
            Left            =   6370
            TabIndex        =   40
            Top             =   510
            Width           =   230
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   5
            Left            =   3960
            TabIndex        =   41
            Tag             =   "wwsol"
            Top             =   960
            Width           =   1000
         End
         Begin VB.ComboBox cboParent 
            Height          =   288
            Left            =   210
            Style           =   2  'Dropdown List
            TabIndex        =   35
            Tag             =   "FSCNAME"
            Top             =   480
            Width           =   3375
         End
         Begin VB.ComboBox cboProgeny 
            Height          =   288
            Left            =   210
            Style           =   2  'Dropdown List
            TabIndex        =   44
            Tag             =   "FSCNAME"
            Top             =   2222
            Width           =   3375
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   6
            Left            =   3960
            TabIndex        =   50
            Tag             =   "wwsol"
            Top             =   2640
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   264
            Index           =   6
            Left            =   4917
            Style           =   2  'Dropdown List
            TabIndex        =   51
            Tag             =   "mg/l"
            Top             =   2640
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   264
            Index           =   5
            Left            =   4917
            Style           =   2  'Dropdown List
            TabIndex        =   42
            Tag             =   "mg/l"
            Top             =   960
            Width           =   1000
         End
         Begin VB.ComboBox ConParms 
            Height          =   288
            ItemData        =   "River.frx":0342
            Left            =   3930
            List            =   "River.frx":034C
            Style           =   2  'Dropdown List
            TabIndex        =   38
            Top             =   480
            Width           =   2892
         End
         Begin VB.ComboBox ProgParms 
            Height          =   288
            ItemData        =   "River.frx":0395
            Left            =   3930
            List            =   "River.frx":039F
            Style           =   2  'Dropdown List
            TabIndex        =   47
            Top             =   2222
            Width           =   2892
         End
         Begin VB.Label lbl 
            Caption         =   "Parameters"
            Height          =   253
            Index           =   7
            Left            =   3839
            TabIndex        =   59
            Top             =   240
            Width           =   3003
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   5
            Left            =   6000
            TabIndex        =   43
            Tag             =   "0"
            Top             =   1050
            Width           =   1005
         End
         Begin VB.Label lbl 
            Caption         =   "Constituent - FS-CNAME"
            Height          =   253
            Index           =   6
            Left            =   121
            TabIndex        =   58
            Top             =   240
            Width           =   2112
         End
         Begin VB.Label lbl 
            Caption         =   "Progeny - FS-CNAME"
            Height          =   253
            Index           =   8
            Left            =   121
            TabIndex        =   57
            Top             =   1980
            Width           =   1936
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   6
            Left            =   6000
            TabIndex        =   52
            Tag             =   "0"
            Top             =   2700
            Width           =   1005
         End
         Begin VB.Label lbl 
            Caption         =   "Parameters"
            Height          =   253
            Index           =   9
            Left            =   3839
            TabIndex        =   56
            Top             =   1980
            Width           =   3003
         End
      End
   End
   Begin VB.Menu file 
      Caption         =   "&File"
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
         Caption         =   "Se&lect"
      End
   End
   Begin VB.Menu option 
      Caption         =   "&Options"
      Begin VB.Menu advan 
         Caption         =   "A&dvanced"
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
      End
      Begin VB.Menu about 
         Caption         =   "&About"
      End
   End
End
Attribute VB_Name = "River"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Const RAD_SOLUBILITY = 0
Const SOLUBILITY = 1
Const HALF_LIFE = 2

Dim temp As parmrec
Dim loadng As Boolean
Dim oldLocation As String
Dim oldProgeny As String
Dim oldParent As String
Dim uStr(2) As String
Dim hStr(2) As String

'Added flags to control the behaviour of the added parms combo box
Dim prevSelectedIdx As Long
Dim prevSelectedProgIdx As Long

' All these just ro resolve indexs from fui to mui
Dim f_numcon As Long
Dim m_numloc As Long
Dim locale() As locparm

Private Sub about_Click()
  frmAbout.Show 1, River
End Sub

Private Sub advan_Click()
  TimePts.Show 1, River
End Sub

Private Sub Form_Unload(Cancel As Integer)
  Dim answer As Long
  If Not called Then
    answer = MsgBox("Do you want to save changes?", 51, App.title)
    If answer = 6 Then save_Click
    If answer = 7 Then
      Unload TimePts
      EndModule
    End If
    If answer = 2 Then Cancel = 1
  End If
End Sub

Function getListParmName(ctlList As Control) As String
  ' fail safe return value
  getListParmName = ""
  
  Select Case ctlList.Name
    Case "ConParms", "ProgParms"
      Select Case ctlList.ListIndex
        Case WATER_SOLUBILITY_IDX
          If con(model(cboParent.ItemData(cboParent.ListIndex)).idx).rad Then
            getListParmName = "WWSOL-RAD"
          Else
            getListParmName = "WWSOL"
          End If
        Case HALF_LIFE_IDX
          getListParmName = "WWSHALF"
      End Select
  End Select
End Function

Function getUnitType(ParmName As String) As String
  ' fail safe return value
  getUnitType = ""
  
  Select Case ParmName
    Case "WWSOL"
      getUnitType = "Mass/LiquidVolume"
    Case "WWSOL-RAD"
      getUnitType = "Activity/LiquidVolume"
    Case "WWSHALF"
      getUnitType = "Time"
  End Select
  
End Function

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

Private Sub locfillet(idx As Long)
  If temp.idx3 > m_numloc Then
    m_numloc = temp.idx1
    ReDim Preserve locale(m_numloc) As locparm
  End If
  locale(temp.idx1).parms(idx).value = convert(temp.cunit, temp.uunit, Val(temp.pval))
  locale(temp.idx1).parms(idx).uunt = temp.uunit
  locale(temp.idx1).parms(idx).punt = temp.cunit
  locale(temp.idx1).parms(idx).ref = temp.ref
End Sub

Private Sub fillet(idx As Long)
  ref(idx).Tag = temp.ref
  ref(idx).Caption = "Ref:" & Str(temp.ref)
  On Error Resume Next
  If temp.cunit <> "N/A" Then set_unit unit(idx), temp.uunit
  txt(idx).Text = convert(temp.cunit, temp.uunit, Val(temp.pval))
End Sub

Private Sub SizeLocal()
  If temp.idx3 > numloc Then
    numloc = temp.idx3
    ReDim Preserve loc(numloc) As snk
  End If
End Sub

Private Sub fillChem(idx As Long)
  If temp.idx2 > f_numcon Then
    f_numcon = temp.idx2
    ReDim Preserve con(f_numcon) As contam_param
  End If
  If temp.idx3 = 0 Then
    con(temp.idx2).param(idx) = temp.pval
  Else
    If temp.idx3 > con(temp.idx2).numprog Then
      con(temp.idx2).numprog = temp.idx3
      ReDim Preserve con(temp.idx2).progeny(temp.idx3) As contam_progeny_param
    End If
    con(temp.idx2).progeny(temp.idx3).param(idx) = temp.pval
  End If
End Sub

Private Sub loadprm()
  Dim i As Long, j As Long, k As Long, m As Long
  Dim gml As Double
  Dim spa As Double
  Dim mx As Long
  Dim prm As String
  Dim prefix As String
  Dim fle As parmfile
  Dim sval As Boolean
  
  numloc = 0
  m_numloc = 0
 
  If open_parm(fle, FUIName, 2) Then
    Do Until EOCF(fle.file)
      If read_parmrec(fle, temp) Then
        Select Case temp.pName
          Case "fui"
            Loading.Gauge1.Max = Val(temp.idx1)
            Loading.Gauge1.value = 0
            For m = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                Loading.update
                If temp.idx1 = siteIdx Then
                  Select Case temp.pName
                    Case "numcon"
                      f_numcon = Val(temp.pval)
                      ReDim Preserve con(f_numcon) As contam_param
                    Case "clktype"
                        If f_numcon < temp.idx2 Then
                          f_numcon = temp.idx2
                          ReDim Preserve con(f_numcon) As contam_param
                        End If
                        If temp.idx3 = 0 Then
                          If temp.pval = 1 Then
                            con(temp.idx2).rad = True
                          Else
                            con(temp.idx2).rad = False
                          End If
                        Else
                          If con(temp.idx2).numprog < temp.idx3 Then
                            con(temp.idx2).numprog = temp.idx3
                            ReDim Preserve con(temp.idx2).progeny(temp.idx3) As contam_progeny_param
                          End If
                        End If
                    Case "fscname"
                        If f_numcon < temp.idx2 Then
                          f_numcon = temp.idx2
                          ReDim Preserve con(f_numcon) As contam_param
                        End If
                        If temp.idx3 = 0 Then
                          con(temp.idx2).Name = temp.pval
                        Else
                          If con(temp.idx2).numprog < temp.idx3 Then
                            con(temp.idx2).numprog = temp.idx3
                            ReDim Preserve con(temp.idx2).progeny(temp.idx3) As contam_progeny_param
                          End If
                          con(temp.idx2).progeny(temp.idx3).Name = temp.pval
                        End If
                    Case "fscasid"
                        If f_numcon < temp.idx2 Then
                          f_numcon = temp.idx2
                          ReDim Preserve con(f_numcon) As contam_param
                        End If
                        If temp.idx3 = 0 Then
                          con(temp.idx2).cas = temp.pval
                        Else
                          If con(temp.idx2).numprog < temp.idx3 Then
                            con(temp.idx2).numprog = temp.idx3
                            ReDim Preserve con(temp.idx2).progeny(temp.idx3) As contam_progeny_param
                          End If
                          con(temp.idx2).progeny(temp.idx3).cas = temp.pval
                        End If
                    Case "clsol"
                        fillChem WATER_SOLUBILITY_IDX
                    Case "clwhalf"
                        fillChem HALF_LIFE_IDX
                    Case "clwm"
                        fillChem MW_IDX
                  End Select
                End If
              End If
            Next m
            ' convert mg/L solubilities to pCi/mL
            For i = 1 To f_numcon
              If con(i).rad And Val(con(i).param(WATER_SOLUBILITY_IDX)) <> 0 Then
                gml = convert("mg/l", "g/ml", Val(con(i).param(WATER_SOLUBILITY_IDX)))
                spa = 130600000 / (Val(con(i).param(HALF_LIFE_IDX)) * Val(con(i).param(MW_IDX)))
                'convert from g/ml tp pci/ml
                con(i).param(WATER_SOLUBILITY_IDX) = CStr(gml * spa * 1000000000000#)
                For j = 1 To con(i).numprog
                  gml = convert("mg/l", "g/ml", Val(con(i).progeny(j).param(WATER_SOLUBILITY_IDX)))
                  spa = 130600000 / (Val(con(i).progeny(j).param(HALF_LIFE_IDX)) * Val(con(i).progeny(j).param(MW_IDX)))
                  'convert from g/ml tp pci/ml
                  con(i).progeny(j).param(WATER_SOLUBILITY_IDX) = CStr(gml * spa * 1000000000000#)
                Next
              End If
            Next
          
            
          Case modName
            Loading.Gauge1.Max = Val(temp.idx1)
            Loading.Gauge1.value = 0
            For m = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                Loading.update
                Select Case temp.pName
                  Case "CVTFormat":    CVTFormat = "General Number" 'CVTFormat = temp.pval
                  Case "tfinal":  SetTFinal temp
                  Case "ntimes":  SetNTimes temp
                  Case "wwname"
                    If temp.idx1 > m_numloc Then
                      m_numloc = temp.idx1
                      ReDim Preserve locale(m_numloc) As locparm
                    End If
                    locale(temp.idx1).Name = temp.pval
                    locale(temp.idx1).idx = 0
                  Case "wwkind":
                    If temp.idx1 > m_numloc Then
                      m_numloc = temp.idx1
                      ReDim Preserve locale(m_numloc) As locparm
                    End If
                    locale(temp.idx1).kind = Val(temp.pval)
                  Case "wwdepth":      fillet DEPTH_IDX
                  Case "wwwidth":      fillet WIDTH_IDX
                  Case "wwdist":       locfillet LOCATION_DIST
                  Case "wwdischg":     locfillet AVG_DISCHARGE
                  Case "wwy":          locfillet YCOORD
                  Case "wwx":          locfillet XCOORD
                  Case "wwz":          locfillet ZCOORD
                  Case "wwveloc":      fillet VELOCITY_IDX
                  Case "wwcasid"
                    If m_numcon < temp.idx1 Then
                      m_numcon = temp.idx1
                      ReDim Preserve model(temp.idx1) As model_contam_param
                    End If
                    If temp.idx2 = 0 Then
                      model(temp.idx1).cas = temp.pval
                    Else
                      If temp.idx2 > model(temp.idx1).numprog Then
                        model(temp.idx1).numprog = temp.idx2
                        ReDim Preserve model(temp.idx1).progeny(temp.idx2) As model_progeny_param
                      End If
                      model(temp.idx1).progeny(temp.idx2).cas = temp.pval
                    End If
                  Case "wwsol", "wwrsol", "wwshalf"
                     Select Case temp.pName
                       Case "wwsol", "wwrsol": i = WATER_SOLUBILITY_IDX
                       Case "wwshalf": i = HALF_LIFE_IDX
                     End Select
                     If temp.idx1 > m_numcon Then
                       m_numcon = temp.idx1
                       ReDim Preserve model(temp.idx1) As model_contam_param
                     End If
                     With model(temp.idx1)
                      If temp.idx2 = 0 Then
                        .idx = 0
                        If temp.pval <> "EMPTY" Then .param(i).value = convert(temp.cunit, temp.uunit, Val(temp.pval))
                        .param(i).uunt = temp.uunit
                        .param(i).ref = temp.ref
                      Else
                        If .numprog < temp.idx2 Then
                         .numprog = temp.idx2
                          ReDim Preserve .progeny(temp.idx2) As model_progeny_param
                        End If
                        With .progeny(temp.idx2)
                          .idx = 0
                          If temp.pval <> "EMPTY" Then .param(i).value = convert(temp.cunit, temp.uunit, Val(temp.pval))
                          .param(i).uunt = temp.uunit
                          .param(i).ref = temp.ref
                        End With    ' .progeny(temp.idx2)
                      End If
                    End With    ' model(temp.idx1)
                End Select
              End If
            Next
          Case "csm"
            mx = -1
            For i = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                'assumes modname will always occur before sink variables
                If temp.pName = "modid" And temp.pval = modName Then
                  mx = temp.idx2
                End If
                If mx > -1 And temp.idx2 = mx And temp.idx1 = siteIdx Then
                  Select Case temp.pName
                    Case "moddespath"
                      DesName = temp.pval
                    Case "modsinkid"
                      SizeLocal
                      loc(temp.idx3).Name = temp.pval
                    Case "modsinklabel"
                      SizeLocal
                      loc(temp.idx3).lbl = temp.pval
                    Case "modsinktype"
                      SizeLocal
                      loc(temp.idx3).type = temp.pval
                    Case "modsinkqual"
                      SizeLocal
                      loc(temp.idx3).qual = temp.pval
                  End Select
                End If
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
    
    For i = 1 To numloc
      If InStr(loc(i).Name, "sen") <> 1 Then
        For j = 1 To m_numloc
          If locale(j).Name = loc(i).Name And InStr(loc(i).type, "wcf") Mod 4 = 1 Then
            locale(j).idx = i
            locale(j).label = loc(i).lbl
            Exit For
          End If
        Next j
        If j > m_numloc And InStr(loc(i).type, "wcf") Mod 4 = 1 Then
          m_numloc = m_numloc + 1
          ReDim Preserve locale(m_numloc) As locparm
          locale(m_numloc).idx = i
          locale(m_numloc).Name = loc(i).Name
          locale(m_numloc).label = loc(i).lbl
          locale(m_numloc).parms(LOCATION_DIST).uunt = "cm"
          locale(m_numloc).parms(LOCATION_DIST).punt = "cm"
          locale(m_numloc).parms(LOCATION_DIST).ref = 0
          locale(m_numloc).parms(AVG_DISCHARGE).uunt = "cm^3/day"
          locale(m_numloc).parms(AVG_DISCHARGE).punt = "cm^3/day"
          locale(m_numloc).parms(AVG_DISCHARGE).ref = 0
          locale(m_numloc).parms(YCOORD).uunt = "m"
          locale(m_numloc).parms(YCOORD).punt = "m"
          locale(m_numloc).parms(YCOORD).ref = 0
          locale(m_numloc).parms(XCOORD).uunt = "m"
          locale(m_numloc).parms(XCOORD).punt = "m"
          locale(m_numloc).parms(XCOORD).ref = 0
          locale(m_numloc).parms(ZCOORD).uunt = "m"
          locale(m_numloc).parms(ZCOORD).punt = "m"
          locale(m_numloc).parms(ZCOORD).ref = 0
          'all exposure locations, flux (kind=1) locations are no longer output by riv module
          locale(m_numloc).kind = 0
        End If
      End If
    Next

'resolve contaminate differences if contaminate no longer exists its index is 0
    For i = 1 To f_numcon
      For j = 1 To m_numcon
        If model(j).cas = con(i).cas Then
          model(j).idx = i
          If model(j).param(WATER_SOLUBILITY_IDX).value = "" Or model(j).param(WATER_SOLUBILITY_IDX).value = Empty Then
            model(j).param(WATER_SOLUBILITY_IDX).value = con(i).param(WATER_SOLUBILITY_IDX)
            If con(i).rad Then
              model(j).param(WATER_SOLUBILITY_IDX).punt = uStr(RAD_SOLUBILITY)
              model(j).param(WATER_SOLUBILITY_IDX).uunt = uStr(RAD_SOLUBILITY)
            Else
              model(j).param(WATER_SOLUBILITY_IDX).punt = uStr(SOLUBILITY)
              model(j).param(WATER_SOLUBILITY_IDX).uunt = uStr(SOLUBILITY)
            End If
            model(j).param(WATER_SOLUBILITY_IDX).ref = 0
          End If
          If model(j).param(HALF_LIFE_IDX).value = "" Or model(j).param(HALF_LIFE_IDX).value = Empty Then
            model(j).param(HALF_LIFE_IDX).value = con(i).param(HALF_LIFE_IDX)
            model(j).param(HALF_LIFE_IDX).punt = uStr(HALF_LIFE)
            model(j).param(HALF_LIFE_IDX).uunt = uStr(HALF_LIFE)
            model(j).param(HALF_LIFE_IDX).ref = 0
          End If
          Exit For
        End If
      Next j
      If m_numcon < j Then
        m_numcon = j
        ReDim Preserve model(j) As model_contam_param
        model(j).cas = con(i).cas
        model(j).idx = i
        model(j).param(WATER_SOLUBILITY_IDX).value = con(i).param(WATER_SOLUBILITY_IDX)
        If con(i).rad Then
          model(j).param(WATER_SOLUBILITY_IDX).punt = uStr(RAD_SOLUBILITY)
          model(j).param(WATER_SOLUBILITY_IDX).uunt = uStr(RAD_SOLUBILITY)
        Else
          model(j).param(WATER_SOLUBILITY_IDX).punt = uStr(SOLUBILITY)
          model(j).param(WATER_SOLUBILITY_IDX).uunt = uStr(SOLUBILITY)
        End If
        model(j).param(WATER_SOLUBILITY_IDX).ref = 0
        model(j).param(HALF_LIFE_IDX).value = con(i).param(HALF_LIFE_IDX)
        model(j).param(HALF_LIFE_IDX).punt = uStr(HALF_LIFE)
        model(j).param(HALF_LIFE_IDX).uunt = uStr(HALF_LIFE)
        model(j).param(HALF_LIFE_IDX).ref = 0
      End If
    Next i
    For i = 1 To m_numcon
      If model(i).idx > 0 Then
        cboParent.AddItem con(model(i).idx).Name
        cboParent.ItemData(cboParent.NewIndex) = i
      End If
    Next i
  
  'resolve progeny differences if progeny no longer exists its index is 0
    For i = 1 To m_numcon
      If model(i).idx <> 0 Then
        For j = 1 To con(model(i).idx).numprog
          For k = 1 To model(i).numprog
            If model(i).progeny(k).cas = con(model(i).idx).progeny(j).cas Then
              model(i).progeny(k).idx = j
              If model(i).progeny(k).param(WATER_SOLUBILITY_IDX).value = "" Or model(i).progeny(k).param(WATER_SOLUBILITY_IDX).value = Empty Then
                model(i).progeny(k).param(WATER_SOLUBILITY_IDX).value = con(model(i).idx).progeny(j).param(WATER_SOLUBILITY_IDX)
                If con(model(i).idx).rad Then
                  model(i).progeny(k).param(WATER_SOLUBILITY_IDX).punt = uStr(RAD_SOLUBILITY)
                  model(i).progeny(k).param(WATER_SOLUBILITY_IDX).uunt = uStr(RAD_SOLUBILITY)
                Else
                  model(i).progeny(k).param(WATER_SOLUBILITY_IDX).punt = uStr(SOLUBILITY)
                  model(i).progeny(k).param(WATER_SOLUBILITY_IDX).uunt = uStr(SOLUBILITY)
                End If
              End If
              If model(i).progeny(k).param(HALF_LIFE_IDX).value = "" Or model(i).progeny(k).param(HALF_LIFE_IDX).value = Empty Then
                model(i).progeny(k).param(HALF_LIFE_IDX).value = con(model(i).idx).progeny(j).param(HALF_LIFE_IDX)
                model(i).progeny(k).param(HALF_LIFE_IDX).punt = uStr(HALF_LIFE)
                model(i).progeny(k).param(HALF_LIFE_IDX).uunt = uStr(HALF_LIFE)
                model(i).progeny(k).param(HALF_LIFE_IDX).ref = 0
              End If
              Exit For
            End If
          Next k
          If k > model(i).numprog Then
            model(i).numprog = k
            ReDim Preserve model(i).progeny(k) As model_progeny_param
            model(i).progeny(k).cas = con(model(i).idx).progeny(j).cas
            model(i).progeny(k).idx = j
            model(i).progeny(k).param(WATER_SOLUBILITY_IDX).value = con(model(i).idx).progeny(j).param(WATER_SOLUBILITY_IDX)
            If con(model(i).idx).rad Then
              model(i).progeny(k).param(WATER_SOLUBILITY_IDX).punt = uStr(RAD_SOLUBILITY)
              model(i).progeny(k).param(WATER_SOLUBILITY_IDX).uunt = uStr(RAD_SOLUBILITY)
            Else
              model(i).progeny(k).param(WATER_SOLUBILITY_IDX).punt = uStr(SOLUBILITY)
              model(i).progeny(k).param(WATER_SOLUBILITY_IDX).uunt = uStr(SOLUBILITY)
            End If
            model(i).progeny(k).param(WATER_SOLUBILITY_IDX).ref = 0
            model(i).progeny(k).param(HALF_LIFE_IDX).value = con(model(i).idx).progeny(j).param(HALF_LIFE_IDX)
            model(i).progeny(k).param(HALF_LIFE_IDX).punt = uStr(HALF_LIFE)
            model(i).progeny(k).param(HALF_LIFE_IDX).uunt = uStr(HALF_LIFE)
            model(i).progeny(k).param(HALF_LIFE_IDX).ref = 0
          End If
        Next j
      End If
    Next i
    
    For i = 1 To m_numloc
      If locale(i).idx > 0 Then Combo2.AddItem locale(i).label + " (" + locale(i).Name + ")"
    Next
    If Combo2.ListCount = 0 Then
      PutError "Not connected to an exposure location"
      Unload TimePts
      EndModule
    Else
      Combo2.ListIndex = 0
    End If
  Else
    PutError "Can't find or open file " & FUIName
    EndModule
  End If
End Sub

Private Sub Form_load()
  Dim i As Long
  
  uStr(RAD_SOLUBILITY) = "pCi/ml"
  uStr(SOLUBILITY) = "mg/l"
  uStr(HALF_LIFE) = "day"
  
  hStr(0) = "WWSOL"
  hStr(1) = "WWSHALF"
  
  StartModule River, App.title, 5
  SetHelpFile App.Path + "\riv.htm"
  SetRefFile ReplaceExt(FUIName, "ref")
  For i = VELOCITY_IDX To ZCOORD_IDX
    If i = 5 Then i = 7
    get_conversion_items unit(i).Tag, unit(i)
  Next
  SetTimeTitle River.Caption
  Loading.Show
  loadng = False
  loadprm
  BackFillParent
  ConParms.ListIndex = 0
  ProgParms.ListIndex = 0
  Unload Loading
  loadng = False
  Refresh
End Sub

Private Sub save_Click()
  Dim i As Long, j As Long, k As Long, cnt As Long
  Dim fName As String
  Dim parm As parmrec
  Dim fle As parmfile
  Dim tmp As parmfile
  Dim cnt1 As Long
  Dim cnt2 As Long
  Dim UserFormat As String
  UserFormat = CVTFormat

  txt_LostFocus RefItem
  fName = RunName & ".GID"
  If open_parm(fle, fName, 1) Then
    set_parm parm, "CVTFormat", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CVTFormat
    write_parmrec fle, parm
    CVTFormat = "General Number"
    WriteTime fle
    For i = VELOCITY_IDX To WIDTH_IDX
      set_parm parm, txt(i).Tag, 0, 0, 0, 0, 0, 0, Val(ref(i).Tag), unit(i).Text, unit(i).Tag, convert(unit(i).Text, unit(i).Tag, Val(txt(i).Text))
      write_parmrec fle, parm
      If er(i) Then PutError "Parameter " & txt(i).Tag & " is invalid"
    Next
   'All exposure points
    cnt1 = 0
    cnt2 = 0
    For i = 1 To m_numloc
      If locale(i).idx > 0 Then
        If locale(i).kind = 1 Then
          cnt1 = cnt1 + 1
        Else
          cnt2 = cnt2 + 1
        End If
        set_parm parm, "wwname", cnt1 + cnt2, 0, 0, 0, 0, 0, 0, "N/A", "N/A", locale(i).Name
        write_parmrec fle, parm
        set_parm parm, "wwkind", cnt1 + cnt2, 0, 0, 0, 0, 0, 0, "N/A", "N/A", Str(locale(i).kind)
        write_parmrec fle, parm
        k = 2
        For j = LOCATION_DIST To ZCOORD
          If j = 3 Then k = 4
          set_parm parm, txt(j + k).Tag, cnt1 + cnt2, 0, 0, 0, 0, 0, locale(i).parms(j).ref, locale(i).parms(j).uunt, locale(i).parms(j).punt, convert(locale(i).parms(j).uunt, locale(i).parms(j).punt, Val(locale(i).parms(j).value))
          write_parmrec fle, parm
          If locale(i).parms(j).value = "" Then PutError "Parameter " & txt(j + k).Tag & " for location " & locale(i).Name & " is invalid"
        Next
      End If
    Next
    set_parm parm, "wwnumflux", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", Str(cnt1)
    write_parmrec fle, parm
    set_parm parm, "wwnumconc", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", Str(cnt2)
    write_parmrec fle, parm
    
    'Contaminant parameters
    For i = 1 To m_numcon
      If model(i).idx <> 0 Then
        set_parm parm, "wwcasid", model(i).idx, 0, 0, 0, 0, 0, model(i).param(0).ref, "N/A", "N/A", model(i).cas
        write_parmrec fle, parm
        
        If Len(model(i).param(WATER_SOLUBILITY_IDX).value) = 0 Then
          If con(model(i).idx).rad Then
            PutError "Missing RAD water solubility (wwrsol) value for " & model(i).cas & " on Constituent Properties tab"
          Else
            PutError "Missing water solubility (WWSOL) value for " & model(i).cas & " on Constituent Properties tab"
          End If
        Else
          If model(i).param(WATER_SOLUBILITY_IDX).value = Empty Or Val(model(i).param(WATER_SOLUBILITY_IDX).value) < 0 Then
            If con(model(i).idx).rad Then
              PutError "Invalid RAD water solubility (wwrsol) value for " & model(i).cas & " on Constituent Properties tab"
              set_parm parm, "wwrsol", model(i).idx, 0, 0, 0, 0, 0, model(i).param(WATER_SOLUBILITY_IDX).ref, model(i).param(WATER_SOLUBILITY_IDX).uunt, uStr(RAD_SOLUBILITY), "EMPTY"
            Else
              PutError "Invalid water solubility (WWSOL) value for " & model(i).cas & " on Constituent Properties tab"
              set_parm parm, "wwsol", model(i).idx, 0, 0, 0, 0, 0, model(i).param(WATER_SOLUBILITY_IDX).ref, model(i).param(WATER_SOLUBILITY_IDX).uunt, uStr(SOLUBILITY), "EMPTY"
            End If
          Else
            If con(model(i).idx).rad Then
              set_parm parm, "wwrsol", model(i).idx, 0, 0, 0, 0, 0, model(i).param(WATER_SOLUBILITY_IDX).ref, model(i).param(WATER_SOLUBILITY_IDX).uunt, uStr(RAD_SOLUBILITY), convert(model(i).param(WATER_SOLUBILITY_IDX).uunt, uStr(RAD_SOLUBILITY), Val(model(i).param(WATER_SOLUBILITY_IDX).value))
            Else
              set_parm parm, "wwsol", model(i).idx, 0, 0, 0, 0, 0, model(i).param(WATER_SOLUBILITY_IDX).ref, model(i).param(WATER_SOLUBILITY_IDX).uunt, uStr(SOLUBILITY), convert(model(i).param(WATER_SOLUBILITY_IDX).uunt, uStr(SOLUBILITY), Val(model(i).param(WATER_SOLUBILITY_IDX).value))
            End If
          End If
          write_parmrec fle, parm
        End If
        
        If Len(model(i).param(HALF_LIFE_IDX).value) = 0 Then
          PutError "Missing half-life (WWSHALF) value for " & model(i).cas & " on Constituent Properties tab"
        Else
          If model(i).param(HALF_LIFE_IDX).value = Empty Or Val(model(i).param(HALF_LIFE_IDX).value) <= 0 Then
            PutError "Invalid half-life (WWSHALF) value for " & model(i).cas & " on Constituent Properties tab"
            set_parm parm, "wwshalf", model(i).idx, 0, 0, 0, 0, 0, model(i).param(HALF_LIFE_IDX).ref, model(i).param(HALF_LIFE_IDX).uunt, uStr(HALF_LIFE), "EMPTY"
          Else
            set_parm parm, "wwshalf", model(i).idx, 0, 0, 0, 0, 0, model(i).param(HALF_LIFE_IDX).ref, model(i).param(HALF_LIFE_IDX).uunt, uStr(HALF_LIFE), convert(model(i).param(HALF_LIFE_IDX).uunt, uStr(HALF_LIFE), Val(model(i).param(HALF_LIFE_IDX).value))
          End If
          write_parmrec fle, parm
        End If
        
        For j = 1 To model(i).numprog
          With model(i).progeny(j)
            If .idx <> 0 Then
              set_parm parm, "wwcasid", model(i).idx, .idx, 0, 0, 0, 0, .param(0).ref, "N/A", "N/A", .cas
              write_parmrec fle, parm
              If Len(.param(WATER_SOLUBILITY_IDX).value) = 0 Then
                If con(model(i).idx).rad Then
                  PutError "Missing RAD water solubility (wwrsol) value for " & .cas & " daughter of " & model(i).cas & " on Constituent Properties tab"
                Else
                  PutError "Missing water solubility (WWSOL) value for " & .cas & " daughter of " & model(i).cas & " on Constituent Properties tab"
                End If
              Else
                If .param(WATER_SOLUBILITY_IDX).value = Empty Or Val(.param(WATER_SOLUBILITY_IDX).value) < 0 Then
                  If con(model(i).idx).rad Then
                    PutError "Invalid RAD water solubility (wwrsol) value for " & .cas & " daughter of " & model(i).cas & " on Constituent Properties tab"
                    set_parm parm, "wwrsol", model(i).idx, .idx, 0, 0, 0, 0, .param(WATER_SOLUBILITY_IDX).ref, .param(WATER_SOLUBILITY_IDX).uunt, uStr(RAD_SOLUBILITY), "EMPTY"
                  Else
                    PutError "Invalid water solubility (WWSOL) value for " & .cas & " daughter of " & model(i).cas & " on Constituent Properties tab"
                    set_parm parm, "wwsol", model(i).idx, .idx, 0, 0, 0, 0, .param(WATER_SOLUBILITY_IDX).ref, .param(WATER_SOLUBILITY_IDX).uunt, uStr(SOLUBILITY), "EMPTY"
                  End If
                Else
                  If con(model(i).idx).rad Then
                    set_parm parm, "wwrsol", model(i).idx, .idx, 0, 0, 0, 0, .param(WATER_SOLUBILITY_IDX).ref, .param(WATER_SOLUBILITY_IDX).uunt, uStr(RAD_SOLUBILITY), convert(.param(WATER_SOLUBILITY_IDX).uunt, uStr(RAD_SOLUBILITY), Val(.param(WATER_SOLUBILITY_IDX).value))
                  Else
                    set_parm parm, "wwsol", model(i).idx, .idx, 0, 0, 0, 0, .param(WATER_SOLUBILITY_IDX).ref, .param(WATER_SOLUBILITY_IDX).uunt, uStr(SOLUBILITY), convert(.param(WATER_SOLUBILITY_IDX).uunt, uStr(SOLUBILITY), Val(.param(WATER_SOLUBILITY_IDX).value))
                  End If
                End If
                write_parmrec fle, parm
              End If
              
              If Len(.param(HALF_LIFE_IDX).value) = 0 Then
                PutError "Missing half-life (WWSHALF) value for " & .cas & " daughter of " & model(i).cas & " on Constituent Properties tab"
              Else
                  If .param(HALF_LIFE_IDX).value = Empty Or Val(.param(HALF_LIFE_IDX).value) <= 0 Then
                    PutError "Missing half-life (WWSHALF) value for " & .cas & " daughter of " & model(i).cas & " on Constituent Properties tab"
                    set_parm parm, "wwshalf", model(i).idx, .idx, 0, 0, 0, 0, .param(HALF_LIFE_IDX).ref, .param(HALF_LIFE_IDX).uunt, uStr(HALF_LIFE), "EMPTY"
                  Else
                    set_parm parm, "wwshalf", model(i).idx, .idx, 0, 0, 0, 0, .param(HALF_LIFE_IDX).ref, .param(HALF_LIFE_IDX).uunt, uStr(HALF_LIFE), convert(.param(HALF_LIFE_IDX).uunt, uStr(HALF_LIFE), Val(.param(HALF_LIFE_IDX).value))
                  End If
                  write_parmrec fle, parm
              End If
            
            End If
          End With  ' model(i).progeny(j)
        Next
      End If
    Next
    close_parm fle
    CVTFormat = UserFormat
    SetFormat Me
  Else
    PutError "Unable to create transaction file" & RunName & ".GID"
  End If
  Unload TimePts
  EndModule
End Sub

Private Sub SSCommand1_Click()
  If cboProgeny.ListIndex > 0 Then cboProgeny.ListIndex = cboProgeny.ListIndex - 1
End Sub

Private Sub SSCommand2_Click()
  If cboProgeny.ListIndex < cboProgeny.ListCount - 1 Then cboProgeny.ListIndex = cboProgeny.ListIndex + 1
End Sub

Private Sub SSCommand4_Click()
  If cboParent.ListIndex > 0 Then cboParent.ListIndex = cboParent.ListIndex - 1
End Sub

Private Sub SSCommand5_Click()
  If cboParent.ListIndex < cboParent.ListCount - 1 Then cboParent.ListIndex = cboParent.ListIndex + 1
End Sub

Private Sub Command1_Click()
  If ConParms.ListIndex < ConParms.ListCount - 1 Then ConParms.ListIndex = ConParms.ListIndex + 1
End Sub

Private Sub Command2_Click()
  If ConParms.ListIndex > 0 Then ConParms.ListIndex = ConParms.ListIndex - 1
End Sub

Private Sub Command3_Click()
  If ProgParms.ListIndex < ProgParms.ListCount - 1 Then ProgParms.ListIndex = ProgParms.ListIndex + 1
End Sub

Private Sub Command4_Click()
  If ProgParms.ListIndex > 0 Then ProgParms.ListIndex = ProgParms.ListIndex - 1
End Sub

Private Sub SSTab1_Click(PreviousTab As Integer)
  
  SSFrame1(PreviousTab).Enabled = False
  SSFrame1(SSTab1.Tab).Enabled = True
  noact.Enabled = False
  
  If SSTab1.Tab = 1 And cboParent.ListIndex = -1 Then SSCommand5_Click
  mes = ""
End Sub

Private Function er(Index As Long) As Boolean
  Dim tval As Double
  Dim t1 As String
  Dim t2 As String
  Dim m As String
  Dim i As Integer
  Dim dischg As Double
  
  m = ""
  er = False
  If txt(Index).Text = "" Then er = True
  tval = Val(txt(Index).Text)
  Select Case Index
    Case VELOCITY_IDX, DEPTH_IDX, WIDTH_IDX
      m = "Value must be greater than zero"
      If (tval <= 0) Then er = True

' fix up annual discharge
      dischg = Val(convert(unit(0).Text, unit(0).Tag, Val(txt(0).Text)))
      dischg = dischg * Val(convert(unit(1).Text, unit(1).Tag, Val(txt(1).Text)))
      dischg = dischg * Val(convert(unit(2).Text, unit(2).Tag, Val(txt(2).Text)))
      If dischg > 0 Then
        For i = 1 To m_numloc
          locale(i).parms(AVG_DISCHARGE).value = dischg
          locale(i).parms(AVG_DISCHARGE).uunt = "cm^3/day"
        Next
        txt(AVG_DISCHARGE_IDX).Text = dischg
        unit(AVG_DISCHARGE_IDX).Text = "cm^3/day"
      Else
        For i = 1 To m_numloc
          locale(i).parms(AVG_DISCHARGE).value = ""
        Next
        txt(AVG_DISCHARGE_IDX).Text = ""
      End If
    
    Case AVG_DISCHARGE_IDX
      m = "Value must be greater than zero"
      If (tval <= 0) Then er = True
    Case DISTANCE_IDX
      m = "Value must be greater than or equal to zero"
      If (tval < 0) Then er = True
    Case CONTAM_PARAMS
      If ConParms.ListIndex = WATER_SOLUBILITY_IDX Then
        m = "Value must be greater than or equal to 0"
        If tval < 0 Then er = True
      Else
        m = "Value must be greater than 0"
        If tval <= 0 Then er = True
      End If
    Case PROGENY_PARAMS
      If ProgParms.ListIndex = WATER_SOLUBILITY_IDX Then
        m = "Value must be greater than or equal to 0"
        If tval < 0 Then er = True
      Else
        m = "Value must be greater than 0"
        If tval <= 0 Then er = True
      End If
  End Select
  mes = Space(140 - Len(m)) & m
  If er Then
    txt(Index).BackColor = &H8080FF
  Else
    txt(Index).BackColor = &HC0FFC0
  End If
End Function

Private Sub elocchange(tx As String, oldtx As String)
  Dim i As Long, j As Long, k As Long
  
  For i = 1 To m_numloc
    If locale(i).label + " (" + locale(i).Name + ")" = oldtx Then
      k = 2
      For j = LOCATION_DIST To ZCOORD
         If j = 3 Then k = 4
         locale(i).parms(j).value = txt(j + k).Text
         locale(i).parms(j).uunt = unit(j + k).Text
         locale(i).parms(j).punt = unit(j + k).Tag
         locale(i).parms(j).ref = ref(j + k).Tag
      Next
      Exit For
    End If
  Next
  For i = 1 To m_numloc
    If locale(i).label + " (" + locale(i).Name + ")" = tx Then
      k = 2
      loadng = True
      For j = LOCATION_DIST To ZCOORD
         If j = 3 Then k = 4
         set_unit unit(j + k), locale(i).parms(j).uunt
         txt(j + k).Text = locale(i).parms(j).value
         ref(j + k).Caption = "Ref: " & locale(i).parms(j).ref
         ref(j + k).Tag = locale(i).parms(j).ref
      Next
      loadng = False
      Exit For
    End If
  Next
End Sub

Private Sub Timer1_Timer()
  mes = Mid(mes, 2) & Mid(mes, 1, 1)
End Sub

Private Sub txt_Change(Index As Integer)
Dim chk As Double
On Error GoTo toolarge
  er CLng(Index)
  chk = CDbl(txt(Index).Text)
  Exit Sub
toolarge:
  txt(Index).BackColor = &H8080FF
End Sub
            
Private Sub BackFillParent()
Dim i As Long
Dim j As Long
Dim m As Long
Dim n As Long
Dim p As Long
  
  For m = 1 To UBound(model)
    For n = 1 To model(m).numprog
      For i = 1 To UBound(model)
        If model(i).cas = model(m).progeny(n).cas Then
          For p = 0 To 2
            model(i).param(p).value = model(m).progeny(n).param(p).value
            model(i).param(p).uunt = model(m).progeny(n).param(p).uunt
            model(i).param(p).ref = model(m).progeny(n).param(p).ref
          Next
        End If
        For j = 1 To model(i).numprog
          If model(i).progeny(j).cas = model(m).progeny(n).cas Then
            For p = 0 To 2
              model(i).progeny(j).param(p).value = model(m).progeny(n).param(p).value
              model(i).progeny(j).param(p).uunt = model(m).progeny(n).param(p).uunt
              model(i).progeny(j).param(p).ref = model(m).progeny(n).param(p).ref
            Next
          End If
        Next
      Next
    Next
  Next
End Sub
            
Private Sub FillParent(cas As String, pIdx As Long, value As String, uunt As String, ref As Long)
Dim i As Long
Dim j As Long

  For i = 1 To UBound(model)
    If model(i).cas = cas Then
      model(i).param(pIdx).value = value
      model(i).param(pIdx).uunt = uunt
      model(i).param(pIdx).ref = ref
    End If
    For j = 1 To model(i).numprog
      If model(i).progeny(j).cas = cas Then
        model(i).progeny(j).param(pIdx).value = value
        model(i).progeny(j).param(pIdx).uunt = uunt
        model(i).progeny(j).param(pIdx).ref = ref
      End If
    Next
  Next
End Sub


Private Sub GetModelParent()
  Dim i As Long
  
  i = listIndexOf(cboParent, oldParent)
  If i >= 0 Then i = cboParent.ItemData(i)
  If i > 0 Then
    With model(i).param(prevSelectedIdx)
      If txt(CONTAM_PARAMS).Text = "" Then
        .value = Empty
      Else
        .value = txt(CONTAM_PARAMS).Text
      End If
      .uunt = unit(CONTAM_PARAMS).Text
      .ref = Val(ref(CONTAM_PARAMS).Tag)
      'fill product params
      FillParent model(i).cas, prevSelectedIdx, .value, .uunt, .ref
    End With
  End If
End Sub

Private Sub GetModelProgeny()
  Dim i As Long
  Dim j As Long
  
  i = listIndexOf(cboParent, oldParent)
  If i >= 0 Then i = cboParent.ItemData(i)
  j = listIndexOf(cboProgeny, oldProgeny)
  If j >= 0 Then j = cboProgeny.ItemData(j)
  If i > 0 And j >= 0 Then
    With model(i).progeny(j).param(prevSelectedProgIdx)
      If txt(PROGENY_PARAMS).Text = "" Then
        .value = Empty
      Else
        .value = txt(PROGENY_PARAMS).Text
      End If
      .uunt = unit(PROGENY_PARAMS).Text
      .ref = Val(ref(PROGENY_PARAMS).Tag)
      'fill parent params
      FillParent model(i).progeny(j).cas, prevSelectedProgIdx, .value, .uunt, .ref
    End With
  End If
End Sub

Private Sub PutModelParent()
  Dim i As Long
  
  i = cboParent.ListIndex
  If i >= 0 Then i = cboParent.ItemData(i)
  If i >= 0 Then
    unit(CONTAM_PARAMS).Clear
    With model(i).param(ConParms.ListIndex)
      If Len(.uunt) > 0 Then
        get_conversion_items .uunt, unit(CONTAM_PARAMS)
        set_unit unit(CONTAM_PARAMS), .uunt
      Else
        get_conversion_items_by_type getUnitType(getListParmName(ConParms)), unit(CONTAM_PARAMS)
      End If
      
      If Len(.value) = 0 Then
        txt(CONTAM_PARAMS).Text = ""
      Else
        txt(CONTAM_PARAMS).Text = .value
      End If
      ref(CONTAM_PARAMS).Caption = "Ref: " & .ref
      ref(CONTAM_PARAMS).Tag = .ref
    End With
  End If
End Sub

Private Sub PutModelProgeny()
  Dim i As Long
  Dim j As Long
  
  i = cboParent.ListIndex
  If i >= 0 Then i = cboParent.ItemData(i)
  j = cboProgeny.ListIndex
  If j >= 0 Then j = cboProgeny.ItemData(j)
  If j >= 0 Then
    unit(PROGENY_PARAMS).Clear
    With model(i).progeny(j).param(ProgParms.ListIndex)
      If Len(.uunt) > 0 Then
        get_conversion_items .uunt, unit(PROGENY_PARAMS)
        set_unit unit(PROGENY_PARAMS), .uunt
      Else
        get_conversion_items_by_type getUnitType(getListParmName(ProgParms)), unit(PROGENY_PARAMS)
      End If
      
      If Len(.value) = 0 Then
        txt(PROGENY_PARAMS).Text = ""
      Else
        txt(PROGENY_PARAMS).Text = .value
      End If
      ref(PROGENY_PARAMS).Caption = "Ref: " & .ref
      ref(PROGENY_PARAMS).Tag = .ref
    End With
  End If
End Sub

Private Sub unit_Click(Index As Integer)
  er CLng(Index)
End Sub

Private Sub Combo2_Click()
  elocchange Combo2.Text, oldLocation
  oldLocation = Combo2.Text
End Sub

Private Sub cboProgeny_Click()
  GetModelProgeny
  PutModelProgeny
  oldProgeny = cboProgeny.Text
End Sub

Private Sub cboParent_Click()
  Dim i As Long
  Dim j As Long
  
  GetModelParent
  GetModelProgeny
  PutModelParent
  oldParent = cboParent.Text
  cboProgeny.Clear
  i = cboParent.ListIndex
  If i >= 0 Then i = cboParent.ItemData(i)
  If i > 0 Then
    If con(model(i).idx).numprog = 0 Then
      txt(PROGENY_PARAMS).Visible = False
      unit(PROGENY_PARAMS).Visible = False
      ref(PROGENY_PARAMS).Visible = False
      Command3.Visible = False
      Command4.Visible = False
      SSCommand1.Visible = False
      SSCommand2.Visible = False
      lbl(8).Visible = False
      lbl(9).Visible = False
      cboProgeny.Visible = False
      ProgParms.Visible = False
    Else
      For j = 1 To model(i).numprog
        If model(i).progeny(j).idx > 0 Then
          cboProgeny.AddItem con(model(i).idx).progeny(model(i).progeny(j).idx).Name
          cboProgeny.ItemData(cboProgeny.NewIndex) = j
        End If
      Next
      txt(PROGENY_PARAMS).Visible = True
      unit(PROGENY_PARAMS).Visible = True
      ref(PROGENY_PARAMS).Visible = True
      Command3.Visible = True
      Command4.Visible = True
      SSCommand1.Visible = True
      SSCommand2.Visible = True
      lbl(8).Visible = True
      lbl(9).Visible = True
      cboProgeny.Visible = True
      oldProgeny = ""
      cboProgeny.ListIndex = 0
      ProgParms.Visible = True
    End If
  End If
End Sub

Private Sub ConParms_Click()
  Dim check As Boolean
  
  If loadng Then Exit Sub
  cboParent_Click
  prevSelectedIdx = ConParms.ListIndex
  HelpAnchor = hStr(ConParms.ListIndex)
  txt(CONTAM_PARAMS).Tag = HelpAnchor
End Sub

Private Sub ProgParms_Click()
  Dim check As Boolean
  
  If loadng Then Exit Sub
  cboProgeny_Click
  prevSelectedProgIdx = ProgParms.ListIndex
  HelpAnchor = hStr(ProgParms.ListIndex)
  txt(PROGENY_PARAMS).Tag = HelpAnchor
End Sub

Private Sub Combo2_GotFocus()
Dim m As String
  m = "Select the location"
  mes = Space(140 - Len(m)) & m
  noact.Enabled = False
  HelpAnchor = ""
End Sub

Private Sub cboProgeny_GotFocus()
Dim m As String
  m = "Select a progeny"
  mes = Space(140 - Len(m)) & m
  RefItem = PROGENY_PARAMS
  noact.Enabled = True
  HelpAnchor = cboProgeny.Tag
End Sub

Private Sub cboParent_GotFocus()
Dim m As String
  m = "Select a constituent"
  mes = Space(140 - Len(m)) & m
  RefItem = CONTAM_PARAMS
  noact.Enabled = True
  HelpAnchor = cboParent.Tag
End Sub

Private Sub ConParms_GotFocus()
Dim m As String
  m = "Select a constituent properties to edit"
  mes = Space(140 - Len(m)) & m
  RefItem = CONTAM_PARAMS
  noact.Enabled = True
  txt(CONTAM_PARAMS).Tag = hStr(ConParms.ListIndex)
  HelpAnchor = txt(CONTAM_PARAMS).Tag
End Sub

Private Sub ProgParms_GotFocus()
Dim m As String
  m = "Select a constituent properties to edit"
  mes = Space(140 - Len(m)) & m
  RefItem = PROGENY_PARAMS
  noact.Enabled = True
  txt(PROGENY_PARAMS).Tag = hStr(ProgParms.ListIndex)
  HelpAnchor = txt(PROGENY_PARAMS).Tag
End Sub

Private Sub Command1_GotFocus()
  ConParms_GotFocus
End Sub

Private Sub Command2_GotFocus()
  ConParms_GotFocus
End Sub

Private Sub Command3_GotFocus()
  ProgParms_GotFocus
End Sub

Private Sub Command4_GotFocus()
  ProgParms_GotFocus
End Sub

Private Sub SSCommand1_GotFocus()
  cboProgeny_GotFocus
End Sub

Private Sub SSCommand2_GotFocus()
  cboProgeny_GotFocus
End Sub

Private Sub SSCommand4_GotFocus()
  cboParent_GotFocus
End Sub

Private Sub SSCommand5_GotFocus()
  cboParent_GotFocus
End Sub

Private Sub SSTab1_GotFocus()
  mes = ""
  noact.Enabled = False
  Select Case SSTab1.Tab
  Case 0: HelpAnchor = "DIMENSIONS"
  Case 1: HelpAnchor = "PROPERTIES"
  End Select
End Sub

Private Sub unit_GotFocus(Index As Integer)
  er CLng(Index)
  RefItem = Index
  noact.Enabled = True
  HelpAnchor = txt(Index).Tag
End Sub

Private Sub txt_GotFocus(Index As Integer)
  er CLng(Index)
  RefItem = Index
  noact.Enabled = True
  HelpAnchor = txt(Index).Tag
End Sub

Private Sub txt_LostFocus(Index As Integer)
  Select Case Index
    Case DISTANCE_IDX, AVG_DISCHARGE_IDX, YCOORD_IDX, XCOORD_IDX, ZCOORD_IDX:   Combo2_Click
    Case CONTAM_PARAMS:                     cboParent_Click
    Case PROGENY_PARAMS:                    cboProgeny_Click
  End Select
End Sub

Private Sub unit_LostFocus(Index As Integer)
  Select Case Index
    Case DISTANCE_IDX, AVG_DISCHARGE_IDX, YCOORD_IDX, XCOORD_IDX, ZCOORD_IDX:   Combo2_Click
    Case CONTAM_PARAMS:                     cboParent_Click
    Case PROGENY_PARAMS:                    cboProgeny_Click
  End Select
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


