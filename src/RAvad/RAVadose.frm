VERSION 5.00
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "tabctl32.ocx"
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "threed32.ocx"
Begin VB.Form Vadose 
   BorderStyle     =   1  'Fixed Single
   ClientHeight    =   5208
   ClientLeft      =   1500
   ClientTop       =   2856
   ClientWidth     =   7680
   Icon            =   "RAVadose.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   NegotiateMenus  =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   434
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   640
   StartUpPosition =   2  'CenterScreen
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
      TabIndex        =   15
      TabStop         =   0   'False
      Top             =   4800
      Width           =   7680
   End
   Begin TabDlg.SSTab SSTab1 
      Height          =   4812
      Left            =   0
      TabIndex        =   3
      Top             =   0
      Width           =   7692
      _ExtentX        =   13568
      _ExtentY        =   8488
      _Version        =   393216
      Style           =   1
      TabHeight       =   529
      TabCaption(0)   =   "Dimensions"
      TabPicture(0)   =   "RAVadose.frx":030A
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "Timer1"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).Control(1)=   "SSFrame1(0)"
      Tab(0).Control(1).Enabled=   0   'False
      Tab(0).ControlCount=   2
      TabCaption(1)   =   "Characteristics"
      TabPicture(1)   =   "RAVadose.frx":0326
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "SSFrame1(1)"
      Tab(1).Control(0).Enabled=   0   'False
      Tab(1).ControlCount=   1
      TabCaption(2)   =   "Constituent Properties"
      TabPicture(2)   =   "RAVadose.frx":0342
      Tab(2).ControlEnabled=   0   'False
      Tab(2).Control(0)=   "SSFrame1(2)"
      Tab(2).Control(0).Enabled=   0   'False
      Tab(2).ControlCount=   1
      Begin Threed.SSFrame SSFrame1 
         Height          =   4020
         Index           =   1
         Left            =   -74760
         TabIndex        =   13
         Top             =   480
         Width           =   7212
         _Version        =   65536
         _ExtentX        =   12721
         _ExtentY        =   7091
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
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   10
            Left            =   4080
            TabIndex        =   63
            Tag             =   "emanation"
            Top             =   1080
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   7
            Left            =   4080
            TabIndex        =   40
            Tag             =   "darcy"
            Top             =   360
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   7
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   39
            Tag             =   "m/sec"
            Top             =   360
            Width           =   996
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   12
            Left            =   4080
            TabIndex        =   16
            Tag             =   "wgPart"
            Top             =   1800
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   9
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   7
            Tag             =   "fraction"
            Top             =   720
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   11
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   11
            Tag             =   "fraction"
            Top             =   1440
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   11
            Left            =   4080
            TabIndex        =   10
            Tag             =   "moist"
            Top             =   1440
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   9
            Left            =   4080
            TabIndex        =   6
            Tag             =   "porosity"
            Top             =   720
            Width           =   1000
         End
         Begin VB.Label Label 
            Caption         =   "Emanation factor - EMANATION"
            Height          =   264
            Index           =   10
            Left            =   120
            TabIndex        =   65
            Top             =   1080
            Width           =   3804
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   10
            Left            =   6120
            TabIndex        =   64
            Tag             =   "0"
            Top             =   1116
            Width           =   996
         End
         Begin VB.Label Label 
            Caption         =   "Darcy infiltration rate - DARCY"
            Height          =   264
            Index           =   7
            Left            =   120
            TabIndex        =   42
            Top             =   360
            Width           =   3804
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   7
            Left            =   6120
            TabIndex        =   41
            Tag             =   "0"
            Top             =   396
            Width           =   1008
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   12
            Left            =   6120
            TabIndex        =   18
            Tag             =   "0"
            Top             =   1836
            Width           =   1008
         End
         Begin VB.Label Label 
            Caption         =   "Water to Radon gas partition coefficient - WGPART"
            Height          =   252
            Index           =   12
            Left            =   120
            TabIndex        =   17
            Top             =   1800
            Width           =   3804
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   11
            Left            =   6120
            TabIndex        =   12
            Tag             =   "0"
            Top             =   1476
            Width           =   1008
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   9
            Left            =   6120
            TabIndex        =   8
            Tag             =   "0"
            Top             =   756
            Width           =   996
         End
         Begin VB.Label Label 
            Caption         =   "Volumetetric moisture content - MOIST"
            Height          =   264
            Index           =   11
            Left            =   120
            TabIndex        =   9
            Top             =   1440
            Width           =   3804
         End
         Begin VB.Label Label 
            Caption         =   "Total porosity - POROSITY"
            Height          =   264
            Index           =   9
            Left            =   120
            TabIndex        =   5
            Top             =   720
            Width           =   3804
         End
      End
      Begin Threed.SSFrame SSFrame1 
         Height          =   4020
         Index           =   2
         Left            =   -74760
         TabIndex        =   14
         Top             =   480
         Width           =   7212
         _Version        =   65536
         _ExtentX        =   12721
         _ExtentY        =   7091
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
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   18
            Left            =   4080
            TabIndex        =   60
            Tag             =   "raActivity"
            Top             =   720
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   18
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   59
            Tag             =   "Ci/g"
            Top             =   720
            Width           =   996
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   20
            Left            =   4080
            TabIndex        =   56
            Tag             =   "halfLife"
            Top             =   1440
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   20
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   55
            Tag             =   "sec"
            Top             =   1440
            Width           =   996
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   19
            Left            =   4080
            TabIndex        =   52
            Tag             =   "rdActivity"
            Top             =   1080
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   19
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   51
            Tag             =   "Ci/g"
            Top             =   1080
            Width           =   996
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   17
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   1
            Tag             =   "Ci"
            Top             =   360
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   17
            Left            =   4080
            TabIndex        =   0
            Tag             =   "radInv"
            Top             =   360
            Width           =   1000
         End
         Begin VB.Label Label 
            Caption         =   "Radium-226 specific activity - RAACTIVITY"
            Height          =   264
            Index           =   18
            Left            =   240
            TabIndex        =   62
            Top             =   720
            Width           =   3804
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   18
            Left            =   6120
            TabIndex        =   61
            Tag             =   "0"
            Top             =   816
            Width           =   1008
         End
         Begin VB.Label Label 
            Caption         =   "Radionuclide Half-life - HALFLIFE"
            Height          =   264
            Index           =   20
            Left            =   240
            TabIndex        =   58
            Top             =   1440
            Width           =   3804
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   20
            Left            =   6120
            TabIndex        =   57
            Tag             =   "0"
            Top             =   1536
            Width           =   1008
         End
         Begin VB.Label Label 
            Caption         =   "Radon -222 specific activity - RDACTIVITY"
            Height          =   264
            Index           =   19
            Left            =   240
            TabIndex        =   54
            Top             =   1080
            Width           =   3804
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   19
            Left            =   6120
            TabIndex        =   53
            Tag             =   "0"
            Top             =   1176
            Width           =   1008
         End
         Begin VB.Label Label 
            Caption         =   "Radium-226 inventory - RADINV"
            Height          =   264
            Index           =   17
            Left            =   240
            TabIndex        =   50
            Top             =   360
            Width           =   3804
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   17
            Left            =   6120
            TabIndex        =   2
            Tag             =   "0"
            Top             =   456
            Width           =   1008
         End
      End
      Begin Threed.SSFrame SSFrame1 
         Height          =   4020
         Index           =   0
         Left            =   240
         TabIndex        =   4
         Top             =   480
         Width           =   7212
         _Version        =   65536
         _ExtentX        =   12721
         _ExtentY        =   7091
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
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   0
            Left            =   4080
            TabIndex        =   45
            Tag             =   "tLength"
            Top             =   360
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   0
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   44
            Tag             =   "yr"
            Top             =   360
            Width           =   996
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   1
            Left            =   4080
            TabIndex        =   43
            Tag             =   "nTime"
            Top             =   720
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   6
            Left            =   4080
            TabIndex        =   36
            Tag             =   "surfThick"
            Top             =   3120
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   6
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   35
            Tag             =   "m"
            Top             =   3120
            Width           =   996
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   5
            Left            =   4080
            TabIndex        =   32
            Tag             =   "tableDist"
            Top             =   2760
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   5
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   31
            Tag             =   "m"
            Top             =   2760
            Width           =   996
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   2
            Left            =   4080
            TabIndex        =   24
            Tag             =   "srcThick"
            Top             =   1320
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   4
            Left            =   4080
            TabIndex        =   23
            Tag             =   "srcLength"
            Top             =   2040
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   2
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   22
            Tag             =   "m"
            Top             =   1320
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   4
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   21
            Tag             =   "m"
            Top             =   2040
            Width           =   996
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   3
            Left            =   4080
            TabIndex        =   20
            Tag             =   "srcWidth"
            Top             =   1680
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   3
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   19
            Tag             =   "m"
            Top             =   1680
            Width           =   996
         End
         Begin VB.Label Label 
            Caption         =   "Duration of simulation - TLENGTH"
            Height          =   264
            Index           =   0
            Left            =   120
            TabIndex        =   49
            Top             =   360
            Width           =   3804
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   0
            Left            =   6120
            TabIndex        =   48
            Tag             =   "0"
            Top             =   396
            Width           =   1008
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   1
            Left            =   6120
            TabIndex        =   47
            Tag             =   "0"
            Top             =   756
            Width           =   1008
         End
         Begin VB.Label Label 
            Caption         =   "Number of reported time points - NTIME"
            Height          =   264
            Index           =   1
            Left            =   120
            TabIndex        =   46
            Top             =   720
            Width           =   3804
         End
         Begin VB.Label Label 
            Caption         =   "Thickness of clean overburden - SURFTHICK"
            Height          =   372
            Index           =   6
            Left            =   120
            TabIndex        =   38
            Top             =   3120
            Width           =   3804
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   6
            Left            =   6120
            TabIndex        =   37
            Tag             =   "0"
            Top             =   3156
            Width           =   1008
         End
         Begin VB.Label Label 
            Caption         =   "Distance to water table ( or  desired location) - TABLEDIST"
            Height          =   372
            Index           =   5
            Left            =   120
            TabIndex        =   34
            Top             =   2760
            Width           =   3804
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   5
            Left            =   6120
            TabIndex        =   33
            Tag             =   "0"
            Top             =   2796
            Width           =   1008
         End
         Begin VB.Label Label 
            Caption         =   "Thickness of source layer - SRCTHICK"
            Height          =   264
            Index           =   2
            Left            =   120
            TabIndex        =   30
            Top             =   1320
            Width           =   3804
         End
         Begin VB.Label Label 
            Caption         =   "Length of source layer (travel direction) - SRCLENGTH"
            Height          =   372
            Index           =   4
            Left            =   120
            TabIndex        =   29
            Top             =   2040
            Width           =   3804
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   2
            Left            =   6120
            TabIndex        =   28
            Tag             =   "0"
            Top             =   1356
            Width           =   1008
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   4
            Left            =   6120
            TabIndex        =   27
            Tag             =   "0"
            Top             =   2076
            Width           =   1008
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   3
            Left            =   6120
            TabIndex        =   26
            Tag             =   "0"
            Top             =   1716
            Width           =   1008
         End
         Begin VB.Label Label 
            Caption         =   "Width of source layer -SRCWIDTH"
            Height          =   264
            Index           =   3
            Left            =   120
            TabIndex        =   25
            Top             =   1680
            Width           =   3804
         End
      End
      Begin VB.Timer Timer1 
         Interval        =   100
         Left            =   0
         Top             =   0
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
Attribute VB_Name = "Vadose"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text


Dim temp As parmrec
Dim loadng As Boolean
Dim oldParent As String
Dim oldProgeny As String
Dim uStr(HALF_LIFE) As String
Dim hStr(2) As String

'Added flags to control the behaviour of the added parms combo box
Dim PrevSelectedIdx As Long
Dim PrevSelectedProgIdx As Long

' All these just ro resolve indices from fui to mui
Dim f_numcon As Long

Private Sub about_Click()
  frmAbout.Show 1, Me
End Sub

Private Sub Form_Unload(Cancel As Integer)
  Dim answer As Long
  If Not called Then
    answer = MsgBox("Do you want to save changes?", 51, App.Title)
    If answer = 6 Then save_Click
    If answer = 7 Then
      EndModule
    End If
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

Sub selref_Click()
  RefMode = 0
  GetRef ref(RefItem)
End Sub

Sub addref_Click()
  RefMode = 1
  GetRef ref(RefItem)
End Sub

Private Sub fillet(idx As Long)
  ref(idx).Tag = temp.ref
  ref(idx).Caption = "Ref:" & Str(temp.ref)
  On Error Resume Next
  
  If temp.cunit <> "N/A" Then set_unit unit(idx), temp.uunit
  txt(idx).Text = convert(temp.cunit, temp.uunit, Val(temp.pval))
End Sub

Private Sub loadprm()
  Dim i As Long, j As Long, k As Long, m As Long
  Dim prm As String
  Dim prefix As String
  Dim cas As String
  Dim fle As parmfile
  Dim found As Boolean
  
  f_numcon = 0
  m_numcon = 0
  If open_parm(fle, FUIName, 2) Then
    Do Until EOCF(fle.file)
      If read_parmrec(fle, temp) Then
        Select Case temp.pname
          Case "fui"
            For m = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                If temp.idx1 = siteIdx Then
                  Select Case temp.pname
                    Case "vaddespath"
                      If temp.idx1 = siteIdx And temp.idx2 = modIdx Then DesName = temp.pval
                    Case "numcon"
                      f_numcon = Val(temp.pval)
                      ReDim Preserve con(f_numcon) As contam_param
                    Case "clktype"
                        If temp.idx2 > f_numcon Then
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
                          If temp.idx3 > con(temp.idx2).numprog Then
                            con(temp.idx2).numprog = temp.idx3
                            ReDim Preserve con(temp.idx2).progeny(temp.idx3) As contam_progeny_param
                          End If
                        End If
                    Case "fscname"
                        If temp.idx2 > f_numcon Then
                          f_numcon = temp.idx2
                          ReDim Preserve con(f_numcon) As contam_param
                        End If
                        If temp.idx3 = 0 Then
                          con(temp.idx2).name = temp.pval
                        Else
                          If temp.idx3 > con(temp.idx2).numprog Then
                            con(temp.idx2).numprog = temp.idx3
                            ReDim Preserve con(temp.idx2).progeny(temp.idx3) As contam_progeny_param
                          End If
                          con(temp.idx2).progeny(temp.idx3).name = temp.pval
                        End If
                    Case "fscasid"
                        If temp.idx2 > f_numcon Then
                          f_numcon = temp.idx2
                          ReDim Preserve con(f_numcon) As contam_param
                        End If
                        If temp.idx3 = 0 Then
                          con(temp.idx2).cas = temp.pval
                        Else
                          If temp.idx3 > con(temp.idx2).numprog Then
                            con(temp.idx2).numprog = temp.idx3
                            ReDim Preserve con(temp.idx2).progeny(temp.idx3) As contam_progeny_param
                          End If
                          con(temp.idx2).progeny(temp.idx3).cas = temp.pval
                        End If
                 End Select
                End If
              End If
            Next
            
          Case ModName
            Loading.Gauge1.Max = Val(temp.idx1)
            Loading.Gauge1.value = 0
            For m = 1 To temp.idx1
              Loading.update
              If read_parmrec(fle, temp) Then
                Select Case temp.pname
                  Case "CVTFormat":       CVTFormat = "General Number" 'CVTFormat = temp.pval
                  Case "tLength":         fillet 0
                  Case "ntime":           fillet 1
                  Case "srcThick":        fillet 2
                  Case "srcWidth":        fillet 3
                  Case "srcLength":       fillet 4
                  Case "tableDist":       fillet 5
                  Case "surfThick":       fillet 6
                  
                  Case "darcy":           fillet 7
                  Case "bulkDen":         fillet 8
                  Case "porosity":        fillet 9
                  Case "emanation":       fillet 10
                  Case "moist":           fillet 11
                  Case "wgPart":          fillet 12
                  
                  Case "radInv":          fillet 17
                  Case "raActivity":      fillet 18
                  Case "rdActivity":      fillet 19
                  Case "halfLife":        fillet 20
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
Dim i As Long
  
  StartModule Me, App.Title, 5
  SetHelpFile App.Path + "\rqvad.htm"
  SetRefFile ReplaceExt(FUIName, "ref")
  'set conversion comboboxes
  For i = 0 To 20
    If i = 1 Then i = 2
    If i = 8 Then i = 9
    If i = 10 Then i = 11
    If i = 12 Then i = 17
    get_conversion_items unit(i).Tag, unit(i)
  Next
  Loading.Show
  loadng = True
  loadprm
  loadng = False
  Unload Loading
End Sub

Private Sub save_Click()
  Dim i As Long
  Dim j As Long
  Dim fname As String
  Dim parm As parmrec
  Dim fle As parmfile
  Dim UserFormat As String
  UserFormat = CVTFormat

  fname = RunName & ".GID"
  If open_parm(fle, fname, 1) Then
    set_parm parm, "CVTFormat", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CVTFormat
    write_parmrec fle, parm
    CVTFormat = "General Number"
    
    For i = 0 To 20
      Select Case i
        Case 8, 13, 14, 15, 16
        Case 1, 10, 12
          If er(i) Then PutError "Parameter " & txt(i).Tag & " is invalid"
          set_parm parm, txt(i).Tag, 0, 0, 0, 0, 0, 0, ref(i).Tag, "N/A", "N/A", txt(i).Text
        Case Else
          If er(i) Then PutError "Parameter " & txt(i).Tag & " is invalid"
          set_parm parm, txt(i).Tag, 0, 0, 0, 0, 0, 0, ref(i).Tag, unit(i).Text, unit(i).Tag, convert(unit(i).Text, unit(i).Tag, Val(txt(i)))
      End Select
      write_parmrec fle, parm
    Next
    
    close_parm fle
    CVTFormat = UserFormat
    SetFormat Me
  Else
    PutError "Unable to create transaction file" & RunName & ".GID"
  End If
  EndModule
End Sub

Private Sub SSTab1_Click(PreviousTab As Integer)
  
  SSFrame1(PreviousTab).Enabled = False
  SSFrame1(SSTab1.Tab).Enabled = True
  noact.Enabled = False
  mes = ""
End Sub

Private Sub Timer1_Timer()
  mes = Mid(mes, 2) & Mid(mes, 1, 1)
End Sub

Private Sub unit_Click(index As Integer)
  er CLng(index)
End Sub

Private Function er(index As Long) As Boolean
  Dim tval As Double
  Dim t1 As String
  Dim t2 As String
  Dim m As String
  
  m = ""
  er = False
  If txt(index).Text = "" Then er = True
  tval = Val(txt(index).Text)
  Select Case index
    Case 0, 1, 2, 3, 4, 8, 18, 19, 20
        m = "Value must be greater than 0"
        If tval <= 0 Then er = True
    Case 10
      m = "Value must be between 0 and 1 inclusive"
      If (tval < 0 Or tval > 1) Then er = True
    Case 1, 5, 6, 7, 12, 17
        m = "Value must be greater than or equal to 0"
        If tval < 0 Then er = True
    Case 9, 11
      t1 = convert(unit(index).Tag, unit(index).Text, 0)
      t2 = convert(unit(index).Tag, unit(index).Text, 1)
      m = "Value must be between " + t1 + " and " + t2 + " inclusive"
      If (tval < t1 Or tval > t2) Then er = True
  End Select
  mes = Space(140 - Len(m)) & m
  If er Then
    txt(index).BackColor = &H8080FF
  Else
    txt(index).BackColor = &HC0FFC0
  End If
End Function

Private Sub txt_Change(index As Integer)
Dim chk As Double
On Error GoTo toolarge
  er CLng(index)
  chk = CDbl(txt(index).Text)
  Exit Sub
toolarge:
  txt(index).BackColor = &H8080FF
End Sub

Private Sub txt_GotFocus(index As Integer)
  er CLng(index)
  RefItem = index
  noact.Enabled = True
  HelpAnchor = txt(index).Tag
End Sub

Private Sub unit_GotFocus(index As Integer)
  er CLng(index)
  RefItem = index
  noact.Enabled = True
  HelpAnchor = txt(index).Tag
End Sub

Private Sub SSTab1_GotFocus()
  mes = ""
  noact.Enabled = False
  Select Case SSTab1.Tab
  Case 0: HelpAnchor = "DIMENSIONS"
  Case 1: HelpAnchor = "CHARACTERISTICS"
  Case 2: HelpAnchor = "PROPERTIES"
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

