VERSION 5.00
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "tabctl32.ocx"
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "threed32.ocx"
Begin VB.Form Impacts 
   BorderStyle     =   1  'Fixed Single
   ClientHeight    =   5628
   ClientLeft      =   9120
   ClientTop       =   636
   ClientWidth     =   7692
   Icon            =   "Impacts.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form2"
   MaxButton       =   0   'False
   NegotiateMenus  =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   469
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   641
   StartUpPosition =   2  'CenterScreen
   Begin TabDlg.SSTab SSTab1 
      Height          =   5172
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   7692
      _ExtentX        =   13568
      _ExtentY        =   9123
      _Version        =   393216
      Style           =   1
      Tabs            =   2
      TabsPerRow      =   2
      TabHeight       =   529
      TabCaption(0)   =   "Chemical"
      TabPicture(0)   =   "Impacts.frx":030A
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "frm(0)"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).Control(1)=   "Timer1"
      Tab(0).Control(1).Enabled=   0   'False
      Tab(0).ControlCount=   2
      TabCaption(1)   =   "Radionuclide"
      TabPicture(1)   =   "Impacts.frx":0326
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "frm(1)"
      Tab(1).ControlCount=   1
      Begin VB.Timer Timer1 
         Interval        =   100
         Left            =   0
         Top             =   0
      End
      Begin Threed.SSFrame frm 
         Height          =   4452
         Index           =   1
         Left            =   -74760
         TabIndex        =   34
         Top             =   468
         Width           =   7212
         _Version        =   65536
         _ExtentX        =   12721
         _ExtentY        =   7853
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
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   7
            Left            =   3960
            TabIndex        =   42
            Tag             =   "RADLIM"
            Text            =   "0.0"
            Top             =   840
            Width           =   1000
         End
         Begin Threed.SSOption SSOption1 
            Height          =   252
            Index           =   0
            Left            =   240
            TabIndex        =   36
            Tag             =   "HEINC"
            Top             =   2040
            Width           =   4608
            _Version        =   65536
            _ExtentX        =   8123
            _ExtentY        =   450
            _StockProps     =   78
            Caption         =   "Calculate lifetime cancer incidence -- HE-INC"
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Value           =   -1  'True
         End
         Begin VB.ComboBox Combo1 
            Height          =   288
            ItemData        =   "Impacts.frx":0342
            Left            =   3720
            List            =   "Impacts.frx":034C
            Style           =   2  'Dropdown List
            TabIndex        =   25
            Tag             =   "IHEAST"
            Top             =   360
            Width           =   3372
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   5
            Left            =   3960
            TabIndex        =   31
            Tag             =   "DSOIL"
            Top             =   1560
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   5
            Left            =   4920
            Style           =   2  'Dropdown List
            TabIndex        =   32
            Tag             =   "g/cm^3"
            Top             =   1560
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   4
            Left            =   3960
            TabIndex        =   27
            Tag             =   "TSOIL"
            Top             =   1200
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   4
            Left            =   4920
            Style           =   2  'Dropdown List
            TabIndex        =   28
            Tag             =   "m"
            Top             =   1200
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Enabled         =   0   'False
            Height          =   288
            Index           =   2
            Left            =   4920
            Style           =   2  'Dropdown List
            TabIndex        =   21
            Tag             =   "risk/Sv"
            Top             =   3480
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Enabled         =   0   'False
            Height          =   288
            Index           =   1
            Left            =   4920
            Style           =   2  'Dropdown List
            TabIndex        =   17
            Tag             =   "risk/Sv"
            Top             =   2880
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   0
            Left            =   4920
            Style           =   2  'Dropdown List
            TabIndex        =   13
            Tag             =   "risk/Sv"
            Top             =   2280
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Enabled         =   0   'False
            Height          =   312
            Index           =   2
            Left            =   3960
            TabIndex        =   20
            Tag             =   "HECONFSH"
            Top             =   3480
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Enabled         =   0   'False
            Height          =   312
            Index           =   1
            Left            =   3960
            TabIndex        =   16
            Tag             =   "HECONFAT"
            Top             =   2880
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   0
            Left            =   3960
            TabIndex        =   12
            Tag             =   "HECONINC"
            Top             =   2280
            Width           =   1000
         End
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   3
            Left            =   240
            TabIndex        =   23
            Tag             =   "HECEDE"
            Top             =   3960
            Width           =   5004
            _Version        =   65536
            _ExtentX        =   8819
            _ExtentY        =   444
            _StockProps     =   78
            Caption         =   "Calculate radiation dose commitment (CEDE) -- HE-CEDE"
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
         End
         Begin Threed.SSOption SSOption1 
            Height          =   252
            Index           =   1
            Left            =   240
            TabIndex        =   37
            Tag             =   "HEFAT"
            Top             =   2640
            Width           =   4608
            _Version        =   65536
            _ExtentX        =   8123
            _ExtentY        =   450
            _StockProps     =   78
            Caption         =   "Calculate cancer fatalities -- HE-FAT"
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
         End
         Begin Threed.SSOption SSOption1 
            Height          =   252
            Index           =   2
            Left            =   240
            TabIndex        =   38
            Tag             =   "HEFSH"
            Top             =   3240
            Width           =   5652
            _Version        =   65536
            _ExtentX        =   9975
            _ExtentY        =   450
            _StockProps     =   78
            Caption         =   "Calculate lifetime cancer and severe hereditary effects -- HE-FSHH"
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   7
            Left            =   6000
            TabIndex        =   44
            Tag             =   "0"
            Top             =   840
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Cancer risk threshold limit -- RADLIM"
            Height          =   252
            Index           =   7
            Left            =   240
            TabIndex        =   43
            Tag             =   "radlim"
            Top             =   840
            Width           =   3012
         End
         Begin VB.Label Label1 
            Caption         =   "Cancer risk evaluation method -- IHEAST"
            Height          =   252
            Left            =   240
            TabIndex        =   24
            Tag             =   "IHEAST"
            Top             =   360
            Width           =   3252
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   5
            Left            =   6000
            TabIndex        =   33
            Tag             =   "0"
            Top             =   1560
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Density of contaminated soil/sediment layer -- DSOIL"
            Height          =   372
            Index           =   5
            Left            =   240
            TabIndex        =   30
            Tag             =   "DSOIL"
            Top             =   1560
            Width           =   3492
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   4
            Left            =   6000
            TabIndex        =   29
            Tag             =   "0"
            Top             =   1200
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Thickness of contaminated soil/sediment layer -- TSOIL"
            Height          =   372
            Index           =   4
            Left            =   240
            TabIndex        =   26
            Tag             =   "TSOIL"
            Top             =   1200
            Width           =   3492
         End
         Begin VB.Label lbl 
            Caption         =   "Conversion factor -- HE-CONINC"
            Height          =   252
            Index           =   0
            Left            =   1200
            TabIndex        =   11
            Tag             =   "HECONINC"
            Top             =   2280
            Width           =   2652
         End
         Begin VB.Label lbl 
            Caption         =   "Conversion factor -- HE-CONFAT"
            Height          =   252
            Index           =   1
            Left            =   1200
            TabIndex        =   15
            Tag             =   "HECONFAT"
            Top             =   2880
            Width           =   2652
         End
         Begin VB.Label lbl 
            Caption         =   "Conversion factor -- HE-CONFSH"
            Height          =   252
            Index           =   2
            Left            =   1200
            TabIndex        =   19
            Tag             =   "HECONFSH"
            Top             =   3480
            Width           =   2652
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   2
            Left            =   6000
            TabIndex        =   22
            Tag             =   "0"
            Top             =   3480
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   1
            Left            =   6000
            TabIndex        =   18
            Tag             =   "0"
            Top             =   2880
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   0
            Left            =   6000
            TabIndex        =   14
            Tag             =   "0"
            Top             =   2280
            Width           =   996
         End
      End
      Begin Threed.SSFrame frm 
         Height          =   4452
         Index           =   0
         Left            =   240
         TabIndex        =   10
         Top             =   480
         Width           =   7212
         _Version        =   65536
         _ExtentX        =   12721
         _ExtentY        =   7853
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
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   6
            Left            =   4080
            TabIndex        =   39
            Tag             =   "CHEMLIM"
            Text            =   "0.0"
            Top             =   720
            Width           =   1000
         End
         Begin VB.ComboBox inhale 
            BackColor       =   &H00FFFFFF&
            Height          =   315
            ItemData        =   "Impacts.frx":0392
            Left            =   4200
            List            =   "Impacts.frx":039C
            Style           =   2  'Dropdown List
            TabIndex        =   9
            Tag             =   "inhale"
            Top             =   3120
            Width           =   1955
         End
         Begin VB.OptionButton Option1 
            Caption         =   "Use any available toxicity value -- CETOXT"
            Height          =   252
            Index           =   1
            Left            =   240
            TabIndex        =   7
            Tag             =   "CETOXT"
            Top             =   2280
            Value           =   -1  'True
            Visible         =   0   'False
            Width           =   6204
         End
         Begin VB.OptionButton Option1 
            Caption         =   "Use only toxicity values from EPA's IRIS or HEAST sources -- CETOXT"
            Height          =   252
            Index           =   0
            Left            =   240
            TabIndex        =   6
            Tag             =   "CETOXT"
            Top             =   1920
            Visible         =   0   'False
            Width           =   5844
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   3
            Left            =   4080
            TabIndex        =   4
            Tag             =   "RFDLIM"
            Text            =   "0.0"
            Top             =   1440
            Width           =   1000
         End
         Begin Threed.SSCheck SSCheck2 
            Height          =   252
            Index           =   1
            Left            =   240
            TabIndex        =   2
            Tag             =   "CHEMHI"
            Top             =   1080
            Width           =   5172
            _Version        =   65536
            _ExtentX        =   9123
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "Calculate hazard index -- CHEMHI"
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Value           =   -1  'True
         End
         Begin Threed.SSCheck SSCheck2 
            Height          =   255
            Index           =   0
            Left            =   255
            TabIndex        =   1
            Tag             =   "CHEMRISK"
            Top             =   360
            Width           =   5175
            _Version        =   65536
            _ExtentX        =   9123
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "Calculate lifetime cancer incidence -- CHEMRISK"
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Value           =   -1  'True
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   6
            Left            =   5160
            TabIndex        =   41
            Tag             =   "0"
            Top             =   720
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Cancer incidence threshold limit -- CHEMLIM"
            Height          =   252
            Index           =   6
            Left            =   600
            TabIndex        =   40
            Tag             =   "chemlim"
            Top             =   720
            Width           =   3492
         End
         Begin VB.Label Label2 
            Caption         =   "Method for inhalation impact analysis -- INHALE"
            Height          =   255
            Left            =   240
            TabIndex        =   8
            Tag             =   "HEINHAL"
            Top             =   3120
            Width           =   3795
         End
         Begin VB.Label lbl 
            Caption         =   "Hazard quotient threshold limit -- RFDLIM"
            Height          =   252
            Index           =   3
            Left            =   600
            TabIndex        =   3
            Tag             =   "rfdlim"
            Top             =   1440
            Width           =   3492
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   3
            Left            =   5160
            TabIndex        =   5
            Tag             =   "0"
            Top             =   1440
            Width           =   996
         End
      End
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
      TabIndex        =   35
      TabStop         =   0   'False
      Top             =   5160
      Width           =   7680
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
      End
      Begin VB.Menu about 
         Caption         =   "&About"
      End
   End
End
Attribute VB_Name = "Impacts"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text
Dim havechem As Boolean
Dim haverad As Boolean
Dim temp As parmrec

Private Sub about_Click()
  frmAbout.Show 1, Impacts
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

Private Sub fillet(idx As Long)
  ref(idx).Tag = temp.ref
  ref(idx).Caption = "Ref:" & Str(temp.ref)
  On Error Resume Next
  If temp.cunit <> "N/A" Then set_unit unit(idx), temp.uunit
  txt(idx).Text = convert(temp.cunit, temp.uunit, Val(temp.pval))
End Sub

Private Sub loadprm()
  Dim m As Long
  Dim i As Integer
  Dim fle As parmfile
  Dim sval As Boolean

  havechem = False
  haverad = False
  
  If open_parm(fle, FUIName, 2) Then
    Do Until EOCF(fle.file)
      If read_parmrec(fle, temp) Then
        Loading.Gauge1.Max = Val(temp.idx1)
        Loading.Gauge1.Value = 0
        Select Case temp.pName
          Case "fui"
            For m = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                If temp.idx1 = siteIdx Then
                  If temp.pName = "heidespath" Then DesName = temp.pval
                  Select Case temp.pName
                    Case "clktype"
                      If temp.pval = 0 Then havechem = True
                      If temp.pval = 1 Then haverad = True
                  End Select
                End If
              End If
            Next
          
          Case modName
            For m = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                Loading.update
                Select Case temp.pName
                  Case "CVTFormat":    CVTFormat = "General Number" 'CVTFormat = temp.pval
                  Case "CHEMRISK"
                    i = False
                    If temp.pval = "true" Then i = True
                    SSCheck2(0) = i
                    SSCheck2_Click 0, i
                  Case "CHEMHI"
                    i = False
                    If temp.pval = "true" Then i = True
                    SSCheck2(1) = i
                    SSCheck2_Click 1, i
                  Case "HEINC"
                    i = False
                    If temp.pval = "true" Then i = True
                    SSOption1(0).Value = i
                    SSOption1_Click 0, i
                  Case "HEFAT"
                    i = False
                    If temp.pval = "true" Then i = True
                    SSOption1(1).Value = i
                    SSOption1_Click 1, i
                  Case "HEFSH"
                    i = False
                    If temp.pval = "true" Then i = True
                    SSOption1(2).Value = i
                    SSOption1_Click 2, i
                  Case "HECEDE"
                    i = False
                    If temp.pval = "true" Then i = True
                    SSCheck1(3) = i
                    SSCheck1_Click 3, i
                  Case "HECONINC":    fillet 0
                  Case "HECONFAT":    fillet 1
                  Case "HECONFSH":    fillet 2
                  Case "RFDLIM":      fillet 3
                  Case "TSOIL":       fillet 4
                  Case "DSOIL":       fillet 5
                  Case "CHEMLIM":     fillet 6
                  Case "RADLIM":      fillet 7
                  Case "IHEAST":    Combo1.ListIndex = Val(temp.pval)
                  Case "inhale":    inhale.ListIndex = Val(temp.pval)
                  Case "CETOX"
                    sval = False
                    If temp.pval = "true" Then sval = True
                    Option1(0).Value = sval
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
  
  StartModule Impacts, App.Title, 5
  SetHelpFile App.Path + "\hei.htm"
  SetRefFile ReplaceExt(FUIName, "ref")
  For i = 0 To 5
    If i = 3 Then i = 4
    get_conversion_items unit(i).Tag, unit(i)
  Next
  Combo1.ListIndex = 0
  inhale.ListIndex = 0
  Loading.Show
  loadprm
  Unload Loading
  
  SSTab1.TabVisible(0) = havechem
  frm(0).Visible = havechem
  SSTab1.TabVisible(1) = haverad
  frm(1).Visible = haverad
End Sub

'Private Sub meta_Click()
'
'  If opendes(App.Path + "\hazhei.des") Then
'
'    put_val des, "mf"
'    put_val des, "version 2.0 beta"
'    put_line des
'    put_val des, "Health Impacts"
'    put_val des, "Mepas 4.0 Chronic Health Impacts Module"
'    put_val des, "hei4.exe"
'    put_val des, "hazhei.bat"
'    put_line des
'    des.putbuff = """MEPAS 4.0 Chronic Health Impact Module" + Chr(13) + Chr(10) + Chr(13) + Chr(10) + _
'          "The MEPAS chronic health impact module calculates" + Chr(13) + Chr(10) + _
'          "health impacts from intake or exposure to chemicals" + Chr(13) + Chr(10) + _
'          "or radionuclides.  Chemical impacts are evaluated" + Chr(13) + Chr(10) + _
'          "for inhalation, ingestion, or dermal contact" + Chr(13) + Chr(10) + _
'          "pathways as either cancer incidence or hazard index," + Chr(13) + Chr(10) + _
'          "as appropriate for the chemical of concern." + Chr(13) + Chr(10) + _
'          "Radionuclide health impacts may be reported as" + Chr(13) + Chr(10)
'
'    des.putbuff = des.putbuff + _
'          "radiation dose, cancer incidence, fatal cancer" + Chr(13) + Chr(10) + _
'          "incidence, or cancer plus severe hereditary effects" + Chr(13) + Chr(10) + _
'          "incidence. Radiation risk calculations can be based" + Chr(13) + Chr(10) + _
'          "on ICRP dosimetry and health effects conversion" + Chr(13) + Chr(10) + _
'          "factors (user defined), or on EPA/HEAST radionuclide" + Chr(13) + Chr(10) + _
'          "slope factors.  The module includes consideration of" + Chr(13) + Chr(10) + _
'          "domestic water use, farm product consumption," + Chr(13) + Chr(10) + _
'          "aquatic food consumption, surface water recreational" + Chr(13) + Chr(10)
'
'    des.putbuff = des.putbuff + _
'          "activities, soil contact exposure, and air exposures." + Chr(13) + Chr(10) + _
'          "Both chemical and radioactive pollutants may be" + Chr(13) + Chr(10) + _
'          "evaluated." + Chr(13) + Chr(10) + Chr(13) + Chr(10)
'
'    des.putbuff = des.putbuff + _
'         "Limitations:  None known at this time." + Chr(13) + Chr(10) + Chr(13) + Chr(10)
'
'    des.putbuff = des.putbuff + _
'         "Reference:" + Chr(13) + Chr(10) + _
'         "    Strenge, D.L., and P.J. Chamberlain.  1995." + Chr(13) + Chr(10) + _
'         "    Multimedia Environmental Pollutant Assessment System (MEPAS):" + Chr(13) + Chr(10) + _
'         "    Exposure Pathway and Human Health Impact Assessment Models." + Chr(13) + Chr(10) + _
'         "    PNL-10523.  Pacific Northwest Laboratory, Richland, WA." + Chr(13) + Chr(10) + Chr(13) + Chr(10)
'
'    des.putbuff = des.putbuff + _
'         "Point of Contact:" + Chr(13) + Chr(10) + _
'         "    Pacific Northwest National Laboratory" + Chr(13) + Chr(10) + _
'         "    Mitch Pelton" + Chr(13) + Chr(10) + _
'         "    P.O. Box 999 MS K6-80" + Chr(13) + Chr(10) + _
'         "    Richland WA 99352" + Chr(13) + Chr(10) + _
'         "    Phone (509)-376-3858" + Chr(13) + Chr(10) + _
'         "    EMail mitch.pelton@pnl.gov"""
'    put_line des
'
'    put_val des, 2
'    put_line des
'
'    putfile RIF, 1
'    putfile HIF, 2
'
'' count entries below and p[lace that number here
'    put_val des, 12
'    put_line des
'    putmeta UCase(SSCheck1(0).Tag), "NOT STOCHASTIC", "", SSCheck1(0).Caption, 0
'    putmeta UCase(SSCheck1(1).Tag), "NOT STOCHASTIC", "", SSCheck1(1).Caption, 0
'    putmeta UCase(SSCheck1(2).Tag), "NOT STOCHASTIC", "", SSCheck1(2).Caption, 0
'    putmeta UCase(SSCheck1(3).Tag), "NOT STOCHASTIC", "", SSCheck1(3).Caption, 0
'    putmeta UCase(SSCheck2(0).Tag), "NOT STOCHASTIC", "", SSCheck2(0).Caption, 0
'    putmeta UCase(SSCheck2(1).Tag), "NOT STOCHASTIC", "", SSCheck2(1).Caption, 0
'
'    putmeta UCase(txt(0).Tag), "CONTINUOUS", unit(0).Tag, lbl(0).Caption, 0, 0.0001, 1
'    putmeta UCase(txt(1).Tag), "CONTINUOUS", unit(1).Tag, lbl(1).Caption, 0, 0.0001, 1
'    putmeta UCase(txt(2).Tag), "CONTINUOUS", unit(2).Tag, lbl(2).Caption, 0, 0.0001, 1
'    putmeta UCase(txt(3).Tag), "CONTINUOUS", "", lbl(3).Caption, 0, 0, 100
'    putmeta UCase(txt(4).Tag), "CONTINUOUS", unit(4).Tag, lbl(4).Caption, 0, 0.001, 5
'    putmeta UCase(txt(5).Tag), "CONTINUOUS", unit(5).Tag, lbl(5).Caption, 0, 1, 4
'    closedes
'  Else
'    MsgBox "Unable to create description file"
'  End If
'
'End Sub

Private Sub save_Click()
  Dim fName As String
  Dim parm As parmrec
  Dim fle As parmfile
  Dim i As Long
  Dim UserFormat As String
  UserFormat = CVTFormat
  Dim ctl As Control
  
  fName = RunName + ".GID"
  If open_parm(fle, fName, 1) Then
    set_parm parm, "CVTFormat", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CVTFormat
    write_parmrec fle, parm
    CVTFormat = "General Number"
    
    'radionuclide tab
    If SSTab1.TabVisible(1) Then
      set_parm parm, Combo1.Tag, 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CStr(Combo1.ListIndex)
      write_parmrec fle, parm
      If er(7) Then PutError "Parameter " & txt(7).Tag & " is invalid"
      set_parm parm, txt(7).Tag, 0, 0, 0, 0, 0, 0, ref(7).Tag, "N/A", "N/A", txt(7).Text
      write_parmrec fle, parm
      If Combo1.ListIndex = 0 Then 'icrp
        For i = 0 To 3
          If i < 3 Then
            Set ctl = SSOption1(i)
          Else
            Set ctl = SSCheck1(i)
          End If
          If ctl.Value Then
            set_parm parm, ctl.Tag, 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", ctl.Value
            write_parmrec fle, parm
            If i < 3 Then
              If er(i) Then PutError "Parameter " & txt(i).Tag & " is invalid"
              set_parm parm, txt(i).Tag, 0, 0, 0, 0, 0, 0, ref(i).Tag, unit(i).Text, unit(i).Tag, convert(unit(i).Text, unit(i).Tag, Val(txt(i)))
              write_parmrec fle, parm
            End If
          End If
        Next
        For i = 0 To 3
          If i < 3 Then
            Set ctl = SSOption1(i)
          Else
            Set ctl = SSCheck1(i)
          End If
          If Not ctl.Value Then
            set_parm parm, ctl.Tag, 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", ctl.Value
            write_parmrec fle, parm
          End If
        Next
        If er(4) Then PutError "Parameter " & txt(4).Tag & " is invalid"
        If er(5) Then PutError "Parameter " & txt(5).Tag & " is invalid"
        set_parm parm, txt(4).Tag, 0, 0, 0, 0, 0, 0, ref(4).Tag, unit(4).Text, unit(4).Tag, convert(unit(4).Text, unit(4).Tag, Val(txt(4)))
        write_parmrec fle, parm
        set_parm parm, txt(5).Tag, 0, 0, 0, 0, 0, 0, ref(5).Tag, unit(5).Text, unit(5).Tag, convert(unit(5).Text, unit(5).Tag, Val(txt(5)))
        write_parmrec fle, parm
      End If
    End If
    
    'chemical tab
    If SSTab1.TabVisible(0) Then
      For i = 0 To 1
        set_parm parm, SSCheck2(i).Tag, 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", SSCheck2(i).Value
        write_parmrec fle, parm
        If SSCheck2(i).Value And i = 1 Then
          If er(3) Then PutError "Parameter " & txt(3).Tag & " is invalid"
          set_parm parm, txt(3).Tag, 0, 0, 0, 0, 0, 0, ref(3).Tag, "N/A", "N/A", txt(3).Text
          write_parmrec fle, parm
        End If
        If SSCheck2(i).Value And i = 0 Then
          If er(6) Then PutError "Parameter " & txt(6).Tag & " is invalid"
          set_parm parm, txt(6).Tag, 0, 0, 0, 0, 0, 0, ref(6).Tag, "N/A", "N/A", txt(6).Text
          write_parmrec fle, parm
        End If
      Next
      set_parm parm, Option1(0).Tag, 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", Option1(0).Value
      write_parmrec fle, parm
      set_parm parm, "INHALE", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CStr(inhale.ItemData(inhale.ListIndex))
      write_parmrec fle, parm
    End If
        
    close_parm fle
    CVTFormat = UserFormat
    SetFormat Me
  Else
    PutError "Unable to create transaction file" & RunName & ".GID"
  End If
  EndModule
End Sub

Private Sub Combo1_Click()
Dim i As Long
Dim st As Boolean
  st = False
  If Combo1.ListIndex = 0 Then
    st = True
  End If
  If Combo1.ListIndex = 1 Then
    SSOption1(0).Value = True
    SSOption1(1).Value = False
    SSOption1(2).Value = False
    SSCheck1(3).Value = True
  End If
  For i = 0 To 5
    If i < 3 Then SSOption1(i).Visible = st
    If i = 3 Then
      SSCheck1(i).Value = st
      SSCheck1(i).Visible = st
    End If
    If i = 3 Then i = 4
    lbl(i).Visible = st
    txt(i).Visible = st
    unit(i).Visible = st
    ref(i).Visible = st
    lbl(i).Enabled = st
    txt(i).Enabled = st
    unit(i).Enabled = st
    ref(i).Enabled = st
  Next
  Combo1_GotFocus
End Sub

Private Sub Option1_Click(Index As Integer)
  Option1_GotFocus Index
End Sub

Private Sub SSCheck1_Click(Index As Integer, Value As Integer)
  SSCheck1_GotFocus Index
End Sub

Private Sub SSCheck2_Click(Index As Integer, Value As Integer)
Dim i As Long
Dim chk As Boolean
  chk = False
  For i = 0 To 1
    If SSCheck2(i) Then chk = True
  Next
  If Not Value And Not chk Then
    SSCheck2(Index) = True
  End If
  txt(3).Enabled = SSCheck2(1).Value
  ref(3).Enabled = SSCheck2(1).Value
  lbl(3).Enabled = SSCheck2(1).Value
  txt(6).Enabled = SSCheck2(0).Value
  ref(6).Enabled = SSCheck2(0).Value
  lbl(6).Enabled = SSCheck2(0).Value
End Sub

Private Sub SSOption1_Click(Index As Integer, Value As Integer)
Dim i As Long
Dim chk As Boolean
  Select Case Index
    Case 0:
'      Combo1.Enabled = SSOption1(Index).Value = True
    Case 1, 2:
      If SSOption1(Index).Value = True Then
        Combo1.ListIndex = 0
        Combo1_Click
'        Combo1.Enabled = False
      End If
  End Select
  For i = SSOption1.LBound To SSOption1.UBound
    txt(i).Enabled = SSOption1(i).Value
    unit(i).Enabled = SSOption1(i).Value
    lbl(i).Enabled = SSOption1(i).Value
    ref(i).Enabled = SSOption1(i).Value
    txt(i).Visible = SSOption1(i).Value
    unit(i).Visible = SSOption1(i).Value
    lbl(i).Visible = SSOption1(i).Value
    ref(i).Visible = SSOption1(i).Value
  Next
  SSOption1_GotFocus Index
End Sub

Private Sub SSOption1_GotFocus(Index As Integer)
Dim m As String
  If SSOption1(Index).Value = True And (Index = 1 Or Index = 2) Then
    m = "Must use ICRP dose and risk factors for cancer risk"
  Else
    m = SSOption1(Index).Caption & ", click to enable"
  End If
  mes = Space(140 - Len(m)) & m
  noact.Enabled = False
  HelpAnchor = SSOption1(Index).Tag
End Sub

Private Sub Timer1_Timer()
  mes = Mid(mes, 2) & Mid(mes, 1, 1)
End Sub

Private Sub unit_Click(Index As Integer)
  er CLng(Index)
End Sub

Private Sub SSTab1_Click(PreviousTab As Integer)
  frm(PreviousTab).Enabled = False
  frm(SSTab1.Tab).Enabled = True
End Sub

Private Function er(Index As Long) As Boolean
  Dim tval As Double
  Dim t1 As String
  Dim t2 As String
  Dim m As String
  
  m = ""
  er = False
  If txt(Index).Text = "" Then er = True
  tval = Val(txt(Index).Text)
  Select Case Index
    Case 3
      m = "Value must be between 0 to 100"
      If (tval < 0 Or tval > 100) Then er = True
    Case 6
      m = "Value must be between 0 to 1"
      If (tval < 0 Or tval > 1) Then er = True
    Case 7
      m = "Value must be between 0 to 1"
      If (tval < 0 Or tval > 1) Then er = True
    Case 0, 1, 2
      t1 = convert(unit(Index).Tag, unit(Index).Text, 0.0001)
      t2 = convert(unit(Index).Tag, unit(Index).Text, 1)
      m = "Value must be between " + t1 + " and " + t2 + " " + unit(Index).Text
      If (tval < Val(t1) Or tval > Val(t2)) Then er = True
    Case 4
      t1 = convert(unit(Index).Tag, unit(Index).Text, 0.001)
      t2 = convert(unit(Index).Tag, unit(Index).Text, 5)
      m = "Value must be between " + t1 + " and " + t2 + " " + unit(Index).Text
      If (tval < Val(t1) Or tval > Val(t2)) Then er = True
    Case 5
      t1 = convert(unit(Index).Tag, unit(Index).Text, 1)
      t2 = convert(unit(Index).Tag, unit(Index).Text, 4)
      m = "Value must be between " + t1 + " and " + t2 + " " + unit(Index).Text
      If (tval < Val(t1) Or tval > Val(t2)) Then er = True
  End Select
  mes = Space(140 - Len(m)) & m
  If er Then
    txt(Index).BackColor = &H8080FF
  Else
    txt(Index).BackColor = &HC0FFC0
  End If
End Function

Private Sub txt_Change(Index As Integer)
Dim chk As Double
On Error GoTo toolarge
  er CLng(Index)
  chk = CDbl(txt(Index).Text)
  Exit Sub
toolarge:
  txt(Index).BackColor = &H8080FF
End Sub

Private Sub Combo1_GotFocus()
Dim m As String
  If Combo1.ListIndex = 1 Then
    m = "Cannot calculate fatalities or hereditary effects using EPA HEAST factors"
  Else
    m = "Select a cancer risk evaluation method"
  End If
  mes = Space(140 - Len(m)) & m
  noact.Enabled = False
  HelpAnchor = Combo1.Tag
End Sub

Private Sub SSTab1_GotFocus()
  mes = ""
  noact.Enabled = False
  If SSTab1.Tab = 0 Then
    HelpAnchor = "Chemical"
  Else
    HelpAnchor = "Radionuclide"
  End If
End Sub

Private Sub txt_GotFocus(Index As Integer)
  er CLng(Index)
  RefItem = Index
  noact.Enabled = True
  HelpAnchor = lbl(Index).Tag
End Sub

Private Sub unit_GotFocus(Index As Integer)
  er CLng(Index)
  RefItem = Index
  noact.Enabled = True
  HelpAnchor = lbl(Index).Tag
End Sub

Private Sub inhale_GotFocus()
Dim m As String
  m = "Choose the method to be used to determine inhalation health impacts"
  mes = Space(140 - Len(m)) & m
  noact.Enabled = False
  HelpAnchor = Label2.Tag
End Sub

Private Sub Option1_GotFocus(Index As Integer)
Dim m As String
  If Index = 0 Then
    m = "Health impact values will be zero if no toxicity value is available from IRIS or HEAST"
  Else
    m = ""
  End If
  mes = Space(140 - Len(m)) & m
  noact.Enabled = False
  HelpAnchor = Option1(Index).Tag
End Sub

Private Sub SSCheck1_GotFocus(Index As Integer)
Dim m As String
  If SSCheck1(Index).Value = -1 And (Index = 1 Or Index = 2) Then
    m = "Must use ICRP dose and risk factors for cancer risk"
  Else
    m = SSCheck1(Index).Caption & ", click to enable"
  End If
  mes = Space(140 - Len(m)) & m
  noact.Enabled = False
  HelpAnchor = SSCheck1(Index).Tag
End Sub

Private Sub SSCheck2_GotFocus(Index As Integer)
Dim m As String
  m = SSCheck2(Index).Caption & ", click to enable"
  mes = Space(140 - Len(m)) & m
  noact.Enabled = False
  HelpAnchor = SSCheck2(Index).Tag
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

