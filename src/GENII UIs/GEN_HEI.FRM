VERSION 5.00
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "tabctl32.ocx"
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "threed32.ocx"
Begin VB.Form Impacts 
   BorderStyle     =   1  'Fixed Single
   ClientHeight    =   5160
   ClientLeft      =   4215
   ClientTop       =   3075
   ClientWidth     =   7695
   Icon            =   "GEN_HEI.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form2"
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   MinButton       =   0   'False
   NegotiateMenus  =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   344
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   513
   StartUpPosition =   2  'CenterScreen
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
      TabIndex        =   12
      TabStop         =   0   'False
      Top             =   4800
      Width           =   7680
   End
   Begin TabDlg.SSTab SSTab1 
      Height          =   4812
      Left            =   0
      TabIndex        =   11
      Top             =   0
      Width           =   7692
      _ExtentX        =   13573
      _ExtentY        =   8493
      _Version        =   393216
      Style           =   1
      Tab             =   2
      TabHeight       =   529
      TabCaption(0)   =   "Method Selection"
      TabPicture(0)   =   "GEN_HEI.frx":08CA
      Tab(0).ControlEnabled=   0   'False
      Tab(0).Control(0)=   "frm(0)"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).Control(1)=   "Timer1"
      Tab(0).Control(1).Enabled=   0   'False
      Tab(0).ControlCount=   2
      TabCaption(1)   =   "Method Parameters"
      TabPicture(1)   =   "GEN_HEI.frx":08E6
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "frm(1)"
      Tab(1).Control(0).Enabled=   0   'False
      Tab(1).ControlCount=   1
      TabCaption(2)   =   "Constituent Parameters"
      TabPicture(2)   =   "GEN_HEI.frx":0902
      Tab(2).ControlEnabled=   -1  'True
      Tab(2).Control(0)=   "frm(2)"
      Tab(2).Control(0).Enabled=   0   'False
      Tab(2).ControlCount=   1
      Begin VB.Timer Timer1 
         Interval        =   100
         Left            =   -75000
         Top             =   0
      End
      Begin Threed.SSFrame frm 
         Height          =   4092
         Index           =   0
         Left            =   -74760
         TabIndex        =   13
         Top             =   480
         Width           =   7212
         _Version        =   65536
         _ExtentX        =   12721
         _ExtentY        =   7218
         _StockProps     =   14
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "MS Sans Serif"
            Size            =   8.24
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ShadowStyle     =   1
         Enabled         =   0   'False
         Begin VB.OptionButton RType 
            Caption         =   "Calculate radiation dose and risk using  ICRP - 30/48 factors (Federal Guidance Reports 11/12)"
            Height          =   492
            Index           =   0
            Left            =   240
            TabIndex        =   0
            Tag             =   "CETOXT"
            Top             =   480
            Value           =   -1  'True
            Width           =   5000
         End
         Begin VB.OptionButton RType 
            Caption         =   "Calculate Dose and/or risk using ICRP - 60 and EPA risk factors (Federal Guidance Reports 12/13)"
            Height          =   612
            Index           =   1
            Left            =   240
            TabIndex        =   1
            Tag             =   "CETOXT"
            Top             =   1080
            Width           =   5000
         End
         Begin VB.OptionButton RType 
            Caption         =   "Calculate risk using EPA slope factors"
            Height          =   252
            Index           =   2
            Left            =   240
            TabIndex        =   2
            Tag             =   "CETOXT"
            Top             =   1920
            Width           =   5000
         End
      End
      Begin Threed.SSFrame frm 
         Height          =   4140
         Index           =   2
         Left            =   240
         TabIndex        =   14
         Top             =   480
         Width           =   7212
         _Version        =   65536
         _ExtentX        =   12721
         _ExtentY        =   7302
         _StockProps     =   14
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ShadowStyle     =   1
         Enabled         =   0   'False
         Begin VB.ComboBox c_sol 
            Height          =   315
            ItemData        =   "GEN_HEI.frx":091E
            Left            =   4680
            List            =   "GEN_HEI.frx":0931
            Style           =   2  'Dropdown List
            TabIndex        =   10
            Tag             =   "solubil"
            Top             =   1000
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   7
            Left            =   4680
            Style           =   2  'Dropdown List
            TabIndex        =   9
            Tag             =   "um"
            Top             =   1200
            Visible         =   0   'False
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H00E0E0E0&
            Height          =   312
            Index           =   7
            Left            =   3720
            TabIndex        =   8
            Tag             =   "AMAD"
            Text            =   "1.0"
            Top             =   1200
            Visible         =   0   'False
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   6
            Left            =   4680
            Style           =   2  'Dropdown List
            TabIndex        =   7
            Tag             =   "fraction"
            Top             =   840
            Visible         =   0   'False
            Width           =   1000
         End
         Begin VB.ComboBox Combo2 
            Height          =   288
            Left            =   2640
            Style           =   2  'Dropdown List
            TabIndex        =   3
            Top             =   360
            Width           =   3012
         End
         Begin VB.TextBox txt 
            BackColor       =   &H00E0E0E0&
            Height          =   312
            Index           =   6
            Left            =   3720
            TabIndex        =   6
            Tag             =   "F1"
            Text            =   "1.0"
            Top             =   840
            Visible         =   0   'False
            Width           =   1000
         End
         Begin Threed.SSCommand SSCommand2 
            Height          =   264
            Left            =   6120
            TabIndex        =   5
            Top             =   360
            Width           =   372
            _Version        =   65536
            _ExtentX        =   656
            _ExtentY        =   466
            _StockProps     =   78
            Caption         =   ">>"
         End
         Begin Threed.SSCommand SSCommand1 
            Height          =   264
            Left            =   5760
            TabIndex        =   4
            Top             =   360
            Width           =   372
            _Version        =   65536
            _ExtentX        =   656
            _ExtentY        =   466
            _StockProps     =   78
            Caption         =   "<<"
         End
         Begin VB.Label lbl 
            Caption         =   "Lung transfer inhalation class -- SOLUBIL"
            Height          =   252
            Index           =   8
            Left            =   360
            TabIndex        =   20
            Top             =   1000
            Width           =   3000
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Enabled         =   0   'False
            Height          =   252
            Index           =   7
            Left            =   5760
            TabIndex        =   19
            Tag             =   "0"
            Top             =   1200
            Visible         =   0   'False
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Particle size -- AMAD"
            Enabled         =   0   'False
            Height          =   252
            Index           =   7
            Left            =   360
            TabIndex        =   18
            Top             =   1200
            Visible         =   0   'False
            Width           =   3000
         End
         Begin VB.Label Label2 
            Caption         =   "Constituent - FS-CNAME"
            Height          =   252
            Left            =   240
            TabIndex        =   17
            Top             =   420
            Width           =   2112
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Enabled         =   0   'False
            Height          =   252
            Index           =   6
            Left            =   5760
            TabIndex        =   16
            Tag             =   "0"
            Top             =   840
            Visible         =   0   'False
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Intake to blood transfer fraction -- F1"
            Enabled         =   0   'False
            Height          =   252
            Index           =   6
            Left            =   360
            TabIndex        =   15
            Top             =   840
            Visible         =   0   'False
            Width           =   3000
         End
      End
      Begin Threed.SSFrame frm 
         Height          =   4092
         Index           =   1
         Left            =   -74760
         TabIndex        =   21
         Top             =   480
         Width           =   7212
         _Version        =   65536
         _ExtentX        =   12721
         _ExtentY        =   7218
         _StockProps     =   14
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ShadowStyle     =   1
         Enabled         =   0   'False
         Begin VB.TextBox txt 
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   0
            Left            =   2400
            TabIndex        =   29
            Tag             =   "HECONINC"
            Text            =   "6.0e-2"
            Top             =   600
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   1
            Left            =   2400
            TabIndex        =   28
            Tag             =   "HECONFAT"
            Text            =   "5.0e-2"
            Top             =   1200
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   0
            Left            =   3360
            Style           =   2  'Dropdown List
            TabIndex        =   27
            Tag             =   "risk/Sv"
            Top             =   600
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   1
            Left            =   3360
            Style           =   2  'Dropdown List
            TabIndex        =   26
            Tag             =   "risk/Sv"
            Top             =   1200
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   4
            Left            =   4800
            Style           =   2  'Dropdown List
            TabIndex        =   25
            Tag             =   "m"
            Top             =   2640
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   4
            Left            =   3840
            TabIndex        =   24
            Tag             =   "SOILT"
            Text            =   "0.15"
            Top             =   2640
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   5
            Left            =   4800
            Style           =   2  'Dropdown List
            TabIndex        =   23
            Tag             =   "kg/m^3"
            Top             =   3240
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   5
            Left            =   3840
            TabIndex        =   22
            Tag             =   "SLDN"
            Text            =   "1500.0"
            Top             =   3240
            Width           =   1000
         End
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   2
            Left            =   240
            TabIndex        =   30
            Tag             =   "HECEDE"
            Top             =   1560
            Width           =   5172
            _Version        =   65536
            _ExtentX        =   9123
            _ExtentY        =   444
            _StockProps     =   78
            Caption         =   "Calculate radiation effective dose equivalent commitment (CEDE)"
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Value           =   -1  'True
         End
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   1
            Left            =   240
            TabIndex        =   31
            Tag             =   "HEFAT"
            Top             =   960
            Width           =   3996
            _Version        =   65536
            _ExtentX        =   7049
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "Calculate cancer fatalities"
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Value           =   -1  'True
         End
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   0
            Left            =   240
            TabIndex        =   32
            Tag             =   "HEINC"
            Top             =   360
            Width           =   3996
            _Version        =   65536
            _ExtentX        =   7049
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "Calculate lifetime cancer incidence"
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.25
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
            Index           =   0
            Left            =   4440
            TabIndex        =   40
            Tag             =   "0"
            Top             =   600
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   1
            Left            =   4440
            TabIndex        =   39
            Tag             =   "0"
            Top             =   1200
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Conversion factor"
            Height          =   252
            Index           =   1
            Left            =   480
            TabIndex        =   38
            Tag             =   "hcom"
            Top             =   1200
            Width           =   1932
         End
         Begin VB.Label lbl 
            Caption         =   "Conversion factor"
            Height          =   252
            Index           =   0
            Left            =   480
            TabIndex        =   37
            Tag             =   "hinc"
            Top             =   600
            Width           =   1932
         End
         Begin VB.Label lbl 
            Caption         =   "Thickness of contaminated soil/sediment layer -- SOILT"
            Height          =   372
            Index           =   4
            Left            =   240
            TabIndex        =   36
            Top             =   2640
            Width           =   3492
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   4
            Left            =   5880
            TabIndex        =   35
            Tag             =   "0"
            Top             =   2640
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Density of contaminated soil/sediment layer -- SLDN"
            Height          =   372
            Index           =   5
            Left            =   240
            TabIndex        =   34
            Top             =   3240
            Width           =   3492
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   5
            Left            =   5880
            TabIndex        =   33
            Tag             =   "0"
            Top             =   3240
            Width           =   996
         End
      End
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
   Begin VB.Menu howto 
      Caption         =   "&Help"
   End
End
Attribute VB_Name = "Impacts"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Dim temp As parmrec
Dim loadng As Boolean
Dim ctext2 As String
Dim ctext3 As String
' All these just ro resolve indexs from fui to mui
Dim f_numcon As Long, con() As contam
Dim m_numcon As Long, cas() As casparm

Private Sub Combo2_Click()
  caschange Combo2.Text, ctext2
  ctext2 = Combo2.Text
End Sub

Private Sub Form_Unload(Cancel As Integer)
  Dim answer As Long
  If Not called Then
    answer = MsgBox("Do you want save changes?", 51, "GENII Health Impacts Module")
    If answer = 6 Then save_Click
    If answer = 7 Then EndModule
    If answer = 2 Then Cancel = 1
  End If
End Sub

Private Sub leave_Click()
  Form_Unload 0
End Sub

Private Sub RType_Click(Index As Integer)
  SSTab1.Visible = False
  If RType(0).Value Then
    SSTab1.TabVisible(1) = True
    SSTab1.TabVisible(2) = False
  End If
  If RType(1).Value Then
    SSTab1.TabVisible(1) = True
    SSTab1.TabVisible(2) = True
  End If
  If RType(2).Value Then
    SSTab1.TabVisible(1) = False
    SSTab1.TabVisible(2) = False
  End If
  SSTab1.Visible = True
End Sub

Private Sub selref_Click()
  RefMode = 0
  GetRef ref(RefItem)
End Sub

Private Sub addref_Click()
  RefMode = 1
  GetRef ref(RefItem)
End Sub

Private Sub howto_Click()
  GetHelp
End Sub

Private Sub fillet(idx As Long)
  ref(idx).Tag = temp.ref
  ref(idx).Caption = "Ref:" & Str(temp.ref)
  On Error Resume Next
  If temp.cunit <> "N/A" Then set_unit unit(idx), temp.uunit
  txt(idx).Text = convert(temp.cunit, temp.uunit, Val(temp.pval))
End Sub

Sub addcas()
  If temp.idx1 > m_numcon Then
    m_numcon = temp.idx1
    ReDim Preserve cas(temp.idx1) As casparm
    cas(temp.idx1).idx = 0
  End If
  If temp.idx2 > cas(temp.idx1).numprog Then
    cas(temp.idx1).numprog = temp.idx2
    ReDim Preserve cas(temp.idx1).progeny(temp.idx2) As d_casparm
    cas(temp.idx1).progeny(temp.idx2).idx = 0
  End If
End Sub

Sub addcon()
  If temp.idx2 > f_numcon Then
    f_numcon = temp.idx2
    ReDim Preserve con(f_numcon) As contam
  End If
  If temp.idx3 > con(temp.idx2).numprog Then
    con(temp.idx2).numprog = temp.idx3
    ReDim Preserve con(temp.idx2).progeny(temp.idx3) As daughter
  End If
End Sub

Private Sub loadprm()
  Dim i As Long
  Dim j As Long
  Dim k As Long
  Dim m As Long
  Dim fle As parmfile
  Dim sval As Boolean
  
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
                  Select Case temp.pName
                    Case "numcon"
                      f_numcon = Val(temp.pval)
                      ReDim Preserve con(f_numcon) As contam
                    Case "clktype"
                      addcon
                      If temp.idx3 = 0 Then
                        If Val(temp.pval) = 1 Then
                          con(temp.idx2).rad = True
                        Else
                          con(temp.idx2).rad = False
                        End If
                      End If
                    Case "solubil"
                      addcon
                      If temp.idx3 = 0 Then
                        con(temp.idx2).lclass = temp.pval
                      Else
                        con(temp.idx2).progeny(temp.idx3).lclass = temp.pval
                      End If
                    Case "fscname"
                      addcon
                      If temp.idx3 = 0 Then
                        con(temp.idx2).Name = temp.pval
                      Else
                        con(temp.idx2).progeny(temp.idx3).Name = temp.pval
                      End If
                    Case "fscasid"
                      addcon
                      If temp.idx3 = 0 Then
                        con(temp.idx2).cas = temp.pval
                      Else
                        con(temp.idx2).progeny(temp.idx3).cas = temp.pval
                      End If
                  End Select
                End If
              End If
            Next
          Case modName
            For m = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                Loading.update
                Select Case temp.pName
                  Case "METHOD"
                    RType(Val(temp.pval)).Value = True
                  Case "HEINC"
                    sval = False
                    If temp.pval = "true" Then sval = True
                    SSCheck1(0).Value = sval
                    SSCheck1_Click 0, CInt(sval)
                  Case "HEFAT"
                    sval = False
                    If temp.pval = "true" Then sval = True
                    SSCheck1(1).Value = sval
                    SSCheck1_Click 1, CInt(sval)
                  Case "HECEDE"
                    sval = False
                    If temp.pval = "true" Then sval = True
                    SSCheck1(2).Value = sval
                    SSCheck1_Click 2, CInt(sval)
                  Case "HECONINC":    fillet 0
                  Case "HECONFAT":    fillet 1
                  Case "SOILT":       fillet 4
                  Case "SLDN":        fillet 5
                  Case "CASID"
                    addcas
                    If temp.idx2 = 0 Then
                      cas(temp.idx1).cas = temp.pval
                    Else
                      cas(temp.idx1).progeny(temp.idx2).cas = temp.pval
                    End If
'                  Case "F1"
'                    addcas
'                    If temp.idx2 = 0 Then
'                      If temp.pval <> "EMPTY" Then cas(temp.idx1).txt(0) = Val(temp.pval)
'                      cas(temp.idx1).unt(0) = temp.uunit
'                      cas(temp.idx1).ref(1) = temp.ref
'                    Else
'                      If temp.pval <> "EMPTY" Then cas(temp.idx1).progeny(temp.idx2).txt(0) = Val(temp.pval)
'                      cas(temp.idx1).progeny(temp.idx2).unt(0) = temp.uunit
'                      cas(temp.idx1).progeny(temp.idx2).ref(0) = temp.ref
'                    End If
'                  Case "AMAD"
'                    addcas
'                    If temp.idx2 = 0 Then
'                      If temp.pval <> "EMPTY" Then cas(temp.idx1).txt(1) = Val(temp.pval)
'                      cas(temp.idx1).unt(1) = temp.uunit
'                      cas(temp.idx1).ref(1) = temp.ref
'                    Else
'                      If temp.pval <> "EMPTY" Then cas(temp.idx1).progeny(temp.idx2).txt(1) = Val(temp.pval)
'                      cas(temp.idx1).progeny(temp.idx2).unt(1) = temp.uunit
'                      cas(temp.idx1).progeny(temp.idx2).ref(1) = temp.ref
'                    End If
                  Case "SOLUBIL"
                    addcas
                    If temp.idx2 = 0 Then
                      cas(temp.idx1).sol = Val(temp.pval) - 1
                    Else
                      cas(temp.idx1).progeny(temp.idx2).sol = Val(temp.pval) - 1
                    End If
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
  

'resolve contaminate differences if contaminate no longer exists its index is 0
    For i = 1 To f_numcon
      For j = 1 To m_numcon
        If cas(j).cas = con(i).cas Then
          cas(j).idx = i
          Exit For
       End If
      Next
      If j > m_numcon Then
        m_numcon = m_numcon + 1
        ReDim Preserve cas(m_numcon) As casparm
        cas(m_numcon).cas = con(i).cas
        cas(m_numcon).idx = i
        cas(m_numcon).unt(0) = unit(6).Tag
        cas(m_numcon).unt(1) = unit(7).Tag
        Select Case con(i).lclass
        Case "F":    cas(m_numcon).sol = 0
        Case "M":    cas(m_numcon).sol = 1
        Case "S":    cas(m_numcon).sol = 2
        Case "V":    cas(m_numcon).sol = 3
        Case "G":    cas(m_numcon).sol = 4
        Case Else:   cas(m_numcon).sol = -1
        End Select
      End If
    Next
    For i = 1 To m_numcon
      If cas(i).idx > 0 And con(cas(i).idx).rad Then Combo2.AddItem con(cas(i).idx).Name
    Next
  
  'resolve progeny differences if progeny no longer exists its index is 0
    For i = 1 To m_numcon
      If cas(i).idx <> 0 Then
        For j = 1 To con(cas(i).idx).numprog
          For k = 1 To cas(i).numprog
            If cas(i).progeny(k).cas = con(cas(i).idx).progeny(j).cas Then
              cas(i).progeny(k).idx = j
              Exit For
            End If
          Next
          If k > cas(i).numprog Then
            cas(i).numprog = cas(i).numprog + 1
            ReDim Preserve cas(i).progeny(cas(i).numprog) As d_casparm
            cas(i).progeny(cas(i).numprog).cas = con(cas(i).idx).progeny(j).cas
            cas(i).progeny(cas(i).numprog).idx = j
            Select Case con(cas(i).idx).progeny(j).lclass
            Case "F":    cas(i).progeny(cas(i).numprog).sol = 0
            Case "M":    cas(i).progeny(cas(i).numprog).sol = 1
            Case "S":    cas(i).progeny(cas(i).numprog).sol = 2
            Case "V":    cas(i).progeny(cas(i).numprog).sol = 3
            Case "G":    cas(i).progeny(cas(i).numprog).sol = 4
            Case Else:   cas(i).progeny(cas(i).numprog).sol = -1
            End Select
          End If
        Next
      End If
    Next
  
  
  Else
    PutError "Can't find or open file " & FUIName
    EndModule
  End If
End Sub

Private Sub Form_load()
Dim i As Long
  
  StartModule Impacts, "GENII Health Impacts Module", 5
  SetHelpFile App.Path + "\gen_hei.htm"
  SetRefFile ReplaceExt(FUIName, "ref")
  get_conversion_items unit(0).Tag, unit(0)
  get_conversion_items unit(1).Tag, unit(1)
  get_conversion_items unit(4).Tag, unit(4)
  get_conversion_items unit(5).Tag, unit(5)
  get_conversion_items unit(6).Tag, unit(6)
  get_conversion_items unit(7).Tag, unit(7)
  RType_Click 0
  Loading.Show
  loadprm
  Combo2.ListIndex = 0
  Unload Loading
  SSTab1_Click 0
End Sub

Private Sub save_Click()
  Dim fName As String
  Dim parm As parmrec
  Dim fle As parmfile
  Dim i As Long
  Dim j As Long
  Dim k As Long
  Dim m As Long
  
  Combo2_Click
  fName = RunName + ".GID"
  If open_parm(fle, fName, 1) Then
    For j = 0 To 2
      If RType(j).Value Then
        If j = 0 Then
          set_parm parm, "FGR13", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", False
          write_parmrec fle, parm
        End If
        If j = 1 Then
          set_parm parm, "FGR13", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", True
          write_parmrec fle, parm
        End If
        set_parm parm, "METHOD", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CStr(j)
        write_parmrec fle, parm
        If j = 2 Then
          set_parm parm, "IHEAST", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CStr(1)
          write_parmrec fle, parm
        Else
          set_parm parm, "IHEAST", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CStr(0)
          write_parmrec fle, parm
          For i = 0 To 2
            set_parm parm, SSCheck1(i).Tag, 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", SSCheck1(i).Value
            write_parmrec fle, parm
            If SSCheck1(i).Value And i < 2 Then
              If er(i) Then PutError "Parameter " & txt(i).Tag & " is invalid"
              set_parm parm, txt(i).Tag, 0, 0, 0, 0, 0, 0, Val(ref(i).Tag), unit(i).Text, unit(i).Tag, convert(unit(i).Text, unit(i).Tag, Val(txt(i)))
              write_parmrec fle, parm
            End If
          Next
          If er(4) Then PutError "Parameter " & txt(4).Tag & " is invalid"
          If er(5) Then PutError "Parameter " & txt(5).Tag & " is invalid"
          set_parm parm, txt(4).Tag, 0, 0, 0, 0, 0, 0, Val(ref(4).Tag), unit(4).Text, unit(4).Tag, convert(unit(4).Text, unit(4).Tag, Val(txt(4)))
          write_parmrec fle, parm
          set_parm parm, txt(5).Tag, 0, 0, 0, 0, 0, 0, Val(ref(5).Tag), unit(5).Text, unit(5).Tag, convert(unit(5).Text, unit(5).Tag, Val(txt(5)))
          write_parmrec fle, parm
          set_parm parm, "NUMCON", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", Format(m_numcon)
          write_parmrec fle, parm
          If j = 1 Then
            For i = 1 To m_numcon
              If cas(i).idx <> 0 And con(cas(i).idx).rad Then
                set_parm parm, "CASID", cas(i).idx, 0, 0, 0, 0, 0, 0, "N/A", "N/A", cas(i).cas
                write_parmrec fle, parm
                If (cas(i).sol < 0) Then PutError "Parameter SOLUBIL for " & cas(i).cas & " is invalid"
                set_parm parm, "SOLUBIL", cas(i).idx, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CStr(cas(i).sol + 1)
                write_parmrec fle, parm
                For k = 0 To 1
                  If cas(i).txt(k) = Empty Then
'                    PutError "Parameter " & txt(k + 6).Tag & " for " & cas(i).cas & " is invalid"
                    set_parm parm, txt(k + 6).Tag, cas(i).idx, 0, 0, 0, 0, 0, 0, unit(k + 6).Tag, unit(k + 6).Tag, "EMPTY"
                  Else
                    set_parm parm, txt(k + 6).Tag, cas(i).idx, 0, 0, 0, 0, 0, cas(i).ref(k), cas(i).unt(k), unit(k + 6).Tag, convert(cas(i).unt(k), unit(k + 6).Tag, Val(cas(i).txt(k)))
                  End If
                  write_parmrec fle, parm
                Next
                set_parm parm, "NDS", cas(i).idx, 0, 0, 0, 0, 0, 0, "N/A", "N/A", Format(cas(i).numprog)
                write_parmrec fle, parm
                For k = 1 To cas(i).numprog
                  If cas(i).progeny(k).idx <> 0 Then
                    set_parm parm, "CASID", cas(i).idx, cas(i).progeny(k).idx, 0, 0, 0, 0, 0, "N/A", "N/A", cas(i).progeny(k).cas
                    write_parmrec fle, parm
                  End If
                Next
              End If
            Next
          End If
        End If
      End If
    Next
    close_parm fle
  Else
    PutError "Unable to create transaction file" & RunName & ".GID"
  End If
  EndModule
End Sub

Private Sub SSCheck1_Click(Index As Integer, Value As Integer)
  If Index = 2 Then Exit Sub
  txt(Index).Enabled = Value
  ref(Index).Enabled = Value
  lbl(Index).Enabled = Value
  ref(Index).Enabled = Value
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

Private Sub SSCommand1_Click()
  If Combo2.ListIndex > 0 Then Combo2.ListIndex = Combo2.ListIndex - 1
End Sub

Private Sub SSCommand2_Click()
  If Combo2.ListIndex < Combo2.ListCount - 1 Then Combo2.ListIndex = Combo2.ListIndex + 1
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
    Case 0, 1
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
      t1 = convert(unit(Index).Tag, unit(Index).Text, 500)
      t2 = convert(unit(Index).Tag, unit(Index).Text, 3000)
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

Private Sub SSTab1_GotFocus()
  mes = ""
  noact.Enabled = False
End Sub

Private Sub txt_GotFocus(Index As Integer)
  er CLng(Index)
  RefItem = Index
  noact.Enabled = True
End Sub

Private Sub unit_GotFocus(Index As Integer)
  er CLng(Index)
  RefItem = Index
  noact.Enabled = True
End Sub

Private Sub SSCheck1_GotFocus(Index As Integer)
Dim m As String
  If SSCheck1(Index).Value = -1 And Index = 1 Then
    m = "Must use ICRP dose and risk factors for cancer risk"
  Else
    m = SSCheck1(Index).Caption & ", click to enable"
  End If
  mes = Space(140 - Len(m)) & m
  noact.Enabled = False
End Sub

Private Sub Combo2_GotFocus()
Dim m As String
  m = "Select a constituent"
  mes = Space(140 - Len(m)) & m
  RefItem = 12
  noact.Enabled = True
  HelpAnchor = ""
End Sub

Private Sub SSCommand1_GotFocus()
  mes = ""
  noact.Enabled = False
  HelpAnchor = ""
End Sub

Private Sub SSCommand2_GotFocus()
  mes = ""
  noact.Enabled = False
  HelpAnchor = ""
End Sub

Private Sub c_sol_GotFocus()
Dim m As String
  m = "Select lung transfer inhalation class"
  mes = Space(140 - Len(m)) & m
  noact.Enabled = False
  HelpAnchor = ""
End Sub

Private Sub caschange(tx As String, oldtx As String)
  Dim i As Long
  Dim j As Long
  For i = 1 To m_numcon
    If con(cas(i).idx).Name = oldtx Then
      For j = 0 To 1
        If txt(j + 6).Text = "" Then
          cas(i).txt(j) = Empty
        Else
          cas(i).txt(j) = txt(j + 6).Text
        End If
        cas(i).unt(j) = unit(j + 6).Text
        cas(i).ref(j) = Val(ref(j + 6).Tag)
      Next
      cas(i).sol = c_sol.ListIndex
      Exit For
    End If
  Next
  
  For i = 1 To m_numcon
    If con(cas(i).idx).Name = tx Then
      For j = 0 To 1
        set_unit unit(j + 6), cas(i).unt(j)
        If cas(i).txt(j) = Empty Then
          txt(j + 6).Text = ""
        Else
          txt(j + 6).Text = cas(i).txt(j)
        End If
        ref(j + 6).Caption = "Ref: " & cas(i).ref(j)
        ref(j + 6).Tag = cas(i).ref(j)
      Next
      c_sol.ListIndex = cas(i).sol
      Exit For
    End If
  Next
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

