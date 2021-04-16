VERSION 5.00
Object = "{B02F3647-766B-11CE-AF28-C3A2FBE76A13}#2.5#0"; "ss32x25.ocx"
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "tabctl32.ocx"
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "THREED32.OCX"
Begin VB.Form Known 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "SourceMod"
   ClientHeight    =   5760
   ClientLeft      =   4728
   ClientTop       =   4356
   ClientWidth     =   7680
   Icon            =   "Knowng.frx":0000
   LinkTopic       =   "Form1"
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   480
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   640
   Begin Threed.SSPanel SSFrame2 
      Height          =   2412
      Left            =   240
      TabIndex        =   38
      Top             =   3108
      Width           =   7212
      _Version        =   65536
      _ExtentX        =   12721
      _ExtentY        =   4255
      _StockProps     =   15
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   7.8
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BevelWidth      =   0
      BorderWidth     =   0
      Begin FPSpreadADO.fpSpread vaSpread1 
         Height          =   1635
         Left            =   360
         TabIndex        =   34
         Top             =   720
         Width           =   2145
         _Version        =   131077
         _ExtentX        =   3768
         _ExtentY        =   2752
         _StockProps     =   64
         AutoCalc        =   0   'False
         AutoSize        =   -1  'True
         BackColorStyle  =   1
         DisplayRowHeaders=   0   'False
         EditEnterAction =   5
         EditModePermanent=   -1  'True
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         FormulaSync     =   0   'False
         MaxCols         =   5
         RetainSelBlock  =   0   'False
         RowsFrozen      =   1
         ScrollBarExtMode=   -1  'True
         SelectBlockOptions=   0
         SpreadDesigner  =   "Knowng.frx":030A
         StartingColNumber=   0
         StartingRowNumber=   0
         UserResize      =   0
         VisibleCols     =   2
         VisibleRows     =   5
      End
      Begin FPSpreadADO.fpSpread vaSpread2 
         Height          =   1635
         Left            =   4080
         TabIndex        =   37
         Top             =   720
         Width           =   2145
         _Version        =   131077
         _ExtentX        =   3768
         _ExtentY        =   2752
         _StockProps     =   64
         AutoCalc        =   0   'False
         AutoSize        =   -1  'True
         BackColorStyle  =   1
         DisplayRowHeaders=   0   'False
         EditEnterAction =   5
         EditModePermanent=   -1  'True
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         FormulaSync     =   0   'False
         MaxCols         =   5
         RetainSelBlock  =   0   'False
         RowsFrozen      =   1
         ScrollBarExtMode=   -1  'True
         SelectBlockOptions=   0
         SpreadDesigner  =   "Knowng.frx":118BD
         StartingColNumber=   0
         StartingRowNumber=   0
         UserResize      =   0
         VisibleCols     =   2
         VisibleRows     =   5
      End
      Begin VB.ComboBox Combo3 
         Height          =   288
         Left            =   4080
         Style           =   2  'Dropdown List
         TabIndex        =   20
         Top             =   360
         Width           =   2052
      End
      Begin VB.ComboBox Combo2 
         Height          =   288
         Left            =   360
         Style           =   2  'Dropdown List
         TabIndex        =   17
         Top             =   384
         Width           =   2052
      End
      Begin Threed.SSCommand SSCommand4 
         Height          =   192
         Left            =   5760
         TabIndex        =   19
         Top             =   120
         Width           =   312
         _Version        =   65536
         _ExtentX        =   550
         _ExtentY        =   339
         _StockProps     =   78
         Caption         =   ">>"
      End
      Begin Threed.SSCommand SSCommand3 
         Height          =   192
         Left            =   5460
         TabIndex        =   18
         Top             =   120
         Width           =   312
         _Version        =   65536
         _ExtentX        =   550
         _ExtentY        =   339
         _StockProps     =   78
         Caption         =   "<<"
      End
      Begin Threed.SSCommand SSCommand2 
         Height          =   192
         Left            =   2040
         TabIndex        =   16
         Top             =   120
         Width           =   312
         _Version        =   65536
         _ExtentX        =   550
         _ExtentY        =   339
         _StockProps     =   78
         Caption         =   ">>"
      End
      Begin Threed.SSCommand SSCommand1 
         Height          =   192
         Left            =   1740
         TabIndex        =   15
         Top             =   120
         Width           =   312
         _Version        =   65536
         _ExtentX        =   550
         _ExtentY        =   339
         _StockProps     =   78
         Caption         =   "<<"
      End
      Begin VB.Label ref 
         Caption         =   "Ref: 0"
         Height          =   252
         Index           =   7
         Left            =   6240
         TabIndex        =   42
         Tag             =   "0"
         Top             =   360
         Width           =   732
      End
      Begin VB.Label ref 
         Caption         =   "Ref: 0"
         Height          =   252
         Index           =   6
         Left            =   2520
         TabIndex        =   41
         Tag             =   "0"
         Top             =   360
         Width           =   732
      End
      Begin VB.Label lbl 
         Caption         =   "Progeny"
         Height          =   252
         Index           =   7
         Left            =   3840
         TabIndex        =   40
         Top             =   120
         Width           =   2412
      End
      Begin VB.Label lbl 
         Caption         =   "Parent"
         Height          =   252
         Index           =   6
         Left            =   120
         TabIndex        =   39
         Top             =   120
         Width           =   2412
      End
   End
   Begin Threed.SSFrame SSFrame1 
      Height          =   5292
      Left            =   192
      TabIndex        =   21
      Top             =   300
      Width           =   7332
      _Version        =   65536
      _ExtentX        =   12933
      _ExtentY        =   9335
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
         Height          =   288
         Index           =   0
         Left            =   3960
         TabIndex        =   3
         Tag             =   "one"
         Top             =   660
         Width           =   1000
      End
      Begin VB.TextBox txt 
         Height          =   288
         Index           =   1
         Left            =   3960
         TabIndex        =   5
         Tag             =   "two"
         Top             =   1020
         Width           =   1000
      End
      Begin VB.TextBox txt 
         Height          =   288
         Index           =   2
         Left            =   3960
         TabIndex        =   7
         Tag             =   "three"
         Top             =   1380
         Width           =   1000
      End
      Begin VB.TextBox txt 
         Height          =   288
         Index           =   3
         Left            =   3960
         TabIndex        =   9
         Tag             =   "four"
         Top             =   1740
         Width           =   1000
      End
      Begin VB.ComboBox unit 
         Height          =   288
         Index           =   0
         Left            =   4920
         Style           =   2  'Dropdown List
         TabIndex        =   4
         Tag             =   "m"
         Top             =   660
         Width           =   1000
      End
      Begin VB.ComboBox unit 
         Height          =   288
         Index           =   1
         Left            =   4920
         Style           =   2  'Dropdown List
         TabIndex        =   6
         Tag             =   "m"
         Top             =   1020
         Width           =   1000
      End
      Begin VB.ComboBox unit 
         Height          =   288
         Index           =   2
         Left            =   4920
         Style           =   2  'Dropdown List
         TabIndex        =   8
         Tag             =   "m"
         Top             =   1380
         Width           =   1000
      End
      Begin VB.ComboBox unit 
         Height          =   288
         Index           =   3
         Left            =   4920
         Style           =   2  'Dropdown List
         TabIndex        =   10
         Tag             =   "m^3/yr"
         Top             =   1740
         Width           =   1000
      End
      Begin VB.ComboBox Combo4 
         Height          =   288
         ItemData        =   "Knowng.frx":22E70
         Left            =   3960
         List            =   "Knowng.frx":22E72
         Style           =   2  'Dropdown List
         TabIndex        =   1
         Tag             =   "fslocate"
         Top             =   300
         Width           =   1932
      End
      Begin VB.TextBox txt 
         Height          =   288
         Index           =   4
         Left            =   3960
         TabIndex        =   11
         Tag             =   "five"
         Top             =   2100
         Width           =   1000
      End
      Begin VB.ComboBox unit 
         Height          =   288
         Index           =   4
         Left            =   4920
         Style           =   2  'Dropdown List
         TabIndex        =   12
         Tag             =   "m^3/yr"
         Top             =   2100
         Width           =   1000
      End
      Begin VB.TextBox txt 
         Height          =   288
         Index           =   5
         Left            =   3960
         TabIndex        =   13
         Tag             =   "six"
         Top             =   2460
         Width           =   1000
      End
      Begin VB.ComboBox unit 
         Height          =   288
         Index           =   5
         Left            =   4920
         Style           =   2  'Dropdown List
         TabIndex        =   14
         Tag             =   "m^3/yr"
         Top             =   2460
         Width           =   1000
      End
      Begin Threed.SSCommand SSCommand5 
         Height          =   312
         Left            =   6000
         TabIndex        =   2
         Top             =   300
         Width           =   1092
         _Version        =   65536
         _ExtentX        =   1926
         _ExtentY        =   550
         _StockProps     =   78
         Caption         =   "Flux Types"
         ForeColor       =   0
      End
      Begin VB.Label lbl 
         Caption         =   "lb0"
         Height          =   252
         Index           =   0
         Left            =   192
         TabIndex        =   36
         Top             =   660
         Width           =   3600
      End
      Begin VB.Label lbl 
         Caption         =   "lb1"
         Height          =   252
         Index           =   1
         Left            =   180
         TabIndex        =   35
         Top             =   1020
         Width           =   3600
      End
      Begin VB.Label lbl 
         Caption         =   "lb2"
         Height          =   252
         Index           =   2
         Left            =   180
         TabIndex        =   33
         Top             =   1380
         Width           =   3600
      End
      Begin VB.Label lbl 
         Caption         =   "lb3"
         Height          =   252
         Index           =   3
         Left            =   180
         TabIndex        =   32
         Top             =   1740
         Width           =   3600
      End
      Begin VB.Label Label3 
         Caption         =   "Contaminate Flux"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   252
         Left            =   1920
         TabIndex        =   31
         Top             =   14280
         Width           =   2412
      End
      Begin VB.Label lbl 
         Caption         =   "lbl9"
         Height          =   252
         Index           =   9
         Left            =   180
         TabIndex        =   30
         Top             =   300
         Width           =   3600
      End
      Begin VB.Label ref 
         Caption         =   "Ref: 0"
         Height          =   252
         Index           =   0
         Left            =   6000
         TabIndex        =   29
         Tag             =   "0"
         Top             =   660
         Width           =   996
      End
      Begin VB.Label ref 
         Caption         =   "Ref: 0"
         Height          =   252
         Index           =   1
         Left            =   6000
         TabIndex        =   28
         Tag             =   "0"
         Top             =   1020
         Width           =   996
      End
      Begin VB.Label ref 
         Caption         =   "Ref: 0"
         Height          =   252
         Index           =   2
         Left            =   6000
         TabIndex        =   27
         Tag             =   "0"
         Top             =   1380
         Width           =   996
      End
      Begin VB.Label ref 
         Caption         =   "Ref: 0"
         Height          =   252
         Index           =   3
         Left            =   6000
         TabIndex        =   26
         Tag             =   "0"
         Top             =   1740
         Width           =   996
      End
      Begin VB.Label ref 
         Caption         =   "Ref: 0"
         Height          =   252
         Index           =   4
         Left            =   6000
         TabIndex        =   25
         Tag             =   "0"
         Top             =   2100
         Width           =   996
      End
      Begin VB.Label lbl 
         Caption         =   "lb4"
         Height          =   252
         Index           =   4
         Left            =   180
         TabIndex        =   24
         Top             =   2100
         Width           =   3600
      End
      Begin VB.Label lbl 
         Caption         =   "lb5"
         Height          =   252
         Index           =   5
         Left            =   180
         TabIndex        =   23
         Top             =   2460
         Width           =   3600
      End
      Begin VB.Label ref 
         Caption         =   "Ref: 0"
         Height          =   252
         Index           =   5
         Left            =   6000
         TabIndex        =   22
         Tag             =   "0"
         Top             =   2460
         Width           =   996
      End
   End
   Begin TabDlg.SSTab SSTab1 
      Height          =   5772
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   7692
      _ExtentX        =   13568
      _ExtentY        =   10181
      _Version        =   393216
      Style           =   1
      Tabs            =   4
      TabsPerRow      =   4
      TabHeight       =   420
      TabCaption(0)   =   "Water Flux"
      TabPicture(0)   =   "Knowng.frx":22E74
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).ControlCount=   0
      TabCaption(1)   =   "Overland Flux"
      TabPicture(1)   =   "Knowng.frx":22E90
      Tab(1).ControlEnabled=   0   'False
      Tab(1).ControlCount=   0
      TabCaption(2)   =   "Atmospheric Flux"
      TabPicture(2)   =   "Knowng.frx":22EAC
      Tab(2).ControlEnabled=   0   'False
      Tab(2).ControlCount=   0
      TabCaption(3)   =   "Soil Concentration"
      TabPicture(3)   =   "Knowng.frx":22EC8
      Tab(3).ControlEnabled=   0   'False
      Tab(3).ControlCount=   0
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
   Begin VB.Menu opt 
      Caption         =   "&Options"
      Begin VB.Menu prog 
         Caption         =   "Include Progeny"
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
   Begin VB.Menu help 
      Caption         =   "&Help"
      Begin VB.Menu howto 
         Caption         =   "How to ..."
      End
      Begin VB.Menu about 
         Caption         =   "&About"
      End
   End
End
Attribute VB_Name = "Known"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Dim kind As String
Dim mode As Long
Dim sitename As String
Dim c1 As Long
Dim c2 As Long
Dim c3 As Long
Dim temp As parmrec
Dim loadng As Boolean
Dim havevad As Boolean
Dim haveovl As Boolean
Dim prevtime As Double
 
Private Sub Check_NTS(nts As Long, errmsg As String)
  If (nts < 2) Then PutError errmsg
End Sub

Private Sub Is_Ascending(time As Double, errmsg As String)
  If prevtime >= time Then
    prevtime = time
    PutError errmsg
  Else
    prevtime = time
  End If
End Sub
  
Private Sub about_Click()
  frmAbout.picIcon = frmAbout.ImageList1.ListImages(1).Picture
  frmAbout.Show 1, Known
End Sub

Private Sub Form_Unload(Cancel As Integer)
  Dim answer As Long
  If Not called Then
    answer = MsgBox("Do you want save changes?", 51, "FRAMES Known Source Module")
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

Private Sub selref_Click()
  RefMode = 0
  SetRefFile argv(1) + ".ref"
  GetRef ref(RefItem)
End Sub

Private Sub addref_Click()
  RefMode = 1
  SetRefFile argv(1) + ".ref"
  GetRef ref(RefItem)
End Sub

Private Sub hlp_Click()
  GetHelp
End Sub

Private Sub Command1_Click()
 SSTab1.Tab = SSTab1.Tab + 1
End Sub

Private Sub prog_Click()
  prog.Checked = Not prog.Checked
  If sink(c1).con(c2).numprog > 0 And prog.Checked Then
    lbl(7).Visible = True
    ref(7).Visible = True
    Combo3.Visible = True
    vaSpread2.Visible = True
    SSCommand3.Visible = True
    SSCommand4.Visible = True
    vaSpread1.VisibleCols = 2
  Else
    lbl(7).Visible = False
    ref(7).Visible = False
    Combo3.Visible = False
    vaSpread2.Visible = False
    SSCommand3.Visible = False
    SSCommand4.Visible = False
    If SSTab1.Tab = 2 Then
      vaSpread1.VisibleCols = 5
    Else
      vaSpread1.VisibleCols = 2
    End If
  End If
End Sub

Private Sub SStab1_GotFocus()
  RefItem = -1
  noact.Enabled = False
End Sub

Private Sub Combo4_GotFocus()
  RefItem = -1
  noact.Enabled = False
End Sub

Private Sub Combo2_GotFocus()
  RefItem = 6
End Sub

Private Sub Combo3_GotFocus()
  RefItem = 7
End Sub

Private Sub Combo4_Click()
  If Combo4.Text = "area" Then
     SetEnabled 1, False
     SetEnabled 2, False
     SetEnabled 3, False
     SetEnabled 4, False
     SetEnabled 5, False
  Else
     SetEnabled 0, False ' Gis data
     SetEnabled 1, False ' Gis data
     SetEnabled 2, True
     SetEnabled 3, True
     SetEnabled 4, True
     SetEnabled 5, True
     If SSTab1.Tab = 0 Then
       If Combo4.Text = "Aquifer" Then
         lbl(1).Caption = "Height of flux plane"
       Else
         lbl(1).Caption = "Length of flux plane"
       End If
     End If
  End If
End Sub

Private Sub fluxfillet()
  FluxTypes.ref(temp.idx1).Tag = temp.ref
  FluxTypes.ref(temp.idx1).Caption = "Ref:" & Str(temp.ref)
  If temp.cunit <> "N/A" Then set_unit FluxTypes.unit(temp.idx1), temp.uunit
  FluxTypes.txt(temp.idx1).Text = convert(temp.cunit, temp.uunit, val(temp.pval))
End Sub

Private Sub gisfillet(idx1 As Long, idx As Long, valu As Double, unt As String)
  sink(idx1).ref(idx) = 0
  sink(idx1).pnt(idx) = unt
  sink(idx1).unt(idx) = unt
  sink(idx1).val(idx) = val(Format$(valu, "0.0#"))
End Sub
Private Sub fillet(idx As Long)
  sink(temp.idx1).ref(idx) = temp.ref
  sink(temp.idx1).pnt(idx) = temp.cunit
  sink(temp.idx1).unt(idx) = temp.uunit
  sink(temp.idx1).val(idx) = convert(temp.cunit, temp.uunit, val(temp.pval))
End Sub

Private Sub SizeParentProgenyList()
  If temp.idx2 > f_numcon Then
    ReDim Preserve con(temp.idx2) As parent
    f_numcon = temp.idx2
  End If
  If temp.idx3 > con(temp.idx2).numprog Then
    ReDim Preserve con(temp.idx2).prog(temp.idx3) As progeny
    con(temp.idx2).numprog = temp.idx3
  End If
End Sub

Private Sub SizeSink()
  If temp.idx2 > sink(temp.idx1).numcon Then
    ReDim Preserve sink(temp.idx1).con(temp.idx2) As parentflux
    sink(temp.idx1).numcon = temp.idx2
  End If
  If temp.idx3 > sink(temp.idx1).con(temp.idx2).numprog Then
    ReDim Preserve sink(temp.idx1).con(temp.idx2).prog(temp.idx3) As progflux
    sink(temp.idx1).con(temp.idx2).numprog = temp.idx3
  End If
  If temp.idx3 = 0 Then
    If temp.idx4 > sink(temp.idx1).con(temp.idx2).nts Then
      ReDim Preserve sink(temp.idx1).con(temp.idx2).series(temp.idx4) As fluxrec
      sink(temp.idx1).con(temp.idx2).nts = temp.idx4
    End If
  Else
    If temp.idx4 > sink(temp.idx1).con(temp.idx2).prog(temp.idx3).nts Then
      ReDim Preserve sink(temp.idx1).con(temp.idx2).prog(temp.idx3).series(temp.idx4) As fluxrec
      sink(temp.idx1).con(temp.idx2).prog(temp.idx3).nts = temp.idx4
    End If
  End If
End Sub

Private Sub loadprm()
  Dim i As Long, j As Long, k As Long, l As Long, m As Long
  Dim fle As parmfile
  Dim sval As Boolean
  
  f_numcon = 0
  For i = 1 To 4
    Select Case i
    Case 1, 2, 4:
      sink(i).unt(1) = "m"
      sink(i).unt(2) = "m"
      sink(i).unt(3) = "m"
      sink(i).unt(4) = "m/yr"
      sink(i).pnt(1) = "m"
      sink(i).pnt(2) = "m"
      sink(i).pnt(3) = "m"
      sink(i).pnt(4) = "m/yr"
    Case 3
      sink(i).unt(1) = "m^2"
      sink(i).unt(2) = "m"
      sink(i).unt(3) = "m"
      sink(i).unt(4) = "m/s"
      sink(i).unt(5) = "C"
      sink(i).unt(6) = "C"
      sink(i).pnt(1) = "m^2"
      sink(i).pnt(2) = "m"
      sink(i).pnt(3) = "m"
      sink(i).pnt(4) = "m/s"
      sink(i).pnt(5) = "C"
      sink(i).pnt(6) = "C"
    End Select
    ReDim Preserve sink(i).con(1) As parentflux
    sink(i).con(0).use = False
    sink(i).con(0).cas = "water"
    sink(i).con(0).unit(0) = "m^3/yr"
    sink(i).con(0).unit(1) = "yr"
    sink(i).con(0).unit(2) = "m^3/yr"
    sink(i).con(0).unit(3) = "m^3/yr"
    sink(i).con(0).unit(4) = "m^3/yr"
    sink(i).con(0).unit(5) = "m^3/yr"
  Next
  
  Dim GisName As String: GisName = ""
  
  If open_parm(fle, FUIName, 2) Then
    Do Until EOCF(fle.file)
      If read_parmrec(fle, temp) Then
        Select Case temp.pname
          Case "fui"
            For i = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                If temp.idx1 = siteIdx Then
                  Select Case temp.pname
                    Case "sitename":         sitename = temp.pval
                    Case "srcdespath"
                      DesName = temp.pval
                    Case "vadsrcname"
                      If temp.pval = ModName Then sink(1).use = True
                      havevad = True
                    Case "aqusrcname"
                      If temp.pval = ModName Then sink(1).use = True
                    Case "ovlsrcname"
                      If temp.pval = ModName Then sink(2).use = True
                      haveovl = True
                    Case "rivsrcname"
                      If temp.pval = ModName Then sink(2).use = True
                    Case "airsrcname"
                      If temp.pval = ModName Then sink(3).use = True
                      Load FluxTypes
                    Case "expsrcname"
                      If temp.pval = ModName Then sink(4).use = True
                    Case "GISName"
                      GisName = temp.pval
                    Case "fscname"
                      SizeParentProgenyList
                      If temp.idx3 > 0 Then
                        con(temp.idx2).prog(temp.idx3).name = temp.pval
                      Else
                        con(temp.idx2).name = temp.pval
                      End If
                    Case "fscasid"
                      SizeParentProgenyList
                      If temp.idx3 > 0 Then
                        con(temp.idx2).prog(temp.idx3).cas = temp.pval
                      Else
                        con(temp.idx2).cas = temp.pval
                      End If
                    Case "clktype"
                      SizeParentProgenyList
                      If temp.idx3 > 0 Then
                        con(temp.idx2).prog(temp.idx3).kind = val(temp.pval)
                      Else
                        con(temp.idx2).kind = val(temp.pval)
                      End If
                   End Select
                End If
              End If
            Next
          Case ModName
            Loading.Gauge1.Max = val(temp.idx1)
            Loading.Gauge1.Value = 0
            For i = 1 To temp.idx1
              Loading.update
              If read_parmrec(fle, temp) Then
                Select Case temp.pname
                  Case "progeny":
                     sval = False
                     If temp.pval = "True" Then sval = True
                     prog.Checked = sval
                  Case "one":              fillet 1
                  Case "two":              fillet 2
                  Case "three":            fillet 3
                  Case "four":             fillet 4
                  Case "five":             fillet 5
                  Case "six":              fillet 6
                  Case "media":
                    sink(temp.idx1).media = temp.pval
                  Case "radius", "density":
                    haveflux = True
                    fluxfillet
                  Case "fluxtypes":
                     sval = False
                     If temp.pval = "True" Then sval = True
                     FluxTypes.SSCheck1(temp.idx1).Value = sval
                  Case "casid":
                    SizeSink
                    If temp.idx3 = 0 Then
                      sink(temp.idx1).con(temp.idx2).cas = temp.pval
                      sink(temp.idx1).con(temp.idx2).ref = temp.ref
                      sink(temp.idx1).con(temp.idx2).use = False
                      sink(temp.idx1).con(temp.idx2).unit(0) = temp.cunit
                      sink(temp.idx1).con(temp.idx2).unit(1) = temp.uunit
                      For j = 2 To 5
                        sink(temp.idx1).con(temp.idx2).unit(j) = temp.cunit
                      Next
                    Else
                      sink(temp.idx1).con(temp.idx2).prog(temp.idx3).cas = temp.pval
                      sink(temp.idx1).con(temp.idx2).prog(temp.idx3).ref = temp.ref
                      sink(temp.idx1).con(temp.idx2).prog(temp.idx3).use = False
                      sink(temp.idx1).con(temp.idx2).prog(temp.idx3).unit(0) = temp.cunit
                      sink(temp.idx1).con(temp.idx2).prog(temp.idx3).unit(1) = temp.uunit
                      For j = 2 To 5
                        sink(temp.idx1).con(temp.idx2).prog(temp.idx3).unit(j) = temp.cunit
                      Next
                    End If
                  Case "cflux":
                    SizeSink
                    If temp.idx3 = 0 Then
                      sink(temp.idx1).con(temp.idx2).series(temp.idx4).flux(temp.idx5) = val(convert(temp.cunit, temp.uunit, val(temp.pval)))
                      If temp.idx5 <> 0 Then sink(temp.idx1).con(temp.idx2).unit(temp.idx5 + 1) = temp.uunit
                    Else
                      sink(temp.idx1).con(temp.idx2).prog(temp.idx3).series(temp.idx4).flux(temp.idx5) = val(convert(temp.cunit, temp.uunit, val(temp.pval)))
                      sink(temp.idx1).con(temp.idx2).prog(temp.idx3).unit(temp.idx5 + 1) = temp.uunit
                    End If
                  Case "ctime":
                    SizeSink
                    If temp.idx3 = 0 Then
                      sink(temp.idx1).con(temp.idx2).series(temp.idx4).time = val(convert(temp.cunit, temp.uunit, val(temp.pval)))
                      sink(temp.idx1).con(temp.idx2).unit(1) = temp.uunit
                    Else
                      sink(temp.idx1).con(temp.idx2).prog(temp.idx3).series(temp.idx4).time = val(convert(temp.cunit, temp.uunit, val(temp.pval)))
                      sink(temp.idx1).con(temp.idx2).prog(temp.idx3).unit(1) = temp.uunit
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
    
    Dim geoidx As Long
    ReDim poly.x(0): ReDim poly.y(0): ReDim poly.z(0)
    
    If GisName <> "" Then
      If open_parm(fle, FUIName, 2) Then
        Do Until EOCF(fle.file)
          If read_parmrec(fle, temp) Then
            Select Case temp.pname
              Case GisName
                For i = 1 To temp.idx1
                  If read_parmrec(fle, temp) Then
                    Select Case temp.pname
                      Case "ModuleId"
                        If ModName = temp.pval Then
                          geoidx = temp.idx1
                        End If
                      Case "ModGeoObjIndex"
                        If geoidx = temp.idx1 Then
                          geoidx = val(temp.pval)
                        End If
                      Case "GeoObjIndex"
                        If temp.idx1 = geoidx Then
                          geoidx = val(temp.pval)
                        End If
                      Case "PolygonArea"
                        If temp.idx1 = geoidx Then
                          poly.area = val(temp.pval)
                        End If
                      Case "PolygonPts"
                        If temp.idx1 = geoidx Then
                          If temp.idx2 > UBound(poly.x) Then
                            ReDim Preserve poly.x(temp.idx2)
                            ReDim Preserve poly.y(temp.idx2)
                            ReDim Preserve poly.z(temp.idx2)
                          End If
                          If temp.idx3 = 1 Then poly.x(temp.idx2) = val(temp.pval)
                          If temp.idx3 = 2 Then poly.y(temp.idx2) = val(temp.pval)
                          If temp.idx3 = 3 Then
                            If 0# < val(temp.pval) Then
                              poly.z(temp.idx2) = val(temp.pval)
                            Else
                              poly.z(temp.idx2) = val(sink(1).val(3))
                            End If
                          End If
                        End If
                      Case "NumPolygonPts"
                        If temp.idx1 = geoidx Then
                          poly.numvtx = val(temp.pval)
                          Exit Do
                        End If
                    End Select
                  End If
                Next
                Exit Do
              Case Else
                For m = 1 To temp.idx1
                  get_line fle.file
                Next
            End Select
          End If
        Loop
      End If
      close_parm fle
      poly.dx = Abs(poly.x(2) - poly.x(1))
      gisfillet 1, 1, poly.dx, "m"                         ' width
      If poly.dx > 0 Then gisfillet 1, 2, poly.area / poly.dx, "m"   ' length
      gisfillet 1, 3, poly.z(1), "m"
      
      
    End If

'resolve contaminate differences if contaminate no longer exists its index is 0
    
    con(0).name = "water"
    con(0).cas = "water"
    con(0).kind = -1
    con(0).numprog = 0
    For i = 1 To 4
      If sink(i).use Then
        For j = 0 To f_numcon
          For k = 0 To sink(i).numcon
            If con(j).cas = sink(i).con(k).cas Then
              If j <> 0 Or (i <> 3 And i <> 4) Then sink(i).con(k).use = True
              sink(i).con(k).cas = con(j).cas
              sink(i).con(k).name = con(j).name
              sink(i).con(k).numprog = con(j).numprog
              
' changes for Bonnie
              If mode = 1 Then
                If sink(i).con(k).unit(1) = "" Then sink(i).con(k).unit(1) = "yr"
                If con(j).kind = 1 Then
                  If i <> 4 Then
                    setparentunit i, k, "pCi/yr"
                  Else
                    setparentunit i, k, "pCi/kg"
                  End If
                ElseIf con(j).kind = -1 Then
                    setparentunit i, k, "m^3/yr"
                Else
                  If i <> 4 Then
                    setparentunit i, k, "g/yr"
                  Else
                    setparentunit i, k, "g/kg"
                  End If
                End If
              End If
' end changes for Bonnie
              
              For l = 1 To con(j).numprog
                For m = 1 To sink(i).con(k).numprog
                  If con(j).prog(l).cas = sink(i).con(k).prog(m).cas Then
                    sink(i).con(k).prog(m).use = True
                    sink(i).con(k).prog(m).name = con(j).prog(l).name
                    sink(i).con(k).prog(m).cas = con(j).prog(l).cas
                    
'changes for bonnie
                    If mode = 1 Then
                      If sink(i).con(k).prog(m).unit(1) = "" Then sink(i).con(k).prog(m).unit(1) = "yr"
                      If con(j).kind = 1 Then
                        If i <> 4 Then
                          setprogunit i, k, m, "pCi/yr"
                        Else
                          setprogunit i, k, m, "pCi/kg"
                        End If
                      Else
                        If i <> 4 Then
                          setprogunit i, k, m, "g/yr"
                        Else
                          setprogunit i, k, m, "g/kg"
                        End If
                      End If
                    End If
' changes for bonnie
                    
                    Exit For
                  End If
                Next
                If m > sink(i).con(k).numprog Then
                  ReDim Preserve sink(i).con(k).prog(m) As progflux
                  sink(i).con(k).numprog = m
                  sink(i).con(k).prog(m).use = True
                  sink(i).con(k).prog(m).name = con(j).prog(l).name
                  sink(i).con(k).prog(m).cas = con(j).prog(l).cas
                  sink(i).con(k).prog(m).unit(1) = "yr"
                  If con(j).kind = 1 Then
                    If i <> 4 Then
                      setprogunit i, k, m, "pCi/yr"
                    Else
                      setprogunit i, k, m, "pCi/kg"
                    End If
                  Else
                    If i <> 4 Then
                      setprogunit i, k, m, "g/yr"
                    Else
                      setprogunit i, k, m, "g/kg"
                    End If
                  End If
                End If
              Next
              Exit For
            End If
          Next
          If k > sink(i).numcon Then
            ReDim Preserve sink(i).con(k) As parentflux
            sink(i).numcon = k
            sink(i).con(k).use = True
            sink(i).con(k).cas = con(j).cas
            sink(i).con(k).name = con(j).name
            sink(i).con(k).numprog = con(j).numprog
            sink(i).con(k).unit(1) = "yr"
            If con(j).kind = 1 Then
              If i <> 4 Then
                setparentunit i, k, "pCi/yr"
              Else
                setparentunit i, k, "pCi/kg"
              End If
            ElseIf con(j).kind = -1 Then
                setparentunit i, k, "m^3/yr"
            Else
              If i <> 4 Then
                setparentunit i, k, "g/yr"
              Else
                setparentunit i, k, "g/kg"
              End If
            End If
            If con(j).numprog > 0 Then
              ReDim Preserve sink(i).con(k).prog(con(j).numprog) As progflux
              For l = 1 To con(j).numprog
                sink(i).con(k).prog(l).use = True
                sink(i).con(k).prog(l).name = con(j).prog(l).name
                sink(i).con(k).prog(l).cas = con(j).prog(l).cas
                sink(i).con(k).prog(l).unit(1) = "yr"
                If con(j).kind = 1 Then
                  If i <> 4 Then
                    setprogunit i, k, l, "pCi/yr"
                  Else
                    setprogunit i, k, l, "pCi/kg"
                  End If
                Else
                  If i <> 4 Then
                    setprogunit i, k, l, "g/yr"
                  Else
                    setprogunit i, k, l, "g/kg"
                  End If
                End If
              Next
            End If
          End If
        Next
      End If
    Next
    
    sval = False
    For i = 0 To SSTab1.Tabs - 1
      If sink(i + 1).use Then
        SSTab1.TabVisible(i) = True
        sval = True
      Else
        If i = SSTab1.Tabs - 1 And Not sval Then
          PutError "Not connected to a glyph"
          EndModule
        End If
        SSTab1.TabVisible(i) = False
      End If
    Next
  Else
    PutError "Can't find or open file " & FUIName
    EndModule
  End If
End Sub

Private Sub setparentunit(a As Long, b As Long, u As String)
Dim i As Long
  sink(a).con(b).unit(0) = u
  For i = 2 To 5
   If sink(a).con(b).unit(i) = "" Then sink(a).con(b).unit(i) = u
  Next
End Sub

Private Sub setprogunit(a As Long, b As Long, c As Long, u As String)
Dim i As Long
  sink(a).con(b).prog(c).unit(0) = u
  For i = 2 To 5
   If sink(a).con(b).prog(c).unit(i) = "" Then sink(a).con(b).prog(c).unit(i) = u
  Next
End Sub

Private Sub Form_Load()
  Dim cnt As Long
  Dim i As Integer
  
  StartModule Known, "FRAMES Known Source Module", 6
  SetHelpFile App.Path + "\known.htm"
  mode = val(argv(0))
  Loading.Show
  loadng = True
  loadprm
  Unload Loading
  If mode = 1 Then
    If sink(1).use Or sink(2).use Then Make_WFF True
    If sink(3).use Then Make_AFF
    If sink(4).use Then Make_SCF
    EndModule
  Else
    For i = 0 To 3
      If SSTab1.TabVisible(i) Then
        c1 = i + 1
        SSTab1_Click i
  '        If i = 0 Then SSTab1_Click 0
        Exit For
      End If
    Next
  End If
  loadng = False
  Refresh
End Sub

Private Sub save_Click()
  Dim i As Long, j As Long, k As Long
  Dim l As Long, m As Long, n As Long
  Dim y As Long
  Dim Ok(4) As Boolean
  Dim fname As String
  Dim parm As parmrec
  Dim fle As parmfile
  
  SSTab1_Click SSTab1.Tab
  fname = RunName & ".GID"
  If open_parm(fle, fname, 1) Then
    set_parm parm, "progeny", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", prog.Checked
    write_parmrec fle, parm
    For i = 1 To 4
      If sink(i).use Then
        set_parm parm, "media", i, 0, 0, 0, 0, 0, 0, "N/A", "N/A", sink(i).media
        write_parmrec fle, parm
        For j = 0 To 5
          If sink(i).val(j + 1) = "" Then
            PutError "Parameter " & txt(j).Tag & " is invalid for media " & sink(i).media
          Else
            set_parm parm, txt(j).Tag, i, 0, 0, 0, 0, 0, sink(i).ref(j + 1), sink(i).unt(j + 1), sink(i).pnt(j + 1), convert(sink(i).unt(j + 1), sink(i).pnt(j + 1), val(sink(i).val(j + 1)))
            write_parmrec fle, parm
          End If
          'skip ones that aren't used for these [i] sinks
          If sink(i).media = "Area" And j = 0 Then j = 5
          If (i = 1 Or i = 2) And j = 3 Then j = 5
          If i = 4 And j = 2 Then j = 5
        Next
        k = 0
        If i = 3 Then
          For j = 0 To 3
            set_parm parm, "Fluxtypes", j, 0, 0, 0, 0, 0, 0, "N/A", "N/A", FluxTypes.SSCheck1(j).Value
            write_parmrec fle, parm
            If FluxTypes.SSCheck1(j).Enabled And FluxTypes.SSCheck1(j).Value Then
              Ok(j) = True
              set_parm parm, "density", k, 0, 0, 0, 0, 0, FluxTypes.ref(k).Tag, FluxTypes.unit(k).Text, FluxTypes.unit(k).Tag, convert(FluxTypes.unit(k).Text, FluxTypes.unit(k).Tag, val(FluxTypes.txt(k).Text))
              write_parmrec fle, parm
              k = k + 1
              If k < 6 Then
                set_parm parm, "radius", k, 0, 0, 0, 0, 0, FluxTypes.ref(k).Tag, FluxTypes.unit(k).Text, FluxTypes.unit(k).Tag, convert(FluxTypes.unit(k).Text, FluxTypes.unit(k).Tag, val(FluxTypes.txt(k).Text))
                write_parmrec fle, parm
                k = k + 1
              End If
            End If
          Next
        End If
        
        If i = 1 Or i = 2 Then
          set_parm parm, "casid", i, 0, 0, 0, 0, 0, sink(i).con(0).ref, "yr", "m^3/yr", "water"
          write_parmrec fle, parm
          Check_NTS sink(i).con(0).nts, sink(i).con(0).name + " number of time sequences less than two"
'check for ascending times
          prevtime = -1
          For l = 1 To sink(i).con(0).nts
            Is_Ascending sink(i).con(0).series(l).time, sink(i).con(0).name + " flux out of sequence"
            set_parm parm, "ctime", i, 0, 0, l, 0, 0, 0, sink(i).con(0).unit(1), "yr", convert(sink(i).con(0).unit(1), "yr", sink(i).con(0).series(l).time)
            write_parmrec fle, parm
            set_parm parm, "cflux", i, 0, 0, l, 1, 0, 0, sink(i).con(0).unit(2), "m^3/yr", convert(sink(i).con(0).unit(2), "m^3/yr", sink(i).con(0).series(l).flux(1))
            write_parmrec fle, parm
          Next
        End If
        prevtime = -1
        For j = 1 To f_numcon
          For k = 1 To sink(i).numcon
            If sink(i).con(k).cas = con(j).cas Then
              set_parm parm, "casid", i, j, 0, 0, 0, 0, sink(i).con(k).ref, "yr", sink(i).con(k).unit(0), con(j).cas
              write_parmrec fle, parm
              prevtime = -1
              Check_NTS sink(i).con(k).nts, sink(i).con(k).name + " number of time sequences less than two"
              For l = 1 To sink(i).con(k).nts
                Is_Ascending sink(i).con(k).series(l).time, sink(i).con(k).name + " flux out of sequence"
                set_parm parm, "ctime", i, j, 0, l, 0, 0, 0, sink(i).con(k).unit(1), "yr", convert(sink(i).con(k).unit(1), "yr", sink(i).con(k).series(l).time)
                write_parmrec fle, parm
                If i = 3 Then         ' if air source
                  For m = 0 To 3
                    If Ok(m) Then
                      set_parm parm, "cflux", i, j, 0, l, m + 1, 0, 0, sink(i).con(k).unit(m + 2), sink(i).con(k).unit(0), convert(sink(i).con(k).unit(m + 2), sink(i).con(k).unit(0), sink(i).con(k).series(l).flux(m + 1))
                      write_parmrec fle, parm
                    End If
                  Next
                Else
                  set_parm parm, "cflux", i, j, 0, l, 1, 0, 0, sink(i).con(k).unit(2), sink(i).con(k).unit(0), convert(sink(i).con(k).unit(2), sink(i).con(k).unit(0), sink(i).con(k).series(l).flux(1))
                  write_parmrec fle, parm
                End If
              Next
              For l = 1 To con(j).numprog
                For m = 1 To sink(i).con(k).numprog
                  If sink(i).con(k).prog(m).cas = con(j).prog(l).cas Then
                    set_parm parm, "casid", i, j, l, 0, 0, 0, sink(i).con(k).prog(m).ref, "yr", sink(i).con(k).prog(m).unit(0), con(j).prog(l).cas
                    write_sparmrec fle, parm
                    If prog.Checked = True Then Check_NTS sink(i).con(k).prog(m).nts, sink(i).con(k).prog(m).name + " number of time sequences less than two"
                    prevtime = -1
                    For n = 1 To sink(i).con(k).prog(m).nts
                      Is_Ascending sink(i).con(k).prog(m).series(n).time, sink(i).con(k).prog(m).name
                      set_parm parm, "ctime", i, j, l, n, 0, 0, 0, sink(i).con(k).prog(m).unit(1), "yr", convert(sink(i).con(k).prog(m).unit(1), "yr", sink(i).con(k).prog(m).series(n).time)
                      write_parmrec fle, parm
                      If i = 3 Then   ' if air source
                        For y = 0 To 3
                          If Ok(y) Then
                            set_parm parm, "cflux", i, j, l, n, y + 1, 0, 0, sink(i).con(k).prog(m).unit(y + 2), sink(i).con(k).prog(m).unit(0), convert(sink(i).con(k).prog(m).unit(y + 2), sink(i).con(k).prog(m).unit(0), sink(i).con(k).prog(m).series(n).flux(y + 1))
                            write_parmrec fle, parm
                          End If
                        Next
                      Else
                        set_parm parm, "cflux", i, j, l, n, 1, 0, 0, sink(i).con(k).prog(m).unit(2), sink(i).con(k).prog(m).unit(0), convert(sink(i).con(k).prog(m).unit(2), sink(i).con(k).prog(m).unit(0), sink(i).con(k).prog(m).series(n).flux(1))
                        write_parmrec fle, parm
                      End If
                    Next
                  End If
                Next
              Next
            End If
          Next
        Next
      End If
    Next
    close_parm fle
  Else
    PutError "Unable to create transaction file" & RunName & ".GID"
  End If
  EndModule
End Sub

Private Sub PutHeader(file As csv)
    put_val file, 7
    put_line file
    put_val file, "=================================================="
    put_line file
    put_val file, "  FRAMES Known Source Module"
    put_line file
    put_val file, "  Version 1.1 September 1, 1999"
    put_line file
    put_val file, "  Run: " & RunName
    put_line file
    put_val file, "  Site: " & sitename
    put_line file
    put_val file, "  " & Date$
    put_line file
    put_val file, "=================================================="
    put_line file
End Sub
Private Function Make_WFF(Optional gff As Boolean = False)
  Dim i As Long, j As Long, k As Long
  Dim l As Long, m As Long, n As Long
  Dim connum As Long
  Dim msg As String
  Dim cnt As Long
  Dim file As csv
  Dim Ok As Boolean
  
  If gff Then
    Ok = open_csv(file, RunName & ".GFF", 1)
  Else
    Ok = open_csv(file, RunName & ".WFF", 1)
  End If
  If Ok Then
    PutHeader file
    cnt = 0
    For i = 1 To 2
      If sink(i).use Then
        cnt = cnt + 1
      End If
    Next
    put_val file, cnt
    put_line file
    For i = 1 To 2
      If sink(i).use Then
        put_val file, ModName
        If i = 1 Then put_val file, sink(i).media
        If i = 2 Then put_val file, "Overland"
        For j = 1 To 4
          put_val file, convert(sink(i).unt(j), sink(i).pnt(j), val(sink(i).val(j)))
          put_val file, sink(i).pnt(j)
        Next
        put_val file, f_numcon
        put_line file
        If gff Then
          put_val file, poly.numvtx
          put_line file
          For j = 1 To poly.numvtx
            put_val file, poly.x(j)
            put_val file, poly.y(j)
            put_val file, val(sink(1).val(3)) ' poly.z(j)
            put_line file
          Next j
        End If
        put_val file, "yr"
        put_val file, "m^3/yr"
        put_val file, sink(i).con(0).nts
        put_line file
        For j = 1 To sink(i).con(0).nts
          put_val file, convert(sink(i).con(0).unit(1), "yr", sink(i).con(0).series(j).time)
          put_val file, convert(sink(i).con(0).unit(2), "m^3/yr", sink(i).con(0).series(j).flux(1))
          put_line file
        Next
        For j = 1 To f_numcon
          For k = 1 To sink(i).numcon
            If sink(i).con(k).cas = con(j).cas Then
              If sink(i).con(k).nts > 0 Then
                put_val file, con(j).name
                put_sval file, con(j).cas
                put_val file, "yr"
                put_val file, sink(i).con(k).unit(0)
                put_val file, sink(i).con(k).nts
                put_val file, 1
                put_val file, sink(i).con(k).numprog
                put_line file
                For l = 1 To sink(i).con(k).nts
                  put_val file, convert(sink(i).con(k).unit(1), "yr", sink(i).con(k).series(l).time)
                  put_val file, convert(sink(i).con(k).unit(2), sink(i).con(k).unit(0), sink(i).con(k).series(l).flux(1))
                  put_line file
                Next
                For l = 1 To con(j).numprog
                  For m = 1 To sink(i).con(k).numprog
                    If sink(i).con(k).prog(m).cas = con(j).prog(l).cas Then
                      If sink(i).con(k).prog(m).nts > 0 And prog.Checked Then
                        put_val file, con(j).prog(l).name
                        put_sval file, con(j).prog(l).cas
                        put_val file, "yr"
                        put_val file, sink(i).con(k).prog(m).unit(0)
                        put_val file, sink(i).con(k).prog(m).nts
                        put_val file, 1
                        put_val file, con(j).name
                        put_val file, con(j).cas
                        put_line file
                        For n = 1 To sink(i).con(k).prog(m).nts
                          put_val file, convert(sink(i).con(k).prog(m).unit(1), "yr", sink(i).con(k).prog(m).series(n).time)
                          put_val file, convert(sink(i).con(k).prog(m).unit(2), sink(i).con(k).prog(m).unit(0), sink(i).con(k).prog(m).series(n).flux(1))
                          put_line file
                        Next
                      Else
                        put_val file, con(j).prog(l).name
                        put_sval file, con(j).prog(l).cas
                        put_val file, "yr"
                        put_val file, sink(i).con(k).prog(m).unit(0)
                        put_val file, 2
                        put_val file, 1
                        put_val file, con(j).name
                        put_val file, con(j).cas
                        put_line file
                        put_val file, 0#
                        put_val file, 0#
                        put_line file
                        put_val file, 1#
                        put_val file, 0#
                        put_line file
                      End If
                    End If
                  Next
                Next
              Else
                put_val file, con(j).name
                put_sval file, con(j).cas
                put_val file, "yr"
                put_val file, sink(i).con(k).unit(0)
                put_val file, 2
                put_val file, 1
                put_val file, sink(i).con(k).numprog
                put_line file
                put_val file, 0#
                put_val file, 0#
                put_line file
                put_val file, 1#
                put_val file, 0#
                put_line file
              End If
            End If
          Next
        Next
      End If
    Next
    close_csv file
  Else
    If gff Then
      PutError "Unable to create water fluxrec file" & RunName & ".GFF"
    Else
      PutError "Unable to create water fluxrec file" & RunName & ".WFF"
    End If
  End If
End Function

Private Function Make_AFF()
  Dim i As Long, j As Long, k As Long
  Dim l As Long, m As Long, n As Long
  Dim connum As Long
  Dim Ok(3) As Boolean
  Dim file As csv
  
  If open_csv(file, RunName & ".AFF", 1) Then
    PutHeader file
    'sets the index to the air concentration dataset
    i = 3
    put_val file, 1
    put_line file
    put_val file, ModName
    put_line file
    put_val file, sink(i).media
    put_line file
    For j = 1 To 6
      If sink(i).media = "Area" And j > 1 And j < 6 Then
        put_val file, 0
      Else
        put_val file, convert(sink(i).unt(j), sink(i).pnt(j), val(sink(i).val(j)))
      End If
      put_val file, sink(i).pnt(j)
      put_line file
    Next
    k = 0
    For j = 0 To 3
      If FluxTypes.SSCheck1(j).Enabled And FluxTypes.SSCheck1(j).Value Then
        k = k + 1
        Ok(j) = True
      End If
    Next
    put_val file, k
    put_line file
    k = 1
    For j = 0 To 3
      If Ok(j) Then
        put_val file, FluxTypes.SSCheck1(j).Tag
        If j = 0 Then
          put_val file, 0
          put_val file, "um"
          put_val file, convert(FluxTypes.unit(j).Text, FluxTypes.unit(j).Tag, val(FluxTypes.txt(j).Text))
          put_val file, FluxTypes.unit(j).Tag
        Else
          put_val file, convert(FluxTypes.unit(k).Text, FluxTypes.unit(k).Tag, val(FluxTypes.txt(k).Text))
          put_val file, FluxTypes.unit(k).Tag
          put_val file, convert(FluxTypes.unit(k + 1).Text, FluxTypes.unit(k + 1).Tag, val(FluxTypes.txt(k + 1).Text))
          put_val file, FluxTypes.unit(k + 1).Tag
          k = k + 2
        End If
        put_line file
      End If
    Next
    put_val file, f_numcon
    put_line file
    
    For j = 1 To f_numcon
      For k = 1 To sink(i).numcon
        If sink(i).con(k).cas = con(j).cas Then
          If sink(i).con(k).nts > 0 Then
            put_val file, con(j).name
            put_sval file, con(j).cas
            put_val file, "yr"
            put_val file, sink(i).con(k).unit(0)
            put_val file, sink(i).con(k).nts
            put_val file, sink(i).con(k).numprog
            put_line file
            For l = 1 To sink(i).con(k).nts
              put_val file, convert(sink(i).con(k).unit(1), "yr", sink(i).con(k).series(l).time)
              If Ok(0) Then put_val file, convert(sink(i).con(k).unit(2), sink(i).con(k).unit(0), sink(i).con(k).series(l).flux(1))
              If Ok(1) Then put_val file, convert(sink(i).con(k).unit(3), sink(i).con(k).unit(0), sink(i).con(k).series(l).flux(2))
              If Ok(2) Then put_val file, convert(sink(i).con(k).unit(4), sink(i).con(k).unit(0), sink(i).con(k).series(l).flux(3))
              If Ok(3) Then put_val file, convert(sink(i).con(k).unit(5), sink(i).con(k).unit(0), sink(i).con(k).series(l).flux(4))
              put_line file
            Next
            For l = 1 To con(j).numprog
              For m = 1 To sink(i).con(k).numprog
                If sink(i).con(k).prog(m).cas = con(j).prog(l).cas Then
                  If sink(i).con(k).prog(m).nts > 0 And prog.Checked Then
                    put_val file, con(j).prog(l).name
                    put_sval file, con(j).prog(l).cas
                    put_val file, "yr"
                    put_val file, sink(i).con(k).prog(m).unit(0)
                    put_val file, sink(i).con(k).prog(m).nts
                    put_val file, con(j).name
                    put_val file, con(j).cas
                    put_line file
                    For n = 1 To sink(i).con(k).prog(m).nts
                      put_val file, convert(sink(i).con(k).prog(m).unit(1), "yr", sink(i).con(k).prog(m).series(n).time)
                      If Ok(0) Then put_val file, convert(sink(i).con(k).prog(m).unit(2), sink(i).con(k).prog(m).unit(0), sink(i).con(k).prog(m).series(n).flux(1))
                      If Ok(1) Then put_val file, convert(sink(i).con(k).prog(m).unit(3), sink(i).con(k).prog(m).unit(0), sink(i).con(k).prog(m).series(n).flux(2))
                      If Ok(2) Then put_val file, convert(sink(i).con(k).prog(m).unit(4), sink(i).con(k).prog(m).unit(0), sink(i).con(k).prog(m).series(n).flux(3))
                      If Ok(3) Then put_val file, convert(sink(i).con(k).prog(m).unit(5), sink(i).con(k).prog(m).unit(0), sink(i).con(k).prog(m).series(n).flux(4))
                      put_line file
                    Next
                  Else
                    put_val file, con(j).prog(l).name
                    put_sval file, con(j).prog(l).cas
                    put_val file, "yr"
                    put_val file, sink(i).con(k).prog(m).unit(0)
                    put_val file, 2
                    put_val file, con(j).name
                    put_val file, con(j).cas
                    put_line file
                    put_val file, 0#
                    If Ok(0) Then put_val file, 0#
                    If Ok(1) Then put_val file, 0#
                    If Ok(2) Then put_val file, 0#
                    If Ok(3) Then put_val file, 0#
                    put_line file
                    put_val file, 1#
                    If Ok(0) Then put_val file, 0#
                    If Ok(1) Then put_val file, 0#
                    If Ok(2) Then put_val file, 0#
                    If Ok(3) Then put_val file, 0#
                    put_line file
                  End If
                End If
              Next
            Next
          Else
            put_val file, con(j).name
            put_sval file, con(j).cas
            put_val file, "yr"
            put_val file, sink(i).con(k).unit(0)
            put_val file, 2
            put_val file, sink(i).con(k).numprog
            put_line file
            put_val file, 0#
            If Ok(0) Then put_val file, 0#
            If Ok(1) Then put_val file, 0#
            If Ok(2) Then put_val file, 0#
            If Ok(3) Then put_val file, 0#
            put_line file
            put_val file, 1#
            If Ok(0) Then put_val file, 0#
            If Ok(1) Then put_val file, 0#
            If Ok(2) Then put_val file, 0#
            If Ok(3) Then put_val file, 0#
            put_line file
          End If
        End If
      Next
    Next
    close_csv file
  Else
    PutError "Unable to create soil concentration file" & RunName & ".SCF"
  End If
End Function

Private Function Make_SCF()
  Dim i As Long, j As Long, k As Long
  Dim l As Long, m As Long, n As Long
  Dim connum As Long
  Dim file As csv
  
  If open_csv(file, RunName & ".SCF", 1) Then
    PutHeader file
    'sets the index to the soil concentration dataset
    i = 4
    put_val file, 1
    put_line file
    put_val file, ModName
    put_val file, sink(i).media
    put_val file, 1
    put_line file
    put_val file, "Source"
    For j = 1 To 3
      put_val file, convert(sink(i).unt(j), sink(i).pnt(j), val(sink(i).val(j)))
      put_val file, sink(i).pnt(j)
    Next
    connum = 0
    For j = 1 To sink(i).numcon
      If sink(i).con(j).nts > 0 Then connum = connum + 1
    Next
    put_val file, connum
    put_line file
    
    For j = 1 To f_numcon
      For k = 1 To sink(i).numcon
        If sink(i).con(k).cas = con(j).cas Then
          If sink(i).con(k).nts > 0 Then
            put_val file, con(j).name
            put_sval file, con(j).cas
            put_val file, "yr"
            put_val file, sink(i).con(k).unit(0)
            put_val file, sink(i).con(k).nts
            If prog.Checked Then
              connum = 0
              For l = 1 To sink(i).con(k).numprog
                If sink(i).con(k).prog(l).nts > 0 Then connum = connum + 1
              Next
              put_val file, connum
            Else
              put_val file, 0
            End If
            put_line file
            For l = 1 To sink(i).con(k).nts
              put_val file, convert(sink(i).con(k).unit(1), "yr", sink(i).con(k).series(l).time)
              put_val file, convert(sink(i).con(k).unit(2), sink(i).con(k).unit(0), sink(i).con(k).series(l).flux(1))
              put_line file
            Next
            If prog.Checked Then
              For l = 1 To con(j).numprog
                For m = 1 To sink(i).con(k).numprog
                  If sink(i).con(k).prog(m).cas = con(j).prog(l).cas Then
                    If sink(i).con(k).prog(m).nts > 0 Then
                      put_val file, con(j).prog(l).name
                      put_sval file, con(j).prog(l).cas
                      put_val file, "yr"
                      put_val file, sink(i).con(k).prog(m).unit(0)
                      put_val file, sink(i).con(k).prog(m).nts
                      put_val file, con(j).name
                      put_val file, con(j).cas
                      put_line file
                      For n = 1 To sink(i).con(k).prog(m).nts
                        put_val file, convert(sink(i).con(k).prog(m).unit(1), "yr", sink(i).con(k).prog(m).series(n).time)
                        put_val file, convert(sink(i).con(k).prog(m).unit(2), sink(i).con(k).prog(m).unit(0), sink(i).con(k).prog(m).series(n).flux(1))
                        put_line file
                      Next
                    End If
                  End If
                Next
              Next
            End If
          End If
        End If
      Next
    Next
    close_csv file
  Else
    PutError "Unable to create soil concentration file" & RunName & ".SCF"
  End If
End Function

Private Sub getparent()
  Dim i As Long
  Dim j As Long
  Dim temp As String
  vaSpread1.Row = 1
  For j = 1 To 5
    vaSpread1.col = j
    sink(c1).con(c2).unit(j) = vaSpread1.Value
  Next
  sink(c1).con(c2).ref = ref(6).Tag
  sink(c1).con(c2).nts = vaSpread1.DataRowCnt - 1
  ReDim Preserve sink(c1).con(c2).series(sink(c1).con(c2).nts)
  
  For j = 2 To sink(c1).con(c2).nts + 1
    vaSpread1.col = 1
    vaSpread1.Row = j
    temp = vaSpread1.Value
    If Len(temp) > 0 Then
      sink(c1).con(c2).series(j - 1).time = val(vaSpread1.Value)
      vaSpread1.Text = ""
      For i = 2 To 5
        vaSpread1.col = i
        sink(c1).con(c2).series(j - 1).flux(i - 1) = val(vaSpread1.Value)
        vaSpread1.Text = ""
      Next
    Else
      sink(c1).con(c2).nts = j - 2
      Exit For
    End If
  Next
End Sub

Private Sub getprog()
  Dim i As Long
  Dim j As Long
  Dim temp As String
  If sink(c1).con(c2).numprog = 0 Then Exit Sub
  vaSpread2.Row = 1
  For j = 1 To 5
    vaSpread2.col = j
    sink(c1).con(c2).prog(c3).unit(j) = vaSpread2.Value
  Next
  sink(c1).con(c2).prog(c3).ref = ref(7).Tag
  sink(c1).con(c2).prog(c3).nts = vaSpread2.DataRowCnt - 1
  ReDim Preserve sink(c1).con(c2).prog(c3).series(sink(c1).con(c2).prog(c3).nts)
  For j = 2 To sink(c1).con(c2).prog(c3).nts + 1
    vaSpread2.col = 1
    vaSpread2.Row = j
    temp = vaSpread2.Value
    If Len(temp) > 0 Then
      sink(c1).con(c2).prog(c3).series(j - 1).time = val(vaSpread2.Value)
      vaSpread2.Text = ""
      For i = 2 To 5
        vaSpread2.col = i
        sink(c1).con(c2).prog(c3).series(j - 1).flux(i - 1) = val(vaSpread2.Value)
        vaSpread2.Text = ""
      Next
    Else
      sink(c1).con(c2).prog(c3).nts = j - 2
      Exit For
    End If
  Next
End Sub
    
Private Function cvt(tval As Double) As String
  If Abs(tval) > 100000 Or Abs(tval) < 0.001 And tval <> 0 Then
    'cvt = Format(tval, "0.0########E+00")
    cvt = Format(tval, "0.0###E+00")
  Else
    cvt = Format(tval, "#####0.0####")
  End If
End Function


Private Sub putparent()
  Dim i As Long
  Dim j As Long
  
  ClearSpread vaSpread1
  ref(6).Caption = "Ref: " + Str(sink(c1).con(c2).ref)
  ref(6).Tag = sink(c1).con(c2).ref
  vaSpread1.Row = 0
  vaSpread1.col = 2
'  If SSTab1.Tab = 2 Then
'    vaSpread1.Text = "Gas 1"
'  ElseIf Combo2.Text = "Water" Then
'    vaSpread1.Text = "Flow Rate"
'  Else
'    vaSpread1.Text = "Flux Rate"
'  End If
  For j = 1 To 5
    SetSpreadUnits vaSpread1, j, sink(c1).con(c2).unit(j)
    vaSpread1.TypeComboBoxEditable = True
    vaSpread1.Text = sink(c1).con(c2).unit(j)
    vaSpread1.TypeComboBoxEditable = False
  Next
  For j = 1 To sink(c1).con(c2).nts
    vaSpread1.col = 1
    vaSpread1.Row = j + 1
    vaSpread1.Text = sink(c1).con(c2).series(j).time
    For i = 2 To 5
      vaSpread1.col = i
      vaSpread1.Text = cvt(sink(c1).con(c2).series(j).flux(i - 1))
    Next
  Next
End Sub

Private Sub putprog()
  Dim i As Long
  Dim j As Long
  
  ClearSpread vaSpread2
  If sink(c1).con(c2).numprog = 0 Then Exit Sub
  ref(7).Caption = "Ref: " + Str(sink(c1).con(c2).prog(c3).ref)
  ref(7).Tag = sink(c1).con(c2).prog(c3).ref
  For j = 1 To 5
    SetSpreadUnits vaSpread2, j, sink(c1).con(c2).prog(c3).unit(j)
    vaSpread2.TypeComboBoxEditable = True
    vaSpread2.Text = sink(c1).con(c2).prog(c3).unit(j)
    vaSpread2.TypeComboBoxEditable = False
  Next
  For j = 1 To sink(c1).con(c2).prog(c3).nts
    vaSpread2.col = 1
    vaSpread2.Row = j + 1
    vaSpread2.Text = sink(c1).con(c2).prog(c3).series(j).time
    For i = 2 To 5
      vaSpread2.col = i
      vaSpread2.Text = cvt(sink(c1).con(c2).prog(c3).series(j).flux(i - 1))
    Next
  Next
End Sub

Private Sub SSTab1_Click(PreviousTab As Integer)
  Dim j As Long
  If Not loadng Then
    If c1 = 3 Then
      sink(c1).media = Combo4.Text
    Else
      sink(1).media = Combo4.Text
      sink(2).media = Combo4.Text
      sink(4).media = Combo4.Text
    End If
    For j = 1 To 6
      sink(c1).unt(j) = unit(j - 1).Text
      sink(c1).val(j) = txt(j - 1).Text
      sink(c1).ref(j) = ref(j - 1).Tag
    Next
    getparent
    getprog
    c1 = SSTab1.Tab + 1
  End If
  Select Case c1
  Case 1, 2: waterLabels
  Case 3: airLabels
  Case 4: soilLabels
  End Select
  Combo4.ListIndex = 0
  For j = 0 To Combo4.ListCount - 1
    If sink(c1).media = Combo4.list(j) Then Combo4.ListIndex = j
  Next
  Combo2.Clear
  For j = 0 To sink(c1).numcon
    If sink(c1).con(j).use Then
      Combo2.AddItem sink(c1).con(j).name
      Combo2.ItemData(Combo2.NewIndex) = j
    End If
  Next
  For j = 1 To 6
    unit(j - 1).Clear
    get_conversion_items sink(c1).pnt(j), unit(j - 1)
    set_unit unit(j - 1), sink(c1).unt(j)
    txt(j - 1).Text = sink(c1).val(j)
    ref(j - 1).Tag = sink(c1).ref(j)
  Next
  putparent
  putprog
  Combo2.ListIndex = 0
End Sub

Private Sub Combo2_Click()
  Dim i As Long
  If Not loadng Then
    getparent
    getprog
  End If
  c2 = Combo2.ItemData(Combo2.ListIndex)
  putparent
  If sink(c1).con(c2).numprog = 0 Then
    lbl(7).Visible = False
    ref(7).Visible = False
    Combo3.Visible = False
    vaSpread2.Visible = False
    SSCommand3.Visible = False
    SSCommand4.Visible = False
    If SSTab1.Tab = 2 Then
      vaSpread1.VisibleCols = 5
      Refresh
    Else
      vaSpread1.VisibleCols = 2

    End If
  Else
    If prog.Checked Then
      lbl(7).Visible = True
      ref(7).Visible = True
      Combo3.Visible = True
      vaSpread2.Visible = True
      SSCommand3.Visible = True
      SSCommand4.Visible = True
      vaSpread1.VisibleCols = 3
    Else
      If SSTab1.Tab = 2 Then
        vaSpread1.VisibleCols = 5
      Else
        vaSpread1.VisibleCols = 2
      End If
      Refresh
    End If
    Combo3.Clear
    For i = 1 To sink(c1).con(c2).numprog
      If sink(c1).con(c2).prog(i).use Then
        Combo3.AddItem sink(c1).con(c2).prog(i).name
        Combo3.ItemData(Combo3.NewIndex) = i
      End If
    Next
    loadng = True
    Combo3.ListIndex = 0
    loadng = False
  End If
End Sub

Private Sub Combo3_Click()
  If Not loadng Then
    getprog
  End If
  c3 = Combo3.ItemData(Combo3.ListIndex)
  putprog
End Sub

Private Sub SSCommand1_Click()
   If Combo2.ListIndex > 0 Then Combo2.ListIndex = Combo2.ListIndex - 1
End Sub

Private Sub SSCommand2_Click()
   If Combo2.ListIndex < Combo2.ListCount - 1 Then Combo2.ListIndex = Combo2.ListIndex + 1
End Sub

Private Sub SSCommand3_Click()
   If Combo3.ListIndex > 0 Then Combo3.ListIndex = Combo3.ListIndex - 1
End Sub

Private Sub SSCommand4_Click()
   If Combo3.ListIndex < Combo3.ListCount - 1 Then Combo3.ListIndex = Combo3.ListIndex + 1
End Sub

Private Sub SSCommand5_Click()
Dim j As Long
  FluxTypes.Show 1
  sink(c1).media = Combo4.Text
  airLabels
  For j = 0 To Combo4.ListCount - 1
    If sink(c1).media = Combo4.list(j) Then Combo4.ListIndex = j
  Next
End Sub

Private Sub txt_GotFocus(Index As Integer)
  RefItem = Index
  noact.Enabled = True
End Sub

Private Sub unit_GotFocus(Index As Integer)
  RefItem = Index
  noact.Enabled = True
End Sub

Private Sub vaSpread1_GotFocus()
  RefItem = 6
End Sub

Private Sub vaSpread2_GotFocus()
  RefItem = 7
End Sub

Private Sub SetSpreadUnits(ss As Object, col As Long, unit As String)
  Dim lst As String
  lst = ""
  ss.Row = 1
  ss.col = col
  ss.Action = 26                     'clear the box
  get_conversion_list unit, lst
  ss.TypeComboBoxIndex = -1
  ss.TypeComboBoxList = lst
  ss.TypeComboBoxCurSel = 0
End Sub

Sub waterLabels()
  Dim j As Long
  vaSpread1.col = 2
  vaSpread1.Row = 0
  vaSpread1.Text = "Flux Rate"
  vaSpread2.col = 2
  vaSpread2.Row = 0
  vaSpread2.Text = "Flux Rate"
  For j = 1 To 2
    vaSpread1.col = j
    vaSpread1.ColWidth(j) = 10
    vaSpread1.ColHidden = False
    vaSpread2.col = j
    vaSpread2.ColWidth(j) = 10
    vaSpread2.ColHidden = False
  Next
  For j = 3 To 5
    vaSpread1.col = j
    vaSpread1.ColHidden = True
    vaSpread2.col = j
    vaSpread2.ColHidden = True
  Next
  vaSpread2.VisibleCols = 2
  lbl(9).Caption = "Type of contaminated medium"
  lbl(0).Caption = "Width of flux plane"
  If SSTab1.Tab = 1 Then
    lbl(1).Caption = "Height of flux plane"
  Else
    lbl(1).Caption = "Length of flux plane"
  End If
  lbl(2).Caption = "Distance below water surface"
  lbl(3).Caption = "Natural recharge rate"
  SSCommand5.Visible = False
  SSFrame2.Visible = True
  SetVisible 3, True
  SetVisible 4, False
  SetVisible 5, False
  Combo4.Clear
  Combo4.AddItem "Vadose"
  If Not havevad Then Combo4.AddItem "Aquifer"
  Combo4.AddItem "Surface Water"
End Sub

Sub airLabels()
  Dim j As Long
  vaSpread1.col = 2
  vaSpread1.Row = 0
  vaSpread1.Text = "Gas 1"
  vaSpread2.col = 2
  vaSpread2.Row = 0
  vaSpread2.Text = "Gas 1"
  For j = 2 To 5
    vaSpread1.col = j
    vaSpread1.ColWidth(j) = 10
    vaSpread1.ColHidden = Not (FluxTypes.SSCheck1(j - 2).Value)
    vaSpread2.col = j
    vaSpread2.ColWidth(j) = 10
    vaSpread2.ColHidden = Not (FluxTypes.SSCheck1(j - 2).Value)
  Next
  lbl(9).Caption = "Type of release"
  lbl(0).Caption = "Exit area of source"
  lbl(1).Caption = "Exit height of source"
  lbl(2).Caption = "Height of adjacent structure"
  lbl(3).Caption = "Exit velocity of source"
  lbl(4).Caption = "Exit temperature of source"
  lbl(5).Caption = "Ambient air temperature"
  SSCommand5.Visible = True
  SSFrame2.Visible = haveflux
  SetVisible 3, True
  SetVisible 4, True
  SetVisible 5, True
  Combo4.Clear
  Combo4.AddItem "Point"
  Combo4.AddItem "Area"
End Sub

Sub soilLabels()
  Dim j As Long
  vaSpread1.col = 2
  vaSpread1.Row = 0
  vaSpread1.Text = "Concentration"
  vaSpread2.col = 2
  vaSpread2.Row = 0
  vaSpread2.Text = "Concentration"
  For j = 1 To 2
    vaSpread1.col = j
    vaSpread1.ColWidth(j) = 10
    vaSpread1.ColHidden = False
    vaSpread2.col = j
    vaSpread2.ColWidth(j) = 10
    vaSpread2.ColHidden = False
  Next
  For j = 3 To 5
    vaSpread1.col = j
    vaSpread1.ColHidden = True
    vaSpread2.col = j
    vaSpread2.ColHidden = True
  Next
  vaSpread2.VisibleCols = 2
  lbl(9).Caption = "Type of contaminated medium"
  lbl(0).Caption = "Width of contaminated soil"
  lbl(1).Caption = "Length of contaminated soil"
  lbl(2).Caption = "Depth of contaminated soil"
  SSCommand5.Visible = False
  SSFrame2.Visible = True
  SetVisible 3, False
  SetVisible 4, False
  SetVisible 5, False
  Combo4.Clear
  Combo4.AddItem "Vadose"
'  If Not havevad Then Combo4.AddItem "Aquifer"
'  Combo4.AddItem "Surface Water"
End Sub

Sub SetVisible(x As Long, v As Boolean)
  lbl(x).Visible = v
  txt(x).Visible = v
  unit(x).Visible = v
  ref(x).Visible = v
End Sub

Sub SetEnabled(x As Long, v As Boolean)
  lbl(x).Enabled = v
  txt(x).Enabled = v
  unit(x).Enabled = v
  ref(x).Enabled = v
End Sub

