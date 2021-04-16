VERSION 5.00
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "TABCTL32.OCX"
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "THREED32.OCX"
Begin VB.Form gen_riv 
   BorderStyle     =   1  'Fixed Single
   ClientHeight    =   5265
   ClientLeft      =   7275
   ClientTop       =   3270
   ClientWidth     =   7680
   Icon            =   "GEN_RIV.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   NegotiateMenus  =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   351
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   512
   StartUpPosition =   2  'CenterScreen
   Begin TabDlg.SSTab SSTab1 
      Height          =   4935
      Left            =   0
      TabIndex        =   40
      Top             =   0
      Width           =   7695
      _ExtentX        =   13573
      _ExtentY        =   8705
      _Version        =   393216
      Tabs            =   2
      TabsPerRow      =   2
      TabHeight       =   520
      TabCaption(0)   =   "River/Lake"
      TabPicture(0)   =   "GEN_RIV.frx":030A
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "SSFrame2"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).ControlCount=   1
      TabCaption(1)   =   "Impoundment"
      TabPicture(1)   =   "GEN_RIV.frx":0326
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "SSFrame1"
      Tab(1).ControlCount=   1
      Begin Threed.SSFrame SSFrame2 
         Height          =   4455
         Left            =   120
         TabIndex        =   41
         Top             =   360
         Width           =   7455
         _Version        =   65536
         _ExtentX        =   13150
         _ExtentY        =   7858
         _StockProps     =   14
         ForeColor       =   16776960
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
         Begin VB.ComboBox Combo2 
            Height          =   315
            Left            =   3600
            Style           =   2  'Dropdown List
            TabIndex        =   9
            Tag             =   "fslocate"
            Top             =   1680
            Width           =   2652
         End
         Begin VB.TextBox txt 
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   2
            Left            =   4320
            TabIndex        =   13
            Tag             =   "wwwidth"
            Top             =   2520
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   1
            Left            =   4320
            TabIndex        =   10
            Tag             =   "wwdepth"
            Top             =   2160
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   4
            Left            =   4320
            TabIndex        =   19
            Tag             =   "wwdischg"
            Top             =   3240
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   3
            Left            =   4320
            TabIndex        =   16
            Tag             =   "wwdist"
            Top             =   2880
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   7
            Left            =   4320
            TabIndex        =   2
            Tag             =   "wwveloc"
            Top             =   840
            Width           =   1000
         End
         Begin VB.ComboBox Combo1 
            Height          =   315
            ItemData        =   "GEN_RIV.frx":0342
            Left            =   3600
            List            =   "GEN_RIV.frx":0352
            Style           =   2  'Dropdown List
            TabIndex        =   1
            Top             =   360
            Width           =   2652
         End
         Begin VB.TextBox txt 
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   8
            Left            =   4320
            TabIndex        =   5
            Tag             =   "wwveloc"
            Top             =   1200
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   5
            Left            =   4320
            TabIndex        =   22
            Tag             =   "wwdischg"
            Top             =   3600
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   6
            Left            =   4320
            TabIndex        =   25
            Tag             =   "wwdischg"
            Top             =   3960
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   315
            Index           =   2
            Left            =   5280
            Style           =   2  'Dropdown List
            TabIndex        =   14
            Tag             =   "ft"
            Top             =   2520
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   315
            Index           =   1
            Left            =   5280
            Style           =   2  'Dropdown List
            TabIndex        =   11
            Tag             =   "ft"
            Top             =   2160
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   315
            Index           =   4
            Left            =   5280
            Style           =   2  'Dropdown List
            TabIndex        =   20
            Tag             =   "ft^3/sec"
            Top             =   3240
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   315
            Index           =   3
            Left            =   5280
            Style           =   2  'Dropdown List
            TabIndex        =   17
            Tag             =   "ft"
            Top             =   2880
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   315
            Index           =   5
            Left            =   5280
            Style           =   2  'Dropdown List
            TabIndex        =   23
            Tag             =   "ft^3/sec"
            Top             =   3600
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   315
            Index           =   6
            Left            =   5280
            Style           =   2  'Dropdown List
            TabIndex        =   26
            Tag             =   "ft^3/sec"
            Top             =   3960
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   315
            Index           =   7
            Left            =   5280
            Style           =   2  'Dropdown List
            TabIndex        =   3
            Tag             =   "ft/sec"
            Top             =   840
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   315
            Index           =   8
            Left            =   5280
            Style           =   2  'Dropdown List
            TabIndex        =   6
            Tag             =   "ft/sec"
            Top             =   1200
            Width           =   1000
         End
         Begin VB.Label lbl 
            Height          =   255
            Index           =   1
            Left            =   120
            TabIndex        =   49
            Top             =   2160
            Width           =   4200
         End
         Begin VB.Label lbl 
            Height          =   255
            Index           =   3
            Left            =   120
            TabIndex        =   48
            Top             =   2880
            Width           =   4200
         End
         Begin VB.Label lbl 
            Height          =   255
            Index           =   4
            Left            =   120
            TabIndex        =   47
            Top             =   3240
            Width           =   4200
         End
         Begin VB.Label clbl 
            Caption         =   "Usage Location"
            Height          =   252
            Index           =   2
            Left            =   120
            TabIndex        =   8
            Top             =   1680
            Width           =   3492
         End
         Begin VB.Label lbl 
            Height          =   255
            Index           =   7
            Left            =   120
            TabIndex        =   46
            Top             =   840
            Width           =   4200
         End
         Begin VB.Label clbl 
            Caption         =   "Type of release and body of water"
            Height          =   252
            Index           =   1
            Left            =   120
            TabIndex        =   0
            Top             =   360
            Width           =   3492
         End
         Begin VB.Label lbl 
            Height          =   255
            Index           =   8
            Left            =   120
            TabIndex        =   45
            Top             =   1200
            Width           =   4200
         End
         Begin VB.Label lbl 
            Height          =   255
            Index           =   5
            Left            =   120
            TabIndex        =   44
            Top             =   3600
            Width           =   4200
         End
         Begin VB.Label lbl 
            Height          =   255
            Index           =   6
            Left            =   120
            TabIndex        =   43
            Top             =   3960
            Width           =   4200
         End
         Begin VB.Label lbl 
            Height          =   255
            Index           =   2
            Left            =   120
            TabIndex        =   42
            Top             =   2520
            Width           =   4200
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   2
            Left            =   6360
            TabIndex        =   15
            Tag             =   "0"
            Top             =   2520
            Width           =   990
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   1
            Left            =   6360
            TabIndex        =   12
            Tag             =   "0"
            Top             =   2160
            Width           =   990
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   4
            Left            =   6360
            TabIndex        =   21
            Tag             =   "0"
            Top             =   3240
            Width           =   990
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   3
            Left            =   6360
            TabIndex        =   18
            Tag             =   "0"
            Top             =   2880
            Width           =   990
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   5
            Left            =   6360
            TabIndex        =   24
            Tag             =   "0"
            Top             =   3600
            Width           =   990
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   6
            Left            =   6360
            TabIndex        =   27
            Tag             =   "0"
            Top             =   3960
            Width           =   990
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   7
            Left            =   6360
            TabIndex        =   4
            Top             =   840
            Width           =   990
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   8
            Left            =   6360
            TabIndex        =   7
            Top             =   1200
            Width           =   990
         End
      End
      Begin Threed.SSFrame SSFrame1 
         Height          =   4335
         Left            =   -74880
         TabIndex        =   50
         Top             =   360
         Width           =   7455
         _Version        =   65536
         _ExtentX        =   13145
         _ExtentY        =   7641
         _StockProps     =   14
         ForeColor       =   16776960
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
         Begin VB.ComboBox unit 
            Height          =   315
            Index           =   16
            Left            =   5280
            Style           =   2  'Dropdown List
            TabIndex        =   37
            Top             =   1560
            Visible         =   0   'False
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   16
            Left            =   4320
            TabIndex        =   36
            Tag             =   "wwveloc"
            Top             =   1560
            Visible         =   0   'False
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   315
            Index           =   15
            Left            =   5280
            Style           =   2  'Dropdown List
            TabIndex        =   34
            Top             =   1200
            Visible         =   0   'False
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   315
            Index           =   14
            Left            =   5280
            Style           =   2  'Dropdown List
            TabIndex        =   31
            Top             =   840
            Visible         =   0   'False
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   15
            Left            =   4320
            TabIndex        =   33
            Tag             =   "wwveloc"
            Top             =   1200
            Visible         =   0   'False
            Width           =   1000
         End
         Begin VB.ComboBox Combo4 
            Height          =   315
            ItemData        =   "GEN_RIV.frx":03A2
            Left            =   3600
            List            =   "GEN_RIV.frx":03B2
            Style           =   2  'Dropdown List
            TabIndex        =   29
            Top             =   360
            Width           =   2652
         End
         Begin VB.TextBox txt 
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   14
            Left            =   4320
            TabIndex        =   30
            Tag             =   "wwveloc"
            Top             =   840
            Visible         =   0   'False
            Width           =   1000
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   16
            Left            =   6360
            TabIndex        =   38
            Top             =   1560
            Visible         =   0   'False
            Width           =   990
         End
         Begin VB.Label lbl 
            Height          =   255
            Index           =   16
            Left            =   120
            TabIndex        =   53
            Top             =   1560
            Visible         =   0   'False
            Width           =   4200
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   15
            Left            =   6360
            TabIndex        =   35
            Top             =   1200
            Visible         =   0   'False
            Width           =   990
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   14
            Left            =   6360
            TabIndex        =   32
            Top             =   840
            Visible         =   0   'False
            Width           =   990
         End
         Begin VB.Label lbl 
            Height          =   255
            Index           =   15
            Left            =   120
            TabIndex        =   52
            Top             =   1200
            Visible         =   0   'False
            Width           =   4200
         End
         Begin VB.Label clbl 
            Caption         =   "Type of impoundment"
            Height          =   252
            Index           =   3
            Left            =   120
            TabIndex        =   28
            Top             =   360
            Width           =   3492
         End
         Begin VB.Label lbl 
            Height          =   255
            Index           =   14
            Left            =   120
            TabIndex        =   51
            Top             =   840
            Visible         =   0   'False
            Width           =   4200
         End
      End
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
      Height          =   360
      Left            =   0
      Locked          =   -1  'True
      TabIndex        =   39
      TabStop         =   0   'False
      Top             =   4920
      Width           =   7680
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
   Begin VB.Menu howto 
      Caption         =   "&Help"
   End
End
Attribute VB_Name = "gen_riv"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Dim reltype As Long
Dim imptype As Long
Dim temp As parmrec
Dim loadng As Boolean
Dim ctext1 As String
' All these just ro resolve indexs from fui to mui
Dim m_numexp As Long, exploc() As locparm
Dim numloc As Long
Dim loc() As snk

Private Sub howto_Click()
  GetHelp
End Sub

Private Sub selref_Click()
  RefMode = 0
  GetRef ref(RefItem)
End Sub

Private Sub addref_Click()
  RefMode = 1
  GetRef ref(RefItem)
End Sub

Private Sub elocfillet(idx As Long)
  If temp.idx3 > m_numexp Then
    m_numexp = temp.idx1
    ReDim Preserve exploc(m_numexp) As locparm
  End If
  exploc(temp.idx1).parms(idx).org = convert(temp.cunit, temp.uunit, Val(temp.pval))
  exploc(temp.idx1).parms(idx).uunt = temp.uunit
  exploc(temp.idx1).parms(idx).punt = temp.cunit
  exploc(temp.idx1).parms(idx).ref = temp.ref
End Sub

Private Sub fillet(idx As Long)
  ref(idx).Tag = temp.ref
  ref(idx).Caption = "Ref:" & Str(temp.ref)
  On Error Resume Next
  If temp.cunit <> "N/A" Then set_unit unit(idx), temp.uunit
  txt(idx).Text = convert(temp.cunit, temp.uunit, Val(temp.pval))
End Sub

Private Sub fillexploc(idx As Long, units As String)
  exploc(m_numexp).parms(idx).uunt = units
  exploc(m_numexp).parms(idx).punt = units
  exploc(m_numexp).parms(idx).ref = 0
  unit(idx).Clear
  unit(idx).Tag = units
  get_conversion_items units, unit(idx)
End Sub

Private Sub SizeLocal()
  If temp.idx3 > numloc Then
    numloc = temp.idx3
    ReDim Preserve loc(numloc) As snk
  End If
End Sub

Private Sub loadprm()
  Dim i As Long, j As Long, k As Long, m As Long
  Dim mx As Long
  Dim prm As String
  Dim prefix As String
  Dim fle As parmfile
  Dim sval As Boolean
  
  m_numexp = 0
 
  If open_parm(fle, FUIName, 2) Then
    Do Until EOCF(fle.file)
      If read_parmrec(fle, temp) Then
        Select Case temp.pName
          Case modName
            'loading is order dependant GID must contain order that is written by save routine
            Loading.Gauge1.Max = Val(temp.idx1)
            Loading.Gauge1.Value = 0
            For m = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                If Right(temp.cunit, 2) = "/s" Then temp.cunit = Replace(temp.cunit, "/s", "/sec")
                If Right(temp.uunit, 2) = "/s" Then temp.uunit = Replace(temp.uunit, "/s", "/sec")
                updategauge
                Select Case temp.pName
                  Case "gnswexpname"
                    If temp.idx1 > m_numexp Then
                      m_numexp = temp.idx1
                      ReDim Preserve exploc(m_numexp) As locparm
                    End If
                    exploc(temp.idx1).Name = temp.pval
                    exploc(temp.idx1).idx = 0
                  Case "gnswtt":      elocfillet 1
                  Case "gnswdschg":   elocfillet 2
                  Case "gnswveloc":   elocfillet 2
                  Case "gnswlsx":     elocfillet 3
                  Case "gnswtype":    reltype = Val(temp.pval)
                                      Combo1.ListIndex = reltype
                  Case "gnswosy"
                    If reltype = 1 Then
                      elocfillet 6
                    Else
                      elocfillet 4
                    End If
                  Case "gnswdpth":    elocfillet 5
                  Case "gnswidth":    elocfillet 4
                  Case "gnswtrel":    fillet 7
                  Case "gnswzdep":    fillet 8
                  
                  Case "gnipond":      imptype = Val(temp.pval)
                                       Combo4.ListIndex = imptype
                  Case "gnpondvol":    fillet 14
                  Case "gnpondout":    fillet 15
                  Case "gnpondyrs":    fillet 16
                  Case "gnponddis":    fillet 16
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
'                    Case "moddespath"
'                      DesName = temp.pval
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

    For i = 1 To numloc
      For j = 1 To m_numexp
        If exploc(j).Name = loc(i).Name Then
          exploc(j).idx = i
          exploc(j).label = loc(i).lbl
          Exit For
        End If
      Next
      If j > m_numexp And InStr("wcf", loc(i).type) Mod 3 = 1 Then
        m_numexp = m_numexp + 1
        ReDim Preserve exploc(m_numexp) As locparm
        exploc(m_numexp).idx = i
        exploc(m_numexp).Name = loc(i).Name
        exploc(m_numexp).label = loc(i).lbl
        fillexploc 1, "hr"
        If reltype = 0 Or reltype = 3 Then
          fillexploc 2, "m^3/sec"
        Else
          fillexploc 2, "m/sec"
          fillexploc 3, "m"
          fillexploc 4, "m"
          fillexploc 5, "m"
          fillexploc 6, "m"
        End If
      End If
    Next
    For i = 1 To m_numexp
      If exploc(i).idx > 0 Then Combo2.AddItem exploc(i).label + " (" + exploc(i).Name + ")"
    Next
    close_parm fle
  Else
    put_val errfile, "Can't find or open file " & FUIName
    put_line errfile
    close_csv errfile
    End
  End If
End Sub

Private Sub Form_load()
  Dim i As Long
  
  GetArguments
  If argc >= 5 Then
    Loading.Show
    FUIName = argv(0) & ".GID"
    RunName = argv(1)
    siteIdx = Val(argv(2))
    modName = argv(4)
    SetForm Me
    SetRefFile ReplaceExt(FUIName, "ref")
    SetHelpFile App.Path + "\genrivhp.htm"
    gen_riv.Caption = "GENII Surface Water Module - " + modName
    
    If open_csv(errfile, RunName & ".ERR", 1) Then
      put_val errfile, "Error report for GENII Surface Water Module"
      put_line errfile
    Else
      MsgBox "Unable to create file " & RunName & ".ERR" & Chr(10) & "Check directory permessions"
      End
    End If
    
    load_convert
    reltype = 0
    imptype = 0
    Combo4.ListIndex = imptype
    loadng = True
    loadprm
    Unload Loading
    If Combo1.ListIndex = -1 Then Combo1.ListIndex = reltype
    Combo4.ListIndex = imptype
    loadng = False
    If Combo2.ListCount = 0 Then
      MsgBox "Not connected to an exposure location"
      close_csv errfile
      End
    Else
      Combo2.ListIndex = 0
    End If
  Else
    MsgBox "Not enough arguments passed" & Chr(10) & "Contact PNNL"
    close_csv errfile
    End
  End If
End Sub

Private Sub leave_Click()
  close_csv errfile
  Kill RunName & ".ERR"
  End
End Sub

Private Sub save_Click()
  Dim i As Long, j As Long, cnt As Long
  Dim fName As String
  Dim parm As parmrec
  Dim fle As parmfile
  Dim tmp As parmfile
  Dim errflag As Boolean
  
  errflag = False
  Combo2_Click
  fName = RunName & ".GID"
  If open_parm(fle, fName, 1) Then
      
      
    set_parm parm, "GNIPond", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", Str(Combo4.ListIndex)
    write_parmrec fle, parm
    If Combo4.ListIndex > 0 Then
      If er(14) Then errflag = True
      set_parm parm, txt(14).Tag, 0, 0, 0, 0, 0, 0, Val(ref(14).Tag), unit(14).Text, unit(14).Tag, convert(unit(14).Text, unit(14).Tag, Val(txt(14)))
      write_parmrec fle, parm
      If er(15) Then errflag = True
      set_parm parm, txt(15).Tag, 0, 0, 0, 0, 0, 0, Val(ref(15).Tag), unit(15).Text, unit(15).Tag, convert(unit(15).Text, unit(15).Tag, Val(txt(15)))
      write_parmrec fle, parm
      If Combo4.ListIndex = 2 Then
        If er(16) Then errflag = True
        set_parm parm, txt(16).Tag, 0, 0, 0, 0, 0, 0, Val(ref(16).Tag), unit(16).Text, unit(16).Tag, convert(unit(16).Text, unit(16).Tag, Val(txt(16)))
        write_parmrec fle, parm
      End If
      If Combo4.ListIndex = 3 Then
        If er(16) Then errflag = True
        set_parm parm, txt(16).Tag, 0, 0, 0, 0, 0, 0, Val(ref(16).Tag), unit(16).Text, unit(16).Tag, convert(unit(16).Text, unit(16).Tag, Val(txt(16)))
        write_parmrec fle, parm
      End If
    End If
      
    set_parm parm, "GNSWType", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", Str(Combo1.ListIndex)
    write_parmrec fle, parm
    If Combo1.ListIndex = 0 Then
      set_parm parm, "GNSWRelType", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "Acute"
      write_parmrec fle, parm
    Else
      set_parm parm, "GNSWRelType", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "Chronic"
      write_parmrec fle, parm
    End If
    If Combo1.ListIndex = 2 Then
      set_parm parm, "GNSWWaterType", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "Near-shore lake"
      write_parmrec fle, parm
    Else
      set_parm parm, "GNSWWaterType", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "River"
      write_parmrec fle, parm
    End If
    
    If er(7) Then errflag = True
    set_parm parm, txt(7).Tag, 0, 0, 0, 0, 0, 0, Val(ref(7).Tag), unit(7).Text, unit(7).Tag, convert(unit(7).Text, unit(7).Tag, Val(txt(7)))
    write_parmrec fle, parm
    If Combo1.ListIndex = 2 Then
      If er(8) Then errflag = True
      set_parm parm, txt(8).Tag, 0, 0, 0, 0, 0, 0, Val(ref(8).Tag), unit(8).Text, unit(8).Tag, convert(unit(8).Text, unit(8).Tag, Val(txt(8)))
      write_parmrec fle, parm
    End If
    
    cnt = 0
    set_parm parm, "GNSWExpNum", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CStr(m_numexp)
    write_parmrec fle, parm
    
    For i = 1 To m_numexp
      If exploc(i).idx > 0 Then
        cnt = cnt + 1
        set_parm parm, "GNSWEXPNAME", cnt, 0, 0, 0, 0, 0, 0, "N/A", "N/A", exploc(i).Name
        write_parmrec fle, parm
        For j = 1 To Combo1.ItemData(Combo1.ListIndex)
          set_parm parm, txt(j).Tag, cnt, 0, 0, 0, 0, 0, exploc(i).parms(j).ref, exploc(i).parms(j).uunt, exploc(i).parms(j).punt, convert(exploc(i).parms(j).uunt, exploc(i).parms(j).punt, exploc(i).parms(j).org)
          write_parmrec fle, parm
          txt(j).Text = exploc(i).parms(j).org
          set_unit unit(j), exploc(i).parms(j).uunt
          If er(i) Then errflag = True
        Next
      End If
    Next
    close_parm fle
    If errflag Then
'      MsgBox "Saving incomplete data!", 64, "Important Notice!"
      put_val errfile, "Incomplete data entry!"
      put_line errfile
      close_csv errfile
    Else
      close_csv errfile
      Kill RunName & ".ERR"
    End If
  Else
    put_val errfile, "Unable to create transaction file" & RunName & ".GID"
    put_line errfile
    close_csv errfile
  End If
  End
End Sub

Private Function er(Index As Long) As Boolean
  Dim tval As Double
  Dim t1 As String
  Dim t2 As String
  
  mes = ""
  er = False
  If txt(Index).Text = "" Then er = True
  tval = Val(txt(Index).Text)
  Select Case Index
    Case 1
      t2 = convert(unit(Index).Tag, unit(Index).Text, 10000)
      mes = "Value must be >= 0 and < " + t2 + " " + unit(Index).Text + "(s)."
      If (tval < 0 Or tval >= Val(t2)) Then
        er = True
      End If
    Case 2
      If Combo1.ListIndex = 0 Or Combo1.ListIndex = 3 Then
        t2 = convert(unit(Index).Tag, unit(Index).Text, 50000)
      Else
        t2 = convert(unit(Index).Tag, unit(Index).Text, 10)
      End If
      mes = "Value must be > 0 and <= " + t2 + " " + unit(Index).Text + "(s)."
      If (tval <= 0 Or tval > Val(t2)) Then
        er = True
      End If
    Case 3
      t2 = convert(unit(Index).Tag, unit(Index).Text, 5000000)
      mes = "Value must be > 0 and <= " + t2 + " " + unit(Index).Text + "(s)."
      If (tval <= 0 Or tval > Val(t2)) Then
        er = True
      End If
    Case 4
      t2 = convert(unit(Index).Tag, unit(Index).Text, 10000)
      mes = "Value must be > 0 and <= " + t2 + " " + unit(Index).Text + "(s)."
      If (tval <= 0 Or tval > Val(t2)) Then
        er = True
      End If
    Case 5
      t2 = convert(unit(Index).Tag, unit(Index).Text, 100)
      mes = "Value must be  > 0 and <=  " + t2 + " " + unit(Index).Text + "(s)."
      If (tval <= 0 Or tval > Val(t2)) Then
        er = True
      End If
    Case 6
      If Combo1.ListIndex = 1 Then
        t2 = convert(unit(4).Text, unit(Index).Text, Val(txt(4).Text))
        mes = "Value must be  > 0 and <= River Width " + unit(Index).Text + "(s)."
      Else
        t2 = convert(unit(Index).Tag, unit(Index).Text, 10000)
        mes = "Value must be  > 0 and <= " + t2 + " " + unit(Index).Text + "(s)."
      End If
      If (tval < 0 Or tval > Val(t2)) Then
        er = True
      End If
    Case 7, 15, 16
      t2 = convert(unit(Index).Tag, unit(Index).Text, 1000000)
      mes = "Value must be  > 0 and <= " + t2 + " " + unit(Index).Text + "(s)."
      If (tval <= 0 Or tval > Val(t2)) Then
        er = True
      End If
    Case 14
      t2 = convert(unit(Index).Tag, unit(Index).Text, 10000000000#)
      mes = "Value must be  > 0 and <= " + t2 + " " + unit(Index).Text + "(s)."
      If (tval <= 0 Or tval > Val(t2)) Then
        er = True
      End If
    Case 8
      If Combo1.ListIndex = 0 Then
        mes = "Value must be > 0"
        If (tval <= 0) Then
          er = True
        End If
      Else
        t1 = convert(unit(Index).Tag, unit(Index).Text, 0)
        t2 = convert(unit(Index).Tag, unit(Index).Text, 100)
        mes = "Value must be  > 0 and <= " + t2 + " " + unit(Index).Text + "(s)."
        If (tval <= Val(t1) Or tval > Val(t2)) Then
          er = True
        End If
      End If
  End Select
  If er Then
    txt(Index).BackColor = &H8080FF
  Else
    txt(Index).BackColor = &HC0FFC0
  End If
End Function

Private Sub txt_GotFocus(Index As Integer)
  er CLng(Index)
  RefItem = Index
  noact.Enabled = True
  HelpAnchor = txt(Index).Tag
End Sub

Private Sub txt_LostFocus(Index As Integer)
  Select Case Index
    Case 1 To 4:      Combo2_Click
  End Select
End Sub

Private Sub txt_Change(Index As Integer)
Dim chk As Double
On Error GoTo toolarge
  If Index = 6 Then er 4
  er CLng(Index)
  chk = CDbl(txt(Index).Text)
  Exit Sub
toolarge:
  txt(Index).BackColor = &H8080FF
End Sub

Private Sub unit_GotFocus(Index As Integer)
  mes = ""
  RefItem = Index
  noact.Enabled = True
  HelpAnchor = txt(Index).Tag
End Sub

Private Sub unit_Click(Index As Integer)
  er CLng(Index)
End Sub

Private Sub elocchange(tx As String, oldtx As String)
  Dim i As Long, j As Long, k As Long
  For i = 1 To m_numexp
    If exploc(i).label + " (" + exploc(i).Name + ")" = oldtx And Not loadng Then
      For j = 1 To Combo1.ItemData(Combo1.ListIndex)
         exploc(i).parms(j).org = Val(txt(j).Text)
         exploc(i).parms(j).uunt = unit(j).Text
         exploc(i).parms(j).punt = unit(j).Tag
         exploc(i).parms(j).ref = Val(ref(j).Tag)
      Next
      Exit For
    End If
  Next
  For i = 1 To m_numexp
    If exploc(i).label + " (" + exploc(i).Name + ")" = tx Then
      loadng = True
      For j = 1 To Combo1.ItemData(Combo1.ListIndex)
         unit(j).Tag = exploc(i).parms(j).punt
         set_unit unit(j), exploc(i).parms(j).uunt
         txt(j).Text = exploc(i).parms(j).org
         ref(j).Caption = "Ref: " & exploc(i).parms(j).ref
         ref(j).Tag = exploc(i).parms(j).ref
      Next
      loadng = False
      Exit For
    End If
  Next
End Sub

Private Sub hideit(i As Long)
  lbl(i).Visible = False
  unit(i).Visible = False
  txt(i).Visible = False
  ref(i).Visible = False
End Sub

Private Sub shwit(i As Long, msg As String, haveunits As Boolean, units As String, pName As String)
  Dim j As Long
  lbl(i).Caption = msg
  lbl(i).Visible = True
  unit(i).Visible = haveunits
  If haveunits Then
    unit(i).Clear
    unit(i).Tag = units
    If i < 7 And Not loadng Then
      For j = 1 To m_numexp
        exploc(j).parms(i).org = 0
        exploc(j).parms(i).uunt = units
        exploc(j).parms(i).punt = units
        exploc(j).parms(i).ref = 0
      Next
    End If
    get_conversion_items units, unit(i)
  End If
  If Not loadng Then txt(i).Text = ""
  txt(i).Tag = pName
  txt(i).Visible = True
  ref(i).Caption = "Ref: 0"
  ref(i).Tag = 0
  ref(i).Visible = True
End Sub

Private Sub Combo1_Click()
  Dim i As Long
  If reltype = Combo1.ListIndex And Not loadng Then
    Exit Sub
  Else
    reltype = Combo1.ListIndex
  End If
  shwit 1, "Travel time in surface water", True, "hr", "GNSWTT"
  shwit 7, "Duration of the release to the surface water", True, "yr", "GNSWTREL"
  Select Case Combo1.ListIndex
  Case 0, 3
    shwit 2, "Total volumetric flow rate of river for location", True, "m^3/sec", "GNSWDSCHG"
    hideit 8
    For i = 3 To 6
      hideit i
    Next
  Case 1
    shwit 2, "Average river flow velocity", True, "m/sec", "GNSWVELOC"
    shwit 3, "Downstream distance to exposure location", True, "m", "GNSWLSX"
    shwit 4, "Constant river width", True, "m", "GNSWIDTH"
    shwit 5, "Constant river flow depth", True, "m", "GNSWDPTH"
    shwit 6, "Offshore distance to water intake at exposure location", True, "m", "GNSWOSY"
    hideit 8
  Case 2
    shwit 8, "Depth of the usage intake in the water body", True, "m", "GNSWZDEP"
    shwit 2, "Average long-shore flow velocity", True, "m/sec", "GNSWVELOC"
    shwit 3, "Long-shore distance to exposure point", True, "m", "GNSWLSX"
    shwit 4, "Offshore distance to water intake for exposure point", True, "m", "GNSWOSY"
    shwit 5, "Constant long-shore flow depth", True, "m", "GNSWDPTH"
    hideit 6
  End Select
End Sub

Private Sub Combo2_Click()
  elocchange Combo2.Text, ctext1
  ctext1 = Combo2.Text
End Sub

Private Sub Combo4_Click()
  Dim i As Long
  If imptype = Combo4.ListIndex And Not loadng Then
    Exit Sub
  Else
    imptype = Combo4.ListIndex
  End If
  Select Case Combo4.ListIndex
  Case 0
    hideit 14
    hideit 15
    hideit 16
  Case 1
    shwit 14, "Effective volume", True, "m^3", "GNPONDVOL"
    shwit 15, "Pond outflow rate", True, "m^3/sec", "GNPONDOUT"
    hideit 16
  Case 2
    shwit 14, "Effective volume", True, "m^3", "GNPONDVOL"
    shwit 15, "Pond outflow rate", True, "m^3/sec", "GNPONDOUT"
    shwit 16, "Years of prior operation", True, "yr", "GNPONDYRS"
  Case 3
    shwit 14, "Effective volume", True, "m^3", "GNPONDVOL"
    shwit 15, "Pond outflow rate", True, "m^3/sec", "GNPONDOUT"
    shwit 16, "Plant discharge", True, "m^3/sec", "GNPONDDIS"
  End Select
End Sub
Private Sub Combo1_GotFocus()
  mes = ""
  noact.Enabled = False
End Sub

Private Sub Combo2_GotFocus()
  mes = ""
  noact.Enabled = False
End Sub

Private Sub Combo4_GotFocus()
  mes = ""
  noact.Enabled = False
End Sub

Private Sub form_KeyPress(KeyAscii As Integer)
  Select Case KeyAscii
    Case 48 To 57, 43, 45, 46, 69, 101, 8:
    Case Else: KeyAscii = 0
  End Select
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
