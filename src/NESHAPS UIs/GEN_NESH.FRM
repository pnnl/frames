VERSION 5.00
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "tabctl32.ocx"
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "threed32.ocx"
Begin VB.Form STRUCTURE 
   BorderStyle     =   1  'Fixed Single
   ClientHeight    =   5160
   ClientLeft      =   4212
   ClientTop       =   3072
   ClientWidth     =   7704
   Icon            =   "GEN_NESH.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form2"
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   MinButton       =   0   'False
   NegotiateMenus  =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   430
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   642
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
      TabIndex        =   5
      TabStop         =   0   'False
      Top             =   4800
      Width           =   7680
   End
   Begin TabDlg.SSTab SSTab1 
      Height          =   4812
      Left            =   0
      TabIndex        =   4
      Top             =   0
      Width           =   7692
      _ExtentX        =   13568
      _ExtentY        =   8488
      _Version        =   393216
      Style           =   1
      TabHeight       =   529
      TabCaption(0)   =   "General Formatting"
      TabPicture(0)   =   "GEN_NESH.frx":030A
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "frm(0)"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).ControlCount=   1
      TabCaption(1)   =   "Site/User Identification"
      TabPicture(1)   =   "GEN_NESH.frx":0326
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "frm(1)"
      Tab(1).ControlCount=   1
      TabCaption(2)   =   "Population File Name"
      TabPicture(2)   =   "GEN_NESH.frx":0342
      Tab(2).ControlEnabled=   0   'False
      Tab(2).Control(0)=   "frm(2)"
      Tab(2).ControlCount=   1
      Begin Threed.SSFrame frm 
         Height          =   4092
         Index           =   0
         Left            =   240
         TabIndex        =   6
         Top             =   480
         Width           =   7212
         _Version        =   65536
         _ExtentX        =   12721
         _ExtentY        =   7218
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
         Begin VB.OptionButton RType 
            Caption         =   "Include Atmospheric Dispersion and Deposition Estimates"
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
            Caption         =   "Include Population Dose/Risk Estimates (requires a file of population distribution around the release site)"
            Height          =   612
            Index           =   1
            Left            =   240
            TabIndex        =   1
            Tag             =   "CETOXT"
            Top             =   1080
            Width           =   5000
         End
         Begin VB.OptionButton RType 
            Caption         =   "Provide only Individual Dose/Risk Results"
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
         Left            =   -74760
         TabIndex        =   7
         Top             =   480
         Width           =   7212
         _Version        =   65536
         _ExtentX        =   12721
         _ExtentY        =   7302
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
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   6
            Left            =   500
            TabIndex        =   3
            Tag             =   "PFilNam"
            Text            =   "C:\FRAMES\Popfile.csv"
            Top             =   1800
            Width           =   6000
         End
         Begin VB.Label Label2 
            Caption         =   $"GEN_NESH.frx":035E
            Height          =   500
            Left            =   240
            TabIndex        =   8
            Top             =   420
            Width           =   6000
         End
      End
      Begin Threed.SSFrame frm 
         Height          =   4092
         Index           =   1
         Left            =   -74760
         TabIndex        =   9
         Top             =   480
         Width           =   7212
         _Version        =   65536
         _ExtentX        =   12721
         _ExtentY        =   7218
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
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   0
            Left            =   500
            TabIndex        =   13
            Tag             =   "FacNam"
            Text            =   "Facility Name"
            Top             =   600
            Width           =   6000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   1
            Left            =   500
            TabIndex        =   12
            Tag             =   "FacStrt"
            Text            =   "Street Address"
            Top             =   1300
            Width           =   6000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   4
            Left            =   500
            TabIndex        =   11
            Tag             =   "FacCity"
            Text            =   "City, State, ZIP"
            Top             =   2000
            Width           =   6000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   5
            Left            =   500
            TabIndex        =   10
            Tag             =   "UsrNam"
            Text            =   "User Name"
            Top             =   2800
            Width           =   6000
         End
         Begin VB.Label lbl 
            Caption         =   "Input Facility Name"
            Height          =   372
            Index           =   3
            Left            =   300
            TabIndex        =   19
            Top             =   300
            Width           =   3492
         End
         Begin VB.Label lbl 
            Caption         =   "Input Facility Mailing Address"
            Height          =   372
            Index           =   2
            Left            =   300
            TabIndex        =   18
            Top             =   1000
            Width           =   3492
         End
         Begin VB.Label lbl 
            Height          =   252
            Index           =   1
            Left            =   480
            TabIndex        =   17
            Tag             =   "hcom"
            Top             =   1200
            Visible         =   0   'False
            Width           =   1932
         End
         Begin VB.Label lbl 
            Height          =   252
            Index           =   0
            Left            =   480
            TabIndex        =   16
            Tag             =   "hinc"
            Top             =   600
            Visible         =   0   'False
            Width           =   1932
         End
         Begin VB.Label lbl 
            Caption         =   "Input Facility City, State, ZIP Code"
            Height          =   372
            Index           =   4
            Left            =   240
            TabIndex        =   15
            Top             =   1700
            Width           =   3492
         End
         Begin VB.Label lbl 
            Caption         =   "Input User Name"
            Height          =   372
            Index           =   5
            Left            =   240
            TabIndex        =   14
            Top             =   2500
            Width           =   3492
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
Attribute VB_Name = "STRUCTURE"
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
' All these just for resolve indexs from fui to mui
Dim f_numcon As Long, con() As contam
Dim m_numcon As Long, cas() As casparm

Private Sub showprog(vis As Boolean)
  lbl(11).Visible = vis
'  d_sol.Visible = vis
'  SSCommand3.Visible = vis
'  SSCommand4.Visible = vis
'  Label3.Visible = vis
'  Combo3.Visible = vis
End Sub

Private Sub Combo2_Click()
  Dim i As Long
  Dim j As Long
  
'  If Combo3.Visible Then Combo3_Click
'  caschange Combo2.Text, ctext2
'  ctext2 = Combo2.Text
'  Combo3.Clear
  For i = 1 To m_numcon
    If con(cas(i).idx).name = ctext2 Then
      If cas(i).numprog = 0 Then
        showprog False
      Else
'        For j = 1 To cas(i).numprog
'          Combo3.AddItem con(cas(i).idx).progeny(cas(i).progeny(j).idx).name
'         Next
        showprog True
'        Combo3.ListIndex = 0
      End If
      Exit For
    End If
  Next
End Sub

Private Sub Combo3_Click()
'  d_caschange Combo3.Text, ctext3
'  ctext3 = Combo3.Text
End Sub

Private Sub Form_Unload(Cancel As Integer)
  Dim answer As Long
  If Not called Then
    answer = MsgBox("Do you want save changes?", 51, "GENII NESHAPS Report Generator")
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
'  GetRef ref(RefItem)
End Sub

Private Sub addref_Click()
  RefMode = 1
'  GetRef ref(RefItem)
End Sub

Private Sub howto_Click()
  GetHelp
End Sub

Private Sub fillet(idx As Long)
'  ref(idx).Tag = temp.ref
'  ref(idx).Caption = "Ref:" & Str(temp.ref)
  On Error Resume Next
'  If temp.cunit <> "N/A" Then set_unit unit(idx), temp.uunit
  txt(idx).Text = temp.pval
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
  Dim m As Long
  Dim i As Long
  Dim j As Long
  Dim k As Long
  Dim fle As parmfile
  Dim sval As Boolean
  
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
                  Select Case temp.pname
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
                    Case "fscname"
                      addcon
                      If temp.idx3 = 0 Then
                        con(temp.idx2).name = temp.pval
                      Else
                        con(temp.idx2).progeny(temp.idx3).name = temp.pval
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
                Select Case temp.pname
                  Case "METHOD"
                    RType(Val(temp.pval)).Value = True
                  Case "HEINC"
                    sval = False
                    If temp.pval = "true" Then sval = True
    '                SSCheck1(0).value = sval
                    SSCheck1_Click 0, CInt(sval)
                  Case "HEFAT"
                    sval = False
                    If temp.pval = "true" Then sval = True
    '                SSCheck1(1).value = sval
                    SSCheck1_Click 1, CInt(sval)
                  Case "HECEDE"
                    sval = False
                    If temp.pval = "true" Then sval = True
     '               SSCheck1(2).value = sval
                    SSCheck1_Click 2, CInt(sval)
                  Case "FacNam":    fillet 0
                  Case "FacStrt":    fillet 1
                  Case "FacCity":       fillet 4
                  Case "UsrNam":        fillet 5
                  Case "PFilNam":       fillet 6
                  Case "CASID"
                    addcas
                    If temp.idx2 = 0 Then
                      cas(temp.idx1).cas = temp.pval
                    Else
                      cas(temp.idx1).progeny(temp.idx2).cas = temp.pval
                    End If

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
  '      cas(m_numcon).unt(0) = unit(6).Tag
   '     cas(m_numcon).unt(1) = unit(7).Tag
        cas(m_numcon).sol = -1
      End If
    Next
'    For i = 1 To m_numcon
 '     If cas(i).idx > 0 And con(cas(i).idx).rad Then Combo2.AddItem con(cas(i).idx).name
  '  Next
  
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
   '         cas(i).progeny(cas(i).numprog).unt(0) = unit(9).Tag
    '        cas(i).progeny(cas(i).numprog).unt(1) = unit(10).Tag
            cas(i).progeny(cas(i).numprog).sol = -1
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
  StartModule STRUCTURE, "GENII NESHAPS Report Generator", 5
  SetHelpFile App.Path + "\gen_nesh.htm"
  SetRefFile ReplaceExt(FUIName, "ref")
  RType_Click 0
  Loading.Show
  loadprm
  Unload Loading
  SSTab1_Click 0
End Sub

Private Sub save_Click()
  Dim fname As String
  Dim parm As parmrec
  Dim fle As parmfile
  Dim i As Long
  Dim j As Long
  Dim k As Long
  Dim m As Long
  
  Combo2_Click
  fname = RunName + ".GID"
  If open_parm(fle, fname, 1) Then
    For j = 0 To 2
      If RType(j).Value Then
        If j = 0 Then
          set_parm parm, "FGR13", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", False
'          write_parmrec fle, parm
        End If
        If j = 1 Then
          set_parm parm, "FGR13", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", True
'          write_parmrec fle, parm
   
          If er(6) Then PutError "Parameter " & txt(4).Tag & " is invalid"
          set_parm parm, txt(6).Tag, 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", txt(6)
          write_parmrec fle, parm

        End If
        set_parm parm, "METHOD", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CStr(j)
        write_parmrec fle, parm
        If j = 2 Then
          set_parm parm, "IHEAST", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CStr(1)
'          write_parmrec fle, parm
        Else
          set_parm parm, "IHEAST", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CStr(0)
'          write_parmrec fle, parm
'          For i = 0 To 2
'            set_parm parm, SSCheck1(i).Tag, 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", SSCheck1(i).Value
'            write_parmrec fle, parm
'            If SSCheck1(i).Value And i < 2 Then
'              If er(i) Then PutError "Parameter " & txt(i).Tag & " is invalid"
'              set_parm parm, txt(i).Tag, 0, 0, 0, 0, 0, 0, 0, 0, 0, txt(i)
'              write_parmrec fle, parm
'            End If
'          Next

          If er(0) Then PutError "Parameter " & txt(0).Tag & " is invalid"
          If er(1) Then PutError "Parameter " & txt(1).Tag & " is invalid"
          set_parm parm, txt(0).Tag, 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", txt(0)
          write_parmrec fle, parm
          set_parm parm, txt(1).Tag, 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", txt(1)
          write_parmrec fle, parm
  
          If er(0) Then PutError "Parameter " & txt(4).Tag & " is invalid"
          If er(1) Then PutError "Parameter " & txt(5).Tag & " is invalid"
          set_parm parm, txt(4).Tag, 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", txt(4)
          write_parmrec fle, parm
          set_parm parm, txt(5).Tag, 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", txt(5)
          write_parmrec fle, parm
  
'          If er(6) Then PutError "Parameter " & txt(6).Tag & " is invalid"
'          set_parm parm, txt(6).Tag, 0, 0, 0, 0, 0, 0, 0, 0, 0, txt(6)
'          write_parmrec fle, parm
'
'          set_parm parm, "NUMCON", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", Format(m_numcon)
'          write_parmrec fle, parm
'          If j = 1 Then
'            For i = 1 To m_numcon
'              If cas(i).idx <> 0 And con(cas(i).idx).rad Then
'                set_parm parm, "CASID", cas(i).idx, 0, 0, 0, 0, 0, 0, "N/A", "N/A", cas(i).cas
'                write_parmrec fle, parm
'                If (cas(i).sol < 0) Then PutError "Parameter SOLUBIL for " & cas(i).cas & " is invalid"
'                set_parm parm, "SOLUBIL", cas(i).idx, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CStr(cas(i).sol + 1)
'                write_parmrec fle, parm
'                For k = 0 To 1
'                  If cas(i).txt(k) = Empty Then
'                    PutError "Parameter " & txt(k + 6).Tag & " for " & cas(i).cas & " is invalid"
'                    set_parm parm, txt(k + 6).Tag, cas(i).idx, 0, 0, 0, 0, 0, 0, 0, 0, "EMPTY"
'                  Else
'                    set_parm parm, txt(k + 6).Tag, cas(i).idx, 0, 0, 0, 0, 0, cas(i).ref(k), cas(i).unt(k), 0, convert(cas(i).unt(k), 0, Val(cas(i).txt(k)))
'                  End If
'                  write_parmrec fle, parm
'                Next
'                set_parm parm, "NDS", cas(i).idx, 0, 0, 0, 0, 0, 0, "N/A", "N/A", Format(cas(i).numprog)
'                write_parmrec fle, parm
'                For k = 1 To cas(i).numprog
'                  If cas(i).progeny(k).idx <> 0 Then
'                    set_parm parm, "CASID", cas(i).idx, cas(i).progeny(k).idx, 0, 0, 0, 0, 0, "N/A", "N/A", cas(i).progeny(k).cas
'                    write_parmrec fle, parm
'                    If (cas(i).progeny(k).sol < 0) Then PutError "Parameter SOLUBIL for " & cas(i).progeny(k).cas & " is invalid"
'                    set_parm parm, "SOLUBIL", cas(i).idx, cas(i).progeny(k).idx, 0, 0, 0, 0, 0, "N/A", "N/A", CStr(cas(i).progeny(k).sol + 1)
'                    write_parmrec fle, parm
'                    For m = 0 To 1
'                      If cas(i).progeny(k).txt(m) = Empty Then
'                        PutError "Parameter " & txt(m + 9).Tag & " for " & cas(i).progeny(k).cas & " daughter of " & cas(i).cas & " is invalid"
'                        set_parm parm, txt(m + 9).Tag, cas(i).idx, cas(i).progeny(k).idx, 0, 0, 0, 0, 0, 0, 0, "EMPTY"
'                      Else
'                        set_parm parm, txt(m + 9).Tag, cas(i).idx, cas(i).progeny(k).idx, 0, 0, 0, 0, cas(i).progeny(k).ref(m), cas(i).progeny(k).unt(m), 0, convert(cas(i).progeny(k).unt(m), 0, Val(cas(i).progeny(k).txt(m)))
'                      End If
'                      write_parmrec fle, parm
'                    Next
'                  End If
'                Next
'              End If
'            Next
'          End If
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
'  ref(Index).Enabled = Value
  lbl(Index).Enabled = Value
'  ref(Index).Enabled = Value
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
 ' If Combo2.ListIndex > 0 Then Combo2.ListIndex = Combo2.ListIndex - 1
End Sub

Private Sub SSCommand2_Click()
 ' If Combo2.ListIndex < Combo2.ListCount - 1 Then Combo2.ListIndex = Combo2.ListIndex + 1
End Sub

Private Sub SSCommand3_Click()
'  If Combo3.ListIndex > 0 Then Combo3.ListIndex = Combo3.ListIndex - 1
End Sub

Private Sub SSCommand4_Click()
'  If Combo3.ListIndex < Combo3.ListCount - 1 Then Combo3.ListIndex = Combo3.ListIndex + 1
End Sub

Private Function er(Index As Long) As Boolean
  Dim tval As Double
  Dim t1 As String
  Dim t2 As String
  Dim m As String
  
  m = ""
  er = False
'  If txt(Index).Text = "" Then er = True
'  tval = Val(txt(Index).Text)
'  Select Case Index
'    Case 0, 1
'      t1 = 0.0001
'      t2 = 1
'      m = "Value must be between " + t1 + " and " + t2 + " "
'      If (tval < Val(t1) Or tval > Val(t2)) Then er = True
'    Case 4
'      t1 = 0.001
'      t2 = 5
'      m = "Value must be between " + t1 + " and " + t2 + " "
'      If (tval < Val(t1) Or tval > Val(t2)) Then er = True
'    Case 5
'      t1 = 500
'      t2 = 3000
'      m = "Value must be between " + t1 + " and " + t2 + " "
'      If (tval < Val(t1) Or tval > Val(t2)) Then er = True
'    Case 6, 9
'      t1 = 0
'      t2 = 1
'      m = "Value must be between " + t1 + " and " + t2 + " "
'      If (tval < Val(t1) Or tval > Val(t2)) Then er = True
'    Case 7, 10
'      t1 = 0.1
'      t2 = 100
'      m = "Value must be between " + t1 + " and " + t2 + " "
'      If (tval < Val(t1) Or tval > Val(t2)) Then er = True
'  End Select
'  mes = Space(140 - Len(m)) & m
'  If er Then
'    txt(Index).BackColor = &H8080FF
'  Else
'    txt(Index).BackColor = &HC0FFC0
'  End If
End Function

Private Sub txt_Change(Index As Integer)
Dim chk As Double
On Error GoTo toolarge
  er CLng(Index)
  chk = CDbl(txt(Index).Text)
  Exit Sub
toolarge:
 ' txt(Index).BackColor = &H8080FF
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
'Dim m As String
'  If SSCheck1(Index).Value = -1 And Index = 1 Then
'    m = "Must use ICRP dose and risk factors for cancer risk"
'  Else
'    m = SSCheck1(Index).Caption & ", click to enable"
'  End If
'  mes = Space(140 - Len(m)) & m
'  noact.Enabled = False
End Sub

Private Sub Combo2_GotFocus()
Dim m As String
  m = "Select a constituent"
  mes = Space(140 - Len(m)) & m
  RefItem = 12
  noact.Enabled = True
  HelpAnchor = ""
End Sub

Private Sub Combo3_GotFocus()
Dim m As String
  m = "Select a progeny"
  mes = Space(140 - Len(m)) & m
  RefItem = 13
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

Private Sub SSCommand3_GotFocus()
  mes = ""
  noact.Enabled = False
  HelpAnchor = ""
End Sub

Private Sub SSCommand4_GotFocus()
  mes = ""
  noact.Enabled = False
  HelpAnchor = ""
End Sub

Private Sub c_sol_GotFocus()
  mes = ""
  noact.Enabled = False
  HelpAnchor = ""
End Sub

Private Sub d_sol_GotFocus()
Dim m As String
  mes = ""
  noact.Enabled = False
  HelpAnchor = ""
End Sub

Private Sub caschange(tx As String, oldtx As String)
  Dim i As Long
  Dim j As Long
  For i = 1 To m_numcon
    If con(cas(i).idx).name = oldtx Then
      For j = 0 To 1
        If txt(j + 6).Text = "" Then
          cas(i).txt(j) = Empty
        Else
          cas(i).txt(j) = txt(j + 6).Text
        End If
  '      cas(i).unt(j) = unit(j + 6).Text
  '      cas(i).ref(j) = Val(ref(j + 6).Tag)
      Next
  '    cas(i).sol = c_sol.ListIndex
      Exit For
    End If
  Next
  
 ' For i = 1 To m_numcon
 '   If con(cas(i).idx).name = tx Then
 '     For j = 0 To 1
 '       set_unit unit(j + 6), cas(i).unt(j)
 '       If cas(i).txt(j) = Empty Then
 '         txt(j + 6).Text = ""
 '       Else
 '         txt(j + 6).Text = cas(i).txt(j)
 '       End If
 '       ref(j + 6).Caption = "Ref: " & cas(i).ref(j)
 '       ref(j + 6).Tag = cas(i).ref(j)
 '     Next
 '     c_sol.ListIndex = cas(i).sol
 '     Exit For
 '   End If
 ' Next
End Sub

Private Sub d_caschange(tx As String, oldtx As String)
  Dim i As Long
  Dim j As Long
  Dim k As Long
  For i = 1 To m_numcon
    If con(cas(i).idx).name = ctext2 Then
 '     For j = 1 To cas(i).numprog
 '       If con(cas(i).idx).progeny(cas(i).progeny(j).idx).name = oldtx Then
 '         For k = 0 To 1
 '           If txt(k + 9).Text = "" Then
 '             cas(i).progeny(j).txt(k) = Empty
 '           Else
 '             cas(i).progeny(j).txt(k) = txt(k + 9).Text
 '           End If
 ''           cas(i).progeny(j).unt(k) = unit(k + 9).Text
 '           cas(i).progeny(j).ref(k) = Val(ref(k + 9).Tag)
 '         Next
 '         cas(i).progeny(j).sol = d_sol.ListIndex
 '         Exit For
 '       End If
 '     Next
 '     Exit For
    End If
  Next
  For i = 1 To m_numcon
    If con(cas(i).idx).name = ctext2 Then
  '    For j = 1 To cas(i).numprog
  '      If con(cas(i).idx).progeny(cas(i).progeny(j).idx).name = tx Then
  '        For k = 0 To 1
  '          set_unit unit(k + 9), cas(i).progeny(j).unt(k)
  '          If cas(i).progeny(j).txt(k) = Empty Then
  '            txt(k + 9).Text = ""
  '          Else
  '            txt(k + 9).Text = cas(i).progeny(j).txt(k)
  '          End If
  '          ref(k + 9).Caption = "Ref: " & cas(i).progeny(j).ref(k)
  '          ref(k + 9).Tag = cas(i).progeny(j).ref(k)
  '        Next
  '        d_sol.ListIndex = cas(i).progeny(j).sol
  '        Exit For
  '      End If
  '    Next
  '    Exit For
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
