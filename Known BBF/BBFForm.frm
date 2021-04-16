VERSION 5.00
Object = "{6B7E6392-850A-101B-AFC0-4210102A8DA7}#1.3#0"; "comctl32.ocx"
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "threed32.ocx"
Object = "{F856EC8B-F03C-4515-BDC6-64CBD617566A}#7.0#0"; "FPSPR70.ocx"
Begin VB.Form BBF 
   BorderStyle     =   1  'Fixed Single
   ClientHeight    =   5988
   ClientLeft      =   7932
   ClientTop       =   5532
   ClientWidth     =   9600
   Icon            =   "BBFForm.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   499
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   800
   StartUpPosition =   2  'CenterScreen
   Begin ComctlLib.TreeView tvwBBF 
      Height          =   5775
      Left            =   120
      TabIndex        =   0
      Tag             =   "media"
      Top             =   90
      Width           =   3000
      _ExtentX        =   5292
      _ExtentY        =   10181
      _Version        =   327682
      HideSelection   =   0   'False
      Style           =   4
      Appearance      =   1
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   7.8
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
   End
   Begin VB.Frame Frame2 
      Height          =   5880
      Left            =   3240
      TabIndex        =   5
      Top             =   0
      Width           =   6300
      Begin VB.ComboBox Combo1 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   315
         Left            =   240
         Style           =   2  'Dropdown List
         TabIndex        =   8
         Tag             =   "species"
         Top             =   600
         Width           =   5655
      End
      Begin VB.ComboBox Combo2 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   315
         Left            =   240
         Style           =   2  'Dropdown List
         TabIndex        =   7
         Tag             =   "parent"
         Top             =   1440
         Width           =   2655
      End
      Begin VB.ComboBox Combo3 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   288
         Left            =   3360
         Style           =   2  'Dropdown List
         TabIndex        =   6
         Tag             =   "parent"
         Top             =   1440
         Visible         =   0   'False
         Width           =   2580
      End
      Begin FPSpreadADO.fpSpread vaSpread1 
         Height          =   3615
         Left            =   240
         TabIndex        =   9
         Tag             =   "concentrations"
         Top             =   1920
         Width           =   2625
         _Version        =   458752
         _ExtentX        =   4614
         _ExtentY        =   6054
         _StockProps     =   64
         ArrowsExitEditMode=   -1  'True
         AutoCalc        =   0   'False
         DisplayRowHeaders=   0   'False
         EditEnterAction =   5
         EditModeReplace =   -1  'True
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
         MaxCols         =   2
         MaxRows         =   5000
         RowsFrozen      =   1
         ScrollBarExtMode=   -1  'True
         ScrollBarShowMax=   0   'False
         SpreadDesigner  =   "BBFForm.frx":030A
         StartingColNumber=   0
         StartingRowNumber=   0
         VisibleCols     =   2
         VisibleRows     =   14
      End
      Begin FPSpreadADO.fpSpread vaSpread2 
         Height          =   3615
         Left            =   3360
         TabIndex        =   10
         Tag             =   "concentrations"
         Top             =   1920
         Visible         =   0   'False
         Width           =   2625
         _Version        =   458752
         _ExtentX        =   4614
         _ExtentY        =   6054
         _StockProps     =   64
         ArrowsExitEditMode=   -1  'True
         AutoCalc        =   0   'False
         DisplayRowHeaders=   0   'False
         EditEnterAction =   5
         EditModeReplace =   -1  'True
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
         MaxCols         =   2
         MaxRows         =   5000
         RowsFrozen      =   1
         ScrollBarExtMode=   -1  'True
         SpreadDesigner  =   "BBFForm.frx":060F
         StartingColNumber=   0
         StartingRowNumber=   0
         UserResize      =   0
         VisibleCols     =   2
         VisibleRows     =   14
      End
      Begin Threed.SSCommand SSCommand2 
         Height          =   315
         Left            =   5520
         TabIndex        =   18
         Tag             =   "species"
         Top             =   240
         Width           =   405
         _Version        =   65536
         _ExtentX        =   706
         _ExtentY        =   564
         _StockProps     =   78
         Caption         =   ">>"
      End
      Begin Threed.SSCommand SSCommand1 
         Height          =   315
         Left            =   5160
         TabIndex        =   19
         Tag             =   "species"
         Top             =   240
         Width           =   405
         _Version        =   65536
         _ExtentX        =   706
         _ExtentY        =   564
         _StockProps     =   78
         Caption         =   "<<"
      End
      Begin Threed.SSCommand SSCommand4 
         Height          =   315
         Left            =   1680
         TabIndex        =   20
         Tag             =   "parent"
         Top             =   1080
         Width           =   405
         _Version        =   65536
         _ExtentX        =   706
         _ExtentY        =   564
         _StockProps     =   78
         Caption         =   ">>"
      End
      Begin Threed.SSCommand SSCommand3 
         Height          =   315
         Left            =   1320
         TabIndex        =   21
         Tag             =   "parent"
         Top             =   1080
         Width           =   405
         _Version        =   65536
         _ExtentX        =   706
         _ExtentY        =   564
         _StockProps     =   78
         Caption         =   "<<"
      End
      Begin Threed.SSCommand SSCommand5 
         Height          =   315
         Left            =   4680
         TabIndex        =   22
         Tag             =   "parent"
         Top             =   1080
         Visible         =   0   'False
         Width           =   405
         _Version        =   65536
         _ExtentX        =   706
         _ExtentY        =   564
         _StockProps     =   78
         Caption         =   ">>"
      End
      Begin Threed.SSCommand SSCommand6 
         Height          =   315
         Left            =   4320
         TabIndex        =   23
         Tag             =   "parent"
         Top             =   1080
         Visible         =   0   'False
         Width           =   405
         _Version        =   65536
         _ExtentX        =   706
         _ExtentY        =   564
         _StockProps     =   78
         Caption         =   "<<"
      End
      Begin VB.Label Label2 
         Caption         =   "Species"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   120
         TabIndex        =   15
         Top             =   360
         Width           =   2295
      End
      Begin VB.Label ref 
         Caption         =   "Ref: 0"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Index           =   6
         Left            =   2160
         TabIndex        =   14
         Tag             =   "0"
         Top             =   1200
         Width           =   735
      End
      Begin VB.Label ref 
         Caption         =   "Ref: 0"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Index           =   7
         Left            =   5160
         TabIndex        =   13
         Tag             =   "0"
         Top             =   1200
         Visible         =   0   'False
         Width           =   735
      End
      Begin VB.Label lbl 
         Caption         =   "Progeny"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Index           =   7
         Left            =   3240
         TabIndex        =   12
         Top             =   1170
         Visible         =   0   'False
         Width           =   1410
      End
      Begin VB.Label lbl 
         Caption         =   "Constituent"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Index           =   8
         Left            =   120
         TabIndex        =   11
         Top             =   1170
         Width           =   1410
      End
   End
   Begin VB.Frame Frame1 
      Height          =   5880
      Left            =   3240
      TabIndex        =   1
      Top             =   0
      Width           =   6300
      Begin VB.CommandButton Command3 
         Caption         =   "Rename"
         Height          =   375
         Left            =   2640
         TabIndex        =   24
         Top             =   3840
         Width           =   1000
      End
      Begin VB.ListBox lstMedia 
         Height          =   2544
         ItemData        =   "BBFForm.frx":091C
         Left            =   480
         List            =   "BBFForm.frx":091E
         TabIndex        =   17
         Top             =   2880
         Width           =   1815
      End
      Begin VB.CommandButton Command2 
         Caption         =   "Delete"
         Height          =   375
         Left            =   2640
         TabIndex        =   16
         Top             =   3360
         Width           =   1000
      End
      Begin VB.CommandButton Command1 
         Caption         =   "Add"
         Height          =   375
         Left            =   2640
         TabIndex        =   4
         Top             =   2880
         Width           =   1000
      End
      Begin VB.Label Label1 
         Caption         =   $"BBFForm.frx":0920
         Height          =   1095
         Left            =   240
         TabIndex        =   3
         Top             =   1440
         Width           =   5775
      End
      Begin VB.Label Label8 
         Caption         =   $"BBFForm.frx":09E9
         Height          =   855
         Left            =   240
         TabIndex        =   2
         Top             =   480
         Width           =   5775
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
   Begin VB.Menu opt 
      Caption         =   "&Options"
      Visible         =   0   'False
      Begin VB.Menu prog 
         Caption         =   "Include Progeny"
      End
   End
   Begin VB.Menu noact 
      Caption         =   "&Reference"
      Enabled         =   0   'False
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
         Caption         =   "How To ..."
      End
      Begin VB.Menu about 
         Caption         =   "&About"
      End
   End
End
Attribute VB_Name = "BBF"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text
Dim orgvalu(4) As Double
Dim orgunit(4) As String
Dim kind As String
Dim c1 As Long
Dim c2 As Long
Dim c3 As Long
Dim c4 As Long
Dim f2 As Long
Dim f3 As Long
Dim c1click As Boolean
Dim c2click As Boolean
Dim c3click As Boolean
Dim oldtext As String
Dim tabindex As Long
Dim temp As parmrec
Dim loadng As Boolean

Private Sub howto_Click()
  GetHelp
End Sub
 
Private Sub about_Click()
  frmAbout.picIcon = frmAbout.ImageList1.ListImages(1).Picture
  frmAbout.Show 1, BBF
End Sub

Private Sub Form_Unload(Cancel As Integer)
  Dim answer As Long
  If Not called Then
  answer = MsgBox("Do you want to save changes?", 51, BBF.Caption)
  If answer = 6 Then save_Click
  If answer = 7 Then EndModule
  If answer = 2 Then Cancel = 1
  End If
End Sub

Private Sub Check_NTS(nts As Long, errmsg As String)
  If (nts < 2) Then PutError errmsg
End Sub

Private Sub leave_Click()
  Form_Unload 0
End Sub

Private Sub selref_Click()
  RefMode = 0
  GetRef ref(RefItem)
End Sub

Private Sub addref_Click()
  RefMode = 1
  GetRef ref(RefItem)
End Sub

Private Sub hlp_Click()
  GetHelp
End Sub

Private Sub SizeParentProgenyList()
  If temp.idx2 > numcon Then
    ReDim Preserve con(temp.idx2) As parent
    numcon = temp.idx2
  End If
  If temp.idx3 > con(temp.idx2).numprog Then
    ReDim Preserve con(temp.idx2).prog(temp.idx3) As progeny
    con(temp.idx2).numprog = temp.idx3
  End If
End Sub

Private Sub SizeLocal(size As Long, someloc() As location)
  If temp.idx2 > size Then
    size = temp.idx2
    ReDim Preserve someloc(size) As location
  End If
End Sub

Private Sub SizeLoc()
  If temp.idx1 > numloc Then
    ReDim Preserve loc(temp.idx1) As BBFLoc
    loc(temp.idx1).use = True
    numloc = temp.idx1
  End If
End Sub

Private Sub SizeSpecie()
  If temp.idx2 > loc(temp.idx1).numspecie Then
    ReDim Preserve loc(temp.idx1).specs(temp.idx2) As Specie
    loc(temp.idx1).numspecie = temp.idx2
  End If
End Sub

Private Sub SizeChem()
  If temp.idx4 = 0 Then
    If temp.idx3 > loc(temp.idx1).specs(temp.idx2).numcon Then
      ReDim Preserve loc(temp.idx1).specs(temp.idx2).con(temp.idx3) As sParent
      loc(temp.idx1).specs(temp.idx2).numcon = temp.idx3
    End If
  Else
    If temp.idx3 > loc(temp.idx1).specs(temp.idx2).numcon Then
      ReDim Preserve loc(temp.idx1).specs(temp.idx2).con(temp.idx3) As sParent
      loc(temp.idx1).specs(temp.idx2).numcon = temp.idx3
    End If
    If temp.idx4 > loc(temp.idx1).specs(temp.idx2).con(temp.idx3).numprog Then
      ReDim Preserve loc(temp.idx1).specs(temp.idx2).con(temp.idx3).prog(temp.idx4) As sProgeny
      loc(temp.idx1).specs(temp.idx2).con(temp.idx3).numprog = temp.idx4
    End If
  End If
End Sub

Private Sub SizeSeries()
  If temp.idx4 = 0 Then
    If temp.idx5 > loc(temp.idx1).specs(temp.idx2).con(temp.idx3).nts Then
      ReDim Preserve loc(temp.idx1).specs(temp.idx2).con(temp.idx3).series(temp.idx5) As entry
      loc(temp.idx1).specs(temp.idx2).con(temp.idx3).nts = temp.idx5
    End If
  Else
    If temp.idx5 > loc(temp.idx1).specs(temp.idx2).con(temp.idx3).prog(temp.idx4).nts Then
      ReDim Preserve loc(temp.idx1).specs(temp.idx2).con(temp.idx3).prog(temp.idx4).series(temp.idx5) As entry
      loc(temp.idx1).specs(temp.idx2).con(temp.idx3).prog(temp.idx4).nts = temp.idx5
    End If
  End If
End Sub

Private Sub ResizeSpecies()
Dim i As Long
Dim j As Long
Dim k As Long
Dim tempspecs() As Specie
Dim speccount As Long
  
  For i = 1 To numloc
    If loc(i).numspecie > 0 Then
      ReDim tempspecs(loc(i).numspecie) As Specie
      speccount = 0
      For j = 1 To loc(i).numspecie
        If loc(i).use = True Then
          speccount = speccount + 1
          AssignSpecie tempspecs(speccount), loc(i).specs(j)
        End If
      Next
      ReDim loc(i).specs(speccount) As Specie
      For j = 1 To speccount
        AssignSpecie loc(i).specs(j), tempspecs(j)
        loc(i).numspecie = speccount
      Next
    Else
      ReDim loc(i).specs(numspec) As Specie
      For j = 1 To numspec
        loc(i).specs(j).name = specname(j)
        loc(i).specs(j).sciname = specsciname(j)
        loc(i).specs(j).use = True
      Next
      loc(i).numspecie = numspec
    End If
  Next
End Sub

Private Sub loadprm()
  Dim mnode As ComctlLib.node
  Dim foundname As Boolean
  Dim i As Long, j As Long, k As Long, l As Long, m As Long, n As Long, cnt As Long
  Dim id As String
  Dim typ As String
  Dim section(3) As String
  Dim prefix As String
  
  Dim fle As parmfile
  
  numcon = 0
  numexp = 0
  numspec = 0
  
  If open_parm(fle, FUIName, 2) Then
    
    LoadSection fle, "csm"
    If ReadLng(fle, cnt, "nummod", siteIdx) Then
      For i = 1 To cnt
        If ReadStr(fle, prefix, "modid", siteIdx, i) Then
          If prefix = modName Then
            ReadStr fle, DesName, "moddespath", siteIdx, i
            If ReadLng(fle, numloc, "modsrcnum", siteIdx, i) Then
              For j = 1 To numloc
                ReadStr fle, id, "modsrcid", siteIdx, i, j
                ReadStr fle, typ, "modsrctype", siteIdx, i, j
                If typ = "con" Then section(0) = id
                If typ = "ebf" Then section(1) = id
                If typ = "aos" Then section(2) = id
                If typ = "tos" Then section(3) = id
              Next
            Else
              PutError "Unable to read number of sources for module id for site index:" & CStr(siteIdx) & " module index:" & CStr(i)
            End If
          End If
        Else
          PutError "Unable to read module id for site index:" & CStr(siteIdx) & " module index:" & CStr(i)
        End If
      Next
    Else
      PutError "Unable to read number of sources for module id for site index:" & CStr(siteIdx) & " module index:" & CStr(i)
    End If
    
    numloc = 0
    reset_csv fle.file
    Do Until EOCF(fle.file)
      If read_parmrec(fle, temp) Then
        Select Case temp.pname
          Case "fui"
            For i = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                If temp.idx1 = siteIdx Then
                  Select Case temp.pname
                    Case "ecodespath"
                      If Left(modName, 3) = "eco" And temp.idx2 = modIdx Then DesName = temp.pval
                    Case "usrdespath"
                      If Left(modName, 3) = "usr" And temp.idx2 = modIdx Then DesName = temp.pval
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
                        con(temp.idx2).prog(temp.idx3).kind = Val(temp.pval)
                      Else
                        con(temp.idx2).kind = Val(temp.pval)
                      End If
                  End Select
                End If
              End If
            Next
          Case modName
            Loading.Gauge1.Max = Val(temp.idx1)
            Loading.Gauge1.Value = 0
            For i = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                Loading.update
                Select Case temp.pname
                  Case "specsciname":
                    SizeLoc
                    SizeSpecie
                    loc(temp.idx1).specs(temp.idx2).sciname = temp.pval
                    loc(temp.idx1).specs(temp.idx2).use = True
                  Case "specname":
                    SizeLoc
                    SizeSpecie
                    loc(temp.idx1).specs(temp.idx2).name = temp.pval
                    loc(temp.idx1).specs(temp.idx2).use = True
                  Case "LocName":
                    SizeLoc
                    loc(temp.idx1).name = temp.pval
                  Case "LocLabel":
                    SizeLoc
                    loc(temp.idx1).lbl = temp.pval
                  Case "conccas":
                    SizeLoc
                    SizeSpecie
                    SizeChem
                    If temp.idx4 = 0 Then
                      loc(temp.idx1).specs(temp.idx2).con(temp.idx3).cas = temp.pval
                      loc(temp.idx1).specs(temp.idx2).con(temp.idx3).ref = temp.ref
                      loc(temp.idx1).specs(temp.idx2).con(temp.idx3).use = 0
                      loc(temp.idx1).specs(temp.idx2).con(temp.idx3).tunit(1) = temp.uunit
                      loc(temp.idx1).specs(temp.idx2).con(temp.idx3).tunit(2) = temp.uunit
                      loc(temp.idx1).specs(temp.idx2).con(temp.idx3).unit(1) = temp.cunit
                      loc(temp.idx1).specs(temp.idx2).con(temp.idx3).unit(2) = temp.cunit
                    Else
                      loc(temp.idx1).specs(temp.idx2).con(temp.idx3).prog(temp.idx4).cas = temp.pval
                      loc(temp.idx1).specs(temp.idx2).con(temp.idx3).prog(temp.idx4).ref = temp.ref
                      loc(temp.idx1).specs(temp.idx2).con(temp.idx3).prog(temp.idx4).use = 0
                      loc(temp.idx1).specs(temp.idx2).con(temp.idx3).prog(temp.idx4).tunit(1) = temp.uunit
                      loc(temp.idx1).specs(temp.idx2).con(temp.idx3).prog(temp.idx4).tunit(2) = temp.uunit
                      loc(temp.idx1).specs(temp.idx2).con(temp.idx3).prog(temp.idx4).unit(1) = temp.cunit
                      loc(temp.idx1).specs(temp.idx2).con(temp.idx3).prog(temp.idx4).unit(2) = temp.cunit
                    End If
                  Case "ctime":
                    SizeLoc
                    SizeSpecie
                    SizeChem
                    SizeSeries
                    If temp.idx4 = 0 Then
                      loc(temp.idx1).specs(temp.idx2).con(temp.idx3).series(temp.idx5).time = Val(convert(temp.cunit, temp.uunit, Val(temp.pval)))
                      loc(temp.idx1).specs(temp.idx2).con(temp.idx3).tunit(1) = temp.uunit
                    Else
                      loc(temp.idx1).specs(temp.idx2).con(temp.idx3).prog(temp.idx4).series(temp.idx5).time = Val(convert(temp.cunit, temp.uunit, Val(temp.pval)))
                      loc(temp.idx1).specs(temp.idx2).con(temp.idx3).prog(temp.idx4).tunit(1) = temp.uunit
                    End If
                  Case "conc":
                    SizeLoc
                    SizeSpecie
                    SizeChem
                    SizeSeries
                    If temp.idx4 = 0 Then
                      loc(temp.idx1).specs(temp.idx2).con(temp.idx3).series(temp.idx5).valu = Val(convert(temp.cunit, temp.uunit, Val(temp.pval)))
                      loc(temp.idx1).specs(temp.idx2).con(temp.idx3).unit(1) = temp.uunit
                    Else
                      loc(temp.idx1).specs(temp.idx2).con(temp.idx3).prog(temp.idx4).series(temp.idx5).valu = Val(convert(temp.cunit, temp.uunit, Val(temp.pval)))
                      loc(temp.idx1).specs(temp.idx2).con(temp.idx3).prog(temp.idx4).unit(1) = temp.uunit
                    End If
                End Select
              End If
            Next
          Case section(1), section(2), section(3)
            Loading.Gauge1.Max = Val(temp.idx1)
            Loading.Gauge1.Value = 0
            For i = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                Loading.update
                Select Case temp.pname
                  Case "LifeFormSci", "ScientificName":
                    If temp.idx1 > numspec Then
                      ReDim Preserve specname(temp.idx1) As String
                      ReDim Preserve specsciname(temp.idx1) As String
                      numspec = temp.idx1
                    End If
                    specsciname(temp.idx1) = temp.pval
                  Case "LifeformName", "CommonName":
                    If temp.idx1 > numspec Then
                      ReDim Preserve specname(temp.idx1) As String
                      ReDim Preserve specsciname(temp.idx1) As String
                      numspec = temp.idx1
                    End If
                    specname(temp.idx1) = temp.pval
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
    
    
    'resolve species differences
    For i = 1 To numloc
      For j = 1 To loc(i).numspecie
        foundname = False
        For k = 1 To numspec
          If (loc(i).specs(j).name = specname(k)) And (loc(i).specs(j).sciname = specsciname(k)) Then
            foundname = True
          End If
        Next
        loc(i).specs(j).use = foundname
      Next
    Next
    For i = 1 To numloc
      For j = 1 To numspec
        foundname = False
        For k = 1 To loc(i).numspecie
          If (specname(j) = loc(i).specs(k).name) And (specsciname(j) = loc(i).specs(k).sciname) Then
            foundname = True
          End If
        Next
        If foundname = False Then
          ReDim Preserve loc(i).specs(loc(i).numspecie + 1) As Specie
          loc(i).numspecie = loc(i).numspecie + 1
          loc(i).specs(loc(i).numspecie).name = specname(j)
          loc(i).specs(loc(i).numspecie).sciname = specsciname(j)
          loc(i).specs(loc(i).numspecie).use = True
          ReDim loc(i).specs(loc(i).numspecie).con(numcon) As sParent
          For k = 1 To numcon
            loc(i).specs(loc(i).numspecie).con(k).cas = con(k).cas
            loc(i).specs(loc(i).numspecie).con(k).numprog = con(k).numprog
            ReDim loc(i).specs(loc(i).numspecie).con(k).prog(con(k).numprog) As sProgeny
            For m = 1 To con(k).numprog
              loc(i).specs(loc(i).numspecie).con(k).prog(m).cas = con(k).prog(m).cas
            Next
          Next
        End If
      Next
    Next
    ResizeSpecies
    'bbf resolve contaminate differences if contaminate no longer exists its index is 0
    For i = 1 To numloc
      If loc(i).use Then
        For n = 1 To loc(i).numspecie
          For j = 1 To numcon
            For k = 1 To loc(i).specs(n).numcon
              If con(j).cas = loc(i).specs(n).con(k).cas Then
                loc(i).specs(n).con(k).use = j
                loc(i).specs(n).con(k).cas = con(j).cas
                For l = 1 To con(j).numprog
                  For m = 1 To loc(i).specs(n).con(k).numprog
                    If con(j).prog(l).cas = loc(i).specs(n).con(k).prog(m).cas Then
                      loc(i).specs(n).con(k).prog(m).use = l
                      loc(i).specs(n).con(k).prog(m).cas = con(j).prog(l).cas
                      Exit For
                    End If
                  Next
                  If m > loc(i).specs(n).con(k).numprog Then
                    ReDim Preserve loc(i).specs(n).con(k).prog(m) As sProgeny
                    loc(i).specs(n).con(k).numprog = m
                    loc(i).specs(n).con(k).prog(m).use = l
                    loc(i).specs(n).con(k).prog(m).cas = con(j).prog(l).cas
                    loc(i).specs(n).con(k).prog(m).tunit(1) = "yr"
                    loc(i).specs(n).con(k).prog(m).tunit(2) = "yr"
                    If con(j).kind = 1 Then
                      loc(i).specs(n).con(k).prog(m).unit(1) = "pCi/kg"
                      loc(i).specs(n).con(k).prog(m).unit(2) = "pCi/kg"
                    Else
                      loc(i).specs(n).con(k).prog(m).unit(1) = "mg/kg"
                      loc(i).specs(n).con(k).prog(m).unit(2) = "mg/kg"
                    End If
                  End If
                Next
                Exit For
              End If
            Next
            If k > loc(i).specs(n).numcon Then
              ReDim Preserve loc(i).specs(n).con(k) As sParent
              loc(i).specs(n).numcon = k
              loc(i).specs(n).con(k).use = j
              loc(i).specs(n).con(k).cas = con(j).cas
              loc(i).specs(n).con(k).numprog = con(j).numprog
              loc(i).specs(n).con(k).tunit(1) = "yr"
              loc(i).specs(n).con(k).tunit(2) = "yr"
              If con(j).kind = 1 Then
                loc(i).specs(n).con(k).unit(1) = "pCi/kg"
                loc(i).specs(n).con(k).unit(2) = "pCi/kg"
              Else
                loc(i).specs(n).con(k).unit(1) = "mg/kg"
                loc(i).specs(n).con(k).unit(2) = "mg/kg"
              End If
              If con(j).numprog > 0 Then
                ReDim Preserve loc(i).specs(n).con(k).prog(con(j).numprog) As sProgeny
                For l = 1 To con(j).numprog
                  loc(i).specs(n).con(k).prog(l).use = l
                  loc(i).specs(n).con(k).prog(l).cas = con(j).prog(l).cas
                  loc(i).specs(n).con(k).prog(l).tunit(1) = "yr"
                  loc(i).specs(n).con(k).prog(l).tunit(2) = "yr"
                  If con(j).kind = 1 Then
                    loc(i).specs(n).con(k).prog(l).unit(1) = "pCi/kg"
                    loc(i).specs(n).con(k).prog(l).unit(2) = "pCi/kg"
                  Else
                    loc(i).specs(n).con(k).prog(l).unit(1) = "mg/kg"
                    loc(i).specs(n).con(k).prog(l).unit(2) = "mg/kg"
                  End If
                Next
              End If
            End If
          Next
        Next
      End If
    Next
    
    For i = 1 To numloc
      If loc(i).use Then
        Set mnode = tvwBBF.Nodes.Add("BBF", tvwChild, loc(i).name, loc(i).name)
        mnode.Tag = i
        mnode.Expanded = True
        mnode.EnsureVisible
      End If
    Next
    
    LoadlstMedia
    
  Else
    PutError "Can't find or open file " & FUIName
    EndModule
  End If
End Sub

Private Sub Form_load()
  Dim mode As Long
  Dim thismod As String
  Dim cnt As Long
  Dim i As Long
  Dim mnode As ComctlLib.node
  
  ' Add first node.
  Set mnode = tvwBBF.Nodes.Add()
  With mnode
    .Text = "Exposure Media"
    .Tag = 0
    .Key = "BBF"
    .Expanded = True
  End With
  
  StartModule BBF, "FRAMES Known Body Burden Module", 6
  SetRefFile ReplaceExt(FUIName, "ref")
  BBF.Caption = "FRAMES Known Body Burden " & modName
  Loading.Show
  loadng = True
  c1 = 0
  c2 = 0
  c3 = 0
  c4 = 0
  SetSpreadUnits vaSpread1, 1, "yr"
  SetSpreadUnits vaSpread2, 1, "yr"
  SetSpreadUnits vaSpread2, 2, "pCi/kg"
  loadprm
  
  mode = Val(argv(0))
  If mode Mod 2 = 0 Then
    Unload Loading
    Make_BBF
    EndModule
  End If
  
  SetHelpFile App.Path + "\Known BBF.html"
  Unload Loading
  oldtext = ""
  RefItem = 6
  tvwBBF.SelectedItem = tvwBBF.Nodes(1)
  tvwBBF_nodeClick tvwBBF.SelectedItem
  loadng = False
End Sub

Private Sub Is_Ascending(time As Double, errmsg As String)
  If ((prevtime >= time) And (Not bnflag)) Then
    prevtime = time
    bnflag = True
    PutError errmsg
  Else
    prevtime = time
  End If
End Sub

Private Sub save_Click()
  Dim i As Long, j As Long, k As Long
  Dim l As Long, m As Long, n As Long
  Dim p As Long
  Dim fname As String
  Dim parm As parmrec
  Dim fle As parmfile
  
  prevtime = -1
  fname = RunName & ".GID"
  If c1 + c2 + c3 > 0 Then Combo1_Click
  If open_parm(fle, fname, 1) Then
    For i = 1 To numloc
      If loc(i).use Then
        set_parm parm, "LocName", i, 0, 0, 0, 0, 0, 0, "N/A", "N/A", loc(i).name
        write_parmrec fle, parm
        For p = 1 To loc(i).numspecie
          set_parm parm, "specname", i, p, 0, 0, 0, 0, 0, "N/A", "N/A", loc(i).specs(p).name
          write_sparmrec fle, parm
          set_parm parm, "specsciname", i, p, 0, 0, 0, 0, 0, "N/A", "N/A", loc(i).specs(p).sciname
          write_sparmrec fle, parm
          For j = 1 To numcon
            For k = 1 To loc(i).specs(p).numcon
              If loc(i).specs(p).con(k).cas = con(j).cas And loc(i).specs(p).use Then
                set_parm parm, "conccas", i, p, j, 0, 0, 0, loc(i).specs(p).con(k).ref, loc(i).specs(p).con(k).tunit(2), loc(i).specs(p).con(k).unit(2), con(j).cas
                write_sparmrec fle, parm
                If loc(i).specs(p).con(k).nts < 2 Then PutError "Less than two " & con(j).name & " time/concentration pairs for " & loc(i).specs(p).name & " at " & loc(i).name
                For l = 1 To loc(i).specs(p).con(k).nts
                  Is_Ascending loc(i).specs(p).con(k).series(l).time, con(j).name & " concentration time series out of sequence"
                  set_parm parm, "ctime", i, p, j, 0, l, 0, 0, loc(i).specs(p).con(k).tunit(1), loc(i).specs(p).con(k).tunit(2), convert(loc(i).specs(p).con(k).tunit(1), loc(i).specs(p).con(k).tunit(2), loc(i).specs(p).con(k).series(l).time)
                  write_parmrec fle, parm
                  set_parm parm, "conc", i, p, j, 0, l, 0, 0, loc(i).specs(p).con(k).unit(1), loc(i).specs(p).con(k).unit(2), convert(loc(i).specs(p).con(k).unit(1), loc(i).specs(p).con(k).unit(2), loc(i).specs(p).con(k).series(l).valu)
                  write_parmrec fle, parm
                Next
                prevtime = -1
                bnflag = False
                If prog.Checked Then
                  For l = 1 To con(j).numprog
                    For m = 1 To loc(i).specs(p).con(k).numprog
                      If loc(i).specs(p).con(k).prog(m).cas = con(j).prog(l).cas Then
                        set_parm parm, "conccas", i, p, j, l, 0, 0, loc(i).specs(p).con(k).prog(m).ref, loc(i).specs(p).con(k).prog(m).tunit(2), loc(i).specs(p).con(k).prog(m).unit(2), con(j).prog(l).cas
                        write_sparmrec fle, parm
                        If loc(i).specs(p).con(k).prog(m).nts < 2 Then PutError "Less than two " & con(j).prog(l).name & " progeny time/concentration pairs for " & loc(i).specs(p).name & " at " & loc(i).name
                        For n = 1 To loc(i).specs(p).con(k).prog(m).nts
                          Is_Ascending loc(i).specs(p).con(k).prog(m).series(n).time, con(j).prog(l).name & " concentration time series out of sequence"
                          set_parm parm, "ctime", i, p, j, l, n, 0, 0, loc(i).specs(p).con(k).prog(m).tunit(1), loc(i).specs(p).con(k).prog(m).tunit(2), convert(loc(i).specs(p).con(k).prog(m).tunit(1), loc(i).specs(p).con(k).prog(m).tunit(2), loc(i).specs(p).con(k).prog(m).series(n).time)
                          write_parmrec fle, parm
                          set_parm parm, "conc", i, p, j, l, n, 0, 0, loc(i).specs(p).con(k).prog(m).unit(1), loc(i).specs(p).con(k).prog(m).unit(2), convert(loc(i).specs(p).con(k).prog(m).unit(1), loc(i).specs(p).con(k).prog(m).unit(2), loc(i).specs(p).con(k).prog(m).series(n).valu)
                          write_parmrec fle, parm
                        Next
                        prevtime = -1
                        bnflag = False
                      End If
                    Next
                  Next
                End If
              End If
            Next
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

Private Function Make_BBF()
  Dim i As Long, j As Long, k As Long
  Dim l As Long, m As Long, n As Long, p As Long
  Dim msg As String
  Dim cnt As Long
  Dim file As csv
  If open_csv(file, RunName & ".BBF", 1) Then
    
    PutHeader file
    cnt = 0
    For i = 1 To numloc
      If loc(i).use Then cnt = cnt + 1
    Next
    put_val file, cnt
    put_line file
    For i = 1 To numloc
      If loc(i).use Then
        put_val file, ""
        put_val file, loc(i).name
        put_val file, loc(i).numspecie
        put_val file, 1
        put_val file, 1
        put_line file
        put_val file, "Discrete"
        put_line file
        put_val file, "Discrete"
        put_line file
        For p = 1 To loc(i).numspecie
          put_val file, loc(i).specs(p).sciname
          put_val file, numcon
          put_line file
          For j = 1 To numcon
            For k = 1 To loc(i).specs(p).numcon
              If loc(i).specs(p).con(k).cas = con(j).cas Then
                put_val file, con(j).name
                put_sval file, con(j).cas
                put_val file, loc(i).specs(p).con(k).tunit(2)
                put_val file, loc(i).specs(p).con(k).unit(2)
                put_val file, loc(i).specs(p).con(k).nts
                If prog.Checked Then
                  put_val file, loc(i).specs(p).con(k).numprog
                Else
                  put_val file, 0
                End If
                put_line file
                For l = 1 To loc(i).specs(p).con(k).nts
                  put_val file, convert(loc(i).specs(p).con(k).tunit(1), loc(i).specs(p).con(k).tunit(2), loc(i).specs(p).con(k).series(l).time)
                  put_val file, convert(loc(i).specs(p).con(k).unit(1), loc(i).specs(p).con(k).unit(2), loc(i).specs(p).con(k).series(l).valu)
                  put_line file
                Next
                If prog.Checked Then
                  For l = 1 To con(j).numprog
                    For m = 1 To loc(i).specs(p).con(k).numprog
                      If loc(i).specs(p).con(k).prog(m).cas = con(j).prog(l).cas Then
                        put_val file, con(j).prog(l).name
                        put_sval file, con(j).prog(l).cas
                        put_val file, loc(i).specs(p).con(k).prog(m).tunit(2)
                        put_val file, loc(i).specs(p).con(k).prog(m).unit(2)
                        put_val file, loc(i).specs(p).con(k).prog(m).nts
                        put_val file, con(j).name
                        put_val file, con(j).cas
                        put_line file
                        For n = 1 To loc(i).specs(p).con(k).prog(m).nts
                          put_val file, convert(loc(i).specs(p).con(k).prog(m).tunit(1), loc(i).specs(p).con(k).prog(m).tunit(2), loc(i).specs(p).con(k).prog(m).series(n).time)
                          put_val file, convert(loc(i).specs(p).con(k).prog(m).unit(1), loc(i).specs(p).con(k).prog(m).unit(2), loc(i).specs(p).con(k).prog(m).series(n).valu)
                          put_line file
                        Next
                      End If
                    Next
                  Next
                End If
              End If
            Next
          Next
        Next
      End If
    Next
    close_csv file
  Else
    PutError "Unable to create water valurec file" & RunName & ".WCF"
  End If
End Function

Private Sub getparent_wcf()
  Dim j As Long
  Dim temp As String
  
  vaSpread1.col = 1
  vaSpread1.Row = 1
  loc(c1).specs(c2).con(c3).tunit(1) = vaSpread1.Text
  vaSpread1.col = 2
  loc(c1).specs(c2).con(c3).unit(1) = vaSpread1.Text
  loc(c1).specs(c2).con(c3).ref = ref(6).Tag
  loc(c1).specs(c2).con(c3).nts = vaSpread1.DataRowCnt - 1
  ReDim loc(c1).specs(c2).con(c3).series(loc(c1).specs(c2).con(c3).nts)

  For j = 2 To loc(c1).specs(c2).con(c3).nts + 1
    vaSpread1.col = 1
    vaSpread1.Row = j
    temp = vaSpread1.Value
    If Len(temp) > 0 Then
      loc(c1).specs(c2).con(c3).series(j - 1).time = Val(vaSpread1.Value)
      vaSpread1.Text = ""
      vaSpread1.col = 2
      loc(c1).specs(c2).con(c3).series(j - 1).valu = Val(vaSpread1.Value)
      vaSpread1.Text = ""
    Else
      loc(c1).specs(c2).con(c3).nts = j - 2
      Exit For
    End If
  Next
End Sub

Private Sub putparent_wcf()
  Dim j As Long
  
  ClearSpread vaSpread1
  ref(6).Caption = "Ref: " + Str(loc(c1).specs(c2).con(c3).ref)
  ref(6).Tag = loc(c1).specs(c2).con(c3).ref
  vaSpread1.col = 1
  vaSpread1.Row = 1
  vaSpread1.TypeComboBoxEditable = True
  vaSpread1.Text = loc(c1).specs(c2).con(c3).tunit(1)
  vaSpread1.TypeComboBoxEditable = False
  vaSpread1.col = 2
  SetSpreadUnits vaSpread1, 2, loc(c1).specs(c2).con(c3).unit(1)
  vaSpread1.TypeComboBoxEditable = True
  vaSpread1.Text = loc(c1).specs(c2).con(c3).unit(1)
  vaSpread1.TypeComboBoxEditable = False
  For j = 1 To loc(c1).specs(c2).con(c3).nts
    vaSpread1.Row = j + 1
    vaSpread1.col = 1
    vaSpread1.Text = loc(c1).specs(c2).con(c3).series(j).time
    vaSpread1.col = 2
    vaSpread1.Text = loc(c1).specs(c2).con(c3).series(j).valu
  Next
End Sub

Private Sub getprog_wcf()
  Dim j As Long
  Dim temp As String
  
  If loc(c1).specs(c2).con(c3).numprog = 0 Then Exit Sub
  vaSpread2.col = 1
  vaSpread2.Row = 1
  loc(c1).specs(c2).con(c3).prog(c4).tunit(1) = vaSpread2.Text
  vaSpread2.col = 2
  loc(c1).specs(c2).con(c3).prog(c4).unit(1) = vaSpread2.Text
  loc(c1).specs(c2).con(c3).prog(c4).ref = ref(7).Tag
  loc(c1).specs(c2).con(c3).prog(c4).nts = vaSpread2.DataRowCnt - 1
  ReDim loc(c1).specs(c2).con(c3).prog(c4).series(loc(c1).specs(c2).con(c3).prog(c4).nts)
  For j = 2 To loc(c1).specs(c2).con(c3).prog(c4).nts + 1
    vaSpread2.col = 1
    vaSpread2.Row = j
    temp = vaSpread2.Value
    If Len(temp) > 0 Then
      loc(c1).specs(c2).con(c3).prog(c4).series(j - 1).time = Val(vaSpread2.Value)
      vaSpread2.Text = ""
      vaSpread2.col = 2
      loc(c1).specs(c2).con(c3).prog(c4).series(j - 1).valu = Val(vaSpread2.Value)
      vaSpread2.Text = ""
    Else
      loc(c1).specs(c2).con(c3).prog(c4).nts = j - 2
      Exit For
    End If
  Next
End Sub

Private Sub putprog_wcf()
  Dim j As Long
  
  ClearSpread vaSpread2
  If loc(c1).specs(c2).con(c3).numprog = 0 Then Exit Sub
  ref(7).Caption = "Ref: " + Str(loc(c1).specs(c2).con(c3).prog(c4).ref)
  ref(7).Tag = loc(c1).specs(c2).con(c3).prog(c4).ref
  vaSpread2.col = 1
  vaSpread2.Row = 1
  vaSpread2.TypeComboBoxEditable = True
  vaSpread2.Text = loc(c1).specs(c2).con(c3).prog(c4).tunit(1)
  vaSpread2.TypeComboBoxEditable = False
  vaSpread2.col = 2
  SetSpreadUnits vaSpread2, 2, loc(c1).specs(c2).con(c3).prog(c4).unit(1)
  vaSpread2.TypeComboBoxEditable = True
  vaSpread2.Text = loc(c1).specs(c2).con(c3).prog(c4).unit(1)
  vaSpread2.TypeComboBoxEditable = False
  For j = 1 To loc(c1).specs(c2).con(c3).prog(c4).nts
    vaSpread2.Row = j + 1
    vaSpread2.col = 1
    vaSpread2.Text = loc(c1).specs(c2).con(c3).prog(c4).series(j).time
    vaSpread2.col = 2
    vaSpread2.Text = loc(c1).specs(c2).con(c3).prog(c4).series(j).valu
  Next
End Sub

Function AddLoc(name) As Long
Dim i As Long
Dim j As Long
Dim k As Long
Dim m As Long

  For i = 1 To numloc
    If loc(i).name = name Then
      AddLoc = i
      Exit Function
    End If
  Next
  numloc = i
  ReDim Preserve loc(i) As BBFLoc
  loc(i).name = name
  loc(i).numspecie = numspec
  ReDim Preserve loc(i).specs(numspec) As Specie
  For j = 1 To numspec
    loc(i).numspecie = loc(i).numspecie
    loc(i).specs(j).name = specname(j)
    loc(i).specs(j).sciname = specsciname(j)
    loc(i).specs(j).use = j
    loc(i).specs(j).numcon = numcon
    ReDim loc(i).specs(j).con(numcon) As sParent
    For k = 1 To numcon
      loc(i).specs(j).con(k).use = k
      loc(i).specs(j).con(k).cas = con(k).cas
      loc(i).specs(j).con(k).numprog = con(k).numprog
      loc(i).specs(j).con(k).tunit(1) = "yr"
      loc(i).specs(j).con(k).tunit(2) = "yr"
      If con(k).kind = 1 Then
        loc(i).specs(j).con(k).unit(1) = "pCi/kg"
        loc(i).specs(j).con(k).unit(2) = "pCi/kg"
      Else
        loc(i).specs(j).con(k).unit(1) = "mg/kg"
        loc(i).specs(j).con(k).unit(2) = "mg/kg"
      End If
      ReDim loc(i).specs(j).con(k).prog(con(k).numprog) As sProgeny
      For m = 1 To con(k).numprog
        loc(i).specs(j).con(k).prog(m).use = m
        loc(i).specs(j).con(k).prog(m).cas = con(k).prog(m).cas
        loc(i).specs(j).con(k).prog(m).tunit(1) = "yr"
        loc(i).specs(j).con(k).prog(m).tunit(2) = "yr"
        If con(k).kind = 1 Then
          loc(i).specs(j).con(k).prog(m).unit(1) = "pCi/kg"
          loc(i).specs(j).con(k).prog(m).unit(2) = "pCi/kg"
        Else
          loc(i).specs(j).con(k).prog(m).unit(1) = "mg/kg"
          loc(i).specs(j).con(k).prog(m).unit(2) = "mg/kg"
        End If
      Next
    Next
  Next
  AddLoc = i
End Function

Sub LoadlstMedia()
Dim i As Long
  
  lstMedia.Clear
  For i = 1 To numloc
    If loc(i).use Then
      lstMedia.AddItem loc(i).name
      lstMedia.ItemData(lstMedia.NewIndex) = i
    End If
  Next
End Sub

Private Sub tvwBBF_nodeClick(ByVal node As ComctlLib.node)
Dim i As Integer

  If node.Tag = 0 Then
    Frame1.Visible = True
    Frame2.Visible = False
  Else
    If Not loadng And c1 > 0 Then
      getparent_wcf
      getprog_wcf
    End If
    
    c1 = node.Tag
    Combo1.Clear
    For i = 1 To loc(c1).numspecie
      If loc(c1).specs(i).use Then
        Combo1.AddItem loc(c1).specs(i).name
        Combo1.ItemData(Combo1.NewIndex) = i
      End If
    Next
    c1click = True
    If Combo1.ListIndex < 0 And Combo1.ListCount > 0 Then
      Combo1.ListIndex = 0
    Else
      Combo1_Click
    End If
    c1click = False
    Frame2.Caption = "Exposure due to " & loc(c1).name
    Frame1.Visible = False
    Frame2.Visible = True
  End If
End Sub

Private Sub Combo1_Click()
Dim i As Long

  If Not c1click Then
    getparent_wcf
    getprog_wcf
  End If
  
'  c2 = Combo1.ListIndex + 1
  If Combo1.ListCount < 1 Then Exit Sub
  c2 = Combo1.ItemData(Combo1.ListIndex)
  Combo2.Clear
  For i = 1 To loc(c1).specs(c2).numcon
    If loc(c1).specs(c2).con(i).use > 0 Then
      Combo2.AddItem con(loc(c1).specs(c2).con(i).use).name
      Combo2.ItemData(Combo2.NewIndex) = i
    End If
  Next
  c2click = True
  Combo2.ListIndex = 0
  c2click = False
End Sub

Private Sub Combo2_Click()
Dim i As Long
  
  If Not c2click Then
    getparent_wcf
    getprog_wcf
  End If
  
  If Combo2.ListCount < 1 Then Exit Sub
  c3 = Combo2.ItemData(Combo2.ListIndex)
  putparent_wcf
  If loc(c1).specs(c2).con(c3).numprog = 0 Then
    lbl(7).Visible = False
    vaSpread2.Visible = False
    Combo3.Visible = False
    ref(7).Visible = False
    SSCommand5.Visible = False
    SSCommand6.Visible = False
    Exit Sub
  Else
    If prog.Checked Then
      lbl(7).Visible = True
      vaSpread2.Visible = True
      Combo3.Visible = True
      ref(7).Visible = True
      SSCommand5.Visible = True
      SSCommand6.Visible = True
      Combo3.Clear
      For i = 1 To loc(c1).specs(c2).con(c3).numprog
        If loc(c1).specs(c2).con(c3).prog(i).use > 0 Then
          Combo3.AddItem con(loc(c1).specs(c2).con(c3).use).prog(loc(c1).specs(c2).con(c3).prog(i).use).name
          Combo3.ItemData(Combo3.NewIndex) = i
        End If
      Next
      c3click = True
      Combo3.ListIndex = 0
      c3click = False
    End If
  End If
End Sub

Private Sub Combo3_Click()
  If Not c3click Then
    getprog_wcf
  End If
  c4 = Combo3.ItemData(Combo3.ListIndex)
  putprog_wcf
End Sub

Private Sub Command1_Click()
Dim idx As Long
Dim medium As String
Dim mnode As ComctlLib.node

  medium = Trim(InputBox("Enter a medium exposing organism", "Exposure Medium Input"))
  If medium = "" Then Exit Sub
  idx = AddLoc(medium)
  If loc(idx).use Then
    MsgBox "'" & medium & "' already in use!"
  Else
    loc(idx).use = True
    Set mnode = tvwBBF.Nodes.Add("BBF", tvwChild, medium, medium)
    mnode.Tag = idx
    mnode.Expanded = True
    mnode.EnsureVisible
    LoadlstMedia
  End If
End Sub

Private Sub Command2_Click()
  If lstMedia.ListIndex < 0 Then Exit Sub
  loc(lstMedia.ItemData(lstMedia.ListIndex)).use = False
  tvwBBF.Nodes.Remove lstMedia.Text
  lstMedia.RemoveItem lstMedia.ListIndex
End Sub

Private Sub Command3_Click()
Dim idx As Long
Dim answer As Integer
Dim medium As String
Dim mnode As ComctlLib.node

  If lstMedia.ListIndex < 0 Then Exit Sub
  medium = Trim(InputBox("Enter a medium exposing organism", "Exposure Medium Input"))
  If medium = "" Then Exit Sub
  For idx = 1 To numloc
    If loc(idx).name = medium Then
      If Not loc(idx).use Then
        answer = MsgBox("'" & medium & "' already in use for deleted media!" & vbCrLf & "Would like to restore deleted media?", vbYesNo)
        If answer = 6 Then
          loc(idx).use = True
          Set mnode = tvwBBF.Nodes.Add("BBF", tvwChild, medium, medium)
          mnode.Tag = idx
          mnode.Expanded = True
          mnode.EnsureVisible
          LoadlstMedia
          Exit Sub
        Else
          loc(idx).name = " Del" & CStr(idx)
        End If
      Else
        MsgBox medium & " already in use!"
        Exit Sub
      End If
    End If
  Next
  idx = lstMedia.ItemData(lstMedia.ListIndex)
  loc(idx).name = medium
  tvwBBF.Nodes.Remove lstMedia.Text
  Set mnode = tvwBBF.Nodes.Add("BBF", tvwChild, medium, medium)
  mnode.Tag = idx
  mnode.Expanded = True
  mnode.EnsureVisible
  LoadlstMedia
End Sub

Private Sub SSCommand1_Click()
  If Combo1.ListIndex > 0 Then Combo1.ListIndex = Combo1.ListIndex - 1
End Sub

Private Sub SSCommand2_Click()
  If Combo1.ListIndex < Combo1.ListCount - 1 Then Combo1.ListIndex = Combo1.ListIndex + 1
End Sub

Private Sub SSCommand3_Click()
  If Combo2.ListIndex > 0 Then Combo2.ListIndex = Combo2.ListIndex - 1
End Sub

Private Sub SSCommand4_Click()
  If Combo2.ListIndex < Combo2.ListCount - 1 Then Combo2.ListIndex = Combo2.ListIndex + 1
End Sub

Private Sub SSCommand5_Click()
  If Combo3.ListIndex > 0 Then Combo3.ListIndex = Combo3.ListIndex - 1
End Sub

Private Sub SSCommand6_Click()
  If Combo3.ListIndex < Combo3.ListCount - 1 Then Combo3.ListIndex = Combo3.ListIndex + 1
End Sub

Private Sub Command1_GotFocus()
  RefItem = -1
  noact.Enabled = False
  HelpAnchor = Command1.Tag
End Sub

Private Sub Command2_GotFocus()
  RefItem = -1
  noact.Enabled = False
  HelpAnchor = Command2.Tag
End Sub

Private Sub Command3_GotFocus()
  RefItem = -1
  noact.Enabled = False
  HelpAnchor = Command3.Tag
End Sub

Private Sub lstMedia_GotFocus()
  RefItem = -1
  noact.Enabled = False
  HelpAnchor = lstMedia.Tag
End Sub

Private Sub SSCommand1_GotFocus()
  RefItem = -1
  noact.Enabled = False
  HelpAnchor = SSCommand1.Tag
End Sub

Private Sub SSCommand2_GotFocus()
  RefItem = -1
  noact.Enabled = False
  HelpAnchor = SSCommand2.Tag
End Sub

Private Sub SSCommand3_GotFocus()
  RefItem = 6
  noact.Enabled = True
  HelpAnchor = SSCommand3.Tag
End Sub

Private Sub SSCommand4_GotFocus()
  RefItem = 6
  noact.Enabled = True
  HelpAnchor = SSCommand4.Tag
End Sub

Private Sub SSCommand5_GotFocus()
  RefItem = 7
  noact.Enabled = True
  HelpAnchor = SSCommand5.Tag
End Sub

Private Sub SSCommand6_GotFocus()
  RefItem = 7
  noact.Enabled = True
  HelpAnchor = SSCommand6.Tag
End Sub

Private Sub Combo1_GotFocus()
  RefItem = -1
  noact.Enabled = False
  HelpAnchor = Combo1.Tag
End Sub

Private Sub Combo2_GotFocus()
  RefItem = 6
  noact.Enabled = True
  HelpAnchor = Combo2.Tag
End Sub

Private Sub Combo3_GotFocus()
  RefItem = 7
  noact.Enabled = True
  HelpAnchor = Combo3.Tag
End Sub

Private Sub tvwBBF_GotFocus()
  HelpAnchor = tvwBBF.Tag
End Sub

Private Sub vaSpread1_GotFocus()
  RefItem = 6
  noact.Enabled = True
  HelpAnchor = vaSpread1.Tag
End Sub

Private Sub vaSpread2_GotFocus()
  RefItem = 7
  noact.Enabled = True
  HelpAnchor = vaSpread2.Tag
End Sub
'
'Private Sub vaSpread1_LeaveCell(ByVal Col As Long, ByVal Row As Long, ByVal NewCol As Long, ByVal NewRow As Long, Cancel As Boolean)
'  Combo2_Click
'End Sub
'
'Private Sub vaSpread2_LeaveCell(ByVal Col As Long, ByVal Row As Long, ByVal NewCol As Long, ByVal NewRow As Long, Cancel As Boolean)
'  Combo3_Click
'End Sub
Private Sub Form_KeyPress(KeyAscii As Integer)
  If KeyAscii = 13 Then KeyAscii = 0
End Sub

Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
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
