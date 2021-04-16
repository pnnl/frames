VERSION 5.00
Object = "{6B7E6392-850A-101B-AFC0-4210102A8DA7}#1.3#0"; "COMCTL32.OCX"
Object = "{F856EC8B-F03C-4515-BDC6-64CBD617566A}#7.0#0"; "FPSPR70.ocx"
Begin VB.Form frmEPF 
   BorderStyle     =   1  'Fixed Single
   ClientHeight    =   5460
   ClientLeft      =   5490
   ClientTop       =   4350
   ClientWidth     =   7320
   Icon            =   "Epf.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   364
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   488
   StartUpPosition =   2  'CenterScreen
   Begin VB.Frame fraEPF 
      Height          =   5200
      Left            =   3888
      TabIndex        =   24
      Top             =   120
      Width           =   3285
      Begin VB.ListBox lstMedia 
         Height          =   1230
         ItemData        =   "Epf.frx":030A
         Left            =   192
         List            =   "Epf.frx":0320
         MultiSelect     =   1  'Simple
         TabIndex        =   25
         Tag             =   "medium"
         Top             =   3600
         Width           =   2892
      End
      Begin VB.Label Label2 
         Caption         =   $"Epf.frx":0359
         ForeColor       =   &H000000C0&
         Height          =   1215
         Left            =   240
         TabIndex        =   39
         Top             =   2400
         Width           =   2895
      End
      Begin VB.Label Label1 
         Caption         =   $"Epf.frx":0415
         Height          =   855
         Left            =   195
         TabIndex        =   27
         Top             =   1395
         Width           =   2895
      End
      Begin VB.Label Label8 
         Caption         =   $"Epf.frx":049D
         Height          =   1356
         Left            =   192
         TabIndex        =   26
         Top             =   288
         Width           =   2892
      End
   End
   Begin VB.Frame fraMedia 
      Height          =   5200
      Left            =   3888
      TabIndex        =   28
      Top             =   120
      Width           =   3285
      Begin VB.TextBox txt 
         Alignment       =   1  'Right Justify
         Height          =   288
         Index           =   3
         Left            =   1128
         TabIndex        =   37
         Top             =   384
         Width           =   852
      End
      Begin VB.ComboBox unit 
         Height          =   288
         Index           =   3
         ItemData        =   "Epf.frx":0553
         Left            =   1968
         List            =   "Epf.frx":0555
         Style           =   2  'Dropdown List
         TabIndex        =   36
         Tag             =   "yr"
         Top             =   384
         Width           =   1000
      End
      Begin VB.ComboBox unit 
         Height          =   288
         Index           =   4
         ItemData        =   "Epf.frx":0557
         Left            =   1968
         List            =   "Epf.frx":0559
         Style           =   2  'Dropdown List
         TabIndex        =   31
         Tag             =   "km"
         Top             =   840
         Width           =   1000
      End
      Begin FPSpreadADO.fpSpread vaSpread1 
         Height          =   1176
         Left            =   192
         TabIndex        =   32
         Tag             =   "coordinates"
         Top             =   1560
         Width           =   1224
         _Version        =   458752
         _ExtentX        =   4948
         _ExtentY        =   5106
         _StockProps     =   64
         ArrowsExitEditMode=   -1  'True
         AutoCalc        =   0   'False
         AutoSize        =   -1  'True
         DisplayRowHeaders=   0   'False
         EditEnterAction =   5
         EditModeReplace =   -1  'True
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "MS Sans Serif"
            Size            =   8.25
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
         SpreadDesigner  =   "Epf.frx":055B
         StartingColNumber=   0
         StartingRowNumber=   0
         VisibleCols     =   2
         VisibleRows     =   11
      End
      Begin VB.OptionButton optDataSet 
         Caption         =   "Chronic"
         Height          =   255
         Index           =   0
         Left            =   240
         TabIndex        =   29
         Tag             =   "acute"
         Top             =   4560
         Value           =   -1  'True
         Visible         =   0   'False
         Width           =   1200
      End
      Begin VB.OptionButton optDataSet 
         Caption         =   "Acute"
         Height          =   255
         Index           =   1
         Left            =   240
         TabIndex        =   30
         Tag             =   "acute"
         Top             =   4560
         Visible         =   0   'False
         Width           =   1200
      End
      Begin VB.Label lbl 
         Caption         =   "Duration"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   252
         Index           =   3
         Left            =   192
         TabIndex        =   38
         Tag             =   "start"
         Top             =   360
         Width           =   876
      End
      Begin VB.Label lbl 
         Caption         =   "Coordinates in"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   252
         Index           =   4
         Left            =   192
         TabIndex        =   35
         Tag             =   "coordinates"
         Top             =   840
         Width           =   1596
      End
      Begin VB.Label key 
         Caption         =   "key"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   252
         Index           =   1
         Left            =   2496
         TabIndex        =   34
         Top             =   1344
         Visible         =   0   'False
         Width           =   372
      End
      Begin VB.Label ref 
         Caption         =   "Ref:"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   252
         Index           =   1
         Left            =   192
         TabIndex        =   33
         Tag             =   "0"
         Top             =   1344
         Width           =   1212
      End
   End
   Begin ComctlLib.TreeView tvwEPF 
      Height          =   5172
      Left            =   120
      TabIndex        =   0
      Top             =   168
      Width           =   3648
      _ExtentX        =   6429
      _ExtentY        =   9128
      _Version        =   327682
      HideSelection   =   0   'False
      Indentation     =   176
      LabelEdit       =   1
      Sorted          =   -1  'True
      Style           =   7
      ImageList       =   "imlSmallIcons"
      BorderStyle     =   1
      Appearance      =   1
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
   End
   Begin VB.Frame fraExposure 
      Height          =   5200
      Left            =   3888
      TabIndex        =   1
      Top             =   120
      Width           =   3285
      Begin VB.ListBox lstExposure 
         Height          =   840
         Index           =   4
         ItemData        =   "Epf.frx":6DB7
         Left            =   240
         List            =   "Epf.frx":6DC4
         MultiSelect     =   1  'Simple
         Sorted          =   -1  'True
         TabIndex        =   5
         Tag             =   "parent"
         Top             =   3840
         Width           =   1308
      End
      Begin VB.ListBox lstExposure 
         Height          =   840
         Index           =   3
         ItemData        =   "Epf.frx":6DE2
         Left            =   1800
         List            =   "Epf.frx":6DF5
         MultiSelect     =   1  'Simple
         Sorted          =   -1  'True
         TabIndex        =   4
         Tag             =   "parent"
         Top             =   3840
         Width           =   1332
      End
      Begin VB.ListBox lstExposure 
         Height          =   2400
         Index           =   2
         ItemData        =   "Epf.frx":6E28
         Left            =   1800
         List            =   "Epf.frx":6E3B
         MultiSelect     =   1  'Simple
         Sorted          =   -1  'True
         TabIndex        =   3
         Tag             =   "parent"
         Top             =   480
         Width           =   1308
      End
      Begin VB.ListBox lstExposure 
         Height          =   2400
         Index           =   1
         ItemData        =   "Epf.frx":6E68
         Left            =   240
         List            =   "Epf.frx":6E99
         MultiSelect     =   1  'Simple
         Sorted          =   -1  'True
         TabIndex        =   2
         Tag             =   "parent"
         Top             =   480
         Width           =   1308
      End
      Begin VB.Label Label7 
         Caption         =   "External"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   252
         Left            =   240
         TabIndex        =   9
         Top             =   3576
         Width           =   1332
      End
      Begin VB.Label Label6 
         Caption         =   "Dermal"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   252
         Left            =   1812
         TabIndex        =   8
         Top             =   3588
         Width           =   972
      End
      Begin VB.Label Label5 
         Caption         =   "Inhalation"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   252
         Left            =   1800
         TabIndex        =   7
         Top             =   240
         Width           =   1332
      End
      Begin VB.Label Label4 
         Caption         =   "Ingestion"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   252
         Left            =   240
         TabIndex        =   6
         Top             =   240
         Width           =   972
      End
   End
   Begin VB.Frame fraChemical 
      Height          =   5200
      Left            =   3888
      TabIndex        =   18
      Top             =   120
      Width           =   3285
      Begin VB.ComboBox unit 
         Height          =   288
         Index           =   2
         ItemData        =   "Epf.frx":6F29
         Left            =   2040
         List            =   "Epf.frx":6F2B
         Style           =   2  'Dropdown List
         TabIndex        =   20
         Tag             =   "yr"
         Top             =   384
         Width           =   1000
      End
      Begin FPSpreadADO.fpSpread vaSpread2 
         Height          =   888
         Left            =   192
         TabIndex        =   19
         Tag             =   "start"
         Top             =   960
         Width           =   1284
         _Version        =   458752
         _ExtentX        =   2275
         _ExtentY        =   7223
         _StockProps     =   64
         ArrowsExitEditMode=   -1  'True
         AutoCalc        =   0   'False
         AutoSize        =   -1  'True
         DisplayRowHeaders=   0   'False
         EditEnterAction =   5
         EditModeReplace =   -1  'True
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         FormulaSync     =   0   'False
         MaxCols         =   1
         MaxRows         =   5000
         RowsFrozen      =   1
         ScrollBarExtMode=   -1  'True
         ScrollBarShowMax=   0   'False
         SpreadDesigner  =   "Epf.frx":6F2D
         StartingColNumber=   0
         StartingRowNumber=   0
         UserResize      =   0
         VisibleCols     =   2
         VisibleRows     =   15
      End
      Begin VB.Label ref 
         Caption         =   "Ref:"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   252
         Index           =   2
         Left            =   192
         TabIndex        =   23
         Tag             =   "0"
         Top             =   744
         Width           =   1212
      End
      Begin VB.Label key 
         Caption         =   "key"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   252
         Index           =   2
         Left            =   2400
         TabIndex        =   22
         Top             =   744
         Visible         =   0   'False
         Width           =   372
      End
      Begin VB.Label lbl 
         Caption         =   "Time in"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   252
         Index           =   2
         Left            =   192
         TabIndex        =   21
         Tag             =   "start"
         Top             =   384
         Width           =   1600
      End
   End
   Begin VB.Frame fraConcentration 
      Height          =   5200
      Left            =   3888
      TabIndex        =   10
      Top             =   120
      Width           =   3285
      Begin VB.ComboBox unit 
         Enabled         =   0   'False
         Height          =   288
         Index           =   0
         ItemData        =   "Epf.frx":A4AD
         Left            =   1824
         List            =   "Epf.frx":A4AF
         Style           =   2  'Dropdown List
         TabIndex        =   12
         Tag             =   "yr"
         Top             =   384
         Width           =   1000
      End
      Begin VB.ComboBox unit 
         Height          =   288
         Index           =   1
         ItemData        =   "Epf.frx":A4B1
         Left            =   1824
         List            =   "Epf.frx":A4B3
         Style           =   2  'Dropdown List
         TabIndex        =   11
         Top             =   840
         Width           =   1000
      End
      Begin FPSpreadADO.fpSpread vaSpread3 
         Height          =   3060
         Left            =   195
         TabIndex        =   13
         Tag             =   "timecon"
         Top             =   1635
         Width           =   2145
         _Version        =   458752
         _ExtentX        =   3768
         _ExtentY        =   5165
         _StockProps     =   64
         ArrowsExitEditMode=   -1  'True
         AutoCalc        =   0   'False
         ColsFrozen      =   1
         DisplayRowHeaders=   0   'False
         EditEnterAction =   5
         EditModeReplace =   -1  'True
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         FormulaSync     =   0   'False
         MaxRows         =   5000
         ScrollBarExtMode=   -1  'True
         ScrollBarShowMax=   0   'False
         SpreadDesigner  =   "Epf.frx":A4B5
         VisibleCols     =   2
         VisibleRows     =   10
      End
      Begin VB.Label ref 
         Caption         =   "Ref:"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   252
         Index           =   3
         Left            =   192
         TabIndex        =   17
         Tag             =   "0"
         Top             =   1344
         Width           =   1212
      End
      Begin VB.Label key 
         Caption         =   "key"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   252
         Index           =   3
         Left            =   2496
         TabIndex        =   16
         Top             =   1344
         Visible         =   0   'False
         Width           =   372
      End
      Begin VB.Label lbl 
         Caption         =   "Concentrations in"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Index           =   1
         Left            =   195
         TabIndex        =   15
         Tag             =   "timecon"
         Top             =   840
         Width           =   1605
      End
      Begin VB.Label lbl 
         Caption         =   "Time in"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   252
         Index           =   0
         Left            =   192
         TabIndex        =   14
         Tag             =   "timecon"
         Top             =   384
         Width           =   1600
      End
   End
   Begin ComctlLib.ImageList imlSmallIcons 
      Left            =   3192
      Top             =   168
      _ExtentX        =   1005
      _ExtentY        =   1005
      BackColor       =   -2147483643
      ImageWidth      =   13
      ImageHeight     =   13
      MaskColor       =   12632256
      _Version        =   327682
      BeginProperty Images {0713E8C2-850A-101B-AFC0-4210102A8DA7} 
         NumListImages   =   6
         BeginProperty ListImage1 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "Epf.frx":A781
            Key             =   "closed"
         EndProperty
         BeginProperty ListImage2 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "Epf.frx":ACA3
            Key             =   "cylinder"
         EndProperty
         BeginProperty ListImage3 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "Epf.frx":B1C5
            Key             =   "leaf"
         EndProperty
         BeginProperty ListImage4 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "Epf.frx":B6E7
            Key             =   "open"
         EndProperty
         BeginProperty ListImage5 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "Epf.frx":BC09
            Key             =   "smlBook"
         EndProperty
         BeginProperty ListImage6 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "Epf.frx":C26B
            Key             =   ""
         EndProperty
      EndProperty
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
Attribute VB_Name = "frmEPF"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text
Public mNode As ComctlLib.Node
Dim numtime As Long
Dim kind As String
Dim temp As parmrec
Dim loadng As Boolean
Dim deselect As Boolean
Dim paths(22) As String
Dim mediakind As Long
Dim pathunit(21) As String
Dim ExpPathOK(3, 4) As New Collection

Private Sub howto_Click()
  GetHelp
End Sub
 
Private Sub about_Click()
  frmAbout.picIcon = frmAbout.ImageList1.ListImages(1).Picture
  frmAbout.Show 1, frmEPF
End Sub


Private Sub Form_Unload(Cancel As Integer)
  Dim answer As Long
  If Not called Then
    answer = MsgBox("Do you want save changes?", 51, frmEPF.Caption)
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

Function PathIndex(i As Long, j As Long, k As Long, route As String, Path As String) As Long
Dim m As Long

  PathIndex = 0
  If k = 0 Then
    If med(i).chem(j).numexp = 0 Then Exit Function
    For m = 1 To med(i).chem(j).numexp
      If Path = med(i).chem(j).exp(m).Path Then
        If route = med(i).chem(j).exp(m).route Then
          If med(i).chem(j).exp(m).use = 1 Then
            PathIndex = m
          Else
            PathIndex = -m
          End If
          Exit Function
        End If
      End If
    Next
  Else
    If med(i).chem(j).prog(k).numexp = 0 Then Exit Function
    For m = 1 To med(i).chem(j).prog(k).numexp
      If Path = med(i).chem(j).prog(k).exp(m).Path Then
        If route = med(i).chem(j).prog(k).exp(m).route Then
          If med(i).chem(j).prog(k).exp(m).use = 1 Then
            PathIndex = m
          Else
            PathIndex = -m
          End If
          Exit Function
        End If
      End If
    Next
  End If
End Function

Function getRoute(Index) As String
  Select Case Index
    Case 1:        getRoute = "Ingestion"
    Case 2:        getRoute = "Inhalation"
    Case 3:        getRoute = "Dermal"
    Case 4:        getRoute = "External"
  End Select
End Function

Function getRouteBox(Index As String) As Long
  Select Case Index
    Case "Ingestion":   getRouteBox = 1
    Case "Inhalation":  getRouteBox = 2
    Case "Dermal":      getRouteBox = 3
    Case "External":    getRouteBox = 4
  End Select
End Function

Function getMedia() As Integer
  Select Case med(temp.idx1).Name
    Case "Air":                        getMedia = 0
    Case "Aquifer":                    getMedia = 1
    Case "Surface Water", "Wetlands":  getMedia = 2
    Case "Soil", "Ground":             getMedia = 3
  End Select
End Function

Sub SetMedIndex(key1 As String)
Dim fle As csv
  
  fle.fnum = 1
  fle.separator = ","
  fle.getbuff = key1
  fle.leng = Len(fle.getbuff)
  get_val fle
  temp.idx1 = Val(get_val(fle))
  temp.idx2 = Val(get_val(fle))
  temp.idx3 = Val(get_val(fle))
  temp.idx4 = Val(get_val(fle))
  get_val fle
  temp.idx5 = Val(get_val(fle))
  temp.idx6 = Val(get_val(fle))

End Sub

Private Sub lstExposure_Click(Index As Integer)
Dim EIndex As Long
Dim route As String
Dim num As Long
  
  If deselect Then Exit Sub
  
  tvwEPF.Nodes.item(tvwEPF.SelectedItem.key).Expanded = True
  
  SetMedIndex tvwEPF.SelectedItem.key
  route = getRoute(Index)
  EIndex = Abs(PathIndex(temp.idx1, temp.idx2, temp.idx3, route, lstExposure(Index).Text))
  
  getloc
  gettime
  getexp
  
  SetMedIndex tvwEPF.SelectedItem.key

  If temp.idx3 = 0 Then
    If EIndex = 0 Then
      temp.idx4 = med(temp.idx1).chem(temp.idx2).numexp + 1
      EIndex = temp.idx4
    Else
      temp.idx4 = EIndex
    End If
    temp.idx5 = med(temp.idx1).chem(temp.idx2).numseries
    temp.idx6 = med(temp.idx1).numloc
    SizeEpfSeries
    med(temp.idx1).chem(temp.idx2).exp(temp.idx4).route = route
    med(temp.idx1).chem(temp.idx2).exp(temp.idx4).Path = lstExposure(Index).Text
    Select Case lstExposure(Index).ItemData(lstExposure(Index).ListIndex)
      Case 0
        med(temp.idx1).chem(temp.idx2).exp(temp.idx4).cunit(1) = "Sv"
        med(temp.idx1).chem(temp.idx2).exp(temp.idx4).cunit(2) = "Sv"
      Case 1
        If con(med(temp.idx1).chem(temp.idx2).use).kind = 1 Then
          med(temp.idx1).chem(temp.idx2).exp(temp.idx4).cunit(1) = "Bq/kg"
          med(temp.idx1).chem(temp.idx2).exp(temp.idx4).cunit(2) = "Bq/kg"
        Else
          med(temp.idx1).chem(temp.idx2).exp(temp.idx4).cunit(1) = "mg/kg"
          med(temp.idx1).chem(temp.idx2).exp(temp.idx4).cunit(2) = "mg/kg"
        End If
      Case 2
        If con(med(temp.idx1).chem(temp.idx2).use).kind = 1 Then
          med(temp.idx1).chem(temp.idx2).exp(temp.idx4).cunit(1) = "Bq/m^3"
          med(temp.idx1).chem(temp.idx2).exp(temp.idx4).cunit(2) = "Bq/m^3"
        Else
          med(temp.idx1).chem(temp.idx2).exp(temp.idx4).cunit(1) = "mg/m^3"
          med(temp.idx1).chem(temp.idx2).exp(temp.idx4).cunit(2) = "mg/m^3"
        End If
      Case 3
        If con(med(temp.idx1).chem(temp.idx2).use).kind = 1 Then
          med(temp.idx1).chem(temp.idx2).exp(temp.idx4).cunit(1) = "Bq/L"
          med(temp.idx1).chem(temp.idx2).exp(temp.idx4).cunit(2) = "Bq/L"
        Else
          med(temp.idx1).chem(temp.idx2).exp(temp.idx4).cunit(1) = "mg/L"
          med(temp.idx1).chem(temp.idx2).exp(temp.idx4).cunit(2) = "mg/L"
        End If
    End Select
  Else
    If EIndex = 0 Then
      temp.idx4 = med(temp.idx1).chem(temp.idx2).prog(temp.idx3).numexp + 1
      EIndex = temp.idx4
    Else
      temp.idx4 = EIndex
    End If
    temp.idx5 = med(temp.idx1).chem(temp.idx2).numseries
    temp.idx6 = med(temp.idx1).numloc
    SizeEpfSeries
    med(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).route = route
    med(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).Path = lstExposure(Index).Text
    Select Case lstExposure(Index).ItemData(lstExposure(Index).ListIndex)
      Case 0
        med(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).cunit(1) = "Sv"
        med(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).cunit(2) = "Sv"
      Case 1
        If con(med(temp.idx1).chem(temp.idx2).use).kind = 1 Then
          med(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).cunit(1) = "Bq/kg"
          med(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).cunit(2) = "Bq/kg"
        Else
          med(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).cunit(1) = "mg/kg"
          med(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).cunit(2) = "mg/kg"
        End If
      Case 2
        If con(med(temp.idx1).chem(temp.idx2).use).kind = 1 Then
          med(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).cunit(1) = "Bq/m^3"
          med(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).cunit(2) = "Bq/m^3"
        Else
          med(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).cunit(1) = "mg/m^3"
          med(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).cunit(2) = "mg/m^3"
        End If
      Case 3
        If con(med(temp.idx1).chem(temp.idx2).use).kind = 1 Then
          med(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).cunit(1) = "Bq/L"
          med(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).cunit(2) = "Bq/L"
        Else
          med(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).cunit(1) = "mg/L"
          med(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).cunit(2) = "mg/L"
        End If
    End Select
  End If
  
  On Error GoTo notfound
  If lstExposure(Index).Selected(lstExposure(Index).ListIndex) Then
    Set mNode = tvwEPF.Nodes.Add(tvwEPF.SelectedItem.key, tvwChild, tvwEPF.SelectedItem.key + "," + CStr(EIndex) + "," + CStr(Index), route + " (" + lstExposure(Index).Text + ")", "closed", "closed")
    mNode.Tag = "Exposure"
    If temp.idx3 = 0 Then
      med(temp.idx1).chem(temp.idx2).exp(EIndex).use = 1
    Else
      med(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(EIndex).use = 1
    End If
  Else
    tvwEPF.Nodes.Remove tvwEPF.SelectedItem.key + "," + CStr(EIndex) + "," + CStr(Index)
    If temp.idx3 = 0 Then
      med(temp.idx1).chem(temp.idx2).exp(EIndex).use = 0
    Else
      med(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(EIndex).use = 0
    End If
  End If
notfound:
End Sub

Private Sub lstExposure_GotFocus(Index As Integer)
  HelpAnchor = lstExposure(Index).Tag
End Sub

Private Sub lstMedia_Click()
Dim beenbefore As Boolean
Dim Index As Long
Dim i As Long
Dim j As Long
Dim k As Long
Dim key1 As String

  If lstMedia.list(lstMedia.ListIndex) = "Wetlands" Then
    MsgBox "Wetlands is not a supported medium at this time.", vbOKOnly
    Exit Sub
  End If

  If lstMedia.ItemData(lstMedia.ListIndex) = 0 And lstMedia.Selected(lstMedia.ListIndex) Then
    temp.idx1 = numepf + 1
    SizeEpf
    Index = numepf
    lstMedia.ItemData(lstMedia.ListIndex) = Index
    beenbefore = False
  Else
    Index = lstMedia.ItemData(lstMedia.ListIndex)
    beenbefore = True
  End If
  
  On Error GoTo notfound
  If lstMedia.Selected(lstMedia.ListIndex) Then
    Set mNode = tvwEPF.Nodes.Add("EPF", tvwChild, "a," + CStr(Index), lstMedia.Text, "closed", "closed")
    mNode.Tag = "Media"
    mNode.Expanded = True
    mNode.EnsureVisible
    med(Index).use = 1
    med(Index).Name = lstMedia.Text
    If Not beenbefore Then
      ' load the list in the tree view
      For i = 1 To numcon
        temp.idx1 = Index
        temp.idx2 = i
        temp.idx3 = con(i).numprog
        SizeEpfChem
        med(Index).lunits(1) = "km"
        med(Index).lunits(2) = "km"
        med(Index).dur = 30
        med(Index).dunit(1) = "yr"
        med(Index).dunit(2) = "yr"
        med(Index).chem(i).use = i
        med(Index).chem(i).cas = con(i).cas
        med(Index).chem(i).Name = con(i).Name
        med(Index).chem(i).kind = con(i).kind
        med(Index).chem(i).tunit(1) = "yr"
        med(Index).chem(i).tunit(2) = "yr"
        key1 = "a," + CStr(temp.idx1) + "," + CStr(temp.idx2)
        Set mNode = tvwEPF.Nodes.Add("a," + CStr(Index), tvwChild, key1 + ",-1", con(i).Name, "closed", "closed")
        mNode.Tag = "Chemical"
        mNode.EnsureVisible
        Set mNode = tvwEPF.Nodes.Add(key1 + ",-1", tvwChild, key1 + ",0", "Constituent Pathways", "closed", "closed")
        mNode.Tag = "Constituent"
        mNode.EnsureVisible
        If prog.Checked Then
          For j = 1 To con(i).numprog
            med(Index).chem(i).prog(j).use = j
            med(Index).chem(i).prog(j).cas = con(i).prog(j).cas
            med(Index).chem(i).prog(j).Name = con(i).prog(j).Name
            med(Index).chem(i).prog(j).kind = con(i).prog(j).kind
            Set mNode = tvwEPF.Nodes.Add(key1 + ",-1", tvwChild, key1 + "," + CStr(j), con(i).prog(j).Name, "closed", "closed")
            mNode.Tag = "Progeny"
            mNode.EnsureVisible
          Next
        End If
      Next
    Else
      For i = 1 To med(Index).numchem
        If med(Index).chem(i).use > 0 Then
          key1 = "a," + CStr(Index) + "," + CStr(i)
          Set mNode = tvwEPF.Nodes.Add("a," + CStr(Index), tvwChild, key1 + ",-1", med(Index).chem(i).Name, "closed", "closed")
          mNode.Tag = "Chemical"
          mNode.EnsureVisible
          Set mNode = tvwEPF.Nodes.Add(key1 + ",-1", tvwChild, key1 + ",0", "Constituent Pathways", "closed", "closed")
          mNode.Tag = "Constituent"
          mNode.EnsureVisible
          For j = 1 To med(Index).chem(i).numexp
            If med(Index).chem(i).exp(j).use Then
              Set mNode = tvwEPF.Nodes.Add(key1 + ",0", tvwChild, key1 + ",0," + CStr(j) + "," + CStr(getRouteBox(med(Index).chem(i).exp(j).route)), med(Index).chem(i).exp(j).route + " (" + med(Index).chem(i).exp(j).Path + ")", "closed", "closed")
              mNode.Tag = "Exposure"
              mNode.EnsureVisible
            End If
          Next
          If prog.Checked Then
            For j = 1 To med(Index).chem(i).numprog
              If med(Index).chem(i).prog(j).use Then
                Set mNode = tvwEPF.Nodes.Add(key1 + ",-1", tvwChild, key1 + "," + CStr(j), med(Index).chem(i).prog(j).Name, "closed", "closed")
                mNode.EnsureVisible
                mNode.Tag = "Progeny"
                For k = 1 To med(Index).chem(i).prog(j).numexp
                  If med(Index).chem(i).prog(j).exp(k).use Then
                    Set mNode = tvwEPF.Nodes.Add(key1 + "," + CStr(j), tvwChild, key1 + "," + CStr(j) + "," + CStr(k) + "," + CStr(getRouteBox(med(Index).chem(i).prog(j).exp(k).route)), med(Index).chem(i).prog(j).exp(k).route + " (" + med(Index).chem(i).prog(j).exp(k).Path + ")", "closed", "closed")
                    mNode.Tag = "Exposure"
                    mNode.EnsureVisible
                  End If
                Next
              End If
            Next
          End If
        End If
      Next
    End If
  Else
    tvwEPF.Nodes.Remove "a," + CStr(Index)
    med(Index).use = 0
  End If
notfound:
End Sub

Private Sub lstMedia_GotFocus()
  HelpAnchor = lstMedia.Tag
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

Private Sub optDataSet_GotFocus(Index As Integer)
  HelpAnchor = optDataSet(Index).Tag
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

Private Sub SizeEpfSeries()
Dim i As Long
  
  SizeEpf
  SizeEpfExp
  If temp.idx5 >= med(temp.idx1).chem(temp.idx2).numseries And temp.idx5 > 0 Then
    med(temp.idx1).chem(temp.idx2).numseries = temp.idx5
    If temp.idx3 = 0 Then
      If med(temp.idx1).chem(temp.idx2).exp(temp.idx4).numseries < temp.idx5 Then
        ReDim Preserve med(temp.idx1).chem(temp.idx2).exp(temp.idx4).series(temp.idx5) As Entry
        med(temp.idx1).chem(temp.idx2).exp(temp.idx4).numseries = temp.idx5
      End If
      For i = 1 To temp.idx5
        If temp.idx6 > med(temp.idx1).chem(temp.idx2).exp(temp.idx4).series(i).num Then
          ReDim Preserve med(temp.idx1).chem(temp.idx2).exp(temp.idx4).series(i).valu(temp.idx6) As Double
          med(temp.idx1).chem(temp.idx2).exp(temp.idx4).series(i).num = temp.idx6
        End If
      Next
    Else
      If med(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).numseries < temp.idx5 Then
        ReDim Preserve med(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).series(temp.idx5) As Entry
        med(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).numseries = temp.idx5
      End If
      For i = 1 To temp.idx5
        If temp.idx6 > med(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).series(i).num Then
          ReDim Preserve med(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).series(i).valu(temp.idx6) As Double
          med(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).series(i).num = temp.idx6
        End If
      Next
    End If
  End If
End Sub

Private Sub SizeEpfExp()
  SizeEpf
  SizeEpfChem
  If temp.idx3 = 0 Then
    If temp.idx4 > med(temp.idx1).chem(temp.idx2).numexp Then
      ReDim Preserve med(temp.idx1).chem(temp.idx2).exp(temp.idx4) As Exposure
      med(temp.idx1).chem(temp.idx2).numexp = temp.idx4
    End If
  Else
    If temp.idx4 > med(temp.idx1).chem(temp.idx2).prog(temp.idx3).numexp Then
      ReDim Preserve med(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4) As Exposure
      med(temp.idx1).chem(temp.idx2).prog(temp.idx3).numexp = temp.idx4
    End If
  End If
End Sub

Private Sub SizeEpfChem()
  SizeEpf
  If temp.idx2 > med(temp.idx1).numchem Then
    ReDim Preserve med(temp.idx1).chem(temp.idx2) As sParent
    med(temp.idx1).numchem = temp.idx2
  End If
  If temp.idx3 > med(temp.idx1).chem(temp.idx2).numprog Then
    ReDim Preserve med(temp.idx1).chem(temp.idx2).prog(temp.idx3) As sProgeny
    med(temp.idx1).chem(temp.idx2).numprog = temp.idx3
  End If
End Sub

Private Sub SizeEpfTime()
  SizeEpfChem
  If temp.idx5 > med(temp.idx1).chem(temp.idx2).numtime Then
    ReDim Preserve med(temp.idx1).chem(temp.idx2).time(temp.idx5) As Double
    med(temp.idx1).chem(temp.idx2).numtime = temp.idx5
  End If
End Sub

Private Sub SizeEpfLoc()
  SizeEpf
  If temp.idx2 > med(temp.idx1).numloc Then
    ReDim Preserve med(temp.idx1).locx(temp.idx2) As Double
    ReDim Preserve med(temp.idx1).locy(temp.idx2) As Double
    med(temp.idx1).numloc = temp.idx2
  End If
End Sub

Private Sub SizeEpf()
  If temp.idx1 > numepf Then
    ReDim Preserve med(temp.idx1) As Medium
    numepf = temp.idx1
  End If
End Sub

Private Sub loadprm()
  Dim i As Long, j As Long, k As Long, l As Long, m As Long
  Dim sval As Boolean
  Dim fle As parmfile

  If open_parm(fle, FUIName, 2) Then
    Do Until EOCF(fle.file)
      If read_parmrec(fle, temp) Then
        Select Case temp.pName
          Case "fui"
            For i = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                If temp.idx1 = siteIdx Then
                  Select Case temp.pName
                    Case "ecodespath"
                      If Left(modName, 3) = "eco" And temp.idx2 = modIdx Then DesName = temp.pval
                    Case "usrdespath"
                      If Left(modName, 3) = "usr" And temp.idx2 = modIdx Then DesName = temp.pval
                    Case "fscname"
                      SizeParentProgenyList
                      If temp.idx3 > 0 Then
                        con(temp.idx2).prog(temp.idx3).Name = temp.pval
                      Else
                        con(temp.idx2).Name = temp.pval
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
                Select Case temp.pName
                  Case "CVTFormat":    CVTFormat = "General Number" 'CVTFormat = temp.pval
                  Case "progeny":
                    sval = False
                    If temp.pval = "True" Then
                      'sval = True
                      'no more prooduct output
                      MsgBox "This case contained progeny concentrations/intakes which are no loneger supported!" & vbCrLf & _
                             "Those inventories have been removed and must be rentered as parents.", vbInformation
                    End If
                    prog.Checked = sval
                  Case "locx"
                    SizeEpfLoc
                    med(temp.idx1).locx(temp.idx2) = convert(temp.cunit, temp.uunit, Val(temp.pval))
                    med(temp.idx1).lunits(1) = temp.uunit
                    med(temp.idx1).lunits(2) = temp.cunit
                  Case "locy"
                    SizeEpfLoc
                    med(temp.idx1).locy(temp.idx2) = convert(temp.cunit, temp.uunit, Val(temp.pval))
                    med(temp.idx1).lunits(1) = temp.uunit
                    med(temp.idx1).lunits(2) = temp.cunit
                  Case "name"
                    SizeEpf
                    med(temp.idx1).use = 1
                    med(temp.idx1).Name = temp.pval
                    med(temp.idx1).ref = temp.ref
                    med(temp.idx1).lunits(1) = "km"
                    med(temp.idx1).lunits(2) = "km"
                  Case "kind"
                    SizeEpf
                    med(temp.idx1).kind = Val(temp.pval)
                  Case "casid":
                    SizeEpfChem
                    If temp.idx3 = 0 Then
                      med(temp.idx1).chem(temp.idx2).cas = temp.pval
                      med(temp.idx1).chem(temp.idx2).ref = temp.ref
                    Else
                      med(temp.idx1).chem(temp.idx2).prog(temp.idx3).cas = temp.pval
                      med(temp.idx1).chem(temp.idx2).prog(temp.idx3).ref = temp.ref
                    End If
                    med(temp.idx1).dunit(1) = "yr"
                    med(temp.idx1).dunit(2) = "yr"
                    med(temp.idx1).chem(temp.idx2).tunit(1) = "yr"
                    med(temp.idx1).chem(temp.idx2).tunit(2) = "yr"
                  Case "route":
                    SizeEpfExp
                    If temp.idx3 = 0 Then
                      med(temp.idx1).chem(temp.idx2).exp(temp.idx4).use = 1
                      med(temp.idx1).chem(temp.idx2).exp(temp.idx4).route = temp.pval
                      med(temp.idx1).chem(temp.idx2).exp(temp.idx4).ref = temp.ref
                    Else
                      med(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).use = 1
                      med(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).route = temp.pval
                      med(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).ref = temp.ref
                    End If
                  Case "path":
                    SizeEpfExp
                    If temp.idx3 = 0 Then
                      med(temp.idx1).chem(temp.idx2).exp(temp.idx4).Path = temp.pval
                      med(temp.idx1).chem(temp.idx2).exp(temp.idx4).ref = temp.ref
                    Else
                      med(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).Path = temp.pval
                      med(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).ref = temp.ref
                    End If
                  Case "time":
                    SizeEpfTime
                    med(temp.idx1).chem(temp.idx2).time(temp.idx5) = Val(convert(temp.cunit, temp.uunit, Val(temp.pval)))
                    med(temp.idx1).chem(temp.idx2).tunit(1) = temp.uunit
                    med(temp.idx1).chem(temp.idx2).tunit(2) = temp.cunit
                  Case "dur":
                    SizeEpfLoc
                    med(temp.idx1).dur = Val(convert(temp.cunit, temp.uunit, Val(temp.pval)))
                    med(temp.idx1).dunit(1) = temp.uunit
                    med(temp.idx1).dunit(2) = temp.cunit
                  Case "conc":
                    SizeEpfSeries
                    If temp.idx3 = 0 Then
                      med(temp.idx1).chem(temp.idx2).exp(temp.idx4).series(temp.idx5).valu(temp.idx6) = Val(convert(temp.cunit, temp.uunit, Val(temp.pval)))
                      med(temp.idx1).chem(temp.idx2).exp(temp.idx4).cunit(1) = temp.uunit
                      med(temp.idx1).chem(temp.idx2).exp(temp.idx4).cunit(2) = temp.cunit
                    Else
                      med(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).series(temp.idx5).valu(temp.idx6) = Val(convert(temp.cunit, temp.uunit, Val(temp.pval)))
                      med(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).cunit(1) = temp.uunit
                      med(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).cunit(2) = temp.cunit
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
    SetFormat Me
  
''epf resolve contaminate differences if contaminate no longer exists its index is 0
    For i = 1 To numepf
      For j = 1 To numcon
        For k = 1 To med(i).numchem
          If con(j).cas = med(i).chem(k).cas Then
            med(i).chem(k).use = j
            med(i).chem(k).Name = con(j).Name
            med(i).chem(k).kind = con(j).kind
            If med(i).chem(k).numseries < med(i).chem(k).numtime Then
              med(i).chem(k).numseries = med(i).chem(k).numtime
            End If
            For l = 1 To con(j).numprog
              For m = 1 To med(i).chem(k).numprog
                If con(j).prog(l).cas = med(i).chem(k).prog(m).cas Then
                  med(i).chem(k).prog(m).use = l
                  med(i).chem(k).prog(m).Name = con(j).prog(l).Name
                  med(i).chem(k).prog(m).kind = con(j).prog(l).kind
                  Exit For
                End If
              Next
              If m > med(i).chem(k).numprog Then
                ReDim Preserve med(i).chem(k).prog(m) As sProgeny
                med(i).chem(k).numprog = m
                med(i).chem(k).prog(m).use = l
                med(i).chem(k).prog(m).cas = con(j).prog(l).cas
                med(i).chem(k).prog(m).Name = con(j).prog(l).Name
                med(i).chem(k).prog(m).kind = con(j).prog(l).kind
              End If
            Next
            Exit For
          End If
        Next
        If k > med(i).numchem Then
          ReDim Preserve med(i).chem(k) As sParent
          med(i).numchem = k
          med(i).chem(k).use = j
          med(i).chem(k).cas = con(j).cas
          med(i).chem(k).Name = con(j).Name
          med(i).chem(k).kind = con(j).kind
          med(i).chem(k).numprog = con(j).numprog
          med(i).chem(k).tunit(1) = "yr"
          med(i).chem(k).tunit(2) = "yr"
          If con(j).numprog > 0 Then
            ReDim Preserve med(i).chem(k).prog(con(j).numprog) As sProgeny
            For l = 1 To con(j).numprog
              med(i).chem(k).prog(l).use = l
              med(i).chem(k).prog(l).cas = con(j).prog(l).cas
              med(i).chem(k).prog(l).Name = con(j).prog(l).Name
              med(i).chem(k).prog(l).kind = con(j).prog(l).kind
            Next
          End If
        End If
      Next
    Next
    
    For i = 1 To numepf
      For j = 0 To lstMedia.ListCount - 1
        If med(i).Name = lstMedia.list(j) Then
          lstMedia.ItemData(j) = i
          lstMedia.Selected(j) = True
        End If
      Next
    Next
  
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
      
  With tvwEPF
    Set mNode = .Nodes.Add()
    .LabelEdit = False
    .LineStyle = tvwRootLines
  End With
  
  With mNode ' Add first node.
    .Text = "EPF"
    .Tag = "EPF"
    .key = "EPF"
    .Image = "closed"
    .Expanded = True
  End With
    
  StartModule frmEPF, "FRAMES Known Exposure Pathway Module", 6
  mode = Val(argv(0))
  kind = "Exposure Pathway"
  thismod = "\EXP"
  SetHelpFile App.Path + "\Known EPF.html"
  SetRefFile ReplaceExt(FUIName, "ref")
  Loading.Show
  loadng = True
  loadprm
  
  paths(0) = "Fruit"
  paths(1) = "Grain"
  paths(2) = "Root vegetables"
  paths(3) = "Leafy vegetables"
  paths(4) = "Other vegetables"
  paths(5) = "Aquatic plants"
  paths(6) = "Beef"
  paths(7) = "Meat"
  paths(8) = "Milk"
  paths(9) = "Poultry"
  paths(10) = "Eggs"
  paths(11) = "Fish"
  paths(12) = "Crustacea"
  paths(13) = "Mollusks"
  paths(14) = "Shower"
  paths(15) = "Swimming"
  paths(16) = "Shoreline"
  paths(17) = "Boating"
  paths(18) = "Water"
  paths(19) = "Soil"
  paths(20) = "Air"
  paths(21) = "Indoor air"
  paths(22) = "Ground"
  
'soil,external
  ExpPathOK(3, 4).Add 19
  ExpPathOK(3, 4).Add 22    'ground same as soil
'soil,dermal
  ExpPathOK(3, 3).Add 19
'soil,inhalation
  ExpPathOK(3, 2).Add 19
'soil,ingestion
  ExpPathOK(3, 1).Add 0
  ExpPathOK(3, 1).Add 1
  ExpPathOK(3, 1).Add 2
  ExpPathOK(3, 1).Add 3
  ExpPathOK(3, 1).Add 4
  ExpPathOK(3, 1).Add 6
  ExpPathOK(3, 1).Add 7
  ExpPathOK(3, 1).Add 8
  ExpPathOK(3, 1).Add 9
  ExpPathOK(3, 1).Add 10
  ExpPathOK(3, 1).Add 19

'surface water,external
  ExpPathOK(2, 4).Add 15
  ExpPathOK(2, 4).Add 16
  ExpPathOK(2, 4).Add 17
'surface water,dermal
  ExpPathOK(2, 3).Add 14
  ExpPathOK(2, 3).Add 15
  ExpPathOK(2, 3).Add 16
'surface water,inhalation
  ExpPathOK(2, 2).Add 14
  ExpPathOK(2, 2).Add 21
'surface water,ingestion
  ExpPathOK(2, 1).Add 0
  ExpPathOK(2, 1).Add 1
  ExpPathOK(2, 1).Add 2
  ExpPathOK(2, 1).Add 3
  ExpPathOK(2, 1).Add 4
  ExpPathOK(2, 1).Add 5
  ExpPathOK(2, 1).Add 6
  ExpPathOK(2, 1).Add 7
  ExpPathOK(2, 1).Add 8
  ExpPathOK(2, 1).Add 9
  ExpPathOK(2, 1).Add 10
  ExpPathOK(2, 1).Add 11
  ExpPathOK(2, 1).Add 12
  ExpPathOK(2, 1).Add 13
  ExpPathOK(2, 1).Add 14
  ExpPathOK(2, 1).Add 15
  ExpPathOK(2, 1).Add 16
  ExpPathOK(2, 1).Add 18
  ExpPathOK(2, 1).Add 19

'aquifer,external
  'none
'aquifer,dermal
  ExpPathOK(1, 3).Add 14
'aquifer,inhalation
  ExpPathOK(1, 2).Add 14
  ExpPathOK(1, 2).Add 21
'aquifer,ingestion
  ExpPathOK(1, 1).Add 18
  ExpPathOK(1, 1).Add 0
  ExpPathOK(1, 1).Add 1
  ExpPathOK(1, 1).Add 2
  ExpPathOK(1, 1).Add 3
  ExpPathOK(1, 1).Add 4
  ExpPathOK(1, 1).Add 6
  ExpPathOK(1, 1).Add 7
  ExpPathOK(1, 1).Add 8
  ExpPathOK(1, 1).Add 9
  ExpPathOK(1, 1).Add 10
  ExpPathOK(1, 1).Add 14

'air,external
  ExpPathOK(0, 4).Add 19
  ExpPathOK(0, 4).Add 22    'ground same as soil
  ExpPathOK(0, 4).Add 20
'air,dermal
  ExpPathOK(0, 3).Add 19
'air,inhalation
  ExpPathOK(0, 2).Add 20
  ExpPathOK(0, 2).Add 21
'air,ingestion
  ExpPathOK(0, 1).Add 0
  ExpPathOK(0, 1).Add 1
  ExpPathOK(0, 1).Add 2
  ExpPathOK(0, 1).Add 3
  ExpPathOK(0, 1).Add 4
  ExpPathOK(0, 1).Add 5
  ExpPathOK(0, 1).Add 6
  ExpPathOK(0, 1).Add 7
  ExpPathOK(0, 1).Add 8
  ExpPathOK(0, 1).Add 9
  ExpPathOK(0, 1).Add 10
  
  For i = 0 To 4
    get_conversion_items unit(i).Tag, unit(i)
  Next
  
  If mode < 2 Then
    mediakind = 0
  Else
    mediakind = 1   'Acute
  End If
  
  If mode Mod 2 = 0 Then
    Unload Loading
    Make_EPF
    EndModule
  End If
  
  Unload Loading
  
  RefItem = 1
  loadng = False
  tvwEPF.SelectedItem = tvwEPF.Nodes(1)
  tvwEPF_NodeClick tvwEPF.SelectedItem
End Sub

Private Sub FillExposureList(Media As Long)
Dim i As Integer
Dim j As Integer
Dim idx As Integer
  
  For i = 1 To 4
    lstExposure(i).Clear
    For j = 1 To ExpPathOK(Media, i).Count
      idx = ExpPathOK(Media, i).item(j)
      lstExposure(i).AddItem paths(idx)
      Select Case i
      Case 4
        If (med(temp.idx1).Name = "Soil") Then
           lstExposure(i).ItemData(lstExposure(i).NewIndex) = 0
        Else
          Select Case idx
            Case 15, 16, 17, 19, 22
              lstExposure(i).ItemData(lstExposure(i).NewIndex) = 1
            Case 20
              lstExposure(i).ItemData(lstExposure(i).NewIndex) = 3
          End Select
        End If
      Case 3
        Select Case idx
          Case 0 To 7, 9 To 13, 16, 17, 19
            lstExposure(i).ItemData(lstExposure(i).NewIndex) = 1
          Case 8, 14, 15, 18, 20, 21
            lstExposure(i).ItemData(lstExposure(i).NewIndex) = 3
        End Select
      Case 2
        lstExposure(i).ItemData(lstExposure(i).NewIndex) = 2
      Case 1
        Select Case idx
          Case 0 To 7, 9 To 13, 16, 17, 19, 22
            lstExposure(i).ItemData(lstExposure(i).NewIndex) = 1
          Case 8, 14, 15, 18, 20, 21
            lstExposure(i).ItemData(lstExposure(i).NewIndex) = 3
        End Select
      End Select
    Next
  Next
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
  Dim p As Long, q As Long, r As Long
  Dim medcount As Long
  Dim parentexpcount As Long
  Dim progexpcount As Long
  
  Dim fName As String
  Dim parm As parmrec
  Dim fle As parmfile
  Dim UserFormat As String
  UserFormat = CVTFormat
  
  getloc
  gettime
  getexp
  prevtime = -1
  fName = RunName & ".GID"
  If open_parm(fle, fName, 1) Then
    set_parm parm, "CVTFormat", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CVTFormat
    write_parmrec fle, parm
    CVTFormat = "General Number"
    For i = 1 To numepf
      If med(i).use Then
        medcount = medcount + 1
        set_parm parm, "name", medcount, 0, 0, 0, 0, 0, med(i).ref, "N/A", "N/A", med(i).Name
        write_parmrec fle, parm
        set_parm parm, "kind", medcount, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CStr(mediakind)
        write_parmrec fle, parm
        set_parm parm, "dur", medcount, 0, 0, 0, 0, 0, 0, med(i).dunit(1), med(i).dunit(2), convert(med(i).dunit(1), med(i).dunit(2), med(i).dur)
        write_parmrec fle, parm
        
        For j = 1 To med(i).numloc
          set_parm parm, "locx", medcount, j, 0, 0, 0, 0, 0, med(i).lunits(1), med(i).lunits(2), convert(med(i).lunits(1), med(i).lunits(2), med(i).locx(j))
          write_parmrec fle, parm
          set_parm parm, "locy", medcount, j, 0, 0, 0, 0, 0, med(i).lunits(1), med(i).lunits(2), convert(med(i).lunits(1), med(i).lunits(2), med(i).locy(j))
          write_parmrec fle, parm
        Next
       
        For j = 1 To numcon
          For k = 1 To med(i).numchem
            If med(i).chem(k).cas = con(j).cas Then
              set_parm parm, "casid", medcount, j, 0, 0, 0, 0, CInt(med(i).chem(k).ref), "N/A", "N/A", med(i).chem(k).cas
              write_sparmrec fle, parm
              prevtime = -1
              bnflag = False
              Check_NTS med(i).chem(k).numseries, con(j).Name & " number of time series is less than two"
              For m = 1 To med(i).chem(k).numseries
                Is_Ascending med(i).chem(k).time(m), con(j).Name & " time out of sequence"
                set_parm parm, "time", medcount, j, 0, 0, m, 0, 0, med(i).chem(k).tunit(1), med(i).chem(k).tunit(2), convert(med(i).chem(k).tunit(1), med(i).chem(k).tunit(2), med(i).chem(k).time(m))
                write_parmrec fle, parm
              Next
              For m = 1 To med(i).chem(k).numseries
                If med(i).numloc > 0 Then
                  parentexpcount = 0
                  For l = 1 To med(i).chem(k).numexp
                    If med(i).chem(k).exp(l).use Then
                      parentexpcount = parentexpcount + 1
                      If m = 1 Then
                        set_parm parm, "route", medcount, j, 0, parentexpcount, 0, 0, med(i).chem(k).exp(l).ref, "N/A", "N/A", med(i).chem(k).exp(l).route
                        write_parmrec fle, parm
                        set_parm parm, "path", medcount, j, 0, parentexpcount, 0, 0, med(i).chem(k).exp(l).ref, "N/A", "N/A", med(i).chem(k).exp(l).Path
                        write_parmrec fle, parm
                      End If
                      For p = 1 To med(i).numloc
                        set_parm parm, "conc", medcount, j, 0, parentexpcount, m, p, 0, med(i).chem(k).exp(l).cunit(1), med(i).chem(k).exp(l).cunit(2), convert(med(i).chem(k).exp(l).cunit(1), med(i).chem(k).exp(l).cunit(2), med(i).chem(k).exp(l).series(m).valu(p))
                        write_parmrec fle, parm
                      Next
                    End If
                  Next
                End If
                If prog.Checked Then
                  For l = 1 To con(j).numprog
                    For q = 1 To med(i).chem(k).numprog
                      If med(i).chem(k).prog(q).cas = con(j).prog(l).cas Then
                        If m = 1 Then
                          set_parm parm, "casid", medcount, j, l, 0, 0, 0, med(i).chem(k).prog(q).ref, "N/A", "N/A", con(j).prog(l).cas
                          write_sparmrec fle, parm
                        End If
                        If med(i).numloc > 0 Then
                          progexpcount = 0
                          For r = 1 To med(i).chem(k).prog(q).numexp
                            If med(i).chem(k).prog(q).exp(l).use Then
                              progexpcount = progexpcount + 1
                              If m = 1 Then
                                set_parm parm, "route", medcount, j, l, progexpcount, 0, 0, med(i).chem(k).prog(q).exp(l).ref, "N/A", "N/A", med(i).chem(k).prog(q).exp(r).route
                                write_parmrec fle, parm
                                set_parm parm, "path", medcount, j, l, progexpcount, 0, 0, med(i).chem(k).prog(q).exp(l).ref, "N/A", "N/A", med(i).chem(k).prog(q).exp(r).Path
                                write_parmrec fle, parm
                              End If
                              For p = 1 To med(i).numloc
                                set_parm parm, "conc", medcount, j, l, progexpcount, m, p, 0, med(i).chem(k).prog(q).exp(r).cunit(1), med(i).chem(k).prog(q).exp(r).cunit(2), convert(med(i).chem(k).prog(q).exp(r).cunit(1), med(i).chem(k).prog(q).exp(r).cunit(2), med(i).chem(k).prog(q).exp(r).series(m).valu(p))
                                write_parmrec fle, parm
                              Next
                            End If
                          Next
                        End If
                      End If
                    Next
                  Next
                End If
              Next
            End If
          Next
        Next
      End If
    Next
    close_parm fle
    CVTFormat = UserFormat
    SetFormat Me
  Else
    PutError "Unable to create transaction file" & RunName & ".GID"
  End If
  EndModule
End Sub

Private Function GetNumExp&(i&, j&, k&)
Dim m As Long
  
  GetNumExp = 0
  If k = 0 Then
    If med(i).chem(j).numexp = 0 Then Exit Function
    For m = 1 To med(i).chem(j).numexp
      If med(i).chem(j).exp(m).use = 1 Then
        GetNumExp = GetNumExp + 1
      End If
    Next
  Else
    If med(i).chem(j).prog(k).numexp = 0 Then Exit Function
    For m = 1 To med(i).chem(j).prog(k).numexp
      If med(i).chem(j).prog(k).exp(m).use = 1 Then
        GetNumExp = GetNumExp + 1
      End If
    Next
  End If
End Function

Private Function GetNumCon&(i&)
Dim j As Long
  
  GetNumCon = 0
  For j = 1 To med(i).numchem
    If med(i).chem(j).use > 0 Then
      If med(i).chem(j).numexp > 0 Then
        GetNumCon = GetNumCon + 1
      End If
    End If
  Next
End Function

Private Function Make_EPF()
  Dim i As Long, j As Long, k As Long
  Dim l As Long, m As Long, n As Long
  Dim q As Long, r As Long, p As Long
  Dim msg As String
  Dim cnt As Long
  Dim file As csv
  
  If open_csv(file, RunName & ".epf", 1) Then
    PutHeader file
    cnt = 0
    For i = 1 To numepf
      If med(i).use Then cnt = cnt + 1
    Next
    put_val file, cnt
    put_line file
    For i = 1 To numepf
     With med(i)
      If .use Then
        If .kind = 1 Then
          put_val file, "acute"
        Else
          put_val file, "chronic"
        End If
        put_val file, modName
        put_val file, .Name
        put_val file, .numloc
        put_val file, numcon                        ' GetNumCon(i)
        put_line file
        For j = 1 To .numloc
          put_val file, convert(.lunits(1), .lunits(2), .locx(j))
          put_val file, .lunits(2)
          put_val file, convert(.lunits(1), .lunits(2), .locy(j))
          put_val file, .lunits(2)
          put_line file
        Next
        
        For j = 1 To numcon
          For k = 1 To .numchem
            If .chem(k).cas = con(j).cas Then        ' And .chem(k).use > 0
              put_val file, .chem(k).Name
              put_sval file, .chem(k).cas
              If prog.Checked Then
                put_val file, .chem(k).numprog
              Else
                put_val file, 0
              End If
              put_val file, .chem(k).numseries
              put_line file
              For l = 1 To .chem(k).numseries
                put_val file, convert(.chem(k).tunit(1), .chem(k).tunit(2), .chem(k).time(l))
                put_val file, .chem(k).tunit(2)
                put_val file, convert(.dunit(1), .dunit(2), .dur)
                put_val file, .dunit(2)
                put_val file, GetNumExp(i, k, 0)
                put_line file
                For m = 1 To .chem(k).numexp
                  If .chem(k).exp(m).use > 0 Then
                    put_val file, .chem(k).exp(m).Path
                    put_val file, .chem(k).exp(m).route
                    put_val file, .chem(k).exp(m).cunit(2)
                    put_line file
                    For n = 1 To .numloc
                      put_val file, convert(.chem(k).exp(m).cunit(1), .chem(k).exp(m).cunit(2), .chem(k).exp(m).series(l).valu(n))
                    Next
                    put_line file
                  End If
                Next
                If prog.Checked Then
                  For q = 1 To con(j).numprog
                    For m = 1 To .chem(k).numprog
                      If .chem(k).prog(m).cas = con(j).prog(q).cas Then      ' And .chem(k).prog(m).use > 0
                        put_val file, .chem(k).prog(m).Name
                        put_sval file, .chem(k).prog(m).cas
                        put_val file, GetNumExp(i, k, m)
                        put_line file
                        For n = 1 To .chem(k).prog(m).numexp
                          If .chem(k).prog(m).exp(n).use > 0 Then
                            put_val file, .chem(k).prog(m).exp(n).Path
                            put_val file, .chem(k).prog(m).exp(n).route
                            put_val file, .chem(k).prog(m).exp(n).cunit(2)
                            put_line file
                            For p = 1 To .numloc
                              put_val file, convert(.chem(k).prog(m).exp(n).cunit(1), .chem(k).prog(m).exp(n).cunit(2), .chem(k).prog(m).exp(n).series(l).valu(p))
                            Next
                            put_line file
                          End If
                        Next
                      End If
                    Next
                  Next
                End If
              Next
            End If
          Next
        Next
      End If
     End With
    Next
    close_csv file
  Else
    PutError "Unable to create water valurec file" & RunName & ".WCF"
  End If
End Function
Private Sub getloc()
Dim j As Long
Dim k As Long
Dim l As Long
Dim m As Long
  
  ' this gets the stuff off the screen and puts it into the structure
  
  If key(1) = "key" Then Exit Sub
  SetMedIndex key(1)
  key(1) = "key"
  temp.idx2 = vaSpread1.DataRowCnt
  With med(temp.idx1)
    .dur = Val(txt(3).Text)
    .dunit(1) = unit(3).Text
    
    ReDim Preserve .locx(temp.idx2) As Double
    ReDim Preserve .locy(temp.idx2) As Double
    .numloc = temp.idx2
    .ref = ref(1).Tag
    For j = 1 To .numchem
     For l = 1 To .chem(j).numseries
      For k = 1 To .chem(j).numexp
        ReDim Preserve .chem(j).exp(k).series(l).valu(temp.idx2) As Double
      Next
      For k = 1 To .chem(j).numprog
        For m = 1 To .chem(j).prog(k).numexp
          ReDim Preserve .chem(j).prog(k).exp(m).series(l).valu(temp.idx2) As Double
        Next
      Next
     Next
    Next
    If temp.idx2 = 0 Then Exit Sub
  
    .kind = IIf(optDataSet(1).Value = True, 1, 0)
    .lunits(1) = unit(4).Text
    For k = 1 To .numloc
      vaSpread1.col = 1
      vaSpread1.Row = k
      .locx(k) = Val(vaSpread1.Value)
      vaSpread1.Text = ""
      vaSpread1.col = 2
      .locy(k) = Val(vaSpread1.Value)
      vaSpread1.Text = ""
    Next
  End With
  
End Sub

Private Sub putloc(key1 As String)
Dim k As Long
  ' this gets the stuff out of the structure and puts it on to the screen

  getloc
  SetMedIndex key1
  key(1).Caption = key1
  With med(temp.idx1)
    txt(3).Text = .dur
    set_unit unit(3), .dunit(1)
    ref(1).Caption = "Ref: " + CStr(.ref)
    ref(1).Tag = .ref
    optDataSet(0).Value = IIf(.kind = 0, True, False)
    optDataSet(1).Value = IIf(.kind = 1, True, False)
    set_unit unit(4), .lunits(1)
    For k = 1 To .numloc
      vaSpread1.col = 1
      vaSpread1.Row = k
      vaSpread1.Text = .locx(k)
      vaSpread1.col = 2
      vaSpread1.Text = .locy(k)
    Next
 End With
End Sub

Private Sub gettime()
Dim j As Long
Dim k As Long
Dim m As Long
Dim n As Long
  
  ' this gets the stuff off the screen and puts it into the structure
  
  If key(2) = "key" Then Exit Sub
  If vaSpread2.DataRowCnt = 0 Then Exit Sub
  SetMedIndex key(2)
  key(2) = "key"
  With med(temp.idx1)
    .chem(temp.idx2).numseries = vaSpread2.DataRowCnt
    If .chem(temp.idx2).numseries = 0 Then Exit Sub
    
    ReDim Preserve .chem(temp.idx2).time(.chem(temp.idx2).numseries) As Double
    For k = 1 To .chem(temp.idx2).numexp
      ReDim Preserve .chem(temp.idx2).exp(k).series(.chem(temp.idx2).numseries) As Entry
      For m = 1 To .chem(temp.idx2).numseries
        ReDim Preserve .chem(temp.idx2).exp(k).series(m).valu(.numloc) As Double
      Next
    Next
    For k = 1 To .chem(temp.idx2).numprog
      For m = 1 To .chem(temp.idx2).prog(k).numexp
        ReDim Preserve .chem(temp.idx2).prog(k).exp(m).series(.chem(temp.idx2).numseries) As Entry
        For n = 1 To .chem(temp.idx2).numseries
          ReDim Preserve .chem(temp.idx2).prog(k).exp(m).series(n).valu(.numloc) As Double
        Next
      Next
    Next
  
    .chem(temp.idx2).ref = ref(2).Tag
    .chem(temp.idx2).tunit(1) = unit(2).Text
    vaSpread2.col = 1
    For k = 1 To .chem(temp.idx2).numseries
      vaSpread2.Row = k
      .chem(temp.idx2).time(k) = Val(vaSpread2.Value)
      vaSpread2.Text = ""
    Next
  End With
End Sub
Private Sub puttime(key1 As String)
Dim j As Long
  ' this get the stuff out of the structure and puts it on to the screen

  getloc
  gettime
  SetMedIndex key1
  key(2).Caption = key1
  With med(temp.idx1)
    ref(2).Caption = "Ref: " + CStr(.chem(temp.idx2).ref)
    ref(2).Tag = .chem(temp.idx2).ref
    set_unit unit(2), .chem(temp.idx2).tunit(1)
    vaSpread2.col = 1
    For j = 1 To .chem(temp.idx2).numseries
      vaSpread2.Row = j
      vaSpread2.Text = .chem(temp.idx2).time(j)
    Next
  End With
End Sub

Private Sub getexp()
Dim j As Long
Dim k As Long
  ' this get the stuff off the screen and puts it into the structure
  
  If key(3) = "key" Then Exit Sub
  SetMedIndex key(3)
  key(3) = "key"
  With med(temp.idx1)
    If temp.idx3 = 0 Then
      .chem(temp.idx2).exp(temp.idx4).cunit(1) = unit(1).Text
      .chem(temp.idx2).exp(temp.idx4).ref = ref(3).Tag
    Else
      .chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).cunit(1) = unit(1).Text
      .chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).ref = ref(3).Tag
    End If
    For j = 1 To .chem(temp.idx2).numseries
      vaSpread3.Row = j
      For k = 1 To .numloc
        vaSpread3.col = k + 1
        If temp.idx3 = 0 Then
          .chem(temp.idx2).exp(temp.idx4).series(j).valu(k) = Val(vaSpread3.Value)
          vaSpread3.Text = ""
        Else
          .chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).series(j).valu(k) = Val(vaSpread3.Value)
          vaSpread3.Text = ""
        End If
      Next
    Next
  End With
End Sub

Private Sub putexp(key1 As String)
Dim j As Long
Dim k As Long
  ' this get the stuff out of the structure and puts it on to the screen

  getloc
  gettime
  getexp
  
  SetMedIndex key1
  key(3).Caption = key1
  With med(temp.idx1)
    set_unit unit(0), .chem(temp.idx2).tunit(1)
    unit(1).Clear
    If temp.idx3 = 0 Then
      get_conversion_items .chem(temp.idx2).exp(temp.idx4).cunit(1), unit(1)
      ref(3).Caption = "Ref: " + CStr(.chem(temp.idx2).exp(temp.idx4).ref)
      ref(3).Tag = .chem(temp.idx2).exp(temp.idx4).ref
    Else
      get_conversion_items .chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).cunit(1), unit(1)
      ref(3).Caption = "Ref: " + CStr(.chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).ref)
      ref(3).Tag = .chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).ref
    End If
    
    vaSpread3.MaxCols = .numloc + 1
    vaSpread3.MaxRows = .chem(temp.idx2).numseries
    vaSpread3.Row = 1
    vaSpread3.col = 1
    vaSpread3.Row2 = .chem(temp.idx2).numseries
    vaSpread3.Col2 = 1
    vaSpread3.Lock = False
    
    vaSpread3.Row = 0
    For k = 1 To .numloc
      vaSpread3.col = k + 1
      vaSpread3.Text = "Conc. at Location " & CStr(k)
    Next
    
    For j = 1 To .chem(temp.idx2).numseries
      vaSpread3.Row = j
      vaSpread3.col = 1
      vaSpread3.Text = .chem(temp.idx2).time(j)
      For k = 1 To .numloc
        vaSpread3.col = k + 1
        If temp.idx3 = 0 Then
          vaSpread3.Text = Format(.chem(temp.idx2).exp(temp.idx4).series(j).valu(k), CVTFormat)
        Else
          vaSpread3.Text = Format(.chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).series(j).valu(k), CVTFormat)
        End If
      Next
    Next
    
    vaSpread3.Row = 1
    vaSpread3.col = 1
    vaSpread3.Row2 = .chem(temp.idx2).numseries
    vaSpread3.Col2 = 1
    vaSpread3.Lock = True
    If .numloc > 1 Then
      vaSpread3.VisibleCols = 3
    Else
      vaSpread3.VisibleCols = 2
    End If
     
  End With
End Sub
Private Sub tvwEPF_BeforeLabelEdit(Cancel As Integer)
  Cancel = True ' Cancel the operation
End Sub
Private Sub tvwEPF_Collapse(ByVal Node As ComctlLib.Node)
  Node.Image = "closed"
End Sub

Private Sub tvwEPF_Expand(ByVal Node As ComctlLib.Node)
  Node.Image = "open"
End Sub

Private Sub FramesClear()
  fraEPF.Visible = False
  fraMedia.Visible = False
  fraChemical.Visible = False
  fraConcentration.Visible = False
  fraExposure.Visible = False
End Sub

Private Sub tvwEPF_NodeClick(ByVal Node As ComctlLib.Node)
Dim i As Long
Dim j As Long
Dim route As String

  FramesClear
  noact.Enabled = True
  
  Select Case Node.Tag
    Case "EPF"
      noact.Enabled = False
      RefItem = -1
      HelpAnchor = "medium"
      fraEPF.Caption = tvwEPF.SelectedItem.Text
      fraEPF.Visible = True

    Case "Media"
      RefItem = 1
      HelpAnchor = "acute"
      putloc tvwEPF.SelectedItem.key
      fraMedia.Caption = tvwEPF.SelectedItem.Text
      fraMedia.Visible = True

    Case "Chemical"
      RefItem = 2
      HelpAnchor = "start"
      puttime tvwEPF.SelectedItem.key
      fraChemical.Caption = tvwEPF.SelectedItem.Text
      fraChemical.Visible = True

    Case "Constituent", "Progeny"
      SetMedIndex tvwEPF.SelectedItem.key
      FillExposureList getMedia
      noact.Enabled = False
      RefItem = -1
      HelpAnchor = "parent"
      deselect = True
      SetMedIndex tvwEPF.SelectedItem.key
      For j = 1 To 4
        route = getRoute(j)
        If route = "External" Then lstExposure(j).Enabled = (med(temp.idx1).chem(temp.idx2).kind = 1)
        For i = 0 To lstExposure(j).ListCount - 1
          If PathIndex(temp.idx1, temp.idx2, temp.idx3, route, lstExposure(j).list(i)) > 0 Then
            lstExposure(j).Selected(i) = True
          End If
        Next
      Next
      deselect = False
      fraExposure.Caption = tvwEPF.SelectedItem.Text
      fraExposure.Visible = True

    Case "Exposure"
      HelpAnchor = "timecon"
      RefItem = 3
      putexp tvwEPF.SelectedItem.key
      fraConcentration.Caption = tvwEPF.SelectedItem.Text
      fraConcentration.Visible = True

  End Select
End Sub

Private Sub unit_Change(Index As Integer)
  HelpAnchor = lbl(Index).Tag
End Sub

Private Sub unit_GotFocus(Index As Integer)
  HelpAnchor = lbl(Index).Tag
End Sub

Private Sub vaSpread1_GotFocus()
  HelpAnchor = vaSpread1.Tag
End Sub

Private Sub vaSpread2_GotFocus()
  HelpAnchor = vaSpread2.Tag
End Sub
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

Private Sub vaSpread3_Advance(ByVal AdvanceNext As Boolean)
  HelpAnchor = vaSpread3.Tag
End Sub
