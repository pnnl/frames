VERSION 5.00
Object = "{F856EC8B-F03C-4515-BDC6-64CBD617566A}#7.0#0"; "FPSPR70.ocx"
Object = "{6B7E6392-850A-101B-AFC0-4210102A8DA7}#1.3#0"; "comctl32.ocx"
Begin VB.Form frmRIF 
   BorderStyle     =   1  'Fixed Single
   ClientHeight    =   5304
   ClientLeft      =   2268
   ClientTop       =   2052
   ClientWidth     =   8448
   Icon            =   "Rif.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   442
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   704
   StartUpPosition =   1  'CenterOwner
   Begin ComctlLib.TreeView tvwEPF 
      Height          =   5052
      Left            =   120
      TabIndex        =   0
      Top             =   120
      Width           =   4812
      _ExtentX        =   8488
      _ExtentY        =   8911
      _Version        =   327682
      HideSelection   =   0   'False
      Indentation     =   0
      LabelEdit       =   1
      Style           =   3
      ImageList       =   "imlSmallIcons"
      BorderStyle     =   1
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
   Begin VB.Frame fraMedia 
      Height          =   5200
      Left            =   5040
      TabIndex        =   14
      Top             =   0
      Width           =   3288
      Begin VB.ComboBox unit 
         Height          =   288
         Index           =   4
         ItemData        =   "Rif.frx":030A
         Left            =   1800
         List            =   "Rif.frx":030C
         Style           =   2  'Dropdown List
         TabIndex        =   17
         Tag             =   "km"
         Top             =   480
         Width           =   1200
      End
      Begin VB.OptionButton optDataSet 
         Caption         =   "Chronic"
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
         Index           =   0
         Left            =   324
         TabIndex        =   15
         Tag             =   "medium"
         Top             =   4560
         Value           =   -1  'True
         Visible         =   0   'False
         Width           =   1600
      End
      Begin FPSpreadADO.fpSpread vaSpread1 
         Height          =   1224
         Left            =   204
         TabIndex        =   18
         Tag             =   "coordinates"
         Top             =   1320
         Width           =   1176
         _Version        =   458752
         _ExtentX        =   4932
         _ExtentY        =   5249
         _StockProps     =   64
         ArrowsExitEditMode=   -1  'True
         AutoCalc        =   0   'False
         AutoSize        =   -1  'True
         BackColorStyle  =   1
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
         ScrollBarExtMode=   -1  'True
         ScrollBarShowMax=   0   'False
         SpreadDesigner  =   "Rif.frx":030E
         StartingColNumber=   0
         StartingRowNumber=   0
         UserResize      =   0
         VisibleCols     =   2
         VisibleRows     =   12
      End
      Begin VB.OptionButton optDataSet 
         Caption         =   "Acute"
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
         Index           =   1
         Left            =   360
         TabIndex        =   16
         Tag             =   "medium"
         Top             =   4560
         Visible         =   0   'False
         Width           =   1600
      End
      Begin VB.Label Lbl 
         Caption         =   "Coordinates in"
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
         Index           =   4
         Left            =   204
         TabIndex        =   20
         Tag             =   "coordinates"
         Top             =   480
         Width           =   1596
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
         Height          =   252
         Index           =   1
         Left            =   204
         TabIndex        =   19
         Tag             =   "0"
         Top             =   1080
         Width           =   1212
      End
   End
   Begin VB.Frame fraChemical 
      Height          =   5200
      Left            =   5040
      TabIndex        =   30
      Top             =   0
      Width           =   3288
      Begin VB.ComboBox unit 
         Height          =   288
         Index           =   2
         ItemData        =   "Rif.frx":6B64
         Left            =   1800
         List            =   "Rif.frx":6B66
         Style           =   2  'Dropdown List
         TabIndex        =   31
         Tag             =   "yr"
         Top             =   360
         Width           =   1200
      End
      Begin FPSpreadADO.fpSpread vaSpread2 
         Height          =   2520
         Left            =   204
         TabIndex        =   32
         Tag             =   "time"
         Top             =   1320
         Width           =   1536
         _Version        =   458752
         _ExtentX        =   2709
         _ExtentY        =   4445
         _StockProps     =   64
         ArrowsExitEditMode=   -1  'True
         AutoCalc        =   0   'False
         AutoSize        =   -1  'True
         BackColorStyle  =   1
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
         MaxCols         =   1
         MaxRows         =   5000
         RowsFrozen      =   1
         ScrollBarExtMode=   -1  'True
         ScrollBarShowMax=   0   'False
         SpreadDesigner  =   "Rif.frx":6B68
         StartingColNumber=   0
         StartingRowNumber=   0
         UserResize      =   0
         VisibleCols     =   2
         VisibleRows     =   10
      End
      Begin VB.CheckBox chkCarc 
         Caption         =   "Carcinogenic Pathways"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   192
         Left            =   324
         TabIndex        =   47
         Top             =   4560
         Value           =   1  'Checked
         Width           =   2772
      End
      Begin VB.CheckBox chkNonCarc 
         Caption         =   "Non-Carcinogenic Pathways"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   192
         Left            =   324
         TabIndex        =   48
         Top             =   4800
         Value           =   1  'Checked
         Width           =   2772
      End
      Begin VB.Label Label8 
         Caption         =   "Create for this chemical"
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
         Left            =   200
         TabIndex        =   49
         Top             =   4200
         Width           =   2892
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
         Height          =   252
         Index           =   2
         Left            =   200
         TabIndex        =   34
         Tag             =   "0"
         Top             =   1080
         Width           =   1212
      End
      Begin VB.Label Lbl 
         Caption         =   "Time in"
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
         Index           =   2
         Left            =   200
         TabIndex        =   33
         Tag             =   "time"
         Top             =   360
         Width           =   1596
      End
   End
   Begin VB.Frame fraExposure 
      Height          =   5200
      Left            =   5040
      TabIndex        =   5
      Top             =   0
      Width           =   3285
      Begin VB.ComboBox cmbInh 
         Height          =   288
         ItemData        =   "Rif.frx":A0D0
         Left            =   1700
         List            =   "Rif.frx":A0DA
         Style           =   2  'Dropdown List
         TabIndex        =   46
         Top             =   600
         Width           =   1400
      End
      Begin VB.ComboBox cmbExt 
         Height          =   288
         ItemData        =   "Rif.frx":A0FA
         Left            =   1700
         List            =   "Rif.frx":A104
         Style           =   2  'Dropdown List
         TabIndex        =   45
         Top             =   3120
         Width           =   1400
      End
      Begin VB.ListBox lstExposure 
         Height          =   1200
         Index           =   4
         ItemData        =   "Rif.frx":A11D
         Left            =   1700
         List            =   "Rif.frx":A11F
         MultiSelect     =   1  'Simple
         Sorted          =   -1  'True
         TabIndex        =   13
         Tag             =   "pathway"
         Top             =   3480
         Width           =   1400
      End
      Begin VB.ListBox lstExposure 
         Height          =   1584
         Index           =   3
         ItemData        =   "Rif.frx":A121
         Left            =   150
         List            =   "Rif.frx":A123
         MultiSelect     =   1  'Simple
         Sorted          =   -1  'True
         TabIndex        =   12
         Tag             =   "pathway"
         Top             =   3120
         Width           =   1400
      End
      Begin VB.ListBox lstExposure 
         Height          =   1392
         Index           =   2
         ItemData        =   "Rif.frx":A125
         Left            =   1700
         List            =   "Rif.frx":A127
         MultiSelect     =   1  'Simple
         Sorted          =   -1  'True
         TabIndex        =   11
         Tag             =   "pathway"
         Top             =   960
         Width           =   1400
      End
      Begin VB.ListBox lstExposure 
         Height          =   1776
         Index           =   1
         ItemData        =   "Rif.frx":A129
         Left            =   150
         List            =   "Rif.frx":A12B
         MultiSelect     =   1  'Simple
         Sorted          =   -1  'True
         TabIndex        =   10
         Tag             =   "pathway"
         Top             =   600
         Width           =   1400
      End
      Begin VB.CommandButton cmdRoute 
         Caption         =   "Ingestion"
         Height          =   255
         Index           =   1
         Left            =   150
         TabIndex        =   9
         Tag             =   "pathway"
         Top             =   360
         Width           =   1400
      End
      Begin VB.CommandButton cmdRoute 
         Caption         =   "Inhalation"
         Height          =   255
         Index           =   2
         Left            =   1700
         TabIndex        =   8
         Tag             =   "pathway"
         Top             =   360
         Width           =   1400
      End
      Begin VB.CommandButton cmdRoute 
         Caption         =   "Dermal"
         Height          =   255
         Index           =   3
         Left            =   150
         TabIndex        =   7
         Tag             =   "pathway"
         Top             =   2880
         Width           =   1400
      End
      Begin VB.CommandButton cmdRoute 
         Caption         =   "External"
         Height          =   255
         Index           =   4
         Left            =   1700
         TabIndex        =   6
         Tag             =   "pathway"
         Top             =   2880
         Width           =   1400
      End
   End
   Begin VB.Frame fraEPF 
      Height          =   5200
      Left            =   5040
      TabIndex        =   1
      Top             =   0
      Width           =   3285
      Begin VB.ListBox lstMedia 
         Height          =   816
         ItemData        =   "Rif.frx":A12D
         Left            =   200
         List            =   "Rif.frx":A140
         MultiSelect     =   1  'Simple
         TabIndex        =   2
         Tag             =   "known"
         Top             =   3480
         Width           =   2892
      End
      Begin VB.Label Label7 
         Caption         =   $"Rif.frx":A171
         Height          =   1356
         Left            =   200
         TabIndex        =   4
         Top             =   288
         Width           =   2892
      End
      Begin VB.Label Label1 
         Caption         =   $"Rif.frx":A226
         Height          =   972
         Left            =   200
         TabIndex        =   3
         Top             =   2112
         Width           =   2892
      End
   End
   Begin VB.Frame fraAgeGroup 
      Height          =   5200
      Left            =   5040
      TabIndex        =   28
      Top             =   0
      Width           =   3285
      Begin VB.TextBox ExpPop 
         Alignment       =   1  'Right Justify
         Height          =   288
         Left            =   480
         TabIndex        =   53
         Tag             =   "population"
         Top             =   600
         Width           =   840
      End
      Begin VB.TextBox txt 
         Alignment       =   1  'Right Justify
         Height          =   288
         Index           =   3
         Left            =   480
         TabIndex        =   52
         Top             =   1320
         Width           =   852
      End
      Begin VB.ComboBox unit 
         Height          =   288
         Index           =   3
         ItemData        =   "Rif.frx":A2AE
         Left            =   1320
         List            =   "Rif.frx":A2B0
         Style           =   2  'Dropdown List
         TabIndex        =   50
         Tag             =   "yr"
         Top             =   1320
         Width           =   1080
      End
      Begin VB.Label Label5 
         Caption         =   "Population"
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
         Left            =   240
         TabIndex        =   54
         Tag             =   "population"
         Top             =   360
         Width           =   1488
      End
      Begin VB.Label Lbl 
         Caption         =   "Duration"
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
         Index           =   3
         Left            =   240
         TabIndex        =   51
         Tag             =   "time"
         Top             =   1080
         Width           =   1596
      End
      Begin VB.Label Label6 
         Caption         =   $"Rif.frx":A2B2
         Height          =   1092
         Left            =   240
         TabIndex        =   29
         Top             =   1800
         Width           =   2772
      End
   End
   Begin VB.Frame fraAgePanel 
      Height          =   5200
      Left            =   5040
      TabIndex        =   35
      Top             =   0
      Width           =   3285
      Begin VB.ComboBox unit 
         Height          =   288
         Index           =   5
         ItemData        =   "Rif.frx":A350
         Left            =   1800
         List            =   "Rif.frx":A352
         Style           =   2  'Dropdown List
         TabIndex        =   39
         Tag             =   "yr"
         Top             =   720
         Width           =   1200
      End
      Begin VB.CommandButton cmdAge 
         Caption         =   "Add"
         Height          =   372
         Index           =   0
         Left            =   1800
         TabIndex        =   38
         Tag             =   "age"
         Top             =   1920
         Width           =   972
      End
      Begin VB.CommandButton cmdAge 
         Caption         =   "Remove"
         Height          =   372
         Index           =   1
         Left            =   1800
         TabIndex        =   37
         Tag             =   "age"
         Top             =   2400
         Width           =   975
      End
      Begin VB.ListBox lstAges 
         Height          =   2736
         Left            =   200
         MultiSelect     =   2  'Extended
         TabIndex        =   36
         Tag             =   "age"
         Top             =   1680
         Width           =   1452
      End
      Begin VB.Label Lbl 
         Caption         =   "Age Group Units"
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
         Index           =   5
         Left            =   200
         TabIndex        =   44
         Tag             =   "age"
         Top             =   720
         Width           =   1608
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
         Height          =   252
         Index           =   4
         Left            =   1800
         TabIndex        =   43
         Tag             =   "0"
         Top             =   1440
         Width           =   1212
      End
      Begin VB.Label Label2 
         Caption         =   "Number of Age Groups"
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
         Left            =   200
         TabIndex        =   42
         Top             =   360
         Width           =   2052
      End
      Begin VB.Label Label3 
         Alignment       =   1  'Right Justify
         Caption         =   "0"
         Height          =   192
         Left            =   2640
         TabIndex        =   41
         Top             =   360
         Width           =   288
      End
      Begin VB.Label Label4 
         Caption         =   "Age Groups"
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
         Left            =   200
         TabIndex        =   40
         Top             =   1440
         Width           =   1332
      End
   End
   Begin VB.Frame fraConcentration 
      Height          =   5200
      Left            =   5040
      TabIndex        =   21
      Top             =   0
      Width           =   3285
      Begin VB.ComboBox unit 
         Enabled         =   0   'False
         Height          =   288
         Index           =   0
         ItemData        =   "Rif.frx":A354
         Left            =   1800
         List            =   "Rif.frx":A356
         Style           =   2  'Dropdown List
         TabIndex        =   24
         Tag             =   "yr"
         Top             =   720
         Width           =   1200
      End
      Begin VB.ComboBox unit 
         Height          =   288
         Index           =   1
         ItemData        =   "Rif.frx":A358
         Left            =   1800
         List            =   "Rif.frx":A35A
         Style           =   2  'Dropdown List
         TabIndex        =   22
         Top             =   1080
         Width           =   1200
      End
      Begin FPSpreadADO.fpSpread vaSpread3 
         Height          =   3360
         Left            =   210
         TabIndex        =   23
         Tag             =   "concentrations"
         Top             =   1800
         Width           =   2205
         _Version        =   458752
         _ExtentX        =   3889
         _ExtentY        =   5927
         _StockProps     =   64
         ArrowsExitEditMode=   -1  'True
         AutoCalc        =   0   'False
         BackColorStyle  =   1
         ColsFrozen      =   1
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
         MaxRows         =   5000
         ScrollBarExtMode=   -1  'True
         ScrollBarShowMax=   0   'False
         SpreadDesigner  =   "Rif.frx":A35C
         VisibleCols     =   2
         VisibleRows     =   11
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
         Height          =   252
         Index           =   3
         Left            =   200
         TabIndex        =   27
         Tag             =   "0"
         Top             =   1560
         Width           =   1212
      End
      Begin VB.Label Lbl 
         Caption         =   "Time units"
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
         Index           =   0
         Left            =   200
         TabIndex        =   26
         Tag             =   "concentrations"
         Top             =   720
         Width           =   1608
      End
      Begin VB.Label Lbl 
         Caption         =   "Value units"
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
         Index           =   1
         Left            =   200
         TabIndex        =   25
         Tag             =   "concentrations"
         Top             =   1080
         Width           =   1608
      End
   End
   Begin ComctlLib.ImageList imlSmallIcons 
      Left            =   7680
      Top             =   4560
      _ExtentX        =   995
      _ExtentY        =   995
      BackColor       =   -2147483643
      ImageWidth      =   13
      ImageHeight     =   13
      MaskColor       =   12632256
      _Version        =   327682
      BeginProperty Images {0713E8C2-850A-101B-AFC0-4210102A8DA7} 
         NumListImages   =   6
         BeginProperty ListImage1 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "Rif.frx":A62E
            Key             =   "closed"
         EndProperty
         BeginProperty ListImage2 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "Rif.frx":AB50
            Key             =   "cylinder"
         EndProperty
         BeginProperty ListImage3 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "Rif.frx":B072
            Key             =   "leaf"
         EndProperty
         BeginProperty ListImage4 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "Rif.frx":B594
            Key             =   "open"
         EndProperty
         BeginProperty ListImage5 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "Rif.frx":BAB6
            Key             =   "smlBook"
         EndProperty
         BeginProperty ListImage6 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "Rif.frx":C118
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
      Enabled         =   0   'False
      Visible         =   0   'False
      Begin VB.Menu NumGeneral 
         Caption         =   "General"
         Checked         =   -1  'True
      End
      Begin VB.Menu NumStandard 
         Caption         =   "Standard"
         Enabled         =   0   'False
         Visible         =   0   'False
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
Attribute VB_Name = "frmRIF"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Dim mnode As ComctlLib.Node
Dim temp As parmrec
Dim loadng As Boolean
Dim paths(21) As String
Dim ExpPathOK(3, 4) As New Collection
Dim treeclick As Boolean

Private Sub chkCarc_Click()
  If treeclick Then Exit Sub
  med(medIdx).ages(ageIdx).chem(chmIdx).haveCarc = chkCarc.Value
End Sub

Private Sub chkNonCarc_Click()
  If treeclick Then Exit Sub
  med(medIdx).ages(ageIdx).chem(chmIdx).haveNonCarc = chkNonCarc.Value
End Sub

Private Sub howto_Click()
  GetHelp
End Sub

Private Sub about_Click()
  frmAbout.picIcon = frmAbout.ImageList1.ListImages(1).Picture
  frmAbout.Show 1, frmRIF
End Sub

Public Sub AddChemNode(parent As ComctlLib.Node)
  Dim i As Integer
  Dim j As Integer
  Dim key1 As String
  Dim key2 As String
  Dim tempnode As ComctlLib.Node
  
  For i = 1 To numcon
    key1 = parent.key + "," + con(i).name
    Set tempnode = tvwEPF.Nodes.Add(parent.key, tvwChild, key1, con(i).name, CLOSED, CLOSED)
    tempnode.Tag = "Chemical"
    tempnode.Expanded = True
    tempnode.EnsureVisible
    
    key2 = key1 + "," + CONSTITUENT
    Set tempnode = tvwEPF.Nodes.Add(key1, tvwChild, key2, CONSTITUENT, CLOSED, CLOSED)
    tempnode.Tag = CONSTITUENT
    tempnode.Expanded = True
    tempnode.EnsureVisible
    
    If prog.Checked Then
      For j = 1 To con(i).numprog
        key2 = key1 + "," + con(i).prog(j).name
        Set tempnode = tvwEPF.Nodes.Add(key1, tvwChild, key2, con(i).prog(j).name, CLOSED, CLOSED)
        tempnode.Tag = "Progeny"
        tempnode.Expanded = True
        tempnode.EnsureVisible
      Next
    End If
  Next
End Sub

Private Sub cmdRoute_Click(Index As Integer)
  Dim tempstring As String
  
  tempstring = InputBox("Enter New Exposure Pathway", "Add Exposure Pathway")
  If Not tempstring = "" Then
    lstExposure(Index).AddItem tempstring
  End If
End Sub

Public Function SearchAgeGroups(begAge As String, endAge As String) As Boolean
  Dim i As Integer
  Dim agestring
  
  agestring = begAge + "-" + endAge
  For i = 0 To lstAges.ListCount - 1
    If agestring = lstAges.list(i) Then
      SearchAgeGroups = True
      Exit Function
    End If
  Next
  SearchAgeGroups = False
End Function

Private Sub cmdAge_Click(Index As Integer)
  Dim tempend As String
  Dim tempstart As String
  Dim agenodename As String
  Dim tempdouble As Double
  Dim tempvalue1 As String
  Dim tempvalue2 As String
  Dim pKey As String
  Dim keystring As String
  Dim i As Integer
  Dim j As Integer
  Dim k As Integer
  Dim m As Integer
  Dim numcheck As Boolean
  
  If Index = 0 Then 'add age group
    'Get starting age for group
    tempdouble = -999
    numcheck = False
    While numcheck = False
      tempstart = InputBox("Please enter a valid Starting Age", "Start Age")
      If tempstart = "" Then Exit Sub
On Error GoTo NUMBERERROR
      tempdouble = CDbl(tempstart)
      If Not tempdouble = -999 Then
        numcheck = True
      End If
    Wend
    'Get ending age for group
    tempdouble = -999
    numcheck = False
    While numcheck = False
      tempend = InputBox("Please enter a valid Ending Age", "End Age")
      If tempend = "" Then Exit Sub
On Error GoTo NUMBERERROR
      tempdouble = CDbl(tempend)
      If Not tempdouble = -999 Then
        numcheck = True
      End If
    Wend
    'make sure ages are OK
    If CDbl(tempstart) >= CDbl(tempend) Then
      MsgBox "Start Age must be less than End Age", vbOKOnly + vbExclamation, "Error"
      Exit Sub
    End If
    If SearchAgeGroups(tempstart, tempend) = True Then
      MsgBox "Duplicate Age Groups can not be entered", vbExclamation + vbOKOnly, "Error"
      Exit Sub
    End If
    
    'Add age group to media
On Error GoTo 0
    lstAges.AddItem Format(tempstart, CVTFormat) + "-" + Format(tempend, CVTFormat)
    Label3.Caption = lstAges.ListCount
    For m = 1 To MAXMEDIA
      agenodename = "Age Group " + Format(tempstart, CVTFormat) + "-" + Format(tempend, CVTFormat)
      pKey = Replace(tvwEPF.SelectedItem.key, tvwEPF.SelectedItem.parent.Text, med(m).name)
      keystring = pKey + "," + CStr(tempstart) + "-" + CStr(tempend)
      If med(m).use Then
        Set mnode = tvwEPF.Nodes.Add(pKey, tvwChild, keystring, agenodename, CLOSED, CLOSED)
        mnode.Tag = "AgeGroup"
        mnode.Expanded = True
        mnode.EnsureVisible
        AddChemNode mnode
      End If
      ReDim Preserve med(m).ages(lstAges.ListCount) As agegroup
      med(m).ages(lstAges.ListCount).dunit(INTLUNIT) = YR
      med(m).ages(lstAges.ListCount).dunit(USERUNIT) = YR
      med(m).ages(lstAges.ListCount).ageunit(INTLUNIT) = YR
      med(m).ages(lstAges.ListCount).ageunit(USERUNIT) = unit(5).list(unit(5).ListIndex)
      med(m).ages(lstAges.ListCount).begAge = CDbl(tempstart)
      med(m).ages(lstAges.ListCount).endAge = CDbl(tempend)
      med(m).ages(lstAges.ListCount).population = 1
      med(m).ages(lstAges.ListCount).use = 1
      med(m).numages = lstAges.ListCount
      For i = 1 To med(m).numages
        med(m).ages(i).numchem = numcon
        ReDim Preserve med(m).ages(i).chem(numcon) As sParent
        For j = 1 To numcon
          med(m).ages(i).chem(j).cas = con(j).cas
          med(m).ages(i).chem(j).name = con(j).name
          med(m).ages(i).chem(j).kind = con(j).kind
          If con(j).kind = 1 Then
            med(m).ages(i).chem(j).haveCarc = 1
          Else
            med(m).ages(i).chem(j).haveNonCarc = 1
          End If
          med(m).ages(i).chem(j).numprog = con(j).numprog
          ReDim Preserve med(m).ages(i).chem(j).prog(con(j).numprog) As sProgeny
          For k = 1 To con(j).numprog
            med(m).ages(i).chem(j).prog(k).cas = con(j).prog(k).cas
            med(m).ages(i).chem(j).prog(k).name = con(j).prog(k).name
            med(m).ages(i).chem(j).prog(k).kind = con(j).prog(k).kind
          Next
        Next
      Next
    Next
  Else 'remove age group
    For i = lstAges.ListCount To 1 Step -1
      If lstAges.Selected(i - 1) Then
        For m = 1 To MAXMEDIA
          med(m).ages(i).use = 0
          keystring = Replace(tvwEPF.SelectedItem.key, tvwEPF.SelectedItem.parent.Text, med(m).name) + "," + lstAges.list(i - 1)
          If med(m).use Then
            tvwEPF.Nodes.Remove keystring
          End If
          ResizeAges m
        Next
        lstAges.RemoveItem i - 1
      End If
    Next
    Label3.Caption = lstAges.ListCount
  End If
  Exit Sub
  
NUMBERERROR:
  numcheck = False
  Resume Next
End Sub

Private Sub Form_Unload(Cancel As Integer)
  Dim answer As Long
  If Not called Then
    answer = MsgBox("Do you want save changes?", 51, frmRIF.Caption)
    If answer = 6 Then save_Click
    If answer = 7 Then EndModule
    If answer = 2 Then Cancel = 1
  End If
End Sub

Private Sub Check_NTS(nts As Long, errmsg As String)
  If (nts = 1) Then PutError errmsg
End Sub

Private Sub leave_Click()
  Form_Unload 0
End Sub

Function GetRoute(Index) As String
  Select Case Index
    Case 1:    GetRoute = INGESTION
    Case 2:    GetRoute = INHALATION
    Case 3:    GetRoute = DERMAL
    Case 4:    GetRoute = EXTERNAL
  End Select
End Function

Function GetRouteIdx(Route As String) As Long
  Select Case Route
    Case INGESTION:   GetRouteIdx = 1
    Case INHALATION:  GetRouteIdx = 2
    Case DERMAL:      GetRouteIdx = 3
    Case EXTERNAL:    GetRouteIdx = 4
  End Select
End Function

Private Sub CreatePathway(medIdx As Integer, ageIdx As Integer, chmIdx As Integer, Index As Integer, unit As String, xType As String, Optional eType As String = "")
  Dim tempnode As ComctlLib.Node
  Dim Route As String
  Dim key As String
  Dim pathkey As String
  Dim prgIdx As Integer
  Dim numexposures As Integer

  Route = GetRoute(Index)
  If unit = "" Then MsgBox "crap"
  pathkey = Route + " " + xType + " for " + lstExposure(Index).Text
  If eType <> "" Then pathkey = pathkey + " (" + eType + ")"
  key = tvwEPF.SelectedItem.key + "," + pathkey
  Set tempnode = tvwEPF.Nodes.Add(tvwEPF.SelectedItem.key, tvwChild, key, pathkey, CLOSED, CLOSED)
  tempnode.Tag = Route
  tempnode.Expanded = True
  tempnode.EnsureVisible
  numexposures = med(medIdx).ages(ageIdx).chem(chmIdx).numexp + 1
  If tvwEPF.SelectedItem.Text = CONSTITUENT Then
    med(medIdx).ages(ageIdx).chem(chmIdx).numexp = numexposures
    ReDim Preserve med(medIdx).ages(ageIdx).chem(chmIdx).exp(numexposures) As Exposure
    med(medIdx).ages(ageIdx).chem(chmIdx).exp(numexposures).Path = lstExposure(Index).Text
    med(medIdx).ages(ageIdx).chem(chmIdx).exp(numexposures).Route = Route
    med(medIdx).ages(ageIdx).chem(chmIdx).exp(numexposures).cunit(USERUNIT) = unit
    med(medIdx).ages(ageIdx).chem(chmIdx).exp(numexposures).cunit(INTLUNIT) = unit
    med(medIdx).ages(ageIdx).chem(chmIdx).exp(numexposures).eType = eType
    med(medIdx).ages(ageIdx).chem(chmIdx).exp(numexposures).xType = xType
    med(medIdx).ages(ageIdx).chem(chmIdx).exp(numexposures).use = 1
  Else
    prgIdx = GetProgenyIndex(medIdx, ageIdx, chmIdx, tvwEPF.SelectedItem.Text)
    numexposures = med(medIdx).ages(ageIdx).chem(chmIdx).prog(prgIdx).numexp + 1
    med(medIdx).ages(ageIdx).chem(chmIdx).prog(prgIdx).numexp = numexposures
    ReDim Preserve med(medIdx).ages(ageIdx).chem(chmIdx).prog(prgIdx).exp(numexposures) As Exposure
    med(medIdx).ages(ageIdx).chem(chmIdx).prog(prgIdx).exp(numexposures).Path = lstExposure(Index).Text
    med(medIdx).ages(ageIdx).chem(chmIdx).prog(prgIdx).exp(numexposures).Route = Route
    med(medIdx).ages(ageIdx).chem(chmIdx).prog(prgIdx).exp(numexposures).cunit(USERUNIT) = unit
    med(medIdx).ages(ageIdx).chem(chmIdx).prog(prgIdx).exp(numexposures).cunit(INTLUNIT) = unit
    med(medIdx).ages(ageIdx).chem(chmIdx).prog(prgIdx).exp(numexposures).eType = eType
    med(medIdx).ages(ageIdx).chem(chmIdx).prog(prgIdx).exp(numexposures).xType = xType
    med(medIdx).ages(ageIdx).chem(chmIdx).prog(prgIdx).exp(numexposures).use = 1
  End If
End Sub

Private Sub DelPathway(medIdx As Integer, ageIdx As Integer, chmIdx As Integer, Index As Integer, xType As String, Optional eType As String = "")
  Dim expIdx As Integer
  Dim Route As String
  Dim key As String
  Dim pathkey As String
  Dim prgIdx As Integer
  
  prgIdx = 0
  Route = GetRoute(Index)
  pathkey = Route + " " + xType + " for " + lstExposure(Index).Text
  If eType <> "" Then pathkey = pathkey + " (" + eType + ")"
  key = tvwEPF.SelectedItem.key + "," + pathkey
On Error GoTo delexit
  tvwEPF.Nodes.Remove (key)
  If tvwEPF.SelectedItem.Text <> CONSTITUENT Then prgIdx = GetProgenyIndex(medIdx, ageIdx, chmIdx, tvwEPF.SelectedItem.Text)
  expIdx = GetExposureIndex(medIdx, ageIdx, chmIdx, prgIdx, lstExposure(Index).Text, Route, xType, eType)
  med(medIdx).ages(ageIdx).chem(chmIdx).exp(expIdx).use = 0
  ResizeExposures medIdx, ageIdx, chmIdx, prgIdx
  
delexit:

End Sub

Private Sub routepathway(Index As Integer, cInType As String, cUnits As String, rUnits As String, rIntype As String)
  Dim medIdx As Integer
  Dim ageIdx As Integer
  Dim chmIdx As Integer
  Dim tempstart As String
  Dim tempend As String
    
  medIdx = GetMedArrayIndex(tvwEPF.SelectedItem.parent.parent.parent.parent.Text)
  GetAges tvwEPF.SelectedItem.parent.parent.key, tempstart, tempend
  ageIdx = GetAgeIndex(medIdx, CDbl(tempstart), CDbl(tempend))
  chmIdx = GetChemIndex(medIdx, ageIdx, tvwEPF.SelectedItem.parent)
       
    If lstExposure(Index).Selected(lstExposure(Index).ListIndex) = True Then
      If med(medIdx).ages(ageIdx).chem(chmIdx).kind <> 1 Then
        If med(medIdx).ages(ageIdx).chem(chmIdx).haveCarc = 1 Then CreatePathway medIdx, ageIdx, chmIdx, Index, cUnits, cInType, CARC
        If med(medIdx).ages(ageIdx).chem(chmIdx).haveNonCarc = 1 Then CreatePathway medIdx, ageIdx, chmIdx, Index, cUnits, cInType, NARC
      Else
        If med(medIdx).ages(ageIdx).chem(chmIdx).haveCarc = 1 Then CreatePathway medIdx, ageIdx, chmIdx, Index, rUnits, rIntype
      End If
    Else  'remove node
      If med(medIdx).ages(ageIdx).chem(chmIdx).kind <> 1 Then
        DelPathway medIdx, ageIdx, chmIdx, Index, INTAKE, CARC
        DelPathway medIdx, ageIdx, chmIdx, Index, INTAKE, NARC
        DelPathway medIdx, ageIdx, chmIdx, Index, CONC, CARC
        DelPathway medIdx, ageIdx, chmIdx, Index, CONC, NARC
      Else
        DelPathway medIdx, ageIdx, chmIdx, Index, INTAKE
        DelPathway medIdx, ageIdx, chmIdx, Index, CONC
        DelPathway medIdx, ageIdx, chmIdx, Index, DOSE
      End If
    End If
End Sub
Private Sub lstExposure_Click(Index As Integer)
  
  If treeclick = True Then Exit Sub
  Select Case Index
    Case 1:                      routepathway Index, INTAKE, MGKGDAY, BQ, INTAKE    ' INGESTION
    Case 2:                                                                         ' INHALATION
      Select Case cmbInh.Text
        Case "Intake Rate":      routepathway Index, INTAKE, MGKGDAY, BQ, INTAKE
        Case "Concentration":    routepathway Index, CONC, MGM3, BQ, INTAKE
      End Select
    Case 3:                      routepathway Index, INTAKE, MGKGDAY, BQ, INTAKE    ' DERMAL
    Case 4:                                                                         ' EXTERNAL
      Select Case cmbExt.Text
        Case "Dose":             routepathway Index, "", "", SV, DOSE
        Case "Concentration":
          Select Case lstExposure(Index).Text
            Case SOIL, "Shoreline":
                                 routepathway Index, "", "", BQKG, CONC
            Case AIR:            routepathway Index, "", "", BQM3, CONC
            Case WATER, "Swimming", "Boating":
                                 routepathway Index, "", "", BQL, CONC
            Case Else
               If lstExposure(Index).Selected(lstExposure(Index).ListIndex) = True Then
                 MsgBox "Unknown pathway must enter as radiation dose", , "Receptor Pathway Warning"
               End If
               routepathway Index, "", "", SV, DOSE
          End Select
      End Select
  End Select
End Sub

Private Sub lstMedia_Click()
  Dim Index As Long
  Dim keystring As String
  
  If lstMedia.list(lstMedia.ListIndex) = "Wetlands" Then
    MsgBox "Wetlands is not a supported medium at this time.", vbOKOnly
    Exit Sub
  End If
  
  numrif = MAXMEDIA
  Index = GetMedArrayIndex(lstMedia.list(lstMedia.ListIndex))
  If lstMedia.Selected(lstMedia.ListIndex) Then
    med(Index).use = 1
    med(Index).name = lstMedia.Text
    LoadTreeViewFromRead
  Else
    keystring = "RIF" + "," + lstMedia.Text
    ' key may or may not exist if this is the first time
    On Error Resume Next
    tvwEPF.Nodes.Remove keystring
    On Error GoTo 0
    med(Index).use = 0
  End If
End Sub

Private Sub optDataSet_Click(Index As Integer)
  If treeclick Then Exit Sub
  If optDataSet(0).Value = True Then
    med(medIdx).kind = 0
  Else
    med(medIdx).kind = 1
  End If
End Sub

Private Sub prog_Click()
  prog.Checked = Not prog.Checked
  LoadTreeViewFromRead
End Sub

Private Sub ref_Change(Index As Integer)
  Dim i As Integer
  
  If treeclick = True Then Exit Sub
  Select Case Index
  Case 1
    med(medIdx).ref = ref(1).Tag
  Case 2
    med(medIdx).ages(ageIdx).chem(chmIdx).ref = ref(2).Tag
  Case 3
    If tvwEPF.SelectedItem.parent.Tag = CONSTITUENT Then
      med(medIdx).ages(ageIdx).chem(chmIdx).exp(expIdx).ref = ref(3).Tag
    Else
      med(medIdx).ages(ageIdx).chem(chmIdx).prog(prgIdx).exp(expIdx).ref = ref(3).Tag
    End If
  Case 4
    For i = 1 To med(medIdx).numages
      med(medIdx).ages(i).ref = ref(4).Tag
    Next
  End Select
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

Public Function GetMedIndex(parmname As String) As Integer
  Dim tempchar As String
  
  tempchar = Mid(parmname, Len(parmname), 1)
  GetMedIndex = Val(tempchar)
End Function

Private Sub SizeRifSeries()
  Dim i As Long
  'SizeRif
  SizeRifAge
  SizeRifExp
  If temp.idx3 = 0 Then
    If temp.idx5 > med(GetMedIndex(temp.pname)).ages(temp.idx1).chem(temp.idx2).exp(temp.idx4).numseries Then
      ReDim Preserve med(GetMedIndex(temp.pname)).ages(temp.idx1).chem(temp.idx2).exp(temp.idx4).series(temp.idx5) As Entry
      med(GetMedIndex(temp.pname)).ages(temp.idx1).chem(temp.idx2).exp(temp.idx4).numseries = temp.idx5
      For i = 1 To temp.idx5
        If temp.idx6 > med(GetMedIndex(temp.pname)).ages(temp.idx1).chem(temp.idx2).exp(temp.idx4).series(temp.idx5).num Then
          ReDim Preserve med(GetMedIndex(temp.pname)).ages(temp.idx1).chem(temp.idx2).exp(temp.idx4).series(temp.idx5).valu(temp.idx6) As Double
          med(GetMedIndex(temp.pname)).ages(temp.idx1).chem(temp.idx2).exp(temp.idx4).series(temp.idx5).num = temp.idx6
        End If
      Next
    End If
  Else
    If temp.idx5 > med(GetMedIndex(temp.pname)).ages(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).numseries Then
      ReDim Preserve med(GetMedIndex(temp.pname)).ages(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).series(temp.idx5) As Entry
      med(GetMedIndex(temp.pname)).ages(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).numseries = temp.idx5
      For i = 1 To temp.idx5
        If temp.idx6 > med(GetMedIndex(temp.pname)).ages(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).series(temp.idx5).num Then
          ReDim Preserve med(GetMedIndex(temp.pname)).ages(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).series(temp.idx5).valu(temp.idx6) As Double
          med(GetMedIndex(temp.pname)).ages(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).series(temp.idx5).num = temp.idx6
        End If
      Next
    End If
  End If
  If temp.idx3 > 0 Then
    For i = 1 To temp.idx5
      If temp.idx6 > med(GetMedIndex(temp.pname)).ages(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).series(temp.idx5).num Then
        ReDim Preserve med(GetMedIndex(temp.pname)).ages(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).series(temp.idx5).valu(temp.idx6) As Double
        med(GetMedIndex(temp.pname)).ages(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).series(temp.idx5).num = temp.idx6
      End If
    Next
  End If
End Sub

Private Sub SizeRifExp()
  SizeRifAge
  SizeRifChem
  If temp.idx3 = 0 Then
    If temp.idx4 > med(GetMedIndex(temp.pname)).ages(temp.idx1).chem(temp.idx2).numexp Then
      ReDim Preserve med(GetMedIndex(temp.pname)).ages(temp.idx1).chem(temp.idx2).exp(temp.idx4) As Exposure
      med(GetMedIndex(temp.pname)).ages(temp.idx1).chem(temp.idx2).numexp = temp.idx4
    End If
  Else
    If temp.idx4 > med(GetMedIndex(temp.pname)).ages(temp.idx1).chem(temp.idx2).prog(temp.idx3).numexp Then
      ReDim Preserve med(GetMedIndex(temp.pname)).ages(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4) As Exposure
      med(GetMedIndex(temp.pname)).ages(temp.idx1).chem(temp.idx2).prog(temp.idx3).numexp = temp.idx4
    End If
  End If
End Sub

Private Sub SizeRifChem()
  SizeRifAge
  If temp.idx2 > med(GetMedIndex(temp.pname)).ages(temp.idx1).numchem Then
    ReDim Preserve med(GetMedIndex(temp.pname)).ages(temp.idx1).chem(temp.idx2) As sParent
    med(GetMedIndex(temp.pname)).ages(temp.idx1).numchem = temp.idx2
  End If
  If temp.idx3 > med(GetMedIndex(temp.pname)).ages(temp.idx1).chem(temp.idx2).numprog Then
    ReDim Preserve med(GetMedIndex(temp.pname)).ages(temp.idx1).chem(temp.idx2).prog(temp.idx3) As sProgeny
    med(GetMedIndex(temp.pname)).ages(temp.idx1).chem(temp.idx2).numprog = temp.idx3
  End If
End Sub

Private Sub SizeRifTime()
  SizeRifChem
  If temp.idx4 > med(GetMedIndex(temp.pname)).ages(temp.idx1).chem(temp.idx2).numtime Then
    ReDim Preserve med(GetMedIndex(temp.pname)).ages(temp.idx1).chem(temp.idx2).time(temp.idx4) As Double
    med(GetMedIndex(temp.pname)).ages(temp.idx1).chem(temp.idx2).numtime = temp.idx4
  End If
End Sub

Private Sub SizeRifLoc()
  If temp.idx1 > med(GetMedIndex(temp.pname)).numloc Then
    ReDim Preserve med(GetMedIndex(temp.pname)).locx(temp.idx1) As Double
    ReDim Preserve med(GetMedIndex(temp.pname)).locy(temp.idx1) As Double
    med(GetMedIndex(temp.pname)).numloc = temp.idx1
  End If
End Sub

Private Sub SizeRifAge()
  If temp.idx1 > med(GetMedIndex(temp.pname)).numages Then
    ReDim Preserve med(GetMedIndex(temp.pname)).ages(temp.idx1)
    numages = temp.idx1
    med(GetMedIndex(temp.pname)).numages = temp.idx1
  End If
End Sub

Private Sub SizeRif()
  numrif = MAXMEDIA
End Sub

Public Function GetMedName(medstring As String) As String
  GetMedName = Mid(medstring, 1, Len(medstring) - 1)
End Function

Private Sub loadprm()
  Dim i As Long, j As Long, k As Long, l As Long, m As Long, p As Long
  Dim sval As Boolean
  Dim medIdx As Long
  Dim fle As parmfile
  Dim tempname As String
  
  If open_parm(fle, FUIName, 2) Then
    Do Until EOCF(fle.file)
      If read_parmrec(fle, temp) Then
        Select Case temp.pname
          Case "fui"
            For i = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                If temp.idx1 = siteIdx Then
                  Select Case temp.pname
                    Case "ecodespath"
                      If Left(ModName, 3) = "eco" And temp.idx2 = modIdx Then DesName = temp.pval
                    Case "usrdespath"
                      If Left(ModName, 3) = "usr" And temp.idx2 = modIdx Then DesName = temp.pval
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
          Case ModName
            Loading.Gauge1.Max = Val(temp.idx1 + 1)
            Loading.Gauge1.Value = 1
            For i = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                Loading.update
                
                Select Case temp.pname
                  Case "CVTFORMAT": CVTFormat = temp.pval
                  Case "progeny":
                    sval = False
                    If temp.pval = "True" Then
                     ' sval = True
                     'no more prooduct output
                      MsgBox "This case contained progeny concentrations/intakes which are no loneger supported!" & vbCrLf & _
                           "Those inventories have been removed and must be rentered as parents.", vbInformation
                    End If
                    prog.Checked = sval
                  Case Else:
                  tempname = Mid(temp.pname, 1, Len(temp.pname) - 1)
                  medIdx = GetMedIndex(temp.pname)
                  Select Case tempname
                    Case "locx"
                      SizeRifLoc
                      med(medIdx).locx(temp.idx1) = convert(temp.cunit, temp.uunit, Val(temp.pval))
                      med(medIdx).lunits(USERUNIT) = temp.uunit
                      med(medIdx).lunits(INTLUNIT) = temp.cunit
                    Case "locy"
                      SizeRifLoc
                      med(medIdx).locy(temp.idx1) = convert(temp.cunit, temp.uunit, Val(temp.pval))
                      med(medIdx).lunits(USERUNIT) = temp.uunit
                      med(medIdx).lunits(INTLUNIT) = temp.cunit
                    Case "name"
                      SizeRif
                      med(medIdx).use = 1
                      med(medIdx).name = temp.pval
                      med(medIdx).ref = temp.ref
                    Case "kind"
                      SizeRif
                      med(medIdx).kind = Val(temp.pval)
                    Case "casid":
                      SizeRifChem
                      If temp.idx3 = 0 Then
                        med(medIdx).ages(temp.idx1).chem(temp.idx2).cas = temp.pval
                        med(medIdx).ages(temp.idx1).chem(temp.idx2).ref = temp.ref
                      Else
                        med(medIdx).ages(temp.idx1).chem(temp.idx2).prog(temp.idx3).cas = temp.pval
                        med(medIdx).ages(temp.idx1).chem(temp.idx2).prog(temp.idx3).ref = temp.ref
                      End If
                      med(medIdx).ages(temp.idx1).chem(temp.idx2).tunit(USERUNIT) = YR
                      med(medIdx).ages(temp.idx1).chem(temp.idx2).tunit(INTLUNIT) = YR
                    Case "route":
                      SizeRifExp
                      If temp.idx3 = 0 Then
                        med(medIdx).ages(temp.idx1).chem(temp.idx2).exp(temp.idx4).use = 1
                        med(medIdx).ages(temp.idx1).chem(temp.idx2).exp(temp.idx4).Route = temp.pval
                        med(medIdx).ages(temp.idx1).chem(temp.idx2).exp(temp.idx4).ref = temp.ref
                      Else
                        med(medIdx).ages(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).use = 1
                        med(medIdx).ages(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).Route = temp.pval
                        med(medIdx).ages(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).ref = temp.ref
                      End If
                    Case "path":
                      SizeRifExp
                      If temp.idx3 = 0 Then
                        med(medIdx).ages(temp.idx1).chem(temp.idx2).exp(temp.idx4).Path = temp.pval
                        med(medIdx).ages(temp.idx1).chem(temp.idx2).exp(temp.idx4).ref = temp.ref
                      Else
                        med(medIdx).ages(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).Path = temp.pval
                        med(medIdx).ages(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).ref = temp.ref
                      End If
                    Case "time":
                      SizeRifTime
                      med(medIdx).ages(temp.idx1).chem(temp.idx2).time(temp.idx4) = Val(convert(temp.cunit, temp.uunit, Val(temp.pval)))
                      med(medIdx).ages(temp.idx1).chem(temp.idx2).tunit(USERUNIT) = temp.uunit
                      med(medIdx).ages(temp.idx1).chem(temp.idx2).tunit(INTLUNIT) = temp.cunit
                    Case "dur":
                      SizeRifAge
                      med(medIdx).ages(temp.idx1).duration = Val(convert(temp.cunit, temp.uunit, Val(temp.pval)))
                      med(medIdx).ages(temp.idx1).dunit(USERUNIT) = temp.uunit
                      med(medIdx).ages(temp.idx1).dunit(INTLUNIT) = temp.cunit
                    Case "conc":
                      SizeRifSeries
                      If temp.idx3 = 0 Then
                        ReDim Preserve med(medIdx).ages(temp.idx1).chem(temp.idx2).exp(temp.idx4).series(temp.idx5).valu(temp.idx6) As Double
                        med(medIdx).ages(temp.idx1).chem(temp.idx2).exp(temp.idx4).series(temp.idx5).num = temp.idx6
                        med(medIdx).ages(temp.idx1).chem(temp.idx2).exp(temp.idx4).series(temp.idx5).valu(temp.idx6) = Val(convert(temp.cunit, temp.uunit, Val(temp.pval)))
                        med(medIdx).ages(temp.idx1).chem(temp.idx2).exp(temp.idx4).cunit(USERUNIT) = temp.uunit
                        med(medIdx).ages(temp.idx1).chem(temp.idx2).exp(temp.idx4).cunit(INTLUNIT) = temp.cunit
                      Else
                        ReDim Preserve med(medIdx).ages(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).series(temp.idx5).valu(temp.idx6) As Double
                        med(medIdx).ages(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).series(temp.idx5).num = temp.idx6
                        med(medIdx).ages(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).series(temp.idx5).valu(temp.idx6) = Val(convert(temp.cunit, temp.uunit, Val(temp.pval)))
                        med(medIdx).ages(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).cunit(USERUNIT) = temp.uunit
                        med(medIdx).ages(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).cunit(INTLUNIT) = temp.cunit
                      End If
                    Case "population"
                      SizeRifAge
                      med(medIdx).ages(temp.idx1).population = Val(temp.pval)
                      med(medIdx).ages(temp.idx1).ref = temp.ref
                    Case "efxtype"
                      SizeRifExp
                      If temp.idx3 = 0 Then
                        med(medIdx).ages(temp.idx1).chem(temp.idx2).exp(temp.idx4).eType = temp.pval
                      Else
                        med(medIdx).ages(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).eType = temp.pval
                      End If
                      Select Case temp.pval
                      Case CARC, ""
                        med(medIdx).ages(temp.idx1).chem(temp.idx2).haveCarc = 1
                      Case NARC
                        med(medIdx).ages(temp.idx1).chem(temp.idx2).haveNonCarc = 1
                      End Select
                      
                    Case "exptype"
                      SizeRifExp
                        Select Case temp.pval
                          Case CARC ' now an effect type placed here for backward compatibility
                            eType = temp.pval
                            If temp.cunit = MGKGDAY Then
                              xType = INTAKE
                            Else
                              xType = CONC
                            End If
                            med(medIdx).ages(temp.idx1).chem(temp.idx2).haveCarc = 1
                          Case NARC ' now an effect type placed here for backward compatibility
                            eType = temp.pval
                            If temp.cunit = MGKGDAY Then
                              xType = INTAKE
                            Else
                              xType = CONC
                            End If
                            med(medIdx).ages(temp.idx1).chem(temp.idx2).haveNonCarc = 1
                          Case DOSE, CONC
                            eType = CARC
                            xType = temp.pval
                            med(medIdx).ages(temp.idx1).chem(temp.idx2).haveCarc = 1
                          Case INTAKE
                            xType = temp.pval
                        End Select
                      
                      If temp.idx3 = 0 Then
                        med(medIdx).ages(temp.idx1).chem(temp.idx2).exp(temp.idx4).xType = xType
                        If temp.pval <> INTAKE Then med(medIdx).ages(temp.idx1).chem(temp.idx2).exp(temp.idx4).eType = eType
                        med(medIdx).ages(temp.idx1).chem(temp.idx2).exp(temp.idx4).ref = temp.ref
                        ' set default units - they get lost if gid is saved with incomplete exposure concentrations
                        med(medIdx).ages(temp.idx1).chem(temp.idx2).exp(temp.idx4).cunit(USERUNIT) = temp.uunit
                        med(medIdx).ages(temp.idx1).chem(temp.idx2).exp(temp.idx4).cunit(INTLUNIT) = temp.cunit
                      Else
                        med(medIdx).ages(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).xType = xType
                        If temp.pval <> INTAKE Then med(medIdx).ages(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).eType = eType
                        med(medIdx).ages(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).ref = temp.ref
                        ' set default units - they get lost if gid is saved with incomplete exposure concentrations
                        med(medIdx).ages(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).cunit(USERUNIT) = temp.uunit
                        med(medIdx).ages(temp.idx1).chem(temp.idx2).prog(temp.idx3).exp(temp.idx4).cunit(INTLUNIT) = temp.cunit
                      End If
                    Case "begAge"
                      SizeRifAge
                      med(medIdx).ages(temp.idx1).use = 1
                      med(medIdx).ages(temp.idx1).begAge = convert(temp.cunit, temp.uunit, Val(temp.pval))
                      med(medIdx).ages(temp.idx1).ref = temp.ref
                      med(medIdx).ages(temp.idx1).ageunit(USERUNIT) = temp.uunit
                      med(medIdx).ages(temp.idx1).ageunit(INTLUNIT) = temp.cunit
                    Case "endage"
                      SizeRifAge
                      med(medIdx).ages(temp.idx1).use = 1
                      med(medIdx).ages(temp.idx1).endAge = convert(temp.cunit, temp.uunit, Val(temp.pval))
                      med(medIdx).ages(temp.idx1).ref = temp.ref
                      med(medIdx).ages(temp.idx1).ageunit(USERUNIT) = temp.uunit
                      med(medIdx).ages(temp.idx1).ageunit(INTLUNIT) = temp.cunit
                  End Select
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
    ''epf resolve contaminate differences if contaminate no longer exists its index is 0
    For i = 1 To MAXMEDIA
      If med(i).use = 1 Then
        For p = 1 To med(i).numages
          For j = 1 To numcon
            For k = 1 To med(i).ages(p).numchem
              If con(j).cas = med(i).ages(p).chem(k).cas Then
                med(i).ages(p).chem(k).use = j
                med(i).ages(p).chem(k).name = con(j).name
                med(i).ages(p).chem(k).kind = con(j).kind
                If con(j).kind = 1 Then
                  med(i).ages(p).chem(k).haveCarc = 1
                Else
                  med(i).ages(p).chem(k).haveNonCarc = 1
                End If
                If med(i).ages(p).chem(k).numseries < med(i).ages(p).chem(k).numtime Then
                  med(i).ages(p).chem(k).numseries = med(i).ages(p).chem(k).numtime
                End If
                For l = 1 To con(j).numprog
                  For m = 1 To med(i).ages(p).chem(k).numprog
                    If con(j).prog(l).cas = med(i).ages(p).chem(k).prog(m).cas Then
                      med(i).ages(p).chem(k).prog(m).use = l
                      med(i).ages(p).chem(k).prog(m).name = con(j).prog(l).name
                      med(i).ages(p).chem(k).prog(m).kind = con(j).prog(l).kind
                      Exit For
                    End If
                  Next
                  If m > med(i).ages(p).chem(k).numprog Then
                    ReDim Preserve med(i).ages(p).chem(k).prog(m) As sProgeny
                    med(i).ages(p).chem(k).numprog = m
                    med(i).ages(p).chem(k).prog(m).use = l
                    med(i).ages(p).chem(k).prog(m).cas = con(j).prog(l).cas
                    med(i).ages(p).chem(k).prog(m).name = con(j).prog(l).name
                    med(i).ages(p).chem(k).prog(m).kind = con(j).prog(l).kind
                  End If
                Next
                Exit For
              End If
            Next
            If k > med(i).ages(p).numchem Then
              ReDim Preserve med(i).ages(p).chem(k) As sParent
              med(i).ages(p).numchem = k
              med(i).ages(p).chem(k).use = j
              med(i).ages(p).chem(k).cas = con(j).cas
              med(i).ages(p).chem(k).name = con(j).name
              med(i).ages(p).chem(k).kind = con(j).kind
              med(i).ages(p).chem(k).numprog = con(j).numprog
              med(i).ages(p).chem(k).tunit(USERUNIT) = YR
              med(i).ages(p).chem(k).tunit(INTLUNIT) = YR
              If con(j).kind = 1 Then
                med(i).ages(p).chem(k).haveCarc = 1
              Else
                med(i).ages(p).chem(k).haveNonCarc = 1
              End If
              If con(j).numprog > 0 Then
                ReDim Preserve med(i).ages(p).chem(k).prog(con(j).numprog) As sProgeny
                For l = 1 To con(j).numprog
                  med(i).ages(p).chem(k).prog(l).use = l
                  med(i).ages(p).chem(k).prog(l).cas = con(j).prog(l).cas
                  med(i).ages(p).chem(k).prog(l).name = con(j).prog(l).name
                  med(i).ages(p).chem(k).prog(l).kind = con(j).prog(l).kind
                Next
              End If
            End If
          Next
        Next
      End If
    Next
    Dim numused As Long, newchem() As sParent
    For i = 1 To MAXMEDIA
      If med(i).use = 1 Then
        For p = 1 To med(i).numages
          numused = 0
          For k = 1 To med(i).ages(p).numchem
          numused = numused + IIf(med(i).ages(p).chem(k).use > 0, 1, 0)
          Next k
          If numused < UBound(med(i).ages(p).chem()) Then
          ReDim newchem(numused)
          numused = 0
          For k = 1 To med(i).ages(p).numchem
            If med(i).ages(p).chem(k).use > 0 Then
            numused = numused + 1
            newchem(numused) = med(i).ages(p).chem(k)
            newchem(numused).use = 0
            End If
          Next k
          med(i).ages(p).chem = newchem
          med(i).ages(p).numchem = numused
          End If
        Next p
      End If
    Next i
    
    For i = 1 To MAXMEDIA
      If med(i).use = 1 Then
        For j = 0 To lstMedia.ListCount - 1
          If med(i).name = lstMedia.list(j) Then
            lstMedia.ItemData(j) = i
            lstMedia.Selected(j) = True
          End If
        Next
      End If
    Next
    
    SetFormat Me
  Else
    PutError "Can't find or open file " & FUIName
    EndModule
  End If
End Sub

Private Sub Form_load()
  Dim mode As Long
  Dim thismod As String
  Dim refname As String
  Dim cnt As Long
  Dim i As Long
  Dim lpos As Long
  
  lpos = tvwEPF.Width + 2 * tvwEPF.Left

  fraEPF.Move lpos, 0
  fraAgePanel.Move lpos, 0
  fraAgeGroup.Move lpos, 0
  fraExposure.Move lpos, 0
  fraMedia.Move lpos, 0
  fraChemical.Move lpos, 0
  fraConcentration.Move lpos, 0
  
  With tvwEPF
    Set mnode = .Nodes.Add()
    .LabelEdit = False
    .LineStyle = tvwRootLines
  End With
  With mnode ' Add first node.
    .Text = "RIF"
    .Tag = "RIF"
    .key = "RIF"
    .Image = CLOSED
    .Expanded = True
  End With
  StartModule frmRIF, "FRAMES Known Receptor Intakes Module", 6
  refname = Mid(FUIName, 1, Len(FUIName) - 4)
  SetRefFile ReplaceExt(FUIName, "ref")
  mode = Val(argv(0))
  thismod = "\RIF"
  SetHelpFile App.Path + "\Known RIF.html"
  Loading.Show
  
  med(1).name = AIR
  med(2).name = AQUIFER
  med(3).name = SURFACEWATER
  med(4).name = WETLANDS
  med(5).name = SOIL
  For i = 1 To MAXMEDIA
    med(i).lunits(USERUNIT) = KM
    med(i).lunits(INTLUNIT) = KM
  Next
  
  loadng = True
  loadprm
  
  'Possible pathways
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
  
  'soil,external
  ExpPathOK(3, 4).Add 19
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
  ExpPathOK(0, 4).Add 20
  'air,dermal
  ExpPathOK(0, 3).Add 19
  'air,inhalation
  ExpPathOK(0, 2).Add 19
  ExpPathOK(0, 2).Add 20
  ExpPathOK(0, 2).Add 21
  'air,ingestion
  ExpPathOK(0, 1).Add 0
  ExpPathOK(0, 1).Add 1
  ExpPathOK(0, 1).Add 2
  ExpPathOK(0, 1).Add 3
  ExpPathOK(0, 1).Add 4
  ExpPathOK(0, 1).Add 6
  ExpPathOK(0, 1).Add 7
  ExpPathOK(0, 1).Add 8
  ExpPathOK(0, 1).Add 9
  ExpPathOK(0, 1).Add 10
  
  For i = 0 To 4
    If i = 1 Then
      i = i
    End If
    get_conversion_items unit(i).Tag, unit(i)
  Next
  get_conversion_items unit(5).Tag, unit(5)
  
  If mode < 2 Then
    med(0).kind = 0
  Else
    med(0).kind = 1
  End If
  
  If mode Mod 2 = 0 Then
    Unload Loading
    Make_RIF
    EndModule
  End If
  LoadTreeViewFromRead
  Unload Loading
  RefItem = 1
  loadng = False
  
   tvwEPF.SelectedItem = tvwEPF.Nodes(1)
   tvwEPF_NodeClick tvwEPF.SelectedItem
End Sub

Private Sub FillRouteList(Media As Long)
  Dim i As Integer
  Dim j As Integer
  Dim idx As Integer
  For i = 1 To 4
    lstExposure(i).Clear
    For j = 1 To ExpPathOK(Media, i).Count
      idx = ExpPathOK(Media, i).item(j)
      lstExposure(i).AddItem paths(idx)
      If i = 4 Then
        lstExposure(i).ItemData(lstExposure(i).NewIndex) = 0
      Else
        Select Case idx
          Case 0 To 7, 9 To 13, 16, 17, 19
            lstExposure(i).ItemData(lstExposure(i).NewIndex) = 1
          Case 8, 14, 15, 18, 20, 21
            lstExposure(i).ItemData(lstExposure(i).NewIndex) = 2
        End Select
      End If
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
  Dim t As Long
  Dim medcount As Long
  Dim parentexpcount As Long
  Dim progexpcount As Long
  Dim fname As String
  Dim parm As parmrec
  Dim fle As parmfile
  Dim check As Boolean
  Dim missing As String
  Dim errmsg As String
  Dim tempname As String
  Dim medIdx As Integer
  
  Dim UserFormat As String
  UserFormat = CVTFormat
  
  prevtime = -1
  medIdx = 0
  fname = RunName & ".GID"
  If open_parm(fle, fname, 1) Then
  
    set_parm parm, "CVTFormat", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CVTFormat
    write_parmrec fle, parm
    CVTFormat = "General Number"
    set_parm parm, "progeny", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", prog.Checked
    write_parmrec fle, parm
  
    For i = 1 To MAXMEDIA
      If med(i).use Then
        If CheckMedium(med(i), missing) = False Then
          med(i).use = False
          errmsg = med(i).name & ": Missing " & missing
          PutError errmsg
        End If
        medIdx = GetMedArrayIndex(med(i).name)
        medcount = medcount + 1
        tempname = "name" + CStr(medIdx)
        set_parm parm, tempname, medcount, 0, 0, 0, 0, 0, med(i).ref, "N/A", "N/A", med(i).name
        write_parmrec fle, parm
        tempname = "kind" + CStr(medIdx)
        set_parm parm, tempname, medcount, 0, 0, 0, 0, 0, med(i).ref, "N/A", "N/A", CStr(med(0).kind)
        write_parmrec fle, parm
        For j = 1 To med(i).numloc
          tempname = "locx" + CStr(medIdx)
          set_parm parm, tempname, j, 0, 0, 0, 0, 0, med(i).ref, med(i).lunits(USERUNIT), med(i).lunits(INTLUNIT), CStr(convert(med(i).lunits(USERUNIT), med(i).lunits(INTLUNIT), med(i).locx(j)))
          write_parmrec fle, parm
          tempname = "locy" + CStr(medIdx)
          set_parm parm, tempname, j, 0, 0, 0, 0, 0, med(i).ref, med(i).lunits(USERUNIT), med(i).lunits(INTLUNIT), CStr(convert(med(i).lunits(USERUNIT), med(i).lunits(INTLUNIT), med(i).locy(j)))
          write_parmrec fle, parm
        Next
        For j = 1 To med(i).numages
          tempname = "begAge" + CStr(medIdx)
          set_parm parm, tempname, j, 0, 0, 0, 0, 0, med(i).ages(j).ref, med(i).ages(j).ageunit(USERUNIT), med(i).ages(j).ageunit(INTLUNIT), CStr(convert(med(i).ages(j).ageunit(USERUNIT), med(i).ages(j).ageunit(INTLUNIT), med(i).ages(j).begAge))
          write_parmrec fle, parm
          tempname = "endage" + CStr(medIdx)
          set_parm parm, tempname, j, 0, 0, 0, 0, 0, med(i).ages(j).ref, med(i).ages(j).ageunit(USERUNIT), med(i).ages(j).ageunit(INTLUNIT), CStr(convert(med(i).ages(j).ageunit(USERUNIT), med(i).ages(j).ageunit(INTLUNIT), med(i).ages(j).endAge))
          write_parmrec fle, parm
          tempname = "population" + CStr(medIdx)
          set_parm parm, tempname, j, k, 0, 0, 0, 0, med(i).ages(j).ref, "N/A", "N/A", CStr(med(i).ages(j).population)
          write_parmrec fle, parm
          tempname = "dur" + CStr(medIdx)
          set_parm parm, tempname, j, k, 0, 0, 0, 0, med(i).ages(j).ref, med(i).ages(j).dunit(USERUNIT), med(i).ages(j).dunit(INTLUNIT), convert(med(i).ages(j).dunit(USERUNIT), med(i).ages(j).dunit(INTLUNIT), CStr(med(i).ages(j).duration))
          write_parmrec fle, parm
          For k = 1 To med(i).ages(j).numchem
            tempname = "chemname" + CStr(medIdx)
            set_parm parm, tempname, j, k, 0, 0, 0, 0, med(i).ages(j).chem(k).ref, "N/A", "N/A", med(i).ages(j).chem(k).name
            write_parmrec fle, parm
            tempname = "casid" + CStr(medIdx)
            set_parm parm, tempname, j, k, 0, 0, 0, 0, med(i).ages(j).chem(k).ref, "N/A", "N/A", med(i).ages(j).chem(k).cas
            write_sparmrec fle, parm
            For m = 1 To med(i).ages(j).chem(k).numtime
              tempname = "time" + CStr(medIdx)
              set_parm parm, tempname, j, k, 0, m, 0, 0, med(i).ages(j).chem(k).ref, med(i).ages(j).chem(k).tunit(USERUNIT), med(i).ages(j).chem(k).tunit(INTLUNIT), convert(med(i).ages(j).chem(k).tunit(USERUNIT), med(i).ages(j).chem(k).tunit(INTLUNIT), CStr(med(i).ages(j).chem(k).time(m)))
              write_parmrec fle, parm
            Next
            For m = 1 To med(i).ages(j).chem(k).numexp
              tempname = "exptype" + CStr(medIdx)
              set_parm parm, tempname, j, k, 0, m, 0, 0, med(i).ages(j).chem(k).exp(m).ref, med(i).ages(j).chem(k).exp(m).cunit(USERUNIT), med(i).ages(j).chem(k).exp(m).cunit(INTLUNIT), med(i).ages(j).chem(k).exp(m).xType
              write_parmrec fle, parm
              
              tempname = "efxtype" + CStr(medIdx)
              set_parm parm, tempname, j, k, 0, m, 0, 0, med(i).ages(j).chem(k).exp(m).ref, med(i).ages(j).chem(k).exp(m).cunit(USERUNIT), med(i).ages(j).chem(k).exp(m).cunit(INTLUNIT), med(i).ages(j).chem(k).exp(m).eType
              write_parmrec fle, parm
              
              tempname = "route" + CStr(medIdx)
              set_parm parm, tempname, j, k, 0, m, 0, 0, med(i).ages(j).chem(k).exp(m).ref, "N/A", "N/A", med(i).ages(j).chem(k).exp(m).Route
              write_parmrec fle, parm
              tempname = "path" + CStr(medIdx)
              set_parm parm, tempname, j, k, 0, m, 0, 0, med(i).ages(j).chem(k).exp(m).ref, "N/A", "N/A", med(i).ages(j).chem(k).exp(m).Path
              write_parmrec fle, parm
              Check_NTS med(i).ages(j).chem(k).exp(m).numseries, med(i).ages(j).chem(k).name + " number of times series is less than 2"
              For n = 1 To med(i).ages(j).chem(k).exp(m).numseries
                For p = 1 To med(i).ages(j).chem(k).exp(m).series(n).num
                  tempname = "conc" + CStr(medIdx)
                  set_parm parm, tempname, j, k, 0, m, n, p, med(i).ages(j).chem(k).exp(m).ref, med(i).ages(j).chem(k).exp(m).cunit(USERUNIT), med(i).ages(j).chem(k).exp(m).cunit(INTLUNIT), CStr(med(i).ages(j).chem(k).exp(m).series(n).valu(p))
                  write_parmrec fle, parm
                Next
              Next
            Next
            If prog.Checked Then
              For t = 1 To med(i).ages(j).chem(k).numprog
                tempname = "chemname" + CStr(medIdx)
                set_parm parm, tempname, j, k, t, 0, 0, 0, med(i).ages(j).chem(k).prog(t).ref, "N/A", "N/A", med(i).ages(j).chem(k).prog(t).name
                write_parmrec fle, parm
                tempname = "casid" + CStr(medIdx)
                set_parm parm, tempname, j, k, t, 0, 0, 0, med(i).ages(j).chem(k).prog(t).ref, "N/A", "N/A", med(i).ages(j).chem(k).prog(t).cas
                write_sparmrec fle, parm
                For m = 1 To med(i).ages(j).chem(k).prog(t).numexp
                  tempname = "exptype" + CStr(medIdx)
                  set_parm parm, tempname, j, k, t, m, 0, 0, med(i).ages(j).chem(k).prog(t).exp(m).ref, med(i).ages(j).chem(k).prog(t).exp(m).cunit(USERUNIT), med(i).ages(j).chem(k).prog(t).exp(m).cunit(INTLUNIT), med(i).ages(j).chem(k).prog(t).exp(m).xType
                  write_parmrec fle, parm
                  
                  tempname = "efxtype" + CStr(medIdx)
                  set_parm parm, tempname, j, k, t, m, 0, 0, med(i).ages(j).chem(k).prog(t).exp(m).ref, med(i).ages(j).chem(k).prog(t).exp(m).cunit(USERUNIT), med(i).ages(j).chem(k).prog(t).exp(m).cunit(INTLUNIT), med(i).ages(j).chem(k).prog(t).exp(m).eType
                  write_parmrec fle, parm
                  
                  tempname = "route" + CStr(medIdx)
                  set_parm parm, tempname, j, k, t, m, 0, 0, med(i).ages(j).chem(k).prog(t).exp(m).ref, "N/A", "N/A", med(i).ages(j).chem(k).prog(t).exp(m).Route
                  write_parmrec fle, parm
                  tempname = "path" + CStr(medIdx)
                  set_parm parm, tempname, j, k, t, m, 0, 0, med(i).ages(j).chem(k).prog(t).exp(m).ref, "N/A", "N/A", med(i).ages(j).chem(k).prog(t).exp(m).Path
                  write_parmrec fle, parm
                  Check_NTS med(i).ages(j).chem(k).prog(t).exp(m).numseries, med(i).ages(j).chem(k).name + " number of times series is less than 2"
                  For n = 1 To med(i).ages(j).chem(k).prog(t).exp(m).numseries
                    For p = 1 To med(i).ages(j).chem(k).prog(t).exp(m).series(n).num
                      tempname = "conc" + CStr(medIdx)
                      set_parm parm, tempname, j, k, t, m, n, p, med(i).ages(j).chem(k).prog(t).exp(m).ref, med(i).ages(j).chem(k).prog(t).exp(m).cunit(USERUNIT), med(i).ages(j).chem(k).prog(t).exp(m).cunit(INTLUNIT), CStr(med(i).ages(j).chem(k).prog(t).exp(m).series(n).valu(p))
                      write_parmrec fle, parm
                    Next
                  Next
                Next
              Next
            End If
          Next
        Next
      End If
    Next
    close_parm fle
    If medcount = 0 Then
      PutError "There are no medium types defined."
    End If
  Else
    PutError "Unable to create transaction file" & RunName & ".GID"
  End If
  'Make_RIF
  EndModule
End Sub

Private Function GetNumExp&(i&, m&, j&, k&)
  Dim n As Long
  GetNumExp = 0
  If k = 0 Then
    If med(i).ages(m).chem(j).numexp = 0 Then Exit Function
    For n = 1 To med(i).ages(m).chem(j).numexp
      If med(i).ages(m).chem(j).exp(n).use = 1 Then
        GetNumExp = GetNumExp + 1
      End If
    Next
  Else
    If med(i).ages(m).chem(j).prog(k).numexp = 0 Then Exit Function
    For n = 1 To med(i).ages(m).chem(j).prog(k).numexp
      If med(i).ages(m).chem(j).prog(k).exp(n).use = 1 Then
        GetNumExp = GetNumExp + 1
      End If
    Next
  End If
End Function

Function GetType(unit As String, eType As String) As String
  Select Case unit
    Case MGKGDAY, MGM3:      GetType = eType
    Case BQKG, BQM3, BQL:    GetType = CONC
    Case BQ:                 GetType = INTAKE
    Case SV:                 GetType = DOSE
  End Select
End Function

Private Function Make_RIF()
  Dim i As Long, j As Long, k As Long
  Dim l As Long, m As Long, n As Long
  Dim q As Long, r As Long, p As Long
  Dim t As Long
  Dim msg As String
  Dim cnt As Long
  Dim file As csv
  
  If open_csv(file, RunName & ".rif", 1) Then
    PutHeader file
    cnt = 0
    For i = 1 To numrif
      If med(i).use Then cnt = cnt + 1
    Next
    put_val file, cnt
    put_line file
    For i = 1 To MAXMEDIA
      With med(i)
        If .use Then
          If .kind = 1 Then
            put_val file, "acute"
          Else
            put_val file, "chronic"
          End If
          put_val file, ModName
          put_val file, .name
          put_val file, .numloc
          put_val file, .numages
          put_val file, numcon            ' GetNumCon(i)
          put_line file
          For j = 1 To .numloc
            put_val file, convert(.lunits(USERUNIT), .lunits(INTLUNIT), .locx(j))
            put_val file, .lunits(INTLUNIT)
            put_val file, convert(.lunits(USERUNIT), .lunits(INTLUNIT), .locy(j))
            put_val file, .lunits(INTLUNIT)
            put_line file
          Next
          For t = 1 To numages
            put_val file, convert(.ages(t).ageunit(USERUNIT), .ages(t).ageunit(INTLUNIT), .ages(t).begAge)
            put_val file, convert(.ages(t).ageunit(USERUNIT), .ages(t).ageunit(INTLUNIT), .ages(t).endAge)
            put_val file, .ages(t).ageunit(INTLUNIT)
            put_line file
            For j = 1 To numcon
              For k = 1 To .ages(t).numchem
                If .ages(t).chem(k).cas = con(j).cas Then    ' And .chem(k).use > 0
                  put_val file, .ages(t).chem(k).name
                  put_sval file, .ages(t).chem(k).cas
                  If prog.Checked Then
                    put_val file, .ages(t).chem(k).numprog
                  Else
                    put_val file, 0
                  End If
                  put_val file, .ages(t).chem(k).numseries
                  put_line file
                  For l = 1 To .ages(t).chem(k).numseries
                    put_val file, convert(.ages(t).chem(k).tunit(USERUNIT), .ages(t).chem(k).tunit(INTLUNIT), .ages(t).chem(k).time(l))
                    put_val file, .ages(t).chem(k).tunit(INTLUNIT)
                    put_val file, convert(.ages(t).dunit(USERUNIT), .ages(t).dunit(INTLUNIT), .ages(t).duration)
                    put_val file, .ages(t).dunit(INTLUNIT)
                    put_val file, GetNumExp(i, t, k, 0)
                    put_line file
                    For m = 1 To .ages(t).chem(k).numexp
                      If .ages(t).chem(k).exp(m).use > 0 Then
                        put_val file, .ages(t).population
                        put_val file, .ages(t).chem(k).exp(m).Path
                        put_val file, .ages(t).chem(k).exp(m).Route
                        put_val file, .ages(t).chem(k).exp(m).cunit(INTLUNIT)
                        put_val file, GetType(.ages(t).chem(k).exp(m).cunit(INTLUNIT), .ages(t).chem(k).exp(m).eType)
                        put_line file
                        For n = 1 To .numloc
                          put_val file, convert(.ages(t).chem(k).exp(m).cunit(USERUNIT), .ages(t).chem(k).exp(m).cunit(INTLUNIT), .ages(t).chem(k).exp(m).series(l).valu(n))
                        Next
                        put_line file
                      End If
                    Next
                    If prog.Checked Then
                      For q = 1 To con(j).numprog
                        For m = 1 To .ages(t).chem(k).numprog
                          If .ages(t).chem(k).prog(m).cas = con(j).prog(q).cas Then    ' And .chem(k).prog(m).use > 0
                            put_val file, .ages(t).chem(k).prog(m).name
                            put_sval file, .ages(t).chem(k).prog(m).cas
                            put_val file, GetNumExp(i, t, k, m)
                            put_line file
                            For n = 1 To med(i).ages(t).chem(k).prog(m).numexp
                              If med(i).ages(t).chem(k).prog(m).exp(n).use > 0 Then
                                put_val file, .ages(t).population
                                put_val file, .ages(t).chem(k).prog(m).exp(n).Path
                                put_val file, .ages(t).chem(k).prog(m).exp(n).Route
                                put_val file, .ages(t).chem(k).prog(m).exp(n).cunit(INTLUNIT)
                                put_val file, GetType(.ages(t).chem(k).prog(m).exp(n).cunit(INTLUNIT), .ages(t).chem(k).prog(m).exp(n).eType)
                                put_line file
                                For p = 1 To .numloc
                                  put_val file, convert(.ages(t).chem(k).prog(m).exp(n).cunit(USERUNIT), .ages(t).chem(k).prog(m).exp(n).cunit(INTLUNIT), .ages(t).chem(k).prog(m).exp(n).series(l).valu(p))
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
           Next
        End If
      End With
    Next
    close_csv file
  Else
    PutError "Unable to create water valurec file" & RunName & ".WCF"
  End If
End Function

Private Sub tvwEPF_BeforeLabelEdit(Cancel As Integer)
  Cancel = True ' Cancel the operation
End Sub

Private Sub tvwEPF_Collapse(ByVal Node As ComctlLib.Node)
  Node.Image = CLOSED
End Sub

Private Sub tvwEPF_Expand(ByVal Node As ComctlLib.Node)
  Node.Image = "open"
End Sub

Public Function GetMedArrayIndex(parmname As String) As Integer
  Select Case parmname
    Case AIR:             GetMedArrayIndex = 1
    Case AQUIFER:         GetMedArrayIndex = 2
    Case SURFACEWATER:    GetMedArrayIndex = 3
    Case WETLANDS:        GetMedArrayIndex = 4
    Case SOIL:            GetMedArrayIndex = 5
    Case Else:            GetMedArrayIndex = 0
  End Select
End Function

Public Sub GetAges(agestring As String, begAge As String, endAge As String)
  Dim dashindex As Integer
  Dim scsv As Variant
  
  scsv = Split(agestring, ",")
  dashindex = InStr(1, scsv(3), "-", vbTextCompare)
  begAge = Left(scsv(3), dashindex - 1)
  endAge = Mid(scsv(3), dashindex + 1)
End Sub

Public Function GetAgeIndex(medIdx As Integer, begAge As Double, endAge As Double) As Integer
  Dim i As Integer
  
  GetAgeIndex = 0
  For i = 1 To med(medIdx).numages
    If (med(medIdx).ages(i).begAge = begAge) And (med(medIdx).ages(i).endAge = endAge) Then
      GetAgeIndex = i
      Exit Function
    End If
  Next
End Function

Public Function GetChemIndex(medIdx As Integer, ageIdx As Integer, name As String) As Integer
  Dim i As Integer
  
  GetChemIndex = 0
  For i = 1 To med(medIdx).ages(ageIdx).numchem
    If med(medIdx).ages(ageIdx).chem(i).name = name Then
      GetChemIndex = i
      Exit Function
    End If
  Next
End Function

Public Function GetProgenyIndex(medIdx As Integer, ageIdx As Integer, chmIdx As Integer, name As String) As Integer
  Dim i As Integer
  
  GetProgenyIndex = 0
  For i = 1 To med(medIdx).ages(ageIdx).chem(chmIdx).numprog
    If med(medIdx).ages(ageIdx).chem(chmIdx).prog(i).name = name Then
      GetProgenyIndex = i
      Exit Function
    End If
  Next
End Function

Public Function GetExposureIndex(medIdx As Integer, ageIdx As Integer, chmIdx As Integer, prgIdx As Integer, Path As String, Route As String, xType As String, eType As String)
  Dim i As Integer
  Dim j As Integer
  
  GetExposureIndex = 0
  If prgIdx = 0 Then
    For i = 1 To med(medIdx).ages(ageIdx).chem(chmIdx).numexp
      If (med(medIdx).ages(ageIdx).chem(chmIdx).exp(i).Path = Path) And _
         (med(medIdx).ages(ageIdx).chem(chmIdx).exp(i).Route = Route) And _
         (med(medIdx).ages(ageIdx).chem(chmIdx).exp(i).xType = xType) And _
         (med(medIdx).ages(ageIdx).chem(chmIdx).exp(i).eType = eType) Then
        GetExposureIndex = i
        Exit Function
      End If
    Next
  Else
    For i = 1 To med(medIdx).ages(ageIdx).chem(chmIdx).prog(prgIdx).numexp
      If (med(medIdx).ages(ageIdx).chem(chmIdx).prog(prgIdx).exp(i).Path = Path) And _
         (med(medIdx).ages(ageIdx).chem(chmIdx).prog(prgIdx).exp(i).Route = Route) And _
         (med(medIdx).ages(ageIdx).chem(chmIdx).prog(prgIdx).exp(i).xType = xType) And _
         (med(medIdx).ages(ageIdx).chem(chmIdx).prog(prgIdx).exp(i).eType = eType) Then
        GetExposureIndex = i
        Exit Function
      End If
    Next
  End If
End Function

Public Sub LoadTreeViewFromRead()
  Dim i As Integer
  Dim j As Integer
  Dim k As Integer
  Dim m As Integer
  Dim n As Integer
  Dim keystring1 As String
  Dim keystring2 As String
  Dim medkey As String
  Dim groupkey As String
  Dim agekey As String
  Dim chemkey As String
  Dim parentkey As String
  Dim progkey As String
  Dim expkey As String
  Dim agenodename As String
  Dim begAge As String
  Dim endAge As String
  Dim expstring As String
  
  tvwEPF.Nodes.Clear
  With tvwEPF
    Set mnode = .Nodes.Add()
    .LabelEdit = False
    .LineStyle = tvwRootLines
  End With
  With mnode ' Add first node.
    .Text = "RIF"
    .Tag = "RIF"
    .key = "RIF"
    .Image = CLOSED
    .Expanded = True
  End With
  For i = 1 To MAXMEDIA
    If med(i).use = 1 Then
      medkey = "RIF," + med(i).name
      Set mnode = tvwEPF.Nodes.Add("RIF", tvwChild, medkey, med(i).name, CLOSED, CLOSED)
      mnode.Tag = "Media"
      mnode.EnsureVisible
      groupkey = medkey + ",Age Groups"
      Set mnode = tvwEPF.Nodes.Add(medkey, tvwChild, groupkey, "Age Groups", CLOSED, CLOSED)
      mnode.Tag = "Ages"
      mnode.EnsureVisible
      keystring1 = keystring2
      For j = 1 To med(i).numages
        begAge = CStr(med(i).ages(j).begAge)
        endAge = CStr(med(i).ages(j).endAge)
        agekey = groupkey + "," + begAge + "-" + endAge
        agenodename = "Age Group " + Format(begAge, CVTFormat) + "-" + Format(endAge, CVTFormat)
        Set mnode = tvwEPF.Nodes.Add(groupkey, tvwChild, agekey, agenodename, CLOSED, CLOSED)
        mnode.Tag = "AgeGroup"
        mnode.EnsureVisible
        For k = 1 To med(i).ages(j).numchem
          chemkey = agekey + "," + med(i).ages(j).chem(k).name
          Set mnode = tvwEPF.Nodes.Add(agekey, tvwChild, chemkey, med(i).ages(j).chem(k).name, CLOSED, CLOSED)
          mnode.Tag = "Chemical"
          mnode.EnsureVisible
          parentkey = chemkey + ",Parent"
          Set mnode = tvwEPF.Nodes.Add(chemkey, tvwChild, parentkey, CONSTITUENT, CLOSED, CLOSED)
          mnode.Tag = CONSTITUENT
          mnode.EnsureVisible
          For m = 1 To med(i).ages(j).chem(k).numexp
            expstring = med(i).ages(j).chem(k).exp(m).Route + " " + med(i).ages(j).chem(k).exp(m).xType + " for " + med(i).ages(j).chem(k).exp(m).Path
            If med(i).ages(j).chem(k).kind <> 1 Then expstring = expstring + " (" + med(i).ages(j).chem(k).exp(m).eType + ")"
            expkey = parentkey + "," + expstring
            Set mnode = tvwEPF.Nodes.Add(parentkey, tvwChild, expkey, expstring, CLOSED, CLOSED)
            mnode.Tag = med(i).ages(j).chem(k).exp(m).Route
            mnode.EnsureVisible
          Next
          If prog.Checked Then
            For m = 1 To med(i).ages(j).chem(k).numprog
              progkey = chemkey + "," + med(i).ages(j).chem(k).prog(m).name
              Set mnode = tvwEPF.Nodes.Add(chemkey, tvwChild, progkey, med(i).ages(j).chem(k).prog(m).name, CLOSED, CLOSED)
              mnode.Tag = "Progeny"
              mnode.EnsureVisible
              For n = 1 To med(i).ages(j).chem(k).prog(m).numexp
                expstring = med(i).ages(j).chem(k).prog(m).exp(m).Route + " " + med(i).ages(j).chem(k).prog(m).exp(m).xType + " for " + med(i).ages(j).chem(k).prog(m).exp(m).Path
                If med(i).ages(j).chem(k).kind <> 1 Then expstring = expstring + " (" + med(i).ages(j).chem(k).prog(m).exp(m).eType + ")"
                expkey = chemkey + "," + expstring
                Set mnode = tvwEPF.Nodes.Add(progkey, tvwChild, expkey, expstring, CLOSED, CLOSED)
                mnode.Tag = med(i).ages(j).chem(k).prog(m).exp(n).Route
                mnode.EnsureVisible
              Next
            Next
          End If
        Next
      Next
    End If
  Next
End Sub

Public Sub SetExposureList(Index As Integer, Path As String)
  Dim j As Integer
  Dim found As Boolean

  found = False
  For j = 1 To lstExposure(Index).ListCount
    If Path = lstExposure(Index).list(j - 1) Then
      found = True
      lstExposure(Index).Selected(j - 1) = True
    End If
  Next
  If found = False Then
    lstExposure(Index).AddItem Path
    lstExposure(Index).Selected(lstExposure(Index).NewIndex) = True
  End If
End Sub

Public Sub SetRouteList()
  Dim i As Integer
  Dim Pathway As String
  Dim RouteIdx As Integer
  
  If prgIdx = 0 Then
    For i = 1 To med(medIdx).ages(ageIdx).chem(chmIdx).numexp
      Pathway = med(medIdx).ages(ageIdx).chem(chmIdx).exp(i).Path
      RouteIdx = GetRouteIdx(med(medIdx).ages(ageIdx).chem(chmIdx).exp(i).Route)
      SetExposureList RouteIdx, Pathway
    Next
  Else
    For i = 1 To med(medIdx).ages(ageIdx).chem(chmIdx).prog(prgIdx).numexp
      Pathway = med(medIdx).ages(ageIdx).chem(chmIdx).prog(prgIdx).exp(i).Path
      RouteIdx = GetRouteIdx(med(medIdx).ages(ageIdx).chem(chmIdx).prog(prgIdx).exp(i).Route)
      SetExposureList RouteIdx, Pathway
    Next
  End If
End Sub

Private Sub tvwEPF_NodeClick(ByVal Node As ComctlLib.Node)
  Dim i As Long
  Dim j As Long
  Dim index1 As Integer
  Dim index2 As Integer
  Dim index3 As Integer
  Dim childnode As ComctlLib.Node
  Dim numcols As Integer
  Dim numrows As Integer
  Dim agenodename As String
  
  treeclick = True
  noact.Enabled = False
  fraEPF.Visible = False
  fraMedia.Visible = False
  fraChemical.Visible = False
  fraConcentration.Visible = False
  fraExposure.Visible = False
  fraAgeGroup.Visible = False
  fraAgePanel.Visible = False
    
  'this event sets these global variables used by other methods
  medIdx = 0
  ageIdx = 0
  begAge = 0
  endAge = 0
  chmIdx = 0
  prgIdx = 0
  expIdx = 0
  
  ' case statement goes in order of outline hierarchy
  Select Case Node.Tag
    Case "RIF"
      RefItem = -1
      HelpAnchor = "known"
      fraEPF.Caption = tvwEPF.SelectedItem.Text
      fraEPF.Visible = True
    
    Case "Media"
      RefItem = 1
      noact.Enabled = True
      HelpAnchor = "medium"
      fraMedia.Caption = tvwEPF.SelectedItem.Text
      fraMedia.Visible = True
      
      medIdx = GetMedArrayIndex(tvwEPF.SelectedItem.Text)
      optDataSet(med(medIdx).kind).Value = True
      set_unit unit(4), med(medIdx).lunits(USERUNIT)
      vaSpread1.Row = -1
      vaSpread1.col = -1
      vaSpread1.BlockMode = True:
      vaSpread1.Action = 12
      vaSpread1.BlockMode = False
      For i = 1 To med(medIdx).numloc
        vaSpread1.Row = i
        vaSpread1.col = 1
        vaSpread1.Text = Format(med(medIdx).locx(i), CVTFormat)
        vaSpread1.col = 2
        vaSpread1.Text = Format(med(medIdx).locy(i), CVTFormat)
      Next
      ref(1).Caption = "Ref: " + CStr(med(medIdx).ref)
 
    Case "Ages"
      RefItem = 4
      noact.Enabled = True
      HelpAnchor = "age"
      fraAgePanel.Caption = tvwEPF.SelectedItem.Text
      fraAgePanel.Visible = True
      
      lstAges.Clear
      set_unit unit(5), YR
      medIdx = GetMedArrayIndex(tvwEPF.SelectedItem.parent.Text)
      
      If Not Node.Children = 0 Then
        Set childnode = tvwEPF.SelectedItem.Child
        GetAges childnode.key, begAge, endAge
        lstAges.AddItem Format(begAge, CVTFormat) + "-" + Format(endAge, CVTFormat)
        For i = 2 To Node.Children
          Set childnode = childnode.Next
          GetAges childnode.key, begAge, endAge
          lstAges.AddItem Format(begAge, CVTFormat) + "-" + Format(endAge, CVTFormat)
        Next
        'all ages will have the same unit
        Label3.Caption = lstAges.ListCount
        If med(medIdx).numages > 0 Then
          ref(4).Caption = "Ref: " + CStr(med(medIdx).ages(1).ref)
          set_unit unit(5), med(medIdx).ages(1).ageunit(USERUNIT)
        End If
      End If

    Case "AgeGroup"
      RefItem = -1
      HelpAnchor = ""
      fraAgeGroup.Caption = tvwEPF.SelectedItem.Text
      fraAgeGroup.Visible = True
      
      medIdx = GetMedArrayIndex(tvwEPF.SelectedItem.parent.parent.Text)
      GetAges tvwEPF.SelectedItem.key, begAge, endAge
      ageIdx = GetAgeIndex(medIdx, CDbl(begAge), CDbl(endAge))
      ExpPop.Text = CStr(med(medIdx).ages(ageIdx).population)
      txt(3).Text = CStr(med(medIdx).ages(ageIdx).duration)
      set_unit unit(3), med(medIdx).ages(ageIdx).dunit(USERUNIT)
      ref(3).Caption = "Ref: " + CStr(med(medIdx).ages(ageIdx).ref)

    Case "Chemical"
      RefItem = 2
      noact.Enabled = True
      HelpAnchor = "time"
      fraChemical.Caption = tvwEPF.SelectedItem.Text
      fraChemical.Visible = True
      
      set_unit unit(2), YR ' default time unit
      set_unit unit(3), YR ' default duration unit
      medIdx = GetMedArrayIndex(tvwEPF.SelectedItem.parent.parent.parent.Text)
      GetAges tvwEPF.SelectedItem.parent.key, begAge, endAge
      ageIdx = GetAgeIndex(medIdx, CDbl(begAge), CDbl(endAge))
      chmIdx = GetChemIndex(medIdx, ageIdx, tvwEPF.SelectedItem.Text)
      
      If med(medIdx).ages(ageIdx).chem(chmIdx).numtime = 0 Then
        vaSpread2.Row = -1
        vaSpread2.col = -1
        vaSpread2.Action = 12
      End If
      
      If con(chmIdx).kind <> 1 Then
        vaSpread2.VisibleRows = 10
      Else
        vaSpread2.VisibleRows = 15
      End If
      
      set_unit unit(2), med(medIdx).ages(ageIdx).chem(chmIdx).tunit(USERUNIT)
      For i = 1 To med(medIdx).ages(ageIdx).chem(chmIdx).numtime
        vaSpread2.Row = i
        vaSpread2.col = 1
        vaSpread2.Text = Format(med(medIdx).ages(ageIdx).chem(chmIdx).time(i), CVTFormat)
      Next
      
      Label8.Visible = (med(medIdx).ages(ageIdx).chem(chmIdx).kind <> 1)
      chkCarc.Visible = (med(medIdx).ages(ageIdx).chem(chmIdx).kind <> 1)
      chkNonCarc.Visible = (med(medIdx).ages(ageIdx).chem(chmIdx).kind <> 1)
      chkCarc.Value = med(medIdx).ages(ageIdx).chem(chmIdx).haveCarc
      chkNonCarc.Value = med(medIdx).ages(ageIdx).chem(chmIdx).haveNonCarc
      
      ref(2).Caption = "Ref: " + CStr(med(medIdx).ages(ageIdx).chem(chmIdx).ref)
      
    Case CONSTITUENT, "Progeny"
      RefItem = -1
      HelpAnchor = "pathway"
      fraExposure.Caption = tvwEPF.SelectedItem.Text
      fraExposure.Visible = True
      
      medIdx = GetMedArrayIndex(tvwEPF.SelectedItem.parent.parent.parent.parent.Text)
      GetAges tvwEPF.SelectedItem.parent.parent.key, begAge, endAge
      ageIdx = GetAgeIndex(medIdx, CDbl(begAge), CDbl(endAge))
      chmIdx = GetChemIndex(medIdx, ageIdx, tvwEPF.SelectedItem.parent.Text)
      If tvwEPF.SelectedItem.Tag = "Progeny" Then prgIdx = GetProgenyIndex(medIdx, ageIdx, chmIdx, tvwEPF.SelectedItem.Text)
      
      
      'enable media exposure for external routes depending on kind of chemical (rad = 1, non-rad = ?)
      lstExposure(4).Visible = (med(medIdx).ages(ageIdx).chem(chmIdx).kind = 1)
      lstExposure(4).Enabled = (med(medIdx).ages(ageIdx).chem(chmIdx).kind = 1)
      cmdRoute(4).Visible = (med(medIdx).ages(ageIdx).chem(chmIdx).kind = 1)
      cmdRoute(4).Enabled = (med(medIdx).ages(ageIdx).chem(chmIdx).kind = 1)
      cmbExt.Visible = (med(medIdx).ages(ageIdx).chem(chmIdx).kind = 1)
      cmbExt.Enabled = (med(medIdx).ages(ageIdx).chem(chmIdx).kind = 1)
      cmbInh.Enabled = (med(medIdx).ages(ageIdx).chem(chmIdx).kind <> 1)
      If cmbExt.ListIndex = -1 Then cmbExt.ListIndex = 0
      If cmbExt.Visible Or cmbInh.ListIndex = -1 Then cmbInh.ListIndex = 0
      
      'fill route lists with appropriate pathways
      Select Case med(medIdx).name
        Case AIR:
          FillRouteList 0
        Case AQUIFER:
          FillRouteList 1
          'disable aquifer exposure for external routes
          lstExposure(4).Enabled = False
          cmdRoute(4).Enabled = False
        Case SURFACEWATER:
          FillRouteList 2
        Case WETLANDS:
          FillRouteList 2
        Case SOIL:
          FillRouteList 3
      End Select
      SetRouteList
      
    Case INGESTION, INHALATION, DERMAL, EXTERNAL
      RefItem = 3
      noact.Enabled = True
      HelpAnchor = "concentrations"
      fraConcentration.Caption = tvwEPF.SelectedItem.Text
      fraConcentration.Visible = True
      
      medIdx = GetMedArrayIndex(tvwEPF.SelectedItem.parent.parent.parent.parent.parent.Text)
      GetAges tvwEPF.SelectedItem.parent.parent.parent.key, begAge, endAge
      ageIdx = GetAgeIndex(medIdx, CDbl(begAge), CDbl(endAge))
      chmIdx = GetChemIndex(medIdx, ageIdx, tvwEPF.SelectedItem.parent.parent.Text)
      
      Route = Node.Tag
      index1 = InStr(1, Node.Text, " ")
      index2 = InStr(1, Node.Text, " for ")
      xType = Mid(Node.Text, index1 + 1, index2 - index1 - 1)
      index3 = InStr(1, Node.Text, "(")
      If index3 = 0 Then
        Path = Mid(Node.Text, index2 + 5, Len(Node.Text) - index2 - 4)
        eType = ""
      Else
        Path = Mid(Node.Text, index2 + 5, index3 - index2 - 6)
        index1 = InStr(1, Node.Text, ")")
        eType = Mid(Node.Text, index3 + 1, index1 - index3 - 1)
      End If
        
      'clear sheet and add labels
      vaSpread3.col = -1
      vaSpread3.Row = -1
      vaSpread3.Action = 12
      vaSpread3.MaxCols = med(medIdx).numloc + 1
      vaSpread3.MaxRows = med(medIdx).ages(ageIdx).chem(chmIdx).numtime
      If vaSpread3.MaxRows > 11 Then
        vaSpread3.VisibleRows = 11
      Else
        vaSpread3.VisibleRows = vaSpread3.MaxRows
      End If
      vaSpread3.Row = 0
      For i = 1 To vaSpread3.MaxCols - 1
        vaSpread3.col = i + 1
        vaSpread3.Text = "Conc at loc " + CStr(i)
      Next
      vaSpread3.col = 1
      For i = 1 To vaSpread3.MaxRows
        vaSpread3.Row = i
        vaSpread3.Text = Format(med(medIdx).ages(ageIdx).chem(chmIdx).time(i), CVTFormat)
      Next
      unit(1).Clear
      set_unit unit(0), med(medIdx).ages(ageIdx).chem(chmIdx).tunit(USERUNIT)
      
      If tvwEPF.SelectedItem.parent.Text = CONSTITUENT Then
        expIdx = GetExposureIndex(medIdx, ageIdx, chmIdx, 0, Path, Route, xType, eType)
        get_conversion_items med(medIdx).ages(ageIdx).chem(chmIdx).exp(expIdx).cunit(USERUNIT), unit(1)
        set_unit unit(1), med(medIdx).ages(ageIdx).chem(chmIdx).exp(expIdx).cunit(USERUNIT)
        ref(3).Caption = "Ref: " + CStr(med(medIdx).ages(ageIdx).chem(chmIdx).exp(expIdx).ref)
        For i = 1 To med(medIdx).ages(ageIdx).chem(chmIdx).exp(expIdx).numseries
          vaSpread3.Row = i
          For j = 1 To med(medIdx).ages(ageIdx).chem(chmIdx).exp(expIdx).series(i).num
            vaSpread3.col = j + 1
            vaSpread3.Text = Format(med(medIdx).ages(ageIdx).chem(chmIdx).exp(expIdx).series(i).valu(j), CVTFormat)
          Next
        Next
      Else
        prgIdx = GetProgenyIndex(medIdx, ageIdx, chmIdx, tvwEPF.SelectedItem.parent.Text)
        expIdx = GetExposureIndex(medIdx, ageIdx, chmIdx, prgIdx, Path, Route, xType, eType)
        get_conversion_items med(medIdx).ages(ageIdx).chem(chmIdx).prog(prgIdx).exp(expIdx).cunit(USERUNIT), unit(1)
        set_unit unit(1), med(medIdx).ages(ageIdx).chem(chmIdx).prog(prgIdx).exp(expIdx).cunit(USERUNIT)
        ref(3).Caption = "Ref: " + CStr(med(medIdx).ages(ageIdx).chem(chmIdx).prog(prgIdx).exp(expIdx).ref)
        For i = 1 To med(medIdx).ages(ageIdx).chem(chmIdx).prog(prgIdx).exp(expIdx).numseries
          vaSpread3.Row = i
          For j = 1 To med(medIdx).ages(ageIdx).chem(chmIdx).prog(prgIdx).exp(expIdx).series(i).num
            vaSpread3.col = j + 1
            vaSpread3.Text = Format(med(medIdx).ages(ageIdx).chem(chmIdx).prog(prgIdx).exp(expIdx).series(i).valu(j), CVTFormat)
          Next
        Next
      End If
  End Select
  treeclick = False
End Sub

Private Sub Txt_Change(Index As Integer)
  Dim i As Integer
  Dim j As Integer
  Dim tempdbl As Double
  
  If treeclick Then Exit Sub
On Error GoTo DBLERROR
  tempdbl = CDbl(txt(Index).Text)
  For j = 1 To MAXMEDIA
    For i = 1 To med(j).numages
      If med(j).ages(i).begAge = begAge And med(j).ages(i).endAge = endAge Then
        med(j).ages(i).duration = tempdbl
      End If
    Next
  Next
  Exit Sub
  
DBLERROR:
  MsgBox "Invalid value entered for duration." + Chr(13) + Chr(10) + "Value entered must be greater than 0.", vbExclamation + vbOKOnly, "Error"
  txt(Index).Text = ""
End Sub

Private Sub ExpPop_Change()
  Dim i As Integer
  Dim j As Integer
  Dim tempint As Integer
  
  If treeclick Then Exit Sub
On Error GoTo INTERROR
  tempint = CInt(ExpPop.Text)
  For j = 1 To MAXMEDIA
    For i = 1 To med(j).numages
      If med(j).ages(i).begAge = begAge And med(j).ages(i).endAge = endAge Then
        med(j).ages(i).population = tempint
      End If
    Next
  Next
  Exit Sub
  
INTERROR:
  MsgBox "Invalid value entered for population." + Chr(13) + Chr(10) + "Value entered must be an integer.", vbExclamation + vbOKOnly, "Error"
  ExpPop.Text = ""
End Sub


Private Sub unit_Click(Index As Integer)
  Dim i As Integer
  Dim j As Integer
  
  If loadng Or treeclick Then Exit Sub
  If Index = 2 Then
    For i = 1 To med(medIdx).ages(ageIdx).chem(chmIdx).numtime
      med(medIdx).ages(ageIdx).chem(chmIdx).tunit(USERUNIT) = unit(2).list(unit(2).ListIndex)
    Next
  ElseIf Index = 3 Then
    For j = 1 To MAXMEDIA
      For i = 1 To med(j).numages
        If med(j).ages(i).begAge = begAge And med(j).ages(i).endAge = endAge Then
          med(j).ages(i).dunit(USERUNIT) = unit(3).list(unit(3).ListIndex)
        End If
      Next
    Next
  ElseIf Index = 4 Then
    med(medIdx).lunits(USERUNIT) = unit(4).list(unit(4).ListIndex)
  ElseIf Index = 5 Then
    For i = 1 To med(medIdx).numages
      med(medIdx).ages(i).ageunit(USERUNIT) = unit(5).list(unit(5).ListIndex)
    Next
  ElseIf Index = 1 Then
    If tvwEPF.SelectedItem.parent.Text = CONSTITUENT Then
      med(medIdx).ages(ageIdx).chem(chmIdx).exp(expIdx).cunit(USERUNIT) = unit(1).Text
    Else
      med(medIdx).ages(ageIdx).chem(chmIdx).prog(prgIdx).exp(expIdx).cunit(USERUNIT) = unit(1).Text
    End If
  End If
End Sub

Private Sub vaSpread1_Change(ByVal col As Long, ByVal Row As Long)
  Dim i As Integer
  Dim tempvalue As String
  Dim tempvalue2 As String
  
  med(medIdx).numloc = 0
  i = 1
  vaSpread1.Row = 1
  vaSpread1.col = 1
  tempvalue = vaSpread1.Value
  While Not tempvalue = ""
    med(medIdx).numloc = med(medIdx).numloc + 1
    ReDim Preserve med(medIdx).locx(med(medIdx).numloc) As Double
    ReDim Preserve med(medIdx).locy(med(medIdx).numloc) As Double
    med(medIdx).locx(med(medIdx).numloc) = Val(vaSpread1.Value)
    vaSpread1.col = 2
    med(medIdx).locy(med(medIdx).numloc) = Val(vaSpread1.Value)
    i = i + 1
    vaSpread1.Row = i
    vaSpread1.col = 1
    tempvalue = vaSpread1.Value
  Wend
End Sub

Private Sub vaSpread2_Change(ByVal col As Long, ByVal Row As Long)
  Dim temptime As String
  Dim tempdur As String
  Dim Index As Integer
  
  vaSpread2.Row = 1
  vaSpread2.col = 1
  temptime = vaSpread2.Value
 ' vaSpread2.col = 2
 ' tempdur = vaSpread2.Value
  Index = 0
  While Not temptime = ""
    Index = Index + 1
    ReDim Preserve med(medIdx).ages(ageIdx).chem(chmIdx).time(Index) As Double
    med(medIdx).ages(ageIdx).chem(chmIdx).time(Index) = Val(temptime)
'    med(medIdx).ages(ageIdx).chem(chmIdx).dur(Index) = Val(tempdur)
    med(medIdx).ages(ageIdx).chem(chmIdx).tunit(INTLUNIT) = YR
    med(medIdx).ages(ageIdx).chem(chmIdx).tunit(USERUNIT) = unit(2).list(unit(2).ListIndex)
'    med(medIdx).ages(ageIdx).chem(chmIdx).dunit(INTLUNIT) = YR
'    med(medIdx).ages(ageIdx).chem(chmIdx).dunit(USERUNIT) = unit(3).list(unit(3).ListIndex)
    med(medIdx).ages(ageIdx).chem(chmIdx).numtime = Index
    vaSpread2.Row = Index + 1
    vaSpread2.col = 1
    temptime = vaSpread2.Value
'    vaSpread2.col = 2
'    tempdur = vaSpread2.Value
  Wend
End Sub

Private Sub vaSpread3_Change(ByVal col As Long, ByVal Row As Long)
  Dim i As Integer
  Dim j As Integer
  
  If tvwEPF.SelectedItem.parent.Text = CONSTITUENT Then
    ReDim med(medIdx).ages(ageIdx).chem(chmIdx).exp(expIdx).series(med(medIdx).ages(ageIdx).chem(chmIdx).numtime) As Entry
    med(medIdx).ages(ageIdx).chem(chmIdx).exp(expIdx).numseries = med(medIdx).ages(ageIdx).chem(chmIdx).numtime
    For i = 1 To med(medIdx).ages(ageIdx).chem(chmIdx).numtime
      vaSpread3.Row = i
      ReDim med(medIdx).ages(ageIdx).chem(chmIdx).exp(expIdx).series(i).valu(vaSpread3.MaxCols - 1) As Double
      med(medIdx).ages(ageIdx).chem(chmIdx).exp(expIdx).series(i).num = vaSpread3.MaxCols - 1
      For j = 1 To med(medIdx).numloc
        vaSpread3.col = j + 1
        med(medIdx).ages(ageIdx).chem(chmIdx).exp(expIdx).series(i).valu(j) = Val(vaSpread3.Value)
      Next
    Next
  Else
    ReDim med(medIdx).ages(ageIdx).chem(chmIdx).prog(prgIdx).exp(expIdx).series(med(medIdx).ages(ageIdx).chem(chmIdx).numtime) As Entry
    med(medIdx).ages(ageIdx).chem(chmIdx).prog(prgIdx).exp(expIdx).numseries = med(medIdx).ages(ageIdx).chem(chmIdx).numtime
    For i = 1 To med(medIdx).ages(ageIdx).chem(chmIdx).numtime
      vaSpread3.Row = i
      ReDim med(medIdx).ages(ageIdx).chem(chmIdx).prog(prgIdx).exp(expIdx).series(i).valu(vaSpread3.MaxCols - 1) As Double
      med(medIdx).ages(ageIdx).chem(chmIdx).prog(prgIdx).exp(expIdx).series(i).num = vaSpread3.MaxCols - 1
      For j = 1 To med(medIdx).numloc
        vaSpread3.col = j + 1
        med(medIdx).ages(ageIdx).chem(chmIdx).prog(prgIdx).exp(expIdx).series(i).valu(j) = Val(vaSpread3.Value)
      Next
    Next
  End If

End Sub

Private Sub NumFixed_Click()
  CVTFormat = "Fixed"
  FormatChecked (CVTFormat)
   'tell user how number will appear
   MsgBox "Displays at least one digit to the left and two " + vbCrLf + _
      "digits to the right of the decimal separator.", vbOKOnly, "Fixed Number Format Selected"
  ResetVisibleFormats
End Sub

Private Sub NumGeneral_Click()
  CVTFormat = "General Number"
  FormatChecked (CVTFormat)
  'tell user how number will appear
  MsgBox "Displays number with no thousand separator.", vbOKOnly, "General Number Format Selected"
  ResetVisibleFormats
End Sub

Private Sub NumScientific_Click()
  CVTFormat = "Scientific"
  FormatChecked (CVTFormat)
  'tell user how number will appear
  MsgBox "Uses standard scientific notation", vbOKOnly, "Scientific Number Format Selected"
  ResetVisibleFormats
End Sub
  
Private Sub NumStandard_Click()
  CVTFormat = "Standard"
  FormatChecked (CVTFormat)
  'tell user how number will appear
  MsgBox "Displays number with thousand separator, at least " + vbCrLf + _
       "one digit to the left and two digits to the " + vbCrLf + _
       "right of the decimal separator.", vbOKOnly, "Standard Number Format Selected"
  ResetVisibleFormats
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
  OkayStr = CheckUserDefinedFormat(UserStrg)     'basic check of characters
  If OkayStr Then
    CVTFormat = UserStrg
    FormatChecked (CVTFormat)
    ResetVisibleFormats
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

Private Sub ResetVisibleFormats()
  Dim sel As Long
  sel = tvwEPF.SelectedItem.Index
  LoadTreeViewFromRead
  Set tvwEPF.SelectedItem = tvwEPF.Nodes(sel)
  tvwEPF_NodeClick tvwEPF.Nodes(sel)
End Sub

Private Sub lstMedia_GotFocus()
  HelpAnchor = lstMedia.Tag
End Sub

Private Sub cmdAge_GotFocus(Index As Integer)
  HelpAnchor = cmdAge(Index).Tag
End Sub

Private Sub cmdRoute_GotFocus(Index As Integer)
  HelpAnchor = cmdRoute(Index).Tag
End Sub

Private Sub ExpPop_GotFocus()
  HelpAnchor = ExpPop.Tag
End Sub

Private Sub lstAges_GotFocus()
  HelpAnchor = lstAges.Tag
End Sub

Private Sub lstExposure_GotFocus(Index As Integer)
  HelpAnchor = lstExposure(Index).Tag
End Sub

Private Sub optDataSet_GotFocus(Index As Integer)
  HelpAnchor = optDataSet(Index).Tag
End Sub

Private Sub Txt_GotFocus(Index As Integer)
  HelpAnchor = Lbl(Index).Tag
End Sub

Private Sub unit_GotFocus(Index As Integer)
  HelpAnchor = Lbl(Index).Tag
End Sub

Private Sub vaSpread1_GotFocus()
  HelpAnchor = vaSpread1.Tag
End Sub

Private Sub vaSpread2_GotFocus()
  HelpAnchor = vaSpread2.Tag
End Sub

Private Sub vaSpread3_GotFocus()
  HelpAnchor = vaSpread3.Tag
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
