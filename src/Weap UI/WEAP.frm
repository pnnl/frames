VERSION 5.00
Object = "{6B7E6392-850A-101B-AFC0-4210102A8DA7}#1.3#0"; "comctl32.ocx"
Object = "{F856EC8B-F03C-4515-BDC6-64CBD617566A}#7.0#0"; "FPSPR70.ocx"
Begin VB.Form Weap 
   BorderStyle     =   1  'Fixed Single
   ClientHeight    =   6015
   ClientLeft      =   6075
   ClientTop       =   2055
   ClientWidth     =   9105
   Icon            =   "WEAP.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   NegotiateMenus  =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   401
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   607
   StartUpPosition =   2  'CenterScreen
   Begin VB.Frame Frame1 
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   5865
      Left            =   4200
      TabIndex        =   10
      Top             =   0
      Width           =   4740
      Begin VB.CheckBox dose 
         Caption         =   "User defined TRVs"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   400
         Index           =   8
         Left            =   240
         TabIndex        =   9
         Tag             =   "user"
         Top             =   720
         Width           =   4000
      End
      Begin VB.CheckBox dose 
         Caption         =   "No observable effect level"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   400
         Index           =   7
         Left            =   240
         TabIndex        =   8
         Tag             =   "noel"
         Top             =   4080
         Visible         =   0   'False
         Width           =   4000
      End
      Begin VB.CheckBox dose 
         Caption         =   "Lowest observable effects dose"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   400
         Index           =   6
         Left            =   240
         TabIndex        =   6
         Tag             =   "loed"
         Top             =   3120
         Visible         =   0   'False
         Width           =   4000
      End
      Begin VB.CheckBox dose 
         Caption         =   "No observable effects dose"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   400
         Index           =   5
         Left            =   240
         TabIndex        =   7
         Tag             =   "noed"
         Top             =   3600
         Visible         =   0   'False
         Width           =   4000
      End
      Begin VB.CheckBox dose 
         Caption         =   "Lowest observable effect level"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   400
         Index           =   4
         Left            =   240
         TabIndex        =   5
         Tag             =   "loel"
         Top             =   2610
         Visible         =   0   'False
         Width           =   4000
      End
      Begin VB.CheckBox dose 
         Caption         =   "Effective Dose"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   400
         Index           =   3
         Left            =   240
         TabIndex        =   4
         Tag             =   "ed"
         Top             =   2130
         Visible         =   0   'False
         Width           =   4000
      End
      Begin VB.CheckBox dose 
         Caption         =   "Effective Concentrations"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   400
         Index           =   2
         Left            =   240
         TabIndex        =   3
         Tag             =   "ec"
         Top             =   1650
         Visible         =   0   'False
         Width           =   4000
      End
      Begin VB.CheckBox dose 
         Caption         =   "Lethal Dose"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   400
         Index           =   1
         Left            =   240
         TabIndex        =   2
         Tag             =   "ld"
         Top             =   1170
         Visible         =   0   'False
         Width           =   4000
      End
      Begin VB.CheckBox dose 
         Caption         =   "Lethal Concentrations"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   400
         Index           =   0
         Left            =   240
         TabIndex        =   1
         Tag             =   "lc"
         Top             =   4560
         Visible         =   0   'False
         Width           =   4000
      End
      Begin VB.Label Label1 
         Alignment       =   2  'Center
         Caption         =   "Select the desired options for the analysis."
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   495
         Left            =   180
         TabIndex        =   43
         Top             =   360
         Width           =   4005
      End
   End
   Begin ComctlLib.TreeView DoseView 
      Height          =   5775
      Left            =   120
      TabIndex        =   0
      Top             =   90
      Width           =   3975
      _ExtentX        =   7011
      _ExtentY        =   10186
      _Version        =   327682
      HideSelection   =   0   'False
      Indentation     =   176
      LineStyle       =   1
      Style           =   7
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
   Begin VB.Frame Frame7 
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   5865
      Left            =   4200
      TabIndex        =   40
      Top             =   0
      Visible         =   0   'False
      Width           =   4740
      Begin VB.CheckBox dose 
         Caption         =   "Toxicity Reference Value"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   400
         Index           =   10
         Left            =   360
         TabIndex        =   46
         Tag             =   "cct"
         Top             =   1320
         Width           =   4000
      End
      Begin VB.CheckBox dose 
         Caption         =   "Soil Screening Level"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   400
         Index           =   9
         Left            =   360
         TabIndex        =   44
         Tag             =   "ssl"
         Top             =   960
         Width           =   4000
      End
      Begin VB.Label Label7 
         Alignment       =   2  'Center
         Caption         =   "Select the desired options for the analysis."
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   495
         Left            =   360
         TabIndex        =   45
         Top             =   360
         Width           =   4005
      End
   End
   Begin VB.Frame Frame2 
      Caption         =   "Frame2"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   5865
      Left            =   4200
      TabIndex        =   13
      Top             =   0
      Width           =   4740
      Begin VB.ListBox List1 
         Height          =   4560
         Left            =   240
         Style           =   1  'Checkbox
         TabIndex        =   12
         Top             =   960
         Width           =   4215
      End
      Begin VB.Label Label2 
         Alignment       =   2  'Center
         Caption         =   "Select the desired options for the analysis."
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   495
         Left            =   195
         TabIndex        =   11
         Top             =   390
         Width           =   4350
      End
   End
   Begin VB.Frame Frame5 
      Caption         =   "Frame5"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   5865
      Left            =   4200
      TabIndex        =   27
      Top             =   0
      Width           =   4740
      Begin VB.TextBox Text1 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   288
         Left            =   360
         TabIndex        =   24
         Text            =   "Text1"
         Top             =   3120
         Width           =   3945
      End
      Begin VB.TextBox txt 
         Alignment       =   1  'Right Justify
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   288
         Index           =   1
         Left            =   288
         TabIndex        =   19
         Tag             =   "acutetime"
         Top             =   1248
         Width           =   1000
      End
      Begin VB.ComboBox unit 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   315
         Index           =   1
         Left            =   1248
         Style           =   2  'Dropdown List
         TabIndex        =   20
         Tag             =   "g/ml"
         Top             =   1248
         Width           =   1000
      End
      Begin VB.TextBox txt 
         Alignment       =   1  'Right Justify
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   288
         Index           =   0
         Left            =   288
         TabIndex        =   15
         Tag             =   "acutetime"
         Top             =   576
         Width           =   1000
      End
      Begin VB.ComboBox unit 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   315
         Index           =   0
         Left            =   1248
         Style           =   2  'Dropdown List
         TabIndex        =   16
         Tag             =   "yr"
         Top             =   576
         Width           =   1000
      End
      Begin FPSpreadADO.fpSpread vaSpread1 
         Height          =   2064
         Left            =   360
         TabIndex        =   26
         Top             =   3480
         Width           =   2832
         _Version        =   458752
         _ExtentX        =   4995
         _ExtentY        =   3641
         _StockProps     =   64
         ArrowsExitEditMode=   -1  'True
         AutoCalc        =   0   'False
         BackColorStyle  =   1
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
         RowsFrozen      =   1
         ScrollBarExtMode=   -1  'True
         ScrollBarShowMax=   0   'False
         SpreadDesigner  =   "WEAP.frx":030A
         StartingColNumber=   0
         StartingRowNumber=   0
         UserResize      =   1
         VisibleCols     =   2
         VisibleRows     =   8
      End
      Begin VB.Label Label6 
         Caption         =   "Description of effect curve"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   210
         Left            =   240
         TabIndex        =   23
         Top             =   2880
         Width           =   2310
      End
      Begin VB.Label ref 
         Caption         =   "Ref: 0"
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
         Left            =   2304
         TabIndex        =   21
         Tag             =   "0"
         Top             =   1248
         Width           =   996
      End
      Begin VB.Label lbl 
         Caption         =   "Chronic Limit - CCC"
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
         TabIndex        =   18
         Top             =   960
         Width           =   3900
      End
      Begin VB.Label ref 
         Caption         =   "Ref: 0"
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
         Left            =   3480
         TabIndex        =   25
         Tag             =   "0"
         Top             =   2760
         Width           =   870
      End
      Begin VB.Label ref 
         Caption         =   "Ref: 0"
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
         Left            =   2304
         TabIndex        =   17
         Tag             =   "0"
         Top             =   576
         Width           =   996
      End
      Begin VB.Label lbl 
         Caption         =   "Acute exposure duration- ACUTETIME"
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
         TabIndex        =   14
         Top             =   288
         Width           =   3900
      End
      Begin VB.Label Label5 
         Caption         =   $"WEAP.frx":6B87
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   1164
         Left            =   192
         TabIndex        =   22
         Top             =   1728
         Width           =   3756
      End
   End
   Begin VB.Frame Frame4 
      Caption         =   "Frame4"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   5865
      Left            =   4200
      TabIndex        =   31
      Top             =   0
      Width           =   4740
      Begin FPSpreadADO.fpSpread vaSpread2 
         Height          =   2976
         Left            =   360
         TabIndex        =   30
         Top             =   2208
         Width           =   3252
         _Version        =   458752
         _ExtentX        =   5736
         _ExtentY        =   5249
         _StockProps     =   64
         ArrowsExitEditMode=   -1  'True
         AutoCalc        =   0   'False
         BackColorStyle  =   1
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
         RowsFrozen      =   1
         ScrollBarExtMode=   -1  'True
         ScrollBarShowMax=   0   'False
         SpreadDesigner  =   "WEAP.frx":6C24
         StartingColNumber=   0
         StartingRowNumber=   0
         VisibleCols     =   2
         VisibleRows     =   12
      End
      Begin VB.Label ref 
         Caption         =   "Ref: 0"
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
         Index           =   3
         Left            =   2640
         TabIndex        =   29
         Tag             =   "0"
         Top             =   1920
         Width           =   990
      End
      Begin VB.Label Label4 
         Caption         =   $"WEAP.frx":D4BA
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   1452
         Left            =   192
         TabIndex        =   28
         Top             =   384
         Width           =   3468
      End
   End
   Begin VB.Frame Frame6 
      Caption         =   "User Toxicity Reference Value"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   5865
      Left            =   4200
      TabIndex        =   39
      Top             =   0
      Width           =   4740
      Begin VB.ComboBox Combo1 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   315
         Index           =   1
         Left            =   285
         Style           =   2  'Dropdown List
         TabIndex        =   33
         Tag             =   "yr"
         Top             =   600
         Width           =   4005
      End
      Begin VB.ComboBox Combo1 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   315
         Index           =   2
         Left            =   285
         Style           =   2  'Dropdown List
         TabIndex        =   35
         Tag             =   "g/ml"
         Top             =   1320
         Width           =   4005
      End
      Begin FPSpreadADO.fpSpread vaSpread3 
         Height          =   2304
         Left            =   360
         TabIndex        =   38
         Top             =   3240
         Width           =   3876
         _Version        =   458752
         _ExtentX        =   6837
         _ExtentY        =   4064
         _StockProps     =   64
         ArrowsExitEditMode=   -1  'True
         AutoCalc        =   0   'False
         BackColorStyle  =   1
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
         MaxCols         =   3
         RowsFrozen      =   1
         ScrollBarExtMode=   -1  'True
         ScrollBarShowMax=   0   'False
         SpreadDesigner  =   "WEAP.frx":D56D
         StartingColNumber=   0
         StartingRowNumber=   0
         UserResize      =   1
         VisibleCols     =   3
         VisibleRows     =   8
      End
      Begin VB.Label ref 
         Caption         =   "Ref: 0"
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
         Index           =   4
         Left            =   3360
         TabIndex        =   37
         Tag             =   "0"
         Top             =   2880
         Width           =   870
      End
      Begin VB.Label Label8 
         Caption         =   $"WEAP.frx":17120
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   1005
         Left            =   240
         TabIndex        =   36
         Top             =   1800
         Width           =   3615
      End
      Begin VB.Label lbl 
         Caption         =   "Constituent"
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
         Left            =   240
         TabIndex        =   34
         Top             =   1080
         Width           =   3660
      End
      Begin VB.Label lbl 
         Caption         =   "Organism"
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
         Index           =   0
         Left            =   240
         TabIndex        =   32
         Top             =   360
         Width           =   3660
      End
   End
   Begin VB.Frame Frame3 
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   5865
      Left            =   4200
      TabIndex        =   41
      Top             =   0
      Width           =   4740
      Begin VB.Label Label3 
         Alignment       =   2  'Center
         Caption         =   "No selections nessasary for this panel."
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   588
         Left            =   192
         TabIndex        =   42
         Top             =   2592
         Width           =   3756
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
   Begin VB.Menu about 
      Caption         =   "&About"
   End
End
Attribute VB_Name = "Weap"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text
Dim temp As parmrec
Dim loadng As Boolean
Dim uchemprev As Integer
Dim uorgprev As Integer
  Dim relative As String
  
  Dim des As MyCollection
  Dim idxs As MyCollection
  Dim progidxs As MyCollection


Private Sub about_Click()
  frmAbout.Show 1, Weap
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

Private Sub Dose_Click(Index As Integer)
Dim xnode As Comctllib.node
  
  Select Case dose(Index).value
  Case 0
    DoseView.Nodes.Remove dose(Index).Tag
  Case 1
    Select Case Index
'    Case 0, 1, 7
'      Set xnode = DoseView.Nodes.Add("bbf", tvwChild, dose(Index).Tag, dose(Index).Caption)
'    Case 2, 3, 4, 5, 6
'      Set xnode = DoseView.Nodes.Add("bbf", tvwChild, dose(Index).Tag, dose(Index).Caption)
'      xnode.Expanded = True
'      DoseView.Nodes.Add dose(Index).Tag, tvwChild, dose(Index).Tag + "part", "Body part of concern"
'      DoseView.Nodes.Add dose(Index).Tag, tvwChild, dose(Index).Tag + "effect", "Type of effect"
    Case 8
      Set xnode = DoseView.Nodes.Add(relative, tvwChild, dose(Index).Tag, "User TRVs")
'    Case 9, 10
'      Set xnode = DoseView.Nodes.Add("scf", tvwChild, dose(Index).Tag, dose(Index).Caption & " Jurisdictions")
    End Select
  End Select
End Sub

Private Sub DoseView_BeforeLabelEdit(Cancel As Integer)
  Cancel = 1
End Sub

Private Sub SetMyList(node As Comctllib.node, st As Long)
Dim i As Long
Dim sel As SeriesPair
Dim myvals As MyCollection
  
  List1.Clear
  Set sel = des.ItemIdx(st)
  Set myvals = idxs.ItemKey(sel.v1)
  For i = 1 To myvals.Count
    List1.AddItem myvals.ItemIdx(i)
    If Mid(sel.v2, i, 1) = "T" Then List1.Selected(i - 1) = True
  Next
  Frame2.Caption = node.Text
  Frame2.Visible = True
End Sub

'Private Sub SetList(node As Comctllib.node, mc As MyCollection, st As Integer)
'Dim i As Long
'  List1.Clear
'  For i = 1 To mc.Count
'    List1.AddItem mc.ItemIdx(i)
'    If Mid(mc.Key(i), st, 1) = "T" Then List1.Selected(i - 1) = True
'  Next
'  Frame2.Caption = node.Text
'  Frame2.Visible = True
'End Sub

Private Sub GetIdx(key As String, i As Integer, j As Integer)
  i = Val(key)
  j = Val(Right(key, Len(key) - InStr(1, key, "_")))
End Sub

Private Sub Combo1_Click(Index As Integer)
Dim i As Long

  If loadng Then Exit Sub
  If Combo1(1).ListIndex < 0 Or Combo1(2).ListIndex < 0 Then Exit Sub
  If uorgprev > -1 And uchemprev > -1 Then Organism(uorgprev).TRVs(uchemprev).GetTRVs vaSpread3, ref(4)
  uorgprev = Combo1(1).ItemData(Combo1(1).ListIndex)
  For i = 1 To Organism(uorgprev).TRVs.Count
    If Organism(uorgprev).TRVs(i).idx = Combo1(2).ListIndex + 1 Then uchemprev = i
  Next
  Organism(uorgprev).TRVs(uchemprev).PutTRVs vaSpread3, ref(4)
End Sub

Private Sub DoseView_NodeClick(ByVal node As Comctllib.node)
Dim i As Integer
Dim j As Integer
Dim idx As Long

  If node Is Nothing Then Exit Sub
  loadng = True
  
  If Frame4.Visible = True And Frame4.Tag <> "" Then
    GetIdx Frame4.Tag, i, j
    LifeForm(i).Locs.GetLocFreq vaSpread2, ref(3)
  End If
  
  If Frame5.Visible = True And Frame5.Tag <> "" Then
    GetIdx Frame5.Tag, i, j
    LifeForm(i).Doses(j).GetAcute txt(0), unit(0), ref(0)
    LifeForm(i).Doses(j).GetCCC txt(1), unit(1), ref(1)
    LifeForm(i).Doses(j).GetDoseCurve vaSpread1, ref(1), Text1
  End If
  
'  If Frame6.Visible = True And Frame5.Tag <> "" Then
'    GetIdx Frame6.Tag, i, j
'    LifeForm(i).Doses(j).GetTRVs vaSpread3, ref(1), Text1
'  End If
  
  Frame1.Visible = False
  Frame2.Visible = False
  Frame3.Visible = False
  Frame4.Visible = False
  Frame5.Visible = False
  Frame6.Visible = False
  Frame7.Visible = False
  
  If Left(node.key, 2) = "l_" Then
    Frame4.Tag = node.Tag
    GetIdx Frame4.Tag, i, j
    LifeForm(i).Locs.PutLocFreq vaSpread2, ref(3)
    Frame4.Caption = "Time vs Location for " + node.Text
    Frame4.Visible = True
  End If
  
  If Left(node.key, 2) = "c_" Then
    Frame5.Tag = node.Tag
    GetIdx Frame5.Tag, i, j
    LifeForm(i).Doses(j).PutAcute txt(0), unit(0), ref(0)
    LifeForm(i).Doses(j).PutCCC txt(1), unit(1), ref(1)
    LifeForm(i).Doses(j).PutDoseCurve vaSpread1, ref(1), Text1
    Frame5.Caption = node.Text + " effects info"
    Frame5.Visible = True
  End If
  
  Select Case node.key
'    Case "ssl":         SetList node, ssl, 1
'    Case "cct":         SetList node, cct, 1
'    Case "lc":          SetList node, lc, 1
'    Case "ld":          SetList node, ld, 1
'    Case "ec":          SetList node, ec, 1
'    Case "ed":          SetList node, ed, 1
'    Case "ecpart":      SetList node, part, 1
'    Case "edpart":      SetList node, part, 2
'    Case "loelpart":    SetList node, part, 3
'    Case "loedpart":    SetList node, ldpart, 3
'    Case "noedpart":    SetList node, ndpart, 3
'    Case "eceffect":    SetList node, effect, 1
'    Case "edeffect":    SetList node, effect, 2
'    Case "loeleffect":  SetList node, effect, 3
'    Case "loedeffect":  SetList node, ldeffect, 3
'    Case "noedeffect":  SetList node, ndeffect, 3
    Case "user":
      Frame6.Visible = True
    Case "bbf", "twi"
      Frame1.Caption = node.Text
      Frame1.Visible = True
      Frame1.Tag = node.key
'    Case "scf", "twi"
'      Frame7.Caption = node.Text
'      Frame7.Visible = True
'    Case "Organism", "loel", "loed", "noel", "noed"
'      Frame3.Caption = node.Text
'      Frame3.Visible = True
    Case Else
      idx = des.Index(node.key)
      If idx > 0 Then SetMyList node, idx
      If Not Frame2.Visible Then
        Frame3.Caption = node.Text
        Frame3.Visible = True
      End If
  End Select
  
  loadng = False
End Sub

Private Sub addLoc(k1 As String)
On Error Resume Next
  LifeForm.Add New LifeForms, k1
End Sub

Private Sub addTRV(k1 As String, k2 As String)
On Error Resume Next
  Organism.Add New Organisms, k1
  Organism(k1).TRVs.Add New TRV, k2
End Sub

Private Sub addTRVSeries(k1 As String, k2 As String, k3 As String)
On Error Resume Next
  addTRV k1, k2
  Organism(k1).TRVs(k2).Addd New SeriesPair, k3
End Sub

Private Sub addDose(k1 As String, k2 As String)
On Error Resume Next
  LifeForm.Add New LifeForms, k1
  LifeForm(k1).Doses.Add New dose, k2
End Sub

Private Sub addDoseSeries(k1 As String, k2 As String, k3 As String)
On Error Resume Next
  addDose k1, k2
  LifeForm(k1).Doses(k2).Addd New SeriesPair, k3
End Sub

Private Sub addLifeProp(idx As Integer)
  If temp.idx1 > numlife Then
    numlife = temp.idx1
    ReDim Preserve life(3, numlife) As String
  End If
  life(idx, temp.idx1) = temp.pval
End Sub

Private Sub addChemProp(idx As Integer)
  If temp.idx3 = 0 Then
    If temp.idx2 > numchem Then
      numchem = temp.idx2
      ReDim Preserve chem(3, numchem) As String
    End If
    chem(idx, temp.idx2) = temp.pval
  End If
End Sub
'
'Private Sub addLocProp(idx As Integer)
'  If temp.idx2 = ModIdx Then
'    If temp.idx3 > numloc Then
'      numloc = temp.idx3
'      ReDim Preserve loc(2, numloc) As String
'    End If
'    loc(idx, temp.idx3) = temp.pval
'  End If
'End Sub
                  
Private Sub SetSelection(idx As Long)
Dim idx2 As Long
Dim key As String
Dim sel As SeriesPair
Dim myvals As MyCollection
  
  Set sel = des.ItemIdx(idx)
  ' get index value from the index position for the key, used with in the parameter
  
    Select Case sel.v3
      Case 1: idx2 = temp.idx1
      Case 2: idx2 = temp.idx2
      Case 3: idx2 = temp.idx3
      Case 4: idx2 = temp.idx4
      Case 5: idx2 = temp.idx5
      Case 6: idx2 = temp.idx6
    End Select
  
  If idx2 = 0 Then
    idx2 = temp.idx1
  End If
  'map to new index, we can do this because all indices got loaded first
  Set myvals = progidxs.ItemKey(sel.v1)
  key = myvals.key(idx2)
  'Leave sub if key not found, this means never had one
  If key = "" Then Exit Sub
  Set myvals = idxs.ItemKey(sel.v1)
  idx2 = myvals.IndexItem(key)
  'Leave sub if the key not found,  idx2=0
  If idx2 = 0 Then Exit Sub
  
  'set selection
  If Len(sel.v2) < idx2 Then sel.v2 = sel.v2 + String(idx2 - Len(sel.v2), "F")
  sel.v2 = Left$(sel.v2, idx2 - 1) + temp.pval + Right$(sel.v2, Len(sel.v2) - idx2)
End Sub

Private Sub InitSelection(idx As Long)
Dim idx2 As Long
Dim sel As SeriesPair

  Set sel = des.ItemIdx(idx)
  ' get index value from the index position for the key, used by the parameter
  Select Case sel.v3
    Case 1: idx2 = temp.idx1
    Case 2: idx2 = temp.idx2
    Case 3: idx2 = temp.idx3
    Case 4: idx2 = temp.idx4
    Case 5: idx2 = temp.idx5
    Case 6: idx2 = temp.idx6
  End Select
  
  If Len(sel.v2) < idx2 Then
    sel.v2 = sel.v2 + String(idx2 - Len(sel.v2), "F")
  Else
    sel.v2 = Left$(sel.v2, idx2 - 1) + "F" + Right$(sel.v2, Len(sel.v2) - idx2)
  End If
End Sub
  
Sub LoadDes(section As String)  'Aquatic /Terrestrial
  Dim i As Long
  Dim j As Long
  Dim desCnt As Long
  Dim idxCnt As Long
  Dim value As String
  Dim postfix As String
  Dim svs As Variant
  Dim sval As Boolean
  Dim sel As SeriesPair
  Dim myvals As MyCollection
  
  If des.Count > 0 Then Exit Sub
  
  ReadIniString section, "postfix", "", postfix
  
  'load up structure for trv lookup
  sval = ReadIniLong(section, "desCount", 0, desCnt)
  For i = 1 To desCnt
    ReadIniString section, "des" & CStr(i), "", value
    svs = Split(value, ",")
    Set sel = New SeriesPair
    sel.v1 = svs(0)
    sel.v2 = ""
    sel.v3 = svs(2)
    des.Add sel, svs(1) & postfix
    Set myvals = New MyCollection
    idxs.Add myvals, CStr(svs(0))
    Set myvals = New MyCollection
    progidxs.Add myvals, CStr(svs(0))
  Next
  
  sval = ReadIniLong(section, "idxCount", 0, idxCnt)
  For i = 1 To idxCnt
    ReadIniString section, "idx" & CStr(i), "", value
    svs = Split(value, ",")
    For j = 1 To Val(svs(1))
      ' check to see if added
      If des.Index(CStr(svs(j * 2))) = 0 Then
        des.Add Null, svs(j * 2) & postfix
      End If
      'v1 will hold the name of the index parameter (bodypart, ...)
      'v2 will hold the selection (T/F) for each value of the index (lungs, heart ...)
      'v2 length will be the count of the values for this index
      'if v2 = "" then not visible
      'v3 will hold position index (1-6) for this index(bodypart...) for this parameter
      Set sel = New SeriesPair
      sel.v1 = svs(0)
      sel.v2 = ""
      sel.v3 = svs(j * 2 + 1)
      des.Add sel, svs(0) & svs(j * 2) & postfix
    Next
    Set myvals = New MyCollection
    idxs.Add myvals, CStr(svs(0))
    Set myvals = New MyCollection
    progidxs.Add myvals, CStr(svs(0))
  Next
End Sub

Private Sub loadprm()
  Dim idx As Long
  Dim edfound As Boolean
  Dim xnode As Comctllib.node
  Dim i As Long, j As Long, k As Long, l As Long, m As Long, cnt As Long
  Dim prm As String
  Dim prefix As String
  Dim sval As Boolean
  Dim orgkey As String
  Dim likey As String
  Dim lokey As String
  Dim dokey As String
  Dim mynumloc As Long
  Dim locidx() As Long
  Dim section(5) As String
  Dim sel As SeriesPair
  Dim myvals As MyCollection
  
'  Dim rv As MyCollection
'  Dim mypart As MyCollection
'  Dim myeffect As MyCollection
'  Dim myLDdescript As MyCollection
'  Dim myLCdescript As MyCollection
'  Dim myEDdescript As MyCollection
'  Dim myECdescript As MyCollection
'  Dim mySSLJuris As MyCollection
'  Dim myCCTJuris As New MyCollection
'
'  Set mypart = New MyCollection
'  Set myeffect = New MyCollection
'  Set myLDdescript = New MyCollection
'  Set myLCdescript = New MyCollection
'  Set myEDdescript = New MyCollection
'  Set myECdescript = New MyCollection
'  Set mySSLJuris = New MyCollection
'  Set myCCTJuris = New MyCollection
'
  Set des = New MyCollection
  Set idxs = New MyCollection
  Set progidxs = New MyCollection
  
  edfound = False
  numloc = 0
  
  Set xnode = DoseView.Nodes.Add(, , "bbf", "Aquatic Dose Assessment")
  xnode.Expanded = True
'  Set xnode = DoseView.Nodes.Add("bbf", tvwChild, "bbfuser", "User TRVs")
  Set xnode = DoseView.Nodes.Add(, , "scf", "Terrestrial Soil Screening Assessment")
  xnode.Expanded = True
  
  Set xnode = DoseView.Nodes.Add(, , "twi", "Terrestrial Intake Assessment")
  xnode.Expanded = True
'  Set xnode = DoseView.Nodes.Add("twi", tvwChild, "scfuser", "User TRVs")

  If open_parm(fle, FUIName, 2) Then
  'loading CSM section and pull out sources and database section names
    LoadSection fle, "csm"
    If ReadLng(fle, cnt, "nummod", siteIdx) Then
      For i = 1 To cnt
        If ReadStr(fle, prefix, "modid", siteIdx, i) Then
          If prefix = modName Then
            ReadStr fle, DesName, "moddespath", siteIdx, i
            If ReadLng(fle, numloc, "modsrcnum", siteIdx, i) Then
              ReDim loc(4, numloc)
              For j = 1 To numloc
                ReadStr fle, loc(wID, j), "modsrcid", siteIdx, i, j
                ReadStr fle, loc(wName, j), "modsrclabel", siteIdx, i, j
                ReadStr fle, loc(wType, j), "modsrctype", siteIdx, i, j
                ReadStr fle, loc(wQual, j), "modsrcqual", siteIdx, i, j
                If loc(wType, j) = "con" Then
                  section(0) = loc(wID, j)
                ElseIf loc(wType, j) = "ebf" Then
                  section(1) = loc(wID, j)
                  LoadDes "EBF"
                ElseIf loc(wType, j) = "aos" Then
                  section(2) = loc(wID, j)
                ElseIf loc(wType, j) = "tos" Then
                  section(3) = loc(wID, j)
                ElseIf loc(wType, j) = "cct" Then
                  section(4) = loc(wID, j)
                  LoadDes "CCT"
                ElseIf loc(wType, j) = "ssl" Then
                  section(5) = loc(wID, j)
                  LoadDes "SSL"
                End If
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
    
    'must read these sections first
    reset_csv fle.file
    Do Until EOCF(fle.file)
      If read_parmrec(fle, temp) Then
        Select Case temp.pName
          Case section(0)  'Constituent
            For i = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                If temp.idx1 = siteIdx Then
                  Select Case LCase(temp.pName)
                    Case "fscasid":         addChemProp wID
                    Case "fscname":         addChemProp wName
                    Case "clktype":         addChemProp wType
                   End Select
                End If
              End If
            Next
          
          Case section(1), section(2), section(3), section(4), section(5)
            If section(1) = temp.pName Or _
               section(2) = temp.pName Then
              relative = "bbf"
            ElseIf section(5) = temp.pName Then
              relative = "scf"
            Else
              relative = "twi"
            End If
            For i = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                
                ' this should load all available selection values
                idx = idxs.Index(temp.pName)
                If idx > 0 Then
                  Set myvals = idxs.ItemIdx(idx)
                  idx = myvals.Index(temp.pval)
                  If idx = 0 Then myvals.Add temp.pval, temp.pval
                End If
                
                ' this should load all available selections
                idx = des.Index(temp.pName)
                If idx > 0 Then
                  InitSelection idx
                  On Error Resume Next
                  Set xnode = DoseView.Nodes.Add(relative, tvwChild, temp.pName, temp.pName)
                  On Error GoTo 0
                End If
                
                For j = 1 To idxs.Count
                  idx = des.Index(idxs.key(j) & temp.pName)
                  If idx > 0 Then
                    InitSelection idx
                    On Error Resume Next
                    Set xnode = DoseView.Nodes.Add(relative, tvwChild, temp.pName, temp.pName)
                    xnode.Expanded = True
                    If idxs.key(j) = idxs.ItemIdx(j) Then
                      DoseView.Nodes.Add temp.pName, tvwChild, idxs.key(j) & temp.pName, idxs.key(j) + " of concern"
                    End If
                    On Error GoTo 0
                  End If
                Next

                Select Case temp.pName
                  Case "LifeFormSci", "ScientificName":
                  addLifeProp wID
                  Case "LifeFormName", "CommonName":
                  addLifeProp wName

'  Commented out for 1.7 revision
'                  Case "NumLOELChem":
'                      If Val(temp.pval) > 0 Then
'                          loelcheck = True
'                      Else
'                          loelcheck = False
'                      End If
'                  Case "NumNOELChem":
'                      If Val(temp.pval) > 0 Then
'                          noelcheck = True
'                      Else
'                          noelcheck = False
'                      End If
'                  Case "NumLOEDChem":
'                      If Val(temp.pval) > 0 Then
'                          loedcheck = True
'                      Else
'                          loedcheck = False
'                      End If
'                  Case "NumNOEDChem":
'                      If Val(temp.pval) > 0 Then
'                          noedcheck = True
'                      Else
'                          noedcheck = False
'                      End If
'                  Case "LCDescript":        lc.Add temp.pval, "F" & temp.idx1
'                  Case "LDDescript":        ld.Add temp.pval, "F" & temp.idx1
'                  Case "ECDescript":        ec.Add temp.pval, "F" & temp.idx1
'                  Case "EDDescript":        ed.Add temp.pval, "F" & temp.idx1
'                  Case "Bodypart":
'                                            part.Add temp.pval, "FFF" & temp.idx1
'                                            ldpart.Add temp.pval, "FFF" & temp.idx1
'                                            ndpart.Add temp.pval, "FFF" & temp.idx1
'                  Case "Effect":
'                                            effect.Add temp.pval, "FFF" & temp.idx1
'                                            ldeffect.Add temp.pval, "FFF" & temp.idx1
'                                            ndeffect.Add temp.pval, "FFF" & temp.idx1
                End Select
              End If
            Next
          
'          Case section(5)  ' terrestrial soil benchmarks
'            For i = 1 To temp.idx1
'              If read_parmrec(fle, temp) Then
'                If temp.pname = "Jurisdiction" Then ssl.Add temp.pval, "F" & temp.idx1
'              End If
'            Next
'
'          Case section(4)  ' terrestrial wildlife benchmarks
'            For i = 1 To temp.idx1
'              If read_parmrec(fle, temp) Then
'                If temp.pname = "Jurisdiction" Then cct.Add temp.pval, "F" & temp.idx1
'              End If
'            Next
          
           Case Else
             For m = 1 To temp.idx1
               get_line fle.file
             Next
        End Select
      End If
    Loop
          
    ' now its ok to read the module section
    reset_csv fle.file
    Do Until EOCF(fle.file)
      If read_parmrec(fle, temp) Then
        Select Case temp.pName
          Case modName
            Loading.Gauge1.Max = Val(temp.idx1)
            Loading.Gauge1.value = 0
            For i = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                Loading.update
                
                'load keys
                idx = progidxs.Index(temp.pName)
                If idx > 0 Then
                  Set myvals = progidxs.ItemIdx(idx)
                  myvals.Add temp.pval, temp.pval
                End If
                
                'load single
                idx = des.Index(temp.pName)
                If idx > 0 Then SetSelection idx
                
                'load doubles
                For j = 1 To idxs.Count
                  idx = des.Index(idxs.key(j) + temp.pName)
                  If idx > 0 Then SetSelection idx
                Next
                  
                likey = "li" + CStr(temp.idx1)
                lokey = "lo" + CStr(temp.idx1) + CStr(temp.idx2)
                dokey = "lo" + CStr(temp.idx1) + CStr(temp.idx2) + CStr(temp.idx3)
                orgkey = "org" + CStr(temp.idx2)
                Select Case LCase(temp.pName)
                  Case "cvtformat":    CVTFormat = "General Number"
                  'get previous list
'                  Case "mybodypart":           mypart.Add part.IndexItem(temp.pval), temp.pval
'                  Case "myeffect":             myeffect.Add effect.IndexItem(temp.pval), temp.pval
'                  Case "myLDdescript":         myLDdescript.Add ld.IndexItem(temp.pval), temp.pval
'                  Case "myLCdescript":         myLCdescript.Add lc.IndexItem(temp.pval), temp.pval
'                  Case "myEDdescript":         myEDdescript.Add ed.IndexItem(temp.pval), temp.pval
'                  Case "myECdescript":         myECdescript.Add ec.IndexItem(temp.pval), temp.pval
'                  Case "myCCTJurisdiction":    myCCTJuris.Add cct.IndexItem(temp.pval), temp.pval
'                  Case "mySSLJurisdiction":    mySSLJuris.Add ssl.IndexItem(temp.pval), temp.pval
''aquatic
'                  Case "loe1":          If temp.idx1 = 0 Then dose(4).value = CInt(temp.pval)
'                  Case "loed":          If temp.idx1 = 0 Then dose(6).value = CInt(temp.pval)
'                  Case "noed":          If temp.idx1 = 0 Then dose(5).value = CInt(temp.pval)
'                  Case "noe1":          If temp.idx1 = 0 Then dose(7).value = CInt(temp.pval)
'                  'rememeber settings
'                  Case "ecpart", "edpart", "loelpart", "loedpart", "noelpart", "noedpart"
'                    If mypart.Count > 0 Then SelectList temp.pname, mypart.ItemIdx(temp.idx1), temp.pval
'                  Case "eceffect", "edeffect", "loeleffect", "loedeffect", "noedeffect"
'                    If myeffect.Count > 0 Then SelectList temp.pname, myeffect.ItemIdx(temp.idx1), temp.pval
'                  Case "lc"
'                    If temp.idx1 = 0 Then dose(0).value = CInt(temp.pval)
'                    If myLCdescript.Count > 0 And temp.idx1 > 0 Then SelectList temp.pname, myLCdescript.ItemIdx(temp.idx1), temp.pval
'                  Case "ld"
'                    If temp.idx1 = 0 Then dose(1).value = CInt(temp.pval)
'                    If myLDdescript.Count > 0 And temp.idx1 > 0 Then SelectList temp.pname, myLDdescript.ItemIdx(temp.idx1), temp.pval
'                  Case "ec"
'                    If temp.idx1 = 0 Then dose(2).value = CInt(temp.pval)
'                    If myECdescript.Count > 0 And temp.idx1 > 0 Then SelectList temp.pname, myECdescript.ItemIdx(temp.idx1), temp.pval
'                  Case "ed"
'                    If temp.idx1 = 0 Then dose(3).value = CInt(temp.pval)
'                    If myEDdescript.Count > 0 And temp.idx1 > 0 Then SelectList temp.pname, myEDdescript.ItemIdx(temp.idx1), temp.pval
'
''soil
'                  Case "ssl"
'                    If temp.idx1 = 0 Then dose(9).value = CInt(temp.pval)
'                    If mySSLJuris.Count > 0 And temp.idx1 > 0 Then SelectList temp.pname, mySSLJuris.ItemIdx(temp.idx1), temp.pval
'                  Case "cct"
'                    If temp.idx1 = 0 Then dose(10).value = CInt(temp.pval)
'                    If myCCTJuris.Count > 0 And temp.idx1 > 0 Then SelectList temp.pname, myCCTJuris.ItemIdx(temp.idx1), temp.pval
                  Case "user"
                      dose(8).value = Val(temp.pval)
                  
                  Case "uspecies"
                      Organism.Add New Organisms, orgkey
                      Organism(orgkey).id = temp.pval
                  Case "ucasid":
                      addTRV orgkey, lokey
                      With Organism(orgkey).TRVs(lokey)
                        .id = temp.pval
                        If .uuConc = "" Then
                          If Val(temp.pval) = 0 Then
                            .uuConc = "pCi/kg"
                            .muConc = "pCi/kg"
                          Else
                            .uuConc = "mg/kg"
                            .muConc = "mg/kg"
                          End If
                          .uuDur = "day"
                          .muDur = "day"
                        End If
                      End With
                  Case "userchem"
                      addTRVSeries orgkey, lokey, dokey
                      With Organism(orgkey).TRVs(lokey)
                        .uuConc = temp.uunit
                        .muConc = temp.cunit
                        .ref = temp.ref
                        .item(temp.idx3).v1 = convert(temp.cunit, temp.uunit, Val(temp.pval))
                      End With
                  Case "userchemdur"
                      addTRVSeries orgkey, lokey, dokey
                      With Organism(orgkey).TRVs(lokey)
                        .uuDur = temp.uunit
                        .muDur = temp.cunit
                        .ref = temp.ref
                        .item(temp.idx3).v2 = convert(temp.cunit, temp.uunit, Val(temp.pval))
                      End With
                  Case "userdescript"
                      addTRVSeries orgkey, lokey, dokey
                      With Organism(orgkey).TRVs(lokey)
                        .ref = temp.ref
                        .item(temp.idx3).v3 = temp.pval
                      End With
                  Case "species"
                      LifeForm.Add New LifeForms, likey
                      LifeForm(likey).id = temp.pval
                  Case "locid"
                      If temp.idx1 > mynumloc Then
                        mynumloc = mynumloc + 1
                        ReDim Preserve locidx(temp.idx1) As Long
                      End If
                      locidx(temp.idx1) = GetTypedIdIndex(temp.pval, "wcf")
                  Case "casid":
                      addDose likey, lokey
                      With LifeForm(likey).Doses(lokey)
                        .id = temp.pval
                        If .uuConc = "" Then
                          If Val(temp.pval) = 0 Then
                            .uuConc = "pCi/ml"
                            .muConc = "pCi/ml"
                            .uuCCC = "pCi/ml"
                            .muCCC = "pCi/ml"
                          Else
                            .uuConc = "g/ml"
                            .muConc = "g/ml"
                            .uuCCC = "g/ml"
                            .muCCC = "g/ml"
                          End If
                          .uuDur = "yr"
                          .muDur = "yr"
                          .uuAcute = "yr"
                          .muAcute = "yr"
                        End If
                      End With
                  Case "time"
                      addLoc likey
                      With LifeForm(likey).Locs
                        .uuTime = temp.uunit
                        .muTime = temp.cunit
                        .rLoc = temp.ref
                        If .Count < temp.idx2 Then
                          .Add Val(convert(temp.cunit, temp.uunit, Val(temp.pval))), 0, CStr(temp.idx2)
                        Else
                          .item(CStr(temp.idx2)).v1 = convert(temp.cunit, temp.uunit, Val(temp.pval))
                        End If
                      End With
                  Case "value"
                      addLoc likey
                      With LifeForm(likey).Locs
                        .uuFreq = temp.uunit
                        .muFreq = temp.cunit
                        .rLoc = temp.ref
                        If .Count < temp.idx2 Then
                          .Add 0, Val(temp.pval), CStr(temp.idx2)
                        Else
                          .item(CStr(temp.idx2)).v2 = Val(temp.pval)
                        End If
                      End With
                  Case "deslc"
                      addDose likey, lokey
                      LifeForm(likey).Doses(lokey).des = temp.pval
                  Case "acutetime"
                      addDose likey, lokey
                      With LifeForm(likey).Doses(lokey)
                        .uuAcute = temp.uunit
                        .muAcute = temp.cunit
                        .rAcute = temp.ref
                        .Acute = convert(temp.cunit, temp.uunit, Val(temp.pval))
                      End With
                  Case "cccvalue"
                      addDose likey, lokey
                      With LifeForm(likey).Doses(lokey)
                        .uuCCC = temp.uunit
                        .muCCC = temp.cunit
                        .rCCC = temp.ref
                        .CCC = convert(temp.cunit, temp.uunit, Val(temp.pval))
                      End With
                  Case "lcdur"
                      addDoseSeries likey, lokey, dokey
                      With LifeForm(likey).Doses(lokey)
                        .uuDur = temp.uunit
                        .muDur = temp.cunit
                        .rDose = temp.ref
                        .item(temp.idx3).v1 = convert(temp.cunit, temp.uunit, Val(temp.pval))
                      End With
                  Case "lcconc"
                      addDoseSeries likey, lokey, dokey
                      With LifeForm(likey).Doses(lokey)
                        .uuConc = temp.uunit
                        .muConc = temp.cunit
                        .rDose = temp.ref
                        .item(temp.idx3).v2 = convert(temp.cunit, temp.uunit, Val(temp.pval))
                      End With
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
  
    For i = 1 To numloc
      If InStr(loc(wType, i), "wcf") Then loccnt = loccnt + 1  'wcf:surface water input
      If InStr(loc(wType, i), "bbf") Then ecocnt = ecocnt + 1  'bbf:body burden input
      If InStr(loc(wType, i), "scf") Then scfcnt = scfcnt + 1  'scf:soil input
      If InStr(loc(wType, i), "twi") Then twicnt = twicnt + 1  'twi:terrestrial wildlife intakes input
    Next
    
    If ecocnt + loccnt + scfcnt + twicnt < 1 Then
      PutError "No valid concentration input!"
      EndModule
    End If
    
    If scfcnt = 0 Then DoseView.Nodes.Remove "scf"
    
    'need water concentrations for aquatic life assessment
    If loccnt > 0 Then
      Set xnode = DoseView.Nodes.Add(, , "wcf", "Aquatic Media Concentration Assessment")
      xnode.Expanded = True
    
      'resolve species differences               if species no longer exists its index is 0
      For i = 1 To numlife
        Set xnode = DoseView.Nodes.Add("wcf", tvwChild, "l_" + life(wID, i), life(wName, i))
        For j = 1 To LifeForm.Count
          If LifeForm(j).id = life(wID, i) Then
            LifeForm(j).idx = i
            Exit For
          End If
        Next
        If j > LifeForm.Count Then
          LifeForm.Add New LifeForms
          LifeForm(j).id = life(wID, i)
          LifeForm(j).idx = i
          LifeForm(j).Locs.uuTime = "yr"
          LifeForm(j).Locs.muTime = "yr"
          LifeForm(j).Locs.uuFreq = "N/A"
          LifeForm(j).Locs.muFreq = "N/A"
        End If
        xnode.Tag = CStr(j) + "_"
      Next
  
      For k = 1 To LifeForm.Count
        If LifeForm(k).idx > 0 Then
          LifeForm(k).Locs.Remap locidx
          If LifeForm(k).Locs.uuTime = "" Then
            LifeForm(k).Locs.uuTime = "yr"
            LifeForm(k).Locs.muTime = "yr"
            LifeForm(k).Locs.uuFreq = "N/A"
            LifeForm(k).Locs.muFreq = "N/A"
          End If
        End If
      Next
    
      'resolve contaminate differences            if contaminate no longer exists its index is 0
      For k = 1 To LifeForm.Count
        For i = 1 To numchem
          If LifeForm(k).idx > 0 Then
            Set xnode = DoseView.Nodes.Add("l_" + life(wID, LifeForm(k).idx), tvwChild, "c_" + life(wID, LifeForm(k).idx) + chem(wID, i), chem(wName, i))
          End If
          For j = 1 To LifeForm(k).Doses.Count
            If chem(wID, i) = LifeForm(k).Doses(j).id Then
              LifeForm(k).Doses(j).idx = i
              Exit For
           End If
          Next
          If j > LifeForm(k).Doses.Count Then
            LifeForm(k).Doses.Add New dose
            LifeForm(k).Doses(j).idx = i
            LifeForm(k).Doses(j).id = chem(wID, i)
            If Val(chem(wType, i)) = 1 Then
              LifeForm(k).Doses(j).uuConc = "pCi/ml"
              LifeForm(k).Doses(j).muConc = "pCi/ml"
              LifeForm(k).Doses(j).uuCCC = "pCi/ml"
              LifeForm(k).Doses(j).muCCC = "pCi/ml"
            Else
              LifeForm(k).Doses(j).uuConc = "g/ml"
              LifeForm(k).Doses(j).muConc = "g/ml"
              LifeForm(k).Doses(j).uuCCC = "g/ml"
              LifeForm(k).Doses(j).muCCC = "g/ml"
            End If
            LifeForm(k).Doses(j).uuDur = "yr"
            LifeForm(k).Doses(j).muDur = "yr"
            LifeForm(k).Doses(j).uuAcute = "yr"
            LifeForm(k).Doses(j).muAcute = "yr"
          End If
          xnode.Tag = CStr(k) + "_" + CStr(j)
        Next
      Next
    End If

   ' need body burdens to do any trv calcs
    If ecocnt + twicnt > 0 Then
      'resolve species differences               if species no longer exists its index is 0
      For i = 1 To numlife
        Combo1(1).AddItem life(wName, i) & " (" & life(wID, i) & ")"
        For j = 1 To Organism.Count
          If Organism(j).id = life(wID, i) Then
            Organism(j).idx = i
            Combo1(1).ItemData(Combo1(1).NewIndex) = j
            Exit For
          End If
        Next
        If j > Organism.Count Then
          Organism.Add New Organisms
          Organism(j).id = life(wID, i)
          Organism(j).idx = i
          Combo1(1).ItemData(Combo1(1).NewIndex) = j
        End If
      Next
  
      'resolve contaminate differences            if contaminate no longer exists its index is 0
      For k = 1 To Organism.Count
        For i = 1 To numchem
          If k = 1 Then Combo1(2).AddItem chem(wName, i) & " (" & chem(wID, i) & ")"
          For j = 1 To Organism(k).TRVs.Count
            If chem(wID, i) = Organism(k).TRVs(j).id Then
              Organism(k).TRVs(j).idx = i
              Exit For
            End If
          Next
          If j > Organism(k).TRVs.Count Then
            Organism(k).TRVs.Add New TRV
            Organism(k).TRVs(j).idx = i
            Organism(k).TRVs(j).id = chem(wID, i)
            If Val(chem(wType, i)) = 1 Then
              Organism(k).TRVs(j).uuConc = "pCi/kg"
              Organism(k).TRVs(j).muConc = "pCi/kg"
            Else
              Organism(k).TRVs(j).uuConc = "mg/kg"
              Organism(k).TRVs(j).muConc = "mg/kg"
            End If
            Organism(k).TRVs(j).uuDur = "day"
            Organism(k).TRVs(j).muDur = "day"
          End If
        Next
      Next
      uorgprev = -1
      uchemprev = -1
      loadng = False
      If Combo1(1).ListCount > 0 Then Combo1(1).ListIndex = 0
      If Combo1(2).ListCount > 0 Then Combo1(2).ListIndex = 0
      If ecocnt = 0 Then DoseView.Nodes.Remove "bbf"
      If twicnt = 0 Then DoseView.Nodes.Remove "twi"
      loadng = True
    Else
      DoseView.Nodes.Remove "bbf"
      DoseView.Nodes.Remove "twi"
    End If
  
  Else
    PutError "Can't find or open file " & FUIName
    EndModule
  End If
End Sub

Private Sub Form_load()
  
  StartModule Weap, App.Title, 5
  SetRefFile ReplaceExt(FUIName, "ref")
  loadng = True
  Loading.Show
  loadprm
'  If ssl.Count < 1 Then
'    dose(9).Enabled = False
'    dose(9).value = 0
'  End If
'  If cct.Count < 1 Then
'    dose(10).Enabled = False
'    dose(10).value = 0
'  End If
'  If lc.Count < 1 Then
'    dose(0).Enabled = False
'    dose(0).value = 0
'  End If
'  If ld.Count < 1 Then
'    dose(1).Enabled = False
'    dose(1).value = 0
'  End If
'  If ec.Count < 1 Then
'    dose(2).Enabled = False
'    dose(2).value = 0
'  End If
'  If ed.Count < 1 Then
'    dose(3).Enabled = False
'    dose(3).value = 0
'  End If
'  If part.Count < 1 Or effect.Count < 1 Then
'    dose(2).Enabled = False
'    dose(3).Enabled = False
'    dose(4).Enabled = False
'    dose(6).Enabled = False
'    dose(2).value = 0
'    dose(3).value = 0
'    dose(4).value = 0
'    dose(6).value = 0
'  End If
'  If loelcheck = False Then
'    dose(4).Enabled = False
'    dose(4).value = 0
'  End If
'  If noelcheck = False Then
'    dose(7).Enabled = False
'    dose(7).value = 0
'  End If
'  If loedcheck = False Then
'    dose(6).Enabled = False
'    dose(6).value = 0
'  End If
'  If noedcheck = False Then
'    dose(5).Enabled = False
'    dose(5).value = 0
'  End If
  Unload Loading
  loadng = False
  DoseView_NodeClick DoseView.Nodes.item(1)
End Sub

'Private Sub SelectList(Key As String, idx As Long, sKey As String)
'  Select Case Key
'  Case "ssl":        ssl.NewKey idx, sKey + CStr(idx)
'  Case "cct":        cct.NewKey idx, sKey + CStr(idx)
'  Case "lc":         lc.NewKey idx, sKey + CStr(idx)
'  Case "ld":         ld.NewKey idx, sKey + CStr(idx)
'  Case "ec":         ec.NewKey idx, sKey + CStr(idx)
'  Case "ed":         ed.NewKey idx, sKey + CStr(idx)
'  Case "ecpart":     part.NewKey idx, sKey + Mid(part.Key(idx), 2)
'  Case "edpart":     part.NewKey idx, Mid(part.Key(idx), 1, 1) + sKey + Mid(part.Key(idx), 3)
'  Case "loelpart":   part.NewKey idx, Mid(part.Key(idx), 1, 2) + sKey + Mid(part.Key(idx), 4)
'  Case "loedpart":   ldpart.NewKey idx, Mid(ldpart.Key(idx), 1, 2) + sKey + Mid(ldpart.Key(idx), 4)
'  Case "noedpart":   ndpart.NewKey idx, Mid(ndpart.Key(idx), 1, 2) + sKey + Mid(ndpart.Key(idx), 4)
'  Case "eceffect":   effect.NewKey idx, sKey + Mid(effect.Key(idx), 2)
'  Case "edeffect":   effect.NewKey idx, Mid(effect.Key(idx), 1, 1) + sKey + Mid(effect.Key(idx), 3)
'  Case "loeleffect": effect.NewKey idx, Mid(effect.Key(idx), 1, 2) + sKey + Mid(effect.Key(idx), 4)
'  Case "loedeffect": ldeffect.NewKey idx, Mid(ldeffect.Key(idx), 1, 2) + sKey + Mid(ldeffect.Key(idx), 4)
'  Case "noedeffect": ndeffect.NewKey idx, Mid(ndeffect.Key(idx), 1, 2) + sKey + Mid(ndeffect.Key(idx), 4)
'  End Select
'End Sub

Private Sub List1_Click()
Dim idx As Long
Dim sKey As String
Dim sel As SeriesPair
Dim myvals As MyCollection

  If loadng Then Exit Sub
  sKey = "F"
  idx = List1.ListIndex + 1
  If List1.Selected(List1.ListIndex) Then sKey = "T"
  
  Set sel = des.ItemKey(DoseView.SelectedItem.key)
  Set myvals = idxs.ItemKey(sel.v1)
  If Len(sel.v2) < idx Then
    sel.v2 = sel.v2 + String(idx - Len(sel.v2) - 1, "F") + sKey
  Else
    sel.v2 = Left$(sel.v2, idx - 1) + sKey + Right$(sel.v2, Len(sel.v2) - idx)
  End If
 
'  SelectList DoseView.SelectedItem.Key, idx, sKey
End Sub

Private Sub WriteLabels()
Dim i As Long
Dim j As Long

'write out lablels for index resolution, subsequent times through he UI

  For i = 1 To numloc
    If loc(wType, i) = "wcf" Then
      j = j + 1
      set_parm parm, "locid", j, 0, 0, 0, 0, 0, 0, "N/A", "N/A", loc(wID, i)
      write_parmrec fle, parm
    End If
  Next

'  For i = 1 To lc.Count
'    set_parm parm, "myLCdescript", i, 0, 0, 0, 0, 0, 0, "", "", lc.ItemKey(lc.Key(i))
'    write_parmrec fle, parm
'  Next
'  For i = 1 To ld.Count
'    set_parm parm, "myLDdescript", i, 0, 0, 0, 0, 0, 0, "", "", ld.ItemKey(ld.Key(i))
'    write_parmrec fle, parm
'  Next
'  For i = 1 To ec.Count
'    set_parm parm, "myECdescript", i, 0, 0, 0, 0, 0, 0, "", "", ec.ItemKey(ec.Key(i))
'    write_parmrec fle, parm
'  Next
'  For i = 1 To ed.Count
'    set_parm parm, "myEDdescript", i, 0, 0, 0, 0, 0, 0, "", "", ed.ItemKey(ed.Key(i))
'    write_parmrec fle, parm
'  Next
'
'  For i = 1 To part.Count
'    set_parm parm, "myBodypart", i, 0, 0, 0, 0, 0, 0, "", "", part.ItemKey(part.Key(i))
'    write_parmrec fle, parm
'  Next
'  For i = 1 To effect.Count
'    set_parm parm, "myEffect", i, 0, 0, 0, 0, 0, 0, "", "", effect.ItemKey(effect.Key(i))
'    write_parmrec fle, parm
'  Next
'  For i = 1 To cct.Count
'    set_parm parm, "myCCTJurisdiction", i, 0, 0, 0, 0, 0, 0, "", "", cct.ItemKey(cct.Key(i))
'    write_parmrec fle, parm
'  Next
'  For i = 1 To ssl.Count
'    set_parm parm, "mySSLJurisdiction", i, 0, 0, 0, 0, 0, 0, "", "", ssl.ItemKey(ssl.Key(i))
'    write_parmrec fle, parm
'  Next
'End Sub
'
'Private Sub WriteAttribute(idx As Integer, idx2 As Integer, ldcheck As Integer, Optional c1 As MyCollection = Nothing)
'Dim i As Long
'
'  set_parm parm, dose(idx).Tag, 0, 0, 0, 0, 0, 0, 0, "", "", CStr(dose(idx).value)
'  write_parmrec fle, parm
'  If dose(idx).value = 1 Then
'    If Not c1 Is Nothing Then
'      For i = 1 To c1.Count
'        set_parm parm, dose(idx).Tag, CLng(Right(c1.Key(i), Len(c1.Key(i)) - 1)), 0, 0, 0, 0, 0, 0, "", "", Left(c1.Key(i), 1)
'        write_parmrec fle, parm
'      Next
'    End If
'    If idx2 > 0 Then
'      If ldcheck = 1 Then
'        For i = 1 To ldpart.Count
'          set_parm parm, dose(idx).Tag + "part", CLng(Right(ldpart.Key(i), Len(ldpart.Key(i)) - 3)), 0, 0, 0, 0, 0, 0, "", "", Mid(ldpart.Key(i), idx2, 1)
'          write_parmrec fle, parm
'        Next
'        For i = 1 To ldeffect.Count
'          set_parm parm, dose(idx).Tag + "effect", CLng(Right(ldeffect.Key(i), Len(ldeffect.Key(i)) - 3)), 0, 0, 0, 0, 0, 0, "", "", Mid(ldeffect.Key(i), idx2, 1)
'          write_parmrec fle, parm
'        Next
'      ElseIf ldcheck = 2 Then
'        For i = 1 To ndpart.Count
'          set_parm parm, dose(idx).Tag + "part", CLng(Right(ndpart.Key(i), Len(ndpart.Key(i)) - 3)), 0, 0, 0, 0, 0, 0, "", "", Mid(ndpart.Key(i), idx2, 1)
'          write_parmrec fle, parm
'        Next
'        For i = 1 To ndeffect.Count
'          set_parm parm, dose(idx).Tag + "effect", CLng(Right(ndeffect.Key(i), Len(ndeffect.Key(i)) - 3)), 0, 0, 0, 0, 0, 0, "", "", Mid(ndeffect.Key(i), idx2, 1)
'          write_parmrec fle, parm
'        Next
'      Else
'        For i = 1 To part.Count
'          set_parm parm, dose(idx).Tag + "part", CLng(Right(part.Key(i), Len(part.Key(i)) - 3)), 0, 0, 0, 0, 0, 0, "", "", Mid(part.Key(i), idx2, 1)
'          write_parmrec fle, parm
'        Next
'        For i = 1 To effect.Count
'          set_parm parm, dose(idx).Tag + "effect", CLng(Right(effect.Key(i), Len(effect.Key(i)) - 3)), 0, 0, 0, 0, 0, 0, "", "", Mid(effect.Key(i), idx2, 1)
'          write_parmrec fle, parm
'        Next
'      End If
'    End If
'  End If
End Sub

Private Sub save_Click()
  Dim i As Long
  Dim j As Long
  Dim fname As String
  Dim UserFormat As String
  Dim sel As SeriesPair
  Dim myvals As MyCollection
  UserFormat = CVTFormat
  
  DoseView_NodeClick DoseView.SelectedItem
  Combo1_Click 1
  fname = RunName & ".GID"
  If open_parm(fle, fname, 1) Then
    set_parm parm, "CVTFormat", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CVTFormat
    write_parmrec fle, parm
    CVTFormat = "General Number"
    
    WriteLabels
    For i = 1 To idxs.Count
      Set myvals = idxs.ItemIdx(i)
      For j = 1 To myvals.Count
        set_parm parm, idxs.key(i), j, 0, 0, 0, 0, 0, 0, "", "", myvals.ItemIdx(j)
        write_parmrec fle, parm
      Next
    Next
    
    For i = 1 To des.Count
      Set sel = des.ItemIdx(i)
      For j = 1 To Len(sel.v2)
        set_parm parm, des.key(i), j, 0, 0, 0, 0, 0, 0, "", "", Mid(sel.v2, j, 1)
        write_parmrec fle, parm
      Next
    Next
'    WriteAttribute 0, 0, 0, lc
'    WriteAttribute 1, 0, 0, ld
'    WriteAttribute 2, 1, 0, ec
'    WriteAttribute 3, 2, 0, ed
'    WriteAttribute 4, 3, 0          'loel
'    WriteAttribute 5, 3, 2          'noed
'    WriteAttribute 6, 3, 1          'loed
'    WriteAttribute 7, 0, 1          'noel
'    WriteAttribute 9, 0, 0, ssl
'    WriteAttribute 10, 0, 0, cct
    
    set_parm parm, "numtwis", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CStr(twicnt)
    write_parmrec fle, parm
    set_parm parm, "numscfs", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CStr(scfcnt)
    write_parmrec fle, parm
    set_parm parm, "numecos", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CStr(ecocnt)
    write_parmrec fle, parm
    set_parm parm, "numlocs", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CStr(loccnt)
    write_parmrec fle, parm
    set_parm parm, "numlife", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CStr(numlife)
    write_parmrec fle, parm
    
    If loccnt > 0 Then
      For i = 1 To LifeForm.Count
        If LifeForm(i).idx > 0 Then
          LifeForm(i).WriteGIDEntries
        End If
      Next
    End If
    
    set_parm parm, dose(8).Tag, 0, 0, 0, 0, 0, 0, 0, "", "", CStr(dose(8).value)
    write_parmrec fle, parm
    If dose(8).value = 1 Then
      For i = 1 To Organism.Count
        If Organism(i).idx > 0 Then
          Organism(i).WriteGIDEntries
        End If
      Next
    End If
   
    close_parm fle
    CVTFormat = UserFormat
    SetFormat Me
  Else
    PutError "Unable to create transaction file" & RunName & ".GID"
  End If
  EndModule
End Sub

Private Sub dose_GotFocus(Index As Integer)
  noact.Enabled = False
  RefItem = -1
End Sub

Private Sub DoseView_GotFocus()
  noact.Enabled = False
  RefItem = -1
End Sub

Private Sub List1_GotFocus()
  noact.Enabled = False
  RefItem = -1
End Sub

Private Sub Text1_GotFocus()
  noact.Enabled = True
  RefItem = 1
End Sub

Private Sub txt_Gotfocus(Index As Integer)
  noact.Enabled = True
  RefItem = Index
End Sub

Private Sub unit_Gotfocus(Index As Integer)
  noact.Enabled = True
  RefItem = Index
End Sub

Private Sub vaSpread1_GotFocus()
  noact.Enabled = True
  RefItem = 1
End Sub

Private Sub vaSpread2_GotFocus()
  noact.Enabled = True
  RefItem = 3
End Sub

Private Sub Combo1_gotfocus(Index As Integer)
  noact.Enabled = False
  RefItem = -1
End Sub

Private Sub vaSpread3_GotFocus()
  noact.Enabled = True
  RefItem = 4
End Sub

Private Sub vaSpread2_KeyUp(KeyCode As Integer, Shift As Integer)
  Dim v As Variant
  vaSpread2.GetText vaSpread2.ActiveCol, vaSpread2.ActiveRow, v
  If vaSpread2.ActiveCol = 1 And v <> "" Then
    vaSpread2.Row = vaSpread2.ActiveRow
    vaSpread2.col = 2
    If vaSpread2.Text = "" Then
      SetSpreadCombo vaSpread2, 2, vaSpread2.Row, loc, numloc, "wcf"
      vaSpread2.Text = 0
    End If
    Exit Sub
  End If
  If vaSpread2.ActiveCol = 1 And v = "" Then
    vaSpread2.Row = vaSpread2.ActiveRow
    vaSpread2.col = 2
    vaSpread2.CellType = 1
    vaSpread2.Text = ""
  End If
End Sub
