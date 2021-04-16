VERSION 5.00
Object = "{6B7E6392-850A-101B-AFC0-4210102A8DA7}#1.3#0"; "comctl32.ocx"
Object = "{F856EC8B-F03C-4515-BDC6-64CBD617566A}#7.0#0"; "FPSPR70.ocx"
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "tabctl32.ocx"
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "threed32.ocx"
Begin VB.Form Known 
   BorderStyle     =   1  'Fixed Single
   ClientHeight    =   7164
   ClientLeft      =   4728
   ClientTop       =   4356
   ClientWidth     =   9660
   Icon            =   "Known.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   597
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   805
   StartUpPosition =   2  'CenterScreen
   Begin Threed.SSFrame SSFrame3 
      Height          =   6612
      Left            =   240
      TabIndex        =   43
      Top             =   360
      Visible         =   0   'False
      Width           =   9252
      _Version        =   65536
      _ExtentX        =   16319
      _ExtentY        =   11663
      _StockProps     =   14
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   9.6
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ShadowStyle     =   1
      Enabled         =   0   'False
      Begin ComctlLib.TreeView treeGrp 
         Height          =   2292
         Left            =   480
         TabIndex        =   49
         Top             =   1680
         Width           =   3252
         _ExtentX        =   5736
         _ExtentY        =   4043
         _Version        =   327682
         Style           =   6
         Appearance      =   1
      End
      Begin VB.TextBox txt 
         Alignment       =   1  'Right Justify
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
         Index           =   8
         Left            =   4320
         TabIndex        =   44
         Tag             =   "irMult"
         Text            =   "1.0"
         Top             =   480
         Width           =   1000
      End
      Begin FPSpreadADO.fpSpread vaSpread3 
         Height          =   2976
         Left            =   4320
         TabIndex        =   47
         Tag             =   "constituent"
         Top             =   1080
         Width           =   3516
         _Version        =   458752
         _ExtentX        =   6202
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
         MaxRows         =   12
         RowsFrozen      =   1
         ScrollBarExtMode=   -1  'True
         ScrollBars      =   0
         ScrollBarShowMax=   0   'False
         SpreadDesigner  =   "Known.frx":030A
         StartingColNumber=   0
         StartingRowNumber=   0
         VisibleCols     =   2
         VisibleRows     =   12
      End
      Begin VB.Label lbl 
         Caption         =   "Chemical Grouping Multipliers"
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
         Index           =   10
         Left            =   360
         TabIndex        =   48
         Tag             =   "one"
         Top             =   1080
         Width           =   3500
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
         Index           =   8
         Left            =   5520
         TabIndex        =   46
         Tag             =   "0"
         Top             =   480
         Width           =   996
      End
      Begin VB.Label lbl 
         Caption         =   "Inventory/Release Multiplier"
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
         Index           =   8
         Left            =   360
         TabIndex        =   45
         Tag             =   "invMult"
         Top             =   480
         Width           =   3500
      End
   End
   Begin TabDlg.SSTab SSTab1 
      Height          =   7176
      Left            =   0
      TabIndex        =   42
      Tag             =   "top"
      Top             =   0
      Width           =   9660
      _ExtentX        =   17039
      _ExtentY        =   12658
      _Version        =   393216
      Style           =   1
      Tabs            =   1
      TabsPerRow      =   2
      TabHeight       =   423
      TabCaption(0)   =   "BC Tab"
      TabPicture(0)   =   "Known.frx":0872
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).ControlCount=   0
   End
   Begin Threed.SSFrame SSFrame1 
      Height          =   6612
      Left            =   240
      TabIndex        =   40
      Top             =   360
      Width           =   9252
      _Version        =   65536
      _ExtentX        =   16325
      _ExtentY        =   11668
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
         Index           =   0
         Left            =   5280
         TabIndex        =   4
         Tag             =   "one"
         Top             =   660
         Width           =   1000
      End
      Begin VB.TextBox txt 
         Alignment       =   1  'Right Justify
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
         Index           =   1
         Left            =   5280
         TabIndex        =   8
         Tag             =   "two"
         Top             =   1020
         Width           =   1000
      End
      Begin VB.TextBox txt 
         Alignment       =   1  'Right Justify
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
         Index           =   2
         Left            =   5280
         TabIndex        =   12
         Tag             =   "three"
         Top             =   1380
         Width           =   1000
      End
      Begin VB.TextBox txt 
         Alignment       =   1  'Right Justify
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
         Index           =   3
         Left            =   5280
         TabIndex        =   16
         Tag             =   "four"
         Top             =   1740
         Width           =   1000
      End
      Begin VB.ComboBox unit 
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
         Index           =   0
         Left            =   6240
         Style           =   2  'Dropdown List
         TabIndex        =   5
         Tag             =   "m"
         Top             =   660
         Width           =   1000
      End
      Begin VB.ComboBox unit 
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
         Index           =   1
         Left            =   6240
         Style           =   2  'Dropdown List
         TabIndex        =   9
         Tag             =   "m"
         Top             =   1020
         Width           =   1000
      End
      Begin VB.ComboBox unit 
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
         Index           =   2
         Left            =   6240
         Style           =   2  'Dropdown List
         TabIndex        =   13
         Tag             =   "m"
         Top             =   1380
         Width           =   1000
      End
      Begin VB.ComboBox unit 
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
         Index           =   3
         Left            =   6240
         Style           =   2  'Dropdown List
         TabIndex        =   17
         Tag             =   "m^3/yr"
         Top             =   1740
         Width           =   1000
      End
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
         Height          =   288
         ItemData        =   "Known.frx":088E
         Left            =   5280
         List            =   "Known.frx":0890
         Style           =   2  'Dropdown List
         TabIndex        =   1
         Tag             =   "beginning"
         Top             =   300
         Width           =   1970
      End
      Begin VB.TextBox txt 
         Alignment       =   1  'Right Justify
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
         Index           =   4
         Left            =   5280
         TabIndex        =   20
         Tag             =   "five"
         Top             =   2100
         Width           =   1000
      End
      Begin VB.ComboBox unit 
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
         Index           =   4
         Left            =   6240
         Style           =   2  'Dropdown List
         TabIndex        =   21
         Tag             =   "m^3/yr"
         Top             =   2100
         Width           =   1000
      End
      Begin VB.TextBox txt 
         Alignment       =   1  'Right Justify
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   300
         Index           =   5
         Left            =   5280
         TabIndex        =   24
         Tag             =   "six"
         Top             =   2460
         Width           =   1000
      End
      Begin VB.ComboBox unit 
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
         Index           =   5
         Left            =   6240
         Style           =   2  'Dropdown List
         TabIndex        =   25
         Tag             =   "m^3/yr"
         Top             =   2460
         Width           =   1000
      End
      Begin Threed.SSCommand SSCommand5 
         Height          =   312
         Left            =   7320
         TabIndex        =   2
         Tag             =   "flux"
         Top             =   300
         Width           =   1092
         _Version        =   65536
         _ExtentX        =   1926
         _ExtentY        =   550
         _StockProps     =   78
         Caption         =   "Flux Types"
         ForeColor       =   0
      End
      Begin Threed.SSPanel SSFrame2 
         Height          =   3495
         Left            =   120
         TabIndex        =   39
         Top             =   3000
         Width           =   9015
         _Version        =   65536
         _ExtentX        =   15901
         _ExtentY        =   6165
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
         BevelOuter      =   0
         Begin FPSpreadADO.fpSpread vaSpread1 
            Height          =   1416
            Left            =   240
            TabIndex        =   32
            Tag             =   "constituent"
            Top             =   840
            Width           =   2796
            _Version        =   458752
            _ExtentX        =   4932
            _ExtentY        =   4064
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
            MaxCols         =   5
            MaxRows         =   5000
            RowsFrozen      =   1
            ScrollBarExtMode=   -1  'True
            ScrollBarShowMax=   0   'False
            SpreadDesigner  =   "Known.frx":0892
            StartingColNumber=   0
            StartingRowNumber=   0
            VisibleCols     =   2
            VisibleRows     =   8
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
            Height          =   288
            ItemData        =   "Known.frx":10AD0
            Left            =   240
            List            =   "Known.frx":10AD2
            Style           =   2  'Dropdown List
            TabIndex        =   28
            Tag             =   "parent"
            Top             =   430
            Width           =   2295
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
            Left            =   4560
            Style           =   2  'Dropdown List
            TabIndex        =   34
            Tag             =   "parent"
            Top             =   430
            Width           =   2175
         End
         Begin FPSpreadADO.fpSpread vaSpread2 
            Height          =   1416
            Left            =   4584
            TabIndex        =   38
            Tag             =   "constituent"
            Top             =   840
            Width           =   2808
            _Version        =   458752
            _ExtentX        =   4953
            _ExtentY        =   4064
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
            MaxCols         =   5
            MaxRows         =   5000
            RowsFrozen      =   1
            ScrollBarExtMode=   -1  'True
            ScrollBarShowMax=   0   'False
            SpreadDesigner  =   "Known.frx":10AD4
            StartingColNumber=   0
            StartingRowNumber=   0
            UserResize      =   1
            VisibleCols     =   2
            VisibleRows     =   8
         End
         Begin Threed.SSCommand SSCommand4 
            Height          =   315
            Left            =   7200
            TabIndex        =   36
            Tag             =   "parent"
            Top             =   435
            Width           =   405
            _Version        =   65536
            _ExtentX        =   706
            _ExtentY        =   564
            _StockProps     =   78
            Caption         =   ">>"
         End
         Begin Threed.SSCommand SSCommand3 
            Height          =   315
            Left            =   6840
            TabIndex        =   35
            Tag             =   "parent"
            Top             =   435
            Width           =   405
            _Version        =   65536
            _ExtentX        =   706
            _ExtentY        =   564
            _StockProps     =   78
            Caption         =   "<<"
         End
         Begin Threed.SSCommand SSCommand2 
            Height          =   315
            Left            =   3000
            TabIndex        =   30
            Tag             =   "parent"
            Top             =   435
            Width           =   405
            _Version        =   65536
            _ExtentX        =   706
            _ExtentY        =   564
            _StockProps     =   78
            Caption         =   ">>"
         End
         Begin Threed.SSCommand SSCommand1 
            Height          =   315
            Left            =   2640
            TabIndex        =   29
            Tag             =   "parent"
            Top             =   435
            Width           =   405
            _Version        =   65536
            _ExtentX        =   706
            _ExtentY        =   564
            _StockProps     =   78
            Caption         =   "<<"
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
            Index           =   6
            Left            =   240
            TabIndex        =   27
            Top             =   120
            Width           =   2415
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
            Left            =   4560
            TabIndex        =   33
            Top             =   120
            Width           =   2415
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
            Left            =   3600
            TabIndex        =   31
            Tag             =   "0"
            Top             =   480
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
            Left            =   7800
            TabIndex        =   37
            Tag             =   "0"
            Top             =   480
            Width           =   735
         End
      End
      Begin VB.Label lbl 
         Caption         =   "lb0"
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
         Left            =   315
         TabIndex        =   3
         Tag             =   "one"
         Top             =   660
         Width           =   4500
      End
      Begin VB.Label lbl 
         Caption         =   "lb1"
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
         Left            =   300
         TabIndex        =   7
         Tag             =   "two"
         Top             =   1020
         Width           =   4500
      End
      Begin VB.Label lbl 
         Caption         =   "lb2"
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
         Index           =   2
         Left            =   300
         TabIndex        =   11
         Tag             =   "three"
         Top             =   1380
         Width           =   4500
      End
      Begin VB.Label lbl 
         Caption         =   "lb3"
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
         Index           =   3
         Left            =   300
         TabIndex        =   15
         Tag             =   "four"
         Top             =   1740
         Width           =   4500
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
         TabIndex        =   41
         Top             =   14280
         Width           =   2412
      End
      Begin VB.Label lbl 
         Caption         =   "lbl9"
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
         Index           =   9
         Left            =   300
         TabIndex        =   0
         Top             =   300
         Width           =   4500
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
         Index           =   0
         Left            =   7320
         TabIndex        =   6
         Tag             =   "0"
         Top             =   660
         Width           =   996
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
         Left            =   7320
         TabIndex        =   10
         Tag             =   "0"
         Top             =   1020
         Width           =   996
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
         Left            =   7320
         TabIndex        =   14
         Tag             =   "0"
         Top             =   1380
         Width           =   996
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
         Left            =   7320
         TabIndex        =   18
         Tag             =   "0"
         Top             =   1740
         Width           =   996
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
         Left            =   7320
         TabIndex        =   22
         Tag             =   "0"
         Top             =   2100
         Width           =   996
      End
      Begin VB.Label lbl 
         Caption         =   "lb4"
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
         Index           =   4
         Left            =   300
         TabIndex        =   19
         Tag             =   "five"
         Top             =   2100
         Width           =   4500
      End
      Begin VB.Label lbl 
         Caption         =   "lb5"
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
         Index           =   5
         Left            =   300
         TabIndex        =   23
         Tag             =   "six"
         Top             =   2460
         Width           =   4500
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
         Index           =   5
         Left            =   7320
         TabIndex        =   26
         Tag             =   "0"
         Top             =   2460
         Width           =   996
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
      Begin VB.Menu multiplier 
         Caption         =   "Relase Multiplier"
      End
      Begin VB.Menu prog 
         Caption         =   "Include Progeny"
         Visible         =   0   'False
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
   Begin VB.Menu help 
      Caption         =   "&Help"
      Begin VB.Menu howto 
         Caption         =   "How to ..."
      End
      Begin VB.Menu about 
         Caption         =   "&About"
      End
   End
   Begin VB.Menu SpreadEdit 
      Caption         =   "SpreadEdit"
      Visible         =   0   'False
      Begin VB.Menu SpreadCopy 
         Caption         =   "Copy         Ctrl+C"
         Checked         =   -1  'True
      End
      Begin VB.Menu SpreadCut 
         Caption         =   "Cut            Ctrl+X"
      End
      Begin VB.Menu SpreadPaste 
         Caption         =   "Paste         Ctrl+V"
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

Dim fluxtypeidx As Long
Dim mode As String
Dim c1 As Long
Dim c2 As Long
Dim c3 As Long
Dim tCnt As Long
Dim temp As parmrec
Dim loadng As Boolean
Dim prevtime As Double
Dim tabclick As Boolean
Dim conCount As Long
Dim conError As String

Private Sub Check_NTS(nts As Long, errmsg As String)
  If (nts < 2) Then
    conError = conError + vbCrLf + errmsg
    conCount = conCount + 1
  End If
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

Private Sub multiplier_Click()
  multiplier.Checked = Not multiplier.Checked
  If multiplier.Checked Then
    SSTab1.Tabs = tCnt + 1
    SSTab1.TabCaption(tCnt) = "Inventory/Release Multipliers"
  Else
    SSTab1.Tabs = tCnt
  End If
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
  Else
    lbl(7).Visible = False
    ref(7).Visible = False
    Combo3.Visible = False
    vaSpread2.Visible = False
    SSCommand3.Visible = False
    SSCommand4.Visible = False
  End If
End Sub

Private Sub fluxfillet()
Dim idx As Long
  
  idx = temp.idx1 * 2
  If temp.pName = "radius" Then idx = idx + 1
  FluxTypes.ref(idx).Tag = temp.ref
  FluxTypes.ref(idx).Caption = "Ref:" & Str(temp.ref)
  If temp.cunit <> "N/A" Then set_unit FluxTypes.unit(idx), temp.uunit
  FluxTypes.txt(idx).Text = convert(temp.cunit, temp.uunit, val(temp.pval))
End Sub

Private Sub fracfillet()
Dim idx As Long
  
  idx = 0
  If temp.pName = "reactivefrac" Then idx = 1
  FluxTypes.ref(idx).Tag = temp.ref
  FluxTypes.ref(idx).Caption = "Ref:" & Str(temp.ref)
  If temp.cunit <> "N/A" Then set_unit FluxTypes.unit(idx), temp.uunit
  FluxTypes.txt(idx).Text = convert(temp.cunit, temp.uunit, val(temp.pval))
End Sub

Private Sub fillet(idx As Long)
  sink(temp.idx1).ref(idx) = temp.ref
  sink(temp.idx1).pnt(idx) = temp.cunit
  sink(temp.idx1).unt(idx) = temp.uunit
  sink(temp.idx1).val(idx) = convert(temp.cunit, temp.uunit, val(temp.pval))
End Sub

Private Sub SizeParentProgenyList()
  If temp.idx2 > f_numcon - 1 Then
    ReDim Preserve con(temp.idx2) As parent
    f_numcon = temp.idx2
  End If
  If temp.idx3 > con(temp.idx2).numprog - 1 Then
    ReDim Preserve con(temp.idx2).prog(temp.idx3) As progeny
    con(temp.idx2).numprog = temp.idx3
  End If
End Sub

Private Sub SizeLocal()
  If temp.idx3 > numsnk Then
    numsnk = temp.idx3
    ReDim Preserve loc(numsnk) As snk
  End If
End Sub
      
Private Sub InitUnits(i As Long)
  Select Case Left(sink(i).dataset, 3)
  Case "wff"
    If sink(i).unt(1) = "" Then sink(i).unt(1) = "m"
    If sink(i).unt(2) = "" Then sink(i).unt(2) = "m"
    If sink(i).unt(3) = "" Then sink(i).unt(3) = "m"
    If sink(i).unt(4) = "" Then sink(i).unt(4) = "m/yr"
    If sink(i).pnt(1) = "" Then sink(i).pnt(1) = "m"
    If sink(i).pnt(2) = "" Then sink(i).pnt(2) = "m"
    If sink(i).pnt(3) = "" Then sink(i).pnt(3) = "m"
    If sink(i).pnt(4) = "" Then sink(i).pnt(4) = "m/yr"
  Case "scf"
    If sink(i).unt(1) = "" Then sink(i).unt(1) = "m"
    If sink(i).unt(2) = "" Then sink(i).unt(2) = "m"
    If sink(i).unt(3) = "" Then sink(i).unt(3) = "m"
    If sink(i).pnt(1) = "" Then sink(i).pnt(1) = "m"
    If sink(i).pnt(2) = "" Then sink(i).pnt(2) = "m"
    If sink(i).pnt(3) = "" Then sink(i).pnt(3) = "m"
  Case "aff"
    If sink(i).unt(1) = "" Then sink(i).unt(1) = "m^2"
    If sink(i).unt(2) = "" Then sink(i).unt(2) = "m"
    If sink(i).unt(3) = "" Then sink(i).unt(3) = "m"
    If sink(i).unt(4) = "" Then sink(i).unt(4) = "m/sec"
    If sink(i).unt(5) = "" Then sink(i).unt(5) = "C"
    If sink(i).unt(6) = "" Then sink(i).unt(6) = "C"
    If sink(i).pnt(1) = "" Then sink(i).pnt(1) = "m^2"
    If sink(i).pnt(2) = "" Then sink(i).pnt(2) = "m"
    If sink(i).pnt(3) = "" Then sink(i).pnt(3) = "m"
    If sink(i).pnt(4) = "" Then sink(i).pnt(4) = "m/sec"
    If sink(i).pnt(5) = "" Then sink(i).pnt(5) = "C"
    If sink(i).pnt(6) = "" Then sink(i).pnt(6) = "C"
  Case "wcf"
    'no units to set
  End Select
End Sub

Private Sub SizeSink()
  If temp.idx1 > UBound(sink) Then
    ReDim Preserve sink(temp.idx1) As location
    ReDim sink(temp.idx1).con(1) As parentflux
    sink(temp.idx1).con(0).use = False
    sink(temp.idx1).con(0).cas = "water"
    sink(temp.idx1).con(0).unit(0) = "m^3/yr"
    sink(temp.idx1).con(0).unit(1) = "yr"
    sink(temp.idx1).con(0).unit(2) = "m^3/yr"
    sink(temp.idx1).con(0).unit(3) = "m^3/yr"
    sink(temp.idx1).con(0).unit(4) = "m^3/yr"
    sink(temp.idx1).con(0).unit(5) = "m^3/yr"
    numsink = temp.idx1
  End If
  If temp.idx2 > sink(temp.idx1).numcon - 1 Then
    ReDim Preserve sink(temp.idx1).con(temp.idx2) As parentflux
    sink(temp.idx1).numcon = temp.idx2
  End If
  If temp.idx3 > sink(temp.idx1).con(temp.idx2).numprog - 1 Then
    ReDim Preserve sink(temp.idx1).con(temp.idx2).prog(temp.idx3) As progflux
    sink(temp.idx1).con(temp.idx2).numprog = temp.idx3
  End If
  If temp.idx3 = 0 Then
    If temp.idx4 > sink(temp.idx1).con(temp.idx2).nts - 1 Then
      ReDim Preserve sink(temp.idx1).con(temp.idx2).series(temp.idx4) As fluxrec
      sink(temp.idx1).con(temp.idx2).nts = temp.idx4
    End If
  Else
    If temp.idx4 > sink(temp.idx1).con(temp.idx2).prog(temp.idx3).nts - 1 Then
      ReDim Preserve sink(temp.idx1).con(temp.idx2).prog(temp.idx3).series(temp.idx4) As fluxrec
      sink(temp.idx1).con(temp.idx2).prog(temp.idx3).nts = temp.idx4
    End If
  End If
End Sub

Private Sub loadprm()
Dim i As Long, j As Long, k As Long, l As Long, m As Long
Dim fle As parmfile
Dim sval As Boolean
Dim asrc As String
Dim mx As Long
Dim bcqual As Variant
Dim bctype As Variant
Dim dataset As String
Dim cnt As Long
Dim vMult As Variant
Dim mNode As ComctlLib.Node

  f_numcon = 0
  ReDim loc(0) As snk
  ReDim sink(0) As location
  If open_parm(fle, FUIName, 2) Then
    Do Until EOCF(fle.file)
      If read_parmrec(fle, temp) Then
        Select Case temp.pName
          Case "fui"
            For i = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                If temp.idx1 = siteIdx Then
                  Select Case temp.pName
                    Case "UsrDesPath"
                      If temp.idx2 = modIdx Then DesName = temp.pval
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
                        con(temp.idx2).prog(temp.idx3).kind = val(temp.pval)
                      Else
                        con(temp.idx2).kind = val(temp.pval)
                      End If
                    Case "clptype"
                      SizeParentProgenyList
                      If temp.idx3 > 0 Then
                        con(temp.idx2).prog(temp.idx3).grp = val(temp.pval) + 1
                      Else
                        con(temp.idx2).grp = val(temp.pval) + 1
                      End If
                   End Select
                End If
              End If
            Next
          Case modName
            Loading.Gauge1.Max = val(temp.idx1)
            Loading.Gauge1.Value = 0
            For i = 1 To temp.idx1
              Loading.update
              If read_parmrec(fle, temp) Then
                Select Case temp.pName
                  Case "CVTFormat":    CVTFormat = "General Number" 'CVTFormat = temp.pval
                  Case "progeny":
                    sval = False
                    If temp.pval = "True" Then
                      'sval = True
                      'no more product output
                      MsgBox "This case contained progeny flux/concentration values which are no loneger supported!" & vbCrLf & _
                             "Those inventories have been removed and must be rentered as parents.", vbInformation
                    End If
                    prog.Checked = sval
                  Case "dataset":          SizeSink: sink(temp.idx1).dataset = temp.pval
                  Case "media":            SizeSink: sink(temp.idx1).media = temp.pval
                  Case "locname":          SizeSink: sink(temp.idx1).locname = temp.pval
                  Case "one":              fillet 1
                  Case "two":              fillet 2
                  Case "three":            fillet 3
                  Case "four":             fillet 4
                  Case "five":             fillet 5
                  Case "six":              fillet 6
                  Case "reactivedensity", "reactivefrac":
                    haveflux = True
                    fracfillet
                  Case "radius", "density":
                    haveflux = True
                    fluxfillet
                  Case "useMult":
                    mVal = False
                    If temp.pval = "True" Then mVal = True
                  Case "irMult":
                    txt(8).Text = temp.pval
                    ref(8).Tag = temp.ref
                    ref(8).Caption = "Ref: " & CStr(temp.ref)
                  Case "irGrpMult":
                    vMult = temp.pval
                    vaSpread3.SetText 2, temp.idx1, vMult
                  Case "fluxtypes":
                    sval = False
                    If temp.pval = "True" Then sval = True
                    FluxTypes.SSCheck1(temp.idx1).Value = sval
                    fluxtypeidx = temp.idx1
                  Case "casid", "conccas":
                    SizeSink
                    If temp.idx3 = 0 Then
                      sink(temp.idx1).con(temp.idx2).cas = temp.pval
                      sink(temp.idx1).con(temp.idx2).ref = temp.ref
                      sink(temp.idx1).con(temp.idx2).use = False
                      sink(temp.idx1).con(temp.idx2).unit(0) = temp.cunit
                      sink(temp.idx1).con(temp.idx2).unit(1) = temp.uunit
                      'for backward compatibilty
                      If temp.cunit = "g/kg" Then sink(temp.idx1).con(temp.idx2).unit(1) = "mg/kg"
                      For j = 2 To 5
                        sink(temp.idx1).con(temp.idx2).unit(j) = temp.cunit
                      Next
                    Else
                      sink(temp.idx1).con(temp.idx2).prog(temp.idx3).cas = temp.pval
                      sink(temp.idx1).con(temp.idx2).prog(temp.idx3).ref = temp.ref
                      sink(temp.idx1).con(temp.idx2).prog(temp.idx3).use = False
                      sink(temp.idx1).con(temp.idx2).prog(temp.idx3).unit(0) = temp.cunit
                      sink(temp.idx1).con(temp.idx2).prog(temp.idx3).unit(1) = temp.uunit
                      'for backward compatibilty
                      If temp.cunit = "g/kg" Then sink(temp.idx1).con(temp.idx2).prog(temp.idx3).unit(1) = "mg/kg"
                      For j = 2 To 5
                        sink(temp.idx1).con(temp.idx2).prog(temp.idx3).unit(j) = temp.cunit
                      Next
                    End If
                  Case "cval", "cflux", "conc":
                    SizeSink
                  ' units compatibility change
                    If Right(temp.uunit, 2) = "/s" Then temp.uunit = temp.uunit & "ec"
                    If temp.pName = "conc" And sink(temp.idx1).locname <> "" Then
                      sink(temp.idx1).loctype = "wcf"
                      temp.idx5 = 1
                    End If
                    If temp.pName = "cflux" And sink(temp.idx1).locname <> "" Then
                      sink(temp.idx1).loctype = "wff"
                      temp.idx5 = 1
                    End If
                    If temp.idx3 = 0 Then
                      sink(temp.idx1).con(temp.idx2).series(temp.idx4).flux(temp.idx5) = val(convert(temp.cunit, temp.uunit, val(temp.pval)))
                      If temp.idx5 <> 0 Then sink(temp.idx1).con(temp.idx2).unit(temp.idx5 + 1) = temp.uunit
                    Else
                      sink(temp.idx1).con(temp.idx2).prog(temp.idx3).series(temp.idx4).flux(temp.idx5) = val(convert(temp.cunit, temp.uunit, val(temp.pval)))
                      sink(temp.idx1).con(temp.idx2).prog(temp.idx3).unit(temp.idx5 + 1) = temp.uunit
                    End If
                  Case "ctime":
                    SizeSink
                  ' units compatibility change
                    If temp.uunit = "s" Then temp.uunit = "sec"
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
                    Case "modlocx"
                      loc(0).x = val(temp.pval)
                    Case "modlocy"
                      loc(0).Y = val(temp.pval)
                    Case "modlocz"
                      loc(0).Z = val(temp.pval)
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
    close_parm fle
    SetFormat Me

    For i = 1 To numsnk
      bctype = Split(loc(i).type, ",")
      bcqual = Split(loc(i).qual, ",")
      For j = 0 To UBound(bcqual)
        dataset = bctype(j) & ":" & bcqual(j)
        For k = 1 To numsink
        
          ' the following added for backward compatibility
          If sink(k).dataset = "" And sink(k).media <> "" Then
            Select Case k
              Case 1, 2:
                sink(k).media = bcqual(j)
                sink(k).dataset = "wff:" & sink(k).media
              Case 3:
                sink(k).dataset = "aff:Air"
              Case 4:
                sink(k).media = bcqual(j)
                sink(k).dataset = "scf:" & sink(k).media
            End Select
          End If
          If sink(k).dataset = "" And sink(k).locname <> "" Then
            sink(k).media = bcqual(j)
            sink(k).dataset = sink(k).loctype & ":" & sink(k).media
          End If
          
          InitUnits k
          ' end compatibility
          
          If (InStr(dataset, sink(k).dataset) > 0) Then
            sink(k).use = True
            sink(k).dataset = dataset
            If Not InStr(bcqual(j), "Air") > 0 Then
              sink(k).media = bcqual(j)
            End If
            Exit For
          End If
        Next
        If k > numsink Then
          temp.idx1 = k: temp.idx2 = 0
          temp.idx3 = 0: temp.idx4 = 0
          SizeSink
          sink(k).use = True
          sink(k).dataset = dataset
          InitUnits k
          If InStr(bcqual(j), "Air") > 0 Then
            sink(k).media = "Point"
          Else
            sink(k).media = bcqual(j)
          End If
        End If
      Next
    Next
    
    cnt = 0
    For i = 1 To numsink
      If sink(i).use Then
        cnt = cnt + 1
        SSTab1.Tabs = cnt
        SSTab1.TabCaption(cnt - 1) = sink(i).media & " located at " & CStr(loc(0).x) & " km Easting, " & CStr(loc(0).Y) & " km Northing"
        If cnt = 1 Then c1 = i
      End If
    Next
    tCnt = cnt
    
    If cnt = 0 Then
      PutError "Nothing to do, down stream module(s) possibly undefined."
      EndModule
    End If

    Set mNode = treeGrp.Nodes.Add(, , "root", "Periodic Groups")
    mNode.Expanded = True
    treeGrp.Nodes.Add "root", tvwChild, "g1", "Not Assigned"
    treeGrp.Nodes.Add "root", tvwChild, "g2", "Noble Gas"
    treeGrp.Nodes.Add "root", tvwChild, "g3", "Halogens"
    treeGrp.Nodes.Add "root", tvwChild, "g4", "Alkali Metals"
    treeGrp.Nodes.Add "root", tvwChild, "g5", "Tellurium Group"
    treeGrp.Nodes.Add "root", tvwChild, "g6", "Noble Metals"
    treeGrp.Nodes.Add "root", tvwChild, "g7", "Lanthanides"
    treeGrp.Nodes.Add "root", tvwChild, "g8", "Actinides"
    treeGrp.Nodes.Add "root", tvwChild, "g9", "Alkaline Earths"
    treeGrp.Nodes.Add "root", tvwChild, "g10", "Non-Metals"
    treeGrp.Nodes.Add "root", tvwChild, "g11", "Transistion Metals"
    treeGrp.Nodes.Add "root", tvwChild, "g12", "Metalloids"

    'resolve contaminate differences if contaminate no longer exists its index is 0
    con(0).Name = "water"
    con(0).cas = "water"
    con(0).kind = -1
    con(0).numprog = 0
    For i = 1 To numsink
      If sink(i).use Then
        For j = 0 To f_numcon
          For k = 0 To sink(i).numcon
            If con(j).cas = sink(i).con(k).cas Then
              'check to see if this is water and is a wff
              If j <> 0 Or (Left(sink(i).dataset, 3) = "wff") Then sink(i).con(k).use = True
              sink(i).con(k).Name = con(j).Name
              sink(i).con(k).irIdx = con(j).grp
              sink(i).con(k).numprog = con(j).numprog
              setparentunits con(j).kind, i, k
              If con(j).grp <> 0 Then treeGrp.Nodes.Add "g" & CStr(con(j).grp), tvwChild, con(j).Name, con(j).Name
              For l = 1 To con(j).numprog
                'If sink(i).con(k).prog Then
                For m = 1 To UBound(sink(i).con(k).prog)
                  If con(j).prog(l).cas = sink(i).con(k).prog(m).cas Then
                    sink(i).con(k).prog(m).use = True
                    sink(i).con(k).prog(m).cas = con(j).prog(l).cas
                    sink(i).con(k).prog(m).Name = con(j).prog(l).Name
                    sink(i).con(k).prog(m).irIdx = con(j).prog(l).grp
                    setprogunits con(j).kind, i, k, m
                    Exit For
                  End If
                Next
              '  End If
                If m > UBound(sink(i).con(k).prog) Then
                  ReDim Preserve sink(i).con(k).prog(m) As progflux
                  sink(i).con(k).numprog = m
                  sink(i).con(k).prog(m).use = True
                  sink(i).con(k).prog(m).cas = con(j).prog(l).cas
                  sink(i).con(k).prog(m).Name = con(j).prog(l).Name
                  sink(i).con(k).prog(m).irIdx = con(j).prog(l).grp
                  sink(i).con(k).prog(m).unit(1) = "yr"
                  setprogunits con(j).kind, i, k, m
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
            sink(i).con(k).Name = con(j).Name
            sink(i).con(k).irIdx = con(j).grp
            sink(i).con(k).numprog = con(j).numprog
            sink(i).con(k).unit(1) = "yr"
            setparentunits con(j).kind, i, k
            If con(j).grp <> 0 Then treeGrp.Nodes.Add "g" & CStr(con(j).grp), tvwChild, con(j).Name, con(j).Name
            If con(j).numprog > 0 Then
              ReDim Preserve sink(i).con(k).prog(con(j).numprog) As progflux
              For l = 1 To con(j).numprog
                sink(i).con(k).prog(l).use = True
                sink(i).con(k).prog(l).cas = con(j).prog(l).cas
                sink(i).con(k).prog(l).Name = con(j).prog(l).Name
                sink(i).con(k).prog(l).irIdx = con(j).prog(l).grp
                sink(i).con(k).prog(l).unit(1) = "yr"
                setprogunits con(j).kind, i, k, l
              Next
            End If
          End If
        Next
      End If
    Next
    
  Else
    PutError "Can't find or open file " & FUIName
    EndModule
  End If
End Sub

Private Sub setparentunits(a As Long, i As Long, k As Long)
  Select Case a
    Case 1
      'rad
      Select Case Left(sink(i).dataset, 3)
        Case "wff": setparentunit i, k, "pCi/yr"
        Case "aff": setparentunit i, k, "pCi/yr"
        Case "scf": setparentunit i, k, "pCi/kg"
        Case "wcf": setparentunit i, k, "pCi/ml"
      End Select
    Case -1
      'water
      setparentunit i, k, "m^3/yr"
    Case Else
      'chem
      Select Case Left(sink(i).dataset, 3)
        Case "wff": setparentunit i, k, "g/yr"
        Case "aff": setparentunit i, k, "g/yr"
        Case "scf": setparentunit i, k, "mg/kg"
        Case "wcf": setparentunit i, k, "g/ml"
      End Select
  End Select
End Sub

Private Sub setparentunit(a As Long, b As Long, u As String)
Dim i As Long

  sink(a).con(b).unit(0) = u
  For i = 2 To 5
    If sink(a).con(b).unit(i) = "" Then sink(a).con(b).unit(i) = u
  Next
End Sub

Private Sub setprogunits(a As Long, i As Long, k As Long, l As Long)

  Select Case a
    Case 1
      'rad
      Select Case Left(sink(i).dataset, 3)
        Case "wff": setprogunit i, k, l, "pCi/yr"
        Case "aff": setprogunit i, k, l, "pCi/yr"
        Case "scf": setprogunit i, k, l, "pCi/kg"
        Case "wcf": setprogunit i, k, l, "pCi/ml"
      End Select
    Case -1
      'water
      setprogunit i, k, l, "m^3/yr"
    Case Else
      'chem
      Select Case Left(sink(i).dataset, 3)
        Case "wff": setprogunit i, k, l, "g/yr"
        Case "aff": setprogunit i, k, l, "g/yr"
        Case "scf": setprogunit i, k, l, "mg/kg"
        Case "wcf": setprogunit i, k, l, "g/ml"
      End Select
  End Select
End Sub

Private Sub setprogunit(a As Long, b As Long, C As Long, u As String)
Dim i As Long

  sink(a).con(b).prog(C).unit(0) = u
  For i = 2 To 5
    If sink(a).con(b).prog(C).unit(i) = "" Then sink(a).con(b).prog(C).unit(i) = u
  Next
End Sub

Private Sub save_Click()
  Dim i As Long, j As Long, k As Long
  Dim l As Long, m As Long, n As Long
  Dim Y As Long, ii As Long
  Dim vMult As Variant
  Dim Ok(4) As Boolean
  Dim fName As String
  Dim sMult As String
  Dim parm As parmrec
  Dim fle As parmfile
  Dim UserFormat As String
  UserFormat = CVTFormat
  
  For j = 1 To 6
    sink(c1).unt(j) = unit(j - 1).Text
    sink(c1).val(j) = txt(j - 1).Text
    sink(c1).ref(j) = ref(j - 1).Tag
  Next
  If SSTab1.Tab <> tCnt Then
    getparent
    getprog
  End If
  fName = RunName & ".gid"
  If open_parm(fle, fName, 1) Then
    set_parm parm, "CVTFormat", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CVTFormat
    write_parmrec fle, parm
    CVTFormat = "General Number"
    set_parm parm, "progeny", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", prog.Checked
    write_parmrec fle, parm
    
    set_parm parm, "useMult", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", multiplier.Checked
    write_parmrec fle, parm
    set_parm parm, "irMult", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", txt(8).Text
    write_parmrec fle, parm
    For j = 1 To 12
      vaSpread3.GetText 2, j, vMult
      sMult = vMult
      set_parm parm, "irGrpMult", j, 0, 0, 0, 0, 0, 0, "N/A", "N/A", sMult
      write_parmrec fle, parm
    Next
    
    ii = 0
    For i = 1 To numsink
      If sink(i).use Then
        ii = ii + 1
        set_parm parm, "media", ii, 0, 0, 0, 0, 0, 0, "N/A", "N/A", sink(i).media
        write_parmrec fle, parm
        set_parm parm, "dataset", ii, 0, 0, 0, 0, 0, 0, "N/A", "N/A", sink(i).dataset
        write_parmrec fle, parm
        
        'write appropriate values for each file type
        Select Case Left(sink(i).dataset, 3)
          Case "wff": wffLabels
          Case "aff": affLabels
          Case "scf": scfLabels
          Case "wcf": wcfLabels
        End Select
        
        'write sink properties
        If Left(sink(i).dataset, 3) <> "wcf" Then
          For j = 0 To 5
            If sink(i).val(j + 1) = "" Then
              PutError lbl(j).Caption & " is invalid for " & sink(i).dataset
            Else
              set_parm parm, txt(j).Tag, ii, 0, 0, 0, 0, 0, sink(i).ref(j + 1), sink(i).unt(j + 1), sink(i).pnt(j + 1), convert(sink(i).unt(j + 1), sink(i).pnt(j + 1), val(sink(i).val(j + 1)))
              write_parmrec fle, parm
            End If
            'skip ones that aren't used for these [i] sinks
            If sink(i).media = "Area" And j = 0 Then j = 3
            If Left(sink(i).dataset, 3) = "wff" And (sink(i).media = "Surface Water" Or sink(i).media = "Vadose") And j = 1 Then j = 5
            If Left(sink(i).dataset, 3) = "wff" And sink(i).media = "Aquifer" And j = 2 Then j = 5
            If Left(sink(i).dataset, 3) = "scf" And j = 2 Then j = 5
          Next
        End If
        
        'write air flux types if appropriate
        If Left(sink(i).dataset, 3) = "aff" Then
          'write flux selections
          For j = 0 To 3
            set_parm parm, "fluxtypes", j, 0, 0, 0, 0, 0, 0, "N/A", "N/A", FluxTypes.SSCheck1(j).Value
            write_parmrec fle, parm
          Next
          
          'write gas flux types
          k = 0
          If FluxTypes.SSCheck1(0).Enabled And FluxTypes.SSCheck1(0).Value Then
            set_parm parm, FluxTypes.lbl(k).Tag, 1, 0, 0, 0, 0, 0, FluxTypes.ref(k).Tag, FluxTypes.unit(k).Text, FluxTypes.unit(k).Tag, convert(FluxTypes.unit(k).Text, FluxTypes.unit(k).Tag, val(FluxTypes.txt(k).Text))
            write_parmrec fle, parm
            k = k + 1
            set_parm parm, FluxTypes.lbl(k).Tag, 1, 0, 0, 0, 0, 0, FluxTypes.ref(k).Tag, FluxTypes.unit(k).Text, FluxTypes.unit(k).Tag, convert(FluxTypes.unit(k).Text, FluxTypes.unit(k).Tag, val(FluxTypes.txt(k).Text))
            write_parmrec fle, parm
          End If
          
          'write particle flux types
          k = 2
          For j = 1 To 3
            If FluxTypes.SSCheck1(j).Enabled And FluxTypes.SSCheck1(j).Value Then
              set_parm parm, FluxTypes.lbl(k).Tag, j, 0, 0, 0, 0, 0, FluxTypes.ref(k).Tag, FluxTypes.unit(k).Text, FluxTypes.unit(k).Tag, convert(FluxTypes.unit(k).Text, FluxTypes.unit(k).Tag, val(FluxTypes.txt(k).Text))
              write_parmrec fle, parm
              k = k + 1
              set_parm parm, FluxTypes.lbl(k).Tag, j, 0, 0, 0, 0, 0, FluxTypes.ref(k).Tag, FluxTypes.unit(k).Text, FluxTypes.unit(k).Tag, convert(FluxTypes.unit(k).Text, FluxTypes.unit(k).Tag, val(FluxTypes.txt(k).Text))
              write_parmrec fle, parm
              k = k + 1
            Else
              k = k + 2
            End If
          Next
        End If
        
        
        conCount = 0
        'write water flux if appropriate
        If Left(sink(i).dataset, 3) = "wff" Then
          set_parm parm, "casid", ii, 0, 0, 0, 0, 0, sink(i).con(0).ref, "yr", "m^3/yr", "water"
          write_parmrec fle, parm
          Check_NTS sink(i).con(0).nts, sink(i).con(0).Name + " number of time points is less than two"
          prevtime = -1
          For l = 1 To sink(i).con(0).nts
            Is_Ascending sink(i).con(0).series(l).time, sink(i).con(0).Name + " time series element " & CStr(l) & " out of sequence"
            set_parm parm, "ctime", ii, 0, 0, l, 0, 0, 0, sink(i).con(0).unit(1), "yr", convert(sink(i).con(0).unit(1), "yr", sink(i).con(0).series(l).time)
            write_parmrec fle, parm
            set_parm parm, "cval", ii, 0, 0, l, 1, 0, 0, sink(i).con(0).unit(2), "m^3/yr", convert(sink(i).con(0).unit(2), "m^3/yr", sink(i).con(0).series(l).flux(1))
            write_parmrec fle, parm
          Next
        End If
        
        'write fluxes or concentrations
        prevtime = -1
        For j = 1 To f_numcon
          For k = 1 To sink(i).numcon
            If sink(i).con(k).cas = con(j).cas Then
              set_parm parm, "casid", ii, j, 0, 0, 0, 0, sink(i).con(k).ref, "yr", sink(i).con(k).unit(0), con(j).cas
              write_sparmrec fle, parm
              prevtime = -1
              Check_NTS sink(i).con(k).nts, sink(i).con(k).Name + " number of time points is less than two"
              For l = 1 To sink(i).con(k).nts
                Is_Ascending sink(i).con(k).series(l).time, sink(i).con(k).Name + " time series element " & CStr(l) & " out of sequence"
                set_parm parm, "ctime", ii, j, 0, l, 0, 0, 0, sink(i).con(k).unit(1), "yr", convert(sink(i).con(k).unit(1), "yr", sink(i).con(k).series(l).time)
                write_parmrec fle, parm
                If Left(sink(i).dataset, 3) = "aff" Then         ' if air source
                  For m = 0 To 3
                    If FluxTypes.SSCheck1(m).Enabled And FluxTypes.SSCheck1(m).Value Then
                      set_parm parm, "cval", ii, j, 0, l, m + 1, 0, 0, sink(i).con(k).unit(m + 2), sink(i).con(k).unit(0), convert(sink(i).con(k).unit(m + 2), sink(i).con(k).unit(0), sink(i).con(k).series(l).flux(m + 1))
                      write_parmrec fle, parm
                    End If
                  Next
                Else
                  set_parm parm, "cval", ii, j, 0, l, 1, 0, 0, sink(i).con(k).unit(2), sink(i).con(k).unit(0), convert(sink(i).con(k).unit(2), sink(i).con(k).unit(0), sink(i).con(k).series(l).flux(1))
                  write_parmrec fle, parm
                  If sink(i).dataset = "wff:Surface Water" Then
                    set_parm parm, "cval", ii, j, 0, l, 2, 0, 0, sink(i).con(k).unit(3), sink(i).con(k).unit(0), convert(sink(i).con(k).unit(3), sink(i).con(k).unit(0), sink(i).con(k).series(l).flux(2))
                    write_parmrec fle, parm
                  End If
                End If
              Next
              If prog.Checked Then
                For l = 1 To con(j).numprog
                  For m = 1 To sink(i).con(k).numprog
                    If sink(i).con(k).prog(m).cas = con(j).prog(l).cas Then
                      set_parm parm, "casid", ii, j, l, 0, 0, 0, sink(i).con(k).prog(m).ref, "yr", sink(i).con(k).prog(m).unit(0), con(j).prog(l).cas
                      write_sparmrec fle, parm
                      Check_NTS sink(i).con(k).prog(m).nts, sink(i).con(k).prog(m).Name + " number of time points is less than two"
                      prevtime = -1
                      For n = 1 To sink(i).con(k).prog(m).nts
                        Is_Ascending sink(i).con(k).prog(m).series(n).time, sink(i).con(k).prog(m).Name + " time series element " & CStr(n) & " out of sequence"
                        set_parm parm, "ctime", ii, j, l, n, 0, 0, 0, sink(i).con(k).prog(m).unit(1), "yr", convert(sink(i).con(k).prog(m).unit(1), "yr", sink(i).con(k).prog(m).series(n).time)
                        write_parmrec fle, parm
                        If Left(sink(i).dataset, 3) = "aff" Then  ' if air source
                          For Y = 0 To 3
                            If FluxTypes.SSCheck1(Y).Enabled And FluxTypes.SSCheck1(Y).Value Then
                              set_parm parm, "cval", ii, j, l, n, Y + 1, 0, 0, sink(i).con(k).prog(m).unit(Y + 2), sink(i).con(k).prog(m).unit(0), convert(sink(i).con(k).prog(m).unit(Y + 2), sink(i).con(k).prog(m).unit(0), sink(i).con(k).prog(m).series(n).flux(Y + 1))
                              write_parmrec fle, parm
                            End If
                          Next
                        Else
                          set_parm parm, "cval", ii, j, l, n, 1, 0, 0, sink(i).con(k).prog(m).unit(2), sink(i).con(k).prog(m).unit(0), convert(sink(i).con(k).prog(m).unit(2), sink(i).con(k).prog(m).unit(0), sink(i).con(k).prog(m).series(n).flux(1))
                          write_parmrec fle, parm
                          If sink(i).dataset = "wff:Surface Water" Then
                            set_parm parm, "cval", ii, j, l, n, 2, 0, 0, sink(i).con(k).prog(m).unit(3), sink(i).con(k).prog(m).unit(0), convert(sink(i).con(k).prog(m).unit(3), sink(i).con(k).prog(m).unit(0), sink(i).con(k).prog(m).series(n).flux(2))
                            write_parmrec fle, parm
                          End If
                        End If
                      Next
                    End If
                  Next
                Next
              End If
            End If
          Next
        Next
        If conCount <> 0 Then
          PutError conError
        End If
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

Private Sub getparent()
  Dim i As Long
  Dim j As Long
  Dim temp As String
  Dim rc As Long
  
  If c1 = -1 Then Exit Sub
  vaSpread1.Row = 1
  For j = 1 To 5
    vaSpread1.col = j
    sink(c1).con(c2).unit(j) = vaSpread1.Text
  Next
  sink(c1).con(c2).ref = ref(6).Tag
  sink(c1).con(c2).nts = vaSpread1.DataRowCnt - 1
  If sink(c1).con(c2).nts > 0 Then
    ReDim sink(c1).con(c2).series(sink(c1).con(c2).nts)
  End If
  For j = 2 To sink(c1).con(c2).nts + 1
    vaSpread1.col = 1
    vaSpread1.Row = j
    temp = vaSpread1.Value
    If Len(temp) > 0 Then
      sink(c1).con(c2).nts = j - 1
      sink(c1).con(c2).series(j - 1).time = val(vaSpread1.Value)
      vaSpread1.Text = ""
      For i = 2 To vaSpread1.DataColCnt
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
  Dim rc As Long
  
  If c1 = -1 Then Exit Sub
  If sink(c1).con(c2).numprog = 0 Then Exit Sub
  vaSpread2.Row = 1
  For j = 1 To 5
    vaSpread2.col = j
    sink(c1).con(c2).prog(c3).unit(j) = vaSpread2.Text
  Next
  sink(c1).con(c2).prog(c3).ref = ref(7).Tag
  sink(c1).con(c2).prog(c3).nts = vaSpread2.DataRowCnt - 1
  If sink(c1).con(c2).prog(c3).nts > 0 Then
    ReDim sink(c1).con(c2).prog(c3).series(sink(c1).con(c2).prog(c3).nts)
  End If
  For j = 2 To sink(c1).con(c2).prog(c3).nts + 1
    vaSpread2.col = 1
    vaSpread2.Row = j
    temp = vaSpread2.Value
    If Len(temp) > 0 Then
      sink(c1).con(c2).prog(c3).series(j - 1).time = val(vaSpread2.Value)
      vaSpread2.Text = ""
      For i = 2 To vaSpread2.DataColCnt
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

Private Sub putparent()
  Dim i As Long
  Dim j As Long
  
  If c1 = -1 Then Exit Sub
  ClearSpread vaSpread1
  ref(6).Caption = "Ref: " + Str(sink(c1).con(c2).ref)
  ref(6).Tag = sink(c1).con(c2).ref
  vaSpread1.Row = 0
  vaSpread1.col = 2
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
      vaSpread1.Value = Format(sink(c1).con(c2).series(j).flux(i - 1), CVTFormat)
    Next
  Next
End Sub

Private Sub putprog()
  Dim i As Long
  Dim j As Long
  
  If c1 = -1 Then Exit Sub
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
      vaSpread2.Value = Format(sink(c1).con(c2).prog(c3).series(j).flux(i - 1), CVTFormat)
    Next
  Next
End Sub

Private Sub Form_load()
Dim i As Integer
  
  StartModule Known, App.Title, 6
  SetRefFile ReplaceExt(FUIName, "ref")
  mode = argv(0)
  Loading.Show
  loadng = True
  loadprm
  Unload Loading
  If mode = "model" Then
    Make_AFF
    Make_WFF
    Make_CF "wcf"
    Make_CF "scf"
    EndModule
  End If
  loadng = False
  SSTab1_Click -1
  If mVal Then multiplier_Click
  Show
  SSTab1.SetFocus
End Sub

Private Sub SSTab1_Click(PreviousTab As Integer)
Dim j As Long
Dim cnt As Long
  
  If Not loadng Then
    If PreviousTab > -1 And PreviousTab < tCnt Then
      For j = 1 To 6
        sink(c1).unt(j) = unit(j - 1).Text
        sink(c1).val(j) = txt(j - 1).Text
        sink(c1).ref(j) = ref(j - 1).Tag
      Next
      getparent
      getprog
    End If
    cnt = 0
    For j = 1 To numsink
      If sink(j).use Then
        If SSTab1.Tab = cnt Then
          c1 = j
          Exit For
        End If
        cnt = cnt + 1
      End If
    Next
  End If
  If SSTab1.Tab = tCnt Then
    SSFrame1.Enabled = False
    SSFrame1.Visible = False
    SSFrame3.Enabled = True
    SSFrame3.Visible = True
  Else
    SSFrame3.Enabled = False
    SSFrame3.Visible = False
    SSFrame1.Enabled = True
    SSFrame1.Visible = True
    Select Case Left(sink(c1).dataset, 3)
    Case "wff":
      SetHelpFile App.Path + "\Known WFF.html"
      Combo1.Clear
      Combo1.AddItem "Aquifer"
      Combo1.AddItem "Surface Water"
      Combo1.AddItem "Vadose"
    Case "aff":
      SetHelpFile App.Path + "\Known AFF.html"
      Combo1.Clear
      Combo1.AddItem "Point"   'elevated
      Combo1.AddItem "Area"    'ground
    Case "scf":
      SetHelpFile App.Path + "\Known SCF.html"
      Combo1.Clear
      Combo1.AddItem "Soil"
      Combo1.AddItem "Sediment"
      Combo1.AddItem "Soil-Dissolved"
      Combo1.AddItem "Sediment-Dissolved"
    Case "wcf":
      SetHelpFile App.Path + "\Known WCF.html"
      Combo1.Clear
      Combo1.AddItem "Aquifer-Total"
      Combo1.AddItem "Surface Water-Total"
      Combo1.AddItem "Aquifer"
      Combo1.AddItem "Surface Water"
    End Select
    
    Combo2.Clear
    For j = 0 To sink(c1).numcon
      If sink(c1).con(j).use Then
        Combo2.AddItem sink(c1).con(j).Name
        Combo2.ItemData(Combo2.NewIndex) = j
      End If
    Next
    For j = 1 To 6
      unit(j - 1).Clear
      get_conversion_items sink(c1).pnt(j), unit(j - 1)
      set_unit unit(j - 1), sink(c1).unt(j)
      txt(j - 1).Text = sink(c1).val(j)
      ref(j - 1).Tag = sink(c1).ref(j)
      ref(j - 1).Caption = "Ref: " & CStr(sink(c1).ref(j))
    Next
    For j = 0 To Combo1.ListCount - 1
      If sink(c1).media = Combo1.list(j) Then Combo1.ListIndex = j
    Next
    tabclick = True
    If Combo2.ListCount > 0 Then Combo2.ListIndex = 0
    tabclick = False
    Combo1.Enabled = (Left(sink(c1).dataset, 3) = "aff")
  End If
End Sub

Private Sub Combo2_Click()
  Dim i As Long
  
  If Not loadng And Not tabclick Then
    getparent
    getprog
  End If
  If Combo2.ListIndex <> -1 Then c2 = Combo2.ItemData(Combo2.ListIndex)
  'must refresh labels every time for WFF
  If Left(sink(c1).dataset, 3) = "wff" Then wffLabels
  
  putparent
  If prog.Checked And sink(c1).con(c2).numprog > 0 Then
    lbl(7).Visible = True
    ref(7).Visible = True
    Combo3.Visible = True
    vaSpread2.Visible = True
    SSCommand3.Visible = True
    SSCommand4.Visible = True
  Else
    lbl(7).Visible = False
    ref(7).Visible = False
    Combo3.Visible = False
    vaSpread2.Visible = False
    SSCommand3.Visible = False
    SSCommand4.Visible = False
  End If
  If sink(c1).con(c2).numprog > 0 Then
    Combo3.Clear
    For i = 1 To sink(c1).con(c2).numprog
      If sink(c1).con(c2).prog(i).use Then
        Combo3.AddItem sink(c1).con(c2).prog(i).Name
        Combo3.ItemData(Combo3.NewIndex) = i
      End If
    Next
    loadng = True
    Combo3.ListIndex = 0
    loadng = False
  End If
End Sub

Private Sub Combo3_Click()
  If Not loadng Then getprog
  If Combo3.ListIndex > -1 Then c3 = Combo3.ItemData(Combo3.ListIndex)
  If Left(sink(c1).dataset, 3) = "wff" Then wffLabels
  putprog
End Sub

Private Sub Combo1_Click()
  sink(c1).media = Combo1.Text
  Select Case Left(sink(c1).dataset, 3)
    Case "wff": wffLabels
    Case "aff": affLabels
    Case "scf": scfLabels
    Case "wcf": wcfLabels
  End Select
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
  FluxTypes.Show 1
  haveflux = True
  sink(c1).media = Combo1.Text
  affLabels
End Sub

Private Sub SSCommand1_GotFocus()
  RefItem = 6
  noact.Enabled = True
  HelpAnchor = SSCommand1.Tag
End Sub

Private Sub SSCommand2_GotFocus()
  RefItem = 6
  noact.Enabled = True
  HelpAnchor = SSCommand2.Tag
End Sub

Private Sub SSCommand3_GotFocus()
  RefItem = 7
  noact.Enabled = True
  HelpAnchor = SSCommand3.Tag
End Sub

Private Sub SSCommand4_GotFocus()
  RefItem = 7
  noact.Enabled = True
  HelpAnchor = SSCommand4.Tag
End Sub

Private Sub SSCommand5_GotFocus()
  RefItem = -1
  noact.Enabled = False
  HelpAnchor = Combo1.Tag
End Sub

Private Sub SStab1_GotFocus()
  RefItem = -1
  noact.Enabled = False
  HelpAnchor = SSTab1.Tag
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

Private Sub txt_GotFocus(Index As Integer)
  RefItem = Index
  noact.Enabled = True
  HelpAnchor = txt(RefItem).Tag
End Sub

Private Sub unit_GotFocus(Index As Integer)
  RefItem = Index
  noact.Enabled = True
  HelpAnchor = lbl(RefItem).Tag
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

Private Sub vaSpread3_GotFocus()
  RefItem = -1
  noact.Enabled = False
  HelpAnchor = Combo1.Tag
End Sub

Sub wffLabels()
Dim j As Long
Dim cnt As Long
    
  SSFrame2.Visible = False
  SSFrame1.Visible = False
  For j = 1 To 2
    vaSpread1.col = j
    vaSpread1.ColHidden = False
    vaSpread2.col = j
    vaSpread2.ColHidden = False
  Next
  For j = 3 To 5
    vaSpread1.col = j
    vaSpread1.ColHidden = True
    vaSpread2.col = j
    vaSpread2.ColHidden = True
  Next
  
' fix for labels on water flux tabs
  vaSpread1.Row = 0
  vaSpread1.col = 2
  If Combo2.Text = "Water" Then
    vaSpread1.Text = "Flux Rate"
  Else
    If sink(c1).media = "Surface Water" Then
      vaSpread1.Text = "Adsorbed Flux"
      vaSpread1.col = 3
      vaSpread1.ColHidden = False
      vaSpread1.Text = "Dissolved Flux"
      cnt = 3
    Else
      vaSpread1.Text = "Mass Flux"
      cnt = 2
    End If
  End If
  
  vaSpread2.Row = 0
  vaSpread2.col = 2
  If Combo3.Text = "Water" Then
    vaSpread2.Text = "Flux Rate"
  Else
    If sink(c1).media = "Surface Water" Then
      vaSpread2.Text = "Adsorbed Flux"
      vaSpread2.col = 3
      vaSpread2.ColHidden = False
      vaSpread2.Text = "Dissolved Flux"
    Else
      vaSpread2.Text = "Mass Flux"
    End If
  End If
  
  SSFrame2.Top = 1740
  SSFrame2.Height = 4600
  vaSpread1.VisibleRows = 14
  vaSpread2.VisibleRows = 14
  vaSpread1.VisibleCols = cnt
  vaSpread2.VisibleCols = cnt
  
  lbl(0).Caption = "Width of flux plane"
  SSCommand5.Visible = False
  SetComboVisible False
  SetVisible 0, True
  SetVisible 1, True
  SetVisible 3, False
  SetVisible 4, False
  SetVisible 5, False
  Select Case sink(c1).media
  Case "Vadose"
    lbl(1).Caption = "Length of flux plane"
    SetVisible 2, False
  Case "Aquifer"
    lbl(1).Caption = "Height of flux plane"
    lbl(2).Caption = "Distance to source below water table"
    SetVisible 2, True
  Case "Surface Water"
    lbl(1).Caption = "Height of flux plane"
    SetVisible 2, False
  End Select
  SSFrame1.Visible = True
  SSFrame2.Visible = True
End Sub

Sub affLabels()
Dim j As Long
Dim cnt As Long
  
  SSFrame2.Visible = False
  SSFrame1.Visible = False
  
  cnt = 1
  vaSpread1.col = 1
  vaSpread1.ColHidden = False
  vaSpread2.col = 1
  vaSpread2.ColHidden = False
  For j = 2 To 5
    vaSpread1.col = j
    vaSpread1.ColHidden = Not (FluxTypes.SSCheck1(j - 2).Value)
    vaSpread2.col = j
    vaSpread2.ColHidden = Not (FluxTypes.SSCheck1(j - 2).Value)
    If Not vaSpread2.ColHidden Then cnt = cnt + 1
  Next
  vaSpread1.col = 2
  vaSpread1.Row = 0
  vaSpread1.Text = "Gas 1"
  vaSpread1.col = 3
  vaSpread1.Text = "Particle 1"
  
  vaSpread2.col = 2
  vaSpread2.Row = 0
  vaSpread2.Text = "Gas 1"
  vaSpread2.col = 3
  vaSpread2.Text = "Particle 1"
  
  SSFrame2.Top = 3000
  SSFrame2.Height = 3500
  vaSpread1.VisibleRows = 8
  vaSpread2.VisibleRows = 8
  vaSpread1.VisibleCols = cnt
  vaSpread2.VisibleCols = cnt
  
  lbl(9).Caption = "Type of release"
  lbl(0).Caption = "Exit area of source"
  lbl(1).Caption = "Exit height of source"
  lbl(2).Caption = "Height of adjacent structure"
  lbl(3).Caption = "Exit velocity of source"
  lbl(4).Caption = "Exit temperature of source"
  lbl(5).Caption = "Ambient air temperature"
  SSCommand5.Visible = True
  SetComboVisible True
  SetVisible 0, True
  SetVisible 1, True
  SetVisible 2, True
  SetVisible 3, True
  SetVisible 4, True
  SetVisible 5, True
  
  If sink(c1).media = "Area" Then
    SetEnabled 1, False
    SetEnabled 2, False
    SetEnabled 3, False
  End If
  SSFrame1.Visible = True
  SSFrame2.Visible = haveflux
End Sub

Sub scfLabels()
Dim j As Long
  
  SSFrame2.Visible = False
  SSFrame1.Visible = False
  For j = 1 To 2
    vaSpread1.col = j
    vaSpread1.ColHidden = False
    vaSpread2.col = j
    vaSpread2.ColHidden = False
  Next
  For j = 3 To 5
    vaSpread1.col = j
    vaSpread1.ColHidden = True
    vaSpread2.col = j
    vaSpread2.ColHidden = True
  Next
  vaSpread1.col = 2
  vaSpread1.Row = 0
  vaSpread1.Text = "Concentration"
  vaSpread2.col = 2
  vaSpread2.Row = 0
  vaSpread2.Text = "Concentration"
  
  SSFrame2.Top = 1740
  SSFrame2.Height = 4600
  vaSpread1.VisibleRows = 14
  vaSpread2.VisibleRows = 14
  vaSpread1.VisibleCols = 2
  vaSpread2.VisibleCols = 2
  
  lbl(0).Caption = "Width of contaminated " & LCase(sink(c1).media)
  lbl(1).Caption = "Length of contaminated " & LCase(sink(c1).media)
  lbl(2).Caption = "Depth of contaminated " & LCase(sink(c1).media)
  
  SSCommand5.Visible = False
  SetComboVisible False
  SetVisible 0, True
  SetVisible 1, True
  SetVisible 2, True
  SetVisible 3, False
  SetVisible 4, False
  SetVisible 5, False
  SSFrame1.Visible = True
  SSFrame2.Visible = True
End Sub

Sub wcfLabels()
Dim j As Long
  
  SSFrame2.Visible = False
  SSFrame1.Visible = False
  For j = 1 To 2
    vaSpread1.col = j
    vaSpread1.ColHidden = False
    vaSpread2.col = j
    vaSpread2.ColHidden = False
  Next
  For j = 3 To 5
    vaSpread1.col = j
    vaSpread1.ColHidden = True
    vaSpread2.col = j
    vaSpread2.ColHidden = True
  Next
  vaSpread1.col = 2
  vaSpread1.Row = 0
  vaSpread1.Text = "Concentration"
  vaSpread2.col = 2
  vaSpread2.Row = 0
  vaSpread2.Text = "Concentration"
  
  SSFrame2.Top = 300
  SSFrame2.Height = 6100
  vaSpread1.VisibleRows = 20
  vaSpread2.VisibleRows = 20
  vaSpread1.VisibleCols = 2
  vaSpread2.VisibleCols = 2
  
  lbl(0).Caption = ""
  lbl(1).Caption = ""
  lbl(2).Caption = ""
  
  SSCommand5.Visible = False
  SetComboVisible False
  SetVisible 0, False
  SetVisible 1, False
  SetVisible 2, False
  SetVisible 3, False
  SetVisible 4, False
  SetVisible 5, False
  SSFrame1.Visible = True
  SSFrame2.Visible = True
End Sub

Sub SetComboVisible(v As Boolean)
  lbl(9).Visible = v
  Combo1.Visible = v
End Sub

Sub SetVisible(x As Long, v As Boolean)
  lbl(x).Visible = v
  txt(x).Visible = v
  unit(x).Visible = v
  ref(x).Visible = v
  SetEnabled x, v
End Sub

Sub SetEnabled(x As Long, v As Boolean)
  lbl(x).Enabled = v
  txt(x).Enabled = v
  unit(x).Enabled = v
  ref(x).Enabled = v
  If Not v Then txt(x).Text = ""
End Sub

Private Sub vaSpread1_RightClick(ByVal ClickType As Integer, ByVal col As Long, ByVal Row As Long, ByVal MouseX As Long, ByVal MouseY As Long)
    'Set global variables
    With vaSpread1
        BeginCol = .SelBlockCol
        EndCol = .SelBlockCol2
        BeginRow = .SelBlockRow
        EndRow = .SelBlockRow2
    End With
    'Copy, cut, paste popup menu
    PopupMenu SpreadEdit, vbPopupMenuLeftAlign
End Sub

Private Sub vaSpread2_RightClick(ByVal ClickType As Integer, ByVal col As Long, ByVal Row As Long, ByVal MouseX As Long, ByVal MouseY As Long)
    'Set global variables
    With vaSpread2
        BeginCol = .SelBlockCol
        EndCol = .SelBlockCol2
        BeginRow = .SelBlockRow
        EndRow = .SelBlockRow2
    End With
    'Copy, cut, paste popup menu
    PopupMenu SpreadEdit, vbPopupMenuLeftAlign
End Sub

'Copy, cut, paste procedures in invisible menu
Private Sub SpreadCopy_Click()
    Call vaSpreadCopy
End Sub
Private Sub SpreadCut_Click()
    Call vaSpreadCut
End Sub
Private Sub SpreadPaste_Click()
    Call vaSpreadPaste
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
