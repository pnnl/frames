VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Object = "{F856EC8B-F03C-4515-BDC6-64CBD617566A}#7.0#0"; "FPSPR70.ocx"
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "tabctl32.ocx"
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "threed32.ocx"
Begin VB.Form Atmospheric 
   AutoRedraw      =   -1  'True
   BorderStyle     =   1  'Fixed Single
   ClientHeight    =   5748
   ClientLeft      =   7800
   ClientTop       =   6048
   ClientWidth     =   7692
   Icon            =   "Atmospheric.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   NegotiateMenus  =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   479
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   641
   StartUpPosition =   2  'CenterScreen
   Begin VB.Timer Timer1 
      Interval        =   100
      Left            =   0
      Top             =   0
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
      TabIndex        =   109
      TabStop         =   0   'False
      Top             =   5400
      Width           =   7692
   End
   Begin TabDlg.SSTab SSTab1 
      Height          =   5412
      Index           =   0
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   7692
      _ExtentX        =   13568
      _ExtentY        =   9546
      _Version        =   393216
      Style           =   1
      TabHeight       =   529
      TabCaption(0)   =   "Climatology"
      TabPicture(0)   =   "Atmospheric.frx":030A
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "frame(0)"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).Control(1)=   "openit"
      Tab(0).Control(1).Enabled=   0   'False
      Tab(0).ControlCount=   2
      TabCaption(1)   =   "Joint Frequency Data"
      TabPicture(1)   =   "Atmospheric.frx":0326
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "SSTab1(1)"
      Tab(1).Control(0).Enabled=   0   'False
      Tab(1).ControlCount=   1
      TabCaption(2)   =   "Topographical Data"
      TabPicture(2)   =   "Atmospheric.frx":0342
      Tab(2).ControlEnabled=   0   'False
      Tab(2).Control(0)=   "SSTab1(2)"
      Tab(2).Control(1)=   "frame(9)"
      Tab(2).ControlCount=   2
      Begin MSComDlg.CommonDialog openit 
         Left            =   7320
         Top             =   0
         _ExtentX        =   699
         _ExtentY        =   699
         _Version        =   393216
      End
      Begin TabDlg.SSTab SSTab1 
         Height          =   3012
         Index           =   2
         Left            =   -74856
         TabIndex        =   79
         Top             =   2088
         Width           =   7452
         _ExtentX        =   13123
         _ExtentY        =   5313
         _Version        =   393216
         Style           =   1
         TabsPerRow      =   4
         TabHeight       =   529
         TabCaption(0)   =   "Surface Roughness"
         TabPicture(0)   =   "Atmospheric.frx":035E
         Tab(0).ControlEnabled=   -1  'True
         Tab(0).Control(0)=   "frame(10)"
         Tab(0).Control(0).Enabled=   0   'False
         Tab(0).ControlCount=   1
         TabCaption(1)   =   "Terrain Heights"
         TabPicture(1)   =   "Atmospheric.frx":037A
         Tab(1).ControlEnabled=   0   'False
         Tab(1).Control(0)=   "frame(11)"
         Tab(1).ControlCount=   1
         TabCaption(2)   =   "Wind Channeling"
         TabPicture(2)   =   "Atmospheric.frx":0396
         Tab(2).ControlEnabled=   0   'False
         Tab(2).Control(0)=   "frame(12)"
         Tab(2).ControlCount=   1
         Begin Threed.SSFrame frame 
            Height          =   2292
            Index           =   12
            Left            =   -74760
            TabIndex        =   89
            Top             =   480
            Width           =   6972
            _Version        =   65536
            _ExtentX        =   12298
            _ExtentY        =   4043
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
            Begin VB.ComboBox arcdir 
               Height          =   288
               ItemData        =   "Atmospheric.frx":03B2
               Left            =   3840
               List            =   "Atmospheric.frx":03E6
               TabIndex        =   107
               Tag             =   "ARCDIR"
               Text            =   "Combo1"
               Top             =   1800
               Width           =   1932
            End
            Begin VB.ComboBox unit 
               Height          =   288
               Index           =   27
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   104
               Tag             =   "km"
               Top             =   1440
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               Height          =   288
               Index           =   26
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   100
               Tag             =   "km"
               Top             =   1080
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               Height          =   288
               Index           =   25
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   96
               Tag             =   "km"
               Top             =   720
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               Height          =   288
               Index           =   24
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   92
               Tag             =   "km"
               Top             =   360
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H008080FF&
               Height          =   288
               Index           =   27
               Left            =   3840
               TabIndex        =   103
               Tag             =   "ARCWIDTH"
               Top             =   1440
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H008080FF&
               Height          =   288
               Index           =   26
               Left            =   3840
               TabIndex        =   99
               Tag             =   "ARCHGT"
               Top             =   1080
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H008080FF&
               Height          =   288
               Index           =   25
               Left            =   3840
               TabIndex        =   95
               Tag             =   "ARCSLOPE"
               Top             =   720
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H008080FF&
               Height          =   288
               Index           =   24
               Left            =   3840
               TabIndex        =   91
               Tag             =   "ARCLEN"
               Top             =   360
               Width           =   1000
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   28
               Left            =   5880
               TabIndex        =   108
               Tag             =   "0"
               Top             =   1800
               Width           =   996
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   27
               Left            =   5880
               TabIndex        =   105
               Tag             =   "0"
               Top             =   1440
               Width           =   996
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   26
               Left            =   5880
               TabIndex        =   101
               Tag             =   "0"
               Top             =   1080
               Width           =   996
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   25
               Left            =   5880
               TabIndex        =   97
               Tag             =   "0"
               Top             =   720
               Width           =   996
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   24
               Left            =   5880
               TabIndex        =   93
               Tag             =   "0"
               Top             =   360
               Width           =   996
            End
            Begin VB.Label lbl 
               Caption         =   "Index for channel direction  (AR-CDIR)"
               Height          =   252
               Index           =   28
               Left            =   360
               TabIndex        =   106
               Tag             =   "ARCDIR"
               Top             =   1800
               Width           =   3400
            End
            Begin VB.Label lbl 
               Caption         =   "Width of channel  (AR-CWIDTH)"
               Height          =   252
               Index           =   27
               Left            =   360
               TabIndex        =   102
               Tag             =   "ARCWIDTH"
               Top             =   1440
               Width           =   3400
            End
            Begin VB.Label lbl 
               Caption         =   "Height of channel  (AR-CHGT)"
               Height          =   252
               Index           =   26
               Left            =   360
               TabIndex        =   98
               Tag             =   "ARCHGT"
               Top             =   1080
               Width           =   3400
            End
            Begin VB.Label lbl 
               Caption         =   "Slope of channel  (AR-CSLOPE)"
               Height          =   252
               Index           =   25
               Left            =   360
               TabIndex        =   94
               Tag             =   "ARCSLOPE"
               Top             =   720
               Width           =   3400
            End
            Begin VB.Label lbl 
               Caption         =   "Length of channel  (AR-CLEN)"
               Height          =   252
               Index           =   24
               Left            =   360
               TabIndex        =   90
               Tag             =   "ARCLEN"
               Top             =   360
               Width           =   3400
            End
         End
         Begin Threed.SSFrame frame 
            Height          =   2292
            Index           =   11
            Left            =   -74748
            TabIndex        =   84
            Top             =   504
            Width           =   6972
            _Version        =   65536
            _ExtentX        =   12298
            _ExtentY        =   4043
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
            Begin FPSpreadADO.fpSpread tophts 
               Height          =   1392
               Left            =   120
               TabIndex        =   88
               Tag             =   "ARTOPHTS"
               Top             =   720
               Width           =   6732
               _Version        =   458752
               _ExtentX        =   11874
               _ExtentY        =   2455
               _StockProps     =   64
               AutoCalc        =   0   'False
               BackColorStyle  =   1
               EditEnterAction =   5
               BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
                  Name            =   "MS Sans Serif"
                  Size            =   7.8
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               FormulaSync     =   0   'False
               MaxCols         =   16
               MaxRows         =   4
               ScrollBars      =   1
               SpreadDesigner  =   "Atmospheric.frx":04B4
               VisibleCols     =   5
               VisibleRows     =   4
            End
            Begin VB.ComboBox unit 
               Height          =   288
               Index           =   22
               Left            =   4560
               Style           =   2  'Dropdown List
               TabIndex        =   86
               Tag             =   "m"
               Top             =   240
               Width           =   1000
            End
            Begin VB.Label lbl 
               Caption         =   "Regional distribution of terrain heights  (AR-TOPHTS)"
               Height          =   252
               Index           =   22
               Left            =   120
               TabIndex        =   85
               Tag             =   "ARTOPHTS"
               Top             =   240
               Width           =   4332
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   22
               Left            =   5640
               TabIndex        =   87
               Tag             =   "0"
               Top             =   240
               Width           =   996
            End
         End
         Begin Threed.SSFrame frame 
            Height          =   2292
            Index           =   10
            Left            =   252
            TabIndex        =   110
            Top             =   504
            Width           =   6972
            _Version        =   65536
            _ExtentX        =   12298
            _ExtentY        =   4043
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
            Begin FPSpreadADO.fpSpread regsur 
               Height          =   1392
               Left            =   120
               TabIndex        =   83
               Tag             =   "ARREGSUR"
               Top             =   720
               Width           =   6732
               _Version        =   458752
               _ExtentX        =   11874
               _ExtentY        =   2455
               _StockProps     =   64
               AutoCalc        =   0   'False
               BackColorStyle  =   1
               EditEnterAction =   5
               EditModePermanent=   -1  'True
               BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
                  Name            =   "MS Sans Serif"
                  Size            =   7.8
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               FormulaSync     =   0   'False
               MaxCols         =   8
               MaxRows         =   4
               ScrollBars      =   1
               SelectBlockOptions=   0
               SpreadDesigner  =   "Atmospheric.frx":1663
               VisibleCols     =   5
               VisibleRows     =   4
            End
            Begin VB.ComboBox unit 
               Height          =   288
               Index           =   19
               Left            =   4560
               Style           =   2  'Dropdown List
               TabIndex        =   81
               Tag             =   "cm"
               Top             =   240
               Width           =   1000
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   19
               Left            =   5640
               TabIndex        =   82
               Tag             =   "0"
               Top             =   240
               Width           =   996
            End
            Begin VB.Label lbl 
               Caption         =   "Regional surface roughness lengths (AR-REGSUR)"
               Height          =   252
               Index           =   19
               Left            =   120
               TabIndex        =   80
               Tag             =   "ARREGSUR"
               Top             =   240
               Width           =   4332
            End
         End
      End
      Begin TabDlg.SSTab SSTab1 
         Height          =   4812
         Index           =   1
         Left            =   -74856
         TabIndex        =   23
         Tag             =   "ajjfname"
         Top             =   480
         Width           =   7452
         _ExtentX        =   13123
         _ExtentY        =   8467
         _Version        =   393216
         Style           =   1
         Tabs            =   8
         TabsPerRow      =   8
         TabHeight       =   529
         TabCaption(0)   =   "General"
         TabPicture(0)   =   "Atmospheric.frx":238A
         Tab(0).ControlEnabled=   -1  'True
         Tab(0).Control(0)=   "frame(1)"
         Tab(0).Control(0).Enabled=   0   'False
         Tab(0).ControlCount=   1
         TabCaption(1)   =   "Class A"
         TabPicture(1)   =   "Atmospheric.frx":23A6
         Tab(1).ControlEnabled=   0   'False
         Tab(1).Control(0)=   "frame(2)"
         Tab(1).ControlCount=   1
         TabCaption(2)   =   "Class B"
         TabPicture(2)   =   "Atmospheric.frx":23C2
         Tab(2).ControlEnabled=   0   'False
         Tab(2).Control(0)=   "frame(3)"
         Tab(2).ControlCount=   1
         TabCaption(3)   =   "Class C"
         TabPicture(3)   =   "Atmospheric.frx":23DE
         Tab(3).ControlEnabled=   0   'False
         Tab(3).Control(0)=   "frame(4)"
         Tab(3).ControlCount=   1
         TabCaption(4)   =   "Class D"
         TabPicture(4)   =   "Atmospheric.frx":23FA
         Tab(4).ControlEnabled=   0   'False
         Tab(4).Control(0)=   "frame(5)"
         Tab(4).ControlCount=   1
         TabCaption(5)   =   "Class E"
         TabPicture(5)   =   "Atmospheric.frx":2416
         Tab(5).ControlEnabled=   0   'False
         Tab(5).Control(0)=   "frame(6)"
         Tab(5).ControlCount=   1
         TabCaption(6)   =   "Class F"
         TabPicture(6)   =   "Atmospheric.frx":2432
         Tab(6).ControlEnabled=   0   'False
         Tab(6).Control(0)=   "frame(7)"
         Tab(6).ControlCount=   1
         TabCaption(7)   =   "Class G"
         TabPicture(7)   =   "Atmospheric.frx":244E
         Tab(7).ControlEnabled=   0   'False
         Tab(7).Control(0)=   "frame(8)"
         Tab(7).ControlCount=   1
         Begin Threed.SSFrame frame 
            Height          =   4332
            Index           =   1
            Left            =   240
            TabIndex        =   24
            Top             =   360
            Width           =   6972
            _Version        =   65536
            _ExtentX        =   12298
            _ExtentY        =   7641
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
            Begin VB.CommandButton JFDExport 
               Caption         =   "Export Joint Frequency Data"
               Height          =   375
               Left            =   3480
               TabIndex        =   111
               Top             =   3720
               Width           =   3135
            End
            Begin FPSpreadADO.fpSpread winds 
               Height          =   1380
               Left            =   720
               TabIndex        =   43
               Tag             =   "AJWINDS"
               Top             =   1692
               Width           =   1008
               _Version        =   458752
               _ExtentX        =   1757
               _ExtentY        =   2434
               _StockProps     =   64
               AutoCalc        =   0   'False
               AutoSize        =   -1  'True
               BackColorStyle  =   1
               DisplayColHeaders=   0   'False
               DisplayRowHeaders=   0   'False
               EditEnterAction =   5
               EditModePermanent=   -1  'True
               BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
                  Name            =   "MS Sans Serif"
                  Size            =   7.8
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               FormulaSync     =   0   'False
               MaxCols         =   1
               MaxRows         =   6
               ScrollBarExtMode=   -1  'True
               ScrollBars      =   0
               SelectBlockOptions=   0
               SpreadDesigner  =   "Atmospheric.frx":246A
               UserResize      =   0
               VisibleCols     =   1
               VisibleRows     =   6
            End
            Begin FPSpreadADO.fpSpread calms 
               Height          =   1608
               Left            =   4068
               TabIndex        =   54
               Tag             =   "AJCALMS"
               Top             =   1680
               Width           =   1008
               _Version        =   458752
               _ExtentX        =   1757
               _ExtentY        =   2836
               _StockProps     =   64
               AutoCalc        =   0   'False
               AutoSize        =   -1  'True
               BackColorStyle  =   1
               DisplayColHeaders=   0   'False
               DisplayRowHeaders=   0   'False
               EditEnterAction =   5
               EditModePermanent=   -1  'True
               BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
                  Name            =   "MS Sans Serif"
                  Size            =   7.8
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               FormulaSync     =   0   'False
               MaxCols         =   1
               MaxRows         =   7
               ScrollBars      =   0
               SelectBlockOptions=   0
               SpreadDesigner  =   "Atmospheric.frx":2822
               UserResize      =   0
               VisibleCols     =   1
               VisibleRows     =   7
            End
            Begin VB.CommandButton JFDImport 
               Caption         =   "Import Joint Frequency Data"
               Height          =   372
               Left            =   360
               TabIndex        =   57
               Top             =   3720
               Width           =   3015
            End
            Begin VB.ComboBox unit 
               Height          =   288
               Index           =   17
               ItemData        =   "Atmospheric.frx":2C0A
               Left            =   5148
               List            =   "Atmospheric.frx":2C17
               Style           =   2  'Dropdown List
               TabIndex        =   55
               Top             =   1680
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               Height          =   288
               Index           =   14
               Left            =   1788
               Style           =   2  'Dropdown List
               TabIndex        =   44
               Tag             =   "m/s"
               Top             =   1680
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               Height          =   315
               Index           =   13
               Left            =   4680
               Style           =   2  'Dropdown List
               TabIndex        =   34
               Tag             =   "cm"
               Top             =   960
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               Height          =   315
               Index           =   12
               Left            =   4680
               Style           =   2  'Dropdown List
               TabIndex        =   30
               Tag             =   "m"
               Top             =   600
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H008080FF&
               Height          =   288
               Index           =   13
               Left            =   3720
               TabIndex        =   33
               Tag             =   "AJRLEN"
               Top             =   960
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H008080FF&
               Height          =   288
               Index           =   12
               Left            =   3720
               TabIndex        =   29
               Tag             =   "AJANEMHT"
               Top             =   600
               Width           =   1000
            End
            Begin VB.TextBox txt 
               BackColor       =   &H00C0FFC0&
               Height          =   288
               Index           =   11
               Left            =   3720
               TabIndex        =   26
               Tag             =   "AJSTATNM"
               Top             =   240
               Width           =   1932
            End
            Begin VB.Label lbl 
               Caption         =   "Class G"
               Height          =   252
               Index           =   31
               Left            =   3480
               TabIndex        =   53
               Tag             =   "AJSTATNM"
               Top             =   3120
               Width           =   612
            End
            Begin VB.Label lbl 
               Caption         =   "Class F"
               Height          =   252
               Index           =   30
               Left            =   3480
               TabIndex        =   52
               Tag             =   "AJSTATNM"
               Top             =   2880
               Width           =   612
            End
            Begin VB.Label lbl 
               Caption         =   "Class E"
               Height          =   252
               Index           =   29
               Left            =   3480
               TabIndex        =   51
               Tag             =   "AJSTATNM"
               Top             =   2640
               Width           =   612
            End
            Begin VB.Label lbl 
               Caption         =   "Class D"
               Height          =   252
               Index           =   23
               Left            =   3480
               TabIndex        =   50
               Tag             =   "AJSTATNM"
               Top             =   2400
               Width           =   612
            End
            Begin VB.Label lbl 
               Caption         =   "Class C"
               Height          =   252
               Index           =   20
               Left            =   3480
               TabIndex        =   49
               Tag             =   "AJSTATNM"
               Top             =   2160
               Width           =   612
            End
            Begin VB.Label lbl 
               Caption         =   "Class B"
               Height          =   252
               Index           =   18
               Left            =   3480
               TabIndex        =   48
               Tag             =   "AJSTATNM"
               Top             =   1920
               Width           =   612
            End
            Begin VB.Label lbl 
               Caption         =   "Class A"
               Height          =   252
               Index           =   16
               Left            =   3480
               TabIndex        =   47
               Tag             =   "AJSTATNM"
               Top             =   1680
               Width           =   612
            End
            Begin VB.Label lbl 
               Caption         =   "Group 6"
               Height          =   252
               Index           =   15
               Left            =   120
               TabIndex        =   42
               Tag             =   "AJSTATNM"
               Top             =   2880
               Width           =   612
            End
            Begin VB.Label lbl 
               Caption         =   "Group 5"
               Height          =   252
               Index           =   8
               Left            =   120
               TabIndex        =   41
               Tag             =   "AJSTATNM"
               Top             =   2640
               Width           =   612
            End
            Begin VB.Label lbl 
               Caption         =   "Group 4"
               Height          =   252
               Index           =   7
               Left            =   120
               TabIndex        =   40
               Tag             =   "AJSTATNM"
               Top             =   2400
               Width           =   612
            End
            Begin VB.Label lbl 
               Caption         =   "Group 3"
               Height          =   252
               Index           =   6
               Left            =   120
               TabIndex        =   39
               Tag             =   "AJSTATNM"
               Top             =   2160
               Width           =   612
            End
            Begin VB.Label lbl 
               Caption         =   "Group 2"
               Height          =   252
               Index           =   5
               Left            =   120
               TabIndex        =   38
               Tag             =   "AJSTATNM"
               Top             =   1920
               Width           =   612
            End
            Begin VB.Label lbl 
               Caption         =   "Group 1"
               Height          =   252
               Index           =   2
               Left            =   120
               TabIndex        =   37
               Tag             =   "AJSTATNM"
               Top             =   1680
               Width           =   612
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   18
               Left            =   6348
               TabIndex        =   56
               Tag             =   "0"
               Top             =   1680
               Width           =   500
            End
            Begin VB.Label lbl 
               Caption         =   "Wind joint frequency calms  (AJ-CALMS)"
               Height          =   252
               Index           =   17
               Left            =   3480
               TabIndex        =   46
               Tag             =   "AJCALMS"
               Top             =   1440
               Width           =   3396
            End
            Begin VB.Label lbl 
               Caption         =   "Wind speed midpoints  (AJ-WINDS)"
               Height          =   252
               Index           =   14
               Left            =   120
               TabIndex        =   36
               Tag             =   "AJWINDS"
               Top             =   1440
               Width           =   3396
            End
            Begin VB.Label lbl 
               Caption         =   "Average roughness length  (AJ-RLEN)"
               Height          =   252
               Index           =   13
               Left            =   120
               TabIndex        =   32
               Tag             =   "AJRLEN"
               Top             =   960
               Width           =   3400
            End
            Begin VB.Label lbl 
               Caption         =   "Anemonometer height  (AJ-ANEMHT)"
               Height          =   252
               Index           =   12
               Left            =   120
               TabIndex        =   28
               Tag             =   "AJANEMHT"
               Top             =   600
               Width           =   3400
            End
            Begin VB.Label lbl 
               Caption         =   "Data station  (AJ-STATNM)"
               Height          =   252
               Index           =   11
               Left            =   120
               TabIndex        =   25
               Tag             =   "AJSTATNM"
               Top             =   240
               Width           =   3400
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   13
               Left            =   5880
               TabIndex        =   35
               Tag             =   "0"
               Top             =   1020
               Width           =   996
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   11
               Left            =   5880
               TabIndex        =   27
               Tag             =   "0"
               Top             =   240
               Width           =   996
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   12
               Left            =   5880
               TabIndex        =   31
               Tag             =   "0"
               Top             =   660
               Width           =   996
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   255
               Index           =   14
               Left            =   2880
               TabIndex        =   45
               Tag             =   "0"
               Top             =   1680
               Width           =   990
            End
         End
         Begin Threed.SSFrame frame 
            Height          =   4332
            Index           =   2
            Left            =   -74760
            TabIndex        =   58
            Top             =   360
            Width           =   6972
            _Version        =   65536
            _ExtentX        =   12298
            _ExtentY        =   7641
            _StockProps     =   14
            Caption         =   "Wind Speed Groups"
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
            Begin FPSpreadADO.fpSpread fdata 
               Height          =   3888
               Index           =   0
               Left            =   240
               TabIndex        =   59
               Top             =   240
               Width           =   6444
               _Version        =   458752
               _ExtentX        =   11218
               _ExtentY        =   6858
               _StockProps     =   64
               AutoCalc        =   0   'False
               AutoSize        =   -1  'True
               BackColorStyle  =   1
               EditEnterAction =   5
               EditModePermanent=   -1  'True
               BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
                  Name            =   "MS Sans Serif"
                  Size            =   7.8
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               FormulaSync     =   0   'False
               MaxCols         =   6
               MaxRows         =   16
               ScrollBars      =   0
               SelectBlockOptions=   0
               SpreadDesigner  =   "Atmospheric.frx":2C36
               UserResize      =   0
               VisibleCols     =   6
               VisibleRows     =   16
            End
         End
         Begin Threed.SSFrame frame 
            Height          =   4332
            Index           =   3
            Left            =   -74760
            TabIndex        =   60
            Top             =   360
            Width           =   6972
            _Version        =   65536
            _ExtentX        =   12298
            _ExtentY        =   7641
            _StockProps     =   14
            Caption         =   "Wind Speed Groups"
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
            Begin FPSpreadADO.fpSpread fdata 
               Height          =   3888
               Index           =   1
               Left            =   240
               TabIndex        =   61
               Top             =   240
               Width           =   6444
               _Version        =   458752
               _ExtentX        =   11218
               _ExtentY        =   6858
               _StockProps     =   64
               AutoCalc        =   0   'False
               AutoSize        =   -1  'True
               BackColorStyle  =   1
               EditEnterAction =   5
               EditModePermanent=   -1  'True
               BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
                  Name            =   "MS Sans Serif"
                  Size            =   7.8
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               FormulaSync     =   0   'False
               MaxCols         =   6
               MaxRows         =   16
               ScrollBars      =   0
               SelectBlockOptions=   0
               SpreadDesigner  =   "Atmospheric.frx":40AA
               UserResize      =   0
               VisibleCols     =   6
               VisibleRows     =   16
            End
         End
         Begin Threed.SSFrame frame 
            Height          =   4332
            Index           =   4
            Left            =   -74760
            TabIndex        =   62
            Top             =   360
            Width           =   6972
            _Version        =   65536
            _ExtentX        =   12298
            _ExtentY        =   7641
            _StockProps     =   14
            Caption         =   "Wind Speed Groups"
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
            Begin FPSpreadADO.fpSpread fdata 
               Height          =   3888
               Index           =   2
               Left            =   240
               TabIndex        =   63
               Top             =   240
               Width           =   6444
               _Version        =   458752
               _ExtentX        =   11218
               _ExtentY        =   6858
               _StockProps     =   64
               AutoCalc        =   0   'False
               AutoSize        =   -1  'True
               BackColorStyle  =   1
               EditEnterAction =   5
               EditModePermanent=   -1  'True
               BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
                  Name            =   "MS Sans Serif"
                  Size            =   7.8
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               FormulaSync     =   0   'False
               MaxCols         =   6
               MaxRows         =   16
               ScrollBars      =   0
               SelectBlockOptions=   0
               SpreadDesigner  =   "Atmospheric.frx":551E
               UserResize      =   0
               VisibleCols     =   6
               VisibleRows     =   16
            End
         End
         Begin Threed.SSFrame frame 
            Height          =   4332
            Index           =   5
            Left            =   -74760
            TabIndex        =   64
            Top             =   360
            Width           =   6972
            _Version        =   65536
            _ExtentX        =   12298
            _ExtentY        =   7641
            _StockProps     =   14
            Caption         =   "Wind Speed Groups"
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
            Begin FPSpreadADO.fpSpread fdata 
               Height          =   3888
               Index           =   3
               Left            =   240
               TabIndex        =   65
               Top             =   240
               Width           =   6444
               _Version        =   458752
               _ExtentX        =   11218
               _ExtentY        =   6858
               _StockProps     =   64
               AutoCalc        =   0   'False
               AutoSize        =   -1  'True
               BackColorStyle  =   1
               EditEnterAction =   5
               EditModePermanent=   -1  'True
               BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
                  Name            =   "MS Sans Serif"
                  Size            =   7.8
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               FormulaSync     =   0   'False
               MaxCols         =   6
               MaxRows         =   16
               ScrollBars      =   0
               SelectBlockOptions=   0
               SpreadDesigner  =   "Atmospheric.frx":6992
               UserResize      =   0
               VisibleCols     =   6
               VisibleRows     =   16
            End
         End
         Begin Threed.SSFrame frame 
            Height          =   4332
            Index           =   6
            Left            =   -74760
            TabIndex        =   66
            Top             =   360
            Width           =   6972
            _Version        =   65536
            _ExtentX        =   12298
            _ExtentY        =   7641
            _StockProps     =   14
            Caption         =   "Wind Speed Groups"
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
            Begin FPSpreadADO.fpSpread fdata 
               Height          =   3888
               Index           =   4
               Left            =   240
               TabIndex        =   67
               Top             =   240
               Width           =   6444
               _Version        =   458752
               _ExtentX        =   11218
               _ExtentY        =   6858
               _StockProps     =   64
               AutoCalc        =   0   'False
               AutoSize        =   -1  'True
               BackColorStyle  =   1
               EditEnterAction =   5
               EditModePermanent=   -1  'True
               BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
                  Name            =   "MS Sans Serif"
                  Size            =   7.8
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               FormulaSync     =   0   'False
               MaxCols         =   6
               MaxRows         =   16
               ScrollBars      =   0
               SelectBlockOptions=   0
               SpreadDesigner  =   "Atmospheric.frx":7E06
               UserResize      =   0
               VisibleCols     =   6
               VisibleRows     =   16
            End
         End
         Begin Threed.SSFrame frame 
            Height          =   4332
            Index           =   7
            Left            =   -74760
            TabIndex        =   68
            Top             =   360
            Width           =   6972
            _Version        =   65536
            _ExtentX        =   12298
            _ExtentY        =   7641
            _StockProps     =   14
            Caption         =   "Wind Speed Groups"
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
            Begin FPSpreadADO.fpSpread fdata 
               Height          =   3888
               Index           =   5
               Left            =   240
               TabIndex        =   69
               Top             =   240
               Width           =   6444
               _Version        =   458752
               _ExtentX        =   11218
               _ExtentY        =   6858
               _StockProps     =   64
               AutoCalc        =   0   'False
               AutoSize        =   -1  'True
               BackColorStyle  =   1
               EditEnterAction =   5
               EditModePermanent=   -1  'True
               BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
                  Name            =   "MS Sans Serif"
                  Size            =   7.8
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               FormulaSync     =   0   'False
               MaxCols         =   6
               MaxRows         =   16
               ScrollBars      =   0
               SelectBlockOptions=   0
               SpreadDesigner  =   "Atmospheric.frx":927A
               UserResize      =   0
               VisibleCols     =   6
               VisibleRows     =   16
            End
         End
         Begin Threed.SSFrame frame 
            Height          =   4332
            Index           =   8
            Left            =   -74760
            TabIndex        =   70
            Top             =   360
            Width           =   6972
            _Version        =   65536
            _ExtentX        =   12298
            _ExtentY        =   7641
            _StockProps     =   14
            Caption         =   "Wind Speed Groups"
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
            Begin FPSpreadADO.fpSpread fdata 
               Height          =   3888
               Index           =   6
               Left            =   240
               TabIndex        =   71
               Top             =   240
               Width           =   6444
               _Version        =   458752
               _ExtentX        =   11218
               _ExtentY        =   6858
               _StockProps     =   64
               AutoCalc        =   0   'False
               AutoSize        =   -1  'True
               BackColorStyle  =   1
               EditEnterAction =   5
               EditModePermanent=   -1  'True
               BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
                  Name            =   "MS Sans Serif"
                  Size            =   7.8
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               FormulaSync     =   0   'False
               MaxCols         =   6
               MaxRows         =   16
               ScrollBars      =   0
               SelectBlockOptions=   0
               SpreadDesigner  =   "Atmospheric.frx":A6EE
               UserResize      =   0
               VisibleCols     =   6
               VisibleRows     =   16
            End
         End
      End
      Begin Threed.SSFrame frame 
         Height          =   1452
         Index           =   9
         Left            =   -74820
         TabIndex        =   72
         Top             =   480
         Width           =   7332
         _Version        =   65536
         _ExtentX        =   12933
         _ExtentY        =   2561
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
            BackColor       =   &H00E0E0E0&
            Enabled         =   0   'False
            Height          =   285
            Index           =   21
            Left            =   4200
            TabIndex        =   76
            Tag             =   "ARTOPBAS"
            Top             =   960
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Enabled         =   0   'False
            Height          =   288
            Index           =   21
            Left            =   5160
            Style           =   2  'Dropdown List
            TabIndex        =   77
            Tag             =   "m"
            Top             =   960
            Width           =   1000
         End
         Begin Threed.SSCheck check1 
            Height          =   255
            Index           =   1
            Left            =   240
            TabIndex        =   74
            Tag             =   "ARCHANL"
            Top             =   225
            Width           =   6000
            _Version        =   65536
            _ExtentX        =   10583
            _ExtentY        =   450
            _StockProps     =   78
            Caption         =   "Complex terrain, use wind channel model  (AR-CHANL)"
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Enabled         =   0   'False
         End
         Begin Threed.SSCheck check1 
            Height          =   375
            Index           =   0
            Left            =   240
            TabIndex        =   73
            Tag             =   "ARTOPTYP"
            Top             =   510
            Width           =   6000
            _Version        =   65536
            _ExtentX        =   10583
            _ExtentY        =   661
            _StockProps     =   78
            Caption         =   "Elevated release, use regional topographical data  (AR-TOPTYP)"
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
         Begin VB.Label lbl 
            Caption         =   "Elevation of release unit (AR-TOPBAS)"
            Enabled         =   0   'False
            Height          =   255
            Index           =   21
            Left            =   480
            TabIndex        =   75
            Tag             =   "ARTOPBAS"
            Top             =   960
            Width           =   3540
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Enabled         =   0   'False
            Height          =   252
            Index           =   21
            Left            =   6240
            TabIndex        =   78
            Tag             =   "0"
            Top             =   960
            Width           =   996
         End
      End
      Begin Threed.SSFrame frame 
         Height          =   3252
         Index           =   0
         Left            =   252
         TabIndex        =   1
         Top             =   468
         Width           =   7212
         _Version        =   65536
         _ExtentX        =   12721
         _ExtentY        =   5736
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
            BackColor       =   &H008080FF&
            Height          =   288
            Index           =   10
            Left            =   5040
            TabIndex        =   18
            Tag             =   "ACPRENUM"
            Top             =   2280
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   288
            Index           =   9
            Left            =   4080
            TabIndex        =   14
            Tag             =   "ACRAIN"
            Top             =   1800
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   288
            Index           =   4
            Left            =   5040
            TabIndex        =   21
            Tag             =   "ACNUMTS"
            Top             =   2760
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H00C0FFC0&
            Height          =   288
            Index           =   3
            Left            =   4080
            TabIndex        =   3
            Tag             =   "ACLCDREF"
            Top             =   360
            Width           =   1932
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   288
            Index           =   1
            Left            =   4080
            TabIndex        =   10
            Tag             =   "ACMIXPM"
            Top             =   1320
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   288
            Index           =   0
            Left            =   4080
            TabIndex        =   6
            Tag             =   "ACMIXAM"
            Top             =   840
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   0
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   7
            Tag             =   "m"
            Top             =   840
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   1
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   11
            Tag             =   "m"
            Top             =   1320
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   9
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   15
            Tag             =   "in"
            Top             =   1800
            Width           =   1000
         End
         Begin VB.Label lbl 
            Caption         =   "Precipitation days per year  (AC-PRENUM)"
            Height          =   252
            Index           =   10
            Left            =   240
            TabIndex        =   17
            Tag             =   "ACPRENUM"
            Top             =   2280
            Width           =   3600
         End
         Begin VB.Label lbl 
            Caption         =   "Annual precipitation  (AC-RAIN)"
            Height          =   252
            Index           =   9
            Left            =   240
            TabIndex        =   13
            Tag             =   "ACRAIN"
            Top             =   1800
            Width           =   3600
         End
         Begin VB.Label lbl 
            Caption         =   "Thunderstorms per year  (AC-NUMTS)"
            Height          =   252
            Index           =   4
            Left            =   240
            TabIndex        =   20
            Tag             =   "ACNUMTS"
            Top             =   2760
            Width           =   3600
         End
         Begin VB.Label lbl 
            Caption         =   "Reference weather station  (AC-LCDREF)"
            Height          =   252
            Index           =   3
            Left            =   240
            TabIndex        =   2
            Tag             =   "ACLCDREF"
            Top             =   360
            Width           =   3600
         End
         Begin VB.Label lbl 
            Caption         =   "Afternoon mixing height  (AC-MIXPM)"
            Height          =   252
            Index           =   1
            Left            =   240
            TabIndex        =   9
            Tag             =   "ACMIXPM"
            Top             =   1320
            Width           =   3600
         End
         Begin VB.Label lbl 
            Caption         =   "Morning mixing height  (AC-MIXAM)"
            Height          =   252
            Index           =   0
            Left            =   240
            TabIndex        =   5
            Tag             =   "ACMIXAM"
            Top             =   840
            Width           =   3600
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   0
            Left            =   6120
            TabIndex        =   8
            Tag             =   "0"
            Top             =   840
            Width           =   1000
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   1
            Left            =   6120
            TabIndex        =   12
            Tag             =   "0"
            Top             =   1320
            Width           =   1000
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   3
            Left            =   6120
            TabIndex        =   4
            Tag             =   "0"
            Top             =   360
            Width           =   1000
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   4
            Left            =   6120
            TabIndex        =   22
            Tag             =   "0"
            Top             =   2760
            Width           =   1000
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   9
            Left            =   6120
            TabIndex        =   16
            Tag             =   "0"
            Top             =   1800
            Width           =   1000
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   10
            Left            =   6120
            TabIndex        =   19
            Tag             =   "0"
            Top             =   2280
            Width           =   1000
         End
      End
   End
   Begin VB.Menu menuFile 
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
      Begin VB.Menu RefAdd 
         Caption         =   "&Add"
      End
      Begin VB.Menu RefSel 
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
   Begin VB.Menu hlp 
      Caption         =   "&Help"
      Begin VB.Menu howto 
         Caption         =   "How to ..."
      End
      Begin VB.Menu about 
         Caption         =   "&About"
      End
   End
End
Attribute VB_Name = "Atmospheric"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text
Dim tophtsheader(16) As String
Dim classheader(7) As String
Dim temp As parmrec
Dim loadng As Boolean
Dim tval As Double
Dim jfloaded As Boolean
Dim unitlist As Variant

Private Sub about_Click()
  frmAbout.Show 1, Atmospheric
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

Private Sub JFDExport_Click()
Dim fhandle As Long
Dim buffer As String
Dim ord As Long
Dim fmt As String
Dim pos1 As Long
Dim pos2 As Long
Dim word As Long
Dim iclass As Long, irow As Long, icol As Long
  
  openit.CancelError = True
  openit.Filter = "Joint Frequency Data (*.jfd)|*.jfd"
  openit.DefaultExt = "jfd"
  openit.FilterIndex = 1
  openit.Flags = cdlOFNOverwritePrompt
  On Error Resume Next
  openit.ShowSave
  
  If Err.Number > 0 Or openit.filename = "" Then Exit Sub
  On Error GoTo JFDError
  fhandle = FreeFile(0)
    
  Open openit.filename For Output As #fhandle
  Print #fhandle, "6(6F10.3)," & txt(3).Text & "," & txt(11).Text
  For iclass = 0 To 6
    For irow = 1 To 16
      buffer = ""
        For icol = 1 To 6
          fdata(iclass).Row = irow
          fdata(iclass).col = icol
          buffer = buffer & Format(fdata(iclass).Value, "0.00E+00") & "  "
        Next icol
    Print #fhandle, buffer
    Next irow
  Next iclass
  
  'calms
  calms.col = 1
  buffer = ""
  
  For irow = 1 To 6
    calms.Row = irow
    buffer = buffer & Format(calms.Value, "0.00E+00") & "  "
  Next irow
  
  Print #fhandle, buffer
  calms.Row = 7
  Print #fhandle, Format(calms.Value, "0.00E+00") & "  "
  
  'winds
  winds.col = 1
  buffer = ""
  For irow = 1 To 6
    winds.Row = irow
    buffer = buffer & Format(winds.Value, "0.00E+00") & "  "
  Next irow
  Print #fhandle, buffer
  
  Close #fhandle
  MsgBox "Data successfully exported to " & openit.filename, 64, "Joint Frequency Summary Data"
  Exit Sub
  
JFDError:
  MsgBox "Incorrect format: " & fmt, 48, "Joint Frequency File Error"
  Close #fhandle
  
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

Private Sub fillet(idx As Long)
  ref(idx).Tag = temp.ref
  ref(idx).Caption = "Ref:" & Str(temp.ref)
  If temp.cunit <> "N/A" Then set_unit unit(idx), temp.uunit
  txt(idx).Text = convert(temp.cunit, temp.uunit, Val(temp.pval))
End Sub

Private Sub sfillet(idx As Long)
  txt(idx).Text = temp.pval
  ref(idx).Tag = temp.ref
  ref(idx).Caption = "Ref:" & Str(temp.ref)
End Sub

Private Sub setjfunits()
Dim i As Long
  jfloaded = True
  'joint frequency data units
  If unit(17).Tag = "" Then
    unit(17).Tag = temp.cunit
    For i = 0 To unit(17).ListCount - 1
      If unit(17).list(i) = temp.cunit Then
        unit(17).ListIndex = i
        Exit For
      End If
    Next
  End If
End Sub

Private Sub arrayfillet(idx As Long)
Dim iclass As Long
Dim i As Long
  Select Case idx
  Case 14 'winds
    winds.Row = temp.idx3
    winds.col = 1
    winds.Text = temp.pval
    winds_er
  Case 17 'fdata
    If Not jfloaded Then If Val(temp.pval) > 0 Then setjfunits
    fdata(temp.idx3 - 1).Row = temp.idx4
    fdata(temp.idx3 - 1).col = temp.idx5
    fdata(temp.idx3 - 1).Text = temp.pval
    fdata_er temp.idx3 - 1
  Case 18 'calms
    If Not jfloaded Then If Val(temp.pval) > 0 Then setjfunits
    calms.Row = temp.idx3
    calms.col = 1
    calms.Text = temp.pval
    calms_er
  Case 19 'regsur
    regsur.Row = temp.idx3
    regsur.col = temp.idx4
    regsur.Text = temp.pval
    regsur_er
  Case 22 'tophts
    tophts.Row = temp.idx3
    tophts.col = temp.idx4
    tophts.Text = temp.pval
    tophts_er
  End Select
End Sub

Private Sub loadprm()
  Dim m As Long
  Dim fle As parmfile
  Dim haveglyph As Boolean
 
  If open_parm(fle, FUIName, 2) Then
    Do Until EOCF(fle.file)
      If read_parmrec(fle, temp) Then
        Select Case temp.pname
          Case "fui"
            For m = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                If temp.idx1 = siteIdx Then
                  If temp.pname = "airdespath" Then DesName = temp.pval
                End If
              End If
            Next
          Case modName
            haveglyph = True
            Loading.Gauge1.Max = Val(temp.idx1)
            For m = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                Loading.update
                Select Case temp.pname
                  Case "CVTFormat":    CVTFormat = "General Number" 'CVTFormat = temp.pval
                  Case "airacmixam":  fillet 0
                  Case "airacmixpm":  fillet 1
                  Case "airaclcdref": sfillet 3
                  Case "airacnumts":  sfillet 4
                  Case "airacrain":   fillet 9
                  Case "airacprenum": sfillet 10
                  Case "airajstatnm": sfillet 11
                  Case "airajanemht": fillet 12
                  Case "airajrlen":   fillet 13
                  Case "airajwinds":  arrayfillet 14
                  Case "airajfdata":  arrayfillet 17
                  Case "airajcalms":  arrayfillet 18
                  Case "airarregsur": arrayfillet 19
                  Case "airartophts": arrayfillet 22
                  Case "airartopbas": fillet 21
                  Case "airartoptyp":
                    If temp.pval = "true" Then
                      check1(0).Value = True
                      check1_Click 0, -1
                    Else
                      check1(0).Value = False
                      check1_Click 0, 0
                    End If
'                  Case "airarchanl":
'                    If temp.pval = "true" Then
'                      check1(1).Value = True
'                      check1_Click 1, -1
'                    Else
'                      check1(1).Value = False
'                      check1_Click 1, 0
'                    End If
                  Case "airarclen":   fillet 24
                  Case "airarcslope": fillet 25
                  Case "airarchgt":   fillet 26
                  Case "airarcwidth": fillet 27
                  Case "airarcdir":   arcdir.ListIndex = CInt(temp.pval) - 1
                  Case "airarrname":  fillet 29
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
  Else
    PutError "Can't find or open file " & FUIName
    EndModule
  End If 'open_parm
  If Not haveglyph Then
    check1_Click 0, 0
    check1_Click 1, 0
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

Private Sub RefAdd_Click()
  RefMode = 1
  GetRef ref(RefItem)
End Sub

Private Sub RefSel_Click()
  RefMode = 0
  GetRef ref(RefItem)
End Sub

Private Sub SSTab1_Click(Index As Integer, PreviousTab As Integer)
Dim i As Long
  noact.Enabled = False
  For i = 0 To 12
    frame(i).Enabled = False
  Next
  Select Case SSTab1(0).Tab
  Case 0: frame(0).Enabled = True
          SSTab1(1).Enabled = False
          SSTab1(2).Enabled = False
  Case 1:
    SSTab1(1).Enabled = True
    Select Case SSTab1(1).Tab
      Case 0: frame(1).Enabled = True
      Case 1: frame(2).Enabled = True
      Case 2: frame(3).Enabled = True
      Case 3: frame(4).Enabled = True
      Case 4: frame(5).Enabled = True
      Case 5: frame(6).Enabled = True
      Case 6: frame(7).Enabled = True
      Case 7: frame(8).Enabled = True
    End Select
  Case 2:
    SSTab1(2).Enabled = True
    frame(9).Enabled = True
    Select Case SSTab1(2).Tab
      Case 0: frame(10).Enabled = True
      Case 1: frame(11).Enabled = True
      Case 2: frame(12).Enabled = True
    End Select
  End Select
End Sub

Private Sub Form_load()
  Dim i As Long
  Dim j As Long
  
  StartModule Atmospheric, App.Title, 5
  SetHelpFile App.Path + "\air.htm"
  SetRefFile ReplaceExt(FUIName, "ref")
  RefItem = -1
  
  'SETUP the units
  ' 21 taken out because lack of functionality
  ' needs to be put back in when becomes available in model
  unitlist = Array(0, 1, 9, 12, 13, 14, 19, 21, 22, 24, 25, 26, 27)
  For i = 0 To UBound(unitlist)
    get_conversion_items unit(unitlist(i)).Tag, unit(unitlist(i))
  Next
  'joint frequency data units
  arcdir.ListIndex = 0
  unit(17).ListIndex = 0

' disabled for this version
  SSTab1(2).TabEnabled(1) = False
  SSTab1(2).TabEnabled(2) = False
  
  jfloaded = False
  
  Loading.Show
  loadng = True
  loadprm
  loadng = False
  Unload Loading

  'SETUP the spread sheets
  tophtsheader(1) = "N"
  tophtsheader(2) = "NNE"
  tophtsheader(3) = "NE"
  tophtsheader(4) = "ENE"
  tophtsheader(5) = "E"
  tophtsheader(6) = "ESE"
  tophtsheader(7) = "SE"
  tophtsheader(8) = "SSE"
  tophtsheader(9) = "S"
  tophtsheader(10) = "SSW"
  tophtsheader(11) = "SW"
  tophtsheader(12) = "WSW"
  tophtsheader(13) = "W"
  tophtsheader(14) = "WNW"
  tophtsheader(15) = "NW"
  tophtsheader(16) = "NNW"
  classheader(0) = "A"
  classheader(1) = "B"
  classheader(2) = "C"
  classheader(3) = "D"
  classheader(4) = "E"
  classheader(5) = "F"
  classheader(6) = "G"
  
  calms.Row = 0
  For i = 1 To 7
    calms.col = i
    calms.Text = "Class" & i
  Next
  
  tophts.Row = 0
  regsur.Row = 0
  For i = 1 To 16
    tophts.col = i
    tophts.Text = tophtsheader(i)
  Next
  For i = 1 To 8
    regsur.col = i
    j = (i * 2) - 1
    regsur.Text = tophtsheader(j)
  Next
  
  For j = 0 To 6
    fdata(j).col = 0
    fdata(j).ColWidth(0) = 5
    For i = 1 To 16
      fdata(j).Row = i
      fdata(j).Text = tophtsheader(i)
    Next
    fdata(j).Row = 0
    For i = 1 To 6
      fdata(j).col = i
      fdata(j).Text = "Group " & i
    Next
  Next
  SSTab1(0).Tab = 0
  SSTab1_Click 0, 0
End Sub

Private Sub JFDImport_Click()
Dim valus
Dim fhandle As Long
Dim buffer As String
Dim info As String
Dim ord As Long
Dim fmt As String
Dim pos1 As Long
Dim pos2 As Long
Dim word As Long
Dim iclass As Long, irow As Long, icol As Long
  
  openit.CancelError = True
  openit.Filter = "Joint Frequency Data (*.jfd)|*.jfd"
  openit.DefaultExt = "jfd"
  openit.FilterIndex = 1
  On Error Resume Next
  openit.ShowOpen
  If Err.Number > 0 Or openit.filename = "" Or Dir(openit.filename) = "" Then Exit Sub
  On Error GoTo JFDError
  fhandle = FreeFile(0)
  
  Open openit.filename For Input Access Read As #fhandle
  Line Input #fhandle, buffer
  pos1 = InStr(1, buffer, "(")
  pos2 = InStr(1, buffer, ")")
  If pos1 + pos2 = 0 Then GoTo JFDError
  ord = Left(buffer, pos1 - 1)
  fmt = Mid(buffer, pos1 + 1, ((pos2 - 1) - pos1))
      
  info = Right(buffer, Len(buffer) - pos2)
  valus = Split(info, ",")
      
  pos1 = InStr(fmt, "e")
  If (pos1 <= 0) Then
    pos1 = InStr(fmt, "f")
    If (pos1 <= 0) Then
      pos1 = InStr(fmt, "g")
      If (pos1 <= 0) Then GoTo JFDError
    End If
  End If
          
  pos2 = InStr(fmt, ".")
  If (pos2 <= 0) Then GoTo JFDError
  word = Val(Mid(fmt, pos1 + 1, pos2 - 1))
  
  If UBound(valus) > 1 Then txt(3).Text = valus(1)
  If UBound(valus) > 2 Then txt(11).Text = valus(2)
  If UBound(valus) > 10 Then txt(12).Text = valus(10)
  If UBound(valus) > 11 Then txt(13).Text = valus(11)
  
  On Error GoTo JFDError
  Select Case ord
    Case "6"
      For iclass = 0 To 6
        For irow = 1 To 16
          fdata(iclass).Row = irow
          Line Input #fhandle, buffer
          For icol = 1 To 6
            fdata(iclass).col = icol
            pos1 = 1 + ((icol - 1) * word)
            fdata(iclass).Text = Trim(Mid(buffer, pos1, word))
            fdata_er iclass
          Next
        Next
      Next
    Case "16"
      For iclass = 0 To 6
        For icol = 1 To 6
          fdata(iclass).col = icol
          Line Input #fhandle, buffer
          For irow = 1 To 16
             fdata(iclass).Row = irow
             pos1 = 1 + ((irow - 1) * word)
             fdata(iclass).Text = Trim(Mid(buffer, pos1, word))
             fdata_er iclass
          Next
        Next
      Next
  End Select
    
  'calms
  calms.col = 1
  If EOF(fhandle) Then
    Close #fhandle
    MsgBox "Data successfully loaded from " & openit.filename, 64, "Joint Frequency Summary Data"
    Exit Sub
  End If
  Line Input #fhandle, buffer
  For irow = 1 To 7
    pos1 = 1 + ((irow - 1) * word)
    If irow = 7 And ord = 6 Then
      Line Input #fhandle, buffer
      pos1 = 1
    End If
    calms.Row = irow
    calms.Text = Trim(Mid(buffer, pos1, word))
    calms_er
  Next
  
  'winds
  winds.col = 1
  If EOF(fhandle) Then
    Close #fhandle
    MsgBox "Data successfully loaded from " & openit.filename, 64, "Joint Frequency Summary Data"
    Exit Sub
  End If
  Line Input #fhandle, buffer
  For irow = 1 To 6
    pos1 = 1 + ((irow - 1) * word)
    winds.Row = irow
    winds.Text = Trim(Mid(buffer, pos1, word))
    winds_er
  Next
  
  Close #fhandle
  MsgBox "Data successfully loaded from " & openit.filename, 64, "Joint Frequency Summary Data"
  Exit Sub
  
JFDError:
  MsgBox "Incorrect format: " & fmt, 48, "Joint Frequency File Error"
End Sub

Private Sub save_Click()
  Dim i As Long
  Dim j As Long
  Dim k As Long
  Dim fname As String
  Dim parm As parmrec
  Dim fle As parmfile
  Dim iclass As Long
  Dim tot As Double
  Dim UserFormat As String
  UserFormat = CVTFormat
  
  fname = RunName & ".GID"
  If open_parm(fle, fname, 1) Then
    set_parm parm, "CVTFormat", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CVTFormat
    write_parmrec fle, parm
    CVTFormat = "General Number"
    set_parm parm, "CARELTYP", siteIdx, modIdx, 0, 0, 0, 0, 0, "N/A", "N/A", 7
    write_parmrec fle, parm
    For i = 0 To 23
      Select Case i
        Case 3, 4, 10, 11:
          set_parm parm, "AIR" & txt(i).Tag, siteIdx, modIdx, 0, 0, 0, 0, ref(i).Tag, "N/A", "N/A", txt(i).Text
          write_parmrec fle, parm
          If er(i) Then PutError "Error in " & lbl(i)
        Case 0, 1, 9, 12, 13
          set_parm parm, "AIR" & txt(i).Tag, siteIdx, modIdx, 0, 0, 0, 0, ref(i).Tag, unit(i).Text, unit(i).Tag, convert(unit(i).Text, unit(i).Tag, Val(txt(i).Text))
          write_parmrec fle, parm
          If er(i) Then PutError "Error in " & lbl(i)
        Case 21
          set_parm parm, "AIR" & txt(i).Tag, siteIdx, modIdx, 0, 0, 0, 0, ref(i).Tag, unit(i).Text, unit(i).Tag, convert(unit(i).Text, unit(i).Tag, Val(txt(i).Text))
          write_parmrec fle, parm
'          If er(i) Then PutError "Error in " & lbl(i)
        Case 14: 'winds array
          If er(i) Then PutError "Error in " & lbl(i)
          winds.col = 1
          For j = 1 To 6
            winds.Row = j
            set_parm parm, "AIRajwinds", siteIdx, modIdx, j, 1, 0, 0, ref(i).Tag, unit(i).Text, unit(i).Tag, convert(unit(i).Text, unit(i).Tag, Val(winds.Text))
            write_parmrec fle, parm
          Next
        Case 18: 'calms array
          If er(i) Then PutError "Error in " & lbl(i)
          calms.col = 1
          tot = 0
          For j = 1 To 7
            calms.Row = j
            set_parm parm, "AIRajcalms", siteIdx, modIdx, j, 1, 0, 0, ref(i).Tag, unit(17).Text, unit(17).Text, calms.Text
            write_parmrec fle, parm
            tot = tot + Val(calms.Text)
          Next
        Case 19: 'regsur array
          If er(i) Then PutError "Error in " & lbl(i)
          For j = 1 To 4 'radius
            regsur.Row = j
            For k = 1 To 8 'direction
              regsur.col = k
              set_parm parm, "AIRarregsur", siteIdx, modIdx, j, k, 0, 0, ref(i).Tag, unit(i).Text, unit(i).Tag, convert(unit(i).Text, unit(i).Tag, Val(regsur.Text))
              write_parmrec fle, parm
            Next
          Next
        Case 22: 'tophts array
          set_parm parm, "AIRartoptyp", siteIdx, modIdx, 0, 0, 0, 0, 0, "N/A", "N/A", check1(0).Value
          write_parmrec fle, parm
          If check1(0).Value Then
            If er(i) Then PutError "Error in " & lbl(i)
            For j = 1 To 4 'radius
              tophts.Row = j
              For k = 1 To 16 'direction
                 tophts.col = k
                 set_parm parm, "AIRartophts", siteIdx, modIdx, j, k, 0, 0, ref(i).Tag, unit(i).Text, unit(i).Tag, convert(unit(i).Text, unit(i).Tag, Val(tophts.Text))
                 write_parmrec fle, parm
              Next
            Next
          End If
        Case 23:
          set_parm parm, "AIRarchanl", siteIdx, modIdx, 0, 0, 0, 0, 0, "N/A", "N/A", check1(1).Value
          write_parmrec fle, parm
          If check1(1).Value Then
            For j = 24 To 27
              If er(j) Then PutError "Error in " & lbl(j)
              set_parm parm, "AIR" & txt(j).Tag, siteIdx, modIdx, 0, 0, 0, 0, ref(j).Tag, unit(j).Text, unit(j).Tag, convert(unit(j).Text, unit(j).Tag, txt(j))
              write_parmrec fle, parm
            Next
            set_parm parm, "AIRarcdir", siteIdx, modIdx, 0, 0, 0, 0, ref(28).Tag, "N/A", "N/A", Str(arcdir.ListIndex + 1)
            write_parmrec fle, parm
          End If
      End Select
    Next
    'fdata array
    For i = 0 To 6 'class A to class G
      For k = 1 To 6 'group1 - group6
         fdata(i).col = k
         For j = 1 To 16 'direction N to NNW
           fdata(i).Row = j
           tot = Val(fdata(i).Text) + tot
           set_parm parm, "AIRajfdata", siteIdx, modIdx, i + 1, j, k, 0, 0, unit(17).Text, unit(17).Text, fdata(i).Text
           write_parmrec fle, parm
         Next
       Next
    Next
    If tot <= 0 Then PutError "Sum of joint frequency must be greater than zero!"
    close_parm fle
    CVTFormat = UserFormat
    SetFormat Me
  Else
    PutError "Unable to create transaction file" & RunName & ".GID"
  End If
  EndModule
End Sub
'
'Private Sub meta_click()
'  If opendes(App.Path + "\mepair.des") Then
'
'    put_val des, "mf"
'    put_val des, "version 2.0 beta"
'    put_line des
'    put_val des, "Air"
'    put_val des, "Mepas 4.0 Air Module"
'    put_val des, "air.exe"
'    put_val des, "air.bat"
'    put_line des
'
'    des.putbuff = """MEPAS 4.0 Atmosheric Module" + Chr(13) + Chr(10) + Chr(13) + Chr(10) + _
'        "This air module provides estimates of potential" + Chr(13) + Chr(10) + _
'        "ambient air concentration and deposition rates.  The" + Chr(13) + Chr(10) + _
'        "atmospheric and surface deposition concentrations are" + Chr(13) + Chr(10) + _
'        "computed using standard Gaussian dispersion models that" + Chr(13) + Chr(10) + _
'        "use local climatological site data.  These computations" + Chr(13) + Chr(10) + _
'        "account for influences such as the regional patterns of" + Chr(13) + Chr(10) + _
'        "surface roughness and terrain heights.  A complex-terrain" + Chr(13) + Chr(10)
'    des.putbuff = des.putbuff + _
'        "option accounts for the influence of local nocturnal wind" + Chr(13) + Chr(10) + _
'        "channeling near the source.  Typically, the air module" + Chr(13) + Chr(10) + _
'        "calculates long-term (i.e. annual or longer ) regional" + Chr(13) + Chr(10) + _
'        "air concentrations and deposition rates." + Chr(13) + Chr(10) + Chr(13) + Chr(10)
'
'    des.putbuff = des.putbuff + _
'         "Limitations:  None known at this time." + Chr(13) + Chr(10) + Chr(13) + Chr(10)
'
'   des.putbuff = des.putbuff + _
'         "Reference:" + Chr(13) + Chr(10) + _
'         "    A more complete description of the assumptions" + Chr(13) + Chr(10) + _
'         "    and theortical foundations can be found in" + Chr(13) + Chr(10) + _
'         "    Multimedia Environmental Pollutant Assessment System(MEPAS):" + Chr(13) + Chr(10) + _
'         "    Atmospheric Pathway Formulations (Droppo and Buck 1996)." + Chr(13) + Chr(10) + Chr(13) + Chr(10)
'
'    des.putbuff = des.putbuff + _
'         "Point of Contact:" + Chr(13) + Chr(10) + _
'         "    Pacific Northwest National Laboratory" + Chr(13) + Chr(10) + _
'         "    Christian Fosmire" + Chr(13) + Chr(10) + _
'         "    P.O. Box 999 MS K9-30" + Chr(13) + Chr(10) + _
'         "    Richland WA 99352" + Chr(13) + Chr(10) + _
'         "    Phone (509)-372-6314" + Chr(13) + Chr(10) + _
'         "    EMail cj_fosmire@pnl.gov"""
'    put_line des
'
'    put_val des, 2
'    put_line des
'
'    putfile AFF, 1
'    putfile ATO, 2
'
''count entries below and place that number here
'    put_val des, 22
'    put_line des
'
'    putmeta "AIR" + UCase(txt(3).Tag), "NOT STOCHASTIC", "N/A", lbl(3).Caption, 0
'    putmeta "AIR" + UCase(txt(0).Tag), "CONTINUOUS", unit(0).Tag, lbl(0).Caption, 0, 300, 900
'    putmeta "AIR" + UCase(txt(1).Tag), "CONTINUOUS", unit(1).Tag, lbl(1).Caption, 0, 800, 2600
'    putmeta "AIR" + UCase(txt(9).Tag), "CONTINUOUS", unit(9).Tag, lbl(9).Caption, 0, 0.1, 130
'    putmeta "AIR" + UCase(txt(10).Tag), "CONTINUOUS", "N/A", lbl(10).Caption, 0, 0, 365
'    putmeta "AIR" + UCase(txt(4).Tag), "CONTINUOUS", "N/A", lbl(4).Caption, 0, 1, 150
'
'    putmeta "AIR" + UCase(txt(11).Tag), "NOT STOCHASTIC", "N/A", lbl(11).Caption, 0
'    putmeta "AIR" + UCase(txt(12).Tag), "CONTINUOUS", unit(12).Tag, lbl(12).Caption, 0, 0, 999.9
'    putmeta "AIR" + UCase(txt(13).Tag), "CONTINUOUS", unit(13).Tag, lbl(13).Caption, 0, 0, 999.9
'    putmeta "AIRAJWINDS", "CONTINUOUS", unit(14).Tag, lbl(14).Caption, 3, 0, 99.9
'    putvariable "SiteName", "Index1"
'    putvariable "Glyph", "Index2"
'    putlabel "Point #", "Index1"
'    putmeta "AIRAJCALMS", "CONTINUOUS", unit(17).Tag, lbl(18).Caption, 3, 0
'    putvariable "SiteName", "Index1"
'    putvariable "Glyph", "Index2"
'    putlabel "Calm #", "Index1"
'    putmeta "AIRAJFDATA", "NOT STOCHASTIC", "N/A", "Joint frequency data point", 5, 0, 1
'    putvariable "SiteName", "Index1"
'    putvariable "Glyph", "Index2"
'    putlabel "Class #", "Index3"
'    putlabel "Row #", "Index4"
'    putlabel "Col #", "Index5"
'
'    putmeta "AIR" + UCase(check1(0).Tag), "NOT STOCHASTIC", "N/A", check1(0).Caption, 0
'    putmeta "AIR" + UCase(check1(1).Tag), "NOT STOCHASTIC", "N/A", check1(1).Caption, 0
'    putmeta "AIR" + UCase(txt(21).Tag), "CONTINUOUS", unit(21).Tag, lbl(21).Caption, 0, -50, 8840
'
'    putmeta "AIRARTOPHTS", "CONTINUOUS", unit(22).Tag, lbl(22).Caption, 4, -50, 8840
'    putvariable "SiteName", "Index1"
'    putvariable "Glyph", "Index2"
'    putlabel "Row #", "Index4"
'    putlabel "Col #", "Index5"
'
'    putmeta "AIRARREGSUR", "CONTINUOUS", unit(19).Tag, lbl(19).Caption, 4, 0, 999.9
'    putvariable "SiteName", "Index1"
'    putvariable "Glyph", "Index2"
'    putlabel "Row #", "Index4"
'    putlabel "Col #", "Index5"
'
'    putmeta "AIR" + UCase(txt(24).Tag), "CONTINUOUS", unit(24).Tag, lbl(24).Caption, 0, 1, 30
'    putmeta "AIR" + UCase(txt(25).Tag), "CONTINUOUS", unit(25).Tag, lbl(25).Caption, 0, -999.9, -0.001
'    putmeta "AIR" + UCase(txt(26).Tag), "CONTINUOUS", unit(26).Tag, lbl(26).Caption, 0, 0.001, 1
'    putmeta "AIR" + UCase(txt(27).Tag), "CONTINUOUS", unit(27).Tag, lbl(27).Caption, 0, 0.001, 1
'    putmeta "AIR" + UCase(arcdir.Tag), "NOT STOCHASTIC", "N/A", lbl(28).Caption, 0
'
'    closedes
'  Else
'    MsgBox "Unable to create description file"
'  End If
'
'End Sub

Private Function er(Index As Long) As Boolean
  Dim sum As Long
  Dim m As String
  Dim t1 As String
  Dim t2 As String
  Dim irow As Long
  Dim icol As Long
  Dim low As Double
  Dim high As Double
  Dim var As Double
  
  mes = ""
  er = False
  Select Case Index
    Case 14
      winds.col = 1
      For irow = 1 To 6
        winds.Row = irow
        If winds_er Then er = True
      Next
      Exit Function
    Case 18
      calms.col = 1
      For irow = 1 To 7
        calms.Row = irow
        If calms_er Then er = True
      Next
      Exit Function
    Case 19
      For icol = 1 To 8
        regsur.col = icol
        For irow = 1 To 4
          regsur.Row = irow
          If regsur_er Then er = True
        Next
      Next
      regsur.Refresh
      Exit Function
    Case 22
      For icol = 1 To 16
        tophts.col = icol
        For irow = 1 To 4
          tophts.Row = irow
          If tophts_er Then er = True
        Next
      Next
      Exit Function
    Case 3, 11, 17, -1:
    Case Else
      Select Case Index
        Case 0:         low = 50:          high = 900
        Case 1:         low = 50:          high = 2600
        Case 4:         low = 1:            high = 150
        Case 9:         low = 0.1:          high = 130
        Case 10:        low = 0:            high = 365
        Case 12:        low = 0:            high = 999.9
        Case 13:        low = 0:            high = 999.9
        Case 21:        low = -50:          high = 8840
        Case 24:        low = 1:            high = 30
        Case 25:        low = -999.9:       high = -0.001
        Case 26:        low = 0.001:        high = 1
        Case 27:        low = 0.001:        high = 1
      End Select
    tval = Val(txt(Index).Text)
    If Index = 4 Or Index = 10 Then
      m = "Value must be between " + Str(low) + " and " + Str(high)
      If (tval < low Or tval > high Or txt(Index) = "") Then er = True
    Else
      low = convert(unit(Index).Tag, unit(Index).Text, low)
      high = convert(unit(Index).Tag, unit(Index).Text, high)
      m = "Value must be between " + Str(low) + " and " + Str(high) + " " + unit(Index).Text
      If (tval < low Or tval > high Or txt(Index) = "") Then er = True
    End If
    mes = Space(140 - Len(m)) & m
    If er Then
      txt(Index).BackColor = &H8080FF
    Else
      txt(Index).BackColor = &HC0FFC0
    End If
  End Select
End Function

Private Sub Timer1_Timer()
  mes = Mid(mes, 2) & Mid(mes, 1, 1)
End Sub

Private Sub txt_Change(Index As Integer)
Dim chk As Double
On Error GoTo toolarge
  If Index = 11 Or Index = 3 Then Exit Sub
  er CLng(Index)
  chk = CDbl(txt(Index).Text)
  Exit Sub
toolarge:
  txt(Index).BackColor = &H8080FF
End Sub

Private Sub unit_Click(Index As Integer)
  er CLng(Index)
End Sub

Private Sub check1_Click(Index As Integer, Value As Integer)
 SSTab1(2).TabEnabled(Index + 1) = Value
 SSTab1(2).Tab = 0
 If Index = 0 Then
   lbl(21).Enabled = Value
   txt(21).Enabled = Value
   unit(21).Enabled = Value
   ref(21).Enabled = Value
 End If
End Sub

Private Sub txt_GotFocus(Index As Integer)
  er CLng(Index)
  RefItem = Index
  noact.Enabled = True
  HelpAnchor = lbl(Index).Tag
End Sub

Private Sub SSTab1_GotFocus(Index As Integer)
  mes = ""
  RefItem = -1
  noact.Enabled = False
  HelpAnchor = ""
End Sub

Private Sub unit_GotFocus(Index As Integer)
  mes = ""
  RefItem = Index
  noact.Enabled = True
  HelpAnchor = lbl(Index).Tag
End Sub

Private Sub check1_GotFocus(Index As Integer)
  mes = ""
  RefItem = -1
  noact.Enabled = False
  HelpAnchor = check1(Index).Tag
End Sub

Private Sub tophts_GotFocus()
Dim m As String
  m = "Value must be greater than " + convert(unit(22).Tag, unit(22).Text, -50) + " and less than " + convert(unit(22).Tag, unit(22).Text, 8840) + " " + unit(22).Text
  mes = Space(140 - Len(m)) & m
  RefItem = 22
  noact.Enabled = True
  HelpAnchor = tophts.Tag
End Sub

Private Sub calms_GotFocus()
Dim m As String
  m = "Value must be greater than or equal to 0"
  mes = Space(140 - Len(m)) & m
  RefItem = 18
  noact.Enabled = True
  HelpAnchor = calms.Tag
End Sub

Private Sub winds_GotFocus()
Dim m As String
  m = "Value must be greater than 0 and less than " + convert(unit(14).Tag, unit(14).Text, 99.99) + " " + unit(14).Text
  mes = Space(140 - Len(m)) & m
  RefItem = 14
  noact.Enabled = True
  HelpAnchor = winds.Tag
End Sub

Private Sub regsur_GotFocus()
Dim m As String
  m = "Value must be greater than 0 and less than " + convert(unit(19).Tag, unit(19).Text, 999.9) + " " + unit(19).Text
  mes = Space(140 - Len(m)) & m
  RefItem = 19
  noact.Enabled = True
  HelpAnchor = regsur.Tag
End Sub

Private Sub fdata_GotFocus(Index As Integer)
Dim m As String
  m = "Note:  All directions are from which the wind blows."
  mes = Space(140 - Len(m)) & m
  RefItem = 18
  noact.Enabled = True
  HelpAnchor = "ajjfname"
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

Private Function winds_er() As Boolean
  On Error GoTo windserr
  tval = CDbl(convert(unit(14).Text, unit(14).Tag, CDbl(winds.Text)))
  If tval >= 0 And tval < 100 Then
    winds.BackColor = &HC0FFC0
    winds_er = False
    Exit Function
  End If
windserr:
  winds.BackColor = &H8080FF
  winds_er = True
End Function

Private Function calms_er() As Boolean
  On Error GoTo calmserr
  If CDbl(calms.Text) >= 0 Then
    calms.BackColor = &HC0FFC0
    calms_er = False
    Exit Function
  End If
calmserr:
  calms.BackColor = &H8080FF
  calms_er = True
End Function

Private Function tophts_er() As Boolean
  On Error GoTo tophtserr
  tval = CDbl(convert(unit(22).Text, unit(22).Tag, CDbl(tophts.Text)))
  If tval >= -50 And tval <= 8840 Then
    tophts.BackColor = &HC0FFC0
    tophts_er = False
    Exit Function
  End If
tophtserr:
  tophts.BackColor = &H8080FF
  tophts_er = True
End Function

Private Function regsur_er() As Boolean
  On Error GoTo regsurerr
  tval = CDbl(convert(unit(19).Text, unit(19).Tag, CDbl(regsur.Text)))
  If tval >= 0 And tval < 1000 Then
    regsur.BackColor = &HC0FFC0
    regsur_er = False
    Exit Function
  End If
regsurerr:
  regsur.BackColor = &H8080FF
  regsur_er = True
End Function

Private Function fdata_er(Index As Long) As Boolean
  On Error GoTo fdataerr
  If CDbl(fdata(Index).Text) >= 0 Then
    fdata(Index).BackColor = &HC0FFC0
    fdata_er = False
    Exit Function
  End If
fdataerr:
  fdata(Index).BackColor = &H8080FF
  fdata_er = True
End Function

Private Sub winds_KeyUp(KeyCode As Integer, Shift As Integer)
  winds.col = winds.ActiveCol
  winds.Row = winds.ActiveRow
  winds_er
End Sub

Private Sub calms_KeyUp(KeyCode As Integer, Shift As Integer)
  calms.col = calms.ActiveCol
  calms.Row = calms.ActiveRow
  calms_er
End Sub

Private Sub tophts_KeyUp(KeyCode As Integer, Shift As Integer)
  tophts.col = tophts.ActiveCol
  tophts.Row = tophts.ActiveRow
  tophts_er
End Sub

Private Sub regsur_KeyUp(KeyCode As Integer, Shift As Integer)
  regsur.col = regsur.ActiveCol
  regsur.Row = regsur.ActiveRow
  regsur_er
End Sub

Private Sub fdata_KeyUp(Index As Integer, KeyCode As Integer, Shift As Integer)
  fdata(Index).col = fdata(Index).ActiveCol
  fdata(Index).Row = fdata(Index).ActiveRow
  fdata_er CLng(Index)
End Sub
