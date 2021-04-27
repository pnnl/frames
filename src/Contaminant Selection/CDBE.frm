VERSION 5.00
Object = "{F856EC8B-F03C-4515-BDC6-64CBD617566A}#7.0#0"; "FPSPR70.ocx"
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "THREED32.OCX"
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "ComDlg32.OCX"
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "TabCtl32.Ocx"
Object = "{6B7E6392-850A-101B-AFC0-4210102A8DA7}#1.4#0"; "comctl32.Ocx"
Begin VB.Form frmCDBE 
   Appearance      =   0  'Flat
   BorderStyle     =   1  'Fixed Single
   Caption         =   "FRAMES Constituent Database Editor"
   ClientHeight    =   8730
   ClientLeft      =   1725
   ClientTop       =   1800
   ClientWidth     =   9825
   ControlBox      =   0   'False
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   9.75
      Charset         =   0
      Weight          =   700
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   ForeColor       =   &H80000008&
   Icon            =   "CDBE.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form5"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   582
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   655
   StartUpPosition =   2  'CenterScreen
   Begin TabDlg.SSTab SSTab1 
      Height          =   8295
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   9810
      _ExtentX        =   17304
      _ExtentY        =   14631
      _Version        =   393216
      Tabs            =   2
      TabsPerRow      =   2
      TabHeight       =   794
      TabCaption(0)   =   "Select Constituents of Concern"
      TabPicture(0)   =   "CDBE.frx":030A
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "Frame3(0)"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).ControlCount=   1
      TabCaption(1)   =   "Edit Constituent Properties"
      TabPicture(1)   =   "CDBE.frx":0326
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "lstCon"
      Tab(1).Control(1)=   "cboView"
      Tab(1).Control(1).Enabled=   0   'False
      Tab(1).Control(2)=   "chkShowCasid"
      Tab(1).Control(2).Enabled=   0   'False
      Tab(1).Control(3)=   "SSTab2"
      Tab(1).ControlCount=   4
      Begin VB.ListBox lstCon 
         DataField       =   "Name"
         DataSource      =   "Data1"
         BeginProperty Font 
            Name            =   "Tahoma"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   1500
         Left            =   -74760
         Sorted          =   -1  'True
         TabIndex        =   12
         Tag             =   "name"
         Top             =   1200
         Width           =   7488
      End
      Begin VB.ComboBox cboView 
         BeginProperty Font 
            Name            =   "Tahoma"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   360
         ItemData        =   "CDBE.frx":0342
         Left            =   -74760
         List            =   "CDBE.frx":0344
         Style           =   2  'Dropdown List
         TabIndex        =   43
         TabStop         =   0   'False
         Top             =   720
         Width           =   7440
      End
      Begin VB.CheckBox chkShowCasid 
         Caption         =   "Show &CASID"
         BeginProperty Font 
            Name            =   "Tahoma"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   -67080
         TabIndex        =   13
         TabStop         =   0   'False
         Top             =   720
         Width           =   1485
      End
      Begin TabDlg.SSTab SSTab2 
         Height          =   5052
         Left            =   -75000
         TabIndex        =   14
         Top             =   3240
         Width           =   9780
         _ExtentX        =   17251
         _ExtentY        =   8916
         _Version        =   393216
         Tabs            =   4
         TabsPerRow      =   4
         TabHeight       =   794
         TabCaption(0)   =   "Identification"
         TabPicture(0)   =   "CDBE.frx":0346
         Tab(0).ControlEnabled=   -1  'True
         Tab(0).Control(0)=   "fraTabStrip(0)"
         Tab(0).Control(0).Enabled=   0   'False
         Tab(0).Control(1)=   "cmdCopy"
         Tab(0).Control(1).Enabled=   0   'False
         Tab(0).Control(2)=   "cmdAdd"
         Tab(0).Control(2).Enabled=   0   'False
         Tab(0).Control(3)=   "cmdDelete"
         Tab(0).Control(3).Enabled=   0   'False
         Tab(0).ControlCount=   4
         TabCaption(1)   =   "Properties"
         TabPicture(1)   =   "CDBE.frx":0362
         Tab(1).ControlEnabled=   0   'False
         Tab(1).Control(0)=   "fraTabStrip(1)"
         Tab(1).ControlCount=   1
         TabCaption(2)   =   "Degradation Products"
         TabPicture(2)   =   "CDBE.frx":037E
         Tab(2).ControlEnabled=   0   'False
         Tab(2).Control(0)=   "fraTabStrip(2)"
         Tab(2).ControlCount=   1
         TabCaption(3)   =   "Degradation Chain"
         TabPicture(3)   =   "CDBE.frx":039A
         Tab(3).ControlEnabled=   0   'False
         Tab(3).Control(0)=   "fraTabStrip(3)"
         Tab(3).ControlCount=   1
         Begin VB.CommandButton cmdDelete 
            Caption         =   "&Delete"
            BeginProperty Font 
               Name            =   "Tahoma"
               Size            =   9.75
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   405
            Left            =   8160
            TabIndex        =   66
            Top             =   1812
            Width           =   1365
         End
         Begin VB.CommandButton cmdAdd 
            Caption         =   "&New"
            BeginProperty Font 
               Name            =   "Tahoma"
               Size            =   9.75
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   405
            Left            =   8160
            TabIndex        =   65
            Top             =   720
            Width           =   1365
         End
         Begin VB.CommandButton cmdCopy 
            Caption         =   "&Copy"
            BeginProperty Font 
               Name            =   "Tahoma"
               Size            =   9.75
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   405
            Left            =   8160
            TabIndex        =   64
            Top             =   1260
            Width           =   1365
         End
         Begin VB.Frame fraTabStrip 
            ClipControls    =   0   'False
            Height          =   4380
            Index           =   3
            Left            =   -74860
            TabIndex        =   51
            Top             =   480
            Width           =   9468
            Begin ComctlLib.TreeView tvwDKview 
               Height          =   3612
               Index           =   1
               Left            =   4800
               TabIndex        =   29
               Top             =   600
               Width           =   4380
               _ExtentX        =   7726
               _ExtentY        =   6376
               _Version        =   327682
               Indentation     =   353
               LabelEdit       =   1
               LineStyle       =   1
               Style           =   2
               BorderStyle     =   1
               Appearance      =   1
               BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
                  Name            =   "Tahoma"
                  Size            =   9.75
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
            End
            Begin ComctlLib.TreeView tvwDKview 
               Height          =   3612
               Index           =   0
               Left            =   240
               TabIndex        =   30
               Top             =   600
               Width           =   4416
               _ExtentX        =   7779
               _ExtentY        =   6376
               _Version        =   327682
               Indentation     =   353
               LabelEdit       =   1
               LineStyle       =   1
               Style           =   2
               BorderStyle     =   1
               Appearance      =   1
               BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
                  Name            =   "Tahoma"
                  Size            =   9.75
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
            End
            Begin VB.Label Label9 
               AutoSize        =   -1  'True
               Caption         =   "Straight Degradation"
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   9.75
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   240
               Left            =   240
               TabIndex        =   53
               Top             =   312
               Width           =   1836
            End
            Begin VB.Label Label7 
               AutoSize        =   -1  'True
               Caption         =   "Branch Degradation"
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   9.75
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   240
               Left            =   4800
               TabIndex        =   52
               Top             =   312
               Width           =   1788
            End
         End
         Begin VB.Frame fraTabStrip 
            ClipControls    =   0   'False
            BeginProperty Font 
               Name            =   "Tahoma"
               Size            =   9.75
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            ForeColor       =   &H00FF0000&
            Height          =   4380
            Index           =   2
            Left            =   -74860
            TabIndex        =   46
            Top             =   480
            Width           =   9468
            Begin VB.ComboBox cboBranch 
               Enabled         =   0   'False
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   9.75
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   336
               Index           =   0
               ItemData        =   "CDBE.frx":03B6
               Left            =   5100
               List            =   "CDBE.frx":03C0
               Style           =   2  'Dropdown List
               TabIndex        =   24
               Top             =   1032
               Width           =   3630
            End
            Begin VB.TextBox txtBranch 
               Enabled         =   0   'False
               Height          =   360
               Index           =   0
               Left            =   7560
               TabIndex        =   25
               Text            =   "0"
               Top             =   1476
               Width           =   1140
            End
            Begin VB.TextBox txtBranch 
               Enabled         =   0   'False
               Height          =   360
               Index           =   1
               Left            =   7548
               TabIndex        =   28
               Text            =   "0"
               Top             =   2736
               Width           =   1140
            End
            Begin VB.CommandButton cmdBranch 
               Caption         =   ">>>"
               Enabled         =   0   'False
               Height          =   360
               Index           =   0
               Left            =   4080
               TabIndex        =   23
               Top             =   1032
               Width           =   660
            End
            Begin VB.CommandButton cmdBranch 
               Caption         =   ">>>"
               Enabled         =   0   'False
               Height          =   360
               Index           =   1
               Left            =   4080
               TabIndex        =   26
               Top             =   2340
               Width           =   660
            End
            Begin VB.ComboBox cboBranch 
               Enabled         =   0   'False
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   9.75
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   336
               Index           =   1
               ItemData        =   "CDBE.frx":03D5
               Left            =   5100
               List            =   "CDBE.frx":03DF
               Style           =   2  'Dropdown List
               TabIndex        =   27
               Top             =   2340
               Width           =   3630
            End
            Begin VB.ListBox lstDeg 
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   9.75
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   3420
               Left            =   216
               TabIndex        =   22
               Top             =   600
               Width           =   3636
            End
            Begin VB.Label Label16 
               Caption         =   "Selected constituents of concern are unavailable for degradation product editing"
               ForeColor       =   &H000000C0&
               Height          =   492
               Left            =   4200
               TabIndex        =   63
               Top             =   3480
               Width           =   4452
            End
            Begin VB.Label Label5 
               AutoSize        =   -1  'True
               Caption         =   "Available constituents"
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   9.75
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   228
               Left            =   240
               TabIndex        =   61
               Top             =   312
               Width           =   1836
            End
            Begin VB.Label Label14 
               AutoSize        =   -1  'True
               Caption         =   "Fraction:"
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   9.75
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   240
               Left            =   6516
               TabIndex        =   50
               Top             =   1548
               Width           =   792
            End
            Begin VB.Label Label15 
               AutoSize        =   -1  'True
               Caption         =   "Fraction:"
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   9.75
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   240
               Left            =   6492
               TabIndex        =   49
               Top             =   2808
               Width           =   792
            End
            Begin VB.Label Label13 
               AutoSize        =   -1  'True
               Caption         =   "Primary Branch"
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   9.75
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   240
               Left            =   5040
               TabIndex        =   48
               Top             =   720
               Width           =   1368
            End
            Begin VB.Label Label19 
               AutoSize        =   -1  'True
               Caption         =   "Secondary Branch"
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   9.75
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   240
               Left            =   5040
               TabIndex        =   47
               Top             =   2040
               Width           =   1668
            End
         End
         Begin VB.Frame fraTabStrip 
            ClipControls    =   0   'False
            BeginProperty Font 
               Name            =   "Tahoma"
               Size            =   9.75
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   4380
            Index           =   1
            Left            =   -74860
            TabIndex        =   44
            Top             =   480
            Width           =   9468
            Begin VB.ComboBox cboPropCat 
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   9.75
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   336
               ItemData        =   "CDBE.frx":03F4
               Left            =   2100
               List            =   "CDBE.frx":03F6
               Style           =   2  'Dropdown List
               TabIndex        =   19
               Top             =   240
               Width           =   4776
            End
            Begin VB.CommandButton cmdPropEst 
               Caption         =   "&Estimate..."
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   9.75
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   345
               Left            =   7200
               TabIndex        =   20
               Top             =   240
               Width           =   1590
            End
            Begin FPSpreadADO.fpSpread vaSpread1 
               Height          =   3432
               Left            =   240
               TabIndex        =   21
               Top             =   720
               Width           =   4632
               _Version        =   458752
               _ExtentX        =   8202
               _ExtentY        =   6376
               _StockProps     =   64
               Enabled         =   0   'False
               AllowCellOverflow=   -1  'True
               AutoSize        =   -1  'True
               BackColorStyle  =   1
               ColHeaderDisplay=   0
               EditModeReplace =   -1  'True
               BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
                  Name            =   "MS Sans Serif"
                  Size            =   8.25
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               GridSolid       =   0   'False
               RowHeaderDisplay=   0
               ScrollBars      =   2
               ScrollBarShowMax=   0   'False
               SelectBlockOptions=   0
               ShadowColor     =   12640511
               ShadowText      =   -2147483640
               SpreadDesigner  =   "CDBE.frx":03F8
               UserResize      =   0
               VisibleCols     =   4
               VisibleRows     =   14
            End
            Begin VB.Label Label8 
               AutoSize        =   -1  'True
               Caption         =   "Category (jump to)"
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   9.75
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   240
               Left            =   240
               TabIndex        =   45
               Top             =   312
               Width           =   1632
            End
         End
         Begin VB.Frame fraTabStrip 
            ClipControls    =   0   'False
            Height          =   4380
            Index           =   0
            Left            =   140
            TabIndex        =   38
            Top             =   480
            Width           =   7668
            Begin VB.CheckBox chkRad 
               Caption         =   "Radionuclide"
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   10.5
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   252
               Left            =   5160
               TabIndex        =   60
               Top             =   360
               Width           =   2292
            End
            Begin VB.ComboBox Combo3 
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   9.75
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   324
               ItemData        =   "CDBE.frx":064F
               Left            =   2280
               List            =   "CDBE.frx":0651
               Style           =   2  'Dropdown List
               TabIndex        =   58
               Top             =   3840
               Width           =   5250
            End
            Begin VB.ComboBox Combo2 
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   9.75
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   324
               ItemData        =   "CDBE.frx":0653
               Left            =   2280
               List            =   "CDBE.frx":0655
               Style           =   2  'Dropdown List
               TabIndex        =   56
               Top             =   3360
               Width           =   5250
            End
            Begin VB.ComboBox Combo1 
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   9.75
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   324
               ItemData        =   "CDBE.frx":0657
               Left            =   2280
               List            =   "CDBE.frx":0659
               Style           =   2  'Dropdown List
               TabIndex        =   54
               Top             =   2880
               Width           =   5250
            End
            Begin VB.TextBox txtSyns 
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   9.75
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   1080
               Left            =   2280
               MultiLine       =   -1  'True
               ScrollBars      =   2  'Vertical
               TabIndex        =   17
               Top             =   1200
               Width           =   5250
            End
            Begin VB.ComboBox Combo0 
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   9.75
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   324
               ItemData        =   "CDBE.frx":065B
               Left            =   2280
               List            =   "CDBE.frx":065D
               Style           =   2  'Dropdown List
               TabIndex        =   18
               Top             =   2400
               Width           =   5250
            End
            Begin VB.TextBox txtName 
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   9.75
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   360
               Left            =   2280
               TabIndex        =   16
               Top             =   720
               Width           =   5250
            End
            Begin VB.TextBox txtCasid 
               Enabled         =   0   'False
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   9.75
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   360
               Left            =   2280
               TabIndex        =   15
               Top             =   240
               Width           =   2652
            End
            Begin VB.Label Label12 
               Caption         =   "RAAS type"
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   10.5
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   252
               Left            =   240
               TabIndex        =   59
               Top             =   3840
               Width           =   2000
            End
            Begin VB.Label Label11 
               Caption         =   "Exposure type"
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   10.5
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   252
               Left            =   240
               TabIndex        =   57
               Top             =   3360
               Width           =   2000
            End
            Begin VB.Label Label10 
               Caption         =   "Periodic Group"
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   10.5
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   252
               Left            =   240
               TabIndex        =   55
               Top             =   2880
               Width           =   2000
            End
            Begin VB.Label Label6 
               Caption         =   "Chemical type"
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   10.5
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   252
               Left            =   240
               TabIndex        =   42
               Top             =   2400
               Width           =   2000
            End
            Begin VB.Label Label4 
               Caption         =   "Synonyms"
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   10.5
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   252
               Left            =   228
               TabIndex        =   41
               Top             =   1560
               Width           =   2004
            End
            Begin VB.Label Label3 
               Caption         =   "CAS ID"
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   10.5
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   252
               Left            =   228
               TabIndex        =   40
               Top             =   312
               Width           =   2000
            End
            Begin VB.Label Label2 
               Caption         =   "Common Name"
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   10.5
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   252
               Left            =   228
               TabIndex        =   39
               Top             =   720
               Width           =   2004
            End
         End
      End
      Begin VB.Frame Frame3 
         BorderStyle     =   0  'None
         ClipControls    =   0   'False
         Height          =   7695
         Index           =   0
         Left            =   135
         TabIndex        =   32
         Top             =   480
         Width           =   9540
         Begin Threed.SSFrame SSFrame2 
            Height          =   1572
            Left            =   0
            TabIndex        =   33
            Top             =   120
            Width           =   9492
            _Version        =   65536
            _ExtentX        =   16748
            _ExtentY        =   2778
            _StockProps     =   14
            Caption         =   " Available Constituent Groupings "
            ForeColor       =   -2147483630
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "Tahoma"
               Size            =   9.75
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Begin VB.ComboBox cboType 
               Appearance      =   0  'Flat
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   9.75
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   324
               ItemData        =   "CDBE.frx":065F
               Left            =   2040
               List            =   "CDBE.frx":066F
               Style           =   2  'Dropdown List
               TabIndex        =   62
               Top             =   1080
               Width           =   2136
            End
            Begin VB.ComboBox cboClass 
               Appearance      =   0  'Flat
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   9.75
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   324
               ItemData        =   "CDBE.frx":06AF
               Left            =   4320
               List            =   "CDBE.frx":06B1
               Style           =   2  'Dropdown List
               TabIndex        =   4
               Top             =   1080
               Width           =   4896
            End
            Begin VB.OptionButton optType 
               Caption         =   "Chemicals"
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   9.75
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   270
               Index           =   0
               Left            =   240
               TabIndex        =   1
               Top             =   360
               Width           =   1700
            End
            Begin VB.OptionButton optType 
               Caption         =   "Radionuclides"
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   9.75
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   270
               Index           =   1
               Left            =   240
               TabIndex        =   2
               Top             =   720
               Width           =   1700
            End
            Begin VB.OptionButton optType 
               Caption         =   "Classification"
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   10.5
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   270
               Index           =   2
               Left            =   240
               TabIndex        =   3
               Top             =   1080
               Width           =   1700
            End
            Begin VB.Label lblCount 
               AutoSize        =   -1  'True
               Caption         =   "(0)"
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   9.75
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   240
               Left            =   8760
               TabIndex        =   35
               Top             =   240
               Width           =   252
            End
            Begin VB.Label Label1 
               AutoSize        =   -1  'True
               Caption         =   "Available"
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   9.75
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   240
               Left            =   7680
               TabIndex        =   34
               Top             =   240
               Width           =   804
            End
         End
         Begin Threed.SSFrame SSFrame1 
            Height          =   5808
            Left            =   0
            TabIndex        =   36
            Top             =   1800
            Width           =   9492
            _Version        =   65536
            _ExtentX        =   16743
            _ExtentY        =   10245
            _StockProps     =   14
            Caption         =   "Select Constituents for Analysis"
            ForeColor       =   -2147483630
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "Tahoma"
               Size            =   9.76
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Begin VB.CommandButton cmdFind 
               Appearance      =   0  'Flat
               BackColor       =   &H80000005&
               Caption         =   "Search &Next"
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   9.75
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   375
               Left            =   240
               TabIndex        =   5
               Top             =   480
               Width           =   1680
            End
            Begin VB.ListBox lstAllName 
               DataField       =   "Name"
               DataSource      =   "Data1"
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   9.75
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   4140
               Left            =   240
               TabIndex        =   37
               Tag             =   "name"
               Top             =   1440
               Width           =   4212
            End
            Begin VB.TextBox Text1 
               Height          =   360
               Left            =   2040
               TabIndex        =   6
               Top             =   480
               Width           =   2412
            End
            Begin VB.CommandButton cmdRemCont 
               Appearance      =   0  'Flat
               BackColor       =   &H80000005&
               Caption         =   "<<<  &Remove"
               Enabled         =   0   'False
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   9.75
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   372
               Left            =   4920
               TabIndex        =   10
               Top             =   1080
               Width           =   4224
            End
            Begin VB.CommandButton cmdSelCont 
               Appearance      =   0  'Flat
               BackColor       =   &H80000005&
               Caption         =   "&Select  >>>"
               Enabled         =   0   'False
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   9.75
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   360
               Left            =   240
               TabIndex        =   8
               Top             =   1095
               Width           =   4212
            End
            Begin VB.CheckBox chkCasid 
               Caption         =   "Show &CASID"
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   9.75
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   255
               Left            =   7440
               TabIndex        =   7
               Top             =   480
               Width           =   1692
            End
            Begin ComctlLib.TreeView tvwSelConts 
               Height          =   4140
               Left            =   4920
               TabIndex        =   11
               Top             =   1440
               Width           =   4224
               _ExtentX        =   7461
               _ExtentY        =   7303
               _Version        =   327682
               LabelEdit       =   1
               LineStyle       =   1
               Sorted          =   -1  'True
               Appearance      =   1
               BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
                  Name            =   "Tahoma"
                  Size            =   9.75
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
            End
            Begin VB.ListBox lstAllCasid 
               DataField       =   "Name"
               DataSource      =   "Data1"
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   9.75
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   4140
               Left            =   240
               TabIndex        =   9
               Tag             =   "casid"
               Top             =   1440
               Visible         =   0   'False
               Width           =   4212
            End
         End
      End
   End
   Begin MSComDlg.CommonDialog FileDialog 
      Left            =   13560
      Top             =   240
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin ComctlLib.StatusBar StatusBar1 
      Align           =   2  'Align Bottom
      Height          =   420
      Left            =   0
      TabIndex        =   31
      Top             =   8310
      Width           =   9825
      _ExtentX        =   17330
      _ExtentY        =   741
      SimpleText      =   ""
      _Version        =   327682
      BeginProperty Panels {0713E89E-850A-101B-AFC0-4210102A8DA7} 
         NumPanels       =   2
         BeginProperty Panel1 {0713E89F-850A-101B-AFC0-4210102A8DA7} 
            AutoSize        =   1
            Object.Width           =   9313
            TextSave        =   ""
            Key             =   ""
            Object.Tag             =   ""
         EndProperty
         BeginProperty Panel2 {0713E89F-850A-101B-AFC0-4210102A8DA7} 
            Object.Width           =   7937
            MinWidth        =   7937
            TextSave        =   ""
            Key             =   ""
            Object.Tag             =   ""
         EndProperty
      EndProperty
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Tahoma"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
   End
   Begin VB.TextBox Text2 
      Alignment       =   2  'Center
      BackColor       =   &H00E0E0E0&
      ForeColor       =   &H000000C0&
      Height          =   3135
      Left            =   840
      MultiLine       =   -1  'True
      TabIndex        =   67
      Text            =   "CDBE.frx":06B3
      Top             =   2400
      Width           =   8175
   End
   Begin VB.Image Image1 
      Height          =   8400
      Left            =   0
      Picture         =   "CDBE.frx":093F
      Stretch         =   -1  'True
      Top             =   0
      Width           =   9840
   End
   Begin VB.Menu mnuFile 
      Caption         =   "&File"
      Begin VB.Menu mnuNew 
         Caption         =   "&New"
      End
      Begin VB.Menu mnuOpen 
         Caption         =   "&Open"
      End
      Begin VB.Menu mnuClose 
         Caption         =   "&Close"
      End
      Begin VB.Menu mnuSep1 
         Caption         =   "-"
      End
      Begin VB.Menu mnuRestoreDB 
         Caption         =   "Restore from &DB"
      End
      Begin VB.Menu mnuRestoreGID 
         Caption         =   "Restore from &GID"
      End
      Begin VB.Menu mnuSep3 
         Caption         =   "-"
      End
      Begin VB.Menu mnuSave 
         Caption         =   "Sa&ve"
      End
      Begin VB.Menu mnuSaveAs 
         Caption         =   "Save &As"
      End
      Begin VB.Menu mnuSep2 
         Caption         =   "-"
      End
      Begin VB.Menu mnuPrint 
         Caption         =   "&Print..."
      End
      Begin VB.Menu mnuPrintSetup 
         Caption         =   "Print Set&up"
      End
      Begin VB.Menu mnuSep7 
         Caption         =   "-"
      End
      Begin VB.Menu mnuExitSave 
         Caption         =   "&Save and Exit"
      End
      Begin VB.Menu mnuExit 
         Caption         =   "E&xit"
      End
   End
   Begin VB.Menu mnuTools 
      Caption         =   "&Tools"
      Begin VB.Menu mnuPropImport 
         Caption         =   "&Property Import..."
      End
      Begin VB.Menu mnuPropWiz 
         Caption         =   "&Property Criteria Wizard..."
      End
      Begin VB.Menu mnuRef 
         Caption         =   "&References..."
      End
   End
   Begin VB.Menu mnuHelp 
      Caption         =   "&Help"
      Begin VB.Menu howto 
         Caption         =   "How to ..."
         Shortcut        =   {F1}
      End
      Begin VB.Menu mnuAbout 
         Caption         =   "&About"
      End
   End
End
Attribute VB_Name = "frmCDBE"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

'Previous Command lines
'C:\ARAMSPRG\FRAMES\arams_dod.mdb c:\frames\pwrmax c:\source\~tmp 1 1 con1
'C:\FRAMES\fui.mdb c:\framesfiles\case\case_01gw c:\source\~tmp 1 1 con1

Private Const KEY_F3 = &H72
Private Const TAB_SELECT As Long = 0
Private Const TEMPLATE As String = "\CDBETemplate.mdb"

Private idCol As Long
Private ID_EDIT As Boolean
Private PROP_EDIT As Boolean
Private checking As Boolean
Private chkViewSel As Boolean
Private IgnoreChange As Boolean
Private myAnchor As String
Private lstVisible As ListBox

Public Sub Log(myError As String)
  put_val errfile, myError
  put_line errfile
End Sub

'=================================================================
'File menu click events
'=================================================================
Private Sub mnuFile_Click()
Dim OK As Boolean

  OK = (Len(DBName) = 0)
  mnuNew.Enabled = OK
  mnuOpen.Enabled = OK
  mnuClose.Enabled = Not OK
  mnuSave.Enabled = Not OK
  mnuSaveAs.Enabled = Not OK
  mnuExitSave.Enabled = Not OK
  mnuRestoreDB.Enabled = Not OK
  mnuRestoreGID.Enabled = Not OK
  mnuPrint.Enabled = Not OK
  mnuPrintSetup.Enabled = Not OK
End Sub

Private Sub mnuNew_Click()
Dim newdbName As String

  MousePointer = vbHourglass
  mnuClose_Click
  newdbName = GetFileName(DB_NEW)
  If GoodFileName(newdbName) Then
    If NewDatabase(newdbName) Then
      LoadData newdbName
    End If
  End If
  SSTab1_Click -1
  MousePointer = vbDefault
End Sub

Private Sub mnuOpen_Click()
Dim newdbName As String

  MousePointer = vbHourglass
  mnuClose_Click
  newdbName = GetFileName(DB_OPEN)
  If GoodFileName(newdbName) Then
    LoadData newdbName
  End If
  SSTab1_Click -1
  MousePointer = vbDefault
End Sub

Private Sub mnuClose_Click()
Dim answer As Long
  
  MousePointer = vbHourglass
  If Len(DBName) = 0 Then Exit Sub
  If Not IN_FRAMES Then
    answer = MsgBox("Do you want save changes?", 51, App.Title)
    If answer = 2 Then Exit Sub
    If answer = 6 Then mnuSave_Click
    ResetDatabaseObjects
    SSTab1_Click -1
  End If
  MousePointer = vbDefault
End Sub

Private Sub mnuPrintSetup_Click()

On Error GoTo ErrorHandler
  
  MousePointer = vbHourglass
  FileDialog.Flags = cdlPDPrintSetup Or cdlPDPrintToFile
  FileDialog.ShowPrinter
  MousePointer = vbDefault

ErrorHandler:

End Sub

Private Sub mnuPrint_Click()
  frmPrint.Show 1
End Sub

Private Sub mnuPropImport_Click()
 frmPropImport.Show vbModal
End Sub

Private Sub mnuRestoreGID_Click()
Dim fName As String
  
  MousePointer = vbHourglass
  fName = DBName
  ResetDatabaseObjects
  LoadData fName, IN_FRAMES
  SSTab1_Click -1
  MousePointer = vbDefault
End Sub

Private Sub mnuRestoreDB_Click()
Dim fName As String
  
  MousePointer = vbHourglass
  fName = DBName
  ResetDatabaseObjects
  LoadData fName, False
  SSTab1_Click -1
  MousePointer = vbDefault
End Sub

Private Sub mnuSave_Click()
  MousePointer = vbHourglass
  If IN_FRAMES Then
    Write_Contams_16 tvwSelConts ' lstSrcConts
  Else
    ws.CommitTrans
    ws.BeginTrans
  End If
  MousePointer = vbDefault
End Sub

Private Sub mnuSaveAs_Click()
Dim OldDBName As String
Dim newdbName As String

  MousePointer = vbHourglass
  OldDBName = DBName
  newdbName = GetFileName(DB_SAVE_AS)
  If GoodFileName(newdbName) Then
    ws.CommitTrans
    ws.BeginTrans  '
    ResetDatabaseObjects
    If CopyDatabase(OldDBName, newdbName) Then
      LoadData newdbName
      If Len(DBName) = 0 Then
        LoadData OldDBName
      End If
      SSTab1_Click -1
    End If
  End If
  MousePointer = vbDefault
End Sub

Private Sub mnuExitSave_Click()
  mnuSave_Click
  mnuExit_Click
End Sub

Private Sub mnuExit_Click()
  Form_Unload 0
End Sub

'=================================================================
'Tools menu click events
'=================================================================
Private Sub mnuTools_Click()
  mnuPropImport.Enabled = Len(DBName) > 0 And (Not IN_FRAMES)
  mnuPropWiz.Enabled = Len(DBName) > 0 And (Not IN_FRAMES)
  mnuRef.Enabled = (Len(DBName) > 0)
End Sub

Private Sub mnuPropWiz_Click()
 frmPropWiz.Show vbModal
 SSTab1_Click SSTab1.Tab
End Sub

Private Sub mnuRef_Click()
  If Len(DBName) > 0 Then
    RefUnload = False
    frmReference.Show
    frmReference.SetFocus
  Else
    Beep
  End If
End Sub

'=================================================================
'Help menu click events
'=================================================================
Public Sub howto_Click()
  GetHelp
End Sub

Private Sub mnuAbout_Click()
  frmAbout.Show 1, Me
End Sub

'=================================================================
'Form events
'=================================================================
Private Sub Form_Activate()
  RestoreAnchor
End Sub

Private Sub Form_KeyUp(KeyCode As Integer, Shift As Integer)
  If KeyCode = KEY_F3 And cmdFind.Enabled Then
    cmdFind_Click
  End If
End Sub

Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
  Select Case KeyCode
    Case vbKeyF1:
      KeyCode = 0
      GetHelp
  End Select
End Sub

Private Sub Form_load()
Dim fle As parmfile
On Error GoTo ErrorHandler

  'ChDir "C:\FramesV1"
  
  MousePointer = vbHourglass
  Set lstVisible = lstAllName
  frmCDBEStartup Me, 1
  SetRefFile ReplaceExt(FUIName, "ref")
  SetHelpFile App.Path & "\CDBE.htm"
  HelpAnchor = "Top"
  SaveAnchor HelpAnchor

  mnuNew.Visible = Not IN_FRAMES
  mnuOpen.Visible = Not IN_FRAMES
  mnuClose.Visible = Not IN_FRAMES
  mnuSave.Visible = Not IN_FRAMES
  mnuSaveAs.Visible = Not IN_FRAMES
  mnuSep1.Visible = Not IN_FRAMES
  mnuSep2.Visible = Not IN_FRAMES
  mnuSep7.Visible = Not IN_FRAMES
  mnuPrint.Visible = Not IN_FRAMES
  mnuPrintSetup.Visible = Not IN_FRAMES
  mnuPropImport.Visible = Not IN_FRAMES
  mnuPropWiz.Visible = Not IN_FRAMES
  SSTab2.TabVisible(0) = Not IN_FRAMES
  SSTab2.TabEnabled(0) = Not IN_FRAMES
  SSTab2.TabVisible(2) = Not IN_FRAMES
  SSTab2.TabEnabled(2) = Not IN_FRAMES

  If Len(Dir$(App.Path & TEMPLATE)) = 0 Then
    If IN_FRAMES Then
      PutError "Missing required file frmCDBETemplate.mdb"
    Else
      MsgBox "Missing required file frmCDBETemplate.mdb", vbOKOnly + vbExclamation, "Closing Application"
    End If
    Unload Me
  End If
  
  If IN_FRAMES Then
    If Not (GoodFileName(argv(0)) And Len(Dir$(argv(0))) <> 0) Then
      PutError "Database Name (" & argv(0) & ") is invalid!"
      Unload Me
    End If
    'check for GID file
    If Not open_parm(fle, FUIName & ".GID", 2) Then
      PutError "Can't open file " & FUIName
      Unload Me
    Else
      close_parm fle
    End If
    LoadData argv(0), True
    If InStr(FUIName, "V2Temp") > 0 Then
      chkCasid.value = 1
      AddFramesIISelectedContam lstVisible
    End If
  Else
    If argc > 0 Then
      If GoodFileName(argv(0)) And Len(Dir$(argv(0))) <> 0 Then
        LoadData argv(0)
      Else
        MsgBox "Database Name (" & argv(0) & ") is invalid!", vbExclamation, "File Path\Name Error"
      End If
    End If
  End If
  
  SSTab1_Click -1
  MousePointer = vbDefault
      
ErrorHandler:
  If Err.Number <> 0 Then
    MsgBox Error, vbOKOnly
  End If
End Sub

Private Sub Form_Unload(Cancel As Integer)
On Error Resume Next
  
  mnuClose_Click
  If IN_FRAMES Then
    close_csv errfile
    If Not AnError Then Kill RunName & ".ERR"
  End If
  
  If ReferenceLoaded Then
    frmReference.Hide
    Unload frmReference
  End If
  If EstimateLoaded Then
    frmEstimate.Hide
    Unload frmEstimate
  End If
  Set lstVisible = Nothing
  
  Close
  End
End Sub

'=================================================================
'  Object click events
'=================================================================

Private Sub cboBranch_Click(index As Integer)
  If checking Then Exit Sub
  CheckBranching
  ResetChainDecayView
End Sub

Private Sub cboView_Click()
  ResetView
End Sub

Private Sub chkShowCasid_Click()
  ResetView
End Sub

Private Sub cmdCopy_Click()
  If lstCon.ListIndex < 0 Then Exit Sub
  Load frmNewCAS
  frmNewCAS.tag = "copy"
  frmNewCAS.Show 1
End Sub

Private Sub cmdBranch_Click(index As Integer)
  If checking Then Exit Sub
  
  If cboBranch(index).ListCount = 1 Then
    cboBranch(index).AddItem ""
  End If
  FindSynonyms lstDeg.itemdata(lstDeg.ListIndex), "", ""
  cboBranch(index).list(1) = tblChems!name
  cboBranch(index).itemdata(1) = lstDeg.ListIndex
  cboBranch(index).ListIndex = 1
  cmdBranch(0).Enabled = False
  cmdBranch(1).Enabled = False
  CheckBranching
  txtBranch(index).Enabled = True
  txtBranch(index).SetFocus
  ResetChainDecayView
End Sub

Private Sub chkRad_Click()
Dim id As Long
  
  If IgnoreChange Then Exit Sub
  id = chkRad.value
  DB.Execute "UPDATE Chemicals SET clktype=" & id & " WHERE casid='" & txtCasid.text & "'"
  SetAllContsList KTYPE, id, lstDeg, Null
  SetListindex Combo0, 0
  SetListindex Combo1, 0
  If id = 1 Then
    SetListindex Combo2, 1
    SetListindex Combo3, 14
  Else
    SetListindex Combo2, 5
    SetListindex Combo3, 0
  End If
End Sub

Private Sub Combo0_Click()
Dim CTYPE As Long
  
  If IgnoreChange Then Exit Sub
  If Combo0.ListIndex < 0 Then Exit Sub
  
  CTYPE = Combo0.itemdata(Combo0.ListIndex)
  DB.Execute "UPDATE Chemicals SET clchem=" & CTYPE & " WHERE casid='" & txtCasid.text & "'"
End Sub

Private Sub Combo1_Click()
Dim CTYPE As Long
  
  If IgnoreChange Then Exit Sub
  If Combo1.ListIndex < 0 Then Exit Sub
  
  CTYPE = Combo1.itemdata(Combo1.ListIndex)
  DB.Execute "UPDATE Chemicals SET clptype=" & CTYPE & " WHERE casid='" & txtCasid.text & "'"
End Sub

Private Sub Combo2_Click()
Dim CTYPE As Long
  
  If IgnoreChange Then Exit Sub
  If Combo2.ListIndex < 0 Then Exit Sub
  
  CTYPE = Combo2.itemdata(Combo2.ListIndex)
  DB.Execute "UPDATE Chemicals SET cletype=" & CTYPE & " WHERE casid='" & txtCasid.text & "'"
End Sub

Private Sub Combo3_Click()
Dim CTYPE As Long
  
  If IgnoreChange Then Exit Sub
  If Combo3.ListIndex < 0 Then Exit Sub
  
  CTYPE = Combo3.itemdata(Combo3.ListIndex)
  DB.Execute "UPDATE Chemicals SET clrtype=" & CTYPE & " WHERE casid='" & txtCasid.text & "'"
End Sub

Private Sub SSTab1_Click(PreviousTab As Integer)
  StatusBar1.Panels(2).text = ""
  If SSTab1.Tab = TAB_SELECT Then
    Frame3(TAB_SELECT).Enabled = True
    SSTab2.Enabled = False
  Else
    Frame3(TAB_SELECT).Enabled = False
    SSTab2.Enabled = True
    SSTab2_Click -1 ' for event to enable/disable child frames
  End If
  If Len(DBName) > 0 Then
    HelpAnchor = "Selection"
    SSTab1.Visible = True
  Else
    HelpAnchor = ""
    SSTab1.Visible = False
  End If
End Sub

Private Sub SSTab2_Click(PreviousTab As Integer)
Dim i As Long
  
  StatusBar1.Panels(2).text = ""
  If 0 < InStr(SSTab2.TabCaption(SSTab2.Tab), "Identification") Then
    HelpAnchor = "Identification"
  End If
  If 0 < InStr(SSTab2.TabCaption(SSTab2.Tab), "Properties") Then
    HelpAnchor = "Properties"
  End If
  For i = 0 To fraTabStrip.Count - 1
    If i = SSTab2.Tab Then
      fraTabStrip(i).Enabled = True
    Else
      fraTabStrip(i).Enabled = False
    End If
  Next
  If Len(DBName) = 0 Then HelpAnchor = "Top"
End Sub

Private Sub tvwSelConts_NodeClick(ByVal Node As ComctlLib.Node)
  cmdSelCont.Enabled = False
  cmdRemCont.Enabled = True
  cmdRemCont.Default = True
End Sub

Private Sub lstCon_click()
Dim rowid As Long
Dim nodx As Node

  If IgnoreChange Then Exit Sub
  
  IgnoreChange = True
  rowid = lstCon.itemdata(lstCon.ListIndex)
'  FindSynonyms 0, lstCon.text, ""
  FindSynonyms rowid, "", ""
  txtCasid = tblChems!CASID
  txtCasid.tag = tblChems!rowid
  txtName = tblChems!name
  txtName.tag = txtName.text ' for change tests

  If IN_FRAMES Then
On Error Resume Next
    Set nodx = tvwSelConts.Nodes(cas & txtCasid.text)
    ID_EDIT = Not (Err.Number = 0)
On Error GoTo 0
  End If
  
  If chkViewSel Then
    PROP_EDIT = True
  Else
    PROP_EDIT = Not IN_FRAMES
  End If
  
  txtCasid.Locked = True
  txtName.Locked = Not ID_EDIT
  txtSyns.Locked = Not ID_EDIT
  chkRad.Enabled = ID_EDIT
  Combo0.Locked = Not ID_EDIT
  Combo1.Locked = Not ID_EDIT
  Combo2.Locked = Not ID_EDIT
  Combo3.Locked = Not ID_EDIT
  FillConstituentIdInfo
  UpdatePropertiesPage
  lstDeg_Click
  IgnoreChange = False
End Sub

Private Sub cmdAdd_Click()
  Load frmNewCAS
  frmNewCAS.tag = "new"
  frmNewCAS.Show 1
End Sub

Private Sub cmdDelete_Click()
Dim ans As Long
Dim msg As String
Dim qry As String

  If lstCon.ListIndex < 0 Then Exit Sub
  msg = "Are you sure you want delete all data for: " & vbCrLf
  msg = msg & UCase$(txtName & " (" & txtCasid & ")")
  ans = MsgBox(msg, vbYesNo, "Delete Constituent")
  If ans = vbYes Then
    qry = " WHERE casid='" & txtCasid & "'"
    DB.Execute "DELETE FROM Chemicals " & qry
    tblCat.MoveFirst
    Do While Not tblCat.EOF
      DB.Execute "DELETE FROM " & tblCat!TableName & qry
      tblCat.MoveNext
    Loop
    DB.Execute "DELETE FROM Branching " & qry
  End If
  cboType_Click
End Sub

Private Sub cmdFind_Click()
Dim found As Boolean

  found = list_find_again(lstVisible, cmdFind)
End Sub

Private Sub cmdPropEst_Click()
  SaveAnchor HelpAnchor
  frmEstimate.Show 1
  RestoreAnchor
End Sub

Private Sub RemoveParentCAS(id As String)
Dim nodx As Node
Dim rowid As Long
Dim pName As String
Dim pcasid As String
  
  For Each nodx In tvwSelConts.Nodes
    SplitNodeTag nodx.tag, rowid, pcasid, pName
    tblDecay.Seek "=", pcasid
    If Not tblDecay.NoMatch Then
      If Trim$(tblDecay!branch1) = id Then
        RemoveParentCAS pcasid
        tvwSelConts.Nodes.Remove nodx.index
        Exit Sub
      End If
    End If
  Next
End Sub

Private Sub cmdRemCont_Click()
Dim rowid As Long
Dim cName As String
Dim id As String

  SplitNodeTag tvwSelConts.SelectedItem.tag, rowid, id, cName
  RemoveParentCAS id
  tvwSelConts.Nodes.Remove tvwSelConts.SelectedItem.index
  cmdRemCont.Enabled = False
  ResetSelConts
  ResetView
End Sub

Private Sub cmdSelCont_Click()
  AddSelectedContam lstVisible
  tvwSelConts.Sorted = True
  cmdSelCont.Enabled = False
  ResetView
End Sub

Private Sub cboPropCat_Click()
Dim r As Long
Dim var As Variant

  HelpAnchor = "Properties"
  vaSpread1.GetText 1, 1, var
  If Len(var) = 0 Then Exit Sub
  
  For r = 1 To vaSpread1.MaxRows
    vaSpread1.GetText 1, r, var
    If var = cboPropCat Then
      vaSpread1.TopRow = r
      Exit For
    End If
  Next
End Sub

Private Sub lstAllCasid_Click()
  If lstAllCasid.Visible Then
    cmdSelCont.Enabled = True
    cmdRemCont.Enabled = False
    cmdSelCont.Default = True
  End If
End Sub

Private Sub lstAllName_Click()
  If lstAllName.Visible Then
    cmdSelCont.Enabled = True
    cmdRemCont.Enabled = False
    cmdSelCont.Default = True
  End If
End Sub

Private Sub lstDeg_Click()
Dim nodx As Node
Dim enable As Boolean

  enable = (0 < cboView.ListIndex)
  If enable Then

On Error Resume Next
    FindSynonyms lstDeg.itemdata(lstDeg.ListIndex), "", ""
    ' can not change a selected constituent decay chain
    Set nodx = tvwSelConts.Nodes(cas & tblChems!CASID)
    enable = (Err.Number <> 0)
On Error GoTo 0
    
    If enable Then
      If txtCasid.text = tblChems!CASID Then
        enable = False
      Else
        If tblChems!CASID = cboBranch(0).tag Or _
           tblChems!CASID = cboBranch(1).tag Then
           enable = False
        End If
      End If
    End If
  End If
  cmdBranch(0).Enabled = enable ' And True
  cmdBranch(1).Enabled = enable And IIf(cboBranch(0).ListIndex > 0, True, False)
End Sub

Private Sub optType_Click(index As Integer)
  cboType.Enabled = optType(2)
  cboClass.Enabled = optType(2)
  If cboType.ListIndex > -1 Then
    cboType_Click
  Else
    cboType.ListIndex = 0
  End If
End Sub

Private Sub cboClass_Click()
Dim id As Long
Dim myType As Long
    
  myType = cboType.ListIndex
  id = cboClass.itemdata(cboClass.ListIndex)
  cboView.list(1) = cboClass.list(cboClass.ListIndex)
  SetAllContsList myType, id, lstAllName, lstAllCasid
  lblCount.Caption = "(" & lstVisible.ListCount & ")"
  If cboView.ListIndex = -1 Then
    If IN_FRAMES Then
      cboView.ListIndex = 0
    Else
      cboView.ListIndex = 1
    End If
  Else
    cboView_Click
  End If
End Sub

Private Sub cboType_Click()
Dim id As Long
Dim myType As Long
Dim fld As String
  
  cboView.list(0) = "Selected Constituents"
  If optType(0) Then
    myType = KTYPE
    id = 0 ' chemicals
    cboView.list(1) = "All " & optType(0).Caption
  ElseIf optType(1) Then
    myType = KTYPE
    id = 1 ' radionuclides
    cboView.list(1) = "All " & optType(1).Caption
  Else
    myType = cboType.ListIndex
    Select Case myType
    Case 0: fld = CLCTYPE
    Case 1: fld = CLETYPE
    Case 2: fld = CLRTYPE
    Case 3: fld = CLPTYPE
    End Select
    If cboClass.tag <> fld Then
      cboClass.Clear
      SetClassList cboClass, fld, True
      cboClass.ListIndex = 0
      cboClass.tag = fld
    End If
    id = cboClass.itemdata(cboClass.ListIndex)
    cboView.list(1) = cboClass.list(cboClass.ListIndex)
  End If
  SetAllContsList myType, id, lstAllName, lstAllCasid
  lblCount.Caption = "(" & lstVisible.ListCount & ")"
  If cboView.ListIndex = -1 Then
    If IN_FRAMES Then
      cboView.ListIndex = 0
    Else
      cboView.ListIndex = 1
    End If
  Else
    cboView_Click
  End If
End Sub

Private Sub chkCasid_Click()
Dim i As Long
Dim rowid As Long
Dim id As String
Dim name As String
Dim nodx As Node

  If chkCasid.value = 0 Then
    lstAllName.Visible = True
    lstAllCasid.Visible = False
    Set lstVisible = lstAllName
  Else
    lstAllName.Visible = False
    lstAllCasid.Visible = True
    Set lstVisible = lstAllCasid
  End If
  For i = 1 To tvwSelConts.Nodes.Count
    Set nodx = tvwSelConts.Nodes(i)
    SplitNodeTag nodx.tag, rowid, id, name
    nodx.text = IIf(chkCasid.value = 0, name, id)
  Next
  tvwSelConts.tag = lstVisible.tag
  tvwSelConts.Sorted = True
  ResetView
  Text1_Change
End Sub

Private Sub vaSpread1_Change(ByVal col As Long, ByVal row As Long)
Dim i
i = i
End Sub

Private Sub vaSpread1_Click(ByVal col As Long, ByVal row As Long)
Dim var As Variant
  
  If Not col = 4 Or Not col = 3 Then Exit Sub
  vaSpread1.GetText 0, row, var
  If Len(var) > 0 Then HelpAnchor = var
  vaSpread1.GetText 4, row, var
  If IsNumeric(var) Then RefIdx = val(var) Else RefIdx = -1
  RefParam = HelpAnchor
  RefUnload = False
  frmReference.set_refrec
End Sub

'=================================================================
'  Object dblclick events
'=================================================================

Private Sub lstAllCasid_DblClick()
  cmdSelCont_Click
End Sub

Private Sub lstAllName_DblClick()
  cmdSelCont_Click
End Sub

Private Sub vaSpread1_DblClick(ByVal col As Long, ByVal row As Long)
  If Not col = 4 Then Exit Sub
  vaSpread1_Click col, row
  If RefIdx = -1 Then Exit Sub
  mnuRef_Click
End Sub

'=================================================================
'  Object change events
'=================================================================

Private Sub Text1_Change()
  If Len(Text1.text) = 0 Then
    cmdFind.Enabled = False
    Exit Sub
  End If
  SetFindString Text1.text
  list_find lstVisible, cmdFind, Text1
End Sub

Private Sub txtBranch_Change(index As Integer)
Dim sval As Single
Dim tval As Single

  If checking Then Exit Sub
  If Not IsNumeric(txtBranch(index).text) Then
    txtBranch(index).BackColor = lightRed
    Exit Sub
  End If
  sval = CDbl(txtBranch(index).text)
  If sval < 0# Or (Not sval > 0#) Or sval > 1# Then
    txtBranch(index).BackColor = lightRed
    Exit Sub
  End If
  txtBranch(index).BackColor = vbWhite
  If index = 0 Then tval = val(txtBranch(1).text)
  If index = 1 Then tval = val(txtBranch(0).text)

  If index = 0 Then
    If 1# < (sval + tval) Then
      txtBranch(1).BackColor = lightRed
    Else
      txtBranch(1).BackColor = vbWhite
    End If
  Else
    If 1# < (sval + tval) Then
      txtBranch(0).BackColor = lightRed
    Else
      txtBranch(0).BackColor = vbWhite
    End If
  End If
  txtBranch(index).BackColor = vbWhite
  CheckBranching
End Sub

Private Sub txtName_Change()
  If IgnoreChange Then Exit Sub

  If (txtName.text <> txtName.tag) And (Len(txtName) = 0 Or FindSynonyms(0, "", txtName.text)) Then
    txtName.BackColor = lightRed
  Else
    txtName.BackColor = vbWhite
  End If
End Sub

Private Sub txtSyns_Change()
Dim i As Long
Dim lb As Long
Dim ub As Long
Dim str As String
Dim var As Variant
  
  If IgnoreChange Then Exit Sub
  If txtSyns.text = txtSyns.tag Then Exit Sub
  
  var = Split(txtSyns, vbCrLf)
  lb = LBound(var)
  ub = UBound(var)
  If ub >= 0 Then
    For i = lb To ub
      str = Trim$(var(i))
      If Len(str) > 0 Then
        If FindSynonyms(0, "", str) Then
          If tblChems!CASID <> txtCasid.text Then
            txtSyns.BackColor = lightRed
            Exit Sub
          End If
        End If
      End If
    Next
  End If
End Sub

Private Sub vaSpread1_ComboSelChange(ByVal col As Long, ByVal row As Long)
  vaspread1_EditChange col, row
  SendKeys "~"
End Sub

'=================================================================
'  Object misc events
'=================================================================

Private Sub Text1_KeyDown(KeyCode As Integer, Shift As Integer)
  Select Case KeyCode
    Case vbKeyUp:
      If lstVisible.ListIndex > 0 Then lstVisible.ListIndex = lstVisible.ListIndex - 1
      KeyCode = 0
    Case vbKeyDown:
      If lstVisible.ListIndex < lstVisible.ListCount - 1 Then lstVisible.ListIndex = lstVisible.ListIndex + 1
      KeyCode = 0
  End Select
End Sub

Private Sub txtName_LostFocus()
  If IgnoreChange Then Exit Sub
  If txtName.BackColor = lightRed Then Exit Sub
  If txtName.text <> txtName.tag Then
    ' search for duplicate fscName
    If Not FindSynonyms(0, "", txtName.text) Then
        SetParamValue "Name", txtCasid.text, txtName.text, 0
        DB.Execute "UPDATE Chemicals SET Name='" & txtName.text & "' WHERE rowid=" & txtCasid.tag
        If Not chkCasid Then
          lstCon.list(lstCon.ListIndex) = txtName.text
        End If
    End If
  End If
End Sub

Private Sub txtSyns_LostFocus()
Dim i As Long
Dim lb As Long
Dim ub As Long
Dim syns As Variant
Dim value As String
  
  If IgnoreChange Then Exit Sub
  If txtSyns.BackColor = lightRed Then Exit Sub
  If txtSyns.text = txtSyns.tag Then Exit Sub
  If Len(txtCasid.text) = 0 Then Exit Sub
      
  DB.Execute "DELETE FROM Chemicals WHERE casid='" & txtCasid.text & "' AND NOT rowid=" & txtCasid.tag
  syns = Split(txtSyns, vbCrLf)
  lb = LBound(syns)
  ub = UBound(syns)
  If ub >= 0 Then
    For i = lb To ub
      value = Trim$(syns(i))
      If Len(value) > 0 Then
        If Not FindSynonyms(0, "", value) Then
          tblChems.AddNew
          tblChems!CASID = txtCasid.text
          tblChems!name = value
          tblChems.update
        End If
      End If
    Next
  End If
  DB.Execute "UPDATE Chemicals SET " & CLKTYPE & "=" & CStr(chkRad.value) & _
      ", " & CLCTYPE & "=" & Combo0.itemdata(Combo0.ListIndex) & _
      ", " & CLPTYPE & "=" & Combo1.itemdata(Combo1.ListIndex) & _
      ", " & CLETYPE & "=" & Combo2.itemdata(Combo2.ListIndex) & _
      ", " & CLRTYPE & "=" & Combo3.itemdata(Combo3.ListIndex) & _
      " WHERE casid='" & txtCasid.text & "'"
End Sub

Private Sub vaSpread1_LeaveCell(ByVal col As Long, ByVal row As Long, ByVal NewCol As Long, ByVal NewRow As Long, Cancel As Boolean)
Dim var As Variant

  If NewCol < 0 Or NewRow < 0 Then Exit Sub
  If NewCol > 4 Then
    Cancel = True
    Exit Sub
  End If
  vaSpread1.GetText 0, NewRow, var
  If Len(var) > 0 Then HelpAnchor = var
  RefParam = HelpAnchor
  vaSpread1.GetText 4, NewRow, var
  If IsNumeric(var) Then RefIdx = val(var) Else RefIdx = -1
  RefUnload = False
  frmReference.set_refrec
  SetRangeDisplay StatusBar1, val(chkRad.value)
End Sub

'=================================================================
'  Helper functions
'=================================================================

Private Sub LoadData(ByVal fileName As String, Optional PostGidData As Boolean = False)
Dim i As Long
Dim ub As Long
Dim CTYPE As Long
Dim temp As String
Dim tdf As TableDef
  
On Error Resume Next
  
  Set DB = ws.OpenDatabase(fileName, False, False)
  If Err.Number <> 0 Then
    MsgBox Error, vbCritical, "Loading data"
    PutError Error
    Exit Sub
  End If
  
  ' indicates this database has not been transformed
  Set tdf = DB.TableDefs("Chemicals")
  If Err.Number <> 0 Then
    DB.Close
    Err.Clear
    Set tdf = Nothing
    
    If Not ValidDatabase(fileName) Then
      MsgBox "Not Valid Database " & fileName
      PutError "Not Valid Database " & fileName
      Exit Sub
    End If
  
    If Not TransformDatabase(fileName) Then
      MsgBox "Error Transforming Database " & fileName
      PutError "Error Transforming Database " & fileName
      Exit Sub
    End If
  Else
    Set tdf = Nothing
    DB.Close
  End If
  
  ' now check that any changes to parameter categories are pushed out to the tables
  RefreshDatabase fileName
  
  ' before opening database, make a copy to allow for SaveAs and recovery
  ' Make sure there isn't already a file with the Name of the compacted database.
  If Not IN_FRAMES Then
    DBNameBackup = "db" & Format(Timer, "00000") & ".mdb"
    If Len(DBNameBackup) > 0 And Len(Dir$(DBNameBackup)) > 0 Then Kill DBNameBackup
    CopyDatabase fileName, DBNameBackup
  End If

On Error GoTo LoadData_Error
  
  OpenDatabaseRecordsets fileName
  If PostGidData Then Read_Contams
  
  tblCat.MoveFirst
  While Not tblCat.EOF
    If tblCat!Category <> 1 Then
      cboPropCat.AddItem tblCat!Description
      cboPropCat.itemdata(cboPropCat.NewIndex) = tblCat!Category
    End If
    tblCat.MoveNext
  Wend
  If cboPropCat.ListCount > 0 Then cboPropCat.ListIndex = 0
  InitDataSpreadsheet
  SetClassList Combo0, CLCTYPE, False
  SetClassList Combo1, CLPTYPE, False
  SetClassList Combo2, CLETYPE, False
  SetClassList Combo3, CLRTYPE, False
  tvwSelConts.Nodes.Clear
  DBName = fileName
  CTYPE = KTYPE
  
  ' it appears this call is necessary to prevent errors when
  ' adding attributes in frmPropWiz.  There must be a reason
  ' but considerable effort to identify it explicitly has been
  ' unsuccessful - and so - leave this call until it is. BLH
  CreateAttribute ""
  Err.Clear
  
  If IN_FRAMES Then
    temp = ""
    ub = UBound(cont)
    For i = 1 To ub
      If Len(cont(i).id) > 0 Then
        FindSynonyms 0, cont(i).id, cont(i).name
        If tblChems.NoMatch Then
          'should not happen if gid is loaded correctly
          temp = temp & vbCrLf & cont(i).name
          cont(i).id = ""
          cont(i).name = ""
        Else
          AddCont cont(i).name, cont(i).id, tblChems!rowid
        End If
      End If
    Next
    If temp <> "" Then
      MsgBox temp, vbExclamation, "Constituents did not exist in the database!"
    End If
  End If
  
  If optType(2).value Then
    optType_Click 2
  Else
    optType(2).value = True    'need click to be called
  End If
  
  StatusBar1.Panels(1) = DBName
    
 'placed here so whenever a db is loaded it check for cooresponding DES file
  temp = SplitPath(fileName, SP_DIR) & SplitPath(fileName, SP_TITLE) & ".des"
  If (Len(Dir$(temp)) = 0 And SplitPath(fileName, SP_TITLE) <> "fui") Then
    CreateDes fileName
  End If

LoadData_Error:
  If AnError Then mnuExit_Click
  If Err.Number <> 0 Then
    MsgBox Error
  End If
End Sub

Private Sub AddCont(name As String, id As String, itemdata As Long)
Dim nodx As Node

On Error GoTo ErrorHandler
  Set nodx = tvwSelConts.Nodes.Add(, , cas & id, name)
  nodx.tag = SetNodeTag(itemdata, id, name)
  SelectDecayChain True, id, cas & id

ErrorHandler:
  If Err.Number <> 0 Then
    Err.Clear
  End If
End Sub

Private Sub SetClassList(cbo As ComboBox, fld As String, all As Boolean)
Dim qry As String
Dim rst As Recordset
Dim itype As Long
  
    qry = "SELECT * FROM Classification WHERE " & fld & "<>0 ORDER BY " & fld
    Set rst = DB.OpenRecordset(qry, dbOpenSnapshot)
    If Not rst.EOF Then
      rst.MoveFirst
      While Not rst.EOF()
        itype = rst.Fields(fld)
        If all Or (itype >= 0 And Not all) Then
          cbo.AddItem rst!Desc
          cbo.itemdata(cbo.NewIndex) = itype
        End If
        rst.MoveNext
      Wend
    End If
    rst.Close
    cbo.AddItem "Not Assigned"
    cbo.itemdata(cbo.NewIndex) = 0
End Sub

Private Sub SetAllContsList(myType As Long, idx As Long, name As Variant, lst As Variant)
Dim qry As String
Dim rst As Recordset

  If IsObject(name) Then name.Clear
  If IsObject(lst) Then lst.Clear
  qry = "FROM Chemicals "
  If myType = KTYPE And idx >= 0 Then qry = qry & " WHERE " & CLKTYPE & "=" & idx
  If myType = RTYPE And idx >= 0 Then qry = qry & " WHERE " & CLRTYPE & "=" & idx
  If myType = RTYPE And idx = -2 Then qry = qry & " WHERE " & CLRTYPE & "<11"                             ' all organics
  If myType = RTYPE And idx = -3 Then qry = qry & " WHERE (" & CLRTYPE & ">=11 AND " & CLRTYPE & "<14)"   ' all inorganics
  If myType = PTYPE And idx >= 0 Then qry = qry & " WHERE " & CLPTYPE & "=" & idx
  If myType = ETYPE And idx >= 0 Then qry = qry & " WHERE " & CLETYPE & "=" & idx
  If myType = CTYPE And idx >= 0 Then qry = qry & " WHERE " & CLCTYPE & "=" & idx
  
  If IsObject(name) Then
    Set rst = DB.OpenRecordset("SELECT * " & qry & " ORDER BY Name", dbOpenDynaset)
    If Not rst.EOF Then rst.MoveFirst
    Do While Not rst.EOF
      name.AddItem LTrim$(rst!name)
      name.itemdata(name.NewIndex) = rst!rowid
      rst.MoveNext
    Loop
    rst.Close
  End If
  
  If IsObject(lst) Then
    Set rst = DB.OpenRecordset("SELECT DISTINCT casid, MIN(RowId) AS MinRowId " & qry & " GROUP BY casid", dbOpenDynaset)
    If Not rst.EOF Then rst.MoveFirst
    Do While Not rst.EOF
      lst.AddItem LTrim$(rst!CASID)
      lst.itemdata(lst.NewIndex) = rst!MinRowId
      rst.MoveNext
    Loop
    rst.Close
  End If
End Sub

Private Sub AddFramesIISelectedContam(lst As ListBox)
Dim i As Long
Dim j As Long
Dim x As Long
Dim id As String
Dim ncon As Long
Dim hIcon As Long
Dim hGale As Long
Dim PID As Integer
Dim modId As String
Dim Path As String
  
  Path = Environ("fPTH")
  PID = CInt(Environ("fPID"))
  modId = Environ("fMOD")
  hLibModule = LoadLibrary(Path & "\systemio.dll")
  If 0 > hLibModule Then Exit Sub

  x = 0
  ncon = 0
  hIcon = IconGetHandle(PID, modId)
  hGale = IconGetInputDataSet(PID, modId, "GaleReo", 1)
  ncon = DataSetDimensionCount(PID, hGale, CASID, SetIdx())
  If ncon > 0 Then
    For i = 0 To ncon - 1
      id = DataSetReadString1(PID, hGale, CASID, "", i + 1)
      For j = 0 To lst.ListCount - 1
        If lst.list(j) = id Then
          lst.Selected(j) = True
          cmdSelCont_Click
          x = x + 1
          Exit For
        End If
      Next
'      If j = lst.ListCount Then
'        Debug.Print id
'      End If
    Next
  End If
  
  If 0 <> hLibModule Then FreeLibrary hLibModule
  

End Sub

Private Sub AddSelectedContam(lst As ListBox) ' , sel As ListBox, cas As ListBox)
Dim i As Long
Dim rowid As Long
Dim name As String
Dim id As String
Dim msg As String
Dim nodx As Node
Dim nodz As Node
Dim tvwkey As String
Dim found As Boolean
Dim oldcas As String
  
On Error Resume Next
  oldcas = txtCasid.text
  For i = 0 To lst.ListCount - 1
    If lst.Selected(i) Then
      rowid = lst.itemdata(i)
      FindSynonyms rowid, "", ""
      name = tblChems!name
      id = tblChems!CASID
      tvwkey = cas & id
      
      Set nodx = tvwSelConts.Nodes(cas & id)
      If Err.Number = 0 Then
        If nodx.text = name Then
          MsgBox "Constituent already selected.", vbOKOnly, "Duplicate"
        Else
          msg = "Constituent already selected as: " & nodx.text & "." & vbCrLf
          msg = msg & " Do you want to replace Name with: " & name & "?"
          If vbYes = MsgBox(msg, vbYesNo, "Replace Duplicate") Then
            nodx.text = IIf(tvwSelConts.tag = CASID, id, name)
            nodx.tag = SetNodeTag(rowid, id, name)
          End If
        End If
      Else
        Set nodx = tvwSelConts.Nodes.Add(, , tvwkey, lst.list(i))
        nodx.tag = SetNodeTag(rowid, id, name)
        txtCasid.text = id
        GetDecayChain id, True
        For Each nodx In tvwDKview(DK_CHAIN).Nodes
          'remove duplicates
          found = False
          For Each nodz In tvwSelConts.Nodes
            SplitNodeTag nodx.tag, rowid, id, name
            If cas & id = nodz.key Then
              found = True
              Exit For
            End If
          Next
          ' add constituent
          If Not found Then
            Set nodz = tvwSelConts.Nodes.Add(, , cas & id, IIf(tvwSelConts.tag = CASID, id, name))
            nodz.tag = nodx.tag
          End If
        Next
        txtCasid.text = oldcas
      End If
    End If
  Next
End Sub

Private Function ValidDatabase(fileName As String) As Boolean
Dim tdb As Database
Dim tdf As TableDef

On Error Resume Next

  ValidDatabase = False
  Set tdb = ws.OpenDatabase(fileName, False, False)
  If Err.Number <> 0 Then Exit Function
  Set tdf = tdb.TableDefs("CATEGORY")
  Set tdf = tdb.TableDefs("PARAM")
  Set tdf = tdb.TableDefs("KD_DATA")
  Set tdf = tdb.TableDefs("REF")
  If Err.Number = 0 Then
    Set tdf = tdb.TableDefs("DATA")
    If Err.Number = 0 Then
      Set tdf = tdb.TableDefs("DKCHAIN")
      If Err.Number = 0 Then
        tdb.Close
        ValidDatabase = True
        Exit Function
      End If
    End If
    
    Err.Clear
    Set tdf = tdb.TableDefs("old DATA")
    If Err.Number = 0 Then
      Set tdf = tdb.TableDefs("old DKCHAIN")
      If Err.Number = 0 Then
        tdb.Close
        ValidDatabase = True
        Exit Function
      End If
    End If
  End If
  tdb.Close
End Function

Private Sub GetBranching()
Dim ref As Long
Dim id As String
Dim casid1 As String
Dim casid2 As String
Dim cName As String
  
  If Len(txtName) = 0 Then Exit Sub
  If cboBranch(0).ListCount = 0 Then Exit Sub
  If cboBranch(1).ListCount = 0 Then Exit Sub
  
  cboBranch(0).ListIndex = 0
  cboBranch(1).ListIndex = 0
  FindSynonyms 0, "", txtName
  If tblChems.NoMatch Then Exit Sub
  
  id = tblChems!CASID
  tblDecay.Seek "=", id
  If Not tblDecay.NoMatch Then
    casid1 = ""
    If Not IsNull(tblDecay!branch1) Then casid1 = Trim$(tblDecay!branch1)
    If Not IsNull(tblDecay!branch2) Then casid2 = Trim$(tblDecay!branch2)
    
    If Len(casid1) > 0 Then
      cName = GetParamValue("Name", Trim$(tblDecay!branch1), ref)
      If Len(cName) = 0 Then cName = casid1
      cboBranch(0).list(1) = cName
      cboBranch(0).tag = casid1
      txtBranch(0).text = Format(tblDecay!fraction1, ".00##")
      cboBranch(0).ListIndex = 1
    End If
    
    If Len(casid2) > 0 Then
      cName = GetParamValue("Name", casid2, ref)
      If Len(cName) = 0 Then cName = casid2
      cboBranch(1).list(1) = cName
      cboBranch(1).tag = casid2
      txtBranch(1).text = Format(tblDecay!fraction2, ".00##")
      cboBranch(1).ListIndex = 1
    End If
  End If
End Sub

Public Sub GetDecayStraight(id As String, Optional Clear As Boolean = False)
Dim ref As Long
Dim key As String
Dim cName As String
Dim nodx As Node
Dim tvw As TreeView
  
On Error Resume Next
  
  Set tvw = tvwDKview(DK_FLAT)
  If Clear Then tvw.Nodes.Clear
  tblDecay.Seek "=", id
  If tblDecay.NoMatch Then Exit Sub
  
  If Clear Then
    FindSynonyms 0, id, ""
    Set nodx = tvw.Nodes.Add(, , cas & id, tblChems!name)
    nodx.tag = SetNodeTag(0, id, tblChems!name)
  End If
  tblDecay.Seek "=", id
  If tblDecay.NoMatch Then Exit Sub

  Set nodx = tvw.Nodes(tblDecay!branch1)
  If Err.Number = 0 Then Exit Sub

  key = Trim$(tblDecay!branch1)
  cName = GetParamValue("Name", key, ref)
  If Len(cName) = 0 Then cName = key
  Set nodx = tvw.Nodes.Add(, , cas & key, cName)
  nodx.tag = SetNodeTag(0, key, cName)
  GetDecayStraight key
End Sub

Public Sub GetDecayChain(id As String, Optional Clear As Boolean = False)
Dim nodx As Node
Dim tvw As TreeView
  
On Error Resume Next
  
  Set tvw = tvwDKview(DK_CHAIN)
  If Clear Then tvw.Nodes.Clear
  tblDecay.Seek "=", id
  If tblDecay.NoMatch Then Exit Sub
  
  If Clear Then
    FindSynonyms 0, id, ""
    Set nodx = tvw.Nodes.Add(, , cas & id, tblChems!name)
    nodx.tag = SetNodeTag(0, id, tblChems!name)
    nodx.Expanded = True
  End If
  SelectDecayChain False, txtCasid.text, cas & id
End Sub

Private Sub InitDataSpreadsheet()
Dim qry As String
Dim str As String
Dim lst As String
Dim pos As Long
Dim r As Long
Dim c As Long
Dim wid As Long
Dim lastCat As Long
Dim rs As Recordset
Dim rsClass As Recordset

  vaSpread1.ReDraw = False
  vaSpread1.row = -1
  vaSpread1.col = -1
  vaSpread1.Action = 3

  qry = "SELECT DISTINCT category.*, param.* " & _
        "FROM category LEFT JOIN param ON category.category = param.category " & _
        "ORDER BY category.category, param.index"
  Set rs = DB.OpenRecordset(qry, dbOpenDynaset)
  
  rs.MoveFirst
  vaSpread1.MaxRows = rs.RecordCount + cboPropCat.ListCount
  vaSpread1.MaxCols = rs.Fields.Count + 5
  vaSpread1.row = -1
  
  For c = 0 To 4
    vaSpread1.col = c
    Select Case c
      Case 0: wid = 9: vaSpread1.TypeHAlign = 0
      Case 1: wid = 33: vaSpread1.CellType = 5  ' static text
      Case 2: wid = 12: vaSpread1.CellType = 5  ' static text
      Case 3: wid = 9:: vaSpread1.CellType = 5  ' static text
      Case 4: wid = 6: vaSpread1.CellType = 5  ' static text
    End Select
    vaSpread1.ColWidth(c) = wid
  Next
  r = 0
  lastCat = -1
  
  While Not rs.EOF
    If rs.Fields("Category.Category") <> 1 Then
      vaSpread1.row = r
      If r = 0 Then
        vaSpread1.SetText 0, r, "Name"
        vaSpread1.SetText 1, r, "Description"
        vaSpread1.SetText 2, r, "Units"
        vaSpread1.SetText 3, r, "Value"
        vaSpread1.SetText 4, r, "Ref"
        For c = 1 To rs.Fields.Count
          vaSpread1.SetText c + 4, r, rs.Fields(c - 1).name
          If "Index" = rs.Fields(c - 1).name Then
            idCol = c + 4
          End If
        Next
        r = r + 1
      End If
      If r > 0 Then
        If Not rs.Fields("Category.Category") = lastCat Then ' write header
          vaSpread1.SetText 0, r, " "
          vaSpread1.row = r
          vaSpread1.Row2 = r
          vaSpread1.col = 0
          vaSpread1.Col2 = vaSpread1.MaxCols
          vaSpread1.BlockMode = True
          vaSpread1.CellType = 5 ' static text
          vaSpread1.BackColor = lightYellow
          vaSpread1.CellBorderType = 16 ' outline
          vaSpread1.CellBorderStyle = 11 ' solid line
          vaSpread1.CellBorderColor = vbBlack
          vaSpread1.Action = 16 ' set border
          vaSpread1.BlockMode = False
          vaSpread1.col = 1
          vaSpread1.TypeHAlign = 2 ' center
          vaSpread1.SetText 1, r, rs!Description
          lastCat = rs.Fields("Category.Category")
          r = r + 1
        End If
        vaSpread1.row = r
        vaSpread1.SetRowItemData r, lastCat
        vaSpread1.SetText 0, r, rs!sName
        vaSpread1.SetText 1, r, rs!Desc
        vaSpread1.SetText 2, r, ""
      
        Select Case rs!sName
          Case "FSCASID"
          Case Else:
            vaSpread1.col = 3
            Select Case rs!Type
              Case "list"
                vaSpread1.CellType = 8 ' combobox
                vaSpread1.TypeComboBoxEditable = False
                If Len(rs!Max) = 0 Then
                  str = rs.Fields!Min
                  lst = ""
                  Do While Len(str) > 0
                    pos = InStr(str, ",")
                    If Len(lst) > 0 Then lst = lst & Chr$(9)
                    If pos > 0 Then
                      lst = lst & Left$(str, pos - 1)
                      str = Mid$(str, pos + 1)
                    Else
                      lst = lst & str
                      str = ""
                    End If
                  Loop
                  vaSpread1.TypeComboBoxList = lst
                Else
                  qry = "SELECT " & rs!sName & ", desc FROM Classification WHERE " & rs!sName & ">0 ORDER BY " & rs!sName
                  Set rsClass = DB.OpenRecordset(qry, dbOpenDynaset)
                  lst = ""
                  If InStr(CLPTYPE & CLETYPE & CLRTYPE & CLCTYPE, rs!sName) Then lst = "Not Assigned"
                  rsClass.MoveFirst
                  While Not rsClass.EOF
                    If Len(lst) > 0 Then lst = lst & Chr$(9)
                    lst = lst & rsClass!Desc
                    rsClass.MoveNext
                  Wend
                  vaSpread1.TypeComboBoxList = lst
                End If
              Case Else
                vaSpread1.CellType = 1 ' edit
            End Select
        End Select
        For c = 1 To rs.Fields.Count
          vaSpread1.SetText c + 4, r, CVar(rs.Fields(c - 1))
        Next
      End If
      r = r + 1
    End If
    rs.MoveNext
  Wend
  vaSpread1.MaxRows = r - 1
  vaSpread1.ReDraw = True
  rs.Close
End Sub

Private Sub ResetDatabaseObjects()
Dim i As Long
Dim lb As Long
Dim ub As Long
Dim tblData As Recordset

On Error Resume Next

  If Not ws Is Nothing Then ws.Rollback
  
  lb = LBound(coldata)
  ub = UBound(coldata)
  For i = lb To ub
    Set tblData = coldata(i)
    If Not tblData Is Nothing Then tblData.Close
  Next
  If Not tblChems Is Nothing Then tblChems.Close
  If Not tblDecay Is Nothing Then tblDecay.Close
  If Not tblParam Is Nothing Then tblParam.Close
  If Not tblRef Is Nothing Then tblRef.Close
  If Not tblCat Is Nothing Then tblCat.Close
  If Not DB Is Nothing Then DB.Close
  
  Set tblChems = Nothing
  Set tblDecay = Nothing
  Set tblParam = Nothing
  Set tblRef = Nothing
  Set tblCat = Nothing
  Set DB = Nothing
  
  If Len(DBNameBackup) > 0 And Len(Dir$(DBNameBackup)) > 0 Then
    Kill DBNameBackup
  End If
  
  Unload frmReference
  cboClass.Clear
  cboClass.tag = ""
  Combo0.Clear
  Combo1.Clear
  Combo2.Clear
  Combo3.Clear
  cboPropCat.Clear
  lstAllName.Clear
  lstAllCasid.Clear
  lstCon.Clear
  lstDeg.Clear
  lstCon.tag = ""
  txtCasid = ""
  txtName = ""
  txtSyns = ""
  Text1 = ""
  DBName = ""
  DBNameBackup = ""
  RefIdx = -1
  StatusBar1.Panels(1) = ""
  lblCount.Caption = "(" & 0 & ")"
  SSTab1.Tab = IIf(SSTab1.TabVisible(0), 0, 1)
  SSTab2.Tab = IIf(SSTab2.TabVisible(0), 0, 1)
  HelpAnchor = "Top"
End Sub

Public Sub set_refrec()
  vaSpread1.SetFocus
  If Not RefCasId = PropertyCasid Then
    Beep
    Exit Sub
  End If
  vaSpread1.SetText 4, vaSpread1.ActiveRow, CStr(RefIdx)
  vaspread1_EditChange 3, vaSpread1.ActiveRow
End Sub

Public Sub set_tablevalue()
  vaSpread1.TypeComboBoxList = PropertyValue & Chr$(9) & "Select..."
  vaSpread1.TypeComboBoxCurSel = 0
  vaSpread1.SetText 4, vaSpread1.ActiveRow, PropertyRef
  RefIdx = PropertyRef
End Sub

Private Sub vaspread1_EditChange(ByVal col As Long, ByVal row As Long)
Dim pName As Variant
Dim pval As Variant
Dim pref As Variant
  
  vaSpread1.EditModeReplace = True
  vaSpread1.row = row
  vaSpread1.GetText 0, row, pName
  vaSpread1.GetText 3, row, pval
  pval = Trim$(pval)
  vaSpread1.GetText 4, row, pref
  If pval = "" Then vaSpread1.SetText 4, row, ""
  
  Select Case col
  Case 3
    If (InStr(pval, "Select")) Then
      PropertyParam = pName
      pval = vaSpread1.TypeComboBoxList
      'popup table of values to choose from
      frmPropWiz.get_tablevalue
      
      vaSpread1.GetText 3, row, pval
      vaSpread1.GetText 4, row, pref
    Else
      PropertyValue = pval
      PropertyRef = val(pref)
    End If
      If pval <> "" And pref = "" Then
        pref = 0
        vaSpread1.SetText 4, row, 0
      End If
      vaSpread1.col = 3
      If pval = "" Then
        pref = 0
        vaSpread1.BackColor = vbWhite
      Else
        vaSpread1.BackColor = lightRed
      End If
      If ValidInput(pName, pval) Then
        SetParamValue CStr(pName), txtCasid.text, pval, pref
        vaSpread1.BackColor = lightGreen
      End If
  End Select
  vaSpread1.EditModeReplace = False
End Sub

Public Sub SaveAnchor(anchor As String)
  myAnchor = anchor
End Sub

Public Sub RestoreAnchor()
  HelpAnchor = myAnchor
End Sub

Private Sub FillConstituentIdInfo()
Dim idKTYPE As Long
Dim idCTYPE As Long
Dim idPTYPE As Long
Dim idETYPE As Long
Dim idRTYPE As Long
Dim rowid As Long
Dim syn As String
Dim tag As String

  rowid = txtCasid.tag ' .itemdata(txtName.ListIndex)
  FindSynonyms 0, txtCasid, "" ' find all Chemicals for this casid
  If Not tblChems.NoMatch Then
    If IsNull(tblChems!CLKTYPE) Then idKTYPE = 0 Else idKTYPE = tblChems!CLKTYPE
    If IsNull(tblChems!CLCHEM) Then idCTYPE = 0 Else idCTYPE = tblChems!CLCHEM
    If IsNull(tblChems!CLPTYPE) Then idPTYPE = 0 Else idPTYPE = tblChems!CLPTYPE
    If IsNull(tblChems!CLETYPE) Then idETYPE = 0 Else idETYPE = tblChems!CLETYPE
    If IsNull(tblChems!CLRTYPE) Then idRTYPE = 0 Else idRTYPE = tblChems!CLRTYPE
    Do While Not tblChems.EOF
      If tblChems!CASID = txtCasid Then
        If tblChems!name <> txtName Then
          ' this is not the primary Name
          If Len(syn) > 0 Then syn = syn & vbCrLf
          syn = syn & tblChems!name
          If Len(tag) > 0 Then tag = tag & ","
          tag = tag & tblChems!rowid & "," & tblChems!name
        End If
      Else
        Exit Do
      End If
      tblChems.MoveNext
    Loop
  End If
  
  If chkRad.value <> idKTYPE Then
    SetAllContsList KTYPE, idKTYPE, lstDeg, Null
  End If
  
  txtSyns.text = syn
  txtSyns.tag = tag
  chkRad.value = idKTYPE
  SetListindex Combo0, idCTYPE
  SetListindex Combo1, idPTYPE
  SetListindex Combo2, idETYPE
  SetListindex Combo3, idRTYPE
  
  checking = True
  GetBranching
  ResetChainDecayView
  CheckBranching
  checking = False
  txtName.BackColor = vbWhite
  txtSyns.BackColor = vbWhite
  Combo0.BackColor = vbWhite
  Combo1.BackColor = vbWhite
  Combo2.BackColor = vbWhite
  Combo3.BackColor = vbWhite
End Sub

Private Sub SetListindex(cbo As ComboBox, id As Long)
Dim i As Long

  If id > -1 Then
    For i = 0 To cbo.ListCount - 1
      If cbo.itemdata(i) = id Then
        cbo.ListIndex = i
        Exit For
      End If
    Next
  End If
End Sub

Private Sub UpdatePropertiesPage()
Dim r As Long
Dim cat As Long
Dim tref As Long
Dim cltype As Long
Dim tval As String
Dim id As Variant
Dim var As Variant
Dim sName As String
Dim lst As String
Dim csv
Dim i As Long

  HelpAnchor = "Properties"
  If lstCon.ListCount = 0 Then Exit Sub
  If lstCon.ListIndex < 0 Then Exit Sub
  
  FindSynonyms lstCon.itemdata(lstCon.ListIndex), "", ""
  RefCasId = tblChems!CASID
  PropertyCasid = tblChems!CASID
  cltype = tblChems!CLKTYPE
  vaSpread1.ReDraw = False
  
  'unlock values and references
  vaSpread1.col = 3
  vaSpread1.Col2 = 4
  vaSpread1.row = -1
  vaSpread1.BlockMode = True
  vaSpread1.Lock = False
  vaSpread1.BlockMode = False

  For r = 1 To vaSpread1.MaxRows
    vaSpread1.row = r
    vaSpread1.GetText 0, r, var
    sName = var
    vaSpread1.GetText idCol, r, id
    cat = vaSpread1.GetRowItemData(r)
    vaSpread1.RowHidden = False
    If Len(id) > 0 And Len(sName) > 0 Then
      'set units for constituent type
      vaSpread1.SetText 2, r, ""
      tblParam.Seek "=", sName
      If Not tblParam.NoMatch Then
        If cltype = 0 Then
          If Not IsNull(tblParam!ChemUnits) Then
            vaSpread1.SetText 2, r, tblParam!ChemUnits
            If tblParam!ChemUnits = "Not Defined" Then vaSpread1.RowHidden = True
          End If
        Else
          If Not IsNull(tblParam!RadUnits) Then
            vaSpread1.SetText 2, r, tblParam!RadUnits
            If tblParam!RadUnits = "Not Defined" Then vaSpread1.RowHidden = True
          End If
        End If
      End If
      
      'clear value and reference
      vaSpread1.SetText 3, r, ""
      vaSpread1.SetCellDirtyFlag 3, r, False
      vaSpread1.SetText 4, r, ""
      vaSpread1.SetCellDirtyFlag 4, r, False
      vaSpread1.col = 3
      vaSpread1.BackColor = vbWhite
      
      'set value and reference
      If Not vaSpread1.RowHidden Then
        tval = GetParamValue(sName, PropertyCasid, tref)
        ' provide for multi-value (attributes) properties here
        vaSpread1.col = 3
        If PropertyHasCriteria(PropertyCasid, sName) Then
          vaSpread1.CellType = 8
          vaSpread1.TypeComboBoxList = tval & Chr$(9) & "Select..."
          vaSpread1.TypeComboBoxCurSel = 0
          vaSpread1.TypeComboBoxEditable = True
          If Len(tval) > 0 Then vaSpread1.SetText 4, r, tref
        ElseIf Len(tval) > 0 Then
          If InStr(CLPTYPE & CLETYPE & CLRTYPE & CLCTYPE, sName) Then
            vaSpread1.Lock = False
            vaSpread1.TypeComboBoxCurSel = tval
            vaSpread1.TypeComboBoxEditable = False
            vaSpread1.Lock = True
            vaSpread1.SetText 2, r, "NA"
          ElseIf InStr(CLKTYPE & "Name", Trim$(sName)) Then
            vaSpread1.Lock = False
            vaSpread1.value = tval
            vaSpread1.Lock = True
            vaSpread1.SetText 2, r, "NA"
          ElseIf vaSpread1.CellType = 8 Then
            lst = vaSpread1.TypeComboBoxList
            csv = Split(lst, vbTab)
            For i = 0 To UBound(csv)
              If tval = csv(i) Then vaSpread1.TypeComboBoxCurSel = i
            Next
            vaSpread1.SetText 4, r, tref
          Else
            vaSpread1.value = tval
            vaSpread1.SetText 4, r, tref
          End If
        End If
        If ValidInput(sName, tval) Then
          vaSpread1.BackColor = lightGreen
        Else
          If Len(tval) > 0 Then vaSpread1.BackColor = lightRed
        End If
      End If
    End If
  Next
  vaSpread1.ReDraw = True
  
  If Not PROP_EDIT Then
    vaSpread1.col = 3
    vaSpread1.Col2 = 4
    vaSpread1.row = -1
    vaSpread1.BlockMode = True
    vaSpread1.Lock = True
    vaSpread1.BlockMode = False
  End If
  cboPropCat_Click
  vaSpread1.Enabled = True
End Sub

Private Sub ResetNameListsFromAllConts(lstSrc As ListBox, lstDest As ListBox)
Dim i As Long

  lstDest.Clear
  For i = 0 To lstSrc.ListCount - 1 ' blh
    lstDest.AddItem lstSrc.list(i)
    lstDest.itemdata(lstDest.NewIndex) = lstSrc.itemdata(i)
  Next
End Sub

Private Sub ResetNameListsFromSelConts(tvw As TreeView, Optional showcas As Boolean = False)
Dim nodx As Node
Dim rowid As Long
Dim id As String
Dim name As String
  
  lstCon.Clear
  If tvw.Nodes.Count = 0 Then Exit Sub
  Set nodx = tvw.Nodes(1)
  Set nodx = nodx.root
  Do While Not nodx Is Nothing
    SplitNodeTag nodx.tag, rowid, id, name
    If showcas Then
      lstCon.AddItem id
      lstCon.itemdata(lstCon.NewIndex) = rowid
    Else
      lstCon.AddItem name
      lstCon.itemdata(lstCon.NewIndex) = rowid
    End If
    Set nodx = nodx.next
  Loop
End Sub

Private Sub ResetSelConts()
Dim i As Long
Dim nodx As Node
Dim rowid As Long
Dim id As String
Dim name As String
Dim col As Collection
  
  Set col = New Collection
  If tvwSelConts.Nodes.Count = 0 Then Exit Sub
  Set nodx = tvwSelConts.Nodes(1)
  Set nodx = nodx.root
  Do While Not nodx Is Nothing
    If Len(nodx.tag) > 0 Then col.Add nodx.tag
    Set nodx = nodx.next
  Loop
  tvwSelConts.Nodes.Clear
  For i = 1 To col.Count
    SplitNodeTag col.item(i), rowid, id, name
    AddCont name, id, rowid
  Next
End Sub

Private Sub SelectDecayChain(complete As Boolean, id As String, Optional key As String = "")
Dim ref As Long
Dim rowid As Long
Dim name As String
Dim newkey As String
Dim casid1 As String
Dim casid2 As String
Dim branch1 As Boolean
Dim branch2 As Boolean
Dim nodx As Node
Dim tvw As TreeView

  Set tvw = tvwDKview(DK_CHAIN)
  tblDecay.Seek "=", id
  If tblDecay.NoMatch Then Exit Sub

On Error Resume Next
  
  If Not IsNull(tblDecay!branch1) Then casid1 = Trim$(tblDecay!branch1)
  If Not IsNull(tblDecay!branch2) Then casid2 = Trim$(tblDecay!branch2)
  If Len(casid1) = 0 Then Exit Sub
  branch1 = True
  branch2 = (Len(casid2) > 0)
  
  newkey = key
  name = GetParamValue("Name", casid1, ref)
  If Len(name) = 0 Then
    name = casid1
    FindSynonyms 0, id, ""
  Else
    FindSynonyms 0, "", name
  End If
  rowid = tblChems!rowid
  Set nodx = tvw.Nodes.Add(key, tvwChild, newkey & casid1, IIf(tvw.tag = CASID, casid1, name))
  If Err.Number = 0 Then
    nodx.Expanded = True
    nodx.tag = SetNodeTag(rowid, casid1, name)
  End If
  If complete Then
    Set nodx = tvw.Nodes(cas & casid1)
    If Err.Number <> 0 Then
      Err.Clear
      Set nodx = tvw.Nodes.Add(, , cas & casid1, IIf(tvw.tag = CASID, casid1, name))
      nodx.Expanded = True
      nodx.tag = SetNodeTag(rowid, casid1, name)
      SelectDecayChain complete, casid1, newkey & casid1
    Else
      ' already on the tree
    End If
  End If
  Err.Clear
  SelectDecayChain complete, casid1, newkey & casid1
  
  If Not branch2 Then Exit Sub
  name = GetParamValue("Name", casid2, ref)
  If Len(name) = 0 Then
    name = casid2
    FindSynonyms 0, id, ""
  Else
    FindSynonyms 0, "", name
  End If
  rowid = tblChems!rowid
  newkey = key
  Set nodx = tvw.Nodes.Add(key, tvwChild, newkey & casid2, IIf(tvw.tag = CASID, casid2, name))
  If Err.Number = 0 Then
    nodx.Expanded = True
    nodx.tag = SetNodeTag(rowid, casid2, name)
  End If
  If complete Then
    nodx.Expanded = True
    Set nodx = tvw.Nodes(cas & casid2)
    If Err.Number <> 0 Then
      Err.Clear
      Set nodx = tvw.Nodes.Add(, , cas & casid2, IIf(tvw.tag = CASID, casid2, name))
      nodx.tag = SetNodeTag(0, casid2, name)
      nodx.Expanded = True
      SelectDecayChain complete, casid2, newkey & casid2
    End If
  End If
  Err.Clear
  If 0 = InStr(key, casid2) Then
    SelectDecayChain complete, casid2, newkey & casid2
  End If
End Sub

Private Function RefreshDatabase(fileName As String) As Boolean
Dim qry As String
Dim var As Variant
Dim fld As Field
Dim tdf As TableDef
Dim tbl As Recordset
Dim rs As Recordset
Dim rscat As Recordset
Dim rsparam As Recordset
Dim col As Collection

On Error Resume Next
  
  Set DB = ws.OpenDatabase(fileName, False, False)
  StatusBar1.Panels(1) = "Refreshing Database..."
  ws.BeginTrans
          
  ' add params missing from tables
  Set rscat = DB.OpenRecordset("SELECT * FROM category ORDER BY category")
  rscat.MoveFirst
  Do While Not rscat.EOF
    Set tdf = DB.TableDefs(rscat!TableName)
    Set rsparam = DB.OpenRecordset("SELECT * FROM param WHERE category =" & rscat!Category & " ORDER BY sName")
    If Not (rsparam.EOF And rsparam.BOF) Then
      rsparam.MoveFirst
      Do While Not rsparam.EOF
        Err.Clear
        Set fld = tdf.Fields(rsparam!sName)
        If Err.Number <> 0 Then
          Set fld = tdf.CreateField(rsparam!sName, dbText)
          fld.AllowZeroLength = True
          tdf.Fields.Append fld
          Set fld = tdf.CreateField("Ref" & rsparam!sName, dbInteger)
          tdf.Fields.Append fld
        End If
        rsparam.MoveNext
      Loop
    End If
    rscat.MoveNext
    DB.TableDefs.Refresh
  Loop
  
  ' for parameters with reassigned categories, move the data from old to new table
  Set rscat = DB.OpenRecordset("SELECT * FROM category ORDER BY category")
  rscat.MoveFirst
  Do While Not rscat.EOF
    Set tdf = DB.TableDefs(rscat!TableName)
    For Each fld In tdf.Fields
      If Not (fld.name = CASID Or Left$(fld.name, 3) = "Ref") Then
        Set rsparam = DB.OpenRecordset("SELECT * FROM param WHERE sName='" & fld.name & "'")
        If rsparam.EOF And rsparam.BOF Then
          ' this parameter has been removed from param table
        Else
          If rsparam!Category <> rscat!Category Then
            ' frmCategory has changed --> table has changed
            Set rs = DB.OpenRecordset("SELECT TableName FROM Category WHERE Category=" & rsparam!Category)
            Set tbl = DB.OpenRecordset(rs.Fields(0), dbOpenTable)
            tbl.index = "Primary"
            qry = "SELECT CASID, " & fld.name & ", Ref" & fld.name & " FROM " & tdf.name
            Set rs = DB.OpenRecordset(qry)
            If Not (rs.EOF And rs.BOF) Then
              rs.MoveFirst
              Do While Not rs.EOF
                tbl.Seek "=", rs.Fields(0).value
                If tbl.NoMatch Then
                  tbl.AddNew
                  tbl!CASID = rs!CASID
                Else
                  tbl.Edit
                End If
                tbl.Fields(fld.name) = rs.Fields(1).value
                tbl.Fields("Ref" & fld.name) = rs.Fields(2).value
                tbl.update
                rs.MoveNext
              Loop
            End If
          End If
        End If
      End If
    Next
    rscat.MoveNext
    DB.TableDefs.Refresh
  Loop
  
  ' now remove reassigned fields
  Set rscat = DB.OpenRecordset("SELECT * FROM category ORDER BY category")
  rscat.MoveFirst
  Do While Not rscat.EOF
    Set tdf = DB.TableDefs(rscat!TableName)
    Set col = New Collection
    For Each fld In tdf.Fields
      If Not (fld.name = CASID Or Left$(fld.name, 3) = "Ref") Then
        Set rsparam = DB.OpenRecordset("SELECT * FROM param WHERE sName='" & fld.name & "'")
        If rsparam.EOF And rsparam.BOF Then
          ' this parameter has been removed from param table
        Else
          If rsparam!Category <> rscat!Category Then
            Err.Clear
            col.Add fld.name
            ' frmCategory has changed --> table has changed
          End If
        End If
      End If
    Next
    For Each var In col
      tdf.Fields.Delete var
      tdf.Fields.Delete "Ref" & var
    Next
    rscat.MoveNext
    DB.TableDefs.Refresh
    Set col = Nothing
  Loop
  
  ' now remove extra data from tables
  Set rscat = DB.OpenRecordset("SELECT * FROM category ORDER BY category")
  Set rs = DB.OpenRecordset("Chemicals", dbOpenTable)
  rs.index = "Primary"
  rscat.MoveFirst
  Do While Not rscat.EOF
    qry = rscat!TableName
    If rscat!TableName = "Chemicals" Then qry = "Branching"
    Set rsparam = DB.OpenRecordset(qry, dbOpenTable)
    rsparam.MoveFirst
    Do While Not rsparam.EOF
      rs.Seek "=", Trim(rsparam!CASID)
      If rs.NoMatch Then rsparam.Delete
      rsparam.MoveNext
    Loop
    rscat.MoveNext
  Loop
  
  ws.CommitTrans
  DB.Close
  RefreshDatabase = True
End Function

Public Sub LoadNewTable(catid As Long, TName As String)
Dim sql As String
Dim fName As String
Dim rName As String
Dim rsData As Recordset
Dim tblData As Recordset
Dim rsparam As Recordset

On Error GoTo ErrorHandler
  
  Set tblData = DB.OpenRecordset(TName, dbOpenTable)
  tblData.index = "Primary"

  sql = "SELECT * FROM Param WHERE category=" & catid
  Set rsparam = DB.OpenRecordset(sql, dbOpenDynaset)
  Do While Not rsparam.EOF
    If Not IsNull(rsparam!index) Then
      fName = rsparam!sName
      rName = "Ref" & fName
      sql = "SELECT * FROM [data] WHERE id=" & rsparam!index & " AND NOT [value] IS null"
      Set rsData = DB.OpenRecordset(sql, dbOpenDynaset)
      Do While Not rsData.EOF
        SetParamValue fName, rsData!CASID, rsData![value], rsData!ref
        rsData.MoveNext
      Loop
    End If
    rsparam.MoveNext
  Loop
  tblData.Close
  rsparam.Close
  rsData.Close
  
ErrorHandler:
  If Err.Number <> 0 Then
    Resume Next
  End If
End Sub

Private Function TransformDatabase(fileName As String) As Boolean
Dim i As Long
Dim lb As Long
Dim ub As Long
Dim catid As Long
Dim sql As String
Dim TName As String
Dim fld As Field
Dim idx As index
Dim rel As Relation
Dim tdf As TableDef
Dim rs As Recordset
Dim rscat As Recordset
Dim rsparam As Recordset
Dim tblDecay As Recordset

On Error Resume Next
  
  FileCopy fileName, fileName & ".backup"
  
  Set DB = ws.OpenDatabase(fileName, False, False)
  StatusBar1.Panels(1) = "Transforming Database..."

  '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  ' setup structure
  '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  'delete outdated tables
  DB.TableDefs.Delete "Param"
  DB.TableDefs.Delete "ContamClass"
  DB.TableDefs.Delete "Category"
  
  ' reName Synonyms to Chemicals
  Set tdf = DB.TableDefs("Synonyms")
  tdf.name = "Chemicals"
  DB.TableDefs.Refresh
  
  ' copy tables
  Set tdf = DB.CreateTableDef("tempParam")
  tdf.Connect = ";DATABASE=" & App.Path & TEMPLATE
  tdf.SourceTableName = "tempParam"
  DB.TableDefs.Append tdf
  DB.Execute "SELECT * INTO Param FROM tempParam"

  Set tdf = DB.CreateTableDef("tempClass")
  tdf.Connect = ";DATABASE=" & App.Path & TEMPLATE
  tdf.SourceTableName = "tempClass"
  DB.TableDefs.Append tdf
  DB.Execute "SELECT * INTO Classification FROM tempClass"
  
  Set tdf = DB.CreateTableDef("tempCategory")
  tdf.Connect = ";DATABASE=" & App.Path & TEMPLATE
  tdf.SourceTableName = "tempCategory"
  DB.TableDefs.Append tdf
  DB.Execute "SELECT * INTO Category FROM tempCategory"
  
  Err.Clear
  Set tdf = DB.TableDefs("Branching")
  If Err.Number <> 0 Then
    Err.Clear
    Set tdf = DB.CreateTableDef("tempBranching")
    tdf.Connect = ";DATABASE=" & App.Path & TEMPLATE
    tdf.SourceTableName = "tempBranching"
    DB.TableDefs.Append tdf
    DB.Execute "SELECT * INTO Branching FROM tempBranching WHERE casid=''"
    DB.TableDefs.Delete "tempBranching"
  Else
    DB.Execute "SELECT distinct casid FROM Chemicals"
  End If
  DB.TableDefs.Refresh
  
  ' restore Category indexes
  Set tdf = DB.TableDefs("Category")
  Set idx = tdf.CreateIndex("Primary")
  idx.Fields.Append idx.CreateField("Category")
  idx.Primary = True
  tdf.Indexes.Append idx
  
  ' restore Param indexes
  Set tdf = DB.TableDefs("Param")
  Set idx = tdf.CreateIndex("Primary")
  idx.Fields.Append idx.CreateField("SName")
  idx.Primary = True
  tdf.Indexes.Append idx
  Set idx = tdf.CreateIndex("Category")
  idx.Fields.Append idx.CreateField("Category")
  tdf.Indexes.Append idx
  
  ' restore Chemicals indexes
  Set tdf = DB.TableDefs("Chemicals")
  Set idx = tdf.CreateIndex("Primary")
  idx.Fields.Append idx.CreateField(CASID)
  tdf.Indexes.Append idx
  Set idx = tdf.CreateIndex("RowId")
  idx.Fields.Append idx.CreateField("RowId")
  tdf.Indexes.Append idx
  tdf.Indexes.Delete CASID
  tdf.Indexes.Refresh
  
  ' restore Reference indexes
  Set tdf = DB.TableDefs("Ref")
  Set idx = tdf.CreateIndex("SName")
  idx.Fields.Append idx.CreateField("SName")
  tdf.Indexes.Append idx
  DB.TableDefs.Refresh
  
  ' restore Branching indexes
  Set tdf = DB.TableDefs("Branching")
  Set idx = tdf.CreateIndex("Primary")
  idx.Fields.Append idx.CreateField(CASID)
  idx.Primary = True
  tdf.Indexes.Append idx
  DB.TableDefs.Refresh
  
  ' restore relationship between Category and Param
  Set rel = DB.CreateRelation("CategoryParams", "Category", "Param", dbRelationDeleteCascade)
  rel.Fields.Append rel.CreateField("Category")
  rel.Fields!Category.ForeignName = "Category"
  DB.Relations.Append rel
  DB.Relations.Refresh
  
  ' cleanup link tables
  DB.TableDefs.Delete "tempParam"
  DB.TableDefs.Delete "tempClass"
  DB.TableDefs.Delete "tempCategory"
  DB.TableDefs.Refresh
  
  '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  ' create and fix data tables
  '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  'needed for loading tables
  Set tblCat = DB.OpenRecordset("Category", dbOpenTable)
  tblCat.index = "Primary"
  Set tblParam = DB.OpenRecordset("Param", dbOpenTable)
  tblParam.index = "Primary"
  Set tblDecay = DB.OpenRecordset("Branching", dbOpenTable)
  tblDecay.index = "Primary"
  Set tblChems = DB.OpenRecordset("Branching", dbOpenTable)
  tblChems.index = "Primary"
  
  'fix chemicals table - formerly synonyms
  Err.Clear
  Set tdf = DB.TableDefs("Chemicals")
  Set fld = tdf!CLPTYPE
  If Err.Number <> 0 Then
    Err.Clear
    tdf.Fields.Append tdf.CreateField(CLPTYPE, dbInteger)
  End If
  
  Err.Clear
  Set fld = tdf!CLETYPE
  If Err.Number <> 0 Then
    Err.Clear
    tdf.Fields.Append tdf.CreateField(CLETYPE, dbInteger)
  End If
  DB.TableDefs.Refresh
  
  'fix CLPTYPE
  DB.Execute "UPDATE Chemicals SET CLPTYPE=2  WHERE CLKTYPE=1 AND LEFT([casid],1) IN ('F','I')"
  DB.Execute "UPDATE Chemicals SET CLPTYPE=3  WHERE CLKTYPE=1 AND LEFT([casid],1) IN ('K')"
  DB.Execute "UPDATE Chemicals SET CLPTYPE=4  WHERE CLKTYPE=1 AND LEFT([casid],1) IN ('B')"
  DB.Execute "UPDATE Chemicals SET CLPTYPE=5  WHERE CLKTYPE=1 AND LEFT([casid],1) IN ('W')"
  DB.Execute "UPDATE Chemicals SET CLPTYPE=6  WHERE CLKTYPE=1 AND LEFT([casid],1) IN ('Y')"
  DB.Execute "UPDATE Chemicals SET CLPTYPE=7  WHERE CLKTYPE=1 AND LEFT([casid],1) IN ('U')"
  DB.Execute "UPDATE Chemicals SET CLPTYPE=9  WHERE CLKTYPE=1 AND LEFT([casid],1) IN ('C','H','N','O','P','S')"
  DB.Execute "UPDATE Chemicals SET CLPTYPE=10 WHERE CLKTYPE=1 AND LEFT([casid],1) IN ('V')"
  DB.Execute "UPDATE Chemicals SET CLPTYPE=1  WHERE CLKTYPE=1 AND LEFT([casid],2) IN ('Ar','He','Kr','Ne','Rn','Xe')"
  DB.Execute "UPDATE Chemicals SET CLPTYPE=2  WHERE CLKTYPE=1 AND LEFT([casid],2) IN ('At','Br','Cl')"
  DB.Execute "UPDATE Chemicals SET CLPTYPE=3  WHERE CLKTYPE=1 AND LEFT([casid],2) IN ('Cs','Fr','K','Li','Na','Rb')"
  DB.Execute "UPDATE Chemicals SET CLPTYPE=4  WHERE CLKTYPE=1 AND LEFT([casid],2) IN ('As','Ge','Po','Sb','Se','Si','Te')"
  DB.Execute "UPDATE Chemicals SET CLPTYPE=5  WHERE CLKTYPE=1 AND LEFT([casid],2) IN ('Ag','Bi','Co','Cr','Cu','Fe','In','Mn','Mo','Ni','Pb','Pd','Re','Rh','Ru','Sn','Tc','Tl')"
  DB.Execute "UPDATE Chemicals SET CLPTYPE=6  WHERE CLKTYPE=1 AND LEFT([casid],2) IN ('Am','Cm','Dy','Er','Eu','Gd','Ho','La','Lu','Nb','Nd','Pm','Pr','Sm','Tb','Tm','Yb','Zr')"
  DB.Execute "UPDATE Chemicals SET CLPTYPE=7  WHERE CLKTYPE=1 AND LEFT([casid],2) IN ('Ac','Bk','Ce','Cf','Es','Fm','Md','No','Np','Pa','Pu','Th')"
  DB.Execute "UPDATE Chemicals SET CLPTYPE=8  WHERE CLKTYPE=1 AND LEFT([casid],2) IN ('Ba','Be','Ca','Mg','Ra','Sr')"
  DB.Execute "UPDATE Chemicals SET CLPTYPE=10 WHERE CLKTYPE=1 AND LEFT([casid],2) IN ('Au','Cd','Hf','Hg','Ir','Os','Pt','Sc','Ta','Ti','Zn')"
  DB.Execute "UPDATE Chemicals SET CLPTYPE=11 WHERE CLKTYPE=1 AND LEFT([casid],2) IN ('Al','Ga')"
  
  'fix CLETYPE
  For i = 1 To 5
    DB.Execute "UPDATE Chemicals SET CLETYPE=" & i & " WHERE CLKTYPE=" & i
    DB.Execute "UPDATE Chemicals, [Identity] SET Chemicals.CLETYPE = " & i & _
               " WHERE (((Chemicals.casid)=[Identity].[casid]) And ((Identity.CLETYPE)='" & i & "'))"
  Next
    
  'fix misc
  DB.Execute "UPDATE Chemicals SET CLKTYPE=0  WHERE CLKTYPE>1"
  DB.Execute "UPDATE Chemicals SET CLRTYPE=14 WHERE CLKTYPE=1"
  DB.Execute "UPDATE Chemicals SET CLCHEM=0   WHERE CLKTYPE=1"
  
  '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  ' insert user defined DECAY into new BRANCHING
  '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  Err.Clear
  Set tdf = DB.TableDefs("DKCHAIN")
  If Err.Number = 0 Then
    Set rs = DB.OpenRecordset("DKCHAIN", dbOpenTable)
    If Not rs.EOF Then
      rs.MoveFirst
      Do While Not rs.EOF
        tblDecay.Seek "=", rs!CASID
        tblChems.Seek "=", rs!CASID
        If tblDecay.NoMatch And Not tblChems.NoMatch Then
          tblDecay.AddNew
          tblDecay!CASID = rs!CASID
          tblDecay!branch1 = rs!d1
          tblDecay!fraction1 = 1#
          tblDecay.update
        End If
        rs.MoveNext
      Loop
    End If
    tblDecay.Close
    rs.Close
    Set rs = Nothing
  End If
  
  '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  ' copy records from DATA into category data tables
  '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  Err.Clear
  Set tdf = DB.TableDefs("Data")
  If Err.Number = 0 Then
    'clear zero entries
    DB.Execute "DELETE FROM [data] WHERE [value]='0' AND ref=0"
    'copy clktype to cletype
    DB.Execute "INSERT INTO [data] SELECT casid, 141 AS id, value, ref FROM [data] WHERE id=98" '
    'set clktype
    DB.Execute "UPDATE [data] SET [value]='0' WHERE id=98 AND [value] IN ('2','3','4','5')"
  
    Set rscat = DB.OpenRecordset("SELECT MIN(category),MAX(category) FROM category", dbOpenDynaset)
    ReDim coldata(val(rscat.Fields(0).value) To val(rscat.Fields(1).value))
    
    Set rscat = DB.OpenRecordset("SELECT * FROM Category ORDER BY Category", dbOpenDynaset)
    If Not rscat.EOF Then rscat.MoveFirst
    
    Do While Not rscat.EOF
      TName = rscat!TableName
      catid = rscat!Category
      Err.Clear
      Set tdf = DB.TableDefs(TName)
      If Err.Number <> 0 Then
        Err.Clear
        Set tdf = DB.CreateTableDef(TName)
        Set fld = tdf.CreateField(CASID, dbText, 20)
        fld.AllowZeroLength = False
        fld.Required = True
        tdf.Fields.Append fld
        sql = "SELECT * FROM Param WHERE category=" & catid
        Set rsparam = DB.OpenRecordset(sql, dbOpenDynaset)
        Do While Not rsparam.EOF
          Set fld = tdf.CreateField(rsparam!sName, dbText) ' , 20)
          fld.AllowZeroLength = True
          tdf.Fields.Append fld
          Set fld = tdf.CreateField("Ref" & rsparam!sName, dbInteger)
          tdf.Fields.Append fld
          rsparam.MoveNext
        Loop
        Set idx = tdf.CreateIndex("Primary")
        idx.Fields.Append idx.CreateField(CASID)
        idx.Primary = True
        tdf.Indexes.Append idx
        DB.TableDefs.Append tdf
        DB.TableDefs.Refresh
      End If
      Set coldata(catid) = DB.OpenRecordset(TName, dbOpenTable)
      coldata(catid).index = "Primary"
      If catid > 1 Then LoadNewTable catid, TName
      rscat.MoveNext
    Loop
    rscat.Close
    lb = LBound(coldata)
    ub = UBound(coldata)
    For i = lb To ub
      coldata(i).Close
    Next
  End If
  
  Err.Clear
  Set tdf = DB.TableDefs("Data")
  If Err.Number = 0 Then
    DB.TableDefs.Delete "DATA"
    DB.TableDefs.Delete "DKCHAIN"
    DB.TableDefs.Delete "KD_DATA"
    DB.TableDefs.Refresh
  End If
  
  Err.Clear
  Set tdf = DB.TableDefs("old DATA")
  If Err.Number = 0 Then
    DB.TableDefs.Delete "old DATA"
    DB.TableDefs.Delete "old DKCHAIN"
    DB.TableDefs.Delete "old Param"
    DB.TableDefs.Delete "KD_DATA"
    DB.TableDefs.Delete "Identity"
    DB.TableDefs.Delete "Contaminant"
    Set tdf = DB.TableDefs("PhysicalProperties")
    tdf.name = "Properties"
    Set tdf = DB.TableDefs("Partitioning")
    tdf.name = "Partition"
    DB.TableDefs.Refresh
  End If
  Err.Clear
  
  DB.Close

  If Err.Number <> 0 Then
    MsgBox Error, vbOKOnly, "TransformDatabase"
    PutError "TransformDatabase: " & Error
    FileCopy fileName & ".backup", fileName
    TransformDatabase = False
  Else
    TransformDatabase = True
  End If
  StatusBar1.Panels(1) = ""
End Function

Private Function GetParamTable(fldName As String) As Recordset

On Error GoTo ErrorHandler

  tblParam.Seek "=", fldName
  tblCat.Seek "=", tblParam!Category
  Set GetParamTable = coldata(tblCat!Category)

ErrorHandler:
  If Err.Number <> 0 Then
    MsgBox Error, vbOKOnly, "GetParamTable " & fldName
  End If
End Function

Public Function SetParamValue(fldName As String, id As String, value As Variant, ref As Variant) As Boolean
Dim tbl As Recordset

On Error GoTo ErrorHandler
  
  Set tbl = GetParamTable(fldName)
  tbl.Seek "=", id
  If tbl.NoMatch Then
    tbl.AddNew
    tbl!CASID = id
  Else
    tbl.Edit
  End If
  tbl.Fields(fldName) = value
  tbl.Fields("Ref" & fldName) = ref
  tbl.update

ErrorHandler:
  If Err.Number <> 0 Then
    MsgBox Error, vbOKOnly, "SetParamValue " & fldName & ":" & id
    SetParamValue = False
  End If
  SetParamValue = True
End Function

Public Function GetParamValue(fldName As String, id As String, ref As Long) As String
Dim var As Variant
Dim fld As Field
Dim tbl As Recordset

On Error GoTo ErrorHandler

  ref = 0
  GetParamValue = ""
  Set tbl = GetParamTable(fldName)
  tbl.Seek "=", id
  If Not tbl.NoMatch Then
    Set fld = tbl.Fields(fldName)
    If Not IsNull(tbl.Fields(fldName).value) Then
      var = tbl.Fields(fldName).value
      If Not IsNull(var) Then
        GetParamValue = var
        If tbl.name <> "Chemicals" Then ref = tbl.Fields("Ref" & fldName).value
      End If
    End If
  End If

ErrorHandler:
  If Err.Number <> 0 Then
    MsgBox Error, vbOKOnly, "GetParamValue " & fldName & ":" & id
  End If
End Function

Public Function SetNodeTag(rowid As Long, id As String, cName As String) As String
  SetNodeTag = rowid & "&&" & Trim$(id) & "&&" & Trim$(cName)
End Function

Public Sub SplitNodeTag(tag As String, rowid As Long, id As String, cName As String)
Dim scsv As Variant

  scsv = Split(tag, "&&")
  rowid = scsv(0)
  id = scsv(1)
  cName = scsv(2)
End Sub

Private Sub CheckBranching()
Dim casid1 As String
Dim casid2 As String
Dim update As Boolean
Dim branch1 As Boolean
Dim branch2 As Boolean

  If lstCon.ListIndex < 0 Then Exit Sub
  If cboBranch(0).ListCount = 0 Then Exit Sub
  If cboBranch(1).ListCount = 0 Then Exit Sub
  
  checking = True
  branch1 = cboBranch(0).ListIndex = 1
  branch2 = cboBranch(1).ListIndex = 1
  cmdBranch(0).Enabled = False
  cmdBranch(1).Enabled = False

  If Not branch1 Then
    cboBranch(0).tag = ""
    cboBranch(1).tag = ""
    branch2 = False
    ' no branches
    cboBranch(1).ListIndex = 0
  Else
    FindSynonyms 0, "", cboBranch(0).text
    cboBranch(0).tag = tblChems!CASID
    If Not IsNumeric(txtBranch(0).text) Then
      txtBranch(0).text = "1.00"
    End If
    If branch2 Then
      FindSynonyms 0, "", cboBranch(1).text
      cboBranch(1).tag = tblChems!CASID
    Else
      cboBranch(1).tag = ""
    End If
  End If

  txtBranch(0).text = IIf(branch1, txtBranch(0).text, "")
  txtBranch(1).text = IIf(branch2, txtBranch(1).text, "")
  txtBranch(0).Enabled = ID_EDIT And branch1
  txtBranch(1).Enabled = ID_EDIT And branch2
  cboBranch(0).Enabled = ID_EDIT And branch1
  cboBranch(1).Enabled = ID_EDIT And branch2

  If branch1 Then
    update = Not (txtBranch(0).BackColor = lightRed)
    If branch2 Then
      update = update And Not (txtBranch(1).BackColor = lightRed)
    Else
      txtBranch(1).BackColor = vbWhite
    End If
  Else
    txtBranch(0).BackColor = vbWhite
  End If

  If Not branch1 Then
    tblDecay.Seek "=", txtCasid.text
    If Not tblDecay.NoMatch Then
      tblDecay.Delete
    End If
  Else
    If (Not IgnoreChange) And update And ID_EDIT Then
      FindSynonyms 0, "", cboBranch(0).list(cboBranch(0).ListIndex)
      casid1 = tblChems!CASID
      casid1 = cboBranch(0).tag
      If branch2 Then
        FindSynonyms 0, "", cboBranch(1).list(cboBranch(1).ListIndex)
        casid2 = tblChems!CASID
        casid2 = cboBranch(1).tag
      End If
      tblDecay.Seek "=", txtCasid.text
      If tblDecay.NoMatch Then
        tblDecay.AddNew
        tblDecay!CASID = txtCasid.text
      Else
        tblDecay.Edit
      End If
      tblDecay!branch1 = casid1
      tblDecay!fraction1 = val(txtBranch(0).text)
      tblDecay!branch2 = ""
      tblDecay!fraction2 = 0
      If branch2 Then
        tblDecay!branch2 = casid2
        If Not IsNumeric(txtBranch(1).text) Then
          txtBranch(1).text = Format(CStr(1# - CSng(txtBranch(0).text)), ".00##")
        End If
        tblDecay!fraction2 = val(txtBranch(1).text)
      End If
      tblDecay.update
    End If
  End If
  checking = False
End Sub

Private Sub ResetChainDecayView()
  GetDecayStraight txtCasid.text, True
  GetDecayChain txtCasid.text, True
End Sub

Public Sub OpenDatabaseRecordsets(fileName As String)
Dim rs As Recordset

  ws.BeginTrans
  Set DB = ws.OpenDatabase(fileName, False, False)
  Set tblCat = DB.OpenRecordset("Category", dbOpenTable)
  tblCat.index = "Primary"
  Set tblParam = DB.OpenRecordset("Param", dbOpenTable)
  tblParam.index = "Primary"
  Set tblChems = DB.OpenRecordset("Chemicals", dbOpenTable)
  tblChems.index = "Primary"
  Set tblDecay = DB.OpenRecordset("Branching", dbOpenTable)
  tblDecay.index = "Primary"
  Set tblRef = DB.OpenRecordset("Ref", dbOpenDynaset)
  
  Set rs = DB.OpenRecordset("SELECT MIN(category),MAX(category) FROM category", dbOpenDynaset)
  ReDim coldata(val(rs.Fields(0).value) To val(rs.Fields(1).value))
  tblCat.MoveFirst
  Do While Not tblCat.EOF
    Set coldata(tblCat!Category) = DB.OpenRecordset(tblCat!TableName, dbOpenTable)
    coldata(tblCat!Category).index = "Primary"
    tblCat.MoveNext
  Loop
End Sub

Private Function PropertyHasCriteria(id As String, sName As String) As Boolean
Dim qry As String
Dim rsCrit As Recordset
Dim rsData As Recordset
  
On Error GoTo ErrorHandler
  
  qry = "SELECT * FROM " & CRITERIA_DEF & " WHERE SName='" & sName & "' ORDER BY Id"
  Set rsCrit = DB.OpenRecordset(qry, dbOpenDynaset)
  If rsCrit.EOF And rsCrit.BOF Then GoTo ErrorHandler
  
  Set rsData = DB.OpenRecordset(CRITERIA_DATA, dbOpenTable)
  rsData.index = CASID
  rsData.Seek "=", id
  
  If rsData.NoMatch Then GoTo ErrorHandler
  
  rsCrit.MoveFirst
  Do While Not rsCrit.EOF
    If Not IsNull(rsData.Fields(rsCrit!id)) Then
      PropertyHasCriteria = True
      GoTo ErrorHandler
    End If
    rsCrit.MoveNext
  Loop
  
ErrorHandler:
  Set rsCrit = Nothing
  Set rsData = Nothing
End Function

Private Sub ResetView()
Dim i As Long
Dim id As Variant
Dim lst As ListBox
  
  If Len(DBName) = 0 Then Exit Sub
  
  ID_EDIT = False ' initial condition
  chkViewSel = (0 = cboView.ListIndex)
  If chkShowCasid.value = 0 Then
    Set lst = lstAllName
  Else
    Set lst = lstAllCasid
  End If
  
  If chkViewSel Then
    ' selected
    ID_EDIT = True
    ResetNameListsFromSelConts tvwSelConts, chkShowCasid
    ResetNameListsFromAllConts lst, lstDeg
  Else
    ' all
    ID_EDIT = Not IN_FRAMES
    ResetNameListsFromAllConts lst, lstCon
    ResetNameListsFromAllConts lst, lstDeg
  End If
  
  IgnoreChange = True
  txtCasid = ""
  txtName = ""
  txtSyns = ""
  vaSpread1.ReDraw = False
  For i = 1 To vaSpread1.MaxRows
    vaSpread1.row = i
    If vaSpread1.GetText(idCol, i, id) Then
      vaSpread1.col = 3
      vaSpread1.BackColor = vbWhite
      vaSpread1.col = 4
      vaSpread1.BackColor = vbWhite
      vaSpread1.SetText 3, i, ""
      vaSpread1.SetText 4, i, ""
      vaSpread1.SetCellDirtyFlag 3, i, False
      vaSpread1.SetCellDirtyFlag 4, i, False
    End If
  Next
  vaSpread1.ReDraw = True
  Combo0.ListIndex = Combo0.ListCount - 1
  Combo1.ListIndex = Combo1.ListCount - 1
  Combo2.ListIndex = Combo2.ListCount - 1
  Combo3.ListIndex = Combo3.ListCount - 1
  IgnoreChange = False
  
  If lstCon.ListCount = 0 Then Exit Sub
  If lstCon.ListIndex < 0 Then
    lstCon.ListIndex = 0
  Else
    lstCon_click
  End If
End Sub
