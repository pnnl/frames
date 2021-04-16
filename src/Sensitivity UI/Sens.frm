VERSION 5.00
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "TABCTL32.OCX"
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "THREED32.OCX"
Object = "{6B7E6392-850A-101B-AFC0-4210102A8DA7}#1.3#0"; "COMCTL32.OCX"
Begin VB.Form Sens 
   AutoRedraw      =   -1  'True
   Caption         =   "SensitivityUI"
   ClientHeight    =   5940
   ClientLeft      =   8655
   ClientTop       =   7410
   ClientWidth     =   10995
   Icon            =   "Sens.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   396
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   733
   StartUpPosition =   2  'CenterScreen
   Begin ComctlLib.TreeView iotree 
      DragIcon        =   "Sens.frx":030A
      Height          =   5895
      Left            =   0
      TabIndex        =   92
      Top             =   0
      Width           =   3375
      _ExtentX        =   5953
      _ExtentY        =   10398
      _Version        =   327682
      HideSelection   =   0   'False
      Indentation     =   212
      LabelEdit       =   1
      Sorted          =   -1  'True
      Style           =   6
      Appearance      =   1
   End
   Begin Threed.SSPanel tPanel 
      Height          =   4575
      Left            =   4080
      TabIndex        =   49
      Top             =   240
      Visible         =   0   'False
      Width           =   6480
      _Version        =   65536
      _ExtentX        =   11430
      _ExtentY        =   8070
      _StockProps     =   15
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BevelWidth      =   4
      BorderWidth     =   2
      BevelInner      =   1
      Begin VB.TextBox txt 
         Alignment       =   1  'Right Justify
         Height          =   315
         Index           =   5
         Left            =   2805
         TabIndex        =   112
         Top             =   720
         Visible         =   0   'False
         Width           =   1000
      End
      Begin VB.ComboBox unt 
         Height          =   315
         Index           =   5
         Left            =   3795
         Style           =   2  'Dropdown List
         TabIndex        =   111
         Top             =   720
         Visible         =   0   'False
         Width           =   1200
      End
      Begin VB.ComboBox unt 
         Height          =   315
         Index           =   14
         Left            =   3795
         Style           =   2  'Dropdown List
         TabIndex        =   109
         Top             =   3960
         Visible         =   0   'False
         Width           =   1200
      End
      Begin VB.ComboBox unt 
         Height          =   315
         Index           =   13
         Left            =   3795
         Style           =   2  'Dropdown List
         TabIndex        =   106
         Top             =   3600
         Visible         =   0   'False
         Width           =   1200
      End
      Begin VB.ComboBox unt 
         Height          =   315
         Index           =   12
         Left            =   3795
         Style           =   2  'Dropdown List
         TabIndex        =   103
         Top             =   3240
         Visible         =   0   'False
         Width           =   1200
      End
      Begin VB.ComboBox unt 
         Height          =   315
         Index           =   11
         Left            =   3795
         Style           =   2  'Dropdown List
         TabIndex        =   100
         Top             =   2880
         Visible         =   0   'False
         Width           =   1200
      End
      Begin VB.CommandButton Command 
         Caption         =   "Ok"
         Height          =   348
         Index           =   2
         Left            =   5160
         TabIndex        =   37
         Top             =   300
         Width           =   1000
      End
      Begin VB.ComboBox yrCnt 
         Height          =   315
         ItemData        =   "Sens.frx":0614
         Left            =   3840
         List            =   "Sens.frx":0636
         Style           =   2  'Dropdown List
         TabIndex        =   38
         Top             =   300
         Visible         =   0   'False
         Width           =   1200
      End
      Begin VB.ComboBox unt 
         Height          =   315
         Index           =   9
         Left            =   3795
         Style           =   2  'Dropdown List
         TabIndex        =   46
         Top             =   2160
         Visible         =   0   'False
         Width           =   1200
      End
      Begin VB.ComboBox unt 
         Height          =   315
         Index           =   10
         Left            =   3795
         Style           =   2  'Dropdown List
         TabIndex        =   48
         Top             =   2520
         Visible         =   0   'False
         Width           =   1200
      End
      Begin VB.ComboBox unt 
         Height          =   315
         Index           =   7
         Left            =   3795
         Style           =   2  'Dropdown List
         TabIndex        =   42
         Top             =   1440
         Visible         =   0   'False
         Width           =   1200
      End
      Begin VB.ComboBox unt 
         Height          =   315
         Index           =   8
         Left            =   3795
         Style           =   2  'Dropdown List
         TabIndex        =   44
         Top             =   1800
         Visible         =   0   'False
         Width           =   1200
      End
      Begin VB.ComboBox unt 
         Height          =   315
         Index           =   6
         Left            =   3795
         Style           =   2  'Dropdown List
         TabIndex        =   40
         Top             =   1080
         Visible         =   0   'False
         Width           =   1200
      End
      Begin VB.TextBox txt 
         Alignment       =   1  'Right Justify
         Height          =   315
         Index           =   6
         Left            =   2805
         TabIndex        =   39
         Top             =   1080
         Visible         =   0   'False
         Width           =   1000
      End
      Begin VB.TextBox txt 
         Alignment       =   1  'Right Justify
         Height          =   315
         Index           =   8
         Left            =   2805
         TabIndex        =   43
         Top             =   1800
         Visible         =   0   'False
         Width           =   1000
      End
      Begin VB.TextBox txt 
         Alignment       =   1  'Right Justify
         Height          =   315
         Index           =   7
         Left            =   2805
         TabIndex        =   41
         Top             =   1440
         Visible         =   0   'False
         Width           =   1000
      End
      Begin VB.TextBox txt 
         Alignment       =   1  'Right Justify
         Height          =   315
         Index           =   10
         Left            =   2805
         TabIndex        =   47
         Top             =   2520
         Visible         =   0   'False
         Width           =   1000
      End
      Begin VB.TextBox txt 
         Alignment       =   1  'Right Justify
         Height          =   315
         Index           =   9
         Left            =   2805
         TabIndex        =   45
         Top             =   2160
         Visible         =   0   'False
         Width           =   1000
      End
      Begin VB.TextBox txt 
         Alignment       =   1  'Right Justify
         Height          =   315
         Index           =   11
         Left            =   2805
         TabIndex        =   99
         Top             =   2880
         Visible         =   0   'False
         Width           =   1000
      End
      Begin VB.TextBox txt 
         Alignment       =   1  'Right Justify
         Height          =   315
         Index           =   12
         Left            =   2805
         TabIndex        =   102
         Top             =   3240
         Visible         =   0   'False
         Width           =   1000
      End
      Begin VB.TextBox txt 
         Alignment       =   1  'Right Justify
         Height          =   315
         Index           =   13
         Left            =   2805
         TabIndex        =   105
         Top             =   3600
         Visible         =   0   'False
         Width           =   1000
      End
      Begin VB.TextBox txt 
         Alignment       =   1  'Right Justify
         Height          =   315
         Index           =   14
         Left            =   2805
         TabIndex        =   108
         Top             =   3960
         Visible         =   0   'False
         Width           =   1000
      End
      Begin VB.Label lbl 
         Caption         =   "Label"
         Height          =   315
         Index           =   5
         Left            =   360
         TabIndex        =   113
         Top             =   720
         Visible         =   0   'False
         Width           =   2460
      End
      Begin VB.Label lbl 
         Caption         =   "Label"
         Height          =   315
         Index           =   14
         Left            =   360
         TabIndex        =   110
         Top             =   3960
         Visible         =   0   'False
         Width           =   2460
      End
      Begin VB.Label lbl 
         Caption         =   "Label"
         Height          =   315
         Index           =   13
         Left            =   360
         TabIndex        =   107
         Top             =   3600
         Visible         =   0   'False
         Width           =   2460
      End
      Begin VB.Label lbl 
         Caption         =   "Label"
         Height          =   315
         Index           =   12
         Left            =   360
         TabIndex        =   104
         Top             =   3240
         Visible         =   0   'False
         Width           =   2460
      End
      Begin VB.Label lbl 
         Caption         =   "Label"
         Height          =   315
         Index           =   11
         Left            =   360
         TabIndex        =   101
         Top             =   2880
         Visible         =   0   'False
         Width           =   2460
      End
      Begin VB.Label lbl 
         Caption         =   "Label"
         Height          =   315
         Index           =   9
         Left            =   360
         TabIndex        =   55
         Top             =   2160
         Visible         =   0   'False
         Width           =   2460
      End
      Begin VB.Label lbl 
         Caption         =   "Label"
         Height          =   315
         Index           =   10
         Left            =   360
         TabIndex        =   54
         Top             =   2520
         Visible         =   0   'False
         Width           =   2460
      End
      Begin VB.Label lbl 
         Caption         =   "Label"
         Height          =   315
         Index           =   7
         Left            =   360
         TabIndex        =   53
         Top             =   1440
         Visible         =   0   'False
         Width           =   2460
      End
      Begin VB.Label lbl 
         Caption         =   "Label"
         Height          =   315
         Index           =   8
         Left            =   360
         TabIndex        =   52
         Top             =   1800
         Visible         =   0   'False
         Width           =   2460
      End
      Begin VB.Label lbl 
         Caption         =   "Number of years to monitor"
         Height          =   252
         Index           =   20
         Left            =   360
         TabIndex        =   51
         Top             =   240
         Width           =   2460
      End
      Begin VB.Label lbl 
         Caption         =   "Label"
         Height          =   315
         Index           =   6
         Left            =   360
         TabIndex        =   50
         Top             =   1080
         Visible         =   0   'False
         Width           =   2460
      End
   End
   Begin Threed.SSFrame uFrame 
      Height          =   5865
      Index           =   0
      Left            =   3480
      TabIndex        =   56
      Top             =   0
      Width           =   7500
      _Version        =   65536
      _ExtentX        =   13229
      _ExtentY        =   10350
      _StockProps     =   14
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
      Begin VB.ListBox List1 
         BeginProperty Font 
            Name            =   "Courier New"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   3660
         ItemData        =   "Sens.frx":0659
         Left            =   240
         List            =   "Sens.frx":065B
         TabIndex        =   36
         Tag             =   "True"
         Top             =   1970
         Width           =   7080
      End
      Begin VB.TextBox vTxt1 
         Height          =   300
         Left            =   1224
         MaxLength       =   9
         TabIndex        =   33
         Tag             =   "VarAlias"
         Top             =   800
         Width           =   2400
      End
      Begin VB.CommandButton Command 
         Caption         =   "Add"
         Enabled         =   0   'False
         Height          =   348
         Index           =   0
         Left            =   6240
         TabIndex        =   31
         Tag             =   "VarAdd"
         Top             =   360
         Width           =   1000
      End
      Begin VB.CommandButton Command 
         Caption         =   "Delete"
         Enabled         =   0   'False
         Height          =   348
         Index           =   1
         Left            =   6240
         TabIndex        =   32
         Tag             =   "VarDel"
         Top             =   720
         Width           =   1000
      End
      Begin VB.TextBox vTxt2 
         BackColor       =   &H00E0E0E0&
         Height          =   500
         Left            =   1224
         Locked          =   -1  'True
         MultiLine       =   -1  'True
         TabIndex        =   34
         Top             =   1140
         Width           =   6060
      End
      Begin Threed.SSCheck SSCheck1 
         Height          =   252
         Left            =   240
         TabIndex        =   35
         Tag             =   "VarView"
         Top             =   360
         Width           =   1680
         _Version        =   65536
         _ExtentX        =   2963
         _ExtentY        =   444
         _StockProps     =   78
         Caption         =   "View Aliased Inputs"
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
      End
      Begin VB.Label ref 
         Caption         =   "Ref:"
         Height          =   252
         Index           =   3
         Left            =   3720
         TabIndex        =   84
         Top             =   800
         Width           =   852
      End
      Begin VB.Label vLbl 
         Caption         =   "Alias"
         Height          =   252
         Index           =   0
         Left            =   240
         TabIndex        =   59
         Top             =   800
         Width           =   996
      End
      Begin VB.Label vLbl 
         Caption         =   "Alias                  Variable Description"
         Height          =   252
         Index           =   1
         Left            =   240
         TabIndex        =   58
         Top             =   1752
         Width           =   3336
      End
      Begin VB.Label Label2 
         Caption         =   "Description"
         Height          =   228
         Left            =   240
         TabIndex        =   57
         Top             =   1140
         Width           =   1020
      End
   End
   Begin Threed.SSFrame uFrame 
      Height          =   5868
      Index           =   2
      Left            =   3480
      TabIndex        =   79
      Top             =   0
      Width           =   7500
      _Version        =   65536
      _ExtentX        =   13229
      _ExtentY        =   10350
      _StockProps     =   14
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
      Begin VB.CommandButton Command 
         Caption         =   "Delete"
         Enabled         =   0   'False
         Height          =   348
         Index           =   9
         Left            =   6240
         TabIndex        =   24
         Tag             =   "OUTDEL"
         Top             =   720
         Width           =   1000
      End
      Begin VB.CommandButton Command 
         Caption         =   "Add"
         Enabled         =   0   'False
         Height          =   348
         Index           =   8
         Left            =   6240
         TabIndex        =   23
         Tag             =   "OUTADD"
         Top             =   360
         Width           =   1000
      End
      Begin VB.TextBox oTxt1 
         Height          =   300
         Left            =   1224
         MaxLength       =   9
         TabIndex        =   27
         Tag             =   "OUTALIAS"
         Top             =   800
         Width           =   2400
      End
      Begin VB.ListBox List4 
         BeginProperty Font 
            Name            =   "Courier New"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   3660
         ItemData        =   "Sens.frx":065D
         Left            =   240
         List            =   "Sens.frx":065F
         TabIndex        =   30
         Top             =   1968
         Width           =   7080
      End
      Begin VB.CommandButton Command 
         Caption         =   "Edit Time"
         Enabled         =   0   'False
         Height          =   348
         Index           =   10
         Left            =   4800
         TabIndex        =   25
         Tag             =   "OUTTIME"
         Top             =   360
         Width           =   1400
      End
      Begin VB.TextBox oTxt2 
         BackColor       =   &H00E0E0E0&
         Height          =   500
         Left            =   1224
         Locked          =   -1  'True
         MultiLine       =   -1  'True
         TabIndex        =   28
         Top             =   1140
         Width           =   6060
      End
      Begin VB.CommandButton Command 
         Caption         =   "Edit Location"
         Enabled         =   0   'False
         Height          =   348
         Index           =   3
         Left            =   4800
         TabIndex        =   26
         Tag             =   "OUTTIME"
         Top             =   720
         Width           =   1400
      End
      Begin Threed.SSCheck SSCheck2 
         Height          =   252
         Left            =   240
         TabIndex        =   29
         Tag             =   "OUTVIEW"
         Top             =   360
         Width           =   1800
         _Version        =   65536
         _ExtentX        =   3175
         _ExtentY        =   444
         _StockProps     =   78
         Caption         =   "View Aliased Outputs"
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
      End
      Begin VB.Label ref 
         Caption         =   "Ref:"
         Height          =   252
         Index           =   2
         Left            =   3720
         TabIndex        =   83
         Top             =   800
         Width           =   852
      End
      Begin VB.Label vLbl 
         Caption         =   "Alias                  Output Description"
         Height          =   252
         Index           =   10
         Left            =   240
         TabIndex        =   82
         Top             =   1752
         Width           =   3336
      End
      Begin VB.Label vLbl 
         Caption         =   "Alias"
         Height          =   252
         Index           =   11
         Left            =   240
         TabIndex        =   81
         Top             =   800
         Width           =   1032
      End
      Begin VB.Label Label3 
         Caption         =   "Description"
         Height          =   228
         Left            =   240
         TabIndex        =   80
         Top             =   1140
         Width           =   1020
      End
   End
   Begin Threed.SSFrame uFrame 
      Height          =   5865
      Index           =   1
      Left            =   3480
      TabIndex        =   60
      Top             =   0
      Width           =   7500
      _Version        =   65536
      _ExtentX        =   13229
      _ExtentY        =   10350
      _StockProps     =   14
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
      Begin VB.CheckBox Check2 
         Height          =   195
         Left            =   4320
         TabIndex        =   97
         Top             =   360
         Width           =   252
      End
      Begin VB.TextBox txt 
         Alignment       =   1  'Right Justify
         Enabled         =   0   'False
         Height          =   300
         Index           =   17
         Left            =   4440
         TabIndex        =   93
         Top             =   684
         Width           =   405
      End
      Begin VB.TextBox txt 
         Alignment       =   1  'Right Justify
         Height          =   300
         Index           =   16
         Left            =   2352
         TabIndex        =   1
         Top             =   680
         Width           =   1000
      End
      Begin VB.TextBox txt 
         Alignment       =   1  'Right Justify
         Height          =   300
         Index           =   15
         Left            =   2352
         TabIndex        =   0
         Top             =   360
         Width           =   1000
      End
      Begin VB.ListBox List2 
         Height          =   4155
         Left            =   240
         TabIndex        =   2
         Tag             =   "PARAM"
         Top             =   1320
         Width           =   2028
      End
      Begin VB.TextBox pTxt1 
         BackColor       =   &H00E0E0E0&
         Height          =   500
         Left            =   2490
         TabIndex        =   3
         Tag             =   "ALIAS"
         Top             =   1680
         Width           =   4692
      End
      Begin TabDlg.SSTab SSTab3 
         Height          =   4452
         Left            =   2340
         TabIndex        =   61
         Top             =   1320
         Width           =   5052
         _ExtentX        =   8916
         _ExtentY        =   7858
         _Version        =   393216
         TabHeight       =   420
         TabCaption(0)   =   "Distribution"
         TabPicture(0)   =   "Sens.frx":0661
         Tab(0).ControlEnabled=   -1  'True
         Tab(0).Control(0)=   "pFrame(0)"
         Tab(0).Control(0).Enabled=   0   'False
         Tab(0).ControlCount=   1
         TabCaption(1)   =   "Correlations"
         TabPicture(1)   =   "Sens.frx":067D
         Tab(1).ControlEnabled=   0   'False
         Tab(1).Control(0)=   "pFrame(1)"
         Tab(1).ControlCount=   1
         TabCaption(2)   =   "Equation"
         TabPicture(2)   =   "Sens.frx":0699
         Tab(2).ControlEnabled=   0   'False
         Tab(2).Control(0)=   "pFrame(2)"
         Tab(2).ControlCount=   1
         Begin Threed.SSFrame pFrame 
            Height          =   3492
            Index           =   0
            Left            =   144
            TabIndex        =   62
            Top             =   840
            Width           =   4764
            _Version        =   65536
            _ExtentX        =   8403
            _ExtentY        =   6159
            _StockProps     =   14
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   0
               Left            =   3600
               Style           =   2  'Dropdown List
               TabIndex        =   15
               Top             =   720
               Visible         =   0   'False
               Width           =   996
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   3
               Left            =   3600
               Style           =   2  'Dropdown List
               TabIndex        =   21
               Top             =   2016
               Visible         =   0   'False
               Width           =   996
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   2
               Left            =   3600
               Style           =   2  'Dropdown List
               TabIndex        =   19
               Top             =   1584
               Visible         =   0   'False
               Width           =   996
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   1
               Left            =   3600
               Style           =   2  'Dropdown List
               TabIndex        =   17
               Top             =   1152
               Visible         =   0   'False
               Width           =   996
            End
            Begin VB.ComboBox distrib 
               Height          =   288
               ItemData        =   "Sens.frx":06B5
               Left            =   1248
               List            =   "Sens.frx":06C8
               Style           =   2  'Dropdown List
               TabIndex        =   13
               Tag             =   "DIST"
               Top             =   300
               Width           =   2412
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               Height          =   300
               Index           =   0
               Left            =   2592
               TabIndex        =   14
               Top             =   720
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               Height          =   300
               Index           =   1
               Left            =   2592
               TabIndex        =   16
               Top             =   1152
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               Height          =   300
               Index           =   2
               Left            =   2592
               TabIndex        =   18
               Top             =   1584
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               Height          =   300
               Index           =   3
               Left            =   2592
               TabIndex        =   20
               Top             =   2016
               Width           =   1000
            End
            Begin VB.ComboBox base 
               Height          =   288
               ItemData        =   "Sens.frx":06FC
               Left            =   2592
               List            =   "Sens.frx":0706
               Style           =   2  'Dropdown List
               TabIndex        =   22
               Tag             =   "DISTBASE"
               Top             =   2496
               Width           =   2028
            End
            Begin VB.Label ref 
               Caption         =   "Ref:"
               Height          =   252
               Index           =   4
               Left            =   3720
               TabIndex        =   86
               Top             =   360
               Width           =   852
            End
            Begin VB.Label vLbl 
               Caption         =   "Type"
               Height          =   252
               Index           =   4
               Left            =   140
               TabIndex        =   68
               Top             =   288
               Width           =   1116
            End
            Begin VB.Label lbl 
               Caption         =   "Label"
               Height          =   252
               Index           =   0
               Left            =   252
               TabIndex        =   67
               Top             =   720
               Width           =   2004
            End
            Begin VB.Label lbl 
               Caption         =   "Label"
               Height          =   252
               Index           =   1
               Left            =   252
               TabIndex        =   66
               Top             =   1152
               Width           =   2004
            End
            Begin VB.Label lbl 
               Caption         =   "Label"
               Height          =   252
               Index           =   2
               Left            =   252
               TabIndex        =   65
               Top             =   1560
               Width           =   2004
            End
            Begin VB.Label lbl 
               Caption         =   "Label"
               Height          =   252
               Index           =   3
               Left            =   252
               TabIndex        =   64
               Top             =   2016
               Width           =   2004
            End
            Begin VB.Label lbl 
               Caption         =   "Log Base"
               Height          =   252
               Index           =   4
               Left            =   252
               TabIndex        =   63
               Top             =   2496
               Width           =   2004
            End
         End
         Begin Threed.SSFrame pFrame 
            Height          =   3492
            Index           =   1
            Left            =   -74856
            TabIndex        =   69
            Top             =   840
            Width           =   4764
            _Version        =   65536
            _ExtentX        =   8403
            _ExtentY        =   6159
            _StockProps     =   14
            Begin VB.TextBox pTxt2 
               Height          =   300
               Left            =   2300
               TabIndex        =   10
               Tag             =   "CORCOR"
               Top             =   1320
               Width           =   708
            End
            Begin VB.ListBox List3 
               Height          =   1035
               Left            =   140
               TabIndex        =   8
               Tag             =   "CORCURRENT"
               Top             =   528
               Width           =   1980
            End
            Begin VB.ComboBox alias 
               Height          =   288
               Left            =   2300
               Style           =   2  'Dropdown List
               TabIndex        =   9
               Tag             =   "CORALIAS"
               Top             =   648
               Width           =   2328
            End
            Begin VB.CommandButton Command 
               Caption         =   "Add"
               Height          =   348
               Index           =   4
               Left            =   3600
               TabIndex        =   11
               Tag             =   "CORADD"
               Top             =   1080
               Width           =   1000
            End
            Begin VB.CommandButton Command 
               Caption         =   "Delete"
               Height          =   348
               Index           =   5
               Left            =   3600
               TabIndex        =   12
               Tag             =   "CORDEL"
               Top             =   1440
               Width           =   1000
            End
            Begin VB.Label ref 
               Caption         =   "Ref:"
               Height          =   252
               Index           =   5
               Left            =   2300
               TabIndex        =   85
               Top             =   1680
               Width           =   612
            End
            Begin VB.Label vLbl 
               Caption         =   "Alias"
               Height          =   252
               Index           =   5
               Left            =   2300
               TabIndex        =   73
               Top             =   408
               Width           =   1260
            End
            Begin VB.Label vLbl 
               Caption         =   "Correlation"
               Height          =   252
               Index           =   7
               Left            =   2300
               TabIndex        =   72
               Top             =   1080
               Width           =   2172
            End
            Begin VB.Label vLbl 
               Caption         =   "Correlations"
               Height          =   252
               Index           =   6
               Left            =   140
               TabIndex        =   71
               Top             =   288
               Width           =   1020
            End
            Begin VB.Label Label1 
               Caption         =   $"Sens.frx":071B
               Height          =   1068
               Left            =   144
               TabIndex        =   70
               Top             =   2160
               Width           =   4308
            End
         End
         Begin Threed.SSFrame pFrame 
            Height          =   3492
            Index           =   2
            Left            =   -74856
            TabIndex        =   74
            Top             =   840
            Width           =   4764
            _Version        =   65536
            _ExtentX        =   8403
            _ExtentY        =   6159
            _StockProps     =   14
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Begin VB.TextBox pTxt3 
               Height          =   300
               Left            =   432
               TabIndex        =   5
               Tag             =   "EQUEQUATION"
               Top             =   1176
               Width           =   4116
            End
            Begin VB.CommandButton Command 
               Caption         =   "Add"
               Height          =   348
               Index           =   6
               Left            =   2280
               TabIndex        =   6
               Tag             =   "EQUADD"
               Top             =   3000
               Width           =   1000
            End
            Begin VB.CommandButton Command 
               Caption         =   "Delete"
               Height          =   348
               Index           =   7
               Left            =   3564
               TabIndex        =   7
               Tag             =   "EQUDEL"
               Top             =   3000
               Width           =   1000
            End
            Begin VB.Label ref 
               Caption         =   "Ref:"
               Height          =   252
               Index           =   6
               Left            =   3720
               TabIndex        =   87
               Top             =   168
               Width           =   852
            End
            Begin VB.Label vLbl 
               Caption         =   "Enter an equation"
               Height          =   252
               Index           =   8
               Left            =   144
               TabIndex        =   77
               Top             =   888
               Width           =   4380
            End
            Begin VB.Label vLbl 
               Caption         =   $"Sens.frx":07EE
               Height          =   1440
               Index           =   9
               Left            =   144
               TabIndex        =   76
               Top             =   1560
               Width           =   4428
            End
            Begin VB.Label vLbl 
               Caption         =   "Current equation"
               Height          =   252
               Index           =   12
               Left            =   144
               TabIndex        =   75
               Top             =   168
               Width           =   3300
            End
            Begin VB.Label pTxt4 
               BackColor       =   &H00E0E0E0&
               BorderStyle     =   1  'Fixed Single
               Height          =   300
               Left            =   432
               TabIndex        =   4
               Tag             =   "EQUEQUATION"
               Top             =   480
               Width           =   4116
            End
         End
      End
      Begin VB.Label lbl 
         Caption         =   ":00 hrs"
         Height          =   252
         Index           =   18
         Left            =   4920
         TabIndex        =   96
         Tag             =   "OUTITERATIONS"
         Top             =   720
         Width           =   552
      End
      Begin VB.Label ref 
         Caption         =   "Ref:"
         Height          =   252
         Index           =   17
         Left            =   5640
         TabIndex        =   95
         Top             =   720
         Visible         =   0   'False
         Width           =   612
      End
      Begin VB.Label lbl 
         Caption         =   "Label"
         Height          =   252
         Index           =   17
         Left            =   4560
         TabIndex        =   94
         Tag             =   "DELAY"
         Top             =   360
         Width           =   1752
      End
      Begin VB.Label lbl 
         Caption         =   "Label"
         Height          =   252
         Index           =   16
         Left            =   240
         TabIndex        =   91
         Tag             =   "OUTITERATIONS"
         Top             =   680
         Width           =   2000
      End
      Begin VB.Label lbl 
         Caption         =   "Label"
         Height          =   252
         Index           =   15
         Left            =   240
         TabIndex        =   90
         Tag             =   "OUTSEED"
         Top             =   360
         Width           =   2000
      End
      Begin VB.Label ref 
         Caption         =   "Ref:"
         Height          =   252
         Index           =   16
         Left            =   3492
         TabIndex        =   89
         Top             =   684
         Width           =   852
      End
      Begin VB.Label ref 
         Caption         =   "Ref:"
         Height          =   252
         Index           =   15
         Left            =   3492
         TabIndex        =   88
         Top             =   360
         Width           =   852
      End
      Begin VB.Label vLbl 
         Caption         =   "Alias"
         Height          =   252
         Index           =   2
         Left            =   240
         TabIndex        =   78
         Top             =   1080
         Width           =   1836
      End
   End
   Begin VB.Label ref 
      Caption         =   "Ref:"
      Height          =   252
      Index           =   0
      Left            =   0
      TabIndex        =   98
      Top             =   0
      Width           =   852
   End
   Begin VB.Menu mFile 
      Caption         =   "&File"
      Begin VB.Menu mSave 
         Caption         =   "&Save && Exit"
      End
      Begin VB.Menu mExit 
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
Attribute VB_Name = "Sens"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text
Dim okToChange As Boolean
Dim temp As parmrec
Dim fui As parmfile
Dim fui2 As parmfile
Dim s1(6) As String
Dim t1(3) As String
Dim u1(2) As String
Dim tCnt As Long

Dim paths(21) As String
Dim organC() As String
Dim organD() As String

Dim suIter As New tInteger
Dim suSeed As New tInteger
Dim startTime As New tInteger

Dim vCt As Long
Dim vCache() As parmrec

Dim gidCt As Long
Dim gidData() As parmrec

Dim fuiCt As Long
Dim fuiData() As parmrec
Dim called As Boolean
Dim uExposure() As Exposure

Sub SetMyRef()
Dim j As Long
Dim k As Long

  Select Case RefItem
  Case 2
    out(List4.ItemData(List4.ListIndex)).ref = ref(RefItem).tag
  Case 3
    var(List1.ItemData(List1.ListIndex)).ref = ref(RefItem).tag
  Case 4
    var(List2.ItemData(List2.ListIndex)).dref = ref(RefItem).tag
  Case 5
    var(List2.ItemData(List2.ListIndex)).vCor(List3.ListIndex).ref = ref(RefItem).tag
    'add reflect reference which should be the same
    For j = 0 To vCnt - 1
      If var(j).vAlias = alias.Text Then
        For k = 0 To var(j).cnt - 1
          If var(j).vCor(k).cAlias = var(List2.ItemData(List2.ListIndex)).vAlias Then
            var(j).vCor(k).ref = ref(RefItem).tag
            Exit For
          End If
        Next
      End If
    Next
  Case 6
    var(List2.ItemData(List2.ListIndex)).eref = ref(RefItem).tag
  End Select
End Sub

Private Sub addref_Click()
  RefMode = 1
  GetRef ref(RefItem)
  SetMyRef
End Sub

Private Sub Check2_Click()
  txt(17).Enabled = Not (Check2.value = 0)
End Sub

Private Sub Form_Unload(Cancel As Integer)
  Dim answer As Long
  If Not called Then
    answer = MsgBox("Do you want to save changes?", 51, App.Title)
    If answer = 6 Then mSave_Click
    If answer = 7 Then
      If Not called Then
        called = True
        close_csv errfile
        Unload Load
        Close 'all files
        If Not AnError Then Kill RunName & ".ERR"
      End If
    End If
    If answer = 2 Then Cancel = 1
  End If
End Sub

Private Sub selref_Click()
  RefMode = 0
  GetRef ref(RefItem)
  SetMyRef
End Sub

Private Sub Form_Resize()
On Error Resume Next
  uFrame(0).width = Me.ScaleWidth - (iotree.width + 10)
  uFrame(1).width = Me.ScaleWidth - (iotree.width + 10)
  uFrame(2).width = Me.ScaleWidth - (iotree.width + 10)
  List1.width = Me.width - Screen.TwipsPerPixelX * (uFrame(0).Left + 40)
  List4.width = Me.width - Screen.TwipsPerPixelX * (uFrame(2).Left + 40)
  List1.Height = Me.Height - Screen.TwipsPerPixelX * 180
  List4.Height = Me.Height - Screen.TwipsPerPixelX * 180
  
  uFrame(0).Height = ScaleHeight - 1
  uFrame(1).Height = ScaleHeight - 1
  uFrame(2).Height = ScaleHeight - 1
  iotree.Height = ScaleHeight - 1
End Sub

Private Sub howto_Click()
  HelpAnchor = ""
  GetHelp
End Sub

Private Sub initparm(t As tFloat, n As String, v As Variant, i As Long, k As Long, u As String, tg As String, Optional lo As Variant, Optional hi As Variant)
  t.SetObj lbl(i), txt(i), unt(i), ref(0)
  t.Prompt = s1(k)
  t.UserUnit = u
  t.DefaultUnit = u
  t.UserValue = v
  t.DefaultValue = v
  t.name = n
  t.tag = tg
  If Not IsMissing(lo) Then
    If VarType(lo) = vbObject Then
      t.lower = lo
    ElseIf lo <> "" Then
      t.lower = val(lo)
    End If
  End If
  If Not IsMissing(hi) Then
    If VarType(hi) = vbObject Then
      t.upper = hi
    ElseIf hi <> "" Then
      t.upper = val(hi)
    End If
  End If
End Sub

Private Sub initstuff()
  called = False
  AnError = False
  suSeed.name = "suSeed"
  suSeed.SetObj lbl(15), txt(15), unt(15), ref(15)
  suSeed.tag = "OUTSEED"
  suSeed.Prompt = "Random seed value"
  suSeed.lower = 1
  suSeed.UserValue = 1
  suSeed.DefaultValue = 1
  suSeed.Refresh
  
  suIter.name = "suIter"
  suIter.SetObj lbl(16), txt(16), unt(16), ref(16)
  suIter.tag = "OUTITERATIONS"
  suIter.Prompt = "Number of iterations"
  suIter.lower = 2
  suIter.UserValue = 2
  suIter.DefaultValue = 2
  suIter.Refresh
    
  startTime.name = "startTime"
  startTime.SetObj lbl(17), txt(17), unt(17), ref(17)
  startTime.tag = "StartTime"
  startTime.Prompt = "Delay Run Until "
  startTime.lower = 0
  startTime.upper = 23
  startTime.UserValue = 0
  startTime.DefaultValue = 0
  startTime.Refresh


  s1(0) = "Upper bound (linear)"
  s1(1) = "Lower bound (linear)"
  s1(2) = "Mean"
  s1(3) = "Standard deviation"
  s1(4) = "Scale"
  s1(5) = "Shift"
  s1(6) = "Mode"
  t1(0) = "peak"
  t1(1) = "average years # to #"
  t1(2) = "at year(s) #(,#...)"
  tCnt = 2

  Dim xnode As ComctlLib.Node
  Set xnode = iotree.Nodes.Add(, , "variables", "Alias Input Variables")
  xnode.Expanded = True
  Set xnode = iotree.Nodes.Add(, , "parameters", "Distributions/Correlations")
  Set xnode = iotree.Nodes.Add(, , "outputs", "Alias Output Watches")
  xnode.Expanded = True
End Sub

Sub GetRoutesAndPathways(pdcfType As String, modname As String)
Dim i As Long
Dim j As Long
Dim k As Long
Dim retc As Long
Dim numDS As Long
Dim numPt As Long
Dim numAge As Long
Dim numOrgansC As Long
Dim numOrgansD As Long
Dim name As String
Dim tstr As String
Dim tstr2 As String
Dim tstr3 As String
  
  ReDim uExposure(0)
  Select Case pdcfType
    'api calls to FRAMES.DLL
    Case "epf": numDS = epfOpen(argv(0), modname)
    Case "rif": numDS = rifOpen(argv(0), modname)
    Case "hif": numDS = hifOpen(argv(0), modname)
  End Select
  
  For i = 0 To numDS - 1
    tstr = String(1024, Chr(0))
    tstr2 = String(1024, Chr(0))
    tstr3 = String(1024, Chr(0))
    Select Case pdcfType
      'api calls to FRAMES.DLL
      Case "epf": retc = epfGetSetInfo(i, numPt, tstr, tstr2, tstr3)
                  numAge = 1
      Case "rif": retc = rifGetSetInfo(i, numPt, numAge, tstr, tstr2, tstr3)
      Case "hif": retc = hifGetSetInfo(i, numPt, numAge, numOrgansC, numOrgansD, tstr, tstr2, tstr3)
        ReDim organC(numOrgansC)
        For j = 0 To numOrgansC - 1
          tstr = String(1024, Chr(0))
          hifGetCancerOrgan i, j, tstr
          organC(j) = Left(tstr, InStr(tstr, Chr(0)) - 1)
        Next
        ReDim organD(numOrgansD)
        For j = 0 To numOrgansD - 1
          tstr = String(1024, Chr(0))
          hifGetDoseOrgan i, j, tstr
          organD(j) = Left(tstr, InStr(tstr, Chr(0)) - 1)
        Next
    End Select
    For j = 0 To numAge - 1
      For k = 1 To cCnt
        GetUniqueRoutesAndPaths pdcfType, i, j, contam(k).cas, contam(k).cas
      Next
    Next
  Next
  
  Select Case pdcfType
    'api calls to FRAMES.DLL
    Case "epf": epfClose
    Case "rif": rifClose
    Case "hif": hifClose
  End Select
End Sub

Sub GetUniqueRoutesAndPaths(pdcfType As String, dsIdx As Long, ageIdx As Long, casid As String, parentCAS As String)
Dim i As Long
Dim j As Long
Dim retc As Long
Dim totc As Long
Dim numTime As Long
Dim timeIdx As Long
Dim Route As String
Dim Path As String
Dim Measure As String
Dim Unit As String
Dim found As Boolean

  Select Case pdcfType
    'api calls to FRAMES.DLL
    Case "epf": numTime = epfGetTimeCount(dsIdx, parentCAS)
    Case "rif": numTime = rifGetTimeCount(dsIdx, ageIdx, parentCAS)
    Case "hif": numTime = hifGetTimeCount(dsIdx, ageIdx, parentCAS)
  End Select
  
  totc = 0
  For timeIdx = 0 To numTime - 1
    Select Case pdcfType
      'api calls to FRAMES.DLL
      Case "epf": retc = epfLoadRoutesAndPathwaysByTime(dsIdx, casid, parentCAS, timeIdx)
      Case "rif": retc = rifLoadRoutesAndPathwaysByTime(dsIdx, ageIdx, casid, parentCAS, timeIdx)
      Case "hif": retc = hifLoadRoutesAndPathwaysByTime(dsIdx, ageIdx, casid, parentCAS, timeIdx)
    End Select
    For i = 0 To retc - 1
      Route = String(128, Chr(0))
      Path = String(128, Chr(0))
      Measure = String(128, Chr(0))
      Unit = String(128, Chr(0))
      Select Case pdcfType
        'api calls to FRAMES.DLL
        Case "epf": epfGetRouteAndPathway dsIdx, i, Path, Route, Unit
        Case "rif": rifGetRouteAndPathway dsIdx, i, Path, Route, Measure, Unit
        Case "hif": hifGetRouteAndPathway dsIdx, i, Path, Route, Measure, Unit
      End Select
      Route = Left(Route, InStr(Route, Chr(0)) - 1)
      Path = Left(Path, InStr(Path, Chr(0)) - 1)
      Measure = Left(Measure, InStr(Measure, Chr(0)) - 1)
      Unit = Left(Unit, InStr(Unit, Chr(0)) - 1)
      found = False
      For j = 1 To UBound(uExposure)
        found = (uExposure(j).Route = Route And _
                 uExposure(j).Path = Path And _
                 uExposure(j).Measure = Measure And _
                 uExposure(j).Unit = Unit)
        If found Then Exit For
      Next
      If Not found Then
        totc = 1 + UBound(uExposure)
        ReDim Preserve uExposure(totc)
        uExposure(totc).Route = Route
        uExposure(totc).Path = Path
        uExposure(totc).Measure = Measure
        uExposure(totc).Unit = Unit
      End If
    Next
  Next
End Sub

Private Function getIdx(Idx As String, p As parmrec, glyph As glyphtype) As Long
  getIdx = 0
  Select Case Idx
  Case "Site": getIdx = SiteIndex
  Case "Glyph": getIdx = glyph.Idx
  Case "Index1": getIdx = p.idx1
  Case "Index2": getIdx = p.idx2
  Case "Index3": getIdx = p.idx3
  Case "Index4": getIdx = p.idx4
  Case "Index5": getIdx = p.idx5
  Case "Index6": getIdx = p.idx6
  End Select
End Function

Private Function vLookupOld(i As Long, v As descriptor, p As parmrec, glyph As glyphtype) As String
Dim p1 As parmrec
Dim p2 As parmrec
Dim m As Long

  vLookupOld = "error"

  p1.pName = Trim(v.ques(i, 1))
  p1.idx1 = getIdx(v.ques(i, 2), p, glyph)
  p1.idx2 = getIdx(v.ques(i, 3), p, glyph)
  p1.idx3 = getIdx(v.ques(i, 4), p, glyph)
  p1.idx4 = getIdx(v.ques(i, 5), p, glyph)
  p1.idx5 = getIdx(v.ques(i, 6), p, glyph)
  p1.idx6 = getIdx(v.ques(i, 7), p, glyph)
  
  For m = 1 To vCt
    If p1.pName = vCache(m).pName And _
       p1.idx1 = vCache(m).idx1 And p1.idx2 = vCache(m).idx2 And p1.idx3 = vCache(m).idx3 And _
       p1.idx4 = vCache(m).idx4 And p1.idx5 = vCache(m).idx5 And p1.idx6 = vCache(m).idx6 Then
       vLookupOld = vCache(m).pval
       Exit Function
    End If
  Next
  
  If find_frst(fui2, p2, v.ques(i, 1)) Then
    Do
      If p1.idx1 = p2.idx1 And p1.idx2 = p2.idx2 And p1.idx3 = p2.idx3 And _
         p1.idx4 = p2.idx4 And p1.idx5 = p2.idx5 And p1.idx6 = p2.idx6 Then
         vLookupOld = p2.pval
         
         vCt = vCt + 1
         ReDim Preserve vCache(vCt)
         vCache(vCt) = p2
         Exit Function
      End If
      If Not find_next(fui2, p2) Then Exit Do
    Loop
  End If
  
  vCt = vCt + 1
  ReDim Preserve vCache(vCt)
  vCache(vCt) = p1
  vCache(vCt).pval = "error"
  
  vLookupOld = "error"
  
End Function
Private Function vLookup(i As Long, v As descriptor, p As parmrec, glyph As glyphtype) As String
Dim p1 As parmrec
Dim p2 As parmrec
Dim m As Long

  vLookup = "error"

  p1.pName = Trim(v.ques(i, 1))
  p1.idx1 = getIdx(v.ques(i, 2), p, glyph)
  p1.idx2 = getIdx(v.ques(i, 3), p, glyph)
  p1.idx3 = getIdx(v.ques(i, 4), p, glyph)
  p1.idx4 = getIdx(v.ques(i, 5), p, glyph)
  p1.idx5 = getIdx(v.ques(i, 6), p, glyph)
  p1.idx6 = getIdx(v.ques(i, 7), p, glyph)
  
  For m = 1 To vCt
    If p1.pName = vCache(m).pName And _
       p1.idx1 = vCache(m).idx1 And p1.idx2 = vCache(m).idx2 And p1.idx3 = vCache(m).idx3 And _
       p1.idx4 = vCache(m).idx4 And p1.idx5 = vCache(m).idx5 And p1.idx6 = vCache(m).idx6 Then
       vLookup = vCache(m).pval
       Exit Function
    End If
  Next
  
  For m = 1 To gidCt
    If p1.pName = gidData(m).pName And _
       p1.idx1 = gidData(m).idx1 And p1.idx2 = gidData(m).idx2 And p1.idx3 = gidData(m).idx3 And _
       p1.idx4 = gidData(m).idx4 And p1.idx5 = gidData(m).idx5 And p1.idx6 = gidData(m).idx6 Then
       vLookup = gidData(m).pval
       Exit Function
    End If
  Next
  
  For m = 1 To fuiCt
    If fuiData(m).pName = v.ques(i, 1) Then
      If p1.idx1 = fuiData(m).idx1 And p1.idx2 = fuiData(m).idx2 And p1.idx3 = fuiData(m).idx3 And _
         p1.idx4 = fuiData(m).idx4 And p1.idx5 = fuiData(m).idx5 And p1.idx6 = fuiData(m).idx6 Then
         vLookup = fuiData(m).pval
         
         vCt = vCt + 1
         ReDim Preserve vCache(vCt)
         vCache(vCt) = fuiData(m)
         Exit Function
      End If
    End If
  Next
  
  vCt = vCt + 1
  ReDim Preserve vCache(vCt)
  vCache(vCt) = p1
  vCache(vCt).pval = "error"
  
  vLookup = "error"

End Function

Private Sub addVar(glyph As glyphtype)
Dim i As Long
Dim pos1 As Long
Dim pos2 As Long
Dim parm As parmrec
  
  For i = 1 To gidCt
    parm = gidData(i)
    If IsNumeric(parm.pval) Then
      ReDim Preserve var(vCnt) As variable
      var(vCnt).vName = parm.pName
      var(vCnt).vDup = -1
      var(vCnt).vMod = glyph.id
      var(vCnt).vExe = glyph.exe
      var(vCnt).vDes = parm.pName + " (" + CStr(parm.idx1) + "," + Str(parm.idx2) + "," + Str(parm.idx3) + "," + Str(parm.idx4) + "," + Str(parm.idx5) + "," + Str(parm.idx6) + ")"
      var(vCnt).vMin = var(vCnt).vLower.lower
      var(vCnt).vMax = var(vCnt).vUpper.upper
      var(vCnt).vUnit = parm.cunit
      var(vCnt).Idx(0) = parm.idx1
      var(vCnt).Idx(1) = parm.idx2
      var(vCnt).Idx(2) = parm.idx3
      var(vCnt).Idx(3) = parm.idx4
      var(vCnt).Idx(4) = parm.idx5
      var(vCnt).Idx(5) = parm.idx6
      If var(vCnt).vUnit = "" Then var(vCnt).vUnit = "N/A"
'          initparm var(vCnt).vStd, "suStd", 0, 3, 3, var(vCnt).vUnit, 0, var(vCnt).vUpper.upper
      
      initparm var(vCnt).vStd, "suStd", 0, 3, 3, var(vCnt).vUnit, "DISTSTD", 0
      initparm var(vCnt).vUpper, "suUpper", 0, 0, 0, var(vCnt).vUnit, "DISTUPPER", var(vCnt).vLower, var(vCnt).vMax
      initparm var(vCnt).vLower, "suLower", 0, 1, 1, var(vCnt).vUnit, "DISTLOWER", var(vCnt).vMin, var(vCnt).vUpper
      initparm var(vCnt).vMean, "suMean", val(parm.pval), 2, 2, var(vCnt).vUnit, "DISTMEAN", var(vCnt).vMin, var(vCnt).vMax
      initparm var(vCnt).vScal, "suScale", 0, 3, 4, "N/A", "DISTSCALE", 0, 1
      initparm var(vCnt).vShift, "suShift", 0, 3, 5, var(vCnt).vUnit, "DISTSHIFT", var(vCnt).vLower, var(vCnt).vUpper
      initparm var(vCnt).vMode, "suMode", 0, 2, 6, var(vCnt).vUnit, "DISTMODE", var(vCnt).vLower, var(vCnt).vUpper
      var(vCnt).vDes2 = var(vCnt).vDes '& " from " + glyph.lbl & "(" & var(vCnt).vMod & ")"
      var(vCnt).vDes = var(vCnt).vDes & " from " + var(vCnt).vMod
      vCnt = vCnt + 1
    End If
  Next
End Sub

Private Sub addDesVar(v As descriptor, glyph As glyphtype)
Dim i As Long
Dim j As Long
Dim m As Long
Dim chk As Double
Dim keep As Boolean
Dim key As String
Dim xnode As ComctlLib.Node

  
  If v.typ <> "Not Stochastic" Then
    open_parm fui2, FUIName, 2
    For m = 1 To gidCt
      If gidData(m).pName = v.name Then
        If vCnt > UBound(var) Then
          ReDim Preserve var(vCnt + 50) As variable
        End If
        var(vCnt).vName = v.name
        var(vCnt).vDup = -1
        var(vCnt).vMod = glyph.id
        var(vCnt).vExe = glyph.exe
        var(vCnt).vMin = v.min
        var(vCnt).vMax = v.max
        var(vCnt).vFmt = v.fmt
        var(vCnt).vUnit = gidData(m).cunit
        var(vCnt).Idx(0) = gidData(m).idx1
        var(vCnt).Idx(1) = gidData(m).idx2
        var(vCnt).Idx(2) = gidData(m).idx3
        var(vCnt).Idx(3) = gidData(m).idx4
        var(vCnt).Idx(4) = gidData(m).idx5
        var(vCnt).Idx(5) = gidData(m).idx6
        If var(vCnt).vUnit = "" Then var(vCnt).vUnit = "N/A"
'        initparm var(vCnt).vStd, "suStdf", 0, 3, 3, var(vCnt).vUnit, 0, var(vCnt).vUpper.upper
        
        initparm var(vCnt).vStd, "suStd", 0, 3, 3, var(vCnt).vUnit, "DISTSTD", 0
        initparm var(vCnt).vUpper, "suUpper", 0, 0, 0, var(vCnt).vUnit, "DISTUPPER", var(vCnt).vLower, var(vCnt).vMax
        initparm var(vCnt).vLower, "suLower", 0, 1, 1, var(vCnt).vUnit, "DISTLOWER", var(vCnt).vMin, var(vCnt).vUpper
        initparm var(vCnt).vMean, "suMean", val(gidData(m).pval), 2, 2, var(vCnt).vUnit, "DISTMEAN", var(vCnt).vMin, var(vCnt).vMax
        initparm var(vCnt).vScal, "suScale", 0, 3, 4, "N/A", "DISTSCALE", 0, 1
        initparm var(vCnt).vShift, "suShift", 0, 3, 5, var(vCnt).vUnit, "DISTSHIFT", var(vCnt).vLower, var(vCnt).vUpper
        initparm var(vCnt).vMode, "suMode", 0, 2, 6, var(vCnt).vUnit, "DISTMODE", var(vCnt).vLower, var(vCnt).vUpper
        If v.des = "" Then
          var(vCnt).vDes = v.name
        Else
          var(vCnt).vDes = v.des
        End If
        
        keep = True
        For i = 1 To v.qCnt
          If v.ques(i, 0) = "Label" Then
            var(vCnt).vDes = var(vCnt).vDes + ", " + v.ques(i, 1) + Str(getIdx(v.ques(i, 2), gidData(m), glyph))
          Else
            key = vLookup(i, v, gidData(m), glyph)
            var(vCnt).vDes = var(vCnt).vDes + ", " + key
            
            On Error Resume Next
            chk = CDbl(key)
            If key <> "" And Err.Number <> 0 Then
              
              For j = 0 To Load.List1.ListCount - 1
                If contam(j + 1).name = key Or contam(j + 1).cas = key Then
                  If Not Load.List1.Selected(j) Then keep = False
                End If
              Next
              If keep Then
                Set xnode = iotree.Nodes.Add("vv" & glyph.id, tvwChild, "vi" & glyph.id & "," & key, key)
              End If
            
            End If
            On Error GoTo 0
          
          End If
        Next
        
        If keep Then
          var(vCnt).vDes2 = var(vCnt).vDes '& " from " & glyph.lbl & "(" & var(vCnt).vMod & ")"
          var(vCnt).vDes = var(vCnt).vDes & " from " + var(vCnt).vMod
          vCnt = vCnt + 1
        End If
      End If
    Next
'   ReDim Preserve var(vCnt) As variable
    close_parm fui2
  End If

End Sub

Private Function getOutNum(o As output) As String
Dim i As Long
  Select Case o.oTime
  Case t1(1): getOutNum = o.oType + " average years" + Str(o.oTimePt(0)) + " to" + Str(o.oTimePt(1))
  Case t1(2): getOutNum = o.oType + " year" + Str(o.oTimePt(0))
  Case t1(3): getOutNum = o.oType + " years"
              For i = 0 To o.oTimePtCnt - 1
                getOutNum = getOutNum + Str(o.oTimePt(i)) + ","
              Next
              getOutNum = getOutNum + " and" + Str(o.oTimePt(i))
  End Select
  getOutNum = getOutNum + " from " + o.oSourceID
  If o.oCASName <> "" Then getOutNum = getOutNum + " for " + o.oCASName
End Function

Private Sub outfill(z0 As String, z1 As String, z2 As String, z3 As String, z4 As String, z5 As String, z6 As String, _
                    Optional z7 As String = "", Optional z8 As String = "", _
                    Optional z9 As String = "", Optional z10 As String = "")
Dim i As Long
Dim yr As String
Dim nm As String
Dim blank2 As String
Dim xnode As ComctlLib.Node


  
  yr = "yr"
  nm = "suOutTimePt"
  If (oCnt > UBound(out)) Then
    ReDim Preserve out(oCnt + 50) As output
  End If
  out(oCnt).oExt = z0
  out(oCnt).oType = z1
  out(oCnt).oTime = z2
  out(oCnt).oSourceID = z3
  out(oCnt).oSourceName = z4
  out(oCnt).oCASID = z5
  out(oCnt).oCASName = z6
  out(oCnt).oParentID = z7
  out(oCnt).oParentName = z8
  out(oCnt).oOrgID = z9
  out(oCnt).oOrgName = z10
  out(oCnt).oTimePtCnt = 0
  
  On Error Resume Next
  If z5 <> "" Then
    Set xnode = iotree.Nodes.Add("oo" & z3, tvwChild, "oi" & z3 & "," & z5, z6 & "(" & z5 & ")")
  End If
  On Error GoTo 0
  
  On Error Resume Next
  If z9 <> "" Then
    If z10 = "" Then
      Set xnode = iotree.Nodes.Add("oo" & z3, tvwChild, "oi" & z3 & "," & z9, z9)
    Else
      Set xnode = iotree.Nodes.Add("oo" & z3, tvwChild, "oi" & z3 & "," & z9, z10 & "(" & z9 & ")")
    End If
  End If
  On Error GoTo 0
  
  Select Case out(oCnt).oTime
    Case t1(1):
      ReDim out(oCnt).oTimePt(2) As New tFloat
      out(oCnt).oTimePtCnt = 2
      out(oCnt).oTimePt(0).name = nm
      out(oCnt).oTimePt(0).SetObj lbl(5), txt(5), unt(5), ref(0)
      out(oCnt).oTimePt(0).Prompt = "Starting from"
      out(oCnt).oTimePt(0).UserUnit = yr
      out(oCnt).oTimePt(0).DefaultUnit = yr
      out(oCnt).oTimePt(1).name = nm
      out(oCnt).oTimePt(1).tag = "OUTTIME"
      out(oCnt).oTimePt(1).SetObj lbl(6), txt(6), unt(6), ref(0)
      out(oCnt).oTimePt(1).Prompt = "Ending at"
      out(oCnt).oTimePt(1).UserUnit = yr
      out(oCnt).oTimePt(1).DefaultUnit = yr
    Case t1(2):
      ReDim out(oCnt).oTimePt(10) As New tFloat
      out(oCnt).oTimePtCnt = 1
      For i = 0 To 9
        out(oCnt).oTimePt(i).name = nm
        out(oCnt).oTimePt(i).tag = "OUTTIME"
        out(oCnt).oTimePt(i).SetObj lbl(i + 5), txt(i + 5), unt(i + 5), ref(0)
        out(oCnt).oTimePt(i).Prompt = "Monitor"
        out(oCnt).oTimePt(i).UserUnit = yr
        out(oCnt).oTimePt(i).DefaultUnit = yr
      Next
  End Select
  
  blank2 = ""
  If z2 <> "" Then blank2 = " "
  
'  out(oCnt).oDes2 = z4 + "(" + z3 + ") " + z1 + blank2 + z2
  out(oCnt).oDes2 = z1 + blank2 + z2
  out(oCnt).oDes = z3 + " " + z1 + blank2 + z2
  
  If z5 <> "" Then
    out(oCnt).oDes2 = out(oCnt).oDes2 + " for " + z6 + "(" + z5 + ")"
    out(oCnt).oDes = out(oCnt).oDes + " for " + z5
  End If
  
  If z7 <> "" Then
    out(oCnt).oDes2 = out(oCnt).oDes2 + " prodgeny of " + z8 + "(" + z7 + ")"
    out(oCnt).oDes = out(oCnt).oDes + " prodgeny of " + z7
  End If
  
  If z9 <> "" Then
    If z10 <> "" Then
'      out(oCnt).oDes2 = Replace(out(oCnt).oDes2, z3 + ") ", z3 + ") " + z10 + "(" + z9 + ") ")
      out(oCnt).oDes2 = z10 + "(" + z9 + ") " + out(oCnt).oDes2
      out(oCnt).oDes = Replace(out(oCnt).oDes, z3 + " ", z3 + " " + z9 + " ")
    Else
'      out(oCnt).oDes2 = Replace(out(oCnt).oDes2, z3 + ") ", z3 + ") " + z9 + " ")
      out(oCnt).oDes2 = z9 + " " + out(oCnt).oDes2
      out(oCnt).oDes = Replace(out(oCnt).oDes, z3 + " ", z3 + " " + z9 + " ")
    End If
  End If
  
  oCnt = oCnt + 1
End Sub

Private Sub add_cpo(ext As String, lbl As String, modlbl As String, modname As String, Optional rad As Boolean = True, Optional chem As Boolean = True)
Dim i As Long
Dim j As Long
Dim k As Long

  For i = 1 To orgCnt
    For j = 1 To cCnt
    If Load.List1.Selected(j - 1) Then
      If (rad And contam(j).typ = 1) Or (chem And contam(j).typ <> 1) Or (rad And chem) Then
        outfill ext, lbl, "", modname, modlbl, contam(j).cas, contam(j).name, "", "", org(i).id, org(i).name
      End If
    End If
    Next
  Next
End Sub

Private Sub add_tcpo(ext As String, lbl As String, modlbl As String, modname As String, Optional rad As Boolean = True, Optional chem As Boolean = True)
Dim i As Long
Dim j As Long
Dim k As Long
Dim t As Long

  For i = 1 To orgCnt
    For j = 1 To cCnt
    If Load.List1.Selected(j - 1) Then
      For t = 0 To tCnt
        If (rad And contam(j).typ = 1) Or (chem And contam(j).typ <> 1) Or (rad And chem) Then
          outfill ext, lbl, t1(t), modname, modlbl, contam(j).cas, contam(j).name, "", "", org(i).id, org(i).name
        End If
      Next
    End If
    Next
  Next
End Sub

Private Sub add_tcpp(ext As String, lbl As String, modlbl As String, modname As String, Optional rad As Boolean = True, Optional chem As Boolean = True)
Dim i As Long
Dim j As Long
Dim k As Long
Dim t As Long
Dim cnt As Long
Dim rFind As String
Dim Path() As String
  
  cnt = -1
  rFind = Left(lbl, 6)
  For i = 0 To UBound(uExposure)
    If InStr(1, uExposure(i).Route, rFind) > 0 Then
      For j = 0 To cnt
        If Path(j) = uExposure(i).Path Then Exit For
      Next
      If j > cnt Then
        ReDim Preserve Path(j)
        Path(j) = uExposure(i).Path
        cnt = j
      End If
    End If
  Next
  
  For i = 0 To cnt
    For j = 1 To cCnt
    If Load.List1.Selected(j - 1) Then
      For t = 0 To tCnt
        If (rad And contam(j).typ = 1) Or (chem And contam(j).typ <> 1) Or (rad And chem) Then
          outfill ext, lbl, t1(t), modname, modlbl, contam(j).cas, contam(j).name, "", "", Path(i), ""
        End If
      Next
    End If
    Next
  Next
End Sub

Private Sub add_tcporgC(ext As String, lbl As String, Unit As String, modlbl As String, modname As String, Optional rad As Boolean = True, Optional chem As Boolean = True)
Dim i As Long
Dim j As Long
Dim k As Long
Dim m As Long
Dim cnt As Long
Dim Path() As String
  
  cnt = -1
  For i = 0 To UBound(uExposure)
    If InStr(1, uExposure(i).Route, lbl) > 0 And Unit = uExposure(i).Unit Then
      For j = 0 To cnt
        If Path(j) = uExposure(i).Path Then Exit For
      Next
      If j > cnt Then
        ReDim Preserve Path(j)
        Path(j) = uExposure(i).Path
        cnt = j
      End If
    End If
  Next
  
  For k = 0 To cnt
  For m = 0 To UBound(organC)
    For i = 1 To cCnt
    If Load.List1.Selected(i - 1) Then
      For j = 0 To tCnt
        If (rad And contam(i).typ = 1) Or (chem And contam(i).typ <> 1) Or (rad And chem) Then
          outfill ext, Path(k) + " " + lbl + " " + Unit, t1(j), modname, modlbl, contam(i).cas, contam(i).name, "", "", organC(m)
        End If
      Next
    End If
    Next
  Next
  Next
End Sub

Private Sub add_tcporgD(ext As String, lbl As String, Unit As String, modlbl As String, modname As String, Optional rad As Boolean = True, Optional chem As Boolean = True)
Dim i As Long
Dim j As Long
Dim k As Long
Dim m As Long
Dim cnt As Long
Dim Path() As String
  
  cnt = -1
  For i = 0 To UBound(uExposure)
    If InStr(1, uExposure(i).Route, lbl) > 0 And Unit = uExposure(i).Unit Then
      For j = 0 To cnt
        If Path(j) = uExposure(i).Path Then Exit For
      Next
      If j > cnt Then
        ReDim Preserve Path(j)
        Path(j) = uExposure(i).Path
        cnt = j
      End If
    End If
  Next
  
  For k = 0 To cnt
  For m = 0 To UBound(organD)
    For i = 1 To cCnt
    If Load.List1.Selected(i - 1) Then
      For j = 0 To tCnt
        If (rad And contam(i).typ = 1) Or (chem And contam(i).typ <> 1) Or (rad And chem) Then
          outfill ext, Path(k) + " " + lbl + " " + Unit, t1(j), modname, modlbl, contam(i).cas, contam(i).name, "", "", organD(m)
        End If
      Next
    End If
    Next
  Next
  Next
End Sub

Private Sub add_tcp(ext As String, lbl As String, modlbl As String, modname As String, Optional rad As Boolean = True, Optional chem As Boolean = True)
Dim i As Long
Dim j As Long
Dim k As Long
  
  For i = 1 To cCnt
    If Load.List1.Selected(i - 1) Then
      For j = 0 To tCnt
        If (rad And contam(i).typ = 1) Or (chem And contam(i).typ <> 1) Or (rad And chem) Then
          outfill ext, lbl, t1(j), modname, modlbl, contam(i).cas, contam(i).name
        End If
      Next
    End If
  Next
End Sub

Private Sub add_t(ext As String, lbl As String, modlbl As String, modname As String)
Dim j As Long

  For j = 0 To tCnt
    outfill ext, lbl, t1(j), modname, modlbl, "", ""
  Next
End Sub

Private Sub add_torgC(ext As String, lbl As String, modlbl As String, modname As String)
Dim j As Long
Dim m As Long

  For m = 0 To UBound(organC)
    For j = 0 To tCnt
      outfill ext, lbl, t1(j), modname, modlbl, "", "", "", "", organC(m)
    Next
  Next
End Sub

Private Sub add_torgD(ext As String, lbl As String, modlbl As String, modname As String)
Dim j As Long
Dim m As Long
  
  For m = 0 To UBound(organD)
    For j = 0 To tCnt
      outfill ext, lbl, t1(j), modname, modlbl, "", "", "", "", organD(m)
    Next
  Next
End Sub

Private Sub addOut(typ As String, glyph As glyphtype)
  Dim i As Long
  Dim j As Long
  Dim k As Long
  Dim fluxtype As String
  Dim scfB As Boolean
  Dim wcfB As Boolean
  
  scfB = False
  wcfB = False
  For i = 0 To glyph.fcount
    Select Case glyph.ftype(i)
    Case "aff"
      add_tcp glyph.ftype(i), "Gas 1 flux", glyph.lbl, glyph.id
      add_tcp glyph.ftype(i), "Particle 1 flux", glyph.lbl, glyph.id
      add_tcp glyph.ftype(i), "Particle 2 flux", glyph.lbl, glyph.id
      add_tcp glyph.ftype(i), "Particle 3 flux", glyph.lbl, glyph.id
    Case "wff"
      add_t glyph.ftype(i), "Water flux", glyph.lbl, glyph.id
      If glyph.fqual(i) = "surface water" Then
        add_tcp glyph.ftype(i), "Dissolved constituent flux", glyph.lbl, glyph.id
        add_tcp glyph.ftype(i), "Adsorbed constituent flux", glyph.lbl, glyph.id
      Else
        add_tcp glyph.ftype(i), "Total constituent flux", glyph.lbl, glyph.id
      End If
    Case "wcf"
      If Not wcfB Then
          add_tcp glyph.ftype(i), "Total/Dissolved water concentration", glyph.lbl, glyph.id
      End If
      wcfB = True
    Case "scf"
      If Not scfB Then
          add_tcp glyph.ftype(i), "Total/Dissolved earth concentration", glyph.lbl, glyph.id
      End If
      scfB = True
    Case "bbf"
      add_tcpo glyph.ftype(i), "Body burden", glyph.lbl, glyph.id, False
      
'    Case "exf"
'      add_cpo glyph.ftype(i), "Percent time exceeding effect", glyph.lbl, glyph.Id, False
'      add_tcpo glyph.ftype(i), "Hazard quotient", glyph.lbl, glyph.Id, False
'      add_tcpo glyph.ftype(i), "Intake hazard quotient", glyph.lbl, glyph.Id, False
'
    Case "twi"
      add_tcpo glyph.ftype(i), "dose", glyph.lbl, glyph.id, False

    Case "ato"
      For j = 1 To 4
        fluxtype = "particle " & CStr(j)
        If j = 4 Then fluxtype = "gas 1"
        For k = 1 To 5
          add_tcp glyph.ftype(i), "Concentration for " & fluxtype & " at location #" & CStr(k), glyph.lbl, glyph.id, True, True
          add_tcp glyph.ftype(i), "Total deposition for " & fluxtype & " at location #" & CStr(k), glyph.lbl, glyph.id, True, True
          add_tcp glyph.ftype(i), "Wet deposition for " & fluxtype & " at location #" & CStr(k), glyph.lbl, glyph.id, True, True
          add_tcp glyph.ftype(i), "Dry deposition for " & fluxtype & " at location #" & CStr(k), glyph.lbl, glyph.id, True, True
        Next
      Next
    Case "epf"
      GetRoutesAndPathways glyph.ftype(i), glyph.id
      add_tcpp glyph.ftype(i), "dermal concentration", glyph.lbl, glyph.id
      add_tcpp glyph.ftype(i), "ingestion concentration", glyph.lbl, glyph.id
      add_tcpp glyph.ftype(i), "inhalation concentration", glyph.lbl, glyph.id
      add_tcpp glyph.ftype(i), "external dose", glyph.lbl, glyph.id, True, False
      
    Case "rif"
      GetRoutesAndPathways glyph.ftype(i), glyph.id
      add_t glyph.ftype(i), "Summed Bq intake", glyph.lbl, glyph.id
      add_t glyph.ftype(i), "Summed Bq/kg concentration exposure", glyph.lbl, glyph.id
      add_t glyph.ftype(i), "Summed Bq/L concentration exposure", glyph.lbl, glyph.id
      add_t glyph.ftype(i), "Summed Bq/m^3 concentration exposure", glyph.lbl, glyph.id
      add_t glyph.ftype(i), "Summed mg/kg/day carcinogenic intake", glyph.lbl, glyph.id
      add_t glyph.ftype(i), "Summed mg/kg/day noncarcinogenic intake", glyph.lbl, glyph.id
      add_t glyph.ftype(i), "Summed mg/m^3 carcinogenic intake", glyph.lbl, glyph.id
      add_t glyph.ftype(i), "Summed mg/m^3 noncarcinogenic intake", glyph.lbl, glyph.id
      
      add_tcpp glyph.ftype(i), "dermal Bq intake", glyph.lbl, glyph.id, True, False
      add_tcpp glyph.ftype(i), "ingestion Bq intake", glyph.lbl, glyph.id, True, False
      add_tcpp glyph.ftype(i), "inhalation Bq intake", glyph.lbl, glyph.id, True, False
      
      add_tcpp glyph.ftype(i), "dermal mg/kg/day carcinogenic intake", glyph.lbl, glyph.id, False
      add_tcpp glyph.ftype(i), "ingestion mg/kg/day carcinogenic intake", glyph.lbl, glyph.id, False
      add_tcpp glyph.ftype(i), "inhalation mg/kg/day carcinogenic intake", glyph.lbl, glyph.id, False
      add_tcpp glyph.ftype(i), "inhalation mg/m^3 carcinogenic intake", glyph.lbl, glyph.id, False

      add_tcpp glyph.ftype(i), "dermal mg/kg/day noncarcinogenic intake", glyph.lbl, glyph.id, False
      add_tcpp glyph.ftype(i), "ingestion mg/kg/day noncarcinogenic intake", glyph.lbl, glyph.id, False
      add_tcpp glyph.ftype(i), "inhalation mg/kg/day noncarcinogenic intake", glyph.lbl, glyph.id, False
      add_tcpp glyph.ftype(i), "inhalation mg/m^3 noncarcinogenic intake", glyph.lbl, glyph.id, False
    
      add_tcpp glyph.ftype(i), "external Bq/kg concentration exposure", glyph.lbl, glyph.id, True, False
      add_tcpp glyph.ftype(i), "external Bq/L concentration exposure", glyph.lbl, glyph.id, True, False
      add_tcpp glyph.ftype(i), "external Bq/m^3 concentration exposure", glyph.lbl, glyph.id, True, False

    Case "hif"
      GetRoutesAndPathways glyph.ftype(i), glyph.id
      add_torgD glyph.ftype(i), "Summed Sv", glyph.lbl, glyph.id
      add_torgC glyph.ftype(i), "Summed cancer incidence", glyph.lbl, glyph.id
      add_torgC glyph.ftype(i), "Summed cancer fatalities", glyph.lbl, glyph.id
      add_torgC glyph.ftype(i), "Summed cancer plus severe hereditary effects", glyph.lbl, glyph.id
      add_t glyph.ftype(i), "Summed carcinogenic risk", glyph.lbl, glyph.id
      add_t glyph.ftype(i), "Summed noncarcinogenic hazard index", glyph.lbl, glyph.id
      
      add_tcporgD glyph.ftype(i), "dermal", "Sv", glyph.lbl, glyph.id, True, False
      add_tcporgC glyph.ftype(i), "dermal", "cancer incidence", glyph.lbl, glyph.id, True, False
      add_tcporgC glyph.ftype(i), "dermal", "cancer fatalities", glyph.lbl, glyph.id, True, False
      add_tcporgC glyph.ftype(i), "dermal", "cancer plus severe hereditary effects", glyph.lbl, glyph.id, True, False
      
      add_tcporgD glyph.ftype(i), "ingestion", "Sv", glyph.lbl, glyph.id, True, False
      add_tcporgC glyph.ftype(i), "ingestion", "cancer incidence", glyph.lbl, glyph.id, True, False
      add_tcporgC glyph.ftype(i), "ingestion", "cancer fatalities", glyph.lbl, glyph.id, True, False
      add_tcporgC glyph.ftype(i), "ingestion", "cancer plus severe hereditary effects", glyph.lbl, glyph.id, True, False
     
      add_tcporgD glyph.ftype(i), "inhalation", "Sv", glyph.lbl, glyph.id, True, False
      add_tcporgC glyph.ftype(i), "inhalation", "cancer incidence", glyph.lbl, glyph.id, True, False
      add_tcporgC glyph.ftype(i), "inhalation", "cancer fatalities", glyph.lbl, glyph.id, True, False
      add_tcporgC glyph.ftype(i), "inhalation", "cancer plus severe hereditary effects", glyph.lbl, glyph.id, True, False

      add_tcporgD glyph.ftype(i), "external", "Sv", glyph.lbl, glyph.id, True, False
      add_tcporgC glyph.ftype(i), "external", "cancer incidence", glyph.lbl, glyph.id, True, False
      add_tcporgC glyph.ftype(i), "external", "cancer fatalities", glyph.lbl, glyph.id, True, False
      add_tcporgC glyph.ftype(i), "external", "cancer plus severe hereditary effects", glyph.lbl, glyph.id, True, False

      add_tcpp glyph.ftype(i), "dermal carcinogenic risk", glyph.lbl, glyph.id, False
      add_tcpp glyph.ftype(i), "ingestion carcinogenic risk", glyph.lbl, glyph.id, False
      add_tcpp glyph.ftype(i), "inhalation carcinogenic risk", glyph.lbl, glyph.id, False
      add_tcpp glyph.ftype(i), "dermal noncarcinogenic hazard index", glyph.lbl, glyph.id, False
      add_tcpp glyph.ftype(i), "ingestion noncarcinogenic hazard index", glyph.lbl, glyph.id, False
      add_tcpp glyph.ftype(i), "inhalation noncarcinogenic hazard index", glyph.lbl, glyph.id, False
    
    End Select
  Next
End Sub

Private Sub loadGidData(glyphId As String)
Dim m As Long
Dim parm As parmrec

  ReDim gidData(0)
  gidCt = 0
  
  If Not find_frst(fui, parm, glyphId) Then
    put_val errfile, "Can't find " + glyphId + " section in " + FUIName
    put_line errfile
    close_csv errfile
    close_parm fui
    close_parm fui2
    End
  End If
' read_parmrec fui, parm
  gidCt = parm.idx1
  ReDim gidData(gidCt)
      
  For m = 1 To gidCt
    If read_parmrec(fui, parm) Then gidData(m) = parm
  Next
End Sub

Private Sub loadDES(glyph As glyphtype)
Dim fle As csv
Dim i As Long, j As Long, k As Long, pos As Long
Dim v As descriptor
Dim pCnt As Long
Dim varCnt As Long
Dim ver As String, nvers As Single
Dim Class As String, group As String, id As String
Dim chk As String
Dim curDrive As String

  curDrive = CurDir
  'ChDrive App.path
  
  If open_csv(fle, glyph.Path, 2) Then
    chk = get_val(fle)                 'des descriptor
    If chk <> "mf" Then GoTo loadDesErr
    ver = get_val(fle)                 'version descriptor
    loadGidData glyph.id
    
    If (InStr(ver, "beta")) Then GoTo loadDesErr
  
    ' assumes version appears as "Version n[.n]"
    If 0 < InStr(ver, "Version") Then
      nvers = val(Mid(ver, InStr(ver, " ")))
    Else
      GoTo loadDesErr
    End If
  
    If nvers < 2# Then GoTo loadDesErr
    get_line fle
    chk = get_val(fle)                  'model type,
    
    pos = InStr(chk, ":")
    If pos = 0 Then pos = InStr(chk, ",") ' late change in specification
    Class = Left(chk, pos - 1)
    chk = Mid(chk, pos + 1)
    pos = InStr(chk, ":")
    If pos = 0 Then pos = InStr(chk, ",") ' late change in specification
    group = Left(chk, pos - 1)
    id = Mid(chk, pos + 1)
    
    get_val fle
    get_val fle
    glyph.exe = get_val(fle)
    get_line fle                        'the rest of the line - name,ui.exe,mod.exe
    get_val fle                         'model description
    get_line fle
    pCnt = val(get_val(fle))
    For i = 0 To pCnt
      get_line fle
    Next
  
  ' for future versions of summm connection information will be read here
  ' The future is now
  ' Karl Castleton 11-27-01 added code to keep which file types a module
  ' produces
    glyph.fcount = val(get_val(fle))
    ReDim glyph.ftype(glyph.fcount) As String
    ReDim glyph.fqual(glyph.fcount) As String
    For i = 0 To glyph.fcount
      glyph.ftype(i) = get_val(fle)
      glyph.fqual(i) = get_val(fle)
      get_line fle
    Next
    
   'read and generate input and output variable descriptions
    addOut group, glyph
    
    varCnt = val(get_val(fle))
    get_line fle
    If varCnt = 0 Then
      addVar glyph
    Else
      For i = 1 To varCnt
        v.name = get_val(fle)
        v.typ = get_val(fle)
        v.Unit = get_val(fle)
        If v.Unit = "" Then v.Unit = "N/A"
        chk = get_val(fle)
        If chk = "MIN" Then
          v.min = get_val(fle)
        Else
          v.min = "-1.78E+308"
        End If
        chk = get_val(fle)
        If chk = "MAX" Then
          v.max = get_val(fle)
        Else
          v.max = "1.78E+308"
        End If
        v.des = get_val(fle)
        v.qCnt = val(get_val(fle))
        v.datatype = get_val(fle)
        v.fmt = get_val(fle)
        get_line fle
        ReDim v.ques(v.qCnt, 7) As String
        For j = 1 To v.qCnt
          For k = 0 To 7
            v.ques(j, k) = get_val(fle)
          Next
          get_line fle
        Next
        addDesVar v, glyph
      Next
      ReDim Preserve var(vCnt) As variable
    End If
    close_csv fle
    ChDrive curDrive
  Else
    put_val errfile, "Can't find or open file " & glyph.Path
    put_line errfile
loadDesErr:
    put_val errfile, "Error in Description file for module " & glyph.id
    put_line errfile
    close_csv errfile
    close_parm fui
    ChDrive curDrive
    End
  End If
End Sub

Private Sub loadSU()
  Dim i As Long, j As Long, k As Long
  Dim m As Long, pos As Long
  Dim tvcnt As Long
  Dim twcnt As Long
  Dim tv() As variable
  Dim tw() As output
  Dim dval As Double
  
  ReDim tv(0)
  ReDim tw(0)
  
  If open_parm(fui, FUIName, 2) Then
    Do Until EOCF(fui.file)
      If read_parmrec(fui, temp) Then
        Select Case temp.pName
          Case Model
            For m = 1 To temp.idx1
              If read_parmrec(fui, temp) Then
                pos = InStr(temp.pName, "suO")
                If pos = 0 And tvcnt < temp.idx1 Then
                  ReDim Preserve tv(temp.idx1) As variable
                  tvcnt = tvcnt + 1
                End If
                If pos > 0 And twcnt < temp.idx1 Then
                  ReDim Preserve tw(temp.idx1) As output
                  twcnt = twcnt + 1
                End If
                Select Case temp.pName
                  Case "suSeed":       suSeed.UserValue = temp.pval: suSeed.Reference = temp.ref: suSeed.Refresh
                  Case "suIter":       suIter.UserValue = temp.pval: suIter.Reference = temp.ref: suIter.Refresh
                  Case "startTime":    startTime.UserValue = temp.pval: startTime.Reference = temp.ref: startTime.Refresh
                  Case "noDelay":
                    If (temp.pval = "T") Then
                      Check2.value = 1
                      txt(17).Enabled = True
                    Else
                      Check2.value = 0
                      txt(17).Enabled = False
                    End If
                  Case "suAlias":
                    tv(temp.idx1).vAlias = temp.pval
                    tv(temp.idx1).ref = temp.ref
                  Case "suName":       tv(temp.idx1).vName = temp.pval
                  Case "suIndex1":     tv(temp.idx1).Idx(0) = temp.pval
                  Case "suIndex2":     tv(temp.idx1).Idx(1) = temp.pval
                  Case "suIndex3":     tv(temp.idx1).Idx(2) = temp.pval
                  Case "suIndex4":     tv(temp.idx1).Idx(3) = temp.pval
                  Case "suIndex5":     tv(temp.idx1).Idx(4) = temp.pval
                  Case "suIndex6":     tv(temp.idx1).Idx(5) = temp.pval
                  Case "suDes":        tv(temp.idx1).vDes = temp.pval
                  Case "suMod":        tv(temp.idx1).vMod = temp.pval
                  Case "suFmt":        tv(temp.idx1).vFmt = temp.pval
                  Case "suDistrib":
                    tv(temp.idx1).dref = temp.ref
                    Select Case temp.pval
                      Case distrib.list(0): tv(temp.idx1).vDistrib = 0
                      Case distrib.list(1): tv(temp.idx1).vDistrib = 1
                      Case distrib.list(2): tv(temp.idx1).vDistrib = 2
                      Case distrib.list(3): tv(temp.idx1).vDistrib = 3
                      Case distrib.list(4): tv(temp.idx1).vDistrib = 4
                      Case distrib.list(5): tv(temp.idx1).vDistrib = 5
                      Case distrib.list(6): tv(temp.idx1).vDistrib = 6
                      Case distrib.list(7): tv(temp.idx1).vDistrib = 7
                      Case distrib.list(8): tv(temp.idx1).vDistrib = 8
                    End Select
                  Case "suUpper":
                    dval = CDbl(val(temp.pval))
                    tv(temp.idx1).vUpper.UserValue = val(convert(temp.cunit, temp.uunit, dval))
                    tv(temp.idx1).vUpper.UserUnit = temp.uunit
                    tv(temp.idx1).vUpper.DefaultUnit = temp.cunit
                  Case "suLower":
                    dval = CDbl(val(temp.pval))
                    tv(temp.idx1).vLower.UserValue = val(convert(temp.cunit, temp.uunit, dval))
                    tv(temp.idx1).vLower.UserUnit = temp.uunit
                    tv(temp.idx1).vLower.DefaultUnit = temp.cunit
                  Case "suMean":
                    dval = CDbl(val(temp.pval))
                    If tv(temp.idx1).vDistrib = 4 Then
                      tv(temp.idx1).vMean.UserValue = val(convertLog(temp.cunit, temp.uunit, dval, tv(temp.idx1).vBase))
                    Else
                      tv(temp.idx1).vMean.UserValue = val(convert(temp.cunit, temp.uunit, dval))
                    End If
                    tv(temp.idx1).vMean.UserUnit = temp.uunit
                    tv(temp.idx1).vMean.DefaultUnit = temp.cunit
                  Case "suStd":
                    dval = CDbl(val(temp.pval))
                    If tv(temp.idx1).vDistrib = 4 Then
                      If temp.ref = 0 Then
                        tv(temp.idx1).vStd.UserValue = val(convertLog(temp.cunit, temp.uunit, dval, tv(temp.idx1).vBase))
                      Else
                        tv(temp.idx1).vStd.UserValue = val(convertLog(temp.cunit, temp.uunit, -dval, tv(temp.idx1).vBase))
                      End If
                    Else
                      tv(temp.idx1).vStd.UserValue = val(convert(temp.cunit, temp.uunit, dval))
                    End If
                    tv(temp.idx1).vStd.UserUnit = temp.uunit
                    tv(temp.idx1).vStd.DefaultUnit = temp.cunit
                  Case "suScale":
                    dval = CDbl(val(temp.pval))
                    tv(temp.idx1).vScal.UserValue = val(convert(temp.cunit, temp.uunit, dval))
                    tv(temp.idx1).vScal.UserUnit = temp.uunit
                    tv(temp.idx1).vScal.DefaultUnit = temp.cunit
                  Case "suShift":
                    dval = CDbl(val(temp.pval))
                    tv(temp.idx1).vShift.UserValue = val(convert(temp.cunit, temp.uunit, dval))
                    tv(temp.idx1).vShift.UserUnit = temp.uunit
                    tv(temp.idx1).vShift.DefaultUnit = temp.cunit
                  Case "suMode":
                    dval = CDbl(val(temp.pval))
                    tv(temp.idx1).vMode.UserValue = val(convert(temp.cunit, temp.uunit, dval))
                    tv(temp.idx1).vMode.UserUnit = temp.uunit
                    tv(temp.idx1).vMode.DefaultUnit = temp.cunit
                  Case "suBase":
                    tv(temp.idx1).vBase = val(temp.pval)
                  Case "suEqu":
                    tv(temp.idx1).vEqu = temp.pval
                    tv(temp.idx1).eref = temp.ref
                  Case "suCorNum":
                    tv(temp.idx1).cnt = val(temp.pval)
                    ReDim tv(temp.idx1).vCor(tv(temp.idx1).cnt) As correlation
                  Case "suCorAlias":
                    tv(temp.idx1).vCor(temp.idx2).cAlias = temp.pval
                    tv(temp.idx1).vCor(temp.idx2).ref = temp.ref
                  Case "suCor":
                    tv(temp.idx1).vCor(temp.idx2).cFactor = val(temp.pval)
                  Case "suOutAlias":
                    tw(temp.idx1).oAlias = temp.pval
                    tw(temp.idx1).ref = temp.ref
                  Case "suOutDes":
                    tw(temp.idx1).oDes = temp.pval
                  Case "suOutType":
                    tw(temp.idx1).oType = temp.pval
                  Case "suOutTime":
                    tw(temp.idx1).oTime = temp.pval
                  Case "suOutNumYear", "suOutTimePtCnt":
                    tw(temp.idx1).oTimePtCnt = val(temp.pval)
                    ReDim tw(temp.idx1).oTimePt(tw(temp.idx1).oTimePtCnt) As New tFloat
                  Case "suOutYear", "suOutTimePt":
                    tw(temp.idx1).oTimePt(temp.idx2).UserValue = val(temp.pval)
                  Case "suOutSource", "suOutSourceID":
                    tw(temp.idx1).oSourceID = temp.pval
                  Case "suOutContam", "suOutCASID":
                    tw(temp.idx1).oCASID = temp.pval
                  Case "suOutParentContam", "suOutParentID":
                    tw(temp.idx1).oParentID = temp.pval
                  Case "suOutOrgId":
                    tw(temp.idx1).oOrgID = temp.pval
                End Select
              End If
            Next
          Case Else
            For m = 1 To temp.idx1
              get_line fui.file
            Next
        End Select
      End If
    Loop
    close_parm fui
    
'   resolve variables
    For j = 1 To tvcnt
      For i = 0 To vCnt - 1
        If var(i).vMod = tv(j).vMod Then
          If (var(i).vName = tv(j).vName And var(i).vAlias = "" And _
             var(i).Idx(0) = tv(j).Idx(0) And _
             var(i).Idx(1) = tv(j).Idx(1) And _
             var(i).Idx(2) = tv(j).Idx(2) And _
             var(i).Idx(3) = tv(j).Idx(3) And _
             var(i).Idx(4) = tv(j).Idx(4) And _
             var(i).Idx(5) = tv(j).Idx(5)) Then
            var(i).vAlias = tv(j).vAlias
            var(i).vEqu = tv(j).vEqu
            var(i).vDistrib = tv(j).vDistrib
            var(i).vUpper.UserValue = tv(j).vUpper.UserValue
            var(i).vUpper.UserUnit = tv(j).vUpper.UserUnit
            var(i).vUpper.DefaultUnit = tv(j).vUpper.DefaultUnit
            
            var(i).vLower.UserValue = tv(j).vLower.UserValue
            var(i).vLower.UserUnit = tv(j).vLower.UserUnit
            var(i).vLower.DefaultUnit = tv(j).vLower.DefaultUnit
            
            var(i).vMean.UserValue = tv(j).vMean.UserValue
            var(i).vMean.UserUnit = tv(j).vMean.UserUnit
            var(i).vMean.DefaultUnit = tv(j).vMean.DefaultUnit
            
            var(i).vStd.UserValue = tv(j).vStd.UserValue
            var(i).vStd.UserUnit = tv(j).vStd.UserUnit
            var(i).vStd.DefaultUnit = tv(j).vStd.DefaultUnit
            
            var(i).vScal.UserValue = tv(j).vScal.UserValue
            var(i).vScal.UserUnit = tv(j).vScal.UserUnit
            var(i).vScal.DefaultUnit = tv(j).vScal.DefaultUnit
            
            var(i).vShift.UserValue = tv(j).vShift.UserValue
            var(i).vShift.UserUnit = tv(j).vShift.UserUnit
            var(i).vShift.DefaultUnit = tv(j).vShift.DefaultUnit
            
            var(i).vMode.UserValue = tv(j).vMode.UserValue
            var(i).vMode.UserUnit = tv(j).vMode.UserUnit
            var(i).vMode.DefaultUnit = tv(j).vMode.DefaultUnit
            
            var(i).vBase = tv(j).vBase
            var(i).ref = tv(j).ref
            var(i).dref = tv(j).dref
            var(i).eref = tv(j).eref
            var(i).cnt = tv(j).cnt
            ReDim Preserve var(i).vCor(var(i).cnt) As correlation
            For k = 1 To tv(j).cnt
              var(i).vCor(k - 1).cAlias = tv(j).vCor(k).cAlias
              var(i).vCor(k - 1).cFactor = tv(j).vCor(k).cFactor
              var(i).vCor(k - 1).ref = tv(j).vCor(k).ref
            Next
            Exit For
          End If
        End If
      Next
    Next
    
' resolve output editorial changes that cause a mismatch for backward compatibility

    For j = 1 To twcnt
      For i = 0 To oCnt - 1
        If (out(i).oType = tw(j).oType Or _
           (out(i).oType = "Total/Dissolved water concentration" And tw(j).oType = "Water concentration") Or _
           (out(i).oType = "Total/Dissolved earth concentration" And tw(j).oType = "Soil/Sediment concentration")) And _
           out(i).oTime = tw(j).oTime And _
           out(i).oSourceID = tw(j).oSourceID And _
           out(i).oCASID = tw(j).oCASID And _
           out(i).oParentID = tw(j).oParentID And _
           (out(i).oOrgID = tw(j).oOrgID Or (out(i).oOrgID = "" And tw(j).oOrgID <> "")) Then
          
          out(i).oOrgID = tw(j).oOrgID
          out(i).oAlias = tw(j).oAlias
          out(i).ref = tw(j).ref
          out(i).oTimePtCnt = tw(j).oTimePtCnt
          For k = 1 To tw(j).oTimePtCnt
            out(i).oTimePt(k - 1).UserValue = tw(j).oTimePt(k).UserValue
          Next
          Exit For
        End If
      Next
    Next
    
  Else
    put_val errfile, "Can't find or open file " & FUIName
    put_line errfile
    close_csv errfile
    End
  End If
End Sub

Private Function addGlyph(srcid As String) As Long
Dim i As Long
  For i = 1 To mCnt
    If srcid = glyph(i).group Then
      addGlyph = i
      Exit Function
    End If
  Next
  mCnt = i
  addGlyph = i
  ReDim Preserve glyph(mCnt) As glyphtype
  glyph(i).group = srcid
End Function

Private Sub addId(srcid As String)
  glyph(addGlyph(srcid)).id = temp.pval
End Sub

Private Sub addLbl(srcid As String)
  glyph(addGlyph(srcid)).lbl = temp.pval
End Sub

Private Sub addPath(srcid As String)
  glyph(addGlyph(srcid)).Path = temp.pval
End Sub

Private Sub addorg(Index As Long)
  If Index > orgCnt Then
    orgCnt = Index
    ReDim Preserve org(orgCnt)
  End If
End Sub

Private Sub addcon()
  If temp.idx2 > cCnt Then
    cCnt = temp.idx2
    ReDim Preserve contam(cCnt) As constituent
  End If
  If temp.idx3 > contam(temp.idx2).numprog Then
    contam(temp.idx2).numprog = temp.idx3
    ReDim Preserve contam(temp.idx2).progeny(temp.idx3) As daughter
  End If
End Sub

Private Sub loadFUI()
Dim i As Long
Dim j As Long
Dim k As Long
Dim m As Long
Dim suCnt As Long
Dim CSMIndex As Long
Dim pName As String
Dim suCAS() As String
Dim xnode As ComctlLib.Node

  sCnt = 0
  mCnt = 0
  suCnt = 0
  If open_parm(fui, FUIName, 2) Then
    Do Until EOCF(fui.file)
      If read_parmrec(fui, temp) Then
        Select Case temp.pName
          Case Model
            For m = 1 To temp.idx1
              If read_parmrec(fui, temp) Then
                Select Case temp.pName
                  Case "suCAS":
                    If temp.idx1 > suCnt Then
                      ReDim Preserve suCAS(temp.idx1) As String
                      suCnt = temp.idx1
                    End If
                    suCAS(temp.idx1) = temp.pval
                End Select
              End If
            Next
          Case "fui"
            fuiCt = 0
            ReDim fuiData(temp.idx1)
            For m = 1 To temp.idx1
              If read_parmrec(fui, temp) Then
                If temp.idx1 = SiteIndex Then
                
                  fuiCt = fuiCt + 1
                  fuiData(fuiCt) = temp
                  
                  Select Case temp.pName
                    Case "clktype"
                      addcon
                      If temp.idx3 = 0 Then
                        contam(temp.idx2).typ = CInt(temp.pval)
                      Else
                        contam(temp.idx2).progeny(temp.idx3).typ = CInt(temp.pval)
                      End If
                    Case "fscname"
                      addcon
                      If temp.idx3 = 0 Then
                        contam(temp.idx2).name = temp.pval
                      Else
                        contam(temp.idx2).progeny(temp.idx3).name = temp.pval
                      End If
                    Case "fscasid"
                      addcon
                      If temp.idx3 = 0 Then
                        contam(temp.idx2).cas = temp.pval
                      Else
                        contam(temp.idx2).progeny(temp.idx3).cas = temp.pval
                      End If
                    Case Else 'grab module name, label and paths
                        If Len(temp.pName) > 3 Then
                          pName = Right(temp.pName, Len(temp.pName) - 3)
                          Select Case pName
                           Case "name":
                             addId Left$(temp.pName, 3) & CStr(temp.idx2)
                           Case "label":
                             addLbl Left$(temp.pName, 3) & CStr(temp.idx2)
                           Case "despath":
                             addPath Left$(temp.pName, 3) & CStr(temp.idx2)
                          End Select
                        End If
                   End Select
                End If
              End If
            Next
            ReDim Preserve fuiData(fuiCt)
          Case "csm"
            For m = 1 To temp.idx1
              If read_parmrec(fui, temp) Then
                If temp.idx1 = SiteIndex Then
                  Select Case temp.pName
                  Case "ModId"
                    If temp.pval = Model Then CSMIndex = temp.idx2
                  Case "ModSrcId"
                    If temp.idx2 = CSMIndex Then
                      If temp.idx3 > sCnt Then
                        sCnt = temp.idx3
                        ReDim Preserve susrc(sCnt) As glyphtype
                      End If
                      susrc(temp.idx3).id = temp.pval
                    End If
                  End Select
                End If
              End If
            Next
          Case Else
            For m = 1 To temp.idx1
              get_line fui.file
            Next
        End Select
      End If
    Loop

'resovle path and label by id and retrieve keys
    For j = 1 To mCnt
      For i = 1 To sCnt
        If susrc(i).id = glyph(j).id Then
          susrc(i).lbl = glyph(j).lbl
          susrc(i).Path = glyph(j).Path
          susrc(i).Idx = val(Right(glyph(j).group, Len(glyph(j).group) - 3))
        End If
      Next
'      'If Left(susrc(i).Id) = "con" Then loadchem
      If Left(glyph(j).id, 3) = "aos" Or Left(glyph(j).id, 3) = "ebf" Or Left(glyph(j).id, 3) = "tos" Then
        loadGidData glyph(j).id
        For k = 1 To gidCt
          Select Case gidData(k).pName
            Case "LifeFormName", "CommonName"
              addorg gidData(k).idx1
              org(gidData(k).idx1).name = gidData(k).pval
            Case "LifeFormSci", "ScientificName"
              addorg gidData(k).idx1
              org(gidData(k).idx1).id = gidData(k).pval
          End Select
        Next
      End If
    Next
    
    'Show and get selection here
    Load.List1.Clear
    For i = 1 To cCnt
      Load.List1.AddItem contam(i).name & "(" & contam(i).cas & ")"
'      Load.List1.AddItem contam(i).name
      For j = 1 To suCnt
        If contam(i).cas = suCAS(j) Then
          Load.List1.Selected(i - 1) = True
        End If
      Next
    Next
    Load.Hide
    Load.Caption = "Select constituents for analysis"
    Load.Height = 3500
    Load.Show vbModal
    
    Load.Caption = "Loading, Please Wait..."
    Load.Height = 480
    Load.Show
    
    ReDim out(0) As output
    For i = 1 To sCnt
      Set xnode = iotree.Nodes.Add("variables", tvwChild, "vv" & susrc(i).id, susrc(i).lbl & "(" & susrc(i).id & ")")
      Set xnode = iotree.Nodes.Add("outputs", tvwChild, "oo" & susrc(i).id, susrc(i).lbl & "(" & susrc(i).id & ")")
      loadDES susrc(i)
    Next
    ReDim Preserve glyph(1) As glyphtype
    ReDim Preserve out(oCnt) As output
    close_parm fui
    
    If oCnt = 0 And sCnt = 0 Then
      MsgBox "Nothing to do vary or monitor!"
      close_csv errfile
      Unload Me
      End
    End If
  
  Else
    put_val errfile, "Can't find or open file " & FUIName
    put_line errfile
    close_csv errfile
    End
  End If
End Sub

Private Sub form_Initialize()
  ReDim var(0) As variable
  ReDim vCache(0) As parmrec
  vCt = 0
End Sub

Private Sub Form_load()
Dim i As Long
Dim j As Long
Dim xnode As ComctlLib.Node
  
  FRAMES_INI = App.Path + "\\FramesUI.ini"
  getargs
  If argc >= 5 Then
    Load.Show
    FUIName = argv(0) & ".GID"
    RunName = argv(1)
    SiteIndex = val(argv(2))
    SUIndex = val(argv(3))
    Model = argv(4)
    Sens.Caption = "Sensitivity/Uncertainty Multimedia Modeling Module - " & Model
    If open_csv(errfile, RunName & ".ERR", 1) Then
      put_val errfile, "Error report for Sensitivity/Uncertainty Model"
      put_line errfile
    Else
      MsgBox "Unable to create file " & RunName & ".ERR" & Chr(10) & "Check directory permissions"
      End
    End If
    
    Set frm = Me
    SetHelpFile App.Path + "\sum3.htm"
    SetRefFile ReplaceExt(FUIName, "ref")
    load_convert
    initstuff
    loadFUI
    loadSU
    
    'scan for auto duplicates
    For i = 0 To vCnt - 1
      For j = i + 1 To vCnt - 1
        If var(i).vDes2 = var(j).vDes2 And var(i).vDup = -1 And var(i).vMod = var(j).vMod Then
          var(j).vDup = i
          var(j).vAlias = ""
        End If
      Next
    Next

'    For i = 0 To vCnt - 1
'      If var(i).vDup = -1 Then
'        List1.AddItem var(i).vAlias + Space(9 - Len(var(i).vAlias)) + "| " + var(i).vDes2
'        List1.ItemData(List1.NewIndex) = i
'      End If
'    Next
'    For i = 0 To oCnt - 1
'      List4.AddItem out(i).oAlias + Space(9 - Len(out(i).oAlias)) + "| " + out(i).oDes2
'      List4.ItemData(List4.NewIndex) = i
'    Next
    
    Load.Hide
    iotree.Nodes("variables").Selected = True
    Set xnode = iotree.SelectedItem
    iotree_nodeClick xnode
    Me.Show
    iotree.SetFocus
  Else
    MsgBox "Not enough arguments passed" & Chr(10) & "Contact PNNL"
    close_csv errfile
    End
  End If
End Sub

Private Sub addVarAlias()
Dim i As Long
Dim ix As Long
Dim cnt As Long

  If List1.ListIndex = -1 Then Exit Sub
  For i = 0 To vCnt - 1
    If var(i).vAlias = vTxt1.Text Then
      MsgBox "Variable alias must be unique!", 64, "Sensitivity Module"
'      vTxt1.SetFocus
      vTxt1.SelStart = 0
      vTxt1.SelLength = Len(vTxt1.Text)
      Exit For
    End If
  Next
  If i = vCnt Then
    ix = List1.ItemData(List1.ListIndex)
    var(ix).vAlias = vTxt1.Text
    List1.list(List1.ListIndex) = vTxt1.Text + Space(9 - Len(vTxt1.Text)) + "| " + var(ix).vDes2
    List1.tag = True
'    vTxt1.SetFocus
    vTxt1.SelStart = Len(vTxt1.Text)
    vTxt1.Enabled = False
    Command(0).Enabled = False
    Command(1).Enabled = True
  End If
End Sub

Private Sub delVarAlias()
Dim i As Long
Dim ix As Long
  
  If List1.ListIndex = -1 Then Exit Sub
  ix = List1.ItemData(List1.ListIndex)
  If var(ix).cnt = 0 And (var(ix).vEqu = "" Or var(ix).vDup > -1) Then
    var(ix).vAlias = ""
    List1.list(List1.ListIndex) = Space(9) + "| " + var(ix).vDes2
    List1.tag = True
'    For i = 0 To vCnt - 1
'      If var(i).vDes2 = var(ix).vDes2 Then
'        var(i).vAlias = ""
'        List1.list(i) = Space(9) + "| " + var(i).vDes2
'        var(i).vEqu = ""
'        var(i).vDup = -1
'        var(i).ref = 0
'      End If
'    Next
    vTxt1.Text = ""
    vTxt1.Enabled = True
    Command(0).Enabled = True
    Command(1).Enabled = False
  Else
    MsgBox "First delete all correlations and equations associated with this parameter.", vbCritical + vbOKOnly, Sens.Caption
  End If
End Sub

Private Sub addVarCor()
Dim i As Long
Dim j As Long
Dim k As Long
  If List2.ListIndex = -1 Then Exit Sub
  If val(pTxt2.Text) > 1 Or val(pTxt2.Text) < -1 Or val(pTxt2.Text) = 0 Then
    MsgBox "Correlation must be between -1 and 1, and can not be equal to 0", 16, "Correlation error"
    Exit Sub
  End If
'add correlation
  For i = 0 To var(List2.ItemData(List2.ListIndex)).cnt - 1
    If var(List2.ItemData(List2.ListIndex)).vCor(i).cAlias = alias.Text Then
      List3.list(i) = alias.Text + "  " + Str(val(pTxt2.Text))
      var(List2.ItemData(List2.ListIndex)).vCor(i).cAlias = alias.Text
      var(List2.ItemData(List2.ListIndex)).vCor(i).cFactor = val(pTxt2.Text)
      Exit For
    End If
  Next
  If i = var(List2.ItemData(List2.ListIndex)).cnt Then
    List3.AddItem alias.Text + "  " + Str(val(pTxt2.Text))
    ReDim Preserve var(List2.ItemData(List2.ListIndex)).vCor(List3.ListCount) As correlation
    var(List2.ItemData(List2.ListIndex)).vCor(List3.ListCount - 1).cAlias = alias.Text
    var(List2.ItemData(List2.ListIndex)).vCor(List3.ListCount - 1).cFactor = val(pTxt2.Text)
    var(List2.ItemData(List2.ListIndex)).cnt = List3.ListCount
  End If
'add reflection correlation
  For j = 0 To vCnt - 1
    If var(j).vAlias = alias.Text Then
      For k = 0 To var(j).cnt - 1
        If var(j).vCor(k).cAlias = var(List2.ItemData(List2.ListIndex)).vAlias Then
          var(j).vCor(i).cFactor = val(pTxt2.Text)
          Exit For
        End If
      Next
      If k = var(j).cnt Then
        ReDim Preserve var(j).vCor(var(j).cnt + 1) As correlation
        var(j).vCor(var(j).cnt).cAlias = var(List2.ItemData(List2.ListIndex)).vAlias
        var(j).vCor(var(j).cnt).cFactor = val(pTxt2.Text)
        var(j).cnt = var(j).cnt + 1
        Exit For
      End If
    End If
  Next
End Sub

Private Sub delVarCor()
Dim i As Long
Dim j As Long
Dim k As Long

  If List3.ListIndex = -1 Then Exit Sub
'delete reflection correlation
  For j = 0 To vCnt - 1
    If var(j).vAlias = var(List2.ItemData(List2.ListIndex)).vCor(List3.ListIndex).cAlias Then
      For k = 0 To var(j).cnt - 1
        If var(j).vCor(k).cAlias = var(List2.ItemData(List2.ListIndex)).vAlias Then
          For i = k To var(j).cnt - 1
            var(j).vCor(i) = var(j).vCor(i + 1)
          Next
          ReDim Preserve var(j).vCor(var(j).cnt) As correlation
          var(j).cnt = var(j).cnt - 1
          Exit For
        End If
      Next
    End If
  Next
'delete correlation
  j = List3.ListIndex
  List3.RemoveItem List3.ListIndex
  For i = j To j - 1
    var(List2.ItemData(List2.ListIndex)).vCor(i) = var(List2.ItemData(List2.ListIndex)).vCor(i + 1)
  Next
  ReDim Preserve var(List2.ItemData(List2.ListIndex)).vCor(List3.ListCount + 1) As correlation
  var(List2.ItemData(List2.ListIndex)).cnt = List3.ListCount
End Sub

Private Sub addVarEqu()
  If List2.ListIndex = -1 Then Exit Sub
  var(List2.ItemData(List2.ListIndex)).vEqu = pTxt3.Text
  pTxt4.Caption = pTxt3.Text
  If pTxt4.Caption = "" Then
    RefItem = -1
    noact = False
  Else
    RefItem = 6
    noact = True
  End If
End Sub

Private Sub delVarEqu()
  If List2.ListIndex = -1 Then Exit Sub
  var(List2.ItemData(List2.ListIndex)).vEqu = ""
  pTxt4.Caption = ""
  RefItem = -1
  noact = False
End Sub

Private Sub HideTime()
Dim i As Long, ix As Long
  ix = List4.ItemData(List4.ListIndex)
  For i = 0 To out(ix).oTimePtCnt - 1
    out(ix).oTimePt(i).UserValue = txt(i + 5).Text
    out(ix).oTimePt(i).UserUnit = unt(i + 5)
  Next
  For i = 5 To 14
    lbl(i).Visible = False
    txt(i).Visible = False
    unt(i).Visible = False
  Next
  yrCnt.Visible = False
  tPanel.Visible = False
  iotree.Enabled = True
  If oTxt1.Enabled Then oTxt1.SetFocus
  oTxt1.SelStart = Len(oTxt1.Text)
End Sub

Private Sub changeTime()
Dim i As Long, ix As Long
  ix = List4.ItemData(List4.ListIndex)
  Select Case out(ix).oTime
  Case t1(1): tPanel.Visible = True
              out(ix).oTimePt(0).tag = "OUTTIME"
              out(ix).oTimePt(1).tag = "OUTTIME"
              out(ix).oTimePt(0).Refresh
              out(ix).oTimePt(1).Refresh
              lbl(20).Caption = "Input years to average"
              Command(10).Visible = True
              iotree.Enabled = False
  Case t1(2): tPanel.Visible = True
              lbl(20).Caption = "Number of years to monitor"
              yrCnt.ListIndex = out(ix).oTimePtCnt - 1
              yrCnt.Visible = True
              For i = 0 To out(ix).oTimePtCnt - 1
                out(ix).oTimePt(i).tag = "OUTTIME"
                out(ix).oTimePt(i).Refresh
              Next
              Command(10).Visible = True
              iotree.Enabled = False
  End Select
End Sub
      
Private Function changeLoc()
Dim ix As Long
Dim x As Double
Dim y As Double
Dim valus
Dim temp As String
  
  changeLoc = False
'  Command(3).Enabled = False
  ix = List4.ItemData(List4.ListIndex)
  temp = InputBox("Please enter a location to monitor in meters " & vbCrLf & "Using spaces and the colon enter as -> northing : easting", "Air location ", out(ix).oOrgID)
  If out(ix).oOrgID <> "" And temp = "" Then
'    Command(3).Enabled = True
    changeLoc = True
    Exit Function
  ElseIf temp = "" Then
    Exit Function
  End If
  valus = Split(temp, ":")
  If UBound(valus) <= 1 Then
    On Error Resume Next
    y = CDbl(valus(0))
    If Err.Number <> 0 Then
      MsgBox "Location must be northing and easting in meters with spaces and the colon in between" & vbCrLf & "For example -> 12.45 : 67.89", 64, App.Title
      Exit Function
    End If
    Err.Clear
    x = CDbl(valus(1))
    If Err.Number <> 0 Then
      MsgBox "Location must be northing and easting in meters with spaces and the colon in between" & vbCrLf & "For example -> 12.45 : 67.89", 64, App.Title
      Exit Function
    End If
    On Error GoTo 0
  End If
  out(ix).oOrgID = temp
  Command(3).Caption = "Edit (" & out(ix).oOrgID & ")"
'  Command(3).Enabled = True
  changeLoc = True
End Function

Private Function addOutAlias() As Boolean
Dim i As Long
Dim ix As Long
Dim x As Double
Dim y As Double
Dim valus

  If List4.ListIndex = -1 Then Exit Function
  For i = 0 To oCnt - 1
    If out(i).oAlias = oTxt1.Text Then
      MsgBox "Output alias must be unique!", 64, App.Title
      oTxt1.SetFocus
      oTxt1.SelStart = 0
      oTxt1.SelLength = Len(oTxt1.Text)
      Command(8).Enabled = False
      addOutAlias = False
      Exit For
    End If
  Next
  
  If i = oCnt Then changeTime
  
  ix = List4.ItemData(List4.ListIndex)
  Select Case Left(out(ix).oType, 8)
  Case "concentr", "dry depo", "wet depo", "total de"
    If out(ix).oOrgID = "" Then
      changeLoc
      If out(ix).oOrgID = "" Then
        Command(8).Enabled = False
        addOutAlias = False
        Exit Function
      End If
    End If
  End Select
  
  out(ix).oAlias = oTxt1.Text
  List4.list(List4.ListIndex) = oTxt1.Text + Space(9 - Len(oTxt1.Text)) + "| " + out(ix).oDes2
  addOutAlias = True
End Function

Private Sub delOutAlias()
Dim ix As Long
  If List4.ListIndex = -1 Then Exit Sub
  ix = List4.ItemData(List4.ListIndex)
  out(ix).oAlias = ""
  out(ix).ref = 0
  If SSCheck2.value Then
    List4.RemoveItem List4.ListIndex
  Else
    List4.list(List4.ListIndex) = Space(9) + "| " + out(ix).oDes2
  End If
  List4.ListIndex = -1
  List4_Click
End Sub

Private Sub iotree_nodeClick(ByVal Node As ComctlLib.Node)
Dim i As Integer
Dim j As Integer
Dim found As Boolean

  If Node Is Nothing Then Exit Sub
  
  uFrame(0).Enabled = False
  uFrame(1).Enabled = False
  uFrame(2).Enabled = False
  
  found = False
  If List1.tag Then
    List1.tag = False
    List2.Clear
    alias.Clear
    For i = 0 To vCnt - 1
      If var(i).vAlias <> "" Then
        List2.AddItem var(i).vAlias & "  (" & var(i).vUpper.UserUnit & ")"
        List2.ItemData(List2.NewIndex) = i
        alias.AddItem var(i).vAlias
        found = True
      End If
    Next
    If found Then
      List2.ListIndex = 0
    Else
      distrib.ListIndex = 0
    End If
  End If

  Select Case Left(Node.key, 1)
    Case "v"
      uFrame(0).Caption = Node.Text
      SSCheck1_Click SSCheck1.value
      uFrame(0).Enabled = True
      uFrame(0).ZOrder 0
    Case "p"
      uFrame(1).Caption = Node.Text
      uFrame(1).Enabled = True
      uFrame(1).ZOrder 0
    Case "o"
      uFrame(2).Caption = Node.Text
      SSCheck2_Click SSCheck2.value
      uFrame(2).Enabled = True
      uFrame(2).ZOrder 0
  End Select
End Sub

Private Sub List1_Click()
  vTxt2.Text = ""
  vTxt1.Text = ""
  RefItem = 3
  ref(3).Caption = "Ref: 0"
  ref(3).tag = 0
  If List1.ListIndex >= 0 Then
    vTxt2.Text = var(List1.ItemData(List1.ListIndex)).vDes2 ' BLH 12/00
    vTxt1.Text = var(List1.ItemData(List1.ListIndex)).vAlias ' BLH 12/00
    ref(3).Caption = "Ref: " & CStr(var(List1.ItemData(List1.ListIndex)).ref)
    ref(3).tag = var(List1.ItemData(List1.ListIndex)).ref
  End If

  vTxt1.Enabled = vTxt1.Text = ""
  noact.Enabled = vTxt1.Text <> ""
  Command(0).Enabled = False
  Command(1).Enabled = vTxt1.Text <> ""
End Sub

Private Sub List1_DblClick()
  List1_Click
End Sub

Private Sub List2_Click()
Dim i As Long

  If distrib.ListIndex <> var(List2.ItemData(List2.ListIndex)).vDistrib Then
    distrib.ListIndex = var(List2.ItemData(List2.ListIndex)).vDistrib
  Else
    distrib_Click
  End If
  ref(4).Caption = "Ref: " & CStr(var(List2.ItemData(List2.ListIndex)).dref)
  ref(5).Caption = "Ref: "
  ref(6).Caption = "Ref: " & CStr(var(List2.ItemData(List2.ListIndex)).eref)
  ref(4).tag = var(List2.ItemData(List2.ListIndex)).dref
  ref(5).tag = 0
  ref(6).tag = var(List2.ItemData(List2.ListIndex)).eref
  pTxt2.Text = ""
  pTxt3.Text = var(List2.ItemData(List2.ListIndex)).vEqu
  pTxt4.Caption = var(List2.ItemData(List2.ListIndex)).vEqu
  alias.Clear
  For i = 0 To vCnt - 1
    If var(i).vAlias <> "" And var(i).vAlias <> var(List2.ItemData(List2.ListIndex)).vAlias Then
      alias.AddItem var(i).vAlias
    End If
  Next
  List3.Clear
  For i = 0 To var(List2.ItemData(List2.ListIndex)).cnt - 1
    List3.AddItem var(List2.ItemData(List2.ListIndex)).vCor(i).cAlias + "  " + Str(var(List2.ItemData(List2.ListIndex)).vCor(i).cFactor)
  Next
  
  If List3.ListCount > 0 Then List3.ListIndex = 0
  RefItem = SSTab3.Tab + 4
  Select Case SSTab3.Tab
  Case 0: noact.Enabled = True
  Case 1: If List3.ListCount > 0 Then noact.Enabled = False
  Case 2: noact.Enabled = pTxt3.Text <> ""
  End Select
End Sub

Private Sub List2_DblClick()
  List2_Click
End Sub

Private Sub List3_Click()
  set_unit alias, var(List2.ItemData(List2.ListIndex)).vCor(List3.ListIndex).cAlias
  pTxt2.Text = CStr(var(List2.ItemData(List2.ListIndex)).vCor(List3.ListIndex).cFactor)
  
  RefItem = 5
  noact.Enabled = True
  ref(5).Caption = "Ref: " & CStr(var(List2.ItemData(List2.ListIndex)).vCor(List3.ListIndex).ref)
  ref(5).tag = var(List2.ItemData(List2.ListIndex)).vCor(List3.ListIndex).ref
End Sub

Private Sub List3_dblClick()
  List3_Click
End Sub

Private Sub List4_Click()
Dim ix As Long, vis As Boolean
  oTxt2.Text = ""
  oTxt1.Text = ""
  RefItem = 2
  ref(2).Caption = "Ref: 0"
  If List4.ListIndex >= 0 Then
    ix = List4.ItemData(List4.ListIndex)
    oTxt2.Text = out(ix).oDes2
    oTxt1.Text = out(ix).oAlias
    ref(2).Caption = "Ref: " & CStr(out(ix).ref)
    ref(2).tag = out(ix).ref
  End If
  oTxt1.Enabled = oTxt1 = ""
  noact.Enabled = oTxt1 <> ""
  
  Command(8).Enabled = False              ' add
  Command(9).Enabled = oTxt1.Text <> ""   ' delete
  Command(10).Visible = False             ' edit time
  If oTxt1.Text <> "" And List4.ListIndex >= 0 Then
    Command(10).Visible = (out(ix).oTime <> t1(0))
    If Command(10).Visible Then
      Command(10).Enabled = True
    End If
  End If
  Command(3).Visible = False              'edit orgid  -> location, dep type, organism ...
  If oTxt1.Text <> "" And List4.ListIndex >= 0 Then
    Select Case Left(out(ix).oType, 8)
      Case "concentr", "dry depo", "wet depo", "total de"
        Command(3).Visible = (out(ix).oOrgID <> "")
    End Select
    If Command(3).Visible Then
      Command(3).Caption = "Edit (" & out(ix).oOrgID & ")"
      Command(3).Enabled = True
    End If
  End If
End Sub

Private Sub List4_DblClick()
  List4_Click
End Sub

Private Sub Command_Click(Index As Integer)
  Select Case Index
  Case 0: addVarAlias
          List1_Click
  Case 1: delVarAlias
  Case 2: HideTime
          List4_Click
  Case 3: changeLoc
  Case 4: addVarCor
  Case 5: delVarCor
  Case 6: addVarEqu
  Case 7: delVarEqu
  Case 8: If addOutAlias() And (Not tPanel.Visible) Then List4_Click
  Case 9: delOutAlias
  Case 10: changeTime
  End Select
End Sub

Private Sub mExit_Click()
  close_csv errfile
  Kill RunName & ".ERR"
  End
End Sub

Private Sub WriteIt(fle As parmfile, t As tFloat, i As Long, j As Long)
Dim parm As parmrec
  set_parm parm, t.name, i, j, 0, 0, 0, 0, 0, t.UserUnit, t.DefaultUnit, convert(t.UserUnit, t.DefaultUnit, t.UserValue)
  write_parmrec fle, parm
End Sub

Private Sub WriteItLog(fle As parmfile, t As tFloat, i As Long, j As Long, valu As String, ref As Long)
Dim parm As parmrec
  set_parm parm, t.name, i, j, 0, 0, 0, 0, ref, t.UserUnit, t.DefaultUnit, valu
  write_parmrec fle, parm
End Sub

Private Function writeVar(fle As parmfile, i As Long, j As Long) As Boolean
Dim valu As Double
Dim myparm As parmrec
Dim k As Long

  writeVar = False
  If var(i).vDistrib = 0 And var(i).vEqu = "" Then
    put_val errfile, "Variable " + var(i).vAlias + " must have a distribution"
    put_line errfile
    put_val errfile, "or an equation defined"
    put_line errfile
    writeVar = True
  End If
  If InStr(var(i).vEqu, var(i).vAlias) > 0 And var(i).vDistrib = 0 Then
    put_val errfile, "Variable " + var(i).vAlias + "'s equation contains"
    put_line errfile
    put_val errfile, "itself and therefore must have a distribution defined"
    put_line errfile
    writeVar = True
  End If
  set_parm myparm, "suName", j, 0, 0, 0, 0, 0, 0, "N/A", "N/A", var(i).vName
  write_parmrec fle, myparm
  myparm.ref = var(i).ref
  myparm.pName = "suAlias"
  myparm.pval = var(i).vAlias
  write_parmrec fle, myparm
  myparm.ref = 0
  myparm.pName = "suDes"
  myparm.pval = var(i).vDes
  write_parmrec fle, myparm
  myparm.pName = "suExe"
  myparm.pval = var(i).vExe
  write_parmrec fle, myparm
  myparm.pName = "suFmt"
  myparm.pval = var(i).vFmt
  write_parmrec fle, myparm
  myparm.pName = "suMod"
  myparm.pval = var(i).vMod
  write_parmrec fle, myparm
  myparm.pName = "suCorNum"
  myparm.pval = CStr(var(i).cnt)
  write_parmrec fle, myparm
  For k = 0 To 5
    myparm.pName = "suIndex" + CStr(k + 1)
    myparm.pval = CStr(var(i).Idx(k))
    write_parmrec fle, myparm
  Next
  myparm.pName = "suDistrib"
  myparm.ref = var(i).dref
  myparm.pval = distrib.list(var(i).vDistrib)
  write_parmrec fle, myparm
  myparm.ref = 0
  myparm.pName = "suBase"
  myparm.pval = CStr(var(i).vBase)
  write_parmrec fle, myparm
  myparm.pName = "suEqu"
  myparm.pval = var(i).vEqu
  myparm.ref = var(i).eref
  write_parmrec fle, myparm
  For k = 0 To var(i).cnt - 1
    myparm.pName = "suCorAlias"
    myparm.idx2 = CStr(k + 1)
    myparm.ref = var(i).vCor(k).ref
    myparm.pval = var(i).vCor(k).cAlias
    write_parmrec fle, myparm
    myparm.ref = 0
    myparm.pName = "suCor"
    myparm.pval = CStr(var(i).vCor(k).cFactor)
    write_parmrec fle, myparm
  Next
  WriteIt fle, var(i).vUpper, j, 0
  WriteIt fle, var(i).vLower, j, 0
  If var(i).vDistrib = 4 Then
    valu = val(convertLog(var(i).vMean.UserUnit, var(i).vMean.DefaultUnit, var(i).vMean.UserValue, var(i).vBase))
    WriteItLog fle, var(i).vMean, j, 0, CStr(valu), 0
    valu = val(convertLog(var(i).vStd.UserUnit, var(i).vStd.DefaultUnit, var(i).vStd.UserValue, var(i).vBase))
    If valu >= 0 Then
      WriteItLog fle, var(i).vStd, j, 0, CStr(valu), 0
    Else
      WriteItLog fle, var(i).vStd, j, 0, CStr(valu), -1
    End If
  Else
    WriteIt fle, var(i).vMean, j, 0
    WriteIt fle, var(i).vStd, j, 0
  End If
  WriteIt fle, var(i).vScal, j, 0
  WriteIt fle, var(i).vShift, j, 0
  WriteIt fle, var(i).vMode, j, 0
End Function

Private Function writeOut(fle As parmfile, i As Long, j As Long) As Boolean
Dim myparm As parmrec
Dim k As Long

  writeOut = False
  set_parm myparm, "suOutAlias", j, 0, 0, 0, 0, 0, out(i).ref, "N/A", "N/A", out(i).oAlias
  write_parmrec fle, myparm
  
  myparm.ref = 0
  myparm.pName = "suOutDes"
  myparm.pval = out(i).oDes
  write_parmrec fle, myparm
  
  myparm.pName = "suOutExt"
  myparm.pval = out(i).oExt
  write_parmrec fle, myparm
  
  myparm.pName = "suOutType"
  myparm.pval = out(i).oType
  write_parmrec fle, myparm
  
  myparm.pName = "suOutSourceID"
  myparm.pval = out(i).oSourceID
  write_parmrec fle, myparm

  myparm.pName = "suOutCASID"
  myparm.pval = out(i).oCASID
  write_parmrec fle, myparm

  myparm.pName = "suOutParentID"
  myparm.pval = out(i).oParentID
  write_parmrec fle, myparm

  myparm.pName = "suOutOrgID"
  myparm.pval = out(i).oOrgID
  write_parmrec fle, myparm
  
  myparm.pName = "suOutTime"
  myparm.pval = out(i).oTime
  write_parmrec fle, myparm
  
  myparm.pName = "suOutTimePtCnt"
  myparm.pval = CStr(out(i).oTimePtCnt)
  write_parmrec fle, myparm
  For k = 1 To out(i).oTimePtCnt
    WriteIt fle, out(i).oTimePt(k - 1), j, k
  Next
End Function

Private Sub mSave_Click()
Dim i As Long
Dim j As Long
Dim cnt As Long
Dim genflag As Boolean
Dim fName As String
Dim fle As parmfile
Dim myparm As parmrec

  cnt = 0
  For i = 0 To vCnt - 1
    If var(i).vDup > -1 Then
      If var(var(i).vDup).vAlias <> "" Then
        cnt = cnt + 1
        var(i).vAlias = "~auto" + CStr(cnt)
        var(i).vEqu = var(var(i).vDup).vAlias
      End If
    End If
  Next
  
  genflag = False
  fName = RunName & ".GID"
  If open_parm(fle, fName, 1) Then
  
    If suSeed.UserValue < 0 Then
      genflag = True
      put_val errfile, "Seed must be positive!"
      put_line errfile
    End If
    set_parm myparm, "suSeed", 0, 0, 0, 0, 0, 0, suSeed.Reference, "N/A", "N/A", suSeed.pval
    write_parmrec fle, myparm
    
    If startTime.UserValue < 0 Or startTime.UserValue > 23 Then
      genflag = True
      put_val errfile, "Start time is  or 0-23!"
      put_line errfile
    End If
    set_parm myparm, "startTime", 0, 0, 0, 0, 0, 0, startTime.Reference, "N/A", "N/A", startTime.pval
    write_parmrec fle, myparm
    If (Check2.value = 0) Then
      set_parm myparm, "noDelay", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "F"
    Else
      set_parm myparm, "noDelay", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "T"
    End If
    write_parmrec fle, myparm
  
    j = 0
    For i = 1 To cCnt
      If Load.List1.Selected(i - 1) Then
        j = j + 1
        set_parm myparm, "suCAS", j, 0, 0, 0, 0, 0, 0, "N/A", "N/A", contam(i).cas
        write_parmrec fle, myparm
      End If
    Next
  
    j = 0
    For i = 0 To vCnt - 1
      If var(i).vAlias <> "" Then
        j = j + 1
        If writeVar(fle, i, j) Then genflag = True
      End If
    Next
    set_parm myparm, "suVarNum", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CStr(j)
    write_parmrec fle, myparm
    
    
    If (suIter.UserValue + cnt <= j) Then
        suIter.UserValue = j + 1 ' just set it!
        suIter.Refresh
        MsgBox "suIter must be greater than the number of defined variables" & vbCrLf & "suIter set to " & suIter.pval
    End If
    set_parm myparm, "suIter", 0, 0, 0, 0, 0, 0, suIter.Reference, "N/A", "N/A", suIter.pval
    write_parmrec fle, myparm

    j = 0
    For i = 0 To oCnt - 1
      If out(i).oAlias <> "" Then
        j = j + 1
        If writeOut(fle, i, j) Then
          genflag = True
          put_val errfile, "Error in " + out(i).oAlias
        End If
      End If
    Next
    set_parm myparm, "suOutNum", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CStr(j)
    write_parmrec fle, myparm
    close_parm fle
    If genflag Then
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

Private Sub oTxt1_Change()
  Command(8).Enabled = oTxt1 <> ""
  Command(9).Enabled = False
End Sub

Private Sub SSCheck1_Click(value As Integer)
Dim i As Integer
Dim pos As Long
Dim key As String
Dim subkey As String
Dim Node As ComctlLib.Node

  Set Node = iotree.SelectedItem
  key = Right(Node.key, Len(Node.key) - 2)
  pos = InStr(key, ",")
  subkey = Right(key, Len(key) - pos)
'  key = Left(key, pos - 1)
  List1.Clear
  For i = 0 To vCnt - 1
    If var(i).vDup = -1 And _
        ((Node.key = "variables") Or _
         (var(i).vMod = key) Or _
         ((var(i).vMod & "," & subkey = key) And (0 < InStr(var(i).vDes, subkey)))) Then
      If value Then
        If var(i).vAlias <> "" Then
          If Node.key = "variables" Then
            List1.AddItem var(i).vAlias + Space(9 - Len(var(i).vAlias)) + "| " + var(i).vMod + " - " + var(i).vDes2
          Else
            List1.AddItem var(i).vAlias + Space(9 - Len(var(i).vAlias)) + "| " + var(i).vDes2
          End If
          List1.ItemData(List1.NewIndex) = i
        End If
      Else
        If Node.key = "variables" Then
          List1.AddItem var(i).vAlias + Space(9 - Len(var(i).vAlias)) + "| " + var(i).vMod + " - " + var(i).vDes2
        Else
          List1.AddItem var(i).vAlias + Space(9 - Len(var(i).vAlias)) + "| " + var(i).vDes2
        End If
        List1.ItemData(List1.NewIndex) = i
      End If
    Else
    'nothing
    End If
  Next
  List1.ListIndex = -1
  List1_Click
End Sub

Private Sub SSCheck2_Click(value As Integer)
Dim i As Integer
Dim pos As Long
Dim key As String
Dim subkey As String
Dim Node As ComctlLib.Node
  
  Set Node = iotree.SelectedItem
  key = Right(Node.key, Len(Node.key) - 2)
  pos = InStr(key, ",")
  subkey = Right(key, Len(key) - pos)
'  key = Left(key, pos - 1)
  List4.Clear
  For i = 0 To oCnt - 1
    If ((Node.key = "outputs") Or _
        (out(i).oSourceID = key) Or _
        ((out(i).oSourceID & "," & subkey = key) And (out(i).oCASID = subkey)) Or _
        ((out(i).oSourceID & "," & subkey = key) And (out(i).oOrgID = subkey))) Then
      If value Then
        If out(i).oAlias <> "" Then
          If Node.key = "outputs" Then
            List4.AddItem out(i).oAlias + Space(9 - Len(out(i).oAlias)) + "| " + out(i).oSourceID + " - " + out(i).oDes2
          Else
            List4.AddItem out(i).oAlias + Space(9 - Len(out(i).oAlias)) + "| " + out(i).oDes2
          End If
          List4.ItemData(List4.NewIndex) = i
        End If
      Else
        If Node.key = "outputs" Then
          List4.AddItem out(i).oAlias + Space(9 - Len(out(i).oAlias)) + "| " + out(i).oSourceID + " - " + out(i).oDes2
        Else
          List4.AddItem out(i).oAlias + Space(9 - Len(out(i).oAlias)) + "| " + out(i).oDes2
        End If
        List4.ItemData(List4.NewIndex) = i
      End If
    Else
    'nothing
    End If
  Next
  List4.ListIndex = -1
  List4_Click
End Sub

Private Sub SSTab3_Click(PreviousTab As Integer)
  SSTab3_GotFocus
End Sub

Private Sub SSTab3_DblClick()
  SSTab3_GotFocus
End Sub

Private Sub txt_Change(Index As Integer)
Dim i As Integer
Dim ul As Double
Dim uv As Double
Dim hold As Double
  
  If List2.ListIndex = -1 Then Exit Sub
  i = List2.ItemData(List2.ListIndex)
  Select Case Index
  Case 0
    var(i).vUpper.Change
    SetMeanBounds i
  Case 1
    var(i).vLower.Change
    SetMeanBounds i
  Case 2
    Select Case distrib.ListIndex
    Case 3, 5, 7, 8
     var(i).vMean.Change
    Case 4
     var(i).vMean.Change False
    Case 6
     var(i).vMode.Change
    End Select
  Case 3
    Select Case distrib.ListIndex
    Case 3, 4, 8
     var(i).vStd.Change
    Case 5
     var(i).vScal.Change
    Case 7
     var(i).vShift.Change
    End Select
  Case 15: suSeed.Change
  Case 16: suIter.Change
  Case 17: startTime.Change
  End Select
End Sub

Private Sub unt_Click(Index As Integer)
Dim i As Integer

' If Index > 5 Then out(List4.ListIndex).oTimePt(Index - 6).Click
  If Index > 4 Then out(List4.ItemData(List4.ListIndex)).oTimePt(Index - 5).Click
  If List2.ListIndex = -1 Then Exit Sub
  i = List2.ItemData(List2.ListIndex)
  Select Case Index
  Case 0
    var(i).vUpper.Click
    SetMeanBounds i
  Case 1
    var(i).vLower.Click
    SetMeanBounds i
  Case 2
    Select Case distrib.ListIndex
    Case 3, 5, 7, 8
     var(i).vMean.Click
     SetMeanBounds i
    Case 4
     var(i).vMean.Click
     SetMeanBounds i
    Case 6
     var(i).vMode.Click
    End Select
  Case 3
    Select Case distrib.ListIndex
    Case 3, 4, 8
     var(i).vStd.Click
   Case 5
     var(i).vScal.Click
    Case 7
     var(i).vShift.Click
    End Select
  Case 15: suSeed.Click
  Case 16: suIter.Click
  Case 17: startTime.Click
  End Select
End Sub

Private Sub base_Click()
  If List2.ListIndex = -1 Then Exit Sub
  var(List2.ItemData(List2.ListIndex)).vBase = base.ListIndex
  SetMeanBounds List2.ItemData(List2.ListIndex)
End Sub

Private Sub Hideit()
Dim i As Long
  For i = 0 To 3
    lbl(i).Visible = False
    txt(i).Visible = False
    unt(i).Visible = False
  Next
  lbl(4).Visible = False
  base.Visible = False
End Sub
      
Function convertLog(from As String, too As String, valu As Double, base As Long) As String
Dim rval As Double
Dim pval As Double
Dim nval As Double
   

  If from = too Then
    rval = valu
  Else
    On Error GoTo LOGERROR
    rval = 0
    If base = 0 Then
      pval = 10 ^ valu
    Else
      pval = exp(valu)
    End If
    nval = val(convert(from, too, pval))
    rval = mylog(nval, base)
  End If
LOGERROR:
  convertLog = CStr(rval)
End Function
      
Function mylog(valu As Double, base As Long) As Double
  If valu < 1E-30 Then
    mylog = -30
  Else
    If base = 0 Then
      mylog = Log(valu) / Log(10#)
    Else
      mylog = Log(valu)
    End If
  End If
End Function

Sub SetMeanBounds(j As Integer)
  Select Case distrib.ListIndex
  Case 3, 5, 7, 8
    var(j).vMean.lower = val(convert(var(j).vLower.UserUnit, var(j).vMean.UserUnit, val(var(j).vLower.UserValue)))  'conversion
    var(j).vMean.upper = val(convert(var(j).vUpper.UserUnit, var(j).vMean.UserUnit, val(var(j).vUpper.UserValue)))  'conversion
    If okToChange Then var(j).vMean.Change
  Case 4
    var(j).vMean.lower = mylog(val(convert(var(j).vLower.UserUnit, var(j).vMean.UserUnit, val(var(j).vLower.UserValue))), var(j).vBase)  'conversion
    var(j).vMean.upper = mylog(val(convert(var(j).vUpper.UserUnit, var(j).vMean.UserUnit, val(var(j).vUpper.UserValue))), var(j).vBase)  'conversion
    If okToChange Then var(j).vMean.Change False
  End Select

'  If (var(j).vMean.vLower - 6 * Abs(vStd)) < var(j).vMean.UserValue And _
'      var(j).vMean.UserValue < (var(j).vMean.vUpper + 6 * Abs(vStd)) Then
'    var(j).vMean.Color = GREEN
'  Else
'    var(j).vMean.Color = RED
'  End If
End Sub

Private Sub distrib_Click()
Dim i As Integer
' removed from the list of possible distribution types are
' Exponential, Triangular, Gamma, Beta
  
  Hideit
  If List2.ListIndex = -1 Then Exit Sub
  i = List2.ItemData(List2.ListIndex)
  pTxt1.Text = var(i).vDes2
  If distrib.ListIndex > 0 Then
    okToChange = False
    var(i).vLower.Refresh
    var(i).vUpper.Refresh
    Select Case distrib.ListIndex
    Case 2
      lbl(4).Visible = True
      base.Visible = True
      base.ListIndex = var(i).vBase
      lbl(2).Caption = lbl(2).Caption + " log"
      lbl(3).Caption = lbl(3).Caption + " log"
    Case 3, 8
      var(i).vMean.Refresh
      var(i).vStd.Refresh
    Case 4
      var(i).vMean.Refresh False
      var(i).vStd.Refresh
      lbl(4).Visible = True
      base.Visible = True
      base.ListIndex = var(i).vBase
      lbl(2).Caption = lbl(2).Caption + " log"
      lbl(3).Caption = lbl(3).Caption + " log"
    Case 5
      var(i).vMean.Refresh
      var(i).vScal.Refresh
    Case 6
      var(i).vMode.Refresh
    Case 7
      var(i).vMean.Refresh
      var(i).vShift.Refresh
    End Select
    okToChange = True
  End If
  var(i).vDistrib = distrib.ListIndex
End Sub

Private Sub unt_GotFocus(Index As Integer)
  Select Case Index
  Case 15, 16:
    RefItem = Index
    noact = True
  Case 0, 1, 2, 3:
    RefItem = 4
    noact = True
  End Select
  noact.Enabled = True
  HelpAnchor = txt(Index).tag
End Sub

Private Sub vTxt1_Change()
  Command(0).Enabled = vTxt1 <> ""
  Command(1).Enabled = False
End Sub

Private Sub yrCnt_Click()
Dim i As Long, ix As Long
  ix = List4.ItemData(List4.ListIndex)
  out(ix).oTimePtCnt = yrCnt.ListIndex + 1
  For i = 0 To yrCnt.ListIndex
    out(ix).oTimePt(i).Refresh
  Next
  For i = yrCnt.ListIndex + 6 To 14
    lbl(i).Visible = False
    txt(i).Visible = False
    unt(i).Visible = False
  Next
End Sub

Private Sub vTxt1_GotFocus()
  HelpAnchor = vTxt1.tag
End Sub

Private Sub pTxt1_GotFocus()
  HelpAnchor = pTxt1.tag
End Sub

Private Sub pTxt2_GotFocus()
  HelpAnchor = pTxt2.tag
End Sub

Private Sub pTxt3_GotFocus()
  HelpAnchor = pTxt3.tag
End Sub

Private Sub pTxt4_GotFocus()
  HelpAnchor = pTxt4.tag
End Sub

Private Sub oTxt1_GotFocus()
  HelpAnchor = oTxt1.tag
End Sub

Private Sub Command_GotFocus(Index As Integer)
  If Index <> 6 And Index <> 7 Then
    RefItem = -1
    noact = False
  Else
    If pTxt4.Caption = "" Then
      RefItem = -1
      noact = False
    End If
  End If
  HelpAnchor = Command(Index).tag
End Sub

Private Sub txt_GotFocus(Index As Integer)
  Select Case Index
  Case 15, 16:
    RefItem = Index
    noact = True
    noact.Enabled = True
  Case 0, 1, 2, 3:
    RefItem = 4
    noact.Enabled = True
  Case Else
'  Case 6, 7, 8, 9, 10:
    RefItem = -1
    noact.Enabled = False
  End Select
  HelpAnchor = txt(Index).tag
End Sub

Private Sub SSCheck1_GotFocus()
  HelpAnchor = SSCheck1.tag
End Sub

Private Sub SSCheck2_GotFocus()
  HelpAnchor = SSCheck2.tag
End Sub

Private Sub distrib_GotFocus()
  RefItem = 4
  noact = True
  HelpAnchor = distrib.tag
End Sub

Private Sub alias_GotFocus()
  HelpAnchor = alias.tag
End Sub

Private Sub base_GotFocus()
  RefItem = 4
  noact = True
  HelpAnchor = base.tag
End Sub

Private Sub List1_GotFocus()
  iotree_GotFocus
End Sub

Private Sub List2_GotFocus()
  HelpAnchor = List2.tag
End Sub

Private Sub List3_GotFocus()
  HelpAnchor = List3.tag
End Sub

Private Sub List4_GotFocus()
  iotree_GotFocus
End Sub

Private Sub yrCnt_GotFocus()
  RefItem = -1
  noact = False
End Sub

Private Sub SSTab3_GotFocus()
  Select Case SSTab3.Tab
  Case 0: HelpAnchor = "DIST"
  RefItem = 4
  noact = True
  Case 1: HelpAnchor = "COR"
  RefItem = 5
  noact = alias.Text <> ""
  Case 2: HelpAnchor = "EQU"
  RefItem = 6
  noact = pTxt3.Text <> ""
  End Select
End Sub

Private Sub iotree_GotFocus()
Dim Node As ComctlLib.Node
  RefItem = -1
  noact = False
  Set Node = iotree.SelectedItem
  Select Case Left(Node.key, 1)
  Case "v": HelpAnchor = "VAR"
  Case "p": HelpAnchor = "PARAM"
  Case "o": HelpAnchor = "OUT"
  End Select
End Sub

Private Sub form_KeyPress(KeyAscii As Integer)
  If KeyAscii = 13 Then KeyAscii = 0
End Sub

Private Sub form_KeyDown(KeyCode As Integer, Shift As Integer)
  Select Case KeyCode
  Case vbKeyF1:
    KeyCode = 0
    GetHelp
'  Case vbKeyUp:
'    KeyCode = 0
'    SendKeys "+{TAB}"
'  Case vbKeyDown:
'    KeyCode = 0
'    SendKeys "{TAB}"
  Case vbKeyReturn:
    KeyCode = 0
    SendKeys "{TAB}"
  End Select
End Sub
