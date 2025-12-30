VERSION 5.00
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "TABCTL32.OCX"
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "THREED32.OCX"
Begin VB.Form gen_exp 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Form1"
   ClientHeight    =   5760
   ClientLeft      =   4110
   ClientTop       =   2850
   ClientWidth     =   7680
   Icon            =   "GEN_EXP.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   384
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   512
   StartUpPosition =   2  'CenterScreen
   Begin TabDlg.SSTab SSTab1 
      Height          =   5775
      Index           =   0
      Left            =   0
      TabIndex        =   0
      TabStop         =   0   'False
      Top             =   0
      Width           =   7695
      _ExtentX        =   13573
      _ExtentY        =   10186
      _Version        =   393216
      Style           =   1
      Tabs            =   6
      TabsPerRow      =   6
      TabHeight       =   423
      TabCaption(0)   =   "Controls"
      TabPicture(0)   =   "GEN_EXP.frx":08CA
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "frm(0)"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).ControlCount=   1
      TabCaption(1)   =   "Water"
      TabPicture(1)   =   "GEN_EXP.frx":08E6
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "SSTab1(1)"
      Tab(1).Control(0).Enabled=   0   'False
      Tab(1).ControlCount=   1
      TabCaption(2)   =   "Soil"
      TabPicture(2)   =   "GEN_EXP.frx":0902
      Tab(2).ControlEnabled=   0   'False
      Tab(2).Control(0)=   "SSTab1(2)"
      Tab(2).Control(0).Enabled=   0   'False
      Tab(2).ControlCount=   1
      TabCaption(3)   =   "Air"
      TabPicture(3)   =   "GEN_EXP.frx":091E
      Tab(3).ControlEnabled=   0   'False
      Tab(3).Control(0)=   "frm(3)"
      Tab(3).Control(0).Enabled=   0   'False
      Tab(3).ControlCount=   1
      TabCaption(4)   =   "Agriculture"
      TabPicture(4)   =   "GEN_EXP.frx":093A
      Tab(4).ControlEnabled=   0   'False
      Tab(4).Control(0)=   "SSTab1(3)"
      Tab(4).ControlCount=   1
      TabCaption(5)   =   "Pathways"
      TabPicture(5)   =   "GEN_EXP.frx":0956
      Tab(5).ControlEnabled=   0   'False
      Tab(5).Control(0)=   "frm(5)"
      Tab(5).ControlCount=   1
      Begin TabDlg.SSTab SSTab1 
         Height          =   5292
         Index           =   1
         Left            =   -74880
         TabIndex        =   133
         TabStop         =   0   'False
         Top             =   360
         Width           =   7452
         _ExtentX        =   13150
         _ExtentY        =   9340
         _Version        =   393216
         Style           =   1
         Tabs            =   5
         TabsPerRow      =   5
         TabHeight       =   423
         Enabled         =   0   'False
         TabCaption(0)   =   "General"
         TabPicture(0)   =   "GEN_EXP.frx":0972
         Tab(0).ControlEnabled=   -1  'True
         Tab(0).Control(0)=   "wt(0)"
         Tab(0).Control(0).Enabled=   0   'False
         Tab(0).ControlCount=   1
         TabCaption(1)   =   "Animal water"
         TabPicture(1)   =   "GEN_EXP.frx":098E
         Tab(1).ControlEnabled=   0   'False
         Tab(1).Control(0)=   "wt(1)"
         Tab(1).ControlCount=   1
         TabCaption(2)   =   "Irrigation sources"
         TabPicture(2)   =   "GEN_EXP.frx":09AA
         Tab(2).ControlEnabled=   0   'False
         Tab(2).Control(0)=   "wt(2)"
         Tab(2).ControlCount=   1
         TabCaption(3)   =   "Irrigation rates"
         TabPicture(3)   =   "GEN_EXP.frx":09C6
         Tab(3).ControlEnabled=   0   'False
         Tab(3).Control(0)=   "wt(3)"
         Tab(3).ControlCount=   1
         TabCaption(4)   =   "Irrigation times"
         TabPicture(4)   =   "GEN_EXP.frx":09E2
         Tab(4).ControlEnabled=   0   'False
         Tab(4).Control(0)=   "wt(4)"
         Tab(4).ControlCount=   1
         Begin Threed.SSFrame wt 
            Height          =   4692
            Index           =   0
            Left            =   240
            TabIndex        =   134
            Top             =   360
            Width           =   6972
            _Version        =   65536
            _ExtentX        =   12298
            _ExtentY        =   8276
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
            Enabled         =   0   'False
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   90
               Left            =   4680
               Style           =   2  'Dropdown List
               TabIndex        =   113
               Top             =   4080
               Width           =   852
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   90
               Left            =   3840
               TabIndex        =   112
               Top             =   4080
               Width           =   852
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   84
               Left            =   4680
               Style           =   2  'Dropdown List
               TabIndex        =   109
               Top             =   3240
               Width           =   850
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   84
               Left            =   3840
               TabIndex        =   108
               Text            =   "0.1"
               Top             =   3240
               Width           =   850
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   83
               Left            =   4680
               Style           =   2  'Dropdown List
               TabIndex        =   107
               Top             =   2880
               Width           =   850
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   83
               Left            =   3840
               TabIndex        =   106
               Text            =   "0.0"
               Top             =   2880
               Width           =   850
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   67
               Left            =   3840
               TabIndex        =   110
               Top             =   3720
               Width           =   850
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   67
               Left            =   4680
               Style           =   2  'Dropdown List
               TabIndex        =   111
               Top             =   3720
               Width           =   850
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   66
               Left            =   3840
               TabIndex        =   103
               Top             =   1800
               Width           =   850
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   66
               Left            =   4680
               Style           =   2  'Dropdown List
               TabIndex        =   104
               Top             =   1800
               Width           =   850
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   65
               Left            =   3840
               TabIndex        =   101
               Top             =   1440
               Width           =   850
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   65
               Left            =   4680
               Style           =   2  'Dropdown List
               TabIndex        =   102
               Top             =   1440
               Width           =   850
            End
            Begin VB.ComboBox cb 
               Height          =   288
               Index           =   7
               Left            =   3480
               Style           =   2  'Dropdown List
               TabIndex        =   105
               Top             =   2520
               Width           =   2052
            End
            Begin VB.ComboBox cb 
               Height          =   288
               Index           =   6
               ItemData        =   "GEN_EXP.frx":09FE
               Left            =   3480
               List            =   "GEN_EXP.frx":0A00
               Style           =   2  'Dropdown List
               TabIndex        =   98
               Top             =   1080
               Width           =   2052
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   60
               Left            =   3840
               TabIndex        =   99
               Top             =   2160
               Width           =   850
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   60
               Left            =   4680
               Style           =   2  'Dropdown List
               TabIndex        =   100
               Top             =   2160
               Width           =   850
            End
            Begin Threed.SSCheck SSCheck1 
               Height          =   252
               Index           =   36
               Left            =   1080
               TabIndex        =   95
               Top             =   240
               Width           =   5172
               _Version        =   65536
               _ExtentX        =   9123
               _ExtentY        =   445
               _StockProps     =   78
               Caption         =   "SSCheck1"
               BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
                  Name            =   "MS Sans Serif"
                  Size            =   8.25
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Value           =   -1  'True
            End
            Begin Threed.SSCheck SSCheck1 
               Height          =   252
               Index           =   29
               Left            =   1080
               TabIndex        =   97
               Top             =   720
               Width           =   5172
               _Version        =   65536
               _ExtentX        =   9123
               _ExtentY        =   445
               _StockProps     =   78
               Caption         =   "SSCheck1"
               BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
                  Name            =   "MS Sans Serif"
                  Size            =   8.25
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Value           =   -1  'True
            End
            Begin Threed.SSCheck SSCheck1 
               Height          =   252
               Index           =   28
               Left            =   1080
               TabIndex        =   96
               Top             =   480
               Width           =   5172
               _Version        =   65536
               _ExtentX        =   9123
               _ExtentY        =   445
               _StockProps     =   78
               Caption         =   "SSCheck1"
               BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
                  Name            =   "MS Sans Serif"
                  Size            =   8.25
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Value           =   -1  'True
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   90
               Left            =   5640
               TabIndex        =   221
               Top             =   4080
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   90
               Left            =   240
               TabIndex        =   220
               Top             =   4080
               Width           =   3612
            End
            Begin VB.Label cref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   36
               Left            =   240
               TabIndex        =   189
               Top             =   240
               Width           =   804
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   84
               Left            =   5640
               TabIndex        =   188
               Top             =   3240
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   84
               Left            =   240
               TabIndex        =   187
               Top             =   3240
               Width           =   3612
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   83
               Left            =   5640
               TabIndex        =   186
               Top             =   2880
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   83
               Left            =   240
               TabIndex        =   185
               Top             =   2880
               Width           =   3612
            End
            Begin VB.Label cref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   29
               Left            =   240
               TabIndex        =   184
               Top             =   720
               Width           =   804
            End
            Begin VB.Label cref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   28
               Left            =   240
               TabIndex        =   183
               Top             =   480
               Width           =   804
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   67
               Left            =   240
               TabIndex        =   146
               Top             =   3720
               Width           =   3612
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   67
               Left            =   5640
               TabIndex        =   145
               Top             =   3720
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   66
               Left            =   360
               TabIndex        =   144
               Top             =   1800
               Width           =   3492
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   66
               Left            =   5640
               TabIndex        =   143
               Top             =   1800
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   65
               Left            =   360
               TabIndex        =   142
               Top             =   1440
               Width           =   3492
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   65
               Left            =   5640
               TabIndex        =   141
               Top             =   1440
               Width           =   972
            End
            Begin VB.Label blbl 
               Height          =   252
               Index           =   7
               Left            =   240
               TabIndex        =   140
               Top             =   2520
               Width           =   3252
            End
            Begin VB.Label bref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   7
               Left            =   5640
               TabIndex        =   139
               Top             =   2520
               Width           =   972
            End
            Begin VB.Label blbl 
               Height          =   252
               Index           =   6
               Left            =   360
               TabIndex        =   138
               Top             =   1080
               Width           =   3132
            End
            Begin VB.Label bref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   6
               Left            =   5640
               TabIndex        =   137
               Top             =   1080
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   60
               Left            =   240
               TabIndex        =   136
               Top             =   2160
               Width           =   3612
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   60
               Left            =   5640
               TabIndex        =   135
               Top             =   2160
               Width           =   972
            End
         End
         Begin Threed.SSFrame wt 
            Height          =   4692
            Index           =   1
            Left            =   -74760
            TabIndex        =   147
            Top             =   360
            Width           =   6972
            _Version        =   65536
            _ExtentX        =   12298
            _ExtentY        =   8276
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
            Enabled         =   0   'False
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   107
               Left            =   3720
               TabIndex        =   87
               Top             =   2520
               Width           =   852
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   107
               Left            =   4560
               Style           =   2  'Dropdown List
               TabIndex        =   88
               Top             =   2520
               Width           =   972
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   108
               Left            =   3720
               TabIndex        =   89
               Top             =   3000
               Width           =   852
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   108
               Left            =   4560
               Style           =   2  'Dropdown List
               TabIndex        =   90
               Top             =   3000
               Width           =   972
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   109
               Left            =   3720
               TabIndex        =   91
               Top             =   3480
               Width           =   852
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   109
               Left            =   4560
               Style           =   2  'Dropdown List
               TabIndex        =   92
               Top             =   3480
               Width           =   972
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   110
               Left            =   3720
               TabIndex        =   93
               Top             =   3960
               Width           =   852
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   110
               Left            =   4560
               Style           =   2  'Dropdown List
               TabIndex        =   94
               Top             =   3960
               Width           =   972
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   77
               Left            =   3720
               TabIndex        =   85
               Top             =   1800
               Width           =   852
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   77
               Left            =   4560
               Style           =   2  'Dropdown List
               TabIndex        =   86
               Top             =   1800
               Width           =   972
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   76
               Left            =   3720
               TabIndex        =   83
               Top             =   1320
               Width           =   852
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   76
               Left            =   4560
               Style           =   2  'Dropdown List
               TabIndex        =   84
               Top             =   1320
               Width           =   972
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   75
               Left            =   3720
               TabIndex        =   81
               Top             =   840
               Width           =   852
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   75
               Left            =   4560
               Style           =   2  'Dropdown List
               TabIndex        =   82
               Top             =   840
               Width           =   972
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   74
               Left            =   3720
               TabIndex        =   79
               Top             =   360
               Width           =   852
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   74
               Left            =   4560
               Style           =   2  'Dropdown List
               TabIndex        =   80
               Top             =   360
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   107
               Left            =   240
               TabIndex        =   287
               Top             =   2520
               Width           =   3492
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   107
               Left            =   5640
               TabIndex        =   286
               Top             =   2520
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   108
               Left            =   240
               TabIndex        =   285
               Top             =   3000
               Width           =   3492
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   108
               Left            =   5640
               TabIndex        =   284
               Top             =   3000
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   109
               Left            =   240
               TabIndex        =   283
               Top             =   3480
               Width           =   3492
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   109
               Left            =   5640
               TabIndex        =   282
               Top             =   3480
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   110
               Left            =   240
               TabIndex        =   281
               Top             =   3960
               Width           =   3492
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   110
               Left            =   5640
               TabIndex        =   280
               Top             =   3960
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   77
               Left            =   240
               TabIndex        =   155
               Top             =   1800
               Width           =   3492
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   77
               Left            =   5640
               TabIndex        =   154
               Top             =   1800
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   76
               Left            =   240
               TabIndex        =   153
               Top             =   1320
               Width           =   3492
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   76
               Left            =   5640
               TabIndex        =   152
               Top             =   1320
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   75
               Left            =   240
               TabIndex        =   151
               Top             =   840
               Width           =   3492
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   75
               Left            =   5640
               TabIndex        =   150
               Top             =   840
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   74
               Left            =   240
               TabIndex        =   149
               Top             =   360
               Width           =   3492
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   74
               Left            =   5640
               TabIndex        =   148
               Top             =   360
               Width           =   972
            End
         End
         Begin Threed.SSFrame wt 
            Height          =   4692
            Index           =   2
            Left            =   -74760
            TabIndex        =   156
            Top             =   360
            Width           =   6972
            _Version        =   65536
            _ExtentX        =   12298
            _ExtentY        =   8276
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
            Enabled         =   0   'False
            Begin VB.ComboBox cb 
               Height          =   288
               Index           =   0
               ItemData        =   "GEN_EXP.frx":0A02
               Left            =   3600
               List            =   "GEN_EXP.frx":0A04
               Style           =   2  'Dropdown List
               TabIndex        =   73
               Top             =   2040
               Width           =   1932
            End
            Begin VB.ComboBox cb 
               Height          =   288
               Index           =   1
               Left            =   3600
               Style           =   2  'Dropdown List
               TabIndex        =   74
               Top             =   2400
               Width           =   1932
            End
            Begin VB.ComboBox cb 
               Height          =   288
               Index           =   2
               Left            =   3600
               Style           =   2  'Dropdown List
               TabIndex        =   75
               Top             =   2760
               Width           =   1932
            End
            Begin VB.ComboBox cb 
               Height          =   288
               Index           =   3
               Left            =   3600
               Style           =   2  'Dropdown List
               TabIndex        =   76
               Top             =   3120
               Width           =   1932
            End
            Begin VB.ComboBox cb 
               Height          =   288
               Index           =   4
               Left            =   3600
               Style           =   2  'Dropdown List
               TabIndex        =   77
               Top             =   3480
               Width           =   1932
            End
            Begin VB.ComboBox cb 
               Height          =   288
               Index           =   5
               Left            =   3600
               Style           =   2  'Dropdown List
               TabIndex        =   78
               Top             =   3840
               Width           =   1932
            End
            Begin VB.ComboBox cb 
               Height          =   288
               Index           =   8
               Left            =   3600
               Style           =   2  'Dropdown List
               TabIndex        =   72
               Top             =   1440
               Width           =   1932
            End
            Begin VB.ComboBox cb 
               Height          =   288
               Index           =   9
               Left            =   3600
               Style           =   2  'Dropdown List
               TabIndex        =   71
               Top             =   1080
               Width           =   1932
            End
            Begin VB.ComboBox cb 
               Height          =   288
               Index           =   10
               Left            =   3600
               Style           =   2  'Dropdown List
               TabIndex        =   70
               Top             =   720
               Width           =   1932
            End
            Begin VB.ComboBox cb 
               Height          =   288
               Index           =   11
               ItemData        =   "GEN_EXP.frx":0A06
               Left            =   3600
               List            =   "GEN_EXP.frx":0A08
               Style           =   2  'Dropdown List
               TabIndex        =   69
               Top             =   384
               Width           =   1932
            End
            Begin VB.Label blbl 
               Height          =   252
               Index           =   0
               Left            =   240
               TabIndex        =   279
               Top             =   2040
               Width           =   3372
            End
            Begin VB.Label bref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   0
               Left            =   5640
               TabIndex        =   278
               Top             =   2040
               Width           =   1092
            End
            Begin VB.Label blbl 
               Height          =   252
               Index           =   1
               Left            =   240
               TabIndex        =   277
               Top             =   2400
               Width           =   3372
            End
            Begin VB.Label bref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   1
               Left            =   5640
               TabIndex        =   276
               Top             =   2400
               Width           =   1092
            End
            Begin VB.Label blbl 
               Height          =   252
               Index           =   2
               Left            =   240
               TabIndex        =   275
               Top             =   2760
               Width           =   3372
            End
            Begin VB.Label bref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   2
               Left            =   5640
               TabIndex        =   274
               Top             =   2760
               Width           =   1092
            End
            Begin VB.Label blbl 
               Height          =   252
               Index           =   3
               Left            =   240
               TabIndex        =   273
               Top             =   3120
               Width           =   3372
            End
            Begin VB.Label bref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   3
               Left            =   5640
               TabIndex        =   272
               Top             =   3120
               Width           =   1092
            End
            Begin VB.Label blbl 
               Height          =   252
               Index           =   4
               Left            =   240
               TabIndex        =   271
               Top             =   3480
               Width           =   3372
            End
            Begin VB.Label bref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   4
               Left            =   5640
               TabIndex        =   270
               Top             =   3480
               Width           =   1092
            End
            Begin VB.Label blbl 
               Height          =   252
               Index           =   5
               Left            =   240
               TabIndex        =   269
               Top             =   3840
               Width           =   3372
            End
            Begin VB.Label bref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   5
               Left            =   5640
               TabIndex        =   268
               Top             =   3840
               Width           =   1092
            End
            Begin VB.Label bref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   8
               Left            =   5640
               TabIndex        =   164
               Top             =   1440
               Width           =   1092
            End
            Begin VB.Label blbl 
               Height          =   252
               Index           =   8
               Left            =   240
               TabIndex        =   163
               Top             =   1440
               Width           =   3372
            End
            Begin VB.Label bref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   9
               Left            =   5640
               TabIndex        =   162
               Top             =   1080
               Width           =   1092
            End
            Begin VB.Label blbl 
               Height          =   252
               Index           =   9
               Left            =   240
               TabIndex        =   161
               Top             =   1080
               Width           =   3372
            End
            Begin VB.Label bref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   10
               Left            =   5640
               TabIndex        =   160
               Top             =   720
               Width           =   1092
            End
            Begin VB.Label blbl 
               Height          =   252
               Index           =   10
               Left            =   240
               TabIndex        =   159
               Top             =   720
               Width           =   3372
            End
            Begin VB.Label bref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   11
               Left            =   5640
               TabIndex        =   158
               Top             =   360
               Width           =   1092
            End
            Begin VB.Label blbl 
               Height          =   252
               Index           =   11
               Left            =   240
               TabIndex        =   157
               Top             =   360
               Width           =   3372
            End
         End
         Begin Threed.SSFrame wt 
            Height          =   4692
            Index           =   3
            Left            =   -74760
            TabIndex        =   165
            Top             =   360
            Width           =   6972
            _Version        =   65536
            _ExtentX        =   12298
            _ExtentY        =   8276
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
            Enabled         =   0   'False
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   43
               Left            =   3480
               TabIndex        =   57
               Top             =   2040
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   43
               Left            =   4440
               Style           =   2  'Dropdown List
               TabIndex        =   58
               Top             =   2040
               Width           =   1092
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   44
               Left            =   3480
               TabIndex        =   59
               Top             =   2400
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   44
               Left            =   4440
               Style           =   2  'Dropdown List
               TabIndex        =   60
               Top             =   2400
               Width           =   1092
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   45
               Left            =   3480
               TabIndex        =   61
               Top             =   2760
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   45
               Left            =   4440
               Style           =   2  'Dropdown List
               TabIndex        =   62
               Top             =   2760
               Width           =   1092
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   46
               Left            =   3480
               TabIndex        =   63
               Top             =   3120
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   46
               Left            =   4440
               Style           =   2  'Dropdown List
               TabIndex        =   64
               Top             =   3120
               Width           =   1092
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   47
               Left            =   3480
               TabIndex        =   65
               Top             =   3480
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   47
               Left            =   4440
               Style           =   2  'Dropdown List
               TabIndex        =   66
               Top             =   3480
               Width           =   1092
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   48
               Left            =   3480
               TabIndex        =   67
               Top             =   3840
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   48
               Left            =   4440
               Style           =   2  'Dropdown List
               TabIndex        =   68
               Top             =   3840
               Width           =   1092
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   49
               Left            =   3480
               TabIndex        =   49
               Top             =   360
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   49
               Left            =   4440
               Style           =   2  'Dropdown List
               TabIndex        =   50
               Top             =   360
               Width           =   1092
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   50
               Left            =   3480
               TabIndex        =   51
               Top             =   720
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   50
               Left            =   4440
               Style           =   2  'Dropdown List
               TabIndex        =   52
               Top             =   720
               Width           =   1092
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   51
               Left            =   3480
               TabIndex        =   53
               Top             =   1080
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   51
               Left            =   4440
               Style           =   2  'Dropdown List
               TabIndex        =   54
               Top             =   1080
               Width           =   1092
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   52
               Left            =   3480
               TabIndex        =   55
               Top             =   1440
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   52
               Left            =   4440
               Style           =   2  'Dropdown List
               TabIndex        =   56
               Top             =   1440
               Width           =   1092
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   43
               Left            =   240
               TabIndex        =   267
               Top             =   2040
               Width           =   3252
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   43
               Left            =   5640
               TabIndex        =   266
               Top             =   2040
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   44
               Left            =   240
               TabIndex        =   265
               Top             =   2400
               Width           =   3252
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   44
               Left            =   5640
               TabIndex        =   264
               Top             =   2400
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   45
               Left            =   240
               TabIndex        =   263
               Top             =   2760
               Width           =   3252
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   45
               Left            =   5640
               TabIndex        =   262
               Top             =   2760
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   46
               Left            =   240
               TabIndex        =   261
               Top             =   3120
               Width           =   3252
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   46
               Left            =   5640
               TabIndex        =   260
               Top             =   3120
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   47
               Left            =   240
               TabIndex        =   259
               Top             =   3480
               Width           =   3252
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   47
               Left            =   5640
               TabIndex        =   258
               Top             =   3480
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   48
               Left            =   240
               TabIndex        =   257
               Top             =   3840
               Width           =   3252
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   48
               Left            =   5640
               TabIndex        =   256
               Top             =   3840
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   49
               Left            =   240
               TabIndex        =   173
               Top             =   360
               Width           =   3252
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   49
               Left            =   5640
               TabIndex        =   172
               Top             =   360
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   50
               Left            =   240
               TabIndex        =   171
               Top             =   720
               Width           =   3252
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   50
               Left            =   5640
               TabIndex        =   170
               Top             =   720
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   51
               Left            =   240
               TabIndex        =   169
               Top             =   1080
               Width           =   3252
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   51
               Left            =   5640
               TabIndex        =   168
               Top             =   1080
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   52
               Left            =   240
               TabIndex        =   167
               Top             =   1440
               Width           =   3252
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   52
               Left            =   5640
               TabIndex        =   166
               Top             =   1440
               Width           =   972
            End
         End
         Begin Threed.SSFrame wt 
            Height          =   4692
            Index           =   4
            Left            =   -74760
            TabIndex        =   174
            Top             =   360
            Width           =   6972
            _Version        =   65536
            _ExtentX        =   12298
            _ExtentY        =   8276
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
            Enabled         =   0   'False
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   53
               Left            =   4440
               Style           =   2  'Dropdown List
               TabIndex        =   38
               Top             =   2040
               Width           =   1092
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   53
               Left            =   3480
               TabIndex        =   37
               Top             =   2040
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   54
               Left            =   4440
               Style           =   2  'Dropdown List
               TabIndex        =   40
               Top             =   2400
               Width           =   1092
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   54
               Left            =   3480
               TabIndex        =   39
               Top             =   2400
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   55
               Left            =   4440
               Style           =   2  'Dropdown List
               TabIndex        =   42
               Top             =   2760
               Width           =   1092
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   55
               Left            =   3480
               TabIndex        =   41
               Top             =   2760
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   56
               Left            =   4440
               Style           =   2  'Dropdown List
               TabIndex        =   44
               Top             =   3120
               Width           =   1092
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   56
               Left            =   3480
               TabIndex        =   43
               Top             =   3120
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   57
               Left            =   4440
               Style           =   2  'Dropdown List
               TabIndex        =   46
               Top             =   3480
               Width           =   1092
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   57
               Left            =   3480
               TabIndex        =   45
               Top             =   3480
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   58
               Left            =   4440
               Style           =   2  'Dropdown List
               TabIndex        =   48
               Top             =   3840
               Width           =   1092
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   58
               Left            =   3480
               TabIndex        =   47
               Top             =   3840
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   61
               Left            =   4440
               Style           =   2  'Dropdown List
               TabIndex        =   30
               Top             =   360
               Width           =   1092
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   61
               Left            =   3480
               TabIndex        =   29
               Top             =   360
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   62
               Left            =   4440
               Style           =   2  'Dropdown List
               TabIndex        =   32
               Top             =   720
               Width           =   1092
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   62
               Left            =   3480
               TabIndex        =   31
               Top             =   720
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   63
               Left            =   4440
               Style           =   2  'Dropdown List
               TabIndex        =   34
               Top             =   1080
               Width           =   1092
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   63
               Left            =   3480
               TabIndex        =   33
               Top             =   1080
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   64
               Left            =   4440
               Style           =   2  'Dropdown List
               TabIndex        =   36
               Top             =   1440
               Width           =   1092
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   64
               Left            =   3480
               TabIndex        =   35
               Top             =   1440
               Width           =   972
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   53
               Left            =   5640
               TabIndex        =   255
               Top             =   2040
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   53
               Left            =   240
               TabIndex        =   254
               Top             =   2040
               Width           =   3252
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   54
               Left            =   5640
               TabIndex        =   253
               Top             =   2400
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   54
               Left            =   240
               TabIndex        =   252
               Top             =   2400
               Width           =   3252
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   55
               Left            =   5640
               TabIndex        =   251
               Top             =   2760
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   55
               Left            =   240
               TabIndex        =   250
               Top             =   2760
               Width           =   3252
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   56
               Left            =   5640
               TabIndex        =   249
               Top             =   3120
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   56
               Left            =   240
               TabIndex        =   248
               Top             =   3120
               Width           =   3252
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   57
               Left            =   5640
               TabIndex        =   247
               Top             =   3480
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   57
               Left            =   240
               TabIndex        =   246
               Top             =   3480
               Width           =   3252
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   58
               Left            =   5640
               TabIndex        =   245
               Top             =   3840
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   58
               Left            =   240
               TabIndex        =   244
               Top             =   3840
               Width           =   3252
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   61
               Left            =   5640
               TabIndex        =   182
               Top             =   360
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   61
               Left            =   240
               TabIndex        =   181
               Top             =   360
               Width           =   3252
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   62
               Left            =   5640
               TabIndex        =   180
               Top             =   720
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   62
               Left            =   240
               TabIndex        =   179
               Top             =   720
               Width           =   3252
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   63
               Left            =   5640
               TabIndex        =   178
               Top             =   1080
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   63
               Left            =   240
               TabIndex        =   177
               Top             =   1080
               Width           =   3252
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   64
               Left            =   5640
               TabIndex        =   176
               Top             =   1440
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   64
               Left            =   240
               TabIndex        =   175
               Top             =   1440
               Width           =   3252
            End
         End
      End
      Begin TabDlg.SSTab SSTab1 
         Height          =   5292
         Index           =   2
         Left            =   -74880
         TabIndex        =   190
         TabStop         =   0   'False
         Top             =   360
         Width           =   7452
         _ExtentX        =   13150
         _ExtentY        =   9340
         _Version        =   393216
         Style           =   1
         Tab             =   2
         TabHeight       =   420
         Enabled         =   0   'False
         TabCaption(0)   =   "Leaching"
         TabPicture(0)   =   "GEN_EXP.frx":0A0A
         Tab(0).ControlEnabled=   0   'False
         Tab(0).Control(0)=   "sl(0)"
         Tab(0).Control(0).Enabled=   0   'False
         Tab(0).ControlCount=   1
         TabCaption(1)   =   "Resuspension"
         TabPicture(1)   =   "GEN_EXP.frx":0A26
         Tab(1).ControlEnabled=   0   'False
         Tab(1).Control(0)=   "sl(1)"
         Tab(1).Control(0).Enabled=   0   'False
         Tab(1).ControlCount=   1
         TabCaption(2)   =   "Surface Soil"
         TabPicture(2)   =   "GEN_EXP.frx":0A42
         Tab(2).ControlEnabled=   -1  'True
         Tab(2).Control(0)=   "sl(2)"
         Tab(2).Control(0).Enabled=   0   'False
         Tab(2).ControlCount=   1
         Begin Threed.SSFrame sl 
            Height          =   4692
            Index           =   0
            Left            =   -74760
            TabIndex        =   191
            Top             =   360
            Width           =   6972
            _Version        =   65536
            _ExtentX        =   12298
            _ExtentY        =   8276
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
            Enabled         =   0   'False
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   91
               Left            =   1440
               Style           =   2  'Dropdown List
               TabIndex        =   12
               Top             =   3360
               Width           =   1092
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   91
               Left            =   480
               TabIndex        =   11
               Top             =   3360
               Width           =   972
            End
            Begin VB.ComboBox kd2 
               Height          =   288
               ItemData        =   "GEN_EXP.frx":0A5E
               Left            =   3840
               List            =   "GEN_EXP.frx":0A60
               Style           =   2  'Dropdown List
               TabIndex        =   13
               Top             =   2880
               Width           =   2052
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   92
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   15
               Top             =   3360
               Width           =   1092
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   92
               Left            =   3840
               TabIndex        =   14
               Top             =   3360
               Width           =   972
            End
            Begin VB.ComboBox kd1 
               Height          =   288
               ItemData        =   "GEN_EXP.frx":0A62
               Left            =   480
               List            =   "GEN_EXP.frx":0A64
               Style           =   2  'Dropdown List
               TabIndex        =   10
               Top             =   2880
               Width           =   2052
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   88
               Left            =   3600
               TabIndex        =   8
               Top             =   1920
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   88
               Left            =   4560
               Style           =   2  'Dropdown List
               TabIndex        =   9
               Top             =   1920
               Width           =   1092
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   87
               Left            =   3600
               TabIndex        =   6
               Top             =   1560
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   87
               Left            =   4560
               Style           =   2  'Dropdown List
               TabIndex        =   7
               Top             =   1560
               Width           =   1092
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   86
               Left            =   3600
               TabIndex        =   4
               Top             =   1200
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   86
               Left            =   4560
               Style           =   2  'Dropdown List
               TabIndex        =   5
               Top             =   1200
               Width           =   1092
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   85
               Left            =   3600
               TabIndex        =   2
               Top             =   840
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   85
               Left            =   4560
               Style           =   2  'Dropdown List
               TabIndex        =   3
               Top             =   840
               Width           =   1092
            End
            Begin VB.ComboBox cb 
               Height          =   288
               Index           =   15
               ItemData        =   "GEN_EXP.frx":0A66
               Left            =   2760
               List            =   "GEN_EXP.frx":0A68
               Style           =   2  'Dropdown List
               TabIndex        =   1
               Top             =   360
               Width           =   2892
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   88
               Left            =   240
               TabIndex        =   205
               Top             =   1920
               Width           =   3252
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   88
               Left            =   5760
               TabIndex        =   204
               Top             =   1920
               Width           =   972
            End
            Begin VB.Label lbl 
               Caption         =   "Progeny (Kd)"
               Height          =   252
               Index           =   92
               Left            =   3600
               TabIndex        =   203
               Top             =   2520
               Width           =   3252
            End
            Begin VB.Label bref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   17
               Left            =   6000
               TabIndex        =   202
               Top             =   2880
               Width           =   972
            End
            Begin VB.Label lbl 
               Caption         =   "Soil absorption coefficient (Kd)"
               Height          =   252
               Index           =   91
               Left            =   240
               TabIndex        =   200
               Top             =   2520
               Width           =   3252
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   87
               Left            =   240
               TabIndex        =   199
               Top             =   1560
               Width           =   3252
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   87
               Left            =   5760
               TabIndex        =   198
               Top             =   1560
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   86
               Left            =   240
               TabIndex        =   197
               Top             =   1200
               Width           =   3252
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   86
               Left            =   5760
               TabIndex        =   196
               Top             =   1200
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   85
               Left            =   240
               TabIndex        =   195
               Top             =   840
               Width           =   3252
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   85
               Left            =   5760
               TabIndex        =   194
               Top             =   840
               Width           =   972
            End
            Begin VB.Label bref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   15
               Left            =   5760
               TabIndex        =   193
               Top             =   360
               Width           =   972
            End
            Begin VB.Label blbl 
               Height          =   252
               Index           =   15
               Left            =   240
               TabIndex        =   192
               Top             =   360
               Width           =   2532
            End
            Begin VB.Label bref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   16
               Left            =   2640
               TabIndex        =   201
               Top             =   2880
               Width           =   972
            End
         End
         Begin Threed.SSFrame sl 
            Height          =   4692
            Index           =   1
            Left            =   -74760
            TabIndex        =   206
            Top             =   360
            Width           =   6972
            _Version        =   65536
            _ExtentX        =   12298
            _ExtentY        =   8276
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
            Enabled         =   0   'False
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   89
               Left            =   4680
               Style           =   2  'Dropdown List
               TabIndex        =   22
               Top             =   1800
               Width           =   972
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   89
               Left            =   3720
               TabIndex        =   21
               Top             =   1800
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   72
               Left            =   4680
               Style           =   2  'Dropdown List
               TabIndex        =   18
               Top             =   840
               Width           =   972
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   72
               Left            =   3720
               TabIndex        =   17
               Top             =   840
               Width           =   972
            End
            Begin VB.ComboBox cb 
               Height          =   288
               Index           =   14
               ItemData        =   "GEN_EXP.frx":0A6A
               Left            =   2880
               List            =   "GEN_EXP.frx":0A6C
               Style           =   2  'Dropdown List
               TabIndex        =   16
               Top             =   360
               Width           =   2772
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   73
               Left            =   3720
               TabIndex        =   19
               Top             =   1320
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   73
               Left            =   4680
               Style           =   2  'Dropdown List
               TabIndex        =   20
               Top             =   1320
               Width           =   972
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   89
               Left            =   5760
               TabIndex        =   219
               Top             =   1800
               Width           =   948
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   89
               Left            =   240
               TabIndex        =   218
               Top             =   1800
               Width           =   3372
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   72
               Left            =   5760
               TabIndex        =   217
               Top             =   840
               Width           =   948
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   72
               Left            =   240
               TabIndex        =   216
               Top             =   840
               Width           =   3372
            End
            Begin VB.Label bref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   14
               Left            =   5760
               TabIndex        =   215
               Top             =   360
               Width           =   972
            End
            Begin VB.Label blbl 
               Height          =   252
               Index           =   14
               Left            =   240
               TabIndex        =   214
               Top             =   360
               Width           =   2652
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   73
               Left            =   240
               TabIndex        =   213
               Top             =   1320
               Width           =   3372
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   73
               Left            =   5760
               TabIndex        =   212
               Top             =   1320
               Width           =   948
            End
         End
         Begin Threed.SSFrame sl 
            Height          =   4692
            Index           =   2
            Left            =   240
            TabIndex        =   207
            Top             =   360
            Width           =   6972
            _Version        =   65536
            _ExtentX        =   12298
            _ExtentY        =   8276
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
            Enabled         =   0   'False
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   139
               Left            =   4560
               Style           =   2  'Dropdown List
               TabIndex        =   28
               Top             =   1320
               Width           =   1092
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   139
               Left            =   3600
               TabIndex        =   27
               Top             =   1320
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   81
               Left            =   4560
               Style           =   2  'Dropdown List
               TabIndex        =   24
               Top             =   360
               Width           =   1092
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   81
               Left            =   3600
               TabIndex        =   23
               Top             =   360
               Width           =   972
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   68
               Left            =   3600
               TabIndex        =   25
               Top             =   840
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   68
               Left            =   4560
               Style           =   2  'Dropdown List
               TabIndex        =   26
               Top             =   840
               Width           =   1092
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   139
               Left            =   5760
               TabIndex        =   243
               Top             =   1320
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   139
               Left            =   360
               TabIndex        =   242
               Top             =   1320
               Width           =   3252
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   81
               Left            =   5760
               TabIndex        =   211
               Top             =   360
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   81
               Left            =   360
               TabIndex        =   210
               Top             =   360
               Width           =   3252
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   68
               Left            =   360
               TabIndex        =   209
               Top             =   840
               Width           =   3252
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   68
               Left            =   5760
               TabIndex        =   208
               Top             =   840
               Width           =   972
            End
         End
      End
      Begin Threed.SSFrame frm 
         Height          =   5172
         Index           =   0
         Left            =   240
         TabIndex        =   222
         Top             =   360
         Width           =   7212
         _Version        =   65536
         _ExtentX        =   12721
         _ExtentY        =   9123
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
         Begin VB.ComboBox unt 
            Height          =   315
            Index           =   70
            Left            =   4800
            Style           =   2  'Dropdown List
            TabIndex        =   494
            Top             =   4320
            Visible         =   0   'False
            Width           =   852
         End
         Begin VB.TextBox txt 
            Height          =   288
            Index           =   70
            Left            =   3960
            TabIndex        =   493
            Top             =   4320
            Visible         =   0   'False
            Width           =   852
         End
         Begin VB.TextBox txt 
            Height          =   288
            Index           =   137
            Left            =   3960
            TabIndex        =   125
            Top             =   2880
            Width           =   852
         End
         Begin VB.ComboBox unt 
            Height          =   288
            Index           =   137
            Left            =   4800
            Style           =   2  'Dropdown List
            TabIndex        =   126
            Top             =   2880
            Width           =   852
         End
         Begin VB.ComboBox unt 
            Height          =   288
            Index           =   2
            Left            =   4800
            Style           =   2  'Dropdown List
            TabIndex        =   124
            Top             =   2520
            Width           =   850
         End
         Begin VB.TextBox txt 
            Height          =   288
            Index           =   2
            Left            =   3960
            TabIndex        =   123
            Top             =   2520
            Width           =   850
         End
         Begin VB.ComboBox unt 
            Height          =   288
            Index           =   1
            Left            =   4800
            Style           =   2  'Dropdown List
            TabIndex        =   122
            Top             =   2160
            Width           =   850
         End
         Begin VB.TextBox txt 
            Height          =   288
            Index           =   1
            Left            =   3960
            TabIndex        =   121
            Top             =   2160
            Width           =   850
         End
         Begin VB.ComboBox unt 
            Height          =   288
            Index           =   0
            Left            =   4800
            Style           =   2  'Dropdown List
            TabIndex        =   120
            Top             =   1800
            Width           =   850
         End
         Begin VB.TextBox txt 
            Height          =   288
            Index           =   0
            Left            =   3960
            TabIndex        =   119
            Top             =   1800
            Width           =   850
         End
         Begin VB.ComboBox unt 
            Height          =   288
            Index           =   71
            Left            =   4800
            Style           =   2  'Dropdown List
            TabIndex        =   128
            Top             =   3240
            Width           =   850
         End
         Begin VB.TextBox txt 
            Height          =   288
            Index           =   71
            Left            =   3960
            TabIndex        =   127
            Top             =   3240
            Width           =   850
         End
         Begin VB.TextBox txt 
            Height          =   288
            Index           =   82
            Left            =   3960
            TabIndex        =   129
            Top             =   3600
            Width           =   850
         End
         Begin VB.ComboBox unt 
            Height          =   288
            Index           =   82
            Left            =   4800
            Style           =   2  'Dropdown List
            TabIndex        =   130
            Top             =   3600
            Width           =   850
         End
         Begin VB.ComboBox unt 
            Height          =   288
            Index           =   69
            Left            =   4800
            Style           =   2  'Dropdown List
            TabIndex        =   132
            Top             =   3960
            Width           =   852
         End
         Begin VB.TextBox txt 
            Height          =   288
            Index           =   69
            Left            =   3960
            TabIndex        =   131
            Top             =   3960
            Width           =   852
         End
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   32
            Left            =   1080
            TabIndex        =   117
            Top             =   1080
            Width           =   2400
            _Version        =   65536
            _ExtentX        =   4233
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
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
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   33
            Left            =   1080
            TabIndex        =   116
            Top             =   840
            Width           =   2400
            _Version        =   65536
            _ExtentX        =   4233
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
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
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   34
            Left            =   1080
            TabIndex        =   115
            Top             =   600
            Width           =   2400
            _Version        =   65536
            _ExtentX        =   4233
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
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
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   35
            Left            =   1080
            TabIndex        =   114
            Top             =   360
            Width           =   2400
            _Version        =   65536
            _ExtentX        =   4233
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
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
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   31
            Left            =   1080
            TabIndex        =   118
            Top             =   1320
            Width           =   2400
            _Version        =   65536
            _ExtentX        =   4233
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Enabled         =   0   'False
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   70
            Left            =   5760
            TabIndex        =   495
            Top             =   4320
            Visible         =   0   'False
            Width           =   975
         End
         Begin VB.Label lbl 
            Height          =   255
            Index           =   70
            Left            =   240
            TabIndex        =   492
            Top             =   4320
            Visible         =   0   'False
            Width           =   3735
         End
         Begin VB.Label lbl 
            Height          =   252
            Index           =   137
            Left            =   240
            TabIndex        =   241
            Top             =   2880
            Width           =   3732
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   137
            Left            =   5760
            TabIndex        =   240
            Top             =   2880
            Width           =   972
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   2
            Left            =   5760
            TabIndex        =   239
            Top             =   2520
            Width           =   948
         End
         Begin VB.Label lbl 
            Height          =   252
            Index           =   2
            Left            =   240
            TabIndex        =   238
            Top             =   2520
            Width           =   3732
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   1
            Left            =   5760
            TabIndex        =   237
            Top             =   2160
            Width           =   948
         End
         Begin VB.Label lbl 
            Height          =   252
            Index           =   1
            Left            =   240
            TabIndex        =   236
            Top             =   2160
            Width           =   3732
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   0
            Left            =   5760
            TabIndex        =   235
            Top             =   1800
            Width           =   948
         End
         Begin VB.Label lbl 
            Height          =   252
            Index           =   0
            Left            =   240
            TabIndex        =   234
            Top             =   1800
            Width           =   3732
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   71
            Left            =   5760
            TabIndex        =   233
            Top             =   3240
            Width           =   948
         End
         Begin VB.Label lbl 
            Height          =   252
            Index           =   71
            Left            =   240
            TabIndex        =   232
            Top             =   3240
            Width           =   3732
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   35
            Left            =   240
            TabIndex        =   231
            Top             =   360
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   34
            Left            =   240
            TabIndex        =   230
            Top             =   600
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   33
            Left            =   240
            TabIndex        =   229
            Top             =   840
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   32
            Left            =   240
            TabIndex        =   228
            Top             =   1080
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   31
            Left            =   240
            TabIndex        =   227
            Top             =   1320
            Width           =   804
         End
         Begin VB.Label lbl 
            Height          =   252
            Index           =   82
            Left            =   240
            TabIndex        =   226
            Top             =   3600
            Width           =   3732
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   82
            Left            =   5760
            TabIndex        =   225
            Top             =   3600
            Width           =   948
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   69
            Left            =   5760
            TabIndex        =   224
            Top             =   3960
            Width           =   972
         End
         Begin VB.Label lbl 
            Height          =   255
            Index           =   69
            Left            =   240
            TabIndex        =   223
            Top             =   3960
            Width           =   3735
         End
      End
      Begin Threed.SSFrame frm 
         Height          =   5172
         Index           =   5
         Left            =   -74760
         TabIndex        =   288
         Top             =   360
         Width           =   7212
         _Version        =   65536
         _ExtentX        =   12721
         _ExtentY        =   9123
         _StockProps     =   14
         ForeColor       =   8454143
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
         Enabled         =   0   'False
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   26
            Left            =   4560
            TabIndex        =   349
            Top             =   3000
            Width           =   2496
            _Version        =   65536
            _ExtentX        =   4403
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
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
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   27
            Left            =   4560
            TabIndex        =   321
            Top             =   2760
            Width           =   2500
            _Version        =   65536
            _ExtentX        =   4403
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
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
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   8
            Left            =   1080
            TabIndex        =   319
            Top             =   2760
            Width           =   2500
            _Version        =   65536
            _ExtentX        =   4403
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
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
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   9
            Left            =   1080
            TabIndex        =   318
            Top             =   3000
            Width           =   2500
            _Version        =   65536
            _ExtentX        =   4403
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
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
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   10
            Left            =   1080
            TabIndex        =   317
            Top             =   3240
            Width           =   2500
            _Version        =   65536
            _ExtentX        =   4403
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
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
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   11
            Left            =   1080
            TabIndex        =   316
            Top             =   3480
            Width           =   2500
            _Version        =   65536
            _ExtentX        =   4403
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
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
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   12
            Left            =   1080
            TabIndex        =   315
            Top             =   3840
            Width           =   2500
            _Version        =   65536
            _ExtentX        =   4403
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
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
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   13
            Left            =   1080
            TabIndex        =   314
            Top             =   4080
            Width           =   2500
            _Version        =   65536
            _ExtentX        =   4403
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
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
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   14
            Left            =   1080
            TabIndex        =   313
            Top             =   4320
            Width           =   2500
            _Version        =   65536
            _ExtentX        =   4403
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
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
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   15
            Left            =   1080
            TabIndex        =   312
            Top             =   4560
            Width           =   2500
            _Version        =   65536
            _ExtentX        =   4403
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
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
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   0
            Left            =   1080
            TabIndex        =   311
            Top             =   600
            Width           =   2500
            _Version        =   65536
            _ExtentX        =   4403
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
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
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   1
            Left            =   1080
            TabIndex        =   310
            Top             =   840
            Width           =   2500
            _Version        =   65536
            _ExtentX        =   4403
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
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
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   2
            Left            =   1080
            TabIndex        =   309
            Top             =   1080
            Width           =   2500
            _Version        =   65536
            _ExtentX        =   4403
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
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
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   3
            Left            =   1080
            TabIndex        =   308
            Top             =   1320
            Width           =   2500
            _Version        =   65536
            _ExtentX        =   4403
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
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
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   4
            Left            =   1080
            TabIndex        =   307
            Top             =   1680
            Width           =   2500
            _Version        =   65536
            _ExtentX        =   4403
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
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
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   5
            Left            =   1080
            TabIndex        =   306
            Top             =   1920
            Width           =   2500
            _Version        =   65536
            _ExtentX        =   4403
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
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
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   6
            Left            =   1080
            TabIndex        =   305
            Top             =   2160
            Width           =   2500
            _Version        =   65536
            _ExtentX        =   4403
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
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
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   7
            Left            =   1080
            TabIndex        =   304
            Top             =   2400
            Width           =   2500
            _Version        =   65536
            _ExtentX        =   4403
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
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
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   16
            Left            =   4560
            TabIndex        =   303
            Top             =   840
            Width           =   2500
            _Version        =   65536
            _ExtentX        =   4403
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
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
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   17
            Left            =   4560
            TabIndex        =   302
            Top             =   1080
            Width           =   2500
            _Version        =   65536
            _ExtentX        =   4403
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
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
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   18
            Left            =   4560
            TabIndex        =   301
            Top             =   1800
            Width           =   2500
            _Version        =   65536
            _ExtentX        =   4403
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
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
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   19
            Left            =   4560
            TabIndex        =   300
            Top             =   2040
            Width           =   2500
            _Version        =   65536
            _ExtentX        =   4403
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
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
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   20
            Left            =   4560
            TabIndex        =   299
            Top             =   2280
            Width           =   2500
            _Version        =   65536
            _ExtentX        =   4403
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
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
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   21
            Left            =   4560
            TabIndex        =   298
            Top             =   2520
            Width           =   2500
            _Version        =   65536
            _ExtentX        =   4403
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
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
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   22
            Left            =   4560
            TabIndex        =   297
            Top             =   3840
            Visible         =   0   'False
            Width           =   2500
            _Version        =   65536
            _ExtentX        =   4403
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
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
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   23
            Left            =   4560
            TabIndex        =   296
            Top             =   4080
            Visible         =   0   'False
            Width           =   2500
            _Version        =   65536
            _ExtentX        =   4403
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
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
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   24
            Left            =   4560
            TabIndex        =   295
            Top             =   4320
            Visible         =   0   'False
            Width           =   2500
            _Version        =   65536
            _ExtentX        =   4403
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
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
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   25
            Left            =   4560
            TabIndex        =   294
            Top             =   4560
            Visible         =   0   'False
            Width           =   2500
            _Version        =   65536
            _ExtentX        =   4403
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
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
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   30
            Left            =   4560
            TabIndex        =   289
            Top             =   600
            Width           =   2500
            _Version        =   65536
            _ExtentX        =   4403
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
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
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   26
            Left            =   3720
            TabIndex        =   491
            Top             =   3000
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   25
            Left            =   3720
            TabIndex        =   348
            Top             =   4560
            Visible         =   0   'False
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   24
            Left            =   3720
            TabIndex        =   347
            Top             =   4320
            Visible         =   0   'False
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   23
            Left            =   3720
            TabIndex        =   346
            Top             =   4080
            Visible         =   0   'False
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   22
            Left            =   3720
            TabIndex        =   345
            Top             =   3840
            Visible         =   0   'False
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   27
            Left            =   3720
            TabIndex        =   344
            Top             =   2760
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   21
            Left            =   3720
            TabIndex        =   343
            Top             =   2520
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   20
            Left            =   3720
            TabIndex        =   342
            Top             =   2280
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   19
            Left            =   3720
            TabIndex        =   341
            Top             =   2040
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   18
            Left            =   3720
            TabIndex        =   340
            Top             =   1800
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   17
            Left            =   3720
            TabIndex        =   339
            Top             =   1080
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   16
            Left            =   3720
            TabIndex        =   338
            Top             =   840
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   15
            Left            =   240
            TabIndex        =   337
            Top             =   4560
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   14
            Left            =   240
            TabIndex        =   336
            Top             =   4320
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   13
            Left            =   240
            TabIndex        =   335
            Top             =   4080
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   12
            Left            =   240
            TabIndex        =   334
            Top             =   3840
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   11
            Left            =   240
            TabIndex        =   333
            Top             =   3480
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   10
            Left            =   240
            TabIndex        =   332
            Top             =   3240
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   9
            Left            =   240
            TabIndex        =   331
            Top             =   3000
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   8
            Left            =   240
            TabIndex        =   330
            Top             =   2760
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   7
            Left            =   240
            TabIndex        =   329
            Top             =   2400
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   6
            Left            =   240
            TabIndex        =   328
            Top             =   2160
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   5
            Left            =   240
            TabIndex        =   327
            Top             =   1920
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   4
            Left            =   240
            TabIndex        =   326
            Top             =   1680
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   3
            Left            =   240
            TabIndex        =   325
            Top             =   1320
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   2
            Left            =   240
            TabIndex        =   324
            Top             =   1080
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   1
            Left            =   240
            TabIndex        =   323
            Top             =   840
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   0
            Left            =   240
            TabIndex        =   322
            Top             =   600
            Width           =   804
         End
         Begin VB.Label Label4 
            Caption         =   "Ingestion"
            Height          =   252
            Left            =   240
            TabIndex        =   320
            Top             =   360
            Width           =   2500
         End
         Begin VB.Label Label1 
            Caption         =   "Inhalation"
            Height          =   252
            Left            =   3720
            TabIndex        =   293
            Top             =   360
            Width           =   2500
         End
         Begin VB.Label Label2 
            Caption         =   "External"
            Height          =   252
            Left            =   3720
            TabIndex        =   292
            Top             =   1560
            Width           =   2500
         End
         Begin VB.Label Label3 
            Caption         =   "Dermal contact"
            Height          =   252
            Left            =   3720
            TabIndex        =   291
            Top             =   3600
            Visible         =   0   'False
            Width           =   2500
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   30
            Left            =   3720
            TabIndex        =   290
            Top             =   600
            Width           =   804
         End
      End
      Begin TabDlg.SSTab SSTab1 
         Height          =   5292
         Index           =   3
         Left            =   -74880
         TabIndex        =   350
         TabStop         =   0   'False
         Top             =   360
         Width           =   7452
         _ExtentX        =   13150
         _ExtentY        =   9340
         _Version        =   393216
         Style           =   1
         Tabs            =   4
         TabsPerRow      =   4
         TabHeight       =   423
         TabCaption(0)   =   "General"
         TabPicture(0)   =   "GEN_EXP.frx":0A6E
         Tab(0).ControlEnabled=   -1  'True
         Tab(0).Control(0)=   "ag(0)"
         Tab(0).Control(0).Enabled=   0   'False
         Tab(0).ControlCount=   1
         TabCaption(1)   =   "Animal Feed"
         TabPicture(1)   =   "GEN_EXP.frx":0A8A
         Tab(1).ControlEnabled=   0   'False
         Tab(1).Control(0)=   "SSTab1(4)"
         Tab(1).Control(1)=   "fd"
         Tab(1).ControlCount=   2
         TabCaption(2)   =   "Food Crop"
         TabPicture(2)   =   "GEN_EXP.frx":0AA6
         Tab(2).ControlEnabled=   0   'False
         Tab(2).Control(0)=   "cp"
         Tab(2).Control(1)=   "SSTab1(5)"
         Tab(2).ControlCount=   2
         TabCaption(3)   =   "Intake delays"
         TabPicture(3)   =   "GEN_EXP.frx":0AC2
         Tab(3).ControlEnabled=   0   'False
         Tab(3).Control(0)=   "ag(3)"
         Tab(3).ControlCount=   1
         Begin Threed.SSFrame cp 
            Height          =   2052
            Left            =   -74640
            TabIndex        =   474
            Top             =   720
            Width           =   6732
            _Version        =   65536
            _ExtentX        =   11875
            _ExtentY        =   3620
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
            Enabled         =   0   'False
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   100
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   482
               Top             =   1440
               Width           =   850
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   100
               Left            =   3960
               TabIndex        =   481
               Top             =   1440
               Width           =   850
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   99
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   480
               Top             =   1080
               Width           =   850
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   99
               Left            =   3960
               TabIndex        =   479
               Top             =   1080
               Width           =   850
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   98
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   478
               Top             =   720
               Width           =   850
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   98
               Left            =   3960
               TabIndex        =   477
               Top             =   720
               Width           =   850
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   97
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   476
               Top             =   360
               Width           =   850
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   97
               Left            =   3960
               TabIndex        =   475
               Top             =   360
               Width           =   850
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   100
               Left            =   5760
               TabIndex        =   490
               Top             =   1440
               Width           =   948
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   100
               Left            =   240
               TabIndex        =   489
               Top             =   1440
               Width           =   3700
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   99
               Left            =   5760
               TabIndex        =   488
               Top             =   1080
               Width           =   948
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   99
               Left            =   240
               TabIndex        =   487
               Top             =   1080
               Width           =   3700
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   98
               Left            =   5760
               TabIndex        =   486
               Top             =   720
               Width           =   948
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   98
               Left            =   240
               TabIndex        =   485
               Top             =   720
               Width           =   3700
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   97
               Left            =   5760
               TabIndex        =   484
               Top             =   360
               Width           =   948
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   97
               Left            =   240
               TabIndex        =   483
               Top             =   360
               Width           =   3700
            End
         End
         Begin Threed.SSFrame fd 
            Height          =   3012
            Left            =   -74700
            TabIndex        =   449
            Top             =   1200
            Width           =   6492
            _Version        =   65536
            _ExtentX        =   11451
            _ExtentY        =   5313
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
            Enabled         =   0   'False
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   101
               Left            =   3600
               TabIndex        =   461
               Top             =   240
               Width           =   850
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   101
               Left            =   4440
               Style           =   2  'Dropdown List
               TabIndex        =   460
               Top             =   240
               Width           =   850
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   102
               Left            =   3600
               TabIndex        =   459
               Top             =   696
               Width           =   850
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   102
               Left            =   4440
               Style           =   2  'Dropdown List
               TabIndex        =   458
               Top             =   696
               Width           =   850
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   103
               Left            =   3600
               TabIndex        =   457
               Top             =   1152
               Width           =   850
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   103
               Left            =   4440
               Style           =   2  'Dropdown List
               TabIndex        =   456
               Top             =   1152
               Width           =   850
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   104
               Left            =   3600
               TabIndex        =   455
               Top             =   1608
               Width           =   850
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   104
               Left            =   4440
               Style           =   2  'Dropdown List
               TabIndex        =   454
               Top             =   1608
               Width           =   850
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   105
               Left            =   3600
               TabIndex        =   453
               Top             =   2064
               Width           =   850
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   105
               Left            =   4440
               Style           =   2  'Dropdown List
               TabIndex        =   452
               Top             =   2064
               Width           =   850
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   106
               Left            =   3600
               TabIndex        =   451
               Top             =   2520
               Width           =   850
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   106
               Left            =   4440
               Style           =   2  'Dropdown List
               TabIndex        =   450
               Top             =   2520
               Width           =   850
            End
            Begin VB.Label lbl 
               Height          =   470
               Index           =   101
               Left            =   140
               TabIndex        =   473
               Top             =   240
               Width           =   3252
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   101
               Left            =   5400
               TabIndex        =   472
               Top             =   240
               Width           =   948
            End
            Begin VB.Label lbl 
               Height          =   470
               Index           =   102
               Left            =   140
               TabIndex        =   471
               Top             =   696
               Width           =   3252
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   102
               Left            =   5400
               TabIndex        =   470
               Top             =   696
               Width           =   948
            End
            Begin VB.Label lbl 
               Height          =   470
               Index           =   103
               Left            =   140
               TabIndex        =   469
               Top             =   1152
               Width           =   3252
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   103
               Left            =   5400
               TabIndex        =   468
               Top             =   1152
               Width           =   948
            End
            Begin VB.Label lbl 
               Height          =   470
               Index           =   104
               Left            =   140
               TabIndex        =   467
               Top             =   1608
               Width           =   3252
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   104
               Left            =   5400
               TabIndex        =   466
               Top             =   1608
               Width           =   948
            End
            Begin VB.Label lbl 
               Height          =   470
               Index           =   105
               Left            =   140
               TabIndex        =   465
               Top             =   2064
               Width           =   3252
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   105
               Left            =   5376
               TabIndex        =   464
               Top             =   2064
               Width           =   948
            End
            Begin VB.Label lbl 
               Height          =   470
               Index           =   106
               Left            =   140
               TabIndex        =   463
               Top             =   2520
               Width           =   3252
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   106
               Left            =   5400
               TabIndex        =   462
               Top             =   2520
               Width           =   948
            End
         End
         Begin Threed.SSFrame ag 
            Height          =   4692
            Index           =   3
            Left            =   -74760
            TabIndex        =   351
            Top             =   360
            Width           =   6972
            _Version        =   65536
            _ExtentX        =   12298
            _ExtentY        =   8276
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
            Enabled         =   0   'False
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   38
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   375
               Top             =   1320
               Width           =   850
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   38
               Left            =   3960
               TabIndex        =   374
               Top             =   1320
               Width           =   850
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   37
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   373
               Top             =   960
               Width           =   850
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   37
               Left            =   3960
               TabIndex        =   372
               Top             =   960
               Width           =   850
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   32
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   371
               Top             =   600
               Width           =   850
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   32
               Left            =   3960
               TabIndex        =   370
               Top             =   600
               Width           =   850
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   31
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   369
               Top             =   240
               Width           =   850
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   31
               Left            =   3960
               TabIndex        =   368
               Top             =   240
               Width           =   850
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   3
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   367
               Top             =   1680
               Width           =   850
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   3
               Left            =   3960
               TabIndex        =   366
               Top             =   1680
               Width           =   850
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   4
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   365
               Top             =   2040
               Width           =   850
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   4
               Left            =   3960
               TabIndex        =   364
               Top             =   2040
               Width           =   850
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   5
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   363
               Top             =   2400
               Width           =   850
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   5
               Left            =   3960
               TabIndex        =   362
               Top             =   2400
               Width           =   850
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   6
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   361
               Top             =   2760
               Width           =   850
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   6
               Left            =   3960
               TabIndex        =   360
               Top             =   2760
               Width           =   850
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   59
               Left            =   3960
               TabIndex        =   359
               Top             =   3120
               Width           =   850
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   59
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   358
               Top             =   3120
               Width           =   850
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   78
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   357
               Top             =   3480
               Width           =   850
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   78
               Left            =   3960
               TabIndex        =   356
               Top             =   3480
               Width           =   850
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   79
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   355
               Top             =   3840
               Width           =   850
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   79
               Left            =   3960
               TabIndex        =   354
               Top             =   3840
               Width           =   850
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   80
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   353
               Top             =   4200
               Width           =   850
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   80
               Left            =   3960
               TabIndex        =   352
               Top             =   4200
               Width           =   850
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   38
               Left            =   5760
               TabIndex        =   399
               Top             =   1320
               Width           =   948
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   38
               Left            =   240
               TabIndex        =   398
               Top             =   1320
               Width           =   3732
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   37
               Left            =   5760
               TabIndex        =   397
               Top             =   960
               Width           =   948
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   37
               Left            =   240
               TabIndex        =   396
               Top             =   960
               Width           =   3732
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   32
               Left            =   5760
               TabIndex        =   395
               Top             =   600
               Width           =   948
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   32
               Left            =   240
               TabIndex        =   394
               Top             =   600
               Width           =   3732
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   31
               Left            =   5760
               TabIndex        =   393
               Top             =   240
               Width           =   948
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   31
               Left            =   240
               TabIndex        =   392
               Top             =   240
               Width           =   3732
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   3
               Left            =   5760
               TabIndex        =   391
               Top             =   1680
               Width           =   948
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   3
               Left            =   240
               TabIndex        =   390
               Top             =   1680
               Width           =   3732
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   4
               Left            =   5760
               TabIndex        =   389
               Top             =   2040
               Width           =   948
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   4
               Left            =   240
               TabIndex        =   388
               Top             =   2040
               Width           =   3732
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   5
               Left            =   5760
               TabIndex        =   387
               Top             =   2400
               Width           =   948
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   5
               Left            =   240
               TabIndex        =   386
               Top             =   2400
               Width           =   3732
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   6
               Left            =   5760
               TabIndex        =   385
               Top             =   2760
               Width           =   948
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   6
               Left            =   240
               TabIndex        =   384
               Top             =   2760
               Width           =   3732
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   59
               Left            =   240
               TabIndex        =   383
               Top             =   3120
               Width           =   3732
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   59
               Left            =   5760
               TabIndex        =   382
               Top             =   3120
               Width           =   948
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   78
               Left            =   5760
               TabIndex        =   381
               Top             =   3480
               Width           =   948
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   78
               Left            =   240
               TabIndex        =   380
               Top             =   3480
               Width           =   3732
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   79
               Left            =   5760
               TabIndex        =   379
               Top             =   3840
               Width           =   948
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   79
               Left            =   240
               TabIndex        =   378
               Top             =   3840
               Width           =   3732
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   80
               Left            =   5760
               TabIndex        =   377
               Top             =   4200
               Width           =   948
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   80
               Left            =   240
               TabIndex        =   376
               Top             =   4200
               Width           =   3732
            End
         End
         Begin Threed.SSFrame ag 
            Height          =   4692
            Index           =   0
            Left            =   240
            TabIndex        =   400
            Top             =   360
            Width           =   6972
            _Version        =   65536
            _ExtentX        =   12298
            _ExtentY        =   8276
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
            Enabled         =   0   'False
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   94
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   410
               Top             =   1800
               Width           =   972
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   94
               Left            =   3840
               TabIndex        =   409
               Top             =   1800
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   96
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   408
               Top             =   3120
               Width           =   972
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   96
               Left            =   3840
               TabIndex        =   407
               Top             =   3120
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   95
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   406
               Top             =   2640
               Width           =   972
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   95
               Left            =   3840
               TabIndex        =   405
               Top             =   2640
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   93
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   404
               Top             =   960
               Width           =   972
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   93
               Left            =   3840
               TabIndex        =   403
               Top             =   960
               Width           =   972
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   138
               Left            =   3840
               TabIndex        =   402
               Top             =   3600
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   138
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   401
               Top             =   3600
               Width           =   972
            End
            Begin Threed.SSCheck SSCheck1 
               Height          =   255
               Index           =   38
               Left            =   1080
               TabIndex        =   417
               Top             =   1440
               Width           =   4815
               _Version        =   65536
               _ExtentX        =   8493
               _ExtentY        =   450
               _StockProps     =   78
               Caption         =   "SSCheck1"
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
            Begin Threed.SSCheck SSCheck1 
               Height          =   255
               Index           =   37
               Left            =   1080
               TabIndex        =   415
               Top             =   600
               Width           =   4815
               _Version        =   65536
               _ExtentX        =   8493
               _ExtentY        =   450
               _StockProps     =   78
               Caption         =   "SSCheck1"
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
            Begin Threed.SSCheck SSCheck1 
               Height          =   252
               Index           =   39
               Left            =   1080
               TabIndex        =   414
               Top             =   240
               Width           =   4092
               _Version        =   65536
               _ExtentX        =   7218
               _ExtentY        =   445
               _StockProps     =   78
               Caption         =   "SSCheck1"
               BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
                  Name            =   "MS Sans Serif"
                  Size            =   8.25
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Value           =   -1  'True
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   94
               Left            =   5880
               TabIndex        =   426
               Top             =   1800
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   94
               Left            =   480
               TabIndex        =   425
               Top             =   1800
               Width           =   3372
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   96
               Left            =   5880
               TabIndex        =   424
               Top             =   3120
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   96
               Left            =   240
               TabIndex        =   423
               Top             =   3120
               Width           =   3612
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   95
               Left            =   5880
               TabIndex        =   422
               Top             =   2640
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   95
               Left            =   240
               TabIndex        =   421
               Top             =   2640
               Width           =   3612
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   93
               Left            =   5880
               TabIndex        =   420
               Top             =   960
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   93
               Left            =   480
               TabIndex        =   419
               Top             =   960
               Width           =   3372
            End
            Begin VB.Label cref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   38
               Left            =   240
               TabIndex        =   418
               Top             =   1440
               Width           =   804
            End
            Begin VB.Label cref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   37
               Left            =   240
               TabIndex        =   416
               Top             =   600
               Width           =   804
            End
            Begin VB.Label cref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   39
               Left            =   240
               TabIndex        =   413
               Top             =   240
               Width           =   804
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   138
               Left            =   240
               TabIndex        =   412
               Top             =   3600
               Width           =   3612
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   138
               Left            =   5880
               TabIndex        =   411
               Top             =   3600
               Width           =   972
            End
         End
         Begin TabDlg.SSTab SSTab1 
            Height          =   4092
            Index           =   4
            Left            =   -74880
            TabIndex        =   427
            TabStop         =   0   'False
            Top             =   360
            Width           =   7212
            _ExtentX        =   12726
            _ExtentY        =   7223
            _Version        =   393216
            Style           =   1
            Tabs            =   10
            TabsPerRow      =   5
            TabHeight       =   420
            TabCaption(0)   =   "Biomass"
            TabPicture(0)   =   "GEN_EXP.frx":0ADE
            Tab(0).ControlEnabled=   -1  'True
            Tab(0).ControlCount=   0
            TabCaption(1)   =   "Consumption"
            TabPicture(1)   =   "GEN_EXP.frx":0AFA
            Tab(1).ControlEnabled=   0   'False
            Tab(1).ControlCount=   0
            TabCaption(2)   =   "Storage Time"
            TabPicture(2)   =   "GEN_EXP.frx":0B16
            Tab(2).ControlEnabled=   0   'False
            Tab(2).ControlCount=   0
            TabCaption(3)   =   "Diet Fraction"
            TabPicture(3)   =   "GEN_EXP.frx":0B32
            Tab(3).ControlEnabled=   0   'False
            Tab(3).ControlCount=   0
            TabCaption(4)   =   "Growing Period"
            TabPicture(4)   =   "GEN_EXP.frx":0B4E
            Tab(4).ControlEnabled=   0   'False
            Tab(4).ControlCount=   0
            TabCaption(5)   =   "Yield"
            TabPicture(5)   =   "GEN_EXP.frx":0B6A
            Tab(5).ControlEnabled=   0   'False
            Tab(5).ControlCount=   0
            TabCaption(6)   =   "Dry/Wet Ratio"
            TabPicture(6)   =   "GEN_EXP.frx":0B86
            Tab(6).ControlEnabled=   0   'False
            Tab(6).ControlCount=   0
            TabCaption(7)   =   "Translocation Factor"
            TabPicture(7)   =   "GEN_EXP.frx":0BA2
            Tab(7).ControlEnabled=   0   'False
            Tab(7).ControlCount=   0
            TabCaption(8)   =   "Acute Forage"
            TabPicture(8)   =   "GEN_EXP.frx":0BBE
            Tab(8).ControlEnabled=   0   'False
            Tab(8).ControlCount=   0
            TabCaption(9)   =   "Soil Intake"
            TabPicture(9)   =   "GEN_EXP.frx":0BDA
            Tab(9).ControlEnabled=   0   'False
            Tab(9).ControlCount=   0
         End
         Begin TabDlg.SSTab SSTab1 
            Height          =   2652
            Index           =   5
            Left            =   -74880
            TabIndex        =   428
            TabStop         =   0   'False
            Top             =   360
            Width           =   7212
            _ExtentX        =   12726
            _ExtentY        =   4683
            _Version        =   393216
            Style           =   1
            Tabs            =   5
            TabsPerRow      =   5
            TabHeight       =   420
            TabCaption(0)   =   "Biomass"
            TabPicture(0)   =   "GEN_EXP.frx":0BF6
            Tab(0).ControlEnabled=   -1  'True
            Tab(0).ControlCount=   0
            TabCaption(1)   =   "Growing Period"
            TabPicture(1)   =   "GEN_EXP.frx":0C12
            Tab(1).ControlEnabled=   0   'False
            Tab(1).ControlCount=   0
            TabCaption(2)   =   "Yield"
            TabPicture(2)   =   "GEN_EXP.frx":0C2E
            Tab(2).ControlEnabled=   0   'False
            Tab(2).ControlCount=   0
            TabCaption(3)   =   "Dry/Wet Ratio"
            TabPicture(3)   =   "GEN_EXP.frx":0C4A
            Tab(3).ControlEnabled=   0   'False
            Tab(3).ControlCount=   0
            TabCaption(4)   =   "Translocation Factor"
            TabPicture(4)   =   "GEN_EXP.frx":0C66
            Tab(4).ControlEnabled=   0   'False
            Tab(4).ControlCount=   0
         End
      End
      Begin Threed.SSFrame frm 
         Height          =   5172
         Index           =   3
         Left            =   -74760
         TabIndex        =   429
         Top             =   360
         Width           =   7212
         _Version        =   65536
         _ExtentX        =   12721
         _ExtentY        =   9123
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
         Enabled         =   0   'False
         Begin VB.TextBox txt 
            Height          =   288
            Index           =   146
            Left            =   3960
            TabIndex        =   445
            Top             =   3720
            Width           =   850
         End
         Begin VB.ComboBox unt 
            Height          =   288
            Index           =   146
            Left            =   4800
            Style           =   2  'Dropdown List
            TabIndex        =   444
            Top             =   3720
            Width           =   850
         End
         Begin VB.TextBox txt 
            Height          =   288
            Index           =   145
            Left            =   3960
            TabIndex        =   441
            Top             =   3360
            Width           =   850
         End
         Begin VB.ComboBox unt 
            Height          =   288
            Index           =   145
            Left            =   4800
            Style           =   2  'Dropdown List
            TabIndex        =   440
            Top             =   3360
            Width           =   850
         End
         Begin VB.TextBox txt 
            Height          =   288
            Index           =   144
            Left            =   3960
            TabIndex        =   437
            Top             =   3000
            Width           =   850
         End
         Begin VB.ComboBox unt 
            Height          =   288
            Index           =   144
            Left            =   4800
            Style           =   2  'Dropdown List
            TabIndex        =   436
            Top             =   3000
            Width           =   850
         End
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   42
            Left            =   1080
            TabIndex        =   434
            Top             =   2520
            Width           =   5000
            _Version        =   65536
            _ExtentX        =   8827
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Value           =   -1  'True
         End
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   41
            Left            =   1080
            TabIndex        =   432
            Top             =   2160
            Width           =   5000
            _Version        =   65536
            _ExtentX        =   8827
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Value           =   -1  'True
         End
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   40
            Left            =   1080
            TabIndex        =   430
            Top             =   1800
            Width           =   5000
            _Version        =   65536
            _ExtentX        =   8827
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Value           =   -1  'True
         End
         Begin VB.Label Label5 
            Caption         =   $"GEN_EXP.frx":0C82
            Height          =   1452
            Left            =   240
            TabIndex        =   448
            Top             =   360
            Width           =   6372
         End
         Begin VB.Label lbl 
            Height          =   252
            Index           =   146
            Left            =   240
            TabIndex        =   447
            Top             =   3720
            Width           =   3372
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   146
            Left            =   5760
            TabIndex        =   446
            Top             =   3720
            Width           =   948
         End
         Begin VB.Label lbl 
            Height          =   252
            Index           =   145
            Left            =   240
            TabIndex        =   443
            Top             =   3360
            Width           =   3372
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   145
            Left            =   5760
            TabIndex        =   442
            Top             =   3360
            Width           =   948
         End
         Begin VB.Label lbl 
            Height          =   252
            Index           =   144
            Left            =   240
            TabIndex        =   439
            Top             =   3000
            Width           =   3372
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   144
            Left            =   5760
            TabIndex        =   438
            Top             =   3000
            Width           =   948
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   42
            Left            =   240
            TabIndex        =   435
            Top             =   2520
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   41
            Left            =   240
            TabIndex        =   433
            Top             =   2160
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   40
            Left            =   240
            TabIndex        =   431
            Top             =   1800
            Width           =   804
         End
      End
   End
   Begin VB.Menu file 
      Caption         =   "&File"
      Begin VB.Menu save 
         Caption         =   "&Save && Exit"
      End
      Begin VB.Menu leave 
         Caption         =   "E&xit"
      End
   End
   Begin VB.Menu def 
      Caption         =   "&Defaults"
      Begin VB.Menu mi 
         Caption         =   "&Maximum Individual"
         Checked         =   -1  'True
      End
      Begin VB.Menu ai 
         Caption         =   "&Average Individual"
      End
      Begin VB.Menu ud 
         Caption         =   "User Defined"
      End
   End
   Begin VB.Menu mnuRef 
      Caption         =   "&Reference"
      Begin VB.Menu addref 
         Caption         =   "&Add"
      End
      Begin VB.Menu selref 
         Caption         =   "Se&lect"
      End
   End
   Begin VB.Menu hlp 
      Caption         =   "&Help"
      Begin VB.Menu howto 
         Caption         =   "&General"
      End
      Begin VB.Menu contxt 
         Caption         =   "&Context"
         Shortcut        =   {F1}
      End
      Begin VB.Menu rng 
         Caption         =   "&Range"
         Shortcut        =   {F2}
      End
   End
End
Attribute VB_Name = "gen_exp"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Dim uiname As String
Dim t1 As Long
Dim t2 As Long
Dim t3 As Integer
Dim oldck As Long

Private Sub WriteDesLabels(fl As parmfile)
Dim parm As parmrec
   set_parm parm, "ANFLabel", 1, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "meat animal"
   write_parmrec fle, parm
   set_parm parm, "ANFLabel", 2, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "poultry animal"
   write_parmrec fle, parm
   set_parm parm, "ANFLabel", 3, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "milk animal"
   write_parmrec fle, parm
   set_parm parm, "ANFLabel", 4, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "egg animal"
   write_parmrec fle, parm
   set_parm parm, "TFDLabel", 1, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "leafy vegetables"
   write_parmrec fle, parm
   set_parm parm, "TFDLabel", 2, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "root vegetables"
   write_parmrec fle, parm
   set_parm parm, "TFDLabel", 3, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "fruits"
   write_parmrec fle, parm
   set_parm parm, "TFDLabel", 4, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "grains"
   write_parmrec fle, parm
   set_parm parm, "AQFLabel", 1, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "fish"
   write_parmrec fle, parm
   set_parm parm, "AQFLabel", 2, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "mollusca"
   write_parmrec fle, parm
   set_parm parm, "AQFLabel", 3, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "crustacea"
   write_parmrec fle, parm
   set_parm parm, "AQFLabel", 4, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "aquatic plants"
   write_parmrec fle, parm
   set_parm parm, "FdFrLabel", 1, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "meat animal feed"
   write_parmrec fle, parm
   set_parm parm, "FdFrLabel", 2, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "poultry animal feed"
   write_parmrec fle, parm
   set_parm parm, "FdFrLabel", 3, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "milk animal feed"
   write_parmrec fle, parm
   set_parm parm, "FdFrLabel", 4, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "egg animal feed"
   write_parmrec fle, parm
   set_parm parm, "FdFrLabel", 5, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "meat animal forage"
   write_parmrec fle, parm
   set_parm parm, "FdFrLabel", 6, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "milk animal forage"
   write_parmrec fle, parm
End Sub

Private Sub ai_Click()
  Loading.Show
  loaddef 2
  ai.Checked = True
  mi.Checked = False
  ud.Checked = False
  SSTab1_Click t3, t3
  Unload Loading
End Sub

Private Sub mi_Click()
  Loading.Show
  loaddef 1
  mi.Checked = True
  ai.Checked = False
  ud.Checked = False
  SSTab1_Click t3, t3
  Unload Loading
End Sub

Private Sub howto_Click()
  GetHelp
End Sub

Private Sub contxt_Click()
  GetHelp
End Sub

Private Sub rng_Click()
  ui(uiname).Help
End Sub

Private Sub selref_Click()
  RefMode = 0
  GetRef rfs
  If RefIdx >= 0 Then ui(uiname).refnum = RefIdx
End Sub

Private Sub addref_Click()
  RefMode = 1
  GetRef rfs
  If RefIdx >= 0 Then ui(uiname).refnum = RefIdx
End Sub

Sub generaltab()
  Dim i As Long
  
  If acute Then
    gn_t(2).Init "ACUTIM", "Duration of acute exposure", lbl(2), txt(2), unt(2), ref(2), "yr", 0, 1, 0.002738
  'Acute forage feed fractions
    gn_t(143).Init "FRACFR", "Fraction of meat animal diet that is fresh forage in autumn", lbl(101), txt(101), unt(101), ref(101), "fraction", 0, 1, 0#, 1
    gn_t(144).Init "FRACFR", "Fraction of milk animal diet that is fresh forage in autumn", lbl(102), txt(102), unt(102), ref(102), "fraction", 0, 1, 0#, 3
    gn_t(145).Init "PROBAIR", "Probability level for air concentration", lbl(144), txt(144), unt(144), ref(144), "percent", 0, 100, 95, 2
    gn_t(146).Init "PROBDEP", "Probability level for deposition rate", lbl(145), txt(145), unt(145), ref(145), "percent", 0, 100, 95, 3
    gn_t(147).Init "PROBEXT", "Probability level for external dose", lbl(146), txt(146), unt(146), ref(146), "percent", 0, 100, 95, 4
    gn_l(40).Init "AIRPROB", "Use air concentration by probability then by specific value", SSCheck1(40), cref(40)
    gn_l(41).Init "DEPPROB", "Use deposition rate by probability then by specific value", SSCheck1(41), cref(41)
    gn_l(42).Init "EXTPROB", "Use external dose by probability then by specific value", SSCheck1(42), cref(42)
    gn_l(1).Add gn_t(139):    gn_l(1).Add gn_t(140)
    For i = 60 To 70
      If i = 61 Then i = 69
      lbl(i).Visible = False
      txt(i).Visible = False
      unt(i).Visible = False
      ref(i).Visible = False
    Next
  Else
    gn_t(2).Init "BEFORE", "Time from start to exposure", lbl(2), txt(2), unt(2), ref(2), "yr", 0, 1000000, 0
    ReDim gn_i(2) As New Whole
    SSTab1(4).TabVisible(8) = False
    gn_i(0).Init "LOIC", "Loss of institutional control time prior to exposure", lbl(70), txt(70), unt(70), ref(70), "yr", 0, gn_t(2), 0
    gn_i(1).Init "BEFIRR", "Irrigation water deposition time prior to exposure", lbl(60), txt(60), unt(60), ref(60), "yr", 0, gn_t(2), 0
    gn_i(2).Init "BEFAIR", "Air deposition time prior to exposure", lbl(69), txt(69), unt(69), ref(69), "yr", 0, gn_t(2), 0
  End If
  
 
  cb(14).AddItem "No Resuspension model":                   cb(14).ItemData(cb(14).NewIndex) = 0
  cb(14).AddItem "Use mass loading model":                  cb(14).ItemData(cb(14).NewIndex) = 1
  cb(14).AddItem "Use Anspaugh model":                      cb(14).ItemData(cb(14).NewIndex) = 2
  cb(14).AddItem "User input of resuspension factor":       cb(14).ItemData(cb(14).NewIndex) = 3
  gn_s(0).Init "IRES", "Type of model to run", blbl(14), cb(14), bref(14)
  
  gn_t(0).Init "NTKEND", "Duration of exposure period", lbl(0), txt(0), unt(0), ref(0), "yr", 0, 100, 1
  gn_t(1).Init "RELEND", "End of release period", lbl(1), txt(1), unt(1), ref(1), "yr", 0, 1000000, 1
  gn_t(4).Init "RF1", "Fraction of plants roots in surface soil", lbl(71), txt(71), unt(71), ref(71), "fraction", 0, 1, 1
  
  gn_t(5).Init "XMLF", "Mass loading factor for resuspension model", lbl(72), txt(72), unt(72), ref(72), "g/m^3", 0, 5, 0.00005
  gn_t(6).Init "AVALSL", "Depth of top soil available for resuspension", lbl(73), txt(73), unt(73), ref(73), "cm", 0.01, 200, 1
  gn_t(79).Init "RESFAC", "Resuspension factor", lbl(89), txt(89), unt(89), ref(89), "1/m", 0, 0.001, 0.000000001
  gn_t(82).Init "RAIN", "Average rain rate, when raining", lbl(82), txt(82), unt(82), ref(82), "mm/d", 0.01, 500, 1
  
  gn_l(1).Init "ANFOOD", "Animal product ingestion", SSCheck1(35), cref(35)
  gn_l(2).Init "TFOOD", "Terrestrial food crop ingestion", SSCheck1(34), cref(34)
  gn_l(3).Init "AQFOOD", "Aquatic food ingestion", SSCheck1(33), cref(33)
  gn_l(4).Init "RECRE", "Recreational surface water", SSCheck1(32), cref(32)
  gn_l(5).Init "DEBUG", "Debug testing", SSCheck1(31), cref(31)
  
  gn_t(3).Init "ABSHUM", "Absolute humidity, used only for tritium model", lbl(137), txt(137), unt(137), ref(137), "kg/m^3", 0, 0.1, 0.008
  
  For i = 1 To 6
    gn_l(1).Add gn_s(i)
  Next
  For i = 7 To 10
    gn_l(2).Add gn_s(i)
  Next
  'Feed biomass
  gn_t(103).Init "BIOMA2", "Standing biomass (wet) for meat animal feed", lbl(101), txt(101), unt(101), ref(101), "kg/m^2", 0.1, 10, 1.6, 1
  gn_t(104).Init "BIOMA2", "Standing biomass (wet) for poultry animal feed", lbl(102), txt(102), unt(102), ref(102), "kg/m^2", 0.1, 10, 1.6, 2
  gn_t(105).Init "BIOMA2", "Standing biomass (wet) for milk animal feed", lbl(103), txt(103), unt(103), ref(103), "kg/m^2", 0.1, 10, 1, 3
  gn_t(106).Init "BIOMA2", "Standing biomass (wet) for egg animal feed", lbl(104), txt(104), unt(104), ref(104), "kg/m^2", 0.1, 10, 1.6, 4
  gn_t(107).Init "BIOMA2", "Standing biomass (wet) for meat animal forage", lbl(105), txt(105), unt(105), ref(105), "kg/m^2", 0.1, 10, 1, 5
  gn_t(108).Init "BIOMA2", "Standing biomass (wet) for milk animal forage", lbl(106), txt(106), unt(106), ref(106), "kg/m^2", 0.1, 10, 1.5, 6
  gn_l(1).Add gn_t(103):    gn_l(1).Add gn_t(104):    gn_l(1).Add gn_t(105):    gn_l(1).Add gn_t(106):    gn_l(1).Add gn_t(107):    gn_l(1).Add gn_t(108)
  'Feed consumption
  gn_t(109).Init "CONSUM", "Meat feed consumption rate (grain)", lbl(101), txt(101), unt(101), ref(101), "kg/d", 0#, 300, 17, 1
  gn_t(110).Init "CONSUM", "Poultry feed consumption rate (grain)", lbl(102), txt(102), unt(102), ref(102), "kg/d", 0#, 5, 0.12, 2
  gn_t(111).Init "CONSUM", "Milk cow feed consumption rate (hay)", lbl(103), txt(103), unt(103), ref(103), "kg/d", 0#, 300, 14, 3
  gn_t(112).Init "CONSUM", "Egg hen feed consumption rate (grain)", lbl(104), txt(104), unt(104), ref(104), "kg/d", 0#, 5, 0.12, 4
  gn_t(113).Init "CONSUM", "Meat forage consumption rate (hay)", lbl(105), txt(105), unt(105), ref(105), "kg/d", 0#, 300, 51, 5
  gn_t(114).Init "CONSUM", "Milk cow forage consumption rate (grass)", lbl(106), txt(106), unt(106), ref(106), "kg/d", 0#, 300, 41, 6
  gn_l(1).Add gn_t(109):    gn_l(1).Add gn_t(110):    gn_l(1).Add gn_t(111):    gn_l(1).Add gn_t(112):    gn_l(1).Add gn_t(113):    gn_l(1).Add gn_t(114)
  'feed delay times  tab 0, tab 1
  gn_t(23).Init "STORTM", "Storage time for meat animal feed", lbl(101), txt(101), unt(101), ref(101), "day", 0, 365, 90, 1
  gn_t(24).Init "STORTM", "Storage time for poultry animal feed", lbl(102), txt(102), unt(102), ref(102), "day", 0, 365, 90, 2
  gn_t(25).Init "STORTM", "Storage time for milk animal feed", lbl(103), txt(103), unt(103), ref(103), "day", 0, 365, 90, 3
  gn_t(26).Init "STORTM", "Storage time for egg animal feed", lbl(104), txt(104), unt(104), ref(104), "day", 0, 365, 90, 4
  gn_t(27).Init "STORTM", "Storage time for meat animal forage", lbl(105), txt(105), unt(105), ref(105), "day", 0, 365, 90, 5
  gn_t(28).Init "STORTM", "Storage time for milk animal forage", lbl(106), txt(106), unt(106), ref(106), "day", 0, 365, 0, 6
  gn_l(1).Add gn_t(23):    gn_l(1).Add gn_t(24):    gn_l(1).Add gn_t(25):    gn_l(1).Add gn_t(26):    gn_l(1).Add gn_t(27):    gn_l(1).Add gn_t(28)
  'Feed fractions
  gn_t(29).Init "DIETFR", "Fraction of meat animal feed that is contaminated", lbl(101), txt(101), unt(101), ref(101), "fraction", 0, 1, 1, 1
  gn_t(30).Init "DIETFR", "Fraction of poultry animal feed that is contaminated", lbl(102), txt(102), unt(102), ref(102), "fraction", 0, 1, 1, 2
  gn_t(31).Init "DIETFR", "Fraction of milk animal feed that is contaminated", lbl(103), txt(103), unt(103), ref(103), "fraction", 0, 1, 1, 3
  gn_t(32).Init "DIETFR", "Fraction of egg animal feed that is contaminated", lbl(104), txt(104), unt(104), ref(104), "fraction", 0, 1, 1, 4
  gn_t(33).Init "DIETFR", "Fraction of meat animal forage that is contaminated", lbl(105), txt(105), unt(105), ref(105), "fraction", 0, 1, 1, 5
  gn_t(34).Init "DIETFR", "Fraction of milk animal forage that is contaminated", lbl(106), txt(106), unt(106), ref(106), "fraction", 0, 1, 1, 6
  gn_l(1).Add gn_t(29):    gn_l(1).Add gn_t(30):    gn_l(1).Add gn_t(31):    gn_l(1).Add gn_t(32):    gn_l(1).Add gn_t(33):    gn_l(1).Add gn_t(34)
  'Feed growing periods
  gn_t(17).Init "GRWPA", "Growing period for meat animal feed", lbl(101), txt(101), unt(101), ref(101), "day", 0, 365, 30, 1
  gn_t(18).Init "GRWPA", "Growing period for poultry animal feed", lbl(102), txt(102), unt(102), ref(102), "day", 0, 365, 30, 2
  gn_t(19).Init "GRWPA", "Growing period for milk animal feed", lbl(103), txt(103), unt(103), ref(103), "day", 0, 365, 30, 3
  gn_t(20).Init "GRWPA", "Growing period for egg animal feed", lbl(104), txt(104), unt(104), ref(104), "day", 0, 365, 30, 4
  gn_t(21).Init "GRWPA", "Growing period for meat animal forage", lbl(105), txt(105), unt(105), ref(105), "day", 0, 365, 30, 5
  gn_t(22).Init "GRWPA", "Growing period for milk animal forage", lbl(106), txt(106), unt(106), ref(106), "day", 0, 365, 30, 6
  gn_l(1).Add gn_t(17):    gn_l(1).Add gn_t(18):    gn_l(1).Add gn_t(19):    gn_l(1).Add gn_t(20):    gn_l(1).Add gn_t(21):    gn_l(1).Add gn_t(22)
  'Animal feed yeilds
  gn_t(7).Init "YELDA", "Yield of meat animal feed", lbl(101), txt(101), unt(101), ref(101), "kg/m^2", 0.1, 10, 0.8, 1
  gn_t(8).Init "YELDA", "Yield of poultry animal feed", lbl(102), txt(102), unt(102), ref(102), "kg/m^2", 0.1, 10, 0.8, 2
  gn_t(9).Init "YELDA", "Yield of milk animal feed", lbl(103), txt(103), unt(103), ref(103), "kg/m^2", 0.1, 10, 2, 3
  gn_t(10).Init "YELDA", "Yield of egg animal feed", lbl(104), txt(104), unt(104), ref(104), "kg/m^2", 0.1, 10, 0.8, 4
  gn_t(11).Init "YELDA", "Yield of meat animal forage", lbl(105), txt(105), unt(105), ref(105), "kg/m^2", 0.1, 10, 2, 5
  gn_t(12).Init "YELDA", "Yield of milk animal forage", lbl(106), txt(106), unt(106), ref(106), "kg/m^2", 0.1, 10, 0.8, 6
  gn_l(1).Add gn_t(7):    gn_l(1).Add gn_t(8):    gn_l(1).Add gn_t(9):    gn_l(1).Add gn_t(10):    gn_l(1).Add gn_t(11):    gn_l(1).Add gn_t(12)
  'Feed translocation factors
  gn_t(129).Init "DRYFA2", "Dry/wet weight ratio for meat animal feed", lbl(101), txt(101), unt(101), ref(101), "fraction", 0.05, 0.95, 0.88, 1
  gn_t(130).Init "DRYFA2", "Dry/wet weight ratio for poultry animal feed", lbl(102), txt(102), unt(102), ref(102), "fraction", 0.05, 0.95, 0.88, 2
  gn_t(131).Init "DRYFA2", "Dry/wet weight ratio for milk animal feed", lbl(103), txt(103), unt(103), ref(103), "fraction", 0.05, 0.95, 0.2, 3
  gn_t(132).Init "DRYFA2", "Dry/wet weight ratio for egg animal feed", lbl(104), txt(104), unt(104), ref(104), "fraction", 0.05, 0.95, 0.2, 4
  gn_t(133).Init "DRYFA2", "Dry/wet weight ratio for meat animal forage", lbl(105), txt(105), unt(105), ref(105), "fraction", 0.05, 0.95, 0.88, 5
  gn_t(134).Init "DRYFA2", "Dry/wet weight ratio for milk animal forage", lbl(106), txt(106), unt(106), ref(106), "fraction", 0.05, 0.95, 0.2, 6
  gn_l(1).Add gn_t(129):    gn_l(1).Add gn_t(130):    gn_l(1).Add gn_t(131):    gn_l(1).Add gn_t(132):    gn_l(1).Add gn_t(133):    gn_l(1).Add gn_t(134)
  'Feed translocation factors
  gn_t(119).Init "TRANSA", "Translocation factor of feed plant for meat animal", lbl(101), txt(101), unt(101), ref(101), "fraction", 0, 1, 0.1, 1
  gn_t(120).Init "TRANSA", "Translocation factor of feed plant for poultry animal", lbl(102), txt(102), unt(102), ref(102), "fraction", 0, 1, 0.1, 2
  gn_t(121).Init "TRANSA", "Translocation factor of feed plant for milk animal", lbl(103), txt(103), unt(103), ref(103), "fraction", 0, 1, 1, 3
  gn_t(122).Init "TRANSA", "Translocation factor of feed plant for egg animal", lbl(104), txt(104), unt(104), ref(104), "fraction", 0, 1, 0.1, 4
  gn_t(123).Init "TRANSA", "Translocation factor of forage plant for meat animal", lbl(105), txt(105), unt(105), ref(105), "fraction", 0, 1, 1, 5
  gn_t(124).Init "TRANSA", "Translocation factor of forage plant for milk animal", lbl(106), txt(106), unt(106), ref(106), "fraction", 0, 1, 1, 6
  gn_l(1).Add gn_t(119):    gn_l(1).Add gn_t(120):    gn_l(1).Add gn_t(121):    gn_l(1).Add gn_t(122):    gn_l(1).Add gn_t(123):    gn_l(1).Add gn_t(124)
 'added by BAN to allow soil intake
 'Animal soil ingestion factors
  gn_t(139).Init "SLCONA", "Soil intake rate for meat animal", lbl(101), txt(101), unt(101), ref(101), "kg/d", 0, 1, 0.1, 1
  gn_t(140).Init "SLCONA", "Soil intake rate for poultry animal", lbl(102), txt(102), unt(102), ref(102), "kg/d", 0, 1, 0.1, 2
  gn_t(141).Init "SLCONA", "Soil intake rate for milk animal", lbl(103), txt(103), unt(103), ref(103), "kg/d", 0, 1, 0.1, 3
  gn_t(142).Init "SLCONA", "Soil intake rate for egg animal", lbl(104), txt(104), unt(104), ref(104), "kg/d", 0, 1, 0.1, 4
  gn_l(1).Add gn_t(139):    gn_l(1).Add gn_t(140):    gn_l(1).Add gn_t(141):    gn_l(1).Add gn_t(142)
  
  'Food crop biomass
  gn_t(115).Init "BIOMAS", "Standing biomass (wet) for leafy vegetables", lbl(97), txt(97), unt(97), ref(97), "kg/m^2", 0.1, 10, 2, 1
  gn_t(116).Init "BIOMAS", "Standing biomass (wet) for root vegetables", lbl(98), txt(98), unt(98), ref(98), "kg/m^2", 0.1, 10, 2, 2
  gn_t(117).Init "BIOMAS", "Standing biomass (wet) for fruits", lbl(99), txt(99), unt(99), ref(99), "kg/m^2", 0.1, 10, 3, 3
  gn_t(118).Init "BIOMAS", "Standing biomass (wet) for grains", lbl(100), txt(100), unt(100), ref(100), "kg/m^2", 0.1, 10, 1.6, 4
  gn_l(2).Add gn_t(115):    gn_l(2).Add gn_t(116):    gn_l(2).Add gn_t(117):    gn_l(2).Add gn_t(118)
  'Food crop growing periods tab 0, tab 4
  gn_t(43).Init "GRWP", "Growing period for leafy vegetables", lbl(97), txt(97), unt(97), ref(97), "day", 0, 365, 60, 1
  gn_t(44).Init "GRWP", "Growing period for root vegetables", lbl(98), txt(98), unt(98), ref(98), "day", 0, 365, 60, 2
  gn_t(45).Init "GRWP", "Growing period for fruits", lbl(99), txt(99), unt(99), ref(99), "day", 0, 365, 90, 3
  gn_t(46).Init "GRWP", "Growing period for grains", lbl(100), txt(100), unt(100), ref(100), "day", 0, 365, 90, 4
  gn_l(2).Add gn_t(43):    gn_l(2).Add gn_t(44):    gn_l(2).Add gn_t(45):    gn_l(2).Add gn_t(46)
  'Food crop yields
  gn_t(35).Init "YELD", "Yield of leafy vegetables", lbl(97), txt(97), unt(97), ref(97), "kg/m^2", 0.001, 10, 0.72, 1
  gn_t(36).Init "YELD", "Yield of root vegetables", lbl(98), txt(98), unt(98), ref(98), "kg/m^2", 0.001, 10, 0.72, 2
  gn_t(37).Init "YELD", "Yield of fruits", lbl(99), txt(99), unt(99), ref(99), "kg/m^2", 0.001, 10, 2, 3
  gn_t(38).Init "YELD", "Yield of grains", lbl(100), txt(100), unt(100), ref(100), "kg/m^2", 0.001, 10, 0.8, 4
  gn_l(2).Add gn_t(35):    gn_l(2).Add gn_t(36):    gn_l(2).Add gn_t(37):    gn_l(2).Add gn_t(38)
  'Crop translocation factors
  gn_t(135).Init "DRYFAC", "Dry/wet weight ratio of leafy vegetables", lbl(97), txt(97), unt(97), ref(97), "fraction", 0.05, 0.95, 0.1, 1
  gn_t(136).Init "DRYFAC", "Dry/wet weight ratio of other vegetables", lbl(98), txt(98), unt(98), ref(98), "fraction", 0.05, 0.95, 0.2, 2
  gn_t(137).Init "DRYFAC", "Dry/wet weight ratio of fruit", lbl(99), txt(99), unt(99), ref(99), "fraction", 0.05, 0.95, 0.2, 3
  gn_t(138).Init "DRYFAC", "Dry/wet weight ratio of grain", lbl(100), txt(100), unt(100), ref(100), "fraction", 0.05, 0.95, 0.88, 4
  gn_l(2).Add gn_t(135):    gn_l(2).Add gn_t(136):    gn_l(2).Add gn_t(137):    gn_l(2).Add gn_t(138)
  'Crop translocation factors
  gn_t(125).Init "TRANS", "Translocation factor of leafy vegetables", lbl(97), txt(97), unt(97), ref(97), "fraction", 0, 1, 1, 1
  gn_t(126).Init "TRANS", "Translocation factor of other vegetables", lbl(98), txt(98), unt(98), ref(98), "fraction", 0, 1, 0.1, 2
  gn_t(127).Init "TRANS", "Translocation factor of fruit", lbl(99), txt(99), unt(99), ref(99), "fraction", 0, 1, 0.1, 3
  gn_t(128).Init "TRANS", "Translocation factor of grain", lbl(100), txt(100), unt(100), ref(100), "fraction", 0, 1, 0.1, 4
  gn_l(2).Add gn_t(125):    gn_l(2).Add gn_t(126):    gn_l(2).Add gn_t(127):    gn_l(2).Add gn_t(128)
  
  'Food crop intake delays  tab 0, tab 3
  gn_t(39).Init "HLDUP", "Time from harvest to ingestion of leafy vegetables", lbl(31), txt(31), unt(31), ref(31), "day", 0, 365, 14, 1
  gn_t(40).Init "HLDUP", "Time from harvest to ingestion of root vegetables", lbl(32), txt(32), unt(32), ref(32), "day", 0, 365, 14, 2
  gn_t(41).Init "HLDUP", "Time from harvest to ingestion of fruits", lbl(37), txt(37), unt(37), ref(37), "day", 0, 365, 14, 3
  gn_t(42).Init "HLDUP", "Time from harvest to ingestion of grains", lbl(38), txt(38), unt(38), ref(38), "day", 0, 365, 180, 4
  gn_l(2).Add gn_t(39):    gn_l(2).Add gn_t(40):    gn_l(2).Add gn_t(41):    gn_l(2).Add gn_t(42)
  'Food animal intake delays
  gn_t(13).Init "HLDUPA", "Time from slaughter to ingestion of meat", lbl(3), txt(3), unt(3), ref(3), "day", 0, 365, 20, 1
  gn_t(14).Init "HLDUPA", "Time from slaughter to ingestion of poultry", lbl(4), txt(4), unt(4), ref(4), "day", 0, 365, 18, 2
  gn_t(15).Init "HLDUPA", "Time from harvest to ingestion of milk", lbl(5), txt(5), unt(5), ref(5), "day", 0, 365, 2, 3
  gn_t(16).Init "HLDUPA", "Time from harvest to ingestion of eggs", lbl(6), txt(6), unt(6), ref(6), "day", 0, 365, 34, 4
  gn_l(1).Add gn_t(13):    gn_l(1).Add gn_t(14):    gn_l(1).Add gn_t(15):    gn_l(1).Add gn_t(16)
  ' Aquatic food animal intake delays
  gn_t(67).Init "HLDUP2", "Time from harvest to ingestion of fish", lbl(59), txt(59), unt(59), ref(59), "day", 0, 365, 1, 1
  gn_t(68).Init "HLDUP2", "Time from harvest to ingestion of mollusca", lbl(78), txt(78), unt(78), ref(78), "day", 0, 365, 0, 2
  gn_t(69).Init "HLDUP2", "Time from harvest to ingestion of crustacea", lbl(79), txt(79), unt(79), ref(79), "day", 0, 365, 0, 3
  gn_t(70).Init "HLDUP2", "Time from harvest to ingestion of aquatic plants", lbl(80), txt(80), unt(80), ref(80), "day", 0, 365, 0, 4
  gn_l(3).Add gn_t(67):    gn_l(3).Add gn_t(68):    gn_l(3).Add gn_t(69):    gn_l(3).Add gn_t(70)
End Sub

Sub watertab()
  'Water tab 1
  'Controls tab 1, tab 0
    gn_s(13).Init "DWSRC", "Source of domestic water", blbl(7), cb(7), bref(7)
    gn_l(36).Init "ISALT", "Aquatic foods from salt water (vesus fresh water)", SSCheck1(36), cref(36)
    gn_l(6).Init "DWTRET", "Treatment plant purification of domestic water", SSCheck1(28), cref(28)
    gn_t(78).Init "HOLDDW", "Delay time in water distribution system", lbl(67), txt(67), unt(67), ref(67), "day", 0, 365, 1
    gn_l(7).Init "RESIRR", "Residential irrigation", SSCheck1(29), cref(29)
    gn_s(11).Init "IRRSR", "Source of residential irrigation", blbl(6), cb(6), bref(6)
    
    
    gn_t(76).Init "RIRRR", "Irrigation rate for residential land", lbl(65), txt(65), unt(65), ref(65), "in/yr", 0, 200, 35
    gn_t(77).Init "IRTIMR", "Irrigation time for residential land", lbl(66), txt(66), unt(66), ref(66), "mon/yr", 0, 12, 6
    gn_l(7).Add gn_s(11):    gn_l(7).Add gn_t(75):       gn_l(7).Add gn_t(76):        gn_l(7).Add gn_t(77):
    gn_t(83).Init "ANDKRN", "Indoor volatilization factor for radon", lbl(83), txt(83), unt(83), ref(83), "l/m^3", 0, 10, 0.1
    gn_t(84).Init "ANDKR", "Indoor volatilization factor for radionuclides", lbl(84), txt(84), unt(84), ref(84), "l/m^3", 0, 10, 0
    gn_l(26).Add gn_t(83):    gn_l(26).Add gn_t(84):
  
  'Animal water fractions
    gn_t(71).Init "DWFACA", "Contaminated fraction of water for meat animal", lbl(74), txt(74), unt(74), ref(74), "fraction", 0, 1, 1, 1
    gn_t(72).Init "DWFACA", "Contaminated fraction of water for poultry animal", lbl(75), txt(75), unt(75), ref(75), "fraction", 0, 1, 1, 2
    gn_t(73).Init "DWFACA", "Contaminated fraction of water for milk animal", lbl(76), txt(76), unt(76), ref(76), "fraction", 0, 1, 1, 3
    gn_t(74).Init "DWFACA", "Contaminated fraction of water for egg animal", lbl(77), txt(77), unt(77), ref(77), "fraction", 0, 1, 1, 4
    gn_l(1).Add gn_t(71):    gn_l(1).Add gn_t(72):    gn_l(1).Add gn_t(73):    gn_l(1).Add gn_t(74)
    gn_t(99).Init "DWATER", "Intake rate of water for meat animal", lbl(107), txt(107), unt(107), ref(107), "L/d", 0#, 200, 50, 1
    gn_t(100).Init "DWATER", "Intake rate of water for poultry animal", lbl(108), txt(108), unt(108), ref(108), "L/d", 0#, 2, 0.3, 2
    gn_t(101).Init "DWATER", "Intake rate of water for milk animal", lbl(109), txt(109), unt(109), ref(109), "L/d", 0#, 200, 60, 3
    gn_t(102).Init "DWATER", "Intake rate of water for egg animal", lbl(110), txt(110), unt(110), ref(110), "L/d", 0#, 2, 0.3, 4
    gn_l(1).Add gn_t(99):    gn_l(1).Add gn_t(100):    gn_l(1).Add gn_t(101):    gn_l(1).Add gn_t(102)
    
  'Crop irrigation sources
    gn_s(7).Init "IRRST", "Source of  irrigation for leafy vegetables", blbl(11), cb(11), bref(11), 1
    gn_s(7).Add gn_t(59):     gn_s(7).Add gn_t(63)
    gn_s(8).Init "IRRST", "Source of  irrigation for root vegetables", blbl(10), cb(10), bref(10), 2
    gn_s(8).Add gn_t(60):    gn_s(8).Add gn_t(64)
    gn_s(9).Init "IRRST", "Source of  irrigation for fruits", blbl(9), cb(9), bref(9), 3
    gn_s(9).Add gn_t(61):    gn_s(9).Add gn_t(65)
    gn_s(10).Init "IRRST", "Source of  irrigation for grains", blbl(8), cb(8), bref(8), 4
    gn_s(10).Add gn_t(62):    gn_s(10).Add gn_t(66)
  'Feed irrigation sources
    gn_s(1).Init "IRRSA", "Source of  irrigation for meat animal feed", blbl(0), cb(0), bref(0), 1
    gn_s(1).Add gn_t(47):    gn_s(1).Add gn_t(53)
    gn_s(2).Init "IRRSA", "Source of  irrigation for poultry animal feed", blbl(1), cb(1), bref(1), 2
    gn_s(2).Add gn_t(48):    gn_s(2).Add gn_t(54)
    gn_s(3).Init "IRRSA", "Source of  irrigation for milk animal feed", blbl(2), cb(2), bref(2), 3
    gn_s(3).Add gn_t(49):    gn_s(3).Add gn_t(55)
    gn_s(4).Init "IRRSA", "Source of  irrigation for egg animal feed", blbl(3), cb(3), bref(3), 4
    gn_s(4).Add gn_t(50):    gn_s(4).Add gn_t(56)
    gn_s(5).Init "IRRSA", "Source of  irrigation for meat animal forage", blbl(4), cb(4), bref(4), 5
    gn_s(5).Add gn_t(51):    gn_s(5).Add gn_t(57)
    gn_s(6).Init "IRRSA", "Source of  irrigation for milk animal forage", blbl(5), cb(5), bref(5), 6
    gn_s(6).Add gn_t(52):    gn_s(6).Add gn_t(58)
  
  'Crop irrigation rates
    gn_t(59).Init "RIRR", "Irrigation rate for leafy vegetables", lbl(49), txt(49), unt(49), ref(49), "in/yr", 0, 200, 35, 1
    gn_t(60).Init "RIRR", "Irrigation rate for root vegetables", lbl(50), txt(50), unt(50), ref(50), "in/yr", 0, 200, 40, 2
    gn_t(61).Init "RIRR", "Irrigation rate for fruits", lbl(51), txt(51), unt(51), ref(51), "in/yr", 0, 200, 35, 3
    gn_t(62).Init "RIRR", "Irrigation rate for grains", lbl(52), txt(52), unt(52), ref(52), "in/yr", 0, 200, 0, 4
  'Feed irrigation rate
    gn_t(47).Init "RIRRA", "Irrigation rate for meat animal feed", lbl(43), txt(43), unt(43), ref(43), "in/yr", 0, 200, 35, 1
    gn_t(48).Init "RIRRA", "Irrigation rate for poultry animal feed", lbl(44), txt(44), unt(44), ref(44), "in/yr", 0, 200, 0, 2
    gn_t(49).Init "RIRRA", "Irrigation rate for milk animal feed", lbl(45), txt(45), unt(45), ref(45), "in/yr", 0, 200, 47, 3
    gn_t(50).Init "RIRRA", "Irrigation rate for egg animal feed", lbl(46), txt(46), unt(46), ref(46), "in/yr", 0, 200, 0, 4
    gn_t(51).Init "RIRRA", "Irrigation rate for meat animal forage", lbl(47), txt(47), unt(47), ref(47), "in/yr", 0, 200, 47, 5
    gn_t(52).Init "RIRRA", "Irrigation rate for milk animal forage", lbl(48), txt(48), unt(48), ref(48), "in/yr", 0, 200, 47, 6
  
  'Crop irrigation times
    gn_t(63).Init "IRTIMT", "Irrigation time for leafy vegetables", lbl(61), txt(61), unt(61), ref(61), "mon/yr", 0, 12, 6, 1
    gn_t(64).Init "IRTIMT", "Irrigation time for root vegetables", lbl(62), txt(62), unt(62), ref(62), "mon/yr", 0, 12, 6, 2
    gn_t(65).Init "IRTIMT", "Irrigation time for fruits", lbl(63), txt(63), unt(63), ref(63), "mon/yr", 0, 12, 6, 3
    gn_t(66).Init "IRTIMT", "Irrigation time for grains", lbl(64), txt(64), unt(64), ref(64), "mon/yr", 0, 12, 6, 4
  'Feed irrigation times
    gn_t(53).Init "IRTIMA", "Irrigation time for meat animal feed", lbl(53), txt(53), unt(53), ref(53), "mon/yr", 0, 12, 6, 1
    gn_t(54).Init "IRTIMA", "Irrigation time for poultry animal feed", lbl(54), txt(54), unt(54), ref(54), "mon/yr", 0, 12, 6, 2
    gn_t(55).Init "IRTIMA", "Irrigation time for milk animal feed", lbl(55), txt(55), unt(55), ref(55), "mon/yr", 0, 12, 6, 3
    gn_t(56).Init "IRTIMA", "Irrigation time for eggs animal feed", lbl(56), txt(56), unt(56), ref(56), "mon/yr", 0, 12, 6, 4
    gn_t(57).Init "IRTIMA", "Irrigation time for meat animal forage", lbl(57), txt(57), unt(57), ref(57), "mon/yr", 0, 12, 6, 5
    gn_t(58).Init "IRTIMA", "Irrigation time for milk animal forage", lbl(58), txt(58), unt(58), ref(58), "mon/yr", 0, 12, 6, 6
End Sub

Private Sub Form_load()
Dim i As Long
Dim j As Long
  
  GetArguments
  If argc >= 5 Then
    Loading.Show
    loadng = True
    acute = False
    If CInt(argv(0)) = 1 Then acute = True
    FUIName = argv(1) & ".GID"
    RunName = argv(2)
    siteIdx = CInt(argv(3))
    modIdx = CInt(argv(4))
    modName = argv(5)
    SetForm Me
    SetRefFile ReplaceExt(FUIName, "ref")
    If open_csv(errfile, RunName & ".ERR", 1) Then
      put_val errfile, "Error report for Exposure Model"
      put_line errfile
    Else
      MsgBox "Unable to create file " & RunName & ".ERR" & Chr(10) & "Check directory permessions"
      End
    End If
    loadng = True
    load_convert
    If acute Then
      SetHelpFile App.Path + "\genexahp.htm"
      contxt.Enabled = HelpAvailable
      rng.Enabled = HelpAvailable
      lsize = 42
      tsize = 147
      gen_exp.Caption = "GENII Acute Exposure Module - " + modName
    Else
      SetHelpFile App.Path + "\genexchp.htm"
      contxt.Enabled = HelpAvailable
      rng.Enabled = HelpAvailable
      lsize = 39
      tsize = 142
      gen_exp.Caption = "GENII Chronic Exposure Module - " + modName
    End If
    SSTab1(0).TabVisible(3) = False
'    SSTab1(4).TabVisible(8) = False
    ReDim gn_t(tsize) As New Float
    ReDim gn_s(13) As New Selects
    ReDim gn_l(lsize) As New Logical
  
   generaltab
   watertab
 
  'Air tab 2
    gn_l(8).Init "FINITE", "Finite plume model", SSCheck1(26), cref(26)
    
  'Soil tab
    cb(15).AddItem ("GENII default leach rates"):                 cb(15).ItemData(cb(15).NewIndex) = 1
    cb(15).AddItem ("Leach rates calculated from user input"):    cb(15).ItemData(cb(15).NewIndex) = 2
    cb(15).AddItem ("User provided leach rate constants"):        cb(15).ItemData(cb(15).NewIndex) = 3
    gn_s(12).Init "LEACHOPTION", "Type of leach rate constant", blbl(15), cb(15), bref(15)
    gn_t(85).Init "SEDDN", "Shoreline sediment density", lbl(90), txt(90), unt(90), ref(90), "kg/m^2", 0, 5000, 15
    gn_t(86).Init "THICK", "Surface soil thickness", lbl(85), txt(85), unt(85), ref(85), "cm", 0, 10000, 200
    gn_t(87).Init "MOISTC", "Surface soil moisture content", lbl(86), txt(86), unt(86), ref(86), "ml/cm^3", 0, 1, 0.08
    gn_t(88).Init "BULKD", "Surface soil bulk density", lbl(87), txt(87), unt(87), ref(87), "g/cm^3", 0.5, 3, 1.6
    gn_t(89).Init "VLEACH", "Total infiltration rate", lbl(88), txt(88), unt(88), ref(88), "cm/yr", 0, 500, 20
    
    gn_t(90).Init "SOILKD", "Parent soil absorption coefficient (Kd)", lbl(91), txt(91), unt(91), bref(16), "ml/g", 0, 1000000, 0
    gn_t(91).Init "SOILKDD", "Progeny soil absorbtion coefficient (Kd)", lbl(92), txt(92), unt(92), bref(17), "ml/g", 0, 1000000, 0
    gn_t(92).Init "LEACHR", "Parent soil leach rate constant", lbl(91), txt(91), unt(91), bref(16), "1/yr", 0.00001, 365#, 0.001
    gn_t(93).Init "LEACHRD", "Progeny soil leach rate constant", lbl(92), txt(92), unt(92), bref(17), "1/yr", 0.00001, 365#, 0.001
    
    gn_t(81).Init "DEPFR1", "Dry deposition interception fraction to plants", lbl(93), txt(93), unt(93), ref(93), "fraction", 0, 1, 0.2
    gn_t(94).Init "DEPFR2", "Wet deposition interception fraction to plants", lbl(94), txt(94), unt(94), ref(94), "fraction", 0, 1, 0.25
    gn_t(95).Init "LEAFRS", "Resuspension factor from soil to plant surfaces", lbl(95), txt(95), unt(95), ref(95), "1/m", 0, 0.001, 0.000000001
    gn_t(96).Init "DPVRES", "Deposition velocity from soil to plant surfaces", lbl(96), txt(96), unt(96), ref(96), "m/s", 0, 0.1, 0.001
    
    gn_t(97).Init "SLDN", "Surface soil areal density", lbl(81), txt(81), unt(81), ref(81), "kg/m^2", 0.00001, 3000, 215
    gn_t(75).Init "SSLDN", "Surface soil density", lbl(139), txt(139), unt(139), ref(139), "kg/m^3", 0.00001, 3000, 1500
    gn_t(98).Init "SURCM", "Surface soil layer thickness used for density", lbl(68), txt(68), unt(68), ref(68), "cm", 0.00001, 500, 15
    gn_t(80).Init "WTIM", "Weathering rate constant from plants", lbl(138), txt(138), unt(138), ref(138), "d", 0.1, 100, 10
  
  
  'Pathways tab 3
  'Ingestion
    gn_l(10).Init "ANF", "Meat", SSCheck1(0), cref(0), 1
    gn_l(11).Init "ANF", "Poultry", SSCheck1(1), cref(1), 2
    gn_l(12).Init "ANF", "Milk", SSCheck1(2), cref(2), 3
    gn_l(13).Init "ANF", "Eggs", SSCheck1(3), cref(3), 4
    gn_l(1).Add gn_l(10):    gn_l(1).Add gn_l(11):    gn_l(1).Add gn_l(12):    gn_l(1).Add gn_l(13)
    gn_l(14).Init "TFD", "Leafy vegetables", SSCheck1(4), cref(4), 1
    gn_l(15).Init "TFD", "Root vegetables", SSCheck1(5), cref(5), 2
    gn_l(16).Init "TFD", "Fruits", SSCheck1(6), cref(6), 3
    gn_l(17).Init "TFD", "Grains", SSCheck1(7), cref(7), 4
    gn_l(2).Add gn_l(14):    gn_l(2).Add gn_l(15):    gn_l(2).Add gn_l(16):    gn_l(2).Add gn_l(17)
    gn_l(18).Init "AQF", "Fish", SSCheck1(8), cref(8), 1
    gn_l(19).Init "AQF", "Mollusca", SSCheck1(9), cref(9), 2
    gn_l(20).Init "AQF", "Crustacea", SSCheck1(10), cref(10), 3
    gn_l(21).Init "AQF", "Aquatic plants", SSCheck1(11), cref(11), 4
    gn_l(3).Add gn_l(18):    gn_l(3).Add gn_l(19):    gn_l(3).Add gn_l(20):    gn_l(3).Add gn_l(21)
    gn_l(22).Init "DRINK", "Drinking water", SSCheck1(12), cref(12)
    gn_l(23).Init "SHING", "Inadvertent shower water", SSCheck1(13), cref(13)
    gn_l(24).Init "SWING", "Inadvertent swimming water", SSCheck1(14), cref(14)
    gn_l(25).Init "SLING", "Inadvertent soil", SSCheck1(15), cref(15)
  'Inhalation
    gn_l(0).Init "INHAL", "Inhalation of Outdoor Air", SSCheck1(30), cref(30)
    gn_l(26).Init "SHINDR", "Inhalation of Indoor Air", SSCheck1(16), cref(16)
    gn_l(27).Init "SLINH", "Suspended or resuspended soil", SSCheck1(17), cref(17)
  'External
    gn_l(28).Init "REC", "Swimming external", SSCheck1(18), cref(18), 1
    gn_l(29).Init "REC", "Boating external", SSCheck1(19), cref(19), 2
    gn_l(30).Init "REC", "Shoreline external", SSCheck1(20), cref(20), 3
    gn_l(31).Init "GROUND", "Soil external", SSCheck1(21), cref(21)
    gn_l(9).Init "AIREXT", "External air", SSCheck1(27), cref(27)
 '   gn_l(9).Add gn_l(8)
  
  'Dermal contact has been removed
    gn_l(32).Init "SWDRML", "Swimming dermal contact", SSCheck1(22), cref(22)
    gn_l(33).Init "SRDRML", "Shoreline dermal contact", SSCheck1(23), cref(23)
    gn_l(34).Init "SHDRML", "Showering dermal contact", SSCheck1(24), cref(24)
    gn_l(35).Init "SLDRML", "Soil dermal contact", SSCheck1(25), cref(25)
    gn_l(4).Add gn_l(24):    gn_l(4).Add gn_l(28):    gn_l(4).Add gn_l(29)
    gn_l(4).Add gn_l(30):
    
    'gn_l(4).Add gn_l(32):    gn_l(4).Add gn_l(33)
   
    gn_l(37).Init "DRYSET", "User defined dry deposition interception fraction to plants", SSCheck1(37), cref(37)
    gn_l(38).Init "WETSET", "User defined wet deposition interception fraction to plants", SSCheck1(38), cref(38)
    gn_l(39).Init "HARVST", "Radionuclide removal due to harvesting", SSCheck1(39), cref(39)
  
    gn_l(37).Add gn_t(81)
    gn_l(38).Add gn_t(94)
    gn_s(0).Add gn_t(5):    gn_s(0).Add gn_t(79):    gn_s(0).Add gn_t(6)
    
    For i = 0 To 13
      ui.Add gn_s(i), gn_s(i).Name + Format(gn_s(i).idx(0))
    Next
    For i = 0 To lsize
      If i = 32 Then i = 36
      ui.Add gn_l(i), gn_l(i).Name + Format(gn_l(i).idx(0))
    Next
    If Not acute Then
      For i = 0 To 2
        ui.Add gn_i(i), gn_i(i).Name + Format(gn_i(i).idx(0))
      Next
    End If
    For i = 0 To tsize
      If gn_t(i).Name <> "" Then ui.Add gn_t(i), gn_t(i).Name + Format(gn_t(i).idx(0))
    Next
    For i = 14 To 15
      cb(i).ListIndex = 0
    Next
    
    loadprm
    Unload Loading
    kd1.ListIndex = 0
    loadng = False
  Else
    End
  End If
' experiment by BAN for NESHAPS
'If SSCheck1(35) = False And SSCheck1(34) = False And SSCheck1(30) = False And SSCheck1(17) = False And SSCheck1(21) = False Then
'  SSCheck1(35).Value = True
'  SSCheck1(34).Value = True
'  SSCheck1(0).Value = True
'  SSCheck1(2).Value = True
'  SSCheck1(4).Value = True
'  SSCheck1(5).Value = True
'  SSCheck1(30).Value = True
'  SSCheck1(16).Value = True
'  SSCheck1(17).Value = True
'  SSCheck1(21).Value = True
'  SSCheck1(27).Value = True
'  SSCheck1(26).Value = True
'  SSCheck1(1).Value = False
'  SSCheck1(3).Value = False
'  SSCheck1(6).Value = False
'  SSCheck1(7).Value = False
'End If
End Sub

Private Sub leave_Click()
  close_csv errfile
  Kill RunName & ".ERR"
  End
End Sub

Private Sub save_Click()
  Dim i As Long
  Dim j As Long
  Dim chk As Double
  Dim fName As String
  Dim parm As parmrec
  Dim tmp As parmfile
  Dim flag As Boolean
  
  kd1_Click
  flag = False
  fName = RunName & ".GID"
  fd.Enabled = True
  cp.Enabled = True
  For i = 0 To 8
    If i = 0 Or i = 3 Or i = 5 Then frm(i).Enabled = True
    If i = 0 Or i = 3 Then ag(i).Enabled = True
    If i < 5 And (haveswater Or havegwater) Then wt(i).Enabled = True
    If i < 3 Then sl(i).Enabled = True
  Next
  If open_parm(fle, fName, 1) Then
    For i = 0 To 13
      If Not gn_s(i).Store And gn_s(i).Enabled Then
        put_val errfile, "Bad data entry for selection flag " + gn_s(i).Name
        put_line errfile
        flag = True
      End If
    Next
    For i = 0 To lsize
      If i = 32 Then i = 36
      If Not gn_l(i).Store And gn_l(i).Enabled Then
        put_val errfile, "Bad data entry for logical flag " + gn_l(i).Name
        put_line errfile
        flag = True
      End If
    Next
    If Not acute Then
      For i = 0 To 2
        If Not gn_i(i).Store And gn_i(i).Enabled Then
          put_val errfile, "Bad data entry for integer " + gn_i(i).Name
          put_line errfile
          flag = True
        End If
      Next
    End If
    For i = 0 To tsize
      If i = 90 Then i = 94
      If Not gn_t(i).Store And gn_t(i).Enabled Then
        put_val errfile, "Bad data entry for float " + gn_t(i).Name
        put_line errfile
        flag = True
      End If
    Next
    
    set_parm parm, "NUMCON", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", Format(m_numcon)
    write_parmrec fle, parm
    For i = 1 To m_numcon
      If kd(i).idx <> 0 Then
        set_parm parm, "CASID", kd(i).idx, 0, 0, 0, 0, 0, kd(i).ref, "N/A", "N/A", kd(i).cas
        write_parmrec fle, parm
        set_parm parm, "SOILKD", kd(i).idx, 0, 0, 0, 0, 0, kd(i).ref, kd(i).uunt, "ml/g", convert(kd(i).uunt, "ml/g", kd(i).kd)
        write_parmrec fle, parm
        set_parm parm, "LEACHR", kd(i).idx, 0, 0, 0, 0, 0, kd(i).ref, kd(i).luunt, "1/yr", convert(kd(i).luunt, "1/yr", kd(i).lr)
        write_parmrec fle, parm
        set_parm parm, "NDS", kd(i).idx, 0, 0, 0, 0, 0, 0, "N/A", "N/A", Format(kd(i).numprog)
        write_parmrec fle, parm
        Select Case cb(15).ListIndex
        Case 1:
          chk = CDbl(convert(kd(i).uunt, "ml/g", kd(i).kd))
          If chk < 0 And chk > 1000000 Then flag = True
        Case 2:
          chk = CDbl(convert(kd(i).luunt, "1/yr", kd(i).lr))
          If chk < 0.00001 And chk > 1 Then flag = True
        End Select
        For j = 1 To kd(i).numprog
          If kd(i).progeny(j).idx <> 0 Then
            set_parm parm, "CASID", kd(i).idx, kd(i).progeny(j).idx, 0, 0, 0, 0, kd(i).progeny(j).ref, "N/A", "N/A", kd(i).progeny(j).cas
            write_parmrec fle, parm
            set_parm parm, "SOILKD", kd(i).idx, kd(i).progeny(j).idx, 0, 0, 0, 0, kd(i).progeny(j).ref, kd(i).progeny(j).uunt, "ml/g", convert(kd(i).progeny(j).uunt, "ml/g", kd(i).progeny(j).kd)
            write_parmrec fle, parm
            set_parm parm, "LEACHR", kd(i).idx, kd(i).progeny(j).idx, 0, 0, 0, 0, kd(i).progeny(j).ref, kd(i).progeny(j).luunt, "1/yr", convert(kd(i).progeny(j).luunt, "1/yr", kd(i).progeny(j).lr)
            write_parmrec fle, parm
            Select Case cb(15).ListIndex
            Case 1:
              chk = CDbl(convert(kd(i).progeny(j).uunt, "ml/g", kd(i).progeny(j).kd))
              If chk < 0 And chk > 1000000 Then flag = True
            Case 2:
              chk = CDbl(convert(kd(i).progeny(j).luunt, "1/yr", kd(i).progeny(j).lr))
              If chk < 0.00001 And chk > 1 Then flag = True
            End Select
          End If
        Next
      End If
    Next
    
    WriteDesLabels fle    'provided for des compatibility
    
    
    close_parm fle
    If flag Then
      MsgBox "Saving incomplete data", 64, "Important Notice!"
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

Sub SSCheck1_Click(index As Integer, Value As Integer)
  If loadng Then Exit Sub
  ui(SSCheck1(index).Tag).Value = Value
End Sub

Private Sub cb_Click(index As Integer)
Dim i As Long
Dim enable As Boolean
Dim degree As Double
  
  ui(blbl(index).Tag).Click
  Select Case index
    Case 0 To 5, 7 To 11
      enable = True
      If cb(index).ListIndex = 0 Then enable = False
        ui(blbl(index).Tag).StatusListeners enable
    Case 14
      ui(blbl(index).Tag).StatusListeners False
      Select Case cb(index).ListIndex
        Case 1:  gn_t(5).Enabled = True
        Case 2:  gn_t(6).Enabled = True
        Case 3:  gn_t(79).Enabled = True
      End Select
    Case 15
      If cb(index).ListIndex = 0 Then
        gn_t(86).Visible = False
        gn_t(87).Visible = False
        gn_t(88).Visible = False
        gn_t(89).Visible = False
        gn_t(86).Enabled = False
        gn_t(87).Enabled = False
        gn_t(88).Enabled = False
        gn_t(89).Enabled = False
        gn_t(90).Visible = False
        gn_t(91).Visible = False
        gn_t(92).Visible = False
        gn_t(93).Visible = False
        kd1.Visible = False
        kd2.Visible = False
      ElseIf cb(index).ListIndex = 1 Then
        gn_t(86).Visible = True
        gn_t(87).Visible = True
        gn_t(88).Visible = True
        gn_t(89).Visible = True
        gn_t(86).Enabled = True
        gn_t(87).Enabled = True
        gn_t(88).Enabled = True
        gn_t(89).Enabled = True
        gn_t(91).Visible = False
        gn_t(90).Refresh
        gn_t(91).Refresh
        gn_t(90).Visible = True
        kd1.Visible = True
        kd2.Visible = False
        ctext3 = ""
        ctext4 = ""
        If Not loadng Then
          kd1.ListIndex = -1
          kd1.ListIndex = 0
        End If
      Else
        gn_t(86).Visible = False
        gn_t(87).Visible = False
        gn_t(88).Visible = False
        gn_t(89).Visible = False
        gn_t(86).Enabled = False
        gn_t(87).Enabled = False
        gn_t(88).Enabled = False
        gn_t(89).Enabled = False
        gn_t(93).Visible = False
        gn_t(92).Refresh
        gn_t(93).Refresh
        gn_t(92).Visible = True
        kd1.Visible = True
        kd2.Visible = False
        ctext3 = ""
        ctext4 = ""
        If Not loadng Then
          kd1.ListIndex = -1
          kd1.ListIndex = 0
        End If
      End If
  End Select
End Sub

Private Sub refTwo(i As Long)
gn_t(i).Refresh
gn_t(i + 1).Refresh
gn_t(i).Visible = True
gn_t(i + 1).Visible = True
For i = 103 To 106
  txt(i).Visible = False
  unt(i).Visible = False
  lbl(i).Visible = False
  ref(i).Visible = False
Next
End Sub

Private Sub refFour(i As Long)
gn_t(i).Refresh
gn_t(i + 1).Refresh
gn_t(i + 2).Refresh
gn_t(i + 3).Refresh
gn_t(i).Visible = True
gn_t(i + 1).Visible = True
gn_t(i + 2).Visible = True
gn_t(i + 3).Visible = True
For i = 105 To 106
  txt(i).Visible = False
  unt(i).Visible = False
  lbl(i).Visible = False
  ref(i).Visible = False
Next
End Sub

Private Sub refSix(i As Long)
gn_t(i).Refresh
gn_t(i + 1).Refresh
gn_t(i + 2).Refresh
gn_t(i + 3).Refresh
gn_t(i + 4).Refresh
gn_t(i + 5).Refresh
gn_t(i).Visible = True
gn_t(i + 1).Visible = True
gn_t(i + 2).Visible = True
gn_t(i + 3).Visible = True
gn_t(i + 4).Visible = True
gn_t(i + 5).Visible = True
End Sub

Private Sub SSTab1_Click(index As Integer, PreviousTab As Integer)
  Select Case t1
      Case 0, 3, 5: frm(t1).Enabled = False
      Case 1: SSTab1(1).Enabled = False
                  wt(t2).Enabled = False
      Case 2: SSTab1(2).Enabled = False
                  sl(t2).Enabled = False
      Case 4: SSTab1(3).Enabled = False
                  Select Case t2
                    Case 0, 3: ag(t2).Enabled = False
                    Case 1: SSTab1(4).Enabled = False
                            fd.Enabled = False
                    Case 2: SSTab1(5).Enabled = False
                            cp.Enabled = False
                  End Select
  End Select
  t3 = 0
  t2 = 0
  t1 = SSTab1(0).Tab
  SSTab1(0).Enabled = True
  Select Case SSTab1(0).Tab
      Case 0, 3, 5: frm(SSTab1(0).Tab).Enabled = True
      Case 1: SSTab1(1).Enabled = True
                  t2 = SSTab1(1).Tab
                  wt(SSTab1(1).Tab).Enabled = True
      Case 2: SSTab1(2).Enabled = True
                  t2 = SSTab1(2).Tab
                  sl(SSTab1(2).Tab).Enabled = True
      Case 4: SSTab1(3).Enabled = True
                  t2 = SSTab1(3).Tab
                  fd.Visible = False
                  cp.Visible = False
                  Select Case SSTab1(3).Tab
                    Case 0, 3: ag(SSTab1(3).Tab).Enabled = True
                    Case 1: SSTab1(4).Enabled = True
                            t3 = SSTab1(4).Tab
                            fd.Enabled = True
                            Select Case t3
                            Case 0: refSix 103
                            Case 1: refSix 109
                            Case 2: refSix 23
                            Case 3: refSix 29
                            Case 4: refSix 17
                            Case 5: refSix 7
                            Case 6: refSix 129
                            Case 7: refSix 119
                            Case 8: refTwo 143
                            Case 9: refFour 139
                            End Select
                            fd.Visible = True
                    Case 2: SSTab1(5).Enabled = True
                            t3 = SSTab1(5).Tab
                            cp.Enabled = True
                            Select Case t3
                            Case 0: refFour 115
                            Case 1: refFour 43
                            Case 2: refFour 35
                            Case 3: refFour 135
                            Case 4: refFour 125
                            End Select
                            cp.Visible = True
                  End Select
  End Select
End Sub

Private Sub ud_Click()
  Loading.Show
  loaddef 3
  mi.Checked = False
  ai.Checked = False
  ud.Checked = True
  SSTab1_Click t3, t3
  Unload Loading
End Sub

Private Sub unt_Click(index As Integer)
  If Not loadng Then ui(lbl(index).Tag).Click
End Sub

Private Sub unt_KeyUp(index As Integer, KeyCode As Integer, Shift As Integer)
  If vbKeyF2 = KeyCode Then ui(lbl(index).Tag).Help
End Sub

Private Sub txt_KeyUp(index As Integer, KeyCode As Integer, Shift As Integer)
  If vbKeyF2 = KeyCode Then
    ui(lbl(index).Tag).Help
  Else
    ui(lbl(index).Tag).Change
  End If
End Sub

Private Sub unt_GotFocus(index As Integer)
  Set rfs = ref(index)
  HelpAnchor = ui(lbl(index).Tag).Name
  uiname = lbl(index).Tag
End Sub

Private Sub txt_GotFocus(index As Integer)
  If index = 91 Then
    Set rfs = bref(16)
  ElseIf index = 92 Then
    Set rfs = bref(17)
  Else
    Set rfs = ref(index)
  End If
  HelpAnchor = ui(lbl(index).Tag).Name
  uiname = lbl(index).Tag
End Sub

Private Sub cb_GotFocus(index As Integer)
  If index = 91 Then
    Set rfs = bref(16)
  ElseIf index = 92 Then
    Set rfs = bref(17)
  Else
    Set rfs = ref(index)
  End If
  HelpAnchor = ui(blbl(index).Tag).Name
  uiname = blbl(index).Tag
End Sub

Private Sub cb_KeyDown(index As Integer, KeyCode As Integer, Shift As Integer)
  If vbKeyF2 = KeyCode Then ui(blbl(index).Tag).Help
End Sub

Private Sub kd1_GotFocus()
  txt_GotFocus 91
End Sub

Private Sub kd2_GotFocus()
  txt_GotFocus 92
End Sub
Private Sub SSCheck1_GotFocus(index As Integer)
  Set rfs = cref(index)
  HelpAnchor = ui(SSCheck1(index).Tag).Name
  uiname = SSCheck1(index).Tag
End Sub

Private Sub SSCheck1_KeyDown(index As Integer, KeyCode As Integer, Shift As Integer)
  If vbKeyF2 = KeyCode Then ui(SSCheck1(index).Tag).Help
End Sub

Private Sub SSTab1_GotFocus(index As Integer)
  mnuRef.Enabled = False
  HelpAnchor = ""
  uiname = ""
End Sub

Private Sub SSTab1_LostFocus(index As Integer)
  mnuRef.Enabled = True
  HelpAnchor = ""
  uiname = ""
End Sub

Private Sub kd1_Click()
  Dim i As Long
  Dim j As Long
  
  Select Case cb(15).ListIndex
    Case 0
      Exit Sub
    Case 1
      If kd2.Visible Then kd2_Click
      kdchange kd1.Text, ctext3
    Case 2
      If kd2.Visible Then kd2_Click
      lrchange kd1.Text, ctext3
  End Select
  ctext3 = kd1.Text
  For i = 1 To m_numcon
    If con(kd(i).idx).Name = ctext3 Then
      If kd(i).numprog = 0 Then
        txt(92).Visible = False
        unt(92).Visible = False
        bref(17).Visible = False
        lbl(92).Visible = False
        kd2.Visible = False
      Else
        kd2.Clear
        For j = 1 To kd(i).numprog
          kd2.AddItem con(kd(i).idx).progeny(kd(i).progeny(j).idx).Name
        Next
        txt(92).Visible = True
        unt(92).Visible = True
        bref(17).Visible = True
        lbl(92).Visible = True
        kd2.Visible = True
        kd2.ListIndex = 0
      End If
      Exit For
    End If
  Next
End Sub

Private Sub kd2_Click()
  If cb(15).ListIndex = 1 Then
    d_kdchange kd2.Text, ctext4
  Else
    d_lrchange kd2.Text, ctext4
  End If
  ctext4 = kd2.Text
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
