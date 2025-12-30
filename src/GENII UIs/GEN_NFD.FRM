VERSION 5.00
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "TABCTL32.OCX"
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "THREED32.OCX"
Begin VB.Form gen_nfd 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Form1"
   ClientHeight    =   5400
   ClientLeft      =   4410
   ClientTop       =   3075
   ClientWidth     =   7680
   Icon            =   "GEN_NFD.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   360
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   512
   StartUpPosition =   2  'CenterScreen
   Begin TabDlg.SSTab SSTab1 
      Height          =   5412
      Index           =   0
      Left            =   0
      TabIndex        =   0
      TabStop         =   0   'False
      Top             =   0
      Width           =   7692
      _ExtentX        =   13573
      _ExtentY        =   9551
      _Version        =   393216
      Style           =   1
      Tabs            =   4
      TabsPerRow      =   4
      TabHeight       =   423
      TabCaption(0)   =   "Controls"
      TabPicture(0)   =   "GEN_NFD.frx":08CA
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "frm(0)"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).ControlCount=   1
      TabCaption(1)   =   "Soil"
      TabPicture(1)   =   "GEN_NFD.frx":08E6
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "SSTab1(1)"
      Tab(1).Control(0).Enabled=   0   'False
      Tab(1).ControlCount=   1
      TabCaption(2)   =   "Agriculture"
      TabPicture(2)   =   "GEN_NFD.frx":0902
      Tab(2).ControlEnabled=   0   'False
      Tab(2).Control(0)=   "SSTab1(2)"
      Tab(2).ControlCount=   1
      TabCaption(3)   =   "Pathways"
      TabPicture(3)   =   "GEN_NFD.frx":091E
      Tab(3).ControlEnabled=   0   'False
      Tab(3).Control(0)=   "frm(3)"
      Tab(3).ControlCount=   1
      Begin Threed.SSFrame frm 
         Height          =   4812
         Index           =   0
         Left            =   240
         TabIndex        =   115
         Top             =   360
         Width           =   7212
         _Version        =   65536
         _ExtentX        =   12721
         _ExtentY        =   8488
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
         Begin VB.ComboBox cb 
            Height          =   288
            Index           =   2
            ItemData        =   "GEN_NFD.frx":093A
            Left            =   3120
            List            =   "GEN_NFD.frx":0941
            Style           =   2  'Dropdown List
            TabIndex        =   251
            Top             =   3960
            Width           =   2772
         End
         Begin VB.ComboBox cb 
            Height          =   288
            Index           =   1
            ItemData        =   "GEN_NFD.frx":094B
            Left            =   3120
            List            =   "GEN_NFD.frx":0952
            Style           =   2  'Dropdown List
            TabIndex        =   248
            Top             =   3600
            Width           =   2772
         End
         Begin VB.ComboBox cb 
            Height          =   288
            Index           =   0
            ItemData        =   "GEN_NFD.frx":095C
            Left            =   3120
            List            =   "GEN_NFD.frx":0963
            Style           =   2  'Dropdown List
            TabIndex        =   245
            Top             =   3240
            Width           =   2772
         End
         Begin VB.ComboBox unt 
            Height          =   288
            Index           =   5
            Left            =   4920
            Style           =   2  'Dropdown List
            TabIndex        =   64
            Top             =   2760
            Width           =   1000
         End
         Begin VB.ComboBox unt 
            Height          =   288
            Index           =   4
            Left            =   4920
            Style           =   2  'Dropdown List
            TabIndex        =   62
            Top             =   2400
            Width           =   1000
         End
         Begin VB.ComboBox unt 
            Height          =   288
            Index           =   3
            Left            =   4920
            Style           =   2  'Dropdown List
            TabIndex        =   60
            Top             =   4320
            Width           =   1000
         End
         Begin VB.ComboBox unt 
            Height          =   288
            Index           =   2
            Left            =   4920
            Style           =   2  'Dropdown List
            TabIndex        =   58
            Top             =   2040
            Visible         =   0   'False
            Width           =   1000
         End
         Begin VB.ComboBox unt 
            Height          =   288
            Index           =   1
            Left            =   4920
            Style           =   2  'Dropdown List
            TabIndex        =   56
            Top             =   1680
            Width           =   1000
         End
         Begin VB.ComboBox unt 
            Height          =   288
            Index           =   0
            Left            =   4920
            Style           =   2  'Dropdown List
            TabIndex        =   54
            Top             =   1320
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Height          =   288
            Index           =   5
            Left            =   3960
            TabIndex        =   63
            Top             =   2760
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Height          =   288
            Index           =   4
            Left            =   3960
            TabIndex        =   61
            Top             =   2400
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Height          =   288
            Index           =   3
            Left            =   3960
            TabIndex        =   59
            Top             =   4320
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Height          =   288
            Index           =   2
            Left            =   3960
            TabIndex        =   57
            Top             =   2040
            Visible         =   0   'False
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Height          =   288
            Index           =   1
            Left            =   3960
            TabIndex        =   55
            Top             =   1680
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Height          =   288
            Index           =   0
            Left            =   3960
            TabIndex        =   53
            Top             =   1320
            Width           =   1000
         End
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   2
            Left            =   1080
            TabIndex        =   52
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
            Index           =   1
            Left            =   1080
            TabIndex        =   51
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
            Index           =   0
            Left            =   1080
            TabIndex        =   50
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
         Begin VB.Label bref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   2
            Left            =   6000
            TabIndex        =   253
            Top             =   3960
            Width           =   972
         End
         Begin VB.Label blbl 
            Height          =   252
            Index           =   2
            Left            =   240
            TabIndex        =   252
            Top             =   3960
            Width           =   2892
         End
         Begin VB.Label bref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   1
            Left            =   6000
            TabIndex        =   250
            Top             =   3600
            Width           =   972
         End
         Begin VB.Label blbl 
            Height          =   252
            Index           =   1
            Left            =   240
            TabIndex        =   249
            Top             =   3600
            Width           =   2892
         End
         Begin VB.Label bref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   0
            Left            =   6000
            TabIndex        =   247
            Top             =   3240
            Width           =   972
         End
         Begin VB.Label blbl 
            Height          =   252
            Index           =   0
            Left            =   240
            TabIndex        =   246
            Top             =   3240
            Width           =   2892
         End
         Begin VB.Label lbl 
            Height          =   252
            Index           =   5
            Left            =   240
            TabIndex        =   170
            Top             =   2760
            Width           =   3696
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   5
            Left            =   6000
            TabIndex        =   169
            Top             =   2760
            Width           =   996
         End
         Begin VB.Label lbl 
            Height          =   252
            Index           =   4
            Left            =   240
            TabIndex        =   168
            Top             =   2400
            Width           =   3696
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   4
            Left            =   6000
            TabIndex        =   167
            Top             =   2400
            Width           =   996
         End
         Begin VB.Label lbl 
            Height          =   252
            Index           =   3
            Left            =   240
            TabIndex        =   166
            Top             =   4320
            Width           =   3696
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   3
            Left            =   6000
            TabIndex        =   165
            Top             =   4320
            Width           =   996
         End
         Begin VB.Label lbl 
            Height          =   252
            Index           =   2
            Left            =   240
            TabIndex        =   164
            Top             =   2040
            Visible         =   0   'False
            Width           =   3696
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   2
            Left            =   6000
            TabIndex        =   163
            Top             =   2040
            Visible         =   0   'False
            Width           =   996
         End
         Begin VB.Label lbl 
            Height          =   252
            Index           =   1
            Left            =   240
            TabIndex        =   162
            Top             =   1680
            Width           =   3696
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   1
            Left            =   6000
            TabIndex        =   161
            Top             =   1680
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   0
            Left            =   6000
            TabIndex        =   120
            Top             =   1320
            Width           =   996
         End
         Begin VB.Label lbl 
            Height          =   252
            Index           =   0
            Left            =   240
            TabIndex        =   119
            Top             =   1320
            Width           =   3696
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   0
            Left            =   240
            TabIndex        =   118
            Top             =   360
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   1
            Left            =   240
            TabIndex        =   117
            Top             =   600
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   2
            Left            =   240
            TabIndex        =   116
            Top             =   840
            Width           =   804
         End
      End
      Begin TabDlg.SSTab SSTab1 
         Height          =   4812
         Index           =   1
         Left            =   -74880
         TabIndex        =   121
         TabStop         =   0   'False
         Top             =   360
         Width           =   7452
         _ExtentX        =   13150
         _ExtentY        =   8493
         _Version        =   393216
         Style           =   1
         Tab             =   2
         TabHeight       =   420
         TabCaption(0)   =   "Leaching"
         TabPicture(0)   =   "GEN_NFD.frx":096D
         Tab(0).ControlEnabled=   0   'False
         Tab(0).Control(0)=   "sl(0)"
         Tab(0).Control(0).Enabled=   0   'False
         Tab(0).ControlCount=   1
         TabCaption(1)   =   "Resuspension"
         TabPicture(1)   =   "GEN_NFD.frx":0989
         Tab(1).ControlEnabled=   0   'False
         Tab(1).Control(0)=   "sl(1)"
         Tab(1).Control(0).Enabled=   0   'False
         Tab(1).ControlCount=   1
         TabCaption(2)   =   "Description"
         TabPicture(2)   =   "GEN_NFD.frx":09A5
         Tab(2).ControlEnabled=   -1  'True
         Tab(2).Control(0)=   "sl(2)"
         Tab(2).Control(0).Enabled=   0   'False
         Tab(2).ControlCount=   1
         Begin Threed.SSFrame sl 
            Height          =   4212
            Index           =   0
            Left            =   -74760
            TabIndex        =   122
            Top             =   360
            Width           =   6972
            _Version        =   65536
            _ExtentX        =   12298
            _ExtentY        =   7430
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
               Index           =   15
               ItemData        =   "GEN_NFD.frx":09C1
               Left            =   2760
               List            =   "GEN_NFD.frx":09C3
               Style           =   2  'Dropdown List
               TabIndex        =   1
               Top             =   360
               Width           =   2892
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
               Index           =   86
               Left            =   4560
               Style           =   2  'Dropdown List
               TabIndex        =   5
               Top             =   1200
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
               Index           =   87
               Left            =   4560
               Style           =   2  'Dropdown List
               TabIndex        =   7
               Top             =   1560
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
               Index           =   88
               Left            =   4560
               Style           =   2  'Dropdown List
               TabIndex        =   9
               Top             =   1920
               Width           =   1092
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   88
               Left            =   3600
               TabIndex        =   8
               Top             =   1920
               Width           =   972
            End
            Begin VB.ComboBox kd1 
               Height          =   288
               ItemData        =   "GEN_NFD.frx":09C5
               Left            =   480
               List            =   "GEN_NFD.frx":09C7
               Style           =   2  'Dropdown List
               TabIndex        =   10
               Top             =   2880
               Width           =   2052
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   92
               Left            =   3840
               TabIndex        =   14
               Top             =   3360
               Width           =   972
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
            Begin VB.ComboBox kd2 
               Height          =   288
               ItemData        =   "GEN_NFD.frx":09C9
               Left            =   3840
               List            =   "GEN_NFD.frx":09CB
               Style           =   2  'Dropdown List
               TabIndex        =   13
               Top             =   2880
               Width           =   2052
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   91
               Left            =   480
               TabIndex        =   11
               Top             =   3360
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   91
               Left            =   1440
               Style           =   2  'Dropdown List
               TabIndex        =   12
               Top             =   3360
               Width           =   1092
            End
            Begin VB.Label blbl 
               Height          =   252
               Index           =   15
               Left            =   240
               TabIndex        =   136
               Top             =   360
               Width           =   2532
            End
            Begin VB.Label bref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   15
               Left            =   5760
               TabIndex        =   135
               Top             =   360
               Width           =   972
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   85
               Left            =   5760
               TabIndex        =   134
               Top             =   840
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   85
               Left            =   240
               TabIndex        =   133
               Top             =   840
               Width           =   3252
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   86
               Left            =   5760
               TabIndex        =   132
               Top             =   1200
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   86
               Left            =   240
               TabIndex        =   131
               Top             =   1200
               Width           =   3252
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   87
               Left            =   5760
               TabIndex        =   130
               Top             =   1560
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   87
               Left            =   240
               TabIndex        =   129
               Top             =   1560
               Width           =   3252
            End
            Begin VB.Label lbl 
               Caption         =   "Soil absorption coefficient (Kd)"
               Height          =   252
               Index           =   91
               Left            =   240
               TabIndex        =   128
               Top             =   2520
               Width           =   3252
            End
            Begin VB.Label bref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   16
               Left            =   2640
               TabIndex        =   127
               Top             =   2880
               Width           =   972
            End
            Begin VB.Label bref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   17
               Left            =   6000
               TabIndex        =   126
               Top             =   2880
               Width           =   972
            End
            Begin VB.Label lbl 
               Caption         =   "Progeny (Kd)"
               Height          =   252
               Index           =   92
               Left            =   3600
               TabIndex        =   125
               Top             =   2520
               Width           =   3252
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   88
               Left            =   5760
               TabIndex        =   124
               Top             =   1920
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   88
               Left            =   240
               TabIndex        =   123
               Top             =   1920
               Width           =   3252
            End
         End
         Begin Threed.SSFrame sl 
            Height          =   4332
            Index           =   1
            Left            =   -74760
            TabIndex        =   137
            Top             =   360
            Width           =   6972
            _Version        =   65536
            _ExtentX        =   12298
            _ExtentY        =   7641
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
               Index           =   42
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   22
               Top             =   1680
               Width           =   1000
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   41
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   20
               Top             =   1320
               Width           =   1000
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   40
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   18
               Top             =   960
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   42
               Left            =   3840
               TabIndex        =   21
               Top             =   1680
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   41
               Left            =   3840
               TabIndex        =   19
               Top             =   1320
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   40
               Left            =   3840
               TabIndex        =   17
               Top             =   960
               Width           =   1000
            End
            Begin VB.ComboBox cb 
               Height          =   288
               Index           =   14
               ItemData        =   "GEN_NFD.frx":09CD
               Left            =   3000
               List            =   "GEN_NFD.frx":09CF
               Style           =   2  'Dropdown List
               TabIndex        =   16
               Top             =   360
               Width           =   2772
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   42
               Left            =   120
               TabIndex        =   242
               Top             =   1680
               Width           =   3696
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   42
               Left            =   5880
               TabIndex        =   241
               Top             =   1680
               Width           =   996
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   41
               Left            =   120
               TabIndex        =   240
               Top             =   1320
               Width           =   3696
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   41
               Left            =   5880
               TabIndex        =   239
               Top             =   1320
               Width           =   996
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   40
               Left            =   120
               TabIndex        =   238
               Top             =   960
               Width           =   3696
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   40
               Left            =   5880
               TabIndex        =   237
               Top             =   960
               Width           =   996
            End
            Begin VB.Label blbl 
               Height          =   252
               Index           =   14
               Left            =   120
               TabIndex        =   139
               Top             =   360
               Width           =   2892
            End
            Begin VB.Label bref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   14
               Left            =   5880
               TabIndex        =   138
               Top             =   360
               Width           =   972
            End
         End
         Begin Threed.SSFrame sl 
            Height          =   4212
            Index           =   2
            Left            =   240
            TabIndex        =   140
            Top             =   360
            Width           =   6972
            _Version        =   65536
            _ExtentX        =   12298
            _ExtentY        =   7430
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
               TabIndex        =   80
               Top             =   3660
               Width           =   1000
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   9
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   70
               Top             =   1860
               Width           =   1000
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   8
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   68
               Top             =   1500
               Width           =   1000
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   37
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   78
               Top             =   3300
               Width           =   1000
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   36
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   76
               Top             =   2940
               Width           =   1000
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   35
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   74
               Top             =   2580
               Width           =   1000
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   34
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   72
               Top             =   2220
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   38
               Left            =   3840
               TabIndex        =   79
               Top             =   3660
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   9
               Left            =   3840
               TabIndex        =   69
               Top             =   1860
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   8
               Left            =   3840
               TabIndex        =   67
               Top             =   1500
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   37
               Left            =   3840
               TabIndex        =   77
               Top             =   3300
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   36
               Left            =   3840
               TabIndex        =   75
               Top             =   2940
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   35
               Left            =   3840
               TabIndex        =   73
               Top             =   2580
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   34
               Left            =   3840
               TabIndex        =   71
               Top             =   2220
               Width           =   1000
            End
            Begin Threed.SSCheck SSCheck1 
               Height          =   252
               Index           =   8
               Left            =   1080
               TabIndex        =   268
               Top             =   1140
               Width           =   3996
               _Version        =   65536
               _ExtentX        =   7049
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
               Index           =   5
               Left            =   1080
               TabIndex        =   66
               Top             =   540
               Width           =   3996
               _Version        =   65536
               _ExtentX        =   7049
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
               Index           =   3
               Left            =   1080
               TabIndex        =   65
               Top             =   840
               Width           =   3996
               _Version        =   65536
               _ExtentX        =   7049
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
            Begin VB.Label cref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   8
               Left            =   240
               TabIndex        =   267
               Top             =   1140
               Width           =   804
            End
            Begin VB.Label lbl 
               Height          =   372
               Index           =   38
               Left            =   120
               TabIndex        =   236
               Top             =   3660
               Width           =   3696
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   38
               Left            =   5880
               TabIndex        =   235
               Top             =   3660
               Width           =   996
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   9
               Left            =   120
               TabIndex        =   234
               Top             =   1860
               Width           =   3696
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   9
               Left            =   5880
               TabIndex        =   233
               Top             =   1860
               Width           =   996
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   8
               Left            =   120
               TabIndex        =   232
               Top             =   1500
               Width           =   3696
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   8
               Left            =   5880
               TabIndex        =   231
               Top             =   1500
               Width           =   996
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   37
               Left            =   120
               TabIndex        =   230
               Top             =   3300
               Width           =   3696
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   37
               Left            =   5880
               TabIndex        =   229
               Top             =   3300
               Width           =   996
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   36
               Left            =   120
               TabIndex        =   228
               Top             =   2940
               Width           =   3696
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   36
               Left            =   5880
               TabIndex        =   227
               Top             =   2940
               Width           =   996
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   35
               Left            =   120
               TabIndex        =   226
               Top             =   2580
               Width           =   3696
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   35
               Left            =   5880
               TabIndex        =   225
               Top             =   2580
               Width           =   996
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   34
               Left            =   120
               TabIndex        =   224
               Top             =   2220
               Width           =   3696
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   34
               Left            =   5880
               TabIndex        =   223
               Top             =   2220
               Width           =   996
            End
            Begin VB.Label cref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   5
               Left            =   240
               TabIndex        =   222
               Top             =   540
               Width           =   804
            End
            Begin VB.Label cref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   3
               Left            =   240
               TabIndex        =   221
               Top             =   840
               Width           =   804
            End
         End
      End
      Begin TabDlg.SSTab SSTab1 
         Height          =   4812
         Index           =   2
         Left            =   -74880
         TabIndex        =   141
         TabStop         =   0   'False
         Top             =   360
         Width           =   7452
         _ExtentX        =   13150
         _ExtentY        =   8493
         _Version        =   393216
         Style           =   1
         Tabs            =   5
         TabsPerRow      =   5
         TabHeight       =   423
         TabCaption(0)   =   "General"
         TabPicture(0)   =   "GEN_NFD.frx":09D1
         Tab(0).ControlEnabled=   -1  'True
         Tab(0).Control(0)=   "ag(0)"
         Tab(0).Control(0).Enabled=   0   'False
         Tab(0).ControlCount=   1
         TabCaption(1)   =   "Food Crop"
         TabPicture(1)   =   "GEN_NFD.frx":09ED
         Tab(1).ControlEnabled=   0   'False
         Tab(1).Control(0)=   "SSTab1(3)"
         Tab(1).Control(1)=   "ag(1)"
         Tab(1).ControlCount=   2
         TabCaption(2)   =   "Animal Feed"
         TabPicture(2)   =   "GEN_NFD.frx":0A09
         Tab(2).ControlEnabled=   0   'False
         Tab(2).Control(0)=   "SSTab1(4)"
         Tab(2).Control(1)=   "ag(2)"
         Tab(2).ControlCount=   2
         TabCaption(3)   =   "Intake delays"
         TabPicture(3)   =   "GEN_NFD.frx":0A25
         Tab(3).ControlEnabled=   0   'False
         Tab(3).Control(0)=   "ag(3)"
         Tab(3).ControlCount=   1
         TabCaption(4)   =   "Biotic Transport"
         TabPicture(4)   =   "GEN_NFD.frx":0A41
         Tab(4).ControlEnabled=   0   'False
         Tab(4).Control(0)=   "ag(4)"
         Tab(4).ControlCount=   1
         Begin Threed.SSFrame ag 
            Height          =   2412
            Index           =   1
            Left            =   -74640
            TabIndex        =   212
            Top             =   840
            Width           =   6732
            _Version        =   65536
            _ExtentX        =   11875
            _ExtentY        =   4255
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
               Index           =   22
               Left            =   3600
               TabIndex        =   95
               Top             =   360
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   23
               Left            =   3600
               TabIndex        =   97
               Top             =   840
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   24
               Left            =   3600
               TabIndex        =   99
               Top             =   1320
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   25
               Left            =   3600
               TabIndex        =   101
               Top             =   1800
               Width           =   1000
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   22
               Left            =   4560
               Style           =   2  'Dropdown List
               TabIndex        =   96
               Top             =   360
               Width           =   1000
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   23
               Left            =   4560
               Style           =   2  'Dropdown List
               TabIndex        =   98
               Top             =   840
               Width           =   1000
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   24
               Left            =   4560
               Style           =   2  'Dropdown List
               TabIndex        =   100
               Top             =   1320
               Width           =   1000
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   25
               Left            =   4560
               Style           =   2  'Dropdown List
               TabIndex        =   102
               Top             =   1800
               Width           =   1000
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   22
               Left            =   5640
               TabIndex        =   220
               Top             =   360
               Width           =   996
            End
            Begin VB.Label lbl 
               Height          =   480
               Index           =   22
               Left            =   120
               TabIndex        =   219
               Top             =   360
               Width           =   3400
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   23
               Left            =   5640
               TabIndex        =   218
               Top             =   840
               Width           =   996
            End
            Begin VB.Label lbl 
               Height          =   480
               Index           =   23
               Left            =   120
               TabIndex        =   217
               Top             =   840
               Width           =   3400
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   24
               Left            =   5640
               TabIndex        =   216
               Top             =   1320
               Width           =   996
            End
            Begin VB.Label lbl 
               Height          =   480
               Index           =   24
               Left            =   120
               TabIndex        =   215
               Top             =   1320
               Width           =   3400
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   25
               Left            =   5640
               TabIndex        =   214
               Top             =   1800
               Width           =   996
            End
            Begin VB.Label lbl 
               Height          =   480
               Index           =   25
               Left            =   120
               TabIndex        =   213
               Top             =   1800
               Width           =   3400
            End
         End
         Begin Threed.SSFrame ag 
            Height          =   3252
            Index           =   2
            Left            =   -74640
            TabIndex        =   199
            Top             =   1080
            Width           =   6612
            _Version        =   65536
            _ExtentX        =   11663
            _ExtentY        =   5736
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
               Index           =   16
               Left            =   3480
               TabIndex        =   103
               Top             =   240
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   17
               Left            =   3480
               TabIndex        =   105
               Top             =   720
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   18
               Left            =   3480
               TabIndex        =   107
               Top             =   1200
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   19
               Left            =   3480
               TabIndex        =   109
               Top             =   1680
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   20
               Left            =   3480
               TabIndex        =   111
               Top             =   2160
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   21
               Left            =   3480
               TabIndex        =   113
               Top             =   2640
               Width           =   1000
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   16
               Left            =   4440
               Style           =   2  'Dropdown List
               TabIndex        =   104
               Top             =   240
               Width           =   1000
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   17
               Left            =   4440
               Style           =   2  'Dropdown List
               TabIndex        =   106
               Top             =   720
               Width           =   1000
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   18
               Left            =   4440
               Style           =   2  'Dropdown List
               TabIndex        =   108
               Top             =   1200
               Width           =   1000
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   19
               Left            =   4440
               Style           =   2  'Dropdown List
               TabIndex        =   110
               Top             =   1680
               Width           =   1000
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   20
               Left            =   4440
               Style           =   2  'Dropdown List
               TabIndex        =   112
               Top             =   2160
               Width           =   1000
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   21
               Left            =   4440
               Style           =   2  'Dropdown List
               TabIndex        =   114
               Top             =   2640
               Width           =   1000
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   16
               Left            =   5520
               TabIndex        =   211
               Top             =   240
               Width           =   996
            End
            Begin VB.Label lbl 
               Height          =   480
               Index           =   16
               Left            =   120
               TabIndex        =   210
               Top             =   240
               Width           =   3300
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   17
               Left            =   5520
               TabIndex        =   209
               Top             =   720
               Width           =   996
            End
            Begin VB.Label lbl 
               Height          =   480
               Index           =   17
               Left            =   120
               TabIndex        =   208
               Top             =   720
               Width           =   3300
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   18
               Left            =   5520
               TabIndex        =   207
               Top             =   1200
               Width           =   996
            End
            Begin VB.Label lbl 
               Height          =   480
               Index           =   18
               Left            =   120
               TabIndex        =   206
               Top             =   1200
               Width           =   3300
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   19
               Left            =   5520
               TabIndex        =   205
               Top             =   1680
               Width           =   996
            End
            Begin VB.Label lbl 
               Height          =   480
               Index           =   19
               Left            =   120
               TabIndex        =   204
               Top             =   1680
               Width           =   3300
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   20
               Left            =   5520
               TabIndex        =   203
               Top             =   2160
               Width           =   996
            End
            Begin VB.Label lbl 
               Height          =   480
               Index           =   20
               Left            =   120
               TabIndex        =   202
               Top             =   2160
               Width           =   3300
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   21
               Left            =   5520
               TabIndex        =   201
               Top             =   2640
               Width           =   996
            End
            Begin VB.Label lbl 
               Height          =   480
               Index           =   21
               Left            =   120
               TabIndex        =   200
               Top             =   2640
               Width           =   3300
            End
         End
         Begin Threed.SSFrame ag 
            Height          =   4212
            Index           =   3
            Left            =   -74760
            TabIndex        =   142
            Top             =   360
            Width           =   6972
            _Version        =   65536
            _ExtentX        =   12298
            _ExtentY        =   7430
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
               Index           =   33
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   38
               Top             =   3000
               Width           =   1000
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   32
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   36
               Top             =   2640
               Width           =   1000
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   31
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   34
               Top             =   2280
               Width           =   1000
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   30
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   32
               Top             =   1920
               Width           =   1000
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   29
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   30
               Top             =   1440
               Width           =   1000
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   28
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   28
               Top             =   1080
               Width           =   1000
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   27
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   26
               Top             =   720
               Width           =   1000
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   26
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   24
               Top             =   360
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   33
               Left            =   3840
               TabIndex        =   37
               Top             =   3000
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   32
               Left            =   3840
               TabIndex        =   35
               Top             =   2640
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   31
               Left            =   3840
               TabIndex        =   33
               Top             =   2280
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   30
               Left            =   3840
               TabIndex        =   31
               Top             =   1920
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   29
               Left            =   3840
               TabIndex        =   29
               Top             =   1440
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   28
               Left            =   3840
               TabIndex        =   27
               Top             =   1080
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   27
               Left            =   3840
               TabIndex        =   25
               Top             =   720
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   26
               Left            =   3840
               TabIndex        =   23
               Top             =   360
               Width           =   1000
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   33
               Left            =   120
               TabIndex        =   196
               Top             =   3000
               Width           =   3696
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   33
               Left            =   5880
               TabIndex        =   195
               Top             =   3000
               Width           =   996
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   32
               Left            =   120
               TabIndex        =   194
               Top             =   2640
               Width           =   3696
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   32
               Left            =   5880
               TabIndex        =   193
               Top             =   2640
               Width           =   996
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   31
               Left            =   120
               TabIndex        =   192
               Top             =   2280
               Width           =   3696
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   31
               Left            =   5880
               TabIndex        =   191
               Top             =   2280
               Width           =   996
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   30
               Left            =   120
               TabIndex        =   190
               Top             =   1920
               Width           =   3696
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   30
               Left            =   5880
               TabIndex        =   189
               Top             =   1920
               Width           =   996
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   29
               Left            =   120
               TabIndex        =   188
               Top             =   1440
               Width           =   3696
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   29
               Left            =   5880
               TabIndex        =   187
               Top             =   1440
               Width           =   996
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   28
               Left            =   120
               TabIndex        =   186
               Top             =   1080
               Width           =   3696
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   28
               Left            =   5880
               TabIndex        =   185
               Top             =   1080
               Width           =   996
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   27
               Left            =   120
               TabIndex        =   184
               Top             =   720
               Width           =   3696
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   27
               Left            =   5880
               TabIndex        =   183
               Top             =   720
               Width           =   996
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   26
               Left            =   120
               TabIndex        =   182
               Top             =   360
               Width           =   3696
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   26
               Left            =   5880
               TabIndex        =   181
               Top             =   360
               Width           =   996
            End
         End
         Begin Threed.SSFrame ag 
            Height          =   4212
            Index           =   0
            Left            =   240
            TabIndex        =   143
            Top             =   360
            Width           =   6972
            _Version        =   65536
            _ExtentX        =   12298
            _ExtentY        =   7430
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
               Index           =   6
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   94
               Top             =   2880
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   6
               Left            =   3840
               TabIndex        =   93
               Top             =   2880
               Width           =   1000
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   15
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   92
               Top             =   2520
               Width           =   1000
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   14
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   90
               Top             =   2160
               Width           =   1000
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   13
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   88
               Top             =   1800
               Width           =   1000
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   12
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   86
               Top             =   1440
               Width           =   1000
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   10
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   84
               Top             =   960
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   15
               Left            =   3840
               TabIndex        =   91
               Top             =   2520
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   14
               Left            =   3840
               TabIndex        =   89
               Top             =   2160
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   13
               Left            =   3840
               TabIndex        =   87
               Top             =   1800
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   12
               Left            =   3840
               TabIndex        =   85
               Top             =   1440
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   10
               Left            =   3840
               TabIndex        =   83
               Top             =   960
               Width           =   1000
            End
            Begin Threed.SSCheck SSCheck1 
               Height          =   252
               Index           =   6
               Left            =   960
               TabIndex        =   81
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
            Begin Threed.SSCheck SSCheck1 
               Height          =   252
               Index           =   7
               Left            =   960
               TabIndex        =   82
               Top             =   600
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
               Index           =   6
               Left            =   5880
               TabIndex        =   244
               Top             =   2880
               Width           =   996
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   6
               Left            =   120
               TabIndex        =   243
               Top             =   2880
               Width           =   3696
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   15
               Left            =   120
               TabIndex        =   180
               Top             =   2520
               Width           =   3696
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   15
               Left            =   5880
               TabIndex        =   179
               Top             =   2520
               Width           =   996
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   14
               Left            =   120
               TabIndex        =   178
               Top             =   2160
               Width           =   3696
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   14
               Left            =   5880
               TabIndex        =   177
               Top             =   2160
               Width           =   996
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   13
               Left            =   120
               TabIndex        =   176
               Top             =   1800
               Width           =   3696
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   13
               Left            =   5880
               TabIndex        =   175
               Top             =   1800
               Width           =   996
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   12
               Left            =   120
               TabIndex        =   174
               Top             =   1440
               Width           =   3696
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   12
               Left            =   5880
               TabIndex        =   173
               Top             =   1440
               Width           =   996
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   10
               Left            =   240
               TabIndex        =   172
               Top             =   960
               Width           =   3504
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   10
               Left            =   5880
               TabIndex        =   171
               Top             =   960
               Width           =   996
            End
            Begin VB.Label cref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   6
               Left            =   120
               TabIndex        =   145
               Top             =   240
               Width           =   804
            End
            Begin VB.Label cref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   7
               Left            =   120
               TabIndex        =   144
               Top             =   600
               Width           =   804
            End
         End
         Begin TabDlg.SSTab SSTab1 
            Height          =   4092
            Index           =   4
            Left            =   -74880
            TabIndex        =   197
            TabStop         =   0   'False
            Top             =   480
            Width           =   7212
            _ExtentX        =   12726
            _ExtentY        =   7223
            _Version        =   393216
            Style           =   1
            Tabs            =   9
            TabsPerRow      =   5
            TabHeight       =   420
            TabCaption(0)   =   "Biomass"
            TabPicture(0)   =   "GEN_NFD.frx":0A5D
            Tab(0).ControlEnabled=   -1  'True
            Tab(0).ControlCount=   0
            TabCaption(1)   =   "Consumption"
            TabPicture(1)   =   "GEN_NFD.frx":0A79
            Tab(1).ControlEnabled=   0   'False
            Tab(1).ControlCount=   0
            TabCaption(2)   =   "Storage Time"
            TabPicture(2)   =   "GEN_NFD.frx":0A95
            Tab(2).ControlEnabled=   0   'False
            Tab(2).ControlCount=   0
            TabCaption(3)   =   "Diet Fraction"
            TabPicture(3)   =   "GEN_NFD.frx":0AB1
            Tab(3).ControlEnabled=   0   'False
            Tab(3).ControlCount=   0
            TabCaption(4)   =   "Growing Period"
            Tab(4).ControlEnabled=   0   'False
            Tab(4).ControlCount=   0
            TabCaption(5)   =   "Yield"
            TabPicture(5)   =   "GEN_NFD.frx":0ACD
            Tab(5).ControlEnabled=   0   'False
            Tab(5).ControlCount=   0
            TabCaption(6)   =   "Translocation Factor"
            TabPicture(6)   =   "GEN_NFD.frx":0AE9
            Tab(6).ControlEnabled=   0   'False
            Tab(6).ControlCount=   0
            TabCaption(7)   =   "Dry/Wet Ratio"
            TabPicture(7)   =   "GEN_NFD.frx":0B05
            Tab(7).ControlEnabled=   0   'False
            Tab(7).ControlCount=   0
            TabCaption(8)   =   "Soil Intake"
            TabPicture(8)   =   "GEN_NFD.frx":0B21
            Tab(8).ControlEnabled=   0   'False
            Tab(8).ControlCount=   0
         End
         Begin TabDlg.SSTab SSTab1 
            Height          =   3012
            Index           =   3
            Left            =   -74880
            TabIndex        =   198
            TabStop         =   0   'False
            Top             =   480
            Width           =   7212
            _ExtentX        =   12726
            _ExtentY        =   5318
            _Version        =   393216
            Style           =   1
            Tabs            =   5
            TabsPerRow      =   5
            TabHeight       =   420
            TabCaption(0)   =   "Biomass"
            TabPicture(0)   =   "GEN_NFD.frx":0B3D
            Tab(0).ControlEnabled=   -1  'True
            Tab(0).ControlCount=   0
            TabCaption(1)   =   "Growing Period"
            Tab(1).ControlEnabled=   0   'False
            Tab(1).ControlCount=   0
            TabCaption(2)   =   "Yield"
            Tab(2).ControlEnabled=   0   'False
            Tab(2).ControlCount=   0
            TabCaption(3)   =   "Translocation Factor"
            Tab(3).ControlEnabled=   0   'False
            Tab(3).ControlCount=   0
            TabCaption(4)   =   "Dry/Wet Ratio"
            Tab(4).ControlEnabled=   0   'False
            Tab(4).ControlCount=   0
         End
         Begin Threed.SSFrame ag 
            Height          =   4212
            Index           =   4
            Left            =   -74760
            TabIndex        =   254
            Top             =   360
            Width           =   6972
            _Version        =   65536
            _ExtentX        =   12298
            _ExtentY        =   7430
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
               Index           =   45
               Left            =   3840
               TabIndex        =   260
               Top             =   1560
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   44
               Left            =   3840
               TabIndex        =   259
               Top             =   960
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   43
               Left            =   3840
               TabIndex        =   258
               Top             =   360
               Width           =   1000
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   45
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   257
               Top             =   1560
               Width           =   1000
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   44
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   256
               Top             =   960
               Width           =   1000
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   43
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   255
               Top             =   360
               Width           =   1000
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   45
               Left            =   5880
               TabIndex        =   266
               Top             =   1560
               Width           =   996
            End
            Begin VB.Label lbl 
               Height          =   400
               Index           =   45
               Left            =   120
               TabIndex        =   265
               Top             =   1560
               Width           =   3696
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   44
               Left            =   5880
               TabIndex        =   264
               Top             =   960
               Width           =   996
            End
            Begin VB.Label lbl 
               Height          =   400
               Index           =   44
               Left            =   120
               TabIndex        =   263
               Top             =   960
               Width           =   3696
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   43
               Left            =   5880
               TabIndex        =   262
               Top             =   360
               Width           =   996
            End
            Begin VB.Label lbl 
               Height          =   400
               Index           =   43
               Left            =   120
               TabIndex        =   261
               Top             =   360
               Width           =   3696
            End
         End
      End
      Begin Threed.SSFrame frm 
         Height          =   4812
         Index           =   3
         Left            =   -74760
         TabIndex        =   146
         Top             =   360
         Width           =   7212
         _Version        =   65536
         _ExtentX        =   12721
         _ExtentY        =   8488
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
            Index           =   18
            Left            =   4200
            TabIndex        =   48
            Top             =   600
            Width           =   2700
            _Version        =   65536
            _ExtentX        =   4763
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
            Index           =   19
            Left            =   4200
            TabIndex        =   49
            Top             =   1440
            Width           =   2700
            _Version        =   65536
            _ExtentX        =   4763
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
            Index           =   16
            Left            =   1080
            TabIndex        =   46
            Top             =   2400
            Width           =   2004
            _Version        =   65536
            _ExtentX        =   3535
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
            Index           =   15
            Left            =   1080
            TabIndex        =   45
            Top             =   2160
            Width           =   2004
            _Version        =   65536
            _ExtentX        =   3535
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
            Index           =   14
            Left            =   1080
            TabIndex        =   44
            Top             =   1920
            Width           =   2004
            _Version        =   65536
            _ExtentX        =   3535
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
            Index           =   13
            Left            =   1080
            TabIndex        =   43
            Top             =   1680
            Width           =   2004
            _Version        =   65536
            _ExtentX        =   3535
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
            Index           =   12
            Left            =   1080
            TabIndex        =   42
            Top             =   1320
            Width           =   2004
            _Version        =   65536
            _ExtentX        =   3535
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
            Index           =   11
            Left            =   1080
            TabIndex        =   41
            Top             =   1080
            Width           =   2004
            _Version        =   65536
            _ExtentX        =   3535
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
            Index           =   10
            Left            =   1080
            TabIndex        =   40
            Top             =   840
            Width           =   2004
            _Version        =   65536
            _ExtentX        =   3535
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
            Index           =   9
            Left            =   1080
            TabIndex        =   39
            Top             =   600
            Width           =   2004
            _Version        =   65536
            _ExtentX        =   3535
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
            Index           =   17
            Left            =   1080
            TabIndex        =   47
            Top             =   2760
            Width           =   2004
            _Version        =   65536
            _ExtentX        =   3535
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
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   18
            Left            =   3360
            TabIndex        =   160
            Top             =   600
            Width           =   804
         End
         Begin VB.Label Label2 
            Caption         =   "External"
            Height          =   252
            Left            =   3360
            TabIndex        =   159
            Top             =   1200
            Width           =   2004
         End
         Begin VB.Label Label1 
            Caption         =   "Inhalation"
            Height          =   252
            Left            =   3360
            TabIndex        =   158
            Top             =   360
            Width           =   2004
         End
         Begin VB.Label Label4 
            Caption         =   "Ingestion"
            Height          =   252
            Left            =   240
            TabIndex        =   157
            Top             =   360
            Width           =   2000
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   9
            Left            =   240
            TabIndex        =   156
            Top             =   600
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   10
            Left            =   240
            TabIndex        =   155
            Top             =   840
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   11
            Left            =   240
            TabIndex        =   154
            Top             =   1080
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   12
            Left            =   240
            TabIndex        =   153
            Top             =   1320
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   13
            Left            =   240
            TabIndex        =   152
            Top             =   1680
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   14
            Left            =   240
            TabIndex        =   151
            Top             =   1920
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   15
            Left            =   240
            TabIndex        =   150
            Top             =   2160
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   16
            Left            =   240
            TabIndex        =   149
            Top             =   2400
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   17
            Left            =   240
            TabIndex        =   148
            Top             =   2760
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   19
            Left            =   3360
            TabIndex        =   147
            Top             =   1440
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
Attribute VB_Name = "gen_nfd"
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
  
Private Sub animalfeed()
'Animal feed
 'Feed biomass
  gn_t(53).Init "BIOMA2", "Standing biomass (wet) for meat animal feed", lbl(16), txt(16), unt(16), ref(16), "kg/m^2", 0.1, 10, 1.6, 1
  gn_t(54).Init "BIOMA2", "Standing biomass (wet) for poultry animal feed", lbl(17), txt(17), unt(17), ref(17), "kg/m^2", 0.1, 10, 1.6, 2
  gn_t(55).Init "BIOMA2", "Standing biomass (wet) for milk animal feed", lbl(18), txt(18), unt(18), ref(18), "kg/m^2", 0.1, 10, 1, 3
  gn_t(56).Init "BIOMA2", "Standing biomass (wet) for egg animal feed", lbl(19), txt(19), unt(19), ref(19), "kg/m^2", 0.1, 10, 1.6, 4
  gn_t(57).Init "BIOMA2", "Standing biomass (wet) for meat animal forage", lbl(20), txt(20), unt(20), ref(20), "kg/m^2", 0.1, 10, 1, 5
  gn_t(58).Init "BIOMA2", "Standing biomass (wet) for milk animal forage", lbl(21), txt(21), unt(21), ref(21), "kg/m^2", 0.1, 10, 1.5, 6
  gn_l(1).Add gn_t(53):    gn_l(1).Add gn_t(54):    gn_l(1).Add gn_t(55):    gn_l(1).Add gn_t(56):    gn_l(1).Add gn_t(57):    gn_l(1).Add gn_t(58)
 'Feed consumption
  gn_t(59).Init "CONSUM", "Meat animal feed consumption rate (grain)", lbl(16), txt(16), unt(16), ref(16), "kg/d", 0.001, 300, 68, 1
  gn_t(60).Init "CONSUM", "Poultry animal feed consumption rate (grain)", lbl(17), txt(17), unt(17), ref(17), "kg/d", 0.001, 5, 0.12, 2
  gn_t(61).Init "CONSUM", "Milk animal feed consumption rate (hay)", lbl(18), txt(18), unt(18), ref(18), "kg/d", 0.001, 300, 55, 3
  gn_t(62).Init "CONSUM", "Egg animal feed consumption rate (grain)", lbl(19), txt(19), unt(19), ref(19), "kg/d", 0.001, 5, 0.12, 4
  gn_t(63).Init "CONSUM", "Meat animal forage consumption rate (hay)", lbl(20), txt(20), unt(20), ref(20), "kg/d", 0.001, 300, 68, 5
  gn_t(64).Init "CONSUM", "Milk animal forage consumption rate (grass)", lbl(21), txt(21), unt(21), ref(21), "kg/d", 0.001, 300, 55, 6
  gn_l(1).Add gn_t(59):    gn_l(1).Add gn_t(60):    gn_l(1).Add gn_t(61):    gn_l(1).Add gn_t(62):    gn_l(1).Add gn_t(63):    gn_l(1).Add gn_t(64)
 'feed delay times  tab 0, tab 1
  gn_t(65).Init "STORTM", "Storage time for meat animal feed", lbl(16), txt(16), unt(16), ref(16), "day", 0, 365, 180, 1
  gn_t(66).Init "STORTM", "Storage time for poultry animal feed", lbl(17), txt(17), unt(17), ref(17), "day", 0, 365, 180, 2
  gn_t(67).Init "STORTM", "Storage time for milk animal feed", lbl(18), txt(18), unt(18), ref(18), "day", 0, 365, 100, 3
  gn_t(68).Init "STORTM", "Storage time for egg animal feed", lbl(19), txt(19), unt(19), ref(19), "day", 0, 365, 180, 4
  gn_t(69).Init "STORTM", "Storage time for meat animal forage", lbl(20), txt(20), unt(20), ref(20), "day", 0, 365, 100, 5
  gn_t(70).Init "STORTM", "Storage time for milk animal forage", lbl(21), txt(21), unt(21), ref(21), "day", 0, 365, 0, 6
  gn_l(1).Add gn_t(65):    gn_l(1).Add gn_t(66):    gn_l(1).Add gn_t(67):    gn_l(1).Add gn_t(68):    gn_l(1).Add gn_t(69):    gn_l(1).Add gn_t(70)
 'Feed fractions
  gn_t(71).Init "DIETFR", "Fraction of meat animal intake that is contaminated feed", lbl(16), txt(16), unt(16), ref(16), "fraction", 0, 1, 0.25, 1
  gn_t(72).Init "DIETFR", "Fraction of poultry animal intake that is contaminated feed", lbl(17), txt(17), unt(17), ref(17), "fraction", 0, 1, 1, 2
  gn_t(73).Init "DIETFR", "Fraction of milk animal intake that is contaminated feed", lbl(18), txt(18), unt(18), ref(18), "fraction", 0, 1, 0.25, 3
  gn_t(74).Init "DIETFR", "Fraction of egg animal intake that is contaminated feed", lbl(19), txt(19), unt(19), ref(19), "fraction", 0, 1, 1, 4
  gn_t(75).Init "DIETFR", "Fraction of meat animal intake that is contaiminated forage", lbl(20), txt(20), unt(20), ref(20), "fraction", 0, 1, 0.75, 5
  gn_t(76).Init "DIETFR", "Fraction of milk animal intake that is contaminated forage", lbl(21), txt(21), unt(21), ref(21), "fraction", 0, 1, 0.75, 6
  gn_l(1).Add gn_t(71):    gn_l(1).Add gn_t(72):    gn_l(1).Add gn_t(73):    gn_l(1).Add gn_t(74):    gn_l(1).Add gn_t(75):    gn_l(1).Add gn_t(76)
 'Feed growing periods
  gn_t(77).Init "GRWPA", "Growing period for meat animal feed", lbl(16), txt(16), unt(16), ref(16), "day", 0, 365, 90, 1
  gn_t(78).Init "GRWPA", "Growing period for poultry animal feed", lbl(17), txt(17), unt(17), ref(17), "day", 0, 365, 90, 2
  gn_t(79).Init "GRWPA", "Growing period for milk animal feed", lbl(18), txt(18), unt(18), ref(18), "day", 0, 365, 45, 3
  gn_t(80).Init "GRWPA", "Growing period for egg animal feed", lbl(19), txt(19), unt(19), ref(19), "day", 0, 365, 90, 4
  gn_t(81).Init "GRWPA", "Growing period for meat animal forage", lbl(20), txt(20), unt(20), ref(20), "day", 0, 365, 45, 5
  gn_t(82).Init "GRWPA", "Growing period for milk animal forage", lbl(21), txt(21), unt(21), ref(21), "day", 0, 365, 30, 6
  gn_l(1).Add gn_t(77):    gn_l(1).Add gn_t(78):    gn_l(1).Add gn_t(79):    gn_l(1).Add gn_t(80):    gn_l(1).Add gn_t(81):    gn_l(1).Add gn_t(82)
 'Animal feed yeilds
  gn_t(83).Init "YELDA", "Yield of meat animal feed", lbl(16), txt(16), unt(16), ref(16), "kg/m^2", 0.1, 10, 0.8, 1
  gn_t(84).Init "YELDA", "Yield of poultry animal feed", lbl(17), txt(17), unt(17), ref(17), "kg/m^2", 0.1, 10, 0.8, 2
  gn_t(85).Init "YELDA", "Yield of milk animal feed", lbl(18), txt(18), unt(18), ref(18), "kg/m^2", 0.1, 10, 2, 3
  gn_t(86).Init "YELDA", "Yield of egg animal feed", lbl(19), txt(19), unt(19), ref(19), "kg/m^2", 0.1, 10, 0.8, 4
  gn_t(87).Init "YELDA", "Yield of meat animal forage", lbl(20), txt(20), unt(20), ref(20), "kg/m^2", 0.1, 10, 2, 5
  gn_t(88).Init "YELDA", "Yield of milk animal forage", lbl(21), txt(21), unt(21), ref(21), "kg/m^2", 0.1, 10, 0.8, 6
  gn_l(1).Add gn_t(83):    gn_l(1).Add gn_t(84):    gn_l(1).Add gn_t(85):    gn_l(1).Add gn_t(86):    gn_l(1).Add gn_t(87):    gn_l(1).Add gn_t(88)
 'Feed translocation factors
  gn_t(89).Init "TRANSA", "Translocation factor of feed plant for meat animal", lbl(16), txt(16), unt(16), ref(16), "fraction", 0, 1, 0.1, 1
  gn_t(90).Init "TRANSA", "Translocation factor of feed plant for poultry animal", lbl(17), txt(17), unt(17), ref(17), "fraction", 0, 1, 0.1, 2
  gn_t(91).Init "TRANSA", "Translocation factor of feed plant for milk animal", lbl(18), txt(18), unt(18), ref(18), "fraction", 0, 1, 1, 3
  gn_t(92).Init "TRANSA", "Translocation factor of feed plant for egg animal", lbl(19), txt(19), unt(19), ref(19), "fraction", 0, 1, 0.1, 4
  gn_t(93).Init "TRANSA", "Translocation factor of forage plant for meat animal", lbl(20), txt(20), unt(20), ref(20), "fraction", 0, 1, 1, 5
  gn_t(94).Init "TRANSA", "Translocation factor of forage plant for milk animal", lbl(21), txt(21), unt(21), ref(21), "fraction", 0, 1, 1, 6
  gn_l(1).Add gn_t(89):    gn_l(1).Add gn_t(90):    gn_l(1).Add gn_t(91):    gn_l(1).Add gn_t(92):    gn_l(1).Add gn_t(93):    gn_l(1).Add gn_t(94)
 'Feed translocation factors
  gn_t(95).Init "DRYFA2", "Dry/wet weight ratio for meat animal feed", lbl(16), txt(16), unt(16), ref(16), "fraction", 0.05, 0.95, 0.18, 1
  gn_t(96).Init "DRYFA2", "Dry/wet weight ratio for poultry animal feed", lbl(17), txt(17), unt(17), ref(17), "fraction", 0.05, 0.95, 0.18, 2
  gn_t(97).Init "DRYFA2", "Dry/wet weight ratio for milk animal feed", lbl(18), txt(18), unt(18), ref(18), "fraction", 0.05, 0.95, 0.18, 3
  gn_t(98).Init "DRYFA2", "Dry/wet weight ratio for egg animal feed", lbl(19), txt(19), unt(19), ref(19), "fraction", 0.05, 0.95, 0.18, 4
  gn_t(99).Init "DRYFA2", "Dry/wet weight ratio for meat animal forage", lbl(20), txt(20), unt(20), ref(20), "fraction", 0.05, 0.95, 0.18, 5
  gn_t(100).Init "DRYFA2", "Dry/wet weight ratio for milk animal forage", lbl(21), txt(21), unt(21), ref(21), "fraction", 0.05, 0.95, 0.2, 6
  gn_l(1).Add gn_t(95):    gn_l(1).Add gn_t(96):    gn_l(1).Add gn_t(97):    gn_l(1).Add gn_t(98):    gn_l(1).Add gn_t(99):    gn_l(1).Add gn_t(100)
'added by BAN 26 May 2005 for completeness; cannot separate feed/fodder for meat, milk
'ANIMAL SOIL INTAKE
  gn_t(110).Init "SLCONA", "Soil intake rate for meat animal with feed", lbl(16), txt(16), unt(16), ref(16), "kg/d", 0#, 5#, 0#, 1
  gn_t(111).Init "SLCONA", "Soil intake rate for poultry animal", lbl(17), txt(17), unt(17), ref(17), "kg/d", 0#, 1#, 0.005, 2
  gn_t(112).Init "SLCONA", "Soil intake rate for milk animal with feed", lbl(18), txt(18), unt(18), ref(18), "kg/d", 0#, 5#, 0#, 3
  gn_t(113).Init "SLCONA", "Soil intake rate for egg animal", lbl(19), txt(19), unt(19), ref(19), "kg/d", 0#, 1#, 0.005, 4
  gn_t(114).Init "SLCONA", "Soil intake rate for meat animal with forage", lbl(20), txt(20), unt(20), ref(20), "kg/d", 0#, 5#, 1#, 5
  gn_t(115).Init "SLCONA", "Soil intake rate for milk animal with forage", lbl(21), txt(21), unt(21), ref(21), "kg/d", 0#, 5#, 1#, 6
  gn_l(1).Add gn_t(110):    gn_l(1).Add gn_t(111):    gn_l(1).Add gn_t(112):    gn_l(1).Add gn_t(113):    gn_l(1).Add gn_t(114):    gn_l(1).Add gn_t(115)

End Sub
  
Sub initprm()
  Dim i As Long
  
'Controls
  gn_t(0).Init "BEFORE", "Time from start to exposure", lbl(0), txt(0), unt(0), ref(0), "yr", 0, 1000000, 0
  gn_t(1).Init "NTKEND", "Duration of exposure period", lbl(1), txt(1), unt(1), ref(1), "yr", 0, 100, 1
  gn_t(2).Init "RELEND", "End of release period", lbl(2), txt(2), unt(2), ref(2), "yr", 0, 1000000, 1
  gn_i(0).Init "LOIC", "Loss of institutional control time prior to exposure", lbl(4), txt(4), unt(4), ref(4), "yr", 0, gn_t(0), 0
  gn_t(4).Init "ABSHUM", "Absolute humidity, used only for tritium model", lbl(5), txt(5), unt(5), ref(5), "g/m^3", 0, 0.1, 0.008
  gn_t(3).Init "PACKHL", "Waste package half life", lbl(3), txt(3), unt(3), ref(3), "yr", 0, 20, 1
  
  gn_l(0).Init "TFOOD", "Terrestrial food crop ingestion", SSCheck1(1), cref(1)
  gn_l(1).Init "ANFOOD", "Animal product ingestion", SSCheck1(0), cref(0)
  gn_l(2).Init "DEBUG", "Debug testing", SSCheck1(2), cref(2)
  
  gn_s(2).Init "SURSOIL", "Surface soil source", blbl(0), cb(0), bref(0)
  gn_s(3).Init "DEPSOIL", "Deep soil source", blbl(1), cb(1), bref(1)
  gn_s(4).Init "WASTPAK", "Waste package soil source", blbl(2), cb(2), bref(2)

'Soil
 'Leaching
  cb(15).AddItem ("GENII default leach rates"):                   cb(15).ItemData(cb(15).NewIndex) = 1
  cb(15).AddItem ("Leach rates calculated from user input"):      cb(15).ItemData(cb(15).NewIndex) = 2
  cb(15).AddItem ("User provided leach rate constants"):          cb(15).ItemData(cb(15).NewIndex) = 3
  gn_s(0).Init "LEACHOPTION", "Type of leach rate constant", blbl(15), cb(15), bref(15)
  gn_t(6).Init "THICK", "Surface soil thickness", lbl(85), txt(85), unt(85), ref(85), "cm", 0, 10000, 200
  gn_t(7).Init "MOISTC", "Surface soil moisture content", lbl(86), txt(86), unt(86), ref(86), "fraction", 0, 1, 0.08
  gn_t(8).Init "BULKD", "Surface soil bulk density", lbl(87), txt(87), unt(87), ref(87), "g/cm^3", 0.5, 3, 1.6
  gn_t(9).Init "VLEACH", "Total infiltration rate", lbl(88), txt(88), unt(88), ref(88), "cm/yr", 0, 500, 20
  
  gn_t(11).Init "SOILKD", "Parent soil absorption coefficient (Kd)", lbl(91), txt(91), unt(91), bref(16), "ml/g", 0, 1000000, 0
  gn_t(12).Init "SOILKDD", "Progeny soil absorbtion coefficient (Kd)", lbl(92), txt(92), unt(92), bref(17), "ml/g", 0, 1000000, 0
  gn_t(13).Init "LEACHR", "Parent soil leach rate constant", lbl(91), txt(91), unt(91), bref(16), "1/yr", 0.00001, 1, 0.001
  gn_t(14).Init "LEACHRD", "Progeny soil leach rate constant", lbl(92), txt(92), unt(92), bref(17), "1/yr", 0.00001, 1, 0.001
 
 'Resuspension
  cb(14).AddItem "No Resuspension model":                    cb(14).ItemData(cb(14).NewIndex) = 0
  cb(14).AddItem "Use mass loading model":                   cb(14).ItemData(cb(14).NewIndex) = 1
  cb(14).AddItem "Use Anspaugh model":                       cb(14).ItemData(cb(14).NewIndex) = 2
  cb(14).AddItem "User input of resuspension factor":        cb(14).ItemData(cb(14).NewIndex) = 3
  gn_s(1).Init "IRES", "Type of model to run", blbl(14), cb(14), bref(14)
  gn_t(15).Init "RESFAC", "Resuspension factor", lbl(40), txt(40), unt(40), ref(40), "1/m", 0, 0.001, 0.000000001
  gn_t(16).Init "XMLF", "Mass loading factor for resuspension model", lbl(41), txt(41), unt(41), ref(41), "g/m^3", 0, 5, 0.00005
  gn_t(17).Init "AVALSL", "Depth of top soil available for resuspension", lbl(42), txt(42), unt(42), ref(42), "cm", 0.01, 2, 1
  gn_s(1).Add gn_t(15):    gn_s(1).Add gn_t(16):    gn_s(1).Add gn_t(17)
    
 
 'Description
  gn_l(3).Init "BTPRE", "Estimate biotic transport rate prior to start of intake", SSCheck1(3), cref(3)
  gn_l(4).Init "ARID", "Arid climate during period prior to intake", SSCheck1(8), cref(8)
  gn_l(3).Add gn_l(4)
'  gn_l(4).Init "BURWAS", "Buried waste source", SSCheck1(4), cref(4)
  gn_l(5).Init "BIOT", "Biotic transport from deep soil to surface soil", SSCheck1(5), cref(5)
  gn_t(18).Init "SLDN", "Surface soil areal density", lbl(8), txt(8), unt(8), ref(8), "kg/m^2", 0.00001, 3000, 224
  gn_t(19).Init "SSLDN", "Surface soil density", lbl(9), txt(9), unt(9), ref(9), "kg/m^3", 0.00001, 3000, 1500
  gn_t(20).Init "SURCM", "Surface soil layer thickness used for density", lbl(34), txt(34), unt(34), ref(34), "cm", 0.00001, 500, 15
  gn_t(21).Init "OVRBRD", "Depth of soil overburden over waste", lbl(35), txt(35), unt(35), ref(35), "m", 0, 20, 1
  gn_t(22).Init "WASDEP", "Thickness of buried waste layer", lbl(36), txt(36), unt(36), ref(36), "m", 0.01, 100, 1
  gn_t(23).Init "FRSIZ", "Source area for external dose modification factor", lbl(37), txt(37), unt(37), ref(37), "m^2", 1, 10000#, 1250
  gn_t(24).Init "MANULR", "Manual redistribution factor", lbl(38), txt(38), unt(38), ref(38), "fraction", 0, 1, 1
  
'Agriculture
'General
  gn_l(6).Init "HARVST", "Radionuclide removal due to harvesting", SSCheck1(6), cref(6)
  gn_l(7).Init "DRYSET", "User defined dry deposition retention fraction to plants", SSCheck1(7), cref(7)
  
  gn_t(27).Init "DEPFR1", "Dry deposition retention fraction to plants", lbl(10), txt(10), unt(10), ref(10), "fraction", 0, 1, 0.2
  gn_t(28).Init "LEAFRS", "Resuspension factor from soil to plant surfaces", lbl(12), txt(12), unt(12), ref(12), "1/m", 0, 0.001, 0.000000001
  gn_t(29).Init "DPVRES", "Deposition velocity from soil to plant surfaces", lbl(13), txt(13), unt(13), ref(13), "m/s", 0, 0.1, 0.001
  gn_t(30).Init "WTIM", "Weathering rate constant from plants", lbl(14), txt(14), unt(14), ref(14), "d", 0.1, 100, 14
  
  gn_l(7).Add gn_t(27)
 
  gn_t(31).Init "RF1", "Fraction of plants roots in surface soil", lbl(15), txt(15), unt(15), ref(15), "fraction", 0, 1, 1
  gn_t(32).Init "RF2", "Fraction of plants roots in deep soil", lbl(6), txt(6), unt(6), ref(6), "fraction", 0, 1, 1

'Food crop
 'Food crop biomass
  gn_t(33).Init "BIOMAS", "Standing biomass (wet) for leafy vegetables", lbl(22), txt(22), unt(22), ref(22), "kg/m^2", 0.1, 10, 1.6, 1
  gn_t(34).Init "BIOMAS", "Standing biomass (wet) for root vegetables", lbl(23), txt(23), unt(23), ref(23), "kg/m^2", 0.1, 10, 2, 2
  gn_t(35).Init "BIOMAS", "Standing biomass (wet) for fruits", lbl(24), txt(24), unt(24), ref(24), "kg/m^2", 0.1, 10, 3, 3
  gn_t(36).Init "BIOMAS", "Standing biomass (wet) for grains", lbl(25), txt(25), unt(25), ref(25), "kg/m^2", 0.1, 10, 0.8, 4
  gn_l(0).Add gn_t(33):    gn_l(0).Add gn_t(34):    gn_l(0).Add gn_t(35):    gn_l(0).Add gn_t(36)
 'Food crop growing periods tab 0, tab 4
  gn_t(37).Init "GRWP", "Growing period for leafy vegetables", lbl(22), txt(22), unt(22), ref(22), "day", 0, 365, 90, 1
  gn_t(38).Init "GRWP", "Growing period for root vegetables", lbl(23), txt(23), unt(23), ref(23), "day", 0, 365, 90, 2
  gn_t(39).Init "GRWP", "Growing period for fruits", lbl(24), txt(24), unt(24), ref(24), "day", 0, 365, 90, 3
  gn_t(40).Init "GRWP", "Growing period for grains", lbl(25), txt(25), unt(25), ref(25), "day", 0, 365, 90, 4
  gn_l(0).Add gn_t(37):    gn_l(0).Add gn_t(38):    gn_l(0).Add gn_t(39):    gn_l(0).Add gn_t(40)
 'Food crop yields
  gn_t(41).Init "YELD", "Yield of leafy vegetables", lbl(22), txt(22), unt(22), ref(22), "kg/m^2", 0.001, 10, 1.5, 1
  gn_t(42).Init "YELD", "Yield of root vegetables", lbl(23), txt(23), unt(23), ref(23), "kg/m^2", 0.001, 10, 4, 2
  gn_t(43).Init "YELD", "Yield of fruits", lbl(24), txt(24), unt(24), ref(24), "kg/m^2", 0.001, 10, 2, 3
  gn_t(44).Init "YELD", "Yield of grains", lbl(25), txt(25), unt(25), ref(25), "kg/m^2", 0.001, 10, 0.8, 4
  gn_l(0).Add gn_t(41):    gn_l(0).Add gn_t(42):    gn_l(0).Add gn_t(43):    gn_l(0).Add gn_t(44)
 'Crop translocation factors
  gn_t(45).Init "TRANS", "Translocation factor of leafy vegetables", lbl(22), txt(22), unt(22), ref(22), "fraction", 0, 1, 1, 1
  gn_t(46).Init "TRANS", "Translocation factor of other vegetables", lbl(23), txt(23), unt(23), ref(23), "fraction", 0, 1, 0.1, 2
  gn_t(47).Init "TRANS", "Translocation factor of fruit", lbl(24), txt(24), unt(24), ref(24), "fraction", 0, 1, 0.1, 3
  gn_t(48).Init "TRANS", "Translocation factor of grain", lbl(25), txt(25), unt(25), ref(25), "fraction", 0, 1, 0.1, 4
  gn_l(0).Add gn_t(45):    gn_l(0).Add gn_t(46):    gn_l(0).Add gn_t(47):    gn_l(0).Add gn_t(48)
 'Crop translocation factors
  gn_t(49).Init "DRYFAC", "Dry/wet weight ratio of leafy vegetables", lbl(22), txt(22), unt(22), ref(22), "fraction", 0.05, 0.95, 0.18, 1
  gn_t(50).Init "DRYFAC", "Dry/wet weight ratio of other vegetables", lbl(23), txt(23), unt(23), ref(23), "fraction", 0.05, 0.95, 0.18, 2
  gn_t(51).Init "DRYFAC", "Dry/wet weight ratio of fruit", lbl(24), txt(24), unt(24), ref(24), "fraction", 0.05, 0.95, 0.18, 3
  gn_t(52).Init "DRYFAC", "Dry/wet weight ratio of grain", lbl(25), txt(25), unt(25), ref(25), "fraction", 0.05, 0.95, 0.88, 4
  gn_l(0).Add gn_t(49):    gn_l(0).Add gn_t(50):    gn_l(0).Add gn_t(51):    gn_l(0).Add gn_t(52)
    
  animalfeed
  
 'Intake Delays
 'Food crop intake delays  tab 0, tab 3
  gn_t(102).Init "HLDUP", "Time from harvest to ingestion of leafy vegetables", lbl(26), txt(26), unt(26), ref(26), "day", 0, 365, 1, 1
  gn_t(103).Init "HLDUP", "Time from harvest to ingestion of root vegetables", lbl(27), txt(27), unt(27), ref(27), "day", 0, 365, 5, 2
  gn_t(104).Init "HLDUP", "Time from harvest to ingestion of fruits", lbl(28), txt(28), unt(28), ref(28), "day", 0, 365, 5, 3
  gn_t(105).Init "HLDUP", "Time from harvest to ingestion of grains", lbl(29), txt(29), unt(29), ref(29), "day", 0, 365, 180, 4
  gn_l(0).Add gn_t(102):    gn_l(0).Add gn_t(103):    gn_l(0).Add gn_t(104):    gn_l(0).Add gn_t(105)
 'Food animal intake delays
  gn_t(106).Init "HLDUPA", "Time from slaughter to ingestion of meat", lbl(30), txt(30), unt(30), ref(30), "day", 0, 365, 15, 1
  gn_t(107).Init "HLDUPA", "Time from slaughter to ingestion of poultry", lbl(31), txt(31), unt(31), ref(31), "day", 0, 365, 1, 2
  gn_t(108).Init "HLDUPA", "Time from harvest to ingestion of milk", lbl(32), txt(32), unt(32), ref(32), "day", 0, 365, 1, 3
  gn_t(109).Init "HLDUPA", "Time from harvest to ingestion of eggs", lbl(33), txt(33), unt(33), ref(33), "day", 0, 365, 1, 4
  gn_l(1).Add gn_t(106):    gn_l(1).Add gn_t(107):    gn_l(1).Add gn_t(108):    gn_l(1).Add gn_t(109)

'Vegetative Production
  gn_t(5).Init "YELDBT", "Yearly vegetative yield (dry weight) for arid lands during optional buildup period", lbl(43), txt(43), unt(43), ref(43), "kg/m^2", 0.1, 20, 0.4, 1
  gn_t(10).Init "YELDBT", "Yearly vegetative yield (dry weight) for humid lands during optional buildup period", lbl(44), txt(44), unt(44), ref(44), "kg/m^2", 0.1, 20, 5#, 2
  gn_t(101).Init "YELDBT", "Yearly vegetative yield (dry weight) for agricutural lands during optional buildup period", lbl(45), txt(45), unt(45), ref(45), "kg/m^2", 0.1, 20, 4#, 3
'  gn_l(0).Add gn_t(111):    gn_l(0).Add gn_t(112):    gn_l(0).Add gn_t(113)
  
'Pathways
  'Ingestion
    gn_l(9).Init "ANF", "Meat", SSCheck1(9), cref(9), 1
    gn_l(10).Init "ANF", "Poultry", SSCheck1(10), cref(10), 2
    gn_l(11).Init "ANF", "Milk", SSCheck1(11), cref(11), 3
    gn_l(12).Init "ANF", "Eggs", SSCheck1(12), cref(12), 4
    gn_l(1).Add gn_l(9):    gn_l(1).Add gn_l(10):    gn_l(1).Add gn_l(11):    gn_l(1).Add gn_l(12)
    gn_l(13).Init "TFD", "Leafy vegetables", SSCheck1(13), cref(13), 1
    gn_l(14).Init "TFD", "Root vegetables", SSCheck1(14), cref(14), 2
    gn_l(15).Init "TFD", "Fruits", SSCheck1(15), cref(15), 3
    gn_l(16).Init "TFD", "Grains", SSCheck1(16), cref(16), 4
    gn_l(0).Add gn_l(13):   gn_l(0).Add gn_l(14):    gn_l(0).Add gn_l(15):    gn_l(0).Add gn_l(16)
    gn_l(17).Init "SLING", "Inadvertent soil", SSCheck1(17), cref(17)
  'Inhalation
    gn_l(18).Init "SLINH", "Suspended or resuspended soil", SSCheck1(18), cref(18)
  'External
    gn_l(19).Init "GROUND", "Soil external", SSCheck1(19), cref(19)
  'Dermal contact
  'No dermal as of 2/4/99   Luckily it was the last parm in the logical list
'    gn_l(20).Init "SLDRML", "Soil dermal contact", SSCheck1(20), cref(20)
    
    For i = 0 To 4
      ui.Add gn_s(i), gn_s(i).Name + Format(gn_s(i).idx(0))
    Next
    For i = 0 To 19
      If i = 8 Then i = 9
      ui.Add gn_l(i), gn_l(i).Name + Format(gn_l(i).idx(0))
    Next
    ui.Add gn_i(0), gn_i(0).Name + Format(gn_i(0).idx(0))
    For i = 0 To tsize
      If i = 25 Then i = 27
      If gn_t(i).Name <> "" Then ui.Add gn_t(i), gn_t(i).Name + Format(gn_t(i).idx(0))
    Next
    gn_s(0).ListIndex = 0
    gn_s(1).ListIndex = 0
End Sub

Private Sub Form_load()
Dim i As Long
Dim j As Long
 
  GetArguments
  If argc >= 5 Then
    Loading.Show
    loadng = True
    FUIName = argv(0) & ".GID"
    RunName = argv(1)
    siteIdx = CInt(argv(2))
    modIdx = CInt(argv(3))
    modName = argv(4)
    SetForm Me
    SetRefFile ReplaceExt(FUIName, "ref")
    SetHelpFile App.Path + "\gennfdhp.htm"
    contxt.Enabled = HelpAvailable
    rng.Enabled = HelpAvailable
    gen_nfd.Caption = "GENII Near Field Exposure Module - " + modName
    
    If open_csv(errfile, RunName & ".ERR", 1) Then
      put_val errfile, "Error report for GENII Near Field Exposure Module"
      put_line errfile
    Else
      MsgBox "Unable to create file " & RunName & ".ERR" & Chr(10) & "Check directory permessions"
      End
    End If
    
    loadng = True
    load_convert
    
    tsize = 115
    ReDim gn_i(1) As New Whole
    ReDim gn_t(tsize) As New Float
    ReDim gn_s(4) As New Selects
    ReDim gn_l(19) As New Logical
    initprm
    cb(0).ListIndex = 0
    cb(1).ListIndex = 0
    cb(2).ListIndex = 0
    loadprm
    Unload Loading
    loadng = False
    SSCheck1(2).Value = False
    frm(0).Enabled = True
    kd1.ListIndex = 0
  Else
    End
  End If
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
  Dim wflag As String
  
  kd1_Click
  flag = False
  fName = RunName & ".GID"
  frm(0).Enabled = True
  frm(3).Enabled = True
  sl(0).Enabled = True
  sl(1).Enabled = True
  sl(2).Enabled = True
  ag(0).Enabled = True
  ag(1).Enabled = True
  ag(2).Enabled = True
  ag(3).Enabled = True
  If open_parm(fle, fName, 1) Then
    For i = 0 To 1
      If Not gn_s(i).Store Then
        put_val errfile, "Bad data entry for selection flag " + gn_s(i).Name
        put_line errfile
        flag = True
      End If
    Next
    For i = 2 To 4
      If Not gn_s(i).StoreName Then
        put_val errfile, "Bad data entry for selection flag " + gn_s(i).Name
        put_line errfile
        flag = True
      End If
    Next
    For i = 0 To 19
      If i = 8 Then i = 9
      If Not gn_l(i).Store Then
        put_val errfile, "Bad data entry for logical flag " + gn_l(i).Name
        put_line errfile
        flag = True
      End If
    Next
    For i = 0 To 0
      If Not gn_i(i).Store And gn_i(i).Enabled Then
        put_val errfile, "Bad data entry for integer " + gn_i(i).Name
        put_line errfile
        flag = True
      End If
    Next
    For i = 0 To tsize
      If i = 11 Then i = 15
      If i = 25 Then i = 27
      If Not gn_t(i).Store And gn_t(i).Enabled Then
        put_val errfile, "Bad data entry for float " + gn_t(i).Name
        put_line errfile
        flag = True
      End If
    Next
    
    wflag = True
    If gn_s(3).tval = "none" Then wflag = False
    set_parm parm, "DEEP", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", wflag
    wflag = True
    If gn_s(4).tval = "none" Then wflag = False
    write_parmrec fle, parm
    set_parm parm, "BURWAS", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", wflag
    write_parmrec fle, parm
    
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
    WriteDesLabels fle
    close_parm fle
    close_csv errfile
    If Not flag Then Kill RunName & ".ERR"
  Else
    put_val errfile, "Unable to create transaction file" & RunName & ".GID"
    put_line errfile
    close_csv errfile
  End If
  End
End Sub

Sub SSCheck1_Click(index As Integer, Value As Integer)
  If loadng Then Exit Sub
  ui(uiname).Value = Value
End Sub

Private Sub cb_Click(index As Integer)
  ui(blbl(index).Tag).Click
  Select Case index
'    Case 0, 1, 2
'      ui(blbl(Index).Tag).Click
    Case 14
      ui(blbl(index).Tag).StatusListeners False
      Select Case cb(index).ListIndex
        Case 1:  gn_t(16).Enabled = True
        Case 2:  gn_t(17).Enabled = True
        Case 3:  gn_t(15).Enabled = True
      End Select
    Case 15
      If cb(index).ListIndex = 0 Then
        gn_t(6).Visible = False
        gn_t(7).Visible = False
        gn_t(8).Visible = False
        gn_t(9).Visible = False
        gn_t(6).Enabled = False
        gn_t(7).Enabled = False
        gn_t(8).Enabled = False
        gn_t(9).Enabled = False
        gn_t(11).Visible = False
        gn_t(12).Visible = False
        gn_t(13).Visible = False
        gn_t(14).Visible = False
        kd1.Visible = False
        kd2.Visible = False
      ElseIf cb(index).ListIndex = 1 Then
        gn_t(6).Visible = True
        gn_t(7).Visible = True
        gn_t(8).Visible = True
        gn_t(9).Visible = True
        gn_t(6).Enabled = True
        gn_t(7).Enabled = True
        gn_t(8).Enabled = True
        gn_t(9).Enabled = True
        gn_t(12).Visible = False
        gn_t(11).Refresh
        gn_t(12).Refresh
        gn_t(11).Visible = True
        kd1.Visible = True
        kd2.Visible = False
        ctext3 = ""
        ctext4 = ""
        If Not loadng Then
          kd1.ListIndex = -1
          kd1.ListIndex = 0
        End If
      Else
        gn_t(6).Visible = False
        gn_t(7).Visible = False
        gn_t(8).Visible = False
        gn_t(9).Visible = False
        gn_t(6).Enabled = False
        gn_t(7).Enabled = False
        gn_t(8).Enabled = False
        gn_t(9).Enabled = False
        gn_t(14).Visible = False
        gn_t(13).Refresh
        gn_t(14).Refresh
        gn_t(13).Visible = True
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

Private Sub SSTab1_Click(index As Integer, PreviousTab As Integer)
  Dim i As Long
  frm(0).Enabled = False
  frm(3).Enabled = False
  sl(0).Enabled = False
  sl(1).Enabled = False
  sl(2).Enabled = False
  ag(0).Enabled = False
  ag(1).Enabled = False
  ag(2).Enabled = False
  ag(3).Enabled = False
  
  For i = SSTab1(3).Tab * 4 + 33 To SSTab1(3).Tab * 4 + 36
    gn_t(i).Refresh
  Next
  For i = SSTab1(4).Tab * 6 + 53 To SSTab1(4).Tab * 6 + 58
'some numerical logic added BAN 26 May 2005 to make the refreshing work for soil ingestion tab
    If i >= 101 Then i = i + 9
    gn_t(i).Refresh
    If i > 101 Then i = i - 9
  Next
  
  Select Case index
  Case 0:
    Select Case SSTab1(0).Tab
    Case 0, 3:
      frm(SSTab1(0).Tab).Enabled = True
    Case 1: sl(SSTab1(1).Tab).Enabled = True
    Case 2: ag(SSTab1(2).Tab).Enabled = True
    End Select
  Case 1: sl(SSTab1(1).Tab).Enabled = True
  Case 2: ag(SSTab1(2).Tab).Enabled = True
  Case 3: ag(1).Enabled = True
  Case 4: ag(2).Enabled = True
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
  Set rfs = bref(index)
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
      If kd1.Visible Then kd2_Click
      kdchange kd1.Text, ctext3
    Case 2
      If kd1.Visible Then kd2_Click
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
