VERSION 5.00
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "tabctl32.ocx"
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "THREED32.OCX"
Begin VB.Form gen_exp 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Form1"
   ClientHeight    =   5775
   ClientLeft      =   4110
   ClientTop       =   2850
   ClientWidth     =   7725
   Icon            =   "GEN_BIOTAEXP.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   385
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   515
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
      TabPicture(0)   =   "GEN_BIOTAEXP.frx":08CA
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "frm(0)"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).ControlCount=   1
      TabCaption(1)   =   "Water"
      TabPicture(1)   =   "GEN_BIOTAEXP.frx":08E6
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "SSTab1(1)"
      Tab(1).Control(0).Enabled=   0   'False
      Tab(1).ControlCount=   1
      TabCaption(2)   =   "Soil"
      TabPicture(2)   =   "GEN_BIOTAEXP.frx":0902
      Tab(2).ControlEnabled=   0   'False
      Tab(2).Control(0)=   "SSTab1(2)"
      Tab(2).Control(0).Enabled=   0   'False
      Tab(2).ControlCount=   1
      TabCaption(3)   =   "Air"
      TabPicture(3)   =   "GEN_BIOTAEXP.frx":091E
      Tab(3).ControlEnabled=   0   'False
      Tab(3).Control(0)=   "frm(3)"
      Tab(3).Control(0).Enabled=   0   'False
      Tab(3).ControlCount=   1
      TabCaption(4)   =   "Agriculture"
      TabPicture(4)   =   "GEN_BIOTAEXP.frx":093A
      Tab(4).ControlEnabled=   0   'False
      Tab(4).Control(0)=   "SSTab1(3)"
      Tab(4).Control(0).Enabled=   0   'False
      Tab(4).ControlCount=   1
      TabCaption(5)   =   "Biota"
      TabPicture(5)   =   "GEN_BIOTAEXP.frx":0956
      Tab(5).ControlEnabled=   0   'False
      Tab(5).Control(0)=   "frm(5)"
      Tab(5).ControlCount=   1
      Begin TabDlg.SSTab SSTab1 
         Height          =   5292
         Index           =   1
         Left            =   -74880
         TabIndex        =   50
         TabStop         =   0   'False
         Top             =   360
         Width           =   7452
         _ExtentX        =   13150
         _ExtentY        =   9340
         _Version        =   393216
         Style           =   1
         Tabs            =   2
         TabsPerRow      =   2
         TabHeight       =   423
         Enabled         =   0   'False
         TabCaption(0)   =   "General"
         TabPicture(0)   =   "GEN_BIOTAEXP.frx":0972
         Tab(0).ControlEnabled=   -1  'True
         Tab(0).Control(0)=   "wt(0)"
         Tab(0).Control(0).Enabled=   0   'False
         Tab(0).ControlCount=   1
         TabCaption(1)   =   "Animal water"
         TabPicture(1)   =   "GEN_BIOTAEXP.frx":098E
         Tab(1).ControlEnabled=   0   'False
         Tab(1).Control(0)=   "wt(1)"
         Tab(1).ControlCount=   1
         Begin Threed.SSFrame wt 
            Height          =   4692
            Index           =   0
            Left            =   240
            TabIndex        =   51
            Top             =   360
            Width           =   6972
            _Version        =   65536
            _ExtentX        =   12298
            _ExtentY        =   8276
            _StockProps     =   14
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.24
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            ShadowStyle     =   1
            Enabled         =   0   'False
            Begin VB.ComboBox unt 
               Height          =   315
               Index           =   90
               Left            =   4680
               Style           =   2  'Dropdown List
               TabIndex        =   34
               Top             =   2880
               Width           =   852
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   90
               Left            =   3840
               TabIndex        =   33
               Top             =   2880
               Width           =   852
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   66
               Left            =   3840
               TabIndex        =   31
               Top             =   1920
               Width           =   850
            End
            Begin VB.ComboBox unt 
               Height          =   315
               Index           =   66
               Left            =   4680
               Style           =   2  'Dropdown List
               TabIndex        =   32
               Top             =   1920
               Width           =   850
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   65
               Left            =   3840
               TabIndex        =   29
               Top             =   1440
               Width           =   850
            End
            Begin VB.ComboBox unt 
               Height          =   315
               Index           =   65
               Left            =   4680
               Style           =   2  'Dropdown List
               TabIndex        =   30
               Top             =   1440
               Width           =   850
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   60
               Left            =   3840
               TabIndex        =   27
               Top             =   2400
               Width           =   850
            End
            Begin VB.ComboBox unt 
               Height          =   315
               Index           =   60
               Left            =   4680
               Style           =   2  'Dropdown List
               TabIndex        =   28
               Top             =   2400
               Width           =   850
            End
            Begin Threed.SSCheck SSCheck1 
               Height          =   252
               Index           =   36
               Left            =   1080
               TabIndex        =   25
               Top             =   240
               Width           =   5172
               _Version        =   65536
               _ExtentX        =   9123
               _ExtentY        =   445
               _StockProps     =   78
               Caption         =   "SSCheck1"
               BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
                  Name            =   "MS Sans Serif"
                  Size            =   8.16
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Value           =   -1  'True
            End
            Begin Threed.SSCheck SSCheck1 
               Height          =   255
               Index           =   29
               Left            =   1080
               TabIndex        =   26
               Top             =   600
               Width           =   5175
               _Version        =   65536
               _ExtentX        =   9123
               _ExtentY        =   445
               _StockProps     =   78
               Caption         =   "SSCheck1"
               BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
                  Name            =   "MS Sans Serif"
                  Size            =   8.16
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
               Height          =   255
               Index           =   90
               Left            =   5640
               TabIndex        =   80
               Top             =   2880
               Width           =   975
            End
            Begin VB.Label lbl 
               Height          =   255
               Index           =   90
               Left            =   240
               TabIndex        =   79
               Top             =   2880
               Width           =   3615
            End
            Begin VB.Label cref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   36
               Left            =   240
               TabIndex        =   62
               Top             =   240
               Width           =   804
            End
            Begin VB.Label cref 
               Caption         =   "Ref: 0"
               Height          =   255
               Index           =   29
               Left            =   240
               TabIndex        =   61
               Top             =   600
               Width           =   810
            End
            Begin VB.Label lbl 
               Height          =   255
               Index           =   66
               Left            =   240
               TabIndex        =   57
               Top             =   1920
               Width           =   3495
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   255
               Index           =   66
               Left            =   5640
               TabIndex        =   56
               Top             =   1920
               Width           =   975
            End
            Begin VB.Label lbl 
               Height          =   255
               Index           =   65
               Left            =   240
               TabIndex        =   55
               Top             =   1440
               Width           =   3495
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   255
               Index           =   65
               Left            =   5640
               TabIndex        =   54
               Top             =   1440
               Width           =   975
            End
            Begin VB.Label lbl 
               Height          =   255
               Index           =   60
               Left            =   240
               TabIndex        =   53
               Top             =   2400
               Width           =   3615
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   255
               Index           =   60
               Left            =   5640
               TabIndex        =   52
               Top             =   2400
               Width           =   975
            End
         End
         Begin Threed.SSFrame wt 
            Height          =   4692
            Index           =   1
            Left            =   -74760
            TabIndex        =   58
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
               Height          =   315
               Index           =   6
               Left            =   4680
               Style           =   2  'Dropdown List
               TabIndex        =   223
               Top             =   4200
               Width           =   975
            End
            Begin VB.ComboBox unt 
               Height          =   315
               Index           =   5
               Left            =   4680
               Style           =   2  'Dropdown List
               TabIndex        =   221
               Top             =   3720
               Width           =   975
            End
            Begin VB.ComboBox unt 
               Height          =   315
               Index           =   4
               Left            =   4680
               Style           =   2  'Dropdown List
               TabIndex        =   219
               Top             =   3240
               Width           =   975
            End
            Begin VB.ComboBox unt 
               Height          =   315
               Index           =   3
               Left            =   4680
               Style           =   2  'Dropdown List
               TabIndex        =   217
               Top             =   2760
               Width           =   975
            End
            Begin VB.ComboBox unt 
               Height          =   315
               Index           =   107
               Left            =   4680
               Style           =   2  'Dropdown List
               TabIndex        =   18
               Top             =   360
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   315
               Index           =   108
               Left            =   4680
               Style           =   2  'Dropdown List
               TabIndex        =   20
               Top             =   840
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   315
               Index           =   109
               Left            =   4680
               Style           =   2  'Dropdown List
               TabIndex        =   22
               Top             =   1320
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   315
               Index           =   110
               Left            =   4680
               Style           =   2  'Dropdown List
               TabIndex        =   24
               Top             =   1800
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   315
               Index           =   77
               Left            =   4680
               Style           =   2  'Dropdown List
               TabIndex        =   16
               Top             =   2280
               Width           =   972
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   77
               Left            =   3840
               TabIndex        =   15
               Top             =   2280
               Width           =   852
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   110
               Left            =   3840
               TabIndex        =   23
               Top             =   1800
               Width           =   852
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   109
               Left            =   3840
               TabIndex        =   21
               Top             =   1320
               Width           =   852
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   108
               Left            =   3840
               TabIndex        =   19
               Top             =   840
               Width           =   852
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   107
               Left            =   3840
               TabIndex        =   17
               Top             =   360
               Width           =   852
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   3
               Left            =   3840
               TabIndex        =   216
               Top             =   2760
               Width           =   852
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   4
               Left            =   3840
               TabIndex        =   218
               Top             =   3240
               Width           =   852
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   5
               Left            =   3840
               TabIndex        =   220
               Top             =   3720
               Width           =   852
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   6
               Left            =   3840
               TabIndex        =   222
               Top             =   4200
               Width           =   852
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   255
               Index           =   6
               Left            =   5760
               TabIndex        =   231
               Top             =   4200
               Width           =   975
            End
            Begin VB.Label lbl 
               Height          =   255
               Index           =   6
               Left            =   360
               TabIndex        =   230
               Top             =   4200
               Width           =   3495
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   255
               Index           =   5
               Left            =   5760
               TabIndex        =   229
               Top             =   3720
               Width           =   975
            End
            Begin VB.Label lbl 
               Height          =   255
               Index           =   5
               Left            =   360
               TabIndex        =   228
               Top             =   3720
               Width           =   3495
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   255
               Index           =   4
               Left            =   5760
               TabIndex        =   227
               Top             =   3240
               Width           =   975
            End
            Begin VB.Label lbl 
               Height          =   255
               Index           =   4
               Left            =   360
               TabIndex        =   226
               Top             =   3240
               Width           =   3495
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   255
               Index           =   3
               Left            =   5760
               TabIndex        =   225
               Top             =   2760
               Width           =   975
            End
            Begin VB.Label lbl 
               Height          =   255
               Index           =   3
               Left            =   360
               TabIndex        =   224
               Top             =   2760
               Width           =   3495
            End
            Begin VB.Label lbl 
               Height          =   255
               Index           =   107
               Left            =   360
               TabIndex        =   104
               Top             =   360
               Width           =   3495
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   255
               Index           =   107
               Left            =   5760
               TabIndex        =   103
               Top             =   360
               Width           =   975
            End
            Begin VB.Label lbl 
               Height          =   255
               Index           =   108
               Left            =   360
               TabIndex        =   102
               Top             =   840
               Width           =   3495
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   255
               Index           =   108
               Left            =   5760
               TabIndex        =   101
               Top             =   840
               Width           =   975
            End
            Begin VB.Label lbl 
               Height          =   255
               Index           =   109
               Left            =   360
               TabIndex        =   100
               Top             =   1320
               Width           =   3495
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   255
               Index           =   109
               Left            =   5760
               TabIndex        =   99
               Top             =   1320
               Width           =   975
            End
            Begin VB.Label lbl 
               Height          =   255
               Index           =   110
               Left            =   360
               TabIndex        =   98
               Top             =   1800
               Width           =   3495
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   255
               Index           =   110
               Left            =   5760
               TabIndex        =   97
               Top             =   1800
               Width           =   975
            End
            Begin VB.Label lbl 
               Height          =   255
               Index           =   77
               Left            =   360
               TabIndex        =   60
               Top             =   2280
               Width           =   3495
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   255
               Index           =   77
               Left            =   5760
               TabIndex        =   59
               Top             =   2280
               Width           =   975
            End
         End
      End
      Begin TabDlg.SSTab SSTab1 
         Height          =   5292
         Index           =   2
         Left            =   -74880
         TabIndex        =   63
         TabStop         =   0   'False
         Top             =   360
         Width           =   7452
         _ExtentX        =   13150
         _ExtentY        =   9340
         _Version        =   393216
         Style           =   1
         Tabs            =   2
         Tab             =   1
         TabsPerRow      =   2
         TabHeight       =   420
         Enabled         =   0   'False
         TabCaption(0)   =   "Leaching"
         TabPicture(0)   =   "GEN_BIOTAEXP.frx":09AA
         Tab(0).ControlEnabled=   0   'False
         Tab(0).Control(0)=   "sl(0)"
         Tab(0).ControlCount=   1
         TabCaption(1)   =   "Surface Soil"
         TabPicture(1)   =   "GEN_BIOTAEXP.frx":09C6
         Tab(1).ControlEnabled=   -1  'True
         Tab(1).Control(0)=   "sl(2)"
         Tab(1).Control(0).Enabled=   0   'False
         Tab(1).ControlCount=   1
         Begin Threed.SSFrame sl 
            Height          =   4692
            Index           =   0
            Left            =   -74760
            TabIndex        =   64
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
            Begin VB.ComboBox cb 
               Height          =   315
               Index           =   15
               ItemData        =   "GEN_BIOTAEXP.frx":09E2
               Left            =   2760
               List            =   "GEN_BIOTAEXP.frx":09E4
               Style           =   2  'Dropdown List
               TabIndex        =   215
               Top             =   360
               Width           =   2892
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   91
               Left            =   1440
               Style           =   2  'Dropdown List
               TabIndex        =   11
               Top             =   3360
               Width           =   1092
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   91
               Left            =   480
               TabIndex        =   10
               Top             =   3360
               Width           =   972
            End
            Begin VB.ComboBox kd2 
               Height          =   288
               ItemData        =   "GEN_BIOTAEXP.frx":09E6
               Left            =   3840
               List            =   "GEN_BIOTAEXP.frx":09E8
               Style           =   2  'Dropdown List
               TabIndex        =   12
               Top             =   2880
               Width           =   2052
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   92
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   14
               Top             =   3360
               Width           =   1092
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   92
               Left            =   3840
               TabIndex        =   13
               Top             =   3360
               Width           =   972
            End
            Begin VB.ComboBox kd1 
               Height          =   288
               ItemData        =   "GEN_BIOTAEXP.frx":09EA
               Left            =   480
               List            =   "GEN_BIOTAEXP.frx":09EC
               Style           =   2  'Dropdown List
               TabIndex        =   9
               Top             =   2880
               Width           =   2052
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   88
               Left            =   3600
               TabIndex        =   7
               Top             =   1920
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   88
               Left            =   4560
               Style           =   2  'Dropdown List
               TabIndex        =   8
               Top             =   1920
               Width           =   1092
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   87
               Left            =   3600
               TabIndex        =   5
               Top             =   1560
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   87
               Left            =   4560
               Style           =   2  'Dropdown List
               TabIndex        =   6
               Top             =   1560
               Width           =   1092
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   86
               Left            =   3600
               TabIndex        =   3
               Top             =   1200
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   86
               Left            =   4560
               Style           =   2  'Dropdown List
               TabIndex        =   4
               Top             =   1200
               Width           =   1092
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   85
               Left            =   3600
               TabIndex        =   1
               Top             =   840
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   85
               Left            =   4560
               Style           =   2  'Dropdown List
               TabIndex        =   2
               Top             =   840
               Width           =   1092
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   88
               Left            =   240
               TabIndex        =   78
               Top             =   1920
               Width           =   3252
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   88
               Left            =   5760
               TabIndex        =   77
               Top             =   1920
               Width           =   972
            End
            Begin VB.Label lbl 
               Caption         =   "Progeny (Kd)"
               Height          =   252
               Index           =   92
               Left            =   3600
               TabIndex        =   76
               Top             =   2520
               Width           =   3252
            End
            Begin VB.Label bref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   17
               Left            =   6000
               TabIndex        =   75
               Top             =   2880
               Width           =   972
            End
            Begin VB.Label lbl 
               Caption         =   "Soil absorption coefficient (Kd)"
               Height          =   252
               Index           =   91
               Left            =   240
               TabIndex        =   73
               Top             =   2520
               Width           =   3252
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   87
               Left            =   240
               TabIndex        =   72
               Top             =   1560
               Width           =   3252
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   87
               Left            =   5760
               TabIndex        =   71
               Top             =   1560
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   86
               Left            =   240
               TabIndex        =   70
               Top             =   1200
               Width           =   3252
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   86
               Left            =   5760
               TabIndex        =   69
               Top             =   1200
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   85
               Left            =   240
               TabIndex        =   68
               Top             =   840
               Width           =   3252
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   85
               Left            =   5760
               TabIndex        =   67
               Top             =   840
               Width           =   972
            End
            Begin VB.Label bref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   15
               Left            =   5760
               TabIndex        =   66
               Top             =   360
               Width           =   972
            End
            Begin VB.Label blbl 
               Height          =   252
               Index           =   15
               Left            =   240
               TabIndex        =   65
               Top             =   360
               Width           =   2532
            End
            Begin VB.Label bref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   16
               Left            =   2640
               TabIndex        =   74
               Top             =   2880
               Width           =   972
            End
         End
         Begin Threed.SSFrame sl 
            Height          =   4695
            Index           =   2
            Left            =   240
            TabIndex        =   202
            Top             =   360
            Width           =   6975
            _Version        =   65536
            _ExtentX        =   12298
            _ExtentY        =   8276
            _StockProps     =   14
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.24
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            ShadowStyle     =   1
            Begin VB.ComboBox unt 
               Height          =   315
               Index           =   68
               Left            =   4560
               Style           =   2  'Dropdown List
               TabIndex        =   208
               Top             =   840
               Width           =   1095
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   68
               Left            =   3600
               TabIndex        =   207
               Top             =   840
               Width           =   972
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   81
               Left            =   3600
               TabIndex        =   206
               Top             =   360
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   315
               Index           =   81
               Left            =   4560
               Style           =   2  'Dropdown List
               TabIndex        =   205
               Top             =   360
               Width           =   1095
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   139
               Left            =   3600
               TabIndex        =   204
               Top             =   1320
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   315
               Index           =   139
               Left            =   4560
               Style           =   2  'Dropdown List
               TabIndex        =   203
               Top             =   1320
               Width           =   1095
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   68
               Left            =   5760
               TabIndex        =   214
               Top             =   840
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   68
               Left            =   360
               TabIndex        =   213
               Top             =   840
               Width           =   3252
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   81
               Left            =   360
               TabIndex        =   212
               Top             =   360
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
               Index           =   139
               Left            =   360
               TabIndex        =   210
               Top             =   1320
               Width           =   3252
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   139
               Left            =   5760
               TabIndex        =   209
               Top             =   1320
               Width           =   972
            End
         End
      End
      Begin Threed.SSFrame frm 
         Height          =   5172
         Index           =   0
         Left            =   240
         TabIndex        =   81
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
            TabIndex        =   200
            Top             =   4560
            Visible         =   0   'False
            Width           =   852
         End
         Begin VB.TextBox txt 
            Height          =   288
            Index           =   70
            Left            =   3960
            TabIndex        =   199
            Top             =   4560
            Visible         =   0   'False
            Width           =   852
         End
         Begin VB.ComboBox unt 
            Height          =   315
            Index           =   137
            Left            =   4800
            Style           =   2  'Dropdown List
            TabIndex        =   45
            Top             =   3120
            Width           =   852
         End
         Begin VB.ComboBox unt 
            Height          =   315
            Index           =   2
            Left            =   4800
            Style           =   2  'Dropdown List
            TabIndex        =   43
            Top             =   2640
            Width           =   850
         End
         Begin VB.TextBox txt 
            Height          =   288
            Index           =   2
            Left            =   3960
            TabIndex        =   42
            Top             =   2640
            Width           =   850
         End
         Begin VB.ComboBox unt 
            Height          =   315
            Index           =   1
            Left            =   4800
            Style           =   2  'Dropdown List
            TabIndex        =   41
            Top             =   2160
            Width           =   850
         End
         Begin VB.TextBox txt 
            Height          =   288
            Index           =   1
            Left            =   3960
            TabIndex        =   40
            Top             =   2160
            Width           =   850
         End
         Begin VB.ComboBox unt 
            Height          =   315
            Index           =   0
            Left            =   4800
            Style           =   2  'Dropdown List
            TabIndex        =   39
            Top             =   1680
            Width           =   850
         End
         Begin VB.TextBox txt 
            Height          =   288
            Index           =   0
            Left            =   3960
            TabIndex        =   38
            Top             =   1680
            Width           =   850
         End
         Begin VB.ComboBox unt 
            Height          =   315
            Index           =   82
            Left            =   4800
            Style           =   2  'Dropdown List
            TabIndex        =   47
            Top             =   3600
            Width           =   850
         End
         Begin VB.TextBox txt 
            Height          =   288
            Index           =   82
            Left            =   3960
            TabIndex        =   46
            Top             =   3600
            Width           =   850
         End
         Begin VB.ComboBox unt 
            Height          =   315
            Index           =   69
            Left            =   4800
            Style           =   2  'Dropdown List
            TabIndex        =   49
            Top             =   4080
            Width           =   852
         End
         Begin VB.TextBox txt 
            Height          =   288
            Index           =   69
            Left            =   3960
            TabIndex        =   48
            Top             =   4080
            Width           =   852
         End
         Begin Threed.SSCheck SSCheck1 
            Height          =   255
            Index           =   33
            Left            =   1080
            TabIndex        =   37
            Top             =   1080
            Width           =   2400
            _Version        =   65536
            _ExtentX        =   4233
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.16
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
         End
         Begin Threed.SSCheck SSCheck1 
            Height          =   255
            Index           =   34
            Left            =   1080
            TabIndex        =   36
            Top             =   720
            Width           =   2400
            _Version        =   65536
            _ExtentX        =   4233
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.16
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
            TabIndex        =   35
            Top             =   360
            Width           =   2400
            _Version        =   65536
            _ExtentX        =   4233
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.16
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
         End
         Begin VB.TextBox txt 
            Height          =   288
            Index           =   137
            Left            =   3960
            TabIndex        =   44
            Top             =   3120
            Width           =   852
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   70
            Left            =   5760
            TabIndex        =   201
            Top             =   4560
            Visible         =   0   'False
            Width           =   975
         End
         Begin VB.Label lbl 
            Height          =   255
            Index           =   70
            Left            =   240
            TabIndex        =   198
            Top             =   4560
            Visible         =   0   'False
            Width           =   3735
         End
         Begin VB.Label lbl 
            Height          =   255
            Index           =   137
            Left            =   240
            TabIndex        =   96
            Top             =   3120
            Width           =   3735
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   137
            Left            =   5760
            TabIndex        =   95
            Top             =   3120
            Width           =   975
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   2
            Left            =   5760
            TabIndex        =   94
            Top             =   2640
            Width           =   945
         End
         Begin VB.Label lbl 
            Height          =   255
            Index           =   2
            Left            =   240
            TabIndex        =   93
            Top             =   2640
            Width           =   3735
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   1
            Left            =   5760
            TabIndex        =   92
            Top             =   2160
            Width           =   945
         End
         Begin VB.Label lbl 
            Height          =   255
            Index           =   1
            Left            =   240
            TabIndex        =   91
            Top             =   2160
            Width           =   3735
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   0
            Left            =   5760
            TabIndex        =   90
            Top             =   1680
            Width           =   945
         End
         Begin VB.Label lbl 
            Height          =   255
            Index           =   0
            Left            =   240
            TabIndex        =   89
            Top             =   1680
            Width           =   3735
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   82
            Left            =   5760
            TabIndex        =   88
            Top             =   3600
            Width           =   945
         End
         Begin VB.Label lbl 
            Height          =   255
            Index           =   82
            Left            =   240
            TabIndex        =   87
            Top             =   3600
            Width           =   3735
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   35
            Left            =   240
            TabIndex        =   86
            Top             =   360
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   34
            Left            =   240
            TabIndex        =   85
            Top             =   720
            Width           =   810
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   33
            Left            =   240
            TabIndex        =   84
            Top             =   1080
            Width           =   810
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   69
            Left            =   5760
            TabIndex        =   83
            Top             =   4080
            Width           =   975
         End
         Begin VB.Label lbl 
            Height          =   255
            Index           =   69
            Left            =   240
            TabIndex        =   82
            Top             =   4080
            Width           =   3735
         End
      End
      Begin Threed.SSFrame frm 
         Height          =   5172
         Index           =   5
         Left            =   -74760
         TabIndex        =   105
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
            Height          =   255
            Index           =   8
            Left            =   1080
            TabIndex        =   115
            Top             =   2520
            Width           =   2505
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
            Height          =   255
            Index           =   9
            Left            =   1080
            TabIndex        =   114
            Top             =   2880
            Width           =   2505
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
            Height          =   255
            Index           =   10
            Left            =   1080
            TabIndex        =   113
            Top             =   3240
            Width           =   2505
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
            Height          =   255
            Index           =   11
            Left            =   1080
            TabIndex        =   112
            Top             =   3600
            Width           =   2505
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
            Height          =   255
            Index           =   0
            Left            =   1080
            TabIndex        =   111
            Top             =   360
            Width           =   2505
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
            Height          =   255
            Index           =   1
            Left            =   1080
            TabIndex        =   110
            Top             =   720
            Width           =   2505
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
            Height          =   255
            Index           =   2
            Left            =   1080
            TabIndex        =   109
            Top             =   1080
            Width           =   2505
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
            Height          =   255
            Index           =   3
            Left            =   1080
            TabIndex        =   108
            Top             =   1440
            Width           =   2505
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
            Height          =   255
            Index           =   4
            Left            =   1080
            TabIndex        =   107
            Top             =   1800
            Width           =   2505
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
            Height          =   255
            Index           =   5
            Left            =   1080
            TabIndex        =   106
            Top             =   2160
            Width           =   2505
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
            Height          =   255
            Index           =   11
            Left            =   240
            TabIndex        =   125
            Top             =   3600
            Width           =   810
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   10
            Left            =   240
            TabIndex        =   124
            Top             =   3240
            Width           =   810
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   9
            Left            =   240
            TabIndex        =   123
            Top             =   2880
            Width           =   810
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   8
            Left            =   240
            TabIndex        =   122
            Top             =   2520
            Width           =   810
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   5
            Left            =   240
            TabIndex        =   121
            Top             =   2160
            Width           =   810
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   4
            Left            =   240
            TabIndex        =   120
            Top             =   1800
            Width           =   810
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   3
            Left            =   240
            TabIndex        =   119
            Top             =   1440
            Width           =   810
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   2
            Left            =   240
            TabIndex        =   118
            Top             =   1080
            Width           =   810
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   1
            Left            =   240
            TabIndex        =   117
            Top             =   720
            Width           =   810
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   0
            Left            =   240
            TabIndex        =   116
            Top             =   360
            Width           =   810
         End
      End
      Begin TabDlg.SSTab SSTab1 
         Height          =   5292
         Index           =   3
         Left            =   -74880
         TabIndex        =   126
         TabStop         =   0   'False
         Top             =   360
         Width           =   7452
         _ExtentX        =   13150
         _ExtentY        =   9340
         _Version        =   393216
         Style           =   1
         Tabs            =   2
         TabsPerRow      =   2
         TabHeight       =   423
         TabCaption(0)   =   "General"
         TabPicture(0)   =   "GEN_BIOTAEXP.frx":09EE
         Tab(0).ControlEnabled=   -1  'True
         Tab(0).Control(0)=   "ag(0)"
         Tab(0).Control(0).Enabled=   0   'False
         Tab(0).ControlCount=   1
         TabCaption(1)   =   "Environmental  Parameters"
         TabPicture(1)   =   "GEN_BIOTAEXP.frx":0A0A
         Tab(1).ControlEnabled=   0   'False
         Tab(1).Control(0)=   "SSTab1(4)"
         Tab(1).Control(0).Enabled=   0   'False
         Tab(1).Control(1)=   "fd"
         Tab(1).Control(1).Enabled=   0   'False
         Tab(1).ControlCount=   2
         Begin Threed.SSFrame fd 
            Height          =   4215
            Left            =   -74700
            TabIndex        =   173
            Top             =   720
            Width           =   6735
            _Version        =   65536
            _ExtentX        =   11880
            _ExtentY        =   7435
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
               TabIndex        =   185
               Top             =   360
               Width           =   850
            End
            Begin VB.ComboBox unt 
               Height          =   315
               Index           =   101
               Left            =   4440
               Style           =   2  'Dropdown List
               TabIndex        =   184
               Top             =   360
               Width           =   850
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   102
               Left            =   3600
               TabIndex        =   183
               Top             =   810
               Width           =   850
            End
            Begin VB.ComboBox unt 
               Height          =   315
               Index           =   102
               Left            =   4440
               Style           =   2  'Dropdown List
               TabIndex        =   182
               Top             =   810
               Width           =   850
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   103
               Left            =   3600
               TabIndex        =   181
               Top             =   1275
               Width           =   850
            End
            Begin VB.ComboBox unt 
               Height          =   315
               Index           =   103
               Left            =   4440
               Style           =   2  'Dropdown List
               TabIndex        =   180
               Top             =   1275
               Width           =   850
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   104
               Left            =   3600
               TabIndex        =   179
               Top             =   1725
               Width           =   850
            End
            Begin VB.ComboBox unt 
               Height          =   315
               Index           =   104
               Left            =   4440
               Style           =   2  'Dropdown List
               TabIndex        =   178
               Top             =   1725
               Width           =   850
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   105
               Left            =   3600
               TabIndex        =   177
               Top             =   2190
               Width           =   850
            End
            Begin VB.ComboBox unt 
               Height          =   315
               Index           =   105
               Left            =   4440
               Style           =   2  'Dropdown List
               TabIndex        =   176
               Top             =   2190
               Width           =   850
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   106
               Left            =   3600
               TabIndex        =   175
               Top             =   2640
               Width           =   850
            End
            Begin VB.ComboBox unt 
               Height          =   315
               Index           =   106
               Left            =   4440
               Style           =   2  'Dropdown List
               TabIndex        =   174
               Top             =   2640
               Width           =   850
            End
            Begin VB.Label lbl 
               Height          =   465
               Index           =   101
               Left            =   135
               TabIndex        =   197
               Top             =   360
               Width           =   3255
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   255
               Index           =   101
               Left            =   5400
               TabIndex        =   196
               Top             =   360
               Width           =   945
            End
            Begin VB.Label lbl 
               Height          =   465
               Index           =   102
               Left            =   135
               TabIndex        =   195
               Top             =   810
               Width           =   3255
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   255
               Index           =   102
               Left            =   5400
               TabIndex        =   194
               Top             =   810
               Width           =   945
            End
            Begin VB.Label lbl 
               Height          =   465
               Index           =   103
               Left            =   135
               TabIndex        =   193
               Top             =   1275
               Width           =   3255
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   255
               Index           =   103
               Left            =   5400
               TabIndex        =   192
               Top             =   1275
               Width           =   945
            End
            Begin VB.Label lbl 
               Height          =   465
               Index           =   104
               Left            =   135
               TabIndex        =   191
               Top             =   1725
               Width           =   3255
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   255
               Index           =   104
               Left            =   5400
               TabIndex        =   190
               Top             =   1725
               Width           =   945
            End
            Begin VB.Label lbl 
               Height          =   465
               Index           =   105
               Left            =   135
               TabIndex        =   189
               Top             =   2190
               Width           =   3255
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   255
               Index           =   105
               Left            =   5370
               TabIndex        =   188
               Top             =   2190
               Width           =   945
            End
            Begin VB.Label lbl 
               Height          =   465
               Index           =   106
               Left            =   135
               TabIndex        =   187
               Top             =   2640
               Width           =   3255
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   255
               Index           =   106
               Left            =   5400
               TabIndex        =   186
               Top             =   2640
               Width           =   945
            End
         End
         Begin Threed.SSFrame ag 
            Height          =   4692
            Index           =   0
            Left            =   240
            TabIndex        =   127
            Top             =   360
            Width           =   6972
            _Version        =   65536
            _ExtentX        =   12298
            _ExtentY        =   8276
            _StockProps     =   14
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.24
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
               TabIndex        =   137
               Top             =   1800
               Width           =   972
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   94
               Left            =   3840
               TabIndex        =   136
               Top             =   1800
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   96
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   135
               Top             =   3120
               Width           =   972
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   96
               Left            =   3840
               TabIndex        =   134
               Top             =   3120
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   95
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   133
               Top             =   2640
               Width           =   972
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   95
               Left            =   3840
               TabIndex        =   132
               Top             =   2640
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   93
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   131
               Top             =   960
               Width           =   972
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   93
               Left            =   3840
               TabIndex        =   130
               Top             =   960
               Width           =   972
            End
            Begin VB.TextBox txt 
               Height          =   288
               Index           =   138
               Left            =   3840
               TabIndex        =   129
               Top             =   3600
               Width           =   972
            End
            Begin VB.ComboBox unt 
               Height          =   288
               Index           =   138
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   128
               Top             =   3600
               Width           =   972
            End
            Begin Threed.SSCheck SSCheck1 
               Height          =   255
               Index           =   38
               Left            =   1080
               TabIndex        =   142
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
               TabIndex        =   140
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
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   94
               Left            =   5880
               TabIndex        =   151
               Top             =   1800
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   94
               Left            =   480
               TabIndex        =   150
               Top             =   1800
               Width           =   3372
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   96
               Left            =   5880
               TabIndex        =   149
               Top             =   3120
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   96
               Left            =   240
               TabIndex        =   148
               Top             =   3120
               Width           =   3612
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   95
               Left            =   5880
               TabIndex        =   147
               Top             =   2640
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   95
               Left            =   240
               TabIndex        =   146
               Top             =   2640
               Width           =   3612
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   93
               Left            =   5880
               TabIndex        =   145
               Top             =   960
               Width           =   972
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   93
               Left            =   480
               TabIndex        =   144
               Top             =   960
               Width           =   3372
            End
            Begin VB.Label cref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   38
               Left            =   240
               TabIndex        =   143
               Top             =   1440
               Width           =   804
            End
            Begin VB.Label cref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   37
               Left            =   240
               TabIndex        =   141
               Top             =   600
               Width           =   804
            End
            Begin VB.Label lbl 
               Height          =   252
               Index           =   138
               Left            =   240
               TabIndex        =   139
               Top             =   3600
               Width           =   3612
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   138
               Left            =   5880
               TabIndex        =   138
               Top             =   3600
               Width           =   972
            End
         End
         Begin TabDlg.SSTab SSTab1 
            Height          =   4815
            Index           =   4
            Left            =   -74880
            TabIndex        =   152
            TabStop         =   0   'False
            Top             =   360
            Width           =   7215
            _ExtentX        =   12726
            _ExtentY        =   8493
            _Version        =   393216
            Style           =   1
            Tabs            =   4
            TabsPerRow      =   4
            TabHeight       =   420
            TabCaption(0)   =   "Biomass"
            TabPicture(0)   =   "GEN_BIOTAEXP.frx":0A26
            Tab(0).ControlEnabled=   -1  'True
            Tab(0).ControlCount=   0
            TabCaption(1)   =   "Consumption"
            TabPicture(1)   =   "GEN_BIOTAEXP.frx":0A42
            Tab(1).ControlEnabled=   0   'False
            Tab(1).ControlCount=   0
            TabCaption(2)   =   "Dry-Wet Ratio"
            TabPicture(2)   =   "GEN_BIOTAEXP.frx":0A5E
            Tab(2).ControlEnabled=   0   'False
            Tab(2).ControlCount=   0
            TabCaption(3)   =   "Soil Intake"
            TabPicture(3)   =   "GEN_BIOTAEXP.frx":0A7A
            Tab(3).ControlEnabled=   0   'False
            Tab(3).ControlCount=   0
         End
      End
      Begin Threed.SSFrame frm 
         Height          =   5172
         Index           =   3
         Left            =   -74760
         TabIndex        =   153
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
            TabIndex        =   169
            Top             =   3720
            Width           =   850
         End
         Begin VB.ComboBox unt 
            Height          =   288
            Index           =   146
            Left            =   4800
            Style           =   2  'Dropdown List
            TabIndex        =   168
            Top             =   3720
            Width           =   850
         End
         Begin VB.TextBox txt 
            Height          =   288
            Index           =   145
            Left            =   3960
            TabIndex        =   165
            Top             =   3360
            Width           =   850
         End
         Begin VB.ComboBox unt 
            Height          =   288
            Index           =   145
            Left            =   4800
            Style           =   2  'Dropdown List
            TabIndex        =   164
            Top             =   3360
            Width           =   850
         End
         Begin VB.TextBox txt 
            Height          =   288
            Index           =   144
            Left            =   3960
            TabIndex        =   161
            Top             =   3000
            Width           =   850
         End
         Begin VB.ComboBox unt 
            Height          =   288
            Index           =   144
            Left            =   4800
            Style           =   2  'Dropdown List
            TabIndex        =   160
            Top             =   3000
            Width           =   850
         End
         Begin Threed.SSCheck SSCheck1 
            Height          =   252
            Index           =   42
            Left            =   1080
            TabIndex        =   158
            Top             =   2520
            Width           =   5000
            _Version        =   65536
            _ExtentX        =   8827
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.16
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
            TabIndex        =   156
            Top             =   2160
            Width           =   5000
            _Version        =   65536
            _ExtentX        =   8827
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.16
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
            TabIndex        =   154
            Top             =   1800
            Width           =   5000
            _Version        =   65536
            _ExtentX        =   8827
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "SSCheck1"
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.16
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Value           =   -1  'True
         End
         Begin VB.Label Label5 
            Caption         =   $"GEN_BIOTAEXP.frx":0A96
            Height          =   1452
            Left            =   240
            TabIndex        =   172
            Top             =   360
            Width           =   6372
         End
         Begin VB.Label lbl 
            Height          =   252
            Index           =   146
            Left            =   240
            TabIndex        =   171
            Top             =   3720
            Width           =   3372
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   146
            Left            =   5760
            TabIndex        =   170
            Top             =   3720
            Width           =   948
         End
         Begin VB.Label lbl 
            Height          =   252
            Index           =   145
            Left            =   240
            TabIndex        =   167
            Top             =   3360
            Width           =   3372
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   145
            Left            =   5760
            TabIndex        =   166
            Top             =   3360
            Width           =   948
         End
         Begin VB.Label lbl 
            Height          =   252
            Index           =   144
            Left            =   240
            TabIndex        =   163
            Top             =   3000
            Width           =   3372
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   144
            Left            =   5760
            TabIndex        =   162
            Top             =   3000
            Width           =   948
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   42
            Left            =   240
            TabIndex        =   159
            Top             =   2520
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   41
            Left            =   240
            TabIndex        =   157
            Top             =   2160
            Width           =   804
         End
         Begin VB.Label cref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   40
            Left            =   240
            TabIndex        =   155
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
         Caption         =   "Add"
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

Private Sub WriteDesLabels(fle As parmfile)
Dim parm As parmrec
   set_parm parm, "ANFLabel", 1, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "terrestrial mammal"
   write_parmrec fle, parm
   set_parm parm, "ANFLabel", 2, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "terrestrial bird"
   write_parmrec fle, parm
   set_parm parm, "ANFLabel", 3, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "riparian mammal"
   write_parmrec fle, parm
   set_parm parm, "ANFLabel", 4, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "riparian bird"
   write_parmrec fle, parm
   set_parm parm, "ANFLabel", 5, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "bird egg producer"
   write_parmrec fle, parm
   set_parm parm, "TFDLabel", 1, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "terrestrial plant"
   write_parmrec fle, parm
   set_parm parm, "AQFLabel", 1, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "fish"
   write_parmrec fle, parm
   set_parm parm, "AQFLabel", 2, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "mollusca"
   write_parmrec fle, parm
   set_parm parm, "AQFLabel", 3, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "crustacea"
   write_parmrec fle, parm
   set_parm parm, "AQFLabel", 4, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "aquatic plants"
   write_parmrec fle, parm
   set_parm parm, "FdFrLabel", 1, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "terrestrial mammal forage"
   write_parmrec fle, parm
   set_parm parm, "FdFrLabel", 2, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "terrestrial bird forage"
   write_parmrec fle, parm
   set_parm parm, "FdFrLabel", 3, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "riparian mammal fish"
   write_parmrec fle, parm
   set_parm parm, "FdFrLabel", 4, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "riparian bird fish"
   write_parmrec fle, parm
   set_parm parm, "FdFrLabel", 5, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "bird egg producer forage"
   write_parmrec fle, parm
   set_parm parm, "FdFrLabel", 6, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "terrestrial plant"
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

Private Sub kd1_GotFocus()
  txt_GotFocus 91
End Sub

Private Sub kd2_GotFocus()
  txt_GotFocus 92
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
  
  gn_t(2).Init "BEFORE", "Time from start to exposure", lbl(2), txt(2), unt(2), ref(2), "yr", 0, 1000000, 0
  ReDim gn_i(2) As New Whole
  'only if connected to air
  gn_i(1).Init "BEFIRR", "Irrigation water deposition time prior to exposure", lbl(60), txt(60), unt(60), ref(60), "yr", 0, gn_t(2), 0
  gn_i(2).Init "BEFAIR", "Air deposition time prior to exposure", lbl(69), txt(69), unt(69), ref(69), "yr", 0, gn_t(2), 0
  
 
  gn_t(0).Init "NTKEND", "Duration of exposure period", lbl(0), txt(0), unt(0), ref(0), "yr", 0, 100, 1
  gn_t(1).Init "RELEND", "End of release period", lbl(1), txt(1), unt(1), ref(1), "yr", 0, 1000000, 1
  gn_t(82).Init "RAIN", "Average rain rate, when raining", lbl(82), txt(82), unt(82), ref(82), "mm/d", 0.01, 500, 1
  
  gn_l(2).Init "TFOOD", "Terrestrial Biota", SSCheck1(34), cref(34)
  'only if connected to surface water
  gn_l(1).Init "ANFOOD", "Riparian Biota", SSCheck1(35), cref(35)
  gn_l(3).Init "AQFOOD", "Aquatic Biota", SSCheck1(33), cref(33)
  
  'only if connected to air
  gn_t(3).Init "ABSHUM", "Absolute humidity, used only for tritium model", lbl(137), txt(137), unt(137), ref(137), "kg/m^3", 0, 0.1, 0.008
  
  'Feed biomass
  gn_t(103).Init "BIOMA2", "Standing biomass (wet) for terrestrial mammal forage", lbl(101), txt(101), unt(101), ref(101), "kg/m^2", 0.1, 10, 1.6, 1
  gn_t(104).Init "BIOMA2", "Standing biomass (wet) for terrestrial bird forage", lbl(102), txt(102), unt(102), ref(102), "kg/m^2", 0.1, 10, 1.6, 2
  gn_t(105).Init "BIOMA2", "Standing biomass (wet) for riparian mammal fish", lbl(103), txt(103), unt(103), ref(103), "kg/m^2", 0.1, 10, 1, 3
  gn_t(106).Init "BIOMA2", "Standing biomass (wet) for riparian bird fish", lbl(104), txt(104), unt(104), ref(104), "kg/m^2", 0.1, 10, 1.6, 4
  gn_t(107).Init "BIOMA2", "Standing biomass (wet) for bird egg producer forage", lbl(105), txt(105), unt(105), ref(105), "kg/m^2", 0.1, 10, 1, 5
  gn_t(108).Init "BIOMA2", "Standing biomass (wet) for terrstrial plants", lbl(106), txt(106), unt(106), ref(106), "kg/m^2", 0.1, 10, 1.5, 6
  gn_l(2).Add gn_t(103):    gn_l(2).Add gn_t(104):    gn_l(1).Add gn_t(105):    gn_l(1).Add gn_t(106):    gn_l(2).Add gn_t(107):    gn_l(2).Add gn_t(108)
  'Feed consumption
  gn_t(109).Init "CONSUM", "Terrestrial mammal forage consumption rate", lbl(101), txt(101), unt(101), ref(101), "kg/d", 0#, 300, 17, 1
  gn_t(110).Init "CONSUM", "Terrestrial bird forage consumption rate", lbl(102), txt(102), unt(102), ref(102), "kg/d", 0#, 5, 0.12, 2
  gn_t(111).Init "CONSUM", "Riparian mammal fish consumption rate", lbl(103), txt(103), unt(103), ref(103), "kg/d", 0#, 300, 14, 3
  gn_t(112).Init "CONSUM", "Riparian bird fish consumption rate", lbl(104), txt(104), unt(104), ref(104), "kg/d", 0#, 5, 0.12, 4
  gn_t(113).Init "CONSUM", "Bird egg producer forage consumption rate", lbl(105), txt(105), unt(105), ref(105), "kg/d", 0#, 300, 51, 5
  gn_l(2).Add gn_t(109):    gn_l(2).Add gn_t(110):    gn_l(1).Add gn_t(111):    gn_l(1).Add gn_t(112):    gn_l(2).Add gn_t(113)
  'Feed translocation factors
  gn_t(129).Init "DRYFA2", "Dry/wet weight ratio for terrestrial mammal forage", lbl(101), txt(101), unt(101), ref(101), "fraction", 0.05, 0.95, 0.88, 1
  gn_t(130).Init "DRYFA2", "Dry/wet weight ratio for terrestrial bird forage", lbl(102), txt(102), unt(102), ref(102), "fraction", 0.05, 0.95, 0.88, 2
  gn_t(131).Init "DRYFA2", "Dry/wet weight ratio for riparian mammal fish", lbl(103), txt(103), unt(103), ref(103), "fraction", 0.05, 0.95, 0.2, 3
  gn_t(132).Init "DRYFA2", "Dry/wet weight ratio for riparian bird fish", lbl(104), txt(104), unt(104), ref(104), "fraction", 0.05, 0.95, 0.2, 4
  gn_t(133).Init "DRYFA2", "Dry/wet weight ratio for bird egg producer forage", lbl(105), txt(105), unt(105), ref(105), "fraction", 0.05, 0.95, 0.88, 5
  gn_t(134).Init "DRYFA2", "Dry/wet weight ratio for terrestrial plants", lbl(106), txt(106), unt(106), ref(106), "fraction", 0.05, 0.95, 0.2, 6
  gn_l(2).Add gn_t(129):    gn_l(2).Add gn_t(130):    gn_l(1).Add gn_t(131):    gn_l(1).Add gn_t(132):    gn_l(2).Add gn_t(133):    gn_l(2).Add gn_t(134)
 'Animal soil ingestion factors
  gn_t(29).Init "SLCONA", "Soil intake rate for terrestrial mammal", lbl(101), txt(101), unt(101), ref(101), "kg/d", 0, 1, 0.1, 1
  gn_t(30).Init "SLCONA", "Soil intake rate for terrestrial bird", lbl(102), txt(102), unt(102), ref(102), "kg/d", 0, 1, 0.1, 2
  gn_t(31).Init "SLCONA", "Soil intake rate for riparian mammal", lbl(103), txt(103), unt(103), ref(103), "kg/d", 0, 1, 0.1, 3
  gn_t(32).Init "SLCONA", "Soil intake rate for riparian bird", lbl(104), txt(104), unt(104), ref(104), "kg/d", 0, 1, 0.1, 4
  gn_t(33).Init "SLCONA", "Soil intake rate for bird egg producer", lbl(105), txt(105), unt(105), ref(105), "kg/d", 0, 1, 0.1, 5
  gn_l(2).Add gn_t(29):    gn_l(2).Add gn_t(30):    gn_l(1).Add gn_t(31):    gn_l(1).Add gn_t(32):    gn_l(2).Add gn_t(33)
  
End Sub

Sub watertab()
  'Water tab 1
  'Controls tab 1, tab 0
    gn_l(36).Init "ISALT", "Aquatic foods from salt water (vesus fresh water)", SSCheck1(36), cref(36)
    gn_l(7).Init "RESIRR", "Irrigation of crops", SSCheck1(29), cref(29)
    
    gn_t(76).Init "RIRRR", "Irrigation rate for land", lbl(65), txt(65), unt(65), ref(65), "in/yr", 0, 200, 35
    gn_t(77).Init "IRTIMR", "Irrigation time for land", lbl(66), txt(66), unt(66), ref(66), "mon/yr", 0, 12, 6
    gn_l(7).Add gn_t(76):    gn_l(7).Add gn_t(77):
  
    gn_t(99).Init "DWATER", "Intake rate of water for terrestrial mammal", lbl(107), txt(107), unt(107), ref(107), "L/d", 0#, 200, 50, 1
    gn_t(100).Init "DWATER", "Intake rate of water for terrestrial bird", lbl(108), txt(108), unt(108), ref(108), "L/d", 0#, 2, 0.3, 2
    gn_t(101).Init "DWATER", "Intake rate of water for riparian mammal", lbl(109), txt(109), unt(109), ref(109), "L/d", 0#, 200, 60, 3
    gn_t(102).Init "DWATER", "Intake rate of water for riparian bird", lbl(110), txt(110), unt(110), ref(110), "L/d", 0#, 2, 0.3, 4
    gn_t(74).Init "DWATER", "Intake rate of water for bird egg producer", lbl(77), txt(77), unt(77), ref(77), "L/d", 0#, 2, 1, 5
    gn_l(2).Add gn_t(99):    gn_l(2).Add gn_t(100):    gn_l(1).Add gn_t(101):    gn_l(1).Add gn_t(102):    gn_l(2).Add gn_t(74)
    
    gn_t(114).Init "RATIME", "Water immersion time for riparian mammal", lbl(3), txt(3), unt(3), ref(3), "hr/yr", 0#, 8760, 2922, 1
    gn_t(115).Init "RATIME", "Water immersion time for riparian bird", lbl(4), txt(4), unt(4), ref(4), "hr/yr", 0#, 8760, 2922, 2
    gn_t(116).Init "RATIME", "Shoreline exposure time for riparian mammal", lbl(5), txt(5), unt(5), ref(5), "hr/yr", 0#, 8760, 8760, 3
    gn_t(117).Init "RATIME", "Shoreline exposure time for riparian bird", lbl(6), txt(6), unt(6), ref(6), "hr/yr", 0#, 8760, 8760, 4
    gn_l(1).Add gn_t(114):    gn_l(1).Add gn_t(115):    gn_l(1).Add gn_t(116):    gn_l(1).Add gn_t(117)
    
    
End Sub

Private Sub Form_load()
Dim i As Long
  
  GetArguments
  If argc = 5 Then
    Loading.Show
    loadng = True
    acute = False
    FUIName = argv(0) & ".GID"
    RunName = argv(1)
    siteIdx = CInt(argv(2))
    modIdx = CInt(argv(3))
    modName = argv(4)
    SetForm Me
    SetRefFile ReplaceExt(FUIName, "ref")
    If open_csv(errfile, RunName & ".ERR", 1) Then
      put_val errfile, "Error report for Biota Exposure Model"
      put_line errfile
    Else
      MsgBox "Unable to create file " & RunName & ".ERR" & Chr(10) & "Check directory permessions"
      End
    End If
    loadng = True
    load_convert
    SetHelpFile App.Path + "\genbiohp.htm"
    contxt.Enabled = HelpAvailable
    rng.Enabled = HelpAvailable
    lsize = 39
    tsize = 142
    gen_exp.Caption = "GENII Biota Exposure Module - " + modName
    
    'no more air tab
    SSTab1(0).TabVisible(3) = False
    
    ReDim gn_t(tsize) As New Float
    ReDim gn_l(lsize) As New Logical
  
   generaltab
   watertab
 
  'Soil tab
    cb(15).AddItem ("GENII default leach rates"):                 cb(15).ItemData(cb(15).NewIndex) = 1
    cb(15).AddItem ("Leach rates calculated from user input"):    cb(15).ItemData(cb(15).NewIndex) = 2
    cb(15).AddItem ("User provided leach rate constants"):        cb(15).ItemData(cb(15).NewIndex) = 3
    gn_s.Init "LEACHOPTION", "Type of leach rate constant", blbl(15), cb(15), bref(15), 0
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
    gn_t(80).Init "WTIM", "Weathering half time from plants", lbl(138), txt(138), unt(138), ref(138), "d", 0.1, 100, 10
  
  
  'Pathways tab 3
  'Ingestion
    gn_l(10).Init "ANF", "Terrestrial mammal (deer, ...)", SSCheck1(0), cref(0), 1
    gn_l(11).Init "ANF", "Terrestrial bird (quail, ...)", SSCheck1(1), cref(1), 2
    gn_l(12).Init "ANF", "Riparian mammal (racoon, ...)", SSCheck1(2), cref(2), 3
    gn_l(13).Init "ANF", "Riparian bird (raptor, ...)", SSCheck1(3), cref(3), 4
    gn_l(14).Init "ANF", "Bird egg producer", SSCheck1(4), cref(4), 5
    gn_l(2).Add gn_l(10):    gn_l(2).Add gn_l(11):    gn_l(1).Add gn_l(12):    gn_l(1).Add gn_l(13):    gn_l(2).Add gn_l(14)
    
    gn_l(15).Init "TFD", "Terrestrial Plant", SSCheck1(5), cref(5), 1
    gn_l(2).Add gn_l(15)
    
    gn_l(18).Init "AQF", "Fish", SSCheck1(8), cref(8), 1
    gn_l(19).Init "AQF", "Mollusca", SSCheck1(9), cref(9), 2
    gn_l(20).Init "AQF", "Crustacea", SSCheck1(10), cref(10), 3
    gn_l(21).Init "AQF", "Aquatic plants", SSCheck1(11), cref(11), 4
    gn_l(3).Add gn_l(18):    gn_l(3).Add gn_l(19):    gn_l(3).Add gn_l(20):    gn_l(3).Add gn_l(21)
   
    gn_l(37).Init "DRYSET", "User defined dry deposition interception fraction to plants", SSCheck1(37), cref(37)
    gn_l(38).Init "WETSET", "User defined wet deposition interception fraction to plants", SSCheck1(38), cref(38)
  
    gn_l(37).Add gn_t(81)
    gn_l(38).Add gn_t(94)
    
    ui.Add gn_s, gn_s.Name + Format(gn_s.idx(0))
    
    For i = 1 To lsize - 1
      If i = 4 Then i = 7
      If i = 8 Then i = 10
      If i = 16 Then i = 18
      If i = 22 Then i = 36
      ui.Add gn_l(i), gn_l(i).Name + Format(gn_l(i).idx(0))
    Next
    
    For i = 1 To 2
      ui.Add gn_i(i), gn_i(i).Name + Format(gn_i(i).idx(0))
    Next
    
    For i = 0 To tsize
      If gn_t(i).Name <> "" Then ui.Add gn_t(i), gn_t(i).Name + Format(gn_t(i).idx(0))
    Next
    
    cb(15).ListIndex = 0
    
    loadprm
    kd1.ListIndex = 0
    Unload Loading
    loadng = False
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
  Dim flag As Boolean
  
  kd1_Click
  flag = False
  fName = RunName & ".GID"
  fd.Enabled = True
  For i = 0 To 8
    If i = 0 Or i = 3 Or i = 5 Then frm(i).Enabled = True
    If i = 0 Then ag(i).Enabled = True
    If i < 2 And (haveswater Or havegwater) Then wt(i).Enabled = True
  Next
  If open_parm(fle, fName, 1) Then
    If Not gn_s.Store And gn_s.Enabled Then
      put_val errfile, "Bad data entry for selection flag " + gn_s.Name
      put_line errfile
      flag = True
    End If
    For i = 1 To lsize - 1
      If i = 4 Then i = 7
      If i = 8 Then i = 10
      If i = 16 Then i = 18
      If i = 22 Then i = 36
      If Not gn_l(i).Store And gn_l(i).Enabled Then
        put_val errfile, "Bad data entry for logical flag " + gn_l(i).Name
        put_line errfile
        flag = True
      End If
    Next
    For i = 1 To 2
      If Not gn_i(i).Store And gn_i(i).Enabled Then
        put_val errfile, "Bad data entry for integer " + gn_i(i).Name
        put_line errfile
        flag = True
      End If
    Next
    For i = 0 To tsize
      If i = 90 Then i = 94
      If gn_t(i).Name <> "" Then
        If Not gn_t(i).Store And gn_t(i).Enabled Then
          put_val errfile, "Bad data entry for float " + gn_t(i).Name
          put_line errfile
          flag = True
        End If
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
Dim enable As Boolean
  
  ui(blbl(index).Tag).Click
  Select Case index
    Case 0 To 5, 7 To 11
      enable = True
      If cb(index).ListIndex = 0 Then enable = False
        ui(blbl(index).Tag).StatusListeners enable
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

Private Sub refFive(i As Long)
gn_t(i).Refresh
gn_t(i + 1).Refresh
gn_t(i + 2).Refresh
gn_t(i + 3).Refresh
gn_t(i + 4).Refresh
gn_t(i).Visible = True
gn_t(i + 1).Visible = True
gn_t(i + 2).Visible = True
gn_t(i + 3).Visible = True
gn_t(i + 4).Visible = True
For i = 106 To 106
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
gn_t(i + 2).Visible = False
gn_t(i + 3).Visible = False
gn_t(i + 4).Visible = True
gn_t(i + 5).Visible = True
End Sub

Private Sub SSTab1_Click(index As Integer, PreviousTab As Integer)
  Select Case t1
      Case 0, 3, 5: frm(t1).Enabled = False
      Case 1: SSTab1(1).Enabled = False
                  wt(t2).Enabled = False
      Case 2: SSTab1(2).Enabled = False
      Case 4: SSTab1(3).Enabled = False
                  Select Case t2
                    Case 0, 3: ag(t2).Enabled = False
                    Case 1: SSTab1(4).Enabled = False
                            fd.Enabled = False
                    Case 2: SSTab1(5).Enabled = False
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
      Case 4: SSTab1(3).Enabled = True
                  t2 = SSTab1(3).Tab
                  fd.Visible = False
                  Select Case SSTab1(3).Tab
                    Case 0, 3: ag(SSTab1(3).Tab).Enabled = True
                    Case 1: SSTab1(4).Enabled = True
                            t3 = SSTab1(4).Tab
                            fd.Enabled = True
                            Select Case t3
                            Case 0: refSix 103
                            Case 1: refFive 109
                            Case 2: refSix 129
                            Case 3: refFive 29
                            End Select
                            fd.Visible = True
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
