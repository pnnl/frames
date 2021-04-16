VERSION 5.00
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "tabctl32.ocx"
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "threed32.ocx"
Begin VB.Form Default 
   BorderStyle     =   1  'Fixed Single
   ClientHeight    =   4572
   ClientLeft      =   1728
   ClientTop       =   2568
   ClientWidth     =   7680
   ForeColor       =   &H80000008&
   Icon            =   "Default.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   MinButton       =   0   'False
   NegotiateMenus  =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   381
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   640
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
      TabIndex        =   201
      TabStop         =   0   'False
      Top             =   4200
      Width           =   7680
   End
   Begin TabDlg.SSTab SSTab2 
      Height          =   4215
      Left            =   0
      TabIndex        =   0
      Top             =   -15
      Width           =   7830
      _ExtentX        =   13801
      _ExtentY        =   7430
      _Version        =   393216
      Style           =   1
      Tabs            =   8
      TabsPerRow      =   4
      TabHeight       =   529
      TabCaption(0)   =   "Soil and Crop"
      TabPicture(0)   =   "Default.frx":030A
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "frm(0)"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).ControlCount=   1
      TabCaption(1)   =   "Animal Intake Rates"
      TabPicture(1)   =   "Default.frx":0326
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "frm(1)"
      Tab(1).Control(0).Enabled=   0   'False
      Tab(1).ControlCount=   1
      TabCaption(2)   =   "Translocation Factors"
      TabPicture(2)   =   "Default.frx":0342
      Tab(2).ControlEnabled=   0   'False
      Tab(2).Control(0)=   "frm(2)"
      Tab(2).ControlCount=   1
      TabCaption(3)   =   "Feed and Water Contamination"
      TabPicture(3)   =   "Default.frx":035E
      Tab(3).ControlEnabled=   0   'False
      Tab(3).Control(0)=   "frm(3)"
      Tab(3).ControlCount=   1
      TabCaption(4)   =   "Harvest Delay Times"
      TabPicture(4)   =   "Default.frx":037A
      Tab(4).ControlEnabled=   0   'False
      Tab(4).Control(0)=   "frm(4)"
      Tab(4).ControlCount=   1
      TabCaption(5)   =   "Growing Periods"
      TabPicture(5)   =   "Default.frx":0396
      Tab(5).ControlEnabled=   0   'False
      Tab(5).Control(0)=   "SSTab1"
      Tab(5).ControlCount=   1
      TabCaption(6)   =   "Resuspension"
      TabPicture(6)   =   "Default.frx":03B2
      Tab(6).ControlEnabled=   0   'False
      Tab(6).Control(0)=   "frm(6)"
      Tab(6).ControlCount=   1
      TabCaption(7)   =   "Indoor Air"
      TabPicture(7)   =   "Default.frx":03CE
      Tab(7).ControlEnabled=   0   'False
      Tab(7).Control(0)=   "frm(7)"
      Tab(7).ControlCount=   1
      Begin TabDlg.SSTab SSTab1 
         Height          =   3072
         Left            =   -74880
         TabIndex        =   93
         TabStop         =   0   'False
         Top             =   960
         Width           =   7452
         _ExtentX        =   13145
         _ExtentY        =   5419
         _Version        =   393216
         Style           =   1
         Tabs            =   5
         TabsPerRow      =   5
         TabHeight       =   529
         TabCaption(0)   =   "Ground Water"
         TabPicture(0)   =   "Default.frx":03EA
         Tab(0).ControlEnabled=   -1  'True
         Tab(0).Control(0)=   "gfrm(0)"
         Tab(0).Control(0).Enabled=   0   'False
         Tab(0).ControlCount=   1
         TabCaption(1)   =   "Not Used"
         TabPicture(1)   =   "Default.frx":0406
         Tab(1).ControlEnabled=   0   'False
         Tab(1).Control(0)=   "gfrm(1)"
         Tab(1).ControlCount=   1
         TabCaption(2)   =   "Surface Water"
         TabPicture(2)   =   "Default.frx":0422
         Tab(2).ControlEnabled=   0   'False
         Tab(2).Control(0)=   "gfrm(2)"
         Tab(2).ControlCount=   1
         TabCaption(3)   =   "Atmospheric"
         TabPicture(3)   =   "Default.frx":043E
         Tab(3).ControlEnabled=   0   'False
         Tab(3).Control(0)=   "gfrm(3)"
         Tab(3).ControlCount=   1
         TabCaption(4)   =   "Measured Soil"
         TabPicture(4)   =   "Default.frx":045A
         Tab(4).ControlEnabled=   0   'False
         Tab(4).Control(0)=   "gfrm(4)"
         Tab(4).ControlCount=   1
         Begin Threed.SSFrame gfrm 
            Height          =   2292
            Index           =   0
            Left            =   240
            TabIndex        =   110
            Tag             =   "EGTGRWLEAF"
            Top             =   540
            Width           =   6972
            _Version        =   65536
            _ExtentX        =   12298
            _ExtentY        =   4043
            _StockProps     =   14
            ForeColor       =   0
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            ShadowColor     =   1
            Font3D          =   2
            ShadowStyle     =   1
            Enabled         =   0   'False
            Begin VB.ComboBox unit 
               BackColor       =   &H00FFFFFF&
               Height          =   288
               Index           =   26
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   104
               Tag             =   "day"
               Top             =   1320
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H00C0FFC0&
               Height          =   312
               Index           =   26
               Left            =   3840
               TabIndex        =   103
               Tag             =   "tgrw"
               Text            =   "30.0"
               Top             =   1320
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               BackColor       =   &H00FFFFFF&
               Height          =   288
               Index           =   27
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   108
               Tag             =   "day"
               Top             =   1800
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H00C0FFC0&
               Height          =   312
               Index           =   27
               Left            =   3840
               TabIndex        =   107
               Tag             =   "tgrw"
               Text            =   "30.0"
               Top             =   1800
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               BackColor       =   &H00FFFFFF&
               Height          =   288
               Index           =   24
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   96
               Tag             =   "day"
               Top             =   360
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H00C0FFC0&
               Height          =   312
               Index           =   24
               Left            =   3840
               TabIndex        =   95
               Tag             =   "tgrw"
               Text            =   "60.0"
               Top             =   360
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               BackColor       =   &H00FFFFFF&
               Height          =   288
               Index           =   25
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   100
               Tag             =   "day"
               Top             =   840
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H00C0FFC0&
               Height          =   312
               Index           =   25
               Left            =   3840
               TabIndex        =   99
               Tag             =   "tgrw"
               Text            =   "60.0"
               Top             =   840
               Width           =   1000
            End
            Begin VB.Label lbl 
               Caption         =   "Other vegetable growing period -- EG-TGRWVEG"
               Height          =   380
               Index           =   25
               Left            =   120
               TabIndex        =   98
               Tag             =   "EGTGRWVEG"
               Top             =   840
               Width           =   3700
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   25
               Left            =   5880
               TabIndex        =   101
               Tag             =   "0"
               Top             =   840
               Width           =   996
            End
            Begin VB.Label lbl 
               Caption         =   "Leafy vegetable growing period -- EG-TGRWLEAF"
               Height          =   380
               Index           =   24
               Left            =   120
               TabIndex        =   94
               Tag             =   "EGTGRWLEAF"
               Top             =   360
               Width           =   3700
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   24
               Left            =   5880
               TabIndex        =   97
               Tag             =   "0"
               Top             =   360
               Width           =   996
            End
            Begin VB.Label lbl 
               Caption         =   "Milk animal feed growing period -- EG-TGRWMK"
               Height          =   380
               Index           =   27
               Left            =   120
               TabIndex        =   106
               Tag             =   "EGTGRWMK"
               Top             =   1800
               Width           =   3700
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   27
               Left            =   5880
               TabIndex        =   109
               Tag             =   "0"
               Top             =   1800
               Width           =   996
            End
            Begin VB.Label lbl 
               Caption         =   "Meat animal feed growing period -- EG-TGRWMT"
               Height          =   380
               Index           =   26
               Left            =   120
               TabIndex        =   102
               Tag             =   "EGTGRWMT"
               Top             =   1320
               Width           =   3700
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   26
               Left            =   5880
               TabIndex        =   105
               Tag             =   "0"
               Top             =   1320
               Width           =   996
            End
         End
         Begin Threed.SSFrame gfrm 
            Height          =   2292
            Index           =   1
            Left            =   -74760
            TabIndex        =   127
            Top             =   540
            Width           =   6972
            _Version        =   65536
            _ExtentX        =   12298
            _ExtentY        =   4043
            _StockProps     =   14
            ForeColor       =   0
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            ShadowColor     =   1
            Font3D          =   2
            ShadowStyle     =   1
            Enabled         =   0   'False
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H00C0FFC0&
               Height          =   312
               Index           =   29
               Left            =   3840
               TabIndex        =   116
               Tag             =   "tgrw"
               Text            =   "60.0"
               Top             =   840
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               BackColor       =   &H00FFFFFF&
               Height          =   288
               Index           =   29
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   117
               Tag             =   "day"
               Top             =   840
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H00C0FFC0&
               Height          =   312
               Index           =   28
               Left            =   3840
               TabIndex        =   112
               Tag             =   "tgrw"
               Text            =   "60.0"
               Top             =   360
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               BackColor       =   &H00FFFFFF&
               Height          =   288
               Index           =   28
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   113
               Tag             =   "day"
               Top             =   360
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H00C0FFC0&
               Height          =   312
               Index           =   31
               Left            =   3840
               TabIndex        =   124
               Tag             =   "tgrw"
               Text            =   "30.0"
               Top             =   1800
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               BackColor       =   &H00FFFFFF&
               Height          =   288
               Index           =   31
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   125
               Tag             =   "day"
               Top             =   1800
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H00C0FFC0&
               Height          =   312
               Index           =   30
               Left            =   3840
               TabIndex        =   120
               Tag             =   "tgrw"
               Text            =   "30.0"
               Top             =   1320
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               BackColor       =   &H00FFFFFF&
               Height          =   288
               Index           =   30
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   121
               Tag             =   "day"
               Top             =   1320
               Width           =   1000
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   30
               Left            =   5880
               TabIndex        =   122
               Tag             =   "0"
               Top             =   1320
               Width           =   996
            End
            Begin VB.Label lbl 
               Caption         =   "Meat animal feed growing period -- EG-TGRWMT"
               Height          =   380
               Index           =   30
               Left            =   120
               TabIndex        =   119
               Top             =   1320
               Width           =   3700
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   31
               Left            =   5880
               TabIndex        =   126
               Tag             =   "0"
               Top             =   1800
               Width           =   996
            End
            Begin VB.Label lbl 
               Caption         =   "Milk animal feed growing period -- EG-TGRWMK"
               Height          =   384
               Index           =   31
               Left            =   120
               TabIndex        =   123
               Top             =   1800
               Width           =   3696
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   28
               Left            =   5880
               TabIndex        =   114
               Tag             =   "0"
               Top             =   360
               Width           =   996
            End
            Begin VB.Label lbl 
               Caption         =   "Leafy vegetable growing period -- EG-TGRWLEAF"
               Height          =   380
               Index           =   28
               Left            =   120
               TabIndex        =   111
               Top             =   360
               Width           =   3700
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   29
               Left            =   5880
               TabIndex        =   118
               Tag             =   "0"
               Top             =   840
               Width           =   996
            End
            Begin VB.Label lbl 
               Caption         =   "Other vegetable growing period -- EG-TGRWVEG"
               Height          =   380
               Index           =   29
               Left            =   120
               TabIndex        =   115
               Top             =   840
               Width           =   3700
            End
         End
         Begin Threed.SSFrame gfrm 
            Height          =   2292
            Index           =   2
            Left            =   -74760
            TabIndex        =   144
            Top             =   540
            Width           =   6972
            _Version        =   65536
            _ExtentX        =   12298
            _ExtentY        =   4043
            _StockProps     =   14
            ForeColor       =   0
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            ShadowColor     =   1
            Font3D          =   2
            ShadowStyle     =   1
            Enabled         =   0   'False
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H00C0FFC0&
               Height          =   312
               Index           =   33
               Left            =   3840
               TabIndex        =   133
               Tag             =   "tgrw"
               Text            =   "60.0"
               Top             =   840
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               BackColor       =   &H00FFFFFF&
               Height          =   288
               Index           =   33
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   134
               Tag             =   "day"
               Top             =   840
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H00C0FFC0&
               Height          =   312
               Index           =   32
               Left            =   3840
               TabIndex        =   129
               Tag             =   "tgrw"
               Text            =   "60.0"
               Top             =   360
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               BackColor       =   &H00FFFFFF&
               Height          =   288
               Index           =   32
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   130
               Tag             =   "day"
               Top             =   360
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H00C0FFC0&
               Height          =   312
               Index           =   35
               Left            =   3840
               TabIndex        =   141
               Tag             =   "tgrw"
               Text            =   "30.0"
               Top             =   1800
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               BackColor       =   &H00FFFFFF&
               Height          =   288
               Index           =   35
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   142
               Tag             =   "day"
               Top             =   1800
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H00C0FFC0&
               Height          =   312
               Index           =   34
               Left            =   3840
               TabIndex        =   137
               Tag             =   "tgrw"
               Text            =   "30.0"
               Top             =   1320
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               BackColor       =   &H00FFFFFF&
               Height          =   288
               Index           =   34
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   138
               Tag             =   "day"
               Top             =   1320
               Width           =   1000
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   34
               Left            =   5880
               TabIndex        =   139
               Tag             =   "0"
               Top             =   1320
               Width           =   996
            End
            Begin VB.Label lbl 
               Caption         =   "Meat animal feed growing period -- EW-TGRWMT"
               Height          =   380
               Index           =   34
               Left            =   120
               TabIndex        =   136
               Tag             =   "EWTGRWMT"
               Top             =   1320
               Width           =   3700
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   35
               Left            =   5880
               TabIndex        =   143
               Tag             =   "0"
               Top             =   1800
               Width           =   996
            End
            Begin VB.Label lbl 
               Caption         =   "Milk animal feed growing period -- EW-TGRWMK"
               Height          =   380
               Index           =   35
               Left            =   120
               TabIndex        =   140
               Tag             =   "EWTGRWMK"
               Top             =   1800
               Width           =   3700
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   32
               Left            =   5880
               TabIndex        =   131
               Tag             =   "0"
               Top             =   360
               Width           =   996
            End
            Begin VB.Label lbl 
               Caption         =   "Leafy vegetable growing period -- EW-TGRWLEAF"
               Height          =   380
               Index           =   32
               Left            =   120
               TabIndex        =   128
               Tag             =   "EWTGRWLEAF"
               Top             =   360
               Width           =   3700
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   33
               Left            =   5880
               TabIndex        =   135
               Tag             =   "0"
               Top             =   840
               Width           =   996
            End
            Begin VB.Label lbl 
               Caption         =   "Other vegetable growing period -- EW-TGRWVEG"
               Height          =   380
               Index           =   33
               Left            =   120
               TabIndex        =   132
               Tag             =   "EWTGRWVEG"
               Top             =   840
               Width           =   3700
            End
         End
         Begin Threed.SSFrame gfrm 
            Height          =   2292
            Index           =   3
            Left            =   -74760
            TabIndex        =   161
            Top             =   540
            Width           =   6972
            _Version        =   65536
            _ExtentX        =   12298
            _ExtentY        =   4043
            _StockProps     =   14
            ForeColor       =   0
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            ShadowColor     =   1
            Font3D          =   2
            ShadowStyle     =   1
            Enabled         =   0   'False
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H00C0FFC0&
               Height          =   312
               Index           =   37
               Left            =   3840
               TabIndex        =   150
               Tag             =   "tgrw"
               Text            =   "60.0"
               Top             =   840
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               BackColor       =   &H00FFFFFF&
               Height          =   288
               Index           =   37
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   151
               Tag             =   "day"
               Top             =   840
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H00C0FFC0&
               Height          =   312
               Index           =   36
               Left            =   3840
               TabIndex        =   146
               Tag             =   "tgrw"
               Text            =   "60.0"
               Top             =   360
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               BackColor       =   &H00FFFFFF&
               Height          =   288
               Index           =   36
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   147
               Tag             =   "day"
               Top             =   360
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H00C0FFC0&
               Height          =   312
               Index           =   39
               Left            =   3840
               TabIndex        =   158
               Tag             =   "tgrw"
               Text            =   "30.0"
               Top             =   1800
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               BackColor       =   &H00FFFFFF&
               Height          =   288
               Index           =   39
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   159
               Tag             =   "day"
               Top             =   1800
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H00C0FFC0&
               Height          =   312
               Index           =   38
               Left            =   3840
               TabIndex        =   154
               Tag             =   "tgrw"
               Text            =   "30.0"
               Top             =   1320
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               BackColor       =   &H00FFFFFF&
               Height          =   288
               Index           =   38
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   155
               Tag             =   "day"
               Top             =   1320
               Width           =   1000
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   38
               Left            =   5880
               TabIndex        =   156
               Tag             =   "0"
               Top             =   1320
               Width           =   996
            End
            Begin VB.Label lbl 
               Caption         =   "Meat animal feed growing period -- EA-TGRWMT"
               Height          =   380
               Index           =   38
               Left            =   120
               TabIndex        =   153
               Tag             =   "EATGRWMT"
               Top             =   1320
               Width           =   3700
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   39
               Left            =   5880
               TabIndex        =   160
               Tag             =   "0"
               Top             =   1800
               Width           =   996
            End
            Begin VB.Label lbl 
               Caption         =   "Milk animal feed growing period -- EA-TGRWMK"
               Height          =   380
               Index           =   39
               Left            =   120
               TabIndex        =   157
               Tag             =   "EATGRWMK"
               Top             =   1800
               Width           =   3700
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   36
               Left            =   5880
               TabIndex        =   148
               Tag             =   "0"
               Top             =   360
               Width           =   996
            End
            Begin VB.Label lbl 
               Caption         =   "Leafy vegetable growing period -- EA-TGRWLEAF"
               Height          =   380
               Index           =   36
               Left            =   120
               TabIndex        =   145
               Tag             =   "EATGRWLEAF"
               Top             =   360
               Width           =   3700
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   37
               Left            =   5880
               TabIndex        =   152
               Tag             =   "0"
               Top             =   840
               Width           =   996
            End
            Begin VB.Label lbl 
               Caption         =   "Other vegetable growing period -- EA-TGRWVEG"
               Height          =   380
               Index           =   37
               Left            =   120
               TabIndex        =   149
               Tag             =   "EATGRWVEG"
               Top             =   840
               Width           =   3700
            End
         End
         Begin Threed.SSFrame gfrm 
            Height          =   2292
            Index           =   4
            Left            =   -74760
            TabIndex        =   178
            Top             =   540
            Width           =   6972
            _Version        =   65536
            _ExtentX        =   12298
            _ExtentY        =   4043
            _StockProps     =   14
            ForeColor       =   0
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            ShadowColor     =   1
            Font3D          =   2
            ShadowStyle     =   1
            Enabled         =   0   'False
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H00C0FFC0&
               Height          =   312
               Index           =   41
               Left            =   3840
               TabIndex        =   167
               Tag             =   "tgrw"
               Text            =   "60.0"
               Top             =   840
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               BackColor       =   &H00FFFFFF&
               Height          =   288
               Index           =   41
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   168
               Tag             =   "day"
               Top             =   840
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H00C0FFC0&
               Height          =   312
               Index           =   40
               Left            =   3840
               TabIndex        =   163
               Tag             =   "tgrw"
               Text            =   "60.0"
               Top             =   360
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               BackColor       =   &H00FFFFFF&
               Height          =   288
               Index           =   40
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   164
               Tag             =   "day"
               Top             =   360
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H00C0FFC0&
               Height          =   312
               Index           =   43
               Left            =   3840
               TabIndex        =   175
               Tag             =   "tgrw"
               Text            =   "30.0"
               Top             =   1800
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               BackColor       =   &H00FFFFFF&
               Height          =   288
               Index           =   43
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   176
               Tag             =   "day"
               Top             =   1800
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H00C0FFC0&
               Height          =   312
               Index           =   42
               Left            =   3840
               TabIndex        =   171
               Tag             =   "tgrw"
               Text            =   "30.0"
               Top             =   1320
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               BackColor       =   &H00FFFFFF&
               Height          =   288
               Index           =   42
               Left            =   4800
               Style           =   2  'Dropdown List
               TabIndex        =   172
               Tag             =   "day"
               Top             =   1320
               Width           =   1000
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   42
               Left            =   5880
               TabIndex        =   173
               Tag             =   "0"
               Top             =   1320
               Width           =   996
            End
            Begin VB.Label lbl 
               Caption         =   "Meat animal feed growing period -- EM-TGRWMT"
               Height          =   380
               Index           =   42
               Left            =   120
               TabIndex        =   170
               Tag             =   "EMTGRWMT"
               Top             =   1320
               Width           =   3700
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   43
               Left            =   5880
               TabIndex        =   177
               Tag             =   "0"
               Top             =   1800
               Width           =   996
            End
            Begin VB.Label lbl 
               Caption         =   "Milk animal feed growing period -- EM-TGRWMK"
               Height          =   380
               Index           =   43
               Left            =   120
               TabIndex        =   174
               Tag             =   "EMTGRWMK"
               Top             =   1800
               Width           =   3700
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   40
               Left            =   5880
               TabIndex        =   165
               Tag             =   "0"
               Top             =   360
               Width           =   996
            End
            Begin VB.Label lbl 
               Caption         =   "Leafy vegetable growing period -- EM-TGRWLEAF"
               Height          =   380
               Index           =   40
               Left            =   120
               TabIndex        =   162
               Tag             =   "EMTGRWLEAF"
               Top             =   360
               Width           =   3700
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Height          =   252
               Index           =   41
               Left            =   5880
               TabIndex        =   169
               Tag             =   "0"
               Top             =   840
               Width           =   996
            End
            Begin VB.Label lbl 
               Caption         =   "Other vegetable growing period -- EM-TGRWVEG"
               Height          =   380
               Index           =   41
               Left            =   120
               TabIndex        =   166
               Tag             =   "EMTGRWVEG"
               Top             =   840
               Width           =   3700
            End
         End
      End
      Begin Threed.SSFrame frm 
         Height          =   3012
         Index           =   0
         Left            =   240
         TabIndex        =   24
         Top             =   840
         Width           =   7212
         _Version        =   65536
         _ExtentX        =   12721
         _ExtentY        =   5313
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
         Enabled         =   0   'False
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   2
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   10
            Tag             =   "kg/m^2"
            Top             =   1200
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   0
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   3
            Tag             =   "kg/m^2"
            Top             =   360
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   3
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   14
            Tag             =   "kg/m^2"
            Top             =   1560
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   5
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   22
            Tag             =   "kg/m^2"
            Top             =   2400
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   4
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   18
            Tag             =   "kg/m^2"
            Top             =   2040
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   2
            Left            =   4080
            TabIndex        =   9
            Tag             =   "yld"
            Text            =   "2.0"
            Top             =   1200
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   1
            Left            =   5040
            TabIndex        =   6
            Tag             =   "ret"
            Text            =   "0.25"
            Top             =   720
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   0
            Left            =   4080
            TabIndex        =   2
            Tag             =   "den"
            Text            =   "240.0"
            Top             =   360
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   3
            Left            =   4080
            TabIndex        =   13
            Tag             =   "yld"
            Text            =   "2.0"
            Top             =   1560
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   5
            Left            =   4080
            TabIndex        =   21
            Tag             =   "yld"
            Text            =   "0.7"
            Top             =   2400
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   4
            Left            =   4080
            TabIndex        =   17
            Tag             =   "yld"
            Text            =   "0.7"
            Top             =   2040
            Width           =   1000
         End
         Begin VB.Label lbl 
            Caption         =   "Yield of other vegetable crops -- EC-YLDVEG"
            Height          =   252
            Index           =   3
            Left            =   120
            TabIndex        =   12
            Tag             =   "ECYLDVEG"
            Top             =   1560
            Width           =   3804
         End
         Begin VB.Label lbl 
            Caption         =   "Yield of leafy vegetable crops -- EC-YLDLEAF"
            Height          =   252
            Index           =   2
            Left            =   120
            TabIndex        =   8
            Tag             =   "ECYLDLEAF"
            Top             =   1200
            Width           =   3804
         End
         Begin VB.Label lbl 
            Caption         =   "Agricultural plant retension fraction of deposited activity -- EC-RET"
            Height          =   252
            Index           =   1
            Left            =   120
            TabIndex        =   5
            Tag             =   "ECRET"
            Top             =   720
            Width           =   4812
         End
         Begin VB.Label lbl 
            Caption         =   "Agricultural areal soil density -- EC-DEN"
            Height          =   252
            Index           =   0
            Left            =   120
            TabIndex        =   1
            Tag             =   "ECDEN"
            Top             =   360
            Width           =   3804
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   3
            Left            =   6120
            TabIndex        =   15
            Tag             =   "0"
            Top             =   1560
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   2
            Left            =   6120
            TabIndex        =   11
            Tag             =   "0"
            Top             =   1200
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   1
            Left            =   6120
            TabIndex        =   7
            Tag             =   "0"
            Top             =   720
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   0
            Left            =   6120
            TabIndex        =   4
            Tag             =   "0"
            Top             =   360
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   4
            Left            =   6120
            TabIndex        =   19
            Tag             =   "0"
            Top             =   2040
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   5
            Left            =   6120
            TabIndex        =   23
            Tag             =   "0"
            Top             =   2400
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Yield of meat animal feed -- EC-YLDMEAT"
            Height          =   252
            Index           =   4
            Left            =   120
            TabIndex        =   16
            Tag             =   "ECYLDMEAT"
            Top             =   2040
            Width           =   3804
         End
         Begin VB.Label lbl 
            Caption         =   "Yield of milk animal feed -- EC-YLDMILK"
            Height          =   252
            Index           =   5
            Left            =   120
            TabIndex        =   20
            Tag             =   "ECYLDMILK"
            Top             =   2400
            Width           =   3804
         End
      End
      Begin Threed.SSFrame frm 
         Height          =   3012
         Index           =   1
         Left            =   -74760
         TabIndex        =   49
         Top             =   840
         Width           =   7212
         _Version        =   65536
         _ExtentX        =   12721
         _ExtentY        =   5313
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
         Enabled         =   0   'False
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   10
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   43
            Tag             =   "kg/day"
            Top             =   2040
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   11
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   47
            Tag             =   "kg/day"
            Top             =   2400
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   7
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   31
            Tag             =   "kg/day"
            Top             =   720
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   6
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   27
            Tag             =   "kg/day"
            Top             =   360
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   8
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   35
            Tag             =   "L/day"
            Top             =   1200
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   9
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   39
            Tag             =   "L/day"
            Top             =   1560
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   11
            Left            =   4080
            TabIndex        =   46
            Tag             =   "usl"
            Text            =   "0.7"
            Top             =   2400
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   10
            Left            =   4080
            TabIndex        =   42
            Tag             =   "usl"
            Text            =   "0.8"
            Top             =   2040
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   6
            Left            =   4080
            TabIndex        =   26
            Tag             =   "umt"
            Text            =   "68.0"
            Top             =   360
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   7
            Left            =   4080
            TabIndex        =   30
            Tag             =   "umk"
            Text            =   "55.0"
            Top             =   720
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   8
            Left            =   4080
            TabIndex        =   34
            Tag             =   "wmt"
            Text            =   "50.0"
            Top             =   1200
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   9
            Left            =   4080
            TabIndex        =   38
            Tag             =   "wmk"
            Text            =   "60.0"
            Top             =   1560
            Width           =   1000
         End
         Begin VB.Label lbl 
            Caption         =   "Milk animal intake rate of soil -- EC-USLMK"
            Height          =   252
            Index           =   11
            Left            =   120
            TabIndex        =   45
            Tag             =   "ECUSLMK"
            Top             =   2400
            Width           =   3804
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   11
            Left            =   6120
            TabIndex        =   48
            Tag             =   "0"
            Top             =   2400
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Meat animal intake rate of soil -- EC-USLMT"
            Height          =   252
            Index           =   10
            Left            =   120
            TabIndex        =   41
            Tag             =   "ECUSLMT"
            Top             =   2040
            Width           =   3804
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   10
            Left            =   6120
            TabIndex        =   44
            Tag             =   "0"
            Top             =   2040
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Milk animal intake rate of water -- EC-WMK"
            Height          =   252
            Index           =   9
            Left            =   120
            TabIndex        =   37
            Tag             =   "ECWMK"
            Top             =   1560
            Width           =   3804
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   6
            Left            =   6120
            TabIndex        =   28
            Tag             =   "0"
            Top             =   360
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   7
            Left            =   6120
            TabIndex        =   32
            Tag             =   "0"
            Top             =   720
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Meat animal intake rate of feed -- EC-UMT"
            Height          =   252
            Index           =   6
            Left            =   120
            TabIndex        =   25
            Tag             =   "ECUMT"
            Top             =   360
            Width           =   3804
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   8
            Left            =   6120
            TabIndex        =   36
            Tag             =   "0"
            Top             =   1200
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Milk animal intake rate of feed -- EC-UMK"
            Height          =   252
            Index           =   7
            Left            =   120
            TabIndex        =   29
            Tag             =   "ECUMK"
            Top             =   720
            Width           =   3804
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   9
            Left            =   6120
            TabIndex        =   40
            Tag             =   "0"
            Top             =   1560
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Meat animal intake rate of water -- EC-WMT"
            Height          =   252
            Index           =   8
            Left            =   120
            TabIndex        =   33
            Tag             =   "ECWMT"
            Top             =   1200
            Width           =   3804
         End
      End
      Begin Threed.SSFrame frm 
         Height          =   2472
         Index           =   2
         Left            =   -74760
         TabIndex        =   62
         Top             =   840
         Width           =   7212
         _Version        =   65536
         _ExtentX        =   12721
         _ExtentY        =   4360
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
         Enabled         =   0   'False
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   15
            Left            =   5040
            TabIndex        =   60
            Tag             =   "trn"
            Text            =   "1.0"
            Top             =   1800
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   14
            Left            =   5040
            TabIndex        =   57
            Tag             =   "trn"
            Text            =   "0.1"
            Top             =   1320
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   13
            Left            =   5040
            TabIndex        =   54
            Tag             =   "trn"
            Text            =   "0.1"
            Top             =   840
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   12
            Left            =   5040
            TabIndex        =   51
            Tag             =   "trn"
            Text            =   "1.0"
            Top             =   360
            Width           =   1000
         End
         Begin VB.Label lbl 
            Caption         =   "Translocation factor to edible parts of milk animal feed -- EC-TRNMK"
            Height          =   380
            Index           =   15
            Left            =   120
            TabIndex        =   59
            Tag             =   "ECTRNMK"
            Top             =   1800
            Width           =   4404
         End
         Begin VB.Label lbl 
            Caption         =   "Translocation factor to edible parts of meat animal feed -- EC-TRNMT"
            Height          =   380
            Index           =   14
            Left            =   120
            TabIndex        =   56
            Tag             =   "ECTRNMT"
            Top             =   1320
            Width           =   4404
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   15
            Left            =   6120
            TabIndex        =   61
            Tag             =   "0"
            Top             =   1800
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Translocation factor to edible parts of other vegetables -- EC-TRNVEG"
            Height          =   380
            Index           =   13
            Left            =   120
            TabIndex        =   53
            Tag             =   "ECTRNVEG"
            Top             =   840
            Width           =   4404
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   14
            Left            =   6120
            TabIndex        =   58
            Tag             =   "0"
            Top             =   1320
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Translocation factor to edible parts of leafy vegetables -- EC-TRNLEAF"
            Height          =   380
            Index           =   12
            Left            =   120
            TabIndex        =   50
            Tag             =   "ECTRNLEAF"
            Top             =   360
            Width           =   4404
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   13
            Left            =   6120
            TabIndex        =   55
            Tag             =   "0"
            Top             =   840
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   12
            Left            =   6120
            TabIndex        =   52
            Tag             =   "0"
            Top             =   360
            Width           =   996
         End
      End
      Begin Threed.SSFrame frm 
         Height          =   2172
         Index           =   3
         Left            =   -74760
         TabIndex        =   75
         Top             =   840
         Width           =   7212
         _Version        =   65536
         _ExtentX        =   12721
         _ExtentY        =   3831
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
         Enabled         =   0   'False
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   19
            Left            =   5040
            TabIndex        =   73
            Tag             =   "fwc"
            Text            =   "1.0"
            Top             =   1560
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   18
            Left            =   5040
            TabIndex        =   70
            Tag             =   "fwc"
            Text            =   "1.0"
            Top             =   1200
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   17
            Left            =   5040
            TabIndex        =   67
            Tag             =   "ffc"
            Text            =   "1.0"
            Top             =   720
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   16
            Left            =   5040
            TabIndex        =   64
            Tag             =   "ffc"
            Text            =   "1.0"
            Top             =   360
            Width           =   1000
         End
         Begin VB.Label lbl 
            Caption         =   "Fraction of meat animal's water that is contaminated -- EC-FWCMT"
            Height          =   252
            Index           =   18
            Left            =   120
            TabIndex        =   69
            Tag             =   "ECFWCMT"
            Top             =   1200
            Width           =   4800
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   19
            Left            =   6120
            TabIndex        =   74
            Tag             =   "0"
            Top             =   1560
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Fraction of milk animal's feed that is contaminated -- EC-FFCMK"
            Height          =   252
            Index           =   17
            Left            =   120
            TabIndex        =   66
            Tag             =   "ECFFCMK"
            Top             =   720
            Width           =   4800
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   18
            Left            =   6120
            TabIndex        =   71
            Tag             =   "0"
            Top             =   1200
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Fraction of meat animal's feed that is contaminated -- EC-FFCMT"
            Height          =   252
            Index           =   16
            Left            =   120
            TabIndex        =   63
            Tag             =   "ECFFCMT"
            Top             =   360
            Width           =   4800
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   17
            Left            =   6120
            TabIndex        =   68
            Tag             =   "0"
            Top             =   720
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   16
            Left            =   6120
            TabIndex        =   65
            Tag             =   "0"
            Top             =   360
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Fraction of milk animal's water that is contaminated -- EC-FWCMK"
            Height          =   252
            Index           =   19
            Left            =   120
            TabIndex        =   72
            Tag             =   "ECFWCMK"
            Top             =   1560
            Width           =   4800
         End
      End
      Begin Threed.SSFrame frm 
         Height          =   2292
         Index           =   4
         Left            =   -74760
         TabIndex        =   92
         Top             =   840
         Width           =   7212
         _Version        =   65536
         _ExtentX        =   12721
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
         Enabled         =   0   'False
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   22
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   86
            Tag             =   "day"
            Top             =   1320
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   21
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   82
            Tag             =   "day"
            Top             =   840
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   20
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   78
            Tag             =   "day"
            Top             =   360
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   23
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   90
            Tag             =   "day"
            Top             =   1800
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   20
            Left            =   4080
            TabIndex        =   77
            Tag             =   "tcrp"
            Text            =   "1.0"
            Top             =   360
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   21
            Left            =   4080
            TabIndex        =   81
            Tag             =   "tcrp"
            Text            =   "60.0"
            Top             =   840
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   22
            Left            =   4080
            TabIndex        =   85
            Tag             =   "tcrp"
            Text            =   "20.0"
            Top             =   1320
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   23
            Left            =   4080
            TabIndex        =   89
            Tag             =   "tcrp"
            Text            =   "4.0"
            Top             =   1800
            Width           =   1000
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   22
            Left            =   6120
            TabIndex        =   87
            Tag             =   "0"
            Top             =   1320
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   20
            Left            =   6120
            TabIndex        =   79
            Tag             =   "0"
            Top             =   360
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   21
            Left            =   6120
            TabIndex        =   83
            Tag             =   "0"
            Top             =   840
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   23
            Left            =   6120
            TabIndex        =   91
            Tag             =   "0"
            Top             =   1800
            Width           =   996
         End
         Begin VB.Label lbl 
            Appearance      =   0  'Flat
            Caption         =   "Leafy vegetable crop ingestion harvest delay time -- EC-TCRPLEAF"
            ForeColor       =   &H80000008&
            Height          =   380
            Index           =   20
            Left            =   120
            TabIndex        =   76
            Tag             =   "ECTCRPLEAF"
            Top             =   360
            Width           =   3900
         End
         Begin VB.Label lbl 
            Appearance      =   0  'Flat
            Caption         =   "Other vegetable crop ingestion harvest delay time -- EC-TCRPVEG"
            ForeColor       =   &H80000008&
            Height          =   380
            Index           =   21
            Left            =   120
            TabIndex        =   80
            Tag             =   "ECTCRPVEG"
            Top             =   840
            Width           =   3900
         End
         Begin VB.Label lbl 
            Appearance      =   0  'Flat
            Caption         =   "Meat product ingestion harvest delay time -- EC-TCRPMT"
            ForeColor       =   &H80000008&
            Height          =   380
            Index           =   22
            Left            =   120
            TabIndex        =   84
            Tag             =   "ECTCRPMT"
            Top             =   1320
            Width           =   3900
         End
         Begin VB.Label lbl 
            Appearance      =   0  'Flat
            Caption         =   "Milk product ingestion harvest delay time -- EC-TCRPMK"
            ForeColor       =   &H80000008&
            Height          =   380
            Index           =   23
            Left            =   120
            TabIndex        =   88
            Tag             =   "ECTCRPMK"
            Top             =   1800
            Width           =   3900
         End
      End
      Begin Threed.SSFrame frm 
         Height          =   1332
         Index           =   6
         Left            =   -74760
         TabIndex        =   187
         Top             =   840
         Width           =   7212
         _Version        =   65536
         _ExtentX        =   12721
         _ExtentY        =   2350
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
         Enabled         =   0   'False
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   44
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   181
            Tag             =   "kg/m^3"
            Top             =   360
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   45
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   185
            Tag             =   "1/m"
            Top             =   840
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   45
            Left            =   4080
            TabIndex        =   184
            Tag             =   "resfac"
            Text            =   "1.0E-07"
            Top             =   840
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   44
            Left            =   4080
            TabIndex        =   180
            Tag             =   "cml"
            Text            =   "1.0E-08"
            Top             =   360
            Width           =   1000
         End
         Begin VB.Label lbl 
            Caption         =   "Airborne particulate mass loading factor -- EC- CML"
            Height          =   252
            Index           =   44
            Left            =   120
            TabIndex        =   179
            Tag             =   "ECCML"
            Top             =   360
            Width           =   3804
         End
         Begin VB.Label lbl 
            Caption         =   "Atmospheric resuspension factor -- EC-RESFAC"
            Height          =   252
            Index           =   45
            Left            =   120
            TabIndex        =   183
            Tag             =   "ECRESFAC"
            Top             =   840
            Width           =   3804
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   45
            Left            =   6120
            TabIndex        =   186
            Tag             =   "0"
            Top             =   840
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   44
            Left            =   6120
            TabIndex        =   182
            Tag             =   "0"
            Top             =   360
            Width           =   996
         End
      End
      Begin Threed.SSFrame frm 
         Height          =   1812
         Index           =   7
         Left            =   -74760
         TabIndex        =   200
         Top             =   840
         Width           =   7212
         _Version        =   65536
         _ExtentX        =   12721
         _ExtentY        =   3196
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
         Enabled         =   0   'False
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   48
            Left            =   4080
            TabIndex        =   197
            Tag             =   "andfo"
            Text            =   "0.0"
            Top             =   1320
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   48
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   198
            Tag             =   "L/m^3"
            Top             =   1320
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   46
            Left            =   4080
            TabIndex        =   189
            Tag             =   "andfc"
            Text            =   "0.5"
            Top             =   360
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   47
            Left            =   4080
            TabIndex        =   193
            Tag             =   "andfr"
            Text            =   "0.1"
            Top             =   840
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   46
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   190
            Tag             =   "L/m^3"
            Top             =   360
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   47
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   194
            Tag             =   "L/m^3"
            Top             =   840
            Width           =   1000
         End
         Begin VB.Label lbl 
            Caption         =   "Andelman inhalation factor for other pollutants -- EC-ANDFO"
            Height          =   380
            Index           =   48
            Left            =   120
            TabIndex        =   196
            Tag             =   "ECANDFO"
            Top             =   1320
            Width           =   3900
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   48
            Left            =   6120
            TabIndex        =   199
            Tag             =   "0"
            Top             =   1320
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Andelman inhalation factor for radon-222 -- EC-ANDFR"
            Height          =   380
            Index           =   47
            Left            =   120
            TabIndex        =   192
            Tag             =   "ECANDFR"
            Top             =   840
            Width           =   3900
         End
         Begin VB.Label lbl 
            Caption         =   "Andelman inhalation factor for volatile chemicals -- EC-ANDFC"
            Height          =   380
            Index           =   46
            Left            =   120
            TabIndex        =   188
            Tag             =   "ECANDFC"
            Top             =   360
            Width           =   3900
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   46
            Left            =   6120
            TabIndex        =   191
            Tag             =   "0"
            Top             =   360
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   47
            Left            =   6120
            TabIndex        =   195
            Tag             =   "0"
            Top             =   840
            Width           =   996
         End
      End
   End
   Begin VB.Menu file 
      Caption         =   "&File"
      WindowList      =   -1  'True
      Begin VB.Menu setdef 
         Caption         =   "Set &Defaults"
      End
      Begin VB.Menu save 
         Caption         =   "&Save and Exit"
      End
      Begin VB.Menu leave 
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
   Begin VB.Menu hlp 
      Caption         =   "&Help"
      Begin VB.Menu howto 
         Caption         =   "How to..."
      End
      Begin VB.Menu about 
         Caption         =   "&About"
      End
   End
End
Attribute VB_Name = "Default"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text
Dim temp As parmrec
Dim tval As Double
Dim other As Variant

Private Sub about_Click()
  frmAbout.Show 1
End Sub

Private Sub howto_Click()
  GetHelp
End Sub

Private Sub setdef_Click()
  Me.Hide
  loaddef
  Me.Show
End Sub

Private Sub Timer1_Timer()
  mes = Mid(mes, 2) & Mid(mes, 1, 1)
End Sub

Private Sub leave_Click()
  Unload Default
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

Private Sub loaddef()
  Dim i As Long
  Dim j As Long
  Dim m As Long
  Dim numref As Long
  Dim abrv As String
  Dim refer As String
  Dim name As String
  Dim post As String
  Dim pre As String
  Dim fle As csv

  On Error GoTo ErrorHandler

  If open_csv(fle, App.Path & "\mepexp.rf_", F_READ) Then
    For i = 0 To 7
      If i = 5 Then i = 6
      frm(i).Enabled = True
    Next
    For i = 0 To 4
      gfrm(i).Enabled = True
    Next
     
    'load references into reference file
    RefMode = 1
    Reference.Hide
    numref = Val(get_val(fle))
    get_line fle
    ReDim refs(numref) As Long
    For i = 1 To numref
      j = Val(get_val(fle))
      If j > UBound(refs) Then ReDim refs(j) As Long
      abrv = get_val(fle)
      refer = get_val(fle)
      refs(j) = Reference.add_ref(abrv, refer)
      get_line fle
    Next
    Unload Reference
    
    numref = Val(get_val(fle))
    get_line fle
    For j = 1 To numref
      temp.pname = get_val(fle)
      temp.ref = refs(Val(get_val(fle)))
      temp.pval = get_val(fle)
      temp.cunit = get_val(fle)
      get_line fle
      m = -1
      name = Right(temp.pname, Len(temp.pname) - 3)
      For i = 0 To 10
        If txt(other(i)).Tag = name Then
          m = other(i)
          Exit For
        End If
      Next
      If (m = -1) Then
        pre = Left(temp.pname, 2)
        post = Right(name, Len(name) - 3)
        name = Left(name, 3)
        Select Case name
          Case "tgr"
            If pre = "EG" Then m = 24
            If pre = "" Then m = 28
            If pre = "EW" Then m = 32
            If pre = "EA" Then m = 36
            If pre = "EM" Then m = 40
            If post = "wleaf" Then m = m
            If post = "wveg" Then m = m + 1
            If post = "wmt" Then m = m + 2
            If post = "wmk" Then m = m + 3
          Case "tcr"
            If post = "pleaf" Then m = 20
            If post = "pveg" Then m = 21
            If post = "pmt" Then m = 22
            If post = "pmk" Then m = 23
          Case "trn"
            If post = "leaf" Then m = 12
            If post = "veg" Then m = 13
            If post = "mt" Then m = 14
            If post = "mk" Then m = 15
          Case "yld"
            If post = "leaf" Then m = 2
            If post = "veg" Then m = 3
            If post = "meat" Then m = 4
            If post = "milk" Then m = 5
          Case "ffc"
            If post = "mt" Then m = 16
            If post = "mk" Then m = 17
          Case "fwc"
            If post = "mt" Then m = 18
            If post = "mk" Then m = 19
          Case "usl"
            If post = "mt" Then m = 10
            If post = "mk" Then m = 11
        End Select
      End If
      If (m > -1) Then
        ref(m).Tag = temp.ref
        ref(m).Caption = "Ref:" & Str(temp.ref)
        txt(m).Text = temp.pval
        If temp.cunit <> "" And temp.cunit <> "fraction" Then set_unit unit(m), temp.cunit
        er m, txt(m).Enabled
      End If
    Next
    close_csv fle
    For i = 0 To 7
      If i = 5 Then i = 6
      frm(i).Enabled = False
    Next
    For i = 0 To 4
      gfrm(i).Enabled = False
    Next
    SSTab2_Click SSTab2.Tab
  Else
    MsgBox "Can't find or open file default reference file", vbExclamation
  End If
 
ErrorHandler:
  If Err.Number <> 0 Then
    MsgBox Err.Description, vbOKOnly, "loadref"
  End If
End Sub

Private Sub loadprm()
  Dim i As Long
  Dim m As Long
  Dim fle As parmfile
  Dim fname As String
  
 
  If usedcustom1 Then
    fname = RunName & ".~ex"
  Else
    fname = FUIName
  End If
  If open_parm(fle, fname, 2) Then
    Do Until EOCF(fle.file)
      If read_parmrec(fle, temp) Then
        Select Case temp.pname
          Case ModName
            For m = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                Select Case temp.pname
                  Case "tgrw"
                    If temp.idx2 = 1 Then i = 24
                    If temp.idx2 = 2 Then i = 28
                    If temp.idx2 = 3 Then i = 32
                    If temp.idx2 = 4 Then i = 36
                    If temp.idx2 = 5 Then i = 40
                    If temp.idx1 = 1 Then fillet i
                    If temp.idx1 = 2 Then fillet i + 1
                    If temp.idx1 = 3 Then fillet i + 2
                    If temp.idx1 = 4 Then fillet i + 3
                  Case "tcrp"
                    If temp.idx1 = 1 Then fillet 20
                    If temp.idx1 = 2 Then fillet 21
                    If temp.idx1 = 3 Then fillet 22
                    If temp.idx1 = 4 Then fillet 23
                  Case "trn"
                    If temp.idx1 = 1 Then fillet 12
                    If temp.idx1 = 2 Then fillet 13
                    If temp.idx1 = 3 Then fillet 14
                    If temp.idx1 = 4 Then fillet 15
                  Case "yld"
                    If temp.idx1 = 1 Then fillet 2
                    If temp.idx1 = 2 Then fillet 3
                    If temp.idx1 = 3 Then fillet 4
                    If temp.idx1 = 4 Then fillet 5
                  Case "ffc"
                    If temp.idx1 = 1 Then fillet 16
                    If temp.idx1 = 2 Then fillet 17
                  Case "fwc"
                    If temp.idx1 = 1 Then fillet 18
                    If temp.idx1 = 2 Then fillet 19
                  Case "usl"
                    If temp.idx1 = 1 Then fillet 10
                    If temp.idx1 = 2 Then fillet 11
                  Case Else
                    For i = 0 To 10
                      If txt(other(i)).Tag = temp.pname Then
                        fillet CInt(other(i))
                        Exit For
                      End If
                    Next
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
    PutError "Can't find or open file " & fname
    Unload Default
  End If
End Sub

Sub Form_load()
  Dim cnt As Long
  Dim i As Long
  Dim fle As parmfile
  
  SSTab1.TabVisible(1) = False
'set conversion comboboxes
  For i = 0 To 48
    Select Case i
    Case 1, 12, 13, 14, 15, 16, 17, 18, 19
    Case Else
      cnt = get_conversion_items(unit(i).Tag, unit(i))
    End Select
  Next
  Default.Caption = "MEPAS Exposure Module Customization - " & ModName
  'assorted parameter indexs
  other = Array(0, 1, 6, 7, 8, 9, 44, 45, 46, 47, 48)
  
  If open_parm(fle, FUIName, 2) Then
    If FindSection(fle, ModName) = 0 Then
      loaddef
    End If
    close_parm fle
  End If
  loadprm
  SSTab2_Click 0
End Sub

Sub save_Click()
  Dim fname As String
  Dim parm As parmrec
  Dim fle As parmfile
  Dim i As Long
  Dim j As Long
  Dim UserFormat As String
  UserFormat = CVTFormat

  fname = RunName & ".~ex"
  If open_parm(fle, fname, 1) Then
    CVTFormat = "General Number"
    set_parm parm, ModName, 49, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "N/A"
    write_parmrec fle, parm
    For i = 0 To 48
      If er(i) Then PutError "Parameter " & txt(i).Tag & " is invalid"
      Select Case i
      Case 1
        set_parm parm, UCase(txt(i).Tag), 0, 0, 0, 0, 0, 0, ref(i).Tag, "N/A", "N/A", txt(i).Text
        write_parmrec fle, parm
      Case 12, 16, 18
        set_parm parm, UCase(txt(i).Tag), 1, 0, 0, 0, 0, 0, ref(i).Tag, "N/A", "N/A", txt(i).Text
        write_parmrec fle, parm
      Case 13, 17, 19
        set_parm parm, UCase(txt(i).Tag), 2, 0, 0, 0, 0, 0, ref(i).Tag, "N/A", "N/A", txt(i).Text
        write_parmrec fle, parm
      Case 14
        set_parm parm, UCase(txt(i).Tag), 3, 0, 0, 0, 0, 0, ref(i).Tag, "N/A", "N/A", txt(i).Text
        write_parmrec fle, parm
      Case 15
        set_parm parm, UCase(txt(i).Tag), 4, 0, 0, 0, 0, 0, ref(i).Tag, "N/A", "N/A", txt(i).Text
        write_parmrec fle, parm
      Case 2, 10, 20
        set_parm parm, UCase(txt(i).Tag), 1, 0, 0, 0, 0, 0, ref(i).Tag, unit(i).Text, unit(i).Tag, convert(unit(i).Text, unit(i).Tag, Val(txt(i)))
        write_parmrec fle, parm
      Case 3, 11, 21
        set_parm parm, UCase(txt(i).Tag), 2, 0, 0, 0, 0, 0, ref(i).Tag, unit(i).Text, unit(i).Tag, convert(unit(i).Text, unit(i).Tag, Val(txt(i)))
        write_parmrec fle, parm
      Case 4, 22
        set_parm parm, UCase(txt(i).Tag), 3, 0, 0, 0, 0, 0, ref(i).Tag, unit(i).Text, unit(i).Tag, convert(unit(i).Text, unit(i).Tag, Val(txt(i)))
        write_parmrec fle, parm
      Case 5, 23
        set_parm parm, UCase(txt(i).Tag), 4, 0, 0, 0, 0, 0, ref(i).Tag, unit(i).Text, unit(i).Tag, convert(unit(i).Text, unit(i).Tag, Val(txt(i)))
        write_parmrec fle, parm
      Case 24 To 43
        Select Case i
        Case 24 To 27:          j = 1
        Case 28 To 31:          j = 2
        Case 32 To 35:          j = 3
        Case 36 To 39:          j = 4
        Case 40 To 43:          j = 5
        End Select
        Select Case i
        Case 24, 28, 32, 36, 40
          set_parm parm, UCase(txt(i).Tag), 1, j, 0, 0, 0, 0, ref(i).Tag, unit(i).Text, unit(i).Tag, convert(unit(i).Text, unit(i).Tag, Val(txt(i)))
          write_parmrec fle, parm
        Case 25, 29, 33, 37, 41
          set_parm parm, UCase(txt(i).Tag), 2, j, 0, 0, 0, 0, ref(i).Tag, unit(i).Text, unit(i).Tag, convert(unit(i).Text, unit(i).Tag, Val(txt(i)))
          write_parmrec fle, parm
        Case 26, 30, 34, 38, 42
          set_parm parm, UCase(txt(i).Tag), 3, j, 0, 0, 0, 0, ref(i).Tag, unit(i).Text, unit(i).Tag, convert(unit(i).Text, unit(i).Tag, Val(txt(i)))
          write_parmrec fle, parm
        Case 27, 31, 35, 39, 43
          set_parm parm, UCase(txt(i).Tag), 4, j, 0, 0, 0, 0, ref(i).Tag, unit(i).Text, unit(i).Tag, convert(unit(i).Text, unit(i).Tag, Val(txt(i)))
          write_parmrec fle, parm
        End Select
      Case Else
        set_parm parm, UCase(txt(i).Tag), 0, 0, 0, 0, 0, 0, ref(i).Tag, unit(i).Text, unit(i).Tag, convert(unit(i).Text, unit(i).Tag, Val(txt(i)))
        write_parmrec fle, parm
      End Select
    Next
    close_parm fle
    CVTFormat = UserFormat
    usedcustom1 = True
  Else
    PutError "Can't find or open file " & fname
  End If
  Unload Default
End Sub
'
'Sub save_metadata()
'
'    putmeta UCase(txt(1).Tag), "CONTINUOUS", "N/A", lbl(1).Caption, 0, 0, 1
'
'' variables same name hard code index 12-15
'    putmeta UCase(txt(12).Tag), "CONTINUOUS", "N/A", "Tranlocation factors -- TRN", 1, 0, 1
'    putlabel "Veg/Animal #", "Index1"
'
'' variables same name hard code index 16-17
'    putmeta UCase(txt(16).Tag), "CONTINUOUS", "N/A", "Feed contamination fraction -- FFC", 1, 0, 1
'    putlabel "Animal #", "Index1"
'
'' variables same name hard code index 18-19
'    putmeta UCase(txt(18).Tag), "CONTINUOUS", "N/A", "Water contamination fraction -- FWC", 1, 0, 1
'    putlabel "Animal #", "Index1"
'
'' variables same name hard code index 2-5
'    putmeta UCase(txt(2).Tag), "CONTINUOUS", unit(2).Tag, "Feed and crop yields -- YLD", 1, 0.01, 20
'    putlabel "Yield #", "Index1"
'
'' variables same name hard code index 10-11
'    putmeta UCase(txt(10).Tag), "CONTINUOUS", unit(10).Tag, "Anaimal soil intake rates -- USL", 1, 0, 20
'    putlabel "Animal #", "Index1"
'
'' variables same name hard code index 20-23
'    putmeta UCase(txt(20).Tag), "CONTINUOUS", unit(20).Tag, "Harvest delay times -- TCRP", 1, 0, 365
'    putlabel "Veg/Animal #", "Index1"
'
'' growing period variables same name hard code index 24-43
'    putmeta UCase(txt(24).Tag), "CONTINUOUS", unit(24).Tag, "Growing period", 2, 10, 365
'    putlabel "Veg/Animal #", "Index1"
'    putlabel "Media #", "Index2"
'
'    putmeta UCase(txt(0).Tag), "CONTINUOUS", unit(0).Tag, lbl(0).Caption, 0, 1, 2000
'    putmeta UCase(txt(6).Tag), "CONTINUOUS", unit(6).Tag, lbl(6).Caption, 0, 0, 200
'    putmeta UCase(txt(7).Tag), "CONTINUOUS", unit(7).Tag, lbl(7).Caption, 0, 0, 200
'    putmeta UCase(txt(8).Tag), "CONTINUOUS", unit(8).Tag, lbl(8).Caption, 0, 0, 200
'    putmeta UCase(txt(9).Tag), "CONTINUOUS", unit(9).Tag, lbl(9).Caption, 0, 0, 200
'    putmeta UCase(txt(44).Tag), "CONTINUOUS", unit(44).Tag, lbl(44).Caption, 0, 0.000000000001, 0.001
'    putmeta UCase(txt(45).Tag), "CONTINUOUS", unit(45).Tag, lbl(45).Caption, 0, 0.000000000001, 0.01
'    putmeta UCase(txt(46).Tag), "CONTINUOUS", unit(46).Tag, lbl(46).Caption, 0, 0, 10
'    putmeta UCase(txt(47).Tag), "CONTINUOUS", unit(47).Tag, lbl(47).Caption, 0, 0, 10
'    putmeta UCase(txt(48).Tag), "CONTINUOUS", unit(48).Tag, lbl(48).Caption, 0, 0, 10
'
'End Sub

Private Function er(Index As Long, Optional enable As Boolean = True) As Boolean
  Dim tval As Double
  Dim m As String
  Dim t1 As String
  Dim t2 As String
  
  mes = ""
  er = False
  tval = Val(txt(Index).Text)
  txt(Index).BackColor = &HE0E0E0
  If txt(Index).Text = "" Then er = True
  Select Case Index
    Case 1, 12 To 19
      m = "Value must be between 0 and 1."
      If (tval < 0 Or tval > 1) Then er = True
    Case 2 To 5
      t1 = convert(unit(Index).Tag, unit(Index).Text, 0.01)
      t2 = convert(unit(Index).Tag, unit(Index).Text, 20)
      m = "Value must be between " + t1 + " and " + t2 + " " + unit(Index).Text + "(s)."
      If (tval < Val(t1) Or tval > Val(t2)) Then er = True
    Case 10, 11, 46, 47, 48
      t1 = convert(unit(Index).Tag, unit(Index).Text, 20)
      m = "Value must be between 0 and " + t1 + " " + unit(Index).Text + "(s)."
      If (tval < 0 Or tval > Val(t1)) Then er = True
    Case 6, 7
      t1 = convert(unit(Index).Tag, unit(Index).Text, 200)
      m = "Value must be between 0 and " + t1 + " " + unit(Index).Text + "(s)."
      If (tval < 0 Or tval > Val(t1)) Then er = True
    Case 8, 9
      t1 = convert(unit(Index).Tag, unit(Index).Text, 300)
      m = "Value must be between 0 and " + t1 + " " + unit(Index).Text + "(s)."
      If (tval < 0 Or tval > Val(t1)) Then er = True
    Case 20 To 23
      t1 = convert(unit(Index).Tag, unit(Index).Text, 365)
      m = "Value must be between 0 and " + t1 + " " + unit(Index).Text + "(s)."
      If (tval < 0 Or tval > Val(t1)) Then er = True
    Case 0
      t1 = convert(unit(Index).Tag, unit(Index).Text, 1)
      t2 = convert(unit(Index).Tag, unit(Index).Text, 2000)
      m = "Value must be between " + t1 + " and " + t2 + " " + unit(Index).Text + "(s)."
      If (tval < Val(t1) Or tval > Val(t2)) Then er = True
    Case 24 To 43
      t1 = convert(unit(Index).Tag, unit(Index).Text, 10)
      t2 = convert(unit(Index).Tag, unit(Index).Text, 365)
      m = "Value must be between " + t1 + " and " + t2 + " " + unit(Index).Text + "(s)."
      If (tval < Val(t1) Or tval > Val(t2)) Then er = True
    Case 44
      t1 = convert(unit(Index).Tag, unit(Index).Text, 0.0001)
      m = "Value must be between 0 and " + t1 + " " + unit(Index).Text + "(s)."
      If (tval < 0 Or tval > Val(t1)) Then er = True
    Case 45
      t1 = convert(unit(Index).Tag, unit(Index).Text, 0.0002)
      m = "Value must be between 0 and " + t1 + " " + unit(Index).Text + "(s)."
      If (tval < 0 Or tval > Val(t1)) Then er = True
  End Select
  mes = Space(140 - Len(m)) & m
  If enable Then
    If er Then
      txt(Index).BackColor = &H8080FF
    Else
      txt(Index).BackColor = &HC0FFC0
    End If
  Else
    txt(Index).BackColor = &HE0E0E0
  End If
End Function

Private Sub txt_Change(Index As Integer)
Dim chk As Double
On Error GoTo toolarge
  er CLng(Index)
  chk = CDbl(txt(Index).Text)
  Exit Sub
toolarge:
  txt(Index).BackColor = &H8080FF
End Sub

Private Sub txt_GotFocus(Index As Integer)
  er CLng(Index)
  RefItem = Index
  noact.Enabled = True
  HelpAnchor = lbl(Index).Tag
End Sub

Private Sub unit_GotFocus(Index As Integer)
  mes = ""
  RefItem = Index
  noact.Enabled = True
  HelpAnchor = lbl(Index).Tag
End Sub

Private Sub SSTab1_GotFocus()
  mes = ""
  noact.Enabled = False
  HelpAnchor = "TGRW"
End Sub

Private Sub SSTab2_GotFocus()
  mes = ""
  noact.Enabled = False
  Select Case SSTab2.Tab
  Case 0: HelpAnchor = "EC_SOIL_AND_CROP"
  Case 1: HelpAnchor = "EC_ANIMAL_INTAKE-RATES"
  Case 2: HelpAnchor = "TRANSLOCATIONE"
  Case 3: HelpAnchor = "FEED_AND_WATER"
  Case 4: HelpAnchor = "HARVEST_DELAY"
  Case 5: HelpAnchor = "GROWING_PERIODS"
  Case 6: HelpAnchor = "RESUSPENSION"
  Case 7: HelpAnchor = "INDOOR_AIR"
  End Select
End Sub

Private Sub unit_Click(Index As Integer)
  er CLng(Index)
End Sub

Private Sub SSTab1_Click(PreviousTab As Integer)
  gfrm(PreviousTab).Enabled = False
  If SSTab2.Tab = 5 Then gfrm(SSTab1.Tab).Enabled = True
End Sub

Private Sub SSTab2_Click(PreviousTab As Integer)
  If PreviousTab <> 5 Then
    frm(PreviousTab).Enabled = False
  Else
    gfrm(SSTab1.Tab).Enabled = False
  End If
  If SSTab2.Tab <> 5 Then
    frm(SSTab2.Tab).Enabled = True
  Else
    gfrm(SSTab1.Tab).Enabled = True
  End If
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


