VERSION 5.00
Object = "{FE0065C0-1B7B-11CF-9D53-00AA003C9CB6}#1.1#0"; "comct232.ocx"
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "tabctl32.ocx"
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "threed32.ocx"
Object = "{F856EC8B-F03C-4515-BDC6-64CBD617566A}#7.0#0"; "FPSPR70.ocx"
Begin VB.Form frmATO 
   BorderStyle     =   1  'Fixed Single
   ClientHeight    =   5760
   ClientLeft      =   7800
   ClientTop       =   4710
   ClientWidth     =   7920
   Icon            =   "Ato.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form2"
   MaxButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   384
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   528
   StartUpPosition =   2  'CenterScreen
   Begin TabDlg.SSTab SSTab1 
      Height          =   5775
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   7935
      _ExtentX        =   13996
      _ExtentY        =   10186
      _Version        =   393216
      Tabs            =   2
      TabHeight       =   423
      TabCaption(0)   =   "Constiutent Description"
      TabPicture(0)   =   "Ato.frx":030A
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "SSFrame3"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).Control(1)=   "SSFrame1"
      Tab(0).Control(1).Enabled=   0   'False
      Tab(0).ControlCount=   2
      TabCaption(1)   =   "Concentrations/ Depositions"
      TabPicture(1)   =   "Ato.frx":0326
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "SSFrame2"
      Tab(1).ControlCount=   1
      Begin Threed.SSFrame SSFrame1 
         Height          =   5265
         Left            =   240
         TabIndex        =   1
         Top             =   360
         Width           =   7455
         _Version        =   65536
         _ExtentX        =   13150
         _ExtentY        =   9287
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
         Begin Threed.SSFrame SSFrame4 
            Height          =   3825
            Left            =   5520
            TabIndex        =   37
            Top             =   1200
            Visible         =   0   'False
            Width           =   1695
            _Version        =   65536
            _ExtentX        =   2990
            _ExtentY        =   6747
            _StockProps     =   14
            Caption         =   "Starting Date"
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            ShadowStyle     =   1
            Enabled         =   0   'False
            Begin ComCtl2.UpDown UpDown1 
               Height          =   285
               Index           =   0
               Left            =   1320
               TabIndex        =   38
               Tag             =   "start"
               Top             =   1920
               Width           =   195
               _ExtentX        =   423
               _ExtentY        =   503
               _Version        =   327681
               Value           =   1900
               BuddyControl    =   "yr"
               BuddyDispid     =   196617
               OrigLeft        =   3600
               OrigTop         =   480
               OrigRight       =   3795
               OrigBottom      =   855
               Max             =   2100
               Min             =   1900
               SyncBuddy       =   -1  'True
               BuddyProperty   =   0
               Enabled         =   -1  'True
            End
            Begin ComCtl2.UpDown UpDown1 
               Height          =   285
               Index           =   1
               Left            =   1320
               TabIndex        =   39
               Tag             =   "start"
               Top             =   1320
               Width           =   195
               _ExtentX        =   423
               _ExtentY        =   503
               _Version        =   327681
               Value           =   31
               BuddyControl    =   "dy"
               BuddyDispid     =   196615
               OrigLeft        =   3600
               OrigTop         =   480
               OrigRight       =   3840
               OrigBottom      =   855
               Max             =   31
               Min             =   1
               SyncBuddy       =   -1  'True
               Wrap            =   -1  'True
               BuddyProperty   =   0
               Enabled         =   -1  'True
            End
            Begin ComCtl2.UpDown UpDown1 
               Height          =   285
               Index           =   2
               Left            =   1320
               TabIndex        =   40
               Tag             =   "start"
               Top             =   3120
               Width           =   195
               _ExtentX        =   423
               _ExtentY        =   503
               _Version        =   327681
               Value           =   59
               BuddyControl    =   "mnt"
               BuddyDispid     =   196616
               OrigLeft        =   3600
               OrigTop         =   480
               OrigRight       =   3795
               OrigBottom      =   855
               Max             =   59
               SyncBuddy       =   -1  'True
               Wrap            =   -1  'True
               BuddyProperty   =   0
               Enabled         =   -1  'True
            End
            Begin ComCtl2.UpDown UpDown1 
               Height          =   285
               Index           =   3
               Left            =   1320
               TabIndex        =   41
               Tag             =   "start"
               Top             =   2520
               Width           =   195
               _ExtentX        =   423
               _ExtentY        =   503
               _Version        =   327681
               Value           =   23
               AutoBuddy       =   -1  'True
               BuddyControl    =   "hr"
               BuddyDispid     =   196618
               OrigLeft        =   1320
               OrigTop         =   2520
               OrigRight       =   1560
               OrigBottom      =   2805
               Max             =   23
               SyncBuddy       =   -1  'True
               Wrap            =   -1  'True
               BuddyProperty   =   0
               Enabled         =   -1  'True
            End
            Begin ComCtl2.UpDown UpDown1 
               Height          =   375
               Index           =   4
               Left            =   3600
               TabIndex        =   75
               Tag             =   "start"
               Top             =   480
               Width           =   195
               _ExtentX        =   423
               _ExtentY        =   661
               _Version        =   327681
               Value           =   12
               OrigLeft        =   3600
               OrigTop         =   480
               OrigRight       =   3840
               OrigBottom      =   855
               Max             =   12
               Min             =   1
               Enabled         =   -1  'True
            End
            Begin ComCtl2.UpDown UpDown1 
               Height          =   285
               Index           =   5
               Left            =   1320
               TabIndex        =   76
               Tag             =   "start"
               Top             =   720
               Width           =   195
               _ExtentX        =   423
               _ExtentY        =   503
               _Version        =   327681
               Value           =   1
               BuddyControl    =   "mnth"
               BuddyDispid     =   196614
               OrigLeft        =   3600
               OrigTop         =   480
               OrigRight       =   3840
               OrigBottom      =   855
               Max             =   12
               Min             =   1
               SyncBuddy       =   -1  'True
               Wrap            =   -1  'True
               BuddyProperty   =   0
               Enabled         =   -1  'True
            End
            Begin VB.Label Label10 
               Caption         =   "Year"
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
               TabIndex        =   51
               Top             =   1680
               Width           =   690
            End
            Begin VB.Label Label9 
               Caption         =   "Month"
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
               TabIndex        =   50
               Top             =   480
               Width           =   690
            End
            Begin VB.Label Label8 
               Caption         =   "Day"
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
               TabIndex        =   49
               Top             =   1080
               Width           =   690
            End
            Begin VB.Label Label7 
               Caption         =   "Hour"
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
               TabIndex        =   48
               Top             =   2280
               Width           =   690
            End
            Begin VB.Label Label6 
               Caption         =   "Minute"
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
               TabIndex        =   47
               Top             =   2880
               Width           =   690
            End
            Begin VB.Label mnth 
               Alignment       =   1  'Right Justify
               BackColor       =   &H80000005&
               BorderStyle     =   1  'Fixed Single
               Height          =   285
               Left            =   360
               TabIndex        =   46
               Top             =   720
               Width           =   900
            End
            Begin VB.Label dy 
               Alignment       =   1  'Right Justify
               BackColor       =   &H80000005&
               BorderStyle     =   1  'Fixed Single
               Height          =   285
               Left            =   360
               TabIndex        =   45
               Top             =   1320
               Width           =   900
            End
            Begin VB.Label mnt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H80000005&
               BorderStyle     =   1  'Fixed Single
               Height          =   285
               Left            =   360
               TabIndex        =   44
               Top             =   3120
               Width           =   900
            End
            Begin VB.Label yr 
               Alignment       =   1  'Right Justify
               BackColor       =   &H80000005&
               BorderStyle     =   1  'Fixed Single
               Height          =   285
               Left            =   360
               TabIndex        =   43
               Top             =   1920
               Width           =   900
            End
            Begin VB.Label hr 
               Alignment       =   1  'Right Justify
               BackColor       =   &H80000005&
               BorderStyle     =   1  'Fixed Single
               Height          =   285
               Left            =   360
               TabIndex        =   42
               Top             =   2520
               Width           =   900
            End
         End
         Begin Threed.SSCommand SSCommand13 
            Height          =   765
            Left            =   5520
            TabIndex        =   2
            Tag             =   "parent"
            Top             =   240
            Width           =   1695
            _Version        =   65536
            _ExtentX        =   2990
            _ExtentY        =   1349
            _StockProps     =   78
            Caption         =   "FluxTypes"
         End
         Begin VB.ComboBox Des1 
            Height          =   288
            ItemData        =   "Ato.frx":0342
            Left            =   1260
            List            =   "Ato.frx":0344
            Style           =   2  'Dropdown List
            TabIndex        =   6
            Tag             =   "fslocate"
            Top             =   250
            Width           =   3000
         End
         Begin VB.ComboBox Des3 
            Height          =   288
            Left            =   1260
            Style           =   2  'Dropdown List
            TabIndex        =   5
            Tag             =   "parent"
            Top             =   2988
            Visible         =   0   'False
            Width           =   3000
         End
         Begin VB.ComboBox Des2 
            Height          =   288
            ItemData        =   "Ato.frx":0346
            Left            =   1275
            List            =   "Ato.frx":0348
            Style           =   2  'Dropdown List
            TabIndex        =   4
            Tag             =   "parent"
            Top             =   648
            Width           =   3000
         End
         Begin VB.ListBox prefix 
            Height          =   255
            Left            =   5640
            TabIndex        =   3
            Top             =   720
            Visible         =   0   'False
            Width           =   1215
         End
         Begin Threed.SSFrame SSFrame5 
            Height          =   1545
            Left            =   210
            TabIndex        =   7
            Top             =   1200
            Width           =   1545
            _Version        =   65536
            _ExtentX        =   2725
            _ExtentY        =   2725
            _StockProps     =   14
            Caption         =   "Flux Type"
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            ShadowStyle     =   1
            Begin VB.CheckBox Check4 
               Caption         =   "Gas 1"
               BeginProperty Font 
                  Name            =   "MS Sans Serif"
                  Size            =   8.25
                  Charset         =   0
                  Weight          =   700
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   312
               Index           =   0
               Left            =   120
               TabIndex        =   11
               Tag             =   "flux"
               Top             =   300
               Width           =   1200
            End
            Begin VB.CheckBox Check5 
               Caption         =   "Particle 1"
               BeginProperty Font 
                  Name            =   "MS Sans Serif"
                  Size            =   8.25
                  Charset         =   0
                  Weight          =   700
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   312
               Index           =   0
               Left            =   120
               TabIndex        =   10
               Tag             =   "flux"
               Top             =   600
               Width           =   1200
            End
            Begin VB.CheckBox Check6 
               Caption         =   "Particle 2"
               BeginProperty Font 
                  Name            =   "MS Sans Serif"
                  Size            =   8.25
                  Charset         =   0
                  Weight          =   700
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   312
               Index           =   0
               Left            =   120
               TabIndex        =   9
               Tag             =   "flux"
               Top             =   900
               Width           =   1200
            End
            Begin VB.CheckBox Check7 
               Caption         =   "Particle 3"
               BeginProperty Font 
                  Name            =   "MS Sans Serif"
                  Size            =   8.25
                  Charset         =   0
                  Weight          =   700
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   312
               Index           =   0
               Left            =   120
               TabIndex        =   8
               Tag             =   "flux"
               Top             =   1200
               Width           =   1200
            End
         End
         Begin Threed.SSFrame SSFrame6 
            Height          =   1545
            Left            =   210
            TabIndex        =   12
            Top             =   3450
            Visible         =   0   'False
            Width           =   1545
            _Version        =   65536
            _ExtentX        =   2725
            _ExtentY        =   2725
            _StockProps     =   14
            Caption         =   "Flux Type"
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            ShadowStyle     =   1
            Begin VB.CheckBox Check7 
               Caption         =   "Particle 3"
               BeginProperty Font 
                  Name            =   "MS Sans Serif"
                  Size            =   8.25
                  Charset         =   0
                  Weight          =   700
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   300
               Index           =   1
               Left            =   96
               TabIndex        =   16
               Tag             =   "flux"
               Top             =   1152
               Width           =   1200
            End
            Begin VB.CheckBox Check6 
               Caption         =   "Particle 2"
               BeginProperty Font 
                  Name            =   "MS Sans Serif"
                  Size            =   8.25
                  Charset         =   0
                  Weight          =   700
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   300
               Index           =   1
               Left            =   96
               TabIndex        =   15
               Tag             =   "flux"
               Top             =   852
               Width           =   1200
            End
            Begin VB.CheckBox Check5 
               Caption         =   "Particle 1"
               BeginProperty Font 
                  Name            =   "MS Sans Serif"
                  Size            =   8.25
                  Charset         =   0
                  Weight          =   700
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   300
               Index           =   1
               Left            =   96
               TabIndex        =   14
               Tag             =   "flux"
               Top             =   576
               Width           =   1200
            End
            Begin VB.CheckBox Check4 
               Caption         =   "Gas 1"
               BeginProperty Font 
                  Name            =   "MS Sans Serif"
                  Size            =   8.25
                  Charset         =   0
                  Weight          =   700
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   300
               Index           =   1
               Left            =   96
               TabIndex        =   13
               Tag             =   "flux"
               Top             =   300
               Width           =   1200
            End
         End
         Begin Threed.SSFrame SSFrame7 
            Height          =   1545
            Left            =   1920
            TabIndex        =   17
            Top             =   1200
            Width           =   3375
            _Version        =   65536
            _ExtentX        =   5948
            _ExtentY        =   2731
            _StockProps     =   14
            Caption         =   "Output Type"
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            ShadowStyle     =   1
            Begin VB.OptionButton Option2 
               Caption         =   "&Total"
               Enabled         =   0   'False
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
               Left            =   1980
               TabIndex        =   23
               Tag             =   "output"
               Top             =   1152
               Width           =   1200
            End
            Begin VB.OptionButton Option8 
               Caption         =   "&Dry"
               Enabled         =   0   'False
               BeginProperty Font 
                  Name            =   "MS Sans Serif"
                  Size            =   8.25
                  Charset         =   0
                  Weight          =   700
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   250
               Index           =   0
               Left            =   1980
               TabIndex        =   22
               Tag             =   "output"
               Top             =   870
               Width           =   1200
            End
            Begin VB.OptionButton Option1 
               Caption         =   "&Wet "
               Enabled         =   0   'False
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
               Left            =   1980
               TabIndex        =   21
               Tag             =   "output"
               Top             =   600
               Width           =   1200
            End
            Begin VB.CheckBox Check3 
               Caption         =   "Exter&nal Dose"
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
               Left            =   120
               TabIndex        =   20
               Tag             =   "output"
               Top             =   960
               Width           =   1600
            End
            Begin VB.CheckBox Check2 
               Caption         =   "De&position"
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
               Left            =   1824
               TabIndex        =   19
               Tag             =   "output"
               Top             =   288
               Width           =   1425
            End
            Begin VB.CheckBox Check1 
               Caption         =   "Air C&oncentration"
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
               Index           =   0
               Left            =   120
               TabIndex        =   18
               Tag             =   "output"
               Top             =   360
               Width           =   1600
            End
         End
         Begin Threed.SSFrame SSFrame8 
            Height          =   1548
            Left            =   1920
            TabIndex        =   24
            Top             =   3480
            Visible         =   0   'False
            Width           =   3336
            _Version        =   65536
            _ExtentX        =   5884
            _ExtentY        =   2730
            _StockProps     =   14
            Caption         =   "Output Type"
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            ShadowStyle     =   1
            Begin VB.CheckBox Check1 
               Caption         =   "Air C&oncentration"
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
               Index           =   1
               Left            =   108
               TabIndex        =   30
               Tag             =   "output"
               Top             =   360
               Width           =   1600
            End
            Begin VB.CheckBox Check2 
               Caption         =   "De&position"
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
               Index           =   1
               Left            =   1800
               TabIndex        =   29
               Tag             =   "output"
               Top             =   288
               Width           =   1425
            End
            Begin VB.CheckBox Check3 
               Caption         =   "Exter&nal Dose"
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
               Index           =   1
               Left            =   120
               TabIndex        =   28
               Tag             =   "output"
               Top             =   960
               Width           =   1600
            End
            Begin VB.OptionButton Option1 
               Caption         =   "&Wet "
               Enabled         =   0   'False
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
               Index           =   1
               Left            =   1980
               TabIndex        =   27
               TabStop         =   0   'False
               Tag             =   "output"
               Top             =   600
               Width           =   1200
            End
            Begin VB.OptionButton Option8 
               Caption         =   "&Dry"
               Enabled         =   0   'False
               BeginProperty Font 
                  Name            =   "MS Sans Serif"
                  Size            =   8.25
                  Charset         =   0
                  Weight          =   700
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   250
               Index           =   1
               Left            =   1980
               TabIndex        =   26
               Tag             =   "output"
               Top             =   864
               Width           =   1200
            End
            Begin VB.OptionButton Option2 
               Caption         =   "&Total"
               Enabled         =   0   'False
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
               Index           =   1
               Left            =   1980
               TabIndex        =   25
               Tag             =   "output"
               Top             =   1152
               Width           =   1200
            End
         End
         Begin Threed.SSCommand SSCommand10 
            Height          =   288
            Left            =   4848
            TabIndex        =   31
            Tag             =   "fslocate"
            Top             =   252
            Width           =   396
            _Version        =   65536
            _ExtentX        =   688
            _ExtentY        =   503
            _StockProps     =   78
            Caption         =   ">>"
         End
         Begin Threed.SSCommand SSCommand9 
            Height          =   288
            Left            =   4452
            TabIndex        =   32
            Tag             =   "fslocate"
            Top             =   252
            Width           =   396
            _Version        =   65536
            _ExtentX        =   688
            _ExtentY        =   503
            _StockProps     =   78
            Caption         =   "<<"
         End
         Begin Threed.SSCommand SSCommand1 
            Height          =   288
            Left            =   4452
            TabIndex        =   33
            Tag             =   "parent"
            Top             =   648
            Width           =   396
            _Version        =   65536
            _ExtentX        =   688
            _ExtentY        =   503
            _StockProps     =   78
            Caption         =   "<<"
         End
         Begin Threed.SSCommand SSCommand2 
            Height          =   288
            Left            =   4848
            TabIndex        =   34
            Tag             =   "parent"
            Top             =   648
            Width           =   396
            _Version        =   65536
            _ExtentX        =   699
            _ExtentY        =   508
            _StockProps     =   78
            Caption         =   ">>"
         End
         Begin Threed.SSCommand SSCommand3 
            Height          =   288
            Left            =   4452
            TabIndex        =   35
            Tag             =   "parent"
            Top             =   2988
            Visible         =   0   'False
            Width           =   396
            _Version        =   65536
            _ExtentX        =   699
            _ExtentY        =   508
            _StockProps     =   78
            Caption         =   "<<"
         End
         Begin Threed.SSCommand SSCommand4 
            Height          =   288
            Left            =   4848
            TabIndex        =   36
            Tag             =   "parent"
            Top             =   2988
            Visible         =   0   'False
            Width           =   396
            _Version        =   65536
            _ExtentX        =   699
            _ExtentY        =   508
            _StockProps     =   78
            Caption         =   ">>"
         End
         Begin VB.Label Label1 
            Caption         =   "Location"
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   240
            Left            =   195
            TabIndex        =   54
            Top             =   285
            Width           =   990
         End
         Begin VB.Label lbl 
            Caption         =   "Progeny"
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
            Index           =   7
            Left            =   200
            TabIndex        =   53
            Top             =   3000
            Visible         =   0   'False
            Width           =   684
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
            Index           =   8
            Left            =   195
            TabIndex        =   52
            Top             =   705
            Width           =   1050
         End
      End
      Begin Threed.SSFrame SSFrame2 
         Height          =   5265
         Left            =   -74760
         TabIndex        =   55
         Top             =   360
         Width           =   7455
         _Version        =   65536
         _ExtentX        =   13150
         _ExtentY        =   9287
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
         Begin VB.ComboBox Conc1 
            Height          =   288
            ItemData        =   "Ato.frx":034A
            Left            =   1260
            List            =   "Ato.frx":034C
            Style           =   2  'Dropdown List
            TabIndex        =   59
            Tag             =   "concentrationsLoc"
            Top             =   252
            Width           =   3000
         End
         Begin VB.ComboBox Conc2 
            Height          =   288
            Left            =   1260
            Style           =   2  'Dropdown List
            TabIndex        =   58
            Tag             =   "concentrationsParent"
            Top             =   648
            Width           =   3000
         End
         Begin VB.ComboBox Conc3 
            Height          =   288
            Left            =   1260
            Style           =   2  'Dropdown List
            TabIndex        =   57
            Tag             =   "concentrationsParent"
            Top             =   2988
            Visible         =   0   'False
            Width           =   3000
         End
         Begin FPSpreadADO.fpSpread vaSpread1 
            Height          =   1635
            Left            =   240
            TabIndex        =   56
            Tag             =   "concentrationsOutput"
            Top             =   1080
            Visible         =   0   'False
            Width           =   7020
            _Version        =   458752
            _ExtentX        =   12382
            _ExtentY        =   2884
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
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            FormulaSync     =   0   'False
            MaxCols         =   16
            MaxRows         =   5000
            RowsFrozen      =   1
            ScrollBarExtMode=   -1  'True
            ScrollBarShowMax=   0   'False
            SpreadDesigner  =   "Ato.frx":034E
            StartingColNumber=   0
            StartingRowNumber=   0
            VisibleCols     =   6
            VisibleRows     =   5
         End
         Begin FPSpreadADO.fpSpread vaSpread2 
            Height          =   1680
            Left            =   240
            TabIndex        =   60
            Tag             =   "concentrationsOutput"
            Top             =   3360
            Visible         =   0   'False
            Width           =   7065
            _Version        =   458752
            _ExtentX        =   12462
            _ExtentY        =   2963
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
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            FormulaSync     =   0   'False
            MaxCols         =   16
            MaxRows         =   5000
            RowsFrozen      =   1
            ScrollBarExtMode=   -1  'True
            ScrollBarShowMax=   0   'False
            SpreadDesigner  =   "Ato.frx":0A11
            StartingColNumber=   0
            StartingRowNumber=   0
            UserResize      =   1
            VisibleCols     =   6
            VisibleRows     =   5
         End
         Begin Threed.SSCommand SSCommand12 
            Height          =   288
            Left            =   4452
            TabIndex        =   61
            Tag             =   "concentrationsLoc"
            Top             =   252
            Width           =   396
            _Version        =   65536
            _ExtentX        =   699
            _ExtentY        =   508
            _StockProps     =   78
            Caption         =   "<<"
         End
         Begin Threed.SSCommand SSCommand11 
            Height          =   288
            Left            =   4848
            TabIndex        =   62
            Tag             =   "concentrationsLoc"
            Top             =   252
            Width           =   396
            _Version        =   65536
            _ExtentX        =   699
            _ExtentY        =   508
            _StockProps     =   78
            Caption         =   ">>"
         End
         Begin Threed.SSCommand SSCommand6 
            Height          =   288
            Left            =   4452
            TabIndex        =   63
            Tag             =   "concentrationsParent"
            Top             =   3000
            Visible         =   0   'False
            Width           =   396
            _Version        =   65536
            _ExtentX        =   699
            _ExtentY        =   508
            _StockProps     =   78
            Caption         =   "<<"
         End
         Begin Threed.SSCommand SSCommand5 
            Height          =   288
            Left            =   4848
            TabIndex        =   64
            Tag             =   "concentrationsParent"
            Top             =   3000
            Visible         =   0   'False
            Width           =   396
            _Version        =   65536
            _ExtentX        =   699
            _ExtentY        =   508
            _StockProps     =   78
            Caption         =   ">>"
         End
         Begin Threed.SSCommand SSCommand8 
            Height          =   288
            Left            =   4848
            TabIndex        =   65
            Tag             =   "concentrationsParent"
            Top             =   648
            Width           =   396
            _Version        =   65536
            _ExtentX        =   699
            _ExtentY        =   508
            _StockProps     =   78
            Caption         =   ">>"
         End
         Begin Threed.SSCommand SSCommand7 
            Height          =   288
            Left            =   4452
            TabIndex        =   66
            Tag             =   "concentrationsParent"
            Top             =   648
            Width           =   396
            _Version        =   65536
            _ExtentX        =   699
            _ExtentY        =   508
            _StockProps     =   78
            Caption         =   "<<"
         End
         Begin VB.Label Label2 
            Caption         =   "Location"
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   240
            Left            =   195
            TabIndex        =   71
            Top             =   285
            Width           =   1110
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
            Index           =   10
            Left            =   195
            TabIndex        =   70
            Top             =   705
            Width           =   1050
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
            Index           =   10
            Left            =   5460
            TabIndex        =   69
            Tag             =   "0"
            Top             =   720
            Width           =   732
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
            Index           =   9
            Left            =   5400
            TabIndex        =   68
            Tag             =   "0"
            Top             =   3060
            Visible         =   0   'False
            Width           =   732
         End
         Begin VB.Label lbl 
            Caption         =   "Progeny"
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
            Index           =   9
            Left            =   200
            TabIndex        =   67
            Top             =   3000
            Visible         =   0   'False
            Width           =   684
         End
      End
      Begin Threed.SSFrame SSFrame3 
         Height          =   1305
         Left            =   5760
         TabIndex        =   72
         Top             =   4080
         Visible         =   0   'False
         Width           =   1725
         _Version        =   65536
         _ExtentX        =   3043
         _ExtentY        =   2302
         _StockProps     =   14
         Caption         =   "Data Set Type"
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ShadowStyle     =   1
         Begin VB.OptionButton Option4 
            Caption         =   "Ac&ute"
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
            Left            =   192
            TabIndex        =   74
            Top             =   864
            Width           =   972
         End
         Begin VB.OptionButton Option4 
            Caption         =   "&Chronic"
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
            Index           =   1
            Left            =   192
            TabIndex        =   73
            Top             =   480
            Value           =   -1  'True
            Width           =   1068
         End
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
      Visible         =   0   'False
      Begin VB.Menu prog 
         Caption         =   "Include Progeny"
      End
   End
   Begin VB.Menu reference 
      Caption         =   "&Reference"
      Begin VB.Menu add 
         Caption         =   "&Add"
      End
      Begin VB.Menu select 
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
         Caption         =   "How To ..."
      End
      Begin VB.Menu about 
         Caption         =   "&About"
      End
   End
End
Attribute VB_Name = "frmATO"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Dim fluxtypeidx As Long
Dim temp As parmrec

Private Sub about_Click()
  frmAbout.picIcon = frmAbout.ImageList1.ListImages(1).Picture
  frmAbout.Show 1, frmATO
End Sub

Private Sub Check1_GotFocus(Index As Integer)
  HelpAnchor = Check1(Index).Tag
End Sub

Private Sub Check2_GotFocus(Index As Integer)
  HelpAnchor = Check2(Index).Tag
End Sub

Private Sub Check3_GotFocus(Index As Integer)
  HelpAnchor = Check3(Index).Tag
End Sub

Private Sub Check4_GotFocus(Index As Integer)
  HelpAnchor = Check4(Index).Tag
End Sub

Private Sub Check5_GotFocus(Index As Integer)
  HelpAnchor = Check5(Index).Tag
End Sub

Private Sub Check6_GotFocus(Index As Integer)
  HelpAnchor = Check6(Index).Tag
End Sub

Private Sub Check7_GotFocus(Index As Integer)
  HelpAnchor = Check7(Index).Tag
End Sub

Private Sub Conc1_GotFocus()
  HelpAnchor = Conc1.Tag
End Sub

Private Sub Des1_GotFocus()
  HelpAnchor = Des1.Tag
End Sub

Private Sub howto_Click()
  GetHelp
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

Private Sub Option1_GotFocus(Index As Integer)
  HelpAnchor = Option1(Index).Tag
End Sub

Private Sub Option2_GotFocus(Index As Integer)
  HelpAnchor = Option2(Index).Tag
End Sub

Private Sub Option8_GotFocus(Index As Integer)
  HelpAnchor = Option8(Index).Tag
End Sub

Private Sub prog_Click()
  If loadng Then Exit Sub
  prog.Checked = Not prog.Checked
  showconcprog
  showprogeny
End Sub

Private Sub select_Click()
  RefMode = 0
  GetRef ref(RefItem)
End Sub

Private Sub add_Click()
  RefMode = 1
  GetRef ref(RefItem)
End Sub

Private Sub SizeConList()
  If temp.idx2 > numcon Then
    numcon = temp.idx2
    ReDim Preserve con(temp.idx2) As parent
  End If
  If temp.idx3 > con(temp.idx2).numprog Then
    con(temp.idx2).numprog = temp.idx3
    ReDim Preserve con(temp.idx2).prog(temp.idx3) As progeny
  End If
End Sub

Private Sub SizeLoc()
  Dim i As Long
  If temp.idx1 > numloc Then
    numloc = temp.idx1
    ReDim Preserve loc(temp.idx1) As location
  End If
  If temp.idx2 > loc(temp.idx1).numparent Then
    loc(temp.idx1).numparent = temp.idx2
    ReDim Preserve loc(temp.idx1).con(temp.idx2) As constituent
  End If
  If temp.idx2 > 0 Then
    If temp.idx3 > loc(temp.idx1).con(temp.idx2).numprog Then
      loc(temp.idx1).con(temp.idx2).numprog = temp.idx3
      ReDim Preserve loc(temp.idx1).con(temp.idx2).prog(temp.idx3) As progstruct
    End If
    If temp.idx4 > 0 Then
      If temp.idx3 = 0 Then
        If temp.idx5 > loc(temp.idx1).con(temp.idx2).out(temp.idx4).cnt Then
          For i = 0 To 15
            loc(temp.idx1).con(temp.idx2).out(i).cnt = temp.idx5
            ReDim Preserve loc(temp.idx1).con(temp.idx2).out(i).valu(temp.idx5) As Double
          Next
        End If
      Else
        If temp.idx5 > loc(temp.idx1).con(temp.idx2).prog(temp.idx3).out(temp.idx4).cnt Then
          For i = 0 To 15
            loc(temp.idx1).con(temp.idx2).prog(temp.idx3).out(i).cnt = temp.idx5
            ReDim Preserve loc(temp.idx1).con(temp.idx2).prog(temp.idx3).out(i).valu(temp.idx5) As Double
          Next
        End If
      End If
    End If
  End If
End Sub

Private Function addPrefix(pfx As String)
Dim i As Long
    
  For i = 0 To prefix.ListCount - 1
    If pfx = prefix.list(i) Then
      addPrefix = i + 1
      Exit Function
    End If
  Next
  prefix.AddItem pfx, prefix.ListCount
  numexp = prefix.ListCount
  ReDim Preserve exploc(prefix.ListCount) As expstruct
  addPrefix = prefix.NewIndex + 1
End Function

Private Sub fluxfillet()
Dim idx As Long
  
  idx = temp.idx1 * 2
  If temp.pName = "radius" Then idx = idx + 1
  FluxTypes.ref(idx).Tag = temp.ref
  FluxTypes.ref(idx).Caption = "Ref:" & Str(temp.ref)
  If temp.cunit <> "N/A" Then set_unit FluxTypes.Unit(idx), temp.uunit
  FluxTypes.txt(idx).Text = convert(temp.cunit, temp.uunit, Val(temp.pval))
End Sub

Private Sub fracfillet()
Dim idx As Long
  
  idx = 0
  If temp.pName = "reactivefrac" Then idx = 1
  FluxTypes.ref(idx).Tag = temp.ref
  FluxTypes.ref(idx).Caption = "Ref:" & Str(temp.ref)
  If temp.cunit <> "N/A" Then set_unit FluxTypes.Unit(idx), temp.uunit
  FluxTypes.txt(idx).Text = convert(temp.cunit, temp.uunit, Val(temp.pval))
End Sub

Private Sub loadprm()
  Dim i As Long, j As Long, k As Long, l As Long, m As Long
  Dim fle As parmfile
  Dim sval As Boolean
  Dim numcas As Long
  Dim cas() As parents
  
  fluxtypeidx = 0
  prefix.Clear
  If open_parm(fle, FUIName, 2) Then
    Do Until EOCF(fle.file)
      If read_parmrec(fle, temp) Then
        Select Case temp.pName
          Case "fui"
            For i = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                If temp.idx1 = siteIdx Then
                  j = addPrefix(Left(temp.pName, 3) & CStr(temp.idx2))
                  Select Case Mid(temp.pName, 4)
                    Case "name"
                      exploc(j).expname = temp.pval
                    Case "label"
                      exploc(j).explbl = temp.pval
                    Case "srcname"
                      If temp.pval = modName Then exploc(j).use = True
                      exploc(j).expsrc = temp.pval
                    ' in km
                    Case "x"
                      exploc(j).x = convert(temp.cunit, "m", Val(temp.pval))
                    Case "y"
                      exploc(j).y = convert(temp.cunit, "m", Val(temp.pval))
                    Case "z"
                      exploc(j).z = convert(temp.cunit, "m", Val(temp.pval))
                  End Select
                  Select Case temp.pName
                    Case "sitename"
                      sitename = temp.pval
                    Case "UsrDesPath"
                      If temp.idx2 = modIdx Then DesName = temp.pval
                    Case "fscname"
                      SizeConList
                      If temp.idx3 > 0 Then
                        con(temp.idx2).prog(temp.idx3).Name = temp.pval
                      Else
                        con(temp.idx2).Name = temp.pval
                      End If
                    Case "fscasid"
                      SizeConList
                      If temp.idx3 > 0 Then
                        con(temp.idx2).prog(temp.idx3).cas = temp.pval
                      Else
                        con(temp.idx2).cas = temp.pval
                      End If
                    Case "nds"
                      SizeConList
                    Case "clktype"
                      SizeConList
                      If temp.idx3 > 0 Then
                        con(temp.idx2).prog(temp.idx3).clk = Val(temp.pval)
                      Else
                        con(temp.idx2).clk = Val(temp.pval)
                      End If
                    Case "clclass"
                       SizeConList
                       If temp.idx3 > 0 Then
                         con(temp.idx2).prog(temp.idx3).clc = Val(temp.pval)
                       Else
                         con(temp.idx2).clc = Val(temp.pval)
                       End If
                  End Select
                End If
              End If
            Next
          Case modName
            Loading.Gauge1.Max = Val(temp.idx1)
            Loading.Gauge1.Value = 0
            For i = 1 To temp.idx1
              Loading.update
              If read_parmrec(fle, temp) Then
                Select Case temp.pName
                  Case "CVTFormat":    CVTFormat = temp.pval
                  Case "locname"
                    SizeLoc
                    loc(temp.idx1).Name = temp.pval
                  Case "reactivefrac", "reactivedensity":
                    fracfillet
                  Case "radius", "density":
                    fluxfillet
                  Case "fluxtypes":
                    sval = False
                    If temp.pval = "True" Then sval = True
                    FluxTypes.SSCheck1(temp.idx1).Value = sval
                    fluxtypeidx = temp.idx1
                  Case "showprog":
                    sval = False
                    If temp.pval = "True" Then
                      'sval = True
                      'no more prooduct output
                      MsgBox "This case contained progeny concentrations/intakes which are no loneger supported!" & vbCrLf & _
                             "Those inventories have been removed and must be rentered as parents.", vbInformation
                    End If
                    prog.Checked = sval
                  Case "settype"
                    If (temp.pval = "acute") And (settype = True) Then
                      Option4(0).Value = True
                      settype = True
                      yr = Val(temp.idx1)
                      mnth = Val(temp.idx2)
                      dy = Val(temp.idx3)
                      hr = Val(temp.idx4)
                      mnt = Val(temp.idx5)
                    End If
                  Case "outcnt"
                    SizeLoc
                    If temp.idx3 = 0 Then
                      loc(temp.idx1).con(temp.idx2).ref = temp.ref
                      loc(temp.idx1).con(temp.idx2).cnt = Val(temp.pval)
                    Else
                      loc(temp.idx1).con(temp.idx2).prog(temp.idx3).ref = temp.ref
                      loc(temp.idx1).con(temp.idx2).prog(temp.idx3).cnt = Val(temp.pval)
                    End If
                  Case "gas"
                    SizeLoc
                    If temp.idx3 = 0 Then
                      loc(temp.idx1).con(temp.idx2).gas = Val(temp.pval)
                    Else
                      loc(temp.idx1).con(temp.idx2).prog(temp.idx3).gas = Val(temp.pval)
                    End If
                  Case "particle1"
                    SizeLoc
                    If temp.idx3 = 0 Then
                      loc(temp.idx1).con(temp.idx2).p1 = Val(temp.pval)
                    Else
                      loc(temp.idx1).con(temp.idx2).prog(temp.idx3).p1 = Val(temp.pval)
                    End If
                  Case "particle2"
                    SizeLoc
                    If temp.idx3 = 0 Then
                      loc(temp.idx1).con(temp.idx2).p2 = Val(temp.pval)
                    Else
                      loc(temp.idx1).con(temp.idx2).prog(temp.idx3).p2 = Val(temp.pval)
                    End If
                  Case "particle3"
                    SizeLoc
                    If temp.idx3 = 0 Then
                      loc(temp.idx1).con(temp.idx2).p3 = Val(temp.pval)
                    Else
                      loc(temp.idx1).con(temp.idx2).prog(temp.idx3).p3 = Val(temp.pval)
                    End If
                  Case "air"
                    SizeLoc
                    If temp.idx3 = 0 Then
                      loc(temp.idx1).con(temp.idx2).air = Val(temp.pval)
                    Else
                      loc(temp.idx1).con(temp.idx2).prog(temp.idx3).air = Val(temp.pval)
                    End If
                  Case "ext"
                    SizeLoc
                    If temp.idx3 = 0 Then
                      loc(temp.idx1).con(temp.idx2).ext = Val(temp.pval)
                    Else
                      loc(temp.idx1).con(temp.idx2).prog(temp.idx3).ext = Val(temp.pval)
                    End If
                  Case "dep"
                    SizeLoc
                    If temp.idx3 = 0 Then
                      loc(temp.idx1).con(temp.idx2).dep = Val(temp.pval)
                    Else
                      loc(temp.idx1).con(temp.idx2).prog(temp.idx3).dep = Val(temp.pval)
                    End If
                  Case "depwet"
                    SizeLoc
                    sval = False
                    If temp.pval = "TRUE" Then sval = True
                    If temp.idx3 = 0 Then
                      loc(temp.idx1).con(temp.idx2).wet = sval
                    Else
                      loc(temp.idx1).con(temp.idx2).prog(temp.idx3).wet = sval
                    End If
                  Case "depdry"
                    SizeLoc
                    sval = False
                    If temp.pval = "TRUE" Then sval = True
                    If temp.idx3 = 0 Then
                      loc(temp.idx1).con(temp.idx2).dry = sval
                    Else
                      loc(temp.idx1).con(temp.idx2).prog(temp.idx3).dry = sval
                    End If
                  Case "deptot"
                    SizeLoc
                    sval = False
                    If temp.pval = "TRUE" Then sval = True
                    If temp.idx3 = 0 Then
                      loc(temp.idx1).con(temp.idx2).tot = sval
                    Else
                      loc(temp.idx1).con(temp.idx2).prog(temp.idx3).tot = sval
                    End If
                  Case "casid":
                    If temp.idx1 > numcas Then
                      numcas = temp.idx1
                      ReDim Preserve cas(temp.idx1) As parents
                    End If
                    If temp.idx2 = 0 Then
                      cas(temp.idx1).cas = temp.pval
                    Else
                      If temp.idx2 > cas(temp.idx1).numprog Then
                        cas(temp.idx1).numprog = temp.idx2
                        ReDim Preserve cas(temp.idx1).prog(temp.idx2) As progenys
                      End If
                      cas(temp.idx1).prog(temp.idx2).cas = temp.pval
                    End If
                  Case "values"
                    SizeLoc
                    'added for backward compatibility error
                    temp.uunit = Replace(temp.uunit, "m2", "m^2")
                    temp.uunit = Replace(temp.uunit, "m3", "m^3")
                    temp.cunit = Replace(temp.cunit, "m2", "m^2")
                    temp.cunit = Replace(temp.cunit, "m3", "m^3")
                    
                    If temp.idx3 = 0 Then
                      loc(temp.idx1).con(temp.idx2).out(temp.idx4).uunit = temp.uunit
                      loc(temp.idx1).con(temp.idx2).out(temp.idx4).cunit = temp.cunit
                      loc(temp.idx1).con(temp.idx2).out(temp.idx4).valu(temp.idx5) = convert(temp.cunit, temp.uunit, Val(temp.pval))
                    Else
                      loc(temp.idx1).con(temp.idx2).prog(temp.idx3).out(temp.idx4).uunit = temp.uunit
                      loc(temp.idx1).con(temp.idx2).prog(temp.idx3).out(temp.idx4).cunit = temp.cunit
                      loc(temp.idx1).con(temp.idx2).prog(temp.idx3).out(temp.idx4).valu(temp.idx5) = convert(temp.cunit, temp.uunit, Val(temp.pval))
                    End If
                  Case "times"
                    If temp.idx1 > numcas Then
                      numcas = temp.idx1
                      ReDim Preserve cas(temp.idx1) As parents
                    End If
                    If temp.idx2 = 0 Then
                      If temp.idx3 > cas(temp.idx1).tsdcnt Then
                        cas(temp.idx1).tsdcnt = temp.idx3
                        ReDim Preserve cas(temp.idx1).tsd(temp.idx3) As Double
                      End If
                      cas(temp.idx1).tsd(temp.idx3) = convert(temp.cunit, temp.uunit, Val(temp.pval))
                      If cas(temp.idx1).uunit = "" Then cas(temp.idx1).uunit = temp.uunit
                      cas(temp.idx1).cunit = "yr"
                    Else
                      If temp.idx2 > cas(temp.idx1).numprog Then
                        cas(temp.idx1).numprog = temp.idx2
                        ReDim Preserve cas(temp.idx1).prog(temp.idx2) As progenys
                      End If
                      If temp.idx3 > cas(temp.idx1).prog(temp.idx2).tsdcnt Then
                        cas(temp.idx1).prog(temp.idx2).tsdcnt = temp.idx3
                        ReDim Preserve cas(temp.idx1).prog(temp.idx2).tsd(temp.idx3) As Double
                      End If
                      cas(temp.idx1).prog(temp.idx2).tsd(temp.idx3) = convert(temp.cunit, temp.uunit, Val(temp.pval))
                      If cas(temp.idx1).prog(temp.idx2).uunit = "" Then cas(temp.idx1).prog(temp.idx2).uunit = temp.uunit
                      cas(temp.idx1).prog(temp.idx2).cunit = "yr"
                    End If
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
    prefix.Clear
    close_parm fle
    SetFormat Me
    
'  set time series stuff for matched 'con'stituents
    If numcas > 0 Then
      For i = 1 To numcon
        For j = 1 To numcas
          If cas(j).cas = con(i).cas Then
            con(i).tsdcnt = cas(j).tsdcnt
            ReDim Preserve con(i).tsd(cas(j).tsdcnt) As Double
            For k = 1 To cas(j).tsdcnt
              con(i).tsd(k) = cas(j).tsd(k)
            Next
            If cas(j).tsdcnt > 0 Then
              con(i).uunit = cas(j).uunit
              con(i).cunit = cas(j).cunit
            End If
            For k = 1 To con(i).numprog
              For l = 1 To cas(j).numprog
                If cas(j).prog(l).cas = con(i).prog(k).cas Then
                  con(i).prog(k).tsdcnt = cas(j).prog(l).tsdcnt
                  ReDim Preserve con(i).prog(k).tsd(cas(j).prog(l).tsdcnt) As Double
                  For m = 1 To cas(j).prog(l).tsdcnt
                    con(i).prog(k).tsd(m) = cas(j).prog(l).tsd(m)
                  Next
                  If cas(j).prog(l).tsdcnt > 0 Then
                    con(i).prog(k).uunit = cas(j).prog(l).uunit
                    con(i).prog(k).cunit = cas(j).prog(l).cunit
                  End If
                  Exit For
                End If
              Next
            Next
            Exit For
          End If
        Next
      Next
    End If
    
    'resolve locations
    For i = 1 To numexp
      If exploc(i).use Then
        For j = 1 To numloc
          If loc(j).Name = exploc(i).expname Then Exit For
        Next
        If j > numloc Then GoTo ADLOC
        If loc(j).Name = exploc(i).expname Then
ADLOC:
          Des1.AddItem exploc(i).explbl + "(" + exploc(i).expname + ")"
          Des1.ItemData(Des1.NewIndex) = j
          Conc1.AddItem exploc(i).explbl + "(" + exploc(i).expname + ")"
          Conc1.ItemData(Conc1.NewIndex) = j
          If j > numloc Then
            ReDim Preserve loc(j) As location
            addChmToLoc j
            numloc = j
          End If
          loc(j).Name = exploc(i).expname
          loc(j).use = i
          loc(j).lbl = exploc(i).explbl
          loc(j).x = exploc(i).x
          loc(j).y = exploc(i).y
          loc(j).z = exploc(i).z
        End If
      End If
    Next
    
    'no locations
    If numloc = 0 Then
      Call PutError("Not connected to a glyph")
      Call EndModule 'program
    End If

    'constituent resolution
    For i = 1 To numcon
      For j = 1 To numcas
        If cas(j).cas = con(i).cas Then
          Des2.AddItem con(i).Name
          Des2.ItemData(Des2.NewIndex) = j
          Conc2.AddItem con(i).Name
          Conc2.ItemData(Conc2.NewIndex) = j
          For k = 1 To numloc
            If loc(k).use > 0 Then
              loc(k).con(j).use = i
              For l = 1 To con(i).numprog
                For m = 1 To cas(j).numprog
                  If cas(j).prog(m).cas = con(i).prog(l).cas Then
                    loc(k).con(j).prog(m).use = l
                    Exit For
                  End If
                Next
                If m > cas(j).numprog Then
                  loc(k).con(j).numprog = m
                  ReDim Preserve loc(k).con(j).prog(m) As progstruct
                  loc(k).con(j).prog(m).use = l
                  SetProgOutputs loc(k).con(j).prog(m)
                End If
                SetUnits loc(k).con(j).prog(m).out, con(i).prog(l).clk
                SetTimeUnits con(i).prog(l).uunit, con(i).prog(l).cunit
              Next
              SetUnits loc(k).con(j).out, con(i).clk
              SetTimeUnits con(i).uunit, con(i).cunit
            End If
          Next
          Exit For
        End If
      Next
      '  Must be a new constituent
      If j > numcas Then
        ReDim Preserve cas(j) As parents
        numcas = j
        Des2.AddItem con(i).Name
        Des2.ItemData(Des2.NewIndex) = j
        Conc2.AddItem con(i).Name
        Conc2.ItemData(Conc2.NewIndex) = j
        For k = 1 To numloc
          If loc(k).use > 0 Then
            loc(k).numparent = j
            ReDim Preserve loc(k).con(j) As constituent
            loc(k).con(j).use = i
            loc(k).con(j).numprog = con(i).numprog
            If con(i).numprog > 0 Then
              ReDim Preserve loc(k).con(j).prog(con(i).numprog) As progstruct
              For l = 1 To con(i).numprog
                loc(k).con(j).prog(l).use = l
                con(i).prog(l).Name = con(i).prog(l).Name
                SetUnits loc(k).con(j).prog(l).out, con(i).prog(l).clk
                SetTimeUnits con(i).prog(l).uunit, con(i).prog(l).cunit
              Next
            End If
            con(i).Name = con(i).Name
            SetOutputs loc(k).con(j)
            SetUnits loc(k).con(j).out, con(i).clk
            SetTimeUnits con(i).uunit, con(i).cunit
          End If
        Next
      End If
    Next

  Else
    PutError "Can't find or open file " & FUIName
    EndModule
  End If
End Sub
          
Private Sub addChmToLoc(k As Long)
Dim j As Long
Dim l As Long
  
  loc(k).numparent = numcon
  For j = 1 To numcon
    ReDim Preserve loc(k).con(j) As constituent
    loc(k).con(j).use = j
    loc(k).con(j).numprog = con(j).numprog
    If con(j).numprog > 0 Then
      ReDim Preserve loc(k).con(j).prog(con(j).numprog) As progstruct
      For l = 1 To con(j).numprog
        loc(k).con(j).prog(l).use = l
        SetUnits loc(k).con(j).prog(l).out, con(j).prog(l).clk
        SetTimeUnits con(j).prog(l).uunit, con(j).prog(l).cunit
      Next
    End If
    SetOutputs loc(k).con(j)
    SetUnits loc(k).con(j).out, con(j).clk
    SetTimeUnits con(j).uunit, con(j).cunit
    For l = 1 To 15
      loc(k).con(j).out(l).cnt = con(j).tsdcnt
      ReDim Preserve loc(k).con(j).out(l).valu(con(j).tsdcnt) As Double
    Next
  Next
End Sub

Private Sub Form_load()
  StartModule frmATO, "FRAMES Known Air Concentrations", 6
  loadng = True
  Loading.Show
  mode = Val(argv(0))
  If (mode = 0 Or mode = 2) Then
    frmATO.Caption = "FRAMES ATO Acute Air Module"
    SSFrame4.Enabled = True
    SSFrame4.Visible = True
  Else
    frmATO.Caption = "FRAMES ATO Chronic Air Module"
  End If
  SetHelpFile App.Path + "\Known ATO.html"
  SetRefFile ReplaceExt(FUIName, "ref")
  
  yr = Year(Now)
  hr = Hour(Now)
  mnth = Month(Now)
  mnt = Minute(Now)
  dy = Day(Now)
  If mode = 0 Then
    settype = True
    Option4(0).Value = True
  ElseIf mode = 1 Then
    settype = False
    Option4(1).Value = True
  ElseIf mode = 2 Then
    settype = True
    Option4(0).Value = True
  Else
    settype = False
    Option4(1).Value = True
  End If
  loadprm
  Unload Loading
  If (mode = 2) Or (mode = 3) Then
    make_ATO
    EndModule
  End If
  SSCommand13_Click
  Des1.ListIndex = 0
  Des2.ListIndex = 0
  c1 = Conc1.ItemData(0)
  c2 = Conc2.ItemData(0)
  Conc1.ListIndex = 0
  Conc2.ListIndex = 0
  SSTab1_GotFocus
  loadng = False
 ' Refresh
End Sub

Private Sub Des1_Click()
  d1 = Des1.ItemData(Des1.ListIndex)
  PutOut
End Sub

Private Sub Des2_Click()
Dim i As Long
Dim j As Long
Dim z As Long

  z = Des2.ListIndex + 1
  d2 = Des2.ItemData(Des2.ListIndex)
  If con(z).numprog > 0 Then
    Des3.Clear
    For i = 1 To con(z).numprog
      Des3.AddItem con(z).prog(i).Name
      For j = 1 To loc(d1).con(d2).numprog
        If loc(d1).con(d2).prog(j).use = i Then Des3.ItemData(i - 1) = j
      Next
    Next
    Des3.ListIndex = 0
  Else
    PutOut
  End If
  showprogeny
End Sub

Private Sub Des3_Click()
  d3 = Des3.ItemData(Des3.ListIndex)
  PutOut
End Sub

Private Sub Check1_Click(Index As Integer)
  If loadng Then Exit Sub
  If Index = 0 Then
    loc(d1).con(d2).air = Check1(0).Value
  Else
    loc(d1).con(d2).prog(d3).air = Check1(1).Value
  End If
End Sub

Private Sub Check2_Click(Index As Integer)
  If Check2(Index).Value = 1 Then
    Option1(Index).Enabled = True
    Option2(Index).Enabled = True
    Option8(Index).Enabled = True
  Else
    Option1(Index).Enabled = False
    Option2(Index).Enabled = False
    Option8(Index).Enabled = False
  End If
  If loadng Then Exit Sub
  If Index = 0 Then
    loc(d1).con(d2).dep = Check2(0).Value
  Else
    loc(d1).con(d2).prog(d3).dep = Check2(1).Value
  End If
End Sub

Private Sub Check3_Click(Index As Integer)
  If loadng Then Exit Sub
  If Index = 0 Then
    loc(d1).con(d2).ext = Check3(0).Value
  Else
    loc(d1).con(d2).prog(d3).ext = Check3(1).Value
  End If
End Sub

Private Sub Check4_Click(Index As Integer)
  If loadng Then Exit Sub
  If Index = 0 Then
    loc(d1).con(d2).gas = Check4(0).Value
  Else
    loc(d1).con(d2).prog(d3).gas = Check4(1).Value
  End If
End Sub

Private Sub Check5_Click(Index As Integer)
  If loadng Then Exit Sub
  If Index = 0 Then
    loc(d1).con(d2).p1 = Check5(0).Value
  Else
    loc(d1).con(d2).prog(d3).p1 = Check5(1).Value
  End If
End Sub

Private Sub Check6_Click(Index As Integer)
  If loadng Then Exit Sub
  If Index = 0 Then
    loc(d1).con(d2).p2 = Check6(0).Value
  Else
    loc(d1).con(d2).prog(d3).p2 = Check6(1).Value
  End If
End Sub

Private Sub Check7_Click(Index As Integer)
  If loadng Then Exit Sub
  If Index = 0 Then
    loc(d1).con(d2).p3 = Check7(0).Value
  Else
    loc(d1).con(d2).prog(d3).p3 = Check7(1).Value
  End If
End Sub

Private Sub DepOption(Index As Long)
  If loadng Then Exit Sub
  If Index = 0 Then
    loc(d1).con(d2).wet = Option1(0).Value
    loc(d1).con(d2).tot = Option2(0).Value
    loc(d1).con(d2).dry = Option8(0).Value
  Else
    loc(d1).con(d2).prog(d3).wet = Option1(0).Value
    loc(d1).con(d2).prog(d3).tot = Option2(0).Value
    loc(d1).con(d2).prog(d3).dry = Option8(0).Value
  End If
End Sub

Private Sub Option1_Click(Index As Integer)
  DepOption CLng(Index)
End Sub

Private Sub Option2_Click(Index As Integer)
  DepOption CLng(Index)
End Sub

Private Sub Option8_Click(Index As Integer)
  DepOption CLng(Index)
End Sub

Private Sub SSCommand1_Click()
  If Des2.ListIndex > 0 Then Des2.ListIndex = Des2.ListIndex - 1
End Sub

Private Sub SSCommand1_GotFocus()
  HelpAnchor = SSCommand1.Tag
End Sub

Private Sub SSCommand10_Click()
  If Des1.ListIndex < Des1.ListCount - 1 Then Des1.ListIndex = Des1.ListIndex + 1
End Sub

Private Sub SSCommand10_GotFocus()
  HelpAnchor = SSCommand10.Tag
End Sub

Private Sub SSCommand11_Click()
  If Conc1.ListIndex < Conc1.ListCount - 1 Then Conc1.ListIndex = Conc1.ListIndex + 1
End Sub

Private Sub SSCommand11_GotFocus()
  HelpAnchor = SSCommand11.Tag
End Sub

Private Sub SSCommand12_Click()
  If Conc1.ListIndex > 0 Then Conc1.ListIndex = Conc1.ListIndex - 1
End Sub

Private Sub SSCommand12_GotFocus()
  HelpAnchor = SSCommand12.Tag
End Sub

Private Sub SSCommand13_Click()
  If Not loadng Then FluxTypes.Show 1
' set particle definition boxes for constituent definition
  If FluxTypes.SSCheck1(0).Value Then
    Check4(0).Enabled = True
    Check4(1).Enabled = True
  Else
    Check4(0).Enabled = False
    Check4(1).Enabled = False
  End If
  If FluxTypes.SSCheck1(1).Value Then
    Check5(0).Enabled = True
    Check5(1).Enabled = True
  Else
    Check5(0).Enabled = False
    Check5(1).Enabled = False
  End If
  If FluxTypes.SSCheck1(2).Value Then
    Check6(0).Enabled = True
    Check6(1).Enabled = True
  Else
    Check6(0).Enabled = False
    Check6(1).Enabled = False
  End If
  If FluxTypes.SSCheck1(3).Value Then
    Check7(0).Enabled = True
    Check7(1).Enabled = True
  Else
    Check7(0).Enabled = False
    Check7(1).Enabled = False
  End If
End Sub

Private Sub SSCommand13_GotFocus()
  HelpAnchor = "definitions"
End Sub

Private Sub SSCommand2_Click()
   If Des2.ListIndex < Des2.ListCount - 1 Then Des2.ListIndex = Des2.ListIndex + 1
End Sub

Private Sub SSCommand2_GotFocus()
  HelpAnchor = SSCommand2.Tag
End Sub

Private Sub SSCommand3_Click()
  If Des3.ListIndex > 0 Then Des3.ListIndex = Des3.ListIndex - 1
End Sub

Private Sub SSCommand3_GotFocus()
  HelpAnchor = SSCommand3.Tag
End Sub

Private Sub SSCommand4_Click()
   If Des3.ListIndex < Des3.ListCount - 1 Then Des3.ListIndex = Des3.ListIndex + 1
End Sub

Private Sub SSCommand4_GotFocus()
  HelpAnchor = SSCommand4.Tag
End Sub

Private Sub SSCommand5_Click()
  If Conc3.ListIndex < Conc3.ListCount - 1 Then Conc3.ListIndex = Conc3.ListIndex + 1
End Sub

Private Sub SSCommand5_GotFocus()
  HelpAnchor = SSCommand5.Tag
End Sub

Private Sub SSCommand6_Click()
  If Conc3.ListIndex > 0 Then Conc3.ListIndex = Conc3.ListIndex - 1
End Sub

Private Sub SSCommand6_GotFocus()
  HelpAnchor = SSCommand6.Tag
End Sub

Private Sub SSCommand7_Click()
  If Conc2.ListIndex > 0 Then Conc2.ListIndex = Conc2.ListIndex - 1
End Sub

Private Sub SSCommand7_GotFocus()
  HelpAnchor = SSCommand7.Tag
End Sub

Private Sub SSCommand8_Click()
  If Conc2.ListIndex < Conc2.ListCount - 1 Then Conc2.ListIndex = Conc2.ListIndex + 1
End Sub

Private Sub Option4_Click(Index As Integer)
  Dim i As Long
  Dim j As Long
  Dim isprog As Boolean
  
  vaSpread1.Visible = False
  If Option4(0).Value Then
    settype = True
  Else
    settype = False
  End If
  For i = 1 To numcon
    SetTimeUnits con(i).uunit, con(i).cunit
    If con(i).numprog > 0 Then
      isprog = True
      For j = 1 To con(i).numprog
        SetTimeUnits con(i).prog(j).uunit, con(i).prog(j).cunit
      Next
    End If
  Next
  vaSpread1.col = 1
  vaSpread1.Row = 1
  vaSpread2.col = 1
  vaSpread2.Row = 1
  If Option4(0).Value Then
    SetSpreadUnits vaSpread1, 1, "hr"
    vaSpread1.TypeComboBoxEditable = True
    vaSpread1.Text = "hr"
    vaSpread1.TypeComboBoxEditable = False
    If isprog Then
      SetSpreadUnits vaSpread2, 1, "hr"
      vaSpread2.TypeComboBoxEditable = True
      vaSpread2.Text = "hr"
      vaSpread2.TypeComboBoxEditable = False
    End If
  Else
    SetSpreadUnits vaSpread1, 1, "yr"
    vaSpread1.TypeComboBoxEditable = True
    vaSpread1.Text = "yr"
    vaSpread1.TypeComboBoxEditable = False
    If isprog Then
      SetSpreadUnits vaSpread2, 1, "yr"
      vaSpread2.TypeComboBoxEditable = True
      vaSpread2.Text = "yr"
      vaSpread2.TypeComboBoxEditable = False
    End If
  End If
  vaSpread1.Visible = True
End Sub

Private Sub showprogeny()
Dim hide As Boolean
  
  hide = False
  If loc(d1).con(d2).numprog > 0 And prog.Checked Then hide = True
  lbl(7).Visible = hide
  Des3.Visible = hide
  SSCommand3.Visible = hide
  SSCommand4.Visible = hide
  SSFrame6.Visible = hide
  SSFrame8.Visible = hide
End Sub

Private Sub showconcprog()
Dim hide As Boolean
  
  hide = False
  If loc(c1).con(c2).numprog > 0 And prog.Checked Then hide = True
  lbl(9).Visible = hide
  Conc3.Visible = hide
  SSCommand6.Visible = hide
  SSCommand5.Visible = hide
  ref(9).Visible = hide
  vaSpread2.Visible = hide
End Sub

Private Sub PutOut()
Dim i As Long
Dim j As Long
Dim k As Long

  If Des1.ListIndex < 0 Then Exit Sub
  If Des2.ListIndex < 0 Then Exit Sub
  i = Des1.ItemData(Des1.ListIndex)
  j = Des2.ItemData(Des2.ListIndex)
  Check4(0).Value = loc(i).con(j).gas
  Check5(0).Value = loc(i).con(j).p1
  Check6(0).Value = loc(i).con(j).p2
  Check7(0).Value = loc(i).con(j).p3
  Check1(0).Value = loc(i).con(j).air
  Check3(0).Value = loc(i).con(j).ext
  If con(Des2.ListIndex + 1).clk = 1 Then
    Check3(0).Enabled = True
  Else
    Check3(0).Enabled = False
  End If
  Check2(0).Value = loc(i).con(j).dep
  Option1(0).Value = loc(i).con(j).wet
  Option8(0).Value = loc(i).con(j).dry
  Option2(0).Value = loc(i).con(j).tot
  If Des3.ListIndex < 0 Then Exit Sub
  k = Des3.ItemData(Des3.ListIndex)
  If con(Des2.ListIndex + 1).numprog > 0 Then
    Check4(1).Value = loc(i).con(j).prog(k).gas
    Check5(1).Value = loc(i).con(j).prog(k).p1
    Check6(1).Value = loc(i).con(j).prog(k).p2
    Check7(1).Value = loc(i).con(j).prog(k).p3
    Check1(1).Value = loc(i).con(j).prog(k).air
    Check3(1).Value = loc(i).con(j).prog(k).ext
    If con(Des2.ListIndex + 1).prog(Des3.ListIndex + 1).clk = 1 Then
      Check3(1).Enabled = True
    Else
      Check3(1).Enabled = False
    End If
    Check2(1).Value = loc(i).con(j).prog(k).dep
    Option1(1).Value = loc(i).con(j).prog(k).wet
    Option8(1).Value = loc(i).con(j).prog(k).dry
    Option2(1).Value = loc(i).con(j).prog(k).tot
  End If
End Sub

Private Sub ShowParentCol()
  hidecolumns vaSpread1
  loc(c1).con(c2).cnt = 0
  If loc(c1).con(c2).ext = 1 Then
    vaSpread1.col = 2
    vaSpread1.ColHidden = False
    loc(c1).con(c2).cnt = loc(c1).con(c2).cnt + 1
  End If
  If (loc(c1).con(c2).dep = 1) And (loc(c1).con(c2).gas = 1) And FluxTypes.SSCheck1(0).Value Then
    vaSpread1.col = 3
    vaSpread1.ColHidden = False
    loc(c1).con(c2).cnt = loc(c1).con(c2).cnt + 1
  End If
  If loc(c1).con(c2).dep = 1 Then
    If loc(c1).con(c2).p1 = 1 And FluxTypes.SSCheck1(1).Value Then
      If loc(c1).con(c2).wet = True Then
        vaSpread1.col = 4
        vaSpread1.ColHidden = False
        loc(c1).con(c2).cnt = loc(c1).con(c2).cnt + 1
      End If
      If loc(c1).con(c2).dry = True Then
        vaSpread1.col = 5
        vaSpread1.ColHidden = False
        loc(c1).con(c2).cnt = loc(c1).con(c2).cnt + 1
      End If
      If loc(c1).con(c2).tot = True Then
        vaSpread1.col = 6
        vaSpread1.ColHidden = False
        loc(c1).con(c2).cnt = loc(c1).con(c2).cnt + 1
      End If
    End If
    If loc(c1).con(c2).p2 = 1 And FluxTypes.SSCheck1(2).Value Then
      If loc(c1).con(c2).wet = True Then
        vaSpread1.col = 7
        vaSpread1.ColHidden = False
        loc(c1).con(c2).cnt = loc(c1).con(c2).cnt + 1
      End If
      If loc(c1).con(c2).dry = True Then
        vaSpread1.col = 8
        vaSpread1.ColHidden = False
        loc(c1).con(c2).cnt = loc(c1).con(c2).cnt + 1
      End If
      If loc(c1).con(c2).tot = True Then
        vaSpread1.col = 9
        vaSpread1.ColHidden = False
        loc(c1).con(c2).cnt = loc(c1).con(c2).cnt + 1
      End If
    End If
    If loc(c1).con(c2).p3 = 1 And FluxTypes.SSCheck1(3).Value Then
      If loc(c1).con(c2).wet = True Then
        vaSpread1.col = 10
        vaSpread1.ColHidden = False
        loc(c1).con(c2).cnt = loc(c1).con(c2).cnt + 1
      End If
      If loc(c1).con(c2).dry = True Then
        vaSpread1.col = 11
        vaSpread1.ColHidden = False
        loc(c1).con(c2).cnt = loc(c1).con(c2).cnt + 1
      End If
      If loc(c1).con(c2).tot = True Then
        vaSpread1.col = 12
        vaSpread1.ColHidden = False
        loc(c1).con(c2).cnt = loc(c1).con(c2).cnt + 1
      End If
    End If
  End If
  If loc(c1).con(c2).air = 1 Then
    If loc(c1).con(c2).gas = 1 And FluxTypes.SSCheck1(0).Value Then
      vaSpread1.col = 13
      vaSpread1.ColHidden = False
      loc(c1).con(c2).cnt = loc(c1).con(c2).cnt + 1
    End If
    If loc(c1).con(c2).p1 = 1 And FluxTypes.SSCheck1(1).Value Then
      vaSpread1.col = 14
      vaSpread1.ColHidden = False
      loc(c1).con(c2).cnt = loc(c1).con(c2).cnt + 1
    End If
    If loc(c1).con(c2).p2 = 1 And FluxTypes.SSCheck1(2).Value Then
      vaSpread1.col = 15
      vaSpread1.ColHidden = False
      loc(c1).con(c2).cnt = loc(c1).con(c2).cnt + 1
    End If
    If loc(c1).con(c2).p3 = 1 And FluxTypes.SSCheck1(3).Value Then
      vaSpread1.col = 16
      vaSpread1.ColHidden = False
      loc(c1).con(c2).cnt = loc(c1).con(c2).cnt + 1
    End If
  End If
End Sub

Private Sub ShowProgenyCol()
  hidecolumns vaSpread2
  loc(c1).con(c2).prog(c3).cnt = 0
  If loc(c1).con(c2).prog(c3).ext = 1 Then
    vaSpread2.col = 2
    vaSpread2.ColHidden = False
    loc(c1).con(c2).prog(c3).cnt = loc(c1).con(c2).prog(c3).cnt + 1
  End If
  If (loc(c1).con(c2).prog(c3).dep = 1) And (loc(c1).con(c2).prog(c3).gas = 1) And FluxTypes.SSCheck1(0).Value Then
    vaSpread2.col = 3
    vaSpread2.ColHidden = False
    loc(c1).con(c2).prog(c3).cnt = loc(c1).con(c2).prog(c3).cnt + 1
  End If
  If loc(c1).con(c2).prog(c3).dep = 1 Then
    If loc(c1).con(c2).prog(c3).p1 = 1 And FluxTypes.SSCheck1(1).Value Then
      If loc(c1).con(c2).prog(c3).wet = True Then
        vaSpread2.col = 4
        vaSpread2.ColHidden = False
        loc(c1).con(c2).prog(c3).cnt = loc(c1).con(c2).prog(c3).cnt + 1
      End If
      If loc(c1).con(c2).prog(c3).dry = True Then
        vaSpread2.col = 5
        vaSpread2.ColHidden = False
        loc(c1).con(c2).prog(c3).cnt = loc(c1).con(c2).prog(c3).cnt + 1
      End If
      If loc(c1).con(c2).prog(c3).tot = True Then
        vaSpread2.col = 6
        vaSpread2.ColHidden = False
        loc(c1).con(c2).prog(c3).cnt = loc(c1).con(c2).prog(c3).cnt + 1
      End If
    End If
    If loc(c1).con(c2).prog(c3).p2 = 1 And FluxTypes.SSCheck1(2).Value Then
      If loc(c1).con(c2).prog(c3).wet = True Then
        vaSpread2.col = 7
        vaSpread2.ColHidden = False
        loc(c1).con(c2).prog(c3).cnt = loc(c1).con(c2).prog(c3).cnt + 1
      End If
      If loc(c1).con(c2).prog(c3).dry = True Then
        vaSpread2.col = 8
        vaSpread2.ColHidden = False
        loc(c1).con(c2).prog(c3).cnt = loc(c1).con(c2).prog(c3).cnt + 1
      End If
      If loc(c1).con(c2).prog(c3).tot = True Then
        vaSpread2.col = 9
        vaSpread2.ColHidden = False
        loc(c1).con(c2).prog(c3).cnt = loc(c1).con(c2).prog(c3).cnt + 1
      End If
    End If
    If loc(c1).con(c2).prog(c3).p3 = 1 And FluxTypes.SSCheck1(3).Value Then
      If loc(c1).con(c2).prog(c3).wet = True Then
        vaSpread2.col = 10
        vaSpread2.ColHidden = False
        loc(c1).con(c2).prog(c3).cnt = loc(c1).con(c2).prog(c3).cnt + 1
      End If
      If loc(c1).con(c2).prog(c3).dry = True Then
        vaSpread2.col = 11
        vaSpread2.ColHidden = False
        loc(c1).con(c2).prog(c3).cnt = loc(c1).con(c2).prog(c3).cnt + 1
      End If
      If loc(c1).con(c2).prog(c3).tot = True Then
        vaSpread2.col = 12
        vaSpread2.ColHidden = False
        loc(c1).con(c2).prog(c3).cnt = loc(c1).con(c2).prog(c3).cnt + 1
      End If
    End If
  End If
  If loc(c1).con(c2).prog(c3).air = 1 Then
    If loc(c1).con(c2).prog(c3).gas = 1 And FluxTypes.SSCheck1(0).Value Then
      vaSpread2.col = 13
      vaSpread2.ColHidden = False
      loc(c1).con(c2).prog(c3).cnt = loc(c1).con(c2).prog(c3).cnt + 1
    End If
    If loc(c1).con(c2).prog(c3).p1 = 1 And FluxTypes.SSCheck1(1).Value Then
      vaSpread2.col = 14
      vaSpread2.ColHidden = False
      loc(c1).con(c2).prog(c3).cnt = loc(c1).con(c2).prog(c3).cnt + 1
    End If
    If loc(c1).con(c2).prog(c3).p2 = 1 And FluxTypes.SSCheck1(2).Value Then
      vaSpread2.col = 15
      vaSpread2.ColHidden = False
      loc(c1).con(c2).prog(c3).cnt = loc(c1).con(c2).prog(c3).cnt + 1
    End If
    If loc(c1).con(c2).prog(c3).p3 = 1 And FluxTypes.SSCheck1(3).Value Then
      vaSpread2.col = 16
      vaSpread2.ColHidden = False
      loc(c1).con(c2).prog(c3).cnt = loc(c1).con(c2).prog(c3).cnt + 1
    End If
  End If
End Sub

Private Sub hidecolumns(spread As Object)
  Dim i As Long
  For i = 2 To 16
    spread.col = i
    spread.ColHidden = True
  Next
 End Sub

Private Sub getparencells()
  Dim col As Long
  Dim Row As Long
  Dim i As Long
  Dim j As Long
  Dim k As Long
  Dim rc As Long
  
  If loadng Then Exit Sub
  If Conc1.ListIndex < 0 Then Exit Sub
  If Conc2.ListIndex < 0 Then Exit Sub
  k = loc(c1).con(c2).use
  
' save time series data
  vaSpread1.col = 1
  vaSpread1.Row = 1
  con(k).uunit = vaSpread1.TypeComboBoxString
  con(k).tsdcnt = 0
  loc(c1).con(c2).ref = ref(10).Tag
  rc = vaSpread1.DataRowCnt
  ReDim con(k).tsd(rc) As Double
  For Row = 2 To rc
    vaSpread1.Row = Row
    If vaSpread1.Text = "" Then
      Exit For
    Else
      con(k).tsdcnt = Row - 1
      con(k).tsd(Row - 1) = Val(vaSpread1.Value)
    End If
  Next
  ReDim Preserve con(k).tsd(Row - 1) As Double
' sync up time series to all locations
  For i = 1 To numloc
    If loc(i).use > 0 Then
      For j = 1 To 15
        loc(i).con(c2).out(j).cnt = con(k).tsdcnt
        ReDim Preserve loc(i).con(c2).out(j).valu(con(k).tsdcnt) As Double
      Next
    End If
  Next
  
  For Row = 1 To con(k).tsdcnt + 1
    vaSpread1.Row = Row
    For col = 2 To 16
      vaSpread1.col = col
      If Row = 1 Then
        If vaSpread1.TypeComboBoxString <> "" Then loc(c1).con(c2).out(col - 1).uunit = vaSpread1.Text
      Else
        loc(c1).con(c2).out(col - 1).valu(Row - 1) = Val(vaSpread1.Value)
      End If
    Next
  Next
End Sub

Private Sub getprogcells()
  Dim col As Long
  Dim Row As Long
  Dim i As Long
  Dim j As Long
  Dim k As Long
  Dim l As Long
  Dim rc As Long

  If loadng Then Exit Sub
  If Conc1.ListIndex < 0 Then Exit Sub
  If Conc2.ListIndex < 0 Then Exit Sub
  If Conc3.ListIndex < 0 Then Exit Sub
  k = loc(c1).con(c2).use
  l = loc(c1).con(c2).prog(c3).use

' save time series data
  vaSpread2.col = 1
  vaSpread2.Row = 1
  con(k).prog(l).uunit = vaSpread2.TypeComboBoxString
  con(k).prog(l).tsdcnt = 0
  loc(c1).con(c2).prog(c3).ref = ref(9).Tag
  rc = vaSpread2.DataRowCnt
  ReDim con(k).prog(l).tsd(rc) As Double
  For Row = 2 To rc
    vaSpread2.Row = Row
    If vaSpread2.Text = "" Then
      Exit For
    Else
      con(k).prog(l).tsdcnt = Row - 1
      con(k).prog(l).tsd(Row - 1) = Val(vaSpread2.Value)
    End If
  Next
  ReDim Preserve con(k).prog(l).tsd(Row - 1) As Double
' sync up time series to all locations
  For i = 1 To numloc
    If loc(i).use > 0 Then
      For j = 1 To 15
        loc(i).con(c2).prog(c3).out(j).cnt = con(k).prog(l).tsdcnt
        ReDim Preserve loc(i).con(c2).prog(c3).out(j).valu(con(k).prog(l).tsdcnt) As Double
      Next
    End If
  Next
  
  For Row = 1 To con(k).prog(l).tsdcnt + 1
    vaSpread2.Row = Row
    For col = 2 To 16
      vaSpread2.col = col
      If Row = 1 Then
        If vaSpread1.TypeComboBoxString <> "" Then loc(c1).con(c2).prog(c3).out(col - 1).uunit = vaSpread1.Text
      Else
        loc(c1).con(c2).prog(c3).out(col - 1).valu(Row - 1) = Val(vaSpread2.Value)
      End If
    Next
  Next
End Sub

Private Sub putparencells()
  Dim i As Long
  Dim j As Long
  Dim k As Long
  
  If Conc1.ListIndex < 0 Then Exit Sub
  If Conc2.ListIndex < 0 Then Exit Sub
  vaSpread1.Visible = False
  k = Conc2.ListIndex + 1
  ref(10).Caption = "Ref: " + Str(loc(c1).con(c2).ref)
  ref(10).Tag = loc(c1).con(c2).ref
  clearcells vaSpread1
  vaSpread1.col = 1
  vaSpread1.Row = 1
  SetSpreadUnits vaSpread1, 1, con(k).uunit
  vaSpread1.TypeComboBoxEditable = True
  vaSpread1.Text = con(k).uunit
  vaSpread1.TypeComboBoxEditable = False
  For j = 2 To 16
    SetSpreadUnits vaSpread1, j, loc(c1).con(c2).out(j - 1).uunit
    vaSpread1.TypeComboBoxEditable = True
    vaSpread1.Text = loc(c1).con(c2).out(j - 1).uunit
    vaSpread1.TypeComboBoxEditable = False
  Next
  For j = 1 To con(k).tsdcnt
    vaSpread1.Row = j + 1
    vaSpread1.col = 1
    vaSpread1.Text = con(k).tsd(j)
    For i = 2 To 16
      vaSpread1.col = i
      vaSpread1.Text = Format(loc(c1).con(c2).out(i - 1).valu(j), CVTFormat)
    Next
  Next
  vaSpread1.Visible = True
End Sub

Private Sub putprogcells()
  Dim i As Long
  Dim j As Long
  Dim k As Long
  Dim l As Long

  If Conc1.ListIndex < 0 Then Exit Sub
  If Conc2.ListIndex < 0 Then Exit Sub
  If Conc3.ListIndex < 0 Then Exit Sub
  vaSpread2.Visible = False
  k = Conc2.ListIndex + 1
  l = Conc3.ListIndex + 1
  ref(9).Caption = "Ref: " + Str(loc(c1).con(c2).prog(c3).ref)
  ref(9).Tag = loc(c1).con(c2).prog(c3).ref
  clearcells vaSpread2
  vaSpread2.col = 1
  vaSpread2.Row = 1
  SetSpreadUnits vaSpread2, 1, con(k).prog(l).uunit
  vaSpread2.TypeComboBoxEditable = True
  vaSpread2.Text = Format(con(k).prog(l).uunit, CVTFormat)
  vaSpread2.TypeComboBoxEditable = False
  For j = 2 To 16
    SetSpreadUnits vaSpread2, j, loc(c1).con(c2).prog(c3).out(j - 1).uunit
    vaSpread2.TypeComboBoxEditable = True
    vaSpread2.Text = Format(loc(c1).con(c2).prog(c3).out(j - 1).uunit, CVTFormat)
    vaSpread2.TypeComboBoxEditable = False
  Next
  For j = 1 To con(k).prog(l).tsdcnt
    vaSpread2.Row = j + 1
    vaSpread2.col = 1
    vaSpread2.Text = con(k).prog(l).tsd(j)
    For i = 2 To 16
      vaSpread2.col = i
      vaSpread2.Text = Format(loc(c1).con(c2).prog(c3).out(i - 1).valu(j), CVTFormat)
    Next
  Next
  If prog.Checked Then vaSpread2.Visible = True
End Sub

Private Sub clearcells(spread As Object)
  spread.BlockMode = True
  spread.col = 1
  spread.Row = 2
  spread.Col2 = 16
  spread.Row2 = 500
  spread.Action = 3
  spread.BlockMode = False
End Sub

Private Sub Conc1_Click()
  getparencells
  If loc(c1).con(c2).numprog > 0 Then getprogcells
  c1 = Conc1.ItemData(Conc1.ListIndex)
  putparencells
  ShowParentCol
  If con(Conc2.ListIndex + 1).numprog > 0 Then
    putprogcells
    ShowProgenyCol
  End If
End Sub

Private Sub Conc2_Click()
Dim i As Long
Dim j As Long

  getparencells
  If loc(c1).con(c2).numprog > 0 Then getprogcells
  c2 = Conc2.ItemData(Conc2.ListIndex)
  putparencells
  ShowParentCol
  If con(Conc2.ListIndex + 1).numprog > 0 Then
    Conc3.Clear
    For i = 1 To con(Conc2.ListIndex + 1).numprog
    Conc3.AddItem con(Conc2.ListIndex + 1).prog(i).Name
      For j = 1 To loc(c1).con(c2).numprog
        If loc(c1).con(c2).prog(j).use = i Then Conc3.ItemData(i - 1) = j
      Next
    Next
    loadng = True
    Conc3.ListIndex = 0
    loadng = False
  End If
  showconcprog
End Sub

Private Sub Conc3_click()
  getprogcells
  c3 = Conc3.ItemData(Conc3.ListIndex)
  putprogcells
  ShowProgenyCol
End Sub

Private Sub leave_Click()
  close_csv errfile
  Kill RunName & ".ERR"
  End
End Sub

Private Sub Form_Unload(Cancel As Integer)
  Dim answer As Long
  If Not called Then
    answer = MsgBox("Do you want to save changes?", 51, "FRAMES Known Air Concentrations")
    If answer = 6 Then save_Click
    If answer = 7 Then leave_Click
    If answer = 2 Then Cancel = 1
  End If
End Sub

Private Sub save_Click()
  Dim ii As Long
  Dim i As Long, j As Long, k As Long
  Dim l As Long, m As Long
  
  Dim fName As String
  Dim parm As parmrec
  Dim fle As parmfile
  Dim UserFormat As String
  UserFormat = CVTFormat
  
  SSTab1_Click 0
  If SSTab1.Tab = 0 Then
    ShowParentCol
    If prog.Checked Then ShowProgenyCol
  End If
  If SSTab1.Tab = 1 Then Conc1_Click
  fName = RunName & ".GID"
  If open_parm(fle, fName, 1) Then
    set_parm parm, "CVTFormat", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CVTFormat
    write_parmrec fle, parm
    CVTFormat = "General Number"
    
    'write flux selections
    For j = 0 To 3
      set_parm parm, "fluxtypes", j, 0, 0, 0, 0, 0, 0, "N/A", "N/A", FluxTypes.SSCheck1(j).Value
      write_parmrec fle, parm
    Next
    
    'write gas flux types
    k = 0
    If FluxTypes.SSCheck1(0).Enabled And FluxTypes.SSCheck1(0).Value Then
      set_parm parm, FluxTypes.lbl(k).Tag, 1, 0, 0, 0, 0, 0, FluxTypes.ref(k).Tag, FluxTypes.Unit(k).Text, FluxTypes.Unit(k).Tag, convert(FluxTypes.Unit(k).Text, FluxTypes.Unit(k).Tag, Val(FluxTypes.txt(k).Text))
      write_parmrec fle, parm
      k = k + 1
      set_parm parm, FluxTypes.lbl(k).Tag, 1, 0, 0, 0, 0, 0, FluxTypes.ref(k).Tag, FluxTypes.Unit(k).Text, FluxTypes.Unit(k).Tag, convert(FluxTypes.Unit(k).Text, FluxTypes.Unit(k).Tag, Val(FluxTypes.txt(k).Text))
      write_parmrec fle, parm
    End If
    
    'write particle flux types
    k = 2
    For j = 1 To 3
      If FluxTypes.SSCheck1(j).Enabled And FluxTypes.SSCheck1(j).Value Then
        set_parm parm, FluxTypes.lbl(k).Tag, j, 0, 0, 0, 0, 0, FluxTypes.ref(k).Tag, FluxTypes.Unit(k).Text, FluxTypes.Unit(k).Tag, convert(FluxTypes.Unit(k).Text, FluxTypes.Unit(k).Tag, Val(FluxTypes.txt(k).Text))
        write_parmrec fle, parm
        k = k + 1
        set_parm parm, FluxTypes.lbl(k).Tag, j, 0, 0, 0, 0, 0, FluxTypes.ref(k).Tag, FluxTypes.Unit(k).Text, FluxTypes.Unit(k).Tag, convert(FluxTypes.Unit(k).Text, FluxTypes.Unit(k).Tag, Val(FluxTypes.txt(k).Text))
        write_parmrec fle, parm
        k = k + 1
      Else
        k = k + 2
      End If
    Next
    
    If Option4(1).Value Then
      set_parm parm, "Settype", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "chronic"
    Else
      set_parm parm, "Settype", yr, mnth, dy, hr, mnt, 0, 0, "N/A", "N/A", "acute"
    End If
    write_parmrec fle, parm
    set_parm parm, "showprog", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CStr(prog.Checked)
    write_parmrec fle, parm
    For i = 1 To numcon
      set_parm parm, "casid", i, 0, 0, 0, 0, 0, 0, "N/A", "N/A", con(i).cas
      write_sparmrec fle, parm
      Check_NTS con(i).tsdcnt, con(i).Name & " number of parent time sequences less than two"
      prevtime = -1
      For j = 1 To con(i).tsdcnt
        Is_Ascending con(i).tsd(j), con(i).Name & " flux out of sequence"
        set_parm parm, "times", i, 0, j, 0, 0, 0, 0, con(i).uunit, con(i).cunit, convert(con(i).uunit, con(i).cunit, con(i).tsd(j))
        write_parmrec fle, parm
      Next
      For j = 1 To con(i).numprog
        set_parm parm, "casid", i, j, 0, 0, 0, 0, 0, "N/A", "N/A", con(i).prog(j).cas
        write_sparmrec fle, parm
        If prog.Checked = True Then
            Check_NTS con(i).prog(j).tsdcnt, con(i).prog(j).Name & " number of progeny time sequences less than two"
        End If
        prevtime = -1
        For k = 1 To con(i).prog(j).tsdcnt
          Is_Ascending con(i).prog(j).tsd(k), con(i).prog(j).Name & " flux out of sequence"
          set_parm parm, "times", i, j, k, 0, 0, 0, 0, con(i).prog(j).uunit, con(i).prog(j).cunit, convert(con(i).prog(j).uunit, con(i).prog(j).cunit, con(i).prog(j).tsd(k))
          write_parmrec fle, parm
        Next
      Next
    Next
    ii = 0
    For i = 1 To numloc
      If loc(i).use > 0 Then
        ii = ii + 1
        set_parm parm, "locname", ii, 0, 0, 0, 0, 0, 0, "N/A", "N/A", loc(i).Name
        write_parmrec fle, parm
        For j = 1 To numcon
          For k = 1 To loc(i).numparent
            If j = loc(i).con(k).use Then
              WriteOutputs fle, parm, loc(i).con(k), ii, j, 0
              If con(j).numprog > 0 Then
                For l = 1 To con(j).numprog
                  For m = 1 To loc(i).con(k).numprog
                    If l = loc(i).con(k).prog(m).use Then
                      WriteProgOutputs fle, parm, loc(i).con(k).prog(m), ii, j, l
                    End If
                  Next
                Next
              End If
            End If
          Next
        Next
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

Private Sub Is_Ascending(time As Double, errmsg As String)
  If prevtime >= time Then
    prevtime = time
    PutError errmsg
  Else
    prevtime = time
  End If
End Sub

Private Sub Check_NTS(nts As Long, errmsg As String)
  If (nts < 2) Then PutError errmsg
End Sub


Private Sub WtConc(a$, b$, c$, funit$, Unit$, valu#, i&, file As csv)
  put_sval file, a
  put_sval file, b
  put_sval file, c
  put_sval file, Unit
  put_val file, 1
  put_sval file, "m"
  put_val file, 1
  put_sval file, "m"
  put_line file
  put_sval file, loc(i).Name
  put_line file
  put_val file, loc(i).x
  put_line file
  put_val file, loc(i).y
  put_line file
  put_val file, -99
  put_val file, convert(funit, Unit, valu)
  put_line file
End Sub

Private Sub WrtOutputs(l As Long, i As Long, funit As String, Unit As String, valu As Double, file As csv, _
                       ext&, dep&, air&, gas&, p1&, p2&, p3&, wet As Boolean, dry As Boolean, tot As Boolean)
  Select Case l
    Case 0:  If ext > 0 Then WtConc "External Dose", "", "", funit, Unit, valu, i, file
    Case 1:  If FluxTypes.SSCheck1(0).Value And dep And gas > 0 Then WtConc "Deposition Rate", "Gas 1", "", funit, Unit, valu, i, file
    Case 2:  If FluxTypes.SSCheck1(1).Value And dep And p1 > 0 And wet Then WtConc "Deposition Rate", "Particle 1", "wet", funit, Unit, valu, i, file
    Case 3:  If FluxTypes.SSCheck1(1).Value And dep And p1 > 0 And dry Then WtConc "Deposition Rate", "Particle 1", "dry", funit, Unit, valu, i, file
    Case 4:  If FluxTypes.SSCheck1(1).Value And dep And p1 > 0 And tot Then WtConc "Deposition Rate", "Particle 1", "total", funit, Unit, valu, i, file
    Case 5:  If FluxTypes.SSCheck1(2).Value And dep And p2 > 0 And wet Then WtConc "Deposition Rate", "Particle 2", "wet", funit, Unit, valu, i, file
    Case 6:  If FluxTypes.SSCheck1(2).Value And dep And p2 > 0 And dry Then WtConc "Deposition Rate", "Particle 2", "dry", funit, Unit, valu, i, file
    Case 7:  If FluxTypes.SSCheck1(2).Value And dep And p2 > 0 And tot Then WtConc "Deposition Rate", "Particle 2", "total", funit, Unit, valu, i, file
    Case 8:  If FluxTypes.SSCheck1(3).Value And dep And p3 > 0 And wet Then WtConc "Deposition Rate", "Particle 3", "wet", funit, Unit, valu, i, file
    Case 9:  If FluxTypes.SSCheck1(3).Value And dep And p3 > 0 And dry Then WtConc "Deposition Rate", "Particle 3", "dry", funit, Unit, valu, i, file
    Case 10: If FluxTypes.SSCheck1(3).Value And dep And p3 > 0 And tot Then WtConc "Deposition Rate", "Particle 3", "total", funit, Unit, valu, i, file
    Case 11: If FluxTypes.SSCheck1(0).Value And air > 0 And gas > 0 Then WtConc "Air Concentration", "Gas 1", "", funit, Unit, valu, i, file
    Case 12: If FluxTypes.SSCheck1(1).Value And air > 0 And p1 > 0 Then WtConc "Air Concentration", "Particle 1", "", funit, Unit, valu, i, file
    Case 13: If FluxTypes.SSCheck1(2).Value And air > 0 And p2 > 0 Then WtConc "Air Concentration", "Particle 2", "", funit, Unit, valu, i, file
    Case 14: If FluxTypes.SSCheck1(3).Value And air > 0 And p3 > 0 Then WtConc "Air Concentration", "Particle 3", "", funit, Unit, valu, i, file
  End Select
End Sub

Private Sub make_ATO()
  Dim i As Long
  Dim j As Long
  Dim k As Long
  Dim l As Long
  Dim m As Long
  Dim file As csv
  Dim cntr As Long
  Dim Ok(3) As Boolean
  
  cntr = 0
  For j = 0 To 3
    If FluxTypes.SSCheck1(j).Enabled And FluxTypes.SSCheck1(j).Value Then
      cntr = cntr + 1
      Ok(j) = True
    End If
  Next
  
  If open_csv(file, RunName & ".ATO", 1) Then
    PutHeader file
    put_val file, numloc
    put_line file
    
    'Location loop
    For i = 1 To numloc
      put_val file, cntr
      put_sval file, modName         'was loc(i).name
      put_line file
      
      k = 0
      For j = 0 To 3
        If Ok(j) Then
          put_val file, FluxTypes.SSCheck1(j).Tag
          put_val file, convert(FluxTypes.Unit(k + 1).Text, FluxTypes.Unit(k + 1).Tag, Val(FluxTypes.txt(k + 1).Text))
          put_val file, FluxTypes.Unit(k + 1).Tag
          put_val file, convert(FluxTypes.Unit(k).Text, FluxTypes.Unit(k).Tag, Val(FluxTypes.txt(k).Text))
          put_val file, FluxTypes.Unit(k).Tag
          file.putbuff = file.putbuff + ","
          put_line file
        End If
        k = k + 2
      Next

      If Option4(1) Then
        put_sval file, "chronic"
      Else
        put_sval file, "acute"
      End If
      put_sval file, "cartesian"
      put_sval file, "points"
      put_val file, numcon
      If Option4(0) Then                  'if acute
        put_val file, Val(yr.Caption)
        put_val file, Val(mnth.Caption)
        put_val file, Val(dy.Caption)
        put_val file, Val(hr.Caption)
        put_val file, Val(mnt.Caption)
      End If
      put_line file
 
      'Constituent loop
      For j = 1 To numcon
        put_sval file, con(j).Name
        put_sval file, con(j).cas
        put_val file, con(j).tsdcnt
        If prog.Checked Then
          put_val file, con(j).numprog
        Else
          put_val file, 0
        End If
        put_line file
        For k = 1 To con(j).tsdcnt
          put_val file, con(j).tsd(k)
          put_sval file, con(j).uunit
          put_val file, loc(i).con(j).cnt
          put_line file
          For l = 0 To 14
            WrtOutputs l, i, loc(i).con(j).out(l + 1).uunit, loc(i).con(j).out(l + 1).cunit, loc(i).con(j).out(l + 1).valu(k), file, _
                       loc(i).con(j).ext, loc(i).con(j).dep, loc(i).con(j).air, loc(i).con(j).gas, loc(i).con(j).p1, _
                       loc(i).con(j).p2, loc(i).con(j).p3, loc(i).con(j).wet, loc(i).con(j).dry, loc(i).con(j).tot
          Next
        Next
        'Progeny loop
        If prog.Checked Then
          If loc(i).con(j).numprog > 0 Then
            For k = 1 To loc(i).con(j).numprog
              put_sval file, con(j).prog(k).Name
              put_sval file, con(j).prog(k).cas
              put_val file, con(j).prog(k).tsdcnt
              put_sval file, con(j).Name
              put_sval file, con(j).cas
              put_line file
              For l = 1 To con(j).prog(k).tsdcnt
                put_val file, con(j).prog(k).tsd(l)
                put_sval file, con(j).prog(k).uunit
                put_val file, loc(i).con(j).prog(k).cnt
                put_line file
                For m = 0 To 14
                    WrtOutputs m, i, loc(i).con(j).out(l + 1).uunit, loc(i).con(j).prog(k).out(m + 1).cunit, loc(i).con(j).prog(k).out(m + 1).valu(l), file, _
                         loc(i).con(j).prog(k).ext, loc(i).con(j).prog(k).dep, loc(i).con(j).prog(k).air, loc(i).con(j).prog(k).gas, loc(i).con(j).prog(k).p1, _
                         loc(i).con(j).prog(k).p2, loc(i).con(j).prog(k).p3, loc(i).con(j).prog(k).wet, loc(i).con(j).prog(k).dry, loc(i).con(j).prog(k).tot
                Next
              Next
            Next
          End If
        End If
      Next
NextLoc:
    Next
    close_csv file
  End If
  
End Sub

Private Sub SSCommand8_GotFocus()
  HelpAnchor = SSCommand8.Tag
End Sub

Private Sub SSCommand9_Click()
  If Des1.ListIndex > 0 Then Des1.ListIndex = Des1.ListIndex - 1
End Sub

Private Sub SSCommand9_GotFocus()
  HelpAnchor = SSCommand9.Tag
End Sub

Private Sub SSTab1_Click(PreviousTab As Integer)
  If loadng = True Then Exit Sub
  If SSTab1.Tab = 0 Then Des1_Click
  If SSTab1.Tab = 1 Or PreviousTab = 1 Then Conc1_Click
  SSTab1_GotFocus
End Sub

Private Sub SSTab1_GotFocus()
  SSFrame1.Enabled = False
  SSFrame2.Enabled = False
  If SSTab1.Tab = 0 Then
    HelpAnchor = "constituent"
    SSFrame1.Enabled = True
    reference.Enabled = False
    RefItem = -1
  Else
    HelpAnchor = "concentrations"
    reference.Enabled = True
    SSFrame2.Enabled = True
    RefItem = 10
  End If
End Sub

Private Sub Conc3_GotFocus()
  RefItem = 9
  HelpAnchor = Conc3.Tag
End Sub

Private Sub Des2_GotFocus()
  HelpAnchor = Des2.Tag
End Sub

Private Sub Des3_GotFocus()
  HelpAnchor = Des3.Tag
End Sub

Private Sub Conc2_GotFocus()
  RefItem = 10
  HelpAnchor = Conc2.Tag
End Sub

Private Sub UpDown1_GotFocus(Index As Integer)
  HelpAnchor = UpDown1(Index).Tag
End Sub

Private Sub vaSpread1_GotFocus()
  HelpAnchor = vaSpread1.Tag
End Sub

Private Sub vaSpread2_GotFocus()
  HelpAnchor = vaSpread2.Tag
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
