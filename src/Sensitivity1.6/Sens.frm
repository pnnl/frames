VERSION 5.00
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "tabctl32.ocx"
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "threed32.ocx"
Begin VB.Form Sens 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "SensitivityUI"
   ClientHeight    =   6312
   ClientLeft      =   8640
   ClientTop       =   7392
   ClientWidth     =   7716
   ControlBox      =   0   'False
   BeginProperty Font 
      Name            =   "Courier New"
      Size            =   9
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "Sens.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   526
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   643
   StartUpPosition =   2  'CenterScreen
   Begin Threed.SSPanel tPanel 
      Height          =   2892
      Left            =   240
      TabIndex        =   54
      Top             =   2280
      Visible         =   0   'False
      Width           =   7212
      _Version        =   65536
      _ExtentX        =   12721
      _ExtentY        =   5101
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
      BevelWidth      =   4
      BorderWidth     =   2
      BevelInner      =   1
      Begin VB.CommandButton Command 
         Caption         =   "Ok"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   348
         Index           =   2
         Left            =   5160
         TabIndex        =   66
         Top             =   300
         Width           =   1000
      End
      Begin VB.ComboBox yrCnt 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   288
         ItemData        =   "Sens.frx":030A
         Left            =   3840
         List            =   "Sens.frx":031D
         Style           =   2  'Dropdown List
         TabIndex        =   65
         Top             =   300
         Visible         =   0   'False
         Width           =   1000
      End
      Begin VB.TextBox txt 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   300
         Index           =   9
         Left            =   2808
         TabIndex        =   64
         Top             =   1920
         Visible         =   0   'False
         Width           =   1000
      End
      Begin VB.ComboBox unt 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   288
         Index           =   9
         Left            =   3840
         Style           =   2  'Dropdown List
         TabIndex        =   63
         Top             =   1920
         Visible         =   0   'False
         Width           =   1000
      End
      Begin VB.TextBox txt 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   300
         Index           =   10
         Left            =   2820
         TabIndex        =   62
         Top             =   2340
         Visible         =   0   'False
         Width           =   1000
      End
      Begin VB.ComboBox unt 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   288
         Index           =   10
         Left            =   3840
         Style           =   2  'Dropdown List
         TabIndex        =   61
         Top             =   2340
         Visible         =   0   'False
         Width           =   1000
      End
      Begin VB.TextBox txt 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   300
         Index           =   7
         Left            =   2808
         TabIndex        =   60
         Top             =   1080
         Visible         =   0   'False
         Width           =   1000
      End
      Begin VB.ComboBox unt 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   288
         Index           =   7
         Left            =   3840
         Style           =   2  'Dropdown List
         TabIndex        =   59
         Top             =   1080
         Visible         =   0   'False
         Width           =   1000
      End
      Begin VB.TextBox txt 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   300
         Index           =   8
         Left            =   2808
         TabIndex        =   58
         Top             =   1500
         Visible         =   0   'False
         Width           =   1000
      End
      Begin VB.ComboBox unt 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   288
         Index           =   8
         Left            =   3840
         Style           =   2  'Dropdown List
         TabIndex        =   57
         Top             =   1500
         Visible         =   0   'False
         Width           =   1000
      End
      Begin VB.TextBox txt 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   300
         Index           =   6
         Left            =   2808
         TabIndex        =   56
         Top             =   660
         Visible         =   0   'False
         Width           =   1000
      End
      Begin VB.ComboBox unt 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   288
         Index           =   6
         Left            =   3840
         Style           =   2  'Dropdown List
         TabIndex        =   55
         Top             =   660
         Visible         =   0   'False
         Width           =   1000
      End
      Begin VB.Label lbl 
         Caption         =   "Label"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   252
         Index           =   9
         Left            =   360
         TabIndex        =   72
         Top             =   1920
         Visible         =   0   'False
         Width           =   2460
      End
      Begin VB.Label lbl 
         Caption         =   "Label"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   252
         Index           =   10
         Left            =   360
         TabIndex        =   71
         Top             =   2340
         Visible         =   0   'False
         Width           =   2460
      End
      Begin VB.Label lbl 
         Caption         =   "Label"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   252
         Index           =   7
         Left            =   360
         TabIndex        =   70
         Top             =   1080
         Visible         =   0   'False
         Width           =   2460
      End
      Begin VB.Label lbl 
         Caption         =   "Label"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   252
         Index           =   8
         Left            =   360
         TabIndex        =   69
         Top             =   1500
         Visible         =   0   'False
         Width           =   2460
      End
      Begin VB.Label lbl 
         Caption         =   "Number of years to monitor"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   252
         Index           =   5
         Left            =   360
         TabIndex        =   68
         Top             =   240
         Width           =   2460
      End
      Begin VB.Label lbl 
         Caption         =   "Label"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   252
         Index           =   6
         Left            =   360
         TabIndex        =   67
         Top             =   660
         Visible         =   0   'False
         Width           =   2460
      End
   End
   Begin TabDlg.SSTab SSTab1 
      Height          =   5772
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   7692
      _ExtentX        =   13568
      _ExtentY        =   10181
      _Version        =   393216
      TabHeight       =   420
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   7.8
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      TabCaption(0)   =   "Variables"
      TabPicture(0)   =   "Sens.frx":0330
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "uFrame(0)"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).ControlCount=   1
      TabCaption(1)   =   "Parameters"
      TabPicture(1)   =   "Sens.frx":034C
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "uFrame(1)"
      Tab(1).ControlCount=   1
      TabCaption(2)   =   "Outputs"
      TabPicture(2)   =   "Sens.frx":0368
      Tab(2).ControlEnabled=   0   'False
      Tab(2).Control(0)=   "uFrame(2)"
      Tab(2).Control(0).Enabled=   0   'False
      Tab(2).ControlCount=   1
      Begin Threed.SSFrame uFrame 
         Height          =   5340
         Index           =   1
         Left            =   -74880
         TabIndex        =   1
         Top             =   300
         Width           =   7500
         _Version        =   65536
         _ExtentX        =   13229
         _ExtentY        =   9419
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
         Begin VB.TextBox pTxt1 
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   300
            Left            =   2304
            TabIndex        =   3
            Tag             =   "ALIAS"
            Top             =   528
            Width           =   5052
         End
         Begin VB.ListBox List2 
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   3504
            Left            =   192
            TabIndex        =   2
            Tag             =   "PARAM"
            Top             =   528
            Width           =   1980
         End
         Begin TabDlg.SSTab SSTab3 
            Height          =   4092
            Left            =   2340
            TabIndex        =   4
            Top             =   1080
            Width           =   5052
            _ExtentX        =   8911
            _ExtentY        =   7218
            _Version        =   393216
            TabHeight       =   420
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            TabCaption(0)   =   "Distribution"
            TabPicture(0)   =   "Sens.frx":0384
            Tab(0).ControlEnabled=   -1  'True
            Tab(0).Control(0)=   "pFrame(0)"
            Tab(0).Control(0).Enabled=   0   'False
            Tab(0).ControlCount=   1
            TabCaption(1)   =   "Correlations"
            TabPicture(1)   =   "Sens.frx":03A0
            Tab(1).ControlEnabled=   0   'False
            Tab(1).Control(0)=   "pFrame(1)"
            Tab(1).ControlCount=   1
            TabCaption(2)   =   "Equation"
            TabPicture(2)   =   "Sens.frx":03BC
            Tab(2).ControlEnabled=   0   'False
            Tab(2).Control(0)=   "pFrame(2)"
            Tab(2).ControlCount=   1
            Begin Threed.SSFrame pFrame 
               Height          =   3612
               Index           =   0
               Left            =   144
               TabIndex        =   5
               Top             =   324
               Width           =   4764
               _Version        =   65536
               _ExtentX        =   8403
               _ExtentY        =   6371
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
               Begin VB.ComboBox base 
                  BeginProperty Font 
                     Name            =   "MS Sans Serif"
                     Size            =   7.8
                     Charset         =   0
                     Weight          =   400
                     Underline       =   0   'False
                     Italic          =   0   'False
                     Strikethrough   =   0   'False
                  EndProperty
                  Height          =   288
                  ItemData        =   "Sens.frx":03D8
                  Left            =   2592
                  List            =   "Sens.frx":03E2
                  Style           =   2  'Dropdown List
                  TabIndex        =   42
                  Tag             =   "DISTBASE"
                  Top             =   2496
                  Width           =   2028
               End
               Begin VB.TextBox txt 
                  BeginProperty Font 
                     Name            =   "MS Sans Serif"
                     Size            =   7.8
                     Charset         =   0
                     Weight          =   400
                     Underline       =   0   'False
                     Italic          =   0   'False
                     Strikethrough   =   0   'False
                  EndProperty
                  Height          =   300
                  Index           =   3
                  Left            =   2592
                  TabIndex        =   14
                  Top             =   2016
                  Width           =   1000
               End
               Begin VB.ComboBox unt 
                  BeginProperty Font 
                     Name            =   "MS Sans Serif"
                     Size            =   7.8
                     Charset         =   0
                     Weight          =   400
                     Underline       =   0   'False
                     Italic          =   0   'False
                     Strikethrough   =   0   'False
                  EndProperty
                  Height          =   288
                  Index           =   3
                  Left            =   3600
                  Style           =   2  'Dropdown List
                  TabIndex        =   13
                  Top             =   2016
                  Width           =   1000
               End
               Begin VB.TextBox txt 
                  BeginProperty Font 
                     Name            =   "MS Sans Serif"
                     Size            =   7.8
                     Charset         =   0
                     Weight          =   400
                     Underline       =   0   'False
                     Italic          =   0   'False
                     Strikethrough   =   0   'False
                  EndProperty
                  Height          =   300
                  Index           =   2
                  Left            =   2592
                  TabIndex        =   12
                  Top             =   1584
                  Width           =   1000
               End
               Begin VB.ComboBox unt 
                  BeginProperty Font 
                     Name            =   "MS Sans Serif"
                     Size            =   7.8
                     Charset         =   0
                     Weight          =   400
                     Underline       =   0   'False
                     Italic          =   0   'False
                     Strikethrough   =   0   'False
                  EndProperty
                  Height          =   288
                  Index           =   2
                  Left            =   3600
                  Style           =   2  'Dropdown List
                  TabIndex        =   11
                  Top             =   1584
                  Width           =   1000
               End
               Begin VB.TextBox txt 
                  BeginProperty Font 
                     Name            =   "MS Sans Serif"
                     Size            =   7.8
                     Charset         =   0
                     Weight          =   400
                     Underline       =   0   'False
                     Italic          =   0   'False
                     Strikethrough   =   0   'False
                  EndProperty
                  Height          =   300
                  Index           =   1
                  Left            =   2592
                  TabIndex        =   10
                  Top             =   1152
                  Width           =   1000
               End
               Begin VB.ComboBox unt 
                  BeginProperty Font 
                     Name            =   "MS Sans Serif"
                     Size            =   7.8
                     Charset         =   0
                     Weight          =   400
                     Underline       =   0   'False
                     Italic          =   0   'False
                     Strikethrough   =   0   'False
                  EndProperty
                  Height          =   288
                  Index           =   1
                  Left            =   3600
                  Style           =   2  'Dropdown List
                  TabIndex        =   9
                  Top             =   1152
                  Width           =   1000
               End
               Begin VB.TextBox txt 
                  BeginProperty Font 
                     Name            =   "MS Sans Serif"
                     Size            =   7.8
                     Charset         =   0
                     Weight          =   400
                     Underline       =   0   'False
                     Italic          =   0   'False
                     Strikethrough   =   0   'False
                  EndProperty
                  Height          =   300
                  Index           =   0
                  Left            =   2592
                  TabIndex        =   8
                  Top             =   720
                  Width           =   1000
               End
               Begin VB.ComboBox unt 
                  BeginProperty Font 
                     Name            =   "MS Sans Serif"
                     Size            =   7.8
                     Charset         =   0
                     Weight          =   400
                     Underline       =   0   'False
                     Italic          =   0   'False
                     Strikethrough   =   0   'False
                  EndProperty
                  Height          =   288
                  Index           =   0
                  Left            =   3600
                  Style           =   2  'Dropdown List
                  TabIndex        =   7
                  Top             =   720
                  Width           =   1000
               End
               Begin VB.ComboBox distrib 
                  BeginProperty Font 
                     Name            =   "MS Sans Serif"
                     Size            =   7.8
                     Charset         =   0
                     Weight          =   400
                     Underline       =   0   'False
                     Italic          =   0   'False
                     Strikethrough   =   0   'False
                  EndProperty
                  Height          =   288
                  ItemData        =   "Sens.frx":03F7
                  Left            =   1248
                  List            =   "Sens.frx":040A
                  Style           =   2  'Dropdown List
                  TabIndex        =   6
                  Tag             =   "DIST"
                  Top             =   300
                  Width           =   3372
               End
               Begin VB.Label lbl 
                  Caption         =   "Log Base"
                  BeginProperty Font 
                     Name            =   "MS Sans Serif"
                     Size            =   7.8
                     Charset         =   0
                     Weight          =   400
                     Underline       =   0   'False
                     Italic          =   0   'False
                     Strikethrough   =   0   'False
                  EndProperty
                  Height          =   252
                  Index           =   4
                  Left            =   144
                  TabIndex        =   43
                  Top             =   2496
                  Width           =   1116
               End
               Begin VB.Label lbl 
                  Caption         =   "Label"
                  BeginProperty Font 
                     Name            =   "MS Sans Serif"
                     Size            =   7.8
                     Charset         =   0
                     Weight          =   400
                     Underline       =   0   'False
                     Italic          =   0   'False
                     Strikethrough   =   0   'False
                  EndProperty
                  Height          =   252
                  Index           =   3
                  Left            =   144
                  TabIndex        =   19
                  Top             =   2016
                  Width           =   2460
               End
               Begin VB.Label lbl 
                  Caption         =   "Label"
                  BeginProperty Font 
                     Name            =   "MS Sans Serif"
                     Size            =   7.8
                     Charset         =   0
                     Weight          =   400
                     Underline       =   0   'False
                     Italic          =   0   'False
                     Strikethrough   =   0   'False
                  EndProperty
                  Height          =   252
                  Index           =   2
                  Left            =   144
                  TabIndex        =   18
                  Top             =   1584
                  Width           =   2460
               End
               Begin VB.Label lbl 
                  Caption         =   "Label"
                  BeginProperty Font 
                     Name            =   "MS Sans Serif"
                     Size            =   7.8
                     Charset         =   0
                     Weight          =   400
                     Underline       =   0   'False
                     Italic          =   0   'False
                     Strikethrough   =   0   'False
                  EndProperty
                  Height          =   252
                  Index           =   1
                  Left            =   144
                  TabIndex        =   17
                  Top             =   1152
                  Width           =   2460
               End
               Begin VB.Label lbl 
                  Caption         =   "Label"
                  BeginProperty Font 
                     Name            =   "MS Sans Serif"
                     Size            =   7.8
                     Charset         =   0
                     Weight          =   400
                     Underline       =   0   'False
                     Italic          =   0   'False
                     Strikethrough   =   0   'False
                  EndProperty
                  Height          =   252
                  Index           =   0
                  Left            =   144
                  TabIndex        =   16
                  Top             =   720
                  Width           =   2460
               End
               Begin VB.Label vLbl 
                  Caption         =   "Type"
                  BeginProperty Font 
                     Name            =   "MS Sans Serif"
                     Size            =   7.8
                     Charset         =   0
                     Weight          =   400
                     Underline       =   0   'False
                     Italic          =   0   'False
                     Strikethrough   =   0   'False
                  EndProperty
                  Height          =   252
                  Index           =   4
                  Left            =   144
                  TabIndex        =   15
                  Top             =   288
                  Width           =   1116
               End
            End
            Begin Threed.SSFrame pFrame 
               Height          =   3612
               Index           =   1
               Left            =   -74856
               TabIndex        =   24
               Top             =   324
               Width           =   4764
               _Version        =   65536
               _ExtentX        =   8403
               _ExtentY        =   6371
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
               Begin VB.CommandButton Command 
                  Caption         =   "Delete"
                  BeginProperty Font 
                     Name            =   "MS Sans Serif"
                     Size            =   7.8
                     Charset         =   0
                     Weight          =   400
                     Underline       =   0   'False
                     Italic          =   0   'False
                     Strikethrough   =   0   'False
                  EndProperty
                  Height          =   348
                  Index           =   5
                  Left            =   1764
                  TabIndex        =   35
                  Tag             =   "CORDEL"
                  Top             =   1188
                  Width           =   708
               End
               Begin VB.CommandButton Command 
                  Caption         =   "Add"
                  BeginProperty Font 
                     Name            =   "MS Sans Serif"
                     Size            =   7.8
                     Charset         =   0
                     Weight          =   400
                     Underline       =   0   'False
                     Italic          =   0   'False
                     Strikethrough   =   0   'False
                  EndProperty
                  Height          =   348
                  Index           =   4
                  Left            =   972
                  TabIndex        =   34
                  Tag             =   "CORADD"
                  Top             =   1188
                  Width           =   708
               End
               Begin VB.ComboBox alias 
                  BeginProperty Font 
                     Name            =   "MS Sans Serif"
                     Size            =   7.8
                     Charset         =   0
                     Weight          =   400
                     Underline       =   0   'False
                     Italic          =   0   'False
                     Strikethrough   =   0   'False
                  EndProperty
                  Height          =   288
                  Left            =   144
                  Style           =   2  'Dropdown List
                  TabIndex        =   31
                  Tag             =   "CORALIAS"
                  Top             =   528
                  Width           =   2328
               End
               Begin VB.ListBox List3 
                  BeginProperty Font 
                     Name            =   "MS Sans Serif"
                     Size            =   7.8
                     Charset         =   0
                     Weight          =   400
                     Underline       =   0   'False
                     Italic          =   0   'False
                     Strikethrough   =   0   'False
                  EndProperty
                  Height          =   1392
                  Left            =   2592
                  TabIndex        =   30
                  Tag             =   "CORCURRENT"
                  Top             =   528
                  Width           =   1980
               End
               Begin VB.TextBox pTxt2 
                  BeginProperty Font 
                     Name            =   "MS Sans Serif"
                     Size            =   7.8
                     Charset         =   0
                     Weight          =   400
                     Underline       =   0   'False
                     Italic          =   0   'False
                     Strikethrough   =   0   'False
                  EndProperty
                  Height          =   300
                  Left            =   144
                  TabIndex        =   29
                  Tag             =   "CORCOR"
                  Top             =   1200
                  Width           =   708
               End
               Begin VB.Label Label1 
                  Caption         =   $"Sens.frx":043E
                  BeginProperty Font 
                     Name            =   "MS Sans Serif"
                     Size            =   7.8
                     Charset         =   0
                     Weight          =   700
                     Underline       =   0   'False
                     Italic          =   0   'False
                     Strikethrough   =   0   'False
                  EndProperty
                  Height          =   1788
                  Left            =   192
                  TabIndex        =   74
                  Top             =   1680
                  Width           =   2268
               End
               Begin VB.Label vLbl 
                  Caption         =   "Current Correlations"
                  BeginProperty Font 
                     Name            =   "MS Sans Serif"
                     Size            =   7.8
                     Charset         =   0
                     Weight          =   400
                     Underline       =   0   'False
                     Italic          =   0   'False
                     Strikethrough   =   0   'False
                  EndProperty
                  Height          =   252
                  Index           =   6
                  Left            =   2592
                  TabIndex        =   36
                  Top             =   288
                  Width           =   1980
               End
               Begin VB.Label vLbl 
                  Caption         =   "Correlation"
                  BeginProperty Font 
                     Name            =   "MS Sans Serif"
                     Size            =   7.8
                     Charset         =   0
                     Weight          =   400
                     Underline       =   0   'False
                     Italic          =   0   'False
                     Strikethrough   =   0   'False
                  EndProperty
                  Height          =   252
                  Index           =   7
                  Left            =   144
                  TabIndex        =   33
                  Top             =   960
                  Width           =   2172
               End
               Begin VB.Label vLbl 
                  Caption         =   "Alias"
                  BeginProperty Font 
                     Name            =   "MS Sans Serif"
                     Size            =   7.8
                     Charset         =   0
                     Weight          =   400
                     Underline       =   0   'False
                     Italic          =   0   'False
                     Strikethrough   =   0   'False
                  EndProperty
                  Height          =   252
                  Index           =   5
                  Left            =   144
                  TabIndex        =   32
                  Top             =   288
                  Width           =   2220
               End
            End
            Begin Threed.SSFrame pFrame 
               Height          =   3612
               Index           =   2
               Left            =   -74856
               TabIndex        =   25
               Top             =   336
               Width           =   4764
               _Version        =   65536
               _ExtentX        =   8403
               _ExtentY        =   6371
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
               Begin VB.CommandButton Command 
                  Caption         =   "Delete"
                  BeginProperty Font 
                     Name            =   "MS Sans Serif"
                     Size            =   7.8
                     Charset         =   0
                     Weight          =   400
                     Underline       =   0   'False
                     Italic          =   0   'False
                     Strikethrough   =   0   'False
                  EndProperty
                  Height          =   348
                  Index           =   7
                  Left            =   3564
                  TabIndex        =   45
                  Tag             =   "EQUDEL"
                  Top             =   3072
                  Width           =   1000
               End
               Begin VB.CommandButton Command 
                  Caption         =   "Add"
                  BeginProperty Font 
                     Name            =   "MS Sans Serif"
                     Size            =   7.8
                     Charset         =   0
                     Weight          =   400
                     Underline       =   0   'False
                     Italic          =   0   'False
                     Strikethrough   =   0   'False
                  EndProperty
                  Height          =   348
                  Index           =   6
                  Left            =   2316
                  TabIndex        =   44
                  Tag             =   "EQUADD"
                  Top             =   3072
                  Width           =   1000
               End
               Begin VB.TextBox pTxt3 
                  BeginProperty Font 
                     Name            =   "MS Sans Serif"
                     Size            =   7.8
                     Charset         =   0
                     Weight          =   400
                     Underline       =   0   'False
                     Italic          =   0   'False
                     Strikethrough   =   0   'False
                  EndProperty
                  Height          =   300
                  Left            =   432
                  TabIndex        =   26
                  Tag             =   "EQUEQUATION"
                  Top             =   1296
                  Width           =   4116
               End
               Begin VB.Label pTxt4 
                  BackColor       =   &H00E0E0E0&
                  BorderStyle     =   1  'Fixed Single
                  BeginProperty Font 
                     Name            =   "MS Sans Serif"
                     Size            =   7.8
                     Charset         =   0
                     Weight          =   400
                     Underline       =   0   'False
                     Italic          =   0   'False
                     Strikethrough   =   0   'False
                  EndProperty
                  Height          =   300
                  Left            =   432
                  TabIndex        =   80
                  Tag             =   "EQUEQUATION"
                  Top             =   576
                  Width           =   4116
               End
               Begin VB.Label vLbl 
                  Caption         =   "Current equation"
                  BeginProperty Font 
                     Name            =   "MS Sans Serif"
                     Size            =   7.8
                     Charset         =   0
                     Weight          =   400
                     Underline       =   0   'False
                     Italic          =   0   'False
                     Strikethrough   =   0   'False
                  EndProperty
                  Height          =   252
                  Index           =   12
                  Left            =   192
                  TabIndex        =   53
                  Top             =   336
                  Width           =   4380
               End
               Begin VB.Label vLbl 
                  Caption         =   $"Sens.frx":0511
                  BeginProperty Font 
                     Name            =   "MS Sans Serif"
                     Size            =   7.8
                     Charset         =   0
                     Weight          =   700
                     Underline       =   0   'False
                     Italic          =   0   'False
                     Strikethrough   =   0   'False
                  EndProperty
                  Height          =   1200
                  Index           =   9
                  Left            =   216
                  TabIndex        =   28
                  Top             =   1692
                  Width           =   4344
               End
               Begin VB.Label vLbl 
                  Caption         =   "Enter an equation"
                  BeginProperty Font 
                     Name            =   "MS Sans Serif"
                     Size            =   7.8
                     Charset         =   0
                     Weight          =   400
                     Underline       =   0   'False
                     Italic          =   0   'False
                     Strikethrough   =   0   'False
                  EndProperty
                  Height          =   252
                  Index           =   8
                  Left            =   192
                  TabIndex        =   27
                  Top             =   1008
                  Width           =   4380
               End
            End
         End
         Begin VB.Label vLbl 
            Caption         =   "Input Variable Description"
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   252
            Index           =   3
            Left            =   2304
            TabIndex        =   21
            Top             =   240
            Width           =   2268
         End
         Begin VB.Label vLbl 
            Caption         =   "Alias"
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   252
            Index           =   2
            Left            =   192
            TabIndex        =   20
            Top             =   240
            Width           =   1836
         End
      End
      Begin Threed.SSFrame uFrame 
         Height          =   5340
         Index           =   0
         Left            =   120
         TabIndex        =   22
         Top             =   300
         Width           =   7500
         _Version        =   65536
         _ExtentX        =   13229
         _ExtentY        =   9419
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
         Begin Threed.SSCheck SSCheck1 
            Height          =   255
            Left            =   195
            TabIndex        =   85
            Tag             =   "VarView"
            Top             =   4995
            Width           =   3660
            _Version        =   65536
            _ExtentX        =   6456
            _ExtentY        =   450
            _StockProps     =   78
            Caption         =   "View Aliased Variables"
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   7.54
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
         End
         Begin VB.TextBox vTxt2 
            BackColor       =   &H00E0E0E0&
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   500
            Left            =   1224
            Locked          =   -1  'True
            MultiLine       =   -1  'True
            TabIndex        =   83
            Top             =   648
            Width           =   6060
         End
         Begin VB.CommandButton Command 
            Caption         =   "Delete"
            Enabled         =   0   'False
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   348
            Index           =   1
            Left            =   6096
            TabIndex        =   40
            Tag             =   "VarDel"
            Top             =   264
            Width           =   1200
         End
         Begin VB.CommandButton Command 
            Caption         =   "Add"
            Enabled         =   0   'False
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   348
            Index           =   0
            Left            =   4860
            TabIndex        =   39
            Tag             =   "VarAdd"
            Top             =   264
            Width           =   1200
         End
         Begin VB.TextBox vTxt1 
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   300
            Left            =   1224
            MaxLength       =   9
            TabIndex        =   38
            Tag             =   "VarAlias"
            Top             =   288
            Width           =   2364
         End
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
            Height          =   2700
            Left            =   195
            TabIndex        =   23
            Tag             =   "True"
            Top             =   1485
            Width           =   7116
         End
         Begin VB.Label Label2 
            Caption         =   "Description"
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   228
            Left            =   230
            TabIndex        =   84
            Top             =   684
            Width           =   1020
         End
         Begin VB.Label vLbl 
            Caption         =   "Alias                  Variable Description"
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   255
            Index           =   1
            Left            =   210
            TabIndex        =   41
            Top             =   1275
            Width           =   7050
         End
         Begin VB.Label vLbl 
            Caption         =   "Alias"
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   252
            Index           =   0
            Left            =   230
            TabIndex        =   37
            Top             =   288
            Width           =   996
         End
      End
      Begin Threed.SSFrame uFrame 
         Height          =   5340
         Index           =   2
         Left            =   -74880
         TabIndex        =   46
         Top             =   300
         Width           =   7500
         _Version        =   65536
         _ExtentX        =   13229
         _ExtentY        =   9419
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
         Begin VB.CommandButton Command 
            Caption         =   "Edit Location"
            Enabled         =   0   'False
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   348
            Index           =   3
            Left            =   3960
            TabIndex        =   87
            Tag             =   "OUTTIME"
            Top             =   720
            Width           =   2184
         End
         Begin Threed.SSCheck SSCheck2 
            Height          =   255
            Left            =   225
            TabIndex        =   86
            Tag             =   "OUTVIEW"
            Top             =   4965
            Width           =   3375
            _Version        =   65536
            _ExtentX        =   5953
            _ExtentY        =   450
            _StockProps     =   78
            Caption         =   "View Aliased Outputs"
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   7.9
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
         End
         Begin VB.TextBox oTxt2 
            BackColor       =   &H00E0E0E0&
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   500
            Left            =   1224
            Locked          =   -1  'True
            MultiLine       =   -1  'True
            TabIndex        =   82
            Top             =   1368
            Width           =   6060
         End
         Begin VB.TextBox txt 
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   300
            Index           =   12
            Left            =   2700
            TabIndex        =   77
            Top             =   624
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   300
            Index           =   11
            Left            =   2700
            TabIndex        =   75
            Top             =   240
            Width           =   1000
         End
         Begin VB.CommandButton Command 
            Caption         =   "Edit Time"
            Enabled         =   0   'False
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   348
            Index           =   10
            Left            =   3960
            TabIndex        =   73
            Tag             =   "OUTTIME"
            Top             =   240
            Width           =   2184
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
            Height          =   2700
            ItemData        =   "Sens.frx":05EA
            Left            =   210
            List            =   "Sens.frx":05EC
            TabIndex        =   50
            Top             =   2175
            Width           =   7080
         End
         Begin VB.TextBox oTxt1 
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   300
            Left            =   1224
            MaxLength       =   9
            TabIndex        =   49
            Tag             =   "OUTALIAS"
            Top             =   1008
            Width           =   2460
         End
         Begin VB.CommandButton Command 
            Caption         =   "Add"
            Enabled         =   0   'False
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   348
            Index           =   8
            Left            =   6240
            TabIndex        =   48
            Tag             =   "OUTADD"
            Top             =   240
            Width           =   1100
         End
         Begin VB.CommandButton Command 
            Caption         =   "Delete"
            Enabled         =   0   'False
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   348
            Index           =   9
            Left            =   6240
            TabIndex        =   47
            Tag             =   "OUTDEL"
            Top             =   720
            Width           =   1100
         End
         Begin VB.Label Label3 
            Caption         =   "Description"
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   228
            Left            =   230
            TabIndex        =   81
            Top             =   1404
            Width           =   1020
         End
         Begin VB.Label lbl 
            Caption         =   "Label"
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   252
            Index           =   12
            Left            =   230
            TabIndex        =   78
            Tag             =   "OUTITERATIONS"
            Top             =   660
            Width           =   2460
         End
         Begin VB.Label lbl 
            Caption         =   "Label"
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   252
            Index           =   11
            Left            =   230
            TabIndex        =   76
            Tag             =   "OUTSEED"
            Top             =   276
            Width           =   2460
         End
         Begin VB.Label vLbl 
            Caption         =   "Alias"
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   252
            Index           =   11
            Left            =   230
            TabIndex        =   52
            Top             =   1044
            Width           =   1032
         End
         Begin VB.Label vLbl 
            Caption         =   "Alias                  Output Description"
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   7.8
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   255
            Index           =   10
            Left            =   225
            TabIndex        =   51
            Top             =   1965
            Width           =   7050
         End
      End
   End
   Begin VB.Label ref 
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   7.8
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   312
      Left            =   0
      TabIndex        =   79
      Top             =   0
      Width           =   3072
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
Dim temp As parmrec
Dim fui As parmfile
Dim fui2 As parmfile
Dim s1(6) As String
Dim t1(3) As String
Dim u1(2) As String
Dim tCnt As Long

Dim paths(21) As String

Dim suIter As New tInteger
Dim suSeed As New tInteger

Dim vCt As Long
Dim vCache() As parmrec

Dim gidCt As Long
Dim gidData() As parmrec

Dim fuiCt As Long
Dim fuiData() As parmrec

Private Sub howto_Click()
  HelpAnchor = ""
  GetHelp
End Sub

Private Sub initparm(t As tFloat, n As String, v As Variant, i As Long, k As Long, u As String, tg As String, Optional lo As Variant, Optional hi As Variant)
  t.SetObj lbl(i), txt(i), unt(i), ref
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
      t.lower = Val(lo)
    End If
  End If
  If Not IsMissing(hi) Then
    If VarType(hi) = vbObject Then
      t.upper = hi
    ElseIf hi <> "" Then
      t.upper = Val(hi)
    End If
  End If
End Sub

Private Sub initstuff()
  suSeed.name = "suSeed"
  suSeed.SetObj lbl(11), txt(11), unt(11), ref
  suSeed.tag = "OUTSEED"
  suSeed.Prompt = "Random seed value"
  suSeed.lower = 1
  suSeed.UserValue = 1
  suSeed.DefaultValue = 1
  suSeed.Refresh
  
  suIter.name = "suIter"
  suIter.SetObj lbl(12), txt(12), unt(12), ref
  suIter.tag = "OUTITERATIONS"
  suIter.Prompt = "Number of iterations"
  suIter.lower = 2
  suIter.UserValue = 2
  suIter.DefaultValue = 2
  suIter.Refresh
  
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
  
  paths(0) = "Fruit"
  paths(1) = "Grain"
  paths(2) = "Root vegetables"
  paths(3) = "Leafy vegetables"
  paths(4) = "Other vegetables"
  paths(5) = "Aquatic plants"
  paths(6) = "Beef"
  paths(7) = "Meat"
  paths(8) = "Milk"
  paths(9) = "Poultry"
  paths(10) = "Eggs"
  paths(11) = "Fish"
  paths(12) = "Crustacea"
  paths(13) = "Mollusks"
  paths(14) = "Shower"
  paths(15) = "Swimming"
  paths(16) = "Shoreline"
  paths(17) = "Boating"
  paths(18) = "Water"
  paths(19) = "Soil"
  paths(20) = "Air"
  paths(21) = "Indoor air"

End Sub

Private Function getIdx(idx As String, p As parmrec, glyph As glyphtype) As Long
  getIdx = 0
  Select Case idx
  Case "Site": getIdx = SiteIndex
  Case "Glyph": getIdx = glyph.idx
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

  p1.pname = Trim(v.ques(i, 1))
  p1.idx1 = getIdx(v.ques(i, 2), p, glyph)
  p1.idx2 = getIdx(v.ques(i, 3), p, glyph)
  p1.idx3 = getIdx(v.ques(i, 4), p, glyph)
  p1.idx4 = getIdx(v.ques(i, 5), p, glyph)
  p1.idx5 = getIdx(v.ques(i, 6), p, glyph)
  p1.idx6 = getIdx(v.ques(i, 7), p, glyph)
  
  For m = 1 To vCt
    If p1.pname = vCache(m).pname And _
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

  p1.pname = Trim(v.ques(i, 1))
  p1.idx1 = getIdx(v.ques(i, 2), p, glyph)
  p1.idx2 = getIdx(v.ques(i, 3), p, glyph)
  p1.idx3 = getIdx(v.ques(i, 4), p, glyph)
  p1.idx4 = getIdx(v.ques(i, 5), p, glyph)
  p1.idx5 = getIdx(v.ques(i, 6), p, glyph)
  p1.idx6 = getIdx(v.ques(i, 7), p, glyph)
  
  For m = 1 To vCt
    If p1.pname = vCache(m).pname And _
       p1.idx1 = vCache(m).idx1 And p1.idx2 = vCache(m).idx2 And p1.idx3 = vCache(m).idx3 And _
       p1.idx4 = vCache(m).idx4 And p1.idx5 = vCache(m).idx5 And p1.idx6 = vCache(m).idx6 Then
       vLookup = vCache(m).pval
       Exit Function
    End If
  Next
  
  For m = 1 To gidCt
    If p1.pname = gidData(m).pname And _
       p1.idx1 = gidData(m).idx1 And p1.idx2 = gidData(m).idx2 And p1.idx3 = gidData(m).idx3 And _
       p1.idx4 = gidData(m).idx4 And p1.idx5 = gidData(m).idx5 And p1.idx6 = gidData(m).idx6 Then
       vLookup = gidData(m).pval
       Exit Function
    End If
  Next
  
  For m = 1 To fuiCt
    If fuiData(m).pname = v.ques(i, 1) Then
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
      var(vCnt).vName = parm.pname
      var(vCnt).vDup = -1
      var(vCnt).vMod = glyph.id
      var(vCnt).vExe = glyph.exe
      var(vCnt).vDes = parm.pname + " (" + CStr(parm.idx1) + "," + Str(parm.idx2) + "," + Str(parm.idx3) + "," + Str(parm.idx4) + "," + Str(parm.idx5) + "," + Str(parm.idx6) + ")"
      var(vCnt).vMin = var(vCnt).vLower.lower
      var(vCnt).vMax = var(vCnt).vUpper.upper
      var(vCnt).vUnit = parm.cunit
      var(vCnt).idx(0) = parm.idx1
      var(vCnt).idx(1) = parm.idx2
      var(vCnt).idx(2) = parm.idx3
      var(vCnt).idx(3) = parm.idx4
      var(vCnt).idx(4) = parm.idx5
      var(vCnt).idx(5) = parm.idx6
      If var(vCnt).vUnit = "" Then var(vCnt).vUnit = "N/A"
'          initparm var(vCnt).vStd, "suStd", 0, 3, 3, var(vCnt).vUnit, 0, var(vCnt).vUpper.upper
      
      initparm var(vCnt).vStd, "suStd", 0, 3, 3, var(vCnt).vUnit, "DISTSTD", 0
      initparm var(vCnt).vUpper, "suUpper", 0, 0, 0, var(vCnt).vUnit, "DISTUPPER", var(vCnt).vLower, var(vCnt).vMax
      initparm var(vCnt).vLower, "suLower", 0, 1, 1, var(vCnt).vUnit, "DISTLOWER", var(vCnt).vMin, var(vCnt).vUpper
      initparm var(vCnt).vMean, "suMean", Val(parm.pval), 2, 2, var(vCnt).vUnit, "DISTMEAN", var(vCnt).vMin, var(vCnt).vMax
      initparm var(vCnt).vScal, "suScale", 0, 3, 4, "N/A", "DISTSCALE", 0, 1
      initparm var(vCnt).vShift, "suShift", 0, 3, 5, var(vCnt).vUnit, "DISTSHIFT", var(vCnt).vLower, var(vCnt).vUpper
      initparm var(vCnt).vMode, "suMode", 0, 2, 6, var(vCnt).vUnit, "DISTMODE", var(vCnt).vLower, var(vCnt).vUpper
      var(vCnt).vDes2 = var(vCnt).vDes & " from " + glyph.lbl & "(" & var(vCnt).vMod & ")"
      var(vCnt).vDes = var(vCnt).vDes & " from " + var(vCnt).vMod
      vCnt = vCnt + 1
    End If
  Next
End Sub

Private Sub addDesVar(v As descriptor, glyph As glyphtype)
Dim i As Long
Dim m As Long
  
  If v.typ <> "Not Stochastic" Then
    open_parm fui2, FUIName, 2
    For m = 1 To gidCt
      If gidData(m).pname = v.name Then
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
        var(vCnt).idx(0) = gidData(m).idx1
        var(vCnt).idx(1) = gidData(m).idx2
        var(vCnt).idx(2) = gidData(m).idx3
        var(vCnt).idx(3) = gidData(m).idx4
        var(vCnt).idx(4) = gidData(m).idx5
        var(vCnt).idx(5) = gidData(m).idx6
        If var(vCnt).vUnit = "" Then var(vCnt).vUnit = "N/A"
'        initparm var(vCnt).vStd, "suStd", 0, 3, 3, var(vCnt).vUnit, 0, var(vCnt).vUpper.upper
        
        initparm var(vCnt).vStd, "suStd", 0, 3, 3, var(vCnt).vUnit, "DISTSTD", 0
        initparm var(vCnt).vUpper, "suUpper", 0, 0, 0, var(vCnt).vUnit, "DISTUPPER", var(vCnt).vLower, var(vCnt).vMax
        initparm var(vCnt).vLower, "suLower", 0, 1, 1, var(vCnt).vUnit, "DISTLOWER", var(vCnt).vMin, var(vCnt).vUpper
        initparm var(vCnt).vMean, "suMean", Val(gidData(m).pval), 2, 2, var(vCnt).vUnit, "DISTMEAN", var(vCnt).vMin, var(vCnt).vMax
        initparm var(vCnt).vScal, "suScale", 0, 3, 4, "N/A", "DISTSCALE", 0, 1
        initparm var(vCnt).vShift, "suShift", 0, 3, 5, var(vCnt).vUnit, "DISTSHIFT", var(vCnt).vLower, var(vCnt).vUpper
        initparm var(vCnt).vMode, "suMode", 0, 2, 6, var(vCnt).vUnit, "DISTMODE", var(vCnt).vLower, var(vCnt).vUpper
        If v.des = "" Then
          var(vCnt).vDes = v.name
        Else
          var(vCnt).vDes = v.des
        End If
        For i = 1 To v.qCnt
          If v.ques(i, 0) = "Label" Then
            var(vCnt).vDes = var(vCnt).vDes + ", " + v.ques(i, 1) + Str(getIdx(v.ques(i, 2), gidData(m), glyph))
          Else
            var(vCnt).vDes = var(vCnt).vDes + ", " + vLookup(i, v, gidData(m), glyph)
          End If
        Next
        var(vCnt).vDes2 = var(vCnt).vDes & " from " & glyph.lbl & "(" & var(vCnt).vMod & ")"
        var(vCnt).vDes = var(vCnt).vDes & " from " + var(vCnt).vMod
        vCnt = vCnt + 1
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

Private Sub outfill(z1 As String, z2 As String, z3 As String, z4 As String, z5 As String, z6 As String, _
                    Optional z7 As String = "", Optional z8 As String = "", _
                    Optional z9 As String = "", Optional z10 As String = "")
Dim i As Long
Dim yr As String
Dim nm As String
Dim blank2 As String
  
  yr = "yr"
  nm = "suOutTimePt"
  If (oCnt > UBound(out)) Then
    ReDim Preserve out(oCnt + 50) As output
  End If
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
  
  Select Case out(oCnt).oTime
    Case t1(1):
      ReDim out(oCnt).oTimePt(2) As New tFloat
      out(oCnt).oTimePtCnt = 2
      out(oCnt).oTimePt(0).name = nm
      out(oCnt).oTimePt(0).SetObj lbl(6), txt(6), unt(6), ref
      out(oCnt).oTimePt(0).Prompt = "Starting from"
      out(oCnt).oTimePt(0).UserUnit = yr
      out(oCnt).oTimePt(0).DefaultUnit = yr
      out(oCnt).oTimePt(1).name = nm
      out(oCnt).oTimePt(1).tag = "OUTTIME"
      out(oCnt).oTimePt(1).SetObj lbl(7), txt(7), unt(7), ref
      out(oCnt).oTimePt(1).Prompt = "Ending at"
      out(oCnt).oTimePt(1).UserUnit = yr
      out(oCnt).oTimePt(1).DefaultUnit = yr
    Case t1(2):
      ReDim out(oCnt).oTimePt(5) As New tFloat
      out(oCnt).oTimePtCnt = 1
      For i = 0 To 4
        out(oCnt).oTimePt(i).name = nm
        out(oCnt).oTimePt(i).tag = "OUTTIME"
        out(oCnt).oTimePt(i).SetObj lbl(i + 6), txt(i + 6), unt(i + 6), ref
        out(oCnt).oTimePt(i).Prompt = "Monitor"
        out(oCnt).oTimePt(i).UserUnit = yr
        out(oCnt).oTimePt(i).DefaultUnit = yr
      Next
  End Select
  
  blank2 = ""
  If z2 <> "" Then blank2 = " "
  
  out(oCnt).oDes2 = z4 + "(" + z3 + ") " + z1 + blank2 + z2
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
      out(oCnt).oDes2 = Replace(out(oCnt).oDes2, z3 + ") ", z3 + ") " + z10 + "(" + z9 + ") ")
      out(oCnt).oDes = Replace(out(oCnt).oDes, z3 + " ", z3 + " " + z9 + " ")
    Else
      out(oCnt).oDes2 = Replace(out(oCnt).oDes2, z3 + ") ", z3 + ") " + z9 + " ")
      out(oCnt).oDes = Replace(out(oCnt).oDes, z3 + " ", z3 + " " + z9 + " ")
    End If
  End If
  
  oCnt = oCnt + 1
End Sub

Private Sub add_cpo(lbl As String, modlbl As String, modname As String, Optional rad As Boolean = True, Optional chem As Boolean = True)
Dim i As Long
Dim j As Long
Dim k As Long

  For i = 1 To orgCnt
    For j = 1 To cCnt
      If (rad And contam(j).typ = 1) Or (chem And contam(j).typ <> 1) Or (rad And chem) Then
        outfill lbl, "", modname, modlbl, contam(j).cas, contam(j).name, "", "", org(i).id, org(i).name
'        For k = 1 To contam(j).numprog
'          outfill lbl, "", modname, modlbl, contam(j).progeny(k).cas, contam(j).progeny(k).name, _
'            contam(j).cas, contam(j).name, org(i).id, org(i).name
'        Next
      End If
    Next
  Next
End Sub

Private Sub add_tcpo(lbl As String, modlbl As String, modname As String, Optional rad As Boolean = True, Optional chem As Boolean = True)
Dim i As Long
Dim j As Long
Dim k As Long
Dim t As Long

  For i = 1 To orgCnt
    For j = 1 To cCnt
      For t = 0 To tCnt
        If (rad And contam(j).typ = 1) Or (chem And contam(j).typ <> 1) Or (rad And chem) Then
          outfill lbl, t1(t), modname, modlbl, contam(j).cas, contam(j).name, "", "", org(i).id, org(i).name
'          For k = 1 To contam(j).numprog
'            outfill lbl, t1(t), modname, modlbl, contam(j).progeny(k).cas, contam(j).progeny(k).name, _
'              contam(j).cas, contam(j).name, org(i).id, org(i).name
'          Next
        End If
      Next
    Next
  Next
End Sub

Private Sub add_tcpp(lbl As String, modlbl As String, modname As String, Optional rad As Boolean = True, Optional chem As Boolean = True)
Dim i As Long
Dim j As Long
Dim k As Long
Dim t As Long
Dim cnt As Long
Dim Path(19) As String
  
  Select Case Left(lbl, 6)
  Case "extern"
    Path(0) = paths(15)
    Path(1) = paths(16)
    Path(2) = paths(17)
    Path(3) = paths(19)
    Path(4) = paths(20)
    Path(5) = ""
    cnt = 5
  Case "dermal"
    Path(0) = paths(14)
    Path(1) = paths(15)
    Path(2) = paths(19)
    Path(3) = ""
    cnt = 3
  Case "inhala"
    Path(0) = paths(14)
    Path(1) = paths(19)
    Path(2) = paths(20)
    Path(3) = paths(21)
    Path(4) = ""
    cnt = 4
  Case "ingest"
    Path(0) = paths(0)
    Path(1) = paths(1)
    Path(2) = paths(2)
    Path(3) = paths(3)
    Path(4) = paths(4)
    Path(5) = paths(5)
    Path(6) = paths(6)
    Path(7) = paths(7)
    Path(8) = paths(8)
    Path(9) = paths(9)
    Path(10) = paths(10)
    Path(11) = paths(11)
    Path(12) = paths(12)
    Path(13) = paths(13)
    Path(14) = paths(14)
    Path(15) = paths(15)
    Path(16) = paths(16)
    Path(17) = paths(18)
    Path(18) = paths(19)
    Path(19) = ""
    cnt = 19
  End Select
  
  For i = 0 To cnt
    For j = 1 To cCnt
      For t = 0 To tCnt
        If (rad And contam(j).typ = 1) Or (chem And contam(j).typ <> 1) Or (rad And chem) Then
          outfill lbl, t1(t), modname, modlbl, contam(j).cas, contam(j).name, "", "", Path(i), ""
'          For k = 1 To contam(j).numprog
'            outfill lbl, t1(t), modname, modlbl, contam(j).progeny(k).cas, contam(j).progeny(k).name, _
'              contam(j).cas, contam(j).name, Path(i), ""
'          Next
        End If
      Next
    Next
  Next
End Sub

Private Sub add_tcp(lbl As String, modlbl As String, modname As String, Optional rad As Boolean = True, Optional chem As Boolean = True)
Dim i As Long
Dim j As Long
Dim k As Long
  
  For i = 1 To cCnt
    For j = 0 To tCnt
      If (rad And contam(i).typ = 1) Or (chem And contam(i).typ <> 1) Or (rad And chem) Then
        outfill lbl, t1(j), modname, modlbl, contam(i).cas, contam(i).name
'        For k = 1 To contam(i).numprog
'          outfill lbl, t1(j), modname, modlbl, contam(i).progeny(k).cas, contam(i).progeny(k).name, _
'            contam(i).cas, contam(i).name
'        Next
      End If
    Next
  Next
End Sub

Private Sub add_t(lbl As String, modlbl As String, modname As String)
Dim j As Long

  For j = 0 To tCnt
    outfill lbl, t1(j), modname, modlbl, "", ""
  Next
End Sub

Private Sub addOut(typ As String, glyph As glyphtype)
  Dim i As Long
  Dim j As Long
  Dim k As Long
  Dim fluxtype As String
  
  For i = 0 To glyph.fcount
    Select Case glyph.ftype(i)
    Case "aff"
      add_tcp "gas 1 flux", glyph.lbl, glyph.id
      add_tcp "particle 1 flux", glyph.lbl, glyph.id
      add_tcp "particle 2 flux", glyph.lbl, glyph.id
      add_tcp "particle 3 flux", glyph.lbl, glyph.id
    Case "wff"
      add_t "water flux", glyph.lbl, glyph.id
      If glyph.fqual(i) = "surface water" Then
        add_tcp "dissolved constituent flux", glyph.lbl, glyph.id
        add_tcp "adsorbed constituent flux", glyph.lbl, glyph.id
      Else
        add_tcp "total constituent flux", glyph.lbl, glyph.id
      End If
    Case "wcf"
      add_tcp "water concentration", glyph.lbl, glyph.id
    Case "scf"
      If glyph.fqual(i) = "Sediment" Then add_tcp "sediment concentration", glyph.lbl, glyph.id
      If glyph.fqual(i) = "Soil" Then add_tcp "soil concentration", glyph.lbl, glyph.id
    Case "bbf"
      add_tcpo "body burden", glyph.lbl, glyph.id, False
      
'    Case "exf"
'      add_cpo "percent time exceeding effect", glyph.lbl, glyph.Id, False
'      add_tcpo "hazard quotient", glyph.lbl, glyph.Id, False
'      add_tcpo "intake hazard quotient", glyph.lbl, glyph.Id, False
'
    Case "twi"
      add_tcpo "dose", glyph.lbl, glyph.id, False



    Case "ato"
      For j = 1 To 4
        fluxtype = "particle " & CStr(j)
        If j = 4 Then fluxtype = "gas 1"
        For k = 1 To 5
          add_tcp "concentration for " & fluxtype & " at location #" & CStr(k), glyph.lbl, glyph.id, True, True
          add_tcp "total deposition for " & fluxtype & " at location #" & CStr(k), glyph.lbl, glyph.id, True, True
          add_tcp "wet deposition for " & fluxtype & " at location #" & CStr(k), glyph.lbl, glyph.id, True, True
          add_tcp "dry deposition for " & fluxtype & " at location #" & CStr(k), glyph.lbl, glyph.id, True, True
        Next
      Next
    Case "epf"
      add_tcpp "ingestion exposure concentration", glyph.lbl, glyph.id
      add_tcpp "inhalation exposure concentration", glyph.lbl, glyph.id
      add_tcpp "dermal exposure concentration", glyph.lbl, glyph.id
      add_tcpp "external dose", glyph.lbl, glyph.id, True, False
    Case "rif"
      add_t "summed Bq intake", glyph.lbl, glyph.id
      add_t "summed Bq/kg concentration (external)", glyph.lbl, glyph.id
      add_t "summed Bq/L concentration (external)", glyph.lbl, glyph.id
      add_t "summed Bq/m^3 concentration (external)", glyph.lbl, glyph.id
      add_t "summed mg/kg/day carcinogenic intake", glyph.lbl, glyph.id
      add_t "summed mg/kg/day noncarcinogenic intake", glyph.lbl, glyph.id
      add_t "summed mg/m^3 carcinogenic intake", glyph.lbl, glyph.id
      add_t "summed mg/m^3 noncarcinogenic intake", glyph.lbl, glyph.id
      add_tcp "Bq intake", glyph.lbl, glyph.id, True, False
      add_tcp "Bq/kg concentration (external)", glyph.lbl, glyph.id, True, False
      add_tcp "Bq/L concentration (external)", glyph.lbl, glyph.id, True, False
      add_tcp "Bq/m^3 concentration (external)", glyph.lbl, glyph.id, True, False
      add_tcp "mg/kg/day carcinogenic intake", glyph.lbl, glyph.id, False
      add_tcp "mg/kg/day noncarcinogenic intake", glyph.lbl, glyph.id, False
      add_tcp "mg/m^3 carcinogenic intake", glyph.lbl, glyph.id, False
      add_tcp "mg/m^3 noncarcinogenic intake", glyph.lbl, glyph.id, False
    Case "hif"
      add_t "summed Sv", glyph.lbl, glyph.id
      add_t "summed cancer incidence", glyph.lbl, glyph.id
      add_t "summed cancer fatalities", glyph.lbl, glyph.id
      add_t "summed cancer plus hereditary effects", glyph.lbl, glyph.id
      add_t "summed carcinogenic risk", glyph.lbl, glyph.id
      add_t "summed noncarcinogenic hazard index", glyph.lbl, glyph.id
      add_tcp "Sv", glyph.lbl, glyph.id, True, False
      add_tcp "cancer incidence", glyph.lbl, glyph.id, True, False
      add_tcp "cancer fatalities", glyph.lbl, glyph.id, True, False
      add_tcp "cancer plus hereditary effects", glyph.lbl, glyph.id, True, False
      add_tcp "carcinogenic risk", glyph.lbl, glyph.id, False
      add_tcp "noncarcinogenic hazard index", glyph.lbl, glyph.id, False
       

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
      nvers = Val(Mid(ver, InStr(ver, " ")))
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
    pCnt = Val(get_val(fle))
    For i = 0 To pCnt
      get_line fle
    Next
  
  ' for future versions of summm connection information will be read here
  ' The future is now
  ' Karl Castleton 11-27-01 added code to keep which file types a module
  ' produces
    glyph.fcount = Val(get_val(fle))
    ReDim glyph.ftype(glyph.fcount) As String
    ReDim glyph.fqual(glyph.fcount) As String
    For i = 0 To glyph.fcount
      glyph.ftype(i) = get_val(fle)
      glyph.fqual(i) = get_val(fle)
      get_line fle
    Next
    
   'read and generate input and output variable descriptions
    addOut group, glyph
    
    varCnt = Val(get_val(fle))
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
        v.qCnt = Val(get_val(fle))
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
        Select Case temp.pname
          Case Model
            For m = 1 To temp.idx1
              If read_parmrec(fui, temp) Then
                pos = InStr(temp.pname, "suO")
                If pos = 0 And tvcnt < temp.idx1 Then
                  ReDim Preserve tv(temp.idx1) As variable
                  tvcnt = tvcnt + 1
                End If
                If pos > 0 And twcnt < temp.idx1 Then
                  ReDim Preserve tw(temp.idx1) As output
                  twcnt = twcnt + 1
                End If
                Select Case temp.pname
                  Case "suSeed":       suSeed.UserValue = temp.pval: suSeed.Refresh
                  Case "suIter":       suIter.UserValue = temp.pval: suIter.Refresh
                  
                  Case "suName":       tv(temp.idx1).vName = temp.pval
                  Case "suIndex1":     tv(temp.idx1).idx(0) = temp.pval
                  Case "suIndex2":     tv(temp.idx1).idx(1) = temp.pval
                  Case "suIndex3":     tv(temp.idx1).idx(2) = temp.pval
                  Case "suIndex4":     tv(temp.idx1).idx(3) = temp.pval
                  Case "suIndex5":     tv(temp.idx1).idx(4) = temp.pval
                  Case "suIndex6":     tv(temp.idx1).idx(5) = temp.pval
                  Case "suAlias":      tv(temp.idx1).vAlias = temp.pval
                  Case "suDes":        tv(temp.idx1).vDes = temp.pval
                  Case "suMod":        tv(temp.idx1).vMod = temp.pval
                  Case "suFmt":        tv(temp.idx1).vFmt = temp.pval
                  Case "suDistrib":
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
                    dval = CDbl(Val(temp.pval))
                    tv(temp.idx1).vUpper.UserValue = Val(convert(temp.cunit, temp.uunit, dval))
                    tv(temp.idx1).vUpper.UserUnit = temp.uunit
                    tv(temp.idx1).vUpper.DefaultUnit = temp.cunit
                  Case "suLower":
                    dval = CDbl(Val(temp.pval))
                    tv(temp.idx1).vLower.UserValue = Val(convert(temp.cunit, temp.uunit, dval))
                    tv(temp.idx1).vLower.UserUnit = temp.uunit
                    tv(temp.idx1).vLower.DefaultUnit = temp.cunit
                  Case "suMean":
                    dval = CDbl(Val(temp.pval))
                    tv(temp.idx1).vMean.UserValue = Val(convert(temp.cunit, temp.uunit, dval))
                    tv(temp.idx1).vMean.UserUnit = temp.uunit
                    tv(temp.idx1).vMean.DefaultUnit = temp.cunit
                  Case "suStd":
                    dval = CDbl(Val(temp.pval))
                    tv(temp.idx1).vStd.UserValue = Val(convert(temp.cunit, temp.uunit, dval))
                    tv(temp.idx1).vStd.UserUnit = temp.uunit
                    tv(temp.idx1).vStd.DefaultUnit = temp.cunit
                  Case "suScale":
                    dval = CDbl(Val(temp.pval))
                    tv(temp.idx1).vScal.UserValue = Val(convert(temp.cunit, temp.uunit, dval))
                    tv(temp.idx1).vScal.UserUnit = temp.uunit
                    tv(temp.idx1).vScal.DefaultUnit = temp.cunit
                  Case "suShift":
                    dval = CDbl(Val(temp.pval))
                    tv(temp.idx1).vShift.UserValue = Val(convert(temp.cunit, temp.uunit, dval))
                    tv(temp.idx1).vShift.UserUnit = temp.uunit
                    tv(temp.idx1).vShift.DefaultUnit = temp.cunit
                  Case "suMode":
                    dval = CDbl(Val(temp.pval))
                    tv(temp.idx1).vMode.UserValue = Val(convert(temp.cunit, temp.uunit, dval))
                    tv(temp.idx1).vMode.UserUnit = temp.uunit
                    tv(temp.idx1).vMode.DefaultUnit = temp.cunit
                  Case "suBase":       tv(temp.idx1).vBase = Val(temp.pval)
                  Case "suEqu":        tv(temp.idx1).vEqu = temp.pval
                  Case "suCorNum":     tv(temp.idx1).cnt = Val(temp.pval)
                                       ReDim tv(temp.idx1).vCor(tv(temp.idx1).cnt) As correlation
                  Case "suCorAlias":   tv(temp.idx1).vCor(temp.idx2).cAlias = temp.pval
                  Case "suCor":        tv(temp.idx1).vCor(temp.idx2).cFactor = Val(temp.pval)
                  Case "suOutAlias":   tw(temp.idx1).oAlias = temp.pval
                  Case "suOutDes":
                    tw(temp.idx1).oDes = temp.pval
                  Case "suOutType":
                    tw(temp.idx1).oType = temp.pval
                  Case "suOutTime":
                    tw(temp.idx1).oTime = temp.pval
                  Case "suOutNumYear", "suOutTimePtCnt":
                    tw(temp.idx1).oTimePtCnt = Val(temp.pval)
                    ReDim tw(temp.idx1).oTimePt(tw(temp.idx1).oTimePtCnt) As New tFloat
                  Case "suOutYear", "suOutTimePt":
                    tw(temp.idx1).oTimePt(temp.idx2).UserValue = Val(temp.pval)
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
             var(i).idx(0) = tv(j).idx(0) And _
             var(i).idx(1) = tv(j).idx(1) And _
             var(i).idx(2) = tv(j).idx(2) And _
             var(i).idx(3) = tv(j).idx(3) And _
             var(i).idx(4) = tv(j).idx(4) And _
             var(i).idx(5) = tv(j).idx(5)) Then
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
            If tv(j).vDistrib = 4 Then
              var(i).vMean.lower = mylog(Val(var(i).vMean.lower), tv(j).vBase)
              var(i).vMean.upper = mylog(Val(var(i).vMean.upper), tv(j).vBase)
            End If
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
            var(i).cnt = tv(j).cnt
            ReDim Preserve var(i).vCor(var(i).cnt) As correlation
            For k = 1 To tv(j).cnt
              var(i).vCor(k - 1).cAlias = tv(j).vCor(k).cAlias
              var(i).vCor(k - 1).cFactor = tv(j).vCor(k).cFactor
            Next
            Exit For
          End If
        
        End If
      Next
    Next
    
'  resolve outputs
'  editorial changes cause a mismatch
'
    For j = 1 To twcnt
      For i = 0 To oCnt - 1
        If out(i).oType = tw(j).oType And _
           out(i).oTime = tw(j).oTime And _
           out(i).oSourceID = tw(j).oSourceID And _
           out(i).oCASID = tw(j).oCASID And _
           out(i).oParentID = tw(j).oParentID And _
           (out(i).oOrgID = tw(j).oOrgID Or (out(i).oOrgID = "" And tw(j).oOrgID <> "")) Then
          
          out(i).oOrgID = tw(j).oOrgID
          out(i).oAlias = tw(j).oAlias
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
Dim CSMIndex As Long
Dim pname As String

  sCnt = 0
  mCnt = 0
  If open_parm(fui, FUIName, 2) Then
    Do Until EOCF(fui.file)
      If read_parmrec(fui, temp) Then
        Select Case Right(temp.pname, 3)
          Case "fui"
            fuiCt = 0
            ReDim fuiData(temp.idx1)
            For m = 1 To temp.idx1
              If read_parmrec(fui, temp) Then
                If temp.idx1 = SiteIndex Then
                
                  fuiCt = fuiCt + 1
                  fuiData(fuiCt) = temp
                  
                  Select Case temp.pname
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
                        If Len(temp.pname) > 3 Then
                          pname = Right(temp.pname, Len(temp.pname) - 3)
                          Select Case pname
                           Case "name":
                             addId Left$(temp.pname, 3) & CStr(temp.idx2)
                           Case "label":
                             addLbl Left$(temp.pname, 3) & CStr(temp.idx2)
                           Case "despath":
                             addPath Left$(temp.pname, 3) & CStr(temp.idx2)
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
                  Select Case temp.pname
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
          susrc(i).idx = Val(Right(glyph(j).group, Len(glyph(j).group) - 3))
        End If
      Next
'      'If Left(susrc(i).Id) = "con" Then loadchem
      If Left(glyph(j).id, 3) = "aos" Or Left(glyph(j).id, 3) = "ebf" Or Left(glyph(j).id, 3) = "tos" Then
        loadGidData glyph(j).id
        For k = 1 To gidCt
          Select Case gidData(k).pname
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
    
    ReDim out(0) As output
    For i = 1 To sCnt
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

Private Sub form_Load()
Dim i As Long
Dim j As Long
  
  getargs
  If argc >= 5 Then
    Load.Show
    FUIName = argv(0) & ".GID"
    RunName = argv(1)
    SiteIndex = Val(argv(2))
    SUIndex = Val(argv(3))
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
    load_convert
    initstuff
    loadFUI
    loadSU
    
    For i = 0 To vCnt - 1
      For j = i + 1 To vCnt - 1
        If var(i).vDes2 = var(j).vDes2 And var(i).vDup = -1 Then
          var(j).vDup = i
          var(j).vAlias = ""
        End If
      Next
    Next
    
    For i = 0 To vCnt - 1
      If var(i).vDup = -1 Then
        List1.AddItem var(i).vAlias + Space(9 - Len(var(i).vAlias)) + "| " + var(i).vDes2
        List1.ItemData(List1.NewIndex) = i
      End If
    Next
    For i = 0 To oCnt - 1
      List4.AddItem out(i).oAlias + Space(9 - Len(out(i).oAlias)) + "| " + out(i).oDes2
      List4.ItemData(List4.NewIndex) = i
    Next
    
    Unload Load
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
      vTxt1.SetFocus
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
    vTxt1.SetFocus
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
    For i = 0 To vCnt - 1
      If var(i).vDes2 = var(ix).vDes2 Then
        var(i).vAlias = ""
        List1.list(i) = Space(9) + "| " + var(i).vDes2
        var(i).vEqu = ""
        var(i).vDup = -1
      End If
    Next
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
  If Val(pTxt2.Text) > 1 Or Val(pTxt2.Text) < -1 Or Val(pTxt2.Text) = 0 Then
    MsgBox "Correlation must be between -1 and 1, and can not be equal to 0", 16, "Correlation error"
    Exit Sub
  End If
'add correlation
  For i = 0 To var(List2.ItemData(List2.ListIndex)).cnt - 1
    If var(List2.ItemData(List2.ListIndex)).vCor(i).cAlias = alias.Text Then
      List3.list(i) = alias.Text + "  " + Str(Val(pTxt2.Text))
      var(List2.ItemData(List2.ListIndex)).vCor(i).cAlias = alias.Text
      var(List2.ItemData(List2.ListIndex)).vCor(i).cFactor = Val(pTxt2.Text)
      Exit For
    End If
  Next
  If i = var(List2.ItemData(List2.ListIndex)).cnt Then
    List3.AddItem alias.Text + "  " + Str(Val(pTxt2.Text))
    ReDim Preserve var(List2.ItemData(List2.ListIndex)).vCor(List3.ListCount) As correlation
    var(List2.ItemData(List2.ListIndex)).vCor(List3.ListCount - 1).cAlias = alias.Text
    var(List2.ItemData(List2.ListIndex)).vCor(List3.ListCount - 1).cFactor = Val(pTxt2.Text)
    var(List2.ItemData(List2.ListIndex)).cnt = List3.ListCount
  End If
'add reflection correlation
  For j = 0 To vCnt - 1
    If var(j).vAlias = alias.Text Then
      For k = 0 To var(j).cnt - 1
        If var(j).vCor(k).cAlias = var(List2.ItemData(List2.ListIndex)).vAlias Then
          var(j).vCor(i).cFactor = Val(pTxt2.Text)
          Exit For
        End If
      Next
      If k = var(j).cnt Then
        ReDim Preserve var(j).vCor(var(j).cnt + 1) As correlation
        var(j).vCor(var(j).cnt).cAlias = var(List2.ItemData(List2.ListIndex)).vAlias
        var(j).vCor(var(j).cnt).cFactor = Val(pTxt2.Text)
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
End Sub

Private Sub delVarEqu()
  If List2.ListIndex = -1 Then Exit Sub
  var(List2.ItemData(List2.ListIndex)).vEqu = ""
  pTxt4.Caption = ""
End Sub

Private Sub HideTime()
Dim i As Long, ix As Long
  ix = List4.ItemData(List4.ListIndex)
  For i = 0 To out(ix).oTimePtCnt - 1
    out(ix).oTimePt(i).UserValue = txt(i + 6).Text
    out(ix).oTimePt(i).UserUnit = unt(i + 6)
  Next
  For i = 6 To 10
    lbl(i).Visible = False
    txt(i).Visible = False
    unt(i).Visible = False
  Next
  yrCnt.Visible = False
  tPanel.Visible = False
  SSTab1.Enabled = True
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
              lbl(5).Caption = "Input years to average"
              Command(10).Visible = True
              SSTab1.Enabled = False
  Case t1(2): tPanel.Visible = True
              lbl(5).Caption = "Number of years to monitor"
              yrCnt.ListIndex = out(ix).oTimePtCnt - 1
              yrCnt.Visible = True
              For i = 0 To out(ix).oTimePtCnt - 1
                out(ix).oTimePt(i).tag = "OUTTIME"
                out(ix).oTimePt(i).Refresh
              Next
              Command(10).Visible = True
              SSTab1.Enabled = False
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
  Case "ingestio", "inhalati", "dermal e", "external"
    If out(ix).oOrgID = "" Then
      out(ix).oOrgID = InputBox("Enter a an exposure pathway", "Exposure pathway", "")
      If out(ix).oOrgID = "" Then
        Command(8).Enabled = False
        addOutAlias = False
        Exit Function
      End If
    End If
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
  If SSCheck2.value Then
    List4.RemoveItem List4.ListIndex
  Else
    List4.list(List4.ListIndex) = Space(9) + "| " + out(ix).oDes2
  End If
  List4.ListIndex = -1
  List4_Click
End Sub

Private Sub List1_Click()
  If List1.ListIndex < 0 Then
    vTxt2.Text = ""
    vTxt1.Text = ""
  Else
    vTxt2.Text = var(List1.ItemData(List1.ListIndex)).vDes2 ' BLH 12/00
    vTxt1.Text = var(List1.ItemData(List1.ListIndex)).vAlias ' BLH 12/00
  End If

  If vTxt1.Text = "" Then
    vTxt1.Enabled = True
  Else
    vTxt1.Enabled = False
  End If
  Command(0).Enabled = (vTxt1.Text = "")
  Command(1).Enabled = (vTxt1.Text <> "")
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
End Sub

Private Sub List2_DblClick()
  List2_Click
End Sub

Private Sub List3_Click()
  set_unit alias, var(List2.ItemData(List2.ListIndex)).vCor(List3.ListIndex).cAlias
  pTxt2.Text = CStr(var(List2.ItemData(List2.ListIndex)).vCor(List3.ListIndex).cFactor)
End Sub

Private Sub List3_dblClick()
  List3_Click
End Sub

Private Sub List4_Click()
Dim ix As Long, vis As Boolean
  oTxt2.Text = ""
  oTxt1.Text = ""
  If List4.ListIndex >= 0 Then
    ix = List4.ItemData(List4.ListIndex)
    oTxt2.Text = out(ix).oDes2
    oTxt1.Text = out(ix).oAlias
  End If
  oTxt1.Enabled = oTxt1 = ""
  Command(8).Enabled = False              ' add
  Command(9).Enabled = oTxt1.Text <> ""   ' delete
  Command(10).Visible = False             ' edit time
  If oTxt1.Text <> "" And List4.ListIndex >= 0 Then
    Command(10).Visible = (out(ix).oTime <> t1(0))
    If Command(10).Visible Then
      Command(10).Enabled = True
    End If
  End If
  Command(3).Visible = False
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

Private Function writeVar(fle As parmfile, i As Long, j As Long) As Boolean
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
  myparm.pname = "suAlias"
  myparm.pval = var(i).vAlias
  write_parmrec fle, myparm
  myparm.pname = "suDes"
  myparm.pval = var(i).vDes
  write_parmrec fle, myparm
  myparm.pname = "suExe"
  myparm.pval = var(i).vExe
  write_parmrec fle, myparm
  myparm.pname = "suFmt"
  myparm.pval = var(i).vFmt
  write_parmrec fle, myparm
  myparm.pname = "suMod"
  myparm.pval = var(i).vMod
  write_parmrec fle, myparm
  myparm.pname = "suDistrib"
  myparm.pval = distrib.list(var(i).vDistrib)
  write_parmrec fle, myparm
  For k = 0 To 5
    myparm.pname = "suIndex" + CStr(k + 1)
    myparm.pval = CStr(var(i).idx(k))
    write_parmrec fle, myparm
  Next
  myparm.pname = "suBase"
  myparm.pval = CStr(var(i).vBase)
  write_parmrec fle, myparm
  myparm.pname = "suEqu"
  myparm.pval = var(i).vEqu
  write_parmrec fle, myparm
  myparm.pname = "suCorNum"
  myparm.pval = CStr(var(i).cnt)
  write_parmrec fle, myparm
  For k = 0 To var(i).cnt - 1
    myparm.pname = "suCorAlias"
    myparm.idx2 = CStr(k + 1)
    myparm.pval = var(i).vCor(k).cAlias
    write_parmrec fle, myparm
    myparm.pname = "suCor"
    myparm.pval = CStr(var(i).vCor(k).cFactor)
    write_parmrec fle, myparm
  Next
  WriteIt fle, var(i).vUpper, j, 0
  WriteIt fle, var(i).vLower, j, 0
  WriteIt fle, var(i).vMean, j, 0
  WriteIt fle, var(i).vStd, j, 0
  WriteIt fle, var(i).vScal, j, 0
  WriteIt fle, var(i).vShift, j, 0
  WriteIt fle, var(i).vMode, j, 0
End Function

Private Function writeOut(fle As parmfile, i As Long, j As Long) As Boolean
Dim myparm As parmrec
Dim k As Long

  writeOut = False
  set_parm myparm, "suOutAlias", j, 0, 0, 0, 0, 0, 0, "N/A", "N/A", out(i).oAlias
  write_parmrec fle, myparm
  
  myparm.pname = "suOutDes"
  myparm.pval = out(i).oDes
  write_parmrec fle, myparm
  
  myparm.pname = "suOutType"
  myparm.pval = out(i).oType
  write_parmrec fle, myparm
  
  myparm.pname = "suOutSourceID"
  myparm.pval = out(i).oSourceID
  write_parmrec fle, myparm

  myparm.pname = "suOutCASID"
  myparm.pval = out(i).oCASID
  write_parmrec fle, myparm

  myparm.pname = "suOutParentID"
  myparm.pval = out(i).oParentID
  write_parmrec fle, myparm

  myparm.pname = "suOutOrgID"
  myparm.pval = out(i).oOrgID
  write_parmrec fle, myparm
  
  myparm.pname = "suOutTime"
  myparm.pval = out(i).oTime
  write_parmrec fle, myparm
  
  myparm.pname = "suOutTimePtCnt"
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
Dim fname As String
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
  fname = RunName & ".GID"
  If open_parm(fle, fname, 1) Then
  
    If suSeed.UserValue < 0 Then
      genflag = True
      put_val errfile, "Seed must be positive!"
      put_line errfile
    End If
    set_parm myparm, "suSeed", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", suSeed.pval
    write_parmrec fle, myparm
  
    j = 0
    For i = 0 To vCnt - 1
      If var(i).vAlias <> "" Then
        j = j + 1
        If writeVar(fle, i, j) Then genflag = True
      End If
    Next
    set_parm myparm, "suVarNum", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CStr(j)
    write_parmrec fle, myparm
    
    If (70 < j) Then
      genflag = True
      put_val errfile, "More than 70 variables defined!"
      put_line errfile
      put_val errfile, "Contact PNNL for more information"
      put_line errfile
    End If
    
    
    If (suIter.UserValue + cnt <= j + 1) Then
      If j < 2 Then
        suIter.UserValue = 2 ' just set it!
        suIter.Refresh
        MsgBox "suIter must be greater than 1!" & vbCrLf & "suIter value set to " & suIter.pval
      Else
        suIter.UserValue = j + 1 ' just set it!
        suIter.Refresh
        MsgBox "suIter must be greater than the number of defined variables." & vbCrLf & _
               CStr(cnt) & " hidden alias auto brrn defined." & vbCrLf & _
               "suIter set to " & suIter.pval
      End If
    End If
    set_parm myparm, "suIter", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", suIter.pval
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
  List1.Clear
  For i = 0 To vCnt - 1
    If var(i).vDup = -1 Then
      If value Then
        If var(i).vAlias <> "" Then
          List1.AddItem var(i).vAlias + Space(9 - Len(var(i).vAlias)) + "| " + var(i).vDes2
          List1.ItemData(List1.NewIndex) = i
        End If
      Else
        List1.AddItem var(i).vAlias + Space(9 - Len(var(i).vAlias)) + "| " + var(i).vDes2
        List1.ItemData(List1.NewIndex) = i
      End If
    End If
  Next
  List1.ListIndex = -1
  List1_Click
End Sub

Private Sub SSCheck2_Click(value As Integer)
Dim i As Integer
  List4.Clear
  For i = 0 To oCnt - 1
    If value Then
      If out(i).oAlias <> "" Then
        List4.AddItem out(i).oAlias + Space(9 - Len(out(i).oAlias)) + "| " + out(i).oDes2
        List4.ItemData(List4.NewIndex) = i
      End If
    Else
      List4.AddItem out(i).oAlias + Space(9 - Len(out(i).oAlias)) + "| " + out(i).oDes2
      List4.ItemData(List4.NewIndex) = i
    End If
  Next
  List4.ListIndex = -1
  List4_Click
End Sub

Private Sub SSTab1_Click(PreviousTab As Integer)
Dim i As Long
Dim found As Boolean
  
  uFrame(0).Enabled = False
  uFrame(1).Enabled = False
  uFrame(2).Enabled = False
  uFrame(SSTab1.Tab).Enabled = True
  SSTab1_GotFocus
  
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
End Sub

Private Sub SSTab3_Click(PreviousTab As Integer)
  SSTab3_GotFocus
End Sub

Private Sub SSTab3_DblClick()
  SSTab3_GotFocus
End Sub

Private Sub txt_Change(Index As Integer)
Dim hold As Double
  If List2.ListIndex = -1 Then Exit Sub
  If distrib.ListIndex = 2 Or distrib.ListIndex = 4 Then
    hold = var(List2.ItemData(List2.ListIndex)).vLower.lower
    var(List2.ItemData(List2.ListIndex)).vLower.lower = 0
  End If
  Select Case Index
  Case 0
    var(List2.ItemData(List2.ListIndex)).vUpper.Change
  Case 1
    var(List2.ItemData(List2.ListIndex)).vLower.Change
  Case 2
    Select Case distrib.ListIndex
    Case 3, 4, 5, 7, 8
     var(List2.ItemData(List2.ListIndex)).vMean.Change
    Case 6
     var(List2.ItemData(List2.ListIndex)).vMode.Change
    End Select
  Case 3
    Select Case distrib.ListIndex
    Case 3, 4, 8
     var(List2.ItemData(List2.ListIndex)).vStd.Change
    Case 5
     var(List2.ItemData(List2.ListIndex)).vScal.Change
    Case 7
     var(List2.ItemData(List2.ListIndex)).vShift.Change
    End Select
  Case 11: suSeed.Change
  Case 12: suIter.Change
  End Select
  If (distrib.ListIndex = 2 Or distrib.ListIndex = 4) Then
    var(List2.ItemData(List2.ListIndex)).vLower.lower = hold
  End If
End Sub

Private Sub unt_Click(Index As Integer)
' If Index > 5 Then out(List4.ListIndex).oTimePt(Index - 6).Click
  If Index > 5 Then out(List4.ItemData(List4.ListIndex)).oTimePt(Index - 6).Click
  If List2.ListIndex = -1 Then Exit Sub
  Select Case Index
  Case 0
    var(List2.ItemData(List2.ListIndex)).vUpper.Click
  Case 1
    var(List2.ItemData(List2.ListIndex)).vLower.Click
  Case 2
    Select Case distrib.ListIndex
    Case 3, 4, 5, 7, 8
     var(List2.ItemData(List2.ListIndex)).vMean.Click
    Case 6
     var(List2.ItemData(List2.ListIndex)).vMode.Click
    End Select
  Case 3
    Select Case distrib.ListIndex
    Case 3, 4, 8
     var(List2.ItemData(List2.ListIndex)).vStd.Click
    Case 5
     var(List2.ItemData(List2.ListIndex)).vScal.Click
    Case 7
     var(List2.ItemData(List2.ListIndex)).vShift.Click
    End Select
  Case 11: suSeed.Click
  Case 12: suIter.Click
  End Select
End Sub

Private Sub base_Click()
  If List2.ListIndex = -1 Then Exit Sub
  var(List2.ItemData(List2.ListIndex)).vBase = base.ListIndex
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
      
Function mylog(Valu As Double, base As Long) As Double
  If Valu < 1E-30 Then
    mylog = -30
  Else
    If base = 0 Then
      mylog = Log(Valu) / Log(10#)
    Else
      mylog = Log(Valu)
    End If
  End If
End Function

Private Sub distrib_Click()
' removed from the list of possible distribution types are
' Exponential, Triangular, Gamma, Beta
  
  Hideit
  If List2.ListIndex = -1 Then Exit Sub
  pTxt1.Text = var(List2.ItemData(List2.ListIndex)).vDes2
  If var(List2.ItemData(List2.ListIndex)).vDistrib <> distrib.ListIndex Then
    var(List2.ItemData(List2.ListIndex)).vMean.lower = Val(var(List2.ItemData(List2.ListIndex)).vMin)
    var(List2.ItemData(List2.ListIndex)).vMean.upper = Val(var(List2.ItemData(List2.ListIndex)).vMax)
    var(List2.ItemData(List2.ListIndex)).vMean.UserValue = var(List2.ItemData(List2.ListIndex)).vMean.DefaultValue
  End If
  If distrib.ListIndex > 0 Then
    var(List2.ItemData(List2.ListIndex)).vLower.Refresh
    var(List2.ItemData(List2.ListIndex)).vUpper.Refresh
    Select Case distrib.ListIndex
    Case 2
      lbl(4).Visible = True
      base.Visible = True
      base.ListIndex = var(List2.ItemData(List2.ListIndex)).vBase
      lbl(2).Caption = lbl(2).Caption + " log"
      lbl(3).Caption = lbl(3).Caption + " log"
    Case 3, 8
      var(List2.ItemData(List2.ListIndex)).vMean.Refresh
      var(List2.ItemData(List2.ListIndex)).vStd.Refresh
    Case 4
      If var(List2.ItemData(List2.ListIndex)).vDistrib <> distrib.ListIndex Then
        var(List2.ItemData(List2.ListIndex)).vMean.lower = mylog(var(List2.ItemData(List2.ListIndex)).vMean.lower, var(List2.ItemData(List2.ListIndex)).vBase)
        var(List2.ItemData(List2.ListIndex)).vMean.upper = mylog(var(List2.ItemData(List2.ListIndex)).vMean.upper, var(List2.ItemData(List2.ListIndex)).vBase)
        var(List2.ItemData(List2.ListIndex)).vMean.UserValue = mylog(var(List2.ItemData(List2.ListIndex)).vMean.UserValue, var(List2.ItemData(List2.ListIndex)).vBase)
      End If
      var(List2.ItemData(List2.ListIndex)).vMean.Refresh
      var(List2.ItemData(List2.ListIndex)).vStd.Refresh
      lbl(4).Visible = True
      base.Visible = True
      base.ListIndex = var(List2.ItemData(List2.ListIndex)).vBase
      lbl(2).Caption = lbl(2).Caption + " log"
      lbl(3).Caption = lbl(3).Caption + " log"
    Case 5
      var(List2.ItemData(List2.ListIndex)).vMean.Refresh
      var(List2.ItemData(List2.ListIndex)).vScal.Refresh
    Case 6
      var(List2.ItemData(List2.ListIndex)).vMode.Refresh
    Case 7
      var(List2.ItemData(List2.ListIndex)).vMean.Refresh
      var(List2.ItemData(List2.ListIndex)).vShift.Refresh
    End Select
  End If
  var(List2.ItemData(List2.ListIndex)).vDistrib = distrib.ListIndex
End Sub

Private Sub vTxt1_Change()
  Command(0).Enabled = vTxt1 <> ""
  Command(1).Enabled = False
End Sub

Private Sub yrCnt_Click()
Dim i As Long, ix As Long
  ix = List4.ItemData(List4.ListIndex)
' out(List4.ListIndex).oTimePtCnt = yrCnt.ListIndex + 1
  out(ix).oTimePtCnt = yrCnt.ListIndex + 1 ' BLH 12/00
  For i = 0 To yrCnt.ListIndex
'   out(List4.ListIndex).oTimePt(i).Refresh
    out(ix).oTimePt(i).Refresh ' BLH 12/00
  Next
  For i = yrCnt.ListIndex + 7 To 10
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
  HelpAnchor = Command(Index).tag
End Sub

Private Sub txt_GotFocus(Index As Integer)
  HelpAnchor = txt(Index).tag
End Sub

Private Sub SSCheck1_GotFocus()
  HelpAnchor = SSCheck1.tag
End Sub

Private Sub SSCheck2_GotFocus()
  HelpAnchor = SSCheck2.tag
End Sub

Private Sub distrib_GotFocus()
  HelpAnchor = distrib.tag
End Sub

Private Sub alias_GotFocus()
  HelpAnchor = alias.tag
End Sub

Private Sub base_GotFocus()
  HelpAnchor = base.tag
End Sub

Private Sub List1_GotFocus()
  SSTab1_GotFocus
End Sub

Private Sub List2_GotFocus()
  HelpAnchor = List2.tag
End Sub

Private Sub List3_GotFocus()
  HelpAnchor = List3.tag
End Sub

Private Sub List4_GotFocus()
  SSTab1_GotFocus
End Sub

Private Sub SSTab3_GotFocus()
  Select Case SSTab3.Tab
  Case 0: HelpAnchor = "DIST"
  Case 1: HelpAnchor = "COR"
  Case 2: HelpAnchor = "EQU"
  End Select
End Sub

Private Sub SSTab1_GotFocus()
  Select Case SSTab1.Tab
  Case 0: HelpAnchor = "VAR"
  Case 1: HelpAnchor = "PARAM"
  Case 2: HelpAnchor = "OUT"
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


