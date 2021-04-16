VERSION 5.00
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "tabctl32.ocx"
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "threed32.ocx"
Begin VB.Form Exposure 
   BorderStyle     =   1  'Fixed Single
   ClientHeight    =   5280
   ClientLeft      =   4980
   ClientTop       =   3036
   ClientWidth     =   7680
   Icon            =   "Expose.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   NegotiateMenus  =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   5280
   ScaleWidth      =   7680
   StartUpPosition =   2  'CenterScreen
   Begin VB.Timer Timer1 
      Interval        =   100
      Left            =   8400
      Top             =   360
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
      TabIndex        =   202
      TabStop         =   0   'False
      Top             =   4920
      Width           =   7692
   End
   Begin TabDlg.SSTab exp_t 
      Height          =   5052
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   7692
      _ExtentX        =   13568
      _ExtentY        =   8911
      _Version        =   393216
      Style           =   1
      Tabs            =   7
      Tab             =   4
      TabsPerRow      =   7
      TabHeight       =   529
      TabCaption(0)   =   "Ground Water"
      TabPicture(0)   =   "Expose.frx":030A
      Tab(0).ControlEnabled=   0   'False
      Tab(0).Control(0)=   "gw_t"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).Control(1)=   "frame(0)"
      Tab(0).Control(1).Enabled=   0   'False
      Tab(0).ControlCount=   2
      TabCaption(1)   =   "Surface Water"
      TabPicture(1)   =   "Expose.frx":0326
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "frame(4)"
      Tab(1).Control(0).Enabled=   0   'False
      Tab(1).Control(1)=   "sw_t"
      Tab(1).Control(1).Enabled=   0   'False
      Tab(1).ControlCount=   2
      TabCaption(2)   =   "Air"
      TabPicture(2)   =   "Expose.frx":0342
      Tab(2).ControlEnabled=   0   'False
      Tab(2).Control(0)=   "frame(8)"
      Tab(2).Control(1)=   "air_t"
      Tab(2).ControlCount=   2
      TabCaption(3)   =   "Soil"
      TabPicture(3)   =   "Expose.frx":035E
      Tab(3).ControlEnabled=   0   'False
      Tab(3).Control(0)=   "frame(11)"
      Tab(3).Control(1)=   "frame(12)"
      Tab(3).ControlCount=   2
      TabCaption(4)   =   "Exposure Controls"
      TabPicture(4)   =   "Expose.frx":037A
      Tab(4).ControlEnabled=   -1  'True
      Tab(4).Control(0)=   "frame(13)"
      Tab(4).Control(0).Enabled=   0   'False
      Tab(4).ControlCount=   1
      TabCaption(5)   =   "Leach Rates"
      TabPicture(5)   =   "Expose.frx":0396
      Tab(5).ControlEnabled=   0   'False
      Tab(5).Control(0)=   "frame(14)"
      Tab(5).ControlCount=   1
      TabCaption(6)   =   "Constituent Parameters"
      TabPicture(6)   =   "Expose.frx":03B2
      Tab(6).ControlEnabled=   0   'False
      Tab(6).Control(0)=   "frame(15)"
      Tab(6).Control(0).Enabled=   0   'False
      Tab(6).ControlCount=   1
      Begin Threed.SSFrame frame 
         Height          =   2835
         Index           =   12
         Left            =   -74760
         TabIndex        =   157
         Top             =   1920
         Width           =   7215
         _Version        =   65536
         _ExtentX        =   12726
         _ExtentY        =   5001
         _StockProps     =   14
         Caption         =   "Pathways"
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
         Begin Threed.SSCheck slpath 
            Height          =   252
            Index           =   7
            Left            =   360
            TabIndex        =   151
            Tag             =   "kexpth"
            Top             =   1680
            Width           =   2004
            _Version        =   65536
            _ExtentX        =   3535
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "Milk"
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
         Begin Threed.SSCheck slpath 
            Height          =   252
            Index           =   6
            Left            =   360
            TabIndex        =   150
            Tag             =   "kexpth"
            Top             =   1440
            Width           =   2004
            _Version        =   65536
            _ExtentX        =   3535
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "Meat"
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
         Begin Threed.SSCheck slpath 
            Height          =   252
            Index           =   5
            Left            =   360
            TabIndex        =   148
            Tag             =   "kexpth"
            Top             =   840
            Width           =   2004
            _Version        =   65536
            _ExtentX        =   3535
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "Other vegetables"
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
         Begin Threed.SSCheck slpath 
            Height          =   252
            Index           =   4
            Left            =   360
            TabIndex        =   147
            Tag             =   "kexpth"
            Top             =   600
            Width           =   2004
            _Version        =   65536
            _ExtentX        =   3535
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "Leafy vegetables"
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
         Begin Threed.SSCheck slpath 
            Height          =   255
            Index           =   14
            Left            =   2880
            TabIndex        =   153
            Tag             =   "kexpth"
            Top             =   600
            Width           =   2010
            _Version        =   65536
            _ExtentX        =   3535
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "Soil - Ingestion"
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
         Begin Threed.SSCheck slpath 
            Height          =   255
            Index           =   19
            Left            =   2880
            TabIndex        =   154
            Tag             =   "kexpth"
            Top             =   840
            Width           =   2010
            _Version        =   65536
            _ExtentX        =   3528
            _ExtentY        =   444
            _StockProps     =   78
            Caption         =   "Soil - Inhalation"
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
         Begin Threed.SSCheck slpath 
            Height          =   255
            Index           =   23
            Left            =   2880
            TabIndex        =   156
            Tag             =   "kexpth"
            Top             =   1320
            Width           =   2010
            _Version        =   65536
            _ExtentX        =   3535
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "Soil - External"
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
         Begin Threed.SSCheck slpath 
            Height          =   255
            Index           =   15
            Left            =   2880
            TabIndex        =   155
            Tag             =   "kexpth"
            Top             =   1080
            Width           =   2010
            _Version        =   65536
            _ExtentX        =   3535
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "Soil - Dermal"
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
            Caption         =   "Plant Product Ingestion"
            Height          =   252
            Index           =   30
            Left            =   240
            TabIndex        =   146
            Top             =   360
            Width           =   2004
         End
         Begin VB.Label lbl 
            Caption         =   "Animal Product Ingestion"
            Height          =   252
            Index           =   29
            Left            =   240
            TabIndex        =   149
            Top             =   1200
            Width           =   2004
         End
         Begin VB.Label lbl 
            Caption         =   "Other Pathways"
            Height          =   255
            Index           =   27
            Left            =   2760
            TabIndex        =   152
            Top             =   360
            Width           =   2010
         End
      End
      Begin TabDlg.SSTab gw_t 
         Height          =   3435
         Left            =   -74760
         TabIndex        =   6
         Tag             =   "EGLTRTL"
         Top             =   1320
         Width           =   7215
         _ExtentX        =   12721
         _ExtentY        =   6054
         _Version        =   393216
         Style           =   1
         Tabs            =   2
         TabsPerRow      =   2
         TabHeight       =   529
         TabCaption(0)   =   "Pathways"
         TabPicture(0)   =   "Expose.frx":03CE
         Tab(0).ControlEnabled=   -1  'True
         Tab(0).Control(0)=   "frame(1)"
         Tab(0).Control(0).Enabled=   0   'False
         Tab(0).ControlCount=   1
         TabCaption(1)   =   "Water Usage"
         TabPicture(1)   =   "Expose.frx":03EA
         Tab(1).ControlEnabled=   0   'False
         Tab(1).Control(0)=   "frame(2)"
         Tab(1).ControlCount=   1
         Begin Threed.SSFrame frame 
            Height          =   2715
            Index           =   1
            Left            =   240
            TabIndex        =   22
            Top             =   480
            Width           =   6735
            _Version        =   65536
            _ExtentX        =   11880
            _ExtentY        =   4789
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
            Begin Threed.SSCheck gwpath 
               Height          =   252
               Index           =   7
               Left            =   240
               TabIndex        =   12
               Tag             =   "kexpth"
               Top             =   1560
               Width           =   2004
               _Version        =   65536
               _ExtentX        =   3535
               _ExtentY        =   445
               _StockProps     =   78
               Caption         =   "Milk"
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
            Begin Threed.SSCheck gwpath 
               Height          =   252
               Index           =   6
               Left            =   240
               TabIndex        =   11
               Tag             =   "kexpth"
               Top             =   1320
               Width           =   2004
               _Version        =   65536
               _ExtentX        =   3535
               _ExtentY        =   445
               _StockProps     =   78
               Caption         =   "Meat"
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
            Begin Threed.SSCheck gwpath 
               Height          =   252
               Index           =   5
               Left            =   240
               TabIndex        =   9
               Tag             =   "kexpth"
               Top             =   720
               Width           =   2000
               _Version        =   65536
               _ExtentX        =   3535
               _ExtentY        =   445
               _StockProps     =   78
               Caption         =   "Other vegetables"
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
            Begin Threed.SSCheck gwpath 
               Height          =   252
               Index           =   4
               Left            =   240
               TabIndex        =   8
               Tag             =   "kexpth"
               Top             =   480
               Width           =   2000
               _Version        =   65536
               _ExtentX        =   3535
               _ExtentY        =   445
               _StockProps     =   78
               Caption         =   "Leafy vegetables"
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
            Begin Threed.SSCheck gwpath 
               Height          =   252
               Index           =   3
               Left            =   2640
               TabIndex        =   15
               Tag             =   "kexpth"
               Top             =   720
               Width           =   2004
               _Version        =   65536
               _ExtentX        =   3535
               _ExtentY        =   445
               _StockProps     =   78
               Caption         =   "Shower water"
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
            Begin Threed.SSCheck gwpath 
               Height          =   252
               Index           =   1
               Left            =   2640
               TabIndex        =   14
               Tag             =   "kexpth"
               Top             =   480
               Width           =   2004
               _Version        =   65536
               _ExtentX        =   3535
               _ExtentY        =   445
               _StockProps     =   78
               Caption         =   "Drinking water"
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
            Begin Threed.SSCheck gwpath 
               Height          =   252
               Index           =   2
               Left            =   4800
               TabIndex        =   21
               Tag             =   "kexpth"
               Top             =   480
               Width           =   1400
               _Version        =   65536
               _ExtentX        =   2469
               _ExtentY        =   444
               _StockProps     =   78
               Caption         =   "Shower"
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
            Begin Threed.SSOption gwkex 
               Height          =   252
               Index           =   25
               Left            =   2820
               TabIndex        =   19
               Top             =   1800
               Width           =   2004
               _Version        =   65536
               _ExtentX        =   3528
               _ExtentY        =   444
               _StockProps     =   78
               Caption         =   "Indoor - Air"
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
               Value           =   -1  'True
            End
            Begin Threed.SSOption gwkex 
               Height          =   252
               Index           =   17
               Left            =   2820
               TabIndex        =   18
               Top             =   1560
               Width           =   2004
               _Version        =   65536
               _ExtentX        =   3528
               _ExtentY        =   444
               _StockProps     =   78
               Caption         =   "Shower - Air"
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
            Begin Threed.SSCheck gwpath 
               Height          =   252
               Index           =   0
               Left            =   2640
               TabIndex        =   17
               Tag             =   "kexpth"
               Top             =   1320
               Width           =   2004
               _Version        =   65536
               _ExtentX        =   3528
               _ExtentY        =   444
               _StockProps     =   78
               Caption         =   "Air - Volatiles from water"
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
               Caption         =   "Inhalation"
               Height          =   252
               Index           =   52
               Left            =   2520
               TabIndex        =   16
               Top             =   1080
               Width           =   2004
            End
            Begin VB.Label lbl 
               Caption         =   "Dermal"
               Height          =   252
               Index           =   13
               Left            =   4680
               TabIndex        =   20
               Top             =   240
               Width           =   1400
            End
            Begin VB.Label lbl 
               Caption         =   "Plant Product Ingestion"
               Height          =   252
               Index           =   16
               Left            =   120
               TabIndex        =   7
               Top             =   240
               Width           =   2000
            End
            Begin VB.Label lbl 
               Caption         =   "Animal Product Ingestion"
               Height          =   252
               Index           =   17
               Left            =   120
               TabIndex        =   10
               Top             =   1080
               Width           =   2000
            End
            Begin VB.Label lbl 
               Caption         =   "Other Ingestion"
               Height          =   252
               Index           =   18
               Left            =   2520
               TabIndex        =   13
               Top             =   240
               Width           =   2004
            End
         End
         Begin Threed.SSFrame frame 
            Height          =   2772
            Index           =   2
            Left            =   -74760
            TabIndex        =   37
            Top             =   480
            Width           =   6732
            _Version        =   65536
            _ExtentX        =   11874
            _ExtentY        =   4890
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
            Begin Threed.SSCheck gwCoverBox 
               Height          =   255
               Index           =   1
               Left            =   240
               TabIndex        =   204
               Top             =   600
               Width           =   200
               _Version        =   65536
               _ExtentX        =   353
               _ExtentY        =   450
               _StockProps     =   78
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
            Begin Threed.SSCheck gwCoverBox 
               Height          =   255
               Index           =   0
               Left            =   240
               TabIndex        =   203
               Top             =   240
               Width           =   200
               _Version        =   65536
               _ExtentX        =   353
               _ExtentY        =   450
               _StockProps     =   78
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
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H00C0FFC0&
               Enabled         =   0   'False
               Height          =   312
               Index           =   2
               Left            =   4620
               TabIndex        =   26
               Tag             =   "firr"
               Text            =   "1.0"
               Top             =   960
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H008080FF&
               Enabled         =   0   'False
               Height          =   312
               Index           =   3
               Left            =   3660
               TabIndex        =   29
               Tag             =   "cirr"
               Top             =   1440
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H00C0FFC0&
               Enabled         =   0   'False
               Height          =   312
               Index           =   1
               Left            =   3660
               TabIndex        =   33
               Tag             =   "twtr"
               Text            =   "1.0"
               Top             =   1920
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               BackColor       =   &H00FFFFFF&
               Enabled         =   0   'False
               Height          =   288
               Index           =   3
               Left            =   4620
               Style           =   2  'Dropdown List
               TabIndex        =   30
               Tag             =   "L/m^2/mon"
               Top             =   1440
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               BackColor       =   &H00FFFFFF&
               Enabled         =   0   'False
               Height          =   288
               Index           =   1
               Left            =   4620
               Style           =   2  'Dropdown List
               TabIndex        =   34
               Tag             =   "day"
               Top             =   1920
               Width           =   1000
            End
            Begin Threed.SSCheck gwdrink 
               Height          =   252
               Left            =   240
               TabIndex        =   23
               Tag             =   "gwdrink"
               Top             =   240
               Width           =   2004
               _Version        =   65536
               _ExtentX        =   3535
               _ExtentY        =   445
               _StockProps     =   78
               Caption         =   " Animal drinking"
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
               Value           =   -1  'True
            End
            Begin Threed.SSCheck gwtreat 
               Height          =   252
               Left            =   240
               TabIndex        =   36
               Tag             =   "EGLTRTL"
               Top             =   2280
               Width           =   3012
               _Version        =   65536
               _ExtentX        =   5313
               _ExtentY        =   444
               _StockProps     =   78
               Caption         =   " Domestic water is treated -- EG-LTRTL"
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
            Begin Threed.SSCheck gwCoverBox 
               Height          =   255
               Index           =   2
               Left            =   240
               TabIndex        =   205
               Top             =   2280
               Width           =   255
               _Version        =   65536
               _ExtentX        =   450
               _ExtentY        =   450
               _StockProps     =   78
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
            Begin Threed.SSCheck gwfeed 
               Height          =   252
               Left            =   240
               TabIndex        =   24
               Tag             =   "gwfeed"
               Top             =   600
               Width           =   2004
               _Version        =   65536
               _ExtentX        =   3535
               _ExtentY        =   445
               _StockProps     =   78
               Caption         =   " Irrigation of animal feed"
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
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Enabled         =   0   'False
               Height          =   252
               Index           =   2
               Left            =   5700
               TabIndex        =   27
               Tag             =   "0"
               Top             =   1020
               Width           =   996
            End
            Begin VB.Label lbl 
               Caption         =   "Fraction of the year that groundwater is used for irrigation -- EG-FIRR"
               Enabled         =   0   'False
               Height          =   432
               Index           =   2
               Left            =   240
               TabIndex        =   25
               Tag             =   "EGFIRR"
               Top             =   960
               Width           =   3300
            End
            Begin VB.Label lbl 
               Caption         =   "Irrigation rate -- EG-CIRR"
               Enabled         =   0   'False
               Height          =   252
               Index           =   3
               Left            =   228
               TabIndex        =   28
               Tag             =   "EGCIRR"
               Top             =   1500
               Width           =   3300
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Enabled         =   0   'False
               Height          =   252
               Index           =   3
               Left            =   5700
               TabIndex        =   31
               Tag             =   "0"
               Top             =   1500
               Width           =   996
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Enabled         =   0   'False
               Height          =   252
               Index           =   1
               Left            =   5700
               TabIndex        =   35
               Tag             =   "0"
               Top             =   1980
               Width           =   996
            End
            Begin VB.Label lbl 
               Caption         =   "Domestic water distribution time -- EG-TWTR"
               Enabled         =   0   'False
               Height          =   252
               Index           =   1
               Left            =   228
               TabIndex        =   32
               Tag             =   "EGTWTR"
               Top             =   1980
               Width           =   3300
            End
         End
      End
      Begin TabDlg.SSTab sw_t 
         Height          =   3435
         Left            =   -74760
         TabIndex        =   43
         Top             =   1320
         Width           =   7215
         _ExtentX        =   12721
         _ExtentY        =   6054
         _Version        =   393216
         Style           =   1
         TabHeight       =   529
         TabCaption(0)   =   "Pathways"
         TabPicture(0)   =   "Expose.frx":0406
         Tab(0).ControlEnabled=   -1  'True
         Tab(0).Control(0)=   "frame(5)"
         Tab(0).Control(0).Enabled=   0   'False
         Tab(0).ControlCount=   1
         TabCaption(1)   =   "Water Usage"
         TabPicture(1)   =   "Expose.frx":0422
         Tab(1).ControlEnabled=   0   'False
         Tab(1).Control(0)=   "frame(6)"
         Tab(1).Control(0).Enabled=   0   'False
         Tab(1).ControlCount=   1
         TabCaption(2)   =   "Recreational"
         TabPicture(2)   =   "Expose.frx":043E
         Tab(2).ControlEnabled=   0   'False
         Tab(2).Control(0)=   "frame(7)"
         Tab(2).Control(0).Enabled=   0   'False
         Tab(2).ControlCount=   1
         Begin Threed.SSFrame frame 
            Height          =   2715
            Index           =   7
            Left            =   -74760
            TabIndex        =   104
            Top             =   480
            Width           =   6732
            _Version        =   65536
            _ExtentX        =   11874
            _ExtentY        =   4789
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
               Enabled         =   0   'False
               Height          =   312
               Index           =   5
               Left            =   4560
               TabIndex        =   102
               Tag             =   "tissue"
               Text            =   "1"
               Top             =   2280
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H00C0FFC0&
               Enabled         =   0   'False
               Height          =   312
               Index           =   10
               Left            =   3660
               TabIndex        =   90
               Tag             =   "tinv"
               Text            =   "10"
               Top             =   840
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H00C0FFC0&
               Enabled         =   0   'False
               Height          =   312
               Index           =   9
               Left            =   3660
               TabIndex        =   86
               Tag             =   "tfsh"
               Text            =   "10"
               Top             =   360
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H008080FF&
               Enabled         =   0   'False
               Height          =   312
               Index           =   11
               Left            =   3660
               TabIndex        =   94
               Tag             =   "tss"
               Top             =   1320
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H008080FF&
               Enabled         =   0   'False
               Height          =   312
               Index           =   12
               Left            =   3660
               TabIndex        =   98
               Tag             =   "rhoss"
               Top             =   1800
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               BackColor       =   &H00FFFFFF&
               Enabled         =   0   'False
               Height          =   288
               Index           =   12
               Left            =   4620
               Style           =   2  'Dropdown List
               TabIndex        =   99
               Tag             =   "g/cm^3"
               Top             =   1800
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               BackColor       =   &H00FFFFFF&
               Enabled         =   0   'False
               Height          =   288
               Index           =   11
               Left            =   4620
               Style           =   2  'Dropdown List
               TabIndex        =   95
               Tag             =   "m"
               Top             =   1320
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               BackColor       =   &H00FFFFFF&
               Enabled         =   0   'False
               Height          =   288
               Index           =   10
               Left            =   4620
               Style           =   2  'Dropdown List
               TabIndex        =   91
               Tag             =   "day"
               Top             =   840
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               BackColor       =   &H00FFFFFF&
               Enabled         =   0   'False
               Height          =   288
               Index           =   9
               Left            =   4620
               Style           =   2  'Dropdown List
               TabIndex        =   87
               Tag             =   "day"
               Top             =   360
               Width           =   1000
            End
            Begin VB.Label lbl 
               Caption         =   "Finfish fillet correction factor -- EW-TISSUE"
               Enabled         =   0   'False
               Height          =   255
               Index           =   5
               Left            =   240
               TabIndex        =   101
               Tag             =   "EWTISSUE"
               Top             =   2310
               Width           =   4260
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Enabled         =   0   'False
               Height          =   255
               Index           =   5
               Left            =   5700
               TabIndex        =   103
               Tag             =   "0"
               Top             =   2340
               Width           =   990
            End
            Begin VB.Label lbl 
               Caption         =   "Thickness of shoreline sediments -- EW-TSS"
               Enabled         =   0   'False
               Height          =   252
               Index           =   11
               Left            =   240
               TabIndex        =   93
               Tag             =   "EWTSS"
               Top             =   1380
               Width           =   3300
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Enabled         =   0   'False
               Height          =   252
               Index           =   11
               Left            =   5700
               TabIndex        =   96
               Tag             =   "0"
               Top             =   1380
               Width           =   996
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Enabled         =   0   'False
               Height          =   252
               Index           =   12
               Left            =   5700
               TabIndex        =   100
               Tag             =   "0"
               Top             =   1860
               Width           =   996
            End
            Begin VB.Label lbl 
               Caption         =   "Density of shoreline sediments -- EW-RHOSS"
               Enabled         =   0   'False
               Height          =   252
               Index           =   12
               Left            =   240
               TabIndex        =   97
               Tag             =   "EWRHOSS"
               Top             =   1860
               Width           =   3300
            End
            Begin VB.Label lbl 
               Caption         =   "Finfish ingestion harvest delay time -- EW-TFSH"
               Enabled         =   0   'False
               Height          =   432
               Index           =   9
               Left            =   240
               TabIndex        =   85
               Tag             =   "EWTFSH"
               Top             =   360
               Width           =   3300
            End
            Begin VB.Label lbl 
               Caption         =   "Shell fish ingestion harvest delay time -- EW-TINV"
               Enabled         =   0   'False
               Height          =   432
               Index           =   10
               Left            =   240
               TabIndex        =   89
               Tag             =   "EWTINV"
               Top             =   840
               Width           =   3300
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Enabled         =   0   'False
               Height          =   252
               Index           =   10
               Left            =   5700
               TabIndex        =   92
               Tag             =   "0"
               Top             =   900
               Width           =   996
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Enabled         =   0   'False
               Height          =   252
               Index           =   9
               Left            =   5700
               TabIndex        =   88
               Tag             =   "0"
               Top             =   420
               Width           =   996
            End
         End
         Begin Threed.SSFrame frame 
            Height          =   2715
            Index           =   5
            Left            =   240
            TabIndex        =   69
            Top             =   480
            Width           =   6735
            _Version        =   65536
            _ExtentX        =   11880
            _ExtentY        =   4789
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
            Begin Threed.SSCheck swpath 
               Height          =   252
               Index           =   7
               Left            =   240
               TabIndex        =   49
               Tag             =   "kexpth"
               Top             =   1560
               Width           =   2004
               _Version        =   65536
               _ExtentX        =   3535
               _ExtentY        =   445
               _StockProps     =   78
               Caption         =   "Milk"
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
            Begin Threed.SSCheck swpath 
               Height          =   252
               Index           =   6
               Left            =   240
               TabIndex        =   48
               Tag             =   "kexpth"
               Top             =   1320
               Width           =   2004
               _Version        =   65536
               _ExtentX        =   3535
               _ExtentY        =   445
               _StockProps     =   78
               Caption         =   "Meat"
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
            Begin Threed.SSCheck swpath 
               Height          =   252
               Index           =   5
               Left            =   240
               TabIndex        =   46
               Tag             =   "kexpth"
               Top             =   720
               Width           =   2000
               _Version        =   65536
               _ExtentX        =   3535
               _ExtentY        =   445
               _StockProps     =   78
               Caption         =   "Other vegetables"
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
            Begin Threed.SSCheck swpath 
               Height          =   252
               Index           =   4
               Left            =   240
               TabIndex        =   45
               Tag             =   "kexpth"
               Top             =   480
               Width           =   2000
               _Version        =   65536
               _ExtentX        =   3535
               _ExtentY        =   445
               _StockProps     =   78
               Caption         =   "Leafy vegetables"
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
            Begin Threed.SSCheck swpath 
               Height          =   252
               Index           =   3
               Left            =   2640
               TabIndex        =   54
               Tag             =   "kexpth"
               Top             =   720
               Width           =   2004
               _Version        =   65536
               _ExtentX        =   3535
               _ExtentY        =   445
               _StockProps     =   78
               Caption         =   "Shower water"
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
            Begin Threed.SSCheck swpath 
               Height          =   252
               Index           =   1
               Left            =   2640
               TabIndex        =   53
               Tag             =   "kexpth"
               Top             =   480
               Width           =   2004
               _Version        =   65536
               _ExtentX        =   3535
               _ExtentY        =   445
               _StockProps     =   78
               Caption         =   "Drinking water"
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
            Begin Threed.SSOption swkex 
               Height          =   255
               Index           =   25
               Left            =   2820
               TabIndex        =   60
               Top             =   2280
               Width           =   1875
               _Version        =   65536
               _ExtentX        =   3307
               _ExtentY        =   450
               _StockProps     =   78
               Caption         =   "Indoor - Air"
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
               Value           =   -1  'True
            End
            Begin Threed.SSOption swkex 
               Height          =   255
               Index           =   17
               Left            =   2820
               TabIndex        =   59
               Top             =   2040
               Width           =   1875
               _Version        =   65536
               _ExtentX        =   3307
               _ExtentY        =   450
               _StockProps     =   78
               Caption         =   "Shower - Air"
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
            Begin Threed.SSCheck swpath 
               Height          =   252
               Index           =   0
               Left            =   2640
               TabIndex        =   58
               Tag             =   "kexpth"
               Top             =   1800
               Width           =   2004
               _Version        =   65536
               _ExtentX        =   3528
               _ExtentY        =   444
               _StockProps     =   78
               Caption         =   "Air - Volatiles from water"
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
            Begin Threed.SSCheck swpath 
               Height          =   252
               Index           =   9
               Left            =   240
               TabIndex        =   51
               Tag             =   "kexpth"
               Top             =   2040
               Width           =   2004
               _Version        =   65536
               _ExtentX        =   3535
               _ExtentY        =   445
               _StockProps     =   78
               Caption         =   "Shell Fish"
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
            Begin Threed.SSCheck swpath 
               Height          =   252
               Index           =   8
               Left            =   240
               TabIndex        =   50
               Tag             =   "kexpth"
               Top             =   1800
               Width           =   2004
               _Version        =   65536
               _ExtentX        =   3535
               _ExtentY        =   445
               _StockProps     =   78
               Caption         =   "Finned Fish"
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
            Begin Threed.SSCheck swpath 
               Height          =   252
               Index           =   13
               Left            =   2640
               TabIndex        =   56
               Tag             =   "kexpth"
               Top             =   1200
               Width           =   2004
               _Version        =   65536
               _ExtentX        =   3535
               _ExtentY        =   445
               _StockProps     =   78
               Caption         =   "Shoreline sediment"
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
            Begin Threed.SSCheck swpath 
               Height          =   252
               Index           =   10
               Left            =   2640
               TabIndex        =   55
               Tag             =   "kexpth"
               Top             =   960
               Width           =   2004
               _Version        =   65536
               _ExtentX        =   3535
               _ExtentY        =   445
               _StockProps     =   78
               Caption         =   "Swimming water"
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
            Begin Threed.SSCheck swpath 
               Height          =   252
               Index           =   20
               Left            =   4800
               TabIndex        =   66
               Tag             =   "kexpth"
               Top             =   1800
               Width           =   1400
               _Version        =   65536
               _ExtentX        =   2469
               _ExtentY        =   444
               _StockProps     =   78
               Caption         =   "Swimming "
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
            Begin Threed.SSCheck swpath 
               Height          =   252
               Index           =   21
               Left            =   4800
               TabIndex        =   68
               Tag             =   "kexpth"
               Top             =   2280
               Width           =   1400
               _Version        =   65536
               _ExtentX        =   2469
               _ExtentY        =   444
               _StockProps     =   78
               Caption         =   "Boating"
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
            Begin Threed.SSCheck swpath 
               Height          =   252
               Index           =   22
               Left            =   4800
               TabIndex        =   67
               Tag             =   "kexpth"
               Top             =   2040
               Width           =   1400
               _Version        =   65536
               _ExtentX        =   2469
               _ExtentY        =   444
               _StockProps     =   78
               Caption         =   "Shoreline"
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
            Begin Threed.SSCheck swpath 
               Height          =   252
               Index           =   2
               Left            =   4800
               TabIndex        =   62
               Tag             =   "kexpth"
               Top             =   480
               Width           =   1400
               _Version        =   65536
               _ExtentX        =   2469
               _ExtentY        =   444
               _StockProps     =   78
               Caption         =   "Shower"
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
            Begin Threed.SSCheck swpath 
               Height          =   252
               Index           =   12
               Left            =   4800
               TabIndex        =   64
               Tag             =   "kexpth"
               Top             =   960
               Width           =   1400
               _Version        =   65536
               _ExtentX        =   2469
               _ExtentY        =   444
               _StockProps     =   78
               Caption         =   "Shoreline"
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
            Begin Threed.SSCheck swpath 
               Height          =   252
               Index           =   11
               Left            =   4800
               TabIndex        =   63
               Tag             =   "kexpth"
               Top             =   720
               Width           =   1400
               _Version        =   65536
               _ExtentX        =   2469
               _ExtentY        =   444
               _StockProps     =   78
               Caption         =   "Swimming"
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
               Caption         =   "Dermal"
               Height          =   252
               Index           =   22
               Left            =   4680
               TabIndex        =   61
               Top             =   240
               Width           =   1400
            End
            Begin VB.Label lbl 
               Caption         =   "Other Ingestion"
               Height          =   252
               Index           =   19
               Left            =   2520
               TabIndex        =   52
               Top             =   240
               Width           =   2004
            End
            Begin VB.Label lbl 
               Caption         =   "Animal Product Ingestion"
               Height          =   252
               Index           =   20
               Left            =   120
               TabIndex        =   47
               Top             =   1080
               Width           =   2000
            End
            Begin VB.Label lbl 
               Caption         =   "Plant Product Ingestion"
               Height          =   252
               Index           =   21
               Left            =   120
               TabIndex        =   44
               Top             =   240
               Width           =   2000
            End
            Begin VB.Label lbl 
               Caption         =   "Inhalation"
               Height          =   252
               Index           =   24
               Left            =   2520
               TabIndex        =   57
               Top             =   1560
               Width           =   2004
            End
            Begin VB.Label lbl 
               Caption         =   "External"
               Height          =   252
               Index           =   26
               Left            =   4680
               TabIndex        =   65
               Top             =   1560
               Width           =   1400
            End
         End
         Begin Threed.SSFrame frame 
            Height          =   2715
            Index           =   6
            Left            =   -74760
            TabIndex        =   84
            Top             =   480
            Width           =   6735
            _Version        =   65536
            _ExtentX        =   11880
            _ExtentY        =   4789
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
            Begin Threed.SSCheck swCoverBox 
               Height          =   255
               Index           =   0
               Left            =   240
               TabIndex        =   206
               Top             =   240
               Width           =   200
               _Version        =   65536
               _ExtentX        =   353
               _ExtentY        =   450
               _StockProps     =   78
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
            Begin Threed.SSCheck swCoverBox 
               Height          =   255
               Index           =   1
               Left            =   240
               TabIndex        =   207
               Top             =   600
               Width           =   200
               _Version        =   65536
               _ExtentX        =   353
               _ExtentY        =   450
               _StockProps     =   78
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
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H008080FF&
               Enabled         =   0   'False
               Height          =   312
               Index           =   8
               Left            =   3660
               TabIndex        =   76
               Tag             =   "cirr"
               Top             =   1440
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H00C0FFC0&
               Enabled         =   0   'False
               Height          =   312
               Index           =   7
               Left            =   4620
               TabIndex        =   73
               Tag             =   "firr"
               Text            =   "1.0"
               Top             =   960
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H00C0FFC0&
               Enabled         =   0   'False
               Height          =   312
               Index           =   6
               Left            =   3660
               TabIndex        =   80
               Tag             =   "twtr"
               Text            =   "1.0"
               Top             =   1920
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               BackColor       =   &H00FFFFFF&
               Enabled         =   0   'False
               Height          =   288
               Index           =   8
               Left            =   4620
               Style           =   2  'Dropdown List
               TabIndex        =   77
               Tag             =   "L/m^2/mon"
               Top             =   1440
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               BackColor       =   &H00FFFFFF&
               Enabled         =   0   'False
               Height          =   288
               Index           =   6
               Left            =   4620
               Style           =   2  'Dropdown List
               TabIndex        =   81
               Tag             =   "day"
               Top             =   1920
               Width           =   1000
            End
            Begin Threed.SSCheck swtreat 
               Height          =   252
               Left            =   240
               TabIndex        =   83
               Tag             =   "EWLTRTL"
               Top             =   2280
               Width           =   3996
               _Version        =   65536
               _ExtentX        =   7049
               _ExtentY        =   445
               _StockProps     =   78
               Caption         =   " Domestic water is treated -- EW-LTRTL"
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
            Begin Threed.SSCheck swdrink 
               Height          =   252
               Left            =   240
               TabIndex        =   70
               Tag             =   "swdrink"
               Top             =   240
               Width           =   2004
               _Version        =   65536
               _ExtentX        =   3535
               _ExtentY        =   445
               _StockProps     =   78
               Caption         =   " Animal drinking"
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
               Value           =   -1  'True
            End
            Begin Threed.SSCheck swfeed 
               Height          =   252
               Left            =   240
               TabIndex        =   71
               Tag             =   "swfeed"
               Top             =   600
               Width           =   2004
               _Version        =   65536
               _ExtentX        =   3535
               _ExtentY        =   445
               _StockProps     =   78
               Caption         =   " Irrigation of animal feed"
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
            Begin Threed.SSCheck swCoverBox 
               Height          =   255
               Index           =   2
               Left            =   240
               TabIndex        =   208
               Top             =   2280
               Width           =   255
               _Version        =   65536
               _ExtentX        =   450
               _ExtentY        =   450
               _StockProps     =   78
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
               Caption         =   "Fraction of the year that surface water is used for irrigation -- EW-FIRR"
               Enabled         =   0   'False
               Height          =   432
               Index           =   7
               Left            =   240
               TabIndex        =   72
               Tag             =   "EWFIRR"
               Top             =   960
               Width           =   3300
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Enabled         =   0   'False
               Height          =   252
               Index           =   7
               Left            =   5700
               TabIndex        =   74
               Tag             =   "0"
               Top             =   1020
               Width           =   996
            End
            Begin VB.Label lbl 
               Caption         =   "Irrigation rate -- EW-CIRR"
               Enabled         =   0   'False
               Height          =   252
               Index           =   8
               Left            =   228
               TabIndex        =   75
               Tag             =   "EWCIRR"
               Top             =   1500
               Width           =   3300
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Enabled         =   0   'False
               Height          =   252
               Index           =   8
               Left            =   5700
               TabIndex        =   78
               Tag             =   "0"
               Top             =   1500
               Width           =   996
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Enabled         =   0   'False
               Height          =   252
               Index           =   6
               Left            =   5700
               TabIndex        =   82
               Tag             =   "0"
               Top             =   1980
               Width           =   996
            End
            Begin VB.Label lbl 
               Caption         =   "Domestic water distribution time -- EW-TWTR"
               Enabled         =   0   'False
               Height          =   252
               Index           =   6
               Left            =   240
               TabIndex        =   79
               Tag             =   "EWTWTR"
               Top             =   1980
               Width           =   3300
            End
         End
      End
      Begin Threed.SSFrame frame 
         Height          =   1455
         Index           =   11
         Left            =   -74760
         TabIndex        =   145
         Top             =   360
         Width           =   7215
         _Version        =   65536
         _ExtentX        =   12726
         _ExtentY        =   2566
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
         Begin VB.ComboBox soilmod 
            Height          =   288
            ItemData        =   "Expose.frx":045A
            Left            =   360
            List            =   "Expose.frx":0464
            Style           =   2  'Dropdown List
            TabIndex        =   144
            Tag             =   "soilmod"
            Top             =   840
            Width           =   6495
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   25
            Left            =   3780
            TabIndex        =   140
            Tag             =   "smed"
            Top             =   240
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   25
            Left            =   4740
            Style           =   2  'Dropdown List
            TabIndex        =   141
            Tag             =   "yr"
            Top             =   240
            Width           =   1000
         End
         Begin VB.Label Label1 
            Caption         =   "Soil Concentration Usage"
            Height          =   255
            Left            =   230
            TabIndex        =   143
            Top             =   600
            Width           =   3495
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   25
            Left            =   5820
            TabIndex        =   142
            Tag             =   "0"
            Top             =   300
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Exposure duration -- EM-SMED"
            Height          =   252
            Index           =   25
            Left            =   230
            TabIndex        =   139
            Tag             =   "EMSMED"
            Top             =   290
            Width           =   3500
         End
      End
      Begin Threed.SSFrame frame 
         Height          =   732
         Index           =   0
         Left            =   -74760
         TabIndex        =   5
         Top             =   360
         Width           =   7212
         _Version        =   65536
         _ExtentX        =   12721
         _ExtentY        =   1291
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
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   0
            Left            =   3780
            TabIndex        =   2
            Tag             =   "dgwed"
            Top             =   240
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   0
            Left            =   4740
            Style           =   2  'Dropdown List
            TabIndex        =   3
            Tag             =   "yr"
            Top             =   240
            Width           =   1000
         End
         Begin VB.Label lbl 
            Caption         =   "Exposure duration -- EG-DGWED"
            Height          =   252
            Index           =   0
            Left            =   230
            TabIndex        =   1
            Tag             =   "EGDGWED"
            Top             =   290
            Width           =   3500
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   0
            Left            =   5820
            TabIndex        =   4
            Tag             =   "0"
            Top             =   300
            Width           =   996
         End
      End
      Begin Threed.SSFrame frame 
         Height          =   732
         Index           =   4
         Left            =   -74760
         TabIndex        =   42
         Top             =   360
         Width           =   7212
         _Version        =   65536
         _ExtentX        =   12721
         _ExtentY        =   1291
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
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   4
            Left            =   3780
            TabIndex        =   39
            Tag             =   "dswed"
            Top             =   240
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   4
            Left            =   4740
            Style           =   2  'Dropdown List
            TabIndex        =   40
            Tag             =   "yr"
            Top             =   240
            Width           =   1000
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   4
            Left            =   5820
            TabIndex        =   41
            Tag             =   "0"
            Top             =   300
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Exposure duration -- EW-DSWED"
            Height          =   252
            Index           =   4
            Left            =   230
            TabIndex        =   38
            Tag             =   "EWDSWED"
            Top             =   290
            Width           =   3500
         End
      End
      Begin TabDlg.SSTab air_t 
         Height          =   3435
         Left            =   -74760
         TabIndex        =   110
         Top             =   1320
         Width           =   7215
         _ExtentX        =   12721
         _ExtentY        =   6054
         _Version        =   393216
         Style           =   1
         Tabs            =   2
         TabsPerRow      =   2
         TabHeight       =   529
         TabCaption(0)   =   "Pathways"
         TabPicture(0)   =   "Expose.frx":04D5
         Tab(0).ControlEnabled=   -1  'True
         Tab(0).Control(0)=   "frame(9)"
         Tab(0).Control(0).Enabled=   0   'False
         Tab(0).ControlCount=   1
         TabCaption(1)   =   "Deposition"
         TabPicture(1)   =   "Expose.frx":04F1
         Tab(1).ControlEnabled=   0   'False
         Tab(1).Control(0)=   "frame(10)"
         Tab(1).ControlCount=   1
         Begin Threed.SSFrame frame 
            Height          =   2595
            Index           =   9
            Left            =   240
            TabIndex        =   124
            Top             =   480
            Width           =   6735
            _Version        =   65536
            _ExtentX        =   11874
            _ExtentY        =   4586
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
            Begin Threed.SSCheck airpath 
               Height          =   252
               Index           =   7
               Left            =   240
               TabIndex        =   116
               Tag             =   "kexpth"
               Top             =   1560
               Width           =   2004
               _Version        =   65536
               _ExtentX        =   3535
               _ExtentY        =   445
               _StockProps     =   78
               Caption         =   "Milk"
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
            Begin Threed.SSCheck airpath 
               Height          =   252
               Index           =   6
               Left            =   240
               TabIndex        =   115
               Tag             =   "kexpth"
               Top             =   1320
               Width           =   2004
               _Version        =   65536
               _ExtentX        =   3535
               _ExtentY        =   445
               _StockProps     =   78
               Caption         =   "Meat"
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
            Begin Threed.SSCheck airpath 
               Height          =   252
               Index           =   5
               Left            =   240
               TabIndex        =   113
               Tag             =   "kexpth"
               Top             =   720
               Width           =   2004
               _Version        =   65536
               _ExtentX        =   3535
               _ExtentY        =   445
               _StockProps     =   78
               Caption         =   "Other vegetables"
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
            Begin Threed.SSCheck airpath 
               Height          =   252
               Index           =   4
               Left            =   240
               TabIndex        =   112
               Tag             =   "kexpth"
               Top             =   480
               Width           =   2004
               _Version        =   65536
               _ExtentX        =   3535
               _ExtentY        =   445
               _StockProps     =   78
               Caption         =   "Leafy vegetables"
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
            Begin Threed.SSCheck airpath 
               Height          =   252
               Index           =   14
               Left            =   3360
               TabIndex        =   118
               Tag             =   "kexpth"
               Top             =   480
               Width           =   2004
               _Version        =   65536
               _ExtentX        =   3535
               _ExtentY        =   445
               _StockProps     =   78
               Caption         =   "Soil - Ingestion"
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
            Begin Threed.SSCheck airpath 
               Height          =   252
               Index           =   19
               Left            =   3360
               TabIndex        =   119
               Tag             =   "kexpth"
               Top             =   720
               Width           =   2004
               _Version        =   65536
               _ExtentX        =   3528
               _ExtentY        =   444
               _StockProps     =   78
               Caption         =   "Soil - Inhalation"
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
            Begin Threed.SSCheck airpath 
               Height          =   252
               Index           =   23
               Left            =   3360
               TabIndex        =   121
               Tag             =   "kexpth"
               Top             =   1200
               Width           =   2004
               _Version        =   65536
               _ExtentX        =   3535
               _ExtentY        =   445
               _StockProps     =   78
               Caption         =   "Soil - External"
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
            Begin Threed.SSCheck airpath 
               Height          =   252
               Index           =   15
               Left            =   3360
               TabIndex        =   120
               Tag             =   "kexpth"
               Top             =   960
               Width           =   2004
               _Version        =   65536
               _ExtentX        =   3535
               _ExtentY        =   445
               _StockProps     =   78
               Caption         =   "Soil - Dermal"
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
            Begin Threed.SSCheck airpath 
               Height          =   252
               Index           =   24
               Left            =   3360
               TabIndex        =   122
               Tag             =   "kexpth"
               Top             =   1440
               Width           =   2004
               _Version        =   65536
               _ExtentX        =   3535
               _ExtentY        =   445
               _StockProps     =   78
               Caption         =   "Air - External"
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
            Begin Threed.SSCheck airpath 
               Height          =   252
               Index           =   18
               Left            =   3360
               TabIndex        =   123
               Tag             =   "kexpth"
               Top             =   1680
               Width           =   2004
               _Version        =   65536
               _ExtentX        =   3535
               _ExtentY        =   445
               _StockProps     =   78
               Caption         =   "Air Inhalation"
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
               Caption         =   "Other Pathways"
               Height          =   252
               Index           =   28
               Left            =   3240
               TabIndex        =   117
               Top             =   240
               Width           =   2004
            End
            Begin VB.Label lbl 
               Caption         =   "Animal Product Ingestion"
               Height          =   252
               Index           =   35
               Left            =   120
               TabIndex        =   114
               Top             =   1080
               Width           =   2004
            End
            Begin VB.Label lbl 
               Caption         =   "Plant Product Ingestion"
               Height          =   252
               Index           =   36
               Left            =   120
               TabIndex        =   111
               Top             =   240
               Width           =   2004
            End
         End
         Begin Threed.SSFrame frame 
            Height          =   2772
            Index           =   10
            Left            =   -74760
            TabIndex        =   138
            Top             =   480
            Width           =   6732
            _Version        =   65536
            _ExtentX        =   11874
            _ExtentY        =   4890
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
               Enabled         =   0   'False
               Height          =   312
               Index           =   51
               Left            =   4980
               TabIndex        =   137
               Tag             =   "rhoas"
               Text            =   "1.5"
               Top             =   2280
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H00C0FFC0&
               Enabled         =   0   'False
               Height          =   312
               Index           =   50
               Left            =   2100
               TabIndex        =   135
               Tag             =   "rhoas"
               Text            =   "1.5"
               Top             =   2280
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H008080FF&
               Enabled         =   0   'False
               Height          =   312
               Index           =   15
               Left            =   4020
               TabIndex        =   130
               Tag             =   "rhoas"
               Top             =   600
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H008080FF&
               Enabled         =   0   'False
               Height          =   312
               Index           =   14
               Left            =   4020
               TabIndex        =   126
               Tag             =   "tas"
               Top             =   240
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               BackColor       =   &H00FFFFFF&
               Enabled         =   0   'False
               Height          =   288
               Index           =   14
               Left            =   4980
               Style           =   2  'Dropdown List
               TabIndex        =   127
               Tag             =   "m"
               Top             =   240
               Width           =   900
            End
            Begin VB.ComboBox unit 
               BackColor       =   &H00FFFFFF&
               Enabled         =   0   'False
               Height          =   288
               Index           =   15
               Left            =   4980
               Style           =   2  'Dropdown List
               TabIndex        =   131
               Tag             =   "g/cm^3"
               Top             =   600
               Width           =   900
            End
            Begin VB.Label Label2 
               Caption         =   $"Expose.frx":050D
               Height          =   1152
               Left            =   240
               TabIndex        =   133
               Top             =   1080
               Width           =   6372
            End
            Begin VB.Label lbl 
               Caption         =   "Y-Coordinate"
               Height          =   252
               Index           =   51
               Left            =   3660
               TabIndex        =   136
               Tag             =   "EARHOAS"
               Top             =   2340
               Width           =   1296
            End
            Begin VB.Label lbl 
               Caption         =   "X-Coordinate"
               Height          =   252
               Index           =   50
               Left            =   780
               TabIndex        =   134
               Tag             =   "EARHOAS"
               Top             =   2340
               Width           =   1296
            End
            Begin VB.Label lbl 
               Caption         =   "Density of soil recieving deposition -- EA-RHOAS"
               Enabled         =   0   'False
               Height          =   252
               Index           =   15
               Left            =   240
               TabIndex        =   129
               Tag             =   "EARHOAS"
               Top             =   600
               Width           =   3804
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Enabled         =   0   'False
               Height          =   252
               Index           =   15
               Left            =   5940
               TabIndex        =   132
               Tag             =   "0"
               Top             =   660
               Width           =   760
            End
            Begin VB.Label ref 
               Caption         =   "Ref: 0"
               Enabled         =   0   'False
               Height          =   252
               Index           =   14
               Left            =   5940
               TabIndex        =   128
               Tag             =   "0"
               Top             =   300
               Width           =   760
            End
            Begin VB.Label lbl 
               Caption         =   "Thickness of soil recieving deposition -- EA-TAS"
               Enabled         =   0   'False
               Height          =   252
               Index           =   14
               Left            =   240
               TabIndex        =   125
               Tag             =   "EATAS"
               Top             =   240
               Width           =   3804
            End
         End
      End
      Begin Threed.SSFrame frame 
         Height          =   732
         Index           =   8
         Left            =   -74760
         TabIndex        =   109
         Top             =   360
         Width           =   7212
         _Version        =   65536
         _ExtentX        =   12721
         _ExtentY        =   1291
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
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   23
            Left            =   3780
            TabIndex        =   106
            Tag             =   "ated"
            Top             =   240
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   23
            Left            =   4740
            Style           =   2  'Dropdown List
            TabIndex        =   107
            Tag             =   "yr"
            Top             =   240
            Width           =   1000
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   23
            Left            =   5820
            TabIndex        =   108
            Tag             =   "0"
            Top             =   300
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Exposure duration -- EA-ATED"
            Height          =   252
            Index           =   23
            Left            =   230
            TabIndex        =   105
            Tag             =   "EAATED"
            Top             =   290
            Width           =   3500
         End
      End
      Begin Threed.SSFrame frame 
         Height          =   4390
         Index           =   14
         Left            =   -74760
         TabIndex        =   201
         Top             =   360
         Width           =   7215
         _Version        =   65536
         _ExtentX        =   12726
         _ExtentY        =   7743
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
         Begin VB.CommandButton cmdEstKd 
            Caption         =   "Soil Class - EC-CLASS"
            Height          =   435
            Left            =   4200
            TabIndex        =   172
            Tag             =   "wzclass"
            Top             =   600
            Width           =   2172
         End
         Begin VB.ComboBox ProgKds 
            Height          =   288
            ItemData        =   "Expose.frx":069A
            Left            =   3960
            List            =   "Expose.frx":069C
            Style           =   2  'Dropdown List
            TabIndex        =   184
            Tag             =   "WASUBKD"
            Top             =   2240
            Visible         =   0   'False
            Width           =   2409
         End
         Begin VB.ComboBox ConKds 
            Height          =   288
            ItemData        =   "Expose.frx":069E
            Left            =   484
            List            =   "Expose.frx":06A0
            Style           =   2  'Dropdown List
            TabIndex        =   178
            Tag             =   "WASUBKD"
            Top             =   2240
            Width           =   2409
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   35
            Left            =   1452
            Style           =   2  'Dropdown List
            TabIndex        =   176
            Tag             =   "ml/g"
            Top             =   1860
            Width           =   1000
         End
         Begin VB.ComboBox cboKdProgeny 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            ItemData        =   "Expose.frx":06A2
            Left            =   3960
            List            =   "Expose.frx":06A4
            Style           =   2  'Dropdown List
            TabIndex        =   180
            Top             =   1500
            Width           =   2412
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   36
            Left            =   4920
            Style           =   2  'Dropdown List
            TabIndex        =   182
            Tag             =   "ml/g"
            Top             =   1860
            Width           =   1000
         End
         Begin VB.ComboBox cboKdParent 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            ItemData        =   "Expose.frx":06A6
            Left            =   480
            List            =   "Expose.frx":06A8
            Style           =   2  'Dropdown List
            TabIndex        =   174
            Top             =   1500
            Width           =   2412
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   34
            Left            =   4920
            Style           =   2  'Dropdown List
            TabIndex        =   199
            Tag             =   "cm/yr"
            Top             =   3839
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   33
            Left            =   4920
            Style           =   2  'Dropdown List
            TabIndex        =   195
            Tag             =   "g/cm^3"
            Top             =   3487
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   32
            Left            =   4920
            Style           =   2  'Dropdown List
            TabIndex        =   191
            Tag             =   "fraction"
            Top             =   3124
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   31
            Left            =   4920
            Style           =   2  'Dropdown List
            TabIndex        =   187
            Tag             =   "cm"
            Top             =   2761
            Width           =   1000
         End
         Begin VB.ComboBox cb 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            ItemData        =   "Expose.frx":06AA
            Left            =   480
            List            =   "Expose.frx":06B4
            Style           =   2  'Dropdown List
            TabIndex        =   171
            Tag             =   "leachoption"
            Top             =   720
            Width           =   3160
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   288
            Index           =   35
            Left            =   480
            TabIndex        =   175
            Tag             =   "WASUBKD"
            Top             =   1860
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   288
            Index           =   36
            Left            =   3960
            TabIndex        =   181
            Tag             =   "WASUBKD"
            Top             =   1860
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   288
            Index           =   34
            Left            =   3960
            TabIndex        =   198
            Tag             =   "LEACV"
            Top             =   3839
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   288
            Index           =   33
            Left            =   3960
            TabIndex        =   194
            Tag             =   "BULKD"
            Top             =   3487
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   288
            Index           =   32
            Left            =   3960
            TabIndex        =   190
            Tag             =   "MOIST"
            Top             =   3124
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   288
            Index           =   31
            Left            =   3960
            TabIndex        =   186
            Tag             =   "THICK"
            Top             =   2761
            Width           =   1000
         End
         Begin VB.Label lbl 
            Caption         =   "Total infiltration rate -- EC-LEACV"
            Height          =   253
            Index           =   34
            Left            =   242
            TabIndex        =   197
            Tag             =   "ECLEACV"
            Top             =   3839
            Width           =   3300
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   253
            Index           =   34
            Left            =   5995
            TabIndex        =   200
            Tag             =   "0"
            Top             =   3872
            Width           =   900
         End
         Begin VB.Label lbl 
            Caption         =   "Progeny (Kd) (Values same as like parent)"
            Height          =   252
            Index           =   92
            Left            =   3720
            TabIndex        =   179
            Top             =   1260
            Width           =   3240
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   253
            Index           =   36
            Left            =   5995
            TabIndex        =   183
            Tag             =   "0"
            Top             =   1920
            Width           =   900
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   253
            Index           =   35
            Left            =   2519
            TabIndex        =   177
            Tag             =   "0"
            Top             =   1920
            Width           =   1001
         End
         Begin VB.Label lbl 
            Caption         =   "Soil adsorption coefficient (Kd)"
            Height          =   253
            Index           =   91
            Left            =   242
            TabIndex        =   173
            Top             =   1260
            Width           =   3003
         End
         Begin VB.Label lbl 
            Caption         =   "Surface soil bulk density -- EC-BULKD"
            Height          =   253
            Index           =   33
            Left            =   242
            TabIndex        =   193
            Tag             =   "ECBULKD"
            Top             =   3487
            Width           =   3300
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   253
            Index           =   33
            Left            =   5995
            TabIndex        =   196
            Tag             =   "0"
            Top             =   3520
            Width           =   900
         End
         Begin VB.Label lbl 
            Caption         =   "Surface soil moisture content -- EC-MOIST"
            Height          =   253
            Index           =   32
            Left            =   242
            TabIndex        =   189
            Tag             =   "ECMOIST"
            Top             =   3124
            Width           =   3300
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   253
            Index           =   32
            Left            =   5995
            TabIndex        =   192
            Tag             =   "0"
            Top             =   3157
            Width           =   900
         End
         Begin VB.Label lbl 
            Caption         =   "Surface soil thickness -- EC-THICK"
            Height          =   253
            Index           =   31
            Left            =   242
            TabIndex        =   185
            Tag             =   "ECTHICK"
            Top             =   2761
            Width           =   3300
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   253
            Index           =   31
            Left            =   5995
            TabIndex        =   188
            Tag             =   "0"
            Top             =   2827
            Width           =   900
         End
         Begin VB.Label blbl 
            Caption         =   "Leachrate selection option -- EC-LEACHOPTION"
            Height          =   435
            Index           =   15
            Left            =   240
            TabIndex        =   170
            Tag             =   "ECLEACHOPTION"
            Top             =   240
            Width           =   3030
         End
      End
      Begin Threed.SSFrame frame 
         Height          =   4390
         Index           =   13
         Left            =   240
         TabIndex        =   169
         Top             =   360
         Width           =   7215
         _Version        =   65536
         _ExtentX        =   12726
         _ExtentY        =   7743
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
            Index           =   38
            Left            =   4620
            Style           =   2  'Dropdown List
            TabIndex        =   164
            Tag             =   "yr"
            Top             =   840
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   37
            Left            =   4620
            Style           =   2  'Dropdown List
            TabIndex        =   160
            Tag             =   "yr"
            Top             =   360
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   39
            Left            =   3660
            TabIndex        =   167
            Tag             =   "ntimes"
            Top             =   1320
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   38
            Left            =   3660
            TabIndex        =   163
            Tag             =   "maxtim"
            Top             =   840
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   37
            Left            =   3660
            TabIndex        =   159
            Tag             =   "texpos"
            Top             =   360
            Width           =   1000
         End
         Begin VB.Label lbl 
            Caption         =   "Number of time points for evaluation - EC-NTIMES"
            Height          =   432
            Index           =   39
            Left            =   240
            TabIndex        =   166
            Tag             =   "ECNTIMES"
            Top             =   1320
            Width           =   3300
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   39
            Left            =   5700
            TabIndex        =   168
            Tag             =   "0"
            Top             =   1380
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Maximum time for reporting - EC-MAXTIM"
            Height          =   432
            Index           =   38
            Left            =   240
            TabIndex        =   162
            Tag             =   "ECMAXTIM"
            Top             =   840
            Width           =   3300
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   38
            Left            =   5700
            TabIndex        =   165
            Tag             =   "0"
            Top             =   900
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   37
            Left            =   5700
            TabIndex        =   161
            Tag             =   "0"
            Top             =   420
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Time to start exposure computation -EC-TEXPOS"
            Height          =   432
            Index           =   37
            Left            =   240
            TabIndex        =   158
            Tag             =   "ECTEXPOS"
            Top             =   360
            Width           =   3300
         End
      End
      Begin Threed.SSFrame frame 
         Height          =   4390
         Index           =   15
         Left            =   -74760
         TabIndex        =   209
         Top             =   360
         Width           =   7200
         _Version        =   65536
         _ExtentX        =   12700
         _ExtentY        =   7743
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
         Begin VB.CommandButton SSCommand5 
            Caption         =   ">"
            Height          =   250
            Left            =   3396
            TabIndex        =   210
            Top             =   490
            Width           =   230
         End
         Begin VB.CommandButton SSCommand4 
            Caption         =   "<"
            Height          =   250
            Left            =   3144
            TabIndex        =   211
            Top             =   490
            Width           =   250
         End
         Begin VB.CommandButton SSCommand2 
            Caption         =   ">"
            Height          =   250
            Left            =   3396
            TabIndex        =   212
            Top             =   2230
            Width           =   230
         End
         Begin VB.CommandButton SSCommand1 
            Caption         =   "<"
            Height          =   250
            Left            =   3144
            TabIndex        =   213
            Top             =   2230
            Width           =   250
         End
         Begin VB.CommandButton Command4 
            Caption         =   "<"
            Height          =   250
            Left            =   6384
            TabIndex        =   214
            Top             =   2230
            Width           =   250
         End
         Begin VB.CommandButton Command3 
            Caption         =   ">"
            Height          =   250
            Left            =   6636
            TabIndex        =   215
            Top             =   2230
            Width           =   230
         End
         Begin VB.CommandButton Command2 
            Caption         =   "<"
            Height          =   250
            Left            =   6384
            TabIndex        =   216
            Top             =   490
            Width           =   250
         End
         Begin VB.CommandButton Command1 
            Caption         =   ">"
            Height          =   250
            Left            =   6636
            TabIndex        =   217
            Top             =   490
            Width           =   230
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   40
            Left            =   3960
            TabIndex        =   218
            Tag             =   "PROPERTIES"
            Top             =   960
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   40
            Left            =   4917
            Style           =   2  'Dropdown List
            TabIndex        =   219
            Tag             =   "mg/l"
            Top             =   960
            Width           =   1000
         End
         Begin VB.ComboBox cboVarParent 
            Height          =   288
            Left            =   240
            Style           =   2  'Dropdown List
            TabIndex        =   220
            Top             =   480
            Width           =   2920
         End
         Begin VB.ComboBox cboVarProgeny 
            Height          =   288
            Left            =   240
            Style           =   2  'Dropdown List
            TabIndex        =   221
            Top             =   2222
            Width           =   2920
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   41
            Left            =   3960
            TabIndex        =   222
            Tag             =   "PROPERTIES"
            Top             =   2640
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   41
            Left            =   4917
            Style           =   2  'Dropdown List
            TabIndex        =   223
            Tag             =   "mg/l"
            Top             =   2640
            Width           =   1000
         End
         Begin VB.ComboBox ConParms 
            Height          =   288
            Left            =   3960
            Style           =   2  'Dropdown List
            TabIndex        =   224
            Top             =   480
            Width           =   2450
         End
         Begin VB.ComboBox ProgParms 
            Height          =   288
            Left            =   3960
            Style           =   2  'Dropdown List
            TabIndex        =   225
            Top             =   2222
            Width           =   2450
         End
         Begin VB.Label lbl 
            Caption         =   "Parameters"
            Height          =   253
            Index           =   42
            Left            =   3839
            TabIndex        =   226
            Top             =   240
            Width           =   3003
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   40
            Left            =   6000
            TabIndex        =   227
            Tag             =   "0"
            Top             =   1050
            Width           =   1005
         End
         Begin VB.Label lbl 
            Caption         =   "Constituent - EC-CNAME"
            Height          =   252
            Index           =   40
            Left            =   120
            TabIndex        =   228
            Tag             =   "PROPERTIES"
            Top             =   240
            Width           =   3432
         End
         Begin VB.Label lbl 
            Caption         =   "Progeny - EC-CNAME"
            Height          =   252
            Index           =   41
            Left            =   120
            TabIndex        =   229
            Tag             =   "PROPERTIES"
            Top             =   1980
            Width           =   3492
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   41
            Left            =   6000
            TabIndex        =   230
            Tag             =   "0"
            Top             =   2700
            Width           =   1005
         End
         Begin VB.Label lbl 
            Caption         =   "Parameters  (Values same as like parent)"
            Height          =   253
            Index           =   43
            Left            =   3839
            TabIndex        =   231
            Top             =   1980
            Width           =   3003
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
   Begin VB.Menu custom 
      Caption         =   "&Customize"
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
         Caption         =   "How to..."
      End
      Begin VB.Menu about 
         Caption         =   "&About"
      End
   End
End
Attribute VB_Name = "Exposure"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

' Constants for Kd value lookup
Const SELECT_LOOKUP = 0
Const DATABASE_LOOKUP = 1
Const EQUATION_LOOKUP = 2
Const TABLE_LOOKUP = 3


Dim EXPType(1 To 7) As Long      'ie. knpath
Dim temp As parmrec
Dim loadng As Boolean

'Added flags to control the behaviour of the added kd combo box
Dim oldKdParent As String
Dim oldKdProgeny As String
Dim prevLeachIdx As Long

Dim lopt As Long
Dim flagclick As Boolean
Dim keystate As Boolean

'Added flags to control the behaviour of the added parms combo box
Dim oldProgeny As String
Dim oldParent As String
Dim prevSelectedIdx As Long
Dim prevSelectedProgIdx As Long

' To resolve indexs from fui to mui
Dim f_numcon As Long

Private Sub leachtab()
Dim i As Long

  If EXPType(SO_TYPE) > 0 Then
    For i = 0 To ConParms.ListCount - 1
      If ConParms.list(i) = "Physical loss half-time in Surface Soil - EC-SHALF" Then Exit For
    Next
    If soilmod.ListIndex = 1 Then
      If i = ConParms.ListCount Then
        ConParms.AddItem "Physical loss half-time in Surface Soil - EC-SHALF", 0
        ConParms.ItemData(ConParms.NewIndex) = SO_HALF_LIFE_IDX
        ProgParms.AddItem "Physical loss half-time in Surface Soil - EC-SHALF", 0
        ProgParms.ItemData(ProgParms.NewIndex) = SO_HALF_LIFE_IDX
      End If
    Else
      If i <> ConParms.ListCount Then
        ConParms.RemoveItem i
        ConParms.ListIndex = 0
        ProgParms.RemoveItem i
        ProgParms.ListIndex = 0
      End If
    End If
  End If
  
  exp_t.TabVisible(LR_TAB_IDX) = False
  'measured soil check
  If exp_t.TabVisible(SO_TAB_IDX) And soilmod.ListIndex = 1 Then
    exp_t.TabVisible(LR_TAB_IDX) = True
    cb_Click
    Exit Sub
  End If
  
  'air check
  If exp_t.TabVisible(AT_TAB_IDX) Then
    For i = 4 To 23
      Select Case i
        Case 4 To 7, 14, 15, 19, 23
          If airpath(i).value Then
            exp_t.TabVisible(LR_TAB_IDX) = True
            cb_Click
            Exit Sub
          End If
      End Select
    Next
  End If
  
  'ground water and surface water checks
  If exp_t.TabVisible(GW_TAB_IDX) Then
    For i = 4 To 7
      If gwpath(i).value Then
        Select Case i
        Case 4, 5
          exp_t.TabVisible(LR_TAB_IDX) = True
          cb_Click
          Exit Sub
        Case 6, 7
          If gwfeed Then
            exp_t.TabVisible(LR_TAB_IDX) = True
            cb_Click
            Exit Sub
          End If
        End Select
      End If
    Next
  End If
  
  If exp_t.TabVisible(SW_TAB_IDX) Then
    For i = 4 To 7
      If swpath(i).value Then
        Select Case i
        Case 4, 5
          exp_t.TabVisible(LR_TAB_IDX) = True
          cb_Click
          Exit Sub
        Case 6, 7
          If swfeed Then
            exp_t.TabVisible(LR_TAB_IDX) = True
            cb_Click
            Exit Sub
          End If
        End Select
      End If
    Next
  End If
       
End Sub

Function getListParmName(ctlList As Control) As String
  ' fail safe return value
  getListParmName = ""
  
  Select Case ctlList.name
    Case "ConParms", "ProgParms"
      Select Case (ctlList.ItemData(ctlList.ListIndex))
        Case GW_HALF_LIFE_IDX
          getListParmName = "ECGHALF"
        Case SW_HALF_LIFE_IDX
          getListParmName = "ECWHALF"
        Case AT_HALF_LIFE_IDX
          getListParmName = "ECTHALF"
        Case SO_HALF_LIFE_IDX
          getListParmName = "ECSHALF"
      End Select
  End Select
End Function

Function getUnitType(ParmName As String) As String
  ' fail safe return value
  getUnitType = ""
  
  Select Case ParmName
    Case "ECSOL"
      getUnitType = "Mass/LiquidVolume"
    Case "ECSOL-RAD"
      getUnitType = "Activity/LiquidVolume"
    Case "ECGHALF", "ECWHALF", "ECTHALF", "ECSHALF"
      getUnitType = "Time"
  End Select
  
End Function

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

Private Sub Timer1_Timer()
  mes = Mid(mes, 2) & Mid(mes, 1, 1)
End Sub

Private Sub fillet(idx As Long)
  ref(idx).Tag = temp.ref
  ref(idx).Caption = "Ref:" & Str(temp.ref)
  On Error Resume Next
  If temp.cunit <> "N/A" Then set_unit Unit(idx), temp.uunit
  If idx = EC_NTIMES Or idx = SW_FISH_CORR Then
    txt(idx).Text = temp.pval
  Else
    txt(idx).Text = convert(temp.cunit, temp.uunit, Val(temp.pval))
  End If
End Sub

Sub addmodelparm()
  If m_numcon < temp.idx1 Then
    m_numcon = temp.idx1
    ReDim Preserve model(temp.idx1) As model_contam_param
  End If
  If model(temp.idx1).numprog < temp.idx2 Then
    model(temp.idx1).numprog = temp.idx2
    ReDim Preserve model(temp.idx1).progeny(temp.idx2) As model_progeny_param
  End If
End Sub

Sub addcon()
  If f_numcon < temp.idx2 Then
    f_numcon = temp.idx2
    ReDim Preserve con(f_numcon) As contam_param
  End If
  If con(temp.idx2).numprog < temp.idx3 Then
    con(temp.idx2).numprog = temp.idx3
    ReDim Preserve con(temp.idx2).progeny(temp.idx3) As contam_progeny_param
  End If
End Sub

Private Sub loaddef()
  Dim i As Long
  Dim j As Long
  Dim m As Long
  Dim numref As Long
  Dim abrv As String
  Dim refer As String
  Dim pre As String
  Dim name As String
  Dim fle As csv
  
  On Error GoTo ErrorHandler
  For i = 0 To 15
    If i = 3 Then i = 4
    frame(i).Enabled = True
  Next
  
  If open_csv(fle, App.Path & "\mepexp.rf_", F_READ) Then
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
    For i = 1 To numref
      temp.pname = get_val(fle)
      temp.ref = refs(Val(get_val(fle)))
      temp.pval = get_val(fle)
      temp.cunit = get_val(fle)
      get_line fle
      
      m = -1
      pre = Left(temp.pname, 2)
      name = Right(temp.pname, Len(temp.pname) - 3)
      Select Case name
        Case "cirr"
          If pre = "eg" Then m = GW_IRR_RATE
          If pre = "ew" Then m = SW_IRR_RATE
        Case "firr"
          If pre = "eg" Then m = GW_IRR_FRACTION
          If pre = "ew" Then m = SW_IRR_FRACTION
        Case "twtr"
          If pre = "eg" Then m = GW_DOMESTIC_DIST
          If pre = "ew" Then m = SW_DOMESTIC_DIST
        Case Else
          For j = 0 To 39
            Select Case j
            Case 13, 16 To 22, 24, 26 To 30
            Case Else
              If txt(j).Tag = name Then
                m = j
                Exit For
              End If
            End Select
          Next
      End Select
      If m > -1 Then
        ref(m).Tag = temp.ref
        ref(m).Caption = "Ref:" & Str(temp.ref)
        txt(m).Text = temp.pval
        If temp.cunit <> "" And temp.cunit <> "fraction" Then set_unit Unit(m), temp.cunit
        er m, txt(m).Enabled
      End If
    Next
    close_csv fle
    For i = 0 To 15
      If i = 3 Then i = 4
      frame(i).Enabled = False
    Next
    exp_t_Click exp_t.Tab
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
  Dim j As Long
  Dim k As Long
  Dim m As Long
  Dim mx As Long
  Dim uidx As Long
  Dim pcnt As Long
  Dim fle As parmfile
  Dim sval As Boolean
  Dim skip As Boolean
  Dim mcp As model_contam_param
  
  On Error GoTo ErrorHandler
  
  If open_parm(fle, FUIName, 2) Then
    Do Until EOCF(fle.file)
      If read_parmrec(fle, temp) Then
        Select Case temp.pname
          Case "fui"
            For m = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                If temp.idx1 = siteIdx Then
                  Select Case temp.pname
                    Case "expdespath"
                      DesName = temp.pval
                    Case "expx":
                      If temp.idx2 = modIdx Then
                        txt(50).Text = temp.pval
                        txt(50).Enabled = False
                      End If
                    Case "expy":
                      If temp.idx2 = modIdx Then
                        txt(51).Text = temp.pval
                        txt(51).Enabled = False
                      End If
                    Case "clktype"
                      addcon
                      If temp.idx3 = 0 Then
                        con(temp.idx2).kind = CInt(temp.pval)
                        If con(temp.idx2).kind = 1 Then con(temp.idx2).rad = True
                      Else
                        con(temp.idx2).progeny(temp.idx3).kind = CInt(temp.pval)
                      End If
                    Case "fscname"
                      addcon
                      If temp.idx3 = 0 Then
                        con(temp.idx2).name = temp.pval
                      Else
                        con(temp.idx2).progeny(temp.idx3).name = temp.pval
                      End If
                    Case "fscasid"
                      addcon
                      If temp.idx3 = 0 Then
                        con(temp.idx2).cas = temp.pval
                      Else
                        con(temp.idx2).progeny(temp.idx3).cas = temp.pval
                      End If
                    Case "clkd", "clkoc", "cltphalf", "clwphalf", "clgphalf", "clsphalf"
                      addcon
                      Select Case temp.pname
                        Case "clkd":      i = KD_IDX
                        Case "clkoc":     i = KOC_IDX
                        Case "clgphalf":   i = GW_HALF_LIFE_IDX
                        Case "clwphalf":   i = SW_HALF_LIFE_IDX
                        Case "cltphalf":   i = AT_HALF_LIFE_IDX
                        Case "clsphalf":   i = SO_HALF_LIFE_IDX
                      End Select
                        If temp.idx3 = 0 Then
                          con(temp.idx2).param(i) = temp.pval
                        Else
                          con(temp.idx2).progeny(temp.idx3).param(i) = temp.pval
                        End If
                  End Select
                End If
              End If
            Next
          Case "csm"
            mx = -1
            For i = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                'assumes modname will always occur before sink variables
                If temp.pname = "modid" And temp.pval = ModName Then
                  mx = temp.idx2
                End If
                If mx > -1 And temp.idx2 = mx Then
                  Select Case temp.pname
                    Case "modsrcqual"
                          If InStr(temp.pval, "Aquifer") Then
                            exp_t.TabVisible(GW_TAB_IDX) = True
                            EXPType(GW_TYPE) = 1
                          End If
                          If InStr(temp.pval, "Surface water") Then
                            exp_t.TabVisible(SW_TAB_IDX) = True
                            EXPType(SW_TYPE) = 1
                          End If
                          If InStr(temp.pval, "Air") Or InStr(temp.pval, "Polar Air") Or InStr(temp.pval, "Cartesian Air") Then
                            exp_t.TabVisible(AT_TAB_IDX) = True
                            EXPType(AT_TYPE) = 1
                          End If
                          If InStr(temp.pval, "Soil") Then
                            exp_t.TabVisible(SO_TAB_IDX) = True
                            EXPType(SO_TYPE) = 1
                          End If
                  End Select
                End If
              End If
            Next
          Case ModName
            Loading.Gauge1.Max = Val(temp.idx1)
            Loading.Gauge1.value = 0
            For m = 1 To temp.idx1
              Loading.update
              If read_parmrec(fle, temp) Then
                Select Case temp.pname
                  Case "CVTFormat":    CVTFormat = "General Number" 'CVTFormat = temp.pval
                  Case "ECCLASS":      soil.Index = Val(temp.pval)
                  Case "ECSAND":       soil.sand = temp.pval
                  Case "ECSILT":       soil.silt = temp.pval
                  Case "ECCLAY":       soil.clay = temp.pval
                  Case "ECOMC":        soil.organic = temp.pval
                  Case "ECIRON":       soil.iron = temp.pval
                  Case "ECPH":         soil.ph = temp.pval
                  Case "cirr"
                    If temp.idx1 = GW_TYPE Then fillet GW_IRR_RATE
                    If temp.idx1 = SW_TYPE Then fillet SW_IRR_RATE
                  Case "ltrtl"
                    If Val(temp.pval) = 1 Then
                      sval = True
                    Else
                      sval = False
                    End If
                    If temp.idx1 = GW_TYPE Then gwtreat.value = sval
                    If temp.idx1 = SW_TYPE Then swtreat.value = sval
                  Case "twtr"
                    If temp.idx1 = GW_TYPE Then fillet GW_DOMESTIC_DIST
                    If temp.idx1 = SW_TYPE Then fillet SW_DOMESTIC_DIST
                  Case "firr"
                    If temp.idx1 = GW_TYPE Then fillet GW_IRR_FRACTION
                    If temp.idx1 = SW_TYPE Then fillet SW_IRR_FRACTION
                  Case "gwdrink"
                    gwdrink.value = False
                    If temp.pval = "True" Then gwdrink.value = True
                  Case "gwfeed"
                    gwfeed.value = False
                    If temp.pval = "True" Then gwfeed.value = True
                  Case "swdrink"
                    swdrink.value = False
                    If temp.pval = "True" Then swdrink.value = True
                  Case "swfeed"
                    swfeed.value = False
                    If temp.pval = "True" Then swfeed.value = True
                  Case "soilmod"
                    If temp.pval = "True" Then
                      soilmod.ListIndex = 1
                    Else
                      soilmod.ListIndex = 0
                    End If
                  Case "kexpth"
                    sval = False
                    If Val(temp.pval) = 1 Then sval = True
                    Select Case temp.idx1
                      Case 17, 25
                        If sval Then
                          If temp.idx2 = GW_TYPE Then
                            gwpath(0).value = sval
                            gwkex(temp.idx1).value = sval
                          Else
                            swpath(0).value = sval
                            swkex(temp.idx1).value = sval
                          End If
                        End If
                      Case Else
                        On Error Resume Next
                        Select Case temp.idx2
                          Case GW_TYPE
                            gwpath(temp.idx1).value = sval
                          Case SW_TYPE
                            swpath(temp.idx1).value = sval
                          Case AT_TYPE
                            airpath(temp.idx1).value = sval
                          Case SO_TYPE
                            slpath(temp.idx1).value = sval
                        End Select
                        On Error GoTo 0
                    End Select
                  Case "casid"
                    addmodelparm
                    If temp.idx2 = 0 Then
                      model(temp.idx1).cas = temp.pval
                    Else
                      model(temp.idx1).progeny(temp.idx2).cas = temp.pval
                    End If
                  Case "soilkd", "soillr"
                    addmodelparm
                    If temp.pname = "soillr" Then lopt = LR_IDX
                    If temp.pname = "soilkd" Then lopt = KD_IDX
                      If temp.idx2 = 0 Then
                        model(temp.idx1).idx = 0
                        model(temp.idx1).param(lopt).value = convert(temp.cunit, temp.uunit, Val(temp.pval))
                        model(temp.idx1).param(lopt).uunt = temp.uunit
                        model(temp.idx1).param(lopt).ref = temp.ref
                      Else
                          model(temp.idx1).progeny(temp.idx2).idx = 0
                          model(temp.idx1).progeny(temp.idx2).param(lopt).value = convert(temp.cunit, temp.uunit, Val(temp.pval))
                          model(temp.idx1).progeny(temp.idx2).param(lopt).uunt = temp.uunit
                          model(temp.idx1).progeny(temp.idx2).param(lopt).ref = temp.ref
                      End If
                  Case "ecghalf", "ecwhalf", "ecthalf", "ecshalf"   ', "ecsol", "ecrsol"
                    addmodelparm
                   Select Case temp.pname
                     Case "ecghalf": i = GW_HALF_LIFE_IDX
                     Case "ecwhalf": i = SW_HALF_LIFE_IDX
                     Case "ecthalf": i = AT_HALF_LIFE_IDX
                     Case "ecshalf": i = SO_HALF_LIFE_IDX
                   End Select
                    If temp.idx2 = 0 Then
                      model(temp.idx1).idx = 0
                      If temp.pval <> "EMPTY" Then model(temp.idx1).param(i).value = convert(temp.cunit, temp.uunit, Val(temp.pval))
                      model(temp.idx1).param(i).uunt = temp.uunit
                      model(temp.idx1).param(i).ref = temp.ref
                    Else
                        model(temp.idx1).progeny(temp.idx2).idx = 0
                        If temp.pval <> "EMPTY" Then model(temp.idx1).progeny(temp.idx2).param(i).value = convert(temp.cunit, temp.uunit, Val(temp.pval))
                        model(temp.idx1).progeny(temp.idx2).param(i).uunt = temp.uunit
                        model(temp.idx1).progeny(temp.idx2).param(i).ref = temp.ref
                    End If
                Case Else
                    For i = 0 To 39
                      Select Case i
                      Case 13, 16 To 22, 24, 26 To 30
                      Case Else
                        If txt(i).Tag = temp.pname Then
                          fillet i
                          Exit For
                        End If
                      End Select
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
    SetFormat Me
  
'resolve contaminant differences          if contaminant no longer exists its index is 0
    For i = 1 To f_numcon
      For j = 1 To m_numcon
        'Note   Once a With block is entered, object can't be changed. As a result,
        'you can't use a single With statement to affect a number of different objects.
        'You can nest With statements by placing one With block within another. However,
        'because members of outer With blocks are masked within the inner With blocks,
        'you must provide a fully qualified object reference in an inner With block to
        'any member of an object in an outer With block.
        
        ' the following is necessary for the above reasons, which cause problems later
        ' when code to redimension model case run-time errors because the object is locked
        If model(j).cas = con(i).cas Then
          model(j).idx = i
          For pcnt = 0 To MAX_CONTAM_PARAM
            uidx = pcnt
            If pcnt = 3 Then pcnt = 4
            If pcnt > 3 Then uidx = 4
            If model(j).param(pcnt).value = "" Or model(j).param(pcnt).value = Empty Then
              model(j).param(pcnt).value = con(i).param(pcnt)
              model(j).param(pcnt).uunt = uStr(uidx)
              model(j).param(pcnt).ref = 0
            End If
          Next
          Exit For
        End If
      Next j
      
      If j > m_numcon Then
        m_numcon = j
        ReDim Preserve model(j) As model_contam_param
        model(j).cas = con(i).cas
        model(j).idx = i
        For pcnt = 0 To MAX_CONTAM_PARAM
          uidx = pcnt
          If pcnt = 3 Then pcnt = 4
          If pcnt > 3 Then uidx = 4
          model(j).param(pcnt).value = con(i).param(pcnt)
          model(j).param(pcnt).uunt = uStr(uidx)
          model(j).param(pcnt).ref = 0
        Next
      End If
    Next i
    
    For i = 1 To m_numcon
      If model(i).idx > 0 Then
        cboKdParent.AddItem con(model(i).idx).name
        cboKdParent.ItemData(cboKdParent.NewIndex) = i
        cboVarParent.AddItem con(model(i).idx).name
        cboVarParent.ItemData(cboVarParent.NewIndex) = i
      End If
    Next i
      
    ConParms.Clear
    'all media get these use these half lifes
    If ConParms.ListCount = 0 Then
      ConParms.AddItem "Physical loss half-time in Surface Soil - EC-SHALF"
      ConParms.ItemData(ConParms.NewIndex) = SO_HALF_LIFE_IDX
    End If
    ConParms.AddItem "Physical loss half-time in Ground Water - EC-GHALF"
    ConParms.ItemData(ConParms.NewIndex) = GW_HALF_LIFE_IDX
    If EXPType(GW_TYPE) > 0 Or EXPType(AT_TYPE) > 0 Or EXPType(SW_TYPE) > 0 Then
      ConParms.AddItem "Physical loss half-time in Air - EC-THALF"
      ConParms.ItemData(ConParms.NewIndex) = AT_HALF_LIFE_IDX
    End If
    If EXPType(SW_TYPE) > 0 Then
      ConParms.AddItem "Physical loss half-time in Surface Water - EC-WHALF"
      ConParms.ItemData(ConParms.NewIndex) = SW_HALF_LIFE_IDX
    End If
    ProgParms.Clear
    If ProgParms.ListCount = 0 Then
      ProgParms.AddItem "Physical loss half-time in Surface Soil - EC-SHALF"
      ProgParms.ItemData(ProgParms.NewIndex) = SO_HALF_LIFE_IDX
    End If
    ProgParms.AddItem "Physical loss half-time in Ground Water - EC-GHALF"
    ProgParms.ItemData(ProgParms.NewIndex) = GW_HALF_LIFE_IDX
    If EXPType(GW_TYPE) > 0 Or EXPType(AT_TYPE) > 0 Or EXPType(SW_TYPE) > 0 Then
      ProgParms.AddItem "Physical loss half-time in Air - EC-THALF"
      ProgParms.ItemData(ProgParms.NewIndex) = AT_HALF_LIFE_IDX
    End If
    If EXPType(SW_TYPE) > 0 Then
      ProgParms.AddItem "Physical loss half-time in Surface Water - EC-WHALF"
      ProgParms.ItemData(ProgParms.NewIndex) = SW_HALF_LIFE_IDX
    End If

'resolve progeny differences         if progeny no longer exists its index is 0
    For i = 1 To m_numcon
      If model(i).idx <> 0 Then
        For j = 1 To con(model(i).idx).numprog
          For k = 1 To model(i).numprog
            If model(i).progeny(k).cas = con(model(i).idx).progeny(j).cas Then
              model(i).progeny(k).idx = j
              For pcnt = 0 To MAX_CONTAM_PARAM
                uidx = pcnt
                If pcnt = 3 Then pcnt = 4
                If pcnt > 3 Then uidx = 4
                If model(i).progeny(k).param(pcnt).value = "" Or _
                   model(i).progeny(k).param(pcnt).value = Empty Then
                  model(i).progeny(k).param(pcnt).value = con(model(i).idx).progeny(j).param(pcnt)
                  model(i).progeny(k).param(pcnt).uunt = uStr(uidx)
                  model(i).progeny(k).param(pcnt).ref = 0
                End If
              Next
              Exit For
            End If
          Next
          If k > model(i).numprog Then
            model(i).numprog = k
            ReDim Preserve model(i).progeny(k) As model_progeny_param
            model(i).progeny(k).cas = con(model(i).idx).progeny(j).cas
            model(i).progeny(k).idx = j
            For pcnt = 0 To MAX_CONTAM_PARAM
              uidx = pcnt
              If pcnt = 3 Then pcnt = 4
              If pcnt > 3 Then uidx = 4
              model(i).progeny(k).param(pcnt).value = con(model(i).idx).progeny(j).param(pcnt)
              model(i).progeny(k).param(pcnt).uunt = uStr(uidx)
              model(i).progeny(k).param(pcnt).ref = 0
            Next
          End If
        Next
      End If
    Next

  Else
    put_val errfile, "Can't find or open file " & FUIName
    put_line errfile
    close_csv errfile
    End
  End If
  
ErrorHandler:
  If Err.Number <> 0 Then
    MsgBox Err.Description, vbOKOnly, "loadprm"
  End If
End Sub

Private Sub Form_load()
  Dim i As Long
  Dim j As Integer
  Dim fle As parmfile
  
  InitConstants
  StartModule Me, App.Title, 5
  SetHelpFile App.Path + "\exp.htm"
  SetRefFile ReplaceExt(FUIName, "ref")
  load_kd
  usedcustom1 = False
  For i = 1 To 7
   EXPType(i) = 0
  Next
  For i = GW_TAB_IDX To SO_TAB_IDX
    exp_t.TabVisible(i) = False
  Next
  
  'set conversion comboboxes
  For i = 0 To 15
    If i <> 2 And i <> 5 And i <> 7 And i <> 13 Then
      get_conversion_items Unit(i).Tag, Unit(i)
    End If
  Next
  For i = 23 To 38
    If i = 24 Then i = 25
    If i = 26 Then i = 31
    If i = 35 Then i = 37
    get_conversion_items Unit(i).Tag, Unit(i)
  Next
  soilmod.ListIndex = 1
  lopt = 0
  loadng = True
  Loading.Show
  With soil
    .sand = ""
    .silt = ""
    .clay = ""
    .organic = ""
    .iron = ""
    .ph = ""
  End With
  
  If open_parm(fle, FUIName, 2) Then
    If FindSection(fle, ModName) = 0 Then
      loaddef
    End If
    close_parm fle
  End If
  
  loadprm
  BackFillParent
  cb.ListIndex = lopt
  cboKdParent.ListIndex = 0
  If ConParms.ListCount > 0 Then
    ConParms.ListIndex = 0
    prevSelectedIdx = ConParms.ItemData(0)
  End If
  If ProgParms.ListCount > 0 Then
    ProgParms.ListIndex = 0
    prevSelectedProgIdx = ProgParms.ItemData(0)
  End If
  Unload Loading
  
  For j = 0 To 7
    gwpath_Click j, gwpath(j).value
  Next
  For j = 0 To 22
    If j = 14 Then j = 20
    swpath_Click j, swpath(j).value
  Next
  For j = 4 To 24
    If j = 8 Then j = 14
    If j = 16 Then j = 18
    If j = 20 Then j = 23
    airpath_Click j, airpath(j).value
  Next
  
  loadng = False
  leachtab
  frame(13).Enabled = True
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

Private Sub save_Click()
  Dim i As Long
  Dim j As Long
  Dim k As Long
  Dim s1 As Double
  Dim s2 As Double
  Dim havepath As Boolean
  Dim pname As String
  Dim fname As String
  Dim parm As parmrec
  Dim fle As parmfile
  Dim tmp As parmfile
  Dim flag(4) As Long
  Dim UserFormat As String
  UserFormat = CVTFormat
  
  cboKdParent_Click
  ConParms_Click
  ProgParms_Click
  fname = RunName & ".gid"
  If open_parm(fle, fname, 1) Then
    set_parm parm, "CVTFormat", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CVTFormat
    write_parmrec fle, parm
    CVTFormat = "General Number"
    
    For i = 1 To 5
      set_parm parm, "Media", i, 0, 0, 0, 0, 0, 0, "N/A", "N/A", media(i)
      write_parmrec fle, parm
    Next
    For i = 0 To 24
      set_parm parm, "Pathways", i, 0, 0, 0, 0, 0, 0, "N/A", "N/A", paths(i)
      write_parmrec fle, parm
    Next
    For i = 1 To 2
      set_parm parm, "fwAnimal", i, 0, 0, 0, 0, 0, 0, "N/A", "N/A", fw(i)
      write_parmrec fle, parm
    Next
    For i = 1 To 4
      set_parm parm, "TerFood", i, 0, 0, 0, 0, 0, 0, "N/A", "N/A", tf(i)
      write_parmrec fle, parm
    Next
    
    For i = 1 To 7
      set_parm parm, "KNPATH", i, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CStr(EXPType(i))
      write_parmrec fle, parm
    Next
'Ground water settings
    If EXPType(GW_TYPE) = 1 Then
      havepath = False
      For i = 0 To 25
        j = 0
        Select Case i
          Case 0 To 7
            If gwpath(i).value Then j = 1
          Case 17, 25
            If gwpath(0).value And gwkex(i).value Then j = 1
        End Select
        If j = 1 Then havepath = True
        set_parm parm, "KEXPTH", i, 1, 0, 0, 0, 0, 0, "N/A", "N/A", Str(j)
        write_parmrec fle, parm
      Next
      If havepath = False Then PutError "No exposure pathway selected for groundwater!"
      
      If (gwpath(0).value Or gwpath(2).value Or gwpath(3).value) Then
        If gwpath(1).value Then
          flag(1) = 2
        Else
          flag(1) = 1
        End If
      Else
        If gwpath(1).value Then
          flag(1) = 3
        Else
          flag(1) = 0
        End If
      End If
      set_parm parm, "KGDR", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", Str(flag(1))
      write_parmrec fle, parm
      
      If (gwpath(4).value Or gwpath(5).value Or gwpath(6).value Or gwpath(7).value) Then
        If (gwpath(6).value Or gwpath(7).value) Then
          If gwdrink.value Then
            If gwfeed.value Then
              flag(2) = 1
            Else
              flag(2) = -1
            End If
          Else
            If gwfeed.value Then
              flag(2) = 2
            Else
              flag(2) = 0    ' should not be able to happen
            End If
          End If
        Else
        ' default to having crops  need a crop only flag
          flag(2) = 2
        End If
      Else
        flag(2) = 0
      End If
      set_parm parm, "KGIR", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", Str(flag(2))
      write_parmrec fle, parm
      set_parm parm, UCase(gwfeed.Tag), 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", gwfeed.value
      write_parmrec fle, parm
      set_parm parm, UCase(gwdrink.Tag), 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", gwdrink.value
      write_parmrec fle, parm
      If er(0) Then PutError "Parameter " & txt(0).Tag & " is invalid"
      set_parm parm, UCase(txt(0).Tag), 0, 0, 0, 0, 0, 0, ref(0).Tag, Unit(0).Text, Unit(0).Tag, convert(Unit(0).Text, Unit(0).Tag, Val(txt(0).Text))
      write_parmrec fle, parm
   
      If flag(1) > 0 Or flag(2) <> 0 Then
        If er(1) Then PutError "Parameter " & txt(1).Tag & " is invalid"
        set_parm parm, UCase(txt(1).Tag), 1, 0, 0, 0, 0, 0, ref(1).Tag, Unit(1).Text, Unit(1).Tag, convert(Unit(1).Text, Unit(1).Tag, Val(txt(1).Text))
        write_parmrec fle, parm
      End If
      
      If flag(1) > 0 Then
        j = 0
        If gwtreat.value = True Then j = 1
        set_parm parm, "LTRTL", 1, 0, 0, 0, 0, 0, 0, "N/A", "N/A", Str(j)
        write_parmrec fle, parm
      End If
      
      If flag(2) <> 0 Then
        If er(2) Then PutError "Parameter " & txt(2).Tag & " is invalid"
        If er(3) Then PutError "Parameter " & txt(3).Tag & " is invalid"
        set_parm parm, UCase(txt(2).Tag), 1, 0, 0, 0, 0, 0, ref(2).Tag, "N/A", "N/A", txt(2)
        write_parmrec fle, parm
        set_parm parm, UCase(txt(3).Tag), 1, 0, 0, 0, 0, 0, ref(3).Tag, Unit(3).Text, Unit(3).Tag, convert(Unit(3).Text, Unit(3).Tag, Val(txt(3).Text))
        write_parmrec fle, parm
      End If
    End If
    
'Surface water settings
    If EXPType(SW_TYPE) = 1 Then
      havepath = False
      For i = 0 To 25
        j = 0
        Select Case i
          Case 0 To 13, 20 To 22
            If swpath(i).value Then j = 1
          Case 17, 25
            If swpath(0).value And swkex(i).value = True Then j = 1
        End Select
        If j = 1 Then havepath = True
        set_parm parm, "KEXPTH", i, 2, 0, 0, 0, 0, 0, "N/A", "N/A", Str(j)
        write_parmrec fle, parm
      Next
      If havepath = False Then PutError "No exposure pathway selected for surface water!"
      
      If (swpath(0).value Or swpath(1).value Or swpath(2).value Or swpath(3).value) Then
        flag(1) = 1
      Else
        flag(1) = 0
      End If
      set_parm parm, "KSDR", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", Str(flag(1))
      write_parmrec fle, parm
      
      If (swpath(4).value Or swpath(5).value Or swpath(6).value Or swpath(7).value) Then
        If (swpath(6).value Or swpath(7).value) Then
          If swdrink.value Then
            If swfeed.value Then
              flag(2) = 1
            Else
              flag(2) = -1
            End If
          Else
            If swfeed.value Then
              flag(2) = 2
            Else
              flag(2) = 0        ' should not be able to happen
            End If
          End If
        Else
        ' default to having crops  need a crop only flag
          flag(2) = 2
        End If
      Else
        flag(2) = 0
      End If
      set_parm parm, "KSIR", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", Str(flag(2))
      write_parmrec fle, parm
      set_parm parm, UCase(swfeed.Tag), 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", swfeed.value
      write_parmrec fle, parm
      set_parm parm, UCase(swdrink.Tag), 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", swdrink.value
      write_parmrec fle, parm
      If er(4) Then PutError "Parameter " & txt(4).Tag & " is invalid"
      set_parm parm, UCase(txt(4).Tag), 0, 0, 0, 0, 0, 0, ref(4).Tag, Unit(4).Text, Unit(4).Tag, convert(Unit(4).Text, Unit(4).Tag, Val(txt(4).Text))
      write_parmrec fle, parm
      
      If flag(1) > 0 Or flag(2) <> 0 Then
        If er(6) Then PutError "Parameter " & txt(6).Tag & " is invalid"
        set_parm parm, UCase(txt(6).Tag), 2, 0, 0, 0, 0, 0, ref(6).Tag, Unit(6).Text, Unit(6).Tag, convert(Unit(6).Text, Unit(6).Tag, Val(txt(6).Text))
        write_parmrec fle, parm
      End If
      
      If flag(1) > 0 Then
        j = 0
        If swtreat.value = True Then j = 1
        set_parm parm, "LTRTL", 2, 0, 0, 0, 0, 0, 0, "N/A", "N/A", Str(j)
        write_parmrec fle, parm
      End If
      
      If flag(2) <> 0 Then
        If er(7) Then PutError "Parameter " & txt(7).Tag & " is invalid"
        If er(8) Then PutError "Parameter " & txt(8).Tag & " is invalid"
        set_parm parm, UCase(txt(7).Tag), 2, 0, 0, 0, 0, 0, ref(7).Tag, "N/A", "N/A", txt(7)
        write_parmrec fle, parm
        set_parm parm, UCase(txt(8).Tag), 2, 0, 0, 0, 0, 0, ref(8).Tag, Unit(8).Text, Unit(8).Tag, convert(Unit(8).Text, Unit(8).Tag, Val(txt(8).Text))
        write_parmrec fle, parm
      End If
      
      If swpath(8).value Then
        If swpath(9).value Then
          flag(3) = 3
        Else
          flag(3) = 1
        End If
      Else
        If swpath(9).value Then
          flag(3) = 2
        Else
          flag(3) = 0
        End If
      End If
      set_parm parm, "KSAQ", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", Str(flag(3))
      write_parmrec fle, parm
      
      If swpath(10).value Or swpath(13).value Or _
         swpath(11).value Or swpath(12).value Or _
         swpath(20).value Or swpath(21) Or swpath(22).value Then
        flag(4) = 1
      Else
        flag(4) = 0
      End If
      
      set_parm parm, "KSR", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", Str(flag(4))
      write_parmrec fle, parm
      
      'recreation
      If swpath(9).value Then
        If er(10) Then PutError "Parameter " & txt(10).Tag & " is invalid"
        set_parm parm, UCase(txt(10).Tag), 0, 0, 0, 0, 0, 0, ref(10).Tag, Unit(10).Text, Unit(10).Tag, convert(Unit(10).Text, Unit(10).Tag, Val(txt(10).Text))
        write_parmrec fle, parm
      End If
        
      If swpath(8).value Then
        If er(9) Then PutError "Parameter " & txt(9).Tag & " is invalid"
        set_parm parm, UCase(txt(9).Tag), 0, 0, 0, 0, 0, 0, ref(9).Tag, Unit(9).Text, Unit(9).Tag, convert(Unit(9).Text, Unit(9).Tag, Val(txt(9).Text))
        write_parmrec fle, parm
        set_parm parm, UCase(txt(5).Tag), 0, 0, 0, 0, 0, 0, ref(5).Tag, "N/A", "N/A", txt(5).Text
        write_parmrec fle, parm
      End If
      
      If swpath(12).value Or swpath(13).value Or swpath(22).value Then
        If er(11) Then PutError "Parameter " & txt(11).Tag & " is invalid"
        If er(12) Then PutError "Parameter " & txt(12).Tag & " is invalid"
        set_parm parm, UCase(txt(11).Tag), 0, 0, 0, 0, 0, 0, ref(11).Tag, Unit(11).Text, Unit(11).Tag, convert(Unit(11).Text, Unit(11).Tag, Val(txt(11).Text))
        write_parmrec fle, parm
        set_parm parm, UCase(txt(12).Tag), 0, 0, 0, 0, 0, 0, ref(12).Tag, Unit(12).Text, Unit(12).Tag, convert(Unit(12).Text, Unit(12).Tag, Val(txt(12).Text))
        write_parmrec fle, parm
      End If
    End If
  
'Air settings
    If EXPType(AT_TYPE) = 1 Then
      If er(23) Then PutError "Parameter " & txt(23).Tag & " is invalid"
      set_parm parm, UCase(txt(23).Tag), 0, 0, 0, 0, 0, 0, ref(23).Tag, Unit(23).Text, Unit(23).Tag, convert(Unit(23).Text, Unit(23).Tag, Val(txt(23).Text))
      write_parmrec fle, parm
    'pathway settings for air
      flag(1) = 0
      flag(2) = 0
      havepath = False
      For i = 1 To 25
        j = 0
        Select Case i
          Case 4 To 7
            If airpath(i).value = True Then
              j = 1
              flag(1) = 1
            End If
          Case 14, 15, 19, 23
            If airpath(i).value = True Then
              j = 1
              flag(2) = 1
            End If
          Case 18, 24
            If airpath(i).value = True Then j = 1
        End Select
        If j = 1 Then havepath = True
        set_parm parm, "KEXPTH", i, 3, 0, 0, 0, 0, 0, "N/A", "N/A", Str(j)
        write_parmrec fle, parm
      Next
      If havepath = False Then PutError "No exposure pathway selected for air!"
      set_parm parm, "KAAG", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", Str(flag(1))
      write_parmrec fle, parm
      set_parm parm, "KAIN", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", Str(flag(2))
      write_parmrec fle, parm
      If flag(2) > 0 Then
        If er(14) Then PutError "Parameter " & txt(14).Tag & " is invalid"
        If er(15) Then PutError "Parameter " & txt(15).Tag & " is invalid"
        set_parm parm, UCase(txt(14).Tag), 0, 0, 0, 0, 0, 0, ref(14).Tag, Unit(14).Text, Unit(14).Tag, convert(Unit(14).Text, Unit(14).Tag, Val(txt(14).Text))
        write_parmrec fle, parm
        set_parm parm, UCase(txt(15).Tag), 0, 0, 0, 0, 0, 0, ref(15).Tag, Unit(15).Text, Unit(15).Tag, convert(Unit(15).Text, Unit(15).Tag, Val(txt(15).Text))
        write_parmrec fle, parm
      End If
    End If
    
'Measured soil
    If EXPType(SO_TYPE) = 1 Then
      If er(25) Then PutError "Parameter " & txt(25).Tag & " is invalid"
      set_parm parm, UCase(txt(25).Tag), 0, 0, 0, 0, 0, 0, ref(25).Tag, Unit(25).Text, Unit(25).Tag, convert(Unit(25).Text, Unit(25).Tag, Val(txt(25).Text))
      write_parmrec fle, parm
      set_parm parm, UCase(soilmod.Tag), 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CBool(soilmod.ListIndex)
      write_parmrec fle, parm
    'pathway settings for measured soil
      havepath = False
      For i = 1 To 25
        j = 0
        Select Case i
          Case 4 To 7, 14, 15, 19, 23
            If slpath(i).value = True Then j = 1
        End Select
        If j = 1 Then havepath = True
        set_parm parm, "KEXPTH", i, 5, 0, 0, 0, 0, 0, "N/A", "N/A", Str(j)
        write_parmrec fle, parm
      Next
      If havepath = False Then PutError "No exposure pathway selected for soil!"
    End If

'Exposure controls
    If er(37) Then PutError "Parameter " & txt(37).Tag & " is invalid"
    If er(38) Then PutError "Parameter " & txt(38).Tag & " is invalid"
    If er(39) Then PutError "Parameter " & txt(39).Tag & " is invalid"
    s1 = CDbl(convert(Unit(37).Text, Unit(37).Tag, Val(txt(37).Text)))
    s2 = CDbl(convert(Unit(38).Text, Unit(38).Tag, Val(txt(38).Text)))
    set_parm parm, UCase(txt(37).Tag), 0, 0, 0, 0, 0, 0, ref(37).Tag, Unit(37).Text, Unit(37).Tag, CStr(s1)
    write_parmrec fle, parm
    set_parm parm, UCase(txt(38).Tag), 0, 0, 0, 0, 0, 0, ref(38).Tag, Unit(38).Text, Unit(38).Tag, CStr(s2)
    write_parmrec fle, parm
    set_parm parm, UCase(txt(39).Tag), 0, 0, 0, 0, 0, 0, ref(39).Tag, "N/A", "N/A", Trim(txt(39).Text)
    write_parmrec fle, parm
    If Val(txt(39).Text) > 0 Then
      set_parm parm, "NYEARS", 0, 0, 0, 0, 0, 0, ref(37).Tag, "N/A", "N/A", CStr(CLng((s2 - s1) / Val(txt(39).Text)))
      write_parmrec fle, parm
    Else
      set_parm parm, "NYEARS", 0, 0, 0, 0, 0, 0, ref(37).Tag, "N/A", "N/A", 1
      write_parmrec fle, parm
    End If
    
'Leach rates
    If exp_t.TabVisible(LR_TAB_IDX) Then
      set_parm parm, "LEACHOPTION", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", cb.ListIndex
      write_parmrec fle, parm
      If cb.ListIndex = 1 Then
        For i = 31 To 34
          set_parm parm, UCase(txt(i).Tag), 0, 0, 0, 0, 0, 0, ref(i).Tag, Unit(i).Text, Unit(i).Tag, convert(Unit(i).Text, Unit(i).Tag, Val(txt(i).Text))
          write_parmrec fle, parm
        Next
      End If
'Soil Parameters
      set_parm parm, "ECCLASS", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CDbl(soil.Index)
      write_parmrec fle, parm
      set_parm parm, "ECSAND", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", soil.sand
      write_parmrec fle, parm
      set_parm parm, "ECSILT", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", soil.silt
      write_parmrec fle, parm
      set_parm parm, "ECCLAY", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", soil.clay
      write_parmrec fle, parm
      set_parm parm, "ECOMC", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", soil.organic
      write_parmrec fle, parm
      set_parm parm, "ECIRON", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", soil.iron
      write_parmrec fle, parm
      set_parm parm, "ECPH", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", soil.ph
      write_parmrec fle, parm
    End If
      
      
      
      set_parm parm, "numcon", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", Format(f_numcon)
      write_parmrec fle, parm
      
'Contaminant parameters
      For i = 1 To m_numcon
        If model(i).idx <> 0 Then
          set_parm parm, "CASID", model(i).idx, 0, 0, 0, 0, 0, model(i).param(0).ref, "N/A", "N/A", model(i).cas
          write_parmrec fle, parm
          
          If exp_t.TabVisible(LR_TAB_IDX) Then
            If cb.ListIndex = 0 Then
              If model(i).param(LR_IDX).value <> "" Then
                set_parm parm, "SOILLR", model(i).idx, 0, 0, 0, 0, 0, model(i).param(LR_IDX).ref, model(i).param(LR_IDX).uunt, uStr(SOIL_LR), convert(model(i).param(LR_IDX).uunt, uStr(SOIL_LR), Val(model(i).param(LR_IDX).value))
                write_parmrec fle, parm
              Else
                PutError "Parameter SOILLR for " & model(i).cas & " is invalid"
              End If
            Else
              If model(i).param(KD_IDX).value <> "" Then
                set_parm parm, "SOILKD", model(i).idx, 0, 0, 0, 0, 0, model(i).param(KD_IDX).ref, model(i).param(KD_IDX).uunt, uStr(SOIL_KD), convert(model(i).param(KD_IDX).uunt, uStr(SOIL_KD), Val(model(i).param(KD_IDX).value))
                write_parmrec fle, parm
              Else
                PutError "Parameter SOILKD for " & model(i).cas & " is invalid"
              End If
            End If
          End If
          
          With ConParms
            For j = 0 To .ListCount - 1
              pname = Replace(Right(.list(j), 8), "-", "")
              If Len(model(i).param(.ItemData(j)).value) = 0 Then
                PutError "Missing ground water half-life (" & pname & ") value for " & model(i).cas & " on Constituent Properties tab"
              Else
                If model(i).param(.ItemData(j)).value = Empty Or Val(model(i).param(.ItemData(j)).value) <= 0 Then
                  PutError "Invalid ground water half-life (" & pname & ") value for " & model(i).cas & " on Constituent Properties tab"
                  set_parm parm, pname, model(i).idx, 0, 0, 0, 0, 0, model(i).param(.ItemData(j)).ref, model(i).param(.ItemData(j)).uunt, uStr(HALF_LIFE), "EMPTY"
                Else
                  set_parm parm, pname, model(i).idx, 0, 0, 0, 0, 0, model(i).param(.ItemData(j)).ref, model(i).param(.ItemData(j)).uunt, uStr(HALF_LIFE), convert(model(i).param(.ItemData(j)).uunt, uStr(HALF_LIFE), Val(model(i).param(.ItemData(j)).value))
                End If
                write_parmrec fle, parm
              End If
            Next
          End With
          set_parm parm, "nds", model(i).idx, 0, 0, 0, 0, 0, 0, "N/A", "N/A", Format(model(i).numprog)
          write_parmrec fle, parm
          
          For j = 1 To model(i).numprog
            With model(i).progeny(j)
              If .idx <> 0 Then
                set_parm parm, "CASID", model(i).idx, .idx, 0, 0, 0, 0, .param(0).ref, "N/A", "N/A", .cas
                write_parmrec fle, parm
                
                If exp_t.TabVisible(LR_TAB_IDX) Then
                    If cb.ListIndex = 0 Then
                      If .param(LR_IDX).value <> "" Then
                        set_parm parm, "SOILLR", model(i).idx, .idx, 0, 0, 0, 0, .param(LR_IDX).ref, .param(LR_IDX).uunt, uStr(SOIL_LR), convert(.param(LR_IDX).uunt, uStr(SOIL_LR), Val(.param(LR_IDX).value))
                        write_parmrec fle, parm
                      Else
                        PutError "Parameter SOILLR for " & .cas & " daughter of " & model(i).cas & " is invalid"
                      End If
                    Else
                      If .param(KD_IDX).value <> "" Then
                        set_parm parm, "SOILKD", model(i).idx, .idx, 0, 0, 0, 0, .param(KD_IDX).ref, .param(KD_IDX).uunt, uStr(SOIL_KD), convert(.param(KD_IDX).uunt, uStr(SOIL_KD), Val(.param(KD_IDX).value))
                        write_parmrec fle, parm
                      Else
                        PutError "Parameter SOILKD for " & .cas & " daughter of " & model(i).cas & " is invalid"
                      End If
                    End If
                End If
                
                For k = 0 To ProgParms.ListCount - 1
                  pname = Replace(Right(ProgParms.list(k), 8), "-", "")
                  If Len(model(i).param(ProgParms.ItemData(k)).value) = 0 Then
                    PutError "Missing ground water half-life (" & pname & ") value for " & model(i).cas & " on Constituent Properties tab"
                  Else
                    If .param(ProgParms.ItemData(k)).value = Empty Or Val(.param(ProgParms.ItemData(k)).value) <= 0 Then
                      PutError "Invalid ground water half-life (" & pname & ") value for " & model(i).cas & " on Constituent Properties tab"
                      set_parm parm, pname, model(i).idx, .idx, 0, 0, 0, 0, .param(ProgParms.ItemData(k)).ref, .param(ProgParms.ItemData(k)).uunt, uStr(HALF_LIFE), "EMPTY"
                    Else
                      set_parm parm, pname, model(i).idx, .idx, 0, 0, 0, 0, .param(ProgParms.ItemData(k)).ref, .param(ProgParms.ItemData(k)).uunt, uStr(HALF_LIFE), convert(.param(ProgParms.ItemData(k)).uunt, uStr(HALF_LIFE), Val(.param(ProgParms.ItemData(k)).value))
                    End If
                    write_parmrec fle, parm
                  End If
                Next
              End If
            End With  ' model(i).progeny(j)
          Next
        End If
      Next


'customized
    If Not (usedcustom1) Then
      Default.Form_load
      Default.save_Click
    End If
    fname = RunName & ".~ex"
    If open_parm(tmp, fname, 2) Then
      read_parmrec tmp, parm
      Do Until EOCF(tmp.file)
        If read_parmrec(tmp, parm) Then
          write_parmrec fle, parm
        End If
      Loop
      close_parm tmp
      Kill fname
    End If
    close_parm fle
    CVTFormat = UserFormat
    SetFormat Me
  Else
    PutError "Unable to create transaction file" & RunName & ".GID"
  End If
  EndModule
End Sub

Private Sub setdef_Click()
  loaddef
End Sub

Private Sub howto_Click()
  GetHelp
End Sub

Private Sub leave_Click()
  Form_Unload 0
End Sub

Private Sub setref()
  GetRef ref(RefItem)
  Select Case RefItem
  Case 0, 4, 23, 25:
    ref(0).Caption = ref(RefItem).Caption
    ref(4).Caption = ref(RefItem).Caption
    ref(23).Caption = ref(RefItem).Caption
    ref(25).Caption = ref(RefItem).Caption
    ref(0).Tag = ref(RefItem).Tag
    ref(4).Tag = ref(RefItem).Tag
    ref(23).Tag = ref(RefItem).Tag
    ref(25).Tag = ref(RefItem).Tag
  End Select
End Sub

Private Sub selref_Click()
  RefMode = 0
  setref
End Sub

Private Sub addref_Click()
  RefMode = 1
  setref
End Sub

Private Sub custom_Click()
  Default.Show 1
End Sub

Private Sub about_Click()
  frmAbout.Show 1, Exposure
End Sub

Private Sub exp_t_Click(PreviousTab As Integer)
Dim i As Long
  For i = 0 To 15
    If i = 3 Then i = 4
    frame(i).Enabled = False
  Next
  gw_t.TabStop = False
  sw_t.TabStop = False
  air_t.TabStop = False
  Select Case exp_t.Tab
    Case GW_TAB_IDX
      frame(0).Enabled = True
      gw_t.TabStop = True
      frame(gw_t.Tab + 1).Enabled = True
    Case SW_TAB_IDX
      frame(4).Enabled = True
      sw_t.TabStop = True
      frame(sw_t.Tab + 5).Enabled = True
    Case AT_TAB_IDX
      frame(8).Enabled = True
      air_t.TabStop = True
      frame(air_t.Tab + 9).Enabled = True
    Case SO_TAB_IDX
      frame(11).Enabled = True
      frame(12).Enabled = True
    Case EXP_TAB_IDX
      frame(13).Enabled = True
    Case LR_TAB_IDX
      frame(14).Enabled = True
    Case CON_TAB_IDX
      frame(15).Enabled = True
      If cboVarParent.ListIndex = -1 Then SSCommand5_Click
  End Select
End Sub

Private Sub gw_t_Click(PreviousTab As Integer)
  frame(PreviousTab + 1).Enabled = False
  frame(gw_t.Tab + 1).Enabled = True
End Sub

Private Sub sw_t_Click(PreviousTab As Integer)
  frame(PreviousTab + 5).Enabled = False
  frame(sw_t.Tab + 5).Enabled = True
End Sub

Private Sub air_t_Click(PreviousTab As Integer)
  frame(PreviousTab + 9).Enabled = False
  frame(air_t.Tab + 9).Enabled = True
End Sub

Private Sub soilmod_Click()
  leachtab
End Sub

Private Sub SetEnabled(X As Long, v As Boolean)
On Error Resume Next
  lbl(X).Enabled = v
  txt(X).Enabled = v
  Unit(X).Enabled = v
  ref(X).Enabled = v
  er X, v
End Sub

Private Sub SetZ(obj As Variant, Optional z As Long = 0)
On Error Resume Next
  obj.Enabled = True
  obj.ZOrder z
  obj.Enabled = False
End Sub

Private Sub gwSetZ()
  If Not (gwpath(6).value Or gwpath(7).value) Then
    SetZ gwCoverBox(0)
    SetZ gwCoverBox(1)
  Else
    SetZ gwCoverBox(0), 1
    SetZ gwCoverBox(1), 1
  End If
End Sub

Private Sub swSetZ()
  If Not (swpath(6).value Or swpath(7).value) Then
    SetZ swCoverBox(0)
    SetZ swCoverBox(1)
  Else
    SetZ swCoverBox(0), 1
    SetZ swCoverBox(1), 1
  End If
End Sub

Private Sub gwdrink_Click(value As Integer)
  If Not loadng Then
    If Not (gwfeed.value Or gwdrink.value) Then gwfeed.value = True
  End If
  gwSetZ
End Sub

Private Sub swdrink_Click(value As Integer)
  If Not loadng Then
    If Not (swfeed.value Or swdrink.value) Then swfeed.value = True
  End If
  swSetZ
End Sub

Private Sub gwfeed_Click(value As Integer)
  If loadng Then Exit Sub
  If Not (gwfeed.value Or gwdrink.value) Then gwdrink.value = True
  gwSetZ
End Sub

Private Sub swfeed_Click(value As Integer)
  If loadng Then Exit Sub
  If Not (swfeed.value Or swdrink.value) Then swdrink.value = True
  swSetZ
End Sub

Private Sub gwtreat_Click(value As Integer)
  If Not (gwpath(0).value Or gwpath(1).value Or gwpath(2).value Or gwpath(3).value) Then
    SetZ gwCoverBox(2)
  Else
    SetZ gwCoverBox(2), 1
  End If
End Sub

Private Sub swtreat_Click(value As Integer)
  If Not (swpath(0).value Or swpath(1).value Or swpath(2).value Or swpath(3).value) Then
    SetZ swCoverBox(2)
  Else
    SetZ swCoverBox(2), 1
  End If
End Sub

Private Sub gwpath_Click(Index As Integer, value As Integer)
  Dim i As Integer
  
  Select Case Index
    Case 0, 1, 2, 3
      gwtreat.Enabled = False
      SetEnabled 1, False
      If gwpath(0).value Or gwpath(1).value Or _
         gwpath(2).value Or gwpath(3).value Then
        gwtreat.Enabled = True
        SetEnabled 1, True
        SetZ gwCoverBox(2), 1
      Else
        If Not (gwpath(0).value Or gwpath(1).value Or gwpath(2).value Or gwpath(3).value) Then
          SetZ gwCoverBox(2)
        End If
      End If
      If Index = 0 Then
        If value Then
          gwkex(17).Enabled = True
          gwkex(25).Enabled = True
        Else
          gwkex(17).Enabled = False
          gwkex(25).Enabled = False
        End If
      End If
    Case 4, 5
      SetEnabled 2, False
      SetEnabled 3, False
      If gwpath(4).value Or gwpath(5).value Then
        SetEnabled 2, True
        SetEnabled 3, True
      End If
      If gwpath(6).value Or gwpath(7).value Then
        SetEnabled 2, True
        SetEnabled 3, True
      End If
    Case 6, 7
      SetEnabled 2, False
      SetEnabled 3, False
      gwdrink.Enabled = False
      gwfeed.Enabled = False
      If gwpath(6).value Or gwpath(7).value Then
        gwdrink.Enabled = True
        gwfeed.Enabled = True
        SetEnabled 2, True
        SetEnabled 3, True
      End If
      gwSetZ
      If gwpath(4).value Or gwpath(5).value Then
        SetEnabled 2, True
        SetEnabled 3, True
      End If
  End Select
  leachtab
  gwpath_GotFocus Index
End Sub

Private Sub swpath_Click(Index As Integer, value As Integer)
  Dim i As Integer
  Select Case Index
    Case 0, 1, 2, 3
      swtreat.Enabled = False
      SetEnabled 6, False
      If swpath(0).value Or swpath(1).value Or _
         swpath(2).value Or swpath(3).value Then
        swtreat.Enabled = True
        SetEnabled 6, True
        SetZ swCoverBox(2), 1
      Else
        If Not (swpath(0).value Or swpath(1).value Or swpath(2).value Or swpath(3).value) Then
          SetZ swCoverBox(2)
        End If
      End If
      If Index = 0 Then
        If value Then
          swkex(17).Enabled = True
          swkex(25).Enabled = True
        Else
          swkex(17).Enabled = False
          swkex(25).Enabled = False
        End If
      End If
    Case 4, 5
      SetEnabled 7, False
      SetEnabled 8, False
      If swpath(4).value Or swpath(5).value Then
        SetEnabled 7, True
        SetEnabled 8, True
      End If
      If swpath(6).value Or swpath(7).value Then
        SetEnabled 7, True
        SetEnabled 8, True
      End If
    Case 6, 7
      SetEnabled 7, False
      SetEnabled 8, False
      swdrink.Enabled = False
      swfeed.Enabled = False
      If swpath(6).value Or swpath(7).value Then
        swdrink.Enabled = True
        swfeed.Enabled = True
        SetEnabled 7, True
        SetEnabled 8, True
      End If
      swSetZ
      If swpath(4).value Or swpath(5).value Then
        SetEnabled 7, True
        SetEnabled 8, True
      End If
    Case 8
      SetEnabled 9, swpath(8).value
      SetEnabled 5, swpath(8).value
    Case 9
      SetEnabled 10, swpath(9).value
    Case 12, 13, 22
      SetEnabled 11, False
      SetEnabled 12, False
      If swpath(13).value Or swpath(12).value Or swpath(22).value Then
        SetEnabled 11, True
        SetEnabled 12, True
      End If
  End Select
  leachtab
  swpath_GotFocus Index
End Sub

Private Sub slpath_Click(Index As Integer, value As Integer)
  leachtab
  slpath_GotFocus Index
End Sub

Private Sub airpath_Click(Index As Integer, value As Integer)
  Select Case Index
    Case 14, 15, 19, 23
      SetEnabled 14, False
      SetEnabled 15, False
      If airpath(14).value Or airpath(15).value Or _
         airpath(19).value Or airpath(23).value Then
        SetEnabled 14, True
        SetEnabled 15, True
      End If
  End Select
  leachtab
  airpath_GotFocus Index
End Sub

Private Function er(Index As Long, Optional enable As Boolean = True) As Boolean
  Dim tval As Double
  Dim t1 As String
  Dim t2 As String
  Dim t3 As String
  Dim m As String
  
  m = ""
  er = False
  tval = Val(txt(Index).Text)
  If txt(Index) = "" Then er = True
  Select Case Index
    Case EC_NTIMES
      m = "Value must be an integer greater than zero"
      If (tval <= 0) Or InStr(1, txt(Index).Text, ".") Or InStr(1, txt(Index).Text, "e") Then er = True
    Case 13, 17, 18, 19, 20, 21, 28, 29
      m = "Value must be zero or greater"
      If (tval < 0) Then er = True
    Case GW_IRR_FRACTION, SW_IRR_FRACTION
      m = "Value must be between 0 and 1"
      If (tval < 0 Or tval > 1) Then er = True
    Case KD_SOIL_MOIST
      t1 = convert(Unit(Index).Tag, Unit(Index).Text, 0)
      t2 = convert(Unit(Index).Tag, Unit(Index).Text, 1)
      m = "Value must be between " + t1 + " and " + t2 + " " + Unit(Index).Text
      If (tval < CDbl(t1) Or tval > CDbl(t2)) Then er = True
    Case SW_DEPOSITION_THICK, AT_DEPOSITION_THICK
      t1 = convert(Unit(Index).Tag, Unit(Index).Text, 0.001)
      t2 = convert(Unit(Index).Tag, Unit(Index).Text, 5)
      m = "Value must be between " + t1 + " and " + t2 + " " + Unit(Index).Text
      If (tval < CDbl(t1) Or tval > CDbl(t2)) Then er = True
    Case GW_DOMESTIC_DIST, SW_DOMESTIC_DIST
      t1 = convert(Unit(Index).Tag, Unit(Index).Text, 100)
      m = "Value must be between 0 and " + t1 + " " + Unit(Index).Text
      If (tval < 0 Or tval > CDbl(t1)) Then er = True
    Case GW_IRR_RATE, SW_IRR_RATE
      t1 = convert(Unit(Index).Tag, Unit(Index).Text, 999.9)
      m = "Value must be between 0 and " + t1 + " " + Unit(Index).Text
      If (tval < 0 Or tval > CDbl(t1)) Then er = True
    Case KD_SOIL_LEACH
      t1 = convert(Unit(Index).Tag, Unit(Index).Text, 500)
      m = "Value must be between 0 and " + t1 + " " + Unit(Index).Text
      If (tval < 0 Or tval > CDbl(t1)) Then er = True
    Case KD_CONTAM_PARAMS, KD_PROGENY_PARAMS
      If cb.ListIndex = 0 Then
        t1 = convert(Unit(Index).Tag, Unit(Index).Text, 100)
      Else
        t1 = convert(Unit(Index).Tag, Unit(Index).Text, 1000000#)
      End If
      m = "Value must be between 0 and " + t1 + " " + Unit(Index).Text
      If (tval < 0 Or tval > CDbl(t1)) Then er = True
    Case EC_START
      If txt(38).Text = "" Then
        t2 = convert(Unit(38).Text, Unit(Index).Text, 5000000#)
      Else
        t2 = convert(Unit(38).Text, Unit(Index).Text, Val(txt(38).Text))
      End If
      m = "Value must be between 0 and MAXTIM, " + t2 + " " + Unit(Index).Text
      If (tval < 0 Or tval >= CDbl(t2)) Then er = True
    Case KD_SOIL_THICK
      t1 = convert(Unit(Index).Tag, Unit(Index).Text, 1000)
      m = "Value must be between 0 and " + t1 + " " + Unit(Index).Text
      If (tval < 0 Or tval > CDbl(t1)) Then er = True
    Case SW_FIN_DELAY, SW_SHELL_DELAY
      t1 = convert(Unit(Index).Tag, Unit(Index).Text, 999.9)
      m = "Value must be between 0 and " + t1 + " " + Unit(Index).Text
      If (tval < 0 Or tval > CDbl(t1)) Then er = True
    Case EC_DURATION
      t1 = convert(Unit(37).Text, Unit(Index).Text, Val(txt(37).Text))
      t2 = convert(Unit(Index).Tag, Unit(Index).Text, 5000000#)
      m = "Value must be between TEXPOS, " + t1 + " " + Unit(37) + " and " + t2 + " " + Unit(Index).Text
      If (tval <= CDbl(t1) Or tval > CDbl(t2)) Then er = True
    Case SW_DEPOSITION_DENSITY, AT_DEPOSITION_DENSITY
      t1 = convert(Unit(Index).Tag, Unit(Index).Text, 1)
      t2 = convert(Unit(Index).Tag, Unit(Index).Text, 4)
      m = "Value must be between " + t1 + " and " + t2 + " " + Unit(Index).Text
      If (tval < CDbl(t1) Or tval > CDbl(t2)) Then er = True
    Case 22, 24
      t1 = convert(Unit(Index).Tag, Unit(Index).Text, 0.1)
      t2 = convert(Unit(Index).Tag, Unit(Index).Text, 100)
      m = "Value must be between " + t1 + " and " + t2 + " " + Unit(Index).Text
      If (tval < CDbl(t1) Or tval > CDbl(t2)) Then er = True
    Case KD_SOIL_DENSITY
      t1 = convert(Unit(Index).Tag, Unit(Index).Text, 0.5)
      t2 = convert(Unit(Index).Tag, Unit(Index).Text, 3#)
      m = "Value must be between " + t1 + " and " + t2 + " " + Unit(Index).Text
      If (tval < CDbl(t1) Or tval > CDbl(t2)) Then er = True
    Case SW_FISH_CORR
      m = "Value must be between 0.0001 to 9999.9"
      If (tval < 0.0001 Or tval > 9999.9) Then er = True
    Case GW_EXP_DURATION, SW_EXP_DURATION, AT_EXP_DURATION, SO_EXP_DURATION
      txt(GW_EXP_DURATION) = txt(Index).Text
      txt(SW_EXP_DURATION) = txt(Index).Text
      txt(AT_EXP_DURATION) = txt(Index).Text
      txt(SO_EXP_DURATION) = txt(Index).Text
      t1 = convert(Unit(Index).Tag, Unit(Index).Text, 1)
      t2 = convert(Unit(Index).Tag, Unit(Index).Text, 100)
      m = "Value must be between " + t1 + " and " + t2 + " " + Unit(Index).Text
      If (tval < CDbl(t1) Or tval > CDbl(t2)) Then er = True
    Case 16, 26, 27, 30
      t1 = convert(Unit(Index).Tag, Unit(Index).Text, 1)
      t2 = convert(Unit(Index).Tag, Unit(Index).Text, 100)
      m = "Value must be between " + t1 + " and " + t2 + " " + Unit(Index).Text
      If (tval < CDbl(t1) Or tval > CDbl(t2)) Then er = True
    Case CP_CONTAM_PARAMS
      m = "Value must be greater than 0"
      If tval <= 0 Then er = True
    Case CP_PROGENY_PARAMS
      m = "Value must be greater than 0"
      If tval <= 0 Then er = True
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
  If Index = 37 Then er 38
  If Index = 38 Then er 37
  er CLng(Index)
  chk = CDbl(txt(Index))
  Exit Sub
toolarge:
  txt(Index).BackColor = &H8080FF
End Sub

Private Sub unit_Click(Index As Integer)
  er CLng(Index)
End Sub
            
Private Sub BackFillParent()
Dim i As Long
Dim j As Long
Dim m As Long
Dim n As Long
Dim p As Long
  
  For m = 1 To UBound(model)
    For n = 1 To model(m).numprog
      For i = 1 To UBound(model)
        If model(i).cas = model(m).progeny(n).cas Then
          For p = 0 To 7
            model(i).param(p).value = model(m).progeny(n).param(p).value
            model(i).param(p).uunt = model(m).progeny(n).param(p).uunt
            model(i).param(p).ref = model(m).progeny(n).param(p).ref
          Next
        End If
        For j = 1 To model(i).numprog
          If model(i).progeny(j).cas = model(m).progeny(n).cas Then
            For p = 0 To 7
              model(i).progeny(j).param(p).value = model(m).progeny(n).param(p).value
              model(i).progeny(j).param(p).uunt = model(m).progeny(n).param(p).uunt
              model(i).progeny(j).param(p).ref = model(m).progeny(n).param(p).ref
            Next
          End If
        Next
      Next
    Next
  Next
End Sub
            
Private Sub FillParent(cas As String, pIdx As Long, value As String, uunt As String, ref As Long)
Dim i As Long
Dim j As Long

  For i = 1 To UBound(model)
    If model(i).cas = cas Then
      model(i).param(pIdx).value = value
      model(i).param(pIdx).uunt = uunt
      model(i).param(pIdx).ref = ref
    End If
    For j = 1 To model(i).numprog
      If model(i).progeny(j).cas = cas Then
        model(i).progeny(j).param(pIdx).value = value
        model(i).progeny(j).param(pIdx).uunt = uunt
        model(i).progeny(j).param(pIdx).ref = ref
      End If
    Next
  Next
End Sub

Private Sub propertyget()
  Dim i As Long
  
  i = listIndexOf(cboKdParent, oldKdParent)
  If i >= 0 Then i = cboKdParent.ItemData(i)
  If i > 0 Then
    With model(i).param(prevLeachIdx)
      If txt(KD_CONTAM_PARAMS).Text = "" Then
        .value = Empty
      Else
        .value = txt(KD_CONTAM_PARAMS).Text
      End If
      .uunt = Unit(KD_CONTAM_PARAMS).Text
      .ref = Val(ref(KD_CONTAM_PARAMS).Tag)
      FillParent model(i).cas, prevLeachIdx, .value, .uunt, .ref
    End With    ' model(i).param(oldindex)
  End If
End Sub

Private Sub d_propertyget()
  Dim i As Long
  Dim j As Long
  
  i = listIndexOf(cboKdParent, oldKdParent)
  If i >= 0 Then i = cboKdParent.ItemData(i)
  j = listIndexOf(cboKdProgeny, oldKdProgeny)
  If j >= 0 Then j = cboKdProgeny.ItemData(j)
  If i > 0 And j > 0 Then
    With model(i).progeny(j).param(prevLeachIdx)
      If txt(KD_PROGENY_PARAMS).Text = "" Then
        .value = Empty
      Else
        .value = txt(KD_PROGENY_PARAMS).Text
      End If
      .uunt = Unit(KD_PROGENY_PARAMS).Text
      .ref = Val(ref(KD_PROGENY_PARAMS).Tag)
      FillParent model(i).progeny(j).cas, prevLeachIdx, .value, .uunt, .ref
    End With    ' model(i).progeny(j).param(oldindex)
  End If
End Sub

Private Sub propertyput()
  Dim i As Long
  
  i = cboKdParent.ListIndex
  If i >= 0 Then i = cboKdParent.ItemData(i)
  If i >= 0 Then
    With model(i).param(cb.ListIndex)
      set_unit Unit(KD_CONTAM_PARAMS), .uunt
      If Len(.value) = 0 Then
        txt(KD_CONTAM_PARAMS).Text = ""
      Else
        txt(KD_CONTAM_PARAMS).Text = .value
      End If
      ref(KD_CONTAM_PARAMS).Caption = "Ref: " & .ref
      ref(KD_CONTAM_PARAMS).Tag = .ref
    End With    ' model(i).param(cb.ListIndex)
    If cb.ListIndex = 1 Then KdListUpdate
  End If
End Sub

Private Sub d_propertyput()
  Dim i As Long
  Dim j As Long
  On Error Resume Next
  i = cboKdParent.ListIndex
  If i >= 0 Then i = cboKdParent.ItemData(i)
  j = cboKdProgeny.ListIndex
  If j >= 0 Then j = cboKdProgeny.ItemData(j)
  If i >= 0 And j > 0 Then
    With model(i).progeny(j).param(cb.ListIndex)
      set_unit Unit(KD_PROGENY_PARAMS), .uunt
      If Len(.value) = 0 Then
        txt(KD_PROGENY_PARAMS).Text = ""
      Else
        txt(KD_PROGENY_PARAMS).Text = .value
      End If
      ref(KD_PROGENY_PARAMS).Caption = "Ref: " & .ref
      ref(KD_PROGENY_PARAMS).Tag = .ref
    End With    ' model(i).progeny(j).param(cb.ListIndex)
    If cb.ListIndex = 1 Then ProgenyKdListUpdate
  End If
End Sub

Function getEstKdDesc(kdEstimateType%) As String
  ' Fail safe return value
  getEstKdDesc = ""
  Select Case kdEstimateType%
    Case DATABASE_LOOKUP  ' Database value
      getEstKdDesc = "Database Value"
    Case EQUATION_LOOKUP  ' Equation Estimator value
      getEstKdDesc = "Estimated Value"
    Case TABLE_LOOKUP  ' Lookup Table value
      getEstKdDesc = "Lookup Table Value"
  End Select
End Function

Function getUserKd() As Double
  ' Fail safe return value
  getUserKd = -1
  If cboKdParent.ListIndex >= 0 Then
    Select Case ConKds.ListIndex
      Case DATABASE_LOOKUP  ' Database value
        getUserKd = Kd_DatabaseValue(cboKdParent.ItemData(cboKdParent.ListIndex))
      Case EQUATION_LOOKUP  ' Equation Estimator value
        getUserKd = Kd_EquationValue(cboKdParent.ItemData(cboKdParent.ListIndex), soil.organic, soil.clay, soil.silt, soil.sand)
      Case TABLE_LOOKUP  ' Lookup Table value
        getUserKd = Kd_LookupTableValue(cboKdParent.ItemData(cboKdParent.ListIndex))
    End Select
  End If
End Function

Function getProgKd() As Double
  ' Fail safe return value
  getProgKd = -1
  If cboKdParent.ListIndex >= 0 And cboKdProgeny.ListIndex >= 0 Then
    Select Case ProgKds.ListIndex
      Case DATABASE_LOOKUP  ' Database value
        getProgKd = Kd_DatabaseValue(cboKdParent.ItemData(cboKdParent.ListIndex), cboKdProgeny.ItemData(cboKdProgeny.ListIndex))
      Case EQUATION_LOOKUP  ' Equation Estimator value
        getProgKd = Kd_EquationValue(cboKdParent.ItemData(cboKdParent.ListIndex), soil.organic, soil.clay, soil.silt, soil.sand, cboKdProgeny.ItemData(cboKdProgeny.ListIndex))
      Case TABLE_LOOKUP  ' Lookup Table value
        getProgKd = Kd_LookupTableValue(cboKdParent.ItemData(cboKdParent.ListIndex), cboKdProgeny.ItemData(cboKdProgeny.ListIndex))
    End Select
  End If
End Function

Public Sub KdListUpdate()
  Dim i As Long
  Dim temp As Double
  
  If cb.ListIndex = 1 Then
    i = cboKdParent.ListIndex
    If i >= 0 Then i = cboKdParent.ItemData(i)
    If i > 0 Then
      ConKds.Clear
      ConKds.AddItem "== Select Adsorption Coefficient Value =="
      temp = Kd_DatabaseValue(cboKdParent.ItemData(cboKdParent.ListIndex))
      With model(i).param(KD_IDX)
        If temp < 0 Then
          ConKds.AddItem "N/A - " & getEstKdDesc(DATABASE_LOOKUP)
        Else
          ConKds.AddItem temp & " " & .uunt & " - " & getEstKdDesc(DATABASE_LOOKUP)
        End If
        temp = Kd_EquationValue(i, soil.organic, soil.clay, soil.silt, soil.sand)
        If temp < 0 Then
          ConKds.AddItem "N/A - " & getEstKdDesc(EQUATION_LOOKUP)
        Else
          ConKds.AddItem temp & " " & .uunt & " - " & getEstKdDesc(EQUATION_LOOKUP)
        End If
        temp = Kd_LookupTableValue(i)
        If temp < 0 Then
          ConKds.AddItem "N/A - " & getEstKdDesc(TABLE_LOOKUP)
        Else
          ConKds.AddItem temp & " " & .uunt & " - " & getEstKdDesc(TABLE_LOOKUP)
        End If
      End With
      ConKds.ListIndex = SELECT_LOOKUP
    End If
  End If
End Sub

Public Sub ProgenyKdListUpdate()
  Dim i As Long
  Dim j As Long
  Dim temp As Double
  
  If cb.ListIndex = 1 Then
    i = cboKdParent.ListIndex
    If i >= 0 Then i = cboKdParent.ItemData(i)
    j = cboKdProgeny.ListIndex
    If j >= 0 Then j = cboKdProgeny.ItemData(j)
    If j > 0 Then
      ProgKds.Clear
      ProgKds.AddItem "== Select Adsorption Coefficient Value =="
      temp = Kd_DatabaseValue(i, j)
      With model(i).progeny(j).param(KD_IDX)
        If temp < 0 Then
          ProgKds.AddItem "N/A - " & getEstKdDesc(DATABASE_LOOKUP)
        Else
          ProgKds.AddItem temp & " " & .uunt & " - " & getEstKdDesc(DATABASE_LOOKUP)
        End If
        temp = Kd_EquationValue(i, soil.organic, soil.clay, soil.silt, soil.sand, j)
        If temp < 0 Then
          ProgKds.AddItem "N/A - " & getEstKdDesc(EQUATION_LOOKUP)
        Else
          ProgKds.AddItem temp & " " & .uunt & " - " & getEstKdDesc(EQUATION_LOOKUP)
        End If
        temp = Kd_LookupTableValue(i, j)
        If temp < 0 Then
          ProgKds.AddItem "N/A - " & getEstKdDesc(TABLE_LOOKUP)
        Else
          ProgKds.AddItem temp & " " & .uunt & " - " & getEstKdDesc(TABLE_LOOKUP)
        End If
      End With
      ProgKds.ListIndex = SELECT_LOOKUP
    End If
  End If
End Sub

Private Sub GetModelParent()
  Dim i As Long
  Dim k As Long
  
  i = listIndexOf(cboVarParent, oldParent)
  If i >= 0 Then i = cboVarParent.ItemData(i)
  If i > 0 Then
    With model(i).param(prevSelectedIdx)
      If txt(CP_CONTAM_PARAMS).Text = "" Then
        .value = Empty
      Else
        .value = txt(CP_CONTAM_PARAMS).Text
      End If
      .uunt = Unit(CP_CONTAM_PARAMS).Text
      .ref = Val(ref(CP_CONTAM_PARAMS).Tag)
      
      ' RAD constituent half life values must be the same for all media
      If con(model(i).idx).rad Then
        Select Case prevSelectedIdx
          Case GW_HALF_LIFE_IDX, SW_HALF_LIFE_IDX, AT_HALF_LIFE_IDX, SO_HALF_LIFE_IDX
            For k = GW_HALF_LIFE_IDX To SO_HALF_LIFE_IDX
              If k <> (prevSelectedIdx) Then
                model(i).param(k).value = .value
                model(i).param(k).uunt = .uunt
                model(i).param(k).ref = .ref
              End If
            Next
        End Select
      End If
      FillParent model(i).cas, prevSelectedIdx, .value, .uunt, .ref
    End With
  End If
End Sub

Private Sub GetModelProgeny()
  Dim i As Long
  Dim j As Long
  Dim k As Long
  
  i = listIndexOf(cboVarParent, oldParent)
  If i >= 0 Then i = cboVarParent.ItemData(i)
  j = listIndexOf(cboVarProgeny, oldProgeny)
  If j >= 0 Then j = cboVarProgeny.ItemData(j)
  If i > 0 And j >= 0 Then
    With model(i).progeny(j).param(prevSelectedProgIdx)
      If txt(CP_PROGENY_PARAMS).Text = "" Then
        .value = Empty
      Else
        .value = txt(CP_PROGENY_PARAMS).Text
      End If
      .uunt = Unit(CP_PROGENY_PARAMS).Text
      .ref = Val(ref(CP_PROGENY_PARAMS).Tag)
      
      ' RAD constituent half life values must be the same for all media
      If con(model(i).idx).rad Then
        Select Case prevSelectedProgIdx
          Case GW_HALF_LIFE_IDX, SW_HALF_LIFE_IDX, AT_HALF_LIFE_IDX, SO_HALF_LIFE_IDX
            For k = GW_HALF_LIFE_IDX To SO_HALF_LIFE_IDX
              If k <> (prevSelectedProgIdx) Then
                model(i).progeny(j).param(k).value = .value
                model(i).progeny(j).param(k).uunt = .uunt
                model(i).progeny(j).param(k).ref = .ref
              End If
            Next
        End Select
      End If
      FillParent model(i).progeny(j).cas, prevSelectedProgIdx, .value, .uunt, .ref
    End With
  End If
End Sub

Private Sub PutModelParent()
  Dim i As Long
  Dim k As Long
  
  i = cboVarParent.ListIndex
  If i >= 0 Then i = cboVarParent.ItemData(i)
  If i >= 0 Then
    Unit(CP_CONTAM_PARAMS).Clear
    With model(i).param(ConParms.ItemData(ConParms.ListIndex))
      If Len(.uunt) > 0 Then
        get_conversion_items .uunt, Unit(CP_CONTAM_PARAMS)
        set_unit Unit(CP_CONTAM_PARAMS), .uunt
      Else
        get_conversion_items_by_type getUnitType(getListParmName(ConParms)), Unit(CP_CONTAM_PARAMS)
      End If
      
      If Len(.value) = 0 Then
        txt(CP_CONTAM_PARAMS).Text = ""
      Else
        txt(CP_CONTAM_PARAMS).Text = .value
      End If
      ref(CP_CONTAM_PARAMS).Caption = "Ref: " & .ref
      ref(CP_CONTAM_PARAMS).Tag = .ref
    End With
  End If
End Sub

Private Sub PutModelProgeny()
  Dim i As Long
  Dim j As Long
  Dim k As Long
    
  i = cboVarParent.ListIndex
  If i >= 0 Then i = cboVarParent.ItemData(i)
  j = cboVarProgeny.ListIndex
  If j >= 0 Then j = cboVarProgeny.ItemData(j)
  If j >= 0 Then
    Unit(CP_PROGENY_PARAMS).Clear
    With model(i).progeny(j).param(ProgParms.ItemData(ProgParms.ListIndex))
      If Len(.uunt) > 0 Then
        get_conversion_items .uunt, Unit(CP_PROGENY_PARAMS)
        set_unit Unit(CP_PROGENY_PARAMS), .uunt
      Else
        get_conversion_items_by_type getUnitType(getListParmName(ProgParms)), Unit(CP_PROGENY_PARAMS)
      End If
      
      If Len(.value) = 0 Then
        txt(CP_PROGENY_PARAMS).Text = ""
      Else
        txt(CP_PROGENY_PARAMS).Text = .value
      End If
      ref(CP_PROGENY_PARAMS).Caption = "Ref: " & .ref
      ref(CP_PROGENY_PARAMS).Tag = .ref
    End With
  End If
End Sub
'==================================================
' event calls for kd /leach rates
'==================================================
Private Sub cb_Click()
  Dim i As Long
  
  propertyget
  d_propertyget
  prevLeachIdx = cb.ListIndex
  
  i = cboKdParent.ListIndex
  If i >= 0 Then i = cboKdParent.ItemData(i)
  If i >= 0 Then
    Select Case cb.ListIndex
    Case KD_IDX
      cmdEstKd.Visible = True
      ConKds.Visible = True
      ProgKds.Visible = con(model(i).idx).numprog > 0
      For i = KD_SOIL_THICK To KD_SOIL_LEACH
        lbl(i).Visible = True
        txt(i).Visible = True
        Unit(i).Visible = True
        ref(i).Visible = True
      Next
      lbl(91).Tag = "ecsoilkd"
      lbl(91).Caption = "Soil adsorption coefficient (Kd)"
      lbl(92).Caption = "Soil adsorption coefficient (Kd)"
      Unit(KD_CONTAM_PARAMS).Clear
      Unit(KD_PROGENY_PARAMS).Clear
      Unit(KD_CONTAM_PARAMS).Tag = uStr(SOIL_KD)
      Unit(KD_PROGENY_PARAMS).Tag = uStr(SOIL_KD)
      get_conversion_items uStr(SOIL_KD), Unit(KD_CONTAM_PARAMS)
      get_conversion_items uStr(SOIL_KD), Unit(KD_PROGENY_PARAMS)
    Case Else
      cmdEstKd.Visible = False
      ConKds.Visible = False
      ProgKds.Visible = False
      For i = KD_SOIL_THICK To KD_SOIL_LEACH
        lbl(i).Visible = False
        txt(i).Visible = False
        Unit(i).Visible = False
        ref(i).Visible = False
      Next
      lbl(91).Tag = "ecsoillr"
      lbl(91).Caption = "Surface soil leach rate constant (SOILLR)"
      lbl(92).Caption = "Surface soil leach rate constant (SOILLR)"
      Unit(KD_CONTAM_PARAMS).Clear
      Unit(KD_PROGENY_PARAMS).Clear
      Unit(KD_CONTAM_PARAMS).Tag = uStr(SOIL_LR)
      Unit(KD_PROGENY_PARAMS).Tag = uStr(SOIL_LR)
      get_conversion_items uStr(SOIL_LR), Unit(KD_CONTAM_PARAMS)
      get_conversion_items uStr(SOIL_LR), Unit(KD_PROGENY_PARAMS)
    End Select
  End If
  
  propertyput
  d_propertyput
End Sub

Private Sub cboKdParent_Click()
  Dim i As Long
  Dim j As Long
  Dim temp As Double
  
  propertyget
  d_propertyget
  propertyput
  oldKdParent = cboKdParent.Text
  cboKdProgeny.Clear
  
  i = cboKdParent.ListIndex
  If i >= 0 Then i = cboKdParent.ItemData(i)
  If i > 0 Then
    ConKds.Visible = (cb.ListIndex = 1)
    If con(model(i).idx).numprog = 0 Then
      txt(KD_PROGENY_PARAMS).Visible = False
      Unit(KD_PROGENY_PARAMS).Visible = False
      ref(KD_PROGENY_PARAMS).Visible = False
      lbl(92).Visible = False
      cboKdProgeny.Visible = False
      ProgKds.Visible = False
    Else
      For j = 1 To model(i).numprog
        If model(i).progeny(j).idx > 0 Then
          cboKdProgeny.AddItem con(model(i).idx).progeny(model(i).progeny(j).idx).name
          cboKdProgeny.ItemData(cboKdProgeny.NewIndex) = j
        End If
      Next
      txt(KD_PROGENY_PARAMS).Visible = True
      Unit(KD_PROGENY_PARAMS).Visible = True
      ref(KD_PROGENY_PARAMS).Visible = True
      lbl(92).Visible = True
      cboKdProgeny.Visible = True
      oldKdProgeny = ""
      cboKdProgeny.ListIndex = 0
      ProgKds.Visible = (cb.ListIndex = 1)
    End If
  End If
End Sub

Private Sub cboKdProgeny_Click()
  d_propertyget
  d_propertyput
  oldKdProgeny = cboKdProgeny.Text
End Sub

Private Sub ConKds_Click()
  Dim i As Long
  Dim temp As Double
  
  If ConKds.ListIndex > SELECT_LOOKUP _
   And cb.ListIndex = 1 Then
    set_unit Unit(KD_CONTAM_PARAMS), Unit(KD_CONTAM_PARAMS).Tag
    temp = getUserKd()
    i = cboKdParent.ListIndex
    If i >= 0 Then i = cboKdParent.ItemData(i)
    If temp < 0 Then
      If i > 0 Then
        model(i).param(KD_IDX).value = Empty
        model(i).param(KD_IDX).uunt = ""
        model(i).param(KD_IDX).ref = Val(ref(KD_CONTAM_PARAMS).Tag)
      End If
    Else
      If i > 0 Then
        model(i).param(KD_IDX).value = temp
        model(i).param(KD_IDX).uunt = Unit(KD_CONTAM_PARAMS).Text
        model(i).param(KD_IDX).ref = Val(ref(KD_CONTAM_PARAMS).Tag)
      End If
    End If
    propertyput
  End If
End Sub

Private Sub ProgKds_Click()
  Dim i As Long
  Dim j As Long
  Dim temp As Double
  
  If ProgKds.ListIndex > SELECT_LOOKUP _
   And cb.ListIndex = 1 Then
    temp = getProgKd()
    i = cboKdParent.ListIndex
    If i >= 0 Then i = cboKdParent.ItemData(i)
    j = cboKdProgeny.ListIndex
    If j >= 0 Then j = cboKdProgeny.ItemData(j)
    If temp < 0 Then
      If j > 0 Then
        model(i).progeny(j).param(KD_IDX).value = Empty
        model(i).progeny(j).param(KD_IDX).uunt = ""
        model(i).progeny(j).param(KD_IDX).ref = Val(ref(KD_PROGENY_PARAMS).Tag)
      End If
    Else
      If j > 0 Then
        model(i).progeny(j).param(KD_IDX).value = temp
        model(i).progeny(j).param(KD_IDX).uunt = Unit(KD_PROGENY_PARAMS).Text
        model(i).progeny(j).param(KD_IDX).ref = Val(ref(KD_PROGENY_PARAMS).Tag)
      End If
    End If
    d_propertyput
  End If
End Sub

Private Sub cmdEstKd_Click()
  MousePointer = vbHourglass
  EstimateKd
  MousePointer = vbDefault
End Sub

'==================================================
' event calls for constiuent properties
'==================================================
Private Sub cboVarParent_Click()
  Dim i As Long
  Dim j As Long
  
  GetModelParent
  GetModelProgeny
  PutModelParent
  oldParent = cboVarParent.Text
  cboVarProgeny.Clear
  
  i = cboVarParent.ListIndex
  If i >= 0 Then i = cboVarParent.ItemData(i)
  If i > 0 Then
    If con(model(i).idx).numprog = 0 Then
      txt(CP_PROGENY_PARAMS).Visible = False
      Unit(CP_PROGENY_PARAMS).Visible = False
      ref(CP_PROGENY_PARAMS).Visible = False
      Command3.Visible = False
      Command4.Visible = False
      SSCommand1.Visible = False
      SSCommand2.Visible = False
      lbl(41).Visible = False
      lbl(43).Visible = False
      cboVarProgeny.Visible = False
      ProgParms.Visible = False
    Else
      For j = 1 To model(i).numprog
        cboVarProgeny.AddItem con(model(i).idx).progeny(model(i).progeny(j).idx).name
        cboVarProgeny.ItemData(cboVarProgeny.NewIndex) = j
      Next
      txt(CP_PROGENY_PARAMS).Visible = True
      Unit(CP_PROGENY_PARAMS).Visible = True
      ref(CP_PROGENY_PARAMS).Visible = True
      Command3.Visible = True
      Command4.Visible = True
      SSCommand1.Visible = True
      SSCommand2.Visible = True
      lbl(41).Visible = True
      lbl(43).Visible = True
      cboVarProgeny.Visible = True
      oldProgeny = ""
      cboVarProgeny.ListIndex = 0
      ProgParms.Visible = True
    End If
  End If
End Sub

Private Sub cboVarProgeny_Click()
  GetModelProgeny
  PutModelProgeny
  oldProgeny = cboVarProgeny.Text
End Sub

Private Sub ConParms_Click()
  cboVarParent_Click
  prevSelectedIdx = ConParms.ListIndex
  If prevSelectedIdx >= 0 Then prevSelectedIdx = ConParms.ItemData(prevSelectedIdx)
End Sub

Private Sub ProgParms_Click()
  cboVarProgeny_Click
  prevSelectedProgIdx = ProgParms.ListIndex
  If prevSelectedProgIdx >= 0 Then prevSelectedProgIdx = ProgParms.ItemData(prevSelectedProgIdx)
End Sub

Private Sub SSCommand4_Click()
  If cboVarParent.ListIndex > 0 Then cboVarParent.ListIndex = cboVarParent.ListIndex - 1
End Sub

Private Sub SSCommand5_Click()
  If cboVarParent.ListIndex < cboVarParent.ListCount - 1 Then cboVarParent.ListIndex = cboVarParent.ListIndex + 1
End Sub

Private Sub SSCommand1_Click()
  If cboVarProgeny.ListIndex > 0 Then cboVarProgeny.ListIndex = cboVarProgeny.ListIndex - 1
End Sub

Private Sub SSCommand2_Click()
  If cboVarProgeny.ListIndex < cboVarProgeny.ListCount - 1 Then cboVarProgeny.ListIndex = cboVarProgeny.ListIndex + 1
End Sub

Private Sub Command1_Click()
  If ConParms.ListIndex < ConParms.ListCount - 1 Then ConParms.ListIndex = ConParms.ListIndex + 1
End Sub

Private Sub Command2_Click()
  If ConParms.ListIndex > 0 Then ConParms.ListIndex = ConParms.ListIndex - 1
End Sub

Private Sub Command3_Click()
  If ProgParms.ListIndex < ProgParms.ListCount - 1 Then ProgParms.ListIndex = ProgParms.ListIndex + 1
End Sub

Private Sub Command4_Click()
  If ProgParms.ListIndex > 0 Then ProgParms.ListIndex = ProgParms.ListIndex - 1
End Sub
'==================================================
' gotfocus event calls
'==================================================
Private Sub exp_t_GotFocus()
  mes = ""
  noact.Enabled = False
  Select Case exp_t.Tab
  Case GW_TAB_IDX:   HelpAnchor = "GROUNDWATER"
  Case SW_TAB_IDX:   HelpAnchor = "SURFACE_WATER"
  Case AT_TAB_IDX:   HelpAnchor = "ATMOSPHERIC"
  Case SO_TAB_IDX:   HelpAnchor = "MEASURED_CONTAMINATION"
  Case EXP_TAB_IDX:  HelpAnchor = "EXPOSURE_CONTROLS"
  Case LR_TAB_IDX:   HelpAnchor = "LEACH_RATES"
  Case CON_TAB_IDX:  HelpAnchor = "CON_PARAMS"
  End Select
End Sub

Private Sub gw_t_GotFocus()
  exp_t_GotFocus
End Sub

Private Sub sw_t_GotFocus()
  exp_t_GotFocus
End Sub

Private Sub air_t_GotFocus()
  exp_t_GotFocus
End Sub

Private Sub gwpath_GotFocus(Index As Integer)
Dim m As String
  m = gwpath(Index).Caption & " pathway has " & IIf(gwpath(Index).value = False, "not ", "") & "been selected."
  mes = Space(140 - Len(m)) & m
  noact.Enabled = False
  HelpAnchor = "ECKEXPTH"
End Sub

Private Sub swpath_GotFocus(Index As Integer)
Dim m As String
  m = swpath(Index).Caption & " pathway has " & IIf(swpath(Index).value = False, "not ", "") & "been selected."
  mes = Space(140 - Len(m)) & m
  noact.Enabled = False
  HelpAnchor = "ECKEXPTH"
End Sub

Private Sub slpath_GotFocus(Index As Integer)
Dim m As String
  m = slpath(Index).Caption & " pathway has " & IIf(slpath(Index).value = False, "not ", "") & "been selected."
  mes = Space(140 - Len(m)) & m
  noact.Enabled = False
  HelpAnchor = "ECKEXPTH"
End Sub

Private Sub airpath_GotFocus(Index As Integer)
Dim m As String
  m = airpath(Index).Caption & " pathway has " & IIf(airpath(Index).value = False, "not ", "") & "been selected"
  mes = Space(140 - Len(m)) & m
  noact.Enabled = False
  HelpAnchor = "ECKEXPTH"
End Sub

Private Sub gwdrink_GotFocus()
  exp_t_GotFocus
End Sub

Private Sub gwfeed_GotFocus()
  exp_t_GotFocus
End Sub

Private Sub gwtreat_GotFocus()
  mes = ""
  noact.Enabled = False
  HelpAnchor = "EGLTRTL"
End Sub

Private Sub swdrink_GotFocus()
  exp_t_GotFocus
End Sub

Private Sub swfeed_GotFocus()
  exp_t_GotFocus
End Sub

Private Sub swtreat_GotFocus()
  mes = ""
  noact.Enabled = False
  HelpAnchor = "EWLTRTL"
End Sub

Private Sub cboVarProgeny_GotFocus()
Dim m As String
  m = "Select a progeny"
  mes = Space(140 - Len(m)) & m
  RefItem = CP_PROGENY_PARAMS
  noact.Enabled = True
  HelpAnchor = txt(CP_PROGENY_PARAMS).Tag
End Sub

Private Sub cboVarParent_GotFocus()
Dim m As String
  m = "Select a constituent"
  mes = Space(140 - Len(m)) & m
  RefItem = CP_CONTAM_PARAMS
  noact.Enabled = True
  HelpAnchor = txt(CP_CONTAM_PARAMS).Tag
End Sub

Private Sub SSCommand1_GotFocus()
  cboVarProgeny_GotFocus
End Sub

Private Sub SSCommand2_GotFocus()
  cboVarProgeny_GotFocus
End Sub

Private Sub SSCommand4_GotFocus()
  cboVarParent_GotFocus
End Sub

Private Sub SSCommand5_GotFocus()
  cboVarParent_GotFocus
End Sub

Private Sub ConParms_GotFocus()
Dim m As String
  m = "Select a property"
  mes = Space(140 - Len(m)) & m
  RefItem = -1
  noact.Enabled = True
  HelpAnchor = "PROPERTIES"
End Sub

Private Sub ProgParms_GotFocus()
Dim m As String
  m = "Select a property"
  mes = Space(140 - Len(m)) & m
  RefItem = -1
  noact.Enabled = True
  HelpAnchor = "PROPERTIES"
End Sub

Private Sub Command1_GotFocus()
  ConParms_GotFocus
End Sub

Private Sub Command2_GotFocus()
  ConParms_GotFocus
End Sub

Private Sub Command3_GotFocus()
  ProgParms_GotFocus
End Sub

Private Sub Command4_GotFocus()
  ProgParms_GotFocus
End Sub

Private Sub txt_GotFocus(Index As Integer)
  er CLng(Index)
  RefItem = Index
  noact.Enabled = True
  If Index = 35 Or Index = 36 Then
    HelpAnchor = lbl(91).Tag
  Else
    HelpAnchor = lbl(Index).Tag
  End If
End Sub

Private Sub unit_GotFocus(Index As Integer)
  RefItem = Index
  noact.Enabled = True
  If Index = KD_CONTAM_PARAMS Or Index = KD_PROGENY_PARAMS Then
    HelpAnchor = lbl(91).Tag
  Else
    HelpAnchor = lbl(Index).Tag
  End If
End Sub

Private Sub soilmod_GotFocus()
Dim m As String
  m = "Select the desired soil concetration usage"
  mes = Space(140 - Len(m)) & m
  noact.Enabled = False
  HelpAnchor = soilmod.Tag
End Sub

Private Sub cb_GotFocus()
  mes = ""
  noact.Enabled = False
  HelpAnchor = blbl(15).Tag
End Sub

Private Sub cboKdParent_GotFocus()
  er KD_CONTAM_PARAMS
  RefItem = KD_CONTAM_PARAMS
  noact.Enabled = True
  HelpAnchor = lbl(91).Tag
End Sub

Private Sub cboKdProgeny_GotFocus()
  er KD_PROGENY_PARAMS
  RefItem = KD_PROGENY_PARAMS
  noact.Enabled = True
  HelpAnchor = lbl(91).Tag
End Sub

Private Sub ConKds_GotFocus()
  cboKdParent_GotFocus
End Sub

Private Sub ProgKds_GotFocus()
  cboKdProgeny_GotFocus
End Sub

Private Sub txt_LostFocus(Index As Integer)
  Select Case Index
    Case CP_CONTAM_PARAMS:                 cboVarParent_Click
    Case CP_PROGENY_PARAMS:                cboVarProgeny_Click
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
'
'Private Sub meta_Click()
'
'  If opendes(App.Path + "\hazexp.des") Then
'
'    put_val des, "mf"
'    put_val des, "version 2.0 beta"
'    put_line des
'    put_val des, "Exposure Pathways"
'    put_val des, "Mepas Chronic Exposure Mocule"
'    put_val des, "exp4.exe"
'    put_val des, "hazexp.bat"
'    put_line des
'    des.putbuff = """MEPAS Chronic Exposure Module" + Chr(13) + Chr(10) + Chr(13) + Chr(10) + _
'         "The MEPAS chronic exposure module can be used to" + Chr(13) + Chr(10) + _
'         "calculate pollutant concentrations in exposure" + Chr(13) + Chr(10) + _
'         "media resulting from contamination of air," + Chr(13) + Chr(10) + _
'         "groundwater, surface water, and soil. The module" + Chr(13) + Chr(10) + _
'         "includes consideration of domestic water use," + Chr(13) + Chr(10) + _
'         "farm product consumption, aquatic food consumption," + Chr(13) + Chr(10) + _
'         "surface water recreational activities, soil contact" + Chr(13) + Chr(10)
'
'    des.putbuff = des.putbuff + _
'         "exposure, and air exposures.  Both chemical and" + Chr(13) + Chr(10) + _
'         "radioactive pollutants may be evaluated.  Indoor" + Chr(13) + Chr(10) + _
'         "air inhalation of volatile compounds may be evaluated" + Chr(13) + Chr(10) + _
'         "using either the MEPAS shower model or the" + Chr(13) + Chr(10) + _
'         "EPA/Andelman indoor air model.  Transfer of activity" + Chr(13) + Chr(10) + _
'         "through food chains is modeled using concentration" + Chr(13) + Chr(10) + _
'         "ratios, bioaccumulation factors and transfer factors." + Chr(13) + Chr(10) + _
'         "Radioactive chain decay with branching is included" + Chr(13) + Chr(10)
'
'    des.putbuff = des.putbuff + _
'         "in evaluation of changing media concentrations with" + Chr(13) + Chr(10) + _
'         "time.  Buildup and leaching of constituents from" + Chr(13) + Chr(10) + _
'         "surface soil is accounted for as a function of time." + Chr(13) + Chr(10) + _
'         "For waterborne contamination, exposure media" + Chr(13) + Chr(10) + _
'         "concentrations are evaluated for one location." + Chr(13) + Chr(10) + _
'         "For atmospheric contamination, media concentrations" + Chr(13) + Chr(10) + _
'         "are evaluated for each location defined in the ATO file." + Chr(13) + Chr(10) + Chr(13) + Chr(10)
'
'    des.putbuff = des.putbuff + _
'         "Limitations:" + Chr(13) + Chr(10) + _
'         "    The atmospheric transport output file (ATO) can have" + Chr(13) + Chr(10) + _
'         "    data for a maximum of 10 time periods, 20 distances," + Chr(13) + Chr(10) + _
'         "    and 36 directions.  A maximum of 100 time points can" + Chr(13) + Chr(10) + _
'         "    be defined for each data set in the water concentration" + Chr(13) + Chr(10) + _
'         "    file (WCF)." + Chr(13) + Chr(10) + Chr(13) + Chr(10)
'
'    des.putbuff = des.putbuff + _
'         "Reference:" + Chr(13) + Chr(10) + _
'         "    Strenge, D.L., and P.J. Chamberlain.  1995." + Chr(13) + Chr(10) + _
'         "    Multimedia Environmental Pollutant Assessment System (MEPAS):" + Chr(13) + Chr(10) + _
'         "    Exposure Pathway and Human Health Impact Assessment Models." + Chr(13) + Chr(10) + _
'         "    PNL-10523.  Pacific Northwest Laboratory, Richland, WA." + Chr(13) + Chr(10) + Chr(13) + Chr(10)
'
'    des.putbuff = des.putbuff + _
'         "Point of Contact:" + Chr(13) + Chr(10) + _
'         "    Pacific Northwest National Laboratory" + Chr(13) + Chr(10) + _
'         "    Mitch Pelton" + Chr(13) + Chr(10) + _
'         "    P.O. Box 999 MS K6-80" + Chr(13) + Chr(10) + _
'         "    Richland WA 99352" + Chr(13) + Chr(10) + _
'         "    Phone (509)-376-3858" + Chr(13) + Chr(10) + _
'         "    EMail mitch.pelton@pnl.gov"""
'    put_line des
'
'    put_val des, 4
'    put_line des
'
'    putfile ATO, 1
'    putfile WCF, 1
'    putfile SCF, 1
'    putfile EPF, 2
'
''count the number of entries below and in the Default module and place that number here
'    put_val des, 51
'    put_line des
'
'    putmeta "KNPATH", "NOT STOCHASTIC", "N/A", "Pathway selection flag", 0, 1, 7
'    putmeta "KEXPTH", "NOT STOCHASTIC", "N/A", "", 1, 0, 1
'    putlabel "Exposure pathway #", "Index1"
'
'
'    putmeta UCase(gwtreat.Tag), "NOT STOCHASTIC", "N/A", gwtreat.Caption, 1
'    putmeta UCase(swtreat.Tag), "NOT STOCHASTIC", "N/A", swtreat.Caption, 1
'    putlabel "KNPATH #", "Index1"
'
'    putmeta "LEACHOPTION", "NOT STOCHASTIC", "N/A", blbl(15).Caption, 0, 0, 1
'
'    putmeta UCase(txt(0).Tag), "CONTINUOUS", unit(0).Tag, lbl(0).Caption, 0, 1, 100
'    putmeta UCase(txt(4).Tag), "CONTINUOUS", unit(4).Tag, lbl(4).Caption, 0, 1, 100
'    putmeta UCase(txt(23).Tag), "CONTINUOUS", unit(23).Tag, lbl(23).Caption, 0, 1, 100
'    putmeta UCase(txt(25).Tag), "CONTINUOUS", unit(25).Tag, lbl(25).Caption, 0, 1, 100
'
'
'    putmeta UCase(txt(1).Tag), "CONTINUOUS", unit(1).Tag, lbl(1).Caption, 1, 0, 100
'    putlabel "KNPATH #", "Index1"
'
'    putmeta UCase(txt(2).Tag), "CONTINUOUS", "N/A", lbl(2).Caption, 1, 0, 1
'    putlabel "KNPATH #", "Index1"
'
'    putmeta UCase(txt(3).Tag), "CONTINUOUS", unit(3).Tag, lbl(3).Caption, 1, 0, 300
'    putlabel "KNPATH #", "Index1"
'
'    putmeta UCase(txt(9).Tag), "CONTINUOUS", unit(9).Tag, lbl(9).Caption, 0, 0, 365
'    putmeta UCase(txt(10).Tag), "CONTINUOUS", unit(10).Tag, lbl(10).Caption, 0, 0, 365
'    putmeta UCase(txt(11).Tag), "CONTINUOUS", unit(11).Tag, lbl(11).Caption, 0, 0.001, 5
'    putmeta UCase(txt(12).Tag), "CONTINUOUS", unit(12).Tag, lbl(12).Caption, 0, 1, 4
'    putmeta UCase(txt(14).Tag), "CONTINUOUS", unit(14).Tag, lbl(14).Caption, 0, 0.001, 5
'    putmeta UCase(txt(15).Tag), "CONTINUOUS", unit(15).Tag, lbl(15).Caption, 0, 1, 4
'    putmeta UCase(txt(37).Tag), "CONTINUOUS", unit(37).Tag, lbl(37).Caption, 0, 0, 1000000
'    putmeta UCase(txt(38).Tag), "NOT STOCHASTIC", unit(38).Tag, lbl(38).Caption, 0, 0
'    putmeta UCase(txt(39).Tag), "NOT STOCHASTIC", "N/A", lbl(39).Caption, 0, 0
'
'    putmeta "numchem", "NOT STOCHASTIC", "N/A", "Constiuent count", 0
'    putmeta "numprog", "NOT STOCHASTIC", "N/A", "Constiuent ID", 1
'    putvariable "FSCNAME", "Site", "Index1"
'    putmeta "CASID", "NOT STOCHASTIC", "N/A", "Constiuent ID", 1
'    putvariable "FSCNAME", "Site", "Index1", "Index2"
'    putmeta "SOILLR", "CONTINUOUS", uStr(SOIL_LR), "Leach rate constant", 1, 0, 100
'    putvariable "FSCNAME", "Site", "Index1", "Index2"
'    putmeta "SOILKD", "CONTINUOUS", uStr(SOIL_KD), "Soil adsorbtion coefficient", 1, 0, 1000000
'    putvariable "FSCNAME", "Site", "Index1", "Index2"
'
'    Default.save_metadata
'    closedes
'  Else
'    MsgBox "Unable to create description file"
'  End If
'End Sub



