VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "tabctl32.ocx"
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "threed32.ocx"
Object = "{B02F3647-766B-11CE-AF28-C3A2FBE76A13}#2.5#0"; "SS32X25.OCX"
Object = "{86CF1D34-0C5F-11D2-A9FC-0000F8754DA1}#2.0#0"; "mscomct2.ocx"
Begin VB.Form frmInpAcPuff 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Input Model Parameters"
   ClientHeight    =   5760
   ClientLeft      =   270
   ClientTop       =   1065
   ClientWidth     =   7680
   Icon            =   "AcutePuff.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   384
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   512
   StartUpPosition =   2  'CenterScreen
   Begin MSComDlg.CommonDialog dlgOpenFile 
      Left            =   96
      Top             =   5376
      _ExtentX        =   688
      _ExtentY        =   688
      _Version        =   393216
   End
   Begin VB.TextBox txtMes 
      BackColor       =   &H00C0FFFF&
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   372
      Left            =   0
      TabIndex        =   49
      TabStop         =   0   'False
      Top             =   5376
      Width           =   7692
   End
   Begin TabDlg.SSTab SSTab3 
      Height          =   5364
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   7692
      _ExtentX        =   13573
      _ExtentY        =   9472
      _Version        =   393216
      Style           =   1
      Tabs            =   4
      TabsPerRow      =   4
      TabHeight       =   423
      TabCaption(0)   =   "Grid Definition"
      TabPicture(0)   =   "AcutePuff.frx":030A
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "SSTab2"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).ControlCount=   1
      TabCaption(1)   =   "Model Parameters"
      TabPicture(1)   =   "AcutePuff.frx":0326
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "SSTab1"
      Tab(1).Control(0).Enabled=   0   'False
      Tab(1).ControlCount=   1
      TabCaption(2)   =   "File Information"
      TabPicture(2)   =   "AcutePuff.frx":0342
      Tab(2).ControlEnabled=   0   'False
      Tab(2).Control(0)=   "Frame6"
      Tab(2).Control(0).Enabled=   0   'False
      Tab(2).ControlCount=   1
      TabCaption(3)   =   "Source Information"
      TabPicture(3)   =   "AcutePuff.frx":035E
      Tab(3).ControlEnabled=   0   'False
      Tab(3).Control(0)=   "SrcTab"
      Tab(3).Control(0).Enabled=   0   'False
      Tab(3).Control(1)=   "Frame7"
      Tab(3).Control(1).Enabled=   0   'False
      Tab(3).ControlCount=   2
      Begin TabDlg.SSTab SSTab2 
         Height          =   4716
         Left            =   192
         TabIndex        =   91
         Top             =   480
         Width           =   7404
         _ExtentX        =   13070
         _ExtentY        =   8308
         _Version        =   393216
         Style           =   1
         Tabs            =   2
         TabsPerRow      =   2
         TabHeight       =   420
         TabCaption(0)   =   "Nodes"
         TabPicture(0)   =   "AcutePuff.frx":037A
         Tab(0).ControlEnabled=   -1  'True
         Tab(0).Control(0)=   "Frame1"
         Tab(0).Control(0).Enabled=   0   'False
         Tab(0).ControlCount=   1
         TabCaption(1)   =   "Surface Roughness"
         TabPicture(1)   =   "AcutePuff.frx":0396
         Tab(1).ControlEnabled=   0   'False
         Tab(1).Control(0)=   "Frame9"
         Tab(1).ControlCount=   1
         Begin VB.Frame Frame9 
            Height          =   4140
            Left            =   -74808
            TabIndex        =   92
            Top             =   384
            Width           =   7020
            Begin VB.TextBox txtAll 
               Height          =   288
               Left            =   2568
               TabIndex        =   2
               Top             =   384
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               Height          =   288
               Index           =   20
               Left            =   2592
               Style           =   2  'Dropdown List
               TabIndex        =   5
               Tag             =   "m"
               Top             =   768
               Width           =   1000
            End
            Begin VB.CommandButton cmdAll 
               Caption         =   "Set all values"
               Enabled         =   0   'False
               Height          =   300
               Left            =   3840
               TabIndex        =   3
               Top             =   384
               Width           =   1644
            End
            Begin FPSpread.vaSpread spdGrid 
               Height          =   2208
               Left            =   768
               TabIndex        =   6
               Top             =   1536
               Width           =   5508
               _Version        =   131077
               _ExtentX        =   9895
               _ExtentY        =   4233
               _StockProps     =   64
               AutoSize        =   -1  'True
               BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
                  Name            =   "MS Sans Serif"
                  Size            =   8.25
                  Charset         =   0
                  Weight          =   700
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               SpreadDesigner  =   "AcutePuff.frx":03B2
               VisibleCols     =   5
               VisibleRows     =   8
            End
            Begin VB.Label Label27 
               AutoSize        =   -1  'True
               Caption         =   "South"
               Height          =   192
               Left            =   3456
               TabIndex        =   97
               Top             =   3840
               Width           =   492
            End
            Begin VB.Label Label16 
               AutoSize        =   -1  'True
               Caption         =   "Surface roughness unit"
               Height          =   192
               Left            =   288
               TabIndex        =   4
               Top             =   768
               Width           =   1620
            End
            Begin VB.Label Label28 
               AutoSize        =   -1  'True
               Caption         =   "West"
               Height          =   192
               Left            =   192
               TabIndex        =   96
               Top             =   2592
               Width           =   372
            End
            Begin VB.Label Label26 
               AutoSize        =   -1  'True
               Caption         =   "East"
               Height          =   192
               Left            =   6432
               TabIndex        =   95
               Top             =   2592
               Width           =   324
            End
            Begin VB.Label Label25 
               AutoSize        =   -1  'True
               Caption         =   "North"
               Height          =   192
               Left            =   3456
               TabIndex        =   94
               Top             =   1248
               Width           =   384
            End
            Begin VB.Label Label3 
               Caption         =   "Set all roughness values"
               Height          =   204
               Left            =   288
               TabIndex        =   1
               Top             =   384
               Width           =   2220
            End
            Begin VB.Label ref 
               AutoSize        =   -1  'True
               Caption         =   "Ref:  0"
               Height          =   192
               Index           =   20
               Left            =   5952
               TabIndex        =   93
               Tag             =   "0"
               Top             =   0
               Width           =   444
            End
         End
         Begin VB.Frame Frame1 
            Height          =   4140
            Left            =   192
            TabIndex        =   8
            Top             =   384
            Width           =   7020
            Begin VB.CheckBox chkGrid 
               Caption         =   "Set grid of surface roughness values"
               Enabled         =   0   'False
               Height          =   252
               Left            =   288
               TabIndex        =   24
               Top             =   3168
               Width           =   4932
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H008080FF&
               Enabled         =   0   'False
               Height          =   288
               Index           =   0
               Left            =   3072
               TabIndex        =   19
               Top             =   1824
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H008080FF&
               Enabled         =   0   'False
               Height          =   288
               Index           =   1
               Left            =   3072
               TabIndex        =   22
               Top             =   2208
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               Enabled         =   0   'False
               Height          =   288
               Index           =   0
               Left            =   4032
               Style           =   2  'Dropdown List
               TabIndex        =   20
               Tag             =   "km"
               Top             =   1824
               Width           =   996
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H008080FF&
               Height          =   288
               Index           =   3
               Left            =   3072
               TabIndex        =   14
               Top             =   768
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H008080FF&
               Height          =   288
               Index           =   4
               Left            =   3072
               TabIndex        =   16
               Top             =   1152
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H008080FF&
               Height          =   288
               Index           =   2
               Left            =   3072
               TabIndex        =   12
               Top             =   384
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               Height          =   288
               Index           =   4
               Left            =   4032
               Style           =   2  'Dropdown List
               TabIndex        =   17
               Tag             =   "km"
               Top             =   1152
               Width           =   996
            End
            Begin VB.ComboBox unit 
               Enabled         =   0   'False
               Height          =   288
               Index           =   1
               Left            =   4032
               Style           =   2  'Dropdown List
               TabIndex        =   23
               Tag             =   "km"
               Top             =   2208
               Width           =   996
            End
            Begin Threed.SSOption optRecp 
               Height          =   252
               Index           =   1
               Left            =   4512
               TabIndex        =   10
               Top             =   672
               Visible         =   0   'False
               Width           =   2304
               _Version        =   65536
               _ExtentX        =   4057
               _ExtentY        =   444
               _StockProps     =   78
               Caption         =   "Enter Number of Nodes"
               BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
                  Name            =   "MS Sans Serif"
                  Size            =   8.14
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
            End
            Begin Threed.SSOption optRecp 
               Height          =   252
               Index           =   0
               Left            =   4512
               TabIndex        =   9
               Top             =   384
               Visible         =   0   'False
               Width           =   2304
               _Version        =   65536
               _ExtentX        =   4057
               _ExtentY        =   444
               _StockProps     =   78
               Caption         =   "Enter Domain Sizes"
               BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
                  Name            =   "MS Sans Serif"
                  Size            =   8.65
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
            End
            Begin VB.Label ref 
               AutoSize        =   -1  'True
               Caption         =   "Ref: 0"
               Height          =   192
               Index           =   17
               Left            =   5952
               TabIndex        =   7
               Tag             =   "0"
               Top             =   0
               Width           =   408
            End
            Begin VB.Label lbl 
               Caption         =   "Size of Domain (East-West)"
               Height          =   192
               Index           =   1
               Left            =   288
               TabIndex        =   21
               Top             =   2208
               Width           =   2496
            End
            Begin VB.Label lbl 
               Caption         =   "Distance Between Nodes"
               Height          =   192
               Index           =   4
               Left            =   288
               TabIndex        =   15
               Top             =   1152
               Width           =   2496
            End
            Begin VB.Label lbl 
               Caption         =   "Number of Nodes (North-South)"
               Height          =   192
               Index           =   2
               Left            =   288
               TabIndex        =   11
               Top             =   384
               Width           =   2496
            End
            Begin VB.Label lbl 
               Caption         =   "Size of Domain (North-South)"
               Height          =   192
               Index           =   0
               Left            =   288
               TabIndex        =   18
               Top             =   1896
               Width           =   2496
            End
            Begin VB.Label lbl 
               Caption         =   "Number of Nodes (East-West)"
               Height          =   192
               Index           =   3
               Left            =   288
               TabIndex        =   13
               Top             =   768
               Width           =   2496
            End
         End
      End
      Begin VB.Frame Frame7 
         Height          =   4092
         Left            =   -74616
         TabIndex        =   38
         Top             =   864
         Width           =   7044
         Begin VB.CheckBox chkRise 
            Caption         =   "Do Plume Rise"
            Height          =   252
            Left            =   192
            TabIndex        =   40
            Top             =   672
            Width           =   1932
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   288
            Index           =   15
            Left            =   4128
            TabIndex        =   42
            Top             =   1008
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   288
            Index           =   16
            Left            =   4128
            TabIndex        =   46
            Top             =   1440
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   15
            Left            =   5088
            Style           =   2  'Dropdown List
            TabIndex        =   43
            Tag             =   "m"
            Top             =   1008
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   16
            Left            =   5088
            Style           =   2  'Dropdown List
            TabIndex        =   47
            Tag             =   "m"
            Top             =   1440
            Width           =   1000
         End
         Begin VB.Label lblSrc 
            AutoSize        =   -1  'True
            Caption         =   "This is Source1 - Point (Area)"
            Height          =   192
            Left            =   192
            TabIndex        =   39
            Top             =   384
            Width           =   2064
         End
         Begin VB.Label lbl 
            AutoSize        =   -1  'True
            Caption         =   "Initial Horizontial Dispersion Coefficient (Sigma r) "
            Height          =   192
            Index           =   15
            Left            =   384
            TabIndex        =   41
            Top             =   1056
            Width           =   3468
         End
         Begin VB.Label lbl 
            AutoSize        =   -1  'True
            Caption         =   "Initial Vertical Dispersion Coefficient (Sigma z )"
            Height          =   192
            Index           =   16
            Left            =   384
            TabIndex        =   45
            Top             =   1440
            Width           =   3276
         End
         Begin VB.Label ref 
            AutoSize        =   -1  'True
            Caption         =   "Ref:  0"
            Height          =   192
            Index           =   15
            Left            =   6144
            TabIndex        =   44
            Tag             =   "0"
            Top             =   1008
            Width           =   800
         End
         Begin VB.Label ref 
            AutoSize        =   -1  'True
            Caption         =   "Ref:  0"
            Height          =   192
            Index           =   16
            Left            =   6144
            TabIndex        =   48
            Tag             =   "0"
            Top             =   1440
            Width           =   800
         End
      End
      Begin VB.Frame Frame6 
         Height          =   4812
         Left            =   -74808
         TabIndex        =   28
         Top             =   384
         Width           =   7308
         Begin Threed.SSCommand cmdProcess 
            Height          =   372
            Index           =   0
            Left            =   4080
            TabIndex        =   36
            Top             =   2928
            Width           =   2772
            _Version        =   65536
            _ExtentX        =   4890
            _ExtentY        =   656
            _StockProps     =   78
            Caption         =   "Use SAMSON or CD144 formatted file"
         End
         Begin Threed.SSCommand cmdChange 
            Height          =   372
            Index           =   0
            Left            =   4032
            TabIndex        =   31
            Top             =   384
            Width           =   2772
            _Version        =   65536
            _ExtentX        =   4890
            _ExtentY        =   656
            _StockProps     =   78
            Caption         =   "Browse for Meteorological File"
         End
         Begin Threed.SSCommand cmdChange 
            Height          =   372
            Index           =   1
            Left            =   4032
            TabIndex        =   34
            Top             =   1248
            Width           =   2772
            _Version        =   65536
            _ExtentX        =   4890
            _ExtentY        =   656
            _StockProps     =   78
            Caption         =   "Browse for Cloud Shine Library"
         End
         Begin VB.Label Label10 
            Caption         =   $"AcutePuff.frx":04F0
            Height          =   864
            Left            =   384
            TabIndex        =   29
            Top             =   2880
            Width           =   3396
            WordWrap        =   -1  'True
         End
         Begin VB.Label Label1 
            Caption         =   "Path and Name of Meteorological Data File"
            Height          =   192
            Left            =   288
            TabIndex        =   30
            Top             =   480
            Width           =   3504
         End
         Begin VB.Label lblFile 
            BackColor       =   &H80000005&
            BorderStyle     =   1  'Fixed Single
            Caption         =   "Label3"
            Height          =   276
            Index           =   0
            Left            =   240
            TabIndex        =   32
            Top             =   840
            Width           =   6576
         End
         Begin VB.Label Label9 
            Caption         =   "Path and Name of Cloud Shine Library"
            Height          =   192
            Left            =   192
            TabIndex        =   33
            Top             =   1344
            Width           =   3504
         End
         Begin VB.Label lblFile 
            BackColor       =   &H80000005&
            BorderStyle     =   1  'Fixed Single
            Caption         =   "Label10"
            Height          =   276
            Index           =   1
            Left            =   240
            TabIndex        =   35
            Top             =   1656
            Width           =   6576
         End
      End
      Begin TabDlg.SSTab SrcTab 
         Height          =   4716
         Left            =   -74808
         TabIndex        =   37
         Top             =   480
         Width           =   7392
         _ExtentX        =   13044
         _ExtentY        =   8308
         _Version        =   393216
         Style           =   1
         Tabs            =   5
         TabsPerRow      =   5
         TabHeight       =   423
         TabCaption(0)   =   "Source 1"
         TabPicture(0)   =   "AcutePuff.frx":059F
         Tab(0).ControlEnabled=   -1  'True
         Tab(0).ControlCount=   0
         TabCaption(1)   =   "Source 2"
         TabPicture(1)   =   "AcutePuff.frx":05BB
         Tab(1).ControlEnabled=   0   'False
         Tab(1).ControlCount=   0
         TabCaption(2)   =   "Source 3"
         TabPicture(2)   =   "AcutePuff.frx":05D7
         Tab(2).ControlEnabled=   0   'False
         Tab(2).ControlCount=   0
         TabCaption(3)   =   "Source 4"
         TabPicture(3)   =   "AcutePuff.frx":05F3
         Tab(3).ControlEnabled=   0   'False
         Tab(3).ControlCount=   0
         TabCaption(4)   =   "Source 5"
         TabPicture(4)   =   "AcutePuff.frx":060F
         Tab(4).ControlEnabled=   0   'False
         Tab(4).ControlCount=   0
      End
      Begin TabDlg.SSTab SSTab1 
         Height          =   4716
         Left            =   -74808
         TabIndex        =   50
         Top             =   480
         Width           =   7392
         _ExtentX        =   13044
         _ExtentY        =   8308
         _Version        =   393216
         Style           =   1
         TabHeight       =   423
         TabCaption(0)   =   "Time and Date of Release"
         TabPicture(0)   =   "AcutePuff.frx":062B
         Tab(0).ControlEnabled=   -1  'True
         Tab(0).Control(0)=   "Frame3"
         Tab(0).Control(0).Enabled=   0   'False
         Tab(0).ControlCount=   1
         TabCaption(1)   =   "Puff Controls"
         TabPicture(1)   =   "AcutePuff.frx":0647
         Tab(1).ControlEnabled=   0   'False
         Tab(1).Control(0)=   "Frame4"
         Tab(1).Control(0).Enabled=   0   'False
         Tab(1).ControlCount=   1
         TabCaption(2)   =   "Other Controls"
         TabPicture(2)   =   "AcutePuff.frx":0663
         Tab(2).ControlEnabled=   0   'False
         Tab(2).Control(0)=   "Frame2"
         Tab(2).Control(0).Enabled=   0   'False
         Tab(2).Control(1)=   "Frame5"
         Tab(2).Control(1).Enabled=   0   'False
         Tab(2).ControlCount=   2
         Begin VB.Frame Frame2 
            Caption         =   "Sigma Parameterization Usage"
            Height          =   1932
            Left            =   -74808
            TabIndex        =   98
            Top             =   480
            Width           =   7020
            Begin VB.OptionButton optSigParm 
               Caption         =   "Turbulence Statistics"
               Height          =   280
               Index           =   4
               Left            =   192
               TabIndex        =   103
               Top             =   1440
               Width           =   2200
            End
            Begin VB.OptionButton optSigParm 
               Caption         =   "Pasquill-Gifford (ISC3)"
               Height          =   280
               Index           =   3
               Left            =   192
               TabIndex        =   102
               Top             =   288
               Value           =   -1  'True
               Width           =   2200
            End
            Begin VB.OptionButton optSigParm 
               Caption         =   "Pasquill-Gifford (NRC)"
               Height          =   280
               Index           =   2
               Left            =   192
               TabIndex        =   101
               Top             =   576
               Width           =   2200
            End
            Begin VB.OptionButton optSigParm 
               Caption         =   "Brigg's Urban Condition"
               Height          =   280
               Index           =   1
               Left            =   192
               TabIndex        =   100
               Top             =   864
               Width           =   2200
            End
            Begin VB.OptionButton optSigParm 
               Caption         =   "Brigg's Open Country"
               Height          =   280
               Index           =   0
               Left            =   192
               TabIndex        =   99
               Top             =   1152
               Width           =   2200
            End
            Begin VB.Label ref 
               AutoSize        =   -1  'True
               Caption         =   "Ref: 0"
               Height          =   192
               Index           =   18
               Left            =   5952
               TabIndex        =   104
               Tag             =   "0"
               Top             =   0
               Width           =   588
            End
         End
         Begin VB.Frame Frame4 
            Height          =   4140
            Left            =   -74808
            TabIndex        =   68
            Top             =   384
            Width           =   7020
            Begin VB.CheckBox optMergePuff 
               Caption         =   "Consolidate Puffs"
               Height          =   192
               Left            =   288
               TabIndex        =   88
               Top             =   3360
               Value           =   1  'Checked
               Width           =   1548
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H00C0FFC0&
               Height          =   288
               Index           =   10
               Left            =   4800
               TabIndex        =   86
               Text            =   "1.5"
               Top             =   3648
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H00C0FFC0&
               Height          =   288
               Index           =   8
               Left            =   4800
               TabIndex        =   81
               Text            =   "0.4"
               Top             =   2304
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H00C0FFC0&
               Height          =   288
               Index           =   9
               Left            =   4800
               TabIndex        =   80
               Text            =   "1.0E-16"
               Top             =   2760
               Width           =   1000
            End
            Begin VB.ComboBox cboTimeStep 
               Height          =   288
               ItemData        =   "AcutePuff.frx":067F
               Left            =   4800
               List            =   "AcutePuff.frx":06A1
               Style           =   2  'Dropdown List
               TabIndex        =   78
               Top             =   384
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H00C0FFC0&
               Height          =   288
               Index           =   5
               Left            =   4800
               TabIndex        =   71
               Text            =   "5.3"
               Top             =   864
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H00C0FFC0&
               Height          =   288
               Index           =   6
               Left            =   4800
               TabIndex        =   70
               Text            =   "3"
               Top             =   1344
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H00C0FFC0&
               Height          =   288
               Index           =   7
               Left            =   4800
               TabIndex        =   69
               Text            =   "0.5"
               Top             =   1824
               Width           =   1000
            End
            Begin VB.Label ref 
               Caption         =   "Ref:  0"
               Height          =   192
               Index           =   19
               Left            =   5856
               TabIndex        =   90
               Tag             =   "0"
               Top             =   384
               Width           =   996
            End
            Begin VB.Label ref 
               Caption         =   "Ref:  0"
               Height          =   192
               Index           =   10
               Left            =   5856
               TabIndex        =   89
               Tag             =   "0"
               Top             =   3648
               Width           =   996
            End
            Begin VB.Label lbl 
               Caption         =   "Maximum Distance between merging puffs (sig r units)"
               Height          =   252
               Index           =   10
               Left            =   576
               TabIndex        =   87
               Top             =   3648
               Width           =   4092
            End
            Begin VB.Label lbl 
               Caption         =   "Tracking Region Factor"
               Height          =   192
               Index           =   8
               Left            =   288
               TabIndex        =   85
               Top             =   2304
               Width           =   3996
            End
            Begin VB.Label lbl 
               Caption         =   "Minimum X/Q at Puff Center to Turn Off Puff"
               Height          =   192
               Index           =   9
               Left            =   288
               TabIndex        =   84
               Top             =   2784
               Width           =   3996
            End
            Begin VB.Label ref 
               Caption         =   "Ref:  0"
               Height          =   192
               Index           =   8
               Left            =   5880
               TabIndex        =   83
               Tag             =   "0"
               Top             =   2304
               Width           =   996
            End
            Begin VB.Label ref 
               Caption         =   "Ref:  0"
               Height          =   192
               Index           =   9
               Left            =   5880
               TabIndex        =   82
               Tag             =   "0"
               Top             =   2760
               Width           =   996
            End
            Begin VB.Label clbl 
               Caption         =   "Maximum Number of Time Steps"
               Height          =   204
               Index           =   5
               Left            =   288
               TabIndex        =   79
               Top             =   384
               Width           =   3180
            End
            Begin VB.Label lbl 
               Caption         =   "Normalized Maximum Radius of Puff (Sigma R units)"
               Height          =   192
               Index           =   5
               Left            =   288
               TabIndex        =   77
               Top             =   864
               Width           =   3996
            End
            Begin VB.Label lbl 
               Caption         =   "Number of Puffs per Hour "
               Height          =   192
               Index           =   6
               Left            =   288
               TabIndex        =   76
               Top             =   1344
               Width           =   3996
            End
            Begin VB.Label lbl 
               Caption         =   "Coefficient on Sigma r after one hour   (Turbulent Statstics Parameterization)"
               Height          =   384
               Index           =   7
               Left            =   288
               TabIndex        =   75
               Top             =   1728
               Width           =   3324
               WordWrap        =   -1  'True
            End
            Begin VB.Label ref 
               Caption         =   "Ref:  0"
               Height          =   192
               Index           =   5
               Left            =   5856
               TabIndex        =   74
               Tag             =   "0"
               Top             =   864
               Width           =   996
            End
            Begin VB.Label ref 
               Caption         =   "Ref:  0"
               Height          =   192
               Index           =   6
               Left            =   5856
               TabIndex        =   73
               Tag             =   "0"
               Top             =   1344
               Width           =   996
            End
            Begin VB.Label ref 
               Caption         =   "Ref:  0"
               Height          =   192
               Index           =   7
               Left            =   5856
               TabIndex        =   72
               Tag             =   "0"
               Top             =   1824
               Width           =   996
            End
         End
         Begin VB.Frame Frame5 
            Caption         =   "Default Parameters"
            Height          =   2028
            Left            =   -74808
            TabIndex        =   51
            Top             =   2496
            Width           =   7020
            Begin VB.ComboBox unit 
               Height          =   288
               Index           =   14
               Left            =   4692
               Style           =   2  'Dropdown List
               TabIndex        =   59
               Tag             =   "s/m"
               Top             =   1536
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               Height          =   288
               Index           =   13
               Left            =   4704
               Style           =   2  'Dropdown List
               TabIndex        =   58
               Tag             =   "s/m"
               Top             =   1152
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               Height          =   288
               Index           =   12
               Left            =   4704
               Style           =   2  'Dropdown List
               TabIndex        =   57
               Tag             =   "m"
               Top             =   768
               Width           =   1000
            End
            Begin VB.ComboBox unit 
               Height          =   288
               Index           =   11
               Left            =   4704
               Style           =   2  'Dropdown List
               TabIndex        =   56
               Tag             =   "m/s"
               Top             =   384
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H00C0FFC0&
               Height          =   288
               Index           =   11
               Left            =   3744
               TabIndex        =   55
               Text            =   "1.5"
               Top             =   384
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H00C0FFC0&
               Height          =   288
               Index           =   12
               Left            =   3744
               TabIndex        =   54
               Text            =   "400.0"
               Top             =   768
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H00C0FFC0&
               Height          =   288
               Index           =   13
               Left            =   3744
               TabIndex        =   53
               Text            =   "10.0"
               Top             =   1152
               Width           =   1000
            End
            Begin VB.TextBox txt 
               Alignment       =   1  'Right Justify
               BackColor       =   &H00C0FFC0&
               Height          =   288
               Index           =   14
               Left            =   3744
               TabIndex        =   52
               Text            =   "100.0"
               Top             =   1536
               Width           =   1000
            End
            Begin VB.Label ref 
               AutoSize        =   -1  'True
               Caption         =   "Ref: 0"
               Height          =   192
               Index           =   14
               Left            =   5748
               TabIndex        =   67
               Tag             =   "0"
               Top             =   1536
               Width           =   804
            End
            Begin VB.Label ref 
               AutoSize        =   -1  'True
               Caption         =   "Ref: 0"
               Height          =   192
               Index           =   13
               Left            =   5760
               TabIndex        =   66
               Tag             =   "0"
               Top             =   1152
               Width           =   804
            End
            Begin VB.Label ref 
               AutoSize        =   -1  'True
               Caption         =   "Ref: 0"
               Height          =   192
               Index           =   12
               Left            =   5760
               TabIndex        =   65
               Tag             =   "0"
               Top             =   768
               Width           =   804
            End
            Begin VB.Label ref 
               AutoSize        =   -1  'True
               Caption         =   "Ref: 0 "
               Height          =   192
               Index           =   11
               Left            =   5760
               TabIndex        =   64
               Tag             =   "0"
               Top             =   384
               Width           =   804
            End
            Begin VB.Label lbl 
               Caption         =   "Minimum wind speed during plume rise"
               Height          =   192
               Index           =   11
               Left            =   288
               TabIndex        =   63
               Top             =   384
               Width           =   3504
            End
            Begin VB.Label lbl 
               Caption         =   "Sigma to shift to semi-infinite cloud shine"
               Height          =   192
               Index           =   12
               Left            =   288
               TabIndex        =   62
               Top             =   756
               Width           =   3504
            End
            Begin VB.Label lbl 
               Caption         =   "Transfer resistance for Iodine"
               Height          =   192
               Index           =   13
               Left            =   288
               TabIndex        =   61
               Top             =   1152
               Width           =   3504
            End
            Begin VB.Label lbl 
               Caption         =   "Transfer resistance for Particles"
               Height          =   192
               Index           =   14
               Left            =   288
               TabIndex        =   60
               Top             =   1536
               Width           =   3504
            End
         End
         Begin VB.Frame Frame3 
            Height          =   4140
            Left            =   192
            TabIndex        =   25
            Top             =   360
            Width           =   7020
            Begin MSComCtl2.MonthView cal 
               Height          =   2370
               Left            =   480
               TabIndex        =   105
               Top             =   1080
               Width           =   2700
               _ExtentX        =   4763
               _ExtentY        =   4180
               _Version        =   393216
               ForeColor       =   -2147483630
               BackColor       =   -2147483633
               Appearance      =   1
               StartOfWeek     =   17432577
               CurrentDate     =   38981
            End
            Begin VB.ComboBox hr 
               Height          =   288
               ItemData        =   "AcutePuff.frx":06C7
               Left            =   2496
               List            =   "AcutePuff.frx":0713
               Style           =   2  'Dropdown List
               TabIndex        =   27
               Top             =   480
               Width           =   780
            End
            Begin VB.Label Label2 
               Alignment       =   2  'Center
               Caption         =   "Starting hour of Realease"
               Height          =   255
               Left            =   390
               TabIndex        =   26
               Top             =   480
               Width           =   1890
            End
         End
      End
   End
   Begin VB.Menu mnuFile 
      Caption         =   "&File"
      Begin VB.Menu mnuFiSave 
         Caption         =   "&Save and Exit"
      End
      Begin VB.Menu mnuFiExit 
         Caption         =   "&Exit"
      End
   End
   Begin VB.Menu mnuRef 
      Caption         =   "&Reference"
      Begin VB.Menu mnuRefAdd 
         Caption         =   "&Add"
      End
      Begin VB.Menu mnuRefSel 
         Caption         =   "S&elect"
      End
   End
End
Attribute VB_Name = "frmInpAcPuff"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Dim msgtext As String
Dim temp As parmrec
Dim numsrc As Long
Dim f_srcname() As String
Dim RefItem As Long
Dim gnumx As Long, gnumy As Long
Dim numairsrc As Long
Dim airsrc() As srcvals
Dim numused As Long
Dim used(5) As Long
Dim loadng As Boolean

Sub TxtEnabled(idx As Long, valu As Boolean)
On Error Resume Next
  lbl(idx).Enabled = valu
  txt(idx).Enabled = valu
  unit(idx).Enabled = valu
  ref(idx).Enabled = valu
End Sub

Sub CheckRecp()
  Dim docalc As Boolean
  Dim i As Long, j As Long
  Dim maxval As Double, gridval As Double, maxx As Double, maxy As Double
  
  If optRecp(0).Value = True Then
    docalc = True
    For i = 0 To 1
      If Errvals(i) Then
        docalc = False
      End If
    Next i
    If docalc Then
      If Val(txt(0).Text) > Val(txt(1).Text) Then
        
        maxval = Val(txt(0).Text)
        txt(2).Text = 41
        txt(4).Text = Int(((maxval / 40) + 0.004999999) * 100) / 100
        txt(3).Text = Int(Val(txt(1).Text) / Val(txt(4).Text)) + 1
        txt(1).Text = Int(Val(txt(4).Text) * (Val(txt(3).Text) - 1) * 100) / 100
      Else
        maxval = Val(txt(1).Text)
        txt(3).Text = 41
        txt(4).Text = Int(((maxval / 40) + 0.004999999) * 100) / 100
        txt(2).Text = Int(Val(txt(1).Text) / Val(txt(4).Text)) + 1
        txt(0).Text = Int(Val(txt(4).Text) * (Val(txt(2).Text) - 1) * 100) / 100
      End If
    End If
  Else
    docalc = True
    For i = 2 To 4
      If Errvals(i) Then
        docalc = False
      End If
    Next i
    If docalc Then
      txt(0).Text = Int((Val(txt(2).Text) - 1) * Val(txt(4).Text) * 100) / 100
      txt(1).Text = Int((Val(txt(3).Text) - 1) * Val(txt(4).Text) * 100) / 100
    End If
  End If
  
  If docalc Then
    gnumx = Val(txt(2).Text)
    gnumy = Val(txt(3).Text)
    spdGrid.MaxCols = gnumx
    spdGrid.MaxRows = gnumy
    gridval = Val(txt(4).Text)
    maxx = gridval * (gnumx - 1) / 2
    maxy = gridval * (gnumy - 1) / 2
    
    spdGrid.Row = 0
    For i = 1 To gnumx
      spdGrid.col = i
      spdGrid.Text = Int((-maxx + (i - 1) * gridval + 0.0049999) * 100) / 100
      
    Next i
    
    spdGrid.col = 0
    For j = 1 To gnumy
      spdGrid.Row = j
      spdGrid.Text = Int((maxy - (j - 1) * gridval + 0.0049999) * 100) / 100
    Next j
    chkGrid.Enabled = True
  End If
End Sub

Sub FillArray()
  ref(20).Tag = temp.ref
  ref(20).Caption = "Ref: " + Str(temp.ref)
  set_unit unit(20), temp.uunit
  spdGrid.col = temp.idx3
  spdGrid.Row = temp.idx4
  spdGrid.Text = convert(temp.cunit, temp.uunit, Val(temp.pval))
  If temp.idx3 > gnumx Then
    gnumx = temp.idx3
  End If
  If temp.idx4 > gnumy Then
    gnumy = temp.idx4
  End If
  chkGrid.Enabled = True
  chkGrid.Value = 1
End Sub

Sub FillIt(idx As Long)
On Error Resume Next
  ref(idx).Tag = temp.ref
  ref(idx).Caption = "Ref: " + Str(temp.ref)
  If temp.cunit <> "N/A" Then
    set_unit unit(idx), temp.uunit
  End If
  txt(idx).Text = convert(temp.cunit, temp.uunit, Val(temp.pval))
End Sub

Sub SizeSrc()
  If temp.idx3 > numairsrc Then
    numairsrc = temp.idx3
    ReDim Preserve airsrc(numairsrc) As srcvals
  End If
End Sub

Sub Loadprm()
  Dim fle As parmfile
  Dim m As Long
  Dim i As Long
  Dim j As Long

  If open_parm(fle, FUIName, 2) Then
    Do Until EOCF(fle.file)
      If read_parmrec(fle, temp) Then
        Select Case temp.pName
          Case "fui"
            Loading.Gauge1.Max = Val(temp.idx1)
            Loading.Gauge1.Value = 0
            For m = 1 To temp.idx1
              updategauge
              If read_parmrec(fle, temp) Then
                If temp.idx1 = siteIdx Then
                  Select Case temp.pName
                    Case "airsrcnum"
                      numsrc = Val(temp.pval)
                      ReDim Preserve f_srcname(numsrc) As String
                    Case "airsrcname"
                      If temp.idx3 > numsrc Then
                        numsrc = Val(temp.idx3)
                        ReDim Preserve f_srcname(numsrc) As String
                      End If
                      f_srcname(temp.idx3) = temp.pval
                  End Select
                End If
              End If
            Next m
          Case modName
            Loading.Gauge1.Max = Val(temp.idx1)
            Loading.Gauge1.Value = 0
            For m = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                updategauge
                Select Case temp.pName
                  Case "arrecpnumx":                   FillIt 2
                  Case "arrecpnumy":                   FillIt 3
                  Case "arrecpdist":                   FillIt 4
                  Case "arpuffmaxrad":                 FillIt 5
                  Case "arpuffnumhr":                  FillIt 6
                  Case "arpuffsigrcoef":               FillIt 7
                  Case "artrackregfact":               FillIt 8
                  Case "arminconcpuff":                FillIt 9
                  Case "arpuffmergedist":              FillIt 10
                  Case "arminrisespd":                 FillIt 11
                  Case "arminsigyshift":               FillIt 12
                  Case "artransresist":                FillIt 12 + temp.idx3
                  Case "arz0gridval":                  FillArray
                  Case "armetfile":                    lblFile(0).Caption = temp.pval
                  Case "arcldshnlib":                  lblFile(1).Caption = temp.pval
                  Case "arstartmonth":                 cal.month = Val(temp.pval)
                  Case "arstartday":                   cal.Day = Val(temp.pval)
                  Case "arstartyear":                  cal.year = Val(temp.pval)
                  Case "arstarthour":                  hr.ListIndex = Val(temp.pval) - 1
                  Case "arpuffmergeflag":              optMergePuff.Value = Val(temp.pval)
                  Case "ardogridflag":                 chkGrid.Value = Val(temp.pval)
                  
                  Case "arsigparm"
                    ref(18).Tag = temp.ref
                    ref(18).Caption = "Ref: " + Str(temp.ref)
                    optSigParm(Val(temp.pval) - 1).Value = True
                  
                  Case "arpufftimestep"
                    ref(19).Tag = temp.ref
                    ref(19).Caption = "Ref: " + Str(temp.ref)
                    cboTimeStep.ListIndex = Val(temp.pval) - 1
                  
                  Case "arsrcnumb"
                    numairsrc = Val(temp.pval)
                    ReDim Preserve airsrc(numairsrc) As srcvals
                  
                  Case "arsrcname"
                    SizeSrc
                    airsrc(temp.idx3).Name = temp.pval
                  
                  Case "arsrcdoriseflag"
                    SizeSrc
                    airsrc(temp.idx3).riseflg = Val(temp.pval)
                  
                  Case "arsrcsigr"
                    SizeSrc
                    airsrc(temp.idx3).sigr.Value = CDbl(convert(temp.cunit, temp.uunit, Val(temp.pval)))
                    airsrc(temp.idx3).sigr.cunit = temp.cunit
                    airsrc(temp.idx3).sigr.uunit = temp.uunit
                    airsrc(temp.idx3).sigr.ref = temp.ref
                  
                  Case "arsrcsigz"
                    SizeSrc
                    airsrc(temp.idx3).sigz.Value = CDbl(convert(temp.cunit, temp.uunit, Val(temp.pval)))
                    airsrc(temp.idx3).sigz.cunit = temp.cunit
                    airsrc(temp.idx3).sigz.uunit = temp.uunit
                    airsrc(temp.idx3).sigz.ref = temp.ref
                End Select
              End If
            Next m
          Case Else
            For m = 1 To temp.idx1
              get_line fle.file
            Next m
        End Select
      End If
    Loop
    close_parm fle
  
    'Resolve difference between FUI section and AirUI section
    numused = 0
    For i = 1 To numsrc
      For j = 1 To numairsrc
        If f_srcname(i) = airsrc(j).Name Then
          used(numused) = j
          numused = numused + 1
          Exit For
        End If
      Next j
      If j > numairsrc Then
        numairsrc = j
        ReDim Preserve airsrc(numairsrc) As srcvals
        airsrc(j).Name = f_srcname(i)
        airsrc(j).sigr.uunit = "m"
        airsrc(j).sigr.cunit = "m"
        airsrc(j).sigz.uunit = "m"
        airsrc(j).sigz.cunit = "m"
        used(numused) = j
        numused = numused + 1
      End If
    Next i

    For i = 1 To 5
      If i <= numused Then
        SrcTab.TabVisible(i - 1) = True
      Else
        SrcTab.TabVisible(i - 1) = False
      End If
    Next i
  
   Else
    msgtext = "Can't find or open file " & FUIName
    put_val errfile, msgtext
    put_line errfile
    close_csv errfile
    MsgBox msgtext
    End
  End If
    
End Sub

Private Sub Form_load()
  Dim err As Boolean
  Dim unitcount As Long
  Dim i As Long
  Dim j As Long
  
  GetArguments
  
  If argc >= 5 Then
    Loading.Gauge1.Max = 1500
    Loading.Show
    FUIName = LTrim(RTrim(argv(0))) & ".GID"
    RunName = LTrim(RTrim(argv(1)))
    siteIdx = Val(argv(2))
    modIdx = Val(argv(3))
    modName = argv(4)
    SetRefFile ReplaceExt(FUIName, "ref")
    frmInpAcPuff.Caption = "GENII Acute Puff Model - " + modName
    
    If open_csv(errfile, RunName & ".ERR", 1) Then
      put_val errfile, "Error report for Atmospheric Model"
      put_line errfile
    Else
      MsgBox "Unable to create file " & RunName & ".ERR" & Chr(10) & "Check directory permission"
      End
    End If
    
    loadng = True
    err = False
    RefItem = -1
    
    'Setup the Units
    load_convert
    For i = 0 To 20
      Select Case i
        Case 0, 1, 4, 11 To 16, 20:
          get_conversion_items unit(i).Tag, unit(i)
      End Select
    Next i
    
    'Set defaults
    lblFile(0).Caption = ""
    If Dir(App.Path + "\CSHNLIB.DAT") <> "" Then
      lblFile(1).Caption = App.Path + "\CSHNLIB.DAT"
    Else
      lblFile(1).Caption = ""
    End If
    If Dir(App.Path + "\inphrlyp.exe") <> "" And Dir(App.Path + "\hrlyproc.exe ") <> "" Then
      cmdProcess(0).Enabled = True
    Else
      cmdProcess(0).Enabled = False
    End If
    
    SSTab2.TabVisible(1) = False
    optRecp(1).Value = True
    cboTimeStep.ListIndex = 2
    gnumx = 0
    gnumy = 0
    
    Loadprm
    Unload Loading
    loadng = False
  Else
    MsgBox "Not enough arguements passed" & Chr(10) & "Contact PNNL"
    close_csv errfile
    mnuFiExit_Click
  End If
  
  'Set Tab and screen
  SSTab3.Tab = 0
  SrcTab_Click 0
End Sub

Private Sub mnuFiSave_Click()
  Dim fName As String
  Dim fle As parmfile
  Dim i As Long, j As Long
  Dim keyword(8) As String
  Dim fileword(2) As String
  Dim pufftype(4) As String
  Dim puffword(4) As String, recpword(2) As String
  Dim parm As parmrec
  Dim isig As Long
  Dim isrc As Long
  Dim chkval As String
  Dim anerr As Boolean
  Dim errflag As Boolean
  
  If cal.Day = 0 Then
    SSTab3.Tab = 1
    SSTab1.Tab = 0
    MsgBox "Please select a day of the month", vbExclamation, "Date Input Error"
    Exit Sub
  End If
  
  keyword(0) = "ARMINRISESPD"
  keyword(1) = "ARMINSIGYSHIFT"
  keyword(2) = "ARTRANSRESIST"
  keyword(3) = "ARTRANSRESIST"
  keyword(4) = "ARSTARTMONTH"
  keyword(5) = "ARSTARTDAY"
  keyword(6) = "ARSTARTYEAR"
  keyword(7) = "ARSTARTHOUR"
  keyword(8) = "JHOUR"
  
  fileword(0) = "ARMETFILE"
  fileword(1) = "ARCLDSHNLIB"
  
  pufftype(0) = "Maximum Radius of Puff"
  pufftype(1) = "Number of Puffs per hour"
  pufftype(2) = "Coefficient for Sigma r"
  pufftype(3) = "Tracking Region Factor"
  pufftype(4) = "Minimum Concentration for puff"
  
  puffword(0) = "ARPUFFMAXRAD"
  puffword(1) = "ARPUFFNUMHR"
  puffword(2) = "ARPUFFSIGRCOEF"
  puffword(3) = "ARTRACKREGFACT"
  puffword(4) = "ARMINCONCPUFF"
  
  recpword(1) = "ARRECPNUMX"
  recpword(2) = "ARRECPNUMY"
  
  errflag = False
  fName = RunName & ".GID"
  If open_parm(fle, fName, 1) Then
    put_val errfile, "Incomplete data entry!"
    put_line errfile
    
    For i = 0 To 8
      anerr = False
      Select Case i
        Case 0, 1:
          anerr = Errvals(i + 11)
          set_parm parm, keyword(i), siteIdx, modIdx, 0, 0, 0, 0, ref(i + 11).Tag, unit(i + 11).Text, unit(i + 11).Tag, convert(unit(i + 11).Text, unit(i + 11).Tag, Val(txt(i + 11).Text))
        Case 2, 3:
          anerr = Errvals(i)
          set_parm parm, keyword(i), siteIdx, modIdx, i - 1, 0, 0, 0, ref(i + 11).Tag, unit(i + 11).Text, unit(i + 11).Tag, convert(unit(i + 11).Text, unit(i + 11).Tag, Val(txt(i + 11).Text))
        Case 4:
          set_parm parm, keyword(i), siteIdx, modIdx, 0, 0, 0, 0, 0, "mn", "mn", Str(cal.month)
        Case 5:
          set_parm parm, keyword(i), siteIdx, modIdx, 0, 0, 0, 0, 0, "day", "day", Str(cal.Day)
        Case 6:
          set_parm parm, keyword(i), siteIdx, modIdx, 0, 0, 0, 0, 0, "yr", "yr", Str(cal.year)
        Case 7:
          set_parm parm, keyword(i), siteIdx, modIdx, 0, 0, 0, 0, 0, "hr", "hr", Str(hr.ListIndex + 1)
        Case 8:
          set_parm parm, keyword(i), siteIdx, modIdx, 0, 0, 0, 0, 0, "Jhr", "Jhr", Str(Date2JulHours(cal.month, cal.Day, cal.year, hr.ListIndex + 1))
      End Select
      If anerr Then
        errflag = True
        put_val errfile, "Error in variable " + keyword(i)
        put_line errfile
      End If
      write_parmrec fle, parm
    Next i
    
    isig = 1
    For i = 1 To 5
      If optSigParm(i - 1).Value = True Then
        isig = i
        Exit For
      End If
    Next i
    set_parm parm, "ARSIGPARM", siteIdx, modIdx, 0, 0, 0, 0, ref(18).Tag, "N/A", "N/A", Str(isig)
    write_parmrec fle, parm
    
    For i = 0 To 4
      Select Case i
      Case 0, 1:
        If Errvals(i) Then
          put_val errfile, "Invalid Domain Distance for Receptors."
          put_line errfile
          errflag = True
        End If
      Case 2, 3:
        If Errvals(i) Then
          put_val errfile, "Invalid Number of Nodes."
          put_line errfile
          errflag = True
        End If
        set_parm parm, recpword(i - 1), siteIdx, modIdx, 0, 0, 0, 0, ref(17).Tag, "N/A", "N/A", Val(txt(i).Text)
        write_parmrec fle, parm
      Case 4:
        If Errvals(i) Then
          put_val errfile, "Invalid distance between Nodes."
          put_line errfile
          errflag = True
        End If
        set_parm parm, "ARRECPDIST", siteIdx, modIdx, 0, 0, 0, 0, ref(17).Tag, unit(i).Text, unit(i).Tag, convert(unit(i).Text, unit(i).Tag, Val(txt(i).Text))
        write_parmrec fle, parm
      End Select
    Next i
    
    For i = 0 To 4
      If Errvals(i + 5) Then
        put_val errfile, "Invalid value for " + pufftype(i)
        put_line errfile
        errflag = True
      End If
      set_parm parm, puffword(i), siteIdx, modIdx, 0, 0, 0, 0, ref(i + 5).Tag, "N/A", "N/A", Val(txt(i + 5).Text)
      write_parmrec fle, parm
    Next i
    
    set_parm parm, "ARPUFFTIMESTEP", siteIdx, modIdx, 0, 0, 0, 0, ref(19).Tag, "N/A", "N/A", Str(cboTimeStep.ListIndex + 1)
    write_parmrec fle, parm
    
    set_parm parm, "ARPUFFMERGEFLAG", siteIdx, modIdx, 0, 0, 0, 0, 0, "N/A", "N/A", Str(optMergePuff.Value)
    write_parmrec fle, parm
    If optMergePuff.Value = 1 Then
      If Errvals(10) Then
        put_val errfile, "Invalid Distance for Max Distance Between Merging Puffs."
        put_line errfile
        errflag = True
      End If
      set_parm parm, "ARPUFFMERGEDIST", siteIdx, modIdx, 0, 0, 0, 0, ref(10).Tag, "N/A", "N/A", Val(txt(10).Text)
      write_parmrec fle, parm
    End If
    
    set_parm parm, "ARSRCNUMB", siteIdx, modIdx, 0, 0, 0, 0, 0, "N/A", "N/A", Str(numused)
    write_parmrec fle, parm
    For isrc = 1 To numused
      With airsrc(used(isrc - 1))
        set_parm parm, "ARSRCNAME", siteIdx, modIdx, isrc, 0, 0, 0, 0, "N/A", "N/A", .Name
        write_parmrec fle, parm
        set_parm parm, "ARSRCDORISEFLAG", siteIdx, modIdx, isrc, 0, 0, 0, 0, "N/A", "N/A", Str(.riseflg)
        write_parmrec fle, parm
        
        chkval = convert(.sigr.uunit, .sigr.cunit, .sigr.Value)
        If Val(chkval) < 0 Or Val(chkval) > 1000000 Then
          put_val errfile, "Invalid initial sigma r value for source " + Str(isrc)
          put_line errfile
          errflag = True
        End If
        set_parm parm, "ARSRCSIGR", siteIdx, modIdx, isrc, 0, 0, 0, .sigr.ref, .sigr.uunit, .sigr.cunit, chkval
        write_parmrec fle, parm
        
        chkval = convert(.sigz.uunit, .sigz.cunit, .sigz.Value)
        If Val(chkval) < 0 Or Val(chkval) > 1000000 Then
          put_val errfile, "Invalid initial sigma z value for source " + Str(isrc)
          put_line errfile
          errflag = True
        End If
        set_parm parm, "ARSRCSIGZ", siteIdx, modIdx, isrc, 0, 0, 0, .sigz.ref, .sigz.uunit, .sigz.cunit, chkval
        write_parmrec fle, parm
      End With
    Next isrc
    
    For i = 0 To 1
      If lblFile(i).Caption = "" Then
        put_val errfile, "No name given for " + fileword(i)
        put_line errfile
        errflag = True
      ElseIf Dir(lblFile(i).Caption) = "" Then
        put_val errfile, "Invalid name given for " + fileword(i)
        put_line errfile
        errflag = True
      Else
        set_parm parm, fileword(i), siteIdx, modIdx, 0, 0, 0, 0, 0, "N/A", "N/A", lblFile(i).Caption
        write_parmrec fle, parm
      End If
    Next i
      
    set_parm parm, "ARDOGRIDFLAG", siteIdx, modIdx, 0, 0, 0, 0, 0, "N/A", "N/A", Str(chkGrid.Value)
    write_parmrec fle, parm
    If chkGrid.Value = 1 And gnumx > 1 And gnumy > 1 Then
      anerr = False
      For i = 1 To gnumx
        spdGrid.col = i
        For j = 1 To gnumy
          spdGrid.Row = j
          chkval = convert(unit(20).Text, unit(20).Tag, Val(spdGrid.Text))
          If Val(chkval) <= 0 Or Val(chkval) > 100 Then anerr = True
          set_parm parm, "ARZ0GRIDVAL", siteIdx, modIdx, i, j, 0, 0, ref(20).Tag, unit(20).Text, unit(20).Tag, chkval
          write_parmrec fle, parm
        Next j
      Next i
      If anerr Then
        put_val errfile, "Invalid surface roughness in grid"
        put_line errfile
        errflag = True
      End If
    End If
    close_parm fle
    
    If errflag Then
      close_csv errfile
    Else
      close_csv errfile
      Kill RunName + ".ERR"
    End If
  Else
    put_val errfile, "Unable to create transaction file " + RunName + ".GID"
    put_line errfile
    close_csv errfile
  End If
  End
End Sub

Private Sub mnuFiExit_Click()
  close_csv errfile
  If Dir(RunName & ".ERR") <> "" Then
    Kill RunName & ".ERR"
  End If
  End
End Sub

Private Sub mnuRefAdd_Click()
  RefMode = 1
  GetRef ref(RefItem)
End Sub

Private Sub mnuRefSel_Click()
  RefMode = 0
  GetRef ref(RefItem)
End Sub

Private Sub chkGrid_Click()
  If chkGrid.Value = 1 Then
    SSTab2.TabVisible(1) = True
  Else
    SSTab2.TabVisible(1) = False
  End If
End Sub

Private Sub chkRise_Click()
  GetSrc
End Sub

Private Sub cmdAll_Click()
  Dim i As Long, j As Long
  
  For i = 1 To gnumx
    spdGrid.col = i
    For j = 1 To gnumy
      spdGrid.Row = j
      spdGrid.Text = Val(txtAll.Text)
    Next j
  Next i
  
End Sub

Private Sub cmdChange_Click(Index As Integer)
  Dim oldname As String
  Dim Name As String
  Dim pathway As String
  Dim ext As String
  Dim Infilter As String
  Dim newname As String
  Dim fileexist As Boolean
  Dim errtext As String
  Dim response As Long
  
  oldname = lblFile(Index).Caption
  If ChkFile(oldname) Then
    GetNamePathExt oldname, Name, pathway, ext
  Else
    pathway = App.Path
  End If
  
  Select Case (Index)
    Case (0)
      Infilter = "All Files (*.*)|*.*|Meteorological Files (*.MET)|*.MET"
    Case (1)
      Infilter = "All Files (*.*)|*.*|Data File (*.DAT)|*.DAT"
  End Select
  
  ChangeFilewithDialog dlgOpenFile, oldname, pathway, Infilter, newname, fileexist, errtext
      
  If errtext <> "" Then
    response = MsgBox(errtext, vbExclamation, "ERROR!!")
    Exit Sub
  End If
  
  If fileexist Then
    lblFile(Index).Caption = newname
  Else
    response = MsgBox("Invalid File Name", vbExclamation, "ERROR!!")
    Exit Sub
  End If
      
End Sub

Private Sub optMergePuff_Click()
  If optMergePuff.Value = 1 Then
    TxtEnabled 10, True
  Else
    TxtEnabled 10, False
  End If
End Sub

Private Sub optRecp_Click(Index As Integer, Value As Integer)
  If optRecp(0).Value = True Then
    TxtEnabled 0, True
    TxtEnabled 1, True
    TxtEnabled 2, False
    TxtEnabled 3, False
    TxtEnabled 4, False
  Else
    TxtEnabled 0, False
    TxtEnabled 1, False
    TxtEnabled 2, True
    TxtEnabled 3, True
    TxtEnabled 4, True
  End If
End Sub

Private Sub SrcTab_Click(PreviousTab As Integer)
  SetSrc
End Sub

Private Sub unit_Click(Index As Integer)
  Errvals CLng(Index)
  Select Case Index
    Case 15, 16: GetSrc
    Case 0 To 4: CheckRecp
  End Select
End Sub

Private Sub cmdProcess_Click(Index As Integer)
  Dim runcode As String
  
  If Index = 0 Then
    runcode = App.Path + "\inphrlyp.exe"
  Else
    runcode = App.Path + "\inpjfdp.exe"
  End If
  ExecCmd runcode
End Sub

Private Sub txtAll_Change()
  If txtAll.Text = "" Or Val(txtAll.Text) <= 0 Or Val(txtAll.Text) > 100 Then
    cmdAll.Enabled = False
  Else
    cmdAll.Enabled = True
  End If
End Sub

Function Errvals(Index As Long) As Boolean
  Dim low As Double
  Dim high As Double
  Dim tval As Double
  
  txtMes.Text = ""
  Errvals = False
  Select Case Index
    Case 0, 1
      low = 0.1
      high = 100
    Case 2, 3
      low = 2
      high = 50
    Case 4
      low = 0.02
      high = 50
    Case 5
      low = 1
      high = 100
    Case 6
      low = 1
      high = 500
    Case 7
      low = 0.1
      high = 1.4
    Case 8
      low = 0
      high = 1
    Case 9
      low = 1E-20
      high = 1
    Case 10
      low = 0
    Case 11
      low = 0
      high = 99.99
    Case 12 To 16
      low = 0
      high = 1000000
    Case Else
      Exit Function
  End Select
  
  If txt(Index) = "" Then Errvals = True
  Select Case Index
    Case 0, 1, 4, 11 To 16:
      tval = Val(convert(unit(Index).Text, unit(Index).Tag, Val(txt(Index).Text)))
      low = Val(convert(unit(Index).Tag, unit(Index).Text, low))
      high = Val(convert(unit(Index).Tag, unit(Index).Text, high))
      txtMes.Text = "Value must be between " + Str(low) + " and " + Str(high) + " " + unit(Index).Text
      If tval < low Or tval > high Then Errvals = True
    Case 10
      tval = Val(txt(Index).Text)
      txtMes.Text = "Value must be greater than zero!"
      If tval <= low Then Errvals = True
    Case Else
      tval = Val(txt(Index).Text)
      txtMes.Text = "Value must be between " + Str(low) + " and " + Str(high)
      If tval < low Or tval > high Then Errvals = True
  End Select
      
  If Errvals Then
    txt(Index).BackColor = &H8080FF
  Else
    txt(Index).BackColor = &HC0FFC0
  End If
End Function

Private Sub txt_Change(Index As Integer)
Dim chk As Double

On Error GoTo toolarge
  Errvals CLng(Index)
  Select Case Index
    Case 15, 16: GetSrc
    Case 0 To 4: CheckRecp
  End Select
  chk = CDbl(txt(Index))
  Exit Sub
toolarge:
  txt(Index).BackColor = &H8080FF
End Sub

Private Sub txt_GotFocus(Index As Integer)
  Errvals CLng(Index)
  Select Case Index
    Case 0 To 4:
      RefItem = 17
    Case Else
      RefItem = Index
  End Select
  mnuRef.Enabled = True
End Sub

Private Sub unit_GotFocus(Index As Integer)
  Errvals CLng(Index)
  Select Case Index
    Case 0 To 5:
      RefItem = 17
    Case Else
      RefItem = Index
  End Select
  mnuRef.Enabled = True
End Sub

Private Sub optRecp_GotFocus(Index As Integer)
  txtMes.Text = ""
  RefItem = 17
  mnuRef.Enabled = True
End Sub

Private Sub optSigParm_GotFocus(Index As Integer)
  txtMes.Text = ""
  RefItem = 18
  mnuRef.Enabled = True
End Sub

Private Sub cboTimeStep_GotFocus()
  txtMes.Text = ""
  RefItem = 19
  mnuRef.Enabled = True
End Sub

Private Sub cboUnit_GotFocus()
  txtMes.Text = ""
  RefItem = 20
  mnuRef.Enabled = True
End Sub

Private Sub spdGrid_GotFocus()
  txtMes.Text = ""
  RefItem = 20
  mnuRef.Enabled = True
End Sub

Private Sub cmdChange_GotFocus(Index As Integer)
  txtMes.Text = ""
  RefItem = -1
  mnuRef.Enabled = False
End Sub

Private Sub chkGrid_GotFocus()
  txtMes.Text = ""
  RefItem = 20
  mnuRef.Enabled = True
End Sub

Private Sub chkRise_GotFocus()
  txtMes.Text = ""
  RefItem = -1
  mnuRef.Enabled = False
End Sub

Private Sub SSTab1_GotFocus()
  txtMes.Text = ""
  RefItem = -1
  mnuRef.Enabled = False
End Sub

Private Sub SSTab3_GotFocus()
  txtMes.Text = ""
  RefItem = -1
  mnuRef.Enabled = False
End Sub

Private Sub SrcTab_GotFocus()
  txtMes.Text = ""
  RefItem = -1
  mnuRef.Enabled = False
End Sub

Private Sub cmdProcess_GotFocus(Index As Integer)
  txtMes.Text = ""
  RefItem = -1
  mnuRef.Enabled = False
End Sub

Private Sub SetSrc()
  
  If loadng Then Exit Sub
  With airsrc(used(SrcTab.Tab))
    loadng = True
    lblSrc.Caption = "Source: " + .Name
    chkRise.Value = .riseflg
    
    ref(15).Tag = .sigr.ref
    ref(15).Caption = "Ref: " + Str(.sigr.ref)
    set_unit unit(15), .sigr.uunit
    txt(15).Text = .sigr.Value
    
    ref(16).Tag = .sigz.ref
    ref(16).Caption = "Ref: " + Str(.sigz.ref)
    set_unit unit(16), .sigz.uunit
    txt(16).Text = .sigz.Value
    loadng = False
  End With
End Sub

Private Sub GetSrc()
  
  If loadng Then Exit Sub
  With airsrc(used(SrcTab.Tab))
    .riseflg = chkRise.Value
    
    .sigr.ref = ref(15).Tag
    .sigr.uunit = unit(15).Text
    .sigr.Value = Val(txt(15).Text)
    
    .sigz.ref = ref(16).Tag
    .sigz.uunit = unit(16).Text
    .sigz.Value = Val(txt(16).Text)
  End With
End Sub
