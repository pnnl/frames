VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "tabctl32.ocx"
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "threed32.ocx"
Object = "{B02F3647-766B-11CE-AF28-C3A2FBE76A13}#2.5#0"; "SS32X25.OCX"
Begin VB.Form frmInpChPlm 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Input Model Parameters"
   ClientHeight    =   5760
   ClientLeft      =   240
   ClientTop       =   768
   ClientWidth     =   7680
   Icon            =   "ChronicPlume.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   480
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   640
   StartUpPosition =   2  'CenterScreen
   Begin VB.TextBox txtMes 
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
      Height          =   372
      Left            =   0
      TabIndex        =   0
      TabStop         =   0   'False
      Top             =   5376
      Width           =   7692
   End
   Begin TabDlg.SSTab SSTab1 
      Height          =   5388
      Left            =   0
      TabIndex        =   1
      Top             =   0
      Width           =   7692
      _ExtentX        =   13568
      _ExtentY        =   9504
      _Version        =   393216
      Style           =   1
      Tabs            =   2
      TabsPerRow      =   2
      TabHeight       =   423
      TabCaption(0)   =   "Model Information"
      TabPicture(0)   =   "ChronicPlume.frx":030A
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "SSTab3"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).ControlCount=   1
      TabCaption(1)   =   "Source Information"
      TabPicture(1)   =   "ChronicPlume.frx":0326
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "SrcTab"
      Tab(1).Control(1)=   "frmSrc"
      Tab(1).ControlCount=   2
      Begin VB.Frame frmSrc 
         Height          =   4116
         Left            =   -74712
         TabIndex        =   49
         Top             =   840
         Width           =   7020
         Begin VB.Frame frmEnch 
            BorderStyle     =   0  'None
            Height          =   2388
            Left            =   192
            TabIndex        =   56
            Top             =   1344
            Visible         =   0   'False
            Width           =   2964
            Begin VB.OptionButton optEnch 
               Caption         =   "PNNL "
               Height          =   252
               Index           =   0
               Left            =   96
               TabIndex        =   63
               Top             =   1440
               Width           =   2700
            End
            Begin VB.CheckBox chkEnch 
               Caption         =   "Buoyance Induced Dispersion"
               Height          =   192
               Index           =   2
               Left            =   288
               TabIndex        =   62
               Top             =   1056
               Width           =   2700
            End
            Begin VB.OptionButton optEnch 
               Caption         =   "EPA's ISC"
               Height          =   192
               Index           =   1
               Left            =   120
               TabIndex        =   61
               Top             =   192
               Width           =   2700
            End
            Begin VB.CheckBox chkEnch 
               Caption         =   "Building Wakes"
               Height          =   192
               Index           =   0
               Left            =   288
               TabIndex        =   60
               Top             =   480
               Width           =   2700
            End
            Begin VB.CheckBox chkEnch 
               Caption         =   "Use Lower Bound"
               Height          =   192
               Index           =   1
               Left            =   288
               TabIndex        =   59
               Top             =   768
               Width           =   2700
            End
            Begin VB.CheckBox chkEnch 
               Caption         =   "High wind correction"
               Height          =   192
               Index           =   3
               Left            =   288
               TabIndex        =   58
               Top             =   1728
               Width           =   2700
            End
            Begin VB.CheckBox chkEnch 
               Caption         =   "Low wind correction"
               Height          =   192
               Index           =   4
               Left            =   288
               TabIndex        =   57
               Top             =   2016
               Width           =   2700
            End
         End
         Begin VB.CheckBox chkRise 
            Caption         =   "Do Plume Rise"
            Height          =   252
            Left            =   240
            TabIndex        =   55
            Top             =   624
            Width           =   1932
         End
         Begin VB.CheckBox chkDisp 
            Caption         =   "Use Enhanced Dispersion"
            Height          =   252
            Left            =   240
            TabIndex        =   54
            Top             =   984
            Width           =   2292
         End
         Begin VB.Frame frmBld 
            BorderStyle     =   0  'None
            Height          =   3276
            Left            =   3168
            TabIndex        =   50
            Top             =   288
            Visible         =   0   'False
            Width           =   3756
            Begin FPSpread.vaSpread spdEnch 
               Height          =   2415
               Left            =   0
               TabIndex        =   69
               Top             =   840
               Width           =   3255
               _Version        =   131077
               _ExtentX        =   5741
               _ExtentY        =   4260
               _StockProps     =   64
               BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
                  Name            =   "MS Sans Serif"
                  Size            =   7.8
                  Charset         =   0
                  Weight          =   700
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               SpreadDesigner  =   "ChronicPlume.frx":0342
            End
            Begin VB.ComboBox cboUnit 
               Height          =   288
               Index           =   6
               Left            =   2304
               Style           =   2  'Dropdown List
               TabIndex        =   65
               Tag             =   "m"
               Top             =   384
               Width           =   996
            End
            Begin VB.ComboBox cboUnit 
               Height          =   288
               Index           =   5
               Left            =   960
               Style           =   2  'Dropdown List
               TabIndex        =   51
               Tag             =   "m"
               Top             =   384
               Width           =   996
            End
            Begin VB.Label lblref 
               AutoSize        =   -1  'True
               Caption         =   "Ref: 0 "
               Height          =   192
               Index           =   5
               Left            =   960
               TabIndex        =   66
               Tag             =   "0"
               Top             =   96
               Width           =   444
            End
            Begin VB.Label lblref 
               AutoSize        =   -1  'True
               Caption         =   "Ref: 0 "
               Height          =   192
               Index           =   6
               Left            =   2304
               TabIndex        =   53
               Tag             =   "0"
               Top             =   96
               Width           =   444
            End
            Begin VB.Label Label7 
               Caption         =   "Unit"
               Height          =   204
               Left            =   96
               TabIndex        =   52
               Top             =   384
               Width           =   684
            End
         End
         Begin VB.Label lblSrc 
            AutoSize        =   -1  'True
            Caption         =   "This is Source1 - Point (Area)"
            Height          =   192
            Left            =   192
            TabIndex        =   64
            Top             =   288
            Width           =   2064
         End
      End
      Begin TabDlg.SSTab SrcTab 
         Height          =   4716
         Left            =   -74904
         TabIndex        =   48
         Top             =   480
         Width           =   7404
         _ExtentX        =   13060
         _ExtentY        =   8319
         _Version        =   393216
         Style           =   1
         Tabs            =   5
         TabsPerRow      =   5
         TabHeight       =   420
         TabCaption(0)   =   "Source 1"
         TabPicture(0)   =   "ChronicPlume.frx":0471
         Tab(0).ControlEnabled=   -1  'True
         Tab(0).ControlCount=   0
         TabCaption(1)   =   "Source 2"
         TabPicture(1)   =   "ChronicPlume.frx":048D
         Tab(1).ControlEnabled=   0   'False
         Tab(1).ControlCount=   0
         TabCaption(2)   =   "Source 3"
         TabPicture(2)   =   "ChronicPlume.frx":04A9
         Tab(2).ControlEnabled=   0   'False
         Tab(2).ControlCount=   0
         TabCaption(3)   =   "Source 4"
         TabPicture(3)   =   "ChronicPlume.frx":04C5
         Tab(3).ControlEnabled=   0   'False
         Tab(3).ControlCount=   0
         TabCaption(4)   =   "Source 5"
         TabPicture(4)   =   "ChronicPlume.frx":04E1
         Tab(4).ControlEnabled=   0   'False
         Tab(4).ControlCount=   0
      End
      Begin TabDlg.SSTab SSTab3 
         Height          =   4716
         Left            =   120
         TabIndex        =   2
         Top             =   480
         Width           =   7404
         _ExtentX        =   13060
         _ExtentY        =   8319
         _Version        =   393216
         Style           =   1
         Tabs            =   4
         TabsPerRow      =   4
         TabHeight       =   423
         TabCaption(0)   =   "Radial Grid Definition"
         TabPicture(0)   =   "ChronicPlume.frx":04FD
         Tab(0).ControlEnabled=   -1  'True
         Tab(0).Control(0)=   "Frame1"
         Tab(0).Control(0).Enabled=   0   'False
         Tab(0).ControlCount=   1
         TabCaption(1)   =   "Model Parameters"
         TabPicture(1)   =   "ChronicPlume.frx":0519
         Tab(1).ControlEnabled=   0   'False
         Tab(1).Control(0)=   "Frame3"
         Tab(1).Control(1)=   "frmCalm"
         Tab(1).Control(2)=   "chkCalm"
         Tab(1).ControlCount=   3
         TabCaption(2)   =   "Default Parameters"
         TabPicture(2)   =   "ChronicPlume.frx":0535
         Tab(2).ControlEnabled=   0   'False
         Tab(2).Control(0)=   "Frame4"
         Tab(2).ControlCount=   1
         TabCaption(3)   =   "Meteorlogical Files"
         TabPicture(3)   =   "ChronicPlume.frx":0551
         Tab(3).ControlEnabled=   0   'False
         Tab(3).Control(0)=   "Frame5"
         Tab(3).ControlCount=   1
         Begin VB.Frame Frame5 
            Height          =   4140
            Left            =   -74808
            TabIndex        =   38
            Top             =   384
            Width           =   7020
            Begin Threed.SSCommand cmdProcess 
               Height          =   372
               Index           =   1
               Left            =   4080
               TabIndex        =   39
               Top             =   3408
               Width           =   2772
               _Version        =   65536
               _ExtentX        =   4890
               _ExtentY        =   656
               _StockProps     =   78
               Caption         =   "Use Joint Fequency Data file"
            End
            Begin Threed.SSCommand cmdProcess 
               Height          =   372
               Index           =   0
               Left            =   4080
               TabIndex        =   40
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
               TabIndex        =   41
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
               TabIndex        =   42
               Top             =   1248
               Width           =   2772
               _Version        =   65536
               _ExtentX        =   4890
               _ExtentY        =   656
               _StockProps     =   78
               Caption         =   "Browse for Cloud Shine Library"
            End
            Begin VB.Label lblFile 
               BackColor       =   &H80000005&
               BorderStyle     =   1  'Fixed Single
               Caption         =   "Label10"
               Height          =   276
               Index           =   1
               Left            =   240
               TabIndex        =   47
               Top             =   1656
               Width           =   6576
            End
            Begin VB.Label Label9 
               Caption         =   "Path and Name of Cloud Shine Library"
               Height          =   192
               Left            =   192
               TabIndex        =   46
               Top             =   1344
               Width           =   3504
            End
            Begin VB.Label lblFile 
               BackColor       =   &H80000005&
               BorderStyle     =   1  'Fixed Single
               Caption         =   "Label3"
               Height          =   276
               Index           =   0
               Left            =   240
               TabIndex        =   45
               Top             =   840
               Width           =   6576
            End
            Begin VB.Label Label1 
               Caption         =   "Path and Name of Meteorological Data File"
               Height          =   192
               Left            =   288
               TabIndex        =   44
               Top             =   480
               Width           =   3504
            End
            Begin VB.Label Label10 
               Caption         =   $"ChronicPlume.frx":056D
               Height          =   864
               Left            =   384
               TabIndex        =   43
               Top             =   2880
               Width           =   3396
               WordWrap        =   -1  'True
            End
         End
         Begin VB.Frame Frame4 
            Enabled         =   0   'False
            Height          =   2295
            Left            =   -74808
            TabIndex        =   21
            Top             =   384
            Width           =   7020
            Begin VB.ComboBox cboUnit 
               Height          =   315
               Index           =   4
               Left            =   4704
               Style           =   2  'Dropdown List
               TabIndex        =   71
               Tag             =   "m/s"
               Top             =   1800
               Width           =   1000
            End
            Begin VB.TextBox txt 
               BackColor       =   &H008080FF&
               Height          =   288
               Index           =   4
               Left            =   3744
               TabIndex        =   70
               Top             =   1800
               Width           =   1000
            End
            Begin VB.TextBox txt 
               BackColor       =   &H008080FF&
               Height          =   288
               Index           =   3
               Left            =   3744
               TabIndex        =   29
               Top             =   1440
               Width           =   1000
            End
            Begin VB.TextBox txt 
               BackColor       =   &H008080FF&
               Height          =   288
               Index           =   2
               Left            =   3744
               TabIndex        =   28
               Top             =   1056
               Width           =   1000
            End
            Begin VB.TextBox txt 
               BackColor       =   &H008080FF&
               Height          =   288
               Index           =   1
               Left            =   3744
               TabIndex        =   27
               Top             =   672
               Width           =   1000
            End
            Begin VB.TextBox txt 
               BackColor       =   &H008080FF&
               Height          =   288
               Index           =   0
               Left            =   3744
               TabIndex        =   26
               Top             =   288
               Width           =   1000
            End
            Begin VB.ComboBox cboUnit 
               Height          =   288
               Index           =   0
               Left            =   4704
               Style           =   2  'Dropdown List
               TabIndex        =   25
               Tag             =   "m/s"
               Top             =   288
               Width           =   1000
            End
            Begin VB.ComboBox cboUnit 
               Height          =   288
               Index           =   1
               Left            =   4704
               Style           =   2  'Dropdown List
               TabIndex        =   24
               Tag             =   "m"
               Top             =   672
               Width           =   1000
            End
            Begin VB.ComboBox cboUnit 
               Height          =   288
               Index           =   2
               Left            =   4704
               Style           =   2  'Dropdown List
               TabIndex        =   23
               Tag             =   "s/m"
               Top             =   1056
               Width           =   1000
            End
            Begin VB.ComboBox cboUnit 
               Height          =   315
               Index           =   3
               Left            =   4704
               Style           =   2  'Dropdown List
               TabIndex        =   22
               Tag             =   "s/m"
               Top             =   1440
               Width           =   1000
            End
            Begin VB.Label lbl 
               Caption         =   "Maximum windspeed for ""calm"""
               Height          =   195
               Index           =   4
               Left            =   288
               TabIndex        =   73
               Top             =   1800
               Width           =   3510
            End
            Begin VB.Label lblref 
               AutoSize        =   -1  'True
               Caption         =   "Ref: 0"
               Height          =   195
               Index           =   4
               Left            =   5760
               TabIndex        =   72
               Tag             =   "0"
               Top             =   1920
               Width           =   810
            End
            Begin VB.Label lbl 
               Caption         =   "Transfer resistance for Particles"
               Height          =   192
               Index           =   3
               Left            =   288
               TabIndex        =   37
               Top             =   1440
               Width           =   3504
            End
            Begin VB.Label lbl 
               Caption         =   "Transfer resistence for Iodine"
               Height          =   192
               Index           =   2
               Left            =   288
               TabIndex        =   36
               Top             =   1056
               Width           =   3504
            End
            Begin VB.Label lbl 
               Caption         =   "Sigma to shift to semi-infinite cloud shine"
               Height          =   192
               Index           =   1
               Left            =   288
               TabIndex        =   35
               Top             =   660
               Width           =   3504
            End
            Begin VB.Label lbl 
               Caption         =   "Minimum wind speed during plume rise"
               Height          =   192
               Index           =   0
               Left            =   288
               TabIndex        =   34
               Top             =   288
               Width           =   3504
            End
            Begin VB.Label lblref 
               AutoSize        =   -1  'True
               Caption         =   "Ref: 0 "
               Height          =   192
               Index           =   0
               Left            =   5760
               TabIndex        =   33
               Tag             =   "0"
               Top             =   384
               Width           =   804
            End
            Begin VB.Label lblref 
               AutoSize        =   -1  'True
               Caption         =   "Ref: 0"
               Height          =   192
               Index           =   1
               Left            =   5760
               TabIndex        =   32
               Tag             =   "0"
               Top             =   768
               Width           =   804
            End
            Begin VB.Label lblref 
               AutoSize        =   -1  'True
               Caption         =   "Ref: 0"
               Height          =   192
               Index           =   2
               Left            =   5760
               TabIndex        =   31
               Tag             =   "0"
               Top             =   1152
               Width           =   804
            End
            Begin VB.Label lblref 
               AutoSize        =   -1  'True
               Caption         =   "Ref: 0"
               Height          =   192
               Index           =   3
               Left            =   5760
               TabIndex        =   30
               Tag             =   "0"
               Top             =   1536
               Width           =   804
            End
         End
         Begin VB.CheckBox chkCalm 
            Caption         =   "Use user's supplied calm wind distribution"
            Height          =   252
            Left            =   -74616
            TabIndex        =   20
            Top             =   2592
            Width           =   3348
         End
         Begin VB.Frame frmCalm 
            BorderStyle     =   0  'None
            Height          =   1452
            Left            =   -74712
            TabIndex        =   17
            Top             =   2976
            Width           =   4044
            Begin FPSpread.vaSpread spdCalm 
               Height          =   1335
               Left            =   2280
               TabIndex        =   68
               Top             =   120
               Width           =   1710
               _Version        =   131077
               _ExtentX        =   3016
               _ExtentY        =   2355
               _StockProps     =   64
               AutoSize        =   -1  'True
               BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
                  Name            =   "MS Sans Serif"
                  Size            =   7.8
                  Charset         =   0
                  Weight          =   700
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               MaxCols         =   1
               MaxRows         =   16
               ScrollBars      =   2
               SpreadDesigner  =   "ChronicPlume.frx":0615
               VisibleCols     =   500
               VisibleRows     =   500
            End
            Begin VB.Label lblref 
               AutoSize        =   -1  'True
               Caption         =   "Ref: 0"
               Height          =   192
               Index           =   8
               Left            =   768
               TabIndex        =   19
               Tag             =   "0"
               Top             =   1056
               Width           =   588
            End
            Begin VB.Label Label3 
               Caption         =   "The calm wind distribution is the fraction the wind blows from the given direction "
               BeginProperty Font 
                  Name            =   "MS Sans Serif"
                  Size            =   7.8
                  Charset         =   0
                  Weight          =   700
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   972
               Left            =   192
               TabIndex        =   18
               Top             =   0
               Width           =   2028
               WordWrap        =   -1  'True
            End
         End
         Begin VB.Frame Frame3 
            Caption         =   "Sigma Parameterization Usage"
            Height          =   4140
            Left            =   -74808
            TabIndex        =   10
            Top             =   384
            Width           =   7020
            Begin VB.OptionButton optSigParm 
               Caption         =   "Turbulence Statistics"
               Height          =   280
               Index           =   4
               Left            =   192
               TabIndex        =   15
               Top             =   1440
               Width           =   2200
            End
            Begin VB.OptionButton optSigParm 
               Caption         =   "Pasquill-Gifford (ISC3)"
               Height          =   280
               Index           =   3
               Left            =   192
               TabIndex        =   14
               Top             =   288
               Width           =   2200
            End
            Begin VB.OptionButton optSigParm 
               Caption         =   "Pasquill-Gifford (NRC)"
               Height          =   280
               Index           =   2
               Left            =   192
               TabIndex        =   13
               Top             =   576
               Width           =   2200
            End
            Begin VB.OptionButton optSigParm 
               Caption         =   "Brigg's Urban Condition"
               Height          =   280
               Index           =   1
               Left            =   192
               TabIndex        =   12
               Top             =   864
               Width           =   2200
            End
            Begin VB.OptionButton optSigParm 
               Caption         =   "Brigg's Open Country"
               Height          =   280
               Index           =   0
               Left            =   192
               TabIndex        =   11
               Top             =   1152
               Value           =   -1  'True
               Width           =   2200
            End
            Begin VB.Label lblref 
               AutoSize        =   -1  'True
               Caption         =   "Ref: 0"
               Height          =   192
               Index           =   7
               Left            =   2880
               TabIndex        =   16
               Tag             =   "0"
               Top             =   864
               Width           =   588
            End
         End
         Begin VB.Frame Frame1 
            Height          =   4140
            Left            =   192
            TabIndex        =   3
            Top             =   384
            Width           =   7020
            Begin FPSpread.vaSpread spdRadii 
               Height          =   2655
               Left            =   4560
               TabIndex        =   67
               Top             =   1200
               Width           =   1485
               _Version        =   131077
               _ExtentX        =   2604
               _ExtentY        =   4445
               _StockProps     =   64
               AutoSize        =   -1  'True
               BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
                  Name            =   "MS Sans Serif"
                  Size            =   7.8
                  Charset         =   0
                  Weight          =   700
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               MaxCols         =   1
               MaxRows         =   10
               ScrollBarExtMode=   -1  'True
               SpreadDesigner  =   "ChronicPlume.frx":0783
               VisibleCols     =   500
               VisibleRows     =   500
            End
            Begin VB.ComboBox cboUnit 
               Height          =   288
               Index           =   9
               ItemData        =   "ChronicPlume.frx":08F1
               Left            =   4704
               List            =   "ChronicPlume.frx":08F3
               Style           =   2  'Dropdown List
               TabIndex        =   4
               Tag             =   "m"
               Top             =   480
               Width           =   972
            End
            Begin Threed.SSOption optRecp 
               Height          =   252
               Index           =   0
               Left            =   384
               TabIndex        =   5
               Top             =   480
               Width           =   1932
               _Version        =   65536
               _ExtentX        =   3408
               _ExtentY        =   444
               _StockProps     =   78
               Caption         =   "16 Sectors in radial grid"
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
            Begin Threed.SSOption optRecp 
               Height          =   252
               Index           =   1
               Left            =   384
               TabIndex        =   6
               Top             =   864
               Width           =   1932
               _Version        =   65536
               _ExtentX        =   3408
               _ExtentY        =   444
               _StockProps     =   78
               Caption         =   "36 Sectors in radial grid"
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
            Begin VB.Label Label11 
               Caption         =   "Unit"
               Height          =   204
               Left            =   4128
               TabIndex        =   9
               Top             =   480
               Width           =   420
            End
            Begin VB.Label lblref 
               AutoSize        =   -1  'True
               Caption         =   "Ref: 0 "
               Height          =   252
               Index           =   9
               Left            =   5760
               TabIndex        =   8
               Tag             =   "0"
               Top             =   480
               Width           =   996
            End
            Begin VB.Label Label8 
               Caption         =   "Please fill in all radial distances.  Distances are required to be entered in acending order.  "
               BeginProperty Font 
                  Name            =   "MS Sans Serif"
                  Size            =   7.8
                  Charset         =   0
                  Weight          =   700
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   972
               Left            =   1056
               TabIndex        =   7
               Top             =   1824
               Width           =   2412
            End
         End
      End
   End
   Begin MSComDlg.CommonDialog dlgOpenFile 
      Left            =   0
      Top             =   5376
      _ExtentX        =   699
      _ExtentY        =   699
      _Version        =   393216
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
Attribute VB_Name = "frmInpChPlm"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Dim msgtext As String
Dim temp As parmrec
Dim maxrecp As Long
Dim srcmaxrecp As Long
Dim numsrc As Long
Dim f_srcname() As String
Dim numairsrc As Long
Dim airsrc() As srcvals
Dim numused As Long
Dim used(5) As Long
Dim loadng As Boolean

Sub set_header(pnlflag As Long)
  Dim i As Long

  If pnlflag = 0 Then
    spdEnch.MaxCols = 2
    spdEnch.col = 1
    spdEnch.Row = 0
    spdEnch.Text = "Build Height"
    spdEnch.col = 2
    spdEnch.ColWidth(2) = 11.2
    spdEnch.Text = "Build Width"
  Else
    spdEnch.MaxCols = 1
    spdEnch.col = 1
    spdEnch.Row = 0
    spdEnch.Text = "Build Area"
  End If
End Sub

Sub Set_Spread()
  Dim dirval(16) As String
  Dim i As Long
  Dim j As Long
  
  dirval(1) = "N"
  dirval(2) = "NNE"
  dirval(3) = "NE"
  dirval(4) = "ENE"
  dirval(5) = "E"
  dirval(6) = "ESE"
  dirval(7) = "SE"
  dirval(8) = "SSE"
  dirval(9) = "S"
  dirval(10) = "SSW"
  dirval(11) = "SW"
  dirval(12) = "WSW"
  dirval(13) = "W"
  dirval(14) = "WNW"
  dirval(15) = "NW"
  dirval(16) = "NNW"

  If optRecp(0).Value = True Then
    maxrecp = 16
    spdEnch.MaxRows = maxrecp
    For i = 1 To maxrecp
      spdCalm.col = 0
      spdCalm.Row = i
      spdCalm.Text = dirval(i)
      
      spdEnch.col = 0
      spdEnch.Row = i
      spdEnch.Text = dirval(i)
    Next i
  Else
    maxrecp = 36
    spdEnch.MaxRows = maxrecp
    For i = 1 To maxrecp
      spdCalm.col = 0
      spdCalm.Row = i
      spdCalm.Text = (i - 1) * 10
      
      spdEnch.col = 0
      spdEnch.Row = i
      spdEnch.Text = (i - 1) * 10
    Next i
    If airsrc(used(SrcTab.Tab)).highflg = 1 Then
      For i = 17 To maxrecp
        spdEnch.col = 1
        spdEnch.Row = i
        spdEnch.Text = airsrc(used(SrcTab.Tab)).bldarea(i)
      Next i
    End If
    If airsrc(used(SrcTab.Tab)).wakeflg = 1 Then
      For i = 17 To maxrecp
        spdEnch.col = 1
        spdEnch.Row = i
        spdEnch.Text = airsrc(used(SrcTab.Tab)).bldhgt(i)
        spdEnch.col = 2
        spdEnch.Text = airsrc(used(SrcTab.Tab)).bldwdth(i)
      Next i
    End If
  End If
End Sub

Function er(Index As Long) As Boolean
  Dim low As Double
  Dim high As Double
  Dim tval As Double
  
  txtMes.Text = ""
  er = False
  If Index > 4 Then Exit Function
  Select Case Index
    Case 0
      low = 0
      high = 99.99
    Case 1
      low = 0
      high = 10000
    Case 2
      low = 0
      high = 1000000
    Case 3
      low = 0
      high = 1000000
    Case 4
      low = 0
      high = 2
  End Select
  tval = Val(txt(Index).Text)
  low = Val(convert(cboUnit(Index).Tag, cboUnit(Index).Text, low))
  high = Val(convert(cboUnit(Index).Tag, cboUnit(Index).Text, high))
  txtMes.Text = "Value must be between " + Str(low) + " and " + Str(high)
  If tval < low Or tval > high Or txt(Index).Text = "" Then
    er = True
  End If
      
  If er Then
    txt(Index).BackColor = &H8080FF
  Else
    txt(Index).BackColor = &HC0FFC0
  End If
End Function

Sub FillArray(idx As Long)
  Select Case (idx)
    Case (1) ' Radii
      If temp.idx3 > 10 Then
        msgtext = "Error - more than 10 receptor radii in GID file."
        put_val errfile, msgtext
        put_line errfile
        close_csv errfile
        MsgBox msgtext
        End
      End If
      lblref(9).Tag = temp.ref
      lblref(9).Caption = "Ref: " + Str(temp.ref)
      set_unit cboUnit(9), temp.uunit
      spdRadii.col = 1
      spdRadii.Row = temp.idx3
      spdRadii.Text = convert(temp.cunit, temp.uunit, Val(temp.pval))
    Case (2) 'Calm Distrb
      If temp.idx3 > maxrecp Then
        Exit Sub
      End If
      lblref(8).Tag = temp.ref
      lblref(8).Caption = "Ref: " + Str(temp.ref)
      spdCalm.col = 1
      spdCalm.Row = temp.idx3
      spdCalm.Text = temp.pval
  End Select
End Sub

Sub FillIt(idx As Long)
  lblref(idx).Tag = temp.ref
  lblref(idx).Caption = "Ref: " + Str(temp.ref)
  If temp.cunit <> "N/A" Then
    set_unit cboUnit(idx), temp.uunit
  End If
  txt(idx).Text = convert(temp.cunit, temp.uunit, Val(temp.pval))
End Sub

Sub SizeSrc(size As Long)
  If size > numairsrc Then
    numairsrc = size
    ReDim Preserve airsrc(numairsrc) As srcvals
  End If
End Sub
       
Sub Loadprm()
  Dim fle As parmfile
  Dim m As Long
  Dim i As Long
  Dim j As Long
  Dim nairsrc As Boolean
  
  'Initialize
  ReDim airsrc(0)
  srcmaxrecp = 0
  If open_parm(fle, FUIName, 2) Then
    Do Until EOCF(fle.file)
      If read_parmrec(fle, temp) Then
        Select Case temp.pname
          Case "fui"
            Loading.Gauge1.Max = Val(temp.idx1) * 4
            Loading.Gauge1.Value = 0
            For m = 1 To temp.idx1
              updategauge
              If read_parmrec(fle, temp) Then
                If temp.idx1 = siteIdx Then
                  Select Case temp.pname
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
                Select Case temp.pname
                  Case "arminrisespd":               FillIt 0
                  Case "arminsigyshift":             FillIt 1
                  Case "artransresist":              FillIt temp.idx3 + 1
                  Case "arminwind":                  FillIt 4
                  Case "arradval":                   FillArray 1
                  Case "arcalmdist":                 FillArray 2
                  Case "armetfile":                  lblFile(0).Caption = temp.pval
                  Case "arcldshnlib":                lblFile(1).Caption = temp.pval
                  Case "arnumrecring"
                    If Val(temp.pval) <> 36 Then
                      optRecp(0).Value = True
                    Else
                      optRecp(1).Value = True
                    End If
                  Case "arsigparm"
                    lblref(7).Tag = temp.ref
                    lblref(7).Caption = "Ref: " + Str(temp.ref)
                    If Val(temp.pval) > 5 Then
                      msgtext = "Invalid value for sigma parmeterization in GID file ( = " + temp.pval + ")" + Chr(13) + Chr(10)
                      msgtext = msgtext + "using default value."
                      MsgBox msgtext
                      optSigParm(0).Value = True
                    Else
                      optSigParm(Val(temp.pval) - 1).Value = True
                    End If
                  Case "arsrcnumb"
                    numairsrc = Val(temp.pval)
                    ReDim Preserve airsrc(numairsrc) As srcvals
                  Case "arsrcname"
                    SizeSrc temp.idx3
                    airsrc(temp.idx3).name = temp.pval
                  Case "arsrcdoriseflag"
                    SizeSrc temp.idx3
                    airsrc(temp.idx3).riseflg = Val(temp.pval)
                  Case "arsrcdodispflag"
                    SizeSrc temp.idx3
                    airsrc(temp.idx3).dispflg = Val(temp.pval)
                    airsrc(temp.idx3).areaunit = "m2"
                    airsrc(temp.idx3).areacunit = "m2"
                    airsrc(temp.idx3).hgtunit = "m"
                    airsrc(temp.idx3).hgtcunit = "m"
                    airsrc(temp.idx3).wdthunit = "m"
                    airsrc(temp.idx3).wdthcunit = "m"
                  Case "ariscflag"
                    SizeSrc temp.idx3
                    airsrc(temp.idx3).iscflg = Val(temp.pval)
                  Case "ariscwakeflag"
                    SizeSrc temp.idx3
                    airsrc(temp.idx3).wakeflg = Val(temp.pval)
                  Case "arlowboundflag"
                    SizeSrc temp.idx3
                    airsrc(temp.idx3).boundflg = Val(temp.pval)
                  Case "arbidflag"
                    SizeSrc temp.idx3
                    airsrc(temp.idx3).bidflg = Val(temp.pval)
                  Case "arhghwndcorflag"
                    SizeSrc temp.idx3
                    airsrc(temp.idx3).highflg = Val(temp.pval)
                  Case "arlowwndcorflag"
                    SizeSrc temp.idx3
                    airsrc(temp.idx3).lowflg = Val(temp.pval)
                  Case "arbldhgt"
                    SizeSrc temp.idx3
                    airsrc(temp.idx3).bldhgt(temp.idx4) = Val(temp.pval)
                    airsrc(temp.idx3).hgtref = temp.ref
                    airsrc(temp.idx3).hgtunit = temp.uunit
                    airsrc(temp.idx3).hgtcunit = temp.cunit
                    If srcmaxrecp < temp.idx4 Then
                      srcmaxrecp = temp.idx4
                    End If
                  Case "arbldwdth"
                    SizeSrc temp.idx3
                    airsrc(temp.idx3).bldwdth(temp.idx4) = Val(temp.pval)
                    airsrc(temp.idx3).wdthref = temp.ref
                    airsrc(temp.idx3).wdthunit = temp.uunit
                    airsrc(temp.idx3).wdthcunit = temp.cunit
                    If srcmaxrecp < temp.idx4 Then
                      srcmaxrecp = temp.idx4
                    End If
                  Case "arbldarea"
                    SizeSrc temp.idx3
                    airsrc(temp.idx3).bldarea(temp.idx4) = Val(temp.pval)
                    airsrc(temp.idx3).arearef = temp.ref
                    airsrc(temp.idx3).areaunit = temp.uunit
                    airsrc(temp.idx3).areacunit = temp.cunit
                    If srcmaxrecp < temp.idx4 Then
                      srcmaxrecp = temp.idx4
                    End If
                  Case "arcalmdistflag"
                    If Val(temp.pval) = 1 Then
                      chkCalm.Value = 1
                    Else
                      chkCalm.Value = 0
                    End If
                    chkCalm_Click
'                  Case "arsectorflag"
'                    If temp.pval = 1 Then
'                      chkSector.Value = 1
'                    Else
'                      chkSector.Value = 0
'                    End If
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
        If f_srcname(i) = airsrc(j).name Then
          used(numused) = j
          numused = numused + 1
          Exit For
        End If
      Next j
      If j > numairsrc Then
        SizeSrc j
        airsrc(j).name = f_srcname(i)
        airsrc(j).hgtunit = "m"
        airsrc(j).hgtcunit = "m"
        airsrc(j).wdthunit = "m"
        airsrc(j).wdthcunit = "m"
        airsrc(j).areaunit = "m2"
        airsrc(j).areacunit = "m2"
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
  Dim defrad(10) As Double
  
  'Set Default Radii
  defrad(1) = 805
  defrad(2) = 2414
  defrad(3) = 4023
  defrad(4) = 5632
  defrad(5) = 7241
  defrad(6) = 12069
  defrad(7) = 24135
  defrad(8) = 40255
  defrad(9) = 56315
  defrad(10) = 72405
  
  GetArguments
  
  If argc >= 5 Then
    Loading.Gauge1.Max = 1500
    Loading.Show
    loadng = True
    FUIName = LTrim(Trim(argv(0))) & ".GID"
    RunName = LTrim(RTrim(argv(1)))
    siteIdx = Val(argv(2))
    modIdx = Val(argv(3))
    modName = argv(4)
    SetRefFile ReplaceExt(FUIName, "ref")
    frmInpChPlm.Caption = "GENII NESHAPS Chronic Plume Model - " + modName
    
    If open_csv(errfile, RunName & ".ERR", 1) Then
      put_val errfile, "Error report for Atmospheric Model"
      put_line errfile
    Else
      MsgBox "Unable to create file " & RunName & ".ERR" & Chr(10) & "Check directory permission"
      End
    End If
    
    err = False
    RefItem = -1
    
    'Setup the Units
    load_convert
    For i = 0 To 6
      get_conversion_items cboUnit(i).Tag, cboUnit(i)
    Next i
    get_conversion_items cboUnit(9).Tag, cboUnit(9)
    
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
    
    If Dir(App.Path + "\inpjfdp.exe") <> "" And Dir(App.Path + "\jjfdproc.exe") <> "" Then
      cmdProcess(1).Enabled = True
    Else
      cmdProcess(1).Enabled = False
    End If
    
    txt(0).Text = "1.5"
    txt(1).Text = "400.0"
    txt(2).Text = "10.0"
    txt(3).Text = "100.0"
    txt(4).Text = "0.8"
    optSigParm(3).Value = True
    optRecp(0).Value = True
    chkCalm.Value = 0
    chkCalm_Click
    Set_Spread
    
    Loadprm
    
    spdRadii.col = 1
    spdRadii.Row = 1
    If spdRadii.Text = "" Then
      'If no values loading - set to defaults
      spdRadii.col = 1
      For i = 1 To 10
        spdRadii.Row = i
        spdRadii.Text = defrad(i)
      Next i
    End If
    
    Unload Loading

  Else
    MsgBox "Not enough arguements passed" & Chr(10) & "Contact PNNL"
    close_csv errfile
    mnuFiExit_Click
    
  End If
  
  'Set Tab and screen
  SSTab1.Tab = 0
  SrcTab_Click 0
  loadng = False
End Sub

Private Sub mnuFiExit_Click()
  close_csv errfile
  If Dir(RunName & ".ERR") <> "" Then
    Kill RunName & ".ERR"
  End If
  End
End Sub

Private Sub mnuFiSave_Click()
  Dim fname As String
  Dim fle As parmfile
  Dim i As Long
  Dim keyword(4) As String
  Dim fileword(2) As String
  Dim parm As parmrec
  Dim isig As Long
  Dim numring As Long
  Dim radii As Double
  Dim esum As Long
  Dim isrc As Long
  Dim bldhgt As Double
  Dim bldwdth As Double
  Dim bldarea As Double
  Dim dsum As Double
  Dim mtext As String
  Dim errflag As Boolean
  
  keyword(0) = "ARMINRISESPD"
  keyword(1) = "ARMINSIGYSHIFT"
  keyword(2) = "ARTRANSRESIST"
  keyword(3) = "ARTRANSRESIST"
  keyword(4) = "ARMINWIND"
  
  fileword(0) = "ARMETFILE"
  fileword(1) = "ARCLDSHNLIB"
  
  errflag = False
  fname = RunName & ".GID"
  If open_parm(fle, fname, 1) Then
    For i = 0 To 4
      If er(i) Then
        errflag = True
        put_val errfile, "Error in variable " + keyword(i)
        put_line errfile
      End If
      If i < 2 Or i = 4 Then
        set_parm parm, keyword(i), siteIdx, modIdx, 0, 0, 0, 0, lblref(i).Tag, cboUnit(i).Text, cboUnit(i).Tag, convert(cboUnit(i).Text, cboUnit(i).Tag, Val(txt(i).Text))
      ElseIf i < 4 Then
        set_parm parm, keyword(i), siteIdx, modIdx, i - 1, 0, 0, 0, lblref(i).Tag, cboUnit(i).Text, cboUnit(i).Tag, convert(cboUnit(i).Text, cboUnit(i).Tag, Val(txt(i).Text))
      End If
      write_parmrec fle, parm
    Next i
    
    isig = 9
    For i = 1 To 5
      If optSigParm(i - 1).Value = True Then
        isig = i
        Exit For
      End If
    Next i
    If isig > 5 Then
      put_val errfile, "No sigma parameterization picked"
      put_line errfile
      errflag = True
    Else
      set_parm parm, "ARSIGPARM", siteIdx, modIdx, 0, 0, 0, 0, lblref(7).Tag, "N/A", "N/A", Str(isig)
      write_parmrec fle, parm
    End If
    
    numring = 0
    spdRadii.col = 1
    For i = 1 To 10
      spdRadii.Row = i
      If spdRadii.Text <> "" Then
        radii = Val(convert(cboUnit(9).Text, cboUnit(9).Tag, Val(spdRadii.Text)))
        If radii < 0 Or radii > 999999 Then
          errflag = True
          mtext = "Invalid Receptor radii - must be greater than zero and less than " + convert(cboUnit(9).Tag, cboUnit(9).Text, 999999) + " " + cboUnit(9).Text
          put_val errfile, mtext
          put_line errfile
        Else
          numring = numring + 1
          set_parm parm, "ARRADVAL", siteIdx, modIdx, numring, 0, 0, 0, lblref(9).Tag, cboUnit(9).Text, cboUnit(9).Tag, Str(radii)
          write_parmrec fle, parm
        End If
      End If
    Next i
    If numring = 0 Then
      put_val errfile, "No Receptor radii have been entered."
      put_line errfile
      errflag = True
    End If
    
    set_parm parm, "ARNUMRECRING", siteIdx, modIdx, 0, 0, 0, 0, 0, "N/A", "N/A", Str(maxrecp)
    write_parmrec fle, parm
    set_parm parm, "ARSRCNUMB", siteIdx, modIdx, 0, 0, 0, 0, 0, "N/A", "N/A", Str(numsrc)
    write_parmrec fle, parm
    
    For isrc = 1 To numused
      With airsrc(used(isrc - 1))
        set_parm parm, "ARSRCNAME", siteIdx, modIdx, isrc, 0, 0, 0, 0, "N/A", "N/A", .name
        write_parmrec fle, parm
        set_parm parm, "ARSRCDORISEFLAG", siteIdx, modIdx, isrc, 0, 0, 0, 0, "N/A", "N/A", Str(.riseflg)
        write_parmrec fle, parm
      
        If .dispflg = 1 Then
          If .iscflg = 1 Then
            esum = .wakeflg + .boundflg + .bidflg
            If esum = 0 Then
              set_parm parm, "ARSRCDODISPFLAG", siteIdx, modIdx, isrc, 0, 0, 0, 0, "N/A", "N/A", Str(0)
              write_parmrec fle, parm
            Else
              set_parm parm, "ARSRCDODISPFLAG", siteIdx, modIdx, isrc, 0, 0, 0, 0, "N/A", "N/A", Str(1)
              write_parmrec fle, parm
              set_parm parm, "ARISCFLAG", siteIdx, modIdx, isrc, 0, 0, 0, 0, "N/A", "N/A", Str(1)
              write_parmrec fle, parm
              set_parm parm, "ARISCWAKEFLAG", siteIdx, modIdx, isrc, 0, 0, 0, 0, "N/A", "N/A", Str(.wakeflg)
              write_parmrec fle, parm
              set_parm parm, "ARLOWBOUNDFLAG", siteIdx, modIdx, isrc, 0, 0, 0, 0, "N/A", "N/A", Str(.boundflg)
              write_parmrec fle, parm
              set_parm parm, "ARBIDFLAG", siteIdx, modIdx, isrc, 0, 0, 0, 0, "N/A", "N/A", Str(.bidflg)
              write_parmrec fle, parm
              
              If .wakeflg = 1 Then
                esum = 0
                For i = 1 To maxrecp
                  bldhgt = Val(convert(.hgtunit, .hgtcunit, .bldhgt(i)))
                  bldwdth = Val(convert(.wdthunit, .wdthcunit, .bldwdth(i)))
                  If bldhgt < 0 Or bldhgt > 9999 Then
                    esum = 1
                  ElseIf bldwdth < 0 Or bldwdth > 9999 Then
                    esum = 1
                  ElseIf (bldhgt > 0 And bldwdth = 0) Or (bldhgt = 0 And bldwdth > 0) Then
                    esum = 1
                  Else
                    set_parm parm, "ARBLDHGT", siteIdx, modIdx, isrc, i, 0, 0, .hgtref, .hgtunit, .hgtcunit, Str(bldhgt)
                    write_parmrec fle, parm
                    set_parm parm, "ARBLDWDTH", siteIdx, modIdx, isrc, i, 0, 0, .wdthref, .wdthunit, .wdthcunit, Str(bldwdth)
                    write_parmrec fle, parm
                  End If
                Next i
                If esum = 1 Then
                  put_val errfile, "Error in building height and/or widths given"
                  put_line errfile
                  errflag = True
                End If
              End If
            End If
          Else
            esum = .highflg + .lowflg
            If esum = 0 Then
              set_parm parm, "ARSRCDODISPFLAG", siteIdx, modIdx, isrc, 0, 0, 0, 0, "N/A", "N/A", Str(0)
              write_parmrec fle, parm
            Else
              set_parm parm, "ARSRCDODISPFLAG", siteIdx, modIdx, isrc, 0, 0, 0, 0, "N/A", "N/A", Str(1)
              write_parmrec fle, parm
              set_parm parm, "ARISCFLAG", siteIdx, modIdx, isrc, 0, 0, 0, 0, "N/A", "N/A", Str(0)
              write_parmrec fle, parm
              set_parm parm, "ARHGHWNDCORFLAG", siteIdx, modIdx, isrc, 0, 0, 0, 0, "N/A", "N/A", Str(.highflg)
              write_parmrec fle, parm
              set_parm parm, "ARLOWWNDCORFLAG", siteIdx, modIdx, isrc, 0, 0, 0, 0, "N/A", "N/A", Str(.lowflg)
              write_parmrec fle, parm
              
              If .highflg = 1 Then
                esum = 0
                For i = 1 To maxrecp
                  bldarea = Val(convert(.areaunit, .areacunit, .bldarea(i)))
                  If bldarea < 0 Or bldarea > 1000000# Then
                    esum = 1
                  Else
                    set_parm parm, "ARBLDAREA", siteIdx, modIdx, isrc, i, 0, 0, .arearef, .areaunit, .areacunit, Str(bldarea)
                    write_parmrec fle, parm
                  End If
                Next i
                
                If esum = 1 Then
                  put_val errfile, "Error in building areas given"
                  put_line errfile
                  errflag = True
                End If
              End If
            End If
          End If
        Else
          set_parm parm, "ARSRCDODISPFLAG", siteIdx, modIdx, isrc, 0, 0, 0, 0, "N/A", "N/A", Str(0)
          write_parmrec fle, parm
        End If
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
      
'    set_parm parm, "ARSECTORFLAG", siteIdx, modIdx, 0, 0, 0, 0, 0, "N/A", "N/A", Str(chkSector.Value)
'    write_parmrec fle, parm
    set_parm parm, "ARCALMDISTFLAG", siteIdx, modIdx, 0, 0, 0, 0, 0, "N/A", "N/A", Str(chkCalm.Value)
    write_parmrec fle, parm
    
    If chkCalm.Value = 1 Then
      dsum = 0
      spdCalm.col = 1
      For i = 1 To maxrecp
        spdCalm.Row = i
        dsum = dsum + Val(spdCalm.Text)
      Next i
      If Abs(dsum - 1) > 0.000001 Then
        put_val errfile, "Sum of calm distribution does not equal 1"
        put_line errfile
        errflag = True
      Else
        For i = 1 To maxrecp
          spdCalm.Row = i
          dsum = Val(spdCalm.Text)
          If dsum < 0 Or dsum > 1 Then
            put_val errfile, "In calm distribution value must between zero and 1"
            put_line errfile
            errflag = True
          Else
            set_parm parm, "ARCALMDIST", siteIdx, modIdx, i, 0, 0, 0, lblref(8).Tag, "fraction", "fraction", Str(dsum)
            write_parmrec fle, parm
          End If
        Next i
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

Private Sub mnuRefAdd_Click()
  RefMode = 1
  GetRef lblref(RefItem)
End Sub

Private Sub mnuRefSel_Click()
  RefMode = 0
  GetRef lblref(RefItem)
End Sub

Private Sub cboUnit_Click(Index As Integer)
  If loadng Then Exit Sub
  Select Case Index
    Case 5:
      If optEnch(1).Value Then
        airsrc(used(SrcTab.Tab)).areaunit = cboUnit(Index).Text
      Else
        airsrc(used(SrcTab.Tab)).hgtunit = cboUnit(Index).Text
      End If
    Case 6:
      airsrc(used(SrcTab.Tab)).wdthunit = cboUnit(Index).Text
  End Select
End Sub

Private Sub chkCalm_Click()
  If chkCalm.Value = 1 Then
    frmCalm.Visible = True
  Else
    frmCalm.Visible = False
  End If
End Sub

Private Sub chkDisp_Click()
  If chkDisp.Value = 1 Then
    frmEnch.Visible = True
    optEnch(airsrc(used(SrcTab.Tab)).iscflg).Value = 1
    optEnch_Click airsrc(used(SrcTab.Tab)).iscflg
  Else
    frmEnch.Visible = False
    frmBld.Visible = False
  End If
  airsrc(used(SrcTab.Tab)).dispflg = chkDisp.Value
End Sub

Private Sub chkEnch_Click(Index As Integer)
  Select Case Index
    Case 0: airsrc(used(SrcTab.Tab)).wakeflg = chkEnch(Index).Value
            If chkEnch(Index).Value = 1 Then
              frmBld.Visible = True
              putspdEnch
            Else
              frmBld.Visible = False
              chkEnch(1).Value = 0
            End If
    Case 1: airsrc(used(SrcTab.Tab)).boundflg = chkEnch(Index).Value
            If chkEnch(Index).Value = 1 Then
              frmBld.Visible = True
              putspdEnch
              chkEnch(0).Value = 1
            End If
    Case 2: airsrc(used(SrcTab.Tab)).bidflg = chkEnch(Index).Value
    Case 3: airsrc(used(SrcTab.Tab)).highflg = chkEnch(Index).Value
            If chkEnch(Index).Value = 1 Then
              frmBld.Visible = True
              putspdEnch
            Else
              frmBld.Visible = False
            End If
    Case 4: airsrc(used(SrcTab.Tab)).lowflg = chkEnch(Index).Value
  End Select
End Sub

Private Sub chkRise_Click()
  airsrc(used(SrcTab.Tab)).riseflg = chkRise.Value
End Sub

Private Sub cmdChange_Click(Index As Integer)
  Dim oldname As String
  Dim name As String
  Dim pathway As String
  Dim ext As String
  Dim Infilter As String
  Dim newname As String
  Dim fileexist As Boolean
  Dim errtext As String
  Dim response As Long
  
  oldname = lblFile(Index).Caption
  If ChkFile(oldname) Then
    GetNamePathExt oldname, name, pathway, ext
  Else
    pathway = App.Path
  End If
  
  Select Case (Index)
    Case 0: Infilter = "All Files (*.*)|*.*|Meteorological Files (*.MET)|*.MET"
    Case 1: Infilter = "All Files (*.*)|*.*|Data File (*.DAT)|*.DAT"
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

Private Sub cmdProcess_Click(Index As Integer)
  Dim runcode As String
  Dim sh As Long
  Dim etext As String
  Dim de As Long
  
  If Index = 0 Then
    runcode = App.Path + "\inphrlyp.exe"
  Else
    runcode = App.Path + "\inpjfdp.exe"
  End If
  
  ExecCmd runcode
End Sub

Private Sub optEnch_Click(Index As Integer)
  If Index = 1 Then
    chkEnch(3).Value = 0
    chkEnch_Click 3
    chkEnch(4).Value = 0
    chkEnch_Click 4
    chkEnch(0).Enabled = True
    chkEnch(1).Enabled = True
    chkEnch(2).Enabled = True
    chkEnch(3).Enabled = False
    chkEnch(4).Enabled = False
    chkEnch(0).Value = airsrc(used(SrcTab.Tab)).wakeflg
    chkEnch_Click 0
    chkEnch(1).Value = airsrc(used(SrcTab.Tab)).boundflg
    chkEnch_Click 1
    chkEnch(2).Value = airsrc(used(SrcTab.Tab)).bidflg
    chkEnch_Click 2
  Else
    chkEnch(0).Value = 0
    chkEnch_Click 0
    chkEnch(1).Value = 0
    chkEnch_Click 1
    chkEnch(2).Value = 0
    chkEnch_Click 2
    chkEnch(0).Enabled = False
    chkEnch(1).Enabled = False
    chkEnch(2).Enabled = False
    chkEnch(3).Enabled = True
    chkEnch(4).Enabled = True
    chkEnch(3).Value = airsrc(used(SrcTab.Tab)).highflg
    chkEnch_Click 3
    chkEnch(4).Value = airsrc(used(SrcTab.Tab)).lowflg
    chkEnch_Click 4
  End If
  airsrc(used(SrcTab.Tab)).iscflg = Index
End Sub

Private Sub SrcTab_Click(PreviousTab As Integer)
  lblSrc.Caption = "Source: " + airsrc(used(SrcTab.Tab)).name
  chkRise.Value = airsrc(used(SrcTab.Tab)).riseflg
  chkRise_Click
  chkDisp.Value = airsrc(used(SrcTab.Tab)).dispflg
  chkDisp_Click
End Sub

Private Sub txt_Change(Index As Integer)
  Dim chk As Double
  On Error GoTo toolarge
  
  er CLng(Index)
  chk = CDbl(txt(Index).Text)
  Exit Sub
toolarge:
  txt(Index).BackColor = &H8080FF
End Sub

Private Sub optRecp_Click(Index As Integer, Value As Integer)
  Set_Spread
End Sub

Private Sub txt_GotFocus(Index As Integer)
  er CLng(Index)
  If Index < 4 Then
    RefItem = Index
    mnuRef.Enabled = True
  ElseIf Index = 4 Then
    RefItem = Index + 5
    mnuRef.Enabled = True
  Else
    RefItem = -1
    mnuRef.Enabled = False
  End If
End Sub

Private Sub optEnch_GotFocus(Index As Integer)
  txtMes.Text = ""
  RefItem = -1
  mnuRef.Enabled = False
End Sub

Private Sub optRecp_GotFocus(Index As Integer)
  txtMes.Text = ""
  RefItem = -1
  mnuRef.Enabled = False
End Sub

Private Sub optSigParm_GotFocus(Index As Integer)
  txtMes.Text = ""
  RefItem = 7
  mnuRef.Enabled = True
End Sub

Private Sub spdCalm_GotFocus()
  txtMes.Text = "In calm distribution value must between zero and 1"
  RefItem = 8
  mnuRef.Enabled = True
End Sub

Private Sub spdEnch_GotFocus()
  txtMes.Text = ""
  RefItem = 5
  mnuRef.Enabled = True
End Sub

Private Sub spdRadii_GotFocus()
  txtMes.Text = "Radial distance must be greater than zero and less than " + convert(cboUnit(4).Tag, cboUnit(4).Text, 999999) + " " + cboUnit(4).Text
  RefItem = 9
  mnuRef.Enabled = True
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

Private Sub SSTabSrc_GotFocus()
  txtMes.Text = ""
  RefItem = -1
  mnuRef.Enabled = False
End Sub

Private Sub chkRise_GotFocus()
  txtMes.Text = ""
  RefItem = -1
  mnuRef.Enabled = False
End Sub

Private Sub chkSector_GotFocus()
  txtMes = ""
  RefItem = -1
  mnuRef.Enabled = False
End Sub

Private Sub chkEnch_GotFocus(Index As Integer)
  txtMes.Text = ""
  RefItem = -1
  mnuRef.Enabled = False
End Sub

Private Sub cboUnit_GotFocus(Index As Integer)
  txtMes.Text = ""
  RefItem = Index
  mnuRef.Enabled = True
End Sub

Private Sub chkCalm_GotFocus()
  txtMes.Text = ""
  RefItem = -1
  mnuRef.Enabled = False
End Sub

Private Sub chkDisp_GotFocus()
  txtMes.Text = ""
  RefItem = -1
  mnuRef.Enabled = False
End Sub

Private Sub spdEnch_KeyUp(KeyCode As Integer, Shift As Integer)
  getspdEnch
End Sub

Private Sub getspdEnch()
  Dim i As Long

  If loadng Then Exit Sub
  If airsrc(used(SrcTab.Tab)).highflg = 1 Then
    airsrc(used(SrcTab.Tab)).arearef = Val(Right(lblref(5).Caption, Len(lblref(5).Caption) - 4))
    For i = 1 To maxrecp
      spdEnch.col = 1
      spdEnch.Row = i
      airsrc(used(SrcTab.Tab)).bldarea(i) = Val(spdEnch.Text)
    Next i
  End If
  If airsrc(used(SrcTab.Tab)).wakeflg = 1 Then
    airsrc(used(SrcTab.Tab)).hgtref = Val(Right(lblref(5).Caption, Len(lblref(5).Caption) - 4))
    airsrc(used(SrcTab.Tab)).wdthref = Val(Right(lblref(6).Caption, Len(lblref(6).Caption) - 4))
    For i = 1 To maxrecp
      spdEnch.col = 1
      spdEnch.Row = i
      airsrc(used(SrcTab.Tab)).bldhgt(i) = Val(spdEnch.Text)
      spdEnch.col = 2
      airsrc(used(SrcTab.Tab)).bldwdth(i) = Val(spdEnch.Text)
    Next i
  End If
End Sub

Private Sub putspdEnch()
  Dim i As Long
  
  loadng = True
  If airsrc(used(SrcTab.Tab)).highflg = 1 Then
    cboUnit(5).Clear
    cboUnit(5).Tag = airsrc(used(SrcTab.Tab)).areacunit
    get_conversion_items cboUnit(5).Tag, cboUnit(5)
    set_unit cboUnit(5), airsrc(used(SrcTab.Tab)).areaunit
    lblref(5).Tag = airsrc(used(SrcTab.Tab)).arearef
    lblref(5).Caption = "Ref: " + Str(airsrc(used(SrcTab.Tab)).arearef)
    cboUnit(6).Visible = False
    lblref(6).Visible = False
    set_header 1
    For i = 1 To maxrecp
      spdEnch.col = 1
      spdEnch.Row = i
      spdEnch.Text = airsrc(used(SrcTab.Tab)).bldarea(i)
    Next i
  End If
  If airsrc(used(SrcTab.Tab)).wakeflg = 1 Then
    cboUnit(5).Clear
    cboUnit(6).Clear
    cboUnit(5).Tag = airsrc(used(SrcTab.Tab)).hgtcunit
    get_conversion_items cboUnit(5).Tag, cboUnit(5)
    set_unit cboUnit(5), airsrc(used(SrcTab.Tab)).hgtunit
    cboUnit(6).Tag = airsrc(used(SrcTab.Tab)).wdthcunit
    get_conversion_items cboUnit(6).Tag, cboUnit(6)
    set_unit cboUnit(6), airsrc(used(SrcTab.Tab)).wdthunit
    lblref(5).Tag = airsrc(used(SrcTab.Tab)).hgtref
    lblref(5).Caption = "Ref: " + Str(airsrc(used(SrcTab.Tab)).hgtref)
    lblref(6).Tag = airsrc(used(SrcTab.Tab)).wdthref
    lblref(6).Caption = "Ref: " + Str(airsrc(used(SrcTab.Tab)).wdthref)
    cboUnit(6).Visible = True
    lblref(6).Visible = True
    set_header 0
    For i = 1 To maxrecp
      spdEnch.col = 1
      spdEnch.Row = i
      spdEnch.Text = airsrc(used(SrcTab.Tab)).bldhgt(i)
      spdEnch.col = 2
      spdEnch.Text = airsrc(used(SrcTab.Tab)).bldwdth(i)
    Next i
  End If
  loadng = False
End Sub
