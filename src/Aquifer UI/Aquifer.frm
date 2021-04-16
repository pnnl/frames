VERSION 5.00
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "tabctl32.ocx"
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "threed32.ocx"
Begin VB.Form Aquifer 
   BorderStyle     =   1  'Fixed Single
   ClientHeight    =   4695
   ClientLeft      =   1650
   ClientTop       =   2610
   ClientWidth     =   7680
   Icon            =   "Aquifer.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   NegotiateMenus  =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   313
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   512
   StartUpPosition =   2  'CenterScreen
   Begin VB.TextBox mes 
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
      ForeColor       =   &H00000000&
      Height          =   360
      Left            =   0
      Locked          =   -1  'True
      TabIndex        =   144
      TabStop         =   0   'False
      Top             =   4320
      Width           =   7680
   End
   Begin TabDlg.SSTab SSTab1 
      Height          =   4455
      Left            =   0
      TabIndex        =   22
      Top             =   0
      Width           =   7689
      _ExtentX        =   13547
      _ExtentY        =   7858
      _Version        =   393216
      Style           =   1
      Tabs            =   5
      TabsPerRow      =   5
      TabHeight       =   529
      TabCaption(0)   =   "Composition"
      TabPicture(0)   =   "Aquifer.frx":030A
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "SSFrame1(0)"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).Control(1)=   "Timer1"
      Tab(0).Control(1).Enabled=   0   'False
      Tab(0).ControlCount=   2
      TabCaption(1)   =   "Characteristics"
      TabPicture(1)   =   "Aquifer.frx":0326
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "SSFrame1(1)"
      Tab(1).Control(0).Enabled=   0   'False
      Tab(1).ControlCount=   1
      TabCaption(2)   =   "Concentration Locations"
      TabPicture(2)   =   "Aquifer.frx":0342
      Tab(2).ControlEnabled=   0   'False
      Tab(2).Control(0)=   "SSFrame1(2)"
      Tab(2).Control(0).Enabled=   0   'False
      Tab(2).ControlCount=   1
      TabCaption(3)   =   "Flux Locations"
      TabPicture(3)   =   "Aquifer.frx":035E
      Tab(3).ControlEnabled=   0   'False
      Tab(3).Control(0)=   "SSFrame1(3)"
      Tab(3).Control(0).Enabled=   0   'False
      Tab(3).ControlCount=   1
      TabCaption(4)   =   "Constituent Properties"
      TabPicture(4)   =   "Aquifer.frx":037A
      Tab(4).ControlEnabled=   0   'False
      Tab(4).Control(0)=   "SSFrame1(4)"
      Tab(4).ControlCount=   1
      Begin VB.Timer Timer1 
         Interval        =   100
         Left            =   0
         Top             =   0
      End
      Begin Threed.SSFrame SSFrame1 
         Height          =   3492
         Index           =   1
         Left            =   -74760
         TabIndex        =   46
         Top             =   480
         Width           =   7212
         _Version        =   65536
         _ExtentX        =   12721
         _ExtentY        =   6160
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
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   8
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   61
            Tag             =   "percent"
            Top             =   1440
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   7
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   57
            Tag             =   "percent"
            Top             =   1080
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   0
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   49
            Tag             =   "percent"
            Top             =   360
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   28
            Left            =   4080
            TabIndex        =   76
            Tag             =   "wzdiv"
            Text            =   "1"
            Top             =   2880
            Visible         =   0   'False
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   0
            Left            =   4080
            TabIndex        =   48
            Tag             =   "wzfract"
            Top             =   360
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   11
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   73
            Tag             =   "g/cm^3"
            Top             =   2520
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   10
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   69
            Tag             =   "cm"
            Top             =   2160
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   9
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   65
            Tag             =   "cm/day"
            Top             =   1800
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   11
            Left            =   4080
            TabIndex        =   72
            Tag             =   "wzbulk"
            Top             =   2520
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   10
            Left            =   4080
            TabIndex        =   68
            Tag             =   "wzthick"
            Top             =   2160
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   9
            Left            =   4080
            TabIndex        =   64
            Tag             =   "wzpveloc"
            Top             =   1800
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   8
            Left            =   4080
            TabIndex        =   60
            Tag             =   "wzeffpor"
            Top             =   1440
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   7
            Left            =   4080
            TabIndex        =   56
            Tag             =   "wztotpor"
            Top             =   1080
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   6
            Left            =   4080
            TabIndex        =   52
            Tag             =   "wzph"
            Top             =   720
            Width           =   1000
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   28
            Left            =   6120
            TabIndex        =   77
            Tag             =   "0"
            Top             =   2910
            Visible         =   0   'False
            Width           =   990
         End
         Begin VB.Label Label46 
            Caption         =   "Number of subdivisions in the aquifer"
            Height          =   252
            Left            =   240
            TabIndex        =   75
            Top             =   2880
            Visible         =   0   'False
            Width           =   3800
         End
         Begin VB.Label Label35 
            Caption         =   "pH"
            Height          =   252
            Left            =   5160
            TabIndex        =   53
            Top             =   720
            Width           =   372
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   0
            Left            =   6120
            TabIndex        =   50
            Tag             =   "0"
            Top             =   390
            Width           =   990
         End
         Begin VB.Label Label21 
            Caption         =   "Percent constituent flux entering aquifer - WZ-FRACT"
            Height          =   252
            Left            =   240
            TabIndex        =   47
            Top             =   360
            Width           =   3804
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   11
            Left            =   6120
            TabIndex        =   74
            Tag             =   "0"
            Top             =   2550
            Width           =   990
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   10
            Left            =   6120
            TabIndex        =   70
            Tag             =   "0"
            Top             =   2190
            Width           =   990
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   9
            Left            =   6120
            TabIndex        =   66
            Tag             =   "0"
            Top             =   1830
            Width           =   990
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   8
            Left            =   6120
            TabIndex        =   62
            Tag             =   "0"
            Top             =   1470
            Width           =   990
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   7
            Left            =   6120
            TabIndex        =   58
            Tag             =   "0"
            Top             =   1110
            Width           =   990
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   6
            Left            =   6120
            TabIndex        =   54
            Tag             =   "0"
            Top             =   750
            Width           =   990
         End
         Begin VB.Label Label13 
            Caption         =   "Dry bulk density - WZ-BULKD"
            Height          =   252
            Left            =   240
            TabIndex        =   71
            Top             =   2520
            Width           =   3800
         End
         Begin VB.Label Label12 
            Caption         =   "Thickness of aquifer - WZ-THICK"
            Height          =   252
            Left            =   240
            TabIndex        =   67
            Top             =   2160
            Width           =   3800
         End
         Begin VB.Label Label11 
            Caption         =   "Darcy velocity - WZ-PVELOC"
            Height          =   264
            Left            =   240
            TabIndex        =   63
            Top             =   1800
            Width           =   3800
         End
         Begin VB.Label Label10 
            Caption         =   "Effective porosity - WZ-EFFPOR"
            Height          =   264
            Left            =   240
            TabIndex        =   59
            Top             =   1440
            Width           =   3800
         End
         Begin VB.Label Label9 
            Caption         =   "Total porosity - WZ-TOTPOR"
            Height          =   264
            Left            =   240
            TabIndex        =   55
            Top             =   1080
            Width           =   3800
         End
         Begin VB.Label Label8 
            Caption         =   "pH of the pore water - WZ-PH"
            Height          =   264
            Left            =   240
            TabIndex        =   51
            Top             =   720
            Width           =   3800
         End
      End
      Begin Threed.SSFrame SSFrame1 
         Height          =   3492
         Index           =   2
         Left            =   -74760
         TabIndex        =   78
         Top             =   480
         Width           =   7212
         _Version        =   65536
         _ExtentX        =   12721
         _ExtentY        =   6160
         _StockProps     =   14
         ForeColor       =   16776960
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
         Begin VB.CommandButton Command8 
            Caption         =   "<"
            Height          =   230
            Left            =   5310
            TabIndex        =   148
            Top             =   395
            Width           =   250
         End
         Begin VB.CommandButton Command7 
            Caption         =   ">"
            Height          =   230
            Left            =   5550
            TabIndex        =   147
            Top             =   395
            Width           =   230
         End
         Begin VB.ComboBox Combo2 
            Height          =   288
            Left            =   1680
            Style           =   2  'Dropdown List
            TabIndex        =   80
            Tag             =   "LOCATIONS"
            Top             =   360
            Width           =   4332
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   13
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   87
            Tag             =   "cm"
            Top             =   1200
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   12
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   83
            Tag             =   "cm"
            Top             =   840
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   13
            Left            =   4080
            TabIndex        =   86
            Tag             =   "wzcydist"
            Top             =   1200
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   12
            Left            =   4080
            TabIndex        =   82
            Tag             =   "wzcdist"
            Top             =   840
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   17
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   106
            Tag             =   "cm"
            Top             =   2880
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   16
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   101
            Tag             =   "cm"
            Top             =   2520
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   15
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   96
            Tag             =   "cm"
            Top             =   2160
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   14
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   91
            Tag             =   "cm"
            Top             =   1680
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   17
            Left            =   4080
            TabIndex        =   105
            Tag             =   "wzcvdisp"
            Top             =   2880
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   16
            Left            =   4080
            TabIndex        =   100
            Tag             =   "wzctdisp"
            Top             =   2520
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   15
            Left            =   4080
            TabIndex        =   95
            Tag             =   "wzcldisp"
            Top             =   2160
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   14
            Left            =   4080
            TabIndex        =   90
            Tag             =   "wzcaqdepth"
            Top             =   1680
            Width           =   1000
         End
         Begin Threed.SSCommand SSCommand3 
            Height          =   252
            Index           =   0
            Left            =   3120
            TabIndex        =   104
            Top             =   2880
            Width           =   900
            _Version        =   65536
            _ExtentX        =   1588
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "Estimate"
         End
         Begin Threed.SSCommand SSCommand2 
            Height          =   252
            Index           =   0
            Left            =   3120
            TabIndex        =   99
            Top             =   2520
            Width           =   900
            _Version        =   65536
            _ExtentX        =   1588
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "Estimate"
         End
         Begin Threed.SSCommand SSCommand1 
            Height          =   252
            Index           =   0
            Left            =   3120
            TabIndex        =   94
            Top             =   2160
            Width           =   900
            _Version        =   65536
            _ExtentX        =   1588
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "Estimate"
         End
         Begin VB.Label Label26 
            Caption         =   "Longitudinal distance to well - WZ-DIST"
            Height          =   252
            Left            =   120
            TabIndex        =   81
            Top             =   840
            Width           =   3804
         End
         Begin VB.Label Label25 
            Caption         =   "Vertical distance below water table to well intake - WZ-AQDEPTH"
            Height          =   492
            Left            =   120
            TabIndex        =   89
            Top             =   1680
            Width           =   3804
         End
         Begin VB.Label Label19 
            Caption         =   "Perpendicular distance from plume center line to well - WZ-YDIST"
            Height          =   372
            Left            =   120
            TabIndex        =   85
            Top             =   1200
            Width           =   3804
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   13
            Left            =   6120
            TabIndex        =   88
            Tag             =   "0"
            Top             =   1245
            Width           =   1005
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   12
            Left            =   6120
            TabIndex        =   84
            Tag             =   "0"
            Top             =   885
            Width           =   1005
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   17
            Left            =   6120
            TabIndex        =   107
            Tag             =   "0"
            Top             =   2925
            Width           =   1005
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   16
            Left            =   6120
            TabIndex        =   102
            Tag             =   "0"
            Top             =   2565
            Width           =   1005
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   15
            Left            =   6120
            TabIndex        =   97
            Tag             =   "0"
            Top             =   2205
            Width           =   1005
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   14
            Left            =   6120
            TabIndex        =   92
            Tag             =   "0"
            Top             =   1725
            Width           =   1005
         End
         Begin VB.Label lbl 
            Caption         =   "Vertical dispersivity - WZ-VDISP"
            Height          =   252
            Index           =   17
            Left            =   120
            TabIndex        =   103
            Tag             =   "WZVDISP"
            Top             =   2880
            Width           =   3804
         End
         Begin VB.Label lbl 
            Caption         =   "Tranverse dispersivity - WZ-TDISP"
            Height          =   252
            Index           =   16
            Left            =   120
            TabIndex        =   98
            Tag             =   "WZTDISP"
            Top             =   2520
            Width           =   3804
         End
         Begin VB.Label lbl 
            Caption         =   "Longitudinal dispersivity - WZ-LDISP"
            Height          =   252
            Index           =   15
            Left            =   120
            TabIndex        =   93
            Tag             =   "WZLDISP"
            Top             =   2160
            Width           =   3804
         End
         Begin VB.Label Label14 
            Caption         =   "Usage location"
            Height          =   252
            Left            =   120
            TabIndex        =   79
            Top             =   360
            Width           =   2532
         End
      End
      Begin Threed.SSFrame SSFrame1 
         Height          =   3372
         Index           =   3
         Left            =   -74760
         TabIndex        =   108
         Top             =   480
         Width           =   7212
         _Version        =   65536
         _ExtentX        =   12721
         _ExtentY        =   5948
         _StockProps     =   14
         ForeColor       =   16776960
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
         Begin VB.CommandButton Command6 
            Caption         =   "<"
            Height          =   230
            Left            =   5310
            TabIndex        =   146
            Top             =   395
            Width           =   250
         End
         Begin VB.CommandButton Command5 
            Caption         =   ">"
            Height          =   230
            Left            =   5555
            TabIndex        =   145
            Top             =   395
            Width           =   230
         End
         Begin VB.ComboBox unit 
            Height          =   315
            Index           =   23
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   136
            Tag             =   "cm"
            Top             =   2880
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   22
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   131
            Tag             =   "cm"
            Top             =   2520
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   21
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   126
            Tag             =   "cm"
            Top             =   2160
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   20
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   121
            Tag             =   "cm"
            Top             =   1680
            Visible         =   0   'False
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   19
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   117
            Tag             =   "cm"
            Top             =   1200
            Visible         =   0   'False
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   18
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   113
            Tag             =   "cm"
            Top             =   840
            Width           =   1000
         End
         Begin VB.ComboBox Combo3 
            Height          =   315
            ItemData        =   "Aquifer.frx":0396
            Left            =   1680
            List            =   "Aquifer.frx":0398
            Style           =   2  'Dropdown List
            TabIndex        =   110
            Tag             =   "LOCATIONS"
            Top             =   360
            Width           =   4332
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   23
            Left            =   4080
            TabIndex        =   135
            Tag             =   "wzfvdisp"
            Top             =   2880
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   22
            Left            =   4080
            TabIndex        =   130
            Tag             =   "wzftdisp"
            Top             =   2520
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   21
            Left            =   4080
            TabIndex        =   125
            Tag             =   "wzfldisp"
            Top             =   2160
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   20
            Left            =   4080
            TabIndex        =   120
            Tag             =   "wzfaqdepth"
            Text            =   "0"
            Top             =   1680
            Visible         =   0   'False
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   19
            Left            =   4080
            TabIndex        =   116
            Tag             =   "wzfydist"
            Text            =   "0"
            Top             =   1200
            Visible         =   0   'False
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   18
            Left            =   4080
            TabIndex        =   112
            Tag             =   "wzfdist"
            Top             =   840
            Width           =   1000
         End
         Begin Threed.SSCommand SSCommand3 
            Height          =   252
            Index           =   1
            Left            =   3120
            TabIndex        =   134
            Top             =   2880
            Width           =   900
            _Version        =   65536
            _ExtentX        =   1588
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "Estimate"
         End
         Begin Threed.SSCommand SSCommand2 
            Height          =   252
            Index           =   1
            Left            =   3120
            TabIndex        =   129
            Top             =   2520
            Width           =   900
            _Version        =   65536
            _ExtentX        =   1588
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "Estimate"
         End
         Begin Threed.SSCommand SSCommand1 
            Height          =   252
            Index           =   1
            Left            =   3120
            TabIndex        =   124
            Top             =   2160
            Width           =   900
            _Version        =   65536
            _ExtentX        =   1588
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "Estimate"
         End
         Begin VB.Label Label31 
            Caption         =   "Usage location"
            Height          =   252
            Left            =   120
            TabIndex        =   109
            Top             =   360
            Width           =   1932
         End
         Begin VB.Label lbl 
            Caption         =   "Longitudinal dispersivity - WZ-LDISP"
            Height          =   252
            Index           =   21
            Left            =   120
            TabIndex        =   123
            Tag             =   "WZLDISP"
            Top             =   2160
            Width           =   3804
         End
         Begin VB.Label lbl 
            Caption         =   "Tranverse dispersivity - WZ-TDISP"
            Height          =   252
            Index           =   22
            Left            =   120
            TabIndex        =   128
            Tag             =   "WZTDISP"
            Top             =   2520
            Width           =   3804
         End
         Begin VB.Label lbl 
            Caption         =   "Vertical dispersivity - WZ-VDISP"
            Height          =   252
            Index           =   23
            Left            =   120
            TabIndex        =   133
            Tag             =   "WZVDISP"
            Top             =   2880
            Width           =   3804
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   23
            Left            =   6120
            TabIndex        =   137
            Tag             =   "0"
            Top             =   2925
            Width           =   1005
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   22
            Left            =   6120
            TabIndex        =   132
            Tag             =   "0"
            Top             =   2565
            Width           =   1005
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   21
            Left            =   6120
            TabIndex        =   127
            Tag             =   "0"
            Top             =   2205
            Width           =   1005
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   20
            Left            =   6120
            TabIndex        =   122
            Tag             =   "0"
            Top             =   1725
            Visible         =   0   'False
            Width           =   1005
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   19
            Left            =   6120
            TabIndex        =   118
            Tag             =   "0"
            Top             =   1245
            Visible         =   0   'False
            Width           =   1005
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   18
            Left            =   6120
            TabIndex        =   114
            Tag             =   "0"
            Top             =   885
            Width           =   1005
         End
         Begin VB.Label Label22 
            Caption         =   "Perpendicular distance from plume center line - WZ-YDIST"
            Height          =   372
            Left            =   120
            TabIndex        =   115
            Top             =   1200
            Visible         =   0   'False
            Width           =   3804
         End
         Begin VB.Label Label20 
            Caption         =   "Vertical distance below groundwater table - WZ-AQDEPTH"
            Height          =   375
            Left            =   120
            TabIndex        =   119
            Top             =   1665
            Visible         =   0   'False
            Width           =   3810
         End
         Begin VB.Label Label15 
            Caption         =   "Longitudinal distance to flux location - WZ-DIST"
            Height          =   252
            Left            =   120
            TabIndex        =   111
            Top             =   840
            Width           =   3804
         End
      End
      Begin Threed.SSFrame SSFrame1 
         Height          =   3784
         Index           =   4
         Left            =   -74758
         TabIndex        =   138
         Top             =   484
         Width           =   7216
         _Version        =   65536
         _ExtentX        =   12728
         _ExtentY        =   6675
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
         Begin VB.CommandButton SSCommand11 
            Caption         =   "<"
            Height          =   230
            Left            =   2880
            TabIndex        =   12
            Top             =   2255
            Width           =   250
         End
         Begin VB.CommandButton SSCommand4 
            Caption         =   "<"
            Height          =   230
            Left            =   2880
            TabIndex        =   1
            Top             =   510
            Width           =   250
         End
         Begin VB.CommandButton Command4 
            Caption         =   "<"
            Height          =   230
            Left            =   6150
            TabIndex        =   4
            Top             =   510
            Width           =   250
         End
         Begin VB.CommandButton Command3 
            Caption         =   ">"
            Height          =   230
            Left            =   6400
            TabIndex        =   5
            Top             =   510
            Width           =   230
         End
         Begin VB.CommandButton Command2 
            Caption         =   "<"
            Height          =   230
            Left            =   6150
            TabIndex        =   15
            Top             =   2255
            Width           =   250
         End
         Begin VB.CommandButton Command1 
            Caption         =   ">"
            Height          =   230
            Left            =   6400
            TabIndex        =   16
            Top             =   2255
            Width           =   230
         End
         Begin VB.CommandButton SSCommand10 
            Caption         =   ">"
            Height          =   230
            Left            =   3135
            TabIndex        =   13
            Top             =   2255
            Width           =   230
         End
         Begin VB.CommandButton SSCommand5 
            Caption         =   ">"
            Height          =   230
            Left            =   3135
            TabIndex        =   2
            Top             =   510
            Width           =   230
         End
         Begin VB.ComboBox ProgKds 
            Height          =   315
            ItemData        =   "Aquifer.frx":039A
            Left            =   360
            List            =   "Aquifer.frx":03A7
            Style           =   2  'Dropdown List
            TabIndex        =   17
            Tag             =   "WASUBKD"
            Top             =   2760
            Visible         =   0   'False
            Width           =   3255
         End
         Begin VB.ComboBox ConKds 
            Height          =   288
            ItemData        =   "Aquifer.frx":040F
            Left            =   360
            List            =   "Aquifer.frx":041C
            Style           =   2  'Dropdown List
            TabIndex        =   6
            Tag             =   "WASUBKD"
            Top             =   960
            Width           =   3255
         End
         Begin VB.ComboBox ProgParms 
            Height          =   288
            ItemData        =   "Aquifer.frx":0484
            Left            =   3960
            List            =   "Aquifer.frx":0491
            Style           =   2  'Dropdown List
            TabIndex        =   14
            Top             =   2220
            Width           =   2892
         End
         Begin VB.ComboBox ConParms 
            Height          =   288
            ItemData        =   "Aquifer.frx":04FC
            Left            =   3960
            List            =   "Aquifer.frx":0509
            Style           =   2  'Dropdown List
            TabIndex        =   3
            Top             =   480
            Width           =   2892
         End
         Begin VB.ComboBox unit 
            Height          =   315
            Index           =   25
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   20
            Tag             =   "ml/g"
            Top             =   2760
            Width           =   1000
         End
         Begin VB.ComboBox cboProgeny 
            Height          =   288
            Left            =   210
            Style           =   2  'Dropdown List
            TabIndex        =   11
            Tag             =   "FSCNAME"
            Top             =   2220
            Width           =   3375
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   304
            Index           =   25
            Left            =   4170
            TabIndex        =   19
            Tag             =   "WASUBKD"
            Top             =   2760
            Width           =   885
         End
         Begin VB.ComboBox unit 
            Height          =   315
            Index           =   24
            ItemData        =   "Aquifer.frx":0574
            Left            =   5040
            List            =   "Aquifer.frx":0576
            Style           =   2  'Dropdown List
            TabIndex        =   9
            Tag             =   "ml/g"
            Top             =   960
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   304
            Index           =   24
            Left            =   4170
            TabIndex        =   8
            Tag             =   "WASUBKD"
            Top             =   960
            Width           =   885
         End
         Begin VB.ComboBox cboParent 
            Height          =   288
            Left            =   210
            Style           =   2  'Dropdown List
            TabIndex        =   0
            Tag             =   "FSCNAME"
            Top             =   480
            Width           =   3375
         End
         Begin Threed.SSCommand SSCommand8 
            Height          =   252
            Left            =   360
            TabIndex        =   18
            Tag             =   "WASUBKD"
            Top             =   3120
            Width           =   3252
            _Version        =   65536
            _ExtentX        =   5736
            _ExtentY        =   444
            _StockProps     =   78
            Caption         =   "Estimate &All"
         End
         Begin Threed.SSCommand SSCommand7 
            Height          =   255
            Left            =   360
            TabIndex        =   7
            Tag             =   "WASUBKD"
            Top             =   1320
            Width           =   3255
            _Version        =   65536
            _ExtentX        =   5741
            _ExtentY        =   450
            _StockProps     =   78
            Caption         =   "Estimate &All"
         End
         Begin VB.Label Label2 
            Caption         =   "Please note: Degradation products move at the same speed as the parent."
            ForeColor       =   &H000000C0&
            Height          =   252
            Left            =   360
            TabIndex        =   149
            Top             =   3480
            Width           =   6492
         End
         Begin VB.Label Label45 
            Caption         =   "Progeny - FS-CNAME"
            Height          =   252
            Left            =   120
            TabIndex        =   141
            Top             =   1980
            Width           =   2172
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   25
            Left            =   6120
            TabIndex        =   21
            Tag             =   "0"
            Top             =   2850
            Width           =   990
         End
         Begin VB.Label Label44 
            Caption         =   "Progeny Parameter Selection"
            Height          =   252
            Left            =   3840
            TabIndex        =   142
            Top             =   1980
            Width           =   3000
         End
         Begin VB.Label Label40 
            Caption         =   "Constituent Parameter Selection"
            Height          =   252
            Left            =   3840
            TabIndex        =   140
            Top             =   240
            Width           =   3000
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   24
            Left            =   6120
            TabIndex        =   10
            Tag             =   "0"
            Top             =   1050
            Width           =   990
         End
         Begin VB.Label Label38 
            Caption         =   "Constituent - FS-CNAME"
            Height          =   252
            Left            =   120
            TabIndex        =   139
            Top             =   240
            Width           =   2172
         End
      End
      Begin Threed.SSFrame SSFrame1 
         Height          =   3492
         Index           =   0
         Left            =   240
         TabIndex        =   23
         Top             =   480
         Width           =   7212
         _Version        =   65536
         _ExtentX        =   12721
         _ExtentY        =   6160
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
         Begin VB.CommandButton cmdEstKd 
            Caption         =   "Soil class - WZ-CLASS"
            Height          =   312
            Left            =   121
            TabIndex        =   24
            Tag             =   "wzclass"
            Top             =   363
            Width           =   6855
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   5
            Left            =   4080
            TabIndex        =   42
            Tag             =   "wziron"
            Top             =   2277
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   4
            Left            =   4080
            TabIndex        =   38
            Tag             =   "wzomc"
            Top             =   1914
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   3
            Left            =   4080
            TabIndex        =   34
            Tag             =   "wzclay"
            Top             =   1562
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   2
            Left            =   4080
            TabIndex        =   30
            Tag             =   "wzsilt"
            Top             =   1199
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   1
            Left            =   4080
            TabIndex        =   26
            Tag             =   "wzsand"
            Top             =   847
            Width           =   1000
         End
         Begin VB.ComboBox Combo1 
            BeginProperty Font 
               Name            =   "Courier"
               Size            =   9.75
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   264
            ItemData        =   "Aquifer.frx":0578
            Left            =   2662
            List            =   "Aquifer.frx":05A0
            Style           =   2  'Dropdown List
            TabIndex        =   143
            Tag             =   "wzclass"
            Top             =   363
            Visible         =   0   'False
            Width           =   4332
         End
         Begin VB.Label Label16 
            Caption         =   "* The percent of sand, silt, clay, organic matter, and iron must add up to 100%"
            Height          =   264
            Left            =   121
            TabIndex        =   45
            Top             =   2860
            Width           =   5863
         End
         Begin VB.Label Label33 
            Caption         =   "%"
            Height          =   264
            Left            =   5148
            TabIndex        =   43
            Top             =   2277
            Width           =   187
         End
         Begin VB.Label Label32 
            Caption         =   "%"
            Height          =   264
            Left            =   5148
            TabIndex        =   39
            Top             =   1914
            Width           =   187
         End
         Begin VB.Label Label28 
            Caption         =   "%"
            Height          =   264
            Left            =   5148
            TabIndex        =   35
            Top             =   1562
            Width           =   187
         End
         Begin VB.Label Label27 
            Caption         =   "%"
            Height          =   264
            Left            =   5148
            TabIndex        =   31
            Top             =   1199
            Width           =   187
         End
         Begin VB.Label Label1 
            Caption         =   "%"
            Height          =   264
            Left            =   5148
            TabIndex        =   27
            Top             =   836
            Width           =   187
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   253
            Index           =   5
            Left            =   5764
            TabIndex        =   44
            Tag             =   "0"
            Top             =   2277
            Width           =   1001
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   253
            Index           =   4
            Left            =   5764
            TabIndex        =   40
            Tag             =   "0"
            Top             =   1914
            Width           =   1001
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   253
            Index           =   3
            Left            =   5764
            TabIndex        =   36
            Tag             =   "0"
            Top             =   1562
            Width           =   1001
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   253
            Index           =   2
            Left            =   5764
            TabIndex        =   32
            Tag             =   "0"
            Top             =   1199
            Width           =   1001
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   253
            Index           =   1
            Left            =   5764
            TabIndex        =   28
            Tag             =   "0"
            Top             =   836
            Width           =   1001
         End
         Begin VB.Label Label7 
            Caption         =   "Percentage of iron and aluminum - WZ-IRON *"
            Height          =   385
            Left            =   121
            TabIndex        =   41
            Top             =   2277
            Width           =   3806
         End
         Begin VB.Label Label6 
            Caption         =   "Percentage of organic matter - WZ-OMC *"
            Height          =   264
            Left            =   121
            TabIndex        =   37
            Top             =   1914
            Width           =   3806
         End
         Begin VB.Label Label5 
            Caption         =   "Percentage of clay - WZ-CLAY *"
            Height          =   264
            Left            =   121
            TabIndex        =   33
            Top             =   1562
            Width           =   3806
         End
         Begin VB.Label Label4 
            Caption         =   "Percentage of silt - WZ-SILT *"
            Height          =   264
            Left            =   121
            TabIndex        =   29
            Top             =   1199
            Width           =   3806
         End
         Begin VB.Label Label3 
            Caption         =   "Percentage of sand - WZ-SAND *"
            Height          =   264
            Left            =   121
            TabIndex        =   25
            Top             =   836
            Width           =   3806
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
   Begin VB.Menu noact 
      Caption         =   "&Reference"
      Begin VB.Menu addref 
         Caption         =   "&Add"
      End
      Begin VB.Menu selref 
         Caption         =   "Se&lect"
      End
   End
   Begin VB.Menu op 
      Caption         =   "&Options"
      Begin VB.Menu auto 
         Caption         =   "A&utofill"
         Checked         =   -1  'True
      End
      Begin VB.Menu advan 
         Caption         =   "A&dvanced"
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
   Begin VB.Menu hlp 
      Caption         =   "&Help"
      Begin VB.Menu howto 
         Caption         =   "How to ..."
      End
      Begin VB.Menu about 
         Caption         =   "&About"
      End
   End
End
Attribute VB_Name = "Aquifer"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Dim temp As parmrec
Dim loadng As Boolean
Dim oldConc As String
Dim oldFlux As String

Dim oldParent As String
Dim oldProgeny As String

Dim uStr(HALF_LIFE) As String
Dim hStr(2) As String

'Added flags to control the behaviour of the added parms combo box
Dim prevSelectedIdx As Long
Dim prevSelectedProgIdx As Long

' All these just ro resolve indexs from fui to mui
Dim f_numcon As Long
Dim m_numflux As Long, flux() As locparm
Dim m_numconc As Long, conc() As locparm
 
Private Sub about_Click()
  frmAbout.Show 1, Me
End Sub

Private Sub advan_Click()
  TimePts.Show 1, Me
End Sub

Private Sub cmdEstKd_Click()
  MousePointer = vbHourglass
  EstimateKd
  MousePointer = vbDefault
End Sub

Private Sub ConParms_GotFocus()
Dim m As String
  m = "Select a constituent properties to edit"
  mes = Space(140 - Len(m)) & m
  RefItem = CONTAM_PARAMS
  noact.Enabled = True
  txt(CONTAM_PARAMS).Tag = hStr(ConParms.ListIndex)
  HelpAnchor = txt(CONTAM_PARAMS).Tag
End Sub

Private Sub ProgParms_GotFocus()
Dim m As String
  m = "Select a constituent properties to edit"
  mes = Space(140 - Len(m)) & m
  RefItem = PROGENY_PARAMS
  noact.Enabled = True
  txt(PROGENY_PARAMS).Tag = hStr(ProgParms.ListIndex)
  HelpAnchor = txt(PROGENY_PARAMS).Tag
End Sub

Private Sub Command1_GotFocus()
  ProgParms_GotFocus
End Sub

Private Sub Command2_GotFocus()
  ProgParms_GotFocus
End Sub

Private Sub Command3_GotFocus()
  ConParms_GotFocus
End Sub

Private Sub Command4_GotFocus()
  ConParms_GotFocus
End Sub

Private Sub Command5_GotFocus()
  Combo3_GotFocus
End Sub

Private Sub Command6_GotFocus()
  Combo3_GotFocus
End Sub

Private Sub Command7_GotFocus()
  Combo2_GotFocus
End Sub

Private Sub Command8_GotFocus()
  Combo2_GotFocus
End Sub

Private Sub ConKds_Click()
  Dim i As Long
  Dim temp As Double
  
  If ConKds.ListIndex > SELECT_LOOKUP Then
    set_unit unit(CONTAM_PARAMS), unit(CONTAM_PARAMS).Tag
    temp = getUserKd()
    i = cboParent.ListIndex
    If i >= 0 Then i = cboParent.ItemData(i)
    If temp < 0 Then
      txt(CONTAM_PARAMS).Text = ""
      If i > 0 Then
        model(i).param(KD_IDX).value = Empty
        model(i).param(KD_IDX).uunt = ""
      End If
    Else
      txt(CONTAM_PARAMS).Text = temp
      If i > 0 Then
        model(i).param(KD_IDX).value = temp
        model(i).param(KD_IDX).uunt = unit(CONTAM_PARAMS).Tag
      End If
    End If
    model(i).param(KD_IDX).ref = ref(CONTAM_PARAMS).Tag
  End If
  
  SSCommand7.Enabled = (ConKds.ListIndex > SELECT_LOOKUP)
  
End Sub

Private Sub Form_Unload(Cancel As Integer)
  Dim answer As Long
  If Not called Then
    answer = MsgBox("Do you want to save changes?", 51, App.Title)
    If answer = 6 Then save_Click
    If answer = 7 Then
      Unload TimePts
      EndModule
    End If
    If answer = 2 Then Cancel = 1
  End If
End Sub

Function getEstKdDesc(kdEstimateType%) As String
  ' Fail safe return value
  getEstKdDesc = ""
  Select Case kdEstimateType
    Case DATABASE_LOOKUP  ' Database value
      getEstKdDesc = "Database Value"
    Case EQUATION_LOOKUP  ' Equation Estimator value
      getEstKdDesc = "Estimated Value"
    Case TABLE_LOOKUP  ' Lookup Table value
      getEstKdDesc = "Lookup Table Value"
  End Select

End Function

Function getKd(conName$, kdEstimateType%, Optional progName$ = "")
  Dim i As Long
  Dim j As Long
  
  ' Fail safe return value
  getKd = -1
  
  i = listIndexOf(cboParent, conName)
  If i >= 0 Then i = cboParent.ItemData(i)
  j = listIndexOf(cboProgeny, progName)
  If j >= 0 Then j = cboProgeny.ItemData(j)
  
  Select Case kdEstimateType
    Case DATABASE_LOOKUP  ' Database value
      getKd = Kd_DatabaseValue(i, j)
    Case EQUATION_LOOKUP  ' Equation Estimator value
      getKd = Kd_EquationValue(i, txt(fOMC).Text, txt(fCLAY).Text, txt(fSILT).Text, txt(fSAND).Text, j)
    Case TABLE_LOOKUP  ' Lookup Table value
      getKd = Kd_LookupTableValue(i, j)
  End Select
End Function

Function getUserKd() As Double
  getUserKd = getKd(cboParent.Text, ConKds.ListIndex)
End Function

Function getProgKd() As Double
  getProgKd = getKd(cboParent.Text, ProgKds.ListIndex, cboProgeny.Text)
End Function

Function getListParmName(ctlList As Control) As String
  ' fail safe return value
  getListParmName = ""
  
  Select Case ctlList.Name
    Case "ConParms", "ProgParms"
      Select Case ctlList.ListIndex
        Case 0
          getListParmName = "WASUBKD"
        Case 1
          If con(cboParent.ItemData(cboParent.ListIndex)).rad Then
            getListParmName = "WZSOL-RAD"
          Else
            getListParmName = "WZSOL"
          End If
        Case 2
          getListParmName = "WZGHALF"
      End Select
  End Select
End Function

Function getUnitType(ParmName As String) As String
  ' fail safe return value
  getUnitType = ""
  
  Select Case ParmName
    Case "WASUBKD"
      getUnitType = "LiquidVolume/Mass"
    Case "WZSOL"
      getUnitType = "Mass/LiquidVolume"
    Case "WZSOL-RAD"
      getUnitType = "Activity/LiquidVolume"
    Case "WZGHALF"
      getUnitType = "Time"
  End Select
  
End Function

Private Sub howto_Click()
  GetHelp
End Sub

Private Sub leave_Click()
  Form_Unload 0
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

Private Sub ProgKds_Click()
  Dim i As Long
  Dim j As Long
  Dim temp As Double
  
  If ProgKds.ListIndex > SELECT_LOOKUP Then
    set_unit unit(PROGENY_PARAMS), unit(PROGENY_PARAMS).Tag
    temp = getProgKd()
    i = cboParent.ListIndex
    If i >= 0 Then i = cboParent.ItemData(i)
    j = cboProgeny.ListIndex
    If j >= 0 Then j = cboProgeny.ItemData(j)
    If temp < 0 Then
      txt(PROGENY_PARAMS).Text = ""
      If j >= 0 Then
        model(i).progeny(j).param(ProgParms.ListIndex).value = Empty
        model(i).progeny(j).param(ProgParms.ListIndex).uunt = ""
      End If
    Else
      txt(PROGENY_PARAMS).Text = temp
      If j >= 0 Then
        model(i).progeny(j).param(ProgParms.ListIndex).value = temp
        model(i).progeny(j).param(ProgParms.ListIndex).uunt = unit(PROGENY_PARAMS).Text
      End If
    End If
  End If

  SSCommand8.Enabled = (ProgKds.ListIndex > SELECT_LOOKUP)

End Sub

Private Sub selref_Click()
  RefMode = 0
  GetRef ref(RefItem)
End Sub

Private Sub addref_Click()
  RefMode = 1
  GetRef ref(RefItem)
End Sub

Private Sub auto_Click()
  auto.Checked = Not auto.Checked
End Sub

Private Sub elocfillet(idx As Long)
  If temp.idx1 > m_numconc Then
    m_numconc = temp.idx1
    ReDim Preserve conc(m_numconc) As locparm
  End If
  conc(temp.idx1).parms(idx).value = convert(temp.cunit, temp.uunit, CDbl(temp.pval))
  conc(temp.idx1).parms(idx).uunt = temp.uunit
  conc(temp.idx1).parms(idx).punt = temp.cunit
  conc(temp.idx1).parms(idx).ref = temp.ref
End Sub

Private Sub rlocfillet(idx As Long)
  If temp.idx1 > m_numflux Then
    m_numflux = temp.idx1
    ReDim Preserve flux(m_numflux) As locparm
  End If
  flux(temp.idx1).parms(idx).value = convert(temp.cunit, temp.uunit, CDbl(temp.pval))
  flux(temp.idx1).parms(idx).uunt = temp.uunit
  flux(temp.idx1).parms(idx).punt = temp.cunit
  flux(temp.idx1).parms(idx).ref = temp.ref
End Sub

Private Sub fillet(idx As Long)
  ref(idx).Tag = temp.ref
  ref(idx).Caption = "Ref:" & Str(temp.ref)
  
  ' BLH 11/97 - for the benefit of 3.1 prm conversion (an import utility)
  ' the units described in the .des file for a parameter that are represented
  ' as a hard coded label (rather than a list control) should not cause the ui to fail
  On Error Resume Next

  If temp.cunit <> "N/A" Then set_unit unit(idx), temp.uunit
  txt(idx).Text = convert(temp.cunit, temp.uunit, Val(temp.pval))
End Sub

Private Sub SizeLocal()
  If temp.idx3 > numloc Then
    numloc = temp.idx3
    ReDim Preserve loc(numloc) As snk
  End If
End Sub

Private Sub fillChem(idx As Long)
  If temp.idx2 > f_numcon Then
    f_numcon = temp.idx2
    ReDim Preserve con(f_numcon) As contam_param
  End If
  If temp.idx3 = 0 Then
    con(temp.idx2).param(idx) = temp.pval
  Else
    If temp.idx3 > con(temp.idx2).numprog Then
      con(temp.idx2).numprog = temp.idx3
      ReDim Preserve con(temp.idx2).progeny(temp.idx3) As contam_progeny_param
    End If
    con(temp.idx2).progeny(temp.idx3).param(idx) = temp.pval
  End If
End Sub

Private Sub loadprm()
  Dim i As Long, j As Long, k As Long, l As Long, m As Long
  Dim gml As Double
  Dim spa As Double
  Dim mx As Long
  Dim prm As String
  Dim prefix As String
  Dim fle As parmfile
  Dim sval As Boolean
  
  numloc = 0
  f_numcon = 0
  m_numflux = 0
  m_numconc = 0
  m_numcon = 0
 
  If open_parm(fle, FUIName, 2) Then
    Do Until EOCF(fle.file)
      If read_parmrec(fle, temp) Then
        Select Case temp.pName
          Case "fui"
            For m = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                If temp.idx1 = siteIdx Then
                  Select Case temp.pName
                    Case "numcon"
                      f_numcon = Val(temp.pval)
                      ReDim Preserve con(f_numcon) As contam_param
                    Case "fscname"
                        If temp.idx2 > f_numcon Then
                          f_numcon = temp.idx2
                          ReDim Preserve con(f_numcon) As contam_param
                        End If
                        If temp.idx3 = 0 Then
                          con(temp.idx2).Name = temp.pval
                        Else
                          If temp.idx3 > con(temp.idx2).numprog Then
                            con(temp.idx2).numprog = temp.idx3
                            ReDim Preserve con(temp.idx2).progeny(temp.idx3) As contam_progeny_param
                          End If
                          con(temp.idx2).progeny(temp.idx3).Name = temp.pval
                        End If
                    Case "fscasid"
                        If temp.idx2 > f_numcon Then
                          f_numcon = temp.idx2
                          ReDim Preserve con(f_numcon) As contam_param
                        End If
                        If temp.idx3 = 0 Then
                          con(temp.idx2).cas = temp.pval
                        Else
                          If temp.idx3 > con(temp.idx2).numprog Then
                            con(temp.idx2).numprog = temp.idx3
                            ReDim Preserve con(temp.idx2).progeny(temp.idx3) As contam_progeny_param
                          End If
                          con(temp.idx2).progeny(temp.idx3).cas = temp.pval
                        End If
                    Case "clktype"
                      If temp.idx2 > f_numcon Then
                        f_numcon = temp.idx2
                        ReDim Preserve con(f_numcon) As contam_param
                      End If
                      If temp.idx3 = 0 Then
                        If temp.pval = 1 Then
                          con(temp.idx2).rad = True
                        Else
                          con(temp.idx2).rad = False
                        End If
                      Else
                        If temp.idx3 > con(temp.idx2).numprog Then
                          con(temp.idx2).numprog = temp.idx3
                          ReDim Preserve con(temp.idx2).progeny(temp.idx3) As contam_progeny_param
                        End If
                      End If
                    Case "clkoc":                        fillChem KOC_IDX
                    Case "clsol":                        fillChem WATER_SOLUBILITY_IDX
                    Case "clghalf":                      fillChem HALF_LIFE_IDX
                    Case "clkd":                         fillChem KD_IDX
                    Case "clwm":                         fillChem MW_IDX
                   End Select
                End If
              End If
            Next
            ' convert mg/L solubilities to pCi/mL
            For i = 1 To f_numcon
              If con(i).rad And Val(con(i).param(WATER_SOLUBILITY_IDX)) <> 0 Then
                gml = convert("mg/l", "g/ml", Val(con(i).param(WATER_SOLUBILITY_IDX)))
                spa = 130600000 / (Val(con(i).param(HALF_LIFE_IDX)) * Val(con(i).param(MW_IDX)))
                'convert from g/ml tp pci/ml
                con(i).param(WATER_SOLUBILITY_IDX) = CStr(gml * spa * 1000000000000#)
                For j = 1 To con(i).numprog
                  gml = convert("mg/l", "g/ml", Val(con(i).progeny(j).param(WATER_SOLUBILITY_IDX)))
                  spa = 130600000 / (Val(con(i).progeny(j).param(HALF_LIFE_IDX)) * Val(con(i).progeny(j).param(MW_IDX)))
                  'convert from g/ml tp pci/ml
                  con(i).progeny(j).param(WATER_SOLUBILITY_IDX) = CStr(gml * spa * 1000000000000#)
                Next
              End If
            Next
          Case modName
            Loading.Gauge1.Max = Val(temp.idx1)
            Loading.Gauge1.value = 0
            For m = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                Loading.update
                Select Case temp.pName
                  Case "CVTFormat":    CVTFormat = "General Number" 'CVTFormat = temp.pval
                  Case "tfinal":  SetTFinal temp
                  Case "ntimes":  SetNTimes temp
                  Case "wzfname"
                    If temp.idx1 > m_numflux Then
                      m_numflux = temp.idx1
                      ReDim Preserve flux(m_numflux) As locparm
                    End If
                    flux(temp.idx1).Name = temp.pval
                    flux(temp.idx1).idx = 0
                  Case "wzfdist":           rlocfillet 0
                  Case "wzfydist":          rlocfillet 1
                  Case "wzfaqdepth":        rlocfillet 2
                  Case "wzfldisp":          rlocfillet 3
                  Case "wzftdisp":          rlocfillet 4
                  Case "wzfvdisp":          rlocfillet 5
                  Case "wzcname"
                    If temp.idx1 > m_numconc Then
                      m_numconc = temp.idx1
                      ReDim Preserve conc(m_numconc) As locparm
                    End If
                    conc(temp.idx1).Name = temp.pval
                    conc(temp.idx1).idx = 0
                  Case "wzcdist":           elocfillet 0
                  Case "wzcydist":          elocfillet 1
                  Case "wzcaqdepth":        elocfillet 2
                  Case "wzcldisp":          elocfillet 3
                  Case "wzctdisp":          elocfillet 4
                  Case "wzcvdisp":          elocfillet 5
                  Case "wzclass":           Combo1.ListIndex = Val(temp.pval)
                  Case "wzfract":
                                            If temp.cunit = "N/A" Then            'backward compatible to old unit
                                              temp.cunit = "percent"
                                              temp.uunit = "percent"
                                            End If
                                            fillet 0
                  Case "wzsand":            fillet 1
                  Case "wzsilt":            fillet 2
                  Case "wzclay":            fillet 3
                  Case "wzomc":             fillet 4
                  Case "wziron":            fillet 5
                  Case "wzph":              fillet 6
                  Case "wztotpor":
                                            If temp.cunit = "N/A" Then            'backward compatible to old unit
                                              temp.cunit = "percent"
                                              temp.uunit = "percent"
                                            End If
                                            fillet 7
                  Case "wzeffpor":
                                            If temp.cunit = "N/A" Then            'backward compatible to old unit
                                              temp.cunit = "percent"
                                              temp.uunit = "percent"
                                            End If
                                            fillet 8
                  Case "wzpveloc":          fillet 9
                  Case "wzthick":           fillet 10
                  Case "wzbulk":            fillet 11
                  Case "wzdiv":             fillet 28
                  Case "aqucasid"
                    If temp.idx1 > m_numcon Then
                      m_numcon = temp.idx1
                      ReDim Preserve model(temp.idx1) As model_contam_param
                    End If
                    If temp.idx2 = 0 Then
                      model(temp.idx1).cas = temp.pval
                    Else
                      If temp.idx2 > model(temp.idx1).numprog Then
                        model(temp.idx1).numprog = temp.idx2
                        ReDim Preserve model(temp.idx1).progeny(temp.idx2) As model_progeny_param
                      End If
                      model(temp.idx1).progeny(temp.idx2).cas = temp.pval
                    End If
                  Case "wasubkd", "wzsol", "wzrsol", "wzghalf"
                     Select Case temp.pName
                        Case "wasubkd": i = KD_IDX
                        Case "wzsol", "wzrsol": i = WATER_SOLUBILITY_IDX
                        Case "wzghalf": i = HALF_LIFE_IDX
                     End Select
                     If temp.idx1 > m_numcon Then
                       m_numcon = temp.idx1
                       ReDim Preserve model(temp.idx1) As model_contam_param
                     End If
                     If temp.idx2 = 0 Then
                       model(temp.idx1).idx = 0
                       model(temp.idx1).param(i).value = Empty
                       If temp.pval <> "EMPTY" Then model(temp.idx1).param(i).value = convert(temp.cunit, temp.uunit, Val(temp.pval))
                       model(temp.idx1).param(i).punt = temp.cunit
                       model(temp.idx1).param(i).uunt = temp.uunit
                       model(temp.idx1).param(i).ref = temp.ref
                     Else
                       If temp.idx2 > model(temp.idx1).numprog Then
                         model(temp.idx1).numprog = temp.idx2
                         ReDim Preserve model(temp.idx1).progeny(temp.idx2) As model_progeny_param
                       End If
                       model(temp.idx1).progeny(temp.idx2).idx = 0
                       model(temp.idx1).progeny(temp.idx2).param(i).value = Empty
                       If temp.pval <> "EMPTY" Then model(temp.idx1).progeny(temp.idx2).param(i).value = convert(temp.cunit, temp.uunit, Val(temp.pval))
                       model(temp.idx1).progeny(temp.idx2).param(i).punt = temp.cunit
                       model(temp.idx1).progeny(temp.idx2).param(i).uunt = temp.uunit
                       model(temp.idx1).progeny(temp.idx2).param(i).ref = temp.ref
                    End If
                End Select
              End If
            Next
          Case "csm"
            mx = -1
            For i = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                'assumes modname will always occur before sink variables
                If temp.pName = "modid" And temp.pval = modName Then
                  mx = temp.idx2
                End If
                If mx > -1 And temp.idx2 = mx And temp.idx1 = siteIdx Then
                  Select Case temp.pName
                    Case "moddespath"
                      DesName = temp.pval
                    Case "modsinkid"
                      SizeLocal
                      loc(temp.idx3).Name = temp.pval
                    Case "modsinklabel"
                      SizeLocal
                      loc(temp.idx3).lbl = temp.pval
                    Case "modsinktype"
                      SizeLocal
                      loc(temp.idx3).type = temp.pval
                    Case "modsinkqual"
                      SizeLocal
                      loc(temp.idx3).qual = temp.pval
                  End Select
                End If
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

'resolve contaminant differences if contaminant no longer exists its index is 0
    For i = 1 To f_numcon
      For j = 1 To m_numcon
        If model(j).cas = con(i).cas Then
          model(j).idx = i
          If model(j).param(KD_IDX).value = "" Or model(j).param(KD_IDX).value = Empty Then
            model(j).param(KD_IDX).value = con(i).param(KD_IDX)
            model(j).param(KD_IDX).punt = uStr(SOIL_KD)
            model(j).param(KD_IDX).uunt = uStr(SOIL_KD)
            model(j).param(KD_IDX).ref = 0
          End If
          If model(j).param(WATER_SOLUBILITY_IDX).value = "" Or model(j).param(WATER_SOLUBILITY_IDX).value = Empty Then
            model(j).param(WATER_SOLUBILITY_IDX).value = con(i).param(WATER_SOLUBILITY_IDX)
            If con(i).rad Then
              model(j).param(WATER_SOLUBILITY_IDX).punt = uStr(RAD_SOLUBILITY)
              model(j).param(WATER_SOLUBILITY_IDX).uunt = uStr(RAD_SOLUBILITY)
            Else
              model(j).param(WATER_SOLUBILITY_IDX).punt = uStr(SOLUBILITY)
              model(j).param(WATER_SOLUBILITY_IDX).uunt = uStr(SOLUBILITY)
            End If
            model(j).param(WATER_SOLUBILITY_IDX).ref = 0
          End If
          If model(j).param(HALF_LIFE_IDX).value = "" Or model(j).param(HALF_LIFE_IDX).value = Empty Then
            model(j).param(HALF_LIFE_IDX).value = con(i).param(HALF_LIFE_IDX)
            model(j).param(HALF_LIFE_IDX).punt = uStr(HALF_LIFE)
            model(j).param(HALF_LIFE_IDX).uunt = uStr(HALF_LIFE)
            model(j).param(HALF_LIFE_IDX).ref = 0
          End If
          Exit For
        End If
      Next j
      If j > m_numcon Then
        m_numcon = j
        ReDim Preserve model(j) As model_contam_param
        model(j).cas = con(i).cas
        model(j).idx = i
        model(j).param(KD_IDX).value = Empty
        model(j).param(KD_IDX).uunt = uStr(SOIL_KD)
        model(j).param(KD_IDX).ref = 0
        model(j).param(WATER_SOLUBILITY_IDX).value = con(i).param(WATER_SOLUBILITY_IDX)
        If con(i).rad Then
          model(j).param(WATER_SOLUBILITY_IDX).punt = uStr(RAD_SOLUBILITY)
          model(j).param(WATER_SOLUBILITY_IDX).uunt = uStr(RAD_SOLUBILITY)
        Else
          model(j).param(WATER_SOLUBILITY_IDX).punt = uStr(SOLUBILITY)
          model(j).param(WATER_SOLUBILITY_IDX).uunt = uStr(SOLUBILITY)
        End If
        model(j).param(WATER_SOLUBILITY_IDX).ref = 0
        model(j).param(HALF_LIFE_IDX).value = con(i).param(HALF_LIFE_IDX)
        model(j).param(HALF_LIFE_IDX).punt = uStr(HALF_LIFE)
        model(j).param(HALF_LIFE_IDX).uunt = uStr(HALF_LIFE)
        model(j).param(HALF_LIFE_IDX).ref = 0
      End If
    Next i
    For i = 1 To m_numcon
      If model(i).idx > 0 Then
        cboParent.AddItem con(model(i).idx).Name
        cboParent.ItemData(cboParent.NewIndex) = i
      End If
    Next i
  
  'resolve progeny differences if progeny no longer exists its index is 0
    For i = 1 To m_numcon
      If model(i).idx <> 0 Then
        For j = 1 To con(model(i).idx).numprog
          For k = 1 To model(i).numprog
            If model(i).progeny(k).cas = con(model(i).idx).progeny(j).cas Then
              model(i).progeny(k).idx = j
              If model(i).progeny(k).param(KD_IDX).value = "" Or model(i).progeny(k).param(KD_IDX).value = Empty Then
                model(i).progeny(k).param(KD_IDX).value = con(model(i).idx).progeny(j).param(KD_IDX)
                model(i).progeny(k).param(KD_IDX).uunt = uStr(SOIL_KD)
              End If
              If model(i).progeny(k).param(WATER_SOLUBILITY_IDX).value = "" Or model(i).progeny(k).param(WATER_SOLUBILITY_IDX).value = Empty Then
                model(i).progeny(k).param(WATER_SOLUBILITY_IDX).value = con(model(i).idx).progeny(j).param(WATER_SOLUBILITY_IDX)
                If con(model(i).idx).rad Then
                  model(i).progeny(k).param(WATER_SOLUBILITY_IDX).uunt = uStr(RAD_SOLUBILITY)
                Else
                  model(i).progeny(k).param(WATER_SOLUBILITY_IDX).uunt = uStr(SOLUBILITY)
                End If
              End If
              If model(i).progeny(k).param(HALF_LIFE_IDX).value = "" Or model(i).progeny(k).param(HALF_LIFE_IDX).value = Empty Then
                model(i).progeny(k).param(HALF_LIFE_IDX).value = con(model(i).idx).progeny(j).param(HALF_LIFE_IDX)
                model(i).progeny(k).param(HALF_LIFE_IDX).uunt = uStr(HALF_LIFE)
                model(i).progeny(k).param(HALF_LIFE_IDX).ref = 0
              End If
              Exit For
            End If
          Next k
          If k > model(i).numprog Then
            model(i).numprog = k
            ReDim Preserve model(i).progeny(k) As model_progeny_param
            model(i).progeny(k).cas = con(model(i).idx).progeny(j).cas
            model(i).progeny(k).idx = j
            model(i).progeny(k).param(KD_IDX).value = con(model(i).idx).progeny(j).param(KD_IDX)
            model(i).progeny(k).param(KD_IDX).uunt = uStr(SOIL_KD)
            model(i).progeny(k).param(KD_IDX).ref = 0
            model(i).progeny(k).param(WATER_SOLUBILITY_IDX).value = con(model(i).idx).progeny(j).param(WATER_SOLUBILITY_IDX)
            If con(model(i).idx).rad Then
              model(i).progeny(k).param(WATER_SOLUBILITY_IDX).uunt = uStr(RAD_SOLUBILITY)
            Else
              model(i).progeny(k).param(WATER_SOLUBILITY_IDX).uunt = uStr(SOLUBILITY)
            End If
            model(i).progeny(k).param(WATER_SOLUBILITY_IDX).ref = 0
            model(i).progeny(k).param(HALF_LIFE_IDX).value = con(model(i).idx).progeny(j).param(HALF_LIFE_IDX)
            model(i).progeny(k).param(HALF_LIFE_IDX).uunt = uStr(HALF_LIFE)
            model(i).progeny(k).param(HALF_LIFE_IDX).ref = 0
          End If
        Next j
      End If
    Next i
    
'resolve flux differences if flux no longer exists its index is 0
    For i = 1 To numloc
      If InStr(loc(i).Name, "sen") <> 1 And InStr(loc(i).Name, "cpt") <> 1 Then
        For j = 1 To m_numflux
          If flux(j).Name = loc(i).Name And InStr(loc(i).type, "wff") Mod 4 = 1 Then
            flux(j).idx = i
            flux(j).label = loc(i).lbl
            Exit For
          End If
        Next
        If j > m_numflux And InStr(loc(i).type, "wff") Mod 4 = 1 Then
          m_numflux = m_numflux + 1
          ReDim Preserve flux(m_numflux) As locparm
          flux(m_numflux).idx = i
          flux(m_numflux).Name = loc(i).Name
          flux(m_numflux).label = loc(i).lbl
          For k = 0 To 5
            flux(m_numflux).parms(k).uunt = "cm"
            flux(m_numflux).parms(k).punt = "cm"
            flux(m_numflux).parms(k).ref = 0
          Next
        End If
      End If
    Next
    
'resolve exp differences if exp no longer exists its index is 0
    For i = 1 To numloc
      If InStr(loc(i).Name, "sen") <> 1 And InStr(loc(i).Name, "cpt") <> 1 Then
        For j = 1 To m_numconc
          If conc(j).Name = loc(i).Name And InStr(loc(i).type, "wcf") Mod 4 = 1 Then
            conc(j).idx = i
            conc(j).label = loc(i).lbl
            Exit For
          End If
        Next
        If j > m_numconc And InStr(loc(i).type, "wcf") Mod 4 = 1 Then
          m_numconc = m_numconc + 1
          ReDim Preserve conc(m_numconc) As locparm
          conc(m_numconc).idx = i
          conc(m_numconc).Name = loc(i).Name
          conc(m_numconc).label = loc(i).lbl
          For k = 0 To 5
            conc(m_numconc).parms(k).uunt = "cm"
            conc(m_numconc).parms(k).punt = "cm"
            conc(m_numconc).parms(k).ref = 0
          Next
        End If
      End If
    Next
    
    
    For i = 1 To m_numconc
      If conc(i).idx > 0 Then Combo2.AddItem conc(i).label + " (" + conc(i).Name + ")"
    Next
    For i = 1 To m_numflux
      If flux(i).idx > 0 Then Combo3.AddItem flux(i).label + " (" + flux(i).Name + ")"
    Next
    If Combo2.ListCount = 0 Then
      SSTab1.TabVisible(2) = False
    Else
      Combo2.ListIndex = 0
    End If
    If Combo3.ListCount = 0 Then
      SSTab1.TabVisible(3) = False
    Else
      Combo3.ListIndex = 0
    End If
    If numloc = 0 Then PutError "Must be connect to wff:Aquifer, wcf:Aquifer consuming icon!"
  Else
    PutError "Can't find or open file " & FUIName
    EndModule
  End If
End Sub

Private Sub Form_load()
  Dim i As Long
  
  uStr(SOIL_KD) = "ml/g"
  uStr(RAD_SOLUBILITY) = "pCi/ml"
  uStr(SOLUBILITY) = "mg/l"
  uStr(HALF_LIFE) = "day"
  
  hStr(0) = "WASUBKD"
  hStr(1) = "WZSOL"
  hStr(2) = "WZGHALF"
  
  StartModule Me, App.Title, 5
  SetHelpFile App.Path + "\aqu.htm"
  SetRefFile ReplaceExt(FUIName, "ref")
  load_kd
  'set conversion comboboxes
  For i = 7 To 25
    get_conversion_items unit(i).Tag, unit(i)
  Next
  get_conversion_items unit(0).Tag, unit(0)
  SetTimeTitle Me.Caption
  Loading.Show
  loadng = True
  loadprm
  BackFillParent
  prevSelectedIdx = 0
  prevSelectedProgIdx = 0
  ConParms.ListIndex = 0
  ProgParms.ListIndex = 0
  loadng = False
  Unload Loading
  Refresh
  vfProcessUserInput = True
End Sub

Private Sub save_Click()
  Dim i As Long, j As Long, cnt As Long
  Dim fname As String
  Dim parm As parmrec
  Dim fle As parmfile
  Dim UserFormat As String
  UserFormat = CVTFormat

  txt_LostFocus RefItem
  fname = RunName & ".GID"
  If open_parm(fle, fname, 1) Then
    set_parm parm, "CVTFormat", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CVTFormat
    write_parmrec fle, parm
    CVTFormat = "General Number"
    WriteTime fle
    For i = 0 To 11
      If er(i) Then PutError "Parameter " & txt(i).Tag & " is invalid"
    Next
    set_parm parm, "wzclass", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", Combo1.ListIndex
    write_parmrec fle, parm
    For i = 0 To 11
      If i = 0 Or i > 6 Then
        set_parm parm, txt(i).Tag, 0, 0, 0, 0, 0, 0, ref(i).Tag, unit(i).Text, unit(i).Tag, convert(unit(i).Text, unit(i).Tag, Val(txt(i)))
        write_parmrec fle, parm
      Else
        set_parm parm, txt(i).Tag, 0, 0, 0, 0, 0, 0, ref(i).Tag, "N/A", "N/A", txt(i).Text
        write_parmrec fle, parm
      End If
    Next
    set_parm parm, txt(28).Tag, 0, 0, 0, 0, 0, 0, ref(28).Tag, "N/A", "N/A", txt(28).Text
    write_parmrec fle, parm
    'All river points
    cnt = 0
    For i = 1 To m_numflux
      If flux(i).idx > 0 Then
        Combo3.ListIndex = cnt
        cnt = cnt + 1
        set_parm parm, "wzfname", cnt, 0, 0, 0, 0, 0, 0, "N/A", "N/A", flux(i).Name
        write_parmrec fle, parm
        For j = 0 To 5
          If j = 1 Then j = 3
          If er(18 + j) Then PutError "Parameter " & txt(18 + j).Tag & " for " & flux(i).Name & " is invalid"
          set_parm parm, txt(18 + j).Tag, cnt, 0, 0, 0, 0, 0, flux(i).parms(j).ref, flux(i).parms(j).uunt, flux(i).parms(0).punt, convert(flux(i).parms(j).uunt, flux(i).parms(j).punt, Val(flux(i).parms(j).value))
          write_parmrec fle, parm
        Next
      End If
    Next
    set_parm parm, "wzfnum", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", Str(cnt)
    write_parmrec fle, parm
   'All exposure points
    cnt = 0
    For i = 1 To m_numconc
      If conc(i).idx > 0 Then
        Combo2.ListIndex = cnt
        cnt = cnt + 1
        set_parm parm, "wzcname", cnt, 0, 0, 0, 0, 0, 0, "N/A", "N/A", conc(i).Name
        write_parmrec fle, parm
        For j = 0 To 5
          If er(12 + j) Then PutError "Parameter " & txt(12 + j).Tag & " for " & conc(i).Name & " is invalid"
          set_parm parm, txt(12 + j).Tag, cnt, 0, 0, 0, 0, 0, conc(i).parms(j).ref, conc(i).parms(j).uunt, conc(i).parms(j).punt, convert(conc(i).parms(j).uunt, conc(i).parms(j).punt, Val(conc(i).parms(j).value))
          write_parmrec fle, parm
        Next
      End If
    Next
    set_parm parm, "wzcnum", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", Str(cnt)
    write_parmrec fle, parm
    
    'Contaminate kd's
    For i = 1 To m_numcon
      With model(i)
        If .idx <> 0 Then
          set_parm parm, "aqucasid", .idx, 0, 0, 0, 0, 0, .param(KD_IDX).ref, "N/A", "N/A", .cas
          write_parmrec fle, parm
          If .param(KD_IDX).value = Empty Then
            PutError "Invalid adsorption coefficient (WASUBKD) value for " & .cas & " on Constituent Properties tab"
            set_parm parm, "wasubkd", .idx, 0, 0, 0, 0, 0, .param(KD_IDX).ref, .param(KD_IDX).uunt, uStr(SOIL_KD), "EMPTY"
          Else
            set_parm parm, "wasubkd", .idx, 0, 0, 0, 0, 0, .param(KD_IDX).ref, .param(KD_IDX).uunt, uStr(SOIL_KD), convert(.param(KD_IDX).uunt, uStr(SOIL_KD), Val(.param(KD_IDX).value))
          End If
          write_parmrec fle, parm
          
          If Len(.param(WATER_SOLUBILITY_IDX).value) = 0 Then
            If con(.idx).rad Then
              PutError "Missing RAD water solubility (wzrsol) value for " & .cas & " on Constituent Properties tab"
            Else
              PutError "Missing water solubility (wzsol) value for " & .cas & " on Constituent Properties tab"
            End If
          Else
            If .param(WATER_SOLUBILITY_IDX).value = Empty Or .param(WATER_SOLUBILITY_IDX).value < 0 Then
              If con(.idx).rad Then
                PutError "Invalid RAD water solubility (wzrsol) value for " & model(i).cas & " on Constituent Properties tab"
                set_parm parm, "wzrsol", .idx, 0, 0, 0, 0, 0, .param(WATER_SOLUBILITY_IDX).ref, .param(WATER_SOLUBILITY_IDX).uunt, uStr(RAD_SOLUBILITY), "EMPTY"
              Else
                PutError "Invalid water solubility (wzsol) value for " & model(i).cas & " on Constituent Properties tab"
                set_parm parm, "wzsol", .idx, 0, 0, 0, 0, 0, .param(WATER_SOLUBILITY_IDX).ref, .param(WATER_SOLUBILITY_IDX).uunt, uStr(SOLUBILITY), "EMPTY"
              End If
            Else
              If con(.idx).rad Then
                set_parm parm, "wzrsol", .idx, 0, 0, 0, 0, 0, .param(WATER_SOLUBILITY_IDX).ref, .param(WATER_SOLUBILITY_IDX).uunt, uStr(RAD_SOLUBILITY), convert(.param(WATER_SOLUBILITY_IDX).uunt, uStr(RAD_SOLUBILITY), Val(.param(WATER_SOLUBILITY_IDX).value))
              Else
                set_parm parm, "wzsol", .idx, 0, 0, 0, 0, 0, .param(WATER_SOLUBILITY_IDX).ref, .param(WATER_SOLUBILITY_IDX).uunt, uStr(SOLUBILITY), convert(.param(WATER_SOLUBILITY_IDX).uunt, uStr(SOLUBILITY), Val(.param(WATER_SOLUBILITY_IDX).value))
              End If
            End If
            write_parmrec fle, parm
          End If
          
          If Len(.param(HALF_LIFE_IDX).value) = 0 Then
            PutError "Missing half-life (wzghalf) value for " & .cas & " on Constituent Properties tab"
          Else
            If .param(HALF_LIFE_IDX).value = Empty Or Val(.param(HALF_LIFE_IDX).value) <= 0 Then
              PutError "Invalid half-life (wzghalf) value for " & .cas & " on Constituent Properties tab"
              set_parm parm, "wzghalf", .idx, 0, 0, 0, 0, 0, .param(HALF_LIFE_IDX).ref, .param(HALF_LIFE_IDX).uunt, uStr(HALF_LIFE), "EMPTY"
            Else
              set_parm parm, "wzghalf", .idx, 0, 0, 0, 0, 0, .param(HALF_LIFE_IDX).ref, .param(HALF_LIFE_IDX).uunt, uStr(HALF_LIFE), convert(.param(HALF_LIFE_IDX).uunt, uStr(HALF_LIFE), Val(.param(HALF_LIFE_IDX).value))
            End If
            write_parmrec fle, parm
          End If
          
          For j = 1 To .numprog
            With .progeny(j)
              If .idx <> 0 Then
                set_parm parm, "aqucasid", model(i).idx, .idx, 0, 0, 0, 0, .param(KD_IDX).ref, "N/A", "N/A", .cas
                write_parmrec fle, parm
                
                If .param(KD_IDX).value = Empty Then
                  PutError "Invalid adsorption coefficient (WASUBKD) value for " & .cas & " daughter of " & model(i).cas & " on Constituent Properties tab"
                  set_parm parm, "wasubkd", model(i).idx, .idx, 0, 0, 0, 0, .param(KD_IDX).ref, .param(KD_IDX).uunt, uStr(SOIL_KD), "EMPTY"
                Else
                  set_parm parm, "wasubkd", model(i).idx, .idx, 0, 0, 0, 0, .param(KD_IDX).ref, .param(KD_IDX).uunt, uStr(SOIL_KD), convert(.param(KD_IDX).uunt, uStr(SOIL_KD), Val(.param(KD_IDX).value))
                End If
                write_parmrec fle, parm
                
                If Len(.param(WATER_SOLUBILITY_IDX).value) = 0 Then
                  If con(model(i).idx).rad Then
                    PutError "Missing RAD water solubility (wzrsol) value for " & .cas & " daughter of " & model(i).cas & " on Constituent Properties tab"
                  Else
                    PutError "Missing water solubility (wzsol) value for " & .cas & " daughter of " & model(i).cas & " on Constituent Properties tab"
                  End If
                Else
                  If .param(WATER_SOLUBILITY_IDX).value = Empty Or Val(.param(WATER_SOLUBILITY_IDX).value) < 0 Then
                    If con(model(i).idx).rad Then
                      PutError "Invalid wRAD ater solubility (wzrsol) value for " & .cas & " daughter of " & model(i).cas & " on Constituent Properties tab"
                      set_parm parm, "wzrsol", model(i).idx, .idx, 0, 0, 0, 0, .param(WATER_SOLUBILITY_IDX).ref, .param(WATER_SOLUBILITY_IDX).uunt, uStr(RAD_SOLUBILITY), "EMPTY"
                    Else
                      PutError "Invalid water solubility (wzsol) value for " & .cas & " daughter of " & model(i).cas & " on Constituent Properties tab"
                      set_parm parm, "wzsol", model(i).idx, .idx, 0, 0, 0, 0, .param(WATER_SOLUBILITY_IDX).ref, .param(WATER_SOLUBILITY_IDX).uunt, uStr(SOLUBILITY), "EMPTY"
                    End If
                  Else
                    If con(model(i).idx).rad Then
                      set_parm parm, "wzrsol", model(i).idx, .idx, 0, 0, 0, 0, .param(WATER_SOLUBILITY_IDX).ref, .param(WATER_SOLUBILITY_IDX).uunt, uStr(RAD_SOLUBILITY), convert(.param(WATER_SOLUBILITY_IDX).uunt, uStr(RAD_SOLUBILITY), Val(.param(WATER_SOLUBILITY_IDX).value))
                    Else
                      set_parm parm, "wzsol", model(i).idx, .idx, 0, 0, 0, 0, .param(WATER_SOLUBILITY_IDX).ref, .param(WATER_SOLUBILITY_IDX).uunt, uStr(SOLUBILITY), convert(.param(WATER_SOLUBILITY_IDX).uunt, uStr(SOLUBILITY), Val(.param(WATER_SOLUBILITY_IDX).value))
                    End If
                  End If
                  write_parmrec fle, parm
                End If
                
                If Len(.param(HALF_LIFE_IDX).value) = 0 Then
                  PutError "Missing half-life (wzghalf) value for " & .cas & " daughter of " & model(i).cas & " on Constituent Properties tab"
                Else
                  If .param(HALF_LIFE_IDX).value = Empty Or Val(.param(HALF_LIFE_IDX).value) <= 0 Then
                    PutError "Invalid half-life (wzghalf) value for " & .cas & " daughter of " & model(i).cas & " on Constituent Properties tab"
                    set_parm parm, "wzghalf", model(i).idx, .idx, 0, 0, 0, 0, .param(HALF_LIFE_IDX).ref, .param(HALF_LIFE_IDX).uunt, uStr(HALF_LIFE), "EMPTY"
                  Else
                    set_parm parm, "wzghalf", model(i).idx, .idx, 0, 0, 0, 0, .param(HALF_LIFE_IDX).ref, .param(HALF_LIFE_IDX).uunt, uStr(HALF_LIFE), convert(.param(HALF_LIFE_IDX).uunt, uStr(HALF_LIFE), Val(.param(HALF_LIFE_IDX).value))
                  End If
                  write_parmrec fle, parm
                End If
                
              End If
            End With    ' .progeny(j)
          Next j
        End If
      End With  ' model(i)
    Next i
    close_parm fle
    CVTFormat = UserFormat
    SetFormat Me
  Else
    PutError "Unable to create transaction file" & RunName & ".GID"
  End If
  Unload TimePts
  EndModule
End Sub

Private Sub SSCommand1_Click(Index As Integer)
  If Val(txt(12 + Index * 6)) > 0 Then
     set_unit unit(15 + Index * 6), unit(12 + Index * 6).Text
     txt(15 + Index * 6).Text = 0.1 * Val(txt(12 + Index * 6))
     If Index = 0 Then
       Combo2_Click
     Else
       Combo3_Click
     End If
  Else
    MsgBox "Need to have a valid travel distance", vbOKOnly + vbExclamation + vbApplicationModal, "Input Error!"
  End If
End Sub

Private Sub SSCommand2_Click(Index As Integer)
  If Val(txt(15 + Index * 6)) > 0 Then
     set_unit unit(16 + Index * 6), unit(15 + Index * 6).Text
     txt(16 + Index * 6).Text = 0.33 * Val(txt(15 + Index * 6))
     If Index = 0 Then
       Combo2_Click
     Else
       Combo3_Click
     End If
  Else
    MsgBox "Need to have a valid Longitudinal Dispersivity", vbOKOnly + vbExclamation + vbApplicationModal, "Input Error!"
  End If
End Sub

Private Sub SSCommand3_Click(Index As Integer)
  If Val(txt(15 + Index * 6)) > 0 Then
     set_unit unit(17 + Index * 6), unit(15 + Index * 6).Text
     txt(17 + Index * 6).Text = 0.0025 * Val(txt(15 + Index * 6))
     If Index = 0 Then
       Combo2_Click
     Else
       Combo3_Click
     End If
  Else
    MsgBox "Need to have a valid Longitudinal Dispersivity", vbOKOnly + vbExclamation + vbApplicationModal, "Input Error!"
  End If
End Sub

Private Sub SSCommand4_Click()
  If cboParent.ListIndex > 0 Then cboParent.ListIndex = cboParent.ListIndex - 1
End Sub

Private Sub SSCommand5_Click()
  If cboParent.ListIndex < cboParent.ListCount - 1 Then cboParent.ListIndex = cboParent.ListIndex + 1
End Sub

Private Sub SSCommand7_Click()
  Dim i As Long
  Dim kdee As Double
  
  For i = 1 To m_numcon
    kdee = getKd(con(model(i).idx).Name, ConKds.ListIndex)
    If kdee < 0 Then
      MsgBox "The " & getEstKdDesc(ConKds.ListIndex) & " is not valid for " & con(model(i).idx).Name & " at this time", vbInformation, "Estimate Kd"
    Else
      model(i).param(ConParms.ListIndex).value = kdee
      model(i).param(ConParms.ListIndex).uunt = unit(CONTAM_PARAMS).Tag
      model(i).param(ConParms.ListIndex).ref = ref(CONTAM_PARAMS).Tag
    End If
  Next
  ConKds_Click
End Sub

Private Sub SSCommand8_Click()
  Dim i As Long
  Dim j As Long
  Dim kdee As Double
  
  If ProgKds.ListIndex > SELECT_LOOKUP Then
    i = cboParent.ListIndex
    If i >= 0 Then i = cboParent.ItemData(i)
    If i > 0 Then
      With model(i)
        For j = 1 To .numprog
          kdee = getKd(con(.idx).Name, ProgKds.ListIndex, con(.idx).progeny(.progeny(j).idx).Name)
          With .progeny(j)
            If kdee < 0 Then
              MsgBox "The " & getEstKdDesc(ProgKds.ListIndex) & " is not valid for " & con(model(i).idx).progeny(.idx).Name & " daughter product of " & con(model(i).idx).Name & " at this time", vbInformation, "Estimate Kd"
            Else
              .param(ProgParms.ListIndex).value = kdee
              .param(ProgParms.ListIndex).uunt = unit(PROGENY_PARAMS).Tag
              .param(ProgParms.ListIndex).ref = Val(ref(PROGENY_PARAMS).Tag)
            End If
          End With  ' .progeny(j)
        Next j
      End With  ' model(i)
    End If
    ProgKds_Click
  End If
End Sub

Private Sub SSCommand10_Click()
  If cboProgeny.ListIndex < cboProgeny.ListCount - 1 Then cboProgeny.ListIndex = cboProgeny.ListIndex + 1
End Sub

Private Sub SSCommand11_Click()
  If cboProgeny.ListIndex > 0 Then cboProgeny.ListIndex = cboProgeny.ListIndex - 1
End Sub

Private Sub Command4_Click()
  If ConParms.ListIndex > 0 Then ConParms.ListIndex = ConParms.ListIndex - 1
End Sub

Private Sub Command3_Click()
  If ConParms.ListIndex < ConParms.ListCount - 1 Then ConParms.ListIndex = ConParms.ListIndex + 1
End Sub

Private Sub Command2_Click()
  If ProgParms.ListIndex > 0 Then ProgParms.ListIndex = ProgParms.ListIndex - 1
End Sub

Private Sub Command1_Click()
  If ProgParms.ListIndex < ProgParms.ListCount - 1 Then ProgParms.ListIndex = ProgParms.ListIndex + 1
End Sub

Private Sub Command5_Click()
  If Combo3.ListIndex < Combo3.ListCount - 1 Then Combo3.ListIndex = Combo3.ListIndex + 1
End Sub

Private Sub Command6_Click()
  If Combo3.ListIndex > 0 Then Combo3.ListIndex = Combo3.ListIndex - 1
End Sub

Private Sub Command7_Click()
  If Combo2.ListIndex < Combo2.ListCount - 1 Then Combo2.ListIndex = Combo2.ListIndex + 1
End Sub

Private Sub Command8_Click()
  If Combo2.ListIndex > 0 Then Combo2.ListIndex = Combo2.ListIndex - 1
End Sub

Private Sub Timer1_Timer()
  mes = Mid(mes, 2) & Mid(mes, 1, 1)
End Sub

Private Sub unit_Click(Index As Integer)
  er CLng(Index)
End Sub

Private Sub SSTab1_Click(PreviousTab As Integer)
  
  SSFrame1(PreviousTab).Enabled = False
  SSFrame1(SSTab1.Tab).Enabled = True
  noact.Enabled = False
  
  If Kdn_Index(False) < 0 Then
    MsgBox "The percentages entered for sand, silt, clay, organic matter, and iron must add up to 100 percent", 48, "Range Error!"
    SSTab1.Tab = 0
    txt(fSAND).SetFocus
    Exit Sub
  ElseIf Kdn_Index(True) < 0 Then
    MsgBox "PH value needed for KD estimator.", 48, "Range Error!"
    SSTab1.Tab = 1
    txt(fPH).SetFocus
    Exit Sub
  End If
  
  If SSTab1.Tab >= 2 And cboParent.ListIndex = -1 Then SSCommand5_Click
  mes = ""
End Sub

Private Function er(Index As Long) As Boolean
  Dim tval As Double
  Dim t1 As String
  Dim t2 As String
  Dim m As String
  
  m = ""
  er = False
  If txt(Index).Text = "" Then er = True
  tval = Val(txt(Index).Text)
  Select Case Index
    Case 0
      t1 = convert(unit(Index).Tag, unit(Index).Text, 0.01)
      t2 = convert(unit(Index).Tag, unit(Index).Text, 100)
      m = "Value must be greater than " + t1 + " and less than or equal to " + t2
      If (tval <= Val(t1) Or tval > Val(t2)) Then er = True
    Case 1 To 5
      m = "Value must be between 0 and 100 inclusive"
      If (tval < 0 Or tval > 100) Then er = True
    Case 28
      m = "Value must be between 1 and 45 inclusive"
      If (tval < 1 Or tval > 46) Then er = True
    Case 6
      m = "Value must be between 1 and 14 inclusive"
      If (tval < 1 Or tval > 14) Then er = True
    Case 7
      t1 = convert(unit(Index).Tag, unit(Index).Text, 0.1)
      t2 = convert(unit(Index).Tag, unit(Index).Text, 99.9)
      m = "Value must be between " + t1 + " and " + t2 + " " + unit(Index).Text + " inclusive"
      If (tval < Val(t1) Or tval > Val(t2)) Then er = True
    Case 8
      t1 = convert(unit(Index).Tag, unit(Index).Text, 0.1)
      t2 = convert(unit(7).Text, unit(Index).Text, Val(txt(7).Text))
      m = "Value must be between " + t1 + " and " + t2 + " " + unit(Index).Text + " inclusive"
      If (tval < Val(t1) Or tval > Val(t2)) Then er = True
    Case 10
      er 14
      t1 = convert(unit(Index).Tag, unit(Index).Text, 3)
      t2 = convert(unit(Index).Tag, unit(Index).Text, 30500)
      m = "Value must be between " + t1 + " and " + t2 + " " + unit(Index).Text
      If (tval < Val(t1) Or tval > Val(t2)) Then er = True
    Case 11
      t1 = convert(unit(Index).Tag, unit(Index).Text, 1)
      t2 = convert(unit(Index).Tag, unit(Index).Text, 3#)
      m = "Value must be between " + t1 + " and " + t2 + " " + unit(Index).Text
      If (tval < Val(t1) Or tval >= Val(t2)) Then er = True
    Case 9, 12, 18
      If Index = 12 Then er 15
      If Index = 18 Then er 21
      m = "Value must be greater than 0"
      If (tval <= 0) Then er = True
    Case 15, 21
      If Index = 15 Then
        t1 = convert(unit(12).Text, unit(Index).Text, Val(txt(12)))
      Else
        t1 = convert(unit(18).Text, unit(Index).Text, Val(txt(18)))
      End If
      m = "Value must be between 0 and " + t1 + " " + unit(Index).Text
      If (tval < 0 Or tval > Val(t1)) Then er = True
    Case 14, 20
      t1 = convert(unit(10).Text, unit(Index).Text, Val(txt(10)))
      m = "Value must be between 0 and " + t1 + " " + unit(Index).Text
      If (tval < 0 Or tval > Val(t1)) Then er = True
    Case CONTAM_PARAMS
      If ConParms.ListIndex = 2 Then
        m = "Value must be greater than 0"
        If tval <= 0 Then er = True
      Else
        m = "Value must be greater than or equal to 0"
        If tval < 0 Then er = True
      End If
    Case PROGENY_PARAMS
      If ProgParms.ListIndex = 2 Then
        m = "Value must be greater than 0"
        If tval <= 0 Then er = True
      Else
        m = "Value must be greater than or equal to 0"
        If tval < 0 Then er = True
      End If
    Case 13, 16, 17, 19, 22, 23
      m = "Value must be greater than or equal to 0"
      If (tval < 0) Then er = True
  End Select
  mes = Space(140 - Len(m)) & m
  If er Then
    txt(Index).BackColor = &H8080FF
  Else
    txt(Index).BackColor = &HC0FFC0
  End If
End Function

Private Sub txt_Change(Index As Integer)
Dim chk As Double
On Error GoTo toolarge
  er CLng(Index)
  chk = CDbl(txt(Index).Text)
  If vfProcessUserInput And Index <= fPH Then
    KdListUpdate
    ProgenyKdListUpdate
  End If
  Exit Sub
toolarge:
  txt(Index).BackColor = &H8080FF
End Sub

Private Sub changetxt(i As Long, j As Long, k As Long, f1 As Double, f2 As Double, f3 As Double, f4 As Double, f5 As Double)
  If Not loadng Then
    vfProcessUserInput = False
    txt(fSAND).Text = i
    txt(fSILT).Text = j
    txt(fCLAY).Text = k
    txt(fOMC).Text = 0
    txt(fIRON).Text = 0
    If auto.Checked Then
      unit(11).Text = unit(11).Tag
      txt(7).Text = f2
      txt(11).Text = f3

'  cannot use field capcity for effective porosity estimate
'  as per John McDonalds instruction
'     txt(8).Text = f4
    End If
    vfProcessUserInput = True
  End If
End Sub

Private Sub fluxchange(tx As String, oldtx As String)
  Dim i As Long, j As Long, k As Long, l As Long
  k = 18
  For i = 1 To m_numflux
    If flux(i).label + " (" + flux(i).Name + ")" = oldtx Then
      For j = 0 To 5
         flux(i).parms(j).value = txt(j + k).Text
         flux(i).parms(j).uunt = unit(j + k).Text
         flux(i).parms(j).punt = unit(j + k).Tag
         flux(i).parms(j).ref = ref(j + k).Tag
      Next
      Exit For
    End If
  Next
  For i = 1 To m_numflux
    If flux(i).label + " (" + flux(i).Name + ")" = tx Then
      For j = 0 To 5
         set_unit unit(j + k), flux(i).parms(j).uunt
         txt(j + k).Text = flux(i).parms(j).value
         ref(j + k).Caption = "Ref: " & flux(i).parms(j).ref
         ref(j + k).Tag = flux(i).parms(j).ref
      Next
      Exit For
    End If
  Next
End Sub

Private Sub concchange(tx As String, oldtx As String)
  Dim i As Long, j As Long, k As Long, l As Long
  k = 12
  For i = 1 To m_numconc
    If conc(i).label + " (" + conc(i).Name + ")" = oldtx Then
      For j = 0 To 5
         conc(i).parms(j).value = txt(j + k).Text
         conc(i).parms(j).uunt = unit(j + k).Text
         conc(i).parms(j).punt = unit(j + k).Tag
         conc(i).parms(j).ref = ref(j + k).Tag
      Next
      Exit For
    End If
  Next
  For i = 1 To m_numconc
    If conc(i).label + " (" + conc(i).Name + ")" = tx Then
      For j = 0 To 5
        set_unit unit(j + k), conc(i).parms(j).uunt
        txt(j + k).Text = conc(i).parms(j).value
        ref(j + k).Caption = "Ref: " & conc(i).parms(j).ref
        ref(j + k).Tag = conc(i).parms(j).ref
      Next
      Exit For
    End If
  Next
End Sub


'<<<<<<<<<<<<<<<<<<<<<<<<
'click events for all controls
'>>>>>>>>>>>>>>>>>>>>>>>>

Private Sub Combo1_Click()
  Select Case Combo1.ListIndex
   Case 0:    changetxt 92, 5, 3, 0.0066, 38, 1.64, 9#, 4.05
   Case 1:    changetxt 83, 11, 6, 0.0019, 43.7, 1.49, 12#, 4.38
   Case 2:    changetxt 65, 25, 10, 0.00072, 44.2, 1.48, 17.5, 4.9
   Case 3:    changetxt 42, 38, 20, 0.00037, 46.6, 1.42, 23.5, 5.39
   Case 4:    changetxt 20, 65, 15, 0.0002, 46.3, 1.42, 27.5, 5.3
   Case 5:    changetxt 7, 88, 5, 0.00013, 44.2, 1.48, 28#, 5.3
   Case 6:    changetxt 60, 14, 26, 0.00011, 39.8, 1.6, 24#, 7.12
   Case 7:    changetxt 32, 35, 33, 0.000062, 47.7, 1.39, 34#, 8.52
   Case 8:    changetxt 10, 57, 33, 0.000046, 49#, 1.35, 37.5, 7.75
   Case 9:    changetxt 52, 7, 41, 0.000034, 43#, 1.51, 32#, 10.4
   Case 10:   changetxt 7, 46, 47, 0.000026, 48.6, 1.36, 42#, 10.4
   Case 11:   changetxt 20, 20, 60, 0.000019, 47.5, 1.39, 40#, 11.4
  End Select
  
  If vfProcessUserInput Then
    KdListUpdate
    ProgenyKdListUpdate
  End If
End Sub

Private Sub Combo2_Click()
  concchange Combo2.Text, oldConc
  oldConc = Combo2.Text
End Sub

Private Sub Combo3_Click()
  fluxchange Combo3.Text, oldFlux
  oldFlux = Combo3.Text
End Sub

Private Sub cboParent_Click()
  Dim i As Long
  Dim j As Long
  
  GetModelParent
  GetModelProgeny
  PutModelParent
  oldParent = cboParent.Text
  cboProgeny.Clear
  
  i = cboParent.ListIndex
  If i >= 0 Then i = cboParent.ItemData(i)
  If i > 0 Then
    If con(model(i).idx).numprog = 0 Then
      txt(PROGENY_PARAMS).Visible = False
      unit(PROGENY_PARAMS).Visible = False
      ref(PROGENY_PARAMS).Visible = False
      Command2.Visible = False
      Command1.Visible = False
      SSCommand10.Visible = False
      SSCommand11.Visible = False
      SSCommand8.Visible = False
      Label44.Visible = False
      Label45.Visible = False
      cboProgeny.Visible = False
      ProgParms.Visible = False
      ProgKds.Visible = False
    Else
      For j = 1 To model(i).numprog
        If model(i).progeny(j).idx > 0 Then
          cboProgeny.AddItem con(model(i).idx).progeny(model(i).progeny(j).idx).Name
          cboProgeny.ItemData(cboProgeny.NewIndex) = j
        End If
      Next
      txt(PROGENY_PARAMS).Visible = True
      unit(PROGENY_PARAMS).Visible = True
      ref(PROGENY_PARAMS).Visible = True
      Command2.Visible = True
      Command1.Visible = True
      SSCommand10.Visible = True
      SSCommand11.Visible = True
      SSCommand8.Visible = (ProgParms.ListIndex = 0)
      SSCommand8.Enabled = (ProgKds.ListIndex > SELECT_LOOKUP)
      Label44.Visible = True
      Label45.Visible = True
      cboProgeny.Visible = True
      oldProgeny = ""
      cboProgeny.ListIndex = 0
      ProgParms.Visible = True
      ProgKds.Visible = (ProgParms.ListIndex = 0)
    End If
  End If
End Sub

Private Sub cboProgeny_Click()
  Dim check As Boolean
  
  GetModelProgeny
  PutModelProgeny
  oldProgeny = cboProgeny.Text
End Sub

Private Sub ConParms_Click()
  Dim check As Boolean
  
  If loadng Then Exit Sub
  cboParent_Click
  prevSelectedIdx = ConParms.ListIndex
  
  check = (ConParms.ListIndex = 0)
  ConKds.Visible = check
  SSCommand7.Visible = check
  SSCommand7.Enabled = (ConKds.ListIndex > SELECT_LOOKUP)
  txt(CONTAM_PARAMS).Tag = hStr(ConParms.ListIndex)
  HelpAnchor = txt(CONTAM_PARAMS).Tag
End Sub

Private Sub ProgParms_Click()
  Dim check As Boolean
  
  If loadng Then Exit Sub
  cboProgeny_Click
  prevSelectedProgIdx = ProgParms.ListIndex
  
  check = (ProgParms.ListIndex = 0)
  ProgKds.Visible = check
  Label2.Visible = check
  SSCommand8.Visible = check
  SSCommand8.Enabled = (ProgKds.ListIndex > SELECT_LOOKUP)
  txt(PROGENY_PARAMS).Tag = hStr(ProgParms.ListIndex)
  HelpAnchor = txt(PROGENY_PARAMS).Tag
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
          For p = 0 To 4
            model(i).param(p).value = model(m).progeny(n).param(p).value
            model(i).param(p).uunt = model(m).progeny(n).param(p).uunt
            model(i).param(p).ref = model(m).progeny(n).param(p).ref
          Next
        End If
        For j = 1 To model(i).numprog
          If model(i).progeny(j).cas = model(m).progeny(n).cas Then
            For p = 0 To 4
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

Private Sub GetModelParent()
  Dim i As Long
  
  i = listIndexOf(cboParent, oldParent)
  If i >= 0 Then i = cboParent.ItemData(i)
  If i > 0 Then
    With model(i).param(prevSelectedIdx)
      If txt(CONTAM_PARAMS).Text = "" Then
        .value = Empty
      Else
        .value = txt(CONTAM_PARAMS).Text
      End If
      .uunt = unit(CONTAM_PARAMS).Text
      .ref = Val(ref(CONTAM_PARAMS).Tag)
      'fill product params
      FillParent model(i).cas, prevSelectedIdx, .value, .uunt, .ref
    End With
  End If
End Sub

Private Sub GetModelProgeny()
  Dim i As Long
  Dim j As Long
  
  i = listIndexOf(cboParent, oldParent)
  If i >= 0 Then i = cboParent.ItemData(i)
  j = listIndexOf(cboProgeny, oldProgeny)
  If j >= 0 Then j = cboProgeny.ItemData(j)
  If i > 0 And j >= 0 Then
    With model(i).progeny(j).param(prevSelectedProgIdx)
      If txt(PROGENY_PARAMS).Text = "" Then
        .value = Empty
      Else
        .value = txt(PROGENY_PARAMS).Text
      End If
      .uunt = unit(PROGENY_PARAMS).Text
      .ref = Val(ref(PROGENY_PARAMS).Tag)
      'fill parent params
      FillParent model(i).progeny(j).cas, prevSelectedProgIdx, .value, .uunt, .ref
    End With
  End If
End Sub

Private Sub PutModelParent()
  Dim i As Long
  
  i = cboParent.ListIndex
  If i >= 0 Then i = cboParent.ItemData(i)
  If i >= 0 Then
    With model(i).param(ConParms.ListIndex)
      unit(CONTAM_PARAMS).Clear
      If Len(.uunt) > 0 Then
        get_conversion_items .uunt, unit(CONTAM_PARAMS)
        set_unit unit(CONTAM_PARAMS), .uunt
      Else
        get_conversion_items_by_type getUnitType(getListParmName(ConParms)), unit(CONTAM_PARAMS)
      End If
      
      If Len(.value) = 0 Then
        txt(CONTAM_PARAMS).Text = ""
      Else
        txt(CONTAM_PARAMS).Text = .value
      End If
      ref(CONTAM_PARAMS).Caption = "Ref: " & .ref
      ref(CONTAM_PARAMS).Tag = .ref
    End With
    
    Select Case ConParms.ListIndex
      Case KD_IDX:                KdListUpdate
      Case HALF_LIFE_IDX:
      Case Else
    End Select
    txt(CONTAM_PARAMS).Enabled = True
    unit(CONTAM_PARAMS).Enabled = True
  End If
End Sub

Private Sub PutModelProgeny()
  Dim i As Long
  Dim j As Long
  
  i = cboParent.ListIndex
  If i >= 0 Then i = cboParent.ItemData(i)
  j = cboProgeny.ListIndex
  If j >= 0 Then j = cboProgeny.ItemData(j)
  If j >= 0 Then
    unit(PROGENY_PARAMS).Clear
    With model(i).progeny(j).param(ProgParms.ListIndex)
      If Len(.uunt) > 0 Then
        get_conversion_items .uunt, unit(PROGENY_PARAMS)
        set_unit unit(PROGENY_PARAMS), .uunt
      Else
        get_conversion_items_by_type getUnitType(getListParmName(ProgParms)), unit(PROGENY_PARAMS)
      End If
      
      If Len(.value) = 0 Then
        txt(PROGENY_PARAMS).Text = ""
      Else
        txt(PROGENY_PARAMS).Text = .value
      End If
      ref(PROGENY_PARAMS).Caption = "Ref: " & .ref
      ref(PROGENY_PARAMS).Tag = .ref
    End With
    
    Select Case ProgParms.ListIndex
      Case KD_IDX
        ProgenyKdListUpdate
        SSCommand8.Enabled = (ProgKds.ListIndex > SELECT_LOOKUP)
      Case HALF_LIFE_IDX
      Case Else
    End Select
    txt(PROGENY_PARAMS).Enabled = True
    unit(PROGENY_PARAMS).Enabled = True
 End If
End Sub

Public Sub KdListUpdate()
  Dim i As Long
  Dim temp As Double
  
  If ConParms.ListIndex = 0 Then
    ConKds.Clear
    ConKds.AddItem "== Select Adsorption Coefficient Value =="
    i = cboParent.ListIndex
    If i >= 0 Then i = cboParent.ItemData(i)
    If i >= 0 Then
      temp = Kd_DatabaseValue(i)
      With model(i).param(KD_IDX)
        If temp < 0 Then
          ConKds.AddItem "N/A - " & getEstKdDesc(DATABASE_LOOKUP)
        Else
          ConKds.AddItem temp & " " & .uunt & " - " & getEstKdDesc(DATABASE_LOOKUP)
        End If
        
        temp = Kd_EquationValue(i, txt(fOMC).Text, txt(fCLAY).Text, txt(fSILT).Text, txt(fSAND).Text)
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
    End If
    ConKds.ListIndex = SELECT_LOOKUP
    SSCommand7.Enabled = False
  End If
End Sub

Public Sub ProgenyKdListUpdate()
  Dim i As Long
  Dim j As Long
  Dim temp As Double
  
  If ProgParms.ListIndex = 0 Then
    ProgKds.Clear
    ProgKds.AddItem "== Select Adsorption Coefficient Value =="
    i = cboParent.ListIndex
    If i >= 0 Then i = cboParent.ItemData(i)
    j = cboProgeny.ListIndex
    If j >= 0 Then j = cboProgeny.ItemData(j)
    If j >= 0 Then
      With model(i).progeny(j).param(KD_IDX)
        temp = Kd_DatabaseValue(i, j)
        If temp < 0 Then
          ProgKds.AddItem "N/A - " & getEstKdDesc(DATABASE_LOOKUP)
        Else
          ProgKds.AddItem temp & " " & .uunt & " - " & getEstKdDesc(DATABASE_LOOKUP)
        End If
        
        temp = Kd_EquationValue(i, txt(fOMC).Text, txt(fCLAY).Text, txt(fSILT).Text, txt(fSAND).Text, j)
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
      SSCommand8.Enabled = False
    End If
  End If
End Sub

'<<<<<<<<<<<<<<<<<<<<<<<<
'got focus events for all controls
'>>>>>>>>>>>>>>>>>>>>>>>>

Private Sub txt_LostFocus(Index As Integer)
  Select Case Index
    Case 12 To 17:     Combo2_Click
    Case 18 To 23:     Combo3_Click
    Case CONTAM_PARAMS:            cboParent_Click
    Case PROGENY_PARAMS:           cboProgeny_Click
  End Select
End Sub

Private Sub txt_GotFocus(Index As Integer)
  er CLng(Index)
  RefItem = Index
  noact.Enabled = True
  Select Case Index
  Case 15, 16, 17, 21, 22, 23:
    HelpAnchor = lbl(Index).Tag
  Case Else:
    HelpAnchor = txt(Index).Tag
  End Select
End Sub

Private Sub unit_LostFocus(Index As Integer)
  Select Case Index
    Case 12 To 17:     Combo2_Click
    Case 18 To 23:     Combo3_Click
    Case CONTAM_PARAMS:            cboParent_Click
    Case PROGENY_PARAMS:           cboProgeny_Click
  End Select
End Sub

Private Sub unit_GotFocus(Index As Integer)
  mes = ""
  RefItem = Index
  noact.Enabled = True
  Select Case Index
  Case 15, 16, 17, 21, 22, 23:
    HelpAnchor = lbl(Index).Tag
  Case Else:
    HelpAnchor = txt(Index).Tag
  End Select
End Sub

Private Sub SSTab1_GotFocus()
  mes = ""
  noact.Enabled = False
  Select Case SSTab1.Tab
  Case 0: HelpAnchor = "COMPOSITION"
  Case 1: HelpAnchor = "CHARACTERISTICS"
  Case 2, 3: HelpAnchor = "LOCATIONS"
  Case 4: HelpAnchor = "PROPERTIES"
  End Select
End Sub

Private Sub Combo1_GotFocus()
Dim m As String
  m = "The percent of sand, silt, clay, organic matter and iron must add up to 100%"
  mes = Space(140 - Len(m)) & m
  noact.Enabled = False
  HelpAnchor = Combo1.Tag
End Sub

Private Sub Combo2_GotFocus()
Dim m As String
  m = "Select a location"
  mes = Space(140 - Len(m)) & m
  noact.Enabled = False
  HelpAnchor = Combo2.Tag
End Sub

Private Sub Combo3_GotFocus()
Dim m As String
  m = "Select a location"
  mes = Space(140 - Len(m)) & m
  noact.Enabled = False
  HelpAnchor = Combo3.Tag
End Sub

Private Sub cboParent_GotFocus()
Dim m As String
  m = "Select a constituent"
  mes = Space(140 - Len(m)) & m
  RefItem = CONTAM_PARAMS
  noact.Enabled = True
  HelpAnchor = cboParent.Tag
End Sub

Private Sub cboProgeny_GotFocus()
Dim m As String
  m = "Select a progeny"
  mes = Space(140 - Len(m)) & m
  RefItem = PROGENY_PARAMS
  noact.Enabled = False
  HelpAnchor = cboProgeny.Tag
End Sub

Private Sub SSCommand1_GotFocus(Index As Integer)
  mes = ""
  noact.Enabled = False
  HelpAnchor = "WZLDISP"
End Sub

Private Sub SSCommand2_GotFocus(Index As Integer)
  mes = ""
  noact.Enabled = False
  HelpAnchor = "WZTDISP"
End Sub

Private Sub SSCommand3_GotFocus(Index As Integer)
  mes = ""
  noact.Enabled = False
  HelpAnchor = "WZVDISP"
End Sub

Private Sub SSCommand4_GotFocus()
  cboParent_GotFocus
End Sub

Private Sub SSCommand5_GotFocus()
  cboParent_GotFocus
End Sub

Private Sub SSCommand7_GotFocus()
  cboParent_GotFocus
End Sub

Private Sub SSCommand8_GotFocus()
  cboProgeny_GotFocus
End Sub

Private Sub SSCommand10_GotFocus()
  cboProgeny_GotFocus
End Sub

Private Sub SSCommand11_GotFocus()
  cboProgeny_GotFocus
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


