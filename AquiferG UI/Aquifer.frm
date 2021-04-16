VERSION 5.00
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "tabctl32.ocx"
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "threed32.ocx"
Begin VB.Form Aquifer 
   BorderStyle     =   1  'Fixed Single
   ClientHeight    =   4536
   ClientLeft      =   1656
   ClientTop       =   2616
   ClientWidth     =   7680
   Icon            =   "Aquifer.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   NegotiateMenus  =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   378
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   640
   StartUpPosition =   2  'CenterScreen
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
      TabIndex        =   116
      TabStop         =   0   'False
      Top             =   4200
      Width           =   7680
   End
   Begin TabDlg.SSTab SSTab1 
      Height          =   4212
      Left            =   0
      TabIndex        =   57
      TabStop         =   0   'False
      Top             =   0
      Width           =   7692
      _ExtentX        =   13568
      _ExtentY        =   7430
      _Version        =   393216
      Style           =   1
      Tabs            =   5
      TabsPerRow      =   5
      TabHeight       =   529
      TabCaption(0)   =   "Soil Composition"
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
      Tab(1).ControlCount=   1
      TabCaption(2)   =   "Concentration Locations"
      TabPicture(2)   =   "Aquifer.frx":0342
      Tab(2).ControlEnabled=   0   'False
      Tab(2).Control(0)=   "SSFrame1(2)"
      Tab(2).ControlCount=   1
      TabCaption(3)   =   "Flux Locations"
      TabPicture(3)   =   "Aquifer.frx":035E
      Tab(3).ControlEnabled=   0   'False
      Tab(3).Control(0)=   "SSFrame1(3)"
      Tab(3).ControlCount=   1
      TabCaption(4)   =   "Constituent Parameters"
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
         TabIndex        =   58
         Top             =   480
         Width           =   7212
         _Version        =   65536
         _ExtentX        =   12721
         _ExtentY        =   6160
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
            Height          =   288
            Index           =   8
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   145
            Tag             =   "percent"
            Top             =   1440
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   7
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   144
            Tag             =   "percent"
            Top             =   1080
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   0
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   143
            Tag             =   "percent"
            Top             =   360
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   28
            Left            =   4080
            TabIndex        =   10
            Tag             =   "wzdiv"
            Text            =   "1"
            Top             =   2880
            Visible         =   0   'False
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   0
            Left            =   4080
            TabIndex        =   0
            Tag             =   "wzfract"
            Top             =   360
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   11
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   9
            Tag             =   "g/cm^3"
            Top             =   2520
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   315
            Index           =   10
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   7
            Tag             =   "cm"
            Top             =   2160
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   9
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   5
            Tag             =   "cm/day"
            Top             =   1800
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   11
            Left            =   4080
            TabIndex        =   8
            Tag             =   "wzbulk"
            Top             =   2520
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   10
            Left            =   4080
            TabIndex        =   6
            Tag             =   "wzthick"
            Top             =   2160
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   9
            Left            =   4080
            TabIndex        =   4
            Tag             =   "wzpveloc"
            Top             =   1800
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   8
            Left            =   4080
            TabIndex        =   3
            Tag             =   "wzeffpor"
            Top             =   1440
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   7
            Left            =   4080
            TabIndex        =   2
            Tag             =   "wztotpor"
            Top             =   1080
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   6
            Left            =   4080
            TabIndex        =   1
            Tag             =   "wzph"
            Top             =   720
            Width           =   1000
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   28
            Left            =   6120
            TabIndex        =   115
            Tag             =   "0"
            Top             =   2880
            Visible         =   0   'False
            Width           =   996
         End
         Begin VB.Label Label46 
            Caption         =   "Number of subdivisions in the aquifer"
            Height          =   252
            Left            =   240
            TabIndex        =   114
            Top             =   2880
            Visible         =   0   'False
            Width           =   3800
         End
         Begin VB.Label Label35 
            Caption         =   "pH"
            Height          =   252
            Left            =   5160
            TabIndex        =   101
            Top             =   720
            Width           =   372
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   0
            Left            =   6120
            TabIndex        =   86
            Tag             =   "0"
            Top             =   360
            Width           =   996
         End
         Begin VB.Label Label21 
            Caption         =   "Percent constituent flux entering aquifer - WZ-FRACT"
            Height          =   252
            Left            =   240
            TabIndex        =   85
            Top             =   360
            Width           =   3804
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   11
            Left            =   6120
            TabIndex        =   75
            Tag             =   "0"
            Top             =   2520
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   10
            Left            =   6120
            TabIndex        =   74
            Tag             =   "0"
            Top             =   2160
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   9
            Left            =   6120
            TabIndex        =   73
            Tag             =   "0"
            Top             =   1800
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   8
            Left            =   6120
            TabIndex        =   72
            Tag             =   "0"
            Top             =   1440
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   7
            Left            =   6120
            TabIndex        =   71
            Tag             =   "0"
            Top             =   1080
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   6
            Left            =   6120
            TabIndex        =   70
            Tag             =   "0"
            Top             =   720
            Width           =   996
         End
         Begin VB.Label Label13 
            Caption         =   "Dry bulk density - WZ-BULKD"
            Height          =   252
            Left            =   240
            TabIndex        =   64
            Top             =   2520
            Width           =   3800
         End
         Begin VB.Label Label12 
            Caption         =   "Thickness of aquifer - WZ-THICK"
            Height          =   252
            Left            =   240
            TabIndex        =   63
            Top             =   2160
            Width           =   3800
         End
         Begin VB.Label Label11 
            Caption         =   "Darcy velocity - WZ-PVELOC"
            Height          =   264
            Left            =   240
            TabIndex        =   62
            Top             =   1800
            Width           =   3800
         End
         Begin VB.Label Label10 
            Caption         =   "Effective porosity - WZ-EFFPOR"
            Height          =   264
            Left            =   240
            TabIndex        =   61
            Top             =   1440
            Width           =   3800
         End
         Begin VB.Label Label9 
            Caption         =   "Total porosity - WZ-TOTPOR"
            Height          =   264
            Left            =   240
            TabIndex        =   60
            Top             =   1080
            Width           =   3800
         End
         Begin VB.Label Label8 
            Caption         =   "pH of the pore water - WZ-PH"
            Height          =   264
            Left            =   240
            TabIndex        =   59
            Top             =   720
            Width           =   3800
         End
      End
      Begin Threed.SSFrame SSFrame1 
         Height          =   3492
         Index           =   2
         Left            =   -74760
         TabIndex        =   65
         Top             =   480
         Width           =   7212
         _Version        =   65536
         _ExtentX        =   12721
         _ExtentY        =   6160
         _StockProps     =   14
         ForeColor       =   16776960
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
         Begin VB.ComboBox Combo2 
            Height          =   288
            Left            =   1680
            Style           =   2  'Dropdown List
            TabIndex        =   11
            Tag             =   "LOCATIONS"
            Top             =   360
            Width           =   4332
         End
         Begin VB.ComboBox unit 
            Height          =   315
            Index           =   13
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   15
            Tag             =   "cm"
            Top             =   1200
            Visible         =   0   'False
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   315
            Index           =   12
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   13
            Tag             =   "cm"
            Top             =   840
            Visible         =   0   'False
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   13
            Left            =   4080
            TabIndex        =   14
            Tag             =   "wzcydist"
            Top             =   1200
            Visible         =   0   'False
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   17
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   23
            Tag             =   "cm"
            Top             =   2880
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   16
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   21
            Tag             =   "cm"
            Top             =   2520
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   15
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   19
            Tag             =   "cm"
            Top             =   2160
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   14
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   17
            Tag             =   "cm"
            Top             =   1680
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   17
            Left            =   4080
            TabIndex        =   22
            Tag             =   "wzcvdisp"
            Top             =   2880
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   16
            Left            =   4080
            TabIndex        =   20
            Tag             =   "wzctdisp"
            Top             =   2520
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   15
            Left            =   4080
            TabIndex        =   18
            Tag             =   "wzcldisp"
            Top             =   2160
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   14
            Left            =   4080
            TabIndex        =   16
            Tag             =   "wzcaqdepth"
            Top             =   1680
            Width           =   1000
         End
         Begin Threed.SSCommand SSCommand3 
            Height          =   252
            Index           =   0
            Left            =   3120
            TabIndex        =   26
            Top             =   2880
            Visible         =   0   'False
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
            TabIndex        =   25
            Top             =   2520
            Visible         =   0   'False
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
            TabIndex        =   24
            Top             =   2160
            Visible         =   0   'False
            Width           =   900
            _Version        =   65536
            _ExtentX        =   1588
            _ExtentY        =   445
            _StockProps     =   78
            Caption         =   "Estimate"
         End
         Begin VB.TextBox txt 
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   12
            Left            =   4080
            TabIndex        =   12
            Tag             =   "wzcdist"
            Top             =   840
            Visible         =   0   'False
            Width           =   1000
         End
         Begin VB.Label Label26 
            Caption         =   "Longitudinal distance to well - WZ-DIST"
            Height          =   252
            Left            =   120
            TabIndex        =   84
            Top             =   840
            Visible         =   0   'False
            Width           =   3804
         End
         Begin VB.Label Label25 
            Caption         =   "Vertical distance below water table to well intake - WZ-AQDEPTH"
            Height          =   492
            Left            =   120
            TabIndex        =   83
            Top             =   1680
            Width           =   3804
         End
         Begin VB.Label Label19 
            Caption         =   "Perpendicular distance from plume center line to well - WZ-YDIST"
            Height          =   372
            Left            =   120
            TabIndex        =   82
            Top             =   1200
            Visible         =   0   'False
            Width           =   3804
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   13
            Left            =   6120
            TabIndex        =   81
            Tag             =   "0"
            Top             =   1200
            Visible         =   0   'False
            Width           =   1000
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   12
            Left            =   6120
            TabIndex        =   80
            Tag             =   "0"
            Top             =   840
            Visible         =   0   'False
            Width           =   1000
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   17
            Left            =   6120
            TabIndex        =   79
            Tag             =   "0"
            Top             =   2880
            Width           =   1000
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   16
            Left            =   6120
            TabIndex        =   78
            Tag             =   "0"
            Top             =   2520
            Width           =   1000
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   15
            Left            =   6120
            TabIndex        =   77
            Tag             =   "0"
            Top             =   2160
            Width           =   1000
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   14
            Left            =   6120
            TabIndex        =   76
            Tag             =   "0"
            Top             =   1680
            Width           =   1000
         End
         Begin VB.Label lbl 
            Caption         =   "Vertical dispersivity - WZ-VDISP"
            Height          =   252
            Index           =   17
            Left            =   120
            TabIndex        =   69
            Tag             =   "WZVDISP"
            Top             =   2880
            Width           =   3804
         End
         Begin VB.Label lbl 
            Caption         =   "Tranverse dispersivity - WZ-TDISP"
            Height          =   252
            Index           =   16
            Left            =   120
            TabIndex        =   68
            Tag             =   "WZTDISP"
            Top             =   2520
            Width           =   3804
         End
         Begin VB.Label lbl 
            Caption         =   "Longitudinal dispersivity - WZ-LDISP"
            Height          =   252
            Index           =   15
            Left            =   120
            TabIndex        =   67
            Tag             =   "WZLDISP"
            Top             =   2160
            Width           =   3804
         End
         Begin VB.Label Label14 
            Caption         =   "Usage location"
            Height          =   252
            Left            =   120
            TabIndex        =   66
            Top             =   360
            Width           =   2532
         End
      End
      Begin Threed.SSFrame SSFrame1 
         Height          =   3372
         Index           =   3
         Left            =   -74760
         TabIndex        =   87
         Top             =   480
         Width           =   7212
         _Version        =   65536
         _ExtentX        =   12721
         _ExtentY        =   5948
         _StockProps     =   14
         ForeColor       =   16776960
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
            Height          =   288
            Index           =   23
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   39
            Tag             =   "cm"
            Top             =   2880
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   22
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   37
            Tag             =   "cm"
            Top             =   2520
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   21
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   35
            Tag             =   "cm"
            Top             =   2160
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   20
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   33
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
            TabIndex        =   31
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
            TabIndex        =   29
            Tag             =   "cm"
            Top             =   840
            Width           =   1000
         End
         Begin VB.ComboBox Combo3 
            Height          =   288
            ItemData        =   "Aquifer.frx":0396
            Left            =   1680
            List            =   "Aquifer.frx":0398
            Style           =   2  'Dropdown List
            TabIndex        =   27
            Tag             =   "LOCATIONS"
            Top             =   336
            Width           =   4332
         End
         Begin VB.TextBox txt 
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   23
            Left            =   4080
            TabIndex        =   38
            Tag             =   "wzfvdisp"
            Top             =   2880
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   22
            Left            =   4080
            TabIndex        =   36
            Tag             =   "wzftdisp"
            Top             =   2520
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   21
            Left            =   4080
            TabIndex        =   34
            Tag             =   "wzfldisp"
            Top             =   2160
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   20
            Left            =   4080
            TabIndex        =   32
            Tag             =   "wzfaqdepth"
            Text            =   "0"
            Top             =   1680
            Visible         =   0   'False
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   19
            Left            =   4080
            TabIndex        =   30
            Tag             =   "wzfydist"
            Text            =   "0"
            Top             =   1200
            Visible         =   0   'False
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   18
            Left            =   4080
            TabIndex        =   28
            Tag             =   "wzfdist"
            Top             =   840
            Width           =   1000
         End
         Begin Threed.SSCommand SSCommand3 
            Height          =   252
            Index           =   1
            Left            =   3120
            TabIndex        =   42
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
            TabIndex        =   41
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
            TabIndex        =   40
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
            TabIndex        =   100
            Top             =   360
            Width           =   1932
         End
         Begin VB.Label lbl 
            Caption         =   "Longitudinal dispersivity - WZ-LDISP"
            Height          =   252
            Index           =   21
            Left            =   120
            TabIndex        =   99
            Tag             =   "WZLDISP"
            Top             =   2160
            Width           =   3804
         End
         Begin VB.Label lbl 
            Caption         =   "Tranverse dispersivity - WZ-TDISP"
            Height          =   252
            Index           =   22
            Left            =   120
            TabIndex        =   98
            Tag             =   "WZTDISP"
            Top             =   2520
            Width           =   3804
         End
         Begin VB.Label lbl 
            Caption         =   "Vertical dispersivity - WZ-VDISP"
            Height          =   252
            Index           =   23
            Left            =   120
            TabIndex        =   97
            Tag             =   "WZVDISP"
            Top             =   2880
            Width           =   3804
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   23
            Left            =   6120
            TabIndex        =   96
            Tag             =   "0"
            Top             =   2880
            Width           =   1000
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   22
            Left            =   6120
            TabIndex        =   95
            Tag             =   "0"
            Top             =   2520
            Width           =   1000
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   21
            Left            =   6120
            TabIndex        =   94
            Tag             =   "0"
            Top             =   2160
            Width           =   1000
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   20
            Left            =   6120
            TabIndex        =   93
            Tag             =   "0"
            Top             =   1680
            Visible         =   0   'False
            Width           =   1000
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   19
            Left            =   6120
            TabIndex        =   92
            Tag             =   "0"
            Top             =   1200
            Visible         =   0   'False
            Width           =   1000
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   18
            Left            =   6120
            TabIndex        =   91
            Tag             =   "0"
            Top             =   840
            Width           =   1000
         End
         Begin VB.Label Label22 
            Caption         =   "Perpendicular distance from plume center line - WZ-YDIST"
            Height          =   372
            Left            =   120
            TabIndex        =   90
            Top             =   1200
            Visible         =   0   'False
            Width           =   3804
         End
         Begin VB.Label Label20 
            Caption         =   "Vertical distance below groundwater table - WZ-AQDEPTH"
            Height          =   372
            Left            =   120
            TabIndex        =   89
            Top             =   1680
            Visible         =   0   'False
            Width           =   3804
         End
         Begin VB.Label Label15 
            Caption         =   "Longitudinal distance to flux location - WZ-DIST"
            Height          =   252
            Left            =   120
            TabIndex        =   88
            Top             =   840
            Width           =   3804
         End
      End
      Begin Threed.SSFrame SSFrame1 
         Height          =   3372
         Index           =   4
         Left            =   -74760
         TabIndex        =   102
         Top             =   480
         Width           =   7212
         _Version        =   65536
         _ExtentX        =   12721
         _ExtentY        =   5948
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
         Begin VB.ComboBox ProgParms 
            Height          =   288
            ItemData        =   "Aquifer.frx":039A
            Left            =   4080
            List            =   "Aquifer.frx":03A7
            Style           =   2  'Dropdown List
            TabIndex        =   142
            Top             =   2160
            Width           =   2892
         End
         Begin VB.ComboBox ConParms 
            Height          =   288
            ItemData        =   "Aquifer.frx":040F
            Left            =   4080
            List            =   "Aquifer.frx":041C
            Style           =   2  'Dropdown List
            TabIndex        =   141
            Top             =   600
            Width           =   2892
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   25
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   53
            Tag             =   "ml/g"
            Top             =   2520
            Width           =   1000
         End
         Begin VB.ComboBox Combo5 
            Height          =   288
            Left            =   240
            Style           =   2  'Dropdown List
            TabIndex        =   50
            Tag             =   "FSCNAME"
            Top             =   2160
            Width           =   3012
         End
         Begin VB.TextBox txt 
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   25
            Left            =   4080
            TabIndex        =   52
            Tag             =   "WASUBKD"
            Top             =   2520
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H00C0C0C0&
            Enabled         =   0   'False
            Height          =   312
            Index           =   27
            Left            =   4080
            TabIndex        =   108
            TabStop         =   0   'False
            Top             =   3000
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H00C0C0C0&
            Enabled         =   0   'False
            Height          =   312
            Index           =   26
            Left            =   4080
            TabIndex        =   106
            TabStop         =   0   'False
            Top             =   1440
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   24
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   46
            Tag             =   "ml/g"
            Top             =   960
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   24
            Left            =   4080
            TabIndex        =   45
            Tag             =   "WASUBKD"
            Top             =   960
            Width           =   1000
         End
         Begin VB.ComboBox Combo4 
            Height          =   288
            Left            =   240
            Style           =   2  'Dropdown List
            TabIndex        =   43
            Tag             =   "FSCNAME"
            Top             =   600
            Width           =   3012
         End
         Begin Threed.SSCommand SSCommand11 
            Height          =   324
            Left            =   2520
            TabIndex        =   54
            Top             =   1800
            Width           =   372
            _Version        =   65536
            _ExtentX        =   656
            _ExtentY        =   572
            _StockProps     =   78
            Caption         =   "<<"
            Font3D          =   1
         End
         Begin Threed.SSCommand SSCommand10 
            Height          =   324
            Left            =   2880
            TabIndex        =   55
            Top             =   1800
            Width           =   372
            _Version        =   65536
            _ExtentX        =   656
            _ExtentY        =   572
            _StockProps     =   78
            Caption         =   ">>"
            Font3D          =   1
         End
         Begin Threed.SSCommand SSCommand9 
            Height          =   312
            Left            =   2040
            TabIndex        =   51
            Top             =   2640
            Width           =   1212
            _Version        =   65536
            _ExtentX        =   2138
            _ExtentY        =   550
            _StockProps     =   78
            Caption         =   "&Use Estimate"
         End
         Begin Threed.SSCommand SSCommand8 
            Height          =   324
            Left            =   240
            TabIndex        =   56
            Top             =   2640
            Width           =   1212
            _Version        =   65536
            _ExtentX        =   2138
            _ExtentY        =   572
            _StockProps     =   78
            Caption         =   "Estimate &All"
         End
         Begin Threed.SSCommand SSCommand7 
            Height          =   324
            Left            =   240
            TabIndex        =   49
            Top             =   1080
            Width           =   1212
            _Version        =   65536
            _ExtentX        =   2138
            _ExtentY        =   572
            _StockProps     =   78
            Caption         =   "Estimate &All"
         End
         Begin Threed.SSCommand SSCommand6 
            Height          =   312
            Left            =   2040
            TabIndex        =   44
            Top             =   1080
            Width           =   1212
            _Version        =   65536
            _ExtentX        =   2138
            _ExtentY        =   550
            _StockProps     =   78
            Caption         =   "&Use Estimate"
         End
         Begin Threed.SSCommand SSCommand5 
            Height          =   324
            Left            =   2880
            TabIndex        =   48
            Top             =   240
            Width           =   372
            _Version        =   65536
            _ExtentX        =   656
            _ExtentY        =   572
            _StockProps     =   78
            Caption         =   ">>"
            Font3D          =   1
         End
         Begin Threed.SSCommand SSCommand4 
            Height          =   324
            Left            =   2520
            TabIndex        =   47
            Top             =   240
            Width           =   372
            _Version        =   65536
            _ExtentX        =   656
            _ExtentY        =   572
            _StockProps     =   78
            Caption         =   "<<"
            Font3D          =   1
         End
         Begin VB.Label Label45 
            Caption         =   "Progeny - FS-CNAME"
            Height          =   252
            Left            =   120
            TabIndex        =   112
            Top             =   1860
            Width           =   2172
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   25
            Left            =   6120
            TabIndex        =   111
            Tag             =   "0"
            Top             =   2520
            Width           =   996
         End
         Begin VB.Label Label44 
            Caption         =   "Progeny Parameter Selection"
            Height          =   252
            Left            =   3840
            TabIndex        =   110
            Top             =   1860
            Width           =   3000
         End
         Begin VB.Label Label43 
            Caption         =   "ml/g"
            Height          =   252
            Left            =   5208
            TabIndex        =   109
            Top             =   3048
            Width           =   996
         End
         Begin VB.Label Label39 
            Caption         =   "ml/g"
            Height          =   252
            Left            =   5208
            TabIndex        =   107
            Top             =   1488
            Width           =   996
         End
         Begin VB.Label Label40 
            Caption         =   "Constituent Parameter Selection"
            Height          =   252
            Left            =   3840
            TabIndex        =   105
            Top             =   300
            Width           =   3000
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   24
            Left            =   6120
            TabIndex        =   104
            Tag             =   "0"
            Top             =   960
            Width           =   996
         End
         Begin VB.Label Label38 
            Caption         =   "Constituent - FS-CNAME"
            Height          =   252
            Left            =   120
            TabIndex        =   103
            Top             =   300
            Width           =   2172
         End
      End
      Begin Threed.SSFrame SSFrame1 
         Height          =   3492
         Index           =   0
         Left            =   240
         TabIndex        =   113
         Top             =   480
         Width           =   7212
         _Version        =   65536
         _ExtentX        =   12721
         _ExtentY        =   6160
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
         Begin VB.TextBox txt 
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   5
            Left            =   4068
            TabIndex        =   122
            Tag             =   "wziron"
            Top             =   2400
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   4
            Left            =   4068
            TabIndex        =   121
            Tag             =   "wzomc"
            Top             =   2040
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   3
            Left            =   4068
            TabIndex        =   120
            Tag             =   "wzclay"
            Top             =   1680
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   2
            Left            =   4068
            TabIndex        =   119
            Tag             =   "wzsilt"
            Top             =   1320
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   1
            Left            =   4068
            TabIndex        =   118
            Tag             =   "wzsand"
            Top             =   960
            Width           =   1000
         End
         Begin VB.ComboBox Combo1 
            BeginProperty Font 
               Name            =   "Courier"
               Size            =   9.6
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   288
            ItemData        =   "Aquifer.frx":0484
            Left            =   2640
            List            =   "Aquifer.frx":04AC
            Style           =   2  'Dropdown List
            TabIndex        =   117
            Tag             =   "wzclass"
            Top             =   480
            Width           =   4332
         End
         Begin VB.Label Label16 
            Caption         =   "* The percent of sand, silt, clay, organic matter, and iron must add up to 100%"
            Height          =   264
            Left            =   120
            TabIndex        =   140
            Top             =   2976
            Width           =   5868
         End
         Begin VB.Label Label33 
            Caption         =   "%"
            Height          =   264
            Left            =   5148
            TabIndex        =   139
            Top             =   2400
            Width           =   192
         End
         Begin VB.Label Label32 
            Caption         =   "%"
            Height          =   264
            Left            =   5148
            TabIndex        =   138
            Top             =   2040
            Width           =   192
         End
         Begin VB.Label Label28 
            Caption         =   "%"
            Height          =   264
            Left            =   5148
            TabIndex        =   137
            Top             =   1680
            Width           =   192
         End
         Begin VB.Label Label27 
            Caption         =   "%"
            Height          =   264
            Left            =   5148
            TabIndex        =   136
            Top             =   1320
            Width           =   192
         End
         Begin VB.Label Label1 
            Caption         =   "%"
            Height          =   264
            Left            =   5148
            TabIndex        =   135
            Top             =   960
            Width           =   192
         End
         Begin VB.Label Label29 
            Caption         =   "Texture         %Sand %Silt %Clay"
            BeginProperty Font 
               Name            =   "Courier"
               Size            =   9.6
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   252
            Left            =   2640
            TabIndex        =   134
            Top             =   240
            Width           =   3972
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   5
            Left            =   5760
            TabIndex        =   133
            Tag             =   "0"
            Top             =   2400
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   4
            Left            =   5760
            TabIndex        =   132
            Tag             =   "0"
            Top             =   2040
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   3
            Left            =   5760
            TabIndex        =   131
            Tag             =   "0"
            Top             =   1680
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   2
            Left            =   5760
            TabIndex        =   130
            Tag             =   "0"
            Top             =   1320
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   1
            Left            =   5760
            TabIndex        =   129
            Tag             =   "0"
            Top             =   960
            Width           =   996
         End
         Begin VB.Label Label7 
            Caption         =   "Percentage of iron and aluminum - WZ-IRON *"
            Height          =   384
            Left            =   120
            TabIndex        =   128
            Top             =   2400
            Width           =   3804
         End
         Begin VB.Label Label6 
            Caption         =   "Percentage of organic matter - WZ-OMC *"
            Height          =   264
            Left            =   120
            TabIndex        =   127
            Top             =   2040
            Width           =   3804
         End
         Begin VB.Label Label5 
            Caption         =   "Percentage of clay - WZ-CLAY *"
            Height          =   264
            Left            =   120
            TabIndex        =   126
            Top             =   1680
            Width           =   3804
         End
         Begin VB.Label Label4 
            Caption         =   "Percentage of silt - WZ-SILT *"
            Height          =   264
            Left            =   120
            TabIndex        =   125
            Top             =   1320
            Width           =   3804
         End
         Begin VB.Label Label3 
            Caption         =   "Percentage of sand - WZ-SAND *"
            Height          =   264
            Left            =   120
            TabIndex        =   124
            Top             =   960
            Width           =   3804
         End
         Begin VB.Label Label2 
            Caption         =   "Soil class - WZ-CLASS"
            Height          =   252
            Left            =   120
            TabIndex        =   123
            Top             =   480
            Width           =   2772
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
Dim ctext1 As String
Dim ctext2 As String
Dim ctext3 As String
Dim ctext4 As String
Dim kdn As Long
Dim uStr(3) As String

'Added flags to control the behaviour of the added parms combo box
Dim oldconidx As Long
Dim oldprogidx As Long
' All these just ro resolve indexs from fui to mui
Dim f_numcon As Long, con() As contam
Dim m_numflux As Long, flux() As locparm
Dim m_numconc As Long, conc() As locparm
Dim m_numcon As Long, kd() As kdparm
 
Private Sub about_Click()
  frmAbout.Show 1, Aquifer
End Sub

Private Sub advan_Click()
  Time.Show 1, Aquifer
End Sub

Private Sub Form_Unload(Cancel As Integer)
  Dim answer As Long
  If Not called Then
    answer = MsgBox("Do you want save changes?", 51, App.Title)
    If answer = 6 Then save_Click
    If answer = 7 Then
      Unload Time
      EndModule
    End If
    If answer = 2 Then Cancel = 1
  End If
End Sub

Private Sub howto_Click()
  GetHelp
End Sub

Private Sub leave_Click()
  Form_Unload 0
End Sub

Private Sub selref_Click()
  RefMode = 0
  GetRef ref(RefItem)
End Sub

Private Sub addref_Click()
  RefMode = 1
  'SetRefFile argv(1) + ".ref"
  GetRef ref(RefItem)
End Sub

Private Sub auto_Click()
  auto.Checked = Not auto.Checked
End Sub

Private Sub estimatekd()
  Dim temp As Double
  Dim i As Long
  For i = 1 To m_numcon
    If con(kd(i).idx).name = Combo4.Text Then
      temp = get_kd(kd(i).cas, kdn)
      If temp > 0 Then
        txt(26).Text = temp
      Else
        txt(26).Text = 0.0001 * con(kd(i).idx).koc(0) * (57.735 * txt(4).Text + 2# * txt(3).Text + 0.4 * txt(2).Text + 0.005 * txt(1).Text)
      End If
      Exit For
    End If
  Next
End Sub

Private Sub d_estimatekd()
  Dim temp As Double
  Dim i As Long
  Dim j As Long
  For i = 1 To m_numcon
    If con(kd(i).idx).name = Combo4.Text Then
      For j = 1 To kd(i).numprog
        If con(kd(i).idx).progeny(kd(i).progeny(j).idx).name = Combo5.Text Then
          temp = get_kd(kd(i).progeny(j).cas, kdn)
          If temp > 0 Then
            txt(27).Text = temp
          Else
            txt(27).Text = 0.0001 * con(kd(i).idx).progeny(kd(i).progeny(j).idx).koc(0) * (57.735 * txt(4).Text + 2# * txt(3).Text + 0.4 * txt(2).Text + 0.005 * txt(1).Text)
          End If
          Exit For
        End If
      Next
      Exit For
    End If
  Next
End Sub

Private Sub elocfillet(idx As Long)
  If temp.ref >= 0 Then
    If temp.idx1 > m_numconc Then
      m_numconc = temp.idx1
      ReDim Preserve conc(m_numconc) As locparm
    End If
    If temp.idx1 = 0 Then
      m_numconc = 1
      ReDim Preserve conc(m_numconc) As locparm
    End If
    conc(temp.idx1).parms(idx).org = convert(temp.cunit, temp.uunit, CDbl(temp.pval))
    conc(temp.idx1).parms(idx).uunt = temp.uunit
    conc(temp.idx1).parms(idx).punt = temp.cunit
    conc(temp.idx1).parms(idx).ref = temp.ref
  End If
End Sub

Private Sub rlocfillet(idx As Long)
  If temp.idx1 > m_numflux Then
    m_numflux = temp.idx1
    ReDim Preserve flux(m_numflux) As locparm
  End If
  flux(temp.idx1).parms(idx).org = convert(temp.cunit, temp.uunit, CDbl(temp.pval))
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

Private Sub loadprm()
  Dim i As Long, j As Long, k As Long, l As Long, m As Long
  Dim prm As String
  Dim prefix As String
  Dim fle As parmfile
  Dim sval As Boolean
  Dim f_numriv As Long, riv() As String, rivl() As String
  Dim f_numrivsrc As Long, rividx() As Long
  Dim f_numaqu As Long, aqu() As String, aqul() As String
  Dim f_numaqusrc As Long, aquidx() As Long
  Dim f_numexp As Long, expp() As String, exppl() As String
  Dim f_numexpsrc As Long, expidx() As Long
   
  f_numriv = 0
  f_numrivsrc = 0
  f_numexp = 0
  f_numexpsrc = 0
  f_numcon = 0
  m_numflux = 0
  m_numcon = 0
  
  m_numconc = 1
  ReDim Preserve conc(m_numconc) As locparm
 
  If open_parm(fle, FUIName, 2) Then
    Do Until EOCF(fle.file)
      If read_parmrec(fle, temp) Then
        Select Case temp.pname
          Case "fui"
            For m = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                If temp.idx1 = siteIdx Then
                  Select Case temp.pname
                    Case "aqudespath"
                      DesName = temp.pval
                    Case "rivnum"
                      f_numriv = Val(temp.pval)
                      ReDim Preserve riv(f_numriv) As String
                      ReDim Preserve rivl(f_numriv) As String
                    Case "rivname"
                      If temp.idx2 > f_numriv Then
                        f_numriv = temp.idx2
                        ReDim Preserve riv(f_numriv) As String
                        ReDim Preserve rivl(f_numriv) As String
                      End If
                      riv(f_numriv) = temp.pval
                    Case "rivlabel"
                      If temp.idx2 > f_numriv Then
                        f_numriv = temp.idx2
                        ReDim Preserve riv(f_numriv) As String
                        ReDim Preserve rivl(f_numriv) As String
                      End If
                      rivl(f_numriv) = temp.pval
                    Case "rivsrcname"
                      If temp.pval = ModName Then
                        f_numrivsrc = f_numrivsrc + 1
                        ReDim Preserve rividx(f_numrivsrc) As Long
                        rividx(f_numrivsrc) = temp.idx2
                      End If
                    Case "aqunum"
                      f_numaqu = Val(temp.pval)
                      ReDim Preserve aqu(f_numaqu) As String
                      ReDim Preserve aqul(f_numaqu) As String
                    Case "aquname"
                      If temp.idx2 > f_numaqu Then
                        f_numaqu = temp.idx2
                        ReDim Preserve aqu(f_numaqu) As String
                        ReDim Preserve aqul(f_numaqu) As String
                      End If
                      aqu(f_numaqu) = temp.pval
                    Case "aqulabel"
                      If temp.idx2 > f_numaqu Then
                        f_numaqu = temp.idx2
                        ReDim Preserve aqu(f_numaqu) As String
                        ReDim Preserve aqul(f_numaqu) As String
                      End If
                      aqul(f_numaqu) = temp.pval
                    Case "aqusrcname"
                      If temp.pval = ModName Then
                        f_numaqusrc = f_numaqusrc + 1
                        ReDim Preserve aquidx(f_numaqusrc) As Long
                        aquidx(f_numaqusrc) = temp.idx2
                      End If
                    Case "expnum"
                      f_numexp = Val(temp.pval)
                      ReDim Preserve expp(f_numexp) As String
                      ReDim Preserve exppl(f_numexp) As String
                    Case "expname"
                      If temp.idx2 > f_numexp Then
                        f_numexp = temp.idx2
                        ReDim Preserve expp(f_numexp) As String
                        ReDim Preserve exppl(f_numexp) As String
                      End If
                      expp(f_numexp) = temp.pval
                    Case "explabel"
                      If temp.idx2 > f_numexp Then
                        f_numexp = temp.idx2
                        ReDim Preserve expp(f_numexp) As String
                        ReDim Preserve exppl(f_numexp) As String
                      End If
                      exppl(f_numexp) = temp.pval
                    Case "expsrcname"
                      If temp.pval = ModName Then
                        f_numexpsrc = f_numexpsrc + 1
                        ReDim Preserve expidx(f_numexpsrc) As Long
                        expidx(f_numexpsrc) = temp.idx2
                      End If
                    Case "numcon"
                      f_numcon = Val(temp.pval)
                      ReDim Preserve con(f_numcon) As contam
                    Case "fscname"
                        If temp.idx2 > f_numcon Then
                          f_numcon = temp.idx2
                          ReDim Preserve con(f_numcon) As contam
                        End If
                        If temp.idx3 = 0 Then
                          con(temp.idx2).name = temp.pval
                        Else
                          If temp.idx3 > con(temp.idx2).numprog Then
                            con(temp.idx2).numprog = temp.idx3
                            ReDim Preserve con(temp.idx2).progeny(temp.idx3) As daughter
                          End If
                          con(temp.idx2).progeny(temp.idx3).name = temp.pval
                        End If
                    Case "fscasid"
                        If temp.idx2 > f_numcon Then
                          f_numcon = temp.idx2
                          ReDim Preserve con(f_numcon) As contam
                        End If
                        If temp.idx3 = 0 Then
                          con(temp.idx2).cas = temp.pval
                        Else
                          If temp.idx3 > con(temp.idx2).numprog Then
                            con(temp.idx2).numprog = temp.idx3
                            ReDim Preserve con(temp.idx2).progeny(temp.idx3) As daughter
                          End If
                          con(temp.idx2).progeny(temp.idx3).cas = temp.pval
                        End If
                    Case "clktype"
                      If temp.idx2 > f_numcon Then
                        f_numcon = temp.idx2
                        ReDim Preserve con(f_numcon) As contam
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
                          ReDim Preserve con(temp.idx2).progeny(temp.idx3) As daughter
                        End If
                      End If
                    Case "clkoc"
                        If temp.idx2 > f_numcon Then
                          f_numcon = temp.idx2
                          ReDim Preserve con(f_numcon) As contam
                        End If
                        If temp.idx3 = 0 Then
                          con(temp.idx2).koc(0) = Val(temp.pval)
                        Else
                          If temp.idx3 > con(temp.idx2).numprog Then
                            con(temp.idx2).numprog = temp.idx3
                            ReDim Preserve con(temp.idx2).progeny(temp.idx3) As daughter
                          End If
                          con(temp.idx2).progeny(temp.idx3).koc(0) = Val(temp.pval)
                        End If
                    Case "clsol"
                      If temp.idx2 > f_numcon Then
                        f_numcon = temp.idx2
                        ReDim Preserve con(f_numcon) As contam
                      End If
                      If temp.idx3 = 0 Then
                        If Val(temp.pval) = 0 Then
                          con(temp.idx2).koc(1) = 1E+23
                        Else
                          con(temp.idx2).koc(1) = Val(temp.pval)
                        End If
                      Else
                        If temp.idx3 > con(temp.idx2).numprog Then
                          con(temp.idx2).numprog = temp.idx3
                          ReDim Preserve con(temp.idx2).progeny(temp.idx3) As daughter
                        End If
                        If Val(temp.pval) = 0 Then
                          con(temp.idx2).progeny(temp.idx3).koc(1) = 1E+23
                        Else
                          con(temp.idx2).progeny(temp.idx3).koc(1) = Val(temp.pval)
                        End If
                      End If
                    Case "clghalf"
                      If temp.idx2 > f_numcon Then
                        f_numcon = temp.idx2
                        ReDim Preserve con(f_numcon) As contam
                      End If
                      If temp.idx3 = 0 Then
                        If Val(temp.pval) = 0 Then
                          con(temp.idx2).koc(2) = 2.738E+17
                        Else
                          con(temp.idx2).koc(2) = Val(temp.pval)
                        End If
                      Else
                        If temp.idx3 > con(temp.idx2).numprog Then
                          con(temp.idx2).numprog = temp.idx3
                          ReDim Preserve con(temp.idx2).progeny(temp.idx3) As daughter
                        End If
                        If Val(temp.pval) = 0 Then
                          con(temp.idx2).progeny(temp.idx3).koc(2) = 2.738E+17
                        Else
                          con(temp.idx2).progeny(temp.idx3).koc(2) = Val(temp.pval)
                        End If
                      End If
                   End Select
                End If
              End If
            Next
          Case ModName
            Loading.Gauge1.Max = Val(temp.idx1)
            Loading.Gauge1.Value = 0
            For m = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                Loading.update
                Select Case temp.pname
                  Case "tfinal":  SetTFinal temp
                  Case "ntimes":  SetNTimes temp
                  Case "timept":  SetTimePt temp
                  Case "wzfname"
                    If temp.idx1 > m_numflux Then
                      m_numflux = temp.idx1
                      ReDim Preserve flux(m_numflux) As locparm
                    End If
                    flux(temp.idx1).name = temp.pval
                    flux(temp.idx1).idx = 0
                  Case "wzfdist":           rlocfillet 0
                  Case "wzfydist":          rlocfillet 1
                  Case "wzfaqdepth":        rlocfillet 2
                  Case "wzfldisp":          rlocfillet 3
                  Case "wzftdisp":          rlocfillet 4
                  Case "wzfvdisp":          rlocfillet 5
                  Case "wzcname"
                    If temp.ref >= 0 Then
                        If temp.idx1 > m_numconc Then
                          m_numconc = temp.idx1
                          ReDim Preserve conc(m_numconc) As locparm
                        End If
                        conc(temp.idx1).name = temp.pval
                        conc(temp.idx1).idx = 0
                    End If
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
                  Case "wzdiv":            fillet 28
                  Case "aqucasid"
                    If temp.idx1 > m_numcon Then
                      m_numcon = temp.idx1
                      ReDim Preserve kd(temp.idx1) As kdparm
                    End If
                    If temp.idx2 = 0 Then
                      kd(temp.idx1).cas = temp.pval
                    Else
                      If temp.idx2 > kd(temp.idx1).numprog Then
                        kd(temp.idx1).numprog = temp.idx2
                        ReDim Preserve kd(temp.idx1).progeny(temp.idx2) As d_kdparm
                      End If
                      kd(temp.idx1).progeny(temp.idx2).cas = temp.pval
                    End If
                  Case "wasubkd", "wzclsol", "wzclthalf"
                     If temp.pname = "wasubkd" Then i = 0
                     If temp.pname = "wzclsol" Then i = 1
                     If temp.pname = "wzclthalf" Then i = 2
                     If temp.idx1 > m_numcon Then
                       m_numcon = temp.idx1
                       ReDim Preserve kd(temp.idx1) As kdparm
                     End If
                     If temp.idx2 = 0 Then
                       kd(temp.idx1).idx = 0
                       If temp.pval <> "EMPTY" Then kd(temp.idx1).kd(i) = Val(temp.pval)
                       kd(temp.idx1).uunt(i) = temp.uunit
                       kd(temp.idx1).ref(i) = temp.ref
                     Else
                       If temp.idx2 > kd(temp.idx1).numprog Then
                         kd(temp.idx1).numprog = temp.idx2
                         ReDim Preserve kd(temp.idx1).progeny(temp.idx2) As d_kdparm
                       End If
                       kd(temp.idx1).progeny(temp.idx2).idx = 0
                       If temp.pval <> "EMPTY" Then kd(temp.idx1).progeny(temp.idx2).kd(i) = Val(temp.pval)
                       kd(temp.idx1).progeny(temp.idx2).uunt(i) = temp.uunit
                       kd(temp.idx1).progeny(temp.idx2).ref(i) = temp.ref
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
    close_parm fle

'resolve contaminate differences if contaminate no longer exists its index is 0
    For i = 1 To f_numcon
      For j = 1 To m_numcon
        If kd(j).cas = con(i).cas Then
          kd(j).idx = i
          If kd(j).kd(1) = "" Or kd(j).kd(1) = Empty Then
            kd(j).kd(1) = con(i).koc(1)
            If con(i).rad Then
              kd(j).uunt(1) = uStr(1)
            Else
              kd(j).uunt(1) = uStr(2)
            End If
            kd(j).kd(2) = con(i).koc(2)
            kd(j).uunt(2) = uStr(3)
          End If
          Exit For
       End If
      Next
      If j > m_numcon Then
        m_numcon = j
        ReDim Preserve kd(j) As kdparm
        kd(j).cas = con(i).cas
        kd(j).idx = i
        kd(j).uunt(0) = uStr(0)
        kd(j).ref(0) = 0
        
        kd(j).kd(1) = con(i).koc(1)
        If con(i).rad Then
          kd(j).uunt(1) = uStr(1)
        Else
          kd(j).uunt(1) = uStr(2)
        End If
        kd(j).ref(1) = 0
        
        kd(j).kd(2) = con(i).koc(2)
        kd(j).uunt(2) = uStr(3)
        kd(j).ref(2) = 0
      End If
    Next
    For i = 1 To m_numcon
      If kd(i).idx > 0 Then Combo4.AddItem con(kd(i).idx).name
    Next

'resolve progeny differences if progeny no longer exists its index is 0
    For i = 1 To m_numcon
      If kd(i).idx <> 0 Then
        For j = 1 To con(kd(i).idx).numprog
          For k = 1 To kd(i).numprog
            If kd(i).progeny(k).cas = con(kd(i).idx).progeny(j).cas Then
              kd(i).progeny(k).idx = j
              If kd(i).progeny(k).kd(1) = "" Or kd(i).progeny(k).kd(1) = Empty Then
                kd(i).progeny(k).kd(1) = con(kd(i).idx).progeny(j).koc(1)
                If con(kd(i).idx).rad Then
                  kd(i).progeny(k).uunt(1) = uStr(1)
                Else
                  kd(i).progeny(k).uunt(1) = uStr(2)
                End If
                
                kd(i).progeny(k).kd(2) = con(kd(i).idx).progeny(j).koc(2)
                kd(i).progeny(k).uunt(2) = uStr(3)
              End If
              Exit For
            End If
          Next
          If k > kd(i).numprog Then
            kd(i).numprog = k
            ReDim Preserve kd(i).progeny(k) As d_kdparm
            kd(i).progeny(k).cas = con(kd(i).idx).progeny(j).cas
            kd(i).progeny(k).idx = j
            
            kd(i).progeny(k).uunt(0) = uStr(0)
            kd(i).progeny(k).ref(0) = 0
            
            kd(i).progeny(k).kd(1) = con(kd(i).idx).progeny(j).koc(1)
            If con(kd(i).idx).rad Then
              kd(i).progeny(k).uunt(1) = uStr(1)
            Else
              kd(i).progeny(k).uunt(1) = uStr(2)
            End If
            kd(i).progeny(k).ref(1) = 0
            
            kd(i).progeny(k).kd(2) = con(kd(i).idx).progeny(j).koc(2)
            kd(i).progeny(k).uunt(2) = uStr(3)
            kd(i).progeny(k).ref(2) = 0
          End If
        Next
      End If
    Next
    
'resolve riv differences         if riv no longer exists its index is 0
    For i = 1 To f_numrivsrc
      For j = 1 To m_numflux
        If flux(j).name = riv(rividx(i)) Then
          flux(j).idx = i
          flux(j).label = rivl(rividx(i))
          Exit For
        End If
      Next
      If j > m_numflux Then
        m_numflux = m_numflux + 1
        ReDim Preserve flux(m_numflux) As locparm
        flux(m_numflux).idx = i
        flux(m_numflux).name = riv(rividx(i))
        flux(m_numflux).label = rivl(rividx(i))
        For k = 0 To 5
          flux(m_numflux).parms(k).uunt = "cm"
          flux(m_numflux).parms(k).punt = "cm"
          flux(m_numflux).parms(k).ref = 0
        Next
      End If
    Next
    
'resolve aqu differences       if aqu no longer exists its index is 0
    For i = 1 To f_numaqusrc
      For j = 1 To m_numflux
        If flux(j).name = aqu(aquidx(i)) Then
          flux(j).idx = i
          flux(j).label = aqul(aquidx(i))
          Exit For
        End If
      Next
      If j > m_numflux Then
        m_numflux = m_numflux + 1
        ReDim Preserve flux(m_numflux) As locparm
        flux(m_numflux).idx = i
        flux(m_numflux).name = aqu(aquidx(i))
        flux(m_numflux).label = aqul(aquidx(i))
        For k = 0 To 5
          flux(m_numflux).parms(k).uunt = "cm"
          flux(m_numflux).parms(k).punt = "cm"
          flux(m_numflux).parms(k).ref = 0
        Next
      End If
    Next
    
'resolve exp differences if riv no longer exists its index is 0
'=============================================
'  Need to add grid if the first time running UI

    conc(0).idx = -1
    If conc(0).name = "" Then
        conc(0).name = "Grid"
        conc(0).label = "Grid"
        For k = 0 To 5
          If k < 2 Then
            conc(0).parms(k).org = 200000
            conc(0).parms(k).uunt = "m"
            conc(0).parms(k).punt = "cm"
            conc(0).parms(k).ref = 0
          Else
            conc(0).parms(k).uunt = "cm"
            conc(0).parms(k).punt = "cm"
            conc(0).parms(k).ref = 0
          End If
        Next
    End If

'=============================================
    For i = 1 To f_numexpsrc
      For j = 1 To m_numconc
        If conc(j).name = expp(expidx(i)) Then
          conc(j).idx = i
          conc(j).label = exppl(expidx(i))
          Exit For
        End If
      Next
      If j > m_numconc Then
        m_numconc = m_numconc + 1
        ReDim Preserve conc(m_numconc) As locparm
        conc(m_numconc).idx = i
        conc(m_numconc).name = expp(expidx(i))
        conc(m_numconc).label = exppl(expidx(i))
        For k = 0 To 5
          conc(m_numconc).parms(k).uunt = "cm"
          conc(m_numconc).parms(k).punt = "cm"
          conc(m_numconc).parms(k).ref = 0
        Next
      End If
    Next
    
    For i = 0 To m_numconc
      If conc(i).name <> "" Then
        If conc(i).idx <> 0 Then
          Combo2.AddItem conc(i).label + " (" + conc(i).name + ")"
        End If
      End If
    Next
    For i = 1 To m_numflux
      If flux(i).idx > 0 Then Combo3.AddItem flux(i).label + " (" + flux(i).name + ")"
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
  Else
    PutError "Can't find or open file " & FUIName
    EndModule
  End If
End Sub

Private Sub Form_load()
  Dim i As Long
  
  uStr(0) = "ml/g"
  uStr(1) = "pCi/ml"
  uStr(2) = "mg/l"
  uStr(3) = "day"
  
  StartModule Aquifer, App.Title, 5
  SetHelpFile App.Path + "\aqu.htm"
  SetRefFile ReplaceExt(FUIName, "ref")
  load_kd
  'set conversion comboboxes
  For i = 7 To 25
    get_conversion_items unit(i).Tag, unit(i)
  Next
  get_conversion_items unit(0).Tag, unit(0)
  SetTimeTitle Aquifer.Caption
  Loading.Show
  loadng = True
  loadprm
  oldconidx = 0
  oldprogidx = 0
  ConParms.ListIndex = 0
  ProgParms.ListIndex = 0
  loadng = False
  Unload Loading
  Refresh
End Sub

Private Sub save_Click()
  Dim i As Long, j As Long, cnt As Long
  Dim fname As String
  Dim parm As parmrec
  Dim fle As parmfile

  txt_LostFocus RefItem
  fname = RunName & ".GID"
  If open_parm(fle, fname, 1) Then
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
        cnt = cnt + 1
        set_parm parm, "wzfname", cnt, 0, 0, 0, 0, 0, 0, "N/A", "N/A", flux(i).name
        write_parmrec fle, parm
        For j = 0 To 5
          set_parm parm, txt(18 + j).Tag, cnt, 0, 0, 0, 0, 0, flux(i).parms(j).ref, flux(i).parms(j).uunt, flux(i).parms(0).punt, convert(flux(i).parms(j).uunt, flux(i).parms(j).punt, Val(flux(i).parms(j).org))
          write_parmrec fle, parm
          If (j + 18 <> 19 And j + 18 <> 20) Then
            If flux(i).parms(j).org = "" Then PutError "Parameter " & txt(18 + j).Tag & " for flux location " & flux(i).name & " is invalid"
          End If
        Next
      End If
    Next
    set_parm parm, "wzfnum", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", Str(cnt)
    write_parmrec fle, parm
   'All exposure points
    cnt = 0
    For i = 0 To m_numconc
      If conc(i).name <> "" Then
        If conc(i).idx <> 0 Then
          set_parm parm, "wzcname", cnt, 0, 0, 0, 0, 0, 0, "N/A", "N/A", conc(i).name
          write_parmrec fle, parm
          For j = 0 To 5
            set_parm parm, txt(12 + j).Tag, cnt, 0, 0, 0, 0, 0, conc(i).parms(j).ref, conc(i).parms(j).uunt, conc(i).parms(j).punt, convert(conc(i).parms(j).uunt, conc(i).parms(j).punt, Val(conc(i).parms(j).org))
            write_parmrec fle, parm
            If conc(i).parms(j).org = "" Then PutError "Parameter " & txt(12 + j).Tag & " for well location " & conc(i).name & " is invalid"
          Next
          cnt = cnt + 1
        End If
      End If
    Next
    set_parm parm, "wzcnum", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", Str(cnt - 1)
    write_parmrec fle, parm

    'Contaminate kd's
    For i = 1 To m_numcon
      If kd(i).idx <> 0 Then
        set_parm parm, "aqucasid", kd(i).idx, 0, 0, 0, 0, 0, kd(i).ref(0), "N/A", "N/A", kd(i).cas
        write_parmrec fle, parm
        If kd(i).kd(0) = Empty Then
          PutError "Parameter WASUBKD for " & kd(i).cas & " is invalid"
          set_parm parm, "wasubkd", kd(i).idx, 0, 0, 0, 0, 0, kd(i).ref(0), kd(i).uunt(0), uStr(0), "EMPTY"
        Else
          set_parm parm, "wasubkd", kd(i).idx, 0, 0, 0, 0, 0, kd(i).ref(0), kd(i).uunt(0), uStr(0), convert(kd(i).uunt(0), uStr(0), Val(kd(i).kd(0)))
        End If
        write_parmrec fle, parm
        
        If kd(i).kd(0) = Empty Or kd(i).kd(1) <= 0 Then
          PutError "Parameter WZCLSOL for " & kd(i).cas & " is invalid"
          If con(kd(i).idx).rad Then
            set_parm parm, "wzclsol", kd(i).idx, 0, 0, 0, 0, 0, kd(i).ref(1), kd(i).uunt(1), uStr(1), "EMPTY"
          Else
            set_parm parm, "wzclsol", kd(i).idx, 0, 0, 0, 0, 0, kd(i).ref(1), kd(i).uunt(1), uStr(2), "EMPTY"
          End If
        Else
          If con(kd(i).idx).rad Then
            set_parm parm, "wzclsol", kd(i).idx, 0, 0, 0, 0, 0, kd(i).ref(1), kd(i).uunt(1), uStr(1), convert(kd(i).uunt(1), uStr(1), Val(kd(i).kd(1)))
          Else
            set_parm parm, "wzclsol", kd(i).idx, 0, 0, 0, 0, 0, kd(i).ref(1), kd(i).uunt(1), uStr(2), convert(kd(i).uunt(1), uStr(2), Val(kd(i).kd(1)))
          End If
        End If
        write_parmrec fle, parm
        
        If kd(i).kd(0) = Empty Or kd(i).kd(2) <= 0 Then
          PutError "Parameter WZCLTHALF for " & kd(i).cas & " is invalid"
          set_parm parm, "wzclthalf", kd(i).idx, 0, 0, 0, 0, 0, kd(i).ref(2), kd(i).uunt(2), uStr(3), "EMPTY"
        Else
          set_parm parm, "wzclthalf", kd(i).idx, 0, 0, 0, 0, 0, kd(i).ref(2), kd(i).uunt(2), uStr(3), convert(kd(i).uunt(2), uStr(3), Val(kd(i).kd(2)))
        End If
        write_parmrec fle, parm
        
        For j = 1 To kd(i).numprog
          If kd(i).progeny(j).idx <> 0 Then
            set_parm parm, "aqucasid", kd(i).idx, kd(i).progeny(j).idx, 0, 0, 0, 0, kd(i).progeny(j).ref(0), "N/A", "N/A", kd(i).progeny(j).cas
            write_parmrec fle, parm
            
            If kd(i).progeny(j).kd(0) = Empty Then
              PutError "Parameter WASUBKD for " & kd(i).progeny(j).cas & " daughter of " & kd(i).cas & " is invalid"
              set_parm parm, "wasubkd", kd(i).idx, kd(i).progeny(j).idx, 0, 0, 0, 0, kd(i).progeny(j).ref(0), kd(i).progeny(j).uunt(0), uStr(0), "EMPTY"
            Else
              set_parm parm, "wasubkd", kd(i).idx, kd(i).progeny(j).idx, 0, 0, 0, 0, kd(i).progeny(j).ref(0), kd(i).progeny(j).uunt(0), uStr(0), convert(kd(i).progeny(j).uunt(0), uStr(0), Val(kd(i).progeny(j).kd(0)))
            End If
            write_parmrec fle, parm
            
            If kd(i).progeny(j).kd(1) = Empty Or kd(i).progeny(j).kd(1) <= 0 Then
              PutError "Parameter WZCLSOL for " & kd(i).progeny(j).cas & " daughter of " & kd(i).cas & " is invalid"
              If con(kd(i).idx).rad Then
                set_parm parm, "wzclsol", kd(i).idx, kd(i).progeny(j).idx, 0, 0, 0, 0, kd(i).progeny(j).ref(1), kd(i).progeny(j).uunt(1), uStr(1), "EMPTY"
              Else
                set_parm parm, "wzclsol", kd(i).idx, kd(i).progeny(j).idx, 0, 0, 0, 0, kd(i).progeny(j).ref(1), kd(i).progeny(j).uunt(1), uStr(2), "EMPTY"
              End If
            Else
              If con(kd(i).idx).rad Then
                set_parm parm, "wzclsol", kd(i).idx, kd(i).progeny(j).idx, 0, 0, 0, 0, kd(i).progeny(j).ref(1), kd(i).progeny(j).uunt(1), uStr(1), convert(kd(i).progeny(j).uunt(1), uStr(1), Val(kd(i).progeny(j).kd(1)))
              Else
                set_parm parm, "wzclsol", kd(i).idx, kd(i).progeny(j).idx, 0, 0, 0, 0, kd(i).progeny(j).ref(1), kd(i).progeny(j).uunt(1), uStr(2), convert(kd(i).progeny(j).uunt(1), uStr(2), Val(kd(i).progeny(j).kd(1)))
              End If
            End If
            write_parmrec fle, parm
            
            If kd(i).progeny(j).kd(2) = Empty Or kd(i).progeny(j).kd(2) <= 0 Then
              PutError "Parameter WZCLTHALF for " & kd(i).progeny(j).cas & " daughter of " & kd(i).cas & " is invalid"
              set_parm parm, "wzclthalf", kd(i).idx, kd(i).progeny(j).idx, 0, 0, 0, 0, kd(i).progeny(j).ref(2), kd(i).progeny(j).uunt(2), uStr(3), "EMPTY"
            Else
              set_parm parm, "wzclthalf", kd(i).idx, kd(i).progeny(j).idx, 0, 0, 0, 0, kd(i).progeny(j).ref(2), kd(i).progeny(j).uunt(2), uStr(3), convert(kd(i).progeny(j).uunt(2), uStr(3), Val(kd(i).progeny(j).kd(2)))
            End If
            write_parmrec fle, parm
            
          End If
        Next
      End If
    Next
    close_parm fle
  Else
    PutError "Unable to create transaction file" & RunName & ".GID"
  End If
  Unload Time
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
  If Combo4.ListIndex > 0 Then Combo4.ListIndex = Combo4.ListIndex - 1
End Sub

Private Sub SSCommand5_Click()
  If Combo4.ListIndex < Combo4.ListCount - 1 Then Combo4.ListIndex = Combo4.ListIndex + 1
End Sub

Private Sub SSCommand6_Click()
  Dim i As Long
  unit(24).Text = unit(24).Tag
  txt(24).Text = txt(26).Text
  For i = 1 To m_numcon
    If con(kd(i).idx).name = Combo4.Text Then
      kd(i).kd(ConParms.ListIndex) = Val(txt(26).Text)
      kd(i).uunt(ConParms.ListIndex) = unit(24).Tag
      Exit For
    End If
  Next
End Sub

Private Sub SSCommand7_Click()
  Dim i As Long
  Dim kdee As Double
  For i = 1 To m_numcon
    kdee = get_kd(kd(i).cas, kdn)
    If kdee > 0 Then
      kd(i).kd(ConParms.ListIndex) = kdee
    Else
      kd(i).kd(ConParms.ListIndex) = 0.0001 * con(kd(i).idx).koc(0) * (57.735 * txt(4).Text + 2# * txt(3).Text + 0.4 * txt(2).Text + 0.005 * txt(1).Text)
    End If
    kd(i).uunt(ConParms.ListIndex) = unit(24).Tag
  Next
  SSCommand6_Click
End Sub

Private Sub SSCommand8_Click()
  Dim i As Long
  Dim j As Long
  Dim kdee As Double
  For i = 1 To m_numcon
    If (con(kd(i).idx).name = Combo4.Text) Then
      For j = 1 To kd(i).numprog
        kdee = get_kd(kd(i).progeny(j).cas, kdn)
        If kdee > 0 Then
          kd(i).progeny(j).kd(ProgParms.ListIndex) = kdee
        Else
          kd(i).progeny(j).kd(ProgParms.ListIndex) = 0.0001 * con(kd(i).idx).progeny(kd(i).progeny(j).idx).koc(0) * (57.735 * txt(4).Text + 2# * txt(3).Text + 0.4 * txt(2).Text + 0.005 * txt(1).Text)
        End If
        kd(i).progeny(j).uunt(ProgParms.ListIndex) = unit(25).Tag
      Next
      Exit For
    End If
  Next
  SSCommand9_Click
End Sub

Private Sub SSCommand9_Click()
  Dim i As Long
  Dim j As Long
  unit(25).Text = unit(25).Tag
  txt(25).Text = txt(27).Text
  For i = 1 To m_numcon
    If con(kd(i).idx).name = Combo4.Text Then
      For j = 1 To kd(i).numprog
        If con(kd(i).idx).progeny(kd(i).progeny(j).idx).name = Combo5.Text Then
          kd(i).progeny(j).kd(ProgParms.ListIndex) = Val(txt(27).Text)
          kd(i).progeny(j).uunt(ProgParms.ListIndex) = unit(25).Tag
          Exit For
        End If
      Next
      Exit For
    End If
  Next
End Sub

Private Sub SSCommand10_Click()
  If Combo5.ListIndex < Combo5.ListCount - 1 Then Combo5.ListIndex = Combo5.ListIndex + 1
End Sub

Private Sub SSCommand11_Click()
  If Combo5.ListIndex > 0 Then Combo5.ListIndex = Combo5.ListIndex - 1
End Sub

Private Sub Timer1_Timer()
  mes = Mid(mes, 2) & Mid(mes, 1, 1)
End Sub

Private Sub unit_Click(Index As Integer)
  er CLng(Index)
End Sub

Private Sub SSTab1_Click(PreviousTab As Integer)
  Dim sum As Long
  
  SSFrame1(PreviousTab).Enabled = False
  SSFrame1(SSTab1.Tab).Enabled = True
  noact.Enabled = False
  sum = Val(txt(1).Text) + Val(txt(2).Text) + Val(txt(3).Text) + Val(txt(4).Text) + Val(txt(5).Text)
  If sum > 100.1 Or sum < 99.9 And SSTab1.Tab <> 0 Then
    MsgBox "The percentages entered for sand, silt, clay, organic matter, and iron must add up to 100 percent", 48, "Range Error!"
    SSTab1.Tab = 0
    txt(1).SetFocus
    Exit Sub
  Else
    If (er(6) And SSTab1.Tab > 1) Then
      MsgBox "PH value needed for KD estimator.", 48, "Range Error!"
      SSTab1.Tab = 1
      txt(6).SetFocus
      Exit Sub
    Else
      sum = Val(txt(3).Text) + Val(txt(4).Text) + Val(txt(5).Text)
      If Val(txt(6).Text) >= 9# Then
        If sum >= 30# Then
          kdn = 3
        Else
          If sum >= 10# Then
            kdn = 2
          Else
            kdn = 1
          End If
        End If
      Else
        If Val(txt(6).Text) > 5# Then
          If sum >= 30# Then
            kdn = 6
          Else
            If sum >= 10# Then
              kdn = 5
            Else
              kdn = 4
            End If
          End If
        Else
          If sum >= 30# Then
            kdn = 9
          Else
            If sum >= 10# Then
              kdn = 8
            Else
              kdn = 7
            End If
          End If
        End If
      End If
    End If
  End If
  If SSTab1.Tab >= 2 And Combo4.ListIndex = -1 Then SSCommand5_Click
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
      t2 = convert(unit(Index).Tag, unit(Index).Text, 304800)
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
    Case 24
      If ConParms.ListIndex = 0 Then
        m = "Value must be greater than or equal to 0"
        If (tval < 0) Then er = True
      Else
        m = "Value must be greater than 0"
        If Not (tval > 0) Then er = True
      End If
    Case 25
      If ProgParms.ListIndex = 0 Then
        m = "Value must be greater than or equal to 0"
        If (tval < 0) Then er = True
      Else
        m = "Value must be greater than 0"
        If Not (tval > 0) Then er = True
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
  Exit Sub
toolarge:
  txt(Index).BackColor = &H8080FF
End Sub

Private Sub changetxt(i As Long, j As Long, k As Long, f1 As Double, f2 As Double, f3 As Double, f4 As Double, f5 As Double)
  If Not loadng Then
    txt(1).Text = i
    txt(2).Text = j
    txt(3).Text = k
    txt(4).Text = 0
    txt(5).Text = 0
    If auto.Checked Then
      unit(11).Text = unit(11).Tag
      txt(7).Text = f2
      txt(11).Text = f3

'  cannot use field capcity for effective porosity estimate
'  as per John McDonalds instruction
'     txt(8).Text = f4
    End If
  End If
End Sub

Private Sub fluxchange(tx As String, oldtx As String)
  Dim i As Long, j As Long, k As Long, l As Long
  k = 18
  For i = 1 To m_numflux
    If flux(i).label + " (" + flux(i).name + ")" = oldtx Then
      For j = 0 To 5
         flux(i).parms(j).org = txt(j + k).Text
         flux(i).parms(j).uunt = unit(j + k).Text
         flux(i).parms(j).punt = unit(j + k).Tag
         flux(i).parms(j).ref = ref(j + k).Tag
      Next
      Exit For
    End If
  Next
  For i = 1 To m_numflux
    If flux(i).label + " (" + flux(i).name + ")" = tx Then
      For j = 0 To 5
         set_unit unit(j + k), flux(i).parms(j).uunt
         txt(j + k).Text = flux(i).parms(j).org
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
  
  For i = 0 To m_numconc
    If conc(i).label + " (" + conc(i).name + ")" = oldtx Then
      For j = 0 To 5
         conc(i).parms(j).org = txt(j + k).Text
         conc(i).parms(j).uunt = unit(j + k).Text
         conc(i).parms(j).punt = unit(j + k).Tag
         conc(i).parms(j).ref = ref(j + k).Tag
      Next
      Exit For
    End If
  Next
  For i = 0 To m_numconc
    If conc(i).label + " (" + conc(i).name + ")" = tx Then
      For j = 0 To 5
        set_unit unit(j + k), conc(i).parms(j).uunt
        txt(j + k).Text = conc(i).parms(j).org
        ref(j + k).Caption = "Ref: " & conc(i).parms(j).ref
        ref(j + k).Tag = conc(i).parms(j).ref
      Next
      Exit For
    End If
  Next
End Sub

Private Sub kdchange(tx As String, oldtx As String)
  Dim i As Long
  
  For i = 1 To m_numcon
    If con(kd(i).idx).name = oldtx Then
       If txt(24).Text = "" Then
         kd(i).kd(oldconidx) = Empty
       Else
         kd(i).kd(oldconidx) = txt(24).Text
       End If
       kd(i).uunt(oldconidx) = unit(24).Text
       kd(i).ref(oldconidx) = Val(ref(24).Tag)
       Exit For
    End If
  Next
  For i = 0 To m_numcon
    If con(kd(i).idx).name = tx Then
      unit(24).Clear
      get_conversion_items kd(i).uunt(ConParms.ListIndex), unit(24)
      set_unit unit(24), kd(i).uunt(ConParms.ListIndex)
      If kd(i).kd(ConParms.ListIndex) = Empty Then
        txt(24).Text = ""
      Else
        txt(24).Text = kd(i).kd(ConParms.ListIndex)
      End If
      ref(24).Caption = "Ref: " & kd(i).ref(ConParms.ListIndex)
      ref(24).Tag = kd(i).ref(ConParms.ListIndex)
      estimatekd
      If ConParms.ListIndex = 2 Then
        If con(i).rad Then
          txt(24).Enabled = False
          unit(24).Enabled = False
        Else
          txt(24).Enabled = True
          unit(24).Enabled = True
        End If
      Else
        txt(24).Enabled = True
        unit(24).Enabled = True
      End If
      Exit For
    End If
  Next
End Sub

Private Sub d_kdchange(tx As String, oldtx As String)
  Dim i As Long
  Dim j As Long
  
  For i = 1 To m_numcon
    If con(kd(i).idx).name = Combo4.Text Then
      For j = 1 To kd(i).numprog
        If con(kd(i).idx).progeny(kd(i).progeny(j).idx).name = oldtx Then
          If txt(25).Text = "" Then
            kd(i).progeny(j).kd(oldprogidx) = Empty
          Else
            kd(i).progeny(j).kd(oldprogidx) = txt(25).Text
          End If
          kd(i).progeny(j).uunt(oldprogidx) = unit(25).Text
          kd(i).progeny(j).ref(oldprogidx) = Val(ref(25).Tag)
          Exit For
        End If
      Next
      Exit For
    End If
  Next
  
  For i = 0 To m_numcon
    If con(kd(i).idx).name = Combo4.Text Then
      For j = 1 To kd(i).numprog
        If con(kd(i).idx).progeny(kd(i).progeny(j).idx).name = tx Then
          unit(25).Clear
          get_conversion_items kd(i).progeny(j).uunt(ProgParms.ListIndex), unit(25)
          set_unit unit(25), kd(i).progeny(j).uunt(ProgParms.ListIndex)
          If kd(i).progeny(j).kd(ProgParms.ListIndex) = Empty Then
            txt(25).Text = ""
          Else
            txt(25).Text = kd(i).progeny(j).kd(ProgParms.ListIndex)
          End If
          ref(25).Caption = "Ref: " & kd(i).progeny(j).ref(ProgParms.ListIndex)
          ref(25).Tag = kd(i).progeny(j).ref(ProgParms.ListIndex)
          d_estimatekd
          If ProgParms.ListIndex = 2 Then
            txt(25).Enabled = False
            unit(25).Enabled = False
          Else
            txt(25).Enabled = True
            unit(25).Enabled = True
          End If
          Exit For
        End If
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
End Sub

Private Sub Combo2_Click()
  concchange Combo2.Text, ctext1
  ctext1 = Combo2.Text
End Sub

Private Sub Combo3_Click()
  fluxchange Combo3.Text, ctext2
  ctext2 = Combo3.Text
End Sub

Private Sub Combo4_Click()
  Dim i As Long
  Dim j As Long
  kdchange Combo4.Text, ctext3
  ctext3 = Combo4.Text
  Combo5.Clear
  For i = 1 To m_numcon
    If con(kd(i).idx).name = ctext3 Then
      If kd(i).numprog = 0 Then
        txt(25).Visible = False
        ProgParms.Visible = False
        Label44.Visible = False
        unit(25).Visible = False
        ref(25).Visible = False
        txt(27).Visible = False
        SSCommand8.Visible = False
        SSCommand9.Visible = False
        SSCommand10.Visible = False
        SSCommand11.Visible = False
        Label43.Visible = False
        Label44.Visible = False
        Label45.Visible = False
        Combo5.Visible = False
      Else
        For j = 1 To kd(i).numprog
          Combo5.AddItem con(kd(i).idx).progeny(kd(i).progeny(j).idx).name
        Next
        txt(25).Visible = True
        ProgParms.Visible = True
        Label44.Visible = True
        unit(25).Visible = True
        ref(25).Visible = True
        txt(27).Visible = True
        SSCommand8.Visible = True
        SSCommand9.Visible = True
        SSCommand10.Visible = True
        SSCommand11.Visible = True
        Label43.Visible = True
        Label44.Visible = True
        Label45.Visible = True
        Combo5.Visible = True
        Combo5.ListIndex = 0
      End If
      Exit For
    End If
  Next
End Sub

Private Sub Combo5_Click()
  d_kdchange Combo5.Text, ctext4
  ctext4 = Combo5.Text
  If ProgParms.ListIndex = 0 Then
    txt(27).Visible = True
    Label43.Visible = True
  Else
    txt(27).Visible = False
    Label43.Visible = False
  End If
End Sub

Private Sub ConParms_Click()
  Dim check As Boolean
  
  If loadng Then Exit Sub
  Combo4_Click
  oldconidx = ConParms.ListIndex
  If ConParms.ListIndex = 0 Then
    check = True
  Else
    check = False
  End If
  SSCommand7.Visible = check
  SSCommand6.Visible = check
  txt(26).Visible = check
  Label39.Visible = check
End Sub

Private Sub ProgParms_Click()
  Dim check As Boolean
  
  If loadng Then Exit Sub
  Combo5_Click
  oldprogidx = ProgParms.ListIndex
  If ProgParms.ListIndex = 0 Then
    check = True
  Else
    check = False
  End If
  SSCommand8.Visible = check
  SSCommand9.Visible = check
  txt(27).Visible = check
  Label43.Visible = check
End Sub

'<<<<<<<<<<<<<<<<<<<<<<<<
'got focus events for all controls
'>>>>>>>>>>>>>>>>>>>>>>>>

Private Sub txt_LostFocus(Index As Integer)
  Select Case Index
    Case 12 To 17:     Combo2_Click
    Case 18 To 23:     Combo3_Click
    Case 24:           Combo4_Click
    Case 25:           Combo5_Click
  End Select
'  ConParms.Enabled = True
'  ProgParms.Enabled = True
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
'  ConParms.Enabled = False
'  ProgParms.Enabled = False
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
  Case 0: HelpAnchor = "SOIL_COMPOSITION"
  Case 1: HelpAnchor = "CHARACTERISTICS"
  Case 2, 3: HelpAnchor = "LOCATIONS"
  Case 4: HelpAnchor = "KDS"
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

Private Sub Combo4_GotFocus()
Dim m As String
  m = "Select a constituent"
  mes = Space(140 - Len(m)) & m
  RefItem = 24
  noact.Enabled = True
  HelpAnchor = Combo4.Tag
End Sub

Private Sub Combo5_GotFocus()
Dim m As String
  m = "Select a progeny"
  mes = Space(140 - Len(m)) & m
  RefItem = 25
  noact.Enabled = False
  HelpAnchor = Combo5.Tag
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
  mes = ""
  noact.Enabled = False
  HelpAnchor = "KDS"
End Sub

Private Sub SSCommand5_GotFocus()
  mes = ""
  noact.Enabled = False
  HelpAnchor = "KDS"
End Sub

Private Sub SSCommand6_GotFocus()
  mes = ""
  noact.Enabled = False
  HelpAnchor = "KDS"
End Sub

Private Sub SSCommand7_GotFocus()
  mes = ""
  noact.Enabled = False
  HelpAnchor = "KDS"
End Sub

Private Sub SSCommand8_GotFocus()
  mes = ""
  noact.Enabled = False
  HelpAnchor = "KDS"
End Sub

Private Sub SSCommand9_GotFocus()
  mes = ""
  noact.Enabled = False
  HelpAnchor = "KDS"
End Sub

Private Sub SSCommand10_GotFocus()
  mes = ""
  noact.Enabled = False
  HelpAnchor = "KDS"
End Sub

Private Sub SSCommand11_GotFocus()
  mes = ""
  noact.Enabled = False
  HelpAnchor = "KDS"
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


