VERSION 5.00
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "tabctl32.ocx"
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "threed32.ocx"
Begin VB.Form Vadose 
   BorderStyle     =   1  'Fixed Single
   ClientHeight    =   4692
   ClientLeft      =   1500
   ClientTop       =   2856
   ClientWidth     =   7680
   Icon            =   "Vadose.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   NegotiateMenus  =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   391
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
      TabIndex        =   84
      TabStop         =   0   'False
      Top             =   4320
      Width           =   7680
   End
   Begin TabDlg.SSTab SSTab1 
      Height          =   4335
      Left            =   0
      TabIndex        =   22
      Top             =   0
      Width           =   7695
      _ExtentX        =   13568
      _ExtentY        =   7641
      _Version        =   393216
      Style           =   1
      TabHeight       =   529
      TabCaption(0)   =   "Composition"
      TabPicture(0)   =   "Vadose.frx":030A
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "Timer1"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).Control(1)=   "SSFrame1(0)"
      Tab(0).Control(1).Enabled=   0   'False
      Tab(0).ControlCount=   2
      TabCaption(1)   =   "Characteristics"
      TabPicture(1)   =   "Vadose.frx":0326
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "SSFrame1(1)"
      Tab(1).ControlCount=   1
      TabCaption(2)   =   "Constituent Properties"
      TabPicture(2)   =   "Vadose.frx":0342
      Tab(2).ControlEnabled=   0   'False
      Tab(2).Control(0)=   "SSFrame1(2)"
      Tab(2).Control(0).Enabled=   0   'False
      Tab(2).ControlCount=   1
      Begin Threed.SSFrame SSFrame1 
         Height          =   3660
         Index           =   1
         Left            =   -74760
         TabIndex        =   77
         Top             =   480
         Width           =   7212
         _Version        =   65536
         _ExtentX        =   12721
         _ExtentY        =   6456
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
            TabIndex        =   59
            Tag             =   "percent"
            Top             =   1080
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   7
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   55
            Tag             =   "percent"
            Top             =   720
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   17
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   71
            Tag             =   "cm"
            Top             =   2400
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   17
            Left            =   4080
            TabIndex        =   70
            Tag             =   "wpldisp"
            Top             =   2400
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   11
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   75
            Tag             =   "g/cm^3"
            Top             =   2760
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   10
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   67
            Tag             =   "cm"
            Top             =   2040
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   9
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   63
            Tag             =   "cm/day"
            Top             =   1680
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   11
            Left            =   4080
            TabIndex        =   74
            Tag             =   "wpbulk"
            Top             =   2760
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   10
            Left            =   4080
            TabIndex        =   66
            Tag             =   "wpthick"
            Top             =   2040
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   9
            Left            =   4080
            TabIndex        =   62
            Tag             =   "wpconduc"
            Top             =   1680
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   8
            Left            =   4080
            TabIndex        =   58
            Tag             =   "wpfieldc"
            Top             =   1080
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   7
            Left            =   4080
            TabIndex        =   54
            Tag             =   "wptotpor"
            Top             =   720
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   6
            Left            =   4080
            TabIndex        =   50
            Tag             =   "wpph"
            Top             =   360
            Width           =   1000
         End
         Begin VB.Label Label18 
            Caption         =   "Longitudinal dispersivity - WP-LDISP"
            Height          =   264
            Left            =   120
            TabIndex        =   69
            Top             =   2400
            Width           =   3804
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   17
            Left            =   6120
            TabIndex        =   72
            Tag             =   "0"
            Top             =   2430
            Width           =   1005
         End
         Begin VB.Label Label35 
            Caption         =   "pH"
            Height          =   252
            Left            =   5160
            TabIndex        =   51
            Top             =   360
            Width           =   504
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   11
            Left            =   6120
            TabIndex        =   76
            Tag             =   "0"
            Top             =   2790
            Width           =   1005
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   10
            Left            =   6120
            TabIndex        =   68
            Tag             =   "0"
            Top             =   2070
            Width           =   1005
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   9
            Left            =   6120
            TabIndex        =   64
            Tag             =   "0"
            Top             =   1710
            Width           =   1005
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   8
            Left            =   6120
            TabIndex        =   60
            Tag             =   "0"
            Top             =   1110
            Width           =   990
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   7
            Left            =   6120
            TabIndex        =   56
            Tag             =   "0"
            Top             =   750
            Width           =   990
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   6
            Left            =   6120
            TabIndex        =   52
            Tag             =   "0"
            Top             =   390
            Width           =   1005
         End
         Begin VB.Label Label13 
            Caption         =   "Dry bulk density - WP-BULKD"
            Height          =   264
            Left            =   120
            TabIndex        =   73
            Top             =   2760
            Width           =   3804
         End
         Begin VB.Label Label12 
            Caption         =   "Thickness of this layer - WP-THICK"
            Height          =   264
            Left            =   120
            TabIndex        =   65
            Top             =   2040
            Width           =   3804
         End
         Begin VB.Label Label11 
            Caption         =   "Saturated hydraulic conductivity - WP-CONDUC"
            Height          =   264
            Left            =   120
            TabIndex        =   61
            Top             =   1680
            Width           =   3804
         End
         Begin VB.Label Label10 
            Caption         =   "Field capacity - WP-FIELDC"
            Height          =   264
            Left            =   120
            TabIndex        =   57
            Top             =   1080
            Width           =   3804
         End
         Begin VB.Label Label9 
            Caption         =   "Total porosity - WP-TOTPOR"
            Height          =   264
            Left            =   120
            TabIndex        =   53
            Top             =   720
            Width           =   3804
         End
         Begin VB.Label Label8 
            Caption         =   "pH of the pore water - WP-PH"
            Height          =   264
            Left            =   120
            TabIndex        =   49
            Top             =   360
            Width           =   3804
         End
      End
      Begin Threed.SSFrame SSFrame1 
         Height          =   3660
         Index           =   2
         Left            =   -74760
         TabIndex        =   82
         Top             =   480
         Width           =   7212
         _Version        =   65536
         _ExtentX        =   12721
         _ExtentY        =   6456
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
         Begin VB.CommandButton Command4 
            Caption         =   "<"
            Height          =   230
            Left            =   6120
            TabIndex        =   15
            Top             =   2250
            Width           =   250
         End
         Begin VB.CommandButton Command2 
            Caption         =   "<"
            Height          =   230
            Left            =   6120
            TabIndex        =   4
            Top             =   510
            Width           =   250
         End
         Begin VB.CommandButton SSCommand1 
            Caption         =   "<"
            Height          =   230
            Left            =   2880
            TabIndex        =   12
            Top             =   2250
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
         Begin VB.CommandButton Command3 
            Caption         =   ">"
            Height          =   230
            Left            =   6370
            TabIndex        =   16
            Top             =   2250
            Width           =   230
         End
         Begin VB.CommandButton Command1 
            Caption         =   ">"
            Height          =   230
            Left            =   6370
            TabIndex        =   5
            Top             =   510
            Width           =   230
         End
         Begin VB.ComboBox ConKds 
            Height          =   315
            ItemData        =   "Vadose.frx":035E
            Left            =   360
            List            =   "Vadose.frx":036B
            Style           =   2  'Dropdown List
            TabIndex        =   6
            Tag             =   "wasubkd"
            Top             =   960
            Width           =   3255
         End
         Begin VB.ComboBox ProgKds 
            Height          =   315
            ItemData        =   "Vadose.frx":03A1
            Left            =   360
            List            =   "Vadose.frx":03AE
            Style           =   2  'Dropdown List
            TabIndex        =   17
            Tag             =   "wasubkd"
            Top             =   2640
            Width           =   3255
         End
         Begin VB.CommandButton SSCommand8 
            Caption         =   "Estimate All"
            Height          =   255
            Left            =   360
            TabIndex        =   21
            Tag             =   "wasubkd"
            Top             =   3000
            Width           =   3255
         End
         Begin VB.CommandButton SSCommand7 
            Caption         =   "Estimate All"
            Height          =   255
            Left            =   360
            TabIndex        =   10
            Tag             =   "wasubkd"
            Top             =   1320
            Width           =   3255
         End
         Begin VB.CommandButton SSCommand2 
            Caption         =   ">"
            Height          =   230
            Left            =   3130
            TabIndex        =   13
            Top             =   2250
            Width           =   230
         End
         Begin VB.CommandButton SSCommand5 
            Caption         =   ">"
            Height          =   230
            Left            =   3130
            TabIndex        =   2
            Top             =   510
            Width           =   230
         End
         Begin VB.ComboBox ProgParms 
            Height          =   288
            ItemData        =   "Vadose.frx":03E4
            Left            =   3920
            List            =   "Vadose.frx":03F1
            Style           =   2  'Dropdown List
            TabIndex        =   14
            Top             =   2222
            Width           =   2892
         End
         Begin VB.ComboBox ConParms 
            Height          =   288
            ItemData        =   "Vadose.frx":045C
            Left            =   3920
            List            =   "Vadose.frx":0469
            Style           =   2  'Dropdown List
            TabIndex        =   3
            Top             =   480
            Width           =   2892
         End
         Begin VB.ComboBox unit 
            Height          =   315
            Index           =   12
            Left            =   4917
            Style           =   2  'Dropdown List
            TabIndex        =   8
            Tag             =   "ml/g"
            Top             =   960
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   315
            Index           =   13
            Left            =   4917
            Style           =   2  'Dropdown List
            TabIndex        =   19
            Tag             =   "ml/g"
            Top             =   2640
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   13
            Left            =   3960
            TabIndex        =   18
            Tag             =   "wasubkd"
            Top             =   2640
            Width           =   1000
         End
         Begin VB.ComboBox cboProgeny 
            Height          =   288
            Left            =   200
            Style           =   2  'Dropdown List
            TabIndex        =   11
            Tag             =   "FSCNAME"
            Top             =   2222
            Width           =   3375
         End
         Begin VB.ComboBox cboParent 
            Height          =   288
            Left            =   200
            Style           =   2  'Dropdown List
            TabIndex        =   0
            Tag             =   "FSCNAME"
            Top             =   480
            Width           =   3375
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   12
            Left            =   3960
            TabIndex        =   7
            Tag             =   "wasubkd"
            Top             =   960
            Width           =   1000
         End
         Begin VB.Label Label2 
            Caption         =   "Please note: Degradation products move at the same speed as the parent."
            ForeColor       =   &H000000C0&
            Height          =   252
            Left            =   360
            TabIndex        =   85
            Top             =   3360
            Width           =   6492
         End
         Begin VB.Label Label17 
            Caption         =   "Parameters"
            Height          =   253
            Left            =   3839
            TabIndex        =   81
            Top             =   1980
            Width           =   3003
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   13
            Left            =   6000
            TabIndex        =   20
            Tag             =   "0"
            Top             =   2700
            Width           =   1005
         End
         Begin VB.Label Label16 
            Caption         =   "Progeny - FS-CNAME"
            Height          =   253
            Left            =   121
            TabIndex        =   80
            Top             =   1980
            Width           =   1936
         End
         Begin VB.Label Label38 
            Caption         =   "Constituent - FS-CNAME"
            Height          =   253
            Left            =   121
            TabIndex        =   78
            Top             =   240
            Width           =   2112
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   12
            Left            =   6000
            TabIndex        =   9
            Tag             =   "0"
            Top             =   1050
            Width           =   1005
         End
         Begin VB.Label Label40 
            Caption         =   "Parameters"
            Height          =   253
            Left            =   3839
            TabIndex        =   79
            Top             =   240
            Width           =   3003
         End
      End
      Begin Threed.SSFrame SSFrame1 
         Height          =   3660
         Index           =   0
         Left            =   240
         TabIndex        =   48
         Top             =   480
         Width           =   7212
         _Version        =   65536
         _ExtentX        =   12721
         _ExtentY        =   6456
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
         Begin VB.CommandButton cmdEstKd 
            Caption         =   "Soil class - WP-CLASS"
            Height          =   312
            Left            =   121
            TabIndex        =   23
            Tag             =   "wpclass"
            Top             =   363
            Width           =   6855
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   1
            Left            =   4100
            TabIndex        =   25
            Tag             =   "wpsand"
            Top             =   836
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
            Height          =   264
            ItemData        =   "Vadose.frx":04D4
            Left            =   2760
            List            =   "Vadose.frx":04FC
            Style           =   2  'Dropdown List
            TabIndex        =   83
            Top             =   363
            Visible         =   0   'False
            Width           =   4212
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   2
            Left            =   4100
            TabIndex        =   29
            Tag             =   "wpsilt"
            Top             =   1199
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   3
            Left            =   4100
            TabIndex        =   33
            Tag             =   "wpclay"
            Top             =   1562
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   4
            Left            =   4100
            TabIndex        =   37
            Tag             =   "wpomc"
            Top             =   1914
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   5
            Left            =   4100
            TabIndex        =   41
            Tag             =   "wpiron"
            Top             =   2277
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   16
            Left            =   4100
            TabIndex        =   45
            Tag             =   "wpsoilcoef"
            Top             =   2640
            Width           =   1000
         End
         Begin VB.Label Label19 
            Caption         =   "* The percent of sand, silt, clay, organic matter, and iron must add up to 100%"
            Height          =   264
            Left            =   121
            TabIndex        =   47
            Top             =   3025
            Width           =   5863
         End
         Begin VB.Label Label3 
            Caption         =   "Percentage of sand - WP-SAND *"
            Height          =   264
            Left            =   121
            TabIndex        =   24
            Top             =   836
            Width           =   3806
         End
         Begin VB.Label Label4 
            Caption         =   "Percentage of silt - WP-SILT *"
            Height          =   264
            Left            =   121
            TabIndex        =   28
            Top             =   1199
            Width           =   3806
         End
         Begin VB.Label Label5 
            Caption         =   "Percentage of clay - WP-CLAY *"
            Height          =   264
            Left            =   121
            TabIndex        =   32
            Top             =   1562
            Width           =   3806
         End
         Begin VB.Label Label6 
            Caption         =   "Percentage of organic matter - WP-OMC *"
            Height          =   264
            Left            =   121
            TabIndex        =   36
            Top             =   1914
            Width           =   3806
         End
         Begin VB.Label Label7 
            Caption         =   "Percentage of iron and aluminum - WP-IRON *"
            Height          =   385
            Left            =   121
            TabIndex        =   40
            Top             =   2277
            Width           =   3806
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   253
            Index           =   1
            Left            =   5764
            TabIndex        =   27
            Tag             =   "0"
            Top             =   836
            Width           =   1001
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   253
            Index           =   2
            Left            =   5764
            TabIndex        =   31
            Tag             =   "0"
            Top             =   1199
            Width           =   1001
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   253
            Index           =   3
            Left            =   5764
            TabIndex        =   35
            Tag             =   "0"
            Top             =   1562
            Width           =   1001
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   253
            Index           =   4
            Left            =   5764
            TabIndex        =   39
            Tag             =   "0"
            Top             =   1914
            Width           =   1001
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   253
            Index           =   5
            Left            =   5764
            TabIndex        =   43
            Tag             =   "0"
            Top             =   2277
            Width           =   1001
         End
         Begin VB.Label Label1 
            Caption         =   "%"
            Height          =   264
            Left            =   5148
            TabIndex        =   26
            Top             =   836
            Width           =   187
         End
         Begin VB.Label Label27 
            Caption         =   "%"
            Height          =   264
            Left            =   5148
            TabIndex        =   30
            Top             =   1199
            Width           =   187
         End
         Begin VB.Label Label28 
            Caption         =   "%"
            Height          =   264
            Left            =   5148
            TabIndex        =   34
            Top             =   1562
            Width           =   187
         End
         Begin VB.Label Label32 
            Caption         =   "%"
            Height          =   264
            Left            =   5148
            TabIndex        =   38
            Top             =   1914
            Width           =   187
         End
         Begin VB.Label Label33 
            Caption         =   "%"
            Height          =   264
            Left            =   5148
            TabIndex        =   42
            Top             =   2277
            Width           =   187
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   253
            Index           =   16
            Left            =   5764
            TabIndex        =   46
            Tag             =   "0"
            Top             =   2640
            Width           =   1001
         End
         Begin VB.Label Label46 
            Caption         =   "Soil type coefficient - WP-SOILCOEF"
            Height          =   264
            Left            =   121
            TabIndex        =   44
            Top             =   2640
            Width           =   3806
         End
      End
      Begin VB.Timer Timer1 
         Interval        =   100
         Left            =   0
         Top             =   0
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
         Caption         =   "How to..."
      End
      Begin VB.Menu about 
         Caption         =   "&About"
      End
   End
End
Attribute VB_Name = "Vadose"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text


Dim temp As parmrec
Dim loadng As Boolean
Dim oldParent As String
Dim oldProgeny As String
Dim uStr(HALF_LIFE) As String
Dim hStr(2) As String

'Added flags to control the behaviour of the added parms combo box
Dim PrevSelectedIdx As Long
Dim PrevSelectedProgIdx As Long

' All these just ro resolve indices from fui to mui
Dim f_numcon As Long

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
    answer = MsgBox("Do you want to save changes?", 51, App.title)
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
  
  Select Case ctlList.name
    Case "ConParms", "ProgParms"
      Select Case ctlList.ListIndex
        Case 0
          getListParmName = "WASUBKD"
        Case 1
          If con(cboParent.ItemData(cboParent.ListIndex)).rad Then
            getListParmName = "WPSOL-RAD"
          Else
            getListParmName = "WPSOL"
          End If
        Case 2
          getListParmName = "WPGHALF"
      End Select
  End Select
End Function

Function getUnitType(ParmName As String) As String
  ' fail safe return value
  getUnitType = ""
  
  Select Case ParmName
    Case "WASUBKD"
      getUnitType = "LiquidVolume/Mass"
    Case "WPSOL"
      getUnitType = "Mass/LiquidVolume"
    Case "WPSOL-RAD"
      getUnitType = "Activity/LiquidVolume"
    Case "WPGHALF"
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
'end format menu

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
      If j > 0 Then
        model(i).progeny(j).param(ProgParms.ListIndex).value = Empty
        model(i).progeny(j).param(ProgParms.ListIndex).uunt = ""
      End If
    Else
      txt(PROGENY_PARAMS).Text = temp
      If j > 0 Then
        model(i).progeny(j).param(ProgParms.ListIndex).value = temp
        model(i).progeny(j).param(ProgParms.ListIndex).uunt = unit(PROGENY_PARAMS).Tag
      End If
    End If
    model(i).progeny(j).param(ProgParms.ListIndex).ref = ref(PROGENY_PARAMS).Tag
  End If
  
  SSCommand8.Enabled = (ProgKds.ListIndex > SELECT_LOOKUP)

End Sub

Sub selref_Click()
  RefMode = 0
  GetRef ref(RefItem)
End Sub

Sub addref_Click()
  RefMode = 1
  GetRef ref(RefItem)
End Sub

Private Sub auto_Click()
  auto.Checked = Not auto.Checked
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
  Dim i As Long, j As Long, k As Long, m As Long
  Dim gml As Double
  Dim spa As Double
  Dim prm As String
  Dim prefix As String
  Dim fle As parmfile
  Dim sval As Boolean
  f_numcon = 0
  m_numcon = 0
  If open_parm(fle, FUIName, 2) Then
    Do Until EOCF(fle.file)
      If read_parmrec(fle, temp) Then
        Select Case temp.pname
          Case "fui"
            For m = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                If temp.idx1 = siteIdx Then
                  Select Case temp.pname
                    Case "vaddespath"
                      If temp.idx1 = siteIdx And temp.idx2 = modIdx Then DesName = temp.pval
                    Case "numcon"
                      f_numcon = Val(temp.pval)
                      ReDim Preserve con(f_numcon) As contam_param
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
                    Case "fscname"
                        If temp.idx2 > f_numcon Then
                          f_numcon = temp.idx2
                          ReDim Preserve con(f_numcon) As contam_param
                        End If
                        If temp.idx3 = 0 Then
                          con(temp.idx2).name = temp.pval
                        Else
                          If temp.idx3 > con(temp.idx2).numprog Then
                            con(temp.idx2).numprog = temp.idx3
                            ReDim Preserve con(temp.idx2).progeny(temp.idx3) As contam_progeny_param
                          End If
                          con(temp.idx2).progeny(temp.idx3).name = temp.pval
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
            
          Case ModName
            Loading.Gauge1.Max = Val(temp.idx1)
            Loading.Gauge1.value = 0
            For m = 1 To temp.idx1
              Loading.update
              If read_parmrec(fle, temp) Then
                Select Case temp.pname
                  Case "CVTFormat":       CVTFormat = "General Number" 'CVTFormat = temp.pval
                  Case "tfinal":          SetTFinal temp
                  Case "ntimes":          SetNTimes temp
                  Case "wpclass":         Combo1.ListIndex = Val(temp.pval)
                  Case "wpsand":          fillet 1
                  Case "wpsilt":          fillet 2
                  Case "wpclay":          fillet 3
                  Case "wpomc":           fillet 4
                  Case "wpiron":          fillet 5
                  Case "wpph":            fillet 6
                  Case "wptotpor":
                                          If temp.cunit = "N/A" Then            'backward compatible to old unit
                                            temp.cunit = "percent"
                                            temp.uunit = "percent"
                                          End If
                                          fillet 7
                  Case "wpfieldc":
                                          If temp.cunit = "N/A" Then            'backward compatible to old unit
                                            temp.cunit = "percent"
                                            temp.uunit = "percent"
                                          End If
                                          fillet 8
                  Case "wpconduc":        fillet 9
                  Case "wpthick":         fillet 10
                  Case "wpbulk":          fillet 11
                  Case "wpldisp":         fillet 17
                  Case "wpsoilcoef":      fillet 16
                  Case "vadcasid"
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
                  Case "wasubkd", "wpsol", "wprsol", "wpghalf"
                     Select Case temp.pname
                        Case "wasubkd": i = KD_IDX
                        Case "wpsol": i = WATER_SOLUBILITY_IDX  'separate because of units
                        Case "wprsol": i = WATER_SOLUBILITY_IDX  'separate because of units
                        Case "wpghalf": i = HALF_LIFE_IDX
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
            model(j).param(KD_IDX).uunt = uStr(SOIL_KD)
            model(j).param(KD_IDX).punt = uStr(SOIL_KD)
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
            model(j).param(HALF_LIFE_IDX).uunt = uStr(HALF_LIFE)
            model(j).param(HALF_LIFE_IDX).punt = uStr(HALF_LIFE)
            model(j).param(HALF_LIFE_IDX).ref = 0
          End If
          Exit For
        End If
      Next j
      If j > m_numcon Then  ' new constituent
        m_numcon = j
        ReDim Preserve model(j) As model_contam_param
        model(j).cas = con(i).cas
        model(j).idx = i
        model(j).param(KD_IDX).value = Empty
        model(j).param(KD_IDX).punt = uStr(SOIL_KD)
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
        cboParent.AddItem con(model(i).idx).name
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
                model(i).progeny(k).param(KD_IDX).punt = uStr(SOIL_KD)
                model(i).progeny(k).param(KD_IDX).uunt = uStr(SOIL_KD)
              End If
              If model(i).progeny(k).param(WATER_SOLUBILITY_IDX).value = "" Or model(i).progeny(k).param(WATER_SOLUBILITY_IDX).value = Empty Then
                model(i).progeny(k).param(WATER_SOLUBILITY_IDX).value = con(model(i).idx).progeny(j).param(WATER_SOLUBILITY_IDX)
                If con(model(i).idx).rad Then
                  model(i).progeny(k).param(WATER_SOLUBILITY_IDX).punt = uStr(RAD_SOLUBILITY)
                  model(i).progeny(k).param(WATER_SOLUBILITY_IDX).uunt = uStr(RAD_SOLUBILITY)
                Else
                  model(i).progeny(k).param(WATER_SOLUBILITY_IDX).punt = uStr(SOLUBILITY)
                  model(i).progeny(k).param(WATER_SOLUBILITY_IDX).uunt = uStr(SOLUBILITY)
                End If
              End If
              If model(i).progeny(k).param(HALF_LIFE_IDX).value = "" Or model(i).progeny(k).param(HALF_LIFE_IDX).value = Empty Then
                model(i).progeny(k).param(HALF_LIFE_IDX).value = con(model(i).idx).progeny(j).param(HALF_LIFE_IDX)
                model(i).progeny(k).param(HALF_LIFE_IDX).punt = uStr(HALF_LIFE)
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
            model(i).progeny(k).param(KD_IDX).punt = uStr(SOIL_KD)
            model(i).progeny(k).param(KD_IDX).uunt = uStr(SOIL_KD)
            model(i).progeny(k).param(KD_IDX).ref = 0
            model(i).progeny(k).param(WATER_SOLUBILITY_IDX).value = con(model(i).idx).progeny(j).param(WATER_SOLUBILITY_IDX)
            If con(model(i).idx).rad Then
              model(i).progeny(k).param(WATER_SOLUBILITY_IDX).punt = uStr(RAD_SOLUBILITY)
              model(i).progeny(k).param(WATER_SOLUBILITY_IDX).uunt = uStr(RAD_SOLUBILITY)
            Else
              model(i).progeny(k).param(WATER_SOLUBILITY_IDX).punt = uStr(SOLUBILITY)
              model(i).progeny(k).param(WATER_SOLUBILITY_IDX).uunt = uStr(SOLUBILITY)
            End If
            model(i).progeny(k).param(WATER_SOLUBILITY_IDX).ref = 0
            model(i).progeny(k).param(HALF_LIFE_IDX).value = con(model(i).idx).progeny(j).param(HALF_LIFE_IDX)
            model(i).progeny(k).param(HALF_LIFE_IDX).punt = uStr(HALF_LIFE)
            model(i).progeny(k).param(HALF_LIFE_IDX).uunt = uStr(HALF_LIFE)
            model(i).progeny(k).param(HALF_LIFE_IDX).ref = 0
          End If
        Next j
      End If
    Next i
  Else
    PutError "Can't find or open file " & FUIName
    EndModule
  End If
End Sub

Private Sub Form_load()
Dim i As Long
  
  uStr(SOIL_KD) = "ml/g"
  uStr(RAD_SOLUBILITY) = "pCi/ml"
  uStr(SOLUBILITY) = "mg/l"  'final conversion to g/ml in the preprocessor to radcon
  uStr(HALF_LIFE) = "day"
  
  hStr(0) = "WASUBKD"
  hStr(1) = "WPSOL"
  hStr(2) = "WPGHALF"
  
  StartModule Me, App.title, 5
  SetHelpFile App.Path + "\vad.htm"
  SetRefFile ReplaceExt(FUIName, "ref")
  load_kd
  'set conversion comboboxes
  For i = 7 To 17
    If i = 14 Then i = 17
    get_conversion_items unit(i).Tag, unit(i)
  Next
  SetTimeTitle Me.Caption
  Loading.Show
  loadng = True
  loadprm
  BackFillParent
  PrevSelectedIdx = 0
  PrevSelectedProgIdx = 0
  ConParms.ListIndex = 0
  ProgParms.ListIndex = 0
  loadng = False
  Unload Loading
  Refresh
  vfProcessUserInput = True
End Sub

Private Sub save_Click()
  Dim i As Long
  Dim j As Long
  Dim fname As String
  Dim parm As parmrec
  Dim fle As parmfile
  Dim UserFormat As String
  UserFormat = CVTFormat

  txt_LostFocus RefItem
  fname = RunName & ".GID"
  If open_parm(fle, fname, 1) Then
    WriteTime fle
    set_parm parm, "CVTFormat", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CVTFormat
    write_parmrec fle, parm
    CVTFormat = "General Number"
    For i = 1 To 11
       If er(i) Then PutError "Parameter " & txt(i).Tag & " is invalid"
    Next
    set_parm parm, "wpclass", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", Combo1.ListIndex
    write_parmrec fle, parm
    For i = 1 To 6
      set_parm parm, txt(i).Tag, 0, 0, 0, 0, 0, 0, ref(i).Tag, "N/A", "N/A", txt(i).Text
      write_parmrec fle, parm
    Next
    For i = 7 To 11
      set_parm parm, txt(i).Tag, 0, 0, 0, 0, 0, 0, ref(i).Tag, unit(i).Text, unit(i).Tag, convert(unit(i).Text, unit(i).Tag, Val(txt(i)))
      write_parmrec fle, parm
    Next
    set_parm parm, txt(16).Tag, 0, 0, 0, 0, 0, 0, ref(16).Tag, "N/A", "N/A", txt(16).Text
    write_parmrec fle, parm
    If er(16) Then PutError "Parameter " & txt(17).Tag & " is invalid"
    set_parm parm, txt(17).Tag, 0, 0, 0, 0, 0, 0, ref(17).Tag, unit(17).Text, unit(17).Tag, convert(unit(17).Text, unit(17).Tag, Val(txt(17)))
    write_parmrec fle, parm
    If er(17) Then PutError "Parameter " & txt(16).Tag & " is invalid"
    
    'Contaminant kd's
    For i = 1 To m_numcon
      With model(i)
        If .idx <> 0 Then
          set_parm parm, "vadcasid", .idx, 0, 0, 0, 0, 0, .param(KD_IDX).ref, "N/A", "N/A", .cas
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
              PutError "Missing RAD water solubility (wprsol) value for " & .cas & " on Constituent Properties tab"
            Else
              PutError "Missing water solubility (wpsol) value for " & .cas & " on Constituent Properties tab"
            End If
          Else
            If .param(WATER_SOLUBILITY_IDX).value = Empty Or .param(WATER_SOLUBILITY_IDX).value < 0 Then
              If con(.idx).rad Then
                PutError "Invalid RAD water solubility (wprsol) value for " & model(i).cas & " on Constituent Properties tab"
                set_parm parm, "wprsol", .idx, 0, 0, 0, 0, 0, .param(WATER_SOLUBILITY_IDX).ref, .param(WATER_SOLUBILITY_IDX).uunt, uStr(RAD_SOLUBILITY), "EMPTY"
              Else
                PutError "Invalid water solubility (wpsol) value for " & model(i).cas & " on Constituent Properties tab"
                set_parm parm, "wpsol", .idx, 0, 0, 0, 0, 0, .param(WATER_SOLUBILITY_IDX).ref, .param(WATER_SOLUBILITY_IDX).uunt, uStr(SOLUBILITY), "EMPTY"
              End If
            Else
              If con(.idx).rad Then
                set_parm parm, "wprsol", .idx, 0, 0, 0, 0, 0, .param(WATER_SOLUBILITY_IDX).ref, .param(WATER_SOLUBILITY_IDX).uunt, uStr(RAD_SOLUBILITY), convert(.param(WATER_SOLUBILITY_IDX).uunt, uStr(RAD_SOLUBILITY), Val(.param(WATER_SOLUBILITY_IDX).value))
              Else
                set_parm parm, "wpsol", .idx, 0, 0, 0, 0, 0, .param(WATER_SOLUBILITY_IDX).ref, .param(WATER_SOLUBILITY_IDX).uunt, uStr(SOLUBILITY), convert(.param(WATER_SOLUBILITY_IDX).uunt, uStr(SOLUBILITY), Val(.param(WATER_SOLUBILITY_IDX).value))
              End If
            End If
            write_parmrec fle, parm
          End If
          
          If Len(.param(HALF_LIFE_IDX).value) = 0 Then
            PutError "Missing half-life (WPGHALF) value for " & .cas & " on Constituent Properties tab"
          Else
            If .param(HALF_LIFE_IDX).value = Empty Or Val(.param(HALF_LIFE_IDX).value) <= 0 Then
              PutError "Invalid half-life (WPGHALF) value for " & .cas & " on Constituent Properties tab"
              set_parm parm, "wpghalf", .idx, 0, 0, 0, 0, 0, .param(HALF_LIFE_IDX).ref, .param(HALF_LIFE_IDX).uunt, uStr(HALF_LIFE), "EMPTY"
            Else
              set_parm parm, "wpghalf", .idx, 0, 0, 0, 0, 0, .param(HALF_LIFE_IDX).ref, .param(HALF_LIFE_IDX).uunt, uStr(HALF_LIFE), convert(.param(HALF_LIFE_IDX).uunt, uStr(HALF_LIFE), Val(.param(HALF_LIFE_IDX).value))
            End If
            write_parmrec fle, parm
          End If
        
          For j = 1 To .numprog
            With .progeny(j)
              If .idx <> 0 Then
                set_parm parm, "vadcasid", model(i).idx, .idx, 0, 0, 0, 0, .param(KD_IDX).ref, "N/A", "N/A", .cas
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
                    PutError "Missing RAD water solubility (wprsol) value for " & .cas & " daughter of " & model(i).cas & " on Constituent Properties tab"
                  Else
                    PutError "Missing water solubility (wpsol) value for " & .cas & " daughter of " & model(i).cas & " on Constituent Properties tab"
                  End If
                Else
                  If .param(WATER_SOLUBILITY_IDX).value = Empty Or Val(.param(WATER_SOLUBILITY_IDX).value) < 0 Then
                    If con(model(i).idx).rad Then
                      PutError "Invalid RAD water solubility (wprsol) value for " & .cas & " daughter of " & model(i).cas & " on Constituent Properties tab"
                      set_parm parm, "wprsol", model(i).idx, .idx, 0, 0, 0, 0, .param(WATER_SOLUBILITY_IDX).ref, .param(WATER_SOLUBILITY_IDX).uunt, uStr(RAD_SOLUBILITY), "EMPTY"
                    Else
                      PutError "Invalid water solubility (wpsol) value for " & .cas & " daughter of " & model(i).cas & " on Constituent Properties tab"
                      set_parm parm, "wpsol", model(i).idx, .idx, 0, 0, 0, 0, .param(WATER_SOLUBILITY_IDX).ref, .param(WATER_SOLUBILITY_IDX).uunt, uStr(SOLUBILITY), "EMPTY"
                    End If
                  Else
                    If con(model(i).idx).rad Then
                      set_parm parm, "wprsol", model(i).idx, .idx, 0, 0, 0, 0, .param(WATER_SOLUBILITY_IDX).ref, .param(WATER_SOLUBILITY_IDX).uunt, uStr(RAD_SOLUBILITY), convert(.param(WATER_SOLUBILITY_IDX).uunt, uStr(RAD_SOLUBILITY), Val(.param(WATER_SOLUBILITY_IDX).value))
                    Else
                      set_parm parm, "wpsol", model(i).idx, .idx, 0, 0, 0, 0, .param(WATER_SOLUBILITY_IDX).ref, .param(WATER_SOLUBILITY_IDX).uunt, uStr(SOLUBILITY), convert(.param(WATER_SOLUBILITY_IDX).uunt, uStr(SOLUBILITY), Val(.param(WATER_SOLUBILITY_IDX).value))
                    End If
                  End If
                  write_parmrec fle, parm
                End If
                
                If Len(.param(HALF_LIFE_IDX).value) = 0 Then
                  PutError "Missing half-life (WPGHALF) value for " & .cas & " daughter of " & model(i).cas & " on Constituent Properties tab"
                Else
                  If .param(HALF_LIFE_IDX).value = Empty Or Val(.param(HALF_LIFE_IDX).value) <= 0 Then
                    PutError "Invalid half-life (WPGHALF) value for " & .cas & " daughter of " & model(i).cas & " on Constituent Properties tab"
                    set_parm parm, "wpghalf", model(i).idx, .idx, 0, 0, 0, 0, .param(HALF_LIFE_IDX).ref, .param(HALF_LIFE_IDX).uunt, uStr(HALF_LIFE), "EMPTY"
                  Else
                    set_parm parm, "wpghalf", model(i).idx, .idx, 0, 0, 0, 0, .param(HALF_LIFE_IDX).ref, .param(HALF_LIFE_IDX).uunt, uStr(HALF_LIFE), convert(.param(HALF_LIFE_IDX).uunt, uStr(HALF_LIFE), Val(.param(HALF_LIFE_IDX).value))
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

Private Sub SSCommand1_Click()
  If cboProgeny.ListIndex > 0 Then cboProgeny.ListIndex = cboProgeny.ListIndex - 1
End Sub

Private Sub SSCommand2_Click()
  If cboProgeny.ListIndex < cboProgeny.ListCount - 1 Then cboProgeny.ListIndex = cboProgeny.ListIndex + 1
End Sub

Private Sub SSCommand4_Click()
  If cboParent.ListIndex > 0 Then cboParent.ListIndex = cboParent.ListIndex - 1
End Sub

Private Sub SSCommand5_Click()
  If cboParent.ListIndex < cboParent.ListCount - 1 Then cboParent.ListIndex = cboParent.ListIndex + 1
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

Private Sub SSCommand7_Click()
  Dim i As Long
  Dim kdee As Double
  
  If Not KD_loaded Then load_kd
  
  If ConKds.ListIndex > SELECT_LOOKUP Then
    For i = 1 To m_numcon
      kdee = getKd(con(model(i).idx).name, ConKds.ListIndex)
      If kdee < 0 Then
        MsgBox "The " & getEstKdDesc(ConKds.ListIndex) & " is not valid for " & con(model(i).idx).name & " at this time", vbInformation, "Estimate Kd"
      Else
        model(i).param(ConParms.ListIndex).value = kdee
        model(i).param(ConParms.ListIndex).uunt = unit(CONTAM_PARAMS).Tag
        model(i).param(ConParms.ListIndex).ref = ref(CONTAM_PARAMS).Tag
      End If
    Next
    ConKds_Click
  End If
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
          kdee = getKd(con(.idx).name, ProgKds.ListIndex, con(.idx).progeny(.progeny(j).idx).name)
          With .progeny(j)
            If kdee < 0 Then
              MsgBox "The " & getEstKdDesc(ProgKds.ListIndex) & " is not valid for " & con(model(i).idx).progeny(.idx).name & " daughter product of " & con(model(i).idx).name & " at this time", vbInformation, "Estimate Kd"
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
  
  If SSTab1.Tab = 2 And cboParent.ListIndex = -1 Then SSCommand5_Click
  mes = ""
End Sub

Private Sub Timer1_Timer()
  mes = Mid(mes, 2) & Mid(mes, 1, 1)
End Sub

Private Sub unit_Click(Index As Integer)
  er CLng(Index)
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
    Case CONTAM_PARAMS
      If ConParms.ListIndex = HALF_LIFE_IDX Then
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
    Case 1 To 5
      m = "Value must be between 0 and 100 inclusive"
      If (tval < 0 Or tval > 100) Then er = True
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
    Case 16
      m = "Value must be between 0 and 15"
      If (tval < 0 Or tval > 15) Then er = True
    Case 17
      t1 = convert(unit(10).Text, unit(17).Text, Val(txt(10).Text))
      m = "Value must be between 0 and WP-THICK"
      If (tval < 0 Or tval > t1) Then er = True
    Case 9
      m = "Value must be greater than 0."
      If (tval <= 0) Then er = True
    Case 10
      t1 = convert(unit(Index).Tag, unit(Index).Text, 0.1)
      t2 = convert(unit(Index).Tag, unit(Index).Text, 304800)
      m = "Value must be between " + t1 + " and " + t2 + " " + unit(Index).Text
      If (tval < Val(t1) Or tval > Val(t2)) Then er = True
    Case 11
      t1 = convert(unit(Index).Tag, unit(Index).Text, 1#)
      t2 = convert(unit(Index).Tag, unit(Index).Text, 3#)
      m = "Value must be between " + t1 + " and " + t2 + " " + unit(Index).Text
      If (tval < Val(t1) Or tval > Val(t2)) Then er = True
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
    With model(i).param(PrevSelectedIdx)
      If txt(CONTAM_PARAMS).Text = "" Then
        .value = Empty
      Else
        .value = txt(CONTAM_PARAMS).Text
      End If
      .uunt = unit(CONTAM_PARAMS).Text
      .ref = Val(ref(CONTAM_PARAMS).Tag)
      
      'fill product params
      FillParent model(i).cas, PrevSelectedIdx, .value, .uunt, .ref
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
    With model(i).progeny(j).param(PrevSelectedProgIdx)
      If txt(PROGENY_PARAMS).Text = "" Then
        .value = Empty
      Else
        .value = txt(PROGENY_PARAMS).Text
      End If
      .uunt = unit(PROGENY_PARAMS).Text
      .ref = Val(ref(PROGENY_PARAMS).Tag)
      'fill parent params
      FillParent model(i).progeny(j).cas, PrevSelectedProgIdx, .value, .uunt, .ref
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
      Case KD_IDX:        KdListUpdate
      Case HALF_LIFE_IDX
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

Private Sub changetxt(i As Long, j As Long, k As Long, f1 As Double, f2 As Double, f3 As Double, f4 As Double, f5 As Double)
  If Not loadng Then
    vfProcessUserInput = False
    txt(fSAND).Text = i
    txt(fSILT).Text = j
    txt(fCLAY).Text = k
    txt(fOMC).Text = 0
    txt(fIRON).Text = 0
    If auto.Checked Then
      set_unit unit(9), unit(9).Tag
      set_unit unit(11), unit(11).Tag
      txt(9).Text = f1 * 86400
      txt(7).Text = f2
      txt(11).Text = f3
      txt(8).Text = f4
      txt(16).Text = f5
    End If
    vfProcessUserInput = True
  End If
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

Private Sub cboProgeny_Click()
  GetModelProgeny
  PutModelProgeny
  oldProgeny = cboProgeny.Text
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
      Command3.Visible = False
      Command4.Visible = False
      SSCommand1.Visible = False
      SSCommand2.Visible = False
      SSCommand8.Visible = False
      Label16.Visible = False
      Label17.Visible = False
      cboProgeny.Visible = False
      ProgParms.Visible = False
      ProgKds.Visible = False
    Else
      For j = 1 To model(i).numprog
        If model(i).progeny(j).idx > 0 Then
          cboProgeny.AddItem con(model(i).idx).progeny(model(i).progeny(j).idx).name
          cboProgeny.ItemData(cboProgeny.NewIndex) = j
        End If
      Next
      txt(PROGENY_PARAMS).Visible = True
      unit(PROGENY_PARAMS).Visible = True
      ref(PROGENY_PARAMS).Visible = True
      Command3.Visible = True
      Command4.Visible = True
      SSCommand1.Visible = True
      SSCommand2.Visible = True
      SSCommand8.Visible = (ProgParms.ListIndex = 0)
      SSCommand8.Enabled = (ProgKds.ListIndex > SELECT_LOOKUP)
      Label16.Visible = True
      Label17.Visible = True
      cboProgeny.Visible = True
      oldProgeny = ""
      cboProgeny.ListIndex = 0
      ProgParms.Visible = True
      ProgKds.Visible = (ProgParms.ListIndex = 0)
    End If
  End If
End Sub

Private Sub ConParms_Click()
  Dim check As Boolean
  
  If loadng Then Exit Sub
  cboParent_Click
  PrevSelectedIdx = ConParms.ListIndex
  
  check = (ConParms.ListIndex = 0)
  ConKds.Visible = check
  SSCommand7.Visible = check
  SSCommand7.Enabled = (ConKds.ListIndex > SELECT_LOOKUP)
  HelpAnchor = hStr(ConParms.ListIndex)
  txt(CONTAM_PARAMS).Tag = HelpAnchor
End Sub

Private Sub ProgParms_Click()
  Dim check As Boolean
  
  If loadng Then Exit Sub
  cboProgeny_Click
  PrevSelectedProgIdx = ProgParms.ListIndex
  
  check = (ProgParms.ListIndex = 0)
  ProgKds.Visible = check
  Label2.Visible = check
  SSCommand8.Visible = check
  SSCommand8.Enabled = (ProgKds.ListIndex > SELECT_LOOKUP)
  HelpAnchor = hStr(ProgParms.ListIndex)
  txt(PROGENY_PARAMS).Tag = HelpAnchor
End Sub

Private Sub txt_LostFocus(Index As Integer)
  If Index = CONTAM_PARAMS Then
    cboParent_Click
  End If
  If Index = PROGENY_PARAMS Then
    cboProgeny_Click
  End If
End Sub

Private Sub txt_GotFocus(Index As Integer)
  er CLng(Index)
  RefItem = Index
  noact.Enabled = True
  HelpAnchor = txt(Index).Tag
End Sub

Private Sub unit_GotFocus(Index As Integer)
  er CLng(Index)
  RefItem = Index
  noact.Enabled = True
  HelpAnchor = txt(Index).Tag
End Sub

Private Sub SSTab1_GotFocus()
  mes = ""
  noact.Enabled = False
  Select Case SSTab1.Tab
  Case 0: HelpAnchor = "COMPOSITION"
  Case 1: HelpAnchor = "CHARACTERISTICS"
  Case 2: HelpAnchor = "PROPERTIES"
  End Select
End Sub

Private Sub Combo1_GotFocus()
Dim m As String
  m = "The percent of sand, silt, clay, organic matter and iron must add up to 100%"
  mes = Space(140 - Len(m)) & m
  noact.Enabled = False
  HelpAnchor = Combo1.Tag
End Sub

Private Sub cboProgeny_GotFocus()
Dim m As String
  m = "Select a progeny"
  mes = Space(140 - Len(m)) & m
  RefItem = PROGENY_PARAMS
  noact.Enabled = True
  HelpAnchor = cboProgeny.Tag
End Sub

Private Sub cboParent_GotFocus()
Dim m As String
  m = "Select a constituent"
  mes = Space(140 - Len(m)) & m
  RefItem = CONTAM_PARAMS
  noact.Enabled = True
  HelpAnchor = cboParent.Tag
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

Private Sub SSCommand1_GotFocus()
  cboProgeny_GotFocus
End Sub

Private Sub SSCommand2_GotFocus()
  cboProgeny_GotFocus
End Sub

Private Sub SSCommand4_GotFocus()
  cboParent_GotFocus
End Sub

Private Sub SSCommand5_GotFocus()
  cboParent_GotFocus
End Sub

Private Sub ConsKds_GotFocus()
  cboParent_GotFocus
End Sub

Private Sub ProgKds_GotFocus()
  cboProgeny_GotFocus
End Sub

Private Sub SSCommand7_GotFocus()
  cboParent_GotFocus
End Sub

Private Sub SSCommand8_GotFocus()
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

