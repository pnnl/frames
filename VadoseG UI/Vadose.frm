VERSION 5.00
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "tabctl32.ocx"
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "threed32.ocx"
Begin VB.Form Vadose 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "VadoseUI"
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
      TabIndex        =   80
      TabStop         =   0   'False
      Top             =   4320
      Width           =   7680
   End
   Begin TabDlg.SSTab SSTab1 
      Height          =   4332
      Left            =   0
      TabIndex        =   32
      Top             =   0
      Width           =   7692
      _ExtentX        =   13568
      _ExtentY        =   7641
      _Version        =   393216
      Style           =   1
      TabHeight       =   529
      TabCaption(0)   =   "Soil Composition"
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
      TabCaption(2)   =   "Constituent Parameters"
      TabPicture(2)   =   "Vadose.frx":0342
      Tab(2).ControlEnabled=   0   'False
      Tab(2).Control(0)=   "SSFrame1(2)"
      Tab(2).ControlCount=   1
      Begin Threed.SSFrame SSFrame1 
         Height          =   3660
         Index           =   1
         Left            =   -74760
         TabIndex        =   51
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
            TabIndex        =   85
            Tag             =   "percent"
            Top             =   1080
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   7
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   84
            Tag             =   "percent"
            Top             =   720
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   315
            Index           =   17
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   15
            Tag             =   "cm"
            Top             =   2040
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   17
            Left            =   4080
            TabIndex        =   14
            Tag             =   "wpldisp"
            Top             =   2040
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   315
            Index           =   11
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   17
            Tag             =   "g/cm^3"
            Top             =   2400
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   315
            Index           =   10
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   13
            Tag             =   "cm"
            Top             =   2760
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   9
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   11
            Tag             =   "cm/day"
            Top             =   1680
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   11
            Left            =   4080
            TabIndex        =   16
            Tag             =   "wpbulk"
            Top             =   2400
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   10
            Left            =   4080
            TabIndex        =   12
            Tag             =   "wpthick"
            Top             =   2760
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   9
            Left            =   4080
            TabIndex        =   10
            Tag             =   "wpconduc"
            Top             =   1680
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   8
            Left            =   4080
            TabIndex        =   9
            Tag             =   "wpfieldc"
            Top             =   1080
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   7
            Left            =   4080
            TabIndex        =   8
            Tag             =   "wptotpor"
            Top             =   720
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   6
            Left            =   4080
            TabIndex        =   7
            Tag             =   "wpph"
            Top             =   360
            Width           =   1000
         End
         Begin VB.Label Label18 
            Caption         =   "Longitudinal dispersivity - WP-LDISP"
            Height          =   270
            Left            =   120
            TabIndex        =   59
            Top             =   2040
            Width           =   3810
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   17
            Left            =   6120
            TabIndex        =   58
            Tag             =   "0"
            Top             =   2040
            Width           =   1005
         End
         Begin VB.Label Label35 
            Caption         =   "pH"
            Height          =   252
            Left            =   5160
            TabIndex        =   35
            Top             =   360
            Width           =   504
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   11
            Left            =   6120
            TabIndex        =   46
            Tag             =   "0"
            Top             =   2400
            Width           =   1005
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   255
            Index           =   10
            Left            =   6120
            TabIndex        =   44
            Tag             =   "0"
            Top             =   2760
            Width           =   1005
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   9
            Left            =   6120
            TabIndex        =   42
            Tag             =   "0"
            Top             =   1680
            Width           =   1000
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   8
            Left            =   6120
            TabIndex        =   40
            Tag             =   "0"
            Top             =   1080
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   7
            Left            =   6120
            TabIndex        =   38
            Tag             =   "0"
            Top             =   720
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   6
            Left            =   6120
            TabIndex        =   36
            Tag             =   "0"
            Top             =   360
            Width           =   1000
         End
         Begin VB.Label Label13 
            Caption         =   "Dry bulk density - WP-BULKD"
            Height          =   270
            Left            =   120
            TabIndex        =   45
            Top             =   2400
            Width           =   3810
         End
         Begin VB.Label Label12 
            Caption         =   "Thickness of this layer - WP-THICK"
            Height          =   270
            Left            =   120
            TabIndex        =   43
            Top             =   2760
            Width           =   3810
         End
         Begin VB.Label Label11 
            Caption         =   "Saturated hydraulic conductivity - WP-CONDUC"
            Height          =   264
            Left            =   120
            TabIndex        =   41
            Top             =   1680
            Width           =   3804
         End
         Begin VB.Label Label10 
            Caption         =   "Field capacity - WP-FIELDC"
            Height          =   264
            Left            =   120
            TabIndex        =   39
            Top             =   1080
            Width           =   3804
         End
         Begin VB.Label Label9 
            Caption         =   "Total porosity - WP-TOTPOR"
            Height          =   264
            Left            =   120
            TabIndex        =   37
            Top             =   720
            Width           =   3804
         End
         Begin VB.Label Label8 
            Caption         =   "pH of the pore water - WP-PH"
            Height          =   264
            Left            =   120
            TabIndex        =   34
            Top             =   360
            Width           =   3804
         End
      End
      Begin Threed.SSFrame SSFrame1 
         Height          =   3660
         Index           =   2
         Left            =   -74760
         TabIndex        =   52
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
         Begin VB.ComboBox ProgParms 
            Height          =   288
            ItemData        =   "Vadose.frx":035E
            Left            =   4080
            List            =   "Vadose.frx":036B
            Style           =   2  'Dropdown List
            TabIndex        =   83
            Top             =   2400
            Width           =   2892
         End
         Begin VB.ComboBox ConParms 
            Height          =   288
            ItemData        =   "Vadose.frx":03D3
            Left            =   4080
            List            =   "Vadose.frx":03E0
            Style           =   2  'Dropdown List
            TabIndex        =   82
            Top             =   720
            Width           =   2892
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   12
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   21
            Tag             =   "ml/g"
            Top             =   1080
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   13
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   28
            Tag             =   "ml/g"
            Top             =   2760
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   13
            Left            =   4080
            TabIndex        =   27
            Tag             =   "wasubkd"
            Top             =   2760
            Width           =   1000
         End
         Begin VB.ComboBox Combo2 
            Height          =   288
            Left            =   240
            Style           =   2  'Dropdown List
            TabIndex        =   25
            Top             =   2400
            Width           =   3012
         End
         Begin VB.TextBox txt 
            BackColor       =   &H00C0C0C0&
            Enabled         =   0   'False
            Height          =   312
            Index           =   15
            Left            =   4080
            TabIndex        =   53
            TabStop         =   0   'False
            Top             =   3120
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H00C0C0C0&
            Enabled         =   0   'False
            Height          =   312
            Index           =   14
            Left            =   4080
            TabIndex        =   33
            TabStop         =   0   'False
            Top             =   1440
            Width           =   1000
         End
         Begin VB.ComboBox Combo4 
            Height          =   288
            Left            =   240
            Style           =   2  'Dropdown List
            TabIndex        =   18
            Top             =   720
            Width           =   3012
         End
         Begin VB.TextBox txt 
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   12
            Left            =   4080
            TabIndex        =   20
            Tag             =   "wasubkd"
            Top             =   1080
            Width           =   1000
         End
         Begin Threed.SSCommand SSCommand8 
            Height          =   324
            Left            =   240
            TabIndex        =   31
            Top             =   2880
            Width           =   1212
            _Version        =   65536
            _ExtentX        =   2138
            _ExtentY        =   572
            _StockProps     =   78
            Caption         =   "Estimate All"
         End
         Begin Threed.SSCommand SSCommand3 
            Height          =   312
            Left            =   2040
            TabIndex        =   26
            Top             =   2880
            Width           =   1212
            _Version        =   65536
            _ExtentX        =   2138
            _ExtentY        =   550
            _StockProps     =   78
            Caption         =   "Use Estimate"
         End
         Begin Threed.SSCommand SSCommand2 
            Height          =   264
            Left            =   2880
            TabIndex        =   30
            Top             =   2100
            Width           =   372
            _Version        =   65536
            _ExtentX        =   656
            _ExtentY        =   466
            _StockProps     =   78
            Caption         =   ">>"
            Font3D          =   1
         End
         Begin Threed.SSCommand SSCommand1 
            Height          =   264
            Left            =   2520
            TabIndex        =   29
            Top             =   2100
            Width           =   372
            _Version        =   65536
            _ExtentX        =   656
            _ExtentY        =   466
            _StockProps     =   78
            Caption         =   "<<"
            Font3D          =   1
         End
         Begin Threed.SSCommand SSCommand4 
            Height          =   264
            Left            =   2520
            TabIndex        =   22
            Top             =   360
            Width           =   372
            _Version        =   65536
            _ExtentX        =   656
            _ExtentY        =   466
            _StockProps     =   78
            Caption         =   "<<"
            Font3D          =   1
         End
         Begin Threed.SSCommand SSCommand5 
            Height          =   270
            Left            =   2880
            TabIndex        =   23
            Top             =   360
            Width           =   375
            _Version        =   65536
            _ExtentX        =   656
            _ExtentY        =   466
            _StockProps     =   78
            Caption         =   ">>"
            Font3D          =   1
         End
         Begin Threed.SSCommand SSCommand6 
            Height          =   312
            Left            =   1980
            TabIndex        =   19
            Top             =   1200
            Width           =   1212
            _Version        =   65536
            _ExtentX        =   2138
            _ExtentY        =   550
            _StockProps     =   78
            Caption         =   "&Use Estimate"
         End
         Begin Threed.SSCommand SSCommand7 
            Height          =   324
            Left            =   240
            TabIndex        =   24
            Top             =   1200
            Width           =   1212
            _Version        =   65536
            _ExtentX        =   2138
            _ExtentY        =   572
            _StockProps     =   78
            Caption         =   "Estimate &All"
         End
         Begin VB.Label Label17 
            Caption         =   "Progeny Parameter Selection"
            Height          =   252
            Left            =   3840
            TabIndex        =   57
            Top             =   2100
            Width           =   3000
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   13
            Left            =   6120
            TabIndex        =   56
            Tag             =   "0"
            Top             =   2760
            Width           =   996
         End
         Begin VB.Label Label16 
            Caption         =   "Progeny - FS-CNAME"
            Height          =   252
            Left            =   120
            TabIndex        =   55
            Top             =   2100
            Width           =   1932
         End
         Begin VB.Label Label14 
            Caption         =   "ml/g"
            Height          =   252
            Left            =   5208
            TabIndex        =   54
            Top             =   3180
            Width           =   804
         End
         Begin VB.Label Label39 
            Caption         =   "ml/g"
            Height          =   252
            Left            =   5208
            TabIndex        =   50
            Top             =   1500
            Width           =   804
         End
         Begin VB.Label Label38 
            Caption         =   "Constituent - FS-CNAME"
            Height          =   252
            Left            =   120
            TabIndex        =   47
            Top             =   420
            Width           =   2112
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   12
            Left            =   6120
            TabIndex        =   49
            Tag             =   "0"
            Top             =   1080
            Width           =   996
         End
         Begin VB.Label Label40 
            Caption         =   "Constituent Parameter Selection"
            Height          =   252
            Left            =   3840
            TabIndex        =   48
            Top             =   360
            Width           =   3000
         End
      End
      Begin Threed.SSFrame SSFrame1 
         Height          =   3660
         Index           =   0
         Left            =   240
         TabIndex        =   60
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
         Begin VB.TextBox txt 
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   1
            Left            =   4080
            TabIndex        =   1
            Tag             =   "wpsand"
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
            ItemData        =   "Vadose.frx":0448
            Left            =   2760
            List            =   "Vadose.frx":0470
            Style           =   2  'Dropdown List
            TabIndex        =   0
            Tag             =   "wpclass"
            Top             =   480
            Width           =   4212
         End
         Begin VB.TextBox txt 
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   2
            Left            =   4068
            TabIndex        =   2
            Tag             =   "wpsilt"
            Top             =   1320
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   3
            Left            =   4068
            TabIndex        =   3
            Tag             =   "wpclay"
            Top             =   1680
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   4
            Left            =   4068
            TabIndex        =   4
            Tag             =   "wpomc"
            Top             =   2040
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   5
            Left            =   4068
            TabIndex        =   5
            Tag             =   "wpiron"
            Top             =   2400
            Width           =   1000
         End
         Begin VB.TextBox txt 
            BackColor       =   &H008080FF&
            Height          =   312
            Index           =   16
            Left            =   4080
            TabIndex        =   6
            Tag             =   "wpsoilcoef"
            Top             =   2760
            Width           =   1000
         End
         Begin VB.Label Label19 
            Caption         =   "* The percent of sand, silt, clay, organic matter, and iron must add up to 100%"
            Height          =   264
            Left            =   120
            TabIndex        =   81
            Top             =   3264
            Width           =   5868
         End
         Begin VB.Label Label2 
            Caption         =   "Soil class - WP-CLASS"
            Height          =   252
            Left            =   120
            TabIndex        =   79
            Top             =   480
            Width           =   2772
         End
         Begin VB.Label Label3 
            Caption         =   "Percentage of sand - WP-SAND *"
            Height          =   264
            Left            =   120
            TabIndex        =   78
            Top             =   960
            Width           =   3804
         End
         Begin VB.Label Label4 
            Caption         =   "Percentage of silt - WP-SILT *"
            Height          =   264
            Left            =   120
            TabIndex        =   77
            Top             =   1320
            Width           =   3804
         End
         Begin VB.Label Label5 
            Caption         =   "Percentage of clay - WP-CLAY *"
            Height          =   264
            Left            =   120
            TabIndex        =   76
            Top             =   1680
            Width           =   3804
         End
         Begin VB.Label Label6 
            Caption         =   "Percentage of organic matter - WP-OMC *"
            Height          =   264
            Left            =   120
            TabIndex        =   75
            Top             =   2040
            Width           =   3804
         End
         Begin VB.Label Label7 
            Caption         =   "Percentage of iron and aluminum - WP-IRON *"
            Height          =   384
            Left            =   120
            TabIndex        =   74
            Top             =   2400
            Width           =   3804
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   1
            Left            =   5760
            TabIndex        =   73
            Tag             =   "0"
            Top             =   960
            Width           =   1000
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   2
            Left            =   5760
            TabIndex        =   72
            Tag             =   "0"
            Top             =   1320
            Width           =   1000
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   3
            Left            =   5760
            TabIndex        =   71
            Tag             =   "0"
            Top             =   1680
            Width           =   1000
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   4
            Left            =   5760
            TabIndex        =   70
            Tag             =   "0"
            Top             =   2040
            Width           =   1000
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   5
            Left            =   5760
            TabIndex        =   69
            Tag             =   "0"
            Top             =   2400
            Width           =   1000
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
            Left            =   2760
            TabIndex        =   68
            Top             =   240
            Width           =   4212
         End
         Begin VB.Label Label1 
            Caption         =   "%"
            Height          =   264
            Left            =   5148
            TabIndex        =   67
            Top             =   960
            Width           =   192
         End
         Begin VB.Label Label27 
            Caption         =   "%"
            Height          =   264
            Left            =   5148
            TabIndex        =   66
            Top             =   1320
            Width           =   192
         End
         Begin VB.Label Label28 
            Caption         =   "%"
            Height          =   264
            Left            =   5148
            TabIndex        =   65
            Top             =   1680
            Width           =   192
         End
         Begin VB.Label Label32 
            Caption         =   "%"
            Height          =   264
            Left            =   5148
            TabIndex        =   64
            Top             =   2040
            Width           =   192
         End
         Begin VB.Label Label33 
            Caption         =   "%"
            Height          =   264
            Left            =   5148
            TabIndex        =   63
            Top             =   2400
            Width           =   192
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   16
            Left            =   5760
            TabIndex        =   62
            Tag             =   "0"
            Top             =   2760
            Width           =   996
         End
         Begin VB.Label Label46 
            Caption         =   "Soil type coefficient - WP-SOILCOEF"
            Height          =   264
            Left            =   120
            TabIndex        =   61
            Top             =   2760
            Width           =   3804
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
Dim ctext3 As String
Dim ctext4 As String
Dim kdn As Long
Dim uStr(3) As String

'Added flags to control the behaviour of the added parms combo box
Dim oldconidx As Long
Dim oldprogidx As Long

' All these just ro resolve indices from fui to mui
Dim f_numcon As Long, con() As contam
Dim m_numcon As Long, kd() As kdparm

Private Sub about_Click()
  frmAbout.Show 1, Vadose
End Sub

Private Sub advan_Click()
  Time.Show 1, Vadose
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

Sub selref_Click()
  RefMode = 0
  GetRef ref(RefItem)
End Sub

Sub addref_Click()
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
        txt(14).Text = temp
      Else
        txt(14).Text = 0.0001 * con(kd(i).idx).koc(0) * (57.735 * txt(4).Text + 2# * txt(3).Text + 0.4 * txt(2).Text + 0.005 * txt(1).Text)
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
        If con(kd(i).idx).progeny(kd(i).progeny(j).idx).name = Combo2.Text Then
          temp = get_kd(kd(i).progeny(j).cas, kdn)
          If temp > 0 Then
            txt(15).Text = temp
          Else
            txt(15).Text = 0.0001 * con(kd(i).idx).progeny(kd(i).progeny(j).idx).koc(0) * (57.735 * txt(4).Text + 2# * txt(3).Text + 0.4 * txt(2).Text + 0.005 * txt(1).Text)
          End If
          Exit For
        End If
      Next
      Exit For
    End If
  Next
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
  Dim i As Long, j As Long, k As Long, m As Long
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
                      DesName = temp.pval
                    Case "numcon"
                      f_numcon = Val(temp.pval)
                      ReDim Preserve con(f_numcon) As contam
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
              Loading.update
              If read_parmrec(fle, temp) Then
                Select Case temp.pname
                  Case "tfinal":  SetTFinal temp
                  Case "ntimes":  SetNTimes temp
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
                  Case "wpsoilcoef":     fillet 16
                  Case "vadcasid"
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
                  Case "wasubkd", "wpclsol", "wpclthalf"
                     If temp.pname = "wasubkd" Then i = 0
                     If temp.pname = "wpclsol" Then i = 1
                     If temp.pname = "wpclthalf" Then i = 2
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
  
  StartModule Vadose, App.Title, 5
  SetHelpFile App.Path + "\vad.htm"
  SetRefFile ReplaceExt(FUIName, "ref")
  load_kd
  'set conversion comboboxes
  For i = 7 To 17
    If i = 14 Then i = 17
    get_conversion_items unit(i).Tag, unit(i)
  Next
  SetTimeTitle Vadose.Caption
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
  Dim i As Long
  Dim j As Long
  Dim fname As String
  Dim parm As parmrec
  Dim fle As parmfile

  txt_LostFocus RefItem
  fname = RunName & ".GID"
  If open_parm(fle, fname, 1) Then
    WriteTime fle
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
    
    'Contaminate kd's
    For i = 1 To m_numcon
      If kd(i).idx <> 0 Then
        set_parm parm, "vadcasid", kd(i).idx, 0, 0, 0, 0, 0, kd(i).ref(0), "N/A", "N/A", kd(i).cas
        write_parmrec fle, parm
        If kd(i).kd(0) = Empty Then
          PutError "Parameter WASUBKD for " & kd(i).cas & " is invalid"
          set_parm parm, "wasubkd", kd(i).idx, 0, 0, 0, 0, 0, kd(i).ref(0), kd(i).uunt(0), uStr(0), "EMPTY"
        Else
          set_parm parm, "wasubkd", kd(i).idx, 0, 0, 0, 0, 0, kd(i).ref(0), kd(i).uunt(0), uStr(0), convert(kd(i).uunt(0), uStr(0), Val(kd(i).kd(0)))
        End If
        write_parmrec fle, parm
        
        If kd(i).kd(0) = Empty Or kd(i).kd(1) <= 0 Then
          PutError "Parameter WPCLSOL for " & kd(i).cas & " is invalid"
          If con(kd(i).idx).rad Then
            set_parm parm, "wpclsol", kd(i).idx, 0, 0, 0, 0, 0, kd(i).ref(1), kd(i).uunt(1), uStr(1), "EMPTY"
          Else
            set_parm parm, "wpclsol", kd(i).idx, 0, 0, 0, 0, 0, kd(i).ref(1), kd(i).uunt(1), uStr(2), "EMPTY"
          End If
        Else
          If con(kd(i).idx).rad Then
            set_parm parm, "wpclsol", kd(i).idx, 0, 0, 0, 0, 0, kd(i).ref(1), kd(i).uunt(1), uStr(1), convert(kd(i).uunt(1), uStr(1), Val(kd(i).kd(1)))
          Else
            set_parm parm, "wpclsol", kd(i).idx, 0, 0, 0, 0, 0, kd(i).ref(1), kd(i).uunt(1), uStr(2), convert(kd(i).uunt(1), uStr(2), Val(kd(i).kd(1)))
          End If
        End If
        write_parmrec fle, parm
        
        If kd(i).kd(0) = Empty Or kd(i).kd(2) <= 0 Then
          PutError "Parameter WPCLTHALF for " & kd(i).cas & " is invalid"
          set_parm parm, "wpclthalf", kd(i).idx, 0, 0, 0, 0, 0, kd(i).ref(2), kd(i).uunt(2), uStr(3), "EMPTY"
        Else
          set_parm parm, "wpclthalf", kd(i).idx, 0, 0, 0, 0, 0, kd(i).ref(2), kd(i).uunt(2), uStr(3), convert(kd(i).uunt(2), uStr(3), Val(kd(i).kd(2)))
        End If
        write_parmrec fle, parm
        
        For j = 1 To kd(i).numprog
          If kd(i).progeny(j).idx <> 0 Then
            set_parm parm, "vadcasid", kd(i).idx, kd(i).progeny(j).idx, 0, 0, 0, 0, kd(i).progeny(j).ref(0), "N/A", "N/A", kd(i).progeny(j).cas
            write_parmrec fle, parm
            If kd(i).progeny(j).kd(0) = Empty Then
              PutError "Parameter WASUBKD for " & kd(i).progeny(j).cas & " daughter of " & kd(i).cas & " is invalid"
              set_parm parm, "wasubkd", kd(i).idx, kd(i).progeny(j).idx, 0, 0, 0, 0, kd(i).progeny(j).ref(0), kd(i).progeny(j).uunt(0), uStr(0), "EMPTY"
            Else
              set_parm parm, "wasubkd", kd(i).idx, kd(i).progeny(j).idx, 0, 0, 0, 0, kd(i).progeny(j).ref(0), kd(i).progeny(j).uunt(0), uStr(0), convert(kd(i).progeny(j).uunt(0), uStr(0), Val(kd(i).progeny(j).kd(0)))
            End If
            write_parmrec fle, parm
            
            If kd(i).progeny(j).kd(1) = Empty Or kd(i).progeny(j).kd(1) <= 0 Then
              PutError "Parameter WPCLSOL for " & kd(i).progeny(j).cas & " daughter of " & kd(i).cas & " is invalid"
              If con(kd(i).idx).rad Then
                set_parm parm, "wpclsol", kd(i).idx, kd(i).progeny(j).idx, 0, 0, 0, 0, kd(i).progeny(j).ref(1), kd(i).progeny(j).uunt(1), uStr(1), "EMPTY"
              Else
                set_parm parm, "wpclsol", kd(i).idx, kd(i).progeny(j).idx, 0, 0, 0, 0, kd(i).progeny(j).ref(1), kd(i).progeny(j).uunt(1), uStr(2), "EMPTY"
              End If
            Else
              If con(kd(i).idx).rad Then
                set_parm parm, "wpclsol", kd(i).idx, kd(i).progeny(j).idx, 0, 0, 0, 0, kd(i).progeny(j).ref(1), kd(i).progeny(j).uunt(1), uStr(1), convert(kd(i).progeny(j).uunt(1), uStr(1), Val(kd(i).progeny(j).kd(1)))
              Else
                set_parm parm, "wpclsol", kd(i).idx, kd(i).progeny(j).idx, 0, 0, 0, 0, kd(i).progeny(j).ref(1), kd(i).progeny(j).uunt(1), uStr(2), convert(kd(i).progeny(j).uunt(1), uStr(2), Val(kd(i).progeny(j).kd(1)))
              End If
            End If
            write_parmrec fle, parm
            
            If kd(i).progeny(j).kd(2) = Empty Or kd(i).progeny(j).kd(2) <= 0 Then
              PutError "Parameter WPCLTHALF for " & kd(i).progeny(j).cas & " daughter of " & kd(i).cas & " is invalid"
              set_parm parm, "wpclthalf", kd(i).idx, kd(i).progeny(j).idx, 0, 0, 0, 0, kd(i).progeny(j).ref(2), kd(i).progeny(j).uunt(2), uStr(3), "EMPTY"
            Else
              set_parm parm, "wpclthalf", kd(i).idx, kd(i).progeny(j).idx, 0, 0, 0, 0, kd(i).progeny(j).ref(2), kd(i).progeny(j).uunt(2), uStr(3), convert(kd(i).progeny(j).uunt(2), uStr(3), Val(kd(i).progeny(j).kd(2)))
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
  EndModule
End Sub

Private Sub SSCommand1_Click()
  If Combo2.ListIndex > 0 Then Combo2.ListIndex = Combo2.ListIndex - 1
End Sub

Private Sub SSCommand2_Click()
  If Combo2.ListIndex < Combo2.ListCount - 1 Then Combo2.ListIndex = Combo2.ListIndex + 1
End Sub

Private Sub SSCommand3_Click()
  Dim i As Long
  Dim j As Long
  set_unit unit(13), unit(13).Tag
  txt(13).Text = txt(15).Text
  For i = 1 To m_numcon
    If con(kd(i).idx).name = Combo4.Text Then
      For j = 1 To kd(i).numprog
        If con(kd(i).idx).progeny(kd(i).progeny(j).idx).name = Combo2.Text Then
          kd(i).progeny(j).kd(ProgParms.ListIndex) = Val(txt(15).Text)
          kd(i).progeny(j).uunt(ProgParms.ListIndex) = unit(13).Tag
          Exit For
        End If
      Next
      Exit For
    End If
  Next
End Sub

Private Sub SSCommand4_Click()
  If Combo4.ListIndex > 0 Then Combo4.ListIndex = Combo4.ListIndex - 1
End Sub

Private Sub SSCommand5_Click()
  If Combo4.ListIndex < Combo4.ListCount - 1 Then Combo4.ListIndex = Combo4.ListIndex + 1
End Sub

Private Sub SSCommand6_Click()
  Dim i As Long
  set_unit unit(12), unit(12).Tag
  txt(12).Text = txt(14).Text
  For i = 1 To m_numcon
    If con(kd(i).idx).name = Combo4.Text Then
      kd(i).kd(0) = Val(txt(14).Text)
      kd(i).uunt(0) = unit(12).Tag
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
    kd(i).uunt(ConParms.ListIndex) = unit(12).Tag
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
          kd(i).progeny(j).uunt(ProgParms.ListIndex) = unit(13).Tag
        Next
        Exit For
      End If
    Next
    SSCommand3_Click
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
    If er(6) And SSTab1.Tab > 1 Then
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
  If SSTab1.Tab = 2 And Combo4.ListIndex = -1 Then SSCommand5_Click
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
    Case 12
      If ConParms.ListIndex = 0 Then
        m = "Value must be greater than or equal to 0"
        If (tval < 0) Then er = True
      Else
        m = "Value must be greater than 0"
        If Not (tval > 0) Then er = True
      End If
    Case 13
      If ProgParms.ListIndex = 0 Then
        m = "Value must be greater than or equal to 0"
        If (tval < 0) Then er = True
      Else
        m = "Value must be greater than 0"
        If Not (tval > 0) Then er = True
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
  Exit Sub
toolarge:
  txt(Index).BackColor = &H8080FF
End Sub

Private Sub kdchange(tx As String, oldtx As String)
  Dim i As Long
  
  For i = 1 To m_numcon
    If con(kd(i).idx).name = oldtx Then
       If txt(12).Text = "" Then
         kd(i).kd(oldconidx) = Empty
       Else
         kd(i).kd(oldconidx) = txt(12).Text
       End If
       kd(i).uunt(oldconidx) = unit(12).Text
       kd(i).ref(oldconidx) = Val(ref(12).Tag)
       Exit For
    End If
  Next
  For i = 0 To m_numcon
    If con(kd(i).idx).name = tx Then
      unit(12).Clear
      get_conversion_items kd(i).uunt(ConParms.ListIndex), unit(12)
      set_unit unit(12), kd(i).uunt(ConParms.ListIndex)
      If kd(i).kd(ConParms.ListIndex) = Empty Then
        txt(12).Text = ""
      Else
        txt(12).Text = kd(i).kd(ConParms.ListIndex)
      End If
      ref(12).Caption = "Ref: " & kd(i).ref(ConParms.ListIndex)
      ref(12).Tag = kd(i).ref(ConParms.ListIndex)
      estimatekd
      If ConParms.ListIndex = 2 Then
        If con(i).rad Then
          txt(12).Enabled = False
          unit(12).Enabled = False
        Else
          txt(12).Enabled = True
          unit(12).Enabled = True
        End If
      Else
        txt(12).Enabled = True
        unit(12).Enabled = True
      End If
      Exit For
    End If
  Next
End Sub

Private Sub d_kdchange(tx As String, oldtx As String)
  Dim i As Long
  Dim j As Long
  Dim lstcnt As Long
  For i = 1 To m_numcon
    If con(kd(i).idx).name = Combo4.Text Then
      For j = 1 To kd(i).numprog
        If con(kd(i).idx).progeny(kd(i).progeny(j).idx).name = oldtx Then
          If txt(13).Text = "" Then
            kd(i).progeny(j).kd(oldprogidx) = Empty
          Else
            kd(i).progeny(j).kd(oldprogidx) = txt(13).Text
          End If
          kd(i).progeny(j).uunt(oldprogidx) = unit(13).Text
          kd(i).progeny(j).ref(oldprogidx) = Val(ref(13).Tag)
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
          unit(13).Clear
          get_conversion_items kd(i).progeny(j).uunt(ProgParms.ListIndex), unit(13)
          set_unit unit(13), kd(i).progeny(j).uunt(ProgParms.ListIndex)
          If kd(i).progeny(j).kd(ProgParms.ListIndex) = Empty Then
            txt(13).Text = ""
          Else
            txt(13).Text = kd(i).progeny(j).kd(ProgParms.ListIndex)
          End If
          ref(13).Caption = "Ref: " & kd(i).progeny(j).ref(ProgParms.ListIndex)
          ref(13).Tag = kd(i).progeny(j).ref(ProgParms.ListIndex)
          d_estimatekd
          If ProgParms.ListIndex = 2 Then
            txt(13).Enabled = False
            unit(13).Enabled = False
          Else
            txt(13).Enabled = True
            unit(13).Enabled = True
          End If
          Exit For
        End If
      Next
      Exit For
    End If
  Next
End Sub

Private Sub changetxt(i As Long, j As Long, k As Long, f1 As Double, f2 As Double, f3 As Double, f4 As Double, f5 As Double)
  If Not loadng Then
    txt(1).Text = i
    txt(2).Text = j
    txt(3).Text = k
    txt(4).Text = 0
    txt(5).Text = 0
    If auto.Checked Then
      set_unit unit(9), unit(9).Tag
      set_unit unit(11), unit(11).Tag
      txt(9).Text = f1 * 86400
      txt(7).Text = f2
      txt(11).Text = f3
      txt(8).Text = f4
      txt(16).Text = f5
    End If
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
End Sub

Private Sub Combo2_Click()
  d_kdchange Combo2.Text, ctext4
  ctext4 = Combo2.Text
End Sub

Private Sub Combo4_Click()
  Dim i As Long
  Dim j As Long
  kdchange Combo4.Text, ctext3
  ctext3 = Combo4.Text
  Combo2.Clear
  For i = 1 To m_numcon
    If con(kd(i).idx).name = ctext3 Then
      If kd(i).numprog = 0 Then
        txt(13).Visible = False
        unit(13).Visible = False
        ref(13).Visible = False
        txt(15).Visible = False
        SSCommand1.Visible = False
        SSCommand2.Visible = False
        SSCommand3.Visible = False
        SSCommand8.Visible = False
        Label14.Visible = False
        Label16.Visible = False
        Label17.Visible = False
        Combo2.Visible = False
        ProgParms.Visible = False
      Else
        For j = 1 To kd(i).numprog
          Combo2.AddItem con(kd(i).idx).progeny(kd(i).progeny(j).idx).name
        Next
        txt(13).Visible = True
        unit(13).Visible = True
        ref(13).Visible = True
        txt(15).Visible = True
        SSCommand1.Visible = True
        SSCommand2.Visible = True
        SSCommand3.Visible = True
        SSCommand8.Visible = True
        Label14.Visible = True
        Label16.Visible = True
        Label17.Visible = True
        Combo2.Visible = True
        Combo2.ListIndex = 0
        ProgParms.Visible = True
        If Not ProgParms.ListIndex = 0 Then
          Label14.Visible = False
          txt(15).Visible = False
        End If
      End If
      Exit For
    End If
  Next
  If Not ProgParms.ListIndex = 0 Then
    Label14.Visible = False
    txt(15).Visible = False
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
  txt(14).Visible = check
  Label39.Visible = check
End Sub

Private Sub ProgParms_Click()
  Dim check As Boolean
  
  If loadng Then Exit Sub
  Combo2_Click
  oldprogidx = ProgParms.ListIndex
  If ProgParms.ListIndex = 0 Then
    check = True
  Else
    check = False
  End If
  SSCommand8.Visible = check
  SSCommand3.Visible = check
  txt(15).Visible = check
  Label14.Visible = check
End Sub

Private Sub txt_LostFocus(Index As Integer)
  If Index = 12 Then
    Combo4_Click
  End If
  If Index = 13 Then
    Combo2_Click
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
  Case 0: HelpAnchor = "SOIL_COMPOSITION"
  Case 1: HelpAnchor = "CHARACTERISTICS"
  Case 2: HelpAnchor = "KDS"
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
  m = "Select a progeny"
  mes = Space(140 - Len(m)) & m
  RefItem = 13
  noact.Enabled = True
  HelpAnchor = txt(13).Tag
End Sub

Private Sub Combo4_GotFocus()
Dim m As String
  m = "Select a constituent"
  mes = Space(140 - Len(m)) & m
  RefItem = 12
  noact.Enabled = True
  HelpAnchor = txt(12).Tag
End Sub

Private Sub ConParms_GotFocus()
Dim m As String
  m = "Select a property"
  mes = Space(140 - Len(m)) & m
  RefItem = -1
  noact.Enabled = True
  HelpAnchor = "KDS"
End Sub

Private Sub ProgParms_GotFocus()
Dim m As String
  m = "Select a property"
  mes = Space(140 - Len(m)) & m
  RefItem = -1
  noact.Enabled = True
  HelpAnchor = "KDS"
End Sub

Private Sub SSCommand1_GotFocus()
  mes = ""
  noact.Enabled = False
  HelpAnchor = ""
End Sub

Private Sub SSCommand2_GotFocus()
  mes = ""
  noact.Enabled = False
  HelpAnchor = ""
End Sub

Private Sub SSCommand3_GotFocus()
  mes = ""
  noact.Enabled = False
  HelpAnchor = ""
End Sub

Private Sub SSCommand4_GotFocus()
  mes = ""
  noact.Enabled = False
  HelpAnchor = ""
End Sub

Private Sub SSCommand5_GotFocus()
  mes = ""
  noact.Enabled = False
  HelpAnchor = ""
End Sub

Private Sub SSCommand6_GotFocus()
  mes = ""
  noact.Enabled = False
  HelpAnchor = ""
End Sub

Private Sub SSCommand7_GotFocus()
  mes = ""
  noact.Enabled = False
  HelpAnchor = ""
End Sub

Private Sub SSCommand8_GotFocus()
  mes = ""
  noact.Enabled = False
  HelpAnchor = ""
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

