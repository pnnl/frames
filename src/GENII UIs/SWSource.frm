VERSION 5.00
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "TABCTL32.OCX"
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Begin VB.Form SWSource 
   BorderStyle     =   1  'Fixed Single
   ClientHeight    =   6345
   ClientLeft      =   9120
   ClientTop       =   630
   ClientWidth     =   7305
   Icon            =   "SWSource.frx":0000
   KeyPreview      =   -1  'True
   MaxButton       =   0   'False
   MinButton       =   0   'False
   NegotiateMenus  =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   6345
   ScaleWidth      =   7305
   StartUpPosition =   2  'CenterScreen
   Begin TabDlg.SSTab SSTab1 
      Height          =   6375
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   7335
      _ExtentX        =   12938
      _ExtentY        =   11245
      _Version        =   393216
      TabHeight       =   750
      TabCaption(0)   =   "General Inputs"
      TabPicture(0)   =   "SWSource.frx":030A
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "Frame1"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).ControlCount=   1
      TabCaption(1)   =   "Pathway Productions"
      TabPicture(1)   =   "SWSource.frx":0326
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "Frame2"
      Tab(1).ControlCount=   1
      TabCaption(2)   =   "Pathway Populations"
      TabPicture(2)   =   "SWSource.frx":0342
      Tab(2).ControlEnabled=   0   'False
      Tab(2).Control(0)=   "Frame3"
      Tab(2).ControlCount=   1
      Begin VB.Frame Frame3 
         Height          =   5775
         Left            =   -74880
         TabIndex        =   80
         Top             =   480
         Width           =   7095
         Begin VB.TextBox txt1 
            Alignment       =   1  'Right Justify
            Height          =   288
            Index           =   28
            Left            =   2400
            TabIndex        =   35
            Tag             =   "filename"
            Top             =   720
            Width           =   855
         End
         Begin VB.TextBox txt1 
            Alignment       =   1  'Right Justify
            Height          =   288
            Index           =   27
            Left            =   5880
            TabIndex        =   58
            Tag             =   "soil external"
            Top             =   4800
            Width           =   855
         End
         Begin VB.TextBox txt1 
            Alignment       =   1  'Right Justify
            Height          =   288
            Index           =   26
            Left            =   5880
            TabIndex        =   57
            Tag             =   "shoreline external"
            Top             =   4440
            Width           =   855
         End
         Begin VB.TextBox txt1 
            Alignment       =   1  'Right Justify
            Height          =   288
            Index           =   25
            Left            =   5880
            TabIndex        =   56
            Tag             =   "boating external"
            Top             =   4080
            Width           =   855
         End
         Begin VB.TextBox txt1 
            Alignment       =   1  'Right Justify
            Height          =   288
            Index           =   24
            Left            =   5880
            TabIndex        =   55
            Tag             =   "swimming external"
            Top             =   3720
            Width           =   855
         End
         Begin VB.TextBox txt1 
            Alignment       =   1  'Right Justify
            Height          =   288
            Index           =   23
            Left            =   5880
            TabIndex        =   54
            Tag             =   "suspended soil"
            Top             =   3360
            Width           =   855
         End
         Begin VB.TextBox txt1 
            Alignment       =   1  'Right Justify
            Height          =   288
            Index           =   22
            Left            =   5880
            TabIndex        =   53
            Tag             =   "indoor air"
            Top             =   3000
            Width           =   855
         End
         Begin VB.TextBox txt1 
            Alignment       =   1  'Right Justify
            Height          =   288
            Index           =   21
            Left            =   5880
            TabIndex        =   52
            Tag             =   "outdoor air"
            Top             =   2640
            Width           =   855
         End
         Begin VB.TextBox txt1 
            Alignment       =   1  'Right Justify
            Height          =   288
            Index           =   20
            Left            =   5880
            TabIndex        =   51
            Tag             =   "soil"
            Top             =   2280
            Width           =   855
         End
         Begin VB.TextBox txt1 
            Alignment       =   1  'Right Justify
            Height          =   288
            Index           =   19
            Left            =   5880
            TabIndex        =   50
            Tag             =   "swimming water"
            Top             =   1920
            Width           =   855
         End
         Begin VB.TextBox txt1 
            Alignment       =   1  'Right Justify
            Height          =   288
            Index           =   18
            Left            =   5880
            TabIndex        =   49
            Tag             =   "shower water"
            Top             =   1560
            Width           =   855
         End
         Begin VB.TextBox txt1 
            Alignment       =   1  'Right Justify
            Height          =   288
            Index           =   17
            Left            =   5880
            TabIndex        =   48
            Tag             =   "drinking water"
            Top             =   1200
            Width           =   855
         End
         Begin VB.TextBox txt1 
            Alignment       =   1  'Right Justify
            Height          =   288
            Index           =   16
            Left            =   2400
            TabIndex        =   47
            Tag             =   "aquatic plants"
            Top             =   5160
            Width           =   855
         End
         Begin VB.TextBox txt1 
            Alignment       =   1  'Right Justify
            Height          =   288
            Index           =   15
            Left            =   2400
            TabIndex        =   46
            Tag             =   "crustacea"
            Top             =   4800
            Width           =   855
         End
         Begin VB.TextBox txt1 
            Alignment       =   1  'Right Justify
            Height          =   288
            Index           =   14
            Left            =   2400
            TabIndex        =   45
            Tag             =   "mollusks"
            Top             =   4440
            Width           =   855
         End
         Begin VB.TextBox txt1 
            Alignment       =   1  'Right Justify
            Height          =   288
            Index           =   13
            Left            =   2400
            TabIndex        =   44
            Tag             =   "fish"
            Top             =   4080
            Width           =   855
         End
         Begin VB.TextBox txt1 
            Alignment       =   1  'Right Justify
            Height          =   288
            Index           =   12
            Left            =   2400
            TabIndex        =   43
            Tag             =   "grain"
            Top             =   3720
            Width           =   855
         End
         Begin VB.TextBox txt1 
            Alignment       =   1  'Right Justify
            Height          =   288
            Index           =   11
            Left            =   2400
            TabIndex        =   42
            Tag             =   "fruit"
            Top             =   3360
            Width           =   855
         End
         Begin VB.TextBox txt1 
            Alignment       =   1  'Right Justify
            Height          =   288
            Index           =   10
            Left            =   2400
            TabIndex        =   41
            Tag             =   "root vegatables"
            Top             =   3000
            Width           =   855
         End
         Begin VB.TextBox txt1 
            Alignment       =   1  'Right Justify
            Height          =   288
            Index           =   9
            Left            =   2400
            TabIndex        =   40
            Tag             =   "leafy vegatables"
            Top             =   2640
            Width           =   855
         End
         Begin VB.TextBox txt1 
            Alignment       =   1  'Right Justify
            Height          =   288
            Index           =   8
            Left            =   2400
            TabIndex        =   39
            Tag             =   "eggs"
            Top             =   2280
            Width           =   855
         End
         Begin VB.TextBox txt1 
            Alignment       =   1  'Right Justify
            Height          =   288
            Index           =   7
            Left            =   2400
            TabIndex        =   38
            Tag             =   "milk"
            Top             =   1920
            Width           =   855
         End
         Begin VB.TextBox txt1 
            Alignment       =   1  'Right Justify
            Height          =   288
            Index           =   6
            Left            =   2400
            TabIndex        =   37
            Tag             =   "poultry"
            Top             =   1560
            Width           =   855
         End
         Begin VB.TextBox txt1 
            Alignment       =   1  'Right Justify
            Height          =   288
            Index           =   5
            Left            =   2400
            TabIndex        =   36
            Tag             =   "meat"
            Top             =   1200
            Width           =   855
         End
         Begin VB.ComboBox unit2 
            Height          =   315
            Index           =   3
            ItemData        =   "SWSource.frx":035E
            Left            =   5640
            List            =   "SWSource.frx":0374
            Style           =   2  'Dropdown List
            TabIndex        =   34
            Tag             =   "m"
            Top             =   360
            Width           =   1125
         End
         Begin VB.Label Label1 
            Caption         =   "Population fill value"
            Height          =   255
            Index           =   28
            Left            =   240
            TabIndex        =   105
            Top             =   720
            Width           =   2130
         End
         Begin VB.Label Label1 
            Caption         =   "Soil External"
            Height          =   255
            Index           =   27
            Left            =   3720
            TabIndex        =   104
            Top             =   4800
            Width           =   2130
         End
         Begin VB.Label Label1 
            Caption         =   "Shoreline external"
            Height          =   255
            Index           =   26
            Left            =   3720
            TabIndex        =   103
            Top             =   4440
            Width           =   2130
         End
         Begin VB.Label Label1 
            Caption         =   "Boating external"
            Height          =   255
            Index           =   25
            Left            =   3720
            TabIndex        =   102
            Top             =   4080
            Width           =   2130
         End
         Begin VB.Label Label1 
            Caption         =   "Swimming external"
            Height          =   255
            Index           =   24
            Left            =   3720
            TabIndex        =   101
            Top             =   3720
            Width           =   2130
         End
         Begin VB.Label Label1 
            Caption         =   "Inhalation of suspeneded soil"
            Height          =   255
            Index           =   23
            Left            =   3720
            TabIndex        =   100
            Top             =   3360
            Width           =   2130
         End
         Begin VB.Label Label1 
            Caption         =   "Inhalation of indoor air"
            Height          =   255
            Index           =   22
            Left            =   3720
            TabIndex        =   99
            Top             =   3000
            Width           =   2130
         End
         Begin VB.Label Label1 
            Caption         =   "Inhalation of outdoor air"
            Height          =   255
            Index           =   21
            Left            =   3720
            TabIndex        =   98
            Top             =   2640
            Width           =   2130
         End
         Begin VB.Label Label1 
            Caption         =   "Soil ingestion"
            Height          =   255
            Index           =   20
            Left            =   3720
            TabIndex        =   97
            Top             =   2280
            Width           =   2130
         End
         Begin VB.Label Label1 
            Caption         =   "Swimming water ingestion"
            Height          =   255
            Index           =   19
            Left            =   3720
            TabIndex        =   96
            Top             =   1920
            Width           =   2130
         End
         Begin VB.Label Label1 
            Caption         =   "Shower water ingestion"
            Height          =   255
            Index           =   18
            Left            =   3720
            TabIndex        =   95
            Top             =   1560
            Width           =   2130
         End
         Begin VB.Label Label1 
            Caption         =   "Drinking water ingestion"
            Height          =   255
            Index           =   17
            Left            =   3720
            TabIndex        =   94
            Top             =   1200
            Width           =   2130
         End
         Begin VB.Label Label1 
            Caption         =   "Aquatic plant ingestion"
            Height          =   255
            Index           =   16
            Left            =   240
            TabIndex        =   93
            Top             =   5160
            Width           =   2130
         End
         Begin VB.Label Label1 
            Caption         =   "Crustacea ingestion"
            Height          =   255
            Index           =   15
            Left            =   240
            TabIndex        =   92
            Top             =   4800
            Width           =   2130
         End
         Begin VB.Label Label1 
            Caption         =   "Mollusks ingestion"
            Height          =   255
            Index           =   14
            Left            =   240
            TabIndex        =   91
            Top             =   4440
            Width           =   2130
         End
         Begin VB.Label Label1 
            Caption         =   "Fish ingestion"
            Height          =   255
            Index           =   13
            Left            =   240
            TabIndex        =   90
            Top             =   4080
            Width           =   2130
         End
         Begin VB.Label Label1 
            Caption         =   "Grains ingestion"
            Height          =   255
            Index           =   12
            Left            =   240
            TabIndex        =   89
            Top             =   3720
            Width           =   2130
         End
         Begin VB.Label Label1 
            Caption         =   "Fruits ingestion"
            Height          =   255
            Index           =   11
            Left            =   240
            TabIndex        =   88
            Top             =   3360
            Width           =   2130
         End
         Begin VB.Label Label1 
            Caption         =   "Root vegatables ingestion"
            Height          =   255
            Index           =   10
            Left            =   240
            TabIndex        =   87
            Top             =   3000
            Width           =   2130
         End
         Begin VB.Label Label1 
            Caption         =   "Leafy vegatables ingestion"
            Height          =   255
            Index           =   9
            Left            =   240
            TabIndex        =   86
            Top             =   2640
            Width           =   2130
         End
         Begin VB.Label Label1 
            Caption         =   "Eggs ingestion"
            Height          =   255
            Index           =   8
            Left            =   240
            TabIndex        =   85
            Top             =   2280
            Width           =   2130
         End
         Begin VB.Label Label1 
            Caption         =   "Milk ingestion"
            Height          =   255
            Index           =   7
            Left            =   240
            TabIndex        =   84
            Top             =   1920
            Width           =   2130
         End
         Begin VB.Label Label1 
            Caption         =   "Poultry ingestion"
            Height          =   255
            Index           =   6
            Left            =   240
            TabIndex        =   83
            Top             =   1560
            Width           =   2130
         End
         Begin VB.Label Label1 
            Caption         =   "Meat Ingestion"
            Height          =   255
            Index           =   5
            Left            =   240
            TabIndex        =   82
            Top             =   1200
            Width           =   2130
         End
         Begin VB.Label Label2 
            Caption         =   "Age group index (This index corresponds to Receptor age group index)"
            Height          =   255
            Index           =   2
            Left            =   240
            TabIndex        =   81
            Top             =   360
            Width           =   5250
         End
      End
      Begin VB.Frame Frame2 
         Height          =   5775
         Left            =   -74880
         TabIndex        =   63
         Top             =   480
         Width           =   7095
         Begin VB.ComboBox unit 
            Height          =   315
            Index           =   11
            Left            =   5400
            Style           =   2  'Dropdown List
            TabIndex        =   33
            Tag             =   "kg/yr"
            Top             =   4560
            Width           =   1365
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            Height          =   285
            Index           =   11
            Left            =   4080
            TabIndex        =   32
            Top             =   4560
            Width           =   1335
         End
         Begin VB.ComboBox unit 
            Height          =   315
            Index           =   10
            Left            =   5400
            Style           =   2  'Dropdown List
            TabIndex        =   31
            Tag             =   "kg/yr"
            Top             =   4200
            Width           =   1365
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            Height          =   285
            Index           =   10
            Left            =   4080
            TabIndex        =   30
            Top             =   4200
            Width           =   1335
         End
         Begin VB.ComboBox unit 
            Height          =   315
            Index           =   9
            Left            =   5400
            Style           =   2  'Dropdown List
            TabIndex        =   29
            Tag             =   "kg/yr"
            Top             =   3840
            Width           =   1365
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            Height          =   285
            Index           =   9
            Left            =   4080
            TabIndex        =   28
            Top             =   3840
            Width           =   1335
         End
         Begin VB.ComboBox unit 
            Height          =   315
            Index           =   8
            Left            =   5400
            Style           =   2  'Dropdown List
            TabIndex        =   27
            Tag             =   "kg/yr"
            Top             =   3480
            Width           =   1365
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            Height          =   285
            Index           =   8
            Left            =   4080
            TabIndex        =   26
            Top             =   3480
            Width           =   1335
         End
         Begin VB.ComboBox unit 
            Height          =   315
            Index           =   7
            Left            =   5400
            Style           =   2  'Dropdown List
            TabIndex        =   25
            Tag             =   "kg/yr"
            Top             =   3000
            Width           =   1365
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            Height          =   285
            Index           =   7
            Left            =   4080
            TabIndex        =   24
            Top             =   3000
            Width           =   1335
         End
         Begin VB.ComboBox unit 
            Height          =   315
            Index           =   6
            Left            =   5400
            Style           =   2  'Dropdown List
            TabIndex        =   23
            Tag             =   "kg/yr"
            Top             =   2640
            Width           =   1365
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            Height          =   285
            Index           =   6
            Left            =   4080
            TabIndex        =   22
            Top             =   2640
            Width           =   1335
         End
         Begin VB.ComboBox unit 
            Height          =   315
            Index           =   5
            Left            =   5400
            Style           =   2  'Dropdown List
            TabIndex        =   21
            Tag             =   "kg/yr"
            Top             =   2280
            Width           =   1365
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            Height          =   285
            Index           =   5
            Left            =   4080
            TabIndex        =   20
            Top             =   2280
            Width           =   1335
         End
         Begin VB.ComboBox unit 
            Height          =   315
            Index           =   4
            Left            =   5400
            Style           =   2  'Dropdown List
            TabIndex        =   19
            Tag             =   "kg/yr"
            Top             =   1920
            Width           =   1365
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            Height          =   285
            Index           =   4
            Left            =   4080
            TabIndex        =   18
            Top             =   1920
            Width           =   1335
         End
         Begin VB.ComboBox unit 
            Height          =   315
            Index           =   3
            Left            =   5400
            Style           =   2  'Dropdown List
            TabIndex        =   17
            Tag             =   "kg/yr"
            Top             =   1440
            Width           =   1365
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            Height          =   285
            Index           =   3
            Left            =   4080
            TabIndex        =   16
            Top             =   1440
            Width           =   1335
         End
         Begin VB.ComboBox unit 
            Height          =   315
            Index           =   2
            Left            =   5400
            Style           =   2  'Dropdown List
            TabIndex        =   15
            Tag             =   "kg/yr"
            Top             =   1080
            Width           =   1365
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            Height          =   285
            Index           =   2
            Left            =   4080
            TabIndex        =   14
            Top             =   1080
            Width           =   1335
         End
         Begin VB.ComboBox unit 
            Height          =   315
            Index           =   1
            Left            =   5400
            Style           =   2  'Dropdown List
            TabIndex        =   13
            Tag             =   "kg/yr"
            Top             =   720
            Width           =   1365
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            Height          =   285
            Index           =   1
            Left            =   4080
            TabIndex        =   12
            Top             =   720
            Width           =   1335
         End
         Begin VB.ComboBox unit 
            Height          =   315
            Index           =   0
            Left            =   5400
            Style           =   2  'Dropdown List
            TabIndex        =   11
            Tag             =   "kg/yr"
            Top             =   360
            Width           =   1365
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            Height          =   285
            Index           =   0
            Left            =   4080
            TabIndex        =   10
            Top             =   360
            Width           =   1335
         End
         Begin VB.Label lbl 
            Caption         =   "Aquatic plant production"
            Height          =   255
            Index           =   11
            Left            =   240
            TabIndex        =   75
            Top             =   4560
            Width           =   2805
         End
         Begin VB.Label lbl 
            Caption         =   "Crustacea production"
            Height          =   255
            Index           =   10
            Left            =   240
            TabIndex        =   74
            Top             =   4200
            Width           =   2805
         End
         Begin VB.Label lbl 
            Caption         =   "Mollusk production"
            Height          =   255
            Index           =   9
            Left            =   240
            TabIndex        =   73
            Top             =   3840
            Width           =   2805
         End
         Begin VB.Label lbl 
            Caption         =   "Fish production"
            Height          =   255
            Index           =   8
            Left            =   240
            TabIndex        =   72
            Top             =   3480
            Width           =   2805
         End
         Begin VB.Label lbl 
            Caption         =   "Grain production"
            Height          =   255
            Index           =   7
            Left            =   240
            TabIndex        =   71
            Top             =   3000
            Width           =   2805
         End
         Begin VB.Label lbl 
            Caption         =   "Fruit production"
            Height          =   255
            Index           =   6
            Left            =   240
            TabIndex        =   70
            Top             =   2640
            Width           =   2805
         End
         Begin VB.Label lbl 
            Caption         =   "Root vegatable production"
            Height          =   255
            Index           =   5
            Left            =   240
            TabIndex        =   69
            Top             =   2280
            Width           =   2805
         End
         Begin VB.Label lbl 
            Caption         =   "Leafy vegatable production"
            Height          =   255
            Index           =   4
            Left            =   240
            TabIndex        =   68
            Top             =   1920
            Width           =   2805
         End
         Begin VB.Label lbl 
            Caption         =   "Egg production"
            Height          =   255
            Index           =   3
            Left            =   240
            TabIndex        =   67
            Top             =   1440
            Width           =   2805
         End
         Begin VB.Label lbl 
            Caption         =   "Milk production"
            Height          =   255
            Index           =   2
            Left            =   240
            TabIndex        =   66
            Top             =   1080
            Width           =   2805
         End
         Begin VB.Label lbl 
            Caption         =   "Poultry production"
            Height          =   255
            Index           =   1
            Left            =   240
            TabIndex        =   65
            Top             =   720
            Width           =   2805
         End
         Begin VB.Label lbl 
            Caption         =   "Meat production"
            Height          =   255
            Index           =   0
            Left            =   240
            TabIndex        =   64
            Top             =   360
            Width           =   2805
         End
      End
      Begin VB.Frame Frame1 
         Height          =   5775
         Left            =   120
         TabIndex        =   59
         Top             =   480
         Width           =   7095
         Begin VB.ComboBox unit2 
            Height          =   315
            Index           =   1
            ItemData        =   "SWSource.frx":038A
            Left            =   5520
            List            =   "SWSource.frx":0394
            Style           =   2  'Dropdown List
            TabIndex        =   2
            Tag             =   "printdetails"
            Top             =   840
            Width           =   1365
         End
         Begin VB.TextBox txt1 
            Height          =   288
            Index           =   4
            Left            =   480
            TabIndex        =   6
            Tag             =   "facUserName"
            Top             =   3360
            Width           =   6375
         End
         Begin VB.TextBox txt1 
            Height          =   288
            Index           =   3
            Left            =   480
            TabIndex        =   5
            Tag             =   "facCityState"
            Top             =   2760
            Width           =   6375
         End
         Begin VB.TextBox txt1 
            Height          =   288
            Index           =   2
            Left            =   480
            TabIndex        =   4
            Tag             =   "facAddress"
            Top             =   2160
            Width           =   6375
         End
         Begin VB.TextBox txt1 
            Height          =   288
            Index           =   1
            Left            =   480
            TabIndex        =   3
            Tag             =   "facName"
            Top             =   1560
            Width           =   6375
         End
         Begin VB.TextBox txt1 
            Height          =   288
            Index           =   0
            Left            =   480
            TabIndex        =   8
            Tag             =   "filename"
            Top             =   4680
            Width           =   6375
         End
         Begin VB.CommandButton Command1 
            Caption         =   "Open Production File"
            Height          =   372
            Left            =   4560
            TabIndex        =   9
            Top             =   5160
            Width           =   2292
         End
         Begin VB.ComboBox unit2 
            Height          =   315
            Index           =   0
            ItemData        =   "SWSource.frx":03A1
            Left            =   5520
            List            =   "SWSource.frx":03B1
            Style           =   2  'Dropdown List
            TabIndex        =   1
            Tag             =   "reportunits"
            Top             =   360
            Width           =   1365
         End
         Begin VB.ComboBox unit2 
            Height          =   315
            Index           =   2
            ItemData        =   "SWSource.frx":03C9
            Left            =   4560
            List            =   "SWSource.frx":03D3
            Style           =   2  'Dropdown List
            TabIndex        =   7
            Tag             =   "exposuretype"
            Top             =   3960
            Width           =   2325
         End
         Begin VB.Label Label2 
            Caption         =   "Print details by pathway by radionuclide"
            Height          =   255
            Index           =   3
            Left            =   240
            TabIndex        =   106
            Top             =   840
            Width           =   3450
         End
         Begin VB.Label Label1 
            Caption         =   "User Name"
            Height          =   255
            Index           =   4
            Left            =   240
            TabIndex        =   79
            Top             =   3120
            Width           =   2490
         End
         Begin VB.Label Label1 
            Caption         =   "City, State and Zip Code"
            Height          =   255
            Index           =   3
            Left            =   240
            TabIndex        =   78
            Top             =   2520
            Width           =   2490
         End
         Begin VB.Label Label1 
            Caption         =   "Address"
            Height          =   255
            Index           =   2
            Left            =   240
            TabIndex        =   77
            Top             =   1920
            Width           =   2490
         End
         Begin VB.Label Label1 
            Caption         =   "Facility Name"
            Height          =   255
            Index           =   1
            Left            =   240
            TabIndex        =   76
            Top             =   1320
            Width           =   2490
         End
         Begin VB.Label Label1 
            Caption         =   "Pathway Production Filename"
            Height          =   255
            Index           =   0
            Left            =   240
            TabIndex        =   62
            Top             =   4440
            Width           =   2490
         End
         Begin VB.Label Label2 
            Caption         =   "Report output units"
            Height          =   255
            Index           =   0
            Left            =   240
            TabIndex        =   61
            Top             =   360
            Width           =   3450
         End
         Begin VB.Label Label2 
            Caption         =   "Type of exposure"
            Height          =   255
            Index           =   1
            Left            =   240
            TabIndex        =   60
            Top             =   3960
            Width           =   3450
         End
      End
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   6960
      Top             =   360
      _ExtentX        =   688
      _ExtentY        =   688
      _Version        =   393216
   End
   Begin VB.Menu file 
      Caption         =   "&File"
      WindowList      =   -1  'True
      Begin VB.Menu save 
         Caption         =   "&Save and Exit"
      End
      Begin VB.Menu leave 
         Caption         =   "E&xit"
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
         Enabled         =   0   'False
         Visible         =   0   'False
      End
      Begin VB.Menu about 
         Caption         =   "&About"
      End
   End
End
Attribute VB_Name = "SWSource"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Dim loadng As Boolean
Dim temp As parmrec

Private Sub about_Click()
  frmAbout.Show 1, SWSource
End Sub

Private Sub Command1_Click()
  CommonDialog1.ShowOpen
  txt1(0).Text = CommonDialog1.fileName
  import
End Sub

Private Sub import()
Dim fle As csv
Dim i As Long
Dim j As Long
Dim k As Long
Dim numage As Long
Dim numpath As Long
Dim pathway As String
  
  If open_csv(fle, txt1(0).Text, F_READ) Then
    numage = get_val(fle)
    numpath = get_val(fle)
    get_line fle
    For k = 1 To numpath
      pathway = get_val(fle)
      ' getpath index just in case user mixed them up
      For i = 1 To MAXPATH
        If pathway = txt1(4 + i).Tag Then Exit For
      Next
      If i <= MAXPATH Then
        ProPops(i).production = Val(get_val(fle))
        If i < 13 Then
          txt(i - 1).Text = ProPops(i).production
        End If
        For j = 1 To numage
          ProPops(i).population(j) = Val(get_val(fle))
        Next
      End If
      get_line fle
    Next
    close_csv fle
    unit2_Click 3
  Else
    MsgBox "Error: Failure to read/open file " & txt1(0).Text
  End If
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

Private Sub loadprm()
  Dim m As Long
  Dim fle As parmfile
  
  If open_parm(fle, FUIName, 2) Then
    Do Until EOCF(fle.file)
      If read_parmrec(fle, temp) Then
        Loading.Gauge1.Max = Val(temp.idx1)
        Loading.Gauge1.Value = 0
        Select Case temp.pName
          Case "fui"
            For m = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                If temp.idx1 = siteIdx And temp.idx2 = modIdx Then
                  If temp.pName = "nesdespath" Then DesName = temp.pval
                End If
              End If
            Next
          
          Case modName
            For m = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                Loading.update
                Select Case temp.pName
                  Case "CVTFormat":    CVTFormat = "General Number" 'CVTFormat = temp.pval
                  Case "reportunits":
                    unit2(0).ListIndex = Val(temp.pval)
                  Case "printdetails":
                    unit2(1).ListIndex = Val(temp.pval)
                  Case "exposuretype":
                    unit2(2).ListIndex = Val(temp.pval)
                  Case "filename":
                    txt1(0).Text = temp.pval
                  Case "facName":
                    txt1(1).Text = temp.pval
                  Case "facAddress":
                    txt1(2).Text = temp.pval
                  Case "facCityState":
                    txt1(3).Text = temp.pval
                  Case "facUserName":
                    txt1(4).Text = temp.pval
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
  Else
    PutError "Can't find or open file " & FUIName
    EndModule
  End If
End Sub

Private Sub Form_load()
Dim i As Long
  StartModule Me, App.Title, 5
  SetHelpFile App.Path + "\swreport.htm"
  
  For i = 0 To 11
    get_conversion_items unit(i).Tag, unit(i)
  Next
  
  For i = 1 To 2
    SSTab1.TabVisible(i) = False
  Next
  
  For i = 0 To 3
    unit2(i).ListIndex = 0
  Next
  
  Loading.Show
  loadprm
  Unload Loading
  If txt1(0).Text <> "" Then import
  unit2_Click 3
End Sub

Private Sub save_Click()
  Dim i As Long
  Dim fName As String
  Dim parm As parmrec
  Dim fle As parmfile
  Dim UserFormat As String
  UserFormat = CVTFormat
  
  fName = RunName + ".GID"
  If open_parm(fle, fName, 1) Then
    set_parm parm, "CVTFormat", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CVTFormat
    write_parmrec fle, parm
    CVTFormat = "General Number"
    For i = 0 To 2
      set_parm parm, unit2(i).Tag, 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", unit2(i).ListIndex
      write_parmrec fle, parm
    Next
    For i = 0 To 4
      set_parm parm, txt1(i).Tag, 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", txt1(i).Text
      write_parmrec fle, parm
    Next
     
    close_parm fle
    save_propops
    CVTFormat = UserFormat
    SetFormat Me
  Else
    PutError "Unable to create transaction file" & RunName & ".GID"
  End If
  EndModule
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

Private Sub save_propops()
  Dim fle As csv
  Dim i As Long
  Dim j As Long
  Dim h As Long
  
  If open_csv(fle, txt1(0).Text, F_WRITE) Then
    put_val fle, MAXGROUP
    put_val fle, MAXPATH
    put_line fle
    For i = 1 To MAXPATH
      put_val fle, txt1(i + 4).Tag
      'convert back to base units before writing
      If i < 13 Then
        put_val fle, convert(unit(i - 1).Text, unit(i - 1).Tag, ProPops(i).production)
      Else
        put_val fle, ProPops(i).production
      End If
      For j = 1 To MAXGROUP
        put_val fle, ProPops(i).population(j)
      Next
      put_line fle
    Next
    close_csv fle
  End If
End Sub

Private Sub txt_Change(Index As Integer)
Dim chk As Double
On Error GoTo toolarge
  chk = CDbl(txt(Index).Text)
  ProPops(Index + 1).production = Val(txt(Index).Text)
  txt(Index).BackColor = &HC0FFC0
  Exit Sub
toolarge:
  txt(Index).BackColor = &H8080FF
End Sub

Private Sub txt1_Change(Index As Integer)
Dim chk As Long
Dim chk2 As Double
Dim i As Long

  If loadng Then Exit Sub
  Select Case Index
  Case 0 To 4
    ' do nothing
  Case 5 To 28
    On Error GoTo toolarge
    chk = CLng(txt1(Index).Text)
    chk2 = CDbl(txt1(Index).Text)
    If Not chk = chk2 Then GoTo toolarge
    txt1(Index).BackColor = &HC0FFC0
    If Index = 28 Then
      For i = 1 To MAXPATH
        txt1(4 + i).Text = Val(txt1(Index).Text)
      Next
      txt1(Index).Text = Val(txt1(Index).Text)
    Else
      ProPops(Index - 4).population(unit2(3).ListIndex + 1) = Val(txt1(Index).Text)
    End If
    Exit Sub
toolarge:
    txt1(Index).BackColor = &H8080FF
    If Index < 28 Then ProPops(Index - 4).population(unit2(3).ListIndex + 1) = 0
  End Select
End Sub

Private Sub unit2_Click(Index As Integer)
Dim i As Long
  Select Case Index
  Case 0 To 1
    ' do nothing
  Case 2
    SSTab1.TabVisible(1) = Not unit2(Index).ListIndex = 0
    SSTab1.TabVisible(2) = SSTab1.TabVisible(1)
    Label1(0).Visible = SSTab1.TabVisible(1)
    txt1(0).Visible = SSTab1.TabVisible(1)
    Command1.Visible = SSTab1.TabVisible(1)
  Case 3
    loadng = True
    For i = 1 To MAXPATH
      txt1(4 + i).Text = ProPops(i).population(unit2(3).ListIndex + 1)
      txt1(4 + i).BackColor = &HC0FFC0
    Next
    loadng = False
  End Select
End Sub
