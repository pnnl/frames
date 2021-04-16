VERSION 5.00
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "TABCTL32.OCX"
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Object = "{F856EC8B-F03C-4515-BDC6-64CBD617566A}#7.0#0"; "FPSPR70.ocx"
Begin VB.Form Source 
   BorderStyle     =   1  'Fixed Single
   ClientHeight    =   6855
   ClientLeft      =   9120
   ClientTop       =   630
   ClientWidth     =   9870
   Icon            =   "AirSource.frx":0000
   KeyPreview      =   -1  'True
   MaxButton       =   0   'False
   MinButton       =   0   'False
   NegotiateMenus  =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   6855
   ScaleWidth      =   9870
   StartUpPosition =   2  'CenterScreen
   Begin TabDlg.SSTab SSTab1 
      Height          =   6375
      Left            =   0
      TabIndex        =   1
      Top             =   0
      Width           =   9855
      _ExtentX        =   17383
      _ExtentY        =   11245
      _Version        =   393216
      Style           =   1
      Tabs            =   4
      TabsPerRow      =   4
      TabHeight       =   706
      TabCaption(0)   =   "Inputs"
      TabPicture(0)   =   "AirSource.frx":030A
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "Frame0"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).ControlCount=   1
      TabCaption(1)   =   "Input Files"
      TabPicture(1)   =   "AirSource.frx":0326
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "Frame1"
      Tab(1).Control(0).Enabled=   0   'False
      Tab(1).ControlCount=   1
      TabCaption(2)   =   "Age Groups"
      TabPicture(2)   =   "AirSource.frx":0342
      Tab(2).ControlEnabled=   0   'False
      Tab(2).Control(0)=   "ageTab"
      Tab(2).Control(0).Enabled=   0   'False
      Tab(2).ControlCount=   1
      TabCaption(3)   =   "Food Production"
      TabPicture(3)   =   "AirSource.frx":035E
      Tab(3).ControlEnabled=   0   'False
      Tab(3).Control(0)=   "foodTab"
      Tab(3).Control(0).Enabled=   0   'False
      Tab(3).ControlCount=   1
      Begin VB.Frame Frame0 
         Height          =   5535
         Left            =   240
         TabIndex        =   56
         Top             =   600
         Width           =   9375
         Begin VB.ComboBox Combo 
            Height          =   315
            ItemData        =   "AirSource.frx":037A
            Left            =   2280
            List            =   "AirSource.frx":038A
            Style           =   2  'Dropdown List
            TabIndex        =   83
            Tag             =   "IremSv"
            Top             =   2400
            Width           =   1335
         End
         Begin VB.CheckBox IPathNuclide 
            Caption         =   "Provide results by pathway and by nuclide"
            Height          =   255
            Left            =   360
            TabIndex        =   82
            Tag             =   "IPthNuc"
            Top             =   1920
            Width           =   4815
         End
         Begin VB.OptionButton RType 
            Caption         =   "Include Atmospheric Dispersion and Deposition Estimates"
            Height          =   195
            Index           =   1
            Left            =   360
            TabIndex        =   63
            Tag             =   "CETOXT"
            Top             =   960
            Value           =   -1  'True
            Width           =   8700
         End
         Begin VB.OptionButton RType 
            Caption         =   "Include Population Dose/Risk Estimates (requires a file of population distribution around the release site)"
            Height          =   195
            Index           =   2
            Left            =   360
            TabIndex        =   62
            Tag             =   "CETOXT"
            Top             =   1440
            Width           =   8700
         End
         Begin VB.OptionButton RType 
            Caption         =   "Provide only Individual Dose/Risk Results"
            Height          =   195
            Index           =   0
            Left            =   360
            TabIndex        =   61
            Tag             =   "CETOXT"
            Top             =   480
            Width           =   8700
         End
         Begin VB.TextBox txt 
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   4
            Left            =   3120
            TabIndex        =   60
            Tag             =   "FacNam"
            Text            =   "Facility Name"
            Top             =   3360
            Width           =   5500
         End
         Begin VB.TextBox txt 
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   5
            Left            =   3120
            TabIndex        =   59
            Tag             =   "FacStrt"
            Text            =   "Street Address"
            Top             =   3840
            Width           =   5500
         End
         Begin VB.TextBox txt 
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   6
            Left            =   3120
            TabIndex        =   58
            Tag             =   "FacCity"
            Text            =   "City, State, ZIP"
            Top             =   4320
            Width           =   5500
         End
         Begin VB.TextBox txt 
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   7
            Left            =   3120
            TabIndex        =   57
            Tag             =   "UsrNam"
            Text            =   "User Name"
            Top             =   4800
            Width           =   5500
         End
         Begin VB.Label Label1 
            Caption         =   "Select reporting units"
            Height          =   255
            Index           =   6
            Left            =   360
            TabIndex        =   81
            Tag             =   "directions"
            Top             =   2400
            Width           =   1890
         End
         Begin VB.Label lbl 
            Caption         =   "Input Facility Name"
            Height          =   300
            Index           =   4
            Left            =   360
            TabIndex        =   67
            Top             =   3360
            Width           =   2700
         End
         Begin VB.Label lbl 
            Caption         =   "Input Facility Mailing Address"
            Height          =   300
            Index           =   5
            Left            =   360
            TabIndex        =   66
            Top             =   3840
            Width           =   2700
         End
         Begin VB.Label lbl 
            Caption         =   "Input Facility City, State, ZIP Code"
            Height          =   300
            Index           =   6
            Left            =   360
            TabIndex        =   65
            Top             =   4320
            Width           =   2700
         End
         Begin VB.Label lbl 
            Caption         =   "Input User Name"
            Height          =   300
            Index           =   7
            Left            =   360
            TabIndex        =   64
            Top             =   4800
            Width           =   2700
         End
         Begin VB.Line Line 
            Index           =   2
            X1              =   120
            X2              =   9000
            Y1              =   3000
            Y2              =   3000
         End
      End
      Begin VB.Frame Frame1 
         Height          =   5535
         Left            =   -74760
         TabIndex        =   34
         Top             =   600
         Width           =   9375
         Begin VB.CheckBox IFoodProd 
            Caption         =   "Use food production file"
            Height          =   255
            Left            =   240
            TabIndex        =   80
            Tag             =   "IFodPrd"
            Top             =   3000
            Value           =   1  'Checked
            Width           =   2655
         End
         Begin VB.CheckBox Check 
            Caption         =   "Grains"
            Height          =   255
            Index           =   7
            Left            =   4920
            TabIndex        =   49
            Top             =   4680
            Value           =   1  'Checked
            Width           =   1695
         End
         Begin VB.CheckBox Check 
            Caption         =   "Fruits"
            Height          =   255
            Index           =   6
            Left            =   4920
            TabIndex        =   48
            Top             =   4440
            Value           =   1  'Checked
            Width           =   1695
         End
         Begin VB.CheckBox Check 
            Caption         =   "Root Vegetables"
            Height          =   255
            Index           =   5
            Left            =   4920
            TabIndex        =   47
            Top             =   4200
            Value           =   1  'Checked
            Width           =   1695
         End
         Begin VB.CheckBox Check 
            Caption         =   "Leafy Vegetables"
            Height          =   255
            Index           =   4
            Left            =   4920
            TabIndex        =   46
            Top             =   3960
            Value           =   1  'Checked
            Width           =   1695
         End
         Begin VB.CheckBox Check 
            Caption         =   "Eggs"
            Height          =   255
            Index           =   3
            Left            =   2880
            TabIndex        =   45
            Top             =   4680
            Value           =   1  'Checked
            Width           =   1695
         End
         Begin VB.CheckBox Check 
            Caption         =   "Milk"
            Height          =   255
            Index           =   2
            Left            =   2880
            TabIndex        =   44
            Top             =   4440
            Value           =   1  'Checked
            Width           =   1695
         End
         Begin VB.CheckBox Check 
            Caption         =   "Poultry"
            Height          =   255
            Index           =   1
            Left            =   2880
            TabIndex        =   43
            Top             =   4200
            Value           =   1  'Checked
            Width           =   1695
         End
         Begin VB.CheckBox Check 
            Caption         =   "Meat"
            Height          =   255
            Index           =   0
            Left            =   2880
            TabIndex        =   42
            Top             =   3960
            Value           =   1  'Checked
            Width           =   1695
         End
         Begin VB.ComboBox numAgeGroups 
            Height          =   315
            ItemData        =   "AirSource.frx":03A2
            Left            =   2880
            List            =   "AirSource.frx":03B8
            Style           =   2  'Dropdown List
            TabIndex        =   41
            Top             =   1560
            Width           =   855
         End
         Begin VB.CommandButton Command2 
            Caption         =   "Open Production file"
            Height          =   372
            Left            =   6480
            TabIndex        =   40
            Top             =   3000
            Width           =   2292
         End
         Begin VB.TextBox txt 
            Height          =   288
            Index           =   3
            Left            =   2880
            TabIndex        =   39
            Tag             =   "FoodFile"
            Top             =   3480
            Width           =   5892
         End
         Begin VB.TextBox txt 
            Height          =   288
            Index           =   0
            Left            =   2880
            TabIndex        =   38
            Tag             =   "PFilNam"
            Top             =   2040
            Width           =   5892
         End
         Begin VB.CommandButton Command1 
            Caption         =   "Open Population File"
            Height          =   372
            Left            =   6480
            TabIndex        =   37
            Top             =   1560
            Width           =   2292
         End
         Begin VB.TextBox txt 
            Height          =   285
            Index           =   1
            Left            =   2880
            TabIndex        =   36
            Tag             =   "distances"
            Top             =   360
            Width           =   735
         End
         Begin VB.TextBox txt 
            Height          =   285
            Index           =   2
            Left            =   2880
            TabIndex        =   35
            Tag             =   "directions"
            Top             =   720
            Width           =   735
         End
         Begin VB.Label Label1 
            Caption         =   "Food Production Products"
            Height          =   255
            Index           =   5
            Left            =   240
            TabIndex        =   55
            Tag             =   "distances"
            Top             =   3960
            Width           =   2490
         End
         Begin VB.Line Line 
            Index           =   1
            X1              =   120
            X2              =   9000
            Y1              =   1200
            Y2              =   1200
         End
         Begin VB.Line Line 
            Index           =   0
            X1              =   120
            X2              =   9000
            Y1              =   2640
            Y2              =   2640
         End
         Begin VB.Label Label1 
            Caption         =   "Number of population age groups"
            Height          =   255
            Index           =   4
            Left            =   240
            TabIndex        =   54
            Tag             =   "distances"
            Top             =   1560
            Width           =   2490
         End
         Begin VB.Label Label1 
            Caption         =   "Food Production filename"
            Height          =   255
            Index           =   3
            Left            =   240
            TabIndex        =   53
            Tag             =   "distances"
            Top             =   3480
            Width           =   2490
         End
         Begin VB.Label Label1 
            Caption         =   "Population filename"
            Height          =   255
            Index           =   1
            Left            =   240
            TabIndex        =   52
            Tag             =   "distances"
            Top             =   2040
            Width           =   2490
         End
         Begin VB.Label Label1 
            Caption         =   "Number of distances and units"
            Height          =   255
            Index           =   0
            Left            =   240
            TabIndex        =   51
            Tag             =   "distances"
            Top             =   360
            Width           =   2490
         End
         Begin VB.Label Label1 
            Caption         =   "Number of directions and units"
            Height          =   255
            Index           =   2
            Left            =   240
            TabIndex        =   50
            Tag             =   "directions"
            Top             =   720
            Width           =   2490
         End
      End
      Begin TabDlg.SSTab foodTab 
         Height          =   5655
         Left            =   -74880
         TabIndex        =   2
         Top             =   600
         Width           =   9615
         _ExtentX        =   16960
         _ExtentY        =   9975
         _Version        =   393216
         Style           =   1
         Tabs            =   8
         TabsPerRow      =   8
         TabHeight       =   706
         TabCaption(0)   =   "Meat"
         TabPicture(0)   =   "AirSource.frx":03CE
         Tab(0).ControlEnabled=   -1  'True
         Tab(0).Control(0)=   "Label4(0)"
         Tab(0).Control(0).Enabled=   0   'False
         Tab(0).Control(1)=   "fpSpread(1)"
         Tab(0).Control(1).Enabled=   0   'False
         Tab(0).Control(2)=   "unit(0)"
         Tab(0).Control(2).Enabled=   0   'False
         Tab(0).ControlCount=   3
         TabCaption(1)   =   "Poultry"
         TabPicture(1)   =   "AirSource.frx":03EA
         Tab(1).ControlEnabled=   0   'False
         Tab(1).Control(0)=   "unit(1)"
         Tab(1).Control(1)=   "fpSpread(2)"
         Tab(1).Control(2)=   "Label4(1)"
         Tab(1).ControlCount=   3
         TabCaption(2)   =   "Milk"
         TabPicture(2)   =   "AirSource.frx":0406
         Tab(2).ControlEnabled=   0   'False
         Tab(2).Control(0)=   "unit(2)"
         Tab(2).Control(1)=   "fpSpread(3)"
         Tab(2).Control(2)=   "Label4(2)"
         Tab(2).ControlCount=   3
         TabCaption(3)   =   "Eggs"
         TabPicture(3)   =   "AirSource.frx":0422
         Tab(3).ControlEnabled=   0   'False
         Tab(3).Control(0)=   "Label4(3)"
         Tab(3).Control(1)=   "fpSpread(4)"
         Tab(3).Control(2)=   "unit(3)"
         Tab(3).ControlCount=   3
         TabCaption(4)   =   "Leafy vegetables"
         TabPicture(4)   =   "AirSource.frx":043E
         Tab(4).ControlEnabled=   0   'False
         Tab(4).Control(0)=   "unit(4)"
         Tab(4).Control(1)=   "fpSpread(5)"
         Tab(4).Control(2)=   "Label4(4)"
         Tab(4).ControlCount=   3
         TabCaption(5)   =   "Root vegetables"
         TabPicture(5)   =   "AirSource.frx":045A
         Tab(5).ControlEnabled=   0   'False
         Tab(5).Control(0)=   "unit(5)"
         Tab(5).Control(1)=   "fpSpread(6)"
         Tab(5).Control(2)=   "Label4(5)"
         Tab(5).ControlCount=   3
         TabCaption(6)   =   "Fruits"
         TabPicture(6)   =   "AirSource.frx":0476
         Tab(6).ControlEnabled=   0   'False
         Tab(6).Control(0)=   "unit(6)"
         Tab(6).Control(1)=   "fpSpread(7)"
         Tab(6).Control(2)=   "Label4(6)"
         Tab(6).ControlCount=   3
         TabCaption(7)   =   "Grains"
         TabPicture(7)   =   "AirSource.frx":0492
         Tab(7).ControlEnabled=   0   'False
         Tab(7).Control(0)=   "unit(7)"
         Tab(7).Control(1)=   "fpSpread(8)"
         Tab(7).Control(2)=   "Label4(7)"
         Tab(7).ControlCount=   3
         Begin VB.ComboBox unit 
            Enabled         =   0   'False
            Height          =   315
            Index           =   7
            Left            =   -73080
            Style           =   2  'Dropdown List
            TabIndex        =   84
            Tag             =   "kg"
            Top             =   480
            Width           =   1452
         End
         Begin VB.ComboBox unit 
            Enabled         =   0   'False
            Height          =   315
            Index           =   6
            Left            =   -73080
            Style           =   2  'Dropdown List
            TabIndex        =   78
            Tag             =   "kg"
            Top             =   480
            Width           =   1452
         End
         Begin VB.ComboBox unit 
            Enabled         =   0   'False
            Height          =   315
            Index           =   5
            Left            =   -73080
            Style           =   2  'Dropdown List
            TabIndex        =   76
            Tag             =   "kg"
            Top             =   480
            Width           =   1452
         End
         Begin VB.ComboBox unit 
            Enabled         =   0   'False
            Height          =   315
            Index           =   4
            Left            =   -73080
            Style           =   2  'Dropdown List
            TabIndex        =   74
            Tag             =   "kg"
            Top             =   480
            Width           =   1452
         End
         Begin VB.ComboBox unit 
            Enabled         =   0   'False
            Height          =   315
            Index           =   3
            Left            =   -73080
            Style           =   2  'Dropdown List
            TabIndex        =   72
            Tag             =   "kg"
            Top             =   480
            Width           =   1452
         End
         Begin VB.ComboBox unit 
            Enabled         =   0   'False
            Height          =   315
            Index           =   2
            Left            =   -73080
            Style           =   2  'Dropdown List
            TabIndex        =   70
            Tag             =   "L"
            Top             =   480
            Width           =   1452
         End
         Begin VB.ComboBox unit 
            Enabled         =   0   'False
            Height          =   315
            Index           =   1
            Left            =   -73080
            Style           =   2  'Dropdown List
            TabIndex        =   68
            Tag             =   "kg"
            Top             =   480
            Width           =   1452
         End
         Begin VB.ComboBox unit 
            Enabled         =   0   'False
            Height          =   315
            Index           =   20
            Left            =   -69600
            Style           =   2  'Dropdown List
            TabIndex        =   8
            Tag             =   "1/m^2"
            Top             =   480
            Width           =   1452
         End
         Begin VB.ComboBox unit 
            Enabled         =   0   'False
            Height          =   315
            Index           =   21
            Left            =   -69600
            Style           =   2  'Dropdown List
            TabIndex        =   7
            Tag             =   "1/m^2"
            Top             =   480
            Width           =   1452
         End
         Begin VB.ComboBox unit 
            Enabled         =   0   'False
            Height          =   315
            Index           =   22
            Left            =   -69600
            Style           =   2  'Dropdown List
            TabIndex        =   6
            Tag             =   "1/m^2"
            Top             =   480
            Width           =   1452
         End
         Begin VB.ComboBox unit 
            Enabled         =   0   'False
            Height          =   315
            Index           =   23
            Left            =   -69600
            Style           =   2  'Dropdown List
            TabIndex        =   5
            Tag             =   "1/m^2"
            Top             =   480
            Width           =   1452
         End
         Begin VB.ComboBox unit 
            Enabled         =   0   'False
            Height          =   315
            Index           =   24
            Left            =   -69600
            Style           =   2  'Dropdown List
            TabIndex        =   4
            Tag             =   "1/m^2"
            Top             =   480
            Width           =   1452
         End
         Begin VB.ComboBox unit 
            Enabled         =   0   'False
            Height          =   315
            Index           =   0
            Left            =   1920
            Style           =   2  'Dropdown List
            TabIndex        =   3
            Tag             =   "kg"
            Top             =   480
            Width           =   1452
         End
         Begin FPSpreadADO.fpSpread fpSpread 
            Height          =   4695
            Index           =   1
            Left            =   120
            TabIndex        =   9
            Top             =   840
            Width           =   9375
            _Version        =   458752
            _ExtentX        =   16536
            _ExtentY        =   8281
            _StockProps     =   64
            DisplayColHeaders=   0   'False
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            SpreadDesigner  =   "AirSource.frx":04AE
         End
         Begin FPSpreadADO.fpSpread fpSpread 
            Height          =   4695
            Index           =   19
            Left            =   -74880
            TabIndex        =   10
            Top             =   840
            Width           =   9375
            _Version        =   458752
            _ExtentX        =   16536
            _ExtentY        =   8281
            _StockProps     =   64
            DisplayColHeaders=   0   'False
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            SpreadDesigner  =   "AirSource.frx":06B7
         End
         Begin FPSpreadADO.fpSpread fpSpread 
            Height          =   4695
            Index           =   20
            Left            =   -74880
            TabIndex        =   11
            Top             =   840
            Width           =   9375
            _Version        =   458752
            _ExtentX        =   16536
            _ExtentY        =   8281
            _StockProps     =   64
            DisplayColHeaders=   0   'False
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            SpreadDesigner  =   "AirSource.frx":08C0
         End
         Begin FPSpreadADO.fpSpread fpSpread 
            Height          =   4695
            Index           =   21
            Left            =   -74880
            TabIndex        =   12
            Top             =   840
            Width           =   9375
            _Version        =   458752
            _ExtentX        =   16536
            _ExtentY        =   8281
            _StockProps     =   64
            DisplayColHeaders=   0   'False
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            SpreadDesigner  =   "AirSource.frx":0AC9
         End
         Begin FPSpreadADO.fpSpread fpSpread 
            Height          =   4695
            Index           =   22
            Left            =   -74880
            TabIndex        =   13
            Top             =   840
            Width           =   9375
            _Version        =   458752
            _ExtentX        =   16536
            _ExtentY        =   8281
            _StockProps     =   64
            DisplayColHeaders=   0   'False
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            SpreadDesigner  =   "AirSource.frx":0CD2
         End
         Begin FPSpreadADO.fpSpread fpSpread 
            Height          =   4695
            Index           =   23
            Left            =   -74880
            TabIndex        =   14
            Top             =   840
            Width           =   9375
            _Version        =   458752
            _ExtentX        =   16536
            _ExtentY        =   8281
            _StockProps     =   64
            DisplayColHeaders=   0   'False
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            SpreadDesigner  =   "AirSource.frx":0EDB
         End
         Begin FPSpreadADO.fpSpread fpSpread 
            Height          =   4695
            Index           =   2
            Left            =   -74880
            TabIndex        =   15
            Top             =   840
            Width           =   9375
            _Version        =   458752
            _ExtentX        =   16536
            _ExtentY        =   8281
            _StockProps     =   64
            DisplayColHeaders=   0   'False
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            SpreadDesigner  =   "AirSource.frx":10E4
         End
         Begin FPSpreadADO.fpSpread fpSpread 
            Height          =   4695
            Index           =   3
            Left            =   -74880
            TabIndex        =   16
            Top             =   840
            Width           =   9375
            _Version        =   458752
            _ExtentX        =   16536
            _ExtentY        =   8281
            _StockProps     =   64
            DisplayColHeaders=   0   'False
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            SpreadDesigner  =   "AirSource.frx":12ED
         End
         Begin FPSpreadADO.fpSpread fpSpread 
            Height          =   4695
            Index           =   4
            Left            =   -74880
            TabIndex        =   17
            Top             =   840
            Width           =   9375
            _Version        =   458752
            _ExtentX        =   16536
            _ExtentY        =   8281
            _StockProps     =   64
            DisplayColHeaders=   0   'False
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            SpreadDesigner  =   "AirSource.frx":14F6
         End
         Begin FPSpreadADO.fpSpread fpSpread 
            Height          =   4695
            Index           =   5
            Left            =   -74880
            TabIndex        =   18
            Top             =   840
            Width           =   9375
            _Version        =   458752
            _ExtentX        =   16536
            _ExtentY        =   8281
            _StockProps     =   64
            DisplayColHeaders=   0   'False
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            SpreadDesigner  =   "AirSource.frx":16FF
         End
         Begin FPSpreadADO.fpSpread fpSpread 
            Height          =   4695
            Index           =   6
            Left            =   -74880
            TabIndex        =   19
            Top             =   840
            Width           =   9375
            _Version        =   458752
            _ExtentX        =   16536
            _ExtentY        =   8281
            _StockProps     =   64
            DisplayColHeaders=   0   'False
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            SpreadDesigner  =   "AirSource.frx":1908
         End
         Begin FPSpreadADO.fpSpread fpSpread 
            Height          =   4695
            Index           =   7
            Left            =   -74880
            TabIndex        =   20
            Top             =   840
            Width           =   9375
            _Version        =   458752
            _ExtentX        =   16536
            _ExtentY        =   8281
            _StockProps     =   64
            DisplayColHeaders=   0   'False
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            SpreadDesigner  =   "AirSource.frx":1B11
         End
         Begin FPSpreadADO.fpSpread fpSpread 
            Height          =   4695
            Index           =   8
            Left            =   -74880
            TabIndex        =   85
            Top             =   840
            Width           =   9375
            _Version        =   458752
            _ExtentX        =   16536
            _ExtentY        =   8281
            _StockProps     =   64
            DisplayColHeaders=   0   'False
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            SpreadDesigner  =   "AirSource.frx":1D1A
         End
         Begin VB.Label Label4 
            Caption         =   "Grid values in"
            Height          =   255
            Index           =   7
            Left            =   -74880
            TabIndex        =   86
            Top             =   480
            Width           =   1695
         End
         Begin VB.Label Label4 
            Caption         =   "Grid values in"
            Height          =   255
            Index           =   6
            Left            =   -74880
            TabIndex        =   79
            Top             =   480
            Width           =   1695
         End
         Begin VB.Label Label4 
            Caption         =   "Grid values in"
            Height          =   255
            Index           =   5
            Left            =   -74880
            TabIndex        =   77
            Top             =   480
            Width           =   1695
         End
         Begin VB.Label Label4 
            Caption         =   "Grid values in"
            Height          =   255
            Index           =   4
            Left            =   -74880
            TabIndex        =   75
            Top             =   480
            Width           =   1695
         End
         Begin VB.Label Label4 
            Caption         =   "Grid values in"
            Height          =   255
            Index           =   3
            Left            =   -74880
            TabIndex        =   73
            Top             =   480
            Width           =   1695
         End
         Begin VB.Label Label4 
            Caption         =   "Grid values in"
            Height          =   255
            Index           =   2
            Left            =   -74880
            TabIndex        =   71
            Top             =   480
            Width           =   1695
         End
         Begin VB.Label Label4 
            Caption         =   "Grid values in"
            Height          =   255
            Index           =   1
            Left            =   -74880
            TabIndex        =   69
            Top             =   480
            Width           =   1695
         End
         Begin VB.Label Label4 
            Caption         =   "Please enter radial distances in the first row of the grid.  Grid values in"
            Height          =   255
            Index           =   18
            Left            =   -74880
            TabIndex        =   26
            Top             =   480
            Width           =   5295
         End
         Begin VB.Label Label4 
            Caption         =   "Please enter radial distances in the first row of the grid.  Grid values in"
            Height          =   255
            Index           =   19
            Left            =   -74880
            TabIndex        =   25
            Top             =   480
            Width           =   5295
         End
         Begin VB.Label Label4 
            Caption         =   "Please enter radial distances in the first row of the grid.  Grid values in"
            Height          =   255
            Index           =   20
            Left            =   -74880
            TabIndex        =   24
            Top             =   480
            Width           =   5295
         End
         Begin VB.Label Label4 
            Caption         =   "Please enter radial distances in the first row of the grid.  Grid values in"
            Height          =   255
            Index           =   21
            Left            =   -74880
            TabIndex        =   23
            Top             =   480
            Width           =   5295
         End
         Begin VB.Label Label4 
            Caption         =   "Please enter radial distances in the first row of the grid.  Grid values in"
            Height          =   255
            Index           =   22
            Left            =   -74880
            TabIndex        =   22
            Top             =   480
            Width           =   5295
         End
         Begin VB.Label Label4 
            Caption         =   "Grid values in"
            Height          =   255
            Index           =   0
            Left            =   120
            TabIndex        =   21
            Top             =   480
            Width           =   1695
         End
      End
      Begin TabDlg.SSTab ageTab 
         Height          =   5655
         Left            =   -74880
         TabIndex        =   27
         Top             =   600
         Width           =   9615
         _ExtentX        =   16960
         _ExtentY        =   9975
         _Version        =   393216
         Style           =   1
         Tabs            =   6
         TabsPerRow      =   6
         TabHeight       =   706
         TabCaption(0)   =   "Age Group1"
         TabPicture(0)   =   "AirSource.frx":1F23
         Tab(0).ControlEnabled=   -1  'True
         Tab(0).Control(0)=   "fpSpread(11)"
         Tab(0).Control(0).Enabled=   0   'False
         Tab(0).ControlCount=   1
         TabCaption(1)   =   "Age Group2"
         TabPicture(1)   =   "AirSource.frx":1F3F
         Tab(1).ControlEnabled=   0   'False
         Tab(1).Control(0)=   "fpSpread(12)"
         Tab(1).ControlCount=   1
         TabCaption(2)   =   "Age Group3"
         TabPicture(2)   =   "AirSource.frx":1F5B
         Tab(2).ControlEnabled=   0   'False
         Tab(2).Control(0)=   "fpSpread(13)"
         Tab(2).ControlCount=   1
         TabCaption(3)   =   "Age Group4"
         TabPicture(3)   =   "AirSource.frx":1F77
         Tab(3).ControlEnabled=   0   'False
         Tab(3).Control(0)=   "fpSpread(14)"
         Tab(3).ControlCount=   1
         TabCaption(4)   =   "Age Group5"
         TabPicture(4)   =   "AirSource.frx":1F93
         Tab(4).ControlEnabled=   0   'False
         Tab(4).Control(0)=   "fpSpread(15)"
         Tab(4).ControlCount=   1
         TabCaption(5)   =   "Age Group6"
         TabPicture(5)   =   "AirSource.frx":1FAF
         Tab(5).ControlEnabled=   0   'False
         Tab(5).Control(0)=   "fpSpread(16)"
         Tab(5).ControlCount=   1
         Begin FPSpreadADO.fpSpread fpSpread 
            Height          =   4695
            Index           =   11
            Left            =   120
            TabIndex        =   28
            Top             =   720
            Width           =   9375
            _Version        =   458752
            _ExtentX        =   16536
            _ExtentY        =   8281
            _StockProps     =   64
            DisplayColHeaders=   0   'False
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            SpreadDesigner  =   "AirSource.frx":1FCB
         End
         Begin FPSpreadADO.fpSpread fpSpread 
            Height          =   4695
            Index           =   12
            Left            =   -74880
            TabIndex        =   29
            Top             =   720
            Width           =   9375
            _Version        =   458752
            _ExtentX        =   16536
            _ExtentY        =   8281
            _StockProps     =   64
            DisplayColHeaders=   0   'False
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            SpreadDesigner  =   "AirSource.frx":21D4
         End
         Begin FPSpreadADO.fpSpread fpSpread 
            Height          =   4695
            Index           =   13
            Left            =   -74880
            TabIndex        =   30
            Top             =   720
            Width           =   9375
            _Version        =   458752
            _ExtentX        =   16536
            _ExtentY        =   8281
            _StockProps     =   64
            DisplayColHeaders=   0   'False
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            SpreadDesigner  =   "AirSource.frx":23DD
         End
         Begin FPSpreadADO.fpSpread fpSpread 
            Height          =   4695
            Index           =   14
            Left            =   -74880
            TabIndex        =   31
            Top             =   720
            Width           =   9375
            _Version        =   458752
            _ExtentX        =   16536
            _ExtentY        =   8281
            _StockProps     =   64
            DisplayColHeaders=   0   'False
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            SpreadDesigner  =   "AirSource.frx":25E6
         End
         Begin FPSpreadADO.fpSpread fpSpread 
            Height          =   4695
            Index           =   15
            Left            =   -74880
            TabIndex        =   32
            Top             =   720
            Width           =   9375
            _Version        =   458752
            _ExtentX        =   16536
            _ExtentY        =   8281
            _StockProps     =   64
            DisplayColHeaders=   0   'False
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            SpreadDesigner  =   "AirSource.frx":27EF
         End
         Begin FPSpreadADO.fpSpread fpSpread 
            Height          =   4695
            Index           =   16
            Left            =   -74880
            TabIndex        =   33
            Top             =   720
            Width           =   9375
            _Version        =   458752
            _ExtentX        =   16536
            _ExtentY        =   8281
            _StockProps     =   64
            DisplayColHeaders=   0   'False
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            SpreadDesigner  =   "AirSource.frx":29F8
         End
      End
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   9480
      Top             =   480
      _ExtentX        =   688
      _ExtentY        =   688
      _Version        =   393216
   End
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
      TabIndex        =   0
      TabStop         =   0   'False
      Top             =   6480
      Width           =   9840
   End
   Begin VB.Timer Timer1 
      Interval        =   100
      Left            =   9480
      Top             =   120
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
Attribute VB_Name = "Source"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Dim temp As parmrec
Dim spreadchange As Boolean

Private Sub about_Click()
  frmAbout.Show 1, Source
End Sub

Private Sub Check_Click(Index As Integer)
Dim i As Long

  SSTab1.TabVisible(3) = False
  foodTab.TabVisible(Index) = Check(Index).Value
  For i = 0 To 7
    If txt(1).BackColor = lightGreen And txt(2).BackColor = lightGreen And Check(i).Value = 1 Then SSTab1.TabVisible(3) = True
  Next
End Sub

Private Sub Command1_Click()
  CommonDialog1.ShowOpen
  txt(0).Text = CommonDialog1.fileName
End Sub

Private Sub Command2_Click()
  CommonDialog1.ShowOpen
  txt(3).Text = CommonDialog1.fileName
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

Private Sub fpSpread_Change(Index As Integer, ByVal col As Long, ByVal Row As Long)
  If spreadchange Then Exit Sub
  spreadchange = True
  Dim i As Long
  Dim j As Long
  fpSpread(Index).Row = Row
  fpSpread(Index).col = col
  
  If Row = 1 Then
    For i = 1 To Val(txt(1).Text)
        For j = 1 To 8
            If Not j = Index Then
                fpSpread(j).Row = Row
                fpSpread(j).col = col
                fpSpread(j).Text = Val(fpSpread(Index).Text)
            End If
        Next j
    Next i
  End If
  spreadchange = False
End Sub

Private Sub howto_Click()
  GetHelp
End Sub

Private Sub IFoodProd_Click()
  If IFoodProd.Value = 1 Then
    SetFoodEnable True
  Else
    SetFoodEnable False
  End If
  txt_Change 3
End Sub

Sub SetFoodEnable(enable As Boolean)
Dim i As Long
  
  Command2.Enabled = enable
  Label1(3).Enabled = enable
  Label1(5).Enabled = enable
  txt(3).Enabled = enable
  For i = 0 To 7
    Check(i).Enabled = enable
  Next
End Sub

Private Sub leave_Click()
  Form_Unload 0
End Sub

Private Sub numAgeGroups_Click()
Dim i As Integer

  For i = 0 To 5
    ageTab.TabVisible(i) = False
  Next
  If txt(1).BackColor = lightGreen And txt(2).BackColor = lightGreen Then
    For i = 0 To 5
      If numAgeGroups.ListIndex >= i Then
        ageTab.TabVisible(i) = True
      Else
        ageTab.TabVisible(i) = False
      End If
    Next
    SSTab1.TabVisible(2) = True
  Else
    SSTab1.TabVisible(2) = False
  End If
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
                  Case "Method":       RType(CInt(temp.pval)).Value = True
                  Case "FacNam":       txt(4).Text = temp.pval
                  Case "FacStrt":      txt(5).Text = temp.pval
                  Case "FacCity":      txt(6).Text = temp.pval
                  Case "UsrNam":       txt(7).Text = temp.pval
                  Case "IremSv":       Combo.ListIndex = CInt(temp.pval)
                  Case "IPthNuc":      IPathNuclide.Value = CInt(temp.pval)
                  Case "IFodPrd":      IFoodProd.Value = CInt(temp.pval)
                  Case "PFilNam":      txt(0).Text = temp.pval
                  Case "FoodFile":     txt(3).Text = temp.pval
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
'  SetHelpFile App.Path + "\xq.htm"
  
  For i = 0 To 7 'TES
    get_conversion_items unit(i).Tag, unit(i)
  Next
    
  Combo.ListIndex = 0
  
  RType(0).Value = True
  Loading.Show
  loadprm
  Unload Loading
  If txt(0).Text <> "" Then importpop
  If txt(3).Text <> "" Then importprod
End Sub

Private Sub RType_Click(Index As Integer)
  Select Case Index
  Case 0
    SSTab1.TabVisible(1) = False
    SSTab1.TabVisible(2) = False
    SSTab1.TabVisible(3) = False
    SetIdEnabled False
  Case 1
    SSTab1.TabVisible(1) = False
    SSTab1.TabVisible(2) = False
    SSTab1.TabVisible(3) = False
    SetIdEnabled True
  Case 2
    txt_Change 0
    txt_Change 3
    SSTab1.TabVisible(1) = True
    SetIdEnabled True
  End Select
End Sub

Sub SetIdEnabled(enable As Boolean)
Dim i As Long
  For i = 4 To 7
    lbl(i).Enabled = enable
    txt(i).Enabled = enable
  Next
End Sub

Private Sub save_Click()
  Dim i As Long
  Dim method As Long
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
      
    If RType(0).Value Then method = 0
    If RType(1).Value Then method = 1
    If RType(2).Value Then method = 2
    
    set_parm parm, "Method", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CStr(method)
    write_parmrec fle, parm
    set_parm parm, IPathNuclide.Tag, 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", IPathNuclide.Value
    write_parmrec fle, parm
    set_parm parm, Combo.Tag, 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", Combo.ListIndex
    write_parmrec fle, parm
        
    If RType(1).Value = True Or RType(2).Value = True Then
      'save facitity info
      For i = 4 To 7
        set_parm parm, txt(i).Tag, 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", txt(i).Text
        write_parmrec fle, parm
      Next
    End If
    
    If RType(2).Value = True Then
      'save group population info
      set_parm parm, txt(0).Tag, 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", txt(0).Text
      write_parmrec fle, parm
      save_pop
      'save food production info
      set_parm parm, IFoodProd.Tag, 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", IFoodProd.Value
      write_parmrec fle, parm
      If IFoodProd.Value = 1 Then
        set_parm parm, txt(3).Tag, 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", txt(3).Text
        write_parmrec fle, parm
        save_prod
      End If
    End If
    
    close_parm fle
    CVTFormat = UserFormat
    SetFormat Me
  Else
    PutError "Unable to create transaction file" & RunName & ".GID"
  End If
  EndModule
End Sub

Private Sub SSTab1_Click(PreviousTab As Integer)
Dim i As Long
  
  If SSTab1.Tab = 2 Then ageTab.Tab = 0
  If SSTab1.Tab = 3 Then
    For i = 0 To 7
      If Check(i).Value = 1 Then
        foodTab.Tab = i
        Exit For
      End If
    Next
  End If
End Sub

Private Sub Timer1_Timer()
  mes = Mid(mes, 2) & Mid(mes, 1, 1)
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

Private Sub txt_Change(Index As Integer)
Dim h As Long
Dim i As Long
Dim j As Long

  Select Case Index
  Case 0
    If Dir(txt(Index).Text) <> "" Then importpop
  Case 1
    If Val(txt(Index).Text) > 0 Then
      For i = 1 To 16
        If i = 9 Then i = 11  'skip indices 9-10
        fpSpread(i).MaxCols = Val(txt(Index).Text)
      Next
      txt(Index).BackColor = lightGreen
      For h = 1 To 16
        If h = 9 Then h = 11  'skip indices 9-10
        fpSpread(h).col = 0
        fpSpread(h).Row = 1
        fpSpread(h).Text = " "
        For i = 1 To Val(txt(Index).Text)
          fpSpread(h).col = i
          fpSpread(h).Text = "D" & CStr(i)
        Next
      Next
    
    Else
      txt(Index).BackColor = lightRed
    End If

  Case 2
    If Val(txt(Index).Text) > 0 Then
      For i = 1 To 16
        If i = 9 Then i = 11  'skip indices 9-10
        fpSpread(i).MaxRows = Val(txt(Index).Text) + 1
      Next
      txt(Index).BackColor = lightGreen
      For h = 1 To 16
        If h = 9 Then h = 11  'skip indices 9-10
        fpSpread(h).col = 0
        fpSpread(h).Row = 1
        fpSpread(h).Text = " "
        For i = 1 To Val(txt(Index).Text)
          fpSpread(h).Row = i + 1
          fpSpread(h).Text = 360 / Val(txt(Index).Text) * i
        Next
      Next
    Else
      txt(Index).BackColor = lightRed
    End If
  Case 3
    If Dir(txt(Index).Text) <> "" Then importprod
  End Select
  SSTab1.TabVisible(2) = False
  SSTab1.TabVisible(3) = False
  If txt(1).BackColor = lightGreen And txt(2).BackColor = lightGreen Then
    If numAgeGroups.ListIndex > -1 Then SSTab1.TabVisible(2) = True
    For i = 0 To 7
      If Check(i).Value = 1 Then SSTab1.TabVisible(3) = True
    Next
  End If
End Sub

Private Sub importpop()
Dim fle As csv
Dim dist As Double
Dim i As Long
Dim h As Long
Dim j As Long
Dim numtab As Long
Dim capt As String
Dim a As Long
Dim id As String

  If open_csv(fle, txt(0).Text, F_READ) Then
    numAgeGroups.ListIndex = -1
    id = get_val(fle)
    If id <> "# age groups" Then Exit Sub
    get_line fle
   
    numtab = Val(get_val(fle)) - 1
    txt(1).Text = get_val(fle)
    txt(2).Text = get_val(fle)
    get_line fle
    
    numAgeGroups.ListIndex = numtab
    
    For h = 0 To numAgeGroups.ListIndex
      a = h + 11
      capt = get_val(fle)
      get_line fle
      For i = 1 To Val(txt(2).Text) ' direction
        fpSpread(a).Row = i + 1
         For j = 1 To Val(txt(1).Text)  ' distance
           fpSpread(a).col = j
           fpSpread(a).Text = get_val(fle)
         Next
       get_line fle
      Next
    Next
    close_csv fle
'  Else
'    MsgBox "Error: Failure to read/open file " & txt(0).Text
  End If
End Sub

Private Sub importprod()
Dim fle As csv
Dim dist As Double
Dim i As Long
Dim h As Long
Dim j As Long
Dim numtab As Long
Dim capt As String
Dim a As Long
Dim id As String
  
  If open_csv(fle, txt(3).Text, F_READ) Then
    For i = 0 To 7
      Check(i).Value = 1
      Check(i).Value = 0
    Next
    id = get_val(fle)
    If id <> "# foods" Then Exit Sub
    get_line fle
   
    numtab = Val(get_val(fle))
    txt(1).Text = get_val(fle)
    txt(2).Text = get_val(fle)
    get_line fle
    
    For h = 1 To numtab
      capt = get_val(fle)
      get_line fle
      For i = 7 To 0 Step -1
        If Check(i).Caption = capt Then
          a = i + 1
          Check(i).Value = 1
        End If
      Next
      If a = -1 Then Exit Sub
      For i = 1 To Val(txt(2).Text) ' direction
        fpSpread(a).Row = i + 1
        For j = 1 To Val(txt(1).Text)  ' distance
          fpSpread(a).col = j
          fpSpread(a).Text = get_val(fle)
        Next
        get_line fle
      Next
    Next
    close_csv fle
'  Else
'    MsgBox "Error: Failure to read/open file " & txt(0).Text
  End If
End Sub

Private Sub save_pop()
  Dim fle As csv
  Dim i As Long
  Dim j As Long
  Dim k As Long
  Dim a As Long
  
  If open_csv(fle, txt(0).Text, F_WRITE) Then
    put_sval fle, "# age groups"
    put_sval fle, "# distances"
    put_sval fle, "# directions"
    put_line fle
    
    put_val fle, numAgeGroups.Text
    put_val fle, txt(1).Text
    put_val fle, txt(2).Text
    put_line fle
    
    For i = 0 To numAgeGroups.ListIndex
      a = i + 11
      put_sval fle, "Group" & CStr(a - 10)
      put_line fle
      For k = 2 To Val(txt(2).Text) + 1
        fpSpread(a).Row = k
        For j = 1 To Val(txt(1).Text)
          fpSpread(a).col = j
          put_val fle, fpSpread(a).Text
        Next
        put_line fle
      Next
    Next
    put_line fle
    close_csv fle
  End If
End Sub

Private Sub save_prod()
  Dim fle As csv
  Dim i As Long
  Dim j As Long
  Dim k As Long
  Dim a As Long
  Dim cnt As Long
  
  If open_csv(fle, txt(3).Text, F_WRITE) Then
    put_sval fle, "# foods"
    put_sval fle, "# distances"
    put_sval fle, "# directions"
    put_line fle
    
    cnt = 0
    For i = 0 To 7
      If Check(i).Value = 1 Then cnt = cnt + 1
    Next
    
    put_val fle, cnt
    put_val fle, txt(1).Text
    put_val fle, txt(2).Text
    put_line fle
    
    For i = 0 To 7
      If Check(i).Value = 1 Then
        put_sval fle, Check(i).Caption
        put_sval fle, unit(i).Tag
        put_line fle
        a = i + 1
        For k = 2 To Val(txt(2).Text) + 1
          fpSpread(a).Row = k
          For j = 1 To Val(txt(1).Text)
            fpSpread(a).col = j
            put_val fle, convert(unit(i).Text, unit(i).Tag, Val(fpSpread(a).Text))
          Next
          put_line fle
        Next
      End If
    Next
    put_line fle
    close_csv fle
  End If
End Sub

