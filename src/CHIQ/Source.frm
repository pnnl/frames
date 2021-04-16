VERSION 5.00
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "tabctl32.ocx"
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Object = "{F856EC8B-F03C-4515-BDC6-64CBD617566A}#7.0#0"; "FPSPR70.ocx"
Object = "{86CF1D34-0C5F-11D2-A9FC-0000F8754DA1}#2.0#0"; "mscomct2.ocx"
Begin VB.Form Source 
   BorderStyle     =   1  'Fixed Single
   ClientHeight    =   6750
   ClientLeft      =   9120
   ClientTop       =   630
   ClientWidth     =   9855
   Icon            =   "Source.frx":0000
   KeyPreview      =   -1  'True
   MaxButton       =   0   'False
   MinButton       =   0   'False
   NegotiateMenus  =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   6750
   ScaleWidth      =   9855
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
      Tabs            =   9
      TabsPerRow      =   5
      TabHeight       =   750
      TabCaption(0)   =   "Inputs"
      TabPicture(0)   =   "Source.frx":030A
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "Frame1"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).ControlCount=   1
      TabCaption(1)   =   "Concentration (GAS)"
      TabPicture(1)   =   "Source.frx":0326
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "Label4(0)"
      Tab(1).Control(0).Enabled=   0   'False
      Tab(1).Control(1)=   "fpSpread(0)"
      Tab(1).Control(1).Enabled=   0   'False
      Tab(1).Control(2)=   "unit(3)"
      Tab(1).Control(2).Enabled=   0   'False
      Tab(1).ControlCount=   3
      TabCaption(2)   =   "Total Deposition (GAS)"
      TabPicture(2)   =   "Source.frx":0342
      Tab(2).ControlEnabled=   0   'False
      Tab(2).Control(0)=   "Label4(1)"
      Tab(2).Control(0).Enabled=   0   'False
      Tab(2).Control(1)=   "fpSpread(1)"
      Tab(2).Control(1).Enabled=   0   'False
      Tab(2).Control(2)=   "unit(4)"
      Tab(2).Control(2).Enabled=   0   'False
      Tab(2).ControlCount=   3
      TabCaption(3)   =   "Wet Deposition (GAS)"
      TabPicture(3)   =   "Source.frx":035E
      Tab(3).ControlEnabled=   0   'False
      Tab(3).Control(0)=   "Label4(2)"
      Tab(3).Control(1)=   "fpSpread(2)"
      Tab(3).Control(2)=   "unit(5)"
      Tab(3).ControlCount=   3
      TabCaption(4)   =   "Dry Deposition (GAS)"
      TabPicture(4)   =   "Source.frx":037A
      Tab(4).ControlEnabled=   0   'False
      Tab(4).Control(0)=   "Label4(3)"
      Tab(4).Control(1)=   "fpSpread(3)"
      Tab(4).Control(2)=   "unit(6)"
      Tab(4).ControlCount=   3
      TabCaption(5)   =   "Concentration"
      TabPicture(5)   =   "Source.frx":0396
      Tab(5).ControlEnabled=   0   'False
      Tab(5).Control(0)=   "Label4(4)"
      Tab(5).Control(0).Enabled=   0   'False
      Tab(5).Control(1)=   "fpSpread(4)"
      Tab(5).Control(1).Enabled=   0   'False
      Tab(5).Control(2)=   "unit(7)"
      Tab(5).Control(2).Enabled=   0   'False
      Tab(5).ControlCount=   3
      TabCaption(6)   =   "Total Deposition"
      TabPicture(6)   =   "Source.frx":03B2
      Tab(6).ControlEnabled=   0   'False
      Tab(6).Control(0)=   "Label4(5)"
      Tab(6).Control(0).Enabled=   0   'False
      Tab(6).Control(1)=   "fpSpread(5)"
      Tab(6).Control(1).Enabled=   0   'False
      Tab(6).Control(2)=   "unit(8)"
      Tab(6).Control(2).Enabled=   0   'False
      Tab(6).ControlCount=   3
      TabCaption(7)   =   "Wet Deposition"
      TabPicture(7)   =   "Source.frx":03CE
      Tab(7).ControlEnabled=   0   'False
      Tab(7).Control(0)=   "Label4(6)"
      Tab(7).Control(0).Enabled=   0   'False
      Tab(7).Control(1)=   "fpSpread(6)"
      Tab(7).Control(1).Enabled=   0   'False
      Tab(7).Control(2)=   "unit(9)"
      Tab(7).Control(2).Enabled=   0   'False
      Tab(7).ControlCount=   3
      TabCaption(8)   =   "Dry Deposition"
      TabPicture(8)   =   "Source.frx":03EA
      Tab(8).ControlEnabled=   0   'False
      Tab(8).Control(0)=   "Label4(7)"
      Tab(8).Control(0).Enabled=   0   'False
      Tab(8).Control(1)=   "fpSpread(7)"
      Tab(8).Control(1).Enabled=   0   'False
      Tab(8).Control(2)=   "unit(10)"
      Tab(8).Control(2).Enabled=   0   'False
      Tab(8).ControlCount=   3
      Begin VB.ComboBox unit 
         Enabled         =   0   'False
         Height          =   315
         Index           =   8
         Left            =   -69480
         Style           =   2  'Dropdown List
         TabIndex        =   51
         Tag             =   "1/m^2"
         Top             =   1080
         Width           =   1452
      End
      Begin VB.ComboBox unit 
         Enabled         =   0   'False
         Height          =   315
         Index           =   7
         Left            =   -69480
         Style           =   2  'Dropdown List
         TabIndex        =   50
         Tag             =   "s/m^3"
         Top             =   1080
         Width           =   1452
      End
      Begin VB.ComboBox unit 
         Enabled         =   0   'False
         Height          =   315
         Index           =   10
         Left            =   -69480
         Style           =   2  'Dropdown List
         TabIndex        =   45
         Tag             =   "1/m^2"
         Top             =   1080
         Width           =   1452
      End
      Begin VB.ComboBox unit 
         Enabled         =   0   'False
         Height          =   315
         Index           =   9
         Left            =   -69480
         Style           =   2  'Dropdown List
         TabIndex        =   44
         Tag             =   "1/m^2"
         Top             =   1080
         Width           =   1452
      End
      Begin VB.ComboBox unit 
         Enabled         =   0   'False
         Height          =   315
         Index           =   6
         Left            =   -69480
         Style           =   2  'Dropdown List
         TabIndex        =   23
         Tag             =   "1/m^2"
         Top             =   1080
         Width           =   1452
      End
      Begin VB.ComboBox unit 
         Enabled         =   0   'False
         Height          =   315
         Index           =   5
         Left            =   -69480
         Style           =   2  'Dropdown List
         TabIndex        =   22
         Tag             =   "1/m^2"
         Top             =   1080
         Width           =   1452
      End
      Begin VB.ComboBox unit 
         Enabled         =   0   'False
         Height          =   315
         Index           =   4
         Left            =   -69480
         Style           =   2  'Dropdown List
         TabIndex        =   21
         Tag             =   "1/m^2"
         Top             =   1080
         Width           =   1452
      End
      Begin VB.ComboBox unit 
         Enabled         =   0   'False
         Height          =   315
         Index           =   3
         Left            =   -69480
         Style           =   2  'Dropdown List
         TabIndex        =   20
         Tag             =   "s/m^3"
         Top             =   1080
         Width           =   1452
      End
      Begin FPSpreadADO.fpSpread fpSpread 
         Height          =   4695
         Index           =   1
         Left            =   -74760
         TabIndex        =   2
         Top             =   1440
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
         SpreadDesigner  =   "Source.frx":0406
      End
      Begin FPSpreadADO.fpSpread fpSpread 
         Height          =   4695
         Index           =   0
         Left            =   -74760
         TabIndex        =   3
         Top             =   1440
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
         SpreadDesigner  =   "Source.frx":060F
      End
      Begin FPSpreadADO.fpSpread fpSpread 
         Height          =   4695
         Index           =   2
         Left            =   -74760
         TabIndex        =   4
         Top             =   1440
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
         SpreadDesigner  =   "Source.frx":0818
      End
      Begin FPSpreadADO.fpSpread fpSpread 
         Height          =   4695
         Index           =   3
         Left            =   -74760
         TabIndex        =   19
         Top             =   1440
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
         SpreadDesigner  =   "Source.frx":0A21
      End
      Begin VB.Frame Frame1 
         Height          =   5175
         Left            =   120
         TabIndex        =   5
         Top             =   960
         Width           =   9372
         Begin VB.CheckBox Check1 
            Caption         =   "Dry Deposition"
            Height          =   200
            Index           =   7
            Left            =   480
            TabIndex        =   38
            Top             =   4680
            Width           =   2000
         End
         Begin VB.CheckBox Check1 
            Caption         =   "Wet Deposition"
            Height          =   200
            Index           =   6
            Left            =   480
            TabIndex        =   37
            Top             =   4440
            Width           =   2000
         End
         Begin VB.CheckBox Check1 
            Caption         =   "Total Deposition"
            Height          =   200
            Index           =   5
            Left            =   480
            TabIndex        =   36
            Top             =   4200
            Width           =   2000
         End
         Begin VB.CheckBox Check1 
            Caption         =   "Concentration"
            Height          =   200
            Index           =   4
            Left            =   480
            TabIndex        =   35
            Top             =   3960
            Width           =   2000
         End
         Begin VB.Frame Frame2 
            Caption         =   "Time and Date of Release"
            Height          =   3420
            Left            =   5880
            TabIndex        =   32
            Top             =   1440
            Width           =   2964
            Begin MSComCtl2.MonthView cal 
               Height          =   2370
               Left            =   120
               TabIndex        =   52
               Top             =   960
               Width           =   2700
               _ExtentX        =   4763
               _ExtentY        =   4180
               _Version        =   393216
               ForeColor       =   -2147483630
               BackColor       =   -2147483633
               Appearance      =   1
               StartOfWeek     =   69992449
               CurrentDate     =   38981
            End
            Begin VB.ComboBox hr 
               Height          =   288
               ItemData        =   "Source.frx":0C2A
               Left            =   1248
               List            =   "Source.frx":0C76
               Style           =   2  'Dropdown List
               TabIndex        =   33
               Top             =   480
               Width           =   780
            End
            Begin VB.Label Label12 
               Alignment       =   2  'Center
               Caption         =   "Hour"
               Height          =   255
               Left            =   390
               TabIndex        =   34
               Top             =   480
               Width           =   735
            End
         End
         Begin VB.OptionButton Option1 
            Caption         =   "Chronic X/Q"
            Height          =   252
            Index           =   1
            Left            =   480
            TabIndex        =   25
            Tag             =   "chronic"
            Top             =   720
            Width           =   2500
         End
         Begin VB.OptionButton Option1 
            Caption         =   "Acute X/Q"
            Height          =   252
            Index           =   0
            Left            =   480
            TabIndex        =   24
            Tag             =   "acute"
            Top             =   720
            Value           =   -1  'True
            Width           =   2500
         End
         Begin VB.CheckBox Check1 
            Caption         =   "Total Deposition"
            Height          =   200
            Index           =   1
            Left            =   480
            TabIndex        =   18
            Top             =   2760
            Width           =   1695
         End
         Begin VB.CheckBox Check1 
            Caption         =   "Dry Deposition"
            Height          =   200
            Index           =   3
            Left            =   480
            TabIndex        =   16
            Top             =   3240
            Width           =   2000
         End
         Begin VB.CheckBox Check1 
            Caption         =   "Wet Deposition"
            Height          =   200
            Index           =   2
            Left            =   480
            TabIndex        =   15
            Top             =   3000
            Width           =   2000
         End
         Begin VB.CheckBox Check1 
            Caption         =   "Concentration"
            Height          =   200
            Index           =   0
            Left            =   480
            TabIndex        =   14
            Top             =   2520
            Width           =   2000
         End
         Begin VB.TextBox txt 
            Height          =   288
            Index           =   0
            Left            =   2880
            TabIndex        =   11
            Tag             =   "filename"
            Top             =   1080
            Width           =   5892
         End
         Begin VB.CommandButton Command1 
            Caption         =   "Open X/Q File"
            Height          =   372
            Left            =   6480
            TabIndex        =   10
            Top             =   600
            Width           =   2292
         End
         Begin VB.TextBox txt 
            Height          =   285
            Index           =   1
            Left            =   2880
            TabIndex        =   9
            Tag             =   "distances"
            Top             =   1440
            Width           =   735
         End
         Begin VB.TextBox txt 
            Height          =   285
            Index           =   2
            Left            =   2880
            TabIndex        =   8
            Tag             =   "directions"
            Top             =   1800
            Width           =   735
         End
         Begin VB.ComboBox unit 
            Height          =   288
            Index           =   1
            Left            =   3720
            Style           =   2  'Dropdown List
            TabIndex        =   7
            Tag             =   "m"
            Top             =   1440
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            Enabled         =   0   'False
            Height          =   288
            Index           =   2
            Left            =   3720
            Style           =   2  'Dropdown List
            TabIndex        =   6
            Tag             =   "deg"
            Top             =   1800
            Width           =   1000
         End
         Begin VB.Label Label5 
            Caption         =   "X/Q Output Grid Selection"
            Height          =   255
            Left            =   240
            TabIndex        =   39
            Top             =   3720
            Width           =   2655
         End
         Begin VB.Label Label1 
            Caption         =   "X/Q filename"
            Height          =   252
            Index           =   1
            Left            =   240
            TabIndex        =   27
            Tag             =   "distances"
            Top             =   1080
            Width           =   2496
         End
         Begin VB.Label Label3 
            Caption         =   "Type of Release "
            Height          =   252
            Left            =   240
            TabIndex        =   26
            Top             =   360
            Width           =   2412
         End
         Begin VB.Label Label2 
            Caption         =   "X/Q Output Grid Selection (Gas)"
            Height          =   252
            Left            =   240
            TabIndex        =   17
            Top             =   2160
            Width           =   2496
         End
         Begin VB.Label Label1 
            Caption         =   "Number of distances and units"
            Height          =   252
            Index           =   0
            Left            =   240
            TabIndex        =   13
            Tag             =   "distances"
            Top             =   1440
            Width           =   2496
         End
         Begin VB.Label Label1 
            Caption         =   "Number of directions and units"
            Height          =   252
            Index           =   2
            Left            =   240
            TabIndex        =   12
            Tag             =   "directions"
            Top             =   1800
            Width           =   2496
         End
      End
      Begin FPSpreadADO.fpSpread fpSpread 
         Height          =   4695
         Index           =   4
         Left            =   -74760
         TabIndex        =   46
         Top             =   1440
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
         SpreadDesigner  =   "Source.frx":0CD0
      End
      Begin FPSpreadADO.fpSpread fpSpread 
         Height          =   4695
         Index           =   5
         Left            =   -74760
         TabIndex        =   47
         Top             =   1440
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
         SpreadDesigner  =   "Source.frx":0ED9
      End
      Begin FPSpreadADO.fpSpread fpSpread 
         Height          =   4695
         Index           =   6
         Left            =   -74760
         TabIndex        =   48
         Top             =   1440
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
         SpreadDesigner  =   "Source.frx":10E2
      End
      Begin FPSpreadADO.fpSpread fpSpread 
         Height          =   4695
         Index           =   7
         Left            =   -74760
         TabIndex        =   49
         Top             =   1440
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
         SpreadDesigner  =   "Source.frx":12EB
      End
      Begin VB.Label Label4 
         Caption         =   "Please enter radial distances in the first row of the grid.  Grid values in"
         Height          =   255
         Index           =   7
         Left            =   -74760
         TabIndex        =   43
         Top             =   1080
         Width           =   5295
      End
      Begin VB.Label Label4 
         Caption         =   "Please enter radial distances in the first row of the grid.  Grid values in"
         Height          =   255
         Index           =   6
         Left            =   -74760
         TabIndex        =   42
         Top             =   1080
         Width           =   5295
      End
      Begin VB.Label Label4 
         Caption         =   "Please enter radial distances in the first row of the grid.  Grid values in"
         Height          =   255
         Index           =   5
         Left            =   -74760
         TabIndex        =   41
         Top             =   1080
         Width           =   5295
      End
      Begin VB.Label Label4 
         Caption         =   "Please enter radial distances in the first row of the grid.  Grid values in"
         Height          =   255
         Index           =   4
         Left            =   -74760
         TabIndex        =   40
         Top             =   1080
         Width           =   5295
      End
      Begin VB.Label Label4 
         Caption         =   "Please enter radial distances in the first row of the grid.  Grid values in"
         Height          =   255
         Index           =   3
         Left            =   -74760
         TabIndex        =   31
         Top             =   1080
         Width           =   5295
      End
      Begin VB.Label Label4 
         Caption         =   "Please enter radial distances in the first row of the grid.  Grid values in"
         Height          =   255
         Index           =   2
         Left            =   -74760
         TabIndex        =   30
         Top             =   1080
         Width           =   5295
      End
      Begin VB.Label Label4 
         Caption         =   "Please enter radial distances in the first row of the grid.  Grid values in"
         Height          =   255
         Index           =   1
         Left            =   -74760
         TabIndex        =   29
         Top             =   1080
         Width           =   5295
      End
      Begin VB.Label Label4 
         Caption         =   "Please enter radial distances in the first row of the grid.  Grid values in"
         Height          =   255
         Index           =   0
         Left            =   -74760
         TabIndex        =   28
         Top             =   1080
         Width           =   5295
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
      Top             =   6360
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
Dim block As Boolean
Dim mode As String
Dim temp As parmrec
Dim spreadchange As Boolean

Private Const MAX_PATH As Long = 260
Private Const ERROR_FILE_NO_ASSOCIATION As Long = 31
Private Const ERROR_FILE_NOT_FOUND As Long = 2
Private Const ERROR_PATH_NOT_FOUND As Long = 3
Private Const ERROR_FILE_SUCCESS As Long = 32 'my constant
Private Const ERROR_BAD_FORMAT As Long = 11

Private Sub about_Click()
  frmAbout.Show 1, Source
End Sub

Private Sub Check1_Click(Index As Integer)
  
  If block Then Exit Sub
  block = True
  SSTab1.TabVisible(Index + 1) = Check1(Index).Value
  
  
  Select Case Index
  Case 1:
    If Check1(1).Value = 1 Then
      Check1(2).Value = 0
      Check1(3).Value = 0
      SSTab1.TabVisible(3) = Check1(2).Value
      SSTab1.TabVisible(4) = Check1(3).Value
    End If
  Case 2, 3:
    If Check1(2).Value = 1 Or Check1(3).Value = 1 Then
      Check1(1).Value = 0
      SSTab1.TabVisible(2) = Check1(1).Value
    End If
  Case 5:
    If Check1(5).Value = 1 Then
      Check1(6).Value = 0
      Check1(7).Value = 0
      SSTab1.TabVisible(7) = Check1(6).Value
      SSTab1.TabVisible(8) = Check1(7).Value
    End If
  Case 6, 7:
    If Check1(6).Value = 1 Or Check1(7).Value = 1 Then
      Check1(5).Value = 0
      SSTab1.TabVisible(6) = Check1(5).Value
    End If
  End Select
  block = False
End Sub

Private Sub Command1_Click()
  CommonDialog1.ShowOpen
  txt(0).Text = CommonDialog1.fileName
  import
End Sub

Private Sub import()
Dim fle As csv
Dim dist As Double
Dim i As Long
Dim h As Long
Dim j As Long
Dim numtab As Long
Dim capt As String
Dim a As Long
  
  If open_csv(fle, txt(0).Text, F_READ) Then
    get_val fle
    numtab = get_val(fle)
    capt = get_val(fle)
    If capt = "acute" And mode = "acute" Then
      Option1(0).Value = True
    ElseIf capt = "chronic" And mode = "chronic" Then
      Option1(1).Value = True
    Else
      MsgBox "Module type must match!" & vbCrLf & "Current module type is " & mode & ", module type read was " & capt & "."
      Exit Sub
    End If
    get_line fle
    
    get_val fle
    txt(1).Text = get_val(fle)
    get_line fle
    
    get_val fle
    
    For i = 1 To Val(txt(1).Text)
        dist = Val(get_val(fle))
      For h = 0 To 7 'TES
        fpSpread(h).col = i
        fpSpread(h).Row = 1
        fpSpread(h).Text = dist
      Next
    Next
    get_line fle
    
    get_val fle
    txt(2).Text = get_val(fle)
    get_line fle
    
    get_line fle
     
    For a = 0 To 7 'TES
      Check1(a).Value = 0
    Next
    
    For h = 0 To numtab - 1
       capt = get_val(fle)
       For a = 0 To 7 'TES
         Dim ftype As String
         If a < 4 Then
            ftype = " (GAS)"
         Else
            ftype = ""
         End If
         
         If capt = Check1(a).Caption & ftype Then
           Check1(a).Value = 1
           Exit For
         End If
         
       Next
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

Private Sub Command2_Click()
Dim url As String
Dim success As Long
Dim pos As Long
Dim sResult As String
Dim msg As String

    sResult = Space$(1024)

    ' lpFile: name of the file of interest
    ' lpDirectory: location of lpFile
    ' sResult: path and name of executable associated with lpFile
    success = FindExecutable("source.xls", "", sResult)
    Select Case success
      Case ERROR_FILE_NO_ASSOCIATION: msg = "no association"
      Case ERROR_FILE_NOT_FOUND: msg = "file not found"
      Case ERROR_PATH_NOT_FOUND: msg = "path not found"
      Case ERROR_BAD_FORMAT:     msg = "bad format"
      Case Is >= ERROR_FILE_SUCCESS:
        pos = InStr(sResult, Chr$(0))
        If pos Then
          url = Left$(sResult, pos - 1)
          'url = url + " " + ConvertURL(HelpFileName + "#" + UCase(HelpAnchor))
          url = url + " " + "source.xls"
          Shell url, vbNormalFocus
          Exit Sub
        End If
        Exit Sub
    End Select
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
  'TES - replaced the code for this event
  If spreadchange Then Exit Sub
  spreadchange = True
  Dim i As Long
  Dim j As Long
  fpSpread(Index).Row = Row
  fpSpread(Index).col = col
  
  If Row = 1 Then
    For i = 1 To Val(txt(1).Text)
        For j = 0 To 7
            If Not j = Index Then
                fpSpread(j).Row = Row
                fpSpread(j).col = col
                fpSpread(j).Text = Val(fpSpread(Index).Text)
            End If
        Next j
    Next i
  End If
  spreadchange = False
  Exit Sub
  
  'Dim i As Long
  'If spreadchange Then Exit Sub
  'spreadchange = True
  'fpSpread(Index).Row = Row
  'fpSpread(Index).col = col
  'If Row = 1 Then
  '  For i = 0 To 3
            'fpSpread(i).Row = Row
            'fpSpread(i).col = col
            'fpSpread(i).Text = Val(fpSpread(Index).Text)
    'Next i
  'End If
  'spreadchange = False
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
                If temp.idx1 = siteIdx Then
                  If temp.pName = "airdespath" Then DesName = temp.pval
                End If
              End If
            Next
          
          Case modName
            For m = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                Loading.update
                Select Case temp.pName
                  Case "CVTFormat":    CVTFormat = "General Number" 'CVTFormat = temp.pval
                  Case "directions":
                    txt(2).Text = convert(temp.cunit, temp.uunit, Val(temp.pval))
                    Unit(2).Text = temp.uunit
                  Case "distances":
                    txt(1).Text = convert(temp.cunit, temp.uunit, Val(temp.pval))
                    Unit(1).Text = temp.uunit
                  Case "filename":
                    txt(0).Text = temp.pval
                  Case "arstartmonth":               cal.month = Val(temp.pval)
                  Case "arstartday":                 cal.Day = Val(temp.pval)
                  Case "arstartyear":                cal.year = Val(temp.pval)
                  Case "arstarthour":                hr.ListIndex = Val(temp.pval) - 1
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
  StartModule Me, App.Title, 6
'  SetHelpFile App.Path + "\xq.htm"
  
  mode = argv(0)
  If mode = "acute" Then
    Option1(0).Value = True
    Option1(1).Visible = False
    Frame2.Visible = True
  ElseIf mode = "chronic" Then
    Option1(1).Value = True
    Option1(0).Visible = False
    Frame2.Visible = False
  Else
    PutError "Unknown module type, must be acute or chronic see des file"
    EndModule
  End If
  For i = 1 To 10 'TES
    get_conversion_items Unit(i).Tag, Unit(i)
  Next
  
  For i = 1 To 8 'TES
    SSTab1.TabVisible(i) = False
  Next
  
  Loading.Show
  loadprm
  Unload Loading
  If txt(0).Text <> "" Then import
End Sub

Private Sub save_Click()
  Dim i As Long
  Dim fName As String
  Dim parm As parmrec
  Dim fle As parmfile
  Dim UserFormat As String
  UserFormat = CVTFormat
  
  
  If Frame2.Visible Then
    If cal.Day = 0 Then
      MsgBox "Please select a day of the month", vbExclamation, "Date Input Error"
      Exit Sub
    End If
  End If
  
  fName = RunName + ".GID"
  If open_parm(fle, fName, 1) Then
    set_parm parm, "CVTFormat", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CVTFormat
    write_parmrec fle, parm
    CVTFormat = "General Number"
    For i = 1 To 2
      set_parm parm, txt(i).Tag, 0, 0, 0, 0, 0, 0, 0, Unit(i).Text, Unit(i).Tag, convert(Unit(i).Text, Unit(i).Tag, Val(txt(i).Text))
      write_parmrec fle, parm
    Next
    set_parm parm, txt(0).Tag, 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", txt(0).Text
    write_parmrec fle, parm
     
    If Frame2.Visible Then
      set_parm parm, "arstartmonth", 0, 0, 0, 0, 0, 0, 0, "mn", "mn", Str(cal.month)
      write_parmrec fle, parm
      set_parm parm, "arstartday", 0, 0, 0, 0, 0, 0, 0, "day", "day", Str(cal.Day)
      write_parmrec fle, parm
      set_parm parm, "arstartyear", 0, 0, 0, 0, 0, 0, 0, "yr", "yr", Str(cal.year)
      write_parmrec fle, parm
      set_parm parm, "arstarthour", 0, 0, 0, 0, 0, 0, 0, "hr", "hr", Str(hr.ListIndex + 1)
      write_parmrec fle, parm
      set_parm parm, "jhour", 0, 0, 0, 0, 0, 0, 0, "Jhr", "Jhr", Str(Date2JulHours(cal.month, cal.Day, cal.year, hr.ListIndex + 1))
      write_parmrec fle, parm
    End If
    
    close_parm fle
    save_xq
    CVTFormat = UserFormat
    SetFormat Me
  Else
    PutError "Unable to create transaction file" & RunName & ".GID"
  End If
  EndModule
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
  Case 1
    If Val(txt(Index).Text) > 0 Then
      For i = 0 To 7 'TES
        fpSpread(i).MaxCols = Val(txt(Index).Text)
      Next
      txt(Index).BackColor = lightGreen
    Else
      txt(Index).BackColor = lightRed
    End If

  Case 2
    If Val(txt(Index).Text) > 0 Then
      For i = 0 To 7 'TES
        fpSpread(i).MaxRows = Val(txt(Index).Text) + 1
      Next
      txt(Index).BackColor = lightGreen
      For h = 0 To 7 'TES
        fpSpread(h).col = 0
        fpSpread(h).Row = 1
        fpSpread(h).Text = " "
        For i = 1 To Val(txt(Index).Text)
          fpSpread(h).Row = i + 1
          fpSpread(h).Text = 360 / Val(txt(Index).Text) * (i - 1)
        Next
      Next
    Else
      txt(Index).BackColor = lightRed
    End If
  Case Else
  
  End Select

End Sub

Private Sub save_xq()
  Dim fle As csv
  Dim i As Long
  Dim j As Long
  Dim h As Long
  
  If open_csv(fle, txt(0).Text, F_WRITE) Then
    put_val fle, "X/Q products"
    For h = 0 To 7 'TES
      i = i + Check1(h).Value
    Next
       
    put_val fle, i
    If Option1(0).Value Then put_val fle, Option1(0).Tag
    If Option1(1).Value Then put_val fle, Option1(1).Tag
    put_line fle
    
    put_val fle, "Distances and units"
    put_val fle, Val(txt(1).Text)
    put_val fle, "m"
    put_line fle
    
    put_val fle, "Distances"
    For i = 1 To Val(txt(1).Text)
      fpSpread(0).Row = 1
      fpSpread(0).col = i
      put_val fle, convert(Unit(1).Text, "m", Val(fpSpread(0).Text))
    Next
    put_line fle
    
    put_val fle, "Directions and units"
    put_val fle, Val(txt(2).Text)
    put_val fle, "deg"
    put_line fle
    
    put_val fle, "Directions"
    For i = 2 To Val(txt(2).Text) + 1
      fpSpread(0).Row = i
      fpSpread(0).col = 0
      put_val fle, fpSpread(0).Text
    Next
    put_line fle
    
    For h = 0 To 7 'TES
    If Check1(h).Value = 1 Then
      'TES - strip suffix off of tabstrib captions
      'Dim strIndex As Integer
      Dim txtline As String
      txtline = SSTab1.TabCaption(h + 1)
      'strIndex = InStr(1, txtline, "(")
      'txtline = Left(txtline, (strIndex - 2))
      'put_val fle, SSTab1.TabCaption(h + 1)
      put_val fle, txtline
      
      put_val fle, Unit(h + 3).Text
      put_line fle
      For i = 1 To Val(txt(2).Text) ' direction
        fpSpread(h).Row = i + 1
          For j = 1 To Val(txt(1).Text)  ' distance
            fpSpread(h).col = j
            put_val fle, Val(fpSpread(h).Text)
          Next
        put_line fle
      Next
    End If
    Next
    close_csv fle
  End If
End Sub

