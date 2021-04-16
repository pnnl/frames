VERSION 5.00
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "tabctl32.ocx"
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "threed32.ocx"
Begin VB.Form Pathway 
   BorderStyle     =   1  'Fixed Single
   ClientHeight    =   5628
   ClientLeft      =   1428
   ClientTop       =   1476
   ClientWidth     =   7944
   Icon            =   "Pathway.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   NegotiateMenus  =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   469
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   662
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
      TabIndex        =   221
      TabStop         =   0   'False
      Top             =   5280
      Width           =   7890
   End
   Begin TabDlg.SSTab SSTab1 
      Height          =   5250
      Left            =   0
      TabIndex        =   220
      Top             =   0
      Width           =   7935
      _ExtentX        =   13991
      _ExtentY        =   9250
      _Version        =   393216
      Tabs            =   9
      TabHeight       =   529
      TabCaption(0)   =   "Air Exposure"
      TabPicture(0)   =   "Pathway.frx":030A
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "frame(0)"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).Control(1)=   "Timer1"
      Tab(0).Control(1).Enabled=   0   'False
      Tab(0).ControlCount=   2
      TabCaption(1)   =   "External Ground Dose"
      TabPicture(1)   =   "Pathway.frx":0326
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "frame(1)"
      Tab(1).ControlCount=   1
      TabCaption(2)   =   "Terrestrial Food"
      TabPicture(2)   =   "Pathway.frx":0342
      Tab(2).ControlEnabled=   0   'False
      Tab(2).Control(0)=   "frame(2)"
      Tab(2).ControlCount=   1
      TabCaption(3)   =   "Domestic Use"
      TabPicture(3)   =   "Pathway.frx":035E
      Tab(3).ControlEnabled=   0   'False
      Tab(3).Control(0)=   "frame(3)"
      Tab(3).ControlCount=   1
      TabCaption(4)   =   "Swimming"
      TabPicture(4)   =   "Pathway.frx":037A
      Tab(4).ControlEnabled=   0   'False
      Tab(4).Control(0)=   "frame(4)"
      Tab(4).ControlCount=   1
      TabCaption(5)   =   "Boating"
      TabPicture(5)   =   "Pathway.frx":0396
      Tab(5).ControlEnabled=   0   'False
      Tab(5).Control(0)=   "frame(5)"
      Tab(5).ControlCount=   1
      TabCaption(6)   =   "Shoreline"
      TabPicture(6)   =   "Pathway.frx":03B2
      Tab(6).ControlEnabled=   0   'False
      Tab(6).Control(0)=   "frame(6)"
      Tab(6).Control(0).Enabled=   0   'False
      Tab(6).ControlCount=   1
      TabCaption(7)   =   "Soil"
      TabPicture(7)   =   "Pathway.frx":03CE
      Tab(7).ControlEnabled=   0   'False
      Tab(7).Control(0)=   "frame(7)"
      Tab(7).ControlCount=   1
      TabCaption(8)   =   "Aquatic Food"
      TabPicture(8)   =   "Pathway.frx":03EA
      Tab(8).ControlEnabled=   0   'False
      Tab(8).Control(0)=   "frame(8)"
      Tab(8).ControlCount=   1
      Begin VB.Timer Timer1 
         Interval        =   100
         Left            =   0
         Top             =   0
      End
      Begin Threed.SSFrame frame 
         Height          =   2772
         Index           =   0
         Left            =   240
         TabIndex        =   17
         Top             =   1080
         Width           =   7224
         _Version        =   65536
         _ExtentX        =   12742
         _ExtentY        =   4890
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
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   6
            Left            =   4080
            TabIndex        =   15
            Tag             =   "freq"
            Text            =   "1.0"
            Top             =   2280
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   5
            Left            =   4080
            TabIndex        =   12
            Tag             =   "freq"
            Text            =   "1.0"
            Top             =   1800
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   4
            Left            =   4080
            TabIndex        =   9
            Tag             =   "freq"
            Text            =   "1.0"
            Top             =   1320
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   3
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   6
            Tag             =   "m^3/day"
            Top             =   840
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   0
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   2
            Tag             =   "m^3/day"
            Top             =   360
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   3
            Left            =   4080
            TabIndex        =   5
            Tag             =   "ubres"
            Text            =   "20.0"
            Top             =   840
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   0
            Left            =   4080
            TabIndex        =   1
            Tag             =   "ubr"
            Text            =   "20.0"
            Top             =   360
            Width           =   1000
         End
         Begin VB.Label lbl 
            Caption         =   "Annual frequency factor for air external exposure -- IC-FREQEXT"
            Height          =   380
            Index           =   6
            Left            =   120
            TabIndex        =   14
            Tag             =   "ICFREQEXT"
            Top             =   2280
            Width           =   3900
            WordWrap        =   -1  'True
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   6
            Left            =   6120
            TabIndex        =   16
            Tag             =   "0"
            Top             =   2280
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Annual frequency factor for soil resuspension inhalation -- IC-FREQSL"
            Height          =   380
            Index           =   5
            Left            =   120
            TabIndex        =   11
            Tag             =   "ICFREQSL"
            Top             =   1800
            Width           =   3900
            WordWrap        =   -1  'True
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   5
            Left            =   6120
            TabIndex        =   13
            Tag             =   "0"
            Top             =   1800
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Annual frequency factor for air inhalation -- IC-FREQINH"
            Height          =   380
            Index           =   4
            Left            =   120
            TabIndex        =   8
            Tag             =   "ICFREQINH"
            Top             =   1320
            Width           =   3900
            WordWrap        =   -1  'True
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   4
            Left            =   6120
            TabIndex        =   10
            Tag             =   "0"
            Top             =   1320
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   3
            Left            =   6120
            TabIndex        =   7
            Tag             =   "0"
            Top             =   840
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   0
            Left            =   6120
            TabIndex        =   3
            Tag             =   "0"
            Top             =   360
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Inhalation rate for resuspension pathway -- IC-UBRES"
            Height          =   380
            Index           =   3
            Left            =   120
            TabIndex        =   4
            Tag             =   "ICUBRES"
            Top             =   840
            Width           =   3900
         End
         Begin VB.Label lbl 
            Caption         =   "Inhalation rate for general exposure to contaminated air -- IC-UBR"
            Height          =   380
            Index           =   0
            Left            =   120
            TabIndex        =   0
            Tag             =   "ICUBR"
            Top             =   360
            Width           =   3900
         End
      End
      Begin Threed.SSFrame frame 
         Height          =   2832
         Index           =   1
         Left            =   -74760
         TabIndex        =   37
         Top             =   1080
         Width           =   7224
         _Version        =   65536
         _ExtentX        =   12742
         _ExtentY        =   4995
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
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   13
            Left            =   4080
            TabIndex        =   35
            Tag             =   "freq"
            Text            =   "1.0"
            Top             =   2280
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   11
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   32
            Tag             =   "hr/day"
            Top             =   1860
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   11
            Left            =   4080
            TabIndex        =   31
            Tag             =   "uext"
            Text            =   "24.0"
            Top             =   1860
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   10
            Left            =   4080
            TabIndex        =   28
            Tag             =   "sho"
            Text            =   "1.0"
            Top             =   1440
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   9
            Left            =   4080
            TabIndex        =   25
            Tag             =   "fto"
            Text            =   "0.27"
            Top             =   1080
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   8
            Left            =   4080
            TabIndex        =   22
            Tag             =   "shi"
            Text            =   "0.33"
            Top             =   720
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   7
            Left            =   4080
            TabIndex        =   19
            Tag             =   "fti"
            Text            =   "0.73"
            Top             =   360
            Width           =   1000
         End
         Begin VB.Label lbl 
            Caption         =   "Annual frequency factor for soil external exposure -- IC-FREQSLEXT"
            Height          =   384
            Index           =   13
            Left            =   120
            TabIndex        =   34
            Tag             =   "ICFREQSLEXT"
            Top             =   2280
            Width           =   3900
            WordWrap        =   -1  'True
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   13
            Left            =   6120
            TabIndex        =   36
            Tag             =   "0"
            Top             =   2280
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   11
            Left            =   6120
            TabIndex        =   33
            Tag             =   "0"
            Top             =   1860
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   10
            Left            =   6120
            TabIndex        =   29
            Tag             =   "0"
            Top             =   1440
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   9
            Left            =   6120
            TabIndex        =   26
            Tag             =   "0"
            Top             =   1080
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   8
            Left            =   6120
            TabIndex        =   23
            Tag             =   "0"
            Top             =   720
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   7
            Left            =   6120
            TabIndex        =   20
            Tag             =   "0"
            Top             =   360
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Shielding factor for outdoors -- IC-SHO"
            Height          =   252
            Index           =   10
            Left            =   120
            TabIndex        =   27
            Tag             =   "ICSHO"
            Top             =   1440
            Width           =   3900
         End
         Begin VB.Label lbl 
            Caption         =   "Daily exposure time to external radiation for atmospheric and measured soil pathways -- IC-UEXT"
            Height          =   384
            Index           =   11
            Left            =   120
            TabIndex        =   30
            Tag             =   "ICUEXT"
            Top             =   1860
            Width           =   3900
            WordWrap        =   -1  'True
         End
         Begin VB.Label lbl 
            Caption         =   "Fraction of time spent indoors -- IC-FTI"
            Height          =   252
            Index           =   7
            Left            =   120
            TabIndex        =   18
            Tag             =   "ICFTI"
            Top             =   360
            Width           =   3900
         End
         Begin VB.Label lbl 
            Caption         =   "Shielding factor for indoor exposure -- IC-SHI"
            Height          =   252
            Index           =   8
            Left            =   120
            TabIndex        =   21
            Tag             =   "ICSHI"
            Top             =   720
            Width           =   3900
         End
         Begin VB.Label lbl 
            Caption         =   "Fraction of time spent outdoors -- IC-FTO"
            Height          =   252
            Index           =   9
            Left            =   120
            TabIndex        =   24
            Tag             =   "ICFTO"
            Top             =   1080
            Width           =   3900
         End
      End
      Begin Threed.SSFrame frame 
         Height          =   3972
         Index           =   2
         Left            =   -74760
         TabIndex        =   66
         Top             =   1080
         Width           =   7224
         _Version        =   65536
         _ExtentX        =   12742
         _ExtentY        =   7006
         _StockProps     =   14
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Font3D          =   2
         ShadowStyle     =   1
         Enabled         =   0   'False
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   17
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   52
            Tag             =   "L/day"
            Top             =   1440
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   16
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   48
            Tag             =   "kg/day"
            Top             =   1080
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   15
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   44
            Tag             =   "kg/day"
            Top             =   720
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   14
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   40
            Tag             =   "kg/day"
            Top             =   360
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   21
            Left            =   4080
            TabIndex        =   64
            Tag             =   "freq"
            Text            =   "1.0"
            Top             =   3360
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   20
            Left            =   4080
            TabIndex        =   61
            Tag             =   "freq"
            Text            =   "1.0"
            Top             =   2880
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   19
            Left            =   4080
            TabIndex        =   58
            Tag             =   "freq"
            Text            =   "1.0"
            Top             =   2400
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   18
            Left            =   4080
            TabIndex        =   55
            Tag             =   "freq"
            Text            =   "1.0"
            Top             =   1920
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   17
            Left            =   4080
            TabIndex        =   51
            Tag             =   "uag"
            Text            =   "0.075"
            Top             =   1440
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   16
            Left            =   4080
            TabIndex        =   47
            Tag             =   "uag"
            Text            =   "0.065"
            Top             =   1080
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   15
            Left            =   4080
            TabIndex        =   43
            Tag             =   "uag"
            Text            =   "0.130"
            Top             =   720
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   14
            Left            =   4080
            TabIndex        =   39
            Tag             =   "uag"
            Text            =   "0.021"
            Top             =   360
            Width           =   1000
         End
         Begin VB.Label lbl 
            Caption         =   "Annual fequency factor for ingestion of milk -- IC-FREQMK"
            Height          =   380
            Index           =   21
            Left            =   120
            TabIndex        =   63
            Tag             =   "ICFREQMK"
            Top             =   3360
            Width           =   3900
            WordWrap        =   -1  'True
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   21
            Left            =   6120
            TabIndex        =   65
            Tag             =   "0"
            Top             =   3360
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Annual fequency factor for ingestion of meat -- IC-FREQMT"
            Height          =   380
            Index           =   20
            Left            =   120
            TabIndex        =   60
            Tag             =   "ICFREQMT"
            Top             =   2880
            Width           =   3900
            WordWrap        =   -1  'True
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   20
            Left            =   6120
            TabIndex        =   62
            Tag             =   "0"
            Top             =   2880
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Annual fequency factor for ingestion of other vegetables -- IC-FREQVEG"
            Height          =   380
            Index           =   19
            Left            =   120
            TabIndex        =   57
            Tag             =   "ICFREQVEG"
            Top             =   2400
            Width           =   3900
            WordWrap        =   -1  'True
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   19
            Left            =   6120
            TabIndex        =   59
            Tag             =   "0"
            Top             =   2400
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   18
            Left            =   6120
            TabIndex        =   56
            Tag             =   "0"
            Top             =   1920
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Annual fequency factor for ingestion of leafy vegetables -- IC-FREQLEAF"
            Height          =   380
            Index           =   18
            Left            =   120
            TabIndex        =   54
            Tag             =   "ICFREQLEAF"
            Top             =   1920
            Width           =   3900
            WordWrap        =   -1  'True
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   17
            Left            =   6120
            TabIndex        =   53
            Tag             =   "0"
            Top             =   1440
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   16
            Left            =   6120
            TabIndex        =   49
            Tag             =   "0"
            Top             =   1080
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   15
            Left            =   6120
            TabIndex        =   45
            Tag             =   "0"
            Top             =   720
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   14
            Left            =   6120
            TabIndex        =   41
            Tag             =   "0"
            Top             =   360
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Ingestion rate of milk -- IC-UAGMK"
            Height          =   252
            Index           =   17
            Left            =   120
            TabIndex        =   50
            Tag             =   "ICUAGMK"
            Top             =   1440
            Width           =   3900
         End
         Begin VB.Label lbl 
            Caption         =   "Ingestion rate of other vegetables -- IC-UAGVEG"
            Height          =   252
            Index           =   15
            Left            =   120
            TabIndex        =   42
            Tag             =   "ICUAGVEG"
            Top             =   720
            Width           =   3900
         End
         Begin VB.Label lbl 
            Caption         =   "Ingestion rate of leafy vegetables -- IC-UAGLEAF"
            Height          =   252
            Index           =   14
            Left            =   120
            TabIndex        =   38
            Tag             =   "ICUAGLEAF"
            Top             =   360
            Width           =   3900
         End
         Begin VB.Label lbl 
            Caption         =   "Ingestion rate of meat -- IC-UAGMT"
            Height          =   252
            Index           =   16
            Left            =   120
            TabIndex        =   46
            Tag             =   "ICUAGMT"
            Top             =   1080
            Width           =   3900
         End
      End
      Begin Threed.SSFrame frame 
         Height          =   3972
         Index           =   3
         Left            =   -74760
         TabIndex        =   99
         Top             =   1080
         Width           =   7224
         _Version        =   65536
         _ExtentX        =   12742
         _ExtentY        =   7006
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
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   36
            Left            =   4080
            TabIndex        =   97
            Tag             =   "freq"
            Text            =   "1.0"
            Top             =   3240
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   35
            Left            =   4080
            TabIndex        =   94
            Tag             =   "freq"
            Text            =   "1.0"
            Top             =   2880
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   34
            Left            =   4080
            TabIndex        =   91
            Tag             =   "freq"
            Text            =   "1.0"
            Top             =   2520
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   33
            Left            =   4080
            TabIndex        =   88
            Tag             =   "freq"
            Text            =   "1.0"
            Top             =   2160
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   28
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   77
            Tag             =   "m^3/day"
            Top             =   1080
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   26
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   69
            Tag             =   "hr"
            Top             =   360
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   31
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   81
            Tag             =   "L/hr"
            Top             =   1440
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   32
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   85
            Tag             =   "cm^2"
            Top             =   1800
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   28
            Left            =   4080
            TabIndex        =   76
            Tag             =   "ubrsh"
            Text            =   "20.0"
            Top             =   1080
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   27
            Left            =   4080
            TabIndex        =   72
            Tag             =   "evshwr"
            Text            =   "1.0"
            Top             =   720
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   26
            Left            =   4080
            TabIndex        =   68
            Tag             =   "teshwr"
            Text            =   "0.167"
            Top             =   360
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   31
            Left            =   4080
            TabIndex        =   80
            Tag             =   "ubw"
            Text            =   "0.06"
            Top             =   1440
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   32
            Left            =   4080
            TabIndex        =   84
            Tag             =   "askin"
            Text            =   "20000.0"
            Top             =   1800
            Width           =   1000
         End
         Begin VB.Label lbl 
            Caption         =   "ev/day"
            Height          =   252
            Index           =   65
            Left            =   5160
            TabIndex        =   73
            Top             =   720
            Width           =   876
         End
         Begin VB.Label lbl 
            Caption         =   "Annual frequency factor for shower or indoor air inhalation -- IC-FREQSRINH"
            Height          =   380
            Index           =   36
            Left            =   120
            TabIndex        =   96
            Tag             =   "ICFREQSRINH"
            Top             =   3240
            Width           =   3900
            WordWrap        =   -1  'True
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   36
            Left            =   6120
            TabIndex        =   98
            Tag             =   "0"
            Top             =   3240
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Annual frequency factor for shower water ingestion -- IC-FREQSRING"
            Height          =   380
            Index           =   35
            Left            =   120
            TabIndex        =   93
            Tag             =   "ICFREQSRING"
            Top             =   2880
            Width           =   3900
            WordWrap        =   -1  'True
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   35
            Left            =   6120
            TabIndex        =   95
            Tag             =   "0"
            Top             =   2880
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Annual frequency factor for shower dermal contact -- IC-FREQSRDER"
            Height          =   380
            Index           =   34
            Left            =   120
            TabIndex        =   90
            Tag             =   "ICFREQSRDER"
            Top             =   2520
            Width           =   3900
            WordWrap        =   -1  'True
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   34
            Left            =   6120
            TabIndex        =   92
            Tag             =   "0"
            Top             =   2520
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Annual frequency factor for drinking water ingestion -- IC-FREQING"
            Height          =   380
            Index           =   33
            Left            =   120
            TabIndex        =   87
            Tag             =   "ICFREQING"
            Top             =   2160
            Width           =   3900
            WordWrap        =   -1  'True
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   33
            Left            =   6120
            TabIndex        =   89
            Tag             =   "0"
            Top             =   2160
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   28
            Left            =   6120
            TabIndex        =   78
            Tag             =   "0"
            Top             =   1080
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   27
            Left            =   6120
            TabIndex        =   74
            Tag             =   "0"
            Top             =   720
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   26
            Left            =   6120
            TabIndex        =   70
            Tag             =   "0"
            Top             =   360
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Duration of one shower event -- IC-TESHWR"
            Height          =   252
            Index           =   26
            Left            =   120
            TabIndex        =   67
            Tag             =   "ICTESHWR"
            Top             =   360
            Width           =   3900
         End
         Begin VB.Label lbl 
            Caption         =   "Frequency of showering -- IC-EVSHWR"
            Height          =   252
            Index           =   27
            Left            =   120
            TabIndex        =   71
            Tag             =   "ICEVSHWR"
            Top             =   720
            Width           =   3900
         End
         Begin VB.Label lbl 
            Caption         =   "Indoor/Showering inhalation rate -- IC-UBRSH"
            Height          =   252
            Index           =   28
            Left            =   120
            TabIndex        =   75
            Tag             =   "ICUBRSH"
            Top             =   1080
            Width           =   3900
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   31
            Left            =   6120
            TabIndex        =   82
            Tag             =   "0"
            Top             =   1440
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   32
            Left            =   6120
            TabIndex        =   86
            Tag             =   "0"
            Top             =   1800
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Water ingestion rate while showering -- IC-UBW"
            Height          =   252
            Index           =   31
            Left            =   120
            TabIndex        =   79
            Tag             =   "ICUBW"
            Top             =   1440
            Width           =   3900
         End
         Begin VB.Label lbl 
            Caption         =   "Area of skin exposed while showering -- IC-ASKINDM"
            Height          =   380
            Index           =   32
            Left            =   120
            TabIndex        =   83
            Tag             =   "ICASKINDM"
            Top             =   1800
            Width           =   3900
         End
      End
      Begin Threed.SSFrame frame 
         Height          =   2112
         Index           =   5
         Left            =   -74760
         TabIndex        =   140
         Top             =   1080
         Width           =   7224
         _Version        =   65536
         _ExtentX        =   12742
         _ExtentY        =   3725
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
            Index           =   44
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   128
            Tag             =   "hr"
            Top             =   360
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   47
            Left            =   4080
            TabIndex        =   138
            Tag             =   "freq"
            Text            =   "1.0"
            Top             =   1560
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   44
            Left            =   4080
            TabIndex        =   127
            Tag             =   "teboat"
            Text            =   "0.5"
            Top             =   360
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   45
            Left            =   4080
            TabIndex        =   131
            Tag             =   "evboat"
            Text            =   "0.066"
            Top             =   720
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   46
            Left            =   4080
            TabIndex        =   135
            Tag             =   "sbrf"
            Text            =   "0.5"
            Top             =   1080
            Width           =   1000
         End
         Begin VB.Label lbl 
            Caption         =   "ev/day"
            Height          =   252
            Index           =   67
            Left            =   5160
            TabIndex        =   132
            Top             =   720
            Width           =   876
         End
         Begin VB.Label lbl 
            Caption         =   "Annual frequency factor for boating external exposure -- IC-FREQBOAT"
            Height          =   380
            Index           =   47
            Left            =   120
            TabIndex        =   137
            Tag             =   "ICFREQBOAT"
            Top             =   1560
            Width           =   3900
            WordWrap        =   -1  'True
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   47
            Left            =   6120
            TabIndex        =   139
            Tag             =   "0"
            Top             =   1560
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Boating geometry factor for external exposure -- IC-SBRF"
            Height          =   380
            Index           =   46
            Left            =   120
            TabIndex        =   134
            Tag             =   "ICSBRF"
            Top             =   1080
            Width           =   3900
         End
         Begin VB.Label lbl 
            Caption         =   "Frequency of boating event -- IC-EVBOAT"
            Height          =   252
            Index           =   45
            Left            =   120
            TabIndex        =   130
            Tag             =   "ICEVBOAT"
            Top             =   720
            Width           =   3900
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   44
            Left            =   6120
            TabIndex        =   129
            Tag             =   "0"
            Top             =   360
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   45
            Left            =   6120
            TabIndex        =   133
            Tag             =   "0"
            Top             =   720
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Time spent in one boating event -- IC-TEBOAT"
            Height          =   252
            Index           =   44
            Left            =   120
            TabIndex        =   126
            Tag             =   "ICTEBOAT"
            Top             =   360
            Width           =   3900
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   46
            Left            =   6120
            TabIndex        =   136
            Tag             =   "0"
            Top             =   1080
            Width           =   996
         End
      End
      Begin Threed.SSFrame frame 
         Height          =   3972
         Index           =   6
         Left            =   -74760
         TabIndex        =   169
         Top             =   1080
         Width           =   7224
         _Version        =   65536
         _ExtentX        =   12742
         _ExtentY        =   7006
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
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   1
            Left            =   4080
            TabIndex        =   223
            Tag             =   "adhsed"
            Text            =   "1"
            Top             =   2160
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   1
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   222
            Tag             =   "mg/cm^2"
            Top             =   2160
            Width           =   996
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   55
            Left            =   4080
            TabIndex        =   167
            Tag             =   "freq"
            Text            =   "1.0"
            Top             =   3240
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   54
            Left            =   4080
            TabIndex        =   164
            Tag             =   "freq"
            Text            =   "1.0"
            Top             =   2880
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   53
            Left            =   4080
            TabIndex        =   161
            Tag             =   "freq"
            Text            =   "1.0"
            Top             =   2520
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   52
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   158
            Tag             =   "cm^2"
            Top             =   1800
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   52
            Left            =   4080
            TabIndex        =   157
            Tag             =   "askin"
            Text            =   "10000.0"
            Top             =   1800
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   51
            Left            =   4080
            TabIndex        =   154
            Tag             =   "swf"
            Text            =   "0.2"
            Top             =   1440
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   50
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   151
            Tag             =   "g/hr"
            Top             =   1080
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   48
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   143
            Tag             =   "hr"
            Top             =   360
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   49
            Left            =   4080
            TabIndex        =   146
            Tag             =   "evshor"
            Text            =   "0.017"
            Top             =   720
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   48
            Left            =   4080
            TabIndex        =   142
            Tag             =   "teshor"
            Text            =   "2.0"
            Top             =   360
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   50
            Left            =   4080
            TabIndex        =   150
            Tag             =   "used"
            Text            =   "0.100"
            Top             =   1080
            Width           =   1000
         End
         Begin VB.Label lbl 
            Caption         =   "Sediment adherence factor for sediment contact events -- IC-ADHSED"
            Height          =   372
            Index           =   1
            Left            =   120
            TabIndex        =   225
            Tag             =   "ICADHSED"
            Top             =   2160
            Width           =   3900
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   1
            Left            =   6120
            TabIndex        =   224
            Tag             =   "0"
            Top             =   2160
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "ev/day"
            Height          =   252
            Index           =   68
            Left            =   5160
            TabIndex        =   147
            Top             =   720
            Width           =   876
         End
         Begin VB.Label lbl 
            Caption         =   "Annual frequency factor for shoreline external exposure -- IC-FREQSHEXT"
            Height          =   492
            Index           =   55
            Left            =   120
            TabIndex        =   166
            Tag             =   "ICFREQSHEXT"
            Top             =   3240
            Width           =   3900
            WordWrap        =   -1  'True
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   55
            Left            =   6120
            TabIndex        =   168
            Tag             =   "0"
            Top             =   3240
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Annual frequency factor for shoreline sediment ingestion -- IC-FREQSHING"
            Height          =   492
            Index           =   54
            Left            =   120
            TabIndex        =   163
            Tag             =   "ICFREQSHING"
            Top             =   2880
            Width           =   3900
            WordWrap        =   -1  'True
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   54
            Left            =   6120
            TabIndex        =   165
            Tag             =   "0"
            Top             =   2880
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Annual frequency factor for shoreline dermal contact -- IC-FREQSHDER"
            Height          =   492
            Index           =   53
            Left            =   120
            TabIndex        =   160
            Tag             =   "ICFREQSHDER"
            Top             =   2520
            Width           =   3900
            WordWrap        =   -1  'True
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   53
            Left            =   6120
            TabIndex        =   162
            Tag             =   "0"
            Top             =   2520
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   52
            Left            =   6120
            TabIndex        =   159
            Tag             =   "0"
            Top             =   1800
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   51
            Left            =   6120
            TabIndex        =   155
            Tag             =   "0"
            Top             =   1440
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   50
            Left            =   6120
            TabIndex        =   152
            Tag             =   "0"
            Top             =   1080
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   49
            Left            =   6120
            TabIndex        =   148
            Tag             =   "0"
            Top             =   720
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   48
            Left            =   6120
            TabIndex        =   144
            Tag             =   "0"
            Top             =   360
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Shore width factor for shoreline external exposure -- IC-SWF"
            Height          =   380
            Index           =   51
            Left            =   120
            TabIndex        =   153
            Tag             =   "ICSWF"
            Top             =   1440
            Width           =   3900
         End
         Begin VB.Label lbl 
            Caption         =   "Area of skin exposed during shoreline sediment contact -- IC-ASKINSL"
            Height          =   492
            Index           =   52
            Left            =   120
            TabIndex        =   156
            Tag             =   "ICASKINSL"
            Top             =   1800
            Width           =   3900
         End
         Begin VB.Label lbl 
            Caption         =   "Duration of a shoreline exposure event -- IC-TESHOR"
            Height          =   380
            Index           =   48
            Left            =   120
            TabIndex        =   141
            Tag             =   "ICTESHOR"
            Top             =   360
            Width           =   3900
         End
         Begin VB.Label lbl 
            Caption         =   "Frequency of a shoreline exposure event -- IC-EVSHOR"
            Height          =   380
            Index           =   49
            Left            =   120
            TabIndex        =   145
            Tag             =   "ICEVSHOR"
            Top             =   720
            Width           =   3900
         End
         Begin VB.Label lbl 
            Caption         =   "Shoreline sediment ingestion rate -- IC-USED"
            Height          =   380
            Index           =   50
            Left            =   120
            TabIndex        =   149
            Tag             =   "ICUSED"
            Top             =   1080
            Width           =   3900
         End
      End
      Begin Threed.SSFrame frame 
         Height          =   3732
         Index           =   7
         Left            =   -74760
         TabIndex        =   204
         Top             =   1080
         Width           =   7224
         _Version        =   65536
         _ExtentX        =   12742
         _ExtentY        =   6583
         _StockProps     =   14
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Font3D          =   2
         ShadowStyle     =   1
         Enabled         =   0   'False
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   64
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   202
            Tag             =   "g/cm^3"
            Top             =   3240
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   63
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   198
            Tag             =   "m"
            Top             =   2880
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   61
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   191
            Tag             =   "g/day"
            Top             =   2160
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   58
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   180
            Tag             =   "mg/cm^2"
            Top             =   1080
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   57
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   176
            Tag             =   "cm^2"
            Top             =   720
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   56
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   172
            Tag             =   "cm"
            Top             =   360
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   64
            Left            =   4080
            TabIndex        =   201
            Tag             =   "rhoms"
            Text            =   "1.5"
            Top             =   3240
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   63
            Left            =   4080
            TabIndex        =   197
            Tag             =   "tms"
            Text            =   "0.04"
            Top             =   2880
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   61
            Left            =   4080
            TabIndex        =   190
            Tag             =   "usoil"
            Text            =   "0.100"
            Top             =   2160
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   62
            Left            =   4080
            TabIndex        =   194
            Tag             =   "freq"
            Text            =   "1.0"
            Top             =   2520
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   60
            Left            =   4080
            TabIndex        =   187
            Tag             =   "freq"
            Text            =   "1.0"
            Top             =   1800
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   59
            Left            =   4080
            TabIndex        =   183
            Tag             =   "evsoil"
            Text            =   "1.0"
            Top             =   1440
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   58
            Left            =   4080
            TabIndex        =   179
            Tag             =   "adhfac"
            Text            =   "1.0"
            Top             =   1080
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   57
            Left            =   4080
            TabIndex        =   175
            Tag             =   "askin"
            Text            =   "5800.0"
            Top             =   720
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   56
            Left            =   4080
            TabIndex        =   171
            Tag             =   "skinl"
            Text            =   "0.001"
            Top             =   360
            Width           =   1000
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   64
            Left            =   6120
            TabIndex        =   203
            Tag             =   "0"
            Top             =   3240
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   63
            Left            =   6120
            TabIndex        =   199
            Tag             =   "0"
            Top             =   2880
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Thickness of measured soil contamination layer -- IC-TMS"
            Height          =   380
            Index           =   63
            Left            =   120
            TabIndex        =   196
            Tag             =   "ICTMS"
            Top             =   2880
            Width           =   3900
         End
         Begin VB.Label lbl 
            Caption         =   "Density of measured soil contamination layer -- IC-RHOMS"
            Height          =   380
            Index           =   64
            Left            =   120
            TabIndex        =   200
            Tag             =   "ICRHOMS"
            Top             =   3240
            Width           =   3900
            WordWrap        =   -1  'True
         End
         Begin VB.Label lbl 
            Caption         =   "Soil ingestion rate -- IC-USOIL"
            Height          =   380
            Index           =   61
            Left            =   120
            TabIndex        =   189
            Tag             =   "ICUSOIL"
            Top             =   2160
            Width           =   3900
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   61
            Left            =   6120
            TabIndex        =   192
            Tag             =   "0"
            Top             =   2160
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   62
            Left            =   6120
            TabIndex        =   195
            Tag             =   "0"
            Top             =   2520
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Annual frequency factor for soil ingestion -- IC-FREQSLING"
            Height          =   380
            Index           =   62
            Left            =   120
            TabIndex        =   193
            Tag             =   "ICFREQSLING"
            Top             =   2520
            Width           =   3900
            WordWrap        =   -1  'True
         End
         Begin VB.Label lbl 
            Caption         =   "ev/day"
            Height          =   252
            Index           =   69
            Left            =   5160
            TabIndex        =   184
            Top             =   1440
            Width           =   876
         End
         Begin VB.Label lbl 
            Caption         =   "Annual frequency factor for soil dermal contact -- IC-FREQSLDER"
            Height          =   380
            Index           =   60
            Left            =   120
            TabIndex        =   186
            Tag             =   "ICFREQSLDER"
            Top             =   1800
            Width           =   3900
            WordWrap        =   -1  'True
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   60
            Left            =   6120
            TabIndex        =   188
            Tag             =   "0"
            Top             =   1800
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   59
            Left            =   6120
            TabIndex        =   185
            Tag             =   "0"
            Top             =   1440
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   58
            Left            =   6120
            TabIndex        =   181
            Tag             =   "0"
            Top             =   1080
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   57
            Left            =   6120
            TabIndex        =   177
            Tag             =   "0"
            Top             =   720
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   56
            Left            =   6120
            TabIndex        =   173
            Tag             =   "0"
            Top             =   360
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Thickness of skin layer, for dermal models -- IC-SKINL"
            Height          =   380
            Index           =   56
            Left            =   120
            TabIndex        =   170
            Tag             =   "ICSKINL"
            Top             =   360
            Width           =   3900
         End
         Begin VB.Label lbl 
            Caption         =   "Area of skin exposed during soil contact events -- IC-ASKINSO"
            Height          =   380
            Index           =   57
            Left            =   120
            TabIndex        =   174
            Tag             =   "ICASKINSO"
            Top             =   720
            Width           =   3900
         End
         Begin VB.Label lbl 
            Caption         =   "Soil adherence factor for soil contact events -- IC-ADHFAC"
            Height          =   380
            Index           =   58
            Left            =   120
            TabIndex        =   178
            Tag             =   "ICADHFAC"
            Top             =   1080
            Width           =   3900
         End
         Begin VB.Label lbl 
            Caption         =   "Frequency of soil dermal contact events -- IC-EVSOIL"
            Height          =   380
            Index           =   59
            Left            =   120
            TabIndex        =   182
            Tag             =   "ICEVSOIL"
            Top             =   1440
            Width           =   3900
         End
      End
      Begin Threed.SSFrame frame 
         Height          =   3732
         Index           =   4
         Left            =   -74760
         TabIndex        =   125
         Top             =   1080
         Width           =   7224
         _Version        =   65536
         _ExtentX        =   12742
         _ExtentY        =   6583
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
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   43
            Left            =   4080
            TabIndex        =   123
            Tag             =   "freq"
            Text            =   "1.0"
            Top             =   2880
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   42
            Left            =   4080
            TabIndex        =   120
            Tag             =   "freq"
            Text            =   "1.0"
            Top             =   2400
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   41
            Left            =   4080
            TabIndex        =   117
            Tag             =   "freq"
            Text            =   "1.0"
            Top             =   1920
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   40
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   114
            Tag             =   "cm^2"
            Top             =   1440
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   40
            Left            =   4080
            TabIndex        =   113
            Tag             =   "askin"
            Text            =   "20000.0"
            Top             =   1440
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   39
            Left            =   4080
            TabIndex        =   109
            Tag             =   "evswim"
            Text            =   "0.066"
            Top             =   1080
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   38
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   106
            Tag             =   "hr"
            Top             =   720
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   38
            Left            =   4080
            TabIndex        =   105
            Tag             =   "teswim"
            Text            =   "0.5"
            Top             =   720
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   37
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   102
            Tag             =   "L/hr"
            Top             =   360
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   37
            Left            =   4080
            TabIndex        =   101
            Tag             =   "usw"
            Text            =   "0.10"
            Top             =   360
            Width           =   1000
         End
         Begin VB.Label lbl 
            Caption         =   "ev/day"
            Height          =   252
            Index           =   66
            Left            =   5160
            TabIndex        =   110
            Top             =   1080
            Width           =   876
         End
         Begin VB.Label lbl 
            Caption         =   "Annual frequency factor for swimming external exposure -- IC-FREQSWEXT"
            Height          =   492
            Index           =   43
            Left            =   120
            TabIndex        =   122
            Tag             =   "ICFREQSWEXT"
            Top             =   2880
            Width           =   3900
            WordWrap        =   -1  'True
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   43
            Left            =   6120
            TabIndex        =   124
            Tag             =   "0"
            Top             =   2880
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Annual frequency factor for swimming dermal contact -- IC-FREQSWDER"
            Height          =   492
            Index           =   42
            Left            =   120
            TabIndex        =   119
            Tag             =   "ICFREQSWDER"
            Top             =   2400
            Width           =   3900
            WordWrap        =   -1  'True
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   42
            Left            =   6120
            TabIndex        =   121
            Tag             =   "0"
            Top             =   2400
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Annual frequency factor for swimming ingestion -- IC-FREQSWING"
            Height          =   492
            Index           =   41
            Left            =   120
            TabIndex        =   116
            Tag             =   "ICFREQSWING"
            Top             =   1920
            Width           =   3900
            WordWrap        =   -1  'True
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   41
            Left            =   6120
            TabIndex        =   118
            Tag             =   "0"
            Top             =   1920
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Area of skin exposed while swimming -- IC-ASKINSW"
            Height          =   252
            Index           =   40
            Left            =   120
            TabIndex        =   112
            Tag             =   "ICASKINSW"
            Top             =   1440
            Width           =   3900
         End
         Begin VB.Label lbl 
            Caption         =   "Frequency of swimming -- IC-EVSWIM"
            Height          =   252
            Index           =   39
            Left            =   120
            TabIndex        =   108
            Tag             =   "ICEVSWIM"
            Top             =   1080
            Width           =   3900
         End
         Begin VB.Label lbl 
            Caption         =   "Duration of a swimming event -- IC-TESWIM"
            Height          =   252
            Index           =   38
            Left            =   120
            TabIndex        =   104
            Tag             =   "ICTESWIM"
            Top             =   720
            Width           =   3900
         End
         Begin VB.Label lbl 
            Caption         =   "Water ingestion rate while swimming -- IC-USW"
            Height          =   252
            Index           =   37
            Left            =   120
            TabIndex        =   100
            Tag             =   "ICUSW"
            Top             =   360
            Width           =   3900
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   40
            Left            =   6120
            TabIndex        =   115
            Tag             =   "0"
            Top             =   1440
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   39
            Left            =   6120
            TabIndex        =   111
            Tag             =   "0"
            Top             =   1080
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   38
            Left            =   6120
            TabIndex        =   107
            Tag             =   "0"
            Top             =   720
            Width           =   996
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   37
            Left            =   6120
            TabIndex        =   103
            Tag             =   "0"
            Top             =   360
            Width           =   996
         End
      End
      Begin Threed.SSFrame frame 
         Height          =   2172
         Index           =   8
         Left            =   -74760
         TabIndex        =   219
         Top             =   1080
         Width           =   7224
         _Version        =   65536
         _ExtentX        =   12742
         _ExtentY        =   3831
         _StockProps     =   14
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Font3D          =   2
         ShadowStyle     =   1
         Enabled         =   0   'False
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   22
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   207
            Tag             =   "kg/day"
            Top             =   360
            Width           =   1000
         End
         Begin VB.ComboBox unit 
            BackColor       =   &H00FFFFFF&
            Height          =   288
            Index           =   23
            Left            =   5040
            Style           =   2  'Dropdown List
            TabIndex        =   211
            Tag             =   "kg/day"
            Top             =   720
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   22
            Left            =   4080
            TabIndex        =   206
            Tag             =   "uaq"
            Text            =   "0.054"
            Top             =   360
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   23
            Left            =   4080
            TabIndex        =   210
            Tag             =   "uaq"
            Text            =   "0.0027"
            Top             =   720
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   24
            Left            =   4080
            TabIndex        =   214
            Tag             =   "freq"
            Text            =   "1.0"
            Top             =   1080
            Width           =   1000
         End
         Begin VB.TextBox txt 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00C0FFC0&
            Height          =   312
            Index           =   25
            Left            =   4080
            TabIndex        =   217
            Tag             =   "freq"
            Text            =   "1.0"
            Top             =   1560
            Width           =   1000
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   22
            Left            =   6120
            TabIndex        =   208
            Tag             =   "0"
            Top             =   360
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Ingestion rate of finned fish -- IC-UAQFIN"
            Height          =   252
            Index           =   22
            Left            =   120
            TabIndex        =   205
            Tag             =   "ICUAQFIN"
            Top             =   360
            Width           =   3900
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   23
            Left            =   6120
            TabIndex        =   212
            Tag             =   "0"
            Top             =   720
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Ingestion rate of shellfish -- IC-UAQSHL"
            Height          =   252
            Index           =   23
            Left            =   120
            TabIndex        =   209
            Tag             =   "ICUAQSHL"
            Top             =   720
            Width           =   3900
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   24
            Left            =   6120
            TabIndex        =   215
            Tag             =   "0"
            Top             =   1080
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Annual frequency factor for finned fish ingestion -- IC-FREQFIN"
            Height          =   380
            Index           =   24
            Left            =   120
            TabIndex        =   213
            Tag             =   "ICFREQFIN"
            Top             =   1080
            Width           =   3900
            WordWrap        =   -1  'True
         End
         Begin VB.Label ref 
            Caption         =   "Ref: 0"
            Height          =   252
            Index           =   25
            Left            =   6120
            TabIndex        =   218
            Tag             =   "0"
            Top             =   1560
            Width           =   996
         End
         Begin VB.Label lbl 
            Caption         =   "Annual frequency factor for shellfish ingestion -- IC-FREQSHL"
            Height          =   384
            Index           =   25
            Left            =   120
            TabIndex        =   216
            Tag             =   "ICFREQSHL"
            Top             =   1560
            Width           =   3900
            WordWrap        =   -1  'True
         End
      End
   End
   Begin VB.Menu file 
      Caption         =   "&File"
      WindowList      =   -1  'True
      Begin VB.Menu setdef 
         Caption         =   "Reset &Defaults"
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
Attribute VB_Name = "Pathway"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text
Dim temp As parmrec
Dim tval As Double

Private Sub about_Click()
  frmAbout.Show 1
End Sub

Private Sub howto_Click()
  GetHelp
End Sub

Private Sub leave_Click()
  Unload Pathway
End Sub

Private Sub selref_Click()
  RefMode = 0
  GetRef ref(RefItem)
End Sub

Private Sub addref_Click()
  RefMode = 1
  GetRef ref(RefItem)
End Sub

Private Sub fillet(idx As Long)
  ref(idx).Tag = temp.ref
  ref(idx).Caption = "Ref:" & Str(temp.ref)
  On Error Resume Next
  If temp.cunit <> "N/A" Then set_unit unit(idx), temp.uunit
  txt(idx).Text = convert(temp.cunit, temp.uunit, Val(temp.pval))
End Sub

Private Sub loaddef()
  Dim i As Long
  Dim j As Long
  Dim m As Long
  Dim numref As Long
  Dim abrv As String
  Dim refer As String
  Dim name As String
  Dim post As String
  Dim pre As String
  Dim fle As csv

  If open_csv(fle, App.Path & "\meprcp.rf_", F_READ) Then
    For i = 0 To 8
      frame(i).Enabled = True
    Next
    'load references inot reference file
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
    For j = 1 To numref
      temp.pname = get_val(fle)
      temp.ref = refs(Val(get_val(fle)))
      temp.pval = get_val(fle)
      temp.cunit = get_val(fle)
      get_line fle
      m = -1
      name = Right(temp.pname, Len(temp.pname) - 3)
      For i = 0 To 64
        ' must skip missing txt box
        If i = 2 Then i = i + 1
        If i = 29 Or i = 12 Then i = i + 2
        If txt(i).Tag = name Then
          m = i
          Exit For
        End If
      Next
      If (m = -1) Then
        pre = Left(temp.pname, 2)
        post = Right(name, Len(name) - 3)
        name = Left(name, 3)
        Select Case name
          Case "fre"
            If post = "QING" Then m = 33
            If post = "QSRDER" Then m = 34
            If post = "QSRING" Then m = 35
            If post = "QLEAF" Then m = 18
            If post = "QVEG" Then m = 19
            If post = "QMT" Then m = 20
            If post = "QMK" Then m = 21
            If post = "QFIN" Then m = 24
            If post = "QSHL" Then m = 25
            If post = "QSWING" Then m = 41
            If post = "QSWDER" Then m = 42
            If post = "QSHDER" Then m = 53
            If post = "QSHING" Then m = 54
            If post = "QSLING" Then m = 62
            If post = "QSLDER" Then m = 60
            If post = "QSRINH" Then m = 36
            If post = "QINH" Then m = 4
            If post = "QSL" Then m = 5
            If post = "QSWEXT" Then m = 43
            If post = "QBOAT" Then m = 47
            If post = "QSHEXT" Then m = 55
            If post = "QSLEXT" Then m = 13
            If post = "QEXT" Then m = 6
          Case "uag"
            If post = "leaf" Then m = 14
            If post = "veg" Then m = 15
            If post = "mt" Then m = 16
            If post = "mk" Then m = 17
          Case "ask"
            If post = "indm" Then m = 32
            If post = "insw" Then m = 40
            If post = "insl" Then m = 52
            If post = "inso" Then m = 57
          Case "uaq"
            If post = "fin" Then m = 22
            If post = "shl" Then m = 23
        End Select
      End If
      If (m > -1) Then
        ref(m).Tag = temp.ref
        ref(m).Caption = "Ref:" & Str(temp.ref)
        txt(m).Text = temp.pval
        If temp.cunit <> "" And temp.cunit <> "fraction" And temp.cunit <> "event/day" Then set_unit unit(m), temp.cunit
        er m, txt(m).Enabled
      End If
    Next
    close_csv fle
    For i = 0 To 8
      frame(i).Enabled = False
    Next
    SSTab1_Click SSTab1.Tab
  Else
    MsgBox "Can't find or open file default reference file", vbExclamation
  End If
  
ErrorHandler:
  If Err.Number <> 0 Then
    MsgBox Err.Description, vbOKOnly, "loadref"
  End If
End Sub


Private Sub loadprm()
  Dim m As Long
  Dim i As Long
  Dim fle As parmfile
  Dim fname As String
    
  If UsedCustomized Then
    fname = RunName & ".~rp"
  Else
    fname = FUIName
  End If
  If open_parm(fle, fname, 2) Then
     Do Until EOCF(fle.file)
       If read_parmrec(fle, temp) Then
        Select Case temp.pname
          Case modName
            For m = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                Select Case temp.pname
                  Case "freq"
                    If temp.idx1 = 1 Then fillet 33
                    If temp.idx1 = 2 Then fillet 34
                    If temp.idx1 = 3 Then fillet 35
                    If temp.idx1 = 4 Then fillet 18
                    If temp.idx1 = 5 Then fillet 19
                    If temp.idx1 = 6 Then fillet 20
                    If temp.idx1 = 7 Then fillet 21
                    If temp.idx1 = 8 Then fillet 24
                    If temp.idx1 = 9 Then fillet 25
                    If temp.idx1 = 10 Then fillet 41
                    If temp.idx1 = 11 Then fillet 42
                    If temp.idx1 = 12 Then fillet 53
                    If temp.idx1 = 13 Then fillet 54
                    If temp.idx1 = 14 Then fillet 62
                    If temp.idx1 = 15 Then fillet 60
                    If temp.idx1 = 17 Then fillet 36
                    If temp.idx1 = 18 Then fillet 4
                    If temp.idx1 = 19 Then fillet 5
                    If temp.idx1 = 20 Then fillet 43
                    If temp.idx1 = 21 Then fillet 47
                    If temp.idx1 = 22 Then fillet 55
                    If temp.idx1 = 23 Then fillet 13
                    If temp.idx1 = 24 Then fillet 6
                  Case "uag"
                    If temp.idx1 = 1 Then fillet 14
                    If temp.idx1 = 2 Then fillet 15
                    If temp.idx1 = 3 Then fillet 16
                    If temp.idx1 = 4 Then fillet 17
                  Case "askin"
                    If temp.idx1 = 1 Then fillet 32
                    If temp.idx1 = 2 Then fillet 40
                    If temp.idx1 = 3 Then fillet 57
                    If temp.idx1 = 4 Then fillet 52
                  Case "uaq"
                    If temp.idx1 = 1 Then fillet 22
                    If temp.idx1 = 2 Then fillet 23
                  Case Else
                    For i = 0 To 64
                      ' must skip missing txt box
                      If i = 2 Then i = i + 1
                      If i = 29 Or i = 12 Then i = i + 2
                      If txt(i).Tag = temp.pname Then
                        fillet i
                        Exit For
                      End If
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
  Else
    PutError "Can't find or open file " & fname
    Unload Pathway
  End If
End Sub

Sub Form_load()
  Dim cnt As Long
  Dim i As Long
  Dim fle As parmfile
  
'set conversion comboboxes
  For i = 0 To 64
    Select Case i
    Case 2, 4 To 10, 12, 13, 18 To 21, 24, 25, 27, 29, 30, 33 To 36, 39
    Case 41 To 43, 45, 46, 47, 49, 51, 53 To 55, 59, 60, 62
    Case Else
      cnt = get_conversion_items(unit(i).Tag, unit(i))
    End Select
  Next
  Pathway.Caption = "MEPAS Receptor Intake Module Customization" + " - " + modName
  If open_parm(fle, FUIName, 2) Then
    If FindSection(fle, modName) = 0 Then
      loaddef
    End If
    close_parm fle
  End If
  loadprm
  SSTab1_Click 0
End Sub

Sub save_Click()
  Dim fname As String
  Dim parm As parmrec
  Dim fle As parmfile
  Dim i As Long
  Dim j As Long
  Dim UserFormat As String
  UserFormat = CVTFormat
  
  fname = RunName & ".~rp"
  If open_parm(fle, fname, 1) Then
    CVTFormat = "General Number"
    set_parm parm, modName, 61, 0, 0, 0, 0, 0, 0, "N/A", "N/A", "N/A"
    write_parmrec fle, parm
    For i = 0 To 64
     'must skip missing txt box
      If i = 29 Then i = i + 2
      If i = 2 Or i = 12 Then i = i + 1
      If er(i) Then PutError "Parameter " & lbl(i).Tag & " is invalid"
      Select Case i
      Case 7, 8, 9, 10, 27, 39, 45, 46, 49, 51, 59
        set_parm parm, UCase(txt(i).Tag), 0, 0, 0, 0, 0, 0, ref(i).Tag, "N/A", "N/A", txt(i).Text
        write_parmrec fle, parm
      Case 0, 1, 3, 11, 26, 28, 31, 37, 38, 44, 48, 50, 58, 56, 61, 63, 64
        set_parm parm, UCase(txt(i).Tag), 0, 0, 0, 0, 0, 0, ref(i).Tag, unit(i).Text, unit(i).Tag, convert(unit(i).Text, unit(i).Tag, txt(i).Text)
        write_parmrec fle, parm
      Case 14, 22, 32
        set_parm parm, UCase(txt(i).Tag), 1, 0, 0, 0, 0, 0, ref(i).Tag, unit(i).Text, unit(i).Tag, convert(unit(i).Text, unit(i).Tag, txt(i).Text)
        write_parmrec fle, parm
      Case 15, 23, 40
        set_parm parm, UCase(txt(i).Tag), 2, 0, 0, 0, 0, 0, ref(i).Tag, unit(i).Text, unit(i).Tag, convert(unit(i).Text, unit(i).Tag, txt(i).Text)
        write_parmrec fle, parm
      Case 16, 57
        set_parm parm, UCase(txt(i).Tag), 3, 0, 0, 0, 0, 0, ref(i).Tag, unit(i).Text, unit(i).Tag, convert(unit(i).Text, unit(i).Tag, txt(i).Text)
        write_parmrec fle, parm
      Case 17, 52
        set_parm parm, UCase(txt(i).Tag), 4, 0, 0, 0, 0, 0, ref(i).Tag, unit(i).Text, unit(i).Tag, convert(unit(i).Text, unit(i).Tag, txt(i).Text)
        write_parmrec fle, parm
      Case 4 To 6, 13, 18 To 21, 24, 25, 33 To 36, 41 To 43, 47, 53 To 55, 60, 62
        If i = 4 Then j = 18
        If i = 5 Then j = 19
        If i = 6 Then j = 24
        If i = 13 Then j = 23
        If i = 18 Then j = 4
        If i = 19 Then j = 5
        If i = 20 Then j = 6
        If i = 21 Then j = 7
        If i = 24 Then j = 8
        If i = 25 Then j = 9
        If i = 33 Then j = 1
        If i = 34 Then j = 2
        If i = 35 Then j = 3
        If i = 36 Then j = 17
        If i = 41 Then j = 10
        If i = 42 Then j = 11
        If i = 43 Then j = 20
        If i = 47 Then j = 21
        If i = 53 Then j = 12
        If i = 54 Then j = 13
        If i = 55 Then j = 22
        If i = 60 Then j = 15
        If i = 62 Then j = 14
        set_parm parm, UCase(txt(i).Tag), j, 0, 0, 0, 0, 0, ref(i).Tag, "N/A", "N/A", txt(i).Text
        write_parmrec fle, parm
      End Select
    Next
    set_parm parm, UCase(txt(36).Tag), 25, 0, 0, 0, 0, 0, ref(36).Tag, "N/A", "N/A", txt(36).Text
    write_parmrec fle, parm
    close_parm fle
    CVTFormat = UserFormat
    UsedCustomized = True
  Else
    PutError "Can't find or open file " & fname
  End If
  Unload Pathway
End Sub
'
'Sub save_metadata()
'
'    putmeta UCase(txt(7).Tag), "CONTINUOUS", "N/A", lbl(7).Caption, 0, 0, 1
'    putmeta UCase(txt(8).Tag), "CONTINUOUS", "N/A", lbl(8).Caption, 0, 0, 1
'    putmeta UCase(txt(9).Tag), "CONTINUOUS", "N/A", lbl(9).Caption, 0, 0, 1
'    putmeta UCase(txt(10).Tag), "CONTINUOUS", "N/A", lbl(10).Caption, 0, 0, 1
'    putmeta UCase(txt(27).Tag), "CONTINUOUS", "N/A", lbl(27).Caption, 0, 0, 5
'    putmeta UCase(txt(39).Tag), "CONTINUOUS", "N/A", lbl(39).Caption, 0, 0, 5
'    putmeta UCase(txt(45).Tag), "CONTINUOUS", "N/A", lbl(45).Caption, 0, 0, 5
'    putmeta UCase(txt(46).Tag), "CONTINUOUS", "N/A", lbl(46).Caption, 0, 0, 1
'    putmeta UCase(txt(49).Tag), "CONTINUOUS", "N/A", lbl(49).Caption, 0, 0, 10
'    putmeta UCase(txt(51).Tag), "CONTINUOUS", "N/A", lbl(51).Caption, 0, 0, 1
'    putmeta UCase(txt(59).Tag), "CONTINUOUS", "N/A", lbl(59).Caption, 0, 0, 10
'
'    putmeta UCase(txt(0).Tag), "CONTINUOUS", unit(0).Tag, lbl(0).Caption, 0, 0, 50
'    putmeta UCase(txt(3).Tag), "CONTINUOUS", unit(3).Tag, lbl(3).Caption, 0, 0, 50
'    putmeta UCase(txt(11).Tag), "CONTINUOUS", unit(11).Tag, lbl(11).Caption, 0, 0, 24
'    putmeta UCase(txt(26).Tag), "CONTINUOUS", unit(26).Tag, lbl(26).Caption, 0, 0.000001, 1
'    putmeta UCase(txt(28).Tag), "CONTINUOUS", unit(28).Tag, lbl(28).Caption, 0, 0, 50
'    putmeta UCase(txt(31).Tag), "CONTINUOUS", unit(31).Tag, lbl(31).Caption, 0, 0, 1
'    putmeta UCase(txt(37).Tag), "CONTINUOUS", unit(37).Tag, lbl(37).Caption, 0, 0, 1
'    putmeta UCase(txt(38).Tag), "CONTINUOUS", unit(38).Tag, lbl(38).Caption, 0, 0, 24
'    putmeta UCase(txt(44).Tag), "CONTINUOUS", unit(44).Tag, lbl(44).Caption, 0, 0, 24
'    putmeta UCase(txt(48).Tag), "CONTINUOUS", unit(48).Tag, lbl(48).Caption, 0, 0, 24
'    putmeta UCase(txt(50).Tag), "CONTINUOUS", unit(50).Tag, lbl(50).Caption, 0, 0, 1
'    putmeta UCase(txt(56).Tag), "CONTINUOUS", unit(56).Tag, lbl(56).Caption, 0, 0.0001, 0.01
'    putmeta UCase(txt(58).Tag), "CONTINUOUS", unit(58).Tag, lbl(58).Caption, 0, 0, 10
'    putmeta UCase(txt(61).Tag), "CONTINUOUS", unit(61).Tag, lbl(61).Caption, 0, 0, 10
'    putmeta UCase(txt(63).Tag), "CONTINUOUS", unit(63).Tag, lbl(63).Caption, 0, 0, 5
'    putmeta UCase(txt(64).Tag), "CONTINUOUS", unit(64).Tag, lbl(64).Caption, 0, 1, 4
'
'    putmeta UCase(txt(14).Tag), "CONTINUOUS", unit(14).Tag, "Terrestrial food, ingestion rate", 1, 0, 5
'    putlabel "Food #", "Index1"
'    putmeta UCase(txt(22).Tag), "CONTINUOUS", unit(22).Tag, "Aquatic food, ingestion rate", 1, 0, 5
'    putlabel "Fish #", "Index1"
'    putmeta UCase(txt(32).Tag), "CONTINUOUS", unit(32).Tag, "Area skin exposued", 1, 0, 40000
'    putlabel "Activity #", "Index1"
'
'End Sub

Private Function er(Index As Long, Optional enable As Boolean = True) As Boolean
  Dim tval As Double
  Dim t1 As String
  Dim t2 As String
  Dim m As String
  
  m = ""
  er = False
  tval = Val(txt(Index).Text)
  If txt(Index).Text = "" Then er = True
  Select Case Index
    Case 4 To 10, 12, 13, 18 To 21, 24, 25, 33 To 36, 41 To 43, 46, 47, 51, 53 To 55, 60, 62
      m = "Value must be between 0 and 1"
      If (tval < 0 Or tval > 1) Then er = True
    Case 27, 39, 45
      m = "Value must be between 0 and 5"
      If (tval < 0 Or tval > 5) Then er = True
    Case 49, 59
      m = "Value must be between 0 and 10"
      If (tval < 0 Or tval > 10) Then er = True
    Case 31, 37, 50
      t1 = convert(unit(Index).Tag, unit(Index).Text, 1)
      m = "Value must be between 0 and " + t1 + " " + unit(Index).Text
      If (tval < 0 Or tval > Val(t1)) Then er = True
    Case 26
      t1 = convert(unit(Index).Tag, unit(Index).Text, 1)
      m = "Value must be greater than 0 and less than " + t1 + " " + unit(Index).Text
      If (tval <= 0 Or tval > Val(t1)) Then er = True
    Case 17
      t1 = convert(unit(Index).Tag, unit(Index).Text, 4)
      m = "Value must be between 0 and " + t1 + " " + unit(Index).Text
      If (tval < 0 Or tval > Val(t1)) Then er = True
    Case 14 To 16, 22, 23, 29, 30, 63
      t1 = convert(unit(Index).Tag, unit(Index).Text, 5)
      m = "Value must be between 0 and " + t1 + " " + unit(Index).Text
      If (tval < 0 Or tval > Val(t1)) Then er = True
    Case 1, 58, 61
      t1 = convert(unit(Index).Tag, unit(Index).Text, 10)
      m = "Value must be between 0 and " + t1 + " " + unit(Index).Text
      If (tval < 0 Or tval > Val(t1)) Then er = True
    Case 11, 38, 44
      t1 = convert(unit(Index).Tag, unit(Index).Text, 24)
      m = "Value must be between 0 and " + t1 + " " + unit(Index).Text
      If (tval < 0 Or tval > Val(t1)) Then er = True
    Case 38, 48
      t1 = convert(unit(Index).Tag, unit(Index).Text, 24)
      m = "Value must be greater than 0 and less than " + t1 + " " + unit(Index).Text
      If (tval <= 0 Or tval > Val(t1)) Then er = True
    Case 0, 3, 28
      t1 = convert(unit(Index).Tag, unit(Index).Text, 50)
      m = "Value must be between 0 and " + t1 + " " + unit(Index).Text
      If (tval < 0 Or tval > Val(t1)) Then er = True
    Case 32, 40, 52, 57
      t1 = convert(unit(Index).Tag, unit(Index).Text, 40000)
      m = "Value must be between 0 and " + t1 + " " + unit(Index).Text
      If (tval < 0 Or tval > Val(t1)) Then er = True
    Case 64
      t1 = convert(unit(Index).Tag, unit(Index).Text, 1)
      t2 = convert(unit(Index).Tag, unit(Index).Text, 4)
      m = "Value must be between " + t1 + " and " + t2 + " " + unit(Index).Text
      If (tval < Val(t1) Or tval > Val(t2)) Then er = True
    Case 56
      t1 = convert(unit(Index).Tag, unit(Index).Text, 0.0001)
      t2 = convert(unit(Index).Tag, unit(Index).Text, 0.01)
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

Private Sub setdef_Click()
  Me.Hide
  loaddef
  Me.Show
End Sub

Private Sub Timer1_Timer()
  mes = Mid(mes, 2) & Mid(mes, 1, 1)
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

Private Sub txt_GotFocus(Index As Integer)
  er CLng(Index)
  RefItem = Index
  noact.Enabled = True
  HelpAnchor = lbl(Index).Tag
End Sub

Private Sub unit_GotFocus(Index As Integer)
  er CLng(Index)
  RefItem = Index
  noact.Enabled = True
  HelpAnchor = lbl(Index).Tag
End Sub

Private Sub unit_Click(Index As Integer)
  er CLng(Index)
End Sub

Private Sub SSTab1_GotFocus()
  mes = ""
  noact.Enabled = False
End Sub

Private Sub SSTab1_Click(PreviousTab As Integer)
  frame(PreviousTab).Enabled = False
  frame(SSTab1.Tab).Enabled = True
  Select Case SSTab1.Tab
    Case 0: HelpAnchor = "AIR_INHALATION"
    Case 1: HelpAnchor = "EXTERNAL_GROUND"
    Case 2: HelpAnchor = "TERRESTRIAL_FOOD"
    Case 3: HelpAnchor = "DOMESTIC_USE"
    Case 4: HelpAnchor = "SWIMMING"
    Case 5: HelpAnchor = "BOATING"
    Case 6: HelpAnchor = "SHORELINE"
    Case 7: HelpAnchor = "SOIL"
    Case 8: HelpAnchor = "AQUATIC_FOOD"
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

