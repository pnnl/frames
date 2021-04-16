VERSION 5.00
Object = "{6B7E6392-850A-101B-AFC0-4210102A8DA7}#1.3#0"; "COMCTL32.OCX"
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Object = "{C932BA88-4374-101B-A56C-00AA003668DC}#1.1#0"; "MSMASK32.OCX"
Begin VB.Form Form1 
   BackColor       =   &H80000004&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Description File Editor"
   ClientHeight    =   6930
   ClientLeft      =   1410
   ClientTop       =   1170
   ClientWidth     =   9600
   Icon            =   "EditDes.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   482.087
   ScaleMode       =   0  'User
   ScaleWidth      =   640
   StartUpPosition =   2  'CenterScreen
   Begin ComctlLib.TreeView TreeView1 
      Height          =   6672
      Left            =   240
      TabIndex        =   102
      Top             =   120
      Width           =   4056
      _ExtentX        =   7144
      _ExtentY        =   11774
      _Version        =   327682
      HideSelection   =   0   'False
      Indentation     =   529
      LabelEdit       =   1
      Style           =   7
      ImageList       =   "ImageList2"
      Appearance      =   1
   End
   Begin MSComDlg.CommonDialog PrintDialog 
      Left            =   480
      Top             =   5880
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   0
      Top             =   0
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
      Filter          =   """Description Files (*.des)|*.des*"
   End
   Begin VB.Frame VarPanel 
      Caption         =   "Variables"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   6780
      Left            =   4440
      TabIndex        =   9
      Top             =   0
      Visible         =   0   'False
      Width           =   4890
      Begin VB.CommandButton DltVar 
         Caption         =   "Delete Variable"
         Height          =   500
         Left            =   2520
         TabIndex        =   87
         Top             =   6120
         Width           =   1000
      End
      Begin VB.CommandButton NewCue 
         Caption         =   "New Cue"
         Height          =   500
         Left            =   3600
         TabIndex        =   88
         Top             =   6120
         Width           =   1000
      End
      Begin VB.TextBox CueText 
         Height          =   525
         Index           =   1
         Left            =   360
         Locked          =   -1  'True
         MultiLine       =   -1  'True
         TabIndex        =   55
         TabStop         =   0   'False
         Top             =   4200
         Width           =   4095
      End
      Begin VB.CommandButton GenVar 
         Caption         =   "Generate Variables"
         Height          =   500
         Index           =   1
         Left            =   360
         TabIndex        =   85
         Top             =   6120
         Width           =   1000
      End
      Begin VB.ComboBox UnitCombo 
         Height          =   315
         Left            =   3000
         TabIndex        =   80
         Top             =   1320
         Width           =   1455
      End
      Begin VB.ComboBox UnitType 
         Height          =   315
         ItemData        =   "EditDes.frx":0442
         Left            =   360
         List            =   "EditDes.frx":0482
         Sorted          =   -1  'True
         Style           =   2  'Dropdown List
         TabIndex        =   79
         Top             =   1320
         Width           =   2295
      End
      Begin VB.CommandButton AddVars 
         Caption         =   "New Variable"
         Height          =   500
         Index           =   1
         Left            =   1440
         TabIndex        =   86
         Top             =   6120
         Width           =   1000
      End
      Begin VB.ComboBox StochCombo 
         Height          =   315
         ItemData        =   "EditDes.frx":0592
         Left            =   360
         List            =   "EditDes.frx":059F
         Style           =   2  'Dropdown List
         TabIndex        =   84
         Top             =   3480
         Width           =   4095
      End
      Begin VB.TextBox MinText 
         Height          =   350
         Left            =   360
         TabIndex        =   81
         Top             =   2040
         Width           =   975
      End
      Begin VB.TextBox MaxText 
         Height          =   350
         Left            =   1680
         TabIndex        =   82
         Top             =   2040
         Width           =   975
      End
      Begin VB.TextBox DescText 
         Height          =   350
         Left            =   360
         TabIndex        =   83
         Top             =   2760
         Width           =   4095
      End
      Begin VB.TextBox VarNameText 
         Height          =   350
         Left            =   360
         TabIndex        =   78
         Top             =   600
         Width           =   4095
      End
      Begin MSComDlg.CommonDialog CommonDialog2 
         Left            =   120
         Top             =   5040
         _ExtentX        =   847
         _ExtentY        =   847
         _Version        =   393216
         Filter          =   "Global Input Data Files (*.gid)""|*.gid*"
      End
      Begin VB.Label Label40 
         Caption         =   "Cue String"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   120
         TabIndex        =   57
         Top             =   3960
         Width           =   2025
      End
      Begin VB.Label Label37 
         Caption         =   "Units"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   2760
         TabIndex        =   16
         Top             =   1080
         Width           =   975
      End
      Begin VB.Label Label38 
         Caption         =   "Stochastic"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   120
         TabIndex        =   15
         Top             =   3240
         Width           =   975
      End
      Begin VB.Label Label31 
         Caption         =   "Name"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   120
         TabIndex        =   14
         Top             =   360
         Width           =   975
      End
      Begin VB.Label Label33 
         Caption         =   "Unit Type"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   120
         TabIndex        =   13
         Top             =   1080
         Width           =   975
      End
      Begin VB.Label Label34 
         Caption         =   "Min"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   120
         TabIndex        =   12
         Top             =   1800
         Width           =   975
      End
      Begin VB.Label Label35 
         Caption         =   "Max"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   1440
         TabIndex        =   11
         Top             =   1800
         Width           =   975
      End
      Begin VB.Label Label36 
         Caption         =   "Description"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   120
         TabIndex        =   10
         Top             =   2520
         Width           =   1215
      End
   End
   Begin VB.Frame VarDesPanel 
      Caption         =   "Variable Cues"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   6780
      Left            =   4440
      TabIndex        =   0
      Top             =   0
      Visible         =   0   'False
      Width           =   4890
      Begin VB.CommandButton DeleteCue 
         Caption         =   "Delete Cue"
         Height          =   500
         Left            =   2520
         TabIndex        =   98
         Top             =   6120
         Width           =   1000
      End
      Begin VB.TextBox CueText 
         Height          =   525
         Index           =   0
         Left            =   360
         MultiLine       =   -1  'True
         TabIndex        =   54
         TabStop         =   0   'False
         Top             =   5400
         Visible         =   0   'False
         Width           =   4095
      End
      Begin VB.CommandButton GenVar 
         Caption         =   "Generate Variables"
         Height          =   500
         Index           =   0
         Left            =   1440
         TabIndex        =   97
         Top             =   6120
         Width           =   1000
      End
      Begin VB.CommandButton AddVars 
         Caption         =   "New Cue"
         Height          =   500
         Index           =   0
         Left            =   3600
         TabIndex        =   99
         Top             =   6120
         Width           =   1000
      End
      Begin VB.TextBox DesVarName 
         Height          =   350
         Left            =   360
         TabIndex        =   90
         Top             =   1200
         Width           =   4095
      End
      Begin VB.ComboBox DesIndex6 
         Height          =   315
         ItemData        =   "EditDes.frx":05C9
         Left            =   360
         List            =   "EditDes.frx":05E5
         Style           =   2  'Dropdown List
         TabIndex        =   96
         Top             =   4800
         Width           =   4095
      End
      Begin VB.ComboBox DesIndex5 
         Height          =   315
         ItemData        =   "EditDes.frx":0626
         Left            =   360
         List            =   "EditDes.frx":0642
         Style           =   2  'Dropdown List
         TabIndex        =   95
         Top             =   4200
         Width           =   4095
      End
      Begin VB.ComboBox DesIndex4 
         Height          =   315
         ItemData        =   "EditDes.frx":0683
         Left            =   360
         List            =   "EditDes.frx":069F
         Style           =   2  'Dropdown List
         TabIndex        =   94
         Top             =   3600
         Width           =   4095
      End
      Begin VB.ComboBox DesIndex3 
         Height          =   315
         ItemData        =   "EditDes.frx":06E0
         Left            =   360
         List            =   "EditDes.frx":06FC
         Style           =   2  'Dropdown List
         TabIndex        =   93
         Top             =   3000
         Width           =   4095
      End
      Begin VB.ComboBox DesIndex2 
         Height          =   315
         ItemData        =   "EditDes.frx":073D
         Left            =   360
         List            =   "EditDes.frx":0759
         Style           =   2  'Dropdown List
         TabIndex        =   92
         Top             =   2400
         Width           =   4095
      End
      Begin VB.ComboBox DesIndex1 
         Height          =   315
         ItemData        =   "EditDes.frx":079A
         Left            =   360
         List            =   "EditDes.frx":07B6
         Style           =   2  'Dropdown List
         TabIndex        =   91
         Top             =   1800
         Width           =   4095
      End
      Begin VB.ComboBox DesVarFlag 
         Height          =   315
         ItemData        =   "EditDes.frx":07F7
         Left            =   360
         List            =   "EditDes.frx":0807
         Style           =   2  'Dropdown List
         TabIndex        =   89
         Top             =   600
         Width           =   4095
      End
      Begin VB.Label Label39 
         Caption         =   "Cue String"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   120
         TabIndex        =   56
         Top             =   5160
         Width           =   1545
      End
      Begin VB.Label Label30 
         Caption         =   "Index 6"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   120
         TabIndex        =   8
         Top             =   4560
         Width           =   705
      End
      Begin VB.Label Label29 
         Caption         =   "Index 5"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   120
         TabIndex        =   7
         Top             =   3960
         Width           =   705
      End
      Begin VB.Label Label28 
         Caption         =   "Index 4"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   120
         TabIndex        =   6
         Top             =   3360
         Width           =   705
      End
      Begin VB.Label Label27 
         Caption         =   "Index 3"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   120
         TabIndex        =   5
         Top             =   2760
         Width           =   705
      End
      Begin VB.Label Label26 
         Caption         =   "Index 2"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   120
         TabIndex        =   4
         Top             =   2160
         Width           =   705
      End
      Begin VB.Label Label25 
         Caption         =   "Type"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   120
         TabIndex        =   3
         Top             =   360
         Width           =   705
      End
      Begin VB.Label Label24 
         Caption         =   "Index 1"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   120
         TabIndex        =   2
         Top             =   1560
         Width           =   705
      End
      Begin VB.Label Label17 
         Caption         =   "Name"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   120
         TabIndex        =   1
         Top             =   960
         Width           =   705
      End
   End
   Begin VB.Frame GidSelect 
      Caption         =   "Select Gid Section"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   6780
      Left            =   4440
      TabIndex        =   52
      Top             =   0
      Visible         =   0   'False
      Width           =   4890
      Begin VB.CommandButton GenButton 
         Caption         =   "Generate"
         Height          =   375
         Left            =   3360
         TabIndex        =   101
         Top             =   1080
         Visible         =   0   'False
         Width           =   855
      End
      Begin VB.ComboBox SectName 
         Height          =   315
         ItemData        =   "EditDes.frx":082C
         Left            =   360
         List            =   "EditDes.frx":082E
         Style           =   2  'Dropdown List
         TabIndex        =   100
         Top             =   600
         Width           =   4095
      End
      Begin VB.Label Label32 
         Caption         =   "Section Name"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   350
         Left            =   120
         TabIndex        =   53
         Top             =   360
         Width           =   2535
      End
   End
   Begin VB.Frame SysReqPanel 
      Caption         =   "System Requirements"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   6780
      Left            =   4440
      TabIndex        =   36
      Top             =   0
      Visible         =   0   'False
      Width           =   4890
      Begin VB.TextBox DiskText 
         Height          =   350
         Left            =   360
         TabIndex        =   63
         Top             =   3120
         Width           =   4095
      End
      Begin VB.TextBox RAMText 
         Height          =   350
         Left            =   360
         TabIndex        =   62
         Top             =   2400
         Width           =   4095
      End
      Begin VB.TextBox ProcText 
         Height          =   350
         Left            =   360
         TabIndex        =   61
         Top             =   1440
         Width           =   4095
      End
      Begin VB.TextBox OSText 
         Enabled         =   0   'False
         Height          =   350
         Left            =   360
         TabIndex        =   60
         Text            =   "WIN 95 / NT"
         Top             =   600
         Width           =   4095
      End
      Begin VB.Label Label23 
         Caption         =   "Disk Space"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   120
         TabIndex        =   40
         Top             =   2880
         Width           =   1650
      End
      Begin VB.Label Label22 
         Caption         =   "RAM Memory"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   90
         TabIndex        =   39
         Top             =   2040
         Width           =   1650
      End
      Begin VB.Label Label21 
         Caption         =   "Processor"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   90
         TabIndex        =   38
         Top             =   1200
         Width           =   1650
      End
      Begin VB.Label Label20 
         Caption         =   "Operating System"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   90
         TabIndex        =   37
         Top             =   360
         Width           =   1650
      End
   End
   Begin VB.Frame DesDesc 
      Caption         =   "Description Files"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   6780
      Left            =   4440
      TabIndex        =   50
      Top             =   0
      Width           =   4890
      Begin VB.TextBox DescInf 
         BackColor       =   &H8000000A&
         Height          =   6255
         Left            =   240
         Locked          =   -1  'True
         MultiLine       =   -1  'True
         ScrollBars      =   3  'Both
         TabIndex        =   51
         Top             =   360
         Width           =   4455
      End
   End
   Begin VB.Frame InfPanel 
      Caption         =   "Description File Information"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   6780
      Left            =   4440
      TabIndex        =   45
      Top             =   0
      Width           =   4890
      Begin VB.CommandButton BrowseExe 
         Caption         =   "Browse"
         Height          =   350
         Index           =   2
         Left            =   3720
         TabIndex        =   123
         Top             =   1920
         Width           =   975
      End
      Begin VB.TextBox Text3 
         Height          =   350
         Left            =   360
         TabIndex        =   122
         Top             =   1920
         Width           =   3255
      End
      Begin VB.CommandButton Command1 
         Caption         =   "Add New Module Name"
         Height          =   735
         Left            =   3720
         TabIndex        =   118
         Top             =   480
         Width           =   975
      End
      Begin VB.ComboBox Combo1 
         Height          =   315
         ItemData        =   "EditDes.frx":0830
         Left            =   360
         List            =   "EditDes.frx":0832
         Style           =   2  'Dropdown List
         TabIndex        =   117
         Top             =   1200
         Width           =   3255
      End
      Begin VB.TextBox Text2 
         Height          =   350
         Left            =   360
         TabIndex        =   115
         Top             =   6240
         Width           =   3255
      End
      Begin VB.TextBox Text1 
         Height          =   350
         Left            =   360
         TabIndex        =   113
         Top             =   4800
         Width           =   3255
      End
      Begin VB.TextBox ModVersText 
         Height          =   350
         Left            =   360
         TabIndex        =   108
         Top             =   3360
         Width           =   3255
      End
      Begin VB.CommandButton BrowseExe 
         Caption         =   "Browse"
         Height          =   350
         Index           =   1
         Left            =   3720
         TabIndex        =   112
         Top             =   5520
         Width           =   975
      End
      Begin VB.CommandButton BrowseExe 
         Caption         =   "Browse"
         Height          =   350
         Index           =   0
         Left            =   3720
         TabIndex        =   110
         Top             =   4080
         Width           =   975
      End
      Begin VB.TextBox ModNmText 
         Height          =   350
         Left            =   360
         TabIndex        =   106
         Top             =   2640
         Width           =   3255
      End
      Begin VB.TextBox ModExeText 
         Height          =   350
         Left            =   360
         TabIndex        =   111
         Top             =   5520
         Width           =   3255
      End
      Begin VB.TextBox UIText 
         Height          =   350
         Left            =   360
         TabIndex        =   109
         Top             =   4080
         Width           =   3255
      End
      Begin VB.ComboBox ModTypeCombo 
         Height          =   315
         ItemData        =   "EditDes.frx":0834
         Left            =   360
         List            =   "EditDes.frx":0844
         Sorted          =   -1  'True
         Style           =   2  'Dropdown List
         TabIndex        =   107
         Top             =   480
         Width           =   3255
      End
      Begin MSComDlg.CommonDialog CommonDialog3 
         Left            =   3720
         Top             =   6000
         _ExtentX        =   847
         _ExtentY        =   847
         _Version        =   393216
         Filter          =   "Application Files (*.exe)|*.exe*"
      End
      Begin VB.Label Label45 
         Caption         =   "Module Description"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   120
         TabIndex        =   124
         Top             =   2400
         Width           =   1935
      End
      Begin VB.Label Label44 
         Caption         =   "Module Icon"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   120
         TabIndex        =   121
         Top             =   1680
         Width           =   1935
      End
      Begin VB.Label Label43 
         Caption         =   "Module Arguments"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   120
         TabIndex        =   116
         Top             =   6000
         Width           =   1935
      End
      Begin VB.Label Label42 
         Caption         =   "UI Arguments"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   120
         TabIndex        =   114
         Top             =   4560
         Width           =   1935
      End
      Begin VB.Label Label41 
         Caption         =   "Module Version"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   120
         TabIndex        =   105
         Top             =   3120
         Width           =   1935
      End
      Begin VB.Label Label4 
         Caption         =   "Module Executable"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   120
         TabIndex        =   49
         Top             =   5280
         Width           =   1935
      End
      Begin VB.Label Label3 
         Caption         =   "UI Executable"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   120
         TabIndex        =   48
         Top             =   3840
         Width           =   1935
      End
      Begin VB.Label Label2 
         Caption         =   "Module Type"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   90
         TabIndex        =   47
         Top             =   240
         Width           =   1935
      End
      Begin VB.Label Label1 
         Caption         =   "Module Name"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   90
         TabIndex        =   46
         Top             =   960
         Width           =   1935
      End
   End
   Begin VB.Frame DescPanel 
      Caption         =   "Communication Information"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   6780
      Left            =   4440
      TabIndex        =   24
      Top             =   0
      Visible         =   0   'False
      Width           =   4890
      Begin VB.TextBox URLText 
         Height          =   350
         Left            =   360
         TabIndex        =   73
         Top             =   3120
         Width           =   4095
      End
      Begin VB.TextBox EmailText 
         Height          =   350
         Left            =   360
         TabIndex        =   72
         Top             =   2280
         Width           =   4095
      End
      Begin MSMask.MaskEdBox PhoneEdit 
         Height          =   375
         Left            =   360
         TabIndex        =   70
         Top             =   600
         Width           =   1575
         _ExtentX        =   2778
         _ExtentY        =   661
         _Version        =   393216
         MaxLength       =   14
         Mask            =   "(###) ###-####"
         PromptChar      =   "_"
      End
      Begin MSMask.MaskEdBox FaxEdit 
         Height          =   375
         Left            =   360
         TabIndex        =   71
         Top             =   1440
         Width           =   1575
         _ExtentX        =   2778
         _ExtentY        =   661
         _Version        =   393216
         MaxLength       =   14
         Mask            =   "(###) ###-####"
         PromptChar      =   "_"
      End
      Begin VB.Label Label15 
         Caption         =   "URL Address"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   120
         TabIndex        =   28
         Top             =   2880
         Width           =   1935
      End
      Begin VB.Label Label14 
         Caption         =   "Email Address"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   120
         TabIndex        =   27
         Top             =   2040
         Width           =   1935
      End
      Begin VB.Label Label11 
         Caption         =   "Fax Number"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   120
         TabIndex        =   26
         Top             =   1200
         Width           =   1935
      End
      Begin VB.Label Label10 
         Caption         =   "Telephone Number"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   90
         TabIndex        =   25
         Top             =   360
         Width           =   1935
      End
   End
   Begin VB.Frame ModDescPanel 
      Caption         =   "Module Description"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   6780
      Left            =   4440
      TabIndex        =   43
      Top             =   120
      Visible         =   0   'False
      Width           =   4890
      Begin VB.TextBox ModDescText 
         Height          =   6255
         Left            =   240
         MultiLine       =   -1  'True
         ScrollBars      =   3  'Both
         TabIndex        =   58
         Top             =   360
         Width           =   4455
      End
      Begin VB.TextBox NewDesc 
         Height          =   375
         Left            =   240
         MultiLine       =   -1  'True
         ScrollBars      =   2  'Vertical
         TabIndex        =   44
         Top             =   5400
         Visible         =   0   'False
         Width           =   4215
      End
   End
   Begin VB.Frame LimitsPanel 
      Caption         =   "Module References"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   6780
      Left            =   4440
      TabIndex        =   41
      Top             =   0
      Visible         =   0   'False
      Width           =   4890
      Begin VB.TextBox LimitText 
         Height          =   6255
         Left            =   240
         MultiLine       =   -1  'True
         ScrollBars      =   3  'Both
         TabIndex        =   59
         Top             =   360
         Width           =   4455
      End
      Begin VB.TextBox NewLimits 
         Height          =   375
         Left            =   360
         MultiLine       =   -1  'True
         ScrollBars      =   2  'Vertical
         TabIndex        =   42
         Top             =   5400
         Visible         =   0   'False
         Width           =   4095
      End
   End
   Begin VB.Frame ContactPanel 
      Caption         =   "Address Information"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   6780
      Left            =   4440
      TabIndex        =   29
      Top             =   120
      Visible         =   0   'False
      Width           =   4890
      Begin VB.TextBox ModlrText 
         Height          =   350
         Left            =   360
         TabIndex        =   103
         Top             =   600
         Width           =   4095
      End
      Begin VB.TextBox CntryText 
         Height          =   350
         Left            =   360
         TabIndex        =   69
         Top             =   4800
         Width           =   4095
      End
      Begin VB.TextBox CompText 
         Height          =   350
         Left            =   360
         TabIndex        =   64
         Top             =   1440
         Width           =   4095
      End
      Begin VB.TextBox AddrssText 
         Height          =   350
         Left            =   360
         TabIndex        =   65
         Top             =   2280
         Width           =   4095
      End
      Begin VB.TextBox CityText 
         Height          =   350
         Left            =   360
         TabIndex        =   66
         Top             =   3120
         Width           =   4095
      End
      Begin VB.TextBox ZipText 
         Height          =   350
         Left            =   1920
         TabIndex        =   68
         Top             =   3960
         Width           =   1980
      End
      Begin VB.ComboBox StateCombo 
         Height          =   315
         ItemData        =   "EditDes.frx":086E
         Left            =   360
         List            =   "EditDes.frx":090B
         Sorted          =   -1  'True
         Style           =   2  'Dropdown List
         TabIndex        =   67
         Top             =   3960
         Width           =   975
      End
      Begin VB.Label Label6 
         Caption         =   "Contact Name"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   120
         TabIndex        =   104
         Top             =   360
         Width           =   1935
      End
      Begin VB.Label Label9 
         Caption         =   "Country"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   120
         TabIndex        =   35
         Top             =   4560
         Width           =   1455
      End
      Begin VB.Label Label8 
         Caption         =   "City"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   90
         TabIndex        =   34
         Top             =   2880
         Width           =   1935
      End
      Begin VB.Label Label7 
         Caption         =   "Address"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   120
         TabIndex        =   33
         Top             =   2040
         Width           =   1935
      End
      Begin VB.Label Label5 
         Caption         =   "Company Name"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   90
         TabIndex        =   32
         Top             =   1200
         Width           =   1935
      End
      Begin VB.Label Label13 
         Caption         =   "Zip Code"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   1680
         TabIndex        =   31
         Top             =   3720
         Width           =   1065
      End
      Begin VB.Label Label12 
         Caption         =   "State"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   120
         TabIndex        =   30
         Top             =   3720
         Width           =   690
      End
   End
   Begin VB.Frame ConDesc 
      Caption         =   "Connection Description"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   6780
      Left            =   4440
      TabIndex        =   22
      Top             =   0
      Width           =   4890
      Begin VB.TextBox Connect 
         BackColor       =   &H8000000A&
         Height          =   6255
         Left            =   240
         Locked          =   -1  'True
         MultiLine       =   -1  'True
         ScrollBars      =   3  'Both
         TabIndex        =   23
         Top             =   360
         Width           =   4455
      End
   End
   Begin VB.Frame ReadPanel 
      Caption         =   "Reads"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   6780
      Left            =   4440
      TabIndex        =   18
      Top             =   0
      Visible         =   0   'False
      Width           =   4890
      Begin VB.CommandButton Command2 
         Caption         =   "Add New File Type"
         Height          =   615
         Left            =   3720
         TabIndex        =   119
         Top             =   5880
         Width           =   975
      End
      Begin VB.TextBox ReadMx 
         Height          =   350
         Left            =   1230
         TabIndex        =   76
         Text            =   "0"
         Top             =   4830
         Width           =   588
      End
      Begin VB.TextBox ReadMn 
         Height          =   350
         Left            =   360
         TabIndex        =   75
         Text            =   "0"
         Top             =   4830
         Width           =   588
      End
      Begin VB.ListBox ReadList 
         Height          =   1860
         ItemData        =   "EditDes.frx":09DB
         Left            =   120
         List            =   "EditDes.frx":09DD
         Style           =   1  'Checkbox
         TabIndex        =   74
         Top             =   360
         Width           =   4455
      End
      Begin VB.Label Label19 
         Caption         =   "Max"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   960
         TabIndex        =   21
         Top             =   4560
         Width           =   585
      End
      Begin VB.Label Label18 
         Caption         =   "Min"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   345
         Left            =   120
         TabIndex        =   20
         Top             =   4560
         Width           =   585
      End
      Begin VB.Label Label16 
         AutoSize        =   -1  'True
         Caption         =   "Number of Files"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   240
         Left            =   120
         TabIndex        =   19
         Top             =   4320
         Width           =   1410
      End
   End
   Begin VB.Frame WritePanel 
      Caption         =   "Writes"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   6780
      Left            =   4440
      TabIndex        =   17
      Top             =   0
      Visible         =   0   'False
      Width           =   4890
      Begin VB.CommandButton Command3 
         Caption         =   "Add New File Type"
         Height          =   615
         Left            =   3720
         TabIndex        =   120
         Top             =   5880
         Width           =   975
      End
      Begin VB.ListBox WriteList 
         Height          =   1860
         ItemData        =   "EditDes.frx":09DF
         Left            =   120
         List            =   "EditDes.frx":09E1
         Style           =   1  'Checkbox
         TabIndex        =   77
         Top             =   360
         Width           =   4455
      End
   End
   Begin ComctlLib.ImageList ImageList2 
      Left            =   1200
      Top             =   5856
      _ExtentX        =   1005
      _ExtentY        =   1005
      BackColor       =   -2147483643
      ImageWidth      =   20
      ImageHeight     =   20
      MaskColor       =   12632256
      _Version        =   327682
      BeginProperty Images {0713E8C2-850A-101B-AFC0-4210102A8DA7} 
         NumListImages   =   5
         BeginProperty ListImage1 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "EditDes.frx":09E3
            Key             =   ""
         EndProperty
         BeginProperty ListImage2 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "EditDes.frx":0CFD
            Key             =   ""
         EndProperty
         BeginProperty ListImage3 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "EditDes.frx":1017
            Key             =   ""
         EndProperty
         BeginProperty ListImage4 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "EditDes.frx":1331
            Key             =   ""
         EndProperty
         BeginProperty ListImage5 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "EditDes.frx":164B
            Key             =   ""
         EndProperty
      EndProperty
   End
   Begin VB.Menu file 
      Caption         =   "&File"
      Begin VB.Menu newfile 
         Caption         =   "&New"
      End
      Begin VB.Menu openfile 
         Caption         =   "&Open"
      End
      Begin VB.Menu line1 
         Caption         =   "-"
      End
      Begin VB.Menu save 
         Caption         =   "Sa&ve"
      End
      Begin VB.Menu saveas 
         Caption         =   "Sav&e As"
      End
      Begin VB.Menu line2 
         Caption         =   "-"
      End
      Begin VB.Menu prntfile 
         Caption         =   "&Print"
      End
      Begin VB.Menu line3 
         Caption         =   "-"
      End
      Begin VB.Menu exit 
         Caption         =   "E&xit"
      End
   End
   Begin VB.Menu VarOpt 
      Caption         =   "&Variable Options"
      Visible         =   0   'False
      Begin VB.Menu AddDes 
         Caption         =   "&Add Variable Cue"
      End
      Begin VB.Menu DeleteVar 
         Caption         =   "&Delete Variable"
      End
   End
   Begin VB.Menu DescOpt 
      Caption         =   "&Descriptor Options"
      Visible         =   0   'False
      Begin VB.Menu DelDesc 
         Caption         =   "De&lete Cue"
      End
   End
   Begin VB.Menu addvarb 
      Caption         =   "&Add Variable"
      Visible         =   0   'False
      Begin VB.Menu addv 
         Caption         =   "A&dd Variable"
      End
   End
   Begin VB.Menu edit 
      Caption         =   "&Edit"
      Begin VB.Menu cut 
         Caption         =   "Cu&t"
      End
      Begin VB.Menu copy 
         Caption         =   "&Copy"
      End
      Begin VB.Menu paste 
         Caption         =   "&Paste"
      End
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Compare Text
Dim reads(256) As filetype
Dim lasttreeindex As Long
Dim nodx As Node
Dim lastparen As Node
Dim lastvar As New Vars
Dim outreads() As filetype
Dim gidfile As parmfile
Dim gidrec As parmrec
Dim readcnt As Long
Dim minlist(256) As Long
Dim maxlist(256) As Long
Dim newflag As Boolean
Dim newvar As Boolean
Dim gotfoc As Boolean
Dim loading As Boolean
Dim tempname As String
Dim fileout As csv
Dim unitreset As Boolean
Const STARTICON = 2
Const VISITEDICON = 3
Const ERRORICON = 5
Const VERSTRING = "MODULE VERSION"
Const DESCSTRING = "MODULE DESCRIPTION"
Const LIMITSTRING = "MODULE REFERENCES"
Const CONNECTSTRING = "VALID CONNECTIONS"
Const INPUTCONNSTRING = "Valid Input Reads"
Const OUTPUTCONNSTRING = "Valid Output Writes"
Const SYSSTRING = "SYSTEM REQUIREMENTS"
Const OSSTRING = "Operating System:   "
Const PROCSTRING = "Processor:      "
Const RAMSTRING = "RAM Memory:       "
Const DISKSTRING = "Disk Space:       "
Const POCSTRING = "POINT OF CONTACT"
Const COMPSTRING = "Company Name:     "
Const MODLRSTRING = "Contact Name:     "
Const ADDSTRING = "Mailing Address:    "
Const CITYSTRING = "City:         "
Const CNTRYSTRING = "Country:        "
Const STSTRING = "State:        "
Const ZIPSTRING = "Zip Code:       "
Const PHONESTRING = "Telephone Number:   "
Const FAXSTRING = "Fax Number:       "
Const EMAILSTRING = "Email Address:    "
Const URLSTRING = "URL Address:      "
Dim filein As csv
Dim versindex As Long
Dim descindex As Long
Dim limitindex As Long
Dim osindex As Long
Dim procindex As Long
Dim ramindex As Long
Dim diskindex As Long
Dim sysindex As Long
Dim compindex As Long
Dim modlrindex As Long
Dim addindex As Long
Dim cityindex As Long
Dim stindex As Long
Dim zipindex As Long
Dim phoneindex As Long
Dim faxindex As Long
Dim emailindex As Long
Dim urlindex As Long
Dim pocindex As Long
Dim cntryindex As Long
Dim connectindex As Long
Dim inputindex As Long
Dim outputindex As Long
Dim opencheck As Boolean
Dim modified As Boolean
Dim variables As New Collection
Dim savecheck As Boolean
Dim sections As SectionName
Public Sub ConnectStatus()
  If ReadList.SelCount = 0 And WriteList.SelCount = 0 Then
    TreeView1.Nodes.item(8).Image = ERRORICON
    TreeView1.Nodes.item(9).Image = ERRORICON
    TreeView1.Nodes.item(10).Image = ERRORICON
  ElseIf ReadList.SelCount > 0 And WriteList.SelCount = 0 Then
    TreeView1.Nodes.item(8).Image = STARTICON
    TreeView1.Nodes.item(9).Image = VISITEDICON
    TreeView1.Nodes.item(10).Image = STARTICON
  ElseIf ReadList.SelCount = 0 And WriteList.SelCount > 0 Then
    TreeView1.Nodes.item(8).Image = STARTICON
    TreeView1.Nodes.item(9).Image = STARTICON
    TreeView1.Nodes.item(10).Image = VISITEDICON
  Else
    TreeView1.Nodes.item(8).Image = VISITEDICON
    TreeView1.Nodes.item(9).Image = VISITEDICON
    TreeView1.Nodes.item(10).Image = VISITEDICON
  End If
End Sub
Public Sub CheckNodeStatus(previndex As Long)
  Dim stacheck As Boolean
  Dim i As Long
  Dim tempkey As String
  Select Case previndex
    Case 1
      Exit Sub
    Case 2
      If ModNmText.Text = "" Or ModTypeCombo.Text = "" Then
        TreeView1.Nodes.item(previndex).Image = ERRORICON
      ElseIf UIText.Text = "" And ModExeText.Text = "" Then
        TreeView1.Nodes.item(previndex).Image = ERRORICON
      ElseIf ModVersText.Text = "" Then
        TreeView1.Nodes.item(previndex).Image = ERRORICON
      ElseIf UIText.Text = "" Or ModExeText.Text = "" Then
        TreeView1.Nodes.item(previndex).Image = STARTICON
      ElseIf Text3.Text = "" Then
        TreeView1.Nodes.item(previndex).Image = STARTICON
      Else
        TreeView1.Nodes.item(previndex).Image = VISITEDICON
      End If
    Case 3
      If ModDescText.Text = "" Then
        TreeView1.Nodes.item(previndex).Image = STARTICON
      Else
        TreeView1.Nodes.item(previndex).Image = VISITEDICON
      End If
    Case 4
      If LimitText.Text = "" Then
        TreeView1.Nodes.item(previndex).Image = STARTICON
      Else
        TreeView1.Nodes.item(previndex).Image = VISITEDICON
      End If
    Case 5
      If ProcText.Text = "" Or RAMText.Text = "" Or DiskText = "" Then
        TreeView1.Nodes.item(previndex).Image = STARTICON
      Else
        TreeView1.Nodes.item(previndex).Image = VISITEDICON
      End If
    Case 6
      If CompText.Text = "" Or AddrssText.Text = "" Or CityText.Text = "" Or StateCombo.Text = "" Or ZipText.Text = "" Or CntryText.Text = "" Or ModlrText.Text = "" Then
        TreeView1.Nodes.item(previndex).Image = STARTICON
      Else
        TreeView1.Nodes.item(previndex).Image = VISITEDICON
      End If
    Case 7
      If PhoneEdit.Text = "(   )   -   " Or PhoneEdit.Text = "(###) ###-####" Or PhoneEdit.Text = "(___) ___-____" Then
        TreeView1.Nodes.item(previndex).Image = STARTICON
      ElseIf FaxEdit.Text = "(   )   -   " Or FaxEdit.Text = "(###) ###-####" Or FaxEdit.Text = "(___) ___-____" Then
        TreeView1.Nodes.item(previndex).Image = STARTICON
      ElseIf EmailText.Text = "" Or URLText.Text = "" Then
        TreeView1.Nodes.item(previndex).Image = STARTICON
      Else
        TreeView1.Nodes.item(previndex).Image = VISITEDICON
      End If
    Case 8
      ConnectStatus
    Case 9
      ConnectStatus
    Case 10
      ConnectStatus
    Case 11
      statcheck = True
      If TreeView1.Nodes.Count > 11 Then
        For i = 12 To TreeView1.Nodes.Count
          If TreeView1.Nodes.item(i).Image = STARTICON Then
            statcheck = False
          End If
        Next
      End If
      If statcheck Then
        TreeView1.Nodes.item(previndex).Image = VISITEDICON
      Else
        TreeView1.Nodes.item(previndex).Image = STARTICON
      End If
    Case Is > 11
      If TreeView1.Nodes.Count >= previndex Then
        If TreeView1.Nodes.item(previndex).Parent.Key = "var" Then
          tempkey = TreeView1.Nodes(previndex).Key
          If variables.item(tempkey).desc = "" Then
            TreeView1.Nodes.item(previndex).Image = STARTICON
          Else
            TreeView1.Nodes.item(previndex).Image = VISITEDICON
          End If
        Else
          TreeView1.Nodes.item(previndex).Image = VISITEDICON
        End If
      End If
  End Select
End Sub
Public Sub InitInfoPanels()
  Dim msg As String
  Dim temp As String
  Dim infocheck As Boolean
  Dim concheck As Boolean
  infocheck = True
  concheck = True
  On Error GoTo InfoFileError
  Open App.Path + "\Description.txt" For Input As #1
  On Error GoTo 0
  If infocheck Then
    Do While Not EOF(1)
      temp = Input(1, #1)
      msg = msg + temp
    Loop
  Else
    msg = ""
  End If
  DescInf.Text = msg
  Close #1
  msg = ""
  On Error GoTo ConnectFileError
  Open App.Path + "\connect.txt" For Input As #1
  On Error GoTo 0
  If concheck Then
    Do While Not EOF(1)
      temp = Input(1, #1)
      msg = msg + temp
    Loop
  Else
    msg = ""
  End If
  Connect.Text = msg
  Close #2
  Exit Sub
InfoFileError:
  infocheck = False
  Resume Next
ConnectFileError:
  concheck = False
  Resume Next
End Sub
Public Function checkemail(tempstring As String) As Boolean
  Dim i As Long
  Dim tempchar As String
  If tempstring = "" Then
    checkemail = True
    Exit Function
  End If
  For i = 1 To Len(tempstring)
    tempchar = Mid(tempstring, i, 1)
    If tempchar = "@" Then
      checkemail = True
      Exit Function
    End If
  Next
  checkemail = False
End Function
Public Sub RemDesVars(temp As Vars)
  Dim i As Long
  For i = 1 To temp.labels.Count
    temp.labels.Remove 1
  Next
End Sub
Public Sub RemVars()
  Dim i As Long
  For i = 1 To variables.Count
    RemDesVars variables.item(1)
    variables.Remove 1
  Next
End Sub
Public Function GetSectName(Name As String)
  Dim i As Integer
  getstectname = ""
  For i = 0 To UBound(modnames) - 1
    If Name = modnames(i) Then
      GetSectName = modids(i) + "Name"
      Exit Function
    End If
  Next
'  Select Case name
'    Case "Sensitivity"
'      GetSectName = "SenName"
'    Case "Source"
'      GetSectName = "SrcName"
'    Case "Air"
'      GetSectName = "AirName"
'    Case "Vadose Zone"
'      GetSectName = "VadName"
'    Case "Aquifer"
'      GetSectName = "AquName"
'    Case "Exposure Pathways"
'      GetSectName = "ExpName"
'    Case "Receptor Intake"
'      GetSectName = "RcpName"
'    Case "Health Impacts"
'      GetSectName = "HeiName"
'    Case "Surface Water"
'      GetSectName = "RivName"
'    Case "Overland Flow"
'      GetSectName = "OvlName"
'    Case "Constituent"
'      GetSectName = "ConName"
'    Case "Viewer"
'      GetSectName = "VwrName"
'    Case "Ecological Dose"
'      GetSectName = "EcoName"
'    Case Else
'      GetSectName = ""
'  End Select
End Function
Public Function ReadSectNames(gidfile As String) As Boolean
  Dim filecheck As Boolean
  Dim filein As parmfile
  Dim tempparm As parmrec
  Dim fuicnt As Long
  Dim parmcheck As Long
  Dim fuicheck As Boolean
  Dim SecName As String
  Dim i As Long
  filecheck = open_parm(filein, gidfile, 2)
  If Not filecheck Then
    MsgBox "Unable to open GID file", vbExclamation + vbOKOnly + vbSystemModal, "Error"
    ReadSectNames = False
    Exit Function
  End If
   fuicheck = find_frst(filein, tempparm, "FUI")
   If fuicheck = False Then
    MsgBox "Unable to read GID file, Missing FUI section", vbExclamation + vbOKOnly + vbSystemModal, "Error"
    ReadSectNames = False
    Exit Function
   End If
'  SecName = GetSectName(ModTypeCombo.Text)
  SecName = GetSectName(Combo1.Text)
  fuicnt = tempparm.idx1
  For i = 1 To fuicnt
    parmcheck = read_parmrec(filein, tempparm)
    If UCase(tempparm.pName) = UCase(SecName) Then
      sections.cnt = sections.cnt + 1
      ReDim Preserve sections.names(sections.cnt) As String
      ReDim Preserve sections.labels(sections.cnt) As String
      sections.names(sections.cnt - 1) = tempparm.pval
      parmcheck = read_parmrec(filein, tempparm)
      sections.labels(sections.cnt - 1) = tempparm.pval
      i = i + 1
    End If
  Next
  For i = 0 To sections.cnt - 1
    SectName.AddItem sections.names(i) + " (" + sections.labels(i) + ")"
  Next
  If sections.cnt > 0 Then
    SectName.ListIndex = 0
  Else
    MsgBox "There are no matching sections in the GID file", vbExclamation + vbOKOnly + vbSystemModal, "Error"
    ReadSectNames = False
    close_parm filein
    Exit Function
  End If
  ReadSectNames = True
  close_parm filein
End Function
Public Sub GetVariables(gidfile As String)
  Dim filecheck As Boolean
  Dim filein As parmfile
  Dim tempparm As parmrec
  Dim tempvar As New Vars
  Dim original As New Vars
  Dim tempdes As New DesVars
  Dim fuicnt As Long
  Dim parmcheck As Long
  Dim SecName As String
  Dim searchcheck As Long
  Dim i As Long
  Dim j As Long
  Dim cuecnt As Long
  cuecnt = 0
  filecheck = open_parm(filein, gidfile, 2)
  If Not filecheck Then
    MsgBox "Unable to open GID file " + gidfile, vbExclamation + vbOKOnly + vbSystemModal, "Error"
    Exit Sub
  End If
  searchcheck = find_frst(filein, tempparm, sections.names(SectName.ListIndex))
  For i = 1 To tempparm.idx1
    Set tempvar = New Vars
    Set tempvar.labels = New Collection
    parmcheck = read_parmrec(filein, tempparm)
    Set tempdes = New DesVars
    tempvar.Name = tempparm.pName
    tempvar.units = tempparm.cunit
    GetCueIndices tempparm.idx1, tempdes, cuecnt, tempvar
    GetCueIndices tempparm.idx2, tempdes, cuecnt, tempvar
    GetCueIndices tempparm.idx3, tempdes, cuecnt, tempvar
    GetCueIndices tempparm.idx4, tempdes, cuecnt, tempvar
    GetCueIndices tempparm.idx5, tempdes, cuecnt, tempvar
    On Error GoTo errorhandler
    variables.Add tempvar, tempvar.Name
    On Error GoTo 0
    cuecnt = 0
  Next
    RemTreeVars
    PutTreeVars
    close_parm filein
    Exit Sub
errorhandler:
    CheckCueNumber tempdes, tempvar
    Resume Next
End Sub
Public Sub GetCueIndices(Index As Long, tempdes As DesVars, cuecnt As Long, tempvar As Vars)
  If Index > 0 Then
    Set tempdes = New DesVars
    cuecnt = cuecnt + 1
    tempdes.Name = "cue " + Str(cuecnt)
    tempdes.varflag = "Label"
    tempvar.labels.Add tempdes, tempdes.Name + tempvar.Name
  End If
End Sub
Public Sub CheckCueNumber(tempdes As DesVars, tempvar As Vars)
  Dim tmpdes As New DesVars
  If tempvar.labels.Count > variables.item(tempvar.Name).labels.Count Then
    For j = variables.item(tempvar.Name).labels.Count + 1 To tempvar.labels.Count
      Set tmpdes = New DesVars
      tmpdes.Name = "cue " + Str(j)
      variables.item(tempvar.Name).labels.Add tempdes, tempdes.Name + tempvar.Name
    Next
  End If
End Sub
Public Sub GetIndices(tempstring As String)
  Dim header As String
  versindex = InStr(1, tempstring, VERSTRING, vbTextCompare)
  descindex = InStr(1, tempstring, DESCSTRING, vbTextCompare)
  limitindex = InStr(1, tempstring, LIMITSTRING, vbTextCompare)
  sysindex = InStr(1, tempstring, SYSSTRING, vbTextCompare)
  osindex = InStr(1, tempstring, OSSTRING, vbTextCompare)
  procindex = InStr(1, tempstring, PROCSTRING, vbTextCompare)
  ramindex = InStr(1, tempstring, RAMSTRING, vbTextCompare)
  diskindex = InStr(1, tempstring, DISKSTRING, vbTextCompare)
  pocindex = InStr(1, tempstring, POCSTRING, vbTextCompare)
  compindex = InStr(1, tempstring, COMPSTRING, vbTextCompare)
  modlrindex = InStr(1, tempstring, MODLRSTRING, vbTextCompare)
  addindex = InStr(1, tempstring, ADDSTRING, vbTextCompare)
  cityindex = InStr(1, tempstring, CITYSTRING, vbTextCompare)
  stindex = InStr(1, tempstring, STSTRING, vbTextCompare)
  cntryindex = InStr(1, tempstring, CNTRYSTRING, vbTextCompare)
  zipindex = InStr(1, tempstring, ZIPSTRING, vbTextCompare)
  phoneindex = InStr(1, tempstring, PHONESTRING, vbTextCompare)
  faxindex = InStr(1, tempstring, FAXSTRING, vbTextCompare)
  emailindex = InStr(1, tempstring, EMAILSTRING, vbTextCompare)
  urlindex = InStr(1, tempstring, URLSTRING, vbTextCompare)
  connectindex = InStr(1, tempstring, CONNECTSTRING, vbTextCompare)
  inputindex = InStr(1, tempstring, INPUTCONNSTRING, vbTextCompare)
  outputindex = InStr(1, tempstring, OUTPUTCONNSTRING, vbTextCompare)
End Sub
Public Function GetHeadLen(header As String) As Long
  Select Case header
    Case VERSTRING
      GetHeadLen = Len(VERSTRING)
    Case DESCSTRING
      GetHeadLen = Len(DESCSTRING)
    Case LIMITSTRING
      GetHeadLen = Len(LIMITSTRING)
    Case SYSSTRING
      GetHeadLen = Len(SYSSTRING)
    Case OSSTRING
      GetHeadLen = Len(OSSTRING)
    Case PROCSTRING
      GetHeadLen = Len(PROCSTRING)
    Case RAMSTRING
      GetHeadLen = Len(RAMSTRING)
    Case DISKSTRING
      GetHeadLen = Len(DISKSTRING)
    Case POCSTRING
      GetHeadLen = Len(POCSTRING)
    Case COMPSTRING
      GetHeadLen = Len(COMPSTRING)
    Case MODLRSTRING
      GetHeadLen = Len(MODLRSTRING)
    Case ADDSTRING
      GetHeadLen = Len(ADDSTRING)
    Case CITYSTRING
      GetHeadLen = Len(CITYSTRING)
    Case STSTRING
      GetHeadLen = Len(STSTRING)
    Case ZIPSTRING
      GetHeadLen = Len(ZIPSTRING)
    Case PHONESTRING
      GetHeadLen = Len(PHONESTRING)
    Case FAXSTRING
      GetHeadLen = Len(FAXSTRING)
    Case EMAILSTRING
      GetHeadLen = Len(EMAILSTRING)
    Case URLSTRING
      GetHeadLen = Len(URLSTRING)
    Case CNTRYSTRING
      GetHeadLen = Len(CNTRYSTRING)
    Case CONNECTSTRING
      GetHeadLen = Len(CONNECTSTRING)
    Case INPUTCONNSTRING
      GetHeadLen = Len(INPUTCONNSTRING)
    Case ouputconnstring
      GetHeadLen = Len(OUTPUTCONNSTRING)
    Case Else
      GetHeadLen = 0
  End Select
End Function
Public Function GetNxtIndex(previndex As Long, header As String, inputstring As String) As Long
  Dim start As Long
  start = previndex + GetHeadLen(header)
  If Not InStr(start, inputstring, VERSTRING, 0) = 0 Then
    GetNxtIndex = InStr(start, inputstring, VERSTRING, 0)
    Exit Function
  End If
  If Not InStr(start, inputstring, DESCSTRING, 0) = 0 Then
    GetNxtIndex = InStr(start, inputstring, DESCSTRING, 0)
    Exit Function
  End If
  If Not InStr(start, inputstring, LIMITSTRING, 0) = 0 Then
    GetNxtIndex = InStr(start, inputstring, LIMITSTRING, 0)
    Exit Function
  End If
  If Not InStr(start, inputstring, CONNECTSTRING, 0) = 0 Then
    GetNxtIndex = InStr(start, inputstring, CONNECTSTRING, 0)
    Exit Function
  End If
  If Not InStr(start, inputstring, INPUTCONNSTRING, 0) = 0 Then
    GetNxtIndex = InStr(start, inputstring, INPUTCONNSTRING, 0)
    Exit Function
  End If
  If Not InStr(start, inputstring, OUTPUTCONNSTRING, 0) = 0 Then
    GetNxtIndex = InStr(start, inputstring, OUTPUTCONNSTRING, 0)
    Exit Function
  End If
  If Not InStr(start, inputstring, SYSSTRING, 0) = 0 Then
    GetNxtIndex = InStr(start, inputstring, SYSSTRING, 0)
    Exit Function
  End If
  If Not InStr(start, inputstring, OSSTRING, 0) = 0 Then
    GetNxtIndex = InStr(start, inputstring, OSSTRING, 0)
    Exit Function
  End If
  If Not InStr(start, inputstring, PROCSTRING, 0) = 0 Then
    GetNxtIndex = InStr(start, inputstring, PROCSTRING, 0)
    Exit Function
  End If
  If Not InStr(start, inputstring, RAMSTRING, 0) = 0 Then
    GetNxtIndex = InStr(start, inputstring, RAMSTRING, 0)
    Exit Function
  End If
  If Not InStr(start, inputstring, DISKSTRING, 0) = 0 Then
    GetNxtIndex = InStr(start, inputstring, DISKSTRING, 0)
    Exit Function
  End If
  If Not InStr(start, inputstring, POCSTRING, 0) = 0 Then
    GetNxtIndex = InStr(start, inputstring, POCSTRING, 0)
    Exit Function
  End If
  If Not InStr(start, inputstring, COMPSTRING, 0) = 0 Then
    GetNxtIndex = InStr(start, inputstring, COMPSTRING, 0)
    Exit Function
  End If
  If Not InStr(start, inputstring, MODLRSTRING, 0) = 0 Then
    GetNxtIndex = InStr(start, inputstring, MODLRSTRING, 0)
    Exit Function
  End If
  If Not InStr(start, inputstring, ADDSTRING, 0) = 0 Then
    GetNxtIndex = InStr(start, inputstring, ADDSTRING, 0)
    Exit Function
  End If
  If Not InStr(start, inputstring, CITYSTRING, 0) = 0 Then
    GetNxtIndex = InStr(start, inputstring, CITYSTRING, 0)
    Exit Function
  End If
  If Not InStr(start, inputstring, STSTRING, 0) = 0 Then
    GetNxtIndex = InStr(start, inputstring, STSTRING, 0)
    Exit Function
  End If
  If Not InStr(start, inputstring, ZIPSTRING, 0) = 0 Then
    GetNxtIndex = InStr(start, inputstring, ZIPSTRING, 0)
    Exit Function
  End If
  If Not InStr(start, inputstring, CNTRYSTRING, 0) = 0 Then
    GetNxtIndex = InStr(start, inputstring, CNTRYSTRING, 0)
    Exit Function
  End If
  If Not InStr(start, inputstring, PHONESTRING, 0) = 0 Then
    GetNxtIndex = InStr(start, inputstring, PHONESTRING, 0)
    Exit Function
  End If
  If Not InStr(start, inputstring, FAXSTRING, 0) = 0 Then
    GetNxtIndex = InStr(start, inputstring, FAXSTRING, 0)
    Exit Function
  End If
  If Not InStr(start, inputstring, EMAILSTRING, 0) = 0 Then
    GetNxtIndex = InStr(start, inputstring, EMAILSTRING, 0)
    Exit Function
  End If
  If Not InStr(start, inputstring, URLSTRING, 0) = 0 Then
    GetNxtIndex = InStr(start, inputstring, URLSTRING, 0)
    Exit Function
  End If
  GetNxtIndex = Len(inputstring) + 1
End Function
Public Function GetDataString(inputstring As String, header As String, desccheck As Boolean) As String
  Dim s1 As Long
  Dim e1 As Long
  Dim s2 As Long
  Dim tabcnt As Long
  Dim tempstring As String
  Dim i As Long
  tempstring = ""
  s1 = GetHeadIndex(header)
  If s1 = 0 Then
    GetDataString = ""
    Exit Function
  End If
  e1 = s1 + GetHeadLen(header)
  s2 = GetNxtIndex(s1, header, inputstring)
  If header = VERSTRING Then
    s2 = s2 - 4
  ElseIf header = DESCSTRING Then
    s2 = s2 - 4
  ElseIf header = LIMITSTRING Then
    's2 = s2 - 4
    s2 = s2 - 2
  ElseIf header = CONNECTSTRING Then
    s2 = s2 - 4
  ElseIf header = INPUTCONNSTRING Then
    s2 = s2 - 4
  ElseIf header = OUTPUTCONNSTRING Then
    s2 = s2 - 4
  End If
  If desccheck Then
    For i = e1 + 2 To s2 - 1
      tempstring = tempstring + Mid(inputstring, i, 1)
    Next
  Else
    For i = e1 To s2 - 1
      If Not (Mid(inputstring, i, 1) = Chr(13) Or Mid(inputstring, i, 1) = Chr(10) Or Mid(inputstring, i, 1) = Chr(34)) Then
        tempstring = tempstring + Mid(inputstring, i, 1)
      End If
    Next
  End If
  GetDataString = Trim(tempstring)
End Function
Public Function GetHeadIndex(header As String) As Long
  Select Case header
    Case VERSTRING
      GetHeadIndex = versindex
    Case DESCSTRING
      GetHeadIndex = descindex
    Case LIMITSTRING
      GetHeadIndex = limitindex
    Case SYSSTRING
      GetHeadIndex = sysindex
    Case OSSTRING
      GetHeadIndex = osindex
    Case PROCSTRING
      GetHeadIndex = procindex
    Case RAMSTRING
      GetHeadIndex = ramindex
    Case DISKSTRING
      GetHeadIndex = diskindex
    Case POCSTRING
      GetHeadIndex = pocindex
    Case COMPSTRING
      GetHeadIndex = compindex
    Case MODLRSTRING
      GetHeadIndex = modlrindex
    Case ADDSTRING
      GetHeadIndex = addindex
    Case CITYSTRING
      GetHeadIndex = cityindex
    Case STSTRING
      GetHeadIndex = stindex
    Case ZIPSTRING
      GetHeadIndex = zipindex
    Case PHONESTRING
      GetHeadIndex = phoneindex
    Case FAXSTRING
      GetHeadIndex = faxindex
    Case EMAILSTRING
      GetHeadIndex = emailindex
    Case URLSTRING
      GetHeadIndex = urlindex
    Case CNTRYSTRING
      GetHeadIndex = cntryindex
    Case CONNECTSTRING
      GetHeadIndex = connectindex
    Case INPUTCONNSTRING
      GetHeadIndex = inputindex
    Case OUTPUTCONNSTRING
      GetHeadIndex = outputindex
    Case Else
      GetHeadIndex = 0
  End Select
End Function
Public Sub RemTreeVars()
  Dim i As Long
  For i = 1 To TreeView1.Nodes.item("var").Children
    TreeView1.Nodes.Remove TreeView1.Nodes.item("var").Child.Key
  Next
End Sub
Public Sub PutTreeVars()
  Dim i As Long
  Dim j As Long
  Dim Index As Long
  Dim msg As String
  For i = 1 To variables.Count
    Index = 1
    Set nodx = TreeView1.Nodes.Add("var", tvwChild, variables.item(i).Name, variables.item(i).Name, STARTICON, 1)
    For j = 1 To variables.item(i).labels.Count
      Index = 2
      parkey = variables.item(i).Name
      deskey = variables.item(i).labels.item(j).Name
      deskey = deskey + parkey
      Set nodx = TreeView1.Nodes.Add(parkey, tvwChild, deskey, variables.item(i).labels.item(j).Name, STARTICON, 1)
    Next
  Next
End Sub
Public Function CheckVariable() As Boolean
  If VarNameText.Text = "" Then
    CheckVariable = False
  ElseIf DescText.Text = "" Then
    CheckVariable = False
  ElseIf CheckMax(MaxText.Text, MinText.Text) = False Then
    CheckVariable = False
  Else
    CheckVariable = True
  End If
End Function

Private Sub Combo1_Click()
  Dim i As Integer
  For i = 0 To UBound(modnames) - 1
    If Combo1.Text = modnames(i) Then
      Text3.Text = modicons(i)
      Exit For
    End If
  Next
End Sub

Private Sub Command1_Click()
  ModAdd.Show vbModal, Me
End Sub

Private Sub Command2_Click()
  Dim tempstring As String
  Dim i As Integer
  Dim errmsg As String
  fileaddcheck = False
  FileAdd.Show vbModal, Me
  If fileaddcheck Then
    tempstring = tempextension + " " + tempqualifier
    For i = 0 To ReadList.ListCount - 1
      If tempstring = ReadList.list(i) Then
        errmsg = "Type qualifier combination already exists" + Chr(13) + Chr(10)
        errmsg = errmsg + "Please enter a unique type qualifier combination"
        MsgBox errmsg, vbExclamation + vbOKOnly, "Error"
        Exit Sub
      End If
    Next
    ReadList.AddItem tempstring
    WriteList.AddItem tempstring
  End If
End Sub
Private Sub Command3_Click()
  Dim tempstring As String
  Dim i As Integer
  Dim errmsg As String
  fileaddcheck = False
  FileAdd.Show vbModal, Me
  If fileaddcheck Then
    tempstring = tempextension + " " + tempqualifier
    For i = 0 To ReadList.ListCount - 1
      If tempstring = ReadList.list(i) Then
        errmsg = "Type qualifier combination already exists" + Chr(13) + Chr(10)
        errmsg = errmsg + "Please enter a unique type qualifier combination"
        MsgBox errmsg, vbExclamation + vbOKOnly, "Error"
        Exit Sub
      End If
    Next
    ReadList.AddItem tempstring
    WriteList.AddItem tempstring
  End If
End Sub
Private Sub copy_Click()
  SendKeys "^c"
End Sub
Private Sub cut_Click()
  SendKeys "^x"
End Sub
Private Sub DeleteCue_Click()
  DelDesc_Click
  TreeView1_NodeClick TreeView1.SelectedItem
End Sub

Private Sub DescText_LostFocus()
  Dim nodx As Node
  Set nodx = TreeView1.SelectedItem
  UpdateCueString
  'TreeView1_NodeClick nodx
  'DescText.SetFocus
End Sub

Private Sub DltVar_Click()
  DeleteVar_Click
  TreeView1_NodeClick TreeView1.SelectedItem
End Sub
Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
  Dim msg As String
  If modified Then
    msg = "Data may have been changed" + Chr(13) + Chr(10)
    msg = msg + "Do you wish to save any changes?"
    If MsgBox(msg, vbQuestion + vbSystemModal + vbYesNo, "Save?") = vbYes Then
      save_Click
    Else
      savecheck = True
    End If
  End If
  If savecheck = False Then
    Cancel = True
  Else
    Cancel = False
  End If
End Sub
Public Sub PrintField(printr As Printer, header As String, textstring As String)
  Dim oldy
  printr.CurrentX = 1000
  oldy = printr.CurrentY
  printr.FontBold = True
  printr.Print header
  printr.FontBold = False
  printr.CurrentX = 4000
  printr.CurrentY = oldy
  printr.Print textstring
'  CheckEOP printr
End Sub
Public Sub CheckEOP(prntr As Printer)
  If prntr.CurrentY >= 2 * (15120 / prntr.TwipsPerPixelY) Then
    prntr.NewPage
    prntr.CurrentY = 2 * (720 / prntr.TwipsPerPixelY)
  End If
End Sub
Public Sub CheckOrphan(prntr As Printer)
  If prntr.CurrentY + (240 / prntr.TwipsPerPixelY) >= 2 * (15120 / prntr.TwipsPerPixelY) Then
    prntr.NewPage
    prntr.CurrentY = 2 * (720 / prntr.TwipsPerPixelY)
  End If
End Sub
Public Sub GetDescLines(lines() As String, linecnt As Long, inputstring As String)
  Dim temp As String
  Dim line As String
  Dim character As String
  Dim i As Long
  temp = inputstring
  linecnt = 0
  For i = 1 To Len(temp)
    character = Mid(temp, i, 1)
    If character = Chr(13) Then
      ReDim Preserve lines(linecnt + 1) As String
      lines(linecnt) = line
      linecnt = linecnt + 1
      line = ""
    ElseIf Not character = Chr(10) Then
      line = line + character
    End If
  Next
End Sub
Public Sub ParseArguments(cmdline As String, executable As String, arguments As String)
  Dim i As Integer
  Dim separator As Integer
  separator = InStr(cmdline, " ")
  If Not separator = 0 Then
    executable = Mid(cmdline, 1, separator - 1)
    arguments = Mid(cmdline, separator + 1, Len(cmdline) - separator)
  Else
    executable = cmdline
    arguments = ""
  End If
End Sub
Public Function SearchModNames(ModName As String) As Boolean
  Dim Match As Boolean
  Dim i As Integer
  Match = False
  For i = 0 To UBound(modnames) - 1
    If ModName = modnames(i) Then
      Match = True
      Exit For
    End If
  Next
  SearchModNames = Match
End Function
Public Function SearchModIDs(modid As String) As Boolean
  Dim Match As Boolean
  Dim i As Integer
  Match = False
  For i = 0 To UBound(modids) - 1
    If modid = modids(i) Then
      Match = True
      Exit For
    End If
  Next
  SearchModIDs = Match
End Function
Public Function SearchModIcons(modicon As String)
  Dim mathc As Boolean
  Dim i As Integer
  Match = False
  For i = 0 To UBound(modicons) - 1
    If modicon = modicons(i) Then
      Match = True
      Exit For
    End If
  Next
  SearchModIcons = Match
End Function
Public Sub SetModType(modtype As String)
  Dim i As Integer
  For i = 0 To ModTypeCombo.ListCount - 1
    If modtype = ModTypeCombo.list(i) Then
      ModTypeCombo.ListIndex = i
      Exit Sub
    End If
  Next
End Sub
Public Sub SetModName(ModName As String)
  Dim i As Integer
  For i = 0 To Combo1.ListCount - 1
    If ModName = Combo1.list(i) Then
      Combo1.ListIndex = i
      Exit Sub
    End If
  Next
End Sub
Public Sub SetModIcon(ModName As String)
  Dim i As Integer
  For i = 0 To UBound(modnames) - 1
    If ModName = modnames(i) Then
      Text3.Text = modicons(i)
      Exit Sub
    End If
  Next
End Sub
Public Sub GetModType(tempname As String, modtype As String, ModName As String, modid As String, midicon As String)
  Select Case tempname
    Case "Air"
      modtype = "Model"
      ModName = "Air"
      modid = "air"
      modicon = "c:\frames\air.ico"
    Case "Aquifer"
      modtype = "Model"
      ModName = "Aquifer"
      modid = "aqu"
      modicon = "c:\frames\aqu.ico"
    Case "Contaminant"
      modtype = "Database"
      ModName = "Constituent"
      modid = "con"
      modicon = "c:\frames\con.ico"
    Case "Ecological Benchmark"
      modtype = "Database"
      ModName = "Eco Aquatic Benchmark"
      modid = "ebf"
      modicon = "c:\frames\ebf.ico"
    Case "Exposure Pathways"
      modtype = "Model"
      ModName = "Exposure Pathways"
      modid = "exp"
      modicon = "c:\frames\exp.ico"
    Case "Health Impacts"
      modtype = "Model"
      ModName = "Health Impacts"
      modid = "hei"
      modicon = "c:\frames\hei.ico"
    Case "Overland Flow"
      modtype = "Model"
      ModName = "Overland Flow"
      modid = "ovl"
      modicon = "c:\frames\ovl.ico"
    Case "Receptor Intake"
      modtype = "Model"
      ModName = "Receptor Intake"
      modid = "rcp"
      modicon = "c:\frames\rcp.ico"
    Case "Sensitivity"
      modtype = "System"
      ModName = "Sensitivity"
      modid = "sen"
      modicon = "c:\frames\sen.ico"
    Case "Source"
      modtype = "Model"
      ModName = "Source"
      modid = "src"
      modicon = "c:\frames\src.ico"
    Case "Surface Water"
      modtype = "Model"
      ModName = "Surface Water"
      modid = "riv"
      modicon = "c:\frames\riv.ico"
    Case "Vadose Zone"
      modtype = "Model"
      ModName = "Vadose Zone"
      modid = "vad"
      modicon = "c:\frames\vad.ico"
    Case "Viewer"
      modtype = "Viewer"
      ModName = "Viewer"
      modid = "vwr"
      modicon = "c:\frames\vwr.ico"
    Case "Closed Method"
      modtype = "Model"
      ModName = "Closed Method"
      modid = "cls"
      modicon = "c:\frames\cls.ico"
    Case Else
      modtype = ""
      ModName = ""
      modid = ""
      modicon = ""
  End Select
End Sub
Private Sub GetFileInfo(filein As csv)
  Dim temp As String
  Dim executable As String
  Dim arguments As String
  Dim temptype As String
  Dim tempname As String
  Dim tempid As String
  Dim tempicon As String
  Dim tempstring As String
  get_line filein
  temp = get_val(filein)
  ParseTypeInfo ":", temp, temptype, tempname, tempid, tempicon
  If tempname = "" Then
    tempstring = temptype
    GetModType tempstring, temptype, tempname, tempid, tempicon
  End If
  SetModType (temptype)
  If SearchModNames(tempname) Then
    
    SetModName (tempname)
'    If tempicon = "" Then
'      SetModIcon (tempname)
'    Else
'      Text3.Text = tempicon
'    End If
  Else
    ReDim Preserve modtypes(UBound(modtypes) + 1) As String
    ReDim Preserve modnames(UBound(modnames) + 1) As String
    ReDim Preserve modids(UBound(modids) + 1) As String
'    ReDim Preserve modicons(UBound(modicons) + 1) As String
    modtypes(UBound(modtypes) - 1) = temptype
    modnames(UBound(modnames) - 1) = tempname
    Combo1.AddItem tempname
    Combo1.ListIndex = Combo1.ListCount - 1
    modids(UBound(modids) - 1) = tempid
'    modicons(UBound(modicons) - 1) = tempicon
'    Text3.Text = tempicon
  End If
  ModNmText.Text = get_val(filein)
  temp = get_val(filein)
  ParseArguments temp, executable, arguments
  UIText.Text = executable
  Text1.Text = arguments
  temp = get_val(filein)
  ParseArguments temp, executable, arguments
  ModExeText.Text = executable
  Text2.Text = arguments
  tempicon = get_val(filein)
  If SearchModNames(tempname) Then
    If tempicon = "" Then
      SetModIcon (tempname)
    Else
      Text3.Text = tempicon
    End If
  Else
    ReDim Preserve modicons(UBound(modicons) + 1) As String
    modicons(UBound(modicons) - 1) = tempicon
    Text3.Text = tempicon
  End If
    
End Sub
Public Function GetDescString(fName As String) As String
  Dim tempbuff As String
  Open fName For Input As #5
  Line Input #5, tempbuff
  Line Input #5, tempbuff
  tempbuff = Input(1, #5)
  GetDescString = tempbuff
  tempbuff = Input(1, #5)
  GetDescString = GetDescString + tempbuff
  While Not tempbuff = Chr(34)
    tempbuff = Input(1, #5)
    If tempbuff = Chr(13) Then
      tempbuff = Input(1, #5)
      tempbuff = Chr(13) + Chr(10)
    End If
    GetDescString = GetDescString + tempbuff
  Wend
  GetDescString = GetDescString + Chr(34)
  Close #5
End Function
Private Function GetDescInfo(filein As csv) As String
  Dim temp As String
  Dim phonetemp As String
  Dim tempstate As String
  Dim tempint As Long
  Dim faxtemp As String
   close_csv filein
   temp = GetDescString(filein.fName)
   GetIndices temp
   ModVersText.Text = GetDataString(temp, VERSTRING, False)
   ModDescText.Text = GetDataString(temp, DESCSTRING, True)
   LimitText.Text = GetDataString(temp, LIMITSTRING, True)
   ProcText.Text = GetDataString(temp, PROCSTRING, False)
   RAMText.Text = GetDataString(temp, RAMSTRING, False)
   DiskText.Text = GetDataString(temp, DISKSTRING, False)
   CompText.Text = GetDataString(temp, COMPSTRING, False)
   ModlrText.Text = GetDataString(temp, MODLRSTRING, False)
   AddrssText.Text = GetDataString(temp, ADDSTRING, False)
   CityText.Text = GetDataString(temp, CITYSTRING, False)
   tempstate = GetDataString(temp, STSTRING, False)
   tempint = GetStIndex(tempstate)
   StateCombo.ListIndex = tempint
   ZipText.Text = GetDataString(temp, ZIPSTRING, False)
   CntryText.Text = GetDataString(temp, CNTRYSTRING, False)
   phonetemp = GetDataString(temp, PHONESTRING, False)
   If Not phonetemp = "" Then
     PhoneEdit.Mask = ""
     PhoneEdit.Text = phonetemp
   Else
     PhoneEdit.Mask = "(###) ###-####"
   End If
   faxtemp = GetDataString(temp, FAXSTRING, False)
   If Not faxtemp = "" Then
     FaxEdit.Mask = ""
     FaxEdit.Text = faxtemp
   Else
     FaxEdit.Mask = "(###) ###-####"
   End If
   EmailText.Text = GetDataString(temp, EMAILSTRING, False)
   URLText.Text = GetDataString(temp, URLSTRING, False)
   If (ModVersText.Text = "") And (ModDescText.Text = "") And (LimitText.Text = "") And (ProcText.Text = "") And (RAMText.Text = "") And _
   (DiskText.Text = "") And (CompText.Text = "") And (ModlrText.Text = "") And (AddrssText.Text = "") And (CityText.Text = "") And _
   (tempstate = "") And (ZipText.Text = "") And (CntryText.Text = "") And (phonetemp = "") And (faxtemp = "") And _
   (EmailText.Text = "") And (URLText.Text = "") Then
    ModDescText.Text = temp
   End If
   GetDescInfo = ""
End Function
Private Sub GetReads(filein As csv, checkstring As String)
  Dim temp As String
  Dim i As Long
  Dim j As Long
  Dim tempreads As Long
  Dim mincnt As Long
  Dim tempindex As Long
  Dim tempstring As String
  Dim temptype As String
  Dim tempqual As String
  Dim found As Boolean
  If checkstring = "" Then
    get_line filein
    readcnt = Val(get_val(filein))
    temp = get_val(filein)
  Else
    readcnt = Val(checkstring)
    temp = get_val(filein)
  End If
  get_line filein
  ReDim inreads(readcnt, readcnt) As filetype
  For j = 0 To readcnt - 1
    tempreads = get_val(filein)
    For i = 0 To readcnt - 1
      inreads(i, j).Name = get_val(filein)
      inreads(i, j).ident = get_val(filein)
      inreads(i, j).min = Val(get_val(filein))
      inreads(i, j).max = Val(get_val(filein))
    Next
    get_line filein
  Next
  For i = 0 To readcnt - 1
    tempstring = inreads(i, j).Name + " " + inreads(i, j).ident
    reads(i).Name = inreads(i, 0).Name
    reads(i).ident = inreads(i, 0).ident
    reads(i).min = inreads(i, 0).min
    reads(i).max = inreads(i, 0).max
  Next
  For i = 0 To readcnt - 1
    mincnt = 0
    For j = 0 To readcnt - 1
      mincnt = mincnt + inreads(i, j).min
    Next
    If mincnt = 1 And Not readcnt = 1 Then
      reads(i).min = 0
    End If
  Next
  For i = 0 To readcnt - 1
    If Not reads(i).Name = "" Then
      tempindex = i
      found = False
      For j = 0 To ReadList.ListCount - 1
        tempstring = reads(i).Name + " " + reads(i).ident
        If tempstring = ReadList.list(j) Then
          ReadList.Selected(j) = True
          maxlist(j) = reads(i).max
          minlist(j) = reads(i).min
          found = True
          Exit For
        End If
      Next
      If found = False Then
        For j = 0 To ReadList.ListCount - 1
          If InStr(1, ReadList.list(j), reads(i).Name) > 0 Then
            ReadList.Selected(j) = True
            maxlist(j) = reads(i).max
            minlist(j) = reads(i).min
            found = True
            Exit For
          End If
        Next
      End If
      If found = False Then
        ReadList.AddItem tempstring
        ReadList.Selected(ReadList.ListCount - 1) = True
        WriteList.AddItem tempstring
        maxlist(ReadList.ListCount - 1) = reads(i).max
        minlist(ReadList.ListCount - 1) = reads(i).min
      End If
    End If
  Next
  
  ReadMn.Text = minlist(tempindex)
  ReadMx.Text = maxlist(tempindex)
'  Label16.Caption = ReadList.Text
  GetFileType ReadList.Text, temptype, tempqual
  Label16.Caption = temptype + " , " + tempqual
End Sub
Private Sub GetWrites(filein As csv)
  Dim writecnt As Long
  Dim temp As String
  Dim i As Long
  Dim writename As String
  Dim writeident As String
  Dim found As Boolean
  writecnt = Val(get_val(filein))
  temp = get_val(filein)
  get_line filein
  For i = 0 To writecnt - 1
    temp = get_val(filein)
    get_line filein
    ParseFileInfo temp, writename, writeident
    temp = writename + " " + writeident
    found = False
    For j = 0 To WriteList.ListCount - 1
      If temp = WriteList.list(j) Then
        WriteList.Selected(j) = True
        found = True
        Exit For
      End If
    Next
    If found = False Then
      For j = 0 To WriteList.ListCount - 1
        If InStr(1, WriteList.list(j), temp) > 0 Then
          WriteList.Selected(j) = True
          found = True
          Exit For
        End If
      Next
    End If
    If found = False Then
      ReadList.AddItem temp
      WriteList.AddItem temp
      WriteList.Selected(WriteList.ListCount - 1) = True
    End If
  Next
End Sub
Private Sub ReadVars(filein As csv)
  Dim tmpvar As New Vars
  Dim errorindex As Long
  Dim linecnt As Long
  Dim tmpdes As New DesVars
  Dim temp2 As String
  Set tmpvar.labels = New Collection
  Dim errormsg As String
  RemVars
  linecnt = 1
  temp = get_val(filein)
  If Not temp = "" Then
    If Val(temp) = 0 Then
      temp = get_val(filein)
      get_line filein
      linecnt = linecnt + 1
      Exit Sub
    End If
  End If
  temp = get_val(filein)
  get_line filein
  linecnt = linecnt + 1
  While Not EOCF(filein)
    temp = get_val(filein)
    temp2 = get_val(filein)
    Set tmpvar = New Vars
    Set tmpdes = New DesVars
    If Not (temp = "Label" Or temp = "Variable") And InStr(1, temp2, "cue", 1) = 0 Then
      errorindex = 0
      tmpvar.Name = temp
      tmpvar.stoch = temp2
      If tmpvar.stoch = "Not Stochastic" Then
        StochCombo.ListIndex = 0
      ElseIf tmpvar.stoch = "Continuous" Then
        StochCombo.ListIndex = 1
      Else
        StochCombo.ListIndex = 2
      End If
      tmpvar.units = get_val(filein)
      temp = get_val(filein)
      If temp = "Min" Then
        tmpvar.min = get_val(filein)
        temp = get_val(filein)
        If temp = "Max" Then
          tmpvar.max = get_val(filein)
        Else
          tmpvar.max = ""
        End If
      Else
        tmpvar.min = ""
        temp = get_val(filein)
        If temp = "Max" Then
          tmpvar.max = get_val(filein)
        Else
          tmpvar.max = ""
        End If
      End If
      tmpvar.desc = get_val(filein)
      temp = get_val(filein)
      On Error GoTo ERROR_HANDLER
      variables.Add tmpvar, tmpvar.Name
      On Error GoTo 0
      On Error Resume Next
      Set nodx = TreeView1.Nodes.Add("var", tvwChild, tmpvar.Name, tmpvar.Name, STARTICON, 1)
      On Error GoTo 0
      Set lastparen = nodx
      Set lastvar = tmpvar
      get_line filein
      linecnt = linecnt + 1
    Else
      errorindex = 1
      tmpdes.varflag = temp
      tmpdes.Name = temp2
      tmpdes.Index1 = get_val(filein)
      tmpdes.Index2 = get_val(filein)
      tmpdes.Index3 = get_val(filein)
      tmpdes.Index4 = get_val(filein)
      tmpdes.Index5 = get_val(filein)
      tmpdes.Index6 = get_val(filein)
      DesVarName.Text = tmpdes.Name
      If tmpdes.varflag = "Label" Then
        DesVarFlag.ListIndex = 0
      Else
        DesVarFlag.ListIndex = 1
      End If
      DesIndex1.ListIndex = GetDesIndex(tmpdes.Index1)
      DesIndex2.ListIndex = GetDesIndex(tmpdes.Index2)
      DesIndex3.ListIndex = GetDesIndex(tmpdes.Index3)
      DesIndex4.ListIndex = GetDesIndex(tmpdes.Index4)
      DesIndex5.ListIndex = GetDesIndex(tmpdes.Index5)
      DesIndex6.ListIndex = GetDesIndex(tmpdes.Index6)
      temp = lastvar.Name
      On Error GoTo ERROR_HANDLER
      Set nodx = TreeView1.Nodes.Add(lastparen.Key, tvwChild, tmpdes.Name + lastparen.Key, tmpdes.Name, STARTICON, 1)
      On Error GoTo 0
      If lastparen.Key = "" Then
        errormsg = "Unable to add variable cue " + tmpdes.Name
        errormsg = errormsg + Chr(13) + Chr(10) + "The cue is associated with "
        errormsg = errormsg + "a duplicate variable that has been discarded"
        errormsg = errormsg + Chr(13) + Chr(10) + "Line #"
        errormsg = errormsg + CStr(linecnt)
        errormsg = errormsg + " in the variable section of the description file"
        MsgBox errormsg, vbExclamation + vbOKOnly + vbSystemModal, "Error"
      Else
        variables.item(lastvar.Name).labels.Add tmpdes, tmpdes.Name + lastparen.Key
      End If
      get_line filein
      linecnt = linecnt + 1
    End If
  Wend
  SetDesVarFlag
  Exit Sub
ERROR_HANDLER:
  If errorindex = 0 Then
    errormsg = "Unable to add variable " + tmpvar.Name
    errormsg = errormsg + Chr(13) + Chr(10) + "Variable is a duplicate"
    errormsg = errormsg + Chr(13) + Chr(10) + "Line #"
    errormsg = errormsg + CStr(linecnt)
    errormsg = errormsg + " in the variable section of the description file"
    MsgBox errormsg, vbExclamation + vbOKOnly + vbSystemModal, "Error"
  Else
    errormsg = "Unable to add variable cue " + tmpdes.Name
    errormsg = errormsg + Chr(13) + Chr(10) + "Variable cue is a duplicate"
    errormsg = errormsg + Chr(13) + Chr(10) + "Line #"
    errormsg = errormsg + CStr(linecnt)
    errormsg = errormsg + " in the variable section of the description file"
    MsgBox errormsg, vbExclamation + vbOKOnly + vbSystemModal, "Error"
  End If
  Resume Next
End Sub
Public Function GetDesIndex(temp As String) As Long
  Select Case temp
    Case "Label"
      GetDesIndex = 0
    Case "Variable"
      GetDesIndex = 1
    Case "Site"
      GetDesIndex = 0
    Case "Model"
      GetDesIndex = 1
    Case "Index1"
      GetDesIndex = 2
    Case "Index2"
      GetDesIndex = 3
    Case "Index3"
      GetDesIndex = 4
    Case "Index4"
      GetDesIndex = 5
    Case "Index5"
      GetDesIndex = 6
    Case "Index6"
      GetDesIndex = 7
    Case Else
      GetDesIndex = -1
  End Select
End Function
Public Sub SetDesVarFlag()
  Dim i As Long
  DesVarFlag.Clear
  DesVarFlag.AddItem "Label"
  DesVarFlag.AddItem "Variable"
  DesVarFlag.AddItem "FSCNAME"
  DesVarFlag.AddItem "CASID"
  For i = 1 To variables.Count
    DesVarFlag.AddItem variables.item(i).Name
  Next
End Sub
Public Function GetDesFlagIndex(temp As String) As Long
  Dim i As Long
  Dim X As Long
  For i = 0 To variables.Count + 3
    X = DesVarFlag.ListCount
    DesVarFlag.ListIndex = i
    If DesVarFlag.Text = temp Then
      GetDesFlagIndex = i
      Exit Function
    End If
  Next
End Function
Private Sub WriteVars(fileout As csv)
  Dim i As Long
  Dim j As Long
  put_val fileout, variables.Count
  put_val fileout, "Variables"
  put_line fileout
  For i = 1 To variables.Count
    put_val fileout, variables.item(i).Name
    put_val fileout, variables.item(i).stoch
    put_val fileout, variables.item(i).units
    If Not variables.item(i).min = "" Then
      put_val fileout, "Min"
      put_val fileout, variables.item(i).min
    ElseIf variables.item(i).min = "" Then
      put_val fileout, ""
    End If
    If Not variables.item(i).max = "" Then
      put_val fileout, "Max"
      put_val fileout, variables.item(i).max
    ElseIf variables.item(i).max = "" Then
      put_val fileout, ""
    End If
    put_val fileout, variables.item(i).desc
    put_val fileout, variables.item(i).labels.Count
    put_line fileout
    If variables.item(i).labels.Count > 0 Then
      For j = 1 To variables.item(i).labels.Count
        put_val fileout, variables.item(i).labels.item(j).varflag
        put_val fileout, variables.item(i).labels.item(j).Name
        put_val fileout, variables.item(i).labels.item(j).Index1
        put_val fileout, variables.item(i).labels.item(j).Index2
        put_val fileout, variables.item(i).labels.item(j).Index3
        put_val fileout, variables.item(i).labels.item(j).Index4
        put_val fileout, variables.item(i).labels.item(j).Index5
        put_val fileout, variables.item(i).labels.item(j).Index6
        put_line fileout
      Next
    End If
  Next
End Sub
Public Sub GetInfoString(ctrl As Control, output As String, header As String)
  If Not ctrl.Text = "" Then
    output = output + header + ctrl.Text + Chr(13) + Chr(10)
  End If
End Sub
Public Sub HidePanels()
  InfPanel.Visible = False
  ContactPanel.Visible = False
  DescPanel.Visible = False
  SysReqPanel.Visible = False
  ModDescPanel.Visible = False
  ReadPanel.Visible = False
  WritePanel.Visible = False
  LimitsPanel.Visible = False
  VarPanel.Visible = False
  VarDesPanel.Visible = False
  GidSelect.Visible = False
  ConDesc.Visible = False
  DesDesc.Visible = False
End Sub
Public Sub LoadModInfo()
  Dim i As Integer
  ModTypeCombo.ListIndex = 0
  For i = 0 To UBound(modtypes) - 1
    If modtypes(i) = ModTypeCombo.Text Then
      Combo1.AddItem modnames(i)
    End If
  Next
  If Combo1.ListCount > 0 Then
  Combo1.ListIndex = 0
  For i = 0 To UBound(modnames) - 1
    If modnames(i) = Combo1.Text Then
      Text3.Text = modicons(i)
      Exit For
    End If
  Next
  End If
End Sub
Public Sub LoadFileInfo()
  Dim i As Integer
  Dim filestring As String
  For i = 0 To UBound(extensions) - 1
    filestring = extensions(i) + " " + qualifiers(i)
    ReadList.AddItem filestring
    WriteList.AddItem filestring
  Next
End Sub
Private Sub Form_Load()
  Dim i As Long
  Dim inipath As String
  Dim modtype As String
  Dim ModName As String
  Dim modid As String
  Dim numtypes As Integer
  Dim tempstring As String
  Dim dummy As String
  Dim check As Boolean
  Dim errmsg As String
  
  FRAMES_INI = App.Path + "\\FramesUI.ini"

  loading = True
  If App.PrevInstance Then
    MsgBox "Des File Editor currently running!", , "Instance notification"
    End
  End If
  check = ReadINI()
  If check = False Then
    errmsg = "Invalid FramesUI.ini found." + Chr(13) + Chr(10)
    errmsg = errmsg + "Please run FRAMES 1.2 before" + Chr(13) + Chr(10)
    errmsg = errmsg + "starting the Des File Editor"
    MsgBox errmsg, vbCritical + vbOKOnly, "ERROR"
    End
  End If
  ModTypeCombo.Clear
  For i = 0 To UBound(modtypes) - 1
    ModTypeCombo.AddItem modtypes(i)
  Next
  For i = UBound(modtypes) - 1 To 1 Step -1
    If ModTypeCombo.list(i) = ModTypeCombo.list(i - 1) Then ModTypeCombo.RemoveItem i
  Next
  LoadModInfo
  LoadFileInfo
  Screen.MousePointer = vbHourglass
  Set nodx = TreeView1.Nodes.Add(, , "root", "Description File Contents", 4, 1)
  Set nodx = TreeView1.Nodes.Add("root", tvwChild, "info", "File Information", ERRORICON, 1)
  Set nodx = TreeView1.Nodes.Add("root", tvwChild, , "Module Description", STARTICON, 1)
  Set nodx = TreeView1.Nodes.Add("root", tvwChild, , "Module References", STARTICON, 1)
  Set nodx = TreeView1.Nodes.Add("root", tvwChild, "sys", "System Requirements", STARTICON, 1)
  Set nodx = TreeView1.Nodes.Add("root", tvwChild, , "Address Information", STARTICON, 1)
  Set nodx = TreeView1.Nodes.Add("root", tvwChild, , "Communication Information", STARTICON, 1)
  Set nodx = TreeView1.Nodes.Add("root", tvwChild, "conn", "Connection", ERRORICON, 1)
  Set nodx = TreeView1.Nodes.Add("conn", tvwChild, "reads", "Reads", ERRORICON, 1)
  Set nodx = TreeView1.Nodes.Add("conn", tvwChild, "writes", "Writes", ERRORICON, 1)
  Set nodx = TreeView1.Nodes.Add("root", tvwChild, "var", "Variables", VISITEDICON, 1)
  nodx.EnsureVisible
  DesVarFlag.ListIndex = 0
  DesIndex1.ListIndex = -1
  newflag = True
  newvar = False
  modified = False
  unitreset = False
  For i = 0 To 10
    maxlist(i) = 0
    minlist(i) = 0
  Next
  InitInfoPanels
  ModDescText.Visible = False
  NewDesc.Visible = True
  NewDesc.Height = 6375
  NewDesc.Left = 120
  NewDesc.Top = 270
  NewDesc.Width = 4455
  LimitText.Visible = False
  NewLimits.Visible = True
  NewLimits.Height = 6375
  NewLimits.Left = 120
  NewLimits.Top = 270
  NewLimits.Width = 4455
  InitUnits UnitCombo
  AddVars(1).Caption = "New Variable"
  AddVars(0).Caption = "New Cue"
  modified = False
  loading = False
  Screen.MousePointer = vbDefault
  savecheck = True
End Sub
Public Function GetTypeString(unitstring As String) As String
  Dim fle As csv
  Dim temp As UnitRec
  Dim fName As String
  Dim Id As String
  Dim cnt As Long
  Dim unitcnt As Long
  Dim tempunit As UnitRec
  fName = App.Path & "\Convert.csv"
  If open_csv(fle, fName, 2) Then
    Id = get_val(fle)
    unitcnt = Val(get_val(fle))
    get_line fle
    If Id = "CVT" Then
      For cnt = 1 To unitcnt
        Read_Units fle, tempunit
        If unitstring = tempunit.f_unit Then
          GetTypeString = tempunit.u_type
          Exit Function
        ElseIf unitstring = tempunit.t_unit Then
          GetTypeString = tempunit.u_type
          Exit Function
        End If
      Next
      GetTypeString = "N/A"
      Exit Function
    Else
      GetTypeString = "BCSV"
    End If
    GetTypeString = "CSVNF"
  End If
  close_csv fle
End Function
Public Sub GetUnitIndex(unitstring As String)
  Dim i As Long
  For i = 0 To UnitCombo.ListCount - 1
    UnitCombo.ListIndex = i
    If UnitCombo.Text = unitstring Then
      Exit Sub
    End If
  Next
End Sub
Public Function GetTypeIndex(typestring As String) As Long
  Select Case typestring
    Case "concentration (dry radioactive)"
      GetTypeIndex = 0
      Exit Function
    Case "concentration (dry)"
      GetTypeIndex = 1
      Exit Function
    Case "concentration (liquid radioactive)"
      GetTypeIndex = 2
      Exit Function
    Case "concentration (liquid)"
      GetTypeIndex = 3
      Exit Function
    Case "distance"
      GetTypeIndex = 4
      Exit Function
    Case "mass"
      GetTypeIndex = 5
      Exit Function
    Case "mass (radioactive)"
      GetTypeIndex = 6
      Exit Function
    Case "miscellaneous"
      GetTypeIndex = 7
      Exit Function
    Case "percentage"
      GetTypeIndex = 9
      Exit Function
    Case "rate"
      GetTypeIndex = 10
      Exit Function
    Case "temperature"
      GetTypeIndex = 11
      Exit Function
    Case "time"
      GetTypeIndex = 12
      Exit Function
    Case "volume (dry radioactive)"
      GetTypeIndex = 13
      Exit Function
    Case "volume (dry)"
      GetTypeIndex = 14
      Exit Function
    Case "volume (liquid):"
      GetTypeIndex = 15
      Exit Function
    Case Else
      GetTypeIndex = 8
  End Select
End Function
Public Function GetType(Index As String) As Long
  Select Case Index
    Case "Air"
      GetType = 0
    Case "Aquifer"
      GetType = 1
    Case "Constituent"
      GetType = 2
    Case "Ecological Dose"
      GetType = 3
    Case "Exposure Pathways"
      GetType = 4
    Case "Health Impacts"
      GetType = 5
    Case "Overland Flow"
      GetType = 6
    Case "Receptor Intake"
      GetType = 7
    Case "Sensitivity"
      GetType = 8
    Case "Source"
      GetType = 9
    Case "Surface Water"
      GetType = 10
    Case "Vadose Zone"
      GetType = 11
    Case "Viewer"
      GetType = 12
  End Select
End Function
Public Function GetStIndex(temp As String)
  Dim tmp As Long
  Select Case temp
    Case "AK"
      tmp = 0
    Case "AL"
      tmp = 1
    Case "AR"
      tmp = 2
    Case "AZ"
      tmp = 3
    Case "CA"
      tmp = 4
    Case "CO"
      tmp = 5
    Case "CT"
      tmp = 6
    Case "DC"
      tmp = 7
    Case "DE"
      tmp = 8
    Case "FL"
      tmp = 9
    Case "GA"
      tmp = 10
    Case "HI"
      tmp = 11
    Case "IA"
      tmp = 12
    Case "ID"
      tmp = 13
    Case "IL"
      tmp = 14
    Case "IN"
      tmp = 15
    Case "KS"
      tmp = 16
    Case "KY"
      tmp = 17
    Case "LA"
      tmp = 18
    Case "MA"
      tmp = 19
    Case "MD"
      tmp = 20
    Case "ME"
      tmp = 21
    Case "MI"
      tmp = 22
    Case "MN"
      tmp = 23
    Case "MO"
      tmp = 24
    Case "MS"
      tmp = 25
    Case "MT"
      tmp = 26
    Case "NC"
      tmp = 27
    Case "ND"
      tmp = 28
    Case "NE"
      tmp = 29
    Case "NH"
      tmp = 30
    Case "NJ"
      tmp = 31
    Case "NM"
      tmp = 32
    Case "NV"
      tmp = 33
    Case "NY"
      tmp = 34
    Case "OH"
      tmp = 35
    Case "OK"
      tmp = 36
    Case "OR"
      tmp = 37
    Case "PA"
      tmp = 38
    Case "RI"
      tmp = 39
    Case "SC"
      tmp = 40
    Case "SD"
      tmp = 41
    Case "TN"
      tmp = 42
    Case "TX"
      tmp = 43
    Case "UT"
      tmp = 44
    Case "VA"
      tmp = 45
    Case "VT"
      tmp = 46
    Case "WA"
      tmp = 47
    Case "WI"
      tmp = 48
    Case "WV"
      tmp = 49
    Case "WY"
      tmp = 50
    Case Else
      tmp = -1
  End Select
  GetStIndex = tmp
End Function
Public Function GetIndex(Name As String, ident As String)
  If Name = "AFF" Then
    GetIndex = 0
  ElseIf Name = "ATO" Then
    GetIndex = 1
  ElseIf Name = "EPF" Then
    GetIndex = 2
  ElseIf Name = "HIF" Then
    GetIndex = 3
  ElseIf Name = "RIF" Then
    GetIndex = 4
  ElseIf Name = "SCF" Then
    GetIndex = 5
  ElseIf Name = "SUF" Then
    GetIndex = 6
  ElseIf Name = "WCF" Then
    If ident = "Flowing Aquifer" Then
      GetIndex = 7
    ElseIf ident = "Flowing Surface Water" Then
      GetIndex = 8
    End If
  ElseIf Name = "WFF" Then
    If ident = "Aquifer Flux" Then
      GetIndex = 9
    ElseIf ident = "Infiltration" Then
      GetIndex = 10
    ElseIf ident = "Runoff" Then
      GetIndex = 11
    End If
  End If
End Function
Public Function DescFormat(tempstring As String) As String
  Dim i As Long
  Dim j As Long
  Dim length As Long
  Dim temp As String
  Dim tmpstring As String
  Dim tempchar As String
  Dim endline As String
  Dim lnfeed As String
  endline = Chr(32) + Chr(13) + Chr(10)
  lnfeed = Chr(13)
  tmpstring = tempstring
  length = Len(tmpstring)
  j = 1
  While j < length
    For i = 0 To 70
      tempchar = Mid(tmpstring, j, 1)
      If tempchar = " " Then
        If i <= 70 And i >= 45 Then
          temp = temp + endline
          j = j + 1
          Exit For
        Else
          temp = temp + tempchar
        End If
      ElseIf tempchar = lnfeed Then
        temp = temp + endline
        j = j + 2
        Exit For
      Else
        temp = temp + tempchar
      End If
      j = j + 1
    Next
  Wend
  DescFormat = temp
End Function
Public Function GetIdent(temp As Long) As String
  Select Case temp
    Case 7
      GetIdent = "Flowing Aquifer"
    Case 8
      GetIdent = "Flowing Surface Water"
    Case 9
      GetIdent = "Aquifer Flux"
    Case 10
      GetIdent = "Infiltration"
    Case 11
      GetIdent = "Runoff"
    Case Else
      GetIdent = ""
  End Select
End Function
Public Function GetName(temp As Long) As String
  Select Case temp
    Case 0
      GetName = "AFF"
    Case 1
      GetName = "ATO"
    Case 2
      GetName = "EPF"
    Case 3
      GetName = "HIF"
    Case 4
      GetName = "RIF"
    Case 5
      GetName = "SCF"
    Case 6
      GetName = "SUF"
    Case 7
      GetName = "WCF"
    Case 8
      GetName = "WCF"
    Case 9
      GetName = "WFF"
    Case 10
      GetName = "WFF"
    Case 11
      GetName = "WFF"
  End Select
End Function
Public Function checknum(tempstring As String) As Boolean
  Dim length As Long
  Dim i As Long
  Dim tempchar As String
  length = Len(tempstring)
  For i = 1 To length
    tempchar = Mid(tempstring, i, 1)
    If Asc(tempchar) > 57 Or Asc(tempchar) < 48 Then
      checknum = False
      Exit Function
    End If
  Next
  checknum = True
End Function
Public Function CheckMinMax(tempstring As String) As Boolean
  Dim i As Long
  Dim deccnt As Long
  Dim tempchar As String
  Dim tempascii As Long
  Dim tempnum As Double
  Dim checknum As Double
  Dim checkstring As String
  deccnt = 0
  On Error GoTo CheckError
  tempnum = Val(tempstring)
  On Error GoTo 0
  checkstring = tempnum
  If Not Len(checkstring) = Len(tempstring) Then
    If Not IsNumeric(tempstring) Then
    If tempstring = "" Then
      CheckMinMax = True
    Else
      CheckMinMax = False
    End If
    Else
    CheckMinMax = True
    End If
  Else
    CheckMinMax = True
  End If
  Exit Function
CheckError:
  CheckMinMax = False
End Function
Public Function CheckMax(max As String, min As String) As Boolean
  Dim minimum As Long
  Dim maximum As Long
  minimum = Val(min)
  maximum = Val(max)
  If minimum > maximum Then
    CheckMax = False
  Else
    CheckMax = True
  End If
End Function
Public Sub VarChange(Index As Long, namecheck As Boolean)
  Dim tempvar As Vars
  Dim priorchild As String
  Dim nextchild As String
  Dim nextcheck As Long
  Dim tempdes As DesVars
  Dim parkey As String
  Dim deskey As String
  Dim tempindex As Long
  Dim i As Long
  If newvar Then
    Exit Sub
  End If
  If loading Then
    Exit Sub
  End If
  If TreeView1.SelectedItem.Key = "var" Then
    Exit Sub
  End If
  If Index = 1 Then
    If VarNameText.Text = "" Or Not gotfoc Then
      Exit Sub
    End If
    Set tempvar = New Vars
    Set tempvar.labels = New Collection
    tempindex = TreeView1.SelectedItem.Index
    tempvar.Name = VarNameText.Text
    tempvar.units = UnitCombo.Text
    tempvar.min = MinText.Text
    tempvar.max = MaxText.Text
    tempvar.desc = DescText.Text
    tempvar.stoch = StochCombo.Text
    For i = 1 To variables.item(TreeView1.SelectedItem.Text).labels.Count
      tempvar.labels.Add variables.item(TreeView1.SelectedItem.Text).labels.item(i), variables.item(TreeView1.SelectedItem.Text).labels.item(i).Name + tempvar.Name
    Next
    If TreeView1.SelectedItem.Index = 12 Then
      variables.Remove TreeView1.SelectedItem.Text
      variables.Add tempvar, tempvar.Name
    ElseIf TreeView1.SelectedItem.LastSibling.Index <> TreeView1.SelectedItem.Index Then
      variables.Remove TreeView1.SelectedItem.Text
      variables.Add tempvar, tempvar.Name, , TreeView1.SelectedItem.Previous.Key
    Else
      variables.Remove TreeView1.SelectedItem.Text
      variables.Add tempvar, tempvar.Name, , TreeView1.SelectedItem.Previous.Key
    End If
  Else
    If DesVarName.Text = "" Or Not gotfoc Then
      Exit Sub
    End If
    Set tempdes = New DesVars
    tempindex = TreeView1.SelectedItem.Index
    tempdes.Name = DesVarName.Text
    tempdes.varflag = DesVarFlag.Text
    tempdes.Index1 = DesIndex1.Text
    tempdes.Index2 = DesIndex2.Text
    tempdes.Index3 = DesIndex3.Text
    tempdes.Index4 = DesIndex4.Text
    tempdes.Index5 = DesIndex5.Text
    tempdes.Index6 = DesIndex6.Text
    parkey = TreeView1.SelectedItem.Parent.Key
    deskey = TreeView1.SelectedItem.Text + parkey
    If TreeView1.SelectedItem.LastSibling.Index <> TreeView1.SelectedItem.Index Then
      nextchild = TreeView1.Nodes.item(deskey).Next.Text
      nextchild = nextchild + TreeView1.Nodes.item(deskey).Parent.Text
      If tempname = "" Then
        tempname = DesVarName.Text
      End If
      variables.item(parkey).labels.Remove tempname + parkey
      variables.item(parkey).labels.Add tempdes, DesVarName.Text + parkey, nextchild
      tempname = DesVarName.Text
    Else
      variables.item(parkey).labels.Remove deskey
      variables.item(parkey).labels.Add tempdes, DesVarName.Text + parkey
    End If
  End If
  If namecheck Then
    RemTreeVars
    PutTreeVars
    For i = 11 To TreeView1.Nodes.Count
      CheckNodeStatus i
    Next
  End If
  If VarPanel.Visible Then
    Set TreeView1.SelectedItem = TreeView1.Nodes.item(tempvar.Name)
    TreeView1.SelectedItem.Image = VISITEDICON
    TreeView1.Nodes.item(tempvar.Name).Expanded = True
  Else
    TreeView1.SelectedItem = TreeView1.Nodes.item(DesVarName.Text + parkey)
    TreeView1.Nodes.item(DesVarName.Text + parkey).Parent.Expanded = True
  End If
End Sub
Private Sub MaxText_LostFocus()
  Dim numcheck As Boolean
  Dim errormsg As String
  numcheck = CheckMinMax(MaxText.Text)
  If Not numcheck Then
    errormsg = "Invalid character entered for maximum value"
    errormsg = errormsg + Chr(13) + Chr(10)
    errormsg = errormsg + "Value entered must be a number"
    MsgBox errormsg, vbOKOnly + vbSystemModal + vbExclamation, "Error"
    MaxText.Text = ""
  End If
  If MinText.Text <> "" Then
    If Val(MaxText.Text) < Val(MinText.Text) Then
      errormsg = "Minimum value must be less than the Maximum value"
      MsgBox errormsg, vbOKOnly + vbExclamation, "Error"
      MaxText.Text = ""
    End If
  End If
End Sub
Private Sub MinText_LostFocus()
  Dim numcheck As Boolean
  Dim errormsg As String
  numcheck = CheckMinMax(MinText.Text)
  If Not numcheck Then
    errormsg = "Invalid character entered for minimum value"
    errormsg = errormsg + Chr(13) + Chr(10)
    errormsg = errormsg + "Value entered must be a number"
    MsgBox errormsg, vbOKOnly + vbSystemModal + vbExclamation, "Error"
    MinText.Text = ""
  End If
  If MaxText.Text <> "" Then
    If Val(MaxText.Text) < Val(MinText.Text) Then
      errormsg = "Minimum value must be less than the Maximum value"
      MsgBox errormsg, vbOKOnly + vbExclamation, "Error"
      MinText.Text = ""
    End If
  End If
End Sub
Private Sub ModVersText_Change()
  modified = True
End Sub
Private Sub NewCue_Click()
  AddVars(0).Caption = "Add"
  newvar = True
  DesVarName.Text = ""
  DeleteCue.Enabled = False
  AddDes_Click
End Sub
Private Sub paste_Click()
  SendKeys "^v"
End Sub
Private Sub StochCombo_Change()
  If Not newvar Then VarChange 1, False
  modified = True
End Sub
Private Sub Text1_Change()
  modified = True
End Sub
Private Sub Text2_Change()
  modified = True
End Sub

Private Sub TreeView1_Collapse(ByVal Node As ComctlLib.Node)
    TreeView1_NodeClick Node
    Set TreeView1.SelectedItem = Node
End Sub
Public Sub ResetCueText()
  CueText(0).Locked = False
  CueText(1).Locked = False
  CueText(0).Text = ""
  CueText(1).Text = ""
  CueText(0).Visible = False
  CueText(1).Visible = False
  CueText(0).Locked = True
  CueText(1).Locked = True
  DltVar.Enabled = False
  NewCue.Enabled = False
  DeleteCue.Enabled = False
  CheckNodeStatus lasttreeindex
  lasttreeindex = TreeView1.SelectedItem.Index
  GenButton.Visible = False
  GenButton.Enabled = False
End Sub
Private Sub TreeView1_NodeClick(ByVal Node As Node)
  Dim i As Long
  Dim desparent As String
  Dim desname As String
  Dim cuestring As String
  Dim tempstring1 As String
  Dim tempstring2 As String
  Dim temptype As String
  Dim typeindex As Long
  Dim msg As String
  HidePanels
  newvar = False
  Select Case Node
    Case "Description File Contents"
      DesDesc.Visible = True
      ResetCueText
      VarDesPanel.Caption = "Variable Cue"
      CheckNodeStatus 11
    Case "Module Description"
      ModDescPanel.Visible = True
      ResetCueText
      VarDesPanel.Caption = "Variable Cue"
      CheckNodeStatus 11
    Case "Module References"
      LimitsPanel.Visible = True
      ResetCueText
      VarDesPanel.Caption = "Variable Cue"
      CheckNodeStatus 11
    Case "System Requirements"
      SysReqPanel.Visible = True
      ResetCueText
      VarDesPanel.Caption = "Variable Cue"
      CheckNodeStatus 11
    Case "File Information"
      InfPanel.Visible = True
      ResetCueText
      VarDesPanel.Caption = "Variable Cue"
      CheckNodeStatus 11
    Case "Reads"
      ReadPanel.Visible = True
      ResetCueText
      VarDesPanel.Caption = "Variable Cue"
      CheckNodeStatus 11
    Case "Writes"
      WritePanel.Visible = True
      ResetCueText
      CueText(1).Visible = False
      CheckNodeStatus 11
    Case "Address Information"
      ContactPanel.Visible = True
      ResetCueText
      VarDesPanel.Caption = "Variable Cue"
      CheckNodeStatus 11
    Case "Communication Information"
      DescPanel.Visible = True
      ResetCueText
      VarDesPanel.Caption = "Variable Cue"
      CheckNodeStatus 11
    Case "Variables"
      HidePanels
      VarPanel.Visible = True
      VarNameText.Text = ""
      AddVars(1).Caption = "Add"
      CueText(0).Visible = False
      ResetCueText
      CueText(1).Visible = True
      VarDesPanel.Caption = "Variable Cue"
      CheckNodeStatus 11
    Case "Connection"
      HidePanels
      ResetCueText
      ConDesc.Visible = True
      VarDesPanel.Caption = "Variable Cue"
      CheckNodeStatus 11
    Case "Description File Contents"
      HidePanels
      ResetCueText
      VarDesPanel.Caption = "Variable Cue"
      CheckNodeStatus 11
    Case Else
      CheckNodeStatus lasttreeindex
      lasttreeindex = TreeView1.SelectedItem.Index
      CueText(1).Visible = True
      CueText(0).Visible = False
      If Node.Parent.Key = "var" Then
        Screen.MousePointer = vbHourglass
        Set lastparen = Node
        DltVar.Enabled = True
        NewCue.Enabled = True
        VarPanel.Visible = True
        GenButton.Enabled = True
        VarNameText.Text = variables.item(TreeView1.SelectedItem.Text).Name
        If variables.item(TreeView1.SelectedItem.Text).stoch = "Not Stochastic" Then
          StochCombo.ListIndex = 0
        ElseIf variables.item(TreeView1.SelectedItem.Text).stoch = "Continuous" Then
          StochCombo.ListIndex = 1
        Else
          StochCombo.ListIndex = 2
        End If
        unitreset = True
        UnitType.ListIndex = FindType(variables.item(TreeView1.SelectedItem.Text).units)
        If UnitType.ListIndex = 11 Then
          UnitCombo.Clear
          UnitCombo.AddItem variables.item(TreeView1.SelectedItem.Text).units
          UnitCombo.ListIndex = 0
        Else
          LoadUnits UnitType.Text, UnitCombo
          SetComboIndex variables.item(TreeView1.SelectedItem.Text).units, UnitCombo
        End If
        unitreset = False
        MinText.Text = variables.item(TreeView1.SelectedItem.Text).min
        MaxText.Text = variables.item(TreeView1.SelectedItem.Text).max
        DescText.Text = variables.item(TreeView1.SelectedItem.Text).desc
        cuestring = variables.item(TreeView1.SelectedItem.Text).desc
        For i = 1 To variables.item(TreeView1.SelectedItem.Text).labels.Count
          cuestring = cuestring + " for "
          cuestring = cuestring + variables.item(TreeView1.SelectedItem.Text).labels.item(i).Name
        Next
        CueText(0).Locked = False
        CueText(1).Locked = False
        CueText(0).Text = cuestring
        CueText(1).Text = cuestring
        CueText(0).Locked = True
        CueText(1).Locked = True
        VarDesPanel.Caption = "Variable Cue"
        AddVars(1).Caption = "New Variable"
        Screen.MousePointer = vbDefault
        CheckNodeStatus 11
      Else
        CueText(0).Visible = True
        CueText(1).Visible = False
        DltVar.Enabled = False
        NewCue.Enabled = False
        DeleteCue.Enabled = True
        VarDesPanel.Visible = True
        VarDesPanel.Caption = Node.Parent.Key + " Cue"
        GenButton.Enabled = False
        desparent = TreeView1.SelectedItem.Parent.Key
        desname = TreeView1.SelectedItem.Text + TreeView1.SelectedItem.Parent.Key
        tempstring1 = variables.item(desparent).Name
        tempstring2 = variables.item(desparent).labels.item(desname).Name
        DesVarName.Text = variables.item(desparent).labels.item(desname).Name
        DesVarFlag.ListIndex = GetDesFlagIndex(variables.item(desparent).labels.item(desname).varflag)
        DesIndex1.ListIndex = GetDesIndex(variables.item(desparent).labels.item(desname).Index1)
        DesIndex2.ListIndex = GetDesIndex(variables.item(desparent).labels.item(desname).Index2)
        DesIndex3.ListIndex = GetDesIndex(variables.item(desparent).labels.item(desname).Index3)
        DesIndex4.ListIndex = GetDesIndex(variables.item(desparent).labels.item(desname).Index4)
        DesIndex5.ListIndex = GetDesIndex(variables.item(desparent).labels.item(desname).Index5)
        DesIndex6.ListIndex = GetDesIndex(variables.item(desparent).labels.item(desname).Index6)
        cuestring = variables.item(TreeView1.SelectedItem.Parent.Text).desc
        For i = 1 To variables.item(TreeView1.SelectedItem.Parent.Text).labels.Count
          cuestring = cuestring + " for "
          cuestring = cuestring + variables.item(TreeView1.SelectedItem.Parent.Text).labels.item(i).Name
        Next
        CueText(0).Locked = False
        CueText(1).Locked = False
        CueText(0).Text = cuestring
        CueText(1).Text = cuestring
        CueText(0).Locked = True
        CueText(1).Locked = True
        CheckNodeStatus 11
      End If
    End Select
End Sub
Public Sub UpdateCueString()
  If TreeView1.SelectedItem.Key = "var" Then Exit Sub
  CheckNodeStatus lasttreeindex
  lasttreeindex = TreeView1.SelectedItem.Index
  CueText(1).Visible = True
  CueText(0).Visible = False
  If TreeView1.SelectedItem.Parent.Key = "var" Then
    Screen.MousePointer = vbHourglass
    Set lastparen = TreeView1.SelectedItem
    DltVar.Enabled = True
    NewCue.Enabled = True
    VarPanel.Visible = True
    GenButton.Enabled = True
    VarNameText.Text = variables.item(TreeView1.SelectedItem.Text).Name
    If variables.item(TreeView1.SelectedItem.Text).stoch = "Not Stochastic" Then
      StochCombo.ListIndex = 0
    ElseIf variables.item(TreeView1.SelectedItem.Text).stoch = "Continuous" Then
      StochCombo.ListIndex = 1
    Else
      StochCombo.ListIndex = 2
    End If
    unitreset = True
    UnitType.ListIndex = FindType(variables.item(TreeView1.SelectedItem.Text).units)
    If UnitType.ListIndex = 11 Then
      UnitCombo.Clear
      UnitCombo.AddItem variables.item(TreeView1.SelectedItem.Text).units
      UnitCombo.ListIndex = 0
    Else
      LoadUnits UnitType.Text, UnitCombo
      SetComboIndex variables.item(TreeView1.SelectedItem.Text).units, UnitCombo
    End If
    unitreset = False
    MinText.Text = variables.item(TreeView1.SelectedItem.Text).min
    MaxText.Text = variables.item(TreeView1.SelectedItem.Text).max
    DescText.Text = variables.item(TreeView1.SelectedItem.Text).desc
    cuestring = variables.item(TreeView1.SelectedItem.Text).desc
    For i = 1 To variables.item(TreeView1.SelectedItem.Text).labels.Count
      cuestring = cuestring + " for "
      cuestring = cuestring + variables.item(TreeView1.SelectedItem.Text).labels.item(i).Name
    Next
    CueText(0).Locked = False
    CueText(1).Locked = False
    CueText(0).Text = cuestring
    CueText(1).Text = cuestring
    CueText(0).Locked = True
    CueText(1).Locked = True
    VarDesPanel.Caption = "Variable Cue"
    AddVars(1).Caption = "New Variable"
    Screen.MousePointer = vbDefault
    CheckNodeStatus 11
  Else
    CueText(0).Visible = True
    CueText(1).Visible = False
    DltVar.Enabled = False
    NewCue.Enabled = False
    DeleteCue.Enabled = True
    VarDesPanel.Visible = True
    VarDesPanel.Caption = TreeView1.SelectedItem.Parent.Key + " Cue"
    GenButton.Enabled = False
    desparent = TreeView1.SelectedItem.Parent.Key
    desname = TreeView1.SelectedItem.Text + TreeView1.SelectedItem.Parent.Key
    tempstring1 = variables.item(desparent).Name
    tempstring2 = variables.item(desparent).labels.item(desname).Name
    DesVarName.Text = variables.item(desparent).labels.item(desname).Name
    DesVarFlag.ListIndex = GetDesFlagIndex(variables.item(desparent).labels.item(desname).varflag)
    DesIndex1.ListIndex = GetDesIndex(variables.item(desparent).labels.item(desname).Index1)
    DesIndex2.ListIndex = GetDesIndex(variables.item(desparent).labels.item(desname).Index2)
    DesIndex3.ListIndex = GetDesIndex(variables.item(desparent).labels.item(desname).Index3)
    DesIndex4.ListIndex = GetDesIndex(variables.item(desparent).labels.item(desname).Index4)
    DesIndex5.ListIndex = GetDesIndex(variables.item(desparent).labels.item(desname).Index5)
    DesIndex6.ListIndex = GetDesIndex(variables.item(desparent).labels.item(desname).Index6)
    cuestring = variables.item(TreeView1.SelectedItem.Parent.Text).desc
    For i = 1 To variables.item(TreeView1.SelectedItem.Parent.Text).labels.Count
      cuestring = cuestring + " for "
      cuestring = cuestring + variables.item(TreeView1.SelectedItem.Parent.Text).labels.item(i).Name
    Next
    CueText(0).Locked = False
    CueText(1).Locked = False
    CueText(0).Text = cuestring
    CueText(1).Text = cuestring
    CueText(0).Locked = True
    CueText(1).Locked = True
    CheckNodeStatus 11
  End If
End Sub
Private Sub TreeView1_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
  If Button = vbRightButton Then
    If TreeView1.SelectedItem.Key = "root" Then
      Exit Sub
    End If
    If TreeView1.SelectedItem.Parent.Key = "var" Then
      PopupMenu VarOpt
    ElseIf TreeView1.SelectedItem.Key = "var" Then
      PopupMenu addvarb
    'ElseIf Not TreeView1.SelectedItem.Parent.Key = "root" Then
    ElseIf (TreeView1.SelectedItem.Parent.Key <> "root") And (TreeView1.SelectedItem.Parent.Key <> "conn") Then
      PopupMenu DescOpt
    End If
  End If
End Sub
Private Sub UnitCombo_Change()
  If Not newvar Then VarChange 1, False
  modified = True
End Sub
Private Sub UnitType_Change()
  If Not newvar Then VarChange 1, False
  modified = True
End Sub
Private Sub UnitType_GotFocus()
  gotfoc = True
End Sub
Private Sub VarNameText_GotFocus()
  tempname = VarNameText.Text
  gotfoc = True
End Sub
Private Sub UnitCombo_GotFocus()
  gotfoc = True
End Sub
Private Sub StochCombo_GotFocus()
  gotfoc = True
End Sub
Private Sub TreeView1_GotFocus()
  gotfoc = False
End Sub
Private Sub MaxText_GotFocus()
  gotfoc = True
End Sub
Private Sub MinText_GotFocus()
  gotfoc = True
End Sub
Private Sub DesIndex3_GotFocus()
  gotfoc = True
End Sub
Private Sub DesIndex4_GotFocus()
  gotfoc = True
End Sub
Private Sub DesIndex5_GotFocus()
  gotfoc = True
End Sub
Private Sub DesIndex6_GotFocus()
  gotfoc = True
End Sub
Private Sub DesVarFlag_GotFocus()
  gotfoc = True
End Sub
Private Sub DesVarName_GotFocus()
  gotfoc = True
  tempname = DesVarName.Text
End Sub
Private Sub DescText_GotFocus()
  gotfoc = True
End Sub
Private Sub DesIndex1_GotFocus()
  gotfoc = True
End Sub
Private Sub DesIndex2_GotFocus()
  gotfoc = True
End Sub
Private Sub VarNameText_LostFocus()
  If Not newvar Then VarChange 1, True
  modified = True
End Sub
Private Sub DesVarName_LostFocus()
  If Not newvar Then VarChange 0, True
  modified = True
End Sub
Private Sub AddDes_Click()
  Dim i As Long
  DesVarFlag.Clear
  DesVarFlag.AddItem "Label"
  DesVarFlag.AddItem "Variable"
  DesVarFlag.AddItem "FSCNAME"
  DesVarFlag.AddItem "CASID"
  For i = 1 To variables.Count
    DesVarFlag.AddItem variables.item(i).Name
  Next
  HidePanels
  VarDesPanel.Visible = True
End Sub
Private Sub addv_Click()
  VarPanel.Visible = True
  VarNameText.Text = ""
  StochCombo.ListIndex = 0
  MinText.Text = "0"
  MaxText.Text = "0"
  DescText.Text = ""
  unitreset = True
  UnitType.ListIndex = -1
  UnitCombo.Clear
  unitreset = False
End Sub
Private Sub AddVars_Click(Index As Integer)
  Dim temp As String
  Dim msg As String
  Dim tempcnt As Long
  Dim tmpvar As New Vars
  Dim tempcue As New DesVars
  newvar = True
  If Index = 1 Then
    If AddVars(1).Caption = "New Variable" Then
      AddVars(1).Caption = "Add"
      VarNameText.Text = ""
      Set TreeView1.SelectedItem = TreeView1.Nodes("var")
    Else
      tmpvar.Name = VarNameText.Text
      If tmpvar.Name = "" Then
        msg = "Variable name required"
        MsgBox msg, vbExclamation + vbOKOnly + vbSystemModal, "ERROR"
        Exit Sub
      End If
      If UnitCombo.Text = "" Then
        msg = "Units required"
        MsgBox msg, vbExclamation + vbOKOnly + vbSystemModal, "ERROR"
        Exit Sub
      End If
      On Error GoTo TreeErrorHandler
      Set nodx = TreeView1.Nodes.Add("var", tvwChild, VarNameText.Text, VarNameText.Text, STARTICON, 1)
      On Error GoTo 0
      Set TreeView1.SelectedItem = TreeView1.Nodes(VarNameText.Text)
      If Not (VarNameText.Text = "" Or UnitType.Text = "" Or UnitCombo.Text = "" Or DescText.Text = "") Then
        TreeView1.SelectedItem.Image = VISITEDICON
      End If
      Set lastparen = nodx
      tmpvar.stoch = StochCombo.Text
      tmpvar.units = UnitCombo.Text
      If IsNumeric(MinText.Text) Then
        tmpvar.min = MinText.Text
      Else
        tmpvar.min = ""
      End If
      If IsNumeric(MaxText.Text) Then
        tmpvar.max = MaxText.Text
      Else
        tmpvar.max = ""
      End If
      tmpvar.desc = DescText.Text
      variables.Add tmpvar, tmpvar.Name
      VarPanel.Visible = True
      AddVars(1).Caption = "New Variable"
      NewCue.Enabled = True
      DltVar.Enabled = True
    End If
  Else
    If AddVars(0).Caption = "New Cue" Then
      HidePanels
      VarDesPanel.Visible = True
      AddVars(0).Caption = "Add"
      Set TreeView1.SelectedItem = TreeView1.Nodes(TreeView1.SelectedItem.Parent.Key)
      DesVarName.Text = ""
    Else
      tempcue.Name = DesVarName.Text
      If tempcue.Name = "" Then
        msg = "Cue name required"
        MsgBox msg, vbExclamation + vbOKOnly + vbSystemModal, "ERROR"
        Exit Sub
      End If
      If DesVarFlag.Text = "" Then
        msg = "Cue type required"
        MsgBox msg, vbExclamation + vbOKOnly + vbSystemModal, "ERROR"
        Exit Sub
      End If
      On Error GoTo TreeErrorHandler
      Set nodx = TreeView1.Nodes.Add(TreeView1.SelectedItem.Index, tvwChild, tempcue.Name + TreeView1.SelectedItem.Key, DesVarName.Text, STARTICON, 1)
      On Error GoTo 0
      tempcue.varflag = DesVarFlag.Text
      tempcue.Index1 = DesIndex1.Text
      tempcue.Index2 = DesIndex2.Text
      tempcue.Index3 = DesIndex3.Text
      tempcue.Index4 = DesIndex4.Text
      tempcue.Index5 = DesIndex5.Text
      tempcue.Index6 = DesIndex6.Text
      variables.item(TreeView1.SelectedItem.Key).labels.Add tempcue, tempcue.Name + TreeView1.SelectedItem.Key
      AddVars(0).Caption = "New Cue"
      Set TreeView1.SelectedItem = TreeView1.Nodes(tempcue.Name + TreeView1.SelectedItem.Key)
      If Not (DesVarFlag.Text = "" Or DesVarName.Text = "") Then
        TreeView1.SelectedItem.Image = VISITEDICON
      End If
    End If
  End If
  modified = True
  Exit Sub
TreeErrorHandler:
  If Index = 1 Then
    msg = "Invalid variable name entered" + Chr(13) + Chr(10)
    msg = msg + "A variable using this name already exists"
    MsgBox msg, vbExclamation + vbOKOnly + vbSystemModal, "ERROR"
    Exit Sub
  Else
    msg = "Invalid variable cue name entered" + Chr(13) + Chr(10)
    msg = msg + "A cue using this name already exists for the variable"
    MsgBox msg, vbExclamation + vbOKOnly + vbSystemModal, "ERROR"
    Exit Sub
  End If
  Resume Next
End Sub
Private Sub BrowseExe_Click(Index As Integer)
  Dim opencheck As Long
  If Index = 2 Then
    CommonDialog3.Filter = "Icon (*.ico)|*.ico"
  Else
    CommonDialog3.Filter = "Executable (*.exe)|*.exe"
  End If
  CommonDialog3.ShowOpen
  If Index = 0 Then
    UIText.Text = CommonDialog3.FileName
    CommonDialog3.FileName = ""
  ElseIf Index = 1 Then
    ModExeText.Text = CommonDialog3.FileName
    CommonDialog3.FileName = ""
  Else
    If CommonDialog3.FileName <> "" Then
      Text3.Text = CommonDialog3.FileName
      CommonDialog3.FileName = ""
    End If
  End If
End Sub
Private Sub DelDesc_Click()
  variables.item(TreeView1.SelectedItem.Parent.Key).labels.Remove TreeView1.SelectedItem.Text + TreeView1.SelectedItem.Parent.Key
  TreeView1.Nodes.Remove TreeView1.SelectedItem.Index
  DesVarName.Text = ""
  DesVarFlag.ListIndex = 0
  DesIndex1.ListIndex = -1
  DesIndex2.ListIndex = -1
  DesIndex3.ListIndex = -1
  DesIndex4.ListIndex = -1
  DesIndex5.ListIndex = -1
  DesIndex6.ListIndex = -1
  VarDesPanel.Visible = False
  VarPanel.Visible = False
End Sub
Private Sub DeleteVar_Click()
  Dim tempindex As Long
  tempindex = TreeView1.SelectedItem.Index
  variables.Remove TreeView1.SelectedItem.Text
  TreeView1.Nodes.Remove TreeView1.SelectedItem.Index
  If tempindex <= TreeView1.Nodes.Count Then
    Set TreeView1.SelectedItem = TreeView1.Nodes(tempindex)
  Else
    Set TreeView1.SelectedItem = TreeView1.Nodes(tempindex - 1)
  End If
  VarNameText.Text = ""
  StochCombo.ListIndex = 0
  unitreset = True
  UnitType.ListIndex = -1
  UnitCombo.Clear
  unitreset = False
  MinText.Text = ""
  MaxText.Text = ""
  DescText.Text = ""
  VarPanel.Visible = False
  VarDesPanel.Visible = False
  If TreeView1.Nodes.item("var").Children = 0 Then
    VarPanel.Visible = False
  End If
End Sub
Private Sub DesIndex1_Click()
  If Not newvar Then VarChange 0, False
  modified = True
End Sub
Private Sub DesIndex2_Click()
  If Not newvar Then VarChange 0, False
  modified = True
End Sub
Private Sub DesIndex3_Click()
  If Not newvar Then VarChange 0, False
  modified = True
End Sub
Private Sub DesIndex4_Click()
  If Not newvar Then VarChange 0, False
  modified = True
End Sub
Private Sub DesIndex5_Click()
  If Not newvar Then VarChange 0, False
  modified = True
End Sub
Private Sub DesIndex6_Click()
  If Not newvar Then VarChange 0, False
  modified = True
End Sub
Private Sub DesVarFlag_Click()
  If Not newvar Then VarChange 0, False
  modified = True
End Sub
Private Sub GenButton_Click()
  Dim i As Long
  Dim crapstring As String
  Screen.MousePointer = vbHourglass
  crapstring = CommonDialog2.FileName
  GetVariables crapstring
  SetDesVarFlag
  modified = True
  For i = 11 To TreeView1.Nodes.Count
    TreeView1.Nodes.item(i).Image = STARTICON
  Next
    For i = 2 To TreeView1.Nodes.Count
    CheckNodeStatus i
  Next
  Screen.MousePointer = vbDefault
End Sub
Private Sub GenVar_Click(Index As Integer)
  Dim i As Long
  Dim gidcheck As Boolean
  sections.cnt = 0
  SectName.Clear
  CommonDialog2.CancelError = True
  On Error GoTo CancelGenerate
  CommonDialog2.ShowOpen
  On Error GoTo 0
  HidePanels
  GidSelect.Visible = True
  GenButton.Visible = True
  GenButton.Enabled = True
  GenButton.Enabled = True
  Screen.MousePointer = vbHourglass
  gidcheck = ReadSectNames(CommonDialog2.FileName)
  If gidcheck Then RemTreeVars
  Screen.MousePointer = vbDefault
  Exit Sub
CancelGenerate:
End Sub
Private Sub ModTypeCombo_Click()
  Dim i As Integer
  If loading Then Exit Sub
  modified = True
  Combo1.Clear
  On Error GoTo NAMEERROR
  For i = 0 To UBound(modtypes) - 1
    If modtypes(i) = ModTypeCombo.Text Then
      Combo1.AddItem modnames(i)
    End If
  Next
  Combo1.ListIndex = 0
  For i = 0 To UBound(modnames) - 1
    If modnames(i) = Combo1.Text Then
      Text3.Text = modicons(i)
    End If
  Next
  Exit Sub
NAMEERROR:
End Sub
Private Sub prntfile_Click()
  Dim i As Long
  Dim j As Long
  Dim minmaxcnt As Long
  Dim boxcheck As Boolean
  Dim oldsize
  Dim oldy As Long
  Dim cuehead As String
  Dim oldprinter As Printer
  Dim newprinter As Printer
  Dim cntrl
  Dim tempstring As String
  Dim output As String
  Dim linecnt As Long
  Dim lines() As String
  PrintDialog.CancelError = True
  PrintDialog.PrinterDefault = True
  Flags = PrintDialog.Flags & cdlPDReturnDC
  PrintDialog.Flags = cdlPDDisablePrintToFile + cdlPDNoPageNums + cdlPDNoSelection + cdlPDUseDevModeCopies
  On Error GoTo PrintCancel
  PrintDialog.ShowPrinter
  On Error GoTo 0
  Set oldprinter = Printer
  For Each newprinter In Printers
    On Error GoTo PrintErrorHandler
    If newprinter.hDC = PrintDialog.hDC Then
      Set Printer = newprinter
    End If
    On Error GoTo 0
  Next
  oldsize = Printer.Font.Size
  Printer.CurrentY = 2 * (720 / Printer.TwipsPerPixelY)
  Printer.Font.Size = 12
  Printer.FontBold = True
  Printer.CurrentX = 500
  Printer.Print "DESCRIPTION FILE: " + CommonDialog1.FileName
  Printer.FontBold = False
  Printer.Font.Size = 11
  Printer.Print ""
  PrintField Printer, "Module Type:", ModTypeCombo.Text
  tempstring = UIText.Text + " " + Text1.Text
  PrintField Printer, "User Interface Executable:", tempstring
  PrintField Printer, "Module Name:", ModNmText.Text
  tempstring = ModExeText.Text + " " + Text2.Text
  PrintField Printer, "Module Executable:", tempstring
  PrintField Printer, "Module Version:", ModVersText.Text
  Printer.Print ""
  Printer.Print ""
  Printer.FontBold = True
  Printer.Font.Size = 12
  Printer.CurrentX = 500
  Printer.Print "MODULE DESCRIPTION"
  Printer.FontBold = False
  Printer.Font.Size = 11
  If ModDescText.Text = "" Then
    Printer.CurrentX = 1000
    Printer.Print "NONE"
  Else
    GetDescLines lines, linecnt, ModDescText.Text
    For i = 0 To linecnt - 1
      Printer.CurrentX = 1000
      Printer.Print lines(i)
    Next
  End If
  Printer.Print ""
  Printer.FontBold = True
  Printer.Font.Size = 12
  Printer.CurrentX = 500
  Printer.Print "MODULE REFERENCES"
  Printer.FontBold = False
  Printer.Font.Size = 11
  If LimitText.Text = "" Then
    Printer.CurrentX = 1000
    Printer.Print "NONE"
  Else
    GetDescLines lines, linecnt, LimitText.Text
    For i = 0 To linecnt - 1
      Printer.CurrentX = 1000
      Printer.Print lines(i)
    Next
  End If
  Printer.Print ""
  Printer.FontBold = True
  Printer.Font.Size = 12
  Printer.CurrentX = 500
  Printer.Print "SYSTEM REQUIREMENTS"
  Printer.FontBold = False
  Printer.Font.Size = 11
  PrintField Printer, "Operating System:", OSText.Text
  PrintField Printer, "Processor:", ProcText.Text
  PrintField Printer, "RAM Memory:", RAMText.Text
  PrintField Printer, "Disk Space:", DiskText.Text
  Printer.Print ""
  Printer.FontBold = True
  Printer.Font.Size = 12
  Printer.CurrentX = 500
  Printer.Print "POINT OF CONTACT INFORMATION"
  Printer.FontBold = False
  Printer.Font.Size = 11
  PrintField Printer, "Company Name:", CompText.Text
  PrintField Printer, "Contact Name:", ModlrText.Text
  PrintField Printer, "Mailing Address:", AddrssText.Text
  PrintField Printer, "City:", CityText.Text
  PrintField Printer, "State:", StateCombo.Text
  PrintField Printer, "Zip Code:", ZipText.Text
  PrintField Printer, "Country:", CntryText.Text
  PrintField Printer, "Telephone Number:", PhoneEdit.Text
  PrintField Printer, "Fax Number:", FaxEdit.Text
  PrintField Printer, "Email Address:", EmailText.Text
  PrintField Printer, "URL Address:", URLText.Text
  Printer.Print ""
  Printer.FontBold = True
  Printer.Font.Size = 12
  Printer.CurrentX = 500
  Printer.Print "files READ BY THIS MODULE"
  Printer.FontBold = False
  Printer.Font.Size = 11
  minmaxcnt = 0
  For i = 0 To 11
    If Not minlist(i) = 0 Then
      minmaxcnt = minmaxcnt + 1
    End If
    If Not maxlist(i) = 0 Then
      minmaxcnt = minmaxcnt + 1
    End If
  Next
  readcnt = ReadList.SelCount
  If readcnt = 0 Or minmaxcnt = 0 Then
    Printer.CurrentX = 1000
    Printer.Print "NONE"
  Else
    Printer.CurrentX = 1000
    Printer.Print "Filename,Qualifier, Minimum, Maximum"
    For i = 0 To ReadList.ListCount - 1
      If ReadList.Selected(i) Then
        output = """" + reads(i).Name
        If Not reads(i).ident = "" Then
          output = output + "," + reads(i).ident + """"
        Else
          output = output + "," + """" + """"
        End If
        output = output + ","
        output = output + Str(minlist(i))
        output = output + ","
        output = output + Str(maxlist(i))
        Printer.CurrentX = 1000
        Printer.Print output
      End If
    Next
  End If
  Printer.Print ""
  Printer.FontBold = True
  Printer.Font.Size = 12
  Printer.CurrentX = 500
  Printer.Print "files WRITTEN BY THIS MODULE"
  Printer.FontBold = False
  Printer.Font.Size = 11
  Printer.CurrentX = 1000
  Printer.Print "Filename,Qualifier"
  If WriteList.SelCount = 0 Then
    Printer.CurrentX = 1000
    Printer.Print "NONE"
  Else
    For i = 0 To WriteList.ListCount - 1
      If WriteList.Selected(i) Then
        output = """" + GetName(i) + """"
        If GetIdent(i) = "" Then
          output = output + "," + """" + """" + """"
        Else
          output = output + "," + GetIdent(i) + """"
        End If
        Printer.CurrentX = 1000
        Printer.Print output
      End If
    Next
  End If
  Printer.Print ""
  Printer.FontBold = True
  Printer.CurrentX = 500
  Printer.Print "VARIABLES USED BY THIS MODULE"
  Printer.FontBold = False
  If variables.Count = 0 Then
    Printer.CurrentX = 1000
    Printer.Print "NONE"
    Printer.CurrentX = 500
  Else
    For i = 1 To variables.Count
      output = """" + variables.item(i).Name + """" + ","
      output = output + """" + variables.item(i).stoch + """" + ","
      output = output + """" + variables.item(i).units + """" + ","
      If Not variables.item(i).min = "" Then
        output = output + """" + "Min" + """" + ","
        output = output + variables.item(i).min + ","
        If Not variables.item(i).max = "" Then
          output = output + """" + "Max" + """" + ","
          output = output + variables.item(i).max + ","
        Else
          output = output + ","
        End If
      ElseIf Not variables.item(i).max = "" Then
        output = output + ","
        output = output + """" + "Max" + """" + ","
        output = output + variables.item(i).max + ","
      Else
        output = output + ",,"
      End If
      output = output + """" + variables.item(i).desc + """" + ","
      output = output + Str(variables.item(i).labels.Count)
      Printer.CurrentX = 1000
      Printer.Print output
      If variables.item(i).labels.Count > 0 Then
        For j = 1 To variables.item(i).labels.Count
          output = """" + variables.item(i).labels.item(j).varflag + """" + ","
          output = output + """" + variables.item(i).labels.item(j).Name + """" + ","
          output = output + """" + variables.item(i).labels.item(j).Index1 + """" + ","
          output = output + """" + variables.item(i).labels.item(j).Index2 + """" + ","
          output = output + """" + variables.item(i).labels.item(j).Index3 + """" + ","
          output = output + """" + variables.item(i).labels.item(j).Index4 + """" + ","
          output = output + """" + variables.item(i).labels.item(j).Index5 + """" + ","
          output = output + """" + variables.item(i).labels.item(j).Index6 + """"
          cuehead = "Cue #" + Str(j) + ": "
          oldy = Printer.CurrentY
          Printer.CurrentX = 2000
          Printer.FontBold = True
          Printer.Print cuehead
          Printer.CurrentY = oldy
          Printer.FontBold = False
          Printer.CurrentX = 3500
          Printer.Print output
        Next
      End If
    Next
  End If
  Printer.EndDoc
  Exit Sub
PrintErrorHandler:
  MsgBox "Printer Unable to complete print job", vbExclamation + vbOKOnly + vbSystemModal, "ERROR"
  Exit Sub
PrintCancel:
End Sub
Private Sub ReadList_Click()
  Dim temptype As String
  Dim tempqual As String
  On Error GoTo INDEXERROR
  If Not opencheck Then
    If Not ReadList.Selected(ReadList.ListIndex) Then
      reads(ReadList.ListIndex).Name = ""
      reads(ReadList.ListIndex).ident = ""
      reads(ReadList.ListIndex).min = 0
      reads(ReadList.ListIndex).max = 0
      GetFileType ReadList.Text, temptype, tempqual
      Label16.Caption = temptype + " , " + tempqual
      maxlist(ReadList.ListIndex) = 0
      minlist(ReadList.ListIndex) = 0
    Else
      If reads(ReadList.ListIndex).Name = "" Then
        GetFileType ReadList.Text, temptype, tempqual
        reads(ReadList.ListIndex).Name = temptype
        reads(ReadList.ListIndex).ident = tempqual
        reads(ReadList.ListIndex).min = 0
        reads(ReadList.ListIndex).max = 0
      End If
'      Label16.Caption = ReadList.Text
      GetFileType ReadList.Text, temptype, tempqual
      Label16.Caption = temptype + " , " + tempqual
      ReadMx.Text = maxlist(ReadList.ListIndex)
      ReadMn.Text = minlist(ReadList.ListIndex)
    End If
    ReadMx.Text = maxlist(ReadList.ListIndex)
    ReadMn.Text = minlist(ReadList.ListIndex)
  End If
  modified = True
INDEXERROR:
End Sub
Private Sub saveas_Click()
  CommonDialog1.CancelError = True
  On Error GoTo CancelSaveAs
  CommonDialog1.ShowSave
  On Error GoTo 0
  If CommonDialog1.FileName = "" Then
    MsgBox "Unable to save" + Chr(13) + Chr(10) + "A filename is required", vbExclamation + vbOKOnly + vbSystemModal, "Error"
  Else
    save_Click
  End If
CancelSaveAs:
End Sub
Private Sub StochCombo_Click()
  If Not newvar Then VarChange 1, False
  modified = True
End Sub
Private Sub exit_Click()
  Dim errormsg As String
  Dim reply As Long
  Unload Form1
End Sub
Public Sub ResetForms()
  Dim i As Long
  ModNmText.Text = ""
  ModlrText.Text = ""
  ModVersText.Text = ""
  ModTypeCombo.ListIndex = -1
  UIText.Text = ""
  Text1.Text = ""
  ModExeText.Text = ""
  Text2.Text = ""
  ModDescText.Text = ""
  LimitText.Text = ""
  ProcText.Text = ""
  RAMText.Text = ""
  DiskText.Text = ""
  CompText.Text = ""
  AddrssText.Text = ""
  CityText.Text = ""
  StateCombo.ListIndex = -1
  ZipText.Text = ""
  CntryText.Text = ""
  PhoneEdit.Mask = "(###) ###-####"
  FaxEdit.Mask = "(###) ###-####"
  EmailText.Text = ""
  URLText.Text = ""
  For i = 0 To 11
    maxlist(i) = 0
    minlist(i) = 0
  Next
  For i = 0 To 11
    ReadList.Selected(i) = False
    WriteList.Selected(i) = False
  Next
  VarNameText.Text = ""
  MinText.Text = ""
  MaxText.Text = ""
  DescText.Text = ""
  StochCombo.ListIndex = -1
  CueText(0).Locked = False
  CueText(1).Locked = False
  CueText(0).Text = ""
  CueText(1).Text = ""
  CueText(0).Locked = True
  CueText(1).Locked = True
  DesVarFlag.ListIndex = -1
  DesVarName.Text = ""
  DesIndex1.ListIndex = -1
  DesIndex2.ListIndex = -1
  DesIndex3.ListIndex = -1
  DesIndex4.ListIndex = -1
  DesIndex5.ListIndex = -1
  DesIndex6.ListIndex = -1
  SectName.Clear
  For i = 1 To variables.Count
    variables.Remove (1)
  Next
  RemTreeVars
End Sub
Private Sub newfile_Click()
  Dim i As Long
  newflag = True
  CommonDialog1.FileName = ""
  Set TreeView1.SelectedItem = TreeView1.Nodes.item(2)
  TreeView1.SelectedItem.Image = VISITEDICON
  For i = 3 To TreeView1.Nodes.Count
    TreeView1.Nodes.item(i).Image = STARTICON
  Next
  ResetForms
  TreeView1.Enabled = True
  HidePanels
  InfPanel.Visible = True
  ModDescText.Visible = False
  NewDesc.Visible = True
  NewDesc.Height = 6375
  NewDesc.Left = 120
  NewDesc.Top = 270
  NewDesc.Width = 4455
  LimitText.Visible = False
  NewLimits.Visible = True
  NewLimits.Height = 6375
  NewLimits.Left = 120
  NewLimits.Top = 270
  NewLimits.Width = 4455
  For i = 2 To TreeView1.Nodes.Count
    CheckNodeStatus i
  Next
End Sub
Private Sub openfile_Click()
  Dim filein As csv
  Dim temp As String
  Dim desccheck As String
  Dim i As Long
  TreeView1.Enabled = True
  Set TreeView1.SelectedItem = TreeView1.Nodes.item(2)
  TreeView1.SelectedItem.Image = VISITEDICON
  For i = 3 To TreeView1.Nodes.Count
    TreeView1.Nodes.item(i).Image = STARTICON
  Next
  HidePanels
  InfPanel.Visible = True
  newflag = False
  CommonDialog1.CancelError = True
  On Error GoTo OpenCancel
  CommonDialog1.ShowOpen
  On Error GoTo 0
  Screen.MousePointer = vbHourglass
  opencheck = True
  RemVars
  RemTreeVars
  HidePanels
  InfPanel.Visible = True
  NewDesc.Visible = False
  ModDescText.Visible = True
  NewLimits.Visible = False
  LimitText.Visible = True
  For i = 0 To ReadList.ListCount - 1
    ReadList.Selected(i) = False
    WriteList.Selected(i) = False
    minlist(i) = 0
    maxlist(i) = 0
  Next
  PhoneEdit.Mask = ""
  PhoneEdit.Text = ""
  PhoneEdit.Mask = "(###) ###-####"
  FaxEdit.Mask = ""
  FaxEdit.Text = ""
  FaxEdit.Mask = "(###) ###-####"
  If CommonDialog1.FileName = "" Then
    Screen.MousePointer = vbDefault
    Exit Sub
  End If
  If open_csv(filein, CommonDialog1.FileName, 2) = False Then
    errormsg = "Unable to open file " + CommonDialog1.FileName
    MsgBox errormsg, vbOKOnly + vbSystemModal + vbExclamation, "Error"
    opencheck = False
    Screen.MousePointer = vbDefault
    Exit Sub
  End If
  temp = get_val(filein)
  If Not temp = "mf" Then
    errormsg = "Unable to read file" + Chr(13) + Chr(10)
    errormsg = errormsg + "non-compatible file type"
    MsgBox errormsg, vbOKOnly + vbSystemModal + vbExclamation, "Error"
    opencheck = False
    CommonDialog1.FileName = ""
    Screen.MousePointer = vbDefault
    Exit Sub
  End If
  Form1.Caption = "Description File Editor " + CommonDialog1.FileName
  temp = get_val(filein)
  GetFileInfo filein
  get_line filein
  temp = get_val(filein)
  If temp = "" Then
    close_csv filein
    Exit Sub
  End If
  If Asc(temp) < 48 Or Asc(temp) > 57 Then
    close_csv filein
    desccheck = GetDescInfo(filein)
    If open_csv(filein, CommonDialog1.FileName, 2) = False Then
      errormsg = "Unable to open file " + CommonDialog1.FileName
      MsgBox errormsg, vbOKOnly + vbSystemModal + vbExclamation, "Error"
      Screen.MousePointer = vbDefault
      opencheck = False
      Exit Sub
    End If
    temp = get_val(filein)
    temp = get_val(filein)
    get_line filein
    temp = get_val(filein)
    temp = get_val(filein)
    temp = get_val(filein)
    temp = get_val(filein)
    get_line filein
    temp = get_val(filein)
  End If
  GetReads filein, get_val(filein)
  GetWrites filein
  ReadList.ListIndex = -1
  WriteList.ListIndex = -1
  ReadVars filein
  close_csv filein
  opencheck = False
  modified = False
  For i = 2 To TreeView1.Nodes.Count
    CheckNodeStatus i
  Next
  Screen.MousePointer = vbDefault
OpenCancel:
End Sub

Public Sub WriteConnections(tempstring As String)
  Dim i As Long
  Dim tempmin As String
  Dim tempmax As String
  tempstring = tempstring + CONNECTSTRING + Chr(13) + Chr(10)
  tempstring = tempstring + INPUTCONNSTRING + Chr(13) + Chr(10)
  For i = 0 To ReadList.ListCount - 1
  If ReadList.Selected(i) = True And Not (minlist(i) = 0 And maxlist(i) = 0) Then
    tempmin = minlist(i)
    tempmax = maxlist(i)
    tempstring = tempstring + "   "
    tempstring = tempstring + tempmin + " to " + tempmax
    tempstring = tempstring + " " + ReadList.list(i) '+ " "
    tempstring = tempstring + " required as input" + Chr(13) + Chr(10)
  End If
  Next
  tempstring = tempstring + Chr(13) + Chr(10)
  tempstring = tempstring + OUTPUTCONNSTRING + Chr(13) + Chr(10)
  For i = 0 To WriteList.ListCount - 1
  If WriteList.Selected(i) = True Then
    tempstring = tempstring + "   "
    tempstring = tempstring + WriteList.list(i) + Chr(13) + Chr(10)
  End If
  Next
End Sub
Public Sub GetFileType(filestring As String, typestring As String, qualifier As String)
  Dim i As Integer
  Dim pos As Integer
  Dim tempstring As String
  pos = InStr(1, filestring, " ")
  tempstring = ""
  For i = 1 To pos - 1
    tempstring = tempstring + Mid(filestring, i, 1)
  Next
  typestring = tempstring
  tempstring = ""
  For i = pos + 1 To Len(filestring)
    tempstring = tempstring + Mid(filestring, i, 1)
  Next
  qualifier = tempstring
End Sub
Private Sub save_Click()
  Dim errormsg As String
  Dim filesavecheck As String
  Dim tempreads() As filetype
  Dim cancelcheck
  Dim response
  Dim emailcheck As Boolean
  Dim output As String
  Dim outstring As String
  Dim tempstring As String
  Dim boxcheck As String
  Dim writecnt As Long
  Dim readcnt As Long
  Dim readarray() As Long
  Dim printcheck As Boolean
  Dim modinfostring As String
  Dim temptype As String
  Dim tempqual As String
  Dim i As Long
  Dim j As Long
  Dim filecheck As Long
  sverror = False
  savecheck = True
  printcheck = False
  writecnt = 0
  readcnt = 0
  For i = 0 To 11
    If maxlist(i) < minlist(i) Then
      HidePanels
      ReadList.ListIndex = i
      ReadPanel.Visible = True
      MsgBox "Maximum value can not be less than the minimum value", vbOKOnly + vbSystemModal + vbExclamation, "Error"
      sverror = True
      savecheck = False
      Exit Sub
    End If
  Next
  If ModNmText.Text = "" Then
    Set TreeView1.SelectedItem = TreeView1.Nodes.item(2)
    TreeView1_NodeClick TreeView1.SelectedItem
    ModNmText.SetFocus
    MsgBox "Module Name required", vbOKOnly + vbSystemModal + vbExclamation, "Error"
    sverror = True
    savecheck = False
    Exit Sub
  End If
  If ModTypeCombo.Text = "" Then
    Set TreeView1.SelectedItem = TreeView1.Nodes.item(2)
    TreeView1_NodeClick TreeView1.SelectedItem
    ModTypeCombo.SetFocus
    MsgBox "Module Type required", vbOKOnly + vbSystemModal + vbExclamation, "Error"
    sverror = True
    savecheck = False
    Exit Sub
  End If
  If Combo1.Text = "" Then
    Set TreeView1.SelectedItem = TreeView1.Nodes.item(2)
    TreeView1_NodeClick TreeView1.SelectedItem
    Combo1.SetFocus
    MsgBox "Module Name required", vbOKOnly + vbSystemModal + vbExclamation, "Error"
    sverror = True
    savecheck = False
    Exit Sub
  End If
'  If Text3.Text = "" Then
'    Set TreeView1.SelectedItem = TreeView1.Nodes.Item(2)
'    TreeView1_NodeClick TreeView1.SelectedItem
'    Text3.SetFocus
'    MsgBox "Module Icon required", vbOKOnly + vbSystemModal + vbExclamation, "Error"
'    sverror = True
'    savecheck = False
'    Exit Sub
'  End If
  If ModVersText.Text = "" Then
    Set TreeView1.SelectedItem = TreeView1.Nodes.item(2)
    TreeView1_NodeClick TreeView1.SelectedItem
    ModVersText.SetFocus
    MsgBox "Module Version Required", vbOKOnly + vbOKOnly + vbExclamation + vbSystemModal, "Error"
    sverror = True
    savecheck = False
    Exit Sub
  End If
  If ModExeText.Text = "" And UIText.Text = "" Then
    Set TreeView1.SelectedItem = TreeView1.Nodes.item(2)
    TreeView1_NodeClick TreeView1.SelectedItem
    UIText.SetFocus
    errormsg = "You must enter at least a Module executable filename" + Chr(13) + Chr(10)
    errormsg = errormsg + "or a User Interface Executable filename "
    MsgBox errormsg, vbOKOnly + vbSystemModal + vbExclamation, "Error"
    sverror = True
    savecheck = False
    Exit Sub
  End If
  If ModExeText.Text = "" Then
    Set TreeView1.SelectedItem = TreeView1.Nodes.item(2)
    TreeView1_NodeClick TreeView1.SelectedItem
    ModExeText.SetFocus
    errormsg = "You have not entered a filename for the Module Executable" + Chr(13) + Chr(10)
    errormsg = errormsg + "Do you still want to continue with the save?"
    cancelcheck = MsgBox(errormsg, vbYesNo + vbSystemModal + vbInformation, "Warning")
    If cancelcheck = vbNo Then
      savecheck = False
      Exit Sub
    End If
  End If
  If UIText.Text = "" Then
    Set TreeView1.SelectedItem = TreeView1.Nodes.item(2)
    TreeView1_NodeClick TreeView1.SelectedItem
    UIText.SetFocus
    errormsg = "You have not entered a filename for the User Interface Executable" + Chr(13) + Chr(10)
    errormsg = errormsg + "Do you still want to continue with the save?"
    cancelcheck = MsgBox(errormsg, vbYesNo + vbSystemModal + vbInformation, "Warning")
    If cancelcheck = vbNo Then
      savecheck = False
      Exit Sub
    End If
  End If
  If Not EmailText.Text = "" Then
    emailcheck = checkemail(EmailText.Text)
    If Not emailcheck Then
        Set TreeView1.SelectedItem = TreeView1.Nodes.item(7)
        TreeView1_NodeClick TreeView1.SelectedItem
        EmailText.SetFocus
        errormsg = "The email address entered is not a valid address"
        MsgBox errormsg, vbOKOnly + vbSystemModal + vbExclamation, "Error"
        savecheck = False
        Exit Sub
    End If
  End If
  If ReadList.SelCount = 0 And WriteList.SelCount = 0 Then
    Set TreeView1.SelectedItem = TreeView1.Nodes.item(9)
    TreeView1_NodeClick TreeView1.SelectedItem
    ReadList.SetFocus
    errormsg = "You must select the types of files that the module will"
    errormsg = errormsg + Chr(13) + Chr(10) + "read or write"
    MsgBox errormsg, vbOKOnly + vbSystemModal + vbExclamation, "Error"
    sverror = True
    savecheck = False
    Exit Sub
  End If
  CommonDialog1.CancelError = True
  If CommonDialog1.FileName = "" Then
    On Error GoTo SaveCancel
    CommonDialog1.ShowSave
    On Error GoTo 0
  End If
  If CommonDialog1.FileName = "" Then
    savecheck = False
    Exit Sub
  End If
  Form1.Caption = "Description File Editor " + CommonDialog1.FileName
  newflag = False
  If Right(CommonDialog1.FileName, 4) <> ".des" Then
    CommonDialog1.FileName = CommonDialog1.FileName + ".des"
'    errormsg = "Unable to save to file because the file does not have a .des extension"
'    MsgBox errormsg, vbExclamation + vbOKOnly + vbSystemModal, "Error"
'    CommonDialog1.filename = ""
'    savecheck = False
'    Exit Sub
  End If
  filesavecheck = Dir(CommonDialog1.FileName)
  If Not filesavecheck = "" Then
    errormsg = "File: " + CommonDialog1.FileName + " already exists" + Chr(13) + Chr(10)
    errormsg = errormsg + "Do you want to overwrite the file?"
    response = MsgBox(errormsg, vbInformation + vbYesNoCancel + vbSystemModal, "Existing File")
    If response = vbCancel Then
      Exit Sub
    ElseIf response = vbNo Then
      CommonDialog1.ShowSave
      save_Click
    End If
  End If
  If open_csv(fileout, CommonDialog1.FileName, 1) = False Then
    errormsg = "Unable to open file " + CommonDialog1.FileName
    sverror = True
    MsgBox errormsg, vbOKOnly + vbSystemModal + vbExclamation, "Error"
    savecheck = False
    Exit Sub
  End If
  Screen.MousePointer = vbHourglass
  put_val fileout, "mf"
  put_val fileout, "Version 2.1"
  put_line fileout
  modinfostring = ModTypeCombo.Text + ":"
  modinfostring = modinfostring + Combo1.Text + ":"
  For i = 0 To UBound(modnames) - 1
    If modnames(i) = Combo1.Text Then
      modinfostring = modinfostring + modids(i) ' + ":"
      Exit For
    End If
  Next
  'modinfostring = modinfostring + Text3.Text
  put_val fileout, modinfostring
  put_val fileout, ModNmText.Text
  tempstring = UIText.Text + " " + Text1.Text
  put_val fileout, tempstring
  tempstring = ModExeText.Text + " " + Text2.Text
  put_val fileout, tempstring
  tempstring = Text3.Text
  put_val fileout, tempstring
  put_line fileout
  output = Chr(13) + Chr(10) + VERSTRING + Chr(13) + Chr(10)
  output = output + ModVersText.Text + Chr(13) + Chr(10) + Chr(13) + Chr(10)
  If Not ModDescText.Text = "" Then
    If newflag = True Then
      tempstring = DescFormat(ModDescText.Text)
    Else
      tempstring = ModDescText.Text
    End If
    output = output + DESCSTRING + Chr(13) + Chr(10)
    output = output + tempstring + Chr(13) + Chr(10)
    printcheck = True
  End If
  If Not LimitText.Text = "" Then
    If newflag = True Then
      tempstring = DescFormat(LimitText.Text)
    Else
      tempstring = LimitText.Text
    End If
    output = output + Chr(13) + Chr(10) + LIMITSTRING + Chr(13) + Chr(10)
    output = output + tempstring + Chr(13) + Chr(10) + Chr(13) + Chr(10)
    printcheck = True
  End If
  WriteConnections output
  If Not (OSText.Text = "" And ProcText.Text = "" And RAMText.Text = "" And DiskText.Text = "") Then
      output = output + Chr(13) + Chr(10) + SYSSTRING + Chr(13) + Chr(10)
    printcheck = True
    GetInfoString OSText, output, OSSTRING
    GetInfoString ProcText, output, PROCSTRING
    GetInfoString RAMText, output, RAMSTRING
    GetInfoString DiskText, output, DISKSTRING
   End If
  If Not (CompText.Text = "" And AddrssText.Text = "" And CityText.Text = "" And StateCombo.Text = "" And ZipText.Text = "" And PhoneEdit.Text = "(___) ___-____" And FaxEdit.Text = "(___) ___-____" And EmailText.Text = "" And URLText.Text = "") Then
      output = output + Chr(13) + Chr(10) + POCSTRING + Chr(13) + Chr(10)
    printcheck = True
    GetInfoString CompText, output, COMPSTRING
    GetInfoString ModlrText, output, MODLRSTRING
    GetInfoString AddrssText, output, ADDSTRING
    GetInfoString CityText, output, CITYSTRING
    GetInfoString StateCombo, output, STSTRING
    GetInfoString ZipText, output, ZIPSTRING
    GetInfoString CntryText, output, CNTRYSTRING
    If Not (PhoneEdit.Text = "(   )   -   " Or PhoneEdit.Text = "(###) ###-####" Or PhoneEdit.Text = "(___) ___-____") Then
      tempstring = PhoneEdit.Text
      output = output + PHONESTRING + tempstring + Chr(13) + Chr(10)
    End If
    If Not (FaxEdit.Text = "(   )   -   " Or FaxEdit.Text = "(###) ###-####" Or FaxEdit.Text = "(___) ___-____") Then
      tempstring = FaxEdit.Text
      output = output + FAXSTRING + tempstring + Chr(13) + Chr(10)
    End If
    If Not EmailText.Text = "" Then
      tempstring = EmailText.Text
      output = output + EMAILSTRING + tempstring + Chr(13) + Chr(10)
    End If
    GetInfoString URLText, output, URLSTRING
  End If
  If printcheck Then
    outstring = output
    put_val fileout, outstring
    put_line fileout
  End If
  For i = 0 To ReadList.ListCount - 1
    If ReadList.Selected(i) = True And Not (minlist(i) = 0 And maxlist(i) = 0) Then
      readcnt = readcnt + 1
      ReDim Preserve tempreads(readcnt) As filetype
      GetFileType ReadList.list(i), temptype, tempqual
      tempreads(readcnt - 1).Name = temptype
      tempreads(readcnt - 1).ident = tempqual
      tempreads(readcnt - 1).min = minlist(i)
      tempreads(readcnt - 1).max = maxlist(i)
    End If
  Next
  put_val fileout, readcnt
  put_val fileout, "Read"
  put_line fileout
  ReDim outreads(readcnt, readcnt) As filetype
  For i = 0 To readcnt - 1
    For j = 0 To readcnt - 1
      outreads(i, j).Name = tempreads(i).Name
      outreads(i, j).ident = tempreads(i).ident
      outreads(i, j).min = 0
      outreads(i, j).max = tempreads(i).max
    Next
  Next
  For i = 0 To readcnt - 1
    If tempreads(i).min = 0 Then
      outreads(i, i).min = 1
    Else
      For j = 0 To readcnt - 1
        outreads(i, j).min = tempreads(i).min
      Next
    End If
  Next
   For j = 0 To readcnt - 1
    put_val fileout, readcnt
    For i = 0 To readcnt - 1
      put_val fileout, outreads(i, j).Name
      put_val fileout, outreads(i, j).ident
      put_val fileout, outreads(i, j).min
      put_val fileout, outreads(i, j).max
    Next
    put_line fileout
  Next
  For i = 0 To WriteList.ListCount - 1
    If WriteList.Selected(i) = True Then
      writecnt = writecnt + 1
    End If
  Next
  put_val fileout, writecnt
  put_val fileout, "Write"
  put_line fileout
  For i = 0 To WriteList.ListCount - 1
    If WriteList.Selected(i) = True Then
      GetFileType WriteList.list(i), temptype, tempqual
'      tempstring = temptype + "," + tempqual
'      put_val fileout, tempstring
      put_val fileout, temptype
      put_val fileout, tempqual
      put_line fileout
    End If
  Next
  WriteVars fileout
  close_csv fileout
  modified = False
  Screen.MousePointer = vbDefault
SaveCancel:
End Sub
Private Sub UnitCombo_Click()
  If Not newvar Then VarChange 1, False
  modified = True
End Sub
Private Sub UnitType_Click()
  Dim temp As String
  Dim i As Long
  Dim msg As String
  If unitreset Then
    Exit Sub
  End If
  unitreset = True
  LoadUnits UnitType.Text, UnitCombo
  If UnitType.ListIndex = 11 Then
    UnitCombo.ListIndex = -1
  Else
    UnitCombo.ListIndex = 0
  End If
  unitreset = False
  If Not newvar Then VarChange 1, False
  modified = True
End Sub
Private Sub WriteList_Click()
  modified = True
End Sub
Private Sub ZipText_Change()
  modified = True
End Sub
Private Sub UIText_Change()
  modified = True
End Sub
Private Sub URLText_Change()
  modified = True
End Sub
Private Sub StateCombo_Change()
  modified = True
End Sub
Private Sub ReadMx_Change()
  Dim numcheck As Boolean
  Dim errormsg As String
  modified = True
  numcheck = checknum(ReadMx.Text)
  If numcheck = True Then
    If ReadMx.Text = "" Then
      ReadList.ListIndex = 0
      ReadMx.Text = 0
    Else
      maxlist(ReadList.ListIndex) = ReadMx.Text
    End If
  Else
    errormsg = "Invalid character entered for the maximum value"
    errormsg = errormsg + Chr(13) + Chr(10)
    errormsg = errormsg + "Value entered must be a positive long"
    MsgBox errormsg, vbOKOnly + vbSystemModal + vbExclamation, "Error"
    ReadMx.Text = maxlist(ReadList.ListIndex)
  End If
End Sub
Private Sub ReadMn_Change()
  Dim numcheck As Boolean
  Dim errormsg As String
  modified = True
  numcheck = checknum(ReadMn.Text)
  If numcheck = True Then
    If ReadMn.Text = "" Then
      ReadList.ListIndex = 0
      ReadMn.Text = 0
    Else
      minlist(ReadList.ListIndex) = ReadMn.Text
    End If
  Else
    errormsg = "Invalid character entered for minimum value"
    errormsg = errormsg + Chr(13) + Chr(10)
    errormsg = errormsg + "Value entered must be a positive long"
    MsgBox errormsg, vbOKOnly + vbSystemModal + vbExclamation, "Error"
    ReadMn.Text = minlist(ReadList.ListIndex)
  End If
End Sub
Private Sub ProcText_Change()
  modified = True
End Sub
Private Sub RAMText_Change()
  modified = True
End Sub
Private Sub NewDesc_Change()
  ModDescText.Text = NewDesc.Text
  modified = True
End Sub
Private Sub NewLimits_Change()
  LimitText.Text = NewLimits.Text
  modified = True
End Sub
Private Sub OSText_Change()
  modified = True
End Sub
Private Sub PhoneEdit_Change()
  modified = True
End Sub
Private Sub ModDescText_Change()
  modified = True
End Sub
Private Sub ModExeText_Change()
  modified = True
End Sub
Private Sub ModlrText_Change()
  modified = True
End Sub
Private Sub ModNmText_Change()
  modified = True
End Sub
Private Sub ModTypeCombo_Change()
  modified = True
End Sub
Private Sub MinText_Change()
'  Dim numcheck As Boolean
'  Dim errormsg As String
  modified = True
'  numcheck = CheckMinMax(MinText.Text)
'  If Not numcheck Then
'    errormsg = "Invalid character entered for minimum value"
'    errormsg = errormsg + Chr(13) + Chr(10)
'    errormsg = errormsg + "Value entered must be a number"
'    MsgBox errormsg, vbOKOnly + vbSystemModal + vbExclamation, "Error"
'    MinText.Text = ""
'  End If
  If Not newvar Then VarChange 1, False
End Sub
Private Sub LimitText_Change()
  modified = True
End Sub
Private Sub MaxText_Change()
'  Dim numcheck As Boolean
'  Dim errormsg As String
  modified = True
'  numcheck = CheckMinMax(MaxText.Text)
'  If Not numcheck Then
'    errormsg = "Invalid character entered for maximum value"
'    errormsg = errormsg + Chr(13) + Chr(10)
'    errormsg = errormsg + "Value entered must be a number"
'    MsgBox errormsg, vbOKOnly + vbSystemModal + vbExclamation, "Error"
'    MaxText.Text = ""
'  End If
  If Not newvar Then VarChange 1, False
End Sub
Private Sub DiskText_Change()
  modified = True
End Sub
Private Sub EmailText_Change()
  modified = True
End Sub
Private Sub FaxEdit_Change()
  modified = True
End Sub
Private Sub DesVarFlag_Change()
  modified = True
End Sub
Private Sub DescText_Change()
  Dim cuestring As String
  Dim nodx As Node
  If Not newvar Then VarChange 1, False
  modified = True
'  Set nodx = TreeView1.SelectedItem
'  TreeView1_NodeClick nodx
'  DescText.SetFocus
'    cuestring = variables.Item(TreeView1.SelectedItem.Parent.Text).desc
'    For i = 1 To variables.Item(TreeView1.SelectedItem.Parent.Text).labels.Count
'      cuestring = cuestring + " for "
'      cuestring = cuestring + variables.Item(TreeView1.SelectedItem.Parent.Text).labels.Item(i).name
'    Next
'    CueText(0).Locked = False
'    CueText(1).Locked = False
'    CueText(0).Text = cuestring
'    CueText(1).Text = cuestring
'    CueText(0).Locked = True
'    CueText(1).Locked = True
End Sub
Private Sub CityText_Change()
  modified = True
End Sub
Private Sub CntryText_Change()
  modified = True
End Sub
Private Sub CompText_Change()
  modified = True
End Sub
Private Sub AddrssText_Change()
  modified = True
End Sub

