VERSION 5.00
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "threed32.ocx"
Begin VB.Form FluxTypes 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Atmospheric Suspended Particle Defintions"
   ClientHeight    =   4476
   ClientLeft      =   3192
   ClientTop       =   3660
   ClientWidth     =   7680
   ControlBox      =   0   'False
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   NegotiateMenus  =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   373
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   640
   ShowInTaskbar   =   0   'False
   Begin Threed.SSFrame s 
      Height          =   4335
      Left            =   120
      TabIndex        =   0
      Top             =   0
      Width           =   6015
      _Version        =   65536
      _ExtentX        =   10610
      _ExtentY        =   7646
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
      Begin Threed.SSCheck SSCheck1 
         Height          =   252
         Index           =   1
         Left            =   240
         TabIndex        =   6
         Tag             =   "Particle 1"
         Top             =   1200
         Width           =   2532
         _Version        =   65536
         _ExtentX        =   4466
         _ExtentY        =   445
         _StockProps     =   78
         Caption         =   "Particle 1"
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Value           =   -1  'True
      End
      Begin VB.ComboBox unit 
         Height          =   288
         Index           =   0
         Left            =   3720
         Style           =   2  'Dropdown List
         TabIndex        =   36
         Tag             =   "g/cm^3"
         Top             =   840
         Width           =   1005
      End
      Begin VB.TextBox txt 
         Height          =   288
         Index           =   0
         Left            =   2760
         TabIndex        =   37
         Tag             =   "flux"
         Text            =   "0.01"
         Top             =   840
         Width           =   1000
      End
      Begin VB.ComboBox unit 
         Height          =   288
         Index           =   7
         Left            =   3720
         Style           =   2  'Dropdown List
         TabIndex        =   4
         Tag             =   "um"
         Top             =   3480
         Width           =   1000
      End
      Begin VB.TextBox txt 
         Height          =   288
         Index           =   7
         Left            =   2760
         TabIndex        =   3
         Tag             =   "flux"
         Text            =   "0.3"
         Top             =   3480
         Width           =   1000
      End
      Begin VB.ComboBox unit 
         Height          =   315
         Index           =   6
         Left            =   3720
         Style           =   2  'Dropdown List
         TabIndex        =   31
         Tag             =   "g/cm^3"
         Top             =   3840
         Width           =   1000
      End
      Begin VB.TextBox txt 
         Height          =   288
         Index           =   6
         Left            =   2760
         TabIndex        =   30
         Tag             =   "flux"
         Text            =   "1.5"
         Top             =   3840
         Width           =   1000
      End
      Begin VB.ComboBox unit 
         Height          =   288
         Index           =   5
         Left            =   3720
         Style           =   2  'Dropdown List
         TabIndex        =   27
         Tag             =   "um"
         Top             =   2400
         Width           =   1000
      End
      Begin VB.TextBox txt 
         Height          =   288
         Index           =   5
         Left            =   2760
         TabIndex        =   26
         Tag             =   "flux"
         Text            =   "1.0"
         Top             =   2400
         Width           =   1000
      End
      Begin VB.ComboBox unit 
         Height          =   315
         Index           =   4
         Left            =   3720
         Style           =   2  'Dropdown List
         TabIndex        =   22
         Tag             =   "g/cm^3"
         Top             =   2760
         Width           =   1000
      End
      Begin VB.TextBox txt 
         Height          =   288
         Index           =   4
         Left            =   2760
         TabIndex        =   21
         Tag             =   "flux"
         Text            =   "1.5"
         Top             =   2760
         Width           =   1000
      End
      Begin VB.ComboBox unit 
         Height          =   288
         Index           =   3
         Left            =   3720
         Style           =   2  'Dropdown List
         TabIndex        =   18
         Tag             =   "um"
         Top             =   1440
         Width           =   1000
      End
      Begin VB.TextBox txt 
         Height          =   288
         Index           =   3
         Left            =   2760
         TabIndex        =   17
         Tag             =   "flux"
         Text            =   "3.0"
         Top             =   1440
         Width           =   1000
      End
      Begin VB.ComboBox unit 
         Height          =   315
         Index           =   2
         Left            =   3720
         Style           =   2  'Dropdown List
         TabIndex        =   13
         Tag             =   "g/cm^3"
         Top             =   1800
         Width           =   1000
      End
      Begin VB.TextBox txt 
         Height          =   288
         Index           =   2
         Left            =   2760
         TabIndex        =   12
         Tag             =   "flux"
         Text            =   "1.5"
         Top             =   1800
         Width           =   1000
      End
      Begin VB.ComboBox unit 
         Height          =   288
         Index           =   1
         Left            =   3720
         Style           =   2  'Dropdown List
         TabIndex        =   9
         Tag             =   "fraction"
         Top             =   480
         Width           =   1000
      End
      Begin VB.TextBox txt 
         Height          =   288
         Index           =   1
         Left            =   2760
         TabIndex        =   8
         Tag             =   "flux"
         Text            =   "0.75"
         Top             =   480
         Width           =   1000
      End
      Begin Threed.SSCheck SSCheck1 
         Height          =   252
         Index           =   0
         Left            =   240
         TabIndex        =   1
         Tag             =   "Gas 1"
         Top             =   240
         Width           =   2532
         _Version        =   65536
         _ExtentX        =   4466
         _ExtentY        =   445
         _StockProps     =   78
         Caption         =   "Gas 1"
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
      Begin Threed.SSCheck SSCheck1 
         Height          =   255
         Index           =   3
         Left            =   240
         TabIndex        =   24
         Tag             =   "Particle 3"
         Top             =   3240
         Width           =   2535
         _Version        =   65536
         _ExtentX        =   4466
         _ExtentY        =   445
         _StockProps     =   78
         Caption         =   "Particle 3"
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
      Begin Threed.SSCheck SSCheck1 
         Height          =   255
         Index           =   2
         Left            =   240
         TabIndex        =   15
         Tag             =   "Particle 2"
         Top             =   2160
         Width           =   2535
         _Version        =   65536
         _ExtentX        =   4466
         _ExtentY        =   445
         _StockProps     =   78
         Caption         =   "Particle 2"
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
      Begin VB.Label Label1 
         Caption         =   "Particle 1"
         Height          =   252
         Left            =   420
         TabIndex        =   40
         Top             =   1200
         Width           =   2532
      End
      Begin VB.Label ref 
         Caption         =   "Ref: 0"
         Height          =   255
         Index           =   0
         Left            =   4800
         TabIndex        =   39
         Tag             =   "0"
         Top             =   840
         Width           =   990
      End
      Begin VB.Label lbl 
         Caption         =   "Density"
         Height          =   255
         Index           =   0
         Left            =   480
         TabIndex        =   38
         Tag             =   "reactivedensity"
         Top             =   840
         Width           =   2295
      End
      Begin VB.Label lbl 
         Caption         =   "Radius"
         Height          =   252
         Index           =   7
         Left            =   480
         TabIndex        =   2
         Tag             =   "radius"
         Top             =   3480
         Width           =   2292
      End
      Begin VB.Label ref 
         Caption         =   "Ref: 0"
         Height          =   252
         Index           =   7
         Left            =   4800
         TabIndex        =   5
         Tag             =   "0"
         Top             =   3480
         Width           =   996
      End
      Begin VB.Label lbl 
         Caption         =   "Density"
         Height          =   255
         Index           =   6
         Left            =   480
         TabIndex        =   29
         Tag             =   "density"
         Top             =   3840
         Width           =   2295
      End
      Begin VB.Label lbl 
         Caption         =   "Radius"
         Height          =   252
         Index           =   5
         Left            =   480
         TabIndex        =   25
         Tag             =   "radius"
         Top             =   2400
         Width           =   2292
      End
      Begin VB.Label ref 
         Caption         =   "Ref: 0"
         Height          =   255
         Index           =   6
         Left            =   4800
         TabIndex        =   32
         Tag             =   "0"
         Top             =   3840
         Width           =   990
      End
      Begin VB.Label ref 
         Caption         =   "Ref: 0"
         Height          =   252
         Index           =   5
         Left            =   4800
         TabIndex        =   28
         Tag             =   "0"
         Top             =   2400
         Width           =   996
      End
      Begin VB.Label lbl 
         Caption         =   "Density"
         Height          =   255
         Index           =   4
         Left            =   480
         TabIndex        =   20
         Tag             =   "density"
         Top             =   2760
         Width           =   2295
      End
      Begin VB.Label lbl 
         Caption         =   "Radius"
         Height          =   252
         Index           =   3
         Left            =   480
         TabIndex        =   16
         Tag             =   "radius"
         Top             =   1440
         Width           =   2292
      End
      Begin VB.Label ref 
         Caption         =   "Ref: 0"
         Height          =   255
         Index           =   4
         Left            =   4800
         TabIndex        =   23
         Tag             =   "0"
         Top             =   2760
         Width           =   990
      End
      Begin VB.Label ref 
         Caption         =   "Ref: 0"
         Height          =   252
         Index           =   3
         Left            =   4800
         TabIndex        =   19
         Tag             =   "0"
         Top             =   1440
         Width           =   996
      End
      Begin VB.Label lbl 
         Caption         =   "Density"
         Height          =   255
         Index           =   2
         Left            =   480
         TabIndex        =   11
         Tag             =   "density"
         Top             =   1800
         Width           =   2295
      End
      Begin VB.Label lbl 
         Caption         =   "Non reactive fraction"
         Height          =   252
         Index           =   1
         Left            =   480
         TabIndex        =   7
         Tag             =   "reactivefrac"
         Top             =   480
         Width           =   2292
      End
      Begin VB.Label ref 
         Caption         =   "Ref: 0"
         Height          =   255
         Index           =   2
         Left            =   4800
         TabIndex        =   14
         Tag             =   "0"
         Top             =   1800
         Width           =   990
      End
      Begin VB.Label ref 
         Caption         =   "Ref: 0"
         Height          =   252
         Index           =   1
         Left            =   4800
         TabIndex        =   10
         Tag             =   "0"
         Top             =   480
         Width           =   996
      End
   End
   Begin Threed.SSCommand SSCommand3 
      Height          =   372
      Left            =   6360
      TabIndex        =   33
      Top             =   240
      Width           =   1212
      _Version        =   65536
      _ExtentX        =   2138
      _ExtentY        =   656
      _StockProps     =   78
      Caption         =   "Add Ref"
   End
   Begin Threed.SSCommand SSCommand2 
      Height          =   372
      Left            =   6360
      TabIndex        =   34
      Top             =   720
      Width           =   1212
      _Version        =   65536
      _ExtentX        =   2138
      _ExtentY        =   656
      _StockProps     =   78
      Caption         =   "Select Ref"
   End
   Begin Threed.SSCommand SSCommand1 
      Height          =   372
      Left            =   6360
      TabIndex        =   35
      Top             =   1560
      Width           =   1212
      _Version        =   65536
      _ExtentX        =   2138
      _ExtentY        =   656
      _StockProps     =   78
      Caption         =   "Done"
   End
End
Attribute VB_Name = "FluxTypes"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text
Dim changing As Boolean

Private Sub Form_load()
  Dim i As Long
  For i = 0 To 7
    get_conversion_items unit(i).Tag, unit(i)
  Next
  Left = (Screen.Width - Width) / 2
  Top = (Screen.Height - Height) / 2
  Hide
End Sub

Sub SetEnabled(x As Long, v As Boolean)
  lbl(x).Enabled = v
  txt(x).Enabled = v
  unit(x).Enabled = v
  ref(x).Enabled = v
End Sub

Private Sub SSCheck1_Click(Index As Integer, Value As Integer)
  SSCheck1(3).Enabled = SSCheck1(2).Value And SSCheck1(1).Value
  SSCheck1(2).Enabled = SSCheck1(1).Value
  If Not SSCheck1(1).Enabled Or Not SSCheck1(1).Value Then SSCheck1(2).Value = False
  If Not SSCheck1(2).Enabled Or Not SSCheck1(2).Value Then SSCheck1(3).Value = False
  If Not SSCheck1(0).Value Then SSCheck1(1).Value = True
  If Not SSCheck1(1).Value Then SSCheck1(0).Value = True
  SetEnabled 0, SSCheck1(0).Value And SSCheck1(0).Enabled
  SetEnabled 1, SSCheck1(0).Value And SSCheck1(0).Enabled
  SetEnabled 2, SSCheck1(1).Value And SSCheck1(1).Enabled
  SetEnabled 3, SSCheck1(1).Value And SSCheck1(1).Enabled
  SetEnabled 4, SSCheck1(2).Value And SSCheck1(2).Enabled
  SetEnabled 5, SSCheck1(2).Value And SSCheck1(2).Enabled
  SetEnabled 6, SSCheck1(3).Value And SSCheck1(3).Enabled
  SetEnabled 7, SSCheck1(3).Value And SSCheck1(3).Enabled
End Sub

Private Sub SSCheck1_GotFocus(Index As Integer)
  SSCommand2.Enabled = False
  SSCommand3.Enabled = False
  HelpAnchor = lbl(Index).Tag
End Sub

Private Sub SSCommand1_Click()
  FluxTypes.Hide
End Sub

Private Sub SSCommand2_Click()
  RefMode = 0
  GetRef ref(RefItem)
  txt(RefItem).SetFocus
End Sub

Private Sub SSCommand3_Click()
  RefMode = 1
  GetRef ref(RefItem)
  txt(RefItem).SetFocus
End Sub

Private Sub txt_GotFocus(Index As Integer)
  RefItem = Index
  SSCommand2.Enabled = True
  SSCommand3.Enabled = True
  HelpAnchor = txt(Index).Tag
End Sub

Private Sub unit_GotFocus(Index As Integer)
  RefItem = Index
  SSCommand2.Enabled = True
  SSCommand3.Enabled = True
  HelpAnchor = lbl(Index).Tag
End Sub

Private Sub Form_KeyPress(KeyAscii As Integer)
  If KeyAscii = 13 Then KeyAscii = 0
End Sub

Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
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
