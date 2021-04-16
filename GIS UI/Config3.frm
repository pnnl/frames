VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Object = "{6B7E6392-850A-101B-AFC0-4210102A8DA7}#1.3#0"; "comctl32.ocx"
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "THREED32.OCX"
Begin VB.Form frmConfig2 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Default Properties"
   ClientHeight    =   7032
   ClientLeft      =   48
   ClientTop       =   336
   ClientWidth     =   8232
   ControlBox      =   0   'False
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   7032
   ScaleWidth      =   8232
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton Command2 
      Caption         =   "Cancel"
      Default         =   -1  'True
      Height          =   345
      Left            =   6540
      TabIndex        =   42
      Top             =   6495
      Width           =   1395
   End
   Begin VB.CommandButton Command1 
      Caption         =   "OK"
      Height          =   360
      Left            =   4935
      TabIndex        =   41
      Top             =   6480
      Width           =   1395
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   6540
      Top             =   -120
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin Threed.SSPanel SSPanel2 
      Height          =   5505
      Left            =   600
      TabIndex        =   36
      Top             =   600
      Width           =   7665
      _Version        =   65536
      _ExtentX        =   13520
      _ExtentY        =   9710
      _StockProps     =   15
      BackColor       =   13160660
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   7.8
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Begin VB.Frame Frame7 
         Height          =   4572
         Left            =   360
         TabIndex        =   37
         Top             =   390
         Width           =   7092
         Begin ComctlLib.Toolbar tbrLayer 
            Height          =   372
            Left            =   216
            Negotiate       =   -1  'True
            TabIndex        =   38
            Top             =   312
            Width           =   384
            _ExtentX        =   677
            _ExtentY        =   656
            ButtonWidth     =   609
            ButtonHeight    =   582
            AllowCustomize  =   0   'False
            ImageList       =   "ImageList1"
            _Version        =   327682
            BeginProperty Buttons {0713E452-850A-101B-AFC0-4210102A8DA7} 
               NumButtons      =   7
               BeginProperty Button1 {0713F354-850A-101B-AFC0-4210102A8DA7} 
                  Key             =   "AddFile"
                  Object.ToolTipText     =   "Add File"
                  Object.Tag             =   "AddFile"
                  ImageKey        =   "AddFile"
               EndProperty
               BeginProperty Button2 {0713F354-850A-101B-AFC0-4210102A8DA7} 
                  Object.Visible         =   0   'False
                  Key             =   "Add"
                  Object.ToolTipText     =   "Add Layer"
                  Object.Tag             =   "AddLayer"
                  ImageKey        =   "Add"
               EndProperty
               BeginProperty Button3 {0713F354-850A-101B-AFC0-4210102A8DA7} 
                  Key             =   "Delete"
                  Object.ToolTipText     =   "Delete Layer"
                  Object.Tag             =   "Delete"
                  ImageKey        =   "Delete"
               EndProperty
               BeginProperty Button4 {0713F354-850A-101B-AFC0-4210102A8DA7} 
                  Object.Tag             =   ""
                  Style           =   3
                  MixedState      =   -1  'True
               EndProperty
               BeginProperty Button5 {0713F354-850A-101B-AFC0-4210102A8DA7} 
                  Key             =   "Up"
                  Object.ToolTipText     =   "Promote Layer"
                  Object.Tag             =   "Up"
                  ImageKey        =   "UpEnabled"
               EndProperty
               BeginProperty Button6 {0713F354-850A-101B-AFC0-4210102A8DA7} 
                  Key             =   "Down"
                  Object.ToolTipText     =   "Demote Layer"
                  Object.Tag             =   "Down"
                  ImageKey        =   "DownEnabled"
               EndProperty
               BeginProperty Button7 {0713F354-850A-101B-AFC0-4210102A8DA7} 
                  Object.Tag             =   ""
                  Style           =   3
                  MixedState      =   -1  'True
               EndProperty
            EndProperty
         End
         Begin VB.TextBox Text2 
            Height          =   285
            Index           =   1
            Left            =   2730
            TabIndex        =   55
            Top             =   4110
            Visible         =   0   'False
            Width           =   1500
         End
         Begin VB.TextBox Text2 
            Height          =   285
            Index           =   0
            Left            =   2730
            TabIndex        =   54
            Top             =   3780
            Visible         =   0   'False
            Width           =   1500
         End
         Begin VB.TextBox Text1 
            Height          =   285
            Index           =   1
            Left            =   795
            TabIndex        =   53
            Top             =   4110
            Visible         =   0   'False
            Width           =   1500
         End
         Begin VB.TextBox Text1 
            Height          =   285
            Index           =   0
            Left            =   795
            TabIndex        =   52
            Top             =   3780
            Visible         =   0   'False
            Width           =   1500
         End
         Begin VB.PictureBox picLayerColor 
            Height          =   315
            HelpContextID   =   301
            Left            =   4800
            MouseIcon       =   "Config3.frx":0000
            ScaleHeight     =   264
            ScaleWidth      =   300
            TabIndex        =   49
            Top             =   2940
            Visible         =   0   'False
            WhatsThisHelpID =   301
            Width           =   345
         End
         Begin VB.ComboBox cboColor 
            Height          =   315
            ItemData        =   "Config3.frx":0442
            Left            =   3345
            List            =   "Config3.frx":0476
            Style           =   2  'Dropdown List
            TabIndex        =   48
            Top             =   2940
            Visible         =   0   'False
            Width           =   1395
         End
         Begin VB.ListBox lstLayers 
            Height          =   2352
            HelpContextID   =   300
            Left            =   645
            TabIndex        =   39
            Top             =   315
            WhatsThisHelpID =   300
            Width           =   6180
         End
         Begin Threed.SSCheck chkLock 
            Height          =   255
            Left            =   780
            TabIndex        =   40
            Top             =   3390
            Visible         =   0   'False
            Width           =   1395
            _Version        =   65536
            _ExtentX        =   2469
            _ExtentY        =   444
            _StockProps     =   78
            Caption         =   "Pickable"
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
         Begin Threed.SSCheck chkVisible 
            Height          =   255
            Left            =   780
            TabIndex        =   50
            Top             =   2970
            Visible         =   0   'False
            Width           =   1395
            _Version        =   65536
            _ExtentX        =   2461
            _ExtentY        =   450
            _StockProps     =   78
            Caption         =   "Visible"
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
         Begin VB.Label lblColor 
            AutoSize        =   -1  'True
            Caption         =   "Color:"
            Height          =   195
            Left            =   2880
            TabIndex        =   51
            Top             =   3000
            Visible         =   0   'False
            Width           =   405
         End
      End
   End
   Begin Threed.SSPanel SSPanel1 
      Height          =   5505
      Left            =   405
      TabIndex        =   0
      Top             =   645
      Width           =   7665
      _Version        =   65536
      _ExtentX        =   13520
      _ExtentY        =   9710
      _StockProps     =   15
      BackColor       =   13160660
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   7.8
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Begin VB.Frame Frame3 
         Caption         =   "Polygons"
         Height          =   1905
         Left            =   270
         TabIndex        =   28
         Top             =   3330
         Width           =   3330
         Begin VB.PictureBox picDLayerColor 
            Height          =   315
            HelpContextID   =   301
            Index           =   2
            Left            =   2880
            MouseIcon       =   "Config3.frx":04F8
            ScaleHeight     =   264
            ScaleWidth      =   300
            TabIndex        =   45
            Top             =   375
            WhatsThisHelpID =   301
            Width           =   345
         End
         Begin VB.ComboBox cboDFill 
            Height          =   315
            Index           =   2
            Left            =   1395
            Style           =   2  'Dropdown List
            TabIndex        =   31
            Top             =   1320
            Width           =   1440
         End
         Begin VB.ComboBox cboDColor 
            Height          =   315
            Index           =   2
            Left            =   1410
            Style           =   2  'Dropdown List
            TabIndex        =   30
            Tag             =   "polygon"
            Top             =   390
            Width           =   1440
         End
         Begin VB.ComboBox cboDLine 
            Height          =   315
            Index           =   2
            Left            =   1395
            Style           =   2  'Dropdown List
            TabIndex        =   29
            Top             =   825
            Width           =   1440
         End
         Begin VB.Label Label6 
            AutoSize        =   -1  'True
            Caption         =   "Fill Style:"
            Height          =   195
            Left            =   405
            TabIndex        =   34
            Top             =   1395
            Width           =   615
         End
         Begin VB.Label Label1 
            AutoSize        =   -1  'True
            Caption         =   "Color:"
            Height          =   195
            Index           =   2
            Left            =   405
            TabIndex        =   33
            Top             =   435
            Width           =   405
         End
         Begin VB.Label Label3 
            AutoSize        =   -1  'True
            Caption         =   "Line Style:"
            Height          =   195
            Index           =   1
            Left            =   390
            TabIndex        =   32
            Top             =   885
            Width           =   735
         End
      End
      Begin VB.Frame Frame4 
         Caption         =   "Grids"
         Height          =   4965
         Left            =   3855
         TabIndex        =   9
         Top             =   270
         Width           =   3570
         Begin VB.Frame Frame5 
            Caption         =   "Cartesian"
            Height          =   2085
            Left            =   180
            TabIndex        =   19
            Top             =   375
            Width           =   3210
            Begin VB.PictureBox picDLayerColor 
               Height          =   315
               HelpContextID   =   301
               Index           =   3
               Left            =   2670
               MouseIcon       =   "Config3.frx":093A
               ScaleHeight     =   264
               ScaleWidth      =   300
               TabIndex        =   46
               Top             =   300
               WhatsThisHelpID =   301
               Width           =   345
            End
            Begin VB.ComboBox cboDColor 
               Height          =   315
               Index           =   3
               Left            =   1185
               Style           =   2  'Dropdown List
               TabIndex        =   23
               Top             =   315
               Width           =   1440
            End
            Begin VB.ComboBox cboDLine 
               Height          =   315
               Index           =   3
               Left            =   1185
               Style           =   2  'Dropdown List
               TabIndex        =   22
               Top             =   735
               Width           =   1440
            End
            Begin VB.TextBox txtDX 
               Height          =   285
               Index           =   3
               Left            =   1500
               TabIndex        =   21
               Text            =   "Text1"
               Top             =   1215
               Width           =   1080
            End
            Begin VB.TextBox txtDY 
               Height          =   285
               Index           =   3
               Left            =   1515
               TabIndex        =   20
               Text            =   "Text1"
               Top             =   1575
               Width           =   1080
            End
            Begin VB.Label Label1 
               AutoSize        =   -1  'True
               Caption         =   "Color:"
               Height          =   195
               Index           =   3
               Left            =   405
               TabIndex        =   27
               Top             =   375
               Width           =   405
            End
            Begin VB.Label Label3 
               AutoSize        =   -1  'True
               Caption         =   "Line Style:"
               Height          =   195
               Index           =   2
               Left            =   390
               TabIndex        =   26
               Top             =   810
               Width           =   735
            End
            Begin VB.Label Label2 
               AutoSize        =   -1  'True
               Caption         =   "X Distances:"
               Height          =   195
               Index           =   0
               Left            =   420
               TabIndex        =   25
               Top             =   1230
               Width           =   900
            End
            Begin VB.Label Label2 
               AutoSize        =   -1  'True
               Caption         =   "Y Distances:"
               Height          =   195
               Index           =   1
               Left            =   405
               TabIndex        =   24
               Top             =   1575
               Width           =   900
            End
         End
         Begin VB.Frame Frame6 
            Caption         =   "Polar"
            Height          =   2085
            Left            =   195
            TabIndex        =   10
            Top             =   2580
            Width           =   3210
            Begin VB.PictureBox picDLayerColor 
               Height          =   315
               HelpContextID   =   301
               Index           =   4
               Left            =   2685
               MouseIcon       =   "Config3.frx":0D7C
               ScaleHeight     =   264
               ScaleWidth      =   300
               TabIndex        =   47
               Top             =   300
               WhatsThisHelpID =   301
               Width           =   345
            End
            Begin VB.ComboBox cboDColor 
               Height          =   315
               Index           =   4
               Left            =   1215
               Style           =   2  'Dropdown List
               TabIndex        =   14
               Top             =   315
               Width           =   1440
            End
            Begin VB.ComboBox cboDLine 
               Height          =   315
               Index           =   4
               Left            =   1230
               Style           =   2  'Dropdown List
               TabIndex        =   13
               Top             =   690
               Width           =   1440
            End
            Begin VB.TextBox txtDX 
               Height          =   285
               Index           =   4
               Left            =   1230
               TabIndex        =   12
               Text            =   "Text1"
               Top             =   1200
               Width           =   1080
            End
            Begin VB.TextBox txtDY 
               Height          =   285
               Index           =   4
               Left            =   1230
               TabIndex        =   11
               Text            =   "Text1"
               Top             =   1515
               Width           =   1080
            End
            Begin VB.Label Label1 
               AutoSize        =   -1  'True
               Caption         =   "Color:"
               Height          =   195
               Index           =   4
               Left            =   435
               TabIndex        =   18
               Top             =   375
               Width           =   405
            End
            Begin VB.Label Label3 
               AutoSize        =   -1  'True
               Caption         =   "Line Style:"
               Height          =   195
               Index           =   3
               Left            =   435
               TabIndex        =   17
               Top             =   765
               Width           =   735
            End
            Begin VB.Label Label2 
               AutoSize        =   -1  'True
               Caption         =   "Sectors:"
               Height          =   195
               Index           =   2
               Left            =   420
               TabIndex        =   16
               Top             =   1230
               Width           =   585
            End
            Begin VB.Label Label2 
               AutoSize        =   -1  'True
               Caption         =   "Directions:"
               Height          =   195
               Index           =   3
               Left            =   375
               TabIndex        =   15
               Top             =   1575
               Width           =   750
            End
         End
      End
      Begin VB.Frame Frame2 
         Caption         =   "(Poly)Lines"
         Height          =   1455
         Left            =   240
         TabIndex        =   4
         Top             =   1620
         Width           =   3360
         Begin VB.PictureBox picDLayerColor 
            Height          =   315
            HelpContextID   =   301
            Index           =   1
            Left            =   2895
            MouseIcon       =   "Config3.frx":11BE
            ScaleHeight     =   264
            ScaleWidth      =   300
            TabIndex        =   44
            Top             =   345
            WhatsThisHelpID =   301
            Width           =   345
         End
         Begin VB.ComboBox cboDLine 
            Height          =   315
            Index           =   1
            Left            =   1425
            Style           =   2  'Dropdown List
            TabIndex        =   6
            Top             =   840
            Width           =   1440
         End
         Begin VB.ComboBox cboDColor 
            Height          =   315
            Index           =   1
            Left            =   1440
            Style           =   2  'Dropdown List
            TabIndex        =   5
            Tag             =   "line"
            Top             =   345
            Width           =   1440
         End
         Begin VB.Label Label3 
            AutoSize        =   -1  'True
            Caption         =   "Line Style:"
            Height          =   195
            Index           =   0
            Left            =   390
            TabIndex        =   8
            Top             =   915
            Width           =   735
         End
         Begin VB.Label Label1 
            AutoSize        =   -1  'True
            Caption         =   "Color:"
            Height          =   195
            Index           =   1
            Left            =   390
            TabIndex        =   7
            Top             =   390
            Width           =   405
         End
      End
      Begin VB.Frame Frame1 
         Caption         =   "Points"
         Height          =   1050
         Left            =   255
         TabIndex        =   1
         Top             =   330
         Width           =   3375
         Begin VB.PictureBox picDLayerColor 
            Height          =   315
            HelpContextID   =   301
            Index           =   0
            Left            =   2910
            MouseIcon       =   "Config3.frx":1600
            ScaleHeight     =   264
            ScaleWidth      =   300
            TabIndex        =   43
            Top             =   375
            WhatsThisHelpID =   301
            Width           =   345
         End
         Begin VB.ComboBox cboDColor 
            Height          =   315
            Index           =   0
            Left            =   1425
            Style           =   2  'Dropdown List
            TabIndex        =   2
            Tag             =   "point"
            Top             =   375
            Width           =   1440
         End
         Begin VB.Label Label1 
            AutoSize        =   -1  'True
            Caption         =   "Color:"
            Height          =   195
            Index           =   0
            Left            =   420
            TabIndex        =   3
            Top             =   450
            Width           =   405
         End
      End
   End
   Begin ComctlLib.TabStrip TabStrip1 
      Height          =   6180
      Left            =   120
      TabIndex        =   35
      Top             =   150
      Width           =   7995
      _ExtentX        =   14097
      _ExtentY        =   10901
      _Version        =   327682
      BeginProperty Tabs {0713E432-850A-101B-AFC0-4210102A8DA7} 
         NumTabs         =   2
         BeginProperty Tab1 {0713F341-850A-101B-AFC0-4210102A8DA7} 
            Caption         =   "Define Coverages"
            Key             =   "sspanel2"
            Object.Tag             =   ""
            ImageVarType    =   2
         EndProperty
         BeginProperty Tab2 {0713F341-850A-101B-AFC0-4210102A8DA7} 
            Caption         =   "Default Properties"
            Key             =   "sspanel1"
            Object.Tag             =   ""
            ImageVarType    =   2
         EndProperty
      EndProperty
   End
   Begin ComctlLib.ImageList ImageList1 
      Left            =   5655
      Top             =   -90
      _ExtentX        =   995
      _ExtentY        =   995
      BackColor       =   12632256
      ImageWidth      =   16
      ImageHeight     =   16
      MaskColor       =   8421376
      _Version        =   327682
      BeginProperty Images {0713E8C2-850A-101B-AFC0-4210102A8DA7} 
         NumListImages   =   7
         BeginProperty ListImage1 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "Config3.frx":1A42
            Key             =   "Delete"
         EndProperty
         BeginProperty ListImage2 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "Config3.frx":1F94
            Key             =   "AddFile"
         EndProperty
         BeginProperty ListImage3 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "Config3.frx":22AE
            Key             =   "UpEnabled"
         EndProperty
         BeginProperty ListImage4 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "Config3.frx":2800
            Key             =   "UpDisabled"
         EndProperty
         BeginProperty ListImage5 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "Config3.frx":2D52
            Key             =   "DownEnabled"
         EndProperty
         BeginProperty ListImage6 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "Config3.frx":32A4
            Key             =   "DownDisabled"
         EndProperty
         BeginProperty ListImage7 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "Config3.frx":37F6
            Key             =   "Add"
         EndProperty
      EndProperty
   End
End
Attribute VB_Name = "frmConfig2"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Private extX0 As Double
Private extY0 As Double
Private extX1 As Double
Private extY1 As Double


Dim Dialog As CommonDialog
Dim PathIdx As Integer
Private layerIndex As Long
Private tmpLayers() As LAYERSTRUCT
Private tmpDefaults() As DEFAULTSTRUCT

Private ReDisplay As Boolean

Private Sub cboColor_Click()
 If layerIndex < 0 Then Exit Sub
 If cboColor.ListIndex < 0 Then Exit Sub
 picLayerColor.BackColor = QBColor(cboColor.ItemData(cboColor.ListIndex))
 layers(layerIndex).color = cboColor.ListIndex + 1
 layers(layerIndex).lcolor = layers(layerIndex).color
 frmMain.ReDisplay = True
End Sub

Private Sub cboDColor_Click(Index As Integer)
'if IgnoreEvents Then Exit Sub
 If cboDColor(Index).ListIndex < 0 Then Exit Sub
 defaults(Index).color = cboDColor(Index).ListIndex + 1
 picDLayerColor(Index).BackColor = QBColor(cboDColor(Index).ItemData(cboDColor(Index).ListIndex))
End Sub

Private Sub cboDFill_Click(Index As Integer)
  If IgnoreEvents Then Exit Sub
  defaults(Index).fill = cboDFill(Index).ItemData(cboDFill(Index).ListIndex)
End Sub

Private Sub cboDLine_Click(Index As Integer)
  If IgnoreEvents Then Exit Sub
  defaults(Index).linetype = cboDLine(Index).ItemData(cboDLine(Index).ListIndex)
End Sub


Private Sub chkLock_Click(Value As Integer)
  If IgnoreEvents Then Exit Sub
  layers(layerIndex).pickable = chkLock.Value
End Sub

Private Sub Command1_Click()
Dim l As Long, dbc As Dbcocx, nod1 As Node, nod2 As Node, i As Integer
Dim nvis As Boolean
  
  If 0 = lstLayers.ListCount Then
    MsgBox "At least one coverage must be defined."
    Exit Sub
  End If
  
  For i = 0 To lstLayers.ListCount - 1
    If layers(i + 1).visible Then nvis = True
    For l = 0 To lstLayers.ListCount - 2
      If i <> l Then
        If lstLayers.list(i) = lstLayers.list(l) Then
          MsgBox "Layer names must not be duplicated."
          Exit Sub
        End If
      End If
    Next l
  Next i
  
  If Not nvis Then
    MsgBox "At least one coverage must be visible."
    Exit Sub
  End If
  
  Set dbc = frmMain.Dbcocx1
  For l = 1 To UBound(layers)
    If layers(l).filename <> "" Then
    Else
      frmMain.Dbcocx1.devgrcmdlayerdef layers(l).name, layers(l).lcolor, layers(l).linetype
      If layers(l).bkgd Then
        frmMain.Dbcocx1.devsetfield "TEXT", layers(l).filename
      Else
        If layers(l).visible Then
          frmMain.Dbcocx1.devgrcmdlayeron layers(l).name ' ECOLOR is positive
        Else
          frmMain.Dbcocx1.devgrcmdlayeroff layers(l).name ' ECOLOR is negative
        End If
        frmMain.Dbcocx1.devsetfield "VX1", layers(l).fill
        frmMain.Dbcocx1.devsetfield "TEXT", layers(l).objtype
      End If
    End If
  Next l
  
  Unload Me
End Sub

Private Sub Command2_Click()
  RestoreGlobalStructures
  Unload Me
End Sub

Private Sub Form_load()
Dim i As Long
  IgnoreEvents = True
  If 0 = UBound(defaults) Then InitDefaults
  
  SaveGlobalStructures
  
' InitObjType cboDType

  InitColors cboDColor(DPOINT), DPOINT

  InitColors cboDColor(DLINE), DLINE
  InitLineStyle cboDLine(DLINE), DLINE
  
  InitColors cboDColor(DPOLY), DPOLY
  InitLineStyle cboDLine(DPOLY), DPOLY
  InitFillStyle cboDFill(DPOLY), DPOLY
  
  InitColors cboDColor(DCGRID), DCGRID
  InitLineStyle cboDLine(DCGRID), DCGRID
  txtDX(DCGRID) = defaults(DCGRID).x: txtDY(DCGRID) = defaults(DCGRID).y
  
  InitColors cboDColor(DPGRID), DPGRID
  InitLineStyle cboDLine(DPGRID), DPGRID
  txtDX(DPGRID) = defaults(DPGRID).x: txtDY(DPGRID) = defaults(DPGRID).y
  
  InitColors cboColor
  
  SSPanel1.Left = TabStrip1.ClientLeft + 0.5 * (TabStrip1.ClientWidth - SSPanel1.Width)
  SSPanel1.Top = TabStrip1.ClientTop + 0.5 * (TabStrip1.ClientHeight - SSPanel1.Height)
  SSPanel1.BevelOuter = 0
  SSPanel2.Move SSPanel1.Left, SSPanel1.Top
  SSPanel2.BevelOuter = 0
  
  IgnoreEvents = False
  
  TabStrip1.Tabs(1).Selected = True
  TabStrip1_Click
  
  For i = 1 To UBound(layers)
    If layers(i).filename <> "" Then
      lstLayers.AddItem layers(i).filename
    End If
  Next i
  If 0 < lstLayers.ListCount Then lstLayers.ListIndex = 0
  
  frmMain.ReDisplay = False
End Sub

Private Sub TabStrip1_Click()
Dim i As Integer
  SSPanel1.visible = TabStrip1.Tabs(2).Selected
  SSPanel2.visible = TabStrip1.Tabs(1).Selected
End Sub


Public Sub InitColors(cbo As ComboBox, Optional Index As Long = -1)
  ' the numbers are QBColor equivalents for DBCad dev
  ' the DBCad color = cbo.listindex+1
  cbo.Clear
  cbo.AddItem "Red": cbo.ItemData(cbo.NewIndex) = 12
  cbo.AddItem "Yellow": cbo.ItemData(cbo.NewIndex) = 14
  cbo.AddItem "Green": cbo.ItemData(cbo.NewIndex) = 10
  cbo.AddItem "Cyan": cbo.ItemData(cbo.NewIndex) = 11
  cbo.AddItem "Blue": cbo.ItemData(cbo.NewIndex) = 9
  cbo.AddItem "Magenta": cbo.ItemData(cbo.NewIndex) = 13
  cbo.AddItem "Black": cbo.ItemData(cbo.NewIndex) = 0
  cbo.AddItem "Dark Red": cbo.ItemData(cbo.NewIndex) = 4
  cbo.AddItem "Dark Yellow": cbo.ItemData(cbo.NewIndex) = 6
  cbo.AddItem "Dark Green": cbo.ItemData(cbo.NewIndex) = 2
  cbo.AddItem "Dark Cyan": cbo.ItemData(cbo.NewIndex) = 3
  cbo.AddItem "Dark Blue": cbo.ItemData(cbo.NewIndex) = 1
  cbo.AddItem "Dark Magenta": cbo.ItemData(cbo.NewIndex) = 5
  cbo.AddItem "White": cbo.ItemData(cbo.NewIndex) = 15
  If Index >= 0 Then cbo.ListIndex = defaults(Index).color - 1
End Sub

Public Sub InitLineStyle(cbo As ComboBox, Optional Index As Long = -1)
  cbo.Clear
  cbo.AddItem "1 Pixel Thick": cbo.ItemData(cbo.NewIndex) = 1
  cbo.AddItem "2 Pixels Thick": cbo.ItemData(cbo.NewIndex) = 2
  cbo.AddItem "3 Pixels Thick": cbo.ItemData(cbo.NewIndex) = 3
  cbo.AddItem "4 Pixels Thick": cbo.ItemData(cbo.NewIndex) = 4
  cbo.AddItem "Dashed": cbo.ItemData(cbo.NewIndex) = -1
  cbo.AddItem "Dotted": cbo.ItemData(cbo.NewIndex) = -2
  cbo.AddItem "Dash-Dot": cbo.ItemData(cbo.NewIndex) = -3
  cbo.AddItem "Dash-Dot-Dot": cbo.ItemData(cbo.NewIndex) = -4
  If Index >= 0 Then cbo.ListIndex = GetListIndex(cbo, defaults(Index).linetype)
End Sub

Public Sub InitFillStyle(cbo As ComboBox, Optional Index As Long = -1)
  cbo.Clear
  cbo.AddItem "Floodfill": cbo.ItemData(cbo.NewIndex) = 0
  cbo.AddItem "Raising Diagonal (/)": cbo.ItemData(cbo.NewIndex) = 1
  cbo.AddItem "Squares": cbo.ItemData(cbo.NewIndex) = 2
  cbo.AddItem "Inclined Squares": cbo.ItemData(cbo.NewIndex) = 3
  cbo.AddItem "Falling Diagonal (\)": cbo.ItemData(cbo.NewIndex) = 4
  cbo.AddItem "Horizontal": cbo.ItemData(cbo.NewIndex) = 5
  cbo.AddItem "Vertical": cbo.ItemData(cbo.NewIndex) = 6
  cbo.AddItem "No Fill": cbo.ItemData(cbo.NewIndex) = 7
  If Index >= 0 Then cbo.ListIndex = GetListIndex(cbo, defaults(Index).fill)
End Sub


'===========================================================
'  Section fro Map LayersTab
'===========================================================

Sub AddDefaultLayer(lname As String, idx As Integer, s1 As Integer, c1 As ColorConstants, Optional c2 As ColorConstants)
Dim directory As String
Dim filename As String
Dim extension As String
' If AddMapLayer(myMap.Layers, frmConfig.Text1(7) & lName) Then
'   With myMap.Layers(idx).Symbol
'     .Style = s1
'     .Color = c1
'     If .SymbolType = moFillSymbol Then
'       .Outline = True
'       .OutlineColor = c2
'     End If
'   End With
' End If
End Sub



Private Sub lstLayers_Click()
Dim myshow As Boolean
Dim i As Integer, layer As Long

  IgnoreEvents = True

  layerIndex = lstLayers.ListIndex + 1
  myLayer = layers(layerIndex)
  
  ConfigureWhat = IIf(myLayer.filename <> "", CONF_BACKGROUND, CONF_DATALAYERS)
  
  Select Case ConfigureWhat
    Case CONF_BACKGROUND:
      myshow = True
      chkVisible.visible = myshow
      picLayerColor.visible = myshow
      chkLock.visible = myshow
      lblColor.visible = myshow
      cboColor.visible = myshow
      chkLock.Value = myshow
    Case CONF_DATALAYERS:
      myshow = True
      chkVisible.visible = myshow
      picLayerColor.visible = myshow
      chkLock.visible = myshow
      lblColor.visible = myshow
      cboColor.visible = myshow
      chkLock.Value = myshow
  End Select

  IgnoreEvents = False

  chkVisible.Value = myLayer.visible
  chkLock.Value = myLayer.pickable
  cboColor.ListIndex = myLayer.color - 1
End Sub

Private Sub chkVisible_Click(Value As Integer)
  If IgnoreEvents Then Exit Sub
' If ConfigureWhat = CONF_BACKGROUND Then
'   bkgd(layerIndex).visible = chkVisible.value
' Else
    frmMain.ReDisplay = True
    layers(layerIndex).visible = chkVisible.Value
' End If
End Sub

Private Sub picLayerColor_Click()

On Error GoTo ErrLCHandler
  Dialog.CancelError = True
  Dialog.ShowColor
  picLayerColor.BackColor = Dialog.color
  layers(layerIndex).lcolor = picLayerColor.BackColor

ErrLCHandler:

End Sub


Private Sub picMapBackColor_Click()
On Error GoTo ErrBCHandler
  Dialog.CancelError = True
  Dialog.ShowColor
' picMapBackColor.BackColor = Dialog.color

ErrBCHandler:

End Sub


Function myAddLayer(lname As String) As Boolean
Dim l As Long
  myAddLayer = False
' lname = InputBox("Enter coverage name", "Add Coverage")
' lname = "unnamed"
  If lname <> "" Then
    l = 1 + UBound(layers)
    ReDim Preserve layers(l)
    layers(l).visible = True
    layers(l).name = lname
'   layers(l).objtype = cboDType ' "Polygon"
    layers(l).objtype = lname
    GetDefaults layers(l).objtype, layers(l).color, layers(l).linetype, layers(l).fill
    layers(l).lcolor = layers(l).color
    layers(l).bkgd = False
    layers(l).filename = ""
    myAddLayer = True
  End If
End Function

Private Sub tbrLayer_ButtonClick(ByVal Button As ComctlLib.Button)
Dim i As Integer
Dim lname As String
Dim l As Integer

  i = lstLayers.ListIndex
  Select Case Button.key
    Case "Add"
'     If myAddLayer() Then
'       UpdateLayersList UBound(layers) - 1, lstLayers, layers()
'     End If
    Case "AddFile"
      If AddLayer(l) Then
        frmMain.ReDisplay = True
        UpdateLayersList UBound(layers) - 1, lstLayers, layers()
      End If
    Case "Delete"
      If i >= 0 Then
'       frmMain.Dbcocx1.devgrcmddelete "L", layers(lstLayers.ListIndex + 1).name
        frmMain.ReDisplay = True
        For l = lstLayers.ListIndex + 1 To lstLayers.ListCount - 1
          layers(l) = layers(l + 1)
        Next
        l = UBound(layers)
        ReDim Preserve layers(l - 1)
        UpdateLayersList l, lstLayers, layers()
      End If
    Case "Up"
      If i > 0 Then
        frmMain.ReDisplay = True
        l = i + 1
        layers(0) = layers(l)
        layers(l) = layers(l - 1)
        layers(l - 1) = layers(0)
        UpdateLayersList i - 1, lstLayers, layers()
      End If
    Case "Down"
      If i < lstLayers.ListCount - 1 Then
        frmMain.ReDisplay = True
        l = i + 1
        layers(0) = layers(l)
        layers(l) = layers(l + 1)
        layers(l + 1) = layers(0)
        UpdateLayersList i + 1, lstLayers, layers()
      End If
  End Select
  
  Dim minx As Double, miny As Double, maxx As Double, maxy As Double
  If 1 <= UBound(layers) Then
    minx = layers(1).extminX: maxx = layers(1).extmaxX
    miny = layers(1).extminY: maxy = layers(1).extmaxY
    For i = 2 To UBound(layers)
      minx = IIf(layers(i).extminX < minx, layers(i).extminX, minx)
      miny = IIf(layers(i).extminY < miny, layers(i).extminY, miny)
      maxx = IIf(layers(i).extmaxX > maxx, layers(i).extmaxX, maxx)
      maxy = IIf(layers(i).extmaxY > maxy, layers(i).extmaxY, maxy)
    Next i
  Else
    minx = 0: maxx = 1000
    miny = 0: maxy = 1000
  End If
  Text1(0) = "minX: " & minx
  Text1(1) = "minY: " & miny
  Text2(0) = "maxX: " & maxx
  Text2(1) = "maxY: " & maxy
End Sub


Public Sub InitDefaults()
Dim i As Long

  ReDim layers(0)
  With layers(0)
    .filename = DBFileDir & Left(DBFileTitle, InStr(DBFileTitle, ".") - 1)
    .workarea = 1
  End With

  ReDim defaults(DPGRID)
  For i = DPOINT To DPGRID
    With defaults(i)
      Select Case i
        Case DPOINT:
          .objtype = TPOINT
          .color = 7
        Case DLINE:
          .objtype = TLINE
          .color = 7
          .linetype = 1
        Case DPOLY:
          .objtype = TPOLYGON
          .color = 7
          .linetype = 1
          .fill = 7
        Case DCGRID:
          .objtype = TCGRID
          .color = 7
          .linetype = -2
          .x = 10
          .y = 10
        Case DPGRID:
          .objtype = TPGRID
          .color = 7
          .linetype = -2
          .x = 8
          .y = 16
        Case DPTGRID:
          .objtype = TPTGRID
          .color = 7
          .linetype = -2
      End Select
    End With
  Next i

End Sub
Private Function GetListIndex(cbo As ComboBox, data As Long) As Long
Dim i As Integer
  For i = 0 To cbo.ListCount - 1
    If cbo.ItemData(i) = data Then
      GetListIndex = i
      Exit Function
    End If
  Next i
  GetListIndex = 0
End Function
Private Sub GetDefaults(otype As String, color As Long, line As Long, fill As Long)
Dim Index As Long

  Select Case otype
    Case TPOINT: Index = DPOINT
      color = cboDColor(Index).ItemData(cboDColor(Index).ListIndex)
    Case TLINE: Index = DLINE
      color = cboDColor(Index).ItemData(cboDColor(Index).ListIndex)
      line = cboDLine(Index).ItemData(cboDLine(Index).ListIndex)
    Case TPOLYGON: Index = DPOLY
      color = cboDColor(Index).ItemData(cboDColor(Index).ListIndex)
      line = cboDLine(Index).ItemData(cboDLine(Index).ListIndex)
      fill = cboDFill(Index).ItemData(cboDFill(Index).ListIndex)
  End Select
End Sub

Private Sub InitObjType(cbo As ComboBox)
  cbo.Clear
  cbo.AddItem TPOINT
  cbo.AddItem TLINE
  cbo.AddItem TPOLYGON
  cbo.ListIndex = 2
End Sub


Private Sub SaveGlobalStructures()
Dim i As Long
  ReDim tmpLayers(UBound(layers)) As LAYERSTRUCT
  For i = 0 To UBound(layers)
    tmpLayers(i) = layers(i)
  Next i
  ReDim tmpDefaults(UBound(defaults)) As DEFAULTSTRUCT
  For i = 0 To UBound(defaults)
    tmpDefaults(i) = defaults(i)
  Next i
End Sub

Private Sub RestoreGlobalStructures()
Dim i As Long
  ReDim layers(UBound(tmpLayers))
  For i = 0 To UBound(tmpLayers)
    layers(i) = tmpLayers(i)
  Next i
  ReDim defaults(UBound(tmpDefaults))
  For i = 0 To UBound(tmpDefaults)
    defaults(i) = tmpDefaults(i)
  Next i
End Sub

Private Sub txtDX_Change(Index As Integer)
  If IgnoreEvents Then Exit Sub
  If Val(txtDX(Index)) > 0 Then defaults(Index).x = Val(txtDX(Index))
End Sub

Private Sub txtDY_Change(Index As Integer)
  If IgnoreEvents Then Exit Sub
  If Val(txtDY(Index)) > 0 Then defaults(Index).y = Val(txtDY(Index))
End Sub

