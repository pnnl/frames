VERSION 5.00
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "THREED32.OCX"
Begin VB.Form frmData 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Form2"
   ClientHeight    =   3180
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   6870
   ControlBox      =   0   'False
   LinkTopic       =   "Form2"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3180
   ScaleWidth      =   6870
   StartUpPosition =   2  'CenterScreen
   Begin Threed.SSPanel SSPanel1 
      Height          =   2985
      Index           =   1
      Left            =   60
      TabIndex        =   0
      Top             =   75
      Width           =   6705
      _Version        =   65536
      _ExtentX        =   11827
      _ExtentY        =   5265
      _StockProps     =   15
      BackColor       =   13160660
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BevelOuter      =   1
      Begin VB.CommandButton Command2 
         Caption         =   "Cancel"
         Height          =   315
         Left            =   5160
         TabIndex        =   20
         Top             =   240
         Width           =   1155
      End
      Begin VB.CommandButton Command1 
         Caption         =   "OK"
         Height          =   315
         Left            =   5160
         TabIndex        =   19
         Top             =   720
         Width           =   1155
      End
      Begin VB.TextBox txtThick 
         Height          =   285
         Left            =   1800
         TabIndex        =   12
         Top             =   588
         Width           =   1725
      End
      Begin VB.ComboBox cboThickUnit 
         Height          =   315
         Left            =   3600
         Style           =   2  'Dropdown List
         TabIndex        =   11
         Top             =   588
         Width           =   1275
      End
      Begin VB.TextBox txtOffset 
         Height          =   285
         Left            =   1800
         TabIndex        =   10
         Top             =   135
         Width           =   1725
      End
      Begin VB.ComboBox cboOffsetUnit 
         Height          =   315
         Left            =   3600
         Style           =   2  'Dropdown List
         TabIndex        =   9
         Top             =   135
         Width           =   1275
      End
      Begin VB.TextBox txtArea 
         Enabled         =   0   'False
         Height          =   285
         Left            =   1800
         TabIndex        =   8
         Top             =   1041
         Width           =   1725
      End
      Begin VB.TextBox txtVolume 
         Enabled         =   0   'False
         Height          =   285
         Left            =   1800
         TabIndex        =   7
         Top             =   1494
         Width           =   1725
      End
      Begin VB.TextBox txtCentroidX 
         Enabled         =   0   'False
         Height          =   285
         Left            =   1800
         TabIndex        =   6
         Top             =   2400
         Width           =   1725
      End
      Begin VB.TextBox txtLength 
         Enabled         =   0   'False
         Height          =   285
         Left            =   1800
         TabIndex        =   5
         Top             =   1947
         Width           =   1725
      End
      Begin VB.ComboBox Combo1 
         Enabled         =   0   'False
         Height          =   315
         Left            =   3600
         Style           =   2  'Dropdown List
         TabIndex        =   4
         Top             =   1041
         Width           =   1275
      End
      Begin VB.ComboBox Combo2 
         Enabled         =   0   'False
         Height          =   315
         Left            =   3600
         Style           =   2  'Dropdown List
         TabIndex        =   3
         Top             =   1494
         Width           =   1275
      End
      Begin VB.ComboBox Combo3 
         Enabled         =   0   'False
         Height          =   315
         Left            =   3600
         Style           =   2  'Dropdown List
         TabIndex        =   2
         Top             =   1947
         Width           =   1275
      End
      Begin VB.TextBox txtCentroidY 
         Enabled         =   0   'False
         Height          =   270
         Left            =   3600
         TabIndex        =   1
         Top             =   2400
         Width           =   1275
      End
      Begin VB.Label Label1 
         AutoSize        =   -1  'True
         Caption         =   "Thickness"
         Height          =   195
         Left            =   195
         TabIndex        =   18
         Top             =   672
         Width           =   735
      End
      Begin VB.Label Label2 
         AutoSize        =   -1  'True
         Caption         =   "Offset from grade (Z)"
         Height          =   195
         Left            =   180
         TabIndex        =   17
         Top             =   240
         Width           =   1455
      End
      Begin VB.Label Label6 
         AutoSize        =   -1  'True
         Caption         =   "Area"
         Height          =   195
         Left            =   195
         TabIndex        =   16
         Top             =   1104
         Width           =   330
      End
      Begin VB.Label Label7 
         AutoSize        =   -1  'True
         Caption         =   "Volume"
         Height          =   195
         Left            =   240
         TabIndex        =   15
         Top             =   1536
         Width           =   525
      End
      Begin VB.Label Label9 
         AutoSize        =   -1  'True
         Caption         =   "Centroid"
         Height          =   195
         Left            =   240
         TabIndex        =   14
         Top             =   2400
         Width           =   585
      End
      Begin VB.Label lblLength 
         AutoSize        =   -1  'True
         Caption         =   "Perimeter/Length"
         Height          =   195
         Left            =   240
         TabIndex        =   13
         Top             =   1968
         Width           =   1230
      End
   End
End
Attribute VB_Name = "frmData"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim dbopened As Boolean

Private Sub Command1_Click()
  frmMain.Dbcocx1.devseek "V" & SelectedEntityId
  If 0 = frmMain.Dbcocx1.devfound() Then
    MsgBox "Entity not found"
    Unload Me
  End If
  frmMain.Dbcocx1.devsetfield "VX1", txtCentroidX
  frmMain.Dbcocx1.devsetfield "VY1", txtCentroidY
  frmMain.Dbcocx1.devsetfield "VX2", txtOffset
  frmMain.Dbcocx1.devsetfield "VY2", txtThick
  Unload Me
End Sub

Private Sub Command2_Click()
  Unload Me
End Sub

Private Sub Form_Load()
Dim dbopened As Boolean
Dim rad As Double

  SelectDbgr SelectedLayer, True

  Caption = "Data for EntityId " & SelectedEntityId
' frmMain.Dbcocx1.devselect layers(SelectedLayer).workarea

  frmMain.Dbcocx1.devgrcmdpolycalc SelectedEntityId, 2, 0, 0

  frmMain.Dbcocx1.devseek "E" & Trim(SelectedEntityId)
  If 0 = frmMain.Dbcocx1.devfound() Then
    MsgBox "Entity not found"
    Exit Sub
  End If
  
  rad = Val(frmMain.Dbcocx1.devgetfield("RAD"))
  If rad > 0 Then
    ' this is a circle
    txtArea = Format(3.14159 * (rad ^ 2), "Fixed")
    txtLength = Format(3.14159 * (rad * 2), "fixed")
  Else
    txtArea = Format(Val(frmMain.Dbcocx1.devgetfield("VY2")), "fixed")
    txtLength = Format(Val(frmMain.Dbcocx1.devgetfield("VX2")), "fixed")
  End If
  
  frmMain.Dbcocx1.devseek "V" & SelectedEntityId
  
  If 0 <> frmMain.Dbcocx1.devfound() Then
    txtCentroidX = Val(frmMain.Dbcocx1.devgetfield("VX1"))
    txtCentroidY = Val(frmMain.Dbcocx1.devgetfield("VY1"))
    txtOffset = Val(frmMain.Dbcocx1.devgetfield("VX2"))
    txtThick = Val(frmMain.Dbcocx1.devgetfield("VY2"))
  End If
  
End Sub

Private Sub Form_Unload(Cancel As Integer)
' If dbopened Then CloseDbgr
' dbopened = False
End Sub

Private Sub txtThick_Change()
  txtVolume = Val(txtArea) * Val(txtThick)
End Sub
