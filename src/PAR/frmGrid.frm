VERSION 5.00
Begin VB.Form frmGrid 
   Caption         =   "Cartesian Grid"
   ClientHeight    =   4215
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   4710
   LinkTopic       =   "Form1"
   ScaleHeight     =   4215
   ScaleWidth      =   4710
   StartUpPosition =   2  'CenterScreen
   Begin VB.ComboBox cboDeltaY 
      Height          =   315
      Left            =   2820
      TabIndex        =   11
      Text            =   "Combo1"
      Top             =   2670
      Width           =   1005
   End
   Begin VB.ComboBox Combo1 
      Height          =   315
      Left            =   2850
      TabIndex        =   10
      Text            =   "Combo1"
      Top             =   1620
      Width           =   1005
   End
   Begin VB.TextBox txtDeltaY 
      Height          =   285
      Left            =   1530
      TabIndex        =   9
      Top             =   2685
      Width           =   1305
   End
   Begin VB.TextBox txtNumY 
      Height          =   285
      Left            =   1530
      Locked          =   -1  'True
      TabIndex        =   8
      Top             =   2265
      Width           =   1305
   End
   Begin VB.TextBox txtDeltaX 
      Height          =   285
      Left            =   1545
      TabIndex        =   7
      Top             =   1620
      Width           =   1305
   End
   Begin VB.TextBox txtNumX 
      Height          =   285
      Left            =   1545
      Locked          =   -1  'True
      TabIndex        =   6
      Top             =   1230
      Width           =   1305
   End
   Begin VB.CommandButton Command2 
      Caption         =   "Cancel"
      Height          =   360
      Left            =   1920
      TabIndex        =   1
      Top             =   3495
      Width           =   1215
   End
   Begin VB.CommandButton Command1 
      Caption         =   "OK"
      Height          =   360
      Left            =   3255
      TabIndex        =   0
      Top             =   3495
      Width           =   1215
   End
   Begin VB.Label lblXOrig 
      AutoSize        =   -1  'True
      Caption         =   "X Origin:"
      Height          =   195
      Left            =   405
      TabIndex        =   13
      Top             =   390
      Width           =   600
   End
   Begin VB.Label lblYOrig 
      AutoSize        =   -1  'True
      Caption         =   "Y Origin:"
      Height          =   195
      Left            =   405
      TabIndex        =   12
      Top             =   750
      Width           =   600
   End
   Begin VB.Label lblDeltaY 
      AutoSize        =   -1  'True
      Caption         =   "Change in Y:"
      Height          =   195
      Left            =   375
      TabIndex        =   5
      Top             =   2760
      Width           =   915
   End
   Begin VB.Label Label3 
      AutoSize        =   -1  'True
      Caption         =   "Change in X:"
      Height          =   195
      Left            =   375
      TabIndex        =   4
      Top             =   1650
      Width           =   915
   End
   Begin VB.Label lblNumY 
      AutoSize        =   -1  'True
      Caption         =   "Y distances:"
      Height          =   195
      Left            =   375
      TabIndex        =   3
      Top             =   2340
      Width           =   870
   End
   Begin VB.Label lblNumX 
      AutoSize        =   -1  'True
      Caption         =   "X distances:"
      Height          =   195
      Left            =   390
      TabIndex        =   2
      Top             =   1275
      Width           =   870
   End
End
Attribute VB_Name = "frmGrid"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Public exitstate

Private Sub Command1_Click()
  If 0 < InStr(title, "Cartesian") Then
    If 0 = Val(txtDeltaX) Or 0 = Val(txtDeltaY) Then
      Beep
      Exit Sub
    End If
  Else
    If 0 = Val(txtDeltaX) Then
      Beep
      Exit Sub
    End If
  End If
  exitstate = SUCCESS
  Me.Hide
End Sub

Private Sub Command2_Click()
  exitstate = FAILURE
  Me.Hide
End Sub

Public Sub InitCartesian(numX As Long, numY As Long)
  Caption = "Cartesian Grid"
  lblNumX = "X distances:": txtNumX = numX
  lblNumY = "Y distances:": txtNumY = numY
End Sub

Public Sub InitPolar(numSector As Long, numDir As Long)
  Caption = "Polar Grid"
  lblNumX = "Sectors:": txtNumX = numSector
  lblNumY = "Directions:": txtNumY = numDir
  lblDeltaX = "Change in Radius:"
  lblDeltaY.visible = False
  txtDeltaY.visible = False
  cboDeltaY.visible = False
End Sub
Public Sub GetCartesianData(ByRef dx As Long, ByRef dy As Long)
  dx = Val(txtDeltaX)
  dy = Val(txtDeltaY)
End Sub
Public Sub GetPolarData(ByRef dx As Long)
  dx = Val(txtDeltaX)
End Sub
