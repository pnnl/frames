VERSION 5.00
Begin VB.Form frmGrView 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Graphic Window View"
   ClientHeight    =   3195
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   4680
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3195
   ScaleWidth      =   4680
   ShowInTaskbar   =   0   'False
   StartUpPosition =   3  'Windows Default
   Begin VB.Label Label1 
      Caption         =   "Label1"
      Height          =   270
      Index           =   5
      Left            =   360
      TabIndex        =   5
      Top             =   1860
      Width           =   2850
   End
   Begin VB.Label Label1 
      Caption         =   "Label1"
      Height          =   270
      Index           =   4
      Left            =   315
      TabIndex        =   4
      Top             =   1515
      Width           =   2850
   End
   Begin VB.Label Label1 
      Caption         =   "Label1"
      Height          =   270
      Index           =   3
      Left            =   330
      TabIndex        =   3
      Top             =   1245
      Width           =   2850
   End
   Begin VB.Label Label1 
      Caption         =   "Label1"
      Height          =   270
      Index           =   2
      Left            =   345
      TabIndex        =   2
      Top             =   930
      Width           =   2850
   End
   Begin VB.Label Label1 
      Caption         =   "Label1"
      Height          =   270
      Index           =   1
      Left            =   330
      TabIndex        =   1
      Top             =   615
      Width           =   2850
   End
   Begin VB.Label Label1 
      Caption         =   "Label1"
      Height          =   270
      Index           =   0
      Left            =   360
      TabIndex        =   0
      Top             =   330
      Width           =   2850
   End
End
Attribute VB_Name = "frmGrView"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Public Sub GetView()
  Label1(0) = "X0: " & frmMain.ViewX0
  Label1(1) = "Y0: " & frmMain.ViewY0
  Label1(2) = "X1: " & frmMain.ViewX1
  Label1(3) = "Y1: " & frmMain.ViewY1
  Label1(4) = "Zoom Factor: " & frmMain.ViewZf
  Label1(5) = "Regen Factor: " & frmMain.ViewRf
End Sub

