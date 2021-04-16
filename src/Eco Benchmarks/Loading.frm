VERSION 5.00
Object = "{6B7E6392-850A-101B-AFC0-4210102A8DA7}#1.3#0"; "comctl32.ocx"
Begin VB.Form Loading 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Loading Data"
   ClientHeight    =   1230
   ClientLeft      =   7875
   ClientTop       =   6480
   ClientWidth     =   3810
   Icon            =   "Loading.frx":0000
   LinkTopic       =   "Form2"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   1230
   ScaleWidth      =   3810
   StartUpPosition =   2  'CenterScreen
   Begin VB.Timer Timer1 
      Enabled         =   0   'False
      Interval        =   500
      Left            =   2640
      Top             =   1080
   End
   Begin ComctlLib.ProgressBar ProgressBar1 
      Height          =   375
      Left            =   120
      TabIndex        =   1
      Top             =   720
      Width           =   3495
      _ExtentX        =   6165
      _ExtentY        =   661
      _Version        =   327682
      Appearance      =   1
      Max             =   150
   End
   Begin VB.Label Label1 
      Caption         =   "Label1"
      Height          =   375
      Left            =   120
      TabIndex        =   0
      Top             =   120
      Width           =   3495
   End
End
Attribute VB_Name = "Loading"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub Form_load()
  Timer1.Enabled = True
End Sub

Private Sub Form_Unload(Cancel As Integer)
  Timer1.Enabled = False
End Sub

Private Sub Timer1_Timer()
  If ProgressBar1.value = 150 Then
    ProgressBar1.value = 0
  Else
    ProgressBar1.value = ProgressBar1.value + 10
  End If
  DoEvents
End Sub
