VERSION 5.00
Begin VB.Form AddKey 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Add Selection Key"
   ClientHeight    =   1560
   ClientLeft      =   6765
   ClientTop       =   6105
   ClientWidth     =   4005
   ControlBox      =   0   'False
   Icon            =   "AddKey.frx":0000
   LinkTopic       =   "Form3"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   1560
   ScaleWidth      =   4005
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.TextBox Text1 
      Height          =   345
      Index           =   1
      Left            =   1400
      TabIndex        =   3
      Top             =   525
      Width           =   2500
   End
   Begin VB.TextBox Text1 
      Height          =   345
      Index           =   0
      Left            =   1400
      TabIndex        =   1
      Top             =   135
      Width           =   2500
   End
   Begin VB.CommandButton cmdCancel 
      Caption         =   "Cancel"
      Height          =   390
      Left            =   2880
      TabIndex        =   5
      Top             =   1080
      Width           =   1000
   End
   Begin VB.CommandButton cmdOK 
      Caption         =   "OK"
      Height          =   390
      Left            =   1440
      TabIndex        =   4
      Top             =   1080
      Width           =   1000
   End
   Begin VB.Label Label1 
      Caption         =   "Common Name"
      Height          =   270
      Index           =   1
      Left            =   105
      TabIndex        =   2
      Top             =   540
      Width           =   1300
   End
   Begin VB.Label Label1 
      Caption         =   "Scientific Name"
      Height          =   270
      Index           =   0
      Left            =   105
      TabIndex        =   0
      Top             =   150
      Width           =   1300
   End
End
Attribute VB_Name = "AddKey"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
'These variables are defined in General.bas
'Global AddKeySucceeded As Boolean
'Global keyId As String
'Global keyName As String

Public Sub SetAddKeyLabels(lblTitle As String, lblId As String, lblName As String)
  Me.Caption = lblTitle
  Label1(0).Caption = lblId
  Label1(1).Caption = lblName
End Sub

Private Sub cmdCancel_Click()
  Unload Me
End Sub

Private Sub cmdOK_Click()
  If Text1(0).Text <> "" Then AddKeySucceeded = True
  keyId = Text1(0).Text
  If Text1(1).Text = "" Then
    KeyName = Text1(0).Text
  Else
    KeyName = Text1(1).Text
  End If
  Unload Me
End Sub

Private Sub Form_load()
  AddKeySucceeded = False
  Text1(0).Text = keyId
  Text1(1).Text = KeyName
End Sub
