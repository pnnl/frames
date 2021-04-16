VERSION 5.00
Begin VB.Form Login 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Login"
   ClientHeight    =   1560
   ClientLeft      =   7470
   ClientTop       =   6930
   ClientWidth     =   4005
   ControlBox      =   0   'False
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   1560
   ScaleWidth      =   4005
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.TextBox txtUserName 
      Height          =   345
      Left            =   1400
      TabIndex        =   1
      Top             =   135
      Width           =   2500
   End
   Begin VB.CommandButton cmdOK 
      Caption         =   "OK"
      Default         =   -1  'True
      Height          =   390
      Left            =   1440
      TabIndex        =   4
      Top             =   1080
      Width           =   1000
   End
   Begin VB.CommandButton cmdCancel 
      Cancel          =   -1  'True
      Caption         =   "Cancel"
      Height          =   390
      Left            =   2880
      TabIndex        =   5
      Top             =   1080
      Width           =   1000
   End
   Begin VB.TextBox txtPassword 
      Height          =   345
      IMEMode         =   3  'DISABLE
      Left            =   1400
      PasswordChar    =   "*"
      TabIndex        =   3
      Top             =   525
      Width           =   2500
   End
   Begin VB.Label lblLabels 
      Caption         =   "&User Name:"
      Height          =   270
      Index           =   0
      Left            =   120
      TabIndex        =   0
      Top             =   150
      Width           =   1305
   End
   Begin VB.Label lblLabels 
      Caption         =   "&Password:"
      Height          =   270
      Index           =   1
      Left            =   105
      TabIndex        =   2
      Top             =   540
      Width           =   1300
   End
End
Attribute VB_Name = "Login"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
'These variables are defined in General.bas
'Global LoginSucceeded As Boolean
'Global usrname As String
'Global pssword As String

Private Sub cmdCancel_Click()
  Unload Me
End Sub

Private Sub cmdOK_Click()
  If txtUserName.Text <> "" Then LoginSucceeded = True
  usrname = txtUserName.Text
  pssword = txtPassword.Text
  Unload Me
End Sub

Private Sub Form_Load()
  LoginSucceeded = False
  txtUserName.Text = usrname
  txtPassword.Text = pssword
End Sub
