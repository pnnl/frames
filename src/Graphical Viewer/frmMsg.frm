VERSION 5.00
Begin VB.Form frmMsg 
   Caption         =   "FRAMES Graphical Viewer"
   ClientHeight    =   2952
   ClientLeft      =   2376
   ClientTop       =   2400
   ClientWidth     =   5412
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   2952
   ScaleWidth      =   5412
   WhatsThisButton =   -1  'True
   WhatsThisHelp   =   -1  'True
   Begin VB.PictureBox Picture1 
      BackColor       =   &H00FFFFFF&
      Height          =   2712
      Left            =   120
      Picture         =   "frmMsg.frx":0000
      ScaleHeight     =   2664
      ScaleWidth      =   3684
      TabIndex        =   1
      Top             =   120
      Width           =   3735
      Begin VB.TextBox txtMsg 
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         Height          =   1812
         Left            =   120
         MultiLine       =   -1  'True
         ScrollBars      =   2  'Vertical
         TabIndex        =   3
         Text            =   "frmMsg.frx":030A
         Top             =   720
         Width           =   3492
      End
      Begin VB.Label lblMsg 
         BackColor       =   &H00FFFFFF&
         Caption         =   "Did you know..."
         Height          =   492
         Left            =   540
         TabIndex        =   2
         Top             =   180
         Width           =   2652
      End
   End
   Begin VB.CommandButton cmdOK 
      Cancel          =   -1  'True
      Caption         =   "OK"
      Default         =   -1  'True
      Height          =   375
      Left            =   4080
      TabIndex        =   0
      Top             =   120
      Width           =   1215
   End
End
Attribute VB_Name = "frmMsg"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Private Sub cmdOK_Click()
  Me.Hide
End Sub

Private Sub Form_Load()
    
    ReadIniString "Options", "ShowViewerMessages", "True", ShowMsg
    If ShowMsg = "False" Then
        Me.Hide
        Exit Sub
    End If
End Sub
