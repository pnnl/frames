VERSION 5.00
Object = "{3B7C8863-D78F-101B-B9B5-04021C009402}#1.2#0"; "RICHTX32.OCX"
Begin VB.Form frmComment 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Project Description"
   ClientHeight    =   5760
   ClientLeft      =   1860
   ClientTop       =   1320
   ClientWidth     =   7680
   Icon            =   "Comment.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   480
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   640
   StartUpPosition =   2  'CenterScreen
   Begin RichTextLib.RichTextBox RichText 
      Height          =   5175
      Left            =   0
      TabIndex        =   1
      Top             =   0
      Width           =   7695
      _ExtentX        =   13568
      _ExtentY        =   9123
      _Version        =   393217
      ScrollBars      =   3
      AutoVerbMenu    =   -1  'True
      TextRTF         =   $"Comment.frx":030A
   End
   Begin VB.CommandButton Command1 
      Caption         =   "OK"
      Default         =   -1  'True
      Height          =   360
      Left            =   6336
      TabIndex        =   0
      Top             =   5280
      Width           =   1260
   End
End
Attribute VB_Name = "frmComment"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim fname As String

Private Sub Command1_Click()
  Unload Me
End Sub

Private Sub Form_load()
  fname = LongGidTitle & DOT_DSC
  If Dir(fname) <> "" Then
    RichText.LoadFile fname, 1
  End If
  Me.Caption = "Project Description for file:  " & fname
End Sub

Private Sub Form_Unload(Cancel As Integer)
  RichText.SaveFile fname, 1
End Sub
