VERSION 5.00
Begin VB.Form Load 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Loading Please Wait..."
   ClientHeight    =   24
   ClientLeft      =   7632
   ClientTop       =   8928
   ClientWidth     =   3984
   ClipControls    =   0   'False
   ControlBox      =   0   'False
   Icon            =   "Load.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   NegotiateMenus  =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   24
   ScaleWidth      =   3984
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton Command1 
      Caption         =   "Continue"
      Height          =   372
      Left            =   120
      TabIndex        =   1
      Top             =   2640
      Width           =   3732
   End
   Begin VB.ListBox List1 
      Height          =   2424
      Left            =   120
      Style           =   1  'Checkbox
      TabIndex        =   0
      Top             =   120
      Width           =   3732
   End
End
Attribute VB_Name = "Load"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub Command1_Click()
  Hide
End Sub
