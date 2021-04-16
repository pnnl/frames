VERSION 5.00
Begin VB.Form StatusForm 
   Appearance      =   0  'Flat
   BackColor       =   &H000000C0&
   Caption         =   "Model Results"
   ClientHeight    =   2496
   ClientLeft      =   36
   ClientTop       =   2556
   ClientWidth     =   5268
   ControlBox      =   0   'False
   BeginProperty Font 
      Name            =   "MS Sans Serif"
      Size            =   7.8
      Charset         =   0
      Weight          =   700
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   ForeColor       =   &H80000008&
   HelpContextID   =   5000
   LinkTopic       =   "Form1"
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   2496
   ScaleWidth      =   5268
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton CloseButton 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Cancel          =   -1  'True
      Caption         =   "Close"
      Height          =   495
      Left            =   360
      TabIndex        =   1
      Top             =   6000
      Width           =   1215
   End
   Begin VB.TextBox StatusText 
      Appearance      =   0  'Flat
      BeginProperty Font 
         Name            =   "Courier"
         Size            =   9.6
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   2175
      Left            =   240
      MultiLine       =   -1  'True
      ScrollBars      =   3  'Both
      TabIndex        =   0
      Top             =   120
      Width           =   4815
   End
   Begin VB.Menu CloseMenu 
      Caption         =   "&Close"
   End
End
Attribute VB_Name = "StatusForm"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Private Sub CloseButton_Click()
    CloseMenu_click
End Sub

Private Sub CloseMenu_click()
    StatusTitle ""
    Hide
End Sub

Private Sub Form_Resize()
    On Local Error Resume Next
    StatusText.Height = Height - 700 - 2 * StatusText.Top
    StatusText.Width = Width - 100 - 2 * StatusText.Left
    CloseButton.Top = Height + 100
End Sub

