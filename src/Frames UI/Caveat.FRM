VERSION 5.00
Begin VB.Form frmCaveat 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "FRAMES Notice"
   ClientHeight    =   5760
   ClientLeft      =   1860
   ClientTop       =   1320
   ClientWidth     =   7680
   Icon            =   "Caveat.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   384
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   512
   StartUpPosition =   2  'CenterScreen
   Begin VB.CheckBox Check1 
      Caption         =   "Don't show this notice again."
      Height          =   195
      Left            =   288
      TabIndex        =   2
      Top             =   5376
      Width           =   3120
   End
   Begin VB.CommandButton Command1 
      Caption         =   "OK"
      Default         =   -1  'True
      Height          =   360
      Left            =   6336
      TabIndex        =   1
      Top             =   5280
      Width           =   1260
   End
   Begin VB.TextBox Text1 
      Height          =   5172
      Left            =   24
      Locked          =   -1  'True
      MultiLine       =   -1  'True
      TabIndex        =   0
      Top             =   24
      Width           =   7632
   End
End
Attribute VB_Name = "frmCaveat"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub Command1_Click()
  Unload Me
End Sub

Private Sub Form_load()
Dim fnum As Long
Dim fname As String
Dim txt As String
  
  fname = App.path & "\caveat.txt"
  fnum = FreeFile
  Open fname For Input As #fnum
  Text1 = Input(FileLen(fname), #fnum)
  Close #fnum
End Sub

Private Sub Form_Unload(Cancel As Integer)
  WriteIniString "Options", "ShowNotice", 0 = Check1.value
End Sub
