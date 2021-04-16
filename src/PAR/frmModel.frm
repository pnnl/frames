VERSION 5.00
Begin VB.Form frmModel 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Select Module"
   ClientHeight    =   2100
   ClientLeft      =   45
   ClientTop       =   435
   ClientWidth     =   8055
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   2100
   ScaleWidth      =   8055
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton Command1 
      Caption         =   "&Cancel"
      Default         =   -1  'True
      Height          =   375
      Index           =   1
      Left            =   6360
      TabIndex        =   5
      Top             =   1440
      Width           =   1455
   End
   Begin VB.CommandButton Command1 
      Caption         =   "&OK"
      Height          =   375
      Index           =   0
      Left            =   4800
      TabIndex        =   4
      Top             =   1440
      Width           =   1455
   End
   Begin VB.ComboBox Combo1 
      Height          =   315
      Left            =   2400
      Style           =   2  'Dropdown List
      TabIndex        =   0
      Top             =   840
      Width           =   5415
   End
   Begin VB.Label lblGlyph 
      AutoSize        =   -1  'True
      Caption         =   "Label4"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   195
      Left            =   2400
      TabIndex        =   8
      Top             =   180
      Width           =   585
   End
   Begin VB.Label Label4 
      AutoSize        =   -1  'True
      Caption         =   "Icon:"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   240
      Left            =   375
      TabIndex        =   7
      Top             =   180
      Width           =   510
   End
   Begin VB.Label lblModel 
      Caption         =   "Label4"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   300
      Left            =   2400
      TabIndex        =   6
      Top             =   532
      Width           =   5310
   End
   Begin VB.Label Label3 
      AutoSize        =   -1  'True
      Caption         =   "Model 2.0"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   375
      TabIndex        =   3
      Top             =   885
      Width           =   1065
   End
   Begin VB.Label Label2 
      Caption         =   "Label2"
      Height          =   135
      Left            =   4680
      TabIndex        =   2
      Top             =   3000
      Width           =   15
   End
   Begin VB.Label Label1 
      AutoSize        =   -1  'True
      Caption         =   "Model 1.x:"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   240
      Left            =   375
      TabIndex        =   1
      Top             =   532
      Width           =   1065
   End
End
Attribute VB_Name = "frmModel"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Private Sub Command1_Click(index As Integer)
  model = ""
  If index = 0 Then
    If Combo1.ListIndex > 0 Then model = Combo1.list(Combo1.ListIndex)
  End If
  Unload Me
End Sub

Private Sub Form_Load()
  Dim i As Long
  lblGlyph.Caption = curGlyph.Label & " (" & curGlyph.Name & ")"
  lblModel.Caption = model
  Combo1.Clear
  Dim obj As Variant
  Combo1.AddItem "<undefined>"
  For Each obj In colModules
    Combo1.AddItem CStr(obj)
  Next obj
  Combo1.ListIndex = 0
End Sub
