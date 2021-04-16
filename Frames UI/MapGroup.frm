VERSION 5.00
Begin VB.Form frmMapGroup 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Unrecognized Module Grouping"
   ClientHeight    =   3072
   ClientLeft      =   2760
   ClientTop       =   3756
   ClientWidth     =   6036
   ControlBox      =   0   'False
   Icon            =   "MapGroup.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3072
   ScaleWidth      =   6036
   ShowInTaskbar   =   0   'False
   Begin VB.TextBox Text3 
      Enabled         =   0   'False
      Height          =   285
      Left            =   1440
      TabIndex        =   7
      Text            =   "Text3"
      Top             =   1920
      Width           =   3000
   End
   Begin VB.TextBox Text2 
      Enabled         =   0   'False
      Height          =   285
      Left            =   1440
      TabIndex        =   6
      Text            =   "Text2"
      Top             =   1560
      Width           =   3000
   End
   Begin VB.TextBox Text1 
      Enabled         =   0   'False
      Height          =   285
      Left            =   1440
      TabIndex        =   5
      Text            =   "Text1"
      Top             =   1200
      Width           =   3000
   End
   Begin VB.ComboBox Combo1 
      Height          =   315
      Left            =   1440
      TabIndex        =   1
      Text            =   "Combo1"
      Top             =   2400
      Width           =   3015
   End
   Begin VB.CommandButton OKButton 
      Caption         =   "OK"
      Height          =   375
      Left            =   4680
      TabIndex        =   0
      Top             =   120
      Width           =   1215
   End
   Begin VB.Label Label5 
      Caption         =   "Group"
      Height          =   285
      Left            =   360
      TabIndex        =   9
      Top             =   2400
      Width           =   1005
   End
   Begin VB.Label Label4 
      Caption         =   $"MapGroup.frx":030A
      Height          =   735
      Left            =   240
      TabIndex        =   8
      Top             =   240
      Width           =   4215
   End
   Begin VB.Label Label3 
      Caption         =   "Label"
      Height          =   285
      Left            =   360
      TabIndex        =   4
      Top             =   1920
      Width           =   1005
   End
   Begin VB.Label Label2 
      Caption         =   "Model"
      Height          =   285
      Left            =   360
      TabIndex        =   3
      Top             =   1560
      Width           =   1005
   End
   Begin VB.Label Label1 
      Caption         =   "Id"
      Height          =   285
      Left            =   360
      TabIndex        =   2
      Top             =   1200
      Width           =   1005
   End
End
Attribute VB_Name = "frmMapGroup"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Option Explicit

Public Sub PrepMapGroup(Id As String, Model As String, Label As String)
 Text1.Text = Id
 Text2.Text = Model
 Text3.Text = Label
End Sub

Private Sub Form_load()
Dim i As Long
  
  For i = 1 To GroupCount - 1
    Combo1.AddItem Group(i).Name & "(" & Group(i).Prefix & ")"
  Next
  If Combo1.ListCount Then Combo1.ListIndex = 0
End Sub

Private Sub OKButton_Click()
  frmgrpIdx = Combo1.ListIndex + 1
  Unload Me
End Sub
