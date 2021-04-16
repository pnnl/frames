VERSION 5.00
Begin VB.Form FileAdd 
   BorderStyle     =   4  'Fixed ToolWindow
   Caption         =   "Add File Type"
   ClientHeight    =   1584
   ClientLeft      =   48
   ClientTop       =   216
   ClientWidth     =   4932
   Icon            =   "FileAdd.frx":0000
   LinkTopic       =   "Form2"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   1584
   ScaleWidth      =   4932
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.ComboBox Combo1 
      Height          =   288
      Index           =   1
      Left            =   1300
      TabIndex        =   5
      Text            =   "Combo1"
      Top             =   600
      Width           =   3500
   End
   Begin VB.ComboBox Combo1 
      Height          =   288
      Index           =   0
      Left            =   1300
      TabIndex        =   4
      Text            =   "Combo1"
      Top             =   120
      Width           =   3500
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Cancel"
      Height          =   372
      Index           =   1
      Left            =   3840
      TabIndex        =   1
      Top             =   1080
      Width           =   972
   End
   Begin VB.CommandButton Command1 
      Caption         =   "OK"
      Height          =   372
      Index           =   0
      Left            =   2760
      TabIndex        =   0
      Top             =   1080
      Width           =   972
   End
   Begin VB.Label Label1 
      AutoSize        =   -1  'True
      Caption         =   "File Qualifier"
      Height          =   192
      Index           =   1
      Left            =   120
      TabIndex        =   3
      Top             =   600
      Width           =   888
   End
   Begin VB.Label Label1 
      AutoSize        =   -1  'True
      Caption         =   "File Extension"
      Height          =   192
      Index           =   0
      Left            =   120
      TabIndex        =   2
      Top             =   120
      Width           =   996
   End
End
Attribute VB_Name = "FileAdd"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Compare Text

Private Sub Command1_Click(Index As Integer)
  Dim errmsg As String
  fileaddcheck = False
  If Index = 0 Then
    If Combo1(0).Text = "" Then
      errmsg = "Please select or enter a file extension"
      MsgBox errmsg, vbExclamation + vbOKOnly, "Error"
      Exit Sub
    ElseIf Combo1(1).Text = "" Then
      errmsg = "Please select or enter a file qualifier"
      MsgBox errmsg, vbExclamation + vbOKOnly, "Error"
      Exit Sub
    Else
      ReDim Preserve extensions(UBound(extensions) + 1) As String
      ReDim Preserve qualifiers(UBound(qualifiers) + 1) As String
      extensions(UBound(extensions) - 1) = Combo1(0).Text
      qualifiers(UBound(qualifiers) - 1) = Combo1(1).Text
      tempextension = UCase(Combo1(0).Text)
      tempqualifier = Combo1(1).Text
    End If
    fileaddcheck = True
  End If
  Unload FileAdd
End Sub

Private Sub Form_Load()
  Dim i As Integer
  For i = 0 To Form1.ReadList.ListCount - 1
    Combo1(0).AddItem extensions(i)
    Combo1(1).AddItem qualifiers(i)
  Next
  Combo1(0).ListIndex = 0
  Combo1(1).ListIndex = 0
End Sub
