VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Begin VB.Form ModAdd 
   BorderStyle     =   4  'Fixed ToolWindow
   Caption         =   "Add New Module Name"
   ClientHeight    =   2052
   ClientLeft      =   48
   ClientTop       =   216
   ClientWidth     =   4932
   LinkTopic       =   "Form2"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   2052
   ScaleWidth      =   4932
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   2160
      Top             =   1560
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
      Filter          =   "Icon (*.ico)|*.ico"
   End
   Begin VB.CommandButton Command2 
      Caption         =   "..."
      Height          =   285
      Left            =   4560
      TabIndex        =   8
      Top             =   1080
      Width           =   252
   End
   Begin VB.TextBox Text2 
      Height          =   285
      Left            =   1300
      TabIndex        =   7
      Top             =   1080
      Width           =   3264
   End
   Begin VB.TextBox Text1 
      Height          =   285
      Left            =   1300
      TabIndex        =   6
      Top             =   600
      Width           =   3500
   End
   Begin VB.ComboBox Combo1 
      Height          =   288
      ItemData        =   "ModAdd.frx":0000
      Left            =   1300
      List            =   "ModAdd.frx":0010
      TabIndex        =   5
      Text            =   "Combo1"
      Top             =   120
      Width           =   3500
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Cancel"
      Height          =   350
      Index           =   1
      Left            =   3840
      TabIndex        =   1
      Top             =   1560
      Width           =   975
   End
   Begin VB.CommandButton Command1 
      Caption         =   "OK"
      Height          =   350
      Index           =   0
      Left            =   2760
      TabIndex        =   0
      Top             =   1560
      Width           =   975
   End
   Begin VB.Label Label1 
      AutoSize        =   -1  'True
      Caption         =   "Module Icon"
      Height          =   192
      Index           =   2
      Left            =   120
      TabIndex        =   4
      Top             =   1080
      Width           =   876
   End
   Begin VB.Label Label1 
      AutoSize        =   -1  'True
      Caption         =   "Module Name"
      Height          =   192
      Index           =   1
      Left            =   120
      TabIndex        =   3
      Top             =   600
      Width           =   1020
   End
   Begin VB.Label Label1 
      AutoSize        =   -1  'True
      Caption         =   "Module Type"
      Height          =   192
      Index           =   0
      Left            =   120
      TabIndex        =   2
      Top             =   120
      Width           =   960
   End
End
Attribute VB_Name = "ModAdd"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Compare Text

Private Sub Command1_Click(Index As Integer)
  Dim errmsg As String
  Dim i As Integer
  If Index = 0 Then
    modaddcheck = True
    If Combo1.Text = "" Then
      errmsg = "Please select the Module type before" + Chr(13) + Chr(10) + "trying to add new Module Information"
      MsgBox errmsg, vbExclamation + vbOKOnly, "Error"
      Exit Sub
    End If
    If Text1.Text = "" Then
      errmsg = "Please enter a Module name before" + Chr(13) + Chr(10) + "trying to add new Module Information"
      MsgBox errmsg, vbExclamation + vbOKOnly, "Error"
      Exit Sub
    End If
    If Text2.Text = "" Then
      errmsg = "Please select a Module icon before" + Chr(13) + Chr(10) + "trying to add new Module Information"
      MsgBox errmsg, vbExclamation + vbOKOnly, "Error"
      Exit Sub
    End If
    For i = 0 To Form1.Combo1.ListCount - 1
      If Text1.Text = Form1.Combo1.list(i) Then
        errmsg = "Module Name entered is already in use." + Chr(13) + Chr(10) + "Please enter a unique Module Name."
        MsgBox errmsg, vbExclamation + vbOKOnly, "Error"
        Exit Sub
      End If
    Next
    ReDim Preserve modtypes(UBound(modtypes) + 1) As String
    ReDim Preserve modnames(UBound(modnames) + 1) As String
    ReDim Preserve modids(UBound(modids) + 1) As String
    ReDim Preserve modicons(UBound(modicons) + 1) As String
    modtypes(UBound(modtypes) - 1) = Combo1.Text
    Form1.ModTypeCombo.ListIndex = Combo1.ListIndex
    modnames(UBound(modnames) - 1) = Text1.Text
    Form1.Combo1.AddItem Text1.Text
    Form1.Combo1.ListIndex = Form1.Combo1.ListCount - 1
    modids(UBound(modids) - 1) = GenerateID(Text1.Text, modids)
    modicons(UBound(modicons) - 1) = Text2.Text
    Form1.Text3.Text = Text2.Text
  Else
    modaddcheck = False
  End If
  Unload ModAdd
End Sub

Private Sub Command2_Click()
  modaddcheck = True
  On Error GoTo FILECANCEL
  CommonDialog1.CancelError = True
  CommonDialog1.ShowOpen
  Text2.Text = CommonDialog1.filename
  Exit Sub
FILECANCEL:
  modaddcheck = False
End Sub

Private Sub Form_Load()
  Combo1.ListIndex = 0
End Sub
