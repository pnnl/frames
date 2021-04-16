VERSION 5.00
Begin VB.Form Dummy 
   BorderStyle     =   1  'Fixed Single
   ClientHeight    =   2040
   ClientLeft      =   7740
   ClientTop       =   8388
   ClientWidth     =   3600
   Icon            =   "DUMMY.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   2040
   ScaleWidth      =   3600
   Begin VB.CommandButton Command2 
      Caption         =   "Failure"
      Height          =   492
      Left            =   480
      TabIndex        =   1
      Top             =   1200
      Width           =   2652
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Success"
      Height          =   492
      Left            =   480
      TabIndex        =   0
      Top             =   420
      Width           =   2652
   End
End
Attribute VB_Name = "Dummy"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim FUIName As String        'the path and name of the run
Dim RunName As String        'the name of the parameter file
Dim RefName As String        'the name of the reference file
Dim SiteIndex As Long     'the index to site model parms
Dim ModIndex As Long      'the index to saturated model parms
Dim Model As String
Dim errfile As csv
Dim gidfile As csv
Dim argc As Long
Dim argv() As String

Private Sub form_Unload(Cancel As Integer)
  Dim answer As Long
  answer = MsgBox("Do you want save changes?", 51, "Save changes?")
  If answer = 6 Then Command1_Click
  If answer = 7 Then Command2_Click
  If answer = 2 Then Cancel = 1
End Sub

Sub getargs()
  Dim args As String
  Dim pos As Long
    
  argc = 0
  args = Trim$(Command$)
  If Len(args) > 0 Then
    Do
      pos = InStr(args, " ")
      argc = argc + 1
      If pos > 0 Then
        ReDim Preserve argv(argc) As String
        argv(argc - 1) = Trim$(Left$(args, pos))
        args = Trim$(Right$(args, Len(args) - pos))
      Else
        ReDim Preserve argv(argc)
        argv(argc - 1) = Trim$(args)
      End If
    Loop Until pos = 0
  End If
End Sub

Private Sub Command1_Click()
  If open_csv(gidfile, RunName & ".GID", 1) Then
   put_val gidfile, "DUMMY"
   put_val gidfile, 0
   put_val gidfile, 0
   put_val gidfile, 0
   put_val gidfile, 0
   put_val gidfile, 0
   put_val gidfile, 0
   put_val gidfile, 0
   put_val gidfile, "N/A"
   put_val gidfile, "N/A"
   put_val gidfile, 0
   put_line gidfile
   close_csv gidfile
   close_csv errfile
   Kill RunName & ".ERR"
  Else
   MsgBox "Unable to create file " & RunName & ".ERR" & Chr(10) & "Check directory permessions"
   close_csv errfile
  End If
  End
End Sub

Private Sub Command2_Click()
  put_val errfile, "The failure button was clicked."
  put_line errfile
  close_csv errfile
  End
End Sub

Private Sub Form_Load()
  getargs
  If argc >= 5 Then
    FUIName = argv(0) & ".GID"
    RunName = argv(1)
    SiteIndex = Val(argv(2))
    ModIndex = Val(argv(3))
    Model = argv(4)
    Dummy.Caption = Model & " - FUI Dummy Module"
    If open_csv(errfile, RunName & ".ERR", 1) Then
      put_val errfile, "Error report for " + Dummy.Caption
      put_line errfile
    Else
      MsgBox "Unable to create file " & RunName & ".ERR" & Chr(10) & "Check directory permessions"
      End
    End If
    Left = (Screen.Width - Width) / 2
    Top = (Screen.Height - Height) / 2
  Else
    MsgBox "Not enough arguments passed" & Chr(10) & "Contact PNNL"
    End
  End If
End Sub
