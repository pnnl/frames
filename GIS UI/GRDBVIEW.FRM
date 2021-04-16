VERSION 5.00
Object = "{00028C01-0000-0000-0000-000000000046}#1.0#0"; "DBGRID32.OCX"
Begin VB.Form GRDBVIEW 
   Caption         =   "Graphic Database View"
   ClientHeight    =   6945
   ClientLeft      =   165
   ClientTop       =   600
   ClientWidth     =   9210
   HelpContextID   =   1050
   LinkTopic       =   "Form1"
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   6945
   ScaleWidth      =   9210
   StartUpPosition =   2  'CenterScreen
   Begin VB.ComboBox Combo1 
      Height          =   315
      Left            =   1305
      Style           =   2  'Dropdown List
      TabIndex        =   1
      Top             =   75
      Width           =   7425
   End
   Begin VB.Data Data1 
      Caption         =   "Data1"
      Connect         =   "dBASE III;"
      DatabaseName    =   ""
      DefaultCursorType=   0  'DefaultCursor
      DefaultType     =   2  'UseODBC
      Exclusive       =   0   'False
      Height          =   345
      Left            =   1290
      Options         =   0
      ReadOnly        =   0   'False
      RecordsetType   =   1  'Dynaset
      RecordSource    =   ""
      Top             =   450
      Width           =   7440
   End
   Begin MSDBGrid.DBGrid DBGrid1 
      Bindings        =   "GRDBVIEW.frx":0000
      Height          =   5925
      Left            =   150
      OleObjectBlob   =   "GRDBVIEW.frx":0014
      TabIndex        =   0
      Top             =   870
      Width           =   8775
   End
   Begin VB.Label Label1 
      AutoSize        =   -1  'True
      Caption         =   "Database:"
      Height          =   195
      Left            =   510
      TabIndex        =   2
      Top             =   135
      Width           =   735
   End
End
Attribute VB_Name = "GRDBVIEW"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim DbcSizeX As Integer, DbcSizeY As Integer
Dim lastHighlighted As String

Private Sub Combo1_Click()
Dim name As String, source As String
  MousePointer = vbHourglass
  DoEvents
  SelectDbgr Combo1.ListIndex, True
  If Combo1.ListIndex = 0 Then
    name = RunName & ".dbf"
  Else
    name = DBFileDir & "~temp" & layers(Combo1.ListIndex).workarea & ".dbf"
  End If
  Data1.DatabaseName = SplitPath(name, SP_DIR)
  Data1.RecordSource = SplitPath(name, SP_TITLE)
  Data1.Refresh
  MousePointer = vbDefault
End Sub

Private Sub Form_Load()
Dim l As Long
  DbcSizeX = Me.Width - DBGrid1.Width
  DbcSizeY = Me.Height - DBGrid1.Height

  Combo1.AddItem FUIName & ".dbf"
  For l = 1 To UBound(layers)
    Combo1.AddItem layers(l).filename
  Next l
  Combo1.ListIndex = 0
End Sub

Private Sub Form_Resize()
    If Me.Width > DbcSizeX Then DBGrid1.Width = Me.Width - DbcSizeX
    If Me.Height > DbcSizeY Then
      DBGrid1.Height = Me.Height - DbcSizeY
    End If
End Sub

Private Sub Form_Unload(Cancel As Integer)
    frmMain.Dbcocx1.devgrclear 0
    frmMain.Dbcocx1.devgrdisplay
    If lastHighlighted <> "" Then
        frmMain.Dbcocx1.devgrdisplayid lastHighlighted, 256
    End If
End Sub

