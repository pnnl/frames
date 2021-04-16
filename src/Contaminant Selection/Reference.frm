VERSION 5.00
Object = "{F856EC8B-F03C-4515-BDC6-64CBD617566A}#7.0#0"; "FPSPR70.ocx"
Begin VB.Form frmReference 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "References"
   ClientHeight    =   8100
   ClientLeft      =   1635
   ClientTop       =   5595
   ClientWidth     =   7815
   ClipControls    =   0   'False
   ControlBox      =   0   'False
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   7.5
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "Reference.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   NegotiateMenus  =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   540
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   521
   StartUpPosition =   1  'CenterOwner
   Begin VB.Frame Frame1 
      ClipControls    =   0   'False
      Height          =   3375
      Left            =   120
      TabIndex        =   11
      Top             =   0
      Width           =   7575
      Begin VB.TextBox Text2 
         BackColor       =   &H00E0E0E0&
         Height          =   1500
         Left            =   240
         Locked          =   -1  'True
         MaxLength       =   256
         MultiLine       =   -1  'True
         TabIndex        =   2
         Top             =   1170
         Width           =   5460
      End
      Begin VB.TextBox Text1 
         BackColor       =   &H00E0E0E0&
         Height          =   312
         Left            =   2295
         Locked          =   -1  'True
         MaxLength       =   100
         TabIndex        =   1
         Top             =   660
         Width           =   3375
      End
      Begin VB.TextBox match 
         Height          =   288
         Left            =   2070
         TabIndex        =   3
         Top             =   2880
         Width           =   2460
      End
      Begin VB.TextBox Text3 
         BackColor       =   &H00E0E0E0&
         Height          =   330
         Left            =   2295
         Locked          =   -1  'True
         TabIndex        =   0
         Top             =   240
         Width           =   855
      End
      Begin VB.CommandButton cmdFindNext 
         Caption         =   "&Find Next"
         Enabled         =   0   'False
         Height          =   350
         Left            =   4605
         TabIndex        =   4
         Top             =   2835
         Width           =   1120
      End
      Begin VB.CommandButton OK 
         Caption         =   "&Apply"
         Enabled         =   0   'False
         Height          =   350
         Left            =   6210
         TabIndex        =   6
         Top             =   360
         Width           =   1120
      End
      Begin VB.CommandButton cmdClose 
         Caption         =   "&Close"
         Height          =   350
         Left            =   6210
         TabIndex        =   10
         Top             =   2820
         Width           =   1120
      End
      Begin VB.CommandButton cmdUpdate 
         Caption         =   "&Update"
         Enabled         =   0   'False
         Height          =   350
         Left            =   6210
         TabIndex        =   9
         Top             =   2145
         Width           =   1120
      End
      Begin VB.CommandButton cmdNew 
         Caption         =   "&New"
         Height          =   350
         Left            =   6210
         TabIndex        =   8
         Top             =   1455
         Width           =   1120
      End
      Begin VB.CommandButton cmdEdit 
         Caption         =   "&Edit"
         Height          =   350
         Left            =   6210
         TabIndex        =   7
         Top             =   1020
         Width           =   1120
      End
      Begin VB.Label Label1 
         AutoSize        =   -1  'True
         Caption         =   "Reference Number:"
         Height          =   195
         Left            =   780
         TabIndex        =   14
         Top             =   315
         Width           =   1395
      End
      Begin VB.Label Label4 
         Caption         =   "Search Short Name for:"
         Height          =   255
         Left            =   255
         TabIndex        =   13
         Top             =   2910
         Width           =   1665
      End
      Begin VB.Label Label2 
         AutoSize        =   -1  'True
         Caption         =   "Short Name:"
         Height          =   195
         Left            =   1290
         TabIndex        =   12
         Top             =   690
         Width           =   885
      End
   End
   Begin FPSpreadADO.fpSpread vaSpread1 
      Height          =   4485
      Left            =   120
      TabIndex        =   5
      Top             =   3480
      Width           =   7545
      _Version        =   458752
      _ExtentX        =   13309
      _ExtentY        =   7911
      _StockProps     =   64
      DAutoCellTypes  =   0   'False
      DAutoHeadings   =   0   'False
      DAutoSave       =   0   'False
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Tahoma"
         Size            =   7.5
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      RowHeaderDisplay=   0
      ScrollBarMaxAlign=   0   'False
      ScrollBarShowMax=   0   'False
      SelectBlockOptions=   0
      ShadowColor     =   12640511
      SpreadDesigner  =   "Reference.frx":0442
      UserResize      =   0
      VScrollSpecial  =   -1  'True
   End
End
Attribute VB_Name = "frmReference"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

' This form module uses global variables that the
' user must define in a BAS file.  Those variables are:

'Global RefIdx As long         'the current selected reference

Private biggest As Long
Private InitText As Boolean

Private Sub put_refrec()
  Text1 = ""
  Text2 = ""
  If Not tblRef.NoMatch Then
    If Not IsNull(tblRef.Fields(1)) Then Text1.text = tblRef.Fields(1)
    If Not IsNull(tblRef.Fields(2)) Then Text2.text = tblRef.Fields(2)
    Text3.text = tblRef.Fields(0)
  Else
    cmdNew_Click
  End If
  Text1.BackColor = Text3.BackColor
  Text2.BackColor = Text3.BackColor
  Text1.Locked = True
  Text2.Locked = True
  If Not tblRef.NoMatch Then
    OK.Enabled = tblRef.Fields(0)
  End If
  cmdEdit.Enabled = Not IN_FRAMES
  cmdNew.Enabled = Not IN_FRAMES
  cmdUpdate.Enabled = False
End Sub

Private Sub cmdClose_Click()
  If RefUnload Then
    Unload Me
  Else
    Me.Hide
  End If
End Sub

Private Sub cmdEdit_Click()
  Text1.BackColor = vbWhite
  Text2.BackColor = vbWhite
  Text1.Locked = False
  Text2.Locked = False
  cmdUpdate.Enabled = True
  cmdUpdate.Caption = "&Update"
  OK.Enabled = False
  Text1.SetFocus
End Sub

Private Sub cmdFindNext_Click()
  tblRef.FindNext "sName like '" & match & "*'"
  If Not tblRef.NoMatch Then
    put_refrec
  End If
End Sub

Private Sub cmdNew_Click()
  Text1 = ""
  Text2 = ""
  Text1.BackColor = vbWhite
  Text2.BackColor = vbWhite
  Text3 = biggest
  Text1.Locked = False
  Text2.Locked = False
  cmdUpdate.Enabled = True
  cmdUpdate.Caption = "&Add"
  cmdEdit.Enabled = False
  OK.Enabled = False
  Text1.SetFocus
End Sub

Private Sub cmdUpdate_Click()
Dim i As Long
Dim qry As String
Dim var As Variant

  If Len(Text1) = 0 Or Len(Text2) = 0 Then
    Beep
    If Len(Text1) = 0 Then Text1.SetFocus Else Text2.SetFocus
    Exit Sub
  End If
  Text1.text = Replace(Text1.text, """", "'")
  Text2.text = Replace(Text2.text, """", "'")
  If cmdUpdate.Caption = "&Add" Then
    tblRef.AddNew
    tblRef.Fields(0) = Text3
    tblRef.Fields(1) = Text1
    tblRef.Fields(2) = Text2
    tblRef.update
    
    vaSpread1.ReDraw = False
    vaSpread1.MaxRows = vaSpread1.MaxRows + 1
    vaSpread1.SetText 0, vaSpread1.MaxRows, Text3
    vaSpread1.SetText 1, vaSpread1.MaxRows, Text1
    vaSpread1.SetText 2, vaSpread1.MaxRows, Text2
    vaSpread1.ReDraw = True
    vaSpread1.TopRow = vaSpread1.MaxRows
    biggest = biggest + 1
  Else
    qry = "UPDATE ref SET sName='" & Text1 & "', bName='" & Text2 & "' WHERE refnum = " & Text3
    DB.Execute qry
    
    For i = 1 To vaSpread1.MaxRows
      vaSpread1.GetText 0, i, var
      If var = Text3 Then
        vaSpread1.SetText 1, i, Text1
        vaSpread1.SetText 2, i, Text2
        vaSpread1.row = i: vaSpread1.col = 2
        vaSpread1.RowHeight(i) = vaSpread1.MaxTextCellHeight
        vaSpread1.TopRow = i
        Exit For
      End If
    Next
  End If
  tblRef.FindFirst "RefNum = " & Text3
  put_refrec
End Sub

Private Sub Form_Activate()
Dim ref As Long

  If Not ReferenceLoaded Then LoadReference
  OK.Enabled = RefIdx >= 0
  If RefIdx < 0 Then ref = 0 Else ref = RefIdx
  tblRef.FindFirst "RefNum = " & ref
  put_refrec
  cmdUpdate.Enabled = False
  Text1.Locked = True
  Text2.Locked = True
  frmCDBE.SaveAnchor HelpAnchor
  HelpAnchor = "References"
End Sub

Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
  Select Case KeyCode
    Case vbKeyF1:
      KeyCode = 0
      frmCDBE.howto_Click
  End Select
End Sub

Private Sub Form_load()
  If Not ReferenceLoaded Then LoadReference
End Sub

Private Sub Form_Unload(Cancel As Integer)
  ReferenceLoaded = False
End Sub

Private Sub Ok_Click()
  If Len(Text2) = 0 Or Len(Text3) = 0 Then Exit Sub
  If RefParam <> "" Then
    RefIdx = Text3
    If RefUnload Then
      Unload Me
    Else
      frmCDBE.set_refrec
      Me.Hide
    End If
  End If
End Sub

Private Sub Match_Change()
  tblRef.FindFirst "sName like '" & match & "*'"
  If tblRef.NoMatch Then Exit Sub
  put_refrec
  cmdFindNext.Enabled = (match <> "")
End Sub

Private Sub Text1_Change()
  If InitText Then Exit Sub
  cmdUpdate.Enabled = Not Text1.Locked
End Sub

Private Sub Text2_Change()
  If InitText Then Exit Sub
  cmdUpdate.Enabled = Not Text1.Locked
End Sub

Private Sub Text1_KeyPress(KeyAscii As Integer)
  If KeyAscii = vbKeyReturn Then KeyAscii = 0
End Sub

Private Sub Text2_KeyPress(KeyAscii As Integer)
  If KeyAscii = vbKeyReturn Then KeyAscii = 0
End Sub

Public Sub LoadReference()
Dim r As Long
Dim c As Long
Dim wid As Long
Dim task As String
Dim rs As Recordset
  
On Error GoTo ErrorHandler
  
  If Len(DBName) = 0 Then Exit Sub
  
  If tblRef.RecordCount > 0 Then
    tblRef.MoveLast
    tblRef.MoveFirst
  End If
  
  vaSpread1.ReDraw = False
  vaSpread1.MaxRows = tblRef.RecordCount
  vaSpread1.MaxCols = 2
  vaSpread1.row = -1
  
  For c = 0 To 2
    vaSpread1.col = c
    Select Case c
      Case 0
        wid = 9
        vaSpread1.TypeHAlign = 0
        vaSpread1.TypeHAlign = 2
        vaSpread1.CellType = 3
        vaSpread1.TypeHAlign = 2
      Case 1
        wid = 16
        vaSpread1.CellType = 5
        vaSpread1.TypeTextWordWrap = True
      Case 2
        wid = 51
        vaSpread1.CellType = 5
        vaSpread1.TypeTextWordWrap = True
    End Select
    vaSpread1.ColWidth(c) = wid
  Next
  r = 1
  vaSpread1.SetText 0, 0, "Reference Number"
  vaSpread1.SetText 1, 0, "Short Name"
  vaSpread1.SetText 2, 0, "Description"
  vaSpread1.RowHeight(0) = 2 * vaSpread1.RowHeight(1)
  
  biggest = 0
  While Not tblRef.EOF
    If tblRef.Fields(0) > biggest Then biggest = tblRef.Fields(0)
    vaSpread1.row = r
    For c = 0 To tblRef.Fields.Count - 1
      vaSpread1.SetText c, r, tblRef.Fields(c)
    Next
    vaSpread1.col = 2
    vaSpread1.RowHeight(r) = vaSpread1.MaxTextCellHeight
    tblRef.MoveNext
    r = r + 1
  Wend

  Set rs = DB.OpenRecordset("SELECT MAX(RefNum) FROM REF")
  If Not IsNull(rs.Fields(0)) Then biggest = rs.Fields(0)
  
  vaSpread1.row = -1
  vaSpread1.col = -1
  vaSpread1.BlockMode = True
  vaSpread1.SortBy = 0
  vaSpread1.SortKey(1) = 0
  vaSpread1.SortKeyOrder(1) = 1
  vaSpread1.Action = 25
  vaSpread1.BlockMode = False
  vaSpread1.ReDraw = True
  biggest = biggest + 1
  ReferenceLoaded = True
  
ErrorHandler:
  If Err.Number <> 0 Then
     MsgBox Error & vbCrLf & task, vbOKOnly, "LoadReference"
  End If
End Sub

Private Sub vaSpread1_Click(ByVal col As Long, ByVal row As Long)
Dim var As Variant

  If row = 0 Then
    vaSpread1.row = -1
    vaSpread1.col = -1
    vaSpread1.BlockMode = True
    vaSpread1.SortBy = 0
    vaSpread1.SortKey(1) = col
    vaSpread1.SortKeyOrder(1) = 1
    vaSpread1.Action = 25
    vaSpread1.BlockMode = False
  Else
    vaSpread1.GetText 0, row, var
    tblRef.FindFirst "refnum = " & var
    put_refrec
  End If
End Sub

Private Sub vaSpread1_LeaveCell(ByVal col As Long, ByVal row As Long, ByVal NewCol As Long, ByVal NewRow As Long, Cancel As Boolean)
Dim var As Variant

  If NewCol < 0 Or NewRow < 0 Then Exit Sub
  InitText = True
  vaSpread1.GetText 0, NewRow, var
  tblRef.FindFirst "refnum = " & var
  put_refrec
  InitText = False
End Sub

Private Sub vaSpread1_LeaveRow(ByVal row As Long, ByVal RowWasLast As Boolean, ByVal RowChanged As Boolean, ByVal AllCellsHaveData As Boolean, ByVal NewRow As Long, ByVal NewRowIsLast As Long, Cancel As Boolean)
Dim var As Variant

  InitText = True
  vaSpread1.GetText 0, NewRow, var
  tblRef.FindFirst "refnum = " & var
  put_refrec
  InitText = False
End Sub

Public Sub set_refrec()
Dim ref As Long

  OK.Enabled = (RefIdx >= 0)
  If RefIdx < 0 Then ref = 0 Else ref = RefIdx
  InitText = True
  Text1 = ""
  Text2 = ""
  If ref >= 0 Then
    tblRef.FindFirst "RefNum = " & ref
    If Not tblRef.NoMatch Then
      put_refrec
    End If
  End If
  InitText = False
End Sub
