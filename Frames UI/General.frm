VERSION 5.00
Object = "{3B7C8863-D78F-101B-B9B5-04021C009402}#1.2#0"; "RICHTX32.OCX"
Begin VB.Form frmGeneral 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Object General Information"
   ClientHeight    =   7200
   ClientLeft      =   3768
   ClientTop       =   4020
   ClientWidth     =   9600
   ControlBox      =   0   'False
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   7200
   ScaleWidth      =   9600
   StartUpPosition =   2  'CenterScreen
   Begin VB.TextBox txtLabel 
      Height          =   300
      Left            =   1800
      TabIndex        =   26
      Top             =   1320
      Width           =   2895
   End
   Begin VB.ComboBox cboGroup 
      Height          =   315
      Left            =   6120
      Style           =   2  'Dropdown List
      TabIndex        =   24
      Top             =   570
      Width           =   2430
   End
   Begin VB.ComboBox cboClass 
      Height          =   315
      ItemData        =   "General.frx":0000
      Left            =   6120
      List            =   "General.frx":0002
      Style           =   2  'Dropdown List
      TabIndex        =   23
      Top             =   195
      Width           =   2430
   End
   Begin VB.TextBox txtModel 
      BackColor       =   &H00E0E0E0&
      Enabled         =   0   'False
      Height          =   300
      Left            =   6120
      TabIndex        =   21
      Top             =   1320
      Width           =   3360
   End
   Begin RichTextLib.RichTextBox txtAdvert 
      Height          =   4665
      Left            =   4110
      TabIndex        =   20
      Top             =   1980
      Width           =   5385
      _ExtentX        =   9504
      _ExtentY        =   8234
      _Version        =   393217
      ReadOnly        =   -1  'True
      ScrollBars      =   3
      DisableNoScroll =   -1  'True
      TextRTF         =   $"General.frx":0004
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Serif"
         Size            =   7.8
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
   End
   Begin VB.ListBox lstNotApp 
      ForeColor       =   &H80000011&
      Height          =   1776
      Left            =   120
      Sorted          =   -1  'True
      TabIndex        =   18
      Top             =   4800
      Width           =   3855
   End
   Begin VB.TextBox txtZ 
      Alignment       =   1  'Right Justify
      Height          =   300
      Left            =   1800
      TabIndex        =   16
      Top             =   960
      Width           =   1650
   End
   Begin VB.TextBox txtY 
      Alignment       =   1  'Right Justify
      Height          =   300
      Left            =   1800
      TabIndex        =   15
      Top             =   570
      Width           =   1650
   End
   Begin VB.TextBox txtX 
      Alignment       =   1  'Right Justify
      Height          =   300
      Left            =   1800
      TabIndex        =   14
      Top             =   195
      Width           =   1650
   End
   Begin VB.ListBox lstModel 
      Height          =   2160
      Left            =   135
      Sorted          =   -1  'True
      TabIndex        =   1
      Top             =   2010
      Width           =   3855
   End
   Begin VB.CommandButton cmdCancel 
      Caption         =   "&Cancel"
      Height          =   360
      Left            =   8235
      TabIndex        =   3
      Top             =   6720
      Width           =   1200
   End
   Begin VB.CommandButton cmdOk 
      Caption         =   "&Ok"
      Default         =   -1  'True
      Height          =   360
      Left            =   6840
      TabIndex        =   2
      Top             =   6720
      Width           =   1200
   End
   Begin VB.TextBox txtName 
      BackColor       =   &H00E0E0E0&
      Enabled         =   0   'False
      Height          =   300
      Left            =   6120
      TabIndex        =   5
      Top             =   960
      Width           =   2430
   End
   Begin VB.Label Label14 
      AutoSize        =   -1  'True
      Caption         =   "Group"
      Height          =   195
      Left            =   4920
      TabIndex        =   25
      Top             =   570
      Width           =   435
   End
   Begin VB.Label Label13 
      AutoSize        =   -1  'True
      Caption         =   "Previous Model"
      Height          =   195
      Left            =   4920
      TabIndex        =   22
      Top             =   1320
      Width           =   1095
   End
   Begin VB.Label Label12 
      Alignment       =   2  'Center
      Caption         =   "Non-applicable Models"
      Height          =   255
      Left            =   75
      TabIndex        =   19
      Top             =   4560
      Width           =   3870
   End
   Begin VB.Label Label11 
      AutoSize        =   -1  'True
      Caption         =   "Object Id"
      Height          =   195
      Left            =   4920
      TabIndex        =   17
      Top             =   960
      Width           =   645
   End
   Begin VB.Label Label10 
      Alignment       =   2  'Center
      Caption         =   "Model Description"
      Height          =   255
      Left            =   4080
      TabIndex        =   13
      Top             =   1800
      Width           =   5385
   End
   Begin VB.Label Label9 
      Alignment       =   2  'Center
      Caption         =   "Select from Applicable Models"
      Height          =   255
      Left            =   135
      TabIndex        =   12
      Top             =   1800
      Width           =   3855
   End
   Begin VB.Label Label8 
      Caption         =   "km"
      Height          =   300
      Left            =   3495
      TabIndex        =   11
      Top             =   960
      Width           =   600
   End
   Begin VB.Label Label7 
      Caption         =   "km"
      Height          =   300
      Left            =   3495
      TabIndex        =   10
      Top             =   600
      Width           =   600
   End
   Begin VB.Label Label6 
      Caption         =   "km"
      Height          =   300
      Left            =   3495
      TabIndex        =   9
      Top             =   240
      Width           =   600
   End
   Begin VB.Label Label5 
      AutoSize        =   -1  'True
      Caption         =   "Elevation"
      Height          =   195
      Left            =   240
      TabIndex        =   8
      Top             =   945
      Width           =   660
   End
   Begin VB.Label Label4 
      AutoSize        =   -1  'True
      Caption         =   "Northing coordinate"
      Height          =   195
      Left            =   240
      TabIndex        =   7
      Top             =   585
      Width           =   1395
   End
   Begin VB.Label Label3 
      AutoSize        =   -1  'True
      Caption         =   "Easting coordinate"
      Height          =   195
      Left            =   240
      TabIndex        =   6
      Top             =   225
      Width           =   1320
   End
   Begin VB.Label Label2 
      AutoSize        =   -1  'True
      Caption         =   "User Label"
      Height          =   195
      Left            =   240
      TabIndex        =   4
      Top             =   1320
      Width           =   765
   End
   Begin VB.Label Label1 
      AutoSize        =   -1  'True
      Caption         =   "Class"
      Height          =   195
      Left            =   4920
      TabIndex        =   0
      Top             =   225
      Width           =   375
   End
End
Attribute VB_Name = "frmGeneral"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Dim x As New CHyperlinks
Dim ignoreClick As Boolean

Private Sub cboClass_Click()
Dim i As Integer

  cboGroup.Clear
  If cboClass.Text = "Unknown" Then
    cboGroup.AddItem "Unknown"
  Else
    For i = 0 To GroupCount - 1
      If cboClass.Text = Group(i).Type Then
        cboGroup.AddItem Group(i).Name
        cboGroup.ItemData(cboGroup.NewIndex) = i
      End If
    Next
  End If
  If cboGroup.ListCount > 0 Then cboGroup.ListIndex = 0
End Sub

Private Sub cboGroup_Click()
  Models_show lstModel, lstNotApp, frmSite, frmGlyph, cboGroup.ItemData(cboGroup.ListIndex)
  lstModel.ListIndex = GetListIndex(lstModel, Sites(frmSite).Glyphs(frmGlyph).Model)
End Sub

'Function added added for multiple urls
Private Function GetUrl(sPos As Long, ePos As Long, Text As String) As String
Dim i As Integer
Dim pos  As Long
Dim lpos  As Long
Dim tpos(4)  As Long
  
  pos = InStr(sPos, Text, "http://")
  If pos > 0 Then
    tpos(0) = InStr(pos, Text, " ")
    tpos(1) = InStr(pos, Text, vbTab)
    tpos(2) = InStr(pos, Text, vbCrLf)
    tpos(3) = InStr(pos, Text, vbCr)
    tpos(4) = InStr(pos, Text, vbLf)
    ePos = 2147483647
    For i = 0 To 4
      If tpos(i) < ePos And tpos(i) > 0 Then ePos = tpos(i)
    Next
    If ePos = 2147483647 Then ePos = Len(Text) + 1
    sPos = pos
    GetUrl = Mid(Text, pos, ePos - pos)
  Else
    GetUrl = ""
  End If
End Function

Private Sub lstModel_Click()
Dim pos1 As Long
Dim pos2 As Long
Dim url As String
  
  If ignoreClick Then Exit Sub
  ignoreClick = True
  lstNotApp.ListIndex = -1
  txtAdvert.Text = Module(lstModel.ItemData(lstModel.ListIndex)).Advert
  
  'added for multiple urls
  x.Clear
  pos1 = 1
  url = GetUrl(pos1, pos2, txtAdvert.Text)
  Do While url <> ""
    x.Add url, url, pos1
    pos1 = pos2
    url = GetUrl(pos1, pos2, txtAdvert.Text)
  Loop
  
  txtAdvert.SelStart = 1
  ignoreClick = False
End Sub

Private Sub lstNotApp_Click()
Dim pos1 As Long
Dim pos2 As Long
Dim url As String
  
  If ignoreClick Then Exit Sub
  ignoreClick = True
  lstModel.ListIndex = -1
  txtAdvert.Text = Module(lstNotApp.ItemData(lstNotApp.ListIndex)).Advert
  
  'added for multiple urls
  x.Clear
  pos1 = 1
  url = GetUrl(pos1, pos2, txtAdvert.Text)
  Do While url <> ""
    x.Add url, url, pos1
    pos1 = pos2
    url = GetUrl(pos1, pos2, txtAdvert.Text)
  Loop
  
  txtAdvert.SelStart = 1
  ignoreClick = False
End Sub

Private Sub cmdCancel_Click()
  Sites(frmSite).Glyphs(frmGlyph).modified = False
  Unload Me
End Sub

Private Sub cmdOK_Click()
Dim fname As String
Dim gfile As csv
Dim errfile As Boolean

  If Not SiteIdx_SetInfo() Then Exit Sub
  If Sites(frmSite).Glyphs(frmGlyph).state = NO_MODULE Then
    Sites(frmSite).Glyphs(frmGlyph).state = MODULE_OK
    Sites(frmSite).Glyphs(frmGlyph).modified = True
  End If
  If Sites(frmSite).Glyphs(frmGlyph).modified Then
    UpdateDownStreamGlyphs frmSite, frmGlyph, NO_MODULE
  End If
  'need to clear module GID section when module selection changes
  If (Sites(frmSite).Glyphs(frmGlyph).Model <> txtModel.Text) _
    And (Group(Sites(frmSite).Glyphs(frmGlyph).GrpIdx).Type <> DB) _
    And (Not oldVersion) Then
    fname = TmpTitle & DOT_GID
    If BLANK <> Dir(fname) Then Kill fname
    If open_csv(gfile, fname, F_WRITE) Then close_csv gfile
    Commit_Writes Sites(frmSite).Glyphs(frmGlyph).Name, errfile
  End If
  Unload Me
End Sub

Private Sub Form_Activate()
  If lstModel.ListCount < 1 Then
    MsgBox "Try connecting a database icon to this icon to aquire applicable models"
  End If
End Sub

Private Sub Form_load()
  Set x.RichTextBox = txtAdvert
  txtAdvert.Font.Name = "Terminal"
  txtAdvert.Font.Size = 8
  txtAdvert.Font.Bold = True
  txtName = Sites(frmSite).Glyphs(frmGlyph).Name
  txtLabel = Sites(frmSite).Glyphs(frmGlyph).Label
  txtModel = Sites(frmSite).Glyphs(frmGlyph).Model
  txtX = Format(Sites(frmSite).Glyphs(frmGlyph).x, "General Number")
  txtY = Format(Sites(frmSite).Glyphs(frmGlyph).y, "General Number")
  txtZ = Format(Sites(frmSite).Glyphs(frmGlyph).z, "General Number")
  SetClassGroup
End Sub

Public Sub SetClassGroup()
Dim i As Integer
Dim j As Integer
Dim found As Boolean
Dim grp As ModuleGroup

  cboClass.Clear
  cboClass.AddItem "Unknown"
  
  ' load unique module classes into dropdown listbox
  For i = 0 To GroupCount - 1
    found = False
    For j = 0 To cboClass.ListCount - 1
      If cboClass.list(j) = Group(i).Type Then
        found = True
        Exit For
      End If
    Next
    If Not found Then cboClass.AddItem Group(i).Type
  Next
  
  grp = Group(Sites(frmSite).Glyphs(frmGlyph).GrpIdx)
  
  ignoreClick = True
  ' set the class list index
  found = False
  For i = 0 To cboClass.ListCount - 1
    If cboClass.list(i) = grp.Type Then
      cboClass.ListIndex = i
      found = True
      Exit For
    End If
  Next
  ' if not found make it user selectable
  cboClass.Enabled = Not found
  
  ' set the group list index
  found = False
  For i = 0 To cboGroup.ListCount - 1
    If cboGroup.list(i) = grp.Name Then
      cboGroup.ListIndex = i
      found = True
      Exit For
    End If
  Next
  ' if not found make it user selectable
  cboGroup.Enabled = Not found
  
  ignoreClick = False
  cboGroup_Click
End Sub

Function SiteIdx_SetInfo() As Boolean
Dim mx As Integer
Dim mname As String
Dim objName As String
    
  SiteIdx_SetInfo = False
  If (Not cboClass.ListIndex > 0) Then
    MsgBox "Select a class and Group"
    cboClass.SetFocus
  ElseIf (lstModel.ListIndex = -1) Then
    MsgBox "An applicable model must be selected."
    lstModel.SetFocus
  ElseIf Len(txtLabel.Text) = 0 Then
    MsgBox "Name must not be empty"
    txtLabel.SetFocus
  ElseIf Len(txtX.Text) = 0 Then
    MsgBox "Easting must not be empty"
    txtX.SetFocus
  ElseIf Len(txtY.Text) = 0 Then
    MsgBox "Northing must not be empty"
    txtY.SetFocus
  ElseIf Len(txtZ.Text) = 0 Then
    MsgBox "Altitude must not be empty"
    txtZ.SetFocus
  ElseIf Not IsNumeric(txtX.Text) Then
    MsgBox "Easting must be numeric"
    txtX.SetFocus
  ElseIf Not IsNumeric(txtY.Text) Then
    MsgBox "Northing must be numeric"
    txtY.SetFocus
  ElseIf Not IsNumeric(txtZ.Text) Then
    MsgBox "Altitude must be numeric"
    txtZ.SetFocus
  Else
    mx = lstModel.ItemData(lstModel.ListIndex)
    mname = Module(mx).Name
    objName = Replace(txtLabel.Text, " ", "_")
    With Sites(frmSite).Glyphs(frmGlyph)
      .modified = .Label <> objName Or _
                  .Model <> mname Or _
                  .x <> Val(txtX) Or _
                  .y <> Val(txtY) Or _
                  .z <> Val(txtZ)
      .Label = objName
      .x = Val(txtX)
      .y = Val(txtY)
      .z = Val(txtZ)
      .Model = mname
      .modIdx = getmodIdxByName(.Model)
      .GrpIdx = Module(.modIdx).GrpIdx
      If .modified Then modified = True
    End With
    SiteIdx_SetInfo = True
  End If
End Function

Private Sub form_KeyDown(KeyCode As Integer, Shift As Integer)
Dim link As New CHyperlink

  Select Case KeyCode
  Case vbKeyF1:
    KeyCode = 0
    HelpAnchor = "Selecting_a_Model"
    GetHelp
  Case vbKeyUp:
    KeyCode = 0
    SendKeys "+{TAB}"
  Case vbKeyDown:
    KeyCode = 0
    SendKeys "{TAB}"
  Case vbKeyReturn:
    KeyCode = 0
    SendKeys "{TAB}"
  End Select
End Sub
