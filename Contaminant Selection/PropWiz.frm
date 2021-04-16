VERSION 5.00
Object = "{F856EC8B-F03C-4515-BDC6-64CBD617566A}#7.0#0"; "FPSPR70.ocx"
Object = "{6B7E6392-850A-101B-AFC0-4210102A8DA7}#1.3#0"; "comctl32.ocx"
Begin VB.Form frmPropWiz 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Property Wizard"
   ClientHeight    =   7545
   ClientLeft      =   2760
   ClientTop       =   3750
   ClientWidth     =   11175
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
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   7545
   ScaleWidth      =   11175
   ShowInTaskbar   =   0   'False
   StartUpPosition =   1  'CenterOwner
   Begin VB.Frame fraTable 
      Caption         =   "Description"
      ClipControls    =   0   'False
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   10.5
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   6900
      Left            =   5640
      TabIndex        =   1
      Top             =   120
      Width           =   5412
      Begin VB.CommandButton cmdCancel 
         Cancel          =   -1  'True
         Caption         =   "Cancel"
         Height          =   350
         Left            =   2880
         TabIndex        =   11
         Top             =   6360
         Width           =   1120
      End
      Begin VB.CommandButton cmdOk 
         Caption         =   "OK"
         Height          =   350
         Left            =   4080
         TabIndex        =   10
         Top             =   6360
         Width           =   1120
      End
      Begin VB.CommandButton cmdAttrAdd 
         Caption         =   "&Add Criteria"
         Height          =   350
         Left            =   240
         TabIndex        =   7
         Top             =   480
         Width           =   1120
      End
      Begin VB.CommandButton cmdAttrDel 
         Caption         =   "&Delete Criteria"
         Height          =   350
         Left            =   240
         TabIndex        =   6
         Top             =   960
         Width           =   1120
      End
      Begin VB.CheckBox chkViewProp 
         Caption         =   "&View only properties with criteria"
         Height          =   192
         Left            =   1920
         TabIndex        =   5
         Top             =   600
         Width           =   3000
      End
      Begin VB.CheckBox chkViewCons 
         Caption         =   "View only populated consitiuents"
         Height          =   192
         Left            =   1920
         TabIndex        =   4
         Top             =   960
         Width           =   3000
      End
      Begin VB.ComboBox cboName 
         DataSource      =   "dsName"
         Height          =   288
         Index           =   0
         Left            =   1680
         Style           =   2  'Dropdown List
         TabIndex        =   3
         Top             =   1440
         Width           =   3468
      End
      Begin VB.ComboBox cboCASID 
         DataSource      =   "dsCasid"
         Height          =   288
         Index           =   0
         Left            =   240
         Style           =   2  'Dropdown List
         TabIndex        =   2
         Top             =   1440
         Width           =   1452
      End
      Begin FPSpreadADO.fpSpread sprCriteria 
         Height          =   4452
         Left            =   240
         TabIndex        =   0
         Top             =   1800
         Width           =   4932
         _Version        =   458752
         _ExtentX        =   8700
         _ExtentY        =   7853
         _StockProps     =   64
         AllowDragDrop   =   -1  'True
         BackColorStyle  =   1
         ColHeaderDisplay=   0
         DisplayRowHeaders=   0   'False
         EditModeReplace =   -1  'True
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         MaxCols         =   4
         ScrollBarExtMode=   -1  'True
         SelectBlockOptions=   0
         SpreadDesigner  =   "PropWiz.frx":0000
         VisibleCols     =   3
         VisibleRows     =   16
      End
      Begin VB.Label Label1 
         AutoSize        =   -1  'True
         Caption         =   "Property not defined for this constituent!"
         BeginProperty Font 
            Name            =   "Tahoma"
            Size            =   12
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   288
         Left            =   480
         TabIndex        =   12
         Top             =   2040
         Width           =   4344
      End
   End
   Begin ComctlLib.StatusBar StatusBar1 
      Height          =   396
      Left            =   120
      TabIndex        =   8
      Top             =   7080
      Width           =   10896
      _ExtentX        =   19209
      _ExtentY        =   688
      SimpleText      =   ""
      _Version        =   327682
      BeginProperty Panels {0713E89E-850A-101B-AFC0-4210102A8DA7} 
         NumPanels       =   2
         BeginProperty Panel1 {0713E89F-850A-101B-AFC0-4210102A8DA7} 
            AutoSize        =   1
            Object.Width           =   9551
            Object.Tag             =   ""
         EndProperty
         BeginProperty Panel2 {0713E89F-850A-101B-AFC0-4210102A8DA7} 
            AutoSize        =   1
            Object.Width           =   9551
            Object.Tag             =   ""
         EndProperty
      EndProperty
   End
   Begin FPSpreadADO.fpSpread sprProperty 
      Height          =   6864
      Left            =   120
      TabIndex        =   9
      Top             =   120
      Width           =   5412
      _Version        =   458752
      _ExtentX        =   9546
      _ExtentY        =   12107
      _StockProps     =   64
      AllowDragDrop   =   -1  'True
      BackColorStyle  =   1
      ColHeaderDisplay=   0
      DisplayRowHeaders=   0   'False
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      MaxCols         =   3
      ScrollBarExtMode=   -1  'True
      SelectBlockOptions=   0
      SpreadDesigner  =   "PropWiz.frx":025C
      VisibleCols     =   3
      VisibleRows     =   28
   End
   Begin ComctlLib.ImageList ImageList1 
      Left            =   10320
      Top             =   240
      _ExtentX        =   1005
      _ExtentY        =   1005
      BackColor       =   -2147483643
      ImageWidth      =   16
      ImageHeight     =   16
      MaskColor       =   12632256
      _Version        =   327682
      BeginProperty Images {0713E8C2-850A-101B-AFC0-4210102A8DA7} 
         NumListImages   =   1
         BeginProperty ListImage1 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "PropWiz.frx":04BF
            Key             =   "Check"
         EndProperty
      EndProperty
   End
End
Attribute VB_Name = "frmPropWiz"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Private mode As String
Private qryData As String
Private selectedAttr As String
Private selectedParam As String

Private IgnoreClick As Boolean
Private IgnoreActivate As Boolean

Public Sub get_tablevalue()
  mode = "Select"
  Me.Show vbModal
  mode = ""
End Sub

Private Sub cmdOK_Click()
Dim var As Variant
  
  If mode = "Select" Then
    sprCriteria.GetText 2, sprCriteria.ActiveRow, var
    PropertyValue = var
    sprCriteria.GetText 3, sprCriteria.ActiveRow, var
    PropertyRef = CLng(var)
    frmCDBE.set_tablevalue
  Else

On Error Resume Next
    DB.TableDefs(CRITERIA_DATA).Fields.Delete "Temp"
    ' cleanup records with no data
    ws.CommitTrans
    DB.TableDefs.Refresh
  End If
  Unload Me
End Sub

Private Sub cmdCancel_Click()
  If mode = "Select" Then
    frmCDBE.set_tablevalue
  Else
    ws.Rollback
  End If
  Unload Me
End Sub

Private Sub Form_Activate()
  RefUnload = True
  If Not PropertyWizLoaded Then LoadPropertyWizard
  If IgnoreActivate Then Exit Sub ' returning from references
  sprProperty.OperationMode = OperationModeSingle
  
  If mode = "Select" Then
    selectedParam = PropertyParam
    chkViewProp.value = 1 ' only populated
    chkViewCons.value = 1 ' only populated
    cmdAttrAdd.Enabled = False
    cmdAttrDel.Enabled = False
    chkViewProp.Enabled = False
    chkViewCons.Enabled = False
    sprProperty.Enabled = False
    cboCASID(1).Enabled = False
    cboCASID(0).Enabled = False
    cboName(1).Enabled = False
    cboName(0).Enabled = False
    cboCASID(1).text = PropertyCasid
    sprCriteria.OperationMode = OperationModeSingle
  Else
    cmdAttrAdd.Enabled = True
    cmdAttrDel.Enabled = True
    chkViewProp.Enabled = True
    chkViewCons.Enabled = True
    sprProperty.Enabled = True
    cboCASID(1).Enabled = True
    cboCASID(0).Enabled = True
    cboName(1).Enabled = True
    cboName(0).Enabled = True
  End If
End Sub

Private Sub Form_load()
  If Not PropertyWizLoaded Then LoadPropertyWizard
End Sub

Private Sub Form_Unload(Cancel As Integer)
  PropertyWizLoaded = False
  RefUnload = False
End Sub

Private Sub sprProperty_LeaveCell(ByVal col As Long, ByVal row As Long, ByVal NewCol As Long, ByVal NewRow As Long, Cancel As Boolean)
  ParamClick NewRow
End Sub

Private Sub sprCriteria_DblClick(ByVal col As Long, ByVal row As Long)
  If mode <> "select" Then SpreadsheetEvent col, row
End Sub

Private Sub sprCriteria_EditChange(ByVal col As Long, ByVal row As Long)
Dim id As String
Dim pval As Variant
Dim prop As Variant
Dim attr As Variant
Dim pref As Variant
Dim pName As Variant
Dim qry As String

  If cboCASID(chkViewCons.value).ListIndex < 0 Then Exit Sub
  id = cboCASID(chkViewCons.value).list(cboCASID(chkViewCons.value).ListIndex)
  sprCriteria.EditModeReplace = True
  sprCriteria.row = row
  sprCriteria.GetText 1, row, pName
  sprCriteria.GetText 2, row, pval
  pval = Trim$(pval)
  sprCriteria.GetText 3, row, pref
  sprCriteria.GetText 4, row, attr
  selectedAttr = attr
  sprProperty.GetText 2, sprProperty.ActiveRow, prop
  selectedParam = prop
  HelpAnchor = selectedParam
  If pval = "" Then sprCriteria.SetText 3, row, ""
  Select Case col
    Case 1
      qry = "UPDATE " & CRITERIA_DEF & " SET [Desc] ='" & pName & "' WHERE Id ='" & selectedAttr & "'"
      DB.Execute qry
    Case 2, 3
      If pval <> "" And pref = "" Then
        pref = 0
        sprCriteria.SetText 3, row, 0
      End If
      sprCriteria.col = 2
      If pval = "" Then
        pref = 0
        sprCriteria.BackColor = vbWhite
      Else
        sprCriteria.BackColor = lightRed
      End If
      If ValidInput(selectedParam, pval) Then
        SetParamValue CStr(selectedAttr), id, pval, pref
        sprCriteria.BackColor = lightGreen
      End If
  End Select
  sprCriteria.EditModeReplace = False
End Sub

Private Sub cboCASID_Click(Index As Integer)
  If IgnoreClick Then Exit Sub
  
  FindSynonyms 0, cboCASID(Index).list(cboCASID(Index).ListIndex), ""
  IgnoreClick = True
  cboName(Index).text = tblChems!Name
  IgnoreClick = False
  LoadTable cboCASID(Index).list(cboCASID(Index).ListIndex)
  SetRangeDisplay StatusBar1, Val(tblChems!CLKTYPE)
  sprCriteria.Visible = (InStr(StatusBar1.Panels(2).text, "Not Defined") = 0)
End Sub

Private Sub cboName_Click(Index As Integer)
  If IgnoreClick Then Exit Sub
  
  FindSynonyms 0, "", cboName(Index).list(cboName(Index).ListIndex)
  IgnoreClick = True
  cboCASID(Index).text = tblChems!CASID
  IgnoreClick = False
  LoadTable cboCASID(Index).list(cboCASID(Index).ListIndex)
  SetRangeDisplay StatusBar1, Val(tblChems!CLKTYPE)
  sprCriteria.Visible = (InStr(StatusBar1.Panels(2).text, "Not Defined") = 0)
End Sub

Private Sub chkViewCons_Click()
Dim rs As Recordset
  
  cboCASID(1).Clear
  cboName(1).Clear
  If chkViewCons.value = 1 And qryData <> "" Then
    Set rs = DB.OpenRecordset(qryData, dbOpenDynaset)
    If Not (rs.EOF And rs.BOF) Then
      rs.MoveFirst
      Do While Not rs.EOF
        cboCASID(1).AddItem rs!CASID
        rs.MoveNext
      Loop
    End If
    rs.Close
    Set rs = Nothing
    Set rs = DB.OpenRecordset("SELECT Name FROM Chemicals WHERE casid IN (" & qryData & ")", dbOpenDynaset)
    If Not (rs.EOF And rs.BOF) Then
      rs.MoveFirst
      Do While Not rs.EOF
        cboName(1).AddItem rs!Name
        rs.MoveNext
      Loop
    End If
    rs.Close
    Set rs = Nothing
    If cboCASID(1).ListCount > 0 Then cboCASID(1).ListIndex = 0
  End If
  cboName(0).Visible = chkViewCons.value = 0
  cboCASID(0).Visible = chkViewCons.value = 0
  cboName(1).Visible = chkViewCons.value = 1
  cboCASID(1).Visible = chkViewCons.value = 1
End Sub

Private Sub chkViewProp_Click()
Dim tw As Long
Dim ht As Long
Dim row As Long
Dim wid As Long
Dim cwid As Long
Dim swid As Single
Dim qry As String
Dim rsCrit As Recordset
Dim tblParam As Recordset
  
  sprProperty.ReDraw = False
  sprProperty.row = -1
  sprProperty.col = -1
  sprProperty.Action = 0 ' clear
  sprProperty.MaxRows = 500
  sprProperty.CellType = 5 ' static text
  sprProperty.col = 1
  sprProperty.Lock = True
  sprProperty.SetText 1, 0, "Criteria"
  sprProperty.SetText 2, 0, "Name"
  sprProperty.SetText 3, 0, "Description"
  
  row = 0
  Set tblParam = DB.OpenRecordset("Param", dbOpenTable)
  tblParam.Index = "Category"
  tblParam.MoveFirst
  Do While Not tblParam.EOF
    If tblParam!Category <> 1 Then
      selectedParam = tblParam!sName
      If (mode = "select" And PropertyParam = selectedParam) Or mode <> "select" Then
        qry = "SELECT * FROM " & CRITERIA_DEF & " WHERE SName='" & selectedParam & "'"
        Set rsCrit = DB.OpenRecordset(qry, dbOpenDynaset)
        If chkViewProp.value = 0 Or Not (rsCrit.EOF And rsCrit.BOF) Then
          row = row + 1
          sprProperty.SetText 1, row, ""
          sprProperty.SetText 2, row, selectedParam
          sprProperty.SetText 3, row, tblParam!Desc
          If Not (rsCrit.EOF And rsCrit.BOF) Then
            sprProperty.row = row
            sprProperty.col = 1
            sprProperty.CellType = 10 ' check box
            sprProperty.TypeCheckCenter = True
            sprProperty.value = 1
          End If
        End If
        rsCrit.Close
      End If
    End If
    tblParam.MoveNext
  Loop
  tblParam.Close
  Set rsCrit = Nothing
  Set tblParam = Nothing
  If row = 0 Then selectedParam = ""
  sprProperty.MaxRows = row
  sprProperty.ColWidth(1) = 0.5 * sprProperty.ColWidth(2)
  sprProperty.ColWidthToTwips sprProperty.ColWidth(1), cwid
  tw = cwid
  sprProperty.ColWidthToTwips sprProperty.ColWidth(2), cwid
  tw = tw + cwid
  sprProperty.GetClientArea wid, ht
  sprProperty.TwipsToColWidth wid - tw, swid
  sprProperty.ColWidth(3) = swid
  ParamClick sprProperty.ActiveRow
  sprProperty.ReDraw = True
End Sub

Private Sub cmdAttrAdd_Click()
Dim row As Long
Dim nc As Long
Dim ncMax As Long
Dim qry As String
Dim attr As String
Dim attrid As String
Dim fld As Field
Dim tdf As TableDef
Dim rsCrit As Recordset
Dim tblCrit As Recordset

  If sprProperty.MaxRows = 0 Then Exit Sub
  attr = InputBox("Enter description or label for criteria", "Criteria Definition", "")
  If Len(attr) = 0 Then Exit Sub
  
On Error GoTo ErrorHandler
  
  ' get count of existing criteria for the selected parameter
  qry = "SELECT * FROM " & CRITERIA_DEF & " WHERE SName='" & selectedParam & "' ORDER BY Id"
  Set rsCrit = DB.OpenRecordset(qry, dbOpenDynaset)
  
  ' obtain last criteria id and assign criteria id
  nc = 0
  ncMax = 0
  If Not (rsCrit.EOF And rsCrit.BOF) Then
    rsCrit.MoveFirst
    Do While Not rsCrit.EOF
      nc = Val(Mid$(rsCrit!id, Len(selectedParam) + 1))
      If nc > ncMax Then ncMax = nc
      rsCrit.MoveNext
    Loop
  End If
  rsCrit.Close
  attrid = selectedParam & (ncMax + 1)
  
  ' ensure CriteriaData table exists
On Error Resume Next
  Set tdf = DB.TableDefs(CRITERIA_DATA)
  If Err.Number <> 0 Then
    Err.Clear
    Set tdf = DB.CreateTableDef(CRITERIA_DATA)
    Set fld = tdf.CreateField(CASID, dbText)
    fld.AllowZeroLength = False
    tdf.Fields.Append fld
    DB.TableDefs.Append tdf
  End If
  DB.TableDefs.Refresh
On Error GoTo ErrorHandler
  
  ' add fields for criteria values to CriteriaData table
  '   the unexplained error response to adding fields to the table
  '   was handled by interacting with the table in the LoadData
  '   function which adds a temporary field.  The temporary field
  '   is then deleted on OK from this form (see sub CreateAttribute)
  CreateAttribute attrid
  
  ' add criteria description to CriteriaDef table
  Set tblCrit = DB.OpenRecordset(CRITERIA_DEF, dbOpenDynaset)
  tblCrit.AddNew
  tblCrit!sName = selectedParam
  tblCrit!id = attrid
  tblCrit!Desc = attr
  tblCrit.update
  tblCrit.Close
  
  ' add criteria check to spreadsheet view
  row = sprProperty.ActiveRow
  sprProperty.row = row
  sprProperty.col = 1
  sprProperty.CellType = 10 ' check box
  sprProperty.TypeCheckCenter = True
  sprProperty.value = 1
  
  'refresh criteria display
  RefreshPropertyCriteria
  
ErrorHandler:
  If Err.Number <> 0 Then
    MsgBox Error
    Exit Sub
  End If
End Sub

Private Sub cmdAttrDel_Click()
Dim i As Long
Dim attr As Variant
Dim msg As String
Dim qrySelect As String
Dim qryDelete As String
Dim tdf As TableDef
Dim rst As Recordset
Dim col As Collection
  
  If sprProperty.MaxRows = 0 Or sprCriteria.MaxRows = 0 Then Exit Sub
  sprCriteria.GetText 4, sprCriteria.ActiveRow, attr
  selectedAttr = attr
  If Len(selectedAttr) = 0 Then Exit Sub
  msg = "Are you sure you want to delete this criteria ?"
  qrySelect = "SELECT * FROM " & CRITERIA_DEF & " WHERE Id='" & selectedAttr & "'"
  qryDelete = "DELETE FROM " & CRITERIA_DEF & " WHERE Id='" & selectedAttr & "'"
  If vbNo = MsgBox(msg, vbYesNo + vbQuestion) Then Exit Sub
  
  ' enumerate attributes to be deleted
  Set col = New Collection
  Set rst = DB.OpenRecordset(qrySelect)
  If Not (rst.EOF And rst.BOF) Then
    rst.MoveFirst
    Do While Not rst.EOF
      selectedAttr = rst!id
      col.Add selectedAttr
      rst.MoveNext
    Loop
  End If
  rst.Close
  Set rst = Nothing
  
  ' delete attribute fields from CriteriaData
  Set tdf = DB.TableDefs(CRITERIA_DATA)
  For i = 1 To col.Count
    tdf.Fields.Delete CStr(col(i))
    tdf.Fields.Delete "Ref" & CStr(col(i))
  Next i
  Set tdf = Nothing
  DB.TableDefs.Refresh
  
  ' delete attribute(s) from values
  DB.Execute qryDelete
  If col.Count = 1 Then
    chkViewProp_Click
  Else
    RefreshPropertyCriteria
  End If
End Sub

Private Sub RefreshPropertyCriteria()
Dim tw As Long
Dim ht As Long
Dim row As Long
Dim wid As Long
Dim cwid As Long
Dim qry As String
Dim swid As Single
Dim rsCrit As Recordset
    
    qry = "SELECT * FROM " & CRITERIA_DEF & " WHERE SName='" & selectedParam & "' ORDER BY Id"
    Set rsCrit = DB.OpenRecordset(qry, dbOpenDynaset)
    
    sprCriteria.ReDraw = False
    
    'clear spreadsheet
    sprCriteria.BlockMode = True
    sprCriteria.col = -1
    sprCriteria.row = -1
    sprCriteria.Action = 3 ' clear
    sprCriteria.col = 3
    sprCriteria.Col2 = 3
    sprCriteria.CellType = 5
    sprCriteria.BlockMode = False
    
    sprCriteria.SetText 1, 0, "Description"
    sprCriteria.SetText 2, 0, "Value"
    sprCriteria.SetText 3, 0, "Ref"
    sprCriteria.SetText 4, 0, "Id"
    sprCriteria.col = 0
    sprCriteria.ColHidden = True
    sprCriteria.col = 4
    sprCriteria.ColHidden = True
    
    sprCriteria.MaxCols = 4
    If rsCrit.BOF And rsCrit.EOF Then
      sprCriteria.MaxRows = 0
      rsCrit.Close
      qryData = ""
    Else
      sprCriteria.MaxRows = 500
      row = 0
      qryData = "SELECT casid FROM " & CRITERIA_DATA & " WHERE "
      rsCrit.MoveFirst
      Do While Not rsCrit.EOF
        row = row + 1
'        sprCriteria.row = row
        sprCriteria.SetText 1, row, rsCrit!Desc
        sprCriteria.SetText 4, row, rsCrit!id
        qryData = qryData & rsCrit!id & " IS NOT null "
        rsCrit.MoveNext
        If Not rsCrit.EOF Then
          qryData = qryData & " OR "
        End If
      Loop
      sprCriteria.MaxRows = rsCrit.RecordCount
      rsCrit.Close
      Set rsCrit = Nothing
      sprCriteria.ColWidthToTwips sprCriteria.ColWidth(0), cwid
      tw = cwid
      sprCriteria.ColWidthToTwips sprCriteria.ColWidth(2), cwid
      tw = tw + cwid
      sprCriteria.ColWidthToTwips sprCriteria.ColWidth(3), cwid
      tw = tw + cwid
      sprCriteria.GetClientArea wid, ht
      sprCriteria.TwipsToColWidth wid - tw, swid
      sprCriteria.ColWidth(1) = swid
      sprCriteria.ReDraw = True
    End If
    
    If cboCASID(0).ListCount > 0 Then
      If cboCASID(0).ListIndex >= 0 Then
        cboCASID_Click (0)
      Else
        cboCASID(0).ListIndex = 0
      End If
    End If
End Sub

Private Sub LoadPropertyWizard()
Dim i As Long
Dim tdf As TableDef
Dim idx As Index
Dim fld As Field
  
  PropertyWizLoaded = True
  
  If mode <> "Select" Then
    ws.BeginTrans
  End If
  
  DB.TableDefs.Refresh
  
On Error Resume Next
  Set tdf = DB.TableDefs(CRITERIA_DEF)
  If Err.Number <> 0 Then
    Err.Clear
    Set tdf = DB.CreateTableDef(CRITERIA_DEF)
    tdf.Fields.Append tdf.CreateField("SName", dbText)
    tdf.Fields.Append tdf.CreateField("Id", dbText)
    tdf.Fields.Append tdf.CreateField("Desc", dbText)
    Set idx = tdf.CreateIndex("CritId")
    idx.Fields.Append idx.CreateField("Id")
    tdf.Indexes.Append idx
    DB.TableDefs.Append tdf
    Set tdf = DB.TableDefs(CRITERIA_DATA)
    If Err.Number <> 0 Then
      Err.Clear
      Set tdf = DB.CreateTableDef(CRITERIA_DATA)
      
      Set fld = tdf.CreateField(CASID, dbText)
      fld.AllowZeroLength = False
      tdf.Fields.Append fld
      
      Set idx = tdf.CreateIndex(CASID)
      idx.Fields.Append idx.CreateField(CASID)
      tdf.Indexes.Append idx
      
      DB.TableDefs.Append tdf
    End If
    DB.TableDefs.Refresh
  End If
  Set tdf = Nothing
     
  For i = 0 To frmCDBE.lstAllCasid.ListCount - 1
    cboCASID(0).AddItem frmCDBE.lstAllCasid.list(i)
  Next
  For i = 0 To frmCDBE.lstAllName.ListCount - 1
    cboName(0).AddItem frmCDBE.lstAllName.list(i)
  Next
  
  Load cboName(1)
  Load cboCASID(1)
  chkViewProp_Click
End Sub

Private Sub ParamClick(row As Long)
  If row < 0 Or row > sprProperty.MaxRows Then Exit Sub
  sprProperty.row = row
  sprProperty.col = 2
  selectedParam = sprProperty.text
  HelpAnchor = selectedParam
  sprProperty.col = 3
  fraTable.Caption = sprProperty.text & " (" & selectedParam & ")"
  RefreshPropertyCriteria
  chkViewCons_Click
End Sub

Public Sub LoadTable(id As String)
Dim i As Long
Dim fRef As String
Dim fName As String
Dim tblData As Recordset

  If sprCriteria.MaxRows > 0 Then
    sprCriteria.row = 1
    sprCriteria.Row2 = sprCriteria.MaxRows
    sprCriteria.col = 2
    sprCriteria.Col2 = 3
    sprCriteria.BlockMode = True
    sprCriteria.Action = 3 ' clear
    sprCriteria.BlockMode = False
  End If
  sprCriteria.row = -1
  sprCriteria.col = 2
  sprCriteria.CellType = 1 ' edit
  
  Set tblData = DB.OpenRecordset(CRITERIA_DATA, dbOpenTable)
  tblData.Fields.Refresh
  tblData.Index = CASID
  tblData.Seek "=", id
  If tblData.NoMatch Then
    tblData.Close
    Exit Sub
  End If
  
  For i = 1 To sprCriteria.MaxRows
    sprCriteria.row = i
    sprCriteria.col = 4
    fName = sprCriteria.text
    fRef = "Ref" & fName
    If Not IsNull(tblData.Fields(fName)) Then
      sprCriteria.SetText 2, i, tblData.Fields(fName)
      sprCriteria.SetText 3, i, tblData.Fields(fRef)
      sprCriteria.col = 2
      If ValidInput(selectedParam, tblData.Fields(fName)) Then
        sprCriteria.BackColor = lightGreen
      Else
        sprCriteria.BackColor = lightRed
      End If
    Else
      sprCriteria.BackColor = vbWhite
    End If
  Next
  tblData.Close
End Sub

Sub SpreadsheetEvent(col As Long, row As Long)
Dim var As Variant

  If Not col = 3 Then Exit Sub
  
  sprCriteria.GetText 2, row, var
  If Len(var) = 0 Then Exit Sub
  
  HelpAnchor = var
  sprCriteria.GetText 3, row, var
  If IsNumeric(var) Then RefIdx = Val(var) Else RefIdx = -1
  RefParam = HelpAnchor
  RefUnload = True
  frmReference.set_refrec
  If Len(DBName) > 0 Then
    IgnoreActivate = True
    frmReference.Show vbModal
    IgnoreActivate = False
    sprCriteria.SetText 3, row, RefIdx
    sprCriteria_EditChange col, row
  Else
    Beep
  End If
End Sub

Private Sub SetParamValue(fName As String, id As String, value As Variant, ref As Variant)
Dim tblData As Recordset

On Error GoTo ErrorHandler
  Set tblData = DB.OpenRecordset(CRITERIA_DATA, dbOpenTable)
  tblData.Index = CASID
  tblData.Seek "=", id
  If tblData.NoMatch Then
    tblData.AddNew
    tblData!CASID = id
  Else
    tblData.Edit
  End If
  tblData.Fields(fName) = value
  tblData.Fields("Ref" & fName) = ref
  tblData.update

ErrorHandler:
  If Err.Number <> 0 Then
    MsgBox Error, vbOKOnly, "SetParamValue"
  End If
  tblData.Close
  Set tblData = Nothing
End Sub
