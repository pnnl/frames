VERSION 5.00
Object = "{48E59290-9880-11CF-9754-00AA00C00908}#1.0#0"; "MSINET.Ocx"
Begin VB.Form frmPropImport 
   Caption         =   "Enivironmental Fate Simulator"
   ClientHeight    =   6270
   ClientLeft      =   60
   ClientTop       =   450
   ClientWidth     =   8295
   LinkTopic       =   "Form1"
   ScaleHeight     =   6270
   ScaleWidth      =   8295
   StartUpPosition =   3  'Windows Default
   Begin VB.CommandButton Command2 
      Caption         =   "Browse"
      Height          =   375
      Left            =   6480
      TabIndex        =   12
      Top             =   240
      Width           =   1575
   End
   Begin VB.TextBox Text3 
      Height          =   360
      Left            =   1800
      TabIndex        =   11
      Top             =   1440
      Width           =   2175
   End
   Begin VB.CommandButton cmdFind 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "Search &Next"
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   375
      Left            =   240
      TabIndex        =   10
      Top             =   1440
      Width           =   1440
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Get Constituent List"
      Height          =   375
      Left            =   6480
      TabIndex        =   9
      Top             =   720
      Width           =   1575
   End
   Begin VB.TextBox Text1 
      Height          =   285
      Left            =   1080
      TabIndex        =   7
      Text            =   "batch-mode-output.xml"
      Top             =   240
      Width           =   5295
   End
   Begin VB.CommandButton Import 
      Caption         =   "Import"
      Height          =   375
      Left            =   6480
      TabIndex        =   6
      Top             =   1200
      Width           =   1575
   End
   Begin VB.CommandButton cmdRemCont 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "<<<  &Remove"
      Enabled         =   0   'False
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   372
      Left            =   4320
      TabIndex        =   5
      Top             =   1920
      Width           =   3750
   End
   Begin VB.CommandButton cmdSelCont 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "&Select  >>>"
      Enabled         =   0   'False
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   360
      Left            =   240
      TabIndex        =   4
      Top             =   1920
      Width           =   3735
   End
   Begin VB.ListBox List2 
      Height          =   3765
      Left            =   4320
      TabIndex        =   2
      Top             =   2280
      Width           =   3735
   End
   Begin VB.ListBox List1 
      Height          =   3765
      Left            =   240
      TabIndex        =   1
      Top             =   2280
      Width           =   3735
   End
   Begin VB.ComboBox Combo1 
      Height          =   315
      ItemData        =   "frmPropImport.frx":0000
      Left            =   240
      List            =   "frmPropImport.frx":0010
      Style           =   2  'Dropdown List
      TabIndex        =   0
      Top             =   960
      Width           =   3735
   End
   Begin InetCtlsObjects.Inet Inet1 
      Left            =   7680
      Top             =   5760
      _ExtentX        =   1005
      _ExtentY        =   1005
      _Version        =   393216
      Protocol        =   4
      URL             =   "http://"
   End
   Begin VB.Label Label2 
      Caption         =   "File"
      Height          =   255
      Left            =   240
      TabIndex        =   8
      Top             =   240
      Width           =   735
   End
   Begin VB.Label Label1 
      Caption         =   "Select the chemical represtation to chose from:"
      Height          =   255
      Left            =   240
      TabIndex        =   3
      Top             =   600
      Width           =   3735
   End
End
Attribute VB_Name = "frmPropImport"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Dim clsDoc As New DOMDocument
Dim cldDoc As New DOMDocument
Dim chemListNode As IXMLDOMElement
Dim chemNode As IXMLDOMElement
Dim table As IXMLDOMElement
Dim errImport As String

Private Sub cmdFind_Click()
Dim found As Boolean

  found = list_find_again(List1, cmdFind)

End Sub

Private Sub Command1_Click()
  Dim url As String
  Dim xml As String
  
'  url = Text1.text + "/ChemicalList.jsp"
'  xml = Inet1.OpenURL(url)
'  If clsDoc.loadXML(xml) Then
'    Combo1.ListIndex = 0
'  End If
  
  If clsDoc.Load(Text1.text) Then
    Combo1.ListIndex = 0
  End If
End Sub

Private Sub Combo1_Click()
Dim i As Integer
Dim j As Integer
Dim d As Integer
Dim kind As String
Dim name As String


  If Combo1.ListIndex < 0 Or clsDoc.xml = "" Then Exit Sub
  d = 0
  List1.Clear
  Set chemListNode = clsDoc.selectSingleNode("/constituents")
  For i = 0 To chemListNode.childNodes.length - 1
    Set table = chemListNode.childNodes.item(i).selectSingleNode("table[@name='Classification']")
    If Not table Is Nothing Then
      For j = 0 To table.childNodes.length - 1
        kind = table.childNodes.item(j).Attributes.getNamedItem("property").text
        name = table.childNodes.item(j).Attributes.getNamedItem("value").text
        If Combo1.text = kind Then
          List1.AddItem name, d
          List1.itemdata(List1.NewIndex) = i
        End If
      Next
    End If
  Next
End Sub

Private Sub Command2_Click()
Dim newdbName As String

  MousePointer = vbHourglass
  newdbName = GetFileName(DB_OPENXML)
  If GoodFileXML(newdbName) Then
    Text1 = newdbName
  End If
  MousePointer = vbDefault

End Sub

Private Sub Import_Click()
Dim i As Integer
Dim j As Integer
Dim k As Integer
Dim m As Integer
Dim qry As String
Dim xml As String
Dim kind As String
Dim prp As String
Dim des As String
Dim unt As String
Dim val As String
Dim ref As IXMLDOMElement
Dim tmp As String
Dim tbl As String
Dim cas As ContStruct
Dim rs As Recordset
Dim rfnum As Integer
Dim sName As String
Dim lName As String
Dim Match As Boolean
Dim saveProp As Boolean
Dim chemImport As Boolean

  
  Set rs = DB.OpenRecordset("SELECT MAX(RefNum) FROM REF")
  If Not IsNull(rs.Fields(0)) Then rfnum = rs.Fields(0)
  
  errImport = ""
  For m = 0 To chemListNode.childNodes.length - 1
    chemImport = False
    For i = 0 To List2.ListCount - 1
      If List2.itemdata(i) = m Then chemImport = True
    Next
    If chemImport Then
      Set chemNode = chemListNode.childNodes.item(m)
'      cas.id = ""
      tmp = ""
      Match = False
      For k = 0 To chemNode.childNodes.length - 1
        Set table = chemNode.childNodes.item(k)
        Select Case table.baseName
        Case "attribute":
        Case "table":
          tbl = table.Attributes.getNamedItem("name").text
          If "classification" = tbl Then
'            cas.cltype(KTYPE) = IIf(chkRadionuclide.value = 1, 1, 0)
            cas.cltype(KTYPE) = 0
            cas.cltype(PTYPE) = 0          'undefined
            cas.cltype(CTYPE) = 0          'undefined
            If cas.cltype(KTYPE) = 1 Then  'radionuclide
              cas.cltype(RTYPE) = 14       'radionuclide
              cas.cltype(ETYPE) = 1        'radionclide exposure
            Else                           'chemical
              cas.cltype(RTYPE) = 0        'undefined
              cas.cltype(ETYPE) = 5        'default exposure type
            End If
            
            For j = 0 To table.childNodes.length - 1
              prp = table.childNodes.item(j).Attributes.getNamedItem("property").text
              des = table.childNodes.item(j).Attributes.getNamedItem("description").text
              unt = table.childNodes.item(j).Attributes.getNamedItem("units").text
              val = table.childNodes.item(j).Attributes.getNamedItem("value").text
'              ref = table.childNodes.item(j).Attributes.getNamedItem("property").text
              Select Case prp
                Case "name": cas.name = val
                Case "casid":
                  tmp = Replace(val, "-", "")
                  tblChems.index = "Primary"
                  tblChems.Seek "=", tmp
                  If Not tblChems.NoMatch Then
                    Match = True
'                    cas.id = tmp
                  End If
                  
                Case "smiles":
                Case "iupac":
                Case "clchem": cas.cltype(CTYPE) = val
                Case "cletype": cas.cltype(ETYPE) = val
                Case "clrtype": cas.cltype(RTYPE) = val
                Case "clptype": cas.cltype(PTYPE) = val
              End Select
            Next
            cas.numprog = 0
            
            'No casid, use chemNode name
            If tmp = "" Then
              tmp = Replace(chemNode.Attributes.getNamedItem("name").text, "-", "")
              tblChems.index = "Primary"
              tblChems.Seek "=", tmp
              If Not tblChems.NoMatch Then
                Match = True
'                cas.id = tmp
              End If
            End If
            
            If Not Match Then
              cas.id = tmp
              tblChems.AddNew
              tblChems!CASID = cas.id
              tblChems!name = Left(cas.name, 40)
              tblChems!CLKTYPE = cas.cltype(KTYPE)
              tblChems!CLCHEM = cas.cltype(CTYPE)
              tblChems!CLPTYPE = cas.cltype(PTYPE)
              tblChems!CLETYPE = cas.cltype(ETYPE)
              tblChems!CLRTYPE = cas.cltype(RTYPE)
              tblChems.update
              
              tblChems.index = "Primary"
              tblChems.Seek "=", cas.id
              
              frmCDBE.lstAllName.AddItem cas.name
              frmCDBE.lstAllName.itemdata(frmCDBE.lstAllName.NewIndex) = tblChems!rowid
              frmCDBE.lstAllCasid.AddItem cas.id
              frmCDBE.lstAllCasid.itemdata(frmCDBE.lstAllCasid.NewIndex) = tblChems!rowid
              
              If frmCDBE.chkShowCasid Then
                frmCDBE.lstCon.AddItem cas.id
              Else
                frmCDBE.lstCon.AddItem cas.name
              End If
              frmCDBE.lstCon.itemdata(frmCDBE.lstCon.NewIndex) = tblChems!rowid
              frmCDBE.lstCon.ListIndex = frmCDBE.lstCon.NewIndex
            End If
          Else
            For j = 0 To table.childNodes.length - 1
              'save property
              prp = table.childNodes.item(j).Attributes.getNamedItem("property").text
              des = table.childNodes.item(j).Attributes.getNamedItem("description").text
              unt = table.childNodes.item(j).Attributes.getNamedItem("units").text
              val = table.childNodes.item(j).Attributes.getNamedItem("value").text
              Set ref = table.childNodes.item(j).selectSingleNode("sName")
              
              If ref Is Nothing Then
                saveProp = SetImportValue(prp, cas.id, val, 0)
              Else
                saveProp = SetImportValue(prp, cas.id, val, rfnum)
                
                If saveProp Then
                 'save ref
                  sName = table.childNodes.item(j).selectSingleNode("sName").text
                  lName = table.childNodes.item(j).selectSingleNode("lName").text
                  
                  rfnum = rfnum + 1
                  tblRef.AddNew
                  tblRef.Fields(0) = rfnum
                  tblRef.Fields(1) = sName
                  tblRef.Fields(2) = lName
                  tblRef.update
                End If
              End If
            Next
          End If
        End Select
      Next
    End If
  Next
  
  If (errImport <> "") Then MsgBox errImport, vbOKOnly, "Import Status"

  
End Sub

Private Function SetImportValue(fldName As String, id As String, value As Variant, ref As Variant) As Boolean
Dim tbl As Recordset

On Error GoTo ErrorHandler
  
  Set tbl = GetImportTable(fldName)
  tbl.Seek "=", id
  If tbl.NoMatch Then
    tbl.AddNew
    tbl!CASID = id
  Else
    tbl.Edit
  End If
  tbl.Fields(fldName) = value
  tbl.Fields("Ref" & fldName) = ref
  tbl.update

ErrorHandler:
  If Err.Number <> 0 Then
    errImport = errImport & "SetImportValue " & fldName & ":" & id & vbCrLf
    SetImportValue = False
  End If
  SetImportValue = True
End Function

Private Function GetImportTable(fldName As String) As Recordset

On Error GoTo ErrorHandler

  tblParam.Seek "=", fldName
  tblCat.Seek "=", tblParam!Category
  Set GetImportTable = coldata(tblCat!Category)

ErrorHandler:
  If Err.Number <> 0 Then
    errImport = errImport & "GetImportTable " & fldName & vbCrLf
  End If
End Function

Private Sub cmdRemCont_Click()
  If List2.ListIndex > -1 Then
    List2.RemoveItem List2.ListIndex
  End If
End Sub

Private Sub cmdSelCont_Click()
Dim i As Integer

  For i = 0 To List2.ListCount - 1
    If List1.text = List2.list(i) Then
      Exit Sub
    End If
  Next
  List2.AddItem List1.text
  List2.itemdata(List2.NewIndex) = List1.itemdata(List1.ListIndex)
End Sub

Private Sub List1_Click()
  If List2.ListCount > -1 Then
    cmdSelCont.Enabled = True
  Else
    cmdSelCont.Enabled = False
  End If
End Sub

Private Sub List1_DblClick()
  cmdSelCont_Click
End Sub

Private Sub List2_Click()
  If List2.ListCount > -1 Then
    cmdRemCont.Enabled = True
  Else
    cmdRemCont.Enabled = False
  End If
End Sub

Private Sub List2_DblClick()
  cmdRemCont_Click
End Sub


Private Sub Text3_Change()
  If Len(Text3.text) = 0 Then
    cmdFind.Enabled = False
    Exit Sub
  End If
  SetFindString Text3.text
  list_find List1, cmdFind, Text3
End Sub
