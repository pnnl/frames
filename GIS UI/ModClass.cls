VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ModClass"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Compare Text

'local variable(s) to hold property value(s)
Private mvarname As String
Private mvarid As String 'local copy
Private mvardespath As String 'local copy
Private mvartvwindex As Long 'local copy
Private mvarnumqry As Long 'local copy
Private mvarnumobj As Long

Private qry() As QryClass

Public Property Let numobj(ByVal vdata As Long)
  mvarnumobj = vdata
End Property

Public Property Get numobj() As Long
  numobj = mvarnumobj
End Property

Public Property Let name(ByVal vdata As String)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.id = 5
    mvarname = vdata
End Property


Public Property Get name() As String
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.id
    name = mvarname
End Property



Public Property Let numqry(ByVal vdata As Long)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.numqry = 5
    mvarnumqry = vdata
    ReDim Preserve qry(mvarnumqry)
End Property


Public Property Get numqry() As Long
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.numqry
    numqry = mvarnumqry
End Property



Public Property Let tvwindex(ByVal vdata As Long)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.tvwindex = 5
    mvartvwindex = vdata
End Property


Public Property Get tvwindex() As Long
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.tvwindex
    tvwindex = mvartvwindex
End Property



Public Property Let despath(ByVal vdata As String)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.despath = 5
    mvardespath = vdata
End Property


Public Property Get despath() As String
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.despath
    despath = mvardespath
End Property



Public Property Let id(ByVal vdata As String)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.id = 5
    mvarid = vdata
End Property


Public Property Get id() As String
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.id
    id = mvarid
End Property

Public Function loadDes(path As String) As Boolean
Dim fle As csv
Dim i As Long, j As Long, k As Long, pos As Long
'Dim v As descriptor
Dim pCnt As Long
Dim varCnt As Long
Dim ver As String, nvers As Single
Dim Class As String, group As String, id As String
Dim chk As String
Dim curDrive As String

  curDrive = CurDir
  'ChDrive App.path

  
  If open_csv(fle, path, 2) Then
    chk = get_val(fle)                 'des descriptor
    If chk <> "mf" Then GoTo loadDesErr
    ver = get_val(fle)                 'version descriptor
'   loadGidData glyph.id
    
    If (InStr(ver, "beta")) Then GoTo loadDesErr
  
    ' assumes version appears as "Version n[.n]"
    If 0 < InStr(ver, "Version") Then
      nvers = Val(Mid(ver, InStr(ver, " ")))
    Else
      GoTo loadDesErr
    End If
  
    If nvers < 2# Then GoTo loadDesErr
    
  ' skip header information
    get_line fle
    get_line fle
    chk = get_val(fle)
    get_line fle
    pCnt = Val(get_val(fle))
    For i = 0 To pCnt
      get_line fle
    Next
  
  ' skip module connection information
    pCnt = Val(get_val(fle))
    For i = 0 To pCnt
      get_line fle
    Next
  
  ' skip sensitivity variable descriptions
    varCnt = Val(get_val(fle))
    get_line fle
    For i = 1 To varCnt
      chk = get_val(fle)
      chk = get_val(fle)
      chk = get_val(fle)
      chk = get_val(fle): If chk = "MIN" Then chk = get_val(fle)
      chk = get_val(fle): If chk = "MAX" Then chk = get_val(fle)
      chk = get_val(fle)
      pCnt = Val(get_val(fle))
      For j = 0 To pCnt
        get_line fle
      Next j
    Next i
    
'   get_line fle
  ' read spatial data requirements
    numqry = Val(get_val(fle)):  get_line fle
    For i = 1 To numqry
      Set qry(i) = New QryClass
'     qry(i).loadDes (fle) ???  can't pass user define structures to class method ???
      With qry(i)
        'status
        'tvwindex
        .prompt = get_val(fle)
        .label = get_val(fle)
        .objtype = get_val(fle)
        Select Case .objtype
          Case TCGRID, TPGRID, TPTGRID:
            .min = Val(get_val(fle))
            .max = Val(get_val(fle))
          Case Else:
            .min = 1: get_val fle
            .max = 1: get_val fle
        End Select
        .dicpath = get_val(fle)
        .numobj = 0
        get_line fle
      End With
    Next i
    
    close_csv fle
    ChDrive curDrive
    loadDes = True
  Else
    put_val errfile, "Can't find or open file " & glyph.path
    put_line errfile
    AnError = True
loadDesErr:
    put_val errfile, "Error in Description file for module " & glyph.id
    put_line errfile
'   close_csv errfile
    AnError = True
    ChDrive curDrive
    loadDes = False
  End If
End Function

Public Function Load(path As String) As Boolean
Dim nod As Node, tvw As TreeView, i As Long, l As Long, j As Long
  Set tvw = frmMain.TreeView1
  Load = False
  If Not loadDes(path) Then Exit Function
  
  Set nod = tvw.Nodes.Add(, , id, "Module: " & name & " (" & id & ")")
  nod.EnsureVisible ' Show all nodes.
  nod.Image = "red"
  
  For i = 1 To numqry
    Set nod = tvw.Nodes.Add(id, tvwChild, id & qry(i).label, qry(i).prompt & " (" & qry(i).objtype & ")")
    nod.EnsureVisible
    nod.Image = "red"
    qry(i).tvwindex = nod.Index
    qry(i).tvwkey = id & qry(i).label
    For j = 1 To qry(i).numobj
      Set nod = tvw.Nodes.Add(id & qry(i).label, tvwChild, id & qry(i).label & qry(i).GetObjId(j), qry(i).GetObjId(j))
    Next j
  Next i
      
  Load = True
End Function

Public Function DisplayTreeviewMenu(key As String)
Dim i As Long
  For i = 1 To numqry
    If qry(i).tvwkey = Left(key, Len(qry(i).tvwkey)) Then
      SelectedEntityId = qry(i).GetObjId(1)
      SelectedLayer = qry(i).GetObjLayer(1)
      frmMain.PopupMenu frmMain.mnuTreeview
      If KeyPadSel = KPS_DELETE Then
        frmMain.TreeView1.Nodes.Remove key
        SelectDbgr SelectedLayer, True
        frmMain.Dbcocx1.devgrdisplayid SelectedEntityId, 256
        qry(i).numobj = 0
      End If
      If KeyPadSel = KPS_DATA Then
        frmData.Show 1
      End If
    End If
  Next i
End Function
Public Function GetData(key As String, objtype As String) As Boolean
Dim i As Long
  For i = 1 To UBound(qry)
    If qry(i).tvwkey = key Or qry(i).tvwkey = Left(key, Len(qry(i).tvwkey)) Then
      qry(i).GetData key, objtype
    End If
  Next i
End Function


Public Function DisplayData(key As String) As Boolean
Dim i As Long
  For i = 1 To numqry
    If qry(i).tvwkey = key Or qry(i).tvwkey = Left(key, Len(qry(i).tvwkey)) Then
      qry(i).DisplayData key
      Exit For
    End If
  Next i
End Function


Public Function SaveGidObj(countonly As Boolean, l&, numGeoObj&, _
  numPoly&, numLine&, numPt&, numPolar&, numCart&, numPtGrid&) As Long
Dim parm As parmrec ' pfile is global in module1.bas
Dim i As Long, j As Long, nobj As Long
  For i = 1 To numqry
    nobj = nobj + qry(i).SaveGidObj(countonly, id, numGeoObj, numPoly, numLine, numPt, _
                                      numPolar, numCart, numPtGrid)
  Next i
  numobj = nobj
  SaveGidObj = numobj
End Function


Public Function SaveGidMod(l As Long)
Dim i As Long, parm As parmrec, ct As Long
    set_parm parm, "ModuleId", l, 0, 0, 0, 0, 0, 0, "n/a", "n/a", id
    write_parmrec pfile, parm
    set_parm parm, "NumModGeoObj", l, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(numobj)
    write_parmrec pfile, parm
    For i = 1 To module(l).numqry
'     qry(i).SaveGidMod l, i, ct
      qry(i).SaveGidMod l, i, ct
    Next i
End Function

Public Function EntityInUse() As Boolean
Dim i As Long, inuse As Boolean
  For i = 1 To numqry
    inuse = inuse Or qry(i).EntityInUse()
  Next i
  EntityInUse = inuse
End Function

Public Function UpdateTreeviewStatus() As Boolean
Dim i As Long, complete As Boolean
  complete = True
  For i = 1 To numqry
    complete = complete And qry(i).UpdateTreeviewStatus
  Next i
  If complete Then frmMain.TreeView1.Nodes(id).Image = "green" Else frmMain.TreeView1.Nodes(id).Image = "red"
  UpdateTreeviewStatus = complete
End Function

Public Sub LoadGid(m As Long)
Dim i As Long
  For i = 1 To numqry
    qry(i).LoadGid id, i
  Next i
End Sub
