VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "QryClass"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Compare Text
Option Explicit

Private mvarstatus As Integer
Private mvartvwindex As Long
Private mvarprompt As String
Private mvarlabel As String
Private mvarobjtype As String
Private mvarmin As Long
Private mvarmax As Long
Private mvardicpath As String
Private mvarnumobj As Long
Private mvartvwkey As String
Private objid() As String
Private objlayer() As Long
Private objindex() As Long
Private exclpt() As String ' points excluded from point grid

Public Property Let tvwkey(ByVal vdata As String)
  mvartvwkey = vdata
End Property

Public Property Get tvwkey() As String
  tvwkey = mvartvwkey
End Property

Public Property Let numobj(ByVal vdata As Long)
    mvarnumobj = vdata
    ReDim objid(mvarnumobj)
    ReDim objlayer(mvarnumobj)
    ReDim objindex(mvarnumobj)
End Property


Public Property Get numobj() As Long
    numobj = mvarnumobj
End Property



Public Property Let dicpath(ByVal vdata As String)
    mvardicpath = vdata
End Property


Public Property Get dicpath() As String
    dicpath = mvardicpath
End Property



Public Property Let max(ByVal vdata As Long)
    mvarmax = vdata
End Property


Public Property Get max() As Long
    max = mvarmax
End Property


Public Property Let min(ByVal vdata As Long)
    mvarmin = vdata
End Property


Public Property Get min() As Long
    min = mvarmin
End Property


Public Property Let objtype(ByVal vdata As String)
    mvarobjtype = vdata
End Property


Public Property Get objtype() As String
    objtype = mvarobjtype
End Property


Public Property Let label(ByVal vdata As String)
    mvarlabel = vdata
End Property


Public Property Get label() As String
    label = mvarlabel
End Property


Public Property Let prompt(ByVal vdata As String)
    mvarprompt = vdata
End Property


Public Property Get prompt() As String
    prompt = mvarprompt
End Property


Public Property Let tvwindex(ByVal vdata As Long)
    mvartvwindex = vdata
End Property


Public Property Get tvwindex() As Long
    tvwindex = mvartvwindex
End Property


Public Property Let status(ByVal vdata As Integer)
    mvarstatus = vdata
End Property


Public Property Get status() As Integer
    status = mvarstatus
End Property


Public Function GetData(key As String, otype As String) As Boolean
Dim i As Long, msg As String

  If key = tvwkey Then
    SelectDbgr 0, True
    DisableUI True
    Select Case objtype
      Case TPOINT:
        GetData = GetDataPoint
      Case TPOLYLINE:
        GetData = GetDataLine
      Case TPOLYGON:
        GetData = GetDataPolygon
      Case TRECTANGLE:
        GetData = GetDataRectangle
      Case TPGRID:
        GetData = GetGridPolar
      Case TCGRID:
        GetData = GetGridCartesian
      Case TPTGRID:
        GetData = GetGridPoint
    End Select
    DisableUI False
  Else
    For i = 1 To numobj
      If key = (tvwkey & GetObjKey(i)) Then
        SelectedEntityId = GetObjId(i)
        SelectedLayer = GetObjLayer(i)
        frmData.Show 1
        Exit For
      End If
    Next i
  End If
End Function

Public Function GetObjIndex(i As Long) As Long
  GetObjIndex = objindex(i)
End Function

Public Sub SetObjIndex(i As Long, ix As Long)
  objindex(i) = ix
End Sub

Public Function GetObjKey(i As Long) As String
  GetObjKey = objid(i) & GetLayerKey(objlayer(i))
End Function

Public Function GetObjLayer(i As Long) As Long
  GetObjLayer = objlayer(i)
End Function

Public Function GetObjId(i As Long) As String
  GetObjId = objid(i)
End Function


Public Sub SetObjId(i As Long, id As String, l As Long)
  objid(i) = Trim(id)
  objlayer(i) = l
End Sub

Public Function DisplayData(key As String) As Boolean
Dim dbc As Dbcocx, i As Long, msg As String, npt As Long

  RefreshGrDisplay

  Set dbc = frmMain.Dbcocx1
  For i = 1 To numobj
    If key = tvwkey Then
      SelectedEntityId = GetObjId(i)
      SelectedEntityLayer = GetObjLayer(i)
      SelectDbgr SelectedEntityLayer, True
      dbc.devgrdisplayid SelectedEntityId, 1 ' red
      If objtype = TPTGRID Then
        npt = CountPointGridPoints(dbc, SelectedEntityId, SelectedEntityLayer)
        DisplayStatus FormatPointGridMessage(npt, min, max)
      Else
        DisplayStatus ""
      End If
      Exit For
    Else
      If key = (tvwkey & GetObjKey(i)) Then
        SelectedEntityId = GetObjId(i)
        SelectedEntityLayer = GetObjLayer(i)
        SelectDbgr SelectedEntityLayer, True
        dbc.devgrdisplayid SelectedEntityId, 1 ' red
        If objtype = TPTGRID Then
          npt = CountPointGridPoints(dbc, SelectedEntityId, SelectedEntityLayer)
          DisplayStatus FormatPointGridMessage(npt, min, max)
        Else
          DisplayStatus ""
        End If
        Exit For
      End If
    End If
  Next i

End Function


Private Function GetGridPoint() As Boolean
Dim msg As String
Dim insx As Double, insy As Double
Dim id As String: id = ""
Dim i As Long
Dim lst As ListBox
Dim dbc As Dbcocx
Dim FirstFind As Long

  Set dbc = frmMain.Dbcocx1
  Set lst = frmMain.List1
  lst.Clear
  
  For i = 1 To numobj
    lst.AddItem GetObjId(i)
    lst.ItemData(lst.NewIndex) = GetObjLayer(i)
  Next i
      
  msg = "Select/Deselect a polygon enclosing the grid points, or click on coordinate to create (right button to end):"
  GetPoint msg, insx, insy
  While dbc.devlastkey() = 13
    SelectedEntityId = ""
    If frmMain.DevgrseekV2(insx, insy, TPOLYGON) Then
        FirstFind = dbc.devrecno()
        SelectedEntityId = Trim(dbc.devgetid())
        SelectedEntityLayer = SelectedLayer
        ' if already selected then de-select
        Dim found As Boolean
        found = False
        For i = 0 To lst.ListCount - 1
          If lst.List(i) = SelectedEntityId Then
            found = True
            lst.RemoveItem (i)
            dbc.devgrdisplayid SelectedEntityId, 256
          End If
        Next i
    End If
    If SelectedEntityId = "" Or (SelectedEntityId <> "" And Not found) Then
      If lst.ListCount = 1 Then
        MsgBox "Maximum exceeded, delete a selection or end"
        dbc.devgrdisplayid SelectedEntityId, 256
      Else
        If SelectedEntityId = "" Then
          SelectedEntityLayer = 0
          SelectDbgr SelectedEntityLayer, True
          SelectedEntityId = InsertPolygon(insx, insy)
          If Not ConfirmPointGridPoints(SelectedEntityId, SelectedEntityLayer, min, max) Then
            SelectedEntityId = ""
          End If
        Else
          If Not ConfirmPointGridPoints(SelectedEntityId, SelectedEntityLayer, min, max) Then
            SelectedEntityId = ""
          End If
        End If
        If SelectedEntityId <> "" Then
          dbc.devgrdisplayid SelectedEntityId, 1
          lst.AddItem SelectedEntityId
          lst.ItemData(lst.NewIndex) = SelectedEntityLayer
        End If
      End If
    End If
    GetPoint msg, insx, insy
  Wend
    
  Dim nod As Node
  Set nod = frmMain.TreeView1.Nodes(tvwindex)
  For i = 1 To nod.Children
    frmMain.TreeView1.Nodes.Remove tvwkey & GetObjKey(i)
  Next i
  numobj = lst.ListCount
  For i = 1 To numobj
    SetObjId i, lst.List(i - 1), lst.ItemData(i - 1)
    Set nod = frmMain.TreeView1.Nodes.Add(tvwkey, tvwChild, tvwkey & GetObjKey(i), GetObjKey(i))
    nod.EnsureVisible
  Next i
  
End Function

Private Function GetDataRectangle() As Boolean
Dim msg As String
Dim insx As Double, insy As Double, x0 As Double, y0 As Double, x1 As Double, y1 As Double
Dim id As String: id = ""
Dim i As Long, l As Long
Dim lst As ListBox
Dim dbc As Dbcocx
Dim found As Boolean
Dim FirstFind As Long
Dim tempId As String

  Set dbc = frmMain.Dbcocx1
  Set lst = frmMain.List1
  lst.Clear
  
  For i = 1 To numobj
    lst.AddItem GetObjId(i)
    lst.ItemData(lst.NewIndex) = GetObjLayer(i)
  Next i
      
      msg = "Select/Deselect a rectangle or click on coordinate to create (right button to end):"
      GetPoint msg, insx, insy
      While dbc.devlastkey() = 13
        SelectedEntityId = ""
        If frmMain.DevgrseekV2(insx, insy, TRECTANGLE) Then
          SelectedEntityId = Trim(dbc.devgetid())
          ' if already selected then de-select
          found = False
          For i = 0 To lst.ListCount - 1
            If lst.List(i) = SelectedEntityId And lst.ItemData(i) = SelectedLayer Then
              found = True
              lst.RemoveItem (i)
              dbc.devgrdisplayid SelectedEntityId, 256
            End If
          Next i
        End If
        If SelectedEntityId = "" Or (SelectedEntityId <> "" And Not found) Then
          If lst.ListCount = max Then
            MsgBox "Maximum exceeded, delete a selection or end"
            dbc.devgrdisplayid SelectedEntityId, 256
          Else
            If SelectedEntityId = "" Then
              SelectDbgr 0, True
              GetPointDrag "Click on opposite corner to insert Rectangle (right button to cancel)", x1, y1, insx, insy, 2
              If dbc.devlastkey() <> 27 Then ' right button
                SelectedEntityId = InsertRectangleIntoDb(dbc, insx, insy, x1, y1)
              End If
            End If
            If SelectedEntityId <> "" Then
              dbc.devgrdisplayid SelectedEntityId, 1
              lst.AddItem SelectedEntityId
              lst.ItemData(lst.NewIndex) = SelectedLayer
            End If
          End If
        End If
        GetPoint msg, insx, insy
      Wend
      
      Dim nod As Node
      Set nod = frmMain.TreeView1.Nodes(tvwindex)
      For i = 1 To nod.Children
        frmMain.TreeView1.Nodes.Remove tvwkey & GetObjKey(i)
      Next i
      numobj = lst.ListCount
      For i = 1 To numobj
        SetObjId i, lst.List(i - 1), lst.ItemData(i - 1)
        Set nod = frmMain.TreeView1.Nodes.Add(tvwkey, tvwChild, tvwkey & GetObjKey(i), GetObjKey(i))
        nod.EnsureVisible
      Next i
End Function

Private Function GetDataPolygon() As Boolean
Dim msg As String
Dim insx As Double, insy As Double
Dim id As String: id = ""
Dim i As Long
Dim lst As ListBox
Dim dbc As Dbcocx
Dim FirstFind As Long

  Set dbc = frmMain.Dbcocx1
  Set lst = frmMain.List1
  lst.Clear
  
  For i = 1 To numobj
    lst.AddItem GetObjId(i)
    lst.ItemData(lst.NewIndex) = GetObjLayer(i)
  Next i
      
      msg = "Select/Deselect a polygon or click on coordinate to create (right button to end):"
      GetPoint msg, insx, insy
      While dbc.devlastkey() = 13
        SelectedEntityId = ""
        If frmMain.DevgrseekV2(insx, insy, TPOLYGON) Then
          FirstFind = dbc.devrecno()
          SelectedEntityId = Trim(dbc.devgetid())
          
          ' if already selected then de-select
          Dim found As Boolean
          found = False
          For i = 0 To lst.ListCount - 1
            If lst.List(i) = SelectedEntityId Then
              found = True
              lst.RemoveItem (i)
              dbc.devgrdisplayid SelectedEntityId, 256
            End If
          Next i
        End If
        If SelectedEntityId = "" Or (SelectedEntityId <> "" And Not found) Then
          If lst.ListCount = max Then
            MsgBox "Maximum exceeded, delete a selection or end"
            dbc.devgrdisplayid SelectedEntityId, 256
          Else
            If SelectedEntityId = "" Then
              SelectDbgr 0, True
              SelectedEntityId = InsertPolygon(insx, insy)
            End If
            If SelectedEntityId <> "" Then
              dbc.devgrdisplayid SelectedEntityId, 1
              lst.AddItem SelectedEntityId
              lst.ItemData(lst.NewIndex) = SelectedLayer
            End If
          End If
        End If
        GetPoint msg, insx, insy
      Wend
      
      Dim nod As Node
      Set nod = frmMain.TreeView1.Nodes(tvwindex)
      For i = 1 To nod.Children
        frmMain.TreeView1.Nodes.Remove tvwkey & GetObjKey(i)
      Next i
      numobj = lst.ListCount
      For i = 1 To numobj
        SetObjId i, lst.List(i - 1), lst.ItemData(i - 1)
        Set nod = frmMain.TreeView1.Nodes.Add(tvwkey, tvwChild, tvwkey & GetObjKey(i), GetObjKey(i))
        nod.EnsureVisible
      Next i
End Function

Private Function GetDataLine() As Boolean
Dim msg As String
Dim insx As Double, insy As Double, x As Double, y As Double
Dim id As String: id = ""
Dim i As Long
Dim lst As ListBox
Dim dbc As Dbcocx
Dim FirstFind As Long
Dim found As Boolean

  Set dbc = frmMain.Dbcocx1
  Set lst = frmMain.List1
  lst.Clear
  
  For i = 1 To numobj
    lst.AddItem GetObjId(i)
    lst.ItemData(lst.NewIndex) = GetObjLayer(i)
  Next i
      
  msg = "Select/Deselect a line or click on coordinate to create (right button to end):"
  GetPoint msg, insx, insy
  While dbc.devlastkey() = 13
    SelectedEntityId = ""
    If frmMain.DevgrseekV2(insx, insy, TPOLYLINE) Then
      FirstFind = dbc.devrecno()
      SelectedEntityId = Trim(dbc.devgetid())
      
      ' if already selected then de-select
      found = False
      For i = 0 To lst.ListCount - 1
        If lst.List(i) = SelectedEntityId Then
          found = True
          lst.RemoveItem (i)
          dbc.devgrdisplayid SelectedEntityId, 256
        End If
      Next i
    End If
    If SelectedEntityId = "" Or (SelectedEntityId <> "" And Not found) Then
      If lst.ListCount = max Then
        MsgBox "Maximum exceeded, delete a selection or end"
        dbc.devgrdisplayid SelectedEntityId, 256
      Else
        If SelectedEntityId = "" Then
          SelectDbgr 0, True  ' select user layer
          While GetPointDrag("Click on vertex to create (right button to cancel):", x, y, insx, insy, 1) = 13
            dbc.devgrcmdline id, insx, insy, x, y
            id = dbc.devgetid()
            dbc.devgrdisplayid id, 256
            insx = x
            insy = y
          Wend
          SelectedEntityId = Trim(id)
        End If
        If SelectedEntityId <> "" Then
          dbc.devgrdisplayid SelectedEntityId, 1
          lst.AddItem SelectedEntityId
          lst.ItemData(lst.NewIndex) = SelectedLayer
        End If
      End If
    End If
    GetPoint msg, insx, insy
  Wend
  
  Dim nod As Node
  Set nod = frmMain.TreeView1.Nodes(tvwindex)
  For i = 1 To nod.Children
    frmMain.TreeView1.Nodes.Remove tvwkey & GetObjKey(i)
  Next i
  numobj = lst.ListCount
  For i = 1 To numobj
    SetObjId i, lst.List(i - 1), lst.ItemData(i - 1)
    Set nod = frmMain.TreeView1.Nodes.Add(tvwkey, tvwChild, tvwkey & GetObjKey(i), GetObjKey(i))
    nod.EnsureVisible
  Next i
End Function

Private Function GetGridPolar() As Boolean
Dim msg As String
Dim insx As Double, insy As Double
Dim id As String: id = ""
Dim i As Long
Dim lst As ListBox
Dim dbc As Dbcocx
Dim frm As Form
Dim blockid As String: blockid = ""
Dim found As Boolean
Dim desc As String
Dim FirstFind As Long

  Set dbc = frmMain.Dbcocx1
  Set lst = frmMain.List1
  lst.Clear
  
  For i = 1 To numobj
    lst.AddItem GetObjId(i)
    lst.ItemData(lst.NewIndex) = GetObjLayer(i)
  Next i
      
  SelectDbgr 0, True ' polar will only exist on user coverage
  
  msg = "Select/Deselect a Polar Grid or click on center coordinate to create (right button to end):"
  GetPoint msg, insx, insy
  Do While dbc.devlastkey() = 13
    dbc.Devgrseek insx, insy, 0
    SelectedEntityId = ""
    If dbc.devfound() <> 0 Then
      If Left(dbc.devgetfield("DESCR"), 6) = "INSERT" Then
        blockid = Trim(Mid$(dbc.devgetfield("DESCR"), 7))
        
        FirstFind = dbc.devrecno() ' save record number
        
        ' does this block CGrid... entity have the required dimensions ?
        dbc.devseek "N" & blockid
        If dbc.devfound() <> 0 Then
          desc = "PGrid." & min & "." & max
          If Left(dbc.devgetfield("DESCR"), Len(desc)) = desc Then
            dbc.devgorecno FirstFind
            SelectedEntityId = Trim(dbc.devgetid())
            
          ' if already selected then de-select
            found = False
            For i = 0 To lst.ListCount - 1
              If lst.List(i) = SelectedEntityId Then
                found = True
                lst.RemoveItem (i)
                dbc.devgrdisplayid SelectedEntityId, 256
              End If
            Next i
          End If
        End If
      End If
    End If
    If SelectedEntityId = "" Or (SelectedEntityId <> "" And Not found) Then
      If lst.ListCount = 1 Then
        MsgBox "Maximum exceeded, delete a selection or end"
        dbc.devgrdisplayid SelectedEntityId, 256
      Else
        If SelectedEntityId = "" Then
            Dim x1 As Double, y1 As Double
            GetPointDrag "Click at radius to create grid (right button to cancel)", x1, y1, insx, insy, 1
            If 27 <> dbc.devlastkey() Then
              SelectedEntityId = InsertPGridIntoDb(dbc, insx, insy, x1, y1)
            End If
        End If
        If SelectedEntityId <> "" Then
          dbc.devgrdisplayid SelectedEntityId, 1
          lst.AddItem SelectedEntityId
          lst.ItemData(lst.NewIndex) = SelectedLayer
        End If
      End If
    End If
    GetPoint msg, insx, insy
  Loop
      
  Dim nod As Node
  Set nod = frmMain.TreeView1.Nodes(tvwindex)
  For i = 1 To nod.Children
    frmMain.TreeView1.Nodes.Remove tvwkey & GetObjKey(i)
  Next i
  numobj = lst.ListCount
  For i = 1 To numobj
    SetObjId i, lst.List(i - 1), lst.ItemData(i - 1)
    Set nod = frmMain.TreeView1.Nodes.Add(tvwkey, tvwChild, tvwkey & GetObjKey(i), GetObjKey(i))
    nod.EnsureVisible
  Next i
End Function

Private Function GetGridCartesian() As Boolean
Dim msg As String
Dim insx As Double, insy As Double
Dim id As String: id = ""
Dim i As Long
Dim lst As ListBox
Dim dbc As Dbcocx
Dim frm As Form
Dim blockid As String: blockid = ""
Dim found As Boolean
Dim desc As String
Dim FirstFind As Long

  Set dbc = frmMain.Dbcocx1
  Set lst = frmMain.List1
  lst.Clear
  
  For i = 1 To numobj
    lst.AddItem GetObjId(i)
    lst.ItemData(lst.NewIndex) = GetObjLayer(i)
  Next i
  
  SelectDbgr 0, True ' cartesian will only exist on user coverage
      
  msg = "Select/Deselect a Cartesian Grid or click at coordinate to create (right button to end):"
  GetPoint msg, insx, insy
  Do While dbc.devlastkey() = 13
    dbc.Devgrseek insx, insy, 0
    SelectedEntityId = ""
    If dbc.devfound() <> 0 Then
      If Left(dbc.devgetfield("DESCR"), 6) = "INSERT" Then
        blockid = Trim(Mid$(dbc.devgetfield("DESCR"), 7))
        
        FirstFind = dbc.devrecno() ' save record number
        
        ' does this block CGrid... entity have the required dimensions ?
        dbc.devseek "N" & blockid
        If dbc.devfound() <> 0 Then
          desc = "CGrid." & min & "." & max
          If Left(dbc.devgetfield("DESCR"), 5) = "CGrid" Then
            dbc.devgorecno FirstFind
            SelectedEntityId = Trim(dbc.devgetid())
            
          ' if already selected then de-select
            found = False
            For i = 0 To lst.ListCount - 1
              If lst.List(i) = SelectedEntityId Then
                found = True
                lst.RemoveItem (i)
                dbc.devgrdisplayid SelectedEntityId, 256
              End If
            Next i
          End If
        End If
      End If
    End If
    If SelectedEntityId = "" Or (SelectedEntityId <> "" And Not found) Then
      If lst.ListCount = 1 Then
        MsgBox "Maximum exceeded, delete a selection or end"
        dbc.devgrdisplayid SelectedEntityId, 256
      Else
        If SelectedEntityId = "" Then
          Dim x1 As Double, y1 As Double
          GetPointDrag "Click on opposite corner to insert grid (right button to cancel) ", x1, y1, insx, insy, 2
          If dbc.devlastkey() <> 27 Then
            SelectedEntityId = InsertCGridIntoDb(dbc, insx, insy, x1, y1)
          End If
        End If
        If SelectedEntityId <> "" Then
          dbc.devgrdisplayid SelectedEntityId, 1
          lst.AddItem SelectedEntityId
          lst.ItemData(lst.NewIndex) = SelectedLayer
        End If
      End If
    End If
    GetPoint msg, insx, insy
  Loop
      
  Dim nod As Node
  Set nod = frmMain.TreeView1.Nodes(tvwindex)
  For i = 1 To nod.Children
    frmMain.TreeView1.Nodes.Remove tvwkey & GetObjKey(i)
  Next i
  numobj = lst.ListCount
  For i = 1 To numobj
    SetObjId i, lst.List(i - 1), lst.ItemData(i - 1)
    Set nod = frmMain.TreeView1.Nodes.Add(tvwkey, tvwChild, tvwkey & GetObjKey(i), GetObjKey(i))
    nod.EnsureVisible
  Next i

End Function
Private Function GetDataPoint() As Boolean
Dim msg As String
Dim insx As Double, insy As Double
Dim id As String: id = ""
Dim i As Long
Dim lst As ListBox
Dim dbc As Dbcocx
Dim FirstFind As Long
Dim blockid As String
Dim found As Boolean

  Set dbc = frmMain.Dbcocx1
  Set lst = frmMain.List1
  lst.Clear
  
  For i = 1 To numobj
    lst.AddItem GetObjId(i)
    lst.ItemData(lst.NewIndex) = GetObjLayer(i)
  Next i
      
  msg = "Select/DeSelect a point or click on coordinate to create (right button to end): "
  GetPoint msg, insx, insy
  While dbc.devlastkey() = 13
    SelectedEntityId = ""
    If frmMain.DevgrseekV2(insx, insy, TPOINT) Then
      FirstFind = dbc.devrecno()
      SelectedEntityId = Trim(dbc.devgetid())
      If SelectedEntityId <> "" Then
      ' if already selected then de-select
        found = False
        For i = 0 To lst.ListCount - 1
          If lst.List(i) = SelectedEntityId Then
            found = True
            lst.RemoveItem (i)
            dbc.devgrdisplayid SelectedEntityId, 256
          End If
        Next i
      End If
    End If
    If SelectedEntityId = "" Or (SelectedEntityId <> "" And Not found) Then
      If lst.ListCount = max Then
        MsgBox "Maximum exceeded, delete a selection or end"
        dbc.devgrdisplayid SelectedEntityId, 256
      Else
        If SelectedEntityId = "" Then
          SelectDbgr 0, True  ' select user layer
          dbc.devgrcmdinsert id, "point", insx, insy, 1, 1, 0, ""
          SelectedEntityId = Trim(dbc.devgetid())
        End If
        If SelectedEntityId <> "" Then
          dbc.devgrdisplayid SelectedEntityId, 1
          lst.AddItem SelectedEntityId
          lst.ItemData(lst.NewIndex) = SelectedLayer
        End If
      End If
    End If
    GetPoint msg, insx, insy
  Wend
      
  Dim nod As Node
  Set nod = frmMain.TreeView1.Nodes(tvwindex)
  For i = 1 To nod.Children
    frmMain.TreeView1.Nodes.Remove tvwkey & GetObjKey(i)
  Next i
  numobj = lst.ListCount
  For i = 1 To numobj
    SetObjId i, lst.List(i - 1), lst.ItemData(i - 1)
     Set nod = frmMain.TreeView1.Nodes.Add(tvwkey, tvwChild, tvwkey & GetObjKey(i), GetObjKey(i))
     nod.EnsureVisible
  Next i
End Function


Public Function SaveGidMod(m As Long, q As Long, ByRef ct As Long) As Long
Dim j As Long, parm As parmrec
  For j = 1 To numobj
    ct = ct + 1
    set_parm parm, "ModGeoObjIndex", m, ct, 0, 0, 0, 0, 0, "n/a", "n/a", GetObjIndex(j)
    write_parmrec pfile, parm
  Next j
End Function
Public Function SaveGidObj(countonly As Boolean, modid$, numGeoObj&, numPoly&, numLine&, numPt&, _
      numPolar&, numCart&, numPtGrid&) As Long
Dim parm As parmrec, num As Long, i As Long, dbc As Dbcocx, l As Long, v As Long
Dim numPolyPt As Long, numLinePt As Long, id As String, key As String, desc As String
Dim length As Double, x1 As Double, x2 As Double, y1 As Double, y2 As Double, dx As Double, dy As Double
Dim lenx As Double, leny As Double, nx As Long, ny As Long
Dim j As Long, k As Long, layer As Long

  Set dbc = frmMain.Dbcocx1
  
  For i = 1 To numobj
    id = GetObjId(CLng(i))
    SelectDbgr GetObjLayer(CLng(i)), True
    
    numGeoObj = numGeoObj + 1
    
    SetObjIndex i, numGeoObj
    
    If Not countonly Then
      set_parm parm, "GeoObjId", numGeoObj, 0, 0, 0, 0, 0, 0, "n/a", "n/a", modid
      write_parmrec pfile, parm
      set_parm parm, "GeoObjLabel", numGeoObj, 0, 0, 0, 0, 0, 0, "n/a", "n/a", label
      write_parmrec pfile, parm
      set_parm parm, "GeoObjKey", numGeoObj, 0, 0, 0, 0, 0, 0, "n/a", "n/a", GetObjKey(i)
      write_parmrec pfile, parm
      
      dbc.devseek "E" & id
    End If

    Select Case objtype
      Case TPOINT:
        numPt = numPt + 1
        If Not countonly Then
          set_parm parm, "GeoObjType", numGeoObj, 0, 0, 0, 0, 0, 0, "n/a", "n/a", TPOINT
          write_parmrec pfile, parm
          set_parm parm, "GeoObjIndex", numGeoObj, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(numPt)
          write_parmrec pfile, parm
          
          set_parm parm, "PointCoordinates", numPt, 1, 0, 0, 0, 0, 0, "n/a", "n/a", dbc.devgetfield("VX1")
          write_parmrec pfile, parm
          set_parm parm, "PointCoordinates", numPt, 2, 0, 0, 0, 0, 0, "n/a", "n/a", dbc.devgetfield("VY1")
          write_parmrec pfile, parm
          set_parm parm, "PointCoordinates", numPt, 3, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(9)
          write_parmrec pfile, parm
          set_parm parm, "PointDiameter", numPt, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(0)
          write_parmrec pfile, parm
        End If
      Case TPGRID:
        numPolar = numPolar + 1
        If Not countonly Then
          set_parm parm, "GeoObjType", numGeoObj, 0, 0, 0, 0, 0, 0, "n/a", "n/a", TPGRID
          write_parmrec pfile, parm
          set_parm parm, "GeoObjIndex", numGeoObj, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(numPolar)
          write_parmrec pfile, parm
  
          x1 = Val(dbc.devgetfield("VX1"))
          y1 = Val(dbc.devgetfield("VY1"))
          dx = Val(dbc.devgetfield("VX2"))
          dy = Val(dbc.devgetfield("VY2"))
          desc = Trim(dbc.devgetfield("TEXT"))
          desc = Mid$(desc, InStr(desc, ".") + 1)
          nx = Val(Left$(desc, InStr(desc, ".") - 1))
          desc = Mid$(desc, InStr(desc, ".") + 1)
          ny = Val(desc)
          lenx = dx * nx
          leny = dy * ny
          dx = lenx / min
          dy = leny / max
          set_parm parm, "NumPolarDirection", numPolar, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(max)
          write_parmrec pfile, parm
          set_parm parm, "NumPolarDistance", numPolar, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(min)
          write_parmrec pfile, parm
          set_parm parm, "PolarCenter", numPolar, 1, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(x1)
          write_parmrec pfile, parm
          set_parm parm, "PolarCenter", numPolar, 2, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(y1)
          write_parmrec pfile, parm
          set_parm parm, "PolarCenter", numPolar, 3, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(0)
          write_parmrec pfile, parm
          set_parm parm, "PolarDistances", numPolar, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(x1 + lenx)
          write_parmrec pfile, parm
        End If
      Case TCGRID:
        numCart = numCart + 1
        If Not countonly Then
          set_parm parm, "GeoObjType", numGeoObj, 0, 0, 0, 0, 0, 0, "n/a", "n/a", TCGRID
          write_parmrec pfile, parm
          set_parm parm, "GeoObjIndex", numGeoObj, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(numCart)
          write_parmrec pfile, parm
  
          x1 = Val(dbc.devgetfield("VX1"))
          y1 = Val(dbc.devgetfield("VY1"))
          dx = Val(dbc.devgetfield("VX2"))
          dy = Val(dbc.devgetfield("VY2"))
          desc = Trim(dbc.devgetfield("TEXT"))
          desc = Mid$(desc, InStr(desc, ".") + 1)
          nx = Val(Left$(desc, InStr(desc, ".") - 1))
          desc = Mid$(desc, InStr(desc, ".") + 1)
          ny = Val(desc)
          lenx = dx * nx
          leny = dy * ny
          dx = lenx / min
          dy = leny / max
          set_parm parm, "NumCartXDist", numCart, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(min)
          write_parmrec pfile, parm
          set_parm parm, "NumCartYDist", numCart, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(max)
          write_parmrec pfile, parm
          set_parm parm, "CartLLCoord", numCart, 1, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(x1)
          write_parmrec pfile, parm
          set_parm parm, "CartLLCoord", numCart, 2, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(y1)
          write_parmrec pfile, parm
          set_parm parm, "CartLLCoord", numCart, 3, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(0)
          write_parmrec pfile, parm
          set_parm parm, "CartURCoord", numCart, 1, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(x1 + lenx)
          write_parmrec pfile, parm
          set_parm parm, "CartURCoord", numCart, 2, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(y1 + leny)
          write_parmrec pfile, parm
          set_parm parm, "CartURCoord", numCart, 3, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(0)
          write_parmrec pfile, parm
        End If
      Case TPTGRID:
        numPtGrid = numPtGrid + 1
        If Not countonly Then
          set_parm parm, "GeoObjType", numGeoObj, 0, 0, 0, 0, 0, 0, "n/a", "n/a", TPTGRID
          write_parmrec pfile, parm
          set_parm parm, "GeoObjIndex", numGeoObj, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(numPtGrid)
          write_parmrec pfile, parm
          LocatePointGridPoints dbc, id, False, numPtGrid
        End If
      Case TPOLYLINE:
        numLine = numLine + 1
        If Not countonly Then
          set_parm parm, "GeoObjType", numGeoObj, 0, 0, 0, 0, 0, 0, "n/a", "n/a", TPOLYLINE
          write_parmrec pfile, parm
          set_parm parm, "GeoObjIndex", numGeoObj, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(numLine)
          write_parmrec pfile, parm
          set_parm parm, "PolylineDiameter", numLine, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(0)
          write_parmrec pfile, parm
          length = 0
          numLinePt = 1
          set_parm parm, "PolylinePts", numLine, numLinePt, 1, 0, 0, 0, 0, "n/a", "n/a", dbc.devgetfield("VX1")
          write_parmrec pfile, parm
          set_parm parm, "PolylinePts", numLine, numLinePt, 2, 0, 0, 0, 0, "n/a", "n/a", dbc.devgetfield("VY1")
          write_parmrec pfile, parm
          set_parm parm, "PolylinePts", numLine, numLinePt, 3, 0, 0, 0, 0, "n/a", "n/a", CStr(0)
          write_parmrec pfile, parm
          Do While Not dbc.deveof() And ("E" = Trim(dbc.devgetfield("SECTION"))) And id = Trim(dbc.devgetfield("ID"))
            x1 = Val(dbc.devgetfield("VX1")): y1 = Val(dbc.devgetfield("VY1"))
            x2 = Val(dbc.devgetfield("VX2")): y2 = Val(dbc.devgetfield("VY2"))
            length = length + DistanceFormula(x1, y1, x2, y2)
  
            numLinePt = numLinePt + 1
            set_parm parm, "PolylinePts", numLine, numLinePt, 1, 0, 0, 0, 0, "n/a", "n/a", CStr(x2)
            write_parmrec pfile, parm
            set_parm parm, "PolylinePts", numLine, numLinePt, 2, 0, 0, 0, 0, "n/a", "n/a", CStr(y2)
            write_parmrec pfile, parm
            dbc.devskip 1
          Loop
          set_parm parm, "NumPolylinePts", numLine, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(numLinePt)
          write_parmrec pfile, parm
          set_parm parm, "PolylineLength", numLine, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(length)
          write_parmrec pfile, parm
        End If
      Case TPOLYGON, TRECTANGLE:
        numPoly = numPoly + 1
        If Not countonly Then
          set_parm parm, "GeoObjType", numGeoObj, 0, 0, 0, 0, 0, 0, "n/a", "n/a", objtype
          write_parmrec pfile, parm
          set_parm parm, "GeoObjIndex", numGeoObj, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(numPoly)
          write_parmrec pfile, parm
          
          
          For l = 1 To 3
            set_parm parm, "PolygonCentroid", numPoly, l, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(0)
            write_parmrec pfile, parm
          Next l
          set_parm parm, "PolygonArea", numPoly, 0, 0, 0, 0, 0, 0, "n/a", "n/a", dbc.devgetfield("VY2")
          write_parmrec pfile, parm
          set_parm parm, "PolygonThick", numPoly, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(0)
          write_parmrec pfile, parm
          set_parm parm, "PolygonVolume", numPoly, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(0)
          write_parmrec pfile, parm
          
          numPolyPt = 0
          dbc.devseek "P" & id
          If 0 <> dbc.devfound() Then
            Do While Not dbc.deveof() And ("P" = Trim(dbc.devgetfield("SECTION"))) And id = Trim(dbc.devgetfield("ID"))
              numPolyPt = numPolyPt + 1
              desc = dbc.devgetfield("DESCR")
              v = Val(Mid$(desc, InStr(desc, " ")))
              set_parm parm, "PolygonPts", numPoly, v, 1, 0, 0, 0, 0, "n/a", "n/a", dbc.devgetfield("VX1")
              write_parmrec pfile, parm
              set_parm parm, "PolygonPts", numPoly, v, 2, 0, 0, 0, 0, "n/a", "n/a", dbc.devgetfield("VY1")
              write_parmrec pfile, parm
              set_parm parm, "PolygonPts", numPoly, v, 3, 0, 0, 0, 0, "n/a", "n/a", CStr("0")
              write_parmrec pfile, parm
              dbc.devskip 1
            Loop
          End If
          set_parm parm, "NumPolygonPts", numPoly, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(numPolyPt)
          write_parmrec pfile, parm
        End If
    End Select
  Next i
  SaveGidObj = numobj
End Function

Public Function EntityInUse() As Boolean
Dim i As Long
  For i = 1 To numobj
    If Trim(SelectedEntityId) = GetObjId(i) And SelectedLayer = GetObjLayer(i) Then
      EntityInUse = True
      Exit Function
    End If
  Next i
  EntityInUse = False
End Function

Public Function UpdateTreeviewStatus() As Boolean
Dim complete As Boolean, i As Long
  Select Case objtype
    Case TCGRID, TPGRID:
    complete = (numobj = 1)
    Case TPTGRID:
      Dim npt As Long
      complete = (numobj = 1)
      If complete Then
      npt = CountPointGridPoints(frmMain.Dbcocx1, GetObjId(1), GetObjLayer(1))
      complete = npt >= min And npt <= max
      If Not complete Then
'       Debug.Print "not complete"
      End If
      End If
    Case Else:
    complete = (numobj >= min) And (numobj <= max)
  End Select
  If complete Then frmMain.TreeView1.Nodes(tvwkey).Image = "green" Else frmMain.TreeView1.Nodes(tvwkey).Image = "red"
  UpdateTreeviewStatus = complete
End Function

Private Function IsARectangle(id As String) As Boolean
Dim lx As Double, ly As Double, vx() As Double, vy() As Double
Dim dbc As Dbcocx, v As Long, desc As String

  IsARectangle = False
  
  ReDim vx(4)
  ReDim vy(4)
  
  Set dbc = frmMain.Dbcocx1
  
  dbc.devseek "P" & id
  If 0 = dbc.devfound() Then Exit Function
  For v = 1 To 4
    If "P" = Trim(dbc.devgetfield("SECTION")) And id = Trim(dbc.devgetfield("ID")) Then
      desc = dbc.devgetfield("DESCR")
      If "Vertex" = Left(desc, 6) And v = Val(Mid$(desc, InStr(desc, " "))) Then
        vx(v) = Val(dbc.devgetfield("VX1"))
        vy(v) = Val(dbc.devgetfield("VY1"))
      Else
        Exit Function
      End If
      dbc.devskip 1
    Else
        Exit Function ' not enough vertices
    End If
  Next v
  If 0 = dbc.deveof() Then
    If "P" = Trim(dbc.devgetfield("SECTION")) And id = Trim(dbc.devgetfield("ID")) Then
      Exit Function  ' too many vertices
    End If
  End If
  lx = DistanceFormula(vx(1), vy(1), vx(2), vy(2))
  ly = DistanceFormula(vx(2), vy(2), vx(3), vy(3))
  If lx = DistanceFormula(vx(3), vy(3), vx(4), vy(4)) Then
    If ly = DistanceFormula(vx(4), vy(4), vx(1), vy(1)) Then
      IsARectangle = True
    End If
  End If
End Function

Public Sub LoadGid(modid As String, p As Long)
Dim i As Long, j As Long, k As Long, ct As Long, ix As Long
Dim oid As String, key As String, nod As Node

  For i = 1 To UBound(gidmod)
    If gidmod(i).id = modid Then
      For j = 1 To gidmod(i).numobj
        ix = gidmod(i).objindex(j)
        If gidobj(ix).id = modid And _
          gidobj(ix).label = label And _
          gidobj(ix).type = objtype Then
          ct = ct + 1
        End If
      Next j
      numobj = ct
    End If
  Next i

  For i = 1 To UBound(gidmod)
    If gidmod(i).id = modid Then
      ct = 0
      For j = 1 To gidmod(i).numobj
        ix = gidmod(i).objindex(j)
        If gidobj(ix).id = modid And _
          gidobj(ix).label = label And _
          gidobj(ix).type = objtype Then
            key = gidobj(ix).key  ' format is id,filename
            oid = Left$(key, InStr(key, ",") - 1)
            key = Mid$(key, InStr(key, ",") + 1)
            If key = SplitPath(FUIName, SP_TITLE) Then
              ct = ct + 1
              SetObjId ct, oid, 0
              Set nod = frmMain.TreeView1.Nodes.Add(tvwkey, tvwChild, tvwkey & GetObjKey(ct), GetObjKey(ct))
              nod.EnsureVisible
                      
            Else
              For k = 1 To UBound(layers)
                If InStr(layers(k).filename, key) Then
                  ct = ct + 1
                  SetObjId ct, oid, k
                  Set nod = frmMain.TreeView1.Nodes.Add(tvwkey, tvwChild, tvwkey & GetObjKey(ct), GetObjKey(ct))
                  nod.EnsureVisible
                  Exit For
                End If
              Next k
            End If
        End If
      Next j
    End If
  Next i
End Sub

Public Function SeekTest(dbc As Dbcocx, otype As String) As Boolean
  Dim recno As Long, blockid As String
  
  SeekTest = False
  recno = dbc.devrecno() ' save database position
  Select Case otype
    Case TPOINT:
      If Left(dbc.devgetfield("DESCR"), 5) = "POINT" Then
        SeekTest = True
      End If
      If Left(dbc.devgetfield("DESCR"), 6) = "INSERT" Then
        blockid = Trim(Mid$(dbc.devgetfield("DESCR"), 7))
        ' does this block entity have the required attributes ?
        dbc.devseek "N" & blockid
        If dbc.devfound() <> 0 Then
          If Left(dbc.devgetfield("DESCR"), 5) = "point" Then
            SeekTest = True
            dbc.devgorecno recno
          End If
        End If
      End If
    Case TPOLYLINE:
      SeekTest = Left(dbc.devgetfield("DESCR"), 4) = "LINE"
    Case TPOLYGON:
      SeekTest = Left(dbc.devgetfield("DESCR"), 7) = TPOLYGON _
            Or Left(dbc.devgetfield("DESCR"), 6) = TCIRCLE
    Case TRECTANGLE:
      If Left(dbc.devgetfield("DESCR"), 7) = TPOLYGON Then
        If Left(dbc.devgetfield("TEXT"), 9) = TRECTANGLE Then
          SeekTest = True
        Else
          ' determine if this is a rectangle
          SeekTest = IsARectangle(dbc.devgetid())
          dbc.devgorecno recno ' restore database position
        End If
      Else
        SeekTest = False
      End If
    Case TPGRID:
    Case TCGRID:
    Case TPTGRID:
  End Select
End Function
