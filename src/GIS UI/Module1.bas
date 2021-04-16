Attribute VB_Name = "Module1"
Option Compare Text
Option Explicit

Public Const WM_USER = &H400
Public Const TB_SETSTYLE = WM_USER + 56
Public Const TB_GETSTYLE = WM_USER + 57
Public Const TBSTYLE_FLAT = &H800
Public Const MF_BYPOSITION = &H400
Public Const MF_REMOVE = &H1000

Public Declare Function SendMessage Lib "user32" Alias "SendMessageA" _
  (ByVal hwnd As Long, _
   ByVal wMsg As Long, _
   ByVal wParam As Long, _
   lParam As Any) As Long

Public Declare Function FindWindowEx Lib "user32" Alias "FindWindowExA" _
  (ByVal hWnd1 As Long, _
   ByVal hWnd2 As Long, _
   ByVal lpsz1 As String, _
   ByVal lpsz2 As String) As Long
   

Public Declare Function DrawMenuBar Lib "user32" _
   (ByVal hwnd As Long) As Long
Public Declare Function GetMenuItemCount Lib "user32" _
  (ByVal hMenu As Long) As Long
Public Declare Function GetSystemMenu Lib "user32" _
  (ByVal hwnd As Long, ByVal bRevert As Long) As Long
Public Declare Function RemoveMenu Lib "user32" _
  (ByVal hMenu As Long, ByVal nPosition As Long, ByVal wFlags As Long) As Long
Public Declare Function GetPrivateProfileString Lib "kernel32" Alias "GetPrivateProfileStringA" _
  (ByVal lpApplicationName As String, ByVal lpKeyName As Any, ByVal lpDefault As String, _
   ByVal lpReturnedString As String, ByVal nSize As Long, ByVal lpFileName As String) As Long
   
   

Global FRAMES_INI As String
Global DesName As String        'the path and name of the run
Global FUIName As String        'the path and name of the run
Global RunName As String        'the name of the parameter file
Global SiteIdx As Long          'the index to site
Global ModIdx As Long           'the index to module
Global ModName As String        'the name of the module glyph
Global called As Boolean
Global frm As Object

Global HelpFileName As String   'the name of the reference file
Global HelpAvailable As Boolean 'does the help file exist
Global HelpAnchor As String     'the name of the help anchor
 
Global RefFileName As String    'the name of the reference file
Global RefAvailable As Boolean  'does the reference file exist
Global RefMode As Long          'if 0 then select a reference else add a reference
Global RefIdx As Long           'the current selected reference
Global RefItem As Integer       'the current selected item index

Global errfile As csv           'error file declaration
Global AnError As Boolean       'error flag to tell when to keep error file
Global argv() As String         'command line argument array
Global argc As Long             'command line argument count
Global saving As Boolean
Global des As csv                 'des file declaration
Global pfile As parmfile

Global IgnoreEvents As Boolean


Public Const MAXMODOBJ = 100  ' see objindex in GIDMODSTRUCT for use in LoadGid

Public Const DBNEW = 1
Public Const DBOLD = 2
Public Const SERVER = 1
Public Const CLIENT = 2
Public Const SUCCESS = 1
Public Const FAILURE = 0
Public Const GET_AREA = 1
Public Const GET_THICK = 2
Public Const GET_LENGTH = 3
Public Const GET_CENTROID = 4
Public Const CONF_BACKGROUND = 1
Public Const CONF_DATALAYERS = 2

Public Const DPOINT = 0
Public Const DLINE = 1
Public Const DPOLY = 2
Public Const DCGRID = 3
Public Const DPGRID = 4
Public Const DPTGRID = 5

Public Const TUSER = "User"
Public Const TPOINT = "Point"
Public Const TLINE = "Line"
Public Const TPOLYLINE = "Polyline"
Public Const TPOLYGON = "Polygon"
Public Const TCGRID = "Cartesian"
Public Const TPGRID = "Polar"
Public Const TPTGRID = "PointGrid"
Public Const TRECTANGLE = "Rectangle"
Public Const TCIRCLE = "Circle"
Public Const TPICK = "Pick"

Type GIDOBJSTRUCT
  id As String
  label As String
  key As String
  type As String
  Index As Long
End Type

Type GIDMODSTRUCT
  id As String
  numobj As Long
  objindex(MAXMODOBJ) As Long
End Type

Type DEFAULTSTRUCT
  objtype As String
  color As Long
  linetype As Long
  fill As Long
  x As Long
  y As Long
End Type

Type LAYERSTRUCT
  Name As String
  dwg As Boolean
  bkgd As Boolean
  visible As Boolean
  pickable As Boolean
  objtype As String
  color As Long
  linetype As Long
  lcolor As Integer
  fill As Long
  filename As String
  extminX As Double
  extminY As Double
  extmaxX As Double
  extmaxY As Double
  workarea As Long ' for devSelect()
  opened As Boolean
End Type


Public gidmod() As GIDMODSTRUCT
Public gidobj() As GIDOBJSTRUCT

Public defaults() As DEFAULTSTRUCT
Public module() As ModClass
Public layers() As LAYERSTRUCT
Public myLayer As LAYERSTRUCT
Public ConfigureWhat As Integer
Public enableState As Boolean

Public parms() As parmrec
Public OpMode As Integer

Public DBFileTitle As String
Public DBFileDir As String
Public DBTableName As String
Public GisName As String

Public LastSelectedEntityId As String
Public LastHighLightedId As String
Public DontHighLight As Boolean
Public SelectedEntityId As String
Public SelectedEntityLayer As Long
Public SelectedLayer As Long

Public CommandArgs As String
Public GisObj As GISClass

Public Sub DisplayStatus(msg As String)
  frmMain.StatusBar1.Panels(1).text = msg
End Sub


' let the user specify a point in the graphic window
' (he can use any of the DbCAD dev zoom buttons at the same time)
Public Function GetPoint(ByVal prompt As String, x As Double, y As Double) As Integer
  frmMain.StatusBar1.Panels(1).text = prompt
  frmMain.Dbcocx1.devgetpoint "", x, y
  GetPoint = frmMain.Dbcocx1.devlastkey()
  frmMain.StatusBar1.Panels(1).text = ""
End Function


' let the user specify a rectangle second point in the graphic window
' (he can use any of the DbCAD dev zoom buttons at the same time)
Public Function GetPointDrag(ByVal prompt As String, x As Double, y As Double, ByVal x1 As Double, ByVal y1 As Double, ByVal dragmode As Integer) As Integer
  frmMain.StatusBar1.Panels(1).text = prompt
  frmMain.Dbcocx1.devgetpointdrag "", x, y, x1, y1, dragmode
  GetPointDrag = frmMain.Dbcocx1.devlastkey()
  frmMain.StatusBar1.Panels(1).text = ""
End Function


Public Function InsertPGridIntoDb(dbc As Dbcocx, insx As Double, insy As Double, x1 As Double, y1 As Double) As String
Dim i As Long
Dim radians As Double
Dim desc As String
Dim id As String
Dim dx As Double
Dim found As Boolean
Dim filedir As String
Dim dist As Long, dir As Long

    dist = defaults(DPGRID).x
    dir = defaults(DPGRID).y

    desc = "PGrid." & dist & "." & dir

    dbc.devgrcmdelineset defaults(DPGRID).linetype
    dbc.devgrcmdecolorset defaults(DPGRID).color
    
    dbc.devindexon "SECTION", SplitPath(RunName, SP_DIR) & "SECTION"
    dbc.devseek "N"
    Do While 0 = dbc.deveof()
      If "N" <> Trim(dbc.devgetfield("SECTION")) Then Exit Do
      If desc = Trim(dbc.devgetfield("DESCR")) Then
        found = True
        Exit Do
      End If
      dbc.devskip 1
    Loop
    dbc.devsetorder 1 ' reset default index
    
    If Not found Then
      dbc.devgrcmdblock desc
      id = dbc.devgetid()
      For i = 1 To dist
        dbc.devgrcmdcircle id, 0, 0, i
      Next i
      For i = 0 To dir
        radians = IIf(i = 0, 0, ((360 / dir) * i) * (3.14159 / 180))
        dbc.devgrcmdline id, 0, 0, dist * Sin(radians), dist * Cos(radians)
      Next i
      dbc.devgrcmdendblock
    End If
    dx = DistanceFormula(insx, insy, x1, y1)
    dbc.devgrcmdinsert "", desc, insx, insy, dx / dist, dx / dist, 0, ""
    dbc.devsetfield "TEXT", desc
    InsertPGridIntoDb = Trim(dbc.devgetid())
End Function


Public Function InsertCGridIntoDb(dbc As Dbcocx, insx As Double, insy As Double, x1 As Double, y1 As Double) As String
Dim id As String, desc As String
Dim nx As Long, ny As Long, dx As Double, dy As Double, x0 As Double, y0 As Double
Dim i As Long, found As Boolean

    nx = defaults(DCGRID).x
    ny = defaults(DCGRID).y
    dx = Abs(insx - x1)
    dy = Abs(insy - y1)
    
    dbc.devgrcmdelineset defaults(DCGRID).linetype ' -2 ' dotted line
    dbc.devgrcmdecolorset defaults(DCGRID).color

    desc = "CGrid." & nx & "." & ny
    
    dbc.devindexon "SECTION", SplitPath(RunName, SP_DIR) & "SECTION"
    dbc.devseek "N"
    Do While 0 = dbc.deveof()
      If "N" <> Trim(dbc.devgetfield("SECTION")) Then Exit Do
      If desc = Trim(dbc.devgetfield("DESCR")) Then
        found = True
        Exit Do
      End If
      dbc.devskip 1
    Loop
    dbc.devsetorder 1 ' reset default index
    
    If Not found Then
      dbc.devgrcmdblock desc
      id = dbc.devgetid()
      For i = 0 To nx
        dbc.devgrcmdline id, i, 0, i, ny
      Next i
      For i = 0 To nx
        dbc.devgrcmdline id, 0, i, nx, i
      Next i
      dbc.devgrcmdendblock
    End If
    
    x0 = IIf(insx < x1, insx, x1)
    y0 = IIf(insy < y1, insy, y1)
          
    dbc.devgrcmdinsert "", desc, x0, y0, dx / nx, dy / ny, 0, ""
    dbc.devsetfield "TEXT", desc
    dbc.devsetfield "ECOLOR", defaults(DCGRID).color
    dbc.devsetfield "ELINE", defaults(DCGRID).linetype
    
    InsertCGridIntoDb = Trim(dbc.devgetid())
End Function

Public Function InsertRectangleIntoDb(dbc As Dbcocx, insx As Double, insy As Double, x1 As Double, y1 As Double) As String
Dim id As String
    dbc.devgrcmdecolorset defaults(DPOLY).color
    dbc.devgrcmdelineset defaults(DPOLY).linetype

    dbc.devgrcmdpolygon id, defaults(DPOLY).fill, 0
    id = dbc.devgetid()
    dbc.devsetfield "TEXT", "Rectangle"
    dbc.devgrcmdvertex insx, insy
    dbc.devgrcmdvertex x1, insy
    dbc.devgrcmdvertex x1, y1
    dbc.devgrcmdvertex insx, y1
    dbc.devgrcmdpolycalc id, 2, 0, 0
    InsertRectangleIntoDb = Trim(id)
End Function


Public Function InsertRectangle(Dbcocx1 As Dbcocx) As String
Dim insx As Double, insy As Double, x1 As Double, y1 As Double
Dim id As String
  
  Dbcocx1.devgrcmdecolorset defaults(DPOLY).color
  Dbcocx1.devgrcmdelineset defaults(DPOLY).linetype
  
  GetPoint "Click on coordinate to begin rectangle (right button to abort).", insx, insy
  GetPointDrag "Move mouse to expand to size, then click to complete rectangle.", x1, y1, insx, insy, 2
  
  id = InsertRectangleIntoDb(Dbcocx1, insx, insy, x1, y1)
  Dbcocx1.devgrdisplayid id, 256
  InsertRectangle = id
End Function


Public Function InsertCircle(Dbcocx1 As Dbcocx) As String
  Dim insx As Double, insy As Double, x1 As Double, y1 As Double
  Dim dist As Double
  Dim id As String: id = ""
  Dbcocx1.devgrcmdelineset 1
  GetPoint "Click on center coordinate of polar grid (right button to abort):", insx, insy
  GetPointDrag "Move mouse to length of radius, then click to complete polar grid.", x1, y1, insx, insy, 1
  
  dist = DistanceFormula(insx, insy, x1, y1)
  Dbcocx1.devgrcmdcircle id, insx, insy, dist
  id = Dbcocx1.devgetid()
  Dbcocx1.devgrdisplayid id, 256
  InsertCircle = id
End Function


Public Function InsertPoint(Dbcocx1 As Dbcocx) As String
  Dim insx As Double, insy As Double, msg As String
  Dim id As String: id = ""
  Dbcocx1.devgrcmdecolorset defaults(DPOINT).color
  Dbcocx1.devgrcmdelineset 1
  msg = "Click at coordinate to insert point (right button to end):"
  GetPoint msg, insx, insy
  While Dbcocx1.devlastkey() = 13
    Dbcocx1.devgrcmdpoint id, insx, insy
    id = Dbcocx1.devgetid()
    Dbcocx1.devgrdisplayid id, 256
    GetPoint msg, insx, insy
    id = ""
  Wend
  InsertPoint = id

End Function


Public Function InsertLine(Dbcocx1 As Dbcocx) As String
    Dim insx As Double, insy As Double, x As Double, y As Double
    Dim id As String: id = ""

    Dbcocx1.devgrcmdecolorset defaults(DLINE).color
    Dbcocx1.devgrcmdelineset defaults(DLINE).linetype
    If GetPoint("Click on coordinate to locate initial vertex of polyline (right button to cancel).", insx, insy) = 13 Then
        While GetPointDrag("Move mouse to next vertex and click (right button to end):", x, y, insx, insy, 1) = 13
            Dbcocx1.devgrcmdline id, insx, insy, x, y
            id = Dbcocx1.devgetid()
            Dbcocx1.devgrdisplayid id, 256
            insx = x
            insy = y
        Wend
    End If
  InsertLine = id
End Function



' ATTENTION :::::::::::::::::::::::::::::::::::::::::::::::::::
' enable/disable User Interface elements which could interferes with a pointing operation
' taking place in a DbCAD dev graphic window.
' So you ensure that the pointing operation reaches a final state
Public Sub DisableUI(pEnableState As Boolean)
  enableState = Not pEnableState
  IgnoreEvents = pEnableState
  frmMain.ShowState
End Sub

' tells if DbCAD dev dragging operation is taking place
Public Function IsUIDisabled() As Boolean
    If IsEmpty(enableState) Then
        IsUIDisabled = False
    Else
        IsUIDisabled = Not enableState
    End If
End Function


Sub MoveLine(Dbcocx1 As Dbcocx)
    Dim VertexMoved As Boolean
    Dim Ident As String
    Dim ptx As Double, pty As Double
    Dim MinDist As Double, dist As Double
    Dim NearestVtx As Long
    Dim xvtx As Double, yvtx As Double
    Dim OldArea As Double, NewArea As Double
    
    VertexMoved = False
    Ident = Dbcocx1.devgetid()
                    
    GetPoint "Select vertex to move (right button to end):", ptx, pty
    While Dbcocx1.devlastkey() = 13
        MinDist = -1
        NearestVtx = 0
        Dbcocx1.devseek "E" & Ident
        
        Do
            If Dbcocx1.deveof() <> 0 Then Exit Do
            If Dbcocx1.devgetfield("SECTION") <> "E" Then Exit Do
            If Dbcocx1.devgetfield("ID") <> Ident Then Exit Do
                        
            xvtx = Val(Dbcocx1.devgetfield("VX1"))
            yvtx = Val(Dbcocx1.devgetfield("VY1"))
                        
            dist = Sqr(((xvtx - ptx) * (xvtx - ptx)) + ((yvtx - pty) * (yvtx - pty)))
            If MinDist < 0 Or dist < MinDist Then
                MinDist = dist
                NearestVtx = Dbcocx1.devrecno()
            End If
            Dbcocx1.devskip 1
        Loop
        If NearestVtx <> 0 Then
            Dbcocx1.devgorecno NearestVtx
            xvtx = Val(Dbcocx1.devgetfield("VX1"))
            yvtx = Val(Dbcocx1.devgetfield("VY1"))
                        
            GetPointDrag "New position (right button to abort):", ptx, pty, xvtx, yvtx, 1
            If Dbcocx1.devlastkey() = 13 Then
                Dbcocx1.devgrdisplayid Ident, 7
                VertexMoved = True
                Dbcocx1.devsetfield "VX1", Trim(Str(ptx))
                Dbcocx1.devsetfield "VY1", Trim(Str(pty))
                Dbcocx1.devgrdisplayid Ident, 1
                Dbcocx1.devgrcmdpolycalc Ident, 2, 0, 0
                NewArea = Val(Dbcocx1.devgetfield("VY2"))
'               Label3.Caption = "Area: " & Format(NewArea, "0.00") & " mq"
'               Label4.Caption = "Perim.: " & Format(Val(dbcocx1.devgetfield("VX2")), "0.00") & " m"
            End If
        End If
        GetPoint "Select vertex to move (right button to end):", ptx, pty
    Wend
    If VertexMoved Then
        Dim PolygonToCheck As Long, CheckCode As Long
    End If
End Sub

Sub MovePolygon(Dbcocx1 As Dbcocx)
    Dim VertexMoved As Boolean
    Dim Ident As String
    Dim ptx As Double, pty As Double
    Dim MinDist As Double, dist As Double
    Dim NearestVtx As Long
    Dim xvtx As Double, yvtx As Double
    Dim OldArea As Double, NewArea As Double
    
    VertexMoved = False
    Ident = Dbcocx1.devgetid()
    OldArea = Val(Dbcocx1.devgetfield("VY2"))
    NewArea = OldArea
'   Label3.Caption = "Area: " & Format(OldArea, "0.00") & " mq"
'   Label4.Caption = "Perim.: " & Format(Val(dbcocx1.devgetfield("VX2")), "0.00") & " m"
                    
    GetPoint "Select vertex to move (right button to end):", ptx, pty
    While Dbcocx1.devlastkey() = 13
        MinDist = -1
        NearestVtx = 0
        Dbcocx1.devseek "P" & Ident
        
        Do
            If Dbcocx1.deveof() <> 0 Then Exit Do
            If Dbcocx1.devgetfield("SECTION") <> "P" Then Exit Do
            If Dbcocx1.devgetfield("ID") <> Ident Then Exit Do
                        
            xvtx = Val(Dbcocx1.devgetfield("VX1"))
            yvtx = Val(Dbcocx1.devgetfield("VY1"))
                        
            dist = Sqr(((xvtx - ptx) * (xvtx - ptx)) + ((yvtx - pty) * (yvtx - pty)))
            If MinDist < 0 Or dist < MinDist Then
                MinDist = dist
                NearestVtx = Dbcocx1.devrecno()
            End If
            Dbcocx1.devskip 1
        Loop
        If NearestVtx <> 0 Then
            Dbcocx1.devgorecno NearestVtx
            xvtx = Val(Dbcocx1.devgetfield("VX1"))
            yvtx = Val(Dbcocx1.devgetfield("VY1"))
                        
            GetPointDrag "New position (right button to abort):", ptx, pty, xvtx, yvtx, 1
            If Dbcocx1.devlastkey() = 13 Then
                Dbcocx1.devgrdisplayid Ident, 7
                VertexMoved = True
                Dbcocx1.devsetfield "VX1", Trim(Str(ptx))
                Dbcocx1.devsetfield "VY1", Trim(Str(pty))
                Dbcocx1.devgrdisplayid Ident, 1
                Dbcocx1.devgrcmdpolycalc Ident, 2, 0, 0
                NewArea = Val(Dbcocx1.devgetfield("VY2"))
'               Label3.Caption = "Area: " & Format(NewArea, "0.00") & " mq"
'               Label4.Caption = "Perim.: " & Format(Val(dbcocx1.devgetfield("VX2")), "0.00") & " m"
            End If
        End If
        GetPoint "Select vertex to move (right button to end):", ptx, pty
    Wend
    If VertexMoved Then
        Dim PolygonToCheck As Long, CheckCode As Long
    End If
End Sub


Function AddLayer(l As Integer) As Boolean
Dim Dialog As CommonDialog
Dim id As Integer, pos As Long
Dim filename As String, filetitle As String, filedir As String, tstr As String

  Set Dialog = frmConfig2.CommonDialog1
  AddLayer = False
  Dialog.Flags = cdlOFNFileMustExist Or cdlOFNAllowMultiselect
  Dialog.CancelError = True
  Dialog.DialogTitle = "Add Coverage"
' Dialog.Filter = "All supported formats|*.shp;*.dwg;*.dxf;*.bmp;*.tif;*.tff;*.tiff;|ESRI Shapefiles (*.shp)|*.shp|AutoCad (*.dwg)|*.dwg|Drawing Interchange File (*.dxf)|*.dxf|Windows Bitmap (*.bmp; *.dib)|*.bmp;*.dib|Tag Image File(*.tif; *.tff; *.tiff)|*.tif;*.tff;*.tiff"
  Dialog.Filter = "ESRI Shapefiles (*.shp)|*.shp"
  Dialog.FilterIndex = 2
  On Error Resume Next
  Dialog.ShowOpen
  If Err.Number = cdlCancel Then Exit Function
  
  If 0 <> InStr(Dialog.filename, " ") Then
    ' multiple files
    pos = InStr(Dialog.filename, " ")
    filedir = Left(Dialog.filename, pos - 1)
    tstr = Mid$(Dialog.filename, pos + 1)
    pos = InStr(tstr, " ")
    filetitle = Left(tstr, pos - 1)
    tstr = Mid$(tstr, pos + 1)
  Else
    ' single file
    tstr = ""
    filedir = SplitPath(Dialog.filename, SP_DIR)
    filetitle = SplitPath(Dialog.filename, SP_TITLE)
  End If

  Do While filetitle <> ""
    l = 1 + UBound(layers)
    ReDim Preserve layers(l)
    layers(l).visible = True
    
    Select Case Right(filetitle, 4)
      Case ".shp"
  '     With bkgd(l)
        With layers(l)
          .Name = filetitle
          .filename = filedir & filetitle
          .bkgd = True
          .dwg = True
          .color = 7 ' black
          .lcolor = 7 ' black
          .visible = True
          .pickable = False
          id = frmMain.Dbcocx1.devgropen(.filename)
          frmMain.Dbcocx1.devgrgetextmin id, .extminX, .extminY
          frmMain.Dbcocx1.devgrgetextmax id, .extmaxX, .extmaxY
          frmMain.Dbcocx1.devgrclose id
        End With
      Case ".dwg", ".dxf"  ' map
        Dim fId As Integer, lname As String, lct As Integer, i As Integer
        lname = String(30, 0)
        With layers(l)
          .Name = filetitle
          .filename = filedir & filetitle
          .bkgd = True
          .dwg = True
          .color = 7 ' black
          .lcolor = 7 ' black
        End With
      Case Else ' image
        With layers(l)
          .Name = filetitle
          .filename = filedir & filename
          .bkgd = True
          .dwg = False
          .color = 7 ' black
          .lcolor = 7 ' black
        End With
    End Select
    
    If tstr <> "" Then
      pos = InStr(tstr, " ")
      If pos > 0 Then
        filetitle = Left(tstr, pos - 1)
        tstr = Mid(tstr, pos + 1)
      Else
        filetitle = tstr
        tstr = ""
      End If
    Else
      filetitle = ""
    End If
  Loop
  AddLayer = True
End Function


Sub UpdateLayersList(l As Integer, lst As ListBox, layers() As LAYERSTRUCT)
Dim i As Integer, lname As String

  lst.Clear
  For i = 1 To UBound(layers)
    lst.AddItem layers(i).filename
  Next i
  If l >= 0 And l < lst.ListCount Then
    lst.ListIndex = l
  End If
End Sub


Sub GetDatabaseLayers(Dbcocx1 As Dbcocx, l As Integer, lst As Variant, layers() As LAYERSTRUCT)
Dim i As Integer, lname As String, fName As String, text As String

  For i = 1 To UBound(layers)
    Dbcocx1.devseek "L" & i
    If 0 <> Dbcocx1.devfound() Then
      lname = Trim(Dbcocx1.devgetfield("DESCR"))
      If layers(i).Name = lname Then
        layers(i).fill = Dbcocx1.devgetfield("VX1")
        layers(i).visible = layers(i).lcolor >= 0
        
        text = Trim(Dbcocx1.devgetfield("TEXT"))
        If text <> "file" Then layers(i).objtype = text
        ''' these cause a later GPF ????????????????/
        'MsgBox "devgetfield ELINE"
        '    layers(nl).linetype = Val(frmMain.Dbcocx1.devgetfield("ELINE"))
        ' MsgBox "devgetfield ECOLOR "
        '    layers(nl).lcolor = Val(frmMain.Dbcocx1.devgetfield("ECOLOR"))
        
      End If
    End If
  Next i
End Sub

Function RasterBackground() As Boolean
Dim i As Integer
  For i = 1 To UBound(layers)
    If layers(i).bkgd Then
      RasterBackground = True
      Exit Function
    End If
  Next i
  RasterBackground = False
End Function

Function VisibleDataLayers() As Boolean
  VisibleDataLayers = 0 < UBound(layers)
End Function

Function SaveDatabase() As Boolean
  If "" <> dir$(FUIName & ".dbf") Then Kill FUIName & ".dbf"
  If "" <> dir$(FUIName & ".ndx") Then Kill FUIName & ".ndx"
  If "" <> dir(RunName & ".dbf") Then
    FileCopy RunName & ".dbf", FUIName & ".dbf"
    Kill RunName & ".dbf"
  End If
  If "" <> dir$(RunName & ".ndx") Then
    FileCopy RunName & ".ndx", FUIName & ".ndx"
    Kill RunName & ".ndx"
  End If
End Function


Function SaveGid() As Boolean
Dim l As Long, fName As String
Dim numGeoObj As Long, numPoly As Long, numLine As Long, numPt As Long
Dim numPolars As Long, numCartesians As Long, numPointGrids As Long
Dim parm As parmrec


  fName = RunName & ".gid"
  If "" <> dir(fName) Then Kill fName
  open_parm pfile, fName, F_WRITE
  
  frmMain.ViewChange
  
  set_parm parm, "XCenter", 0, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(frmMain.XCentr)
  write_parmrec pfile, parm
  set_parm parm, "YCenter", 1, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(frmMain.YCentr)
  write_parmrec pfile, parm
  set_parm parm, "ViewX0", 0, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(frmMain.ViewX0)
  write_parmrec pfile, parm
  set_parm parm, "ViewY0", 0, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(frmMain.ViewY0)
  write_parmrec pfile, parm
  set_parm parm, "ViewX1", 0, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(frmMain.ViewX1)
  write_parmrec pfile, parm
  set_parm parm, "ViewY1", 0, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(frmMain.ViewY1)
  write_parmrec pfile, parm
  
  set_parm parm, "NumLayer", 0, 0, 0, 0, 0, 0, 0, "n/a", "n/a", UBound(layers)
  write_parmrec pfile, parm
  For l = 1 To UBound(layers)
    With layers(l)
      If "" <> .filename Then
        set_parm parm, "LayerFilename", l, 0, 0, 0, 0, 0, 0, "n/a", "n/a", .filename
        write_parmrec pfile, parm
      End If
      set_parm parm, "LayerColor", l, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(.lcolor)
      write_parmrec pfile, parm
      set_parm parm, "LayerVisible", l, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(.visible)
      write_parmrec pfile, parm
      set_parm parm, "LayerPickable", l, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(.pickable)
      write_parmrec pfile, parm
      set_parm parm, "ExtMinX", l, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(.extminX)
      write_parmrec pfile, parm
      set_parm parm, "ExtMinY", l, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(.extminY)
      write_parmrec pfile, parm
      set_parm parm, "ExtMaxX", l, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(.extmaxX)
      write_parmrec pfile, parm
      set_parm parm, "ExtMaxY", l, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(.extmaxY)
      write_parmrec pfile, parm
    End With
  Next l
  set_parm parm, "NumDefaults", 0, 0, 0, 0, 0, 0, 0, "n/a", "n/a", UBound(defaults) + 1
  write_parmrec pfile, parm
  For l = 0 To UBound(defaults)
    With defaults(l)
      set_parm parm, "DefType", l + 1, 0, 0, 0, 0, 0, 0, "n/a", "n/a", .objtype
      write_parmrec pfile, parm
      set_parm parm, "DefColor", l + 1, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(.color)
      write_parmrec pfile, parm
      set_parm parm, "DefLine", l + 1, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(.linetype)
      write_parmrec pfile, parm
      set_parm parm, "DefFill", l + 1, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(.fill)
      write_parmrec pfile, parm
      set_parm parm, "DefX", l + 1, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(.x)
      write_parmrec pfile, parm
      set_parm parm, "DefY", l + 1, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(.y)
      write_parmrec pfile, parm
    End With
  Next l
  
  numGeoObj = 0
  numPoly = 0
  numLine = 0
  numPt = 0
  numPolars = 0
  numCartesians = 0
  numPointGrids = 0
  
  For l = 1 To UBound(module)
    ' get counts only
    module(l).SaveGidObj True, l, numGeoObj, numPoly, numLine, numPt, _
      numPolars, numCartesians, numPointGrids
  Next l
  
  Dim ct As Long
  set_parm parm, "NumModules", 0, 0, 0, 0, 0, 0, 0, "n/a", "n/a", UBound(module)
  write_parmrec pfile, parm
  For l = 1 To UBound(module)
    module(l).SaveGidMod l
  Next l

  set_parm parm, "NumGeoObjects", 0, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(numGeoObj)
  write_parmrec pfile, parm
  set_parm parm, "NumPolygons", 0, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(numPoly)
  write_parmrec pfile, parm
  set_parm parm, "NumPolylines", 0, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(numLine)
  write_parmrec pfile, parm
  set_parm parm, "NumPoints", 0, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(numPt)
  write_parmrec pfile, parm
  set_parm parm, "NumPolars", 0, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(numPolars)
  write_parmrec pfile, parm
  set_parm parm, "NumCartesians", 0, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(numCartesians)
  write_parmrec pfile, parm
  set_parm parm, "NumGrid", 0, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(numPointGrids)
  write_parmrec pfile, parm
  
  numGeoObj = 0
  numPoly = 0
  numLine = 0
  numPt = 0
  numPolars = 0
  numCartesians = 0
  numPointGrids = 0

  For l = 1 To UBound(module)
    ' write objects to gid
    module(l).SaveGidObj False, l, numGeoObj, numPoly, numLine, numPt, _
        numPolars, numCartesians, numPointGrids
  Next l

  close_parm pfile
    
  SaveGid = True
End Function
Private Sub GisGetArguments(Optional args As String = "")
Dim pos As Long
  
  If args = "" Then
    args = Trim$(Command$)
  Else
    args = Trim$(args)
  End If
    
  argc = 0
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

Function GisStartModule(Title As String, argcnt As Long, Optional args As String = "") As Integer
  GisStartModule = FAILURE
  GisGetArguments args
  If argc < argcnt Then
    MsgBox "Invalid arguments passed to module" & Chr(10) & Command & Chr(10) & "Contact PNNL", 16, "Usage error!"
    Exit Function ' End
  End If
  FUIName = argv(argcnt - 5) '& ".GID"
  RunName = argv(argcnt - 4)
  SiteIdx = Val(argv(argcnt - 3))
  ModIdx = Val(argv(argcnt - 2))
  ModName = argv(argcnt - 1)

  If SiteIdx + ModIdx < 2 Then
    MsgBox "Invalid arguments passed to module" & Chr(10) & Command & Chr(10) & "Contact PNNL", 16, "Usage error!"
    Exit Function '     End
  End If
  If Not open_csv(errfile, RunName & ".ERR", 1) Then
    MsgBox "Unable to create file " & RunName & ".ERR" & Chr(10) & "Check directory permissions", 16, "File IO error!"
    Exit Function
  End If
  AnError = False
  GisStartModule = SUCCESS
End Function

Sub GisEndModule()
Dim i As Integer
'On Error Resume Next
' For i = 1 To UBound(layers)
'   With layers(i)
'     If .workarea > 0 And .opened Then
'       frmMain.Dbcocx1.devcloseall
'       Exit For
'     End If
'   End With
' Next i
  
  If "" <> dir$(DBFileDir & "\~temp*.*") Then Kill DBFileDir & "\~temp*.*"
  If "" <> dir$(SplitPath(RunName, SP_DIR) & "SECTION.ndx") Then
    Kill SplitPath(RunName, SP_DIR) & "SECTION.ndx"
  End If
  
  close_csv errfile
  If Not AnError Then Kill RunName & ".ERR"
  
  For i = 0 To Forms.Count - 1
    If Forms(i).Caption <> frmMain.Caption Then
      Unload Forms(i)
    End If
  Next
  Unload frmMain
  End
End Sub


Function IsGisConnectedToModule(gis As String) As Integer
Dim fle As parmfile, temp As parmrec, i As Long, modix As Long, m As Long

  IsGisConnectedToModule = FAILURE
  
  If open_parm(fle, FUIName & ".gid", 2) Then
    Do Until EOCF(fle.file)
      If read_parmrec(fle, temp) Then
        Select Case temp.pName
          Case "csm"
            For i = 1 To temp.idx1
              If 1 = read_parmrec(fle, temp) Then
                If temp.idx1 = SiteIdx Then
                  Select Case temp.pName
                    Case "modid":
                      If temp.pval = ModName Then modix = temp.idx2
                    Case "modsrcid"
                      If temp.idx2 = modix Then
                        If "gis" = Left(temp.pval, 3) Then
                          gis = temp.pval
                          IsGisConnectedToModule = SUCCESS
                          Exit Do
                        End If
                      End If
                  End Select
                End If
              End If
            Next
          Case Else
            For m = 1 To temp.idx1
              get_line fle.file
            Next
        End Select
      End If
    Loop
    close_parm fle
  End If
End Function


Public Function DistanceFormula(x1 As Double, y1 As Double, x2 As Double, y2 As Double) As Double
Dim v As Double
    DistanceFormula = 0#
    v = (x2 - x1) ^ 2 + (y2 - y1) ^ 2
    If v > 0 Then DistanceFormula = Sqr(v)
End Function


Public Function AddModule(mname As String)
Dim n As Long
Dim obj As ModClass
  
  Set obj = New ModClass
  obj.id = mname
  n = 1 + UBound(module)
  ReDim Preserve module(n)
  Set module(n) = obj
End Function


Public Function EntityInUse() As Boolean
Dim i As Long, inuse As Boolean
  For i = 1 To UBound(module)
    inuse = inuse Or module(i).EntityInUse()
  Next i
  EntityInUse = inuse
End Function


Public Function InsertGridPolar(dbc As Dbcocx) As String
Dim insx As Double, insy As Double, x1 As Double, y1 As Double
Dim id As String: id = ""
  GetPoint "Click on center coordinate of polar grid (right button to abort).", insx, insy
  GetPointDrag "Move mouse to length of radius and click to complete polar grid.", x1, y1, insx, insy, 1
  id = InsertPGridIntoDb(dbc, insx, insy, x1, y1)
  dbc.devgrdisplayid id, defaults(DPGRID).color
  InsertGridPolar = id
End Function


Public Function InsertGridCartesian(dbc As Dbcocx) As String
Dim insx As Double, insy As Double, x1 As Double, y1 As Double
Dim id As String: id = ""

  GetPoint "Click on coordinate to locate cartesian grid (right button to abort).", insx, insy
  GetPointDrag "Move mouse to expand to size, then click to complete grid.", x1, y1, insx, insy, 2
  id = InsertCGridIntoDb(dbc, insx, insy, x1, y1)
  dbc.devgrdisplayid id, defaults(DCGRID).color
  InsertGridCartesian = id
End Function
  
Public Sub LayerSet(dbc As Dbcocx, layername As String)
  dbc.devgrcmdlayerset layername
End Sub


Public Function GetLayerKey(l As Long) As String
Dim fName As String
  If l = 0 Then
    GetLayerKey = LCase("," & SplitPath(FUIName, SP_TITLE))
  Else
    fName = SplitPath(layers(l).filename, SP_TITLE)
    GetLayerKey = LCase("," & Left(fName, InStr(fName, ".") - 1))
  End If
End Function

Public Sub RefreshGrDisplay()
Dim l As Long
  If SelectedEntityId <> "" And SelectedLayer >= 0 Then
    For l = 0 To UBound(layers)
      SelectDbgr l, True
      frmMain.Dbcocx1.devgrdisplay
    Next l
    SelectDbgr 0, True
  End If
End Sub

Public Function GisConnect(args As String) As Integer
Dim status As Integer

  status = GisStartModule("GIS", 5, args)
  If status = SUCCESS Then
    If "gis" = Left(ModName, 3) Then
      GisName = ModName
    Else
      status = IsGisConnectedToModule(GisName)
      If status = FAILURE Then
        MsgBox "GisConnect Failed" & vbCrLf & " -- The GIS must be connected as a source to " & ModName
      End If
     End If
   End If
  GisConnect = status
End Function


Public Function CountPointGridPoints(dbc As Dbcocx, id As String, layer As Long) As Long
Dim l As Long, recno As Long, npt As Long, slayer As Long, rno As Long
  slayer = SelectedLayer:   rno = dbc.devrecno ' save state
  
  CountPointGridPoints = 0&
  If id = "" Then Exit Function
   SelectDbgr layer, True, dbc
   dbc.devgrcmdsavepolyg id
   For l = 0 To UBound(layers)
     If l = 0 Or (layers(l).visible And layers(l).pickable) Then
       SelectDbgr l, True, dbc
       recno = 0
       dbc.devgrseekp 1, recno
       While 0 <> dbc.devfound()
         If Left(dbc.devgetfield("DESCR"), 5) = "POINT" Then npt = npt + 1
         recno = dbc.devrecno()
         dbc.devgrseekp 1, recno
       Wend
     End If
   Next l
   
   SelectDbgr slayer, True, dbc:   dbc.devgorecno rno  ' restore state
   
   CountPointGridPoints = npt
End Function

Public Function FormatPointGridMessage(npt&, min&, max&) As String
Dim msg As String
  msg = "The number of grid points (" + CStr(npt) + ") is "
  If npt >= min And npt <= max Then
    msg = msg + "within the range "
  Else
    If npt < min Then msg = msg + "less than the minimum "
    If npt > max Then msg = msg + "greater than the maximum "
  End If
  msg = msg + "required by the model (" + CStr(min) + " to " + CStr(max) + ")"
  FormatPointGridMessage = msg
End Function

Public Function ConfirmPointGridPoints(id As String, layer As Long, min As Long, max As Long) As Boolean
  Dim npt As Long, ans
  Dim slayer As Long, rno As Long
  Dim msg As String
  
  Dim dbc As Dbcocx: Set dbc = frmMain.Dbcocx1
  slayer = SelectedLayer:   rno = dbc.devrecno ' save state
  
  npt = CountPointGridPoints(dbc, id, layer)
  msg = FormatPointGridMessage(npt, min, max)
  ans = MsgBox(msg, vbOKCancel)
  ConfirmPointGridPoints = IIf(ans = vbCancel, False, True)
  
  ' restore original conditions
  SelectDbgr slayer, True:   dbc.devgorecno rno ' restore state
End Function


Public Function LocatePointGridPoints(dbc As Dbcocx, id As String, Optional display As Boolean = False, Optional idx1& = 0) As Long
Dim l As Long, recno As Long, ptid As String, npt As Long, parm As parmrec

  LocatePointGridPoints = 0
  
  If id = "" Then Exit Function

  If id <> "" Then
    SelectDbgr 0, True
    dbc.devgrcmdsavepolyg id
    For l = 0 To UBound(layers)
      If l = 0 Or (layers(l).visible And layers(l).pickable) Then
        SelectDbgr l, True
        recno = 0
        dbc.devgrseekp 1, recno
        While 0 <> dbc.devfound()
          If Left(dbc.devgetfield("DESCR"), 5) = "POINT" Then
            npt = npt + 1
            If display Then
              dbc.devgrdisplayid dbc.devgetid(), 1
            Else
              set_parm parm, "GridPtCoord", idx1, npt, 1, 0, 0, 0, 0, "n/a", "n/a", dbc.devgetfield("VX1")
              write_parmrec pfile, parm
              set_parm parm, "GridPtCoord", idx1, npt, 2, 0, 0, 0, 0, "n/a", "n/a", dbc.devgetfield("VY1")
              write_parmrec pfile, parm
              set_parm parm, "GridPtCoord", idx1, npt, 3, 0, 0, 0, 0, "n/a", "n/a", CStr(0)
              write_parmrec pfile, parm
            End If
          End If
          recno = dbc.devrecno()
          dbc.devgrseekp 1, recno
        Wend
      End If
    Next l
    SelectDbgr 0, True
    dbc.devgrcmdfreepolyg
    If Not display Then
      set_parm parm, "NumGridPts", idx1, 0, 0, 0, 0, 0, 0, "n/a", "n/a", CStr(npt)
      write_parmrec pfile, parm
    End If
  End If
  LocatePointGridPoints = npt
  
End Function
