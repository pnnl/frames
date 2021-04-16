VERSION 5.00
Object = "{C0A90FC0-6A87-11D2-98B7-0000E8CCF7AF}#1.0#0"; "dbcgeo.ocx"
Object = "{EB962840-1204-11D0-98B4-0000E8CCF7AF}#1.0#0"; "dbcocx32.ocx"
Object = "{00028C01-0000-0000-0000-000000000046}#1.0#0"; "DBGRID32.OCX"
Begin VB.Form frmMain 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Protective Action Recommendations Module"
   ClientHeight    =   9864
   ClientLeft      =   48
   ClientTop       =   432
   ClientWidth     =   7128
   Icon            =   "frmMain.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   9864
   ScaleWidth      =   7128
   StartUpPosition =   2  'CenterScreen
   Begin MSDBGrid.DBGrid DBGrid1 
      Bindings        =   "frmMain.frx":030A
      Height          =   1932
      Left            =   240
      OleObjectBlob   =   "frmMain.frx":031E
      TabIndex        =   2
      Top             =   840
      Width           =   6252
   End
   Begin VB.Data Data1 
      Caption         =   "Data1"
      Connect         =   "dBASE III;"
      DatabaseName    =   ""
      DefaultCursorType=   0  'DefaultCursor
      DefaultType     =   2  'UseODBC
      Exclusive       =   0   'False
      Height          =   492
      Left            =   120
      Options         =   0
      ReadOnly        =   0   'False
      RecordsetType   =   1  'Dynaset
      RecordSource    =   ""
      Top             =   2880
      Width           =   6612
   End
   Begin DBCOCXLib.Dbcocx Dbcocx1 
      Height          =   6132
      Left            =   240
      TabIndex        =   1
      Top             =   3480
      Width           =   6612
      _Version        =   65958
      _ExtentX        =   11663
      _ExtentY        =   10816
      _StockProps     =   0
   End
   Begin DBCGEOLib.DbcGeo DbcGeo1 
      Left            =   6360
      Top             =   120
      _Version        =   65536
      _ExtentX        =   868
      _ExtentY        =   868
      _StockProps     =   0
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      AutoSize        =   -1  'True
      Caption         =   "Summary"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.6
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   240
      Left            =   300
      TabIndex        =   0
      Top             =   345
      Width           =   6600
   End
End
Attribute VB_Name = "frmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Compare Text
Option Explicit

Dim landZn As Collection
Dim shoreZn As Collection
Dim waterZn As Collection

Dim landMax As Double
Dim shoreMax As Double
Dim waterMax As Double
Dim layers() As LAYERSTRUCT

Function AddLayer(id As String, bind As String, tbl As String, qry As String) As Boolean
Dim i As Long
Dim l As Long
Dim k As Long
Dim result As String
'Dim id As Long
'Dim mytext As String
Dim filetitle As String
Dim filedir As String
Dim fileext As String
Dim lname As String
Dim mytext As String
Dim lcount As Integer
Dim db As Database

  AddLayer = False
  
  ' single file
  filedir = SplitPath(bind, SP_DIR)
  filetitle = SplitPath(bind, SP_TITLE)
  fileext = SplitPath(bind, SP_EXT)

  'check for duplicate
  For i = 1 To UBound(layers)
    If layers(i).name = filetitle & fileext Then
      AddLayer = True
      Exit Function
    End If
  Next

  l = 1 + UBound(layers)
  ReDim Preserve layers(l)
  layers(l).visible = True
  
  Select Case fileext
    Case ".shp"
      With layers(l)
        .name = filetitle & fileext
        .filename = filedir & filetitle & fileext
        .bkgd = False
        .dwg = True
        .color = 7 ' black
        .lcolor = 7 ' black
        .visible = True
        .pickable = False
        Dbcocx1.devdrawdwg .filename, .lcolor, ""
        Dbcocx1.devselect l
        Dbcocx1.devgrcreateext "my" & filetitle, 25
        Dbcocx1.devgrappend .filename
        
        lcount = Dbcocx1.devreccount
        
        Data1.DatabaseName = filedir
'        Data1.RecordSource = "Select * from " & tbl & "where" & qry
        Data1.RecordSource = "Select * from " & filetitle
        Data1.Refresh
        Data1.Recordset.MoveLast
        lcount = Data1.Recordset.RecordCount
   
        For k = 1 To lcount
          Data1.Recordset.MoveFirst
          Dbcocx1.devseek "E" & Hex(k)
          result = Dbcocx1.devgetfield("EXTLINK")
          Data1.Recordset.Move CLng(result) - 1
          mytext = Data1.Recordset.Fields("TYPE").value + "|" + Data1.Recordset.Fields("ZONE").value
          Dbcocx1.devsetfield "TEXT", mytext
        Next
        
'        Dbcocx1.grdevcopyto
        Dbcocx1.devgrdisplay
      End With
    Case ".dwg", ".dxf"  ' map
      With layers(l)
        .name = filetitle & fileext
        .filename = filedir & filetitle & fileext
        .bkgd = False
        .dwg = True
        .color = 7 ' black
        .lcolor = 7 ' black
        Dbcocx1.devdrawdwg .filename, .lcolor, ""
      End With
    Case Else ' image
      With layers(l)
        .name = filetitle & fileext
        .filename = filedir & filetitle & fileext
        .bkgd = True
        .dwg = False
        .color = 7 ' black
        .lcolor = 7 ' black
      End With
  End Select
  
  AddLayer = True
End Function

Private Sub LoadZones(zonetype As String, zones As Collection)
Dim i As Long
Dim iZnSet As Long
Dim numZones As Long
Dim id As String
Dim bind As String
Dim tbl As String
Dim qry As String

  iZnSet = IconGetInputDataSet(PID, modid, zonetype, 1)
  numZones = DataSetDimensionCount(PID, iZnSet, "Feature", SetIdx())
  
  iChmSet = IconGetInputDataSet(PID, modid, "ChemList", 1)
  numChms = DataSetDimensionCount(PID, iChmSet, "CASID", SetIdx())
  iRadSet = IconGetInputDataSet(PID, modid, "RadList", 1)
  numRads = DataSetDimensionCount(PID, iRadSet, "CASID", SetIdx())
  redim wMax(
  
  For i = 1 To numZones
    id = DataSetReadString1(PID, iZnSet, "Feature", "", i)
    bind = DataSetReadString1(PID, iZnSet, "DataBinding", "", i)
    qry = DataSetReadString1(PID, iZnSet, "DataQuery", "", i)
    tbl = DataSetReadString1(PID, iZnSet, "DataTable", "", i)
    AddLayer id, bind, tbl, qry
  Next
  
End Sub

Private Sub IndirectImpacts()

End Sub

Private Sub DirectImpacts()


End Sub

Private Sub LoadWaterConcentration(prefix As String, _
                                   casidx As Long, _
                                   threshold As Double, _
                                   timeEvt As Double, _
                                   timePov As Double, _
                                   actual As Boolean)
Dim numPts As Long
Dim numTimes As Long
Dim iDataSet As Long
Dim oDataSet As Long

Dim t() As Double
Dim v() As Double
Dim x() As Double
Dim y() As Double
Dim z() As Double

  iPtSet = IconGetInputDataSet(PID, modid, "SurfaceWaterPoints", 1)
  iConcSet = IconGetInputDataSet(PID, modid, prefix & "SurfaceWaterConc", 1)
  
  numPts = DataSetDimensionCount(PID, iPtSet, "Feature", SetIdx())
  numTimes = DataSetDimensionCount(PID, iConcSet, "TimePts", SetIdx())
  ReDim x(numPts) As Double
  ReDim y(numPts) As Double
  ReDim z(numPts) As Double
  
  ReDim t(numTimes) As Double
  ReDim v(numTimes) As Double
  
  For i = 1 To numPts
    x(i) = DataSetReadReal3(PID, iPtSet, "FeaturePts", "m", i, 1, 1)
    y(i) = DataSetReadReal3(PID, iPtSet, "FeaturePts", "m", i, 2, 1)
    
    
    ' want integrated plume, sort of. using max for each point
    For j = 1 To numTimes
      t(j - 1) = DataSetReadReal3(PID, iConcSet, "TimePts", "yr", i, casidx, j)
      If t(j - 1) > endtime Then Exit For
      If prefix = "Rad" Then
        v(j - 1) = DataSetReadReal3(PID, iConcSet, "Conc", "Bq/L", i, casidx, j)
      Else
        v(j - 1) = DataSetReadReal3(PID, iConcSet, "Conc", "mg/L", i, casidx, j)
      End If
      If v(j - 1) > z(i) Then z(i) = v(j - 1)
    Next
    Duration = ExceedDelta(x, y, numTimes, threshold, 0, numTimes - 1)
    
    If Duration > 0 Then
      'Find zone idx
'      zone = findzone("MSZ",x(i),y(i))
      If Duration > wdur(zone, casidx) Then
        wdur(zone, casidx) = Duration
        wXY(zone, casidx, 1) = x(i)
        wXY(zone, casidx, 2) = y(i)
      End If
      If z(i) > wMax(zone, casidx) Then
        wMax(zone, casidx) = z(i)
        wXY(zone, casidx, 1) = x(i)
        wXY(zone, casidx, 2) = y(i)
      End If
    End If
  Next

End Sub

Private Sub Form_Load()
  Dim strFileName As String
  Dim lngCount As Long

  ReDim layers(0) As LAYERSTRUCT
  StartModule 3
  
  strFileName = String(255, 0)
  lngCount = GetModuleFileName(App.hInstance, strFileName, 255)
  strFileName = Left(strFileName, lngCount)
  
  If UCase(Right(strFileName, 7)) = "VB6.EXE" Then
      strFileName = "C:\Program files\Framesv2"
      strFileName = Replace(strFileName, Chr$(34), "") ' remove quotes
      ChDir strFileName
      hLibModule = LoadLibrary(strFileName & "\systemio.dll")
  End If
  
  PID = ModuleDevOpen(SimPath, SimName, modid)
  If PID <= 0 Then
    MsgBox "Invalid PID: " & PID & " " & SimPath
    End
  End If
    
  LoadZones "LandZones", landZn
  LoadZones "ShorelineZones", shoreZn
  LoadZones "WaterZones", waterZn
  
End Sub

Private Sub Form_Unload(cancel As Integer)
    On Error Resume Next
   
    Close
    If 0 <> hLibModule Then
      ModuleDevClose PID, 0
      FreeLibrary hLibModule
    Else
      ModuleDevClose PID, 0
    End If

End Sub

Public Sub OpenGrDatabase()
Dim i As Long
Dim Action As String

On Error GoTo OpenGrDatabase_error

  Dbcocx1.devsetautoregen 1 ' autoregen = true
  Dbcocx1.devsetaperture 6 ' for making easy to catch a block with the mouse
  Dbcocx1.devsetgrseekall 1 ' search also the polygon lines, not only the insertion point

  Dbcocx1.devdisplayxyext 1000, 1000, 1 ' default raster background
  Dbcocx1.devcalibrate 0, 999, 0, 0, 999, 0, 1000, 1000

  mnuDB.Enabled = True
    
' GetDatabaseLayers Dbcocx1, 0, cboLayer, layers()

' cboLayer.Clear
' If 0 < UBound(layers) Then
    DisplayBackground
'   For i = 1 To UBound(layers)
'     If layers(i).visible And Not layers(i).bkgd Then
'       cboLayer.AddItem layers(i).name
'       cboLayer.ItemData(cboLayer.NewIndex) = i
'     End If
'   Next i
'   If 0 < cboLayer.ListCount Then cboLayer.ListIndex = 0
' End If
' Dbcocx1.devgrdisplay
    
  mnuInsert.Enabled = VisibleDataLayers()
  mnuZoom.Enabled = mnuInsert.Enabled
  Exit Sub
  
OpenGrDatabase_error:
  MsgBox Error & " action:" & Action

End Sub

Public Function SelectGraphicDatabase(dbname As String, ext As String) As Boolean
  SelectGraphicDatabase = False
  
  DBFileTitle = SplitPath(dbname, SP_TITLE) & ".dbf"
  If (".sim") = Right(DBFileTitle, 4) Then
    DBFileTitle = Left(DBFileTitle, Len(DBFileTitle) - 4) & ".dbf"
  End If
  
  DBTableName = dbname ' & ".dbf"
  DBFileDir = SplitPath(dbname, SP_DIR)
  
  If ("." & ext) = Right(DBTableName, 4) Then
    DBTableName = Left(DBTableName, Len(DBTableName) - 4)
  End If
  
  SelectGraphicDatabase = True
End Function

Private Sub CreateGraphicDatabase(dbname As String, ext As String)
Dim id As String
    
  Dbcocx1.devsetdbdrv "dbcdbf32.dll", "", 0
  Dbcocx1.devgrclear 0
  Dbcocx1.devgrcreateext dbname, 25
  
  Dbcocx1.devgrcmdblock "FancyPoint"
  id = Dbcocx1.devgetid()
  Dbcocx1.devgrcmdline id, 0, 0, 3, 3
  Dbcocx1.devgrcmdline id, 0, 0, 3, -3
  Dbcocx1.devgrcmdline id, 0, 0, -3, -3
  Dbcocx1.devgrcmdline id, 0, 0, -3, 3
  Dbcocx1.devgrcmdcircle id, 0, 0, 3
  Dbcocx1.devgrcmdendblock
  
  Dbcocx1.devgrcmdlayerdef TUSER, 7, 1
' dbcocx1.devgrcmdlayerdef TPICK, 7, 1
 
  Dbcocx1.devcloseall
End Sub
