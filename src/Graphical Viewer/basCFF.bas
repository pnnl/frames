Attribute VB_Name = "basCFF"
Option Explicit
Option Compare Text

Function scfSheetChange(col As Long, row As Long) As Boolean
  Dim retcode As Boolean
  Select Case row
    Case CBO_DS
      updateContams col
      retcode = scfGetSeries(col)
    Case CBO_CHM
      retcode = scfGetSeries(col)
  End Select
  scfSheetChange = retcode
End Function

Function scfGetSeries(col As Long) As Boolean
  Dim i As Long
  Dim dsIdx As Long
  Dim conIdx As Long
  Dim nvalues As Long
  Dim newunits As String

  scfGetSeries = False
  dsIdx = GetSelIdx(CBO_DS, col) + 1
  conIdx = GetSelIdx(CBO_CHM, col) + 1
  
  nvalues = scfGetSeriesProperties(dsIdx, conIdx, 0, ylabel, xlabel)
  ReDim xValues(nvalues)
  ReDim yValues(nvalues)
  If nvalues = 0 Then Exit Function
  nvalues = scfGetSeriesValues(dsIdx, conIdx, 0, nvalues, xValues(0), yValues(0))
  'ERDC request that viewer output values are in mg/kg
  For i = 0 To nvalues - 1
    yValues(i) = DoUnitConversion(ylabel, yValues(i), newunits)
  Next
  ylabel = newunits
  
  scfGetSeries = True
End Function

Function scfLoadDatasets() As Long
Dim i As Long
Dim retc As Long

  On Error GoTo ErrorHandler
  
  task = "numds = readscfDatasets(" & FUIName & ", " & modName & ")"
  scfOpen FUIName, modName
  numDS = scfGetNumSets()
  ReDim ds(numDS)
  For i = 0 To numDS - 1
     task = "retc = scfGetDatasetInfo(i + 1, ds(i).locName, ds(i).locType)"
     retc = scfGetSetInfo(i + 1, ds(i).locName, ds(i).locType)
     'retc = scfGetDimensions(i + 1, width, length, depth)
  Next
  
ErrorHandler:
  If Err.Number <> 0 Then
    PutError task
    numDS = 0
  End If
  scfLoadDatasets = numDS
End Function

Function wcfSheetChange(col As Long, row As Long) As Boolean
  Dim retcode As Boolean
  Select Case row
    Case CBO_DS
      updateContams col
      retcode = wcfGetSeries(col)
    Case CBO_CHM
      retcode = wcfGetSeries(col)
  End Select
  wcfSheetChange = retcode
End Function

Function wcfGetSeries(col As Long) As Boolean
  Dim i As Long
  Dim dsIdx As Long
  Dim conIdx As Long
  Dim nvalues As Long
  Dim newunits As String

  wcfGetSeries = False
  dsIdx = GetSelIdx(CBO_DS, col) + 1
  conIdx = GetSelIdx(CBO_CHM, col) + 1
  
  nvalues = wcfGetSeriesProperties(dsIdx, conIdx, 0, ylabel, xlabel)
  ReDim xValues(nvalues)
  ReDim yValues(nvalues)
  If nvalues = 0 Then Exit Function
  nvalues = wcfGetSeriesValues(dsIdx, conIdx, 0, nvalues, xValues(0), yValues(0))
  'ERDC request that viewer output values are in mg/L
  For i = 0 To nvalues - 1
    yValues(i) = DoUnitConversion(ylabel, yValues(i), newunits)
  Next
  ylabel = newunits
  
  wcfGetSeries = True
End Function

Function wcfLoadDatasets() As Long
Dim i As Long
Dim retc As Long
Dim x As Double
Dim y As Double
Dim z As Double

  On Error GoTo ErrorHandler
  
  task = "numds = readwcfDatasets(" & FUIName & ", " & modName & ")"
  wcfOpen FUIName, modName
  numDS = wcfGetNumSets()
  ReDim ds(numDS)
  For i = 0 To numDS - 1
     task = "retc = wcfGetDatasetInfo(i + 1, ds(i).locName, ds(i).locType)"
     retc = wcfGetSetInfo(i + 1, ds(i).locName, ds(i).locType)
     retc = wcfGetLocation(i + 1, x, y, z)
  Next
  
ErrorHandler:
  If Err.Number <> 0 Then
    PutError task
    numDS = 0
  End If
  wcfLoadDatasets = numDS
End Function

Function affSheetChange(col As Long, row As Long) As Boolean
  Dim retcode As Boolean
  Select Case row
    Case CBO_DS
      updateContams col
      updateAirFlux col
      retcode = affGetSeries(col)
    Case CBO_CHM, CBO_FLX
      retcode = affGetSeries(col)
  End Select
  affSheetChange = retcode
End Function

Function affGetSeries(col As Long) As Boolean
  Dim dsIdx As Long
  Dim conIdx As Long
  Dim flxIdx As Long
  Dim nvalues As Long

  affGetSeries = False
  dsIdx = GetSelIdx(CBO_DS, col) + 1
  conIdx = GetSelIdx(CBO_CHM, col) + 1
  flxIdx = GetSelIdx(CBO_FLX, col) + 1
  
  nvalues = affGetSeriesProperties(dsIdx, conIdx, 0, ylabel, xlabel)
  ReDim xValues(nvalues)
  ReDim yValues(nvalues)
  If nvalues = 0 Then Exit Function
  nvalues = affGetSeriesValues(dsIdx, conIdx, 0, flxIdx, nvalues, xValues(0), yValues(0))
  
  affGetSeries = True
End Function

Function affLoadDatasets() As Long
Dim i As Long
Dim retc As Long

  On Error GoTo ErrorHandler
  
  task = "numds = readaffDatasets(fuiname, ModName)"
  affOpen FUIName, modName
  numDS = affGetNumSets()
  
  ReDim ds(numDS)
  For i = 0 To numDS - 1
     task = "retc = affGetDatasetInfo(i + 1, ds(i).locName, ds(i).locType)"
     retc = affGetSetInfo(i + 1, ds(i).locName, ds(i).locType)
     'retc = affGetDimensions(i + 1, width, length, depth)
  Next
  
ErrorHandler:
  If Err.Number <> 0 Then
    PutError task
    numDS = 0
  End If
  affLoadDatasets = numDS
End Function

Function wffSheetChange(col As Long, row As Long) As Boolean
  Dim retcode As Boolean
  Select Case row
    Case CBO_DS
      updateContams col
      updateWaterFlux col
      retcode = wffGetSeries(col)
    Case CBO_CHM, CBO_FLX
      retcode = wffGetSeries(col)
  End Select
  wffSheetChange = retcode
End Function

Function wffGetSeries(col As Long) As Boolean
  Dim dsIdx As Long
  Dim conIdx As Long
  Dim flxIdx As Long
  Dim nvalues As Long

  wffGetSeries = False
  dsIdx = GetSelIdx(CBO_DS, col) + 1
  conIdx = GetSelIdx(CBO_CHM, col) + 1
  flxIdx = GetSelIdx(CBO_FLX, col) + 1
  
  nvalues = wffGetSeriesProperties(dsIdx, conIdx, 0, ylabel, xlabel)
  ReDim xValues(nvalues)
  ReDim yValues(nvalues)
  If nvalues = 0 Then Exit Function
  nvalues = wffGetSeriesValues(dsIdx, conIdx, 0, flxIdx, nvalues, xValues(0), yValues(0))
  wffGetSeries = True
End Function

Function wffLoadDatasets() As Long
Dim i As Long
Dim retc As Long

  On Error GoTo ErrorHandler
  
  task = "numds = readwffDatasets(fuiname, ModName)"
  wffOpen FUIName, modName
  numDS = wffGetNumSets()
  
  ReDim ds(numDS)
  For i = 0 To numDS - 1
     task = "retc = wffGetDatasetInfo(i + 1, ds(i).locName, ds(i).locType)"
     retc = wffGetSetInfo(i + 1, ds(i).locName, ds(i).locType)
     'retc = wffGetDimensions(i + 1, width, length, depth)
  Next
  
ErrorHandler:
  If Err.Number <> 0 Then
    PutError task
    numDS = 0
  End If
  wffLoadDatasets = numDS
End Function


