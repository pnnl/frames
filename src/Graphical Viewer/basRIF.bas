Attribute VB_Name = "basRIF"
Option Explicit
Option Compare Text

Function rifSheetChange(col As Long, row As Long) As Boolean
  Dim retCode As Boolean
  
  Select Case row
    Case CBO_DS
      updateLocations col
      updateAgeGroup col
      updateConstituents col
      GetRoutesAndPathways col
      updateMeasures col
      updateUnits col
      updateRoutes col
      updatePaths col
      retCode = rifGetSeries(col)
    Case CBO_XY
      retCode = rifGetSeries(col)
    Case CBO_AGE
      updateConstituents col
      GetRoutesAndPathways col
      updateMeasures col
      updateUnits col
      updateRoutes col
      updatePaths col
      retCode = rifGetSeries(col)
    Case CBO_CHM
      GetRoutesAndPathways col
      updateMeasures col
      updateUnits col
      updateRoutes col
      updatePaths col
      retCode = rifGetSeries(col)
    Case CBO_MSR
      If col <> lastcol Then GetRoutesAndPathways col
      updateUnits col
      updateRoutes col
      updatePaths col
      retCode = rifGetSeries(col)
    Case CBO_UNT
      If col <> lastcol Then GetRoutesAndPathways col
      updateRoutes col
      updatePaths col
      retCode = rifGetSeries(col)
    Case CBO_RTE
      If col <> lastcol Then GetRoutesAndPathways col
      updatePaths col
      retCode = rifGetSeries(col)
    Case CBO_PTH
      retCode = rifGetSeries(col)
  End Select
  lastcol = col
  rifSheetChange = retCode
End Function

Function rifGetSeries(col As Long) As Boolean
  Dim i As Long
  Dim m As Long
  Dim dsIdx As Long
  Dim nvalues As Long
  Dim ageIdx As Long
  Dim xyIdx As Long
  Dim casid As String
  Dim rte As String
  Dim Path As String
  Dim measure As String
  Dim Unit As String
  Dim xvalue As Double
  Dim yvalue As Double
  
  rifGetSeries = False
  ReDim xValues(0)
  ReDim yValues(0)
  dsIdx = GetSelIdx(CBO_DS, col)
  xyIdx = GetSelIdx(CBO_XY, col)
  ageIdx = GetSelIdx(CBO_AGE, col)
  measure = GetSelText(CBO_MSR, col)
  Unit = GetSelText(CBO_UNT, col)
  rte = GetSelText(CBO_RTE, col)
  Path = GetSelText(CBO_PTH, col)
  casid = GetSelText(CBO_CHM, col)
  If measure = NOT_AVAILABLE Or _
     Unit = NOT_AVAILABLE Or _
     rte = NOT_AVAILABLE Or _
     Path = NOT_AVAILABLE Then Exit Function
  
  i = GetAllContIdx(casid)
  If i > -1 Then
    nvalues = rifLoadTimeSeries(dsIdx, xyIdx, ageIdx, allCAS(i).cas, allCAS(i).pcas(0), Path, rte, measure, Unit)
    If UBound(xValues) = 0 Then ReDim xValues(nvalues)
    If UBound(yValues) = 0 Then ReDim yValues(nvalues)
    For m = 0 To nvalues - 1
      rifGetTimeAndValue dsIdx, m, xvalue, yvalue
      xValues(m) = xvalue
      yValues(m) = yvalue
    Next
  End If
  
  xlabel = "yr"
  ylabel = Unit
  
  rifGetSeries = True
End Function

Function rifLoadDatasets() As Long
Dim i As Long
Dim j As Long
Dim retc As Long
Dim tstr As String
Dim tstr2 As String
Dim tstr3 As String
Dim cbolist As String

  On Error GoTo ErrorHandler
  
  task = "numds = readRIFDatasets(FUIName, ModName)"
  numDS = rifOpen(FUIName, ModName)
  
  ReDim ds(numDS)
  tstr = String(1024, Chr(0))
  tstr2 = String(1024, Chr(0))
  tstr3 = String(1024, Chr(0))
  cbolist = ""
  For i = 0 To numDS - 1
     task = "retc = rifGetDatasetInfo(i, ds(i).numPt, ds(i).numAge, tstr, tstr2, tstr3)"
     retc = rifGetSetInfo(i, ds(i).numPt, ds(i).numAge, tstr, tstr2, tstr3)
     ds(i).locName = Left(tstr, InStr(tstr, Chr(0)) - 1)
     ds(i).locType = Left(tstr2, InStr(tstr2, Chr(0)) - 1)
     ds(i).locExp = Left(tstr3, InStr(tstr3, Chr(0)) - 1)
     
     task = "retc = rifGetExposurePoint(i, j, ds(i).x(j), ds(i).y(j))"
     ReDim ds(i).x(ds(i).numPt)
     ReDim ds(i).y(ds(i).numPt)
     For j = 0 To ds(i).numPt - 1
       retc = rifGetExposurePoint(i, j, ds(i).x(j), ds(i).y(j))
     Next
     
     task = "retc = rifGetAgeGroup(i, j, ds(i).agemin(j), ds(i).agemax(j))"
     ReDim ds(i).ageMin(ds(i).numAge)
     ReDim ds(i).ageMax(ds(i).numAge)
     For j = 0 To ds(i).numAge - 1
       retc = rifGetAgeGroup(i, j, ds(i).ageMin(j), ds(i).ageMax(j))
     Next
  Next

ErrorHandler:
  If Err.Number <> 0 Then
    PutError task
    numDS = 0
  End If
  rifLoadDatasets = numDS
End Function
