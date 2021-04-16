Attribute VB_Name = "basEPF"
Option Explicit
Option Compare Text

Function epfSheetChange(col As Long, row As Long) As Boolean
  Dim retCode As Boolean
  Select Case row
    Case CBO_DS
      updateLocations col
      updateConstituents col
      GetRoutesAndPathways col
      updateRoutes col
      updatePaths col
      retCode = epfGetSeries(col)
    Case CBO_XY
      retCode = epfGetSeries(col)
    Case CBO_CHM
      GetRoutesAndPathways col
      updateRoutes col
      updatePaths col
      retCode = epfGetSeries(col)
    Case CBO_RTE
      If col <> lastcol Then GetRoutesAndPathways col
      updatePaths col
      retCode = epfGetSeries(col)
    Case CBO_PTH
      retCode = epfGetSeries(col)
  End Select
  lastcol = col
  epfSheetChange = retCode
End Function

Function epfGetSeries(col As Long) As Boolean
  Dim i As Long
  Dim dsIdx As Long
  Dim xyIdx As Long
  Dim rte As String
  Dim Path As String
  Dim nvalues As Long
  Dim casid As String

  epfGetSeries = False
  dsIdx = GetSelIdx(CBO_DS, col)
  xyIdx = GetSelIdx(CBO_XY, col)
  casid = GetSelText(CBO_CHM, col)
  rte = GetSelText(CBO_RTE, col)
  Path = GetSelText(CBO_PTH, col)
  If rte = NOT_AVAILABLE Or Path = NOT_AVAILABLE Then Exit Function
  i = GetAllContIdx(casid)
  
  If i > -1 Then
    nvalues = epfLoadTimeSeries(dsIdx, xyIdx, allCAS(i).cas, allCAS(i).pcas(0), Path, rte)
    ReDim xValues(nvalues)
    ReDim yValues(nvalues)
    For i = 0 To nvalues - 1
      epfGetTimeAndValue dsIdx, i, xValues(i), yValues(i)
    Next
  End If
  
  xlabel = "yr"
  ylabel = ""
  For i = 1 To UBound(uExposure)
    If rte = uExposure(i).Route And Path = uExposure(i).Path Then
      ylabel = uExposure(i).Unit
      Exit For
    End If
  Next
  
  epfGetSeries = True
End Function

Function epfLoadDatasets() As Long
Dim i As Long
Dim j As Long
Dim retc As Long
Dim tstr As String
Dim tstr2 As String
Dim tstr3 As String
Dim cbolist As String

  On Error GoTo ErrorHandler
  
  task = "numds = readEPFDatasets(fuiname, ModName)"
  numDS = epfOpen(FUIName, ModName)
  
  ReDim ds(numDS)
  tstr = String(1024, Chr(0))
  tstr2 = String(1024, Chr(0))
  tstr3 = String(1024, Chr(0))
  cbolist = ""
  For i = 0 To numDS - 1
     task = "retc = epfGetDatasetInfo(i, ds(i).numpt, tstr, tstr2)"
     retc = epfGetSetInfo(i, ds(i).numPt, tstr, tstr2, tstr3)
     ds(i).locName = Left(tstr, InStr(tstr, Chr(0)) - 1)
     ds(i).locType = Left(tstr2, InStr(tstr2, Chr(0)) - 1)
     ds(i).locExp = Left(tstr3, InStr(tstr3, Chr(0)) - 1)
     
     task = "retc = epfGetExposurePoint(i , j, ds(i).x(j), ds(i).y(j))"
     ReDim ds(i).x(ds(i).numPt)
     ReDim ds(i).y(ds(i).numPt)
     For j = 0 To ds(i).numPt - 1
       retc = epfGetExposurePoint(i, j, ds(i).x(j), ds(i).y(j))
     Next
  Next
  
ErrorHandler:
  If Err.Number <> 0 Then
    PutError task
    numDS = 0
  End If
  epfLoadDatasets = numDS
End Function
