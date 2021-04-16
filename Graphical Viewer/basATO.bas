Attribute VB_Name = "basATO"
Option Explicit
Option Compare Text

Public Function addToList(aList() As String, name As String) As Boolean 'no duplicates
   Dim count As Long, i As Long

   On Error GoTo Error_check
   name = RTrim(name)
   name = LTrim(name)
   count = 0
   If Len(name) = 0 Then
    addToList = False
    Exit Function
   End If
   count = UBound(aList, 1)
   For i = 0 To count - 1
    If name = aList(i) Then
      addToList = False
      Exit Function
    End If
   Next
   ReDim Preserve aList(count + 1)
   aList(count) = name
   addToList = True
   Exit Function
Error_check:
   ReDim Preserve aList(count + 1)
   aList(count) = name
   addToList = True
End Function

Public Function atoSheetChange(col As Long, row As Long) As Boolean
  Dim retcode As Boolean
  
  retcode = False
  Select Case row
    Case CBO_DS:
      updateContams col
      atoUpdateAvailableFluxes col
      atoUpdateAvailableOutputs col
      atoUpdateAvailableMoist col
      atoUpdateAvailableCoordinates col
      retcode = atoGetSeries(col)
    Case CBO_CHM:
      atoUpdateAvailableFluxes col
      atoUpdateAvailableOutputs col
      atoUpdateAvailableMoist col
      atoUpdateAvailableCoordinates col
      retcode = atoGetSeries(col)
    Case CBO_FLX:
      atoUpdateAvailableOutputs col
      atoUpdateAvailableMoist col
      atoUpdateAvailableCoordinates col
      retcode = atoGetSeries(col)
    Case CBO_MSR:
      atoUpdateAvailableMoist col
      atoUpdateAvailableCoordinates col
      retcode = atoGetSeries(col)
    Case CBO_DEP:
      atoUpdateAvailableCoordinates col
      retcode = atoGetSeries(col)
    Case CBO_LOCX, CBO_LOCY:
      retcode = atoGetSeries(col)
  End Select
  atoSheetChange = retcode
End Function

Public Function atoGetSeries(col As Long) As Boolean
  Dim i As Long
  Dim dsIdx As Long
  Dim conIdx As Long
  Dim nvalues As Long
  Dim selFlx As String
  Dim selMsr As String
  Dim selDep As String
  Dim selxLoc As String
  Dim selyLoc As String
  
  atoGetSeries = False
  dsIdx = GetSelIdx(CBO_DS, col)
  conIdx = GetSelIdx(CBO_CHM, col)
  selFlx = GetSelText(CBO_FLX, col)
  selMsr = GetSelText(CBO_MSR, col)
  selDep = GetSelText(CBO_DEP, col)
  selxLoc = GetSelText(CBO_LOCX, col)
  selyLoc = GetSelText(CBO_LOCY, col)

  If selxLoc = "Not Available" Then Exit Function
  If selyLoc = "Not Available" Then Exit Function
  
  If selDep = "Not Available" Then selDep = ""
  nvalues = atoGetTimeSeries(dsIdx, conIdx, -1, selMsr, selFlx, selDep, selxLoc, selyLoc)
  ReDim yValues(nvalues)
  ReDim xValues(nvalues)
  If nvalues = 0 Then Exit Function
  atoGetTimeSeriesXAxisUnit dsIdx, conIdx, -1, xlabel
  atoGetTimeSeriesYAxisUnit dsIdx, conIdx, -1, ylabel
  For i = 0 To nvalues - 1
    xValues(i) = atoGetTimeSeriesTime(dsIdx, conIdx, -1, i)
    yValues(i) = atoGetTimeSeriesValue(dsIdx, conIdx, -1, i)
  Next
  atoGetSeries = True
End Function

Function atoLoadDatasets() As Long
Dim i As Long
  On Error GoTo ErrorHandler
  
  task = "numds = readatoDatasets(fuiname, ModName)"
  atoOpen FUIName, modName
  numDS = atoGetDatasetCount()
  
  ReDim ds(numDS)
  For i = 0 To numDS - 1
    task = "atoGetDatasetName(i, ds(i).locName)"
    atoGetDatasetName i, ds(i).locName
    task = "atoGetDatasetName(i, ds(i).locType)"
    atoGetReleaseType i, ds(i).locType
  Next
  
ErrorHandler:
  If Err.Number <> 0 Then
    PutError task
    numDS = 0
  End If
  atoLoadDatasets = numDS
  
End Function

Sub atoUpdateAvailableFluxes(col As Long)
  Dim i As Long
  Dim j As Long
  Dim k As Long
  Dim m As Long
  Dim num As Long
  Dim numOut As Long
  Dim fluxIndex As Long
  Dim flx As String
  Dim cbolist As String
  Dim available() As String
 
  If CBO_DS < 0 Then Exit Sub
  cbolist = ""
  ReDim available(0)
  i = GetSelIdx(CBO_DS, col)
  j = GetSelIdx(CBO_CHM, col)
  num = atoGetTimePeriodCount(i, j, -1)
  For k = 0 To num - 1
    numOut = atoGetTimePeriodOutputCount(i, j, -1, k)
    For m = 0 To numOut - 1
      fluxIndex = atoGetGridFluxIndex(i, j, -1, k, m)
      atoGetGridFluxType i, j, -1, k, m, fluxIndex, flx
      addToList available(), flx
    Next
  Next
  
  For j = 0 To UBound(available)
    cbolist = cbolist + IIf(cbolist <> "", Chr$(9), "") + available(j)
  Next
  SetSelList CBO_FLX, col, cbolist
End Sub

Sub atoUpdateAvailableOutputs(col As Long)
  Dim i As Long
  Dim j As Long
  Dim k As Long
  Dim m As Long
  Dim num As Long
  Dim numOut As Long
  Dim fluxIndex As Long
  Dim msr As String
  Dim flx As String
  Dim selFlx As String
  Dim cbolist As String
  Dim available() As String
 
  If CBO_DS < 0 Then Exit Sub
  cbolist = ""
  ReDim available(0)
  i = GetSelIdx(CBO_DS, col)
  j = GetSelIdx(CBO_CHM, col)
  selFlx = GetSelText(CBO_FLX, col)
  num = atoGetTimePeriodCount(i, j, -1)
  For k = 0 To num - 1
    numOut = atoGetTimePeriodOutputCount(i, j, -1, k)
    For m = 0 To numOut - 1
      fluxIndex = atoGetGridFluxIndex(i, j, -1, k, m)
      atoGetGridFluxType i, j, -1, k, m, fluxIndex, flx
      If (flx = selFlx) Then
        atoGetGridOutputType i, j, -1, k, m, msr
        addToList available(), msr
      End If
    Next
  Next
  
  For j = 0 To UBound(available)
    cbolist = cbolist + IIf(cbolist <> "", Chr$(9), "") + available(j)
  Next
  SetSelList CBO_MSR, col, cbolist
End Sub

Sub atoUpdateAvailableMoist(col As Long)
  Dim i As Long
  Dim j As Long
  Dim k As Long
  Dim m As Long
  Dim num As Long
  Dim numOut As Long
  Dim fluxIndex As Long
  Dim moist As String
  Dim msr As String
  Dim selMsr As String
  Dim flx As String
  Dim selFlx As String
  Dim cbolist As String
  Dim available() As String
 
  If CBO_DS < 0 Then Exit Sub
  ReDim available(0)
  cbolist = ""
  i = GetSelIdx(CBO_DS, col)
  j = GetSelIdx(CBO_CHM, col)
  selFlx = GetSelText(CBO_FLX, col)
  selMsr = GetSelText(CBO_MSR, col)
  num = atoGetTimePeriodCount(i, j, -1)
  For k = 0 To num - 1
    numOut = atoGetTimePeriodOutputCount(i, j, -1, k)
    For m = 0 To numOut - 1
      fluxIndex = atoGetGridFluxIndex(i, j, -1, k, m)
      atoGetGridFluxType i, j, -1, k, m, fluxIndex, flx
      atoGetGridOutputType i, j, -1, k, m, msr
      If (selFlx = flx And selMsr = msr) Then
        atoGetGridMoist i, j, -1, k, m, moist
        addToList available(), moist
      End If
    Next
  Next
  
  For j = 0 To UBound(available)
    cbolist = cbolist + IIf(cbolist <> "", Chr$(9), "") + available(j)
  Next
  SetSelList CBO_DEP, col, cbolist
End Sub

Sub atoUpdateAvailableCoordinates(col As Long)
  Dim i As Long
  Dim j As Long
  Dim k As Long
  Dim m As Long
  Dim n As Long
  Dim num As Long
  Dim numOut As Long
  Dim fluxIndex As Long
  Dim dep As String
  Dim selDep As String
  Dim msr As String
  Dim selMsr As String
  Dim flx As String
  Dim selFlx As String
  Dim selCrd As String
  Dim cbolist As String
  Dim cbolist2 As String
  Dim xCount As Long
  Dim yCount As Long
  Dim xvalue As Double
  Dim yvalue As Double

  If CBO_DS < 0 Then Exit Sub
  cbolist = ""
  cbolist2 = ""
  i = GetSelIdx(CBO_DS, col)
  j = GetSelIdx(CBO_CHM, col)
  selFlx = GetSelText(CBO_FLX, col)
  selMsr = GetSelText(CBO_MSR, col)
  selDep = GetSelText(CBO_DEP, col)
  
  atoGetCoordinateType i, selCrd
  If selCrd = "polar" Then
    activeSpread.SetText col - 1, CBO_LOCX, "Distance"
    activeSpread.SetText col - 1, CBO_LOCY, "Direction"
  ElseIf selCrd = "cartesian" Then
    activeSpread.SetText col - 1, CBO_LOCX, "X coordinate"
    activeSpread.SetText col - 1, CBO_LOCY, "Y coordinate"
  End If
  
  num = atoGetTimePeriodCount(i, j, -1)
  For k = 0 To num - 1
    numOut = atoGetTimePeriodOutputCount(i, j, -1, k)
    For m = 0 To numOut - 1
      fluxIndex = atoGetGridFluxIndex(i, j, -1, k, m)
      atoGetGridFluxType i, j, -1, k, m, fluxIndex, flx
      atoGetGridOutputType i, j, -1, k, m, msr
      atoGetGridMoist i, j, -1, k, m, dep
      If (selFlx = flx And selMsr = msr And (selDep = dep Or selDep = "Not Available")) Then
        xCount = atoGetGridAxis1Count(i, j, -1, k, m)
        For n = 0 To xCount - 1
          xvalue = atoGetGridAxis1Value(i, j, -1, k, m, n)
          cbolist = cbolist + IIf(cbolist <> "", Chr$(9), "") + CStr(xvalue)
        Next
        yCount = atoGetGridAxis2Count(i, j, -1, k, m)
        For n = 0 To yCount - 1
          yvalue = atoGetGridAxis2Value(i, j, -1, k, m, n)
          cbolist2 = cbolist2 + IIf(cbolist2 <> "", Chr$(9), "") + CStr(yvalue)
        Next
        SetSelList CBO_LOCX, col, cbolist
        SetSelList CBO_LOCY, col, cbolist2
        Exit Sub
      End If
    Next
  Next
  If numOut = 0 Then
    SetSelList CBO_LOCX, col, cbolist
    SetSelList CBO_LOCY, col, cbolist2
  End If
End Sub

