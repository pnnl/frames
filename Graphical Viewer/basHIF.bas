Attribute VB_Name = "basHIF"
Option Explicit
Option Compare Text

Function hifSheetChange(col As Long, row As Long) As Boolean
  Dim retcode As Boolean
  
  retcode = False
  Select Case row
    Case CBO_DS
      updateLocations col
      updateAgeGroup col
      updateConstituents col
      GetRoutesAndPathways col
      updateMeasures col
      updateUnits col
      updateOrgans col
      updateRoutes col
      updatePaths col
      retcode = hifGetSeries(col)
    Case CBO_CHM, CBO_AGE
      GetRoutesAndPathways col
      updateMeasures col
      updateUnits col
      updateOrgans col
      updateRoutes col
      updatePaths col
      retcode = hifGetSeries(col)
    Case CBO_MSR
      If col <> lastCol Then GetRoutesAndPathways col
      updateUnits col
      updateOrgans col
      updateRoutes col
      updatePaths col
      retcode = hifGetSeries(col)
    Case CBO_UNT
      If col <> lastCol Then GetRoutesAndPathways col
      updateOrgans col
      updateRoutes col
      updatePaths col
      retcode = hifGetSeries(col)
    Case CBO_RTE
      If col <> lastCol Then GetRoutesAndPathways col
      updatePaths col
      retcode = hifGetSeries(col)
    Case CBO_PTH
      retcode = hifGetSeries(col)
    Case CBO_XY, CBO_ORG
      retcode = hifGetSeries(col)
  End Select
  lastCol = col
  hifSheetChange = retcode
End Function

Function hifGetSeries(col As Long) As Boolean
  Dim i As Long
  Dim h As Long
  Dim m As Long
  Dim ktype As Long
  Dim dsIdx As Long
  Dim nvalues As Long
  Dim ageIdx As Long
  Dim xyIdx As Long
  Dim casid As String
  Dim rte As String
  Dim Path As String
  Dim measure As String
  Dim Unit As String
  Dim org As String
  Dim orgIdx As Long
  Dim xvalue As Double
  Dim yvalue As Double
  Dim o As Variant
  Dim firsttime As Boolean
  
  hifGetSeries = False
  ReDim xValues(0)
  ReDim yValues(0)
  dsIdx = GetSelIdx(CBO_DS, col)
  xyIdx = GetSelIdx(CBO_XY, col)
  ageIdx = GetSelIdx(CBO_AGE, col)
  measure = GetSelText(CBO_MSR, col)
  Unit = GetSelText(CBO_UNT, col)
  rte = GetSelText(CBO_RTE, col)
  Path = GetSelText(CBO_PTH, col)
  org = GetSelText(CBO_ORG, col)
  orgIdx = GetSelIdx(CBO_ORG, col) - 1
  casid = GetSelText(CBO_CHM, col)
  If measure = NOT_AVAILABLE Or _
     Unit = NOT_AVAILABLE Or _
     rte = NOT_AVAILABLE Or _
     Path = NOT_AVAILABLE Then Exit Function
  
  o = Split(GetSelList(CBO_ORG, col), Chr(9))
  If org <> ALLITEMS Then
    o(1) = org
    ReDim Preserve o(1)
  Else
    ReDim Preserve o(UBound(o) - 1)
  End If
   
  firsttime = True
  If casid = ALLCHEMS Then ktype = 0
  If casid = ALLRADS Then ktype = 1

  Select Case casid
  Case ALLCHEMS, ALLRADS
    For i = 1 To UBound(allCAS)
      If allCAS(i).ktype = ktype Then
        For h = 1 To UBound(o)
          If org = ALLITEMS Then orgIdx = h - 1
          nvalues = hifLoadTimeSeries(dsIdx, xyIdx, ageIdx, orgIdx, allCAS(i).cas, allCAS(i).pcas(0), Path, rte, measure, Unit)
          If UBound(xValues) = 0 Then ReDim xValues(nvalues)
          If UBound(yValues) = 0 Then ReDim yValues(nvalues)
          For m = 0 To nvalues - 1
            hifGetTimeAndValue dsIdx, m, xvalue, yvalue
            If firsttime Then
              xValues(m) = xvalue
              yValues(m) = yValues(m) + yvalue
            Else
              If xValues(m) = xvalue Then
                xValues(m) = xvalue
                yValues(m) = yValues(m) + yvalue
              Else
                ReDim xValues(0)
                ReDim yValues(0)
                MsgBox "Some summary tables will be unavailable beacuse the constituent time points do not match, and cannot add series!"
                Exit Function
              End If
            End If
          Next
          firsttime = False
        Next
      End If
    Next
  Case Else
    i = GetAllContIdx(casid)
    If i > -1 Then
      For h = 1 To UBound(o)
        If org = ALLITEMS Then orgIdx = h - 1
        nvalues = hifLoadTimeSeries(dsIdx, xyIdx, ageIdx, orgIdx, allCAS(i).cas, allCAS(i).pcas(0), Path, rte, measure, Unit)
        If UBound(xValues) = 0 Then ReDim xValues(nvalues)
        If UBound(yValues) = 0 Then ReDim yValues(nvalues)
        For m = 0 To nvalues - 1
          hifGetTimeAndValue dsIdx, m, xvalue, yvalue
          If firsttime Then
            xValues(m) = xvalue
            yValues(m) = yValues(m) + yvalue
          Else
            If xValues(m) = xvalue Then
              xValues(m) = xvalue
              yValues(m) = yValues(m) + yvalue
            Else
              ReDim xValues(0)
              ReDim yValues(0)
              MsgBox "Some summary tables will be unavailable beacuse the constituent time points do not match, and cannot add series!"
              Exit Function
            End If
          End If
        Next
        firsttime = False
      Next
    End If
  End Select
  xlabel = "yr"
  ylabel = Unit
  
  hifGetSeries = True
End Function

Public Function hifLoadDatasets() As Long
Dim i As Long
Dim j As Long
Dim retc As Long
Dim tstr As String
Dim tstr2 As String
Dim tstr3 As String

  numDS = 0
  ReDim ds(numDS)
  numDS = hifOpen(FUIName, modName)
  If 0 = hifChecksum() Then
    numDS = 0
    PutError "The HIF file is incomplete or incorrectly formatted."
    hifLoadDatasets = numDS
    Exit Function
  End If
  
  tstr = String(1024, Chr(0))
  tstr2 = String(1024, Chr(0))
  tstr3 = String(1024, Chr(0))
  ReDim ds(numDS)
  For i = 0 To numDS - 1
    
     task = "retc = hifGetDatasetCounts(i, ds(i).numPt, ds(i).numAge, ds(i).ncOrgans, ds(i).ndOrgans, tstr, tstr2, tstr3)"
     retc = hifGetSetInfo(i, ds(i).numPt, ds(i).numAge, ds(i).ncOrgans, ds(i).ndOrgans, tstr, tstr2, tstr3)
     ds(i).locName = Left(tstr, InStr(tstr, Chr(0)) - 1)
     ds(i).locType = Left(tstr2, InStr(tstr2, Chr(0)) - 1)
     ds(i).locExp = Left(tstr3, InStr(tstr3, Chr(0)) - 1)
     
     task = "retc = hifGetExposurePoint(i, j, ds(i).x(j), ds(i).y(j))"
     ReDim ds(i).x(ds(i).numPt)
     ReDim ds(i).y(ds(i).numPt)
     For j = 0 To ds(i).numPt - 1
       retc = hifGetExposurePoint(i, j, ds(i).x(j), ds(i).y(j))
     Next
   
     task = "retc = hifGetAgeGroup(i, j, ds(i).agemin(j), ds(i).agemax(j))"
     ReDim ds(i).ageMin(ds(i).numAge)
     ReDim ds(i).ageMax(ds(i).numAge)
     For j = 0 To ds(i).numAge - 1
       retc = hifGetAgeGroup(i, j, ds(i).ageMin(j), ds(i).ageMax(j))
     Next
     
     task = "hifGetCancerOrgan i, j, tstr"
     ReDim ds(i).cOrgans(ds(i).ncOrgans)
     For j = 0 To ds(i).ncOrgans - 1
       tstr = String(1024, Chr(0))
       hifGetCancerOrgan i, j, tstr
       ds(i).cOrgans(j) = Left(tstr, InStr(tstr, Chr(0)) - 1)
     Next
     
     task = "hifGetDoseOrgan i, j, tstr"
     ReDim ds(i).dOrgans(ds(i).ndOrgans)
     For j = 0 To ds(i).ndOrgans - 1
       tstr = String(1024, Chr(0))
       hifGetDoseOrgan i, j, tstr
       ds(i).dOrgans(j) = Left(tstr, InStr(tstr, Chr(0)) - 1)
     Next
  Next
  
ErrorHandler:
  If Err.Number <> 0 Then
    PutError task
    numDS = 0
  End If
  task = ""
  hifLoadDatasets = numDS
End Function


