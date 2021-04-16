Attribute VB_Name = "basEXF"
Option Explicit
Option Compare Text

Function exfSheetChange(col As Long, row As Long) As Boolean
  Dim retcode As Boolean
  Select Case row
    Case CBO_DS
      updateOrganisms col
      updateContams col
      updateEffects col
      retcode = exfGetSeries(col)
    Case CBO_OR
      updateContams col
      updateEffects col
      retcode = exfGetSeries(col)
    Case CBO_CHM
      updateEffects col
      retcode = exfGetSeries(col)
    Case CBO_EFX
      retcode = exfGetSeries(col)
  End Select
  exfSheetChange = retcode
End Function

Function exfGetSeries(col As Long) As Boolean
  Dim dsIdx As Long
  Dim orgIdx As Long
  Dim efxIdx As Long
  Dim conIdx As Long
  Dim nvalues As Long
  Dim tmp As String

  exfGetSeries = False
  dsIdx = GetSelIdx(CBO_DS, col) + 1
  orgIdx = GetSelIdx(CBO_OR, col) + 1
  conIdx = GetSelIdx(CBO_CHM, col) + 1
  efxIdx = GetSelIdx(CBO_EFX, col) + 1
  
  nvalues = exfGetSeriesProperties(dsIdx, orgIdx, conIdx, efxIdx, tmp, ylabel, xlabel)
  ReDim xValues(nvalues)
  ReDim yValues(nvalues)
  If nvalues = 0 Then Exit Function
  nvalues = exfGetSeriesValues(dsIdx, orgIdx, conIdx, efxIdx, nvalues, xValues(0), yValues(0))
  exfGetSeries = True
End Function

Function exfLoadDatasets() As Long
Dim i As Long
Dim retc As Long

  On Error GoTo ErrorHandler
  
  task = "numds = readEXFDatasets(fuiname, ModName)"
  exfOpen FUIName, modName
  numDS = exfGetNumSets()

  ReDim ds(numDS)
  For i = 0 To numDS - 1
     task = "retc = exfGetDatasetInfo(i + 1, ds(i).locName, ds(i).locType)"
     retc = exfGetSetInfo(i + 1, ds(i).locName, ds(i).locType)
  Next
  
ErrorHandler:
  If Err.Number <> 0 Then
    PutError task
    numDS = 0
  End If
  exfLoadDatasets = numDS
End Function

Function exfChart(ByVal fName As String, ByVal Source$) As Boolean
' created:      4/25/2001
' author:       Kevin Dorow, Nila Reitz, Mitch Pelton
' description:  This function reads in an exf file output from
'               the WEAP model and renders the information in an
'               Excel spreadsheet.

Dim line As String
Dim Module As String
Dim numlines As Long
Dim fnum As Long
Dim found As Boolean
Dim i As Long
Dim k As Long
Dim nRec As Long
Dim csv() As String
Dim range As String
Dim range1 As String
Dim grouplocLines As Long
Dim intSheetCount As Long
Dim intSheetLoc As Long
Dim lngChartLoc As Long
Dim lngRowHeight As Double
Dim strExposure As String
Dim strScientificName As String
Dim strCommonName As String
Dim strChemName As String
Dim strCasID As String
Dim strSite As String
Dim strChartTitle As String
Dim strYAxis As String
Dim strXAxis As String
Dim numexposure As Long
Dim numlife As Long
Dim numcasids As Long
Dim numeffects As Long

  If Not GetWorkbookObject() Then End
  On Error GoTo exfChart_Error
  fnum = FreeFile
  Open fName For Input As #fnum
  intSheetCount = 0
  While (Not EOF(fnum)) And (Not found)
    ReadCSVLine fnum, csv()
    Module = csv(1)
    numlines = csv(2)
    If LCase(Module) = LCase(Source) Then
      found = True
      While Not EOF(fnum) And found
        ReadCSVLine fnum, csv()
        numexposure = csv(1)
        For k = 1 To numexposure
          Line Input #fnum, line
        Next
        ReadCSVLine fnum, csv()
        numexposure = csv(1)
        While numexposure > 0
          ReadCSVLine fnum, csv()
          strExposure = csv(1)
          strSite = csv(2)
          numlife = csv(3)
          Select Case strExposure
          Case "Aquatic Organism Effects"
            While numlife > 0
              ReadCSVLine fnum, csv
              strCommonName = csv(1)
              strScientificName = csv(2)
              numcasids = csv(3)
              Set wso = GetWorkSheetObject(WorkSheetName(strCommonName))
              lngRowHeight = wso.Rows(1).rowHeight
              lngChartLoc = 0
              intSheetLoc = 1
              grouplocLines = 1
              While numcasids > 0
                ReadCSVLine fnum, csv
                strChemName = csv(1)
                strCasID = csv(2)
                numeffects = csv(3)
                strChartTitle = "Species: " & strCommonName & ", " & strScientificName & Chr(10) & _
                                "Chemical: " & strChemName & Chr(10) & "CAS ID: " & strCasID
                ReDim varray(numeffects, 1)
                varray(0, 0) = "Effects"
                varray(0, 1) = fName
                For k = 1 To numeffects
                  ReadCSVLine fnum, csv()
                  varray(k, 0) = csv(1)
                  varray(k, 1) = csv(2)
                Next
                range = excelRange(1, intSheetLoc, 1 + UBound(varray, 2), intSheetLoc + UBound(varray, 1))
                wso.range(range).value = varray
                range = excelRange(1, intSheetLoc + 1, 1, intSheetLoc + UBound(varray, 1))
                range1 = excelRange(2, intSheetLoc + 1, 2, intSheetLoc + UBound(varray, 1))
                ExportToExcelPie wso, range, range1, strChartTitle, 200, lngChartLoc
                nRec = numeffects
                
                'calculate next chart position
                If ((nRec + 4) * lngRowHeight) > 275 Then
                  intSheetLoc = intSheetLoc + nRec + 4
                  lngChartLoc = lngChartLoc + ((nRec + 4) * lngRowHeight) - (lngRowHeight / 2)
                Else
                  intSheetLoc = intSheetLoc + 24
                  lngChartLoc = lngChartLoc + (24 * lngRowHeight) - (lngRowHeight / 2)
                End If
                ReadCSVLine fnum, csv()
                nRec = csv(1)
                strXAxis = csv(2)
                strYAxis = csv(3)
                ReDim varray(nRec + 1, 1)
                varray(0, 0) = "Effects"
                varray(0, 1) = fName
                varray(1, 0) = "Concentration in mg/L"          'strXAxis
                varray(1, 1) = strYAxis
                For k = 1 To nRec
                  ReadCSVLine fnum, csv()
                  varray(k + 1, 0) = csv(1) * 1000000          'convert from g/mL to mg/L
                  varray(k + 1, 1) = csv(2)
                Next
                range = excelRange(1, intSheetLoc, 1 + UBound(varray, 2), intSheetLoc + UBound(varray, 1))
                wso.range(range).value = varray
                range = excelRange(1, intSheetLoc + 1, 1 + UBound(varray, 2), intSheetLoc + UBound(varray, 1))
                ExportToExcelLoc wso, range, strChartTitle, varray(1, 0), varray(1, 1), 200, lngChartLoc
                
                'calculate next chart position
                If ((nRec + 4) * lngRowHeight) > 275 Then
                  intSheetLoc = intSheetLoc + nRec + 4 + (grouplocLines * 10)
                  lngChartLoc = lngChartLoc + ((nRec + 4 + (grouplocLines * 10)) * lngRowHeight) - (lngRowHeight / 2)
                Else
                  intSheetLoc = intSheetLoc + 24 + (grouplocLines * 10)
                  lngChartLoc = lngChartLoc + ((24 + (grouplocLines * 10)) * lngRowHeight) - (lngRowHeight / 2)
                End If
                numcasids = numcasids - 1
              Wend
              strCommonName = Replace(strCommonName, " ", "_")
              grouplocLines = grouplocLines + 1
              numlife = numlife - 1
            Wend
          Case Else
            GoTo exfChart_Exit
          End Select
          numexposure = numexposure - 1
        Wend
      Wend
    Else
      For i = 1 To numlines
        Line Input #fnum, line
      Next
    End If
  Wend

exfChart_Exit:
  Close #fnum
  If Not found Then
    PutError "There is no data to report or unknown file format."
  End If
  exfChart = found
  EndModule
  Exit Function

exfChart_Error:
  If Err() Then
    If Not Err = 62 Then MsgBox Error()
  End If
  Resume exfChart_Exit
  Resume Next
End Function

Function bbfSheetChange(col As Long, row As Long) As Boolean
  Dim retcode As Boolean
  Select Case row
    Case CBO_DS
      updateVULevels col
      updateOrganisms col
      updateContams col
      retcode = bbfGetSeries(col)
    Case CBO_OR
      updateContams col
      retcode = bbfGetSeries(col)
    Case CBO_CHM, CBO_VU
      retcode = bbfGetSeries(col)
  End Select
  bbfSheetChange = retcode
End Function

Function bbfGetSeries(col As Long) As Boolean
  Dim dsIdx As Long
  Dim orgIdx As Long
  Dim conIdx As Long
  Dim vuIdx As Long
  Dim nvalues As Long

  bbfGetSeries = False
  dsIdx = GetSelIdx(CBO_DS, col) + 1
  orgIdx = GetSelIdx(CBO_OR, col) + 1
  conIdx = GetSelIdx(CBO_CHM, col) + 1
  vuIdx = GetSelIdx(CBO_VU, col) + 1
  
  nvalues = bbfGetSeriesProperties(dsIdx, orgIdx, conIdx, 0, ylabel, xlabel)
  ReDim xValues(nvalues)
  ReDim yValues(nvalues)
  If nvalues = 0 Then Exit Function
  nvalues = bbfGetSeriesValues(dsIdx, orgIdx, conIdx, 0, vuIdx, nvalues, xValues(0), yValues(0))
  
  bbfGetSeries = True
End Function

Function bbfLoadDatasets() As Long
Dim i As Long
Dim retc As Long

  On Error GoTo ErrorHandler
  
  task = "numds = readbbfDatasets(fuiname, ModName)"
  bbfOpen FUIName, modName
  numDS = bbfGetNumSets()
  
  ReDim ds(numDS)
  For i = 0 To numDS - 1
     task = "retc = bbfGetDatasetInfo(i + 1, ds(i).locName, ds(i).locType)"
     retc = bbfGetSetInfo(i + 1, ds(i).locName, ds(i).locType)
  Next
  
ErrorHandler:
  If Err.Number <> 0 Then
    PutError task
    numDS = 0
  End If
  bbfLoadDatasets = numDS
End Function

