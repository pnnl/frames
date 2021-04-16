Attribute VB_Name = "basBBF"
Option Explicit
Option Compare Text

Function bbfChart(fname As String, source As String) As Boolean
  Dim line As String, Module As String
  Dim fnum As Long
  Dim found As Boolean
  Dim i As Integer, j As Integer, k As Integer
  Dim nMed As Integer, nm As Integer
  Dim nLoc As Integer, nl As Integer
  Dim ncon As Integer, nc As Integer
  Dim nRec As Integer, nr As Integer
  Dim c(11)
  Dim csv() As String
  Dim range As String
  Dim nprog As Integer
  Dim nSheet As Long
  Dim locName As String, locType As String
  Dim Title As String
  Dim medtype As String
  Dim Lines As Long, l As Long
  Dim atimes()
  Dim Row As Integer
  Dim sheetId As String
  
  Dim intDataSets As Integer
  Dim intCurDataSet As Integer
  Dim intNumSpecies As Integer
  Dim strMedium As String
  Dim intCurSpecies As Integer
  Dim strSpeciesName As String
  Dim intNumConstituents As Integer
  Dim intCurConstituent As Integer
  Dim strConstituentName As String
  Dim strConstituentID As String
  Dim strXAxis As String
  Dim strYAxis As String
  Dim strChartTitle As String
  Dim intNumMainPairs As Integer
  Dim intCurMainPair As Integer
  Dim intNumProgeny As Integer
  Dim intCurProgeny As Integer
  Dim intSheetLoc As Long
  Dim lngChartLoc As Long
  Dim strProgenyName As String
  Dim strProgenyID As String
  Dim intNumProgenyPairs As Integer
  Dim intCurProgenyPair As Integer
  Dim strParentName As String
  Dim strParentID As String
  Dim lngRowHeight As Long

  If Not GetWorkbookObject() Then End
  On Error GoTo bbfChart_Error
  
  fnum = FreeFile
  Open fname For Input As #fnum
  
  nSheet = 0
    
  ' set up a loop to search through the file to find the
  '     right module output
  While (Not EOF(fnum)) And (Not found)
    ReadCSVLine fnum, csv()
    Module = csv(1)
    Lines = csv(2)
    If LCase(Module) = LCase(source) Then
      ' the correct module has been found--process this section
      found = True
      ' read in a line
      ReadCSVLine fnum, csv()
      ' store the number of lines in the header
      nRec = csv(1)
      ' set up a loop to go through the header lines
      For i = 1 To nRec
        Line Input #fnum, line
      Next
      ' read in a line
      ReadCSVLine fnum, csv()
      ' store the number of data sets
      intDataSets = csv(1)
      ' set up a loop to process each of the data sets
      For intCurDataSet = 1 To intDataSets
        ' increment sheet count
        nSheet = nSheet + 1
        ' create a new sheet
        Set wso = GetWorkSheetObject(WorkSheetName("DataSet_" & CStr(intCurDataSet)), nSheet)
        ' get the row height in points
        lngRowHeight = wso.Rows(1).RowHeight
        ' initialize intSheetloc
        intSheetLoc = 1
        ' initialize the chart location
        lngChartLoc = 0
        ' read in the next line
        ReadCSVLine fnum, csv()
        ' store the location info
        strMedium = csv(2)
        intNumSpecies = csv(3)
        
        ' set up a loop to read in all the species
        For intCurSpecies = 1 To intNumSpecies
            ' read in a line
            ReadCSVLine fnum, csv()
            ' store the Species Name
            strSpeciesName = csv(1)
            ' store the number of chemical constituents
            intNumConstituents = csv(2)
            
            ' set up a loop to read in all the constituents
            For intCurConstituent = 1 To intNumConstituents
                ' read in a line
                ReadCSVLine fnum, csv()
                ' store the Constituent Name and ID
                strConstituentName = csv(1)
                strConstituentID = csv(2)
                ' store the x-axis and y-axis units
                strXAxis = csv(3)
                strYAxis = csv(4)
                ' store the number of data entries for the graph
                intNumMainPairs = csv(5)
                ' store the number of progeny
                intNumProgeny = csv(6)
                
                ' now set up a variant array to hold the information to
                '   be loaded into the spreadsheet
                ReDim varray(intNumMainPairs + 1, 1)
                ' load the header info into the array
                varray(0, 0) = "BBF"
                varray(0, 1) = fname
                varray(1, 0) = strXAxis
                varray(1, 1) = strYAxis
                
                ' set up the chart title
                strChartTitle = "Medium: " & strMedium & Chr(10) & _
                                "Species: " & strSpeciesName & Chr(10) & _
                                "Constituent: " & strConstituentName & " (" & strConstituentID & ")"

                ' set up a loop to read in the main data pairs
                For intCurMainPair = 1 To intNumMainPairs
                    ' read in a line from the file
                    ReadCSVLine fnum, csv()
                    ' load the values into the array
                    varray(intCurMainPair + 1, 0) = csv(1)
                    varray(intCurMainPair + 1, 1) = csv(2)
                Next
                        
                ' now we have all the data loaded into the array,
                '   time to load it into the spreadsheet
                range = excelRange(1, intSheetLoc, 1 + UBound(varray, 2), intSheetLoc + UBound(varray, 1))
                wso.range(range).Value = varray
                
                ' next we need to set up a chart on the Excel worksheet
                range = excelRange(1, intSheetLoc + 1, 1 + UBound(varray, 2), intSheetLoc + UBound(varray, 1))
                ExportToExcelLoc wso, range, strChartTitle, varray(1, 0), varray(1, 1), 200, lngChartLoc
                
                ' next we need to check to see where to assign the next chart / data set
                ' first check to see if the chart or data set will dominate
                If ((intNumMainPairs + 4) * lngRowHeight) > 275 Then
                    ' here the data is dominating--reset the sheetloc
                    intSheetLoc = intSheetLoc + intNumMainPairs + 4
                    ' next determine the placement of the chart
                    lngChartLoc = lngChartLoc + ((intNumMainPairs + 4) * lngRowHeight) - (lngRowHeight / 2)
                Else
                    ' here the chart is dominating--reset the sheetloc
                    intSheetLoc = intSheetLoc + 24
                    ' next determine the placement of the chart
                    lngChartLoc = lngChartLoc + (24 * lngRowHeight) - (lngRowHeight / 2)
                End If
                    
                ' next set up a loop to handle all progeny
                For intCurProgeny = 1 To intNumProgeny
                    ' read in a line from the file
                    ReadCSVLine fnum, csv()
                    ' store the Progeny Name and ID
                    strProgenyName = csv(1)
                    strProgenyID = csv(2)
                    ' store the x-axis and y-axis units
                    strXAxis = csv(3)
                    strYAxis = csv(4)
                    ' store the number of data entries for the graph
                    intNumProgenyPairs = csv(5)
                    ' store the number of Parent Name and ID
                    strParentName = csv(6)
                    strParentID = csv(7)
                    
                    ' now set up a variant array to hold the information to
                    '   be loaded into the spreadsheet
                    ReDim varray(intNumMainPairs + 1, 1)
                    ' load the header info into the array
                    varray(0, 0) = "BBF"
                    varray(0, 1) = fname
                    varray(1, 0) = strXAxis
                    varray(1, 1) = strYAxis
                    
                    ' set up the chart title
                    strChartTitle = "Medium: " & strMedium & Chr(10) & _
                                    "Species: " & strSpeciesName & Chr(10) & _
                                    "Constituent: " & strConstituentName & " (" & strConstituentID & ")" & Chr(10) & _
                                    "Progeny: " & strProgenyName & " (" & strProgenyID & ")" & Chr(10) & _
                                    "Parent: " & strParentName & " (" & strParentID & ")"
                                       
                    ' set up a loop to read in the main data pairs
                    For intCurProgenyPair = 1 To intNumProgenyPairs
                        ' read in a line from the file
                        ReadCSVLine fnum, csv()
                        ' load the values into the array
                        varray(intCurProgenyPair + 1, 0) = csv(1)
                        varray(intCurProgenyPair + 1, 1) = csv(2)
                    Next
                            
                    ' now we have all the data loaded into the array,
                    '   time to load it into the spreadsheet
                    range = excelRange(1, intSheetLoc, 1 + UBound(varray, 2), intSheetLoc + UBound(varray, 1))
                    wbo.Sheets(sheetId).range(range).Value = varray
                    
                    ' next we need to set up a chart on the Excel worksheet
                    range = excelRange(1, intSheetLoc + 1, 1 + UBound(varray, 2), intSheetLoc + UBound(varray, 1))
                    ExportToExcelLoc wso, range, strChartTitle, varray(1, 0), varray(1, 1), 200, lngChartLoc
                    
                    ' next we need to check to see where to assign the next chart / data set
                    ' first check to see if the chart or data set will dominate
                    If ((intNumProgenyPairs + 4) * lngRowHeight) > 275 Then
                        ' here the data is dominating--reset the sheetloc
                        intSheetLoc = intSheetLoc + intNumProgenyPairs + 4
                        ' next determine the placement of the chart
                        lngChartLoc = lngChartLoc + ((intNumProgenyPairs + 4) * lngRowHeight) - (lngRowHeight / 2)
                    Else
                        ' here the chart is dominating--reset the sheetloc
                        intSheetLoc = intSheetLoc + 24
                        ' next determine the placement of the chart
                        lngChartLoc = lngChartLoc + (24 * lngRowHeight) - (lngRowHeight / 2)
                    End If
                    
                Next intCurProgeny
            Next intCurConstituent
        Next intCurSpecies
      Next intCurDataSet
    Else
      For i = l To Lines
        Line Input #fnum, line
      Next
    End If
  Wend
  
bbfChart_Exit:
  Close #fnum
  If Not found Then
    PutError "There is data to report."
  End If
  bbfChart = found
  EndModule
  Exit Function
  
bbfChart_Error:
  If Err() Then
    If Not Err = 62 Then MsgBox Error()
  End If
  Resume bbfChart_Exit
End Function

