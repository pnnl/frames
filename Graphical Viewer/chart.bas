Attribute VB_Name = "chartMethods"
Option Explicit
Option Compare Text


Public Declare Function LoadLibrary Lib "kernel32" Alias "LoadLibraryA" (ByVal lpLibFileName As String) As Long
Public Declare Function FreeLibrary Lib "kernel32" (ByVal hLibModule As Long) As Long
Public Declare Function GetModuleFileName Lib "kernel32" Alias "GetModuleFileNameA" (ByVal hModule As Long, ByVal lpFileName As String, ByVal nSize As Long) As Long
Public Declare Function GetSystemMetrics Lib "user32" (ByVal nIndex As Long) As Long
   
 
Public Const xlArea = 1
Public Const xlBar = 2
Public Const xlColumn = 3
Public Const xlLine = 4
Public Const xlPie = 5
Public Const xlRadar = -4151
Public Const xlXYScatter = 72 ' scattersmooth-4169
Public Const xlCombination = -4111
Public Const xl3DArea = -4098
Public Const xl3DBar = -4099
Public Const xl3DColumn = -4100
Public Const xl3DLine = -4101
Public Const xl3DPie = -4102
Public Const xl3DSurface = -4103
Public Const xlDoughnut = -4120
Public Const xlXYScatterSmooth = 72
Public Const xlXYScatterLines = 74
Public Const xlCategory = 1
Public Const xlPrimary = 2
Public Const xlValue = 2
Public Const xlLocationAsObject = 2
Public Const xlDataLabelsShowNone = -4142
Public Const xlDataLabelsShowPercent = 3
Public Const xlRows = 1
Public Const xlColumns = 2
Public Const xlThin = 2
Public Const xlNone = -4142

'Excel chart structure
Type ChartWizardStruct
  cwSource As String
  cwGallery As Long
  cwFormat As Long
  cwPlotBy As Long
  cwCategoryLabels As Long
  cwSeriesLabels As Long
  cwHasLegend As Long
  cwTitle As String
  cwCategoryTitle As String
  cwValueTitle As String
  cwExtraTitle As String
End Type
   
Type CASIDList
  cas As String
  name As String
  nprog As Long
  pcas() As String
  pName() As String
  ktype As Long
End Type

Public ShowMsg As String
Public viewType As String        ' "VWR" or "PE"
Public modSrc() As String        ' PDCF producing module sources
Public modSrcId() As String      ' PDCF producing module sources Id
Public modSrcLbl() As String     ' PDCF producing module sources Label
Public modIds() As String        ' module Ids
Public modLbls() As String       ' module Labels
Public modLabel As String        ' PDCF producing module label
Public modDesName As String      ' PDCF producing module description file path
Public pdcfName As String        ' PDCF file name
Public pdcfType As String        ' PDCF file type
Public sufName As String         ' Sensitivity Module Icon
Public XMLUrl As String          ' For use with SUFWCF/SUFSCF chart Benchmarks

Public task As String

Public cho As Object             ' Excel.ChartObject
Public wso As Object             ' Excel.Application
Public wbo As Object             ' Excel.Application
Public sheetId As String
Public activeForm As Form
Public activeSpread As fpSpread

Public numCAS As Long
Public cas() As CASIDList

Function WorkSheetName(name As Variant) As String
  Dim i As Long
  Dim ct As Long
  Dim tmp As String
  Dim tmp1 As String
  
  tmp1 = Left(CStr(name), 28)
  For i = 1 To wbo.sheets.count
    If InStr(wbo.sheets(i).name, tmp1) Then ct = ct + 1
  Next i
  If ct > 0 Then
    tmp = "-" & CStr(ct + 1)
    tmp1 = tmp1 & tmp
  End If
  WorkSheetName = Replace(tmp1, "/", "-")
End Function

Sub xlsSetCell(obj As Object, col As Long, row As Long, value)
  obj.range(wbo.Cells(row, col), wbo.Cells(row, col)).value = value
End Sub

Function xlsGetCell(obj As Object, col As Long, row As Long) As Variant
  xlsGetCell = obj.range(wbo.Cells(row, col), wbo.Cells(row, col)).value
End Function

Function xlsSetRange(obj As Object, col As Long, row As Long, value(), Optional fmt$ = "") As String
Dim range As String
  range = excelRange(col, row, col + UBound(value, 2) - 1, row + UBound(value, 1) - 1)
  obj.range(range).value = value
  If fmt <> "" Then obj.range(range).NumberFormat = fmt
  xlsSetRange = range
End Function

Function excelRange(col1&, Optional row1& = -1, Optional col2& = -1, Optional row2& = -1) As String
Dim s As String

  s = ""
  If col1 > 26 Then s = Chr(Asc("A") + Int((col1 - 1) / 26) - 1)
  s = s & Chr(Asc("A") + ((col1 - 1) Mod 26))
  If row1 > 0 Then s = s & row1
  If col2 > 0 Then s = s & ":" & excelRange(col2, row2)
  excelRange = s
End Function

Public Function AddSeriesToChart(wso, cho, name As String, xrange As String, yrange As String, Optional ag = 1, Optional Smooth = False) As Object
Dim ns As Object
  
  Set ns = cho.chart.SeriesCollection.NewSeries()
  ns.values = wso.range(yrange)
  ns.xValues = wso.range(xrange)
  ns.name = name
  ns.AxisGroup = ag
  ns.Smooth = Smooth
  Set AddSeriesToChart = ns
End Function

Function GetChartObject(wso As Object) As Object
Dim cho As Object
  Set cho = Nothing
  Set cho = wso.ChartObjects.Add(200, 15, 450, 300)
  Set GetChartObject = cho
  cho.chart.ChartType = xlXYScatterLines
End Function

Function GetWorkSheetObject(name As String) As Object
Dim obj As Object
  
  If wbo.sheets(1).name = "sheet1" Then
    Set obj = wbo.sheets(1)
  Else
    Set obj = wbo.sheets.Add(after:=wbo.sheets(wbo.sheets.count))
  End If
  obj.Activate
  obj.name = name
  Set GetWorkSheetObject = obj
End Function

Function GetWorkbookObject() As Boolean
  Dim sht
  On Error Resume Next
  Set wbo = GetObject(, "excel.application")
  If Err <> 0 Then
    On Error GoTo getWorkbookObject_Error
    Set wbo = CreateObject("excel.application")
  End If
  wbo.Workbooks.Add
  wbo.DisplayAlerts = False
  
'  MsgBox "GetWorkbook: " & wbo.sheets.count
  On Error Resume Next
  If wbo.sheets.count <= 3 Then wbo.sheets("sheet3").Delete
  If wbo.sheets.count <= 2 Then wbo.sheets("sheet2").Delete
  On Error GoTo getWorkbookObject_Error
  
  wbo.DisplayAlerts = True
  wbo.Visible = True
  GetWorkbookObject = True
  Exit Function
  
getWorkbookObject_Error:
  MsgBox "GetWorkbook:" & Error()
  GetWorkbookObject = False
End Function

Sub ExportToExcelPie(wso As Object, range As String, range1 As String, Title As String, Left As Long, Top As Long)
' created:      4/25/2001
' author:       Kevin Dorow
' purpose:      This routine places an Excel chart in a specified location on the
'               worksheet.

  On Error GoTo ExportToExcelPie_Error
  
  Set cho = wso.ChartObjects.Add(Left, Top, 450, 275)
  With cho.chart
    .ChartType = xlPie
    .setSourceData Source:=wso.range(range), PlotBy _
        :=xlColumns
    .SeriesCollection(1).xValues = wso.range(range1)
    .HasTitle = True
    .charttitle.Font.size = 10
    .charttitle.characters.Text = Title
    .ApplyDataLabels Type:=xlDataLabelsShowPercent, LegendKey:=False _
                        , HasLeaderLines:=True
    .PlotArea.Border.Weight = xlThin
    .PlotArea.Border.LineStyle = xlNone
    .PlotArea.interior.colorindex = xlNone
  End With
  
Exit Sub

ExportToExcelPie_Exit:
  Exit Sub
ExportToExcelPie_Error:
  If Err <> 1004 Then
    ' 91, 424
    MsgBox Err & " " & Error
    Resume ExportToExcelPie_Exit
  Else
    Resume Next
  End If
End Sub

Sub ExportToExcelLoc(wso As Object, ByVal range As String, ByVal Title As String, ByVal Xaxis As String, ByVal Yaxis As String, ByVal Left As Long, ByVal Top As Long)
Dim ht As Long
  
  ht = wso.Rows(1).rowHeight * 22
  Set cho = wso.ChartObjects.Add(Left, Top, 450, ht)
  With cho.chart
    .ChartType = xlXYScatterSmooth
    .setSourceData Source:=wso.range(range), PlotBy:=xlColumns
  End With
  CompleteChartObject cho, Title, Xaxis, Yaxis, ""
End Sub

Sub CompleteChartObject(cho As Object, Title As String, Xaxis As String, Yaxis As String, Optional secaxis As String = "")
Dim i As Long
   
   On Error GoTo LabelChartObject_Error
   With cho.chart
    If .Axes.count >= 1 Then
      .Axes(1).HasTitle = True
      .Axes(1).AxisTitle.characters.Text = Xaxis
      .Axes(1).MinimumScaleIsAuto = True
    End If
    
    If .Axes.count >= 2 Then
      .Axes(2).HasTitle = True
      .Axes(2).AxisTitle.characters.Text = Yaxis
      If InStr(1, Yaxis, "%", vbTextCompare) > 0 Then
        .Axes(2).MaximumScaleIsAuto = False
        .Axes(2).MaximumScale = 100
        .Axes(2).TickLabels.NumberFormat = "0"
      Else
        .Axes(2).MaximumScaleIsAuto = True
        .Axes(2).TickLabels.NumberFormat = "0.00E+00"
      End If
      .Axes(2).MinimumScaleIsAuto = True
      .Axes(2).MinorUnitIsAuto = True
      .Axes(2).MajorUnitIsAuto = True
      .Axes(2).ReversePlotOrder = False
    End If
    
    If .HasAxis(2, 2) Then
      .Axes(2, 2).TickLabels.NumberFormat = "0.00E+00"
      If secaxis <> "" Then
        .Axes(2, 2).HasTitle = True
        .Axes(2, 2).AxisTitle.characters.Text = secaxis
        .Axes(2, 2).MinimumScale = 0#
      End If
    End If
    
    For i = 1 To .SeriesCollection.count
      If (i = 1 And .SeriesCollection.count > 1) Then
        .SeriesCollection(i).AxisGroup = 1
      End If
      .SeriesCollection(i).Smooth = False
    Next
   
   .HasTitle = True
   .charttitle.characters.Text = Left(Title, 255)
   .charttitle.Font.size = 10
   .charttitle.Font.Bold = True
  End With
   
LabelChartObject_Exit:
  Exit Sub
  
LabelChartObject_Error:
  If Err <> 1004 Then
    MsgBox Err & " " & Error
    Resume LabelChartObject_Exit
  Else
    Resume Next
  End If
End Sub

Sub ReadCSVLine(fnum As Long, csv() As String)
Dim i As Long
Dim fle As csv

On Error GoTo ReadCSVLine_error

  i = 0
  fle.fnum = fnum
  fle.separator = ","
  get_line fle
  While Not EOL(fle)
    i = i + 1
    ReDim Preserve csv(i)
    csv(i) = get_val(fle)
  Wend
  Exit Sub
  
ReadCSVLine_error:
  csv = Split("", ",")
End Sub

Function DoUnitConversion(unitLabel As String, uval As Double, newUnitLabel As String) As Double
  If unitLabel = "g/mL" Then
    'converts g/mL to mg/L
    DoUnitConversion = 1000000 * uval
    newUnitLabel = "mg/L"
  ElseIf unitLabel = "g/mg" Then
    'converts g/mg to mg/kg
    DoUnitConversion = 0.001 * uval
    newUnitLabel = "mg/kg"
  ElseIf unitLabel = "ug/L" Then
    'converts ug/L to mg/L
    DoUnitConversion = 0.001 * uval
    newUnitLabel = "mg/L"
  Else
    DoUnitConversion = uval
    newUnitLabel = unitLabel
  End If
End Function

Function sufChart2(fName As String, Source As String) As Boolean
  Dim csv() As String
  Dim pos As Long
  Dim fpos As Long
  Dim fnum As Long
  Dim found As Boolean
  Dim line As String
  Dim Module As String
  Dim j As Long
  Dim k As Long
  Dim nLines As Long, nl As Long
  Dim nRec As Long, nr As Long
  Dim nvar As Long, nv As Long
  Dim label() As String
  Dim tstr As String
  Dim range As String
  Dim sname As String
  Dim varid() As String
  Dim vdesc() As String
  Dim vdcnt() As Long
  Dim rval() As Double
  Dim ridx() As Long
  Dim ncol As Long
  Dim rmin As Double
  Dim rmax As Double
  Dim sum As Double
  Dim dual As Boolean
  Dim ftime As Boolean
  
  If Not GetWorkbookObject() Then End
  On Error GoTo sufChart2_Error
  fnum = FreeFile
  Open fName For Input As #fnum
  While (Not EOF(fnum)) And (Not found)
    ReadCSVLine fnum, csv()
    Module = csv(1)
    nLines = csv(2)
    If LCase(Module) = LCase(Source) Then
      found = True
      ReadCSVLine fnum, csv()
      nRec = csv(1)
      For nr = 1 To nRec
        Line Input #fnum, line  ' skip header lines
      Next
      ReadCSVLine fnum, csv() ' Line Input #fnum, line
      nRec = csv(1)
      nRec = nRec + csv(2)
      ReDim varid(nRec)
      ReDim vdesc(nRec)
      ReDim vdcnt(nRec)
      For nr = 1 To nRec
        ReadCSVLine fnum, csv()
        varid(nr) = csv(1)
        vdesc(nr) = csv(2)
        If UBound(csv) > 2 Then
          If 0 < InStr(vdesc(nr), "peak for") Then
            vdcnt(nr) = 1
          Else
            vdcnt(nr) = csv(3)
          End If
        Else
          vdcnt(nr) = 1
        End If
      Next
      ReadCSVLine fnum, csv()  'read number of col headings
      nRec = csv(1)
      ReadCSVLine fnum, csv()  'read col headings
      nvar = UBound(csv)
      ReDim label(nvar)
      For nv = 1 To nvar
        label(nv) = csv(nv)    'store headings
      Next
      fpos = Seek(fnum)        'save file position
      ReDim rval(nRec)
      ReDim ridx(nRec)
      ReDim rpk(nRec)
      
      ncol = 2
      nv = 2
      ftime = True
      Do While ncol <= nvar
        dual = 0 < InStr(label(ncol), "peak time")
        If dual Then
          ReDim varray(nRec + 4, 4)
        Else
          ReDim varray(nRec + 4, 3)
        End If

        Seek #fnum, fpos       'restore file position
        For nr = 1 To nRec     'read records
          ReadCSVLine fnum, csv()
          If dual Then
            rpk(nr) = val(csv(ncol))
            rval(nr) = val(csv(ncol + 1))
          Else
            rval(nr) = val(csv(ncol))
          End If
          ridx(nr) = nr
        Next
        sort nRec, ridx, rval, rmin, rmax, sum
        
        For j = 1 To nRec
          k = j + 4
          varray(k, 0) = ridx(j)
          varray(k, 1) = j
          If dual Then
            varray(k, 2) = rpk(ridx(j))
            varray(k, 3) = rval(ridx(j))
            varray(k, 4) = j / (nRec + 1)
          Else
            varray(k, 2) = rval(ridx(j))
            varray(k, 3) = j / (nRec + 1)
          End If
        Next j
          
        varray(0, 0) = "File:":          varray(0, 1) = fName
        varray(1, 0) = "Module:":        varray(1, 1) = modLabel
        varray(2, 0) = "Description:":
        varray(4, 0) = "Realization"
        
        pos = InStr(label(ncol), "at time")
        If pos > 0 Then
          tstr = Right(label(ncol), Len(label(ncol)) - (pos + 6))
          tstr = Replace(vdesc(nv - 1), "(s) #(,#...)", tstr)
        Else
          tstr = vdesc(nv - 1)
        End If
        varray(2, 1) = tstr
        
        If dual Then
          varray(4, 1) = "Rank Order"
          varray(4, 2) = label(ncol)
          varray(4, 3) = label(ncol + 1)
          varray(4, 4) = "Cum. Prob."
        Else
          varray(4, 1) = "Rank Order"
          varray(4, 2) = label(ncol)
          varray(4, 3) = "Cum. Prob."
        End If
          
        j = InStr(1, label(ncol), " ")
        If j > 0 Then
          sname = Left(label(ncol), j - 1)
        Else
          sname = label(ncol)
        End If
        Set wso = GetWorkSheetObject(WorkSheetName(sname))
        range = excelRange(1, 1, 1 + UBound(varray, 2), 1 + UBound(varray, 1))
        wso.range(range).value = varray
        If dual Then
          range = excelRange(4, 5, 5, 1 + UBound(varray, 1))
        Else
          range = excelRange(3, 5, 4, 1 + UBound(varray, 1))
        End If
        
        Set cho = wso.ChartObjects.Add(300, 80, 500, 300)
        With cho.chart
          .ChartType = xlXYScatterLines
          .setSourceData Source:=wso.range(range), PlotBy:=xlColumns
          .HasLegend = False
'          .Axes(2).MaximumScale = 1
'          .Axes(2).MinimumScale = 0
          .Axes(2).HasTitle = True
          .Axes(2).AxisTitle.characters.Text = "Cumulative Probability"
          .HasTitle = True
          .charttitle.characters.Text = tstr
        End With
        nv = nv + 1

        If Not dual And vdcnt(nv - 2) > 1 Then         'need to adjust nv index
          nv = nv - 1
          vdcnt(nv - 1) = vdcnt(nv - 1) - 1
        End If
        
        ncol = ncol + 1
        If dual Then ncol = ncol + 1
        
      Loop
    Else
      For nl = 1 To nLines
        Line Input #fnum, line
      Next
    End If
  Wend
  
sufChart2_Exit:
  Close #fnum
  If Not found Then
    PutError "There is no data to report."
  End If
  sufChart2 = found
  EndModule
  Exit Function
  
sufChart2_Error:
  If Err() Then
    If Not Err = 62 Then MsgBox "sufChart2: " & Err & " " & Error$
  End If
  Resume sufChart2_Exit
End Function

Function sufChart(fName As String, Source As String) As Boolean
  Dim csv() As String
  Dim pos As Long
  Dim fpos As Long
  Dim fnum As Long
  Dim found As Boolean
  Dim line As String
  Dim Module As String
  Dim j As Long
  Dim k As Long
  Dim nLines As Long, nl As Long
  Dim nRec As Long, nr As Long
  Dim nvar As Long, nv As Long
  Dim label() As String
  Dim tstr As String
  Dim range As String
  Dim sname As String
  Dim varid() As String
  Dim vdesc() As String
  Dim vdcnt() As Long
  Dim rval() As Double
  Dim ridx() As Long
  Dim ncol As Long
  Dim rmin As Double
  Dim rmax As Double
  Dim sum As Double
  Dim dual As Boolean
  
  If Not GetWorkbookObject() Then End
  On Error GoTo sufChart_Error
  fnum = FreeFile
  Open fName For Input As #fnum
  While (Not EOF(fnum)) And (Not found)
    ReadCSVLine fnum, csv()
    Module = csv(1)
    nLines = val(csv(2))
    If LCase(Module) = LCase(Source) Then
      found = True
      ReadCSVLine fnum, csv()
      nRec = val(csv(1))
      For nr = 1 To nRec
        Line Input #fnum, line  ' skip header lines
      Next
      ReadCSVLine fnum, csv()
      nRec = val(csv(1))
      nRec = nRec + val(csv(2))
      ReDim varid(nRec)
      ReDim vdesc(nRec)
      ReDim vdcnt(nRec)
      For nr = 1 To nRec
        ReadCSVLine fnum, csv()
        varid(nr) = csv(1)
        vdesc(nr) = csv(2)
        If UBound(csv) > 2 Then
          If 0 < InStr(vdesc(nr), "peak for") Then
            vdcnt(nr) = 1
          Else
            vdcnt(nr) = csv(3)
          End If
        Else
          vdcnt(nr) = 1
        End If
      Next
      'read number of records
      ReadCSVLine fnum, csv()
      nRec = csv(1)
      ReadCSVLine fnum, csv()
      nvar = UBound(csv)
      ReDim label(nvar)
      For nv = 1 To nvar
        label(nv) = csv(nv)
      Next
      fpos = Seek(fnum)
      ReDim rval(nRec)
      ReDim ridx(nRec)
      ReDim rpk(nRec)
      
      ncol = 2
      nv = 2
      Do While ncol <= nvar
        
        dual = 0 < InStr(label(ncol), "peak time")
        If dual Then
          ReDim varray(nRec + 4, 4)
        Else
          ReDim varray(nRec + 4, 3)
        End If

        Seek #fnum, fpos
        For nr = 1 To nRec
          ReadCSVLine fnum, csv()
          If dual Then
            rpk(nr) = val(csv(ncol))
            rval(nr) = val(csv(ncol + 1))
          Else
            rval(nr) = val(csv(ncol))
          End If
          ridx(nr) = nr
        Next
        sort nRec, ridx, rval, rmin, rmax, sum
        
        For j = 1 To nRec
          k = j + 4
          varray(k, 0) = ridx(j)
          If dual Then
            varray(k, 1) = rpk(ridx(j))
            varray(k, 2) = rval(ridx(j))
'         Weibull Method
            varray(k, 4) = 1 + (nRec - j)            ' reverse rank
            varray(k, 3) = varray(k, 4) / (nRec + 1) ' probability
'         California Method
'           varray(k, 4) = j
'           varray(k, 3) = j / nrec
          Else
            varray(k, 1) = rval(ridx(j))
'         Weibull Method
            varray(k, 3) = 1 + (nRec - j)            ' reverse rank
            varray(k, 2) = varray(k, 3) / (nRec + 1) ' probability
'         California Method
'           varray(k, 3) = j
'           varray(k, 2) = j / nrec
          End If
        Next j
          
        varray(0, 0) = "File:":          varray(0, 1) = fName
        varray(1, 0) = "Module:":        varray(1, 1) = modLabel
        varray(2, 0) = "Description:":
        varray(4, 0) = "Realization"
        
        pos = InStr(label(ncol), "at time")
        If pos > 0 Then
          tstr = Right(label(ncol), Len(label(ncol)) - (pos + 6))
          tstr = Replace(vdesc(nv - 1), "(s) #(,#...)", tstr)
        Else
          tstr = vdesc(nv - 1)
        End If
        varray(2, 1) = tstr
        
        If dual Then
          varray(4, 1) = label(ncol)
          varray(4, 2) = label(ncol + 1)
          varray(4, 3) = "Prob of exceedence"
          varray(4, 4) = "Rank"
        Else
          varray(4, 1) = label(ncol)
          varray(4, 2) = "Prob of exceedence"
          varray(4, 3) = "Rank"
        End If
        
        j = InStr(1, label(ncol), " ")
        If j > 0 Then
          sname = Left(label(ncol), j - 1)
        Else
          sname = label(ncol)
        End If
        Set wso = GetWorkSheetObject(WorkSheetName(sname))
        range = excelRange(1, 1, 1 + UBound(varray, 2), 1 + UBound(varray, 1))
        wso.range(range).value = varray
        If dual Then
          range = excelRange(3, 5, 4, 1 + UBound(varray, 1))
        Else
          range = excelRange(2, 5, 3, 1 + UBound(varray, 1))
        End If
        
        Set cho = wso.ChartObjects.Add(300, 80, 500, 300)
        With cho.chart
          .ChartType = xlXYScatterLines
          .setSourceData Source:=wso.range(range), PlotBy:=xlColumns
          .HasLegend = False
          .Axes(2).HasTitle = True
          .Axes(2).AxisTitle.characters.Text = "Probability of Exceedence"
          .HasTitle = True
          .charttitle.characters.Text = tstr
        End With
        nv = nv + 1
        
        If Not dual And vdcnt(nv - 2) > 1 Then
          nv = nv - 1
          vdcnt(nv - 1) = vdcnt(nv - 1) - 1
        End If
        
        ncol = ncol + 1
        If dual Then ncol = ncol + 1
       Loop
    Else
      For nl = 1 To nLines
        Line Input #fnum, line
      Next
    End If
  
  Wend
  
sufChart_Exit:
  Close #fnum
  If Not found Then
    PutError "There is no data to report."
  End If
  sufChart = found
  EndModule
  Exit Function
  
sufChart_Error:
  If Err() Then
    If Not Err = 62 Then MsgBox "sufChart: " & Err & " " & Error$
  End If
  Resume sufChart_Exit
End Function

Sub confidenceIntervalChart(cho As Object, fName As String)
Dim csv() As String
Dim fnum As Long
Dim line As String
Dim Module As String
Dim nLines As Long
Dim nr As Long
Dim range As String
Dim found As Boolean
Dim numHeader As Long     'number of header lines
Dim numInputs As Long     'number of input variables
Dim numOutputs As Long    'number of output variables
Dim numReals As Long      'number of realizations
Dim myLabels() As String  'Array of column headings
Dim numtimepts As Long    'number of time points
Dim nc As Long            'lame counter, usually columns
Dim bump As Long          'another lame counter, for start column
Dim count As Long         'another lame counter, for alternating entry into the myData array
Dim startCol As Long      'column to start reading
Dim upper As Double
Dim lower As Double
Dim passLoc As Variant    'location, taken from spreadsheet
Dim passChem As Variant   'constituent, taken from spreadsheet
Dim pos1 As Long          'used in determining constituent on spreadsheet
Dim pos2 As Long          'used in determining constituent on spreadsheet
Dim ns1 As Object         'used for chart series of bound 1
Dim ns2 As Object         'used for chart series of bound 2
Dim passId As String
Dim passType As String
Dim units As String
Dim pe As PEtype
Dim result As Integer
Dim max As Double
Dim newunits As String
  
  On Error GoTo ConfidenceIntervalChart_Error
  
  fnum = FreeFile
  Open fName For Input As #fnum
  While (Not EOF(fnum)) And (Not found)
    ReadCSVLine fnum, csv() 'reads line 1
    Module = csv(1)
    nLines = val(csv(2))
    
    If Not sufName = Module Then
      For nr = 1 To nLines
        Line Input #fnum, line
      Next
    Else
      ReadCSVLine fnum, csv() 'reads line 2
      numHeader = val(csv(1))
      found = True
      
      'skip header lines
      For nr = 1 To numHeader
        Line Input #fnum, line
      Next
      
      ReadCSVLine fnum, csv() 'reads first line after header
      numInputs = val(csv(1))
      numOutputs = val(csv(2))
      'skip input variable rows
      For nr = 1 To numInputs
        Line Input #fnum, line
      Next
        
      'gets current worksheet
      Set wso = wbo.sheets(wbo.sheets.count)
         
      'get location and constituent from spreadsheet
      passLoc = xlsGetCell(wbo, 2, 3)
      passChem = xlsGetCell(wbo, 2, 4)
      passLoc = Replace(passLoc, ":", ".")
      pos1 = InStrRev(passChem, "(")
      pos2 = InStrRev(passChem, ")")
      passChem = Mid(passChem, pos1 + 1, pos2 - pos1 - 1)
      'get id
      pos1 = InStr(passLoc, ".")
      ' if pos is zero then we have an 'all' case
      If pos1 = 0 Then pos1 = InStr(passLoc, " to ") + 3
      pos2 = Len(passLoc)
      passId = Mid(passLoc, pos1 + 1, pos2 - pos1)
      
      'get type
      pos1 = InStr(passLoc, " to ")
      passType = Left(passLoc, pos1 - 1)
  
  'fix up qualifier types
  If passType = "Aquifer-Dissolved" Then passType = "Aquifer"
  If passType = "Surface Water-Dissolved" Then passType = "Surface Water"
  If passType = "Soil-Total" Then passType = "Soil"
  If passType = "Sediment-Total" Then passType = "Sediment"
      
      passLoc = passId + "." + passType
      pos2 = Len(passLoc)
      
      'get values from form
      upper = frmSeries.txtSecondBound.Text
      lower = frmSeries.txtFirstBound.Text
      zeroes = frmSeries.Check1.value
      resolution = frmSeries.txtIntv.Text
      'grab desired output variable row
      bump = 0
      startCol = 0
      numtimepts = 0
      For nr = 1 To numOutputs
        ReadCSVLine fnum, csv() 'reads first output variable row
'        If InStr(csv(1), passLoc) > 0 And InStr(csv(2), "at year(s) #(,#...) for " & passChem) > 0 Then
        If Right(csv(1), pos2) = passLoc And InStr(csv(2), "at year(s) #(,#...) for " & passChem) > 0 Then
          units = Right(csv(2), Len(csv(2)) - InStrRev(csv(2), "("))
          units = Left(units, Len(units) - 1)
          numtimepts = val(csv(3))
          ReDim timearray(numtimepts, 1 To numtimepts * 3) As Variant
          For nc = 1 To numtimepts
            timearray(nc, 1) = csv(4 + nc)
            If nc = 1 Then
              timearray(0, 1) = "yr"
              timearray(0, 2) = lower & " Percentile"
              timearray(0, 3) = upper & " Percentile"
            End If
          Next
          If startCol = 0 Then startCol = numInputs + bump + 2
        Else
          If val(csv(3)) > 0 Then bump = bump + val(csv(3))
        End If
      Next
      
      If startCol = 0 Then
        MsgBox "Montoring data not found for " & passLoc & " at year(s) #(,#...) for " & passChem, vbExclamation
        GoTo Chart_Benchmarks
      End If
      
      ReadCSVLine fnum, csv() 'reads realization row
      numReals = val(csv(1))
      
      ReadCSVLine fnum, csv() 'reads column headings row
      ReDim myLabels(1 To (numtimepts * 2))
      'stores column headings for desired output variable timepoints
      count = 1
      For nc = 1 To (numtimepts * 2)
        If nc Mod 2 = 0 Then
          myLabels(nc) = "Values at time " & timearray(nc / 2, 1)
        Else
          myLabels(nc) = "Probability of Exceedence"
        End If
        count = count + 1
      Next
      
      ReDim pe.series(numtimepts - 1)
      For nc = 0 To numtimepts - 1
        pe.series(nc).count = numReals
        ReDim pe.series(nc).T(numReals - 1)
        ReDim pe.series(nc).V(numReals - 1)
      Next
      For nr = 0 To numReals - 1
        ReadCSVLine fnum, csv() 'reads realization
        For nc = 0 To numtimepts - 1
          pe.series(nc).T(nr) = nr + 1
          'convert g/ml to mg/L
          pe.series(nc).V(nr) = CStr(DoUnitConversion(units, CDbl(csv(startCol + nc)), newunits))
'          pe.series(nc).V(nr) = csv(startCol + nc)
        Next
      Next
      
      max = 0
      For nc = 0 To numtimepts - 1
        ReDim newdata(resolution, 1) As Double
        With pe.series(nc)
          'FRAMES.DLL API call
          result = GetProbability(zeroes, resolution, .count, .T(0), .V(0), .minval, .maxval, .timestart, .timeend, newdata(0, 0), newdata(0, 1))
          'write out concentrations
          If result = 1 Then MsgBox "Time series is not linear, some time points may have been repeated!", vbExclamation, App.Title
          range = excelRange(8 + nc * 2, 7, 9 + nc * 2, 7 + resolution)
          wso.range(range).value = newdata
        End With
        For nr = 0 To resolution - 1
          If newdata(nr, 0) > upper And upper >= newdata(nr + 1, 0) Then
            timearray(nc + 1, 3) = interpolate(newdata(nr, 0), newdata(nr, 1), newdata(nr + 1, 0), newdata(nr + 1, 1), upper)
            If max < val(timearray(nc + 1, 3)) Then max = val(timearray(nc + 1, 3))
          End If
          If lower < newdata(nr, 0) And lower >= newdata(nr + 1, 0) Then
            timearray(nc + 1, 2) = interpolate(newdata(nr, 0), newdata(nr, 1), newdata(nr + 1, 0), newdata(nr + 1, 1), lower)
            If max < val(timearray(nc + 1, 2)) Then max = val(timearray(nc + 1, 2))
          End If
        Next
      Next
      
      'set chart series for bound 1 and 2
      Set ns1 = cho.chart.SeriesCollection.NewSeries()
      Set ns2 = cho.chart.SeriesCollection.NewSeries()
      
      'write out time points
      range = excelRange(4, 6, 6, 6 + UBound(timearray, 1))
      wso.range(range).value = timearray
      
      'write out column headings
      range = excelRange(8, 6, 8 + numtimepts * 2 - 1, 6)
      wso.range(range).value = myLabels
      
      'x-values for chart
      ns1.xValues = wso.range(excelRange(4, 7, 4, 6 + UBound(timearray, 1)))
      ns2.xValues = wso.range(excelRange(4, 7, 4, 6 + UBound(timearray, 1)))
            
      'y-values for chart
      ns1.values = wso.range(excelRange(5, 7, 5, 6 + UBound(timearray, 1)))
      ns2.values = wso.range(excelRange(6, 7, 6, 6 + UBound(timearray, 1)))
      
      'series labels for chart
      ns1.name = timearray(0, 2)
      ns2.name = timearray(0, 3)
      
      'final placement of chart
      cho.Top = 68
      cho.Left = 325
    End If
  Wend
      
Chart_Benchmarks:
  benchmarkChart cho, numtimepts
  
ConfidenceIntervalChart_Error:
  If Err() Then
    If Not Err = 62 Then MsgBox "confidenceInterval: " & Err & " " & Error$
  End If
End Sub

Sub benchmarkChart(cho As Object, Optional numtimepts As Long = 0)
Dim passLoc As Variant    'location, taken from spreadsheet
Dim passChem As Variant   'constituent, taken from spreadsheet
Dim pos1 As Long          'used in determining constituent on spreadsheet
Dim pos2 As Long          'used in determining constituent on spreadsheet
Dim clsDoc As DOMDocument
Dim classNode As IXMLDOMElement
Dim mediumNode As IXMLDOMElement
Dim benchmarkNode As IXMLDOMElement
Dim mediumNodes As IXMLDOMNodeList
Dim benchmarkNodes As IXMLDOMNodeList
Dim url As String
Dim myNode As String
Dim applic As String
Dim benchmarkValue As Double
Dim nodeCount As Long
Dim benchmarkSeries As Object
Dim bcount As Long
Dim passType As String
Dim passId As String
Dim row As Long
Dim celldata As String
Dim numConcRows As Long
Dim Title As String
Dim unit As String
Dim bUnit As String
Dim newUnit As String
Dim newValue As Double
  
  On Error GoTo ConfidenceIntervalChart_Error
  
  url = XMLUrl
  'gets current worksheet
  Set wso = wbo.sheets(wbo.sheets.count)
  
  'get location and constituent from spreadsheet
  passLoc = xlsGetCell(wbo, 2, 3)
  passChem = xlsGetCell(wbo, 2, 4)
  unit = xlsGetCell(wbo, 2, 6)
  passLoc = Replace(passLoc, ":", ".")
  pos1 = InStrRev(passChem, "(")
  pos2 = InStrRev(passChem, ")")
  passChem = Mid(passChem, pos1 + 1, pos2 - pos1 - 1)
  
  'get type
  pos1 = InStr(passLoc, ".")
  pos2 = Len(passLoc)
  passId = Mid(passLoc, pos1 + 1, pos2 - pos1)
  
  'get id
  pos1 = InStr(passLoc, " to ")
  passType = Left(passLoc, pos1 - 1)
  passLoc = passId + "." + passType
  
  'fix up qualifier types
  If passType = "Aquifer-Dissolved" Then passType = "Aquifer"
  If passType = "Surface Water-Dissolved" Then passType = "Surface Water"
  If passType = "Soil-Total" Then passType = "Soil"
  If passType = "Sediment-Total" Then passType = "Sediment"
    
  'get last row from worksheet
  row = 7
  Do
    celldata = xlsGetCell(wso, 2, row)
    row = row + 1
  Loop Until celldata = ""
  numConcRows = row - 8
  
  'load benchmark document
  On Error Resume Next
  Set clsDoc = New DOMDocument
  clsDoc.async = False
  clsDoc.Load url
  If Err.Number > 0 Or url = "" Then
    On Error GoTo 0
    xlsSetCell wso, 4, 8 + numtimepts, "No benchmark file specified"
  Else
    nodeCount = 1
    'find node
    Set classNode = clsDoc.selectSingleNode("//TREECSBenchmarkInfo")
    myNode = "Medium[@type='" + passType + "']"
    Set mediumNodes = classNode.selectNodes(myNode)
    If mediumNodes.length = 0 Then
      xlsSetCell wso, 4, 7 + nodeCount + numtimepts, "No '" + passType + "' benchmarks found in " + XMLUrl
      nodeCount = nodeCount + 2
    Else
      For Each mediumNode In mediumNodes
        applic = mediumNode.getAttribute("applicability")
        Set benchmarkNodes = mediumNode.selectNodes("Benchmark[@casrn='" + passChem + "']")
        bcount = 0
        If benchmarkNodes.length = 0 Then
          xlsSetCell wso, 4, 7 + nodeCount + numtimepts, "Missing benchmark value for " + applic + " " + passChem
          nodeCount = nodeCount + 2
        Else
          For Each benchmarkNode In benchmarkNodes
            If benchmarkNode Is Nothing Then
              xlsSetCell wso, 4, 7 + nodeCount + numtimepts, "Missing benchmark value for " + applic + " " + passChem
              nodeCount = nodeCount + 2
            Else
              bcount = bcount + 1
              On Error Resume Next
              benchmarkValue = CDbl(benchmarkNode.Text)
              If Err.Number = 0 Then
                
                'fix up benchmark units and values for known conversions
                bUnit = benchmarkNode.getAttribute("unit")
                If unit <> bUnit Then
                  newValue = DoUnitConversion(bUnit, benchmarkValue, newUnit)
                  If newUnit <> bUnit Then
                    bUnit = newUnit
                    benchmarkValue = newValue
                  End If
                End If
                
                'write out values to worksheet
                xlsSetCell wso, 4, 7 + nodeCount + numtimepts, applic + " Benchmark " + CStr(bcount)
                xlsSetCell wso, 4, 8 + nodeCount + numtimepts, "yr"
                xlsSetCell wso, 5, 8 + nodeCount + numtimepts, bUnit
                xlsSetCell wso, 4, 9 + nodeCount + numtimepts, xlsGetCell(wso, 1, 7)
                xlsSetCell wso, 5, 9 + nodeCount + numtimepts, benchmarkValue
                xlsSetCell wso, 4, 10 + nodeCount + numtimepts, xlsGetCell(wso, 1, numConcRows + 6)
                xlsSetCell wso, 5, 10 + nodeCount + numtimepts, benchmarkValue
                
                If unit = bUnit Then
                  'add series to chart
                  Set benchmarkSeries = cho.chart.SeriesCollection.NewSeries()
                  'x-values for chart
                  benchmarkSeries.xValues = wso.range(excelRange(4, nodeCount + 9 + numtimepts, 4, nodeCount + 10 + numtimepts))
                  'y-values for chart
                  benchmarkSeries.values = wso.range(excelRange(5, nodeCount + 9 + numtimepts, 5, nodeCount + 10 + numtimepts))
                  'label series
                  benchmarkSeries.name = applic + " Benchmark " + CStr(bcount)
                Else
                  MsgBox applic + " Benchmark " + CStr(bcount) + " units do not match constituent's unit! " + vbCrLf + applic + " Benchmark " + CStr(bcount) + " not graphed!", vbExclamation
                End If
                'add in 5 lines per benchmark
                nodeCount = nodeCount + 5
              Else
                On Error GoTo 0
                xlsSetCell wso, 4, 7 + nodeCount + numtimepts, """" + benchmarkNode.Text + """" + " value not found for " + applic + " " + passChem
                nodeCount = nodeCount + 2
              End If
            End If
          Next
        End If
      Next
    End If
  End If
  
  'final placement of chart
  cho.Top = 68
  cho.Left = 325
  
  'modify title if non-zero values is checked
  If frmSeries.Check1.value = 1 Then
    Title = cho.chart.charttitle.characters.Text
    Title = Title + vbCrLf + " (Using non-zero values only)"
    cho.chart.charttitle.characters.Text = Title
  End If
  
ConfidenceIntervalChart_Error:
  If Err() Then
    If Not Err = 62 Then MsgBox "benchMarkChart: " & Err & " " & Error$
  End If
End Sub

Function IsBenchmark(url As String, ByRef msg) As Boolean
  Dim errmsg As String
  Dim clsDoc As DOMDocument
  Dim classNode As IXMLDOMElement
  
  'load document
  On Error Resume Next
  Set clsDoc = New DOMDocument
  clsDoc.async = False
  clsDoc.Load url
  If Err.Number > 0 Then
     MsgBox "Error loading URL: " + url
     Exit Function
  End If
  
  'find node
  errmsg = ""
  Set classNode = clsDoc.selectSingleNode("TREECSBenchmarkInfo")
  If classNode Is Nothing Then
     msg = "Error loading Benchmarks: XML tag <TREECSBenchmarkInfo> not present."
     IsBenchmark = False
  Else
     IsBenchmark = True
     msg = ""
  End If
  Set clsDoc = Nothing
End Function

Function interpolate(x1 As Double, y1 As Double, x2 As Double, y2 As Double, targetX As Double) As Double
  If x1 = x2 Then
    If x1 = targetX Then
      interpolate = (y1 + y2) * 0.5
      Exit Function
    Else
      interpolate = 1E+300
      Exit Function
    End If
  End If
  If y1 = y2 Then
    interpolate = y1
    Exit Function
  End If
  interpolate = y1 + (y2 - y1) * (targetX - x1) / (x2 - x1)
End Function

Sub sort(n As Long, Idx() As Long, val() As Double, min As Double, max As Double, sum As Double)
Dim i As Long
Dim j As Long
Dim temp As Long

  If (n = 0) Then Exit Sub
  min = val(1)
  max = val(1)
  sum = val(1)
  sum = 0
  
  For i = 1 To n
    sum = sum + val(i)
  Next i
  
  For i = 0 To n - 1
    For j = i + 1 To n
      If (val(Idx(i)) >= val(Idx(j))) Then
        temp = Idx(i)
        Idx(i) = Idx(j)
        Idx(j) = temp
      End If
    Next j
  Next i
End Sub

Public Sub GetModLabelAndDes()
Dim pfilein As parmfile
Dim parm As parmrec
Dim numrec As Long
Dim i As Long
Dim c2 As Long
Dim found As Boolean

  modLabel = ""
  modDesName = ""
  found = False

  If open_parm(pfilein, FUIName & ".GID", F_READ) Then
    Do Until EOCF(pfilein.file)
      If read_parmrec(pfilein, parm) Then
        numrec = parm.idx1
        Select Case parm.pName
        Case "CSM"
          c2 = -1
          found = False
          For i = 1 To numrec
            If read_parmrec(pfilein, parm) Then
              If parm.pName = "ModId" And parm.pval = modName Then
                c2 = parm.idx2
                found = True
              End If
              
              If parm.pName = "ModLabel" And parm.idx1 = siteIdx And parm.idx2 = c2 And found Then
                modLabel = parm.pval
              End If
              If parm.pName = "ModDesPath" And parm.idx1 = siteIdx And parm.idx2 = c2 And found Then
                modDesName = parm.pval
              End If
              
              If parm.pName = "ModSrcId" And parm.idx1 = siteIdx And parm.idx2 = c2 And found Then
                ReDim Preserve modSrcId(parm.idx3) As String
                modSrcId(parm.idx3) = parm.pval
              End If
              If parm.pName = "ModSrcType" And parm.idx1 = siteIdx And parm.idx2 = c2 And found Then
                ReDim Preserve modSrc(parm.idx3) As String
                modSrc(parm.idx3) = parm.pval
              End If
              If parm.pName = "ModSrcLabel" And parm.idx1 = siteIdx And parm.idx2 = c2 And found Then
                ReDim Preserve modSrcLbl(parm.idx3) As String
                modSrcLbl(parm.idx3) = parm.pval
              End If
              
              If parm.pName = "ModId" And parm.idx1 = siteIdx Then
                ReDim Preserve modIds(parm.idx2) As String
                modIds(parm.idx2) = parm.pval
              End If
              If parm.pName = "ModLabel" And parm.idx1 = siteIdx Then
                ReDim Preserve modLbls(parm.idx2) As String
                modLbls(parm.idx2) = parm.pval
              End If
              
              'If found And modLabel <> "" And modDesName <> "" Then Exit For
            End If
          Next
        Case "FUI"
          For i = 1 To numrec
            If read_parmrec(pfilein, parm) Then
              If parm.idx1 = siteIdx Then
                Select Case parm.pName
                  Case "fscasid"
                    If parm.idx3 = 0 Then
                      If parm.idx2 > numCAS Then
                        numCAS = parm.idx2
                        ReDim Preserve cas(numCAS)
                      End If
                      cas(parm.idx2).cas = parm.pval
                    Else
                      If parm.idx3 > cas(parm.idx2).nprog Then
                        cas(parm.idx2).nprog = parm.idx3
                        ReDim Preserve cas(parm.idx2).pcas(parm.idx3)
                        ReDim Preserve cas(parm.idx2).pName(parm.idx3)
                      End If
                      cas(parm.idx2).pcas(parm.idx3) = parm.pval
                    End If
                  Case "fscname"
                    If parm.idx3 = 0 Then
                      If parm.idx2 > numCAS Then
                        numCAS = parm.idx2
                        ReDim Preserve cas(numCAS)
                      End If
                      cas(parm.idx2).name = parm.pval
                    Else
                      If parm.idx3 > cas(parm.idx2).nprog Then
                        cas(parm.idx2).nprog = parm.idx3
                        ReDim Preserve cas(parm.idx2).pcas(parm.idx3)
                        ReDim Preserve cas(parm.idx2).pName(parm.idx3)
                      End If
                      cas(parm.idx2).pName(parm.idx3) = parm.pval
                    End If
                  Case "clktype"
                    If parm.idx3 = 0 Then
                      If parm.idx2 > numCAS Then
                        numCAS = parm.idx2
                        ReDim Preserve cas(numCAS)
                      End If
                      cas(parm.idx2).ktype = val(parm.pval)
                    End If
                End Select
              End If
            End If
          Next
        Case Else
          For i = 1 To numrec
            get_line pfilein.file
          Next
        End Select
      End If
    Loop
    close_parm pfilein
  End If
  
  If viewType = "PE" Then DesName = pdcfType + "probx" + ".des"
  If viewType = "VWR" And Len(pdcfType) = 3 Then DesName = pdcfType + "chart" + ".des"
  If viewType = "VWR" And Len(pdcfType) > 3 Then DesName = Left(pdcfType, 3) + "vwr" + Right(pdcfType, 1) + ".des"
  If viewType = "VWR" Then
    If pdcfType = "SUFWCF" Or pdcfType = "SUFSCF" Or pdcfType = "MRKWCF" Or pdcfType = "MRKSCF" Then
        DesName = Left(pdcfType, 3) + "_" + Right(pdcfType, 3) + ".des"
    End If
  End If
  If Not found Then
    MsgBox "Source not found for " & modName
    End
  End If
End Sub

Function WriteIniString(AppName As String, KeyName As String, value As String) As Boolean
  WriteIniString = WritePrivateProfileString(AppName, KeyName, value, FRAMES_INI)
End Function

Function ReadIniString(AppName As String, KeyName As String, Default As String, value As String) As Boolean
Dim i As Long
Dim pszVal As String

  value = Default
  ReadIniString = False
  
  pszVal = String(255, 0)
  i = GetPrivateProfileString(AppName, KeyName, "", pszVal, Len(pszVal), FRAMES_INI)
  If i > 0 Then
    ReadIniString = True
    value = LCase$(Left$(pszVal, i))
  End If
End Function
