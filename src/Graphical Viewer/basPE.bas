Attribute VB_Name = "basPE"
Option Explicit
Option Compare Text
' define constants to hold the linear regression constants for
'   Kolomogorov-Smirnoff Test Statistic (sample size <= 20)
Public Const c085Percent = 1.23680959769093
Public Const c185Percent = -0.424810732789173
Public Const c285Percent = 9.56946410953763E-02
Public Const c385Percent = -1.18893797287651E-02
Public Const c485Percent = 8.06028926944737E-04
Public Const c585Percent = -2.79332122377444E-05
Public Const c685Percent = 3.86735167788049E-07

Public Const c090Percent = 0.942218564635877
Public Const c190Percent = -0.154781997681289
Public Const c290Percent = 2.06142585693565E-02
Public Const c390Percent = -1.79627126556311E-03
Public Const c490Percent = 9.48410937432886E-05
Public Const c590Percent = -2.71249402610508E-06
Public Const c690Percent = 3.20343906221074E-08

Public Const c095Percent = 0.425167194717695
Public Const c195Percent = 0.140420768718378
Public Const c295Percent = -0.039690617795087
Public Const c395Percent = 4.41313872232513E-03
Public Const c495Percent = -2.48572763413279E-04
Public Const c595Percent = 7.00767619813886E-06
Public Const c695Percent = -7.84356649971222E-08

Public Const c099Percent = 2.03878502389355
Public Const c199Percent = -0.54523505071814
Public Const c299Percent = 8.36676516893566E-02
Public Const c399Percent = -6.97923450335533E-03
Public Const c499Percent = 3.19930297821197E-04
Public Const c599Percent = -7.56648269803519E-06
Public Const c699Percent = 7.20706321192191E-08

'define constants to hold the calculation constants for
'   Kolomogorov-Smirnoff Test Statistic (sample size > 20)
Public Const c785Percent = 1.07
Public Const c790Percent = 1.22
Public Const c795Percent = 1.36
Public Const c799Percent = 1.63

Type TimeSeries
  id As String
  Name As String
  count As Long
  minval As Double
  maxval As Double
  timestart As Long
  timeend As Long
  T() As Double
  V() As Double
  svalue() As Double
  sprob() As Double
End Type

Type PEtype
  id As String
  Name As String
  
  Sheet As String
  label As String
  units As String
  timeunits As String
  xlabel As String
  ylabel As String
  Title As String
  numseries As Long
  series() As TimeSeries
End Type

Dim dblCI85Offset As Double
Dim dblCI90Offset As Double
Dim dblCI95Offset As Double
Dim dblCI99Offset As Double

'variables used to by getprobability call set by frmSeries
Public includeKS As Long
Public zeroes As Long
Public resolution As Long
Public peArray() As PEtype

Public Sub SetBoundsLabels(varray(), nRec As Long)
  varray(1, 5) = "85% CI"
  varray(1, 6) = "15% CI"
  varray(1, 4) = "90% CI"
  varray(1, 7) = "10% CI"
  varray(1, 3) = "95% CI"
  varray(1, 8) = "5% CI"
  varray(1, 2) = "99% CI"
  varray(1, 9) = "1% CI"
  If nRec > 20 Then
    dblCI85Offset = (c785Percent / Sqr(nRec)) * 100
    dblCI90Offset = (c790Percent / Sqr(nRec)) * 100
    dblCI95Offset = (c795Percent / Sqr(nRec)) * 100
    dblCI99Offset = (c799Percent / Sqr(nRec)) * 100
  Else
    dblCI85Offset = (c085Percent + (c185Percent * nRec) + (c285Percent * nRec ^ 2) + (c385Percent * nRec ^ 3) + (c485Percent * nRec ^ 4) + (c585Percent * nRec ^ 5) + (c685Percent * nRec ^ 6)) * 100
    dblCI90Offset = (c090Percent + (c190Percent * nRec) + (c290Percent * nRec ^ 2) + (c390Percent * nRec ^ 3) + (c490Percent * nRec ^ 4) + (c590Percent * nRec ^ 5) + (c690Percent * nRec ^ 6)) * 100
    dblCI95Offset = (c095Percent + (c195Percent * nRec) + (c295Percent * nRec ^ 2) + (c395Percent * nRec ^ 3) + (c495Percent * nRec ^ 4) + (c595Percent * nRec ^ 5) + (c695Percent * nRec ^ 6)) * 100
    dblCI99Offset = (c099Percent + (c199Percent * nRec) + (c299Percent * nRec ^ 2) + (c399Percent * nRec ^ 3) + (c499Percent * nRec ^ 4) + (c599Percent * nRec ^ 5) + (c699Percent * nRec ^ 6)) * 100
  End If
End Sub

Public Sub SetBounds(varray(), k As Long, X As Double, Y As Double)
'must call SetBoundLabels first
Dim vTemp As Double

  varray(k + 1, 0) = X
  varray(k + 1, 1) = Y
  vTemp = Y + dblCI99Offset
  If vTemp < 100 Then
    varray(k + 1, 2) = vTemp
  Else
    varray(k + 1, 2) = 100
  End If
  vTemp = Y - dblCI99Offset
  If vTemp > 0 Then
    varray(k + 1, 9) = vTemp
  Else
    varray(k + 1, 9) = 0
  End If
  vTemp = Y + dblCI95Offset
  If vTemp < 100 Then
    varray(k + 1, 3) = vTemp
  Else
    varray(k + 1, 3) = 100
  End If
  vTemp = Y - dblCI95Offset
  If vTemp > 0 Then
    varray(k + 1, 8) = vTemp
  Else
    varray(k + 1, 8) = 0
  End If
  vTemp = Y + dblCI90Offset
  If vTemp < 100 Then
    varray(k + 1, 4) = vTemp
  Else
    varray(k + 1, 4) = 100
  End If
  vTemp = Y - dblCI90Offset
  If vTemp > 0 Then
    varray(k + 1, 7) = vTemp
  Else
    varray(k + 1, 7) = 0
  End If
  vTemp = Y + dblCI85Offset
  If vTemp < 100 Then
    varray(k + 1, 5) = vTemp
  Else
    varray(k + 1, 5) = 100
  End If
  vTemp = Y - dblCI85Offset
  If vTemp > 0 Then
    varray(k + 1, 6) = vTemp
  Else
    varray(k + 1, 6) = 0
  End If
End Sub

Public Function peAddSeriesToChart(wso As Object, cho As Object, Name As String, _
                                   xrange As String, yrange As String, Optional ag = 1, Optional Smooth = False)
Dim ns As Object
  Set ns = cho.chart.SeriesCollection.NewSeries()
  ns.xValues = wso.range(xrange)
  ns.values = wso.range(yrange)
  ns.Name = Name
  ns.AxisGroup = ag
  ns.Smooth = Smooth
End Function

Public Sub GetProbabilityForThis(ByRef pe As TimeSeries)
Dim result As Integer

On Error GoTo ErrorHandler
  
  If pe.count = 0 Then Exit Sub
  
  ReDim pe.sprob(0 To resolution)
  ReDim pe.svalue(0 To resolution)
  'FRAMES.DLL API call
  result = GetProbability(zeroes, resolution, pe.count, pe.T(0), pe.V(0), pe.minval, pe.maxval, pe.timestart, pe.timeend, pe.sprob(0), pe.svalue(0))
  If result = 1 Then MsgBox "Time series is not linear, some time points may have been repeated!", vbExclamation, App.Title

ErrorHandler:
  If Err.Number <> 0 Then
    MsgBox task & vbCrLf & Err.Description, vbOKOnly, "GetProbabilityForThis"
  End If
End Sub

Public Function GetProbabilitys(pe As PEtype) As Long
  Dim i As Long
  Dim result As Integer
  
  GetProbabilitys = 1
  If UBound(xValues) = 0 Then Exit Function
  
  ReDim pe.series(0)
  pe.series(0).count = UBound(xValues)
  ReDim pe.series(0).T(UBound(xValues))
  ReDim pe.series(0).V(UBound(yValues))
  For i = 0 To UBound(xValues) - 1
    pe.series(0).T(i) = xValues(i)
    pe.series(0).V(i) = yValues(i)
  Next
  ReDim pe.series(0).sprob(resolution)
  ReDim pe.series(0).svalue(resolution)
  
  With pe.series(0)
    'FRAMES.DLL API call
    result = GetProbability(zeroes, resolution, .count, .T(0), .V(0), .minval, .maxval, .timestart, .timeend, .sprob(0), .svalue(0))
    If result = 1 Then MsgBox "Time series is not linear, some time points may have been repeated!", vbExclamation, App.Title
  End With
  GetProbabilitys = result
End Function

Public Sub GetLabels(off As Long, pxlabel As String, pylabel As String, _
                      Title As String, xlabel As String, ylabel As String)
  Dim vstr As Variant
  Dim tstr As Variant
  Title = ""
  xlabel = ""
  ylabel = ""
  
  Title = "Probability of Equaling or Exceeding "
  Select Case pdcfType
    Case "wcf"
      activeSpread.GetText off + 1, CBO_CHM, vstr:    Title = Title + vstr + " Concentration"
      ylabel = "Concentrations"
      xlabel = ylabel & " " & pxlabel
    Case "scf"
      activeSpread.GetText off + 1, CBO_CHM, vstr:    Title = Title + vstr + " Concentration"
      ylabel = "Concentrations"
      xlabel = ylabel & " " & pxlabel
    Case "wff"
      activeSpread.GetText off + 1, CBO_CHM, vstr:    Title = Title + vstr + " Water Flux"
      ylabel = "Flux"
      xlabel = ylabel & " " & pxlabel
    Case "aff"
      activeSpread.GetText off + 1, CBO_CHM, vstr:    Title = Title + vstr + " Air Flux"
      ylabel = "Flux"
      xlabel = ylabel & " " & pxlabel
    Case "ato"
      activeSpread.GetText off + 1, CBO_CHM, vstr:    Title = Title + vstr + " "
      activeSpread.GetText off + 1, CBO_MSR, tstr:
      activeSpread.GetText off + 1, CBO_DEP, vstr:    If vstr <> "Not Available" Then tstr = vstr + " " + tstr
      activeSpread.GetText off + 1, CBO_FLX, vstr:    Title = Title + " " + tstr + " from " + vstr + " flux"
      activeSpread.GetText off, numSel + 2, vstr:     xlabel = tstr & " " & vstr
      ylabel = tstr
    Case "epf":
      activeSpread.GetText off + 1, CBO_CHM, vstr:    Title = Title + vstr + " Concentration for "
      activeSpread.GetText off + 1, CBO_RTE, vstr:    Title = Title + vstr + " route(s) and "
      activeSpread.GetText off + 1, CBO_PTH, vstr:    Title = Title + vstr + " pathway(s)"
      activeSpread.GetText off, numSel + 2, vstr:     xlabel = "Concentration " & vstr
      ylabel = "Concentration"
    Case "rif":
      activeSpread.GetText off + 1, CBO_MSR, vstr
      If vstr <> "intake" Then
        ylabel = vstr & " Intake"
      Else
        ylabel = vstr
      End If
      activeSpread.GetText off + 1, CBO_UNT, vstr:    xlabel = xlabel + " (" + vstr + ")"
      activeSpread.GetText off + 1, CBO_CHM, vstr:    Title = Title + vstr + " " + ylabel + " for "
      activeSpread.GetText off + 1, CBO_RTE, vstr:    Title = Title + vstr + " route(s) and "
      activeSpread.GetText off + 1, CBO_PTH, vstr:    Title = Title + vstr + " pathway(s)"
    Case "hif":
      activeSpread.GetText off + 1, CBO_MSR, vstr:    ylabel = vstr
      activeSpread.GetText off + 1, CBO_UNT, vstr:    xlabel = xlabel + " (" + vstr + ")"
      activeSpread.GetText off + 1, CBO_CHM, vstr:    Title = Title + vstr + " " + ylabel + " for "
      activeSpread.GetText off + 1, CBO_RTE, vstr:    Title = Title + vstr + " route(s) and "
      activeSpread.GetText off + 1, CBO_PTH, vstr:    Title = Title + vstr + " pathway(s)"
    Case "bbf":
      activeSpread.GetText off + 1, CBO_OR, vstr:     Title = Title + "the Body Burden of " + vstr + " exposed to "
      activeSpread.GetText off + 1, CBO_CHM, vstr:    Title = Title + vstr
      ylabel = "Body Burden"
      xlabel = ylabel & " " & pxlabel
    Case "hqf":
      activeSpread.GetText off + 1, CBO_OR, vstr:     Title = Title + "the Hazard Quotient for " + vstr + " exposed to "
      activeSpread.GetText off + 1, CBO_CHM, vstr:    Title = Title + vstr
      ylabel = "Hazard Quotient"
      xlabel = ylabel & " " & pxlabel
  End Select
  ylabel = "Probability of Equaling or Exceeding " & ylabel & " " & pylabel
    
End Sub

Public Sub ReportError(task As String, caption As String)
  MsgBox "ERROR: " & Err.Description & vbCrLf & "Task: " & task, vbOKOnly, caption
End Sub

Public Sub peChart(wso As Object, off As Long, pe As PEtype)
Dim row As Long
Dim nrow As Long
Dim vstr As Variant
Dim i As Long
Dim k As Long
Dim s As Long
Dim nseries As Long
Dim series As TimeSeries
Dim lngChartLoc As Long
Dim lngRowHeight As Double
Dim range As String
Dim Title As String
Dim xlabel As String
Dim ylabel As String

  series = pe.series(0)
  ' get the row height in points
  lngRowHeight = wso.Rows(1).rowHeight
                            
  On Error GoTo ErrorHandler
  row = 1
  xlsSetCell wso, 1, row, "File"
  xlsSetCell wso, 2, row, FUIName + "." + pdcfType
  row = 2
  xlsSetCell wso, 1, row, "Module"
  xlsSetCell wso, 2, row, modLabel + " (" + modName + ")"
  For row = 1 To numSel
    activeSpread.GetText off, row, vstr:      xlsSetCell wso, 1, row + 2, vstr
    activeSpread.GetText off + 1, row, vstr:  xlsSetCell wso, 2, row + 2, vstr
  Next
  
  row = row + 3
  nseries = IIf(1 = UBound(pe.series), 0, UBound(pe.series))
  For s = 0 To nseries
    series = pe.series(s)
    nrow = UBound(series.sprob) + 1
    
    xlsSetCell wso, 1, row, "Probability of Exceedence "
    row = row + 1
    xlsSetCell wso, 1, row, "Time Start"
    xlsSetCell wso, 2, row, Format(series.T(series.timestart), "0.0") & " " & IIf("" = pe.timeunits, "yr", pe.timeunits)
    row = row + 1
    xlsSetCell wso, 1, row, "Time End"
    xlsSetCell wso, 2, row, Format(series.T(series.timeend), "0.0") & " " & IIf("" = pe.timeunits, "yr", pe.timeunits)
    row = row + 1
    xlsSetCell wso, 1, row, _
        IIf(s = 0 And nseries > 1, "Total", _
        IIf("" <> series.Name, series.Name & " (" & series.id & ")", ""))
    lngChartLoc = row * lngRowHeight
    row = row + 23
    
    If (s = 0 And nseries > 1) Then pe.Title = "Total " & pe.Title
    If (s > 0 And nseries > 1) Then pe.Title = series.Name & " (" & series.id & ")"
    
    ReDim X(1 To nrow, 1 To 1)
    ReDim Y(1 To nrow, 1 To 1)
    For i = LBound(series.sprob) To UBound(series.sprob)
      X(i + 1, 1) = series.svalue(i)
      Y(i + 1, 1) = series.sprob(i)
    Next
    
    If includeKS = 1 Then
      ' now set up a variant array to hold the information to be loaded into the spreadsheet
      ReDim varray(nrow + 1, 10)
      ' load the header info into the array
      varray(1, 0) = pe.xlabel
      varray(1, 1) = pe.ylabel
      SetBoundsLabels varray, nrow
      ' setup a loop to load the data values into the array
      For k = 1 To nrow
        SetBounds varray, k, val(X(k, 1)), val(Y(k, 1))
      Next
    Else
      ' now set up a variant array to hold the information to be loaded into the spreadsheet
      ReDim varray(nrow + 1, 2)
      ' load the header info into the array
      varray(1, 0) = pe.xlabel
      varray(1, 1) = pe.ylabel
      For k = 1 To nrow
        varray(k + 1, 0) = val(X(k, 1))
        varray(k + 1, 1) = val(Y(k, 1))
      Next
    End If
    
    ' now we have all the data loaded into the array, time to load it into the spreadsheet
    range = excelRange(1, row, UBound(varray, 2), row + UBound(varray, 1))
    wso.range(range).value = varray
    ' next we need to set up a chart on the Excel worksheet
    range = excelRange(1, row + 1, UBound(varray, 2), row + UBound(varray, 1))
    GetLabels off, pe.xlabel, pe.ylabel, Title, xlabel, ylabel
    ExportToExcelLoc wso, range, Title, xlabel, ylabel, 10, lngChartLoc
    row = row + nrow + 3
  Next
  wso.Name = WorkSheetName(pe.Sheet)
  
ErrorHandler:
  If Err.Number <> 0 Then
    MsgBox task & vbCrLf & Err.Description, vbOKOnly, "peChart"
  End If
  
End Sub

Public Sub peCreateChart(cnt As Long)
Dim i As Long
Dim pe As PEtype
  
  If Not GetWorkbookObject() Then
    MsgBox "Error getting Excel workbook object"
    Exit Sub
  End If
  
  For i = 1 To cnt
    pe = peArray(i)
    Set wso = GetWorkSheetObject("sheet" & CStr(i))
    peChart wso, (i * 2) - 1, pe
  Next i
  
ErrorHandler:
  If Err.Number <> 0 Then
    MsgBox task & vbCrLf & "Error: " + Error, vbOKOnly, "peCreateChart"
  End If
  activeForm.MousePointer = vbDefault
End Sub

