Attribute VB_Name = "basHQF"
Option Explicit
Option Compare Text

Private jurisdiction() As String

Function hqfSheetChange(col As Long, row As Long) As Boolean
Dim i As Long
Dim j As Long
Dim k As Long
Dim m As Long
Dim T As Long
Dim num As Long
Dim mrow  As Long
Dim totalDose As Double
Dim totalEHQ As Double
Dim effect As String
Dim valus
Dim juris As String
Dim measure As String
Dim typefx As String
Dim trv As Double
Dim ehq As Double
Dim retcode As Boolean
Dim ok As Boolean
Dim eunit As String

  Select Case row
    Case CBO_OR
      updateContams col
      updateJurisdiction col
      updateTimePts col
    Case CBO_CHM
      updateJurisdiction col
      updateTimePts col
    Case CBO_JRS
      updateTimePts col
  End Select
  
  mrow = CBO_TME + 2
 
  activeSpread.row = numSel + 2
  activeSpread.row2 = activeSpread.MaxRows
  activeSpread.col = col - 1
  activeSpread.col2 = col + 4
  activeSpread.BlockMode = True
  activeSpread.Action = 3 ' clear
  activeSpread.BlockMode = False
  
  activeSpread.SetText col - 1, mrow, "Effects Measure"
  activeSpread.SetText col, mrow, "Type of Effect"
  activeSpread.SetText col + 1, mrow, "Toxicity Reference Value (mg/kg/day)"
  activeSpread.SetText col + 2, mrow, "Associated Medium"
  activeSpread.SetText col + 3, mrow, "Exposure Dose (mg/kg/day)"
  activeSpread.SetText col + 4, mrow, "Ecological HQ"
  
  For m = col - 1 To col + 6
    activeSpread.ColWidth(m) = 10
  Next
  
  activeSpread.row = numSel + 2
  activeSpread.row2 = activeSpread.row
  activeSpread.col = col - 1
  activeSpread.col2 = col + 4
  activeSpread.BlockMode = True
  activeSpread.CellType = CellTypeStaticText
  activeSpread.BackColor = vbYellow
  activeSpread.TypeTextWordWrap = True
  activeSpread.TypeHAlign = TypeHAlignCenter
  activeSpread.TypeVAlign = TypeVAlignCenter
  activeSpread.rowHeight(activeSpread.row) = 40
  activeSpread.BlockMode = False
  
  mrow = mrow + 1
  j = GetSelIdx(CBO_OR, col) + 1
  k = GetSelIdx(CBO_CHM, col) + 1
  T = GetSelIdx(CBO_TME, col)
  
  num = exfGetNumEffects(1, j, k)
  For m = 1 To num
    exfGetSeriesProperties 1, j, k, m, effect, eunit, eunit
    
    valus = Split(effect, "~")
    juris = valus(0)
    measure = valus(1)
    typefx = valus(2)
    trv = val(valus(3))
    
    If juris = GetSelText(CBO_JRS, col) Then
      activeSpread.SetText col - 1, mrow, measure
      activeSpread.SetText col, mrow, typefx
      activeSpread.SetText col + 1, mrow, trv
      
      totalDose = 0
      totalEHQ = 0
      For i = 0 To numDS - 1
        activeSpread.SetText col + 2, mrow, ds(i).locName
        ok = hqfGetSeries(i + 1, j, k, m)
        ehq = 0
        If T <= UBound(yValues) Then
          ehq = yValues(T)
          activeSpread.SetText col + 3, mrow, Format(ehq * trv, "Standard")
          activeSpread.SetText col + 4, mrow, Format(ehq, "Scientific")
        Else
          activeSpread.SetText col + 3, mrow, ""
          activeSpread.SetText col + 4, mrow, ""
        End If
        totalDose = totalDose + (ehq * trv)
        totalEHQ = totalEHQ + ehq
        mrow = mrow + 1
      Next
      activeSpread.SetText col + 2, mrow, "Total"
      activeSpread.SetText col + 3, mrow, Format(totalDose, "Scientific")
      activeSpread.SetText col + 4, mrow, Format(totalEHQ, "Scientific")
      mrow = mrow + 1
    End If
  Next
  
  hqfSheetChange = retcode
End Function

Function hqf2SheetChange(col As Long, row As Long) As Boolean
Dim i As Long
Dim j As Long
Dim k As Long
Dim m As Long
Dim T As Long
Dim num As Long
Dim pos As Long
Dim pos1 As Long
Dim mrow As Long
Dim effect As String
Dim duration As String
Dim bodypart As String
Dim measure As String
Dim typefx As String
Dim variability As String
Dim uncertainty As String
Dim trv As Double
Dim ehq As Double
Dim retcode As Boolean
Dim ok As Boolean
Dim eunit As String

  Select Case row
    Case CBO_DS
      updateOrganisms col
      updateContams col
      updateTimePts2 col
    Case CBO_OR
      updateContams col
      updateTimePts2 col
    Case CBO_CHM
      updateTimePts2 col
  End Select
  
  mrow = CBO_TME + 2
  
  activeSpread.row = numSel + 2
  activeSpread.row2 = activeSpread.MaxRows
  activeSpread.col = col - 1
  activeSpread.col2 = col + 8
  activeSpread.BlockMode = True
  activeSpread.Action = 3 ' clear
  activeSpread.BlockMode = False
  
  activeSpread.SetText col - 1, mrow, "Effects Measure"
  activeSpread.SetText col, mrow, "Type of Effect"
  activeSpread.SetText col + 1, mrow, "Body Part"
'  activeSpread.SetText col + 2, mrow, "Life Stage"
  activeSpread.col = col + 2
  activeSpread.ColHidden = True
  activeSpread.SetText col + 3, mrow, "Toxicity Reference Value (mg/kg wet)"
  activeSpread.SetText col + 4, mrow, "TRV Exposure Duration (day)"
  activeSpread.SetText col + 5, mrow, "Tissue Concentration (mg/kg wet)"
  activeSpread.SetText col + 6, mrow, "Ecological HQ"
  activeSpread.SetText col + 7, mrow, "Uncertainty"
  activeSpread.SetText col + 8, mrow, "Variabilty"
  
  For m = col - 1 To col + 8
    activeSpread.ColWidth(m) = 10
  Next
  
  activeSpread.row = numSel + 2
  activeSpread.row2 = activeSpread.row
  activeSpread.col = col - 1
  activeSpread.col2 = col + 8
  activeSpread.BlockMode = True
  activeSpread.CellType = CellTypeStaticText
  activeSpread.BackColor = vbYellow
  activeSpread.TypeTextWordWrap = True
  activeSpread.TypeHAlign = TypeHAlignCenter
  activeSpread.TypeVAlign = TypeVAlignCenter
  activeSpread.rowHeight(activeSpread.row) = 40
  activeSpread.BlockMode = False
  
  
  mrow = mrow + 1
  i = GetSelIdx(CBO_DS, col) + 1
  j = GetSelIdx(CBO_OR, col) + 1
  k = GetSelIdx(CBO_CHM, col) + 1
  T = GetSelIdx(CBO_TME, col)
  
  num = exfGetNumEffects(1, j, k)
  For m = 1 To num
    exfGetSeriesProperties 1, j, k, m, effect, eunit, eunit
    pos = InStr(effect, " ")
    pos1 = InStr(effect, " mg/kg for ")
    measure = Left(effect, pos - 1)
    
    duration = Mid(effect, pos + 1, pos1 - (pos + 1))
    trv = val(duration)
    
    pos = InStr(effect, " mg/kg for ")
    pos1 = InStr(effect, " days")
    If pos = 0 Then
      duration = "N/A"
    Else
      duration = Mid(effect, pos + 11, pos1 - (pos + 11))
    End If
    
    pos = InStr(effect, " resulting in ")
    pos1 = InStr(effect, " effects")
    If pos = 0 Then
      typefx = "N/A"
    Else
      typefx = Mid(effect, pos + 14, pos1 - (pos + 14))
    End If
    
    pos = InStr(effect, " for the ")
    pos1 = InStr(effect, " with un")
    If pos = 0 Then
      bodypart = "N/A"
    ElseIf pos1 = 0 Then
      bodypart = Right(effect, Len(effect) - (pos + 8))
    Else
      bodypart = Mid(effect, pos + 9, pos1 - (pos + 9))
    End If
    
    pos = InStr(effect, " uncertainty of ")
    pos1 = InStr(effect, " and vari")
    If pos = 0 Then
      uncertainty = "N/A"
    ElseIf pos1 = 0 Then
      uncertainty = Right(effect, Len(effect) - (pos + 15))
    Else
      uncertainty = Mid(effect, pos + 16, pos1 - (pos + 16))
    End If
    
    pos = InStr(effect, " variabilty of ")
    If pos = 0 Then
      variability = "N/A"
    Else
      variability = Right(effect, Len(effect) - (pos + 14))
    End If
    
    activeSpread.SetText col - 1, mrow, measure
    activeSpread.SetText col, mrow, typefx
    activeSpread.SetText col + 1, mrow, bodypart
'    activeSpread.SetText col + 2, mrow, stage
    activeSpread.SetText col + 3, mrow, trv
    activeSpread.SetText col + 4, mrow, duration
    activeSpread.SetText col + 7, mrow, uncertainty
    activeSpread.SetText col + 8, mrow, variability
    
    ok = hqfGetSeries(i, j, k, m)
    ehq = 0
    If T <= UBound(yValues) Then
      ehq = yValues(T)
      activeSpread.SetText col + 5, mrow, Format(ehq * trv, "Standard")
      activeSpread.SetText col + 6, mrow, Format(ehq, "Scientific")
    Else
      activeSpread.SetText col + 5, mrow, ""
      activeSpread.SetText col + 6, mrow, ""
    End If
    mrow = mrow + 1
  Next
  
  hqf2SheetChange = retcode
End Function

Sub updateJurisdiction(col As Long)
Dim j As Long
Dim cbolist As String

  If CBO_DS < 0 Then Exit Sub
  cbolist = ""
  For j = 1 To UBound(jurisdiction)
    cbolist = cbolist + IIf(cbolist <> "", Chr$(9), "") + jurisdiction(j)
  Next
  SetSelList CBO_JRS, col, cbolist
End Sub

Sub updateTimePts2(col As Long)
Dim j As Long
Dim ok As Boolean
Dim dsIdx As Long
Dim orgIdx As Long
Dim conIdx As Long
Dim efxIdx As Long
Dim cbolist As String
  
  dsIdx = GetSelIdx(CBO_DS, col) + 1
  orgIdx = GetSelIdx(CBO_OR, col) + 1
  conIdx = GetSelIdx(CBO_CHM, col) + 1
  efxIdx = 1
  ok = hqfGetSeries(dsIdx, orgIdx, conIdx, efxIdx)
  If CBO_DS < 0 Then Exit Sub
  cbolist = ""
  For j = 0 To UBound(xValues) - 1
    cbolist = cbolist + IIf(cbolist <> "", Chr$(9), "") + CStr(xValues(j))
  Next
  SetSelList CBO_TME, col, cbolist
End Sub

Sub updateTimePts(col As Long)
Dim j As Long
Dim ok As Boolean
Dim orgIdx As Long
Dim conIdx As Long
Dim efxIdx As Long
Dim cbolist As String
  
  orgIdx = GetSelIdx(CBO_OR, col) + 1
  conIdx = GetSelIdx(CBO_CHM, col) + 1
  efxIdx = 1
  For j = 1 To numDS
    ok = hqfGetSeries(j, orgIdx, conIdx, efxIdx)
    If UBound(xValues) > 0 Then Exit For
  Next
  If CBO_DS < 0 Then Exit Sub
  cbolist = ""
  For j = 0 To UBound(xValues) - 1
    cbolist = cbolist + IIf(cbolist <> "", Chr$(9), "") + CStr(xValues(j))
  Next
  SetSelList CBO_TME, col, cbolist
End Sub

Function hqfGetSeries(dsIdx As Long, orgIdx As Long, conIdx As Long, efxIdx As Long) As Boolean
  Dim nvalues As Long
  Dim tmp As String

  hqfGetSeries = False
  
  nvalues = exfGetSeriesProperties(dsIdx, orgIdx, conIdx, efxIdx, tmp, ylabel, xlabel)
  ReDim xValues(nvalues)
  ReDim yValues(nvalues)
  If nvalues = 0 Then Exit Function
  nvalues = exfGetSeriesValues(dsIdx, orgIdx, conIdx, efxIdx, nvalues, xValues(0), yValues(0))
  hqfGetSeries = True
End Function

Function hqfLoadDatasets() As Long
Dim pfilein As parmfile
Dim parm As parmrec
Dim numrec As Long
Dim i As Long
Dim retc As Long
Dim temp As String

  On Error GoTo ErrorHandler
  
  task = "numds = readEXFDatasets(fuiname, ModName)"
  exfOpen FUIName, modName
  numDS = exfGetNumSets()

  ReDim ds(numDS)
  For i = 0 To numDS - 1
     task = "retc = exfGetDatasetInfo(i + 1, ds(i).locName, ds(i).locType)"
     retc = exfGetSetInfo(i + 1, ds(i).locName, ds(i).locType)
  Next
  
  temp = ""
  For i = 1 To UBound(modSrcId)
    If InStr("cct", modSrc(i)) > 0 Then
      temp = modSrcId(i)
    End If
  Next
  If temp <> "" Then
    If open_parm(pfilein, FUIName & ".GID", F_READ) Then
      Do Until EOCF(pfilein.file)
        If read_parmrec(pfilein, parm) Then
          numrec = parm.idx1
          Select Case parm.pName
          Case temp
            For i = 1 To numrec
              If read_parmrec(pfilein, parm) Then
                If parm.pName = "Jurisdiction" Then
                  ReDim Preserve jurisdiction(parm.idx1) As String
                  jurisdiction(parm.idx1) = parm.pval
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
  End If

  
ErrorHandler:
  If Err.Number <> 0 Then
    PutError task
    numDS = 0
  End If
  hqfLoadDatasets = numDS
End Function
