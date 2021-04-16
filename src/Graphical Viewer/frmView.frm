VERSION 5.00
Object = "{FE0065C0-1B7B-11CF-9D53-00AA003C9CB6}#1.1#0"; "comct232.ocx"
Object = "{B02F3647-766B-11CE-AF28-C3A2FBE76A13}#2.5#0"; "ss32x25.ocx"
Begin VB.Form frmView 
   Caption         =   "Health Impacts"
   ClientHeight    =   6660
   ClientLeft      =   5550
   ClientTop       =   3390
   ClientWidth     =   6840
   LinkTopic       =   "Form1"
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   6660
   ScaleWidth      =   6840
   StartUpPosition =   2  'CenterScreen
   Begin ComCtl2.UpDown UpDown1 
      Height          =   288
      Left            =   2880
      TabIndex        =   5
      Top             =   240
      Width           =   240
      _ExtentX        =   344
      _ExtentY        =   503
      _Version        =   327681
      Value           =   1
      BuddyControl    =   "txtCount"
      BuddyDispid     =   196610
      OrigLeft        =   3720
      OrigTop         =   120
      OrigRight       =   3960
      OrigBottom      =   495
      Max             =   25
      Min             =   1
      SyncBuddy       =   -1  'True
      BuddyProperty   =   0
      Enabled         =   -1  'True
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Print Tables"
      Height          =   405
      Left            =   5040
      TabIndex        =   4
      Top             =   120
      Width           =   1545
   End
   Begin FPSpreadADO.fpSpread vaSpread1 
      Height          =   5712
      Left            =   240
      TabIndex        =   0
      Top             =   720
      Width           =   6360
      _Version        =   131077
      _ExtentX        =   11218
      _ExtentY        =   10075
      _StockProps     =   64
      BackColorStyle  =   1
      ColHeaderDisplay=   0
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      MaxCols         =   2
      SelectBlockOptions=   0
      SpreadDesigner  =   "frmView.frx":0000
   End
   Begin VB.TextBox txtCount 
      Alignment       =   2  'Center
      Enabled         =   0   'False
      Height          =   285
      Left            =   2400
      TabIndex        =   3
      Text            =   "1"
      Top             =   240
      Width           =   420
   End
   Begin VB.CommandButton Command2 
      Caption         =   "Excel Chart"
      Height          =   405
      Left            =   3360
      TabIndex        =   1
      Top             =   120
      Width           =   1545
   End
   Begin VB.Label Label1 
      AutoSize        =   -1  'True
      Caption         =   "Number of criteria (max 25) :"
      Height          =   192
      Left            =   240
      TabIndex        =   2
      Top             =   240
      Width           =   1968
   End
End
Attribute VB_Name = "frmView"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Private Sub Command1_Click()
    vaSpread1.PrintUseDataMax = True
    vaSpread1.PrintGrid = False
    vaSpread1.PrintColHeaders = False
    vaSpread1.PrintRowHeaders = False
    vaSpread1.PrintBorder = False
    On Error GoTo PRINTERROR
    vaSpread1.Action = 13
    On Error GoTo 0
PRINTERROR:
End Sub

Private Sub Command2_Click()
  If pdcfType = "ato" Then
    atoCreateChart txtCount
  Else
    CreateChart
  End If
End Sub

Private Sub form_Load()

  On Error GoTo ErrorHandler:
  
  Set activeForm = Me
  Set activeSpread = vaSpread1
  task = "ReDim Routes(0)"
  ReDim Routes(0)
  task = "ReDim exposure(0, 0)"
  ReDim Exposure(0, 0)
  
  txtCount = 1
  
  Select Case pdcfType
    Case "ato"
      caption = "Atmospheric Concentration and Deposistions"
      atoLoadDatasets pdcfName, ModName
      atoInitSelect 1
    Case "epf":
      caption = "Exposure Pathway Concentrations"
      epfLoadDatasets
      expInitSelect 1
    Case "rif":
      caption = "Receptor Intakes"
      rifLoadDatasets
      expInitSelect 1
    Case "hif":
      caption = "Health Impacts Over Time"
      hifLoadDatasets
      expInitSelect 1
  End Select
  If AnError Or numds = 0 Then Unload Me
ErrorHandler:
  If Err.Number <> 0 Then ReportError "Form_Load", "frmView.frm"
  vaSpread1_Change 2, 2
End Sub

Private Sub Form_Resize()
  If Me.ScaleWidth = 0 Or Me.ScaleHeight = 0 Then Exit Sub
  vaSpread1.Move 400, 1000, Me.ScaleWidth - 700, Me.ScaleHeight - 1200
End Sub

Private Sub Form_Unload(Cancel As Integer)
  On Error Resume Next
  Select Case pdcfType
    Case "ato":
      atoCloseDataset
    Case "hif":
      hifCloseDataset
    Case "epf":
      epfCloseDataset
    Case "rif":
      rifCloseDataset
  End Select
  Close
  If Not AnError Then Kill RunName & ".ERR"
  Set wbo = Nothing
  Set cho = Nothing
  End
End Sub

Private Sub txtCount_Change()
  numcol = ChangeInCriteriaCount(txtCount)
End Sub

Private Sub UpDown1_Change()
  If Not vaSpread1.Enabled Then Exit Sub
  txtCount = UpDown1.Value
End Sub

Public Sub vaSpread1_Change(ByVal Col As Long, ByVal Row As Long)
  If Not (Col > 0 And Col <= CInt(txtCount) * 2) Then Exit Sub
  Select Case pdcfType
    Case "ato"
      atoSpreadsheetChange Col, Row
    Case Else
      Command2.Enabled = expSpreadsheetChange(Col, Row)
  End Select
End Sub

Private Sub vaSpread1_KeyDown(KeyCode As Integer, Shift As Integer)
  Dim cursel As Long
  If Not (Shift = 1 And (KeyCode = vbKeyUp Or KeyCode = vbKeyDown)) Then Exit Sub
  vaSpread1.Col = vaSpread1.ActiveCol: vaSpread1.Row = vaSpread1.ActiveRow
  If Not vaSpread1.CellType = 8 Then Exit Sub
  cursel = vaSpread1.TypeComboBoxCurSel
  If KeyCode = vbKeyUp Then
    vaSpread1.TypeComboBoxCurSel = IIf(cursel = 0, vaSpread1.TypeComboBoxCount - 1, cursel - 1)
  Else
    vaSpread1.TypeComboBoxCurSel = IIf(cursel = vaSpread1.TypeComboBoxCount - 1, 0, cursel + 1)
  End If
  vaSpread1_Change vaSpread1.Col, vaSpread1.Row
  KeyCode = 0
End Sub

Function hifGetSeries(Col&, ttimes() As Single, tvalues() As Single, nprog&, ncon&, nd&, age&, npt&) As Integer
Dim organ As Long
Dim uom As String, endpt() As String, rte() As String, Path() As String
Dim numpt As Long, numrte As Long, numpath As Long
Dim cas As String, pcas As String
Dim ntimes As Long, cursel As Long
Dim rtimes() As Single, rvalues() As Single
Dim j As Long, k As Long, l As Long, m As Long, n As Long
Dim locName As String, medium As String, agestr As String, ptstr As String

ReDim ttimes(0): ReDim tvalues(0)

  hifChartGetCriteria Col, locName, medium, agestr, ptstr, organ, uom, endpt(), rte(), Path(), cas
  
  cas = IIf(nprog > 0, allCont(ncon).pcas(nprog), allCont(ncon).cas)
  pcas = allCont(ncon).pcas(0)
  
  For j = 1 To UBound(endpt)
    For k = 1 To UBound(rte)
      For l = 1 To UBound(Path)
        ntimes = hifReadTimeSeries(nd, age, pcas, npt, organ, endpt(j), uom, rte(k), Path(l), cas)
        If ntimes > 0 Then
          ReDim rvalues(ntimes): ReDim rtimes(ntimes)
          hifGetTimeSeriesArray nd, rtimes(1), rvalues(1)
          If 0 = UBound(ttimes) Then
            ReDim ttimes(ntimes): ReDim tvalues(ntimes)
            For m = 1 To ntimes
              ttimes(m) = rtimes(m)
            Next m
          End If
          For m = 1 To ntimes
            If ttimes(m) = rtimes(m) Then
              tvalues(m) = tvalues(m) + rvalues(m)
            Else
              MsgBox "Times returned by hifGetTimeSeriesArray do not match"
              hifGetSeries = 0
              Exit Function
            End If
          Next m
        End If
      Next l
    Next k
  Next j
  hifGetSeries = ntimes
End Function

Function epfGetSeries(Col As Long, rtimes() As Single, rvalues() As Single, nprog As Long)
  Dim retc As Long
  Dim nd As Long
  Dim casid As String
  Dim nxy As Long
  Dim epunit As String
  Dim endpt As String
  Dim organ As Long
  Dim rte As String
  Dim Path As String
  Dim age As Long
  Dim i As Integer
  Dim Row As Integer
  Dim var As Variant
  Dim nvalues As Long
  Dim r As Integer
  Dim nrow As Integer
  Dim pcas As String
  Dim ncon As Integer
  Dim uom As String
  
  vaSpread1.Col = Col
  vaSpread1.Row = CBO_DS
  nd = vaSpread1.TypeComboBoxCurSel
  
  vaSpread1.Row = CBO_CONT
  ncon = vaSpread1.TypeComboBoxCurSel + 1
  If nprog > 0 Then casid = allCont(ncon).pcas(nprog) Else casid = allCont(ncon).cas
  pcas = allCont(ncon).pcas(0)
  
  vaSpread1.Row = CBO_AGE
  age = vaSpread1.TypeComboBoxCurSel
  
  vaSpread1.Row = CBO_XY
  nxy = vaSpread1.TypeComboBoxCurSel
  
  vaSpread1.Row = CBO_RTE
  rte = vaSpread1.Text
  
  vaSpread1.Row = CBO_PATH
  Path = vaSpread1.Text
  
  nvalues = epfReadTimeSeries(nd, casid, nxy, rte, Path, pcas)
  ReDim rvalues(nvalues)
  ReDim rtimes(nvalues)
  
  For i = 0 To nvalues - 1
    epfGetTimeSeries nd, i, rtimes(i + 1), rvalues(i + 1)
  Next
  
  epfGetSeries = nvalues
End Function

Public Sub CumulativeTotal(cvalues() As Single, ttimes() As Single, tvalues() As Single)
    Dim tmass As Single, massA As Single, massB As Single, dt As Single
    Dim r As Long
    
    For r = 1 To UBound(tvalues)
      If r = 1 Then
        cvalues(r) = tvalues(r)
      Else
        cvalues(r) = tvalues(r) + cvalues(r - 1)
      End If
    Next r
    Exit Sub
    
    ReDim cvalues(UBound(tvalues))
    For r = 1 To UBound(tvalues)
      massA = 0#
      massB = 0#
      tmass = 0#
      dt = ttimes(r)
      If (r > 1) Then
        dt = dt - ttimes(r - 1)
        massA = massA + tvalues(r - 1)
      End If
      massB = massB + tvalues(r)
      If (r > 1) Then
        If (dt > 1# Or dt < 1#) Then
          tmass = 0.5 * dt * (massA + massB)
        Else
          tmass = massB
        End If
        tmass = cvalues(r - 1) + tmass
      End If
      cvalues(r) = tmass
    Next

End Sub

Private Sub CreateChart()
  Dim i As Long, c As Long, r As Long, var As Variant
  Dim drow As Long, nrow As Long, ncol As Long
  Dim yaxis As String
  Dim range As String
  Dim off As Long
  Dim Title As String
  Dim numrow As Long, numcol As Long, nhdr As Long
  Dim varray(), vstr As Variant
  Dim crit As String, cas As String, ncont As Long, nprog As Long
  Dim rtimes() As Single, rvalues() As Single, tot As Double
  Dim nvalues As Long
  Dim pcol As Long
  Dim cTime(), cValue(), tvalue(), xrange As String, yrange As String
  Dim xlabel As String, ylabel As String, x2label As String, tlabel As String
  Dim wso As Object, cho As Object
  Dim hif As Boolean
  Dim inctot() As Double
  Dim locName As String, medium As String, agestr As String, ptstr As String
  Dim x As Long, y As Long, agemin As Single, agemax As Single
  Dim scsv As Variant
  
  hif = (pdcfType = "hif")
  numrow = vaSpread1.DataRowCnt
  numcol = txtCount
  nhdr = numHDR + 2
  
  If numrow - nhdr = 0 Then
    MsgBox "There are no results." & vbCrLf & "Please make another selection.", , "Note"
    Exit Sub
  End If
  
  frmView.MousePointer = vbHourglass
  
  If Not GetWorkbookObject() Then
    MsgBox "Error getting Excel workbook object"
    Exit Sub
  End If
  
  On Error GoTo command2_error
  
  For ncol = 0 To numcol - 1
    ReDim inctot(1 To (numrow - nhdr), 1 To 1)
    off = (ncol * 2) + 1
    vaSpread1.Col = off + 1
    
    vaSpread1.Row = CBO_DS:  locName = vaSpread1.Text
    vaSpread1.Row = CBO_MED: medium = vaSpread1.Text
    vaSpread1.Row = CBO_AGE: agestr = vaSpread1.Text
    vaSpread1.Row = CBO_XY:  ptstr = vaSpread1.Text
    
    If ptstr <> ALLITEMS Then
      scsv = Split(ptstr, " ")
      x = scsv(1): y = scsv(3)
    End If
    If agestr <> ALLITEMS Then
      scsv = Split(agestr, " ")
      If 2 = UBound(scsv) Then
        agemin = CSng(scsv(0)): agemax = CSng(scsv(2))
      End If
    End If
      
    ncont = GetContIndexFromSpread(crit)
    If ncont > 0 Then nprog = allCont(ncont).nprog
  
    Set wso = GetWorkSheetObject(WorkSheetName(crit + " (#" + CStr(ncol + 1) + ")"))
    Set cho = GetChartObject(wso)
    
    xlsSetCell wso, 1, 1, "File:": xlsSetCell wso, 2, 1, FUIName + "." + pdcfType
    xlsSetCell wso, 1, 2, "Module:": xlsSetCell wso, 2, 2, modLabel + " (" + ModName + ")"

    For r = 1 To nhdr
      vaSpread1.GetText off, r, vstr:      xlsSetCell wso, 1, r + 3, vstr: xlabel = vstr
      vaSpread1.GetText off + 1, r, vstr:  xlsSetCell wso, 2, r + 3, vstr: ylabel = vstr
      If hif And r = CBO_EP Then Title = vstr
    Next r
    
    xlsSetCell wso, 1, nhdr + 4, "Start Time"
    If nprog > 0 Or crit = ALLRADS Or crit = ALLCHEMS Or hif Then
      tlabel = "Total": xlsSetCell wso, 2, nhdr + 4, tlabel
      If hif Then
        xlsSetCell wso, 3, nhdr + 3, ylabel
      End If
    Else
      xlsSetCell wso, 2, nhdr + 4, allCont(ncont).name
      tlabel = allCont(ncont).cas: xlsSetCell wso, 2, nhdr + 5, tlabel
    End If
    
    ReDim cTime(1 To (numrow - nhdr), 1 To 1)
    ReDim cValue(1 To (numrow - nhdr), 1 To 1)
    ReDim tvalue(1 To (numrow - nhdr), 1 To 1)
    
    For r = 1 To UBound(cTime)
      ' parents (plus progeny - if any) or "All Chemicals" or "All Radionuclides", etc.
      vaSpread1.GetText off, r + nhdr, vstr:  cTime(r, 1) = val(vstr)
      If Not hif Then
        vaSpread1.GetText off + 1, r + nhdr, vstr: tvalue(r, 1) = val(vstr)
      End If
    Next r
    xrange = xlsSetRange(wso, 1, (nhdr + 4 + 3), cTime())
    pcol = 2
    
    Dim nd As Long, age As Long, npt As Long
    For nd = 1 To numds
      If ds(nd).locName = locName Or locName = ALLITEMS Then
        If ds(nd).medtype = medium Or medium = ALLITEMS Then
          For age = 1 To ds(nd).numage
            If agestr = ALLITEMS Or (ds(nd).agemin(age) = agemin And ds(nd).agemax(age) = agemax) Then
              For npt = 1 To ds(nd).numpt
                If ptstr = ALLITEMS Or (ds(nd).x(npt) = x And ds(nd).y(npt) = y) Then
                  For i = 1 To UBound(allCont)
                    If (crit = ALLRADS And allCont(i).ktype = 1) Or _
                         (crit = ALLCHEMS And allCont(i).ktype > 1) Or ncont = i Then
                      nprog = allCont(i).nprog
                      If crit = ALLRADS Or crit = ALLCHEMS Or (ncont = i And nprog > 0) Or hif Then
                        tot = 0
                        For c = 0 To nprog
                          Select Case pdcfType
                            Case "epf":
                              nvalues = epfGetSeries(CLng(off + 1), rtimes(), rvalues(), c)
                            Case "rif":
                              nvalues = rifGetSeries(CLng(off + 1), rtimes(), rvalues(), c)
                            Case "hif":
                              nvalues = hifGetSeries(CLng(off + 1), rtimes(), rvalues(), c, i, nd - 1, age - 1, npt - 1)
                          End Select
                          If hif And (crit = ALLRADS Or crit = ALLCHEMS) Then
                            For r = 1 To nvalues
                              cValue(r, 1) = IIf(c = 0, rvalues(r), cValue(r, 1) + rvalues(r))
                              tot = tot + cValue(r, 1)
                            Next
                            If c = nprog Then
                              pcol = pcol + 1
                              If locName = ALLITEMS Then xlsSetCell wso, pcol, CBO_DS + 3, ds(nd).locName
                              If medium = ALLITEMS Then xlsSetCell wso, pcol, CBO_MED + 3, ds(nd).medtype
                              If agestr = ALLITEMS Then xlsSetCell wso, pcol, CBO_AGE + 3, ds(nd).agemin(age) & " to " & ds(nd).agemax(age)
                              If ptstr = ALLITEMS Then xlsSetCell wso, pcol, CBO_XY + 3, "'( " & ds(nd).x(npt) & ", " & ds(nd).y(npt) & " )"
                              xlsSetCell wso, pcol, nhdr + 3, ylabel
                              xlsSetCell wso, pcol, nhdr + 4, allCont(i).name
                              xlsSetCell wso, pcol, nhdr + 5, allCont(i).pcas(0)
                              yrange = xlsSetRange(wso, pcol, (nhdr + 4 + 3), cValue())
                              If tot > 0 Then
                                AddSeriesToChart wso, cho, allCont(i).pcas(c) & IIf(hif, " (" & excelRange(pcol) & ")", ""), xrange, yrange
                                For r = 1 To nvalues
                                  inctot(r, 1) = inctot(r, 1) + CDbl(cValue(r, 1))
                                Next
                              End If
                            End If
                          Else
                            tot = 0
                            For r = 1 To nvalues
                              cValue(r, 1) = rvalues(r)
                              tot = tot + cValue(r, 1)
                            Next
                            pcol = pcol + 1 + IIf(hif And nprog > 0 And pcol = 2, 0, 0)
                            If locName = ALLITEMS Then xlsSetCell wso, pcol, CBO_DS + 3, ds(nd).locName
                            If medium = ALLITEMS Then xlsSetCell wso, pcol, CBO_MED + 3, ds(nd).medtype
                            If agestr = ALLITEMS Then xlsSetCell wso, pcol, CBO_AGE + 3, ds(nd).agemin(age) & " to " & ds(nd).agemax(age)
                            If ptstr = ALLITEMS Then xlsSetCell wso, pcol, CBO_XY + 3, "'( " & ds(nd).x(npt) & ", " & ds(nd).y(npt) & " )"
                            xlsSetCell wso, pcol, nhdr + 3, ylabel
                            xlsSetCell wso, pcol, nhdr + 4, IIf(c = 0, allCont(i).name, allCont(i).pname(c))
                            xlsSetCell wso, pcol, nhdr + 5, allCont(i).pcas(c)
                            yrange = xlsSetRange(wso, pcol, (nhdr + 4 + 3), cValue())
                            If tot > 0 Then
                              AddSeriesToChart wso, cho, allCont(i).pcas(c) & IIf(hif, " (" & excelRange(pcol) & ")", ""), xrange, yrange
                              For r = 1 To nvalues
                                inctot(r, 1) = inctot(r, 1) + CDbl(cValue(r, 1))
                              Next
                            End If
                          End If
                        Next c
                        tlabel = IIf(pcol > 2, "Total", allCont(i).pcas(0))
                      End If
                    End If
                  Next i
                End If
              Next npt
            End If
          Next age
        End If
      End If
    Next nd
    
    If Not hif Then
      yrange = xlsSetRange(wso, 2, (nhdr + 4 + 3), tvalue())
      AddSeriesToChart wso, cho, tlabel, xrange, yrange, IIf(pcol > 2, 2, 1)
    Else
      If pcol > 3 Then
        ' skip this cumulative output if only one column of detail
        xlsSetCell wso, 2, nhdr + 3, ylabel
        xlsSetCell wso, 2, nhdr + 4, "Total"
        xlsSetCell wso, 2, nhdr + 5, "Impact"
        ' precision can be lost in the variant so copy from double to variant for xlsSetRange
        For r = 1 To UBound(inctot)
          tvalue(r, 1) = inctot(r, 1)
        Next r
        yrange = xlsSetRange(wso, 2, (nhdr + 4 + 3), tvalue())
        AddSeriesToChart wso, cho, "Total", xrange, yrange, 1
      Else
        ' clear cumulative column headings
        xlsSetCell wso, 2, nhdr + 3, "": xlsSetCell wso, 2, nhdr + 4, "": xlsSetCell wso, 2, nhdr + 5, ""
      End If
    End If
    
    Select Case pdcfType
      Case "hif":
        vaSpread1.GetText off, CBO_EP, vstr: Title = vstr
        vaSpread1.GetText off + 1, CBO_EP, vstr: Title = Title + ": " + vstr
        vaSpread1.GetText off + 1, CBO_UOM, vstr: Title = Title + " (" + vstr + ")"
'       Select Case cas
'         Case ALLRADS, ALLCHEMS:
'           title = title + Chr(10) + crit
'         Case Else
            vaSpread1.GetText off + 1, CBO_CONT, vstr
            Title = Title + Chr(10) + vstr
'       End Select
        vaSpread1.GetText off + 1, CBO_RTE, vstr:  Title = Title + Chr(10) + "Route: " + vstr
        vaSpread1.GetText off + 1, CBO_PATH, vstr: Title = Title + "   Pathway: " + vstr
        vaSpread1.GetText off + 1, CBO_EP, vstr:   yaxis = vstr ' yaxis = ""
        vaSpread1.GetText off + 1, CBO_UOM, vstr:  yaxis = yaxis + " (" + vstr + ")"
        CompleteChartObject cho, Title, "Start Time " + xlabel, yaxis, ""
      Case "rif":
        Title = "Receptor Intake"
        Title = Title + Chr(10) + allCont(ncont).name + " (" + allCont(ncont).cas + ")"
        vaSpread1.GetText off + 1, CBO_RTE, vstr:  Title = Title + Chr(10) + "Route: " + vstr
        vaSpread1.GetText off + 1, CBO_PATH, vstr: Title = Title + "   Pathway: " + vstr
        vaSpread1.GetText off + 1, CBO_EP, vstr:   ylabel = vstr
        vaSpread1.GetText off + 1, CBO_UOM, vstr:  ylabel = ylabel + " (" + vstr + ")"
        CompleteChartObject cho, Title, "Start Time " + xlabel, ylabel, IIf(nprog > 0, "Total (" + vstr + ")", "")
      Case "epf":
        Title = "Exposure Concentration"
        Title = Title + Chr(10) + allCont(ncont).name + " (" + allCont(ncont).cas + ")"
        vaSpread1.GetText off + 1, CBO_RTE, vstr:  Title = Title + Chr(10) + "Route: " + vstr
        vaSpread1.GetText off + 1, CBO_PATH, vstr: Title = Title + "   Pathway: " + vstr
        yaxis = Left(expRoute(1).unit, InStr(expRoute(1).unit, "/") - 1)
        CompleteChartObject cho, Title, "Start Time " + xlabel, "Concentration " + ylabel, IIf(nprog > 0, "Total " + ylabel, "")
    End Select
    
  Next ncol
  frmView.MousePointer = vbDefault
  Exit Sub

command2_error:
  MsgBox "Error: " + Error, vbOKOnly, "frmView.frm:Command2_Click"
  frmView.MousePointer = vbDefault
  Exit Sub

End Sub
