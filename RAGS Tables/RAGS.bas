Attribute VB_Name = "RAGS"
Option Explicit
Option Compare Text

Public Cancel As Boolean
Public hLibModule As Long
Public ProcessEvents As Boolean

Public Declare Function LoadLibrary Lib "kernel32" Alias "LoadLibraryA" (ByVal lpLibFileName As String) As Long
Public Declare Function FreeLibrary Lib "kernel32" (ByVal hLibModule As Long) As Long
Public Declare Function SetWindowPos Lib "user32" (ByVal hWnd As Long, ByVal hWndInsertAfter As Long, ByVal x As Long, ByVal y As Long, ByVal cx As Long, ByVal cy As Long, ByVal wFlags As Long) As Long
Public Declare Function GetModuleFileName Lib "kernel32" Alias "GetModuleFileNameA" (ByVal hModule As Long, ByVal lpFileName As String, ByVal nSize As Long) As Long
Public Declare Function GetSystemMetrics Lib "user32" (ByVal nIndex As Long) As Long
Public Const SM_CXVSCROLL = 2

Public Const ALPHA As String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
Public Const ixTSTART = 0
Public Const ixTEND = 1
Public Const ixMIN = 2
Public Const ixMAX = 3
Public Const ixSTEP = 4
Public Const ixMEAN = 5
Public Const ixMEDIAN = 6
Public Const ixMIDPOINT = 7

Public Const xlNone = -4142
Public Const xlDiagonalDown = 5
Public Const xlDiagonalUp = 6
Public Const xlEdgeLeft = 7
Public Const xlEdgeTop = 8
Public Const xlEdgeBottom = 9
Public Const xlEdgeRight = 10
Public Const xlDouble = -4119
Public Const xlThick = 4
Public Const xlAutomatic = -4105
Public Const xlCenter = -4108
Public Const xlBottom = -4107
Public Const xlUnderlineStyleNone = -4142
Public Const xlRight = -4152
Public Const xlInsideVertical = 11

Public Const NA = "NA"
Public Const NOTAPP = "NA"
Public Const TITLE_RME = "REASONABLE MAXIMUM EXPOSURE"
Public Const TITLE_CT = "CENTRAL TENDENCY"
Public Const SN_FORMAT = "0.0##E+00"
Public Const TIMESTRING_LEN = 8192 ' size of string passed to LoadStartTimes

  ' rows 1=constituent, 2=exposure, 3=medium, 4=grand total
Public Const ST_KTYPE = -1
Public Const ST_CONSTITUENT = 0
Public Const ST_EXPOSUREROUTE = 1
Public Const ST_EXPOSUREPOINT = 2
Public Const ST_EXPOSUREMEDIUM = 3
Public Const ST_MEDIUM = 4
Public Const ST_ALLMEDIA = 5

Public whichTables As String

Public advise() As String
Public Title As String
Public Template As String

Dim colMedia As Collection ' the collection of all media, exp med, exp pt, exp rt... etc for tables
Dim objMedium As DatasetCls
Dim objExpMed As ExpMedCls
Dim objExpPt As ExpPtCls
Dim objExpRt As ExpRtCls
Dim objAge As AgeCls

Public cont As Collection
Public epfds As Collection
Public rifds As Collection
Public hifds As Collection
Public csm As Collection

Public epfGlyph As CsmCls
Public rifGlyph As CsmCls
Public hifglyph As CsmCls

' problems occur with early binding and type libraries are not redistributable
Public xlo As Object ' Excel.Application
Public wbo As Object ' Excel.Workbook
Public mainform As Form

Public AppPath As String
Public SiteName As String

Public mfCT As Boolean
Public mfDeMin As String
Public mfMin As Boolean
Public mfMax As Boolean
Public mfMidpoint As Boolean
Public mfMedian As Boolean
Public mfMean As Boolean
Public mfThreshold As Integer
Public mfPE As Boolean

Public measureOfInterest As Integer
Public Const mct = 1
Public Const mRME = 2
Public Const mMIN = 3
Public Const mMAX = 4
Public Const mMID = 5
Public Const mMED = 6
Public Const mMN = 7
Public Const mPE = 8
Public mTitle As String

Public hifSeries As New Collection

Public t9 As MSComctlLib.TreeView

Public Const SCFMT = "0.000E+00"

Sub EndRAGS()
  On Error Resume Next
  close_csv errfile
  epfClose
  rifClose
  hifClose
  Set wbo = Nothing
  Set xlo = Nothing
  Close 'all files
  If Not AnError Then Kill RunName & ".ERR"
  If 0 <> hLibModule Then
    FreeLibrary hLibModule
  End If
  End
End Sub

Sub PutLog(myError As String)
  put_val errfile, myError
  put_line errfile
End Sub

Public Function StartExcel() As Boolean
Dim fName As String

  On Error Resume Next

  fName = Left(FUIName, InStr(FUIName, ".gid") - 1) + ".rags.xls"
  

  PutLog "GetObject excel.application"
  Set xlo = GetObject(, "excel.application")
  If Err <> 0 Then
    On Error GoTo StartExcel_Error
    PutLog "CreateObject excel.applications"
    Set xlo = CreateObject("excel.application")
  End If
  

  xlo.Visible = True
  
  For Each wbo In xlo.workbooks
    Debug.Print wbo.Name, wbo.fullname
    If wbo.fullname = fName Then
      wbo.Close SaveChanges:=False
    End If
  Next wbo
  
  On Error Resume Next
  
  PutLog "FileCopy from " & Template ' AppPath + "\" + "rags_templates.xls"
  PutLog "FileCopy to " & fName
  
  FileCopy Template, fName ' AppPath + "\" + "rags_templates.xls", fname
  If Err.Number <> 0 Then
    MsgBox "Permission denied writing from " & Template & " to " & fName
    PutLog "Permission denied writing to " & fName
    StartExcel = False
    Err.Clear
    Exit Function
  End If
  
  PutLog "Set wbo = xlo.workbooks.open(" & fName & ")"
  Set wbo = xlo.workbooks.Open(fName)
  PutLog "StartExcel is OK"
  StartExcel = True
StartExcel_Error:
  If Err.Number <> 0 Then
    MsgBox Err.Description, vbOKOnly, "StartExcel"
    StartExcel = False
  End If
End Function

Function xlsGetCell(obj As Object, col&, row&)
On Error GoTo ErrorHandler
  xlsGetCell = obj.cells(row, col).value
ErrorHandler:
  If Err.Number <> 0 Then
    PutError "xlsGetCell " & col & ", " & row & ": " & Err.Description
  End If
End Function

Sub xlsSetCell(obj As Object, col As Variant, row As Variant, value, Optional fmt As String = "")
On Error GoTo ErrorHandler
  obj.range(xlo.cells(row, col), xlo.cells(row, col)).value = value
  If fmt <> "" Then
    obj.range(xlo.cells(row, col), xlo.cells(row, col)).numberformat = fmt
  End If
ErrorHandler:
  If Err.Number <> 0 Then
    PutError "xlsSetCell " & col & ", " & row & ", " & value & ", " & fmt & ": " & Err.Description
  End If
End Sub


Function xlsSetRange(obj As Object, col&, row&, value()) As String
Dim range As String
On Error GoTo ErrorHandler
  range = excelRange(col, row, col + UBound(value, 2) - 1, row + UBound(value, 1) - 1)
  obj.range(range).value = value
  xlsSetRange = range
ErrorHandler:
  If Err.Number <> 0 Then
    PutError "xlsSetCell " & col & ", " & row & ": " & Err.Description
  End If
End Function


Function excelRange(col1, row1, Optional col2 = -1, Optional row2 = -1) As String
Dim s As String, n1 As Integer, n2 As Integer
  n1 = col1 Mod 26
  n2 = Int(col1 / 26)
  If n2 > 0 Then s = Mid$(ALPHA, n2, 1)
  s = s & Mid$(ALPHA, n1, 1)
  s = s & row1
  If col2 > 0 Then
    s = s & ":"
    n1 = col2 Mod 26
    n2 = Int(col2 / 26)
    If n2 > 0 Then s = s & Mid$(ALPHA, n2, 1)
    s = s & Mid$(ALPHA, n1, 1)
    s = s & row2
  End If
  excelRange = s
End Function

Public Function PopDesc() As String
  PopDesc = "1"
End Function

Public Function OnOffSite() As String
  OnOffSite = "Off-Site"
End Function

Public Sub CheckRow(wso As Object, row As Long, label As String)
  While wso.range(label).row <= row
    wso.range(label).EntireRow.Insert
  Wend
End Sub

Public Function TimeFrame() As String
Dim ntimes As Long, timestring As String, i As Long, l As Long
Static tf As String
  
  Dim t1 As Double
  Dim t2 As Double
  Dim valu As Double
  
  If tf = "" Then
    ntimes = hifGetTimeCount(0, 0, cont.item(1).cas)
    t1 = hifGetTime(0, 0, cont.item(1).cas, 0)
    t2 = hifGetTime(0, 0, cont.item(1).cas, ntimes - 1)
    tf = CStr(t1) & "-" & CStr(t2) & " yr"
  End If
  TimeFrame = tf ' "0-1000 yr"
End Function


Public Sub FormatTable9Totals(wso As Object, col As Long, row As Long)
Dim r As Object
  Set r = wso.cells(row, col)
  r.Select
  With r
    .borders(xlDiagonalDown).linestyle = xlNone
    .borders(xlDiagonalUp).linestyle = xlNone
    With .borders(xlEdgeLeft)
        .linestyle = xlDouble
        .Weight = xlThick
        .ColorIndex = xlAutomatic
    End With
    With .borders(xlEdgeTop)
        .linestyle = xlDouble
        .Weight = xlThick
        .ColorIndex = xlAutomatic
    End With
    With .borders(xlEdgeBottom)
        .linestyle = xlDouble
        .Weight = xlThick
        .ColorIndex = xlAutomatic
    End With
    With .borders(xlEdgeRight)
        .linestyle = xlDouble
        .Weight = xlThick
        .ColorIndex = xlAutomatic
    End With
    .numberformat = "@"
    .HorizontalAlignment = xlCenter
    .VerticalAlignment = xlBottom
    .WrapText = False
    .Orientation = 0
    .AddIndent = False
    .ShrinkToFit = False
    .MergeCells = False
    With .Font
        .Name = "Arial"
        .FontStyle = "Regular"
        .size = 8
        .Strikethrough = False
        .superscript = False
        .Subscript = False
        .OutlineFont = False
        .Shadow = False
        .Underline = xlUnderlineStyleNone
        .ColorIndex = xlAutomatic
    End With
  End With
  Set r = wso.cells(row, col - 1)
  With r
    .numberformat = "@"
    .HorizontalAlignment = xlRight
    .VerticalAlignment = xlBottom
    .WrapText = False
    .Orientation = 0
    .AddIndent = False
    .ShrinkToFit = False
    .MergeCells = False
    With .Font
        .Name = "Arial"
        .FontStyle = "Regular"
        .size = 8
        .Strikethrough = False
        .superscript = False
        .Subscript = False
        .OutlineFont = False
        .Shadow = False
        .Underline = xlUnderlineStyleNone
        .ColorIndex = xlAutomatic
    End With
  End With
  
End Sub

Public Sub Table1()
Dim rc As Long, i As Long, Route As String, Path As String, Unit As String
Dim pw As String
Dim cmin As Double, cmax As Double, tmin As Double, tmax As Double
Dim wso As Object, ds As DatasetCls, con As ContamCls, rt As ExpRtCls, rt2 As ExpRtCls
Dim nc As Long, row As Long, col As Long, rn As Long, ep As ExpPtCls, ag As AgeCls
Dim nd As Long
Dim exppt As String
Dim exprt As String

Dim objMedium As DatasetCls
Dim objExpMed As ExpMedCls
Dim objExpPt As ExpPtCls
Dim objExpRt As ExpRtCls
Dim objAge As AgeCls

  PutLog "Table1"
  
  On Error Resume Next

  ' Get the unique collections for table summaries

  Set colMedia = New Collection

  For nd = 1 To hifds.count
    Set ds = hifds.item(nd)

    Set objMedium = colMedia(ds.medtype)
    If Err.Number <> 0 Then
      Err.Clear
      Set objMedium = New DatasetCls
      objMedium.medtype = ds.medtype
      colMedia.Add objMedium, objMedium.medtype
    End If
    For Each ag In ds.age
      Set objAge = objMedium.age(ag.Desc)
      If Err.Number <> 0 Then
        Err.Clear
        Set objAge = New AgeCls
        objAge.min = ag.min
        objAge.max = ag.max
        objMedium.age.Add objAge, objAge.Desc
      End If
    Next ag
    For Each rt In ds.exprt
      Set objExpMed = objMedium.expmed(rt.pathway)
      If Err.Number <> 0 Then
        Err.Clear
        Set objExpMed = New ExpMedCls
        objExpMed.label = rt.pathway
        objMedium.expmed.Add objExpMed, objExpMed.label
      End If
      For Each ep In ds.exppt
        Set objExpPt = objExpMed.exppt(ep.Desc) ' ep.label
        If Err.Number <> 0 Then
          Err.Clear
          Set objExpPt = New ExpPtCls
          objExpPt.label = ep.label
          objExpMed.exppt.Add objExpPt, objExpPt.Desc ' label
        End If
        For Each rt2 In ds.exprt
          If rt2.pathway = objExpMed.label Then
            Set objExpRt = objExpPt.exprt(rt2.Route)
            If Err.Number <> 0 Then
              Err.Clear
              objExpPt.exprt.Add rt2, rt2.Route
            End If
          End If
        Next rt2
      Next ep
    Next
  Next nd

  SiteName = UCase(Left(FUIName, InStr(FUIName, ".gid") - 1))
  
  Set wso = wbo.sheets("BLANK1")
  wso.Activate
  wso.Name = "Table 1"
  wso.range("T1_SITENAME").value = SiteName
  row = wso.range("T1_MEDIUM").row
  col = wso.range("T1_TIME").Column: xlsSetCell wso, col, row, TimeFrame()
  
  For Each objMedium In colMedia
    For Each objAge In objMedium.age
      CheckRow wso, row, "T1_LASTROW"
      col = wso.range("T1_MEDIUM").Column: xlsSetCell wso, col, row, objMedium.medtype
      For Each objExpMed In objMedium.expmed
        For Each objExpPt In objExpMed.exppt
            For Each objExpRt In objExpPt.exprt
              CheckRow wso, row, "T1_LASTROW"
              col = wso.range("T1_EXPMED").Column: xlsSetCell wso, col, row, objExpMed.label
              col = wso.range("T1_EXPPT").Column: xlsSetCell wso, col, row, objExpPt.label
              col = wso.range("T1_POP").Column: xlsSetCell wso, col, row, PopDesc()
              col = wso.range("T1_AGE").Column: xlsSetCell wso, col, row, objAge.Desc
              col = wso.range("T1_EXPRT").Column: xlsSetCell wso, col, row, objExpRt.Route
              col = wso.range("T1_ANAL").Column: xlsSetCell wso, col, row, "Quant"
              row = row + 1
            Next objExpRt
        Next objExpPt
      Next objExpMed
    Next objAge
    row = row + 1
  Next objMedium
  
  
End Sub

Public Sub Table4()
Dim rc As Long, i As Long, Route As String, Path As String, Unit As String
Dim pw As String, j As Long, k As Long
Dim cmin As Double, cmax As Double, tmin As Double, tmax As Double
Dim wso As Object, ds As DatasetCls, con As ContamCls, rt As ExpRtCls
Dim nc As Long, row As Long, col As Long, tn As Long
Dim nd As Long, ep As ExpPtCls, ag As AgeCls, rte As String, pathway As String
  
  PutLog "Table4"
  
  For Each objMedium In colMedia
    For Each objExpMed In objMedium.expmed
      tn = tn + 1
      Set wso = wbo.sheets("BLANK4")
      wso.Copy before:=wbo.sheets("BLANK4")
      Set wso = wbo.sheets("BLANK4 (2)")
      wso.Activate
      wso.Name = "Table 4." + CStr(tn)
      
      wso.range("T4_TABLENO").value = UCase(wso.Name) & " " & whichTables
      wso.range("T4_TITLE").value = mTitle
      wso.range("T4_SITENAME").value = SiteName
      wso.range("T4_TIME").value = TimeFrame()
      wso.range("T4_MEDIUM").value = objMedium.medtype
      wso.range("T4_EXPMED").value = objExpMed.label
      
      row = wso.range("T4_EXPRT").row

      For Each objAge In objMedium.age
         For Each objExpPt In objExpMed.exppt
           For Each objExpRt In objExpPt.exprt
              CheckRow wso, row, "T4_LASTROW"
              col = wso.range("T4_EXPPT").Column: xlsSetCell wso, col, row, objExpPt.label
              col = wso.range("T4_POP").Column: xlsSetCell wso, col, row, PopDesc()
              col = wso.range("T4_AGE").Column: xlsSetCell wso, col, row, objAge.Desc
        
              col = wso.range("T4_EXPRT").Column: xlsSetCell wso, col, row, objExpRt.Route
              col = wso.range("T4_PCODE").Column: xlsSetCell wso, col, row, "(1)"
              col = wso.range("T4_PDEF").Column: xlsSetCell wso, col, row, "(1)"
              col = wso.range("T4_UNIT").Column: xlsSetCell wso, col, row, "(1)"
              col = wso.range("T4_VALUE").Column: xlsSetCell wso, col, row, "(1)"
              col = wso.range("T4_REF").Column: xlsSetCell wso, col, row, "(1)"
              For Each ds In hifds
                If ds.medtype = objMedium.medtype Then
                  col = wso.range("T4_MODEL").Column: xlsSetCell wso, col, row, csm(rifGlyph.id).model
                  row = row + 1
                End If
              Next ds
           Next objExpRt
        Next objExpPt
      Next objAge
    Next objExpMed
  Next objMedium
  
  
  xlo.DisplayAlerts = False
  wbo.sheets("BLANK4").Delete
  xlo.DisplayAlerts = True
End Sub

Public Sub Table51()
Dim wso As Object, con As ContamCls
Dim row As Long, col As Long, parm As ParmCls
Dim rfd As Double, adj As Double
Dim ncon As Long

  PutLog "Table 5.1"
  
  Set wso = wbo.sheets("BLANK51")
  wso.Activate
  wso.Name = "Table 5.1"
  wso.range("T51_SITENAME").value = SiteName
  wso.range("T51_CNAME:T51_LASTROW").RowHeight = 22.5
  
  row = wso.range("T51_CNAME").row
  col = wso.range("T51_CNAME").Column
  For Each con In cont
    If con.ktype <> 1 Then
      ncon = ncon + 1
      CheckRow wso, row, "T51_LASTROW"
      col = wso.range("T51_CNAME").Column: xlsSetCell wso, col, row, con.cname
      col = wso.range("T51_CHRONIC").Column: xlsSetCell wso, col, row, "Chronic"
      ToxicityValues wso, con, row, "T51_RFDV", "T51_ADJF", "T51_ARFDV", "", ""
      col = wso.range("T51_ORGAN").Column: xlsSetCell wso, col, row, NA
      col = wso.range("T51_CF").Column: xlsSetCell wso, col, row, NA
      col = wso.range("T51_SRC").Column: xlsSetCell wso, col, row, NA
      col = wso.range("T51_DATE").Column: xlsSetCell wso, col, row, NA
      row = row + 1
    End If
  Next
  
  row = wso.range("T51_LASTROW").row: xlsSetCell wso, 1, row, "": row = row + 2
  If ncon > 0 Then
    xlsSetCell wso, 1, row, "NA = Not Available"
    
    row = row + 2: xlsSetCell wso, 1, row, "(1) Source: Ingestion Reference Dose, Water [CLRFDG]"
    row = row + 1: xlsSetCell wso, 1, row, "(2) Source: The GI absorption fraction, Insoluble [CLFONEI].  If not available, use 1"
    row = row + 1: xlsSetCell wso, 1, row, "(3) Equation: RFD * absorption efficiency"
    
    row = row + 2: xlsSetCell wso, 1, row, _
      "** NOTE:  If the user changes the modeled toxicity data, and it differs from the constituent database, this table should be updated to reflect the changes."
  Else
    xlsSetCell wso, 1, row, "There are no chemicals in this assessment.  As a result, this table is blank."
  End If
  
  With wso.range(excelRange(1, wso.range("T51_LASTROW").row + 1, 1, row))
    .indentlevel = 1
  End With
End Sub

Public Sub Table52()
Dim wso As Object, con As ContamCls
Dim row As Long, col As Long, parm As ParmCls
Dim rfc As Double, rfd As Double, adj As Double, Unit As String
Dim rfcu As String, rfdu As String
Dim ncon As Long

  PutLog "Table 5.2"

' The toxicity parameters for cancer risk are Inhalation Cancer Potency Factor
' (units are inverse of mg/kg/d) and the Inhalation Unit Risk Factor
' [units of risk per (microgram/m3)].  These are the parameters and units used
' in the chemical database.
'
' The relationship between the two parameters is as follows:
'
'   Unit risk (risk/microgram/m3) =
'       Cancer potency factor (risk/(mg/kg/d) * 20 m3/d / 70 kg * 0.001 mg/microgram
'
' If you are working with unit risk as risk per mg/m3, then you don't need the last factor
' converting from mg to micrograms.
'
' Inhalation Reference Concentration is used only for non-cancer impacts (i.e. hazard quotient),
' and is related to the inhalation reference dose as follows:
'
'   Inhalation Reference Concentration (mg/m3) =
'       Inhalation Reference Dose (mg/kg/d) * 70 kg / 20 m3/day.
'
' Dennis Strenge

  Set wso = wbo.sheets("BLANK52")
  wso.Activate
  wso.Name = "Table 5.2"
  wso.range("T52_SITENAME").value = SiteName
  wso.range("T52_CNAME:T52_LASTROW").RowHeight = 22.5
  
  row = wso.range("T52_CNAME").row
  col = wso.range("T52_CNAME").Column
  For Each con In cont
    If con.ktype <> 1 Then
    
      ncon = ncon + 1
      CheckRow wso, row, "T52_LASTROW"
      
      col = wso.range("T52_RFCV").Column: xlsSetCell wso, col, row, NA
      col = wso.range("T52_RFCU").Column: xlsSetCell wso, col, row, NA
      col = wso.range("T52_ARFDV").Column: xlsSetCell wso, col, row, NA
      col = wso.range("T52_ARFDU").Column: xlsSetCell wso, col, row, NA
      col = wso.range("T52_ORGAN").Column: xlsSetCell wso, col, row, NA
      col = wso.range("T52_CF").Column: xlsSetCell wso, col, row, NA
      col = wso.range("T52_SRC").Column: xlsSetCell wso, col, row, NA
      col = wso.range("T52_DATE").Column: xlsSetCell wso, col, row, NA
    
      col = wso.range("T52_CNAME").Column: xlsSetCell wso, col, row, con.cname
      col = wso.range("T52_CHRONIC").Column: xlsSetCell wso, col, row, "Chronic"
      col = wso.range("T52_RFCV").Column
      
      ToxicityValues wso, con, row, "", "", "", "T52_RFCV", "T52_ARFDV"
      
      row = row + 1
    End If
  Next
  
  row = wso.range("T52_LASTROW").row: xlsSetCell wso, 1, row, "": row = row + 2
  If ncon > 0 Then
    xlsSetCell wso, 1, row, "NA = Not Available"
    
    row = row + 2: xlsSetCell wso, 1, row, "(1) Source: Inhalation Reference Concentration [CLRFCH]"
    row = row + 1: xlsSetCell wso, 1, row, "            If NA then RfC(mg/m^3) = RfD(mg/kg/d) * 70(kg) / 20(m^3/d)"
    row = row + 1: xlsSetCell wso, 1, row, "(2) Source: Inhalation Reference Dose [CLRFDH]"
    row = row + 1: xlsSetCell wso, 1, row, "            If NA then RfD(mg/kg/d) = RfC(mg/m^3) * 20(m^3/d) / 70(kg) "
    
    row = row + 2: xlsSetCell wso, 1, row, _
      "** NOTE:  If the user changes the modeled toxicity data, and it differs from the constituent database, this table should be updated to reflect the changes."
  Else
    xlsSetCell wso, 1, row, "There are no chemicals in this assessment.  As a result, this table is blank."
  End If
  
  With wso.range(excelRange(1, wso.range("T52_LASTROW").row + 1, 1, row))
    .indentlevel = 1
  End With
End Sub


Public Sub Table53()
Dim i As Long, wso As Object, con As ContamCls
Dim row As Long, col As Long

  PutLog "Table 5.3"
  
  Set wso = wbo.sheets("BLANK53")
  wso.Activate
  wso.Name = "Table 5.3"
  wso.range("T53_SITENAME").value = SiteName
  wso.range("T53_CNAME:T53_LASTROW").RowHeight = 22.5
  
  row = wso.range("T53_LASTROW").row + 2
  col = wso.range("T53_LASTROW").Column
  xlsSetCell wso, col, row, "There are no special case chemicals in this assessment.  As a result, this table is blank."
  With wso.range(excelRange(col, row, col, row))
    .indentlevel = 1
  End With
End Sub

Public Sub Table61()
Dim wso As Object, con As ContamCls
Dim row As Long, col As Long, parm As ParmCls
Dim csf As Double, adj As Double, Unit As String
Dim ncon As Long

  On Error GoTo ErrorHandler

  PutLog "Table 6.1"

  Set wso = wbo.sheets("BLANK61")
  wso.Activate
  wso.Name = "Table 6.1"
  wso.range("T61_SITENAME").value = SiteName
  wso.range("T61_CNAME:T61_LASTROW").RowHeight = 22.5
  
  row = wso.range("T61_CNAME").row
  For Each con In cont
    If con.ktype <> 1 Then
      ncon = ncon + 1
      CheckRow wso, row, "T61_LASTROW"
      
      col = wso.range("T61_CSF").Column: xlsSetCell wso, col, row, NA: xlsSetCell wso, col + 1, row, NA
      col = wso.range("T61_ADJF").Column: xlsSetCell wso, col, row, NA
      col = wso.range("T61_CSFA").Column: xlsSetCell wso, col, row, NA
      col = wso.range("T61_CSFV").Column: xlsSetCell wso, col, row, NA
      col = wso.range("T61_WOE").Column: xlsSetCell wso, col, row, NA
      col = wso.range("T61_SRC").Column: xlsSetCell wso, col, row, NA
      col = wso.range("T61_DATE").Column: xlsSetCell wso, col, row, NA
      
      col = wso.range("T61_CNAME").Column: xlsSetCell wso, col, row, con.cname
      
      SlopeFactors wso, con, row, "T61_CSF", "T61_ADJF", "T61_CSFA", ""
      
      row = row + 1
    End If
  Next
  
  row = wso.range("T61_LASTROW").row: xlsSetCell wso, 1, row, "": row = row + 2
  If ncon > 0 Then
    xlsSetCell wso, 1, row, "NA = Not Available"
    
    row = row + 2: xlsSetCell wso, 1, row, "(1) Source: Ingestion Cancer Potency Factor, Water [CLCPFG]"
    row = row + 1: xlsSetCell wso, 1, row, "(2) Source: The GI absorption fraction, Insoluble [CLFONEI].  If not available, use 1"
    row = row + 1: xlsSetCell wso, 1, row, "(3) Equation: Oral CSF [1] * absorption efficiency [2]"
    
    row = row + 2: xlsSetCell wso, 1, row, _
      "** NOTE:  If the user changes the modeled toxicity data, and it differs from the constituent database, this table should be updated to reflect the changes."
  Else
    xlsSetCell wso, 1, row, "There are no chemicals in this assessment.  As a result, this table is blank."
  End If
  
  With wso.range(excelRange(1, wso.range("T61_LASTROW").row + 1, 1, row))
    .indentlevel = 1
  End With
ErrorHandler:
  If Err.Number <> 0 Then
    MsgBox Err.Description, vbOKOnly + vbCritical, "Table 6.1"
  End If
End Sub

Public Sub Table62()
Dim wso As Object, con As ContamCls
Dim row As Long, col As Long, parm As ParmCls
Dim risk As Double, slope As Double, adj As Double, urisk As String, uslope As String
Dim ncon As Long

  On Error GoTo ErrorHandler

  PutLog "Table 6.2"

'  re: email from DLS to BLH 5/26/04
'  The toxicity parameters for cancer risk are Inhalation Cancer Potency Factor (units are inverse of mg/kg/d)
'  and the Inhalation Unit Risk Factor [units of risk per (microgram/m3)].  These are the parameters and units
'  used in the chemical database.
'  The relationship between the two parameters is as follows:
'    Unit risk (risk/microgram/m3) =
'        Cancer potency factor (risk/(mg/kg/d) * 20 m3/d / 70 kg * 0.001 mg/microgram

  Set wso = wbo.sheets("BLANK62")
  wso.Activate
  wso.Name = "Table 6.2"
  wso.range("T62_SITENAME").value = SiteName
  wso.range("T62_CNAME:T62_LASTROW").RowHeight = 22.5
  
  row = wso.range("T62_CNAME").row
 
  For Each con In cont
    If con.ktype <> 1 Then
      ncon = ncon + 1
      CheckRow wso, row, "T62_LASTROW"
      
      col = wso.range("T62_RISK").Column: xlsSetCell wso, col, row, NA: xlsSetCell wso, col + 1, row, NA
      col = wso.range("T62_CSF").Column: xlsSetCell wso, col, row, NA: xlsSetCell wso, col + 1, row, NA
      col = wso.range("T62_WOE").Column: xlsSetCell wso, col, row, NA
      col = wso.range("T62_SRC").Column: xlsSetCell wso, col, row, NA
      col = wso.range("T62_DATE").Column: xlsSetCell wso, col, row, NA
  
      col = wso.range("T62_CNAME").Column: xlsSetCell wso, col, row, con.cname
      
      Set parm = con.parm("CLURISKH"): risk = val(parm.pval): urisk = parm.cunit
      If risk > 0 Then
        col = wso.range("T62_RISK").Column: xlsSetCell wso, col, row, Format(risk, SN_FORMAT)
        col = wso.range("T62_UNIT").Column: xlsSetCell wso, col, row, urisk
      End If
      
      SlopeFactors wso, con, row, "", "", "", "T62_CSF"
      
      row = row + 1
    End If
  Next
  row = wso.range("T62_LASTROW").row: xlsSetCell wso, 1, row, "": row = row + 2
  If ncon > 0 Then
    xlsSetCell wso, 1, row, "NA = Not Available"
    
    row = row + 2: xlsSetCell wso, 1, row, "(1) Source: Inhalation Unit Risk Factor [CLURISKH]"
    row = row + 1: xlsSetCell wso, 1, row, "(2) Source: Inhalation Cancer Potency Factor [CLCPFH]"
    
    row = row + 2: xlsSetCell wso, 1, row, _
      "** NOTE:  If the user changes the modeled toxicity data, and it differs from the constituent database, this table should be updated to reflect the changes."
  Else
    xlsSetCell wso, 1, row, "There are no chemicals in this assessment.  As a result, this table is blank."
  End If
  
  With wso.range(excelRange(1, wso.range("T62_LASTROW").row + 1, 1, row))
    .indentlevel = 1
  End With
ErrorHandler:
  If Err.Number <> 0 Then
    MsgBox Err.Description, vbOKOnly + vbCritical, "Table 6.2"
  End If
End Sub


Public Sub Table63()
Dim wso As Object, con As ContamCls
Dim row As Long, col As Long

  PutLog "Table 6.3"

  Set wso = wbo.sheets("BLANK63")
  wso.Activate
  wso.Name = "Table 6.3"
  wso.range("T63_SITENAME").value = SiteName
  wso.range("T63_CNAME:T63_LASTROW").RowHeight = 22.5
  
  row = wso.range("T63_LASTROW").row + 2
  col = wso.range("T63_LASTROW").Column
  xlsSetCell wso, col, row, "There are no special case chemicals in this assessment.  As a result, this table is blank."
  With wso.range(excelRange(col, row, col, row))
    .indentlevel = 1
  End With
  
End Sub

Public Sub Table64()
Dim wso As Object, con As ContamCls
Dim row As Long, col As Long, parm As ParmCls
Dim risk As Double, slope As Double, adj As Double, urisk As String, uslope As String
Dim nrad As Long

  PutLog "Table 6.4"

  Set wso = wbo.sheets("BLANK64")
  wso.Activate
  wso.Name = "Table 6.4"
  wso.range("T64_SITENAME").value = SiteName
  wso.range("T64_CNAME:T64_LASTROW").RowHeight = 22.5
  
  row = wso.range("T64_CNAME").row
 
  For Each con In cont
    If (con.ktype = 1) Then
      ' radionuclides only
      nrad = nrad + 1
      CheckRow wso, row, "T64_LASTROW"
      col = wso.range("T64_CNAME").Column: xlsSetCell wso, col, row, con.cname
      col = wso.range("T64_CSF").Column: xlsSetCell wso, col, row, NA
      
      Set parm = con.parm("CLSFEX")
      slope = val(parm.pval): uslope = parm.cunit
      If slope > 0 Then
        col = wso.range("T64_CSF").Column: xlsSetCell wso, col, row, Format(slope, SN_FORMAT)
        col = col + 1: xlsSetCell wso, col, row, uslope
      End If
      
      col = wso.range("T64_SRC").Column: xlsSetCell wso, col, row, NA
      col = wso.range("T64_DATE").Column: xlsSetCell wso, col, row, NA
      row = row + 1
    End If
  Next
  row = wso.range("T64_LASTROW").row: xlsSetCell wso, 1, row, "": row = row + 2
  If nrad > 0 Then
    xlsSetCell wso, 1, row, "NA = Not Available"
    
    row = row + 2: xlsSetCell wso, 1, row, "(1) Source: External Slope Factor [CLSFEX]"
    
    row = row + 2: xlsSetCell wso, 1, row, _
      "** NOTE:  If the user changes the modeled toxicity data, and it differs from the constituent database, this table should be updated to reflect the changes."
  Else
    xlsSetCell wso, 1, row, "There are no radionuclides in this risk assessment.  As a result, this table is blank."
  End If
  
  With wso.range(excelRange(1, wso.range("T64_LASTROW").row + 1, 1, row))
    .indentlevel = 1
  End With
End Sub

Function getEpfExp(exp As Collection, rte As String, Path As String) As ExpRtCls
Dim rt As ExpRtCls

Set getEpfExp = Nothing
For Each rt In exp
  If rt.Route = rte And rt.pathway = Path Then
    Set getEpfExp = rt
    Exit Function
  End If
Next
End Function

Function getRifExp(exp As Collection, rte As String, Path As String, carc As Boolean, ktype As Long) As ExpRtCls
Dim rt As ExpRtCls

Set getRifExp = Nothing
For Each rt In exp
  If rt.Route = rte And rt.pathway = Path Then
    If carc Then
      If rt.measure = "intake" Or rt.measure = "carcinogenic" Then
        Set getRifExp = rt
        Exit Function
      End If
    Else
      If rt.measure = "noncarcinogenic" Then
        Set getRifExp = rt
        Exit Function
      End If
    End If
  End If
Next
End Function


Public Sub Table7A(tableid As Integer, tablesub As String, measure As String, ktype As Integer)
Dim pw As String, j As Long, k As Long ', l As Long
Dim cmin As Double, cmax As Double, tmin As Double, tmax As Double
Dim wso As Object, ds As DatasetCls, rs As DatasetCls, es As DatasetCls, con As ContamCls, rt As ExpRtCls, exp As ExpRtCls
Dim nc As Long, row As Long, col As Long, tn As Long
Dim nd As Long, ep As ExpPtCls, ag As AgeCls
Dim ref As String, rte As String, pathway As String
Dim nrt As Long
Dim locIdx As Long
Dim nage As Long
Dim ntimes As Long
Dim rows1 As Long, rows2 As Long, rows3 As Long
Dim TOTAL(ST_CONSTITUENT To ST_ALLMEDIA) As Double
Dim tvalue As Double
Dim cseries As Series
Dim key As String
Dim kunit As String
Dim parm As ParmCls
Dim ttitle As String
Dim borders As Collection
Dim ncon
Dim rtpw As ExpRtCls
Dim contpw As Collection
Dim tableno  As String

  tableno = tableid & tablesub
  
  PutLog "Table " & tableno & ", " & measure & ", " & ktype
  
  tn = 0
  nd = 0
  
  On Error Resume Next
  ' get collection of all unique age groups
  Dim ages As New Collection
  For Each objMedium In colMedia
    For Each ag In objMedium.age
      ages.Add ag, ag.Desc
    Next ag
  Next objMedium
  
  On Error GoTo ErrorHandler

    For Each ag In ages ' ds.age
      
      Set borders = New Collection
    
      For j = ST_ALLMEDIA To ST_CONSTITUENT Step -1
        TOTAL(j) = -99
      Next j
      
      nage = nage + 1
      tn = tn + 1
      
      Set wso = wbo.sheets("BLANK" & tableno)
      wso.Copy before:=wbo.sheets("BLANK" & tableno)
      Set wso = wbo.sheets("BLANK" & tableno & " (2)")
      wso.Activate
      wso.Name = "Table " & tableid & "." & CStr(tn) & tablesub
      
      wso.range(fmtCellLabel(tableno, "TITLE")) = mTitle
      
      wso.range(fmtCellLabel(tableno, "TABLENO")).value = wso.Name ' UCase(wso.name)
      wso.range(fmtCellLabel(tableno, "SITENAME")).value = SiteName
      
      wso.range(fmtCellLabel(tableno, "TIME")).value = TimeFrame()

      wso.range(fmtCellLabel(tableno, "POP")).value = PopDesc()
      wso.range(fmtCellLabel(tableno, "AGE")).value = ag.Desc
      
      row = wso.range(fmtCellLabel(tableno, "MEDIUM")).row
      
      
      For Each objMedium In colMedia
      
        For j = ST_MEDIUM To ST_CONSTITUENT Step -1
          TOTAL(j) = -99
        Next j
      
        col = wso.range(fmtCellLabel(tableno, "MEDIUM")).Column: xlsSetCell wso, col, row, objMedium.medtype
        
        For nd = 1 To hifds.count
          Set ds = hifds.item(nd)
          If ds.medtype = objMedium.medtype Then
            Set es = epfds.item(nd)
            Set rs = rifds.item(nd)
            Exit For
          End If
        Next nd

        For Each objExpMed In objMedium.expmed
          
          CheckRow wso, row, fmtCellLabel(tableno, "LASTROW")
          
          col = wso.range(fmtCellLabel(tableno, "EXPMED")).Column: xlsSetCell wso, col, row, objExpMed.label
          For j = ST_EXPOSUREMEDIUM To ST_CONSTITUENT Step -1
            TOTAL(j) = -99
          Next j
          
          For Each objExpPt In objExpMed.exppt
          
            CheckRow wso, row, fmtCellLabel(tableno, "LASTROW")
            col = wso.range(fmtCellLabel(tableno, "EXPPT")).Column: xlsSetCell wso, col, row, objExpPt.label
            
            For j = ST_EXPOSUREPOINT To ST_CONSTITUENT Step -1
              TOTAL(j) = -99
            Next j
            
            For Each objExpRt In objExpPt.exprt
            
              CheckRow wso, row, fmtCellLabel(tableno, "LASTROW")
              col = wso.range(fmtCellLabel(tableno, "EXPRT")).Column: xlsSetCell wso, col, row, objExpRt.Route
              
              For j = ST_EXPOSUREROUTE To ST_CONSTITUENT Step -1
                TOTAL(j) = -99
              Next j
              
              col = wso.range(fmtCellLabel(tableno, "CNAME")).Column
              xlsSetCell wso, col, row, "--"
              
              ncon = 0
              
              For Each con In cont
                If con.ktype = ktype Then
                
                  ncon = ncon + 1
                  CheckRow wso, row, fmtCellLabel(tableno, "LASTROW")
                  col = wso.range(fmtCellLabel(tableno, "CNAME")).Column
                  xlsSetCell wso, col, row, con.cname
                  
                  xlsSetCell wso, wso.range(fmtCellLabel(tableno, "EPC")).Column, row, NA
                  xlsSetCell wso, wso.range(fmtCellLabel(tableno, "EPC")).Column + 1, row, NA
                  col = wso.range(fmtCellLabel(tableno, "INTAKE")).Column
                  xlsSetCell wso, col, row, NA
                  xlsSetCell wso, col + 1, row, NA
                  xlsSetCell wso, col + 2, row, NA
                  xlsSetCell wso, col + 3, row, NA
                  xlsSetCell wso, col + 4, row, NA
                  
                  For nd = 1 To hifds.count
                  
                    ' find matching media in dataset collections
                    Set ds = hifds.item(nd)
                    If ds.medtype = objMedium.medtype Then
                      Set es = epfds.item(nd)
                      Set rs = rifds.item(nd)
                      
                      For locIdx = 1 To ds.exppt.count
                        If ds.exppt.item(locIdx).Desc() = objExpPt.Desc() Then Exit For
                      Next locIdx
                  
                      Set contpw = con.rtpw("epf")(CStr(nd))
                      
                    ' check that epf has this particular route and pathway
                      For Each rtpw In contpw
                        If rtpw.Route = objExpRt.Route And rtpw.pathway = objExpRt.pathway Then
                          rows1 = Table7A_EPF(wso, row, objMedium.medtype, locIdx - 1, rtpw.Route, rtpw.pathway, rtpw.Unit, con, tvalue, tableno)
                          Exit For
                        End If
                      Next rtpw
    
                      Select Case objExpRt.Route
                        Case "ingestion":
                          If tableno = "7A" Or tableno = "8" Then
                            SlopeFactors wso, con, row, fmtCellLabel(tableno, "CSF"), "", "", ""
                          Else
                            ToxicityValues wso, con, row, fmtCellLabel(tableno, "RFD"), "", "", "", ""
                          End If
                        Case "dermal":
                          If tableno = "7A" Or tableno = "8" Then
                            SlopeFactors wso, con, row, "", "", fmtCellLabel(tableno, "CSF"), ""
                          Else
                            ToxicityValues wso, con, row, "", "", fmtCellLabel(tableno, "RFD"), "", ""
                          End If
                        Case "inhalation":
                          If tableno = "7A" Or tableno = "8" Then
                            SlopeFactors wso, con, row, "", "", "", fmtCellLabel(tableno, "CSF")
                          Else
                            ToxicityValues wso, con, row, "", "", "", fmtCellLabel(tableno, "RFD"), ""
                          End If
                        Case "external":
                          If tableno = "8" Then
                            SlopeFactors wso, con, row, "", "", "", "", fmtCellLabel(tableno, "CSF")
                          End If
                      End Select

                      Set contpw = con.rtpw("rif")(CStr(nd))
                      Dim carc As String: carc = IIf(measure = "RISK", "carcinogenic", "noncarcinogenic")
                      For Each rtpw In contpw
                        If rtpw.Route = objExpRt.Route And rtpw.pathway = objExpRt.pathway And rtpw.measure = carc Then
                          rows2 = Table7A_RIF(wso, row, ds.medtype, nage - 1, locIdx - 1, rtpw.Route, rtpw.pathway, rtpw.measure, rtpw.Unit, con, tvalue, tableno)
                          Exit For
                        End If
                      Next rtpw
                      
                      Set cseries = New Series
                      Set contpw = con.rtpw("hif")(CStr(nd))
                      For Each rtpw In contpw
                        If rtpw.Route = objExpRt.Route And _
                           rtpw.pathway = objExpRt.pathway And _
                           rtpw.measure = measure Then
                           kunit = rtpw.Unit
                                rows3 = Table7A_HIF(wso, row, nd - 1, _
                                  nage - 1, _
                                  con, _
                                  locIdx - 1, _
                                  0, _
                                  measure, _
                                  kunit, _
                                  objExpRt.Route, _
                                  objExpRt.pathway, _
                                  cseries, tableno)
                                  If cseries.tvaluect > 0 Then ' (cseries.num > 0) Then
                                    For j = ST_CONSTITUENT To ST_ALLMEDIA
                                      If TOTAL(j) < 0 Then TOTAL(j) = 0
                                      TOTAL(j) = TOTAL(j) + cseries.tvalue
                                    Next j
                                  End If
                           Exit For
                        End If
                      Next rtpw
                        
                      key = Table9_Collection(ag.Desc, objMedium.medtype, objExpRt.pathway, _
                            ds.exppt.item(1).Desc, con.cname, objExpRt.Route, _
                            measure, wso.range(excelRange(wso.range(fmtCellLabel(tableno, measure)).Column, row)).value)
                      
                      cseries.key = key
                      hifSeries.Add cseries, key
                    End If
                  Next nd
                  row = row + 1
                  CheckRow wso, row, fmtCellLabel(tableno, "LASTROW")
                  
                End If ' chemical
              Next con
              
              If ncon = 0 Then row = row + 1
              
              row = row + 1
              CheckRow wso, row, fmtCellLabel(tableno, "LASTROW")
              
              DrawDoubleBorders wso, wso.range(fmtCellLabel(tableno, "EXPRT")).Column, wso.range(fmtCellLabel(tableno, measure)).Column, row - 1, _
                "Exposure Route Total", TOTAL(ST_EXPOSUREROUTE), SCFMT
              
            Next objExpRt ' next exposure route

            wso.Columns("B:B").AutoFit
            row = row + 1
            CheckRow wso, row, fmtCellLabel(tableno, "LASTROW")
            
            DrawDoubleBorders wso, wso.range(fmtCellLabel(tableno, "EXPPT")).Column, wso.range(fmtCellLabel(tableno, measure)).Column, row - 1, _
              "Exposure Point Total", TOTAL(ST_EXPOSUREPOINT), SCFMT
                
          Next objExpPt ' next exposure point

          row = row + 1
          CheckRow wso, row, fmtCellLabel(tableno, "LASTROW")
          
          DrawDoubleBorders wso, wso.range(fmtCellLabel(tableno, "EXPMED")).Column, wso.range(fmtCellLabel(tableno, measure)).Column, row - 1, _
            "Exposure Medium Total", TOTAL(ST_EXPOSUREMEDIUM), SCFMT
          
        Next objExpMed ' next exposure medium

        row = row + 1
        CheckRow wso, row, fmtCellLabel(tableno, "LASTROW")
        
        DrawDoubleBorders wso, wso.range(fmtCellLabel(tableno, "MEDIUM")).Column, wso.range(fmtCellLabel(tableno, measure)).Column, row - 1, _
          "Medium Total", TOTAL(ST_MEDIUM), SCFMT
                
      Next objMedium
      
      col = wso.range(fmtCellLabel(tableno, "TOTAL")).Column
      row = wso.range(fmtCellLabel(tableno, "TOTAL")).row
      xlsSetCell wso, col, row, "--"
      If TOTAL(ST_ALLMEDIA) >= 0# Then xlsSetCell wso, col, row, TOTAL(ST_ALLMEDIA), SCFMT
   
      Dim txtNote As String
      If measureOfInterest = mct Or measureOfInterest = mRME Then
        txtNote = " (see NOTE below) "
        col = wso.range(fmtCellLabel(tableno, "REF4")).Column
        row = wso.range(fmtCellLabel(tableno, "REF4")).row + 2
        xlsSetCell wso, col, row, "NOTE:"
        xlsSetCell wso, col, row + 1, "  " & advise(1)
        xlsSetCell wso, col, row + 2, "  " & advise(2)
        xlsSetCell wso, col, row + 3, "  " & advise(3)
        wso.range(fmtCellLabel(tableno, "TITLE")).EntireRow.Insert
        row = wso.range(fmtCellLabel(tableno, "TITLE")).row
        col = wso.range(fmtCellLabel(tableno, "TITLE")).Column
        xlsSetCell wso, col, row - 1, mTitle
        ttitle = "For applicable time varying outputs (see footnote 2,3,4) the first value is reported in this table (see footnote a,b,c)."
        xlsSetCell wso, col, row, ttitle
      End If
      
      ref = wso.range(fmtCellLabel(tableno, "REF3")).value
      ref = Left(ref, InStr(ref, "#") - 1)
      ref = Left(ref, InStr(ref, "See") - 1)
      If measureOfInterest = mct Or measureOfInterest = mRME Then
        ref = ref + "Values are based on user specified criteria" + txtNote + "applied to time-varying exposure concentrations in "
      Else
        ref = ref + "Route EPC is determined from sampling the time-varying exposure concentrations curve in "
      End If
      ref = ref + wso.range(fmtCellLabel(tableno, "SITENAME")).value + ".epf"
      wso.range(fmtCellLabel(tableno, "REF3")).value = ref
      ref = wso.range(fmtCellLabel(tableno, "REF4")).value
      ref = Left(ref, InStr(ref, "See") - 1)
      If measureOfInterest = mct Or measureOfInterest = mRME Then
        ref = ref + "Values are based on user specified criteria" + txtNote + "applied to time-varying intakes in "
      Else
        ref = ref + "Intake is determined from sampling the time-varying intakes curve in "
      End If
      ref = ref + wso.range(fmtCellLabel(tableno, "SITENAME")).value + ".rif"
      wso.range(fmtCellLabel(tableno, "REF4")).value = ref
      ref = wso.range(fmtCellLabel(tableno, "REF5")).value
      ref = Left(ref, InStr(ref, "See") - 1)
      If measureOfInterest = mct Or measureOfInterest = mRME Then
        ref = ref + "Values are based on user specified criteria" + txtNote + "applied to time-varying health impacts in "
      Else
        ref = ref + "Cancer Risk is determined from sampling the time-varying cancer risk curve in "
      End If
      ref = ref + wso.range(fmtCellLabel(tableno, "SITENAME")).value + ".hif"
      wso.range(fmtCellLabel(tableno, "REF5")).value = ref
      
      wso.Columns("B:E").AutoFit
  Next ag
  xlo.DisplayAlerts = False
  wbo.sheets("BLANK" & tableno).Delete
  xlo.DisplayAlerts = True

  
ErrorHandler:
  If Err.Number <> 0 Then
    MsgBox Err.Description, vbCritical + vbOKOnly, "Table " & UCase(tableno)
  End If
End Sub

Public Sub Table9_2(tno As Integer, tableno As String, Cthresh As Double, NCthresh As Double)
Dim rc As Long, i As Long, j As Long, k As Long, m As Long, ttype As Long
Dim Route As String, Path As String, Unit As String, pw As String
Dim wso As Object, ds As DatasetCls, con As ContamCls, rt As ExpRtCls, sel As Object
Dim nc As Long, np As Long, row As Long, col As Long, tn As Long
Dim nd As Long, ageIdx As Long, ep As ExpPtCls, ag As AgeCls
Dim locIdx As Long, organ As Long, ntimes As Long, nrt As Long, nr As Long
Dim rvalues() As Double, rtimes() As Double
Dim tvalues() As Double, ttimes() As Double
Dim rte As String, crt As ExpRtCls
Dim ctot() As Double, nctot() As Double, ccol As Variant, nccol As Variant ' ccol() As Long, nccol() As Long
Dim cnum() As Long, ncnum() As Long
Dim rtpw As ExpRtCls ' Collection
Dim pathway As String
Dim col1 As Long, col2 As Long
Dim rng As String
Dim cseries As Series
Dim carcSeries() As Series
Dim ncarcSeries() As Series
Dim key As String
Dim ttitle As String
Dim task As String
Dim borders As Collection
Dim ktype As Integer

  PutLog "Table " & tno & tableno & ", " & Cthresh & ", " & NCthresh

  ' cols 1=ingestion, 2=inhalation, 3=dermal, 4=total
  Const INGESTION = 0
  Const INHALATION = 1
  Const DERMAL = 2
  Const EXTERNAL = 3
  Const ERTOT = 4
      
  On Error Resume Next
  ' get collection of all unique age groups
  Dim ages As New Collection
  For Each objMedium In colMedia
    For Each ag In objMedium.age
      ages.Add ag, ag.Desc
    Next ag
  Next objMedium


  On Error GoTo ErrorHandler
 
  locIdx = 0
  organ = 0
  tn = 0
  ageIdx = -1
  Set ds = hifds.item(1)
  
    
  For Each ag In ages ' ds.age
    Set borders = New Collection
  
    ageIdx = ageIdx + 1
    tn = tn + 1
    Set wso = wbo.sheets("BLANK9")
    wso.Copy before:=wbo.sheets("BLANK9")
    Set wso = wbo.sheets("BLANK9 (2)")
    wso.Activate
      
    ReDim ctot(ST_KTYPE To ST_ALLMEDIA, ERTOT)    ' carcinogenic totals
    ReDim nctot(ST_KTYPE To ST_ALLMEDIA, ERTOT)   ' non-carcinogenic totals
    ReDim cnum(ST_ALLMEDIA, ERTOT)    ' carcinogenic totals
    ReDim ncnum(ST_ALLMEDIA, ERTOT)   ' non-carcinogenic totals
    
    ReDim carcSeries(ST_KTYPE To ST_ALLMEDIA, ERTOT)
    ReDim ncarcSeries(ST_KTYPE To ST_ALLMEDIA, ERTOT)
      
      For i = ST_KTYPE To ST_ALLMEDIA
        For j = INGESTION To ERTOT
          ctot(i, j) = -1
          nctot(i, j) = -1
          Set carcSeries(i, j) = New Series
          Set ncarcSeries(i, j) = New Series
        Next j
      Next i
      
      ' carcinogenic columns
      ccol = Array(wso.range("T9_C_ING").Column, _
                    wso.range("T9_C_INH").Column, _
                    wso.range("T9_C_DERM").Column, _
                    wso.range("T9_C_EXTRAD").Column, _
                    wso.range("T9_C_ERTOT").Column)
      
      ' non-carcinogenic columns
      nccol = Array(wso.range("T9_NC_ING").Column, _
                    wso.range("T9_NC_INH").Column, _
                    wso.range("T9_NC_DERM").Column, _
                    -1, _
                    wso.range("T9_NC_ERTOT").Column)
      
      For i = INHALATION To ERTOT
        ctot(ST_ALLMEDIA, i) = -1
        nctot(ST_ALLMEDIA, i) = -1
      Next i

      wso.Name = "Table " & tno & "." + CStr(tn) + "." & UCase(tableno)
      wso.range("T9_TITLE") = mTitle
      wso.range("T9_SITENAME").EntireRow.Insert
      rng = excelRange(1, wso.range("T9_SITENAME").row - 1)
      If tableno = "A" Then
        wso.range(rng).value = "Summarization is based on the sampling of individual time-varying curves, irrespective of time."
      Else
        wso.range(rng).value = "Summarization is based on the sampling of the time-varying curve, after the time-varying Hazard/Risk curves have been combined in time."
      End If
      If tno = 10 Then
        wso.range("T9_SITENAME").EntireRow.Insert
        rng = excelRange(1, wso.range("T9_SITENAME").row - 1)
        wso.range(rng).value = "Threshold for Carcinogenic Risk = " & Format(Cthresh, SCFMT)
        
        wso.range("T9_SITENAME").EntireRow.Insert
        rng = excelRange(1, wso.range("T9_SITENAME").row - 1)
        wso.range(rng).value = "Threshold for Non-Carcinogenic Hazard Quotient = " & Format(NCthresh, SCFMT)
        
      End If
      wso.range("T9_TABLENO").value = UCase(wso.Name)
      
      row = wso.range("T9_TABLENO").row + 1
      col = wso.range("T9_TABLENO").Column
      If tno = 9 Then
        xlsSetCell wso, col, row, "SUMMARY OF RECEPTOR RISKS AND HAZARDS FOR COPCs"
      Else
        xlsSetCell wso, col, row, "RISK ASSESSMENT SUMMARY"
      End If
      
      wso.range("T9_SITENAME").value = SiteName
      wso.range("T9_TIME").value = TimeFrame()
      wso.range("T9_POP").value = PopDesc()
      wso.range("T9_AGE").value = ag.Desc ' colAge.Text
      
      Dim txtNote As String
      If measureOfInterest = mct Or measureOfInterest = mRME Then
        txtNote = " (see NOTE below) "
        col = 1
        row = wso.range("T9_NC_OTOT").row + 2
        xlsSetCell wso, col, row + 1, "NOTE:"
        xlsSetCell wso, col, row + 2, "  " & advise(1)
        xlsSetCell wso, col, row + 3, "  " & advise(2)
        xlsSetCell wso, col, row + 4, "  " & advise(3)
        wso.range("T9_SITENAME").EntireRow.Insert
        row = wso.range("T9_SITENAME").row - 1
        col = wso.range("T9_TITLE").Column
        ttitle = "For applicable time varying outputs the first value is reported in this table (see footnote a,b,c)."
        xlsSetCell wso, col, row, ttitle
      End If
      row = wso.range("T9_MEDIUM").row
      task = ""
      
      For Each objMedium In colMedia
        col = wso.range("T9_MEDIUM").Column: xlsSetCell wso, col, row, objMedium.medtype
        
        For i = ST_KTYPE To ST_MEDIUM
          For j = INGESTION To ERTOT
            ctot(i, j) = -1
            nctot(i, j) = -1
            carcSeries(i, j).Reset
            ncarcSeries(i, j).Reset
          Next j
        Next i
        
        For nd = 1 To hifds.count
          Set ds = hifds.item(nd)
          If ds.medtype = objMedium.medtype Then
            Exit For
          End If
        Next nd

        For Each objExpMed In objMedium.expmed
          col = wso.range("T9_EXPMED").Column: xlsSetCell wso, col, row, objExpMed.label
          
          For i = ST_KTYPE To ST_EXPOSUREMEDIUM
            For j = INGESTION To ERTOT
              ctot(i, j) = -1
              nctot(i, j) = -1
              carcSeries(i, j).Reset
              ncarcSeries(i, j).Reset
            Next j
          Next i
          
          For Each objExpPt In objExpMed.exppt
            col = wso.range("T9_EXPPT").Column: xlsSetCell wso, col, row, objExpPt.label
            
            For i = ST_CONSTITUENT To ST_EXPOSUREPOINT
                For j = INGESTION To ERTOT
                  ctot(i, j) = -1#
                  nctot(i, j) = -1#
                  carcSeries(i, j).Reset
                  ncarcSeries(i, j).Reset
                Next j
            Next i
            
            For ktype = 0 To 1
            
              For j = INGESTION To ERTOT
                ctot(ST_KTYPE, j) = -1#
                nctot(ST_KTYPE, j) = -1#
                carcSeries(ST_KTYPE, j).Reset
                ncarcSeries(ST_KTYPE, j).Reset
              Next j
              
              For Each con In cont
                If con.ktype = ktype Then
                  key = ag.Desc()
                  key = key & "\" & objMedium.medtype
                  key = key & "\" & objExpMed.label
                  key = key & "\" & objExpPt.Desc()
                  key = key & "\" & con.cname
                  
                  For j = INGESTION To ERTOT
                    col = ccol(j)
                    If col > 0 Then
                      xlsSetCell wso, col, row, "--"
                      wso.range(excelRange(col, row, col, row)).borders(xlEdgeLeft).linestyle = 1
                    End If
                    col = nccol(j)
                    If col > 0 Then
                      xlsSetCell wso, col, row, "--"
                      wso.range(excelRange(col, row, col, row)).borders(xlEdgeLeft).linestyle = 1
                    End If
                    
                  Next j
                  
                  For j = INGESTION To ERTOT ' reinitialize chemical totals
                    ctot(ST_CONSTITUENT, j) = -1#
                    nctot(ST_CONSTITUENT, j) = -1#
                    carcSeries(ST_CONSTITUENT, j).Reset
                    ncarcSeries(ST_CONSTITUENT, j).Reset
                  Next j
                  col = wso.range("T9_C_CNAME").Column: xlsSetCell wso, col, row, con.cname ' colContam.Text
                  
                  Dim contpw As New Collection
                  Set contpw = con.rtpw("hif")(CStr(nd))
                  
                  For Each rtpw In contpw
                    If rtpw.pathway = objExpMed.label Then
                    
                      Set cseries = GetSeries(key & "\" & rtpw.measure & "\" & rtpw.Route)
                      If cseries.tvaluect > 0 Then
                        nc = -1
                        Select Case rtpw.measure ' colType.Text
                          Case "Risk":
                            Select Case rtpw.Route ' colRoute.Text
                              Case "ingestion": nc = INGESTION
                              Case "inhalation": nc = INHALATION
                              Case "dermal": nc = DERMAL
                              Case "external": nc = EXTERNAL
                            End Select
                            col = ccol(nc)
                            If cseries.tvalue < Cthresh Then nc = -1
'                            If nc >= 0 Then
'                              col = ccol(nc)
'                              For j = ST_KTYPE To ST_ALLMEDIA
'                                If ctot(j, nc) < 0 Then ctot(j, nc) = 0
'                                ctot(j, nc) = ctot(j, nc) + cseries.tvalue
'                                If ctot(j, ERTOT) < 0 Then ctot(j, ERTOT) = 0
'                                ctot(j, ERTOT) = ctot(j, ERTOT) + cseries.tvalue
'                                carcSeries(j, nc).AddInSeries cseries
'                                carcSeries(j, ERTOT).AddInSeries cseries
'                              Next j
'                            End If
                              For j = ST_KTYPE To ST_ALLMEDIA
                                If nc >= 0 Then
                                    If ctot(j, nc) < 0 Then ctot(j, nc) = 0
                                    ctot(j, nc) = ctot(j, nc) + cseries.tvalue
                                    carcSeries(j, nc).AddInSeries cseries
                                End If
                                If ctot(j, ERTOT) < 0 Then ctot(j, ERTOT) = 0
                                ctot(j, ERTOT) = ctot(j, ERTOT) + cseries.tvalue
                                carcSeries(j, ERTOT).AddInSeries cseries
                              Next j
                          Case "HI":
                            Select Case rtpw.Route ' colRoute.Text
                              Case "ingestion": nc = INGESTION
                              Case "inhalation": nc = INHALATION
                              Case "dermal": nc = DERMAL
                            End Select
                            col = nccol(nc)
                            If cseries.tvalue < NCthresh Then nc = -1
'                            If nc >= 0 Then
'                              col = nccol(nc)
'                              For j = ST_KTYPE To ST_ALLMEDIA
'                                If nctot(j, nc) < 0 Then nctot(j, nc) = 0
'                                nctot(j, nc) = nctot(j, nc) + cseries.tvalue
'                                If nctot(j, ERTOT) < 0 Then nctot(j, ERTOT) = 0
'                                nctot(j, ERTOT) = nctot(j, ERTOT) + cseries.tvalue
'                                ncarcSeries(j, nc).AddInSeries cseries
'                                ncarcSeries(j, ERTOT).AddInSeries cseries
'                              Next j
'                            End If
                            For j = ST_KTYPE To ST_ALLMEDIA
                              If nc >= 0 Then
                                  If nctot(j, nc) < 0 Then nctot(j, nc) = 0
                                  nctot(j, nc) = nctot(j, nc) + cseries.tvalue
                                  ncarcSeries(j, nc).AddInSeries cseries
                              End If
                              If nctot(j, ERTOT) < 0 Then nctot(j, ERTOT) = 0
                              nctot(j, ERTOT) = nctot(j, ERTOT) + cseries.tvalue
                              ncarcSeries(j, ERTOT).AddInSeries cseries
                            Next j
                        End Select
                        If nc >= 0 Then
                            xlsSetCell wso, col, row, IIf(Not cseries.tvaluect >= 0#, "", cseries.tvalue), SCFMT
                        End If
                      End If
                    End If
                  Next rtpw
                  If tableno = "A" Then
                    For nr = INGESTION To ERTOT - 1
                      If ctot(ST_CONSTITUENT, nr) >= Cthresh Then xlsSetCell wso, ccol(nr), row, ctot(ST_CONSTITUENT, nr), SCFMT
                      If nctot(ST_CONSTITUENT, nr) >= NCthresh Then xlsSetCell wso, nccol(nr), row, nctot(ST_CONSTITUENT, nr), SCFMT
                    Next nr
                      If ctot(ST_CONSTITUENT, ERTOT) >= 0# Then xlsSetCell wso, ccol(nr), row, ctot(ST_CONSTITUENT, nr), SCFMT
                      If nctot(ST_CONSTITUENT, ERTOT) >= 0# Then xlsSetCell wso, nccol(nr), row, nctot(ST_CONSTITUENT, nr), SCFMT
                  Else
                    For nr = INGESTION To ERTOT - 1
                      If carcSeries(ST_CONSTITUENT, nr).SummaryValue >= Cthresh Then xlsSetCell wso, ccol(nr), row, carcSeries(ST_CONSTITUENT, nr).SummaryValue, SCFMT
                      If ncarcSeries(ST_CONSTITUENT, nr).SummaryValue >= NCthresh Then xlsSetCell wso, nccol(nr), row, ncarcSeries(ST_CONSTITUENT, nr).SummaryValue, SCFMT
                    Next nr
                      If carcSeries(ST_CONSTITUENT, ERTOT).SummaryValue >= 0# Then xlsSetCell wso, ccol(nr), row, carcSeries(ST_CONSTITUENT, nr).SummaryValue, SCFMT
                      If ncarcSeries(ST_CONSTITUENT, ERTOT).SummaryValue >= 0# Then xlsSetCell wso, nccol(nr), row, ncarcSeries(ST_CONSTITUENT, nr).SummaryValue, SCFMT
                  End If
                  xlsSetCell wso, wso.range("T9_NC_ORGAN").Column, row, ds.organs.item(1)
                  row = row + 1
                  CheckRow wso, row, "T9_LASTROW"
                End If
              Next con
              
              row = row + 1: CheckRow wso, row, "T9_LASTROW"
              If tableno = "A" Then
                DrawSingleBorders wso, wso.range("T9_C_CNAME").Column, ccol(ERTOT), row - 1, IIf(ktype = 0, "Chemical Total", "Radionuclide Total"), ctot(ST_KTYPE, ERTOT), SCFMT
                DrawSingleBorders wso, ccol(ERTOT) + 1, nccol(ERTOT), row - 1, "", nctot(ST_KTYPE, ERTOT), SCFMT
              Else
                DrawSingleBorders wso, wso.range("T9_C_CNAME").Column, ccol(ERTOT), row - 1, IIf(ktype = 0, "Chemical Total", "Radionuclide Total"), carcSeries(ST_KTYPE, ERTOT).SummaryValue, SCFMT
                DrawSingleBorders wso, ccol(ERTOT) + 1, nccol(ERTOT), row - 1, "", ncarcSeries(ST_KTYPE, ERTOT).SummaryValue, SCFMT
              End If
                      
            Next ktype
            row = row + 1: CheckRow wso, row, "T9_LASTROW"
            If tableno = "A" Then
              DrawDoubleBorders wso, wso.range("T9_EXPPT").Column, ccol(ERTOT), row - 1, "Exposure Point Total", ctot(ST_EXPOSUREPOINT, ERTOT), SCFMT
              DrawDoubleBorders wso, ccol(ERTOT) + 1, nccol(ERTOT), row - 1, "", nctot(ST_EXPOSUREPOINT, ERTOT), SCFMT
            Else
              DrawDoubleBorders wso, wso.range("T9_EXPPT").Column, ccol(ERTOT), row - 1, "Exposure Point Total", carcSeries(ST_EXPOSUREPOINT, ERTOT).SummaryValue, SCFMT
              DrawDoubleBorders wso, ccol(ERTOT) + 1, nccol(ERTOT), row - 1, "", ncarcSeries(ST_EXPOSUREPOINT, ERTOT).SummaryValue, SCFMT
            End If
 
          Next objExpPt ' in objExpMed.exppt
          
            row = row + 1: CheckRow wso, row, "T9_LASTROW"
            If tableno = "A" Then
              DrawDoubleBorders wso, wso.range("T9_EXPMED").Column, ccol(ERTOT), row - 1, "Exposure Medium Total", ctot(ST_EXPOSUREMEDIUM, ERTOT), SCFMT
              DrawDoubleBorders wso, ccol(ERTOT) + 1, nccol(ERTOT), row - 1, "", nctot(ST_EXPOSUREMEDIUM, ERTOT), SCFMT
            Else
              DrawDoubleBorders wso, wso.range("T9_EXPMED").Column, ccol(ERTOT), row - 1, "Exposure Medium Total", carcSeries(ST_EXPOSUREMEDIUM, ERTOT).SummaryValue, SCFMT
              DrawDoubleBorders wso, ccol(ERTOT) + 1, nccol(ERTOT), row - 1, "", ncarcSeries(ST_EXPOSUREMEDIUM, ERTOT).SummaryValue, SCFMT
            End If
            
        Next objExpMed ' in objMedium.expmed
            
            row = row + 1: CheckRow wso, row, "T9_LASTROW"
            If tableno = "A" Then
              DrawDoubleBorders wso, wso.range("T9_MEDIUM").Column, ccol(ERTOT), row - 1, "Medium Total", ctot(ST_MEDIUM, ERTOT), SCFMT
              DrawDoubleBorders wso, ccol(ERTOT) + 1, nccol(ERTOT), row - 1, "", nctot(ST_MEDIUM, ERTOT), SCFMT
            Else
              DrawDoubleBorders wso, wso.range("T9_MEDIUM").Column, ccol(ERTOT), row - 1, "Medium Total", carcSeries(ST_MEDIUM, ERTOT).SummaryValue, SCFMT
              DrawDoubleBorders wso, ccol(ERTOT) + 1, nccol(ERTOT), row - 1, "", ncarcSeries(ST_MEDIUM, ERTOT).SummaryValue, SCFMT
            End If
        
      Next objMedium ' in colMedia
          
      row = wso.range("T9_TMED").row + 1
      col = wso.range("T9_TMED").Column
      xlsSetCell wso, col - 1, row, "Total Risk Across All Media"
      xlsSetCell wso, col, row, "--"
      If tableno = "A" Then
        If (ctot(ST_ALLMEDIA, ERTOT) >= 0) Then xlsSetCell wso, col, row, ctot(ST_ALLMEDIA, ERTOT), SCFMT
      Else
        If (carcSeries(ST_ALLMEDIA, ERTOT).SummaryValue >= 0#) Then xlsSetCell wso, col, row, carcSeries(ST_ALLMEDIA, ERTOT).SummaryValue, SCFMT
      End If
      DrawBorders2 wso, col, col, row, -4119
      
      col = wso.range("T9_NC_TOT").Column
      row = wso.range("T9_TMED").row + 1
      xlsSetCell wso, col - 1, row, "Total Hazard Quotient Across All Media"
      borders.Add excelRange(col, row, col, row)
      xlsSetCell wso, col, row, "--"
      If tableno = "A" Then
        If nctot(ST_ALLMEDIA, ERTOT) >= 0 Then xlsSetCell wso, col, row, nctot(ST_ALLMEDIA, ERTOT), SCFMT
      Else
        If ncarcSeries(ST_ALLMEDIA, ERTOT).SummaryValue >= 0# Then xlsSetCell wso, col, row, ncarcSeries(ST_ALLMEDIA, ERTOT).SummaryValue, SCFMT
      End If
      DrawBorders2 wso, col, col, row, -4119
      
      wso.Columns("B:D").AutoFit
     Next ag
     
  xlo.DisplayAlerts = False
  If tableno = "B" And tno = 10 Then
    wbo.sheets("BLANK9").Delete
    wbo.sheets("BLANK10").Delete
  End If
  xlo.DisplayAlerts = True
  
ErrorHandler:
  If Err.Number <> 0 Then
    MsgBox Err.Description, vbCritical + vbOKOnly, "Table " & UCase(tableno)
  End If
End Sub


Public Sub Table3()
Dim rc As Long, i As Long, Route As String, Path As String, Unit As String
Dim pw As String
Dim cmin As Double, cmax As Double, tmin As Double, tmax As Double
Dim wso As Object, ds As DatasetCls, con As ContamCls, rt As ExpRtCls
Dim nc As Long, row As Long, col As Long, tn As Long
Dim nd As Long, pathway As String
Dim es As DatasetCls
Dim locIdx As Long
Dim exp As ExpRtCls
Dim tvalue As Double

  PutLog "Table3"

  For Each objMedium In colMedia
    For Each objExpMed In objMedium.expmed
      For Each objExpPt In objExpMed.exppt
      
        tn = tn + 1
        Set wso = wbo.sheets("BLANK3")
        wso.Copy before:=wbo.sheets("BLANK3")
        Set wso = wbo.sheets("BLANK3 (2)")
        wso.Activate
        wso.Name = "Table 3." + CStr(tn)
        
        wso.range("T3_TABLENO").value = UCase(wso.Name) & " " & whichTables
        wso.range("T3_TITLE").value = mTitle
        wso.range("T3_SITENAME").value = SiteName
        wso.range("T3_TIME").value = TimeFrame()
        wso.range("T3_MEDIUM").value = objMedium.medtype
        wso.range("T3_EXPMED").value = objExpMed.label
        wso.range("T3_EXPPT").value = objExpPt.label
        
        row = wso.range("T3_LASTROW").row - wso.range("T3_CNAME").row
        If cont.count > row Then
          For i = 1 To row - cont.count
            wso.range("T3_LASTROW").EntireRow.Insert
          Next
        End If
        nc = 0
        For Each con In cont
          row = wso.range("T3_CNAME").row + nc: CheckRow wso, row, "T3_LASTROW"
          col = wso.range("T3_CNAME").Column: xlsSetCell wso, col, row, con.cname
          
'          ' NOTE:  modeled epc's (below) are not displayed on this table because the table does not report
'          ' the exposure route detail, without which unit differences make summation to the medium/exposure point
'          ' incorrect

          nc = nc + 1
        Next
        wso.Columns("B").AutoFit
      Next objExpPt
    Next objExpMed
  Next objMedium
  
  xlo.DisplayAlerts = False
  wbo.sheets("BLANK3").Delete
  xlo.DisplayAlerts = True
End Sub

Public Sub Table2()
Dim rc As Long, i As Long, Route As String, Path As String, Unit As String
Dim pw As String
Dim cmin As Double, cmax As Double, tmin As Double, tmax As Double
Dim wso As Object, ds As DatasetCls, con As ContamCls, rt As ExpRtCls
Dim nc As Long, row As Long, col As Long, tn As Long
Dim nd As Long, pathway As String

  PutLog "Table2"
  
  On Error GoTo ErrorHandler
  
  For Each objMedium In colMedia
    PutLog "Table2 " & objMedium.medtype
    For Each objExpMed In objMedium.expmed
      PutLog "Table2 " & objExpMed.label
      For Each objExpPt In objExpMed.exppt
        PutLog "Table2 " & objExpPt.Desc
        tn = tn + 1
        Set wso = wbo.sheets("BLANK2")
        wso.Copy before:=wbo.sheets("BLANK3")
        Set wso = wbo.sheets("BLANK2 (2)")
        wso.Activate
        wso.Name = "Table 2." + CStr(tn)
        wso.range("TABLENO").value = UCase(wso.Name)
        wso.range("SITENAME").value = SiteName
        wso.range("T2_TIME").value = TimeFrame()
        wso.range("MEDIUM").value = objMedium.medtype
        wso.range("EXPMED").value = objExpMed.label
        wso.range("T2_EXPPT").value = objExpPt.label
        row = wso.range("T2_LASTROW").row - wso.range("T2_CASID").row
        If cont.count > row Then
          For i = 1 To row - cont.count
            wso.range("T2_LASTROW").EntireRow.Insert
          Next
        End If
        nc = 0
        For Each con In cont
          row = wso.range("T2_CASID").row + nc: CheckRow wso, row, "T2_LASTROW"
          col = wso.range("T2_CASID").Column: xlsSetCell wso, col, row, con.cas
          col = wso.range("T2_CNAME").Column: xlsSetCell wso, col, row, con.cname
          nc = nc + 1
        Next
        wso.Columns("B:C").AutoFit
      Next objExpPt
    Next objExpMed
  Next objMedium
  
  
  xlo.DisplayAlerts = False
  wbo.sheets("BLANK2").Delete
  xlo.DisplayAlerts = True
  
ErrorHandler:
  If Err.Number <> 0 Then
    MsgBox Err.Description, vbOKOnly + vbCritical, "Table2"
  End If
End Sub


Public Sub LoadEPFDatasets(fName$, Source$)
Dim ds As Object, numds As Long, tname As String
Dim i As Long, j As Long, k As Long, l As Long
Dim tstr As String, tstr2 As String, tstr3 As String, numpt As Long
Dim obj As ExpPtCls

  PutLog "LoadEPFDatasets"
  
  tname = IIf(0 = InStr(fName, "."), fName, Left(fName, InStr(fName, ".") - 1))
  tname = Left(fName, InStr(fName, ".gid") - 1)

  numds = epfOpen(tname, Source)
  
  For i = 1 To numds
    Set ds = New DatasetCls
    
    tstr = String(1024, Chr(0))
    tstr2 = String(1024, Chr(0))
    tstr3 = String(1024, Chr(0))
    
    epfGetSetInfo i - 1, numpt, tstr, tstr2, tstr3
    
    ds.locName = Left(tstr, InStr(tstr, Chr(0)) - 1)
    ds.medtype = Left(tstr2, InStr(tstr2, Chr(0)) - 1)
    
    Set ds.exppt = New Collection
    For j = 1 To numpt
      Set obj = New ExpPtCls
      epfGetExposurePoint i - 1, j - 1, obj.x, obj.y
      ds.exppt.Add obj, CStr(j)
      obj.label = csm(epfGlyph.id).label
    Next j
    
    epfds.Add ds, ds.locName + "," + ds.medtype
    
    Set ds.exprt = New Collection
    
    Dim contpw As Collection
    Dim found As Boolean
    For j = 1 To cont.count
      Set contpw = cont.item(j).GetEpfRoutesAndPathways(i - 1)
      For k = 1 To contpw.count
        found = False
        For l = 1 To ds.exprt.count
          If (contpw.item(k).Route = ds.exprt(l).Route And _
            contpw.item(k).pathway = ds.exprt(l).pathway And _
            contpw.item(k).Unit = ds.exprt(l).Unit) Then
            found = True
            Exit For
          End If
        Next l
        If Not found Then ds.exprt.Add contpw.item(k), CStr(ds.exprt.count + 1)
      Next k
    Next j
  Next i
End Sub

Function LoadRIFDatasets(fName$, Source$) As Long
Dim i As Long, j As Long, k As Long, l As Long, r As Integer, c As Integer, retc As Long, x As Long, y As Long
Dim tstr As String, tstr2 As String, tstr3 As String
Dim cbolist As String
Dim ds As Object, obj As Object

Dim numds As Integer, numpt As Long, numAge As Long
Dim s1 As Double, s2 As Double

  PutLog "LoadRIFDatasets"
  
  tstr = IIf(0 = InStr(fName, "."), fName, Left(fName, InStr(fName, ".") - 1))
  tstr = Left(fName, InStr(fName, ".gid") - 1)
  
  numds = rifOpen(tstr, Source)
  
'  ReDim ds(numds)
  tstr = String(1024, Chr(0))
  tstr2 = String(1024, Chr(0))
  tstr3 = String(1024, Chr(0))
  cbolist = ""
  For i = 1 To numds
    Set ds = New DatasetCls
    
     retc = rifGetSetInfo(i - 1, numpt, numAge, tstr, tstr2, tstr3)
     ds.locName = Left(tstr, InStr(tstr, Chr(0)) - 1)
     ds.medtype = Left(tstr2, InStr(tstr2, Chr(0)) - 1)
     ds.dstype = Left(tstr3, InStr(tstr3, Chr(0)) - 1)
     
    Set ds.exppt = New Collection
    For j = 1 To numpt
      Set obj = New ExpPtCls
      rifGetExposurePoint i - 1, j - 1, obj.x, obj.y
      ds.exppt.Add obj, CStr(j)
      obj.label = csm(epfGlyph.id).label
    Next j
    
    Set ds.age = New Collection
    For j = 1 To numAge
      Set obj = New AgeCls
      rifGetAgeGroup i - 1, j - 1, s1, s2
      obj.min = s1: obj.max = s2: ds.age.Add obj, CStr(j)
    Next j
    
    rifds.Add ds, ds.locName + "," + ds.medtype
  
    Set ds.exprt = New Collection
    
    Dim contpw As Collection
    Dim found As Boolean
    For j = 1 To cont.count
      Set contpw = cont.item(j).GetRifRoutesAndPathways(i - 1, 0)
      For k = 1 To contpw.count
        found = False
        For l = 1 To ds.exprt.count
          If (contpw.item(k).Route = ds.exprt(l).Route And _
            contpw.item(k).pathway = ds.exprt(l).pathway And _
            contpw.item(k).measure = ds.exprt(l).measure And _
            contpw.item(k).Unit = ds.exprt(l).Unit) Then
            found = True
            Exit For
          End If
        Next l
        If Not found Then
          ds.exprt.Add contpw.item(k), CStr(ds.exprt.count + 1)
        End If
      Next k
    Next j
  
  Next i
  LoadRIFDatasets = numds
End Function

Public Sub LoadHIFDatasets(fName$, Source$)
Dim tstr As String, tstr2 As String, tstr3 As String
Dim j As Long, k As Long, l As Long, obj As Object
Dim numpt As Long, numAge As Long, nsites As Long, norgans As Long
Dim xp As Long, yp As Long
Dim ds As Object, numds As Long, i As Long, tname As String
Dim s1 As Double, s2 As Double
Dim con As ContamCls

  PutLog "LoadHIFDatasets"
  
  tname = IIf(0 = InStr(fName, "."), fName, Left(fName, InStr(fName, ".") - 1))
  
  tname = Left(fName, InStr(fName, ".gid") - 1)
  
  numds = hifOpen(tname, Source)
  
  For i = 1 To numds
    Set ds = New DatasetCls
  
    tstr = String(1024, Chr(0))
    tstr2 = String(1024, Chr(0))
    tstr3 = String(1024, Chr(0))
    
    hifGetSetInfo i - 1, numpt, numAge, nsites, norgans, tstr, tstr2, tstr3
    
    ds.locName = Left(tstr, InStr(tstr, Chr(0)) - 1)
    ds.medtype = Left(tstr2, InStr(tstr2, Chr(0)) - 1)
    ds.dstype = Left(tstr3, InStr(tstr3, Chr(0)) - 1)
    
    For j = 1 To numpt
      Set obj = New ExpPtCls
      hifGetExposurePoint i - 1, j - 1, obj.x, obj.y
      ds.exppt.Add obj, CStr(j)
      obj.label = csm(epfGlyph.id).label
    Next j
    
    For j = 1 To numAge
      Set obj = New AgeCls
      hifGetAgeGroup i - 1, j - 1, s1, s2
      obj.min = s1: obj.max = s2: ds.age.Add obj, CStr(j)
    Next j
    
    For j = 1 To nsites
      tstr = String(1024, Chr(0))
      hifGetCancerOrgan i - 1, j - 1, tstr
      ds.sites.Add Left(tstr, InStr(tstr, Chr(0)) - 1), CStr(j)
    Next j
    
    For j = 1 To norgans
      tstr = String(1024, Chr(0))
      hifGetDoseOrgan i - 1, j - 1, tstr
      ds.organs.Add Left(tstr, InStr(tstr, Chr(0)) - 1), CStr(j)
    Next j
    
    hifds.Add ds, ds.locName + "," + ds.medtype
    
    Dim contpw As Collection
    Dim found As Boolean
    For j = 1 To cont.count
      Set contpw = cont.item(j).GetHifRoutesAndPathways(i - 1, 0)
      For k = 1 To contpw.count
        found = False
        For l = 1 To ds.exprt.count
          If (contpw.item(k).Route = ds.exprt(l).Route And _
            contpw.item(k).pathway = ds.exprt(l).pathway) Then
            found = True
            Exit For
          End If
        Next l
        If Not found Then
          ds.exprt.Add contpw.item(k), CStr(ds.exprt.count + 1)
        End If
      Next k
      Set con = cont.item(j)
    Next j
  Next i

End Sub


Public Function Table7A_EPF(ByRef wso As Object, ByVal inrow As Long, medtype As String, ByVal locIdx As Long, Route As String, pathway As String, Unit As String, ByRef cont As ContamCls, ByRef retvalue As Double, ByVal tableno As String)
  Dim ntimes As Long
  Dim row As Long
  Dim rvalues() As Double
  Dim rtimes() As Double
  Dim dsIdx As Long
  Dim i As Long
  Dim np As Long
  Dim ttime As Double
  Dim tvalue As Double
  Dim cseries As New Series
  Dim col As Long
  
  retvalue = 0#
  ReDim rvalues(0)
  ReDim rtimes(0)
  
  PutLog "Table7A_EPF " & medtype & ", " & locIdx & ", " & cont.cas & ", " & pathway & ", " & Route
  
  On Error GoTo ErrorHandler
  row = inrow
  col = wso.range(fmtCellLabel(tableno, "EPC")).Column
  xlsSetCell wso, col, row, NA
  xlsSetCell wso, col + 1, row, NA
          
  For dsIdx = 0 To epfds.count - 1
    If epfds(dsIdx + 1).medtype = medtype Then
      
      ntimes = epfLoadTimeSeries(dsIdx, locIdx, cont.cas, cont.cas, pathway, Route)
      
      If ntimes > 0 And UBound(rtimes) <= 0 Then
        ReDim rvalues(1 To ntimes)
        ReDim rtimes(1 To ntimes)
      End If
      For i = 1 To ntimes
        epfGetTimeAndValue dsIdx, i - 1, rtimes(i), rvalues(i)
      Next
    End If
  Next
  If UBound(rtimes) > 0 Then
    cseries.SetSeries ntimes, rtimes, rvalues
    
    row = cseries.SetCell(wso, inrow, fmtCellLabel(tableno, "EPC"), fmtCellLabel(tableno, "LASTROW"), Unit)
  End If
          
ErrorHandler:
  If Err.Number <> 0 Then
    MsgBox Err.Description, vbOKOnly, "Table7A_EPF"
  End If
  Table7A_EPF = row
End Function



Public Function Table7A_RIF(ByRef wso As Object, ByVal inrow As Long, medtype As String, ByVal ageIdx As Long, ByVal locIdx As Long, Route As String, pathway As String, meas As String, Unit As String, ByRef cont As ContamCls, ByRef retvalue As Double, ByVal tableno As String) As Long
  Dim ntimes As Long
  Dim row As Long
  Dim rvalues() As Double
  Dim rtimes() As Double
  Dim dsIdx As Long
  Dim i As Long
  Dim np As Long
  Dim ttime As Double
  Dim tvalue As Double
  Dim cseries As New Series

  retvalue = 0#
  ReDim rvalues(0)
  ReDim rtimes(0)
  
  PutLog "Table7A_RIF " & medtype & ", " & locIdx & ", " & ageIdx & ", " & cont.cas & ", " & pathway & ", " & Route & ", " & meas & ", " & Unit
 
  On Error GoTo ErrorHandler
  row = inrow
  For dsIdx = 0 To rifds.count - 1
    If rifds(dsIdx + 1).medtype = medtype Then
        ntimes = rifLoadTimeSeries(dsIdx, locIdx, ageIdx, cont.cas, cont.cas, pathway, Route, meas, Unit)
        If ntimes > 0 And UBound(rtimes) <= 0 Then
          ReDim rvalues(1 To ntimes)
          ReDim rtimes(1 To ntimes)
        End If
        For i = 1 To ntimes
          rifGetTimeAndValue dsIdx, i - 1, rtimes(i), rvalues(i)
        Next i
    End If
  Next dsIdx
          
  If UBound(rtimes) > 0 Then
    cseries.SetSeries ntimes, rtimes, rvalues
    row = cseries.SetCell(wso, inrow, fmtCellLabel(tableno, "INTAKE"), fmtCellLabel(tableno, "LASTROW"), Unit)
  End If
          
ErrorHandler:
     If Err.Number <> 0 Then
      MsgBox Err.Description, vbOKOnly, "Table7A_RIF"
    End If
  Table7A_RIF = row
End Function



Public Function Table7A_HIF(ByRef wso As Object, ByVal inrow As Long, ByVal nd As Long, ByVal age As Long, cont As ContamCls, _
      ByVal locIdx As Long, ByVal organ As Long, ByVal endpt As String, ByVal epunit As String, ByVal rte As String, _
      ByVal Path As String, ByRef cseries As Series, ByVal tableno As String) As Long
      
  Dim m As Long, np As Long, i As Long
  Dim ntimes As Long
  Dim row As Long
  Dim rvalues() As Double, rtimes() As Double
  Dim ttimes() As Double, tvalues() As Double
  
  On Error GoTo ErrorHandler
  
  PutLog "Table7A_HIF " & nd & ", " & locIdx & ", " & age & ", " & _
    organ & ", " & cont.cas & ", " & Path & ", " & rte & ", " & endpt & ", " & epunit

  row = inrow
  
  ReDim rvalues(0)
  ReDim rtimes(0)
  ReDim ttimes(0)
  ReDim tvalues(0)
  
  ntimes = hifLoadTimeSeries(nd, locIdx, age, organ, cont.cas, cont.cas, Path, rte, endpt, epunit)
   
  If ntimes > 0 Then
    ReDim rvalues(1 To ntimes)
    ReDim rtimes(1 To ntimes)
    ReDim tvalues(1 To ntimes)
    ReDim ttimes(1 To ntimes)
    hifGetTimeSeries nd, rtimes(1), rvalues(1)
  End If
  
  If UBound(rtimes) Then
    cseries.SetSeries ntimes, rtimes, rvalues
    row = cseries.SetCell(wso, inrow, fmtCellLabel(tableno, endpt), fmtCellLabel(tableno, "LASTROW"))
  End If
  
ErrorHandler:
  If Err.Number <> 0 Then
    MsgBox Err.Description, vbOKOnly, "Table7A_HIF"
  End If
  Table7A_HIF = row
End Function

Public Sub GenerateTables()
Dim modId As Collection, sinkid As Collection
Dim i As Long, j As Long, obj As Object, con As ContamCls
Dim cmod As CsmCls, csrc As CsmCls
  
  On Error GoTo ErrorHandler
  mainform.ProgressBar1.value = 0
  
  Set epfds = New Collection
  Set rifds = New Collection
  Set hifds = New Collection
  Set cont = New Collection

  If Not StartExcel() Then
    Cancel = True
    Exit Sub
  End If
  
  getContamList FUIName ' initializes cont collection
  
  Set rifGlyph = Nothing
  Set epfGlyph = Nothing
  Set hifglyph = Nothing
  Set cmod = csm(modName)
  For Each csrc In cmod.src
    If csrc.stype = "epf" Then Set epfGlyph = csrc
    If csrc.stype = "rif" Then Set rifGlyph = csrc
    If csrc.stype = "hif" Then Set hifglyph = csrc
  Next csrc
  
  If epfGlyph Is Nothing Or rifGlyph Is Nothing Or hifglyph Is Nothing Then
    MsgBox "Connections are incomplete.  RAGS cannot continue.", _
      vbOKOnly + vbCritical, "Generate Tables"
    End
  End If
  
  DoEvents: If Not Cancel Then LoadEPFDatasets FUIName, epfGlyph.id
  DoEvents: If Not Cancel Then LoadRIFDatasets FUIName, rifGlyph.id
  DoEvents: If Not Cancel Then LoadHIFDatasets FUIName, hifglyph.id
  
  PutLog "GetObject excel.application"

  PutLog mTitle
  PutLog "carcinogenic threshold: " & frmUserDef.txtCThresh
  PutLog "noncarcinogenic threshold: " & frmUserDef.txtNCthresh
  
  Dim Cthresh As Double
  Dim NCthresh As Double
  
  mainform.ProgressBar1.value = 5: DoEvents: If Not Cancel Then Table1
  mainform.ProgressBar1.value = 10: DoEvents: If Not Cancel Then Table2
  mainform.ProgressBar1.value = 20: DoEvents: If Not Cancel Then Table3
  mainform.ProgressBar1.value = 30: DoEvents: If Not Cancel Then Table4
  mainform.ProgressBar1.value = 40: DoEvents: If Not Cancel Then Table51
  mainform.ProgressBar1.value = 52: DoEvents: If Not Cancel Then Table52
  mainform.ProgressBar1.value = 54: DoEvents: If Not Cancel Then Table53
  mainform.ProgressBar1.value = 56: DoEvents: If Not Cancel Then Table61
  mainform.ProgressBar1.value = 62: DoEvents: If Not Cancel Then Table62
  mainform.ProgressBar1.value = 64: DoEvents: If Not Cancel Then Table63
  mainform.ProgressBar1.value = 65: DoEvents: If Not Cancel Then Table64
  mainform.ProgressBar1.value = 70: DoEvents: If Not Cancel Then Table7A 7, "A", "RISK", 0
  mainform.ProgressBar1.value = 75: DoEvents: If Not Cancel Then Table7A 7, "B", "HI", 0
  mainform.ProgressBar1.value = 78: DoEvents: If Not Cancel Then Table7A 8, "", "RISK", 1
  mainform.ProgressBar1.value = 80: DoEvents: If Not Cancel Then Table9_2 9, "A", Cthresh, NCthresh
  mainform.ProgressBar1.value = 83: DoEvents: If Not Cancel Then Table9_2 9, "B", Cthresh, NCthresh
  
  Cthresh = val(frmUserDef.txtCThresh)
  NCthresh = val(frmUserDef.txtNCthresh)
  If Cthresh < 0# Then Cthresh = 0#
  If NCthresh < 0# Then NCthresh = 0#
  
  mainform.ProgressBar1.value = 86: DoEvents: If Not Cancel Then Table9_2 10, "A", Cthresh, NCthresh
  mainform.ProgressBar1.value = 90: DoEvents: If Not Cancel Then Table9_2 10, "B", Cthresh, NCthresh
  mainform.ProgressBar1.value = 100: DoEvents
  
  wbo.Save
  
  Set wbo = Nothing

ErrorHandler:
  If Err.Number <> 0 Then
    MsgBox "Exiting due to error: " & vbCrLf & Err.Description, vbOKOnly, "Error Generating Tables"
    Cancel = False
  End If

End Sub

Function getContamList(fName$) As Integer
  Dim m As Integer
  Dim fle As parmfile
  Dim nrec As Integer
  Dim temp As parmrec
  Dim con As ContamCls
  Set con = New ContamCls
  Dim parm As Collection
 
  If open_parm(fle, fName, 2) Then
    Do Until EOCF(fle.file)
      read_parmrec fle, temp
      Select Case temp.pName
        Case "fui"
          nrec = temp.idx1
          For m = 1 To nrec
            If read_parmrec(fle, temp) Then
              If temp.idx1 = siteIdx Then
                Select Case temp.pName
                  Case "fscasid":
                    If temp.idx3 = 0 Then
                      Set con = New ContamCls
                      'Set con.prog = Nothing
                      con.cas = temp.pval
                      con.pcas = temp.pval
'''                      con.index = temp.idx2
                      
                      cont.Add con, CStr(temp.idx2)
                      
                      Set con.parm = New Collection
                      con.parm.Add New ParmCls, "CLCPFH"
                      con.parm.Add New ParmCls, "CLCPFG"
                      con.parm.Add New ParmCls, "CLRFDG"
                      con.parm.Add New ParmCls, "CLRFDH"
                      con.parm.Add New ParmCls, "CLRFCH"
                      con.parm.Add New ParmCls, "CLFONEI"
                      con.parm.Add New ParmCls, "CLURISKH"
                      con.parm.Add New ParmCls, "CLSFG"
                      con.parm.Add New ParmCls, "CLSFH"
                      con.parm.Add New ParmCls, "CLSFEX"
                      con.parm.Add New ParmCls, "CLURISKG"
                    Else
                      Set con = New ContamCls
                      con.cas = temp.pval
                      con.pcas = cont(CStr(temp.idx2)).cas
'''                      con.index = temp.idx3
                      cont(CStr(temp.idx2)).prog.Add con, CStr(temp.idx3)
                    End If
                  Case "fscname":
                    If temp.idx3 = 0 Then
                      cont(CStr(temp.idx2)).cname = temp.pval
                    Else
                      cont(CStr(temp.idx2)).prog(CStr(temp.idx3)).cname = temp.pval
                    End If
                  Case "clktype":
                    cont(CStr(temp.idx2)).ktype = IIf(1 = val(temp.pval), 1, 0)
                    
                  Case "CLCPFH", "CLCPFG", "CLRFDG", _
                       "CLRFDH", "CLRFCH", "CLFONEI", _
                       "CLURISKH", "CLSFG", "CLSFH", "CLSFEX":
                    If temp.idx3 = 0 Then
                      cont(CStr(temp.idx2)).parm(temp.pName).pName = temp.pName
                      cont(CStr(temp.idx2)).parm(temp.pName).cunit = temp.cunit
                      cont(CStr(temp.idx2)).parm(temp.pName).uunit = temp.uunit
                      cont(CStr(temp.idx2)).parm(temp.pName).pval = temp.pval
                      cont(CStr(temp.idx2)).parm(temp.pName).ref = temp.ref
                    End If
                End Select
              End If
            End If
          Next
          Exit Do
        Case Else
          nrec = temp.idx1
          For m = 1 To nrec
            get_line fle.file
          Next
      End Select
    Loop
    close_parm fle
  Else
    PutError "Can't find or open file " & fName
    End
  End If
  getContamList = cont.count
End Function

Sub getCsm(fName$)
Dim fle As parmfile, parm As parmrec
Dim m As Long, nrec As Long, i As Long, cls As CsmCls, snk As CsmCls
Dim src As CsmCls

  Set csm = New Collection

  If open_parm(fle, fName, 2) Then
    Do Until EOCF(fle.file)
'     If read_parmrec(fle, parm) Then
      read_parmrec fle, parm
        Select Case parm.pName
          Case "csm"
            nrec = parm.idx1
            For m = 1 To nrec
              If read_parmrec(fle, parm) Then
                If parm.idx1 = siteIdx Then
                  Select Case parm.pName
                    '  NOTE:  This is ORDER DEPENDENT!
                    '  This logic will not work if GID CSM is not in assumed order
                    Case "ModId":
                      Set cls = New CsmCls
                      Set cls.sink = New Collection
                      Set cls.src = New Collection
                      cls.id = parm.pval
                      csm.Add cls, cls.id
                    Case "ModLabel":
                      cls.label = parm.pval
                    Case "ModLocX":
                      cls.x = val(parm.pval)
                    Case "ModLocY":
                      cls.y = val(parm.pval)
                    Case "ModModel"
                      cls.model = parm.pval
                    Case "ModSinkId":
                      Set snk = New CsmCls
                      snk.id = parm.pval
                    Case "ModSinkType":
                      snk.stype = parm.pval
                    Case "ModSinkQual":
                      snk.squal = parm.pval
                      cls.sink.Add snk, snk.id
                    Case "ModSrcId":
                      Set src = New CsmCls
                      src.id = parm.pval
                    Case "ModSrcType":
                      src.stype = parm.pval
                    Case "ModSrcQual":
                      src.squal = parm.pval
                      cls.src.Add src, src.id
                  End Select
                End If
              End If
            Next
            Exit Do
          Case Else
            nrec = parm.idx1
            For m = 1 To nrec
              get_line fle.file
            Next
        End Select
'     End If
    Loop
    close_parm fle
  End If
End Sub

Function getDatasets(fName$, pName$, modId As Collection)
Dim fle As parmfile, parm As parmrec
Dim m As Long, nrec As Long, i As Long

  Set modId = New Collection
  If open_parm(fle, fName, 2) Then
    Do Until EOCF(fle.file)
      read_parmrec fle, parm
        Select Case parm.pName
          Case "fui"
            nrec = parm.idx1
            For m = 1 To nrec
              If read_parmrec(fle, parm) Then
                If parm.idx1 = siteIdx Then
                  Select Case parm.pName
                    Case pName
                      modId.Add parm.pval, parm.pval
                  End Select
                End If
              End If
            Next
            Exit Do
          Case Else
            nrec = parm.idx1
            For m = 1 To nrec
              get_line fle.file
            Next
        End Select
'     End If
    Loop
    close_parm fle
  End If
  
End Function


Public Function Table9_Collection(age$, MEDIUM$, pathway$, recep$, contam$, Route$, vtype$, value As Variant) As String

  On Error Resume Next
  Dim colAge As MSComctlLib.Node
  Dim colMedium As MSComctlLib.Node
  Dim colPathway As MSComctlLib.Node
  Dim colRecep As MSComctlLib.Node
  Dim colContam As MSComctlLib.Node
  Dim colRoute As MSComctlLib.Node
  Dim colValue As MSComctlLib.Node
  Dim colType As MSComctlLib.Node
  Dim nod As MSComctlLib.Node
  ' medium = Air
  ' pathway = Leafy vegetables
  ' recep = receptor_intake
  ' contam = Sr90
  ' route = ingestion
  ' vtype = risk or hq
  Dim key As String
  Dim delim As String
  
  delim = t9.PathSeparator
  
  key = age
  Set colAge = t9.Nodes(key)
  If Err.Number <> 0 Then
    Set colAge = t9.Nodes.Add(, , key, age)
    Err.Clear
  End If
  
  key = key & delim & MEDIUM
  Set colMedium = t9.Nodes(key)
  If Err.Number = 35601 Then
    Set colMedium = t9.Nodes.Add(colAge, tvwChild, key, MEDIUM)
  End If
  
  key = key & delim & pathway
  Set colPathway = t9.Nodes(key)
  If Err.Number <> 0 Then
     Set colPathway = t9.Nodes.Add(colMedium, tvwChild, key, pathway)
  End If
  
  key = key & delim & recep
  Set colRecep = t9.Nodes(key)
  If Err.Number <> 0 Then
    Set colRecep = t9.Nodes.Add(colPathway, tvwChild, key, recep)
  End If
  
  key = key & delim & contam
  Set colContam = t9.Nodes(key)
  If Err.Number <> 0 Then
    Set colContam = t9.Nodes.Add(colRecep, tvwChild, key, contam)
  End If
  
  key = key & delim & vtype
  Set colType = t9.Nodes(key)
  If Err.Number <> 0 Then
    Set colType = t9.Nodes.Add(colContam, tvwChild, key, vtype)
  End If
  
  key = key & delim & Route
  Set colValue = t9.Nodes(key)
  If Err.Number = 0 Then
    MsgBox "duplicate value and route"
  Else
    Set colValue = t9.Nodes.Add(colType, tvwChild, key, Route)
    colValue.Tag = value
    colValue.Visible = True
  End If
  
  Table9_Collection = key
End Function

Public Function GetSeries(key As String) As Series
  On Error Resume Next
  Dim t As Series
  Set t = hifSeries(key)
  If Err.Number > 0 Then
    Set t = New Series
  End If
  Set GetSeries = t
End Function

Public Sub DrawSingleBorders(wso As Object, col1, col2, row, val1$, val2 As Double, Optional fmt$ = "")
  Dim selection As Object
  
  On Error GoTo ErrorHandler
  
  xlsSetCell wso, col1, row, val1
  If val2 >= 0# Then
    xlsSetCell wso, col2, row, val2, fmt
  Else
    xlsSetCell wso, col2, row, "--"
  End If
  DrawBorders2 wso, col1, col2, row, 1
ErrorHandler:
  If Err.Number <> 0 Then
    Debug.Print Err.Description
    Resume Next
  End If
End Sub

Public Sub DrawBorders2(wso As Object, col1, col2, row, linestyle)
  With wso.range(excelRange(col1, row, col2, row))
    .borders(xlEdgeTop).linestyle = linestyle
    .borders(xlEdgeBottom).linestyle = linestyle
    .borders(xlInsideVertical).linestyle = xlNone
  End With
  With wso.range(excelRange(col2, row))
    .borders(xlEdgeLeft).linestyle = 1
  End With

End Sub
Public Sub DrawDoubleBorders(wso As Object, col1, col2, row, val1$, val2 As Double, Optional fmt$ = "")
  Dim selection As Object
  
  On Error GoTo ErrorHandler
  
  xlsSetCell wso, col1, row, val1
  If val2 >= 0# Then
    xlsSetCell wso, col2, row, val2, fmt
  Else
    xlsSetCell wso, col2, row, "--"
  End If
  DrawBorders2 wso, col1, col2, row, -4119 ' xlDouble
ErrorHandler:
  If Err.Number <> 0 Then
    Debug.Print Err.Description
    Resume Next
  End If
End Sub

Public Sub DrawBorders(wso As Object, range As String)
  Dim selection As Object
  
  On Error GoTo ErrorHandler
  
  wso.range(range).borders(xlEdgeBottom).linestyle = 1
  With wso.range(range)
    .borders(xlDiagonalDown).linestyle = xlNone
    .borders(xlDiagonalUp).linestyle = xlNone
    .borders(xlEdgeTop).linestyle = xlNone
    .borders(xlEdgeBottom).linestyle = xlNone
    .borders(xlEdgeRight).linestyle = xlNone
    .borders(11).linestyle = xlNone ' xlinsidevertical
    .borders(12).linestyle = xlNone ' xlInsideHorizontal
    .borders(xlDiagonalDown).linestyle = xlNone
    .borders(xlDiagonalUp).linestyle = xlNone
    With .borders(xlEdgeTop)
        .linestyle = -4119 ' xlDouble 1 ' xlContinuous
        .Weight = 4 ' xlThick -4138 ' xlMedium
        .ColorIndex = xlAutomatic
    End With
    With .borders(xlEdgeBottom)
        .linestyle = -4119 ' xlDouble 1 ' xlContinuous
        .Weight = 4 ' xlThick -4138 ' xlMedium
        .ColorIndex = xlAutomatic
    End With
    With .borders(xlEdgeRight)
        .linestyle = -4119 ' xlDouble 1 ' xlContinuous
        .Weight = 4 ' xlThick -4138 ' xlMedium
        .ColorIndex = xlAutomatic
    End With
  End With
ErrorHandler:
  If Err.Number <> 0 Then
    Debug.Print Err.Description
    Resume Next
  End If
End Sub

Public Sub CheckRowWithBorders(wso As Object, row As Long, label As String, range As String)
  While wso.range(label).row <= row
    wso.range(label).EntireRow.Insert
  Wend
  wso.range(range).borders(xlEdgeBottom).linestyle = xlNone
  wso.range(range).borders(xlEdgeTop).linestyle = xlNone

End Sub

Public Function fmtCellLabel(tableno As String, cellname As String)
  fmtCellLabel = "T" & tableno & "_" & cellname
End Function

Sub ToxicityValues(wso As Object, con As ContamCls, row As Long, CL_ING As String, CL_ADJ As String, CL_DERM As String, CL_INH As String, CL_EXT As String)
Dim col As Long
Dim parm As ParmCls
Dim rfd As Double, adj As Double, NA As String
Dim rfdu As String

  On Error GoTo ErrorHandler

  '==================== INGESTION and DERMAL ======================

  If "" <> CL_ING Then
    col = wso.range(CL_ING).Column
    xlsSetCell wso, col, row, NOTAPP
    xlsSetCell wso, col + 1, row, NOTAPP
  End If
  If "" <> CL_ADJ Then
    col = wso.range(CL_ADJ).Column
    xlsSetCell wso, col, row, NOTAPP
  End If
  If "" <> CL_DERM Then
    col = wso.range(CL_DERM).Column
    xlsSetCell wso, col, row, NOTAPP
    xlsSetCell wso, col + 1, row, NOTAPP
  End If

  Set parm = con.parm("CLRFDG")
  If parm.pName <> "" And val(parm.pval) > 0# Then
    rfd = val(parm.pval)
    rfdu = parm.cunit
    Set parm = con.parm("CLFONEI")
    adj = val(parm.pval):
    If Not adj > 0# Then adj = 1#
    
    If "" <> CL_ING Then
      col = wso.range(CL_ING).Column
      xlsSetCell wso, col, row, Format(rfd, SN_FORMAT)
      xlsSetCell wso, col + 1, row, rfdu
    End If
    If "" <> CL_ADJ Then
      col = wso.range(CL_ADJ).Column
      xlsSetCell wso, col, row, Format(adj, SN_FORMAT)
    End If
    If "" <> CL_DERM Then
      col = wso.range(CL_DERM).Column
      xlsSetCell wso, col, row, Format(rfd * adj, SN_FORMAT)
      xlsSetCell wso, col + 1, row, rfdu
    End If
  End If
  If "" = CL_INH Then Exit Sub
  
  '==================== INHALATION ======================

  col = wso.range(CL_INH).Column
  xlsSetCell wso, col, row, NOTAPP
  xlsSetCell wso, col + 1, row, NOTAPP
  
  If "" <> CL_EXT Then
    col = wso.range(CL_EXT).Column
    xlsSetCell wso, col, row, NOTAPP
    xlsSetCell wso, col + 1, row, NOTAPP
  End If
  
  Dim rfc As Double, rfcu As String
  
  '  RfC(mg/m^3) = RfD(mg/kg/d) * 70(kg) / 20(m^3/d)
  Set parm = con.parm("CLRFCH"): rfc = val(parm.pval): rfcu = parm.cunit
  Set parm = con.parm("CLRFDH"): rfd = val(parm.pval): rfdu = parm.cunit
  If rfc > 0# And rfd <= 0 Then
    rfd = rfc * 20# / 70#: rfdu = "mg/kg/d"
  End If
  If rfd > 0 And rfc <= 0 Then
    rfc = rfd * 70# / 20#: rfcu = "mg/m^3"
  End If
  If rfc > 0 Then
    
    col = wso.range(CL_INH).Column
    xlsSetCell wso, col, row, Format(rfc, SN_FORMAT)
    xlsSetCell wso, col + 1, row, rfcu ' "mg/m^3"
    
    If "" <> CL_EXT Then
      col = wso.range(CL_EXT).Column
      xlsSetCell wso, col, row, Format(rfd, SN_FORMAT)
      xlsSetCell wso, col + 1, row, rfcu ' "mg/kg/d"
    End If
  End If
ErrorHandler:
  If Err.Number <> 0 Then
'    MsgBox Err.Description, vbOKOnly, "ToxicityValues"
  End If
End Sub


Sub SlopeFactors(wso As Object, con As ContamCls, row As Long, CL_ING As String, CL_ADJ As String, CL_DERM As String, CL_INH As String, Optional CL_EXT As String = "")
Dim col As Long
Dim parm As ParmCls
Dim csf As Double, adj As Double, derm As Double
Dim csfu As String

  On Error GoTo ErrorHandler

  '==================== INGESTION and DERMAL ======================

  If "" <> CL_ING Then
    col = wso.range(CL_ING).Column
    xlsSetCell wso, col, row, NOTAPP
    xlsSetCell wso, col + 1, row, NOTAPP
  End If
  If "" <> CL_ADJ Then
    col = wso.range(CL_ADJ).Column
    xlsSetCell wso, col, row, NOTAPP
  End If
  If "" <> CL_DERM Then
    col = wso.range(CL_DERM).Column
    xlsSetCell wso, col, row, NOTAPP
    xlsSetCell wso, col + 1, row, NOTAPP
  End If
  
  If con.ktype = 1 Then
    ' rad
    Set parm = con.parm("CLSFG")  ' rad:  Ingestion Slope Factor, Water
  Else
    Set parm = con.parm("CLCPFG") ' chem: Ingestion Cancer Potency Factor, Water
  End If
  csf = val(parm.pval): csfu = parm.cunit
  If 0# < csf Then
'''    If con.ktype <> 1 Then
      Set parm = con.parm("CLFONEI"): adj = val(parm.pval)
      If adj > 0 Then
        adj = 1 / adj
      End If
      derm = csf * adj
'''    End If
    derm = csf * adj
    If "" <> CL_ING Then
      col = wso.range(CL_ING).Column
      xlsSetCell wso, col, row, Format(csf, SN_FORMAT)
      xlsSetCell wso, col + 1, row, csfu
    End If
'    If con.ktype <> 1 Then
      If "" <> CL_ADJ Then
        col = wso.range(CL_ADJ).Column
        xlsSetCell wso, col, row, Format(adj, SN_FORMAT)
      End If
      If "" <> CL_DERM Then
        col = wso.range(CL_DERM).Column
        xlsSetCell wso, col, row, Format(derm, SN_FORMAT)
        xlsSetCell wso, col + 1, row, csfu
      End If
'    End If
  End If
  
  If "" <> CL_INH Then
    '==================== INHALATION ======================
    col = wso.range(CL_INH).Column
    xlsSetCell wso, col, row, NOTAPP
    xlsSetCell wso, col + 1, row, NOTAPP
    
    If con.ktype = 1 Then
      Set parm = con.parm("CLSFH") ' rad: Inhalation Slope Factor [ risk/pCi ]
    Else
      Set parm = con.parm("CLCPFH") ' chem: Inhalation Cancer Potency Factor [ (mg/kg/day)^-1 ]
    End If
    csf = val(parm.pval): csfu = parm.cunit
    If csf > 0 Then
      col = wso.range(CL_INH).Column
      xlsSetCell wso, col, row, Format(csf, SN_FORMAT)
      xlsSetCell wso, col + 1, row, csfu
    End If
  End If
  
  If "" <> CL_EXT And con.ktype = 1 Then
    '==================== EXTERNAL ======================
    col = wso.range(CL_EXT).Column
    xlsSetCell wso, col, row, NOTAPP
    xlsSetCell wso, col + 1, row, NOTAPP
    
    Set parm = con.parm("CLSFEX") ' rad: Inhalation Slope Factor [ risk/pCi ]
    csf = val(parm.pval): csfu = parm.cunit
    If csf > 0 Then
      col = wso.range(CL_EXT).Column
      xlsSetCell wso, col, row, Format(csf, SN_FORMAT)
      xlsSetCell wso, col + 1, row, csfu
    End If
  End If
  
ErrorHandler:
  If Err.Number <> 0 Then
'    MsgBox Err.Description, vbOKOnly, "SlopeFactors"
  End If
End Sub

Public Function areModulesConnected(Source As String, sink As String) As Boolean
  Dim csrc As CsmCls, csnk As CsmCls
  
  Set csrc = csm(Source)
  For Each csnk In csrc.sink
    If sink = csnk.id Then
      areModulesConnected = True
      Exit Function
    End If
  Next csnk
  areModulesConnected = False
End Function

Public Function areSourcesConnected() As Boolean
  Dim cmod As CsmCls, csrc As CsmCls
  Dim epfid As String, rifid As String, hifid As String
  
  areSourcesConnected = False
  
  getCsm FUIName
  Set cmod = csm(modName)
  For Each csrc In cmod.src
    Select Case csrc.stype
      Case "epf": epfid = csrc.id
      Case "rif": rifid = csrc.id
      Case "hif": hifid = csrc.id
    End Select
  Next csrc
  areSourcesConnected = _
    areModulesConnected(epfid, rifid) And _
    areModulesConnected(rifid, hifid)
    
End Function
