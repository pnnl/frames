Attribute VB_Name = "HIFAPI"
Option Compare Text
Option Explicit

Type ContamList
  cas As String
  name As String
  nprog As Integer
  pcas() As String
  pname() As String
  ktype As Long
End Type

Type AllContamList
  cas As String
  name As String
  pcas As String
  pname As String
End Type

Public Const MAXFIELD = 4096
Public Const TIMESTRING_LEN = 8192 ' size of string passed to LoadStartTimes
Public Const NOT_AVAILABLE = "Not Available"

Public Type typeExpRoute
  Route As String
  Path As String
  Unit As String
  name As String
End Type

Public Type dataset
  dstype As String
  locname As String
  medtype As String
  name As String
  endpoint As String
  numpt As Long
  numAge As Long
  numcon As Long
  nsites As Long
  norgans As Long
  x() As Double
  y() As Double
  agemin() As Double
  agemax() As Double
  Sites() As String
  Organs() As String
End Type

Public cont() As ContamList
Public allCont() As ContamList
Public expRoute() As typeExpRoute
Public endpt() As Variant
Public Routes As Variant
Public exposure() As Variant
Public ds() As dataset
Public numcol As Long
Public numds As Long, numpt As Long, numAge As Long, ncancer As Long, numct As Integer
Public PDFFileType As String
Public rvalues() As Double
Public rtimes() As Double

Sub initExposure()
  Dim i As Integer, j As Integer

  If InStr("hif", PDFFileType) > 0 Then Exit Sub
  
  Routes = Array("", "ingestion", "inhalation", "dermal", "external")
  
  ReDim exposure(22, UBound(Routes))
  
  For i = 0 To 22
    For j = 1 To 4
      exposure(i, j) = True
    Next j
    Select Case i
      Case 0: 'exposure(i) = Array("all", True, True, True, True)
        exposure(i, 0) = "all"
      Case 1: ' exposure(i) = Array("air", False, True, False, True)
        exposure(i, 0) = "air"
        exposure(i, 1) = False: exposure(i, 3) = False
      Case 2: ' exposure(i) = Array("indoor air", False, True, False, False)
        exposure(i, 0) = "indoor air"
        exposure(i, 1) = False  ' ingestion
        exposure(i, 3) = False  ' dermal
        exposure(i, 4) = False  ' external
      Case 3: ' exposure(i) = Array("ground", True, True, True, True)
        exposure(i, 0) = "ground"
      Case 4: ' exposure(i) = Array("leafy vegetables", True, False, False, False)
        exposure(i, 0) = "leafy vegetables"
        exposure(i, 2) = False  ' inhalation
        exposure(i, 3) = False  ' dermal
        exposure(i, 4) = False  ' external
      Case 5: ' exposure(i) = Array("root vegetables", True, False, False, False)
        exposure(i, 0) = "root vegetables"
        exposure(i, 2) = False  ' inhalation
        exposure(i, 3) = False  ' dermal
        exposure(i, 4) = False  ' external
      Case 6: ' exposure(i) = Array("fruit", True, False, False, False)
        exposure(i, 0) = "fruit"
        exposure(i, 2) = False  ' inhalation
        exposure(i, 3) = False  ' dermal
        exposure(i, 4) = False  ' external
      Case 7: ' exposure(i) = Array("grain", True, False, False, False)
        exposure(i, 0) = "grain"
        exposure(i, 2) = False  ' inhalation
        exposure(i, 3) = False  ' dermal
        exposure(i, 4) = False  ' external
      Case 8: ' exposure(i) = Array("beef or meat", True, False, False, False)
        exposure(i, 0) = "beef or meat"
        exposure(i, 2) = False  ' inhalation
        exposure(i, 3) = False  ' dermal
        exposure(i, 4) = False  ' external
      Case 9: ' exposure(i) = Array("poultry", True, False, False, False)
        exposure(i, 0) = "poultry"
        exposure(i, 2) = False  ' inhalation
        exposure(i, 3) = False  ' dermal
        exposure(i, 4) = False  ' external
      Case 10: ' exposure(i) = Array("milk", True, False, False, False)
        exposure(i, 0) = "milk"
        exposure(i, 2) = False  ' inhalation
        exposure(i, 3) = False  ' dermal
        exposure(i, 4) = False  ' external
      Case 11: ' exposure(i) = Array("eggs", True, False, False, False)
        exposure(i, 0) = "eggs"
        exposure(i, 2) = False  ' inhalation
        exposure(i, 3) = False  ' dermal
        exposure(i, 4) = False  ' external
      Case 12: ' exposure(i) = Array("soil", True, True, True, True)
        exposure(i, 0) = "soil"
      Case 13: ' exposure(i) = Array("swimming", True, False, True, True)
        exposure(i, 0) = "swimming"
        exposure(i, 2) = False  ' inhalation
      Case 14: ' exposure(i) = Array("boating", False, False, False, True)
        exposure(i, 0) = "boating"
        exposure(i, 1) = False  ' ingestion
        exposure(i, 2) = False  ' inhalation
        exposure(i, 3) = False  ' dermal
      Case 15: ' exposure(i) = Array("shoreline", True, False, True, True)
        exposure(i, 0) = "shoreline"
        exposure(i, 2) = False  ' inhalation
      Case 16: ' exposure(i) = Array("water", True, False, False, False)
        exposure(i, 0) = "water"
        exposure(i, 2) = False  ' inhalation
        exposure(i, 3) = False  ' dermal
        exposure(i, 4) = False  ' external
      Case 17: ' exposure(i) = Array("fish or finfish", True, False, False, False)
        exposure(i, 0) = "fish or finfish"
        exposure(i, 2) = False  ' inhalation
        exposure(i, 3) = False  ' dermal
        exposure(i, 4) = False  ' external
      Case 18: ' exposure(i) = Array("mollusks", True, False, False, False)
        exposure(i, 0) = "mollusks"
        exposure(i, 2) = False  ' inhalation
        exposure(i, 3) = False  ' dermal
        exposure(i, 4) = False  ' external
      Case 19: ' exposure(i) = Array("crustacea or shellfish", True, False, False, False)
        exposure(i, 0) = "crustacea or shellfish"
        exposure(i, 2) = False  ' inhalation
        exposure(i, 3) = False  ' dermal
        exposure(i, 4) = False  ' external
      Case 20: ' exposure(i) = Array("aquatic plants", True, False, False, False)
        exposure(i, 0) = "aquatic plants"
        exposure(i, 2) = False  ' inhalation
        exposure(i, 3) = False  ' dermal
        exposure(i, 4) = False  ' external
      Case 21: ' exposure(i) = Array("showering", True, True, True, False)
        exposure(i, 0) = "showering"
        exposure(i, 4) = False  ' external
      Case 22: ' exposure(i) = Array("other", True, False, False, False)
        exposure(i, 0) = "other"
        exposure(i, 2) = False  ' inhalation
        exposure(i, 3) = False  ' dermal
        exposure(i, 4) = False  ' external
    End Select
  Next
End Sub

Public Function hifLoadDatasets() As Long
Dim i As Long, j As Long, r As Integer, c As Integer, retc As Long
Dim tstr As String, tstr2 As String, tstr3 As String
Dim Source As String, fname As String
Dim cbolist As String

  numds = 0
  ReDim ds(0)
  
  Source = ModName
  fname = Left$(FUIName, Len(FUIName) - 4)
  PDFFileType = "hif"
  
  numct = getContamList(cont())
  Call initExposure
  
  numds = hifOpen(fname, Source)
  If 0 = hifChecksum() Then
    numds = 0
    Call PutError("The HIF file is incomplete or incorrectly formatted.")
    hifLoadDatasets = numds
    Exit Function
  End If
    
  endpt = Array("cancer incidence", "cancer fatalities", "cancer plus severe hereditary effects", _
                  "radiation dose", "hazard quotient/hazard index")
                  
  ReDim ds(numds)
  tstr = String(1024, Chr(0))
  tstr2 = String(1024, Chr(0))
  tstr3 = String(1024, Chr(0))
  cbolist = ""
  For i = 1 To numds
    
     retc = hifGetSetInfo(i - 1, ds(i).numpt, ds(i).numAge, ds(i).nsites, ds(i).norgans, tstr, tstr2, tstr3)
     ds(i).locname = Left(tstr, InStr(tstr, Chr(0)) - 1)
     ds(i).medtype = Left(tstr2, InStr(tstr2, Chr(0)) - 1)
     ds(i).dstype = Left(tstr3, InStr(tstr3, Chr(0)) - 1)
     ReDim ds(i).x(ds(i).numpt)
     ReDim ds(i).y(ds(i).numpt)
     For j = 1 To ds(i).numpt
       retc = hifGetExposurePoint(i - 1, j - 1, ds(i).x(j), ds(i).y(j))
     Next j
   
     ReDim ds(i).agemin(ds(i).numAge)
     ReDim ds(i).agemax(ds(i).numAge)
     For j = 1 To ds(i).numAge
       retc = hifGetAgeGroup(i - 1, j - 1, ds(i).agemin(j), ds(i).agemax(j))
     Next j
     
     ReDim ds(i).Sites(ds(i).nsites)
     For j = 1 To ds(i).nsites
       tstr = String(1024, Chr(0))
       hifGetCancerOrgan i - 1, j - 1, tstr
       ds(i).Sites(j) = Left(tstr, InStr(tstr, Chr(0)) - 1)
     Next j
     
     ReDim ds(i).Organs(ds(i).norgans)
     For j = 1 To ds(i).norgans
       tstr = String(1024, Chr(0))
       hifGetDoseOrgan i - 1, j - 1, tstr
       ds(i).Organs(j) = Left(tstr, InStr(tstr, Chr(0)) - 1)
     Next j
  Next i
  hifLoadDatasets = numds
End Function

Function getEndpointUnit(endpt As String, Route As String, Path As String) As String
Dim i As Integer, Unit As String
  getEndpointUnit = ""
  For i = 1 To UBound(expRoute)
    If endpt = expRoute(i).name Then
      getEndpointUnit = expRoute(i).Unit
      Exit Function
    End If
  Next i
End Function

Sub GetUniqueRoutesAndPathways(key$, nd&, NA&, casid$, pcasid$, Optional reset As Boolean = True)
Dim timestring As String
Dim numtimes As Long
Dim timeindex As Integer, i As Integer, j As Integer
Dim Route As String, Path As String, Unit As String, name As String
Dim retc As Integer, totc As Long

Dim found As Boolean
  Select Case key
    Case "epf": numtimes = epfGetTimeCount(nd, pcasid)
    Case "rif": numtimes = rifGetTimeCount(nd, NA, pcasid)
    Case "hif": numtimes = hifGetTimeCount(nd, NA, pcasid)
  End Select
  If reset Then ReDim expRoute(0)
  If reset Then ReDim Routes(0)
  For timeindex = 0 To numtimes - 1
    Select Case key
      Case "epf": retc = epfLoadRoutesAndPathwaysByTime(nd, casid, pcasid, timeindex)
      Case "rif": retc = rifLoadRoutesAndPathwaysByTime(nd, NA, casid, pcasid, timeindex)
      Case "hif": retc = hifLoadRoutesAndPathwaysByTime(nd, NA, casid, pcasid, timeindex)
    End Select
    For i = 1 To retc
      Route = String(128, Chr(0)):  Path = String(128, Chr(0))
      Unit = String(128, Chr(0)):   name = String(128, Chr(0))
      Select Case key
        Case "epf": epfGetRouteAndPathway nd, i - 1, Path, Route, Unit
        Case "rif": rifGetRouteAndPathway nd, i - 1, Path, Route, name, Unit
        Case "hif": hifGetRouteAndPathway nd, i - 1, Path, Route, name, Unit
      End Select
      Route = Left(Route, InStr(Route, Chr(0)) - 1)
      Path = Left(Path, InStr(Path, Chr(0)) - 1)
      Unit = Left(Unit, InStr(Unit, Chr(0)) - 1)
      name = Left(name, InStr(name, Chr(0)) - 1)
      found = False
      For j = 1 To UBound(expRoute)
        found = (expRoute(j).Route = Route And expRoute(j).Path = Path And _
                  expRoute(j).Unit = Unit And expRoute(j).name = name)
        If found Then Exit For
      Next
      If Not found Then
        totc = 1 + UBound(expRoute)
        ReDim Preserve expRoute(totc)
          expRoute(totc).Route = Route
          expRoute(totc).Path = Path
          expRoute(totc).Unit = Unit
          expRoute(totc).name = name
      End If
    Next i
    ReDim exposure(0, 0)
  Next timeindex
End Sub

Function readTimeSeries(nd As Long, _
                        age As Long, _
                        pcas As String, _
                        point As Long, _
                        organ As Long, _
                        Unit As String, _
                        epunit As String, _
                        Route As String, _
                        Path As String, _
                        cas As String)
   Dim ntimes As Long
  ntimes = hifLoadTimeSeries(nd - 1, _
                            point - 1, _
                            age - 1, _
                            organ - 1, _
                            cas, _
                            pcas, _
                            Path, _
                            Route, _
                            epunit, _
                            Unit)
  If ntimes > 0 Then
    ReDim rvalues(ntimes): ReDim rtimes(ntimes)
    hifGetTimeSeries nd - 1, rtimes(1), rvalues(1)
  End If
  readTimeSeries = ntimes
End Function

Function findPathIndex(Path As String, Route As String, HealthImpact As String)
  Dim i As Integer
  For i = 1 To UBound(expRoute)
    If (Path = expRoute(i).Path Or Path = "all") And _
       (Route = expRoute(i).Route Or Route = "all") And _
       HealthImpact = expRoute(i).Unit Then
      Exit For
    End If
  Next i
  If i > UBound(expRoute) Then
    findPathIndex = -1
  Else
    findPathIndex = i
  End If
End Function

'Each entry in the list must be followed by a ',' including the last entry
Public Sub SetComboFromString(box As ComboBox, listitems As String)
    Dim i As Integer
    Dim temp As String
    Dim csv As Variant
    
    box.Clear
    csv = Split(listitems, ",")
    If 0 > UBound(csv) Then Exit Sub
    For i = 0 To UBound(csv)
      If csv(i) <> "" Then box.AddItem csv(i)
    Next i
    Exit Sub
    
    temp = ""
    For i = 1 To Len(listitems)
        If Mid(listitems, i, 1) = "," Then
            box.AddItem temp
            temp = ""
        Else
            temp = temp + Mid(listitems, i, 1)
        End If
    Next
End Sub

' A handy function to get the constituent (contaminant) list
Function getContamList(cont() As ContamList) As Integer
  Dim i As Integer, j As Integer, k As Integer, l As Integer, m As Integer
  Dim prm As String
  Dim prefix As String
  Dim fle As parmfile
  Dim sval As Boolean
  Dim numcon As Integer
  Dim np As Integer
  Dim nrec As Integer
  Dim pvalue As String
  Dim nds As Integer
  Dim n As Integer
  Dim remap() As Integer
  Dim sol As String
  Dim temp As parmrec
 
  numcon = 0
  
  ReDim cont(0)
  If open_parm(fle, FUIName, 2) Then
    Do Until EOCF(fle.file)
'     If read_parmrec(fle, temp) Then
      read_parmrec fle, temp
        Select Case temp.pname
          Case "fui"
            nrec = temp.idx1
            For m = 1 To nrec
              If read_parmrec(fle, temp) Then
                If temp.idx1 = siteIdx Then
                  Select Case temp.pname
                    Case "fscasid"
                      If temp.idx3 = 0 Then
                        If temp.idx2 > numcon Then
                          numcon = temp.idx2
                          ReDim Preserve cont(numcon)
                        End If
                        cont(temp.idx2).cas = temp.pval
                      Else
                        If temp.idx3 > cont(temp.idx2).nprog Then
                          cont(temp.idx2).nprog = temp.idx3
                          ReDim Preserve cont(temp.idx2).pcas(temp.idx3)
                          ReDim Preserve cont(temp.idx2).pname(temp.idx3)
                        End If
                        cont(temp.idx2).pcas(temp.idx3) = temp.pval
                      End If
                    Case "fscname"
                      If temp.idx3 = 0 Then
                        If temp.idx2 > numcon Then
                          numcon = temp.idx2
                          ReDim Preserve cont(numcon)
                        End If
                        cont(temp.idx2).name = temp.pval
                      Else
                        If temp.idx3 > cont(temp.idx2).nprog Then
                          cont(temp.idx2).nprog = temp.idx3
                          ReDim Preserve cont(temp.idx2).pcas(temp.idx3)
                          ReDim Preserve cont(temp.idx2).pname(temp.idx3)
                        End If
                        cont(temp.idx2).pname(temp.idx3) = temp.pval
                      End If
                    Case "clktype"
                      If temp.idx3 = 0 Then
                        If temp.idx2 > numcon Then
                          numcon = temp.idx2
                          ReDim Preserve cont(numcon)
                        End If
                        cont(temp.idx2).ktype = val(temp.pval)
                      End If
                  End Select
                End If
              End If
            Next
          Case Else
            nrec = temp.idx1
            For m = 1 To nrec
              get_line fle.file
            Next
        End Select
'     End If
    Loop
    close_parm fle
    
  Else
    PutError "Can't find or open file " & FUIName
'   put_val errfile, "Can't find or open file " & FuiName
'   put_line errfile
'   close_csv errfile
    End
  End If
  
  getContamList = UBound(cont)

End Function


