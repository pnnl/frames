Attribute VB_Name = "basEXP"
Option Explicit
Option Compare Text

Public Type Exposure
  Route As String
  Path As String
  measure As String
  Unit As String
End Type

Public Type expDataSet
  locExp As String
  locName As String
  locType As String
  numPt As Long
  numAge As Long
  numCon As Long
  ncOrgans As Long
  ndOrgans As Long
  X() As Double
  Y() As Double
  ageMin() As Double
  ageMax() As Double
  cOrgans() As String
  dOrgans() As String
End Type

Public CBO_DS   As Long
Public CBO_XY   As Long
Public CBO_OR   As Long
Public CBO_VU   As Long
Public CBO_EFX  As Long
Public CBO_AGE  As Long
Public CBO_CHM  As Long
Public CBO_FLX  As Long
Public CBO_PTH  As Long
Public CBO_RTE  As Long
Public CBO_MSR  As Long
Public CBO_DEP  As Long
Public CBO_UNT  As Long
Public CBO_ORG  As Long
Public CBO_LOCX As Long
Public CBO_LOCY As Long

Public CBO_JRS  As Long
Public CBO_TME  As Long

Public Const NOT_AVAILABLE = "Not Available"
Public Const PLUSPROGENY = " (+progeny)"
Public Const ALLCHEMS = "All Chemicals"
Public Const ALLRADS = "All Radionuclides"
Public Const ALLITEMS = "All"
Public Const REPORTALL = "Report all"
Public Const MAXIMUM = "Maximum"

'variables used for work space
Public numDS As Long
Public ds() As expDataSet
Public allCAS() As CASIDList
Public uExposure() As Exposure
Public uVary() As String
Public uCert() As String
Public uRoutes() As String
Public uPaths() As String
Public uMeasure() As String

'spread control work space
Public rowHeight As Double
Public lastCol As Long
Public numCol As Long
Public numSel As Long
Public xlabel As String
Public ylabel As String
Public xValues() As Double
Public yValues() As Double

Public IgnoreEvents As Boolean
  
Function GetSelText(row As Long, col As Long) As String
  GetSelText = ""
  activeSpread.row = row
  activeSpread.col = col
  GetSelText = activeSpread.Text
End Function
  
Function GetSelIdx(row As Long, col As Long) As String
  GetSelIdx = -1
  activeSpread.row = row
  activeSpread.col = col
  GetSelIdx = activeSpread.TypeComboBoxCurSel
End Function
  
Function GetSelList(row As Long, col As Long) As Variant
  GetSelList = ""
  activeSpread.row = row
  activeSpread.col = col
  GetSelList = activeSpread.TypeComboBoxList
End Function
  
Sub SetSelList(row As Long, col As Long, list As String)
  If list = "" Then list = NOT_AVAILABLE
  activeSpread.row = row
  activeSpread.col = col
  activeSpread.TypeComboBoxList = list
  activeSpread.TypeComboBoxCurSel = 0
End Sub

Function GetAllContIdx(name As String)
Dim i As Long
Dim pcas As String

  i = InStr(name, " (")
  If i > 0 Then
    pcas = Right(name, Len(name) - i - 1)
    pcas = Left(pcas, Len(pcas) - 1)
    name = Left(name, i - 1)
    For i = 1 To UBound(allCAS)
      If allCAS(i).name = name And allCAS(i).pcas(0) = pcas Then Exit For
    Next
  Else
    For i = 1 To UBound(allCAS)
      If allCAS(i).name = name Then Exit For
    Next
  End If
  If i > UBound(allCAS) Then
    GetAllContIdx = -1
  Else
    GetAllContIdx = i
  End If
End Function

Sub updateConstituentList()
Dim i As Long

  ReDim allCAS(numCAS)
  For i = 1 To numCAS
    allCAS(i) = cas(i)
    If cas(i).nprog = 0 Then
      ReDim allCAS(i).pcas(0)
      ReDim allCAS(i).pName(0)
    End If
    allCAS(i).pcas(0) = cas(i).cas
    allCAS(i).pName(0) = cas(i).name
    allCAS(i).ktype = IIf(cas(i).ktype = 1, 1, 0)
  Next
End Sub

Sub updateOrganisms(col As Long)
Dim i As Long
Dim j As Long
Dim num As Long
Dim id As String
Dim name As String
Dim cbolist As String

  If CBO_DS < 0 Then Exit Sub
  cbolist = ""
  i = GetSelIdx(CBO_DS, col) + 1
  Select Case pdcfType
  Case "hqf", "hqf1", "hqf2"
    If ds(i - 1).locType = "Aquatic HQ" Or ds(i - 1).locType = "Terrestrial HQ" Then
      activeSpread.SetText col - 1, CBO_OR, "Location"
      num = exfGetNumLocation(i)
      For j = 1 To num
        exfGetLocationName i, j, name, id
        cbolist = cbolist + IIf(cbolist <> "", Chr$(9), "") + name + " (" + id + ")"
      Next
    Else
      activeSpread.SetText col - 1, CBO_OR, "Organism"
      num = exfGetNumOrganism(i)
      For j = 1 To num
        exfGetOrganismName i, j, name, id
        cbolist = cbolist + IIf(cbolist <> "", Chr$(9), "") + name + " (" + id + ")"
      Next
    End If
  Case "bbf"
    num = bbfGetNumOrganism(i)
    For j = 1 To num
      bbfGetOrganismName i, j, name, id
      cbolist = cbolist + IIf(cbolist <> "", Chr$(9), "") + name + " (" + id + ")"
    Next
  End Select
  SetSelList CBO_OR, col, cbolist
End Sub

Sub updateContams(col As Long)
Dim i As Long
Dim j As Long
Dim k As Long
Dim num As Long
Dim name As String
Dim cas As String
Dim cbolist As String

  If CBO_DS < 0 Then Exit Sub
  cbolist = ""
  i = GetSelIdx(CBO_DS, col) + 1
  Select Case pdcfType
  Case "ato"
    num = atoGetConstituentCount(i - 1)
    For k = 0 To num - 1
      atoGetCasID i - 1, k, -1, cas
      atoGetChemName i - 1, k, -1, name
      cbolist = cbolist + IIf(cbolist <> "", Chr$(9), "") + name + " (" + cas + ")"
    Next
  Case "scf", "sufscf", "mrkscf"
    num = scfGetNumContam(i)
    For k = 1 To num
      scfGetContamName i, k, 0, name, cas
      cbolist = cbolist + IIf(cbolist <> "", Chr$(9), "") + name + " (" + cas + ")"
    Next
  Case "aff"
    num = affGetNumContam(i)
    For k = 1 To num
      affGetContamName i, k, 0, name, cas
      cbolist = cbolist + IIf(cbolist <> "", Chr$(9), "") + name + " (" + cas + ")"
    Next
  Case "wff"
    num = wffGetNumContam(i)
    For k = 1 To num
      wffGetContamName i, k, 0, name, cas
      cbolist = cbolist + IIf(cbolist <> "", Chr$(9), "") + name + " (" + cas + ")"
    Next
  Case "wcf", "sufwcf", "mrkwcf"
    num = wcfGetNumContam(i)
    For k = 1 To num
      wcfGetContamName i, k, 0, name, cas
      cbolist = cbolist + IIf(cbolist <> "", Chr$(9), "") + name + " (" + cas + ")"
    Next
  Case "hqf", "hqf1", "hqf2"
    j = GetSelIdx(CBO_OR, col) + 1
    num = exfGetNumContam(i, j)
    For k = 1 To num
      exfGetContamName i, j, k, name, cas
      cbolist = cbolist + IIf(cbolist <> "", Chr$(9), "") + name + " (" + cas + ")"
    Next
  Case "bbf"
    j = GetSelIdx(CBO_OR, col) + 1
    num = bbfGetNumContam(i, j)
    For k = 1 To num
      bbfGetContamName i, j, k, 0, name, cas
      cbolist = cbolist + IIf(cbolist <> "", Chr$(9), "") + name + " (" + cas + ")"
    Next
  End Select
  SetSelList CBO_CHM, col, cbolist
End Sub

Sub updateWaterFlux(col As Long)
Dim i As Long
Dim num As Long
Dim cbolist As String

  If CBO_DS < 0 Then Exit Sub
  cbolist = ""
  i = GetSelIdx(CBO_DS, col) + 1
  num = wffGetNumFluxTypes(i)
  If num = 2 Then
    cbolist = cbolist + IIf(cbolist <> "", Chr$(9), "") + "Adsorbed"
    cbolist = cbolist + IIf(cbolist <> "", Chr$(9), "") + "Dissolved"
  Else
    cbolist = cbolist + IIf(cbolist <> "", Chr$(9), "") + "Total"
  End If
  SetSelList CBO_FLX, col, cbolist
End Sub

Sub updateAirFlux(col As Long)
Dim i As Long
Dim j As Long
Dim num As Long
Dim fsize As Double
Dim density As Double
Dim name As String
Dim suom As String
Dim duom As String
Dim cbolist As String

  If CBO_DS < 0 Then Exit Sub
  cbolist = ""
  i = GetSelIdx(CBO_DS, col) + 1
  num = affGetNumFluxTypes(i)
  For j = 1 To num
    affGetFluxType i, j, name, fsize, suom, density, duom
    cbolist = cbolist + IIf(cbolist <> "", Chr$(9), "") + name ' + " (" + CStr(fsize) + " " + suom + ")"
  Next
  SetSelList CBO_FLX, col, cbolist
End Sub

Sub updateEffects(col As Long)
Dim i As Long
Dim j As Long
Dim k As Long
Dim m As Long
Dim num As Long
Dim effect As String
Dim eunit As String
Dim cbolist As String

  If CBO_DS < 0 Then Exit Sub
  cbolist = ""
  i = GetSelIdx(CBO_DS, col) + 1
  j = GetSelIdx(CBO_OR, col) + 1
  k = GetSelIdx(CBO_CHM, col) + 1
  num = exfGetNumEffects(i, j, k)
  For m = 1 To num
    exfGetSeriesProperties i, j, k, m, effect, eunit, eunit
    cbolist = cbolist + IIf(cbolist <> "", Chr$(9), "") + effect
  Next
  SetSelList CBO_EFX, col, cbolist
End Sub

Sub updateVULevels(col As Long)
Dim i As Long
Dim j As Long
Dim num As Long
Dim vunit As String
Dim uunit As String
Dim cbolist As String

  If CBO_DS < 0 Then Exit Sub
  cbolist = ""
  i = GetSelIdx(CBO_DS, col) + 1
  num = bbfGetNumVULevels(i)
  For j = 1 To num
    bbfGetVULevel i, j, vunit, uunit
    cbolist = cbolist + IIf(cbolist <> "", Chr$(9), "") + "v" + vunit + " u" + uunit
  Next
  SetSelList CBO_VU, col, cbolist
End Sub

Sub updateLocations(col As Long)
Dim i As Long
Dim j As Long
Dim name As String
Dim cbolist As String

  If CBO_DS < 0 Or CBO_XY < 0 Then Exit Sub
  cbolist = ""
  i = GetSelIdx(CBO_DS, col)
  For j = 0 To ds(i).numPt - 1
    name = "(" + CStr(ds(i).X(j)) + ", " + CStr(ds(i).Y(j)) + ") km"
    cbolist = cbolist + IIf(cbolist <> "", Chr$(9), "") + name
  Next
  SetSelList CBO_XY, col, cbolist
End Sub

Sub updateAgeGroup(col As Long)
Dim i As Long
Dim j As Long
Dim name As String
Dim cbolist As String

  If CBO_DS < 0 Or CBO_AGE < 0 Then Exit Sub
  cbolist = ""
  i = GetSelIdx(CBO_DS, col)
  For j = 0 To ds(i).numAge - 1
    name = CStr(ds(i).ageMin(j)) + " to " + CStr(ds(i).ageMax(j))
    cbolist = cbolist + IIf(cbolist <> "", Chr$(9), "") + name
  Next
  SetSelList CBO_AGE, col, cbolist
End Sub

Sub updateConstituents(col As Long)
  Dim i As Long
  Dim rads As Long
  Dim chems As Long
  Dim cbolist As String
  
  cbolist = ""
  For i = 1 To UBound(allCAS)
    If 0 < Len(cbolist) Then cbolist = cbolist + Chr(9)
    cbolist = cbolist + allCAS(i).name

    'used to want to display parent now just want to display cas and name
    'If allCAS(i).pcas(0) <> allCAS(i).cas Then cbolist = cbolist + " (" & allCAS(i).pcas(0) & ")"
    If allCAS(i).pcas(0) = allCAS(i).cas Then cbolist = cbolist + " (" & allCAS(i).pcas(0) & ")"
    
    If allCAS(i).cas = allCAS(i).pcas(0) Then
      If allCAS(i).ktype = 1 Then rads = rads + 1
      If allCAS(i).ktype <> 1 Then chems = chems + 1
    End If
  Next i
  If pdcfType = "hif" Then
    If chems > 0 Then cbolist = ALLCHEMS + Chr(9) + cbolist
    If rads > 0 Then cbolist = ALLRADS + Chr(9) + cbolist
  End If
  SetSelList CBO_CHM, col, cbolist
End Sub

Sub updateMeasures(col As Long)
  Dim i As Long
  Dim j As Long
  Dim cbolist As String
  
  If CBO_MSR < 0 Then Exit Sub
  cbolist = ""
  ReDim uMeasure(0)
  For i = 1 To UBound(uExposure)
    For j = 1 To UBound(uMeasure)
      If uMeasure(j) = uExposure(i).measure Then Exit For
    Next
    If j > UBound(uMeasure) Then
      ReDim Preserve uMeasure(j)
      uMeasure(j) = uExposure(i).measure
      cbolist = cbolist & IIf(cbolist <> "", Chr(9), "") & uExposure(i).measure
    End If
  Next
  SetSelList CBO_MSR, col, cbolist
End Sub

Sub updateUnits(col As Long)
  Dim i As Long
  Dim j As Long
  Dim measure As String
  Dim cbolist As String
  Dim uUnits() As String
  
  If CBO_UNT < 0 Then Exit Sub
  cbolist = ""
  ReDim uUnits(0)
  measure = GetSelText(CBO_MSR, col)
  For i = 1 To UBound(uExposure)
    If measure = uExposure(i).measure Then
      For j = 1 To UBound(uUnits)
        If uUnits(j) = uExposure(i).Unit Then Exit For
      Next
      If j > UBound(uUnits) Then
        ReDim Preserve uUnits(j)
        uUnits(j) = uExposure(i).Unit
        cbolist = cbolist & IIf(cbolist <> "", Chr(9), "") & uExposure(i).Unit
      End If
    End If
  Next
  SetSelList CBO_UNT, col, cbolist
End Sub

Sub updateRoutes(col As Long)
  Dim i As Long
  Dim j As Long
  Dim measure As String
  Dim Unit As String
  Dim cbolist As String

  If CBO_RTE < 0 Then Exit Sub
  cbolist = ""
  ReDim uRoutes(0)
  Select Case pdcfType
    Case "epf":
      For i = 1 To UBound(uExposure)
        For j = 1 To UBound(uRoutes)
          If uRoutes(j) = uExposure(i).Route Then Exit For
        Next
        If j > UBound(uRoutes) Then
          ReDim Preserve uRoutes(j)
          uRoutes(j) = uExposure(i).Route
          cbolist = cbolist & IIf(cbolist <> "", Chr(9), "") & uExposure(i).Route
        End If
      Next i
    
    Case "rif", "hif":
      measure = GetSelText(CBO_MSR, col)
      Unit = GetSelText(CBO_UNT, col)
      For i = 1 To UBound(uExposure)
        If uExposure(i).measure = measure And uExposure(i).Unit = Unit Then
          For j = 1 To UBound(uRoutes)
            If uRoutes(j) = uExposure(i).Route Then Exit For
          Next
          If j > UBound(uRoutes) Then
            ReDim Preserve uRoutes(j)
            uRoutes(j) = uExposure(i).Route
            cbolist = cbolist & IIf(cbolist <> "", Chr(9), "") & uExposure(i).Route
          End If
        End If
      Next
      If (pdcfType = "hif") And UBound(uRoutes) > 0 Then cbolist = ALLITEMS & Chr(9) & cbolist
  End Select
  SetSelList CBO_RTE, col, cbolist
End Sub

Sub updatePaths(col As Long)
  Dim i As Long
  Dim j As Long
  Dim measure As String
  Dim Unit As String
  Dim rte As String
  Dim cbolist As String
  
  If CBO_RTE < 0 Or CBO_PTH < 0 Then Exit Sub
  cbolist = ""
  ReDim uPaths(0)
  Select Case pdcfType
    Case "epf":
      rte = GetSelText(CBO_RTE, col)
      For i = 1 To UBound(uExposure)
        If uExposure(i).Route = rte Then
          For j = 1 To UBound(uPaths)
            If uPaths(j) = uExposure(i).Path Then Exit For
          Next
          If j > UBound(uPaths) Then
            ReDim Preserve uPaths(j)
            uPaths(j) = uExposure(i).Path
            cbolist = cbolist & IIf(cbolist <> "", Chr(9), "") & uExposure(i).Path
          End If
        End If
      Next
      
    Case "rif", "hif":
      rte = GetSelText(CBO_RTE, col)
      measure = GetSelText(CBO_MSR, col)
      Unit = GetSelText(CBO_UNT, col)
      For i = 1 To UBound(uExposure)
        If uExposure(i).measure = measure And uExposure(i).Unit = Unit And (uExposure(i).Route = rte Or rte = ALLITEMS) Then
          For j = 1 To UBound(uPaths)
            If uPaths(j) = uExposure(i).Path Then Exit For
          Next
          If j > UBound(uPaths) Then
            ReDim Preserve uPaths(j)
            uPaths(j) = uExposure(i).Path
            cbolist = cbolist & IIf(cbolist <> "", Chr(9), "") & uExposure(i).Path
          End If
        End If
      Next
      If UBound(uPaths) > 0 Then cbolist = ALLITEMS & Chr(9) & cbolist
  End Select
  SetSelList CBO_PTH, col, cbolist
End Sub

Sub updateOrgans(col As Long)
Dim dsIdx As Long
Dim i As Long
Dim measure As String
Dim cbolist As String

  If CBO_DS < 0 Then Exit Sub
  dsIdx = GetSelIdx(CBO_DS, col)
  measure = GetSelText(CBO_MSR, col)
  
  cbolist = ""
  Select Case measure
    Case "HI", "Dose":
      For i = 0 To ds(dsIdx).ndOrgans - 1
        cbolist = cbolist & IIf(cbolist <> "", Chr(9), "") & ds(dsIdx).dOrgans(i)
      Next
      If ds(dsIdx).ndOrgans > 0 Then cbolist = ALLITEMS & Chr(9) & cbolist
    Case "Risk":
      For i = 0 To ds(dsIdx).ncOrgans - 1
        cbolist = cbolist & IIf(cbolist <> "", Chr(9), "") & ds(dsIdx).cOrgans(i)
      Next
      If ds(dsIdx).ncOrgans > 0 Then cbolist = ALLITEMS & Chr(9) & cbolist
  End Select
  SetSelList CBO_ORG, col, cbolist
End Sub

Sub GetRoutesAndPathways(col As Long)
Dim dsIdx As Long
Dim ageIdx As Long
Dim name As String
  
  dsIdx = GetSelIdx(CBO_DS, col)
  ageIdx = GetSelIdx(CBO_AGE, col)
  name = GetSelText(CBO_CHM, col)
  GetRoutesAndPaths dsIdx, ageIdx, name
End Sub

Sub GetRoutesAndPaths(dsIdx As Long, ageIdx As Long, name As String)
Dim i As Long
  
  ReDim uExposure(0)
  Select Case name
    Case ALLRADS:
      For i = 1 To UBound(allCAS)
        If allCAS(i).ktype = 1 Then
          GetUniqueRoutesAndPaths dsIdx, ageIdx, allCAS(i).cas, allCAS(i).pcas(0)
        End If
      Next
    Case ALLCHEMS:
      For i = 1 To UBound(allCAS)
        If allCAS(i).ktype <> 1 Then
          GetUniqueRoutesAndPaths dsIdx, ageIdx, allCAS(i).cas, allCAS(i).pcas(0)
        End If
      Next
    Case Else:
      i = GetAllContIdx(name)
      If i > -1 Then
        GetUniqueRoutesAndPaths dsIdx, ageIdx, allCAS(i).cas, allCAS(i).pcas(0)
      End If
    End Select
End Sub

Sub GetUniqueRoutesAndPaths(dsIdx As Long, ageIdx As Long, casid As String, parentCAS As String)
Dim i As Long
Dim j As Long
Dim retc As Long
Dim totc As Long
Dim numTime As Long
Dim timeIdx As Long
Dim Route As String
Dim Path As String
Dim measure As String
Dim Unit As String
Dim found As Boolean

  Select Case pdcfType
    'api calls to FRAMES.DLL
    Case "epf": numTime = epfGetTimeCount(dsIdx, parentCAS)
    Case "rif": numTime = rifGetTimeCount(dsIdx, ageIdx, parentCAS)
    Case "hif": numTime = hifGetTimeCount(dsIdx, ageIdx, parentCAS)
  End Select
  
  totc = 0
  For timeIdx = 0 To numTime - 1
    Select Case pdcfType
      'api calls to FRAMES.DLL
      Case "epf": retc = epfLoadRoutesAndPathwaysByTime(dsIdx, casid, parentCAS, timeIdx)
      Case "rif": retc = rifLoadRoutesAndPathwaysByTime(dsIdx, ageIdx, casid, parentCAS, timeIdx)
      Case "hif": retc = hifLoadRoutesAndPathwaysByTime(dsIdx, ageIdx, casid, parentCAS, timeIdx)
    End Select
    For i = 0 To retc - 1
      Route = String(128, Chr(0))
      Path = String(128, Chr(0))
      measure = String(128, Chr(0))
      Unit = String(128, Chr(0))
      Select Case pdcfType
        'api calls to FRAMES.DLL
        Case "epf": epfGetRouteAndPathway dsIdx, i, Path, Route, Unit
        Case "rif": rifGetRouteAndPathway dsIdx, i, Path, Route, measure, Unit
        Case "hif": hifGetRouteAndPathway dsIdx, i, Path, Route, measure, Unit
      End Select
      Route = Left(Route, InStr(Route, Chr(0)) - 1)
      Path = Left(Path, InStr(Path, Chr(0)) - 1)
      measure = Left(measure, InStr(measure, Chr(0)) - 1)
      Unit = Left(Unit, InStr(Unit, Chr(0)) - 1)
      found = False
      For j = 1 To UBound(uExposure)
        found = (uExposure(j).Route = Route And _
                 uExposure(j).Path = Path And _
                 uExposure(j).measure = measure And _
                 uExposure(j).Unit = Unit)
        If found Then Exit For
      Next
      If Not found Then
        totc = 1 + UBound(uExposure)
        ReDim Preserve uExposure(totc)
        uExposure(totc).Route = Route
        uExposure(totc).Path = Path
        uExposure(totc).measure = measure
        uExposure(totc).Unit = Unit
      End If
    Next
  Next
End Sub

Sub cboUpdateExpPT(dsIdx As Long, cbo As ComboBox)
Dim i As Long

  IgnoreEvents = True
  cbo.Clear
  For i = 0 To ds(dsIdx).numPt - 1
    cbo.AddItem "(" + CStr(ds(dsIdx).X(i)) + ", " + CStr(ds(dsIdx).Y(i)) + ") km"
    cbo.ItemData(cbo.NewIndex) = i
  Next
  If cbo.Tag = "true" Then cbo.AddItem MAXIMUM, 0
  If cbo.ListCount > 0 Then cbo.ListIndex = 0
  IgnoreEvents = False
End Sub

Sub cboUpdateAgeGroup(dsIdx As Long, cbo As ComboBox)
Dim i As Long

  IgnoreEvents = True
  cbo.Clear
  For i = 0 To ds(dsIdx).numAge - 1
    cbo.AddItem CStr(ds(dsIdx).ageMin(i)) + " to " + CStr(ds(dsIdx).ageMax(i))
    cbo.ItemData(cbo.NewIndex) = i
  Next
  If cbo.Tag = "true" Then cbo.AddItem REPORTALL, 0
  If cbo.ListCount > 0 Then cbo.ListIndex = 0
  IgnoreEvents = False
End Sub

Sub cboUpdateStartTimes(dsIdx As Long, ageIdx As Long, cas As String, cbo As ComboBox)
  Dim i As Long
  
  IgnoreEvents = True
  cbo.Clear
  For i = 0 To hifGetTimeCount(dsIdx, ageIdx, cas) - 1
    cbo.AddItem CStr(hifGetTime(dsIdx, ageIdx, cas, i))
  Next
  If cbo.Tag = "true" Then cbo.AddItem MAXIMUM, 0
  If cbo.ListCount > 0 Then cbo.ListIndex = 0
  IgnoreEvents = False
End Sub

Sub cboUpdateConstituent(cbo As ComboBox, all As Boolean)
Dim i As Long
Dim numRad As Long
Dim numChem As Long
Dim name As String
  
  IgnoreEvents = True
  cbo.Clear
  For i = 1 To UBound(allCAS)
    name = allCAS(i).name
'used to want to display parent now just want to display cas and name
'    If allCAS(i).pcas(0) <> allCAS(i).cas Then
    If allCAS(i).pcas(0) = allCAS(i).cas Then
      name = name & " (" & allCAS(i).pcas(0) & ")"
    End If
    cbo.AddItem name
    If allCAS(i).cas = allCAS(i).pcas(0) Then
      If allCAS(i).ktype = 1 Then numRad = numRad + 1
      If allCAS(i).ktype <> 1 Then numChem = numChem + 1
    End If
  Next
  If numChem > 0 And all Then cbo.AddItem ALLCHEMS, 0
  If numRad > 0 And all Then cbo.AddItem ALLRADS, 0
  If cbo.ListCount > 0 Then cbo.ListIndex = 0
  IgnoreEvents = False
End Sub

Sub cboUpdateMeasures(cbo As ComboBox)
  Dim i As Long
  Dim j As Long
  Dim measure As String
  
  ReDim uMeasure(0)
  IgnoreEvents = True
  cbo.Clear
  For i = 1 To UBound(uExposure)
    measure = uExposure(i).measure & " (" & uExposure(i).Unit & ")"
    For j = 1 To UBound(uMeasure)
      If uMeasure(j) = measure Then Exit For
    Next
    If j > UBound(uMeasure) Then
      ReDim Preserve uMeasure(j)
      uMeasure(j) = measure
      cbo.AddItem measure
      cbo.ItemData(cbo.NewIndex) = i
    End If
  Next
  If cbo.Tag = "true" Then cbo.AddItem REPORTALL, 0
  If cbo.ListCount = 0 Then cbo.AddItem NOT_AVAILABLE
  If cbo.ListCount > 0 Then cbo.ListIndex = 0
  IgnoreEvents = False
End Sub

Sub cboUpdateRoutes(measure As String, Unit As String, cbo As ComboBox)
  Dim i As Long
  Dim j As Long

  ReDim uRoutes(0)
  IgnoreEvents = True
  cbo.Clear
  For i = 1 To UBound(uExposure)
    If (uExposure(i).measure = measure And uExposure(i).Unit = Unit) Or measure = "" Then
      For j = 1 To UBound(uRoutes)
        If uRoutes(j) = uExposure(i).Route Then Exit For
      Next
      If j > UBound(uRoutes) Then
        ReDim Preserve uRoutes(j)
        uRoutes(j) = uExposure(i).Route
        cbo.AddItem uExposure(i).Route
        cbo.ItemData(cbo.NewIndex) = i
      End If
    End If
  Next
  If cbo.Tag = "true" Then cbo.AddItem REPORTALL, 0
  If cbo.ListCount = 0 Then cbo.AddItem NOT_AVAILABLE
  If cbo.ListCount > 0 Then cbo.ListIndex = 0
  IgnoreEvents = False
End Sub

Sub cboUpdatePaths(measure As String, Unit As String, rte As String, cbo As ComboBox)
  Dim i As Long
  Dim j As Long
  
  ReDim uPaths(0)
  IgnoreEvents = True
  cbo.Clear
  For i = 1 To UBound(uExposure)
    If (uExposure(i).measure = measure And uExposure(i).Unit = Unit And uExposure(i).Route = rte) Or rte = "" Then
      For j = 1 To UBound(uPaths)
        If uPaths(j) = uExposure(i).Path Then Exit For
      Next
      If j > UBound(uPaths) Then
        ReDim Preserve uPaths(j)
        uPaths(j) = uExposure(i).Path
        cbo.AddItem uExposure(i).Path
      End If
    End If
  Next
  If cbo.Tag = "true" Then cbo.AddItem REPORTALL, 0
  If cbo.ListCount = 0 Then cbo.AddItem NOT_AVAILABLE
  If cbo.ListCount > 0 Then cbo.ListIndex = 0
  IgnoreEvents = False
End Sub

Sub cboUpdateCancerOrgans(dsIdx As Long, cbo As ComboBox)
Dim i As Long

  IgnoreEvents = True
  cbo.Clear
  For i = 0 To ds(dsIdx).ncOrgans - 1
    cbo.AddItem ds(dsIdx).cOrgans(i)
  Next
  If cbo.Tag = "true" Then cbo.AddItem REPORTALL, 0
  If cbo.ListCount > 0 Then cbo.ListIndex = 0
  IgnoreEvents = False
End Sub

Sub cboUpdateDoseOrgans(dsIdx As Long, cbo As ComboBox)
Dim i As Long

  IgnoreEvents = True
  cbo.Clear
  For i = 0 To ds(dsIdx).ndOrgans - 1
    cbo.AddItem ds(dsIdx).dOrgans(i)
  Next
  If cbo.Tag = "true" Then cbo.AddItem REPORTALL, 0
  If cbo.ListCount > 0 Then cbo.ListIndex = 0
  IgnoreEvents = False
End Sub

Function GetContIndexFromSpread(ByRef cname As String) As Long
  Dim ncon As Long
  
  GetContIndexFromSpread = -1
  If Not CBO_CHM > 0 Then Exit Function

  activeSpread.row = CBO_CHM
  ncon = activeSpread.TypeComboBoxCurSel
  cname = activeSpread.Text
  If "all" <> Left(activeSpread.Text, 3) Then
    activeSpread.TypeComboBoxCurSel = 0
    Do While "all" = Left(activeSpread.Text, 3)
      activeSpread.TypeComboBoxCurSel = activeSpread.TypeComboBoxCurSel + 1
    Loop
    GetContIndexFromSpread = (ncon - activeSpread.TypeComboBoxCurSel) + 1
    activeSpread.TypeComboBoxCurSel = ncon
  End If
End Function

