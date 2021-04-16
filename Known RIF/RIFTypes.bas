Attribute VB_Name = "DataTypes"
Option Explicit
Option Compare Text

Public Const CLOSED = "closed"
Public Const CONSTITUENT = "Constituent Pathways"

'units
Public Const YR = "yr"
Public Const KM = "km"
Public Const SV = "Sv"
Public Const BQ = "Bq"
Public Const BQL = "Bq/L"
Public Const BQKG = "Bq/kg"
Public Const BQM3 = "Bq/m^3"
Public Const MGM3 = "mg/m^3"
Public Const MGKGDAY = "mg/kg/day"

'effect types
Public Const CARC = "carcinogenic"
Public Const NARC = "noncarcinogenic"

'exposure type
Public Const INTAKE = "intake"
Public Const CONC = "concentration"
Public Const DOSE = "radiation dose"

'Exposure routes
Public Const INGESTION = "Ingestion"
Public Const INHALATION = "Inhalation"
Public Const DERMAL = "Dermal"
Public Const EXTERNAL = "External"

'Media
Public Const AIR = "Air"
Public Const SOIL = "Soil"
Public Const WETLANDS = "WetLands"
Public Const SURFACEWATER = "Surface Water"
Public Const AQUIFER = "Aquifer"

Public Const WATER = "Water"

Public Const USERUNIT = 1
Public Const INTLUNIT = 2

Type Entry
    valu() As Double
    num As Long
End Type

Type Exposure
    xType As String           'conc, intake, dose
    eType As String           'carc, noncarc, death ...
    Route As String
    Path As String
    ref As Long
    use As Long
    numseries As Long
    series() As Entry
    cunit(2) As String
End Type

Type sProgeny
    cas As String
    name As String
    kind As Long
    ref As Long
    use As Long
    numexp As Long
    exp() As Exposure
End Type

Type sParent
    cas As String
    name As String
    kind As Long
    ref As Long
    use As Long
    time() As Double
    tunit(2) As String
    numexp As Long
    exp() As Exposure
    numprog As Long
    prog() As sProgeny
    numtime As Long
    numseries As Long
    haveCarc As Integer
    haveNonCarc As Integer
End Type

Type agegroup
    population As Long
    duration As Double
    dunit(2) As String
    begAge As Double
    endAge As Double
    ageunit(2) As String
    numchem As Long
    chem() As sParent
    ref As Long
    use As Long
End Type

Type Medium
    kind As Long
    name As String
    ref As Long
    use As Long
    numloc As Long
    locx() As Double
    locy() As Double
    lunits(2) As String
    numages As Long
    ages() As agegroup
End Type

Type progeny
    name As String
    cas As String
    kind As Long
End Type

Type parent
    name As String
    cas As String
    kind As Long
    numprog As Long
    prog() As progeny
End Type

Global Const MAXMEDIA = 5
'Global numepf As Long
Global numrif As Long
Global numages As Long

Global med(MAXMEDIA) As Medium
Global prevtime As Double
Global bnflag As Boolean
Global numcon As Long
Global con() As parent

Global medIdx As Integer
Global ageIdx As Integer
Global chmIdx As Integer
Global prgIdx As Integer
Global expIdx As Integer

Global begAge As String
Global endAge As String
Global Route As String
Global Path As String
Global eType As String
Global xType As String

Public Sub AssignEntry(entry1 As Entry, entry2 As Entry)
    Dim i As Integer
    entry1.num = entry2.num
    ReDim entry1.valu(entry2.num) As Double
    For i = 1 To entry2.num
        entry1.valu(i) = entry2.valu(i)
    Next
End Sub

Public Sub AssignExposure(exp1 As Exposure, exp2 As Exposure)
    Dim i As Integer
    exp1.eType = exp2.eType
    exp1.xType = exp2.xType
    exp1.Route = exp2.Route
    exp1.Path = exp2.Path
    exp1.ref = exp2.ref
    exp1.use = exp2.use
    exp1.cunit(0) = exp2.cunit(0)
    exp1.cunit(1) = exp2.cunit(1)
    exp1.cunit(2) = exp2.cunit(2)
    exp1.numseries = exp2.numseries
    ReDim exp1.series(exp2.numseries) As Entry
    If exp2.numseries > 0 Then
        For i = 0 To exp2.numseries
            AssignEntry exp1.series(i), exp2.series(i)
        Next
    End If
End Sub

Public Sub AssignProgeny(prog1 As sProgeny, prog2 As sProgeny)
    Dim i As Integer
    prog1.cas = prog2.cas
    prog1.name = prog2.name
    prog1.kind = prog2.kind
    prog1.ref = prog2.ref
    prog1.use = prog2.use
    prog1.numexp = prog2.numexp
    ReDim prog1.exp(prog1.numexp) As Exposure
    If prog1.numexp > 0 Then
        For i = 0 To prog1.numexp
            AssignExposure prog1.exp(i), prog2.exp(i)
        Next
    End If
End Sub

Public Sub AssignParent(parent1 As sParent, parent2 As sParent)
    Dim i As Integer
    parent1.cas = parent2.cas
    parent1.name = parent2.name
    parent1.kind = parent2.kind
    parent1.ref = parent2.ref
    parent1.use = parent2.use
    parent1.numtime = parent2.numtime
    parent1.numseries = parent2.numseries
    ReDim parent1.time(parent1.numtime) As Double
    If parent1.numtime > 0 Then
        For i = 0 To parent1.numtime
            parent1.time(i) = parent2.time(i)
        Next
    End If
    parent1.tunit(0) = parent2.tunit(0)
    parent1.tunit(1) = parent2.tunit(1)
    parent1.tunit(2) = parent2.tunit(2)
    parent1.numexp = parent2.numexp
    ReDim parent1.exp(parent1.numexp) As Exposure
    If parent1.numexp > 0 Then
        For i = 0 To parent1.numexp
            AssignExposure parent1.exp(i), parent2.exp(i)
        Next
    End If
    parent1.numprog = parent2.numprog
    ReDim parent1.prog(parent1.numprog) As sProgeny
    If parent1.numprog > 0 Then
        For i = 0 To parent1.numprog
            AssignProgeny parent1.prog(i), parent2.prog(i)
        Next
    End If
End Sub

Public Sub AssignAgeGroup(age1 As agegroup, age2 As agegroup)
    Dim i As Integer
    age1.population = age2.population
    age1.duration = age2.duration
    age1.begAge = age2.begAge
    age1.endAge = age2.endAge
    age1.dunit(0) = age2.dunit(0)
    age1.dunit(1) = age2.dunit(1)
    age1.dunit(2) = age2.dunit(2)
    age1.ageunit(0) = age2.ageunit(0)
    age1.ageunit(1) = age2.ageunit(1)
    age1.ageunit(2) = age2.ageunit(2)
    age1.numchem = age2.numchem
    ReDim age1.chem(age1.numchem) As sParent
    If age1.numchem > 0 Then
        For i = 0 To age1.numchem
            AssignParent age1.chem(i), age2.chem(i)
        Next
    End If
    age1.ref = age2.ref
    age1.use = age2.use
End Sub

Public Sub ResizeAges(medIdx As Integer)
  Dim i As Integer
  Dim j As Integer
  Dim tempindex
  Dim tempages() As agegroup
  
  tempindex = 0
  ReDim tempages(med(medIdx).numages) As agegroup
  For i = 1 To med(medIdx).numages
    If med(medIdx).ages(i).use = 1 Then
      AssignAgeGroup tempages(tempindex + 1), med(medIdx).ages(i)
      tempindex = tempindex + 1
    End If
  Next
  ReDim med(medIdx).ages(tempindex) As agegroup
  For i = 1 To tempindex
    AssignAgeGroup med(medIdx).ages(i), tempages(i)
  Next
  med(medIdx).numages = tempindex
End Sub

Public Sub ResizeExposures(medIdx As Integer, ageIdx As Integer, chmIdx As Integer, prgIdx As Integer)
  Dim i As Integer
  Dim j As Integer
  Dim tempindex As Integer
  Dim tempexps() As Exposure
  
  tempindex = 0
  If prgIdx = 0 Then
    ReDim tempexps(med(medIdx).ages(ageIdx).chem(chmIdx).numexp) As Exposure
    For i = 1 To med(medIdx).ages(ageIdx).chem(chmIdx).numexp
      If med(medIdx).ages(ageIdx).chem(chmIdx).exp(i).use = 1 Then
        AssignExposure tempexps(tempindex + 1), med(medIdx).ages(ageIdx).chem(chmIdx).exp(i)
        tempindex = tempindex + 1
      End If
    Next
    ReDim med(medIdx).ages(ageIdx).chem(chmIdx).exp(tempindex + 1) As Exposure
    For i = 1 To tempindex
      AssignExposure med(medIdx).ages(ageIdx).chem(chmIdx).exp(i), tempexps(i)
    Next
    med(medIdx).ages(ageIdx).chem(chmIdx).numexp = tempindex
  Else
    ReDim tempexps(med(medIdx).ages(ageIdx).chem(chmIdx).prog(prgIdx).numexp) As Exposure
    For i = 1 To med(medIdx).ages(ageIdx).chem(chmIdx).prog(prgIdx).numexp
      If med(medIdx).ages(ageIdx).chem(chmIdx).prog(prgIdx).exp(i).use = 1 Then
        AssignExposure tempexps(tempindex + 1), med(medIdx).ages(ageIdx).chem(chmIdx).prog(prgIdx).exp(i)
        tempindex = tempindex + 1
      End If
    Next
    ReDim med(medIdx).ages(ageIdx).chem(chmIdx).prog(prgIdx).exp(tempindex) As Exposure
    For i = 1 To tempindex
      AssignExposure med(medIdx).ages(ageIdx).chem(chmIdx).prog(prgIdx).exp(i), tempexps(i)
    Next
    med(medIdx).ages(ageIdx).chem(chmIdx).prog(prgIdx).numexp = tempindex
  End If
End Sub

Public Function CheckEntry(tentry As Entry) As Boolean
    CheckEntry = True
    If tentry.num = 0 Then CheckEntry = False
End Function

Public Function CheckExposure(texp As Exposure, missing As String) As Boolean
    Dim i As Integer
    Dim check As Boolean
    CheckExposure = True
    With texp
        If .eType = "" And .xType = "" Then
            missing = "Exposure Type"
            CheckExposure = False
            Exit Function
        ElseIf .Route = "" Then
            missing = "Exposure Route"
            CheckExposure = False
            Exit Function
        ElseIf .Path = "" Then
            missing = "Exposure Path"
            CheckExposure = False
            Exit Function
        ElseIf .numseries = 0 Then
            missing = "Series Data"
            CheckExposure = False
            Exit Function
        End If
        For i = 1 To .numseries
            If CheckEntry(.series(i)) = False Then
                missing = "Series Values"
                missing = "Series " & i & " Values"
                CheckExposure = False
                Exit Function
            End If
        Next
    End With
End Function

Public Function CheckProgeny(tprog As sProgeny, missing As String) As Boolean
    Dim i As Integer
    Dim check As Boolean
    CheckProgeny = True
    With tprog
        If .cas = "" Then
            missing = "CAS ID"
            CheckProgeny = False
            Exit Function
        ElseIf .name = "" Then
            missing = "Chemical Name"
            CheckProgeny = False
            Exit Function
        ElseIf .numexp = 0 Then
            missing = "Exposures"
            CheckProgeny = False
            Exit Function
        End If
        For i = 1 To .numexp
            If CheckExposure(.exp(i), missing) = False Then
                CheckProgeny = False
                Exit Function
            End If
        Next
    End With
End Function


Public Function CheckParent(tparent As sParent, missing As String) As Boolean
    Dim i As Integer
    Dim check As Boolean
    CheckParent = True
    With tparent
        If .cas = "" Then
            missing = "CAS ID"
            CheckParent = False
            Exit Function
        ElseIf .name = "" Then
            missing = "Chemical Name"
            CheckParent = False
            Exit Function
        ElseIf .tunit(INTLUNIT) = "" Then
            missing = "Program Time Unit"
            CheckParent = False
            Exit Function
        ElseIf .tunit(USERUNIT) = "" Then
            missing = "User Time Unit"
            CheckParent = False
            Exit Function
        ElseIf .numtime = 0 Then
            missing = "Times and Durations"
            CheckParent = False
            Exit Function
        ElseIf .numexp = 0 Then
            missing = "Exposures"
            CheckParent = False
            Exit Function
        End If
        If frmRIF.prog.Checked Then
          For i = 1 To .numprog
              If CheckProgeny(.prog(i), missing) = False Then
                  missing = .prog(i).name & " " & missing
                  CheckParent = False
                  Exit Function
              End If
          Next
        End If
        For i = 1 To .numexp
            If CheckExposure(.exp(i), missing) = False Then
              missing = .exp(i).Path & " " & missing
                CheckParent = False
                Exit Function
            End If
        Next
    End With
End Function
Public Function CheckAges(tage As agegroup, missing As String) As Boolean
    Dim i As Integer
    Dim check As Boolean
    CheckAges = True
    With tage
        If .ageunit(INTLUNIT) = "" Then
            missing = "Program Age Unit"
            CheckAges = False
            Exit Function
        ElseIf .ageunit(USERUNIT) = "" Then
            missing = "User Age Unit"
            CheckAges = False
            Exit Function
        ElseIf .dunit(INTLUNIT) = "" Then
            missing = "Program Duration Unit"
            CheckAges = False
            Exit Function
        ElseIf .dunit(USERUNIT) = "" Then
            missing = "User Duration Unit"
            CheckAges = False
            Exit Function
        ElseIf .numchem = 0 Then
            missing = "Chemicals"
            CheckAges = False
            Exit Function
        End If
        For i = 1 To .numchem
            If CheckParent(.chem(i), missing) = False Then
                missing = .chem(i).name & " " & missing
                CheckAges = False
                Exit Function
            End If
        Next
    End With
End Function
Public Function CheckMedium(tmed As Medium, missing As String) As Boolean
    Dim i As Integer
    Dim check As Boolean
    CheckMedium = True
    With tmed
        If .name = "" Then
            missing = "Medium Name"
            CheckMedium = False
            Exit Function
        ElseIf .numloc = 0 Then
            missing = "Locations"
            CheckMedium = False
            Exit Function
        ElseIf .numages = 0 Then
            missing = "Age Groups"
            CheckMedium = False
            Exit Function
        End If
        For i = 1 To .numages
            If CheckAges(.ages(i), missing) = False Then
                missing = "Age " & Format(.ages(i).begAge, "Fixed") & "-" & Format(.ages(i).endAge, "Fixed") & " " & missing
                CheckMedium = False
                Exit Function
            End If
        Next
    End With
End Function

