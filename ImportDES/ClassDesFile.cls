VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ClassDESFile"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

'Description file object

'===============================================================
' Local variables to hold property values.
'===============================================================
Private Const MDL       As String = "Model"
Private Const VWR       As String = "Viewer"
Private Const SYS       As String = "System"
Private Const DB        As String = "Database"

Public Nread            As Long
Public FRead            As Collection 'ClassSchemeStruct

Public Nwrite           As Long
Public FWrite           As Collection 'ClassSchemeStruct

Public Nvars            As Long
Public vars             As Collection 'ClassDesDeclaration

Public Nvers            As Single
Public Vers             As String
Public ModType          As String
Public ModulePath       As String   ' complete [drive:\path\filename.extension] to this file
Public ModuleName       As String   ' model name
Public UIBat            As String   ' user interface executable [drive:\path\filename.extension cmdline]
Public EXEBat           As String   ' model executable [drive:\path\filename.extension cmdline]
Public IcoPath          As String
Public ModuleGroup      As ClassModGroup

Public RAM              As Integer
Public DiskSpace        As Integer
Public OperatingSystem  As String
Public Processor        As String
Public POCCompany       As String
Public POCContact       As String
Public POCAddress       As String
Public POCCity          As String
Public POCState         As String
Public POCZip           As String
Public POCCountry       As String
Public POCPhone         As String
Public POCFax           As String
Public POCEmail         As String
Public POCUrl           As String

Public DescLines        As Collection 'string
Public RefLines         As Collection 'string

Private Function MapGroup(ByVal name As String, ByVal grp As ClassModGroup) As String

    With grp
        .ModuleName = name
        Select Case name
            Case "Contaminant":       .ModuleType = DB:  .ModulePrefix = "con": .ModuleName = "Constituent"
            Case "Viewer":            .ModuleType = VWR: .ModulePrefix = "vwr"
            Case "Sensitivity":       .ModuleType = SYS: .ModulePrefix = "sen"
            Case "Source":            .ModuleType = MDL: .ModulePrefix = "src"
            Case "Air":               .ModuleType = MDL: .ModulePrefix = "air"
            Case "Overland Flow":     .ModuleType = MDL: .ModulePrefix = "ovl"
            Case "Vadose Zone":       .ModuleType = MDL: .ModulePrefix = "vad"
            Case "Aquifer":           .ModuleType = MDL: .ModulePrefix = "aqu"
            Case "Surface Water":     .ModuleType = MDL: .ModulePrefix = "riv"
            Case "Exposure Pathways": .ModuleType = MDL: .ModulePrefix = "exp"
            Case "Receptor Intakes":  .ModuleType = MDL: .ModulePrefix = "rcp"
            Case "Health Impacts":    .ModuleType = MDL: .ModulePrefix = "hei"
            Case "Import":            .ModuleType = MDL: .ModulePrefix = "mpt"
            Case "Export":            .ModuleType = MDL: .ModulePrefix = "xpt"
            Case "Closed":            .ModuleType = MDL: .ModulePrefix = "cls"
        End Select
    End With
    
End Function

Private Function ExtractLong(ByVal stToken As String) As Long
    Dim lTemp As Long
    Dim stVal As String
    Dim stDigit As String
    
    On Error GoTo ExtractLongError
    
    ExtractLong = 0
    stVal = BLANK
    stDigit = BLANK
    lTemp = 1
    While lTemp < Len(stToken)
        stDigit = Mid(stToken, lTemp, 1)
        lTemp = lTemp + 1
        While (IsNumeric(stDigit) And lTemp < Len(stToken))
            stVal = stVal & stDigit
            stDigit = Mid(stToken, lTemp, 1)
            lTemp = lTemp + 1
        Wend
        If stVal <> BLANK Then
            If IsNumeric(stVal) Then
                ExtractLong = CLng(stVal)
            Else
                ExtractLong = 0
            End If
            Exit Function
        End If
    Wend
    Exit Function
    
ExtractLongError:
    ExtractLong = 0
End Function

Private Function FindTokenValue(ByVal stBuffer As String, ByVal stToken As String) As String
    Dim pos1 As Long
    Dim pos2 As Long
    Dim pos3 As Long
    
    On Error GoTo FindTokenValueError
    
    FindTokenValue = BLANK
    pos1 = InStr(1, stBuffer, stToken)
    If pos1 > 0 Then
        pos1 = pos1 + Len(stToken)
        While Mid(stBuffer, pos1, 1) = SPACER
            pos1 = pos1 + 1
            If pos1 > Len(stBuffer) Then
                FindTokenValue = BLANK
                Exit Function
            End If
        Wend
        pos2 = InStr(pos1, stBuffer, SPACER)
        pos3 = InStr(pos1, stBuffer, vbCrLf, vbBinaryCompare)
        If pos2 = 0 And pos3 = 0 Then
            FindTokenValue = Trim(Mid(stBuffer, pos1, Len(stBuffer) - pos1 + 1))
        ElseIf pos3 = 0 Then
            FindTokenValue = Trim(Mid(stBuffer, pos1, pos2 - pos1 + 1))
        Else
            FindTokenValue = Trim(Mid(stBuffer, pos1, pos3 - pos1))
        End If
    End If
    Exit Function
    
FindTokenValueError:
    FindTokenValue = BLANK
End Function

Private Sub ExtractProseLines(ByVal stBuffer As String, ByVal stStartToken As String, _
                                    ByVal stEndToken As String, ByRef objColl As Collection)
    Dim posStart As Long
    Dim posEnd As Long
    Dim pos1 As Long
    Dim pos2 As Long
    Dim stLine As String
    
    On Error GoTo ExtractProseLinesErr
    
    posStart = InStr(1, stBuffer, stStartToken)
    posEnd = InStr(1, stBuffer, stEndToken)
    If (posStart = 0) Or (posEnd = 0) Or (posStart > posEnd) Then Exit Sub
    
    pos1 = posStart
    pos2 = InStr(pos1, stBuffer, vbCrLf)
    While pos2 < posEnd
        If pos2 > pos1 Then
            stLine = Mid(stBuffer, pos1, pos2 - pos1)
            If stLine = BLANK Then stLine = SPACER
            objColl.add stLine
        End If
        pos1 = pos2 + 2
        pos2 = InStr(pos1, stBuffer, vbCrLf)
    Wend
    Exit Sub
    
ExtractProseLinesErr:
End Sub

Public Function ReadModuleDescription(ByVal stFileName As String, ByVal stSrcPath As String) As Boolean

    Dim i As Long
    Dim j As Long
    Dim k As Long
    Dim lCount As Long
    Dim temp As String
    Dim fle As csv
    Dim objVar As ClassDesDeclaration
    Dim objQue As ClassDesQue
    Dim objPiece As ClassSchemePiece
    Dim objScheme As Collection
    Dim pos1 As Long
    Dim Advert As String
    Dim stTemp As String
    Dim csvtemp As Variant

    On Error GoTo ReadModuleDescriptionErr
    
    ReadModuleDescription = False
    ModulePath = stSrcPath & stFileName ' stFileName
    Nvers = 0
    Nread = 0
    Nwrite = 0
    Nvars = 0
    RAM = 0
    DiskSpace = 0
    OperatingSystem = SPACER
    Processor = SPACER
    POCCompany = SPACER
    POCContact = SPACER
    POCAddress = SPACER
    POCCity = SPACER
    POCState = SPACER
    POCZip = SPACER
    POCCountry = SPACER
    POCPhone = SPACER
    POCFax = SPACER
    POCEmail = SPACER
    POCUrl = SPACER
    
    If Not open_csv(fle, stSrcPath & stFileName, F_READ) Then Exit Function
    
    'read to verify this is a Metafile (mf) and the correct version
    temp = get_val(fle)
    Vers = get_val(fle)
    get_line fle
    If Not temp = "mf" Then Exit Function
    If 0 < InStr(Vers, "Version") Then Nvers = val(Mid(Vers, InStr(Vers, " ")))
    If Nvers < 2# Then Exit Function
    
    'read execution header
    temp = get_val(fle)
    csvtemp = Split(temp, ":")
    ModType = csvtemp(0)
    If (csvtemp(0) <> DB And csvtemp(0) <> VWR And csvtemp(0) <> SYS) Then ModType = MDL
    'Debug.Print ModType
    ModuleName = get_val(fle)
    UIBat = get_val(fle)
    EXEBat = get_val(fle)
    IcoPath = get_val(fle)
    get_line fle
    
    'adjust paths
    If (IcoPath <> BLANK) And (0 = InStr(IcoPath, ":") And 0 = InStr(IcoPath, "\")) Then
      IcoPath = stSrcPath & IcoPath
    End If
  
    'parse module type and grouping information
    If Nvers > 2# Then
      pos1 = InStr(temp, ":")
      If pos1 = 0 Then Exit Function
      ModuleGroup.ModuleType = Left(temp, pos1 - 1)
      temp = Mid(temp, pos1 + 1)
      pos1 = InStr(temp, ":")
      If pos1 = 0 Then Exit Function
      ModuleGroup.ModuleName = Left(temp, pos1 - 1)
      ModuleGroup.ModulePrefix = Mid(temp, pos1 + 1)
    Else
      MapGroup temp, ModuleGroup
      Debug.Print temp, ModuleGroup
    End If
    ModuleGroup.ModuleIcoPath = IcoPath
    
    'read the description and retrieve the reference url
    Advert = get_val_multiline(fle)
    Set DescLines = New Collection
    ExtractProseLines Advert, "MODULE DESCRIPTION", "MODULE REFERENCES", DescLines
    Set RefLines = New Collection
    ExtractProseLines Advert, "MODULE REFERENCES", "VALID CONNECTIONS", RefLines
    RAM = ExtractLong(FindTokenValue(Advert, "RAM Memory:"))
    DiskSpace = ExtractLong(FindTokenValue(Advert, "Disk Space:"))
    OperatingSystem = FindTokenValue(Advert, "Operating System:")
    Processor = FindTokenValue(Advert, "Processor:")
    POCCompany = FindTokenValue(Advert, "Company Name:")
    POCContact = FindTokenValue(Advert, "Contact Name:")
    POCAddress = FindTokenValue(Advert, "Mailing Address:")
    POCCity = FindTokenValue(Advert, "City:")
    POCState = FindTokenValue(Advert, "State:")
    POCZip = FindTokenValue(Advert, "Zip Code:")
    POCCountry = FindTokenValue(Advert, "Country:")
    POCPhone = FindTokenValue(Advert, "Telephone Number:")
    POCFax = FindTokenValue(Advert, "Fax Number:")
    POCEmail = FindTokenValue(Advert, "Email Address:")
    POCUrl = FindTokenValue(Advert, "URL Address:")

    'read the number of and input schemes
    get_line fle
    Nread = val(get_val(fle))
    For i = 1 To Nread
        get_line fle
        Set objScheme = New Collection
        lCount = val(get_val(fle))
        For j = 1 To lCount
            Set objPiece = New ClassSchemePiece
            objPiece.FileExtension = get_val(fle)
            objPiece.Qualifier = get_val(fle)
            objPiece.MinAllowed = val(get_val(fle))
            objPiece.MaxAllowed = val(get_val(fle))
            If objPiece.Qualifier = "" Then objPiece.Qualifier = "<none>"
            objScheme.add objPiece
        Next
        FRead.add objScheme
    Next
    
    'read the number and kind of outputs
    get_line fle
    Set objScheme = New Collection
    lCount = val(get_val(fle))
    For i = 1 To lCount
        get_line fle
        Set objPiece = New ClassSchemePiece
        objPiece.FileExtension = get_val(fle)
        objPiece.Qualifier = get_val(fle)
        If objPiece.Qualifier = "" Then objPiece.Qualifier = "<none>"
        objPiece.MinAllowed = 0
        objPiece.MaxAllowed = 0
        objScheme.add objPiece
    Next
    FWrite.add objScheme
    Nwrite = 1
    
    'read variable declarations for GID file
    get_line fle
    Nvars = val(get_val(fle))
    
    For i = 1 To Nvars
        get_line fle
        Set objVar = New ClassDesDeclaration
        With objVar
            .name = get_val(fle)
            Debug.Print i, .name
            .Scalar = FalseFlag
            .Stochastic = get_val(fle)
            .Unit = get_val(fle)
            .VarMinS = get_val(fle)
            If (IsNumeric(.VarMinS)) Then Debug.Print stFileName
            If (.VarMinS <> BLANK) Then .Minimum = val(get_val(fle)) Else .Minimum = 0#
            .VarMaxS = get_val(fle)
            If (IsNumeric(.VarMaxS)) Then Debug.Print stFileName
            If (.VarMaxS <> BLANK) Then .Maximum = val(get_val(fle)) Else .Maximum = FLT_MAX
            'Need to resolve the case when only one is blank
            If (.VarMaxS = BLANK) And (.VarMinS <> BLANK) Then
                .VarMaxS = "Max"
                .Maximum = FLT_MAX
            ElseIf (.VarMaxS <> BLANK) And (.VarMinS = BLANK) Then
                .VarMinS = "Min"
                .Minimum = 0#
            End If
            .description = get_val(fle)
            .VarNq = val(get_val(fle))
            If fle.getbuff <> "" Then
              .DataType = get_val(fle)
              .VarMinS = "Min"
              .VarMaxS = "Max"
              Select Case .DataType
                Case dtSTRING:
                .Minimum = 0
                .Maximum = 0
                Case dtFLOAT:
                Case dtINTEGER:
                  If .Maximum > INT_MAX Then .Maximum = INT_MAX
                Case dtLOGICAL:
                  .VarMinS = "Min": .Maximum = 0
                  .VarMaxS = "Max": .Maximum = 1
              End Select
            End If
            For j = 1 To .VarNq
                get_line fle
                Set objQue = New ClassDesQue
                With objQue
                    .QueType = get_val(fle)
                    .QueName = get_val(fle)
                    For k = 1 To 6
                        stTemp = get_val(fle)
                        If Trim(stTemp) <> BLANK Then
                            .QueIndices.add stTemp
                        End If
                    Next
                End With
                .VarQue.add objQue
            Next
        End With
        If objVar.name <> "" Then vars.add objVar, objVar.name
    Next
    
    close_csv fle
    ReadModuleDescription = True
    Exit Function
    
ReadModuleDescriptionErr:
    close_csv fle
    Resume Next
    ReadModuleDescription = False
End Function

Private Sub Class_Initialize()
    Nread = 0
    Nwrite = 0
    Nvars = 0
    Nvers = 0
    Vers = BLANK
    ModulePath = BLANK
    ModuleName = BLANK
    UIBat = BLANK
    EXEBat = BLANK
    IcoPath = BLANK
    RAM = 0
    DiskSpace = 0
    OperatingSystem = BLANK
    Processor = BLANK
    POCCompany = BLANK
    POCContact = BLANK
    POCAddress = BLANK
    POCCity = BLANK
    POCState = BLANK
    POCZip = BLANK
    POCCountry = BLANK
    POCPhone = BLANK
    POCFax = BLANK
    POCEmail = BLANK
    POCUrl = BLANK
    Set FRead = New Collection
    Set FWrite = New Collection
    Set vars = New Collection
    Set ModuleGroup = New ClassModGroup
    Set DescLines = New Collection
    Set RefLines = New Collection
End Sub

Private Sub Class_Terminate()
    Set FRead = Nothing
    Set FWrite = Nothing
    Set vars = Nothing
    Set ModuleGroup = Nothing
    Set DescLines = Nothing
    Set RefLines = Nothing
End Sub
