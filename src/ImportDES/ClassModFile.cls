VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ClassMODFile"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

'===============================================================
' Module file object
'===============================================================

' Local Constants.
Private Const InputDics As String = "Consumed"       'Index to scheme consumed dictionary collection
Private Const OutputDics As String = "Produced"      'Index to scheme produced dictionary collection

' Local variables to hold property values.
Public miDiskSpace         As Long
Public miRAM               As Long
Public miSystemVersion     As Long
Public mbSystemUpdate      As Boolean
Public mstClass            As String
Public mstGroup            As String
Public mstSubGrp           As String
Public mstDatabaseID       As String
Public mstDictionary       As String
Public mstIcon             As String
Public mstLogin            As String
Public mstModelExe         As String
Public mstModelCmdLine     As String
Public mstModelURL         As String
Public mstName             As String
Public mstOperatingSystem  As String
Public mstPassword         As String
Public mstPOCAddress1      As String
Public mstPOCAddress2      As String
Public mstPOCCity          As String
Public mstPOCCompany       As String
Public mstPOCContact       As String
Public mstPOCCountry       As String
Public mstPOCEmail         As String
Public mstPOCFax           As String
Public mstPOCPerson        As String
Public mstPOCPhone         As String
Public mstPOCState         As String
Public mstPOCUrl           As String
Public mstPOCZip           As String
Public mstProcessor        As String
Public mstTool             As Boolean
Public mstUIExe            As String
Public mstUICmdLine        As String
Public mstVersion          As String

Public miDescriptionCount  As Long
Public miReferenceCount    As Long
Public mobjDescLines       As Collection 'collection of description lines
Public mobjRefLines        As Collection 'collection of reference lines
Public mobjSchemes         As Collection 'collection of schemes, containing name and two collections of dictionaries (Con, Pro)
Public mobjSchNames        As Collection 'collection of scheme names
Public mobjSchNotes        As Collection 'collection of scheme notes

'Some control parameters
Public mbLoaded            As Boolean

Private Sub Class_Initialize()
  miDiskSpace = 0
  miSystemVersion = 0
  mstClass = BLANK
  mstDatabaseID = BLANK
  mstDictionary = BLANK
  mstIcon = BLANK
  mstLogin = BLANK
  mstModelExe = BLANK
  mstModelCmdLine = BLANK
  mstModelURL = BLANK
  mstName = BLANK
  mstOperatingSystem = BLANK
  mstPassword = BLANK
  mstPOCAddress1 = BLANK
  mstPOCAddress2 = BLANK
  mstPOCCity = BLANK
  mstPOCCompany = BLANK
  mstPOCContact = BLANK
  mstPOCCountry = BLANK
  mstPOCEmail = BLANK
  mstPOCFax = BLANK
  mstPOCPerson = BLANK
  mstPOCPhone = BLANK
  mstPOCState = BLANK
  mstPOCUrl = BLANK
  mstPOCZip = BLANK
  mstProcessor = BLANK
  mbSystemUpdate = False
  mstUIExe = BLANK
  mstUICmdLine = BLANK
  mstVersion = BLANK
  miDescriptionCount = 0
  miReferenceCount = 0
  mbLoaded = False
  Set mobjDescLines = New Collection
  Set mobjRefLines = New Collection
  Set mobjSchemes = New Collection
End Sub

Private Sub Class_Terminate()
    Set mobjDescLines = Nothing
    Set mobjRefLines = Nothing
    Set mobjSchemes = Nothing
End Sub

Public Sub DeleteMod()
    On Error Resume Next
    If Not mbLoaded Then Exit Sub
    DelModule PID, mstName 'in case it already exists
End Sub

Private Sub MoveFirst(Name As String, scheme As Collection)
  Dim i As Long
  Dim j As Long

  j = 0
  If scheme.count = 1 Then Exit Sub
  For i = scheme.count To 1 Step -1
    If scheme(i) = Name Then
      scheme.Remove i
      j = j + 1
    End If
  Next
  For i = 1 To j
    scheme.add Name, , 1
  Next
End Sub

Private Sub OrderDependencies(scheme As Collection)
'should read this list form the maintainimport file
' done in reverse order

'  MoveFirst "AOCPolygons", scheme
  MoveFirst "AquiferPoints", scheme
  MoveFirst "SurfaceWaterPoints", scheme
  MoveFirst "AirPoints", scheme
  
  MoveFirst "VadosePolygons", scheme
  MoveFirst "AquiferPolygons", scheme
  MoveFirst "SurfaceWaterPolygons", scheme
  
  MoveFirst "SoilVolumes", scheme
  MoveFirst "SedimentVolumes", scheme
  MoveFirst "AirPolygons", scheme
  
  MoveFirst "PopulationPolygons", scheme
  MoveFirst "AgeGroup", scheme
  
  MoveFirst "SuspendedSolidsDef", scheme
  MoveFirst "SuspendedLiquidsDef", scheme
  MoveFirst "SuspendedGasesDef", scheme
  
  MoveFirst "AnimalProducts", scheme
  MoveFirst "TerrestrialOrganisms", scheme
  MoveFirst "AquaticOrganisms", scheme
  MoveFirst "RadList", scheme
  MoveFirst "ChemList", scheme
  MoveFirst "GeoReference", scheme
  MoveFirst "Status", scheme
  
End Sub


Public Function WriteMod(ByVal stFileName As String, ByVal stDstPath As String) As Boolean
  Dim modhndl As Long
  Dim dichndl As Long
  Dim domhndl As Long
  Dim clshndl As Long
  Dim grphndl As Long
  Dim subhndl As Long
  Dim lTemp As Long
  Dim iFile As Long
  Dim iCount As Long
  Dim stTemp As String
  Dim stName As String
  Dim stNote As String
  Dim vTemp As Variant
  Dim objScheme As Collection
  
  On Error GoTo WriteModErr
  
  WriteMod = False
  If Not mbLoaded Then Exit Function
  If DisplayError(AddNewModule(PID, stDstPath & mstName & ".mod", mstName), "AddNewModule", True) Then Exit Function
  
  'Executable Info
  If DisplayError(PutModLogical(PID, mstName, "SystemUpdate", BLANK, SetIdx(), mbSystemUpdate), "PutModLogical", True) Then Exit Function
  If DisplayError(PutModInteger(PID, mstName, "SystemVersion", BLANK, SetIdx(), miSystemVersion), "PutModInteger", True) Then Exit Function
  If DisplayError(PutModString(PID, mstName, "Version", BLANK, SetIdx(), mstVersion), "PutModString", True) Then Exit Function
  If DisplayError(PutModString(PID, mstName, "Icon", BLANK, SetIdx(), mstIcon), "PutModString", True) Then Exit Function
  If DisplayError(PutModString(PID, mstName, "UIExe", BLANK, SetIdx(), mstUIExe), "PutModString", True) Then Exit Function
  If DisplayError(PutModString(PID, mstName, "UICmdLine", BLANK, SetIdx(), mstUICmdLine), "PutModString", True) Then Exit Function
  If DisplayError(PutModString(PID, mstName, "ModelExe", BLANK, SetIdx(), mstModelExe), "PutModString", True) Then Exit Function
  If DisplayError(PutModString(PID, mstName, "ModelCmdLine", BLANK, SetIdx(), mstModelCmdLine), "PutModString", True) Then Exit Function
  If DisplayError(PutModString(PID, mstName, "ModelUrl", BLANK, SetIdx(), mstModelURL), "PutModString", True) Then Exit Function
  If DisplayError(PutModString(PID, mstName, "Login", BLANK, SetIdx(), mstLogin), "PutModString", True) Then Exit Function
  If DisplayError(PutModString(PID, mstName, "Password", BLANK, SetIdx(), mstPassword), "PutModString", True) Then Exit Function
  If DisplayError(PutModString(PID, mstName, "DatabaseID", BLANK, SetIdx(), mstDatabaseID), "PutModString", True) Then Exit Function
  If DisplayError(PutModString(PID, mstName, "Class", BLANK, SetIdx(), mstClass), "PutModString", True) Then Exit Function
  If mstTool Then
    If DisplayError(PutModLogical(PID, mstName, "Tool", BLANK, SetIdx(), CLng(1)), "PutModLogical", True) Then Exit Function
  Else
    If DisplayError(PutModLogical(PID, mstName, "Tool", BLANK, SetIdx(), CLng(0)), "PutModLogical", True) Then Exit Function
  End If
  
  'Reference info
  lTemp = 0
  For Each vTemp In mobjDescLines
    lTemp = lTemp + 1
    If DisplayError(PutModString(PID, mstName, "Description", BLANK, SetIdx(lTemp), CStr(vTemp)), "PutModString", True) Then Exit Function
  Next vTemp
  If DisplayError(PutModInteger(PID, mstName, "DescriptionCount", BLANK, SetIdx(), miDescriptionCount), "PutModInteger", True) Then Exit Function
  lTemp = 0
  For Each vTemp In mobjRefLines
    lTemp = lTemp + 1
    If DisplayError(PutModString(PID, mstName, "Reference", BLANK, SetIdx(lTemp), CStr(vTemp)), "PutModString", True) Then Exit Function
  Next vTemp
  If DisplayError(PutModInteger(PID, mstName, "ReferenceCount", BLANK, SetIdx(), miReferenceCount), "PutModInteger", True) Then Exit Function
  
  'POC Info
  If DisplayError(PutModString(PID, mstName, "POCCompany", BLANK, SetIdx(), mstPOCCompany), "PutModString", True) Then Exit Function
  If DisplayError(PutModString(PID, mstName, "POCAddress1", BLANK, SetIdx(), mstPOCAddress1), "PutModString", True) Then Exit Function
  If DisplayError(PutModString(PID, mstName, "POCAddress2", BLANK, SetIdx(), mstPOCAddress2), "PutModString", True) Then Exit Function
  If DisplayError(PutModString(PID, mstName, "POCCity", BLANK, SetIdx(), mstPOCCity), "PutModString", True) Then Exit Function
  If DisplayError(PutModString(PID, mstName, "POCState", BLANK, SetIdx(), mstPOCState), "PutModString", True) Then Exit Function
  If DisplayError(PutModString(PID, mstName, "POCCountry", BLANK, SetIdx(), mstPOCCountry), "PutModString", True) Then Exit Function
  If DisplayError(PutModString(PID, mstName, "POCZip", BLANK, SetIdx(), mstPOCZip), "PutModString", True) Then Exit Function
  If DisplayError(PutModString(PID, mstName, "POCUrl", BLANK, SetIdx(), mstPOCUrl), "PutModString", True) Then Exit Function
  
  'Developer Info
  If DisplayError(PutModString(PID, mstName, "POCContact", BLANK, SetIdx(), mstPOCContact), "PutModString", True) Then Exit Function
  If DisplayError(PutModString(PID, mstName, "POCPhone", BLANK, SetIdx(), mstPOCPhone), "PutModString", True) Then Exit Function
  If DisplayError(PutModString(PID, mstName, "POCFax", BLANK, SetIdx(), mstPOCFax), "PutModString", True) Then Exit Function
  If DisplayError(PutModString(PID, mstName, "POCEmail", BLANK, SetIdx(), mstPOCEmail), "PutModString", True) Then Exit Function
  
  'Requirement Info
  If DisplayError(PutModString(PID, mstName, "OperatingSystem", BLANK, SetIdx(), mstOperatingSystem), "PutModString", True) Then Exit Function
  If DisplayError(PutModString(PID, mstName, "Processor", BLANK, SetIdx(), mstProcessor), "PutModString", True) Then Exit Function
  If DisplayError(PutModInteger(PID, mstName, "RAM", BLANK, SetIdx(), miRAM), "PutModInteger", True) Then Exit Function
  If DisplayError(PutModInteger(PID, mstName, "DiskSpace", BLANK, SetIdx(), miDiskSpace), "PutModInteger", True) Then Exit Function
  
  modhndl = ModuleGetHandle(PID, mstName)
  'Input Dictionary
  If (mstDictionary <> BLANK) Then
    dichndl = DictionaryGetHandle(PID, mstDictionary)
    ModuleAddDictionary PID, modhndl, dichndl
  End If
  
  'Add module to impDomain domain, define in mdiImportDes
  'more work here in regards to
  domhndl = DomainGetHandle(PID, impDomain)
  DomainAddModule PID, domhndl, modhndl
  If (mstGroup <> "") Then
    clshndl = ClassGetHandle(PID, domhndl, mstClass)
    If (clshndl > 0) Then
      grphndl = GroupGetHandle(PID, domhndl, clshndl, mstGroup)
      If (grphndl > 0) Then
        GroupAddModule PID, domhndl, clshndl, grphndl, modhndl
        If (mstSubGrp <> "") Then
          subhndl = SubGroupGetHandle(PID, domhndl, clshndl, grphndl, mstSubGrp)
          If (subhndl > 0) Then
            SubGroupAddModule PID, domhndl, clshndl, grphndl, subhndl, modhndl
          End If
        End If
      End If
    End If
  End If
  
  'Place Schemes
  For Each objScheme In mobjSchemes
    iCount = iCount + 1
    If mobjSchNames.count > 0 Then
      stName = "Scheme " + Format(CStr(iCount), "00") + "-" + mobjSchNames(iCount)
      stNote = mobjSchNotes(iCount)
    Else
      stName = "Scheme " & Format(CStr(iCount), "00")
      stNote = "Module file generated by ImportDes.exe." + vbCrLf + _
              "1x file types are used for the scheme name,  " + vbCrLf + _
              "# indicates the instance count of the file type.  " + vbCrLf + _
              "There are no input file types used for this scheme."
    End If
    
    If DisplayError(AddScheme(PID, mstName, stName), "AddScheme", True) Then Exit Function
    If DisplayError(PutModString(PID, mstName, "SchemeNote", BLANK, SetIdx(iCount), stNote), "PutModString", True) Then Exit Function
    
    OrderDependencies objScheme.item(InputDics)
    OrderDependencies objScheme.item(OutputDics)
    'add georeference to every input scheme if not database
    If mstClass <> "Database" And mstClass <> "System" And objScheme.item(InputDics).count > 0 Then
      If DisplayError(AddSchemeIDic(PID, mstName, stName, "GeoReference"), "AddSchemeIDic", False) Then
        Print #fTmp, "Error adding " & vTemp & " to input scheme " & mstName & ", " & stName, vbOKOnly, "AddSchemeIDic"
        Exit Function
      End If
    End If
    'add input scheme
    For Each vTemp In objScheme.item(InputDics)
      If DisplayError(AddSchemeIDic(PID, mstName, stName, CStr(vTemp)), "AddSchemeIDic", False) Then
        Print #fTmp, "Error adding " & vTemp & " to input scheme " & mstName & ", " & stName, vbOKOnly, "AddSchemeIDic"
        Exit Function
      End If
    Next vTemp
    'add output scheme
    For Each vTemp In objScheme.item(OutputDics)
      If DisplayError(AddSchemeODic(PID, mstName, stName, CStr(vTemp)), "AddSchemeODic", False) Then
        Print #fTmp, "Error adding " & vTemp & " to output scheme " & mstName & ", " & stName, vbOKOnly, "AddSchemeODic"
        Exit Function
      End If
    Next vTemp
  Next objScheme
   
  'Write
  If DisplayError(SaveModule(PID, mstName), "SaveModule", True) Then Exit Function
  WriteMod = True
  Exit Function
  
WriteModErr:
  WriteMod = False
End Function
      
Sub GetScheme(outSchemes As Collection, inSchemes As Collection, scheme As Collection, idx As Long)
Dim i As Long
Dim key As String
Dim objScheme As Collection
Dim piece As ClassSchemePiece

  If scheme Is Nothing Then
    Set scheme = New Collection
    For i = 1 To inSchemes(idx).MinAllowed - 1
      scheme.add inSchemes(idx)
    Next
  End If
  For i = inSchemes(idx).MinAllowed To inSchemes(idx).MaxAllowed
    ' add piece to scheme
    If i <> 0 Then scheme.add inSchemes(idx)
    ' make copy of scheme to send on to next GetScheme call
    Set objScheme = New Collection
    For Each piece In scheme
      objScheme.add piece
    Next
    'if the last piece then add it to the copied scheme else recurse
    If idx = inSchemes.count Then
      key = ""
      For Each piece In objScheme
        key = key & piece.Qualifier & piece.FileExtension
      Next
      key = Replace(key, " ", "")
      On Error Resume Next
      outSchemes.add objScheme, key
      If Err = 0 Then Print #fTmp, key
      On Error GoTo 0
    Else
      GetScheme outSchemes, inSchemes, objScheme, idx + 1
    End If
  Next
End Sub
      
Sub ConvertScheme(iold As Collection, inew As Collection, names As Collection, notes As Collection)
  Dim tIDX1 As tINDEX
  Dim tIDX2 As tINDEX
  Dim i As Long
  Dim pos As Long
  Dim cnt As Long
  Dim lDim As Long
  Dim lReq As Long
  Dim lTemp As Long
  Dim lStatus As Long
  Dim stDic As String
  Dim stName As String
  Dim stNote As String
  Dim stFind As String
  Dim add As Boolean
  Dim objPiece As ClassSchemePiece
  Dim objScheme As Collection
  Dim newScheme As Collection
  Dim objModScheme As Collection
  Dim qIdx As Collection
  Dim itmp As Collection
  Dim isch As Collection
  
  'remove dupilcates and create name and descriptiton
  'need to add count to qualifiers
  Set itmp = New Collection
  Set isch = New Collection
  For Each objScheme In iold
    stName = ""
    stNote = "Module file generated by ImportDes.exe." + vbCrLf + _
              "1x file types are used for the scheme name,  " + vbCrLf + _
              "# indicates the instance count of the file type.  " + vbCrLf + _
              "Input file types and their respective qualifiers:" + vbCrLf + vbTab
    FormImportDes.List1.Clear
    Set qIdx = New Collection
    Set objModScheme = New Collection
    For Each objPiece In objScheme
      If stName = "" Then
        cnt = 1
        stName = objPiece.FileExtension + "#" + Format(CStr(cnt), "00")
        stNote = stNote + objPiece.FileExtension + ":" + objPiece.Qualifier
        qIdx.add 1, objPiece.FileExtension + ":" + objPiece.Qualifier
      Else
        pos = InStr(stNote, objPiece.FileExtension + ":" + objPiece.Qualifier)
        If pos = 0 Then
          cnt = 1
          qIdx.add Len(stName) + 2, objPiece.FileExtension + ":" + objPiece.Qualifier
          stName = stName + ";" + objPiece.FileExtension + "#" + Format(CStr(cnt), "00")
          stNote = stNote + vbCrLf + vbTab + objPiece.FileExtension + ":" + objPiece.Qualifier
        Else
          pos = qIdx(objPiece.FileExtension + ":" + objPiece.Qualifier)
          stFind = Mid(stName, pos, 6)
          cnt = val(Right(stFind, 2)) + 1
          On Error Resume Next
          stName = Left(stName, pos - 1) + Replace(stName, stFind, objPiece.FileExtension + "#" + Format(CStr(cnt), "00"), pos, 1)
          If Err.Number > 0 Then
            pos = pos
          End If
        End If
      End If
        'use sorted list box to hold temp list, used sorted list so comparison is easier
      FormImportDes.List1.AddItem LCase(objPiece.FileExtension + ":" + objPiece.Qualifier)
    Next
    
    'see if not a duplicate scheme then add
    add = True
    For Each newScheme In itmp
      If FormImportDes.List1.ListCount = newScheme.count Then
        For i = 1 To newScheme.count
          If newScheme(i) <> FormImportDes.List1.list(i - 1) Then Exit For
        Next
        If i - 1 = newScheme.count Then
          add = False
          Exit For
        End If
      End If
    Next
    If add Then
      For i = 0 To FormImportDes.List1.ListCount - 1
        objModScheme.add FormImportDes.List1.list(i)
      Next
      itmp.add objModScheme
      isch.add objScheme
      names.add stName
      notes.add stNote
    Else
      Print #fTmp, "Dulipcate scheme found!" + stNote
    End If
  Next
  
  'now convert schemes
  For Each objScheme In isch
    FormImportDes.List1.Clear
    Set objModScheme = New Collection
    For Each objPiece In objScheme
      tIDX1 = SetIdx()
      tIDX2 = SetIdx()
      lStatus = VarLookUp(PID, LookupDataSet, "FileType", objPiece.FileExtension, tIDX1)
      lStatus = lStatus + VarLookUp(PID, LookupDataSet, "FileQual", objPiece.Qualifier, tIDX1)
      lStatus = lStatus + GetVarDimSize(PID, LookupDataSet, "DicList", SetIdx(tIDX1.i1, tIDX1.i2, 1), lDim)
      If lStatus <> 0 Then
        MsgBox "Unable to get dictionary mappings for " + objPiece.FileExtension + ":" + objPiece.Qualifier
      Else
        For lTemp = 1 To lDim
          lStatus = GetString(PID, LookupDataSet, "DicList", BLANK, SetIdx(tIDX1.i1, tIDX1.i2, lTemp), stDic)
          lStatus = lStatus + GetInteger(PID, LookupDataSet, "DicRequired", BLANK, SetIdx(tIDX1.i1, tIDX1.i2, lTemp), lReq)
          If lStatus <> 0 Then
             Print #fTmp, "Unable to get dictionary for " + objPiece.FileExtension + ":" + objPiece.Qualifier
          Else
            objModScheme.add stDic
          End If
        Next
      End If
    Next
      
    Print #fTmp, objModScheme.count
    inew.add objModScheme
  Next
  
End Sub

Public Function ImportDesFile(ByVal mobjDesFile As ClassDESFile, ByVal stFilePfx As String) As Boolean
  Dim used() As Boolean
  Dim i As Long
  Dim j As Long
  Dim cnt As Long
  Dim scnt As Long
  Dim key As String
  Dim vTemp As Variant
  Dim objPiece As ClassSchemePiece
  Dim reqPiece As ClassSchemePiece
  Dim objScheme As Collection
  Dim reqScheme As Collection
  Dim outScheme As Collection
  Dim objDESInputScheme As New Collection
  Dim objMODInputScheme As New Collection
  Dim objDESOutputScheme As New Collection
  Dim objMODOutputScheme As New Collection
    
  On Error GoTo ImportDesFileErr
  'Copy all DES data to comparable MOD parameters
  ImportDesFile = False
  With mobjDesFile
    miDiskSpace = .DiskSpace
    miRAM = .RAM
    miSystemVersion = Int(.Nvers) 'verify this is the right correspondence
    mstClass = .ModType
    If mstClass = "database" Or mstClass = "viewer" Or mstClass = "system" Then
      mstGroup = .ModuleGroup.moduleName
    Else
      mstGroup = .ModuleGroup.ModuleType
      mstSubGrp = .ModuleGroup.moduleName
    End If
    mstIcon = .ModuleGroup.ModuleIcoPath
    mstName = .moduleName
    mstPOCAddress1 = .POCAddress
    mstPOCAddress2 = BLANK
    mstPOCCity = .POCCity
    mstPOCCompany = .POCCompany
    mstPOCContact = .POCContact
    mstPOCCountry = .POCCountry
    mstPOCEmail = .POCEmail
    mstPOCFax = .POCFax
    mstPOCPerson = .POCContact
    mstPOCPhone = .POCPhone
    mstPOCState = .POCState
    mstPOCUrl = .POCUrl
    mstPOCZip = .POCZip
    mstProcessor = .Processor
    mstOperatingSystem = .OperatingSystem
    mstVersion = .Vers
    mstDatabaseID = BLANK
    mstModelURL = BLANK
    mstPassword = BLANK
    mstLogin = BLANK
    
    mbSystemUpdate = False
    
    If .UIBat <> "" Then
      mstUIExe = WRAPPER ' AppPath & WRAPPER
      mstUICmdLine = "/UI """ & SplitPath(.ModulePath, SP_DIRR) & """ """ & SplitPath(.ModulePath, SP_TITLE) & """"
    End If
    If .EXEBat <> "" Then
      mstModelExe = WRAPPER ' AppPath & WRAPPER
      mstModelCmdLine = "/EXE """ & SplitPath(.ModulePath, SP_DIRR) & """ """ & SplitPath(.ModulePath, SP_TITLE) & """"
    End If
    If (.Nvars > 0) Then
      mstDictionary = stFilePfx
    End If
    
    'copy descriptions and references in collections
    miDescriptionCount = 0
    For Each vTemp In .DescLines
      mobjDescLines.add vTemp
      miDescriptionCount = miDescriptionCount + 1
    Next vTemp
    miReferenceCount = 0
    For Each vTemp In .RefLines
      mobjRefLines.add vTemp
      miReferenceCount = miReferenceCount + 1
    Next vTemp
    
    'Create scheme collections
    Set mobjSchemes = New Collection
    
    'permute input schemes
    scnt = 1
    For Each objScheme In .FRead
      Set reqScheme = New Collection
      For i = 1 To objScheme.count
        Set objPiece = objScheme(i)
        For j = 1 To objPiece.MinAllowed
          reqScheme.add objPiece
        Next
      Next
      For i = 1 To objScheme.count
       'rescursive call to get all input schemes
        GetScheme objDESInputScheme, objScheme, Nothing, i
      Next
      For i = objDESInputScheme.count To scnt Step -1
        cnt = 0
        Set objScheme = objDESInputScheme(i)
        ReDim used(objScheme.count) As Boolean
        For Each reqPiece In reqScheme
          j = 0
          For Each objPiece In objScheme
            If reqPiece.Qualifier = objPiece.Qualifier And _
               reqPiece.FileExtension = objPiece.FileExtension And _
               Not used(j) Then
              cnt = cnt + 1
              used(j) = True
            End If
            j = j + 1
          Next
        Next
        If cnt < reqScheme.count Then
          objDESInputScheme.Remove i
        End If
        scnt = objDESInputScheme.count + 1
      Next
    Next
    'if no input create blank input
    If .FRead.count = 0 Then
      Set reqScheme = New Collection
      objMODInputScheme.add reqScheme
    End If
       
    'permute output schemes
    Set objDESOutputScheme = .FWrite
    Set mobjSchNames = New Collection
    Set mobjSchNotes = New Collection
    Dim onote As Collection
    Dim oname As Collection
    Set onote = New Collection
    Set oname = New Collection
    
    'Convert the output scheme from DES to MOD
    ConvertScheme objDESInputScheme, objMODInputScheme, mobjSchNames, mobjSchNotes
    ConvertScheme objDESOutputScheme, objMODOutputScheme, oname, onote

    ' add location dics, if needed, to output
    ' (ie starter modules with dependencies on location)
    If Check(objDESOutputScheme(1), "epf") Then
      For Each objScheme In objDESInputScheme
        If Not (Check(objScheme, "wcf") Or Check(objScheme, "scf") Or Check(objScheme, "ato")) Then
          AddLocationFiles objMODOutputScheme(1)
          Exit For
        End If
      Next
    ElseIf Check(objDESOutputScheme(1), "rif") Then
      For Each objScheme In objDESInputScheme
        If Not Check(objScheme, "epf") Then
          AddLocationFiles objMODOutputScheme(1)
          Exit For
        End If
      Next
    End If

    'connect output scheme to all input schema
    For Each objScheme In objMODInputScheme
      Set reqScheme = New Collection
      reqScheme.add objScheme, InputDics
      reqScheme.add objMODOutputScheme(1), OutputDics
      mobjSchemes.add reqScheme
    Next
  End With
  mbLoaded = True
  ImportDesFile = True
  Exit Function
  
ImportDesFileErr:
  ImportDesFile = False
End Function

Private Function Check(scheme As Collection, typ As String) As Boolean
Dim piece As ClassSchemePiece

  Check = True
  For Each piece In scheme
    If typ = piece.FileExtension Then Exit Function
  Next
  Check = False
End Function

Private Sub AddLocationFiles(scheme As Collection)
Dim objPiece As ClassSchemePiece

  scheme.add "SurfaceWaterPoints"
  scheme.add "AquiferPoints"
  scheme.add "SoilVolumes"
  scheme.add "AirPoints"
End Sub
