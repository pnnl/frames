VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ClassDICFile"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

'Dictionary file object

'===============================================================
' Local variables to hold property values.
'===============================================================
Private mbLoaded As Boolean
Private mobjDicLines As Collection
Private mobjDicHeader As ClassDicHeader

Private Sub Class_Initialize()
  mbLoaded = False
  Set mobjDicLines = New Collection
  Set mobjDicHeader = New ClassDicHeader
End Sub

Private Sub Class_Terminate()
  Set mobjDicLines = Nothing
End Sub

Public Sub DeleteDic()
  On Error Resume Next
  If Not mbLoaded Then Exit Sub
  DelDictionary PID, mobjDicHeader.Name 'in case it already exists
End Sub

Public Function WriteDic(ByVal stFileName As String, ByVal stDstPath As String) As Boolean
  Dim objDicPiece As ClassDicDeclaration
  Dim vals() As String
  Dim vTemp As Variant
  Dim domhdl As Long
  Dim dichdl As Long

  On Error GoTo WriteDicErr
  
  WriteDic = False
  If Not mbLoaded Then Exit Function
  
  With mobjDicHeader
    If DisplayError(AddNewDictionary(PID, stDstPath & stFileName, .Name), "AddNewDictionary", True) Then GoTo WriteDicErr
    If DisplayError(SetDicPrivilege(PID, .Name, .privilege), "SetDicPrivilege", True) Then GoTo WriteDicErr
    If DisplayError(SetDicDescription(PID, .Name, .description), "SetDicDescription", True) Then GoTo WriteDicErr
    If DisplayError(SetDicVersion(PID, .Name, .version), "SetDicVersion", True) Then GoTo WriteDicErr
  ' comment out until we settle
    domhdl = DomainGetHandle(PID, impDomain)
    dichdl = DictionaryGetHandle(PID, .Name)
    DomainAddDictionary PID, domhdl, dichdl
  End With
  
  For Each objDicPiece In mobjDicLines
    If objDicPiece.Name <> "" Then
      DelVariable PID, mobjDicHeader.Name, objDicPiece.Name
      If DisplayError(AddVariable(PID, mobjDicHeader.Name, objDicPiece.Name), "AddVariable", True) Then GoTo WriteDicErr
      If DisplayError(SetVarDescription(PID, mobjDicHeader.Name, objDicPiece.Name, objDicPiece.description), "SetDescription", True) Then GoTo WriteDicErr
      If DisplayError(SetVarType(PID, mobjDicHeader.Name, objDicPiece.Name, objDicPiece.DataType), "SetDataType", True) Then GoTo WriteDicErr
      If DisplayError(SetVarScalar(PID, mobjDicHeader.Name, objDicPiece.Name, objDicPiece.Scalar), "SetScalar", True) Then GoTo WriteDicErr
      If DisplayError(SetVarMin(PID, mobjDicHeader.Name, objDicPiece.Name, objDicPiece.Minimum), "SetVarMin", True) Then GoTo WriteDicErr
      If DisplayError(SetVarMax(PID, mobjDicHeader.Name, objDicPiece.Name, objDicPiece.Maximum), "SetVarMax", True) Then GoTo WriteDicErr
      'verify that all units got set
      If objDicPiece.Measure <> BLANK Then
        If objDicPiece.Unit <> BLANK Then
          If DisplayError(SetVarMeasure(PID, mobjDicHeader.Name, objDicPiece.Name, objDicPiece.Measure), "SetVarMeasure", True) Then GoTo WriteDicErr
          If DisplayError(SetVarUnit(PID, mobjDicHeader.Name, objDicPiece.Name, objDicPiece.Unit), "SetUnit", True) Then GoTo WriteDicErr
        Else
          Print #fTmp, mobjDicHeader.Name & " (unit)," & objDicPiece.Name & "," & objDicPiece.Measure & "," & objDicPiece.Unit
        End If
      Else
        If objDicPiece.Unit <> BLANK Then
          Print #fTmp, mobjDicHeader.Name & " (measure)," & objDicPiece.Name & "," & objDicPiece.Measure & "," & objDicPiece.Unit
        End If
      End If
      If DisplayError(SetVarStochastic(PID, mobjDicHeader.Name, objDicPiece.Name, objDicPiece.Stochastic), "SetStochastichastic", True) Then GoTo WriteDicErr
      If DisplayError(SetVarPreposition(PID, mobjDicHeader.Name, objDicPiece.Name, objDicPiece.preposition), "SetVarPreposition", True) Then GoTo WriteDicErr
      If DisplayError(SetVarPrimaryKey(PID, mobjDicHeader.Name, objDicPiece.Name, objDicPiece.PrimaryKey), "SetVarPrimaryKey", True) Then GoTo WriteDicErr
    End If
  Next
  
  'Need to separate this part out so that all of the referenced variables are present
  For Each objDicPiece In mobjDicLines
    For Each vTemp In objDicPiece.indices
      vals = Split(vTemp, ".")
      If UBound(vals) = 1 Then
        If DisplayError(AddVarIndex(PID, mobjDicHeader.Name, objDicPiece.Name, vals(0), vals(1)), "AddVarIndex", True) Then GoTo WriteDicErr
      Else
        If DisplayError(AddVarIndex(PID, mobjDicHeader.Name, objDicPiece.Name, mobjDicHeader.Name, vals(0)), "AddVarIndex", True) Then GoTo WriteDicErr
      End If
    Next
  Next
  
  If DisplayError(SaveDictionary(PID, mobjDicHeader.Name), "SaveDictionary", True) Then GoTo WriteDicErr
  WriteDic = True
  Exit Function
  
WriteDicErr:
  WriteDic = False
  Print #fTmp, mobjDicHeader.Name & " (general)," & objDicPiece.Name & "," & objDicPiece.Measure & "," & objDicPiece.Unit
  
End Function

Public Function ImportDesFile(ByVal mobjDesFile As ClassDESFile, ByVal stFilePfx As String) As Boolean
  Dim objVar As ClassDesDeclaration
  Dim objIdx As ClassDesDeclaration
  Dim objQue As ClassDesQue
  Dim objDicPiece As ClassDicDeclaration
  Dim newvars As Collection
  Dim col As Collection
  Dim vTemp As Variant
  
  On Error Resume Next
  'Copy all DES data to comparable DIC parameters
  
  ImportDesFile = False
  mobjDicHeader.description = mobjDesFile.moduleName
  mobjDicHeader.Name = stFilePfx
  mobjDicHeader.privilege = 0
  mobjDicHeader.version = 1
  mobjDicHeader.Updated = 0
  mobjDicHeader.Template = BLANK
  
  Set newvars = New Collection
    
  Set objVar = New ClassDesDeclaration
  objVar.Scalar = FalseFlag
  objVar.Name = "zVAR"
  objVar.description = "Variable names"
  newvars.add objVar, objVar.Name
  
  Set objIdx = New ClassDesDeclaration
  objIdx.Scalar = FalseFlag
  objIdx.Name = "zzIDX"
  objIdx.description = "Index info"
  objIdx.indices.add objVar.Name
  newvars.add objIdx, objIdx.Name
  
  Set objIdx = New ClassDesDeclaration
  objIdx.Scalar = FalseFlag
  objIdx.Name = "zzREF"
  objIdx.description = "Reference info"
  objIdx.DataType = dtINTEGER
  objIdx.indices.add objVar.Name
  newvars.add objIdx, objIdx.Name
  
  Set objIdx = New ClassDesDeclaration
  objIdx.Scalar = FalseFlag
  objIdx.Name = "zzREC"
  objIdx.description = "Record number"
  objIdx.DataType = dtINTEGER
  objIdx.indices.add objVar.Name
  newvars.add objIdx, objIdx.Name
  
  Set objIdx = New ClassDesDeclaration
  objIdx.Scalar = FalseFlag
  objIdx.Name = "zzUUNT"
  objIdx.description = "Unit info"
  objIdx.indices.add objVar.Name
  newvars.add objIdx, objIdx.Name
  
  Set objIdx = New ClassDesDeclaration
  objIdx.Scalar = FalseFlag
  objIdx.Name = "zzUNT"
  objIdx.description = "Unit info"
  objIdx.indices.add objVar.Name
  newvars.add objIdx, objIdx.Name
  
  Set objIdx = New ClassDesDeclaration
  objIdx.Scalar = FalseFlag
  objIdx.Name = "zzFMT"
  objIdx.description = "Unit info"
  objIdx.indices.add objVar.Name
  newvars.add objIdx, objIdx.Name
  
  Set objIdx = New ClassDesDeclaration
  objIdx.Scalar = TrueFlag
  objIdx.Name = "zzQues"
  objIdx.description = "Description que info"
  objIdx.indices.add objVar.Name
  newvars.add objIdx, objIdx.Name
  
  'add any all orignal
  For Each objVar In mobjDesFile.vars
    newvars.add objVar, objVar.Name
  Next
  Set mobjDesFile.vars = newvars
  
  For Each objVar In mobjDesFile.vars
    Set objDicPiece = New ClassDicDeclaration
    objDicPiece.Name = objVar.Name
    objDicPiece.description = objVar.description
    objDicPiece.Scalar = objVar.Scalar
    objDicPiece.Unit = objVar.Unit
    objDicPiece.Stochastic = IIf(objVar.Stochastic = "NOT STOCHASTIC", FalseFlag, TrueFlag)
    objDicPiece.DataType = objVar.DataType
    Select Case objVar.DataType
      Case dtFLOAT, dtINTEGER:
        objDicPiece.DataType = objVar.DataType
        objDicPiece.Minimum = objVar.Minimum
        objDicPiece.Maximum = objVar.Maximum
        If objVar.VarMinS = BLANK And objVar.VarMaxS = BLANK Then
          objDicPiece.Minimum = 0
          objDicPiece.Maximum = 0
        End If
      Case Else: ' dtSTRING, dtLOGICAL or unknown
        objDicPiece.DataType = dtSTRING
    End Select
    If objDicPiece.Maximum < objDicPiece.Minimum Then
      objDicPiece.Maximum = 0
      objDicPiece.Minimum = 0
      Print #fTmp, "Min and Max inconsistant for " & objDicPiece.Name & " in " & mobjDicHeader.Name
    End If
    For Each vTemp In objVar.indices
      objDicPiece.indices.add CStr(vTemp)
    Next
    mobjDicLines.add objDicPiece, objDicPiece.Name
  Next
  
  'Its assumed that all units will be contain in the convert.csv
  'change datatype to string if can't be found, in writedic
  For Each objDicPiece In mobjDicLines
    Set col = Nothing
    Set col = colUnits(objDicPiece.Unit)
    If Err.Number = 0 Then
      objDicPiece.Measure = col.item(1)
    End If
    Err.Clear
  Next objDicPiece
  
  mobjDicHeader.VariableCount = mobjDicLines.count
  mbLoaded = True
  ImportDesFile = True
End Function
