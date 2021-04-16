Attribute VB_Name = "DataTypes"
Option Explicit

Public Type Exposure
  Route As String
  Path As String
  Measure As String
  Unit As String
End Type

Private Const MAX_PATH As Long = 260
Private Const ERROR_FILE_NO_ASSOCIATION As Long = 31
Private Const ERROR_FILE_NOT_FOUND As Long = 2
Private Const ERROR_PATH_NOT_FOUND As Long = 3
Private Const ERROR_FILE_SUCCESS As Long = 32 'my constant
Private Const ERROR_BAD_FORMAT As Long = 11


Declare Function GetPrivateProfileString Lib "kernel32" Alias "GetPrivateProfileStringA" (ByVal lpApplicationName As String, ByVal lpKeyName As Any, ByVal lpDefault As String, ByVal lpReturnedString As String, ByVal nSize As Long, ByVal lpFileName As String) As Long
Declare Function GetPrivateProfileInt Lib "kernel32" Alias "GetPrivateProfileIntA" (ByVal lpApplicationName As String, ByVal lpKeyName As String, ByVal nDefault As Long, ByVal lpFileName As String) As Long
Declare Function FindExecutable Lib "shell32" Alias "FindExecutableA" (ByVal lpFile As String, ByVal lpDirectory As String, ByVal sResult As String) As Long

Global AnError As Boolean       'error flag to tell when to keep error file
Global errfile As csv
Global errflag As Boolean
Global argv() As String
Global argc As Long
Global FUIName As String        'the path and name of the run
Global RunName As String        'the name of the parameter file
Global RefName As String        'the name of the reference file
Global SiteIndex As Long     'the index to site model parms
Global SUIndex As Long       'the index to saturated model parms
Global Model As String
Global currentref As Integer    'the current selected reference
Global Const thismod As String = "\SU"

Global frm As Object
Global HelpFileName As String   'the name of the reference file
Global HelpAvailable As Boolean 'does the help file exist
Global HelpAnchor As String     'the name of the help anchor

Global RefFileName As String    'the name of the reference file
Global RefAvailable As Boolean  'does the reference file exist
Global RefMode As Long          'if 0 then select a reference else add a reference
Global RefIdx As Long           'the current selected reference
Global RefItem As Integer       'the current selected item index

Type glyphtype
  id As String
  lbl As String
  Path As String
  Idx As Long
  pos As Long
  Class As String
  group As String
  type As String
  ftype() As String
  fqual() As String
  fcount As Integer
  exe As String
End Type

Type CSMtype
  id As String
  label As String
  Class As String
  group As String
  despath As String
  sinkCt As Integer
  sink() As String
  sinkType() As String
End Type

Type correlation
  cAlias As String
  cFactor As Double
  ref As Long
End Type

Type variable
  vDup As Long
  vName As String
  vAlias As String
  vExe As String
  vDes As String
  vDes2 As String
  vEqu As String
  vDistrib As Long
  vMod As String
  vMin As String
  vMax As String
  vFmt As String
  vUpper As New tFloat
  vLower As New tFloat
  vMean As New tFloat
  vStd As New tFloat
  vScal As New tFloat
  vShift As New tFloat
  vMode As New tFloat
  vBase As Long
  vUnit As String
  vCor() As correlation
  cnt As Long
  Idx(5) As Long
  ref As Long
  dref As Long
  eref As Long
End Type

Type output
  oDes As String
  oDes2 As String
  oAlias As String
  oExt As String
  oType As String
  oTime As String
  oTimePtCnt As Long
  oTimePt() As New tFloat
  oSourceID As String
  oSourceName As String
  oCASID As String
  oCASName As String
  oParentID As String
  oParentName As String
  oOrgID As String
  oOrgName As String
  ref As Long
End Type

Type daughter
  name As String
  cas As String
  typ As Long
End Type

Type constituent
  name As String
  cas As String
  typ As Double
  numprog As Long
  progeny() As daughter
End Type

Type organism
  name As String
  id As String
End Type

Type descriptor
  name As String
  des As String
  typ As String
  datatype As String
  fmt As String
  Unit As String
  min As String
  max As String
  qCnt As Long
  ques() As String
End Type

Global vCnt As Long
Global var() As variable

Global oCnt As Long
Global out() As output

Global gCnt(8) As Long

Global mCnt As Long
Global glyph() As glyphtype

Global sCnt As Long
Global susrc() As glyphtype

'specific to constituent driver
Global cCnt As Long
Global contam() As constituent

Global orgCnt As Long
Global org() As organism

Global FRAMES_INI As String

' the following 4 functions are also found in common.bas

Sub getargs()
  Dim args As String
  Dim pos As Long
    
  argc = 0
  args = Trim$(Command$)
  If Len(args) > 0 Then
    Do
      pos = InStr(args, " ")
      argc = argc + 1
      If pos > 0 Then
        ReDim Preserve argv(argc) As String
        argv(argc - 1) = Trim$(Left$(args, pos))
        args = Trim$(Right$(args, Len(args) - pos))
      Else
        ReDim Preserve argv(argc)
        argv(argc - 1) = Trim$(args)
      End If
    Loop Until pos = 0
  End If
End Sub

Sub SetHelpFile(filename As String)
Dim temp As String

  HelpFileName = filename
  temp = Dir(HelpFileName)
  If Len(temp) > 0 Then
    HelpAvailable = True
    frm.howto.Enabled = True
  Else
    HelpAvailable = False
    On Error Resume Next ' reference to specific control in general purpose module
    frm.howto.Enabled = False
  End If
End Sub

Public Function ConvertURL(URLSTRING As String) As String
    Dim i As Integer
    Dim tempchar As String
    Dim rstring As String
    rstring = ""
    For i = 1 To Len(URLSTRING)
        tempchar = Mid(URLSTRING, i, 1)
        If tempchar = "\" Then
            rstring = rstring + "/"
        Else
            rstring = rstring + tempchar
        End If
    Next i
    ConvertURL = "file:///" + rstring
End Function

Sub GetHelp()
Dim url As String
Dim success As Long
Dim pos As Long
Dim sResult As String
Dim msg As String

  If HelpAvailable Then
    sResult = Space$(MAX_PATH)

    ' lpFile: name of the file of interest
    ' lpDirectory: location of lpFile
    ' sResult: path and name of executable associated with lpFile
    success = FindExecutable(HelpFileName, "", sResult)
      
    Select Case success
      Case ERROR_FILE_NO_ASSOCIATION: msg = "no association"
      Case ERROR_FILE_NOT_FOUND: msg = "file not found"
      Case ERROR_PATH_NOT_FOUND: msg = "path not found"
      Case ERROR_BAD_FORMAT:     msg = "bad format"
      Case Is >= ERROR_FILE_SUCCESS:
        pos = InStr(sResult, Chr$(0))
        If pos Then
          url = Left$(sResult, pos - 1)
          url = url + " " + ConvertURL(HelpFileName + "#" + UCase(HelpAnchor))
          Shell url, vbNormalFocus
          Exit Sub
        End If
        Exit Sub
    End Select
    MsgBox msg, vbOKOnly, HelpFileName
  End If
End Sub

Sub SetRefFile(filename As String)
Dim temp As String
  RefFileName = filename
  temp = Dir(RefFileName)
  If Len(temp) > 0 Then
    RefAvailable = True
  Else
    RefAvailable = False
  End If
End Sub

Sub GetRef(RefLabel As Object)
  If RefItem = -1 Then Exit Sub
#If NOREFERENCE Then
  Exit Sub
#Else
  RefIdx = RefLabel.tag
  Reference.Show 1
  If RefIdx <> -1 Then
    RefLabel.tag = RefIdx
    RefLabel.Caption = "Ref:" & Str(RefIdx)
  End If
#End If
End Sub

