VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "clsFrames"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit

Public Enum enuFileMode
  efmF_WRITE = 1
  efmF_READ = 2
  efmF_APPEND = 3
End Enum

Public Type csv
  pos As Long
  fnum As Long
  fmode As Long
  fname As String
  leng As Long
  getbuff As String
  putbuff As String
  separator As String
End Type

Public Type parmrec
  pname As String
  idx1 As Long
  idx2 As Long
  idx3 As Long
  idx4 As Long
  idx5 As Long
  idx6 As Long
  Ref As Long
  uunit As String
  cunit As String
  pval As String
End Type

Public Type parmfile
  pos As Long
  file As csv
  find_pattern As String
  pList() As parmrec
  pLookup As Collection
End Type

Public Enum enuFramesError
  efeInvalidArguments = vbObjectError + 512 + 601
  efeDESFile = vbObjectError + 512 + 602
  efeErrFile = vbObjectError + 512 + 603
End Enum

Event Error(lErrNum As Long, sErrStr As String)
Event ProgressPercent(iPercent As Integer)
Event ActionMessages(sMajorLabel As String, sMajorMsg As String, sMinorLabel As String, sMinorMsg As String)

Public Property Let AnError(ByVal vData As Boolean)
    g_AnError = vData
End Property

Public Property Get AnError() As Boolean
    AnError = g_AnError
End Property

Public Property Let RefMode(ByVal vData As Long)
    RefMode = vData
End Property

Public Property Get RefMode() As Long
    RefMode = RefMode
End Property

Public Function ReferenceAvailable() As Boolean
    ReferenceAvailable = RefAvailable
End Function

Public Property Let RefFileName(ByVal vData As String)
Dim temp As String
  RefFileName = vData
  temp = Dir(RefFileName)
  If Len(temp) > 0 Then
    RefAvailable = True
  Else
    RefAvailable = False
  End If
End Property

Public Property Get RefFileName() As String
    RefFileName = RefFileName
End Property

Public Property Let Ref(ByVal vData As Integer)
    RefIdx = vData
    Ref = vData
End Property

Public Property Get Ref() As Integer
  Reference.Show 1
  If RefIdx <> -1 Then
    Ref = RefIdx
  End If
End Property

Public Function HelpAvailable() As Boolean
    HelpAvailable = g_HelpAvailable
End Function

Public Property Let HelpFileName(ByVal vData As String)
Dim temp As String
  g_HelpFileName = vData
  temp = Dir(g_HelpFileName)
  If Len(temp) > 0 Then
    g_HelpAvailable = True
  Else
    g_HelpAvailable = False
  End If
End Property

Public Property Get HelpFileName() As String
    HelpFileName = g_HelpFileName
End Property

Public Property Let HelpAnchor(ByVal vData As String)
    g_HelpAnchor = vData
End Property

Public Property Get HelpAnchor() As String
    HelpAnchor = g_HelpAnchor
End Property

Public Function GetHelp() As Integer
    GetSomeHelp
End Function

Public Property Let ModName(ByVal vData As String)
    g_ModName = vData
End Property

Public Property Get ModName() As String
    ModName = g_ModName
End Property

Public Property Let ModIdx(ByVal vData As Long)
    g_ModIdx = vData
End Property

Public Property Get ModIdx() As Long
    ModIdx = g_ModIdx
End Property

Public Property Let SiteIdx(ByVal vData As Long)
    g_SiteIdx = vData
End Property

Public Property Get SiteIdx() As Long
    SiteIdx = g_SiteIdx
End Property

Public Property Let RunName(ByVal vData As String)
    g_RunName = vData
End Property

Public Property Get RunName() As String
    RunName = g_RunName
End Property

Public Property Let FUIName(ByVal vData As String)
    g_FUIName = vData
End Property

Public Property Get FUIName() As String
    FUIName = g_FUIName
End Property

Public Property Let DesName(ByVal vData As String)
    g_DesName = vData
End Property

Public Property Get DesName() As String
    DesName = g_DesName
End Property

Public Property Get RunPath() As String 'KMM    6-21-02
    RunPath = g_RunPath
End Property

'--------------calls related to CSV_IO.bas
Public Function CloseCSV(tCsv As csv)
    close_csv tCsv
End Function

Public Function GetLine(tCsv As csv) As Long
    get_line tCsv
End Function

Public Function GetVal(tCsv As csv) As String
    GetVal = get_val(tCsv)
End Function

Public Function InitCSV(fname As String, fmode As enuFileMode, tCsv As csv) As Long
    Dim bRet As Boolean
    bRet = open_csv(tCsv, fname, fmode)
End Function

Public Function PutLine(tCsv As csv) As Long
    put_line tCsv
End Function

Public Function PutVal(vVal As Variant, tCsv As csv)
    put_val tCsv, vVal
End Function

Public Function FindFirstCSV(tCsv As csv, vVal As Variant) As Boolean
    'This function searches the tCsv to find vVal in the first field of the CSV
    Dim vTest As Variant
    
    reset_csv tCsv    'reset_csv does a GetLine
    Do
        vTest = GetVal(tCsv)
        If vTest <> vVal Then GetLine tCsv
    Loop Until vTest = vVal Or Len(tCsv.getbuff) = 0
    FindFirstCSV = (vTest = vVal)
End Function

Public Function FindModSectionCSV(tCsv As csv, vVal As Variant) As Long
    'this module looks for a Module section and returns the number of following 'records'
    If FindFirstCSV(tCsv, vVal) <> False Then FindModSectionCSV = CLng(GetVal(tCsv))
End Function

'--------calls related to common.bas
Public Sub EndAppModule()
    EndModule
End Sub

Public Function StartAppModule(ModForm As Object, Title As String, argcnt As Long, sCmdLine As String) As Long
    StartAppModule = StartModule(ModForm, Title, argcnt, sCmdLine)
End Function

'--------calls related to PARM_IO
Function CloseParm(pfile As parmfile) As Long
    close_parm pfile
End Function

Function FindFirstParm(pfile As parmfile, prec As parmrec, Match As String) As Boolean
    FindFirstParm = find_frst(pfile, prec, Match)
End Function

Function FindNextParm(pfile As parmfile, prec As parmrec) As Boolean
    FindNextParm = find_next(pfile, prec)
End Function

Function OpenParm(pfile As parmfile, fname As String, fmode As enuFileMode) As Boolean
    OpenParm = open_parm(pfile, fname, CInt(fmode))
End Function

Function ReadParmRec(pfile As parmfile, prec As parmrec) As Long
    ReadParmRec = read_parmrec(pfile, prec)
End Function

Function SetParm(prec As parmrec, pname As String, i1 As Long, i2 As Long, i3 As Long, i4 As Long, i5 As Long, i6 As Long, Ref As Long, uunit As String, cunit As String, pval As String) As Long
    set_parm prec, pname, i1, i2, i3, i4, i5, i6, Ref, uunit, cunit, pval
End Function

Function WriteParmRec(pfile As parmfile, prec As parmrec) As Long
    write_parmrec pfile, prec
End Function

Function WriteSParmRec(pfile As parmfile, prec As parmrec) As Long
    write_sparmrec pfile, prec
End Function

Function PutTransactions(ByVal marker As String, ByVal inpfile As String, pfileout As parmfile) As Long
    Put_Transactions marker, inpfile, pfileout
End Function


