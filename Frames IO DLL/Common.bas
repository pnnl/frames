Attribute VB_Name = "Common"
Option Explicit
Option Compare Text

Global g_DesName As String        'the path and name of the run
Global g_FUIName As String        'the path and name of the run
Global g_RunName As String        'the name of the parameter file
Global g_RunPath As String        'the path of the run  KMM 6-21-02
Global g_SiteIdx As Long          'the index to site
Global g_ModIdx As Long           'the index to module
Global g_ModName As String        'the name of the module glyph
Global called As Boolean
Global frm As Object
Global frmRef As Object

Global g_HelpFileName As String   'the name of the reference file
Global g_HelpAvailable As Boolean 'does the help file exist
Global g_HelpAnchor As String     'the name of the help anchor
 
Global RefFileName As String    'the name of the reference file
Global RefAvailable As Boolean  'does the reference file exist
Global RefMode As Long          'if 0 then select a reference else add a reference
Global RefIdx As Long           'the current selected reference

Global errfile As csv             'error file declaration
Global g_AnError As Boolean       'error flag to tell when to keep error file
Global argv() As String           'command line argument array
Global argc As Long               'command line argument count
Global saving As Boolean
Global FRAMES_INI As String

Global des As csv                 'des file declaration
   
Private Const MAX_PATH As Long = 260
Private Const ERROR_FILE_NO_ASSOCIATION As Long = 31
Private Const ERROR_FILE_NOT_FOUND As Long = 2
Private Const ERROR_PATH_NOT_FOUND As Long = 3
Private Const ERROR_FILE_SUCCESS As Long = 32 'my constant
Private Const ERROR_BAD_FORMAT As Long = 11
   
Declare Function GetPrivateProfileString Lib "kernel32" Alias "GetPrivateProfileStringA" (ByVal lpApplicationName As String, ByVal lpKeyName As Any, ByVal lpDefault As String, ByVal lpReturnedString As String, ByVal nSize As Long, ByVal lpFileName As String) As Long
Declare Function FindExecutable Lib "shell32" Alias "FindExecutableA" (ByVal lpFile As String, ByVal lpDirectory As String, ByVal sResult As String) As Long
Private Declare Function ShellExecute _
    Lib "shell32.dll" Alias "ShellExecuteA" ( _
    ByVal hwnd As Long, _
    ByVal lpOperation As String, _
    ByVal lpFile As String, _
    ByVal lpParameters As String, _
    ByVal lpDirectory As String, _
    ByVal nShowCmd As Long) As Long
    
Function StartModule(ModForm As Object, Title As String, argcnt As Long, sCmdLine As String) As Long
    On Local Error GoTo StartModuleErr1
    Dim lHC As Long      'function return(Hold) Code  - (other people are probably iRC for return code)
    lHC = 0              'Functions will return 0 by default; Err if an error; other codes as assigned

    FRAMES_INI = App.Path + "\\FramesUI.ini"
    Dim lRc As Long
    called = False
    argv = Split(sCmdLine, " ")
    If argv(0) > "" Then
      argc = UBound(argv) + 1
    Else
      argc = 0
    End If
    
    g_FUIName = argv(argcnt - 5) & ".GID"
    g_RunName = argv(argcnt - 4)
    g_RunPath = Left$(argv(argcnt - 4), InStrRev(argv(argcnt - 4), "\"))
    g_SiteIdx = Val(argv(argcnt - 3))
    g_ModIdx = Val(argv(argcnt - 2))
    g_ModName = argv(argcnt - 1)
    
    If argc < argcnt Then
        lRc = efeInvalidArguments
        Exit Function
    ElseIf g_SiteIdx + g_ModIdx < 2 Then
        lRc = efeInvalidArguments
        Exit Function
    ElseIf Not open_csv(errfile, g_RunName & ".ERR", 1) Then
        lRc = efeErrFile
        Exit Function
    End If
    
    g_AnError = False
    load_convert
    
    If Not ModForm Is Nothing Then
        Set frm = ModForm
        frm.Caption = Title + " - " + g_ModName
        frm.Show
    End If
    
    Set frmRef = Nothing

    lHC = lRc
StartModuleExit:
    On Error Resume Next
    StartModule = lHC   'unrem this line for functions
    Exit Function

'****************************************************************************************************************************
StartModuleErr1:
    lHC = Abs(Err) * -1   'unrem this line for functions
    Select Case Err
       'Case whatever       '* notes about this special case
       '    Resume Next
       Case Else
         '* if GUI level use these
         'msg = "Error " & Err.Number & vbCrLf & Err.Description & vbCrLf & err.source
         'Title = App.EXEName & "::StartModule"
         'MsgBox msg, vbExclamation, Title
         'Resume StartModuleExit

         '* if .CLS use these
       'Screen.MousePointer = iPointer
         If UCase$(Left$(Err.Source, 3)) = "CLS" Then
             Err.Raise Err.Number, Err.Source, Err.Description
         Else
             Err.Raise Err.Number, "CLS-Common ::StartModule", Err.Description
         End If
    End Select

Resume StartModuleExit
  
End Function

Sub EndModule()
  If Not called Then
    called = True
    close_csv errfile
    If Not g_AnError Then Kill g_RunName & ".ERR"
'    Unload frm
  End If
End Sub

Sub PutError(myError As String)
  put_val errfile, myError
  put_line errfile
  g_AnError = True
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

Sub GetSomeHelp()
Dim url As String
Dim success As Long
Dim pos As Long
Dim sResult As String
Dim msg As String

  If g_HelpAvailable Then
    sResult = Space$(MAX_PATH)

    ' lpFile: name of the file of interest
    ' lpDirectory: location of lpFile
    ' sResult: path and name of executable associated with lpFile
    success = FindExecutable(g_HelpFileName, "", sResult)
      
    Select Case success
      Case ERROR_FILE_NO_ASSOCIATION: msg = "no association"
      Case ERROR_FILE_NOT_FOUND: msg = "file not found"
      Case ERROR_PATH_NOT_FOUND: msg = "path not found"
      Case ERROR_BAD_FORMAT:     msg = "bad format"
      Case Is >= ERROR_FILE_SUCCESS:
        pos = InStr(sResult, Chr$(0))
        If pos Then
          url = Left$(sResult, pos - 1)
          url = url + " " + ConvertURL(g_HelpFileName + "#" + UCase(g_HelpAnchor))
          Shell url, vbNormalFocus
          Exit Sub
        End If
        Exit Sub
    End Select
    MsgBox msg, vbOKOnly, g_HelpFileName
  End If
End Sub
