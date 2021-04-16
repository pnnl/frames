Attribute VB_Name = "Common"
Option Explicit
Option Compare Text

Public Type STARTUPINFO
  cb As Long
  lpReserved As String
  lpDesktop As String
  lpTitle As String
  dwX As Long
  dwY As Long
  dwXSize As Long
  dwYSize As Long
  dwXCountChars As Long
  dwYCountChars As Long
  dwFillAttribute As Long
  dwFlags As Long
  wShowWindow As Integer
  cbReserved2 As Integer
  lpReserved2 As Long
  hStdInput As Long
  hStdOutput As Long
  hStdError As Long
End Type

Type PROCESS_INFORMATION
  hProcess As Long
  hThread As Long
  dwProcessId As Long
  dwThreadId As Long
End Type

Global FRAMES_INI As String
Global DesName As String        'the path and Name of the run
Global FUIName As String        'the path and Name of the run
Global RunName As String        'the Name of the parameter file
Global siteIdx As Long          'the index to site
Global modIdx As Long           'the index to module
Global modName As String        'the Name of the module glyph
Global called As Boolean
Global frm As Object
Global frmRef As Object

Global HelpFileName As String   'the Name of the reference file
Global HelpAvailable As Boolean 'does the help file exist
Global HelpAnchor As String     'the Name of the help anchor
 
Global RefFileName As String    'the Name of the reference file
Global RefAvailable As Boolean  'does the reference file exist
Global RefMode As Long          'if 0 then select a reference else add a reference
Global RefIdx As Long           'the current selected reference
Global RefItem As Integer       'the current selected item index

Global errfile As csv           'error file declaration
Global AnError As Boolean       'error flag to tell when to keep error file
Global argv() As String         'command line argument array
Global argc As Long             'command line argument count
Global saving As Boolean

Global des As csv                 'des file declaration
   
Public CopyRows As Integer                              'Used for cut,copy,paste
Public CopyCols As Integer                              'Used for cut,copy,paste
Public BeginCol, EndCol, BeginRow, EndRow As Integer    'Used for cut,copy,paste

Private Const MAX_PATH As Long = 260
Private Const ERROR_FILE_NO_ASSOCIATION As Long = 31
Private Const ERROR_FILE_NOT_FOUND As Long = 2
Private Const ERROR_PATH_NOT_FOUND As Long = 3
Private Const ERROR_FILE_SUCCESS As Long = 32 'my constant
Private Const ERROR_BAD_FORMAT As Long = 11

Private Const NORMAL_PRIORITY_CLASS = &H20&
Private Const INFINITE = -1&

Global Const lightGreen = &HC0FFC0
Global Const lightRed = &HC0C0FF
Global Const lightYellow = &HC0FFFF
Global Const lightGray = &HC0C0C0

Declare Function GetModuleFileName Lib "kernel32" Alias "GetModuleFileNameA" (ByVal hModule As Long, ByVal lpFileName As String, ByVal nSize As Long) As Long
Declare Function GetPrivateProfileString Lib "kernel32" Alias "GetPrivateProfileStringA" (ByVal lpApplicationName As String, ByVal lpKeyName As Any, ByVal lpDefault As String, ByVal lpReturnedString As String, ByVal nSize As Long, ByVal lpFileName As String) As Long
Declare Function WritePrivateProfileString Lib "kernel32" Alias "WritePrivateProfileStringA" (ByVal lpApplicationName As String, ByVal lpKeyName As Any, ByVal lpString As Any, ByVal lpFileName As String) As Long
Declare Function FindExecutable Lib "shell32" Alias "FindExecutableA" (ByVal lpFile As String, ByVal lpDirectory As String, ByVal sResult As String) As Long
Declare Function ShellExecute Lib "shell32.dll" Alias "ShellExecuteA" (ByVal hWnd As Long, ByVal lpOperation As String, ByVal lpFile As String, ByVal lpParameters As String, ByVal lpDirectory As String, ByVal nShowCmd As Long) As Long
Declare Function SendMessage Lib "user32" Alias "SendMessageA" (ByVal hWnd As Long, ByVal wMsg As Long, ByVal wParam As Long, lParam As Any) As Long
Declare Function WaitForSingleObject Lib "kernel32" (ByVal hHandle As Long, ByVal dwMilliseconds As Long) As Long
Declare Function CreateProcess Lib "kernel32" Alias "CreateProcessA" (ByVal lpApplicationName As String, ByVal lpCommandLine As String, lpProcessAttributes As Any, lpThreadAttributes As Any, ByVal bInheritHandles As Long, ByVal dwCreationFlags As Long, lpEnvironment As Any, ByVal lpCurrentDriectory As String, lpStartupInfo As STARTUPINFO, lpProcessInformation As PROCESS_INFORMATION) As Long
Declare Function CloseHandle Lib "kernel32" (ByVal hObject As Long) As Long

Sub SetForm(ModForm As Object)
  Set frm = ModForm
End Sub

Public Sub ExecCmd(cmdline As String)
'Execute another program and waits till other program is finished
  Dim ret As Long
  Dim sNull As String
  Dim proc As PROCESS_INFORMATION
  Dim start As STARTUPINFO

  start.cb = Len(start)
  ret = CreateProcess(sNull, cmdline, ByVal 0&, ByVal 0&, 1&, NORMAL_PRIORITY_CLASS, ByVal 0&, sNull, start, proc)
  ret = WaitForSingleObject(proc.hProcess, INFINITE)
  ret = CloseHandle(proc.hProcess)
End Sub

Sub GetArguments()
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

Sub StartModule(ModForm As Object, Title As String, argcnt As Long)
  called = False
  GetArguments
  If argc < argcnt Then
    MsgBox "Invalid arguments passed to module" & Chr$(10) & Command & Chr$(10) & "Contact PNNL", 16, "Usage error!"
    End
  End If
  FUIName = argv(argc - 5) & ".gid"
  RunName = argv(argc - 4)
  siteIdx = val(argv(argc - 3))
  modIdx = val(argv(argc - 2))
  modName = argv(argc - 1)

  If siteIdx + modIdx < 2 Then
    MsgBox "Invalid arguments passed to module" & Chr$(10) & Command & Chr$(10) & "Contact PNNL", 16, "Usage error!"
    End
  End If
  If Not open_csv(errfile, RunName & ".ERR", 1) Then
    MsgBox "Unable to create file " & RunName & ".ERR" & Chr$(10) & "Check directory permissions", 16, "File IO error!"
    End
  End If
  AnError = False
#If NOCONVERT Then
#Else
  load_convert
#End If
  If Not ModForm Is Nothing Then
    ModForm.Caption = Title + " - " + modName
    ModForm.ZOrder
  End If
  Set frm = ModForm
  Set frmRef = Nothing
  'ModForm.Show
End Sub

Sub EndModule()
  If Not called Then
    called = True
    close_csv errfile
    Close 'all files
    If Not AnError Then Kill RunName & ".ERR"
    If Not frm Is Nothing Then Unload frm
    End
  End If
End Sub

Sub PutError(myError As String)
  put_val errfile, myError
  put_line errfile
  AnError = True
End Sub

Sub PutHeader(file As csv)
    put_val file, 8
    put_line file
    put_val file, "=================================================="
    put_line file
    put_val file, "  " & App.Title
    put_line file
    put_val file, "  Version " & App.Major & "." & App.Minor
    put_line file
    put_val file, "  Site Index: " & siteIdx
    put_line file
    put_val file, "  Module Index: " & modIdx
    put_line file
    put_val file, "  Module Name: " & modName
    put_line file
    put_val file, "  Created:  " & Now
    put_line file
'    put_val file, "  Created Time: " & Time$
'    put_line file
    put_val file, "=================================================="
    put_line file
End Sub

Public Sub SetRefFile(fileName As String)
Dim temp As String
  RefFileName = fileName
  temp = Dir$(RefFileName)
  If Len(temp) > 0 Then
    RefAvailable = True
  Else
    RefAvailable = False
  End If
End Sub

Public Sub GetRef(RefLabel As Object)
  If RefItem = -1 Then Exit Sub
#If NOREFERENCE Then
  Exit Sub
#Else
  RefIdx = RefLabel.tag
  Reference.Show 1
  If RefIdx <> -1 Then
    RefLabel.tag = RefIdx
    RefLabel.Caption = "Ref:" & str(RefIdx)
  End If
#End If
End Sub

Sub SetHelpFile(fileName As String)
Dim temp As String

  HelpFileName = fileName
  temp = Dir$(HelpFileName)
  If Len(temp) > 0 Then
    HelpAvailable = True
    frm.howto.Enabled = True
  Else
    HelpAvailable = False
    On Error Resume Next ' reference to specific control in general purpose module
    frm.howto.Enabled = False
    On Error GoTo 0
  End If
End Sub

Public Function ConvertURL(URLSTRING As String) As String
    Dim i As Integer
    Dim tempchar As String
    Dim rstring As String
    rstring = ""
    For i = 1 To Len(URLSTRING)
        tempchar = Mid$(URLSTRING, i, 1)
        Select Case tempchar
        Case "\":            rstring = rstring + "/"
        Case " ":            rstring = rstring + "%20"
        Case Else:           rstring = rstring + tempchar
        End Select
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

    ' lpFile: Name of the file of interest
    ' lpDirectory: location of lpFile
    ' sResult: path and Name of executable associated with lpFile
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

Public Sub ClearSpread(ss As Object)
  ss.row = -1
  ss.col = -1
  ss.BlockMode = True:
  ss.Action = 12 ' clear text
  ss.BlockMode = False
End Sub

Public Sub SetSpreadUnits(ss As Object, col As Long, unit As String)
  Dim lst As String
  lst = ""
  ss.row = 1
  ss.col = col
  ss.Action = 26                     'clear the box
#If NOCONVERT Then
  lst = unit
#Else
  get_conversion_list unit, lst
#End If
  ss.TypeComboBoxIndex = -1
  ss.TypeComboBoxList = lst
  ss.TypeComboBoxCurSel = 0
End Sub

Public Sub vaSpreadCopy()              'spread control text copy
    Clipboard.Clear
    'Store size of copied data
    If BeginCol > EndCol Then CopyCols = (BeginCol - EndCol) + 1
    If EndCol > BeginCol Then CopyCols = (EndCol - BeginCol) + 1
    If BeginCol = EndCol Then CopyCols = 1
    If BeginRow > EndRow Then CopyRows = (BeginRow - EndRow) + 1
    If EndRow > BeginRow Then CopyRows = (EndRow - BeginRow) + 1
    If BeginRow = EndRow Then CopyRows = 1
    
    SendKeys "^C", True
End Sub

Public Sub vaSpreadCut()               'spread control text cut
    Clipboard.Clear
    'Store size of cut data
    If BeginCol > EndCol Then CopyCols = (BeginCol - EndCol) + 1
    If EndCol > BeginCol Then CopyCols = (EndCol - BeginCol) + 1
    If BeginCol = EndCol Then CopyCols = 1
    If BeginRow > EndRow Then CopyRows = (BeginRow - EndRow) + 1
    If EndRow > BeginRow Then CopyRows = (EndRow - BeginRow) + 1
    If BeginRow = EndRow Then CopyRows = 1
    
    SendKeys "^X", True
End Sub

Public Sub vaSpreadPaste()              'spread control text paste
    Dim result As Integer
    Dim CopyCols2 As Integer
    Dim CopyRows2 As Integer
    
    'Single cell selected
    If ((BeginCol = EndCol) And (BeginRow = EndRow)) Then
        result = MsgBox("Existing data may be overwritten.  Continue?", vbYesNo)
        If Not result = vbYes Then Exit Sub
        GoTo GoPaste
    End If
    
    'Pasting area selected, but pre-existing clipboard data
    If CopyRows = 0 Or CopyCols = 0 Then
        MsgBox ("Clipboard size unknown." + vbCrLf + "Select single cell starting point." _
            + vbCrLf + "Paste cancelled.")
        Exit Sub
    End If
    
    'Check paste size selected
    If BeginCol > EndCol Then CopyCols2 = (BeginCol - EndCol) + 1
    If EndCol > BeginCol Then CopyCols2 = (EndCol - BeginCol) + 1
    If BeginCol = EndCol Then CopyCols2 = 1
    If BeginRow > EndRow Then CopyRows2 = (BeginRow - EndRow) + 1
    If EndRow > BeginRow Then CopyRows2 = (EndRow - BeginRow) + 1
    If BeginRow = EndRow Then CopyRows2 = 1

    'Compare paste size selected to clipboard size
    If (CopyCols <> CopyCols2) Or (CopyRows <> CopyRows2) Then
        MsgBox ("Clipboard size differs from paste size selected." + vbCrLf + "Select " + _
            str(CopyCols) + " columns and " + str(CopyRows) + " rows, or single cell." + _
            vbCrLf + "Paste cancelled.")
        Exit Sub
    Else
        GoTo GoPaste
    End If
    Exit Sub
GoPaste:    SendKeys "^V", True
End Sub

'
'Function opendes(fName$)
'  opendes = open_csv(des, fName$, 1)
'End Function
'
'Sub closedes()
'  close_csv des
'End Sub
'
'Sub putfile(typ%, mode%)
'  Select Case typ
'    Case 1: put_val des, "SCF"
'    Case 2: put_val des, "WFF"
'    Case 3: put_val des, "WCF"
'    Case 4: put_val des, "AFF"
'    Case 5: put_val des, "ATO"
'    Case 6: put_val des, "med"
'    Case 7: put_val des, "RIF"
'    Case 8: put_val des, "HIF"
'  End Select
'  Select Case mode
'    Case 1: put_val des, "READ"
'    Case 2: put_val des, "WRITE"
'    Case 3: put_val des, "READ/WRITE"
'  End Select
'  put_line des
'End Sub
'
'Sub putmeta(Name$, typ$, unit$, descript$, cnt%, Optional min, Optional max)
'  put_val des, Name$
'  put_val des, typ$
'  put_val des, unit$
'  If IsMissing(min) Then
'    put_val des, ""
'  Else
'    put_val des, "MIN"
'    put_val des, min
'  End If
'  If IsMissing(max) Then
'    put_val des, ""
'  Else
'    put_val des, "MAX"
'    put_val des, max
'  End If
'  put_val des, descript$
'  put_val des, cnt%
'  put_line des
'End Sub
'
'Sub putvariable(Name$, i1$, Optional i2, Optional i3, Optional i4, Optional i5, Optional i6)
'  put_val des, "Variable"
'  put_val des, Name$
'  put_val des, i1
'  If Not IsMissing(i2) Then put_val des, i2
'  If Not IsMissing(i3) Then put_val des, i3
'  If Not IsMissing(i4) Then put_val des, i4
'  If Not IsMissing(i5) Then put_val des, i5
'  If Not IsMissing(i6) Then put_val des, i6
'  put_line des
'End Sub
'
'Sub putlabel(Name$, i1$, Optional i2, Optional i3, Optional i4, Optional i5, Optional i6)
'  put_val des, "Label"
'  put_val des, Name$
'  put_val des, i1
'  If Not IsMissing(i2) Then put_val des, i2
'  If Not IsMissing(i3) Then put_val des, i3
'  If Not IsMissing(i4) Then put_val des, i4
'  If Not IsMissing(i5) Then put_val des, i5
'  If Not IsMissing(i6) Then put_val des, i6
'  put_line des
'End Sub
'
