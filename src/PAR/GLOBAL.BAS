Attribute VB_Name = "Modulo3"
Public Const KPS_NONE = 1
Public Const KPS_MOVE = 2
Public Const KPS_DELETE = 3
Public Const KPS_DATA = 4
Public Const KPS_DBGR = 5
Public Const KPS_INSLABEL = 6
Public Const KPS_DELLABEL = 7
Public Const KPS_PROP = 8

Public Const KPS_END = 1
Public Const KPS_CONTINUE = 2
Public Const KPS_UNDOALL = 3
Public Const KPS_UNDOLAST = 4
Public Const KPS_ISLE = 5
Public Const KPS_HOLE = 6
Public Const KPS_NEWPOINT = 7

Public Const KPS_MOVEALL = 11
Public Const KPS_MOVEVRTX = 12
Public Const KPS_ADDVRTX = 13
Public Const KPS_DELVRTX = 14

Public Const IC_INSERTED = 1
Public Const IC_PRESENT = 2

Public Const OPEN_POLYLINE = 1
Public Const CLOSED_POLYLINE = 2
Public Const POLYGON = 3

Public Const SW_SHOWDEFAULT = 10
Public Const HELP_CONTEXT = &H1

Public Const SUCCESS = 0

Public Const FILE_ATTRIBUTE_DIRECTORY As Long = &H10
Public Const INVALID_HANDLE_VALUE As Long = -1
Public Const MAX_STRING As Long = 4096
Public Const MAX_PATH As Long = 260

Public Const LB_SETHORIZONTALEXTENT = &H194

Public Const SB_LINELEFT = 0
Public Const SB_PAGELEFT = 2
Public Const SM_TWIPS = 1
Public Const SM_PIXEL = 3

Public Const MAXFIELD = 4096
Public Const SP_DRIVE = 0
Public Const SP_DIR = 1
Public Const SP_DIRR = 4
Public Const SP_TITLE = 2
Public Const SP_EXT = 3

Private Type FILETIME
  dwLowDateTime As Long
  dwHighDateTime As Long
End Type

Private Type WIN32_FIND_DATA
  dwFileAttributes As Long
  ftCreationTime As FILETIME
  ftLastAccessTime As FILETIME
  ftLastWriteTime As FILETIME
  nFileSizeHigh As Long
  nFileSizeLow As Long
  dwReserved0 As Long
  dwReserved1 As Long
  cFileName As String * MAX_PATH
  cAlternate As String * 14
End Type

Type RECT
  Left As Long
  Top As Long
  Right As Long
  Bottom As Long
End Type

Type POINTAPI
  x As Long
  y As Long
End Type

Type LAYERSTRUCT
  name As String
  dwg As Boolean
  bkgd As Boolean
  visible As Boolean
  pickable As Boolean
  objtype As String
  color As Long
  linetype As Long
  lcolor As Integer
  fill As Long
  filename As String
  extminX As Double
  extminY As Double
  extmaxX As Double
  extmaxY As Double
  workarea As Long ' for devSelect()
  opened As Boolean
End Type

Public Type tindex
  i1 As Long
  i2 As Long
  i3 As Long
  i4 As Long
  i5 As Long
  i6 As Long
  i7 As Long
  i8 As Long
  i9 As Long
  i10 As Long
  i11 As Long
  i12 As Long
End Type

Declare Function WinHelp Lib "user32" Alias "WinHelpA" (ByVal hwnd As Long, ByVal lpHelpFile As String, ByVal wCommand As Long, ByVal dwData As Long) As Long
Declare Function GetCursorPos Lib "user32" (lpPoint As POINTAPI) As Long
Declare Function GetMenu Lib "user32" (ByVal hwnd As Long) As Long
Declare Function GetSubMenu Lib "user32" (ByVal hMenu As Long, ByVal nPos As Long) As Long
Declare Function TrackPopupMenu Lib "user32" (ByVal hMenu As Long, ByVal wFlags As Long, ByVal x As Long, ByVal y As Long, ByVal nReserved As Long, ByVal hwnd As Long, lprc As RECT) As Long
Declare Function ShellExecute Lib "shell32.dll" Alias "ShellExecuteA" (ByVal hwnd As Long, ByVal lpOperation As String, ByVal lpFile As String, ByVal lpParameters As String, ByVal lpDirectory As String, ByVal nShowCmd As Long) As Long
Declare Function GetTempFileName Lib "kernel32" Alias "GetTempFileNameA" (ByVal lpszPath As String, ByVal lpPrefixString As String, ByVal wUnique As Long, ByVal lpTempFileName As String) As Long
Declare Function GetModuleFileName Lib "kernel32" Alias "GetModuleFileNameA" (ByVal hModule As Long, ByVal lpFileName As String, ByVal nSize As Long) As Long
Declare Function LoadLibrary Lib "kernel32" Alias "LoadLibraryA" (ByVal lpLibFileName As String) As Long
Declare Function FreeLibrary Lib "kernel32" (ByVal hLibModule As Long) As Long

Public hLibModule As Long
Public PID As Long              'the process id for all systemio calls
Public SimPath As String        'the path to the simulation files
Public SimName As String        'the name of the simulation
Public modid As String          'the name of the module icon
Public argv() As String         'command line argument array
Public argc As Long             'command line argument count

Public DBFileTitle As String
Public DBFileDir As String
Public DBTableName As String

Public retStr As String * MAXFIELD

Function ReplaceExt(name As String, ext As String) As String
  ReplaceExt = SplitPath(name, SP_DIR) & SplitPath(name, SP_TITLE) & "." & ext
End Function

Function SplitPath(path As String, item As Integer) As String
Dim i As Integer
Dim dot As Integer
Dim slash As Integer
Dim colon As Integer
Dim colonnotfound As Boolean
Dim slashnotfound As Boolean
Dim dotnotfound As Boolean

  SplitPath = ""
  If path = "" Then Exit Function
  dotnotfound = True
  slashnotfound = True
  colonnotfound = True
  i = Len(path) + 1
  dot = i
  slash = 0
  colon = 0
  'start form the end and find first dot
  While i > 0 Or (slashnotfound And dotnotfound)
    If Mid(path, i, 1) = "." And dotnotfound And slashnotfound Then
      dot = i
      dotnotfound = False
    ElseIf Mid(path, i, 1) = "\" And slashnotfound Then
      slash = i
      slashnotfound = False
    ElseIf Mid(path, i, 1) = ":" And colonnotfound Then
      colon = i
      colonnotfound = False
    End If
    i = i - 1
  Wend
  
  Select Case item
    Case SP_DRIVE:  SplitPath = Left(path, colon)                             'drive up to slash (c: | "")
    Case SP_DIR:    SplitPath = Left(path, slash)                             'drive to slash inclusive (c:\dir1\ | \dir1\dir2\ | dir\ | "")
    Case SP_DIRR:   SplitPath = Left(path, slash - 1)                         'drive to slash exclusive (c:\dir1 | \dir1\dir2 | dir | "")
    Case SP_TITLE:  SplitPath = Mid(path, slash + 1, dot - slash - 1)         'slash to dot exclusive (.my.firstfile | myfirstfile | "")
    Case SP_EXT:    SplitPath = Right(path, Len(path) - dot + 1)              'dot to end inclusive (.txt | .myextension | "")
  End Select
End Function

Function StripTerminator(ByVal strString As String) As String
  Dim intZeroPos As Integer
  intZeroPos = InStr(strString, Chr$(0))
  If intZeroPos > 0 Then
    StripTerminator = Left$(strString, intZeroPos - 1)
  Else
    StripTerminator = strString
  End If
End Function

Public Function SetIdx(Optional i1 As Long = 0, Optional i2 As Long = 0, _
                        Optional i3 As Long = 0, Optional i4 As Long = 0, _
                        Optional i5 As Long = 0, Optional i6 As Long = 0, _
                        Optional i7 As Long = 0, Optional i8 As Long = 0, _
                        Optional i9 As Long = 0, Optional i10 As Long = 0, _
                        Optional i11 As Long = 0, Optional i12 As Long = 0) As tindex
  Dim index As tindex
  index.i1 = i1
  index.i2 = i2
  index.i3 = i3
  index.i4 = i4
  index.i5 = i5
  index.i6 = i6
  index.i7 = i7
  index.i8 = i8
  index.i9 = i9
  index.i10 = i10
  index.i11 = i11
  index.i12 = i12
  SetIdx = index
End Function

Public Sub GetCommandLine(Optional MaxArgs)
   Dim quote As String
   Dim C, cmdline, CmdLnLen, InArg, i, quoted As Boolean
   
   quote = Chr$(34)
   'Declare variables.
   'See if MaxArgs was provided.
   If IsMissing(MaxArgs) Then MaxArgs = 10
   'Make array of the correct size.
   ReDim argv(MaxArgs)
   argc = 0: InArg = False
   'Get command line arguments.
   cmdline = Command()
   CmdLnLen = Len(cmdline)
   'Go thru command line one character
   'at a time.
   For i = 1 To CmdLnLen
      C = Mid(cmdline, i, 1)
      'Test for space or tab.
      If (C <> " " And C <> vbTab) Or quoted Then
         'Neither space nor tab.
         'Test if already in argument.
         If Not InArg Then
         'New argument begins.
         'Test for too many arguments.
            If argc = MaxArgs Then Exit For
            argc = argc + 1
            InArg = True
         End If
         'Concatenate character to current argument.
         argv(argc) = argv(argc) & C
         If C = quote Then
           If Right$(argv(argc), 1) = quote Then
            quoted = IIf(quoted, False, True)
           End If
         End If
      Else
         'Found a space or tab.
         'Set InArg flag to False.
         InArg = False
      End If
   Next i
   'Resize array just enough to hold arguments.
   ReDim Preserve argv(argc)
End Sub

Public Sub StartModule(argcnt As Long)
  GetCommandLine
  If argc < argcnt Then
    MsgBox "Invalid arguments passed to module" & Chr(10) & Command & Chr(10) & "Contact PNNL", 16, "Usage error!"
    End
  End If
  modid = argv(argc)
  SimName = argv(argc - 1)
  SimPath = argv(argc - 2)
End Sub

Public Function UnQualifyPath(ByVal sFolder As String) As String

  'trim and remove any trailing slash
   sFolder = Trim$(sFolder)
   
   If Right$(sFolder, 1) = "\" Then
     UnQualifyPath = Left$(sFolder, Len(sFolder) - 1)
   Else
     UnQualifyPath = sFolder
   End If
   
End Function

Public Function FolderExists(sFolder As String) As Boolean
   Dim hFile As Long
   Dim WFD As WIN32_FIND_DATA
   
  'remove trailing slash before verifying
   sFolder = UnQualifyPath(sFolder)

  'call the API passing the folder
   hFile = FindFirstFile(sFolder, WFD)
   
  'if a valid file handle was returned, and the directory attribute is set the folder exists
   FolderExists = (hFile <> INVALID_HANDLE_VALUE) And _
                  (WFD.dwFileAttributes And FILE_ATTRIBUTE_DIRECTORY)
   
  'clean up
   Call FindClose(hFile)
   
End Function

Public Function DisplayError(ByVal code As Long, ByVal API_CALL As String, Optional ByVal display As Boolean = False) As Long
Dim errstr As String
Dim errmsg As String

  DisplayError = code
  If code < 0 Then
  '  If code = -1010 Or code = -1001 Then Exit Function
    ReadError PID, code, errstr
'    errstr = Replace(errstr, vbTab, "")
    errmsg = "API Call : " + API_CALL + vbCrLf
    errmsg = errmsg + "Error Code : " + CStr(code) + vbCrLf
    errmsg = errmsg + "Error Description : " + errstr
    Debug.Print errmsg
'    If display Then MsgBox errmsg, vbExclamation + vbOKOnly, "Import DES File Error"
    If display Then AddText errmsg
  Else
  End If
End Function

Function InterpX(xValues() As Double, yValues() As Double, count As Long, y As Double, idx As Long) As Double    'InterpTime

  Dim interval As Double

  If (count = 0) Then
    InterpX = 0#
  Else
    interval = yValues(idx + 1) - yValues(idx)
    If (fabs(interval) > 0#) Then
      InterpX = xValues(idx) + (y - yValues(idx)) * (xValues(idx + 1) - xValues(idx)) / interval
    Else
      InterpX = xValues(idx)
    End If
  End If
End Function


Function ExceedDeltaIdx(xValues() As Double, yValues() As Double, count As Long, y As Double, idx As Long) As Double

  ExceedDelta = 0#
  If (idx > count - 2 Or idx < 0) Then                               ' no segments
    ExceedDelta = 0#
  ElseIf (xValues(idx) = xValues(idx + 1)) Then                      ' vertical segment
    ExceedDelta = 0#
  ElseIf (yValues(idx) >= y And yValues(idx + 1) >= y) Then          ' segment above y
    ExceedDelta = xValues(idx + 1) - xValues(idx)
  ElseIf (xValues(idx) < y And yValues(idx + 1) < y) Then            ' segment below y
    ExceedDelta = 0#
  ElseIf (yValues(idx) <= y And y < yValues(idx + 1)) Then           ' y intersects segment
      ExceedDelta = xValues(idx + 1) - InterpX(xValues, yValues, count, y, idx)            ' in ascending segment
  ElseIf (yValues(idx) > y And y >= yValues(idx + 1)) Then           ' y intersects segment
      ExceedDelta = InterpX(xValues, yValues, count, y, idx) - xValues(idx)                  ' in descending segment
  End If
End Function

Function ExceedDelta(xValues() As Double, yValues() As Double, count As Long, y As Double, begIdx As Long, endIdx As Long) As Double

  Dim n As Long
  Dim deltaX As dobule
  
  deltaX = 0#
  If (endIdx > count - 1 Or begIdx < 0) Then
    ExceedDelta = 0#
  Else
    For n = begIdx To endIdx - 1
      deltaX = deltaX + ExceedDeltaIdx(xValues, yValues, count, y, n)
    Next
  End If
  ExceedDelta = deltaX
End Function


