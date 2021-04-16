Attribute VB_Name = "Common"
Option Explicit
Option Compare Text

Public Declare Function LoadLibrary Lib "kernel32" Alias "LoadLibraryA" (ByVal lpLibFileName As String) As Long
Public Declare Function FreeLibrary Lib "kernel32" (ByVal hLibModule As Long) As Long
Public Declare Function SetWindowPos Lib "user32" (ByVal hWnd As Long, ByVal hWndInsertAfter As Long, ByVal x As Long, ByVal y As Long, ByVal cx As Long, ByVal cy As Long, ByVal wFlags As Long) As Long
Public Declare Function GetModuleFileName Lib "kernel32" Alias "GetModuleFileNameA" (ByVal hModule As Long, ByVal lpFileName As String, ByVal nSize As Long) As Long
Public Declare Function GetCursorPos Lib "user32" (lpPoint As POINTAPI) As Long
Public Declare Function SetCursorPos Lib "user32" (ByVal x As Long, ByVal y As Long) As Long
Public Declare Function FindFirstFile Lib "kernel32" Alias "FindFirstFileA" (ByVal lpFileName As String, lpFindFileData As WIN32_FIND_DATA) As Long
Public Declare Function FindNextFile Lib "kernel32" Alias "FindNextFileA" (ByVal hFindFile As Long, lpFindFileData As WIN32_FIND_DATA) As Long
Public Declare Function FindClose Lib "kernel32" (ByVal hFindFile As Long) As Long
Public Declare Function BitBlt Lib "gdi32" (ByVal hDestDC As Long, ByVal x As Long, ByVal y As Long, ByVal nWidth As Long, ByVal nHeight As Long, ByVal hSrcDC As Long, ByVal XSrc As Long, ByVal YSrc As Long, ByVal dwRop As Long) As Long
Public Declare Function SetBkColor Lib "gdi32" (ByVal hdc As Long, ByVal crColor As Long) As Long
Public Declare Function CreateCompatibleDC Lib "gdi32" (ByVal hdc As Long) As Long
Public Declare Function DeleteDC Lib "gdi32" (ByVal hdc As Long) As Long
Public Declare Function CreateBitmap Lib "gdi32" (ByVal nWidth As Long, ByVal nHeight As Long, ByVal nPlanes As Long, ByVal nBitCount As Long, ByVal lpBits As Any) As Long
Public Declare Function CreateCompatibleBitmap Lib "gdi32" (ByVal hdc As Long, ByVal nWidth As Long, ByVal nHeight As Long) As Long
Public Declare Function FloodFill Lib "gdi32" (ByVal hdc As Long, ByVal x As Long, ByVal y As Long, ByVal crColor As Long) As Long
Public Declare Function GetTempFileName Lib "kernel32" Alias "GetTempFileNameA" (ByVal lpszPath As String, ByVal lpPrefixString As String, ByVal wUnique As Long, ByVal lpTempFileName As String) As Long
Public Declare Function SendMessage Lib "user32" Alias "SendMessageA" (ByVal hWnd As Long, ByVal wMsg As Long, ByVal wParam As Long, lParam As Any) As Long
Public Declare Function SendMessageByNum Lib "user32" Alias "SendMessageA" (ByVal hWnd As Long, ByVal wMsg As Long, ByVal wParam As Long, ByVal lParam As Long) As Long

Public PID As Long              'the process id for all systemio calls
Public SimPath As String        'the path to the simulation files
Public SimName As String        'the name of the simulation
Public modid As String          'the name of the module icon
Public argv() As String         'command line argument array
Public argc As Long             'command line argument count

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

Type POINTAPI
  x As Long
  y As Long
End Type

Public hLibModule As Long

Public Sub GetCommandLine(Optional MaxArgs)
 
   Dim quote As String:   quote = Chr$(34)
   
   'Declare variables.
   Dim C, cmdline, CmdLnLen, InArg, i, quoted As Boolean
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

Sub EndModule()
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


