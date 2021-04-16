Attribute VB_Name = "WinAPI"
Option Explicit

' All winAPI calls and type used by FRAMES User Interface

'Const used by setwinpos functions
Public Const HWND_BOTTOM = 1
Public Const HWND_NOTOPMOST = -2
Public Const HWND_TOP = 0
Public Const HWND_TOPMOST = -1
Public Const SWP_FRAMECHANGED = &H20        '  The frame changed: send WM_NCCALCSIZE
Public Const SWP_HIDEWINDOW = &H80
Public Const SWP_NOACTIVATE = &H10
Public Const SWP_NOCOPYBITS = &H100
Public Const SWP_NOMOVE = &H2
Public Const SWP_NOOWNERZORDER = &H200      '  Don't do owner Z ordering
Public Const SWP_NOREDRAW = &H8
Public Const SWP_NOREPOSITION = SWP_NOOWNERZORDER
Public Const SWP_NOSIZE = &H1
Public Const SWP_NOZORDER = &H4
Public Const SWP_SHOWWINDOW = &H40

Public Const SWP_HIDE = SWP_HIDEWINDOW Or SWP_NOMOVE Or SWP_NOSIZE Or SWP_NOZORDER
Public Const SWP_SHOW = SWP_SHOWWINDOW Or SWP_NOMOVE Or SWP_NOSIZE Or SWP_NOZORDER


'Const used by *GDI functions
Public Const SRCCOPY = &HCC0020         ' (DWORD) dest = source
Public Const SRCAND = &H8800C6          ' (DWORD) dest = source AND dest
Public Const SRCPAINT = &HEE0086        ' (DWORD) dest = source OR dest
Public Const NOTSRCCOPY = &H330008      ' (DWORD) dest = (NOT source)
Public Const WHITENESS = &HFF0062       ' (DWORD) dest = WHITE
Public Const BLACKNESS = &H42&          ' (DWORD) dest = BLACK

'Const values used with SendMessage
Public Const EM_GETLINE = &HC4
Public Const EM_LINEFROMCHAR = &HC9
Public Const EM_CHARFROMPOS = &H427
Public Const EM_LINEINDEX = &HBB

'Const used by *Process functions
Public Const INFINITE = &HFFFF             '  Infinite timeout
Public Const WAIT_FAILED = &HFFFF          '  Wait failed
Public Const NORMAL_PRIORITY_CLASS = &H20

' Const used by GetSystemMetrics
Public Const SM_CXSCREEN = 0        ' Width of screen
Public Const SM_CYSCREEN = 1        ' Height of screen
Public Const SM_CXFULLSCREEN = 16   ' Width of window client area
Public Const SM_CYFULLSCREEN = 17   ' Height of window client area
Public Const SM_CYMENU = 15         ' Height of menu
Public Const SM_CYCAPTION = 4       ' Height of caption or title
Public Const SM_CXFRAME = 32        ' Width of window frame
Public Const SM_CYFRAME = 33        ' Height of window frame
Public Const SM_CXHSCROLL = 21      ' Width of arrow bitmap on horizontal scroll bar
Public Const SM_CYHSCROLL = 3       ' Height of arrow bitmap on horizontal scroll bar
Public Const SM_CXVSCROLL = 2       ' Width of arrow bitmap on vertical scroll bar
Public Const SM_CYVSCROLL = 20      ' Height of arrow bitmap on vertical scroll bar
Public Const SM_CXSIZE = 30         ' Width of bitmaps in title bar
Public Const SM_CYSIZE = 31         ' Height of bitmaps in title bar
Public Const SM_CXCURSOR = 13       ' Width of cursor
Public Const SM_CYCURSOR = 14       ' Height of cursor
Public Const SM_CXBORDER = 5        ' Width of window frame that cannot be sized
Public Const SM_CYBORDER = 6        ' Height of window frame that cannot be sized
Public Const SM_CXDOUBLECLICK = 36  ' Width of rectangle around the location of the first click.
                                    ' The second click must occur in the same rectangular location.
Public Const SM_CYDOUBLECLICK = 37  ' Height of rectangle around the location of the first click.
                                    ' The second click must occur in the same rectangular location.
Public Const SM_CXDLGFRAME = 7      ' Width of dialog frame window
Public Const SM_CYDLGFRAME = 8      ' Height of dialog frame window
Public Const SM_CXICON = 11         ' Width of icon
Public Const SM_CYICON = 12         ' Height of icon
Public Const SM_CXICONSPACING = 38  ' Width of rectangles the system uses to position tiled icons
Public Const SM_CYICONSPACING = 39  ' Height of rectangles the system uses to position tiled icons
Public Const SM_CXMIN = 28          ' Minimum width of window
Public Const SM_CYMIN = 29          ' Minimum height of window
Public Const SM_CXMINTRACK = 34     ' Minimum tracking width of window
Public Const SM_CYMINTRACK = 35     ' Minimum tracking height of window
Public Const SM_CXHTHUMB = 10       ' Width of scroll box (thumb) on horizontal scroll bar
Public Const SM_CYVTHUMB = 9        ' Width of scroll box (thumb) on vertical scroll bar
Public Const SM_DBCSENABLED = 42    ' Returns a non-zero if the current Windows version uses double-byte
                                    '  characters, otherwise returns zero
Public Const SM_DEBUG = 22          ' Returns non-zero if the Windows version is a debugging version
Public Const SM_MENUDROPALIGNMENT = 40   ' Alignment of pop-up menus. If zero, left side is aligned with
                             '  corresponding Left$ side of menu-bar item. If non-zero, Left$ side
                             '  is aligned with Right$ side of corresponding menu bar item
Public Const SM_MOUSEPRESENT = 19   ' Non-zero if mouse hardware is  installed
Public Const SM_PENWINDOWS = 41     ' Handle of Pen Windows dynamic link library if Pen Windows is installed
Public Const SM_SWAPBUTTON = 23     ' Non-zero if the left and right mouse buttons are swapped


Public Type RECT
  Left As Long
  Top As Long
  Right As Long
  Bottom As Long
End Type

Type BITMAP '14 bytes
  bmType As Long
  bmWidth As Long
  bmHeight As Long
  bmWidthBytes As Long
  bmPlanes As String * 1
  bmBitsPixel As String * 1
  bmBits As Long
End Type

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

Public Type PROCESS_INFORMATION
  hProcess As Long
  hThread As Long
  dwProcessId As Long
  dwThreadId As Long
End Type

'API declaration
Public Declare Function GetShortPathName Lib "kernel32" Alias "GetShortPathNameA" (ByVal lpszLongPath As String, ByVal lpszShortPath As String, ByVal cchBuffer As Long) As Long
Public Declare Function FloodFill Lib "gdi32" (ByVal hdc As Long, ByVal x As Long, ByVal y As Long, ByVal crColor As Long) As Long
Public Declare Function BitBlt Lib "gdi32" (ByVal hDestDC As Long, ByVal x As Long, ByVal y As Long, ByVal nWidth As Long, ByVal nHeight As Long, ByVal hSrcDC As Long, ByVal XSrc As Long, ByVal YSrc As Long, ByVal dwRop As Long) As Long
Public Declare Function SetBkColor Lib "gdi32" (ByVal hdc As Long, ByVal crColor As Long) As Long
Public Declare Function CreateCompatibleDC Lib "gdi32" (ByVal hdc As Long) As Long
Public Declare Function DeleteDC Lib "gdi32" (ByVal hdc As Long) As Long
Public Declare Function CreateBitmap Lib "gdi32" (ByVal nWidth As Long, ByVal nHeight As Long, ByVal nPlanes As Long, ByVal nBitCount As Long, ByVal lpBits As Any) As Long
Public Declare Function CreateCompatibleBitmap Lib "gdi32" (ByVal hdc As Long, ByVal nWidth As Long, ByVal nHeight As Long) As Long
Public Declare Function SelectObject Lib "gdi32" (ByVal hdc As Long, ByVal hObject As Long) As Long
Public Declare Function DeleteObject Lib "gdi32" (ByVal hObject As Long) As Long
Public Declare Function GetSystemMetrics Lib "user32" (ByVal nIndex As Long) As Long
Public Declare Function GetWindow Lib "user32" (ByVal hWnd As Long, ByVal wCmd As Long) As Long
Public Declare Function GetForegroundWindow Lib "user32" () As Long
Public Declare Function SetForegroundWindow Lib "user32" (ByVal hWnd As Long) As Long
Public Declare Function SetWindowPos _
                                      Lib "user32" _
                                       (ByVal hWnd As Long, _
                                        ByVal hWndInsertAfter As Long, _
                                        ByVal x As Long, _
                                        ByVal y As Long, _
                                        ByVal cx As Long, _
                                        ByVal cy As Long, _
                                        ByVal wFlags As Long) As Long
Public Declare Function SendMessage _
                                      Lib "user32" Alias "SendMessageA" _
                                       (ByVal hWnd As Long, _
                                        ByVal wMsg As Long, _
                                        ByVal wParam As Long, _
                                        lParam As Any) As Long
Public Declare Function WaitForInputIdle _
                                      Lib "user32" _
                                       (ByVal hProcess As Long, _
                                        ByVal dwMilliseconds As Long) As Long
Public Declare Function ShellExecute _
                                      Lib "shell32.dll" Alias "ShellExecuteA" _
                                       (ByVal hWnd As Long, _
                                        ByVal lpOperation As String, _
                                        ByVal lpFile As String, _
                                        ByVal lpParameters As String, _
                                        ByVal lpDirectory As String, _
                                        ByVal nShowCmd As Long) As Long
Public Declare Function CreateProcess _
                                      Lib "kernel32" Alias "CreateProcessA" _
                                       (ByVal lpApplicationName As String, _
                                        ByVal lpCommandLine As String, _
                                        lpProcessAttributes As Any, _
                                        lpThreadAttributes As Any, _
                                        ByVal bInheritHandles As Long, _
                                        ByVal dwCreationFlags As Long, _
                                        lpEnvironment As Any, _
                                        ByVal lpCurrentDriectory As String, _
                                        lpStartupInfo As STARTUPINFO, _
                                        lpProcessInformation As PROCESS_INFORMATION) As Long
Public Declare Function WaitForSingleObject _
                                      Lib "kernel32" _
                                       (ByVal hHandle As Long, _
                                        ByVal dwMilliseconds As Long) As Long
Public Declare Function CloseHandle _
                                      Lib "kernel32" _
                                       (ByVal hObject As Long) As Long
Public Declare Function GetPrivateProfileString _
                                      Lib "kernel32" Alias "GetPrivateProfileStringA" _
                                       (ByVal lpApplicationName As String, _
                                        ByVal lpKeyName As Any, _
                                        ByVal lpDefault As String, _
                                        ByVal lpReturnedString As String, _
                                        ByVal nSize As Long, _
                                        ByVal lpFileName As String) As Long
Public Declare Function WritePrivateProfileString _
                                      Lib "kernel32" Alias "WritePrivateProfileStringA" _
                                       (ByVal lpApplicationName As String, _
                                        ByVal lpKeyName As Any, _
                                        ByVal lpString As Any, _
                                        ByVal lpFileName As String) As Long
Public Declare Function GetLastError _
                                      Lib "kernel32" () As Long
