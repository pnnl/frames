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


Public Const MAX_PATH = 260
'Type POINTAPI
'        X As Long
'        Y As Long
'End Type

'Declare Function GetCursorPos Lib "user32" (lpPoint As POINTAPI) As Long

Declare Function GetTempFileName Lib "kernel32" Alias "GetTempFileNameA" (ByVal lpszPath As String, ByVal lpPrefixString As String, ByVal wUnique As Long, ByVal lpTempFileName As String) As Long


