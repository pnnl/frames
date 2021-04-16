VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ClassDicHeader"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

'Dictionary file row
Public VariableCount As Long
Public Description As String
Public Name As String
Public Privilege As Long
Public Version As Long
Public Updated As Long
Public Template As String

Private Sub Class_Initialize()
  VariableCount = 0
  Description = BLANK
  Name = BLANK
  Privilege = 0
  Version = 1
  Updated = 0
  Template = BLANK
End Sub
