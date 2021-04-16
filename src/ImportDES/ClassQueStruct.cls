VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ClassDesQue"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

'Descripion file variable sub-information

Public QueType  As String
Public QueName   As String
Public QueIndices As Collection 'String

Private Sub Class_Initialize()
    On Error GoTo InitializeError
    QueType = BLANK
    QueName = BLANK
    Set QueIndices = New Collection
    Exit Sub

InitializeError:
End Sub
