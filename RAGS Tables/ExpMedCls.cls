VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ExpMedCls"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

'local variable(s) to hold property value(s)
Private mvarlabel As String 'local copy
Private mvarexppt As Collection

Public Property Set exppt(ByVal vData As Collection)
    Set mvarexppt = vData
End Property

Public Property Get exppt() As Collection
    Set exppt = mvarexppt
End Property

Public Property Let label(ByVal vData As String)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.label = 5
    mvarlabel = vData
End Property

Public Property Get label() As String
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.label
    label = mvarlabel
End Property

Private Sub Class_Initialize()
  Set mvarexppt = New Collection
End Sub

