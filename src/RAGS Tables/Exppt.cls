VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ExpPtCls"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
Option Compare Text
'local variable(s) to hold property value(s)
Private mvarx As Double 'local copy
Private mvary As Double 'local copy
Private mvarlabel As String 'local copy
Private mvarexprt As Collection

Public Property Set exprt(ByVal vData As Collection)
    Set mvarexprt = vData
End Property

Public Property Get exprt() As Collection
    Set exprt = mvarexprt
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

Public Property Let y(ByVal vData As Double)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.y = 5
    mvary = vData
End Property

Public Property Get y() As Double
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.y
    y = mvary
End Property

Public Property Let x(ByVal vData As Double)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.x = 5
    mvarx = vData
End Property

Public Property Get x() As Double
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.x
    x = mvarx
End Property

Public Function Desc() As String
  Desc = label & " [" + CStr(x) + "," + CStr(y) + " km]"
' Desc = label
End Function

Private Sub Class_Initialize()
  Set mvarexprt = New Collection
End Sub
