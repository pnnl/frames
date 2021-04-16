VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "AgeCls"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
Option Compare Text
Private mvarmin As Double 'local copy
Private mvarmax As Double 'local copy

Public Property Let min(ByVal vData As Double)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.min = 5
    mvarmin = vData
End Property

Public Property Get min() As Double
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.min
    min = mvarmin
End Property

Public Property Let max(ByVal vData As Double)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.max = 5
    mvarmax = vData
End Property

Public Property Get max() As Double
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.max
    max = mvarmax
End Property

Public Function Desc() As String
  Desc = CStr(min) + "-" + CStr(max) + " yr"
End Function
