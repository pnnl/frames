VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ExpRtCls"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
Option Compare Text
'local variable(s) to hold property value(s)
Private mvarroute As String 'local copy
Private mvarpathway As String 'local copy
Private mvarunit As String 'local copy
Private mvarmeasure As String 'local copy

Public Property Let Route(ByVal vData As String)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.route = 5
    mvarroute = vData
End Property

Public Property Get Route() As String
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.route
    Route = mvarroute
End Property

Public Property Let pathway(ByVal vData As String)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.pathway = 5
    mvarpathway = vData
End Property

Public Property Get pathway() As String
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.pathway
    pathway = mvarpathway
End Property

Public Property Let Measure(ByVal vData As String)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.measure = 5
    mvarmeasure = vData
End Property

Public Property Get Measure() As String
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.measure
    Measure = mvarmeasure
End Property

Public Property Let Unit(ByVal vData As String)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.unit = 5
    mvarunit = vData
End Property

Public Property Get Unit() As String
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.unit
    Unit = mvarunit
End Property

