VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ParmCls"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
Option Compare Text
'local variable(s) to hold property value(s)
Private mvaruunit As String 'local copy
Private mvarcunit As String 'local copy
Private mvarpname As String 'local copy
Private mvarpval As String 'local copy
Private mvarfound As Boolean 'local copy
Private mvarref As Long
Public Property Let found(ByVal vData As Boolean)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.found = 5
    mvarfound = vData
End Property

Public Property Get found() As Boolean
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.found
    found = mvarfound
End Property

Public Property Let pval(ByVal vData As String)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.pval = 5
    mvarpval = vData
End Property

Public Property Get pval() As String
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.pval
    pval = mvarpval
End Property

Public Property Let pname(ByVal vData As String)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.pname = 5
    mvarpname = vData
End Property

Public Property Get pname() As String
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.pname
    pname = mvarpname
End Property

Public Property Let cunit(ByVal vData As String)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.cunit = 5
    mvarcunit = vData
End Property

Public Property Get cunit() As String
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.cunit
    cunit = mvarcunit
End Property

Public Property Let uunit(ByVal vData As String)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.uunit = 5
    mvaruunit = vData
End Property

Public Property Get uunit() As String
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.uunit
    uunit = mvaruunit
End Property

Public Property Get ref() As Long
  ref = mvarref
End Property

Public Property Let ref(ByVal vData As Long)
  mvarref = vData
End Property

