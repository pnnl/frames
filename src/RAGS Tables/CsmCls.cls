VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "CsmCls"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
Option Compare Text
'local variable(s) to hold property value(s)
Private mvarid As String 'local copy
Private mvarlabel As String 'local copy
Private mvarx As Double 'local copy
Private mvary As Double 'local copy
Private mvarsink As Collection 'local copy
Private mvarsrc As Collection
'local variable(s) to hold property value(s)
Private mvarstype As String 'local copy
Private mvarsqual As String 'local copy
'local variable(s) to hold property value(s)
Private mvarmodel As String 'local copy
Public Property Let model(ByVal vData As String)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.model = 5
    mvarmodel = vData
End Property


Public Property Get model() As String
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.model
    model = mvarmodel
End Property



Public Property Let squal(ByVal vData As String)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.squal = 5
    mvarsqual = vData
End Property


Public Property Get squal() As String
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.squal
    squal = mvarsqual
End Property



Public Property Let stype(ByVal vData As String)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.stype = 5
    mvarstype = vData
End Property


Public Property Get stype() As String
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.stype
    stype = mvarstype
End Property



Public Property Set sink(ByVal vData As Collection)
'used when assigning an Object to the property, on the left side of a Set statement.
'Syntax: Set x.sink = Form1
    Set mvarsink = vData
End Property


Public Property Get sink() As Collection
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.sink
    Set sink = mvarsink
End Property


Public Property Set src(ByVal vData As Collection)
'used when assigning an Object to the property, on the left side of a Set statement.
'Syntax: Set x.sink = Form1
    Set mvarsrc = vData
End Property


Public Property Get src() As Collection
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.sink
    Set src = mvarsrc
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



Public Property Let Id(ByVal vData As String)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.id = 5
    mvarid = vData
End Property


Public Property Get Id() As String
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.id
    Id = mvarid
End Property



