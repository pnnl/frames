VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Vars"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
Option Compare Text

Private mvardesc As String 'local copy
Private mvarlabels As New Collection 'local copy
Private mvarmax As String 'local copy
Private mvarmin As String 'local copy
Private mvarname As String 'local copy
Private mvarstoch As String 'local copy
Private mvarunits As String 'local copy
'Private mvarvarcnt As Integer 'local copy
'Public Property Let varcnt(ByVal vData As Integer)
''used when assigning a value to the property, on the left side of an assignment.
''Syntax: X.varcnt = 5
'  mvarvarcnt = vData
'End Property


'Public Property Get varcnt() As Integer
''used when retrieving value of a property, on the right side of an assignment.
''Syntax: Debug.Print X.varcnt
'  varcnt = mvarvarcnt
'End Property



Public Property Let units(ByVal vData As String)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.units = 5
  mvarunits = vData
End Property


Public Property Get units() As String
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.units
  units = mvarunits
End Property



Public Property Let stoch(ByVal vData As String)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.stoch = 5
  mvarstoch = vData
End Property


Public Property Get stoch() As String
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.stoch
  stoch = mvarstoch
End Property



Public Property Let name(ByVal vData As String)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.name = 5
  mvarname = vData
End Property


Public Property Get name() As String
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.name
  name = mvarname
End Property



Public Property Let min(ByVal vData As String)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.min = 5
  mvarmin = vData
End Property


Public Property Get min() As String
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.min
  min = mvarmin
End Property



Public Property Let max(ByVal vData As String)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.max = 5
  mvarmax = vData
End Property


Public Property Get max() As String
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.max
  max = mvarmax
End Property



Public Property Set labels(ByVal vData As Object)
'used when assigning an Object to the property, on the left side of a Set statement.
'Syntax: Set x.labels = Form1
  Set mvarlabels = vData
End Property


Public Property Get labels() As Collection
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.labels
  Set labels = mvarlabels
End Property



Public Property Let desc(ByVal vData As String)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.desc = 5
  mvardesc = vData
End Property


Public Property Get desc() As String
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.desc
  desc = mvardesc
End Property



