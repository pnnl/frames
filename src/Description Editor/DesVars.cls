VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "DesVars"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
Option Compare Text

'local variable(s) to hold property value(s)
Private mvarname As String 'local copy
Private mvarvarflag As String 'local copy
Private mvarindex1 As String 'local copy
Private mvarindex2 As String 'local copy
Private mvarindex3 As String 'local copy
Private mvarindex4 As String 'local copy
Private mvarindex5 As String 'local copy
Private mvarindex6 As String 'local copy
Public Property Let Index6(ByVal vData As String)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.index6 = 5
  mvarindex6 = vData
End Property


Public Property Get Index6() As String
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.index6
  Index6 = mvarindex6
End Property



Public Property Let Index5(ByVal vData As String)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.index5 = 5
  mvarindex5 = vData
End Property


Public Property Get Index5() As String
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.index5
  Index5 = mvarindex5
End Property



Public Property Let Index4(ByVal vData As String)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.index4 = 5
  mvarindex4 = vData
End Property


Public Property Get Index4() As String
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.index4
  Index4 = mvarindex4
End Property



Public Property Let Index3(ByVal vData As String)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.index3 = 5
  mvarindex3 = vData
End Property


Public Property Get Index3() As String
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.index3
  Index3 = mvarindex3
End Property



Public Property Let Index2(ByVal vData As String)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.index2 = 5
  mvarindex2 = vData
End Property


Public Property Get Index2() As String
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.index2
  Index2 = mvarindex2
End Property



Public Property Let Index1(ByVal vData As String)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.index1 = 5
  mvarindex1 = vData
End Property


Public Property Get Index1() As String
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.index1
  Index1 = mvarindex1
End Property



Public Property Let varflag(ByVal vData As String)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.varflag = 5
  mvarvarflag = vData
End Property


Public Property Get varflag() As String
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.varflag
  varflag = mvarvarflag
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



