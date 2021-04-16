VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "DatasetCls"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Attribute VB_Ext_KEY = "Member0" ,"exppt"
Attribute VB_Ext_KEY = "Member1" ,"age"
Attribute VB_Ext_KEY = "Member2" ,"exprt"
Option Explicit
Option Compare Text
'local variable(s) to hold property value(s)
Private mvardstype As String 'local copy
Private mvarlocname As String 'local copy
Private mvarmedtype As String 'local copy
Private mvardsname As String 'local copy
Private mvarexppt As Collection 'local copy
Private mvarage As Collection 'local copy
Private mvarsites As Collection 'local copy
Private mvarorgans As Collection 'local copy
Private mvarexprt As Collection 'local copy
Private mvarexpmed As Collection

'''Public Type dataset
'''  dstype As String
'''  locName As String
'''  medtype As String
'''  Name As String
'''  endpoint As String
'''  numpt As Long
'''  numage As Long
'''  numcon As Long
'''  nsites As Long
'''  norgans As Long
'''  x() As Long
'''  y() As Long
'''  agemin() As Double
'''  agemax() As Double
'''  sites() As String
'''  organs() As String
'''End Type

Public Property Set exprt(ByVal vData As Collection)
    Set mvarexprt = vData
End Property

Public Property Get exprt() As Collection
    Set exprt = mvarexprt
End Property

Public Property Set organs(ByVal vData As Collection)
    Set mvarorgans = vData
End Property

Public Property Get organs() As Collection
    Set organs = mvarorgans
End Property

Public Property Set sites(ByVal vData As Collection)
    Set mvarsites = vData
End Property

Public Property Get sites() As Collection
    Set sites = mvarsites
End Property

Public Property Set age(ByVal vData As Collection)
    Set mvarage = vData
End Property

Public Property Get age() As Collection
    Set age = mvarage
End Property

Public Property Set exppt(ByVal vData As Collection)
    Set mvarexppt = vData
End Property

Public Property Get exppt() As Collection
    Set exppt = mvarexppt
End Property

Public Property Set expmed(ByVal vData As Collection)
    Set mvarexpmed = vData
End Property

Public Property Get expmed() As Collection
    Set expmed = mvarexpmed
End Property

Public Property Let dsname(ByVal vData As String)
    mvardsname = vData
End Property

Public Property Get dsname() As String
    dsname = mvardsname
End Property

Public Property Let medtype(ByVal vData As String)
    mvarmedtype = vData
End Property

Public Property Get medtype() As String
    medtype = mvarmedtype
End Property

Public Property Let locName(ByVal vData As String)
    mvarlocname = vData
End Property

Public Property Get locName() As String
    locName = mvarlocname
End Property

Public Property Let dstype(ByVal vData As String)
    mvardstype = vData
End Property

Public Property Get dstype() As String
    dstype = mvardstype
End Property

Private Sub Class_Initialize()
  Set mvarexppt = New Collection
  Set mvarage = New Collection
  Set mvarsites = New Collection
  Set mvarorgans = New Collection
  Set mvarexprt = New Collection
  Set mvarexpmed = New Collection
End Sub
