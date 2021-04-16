VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "FileType"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
Option Compare Text

Private mvarfname As String
Private mvarident As String
Private mvarMin As Integer
Private mvarMax As Integer
Private mvarftype As Collection

Public Property Set ftype(ByVal vData As Collection)
  Set mvarftype = vData
End Property

Public Property Get ftype() As Collection
  Set ftype = mvarftype
End Property

Public Property Let Max(ByVal vData As Integer)
  mvarMax = vData
End Property

Public Property Get Max() As Integer
  Max = mvarMax
End Property

Public Property Let Min(ByVal vData As Integer)
  mvarMin = vData
End Property

Public Property Get Min() As Integer
  Min = mvarMin
End Property

Public Property Let ident(ByVal vData As String)
  mvarident = vData
End Property

Public Property Get ident() As String
  ident = mvarident
End Property

Public Property Let fname(ByVal vData As String)
  mvarfname = vData
End Property

Public Property Get fname() As String
  fname = mvarfname
End Property

Public Sub Init(v1 As String, v2 As String, v3 As String, v4 As String)
  fname = v1
  ident = v2
  Min = Val(v3)
  Max = Val(v4)
End Sub

