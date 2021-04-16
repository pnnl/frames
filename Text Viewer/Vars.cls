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
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Option Explicit
Option Compare Text

Private mvarName As String
Private mvarMin As String
Private mvarMax As String
Private mvarUnits As String
Private mvarDeskey As String
Private mvarStochastic As String
Private mvarDescription As String
Private mvarLabels As New Collection

Public Property Let Deskey(ByVal vData As String)
  mvarDeskey = vData
End Property

Private Property Get Deskey() As String
  Deskey = mvarDeskey
End Property

Public Property Let Units(ByVal vData As String)
  mvarUnits = vData
End Property

Public Property Get Units() As String
  Units = mvarUnits
End Property

Public Property Let Stoch(ByVal vData As String)
  mvarStochastic = vData
End Property

Public Property Get Stoch() As String
  Stoch = mvarStochastic
End Property

Public Property Let Name(ByVal vData As String)
  mvarName = vData
End Property

Public Property Get Name() As String
  Name = mvarName
End Property

Public Property Let Min(ByVal vData As String)
  mvarMin = vData
End Property

Public Property Get Min() As String
  Min = mvarMin
End Property

Public Property Let Max(ByVal vData As String)
  mvarMax = vData
End Property

Public Property Get Max() As String
  Max = mvarMax
End Property

Public Property Set Labels(ByVal vData As Object)
  Set mvarLabels = vData
End Property

Public Property Get Labels() As Collection
  Set Labels = mvarLabels
End Property

Public Property Let Desc(ByVal vData As String)
  mvarDescription = vData
End Property

Public Property Get Desc() As String
  Desc = mvarDescription
End Property

Public Function FormatGID(i1 As Long, i2 As Long, i3 As Long, i4 As Long, i5 As Long, i6 As Long, _
                          ref As Long, uunit As String, cunit As String, value As String, vfui As FuiCls) As String
Dim i As Long
Dim tmp As String

  tmp = Desc
  For i = 1 To Labels.Count
    tmp = tmp & Labels.Item(i).FormatGID(i1, i2, i3, i4, i5, i6, vfui)
  Next i
  tmp = tmp & " (" & Name & "):  "
  If IsNumeric(value) Then
    tmp = tmp & convert(cunit, uunit, CDbl(Val(value)))
  Else
    tmp = tmp & value
  End If
  If uunit <> "N/A" Then tmp = tmp & " " & uunit
  FormatGID = tmp
End Function
