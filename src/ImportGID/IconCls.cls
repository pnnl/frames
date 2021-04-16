VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "IconCls"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
Option Compare Text
Private mvarId As String
Private mvarLabel As String
Private mvarPosX As Long
Private mvarPosY As Long
Private mvarPosZ As Long
Private mvarDes As String
Private mvarDomain As String
Private mvarClass As String
Private mvarGroup As String
Private mvarSubGrp As String
Private mvarIcon As String
Private mvarState As Long
Private mvarScope As Long
Private mvarNote As String
Private mvarProducers As Collection
Private mvarConsumers As Collection
Private mvarIndex As Long
Private mvarModel As String
Private mvarScheme As String
Private mvarSimSet As String

Public Property Get scope() As Long
  scope = mvarScope
End Property

Public Property Set Consumers(ByVal vData As Collection)
  Set mvarConsumers = vData
End Property

Public Property Get Consumers() As Collection
  Set Consumers = mvarConsumers
End Property

Public Property Set Producers(ByVal vData As Collection)
  Set mvarProducers = vData
End Property

Public Property Get Producers() As Collection
  Set Producers = mvarProducers
End Property

Public Property Let state(ByVal vData As Long)
  mvarState = vData
End Property

Public Property Get state() As Long
  state = mvarState
End Property

Public Property Get domain() As String
  domain = mvarDomain
End Property

Public Property Get Class() As String
  Class = mvarClass
End Property

Public Property Get group() As String
  group = mvarGroup
End Property

Public Property Get Subgrp() As String
  Subgrp = mvarSubGrp
End Property

Public Property Let id(ByVal vData As String)
Dim ix As tINDEX
  If DisplayError(VarLookUp(PID, mvarSimSet, "ModId", vData, ix), "_VarLookUp") = FAIL Then Exit Property
  mvarIndex = ix.i1
  mvarId = vData
End Property

Public Property Get id() As String
  id = mvarId
End Property

Public Property Let SimSet(ByVal vData As String)
  mvarSimSet = vData
End Property

Public Property Get SimSet() As String
  SimSet = mvarSimSet
End Property

Public Property Let posY(ByVal vData As Long)
  If mvarSimSet = blank Or mvarId = blank Then Exit Property
  mvarPosY = vData
  DisplayError PutInteger(PID, mvarSimSet, "ModPosY", "", SetIdx(mvarIndex), mvarPosY), "_PutInteger"
End Property

Public Property Get posY() As Long
  posY = mvarPosY
End Property

Public Property Let posX(ByVal vData As Long)
  If mvarSimSet = blank Or mvarId = blank Then Exit Property
  mvarPosX = vData
  DisplayError PutInteger(PID, mvarSimSet, "ModPosX", "", SetIdx(mvarIndex), mvarPosX), "_PutInteger"
End Property

Public Property Get posX() As Long
  posX = mvarPosX
End Property

Public Property Let Label(ByVal vData As String)
  If mvarSimSet = blank Or mvarId = blank Then Exit Property
  mvarLabel = vData
  DisplayError PutString(PID, mvarSimSet, "ModLabel", "", SetIdx(mvarIndex), mvarLabel), "_PutString"
End Property

Public Property Get Label() As String
  Label = mvarLabel
End Property

Public Property Let Note(ByVal vData As String)
  If mvarSimSet = blank Or mvarId = blank Then Exit Property
  mvarNote = vData
  DisplayError PutString(PID, mvarSimSet, "ModNote", "", SetIdx(mvarIndex), mvarNote), "_PutString"
End Property

Public Property Get Note() As String
  Note = mvarNote
End Property

Public Property Let model(ByVal vData As String)
  If mvarSimSet = blank Or mvarId = blank Then Exit Property
  mvarModel = vData
  DisplayError PutString(PID, mvarSimSet, "ModName", "", SetIdx(mvarIndex), mvarModel), "_PutString"
End Property

Public Property Get model() As String
  model = mvarModel
End Property

Public Property Let scheme(ByVal vData As String)
  If mvarSimSet = blank Or mvarId = blank Then Exit Property
  mvarScheme = vData
  DisplayError PutString(PID, mvarSimSet, "ModScheme", "", SetIdx(mvarIndex), mvarScheme), "_PutString"
End Property

Public Property Get scheme() As String
  scheme = mvarScheme
End Property

Public Sub SetXY(x As Long, y As Long)
  posX = x
  posY = y
End Sub

Public Property Let des(ByVal vData As String)
  mvarDes = vData
End Property

Public Property Get des() As String
  des = mvarDes
End Property

Public Property Get index() As Long
  index = mvarIndex
End Property

Public Property Let index(ByVal vData As Long)
  mvarIndex = vData
End Property

Public Property Get icon() As String
  icon = mvarIcon
End Property

Public Property Let icon(ByVal vData As String)
  mvarIcon = icon
End Property

Public Sub GetConsumers()
Dim i As Long, scsv, tStr As String
  
  Set mvarConsumers = New Collection
  If DisplayError(GetIModList(PID, mvarId, delimiter, tStr), "_GetIModList") = FAIL Then Exit Sub
  scsv = Split(tStr, delimiter)
  For i = 0 To UBound(scsv)
    mvarConsumers.Add CStr(scsv(i)), scsv(i)
  Next i
  End Sub

Public Sub GetProducers()
Dim i As Long, scsv, tStr As String
  
  Set mvarProducers = New Collection
  If DisplayError(GetOModList(PID, mvarId, delimiter, tStr), "_GetOModList") = FAIL Then Exit Sub
  scsv = Split(tStr, delimiter)
  For i = 0 To UBound(scsv)
    If (scsv(i) <> "sink") Then
      mvarProducers.Add CStr(scsv(i)), scsv(i)
    End If
  Next i
End Sub

Public Sub GetModuleProperties()
Dim ix As tINDEX
Dim sResult As String

  If mvarSimSet = blank Or mvarId = blank Then Exit Sub
  If DisplayError(VarLookUp(PID, mvarSimSet, "ModId", mvarId, ix), "_VarLookUp") = FAIL Then Exit Sub
  mvarIndex = ix.i1
  ix = SetIdx(mvarIndex)
  DisplayError GetInteger(PID, mvarSimSet, "ModPosX", "", ix, mvarPosX), "_GetInteger"
  DisplayError GetInteger(PID, mvarSimSet, "ModPosY", "", ix, mvarPosY), "_GetInteger"
  DisplayError GetInteger(PID, mvarSimSet, "ModScope", "", ix, mvarScope), "_GetInteger"
  DisplayError GetInteger(PID, mvarSimSet, "ModState", "", ix, mvarState), "_GetInteger"
  DisplayError GetString(PID, mvarSimSet, "ModDomain", "", ix, mvarDomain), "_GetString"
  DisplayError GetString(PID, mvarSimSet, "ModClass", "", ix, mvarClass), "_GetString"
  DisplayError GetString(PID, mvarSimSet, "ModGroup", "", ix, mvarGroup), "_GetString"
  DisplayError GetString(PID, mvarSimSet, "ModSubGroup", "", ix, mvarSubGrp), "_GetString"
  DisplayError GetString(PID, mvarSimSet, "ModLabel", "", ix, mvarLabel), "_GetString"
  DisplayError GetString(PID, mvarSimSet, "ModIcon", "", ix, mvarIcon), "_GetString"
  DisplayError GetString(PID, mvarSimSet, "ModName", "", ix, mvarModel), "_GetString"
  DisplayError GetString(PID, mvarSimSet, "ModScheme", "", ix, mvarScheme), "_GetString"
  DisplayError GetString(PID, mvarSimSet, "ModNote", "", ix, mvarNote), "_GetString"
  GetConsumers
  GetProducers
  
  If "" = Dir$(mvarIcon) Or "" = mvarIcon Then
    DisplayError GetModString(PID, mvarModel, "Icon", blank, SetIdx(), mvarIcon), "_GetModString"
  End If
  If "" = Dir$(mvarIcon) Or "" = mvarIcon Then
    If mvarSubGrp = "" Then
      If mvarGroup = "" Then
        DisplayError (GetClassIcon(PID, mvarDomain, mvarClass, mvarIcon)), "_GetClassIcon"
      Else
        DisplayError (GetGroupIcon(PID, mvarDomain, mvarClass, mvarGroup, mvarIcon)), "_GetGroupIcon"
      End If
    Else
      DisplayError (GetSubGrpIcon(PID, mvarDomain, mvarClass, mvarGroup, mvarSubGrp, mvarIcon)), "_GetSubGrpIcon"
    End If
  End If
  If "" = mvarIcon Or "" = Dir$(mvarIcon) Then
    mvarIcon = ClassIconPath
    DisplayError PutString(PID, mvarSimSet, "ModIcon", "", ix, mvarIcon), "_PutString"
  End If
    
End Sub

Public Sub Class_Initialize()
  mvarId = ""
  mvarIndex = 0
  mvarPosX = 0
  mvarPosY = 0
  mvarPosZ = 0
  mvarState = 0
  mvarClass = ""
  mvarLabel = ""
  mvarIcon = ""
  mvarNote = ""
  mvarDes = ""
  mvarScope = 0
  Set mvarConsumers = New Collection
  Set mvarProducers = New Collection
End Sub
