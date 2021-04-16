VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "MyCollection"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Attribute VB_Ext_KEY = "Member0" ,"MyLocations"
Option Explicit
Option Compare Text

Private mObjects() As Variant
Private mKeys() As String
Private mCount As Long

Private Sub Class_Initialize()
  mCount = 0
End Sub

Public Function NewKey(Index As Long, key As String) As Boolean
  If Index > 0 And Index <= mCount Then
    mKeys(Index) = key
    NewKey = True
  Else
    NewKey = False
  End If
End Function

Public Function key(Index As Long) As String
  If Index > 0 And Index <= mCount Then
    key = mKeys(Index)
  Else
    key = ""
  End If
End Function

Public Function Index(key As String) As Long
Dim i As Long
  For i = 1 To mCount
    If mKeys(i) = key Then
      Index = i
      Exit Function
    End If
  Next
  Index = 0
End Function

Public Function IndexItem(item As Variant) As Long
Dim i As Long
  For i = 1 To mCount
    If mObjects(i) = item Then
      IndexItem = i
      Exit Function
    End If
  Next
  IndexItem = 0
End Function

Private Function use(item As Variant)
Dim dtype As String
  dtype = TypeName(item)
  If InStr(1, dtype, "(") <> 0 Then
    dtype = Right(dtype, Len(dtype) - 2)
  End If
  Select Case dtype
    Case "Empty":         use = -1
    Case "Null":          use = -1
    Case "object type":   use = 1
    Case "Object":        use = 1
    Case "Unknown":       use = 1
    Case "Nothing":       use = 1
    Case "SeriesPair":    use = 1
    Case "MyCollection":  use = 1
    Case Else:            use = 0
'    Case "Byte"
'    Case "Long"
'    Case "Long"
'    Case "Single"
'    Case "Double"
'    Case "Currency"
'    Case "Decimal"
'    Case "Date"
'    Case "String"
'    Case "Boolean"
'    Case "Error"
  End Select
End Function

Public Function Add(item As Variant, key As String) As Boolean
Dim mUse
  mUse = use(item)
  If mUse > -1 Then
    mCount = mCount + 1
    ReDim Preserve mKeys(mCount) As String
    ReDim Preserve mObjects(mCount) As Variant
    mKeys(mCount) = key
    If mUse = 1 Then
      Set mObjects(mCount) = item
    Else
      mObjects(mCount) = item
    End If
    Add = True
  Else
    Add = False
  End If
End Function

Public Function ItemIdx(Index As Long) As Variant
  If Index > 0 And Index <= mCount Then
    If use(mObjects(Index)) = 1 Then
      Set ItemIdx = mObjects(Index)
    Else
      ItemIdx = mObjects(Index)
    End If
  Else
    ItemIdx = Nothing
  End If
End Function

Public Function ItemKey(key As String) As Variant
Dim idx As Long
  idx = Index(key)
  If idx > 0 And idx <= mCount Then
    If use(mObjects(idx)) = 1 Then
      Set ItemKey = mObjects(idx)
    Else
      ItemKey = mObjects(idx)
    End If
  Else
    ItemKey = Nothing
  End If
End Function

Public Function RemoveIdx(Index As Long) As Boolean
Dim i As Long
  If Index > 0 And Index <= mCount Then
    For i = Index To mCount
      mKeys(i) = mKeys(i + 1)
      If use(mObjects(i + 1)) = 1 Then
        Set mObjects(i) = mObjects(i + 1)
      Else
        mObjects(i) = mObjects(i + 1)
      End If
    Next
    mCount = mCount - 1
    ReDim Preserve mKeys(Count) As String
    ReDim Preserve mObjects(Count) As Variant
    RemoveIdx = True
  Else
    RemoveIdx = False
  End If
End Function

Public Function RemoveKey(key As String) As Boolean
  RemoveKey = RemoveIdx(Index(key))
End Function

Public Property Get Count() As Long
  Count = mCount
End Property
