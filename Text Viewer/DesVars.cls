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
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Option Explicit
Option Compare Text

Private mvarName As String
Private mvarFlag As String
Private mvarIndex1 As String
Private mvarIndex2 As String
Private mvarIndex3 As String
Private mvarIndex4 As String
Private mvarIndex5 As String
Private mvarIndex6 As String
Private mvarDeskey As String

Public Property Let Deskey(ByVal vData As String)
  mvarDeskey = vData
End Property

Private Property Get Deskey() As String
  Deskey = mvarDeskey
End Property

Public Property Let Index6(ByVal vData As String)
  mvarIndex6 = vData
End Property

Public Property Get Index6() As String
  Index6 = mvarIndex6
End Property

Public Property Let Index5(ByVal vData As String)
  mvarIndex5 = vData
End Property

Public Property Get Index5() As String
  Index5 = mvarIndex5
End Property

Public Property Let Index4(ByVal vData As String)
  mvarIndex4 = vData
End Property

Public Property Get Index4() As String
  Index4 = mvarIndex4
End Property

Public Property Let Index3(ByVal vData As String)
  mvarIndex3 = vData
End Property

Public Property Get Index3() As String
  Index3 = mvarIndex3
End Property

Public Property Let Index2(ByVal vData As String)
  mvarIndex2 = vData
End Property

Public Property Get Index2() As String
  Index2 = mvarIndex2
End Property

Public Property Let Index1(ByVal vData As String)
  mvarIndex1 = vData
End Property

Public Property Get Index1() As String
  Index1 = mvarIndex1
End Property

Public Property Let varFlag(ByVal vData As String)
  mvarFlag = vData
End Property

Public Property Get varFlag() As String
  varFlag = mvarFlag
End Property

Public Property Let Name(ByVal vData As String)
  mvarName = vData
End Property

Public Property Get Name() As String
  Name = mvarName
End Property

Private Function GetIndex(idx As String, i1&, i2&, i3&, i4&, i5&, i6&, siteIdx&, modIdx&) As Long
  GetIndex = 0
  Select Case idx:
    Case "Site": GetIndex = siteIdx
    Case "Glyph": GetIndex = modIdx
    Case "Index1": GetIndex = i1
    Case "Index2": GetIndex = i2
    Case "Index3": GetIndex = i3
    Case "Index4": GetIndex = i4
    Case "Index5": GetIndex = i5
    Case "Index6": GetIndex = i6
  End Select
End Function

Public Function FormatGID(i1&, i2&, i3&, i4&, i5&, i6&, vfui As FuiCls) As String
Dim tmp As String
Dim p1 As parmrec

  Select Case varFlag
    Case "Label":
      tmp = Name & " " & GetIndex(Index1, i1, i2, i3, i4, i5, i6, vfui.siteIdx, vfui.modIdx)
    Case "Variable":
      p1.idx1 = GetIndex(Index1, i1, i2, i3, i4, i5, i6, vfui.siteIdx, vfui.modIdx)
      p1.idx2 = GetIndex(Index2, i1, i2, i3, i4, i5, i6, vfui.siteIdx, vfui.modIdx)
      p1.idx3 = GetIndex(Index3, i1, i2, i3, i4, i5, i6, vfui.siteIdx, vfui.modIdx)
      p1.idx4 = GetIndex(Index4, i1, i2, i3, i4, i5, i6, vfui.siteIdx, vfui.modIdx)
      p1.idx5 = GetIndex(Index5, i1, i2, i3, i4, i5, i6, vfui.siteIdx, vfui.modIdx)
      p1.idx6 = GetIndex(Index6, i1, i2, i3, i4, i5, i6, vfui.siteIdx, vfui.modIdx)
      tmp = vfui.Lookup(vfui.ModName, Name, p1.idx1, p1.idx2, p1.idx3, p1.idx4, p1.idx5, p1.idx6)
  End Select
  FormatGID = IIf(tmp <> "", " for " & tmp, "")
End Function

