VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Location"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder" ,"Yes"
Attribute VB_Ext_KEY = "Collection" ,"SeriesPair"
Attribute VB_Ext_KEY = "Member0" ,"SeriesPair"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit

'local variable to hold collection
Private mCol As Collection
Public uuTime As String
Public muTime As String
Public uuFreq As String
Public muFreq As String
Public rLoc As Long

Public Function Addd(s As SeriesPair, Optional sKey As String) As SeriesPair
On Error Resume Next
    If Len(sKey) = 0 Then
        mCol.Add s
    Else
        mCol.Add s, sKey
    End If
    'return the object created
    Set Addd = s
End Function

Public Function Add(v1 As Variant, v2 As Variant, Optional sKey As String) As SeriesPair
    'create a new object
    Dim objNewMember As SeriesPair
    Set objNewMember = New SeriesPair


    'set the properties passed into the method
    objNewMember.v1 = v1
    objNewMember.v2 = v2


    If Len(sKey) = 0 Then
        mCol.Add objNewMember
    Else
        mCol.Add objNewMember, sKey
    End If

    'return the object created
    Set Add = objNewMember
    Set objNewMember = Nothing
End Function

Public Property Get item(vntIndexKey As Variant) As SeriesPair
Attribute item.VB_UserMemId = 0
    'used when referencing an element in the collection
    'vntIndexKey contains either the Index or Key to the collection,
    'this is why it is declared as a Variant
    'Syntax: Set foo = x.Item(xyz) or Set foo = x.Item(5)
  Set item = mCol(vntIndexKey)
End Property

Public Property Get Count() As Long
    'used when retrieving the number of elements in the
    'collection. Syntax: Debug.Print x.Count
    Count = mCol.Count
End Property

Public Sub Remove(vntIndexKey As Variant)
    'used when removing an element from the collection
    'vntIndexKey contains either the Index or Key, which is why
    'it is declared as a Variant
    'Syntax: x.Remove(xyz)
    mCol.Remove vntIndexKey
End Sub
'
'Public Property Get NewEnum() As IUnknown
'    'this property allows you to enumerate
'    'this collection with the For...Each syntax
'    Set NewEnum = mCol.[_NewEnum]
'End Property

Private Sub Class_Initialize()
    'creates the collection when this class is created
    Set mCol = New Collection
End Sub

Private Sub Class_Terminate()
    'destroys collection when this class is terminated
    Set mCol = Nothing
End Sub

Public Function WriteLocFreq(lifeidx As Long) As Boolean
Dim i As Long
Dim prev As Double

  WriteLocFreq = True
  If mCol.Count > 1 Then
    WriteLocFreq = False
    prev = mCol(1).v1
    set_parm parm, "numsteps", lifeidx, 0, 0, 0, 0, 0, 0, "N/A", "N/A", mCol.Count
    write_parmrec fle, parm
    For i = 1 To mCol.Count
      set_parm parm, "time", lifeidx, i, 0, 0, 0, 0, rLoc, uuTime, muTime, convert(uuTime, muTime, mCol(i).v1)
      write_parmrec fle, parm
      set_parm parm, "value", lifeidx, i, 0, 0, 0, 0, rLoc, uuFreq, muFreq, CStr(mCol(i).v2)
      write_parmrec fle, parm
 ' Check to make sure durations are increasing
      If mCol(i).v1 < prev Then WriteLocFreq = True
      prev = mCol(i).v1
    Next
  End If
End Function

Public Sub GetLocFreq(ss As fpSpread, lbl As Label)
Dim i As Integer
Dim v1 As Variant
Dim v2 As Variant

  rLoc = lbl.Tag
  ss.col = 1
  ss.Row = 1
  uuTime = ss.Text
  ss.col = 2
  Set mCol = Nothing
  Set mCol = New Collection
  For i = 2 To ss.DataRowCnt
    ss.col = 1
    ss.Row = i
    v1 = Val(ss.value)
    ss.value = ""
    ss.col = 2
    v2 = Val(ss.value)
    ss.CellType = 1
    ss.value = ""
    Add v1, v2
  Next
End Sub

Public Sub PutLocFreq(ss As fpSpread, lbl As Label)
Dim i As Integer
  
  lbl.Caption = "Ref: " + Str(rLoc)
  lbl.Tag = rLoc
  ss.col = 1
  ss.Row = 1
  SetSpreadUnits ss, 1, uuTime
  For i = 1 To mCol.Count
    ss.Row = i + 1
    ss.col = 1
    ss.value = mCol(i).v1
    ss.col = 2
    SetSpreadCombo ss, 2, i + 1, loc, numloc, "wcf"
    ss.value = mCol(i).v2
  Next
End Sub

Public Sub Remap(idx() As Long)
Dim i As Integer
  
  For i = 1 To mCol.Count
    mCol(i).v2 = idx(mCol(i).v2)
  Next
End Sub

