VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "TRV"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

'local variable to hold collection
Private mCol As Collection
Public id As String
Public idx As Long
Public uuDur As String
Public muDur As String
Public uuConc As String
Public muConc As String
Public ref As Long
Private parm As parmrec

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

Public Function Add(v1 As Variant, v2 As Variant, v3 As Variant, Optional sKey As String) As SeriesPair
    'create a new object
    Dim objNewMember As SeriesPair
    Set objNewMember = New SeriesPair

    'set the properties passed into the method
    objNewMember.v1 = v1
    objNewMember.v2 = v2
    objNewMember.v3 = v3

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

Public Function WriteTRVs(lifeidx As Long) As Boolean
Dim i As Long
 
  WriteTRVs = True
  If mCol.Count > 0 Then
    WriteTRVs = False
    set_parm parm, "ucasid", idx, lifeidx, 0, 0, 0, 0, 0, "N/A", "N/A", id
    write_parmrec fle, parm
    set_parm parm, "NumUserChem", idx, lifeidx, 0, 0, 0, 0, 0, "N/A", "N/A", mCol.Count
    write_parmrec fle, parm
    For i = 1 To mCol.Count
      set_parm parm, "UserDescript", idx, lifeidx, i, 0, 0, 0, 0, "N/A", "N/A", mCol(i).v3
      write_parmrec fle, parm
      set_parm parm, "UserChem", idx, lifeidx, i, 0, 0, 0, ref, uuConc, muConc, convert(uuConc, muConc, mCol(i).v1)
      write_parmrec fle, parm
      set_parm parm, "UserChemDur", idx, lifeidx, i, 0, 0, 0, ref, uuDur, muDur, convert(uuDur, muDur, mCol(i).v2)
      write_parmrec fle, parm
    Next
  End If
End Function

Public Sub GetTRVs(ss As fpSpread, lbl As Label)
Dim i As Integer
Dim v1 As Variant
Dim v2 As Variant
Dim v3 As Variant

  ref = lbl.Tag
  ss.Row = 1
  ss.col = 1
  uuConc = ss.Text
  ss.col = 2
  uuDur = ss.Text
  Set mCol = Nothing
  Set mCol = New Collection
  For i = 2 To ss.DataRowCnt
    ss.Row = i
    ss.col = 1
    v1 = Val(ss.value)
    ss.value = ""
    ss.col = 2
    v2 = Val(ss.value)
    ss.value = ""
    ss.col = 3
    v3 = ss.Text
    ss.value = ""
    Add v1, v2, v3
  Next
End Sub

Public Sub PutTRVs(ss As fpSpread, lbl As Label)
Dim i As Integer
    
  lbl.Caption = "Ref: " + Str(ref)
  lbl.Tag = ref
  ss.col = 1
  ss.Row = 1
  SetSpreadUnits ss, 1, uuConc
  SetSpreadUnits ss, 2, uuDur
  For i = 1 To mCol.Count
    ss.Row = i + 1
    ss.col = 1
    ss.value = mCol(i).v1
    ss.col = 2
    ss.value = mCol(i).v2
    ss.col = 3
    ss.value = mCol(i).v3
  Next
End Sub


