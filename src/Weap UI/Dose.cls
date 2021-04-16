VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Dose"
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
Public des As String
Public id As String
Public idx As Long
Public Acute As Double
Public uuAcute As String
Public muAcute As String
Public rAcute As Long
Public CCC As Double
Public uuCCC As String
Public muCCC As String
Public rCCC As Long
Public uuDur As String
Public muDur As String
Public uuConc As String
Public muConc As String
Public rDose As Long
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

Public Function WriteAcute(lifeidx As Long) As Boolean
  WriteAcute = False
  set_parm parm, "casid", lifeidx, idx, 0, 0, 0, 0, 0, "N/A", "N/A", id
  write_parmrec fle, parm
  set_parm parm, "acutetime", lifeidx, idx, 0, 0, 0, 0, rAcute, uuAcute, muAcute, convert(uuAcute, muAcute, Acute)
  write_parmrec fle, parm
  If Acute <= 0 Then WriteAcute = True
End Function

Public Function CurvesOK() As Boolean
Dim tc As Double
Dim tc2 As Double

  CurvesOK = True
  tc = Val(convert(uuCCC, muCCC, CCC))
  tc2 = Val(convert(uuConc, muConc, mCol(mCol.Count).v2))
  If tc > tc2 Then CurvesOK = False
End Function

Public Function WriteCCC(lifeidx As Long) As Boolean
  WriteCCC = False
  set_parm parm, "cccvalue", lifeidx, idx, 0, 0, 0, 0, rCCC, uuCCC, muCCC, convert(uuCCC, muCCC, CCC)
  write_parmrec fle, parm
  If CCC <= 0 Then WriteCCC = True
End Function

Public Function WriteDoseCurve(lifeidx As Long) As Boolean
Dim i As Long
Dim prev As Double
Dim prevc As Double

  WriteDoseCurve = True
  If mCol.Count > 1 Then
    WriteDoseCurve = False
    prev = mCol(1).v1
    prevc = mCol(1).v2
    set_parm parm, "numlc", lifeidx, idx, 0, 0, 0, 0, 0, "N/A", "N/A", mCol.Count
    write_parmrec fle, parm
    set_parm parm, "deslc", lifeidx, idx, 0, 0, 0, 0, 0, "N/A", "N/A", des
    write_parmrec fle, parm
    For i = 1 To mCol.Count
      set_parm parm, "lcdur", lifeidx, idx, i, 0, 0, 0, rDose, uuDur, muDur, convert(uuDur, muDur, mCol(i).v1)
      write_parmrec fle, parm
      set_parm parm, "lcconc", lifeidx, idx, i, 0, 0, 0, rDose, uuConc, muConc, convert(uuConc, muConc, mCol(i).v2)
      write_parmrec fle, parm
 ' Check to make sure durations are increasing
 ' Check to make sure concentrations are decreasing or equal
      If mCol(i).v1 < prev Then WriteDoseCurve = True
      If mCol(i).v2 > prevc Then WriteDoseCurve = True
      prev = mCol(i).v1
      prevc = mCol(i).v2
    Next
  End If
End Function

Public Sub GetAcute(txt As TextBox, unit As ComboBox, lbl As Label)
  Acute = Val(txt.Text)
  uuAcute = unit.Text
  rAcute = lbl.Tag
End Sub

Public Sub PutAcute(txt As TextBox, unit As ComboBox, lbl As Label)
  txt.Text = Acute
  unit.Clear
  get_conversion_items uuAcute, unit
  lbl.Tag = rAcute
  lbl.Caption = "Ref: " + Str(rAcute)
End Sub

Public Sub GetCCC(txt As TextBox, unit As ComboBox, lbl As Label)
  CCC = Val(txt.Text)
  uuCCC = unit.Text
  rCCC = lbl.Tag
End Sub

Public Sub PutCCC(txt As TextBox, unit As ComboBox, lbl As Label)
  txt.Text = CCC
  unit.Clear
  get_conversion_items uuCCC, unit
  lbl.Tag = rCCC
  lbl.Caption = "Ref: " + Str(rCCC)
End Sub

Public Sub GetDoseCurve(ss As fpSpread, lbl As Label, dtext As TextBox)
Dim i As Integer
Dim v1 As Variant
Dim v2 As Variant

  des = dtext.Text
  rDose = lbl.Tag
  ss.col = 1
  ss.Row = 1
  uuDur = ss.Text
  ss.col = 2
  uuConc = ss.Text
  Set mCol = Nothing
  Set mCol = New Collection
  For i = 2 To ss.DataRowCnt
    ss.col = 1
    ss.Row = i
    v1 = Val(ss.value)
    ss.value = ""
    ss.col = 2
    v2 = Val(ss.value)
    ss.value = ""
    Add v1, v2
  Next
End Sub

Public Sub PutDoseCurve(ss As fpSpread, lbl As Label, dtext As TextBox)
Dim i As Integer
    
  dtext.Text = des
  lbl.Caption = "Ref: " + Str(rDose)
  lbl.Tag = rDose
  ss.col = 1
  ss.Row = 1
  SetSpreadUnits ss, 1, uuDur
  SetSpreadUnits ss, 2, uuConc
  For i = 1 To mCol.Count
    ss.Row = i + 1
    ss.col = 1
    ss.value = mCol(i).v1
    ss.col = 2
    ss.value = mCol(i).v2
  Next
End Sub

