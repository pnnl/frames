VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Selects"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Private lbl As Object
Private box As Object
Private rf As Object
Private listeners As New Collection
Private orgname As String
Private orglbl As String
Private orgval As Long
Private orgref As Long
Private ind(5) As Long
Private namestored As Boolean
Private valustored As Boolean
Public Islogical As Boolean
 

Sub Init(n As String, c As String, l As Object, b As Object, r As Object, ParamArray ix())
  Dim j As Long
  Set lbl = l
  Set box = b
  Set rf = r
  For j = LBound(ix) To UBound(ix)
    ind(j) = ix(j)
  Next
  lbl.Tag = n + Format(ind(0))
  Label = c
  Name = n
  orgval = -1
  namestored = False
  valustored = False
  Islogical = False
End Sub

Public Property Get Visible()
  Visible = box.Visible
End Property

Public Property Let Visible(vNewValue)
  lbl.Visible = vNewValue
  box.Visible = vNewValue
  rf.Visible = vNewValue
End Property

Public Property Get Enabled()
  Enabled = box.Enabled
End Property

Public Property Let Enabled(vNewValue)
    lbl.Enabled = vNewValue
    box.Enabled = vNewValue
    rf.Enabled = vNewValue
End Property

Public Sub StatusListeners(vNewValue)
Dim i As Long
  For i = 1 To listeners.Count
    listeners(i).Enabled = vNewValue
    listeners(i).StatusListeners vNewValue
  Next
End Sub

Public Property Get Name()
  Name = orgname
End Property

Public Property Let Name(vNewValue)
  box.Tag = vNewValue
  orgname = vNewValue
End Property

Public Property Get Label()
  Label = orglbl
End Property

Public Property Let Label(vNewValue)
  lbl.Caption = vNewValue
  orglbl = vNewValue
End Property

Public Property Get ListIndex()
  ListIndex = orgval
End Property

Public Property Let ListIndex(vNewValue)
  box.ListIndex = vNewValue
  orgval = vNewValue
End Property

Public Property Get ref()
  ref = rf
End Property

Public Property Let ref(vNewValue)
  rf = vNewValue
  rf.Caption = "Ref: " + rf.Tag
End Property

Public Property Get refnum()
  refnum = orgref
End Property

Public Property Let refnum(vNewValue)
  orgref = vNewValue
  rf.Tag = vNewValue
  rf.Caption = "Ref: " + rf.Tag
End Property

Public Property Get idx(i As Long)
  If LBound(ind) <= i And i <= UBound(ind) Then
    idx = ind(i)
  Else
    idx = ind(0)
  End If
End Property

Public Property Let idx(i As Long, vNewValue)
  If LBound(ind) <= i And i <= UBound(ind) Then ind(i) = vNewValue
End Property

Public Property Get pval()
  If box.ListIndex = -1 Then
    pval = ""
  Else
    pval = CStr(box.ItemData(box.ListIndex))
  End If
End Property

Public Property Let pval(vNewValue)
Dim i As Long
  For i = 0 To box.ListCount - 1
    If box.ItemData(i) = vNewValue Then
      box.ListIndex = i
      orgval = i
    End If
  Next
End Property

Public Property Get tval()
  If ListIndex = -1 Then
    tval = ""
  Else
    tval = box.list(ListIndex)
  End If
End Property

Public Property Let tval(vNewValue)
Dim i As Long
  For i = 0 To box.ListCount - 1
    If box.list(i) = vNewValue Then
      box.ListIndex = i
      orgval = i
    End If
  Next
End Property

Public Sub Refresh()
  lbl.Tag = orgname + Format(ind(0))
  Label = orglbl
'  ListIndex = orgval
  refnum = orgref
End Sub

Public Sub Click()
  orgval = box.ListIndex
End Sub

Public Function Store()
Dim parm As parmrec
  If Not valustored Then
    set_parm parm, Name, idx(0), idx(1), idx(2), idx(3), idx(4), idx(5), refnum, "N/A", "N/A", pval
    write_parmrec fle, parm
    valustored = True
  End If
  If ListIndex = -1 Then
    Store = False
  Else
    Store = True
  End If
End Function

Public Function StoreName()
Dim parm As parmrec
  If Not namestored Then
    set_parm parm, Name, idx(0), idx(1), idx(2), idx(3), idx(4), idx(5), refnum, "N/A", "N/A", box.Text
    write_parmrec fle, parm
    namestored = True
  End If
  If box.Text = "" Then
    StoreName = False
  Else
    StoreName = True
  End If
End Function

Public Sub Help()
  MsgBox "Choose one of the values in the list.", 64, Label + " help!"
End Sub

Public Sub Add(item As Variant, Optional key As Variant)
  listeners.Add item, key
End Sub

Public Sub Remove(index As Variant)
  listeners.Remove index
End Sub

