VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "Logical"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Private box As Object
Private rf As Object
Private listeners As New Collection
Private orgval As Integer
Private orgref As Long
Private orgname As String
Private ind(5) As Long
Private stored As Boolean
Public Islogical As Boolean
 

Sub Init(n As String, c As String, b As Object, r As Object, ParamArray ix())
  Dim j As Long
  Set box = b
  Set rf = r
  Name = n
  Label = c
  For j = LBound(ix) To UBound(ix)
    ind(j) = ix(j)
  Next
  box.Tag = n + Format(ind(0))
  stored = False
  Islogical = True
End Sub

Public Property Get Visible()
  Visible = box.Visible
End Property

Public Property Let Visible(vNewValue)
  box.Visible = vNewValue
  rf.Visible = vNewValue
End Property

Public Property Get Enabled()
  Enabled = box.Enabled
End Property

Public Property Let Enabled(vNewValue)
  box.Enabled = vNewValue
  rf.Enabled = vNewValue
End Property

Public Sub StatusListeners(vNewValue)
Dim i As Long
  For i = 1 To listeners.Count
    If listeners(i).Islogical Then listeners(i).Value = vNewValue
    listeners(i).Enabled = vNewValue
    listeners(i).StatusListeners vNewValue
  Next
End Sub

Public Property Get TabStop()
  TabStop = box.TabStop
End Property

Public Property Let TabStop(vNewValue)
  box.TabStop = vNewValue
End Property

Public Property Get Name()
  Name = orgname
End Property

Public Property Let Name(vNewValue)
  orgname = vNewValue
End Property

Public Property Get Label()
  Label = box.Caption
End Property

Public Property Let Label(vNewValue)
  box.Caption = vNewValue
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

Public Property Get index()
  index = box.index
End Property

Public Property Let index(vNewValue)
  box.index = vNewValue
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

Public Property Get Value()
  Value = orgval
End Property

Public Property Let Value(vNewValue)
  orgval = vNewValue
  StatusListeners orgval
  box.Value = orgval
End Property

Public Function pval() As String
  If orgval = 0 Then
    pval = "False"
  Else
    pval = "True"
  End If
End Function

Public Sub Refresh()
  box.Tag = orgname + Format(ind(0))
  box.Value = orgval
'  Label = orglbl
'  ListIndex = orgval
  refnum = orgref
End Sub

Public Function Store()
Dim parm As parmrec
  If Not stored Then
    set_parm parm, Name, idx(0), idx(1), idx(2), idx(3), idx(4), idx(5), refnum, "N/A", "N/A", pval
    write_parmrec fle, parm
    stored = True
  End If
  Store = True
End Function

Public Sub Help()
  MsgBox "The statement is true when checked.", 64, Label + " help!"
End Sub

Public Sub Add(item As Variant, Optional key As Variant)
  listeners.Add item, key
End Sub

Public Sub Remove(index As Variant)
  listeners.Remove index
End Sub
