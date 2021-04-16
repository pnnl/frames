VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Float"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Dim refreshing As Boolean
Private lbl As Object
Private txt As Object
Private unt As Object
Private rf As Object
Private listeners As New Collection
Private default As Double
Private orgname As String
Private orglbl As String
Private orgval As Double
Private orgunt As String
Private orgpunt As String
Private orgref As Long
Private upper As Variant
Private lower As Variant
Private colour As Variant
Private ind(5) As Long
Private stored As Boolean
Const red = &HC0C0FF 'red
Const green = &HC0FFC0 'green
Public Islogical As Boolean

Sub Init(n As String, c As String, l As Object, t As Object, u As Object, r As Object, defUnit As String, lb As Variant, ub As Variant, defVal As Double, ParamArray ix())
Dim j As Long
  Set lbl = l
  Set txt = t
  Set unt = u
  Set rf = r
  For j = LBound(ix) To UBound(ix)
    ind(j) = ix(j)
  Next
  lbl.Tag = n + Format(ind(0))
  default = defVal
  orgval = defVal
  Label = c
  Name = n
  Text = defVal
  pUnit = defUnit
  Unit = defUnit
  LoEnd = lb
  HiEnd = ub
  Change
  stored = False
  Islogical = False
End Sub

Public Property Get Visible()
  Visible = txt.Visible
End Property

Public Property Let Visible(vNewValue)
  lbl.Visible = vNewValue
  txt.Visible = vNewValue
  unt.Visible = vNewValue
  rf.Visible = vNewValue
End Property

Public Property Get Enabled()
  Enabled = txt.Enabled
End Property

Public Property Let Enabled(vNewValue)
  lbl.Enabled = vNewValue
  txt.Enabled = vNewValue
  unt.Enabled = vNewValue
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
  txt.Tag = vNewValue
  orgname = vNewValue
End Property

Public Property Get Label()
  Label = orglbl
End Property

Public Property Let Label(vNewValue)
  lbl.Caption = vNewValue
  orglbl = vNewValue
End Property

Public Property Get Text()
  Text = orgval
End Property

Public Property Let Text(vNewValue)
  orgval = Val(vNewValue)
  If orgval > 100000 Or orgval < 0.001 And orgval <> 0 Then
     txt.Text = Format(orgval, "Scientific")
  Else
     txt.Text = Format(orgval, "#####0.0######")
  End If
End Property

Public Property Get Unit()
  Unit = orgunt
End Property

Public Property Let Unit(vNewValue)
  orgunt = vNewValue
  set_unit unt, orgunt
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
  rf.Tag = vNewValue
  orgref = vNewValue
  rf.Caption = "Ref: " + Str(orgref)
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

Public Property Get pUnit()
  pUnit = orgpunt
End Property

Public Property Let pUnit(vNewValue)
  orgpunt = vNewValue
  unt.Tag = vNewValue
  unt.Clear
  If vNewValue <> "N/A" Then get_conversion_items unt.Tag, unt
End Property

Public Property Get LoEnd()
  If VarType(lower) = vbObject Then
    LoEnd = lower.pval
    If LoEnd = "" Or Not lower.InRange Then LoEnd = lower.LoEnd
  Else
    LoEnd = lower
  End If
End Property

Public Property Let LoEnd(vNewValue)
   If VarType(vNewValue) = vbObject Then
     Set lower = vNewValue
   Else
     lower = vNewValue
   End If
End Property

Public Property Get HiEnd()
  If VarType(upper) = vbObject Then
    HiEnd = upper.pval
    If HiEnd = "" Or Not upper.InRange Then HiEnd = upper.HiEnd
  Else
    HiEnd = upper
  End If
End Property

Public Property Let HiEnd(vNewValue)
   If VarType(vNewValue) = vbObject Then
     Set upper = vNewValue
   Else
     upper = vNewValue
   End If
End Property

Public Property Get Color()
  Color = colour
End Property

Public Property Let Color(vNewValue)
  txt.BackColor = vNewValue
  colour = vNewValue
End Property

Public Function InRange()
  If Color = red Then
    InRange = False
  Else
    InRange = True
  End If
End Function

Public Sub Refresh()
  refreshing = True
  Color = colour
  lbl.Tag = orgname + Format(ind(0))
  Label = orglbl
  Text = orgval
  pUnit = orgpunt
  Unit = orgunt
  refnum = orgref
  refreshing = False
End Sub

Public Function Store()
Dim parm As parmrec
  If Not stored Then
    set_parm parm, Name, idx(0), idx(1), idx(2), idx(3), idx(4), idx(5), refnum, Unit, pUnit, pval
    write_parmrec fle, parm
    stored = True
  End If
  Store = InRange
End Function

Public Sub Change()
Dim v1 As Double
On Error GoTo CDblError
  v1 = CDbl(convert(Unit, pUnit, CDbl(txt.Text)))
  If LoEnd <= v1 And v1 <= HiEnd Then
    Color = green
    orgval = CDbl(txt.Text)
  Else
    orgval = CDbl(txt.Text)
CDblError:
    Color = red
  End If
End Sub

Public Sub Click()
  If Not refreshing Then
    orgunt = unt.Text
    Change
  End If
End Sub

Public Sub Help()
  Dim msg As String
  Dim ut As String
  ut = Unit
  If ut = "N/A" Then ut = ""
  If VarType(lower) = vbObject And VarType(upper) = vbObject Then
    msg = lower.Label + " gives this parameter its lower limit" + Chr(10)
    msg = msg + upper.Label + " gives this parameter its upper limit" + Chr(10)
    msg = msg + "The parameter must range from " + convert(pUnit, Unit, LoEnd) + " to " + convert(pUnit, Unit, HiEnd) + " " + ut
  ElseIf VarType(lower) = vbObject Then
    msg = upper.Label + " gives this parameter its lower limit" + Chr(10)
    msg = msg + "The parameter must range from " + convert(pUnit, Unit, LoEnd) + " to " + convert(pUnit, Unit, HiEnd) + " " + ut
  ElseIf VarType(upper) = vbObject Then
    msg = upper.Label + " gives this parameter its upper limit" + Chr(10)
    msg = msg + "The parameter must range from " + convert(pUnit, Unit, LoEnd) + " to " + convert(pUnit, Unit, HiEnd) + " " + ut
  Else
    msg = "The parameter must range from " + convert(pUnit, Unit, LoEnd) + " to " + convert(pUnit, Unit, HiEnd) + " " + ut
  End If
  MsgBox msg, 64, Label + " help!"
End Sub

Public Function pval() As String
  pval = convert(Unit, pUnit, orgval)
End Function

Public Sub Add(item As Variant, Optional key As Variant)
  listeners.Add item, key
End Sub

Public Sub Remove(index As Variant)
  listeners.Remove index
End Sub
