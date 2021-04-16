VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "tInteger"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Private myRefresh
Private lbl As Object
Private txt As Object
Private unt As Object
Private ref As Object
Private tTag As String
Private tName As String
Private tPrompt As String
Private tUserValue As Long
Private tDefaultValue As Long
Private tUserUnit As String
Private tDefaultUnit As String
Private tReference As Long
Private tUpper As Variant
Private tLower As Variant
Private tColor As Variant
Private tEnabled As Boolean
Private tVisible As Boolean
Private tIndex(5) As Long
Private listeners As New Collection
Const RED = &H8080FF
Const GREEN = &HC0FFC0

Private Sub Class_Initialize()
  tUpper = 2147483647
  tLower = -tUpper
  tEnabled = True
  tVisible = True
  tUserUnit = "N/A"
  tDefaultUnit = tUserUnit
End Sub

'Public Sub Init(iName As String, iPrompt As String, iDefaultValue As Long, iDefaultUnit As String, iLower As Variant, iUpper As Variant, ParamArray iIndex())
'Dim j As Long
'  tName = iName
'  tPrompt = iPrompt
'  tUserValue = defVal
'  tDefaultValue = defVal
'  tUserUnit = defUnit
'  tDefaultUnit = defUnit
'  tLower = lb
'  tUpper = ub
'  For j = LBound(ix) To UBound(ix)
'    tIndex(j) = iIndex(j)
'  Next
'End Sub

Public Sub SetObj(tLbl As Object, tTxt As Object, tUnt As Object, tRef As Object)
  Set lbl = tLbl
  Set txt = tTxt
  Set unt = tUnt
  Set ref = tRef
End Sub

Public Property Get Visible()
  Visible = tVisible
End Property

Public Property Let Visible(vNewValue)
  tVisible = vNewValue
End Property

Public Property Get Enabled()
  Enabled = tEnabled
End Property

Public Property Let Enabled(vNewValue)
  tEnabled = vNewValue
End Property

Public Property Get tag()
  tag = tTag
End Property

Public Property Let tag(vNewValue)
  tTag = vNewValue
End Property

Public Property Get name()
  name = tName
End Property

Public Property Let name(vNewValue)
  tName = vNewValue
End Property

Public Property Get Prompt()
  Prompt = tPrompt
End Property

Public Property Let Prompt(vNewValue)
  tPrompt = vNewValue
End Property

Public Property Get UserValue()
  UserValue = tUserValue
End Property

Public Property Let UserValue(vNewValue)
  tUserValue = CLng(vNewValue)
End Property

Public Property Get DefaultValue()
  DefaultValue = tDefaultValue
End Property

Public Property Let DefaultValue(vNewValue)
  tDefaultValue = CLng(vNewValue)
End Property

Public Property Get UserUnit()
  UserUnit = tUserUnit
End Property

Public Property Let UserUnit(vNewValue)
  tUserUnit = vNewValue
End Property

Public Property Get DefaultUnit()
  DefaultUnit = tDefaultUnit
End Property

Public Property Let DefaultUnit(vNewValue)
  tDefaultUnit = vNewValue
End Property

Public Property Get Reference()
  Reference = tReference
End Property

Public Property Let Reference(vNewValue)
  tReference = vNewValue
End Property

Public Property Get lower()
  If VarType(tLower) = vbObject Then
    lower = tLower.pval(tDefaultUnit)
    If lower = "" Or Not tLower.InRange Then lower = tLower.lower
  Else
    lower = tLower
  End If
End Property

Public Property Let lower(vNewValue)
   If VarType(vNewValue) = vbObject Then
     Set tLower = vNewValue
   Else
     tLower = vNewValue
   End If
End Property

Public Property Get upper()
  If VarType(tUpper) = vbObject Then
    upper = tUpper.pval(tDefaultUnit)
    If upper = "" Or Not tUpper.InRange Then upper = tUpper.upper
  Else
    upper = tUpper
  End If
End Property

Public Property Let upper(vNewValue)
   If VarType(vNewValue) = vbObject Then
     Set tUpper = vNewValue
   Else
     tUpper = vNewValue
   End If
End Property

Public Property Get Index(i As Long)
  If LBound(tIndex) <= i And i <= UBound(tIndex) Then
    Index = tIndex(i)
  Else
    Index = 0
  End If
End Property

Public Property Let Index(i As Long, vNewValue)
  If LBound(tIndex) <= i And i <= UBound(tIndex) Then
    tIndex(i) = vNewValue
  End If
End Property

Public Sub Add(item As Variant, Optional key As Variant)
  listeners.Add item, key
End Sub

Public Sub Remove(Index As Variant)
  listeners.Remove Index
End Sub

Public Sub StatusListeners(vNewValue)
Dim i As Long
  For i = 1 To listeners.count
    listeners(i).Enabled = vNewValue
    listeners(i).StatusListeners vNewValue
  Next
End Sub

Public Sub Help()
  Dim msg As String
  Dim ut As String
  ut = tUserUnit
  If ut = "N/A" Then ut = ""
  If VarType(tLower) = vbObject And VarType(tUpper) = vbObject Then
    msg = tLower.Prompt + " gives this Name its lower limit" + Chr(10)
    msg = msg + tUpper.Prompt + " gives this Name its upper limit" + Chr(10)
    msg = msg + "The Name must range from " + convert(tDefaultUnit, tUserUnit, lower) + " to " + convert(tDefaultUnit, tUserUnit, upper) + " " + ut
  ElseIf VarType(tLower) = vbObject Then
    msg = tUpper.Prompt + " gives this Name its lower limit" + Chr(10)
    msg = msg + "The Name must range from " + convert(tDefaultUnit, tUserUnit, lower) + " to " + convert(tDefaultUnit, tUserUnit, upper) + " " + ut
  ElseIf VarType(upper) = vbObject Then
    msg = tUpper.Prompt + " gives this Name its upper limit" + Chr(10)
    msg = msg + "The Name must range from " + convert(tDefaultUnit, tUserUnit, lower) + " to " + convert(tDefaultUnit, tUserUnit, upper) + " " + ut
  Else
    msg = "The Name must range from " + convert(tDefaultUnit, tUserUnit, lower) + " to " + convert(tDefaultUnit, tUserUnit, upper) + " " + ut
  End If
  MsgBox msg, 64, Prompt + " help!"
End Sub

Public Sub Refresh()
  myRefresh = True
  lbl.Caption = tPrompt
  Text = tUserValue
  txt.tag = tTag
  ref.Caption = "Ref: " + Str(tReference)
  ref.tag = tReference
  If tDefaultUnit <> "N/A" Then
    unt.Clear
    unt.tag = tTag
    get_conversion_items tDefaultUnit, unt
    set_unit unt, tUserUnit
    unt.Visible = tVisible
    unt.Enabled = tEnabled
  End If
  lbl.Visible = tVisible
  txt.Visible = tVisible
'  ref.Visible = tVisible
  lbl.Enabled = tEnabled
  txt.Enabled = tEnabled
  ref.Enabled = tEnabled
  Change
  myRefresh = False
End Sub

Private Property Let Text(vNewValue)
Dim value As Double
    value = val(vNewValue)
    If value > 100000 Or value < -100000 Then
     txt.Text = Format(value, "Scientific")
    Else
     txt.Text = Format(value, "######0")
    End If
End Property


Public Property Get Color()
  Color = tColor
End Property

Public Property Let Color(vNewValue)
  tColor = vNewValue
  txt.BackColor = vNewValue
End Property

Public Function InRange()
  If Color = RED Then
    InRange = False
  Else
    InRange = True
  End If
End Function

'Public Sub ReadIt(p As parmrec)
'  tUserUnit = p.uunit
'On Error Resume Next
'  tUserValue = CLng(convert(tUserUnit, p.cunit, CDbl(p.pval)))
'End Sub

'Public Sub WriteIt(fle As parmfile)
'Dim parm As parmrec
'  set_parm parm, tName, tIndex(0), tIndex(1), tIndex(2), tIndex(3), tIndex(4), tIndex(5), tReference, tUserUnit, tDefaultUnit, CStr(CLng(convert(tUserUnit, tDefaultUnit, CDbl(tUserValue))))
'  write_parmrec fle, parm
'End Sub

Public Sub Change(Optional doconvert As Boolean = True)
Dim v1 As Long
  
    On Error GoTo CIntError
    If doconvert Then
      v1 = CLng(convert(tUserUnit, tDefaultUnit, CDbl(txt.Text)))
    Else
      v1 = CLng(txt.Text)
    End If
    If (lower <= v1 And v1 <= upper) Then
      Color = GREEN
      tUserValue = CLng(txt.Text)
    Else
      tUserValue = CLng(txt.Text)
CIntError:
      Color = RED
    End If
End Sub

Public Sub Click()
  If Not myRefresh Then
    tUserUnit = unt.Text
    Change
  End If
End Sub

Public Function pval(Optional tUnit As String = "") As String
  If tUnit = "" Then
    pval = convert(tUserUnit, tDefaultUnit, CDbl(tUserValue))
  Else
    pval = convert(tUserUnit, tUnit, CDbl(tUserValue))
  End If
End Function

