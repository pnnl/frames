Attribute VB_Name = "CNVRT_IO"
Option Explicit
Option Compare Text

Type cnvrtrec
  f_unit As String
  Factor As Double
  t_unit As String
  Measure As String
End Type

Global CVTFormat As String

Private CVT_loaded As Long
Private CVT_count As Long
Private CVT_table() As cnvrtrec

Function convert(from As String, too As String, pval As Double) As String
  Dim i As Integer
  Dim tval As Double
  Dim cnvrt As String
  Dim chk As Double
'  Dim doFormat As Boolean
  
  convert = pval
  If Not CVT_loaded Then Exit Function
'  doFormat = False
  convert = "0"
  
  ' BLH 9/87 handle special cases
  '    also moved formatting to one location with boolean to control it
  
  If (from = "C" Or from = "F") And (too = "C" Or too = "F") Then
  ' between Centigrade and Fahrenheit
    If (from = too) Then
      tval = pval
    Else
      If (from = "C" And too = "F") Then
      tval = (pval * (9# / 5#)) + 32
      Else
      tval = (pval - 32#) * (5# / 9#)
      End If
    End If
'  doFormat = True
  Else
    If StrComp(from, too) = 0 Then
      tval = pval
    '  doFormat = True
    Else
      For i = 1 To CVT_count
        If StrComp(from, CVT_table(i).f_unit) = 0 And StrComp(too, CVT_table(i).t_unit) = 0 Then
          tval = pval * CVT_table(i).Factor
          chk = tval / CVT_table(i).Factor
          If Abs(pval - chk) < 0.001 Then
            cnvrt = CStr(tval)
            If (InStr(cnvrt, ".999999") > 0) Then
              tval = CDbl(Left(cnvrt, InStr(cnvrt, ".99999")) + "0") + 1
            End If
            If (InStr(convert, ".000000") > 0) Then
              tval = CDbl(Left(cnvrt, InStr(cnvrt, ".00000")) + "0")
            End If
          End If
          Exit For
      '    doFormat = True
        Else
          If StrComp(from, CVT_table(i).t_unit) = 0 And StrComp(too, CVT_table(i).f_unit) = 0 Then
            tval = pval / CVT_table(i).Factor
            chk = tval * CVT_table(i).Factor
            If Abs(pval - chk) < 0.001 Then
              cnvrt = CStr(tval)
              If (InStr(cnvrt, ".999999") > 0) Then
                tval = CDbl(Left(cnvrt, InStr(cnvrt, ".99999")) + ".0") + 1
              End If
              If (InStr(convert, ".000000") > 0) Then
                tval = CDbl(Left(cnvrt, InStr(cnvrt, ".00000")) + ".0")
              End If
            End If
            Exit For
        '    doFormat = True
          End If
        End If
      Next
    End If
  End If
'  If doFormat Then
'   If Abs(tval) > 100000 Or Abs(tval) < 0.001 And pval <> 0 Then
'   convert = Format(tval, "0.0##E+00")
'   Else
'   convert = Format(tval, "#####0.0###")
'   End If
  convert = Format(tval, CVTFormat)
'  End If
End Function

'get conversion type strings and load into dropdown control
Function get_conversion_items_by_type(UnitType As String, ctrl As Control) As Long
Dim i As Long
  
  If CVT_loaded Then
    For i = 1 To CVT_count
      If StrComp(UnitType, CVT_table(i).Measure) = 0 Then
        set_unit ctrl, CVT_table(i).f_unit, -1
        If ctrl.ListIndex < 0 Then ctrl.AddItem CVT_table(i).f_unit
      End If
    Next
    ctrl.ListIndex = -1
    get_conversion_items_by_type = ctrl.ListCount
  End If
End Function

'get conversion type strings and load into dropdown control
'sets default to units to the unit passed in
Function get_conversion_items(unit As String, ctrl As Control) As Long
Dim i As Long
  
  ctrl.AddItem unit
  ctrl.ListIndex = 0
  If CVT_loaded Then
    For i = 1 To CVT_count
      If StrComp(unit, CVT_table(i).f_unit) = 0 Then
        ctrl.AddItem CVT_table(i).t_unit
      Else
        If StrComp(unit, CVT_table(i).t_unit) = 0 Then
          ctrl.AddItem CVT_table(i).f_unit
        End If
      End If
    Next
    get_conversion_items = ctrl.ListCount
  End If
End Function

'get conversion type strings and put
'it into tab delimited string "ulist"
Function get_conversion_list(unit As String, ulist As String) As Long
Dim i As Integer
Dim cnt As Long
  
  ulist = unit
  cnt = 1
  If CVT_loaded Then
    For i = 1 To CVT_count
      If StrComp(unit, CVT_table(i).f_unit) = 0 Then
        cnt = cnt + 1
        ulist = ulist & Chr$(9) & CVT_table(i).t_unit
      Else
        If StrComp(unit, CVT_table(i).t_unit) = 0 Then
          cnt = cnt + 1
          ulist = ulist & Chr$(9) & CVT_table(i).f_unit
        End If
      End If
    Next
    get_conversion_list = cnt
  End If
End Function

Function get_conversion_units(Measure As String) As Collection
Dim i As Integer
Dim unit As New Collection
  
  If CVT_loaded Then
    For i = 1 To CVT_count
      If StrComp(Measure, CVT_table(i).Measure) = 0 Then
On Error Resume Next
        unit.Add CVT_table(i).t_unit, CVT_table(i).t_unit
        unit.Add CVT_table(i).f_unit, CVT_table(i).f_unit
On Error GoTo 0
      End If
    Next
    Set get_conversion_units = unit
  End If
End Function

'get conversion type strings and load into dropdown control
Function get_conversion_measures() As Collection
Dim i As Long
Dim measures As New Collection

  If CVT_loaded Then
    For i = 1 To CVT_count

On Error Resume Next
      measures.Add CVT_table(i).Measure, CVT_table(i).Measure
On Error GoTo 0
    
    Next
    Set get_conversion_measures = measures
  End If
End Function

Sub load_convert()
Dim fle As csv
Dim fName As String
Dim id As String
Dim cnt As Long
Dim returned As String
  
  If CVT_loaded Then Exit Sub
  FRAMES_INI = App.Path + "\\FramesUI.ini"
  returned = Space$(256)
  GetPrivateProfileString "App Path", "FUI", App.Path, returned, 256, FRAMES_INI
  returned = StripTerminator(returned)
  CVTFormat = "General Number"
  fName = Trim(returned) & "\CONVERT.CSV"
  
  If open_csv(fle, fName, 2) Then
    id = get_val(fle)
    CVT_count = val(get_val(fle))
    get_line fle
    If id = "CVT" Then
      ReDim Preserve CVT_table(CVT_count) As cnvrtrec
      For cnt = 1 To CVT_count
        read_cnvrtrec fle, CVT_table(cnt)
      Next
      close_csv fle
      If CVT_count > 0 Then
        CVT_loaded = True
      Else
        MsgBox "Error in file:" & UCase(fName) & Chr$(10) & "Values must be entered in stated units.", 64, "Unit conversion error!"
        CVT_loaded = False
      End If
    Else
      MsgBox "Not a convert file:" & UCase(fName) & Chr$(10) & "Values must be entered in stated units.", 64, "Unit conversion error!"
      CVT_loaded = False
    End If
  Else
    MsgBox "Unable to locate or open:" & UCase(fName) & Chr$(10) & "Values must be entered in stated units.", 64, "Unit conversion error!"
    CVT_loaded = False
  End If
End Sub

Function read_cnvrtrec(fle As csv, temp As cnvrtrec) As Long
  temp.f_unit = get_val(fle)
  temp.Factor = val(get_val(fle))
  temp.t_unit = get_val(fle)
  temp.Measure = get_val(fle)
  get_line fle
  read_cnvrtrec = True
  If temp.f_unit = "" Or temp.Factor = 0 Or temp.t_unit = "" Then read_cnvrtrec = False
End Function

Function listIndexOf(list As Control, item As String) As Long
  Dim i As Long
  
  ' fail-safe return value
  listIndexOf = -1
  
  For i = 0 To list.ListCount - 1
    If list.list(i) = item Then Exit For
  Next i
  
  If i < list.ListCount Then listIndexOf = i
  
End Function

' set_unit()
'   sets a list control selection to a given string, with optional default
'   index selected, if specified, or the 0th item selected otherwise
'
'------
' NOTE:
'------
'
' Use default_index with care.  In particular, do not choose an index which
' depends on the order of items in the loaded conversion table, as this ordering
' may change at any time.
'
' For most uses, either a default_index of -1 (no selection) or 0 (if a default unit
' is explicitly listed first in the list) are the two most logical values to use.
'
Sub set_unit(ctrl As Control, unit As String, Optional default_index As Long)
  Dim i As Long

  i = listIndexOf(ctrl, unit)
  
  If i >= 0 Then
    ctrl.ListIndex = i
  ElseIf IsMissing(default_index) Then
    If ctrl.ListCount > 0 Then ctrl.ListIndex = 0
  ElseIf default_index < ctrl.ListCount Then
    ctrl.ListIndex = default_index
  End If
  
End Sub

Sub SetFormat(frm As Object)
  frm.NumFixed.Checked = False
  frm.NumGeneral.Checked = False
  frm.NumStandard.Checked = False
  frm.NumScientific.Checked = False
  frm.NumUser.Checked = False
  Select Case CVTFormat
  Case "Standard"
    frm.NumStandard.Checked = True
  Case "Fixed"
    frm.NumFixed.Checked = True
  Case "General Number"
    frm.NumGeneral.Checked = True
  Case "Scientific"
    frm.NumScientific.Checked = True
  Case Else
    frm.NumUser.Checked = True
  End Select
End Sub

Function CheckUserDefinedFormat(UserStr As String) As Boolean
'check that user inputs an acceptable conversion format
Dim ChkStr As String
Dim ChkChar As String
Dim counter As Integer
Dim TripCounter As Integer

  TripCounter = 0
  For counter = 1 To Len(UserStr)
    ChkChar = Mid$(UserStr, counter, 1)
    If Not ChkChar = "0" Then
      If Not ChkChar = "#" Then
      If Not (ChkChar = "e" Or ChkChar = "E") Then
        If Not ChkChar = "+" Then
        If Not ChkChar = "." Then
          MsgBox ChkChar + " is not a valid format character."
          CheckUserDefinedFormat = False
          Exit Function
        Else
          TripCounter = TripCounter + 1
          If TripCounter > 1 Then
            MsgBox "Format invalid. Only one decimal allowed"
            CheckUserDefinedFormat = False
            Exit Function
          End If
        End If
        End If
      Else
        ChkStr = Mid$(UserStr, counter, Len(UserStr))
        If Not (ChkStr = "E+00" Or ChkStr = "E+0") Then
          MsgBox "Format invalid.  Valid entries include E+0 or E+00."
          CheckUserDefinedFormat = False
          Exit Function
        End If
      End If
      End If
    End If
  Next counter
  CheckUserDefinedFormat = True
End Function
