Attribute VB_Name = "Timemethods"
Option Explicit

Private Sub fillet(idx As Long, temp As parmrec)
  On Error Resume Next
  If temp.cunit <> "N/A" Then set_unit TimePts.unit(idx), temp.uunit
  TimePts.txt(idx).Text = convert(temp.cunit, temp.uunit, Val(temp.pval))
End Sub

Sub SetTFinal(parm As parmrec)
  fillet 0, parm
End Sub

Sub SetNTimes(parm As parmrec)
  fillet 1, parm
End Sub

Sub WriteTime(fle As parmfile)
Dim parm As parmrec
  set_parm parm, TimePts.txt(0).Tag, 0, 0, 0, 0, 0, 0, 0, TimePts.unit(0).Text, TimePts.unit(0).Tag, convert(TimePts.unit(0).Text, TimePts.unit(0).Tag, Val(TimePts.txt(0).Text))
  write_parmrec fle, parm
  If TimePts.er(0) Then PutError "Parameter " & TimePts.txt(0).Tag & " is invalid"
  set_parm parm, TimePts.txt(1).Tag, 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CInt(Val(TimePts.txt(1).Text))
  write_parmrec fle, parm
  If TimePts.er(1) Then PutError "Parameter " & TimePts.txt(1).Tag & " is invalid"
End Sub

Sub SetTimeTitle(title As String)
  Load TimePts
  TimePts.Caption = title
  get_conversion_items TimePts.unit(0).Tag, TimePts.unit(0)
End Sub
