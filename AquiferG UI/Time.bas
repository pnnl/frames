Attribute VB_Name = "Timemethods"
Option Explicit

Private Sub fillet(idx As Long, temp As parmrec)
  On Error Resume Next
  If temp.cunit <> "N/A" Then set_unit Time.unit(idx), temp.uunit
  Time.txt(idx).Text = convert(temp.cunit, temp.uunit, Val(temp.pval))
End Sub

Sub SetTimePt(parm As parmrec)
  Time.Option2.Value = True
  set_unit Time.unit(1), parm.uunit
  Time.vaSpread1.Col = 1
  Time.vaSpread1.Row = parm.idx1
  Time.vaSpread1.Text = parm.pval
End Sub

Sub SetTFinal(parm As parmrec)
  fillet 0, parm
End Sub

Sub SetNTimes(parm As parmrec)
  fillet 1, parm
End Sub

Sub WriteTime(fle As parmfile)
Dim parm As parmrec
Dim cnt As Long
Dim i As Long
  cnt = 0
  If Time.Option1.Value Then
    set_parm parm, Time.txt(0).Tag, 0, 0, 0, 0, 0, 0, 0, Time.unit(0).Text, Time.unit(0).Tag, convert(Time.unit(0).Text, Time.unit(0).Tag, Val(Time.txt(0).Text))
    write_parmrec fle, parm
    If Time.er(0) Then PutError "Parameter " & Time.txt(0).Tag & " is invalid"
    set_parm parm, Time.txt(1).Tag, 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CInt(Val(Time.txt(1).Text))
    write_parmrec fle, parm
    If Time.er(1) Then PutError "Parameter " & Time.txt(1).Tag & " is invalid"
  Else
    Time.vaSpread1.Col = 1
    For i = 1 To 50
      Time.vaSpread1.Row = i
      If Val(Time.vaSpread1.Text) > 0 Then
        cnt = cnt + 1
        set_parm parm, "TimePt", cnt, 0, 0, 0, 0, 0, 0, Time.unit(1).Text, Time.unit(1).Tag, convert(Time.unit(1).Text, Time.unit(1).Tag, Val(Time.vaSpread1.Text))
        write_parmrec fle, parm
      End If
    Next
  End If
  set_parm parm, "NumTimePt", 0, 0, 0, 0, 0, 0, 0, "", "", CStr(cnt)
  write_parmrec fle, parm
End Sub

Sub SetTimeTitle(title As String)
  Load Time
  Time.Caption = title
  get_conversion_items Time.unit(0).Tag, Time.unit(0)
  get_conversion_items Time.unit(1).Tag, Time.unit(1)
End Sub
