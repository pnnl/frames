Attribute VB_Name = "UNIT_IO"
Option Compare Text

Type UnitRec
  f_unit As String
  factor As Double
  t_unit As String
  u_type As String
End Type

'Dim temptype As String
Dim DistanceTime() As String
Dim VolumeTime() As String
Dim SolidSolid() As String
Dim SolidVolume() As String
Dim Distance() As String
Dim Time() As String
Dim Solid() As String
Dim Liquid() As String
Dim Activity() As String
Dim ActivitySolid() As String
Dim ActivityLiquid() As String
Dim AreaSolid() As String
Dim Miscellaneous() As String
Dim SolidLiquid() As String
Dim ActivityVolume() As String
Dim Temperature() As String
Dim Percentage() As String
Dim ActivityTime() As String



Function GetUnitType(unit_type As String) As String
  Dim fle As csv
  Dim temp As UnitRec
  Dim fname As String
  Dim Id As String
  Dim cnt As Integer
  Dim unitcnt As Integer
  Dim tempunit As UnitRec
  
  fname = App.Path & "\convert.CSV"
  If open_csv(fle, fname, 2) Then
    Id = get_val(fle)
    unitcnt = Val(get_val(fle))
    get_line fle
    If Id = "CVT" Then
      For cnt = 1 To unitcnt
        Read_Units fle, tempunit
        If StrComp(tempunit.u_type, unit_type) = 0 Then
          GetUnitType = tempunit.f_unit
          close_csv fle
          Exit Function
        End If
      Next
      GetUnitType = "N/A"
    Else
      GetUnitType = "BCSV"
    End If
  Else
    GetUnitType = "CSVNF"
  End If
  close_csv fle
End Function

Sub Read_Units(fle As csv, temp As UnitRec)
  temp.f_unit = get_val(fle)
  temp.factor = Val(get_val(fle))
  temp.t_unit = get_val(fle)
  temp.u_type = get_val(fle)
  get_line fle
End Sub
'********************************************************************************
'********************************************************************************

Public Sub SetArray(cntrl As Control, temparray() As String, searchunit As String)
  Dim unitcheck As Integer
  Dim unitstring As String
  Dim check As Boolean
  Dim i As Integer
  unitcheck = get_conversion_items(searchunit, cntrl)
  For i = 0 To cntrl.ListCount - 1
  ReDim Preserve temparray(i + 1) As String
    cntrl.ListIndex = i
    temparray(i) = cntrl.Text
  Next
  cntrl.Clear
End Sub

Public Sub InitUnits(ctrl As Control)
  load_convert
  SetArray ctrl, DistanceTime, "cm/s"
  SetArray ctrl, VolumeTime, "cm^3/s"
  SetArray ctrl, SolidSolid, "ug/ug"
  SetArray ctrl, SolidVolume, "ug/um^3"
  SetArray ctrl, Distance, "um"
  SetArray ctrl, Time, "sec"
  SetArray ctrl, Solid, "g"
  SetArray ctrl, Liquid, "l"
  SetArray ctrl, Activity, "Ci"
  SetArray ctrl, ActivityLiquid, "Ci/ml"
  SetArray ctrl, ActivitySolid, "Ci/g"
  SetArray ctrl, AreaSolid, "m^3/kg"
  SetArray ctrl, Miscellaneous, "risk/Sv"
  SetArray ctrl, SolidLiquid, "g/ml"
  SetArray ctrl, Temperature, "F"
  SetArray ctrl, Percentage, "%"
  SetArray ctrl, ActivityVolume, "Ci/cm^3"
  SetArray ctrl, ActivityTime, "Ci/yr"
End Sub

Public Sub SetComboIndex(temp As String, ctrl As Control)
  Dim i As Integer
  Dim tempindex As Integer
  tempindex = ctrl.ListIndex
  If temp = "N/A" Or temp = "" Then
    ctrl.ListIndex = 0
    Exit Sub
  End If
  For i = 0 To ctrl.ListCount - 1
    ctrl.ListIndex = i
    If temp = ctrl.Text Then
      Exit Sub
    ElseIf (temp = "year" Or temp = "years") And ctrl.Text = "yr" Then
      Exit Sub
    End If
  Next
End Sub
Public Sub InsertUnits(temp() As String, ctrl As Control)
  Dim i As Integer
  For i = 0 To ArraySize(temp) - 1
    ctrl.AddItem temp(i)
  Next
End Sub

Public Sub LoadUnits(temp As String, ctrl As Control)
  ctrl.Clear
  Select Case temp
    Case "Distance / Time"
      InsertUnits DistanceTime, ctrl
    Case "Volume / Time"
      InsertUnits VolumeTime, ctrl
    Case "Solid / Solid"
      InsertUnits SolidSolid, ctrl
    Case "Solid / Volume"
      InsertUnits SolidVolume, ctrl
    Case "Distance"
      InsertUnits Distance, ctrl
    Case "Time"
      InsertUnits Time, ctrl
    Case "Solid"
      InsertUnits Solid, ctrl
    Case "Liquid"
      InsertUnits Liquid, ctrl
    Case "Activity"
      InsertUnits Activity, ctrl
    Case "Activity / Solid"
      InsertUnits ActivitySolid, ctrl
    Case "Activity / Liquid"
      InsertUnits ActivityLiquid, ctrl
    Case "Area / Solid"
      InsertUnits AreaSolid, ctrl
    Case "Miscellaneous"
      InsertUnits Miscellaneous, ctrl
    Case "Solid / Liquid"
      InsertUnits SolidLiquid, ctrl
    Case "Activity / Volume"
      InsertUnits ActivityVolume, ctrl
    Case "Temperature"
      InsertUnits Temperature, ctrl
    Case "Percentage"
      InsertUnits Percentage, ctrl
    Case "Activity / Time"
      InsertUnits ActivityTime, ctrl
    Case "N/A"
      ctrl.AddItem "N/A"
    Case ""
      ctrl.AddItem "N/A"
  End Select
End Sub

Public Function ArraySize(temparray() As String) As Long
  Dim lower As Long
  Dim upper As Long
  ArraySize = UBound(temparray) - LBound(temparray)
End Function

Public Function IsType(temp As String, temparray() As String) As Boolean
  Dim i As Integer
  For i = 0 To ArraySize(temparray) - 1
'    If temparray(i) = temp Then
    If StrComp(temparray(i), temp, 1) = 0 Then
      IsType = True
      Exit Function
    End If
  Next
  IsType = False
End Function

Public Function FindType(temp As String) As Integer
  If temp = "N/A" Or temp = "" Then
    FindType = 10
    Exit Function
  ElseIf IsType(temp, Activity) Then
    FindType = 0
    Exit Function
  ElseIf IsType(temp, ActivityLiquid) Then
    FindType = 1
    Exit Function
  ElseIf IsType(temp, ActivitySolid) Then
    FindType = 2
    Exit Function
  ElseIf IsType(temp, ActivityTime) Then
    FindType = 3
    Exit Function
  ElseIf IsType(temp, ActivityVolume) Then
    FindType = 4
    Exit Function
  ElseIf IsType(temp, AreaSolid) Then
    FindType = 5
    Exit Function
  ElseIf IsType(temp, Distance) Then
    FindType = 6
    Exit Function
  ElseIf IsType(temp, DistanceTime) Then
    FindType = 7
    Exit Function
  ElseIf IsType(temp, Liquid) Then
    FindType = 8
    Exit Function
  ElseIf IsType(temp, Miscellaneous) Then
    FindType = 9
    Exit Function
  ElseIf IsType(temp, Percentage) Then
    FindType = 12
    Exit Function
  ElseIf IsType(temp, Solid) Then
    FindType = 13
    Exit Function
  ElseIf IsType(temp, SolidLiquid) Then
    FindType = 14
    Exit Function
  ElseIf IsType(temp, SolidSolid) Then
    FindType = 15
    Exit Function
  ElseIf IsType(temp, SolidVolume) Then
    FindType = 16
    Exit Function
  ElseIf IsType(temp, Temperature) Then
    FindType = 17
    Exit Function
  ElseIf IsType(temp, Time) Then
    FindType = 18
    Exit Function
  ElseIf IsType(temp, VolumeTime) Then
    FindType = 19
    Exit Function
  Else
    FindType = 11
    temptype = temp
    Exit Function
  End If
End Function

