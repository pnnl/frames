Attribute VB_Name = "ExpFunctions"
Option Explicit
Option Compare Text

' constants for soil composition
Global Const fSAND = 1
Global Const fSILT = 2
Global Const fCLAY = 3
Global Const fOMC = 4
Global Const fIRON = 5
Global Const fPH = 6

Function Kd_DatabaseValue(ModelIndex As Long, Optional ModelProgenyIndex As Long = -1) As Double
  
  ' Fail safe return value
  Kd_DatabaseValue = -1
  
  If ModelProgenyIndex >= 0 Then
    Kd_DatabaseValue = Val(con(model(ModelIndex).idx).progeny(model(ModelIndex).progeny(ModelProgenyIndex).idx).param(KD_IDX))
    Exit Function
  End If
  
  If ModelIndex > 0 Then
    Kd_DatabaseValue = Val(con(model(ModelIndex).idx).param(KD_IDX))
  End If
  
End Function

Function Kd_SoilEquationCalc(koc#, organic#, clay#, silt#, sand#) As Double
  Kd_SoilEquationCalc = 0.0001 * koc * (57.735 * organic + 2# * clay + 0.4 * silt + 0.005 * sand)
End Function

Function Kd_EquationValue(ModelIndex As Long, organic$, clay$, silt$, sand$, Optional ModelProgenyIndex As Long = -1) As Double
  
  ' Fail safe return value
  Kd_EquationValue = -1
  
  If Kdn_Index(False) > 0 Then
    If ModelProgenyIndex >= 0 Then
      Kd_EquationValue = Kd_SoilEquationCalc(Val(con(model(ModelIndex).idx).progeny(model(ModelIndex).progeny(ModelProgenyIndex).idx).param(KOC_IDX)), Val(organic), Val(clay), Val(silt), Val(sand))
      Exit Function
    End If
    Kd_EquationValue = Kd_SoilEquationCalc(Val(con(model(ModelIndex).idx).param(KOC_IDX)), Val(organic), Val(clay), Val(silt), Val(sand))
  End If

End Function

Function Kd_LookupTableValue(ModelIndex As Long, Optional ModelProgenyIndex As Long = -1) As Double
  
  ' Fail safe return value
  Kd_LookupTableValue = -1
  
  If ModelProgenyIndex >= 0 Then
    Kd_LookupTableValue = get_kd(con(model(ModelIndex).idx).progeny(model(ModelIndex).progeny(ModelProgenyIndex).idx).cas, Kdn_Index())
    Exit Function
  End If
  
  If ModelIndex > 0 Then
    Kd_LookupTableValue = get_kd(con(model(ModelIndex).idx).cas, Kdn_Index())
  End If

End Function

Sub LoadSoilParams()
  vfProcessUserInput = False
  
  frmSoilTri.txt(1) = soil.sand
  frmSoilTri.txt(2) = soil.silt
  frmSoilTri.txt(3) = soil.clay
  frmSoilTri.txt(4) = soil.organic
  frmSoilTri.txt(5) = soil.iron
  frmSoilTri.txt(6) = soil.ph
  
  frmSoilTri.lstSoils.ListIndex = soil.Index
  
  vfProcessUserInput = True
End Sub

Function Kdn_Index(Optional IncludepH As Boolean = True) As Integer
  Dim sum As Double
  
  ' Fail safe return value
  Kdn_Index = -1
  
  sum = Val(soil.sand) + Val(soil.silt) + Val(soil.clay) + Val(soil.organic) + Val(soil.iron)
  If sum > 100.1 Or sum < 99.9 Then
    Exit Function
  ElseIf IncludepH And (Val(soil.ph) < 1 Or Val(soil.ph) > 14) Then
    Exit Function
  Else
    sum = Val(soil.clay) + Val(soil.organic) + Val(soil.iron)
    If Val(soil.ph) >= 9# Then
      If sum >= 30# Then
        Kdn_Index = 3
      Else
        If sum >= 10# Then
          Kdn_Index = 2
        Else
          Kdn_Index = 1
        End If
      End If
    Else
      If Val(soil.ph) > 5# Then
        If sum >= 30# Then
          Kdn_Index = 6
        Else
          If sum >= 10# Then
            Kdn_Index = 5
          Else
            Kdn_Index = 4
          End If
        End If
      Else
        If sum >= 30# Then
          Kdn_Index = 9
        Else
          If sum >= 10# Then
            Kdn_Index = 8
          Else
            Kdn_Index = 7
          End If
        End If
      End If
    End If
  End If

End Function

Public Sub SetSoilParams(soilclass%, sand$, silt$, clay$, omc$, iron$, ph$)
  
  soil.Index = soilclass
  soil.sand = sand
  soil.silt = silt
  soil.clay = clay
  soil.organic = omc
  soil.iron = iron
  soil.ph = ph
  
  Exposure.KdListUpdate
  Exposure.ProgenyKdListUpdate

End Sub

