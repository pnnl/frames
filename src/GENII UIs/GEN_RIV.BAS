Attribute VB_Name = "DataTypes"
Option Explicit
Option Compare Text

Type locvals
  org As Double
  uunt As String
  punt As String
  ref As Long
End Type

Type locparm
  name As String
  label As String
  idx As Long
  parms(6) As locvals
End Type

Type snk
  lbl As String
  name As String
  type As String
  qual As String
End Type

Sub updategauge()
  Loading.Gauge1.Value = Loading.Gauge1.Value + 1
  Loading.Label1 = "Loading ... " + Format(Loading.Gauge1.Value / Loading.Gauge1.Max * 100, "###") + "%"
  Loading.Refresh
End Sub
