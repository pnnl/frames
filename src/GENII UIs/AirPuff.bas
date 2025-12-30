Attribute VB_Name = "DataTypes"
Option Explicit
Option Compare Text

Type desc
  name As String
  idx As Long
  kind As Boolean
End Type

Type sigvals
    Value As Double
    cunit As String
    uunit As String
    ref As Long
End Type

Type srcvals
    name As String
    riseflg As Long
    sigr As sigvals
    sigz As sigvals
End Type

Sub updategauge()
  Loading.Gauge1.Value = Loading.Gauge1.Value + 1
  Loading.Label1 = "Loading ... " + Format(Loading.Gauge1.Value / Loading.Gauge1.Max * 100, "###") + "%"
  Loading.Refresh
End Sub
