Attribute VB_Name = "DataTypes"
Option Explicit
Option Compare Text

Type desc
  name As String
  idx As Long
  kind As Boolean
End Type

Type srcvals
  name As String
  riseflg As Long
  dispflg As Long
  iscflg As Integer
  wakeflg As Long
  boundflg As Long
  bidflg As Long
  highflg As Long
  lowflg As Long
  bldhgt(36) As Double
  hgtref As Long
  hgtunit As String
  hgtcunit As String
  bldwdth(36) As Double
  wdthref As Long
  wdthunit As String
  wdthcunit As String
  bldarea(36) As Double
  arearef As Long
  areaunit As String
  areacunit As String
End Type

Sub updategauge()
  Loading.Gauge1.Value = Loading.Gauge1.Value + 1
  Loading.Label1 = "Loading ... " + Format(Loading.Gauge1.Value / Loading.Gauge1.Max * 100, "###") + "%"
  Loading.Refresh
End Sub
