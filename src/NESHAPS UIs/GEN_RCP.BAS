Attribute VB_Name = "DataTypes"
Option Explicit
Option Compare Text

Global fle As parmfile
Global temp As parmrec

Type raddaughter
  cas As String
  idx As Long
  perm As String
  absf As String
  pkunt As String
  afunt As String
  pkref As Long
  afref As Long
End Type

Type radparent
  cas As String
  idx As Long
  perm As String
  absf As String
  pkunt As String
  afunt As String
  pkref As Long
  afref As Long
  numprog As Long
  progeny() As raddaughter
End Type

Type daughter
  Name As String
  cas As String
  kind As Long
  perm As Double
  absf As Double
End Type

Type parent
  Name As String
  cas As String
  kind As Double
  perm As Double
  absf As Double
  numprog As Long
  progeny() As daughter
End Type

Sub updategauge()
  Loading.Gauge1.Value = Loading.Gauge1.Value + 1
  Loading.Label1 = "Loading ... " + Format(Loading.Gauge1.Value / Loading.Gauge1.Max * 100, "###") + "%"
  Loading.Refresh
End Sub
