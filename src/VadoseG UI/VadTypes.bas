Attribute VB_Name = "DataTypes"
Option Explicit

Type d_kdparm
  cas As String
  idx As Long
  kd(3) As String
  uunt(3) As String
  ref(3) As Long
End Type

Type kdparm
  cas As String
  idx As Long
  kd(3) As String
  uunt(3) As String
  ref(3) As Long
  numprog As Long
  progeny() As d_kdparm
End Type

Type daughter
  name As String
  cas As String
  koc(3) As Double
End Type

Type contam
  name As String
  cas As String
  rad As Boolean
  koc(3) As Double
  numprog As Long
  progeny() As daughter
End Type

