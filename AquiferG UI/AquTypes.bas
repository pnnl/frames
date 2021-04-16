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
  koc(3) As Double
  rad As Boolean
  numprog As Long
  progeny() As daughter
End Type
  
Type desc
  name As String
  idx As Long
End Type

Type locvals
  org As String
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
