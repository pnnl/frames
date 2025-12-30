Attribute VB_Name = "DataTypes"
Option Explicit
Option Compare Text

Type d_casparm
  cas As String
  idx As Long
  txt(1) As String
  unt(1) As String
  ref(1) As Long
  sol As Long
End Type

Type casparm
  cas As String
  idx As Long
  txt(1) As String
  unt(1) As String
  ref(1) As Long
  sol As Long
  numprog As Long
  progeny() As d_casparm
End Type

Type daughter
  name As String
  cas As String
  lclass As String
End Type

Type contam
  name As String
  cas As String
  rad As Boolean
  lclass As String
  numprog As Long
  progeny() As daughter
End Type

