Attribute VB_Name = "Module1"
Public Const MAXGROUP = 6
Public Const MAXPATH = 23

Public Type ProPopRec
  production As Double
  population(6) As Long
End Type

Public ProPops(MAXPATH) As ProPopRec

