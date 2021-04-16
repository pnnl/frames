Attribute VB_Name = "DataTypes"
Option Explicit

Type Entry
  valu() As Double
  num As Long
End Type

Type Exposure
  route As String
  Path As String
  ref As Long
  use As Long
  numseries As Long
  series() As Entry
  cunit(2) As String
End Type

Type sProgeny
  cas As String
  name As String
  kind As Long
  ref As Long
  use As Long
  numexp As Long
  exp() As Exposure
End Type

Type sParent
  cas As String
  name As String
  kind As Long
  ref As Long
  use As Long
  time() As Double
  tunit(2) As String
  numexp As Long
  exp() As Exposure
  numprog As Long
  prog() As sProgeny
  numtime As Long
  numseries As Long
End Type

Type Medium
  kind As Long
  name As String
  ref As Long
  use As Long
  dur As Double
  dunit(2) As String
  numloc As Long
  locx() As Double
  locy() As Double
  lunits(2) As String
  numchem As Long
  chem() As sParent
End Type

Type progeny
  name As String
  cas As String
  kind As Long
End Type

Type parent
  name As String
  cas As String
  kind As Long
  numprog As Long
  prog() As progeny
End Type

Global numepf As Long
Global med() As Medium
Global prevtime As Double
Global bnflag As Boolean
Global numcon As Long
Global con() As parent
