Attribute VB_Name = "DataTypes"
Option Explicit
Global haveflux As Boolean
Global moist As String          'the moisture type of the output
Global dtset As String          'the data set type
Global outname As String        'name of the output

Type polygon
  area As Double
  dx As Double
  dy As Double
  numvtx As Long
  x() As Double
  y() As Double
  z() As Double
End Type

Type fluxrec
  time As Double
  flux(4) As Double
End Type

Type progflux
  ref As Long
  name As String
  cas As String
  use As Boolean
  unit(5) As String
  nts As Long
  series() As fluxrec
End Type

Type parentflux
  ref As Long
  cas As String
  name As String
  use As Boolean
  unit(5) As String
  nts As Long
  series() As fluxrec
  numprog As Long
  prog() As progflux
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

Type location
  id As String
  media As String
  use As Boolean
  numcon As Long
  con() As parentflux
  pnt(6) As String
  unt(6) As String
  val(6) As String
  ref(6) As Long
End Type

Global poly As polygon
Global sink(4) As location
Global f_numcon As Long, con() As parent
