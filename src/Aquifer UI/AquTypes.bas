Attribute VB_Name = "DataTypes"
Option Explicit

' Module-specific indexes
'  UI element indexes
Public Const CONTAM_PARAMS = 24
Public Const PROGENY_PARAMS = 25
' Constants for uStr standard unit array
Public Const SOIL_KD = 0
Public Const RAD_SOLUBILITY = 1
Public Const SOLUBILITY = 2
Public Const HALF_LIFE = 3

' Constants for Kd value lookup
Public Const SELECT_LOOKUP = 0
Public Const DATABASE_LOOKUP = 1
Public Const EQUATION_LOOKUP = 2
Public Const TABLE_LOOKUP = 3

'  contaminant parameter list indexes
Public Const KD_IDX = 0
Public Const WATER_SOLUBILITY_IDX = 1
Public Const HALF_LIFE_IDX = 2
Public Const KOC_IDX = 3
Public Const MW_IDX = 4
Public Const MAX_CONTAM_PARAM = 4

Type measuredval
  value As String
  uunt As String
  punt As String
  ref As Long
End Type

Type model_progeny_param
  cas As String
  idx As Long
  param(MAX_CONTAM_PARAM) As measuredval
End Type

Type model_contam_param
  cas As String
  idx As Long
  param(MAX_CONTAM_PARAM) As measuredval
  numprog As Long
  progeny() As model_progeny_param
End Type

Type contam_progeny_param
  name As String
  cas As String
  param(MAX_CONTAM_PARAM) As String
End Type

Type contam_param
  name As String
  cas As String
  rad As Boolean
  param(MAX_CONTAM_PARAM) As String
  numprog As Long
  progeny() As contam_progeny_param
End Type
  
Type locparm
  name As String
  label As String
  idx As Long
  parms(6) As measuredval
End Type

Type snk
  lbl As String
  name As String
  type As String
  qual As String
End Type

Public loc() As snk
Public model() As model_contam_param
Public con() As contam_param

Public numloc As Long
Public m_numcon As Long


