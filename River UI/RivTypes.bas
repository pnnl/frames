Attribute VB_Name = "DataTypes"
Option Explicit

' Module-specific indexes
'  UI element indexes
Public Const VELOCITY_IDX = 0
Public Const DEPTH_IDX = 1
Public Const WIDTH_IDX = 2
Public Const DISTANCE_IDX = 3
Public Const AVG_DISCHARGE_IDX = 4
Public Const YCOORD_IDX = 7
Public Const XCOORD_IDX = 8
Public Const ZCOORD_IDX = 9
Public Const CONTAM_PARAMS = 5
Public Const PROGENY_PARAMS = 6

'  locparm structure param indexes
Public Const ENTRY_POINT_WIDTH = 0
Public Const LOCATION_DIST = 1
Public Const AVG_DISCHARGE = 2
Public Const YCOORD = 3
Public Const XCOORD = 4
Public Const ZCOORD = 5

'  contaminant parameter list indexes
Public Const WATER_SOLUBILITY_IDX = 0
Public Const HALF_LIFE_IDX = 1
Public Const MW_IDX = 2
Public Const MAX_CONTAM_PARAM = 2

Type measuredval
  value As String
  uunt As String
  punt As String
  ref As Long
End Type

Type locparm
  name As String
  label As String
  idx As Long
  kind As Long
  parms(5) As measuredval
End Type

Type snk
  lbl As String
  name As String
  type As String
  qual As String
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

Public numloc As Long
Public loc() As snk
Public model() As model_contam_param
Public con() As contam_param

Public m_numcon As Long

