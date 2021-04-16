Attribute VB_Name = "DataTypes"
Option Explicit
Global usedcustom1 As Boolean   'lets exposure know whether or not customization
Global usedpopair As Boolean    'lets exposure know whether or not population data entered
Global poptot As Long

' Module-specific indexes
'  UI element indexes
Public Const GW_EXP_DURATION = 0
Public Const GW_DOMESTIC_DIST = 1
Public Const GW_IRR_FRACTION = 2
Public Const GW_IRR_RATE = 3
Public Const SW_EXP_DURATION = 4
Public Const SW_FISH_CORR = 5
Public Const SW_DOMESTIC_DIST = 6
Public Const SW_IRR_FRACTION = 7
Public Const SW_IRR_RATE = 8
Public Const SW_FIN_DELAY = 9
Public Const SW_SHELL_DELAY = 10
Public Const SW_DEPOSITION_THICK = 11
Public Const SW_DEPOSITION_DENSITY = 12
Public Const AT_DEPOSITION_THICK = 14
Public Const AT_DEPOSITION_DENSITY = 15
Public Const AT_EXP_DURATION = 23
Public Const SO_EXP_DURATION = 25
Public Const KD_SOIL_THICK = 31
Public Const KD_SOIL_MOIST = 32
Public Const KD_SOIL_DENSITY = 33
Public Const KD_SOIL_LEACH = 34
Public Const KD_CONTAM_PARAMS = 35
Public Const KD_PROGENY_PARAMS = 36
Public Const EC_START = 37
Public Const EC_DURATION = 38
Public Const EC_NTIMES = 39
Public Const CP_CONTAM_PARAMS = 40
Public Const CP_PROGENY_PARAMS = 41
Public Const AT_DEPOSITION_X = 50
Public Const AT_DEPOSITION_Y = 51

' Form tabs
Public Const GW_TAB_IDX = 0
Public Const SW_TAB_IDX = 1
Public Const AT_TAB_IDX = 2
Public Const SO_TAB_IDX = 3
Public Const EXP_TAB_IDX = 4
Public Const LR_TAB_IDX = 5
Public Const CON_TAB_IDX = 6

' Exposure model attributes
Public Const GW_TYPE = 1
Public Const SW_TYPE = 2
Public Const AT_TYPE = 3
Public Const SO_TYPE = 5

' Constants for measurement unit types
Public Const SOIL_LR = 0
Public Const SOIL_KD = 1
Public Const KOC = 2
Public Const HALF_LIFE = 4

'  contaminant parameter list indexes
Public Const LR_IDX = 0
Public Const KD_IDX = 1
Public Const KOC_IDX = 2
Public Const GW_HALF_LIFE_IDX = 4
Public Const SW_HALF_LIFE_IDX = 5
Public Const AT_HALF_LIFE_IDX = 6
Public Const SO_HALF_LIFE_IDX = 7
Public Const MAX_CONTAM_PARAM = 7

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
  kind As Double
  param(MAX_CONTAM_PARAM) As String
End Type

Type contam_param
  name As String
  cas As String
  kind As Double
  rad As Boolean
  param(MAX_CONTAM_PARAM) As String
  numprog As Long
  progeny() As contam_progeny_param
End Type

Type soilparm
  Index As Long
  sand As String
  silt As String
  clay As String
  organic As String
  iron As String
  ph As String
End Type

Public soil As soilparm
Public model() As model_contam_param
Public con() As contam_param
Public m_numcon As Long
Public refs() As Long

Global uStr(4) As String
Global media(5) As String
Global tf(4) As String
Global fw(2) As String
Global paths(25) As String


Sub InitConstants()
  
  uStr(SOIL_LR) = "1/yr"
  uStr(SOIL_KD) = "mL/g"
  uStr(KOC) = "mL/g"
  uStr(HALF_LIFE) = "day"
   
  media(1) = "groundwater"
  media(2) = "surface water"
  media(3) = "air"
  media(4) = "not used"
  media(5) = "soil"
   
  tf(1) = "leafy vegetables"
  tf(2) = "other vegetables"
  tf(3) = "meat"
  tf(4) = "milk"
  
  fw(1) = "meat animal"
  fw(2) = "milk animal"

  paths(0) = "air volatiles from water"
  paths(1) = "ingestion of drinking water"
  paths(2) = "shower dermal contact"
  paths(3) = "ingestion of water while showering"
  paths(4) = "ingestion of leafy vegetables"
  paths(5) = "ingestion of other vegetables"
  paths(6) = "ingestion of meat"
  paths(7) = "ingestion of milk"
  paths(8) = "ingestion of finned fish"
  paths(9) = "ingestion of shell fish"
  paths(10) = "ingestion of water while swimming"
  paths(11) = "swimming dermal contact"
  paths(12) = "shoreline dermal contact"
  paths(13) = "ingestion of shoreline sediment"
  paths(14) = "ingestion of soil"
  paths(15) = "soil dermal contact"
  paths(16) = "ingestion of special foods"
  paths(17) = "shower or indoor air inhalation"
  paths(18) = "air inhalation"
  paths(19) = "soil resuspension inhalation"
  paths(20) = "swimming external exposure"
  paths(21) = "boating external exposure"
  paths(22) = "shoreline external exposure"
  paths(23) = "soil external exposure"
  paths(24) = "air external exposure"
  paths(25) = "shower or indoor air inhalation"

End Sub

