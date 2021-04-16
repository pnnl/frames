VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "clsThreatArea"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Private mstThreatAreaName   As String
Private mstThreatAreaType   As String   'TA_WEDGE_ENGLISH, TA_CYLINDER_ENGLISH, TA_CIRCLE_ENGLISH, TA_POLYGON_ENGLISH
Private mstThreatAreaCategory As String 'modeldefined, userdefined or template.
Private mstImpactType       As String   'local copy
Private mstDescShort        As String
Private mstDescLong         As String

Private mlAngle             As Long     'local copy
Private mdDistance          As Double   'local copy
Private mlDirection         As Long     '
Private mlIsolationRadius   As Long     'local copy
Private mlSourceGeoObjectID As Long     'local copy
Private mdLattitude         As Double   'local copy
Private mdLongitude         As Double   'local copy
Private mdOffsetDist        As Double   'local copy
Private mdOffsetDir         As Double   'local copy
Private mstLocType          As String   'local copy
Private mstPolygonData      As String   't

