VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "PointClass"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
Private mvarXCoord As Double
Private mvarYCoord As Double
Public Property Let ycoord(ByVal vData As Double)
    mvarYCoord = vData
End Property
Public Property Get ycoord() As Double
    ycoord = mvarYCoord
End Property
Public Property Let xcoord(ByVal vData As Double)
    mvarXCoord = vData
End Property
Public Property Get xcoord() As Double
    xcoord = mvarXCoord
End Property
Public Function GetDistance(tpoint As PointClass) As Double
    GetDistance = Sqr((Me.xcoord - tpoint.xcoord) ^ 2 + (Me.ycoord - tpoint.ycoord) ^ 2)
End Function



