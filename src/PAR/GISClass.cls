VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "GISClass"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
Option Compare Text
'local variable(s) to hold property value(s)
Private mvarobjid As Variant 'local copy
Private mvarCmdArgs As Variant ' local copy
Private mvarmsgText As Variant
Private mvarstatus As Variant
Private mvarobjtype As Variant
Private Property Let objtype(ByVal vdata As Variant)
  mvarobjtype = vdata
End Property
Private Property Get objtype() As Variant
  If IsObject(mvarobjtype) Then
    Set objtype = mvarobjtype
  Else
    objtype = mvarobjtype
  End If
End Property

Public Property Let status(ByVal vdata As Variant)
  mvarstatus = vdata
End Property
Public Property Get status() As Variant
  If IsObject(mvarstatus) Then
    Set status = mvarstatus
  Else
    status = mvarstatus
  End If
End Property

Public Property Let msgText(ByVal vdata As Variant)
  mvarmsgText = vdata
End Property
Public Property Get msgText() As Variant
  If IsObject(mvarmsgText) Then
    Set msgText = mvarmsgText
  Else
    msgText = mvarmsgText
  End If
End Property
Public Property Let objid(ByVal vdata As Variant)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.ObjId = 5
    mvarobjid = vdata
End Property

Private Property Let CmdArgs(ByVal vdata As Variant)
  mvarCmdArgs = vdata
End Property

Private Property Get CmdArgs() As Variant
  If IsObject(mvarCmdArgs) Then
    Set CmdArgs = mvarCmdArgs
  Else
    CmdArgs = mvarCmdArgs
  End If
End Property


Public Property Set objid(ByVal vdata As Object)
'used when assigning an Object to the property, on the left side of a Set statement.
'Syntax: Set x.ObjId = Form1
    Set mvarobjid = vdata
End Property


Public Property Get objid() As Variant
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.ObjId
    If IsObject(mvarobjid) Then
        Set objid = mvarobjid
    Else
        objid = mvarobjid
    End If
End Property


Public Function GisConnect(args As String) As Integer
  If (0 < InStr(args, Chr(0))) Then
    CmdArgs = Left(args, InStr(args, Chr(0)) - 1)
  Else
    CmdArgs = args
  End If
  status = GisStartModule("GIS", 5, CmdArgs)
  If status = SUCCESS Then
    If "gis" = Left(ModName, 3) Then
      GisName = ModName
    Else
      status = IsGisConnectedToModule(GisName)
'    If Status = 1 Then
'      Load frmMain
'    Else
      If status = FAILURE Then
        MsgBox "GisConnect Failed" & vbCrLf & " -- The GIS must be connected as a source to " & ModName
      End If
     End If
   End If
  GisConnect = status
End Function

Public Function GisGetPolygon(id As String, msg As String) As Integer
   OpMode = SERVER
   Set GisObj = Me
   msgText = msg
   objtype = "POLYGON"
   Load frmMain
   frmMain.FormLoad
   frmMain.Show 1
   id = objid
   GisGetPolygon = status
End Function

Public Function GisGetPoint(ByRef id As String, msg As String) As Integer
  OpMode = SERVER
   Set GisObj = Me
   msgText = msg
   objtype = "POINT"
   Load frmMain
   frmMain.FormLoad
   frmMain.Show 1
   id = objid
  GisGetPoint = status
End Function

Public Function GisGetLine(ByRef id As String, msg As String) As Integer
  OpMode = SERVER
   Set GisObj = Me
   msgText = msg
   objtype = "LINE"
   Load frmMain
   frmMain.FormLoad
   frmMain.Show 1
   id = objid
  GisGetLine = status
End Function

Public Function GisGetArea(ByVal id As String, ByRef area As Double) As Integer
  OpMode = SERVER
  Load frmMain
  GisGetArea = frmMain.GetData(id, GET_AREA, area)
  Unload frmMain
End Function
 
Public Function GisGetLength(ByVal id As String, ByRef length As Double) As Integer
  OpMode = SERVER
  Load frmMain
  GisGetLength = frmMain.GetData(id, GET_LENGTH, length)
  Unload frmMain
End Function

Public Function GisGetCentroid(ByVal id As String, ByRef x As Double, ByRef y As Double, ByRef z As Double) As Integer
  OpMode = SERVER
  Load frmMain
  GisGetCentroid = frmMain.GetData(id, GET_CENTROID, x, y, z)
  Unload frmMain
End Function

Public Function GisExecute() As Integer
  OpMode = CLIENT
  Load frmMain
  frmMain.FormLoad
  frmMain.Show 1
  GisExecute = SUCCESS
End Function

Public Function ValidObject(desc As String, objid As String) As Boolean
  ValidObject = (objtype = Left(desc, Len(objtype)))
End Function

Private Sub Class_Initialize()
  status = 0
  CmdArgs = ""
  objtype = ""
  msgText = ""
  objid = ""
End Sub

