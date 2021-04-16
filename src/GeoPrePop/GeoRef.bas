Attribute VB_Name = "GeoRef"
Option Explicit
Option Compare Text

Const blank = ""

Sub Main()
  Dim cmdline As String
  Dim hSet As Long, hCoord As Long, hPoint As Long, hAxis As Long, hMod
  
  cmdline = Command$
  
  Dim retval As Long
  Call StartModule(3)
  PID = OpenIO(SimPath, SimName, modid)
  If (PID < 0) Then End
  
  hMod = IconGetHandle(PID, modid)
 
  Dim ErrorCode As Integer
  hSet = DataSetGetHandle(PID, modid & ".GEOReference")
  hCoord = VariableGetHandleByDataSet(PID, hSet, "Coordinates")
  hPoint = VariableGetHandleByDataSet(PID, hSet, "PointID")
  hAxis = VariableGetHandleByDataSet(PID, hSet, "Axis")
  
  Dim ct As Long
  ct = VariableDimensionCount(PID, hSet, hAxis, SetIdx())
  If (ct < 1) Then VariableWriteString1 PID, hSet, hAxis, blank, 1, "X"
  If (ct < 2) Then VariableWriteString1 PID, hSet, hAxis, blank, 2, "Y"
  If (ct < 3) Then VariableWriteString1 PID, hSet, hAxis, blank, 3, "Z"
  
  Dim hEnum As Long
  Dim index As Long
  Dim slen As Long
  Dim i As Long
  Dim ctid As Long
  Dim cticon As Long
  Dim omodid As String
  Dim touched As Boolean
  
  hEnum = EnumOpen(PID)
  cticon = EnumOutputIcons(hEnum, hMod)
  ctid = VariableDimensionCount(PID, hSet, hPoint, SetIdx())
  For i = 1 To cticon
    slen = EnumGetStringAtIndex(hEnum, i, omodid)
    If slen > 0 And omodid <> "sink" Then
      index = VariableLookUp(PID, hSet, hPoint, omodid, SetIdx())
      If (index <= 0) Then
        touched = True
        ctid = ctid + 1
        index = ctid
        VariableWriteString1 PID, hSet, hPoint, blank, index, omodid
        VariableWriteFloat2 PID, hSet, hCoord, "m", index, 1, 0#
        VariableWriteFloat2 PID, hSet, hCoord, "m", index, 2, 0#
        VariableWriteFloat2 PID, hSet, hCoord, "m", index, 3, 0#
      End If
    End If
  Next i
  EnumClose hEnum
  CloseIO PID, 0
  End
End Sub
