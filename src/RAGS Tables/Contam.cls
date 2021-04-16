VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ContamCls"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
Option Compare Text
'local variable(s) to hold property value(s)
Private mvarcas As String
Private mvarcname As String
Private mvarktype As Integer
Private mvarprog As Collection
Private mvarpcas As String
Private mvarparm As Collection
Private mvarrtpw As Collection


Public Property Set rtpw(ByVal vData As Collection)
'used when assigning an Object to the property, on the left side of a Set statement.
'Syntax: Set x.rtpw = Form1
    Set mvarrtpw = vData
End Property

Public Property Get rtpw() As Collection
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.rtpw
    Set rtpw = mvarrtpw
End Property

Public Property Set parm(ByVal vData As Collection)
'used when assigning an Object to the property, on the left side of a Set statement.
'Syntax: Set x.parm = Form1
    Set mvarparm = vData
End Property

Public Property Get parm() As Collection
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.parm
    Set parm = mvarparm
End Property

Public Property Let pcas(ByVal vData As String)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.pcas = 5
    mvarpcas = vData
End Property

Public Property Get pcas() As String
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.pcas
    pcas = mvarpcas
End Property

Public Property Set prog(ByVal vData As Collection)
'used when assigning an Object to the property, on the left side of a Set statement.
'Syntax: Set x.prog = Form1
    Set mvarprog = vData
End Property

Public Property Get prog() As Collection
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.prog
    Set prog = mvarprog
End Property

Public Property Let ktype(ByVal vData As Integer)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.ktype = 5
    mvarktype = vData
End Property

Public Property Get ktype() As Integer
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.ktype
    ktype = mvarktype
End Property

Public Property Let cname(ByVal vData As String)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.cname = 5
    mvarcname = vData
End Property

Public Property Get cname() As String
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.cname
    cname = mvarcname
End Property

Public Property Let cas(ByVal vData As String)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.cas = 5
    mvarcas = vData
End Property

Public Property Get cas() As String
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.cas
    cas = mvarcas
End Property

Private Sub Class_Initialize()
  Set prog = New Collection
  Set rtpw = New Collection
  rtpw.Add New Collection, "epf"
  rtpw.Add New Collection, "rif"
  rtpw.Add New Collection, "hif"
End Sub

Public Function GetEpfRoutesAndPathways(dsIdx&) As Collection
Dim i As Long
Dim numtimes As Long
Dim timeIdx As Long
Dim Route As String
Dim Path As String
Dim Unit As String
Dim retc As Integer
Dim found As Boolean
Dim pw As ExpRtCls
Dim rt As ExpRtCls
Dim dsrt As Collection

  On Error GoTo ErrorHandler
  Set dsrt = New Collection
  
  numtimes = epfGetTimeCount(dsIdx, cas)
  For timeIdx = 0 To numtimes - 1
    retc = epfLoadRoutesAndPathwaysByTime(dsIdx, cas, cas, timeIdx)
    For i = 1 To retc
      Route = String(128, Chr(0))
      Path = String(128, Chr(0))
      Unit = String(128, Chr(0))
      
      epfGetRouteAndPathway dsIdx, i - 1, Path, Route, Unit
      
      Route = Left(Route, InStr(Route, Chr(0)) - 1)
      Path = Left(Path, InStr(Path, Chr(0)) - 1)
      Unit = Left(Unit, InStr(Unit, Chr(0)) - 1)
      found = False
      For Each rt In dsrt
        found = (rt.Route = Route And rt.pathway = Path And rt.Unit = Unit)
        If found Then Exit For
      Next
      If Not found Then
        Set pw = New ExpRtCls
        pw.Route = Route
        pw.pathway = Path
        pw.Unit = Unit
        dsrt.Add pw, CStr(dsrt.count + 1)
      End If
    Next i
  Next timeIdx
  On Error Resume Next
  rtpw("epf").Add dsrt, CStr(dsIdx + 1)
  On Error GoTo ErrorHandler
  Set GetEpfRoutesAndPathways = dsrt
  Exit Function

ErrorHandler:
  MsgBox Err.Description, vbOKOnly, "GetHifRoutesAndPathways"
End Function

Public Function GetRifRoutesAndPathways(dsIdx&, ageIdx&) As Collection
Dim i As Long
Dim numtimes As Long
Dim timeIdx As Long
Dim Route As String
Dim Path As String
Dim Measure As String
Dim Unit As String
Dim retc As Long
Dim found As Boolean
Dim pw As ExpRtCls
Dim rt As ExpRtCls
Dim dsrt As Collection

  On Error GoTo ErrorHandler
  Set dsrt = New Collection
  
  numtimes = rifGetTimeCount(dsIdx, ageIdx, cas)
  For timeIdx = 0 To numtimes - 1
    retc = rifLoadRoutesAndPathwaysByTime(dsIdx, ageIdx, cas, cas, timeIdx)
    For i = 0 To retc - 1
      Route = String(128, Chr(0))
      Path = String(128, Chr(0))
      Measure = String(128, Chr(0))
      Unit = String(128, Chr(0))
      
      rifGetRouteAndPathway dsIdx, i, Path, Route, Measure, Unit
      
      Route = Left(Route, InStr(Route, Chr(0)) - 1)
      Path = Left(Path, InStr(Path, Chr(0)) - 1)
      Measure = Left(Measure, InStr(Measure, Chr(0)) - 1)
      Unit = Left(Unit, InStr(Unit, Chr(0)) - 1)
      found = False
      For Each rt In dsrt
        found = (rt.Route = Route And rt.pathway = Path And rt.Measure = Measure And rt.Unit = Unit)
        If found = True Then Exit For
      Next
      If Not found Then
        Set pw = New ExpRtCls
        pw.Route = Route
        pw.pathway = Path
        pw.Measure = Measure
        pw.Unit = Unit
        dsrt.Add pw, CStr(dsrt.count + 1)
      End If
    Next
  Next
  
  On Error Resume Next
  rtpw("rif").Add dsrt, CStr(dsIdx + 1)
  On Error GoTo ErrorHandler
  
  Set GetRifRoutesAndPathways = dsrt
  Exit Function

ErrorHandler:
  MsgBox Err.Description, vbOKOnly, "GetHifRoutesAndPathways"
End Function


Public Function GetHifRoutesAndPathways(dsIdx&, ageIdx&) As Collection
Dim i As Long
Dim numtimes As Long
Dim timeIdx As Long
Dim Route As String
Dim Path As String
Dim Measure As String
Dim Unit As String
Dim retc As Long
Dim found As Boolean
Dim pw As ExpRtCls
Dim rt As ExpRtCls
Dim dsrt As Collection

  On Error GoTo ErrorHandler
  Set dsrt = New Collection
  
  numtimes = hifGetTimeCount(dsIdx, ageIdx, cas)
  For timeIdx = 0 To numtimes - 1
    retc = hifLoadRoutesAndPathwaysByTime(dsIdx, ageIdx, cas, cas, timeIdx)
    For i = 0 To retc - 1
      Route = String(128, Chr(0))
      Path = String(128, Chr(0))
      Measure = String(128, Chr(0))
      Unit = String(128, Chr(0))
      
      hifGetRouteAndPathway dsIdx, i, Path, Route, Measure, Unit
      
      Route = Left(Route, InStr(Route, Chr(0)) - 1)
      Path = Left(Path, InStr(Path, Chr(0)) - 1)
      Measure = Left(Measure, InStr(Measure, Chr(0)) - 1)
      Unit = Left(Unit, InStr(Unit, Chr(0)) - 1)
      found = False
      For Each rt In dsrt
        found = (rt.Route = Route And rt.pathway = Path And rt.Measure = Measure And rt.Unit = Unit)
        If found = True Then Exit For
      Next
      If Not found Then
        Set pw = New ExpRtCls
        pw.Route = Route
        pw.pathway = Path
        pw.Measure = Measure
        pw.Unit = Unit
        dsrt.Add pw, CStr(dsrt.count + 1)
      End If
    Next
  Next
  
  On Error Resume Next
  rtpw("hif").Add dsrt, CStr(dsIdx + 1)
  On Error GoTo ErrorHandler
  
  Set GetHifRoutesAndPathways = dsrt
  Exit Function

ErrorHandler:
  MsgBox Err.Description, vbOKOnly, "GetHifRoutesAndPathways"
End Function


