VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "LineClass"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
Private mvarBegX As Double
Private mvarBegY As Double
Private mvarEndX As Double
Private mvarEndY As Double
Private mvarLength As Double
Private mvarSlope As Double
Private mvarYIntercept As Double
Private mvarA As Double
Private mvarB As Double
Private mvarC As Double
'local variable(s) to hold property value(s)
Private mvarBegPt As New PointClass 'local copy
Private mvarEndPt As New PointClass 'local copy
Public Property Set EndPt(ByVal vData As PointClass)
'used when assigning an Object to the property, on the left side of a Set statement.
'Syntax: Set x.EndPt = Form1
    Set mvarEndPt = vData
End Property


Public Property Get EndPt() As PointClass
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.EndPt
    Set EndPt = mvarEndPt
End Property



Public Property Set BegPt(ByVal vData As PointClass)
'used when assigning an Object to the property, on the left side of a Set statement.
'Syntax: Set x.BegPt = Form1
    Set mvarBegPt = vData
End Property


Public Property Get BegPt() As PointClass
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.BegPt
    Set BegPt = mvarBegPt
End Property



Public Property Let C(ByVal vData As Double)
    mvarC = vData
End Property
Public Property Get C() As Double
    C = mvarC
End Property
Public Property Let B(ByVal vData As Double)
    mvarB = vData
End Property
Public Property Get B() As Double
    B = mvarB
End Property
Public Property Let a(ByVal vData As Double)
    mvarA = vData
End Property
Public Property Get a() As Double
    a = mvarA
End Property
Public Property Let YIntercept(ByVal vData As Double)
    mvarYIntercept = vData
End Property
Public Property Get YIntercept() As Double
    YIntercept = mvarYIntercept
End Property
Public Property Let Slope(ByVal vData As Double)
    mvarSlope = vData
End Property
Public Property Get Slope() As Double
    Slope = mvarSlope
End Property
Public Property Let Length(ByVal vData As Double)
    mvarLength = vData
End Property
Public Property Get Length() As Double
    Length = mvarLength
End Property
Public Property Let EndY(ByVal vData As Double)
    mvarEndY = vData
End Property
Public Property Get EndY() As Double
    EndY = mvarEndY
End Property
Public Property Let EndX(ByVal vData As Double)
    mvarEndX = vData
End Property
Public Property Get EndX() As Double
    EndX = mvarEndX
End Property
Public Property Let begy(ByVal vData As Double)
    mvarBegY = vData
End Property
Public Property Get begy() As Double
    begy = mvarBegY
End Property
Public Property Let begx(ByVal vData As Double)
    mvarBegX = vData
End Property
Public Property Get begx() As Double
    begx = mvarBegX
End Property
Private Function CheckDistances(x1 As Double, y1 As Double, x2 As Double, y2 As Double, ptx As Double, pty As Double) As Boolean
    Dim d1 As Double
    Dim d2 As Double
    Dim d3 As Double
    
    d1 = Sqr((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2)) 'distance between two points of the line
    d2 = Sqr((ptx - x2) * (ptx - x2) + (pty - y2) * (pty - y2)) 'distance between the point and the ending point of the line
    d3 = Sqr((x1 - ptx) * (x1 - ptx) + (y1 - pty) * (y1 - pty)) 'distance between the point and the beginning point of the line
    If d2 <= d1 And d3 <= d1 Then
        CheckDistances = True
    Else
        CheckDistances = False
    End If
End Function
Public Function IsPointOnLine(x1 As Double, y1 As Double, x2 As Double, y2 As Double, ptx As Double, pty As Double) As Boolean
    Dim bot As Double
    Dim dist As Double
    Dim dot As Double
    Dim t As Double
    Dim xn As Double
    Dim yn As Double
    bot = (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)
    If bot = 0# Then
        xn = x1
        yn = y1
    Else
        dot = (ptx - x1) * (x2 - x1) + (pty - y1) * (y2 - y1)
        t = dot / bot
        xn = x1 + t * (x2 - x1)
        yn = y1 + t * (y2 - y1)
    End If
    dist = Sqr((ptx - xn) * (ptx - xn) + (pty - yn) * (pty - yn))
    If dist = 0 Then
        If CheckDistances(x1, y1, x2, y2, ptx, pty) = True Then
            IsPointOnLine = True
        Else
            IsPointOnLine = False
        End If
    Else
        IsPointOnLine = False
    End If
End Function
Public Sub SetLine(BX As Double, by As Double, EX As Double, ey As Double)
    begx = BX
    begy = by
    EndX = EX
    EndY = ey
    BegPt.xcoord = BX
    BegPt.ycoord = by
    EndPt.xcoord = EX
    EndPt.ycoord = ey
    If begx = EndX Then
        Slope = -9999
    Else
        Slope = (begy - EndY) / (begx - EndX)
    End If
    Length = Sqr((EndX - begx) ^ 2 + (EndY - begy) ^ 2)
    YIntercept = begy - (Slope * begx)
    a = begy - EndY
    B = EndX - begx
    C = (begx * EndY) - (EndX * begy)
End Sub
Public Function rmat2_inverse(ByRef ta() As Double, ByRef tb() As Double) As Double
    Dim det As Double
    Dim i As Integer
    Dim j As Integer
    det = ta(0, 0) * ta(1, 1) - ta(0, 1) * ta(1, 0)
    If det = 0# Then
        For i = 0 To 1
            For j = 0 To 1
                tb(i, j) = 0#
            Next j
        Next i
        rmat2_inverse = det
        Exit Function
    End If
    tb(0, 0) = tb(0, 0) + ta(1, 1) / det
    tb(0, 1) = tb(0, 1) - ta(0, 1) / det
    tb(1, 0) = tb(1, 0) - ta(1, 0) / det
    tb(1, 1) = tb(1, 1) + ta(0, 0) / det
    rmat2_inverse = det
End Function
'Returns the following based upon the calculations
'    -1, both A1 and B1 were zero.
'    -2, both A2 and B2 were zero.
'     0, no intersection, the lines are parallel.
'     1, one intersection point, returned in X, Y.
'     2, infinitely many intersections, the lines are identical.
Public Function LineIntersects(tline As LineClass, ByRef tpoint As PointClass) As Integer
    Dim ta(1, 1) As Double
    Dim tb(1, 1) As Double
    Dim det As Double
    Dim x As Double
    Dim y As Double
    x = 0#
    y = 0#
    If Me.a = 0# And Me.B = 0# Then
        LineIntersects = -1
        Exit Function
    ElseIf tline.a = 0# And tline.B = 0# Then
        LineIntersects = -2
        Exit Function
    End If
    ta(0, 0) = Me.a
    ta(0, 1) = Me.B
    ta(1, 0) = tline.a
    ta(1, 1) = tline.B
    
    det = rmat2_inverse(ta, tb)
    If det <> 0# Then
        LineIntersects = 1
        tpoint.xcoord = -tb(0, 0) * Me.C - tb(0, 1) * tline.C
        tpoint.ycoord = -tb(1, 0) * Me.C - tb(1, 1) * tline.C
        If tpoint.GetDistance(Me.BegPt) > Me.BegPt.GetDistance(Me.EndPt) Or tpoint.GetDistance(Me.EndPt) > Me.BegPt.GetDistance(Me.EndPt) Then
            LineIntersects = 0
            tpoint.xcoord = 0#
            tpoint.ycoord = 0#
        End If
        If tpoint.GetDistance(tline.BegPt) > tline.BegPt.GetDistance(tline.EndPt) Or tpoint.GetDistance(tline.EndPt) > tline.BegPt.GetDistance(tline.EndPt) Then
            LineIntersects = 0
            tpoint.xcoord = 0#
            tpoint.ycoord = 0#
        End If
        Exit Function
    Else
        LineIntersects = 0
        If Me.a = 0# Then
            If tline.B * Me.C = tline.C * Me.B Then
                LineIntersects = 2
                tpoint.xcoord = 0#
                tpoint.ycoord = 0#
            End If
        Else
            If tline.a * Me.C = tline.C * Me.a Then
                LineIntersects = 2
                tpoint.xcoord = 0#
                tpoint.ycoord = 0#
            End If
        End If
    End If
End Function









