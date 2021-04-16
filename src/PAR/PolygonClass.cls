VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "PolygonClass"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
Private mvarPoints As New Collection
Private mvarArea As Double
Private mvarPerimeter As Double
Private mvarCentroid As New PointClass
Private mvarVPoint As New PointClass 'local copy
Public Property Set VPoint(ByVal vData As PointClass)
    Set mvarVPoint = vData
End Property
Public Property Get VPoint() As PointClass
    Set VPoint = mvarVPoint
End Property
Public Property Set Centroid(ByVal vData As PointClass)
    Set mvarCentroid = vData
End Property
Public Property Get Centroid() As PointClass
    Set Centroid = mvarCentroid
End Property
Public Property Let Perimeter(ByVal vData As Double)
    mvarPerimeter = vData
End Property
Public Property Get Perimeter() As Double
    Perimeter = mvarPerimeter
End Property
Public Property Let Area(ByVal vData As Double)
    mvarArea = vData
End Property
Public Property Get Area() As Double
    Area = mvarArea
End Property
Public Property Set Points(ByVal vData As Collection)
    Set mvarPoints = vData
End Property
Public Property Get Points() As Collection
    Set Points = mvarPoints
End Property
Public Sub AddPoint(tpoint As PointClass)
    Points.Add tpoint
End Sub
Public Sub SetPolygon(xvertices As Collection, yvertices As Collection)
    Dim i As Integer
    Dim tpoint As New PointClass
    For i = 1 To xvertices.count
        Set tpoint = New PointClass
        tpoint.xcoord = xvertices.Item(i)
        tpoint.ycoord = yvertices.Item(i)
        Me.AddPoint tpoint
    Next i
End Sub
'Calculates the perimeter of the polygon and sets the perimeter property of the polygon
Public Sub GetPerimeter()
    Dim i As Integer
    Dim tsum As Double
    Dim tline As New LineClass
    For i = 1 To Points.count
        If i = Points.count Then
            tline.SetLine Points.Item(i).xcoord, Points.Item(i).ycoord, Points.Item(1).xcoord, Points.Item(1).ycoord
            tsum = tsum + tline.Length
        Else
            tline.SetLine Points.Item(i).xcoord, Points.Item(i).ycoord, Points.Item(i + 1).xcoord, Points.Item(i + 1).ycoord
            tsum = tsum + tline.Length
        End If
    Next i
    Perimeter = tsum
End Sub
'Calculates the centroid of the polygon and sets the centroid property of the polygon
Public Sub SetCentroid()
    Dim tarea As Double
    Dim i As Integer
    Dim ip1 As Integer
    Dim TEMP As Double
    tarea = 0#
    Centroid.xcoord = 0#
    Centroid.ycoord = 0#
    For i = 1 To Points.count
        If i < Points.count Then
            ip1 = i + 1
        Else
            ip1 = 1
        End If
        TEMP = (Points.Item(i).xcoord * Points.Item(ip1).ycoord - Points.Item(ip1).xcoord * Points.Item(i).ycoord)
        tarea = tarea + TEMP
        Centroid.xcoord = Centroid.xcoord + (Points.Item(ip1).xcoord + Points.Item(i).xcoord) * TEMP
        Centroid.ycoord = Centroid.ycoord + (Points.Item(ip1).ycoord + Points.Item(i).ycoord) * TEMP
    Next i
    tarea = tarea / 2#
    Centroid.xcoord = Centroid.xcoord / (6# * tarea)
    Centroid.ycoord = Centroid.ycoord / (6# * tarea)
End Sub
'Calculates the area of the polygon and sets the area property of the polygon
Public Sub SetArea()
    Dim tarea As Double
    Dim i As Integer
    Dim im1 As Integer
    Dim ip1 As Integer
    tarea = 0#
    For i = 1 To Points.count
        If i = 1 Then
            im1 = Points.count
        Else
            im1 = i - 1
        End If
        If i < Points.count - 1 Then
            ip1 = i + 1
        Else
            ip1 = 1
        End If
        tarea = tarea + Points.Item(i).xcoord * (Points.Item(ip1).ycoord - Points.Item(im1).ycoord)
    Next i
    tarea = 0.5 * Abs(tarea)
    Area = tarea
End Sub
'Determines if a given point lays inside of the polygon
'tpoint The point which will checked against the polygon
Public Function PointInside(tpoint As PointClass) As Boolean
    Dim xnew As Double
    Dim ynew As Double
    Dim xold As Double
    Dim yold As Double
    Dim x1 As Double
    Dim y1 As Double
    Dim x2 As Double
    Dim y2 As Double
    Dim i As Integer
    Dim inside As Boolean
    Dim tline As New LineClass
    inside = False
    If Points.count < 3 Then
        PointInside = 0
        Exit Function
    End If
    For i = 1 To Me.Points.count - 1
        tline.SetLine Me.Points.Item(i).xcoord, Me.Points.Item(i).ycoord, Me.Points.Item(i + 1).xcoord, Me.Points.Item(i + 1).ycoord
        If tline.IsPointOnLine(Me.Points.Item(i).xcoord, Me.Points.Item(i).ycoord, Me.Points.Item(i + 1).xcoord, Me.Points.Item(i + 1).ycoord, tpoint.xcoord, tpoint.ycoord) = True Then
            PointInside = True
            Exit Function
        End If
    Next i
    tline.SetLine Me.Points.Item(Me.Points.count).xcoord, Me.Points.Item(Me.Points.count).ycoord, Me.Points.Item(1).xcoord, Me.Points.Item(1).ycoord
    If tline.IsPointOnLine(Me.Points.Item(Me.Points.count).xcoord, Me.Points.Item(Me.Points.count).ycoord, Me.Points.Item(1).xcoord, Me.Points.Item(1).ycoord, tpoint.xcoord, tpoint.ycoord) = True Then
        PointInside = True
        Exit Function
    End If
    xold = Points.Item(Points.count).xcoord
    yold = Points.Item(Points.count).ycoord
    For i = 1 To Points.count
        xnew = Points.Item(i).xcoord
        ynew = Points.Item(i).ycoord
        If xnew > xold Then
            x1 = xold
            x2 = xnew
            y1 = yold
            y2 = ynew
        Else
            x1 = xnew
            x2 = xold
            y1 = ynew
            y2 = yold
        End If
        If ((xnew < tpoint.xcoord) = (tpoint.xcoord <= xold)) And ((tpoint.ycoord - y1) * (x2 - x1) < (y2 - y1) * (tpoint.xcoord - x1)) Then
            If inside = False Then
                inside = True
            Else
                inside = False
            End If
        End If
        xold = xnew
        yold = ynew
    Next i
    PointInside = inside
End Function
'Determines if a given line intersects any portion of the polygon
'tline the line to be checked
'numpoints The number of polygon boundary points the line intersected. numpoints is set within the function
'tpoint1 point on the boundary of the polygon in which the line intersects the polygon. tpoint1 is set within the function
'tpoint2 point on the boundary of the polygon in which the line intersects the polygon. tpoint2 is set within the function
Public Function IsLineIntersecting(tline As LineClass, ByRef numpoints As Integer, ByRef tpoint1 As PointClass, ByRef tpoint2 As PointClass) As Boolean
    Dim i As Integer
    Dim intersectcheck As Integer
    Dim pline As New LineClass
    Dim ppoint As New PointClass
    IsLineIntersecting = False
    numpoints = 0
    For i = 1 To Points.count
        If i = Points.count Then
            pline.SetLine Points.Item(i).xcoord, Points.Item(i).ycoord, Points.Item(1).xcoord, Points.Item(1).ycoord
        Else
        pline.SetLine Points.Item(i).xcoord, Points.Item(i).ycoord, Points.Item(i + 1).xcoord, Points.Item(i + 1).ycoord
        End If
        intersectcheck = pline.LineIntersects(tline, ppoint)
        If intersectcheck = 2 Then
            numpoints = -9999
            IsLineIntersecting = True
            tpoint1.xcoord = -9999
            tpoint1.ycoord = -9999
            tpoint2.xcoord = -9999
            tpoint2.ycoord = -9999
            Exit Function
        ElseIf intersectcheck = 1 Then
            If numpoints = 0 Then
                tpoint1.xcoord = ppoint.xcoord
                tpoint1.ycoord = ppoint.ycoord
                numpoints = 1
                IsLineIntersecting = True
            Else
                tpoint2.xcoord = ppoint.xcoord
                tpoint2.ycoord = ppoint.ycoord
                numpoints = 2
                IsLineIntersecting = True
            End If
        Else
            If numpoints = 0 Then
                tpoint1.xcoord = -999
                tpoint1.ycoord = -999
                tpoint2.xcoord = -999
                tpoint2.ycoord = -999
            ElseIf numpoints = 1 Then
                tpoint2.xcoord = -999
                tpoint2.ycoord = -999
            End If
        End If
    Next i
End Function
'Calculates the index of the point within the Points property which lies the farthest out on the x axis
Public Function FarthestRightPoint() As Integer
    Dim i As Integer
    Dim tindex As Integer
    Dim tx As Double
    tx = Points.Item(1).xcoord
    tindex = 1
    For i = 2 To Points.count
        If Points.Item(i).xcoord > tx Then
            tx = Points.Item(i).xcoord
            tindex = i
        End If
    Next i
    FarthestRightPoint = tindex
End Function
'Caclulates the angle theta of a point present in the Points property based upon
'the location of the point that lies the farthest out on the x axis
'x The x coordinate of the farthest right point
'y The y coordinate of the farthest right point
Public Function FindTheta(x As Double, y As Double) As Double
    Dim tx As Double
    Dim plusdeg As Double
    Dim rad2deg As Double
    rad2deg = 180# / 3.14159
    If x <> 0 Then
        'ATAN
        tx = y / x
        FindTheta = Atn(tx)
    ElseIf y <> 0 Then
        'ACOT
        tx = x / y
        FindTheta = Atn(tx) + 2 * Atn(1)
    End If
    FindTheta = FindTheta * rad2deg
    If x >= 0 And y < 0 Then
        FindTheta = 270 + FindTheta
    ElseIf x < 0 And y >= 0 Then
        FindTheta = 180 + FindTheta
    ElseIf x < 0 And y < 0 Then
        FindTheta = 180 + FindTheta
    End If
End Function
'Calculates the angles for each point present in the points property of the polygon.
'The angles are stored in a collection which is returned by the function.
'stpoint The angle which lies at the farthest right of the polygon
'stindex The index at which the farthest right point can be found in the Points property
Public Function SetAngles(stpoint As PointClass, stindex As Integer) As Collection
    Dim i As Integer
    Dim tangles As New Collection
    Dim x As Double
    Dim y As Double
    Dim r As Double
    Dim theta As Double
    Set SetAngles = New Collection
    For i = 1 To Points.count
        'if the point is the farthest right point then there is no need to calculate the angle
        If i = stindex Then
            theta = -999
            SetAngles.Add theta
        Else
            x = Points.Item(i).xcoord - stpoint.xcoord
            y = Points.Item(i).ycoord - stpoint.ycoord
            theta = FindTheta(x, y)
            SetAngles.Add theta
        End If
    Next i
End Function
'Sorts the vertices of a polygon based upon the angle of the vertice
'in relation to the vertice of the polygon which lies at the farthest
'distance along the x axis.
Public Sub SortVertices()
    Dim i As Integer
    Dim stptindex As Integer
    Dim ptangles As New Collection
    Dim indices As New Collection
    Dim sortedindices As New Collection
    Dim tangle As Double
    Dim tindex As Integer
    Dim tpoints As New Collection
    stptindex = FarthestRightPoint()
    Set ptangles = SetAngles(Points.Item(stptindex), stptindex)
    For i = 1 To ptangles.count
        indices.Add i
    Next i
    sortedindices.Add indices.Item(stptindex)
    indices.Remove stptindex
    ptangles.Remove stptindex
    While ptangles.count > 0
        tangle = -1
        For i = 1 To ptangles.count
            If ptangles.Item(i) > tangle Then
                tangle = ptangles.Item(i)
                tindex = i
            End If
        Next i
        sortedindices.Add indices.Item(tindex)
        indices.Remove tindex
        ptangles.Remove tindex
    Wend
    For i = 1 To sortedindices.count
        tpoints.Add Points.Item(sortedindices.Item(i))
    Next i
    Set Points = tpoints
End Sub
'
Public Function PolygonIntersection(ipoly As PolygonClass, opoly As PolygonClass) As Boolean
    Dim temppoly As New PolygonClass
    Dim i As Integer
    Dim j As Integer
    Dim check As Boolean
    Dim checkint As Integer
    Dim Line1 As New LineClass
    Dim Line2 As New LineClass
    Dim tpoint As New PointClass
    Dim tpoint2 As New PointClass
    Dim linecnt As Integer
    On Error GoTo EH
linecnt = 1
    'check if vertices of this polygon are inside of ipoly
    For i = 1 To Me.Points.count
linecnt = 2
        check = ipoly.PointInside(Me.Points.Item(i))
linecnt = 3
        If check = True Then
linecnt = 4
            temppoly.Points.Add Me.Points.Item(i)
linecnt = 5
        End If
linecnt = 6
    Next i
linecnt = 7
    'if the number of intersecting points are equal to the number
    'of points in this polygon then this polygon is fully contained
    'within the ipoly polygon
    If temppoly.Points.count = Me.Points.count Then
linecnt = 8
        Set opoly = Me
linecnt = 9
        PolygonIntersection = True
linecnt = 10
        Exit Function
linecnt = 11
    End If
linecnt = 12
    'check if vertices of ipoly are inside of this polygon
    For i = 1 To ipoly.Points.count
linecnt = 13
        check = Me.PointInside(ipoly.Points.Item(i))
linecnt = 14
        If check = True Then
linecnt = 15
            temppoly.Points.Add ipoly.Points.Item(i)
linecnt = 16
        End If
linecnt = 17
    Next i
linecnt = 18
    'if the number of intersecting points are equal to the number
    'of points in the ipoly polygon then the ipoly polygon is fully
    'contained within this polygon
    If temppoly.Points.count = ipoly.Points.count Then
linecnt = 19
        Set opoly = ipoly
linecnt = 20
        PolygonIntersection = True
linecnt = 21
        Exit Function
linecnt = 22
    End If
linecnt = 23
    'check if any lines of this polygon intersect the lines of ipoly
    For i = 1 To Me.Points.count
linecnt = 24
        If i = Me.Points.count Then
linecnt = 25
            Line1.SetLine Me.Points.Item(i).xcoord, Me.Points.Item(i).ycoord, Me.Points.Item(1).xcoord, Me.Points.Item(1).ycoord
linecnt = 26
        Else
linecnt = 27
            Line1.SetLine Me.Points.Item(i).xcoord, Me.Points.Item(i).ycoord, Me.Points.Item(i + 1).xcoord, Me.Points.Item(i + 1).ycoord
linecnt = 28
        End If
linecnt = 29
        For j = 1 To ipoly.Points.count
linecnt = 30
            Set tpoint = New PointClass
linecnt = 31
            If j = ipoly.Points.count Then
linecnt = 32
                Line2.SetLine ipoly.Points.Item(j).xcoord, ipoly.Points.Item(j).ycoord, ipoly.Points.Item(1).xcoord, ipoly.Points.Item(1).ycoord
linecnt = 33
            Else
linecnt = 34
                Line2.SetLine ipoly.Points.Item(j).xcoord, ipoly.Points.Item(j).ycoord, ipoly.Points.Item(j + 1).xcoord, ipoly.Points.Item(j + 1).ycoord
linecnt = 35
            End If
linecnt = 36
            checkint = Line1.LineIntersects(Line2, tpoint)
linecnt = 37
            If checkint = 1 Then
linecnt = 38
                temppoly.Points.Add tpoint
linecnt = 39
            ElseIf checkint = 2 Then
linecnt = 40
                LineOnLineIntersectPoints Line1, Line2, tpoint, tpoint2
linecnt = 41
                temppoly.Points.Add tpoint
linecnt = 42
                temppoly.Points.Add tpoint2
linecnt = 43
            End If
linecnt = 44
        Next j
linecnt = 45
    Next i
linecnt = 46
    If temppoly.Points.count > 2 Then
linecnt = 47
        temppoly.SortVertices
linecnt = 48
        temppoly.SetArea
linecnt = 49
        'temppoly.SetCentroid
linecnt = 50
        temppoly.RemoveDupVertices
linecnt = 51
        Set opoly = temppoly
linecnt = 52
        PolygonIntersection = True
linecnt = 53
    Else
linecnt = 54
        PolygonIntersection = False
linecnt = 55
    End If
linecnt = 56
    PolygonIntersection = True
linecnt = 57
    Exit Function
EH:
    PolygonIntersection = False
    MsgBox Err.Description + vbCrLf + CStr(linecnt), vbExclamation + vbOKOnly, "PolygonIntersection Error"
End Function
Public Sub RemoveDupVertices()
    Dim tpoints As New Collection
    Dim i As Integer
    Dim j As Integer
    Dim found As Boolean
    For i = 1 To Points.count
        found = False
        For j = 1 To tpoints.count
            If Points.Item(i).xcoord = tpoints.Item(j).xcoord And Points.Item(i).ycoord = tpoints.Item(j).ycoord Then
                found = True
                Exit For
            End If
        Next
        If found = False Then
            tpoints.Add Points.Item(i)
        End If
    Next i
    While Points.count > 0
        Points.Remove 1
    Wend
    For i = 1 To tpoints.count
        Points.Add tpoints.Item(i)
    Next i
End Sub
Public Sub LineOnLineIntersectPoints(Line1 As LineClass, Line2 As LineClass, tpoint As PointClass, tpoint2 As PointClass)
    Dim pointcount As Integer
    pointcount = 0
    'Line #1
    If Line1.IsPointOnLine(Line1.begx, Line1.begy, Line1.EndX, Line1.EndY, Line2.begx, Line2.begy) = True Then
        If pointcount = 0 Then
            pointcount = 1
            tpoint.xcoord = Line2.begx
            tpoint.ycoord = Line2.begy
        Else
            pointcount = 2
            tpoint2.xcoord = Line2.begx
            tpoint2.ycoord = Line2.begy
        End If
    End If
    If Line1.IsPointOnLine(Line1.begx, Line1.begy, Line1.EndX, Line1.EndY, Line2.EndX, Line2.EndY) = True Then
        If pointcount = 0 Then
            pointcount = 1
            tpoint.xcoord = Line2.EndX
            tpoint.ycoord = Line2.EndY
        Else
            pointcount = 2
            tpoint2.xcoord = Line2.EndX
            tpoint2.ycoord = Line2.EndY
        End If
    End If
    'Line #2
    If Line2.IsPointOnLine(Line2.begx, Line2.begy, Line2.EndX, Line2.EndY, Line1.begx, Line1.begy) = True Then
        If pointcount = 0 Then
            pointcount = 1
            tpoint.xcoord = Line1.begx
            tpoint.ycoord = Line1.begy
        Else
            pointcount = 2
            tpoint2.xcoord = Line1.begx
            tpoint2.ycoord = Line1.begy
        End If
    End If
    If Line1.IsPointOnLine(Line2.begx, Line2.begy, Line2.EndX, Line2.EndY, Line1.EndX, Line1.EndY) = True Then
        If pointcount = 0 Then
            pointcount = 1
            tpoint.xcoord = Line1.EndX
            tpoint.ycoord = Line1.EndY
        Else
            pointcount = 2
            tpoint2.xcoord = Line1.EndX
            tpoint2.ycoord = Line1.EndY
        End If
    End If
End Sub
Public Function PercentOfLineInside(tline As LineClass, numlinepoints As Integer, tpoint1 As PointClass, Optional tpoint2 As PointClass) As Double
    Dim tlinedist As Double
    Dim ilinedist As Double
    Dim iline As New LineClass
    'case where line cuts across the entire polygon
    If numlinepoints > 1 Then
        iline.SetLine tpoint1.xcoord, tpoint1.ycoord, tpoint2.xcoord, tpoint2.ycoord
        ilinedist = iline.Length
        tlinedist = tline.Length
        PercentOfLineInside = ilinedist / tlinedist
    Else
        'case where the beginning point of the line lays within the polygon
        If Me.PointInside(tline.BegPt) = True Then
            iline.SetLine tline.BegPt.xcoord, tline.BegPt.ycoord, tpoint1.xcoord, tpoint1.ycoord
            tlinedist = tline.Length
            ilinedist = iline.Length
            PercentOfLineInside = ilinedist / tlinedist
        'case where the ending point of the line lays within the polygon
        Else
            iline.SetLine tline.EndPt.xcoord, tline.EndPt.ycoord, tpoint1.xcoord, tpoint1.ycoord
            tlinedist = tline.Length
            ilinedist = iline.Length
            PercentOfLineInside = ilinedist / tlinedist
        End If
    End If
End Function

