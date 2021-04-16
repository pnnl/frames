VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "AGDISPDEP"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Private mvarNumX As Long
Private mvarNUMY As Long
Private mvarXCOORDS As New Collection
Private mvarYCOORDS As New Collection
Private mvarDEPVALUES As New Collection
Private mvarCID As String
Private mvarLLCornerX As Double
Private mvarLLCornerY As Double
Public Property Let LLCornerY(ByVal vData As Double)
    mvarLLCornerY = vData
End Property
Public Property Get LLCornerY() As Double
    LLCornerY = mvarLLCornerY
End Property
Public Property Let LLCornerX(ByVal vData As Double)
    mvarLLCornerX = vData
End Property
Public Property Get LLCornerX() As Double
    LLCornerX = mvarLLCornerX
End Property
Public Property Let CID(ByVal vData As String)
    mvarCID = vData
End Property
Public Property Get CID() As String
    CID = mvarCID
End Property
Public Property Set DEPVALUES(ByVal vData As Collection)
    Set mvarDEPVALUES = vData
End Property
Public Property Get DEPVALUES() As Collection
    Set DEPVALUES = mvarDEPVALUES
End Property
Public Property Set ycoords(ByVal vData As Collection)
    Set mvarYCOORDS = vData
End Property
Public Property Get ycoords() As Collection
    Set ycoords = mvarYCOORDS
End Property
Public Property Set xcoords(ByVal vData As Collection)
    Set mvarXCOORDS = vData
End Property
Public Property Get xcoords() As Collection
    Set xcoords = mvarXCOORDS
End Property
Public Property Let numy(ByVal vData As Long)
    mvarNUMY = vData
End Property
Public Property Get numy() As Long
    numy = mvarNUMY
End Property
Public Property Let numx(ByVal vData As Long)
    mvarNumX = vData
End Property
Public Property Get numx() As Long
    numx = mvarNumX
End Property
Public Function ReadFile(fpath As String, Optional conid = "", Optional begx = 0#, Optional begy = 0#) As Boolean
    Dim fnum As Integer
    Dim txtLine As String
    Dim inline As String
    Dim tarray
    On Error GoTo EH
    While xcoords.Count > 0
        xcoords.Remove 1
    Wend
    While ycoords.Count > 0
        ycoords.Remove 1
    Wend
    While DEPVALUES.Count > 0
        DEPVALUES.Remove 1
    Wend
    Me.CID = conid
    Me.LLCornerX = begx
    Me.LLCornerY = begy
    fnum = FreeFile()
    Open fpath For Input As #fnum
    Line Input #fnum, txtLine
    inline = RemoveExtraSpaces(txtLine)
    tarray = Split(inline, " ")
    numx = Val(tarray(0))
    numy = Val(tarray(1))
    Line Input #fnum, txtLine
    While EOF(fnum) = False
        inline = RemoveExtraSpaces(txtLine)
        tarray = Split(inline, " ")
        xcoords.Add Val(tarray(0))
        ycoords.Add Val(tarray(1))
        DEPVALUES.Add (Val(tarray(2))) * 10000
        Line Input #fnum, txtLine
    Wend
    inline = RemoveExtraSpaces(txtLine)
    tarray = Split(inline, " ")
    xcoords.Add Val(tarray(0))
    ycoords.Add Val(tarray(1))
    DEPVALUES.Add (Val(tarray(2))) * 10000
    Close #fnum
    ReadFile = True
    Exit Function
EH:
    Close #fnum
    MsgBox Err.description, vbExclamation + vbOKOnly, "ReadFile Error"
    ReadFile = False
End Function
Public Function FindMinDepVal() As Double
    Dim i As Integer
    Dim dmin As Double
    dmin = Me.DEPVALUES.Item(1)
    For i = 2 To Me.DEPVALUES.Count
        If Me.DEPVALUES.Item(i) < dmin Then
            dmin = Me.DEPVALUES.Item(i)
        End If
    Next i
    FindMinDepVal = dmin
End Function
Public Function WriteCRDFile(fpath As String, begx As Double, begy As Double) As Boolean
    Dim txtLine As String
    Dim fnum As Integer
    Dim i As Integer
    On Error GoTo EH
    fnum = FreeFile()
    Open fpath For Output As #fnum
    txtLine = CStr(Me.DEPVALUES.Count)
    Print #fnum, txtLine
    For i = 1 To Me.DEPVALUES.Count
        txtLine = CStr(i) + "," + CStr(Me.xcoords.Item(i)) + "," + CStr(Me.ycoords.Item(i))
        Print #fnum, txtLine
    Next i
    Close #fnum
    WriteCRDFile = True
    Exit Function
EH:
    Close #fnum
    MsgBox Err.description, vbExclamation + vbOKOnly, "WriteCRDFile Error"
    WriteCRDFile = False
End Function
Public Function WriteContourInput(fpath As String) As Boolean
    Dim txtLine As String
    Dim fnum As Integer
    Dim i As Integer
    On Error GoTo EH
    fnum = FreeFile()
    Open fpath For Output As #fnum
    txtLine = CStr(Me.DEPVALUES.Count)
    Print #fnum, txtLine
    txtLine = CStr(Me.DEPVALUES.Count) + ", 2, " + CStr(Me.FindMinDepVal())
    Print #fnum, txtLine
    txtLine = "E (km) N (km) TEDE Dep"
    Print #fnum, txtLine
    For i = 1 To Me.DEPVALUES.Count
        txtLine = CStr(i) + " " + CStr(Me.LLCornerX + Me.xcoords.Item(i)) + " " + CStr(Me.LLCornerY + Me.ycoords.Item(i)) + " 9999.0 " + CStr(Me.DEPVALUES.Item(i))
        Print #fnum, txtLine
    Next i
    Close #fnum
    WriteAdjustedContourInput = True
    Exit Function
EH:
    Close #fnum
    MsgBox Err.description, vbExclamation + vbOKOnly, "WriteContourInput Error"
    WriteAdjustedContourInput = False
End Function
Public Function FindMinCollVal(cvals As Collection) As Double
    Dim i As Integer
    Dim dmin As Double
    dmin = cvals.Item(1)
    For i = 2 To cvals.Count
        If cvals.Item(i) < dmin Then
            dmin = cvals.Item(i)
        End If
    Next i
    FindMinCollVal = dmin
End Function
Public Function WriteContourCTRFile(contlevels As Collection, fpath As String) As Boolean
    Dim txtLine As String
    Dim fnum As Integer
    Dim i As Integer
    On Error GoTo EH
    fnum = FreeFile()
    Open fpath For Output As #fnum
    txtLine = CStr(contlevels.Count) + ", .TRUE."
    Print #fnum, txtLine
    For i = 1 To contlevels.Count
        txtLine = CStr(contlevels.Item(i))
        Print #fnum, txtLine
    Next i
    Print #fnum,
    Print #fnum,
    txtLine = "++++++++++NOTES++++++++++++++++"
    Print #fnum, txtLine
    txtLine = "Number of contour levels; Convert input data to LOG10"
    Print #fnum, txtLine
    txtLine = "Contour level 1"
    Print #fnum, txtLine
    txtLine = "Contour level 2"
    Print #fnum, txtLine
    txtLine = ".."
    Print #fnum, txtLine
    txtLine = ".."
    Print #fnum, txtLine
    txtLine = "Contour level n"
    Print #fnum, txtLine
    Print #fnum,
    txtLine = "**IMPORTANT NOTE**"
    Print #fnum, txtLine
    txtLine = "Smallest contour level must be greater than minimum value in data file."
    Print #fnum, txtLine
    Close #fnum
    WriteContourCTCFile = True
    Exit Function
EH:
    Close #fnum
    WriteContourCTRFile = False
    MsgBox Err.description, vbExclamation + vbOKOnly, "WriteContourCTRFile Error"
End Function
Public Function WriteContourRSFile(fpath As String, datpath As String, ctrpath As String, crdpath As String, outpath As String) As Boolean
    Dim txtLine As String
    Dim fnum As Integer
    On Error GoTo EH
    fnum = FreeFile()
    Open fpath For Output As #fnum
    txtLine = "'" + datpath + "'"
    Print #fnum, txtLine
    txtLine = "'" + ctrpath + "'"
    Print #fnum, txtLine
    txtLine = "'" + crdpath + "'"
    Print #fnum, txtLine
    txtLine = "'" + outpath + "'"
    Print #fnum, txtLine
    txtLine = "2"
    Print #fnum, txtLine
    Print #fnum,
    txtLine = "++++++++++NOTES++++++++++++++++"
    Print #fnum, txtLine
    txtLine = "Input file of data to contour"
    Print #fnum, txtLine
    txtLine = "Input file of contour levels"
    Print #fnum, txtLine
    txtLine = "Input file of data point coordinates"
    Print #fnum, txtLine
    txtLine = "Output file of countours"
    Print #fnum, txtLine
    txtLine = "Column of data in input file to contour"
    Print #fnum, txtLine
    Close #fnum
    WriteContourRSFile = True
    Exit Function
EH:
    Close #fnum
    WriteContourRSFile = False
    MsgBox Err.description, vbExclamation + vbOKOnly, "WriteContourRSFile Error"
End Function

'Public Sub PopulateCustomContoursFeatureClass(tRunName As String, tDomName As String, tdomsize As Integer, timeperiod As String, fType As String, relType As String, filename As String, ByRef pFeatureClass As IFeatureClass)
'    Dim CntNames As New Collection
'    Dim i As Long, j As Long, k As Long, id As Long
'    Dim SWcorX As Double        ' SWcorX         = WA-S Easting of southwest corner of square area (km)
'    Dim SWcorY As Double        ' SWcorY         = WA-S Northing of southwest corner of square area (km)
'    Dim size As Double          ' Size           = length of one side of square area (km)
'    Dim UTMZone As String       ' UTMzone        = code if UTM instead of WA-S [currently not used]
'    Dim NoCont As Long          ' NoCont         = total potential number of contours (typically 12)
'    Dim NoPts As Long           ' NoPts          = number of points in contour
'    Dim ContNo As Long          ' ContNo         = contour number; CntNames array element number (1-12)
'    Dim OpenClose As String     ' OpenClose      = "open" or "closed" identifying if contour closes
'    Dim Xpt As Double           ' Xpt, Ypt       = normalized coordinate of contour point (0 to 1)
'    Dim Ypt As Double
'    Dim noPoints As Long, contLevel As Long
'    Dim xfr As Double, yfr As Double, x As Double, y As Double
'    Dim ixID As Long, ixType As Long, ixTime As Long, ixContour As Long, ixRunAlias As Long
'    Dim ixPTSrcs As Long, ixLNSrcs As Long, ixARSrcs As Long, ixScenName As Long, ixreltype As Long
'    Dim ixRunName As Long, ixDomName As Long, ixDomSize As Long, ixComment As Long
'    Dim pFeatureBuffer As IFeatureBuffer
'    Dim pFeatureCursor As IFeatureCursor
'    Dim tGeometry As IGeometry
'    Dim pFeature As IFeature
'    Dim fldContour As Double
'    Dim ppoint As IPoint
'    Dim pPolyline As IPolyline
'    Dim pPointCollection As IPointCollection
'    Dim npt As Long
'    Dim inputlistCount As Long
'    Dim nod As Node
'    Dim fnum, Line1 As String, List1, lastLine As Long
'    Dim pos As Long
'    Dim ct As Long
'    Dim List2, Line2 As String
'    Dim ptnames As String
'    Dim lnnames As String
'    Dim arnames As String
'    Dim tfstring As String
'    On Error GoTo ErrorHandler
'    fnum = FreeFile
'    Open filename For Input As #fnum
'    Do While Not EOF(fnum)
'        lastLine = Seek(fnum)
'        Line Input #fnum, Line1
'    Loop
'    ' read contour per time step counts from last line
'    Seek #fnum, lastLine
'    Line Input #fnum, Line1
'    Dim numconts As Long
'    numconts = Val(Trim(Line1))
'    Seek #fnum, 1  ' return to BOF
'    Line Input #fnum, Line1
'    List1 = Split(Trim(Line1), " ")
'    ct = 0
'    UTMZone = "none"
'    SWcorX = Val(List1(0))
'    SWcorY = Val(List1(1))
'    size = Val(List1(2))
'    Line Input #fnum, Line2
'    List2 = Split(Trim(Line2), " ")
'    NoCont = 0
'    For i = 1 To UBound(List2)
'        If List2(i) <> "" Then
'            NoCont = NoCont + 1
'            CntNames.Add List2(i)
'        End If
'    Next i
'    Set pFeatureCursor = pFeatureClass.Insert(True)
'    Set pFeatureBuffer = pFeatureClass.CreateFeatureBuffer
'    ixRunName = pFeatureBuffer.Fields.FindField("RunName")
'    ixRunAlias = pFeatureBuffer.Fields.FindField("RunAlias")
'    ixDomName = pFeatureBuffer.Fields.FindField("DomName")
'    ixDomSize = pFeatureBuffer.Fields.FindField("DomSize")
'    ixID = pFeatureBuffer.Fields.FindField("ID")
'    ixType = pFeatureBuffer.Fields.FindField("Type")
'    ixreltype = pFeatureBuffer.Fields.FindField("EmissionType")
'    ixTime = pFeatureBuffer.Fields.FindField("Time")
'    ixContour = pFeatureBuffer.Fields.FindField("Contour")
'    ixPTSrcs = pFeatureBuffer.Fields.FindField("PTSrcNames")
'    ixLNSrcs = pFeatureBuffer.Fields.FindField("LNSrcNames")
'    ixARSrcs = pFeatureBuffer.Fields.FindField("ARSrcNames")
'    ixScenName = pFeatureBuffer.Fields.FindField("ScenName")
'    ixComment = pFeatureBuffer.Fields.FindField("Comment")
'    ptnames = "custom"
'    lnnames = "custom"
'    arnames = "custom"
'    'Set ppoint = pObjFactory.Create("esricore.Point")
'    Set ppoint = New Point
'    Set pPolyline = New Polyline
'    Set pPointCollection = pPolyline
'    Input #fnum, noPoints, contLevel, OpenClose
'    fldContour = Val(CntNames.Item(contLevel))
'    For npt = 1 To noPoints
'        Input #fnum, xfr, yfr
'        x = SWcorX + (xfr * size)
'        y = SWcorY + (yfr * size)
'        tfstring = CStr(Int((SWcorX / 1000#) + (xfr * size)) * 1000#) + "," + CStr(Int((SWcorY / 1000#) + (yfr * size)) * 1000#)
'        ppoint.PutCoords ((SWcorX / 1000#) + (xfr * size)) * 1000#, ((SWcorY / 1000#) + (yfr * size)) * 1000#
'        pPointCollection.AddPoint ppoint
'    Next npt
'    id = id + 1
'    Set pPolyline = pPointCollection
'    Set pFeatureBuffer.Shape = pPolyline
'    pFeatureBuffer.value(ixRunName) = tRunName
'    pFeatureBuffer.value(ixDomName) = tDomName
'    pFeatureBuffer.value(ixRunAlias) = "TEMP"
'    pFeatureBuffer.value(ixDomSize) = CStr(tdomsize)
'    pFeatureBuffer.value(ixPTSrcs) = ptnames
'    pFeatureBuffer.value(ixLNSrcs) = lnnames
'    pFeatureBuffer.value(ixARSrcs) = arnames
'    pFeatureBuffer.value(ixScenName) = CurrScenario
'    pFeatureBuffer.value(ixID) = id
'    pFeatureBuffer.value(ixType) = fType
'    pFeatureBuffer.value(ixreltype) = relType
'    pFeatureBuffer.value(ixTime) = timeperiod
'    pFeatureBuffer.value(ixContour) = fldContour
'    pFeatureBuffer.value(ixComment) = " "
'    pFeatureCursor.InsertFeature pFeatureBuffer
'    pFeatureCursor.Flush
'    Close #fnum
'    Exit Sub
'ErrorHandler:
'    Close #fnum
'    MsgBox Err.description, vbExclamation + vbOKOnly, "PopulateCustomContoursFeatureClass Error"
'End Sub
'
