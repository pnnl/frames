Attribute VB_Name = "PolyEdit"
Option Explicit
Option Compare Text

Private TempFileName As String
Public KeyPadSel As Integer

Public Function InsertPolygon(pxp As Double, pyp As Double) As String
'Public Function InsertPolygon(dbc As Dbcocx, PolyType As Integer, WorkArea As Long, Optional DbcGeo1 As DbcGeo = Nothing, Optional TempArea As Long = 0, Optional id As String = "", Optional Style As Long = 6, Optional BitMask As String = "") As Boolean
    InsertPolygon = ""
    If TempFileName = "" Then TempFileName = "Dbc0000"
    Dim px As Double, py As Double
    Dim Vertexes As Integer
    Dim x1st As Double, y1st As Double
    Dim IDtemp As String: IDtemp = ""
    Dim id As String: id = ""
    Dim dbc As Dbcocx
    Dim PolyType As Integer: PolyType = POLYGON
    Set dbc = frmMain.Dbcocx1
    Dim Style As Long: Style = 7
    Dim BitMask As String: BitMask = ""
    Dim DbcGeo1 As DbcGeo: Set DbcGeo1 = Nothing
    Dim workarea As Long: workarea = 1
    Dim temparea As Long: temparea = GetRandomWorkArea()
    
    Dim msgBegin As String, msgResume As String
    msgBegin = "Click on coordinate to locate initial vertex of polygon (right button to cancel)."
    msgResume = "Move mouse to next vertex and click (right button for menu)."

     
    frmMain.mnuPolyInsEnd.Enabled = True
    frmMain.mnuPolyInsUndoAll.Enabled = True
    frmMain.mnuPolyInsUndoLast.Enabled = True
    frmMain.mnuPolyInsCont.Enabled = True
    frmMain.mnuPolyInsIsle.Enabled = False ' True
    frmMain.mnuPolyInsHole.Enabled = False ' True
    
    SelectDbgr 0, True
    dbc.devgrcmdecolorset defaults(DPOLY).color
    dbc.devgrcmdelineset defaults(DPOLY).linetype
        
    If dbc.devlastkey() = 13 Then
        x1st = pxp
        y1st = pyp
        dbc.devselect temparea
        dbc.devgrcreate App.path + "\" + TempFileName, 0
        Vertexes = 1
        GetPointDrag msgResume, px, py, pxp, pyp, 1
    Else
        Vertexes = 0
    End If
    Dim curVertexes As Integer
    Dim OrigNewFlag As Integer: OrigNewFlag = 0
    Dim OrigVertexes As Integer: OrigVertexes = Vertexes
    curVertexes = Vertexes
    Do While Vertexes > 0
        Dim newFlag As Integer: newFlag = 0
        KeyPadSel = KPS_NEWPOINT
        If PolyType <> POLYGON Then
            frmMain.mnuPolyInsIsle.Enabled = False
            frmMain.mnuPolyInsHole.Enabled = False
        End If
        If dbc.devlastkey() <> 13 Then
            If curVertexes < 3 Then
                frmMain.mnuPolyInsIsle.Enabled = False
                frmMain.mnuPolyInsHole.Enabled = False
                If curVertexes < 2 Then
                    frmMain.mnuPolyInsUndoLast.Enabled = False
                End If
            End If
            If ((PolyType = POLYGON Or PolyType = CLOSED_POLYLINE) And curVertexes < 3) Or _
                    (PolyType <> POLYGON And curVertexes < 2) Then
                frmMain.mnuPolyInsEnd.Enabled = False
            End If
            frmMain.PopupMenu frmMain.mnuPolyInsert ' KeyPad.Show 1
            If KeyPadSel = KPS_END Or KeyPadSel = KPS_ISLE Or KeyPadSel = KPS_HOLE Then
                ' closed current subpolygon : write it to graphic database
                Dim FirstVertex As Boolean: FirstVertex = True
                Dim px2 As Double, py2 As Double
                Do
                    dbc.devseek "E" & "TMP" & Format(OrigVertexes, "00000")
                    If dbc.devfound() = 0 Then
                        Exit Do
                    End If
                    px = Val(dbc.devgetfield("VX1"))
                    py = Val(dbc.devgetfield("VY1"))
                    px2 = Val(dbc.devgetfield("VX2"))
                    py2 = Val(dbc.devgetfield("VY2"))
                    If PolyType = POLYGON Then
                        If FirstVertex Then ' first vertex only (of every subpolygon)
                            FirstVertex = False
                            If OrigNewFlag = 0 Then ' first subpolygon only
                                dbc.devgrcmdpolygon "", Style, BitMask
                                IDtemp = dbc.devgetid()
                            End If
                            dbc.devgrcmdvertexnew px, py, OrigNewFlag
                        End If
                        dbc.devgrcmdvertexnew px2, py2, 0
                    Else
                        dbc.devgrcmdline IDtemp, px, py, px2, py2
                        If IDtemp = "" Then IDtemp = dbc.devgetid()
                    End If
                    OrigVertexes = OrigVertexes + 1
                Loop
                OrigVertexes = Vertexes + 1
                If PolyType = POLYGON Then ' check polygon
                    If Not DbcGeo1 Is Nothing Then
                        Dim PolygonToCheck As Long, CheckCode As Long
                        PolygonToCheck = dbc.devreadentity(IDtemp, 0)
                        CheckCode = DbcGeo1.poCheckPolygon(0.00001, PolygonToCheck)
                        dbc.devfreeentity PolygonToCheck
                        If CheckCode <> 0 Then
                            MsgBox "The polygon just inserted is not valid !", vbOKOnly, "Warning."
                            KeyPadSel = KPS_UNDOALL ' for exiting the loop
                        End If
                    End If
                ElseIf PolyType = CLOSED_POLYLINE Then ' close the polyline
                    dbc.devgrcmdline IDtemp, px2, py2, x1st, y1st
                End If
            End If
            If KeyPadSel = KPS_END Or KeyPadSel = KPS_UNDOALL Then
                Exit Do
            ElseIf KeyPadSel = KPS_ISLE Then
                OrigNewFlag = 1: newFlag = 1
            ElseIf KeyPadSel = KPS_HOLE Then
                OrigNewFlag = 2: newFlag = 2
            End If
        End If
        If KeyPadSel = KPS_UNDOLAST Then
            dbc.devseek "E" & "TMP" & Format(Vertexes - 1, "00000")
            dbc.devgrdisplayxor "TMP" & Format(Vertexes - 1, "00000"), 1
            pxp = Val(dbc.devgetfield("VX1"))
            pyp = Val(dbc.devgetfield("VY1"))
            dbc.devdelete
            dbc.devpack
            curVertexes = curVertexes - 1
            Vertexes = Vertexes - 1
        ElseIf KeyPadSel <> KPS_CONTINUE Then
            If Not (dbc.devlastkey() <> 13 And curVertexes < 3) Then
                If newFlag = 0 Then
                    Vertexes = Vertexes + 1
                    curVertexes = curVertexes + 1
                    dbc.devgrcmdline "TMP" & Format(Vertexes - 1, "00000"), pxp, pyp, px, py
                    pxp = px
                    pyp = py
                    dbc.devgrdisplayxor "TMP" & Format(Vertexes - 1, "00000"), 1
                Else
                    dbc.devgrcmdline "TMP" & Format(Vertexes, "00000"), pxp, pyp, x1st, y1st
                    dbc.devgrdisplayxor "TMP" & Format(Vertexes, "00000"), 1
                    dbc.devdelete
                    dbc.devpack
                End If
            End If
            If newFlag <> 0 Then
                GetPoint msgBegin, pxp, pyp
                If dbc.devlastkey() <> 13 Then Exit Do
                x1st = pxp
                y1st = pyp
                curVertexes = 1
                Vertexes = Vertexes + 1
            End If
        End If
        GetPointDrag msgResume, px, py, pxp, pyp, 1
    Loop
    If Vertexes > 0 Then
        Dim PolygonToWrite As Long
        If KeyPadSel <> KPS_UNDOALL Then
            PolygonToWrite = dbc.devreadentity(IDtemp, 0)
        End If
        dbc.devuse ""
        Kill App.path + "\" + TempFileName + ".dbf"
        Kill App.path + "\" + TempFileName + ".ndx"
        SelectDbgr 0, True
        If KeyPadSel = KPS_UNDOALL Then
            dbc.devgrclear 0
            dbc.devgrdisplay
        Else
            ' copy from temp area to work area
            dbc.devwriteentity id, PolygonToWrite, 0
            If id = "" Then id = dbc.devgetid()
            dbc.devfreeentity PolygonToWrite
            If PolyType = POLYGON Then dbc.devgrcmdpolycalc id, 2, 0, 0
            dbc.devgrdisplayid id, 256
            InsertPolygon = id
        End If
    End If
  InsertPolygon = Trim(id)
End Function

Public Function InsertPointGrid(Dbcocx1 As Dbcocx, PolyType As Integer, workarea As Long, Optional DbcGeo1 As DbcGeo = Nothing, Optional temparea As Long = 0, Optional id As String = "", Optional Style As Long = 7, Optional BitMask As String = "") As String
    InsertPointGrid = ""
    If TempFileName = "" Then TempFileName = "Dbc0000"
    Dim px As Double, py As Double, pxp As Double, pyp As Double
    Dim Vertexes As Integer
    Dim x1st As Double, y1st As Double
    Dim IDtemp As String: IDtemp = ""
    
    frmMain.mnuPolyInsEnd.Enabled = True
    frmMain.mnuPolyInsUndoAll.Enabled = True
    frmMain.mnuPolyInsUndoLast.Enabled = True
    frmMain.mnuPolyInsCont.Enabled = True
    
    Dbcocx1.devgrcmdecolorset defaults(DPOLY).color
    Dbcocx1.devgrcmdelineset defaults(DPOLY).linetype
    
    GetPoint "From point (right button to abort):", pxp, pyp
    If Dbcocx1.devlastkey() = 13 Then
        x1st = pxp
        y1st = pyp
        Dbcocx1.devselect temparea
        Dbcocx1.devgrcreate App.path + "\" + TempFileName, 0
        
        Vertexes = 1
        GetPointDrag "To point (right button for menu):", px, py, pxp, pyp, 1
    Else
        Vertexes = 0
    End If
    Dim curVertexes As Integer
    Dim OrigNewFlag As Integer: OrigNewFlag = 0
    Dim OrigVertexes As Integer: OrigVertexes = Vertexes
    curVertexes = Vertexes
    Do While Vertexes > 0
        Dim newFlag As Integer: newFlag = 0
        KeyPadSel = KPS_NEWPOINT
        If PolyType <> POLYGON Then
            frmMain.mnuPolyInsIsle.Enabled = False
            frmMain.mnuPolyInsHole.Enabled = False
        End If
        If Dbcocx1.devlastkey() <> 13 Then
            If curVertexes < 3 Then
                frmMain.mnuPolyInsIsle.Enabled = False
                frmMain.mnuPolyInsHole.Enabled = False
                If curVertexes < 2 Then
                    frmMain.mnuPolyInsUndoLast.Enabled = False
                End If
            End If
            If ((PolyType = POLYGON Or PolyType = CLOSED_POLYLINE) And curVertexes < 3) Or _
                    (PolyType <> POLYGON And curVertexes < 2) Then
                frmMain.mnuPolyInsEnd.Enabled = False
            End If
            frmMain.PopupMenu frmMain.mnuPolyInsert ' KeyPad.Show 1
            If KeyPadSel = KPS_END Or KeyPadSel = KPS_ISLE Or KeyPadSel = KPS_HOLE Then
                ' closed current subpolygon : write it to graphic database
                Dim FirstVertex As Boolean: FirstVertex = True
                Dim px2 As Double, py2 As Double
                Do
                    Dbcocx1.devseek "E" & "TMP" & Format(OrigVertexes, "00000")
                    If Dbcocx1.devfound() = 0 Then
                        Exit Do
                    End If
                    px = Val(Dbcocx1.devgetfield("VX1"))
                    py = Val(Dbcocx1.devgetfield("VY1"))
                    px2 = Val(Dbcocx1.devgetfield("VX2"))
                    py2 = Val(Dbcocx1.devgetfield("VY2"))
                    If PolyType = POLYGON Then
                        If FirstVertex Then ' first vertex only (of every subpolygon)
                            FirstVertex = False
                            If OrigNewFlag = 0 Then ' first subpolygon only
                                Dbcocx1.devgrcmdpolygon "", Style, BitMask
                                IDtemp = Dbcocx1.devgetid()
                            End If
                            Dbcocx1.devgrcmdvertexnew px, py, OrigNewFlag
                        End If
                        Dbcocx1.devgrcmdvertexnew px2, py2, 0
                    Else
                        Dbcocx1.devgrcmdline IDtemp, px, py, px2, py2
                        If IDtemp = "" Then IDtemp = Dbcocx1.devgetid()
                    End If
                    OrigVertexes = OrigVertexes + 1
                Loop
                OrigVertexes = Vertexes + 1
                If PolyType = POLYGON Then ' check polygon
                    If Not DbcGeo1 Is Nothing Then
                        Dim PolygonToCheck As Long, CheckCode As Long
                        PolygonToCheck = Dbcocx1.devreadentity(IDtemp, 0)
                        CheckCode = DbcGeo1.poCheckPolygon(0.00001, PolygonToCheck)
                        Dbcocx1.devfreeentity PolygonToCheck
                        If CheckCode <> 0 Then
                            MsgBox "The polygon just inserted is not valid !", vbOKOnly, "Warning."
                            KeyPadSel = KPS_UNDOALL ' for exiting the loop
                        End If
                    End If
                ElseIf PolyType = CLOSED_POLYLINE Then ' close the polyline
                    Dbcocx1.devgrcmdline IDtemp, px2, py2, x1st, y1st
                End If
            End If
            If KeyPadSel = KPS_END Or KeyPadSel = KPS_UNDOALL Then
                Exit Do
            ElseIf KeyPadSel = KPS_ISLE Then
                OrigNewFlag = 1: newFlag = 1
            ElseIf KeyPadSel = KPS_HOLE Then
                OrigNewFlag = 2: newFlag = 2
            End If
        End If
        If KeyPadSel = KPS_UNDOLAST Then
            Dbcocx1.devseek "E" & "TMP" & Format(Vertexes - 1, "00000")
            Dbcocx1.devgrdisplayxor "TMP" & Format(Vertexes - 1, "00000"), 1
            pxp = Val(Dbcocx1.devgetfield("VX1"))
            pyp = Val(Dbcocx1.devgetfield("VY1"))
            Dbcocx1.devdelete
            Dbcocx1.devpack
            curVertexes = curVertexes - 1
            Vertexes = Vertexes - 1
        ElseIf KeyPadSel <> KPS_CONTINUE Then
            If Not (Dbcocx1.devlastkey() <> 13 And curVertexes < 3) Then
                If newFlag = 0 Then
                    Vertexes = Vertexes + 1
                    curVertexes = curVertexes + 1
                    Dbcocx1.devgrcmdline "TMP" & Format(Vertexes - 1, "00000"), pxp, pyp, px, py
                    pxp = px
                    pyp = py
                    Dbcocx1.devgrdisplayxor "TMP" & Format(Vertexes - 1, "00000"), 1
                Else
                    Dbcocx1.devgrcmdline "TMP" & Format(Vertexes, "00000"), pxp, pyp, x1st, y1st
                    Dbcocx1.devgrdisplayxor "TMP" & Format(Vertexes, "00000"), 1
                    Dbcocx1.devdelete
                    Dbcocx1.devpack
                End If
            End If
            If newFlag <> 0 Then
                GetPoint "From point (right button to abort):", pxp, pyp
                If Dbcocx1.devlastkey() <> 13 Then Exit Do
                x1st = pxp
                y1st = pyp
                curVertexes = 1
                Vertexes = Vertexes + 1
            End If
        End If
        GetPointDrag "To point (right button for menu):", px, py, pxp, pyp, 1
    Loop
    If Vertexes > 0 Then
        Dim PolygonToWrite As Long
        If KeyPadSel <> KPS_UNDOALL Then
            PolygonToWrite = Dbcocx1.devreadentity(IDtemp, 0)
        End If
        Dbcocx1.devuse ""
        Kill App.path + "\" + TempFileName + ".dbf"
        Kill App.path + "\" + TempFileName + ".ndx"
        Dbcocx1.devselect workarea
        If KeyPadSel = KPS_UNDOALL Then
            Dbcocx1.devgrclear 0
            Dbcocx1.devgrdisplay
        Else
            ' copy from temp area to work area
            
            ' create a layer for this point grid
            Dim lname As String
            lname = "POINTGRID" ' & id
            Dbcocx1.devgrcmdlayerdef lname, 7, 1
            Dbcocx1.devgrcmdlayerset lname
            
            Dbcocx1.devwriteentity id, PolygonToWrite, 0
            If id = "" Then id = Dbcocx1.devgetid()
            Dbcocx1.devfreeentity PolygonToWrite
            If PolyType = POLYGON Then Dbcocx1.devgrcmdpolycalc id, 2, 0, 0
            Dbcocx1.devgrdisplayid id, 256
            Dbcocx1.devgrcmdlayerset TUSER
            
            Dbcocx1.devindexon "SECTION", SplitPath(RunName, SP_DIR) & "SECTION"
            Dbcocx1.devseek "L"
            Do While 0 = Dbcocx1.deveof()
              If "L" <> Trim(Dbcocx1.devgetfield("SECTION")) Then Exit Do
              If lname = Trim(Dbcocx1.devgetfield("DESCR")) Then
                Dbcocx1.devsetfield "DESCR", lname + " " + id
                Exit Do
              End If
              Dbcocx1.devskip 1
            Loop
            Dbcocx1.devsetorder 1 ' reset default index
    
            InsertPointGrid = id
        End If
    End If
End Function

' Let the user insert a new polygon (if PolyType = POLYGON),
' a new open polyline (if PolyType = OPEN_POLYLINE),
' or a new closed polyline (if PolyType = CLOSED_POLYLINE).
' The function return value is a boolean
' that tells you the operation outcome (true if insertion has been confirmed,
' false if the operation has been aborted).
' The parameters Dbcocx1 and WorkArea are mandatory. WorkArea is the area number
' where the entity will be placed at the end of the operations. The work is done in the
' work area specified by TempArea (if you pass 0 or you don't specify the parameter
' an unoccupied area is automatically used). A new temporary graphic database is created
' in the TempArea area. Its default name is "Dbc0000.dbf" in the directory where the
' application executable resides. You can change the graphic db name by modifing
' the public string variable TempFileName. You have to specify a path relative to
' the executable directory without the filename extension.
' You have to specify a valid DbcGeo1 parameter only if you work with a polygon and you
' want that it's checked before it's inserted in the main area.
' You can specify an ID if you want that the new entity will have that identifier. If
' you specify "" or skip that parameter a new ID is automatically assigned.
' The optional Style and BitMask parameters are used for the polygon case only. They are
' identical to those used in the devgrcmdpolygon() DbCAD dev method. The default filling
' is 6 that is Vertical Rows.
Public Function InsertPoly(Dbcocx1 As Dbcocx, PolyType As Integer, workarea As Long, Optional DbcGeo1 As DbcGeo = Nothing, Optional temparea As Long = 0, Optional id As String = "", Optional Style As Long = 7, Optional BitMask As String = "") As Boolean
    InsertPoly = False
    If TempFileName = "" Then TempFileName = "Dbc0000"
    Dim px As Double, py As Double, pxp As Double, pyp As Double
    Dim Vertexes As Integer
    Dim x1st As Double, y1st As Double
    Dim IDtemp As String: IDtemp = ""
    Dim msgBegin As String, msgResume As String
    frmMain.mnuPolyInsEnd.Enabled = True
    frmMain.mnuPolyInsUndoAll.Enabled = True
    frmMain.mnuPolyInsUndoLast.Enabled = True
    frmMain.mnuPolyInsCont.Enabled = True
'   frmMain.mnuPolyInsIsle.enabled = True
'   frmMain.mnuPolyInsHole.enabled = True
    
  Dbcocx1.devgrcmdecolorset defaults(DPOLY).color
  Dbcocx1.devgrcmdelineset defaults(DPOLY).linetype
  
  msgBegin = "Click on coordinate to locate initial vertex of polygon (right button to cancel)."
  msgResume = "Move mouse to next vertex and click (right button for menu)."
  
   GetPoint msgBegin, pxp, pyp
    If Dbcocx1.devlastkey() = 13 Then
        x1st = pxp
        y1st = pyp
        Dbcocx1.devselect temparea
        Dbcocx1.devgrcreate App.path + "\" + TempFileName, 0
        Vertexes = 1
        GetPointDrag msgResume, px, py, pxp, pyp, 1
    Else
        Vertexes = 0
    End If
    Dim curVertexes As Integer
    Dim OrigNewFlag As Integer: OrigNewFlag = 0
    Dim OrigVertexes As Integer: OrigVertexes = Vertexes
    curVertexes = Vertexes
    Do While Vertexes > 0
        Dim newFlag As Integer: newFlag = 0
        KeyPadSel = KPS_NEWPOINT
        If PolyType <> POLYGON Then
            frmMain.mnuPolyInsIsle.Enabled = False
            frmMain.mnuPolyInsHole.Enabled = False
        End If
        If Dbcocx1.devlastkey() <> 13 Then
            If curVertexes < 3 Then
                frmMain.mnuPolyInsIsle.Enabled = False
                frmMain.mnuPolyInsHole.Enabled = False
                If curVertexes < 2 Then
                    frmMain.mnuPolyInsUndoLast.Enabled = False
                End If
            End If
            If ((PolyType = POLYGON Or PolyType = CLOSED_POLYLINE) And curVertexes < 3) Or _
                    (PolyType <> POLYGON And curVertexes < 2) Then
                frmMain.mnuPolyInsEnd.Enabled = False
            End If
            frmMain.PopupMenu frmMain.mnuPolyInsert ' KeyPad.Show 1
            If KeyPadSel = KPS_END Or KeyPadSel = KPS_ISLE Or KeyPadSel = KPS_HOLE Then
                ' closed current subpolygon : write it to graphic database
                Dim FirstVertex As Boolean: FirstVertex = True
                Dim px2 As Double, py2 As Double
                Do
                    Dbcocx1.devseek "E" & "TMP" & Format(OrigVertexes, "00000")
                    If Dbcocx1.devfound() = 0 Then
                        Exit Do
                    End If
                    px = Val(Dbcocx1.devgetfield("VX1"))
                    py = Val(Dbcocx1.devgetfield("VY1"))
                    px2 = Val(Dbcocx1.devgetfield("VX2"))
                    py2 = Val(Dbcocx1.devgetfield("VY2"))
                    If PolyType = POLYGON Then
                        If FirstVertex Then ' first vertex only (of every subpolygon)
                            FirstVertex = False
                            If OrigNewFlag = 0 Then ' first subpolygon only
                                Dbcocx1.devgrcmdpolygon "", Style, BitMask
                                IDtemp = Dbcocx1.devgetid()
                            End If
                            Dbcocx1.devgrcmdvertexnew px, py, OrigNewFlag
                        End If
                        Dbcocx1.devgrcmdvertexnew px2, py2, 0
                    Else
                        Dbcocx1.devgrcmdline IDtemp, px, py, px2, py2
                        If IDtemp = "" Then IDtemp = Dbcocx1.devgetid()
                    End If
                    OrigVertexes = OrigVertexes + 1
                Loop
                OrigVertexes = Vertexes + 1
                If PolyType = POLYGON Then ' check polygon
                    If Not DbcGeo1 Is Nothing Then
                        Dim PolygonToCheck As Long, CheckCode As Long
                        PolygonToCheck = Dbcocx1.devreadentity(IDtemp, 0)
                        CheckCode = DbcGeo1.poCheckPolygon(0.00001, PolygonToCheck)
                        Dbcocx1.devfreeentity PolygonToCheck
                        If CheckCode <> 0 Then
                            MsgBox "The polygon just inserted is not valid !", vbOKOnly, "Warning."
                            KeyPadSel = KPS_UNDOALL ' for exiting the loop
                        End If
                    End If
                ElseIf PolyType = CLOSED_POLYLINE Then ' close the polyline
                    Dbcocx1.devgrcmdline IDtemp, px2, py2, x1st, y1st
                End If
            End If
            If KeyPadSel = KPS_END Or KeyPadSel = KPS_UNDOALL Then
                Exit Do
            ElseIf KeyPadSel = KPS_ISLE Then
                OrigNewFlag = 1: newFlag = 1
            ElseIf KeyPadSel = KPS_HOLE Then
                OrigNewFlag = 2: newFlag = 2
            End If
        End If
        If KeyPadSel = KPS_UNDOLAST Then
            Dbcocx1.devseek "E" & "TMP" & Format(Vertexes - 1, "00000")
            Dbcocx1.devgrdisplayxor "TMP" & Format(Vertexes - 1, "00000"), 1
            pxp = Val(Dbcocx1.devgetfield("VX1"))
            pyp = Val(Dbcocx1.devgetfield("VY1"))
            Dbcocx1.devdelete
            Dbcocx1.devpack
            curVertexes = curVertexes - 1
            Vertexes = Vertexes - 1
        ElseIf KeyPadSel <> KPS_CONTINUE Then
            If Not (Dbcocx1.devlastkey() <> 13 And curVertexes < 3) Then
                If newFlag = 0 Then
                    Vertexes = Vertexes + 1
                    curVertexes = curVertexes + 1
                    Dbcocx1.devgrcmdline "TMP" & Format(Vertexes - 1, "00000"), pxp, pyp, px, py
                    pxp = px
                    pyp = py
                    Dbcocx1.devgrdisplayxor "TMP" & Format(Vertexes - 1, "00000"), 1
                Else
                    Dbcocx1.devgrcmdline "TMP" & Format(Vertexes, "00000"), pxp, pyp, x1st, y1st
                    Dbcocx1.devgrdisplayxor "TMP" & Format(Vertexes, "00000"), 1
                    Dbcocx1.devdelete
                    Dbcocx1.devpack
                End If
            End If
            If newFlag <> 0 Then
                GetPoint msgBegin, pxp, pyp
                If Dbcocx1.devlastkey() <> 13 Then Exit Do
                x1st = pxp
                y1st = pyp
                curVertexes = 1
                Vertexes = Vertexes + 1
            End If
        End If
        GetPointDrag msgResume, px, py, pxp, pyp, 1
    Loop
    If Vertexes > 0 Then
        Dim PolygonToWrite As Long
        If KeyPadSel <> KPS_UNDOALL Then
            PolygonToWrite = Dbcocx1.devreadentity(IDtemp, 0)
        End If
        Dbcocx1.devuse ""
        Kill App.path + "\" + TempFileName + ".dbf"
        Kill App.path + "\" + TempFileName + ".ndx"
        Dbcocx1.devselect workarea
        If KeyPadSel = KPS_UNDOALL Then
            Dbcocx1.devgrclear 0
            Dbcocx1.devgrdisplay
        Else
            ' copy from temp area to work area
            Dbcocx1.devwriteentity id, PolygonToWrite, 0
            If id = "" Then id = Dbcocx1.devgetid()
            Dbcocx1.devfreeentity PolygonToWrite
            If PolyType = POLYGON Then Dbcocx1.devgrcmdpolycalc id, 2, 0, 0
            Dbcocx1.devgrdisplayid id, 256
            InsertPoly = True
        End If
    End If

End Function

' Let the user modify the polygon or polyline specified by the ID parameter.
' If you pass "" or you skip that parameter then the current entity is edited.
' The parameters have the same meaning of the InsertPoly function.
Public Function ModifyPoly(Dbcocx1 As Dbcocx, workarea As Long, Optional DbcGeo1 As DbcGeo = Nothing, Optional temparea As Long = 0, Optional id As String = "") As Boolean
    ModifyPoly = False
    If TempFileName = "" Then TempFileName = "Dbc0000"
    If id = "" Then id = Dbcocx1.devgetid()
    Dim PolygonToWork As Long: PolygonToWork = Dbcocx1.devreadentity(id, 0)
    Dbcocx1.devselect temparea
    Dbcocx1.devgrcreate App.path + "\" + TempFileName, 0
    Dbcocx1.devwriteentity "", PolygonToWork, 0
    Dbcocx1.devfreeentity PolygonToWork
    Dim IDtemp As String: IDtemp = Dbcocx1.devgetid()
    Dim PolyType As Integer: PolyType = IIf(StrComp(Left(Dbcocx1.devgetfield("DESCR"), 4), "LINE", vbTextCompare) = 0, OPEN_POLYLINE, POLYGON)
    Dim px As Double, py As Double, px2 As Double, py2 As Double, filling As String
    Dim x1st As Double, y1st As Double
    Dim numVertexes As Integer: numVertexes = 0
    If PolyType <> POLYGON Then
        ' determines if it is an open or closed polyline
        Dbcocx1.devseek "E" & IDtemp
        px = Val(Dbcocx1.devgetfield("VX1"))
        py = Val(Dbcocx1.devgetfield("VY1"))
        x1st = px: y1st = py
        numVertexes = 1
        While Dbcocx1.deveof() = 0 And Dbcocx1.devgetid() = IDtemp And Dbcocx1.devgetfield("SECTION") = "E"
            numVertexes = numVertexes + 1
            px2 = Val(Dbcocx1.devgetfield("VX2"))
            py2 = Val(Dbcocx1.devgetfield("VY2"))
            Dbcocx1.devskip 1
        Wend
        If px2 = px And py2 = py Then
            PolyType = CLOSED_POLYLINE
            numVertexes = numVertexes - 1
        End If
    Else
        filling = Dbcocx1.devgetfield("DESCR")
        Dbcocx1.devsetfield "DESCR", Left(filling, 10) & "7"
        Dbcocx1.devseek "P" & IDtemp
        While Dbcocx1.deveof() = 0 And Dbcocx1.devgetid() = IDtemp And Dbcocx1.devgetfield("SECTION") = "P"
            numVertexes = numVertexes + 1
            Dbcocx1.devskip 1
        Wend
    End If
    
    Dbcocx1.devseek "E" & IDtemp
    Dbcocx1.devgrdisplayxor IDtemp, 1
    
    Do
        Dim ptx As Double, pty As Double
        Dim MinDist As Double, dist As Double
        Dim NearestVtx As Long, firstOpenPolyline As Boolean: firstOpenPolyline = False
        Dim xvtx As Double, yvtx As Double
            
        If PolyType = OPEN_POLYLINE Then
            If numVertexes < 2 Then frmMain.mnuPolyEditEnd.Enabled = False
            If numVertexes < 3 Then frmMain.mnuPolyEditDelVrtx.Enabled = False
        Else
            If numVertexes < 3 Then frmMain.mnuPolyEditEnd.Enabled = False
            If numVertexes < 4 Then frmMain.mnuPolyEditDelVrtx.Enabled = False
        End If
        frmMain.PopupMenu frmMain.mnuPolyEdit
'       .Show 1
        
        If KeyPadSel = KPS_UNDOALL Then
            Exit Do
        ElseIf KeyPadSel = KPS_END Then
            Dim CheckCode As Long: CheckCode = 0
            If PolyType = POLYGON Then
                If Not DbcGeo1 Is Nothing Then
                    Dim PolygonToCheck As Long
                    PolygonToCheck = Dbcocx1.devreadentity(IDtemp, 0)
                    CheckCode = DbcGeo1.poCheckPolygon(0.00001, PolygonToCheck)
                    Dbcocx1.devfreeentity PolygonToCheck
                    If CheckCode <> 0 Then
                        MsgBox "The polygon just edited is not valid !", vbOKOnly, "Warning."
                    End If
                End If
            End If
            If CheckCode = 0 Then
                Exit Do
            End If
        ElseIf KeyPadSel = KPS_MOVEALL Then
            GetPoint "Choose a base point (right button to abort):", px, py
            If Dbcocx1.devlastkey() = 13 Then
                GetPointDrag "Choose a destination point (right button to abort):", px2, py2, px, py, 1
                If Dbcocx1.devlastkey() = 13 Then
                    Dbcocx1.devgrdisplayxor IDtemp, 1
                    Dbcocx1.devseek IIf(PolyType = POLYGON, "P", "E") & IDtemp
                    While Dbcocx1.deveof() = 0 And Dbcocx1.devgetid() = IDtemp And Dbcocx1.devgetfield("SECTION") = IIf(PolyType = POLYGON, "P", "E")
                        Dbcocx1.devsetfield "VX1", Trim(Str(Val(Dbcocx1.devgetfield("VX1")) + px2 - px))
                        Dbcocx1.devsetfield "VY1", Trim(Str(Val(Dbcocx1.devgetfield("VY1")) + py2 - py))
                        If PolyType <> POLYGON Then
                            Dbcocx1.devsetfield "VX2", Trim(Str(Val(Dbcocx1.devgetfield("VX2")) + px2 - px))
                            Dbcocx1.devsetfield "VY2", Trim(Str(Val(Dbcocx1.devgetfield("VY2")) + py2 - py))
                        End If
                        Dbcocx1.devskip 1
                    Wend
                    Dbcocx1.devseek IIf(PolyType = POLYGON, "P", "E") & IDtemp
                    Dbcocx1.devgrdisplayxor IDtemp, 1
                End If
            End If
        ElseIf KeyPadSel = KPS_MOVEVRTX Or KeyPadSel = KPS_DELVRTX _
                Or KeyPadSel = KPS_ADDVRTX Then
            If KeyPadSel = KPS_ADDVRTX Then
                GetPoint "Select line to add to (right button to abort):", ptx, pty
            Else
                GetPoint "Select vertex to " + IIf(KeyPadSel = KPS_MOVEVRTX, "move", "delete") + " (right button to " + IIf(KeyPadSel = KPS_MOVEVRTX, "end", "abort") + "):", ptx, pty
            End If
            Do While Dbcocx1.devlastkey() = 13
                MinDist = -1
                NearestVtx = 0
                Dbcocx1.devseek IIf(PolyType = POLYGON, "P", "E") & IDtemp
                
                Dim xPrev As Double, yPrev As Double, xMid As Double, yMid As Double
                Dim FirstVertex As Boolean: FirstVertex = True
                Do
                    If Dbcocx1.deveof() <> 0 Then Exit Do
                    If Dbcocx1.devgetfield("SECTION") <> IIf(PolyType = POLYGON, "P", "E") Then Exit Do
                    If Dbcocx1.devgetfield("ID") <> IDtemp Then Exit Do
                    If Trim(Dbcocx1.devgetfield("LAYER")) = "POLYGON" Or Trim(Dbcocx1.devgetfield("LAYER")) = "HOLE" Then FirstVertex = True
                                
                    If KeyPadSel <> KPS_ADDVRTX Then
                        If PolyType = OPEN_POLYLINE And NearestVtx = 0 Then
                            xvtx = Val(Dbcocx1.devgetfield("VX1"))
                            yvtx = Val(Dbcocx1.devgetfield("VY1"))
                            dist = Sqr(((xvtx - ptx) * (xvtx - ptx)) + ((yvtx - pty) * (yvtx - pty)))
                            If MinDist < 0 Or dist < MinDist Then
                                MinDist = dist
                                NearestVtx = Dbcocx1.devrecno()
                                firstOpenPolyline = True
                            End If
                        End If
                    End If
                    
                    If KeyPadSel = KPS_ADDVRTX Then ' take mid point of lines
                        If PolyType = POLYGON Then
                            If FirstVertex Then
                                ' determine last vertex
                                Dim curNo As Long: curNo = Dbcocx1.devrecno()
                                Dbcocx1.devskip 1
                                While Dbcocx1.deveof() = 0 And Dbcocx1.devgetid() = IDtemp And Dbcocx1.devgetfield("SECTION") = "P" And Trim(Dbcocx1.devgetfield("LAYER")) <> "POLYGON" And Trim(Dbcocx1.devgetfield("LAYER")) <> "HOLE"
                                    xPrev = Val(Dbcocx1.devgetfield("VX1"))
                                    yPrev = Val(Dbcocx1.devgetfield("VY1"))
                                    Dbcocx1.devskip 1
                                Wend
                                Dbcocx1.devgorecno curNo
                                FirstVertex = False
                            End If
                            xvtx = (xPrev + Val(Dbcocx1.devgetfield("VX1"))) / 2
                            yvtx = (yPrev + Val(Dbcocx1.devgetfield("VY1"))) / 2
                            xPrev = Val(Dbcocx1.devgetfield("VX1"))
                            yPrev = Val(Dbcocx1.devgetfield("VY1"))
                        Else
                            xvtx = (Val(Dbcocx1.devgetfield("VX1")) + Val(Dbcocx1.devgetfield("VX2"))) / 2
                            yvtx = (Val(Dbcocx1.devgetfield("VY1")) + Val(Dbcocx1.devgetfield("VY2"))) / 2
                        End If
                    Else
                        xvtx = Val(Dbcocx1.devgetfield(IIf(PolyType = POLYGON, "VX1", "VX2")))
                        yvtx = Val(Dbcocx1.devgetfield(IIf(PolyType = POLYGON, "VY1", "VY2")))
                    End If
                    dist = Sqr(((xvtx - ptx) * (xvtx - ptx)) + ((yvtx - pty) * (yvtx - pty)))
                    If MinDist < 0 Or dist < MinDist Then
                        MinDist = dist
                        NearestVtx = Dbcocx1.devrecno()
                        firstOpenPolyline = False
                        If KeyPadSel = KPS_ADDVRTX Then ' take mid point
                            xMid = xvtx
                            yMid = yvtx
                        End If
                    End If
                    Dbcocx1.devskip 1
                Loop
                If NearestVtx <> 0 Then
                    Dbcocx1.devgorecno NearestVtx
                    If KeyPadSel = KPS_ADDVRTX Then
                        xvtx = xMid
                        yvtx = yMid
                        GetPointDrag "Click on new point position (right button to abort):", ptx, pty, xvtx, yvtx, 1
                        If Dbcocx1.devlastkey() = 13 Then
                            Dbcocx1.devgrdisplayxor IDtemp, 1
                            ' the following code add the vertexes mantaining the correct
                            ' physical relative order of the vertexes (in the case of polygon)
                            ' or the line composing the polyline. This is required in order
                            ' to correctly deals with subsequent operations on the entities.
                            If PolyType = POLYGON Then
                                ' if we are on the first vertex of the current subpolygon
                                ' then we have to go forth to the last subpolygon vertex
                                Dim OnFirst As Boolean: OnFirst = False
                                If Trim(Dbcocx1.devgetfield("LAYER")) = "POLYGON" Or Trim(Dbcocx1.devgetfield("LAYER")) = "HOLE" Then
                                    OnFirst = True
                                Else
                                    Dbcocx1.devskip -1
                                    If Not (Dbcocx1.devbof() = 0 And Dbcocx1.devgetid() = IDtemp And Dbcocx1.devgetfield("SECTION") = "P") Then
                                        OnFirst = True
                                    End If
                                    Dbcocx1.devgorecno NearestVtx
                                End If
                                If OnFirst Then ' going forth to the last subpolygon vertex
                                    Dbcocx1.devskip 1
                                    While Dbcocx1.deveof() = 0 And Dbcocx1.devgetid() = IDtemp And Dbcocx1.devgetfield("SECTION") = "P" And Trim(Dbcocx1.devgetfield("LAYER")) <> "POLYGON" And Trim(Dbcocx1.devgetfield("LAYER")) <> "HOLE"
                                        NearestVtx = Dbcocx1.devrecno()
                                        Dbcocx1.devskip 1
                                    Wend
                                    Dbcocx1.devgorecno NearestVtx
                                End If
                                Dim fSection As String, fId As String, fDescr As String, fEcolor As String, fLayer As String, fEline As String, fVx1 As String, fVy1 As String, fVx2 As String, fVy2 As String, fRad As String
                                fSection = Dbcocx1.devgetfield("SECTION"): fId = Dbcocx1.devgetfield("ID")
MsgBox "devgetfield ECOLOR"
                                fDescr = Dbcocx1.devgetfield("DESCR"): fEcolor = Dbcocx1.devgetfield("ECOLOR")
                                fLayer = Dbcocx1.devgetfield("LAYER"): fEline = Dbcocx1.devgetfield("ELINE")
                                fVx1 = Dbcocx1.devgetfield("VX1"): fVy1 = Dbcocx1.devgetfield("VY1")
                                fVx2 = Dbcocx1.devgetfield("VX2"): fVy2 = Dbcocx1.devgetfield("VY2")
                                fRad = Dbcocx1.devgetfield("RAD")
                                Dim progr As Integer: progr = Val(Mid(Dbcocx1.devgetfield("DESCR"), 11))
                                FirstVertex = True
                                fVx1 = Trim(Str(ptx))
                                fVy1 = Trim(Str(pty))
                                fLayer = "" ' never begin a new island or hole on the just inserted vertex !
                                
                                If OnFirst Then Dbcocx1.devskip 1 ' pass beyond last vertex of current subpolygon
                                While Dbcocx1.deveof() = 0 And Dbcocx1.devgetid() = IDtemp And Dbcocx1.devgetfield("SECTION") = "P"
                                    Dim tSection As String, tId As String, tDescr As String, tEcolor As String, tLayer As String, tEline As String, tVx1 As String, tVy1 As String, tVx2 As String, tVy2 As String, tRad As String
                                    tSection = Dbcocx1.devgetfield("SECTION"): tId = Dbcocx1.devgetfield("ID")
                                    tEcolor = Dbcocx1.devgetfield("ECOLOR"): tLayer = Dbcocx1.devgetfield("LAYER")
                                    tEline = Dbcocx1.devgetfield("ELINE"): tVx1 = Dbcocx1.devgetfield("VX1")
                                    tVy1 = Dbcocx1.devgetfield("VY1"): tVx2 = Dbcocx1.devgetfield("VX2")
                                    tVy2 = Dbcocx1.devgetfield("VY2"): tRad = Dbcocx1.devgetfield("RAD")
                                    
                                    progr = Val(Mid(Dbcocx1.devgetfield("DESCR"), 11))
                                    
                                    Dbcocx1.devsetfield "SECTION", fSection
                                    Dbcocx1.devsetfield "ID", fId
                                    Dbcocx1.devsetfield "ECOLOR", fEcolor
                                    Dbcocx1.devsetfield "LAYER", fLayer
                                    Dbcocx1.devsetfield "ELINE", fEline
                                    Dbcocx1.devsetfield "VX1", fVx1
                                    Dbcocx1.devsetfield "VY1", fVy1
                                    Dbcocx1.devsetfield "VX2", fVx2
                                    Dbcocx1.devsetfield "VY2", fVy2
                                    Dbcocx1.devsetfield "RAD", fRad
                                    
                                    tSection = tSection: fId = tId: fEcolor = tEcolor
                                    fLayer = tLayer: fEline = tEline: fVx1 = tVx1
                                    fVy1 = tVy1: fVx2 = tVx2: fVy2 = tVy2: fRad = tRad
                                    
                                    Dbcocx1.devskip 1
                                Wend
                                Dbcocx1.devappendblank
                                Dbcocx1.devsetfield "SECTION", fSection
                                Dbcocx1.devsetfield "ID", fId
                                Dbcocx1.devsetfield "DESCR", Left(fDescr, 10) & LTrim(Str(progr + 1))
                                Dbcocx1.devsetfield "ECOLOR", fEcolor
                                Dbcocx1.devsetfield "LAYER", fLayer
                                Dbcocx1.devsetfield "ELINE", fEline
                                Dbcocx1.devsetfield "VX1", fVx1
                                Dbcocx1.devsetfield "VY1", fVy1
                                Dbcocx1.devsetfield "VX2", fVx2
                                Dbcocx1.devsetfield "VY2", fVy2
                                Dbcocx1.devsetfield "RAD", fRad
                            Else
                                Dim xPoint As String, yPoint As String
                                Dim xPoint2 As String, yPoint2 As String
                                xPoint2 = Dbcocx1.devgetfield("VX2")
                                yPoint2 = Dbcocx1.devgetfield("VY2")
                                Dbcocx1.devsetfield "VX2", Trim(Str(ptx))
                                Dbcocx1.devsetfield "VY2", Trim(Str(pty))
                                xPoint = Dbcocx1.devgetfield("VX2")
                                yPoint = Dbcocx1.devgetfield("VY2")
                                Dbcocx1.devskip 1
                                While Dbcocx1.deveof() = 0 And Dbcocx1.devgetid() = IDtemp And Dbcocx1.devgetfield("SECTION") = "E"
                                    Dbcocx1.devsetfield "VX1", xPoint
                                    Dbcocx1.devsetfield "VY1", yPoint
                                    xPoint = Dbcocx1.devgetfield("VX2")
                                    yPoint = Dbcocx1.devgetfield("VY2")
                                    Dbcocx1.devsetfield "VX2", xPoint2
                                    Dbcocx1.devsetfield "VY2", yPoint2
                                    xPoint2 = xPoint
                                    yPoint2 = yPoint
                                    xPoint = Dbcocx1.devgetfield("VX2")
                                    yPoint = Dbcocx1.devgetfield("VY2")
                                    Dbcocx1.devskip 1
                                Wend
                                Dbcocx1.devgrcmdline IDtemp, Val(xPoint), Val(yPoint), Val(xPoint2), Val(yPoint2)
                            End If
                            Dbcocx1.devgrdisplayxor IDtemp, 1
                            numVertexes = numVertexes + 1
                        End If
                        Exit Do
                    ElseIf KeyPadSel = KPS_MOVEVRTX Then
                        xvtx = Val(Dbcocx1.devgetfield(IIf(firstOpenPolyline Or PolyType = POLYGON, "VX1", "VX2")))
                        yvtx = Val(Dbcocx1.devgetfield(IIf(firstOpenPolyline Or PolyType = POLYGON, "VY1", "VY2")))
                        GetPointDrag "New position (right button to abort):", ptx, pty, xvtx, yvtx, 1
                        If Dbcocx1.devlastkey() = 13 Then
                            Dbcocx1.devgrdisplayxor IDtemp, 1
                            Dbcocx1.devsetfield IIf(firstOpenPolyline Or PolyType = POLYGON, "VX1", "VX2"), Trim(Str(ptx))
                            Dbcocx1.devsetfield IIf(firstOpenPolyline Or PolyType = POLYGON, "VY1", "VY2"), Trim(Str(pty))
                            If PolyType = CLOSED_POLYLINE Then ' it surely exists
                                Dbcocx1.devskip 1
                                If Not (Dbcocx1.deveof() = 0 And Dbcocx1.devgetid() = IDtemp And Dbcocx1.devgetfield("SECTION") = "E") Then
                                    Dbcocx1.devseek "E" & IDtemp
                                End If
                                Dbcocx1.devsetfield "VX1", Trim(Str(ptx))
                                Dbcocx1.devsetfield "VY1", Trim(Str(pty))
                            ElseIf PolyType = OPEN_POLYLINE And Not firstOpenPolyline Then
                                Dbcocx1.devskip 1
                                If Dbcocx1.deveof() = 0 And Dbcocx1.devgetid() = IDtemp And Dbcocx1.devgetfield("SECTION") = "E" Then
                                    Dbcocx1.devsetfield "VX1", Trim(Str(ptx))
                                    Dbcocx1.devsetfield "VY1", Trim(Str(pty))
                                End If
                            End If
                            Dbcocx1.devgrdisplayxor IDtemp, 1
                        End If
                    Else 'if keypadsel = KPS_DELVRTX Then
                        Dbcocx1.devgrdisplayxor IDtemp, 1
                        If PolyType = POLYGON Then
                            ' test if current subpolygon has at least four vertexes
                            Dim curVertexes As Integer: curVertexes = 0
                            Dbcocx1.devgorecno NearestVtx
                            Dbcocx1.devskip 1 ' from the following on
                            While Dbcocx1.deveof() = 0 And Dbcocx1.devgetid() = IDtemp And Dbcocx1.devgetfield("SECTION") = "P" And Trim(Dbcocx1.devgetfield("LAYER")) <> "POLYGON" And Trim(Dbcocx1.devgetfield("LAYER")) <> "HOLE"
                                curVertexes = curVertexes + 1
                                Dbcocx1.devskip 1
                            Wend
                            Dbcocx1.devgorecno NearestVtx ' current one and going back
                            Do While Dbcocx1.devbof() = 0 And Dbcocx1.devgetid() = IDtemp And Dbcocx1.devgetfield("SECTION") = "P"
                                curVertexes = curVertexes + 1
                                If Trim(Dbcocx1.devgetfield("LAYER")) = "POLYGON" Or Trim(Dbcocx1.devgetfield("LAYER")) = "HOLE" Then Exit Do
                                Dbcocx1.devskip -1
                            Loop
                            Dbcocx1.devgorecno NearestVtx
                            If curVertexes < 4 Then
                                MsgBox "You have to choose a subpolygon with at least four vertexes !", vbOKOnly, "Warning."
                            Else
                                Dim isAHoleOrIsland As String: isAHoleOrIsland = Dbcocx1.devgetfield("LAYER")
                                Dbcocx1.devskip 1
                                While Dbcocx1.deveof() = 0 And Dbcocx1.devgetid() = IDtemp And Dbcocx1.devgetfield("SECTION") = "P"
                                    If Len(Trim(isAHoleOrIsland)) > 0 Then
                                        Dbcocx1.devsetfield "LAYER", isAHoleOrIsland
                                        isAHoleOrIsland = ""
                                    End If
                                    progr = Val(Mid(Dbcocx1.devgetfield("DESCR"), 11))
                                    Dbcocx1.devsetfield "DESCR", Left(Dbcocx1.devgetfield("DESCR"), 10) & LTrim(Str(progr - 1))
                                    Dbcocx1.devskip 1
                                Wend
                                Dbcocx1.devgorecno NearestVtx
                                Dbcocx1.devdelete
                                Dbcocx1.devpack
                                numVertexes = numVertexes - 1
                            End If
                        Else
                            xvtx = Val(Dbcocx1.devgetfield("VX1"))
                            yvtx = Val(Dbcocx1.devgetfield("VY1"))
                            If PolyType = CLOSED_POLYLINE Then ' it surely exists
                                Dbcocx1.devskip 1
                                If Not (Dbcocx1.deveof() = 0 And Dbcocx1.devgetid() = IDtemp And Dbcocx1.devgetfield("SECTION") = "E") Then
                                    Dbcocx1.devseek "E" & IDtemp
                                End If
                                Dbcocx1.devsetfield "VX1", Trim(Str(xvtx))
                                Dbcocx1.devsetfield "VY1", Trim(Str(yvtx))
                                Dbcocx1.devgorecno NearestVtx
                            ElseIf PolyType = OPEN_POLYLINE And Not firstOpenPolyline Then
                                Dbcocx1.devskip 1
                                If Dbcocx1.deveof() = 0 And Dbcocx1.devgetid() = IDtemp And Dbcocx1.devgetfield("SECTION") = "E" Then
                                    Dbcocx1.devsetfield "VX1", Trim(Str(xvtx)) ' only if it does exist
                                    Dbcocx1.devsetfield "VY1", Trim(Str(yvtx))
                                End If
                                Dbcocx1.devgorecno NearestVtx
                            End If
                            Dbcocx1.devdelete
                            Dbcocx1.devpack
                            numVertexes = numVertexes - 1
                        End If
                        Dbcocx1.devgrdisplayxor IDtemp, 1
                        Exit Do
                    End If
                End If
                GetPoint "Select vertex to move (right button to end):", ptx, pty
            Loop
        End If
    Loop
    
    If KeyPadSel = KPS_END Then ' user has confirmed
        PolygonToWork = Dbcocx1.devreadentity(IDtemp, 0)
    End If
    Dbcocx1.devuse ""
    Kill App.path + "\" + TempFileName + ".dbf"
    Kill App.path + "\" + TempFileName + ".ndx"
    Dbcocx1.devselect workarea
    If KeyPadSel = KPS_END Then ' user has confirmed
        Dbcocx1.devgrcmddelete "E", id
        Dbcocx1.devpack
        Dbcocx1.devwriteentity id, PolygonToWork, 0
        Dbcocx1.devfreeentity PolygonToWork
        If PolyType = POLYGON Then Dbcocx1.devsetfield "DESCR", filling
        ModifyPoly = True
    End If
    Dbcocx1.devgrclear 0
    Dbcocx1.devgrdisplay
End Function

