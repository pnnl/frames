Attribute VB_Name = "Modulo1"
Public MapData As Database
Public Allotments As Recordset
Public Maps As Recordset
Public DbgrOpened As Boolean

Public Sub CloseDbgr(Optional ResetLastSelectedEntityId As Boolean = False)
  frmMain.Dbcocx1.devselect 1
  frmMain.Dbcocx1.devuse "" ' frmMain.Dbcocx1.devcloseall
  layers(0).opened = False
  DbgrOpened = False
End Sub

Sub CloseTables()
    MapData.Close
End Sub

Public Sub OpenDbgr(ByVal temp As Boolean)
Dim workarea As Long
    frmMain.Dbcocx1.devselect 1
    frmMain.Dbcocx1.devuse DBTableName
    layers(0).opened = True
    SelectedLayer = 0
    If temp Then
    Dim area As Integer
        
        frmMain.Dbcocx1.devselect 0
        frmMain.Dbcocx1.devgrcreate App.path & "\temp", 0
        frmMain.Dbcocx1.devgrcmdlayerdef "0", 1, 1
        frmMain.Dbcocx1.devselect 1
    End If
    DbgrOpened = True
End Sub
Sub OpenTables()
    Set MapData = OpenDatabase(App.path & "\data\mapdata.mdb")
End Sub

Public Function min(a As Variant, b As Variant) As Variant
    If a < b Then
        min = a
    Else
        min = b
    End If
End Function
Public Function max(a As Variant, b As Variant) As Variant
    If a > b Then
        max = a
    Else
        max = b
    End If
End Function

Public Function GetRandomWorkArea() As Integer
  ' Random # between 100 and 200
  GetRandomWorkArea = Int((100 * Rnd) + 100)
End Function


Public Function SelectDbgr(ix As Long, use As Boolean, Optional idbc As Dbcocx = Nothing) As Boolean
Dim tempname As String, dbc As Dbcocx

  If idbc Is Nothing Then
    Set dbc = frmMain.Dbcocx1
  Else
    Set dbc = idbc
  End If
  
  If ix >= 0 And ix <= UBound(layers) Then
    With layers(ix)
      If ix = 0 Then
        .workarea = 1
        tempname = DBTableName
      Else
        If Not .workarea > 0 Then
          .workarea = GetRandomWorkArea()
        End If
      End If
      dbc.devselect .workarea
      If use Then
        If "shp" = LCase(Right(.filename, 3)) Then
          tempname = DBFileDir & "~temp" & CStr(.workarea)
          If "" = dir$(tempname & ".dbf") Then
            frmMain.StatusBar1.Panels(1).text = "Please wait while loading data for " & .filename & " ..."
            dbc.devgrcreateext tempname, 0
            dbc.devgrappend .filename
            frmMain.StatusBar1.Panels(1).text = ""
          End If
        End If
        If Not .opened Then
          dbc.devuse tempname
          .opened = True
        End If
        SelectedLayer = ix
      Else
        dbc.devuse ""
        SelectedLayer = -1
        .opened = False
      End If
    End With
    SelectDbgr = True
  Else
    SelectDbgr = False
  End If
End Function

