Attribute VB_Name = "DoAll"
Option Explicit
Option Compare Text

Dim modRoots() As Long
Dim numRoots As Long
Function hasCompleteInfoRecurse(ByVal site&, ByVal glyph&, ByVal recurse As Boolean) As Boolean
Dim getInfo As Boolean, i, success As Boolean

  success = True

  If recurse Then
    For i = 0 To Sites(site&).Count - 1
      If Sites(site&).Connect(glyph&, i) And (Sites(site&).SrcSnk(glyph&, i) = ISRC) Then
        success = hasCompleteInfoRecurse(site&, i, True)
        If Not success Then Exit For
      End If
    Next
  End If
  
  ' now it's my turn
  If success Then
    CurrentSite = site
    CurrentGlyph = glyph
    hasCompleteInfoRecurse = False
    If 0 >= getModelIndex(Sites(CurrentSite).Glyphs(CurrentGlyph).mname) Then
      getInfo = True
      Sites(CurrentSite).Glyphs(CurrentGlyph).mname = ""
      Sites(CurrentSite).Glyphs(CurrentGlyph).Status = STATUS_MODULE_NO
    Else
      getInfo = (Sites(CurrentSite).Glyphs(CurrentGlyph).Status = STATUS_MODULE_NO)
    End If
    If getInfo Then
      InfoClick
      If 0 >= getModelIndex(Sites(CurrentSite).Glyphs(CurrentGlyph).mname) Then
        Exit Function
      End If
    End If
    success = (Sites(CurrentSite).Glyphs(CurrentGlyph).Status >= STATUS_MODULE_OK)
  End If
  
  hasCompleteInfoRecurse = success
End Function

Function hasCompleteInfo(ByVal site&, ByVal glyph&) As Boolean
Dim getInfo As Boolean
  hasCompleteInfo = False
  CurrentSite = site
  CurrentGlyph = glyph
  
  If 0 >= getModelIndex(Sites(CurrentSite).Glyphs(CurrentGlyph).mname) Then
    getInfo = True
    Sites(CurrentSite).Glyphs(CurrentGlyph).mname = ""
    Sites(CurrentSite).Glyphs(CurrentGlyph).Status = STATUS_MODULE_NO
  Else
    getInfo = (Sites(CurrentSite).Glyphs(CurrentGlyph).Status = STATUS_MODULE_NO)
  End If
  If getInfo Then
    InfoClick
    If 0 >= getModelIndex(Sites(CurrentSite).Glyphs(CurrentGlyph).mname) Then
      Exit Function
    End If
  End If
' hasCompleteInfo = True
  hasCompleteInfo = (Sites(CurrentSite).Glyphs(CurrentGlyph).Status >= STATUS_MODULE_OK)
End Function

Public Sub DoAll(ByVal mode As String, ByVal FuiName As String, ByVal RunName As String, ByVal SiteIndex As Integer)
Dim i As Long, cls As Long, gStart As Integer, success As Boolean, isErr As Boolean
Dim con As Boolean

  doMod = mode
  
 ' first check that all glyphs are connected correctly
  For gStart = 0 To Sites(SiteIndex).Count - 1
    If Not hasRequiredConnections(SiteIndex, gStart) Then Exit Sub
  Next gStart
  
  If Not GidFileSave(False) Then Exit Sub
  
  If doMod <> "FUI" Then
    ' when called by sensitivity
    numRoots = -1
    For gStart = 0 To Sites(SiteIndex).Count - 1
      If (Sites(SiteIndex).Glyphs(gStart).Name = doMod) Then
        lookupRoots SiteIndex, gStart
      End If
    Next gStart
  End If

  For gStart = 0 To Sites(SiteIndex).Count - 1
    If Models(getModelIndex(Sites(SiteIndex).Glyphs(gStart).mname)).nread = 0 Then ' V1.2
    ' If (Sites(siteIndex).Glyphs(gStart).Class = ClassSrc) Then
      success = doModuleIO(SiteIndex, gStart, "UI")
      If Not success Then Exit For
    End If
  Next gStart
  If success Then
    For gStart = 0 To Sites(SiteIndex).Count - 1
      If Models(getModelIndex(Sites(SiteIndex).Glyphs(gStart).mname)).nread = 0 Then ' V1.2
        'If (Sites(siteIndex).Glyphs(gStart).Class = ClassSrc) Then
        success = doModuleIO(SiteIndex, gStart, "EXE")
        If Not success Then Exit For
      End If
    Next gStart
  End If
  
  Sites_Write_FUI RunName & ".gid"
  Commit_Transactions "FUI", isErr
  Sites_Write_CSM RunName & ".gid"
  Commit_Transactions "CSM", isErr

End Sub

Function doModuleIO(ByVal SiteIndex As Integer, ByVal gStart As Integer, ByVal mode As String) As Boolean
Dim i As Long, doInput As Long, doRun As Long, cls As String, success As Boolean
Dim msg As String, inc As Long, srcs() As Long


  If (doMod = "FUI") Or (doMod <> "FUI" And isRoot(gStart)) Then
    doModuleIO = False
    
    ' check connect
    If Not hasRequiredConnections(SiteIndex, gStart) Then Exit Function
            
    ' select module if not already
    If Not hasCompleteInfo(SiteIndex, gStart) Then Exit Function
    
    ' do input and run test
    doInput = False
    doRun = Sites(SiteIndex).Glyphs(gStart).Status > STATUS_MODULE_OK And hasRunModule(SiteIndex, gStart)
    If Not doRun Then
      If hasInputModule(SiteIndex, gStart) Then
        doInput = True
      ElseIf hasRunModule(SiteIndex, gStart) Then
        doRun = True
      End If
    End If
    
    ' input if applicable
    success = True
    If mode = "UI" And doInput And Sites(SiteIndex).Glyphs(gStart).Status < STATUS_INPUT_OK Then
      ' do input
      If RequisiteInputComplete(SiteIndex, gStart, cls, inc) Then
        success = hasCompleteInfo(SiteIndex, gStart)
        If success Then success = doUI(SiteIndex, gStart)
      Else
        success = False
        If cls = "Constituent" Then
          msg = "Constituent selection must be completed before continuing."
        Else
          If 1 = getSources(SiteIndex, gStart, srcs()) Then
            msg = "First complete input for object: " & Sites(SiteIndex).Glyphs(inc).Name
          Else
            ' if multiple sources then they will be completed by other paths
            doModuleIO = True
            Exit Function
          End If
        End If
        MsgBox msg
      End If
    End If
    
    ' run if applicable
    If mode = "EXE" And doRun And Sites(SiteIndex).Glyphs(gStart).Status < STATUS_RUN_OK Then
      If Not (doMod = Sites(SiteIndex).Glyphs(gStart).Name) Then
        ' the above test should prevent the sensitivity module from calling itself
        inc = RequisiteRunsIncomplete(SiteIndex, gStart)
        If 0 > inc Then
          success = hasCompleteInfo(SiteIndex, gStart)
          If success Then success = doModule(SiteIndex, gStart)
        Else
          success = False
          If 1 = getSources(SiteIndex, gStart, srcs()) Then
            msg = "First complete the run for object: " & Sites(SiteIndex).Glyphs(inc).Name
            MsgBox msg
          Else
            ' if multiple sources then they will be completed by other paths
            doModuleIO = True
            Exit Function
          End If
        End If
      End If
    End If
  Else
    success = True
  End If
  
  ' look for next glyph to process
  If success Then
    For i = 0 To Sites(SiteIndex).Count - 1
      If Sites(SiteIndex).Connect(gStart, i) And (Sites(SiteIndex).SrcSnk(gStart, i) = OSNK) Then
        If Sites(SiteIndex).Glyphs(i).group = "Model" _
          Or Sites(SiteIndex).Glyphs(i).group = "Database" Then            ' V1.2
          success = doModuleIO(SiteIndex, i, mode)
          If Not success Then Exit For
        End If
      End If
    Next
  End If
  
  doModuleIO = success
  
End Function

Function isRoot(glyph As Integer) As Boolean
Dim i As Long

  For i = 1 To numRoots
    If (modRoots(i) = glyph) Then
      isRoot = True
      Exit Function
    End If
  Next
  isRoot = False
End Function

Sub addRoot(glyph As Integer)
  If isRoot(glyph) Then Exit Sub
  numRoots = numRoots + 1
  If numRoots > UBound(modRoots) Then ReDim Preserve modRoots(numRoots + 10)
  modRoots(numRoots) = glyph
End Sub

Sub lookupRoots(site As Integer, glyph As Integer)
Dim i As Integer
  
  If numRoots < 0 Then
    ReDim modRoots(10)
    numRoots = 1
    modRoots(1) = glyph
  End If
  
  For i = 0 To Sites(site).Count - 1
    If Sites(site).Connect(i, glyph) And (OSNK = Sites(site).SrcSnk(i, glyph)) Then
      lookupRoots site, i
      addRoot i
    End If
  Next
  ReDim Preserve modRoots(numRoots)
End Sub

Sub doAllNew2(ByVal mode As String, ByVal FuiName As String, ByVal RunName As String, ByVal SiteIndex&)
Dim i As Long, j As Long, cls As Long
Dim success As Boolean, isErr As Boolean
Dim con As Boolean, nsnk As Long, nSrc As Long

  doMod = mode
        
  ' first check that the glyph has a module defined
  ' validate only glyphs that are connected  12/00
  For j = 0 To Sites(SiteIndex).Count - 1
    ' not essential for viewers, important for models and databases
    If Sites(SiteIndex).Glyphs(j).group <> "Viewer" Then
      nsnk = 0
      nSrc = 0
      For i = 0 To Sites(SiteIndex).Count - 1
        If Sites(SiteIndex).Glyphs(i).group <> "Viewer" Then
          If Sites(SiteIndex).Connect(j, i) Then
            If Sites(SiteIndex).SrcSnk(j, i) = OSNK Then nsnk = nsnk + 1
            If Sites(SiteIndex).SrcSnk(j, i) = ISRC Then nSrc = nSrc + 1
          End If
        End If
      Next i
'     If 0 < nsnk Or 0 < nSrc Then
      If 0 = nsnk And 0 < nSrc Then
        If Not hasCompleteInfoRecurse(SiteIndex, j, True) Then Exit Sub
      End If
    End If
  Next j
        
  
  ' next check that the glyph is connected
  For j = 0 To Sites(SiteIndex).Count - 1
    ' not essential for viewers, important for models and databases
    If Sites(SiteIndex).Glyphs(j).group <> "Viewer" Then
      nsnk = 0
      nSrc = 0
      For i = 0 To Sites(SiteIndex).Count - 1
        If Sites(SiteIndex).Glyphs(i).group <> "Viewer" Then
          If Sites(SiteIndex).Connect(j, i) Then
            If Sites(SiteIndex).SrcSnk(j, i) = OSNK Then nsnk = nsnk + 1
            If Sites(SiteIndex).SrcSnk(j, i) = ISRC Then nSrc = nSrc + 1
          End If
        End If
      Next i
'     If 0 < nsnk Or 0 < nSrc Then
      If 0 = nsnk And 0 < nSrc Then
'       If Not hasRequiredConnections(siteIndex, j) Then Exit Sub
        If Not hasRequiredConnectionsRecurse(SiteIndex, j, True) Then Exit Sub
      End If
    End If
  Next j
  
  If Not GidFileSave(False) Then Exit Sub
  
' If doMod <> "FUI" Then
'   ' when called by sensitivity
'   numRoots = -1
'   For i = 0 To Sites(siteIndex).Count - 1
'     If (Sites(siteIndex).Glyphs(i).name = doMod) Then
'       lookupRoots siteIndex, i
'     End If
'   Next i
' End If
  
  For j = 0 To Sites(SiteIndex).Count - 1
    If doMod = "FUI" Then
      nsnk = 0
      nSrc = 0
      For i = 0 To Sites(SiteIndex).Count - 1
        If Sites(SiteIndex).Connect(j, i) Then
          If Sites(SiteIndex).SrcSnk(j, i) = OSNK Then nsnk = nsnk + 1
          If Sites(SiteIndex).SrcSnk(j, i) = ISRC Then nSrc = nSrc + 1
        End If
      Next i
      If 0 = nsnk And 0 < nSrc Then
'     If 0 = nSrc And 0 < nsnk Then
        ' check the state upstream
        success = doModuleIONew(SiteIndex, j, "UI")
        If Not success Then Exit For
      End If
    Else
      If Sites(SiteIndex).Glyphs(j).Name = doMod Then
        success = doModuleIONew(SiteIndex, j, "UI")
        Exit For
      End If
    End If
  Next
  
  If success Then
    For j = 0 To Sites(SiteIndex).Count - 1
      If doMod = "FUI" Then
        nsnk = 0
        nSrc = 0
        For i = 0 To Sites(SiteIndex).Count - 1
          If Sites(SiteIndex).Connect(j, i) Then
            If Sites(SiteIndex).SrcSnk(j, i) = OSNK Then nsnk = nsnk + 1
            If Sites(SiteIndex).SrcSnk(j, i) = ISRC Then nSrc = nSrc + 1
          End If
        Next i
        If 0 = nsnk And 0 < nSrc Then
'       If 0 = nSrc And 0 < nsnk Then
          ' check the state upstream
          success = doModuleIONew(SiteIndex, j, "EXE")
          If Not success Then Exit For
        End If
      Else
      If Sites(SiteIndex).Glyphs(j).Name = doMod Then
        success = doModuleIONew(SiteIndex, j, "EXE")
        Exit For
      End If
    End If
    Next j
  End If
  
  Sites_Write_FUI RunName & ".gid"
  Commit_Transactions "FUI", isErr
  Sites_Write_CSM RunName & ".gid"
  Commit_Transactions "CSM", isErr

End Sub

Function doModuleIONew(ByVal SiteIndex As Integer, ByVal gStart As Integer, ByVal mode As String) As Boolean
Dim i As Long, doInput As Boolean, doRun As Boolean, cls As String, success As Boolean
Dim msg As String, inc As Long, srcs() As Long, curStatus As Long


  For i = 0 To Sites(SiteIndex).Count - 1
    If Sites(SiteIndex).Connect(gStart, i) And (Sites(SiteIndex).SrcSnk(gStart, i) = ISRC) Then
      success = doModuleIONew(SiteIndex, i, mode)
      If Not success Then
        doModuleIONew = False
        Exit Function
      End If
    End If
  Next
  
  
  ' now its my turn
  
  If Sites(SiteIndex).Glyphs(gStart).group = "Viewer" _
     Or Sites(SiteIndex).Glyphs(gStart).group = "Sensitivity" Then
       doModuleIONew = True
       Exit Function
  End If
    
            
  ' select module if not already
  ' If Not hasCompleteInfo(siteIndex, gStart) Then Exit Function  ' already done
    
  ' check connect
  ' If Not hasRequiredConnections(siteIndex, gStart) Then Exit Function ' already done
    
  ' do input and run test
  doInput = False
  doRun = Sites(SiteIndex).Glyphs(gStart).Status > STATUS_MODULE_OK And hasRunModule(SiteIndex, gStart)
  
  If Not doRun Then
    If hasInputModule(SiteIndex, gStart) Then
      doInput = True
    ElseIf hasRunModule(SiteIndex, gStart) Then
      doRun = True
    End If
  End If
    
  ' input if applicable
  success = True
  If mode = "UI" And doInput And Sites(SiteIndex).Glyphs(gStart).Status < STATUS_INPUT_OK Then
  
'   If done(gStart).ui Then
'     doModuleIONew = done(gStart).uiSuccess
'     Exit Function
'   End If
    
    ' do input
    If RequisiteInputComplete(SiteIndex, gStart, cls, inc) Then
      success = hasCompleteInfo(SiteIndex, gStart)
      curStatus = Sites(SiteIndex).Glyphs(gStart).Status
      If success Then success = doUI(SiteIndex, gStart)
      ' if no change of state then this isn't completed
      If curStatus = Sites(SiteIndex).Glyphs(gStart).Status Then success = False
    Else
      success = False
      If cls = "Constituent" Then
        msg = "Constituent selection must be completed before continuing."
      Else
        If 1 = getSources(SiteIndex, gStart, srcs()) Then
          msg = "First complete input for object: " & Sites(SiteIndex).Glyphs(inc).Name
        Else
          ' if multiple sources then they will be completed by other paths
          doModuleIONew = True
          Exit Function
        End If
      End If
      MsgBox msg
    End If
  End If
    
  ' run if applicable
  If mode = "EXE" And doRun And Sites(SiteIndex).Glyphs(gStart).Status < STATUS_RUN_OK Then
  
'   If done(gStart).exe Then
'     doModuleIONew = done(gStart).exeSuccess
'     Exit Function
'   End If
      
    If Not (doMod = Sites(SiteIndex).Glyphs(gStart).Name) Then
      ' the above test should prevent the sensitivity module from calling itself
      inc = RequisiteRunsIncomplete(SiteIndex, gStart)
      If 0 > inc Then
        success = hasCompleteInfo(SiteIndex, gStart)
        curStatus = Sites(SiteIndex).Glyphs(gStart).Status
        If success Then success = doModule(SiteIndex, gStart)
        If curStatus = Sites(SiteIndex).Glyphs(gStart).Status Then success = False
      Else
        success = False
        If 1 = getSources(SiteIndex, gStart, srcs()) Then
          msg = "First complete the run for object: " & Sites(SiteIndex).Glyphs(inc).Name
          MsgBox msg
        Else
          ' if multiple sources then they will be completed by other paths
          doModuleIONew = True
          Exit Function
        End If
      End If
'     done(gStart).exe = True
'     done(gStart).exeSuccess = success
    End If
  End If
  ' if no change in state then not done
  doModuleIONew = success
End Function


