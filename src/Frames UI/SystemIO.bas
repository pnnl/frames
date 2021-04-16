Attribute VB_Name = "SytemIO"
Option Explicit
Option Compare Text

Type srcId
  src() As String
End Type

Dim SiteName() As String
Global conList As Collection
 
Function Commit_Writes(marker As String, errfile As Boolean) As Integer
Dim i As Long
Dim j As Long
Dim pos1 As Long
Dim numChanges As Integer
Dim numFiles As Integer
Dim file() As String
Dim numSval As Integer
Dim sval()
Dim TMPfile As String
Dim OLDfile As String
Dim PDCfile As String
Dim Extension As String
Dim fTMP As Integer
Dim fOLD As Integer
Dim fPDC As Integer
Dim section As String
Dim numLines As Long
Dim parm As parmrec
Dim p As ParmCls
Dim pfilein As parmfile
Dim numrec As Long
Dim errmsg As String
Dim wrnmsg As String
Dim ExtType As String
Dim buffer As String
Dim ConParm As Collection

  On Error Resume Next

  ' get list of files produced by module
  numFiles = 0
  TMPfile = Dir$(TmpTitle & DOT_STAR)
  While (BLANK <> TMPfile)
    numFiles = numFiles + 1
    ReDim Preserve file(numFiles)
    file(numFiles) = LongGidDir & TMPfile
    TMPfile = Dir()
  Wend

  ' commit each file produced to thier respective PDCF
  errmsg = BLANK
  wrnmsg = BLANK
  errfile = False
  numChanges = 0
  For i = 1 To numFiles
    Extension = SplitPath(file(i), SP_EXT)
    PDCfile = LongGidTitle & Extension
    OLDfile = LongGidDir & Extension
    SetAttr PDCfile, vbNormal
    SetAttr OLDfile, vbNormal
    Kill OLDfile
    
    Select Case Extension
      Case DOT_TMP
        ' ignore [TmpTitle].tmp files
        ' any file with a ".tmp" extension will be deleted
        ' when the like named ".gid" file is closed
      Case DOT_ERR
        errfile = True
        errmsg = errmsg + vbCrLf + "** Error file contents start here for "
        errmsg = errmsg + Sites(currentSite).Glyphs(currentGlyph).label + " ("
        errmsg = errmsg + Sites(currentSite).Glyphs(currentGlyph).name + ") **" + vbCrLf
        fTMP = FreeFile
        Open file(i) For Input As #fTMP
        errmsg = errmsg + Input(FileLen(file(i)), #fTMP) + vbCrLf
        errmsg = errmsg + "** Error file contents end here for "
        errmsg = errmsg + Sites(currentSite).Glyphs(currentGlyph).label + " ("
        errmsg = errmsg + Sites(currentSite).Glyphs(currentGlyph).name + ") **" + vbCrLf
        Close #fTMP
#If UI Then
        rtb.Text = rtb.Text + errmsg
#End If
      Case Else
        ExtType = "auxillary"
        For j = 0 To BCFileCount
          If dot & BCFile(j).Extension = Extension Then ExtType = "boundary condition"
        Next
        wrnmsg = vbCrLf + "** Committing transaction file " + file(i) + " for (" + marker + ") **" + vbCrLf
#If UI Then
        rtb.Text = rtb.Text + wrnmsg
#End If
        
        If Extension = DOT_WRN Then
          wrnmsg = vbCrLf + "** Warning file contents start here for "
          wrnmsg = wrnmsg + Sites(currentSite).Glyphs(currentGlyph).label + " ("
          wrnmsg = wrnmsg + Sites(currentSite).Glyphs(currentGlyph).name + ") **" + vbCrLf
          fTMP = FreeFile
          Open file(i) For Input As #fTMP
          wrnmsg = wrnmsg + Input(FileLen(file(i)), #fTMP) + vbCrLf
          wrnmsg = wrnmsg + "** Warning file contents end here for "
          wrnmsg = wrnmsg + Sites(currentSite).Glyphs(currentGlyph).label + " ("
          wrnmsg = wrnmsg + Sites(currentSite).Glyphs(currentGlyph).name + ") **" + vbCrLf
          Close #fTMP
#If UI Then
          rtb.Text = rtb.Text + wrnmsg
#End If
        End If
        
        'Grab constituent list for old version models
        If Extension = DOT_GID Then
          ExtType = "module input"
          If InStr(marker, "con") = 1 Then
            If open_parm(pfilein, file(i), F_READ) Then
              Set ConParm = New Collection
              Do Until EOCF(pfilein.file)
                If read_parmrec(pfilein, parm) Then
                  Set p = New ParmCls
                  With parm
                    p.setparm .pname, .idx1, .idx2, .idx3, .idx4, .idx5, .idx6, .ref, .uunit, .cunit, .pval
                  End With
                  ConParm.Add p
                End If
              Loop
              conList.Remove marker
              conList.Add ConParm, marker
              close_parm pfilein
            Else
              errmsg = errmsg & "Cannot read constituent selection file:" & file(i) & vbCrLf
            End If
          End If
        End If

        Err.Clear
        fPDC = FreeFile
        If BLANK <> Dir$(PDCfile) Then Name PDCfile As OLDfile
        Open PDCfile For Binary Access Read Write As #fPDC
        If Err.Number = 0 Then
          ' copy all old records except for this marker
          fOLD = FreeFile
          Open OLDfile For Input As #fOLD
          If Err = 0 Then
            While Not EOF(fOLD)
              Line Input #fOLD, buffer
              csvParse buffer, sval, numSval
              section = sval(1)
              numLines = Val(sval(2))
              If Err = 0 Then
                Select Case ValidGlyphName(Extension, section)
                  Case BLANK, marker
                    For j = 1 To numLines
                      Line Input #fOLD, buffer
                    Next
                  Case Else
                    buffer = """" & section & """," & CStr(numLines) & vbCrLf
                    Put #fPDC, , buffer
                    For j = 1 To numLines
                      Line Input #fOLD, buffer
                      If Err = 0 Then
                        buffer = buffer & vbCrLf
                        Put #fPDC, , buffer
                      Else
                        errmsg = errmsg & Err.Description & vbCrLf & "Cannot read record " & ExtType & " file: " & OLDfile & vbCrLf
                        Err.Clear
                      End If
                    Next
                End Select
              Else
                errmsg = errmsg & Err.Description & vbCrLf & "Cannot read marker in " & ExtType & " file: " & OLDfile & vbCrLf
                Err.Clear
              End If
            Wend
            Close #fOLD
          Else
            If Err.Number <> 53 Then
              errmsg = errmsg & Err.Description & vbCrLf & "Cannot access " & ExtType & " file: " & OLDfile & vbCrLf
            End If
            Err.Clear
          End If
          
          ' insert new records for this marker
          fTMP = FreeFile
          Open file(i) For Input As fTMP
          If LOF(fTMP) > 0 Then
            If Err = 0 Then
              pos1 = Seek(fPDC)
              numLines = 0
              buffer = Space(40) & vbCrLf
              Put #fPDC, , buffer
              While Not EOF(fTMP)
                Line Input #fTMP, buffer
                If Err = 0 Then
                  buffer = buffer & vbCrLf
                  Put #fPDC, , buffer
                  numLines = numLines + 1
                Else
                  errmsg = errmsg & Err.Description & vbCrLf & "Cannot read record " & ExtType & " file: " & file(i) & vbCrLf
                  Err.Clear
                End If
              Wend
              Seek fPDC, pos1
              buffer = """" & marker & """," & CStr(numLines)
              Put #fPDC, , buffer
            Else
              errmsg = errmsg & Err.Description & vbCrLf & "Cannot read " & ExtType & " file: " & file(i) & vbCrLf
              Err.Clear
            End If
          End If
          Close #fTMP
          Close #fPDC
          numChanges = numChanges + 1
        Else
          errmsg = errmsg & Err.Description & vbCrLf & "Cannot write " & ExtType & " file: " & PDCfile & vbCrLf
          Err.Clear
        End If
        Kill OLDfile
    End Select
  Next
  
  If errmsg <> BLANK Or errfile Then
    #If UI Then
      MsgBox errmsg, vbOKOnly, "ERROR in Commit Transactions"
    #End If
    errfile = True
  End If
  Commit_Writes = numChanges
  Close
End Function

Function Sites_Read(fname As String) As Boolean
Dim i As Long
Dim j As Long
Dim rc As Long
Dim temp As parmrec
Dim pfile As parmfile
Dim vers As Single
Dim p As ParmCls
Dim ConName As String
Dim ConParm As Collection
  
  vers = 0
  Sites_Read = False
  rc = LoadGIDData(pfile, fname, FUI)
  Read_Single "Version", 0, 0, 0, 0, 0, 0, vers
  If vers < App.Major Or vers < App.Major + App.Minor Or vers = 0 Then oldVersion = True
  'clear error older gid files don't have a version number
  readErr = BLANK
  Read_Long "Sites", 0, 0, 0, 0, 0, 0, NumSites
  ReDim SiteName(MAX_SITES)
  For i = 0 To NumSites - 1
    Read_String "SiteName", i + 1, 0, 0, 0, 0, 0, SiteName(i)
    Sites(i).NumGlyphs = 0
  Next
  If readErr <> BLANK Then
    MsgBox readErr
    Exit Function
  End If
  
  'Must host CON section in FUI section for backward compatibility.
  Set conList = New Collection
  For i = 1 To NumSites
    Read_String "ConName", i, 1, 0, 0, 0, 0, ConName
    rc = 0
    If ConName <> BLANK Then
      rc = FindGidMarker(pfile, fname, ConName)
    Else
    End If
    If rc > 0 Then
      get_line pfile.file
      Set ConParm = New Collection
      For j = 1 To rc
        read_parmrec pfile, temp
        Set p = New ParmCls
        With temp
          p.setparm .pname, .idx1, .idx2, .idx3, .idx4, .idx5, .idx6, .ref, .uunit, .cunit, .pval
        End With
        ConParm.Add p
      Next
      conList.Add ConParm, ConName
    End If
    close_parm pfile
  Next
  readErr = BLANK
  
  'get connectivity
  Sites_Read_CSM fname
  If readErr <> BLANK Then
    If MsgBox(readErr & vbCrLf & "Old GID try to recover?", vbOKCancel) = vbOK Then
      readErr = ""
      ReDim Sites(MAX_SITES)
      Sites_Read_FUI fname
    Else
      Exit Function
    End If
  End If
  If readErr <> BLANK Then
    MsgBox readErr
    Exit Function
  End If
  
  ReDim parmList(0)
  
  ' update picture
  For i = 0 To NumSites - 1
    For j = 0 To Sites(i).NumGlyphs - 1
       If Sites(i).Glyphs(j).modIdx < 0 Then UpdateDownStreamGlyphs i, j, NO_MODULE
    Next
  Next
  
  Sites_Read = True
End Function

Sub Sites_Read_CSM(fname As String)
  Dim i As Long, j As Long, k As Long, rc As Long
  Dim pfile As parmfile, src As Long, srcId As String, name As String
  Dim Prefix As String, gc As Long, map() As Long
  Dim mx As Long
  
  ' 4/01 - Viewers are now be accessed from the module popupmenu so they are
  '        excluded from the glyphs and connections - BLH
  
  rc = LoadGIDData(pfile, fname, CSM)
  Prefix = "Mod"
  For i = 0 To NumSites - 1
    gc = 0
    Sites(i).name = SiteName(i)
    Read_Long "NumMod", i + 1, 0, 0, 0, 0, 0, Sites(i).NumGlyphs
    ReDim map(Sites(i).NumGlyphs)
    For j = 0 To Sites(i).NumGlyphs - 1
      map(j) = -1
      Read_String Prefix & "Id", i + 1, j + 1, 0, 0, 0, 0, name
'      If "vwr" <> Left(Name, 3) Then
        map(j) = gc
        With Sites(i).Glyphs(gc)
          .name = name
          Read_String Prefix & "Label", i + 1, j + 1, 0, 0, 0, 0, .label
          Read_String Prefix & "Model", i + 1, j + 1, 0, 0, 0, 0, .Model
          Read_Single Prefix & "LocX", i + 1, j + 1, 0, 0, 0, 0, .x
          Read_Single Prefix & "LocY", i + 1, j + 1, 0, 0, 0, 0, .y
          Read_Single Prefix & "LocZ", i + 1, j + 1, 0, 0, 0, 0, .z
          Read_Single Prefix & "ScrX", i + 1, j + 1, 0, 0, 0, 0, .sx
          Read_Single Prefix & "ScrY", i + 1, j + 1, 0, 0, 0, 0, .sy
          Read_Long Prefix & "State", i + 1, j + 1, 0, 0, 0, 0, .State
          .GrpIdx = getgrpIdxByPrefix(Left(.name, 3))
          .modIdx = getmodIdxByName(.Model)
          If .modIdx < 0 Then
            .State = NO_MODULE
            If .GrpIdx < 0 Then
              Load frmMapGroup
              frmMapGroup.PrepMapGroup .name, .Model, .label
              frmMapGroup.Show vbModal
              .GrpIdx = frmgrpIdx
         '     If .Name <> BLANK Then .Name = Group(.GrpIdx).Prefix & CStr(Right(.Name, Len(.Name) - 3))
         '     .Model = BLANK
            End If
          Else
            If .GrpIdx <> Module(.modIdx).GrpIdx Then
              .GrpIdx = Module(.modIdx).GrpIdx
'              .Name = Group(.grpIdx).Prefix & CStr(Right(.Name, Len(.Name) - 3))
'              .State = MODULE_OK
            End If
          End If
        End With
        gc = gc + 1
'      End If
    Next j
    
    For j = 0 To Sites(i).NumGlyphs - 1
      If map(j) >= 0 Then
        Read_Long Prefix & "SrcNum", i + 1, j + 1, 0, 0, 0, 0, src
        For k = 1 To src
          Read_String Prefix & "SrcId", i + 1, j + 1, k, 0, 0, 0, srcId
          ConnectGlyphs i, map(j), srcId
        Next
      End If
    Next
    Sites(i).NumGlyphs = gc  ' see note 4/01
  Next
End Sub

Sub Sites_Read_FUI(fname As String)
Dim i As Long
Dim j As Long
Dim k As Long
Dim m As Long
Dim rc As Long
Dim gc As Long
Dim tmp As String
Dim saveErr As String
Dim Prefix As String
Dim ConstituentDB As String
Dim srcNum As Long
Dim src() As srcId
Dim pfile As parmfile
  
  ' April 2001 - Viewers can now be accessed from the module popupmenu so they are
  '     excluded from the glyphs and connections
  
  rc = LoadGIDData(pfile, fname, FUI)
  For i = 0 To NumSites - 1
    gc = 0
    ConstituentDB = ""
    ReDim src(0)
    Sites(i).name = SiteName(i)
    For j = 1 To GroupCount - 1
      Prefix = Group(j).Prefix
'      If Prefix <> "vwr" Then  ' see note April 2001
        'try to read num if no num then clear readerr
        Read_Long Prefix & "Num", i + 1, 0, 0, 0, 0, 0, rc
        If readErr <> BLANK Then readErr = BLANK
        For k = 1 To rc
          With Sites(i).Glyphs(gc)
            Read_String Prefix & "Name", i + 1, k, 0, 0, 0, 0, .name
            Read_String Prefix & "Label", i + 1, k, 0, 0, 0, 0, .label
            Read_String Prefix & "Model", i + 1, k, 0, 0, 0, 0, .Model
            If Prefix = "con" Then ConstituentDB = .name
          
            saveErr = readErr
            readErr = BLANK
            Read_Single Prefix & "X", i + 1, k, 0, 0, 0, 0, .x
            Read_Single Prefix & "Y", i + 1, k, 0, 0, 0, 0, .y
            Read_Single Prefix & "Z", i + 1, k, 0, 0, 0, 0, .z
            readErr = saveErr
            
            Read_Single Prefix & "ScrX", i + 1, k, 0, 0, 0, 0, .sx
            Read_Single Prefix & "ScrY", i + 1, k, 0, 0, 0, 0, .sy
            Read_Long Prefix & "ModelStat", i + 1, k, 0, 0, 0, 0, .State
            .GrpIdx = getgrpIdxByPrefix(Left(.name, 3))
            .modIdx = getmodIdxByName(.Model)
            If .modIdx < 0 Then
              .State = NO_MODULE
              If .GrpIdx < 0 Then
                Load frmMapGroup
                frmMapGroup.PrepMapGroup .name, .Model, .label
                frmMapGroup.Show vbModal
                .GrpIdx = frmgrpIdx
'                .Name = Group(.GrpIdx).Prefix & CStr(Right(.Name, Len(.Name) - 3))
'                .Model = BLANK
              End If
            Else
              If .GrpIdx <> Module(.modIdx).GrpIdx Then
                .GrpIdx = Module(.modIdx).GrpIdx
'                .Name = Group(.grpIdx).Prefix & CStr(Right(.Name, Len(.Name) - 3))
'                .State = MODULE_OK
              End If
            End If
          End With
          
          saveErr = readErr
          readErr = BLANK
          tmp = "SrcName"
          Read_Long Prefix & "SrcNum", i + 1, k, 0, 0, 0, 0, srcNum
          If 0 = srcNum Then
            Read_Long Prefix & "TypeNum", i + 1, k, 0, 0, 0, 0, srcNum
            If 0 = srcNum Then
              Read_Long Prefix & "RCPNum", i + 1, k, 0, 0, 0, 0, srcNum
              If 0 < srcNum Then tmp = "RCP"
            End If
          End If
          readErr = saveErr
          
          ReDim Preserve src(gc)
          ReDim Preserve src(gc).src(srcNum)
          For m = 1 To srcNum
            Read_String Prefix & tmp, i + 1, k, m, 0, 0, 0, src(gc).src(m)
          Next m
          gc = gc + 1
        Next k
'      End If
    Next j
    Sites(i).NumGlyphs = gc
      
    For j = 0 To Sites(i).NumGlyphs - 1
      For k = 1 To UBound(src(j).src())
        ConnectGlyphs i, j, src(j).src(k)
      Next k
      tmp = Group(Sites(i).Glyphs(j).GrpIdx).Type
      If ConstituentDB <> BLANK And tmp <> DB And tmp <> VWR And tmp <> SYS Then
        ConnectGlyphs i, j, ConstituentDB
      End If
    Next j
      
    ' assign id's to all glyphs before IDs
    For j = 0 To Sites(i).NumGlyphs - 1
      If Sites(i).Glyphs(j).id = 0 Then
        ' This must be an old gid file
        lastId = lastId + 1
        Sites(i).Glyphs(j).id = lastId
        Sites(i).Glyphs(j).label = Sites(i).Glyphs(j).name
      End If
    Next j
  Next i
End Sub

Sub Sites_Write_CSM(fname As String)
Dim i As Integer
Dim j As Integer
Dim k As Integer
Dim siteIdx As Long
Dim glyphIdx As Long
Dim r As Long
Dim c As Long
Dim src As Long
Dim snk As Long
Dim sCount As Long
Dim fCount As Long
Dim msrc() As Long  'glyphIdx' src module indices
Dim gsrc() As Long  'glyphIdx' src glyphIdx indices
Dim msnk() As Long  'glyphIdx' snk module indices
Dim gsnk() As Long  'glyphIdx' snk glyphIdx indices
Dim found As Boolean
Dim ftype As String
Dim fQual As String
Dim dfType As String
Dim dfQual As String
Dim Prefix As String
Dim pfile As parmfile
Dim icon As Glyph
Dim fio() As fileIOStruct

Dim m1 As ModuleDescription
Dim m2 As ModuleDescription


  Prefix = "Mod"
  On Error GoTo Sites_Write_CSM_Exit
  If BLANK <> Dir(fname) Then Kill fname
  If Not open_parm(pfile, fname, F_WRITE) Then Exit Sub
  write_param pfile, "Version", 0, 0, 0, 0, 0, 0, 0, NOT_APP, NOT_APP, App.Major & dot & App.Minor
  write_param pfile, "Sites", 0, 0, 0, 0, 0, 0, 0, NOT_APP, NOT_APP, NumSites
  For siteIdx = 0 To NumSites - 1
    write_param pfile, "SiteName", siteIdx + 1, 0, 0, 0, 0, 0, 0, NOT_APP, NOT_APP, Sites(siteIdx).name
    write_param pfile, "Num" & Prefix, siteIdx + 1, 0, 0, 0, 0, 0, 0, NOT_APP, NOT_APP, Sites(siteIdx).NumGlyphs
    For glyphIdx = 0 To Sites(siteIdx).NumGlyphs - 1
      icon = Sites(siteIdx).Glyphs(glyphIdx)
      write_param pfile, Prefix & "Id", siteIdx + 1, glyphIdx + 1, 0, 0, 0, 0, 0, NOT_APP, NOT_APP, icon.name
      write_param pfile, Prefix & "Label", siteIdx + 1, glyphIdx + 1, 0, 0, 0, 0, 0, NOT_APP, NOT_APP, icon.label
      write_param pfile, Prefix & "Model", siteIdx + 1, glyphIdx + 1, 0, 0, 0, 0, 0, NOT_APP, NOT_APP, icon.Model
      write_param pfile, Prefix & "DesPath", siteIdx + 1, glyphIdx + 1, 0, 0, 0, 0, 0, NOT_APP, NOT_APP, getDesPath(icon.modIdx)
      write_param pfile, Prefix & "LocX", siteIdx + 1, glyphIdx + 1, 0, 0, 0, 0, 0, KM, KM, icon.x
      write_param pfile, Prefix & "LocY", siteIdx + 1, glyphIdx + 1, 0, 0, 0, 0, 0, KM, KM, icon.y
      write_param pfile, Prefix & "LocZ", siteIdx + 1, glyphIdx + 1, 0, 0, 0, 0, 0, KM, KM, icon.z
      write_param pfile, Prefix & "ScrX", siteIdx + 1, glyphIdx + 1, 0, 0, 0, 0, 0, NOT_APP, NOT_APP, icon.sx
      write_param pfile, Prefix & "ScrY", siteIdx + 1, glyphIdx + 1, 0, 0, 0, 0, 0, NOT_APP, NOT_APP, icon.sy
      write_param pfile, Prefix & "State", siteIdx + 1, glyphIdx + 1, 0, 0, 0, 0, 0, NOT_APP, NOT_APP, icon.State
      src = 0
      snk = 0
      
      'write out 'glyphIdx' sources and the fType and fQual
      For j = 0 To Sites(siteIdx).NumGlyphs - 1
        If Sites(siteIdx).connect(glyphIdx, j) = ISRC Then
          src = src + 1
          write_param pfile, Prefix & "SrcId", siteIdx + 1, glyphIdx + 1, src, 0, 0, 0, 0, NOT_APP, NOT_APP, Sites(siteIdx).Glyphs(j).name
          write_param pfile, Prefix & "SrcLabel", siteIdx + 1, glyphIdx + 1, src, 0, 0, 0, 0, NOT_APP, NOT_APP, Sites(siteIdx).Glyphs(j).label
          
          If icon.modIdx >= 0 And Sites(siteIdx).Glyphs(j).modIdx >= 0 Then
            m1 = Module(icon.modIdx)
            m2 = Module(Sites(siteIdx).Glyphs(j).modIdx)
            sCount = 0
            ftype = BLANK
            fQual = BLANK
            For c = 1 To m1.nread
              fCount = 0
              dfType = BLANK
              dfQual = BLANK
              For r = 1 To m2.nwrite
                For i = 1 To UBound(m1.reads(c).Spec)
                  If m1.reads(c).Spec(i).Type = m2.writes(r).Spec(1).Type And _
                     m1.reads(c).Spec(i).Qual = m2.writes(r).Spec(1).Qual Then
                    If Not (dfType = BLANK) Then dfType = dfType & ","
                    dfType = dfType & m2.writes(r).Spec(1).Type
                    If Not (dfQual = BLANK) Then dfQual = dfQual & ","
                    dfQual = dfQual & m2.writes(r).Spec(1).Qual
                    fCount = fCount + 1
                  End If
                Next
              Next
              If fCount > sCount Then
                ftype = dfType
                fQual = dfQual
                sCount = fCount
              End If
            Next
            write_param pfile, Prefix & "SrcType", siteIdx + 1, glyphIdx + 1, src, 0, 0, 0, 0, NOT_APP, NOT_APP, ftype
            write_param pfile, Prefix & "SrcQual", siteIdx + 1, glyphIdx + 1, src, 0, 0, 0, 0, NOT_APP, NOT_APP, fQual
          End If
        End If
        
        If Sites(siteIdx).connect(glyphIdx, j) = OSNK Then
          snk = snk + 1
          write_param pfile, Prefix & "SinkId", siteIdx + 1, glyphIdx + 1, snk, 0, 0, 0, 0, NOT_APP, NOT_APP, Sites(siteIdx).Glyphs(j).name
          write_param pfile, Prefix & "SinkLabel", siteIdx + 1, glyphIdx + 1, snk, 0, 0, 0, 0, NOT_APP, NOT_APP, Sites(siteIdx).Glyphs(j).label
          If icon.modIdx >= 0 And Sites(siteIdx).Glyphs(j).modIdx >= 0 Then
            m2 = Module(icon.modIdx)
            m1 = Module(Sites(siteIdx).Glyphs(j).modIdx)
            sCount = 0
            ftype = BLANK
            fQual = BLANK
            For c = 1 To m1.nread
              fCount = 0
              dfType = BLANK
              dfQual = BLANK
              For r = 1 To m2.nwrite
                For i = 1 To UBound(m1.reads(c).Spec)
                  If m1.reads(c).Spec(i).Type = m2.writes(r).Spec(1).Type And _
                     m1.reads(c).Spec(i).Qual = m2.writes(r).Spec(1).Qual Then
                    If Not (dfType = BLANK) Then dfType = dfType & ","
                    dfType = dfType & m2.writes(r).Spec(1).Type
                    If Not (dfQual = BLANK) Then dfQual = dfQual & ","
                    dfQual = dfQual & m2.writes(r).Spec(1).Qual
                    fCount = fCount + 1
                  End If
                Next
              Next
              If fCount > sCount Then
                ftype = dfType
                fQual = dfQual
                sCount = fCount
              End If
            Next
            write_param pfile, Prefix & "SinkType", siteIdx + 1, glyphIdx + 1, snk, 0, 0, 0, 0, NOT_APP, NOT_APP, ftype
            write_param pfile, Prefix & "SinkQual", siteIdx + 1, glyphIdx + 1, snk, 0, 0, 0, 0, NOT_APP, NOT_APP, fQual
          End If
        End If
      Next
      write_param pfile, Prefix & "SrcNum", siteIdx + 1, glyphIdx + 1, 0, 0, 0, 0, 0, NOT_APP, NOT_APP, src
      write_param pfile, Prefix & "SinkNum", siteIdx + 1, glyphIdx + 1, 0, 0, 0, 0, 0, NOT_APP, NOT_APP, snk
    Next
  Next

Sites_Write_CSM_Exit:
  
  close_parm pfile
End Sub

Sub Sites_Write_FUI(fname As String)
Dim i As Long
Dim j As Long
Dim srcNum As Long
Dim rcpNum As Long
Dim siteIdx As Long
Dim glyphIdx As Long
Dim GrpIdx As Long
Dim Prefix As String
Dim pfile As parmfile
Dim parm As parmrec
Dim ConParm As Collection
  
  On Error GoTo Sites_Write_Exit
  If BLANK <> Dir(fname) Then Kill fname
  If Not open_parm(pfile, fname, F_WRITE) Then Exit Sub
  
  ' NOTE:  App.Major and App.Minor MUST BE the same as Frames.vbp for this to work correctly
  write_param pfile, "Version", 0, 0, 0, 0, 0, 0, 0, NOT_APP, NOT_APP, App.Major & dot & App.Minor
  write_param pfile, "Sites", 0, 0, 0, 0, 0, 0, 0, NOT_APP, NOT_APP, NumSites
  For siteIdx = 1 To NumSites
    With Sites(siteIdx - 1)
    write_param pfile, "SiteName", siteIdx, 0, 0, 0, 0, 0, 0, NOT_APP, NOT_APP, .name
    glyphIdx = 0
    For GrpIdx = 0 To GroupCount - 1
      Prefix = Group(GrpIdx).Prefix
      For i = 0 To .NumGlyphs - 1
        If Prefix = Group(.Glyphs(i).GrpIdx).Prefix Then
          With .Glyphs(i)
            srcNum = 0
            rcpNum = 0
            glyphIdx = glyphIdx + 1
            write_param pfile, Prefix & "Name", siteIdx, glyphIdx, 0, 0, 0, 0, 0, NOT_APP, NOT_APP, .name
            write_param pfile, Prefix & "Label", siteIdx, glyphIdx, 0, 0, 0, 0, 0, NOT_APP, NOT_APP, .label
            write_param pfile, Prefix & "Model", siteIdx, glyphIdx, 0, 0, 0, 0, 0, NOT_APP, NOT_APP, .Model
            write_param pfile, Prefix & "DesPath", siteIdx, glyphIdx, 0, 0, 0, 0, 0, NOT_APP, NOT_APP, getDesPath(.modIdx)
            write_param pfile, Prefix & "X", siteIdx, glyphIdx, 0, 0, 0, 0, 0, KM, KM, .x
            write_param pfile, Prefix & "Y", siteIdx, glyphIdx, 0, 0, 0, 0, 0, KM, KM, .y
            write_param pfile, Prefix & "Z", siteIdx, glyphIdx, 0, 0, 0, 0, 0, KM, KM, .z
            write_param pfile, Prefix & "ScrX", siteIdx, glyphIdx, 0, 0, 0, 0, 0, NOT_APP, NOT_APP, .sx
            write_param pfile, Prefix & "ScrY", siteIdx, glyphIdx, 0, 0, 0, 0, 0, NOT_APP, NOT_APP, .sy
            write_param pfile, Prefix & "ModelStat", siteIdx, glyphIdx, 0, 0, 0, 0, 0, NOT_APP, NOT_APP, .State
          End With
          For j = 0 To .NumGlyphs - 1
            If .connect(i, j) = ISRC Then
              If (Group(.Glyphs(j).GrpIdx).Type <> DB And Group(.Glyphs(j).GrpIdx).Type <> SYS) Then
                srcNum = srcNum + 1
                write_param pfile, Prefix & "SrcName", siteIdx, glyphIdx, srcNum, 0, 0, 0, 0, NOT_APP, NOT_APP, .Glyphs(j).name
                write_param pfile, Prefix & "Type", siteIdx, glyphIdx, srcNum, 0, 0, 0, 0, NOT_APP, NOT_APP, Group(.Glyphs(j).GrpIdx).name
              End If
            End If
          Next
          write_param pfile, Prefix & "SrcNum", siteIdx, glyphIdx, 0, 0, 0, 0, 0, NOT_APP, NOT_APP, srcNum
          write_param pfile, Prefix & "TypeNum", siteIdx, glyphIdx, 0, 0, 0, 0, 0, NOT_APP, NOT_APP, srcNum
        End If

      Next
      If glyphIdx > 0 Then
        write_param pfile, Prefix & "Num", siteIdx, 0, 0, 0, 0, 0, 0, NOT_APP, NOT_APP, glyphIdx
      End If
      glyphIdx = 0
    Next
    End With
  Next
  
  ' Repeat "CON" section (ConList) for backward compatibility
  For i = 1 To conList.Count
    For j = 1 To conList.item(i).Count
      Set ConParm = conList.item(i)
      With parm
       ConParm.item(j).getparm .pname, .idx1, .idx2, .idx3, .idx4, .idx5, .idx6, .ref, .uunit, .cunit, .pval
      End With
      If parm.pname = "FSCASID" Or parm.pname = "FSCNAME" Then
        write_sparmrec pfile, parm
      Else
        write_parmrec pfile, parm
      End If
    Next
  Next
  
Sites_Write_Exit:
  On Error Resume Next
  close_parm pfile
  
End Sub
