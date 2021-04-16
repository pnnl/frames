Attribute VB_Name = "Modules"
Option Explicit
Option Compare Text

Type FileTyps
  Extension As String
  Qualifier As String
End Type

Type FileType
  Type As String
  Qual As String
  Min As Long
  Max As Long
  sel As Boolean
End Type

Type MultiFileType
  Spec() As FileType
End Type

Type fileIOStruct
  ftype As String
  fQual As String
  Class As Long
  numIn As Long
  numOut As Long
  sel As Boolean
End Type

Type queStruct
  Type As String
  lbl As String
  i1 As String
  i2 As String
  i3 As String
  i4 As String
  i5 As String
  i6 As String
End Type

Type varStruct
  Name As String
  vtype As String
  munit As String
  minf As String
  minv As Double
  maxf As String
  maxv As Double
  Desc As String
  nq As Long
  que() As queStruct
End Type

Type ModuleDescription
  nvers As Single
  vers As String
  Path As String           ' complete [drive:\path\filename.extension] to this file
  FileName As String       ' [filename.extension] no path
  Name As String           ' model name
  ImageIdx As Long
  GrpIdx As Long           ' index into group array
  Advert As String
  UIBat As String          ' user interface executable [drive:\path\filename.extension cmdline]
  EXEBat As String         ' model executable [drive:\path\filename.extension cmdline]
  icoPath As String
  
  nread As Long
  fread() As String
  
  nwrite As Long
  fwrite() As String
  
  nvars As Long
  vars() As varStruct
  
  reads() As MultiFileType
  writes() As MultiFileType
End Type

Type ModuleGroup
  Type As String           ' ex: Database, Model, System, etc.
  Name As String           ' ex: Constituent, Aquifer, Sensitivty/Uncertainty, etc
  Prefix As String         ' ex: con, aqu, sen, etc
  icoPath As String
  sel As Boolean
End Type
  
Public frmgrpIdx As Integer
Public IgnoreGroupInfo As Boolean
Public GroupCount As Integer
Public Group() As ModuleGroup
Public ModuleCount As Integer
Public Module() As ModuleDescription
Public BCFileCount As Integer
Public BCFile() As FileTyps

Function MapGroup(Name As String, grp As ModuleGroup) As String
  With grp
    .Name = Name
    Select Case Name
      Case "Contaminant":       .Type = DB:  .Prefix = "con": .Name = "Constituent"
      Case "Source":            .Type = MDL: .Prefix = "src"
      Case "Air":               .Type = MDL: .Prefix = "air"
      Case "Overland Flow":     .Type = MDL: .Prefix = "ovl"
      Case "Vadose Zone":       .Type = MDL: .Prefix = "vad"
      Case "Aquifer":           .Type = MDL: .Prefix = "aqu"
      Case "Surface Water":     .Type = MDL: .Prefix = "riv"
      Case "Exposure Pathways": .Type = MDL: .Prefix = "exp"
      Case "Receptor Intake":   .Type = MDL: .Prefix = "rcp"
      Case "Health Impacts":    .Type = MDL: .Prefix = "hei"
      Case "Import":            .Type = MDL: .Prefix = "mpt"
      Case "Export":            .Type = MDL: .Prefix = "xpt"
      Case "Closed":            .Type = MDL: .Prefix = "cls"
      Case "Viewer":            .Type = VWR: .Prefix = "vwr"
      Case "Sensitivity":       .Type = SYS: .Prefix = "sen"
    End Select
  End With
End Function

Public Function ConvertGlyphName(ModId As String, Typ As String, Name As String, Prefix As String) As Boolean
Dim i As Long

  ConvertGlyphName = False
  For i = 0 To GroupCount - 1
    If Group(i).Prefix = Left(ModId, Len(Group(i).Prefix)) Then
      Typ = Group(i).Type
      Name = Group(i).Name
      Prefix = Group(i).Prefix
      ConvertGlyphName = True
      Exit Function
    End If
  Next
End Function

Public Function getBCFileTypeIdx(fle As FileTyps) As Long
Dim i As Long
  
  getBCFileTypeIdx = -1
  For i = 0 To BCFileCount - 1
    If BCFile(i).Extension = fle.Extension And BCFile(i).Qualifier = fle.Qualifier Then
      getBCFileTypeIdx = i
      Exit For
    End If
  Next
End Function

Public Function AddBCFileType(fle As FileTyps) As Long
Dim i As Long
  
  AddBCFileType = -1
  i = getBCFileTypeIdx(fle)
  If i < 0 Then
    If Len(fle.Extension) = 3 And fle.Extension <> BLANK Then
      ReDim Preserve BCFile(BCFileCount)
      BCFile(BCFileCount) = fle
      AddBCFileType = BCFileCount
      BCFileCount = BCFileCount + 1
    Else
      MsgBox fle.Extension & ":" & fle.Qualifier & " boundary condition file extension must contain three vaild file name characters.", vbCritical, "Error in adding boundary condition file type!"
    End If
  Else
    AddBCFileType = i
  End If
End Function

Public Function getgrpIdxByPrefix(Prefix As String) As Long
Dim i As Long
  
  getgrpIdxByPrefix = -1
  For i = 0 To GroupCount - 1
    If Group(i).Prefix = Prefix Then
      getgrpIdxByPrefix = i
      Exit For
    End If
  Next
End Function

Public Function AddGroup(grp As ModuleGroup) As Long
Dim i As Long
  
  AddGroup = -1
  i = getgrpIdxByPrefix(grp.Prefix)
  If i < 0 Then
    If Len(grp.Prefix) = 3 And grp.Name <> BLANK And grp.Type <> BLANK Then
      ReDim Preserve Group(GroupCount)
      Group(GroupCount) = grp
      AddGroup = GroupCount
      GroupCount = GroupCount + 1
    Else
      MsgBox "Module " & grp.Type & ":" & grp.Name & ":" & grp.Prefix & " group is invalid!", vbCritical, "Error in adding module grouping!"
    End If
  Else
    With Group(i)
      If .Prefix = grp.Prefix And (grp.Name <> .Name Or grp.Type <> .Type) Then
        MsgBox "Module " & grp.Type & ":" & grp.Name & ":" & grp.Prefix & " prefix is already in use for " & .Type & ":" & .Name & ":" & .Prefix, vbCritical, "Error in adding module grouping!"
      Else
        AddGroup = i
      End If
    End With
  End If
End Function

Function Models_ParseIO(m As ModuleDescription) As Boolean
Dim i As Long
Dim j As Long
Dim k As Long
Dim num As Long
Dim cnt As Integer
Dim con As Boolean
Dim tstr As String
Dim values() As Variant
Dim bc As FileTyps
  
  If m.nwrite = 0 And m.nvers < 2.1 And m.Name = "Constituent" Then
    m.nwrite = 1
    ReDim m.fwrite(1)
    m.fwrite(1) = "con"
  End If
  
  ' retrofit models that assumed a Constiuent DB connection
  If m.nread = 0 And m.nvers < 2.1 And Group(m.GrpIdx).Type = MDL Then
    m.nread = 1
    ReDim m.fread(1)
    m.fread(1) = "1,con,,1,1"
  End If
  
  ReDim m.reads(m.nread)
  
  For i = 1 To m.nread
    csvParse m.fread(i), values(), cnt
    num = values(1)
    ReDim Preserve values(1 + (num * 4))
    ReDim m.reads(i).Spec(num)
    con = False
    For j = 1 To num
      k = 1 + (j - 1) * 4
      m.reads(i).Spec(j).Type = Trim(values(k + 1))
      m.reads(i).Spec(j).Qual = Trim(values(k + 2))
      bc.Extension = m.reads(i).Spec(j).Type
      bc.Qualifier = m.reads(i).Spec(j).Qual
      AddBCFileType bc
      m.reads(i).Spec(j).Min = values(k + 3)
      m.reads(i).Spec(j).Max = values(k + 4)
      If "con" = m.reads(i).Spec(j).Type Then con = True
    Next j
    
    If (Not con) And (m.nvers < 2.1) And (Group(m.GrpIdx).Type = MDL) Then
      num = num + 1
      ReDim Preserve m.reads(i).Spec(num)
      m.reads(i).Spec(num).Type = "con"
      m.reads(i).Spec(num).Min = 1
      m.reads(i).Spec(num).Max = 1
      
      ' patch des specs for early versions
      ' this will minimize potential problems converting from XXX
      tstr = num
      For j = 1 To num
        tstr = tstr & "," & m.reads(i).Spec(j).Type
        tstr = tstr & "," & m.reads(i).Spec(j).Qual
        tstr = tstr & "," & m.reads(i).Spec(j).Min
        tstr = tstr & "," & m.reads(i).Spec(j).Max
      Next
      m.fread(i) = tstr
    End If
  Next
  
  ReDim m.writes(m.nwrite)
  For i = 1 To m.nwrite
    csvParse m.fwrite(i), values(), cnt
    ReDim m.writes(i).Spec(1)
    m.writes(i).Spec(1).Type = Trim(values(1))
    bc.Extension = m.writes(i).Spec(1).Type
    If 1 < cnt Then
      m.writes(i).Spec(1).Qual = Trim(values(2))
      bc.Qualifier = m.writes(i).Spec(1).Qual
    Else
      bc.Qualifier = m.writes(i).Spec(1).Qual
    End If
    AddBCFileType bc
  Next
  
  Models_ParseIO = True

End Function

Function ReadModuleDescription(fname As String, m As ModuleDescription) As Boolean
Dim i As Long
Dim j As Long
Dim pos1 As Long
Dim pos2 As Long
Dim pos3 As Long
Dim temp As String
Dim grp As ModuleGroup
Dim fle As csv
  
  ReadModuleDescription = False
  
  m.Path = fname
  m.nvers = 0
  m.nread = 0
  m.nwrite = 0
  m.nvars = 0
  
  If Not open_csv(fle, fname, F_READ) Then Exit Function
  
  'read to verify this is a Metafile (mf) and the correct version
  temp = get_val(fle)
  m.vers = get_val(fle)
  get_line fle
  If Not temp = "mf" Then GoTo ExitRead
  If 0 < InStr(m.vers, "Version") Then m.nvers = Val(Mid(m.vers, InStr(m.vers, " ")))
  If m.nvers < 2# Then GoTo ExitRead

  'read execution header
  temp = get_val(fle)
  m.Name = get_val(fle)
  m.UIBat = get_val(fle)
  m.EXEBat = get_val(fle)
  m.icoPath = get_val(fle)
  get_line fle
  
  'adjust paths
  If (m.icoPath <> BLANK) And (0 = InStr(m.icoPath, ":") And 0 = InStr(m.icoPath, "\")) Then
    m.icoPath = AppPath & m.icoPath
  End If
  
  'parse module type and grouping information
  If m.nvers > 2# Then
    pos1 = InStr(temp, ":")
    If pos1 = 0 Then GoTo ExitRead
    grp.Type = Left(temp, pos1 - 1)
    temp = Mid(temp, pos1 + 1)
    pos1 = InStr(temp, ":")
    If pos1 = 0 Then GoTo ExitRead
    grp.Name = Left(temp, pos1 - 1)
    grp.Prefix = LCase(Mid(temp, pos1 + 1))
  Else
    MapGroup temp, grp
  End If
  
  'added for generalized icons - user define, plus operator
  ' all have there own unique icon,
  ' but want to share the same general icon
  Select Case grp.Name
    Case "Aquatic Organism Selector"
      grp.icoPath = AppPath & "aos.ico"
    Case "User Defined"
      grp.icoPath = AppPath & "usr.ico"
    Case "Plus Operators"
      grp.icoPath = AppPath & "pls.ico"
    Case "Sync Operators"
      grp.icoPath = AppPath & "syn.ico"
    Case Else
      grp.icoPath = m.icoPath
  End Select
  
  m.GrpIdx = AddGroup(grp)
  If m.GrpIdx < 0 Then GoTo ExitRead
  
  'read the description
  m.Advert = get_val_multiline(fle)
  get_line fle
  
  'read the number of and input schemes
  m.nread = Val(get_val(fle))
  ReDim m.fread(m.nread)
  For i = 1 To m.nread
    get_line fle
    m.fread(i) = fle.getbuff
  Next
  
  'read the number and kind of outputs
  get_line fle
  m.nwrite = Val(get_val(fle))
  ReDim m.fwrite(m.nwrite)
  For i = 1 To m.nwrite
    get_line fle
    m.fwrite(i) = fle.getbuff
  Next
  
  'read variable declarations for GID file
  get_line fle
  m.nvars = Val(get_val(fle))
  ReDim m.vars(m.nvars)
  For i = 1 To m.nvars
    With m.vars(i)
      get_line fle
      .Name = get_val(fle)
      .vtype = get_val(fle)
      .munit = get_val(fle)
      .minf = get_val(fle)
      If (Not .minf = BLANK) Then .minv = Val(get_val(fle)) Else .minv = -1.78E+308
      .maxf = get_val(fle)
      If (Not .maxf = BLANK) Then .maxv = Val(get_val(fle)) Else .maxv = 1.78E+308
      .Desc = get_val(fle)
      .nq = Val(get_val(fle))
      ReDim .que(.nq)
      For j = 1 To .nq
        get_line fle
      Next
    End With
  Next
  ReadModuleDescription = True
ExitRead:
  close_csv fle
End Function

Sub ReadModules()
Dim errmsg As String
Dim FileName As String
Dim tmp As ModuleDescription
Dim mods As New Collection
  
  On Error Resume Next
  
  ReDim Group(0)
  Group(0).Type = SYS
  Group(0).Name = "Conceptual Site Model"
  Group(0).Prefix = CSM
  GroupCount = 1
  
  ModuleCount = 0
  ReDim Module(0)
  
  BCFileCount = 0
  ReDim BCFile(0)
  
  errmsg = BLANK
  FileName = Dir$(AppPath & "*" & DOT_DES)
  While FileName <> BLANK
    If ReadModuleDescription(AppPath & FileName, tmp) Then
      If Models_ParseIO(tmp) Then
        If ModuleCount > UBound(Module) Then ReDim Preserve Module(ModuleCount * 2)
        mods.Add ModuleCount, tmp.Name
        If Err.Number <> 0 Then
          errmsg = errmsg & vbCrLf & "Duplicate module name, failed reading " & AppPath & FileName
          errmsg = errmsg & vbCrLf & "Matched module name for " & AppPath & Module(mods(tmp.Name)).FileName
          Err.Clear
        Else
          Module(ModuleCount) = tmp
          Module(ModuleCount).FileName = FileName
          ModuleCount = ModuleCount + 1
        End If
      Else
        errmsg = errmsg & vbCrLf & "Failed parsing " & AppPath & FileName
      End If
    Else
      errmsg = errmsg & vbCrLf & "Failed reading " & AppPath & FileName
    End If
    FileName = Dir()
  Wend
  ReDim Preserve Module(ModuleCount - 1)
  If errmsg <> BLANK Then MsgBox errmsg
End Sub

Sub csvParse(Line As String, csv(), cnt As Integer)
Dim fle As csv
Dim c As String
Dim t
   
  t = Split(Line, ",")
   
  fle.fnum = 1
  fle.getbuff = Line
  fle.leng = Len(Line)
  fle.separator = ","
  
  ReDim csv(100)
  cnt = 0
  While (0 < fle.leng)
    c = get_val(fle)
    cnt = cnt + 1
    csv(cnt) = c
  Wend
  ReDim Preserve csv(cnt)
End Sub

Sub getSourceModules(siteIdx As Long, glyphIdx As Long, gsrc() As Long, msrc() As Long, modelsonly As Boolean)
Dim i As Long
Dim x As Long
Dim r As Long
Dim c As Long

' find all glyphIdx that source this glyphIdx
' gsrc contains an indices into .Glyphs() sources of .Glyphs(glyphIdx)
' msrc contains an indices into Module() sources of .Glyphs(glyphIdx)  -- really don't need, have module index from above

  ReDim msrc(0)
  ReDim gsrc(0)
  With Sites(siteIdx)
    For i = 0 To .NumGlyphs - 1
      If .connect(i, glyphIdx) = OSNK And .Glyphs(i).state >= MODULE_OK Then
        If .Glyphs(i).modIdx >= 0 Then
          If (Not modelsonly) Or (modelsonly And Group(.Glyphs(i).GrpIdx).Type = MDL) Then
            x = 1 + UBound(msrc)
            ReDim Preserve msrc(x)
            ReDim Preserve gsrc(x)
            msrc(x) = .Glyphs(i).modIdx
            gsrc(x) = i
            
            'writing something here why?
           ' For r = 1 To Module(.Glyphs(i).modIdx).nwrite
           '   For c = 1 To UBound(Module(.Glyphs(i).modIdx).writes(r).Spec())
           '     Module(.Glyphs(i).modIdx).writes(r).Spec(c).sel = True
           '   Next
           ' Next
          End If
        End If
      End If
    Next
  End With
End Sub

Sub getSinkModules(siteIdx As Long, glyphIdx As Long, gsrc() As Long, msrc() As Long, gsnk() As Long, msnk() As Long, modelsonly As Boolean)
Dim i As Long
Dim j As Long
Dim n As Long
  
' find all glyphIdx that sink this glyphIdx
' gsnk contains an indices into .Glyphs() sinks of .Glyphs(glyphIdx)
' msnk contains an indices into Module() sinks of .Glyphs(glyphIdx)  -- really don't need, have module index from above
  
  ReDim gsnk(0)
  ReDim msnk(0)
  With Sites(siteIdx)
    For i = 1 To UBound(gsrc)
      If Module(.Glyphs(gsrc(i)).modIdx).nwrite > 0 Then
        For j = 0 To .NumGlyphs - 1
          If j <> glyphIdx Then
            If .connect(gsrc(i), j) = OSNK And .Glyphs(j).state >= MODULE_OK Then
              If .Glyphs(j).modIdx >= 0 Then
                If (Not modelsonly) Or (modelsonly And Group(.Glyphs(j).GrpIdx).Type = MDL) Then
                  n = 1 + UBound(gsnk)
                  ReDim Preserve msnk(n)
                  ReDim Preserve gsnk(n)
                  msnk(n) = .Glyphs(j).modIdx
                  gsnk(n) = j
                End If
              End If
            End If
          End If
        Next
      End If
    Next
  End With
End Sub

Sub GetMatchingViewers(siteIdx As Long, glyphIdx As Long, cls As String, msel() As Boolean)
Dim i As Long, c As Long, r As Long, s As Long, j As Long
Dim Prefix As String
Dim gsrc() As Long, msrc() As Long
Dim gsnk() As Long, msnk() As Long
Dim sel As Boolean
Dim fio() As fileIOStruct
Dim gsrc1() As Long, msrc1() As Long
  
  ReDim gsrc(1): gsrc(1) = glyphIdx
  ReDim msrc(1): msrc(1) = Sites(siteIdx).Glyphs(glyphIdx).modIdx
  
 ' if this is not a model then the available input may be limited by
 ' other sinks connected to the same source - find out...
  ReDim fio(0)
  If Sites(siteIdx).Glyphs(glyphIdx).state >= MODULE_OK Then
    ReDim gsrc1(1): gsrc1(1) = gsrc(1)
    ReDim msrc1(1): msrc1(1) = msrc(1)
    getSinkModules siteIdx, glyphIdx, gsrc1(), msrc1(), gsnk(), msnk(), True
    If 0 < UBound(msnk) Then
      getModuleOutputCounts msrc(1), fio(), False
      For i = 1 To UBound(msnk)
        For j = 1 To Module(msnk(i)).nread
          MatchProduceToConsume Module(msnk(i)).reads(j).Spec, fio(), False
        Next j
      Next i
      For i = 1 To Module(msrc(1)).nwrite
        Module(msrc(1)).writes(i).Spec(1).sel = fio(i, 1).sel
      Next i
    Else
      For i = 1 To Module(msrc(1)).nwrite
        Module(msrc(1)).writes(i).Spec(1).sel = True
      Next i
    End If
  End If
  buildFioA fio(), msrc(), True
  cls = "vwr"
  sel = haveConnectingModule(cls, fio(), msel(), True)
End Sub

Sub GetMatchingModule(siteIdx As Long, glyphIdx As Long, msel() As Boolean)
  Dim i As Long, c As Long, r As Long, s As Long, j As Long
  Dim gsrc() As Long, msrc() As Long
  Dim gsnk() As Long, msnk() As Long
  Dim sel As Boolean
  Dim fio() As fileIOStruct
  Dim gsrc1() As Long, msrc1() As Long
  Dim Prefix As String
  
  getSourceModules siteIdx, glyphIdx, gsrc(), msrc(), False
  If Group(Sites(siteIdx).Glyphs(glyphIdx).GrpIdx).Type <> MDL Then
   ' if this is not a model then the available input may be limited by
   ' other sinks connected to the same source - find out...
    ReDim fio(0)
    For s = 1 To UBound(gsrc)
      If Sites(siteIdx).Glyphs(gsrc(s)).state >= MODULE_OK Then
        ReDim gsrc1(1)
        ReDim msrc1(1)
        gsrc1(1) = gsrc(s)
        msrc1(1) = msrc(s)
        getSinkModules siteIdx, glyphIdx, gsrc1(), msrc1(), gsnk(), msnk(), True
        If 0 < UBound(msnk) Then
          getModuleOutputCounts msrc(s), fio(), True
          For i = 1 To UBound(msnk)
            For j = 1 To Module(msnk(i)).nread
              MatchProduceToConsume Module(msnk(i)).reads(j).Spec, fio(), False
            Next j
          Next i
          For i = 1 To Module(msrc(s)).nwrite
            Module(msrc(s)).writes(i).Spec(1).sel = fio(i, 1).sel
          Next i
        Else
          For i = 1 To Module(msrc(s)).nwrite
            Module(msrc(s)).writes(i).Spec(1).sel = True
          Next i
        End If
      End If
    Next s
    buildFioA fio(), msrc(), True
  Else
    buildFioA fio(), msrc(), False
  End If
  
  sel = haveConnectingModule(Group(Sites(currentSite).Glyphs(glyphIdx).GrpIdx).Prefix, fio(), msel(), True)
End Sub

Sub Models_show(lbapp As ListBox, lbna As ListBox, siteIdx As Long, glyphIdx As Long, GrpIdx As Long)
Dim i As Long
Dim msel() As Boolean

  GetMatchingModule siteIdx, glyphIdx, msel()

  lbapp.Clear
  lbna.Clear
  For i = 0 To ModuleCount - 1
    If msel(i) Then
      lbapp.AddItem Module(i).Name
      lbapp.ItemData(lbapp.NewIndex) = i
    Else
      If Module(i).GrpIdx = GrpIdx Then
        lbna.AddItem Module(i).Name
        lbna.ItemData(lbna.NewIndex) = i
      End If
    End If
  Next i
End Sub

Sub getModuleOutputCounts(mx As Long, fio() As fileIOStruct, selected As Boolean)
Dim m() As Long
  
  ReDim m(1 To 1)
  m(1) = mx
  buildFioA fio(), m(), selected
End Sub

Function MatchProduceToConsume(fspec() As FileType, fio() As fileIOStruct, exact As Boolean) As Boolean
Dim i As Long, sel As Boolean, found As Boolean, hit() As Boolean
Dim r As Long, c As Long

  ReDim hit(UBound(fio, 1), UBound(fio, 2))
  
  For i = 1 To UBound(fspec)
    found = False
    fspec(i).sel = False
    For r = 1 To UBound(fio, 1)
      For c = 1 To UBound(fio, 2)
        If fio(r, c).ftype = fspec(i).Type And fio(r, c).fQual = fspec(i).Qual Then
          found = True
          If fspec(i).Min <= fio(r, c).numOut And fspec(i).Max >= fio(r, c).numOut Then
            hit(r, c) = True
            fio(r, c).sel = True
            fspec(i).sel = True
          End If
        End If
      Next c
    Next r
    If exact And Not found Then
'   If Not fspec(i).sel And fspec(i).min > 0 Then
      If fspec(i).Min > 0 Then
        MatchProduceToConsume = False
        Exit Function
      End If
    End If
  Next i
  For r = 1 To UBound(fio, 1)
    sel = True
    For c = 1 To UBound(fio, 2)
      If fio(r, c).ftype <> BLANK Then
        If exact Then
          sel = sel And hit(r, c)
        Else
          sel = sel Or hit(r, c)
        End If
      End If
    Next c
    If sel Then
      MatchProduceToConsume = True
      Exit Function
    End If
  Next r
  
  MatchProduceToConsume = sel
End Function

Function anyApplicableModule(siteIdx As Long, gStart As Long, gEnd As Long) As Boolean
  Dim i As Long, j As Long, k As Long, m As Long, c As Long, mx As Long
  Dim ftype As String, fclass As Long, fmin As Long, fmax As Long
  Dim msel() As Boolean, sel As Boolean, num As Long
  Dim fio() As fileIOStruct, csv()
  Dim icls As String, ocls As String
  ReDim fio(0) As fileIOStruct
  
  ''''  answers the question: is there at least one model of sink(gEnd) type
  ''''                        that consumes file(s) produced by the source(gStart)
 anyApplicableModule = True
  
  icls = Group(Sites(siteIdx).Glyphs(gStart).GrpIdx).Prefix
  ocls = Group(Sites(siteIdx).Glyphs(gEnd).GrpIdx).Prefix
  If (MODULE_OK = Sites(siteIdx).Glyphs(gStart).state) Then
    ' a model is already selected for the starting glyphIdx
    ' get the output file characteristics for the selected model
    getModuleOutputCounts CLng(Sites(siteIdx).Glyphs(gStart).modIdx), fio(), False
    anyApplicableModule = haveConnectingModule(ocls, fio(), msel(), False)
  Else
    For k = 0 To ModuleCount - 1
      If (Group(Module(k).GrpIdx).Prefix = icls And (Module(k).nvers >= 2#)) Then
        getModuleOutputCounts k, fio(), False
        If haveConnectingModule(ocls, fio(), msel(), False) Then
          anyApplicableModule = True
          Exit Function
        End If
      End If
     Next k
  End If
End Function

Function haveConnectingModule(Prefix As String, fio() As fileIOStruct, msel() As Boolean, exact As Boolean) As Boolean
Dim i As Long
Dim j As Long
Dim sel As Boolean

  ReDim msel(ModuleCount - 1)
  haveConnectingModule = False
  For i = 0 To ModuleCount - 1
    sel = False
    If Group(Module(i).GrpIdx).Prefix = Prefix Then
      If Module(i).nvers >= 2# Then
        If (0 < UBound(fio)) Then
          For j = 1 To Module(i).nread
            sel = sel Or MatchProduceToConsume(Module(i).reads(j).Spec, fio(), exact)
            If sel Then Exit For
          Next j
        Else
          sel = (0 = Module(i).nread) ' no input requirements
        End If
      Else
        sel = True
      End If
    End If
    msel(i) = sel
    If sel Then
      haveConnectingModule = True
      If Not exact Then Exit Function
    End If
  Next
End Function

Sub LoadModuleGroupIconInfo()
Dim i As Long
Dim j As Long
Dim ct As Long
Dim cnt As Integer
Dim del As String
Dim section As String
Dim value As String
Dim grp As ModuleGroup
Dim csv()

  If IgnoreGroupInfo Then Exit Sub
  section = "ModuleTypeInfo"
  ReadIniString section, "Count", "0", value
  ct = Val(value)
  For i = 1 To ct
    If ReadIniString(section, "ModInfo" & i, BLANK, value) Then
      If value <> BLANK Then
        csvParse value, csv(), cnt
        If 1 <= UBound(csv) Then grp.Type = csv(1) Else grp.Type = BLANK
        If 2 <= UBound(csv) Then grp.Name = csv(2) Else grp.Name = BLANK
        If 3 <= UBound(csv) Then grp.Prefix = csv(3) Else grp.Prefix = BLANK
        If 4 <= UBound(csv) Then grp.icoPath = csv(4) Else grp.icoPath = BLANK
        For j = 0 To GroupCount - 1
          If Group(j).Prefix = grp.Prefix Then
            On Error Resume Next
            If Dir(grp.icoPath) <> BLANK Then Group(j).icoPath = grp.icoPath
            On Error GoTo 0
            Exit For
          End If
        Next
      End If
    End If
  Next
End Sub

Public Sub UpdateModuleGroupIconInfo()
Dim i As Long
Dim ct As Long
Dim del As String
Dim section As String
Dim value As String

  ct = 0
  del = ","
  section = "ModuleTypeInfo"
  ReadIniString section, "Count", "0", value
  ct = Val(value)
  For i = 0 To GroupCount - 1
    value = Group(i).Type & del & Group(i).Name & del & Group(i).Prefix & del & Group(i).icoPath
    WriteIniString section, "ModInfo" & i + 1, value
  Next
  For i = i To ct - 1
    WriteIniString section, "ModInfo" & i + 1, BLANK
  Next
  WriteIniString section, "Count", CStr(GroupCount)
  IgnoreGroupInfo = True
End Sub

Public Sub UpdateFileTypeInfo()
Dim i As Long
Dim ct As Long
Dim del As String
Dim section As String
Dim value As String

  ct = 0
  del = ","
  section = "FileTypeInfo"
  If ReadIniString(section, "Count", "0", value) Then ct = Val(value)
  For i = 0 To BCFileCount - 1
    value = BCFile(i).Extension & del & BCFile(i).Qualifier
    WriteIniString section, "FileInfo" & i + 1, value
  Next
  For i = i To ct - 1
    WriteIniString section, "FileInfo" & i + 1, BLANK
  Next
  WriteIniString section, "Count", CStr(BCFileCount)
End Sub

Public Sub UpdateModuleGroupIcon(Prefix As String, fname As String)
Dim i As Long
Dim value As String

  For i = 0 To GroupCount - 1
    If Group(i).Prefix = Prefix Then
      value = Group(i).Type & "," & Group(i).Name & "," & Group(i).Prefix & "," & fname
      WriteIniString "ModuleTypeInfo", "ModInfo" & i + 1, value
      Group(i).icoPath = fname
      Exit For
    End If
  Next
End Sub

Public Sub buildFioA(fio() As fileIOStruct, mx() As Long, selected As Boolean)
Dim r As Long
Dim c As Long
Dim i As Long
Dim j As Long
Dim fTMP() As fileIOStruct
Dim fout() As fileIOStruct
  
  ReDim fio(UBound(mx), 0) As fileIOStruct
  If Not 0 < UBound(mx) Then Exit Sub
  ReDim fTMP(UBound(mx), 0) As fileIOStruct
  ReDim fout(0) As fileIOStruct
  buildFioB fTMP(), fout(), mx(), 1, selected
  
  ' rotate ftmp() to fio() - easier to visualize (for BLH)
  r = UBound(fTMP, 2)
  c = UBound(fTMP, 1)
  ReDim fio(r, c)
  For i = 1 To r
    For j = 1 To c
      fio(i, j) = fTMP(j, i)
    Next j
  Next i

End Sub

Public Sub buildFioB(fio() As fileIOStruct, fout() As fileIOStruct, mx() As Long, nm As Long, selected As Boolean)
Dim i As Long
Dim j As Long
Dim w As Long
Dim ftype As String
Dim fQual As String
Dim found As Boolean
Dim fnew() As fileIOStruct

  If nm > UBound(mx) Then
   ' done  - update fio()
    If 0 < UBound(fout) Then
      i = 1 + UBound(fio, 2)
      ReDim Preserve fio(UBound(mx), i)
      For j = 1 To UBound(fout)
        fio(j, i) = fout(j)
      Next
    End If
  Else
    For w = 1 To Module(mx(nm)).nwrite
      If Not selected Or (selected And Module(mx(nm)).writes(w).Spec(1).sel) Then
        ftype = Module(mx(nm)).writes(w).Spec(1).Type
        fQual = Module(mx(nm)).writes(w).Spec(1).Qual
        found = False
        For i = 1 To UBound(fout)
          If fout(i).ftype = ftype And fout(i).fQual = fQual Then
            fout(i).numOut = fout(i).numOut + 1
            found = True
            Exit For
          End If
        Next
        If Not found Then
          ReDim fnew(UBound(fout))
          For i = 1 To UBound(fout)
            fnew(i) = fout(i)
          Next
          i = 1 + UBound(fnew)
          ReDim Preserve fnew(i)
          fnew(i).ftype = ftype
          fnew(i).fQual = fQual
          fnew(i).numOut = 1
          buildFioB fio(), fnew(), mx(), nm + 1, selected
        Else
          buildFioB fio(), fout(), mx(), nm + 1, selected
        End If
      End If
    Next
  End If
End Sub
