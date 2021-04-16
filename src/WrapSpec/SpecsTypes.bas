Attribute VB_Name = "SpecTypes"
Option Explicit
Option Compare Text

Global errfile As csv
Global errflag As Boolean
Global argv() As String
Global argc As Long
Global mode As String
Global FUIName As String        'the path and name of the run
Global RunName As String        'the name of the parameter file
Global RefName As String        'the name of the reference file
Global SiteIndex As Long     'the index to site model parms
Global ModIndex As Long     'the index to site model parms
Global Model As String

Global frm As Object

Dim temp As parmrec

Type glyphtype
  id As String
  lbl As String
  Path As String
  Idx As Long
  pos As Long
  Class As String
  group As String
  type As String
  ftype() As String
  fqual() As String
  fcount As Integer
  exe As String
End Type

Public Declare Function GetPrivateProfileString _
                                      Lib "kernel32" Alias "GetPrivateProfileStringA" _
                                       (ByVal lpApplicationName As String, _
                                        ByVal lpKeyName As Any, _
                                        ByVal lpDefault As String, _
                                        ByVal lpReturnedString As String, _
                                        ByVal nSize As Long, _
                                        ByVal lpFileName As String) As Long

Global errcnt As Long
Global branching As Long
Global chemlist As String
Global nameList As String
Global degList As String
Global secList As String
Global mCnt As Long
Global glyph() As glyphtype
Global modsrcid() As String

Sub getargs()
  Dim args As String
  Dim pos As Long
    
  argc = 0
  args = Trim$(Command$)
  If Len(args) > 0 Then
    Do
      pos = InStr(args, " ")
      argc = argc + 1
      If pos > 0 Then
        ReDim Preserve argv(argc) As String
        argv(argc - 1) = Trim$(Left$(args, pos))
        args = Trim$(Right$(args, Len(args) - pos))
      Else
        ReDim Preserve argv(argc)
        argv(argc - 1) = Trim$(args)
      End If
    Loop Until pos = 0
  End If
End Sub

Private Sub loadDES(glyph As glyphtype)
Dim fle As csv
Dim i As Long, j As Long, k As Long, pos As Long
Dim pCnt As Long
Dim fcount As Long
Dim ver As String, nvers As Single
Dim ftype As String
Dim fqual As String
Dim Class As String, group As String, id As String
Dim chk As String
Dim curDrive As String
Dim mypath As String

  curDrive = CurDir
  'ChDrive App.path
  
  mypath = App.Path & "\" & SplitPath(glyph.Path, SP_TITLE) & SplitPath(glyph.Path, SP_EXT)
'  If open_csv(fle, glyph.Path, 2) Then
  If open_csv(fle, mypath, 2) Then
    chk = get_val(fle)                 'des descriptor
    If chk <> "mf" Then GoTo loadDesErr
    ver = get_val(fle)                 'version descriptor
    If (InStr(ver, "beta")) Then GoTo loadDesErr
    ' assumes version appears as "Version n[.n]"
    If 0 < InStr(ver, "Version") Then
      nvers = val(Mid(ver, InStr(ver, " ")))
    Else
      GoTo loadDesErr
    End If
  
    If nvers < 2# Then GoTo loadDesErr
    get_line fle                        'the rest of the version line
    get_line fle                        'name,ui.exe,mod.exe
    get_val fle                         'model description
    get_line fle
    
    ReDim glyph.ftype(0) As String
    ReDim glyph.fqual(0) As String
    
    pCnt = val(get_val(fle))            ' number of reads
    get_line fle
    If mode = "in" Then
      For i = 1 To pCnt
        fcount = val(get_val(fle))
        For j = 1 To fcount
          ftype = get_val(fle)
          fqual = get_val(fle)
          For k = 1 To UBound(glyph.ftype)
            If ftype = glyph.ftype(k) Then Exit For
          Next
          If k > glyph.fcount Then
            glyph.fcount = k
            ReDim Preserve glyph.ftype(k) As String
            ReDim Preserve glyph.fqual(k) As String
            glyph.ftype(k) = ftype
            glyph.fqual(k) = fqual
          End If
          get_val fle
          get_val fle
        Next
        get_line fle
      Next
    Else
      For i = 1 To pCnt
        get_line fle
      Next
    End If
    
    pCnt = val(get_val(fle))
    get_line fle
    If mode = "out" Then
      glyph.fcount = pCnt
      ReDim glyph.ftype(glyph.fcount) As String
      ReDim glyph.fqual(glyph.fcount) As String
      For i = 1 To glyph.fcount
        glyph.ftype(i) = get_val(fle)
        glyph.fqual(i) = get_val(fle)
        get_line fle
      Next
    Else
      For i = 1 To pCnt
        get_line fle
      Next
    End If
    
    close_csv fle
    ChDrive curDrive
  Else
    put_val errfile, "Can't find or open file " & glyph.Path
    put_line errfile
loadDesErr:
    put_val errfile, "Error in Description file for module " & glyph.id
    put_line errfile
    close_csv errfile
    ChDrive curDrive
    End
  End If
End Sub

Private Function addGlyph(srcid As String) As Long
Dim i As Long
  For i = 1 To mCnt
    If srcid = glyph(i).group Then
      addGlyph = i
      Exit Function
    End If
  Next
  mCnt = i
  addGlyph = i
  ReDim Preserve glyph(mCnt) As glyphtype
  glyph(i).group = srcid
End Function

Private Sub addId(srcid As String)
  glyph(addGlyph(srcid)).id = temp.pval
End Sub

Private Sub addLbl(srcid As String)
  glyph(addGlyph(srcid)).lbl = temp.pval
End Sub

Private Sub addPath(srcid As String)
  glyph(addGlyph(srcid)).Path = temp.pval
End Sub

Private Sub loadFUI()
Dim k As Long
Dim m As Long
Dim clast As Long
Dim slast As Long
Dim mIdx As Long
Dim pName As String
Dim ftime As Boolean
Dim fui As parmfile

  mCnt = 0
  slast = 0
  chemlist = ""
  nameList = ""
  degList = ""
  secList = ""
  If open_parm(fui, FUIName & ".gid", 2) Then
    Do Until EOCF(fui.file)
      If read_parmrec(fui, temp) Then
        Select Case Right(temp.pName, 3)
          Case "csm"
            For m = 1 To temp.idx1
              If read_parmrec(fui, temp) Then
                If temp.idx1 = SiteIndex Then
                  Select Case temp.pName
                    Case "modid"
                      If temp.pval = Model Then mIdx = temp.idx2
                    Case "modsrcid"
                      If temp.idx2 = mIdx Then
                        ReDim Preserve modsrcid(temp.idx3)
                        modsrcid(temp.idx3) = temp.pval
                      End If
                   End Select
                End If
              End If
            Next
          
          Case "fui"
            For m = 1 To temp.idx1
              If read_parmrec(fui, temp) Then
                If temp.idx1 = SiteIndex Then
                  Select Case temp.pName
                    'nds and casid reads for deglist, seclist are dependant on order
                    Case "nds"
                        If temp.pval = 0 Then
                          If degList <> "" Then degList = degList & ","
                          degList = degList & " "
                        End If
                    Case "fscname"
                      If temp.idx3 = 0 Then
                        If nameList <> "" Then nameList = nameList & ","
                        nameList = nameList & temp.pval
                      End If
                    Case "fscasid"
                      If temp.idx3 = 0 Then
                        If chemlist <> "" Then chemlist = chemlist & ","
                        chemlist = chemlist & temp.pval
                        clast = temp.idx2
                      Else
                        If temp.idx3 = 1 Then  ' first decay order only
                          If degList <> "" Then degList = degList & ","
                          degList = degList & temp.pval
                        End If
                      End If
                    Case "sscasid"
                      If temp.idx3 = 1 Then  ' first decay order only
                        
                        ' adjust for missing secondary branch
                        If temp.idx2 > slast Then
                          For k = slast To temp.idx2 - 2
                            If secList <> "" Then secList = secList & ","
                            secList = secList & " "
                          Next
                        End If
                        
                        If secList <> "" Then secList = secList & ","
                        secList = secList & temp.pval
                        slast = temp.idx2
                      End If
                    Case Else 'grab module name, label and paths and must work for any module
                        If Len(temp.pName) > 3 Then
                          pName = Right(temp.pName, Len(temp.pName) - 3)
                          Select Case pName
                           Case "name":
                             addId Left$(temp.pName, 3) & CStr(temp.idx2)
                           Case "despath":
                             addPath Left$(temp.pName, 3) & CStr(temp.idx2)
                          End Select
                        End If
                   End Select
                End If
              End If
            Next
          Case Else
            For m = 1 To temp.idx1
              get_line fui.file
            Next
        End Select
      End If
    Loop
    close_parm fui
    
    ' adjust for missing secondary branch
    If clast > slast Then
      For k = slast To clast - 1
        If secList <> "" Then secList = secList & ","
        secList = secList & " "
      Next
    End If
    
  Else
    put_val errfile, "Can't find or open file " & FUIName & ".gid"
    put_line errfile
    close_csv errfile
    End
  End If
End Sub

Sub RecomposeFiles(glyph As glyphtype)
Dim j As Long
Dim run(7) As Boolean
  
  On Error GoTo RecomposeFilesErr
  For j = 0 To 7
    run(j) = False
  Next
  HasSections 0
  For j = 1 To glyph.fcount
    If Dir(RunName & "xx." & glyph.ftype(j)) <> "" Then Kill RunName & "xx." & glyph.ftype(j)
    Select Case glyph.ftype(j)
    Case "AFF":
    If Not run(0) Then
      'rename only once
      Name RunName & "." & glyph.ftype(j) As RunName & "xx." & glyph.ftype(j)
      affOpen RunName & "xx", Model
      affAggregate RunName & "." & glyph.ftype(j), chemlist
      affClose
      run(0) = True
    End If
    Case "WFF":
    If Not run(1) Then
      'rename only once
      Name RunName & "." & glyph.ftype(j) As RunName & "xx." & glyph.ftype(j)
      wffOpen RunName & "xx", Model
      wffAggregate RunName & "." & glyph.ftype(j), chemlist
      wffClose
      run(1) = True
    End If
    Case "WCF":
    If Not run(2) Then
      'rename only once
      Name RunName & "." & glyph.ftype(j) As RunName & "xx." & glyph.ftype(j)
      wcfOpen RunName & "xx", Model
      wcfAggregate RunName & "." & glyph.ftype(j), chemlist
      wcfClose
      run(2) = True
    End If
    Case "SCF":
    If Not run(3) Then
      'rename only once
      Name RunName & "." & glyph.ftype(j) As RunName & "xx." & glyph.ftype(j)
      scfOpen RunName & "xx" & Chr(0), Model & Chr(0)
      scfAggregate RunName & "." & glyph.ftype(j), chemlist
      scfClose
      run(3) = True
    End If
    Case "ATO":
    If Not run(4) Then
      'rename only once
      Name RunName & "." & glyph.ftype(j) As RunName & "xx." & glyph.ftype(j)
      atoOpen RunName & "xx" & Chr(0), Model & Chr(0)
      atoAggregate RunName & "." & glyph.ftype(j), chemlist
      atoClose
      run(4) = True
    End If
    Case "EPF":
    If Not run(5) Then
      'rename only once
      Name RunName & "." & glyph.ftype(j) As RunName & "xx." & glyph.ftype(j)
      epfOpen RunName & "xx", Model
      epfAggregate RunName & "." & glyph.ftype(j), chemlist
      epfClose
      run(5) = True
    End If
    Case "RIF":
    If Not run(6) Then
      'rename only once
      Name RunName & "." & glyph.ftype(j) As RunName & "xx." & glyph.ftype(j)
      rifOpen RunName & "xx", Model
      rifAggregate RunName & "." & glyph.ftype(j), chemlist
      rifClose
      run(6) = True
    End If
    Case "HIF":
    If Not run(7) Then
      'rename only once
      Name RunName & "." & glyph.ftype(j) As RunName & "xx." & glyph.ftype(j)
      hifOpen RunName & "xx", Model
      hifAggregate RunName & "." & glyph.ftype(j), chemlist
      hifClose
      run(7) = True
    End If
    End Select
    If Dir(RunName & "xx." & glyph.ftype(j)) <> "" Then Kill RunName & "xx." & glyph.ftype(j)

rNextFileType:
  
  Next
  Exit Sub

RecomposeFilesErr:
    put_val errfile, "Error in recomposing file for module " & glyph.id & " filetype " & glyph.ftype(j)
    put_line errfile
    errcnt = errcnt + 1
    GoTo rNextFileType
End Sub

Sub DecomposeFiles(glyph As glyphtype)
Dim i As Long
Dim j As Long
Dim run(7) As Boolean
  
  On Error GoTo DecomposeFilesErr
  For j = 0 To 7
    run(j) = False
  Next
  For j = 1 To glyph.fcount
    If Dir(RunName & "xx." & glyph.ftype(j)) <> "" Then Kill RunName & "xx." & glyph.ftype(j)
    Select Case glyph.ftype(j)
    Case "AFF":
    If Not run(0) Then
      'rename only once
      On Error GoTo dNextFileType
      Name FUIName & "." & glyph.ftype(j) As FUIName & "xx." & glyph.ftype(j)
      On Error GoTo DecomposeFilesErr
      For i = 1 To UBound(modsrcid)
        affOpen FUIName & "xx", modsrcid(i)
        If affGetNumSets() > 0 Then affInsert FUIName & "." & glyph.ftype(j), chemlist, nameList, degList, secList, branching
        affClose
      Next
      run(0) = True
    End If
    Case "WFF":
    If Not run(1) Then
      'rename only once
      On Error GoTo dNextFileType
      Name FUIName & "." & glyph.ftype(j) As FUIName & "xx." & glyph.ftype(j)
      On Error GoTo DecomposeFilesErr
      For i = 1 To UBound(modsrcid)
        wffOpen FUIName & "xx", modsrcid(i)
        If wffGetNumSets() > 0 Then wffInsert FUIName & "." & glyph.ftype(j), chemlist, nameList, degList, secList, branching
        wffClose
      Next
      run(1) = True
    End If
    Case "SCF":
    If Not run(2) Then
      'rename only once
      On Error GoTo dNextFileType
      Name FUIName & "." & glyph.ftype(j) As FUIName & "xx." & glyph.ftype(j)
      On Error GoTo DecomposeFilesErr
      For i = 1 To UBound(modsrcid)
        scfOpen FUIName & "xx", modsrcid(i)
        If scfGetNumSets() > 0 Then scfInsert FUIName & "." & glyph.ftype(j), chemlist, nameList, degList, secList, branching
        scfClose
      Next
      run(2) = True
    End If
    Case "WCF":
    If Not run(3) Then
      'rename only once
      On Error GoTo dNextFileType
      Name FUIName & "." & glyph.ftype(j) As FUIName & "xx." & glyph.ftype(j)
      On Error GoTo DecomposeFilesErr
      For i = 1 To UBound(modsrcid)
        wcfOpen FUIName & "xx", modsrcid(i)
        If wcfGetNumSets() > 0 Then wcfInsert FUIName & "." & glyph.ftype(j), chemlist, nameList, degList, secList, branching
        wcfClose
      Next
      run(3) = True
    End If
    Case "ATO":
    If Not run(4) Then
      'rename only once
      On Error GoTo dNextFileType
      Name FUIName & "." & glyph.ftype(j) As FUIName & "xx." & glyph.ftype(j)
      On Error GoTo DecomposeFilesErr
      For i = 1 To UBound(modsrcid)
        atoOpen FUIName & "xx", modsrcid(i)
        If atoGetDatasetCount() > 0 Then atoInsert FUIName & "." & glyph.ftype(j), chemlist, nameList, degList, secList, branching
        atoClose
      Next
      run(4) = True
    End If
    Case "EPF":
      run(5) = True
    Case "RIF":
      run(6) = True
    Case "HIF":
      run(7) = True
    End Select
' done by the calling batch file because we do not call or launch the process that follows
'    If Dir(FUIName & "." & glyph.ftype(j)) <> "" Then Kill FUIName & "." & glyph.ftype(j)
'    Name FUIName & "xx." & glyph.ftype(j) As FUIName & "xx." & glyph.ftype(j)

dNextFileType:
  
  Next
  Exit Sub

DecomposeFilesErr:
    put_val errfile, "Error in decomposing file for module " & glyph.id & " filetype " & glyph.ftype(j)
    put_line errfile
    errcnt = errcnt + 1
    GoTo dNextFileType
End Sub
    
Sub Main()
Dim i As Long
  getargs
  If argc = 7 Then
    Load.Show
    mode = argv(0)
    branching = CInt(argv(1))
    FUIName = argv(2)
    RunName = argv(3)
    SiteIndex = val(argv(4))
    ModIndex = val(argv(5))
    Model = argv(6)
  ElseIf argc = 6 Then
    Load.Show
    mode = argv(0)
    branching = 0
    FUIName = argv(1)
    RunName = argv(2)
    SiteIndex = val(argv(3))
    ModIndex = val(argv(4))
    Model = argv(5)
  Else
    MsgBox "Not enough arguments passed" & Chr(10) & "Contact PNNL"
    close_csv errfile
    End
  End If
    
  If open_csv(errfile, RunName & ".ERR", F_APPEND) Then
    put_val errfile, "Error report for specification wrapper"
    put_line errfile
  Else
    MsgBox "Unable to create file " & RunName & ".ERR" & Chr(10) & "Check directory permissions"
    End
  End If
  
  loadFUI
  errcnt = 0
  For i = 1 To mCnt
    If Model = glyph(i).id Then
      If mode = "out" Then
        loadDES glyph(i)
        RecomposeFiles glyph(i)
      End If
      If mode = "in" Then
        loadDES glyph(i)
        DecomposeFiles glyph(i)
      End If
      Exit For
    End If
  Next
  
  Unload Load
  close_csv errfile
  If errcnt = 0 Then Kill RunName & ".ERR"
  End
    
End Sub
