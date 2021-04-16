Attribute VB_Name = "Con_IO"
Option Explicit
Option Compare Text

Type EstStruct
  ctlIndex As Long
  sprRow As Long
End Type

Type Progeny
  id As String
  name As String
  cltype(5) As Long
End Type

Type ContStruct
  id As String
  name As String
  numprog As Long
  ProgID() As Progeny
  ProgSS As String
  cltype(5) As Long
End Type

Public Const DB_OPEN = 0
Public Const DB_NEW = 1
Public Const DB_SAVE_AS = 2
Public Const DB_OPENXML = 3

Public Const BLANK = ""
Public Const NOT_APP = "n/a"
Public Const CHM_IMP = 0
Public Const RAD_IMP = 1
Public Const RAD_CLS = 0 ' 14
Public Const CTYPE = 0
Public Const ETYPE = 1
Public Const RTYPE = 2
Public Const PTYPE = 3
Public Const KTYPE = 4

Public Const Chemicals = "Chemicals"
Public Const CRITERIA_DEF = "CriteriaDef"
Public Const CRITERIA_DATA = "CriteriaData"

Public Const cas = "cas"
Public Const CASID = "casid"
Public Const FSCASID = "fscasid"
Public Const FSCName = "fscName"
Public Const SSCASID = "sscasid"
Public Const CLCTYPE = "CLCHEM"
Public Const CLKTYPE = "CLKTYPE"
Public Const CLRTYPE = "CLRTYPE"
Public Const CLPTYPE = "CLPTYPE"
Public Const CLETYPE = "CLETYPE"


Public Const DK_FLAT = 0
Public Const DK_CHAIN = 1

Public errflag As Boolean
Public RefName As String        'the Name of the reference file
Public RefParam As String       'the current selected item index
Public RefCasId As String
Public RefUnload As Boolean
Public PropertyCasid As String
Public PropertyParam As String
Public PropertyValue As Variant
Public PropertyRef As Long

Public IN_FRAMES As Boolean
Public EstimateLoaded As Boolean
Public ReferenceLoaded As Boolean
Public PropertyWizLoaded As Boolean

Public mode As Long
Public cdl As CommonDialog

Public ws As Workspace
Public DB As Database

Public coldata() As Recordset
Public tblChems As Recordset
Public tblDecay As Recordset
Public tblParam As Recordset
Public tblRef As Recordset
Public tblRefer As Recordset
Public tblCat As Recordset

Public numcon As Long
Public cont() As ContStruct

Public DBName As String
Public DBNameBackup As String
Public FindString As String

Public hLibModule As Long

Public Declare Function LoadLibrary Lib "kernel32" Alias "LoadLibraryA" (ByVal lpLibFileName$) As Long
Public Declare Function FreeLibrary Lib "kernel32" (ByVal hLibModule&) As Long


Sub SetFindString(find As String)
  FindString = find
End Sub

Function GetFindString() As String
  GetFindString = FindString
End Function

Public Function FindSynonyms(rowid As Long, id As String, name As String) As Boolean
  FindSynonyms = False
  With tblChems
    If rowid > 0 Then
      .index = "RowId"
      .Seek "=", rowid
      FindSynonyms = Not tblChems.NoMatch
      Exit Function
    End If
    If Len(id) > 0 And Len(name) = 0 Then
      .index = "Primary"
      .Seek "=", id
      FindSynonyms = Not tblChems.NoMatch
      Exit Function
    End If
    If Len(id) = 0 And Len(name) > 0 Then
      .index = "Name"
      .Seek "=", name
      FindSynonyms = Not tblChems.NoMatch
      Exit Function
    End If
    If Len(id) > 0 And Len(name) > 0 Then
      .index = "CasidName"
      .Seek "=", id, name
      If tblChems.NoMatch Then
        ' this could occur if Chemicals table has changed since the gid file was generated
        .index = "Primary"
        .Seek "=", id
      End If
      FindSynonyms = Not tblChems.NoMatch
      Exit Function
    End If
  End With
End Function

Sub list_find(lst As Control, btn As Control, txt As Control)
Dim i As Long
Dim found As Long
Dim utext As String

  found = False
  'search list for text
  'if not found disable btn
  If Len(GetFindString()) > 0 Then
    utext = UCase$(GetFindString())
    For i = 0 To lst.ListCount
      If InStr(UCase$(lst.list(i)), utext) Then
        found = True
        Exit For
      End If
    Next
    If found Then
      lst.ListIndex = i
      btn.Enabled = True
      txt.BackColor = lightGreen
    Else
      btn.Enabled = False
      txt.BackColor = lightRed
    End If
  End If
End Sub

Function list_find_again(lst As Control, btn As Control) As Boolean
Dim i As Long
Dim found As Boolean
Dim utext As String

  found = False
  'search list for text from current position
  'if not found disable btn
  utext = UCase$(GetFindString())
  For i = lst.ListIndex + 1 To lst.ListCount
  If InStr(UCase$(lst.list(i)), utext) Then
    found = True
    Exit For
  End If
  Next
  If found Then
    lst.ListIndex = i
  Else
    btn.Enabled = False
    MsgBox "Could not find another " + GetFindString() + " in the list.", 0, "Search"
  End If
  list_find_again = found
End Function

Sub frmCDBEStartup(ModForm As Form, addArg As Long)
  ' addArg is the # of args expected in addition to standard 5
  Set frm = ModForm
  Set cdl = frm.FileDialog
  Set ws = CreateWorkspace("", "admin", "", dbUseJet)
  GetArguments
  If argc = (5 + addArg) Then
    IN_FRAMES = True
    FUIName = argv(argc - 5)         ' argv(1) ' & ".GID"
    RunName = argv(argc - 4)         ' argv(2)
    siteIdx = val(argv(argc - 3))    ' Val(argv(3))
    modIdx = val(argv(argc - 2))     ' Val(argv(4))
    modName = argv(argc - 1)         ' argv(5)
    If Not open_csv(errfile, RunName & ".ERR", 1) Then
      MsgBox "Unable to create file " & RunName & ".ERR" & Chr$(10) & "Check  permissions"
      Unload frm
    End If
  Else
    IN_FRAMES = False
  End If
End Sub

Function fuiOpenGidInput(fle As parmfile) As Boolean
  If open_parm(fle, FUIName & ".GID", 2) Then
    fuiOpenGidInput = True
  Else
    fuiOpenGidInput = False
    PutError "Can't open file " & FUIName
  End If
End Function

Function fuiOpenGidOutput(fle As parmfile) As Boolean
On Error Resume Next
  
  If Len(Dir$(RunName + ".gid")) > 0 Then Kill (RunName + ".gid")
  If open_parm(fle, RunName + ".gid", 1) Then
    fuiOpenGidOutput = True
  Else
    fuiOpenGidOutput = False
    PutError "Can't open file " & RunName + ".gid"
  End If
End Function

Function Read_Contams() As Boolean
Dim n As Long
Dim m As Long
Dim nrec As Long
Dim ncon As Long
Dim name As String
Dim id As String
Dim preGidRef As Boolean
Dim temp As parmrec
Dim fle As parmfile

On Error GoTo Read_Contams_Err
  
  Read_Contams = False
  
  If Not fuiOpenGidInput(fle) Then
    Read_Contams = False
    Exit Function
  End If
  
  ReDim cont(0)
  refBegin
  preGidRef = True
  Do Until EOCF(fle.file)
    If read_parmrec(fle, temp) Then
      Select Case temp.pName
        Case "fui"
          nrec = temp.idx1
          For m = 1 To nrec
            If read_parmrec(fle, temp) Then
              If temp.idx1 = siteIdx Then
                Select Case temp.pName
                  Case "ConDesPath"
                    DesName = temp.pval
                  Case "Version"
                    preGidRef = False
                  Case "numcon"
                    numcon = val(temp.pval)
                    ReDim Preserve cont(numcon)
                  Case CLKTYPE
                    If temp.idx3 = 0 Then
                      cont(temp.idx2).cltype(KTYPE) = temp.pval
                    Else
                      cont(temp.idx2).ProgID(temp.idx3).cltype(KTYPE) = temp.pval
                    End If
                  Case CLETYPE
                    If temp.idx3 = 0 Then
                      cont(temp.idx2).cltype(ETYPE) = temp.pval
                    Else
                      cont(temp.idx2).ProgID(temp.idx3).cltype(ETYPE) = temp.pval
                    End If
                  Case CLRTYPE, "clRAAS"
                    If temp.idx3 = 0 Then
                      cont(temp.idx2).cltype(RTYPE) = temp.pval
                    Else
                      cont(temp.idx2).ProgID(temp.idx3).cltype(RTYPE) = temp.pval
                    End If
                  Case CLPTYPE
                    If temp.idx3 = 0 Then
                      cont(temp.idx2).cltype(PTYPE) = temp.pval
                    Else
                      cont(temp.idx2).ProgID(temp.idx3).cltype(PTYPE) = temp.pval
                    End If
                  Case CLCTYPE
                    If temp.idx3 = 0 Then
                      cont(temp.idx2).cltype(CTYPE) = temp.pval
                    Else
                      cont(temp.idx2).ProgID(temp.idx3).cltype(CTYPE) = temp.pval
                    End If
                  
                  Case FSCASID, FSCName, SSCASID
                    ncon = temp.idx2
                    If ncon > UBound(cont) Then ReDim Preserve cont(ncon)
                    If temp.idx3 > cont(ncon).numprog Then
                      cont(ncon).numprog = temp.idx3
                      ReDim Preserve cont(ncon).ProgID(temp.idx3)
                    End If
                    If temp.idx3 = 0 Then
                      If temp.pName = FSCASID Then
                        cont(ncon).id = temp.pval
                      ElseIf temp.pName = FSCName Then
                        cont(ncon).name = temp.pval
                      End If
                    Else
                      If temp.pName = FSCASID Then
                        cont(ncon).ProgID(temp.idx3).id = temp.pval
                      ElseIf temp.pName = FSCName Then
                        cont(ncon).ProgID(temp.idx3).name = temp.pval
                      ElseIf temp.pName = SSCASID Then
                        cont(temp.idx2).ProgSS = temp.pval
                      End If
                    End If
                End Select
              End If
            End If
          Next
          Exit Do
        Case Else
          nrec = temp.idx1
          For m = 1 To nrec
            get_line fle.file
          Next
      End Select
    End If
  Loop
  close_parm fle
  
  ' update Chemicals table with missing chemicals
  For m = 1 To numcon
    tblChems.Seek "=", cont(m).id
    If tblChems.NoMatch And cont(m).name <> "" Then
      tblChems.AddNew
      tblChems!CASID = cont(m).id
      tblChems!name = cont(m).name
      'backward compatible
      If cont(m).cltype(KTYPE) > 1 Then
        cont(m).cltype(ETYPE) = cont(m).cltype(KTYPE)
        cont(m).cltype(KTYPE) = 0
      End If
      tblChems!CLKTYPE = cont(m).cltype(KTYPE)
      tblChems!CLRTYPE = cont(m).cltype(RTYPE)
      tblChems!CLPTYPE = cont(m).cltype(PTYPE)
      tblChems!CLETYPE = cont(m).cltype(ETYPE)
      tblChems!CLCHEM = cont(m).cltype(CTYPE)
      tblChems.update
    End If
  Next
  For m = 1 To numcon
    For n = 1 To cont(m).numprog
      tblChems.Seek "=", cont(m).ProgID(n).id
      If tblChems.NoMatch And cont(m).ProgID(n).name <> "" Then
        tblChems.AddNew
        tblChems!CASID = cont(m).ProgID(n).id
        tblChems!name = cont(m).ProgID(n).name
        'backward compatible
        If cont(m).ProgID(n).cltype(KTYPE) > 1 Then
          cont(m).ProgID(n).cltype(ETYPE) = cont(m).ProgID(n).cltype(KTYPE)
          cont(m).ProgID(n).cltype(KTYPE) = 0
        End If
        tblChems!CLKTYPE = cont(m).ProgID(n).cltype(KTYPE)
        tblChems!CLRTYPE = cont(m).ProgID(n).cltype(RTYPE)
        tblChems!CLPTYPE = cont(m).ProgID(n).cltype(PTYPE)
        tblChems!CLETYPE = cont(m).ProgID(n).cltype(ETYPE)
        tblChems!CLCHEM = cont(m).ProgID(n).cltype(CTYPE)
        tblChems.update
      End If
    Next
  Next
  
  If Not fuiOpenGidInput(fle) Then
    Read_Contams = False
    Exit Function
  End If
  
  Do Until EOCF(fle.file)
    If read_parmrec(fle, temp) Then
      Select Case temp.pName
        Case modName
          nrec = temp.idx1
          For m = 1 To nrec
            If read_parmrec(fle, temp) Then
              If temp.idx1 = siteIdx And temp.idx2 > 0 Then
                If temp.idx3 = 0 Then
                  id = cont(temp.idx2).id
                Else
                  id = cont(temp.idx2).ProgID(temp.idx3).id
                End If
                
                Select Case temp.pName
                Case FSCASID, FSCName, SSCASID
                  'delete chain info if chain no progeny
                  If cont(temp.idx2).numprog = 0 Then
                    tblDecay.Seek "=", cont(temp.idx2).id
                    If Not tblDecay.NoMatch Then tblDecay.Delete
                  End If
  
                Case "FSFRACTION", "SSFRACTION"
                  tblDecay.Seek "=", cont(temp.idx2).id
                  If tblDecay.NoMatch Then
                    tblDecay.AddNew
                    tblDecay!CASID = cont(temp.idx2).id
                  Else
                    tblDecay.Edit
                  End If
                  If temp.pName = "FSFRACTION" Then
                    tblDecay!branch1 = cont(temp.idx2).ProgID(temp.idx3).id
                    tblDecay!fraction1 = val(temp.pval)
                  Else
                    tblDecay!branch2 = cont(temp.idx2).ProgSS
                    tblDecay!fraction2 = val(temp.pval)
                  End If
                  tblDecay.update
                  
                Case Else
                  ' get parameter id from parameter Name
                  tblParam.Seek "=", temp.pName
                  If Not tblParam.NoMatch Then
                    Select Case temp.pName
                    Case CLKTYPE, CLETYPE, CLCTYPE, CLRTYPE, CLPTYPE, "clRAAS"
                      ' do nothing
                    Case Else
                      If preGidRef Then
                        frmCDBE.SetParamValue temp.pName, id, temp.pval, temp.ref
                      Else
                        frmCDBE.SetParamValue temp.pName, id, temp.pval, GidToRef(temp.ref)
                      End If
                    End Select
                  End If
                End Select
              End If
            End If
          Next
          Exit Do
        Case Else
          nrec = temp.idx1
          For m = 1 To nrec
            get_line fle.file
          Next
      End Select
    End If
  Loop
  close_parm fle
  numcon = UBound(cont)
  Read_Contams = True

Read_Contams_Err:
  If Err.Number <> 0 Then
    MsgBox Error, vbCritical, "Read_Contams: " & id
  End If
  refEnd
End Function

Sub refEnd()
  Set tblRefer = Nothing
  Unload Reference
End Sub

Sub refBegin()
  Set tblRefer = DB.OpenRecordset("Ref", dbOpenTable)
  RefMode = 1
  Reference.Hide
End Sub

Function GidToRef(rIdx As Long) As Long
Dim abbr As String
Dim ref As String

On Error GoTo FG

  Reference.get_ref Reference.get_recnum(rIdx), abbr, ref
  tblRefer.index = "SNAME"
  tblRefer.Seek "=", abbr
  If tblRefer.NoMatch Then
    GidToRef = 0
  Else
    GidToRef = tblRefer!refnum
  End If

FG:
  If Err.Number <> 0 Then
    MsgBox Error, vbCritical, "GidToRef: " & rIdx
  End If
End Function

Function RefToGid(rIdx As Long) As Long
  
On Error GoTo FG
  
  tblRefer.index = "RefNum"
  tblRefer.Seek "=", rIdx
  If tblRefer.NoMatch Then
    RefToGid = Reference.add_ref("Not Valid", "No Valid Reference")
  Else
    If rIdx <> 0 Then RefToGid = Reference.add_ref(tblRefer!sName, tblRefer!bName)
  End If

FG:
  If Err.Number <> 0 Then
    MsgBox Error, vbCritical, "RefToGid: " & rIdx
  End If
End Function

Sub Write_Contams_Properties(pfile As parmfile, j As Long, k As Long, tag As String)
Dim tref As Long
Dim rowid As Long
Dim idKTYPE As Long
Dim tval As String
Dim tunit As String
Dim id As String
Dim cName As String

On Error GoTo ErrorHandler

  frmCDBE.SplitNodeTag tag, rowid, id, cName
  FindSynonyms rowid, "", ""
  idKTYPE = tblChems!CLKTYPE

  tblParam.MoveFirst
  Do While Not tblParam.EOF
    If ("Name" <> tblParam!sName) Then
      tval = frmCDBE.GetParamValue(Trim$(tblParam!sName), id, tref)
      If Len(tval) > 0 Then
        tunit = ""
        If idKTYPE = 0 Then
          If Not IsNull(tblParam!ChemUnits) Then tunit = tblParam!ChemUnits
        Else
          If Not IsNull(tblParam!RadUnits) Then tunit = tblParam!RadUnits
        End If
        If Len(tunit) = 0 Then tunit = "n/a"
        If tunit <> "Not Defined" Then
         write_param pfile, Trim$(tblParam!sName), siteIdx, j, k, 0, 0, 0, CStr(RefToGid(tref)), tunit, tunit, Trim$(tval)
        End If
      End If
    End If
    tblParam.MoveNext
  Loop
  
ErrorHandler:
  If Err.Number <> 0 Then
    MsgBox Error, vbCritical, "Write_Contams_Properties " & tag
  End If
End Sub

Function Write_Contams_16(tvwSel As TreeView) As Boolean
Dim j As Long
Dim k As Long
Dim id As String
Dim msg As String
Dim rsp As Recordset
Dim rsd As Recordset
Dim pfile As parmfile
Dim nodx As Node
Dim root As Node
Dim numcon As Long
Dim rowid As Long
Dim cName As String
Dim tvw As TreeView
Dim casid1 As String
Dim casid2 As String
Dim snode As Node
Dim cnode As Node
Dim NDS As Long

On Error GoTo Write_Contams_Err

  If tvwSel.Nodes.Count = 0 Then
    PutError "No constituents selected for simulation!"
    GoTo Write_Contams_Exit
  End If

  refBegin
  Set root = tvwSel.Nodes(1).root

  If fuiOpenGidOutput(pfile) Then

    Set nodx = root
    Do While Not nodx Is Nothing
      numcon = numcon + 1
      frmCDBE.SplitNodeTag nodx.tag, rowid, id, cName
      nodx.text = LCase(cName)
      Set nodx = nodx.next
    Loop
    tvwSel.Sorted = True
    write_param pfile, "Version", siteIdx, 0, 0, 0, 0, 0, 0, NOT_APP, NOT_APP, App.Major & "." & App.Minor
    write_param pfile, "NUMCON", siteIdx, 0, 0, 0, 0, 0, 0, NOT_APP, NOT_APP, numcon
    
    j = 0
    k = 0
    Set nodx = root
    Do While Not nodx Is Nothing
      j = j + 1
      'Write_Contams_Recurse pfile, nodx.tag, j, k
      frmCDBE.SplitNodeTag nodx.tag, rowid, id, cName
      write_sparam pfile, FSCASID, siteIdx, j, 0, 0, 0, 0, 0, NOT_APP, NOT_APP, id
      write_sparam pfile, FSCName, siteIdx, j, 0, 0, 0, 0, 0, NOT_APP, NOT_APP, cName

      Set tvw = frmCDBE.tvwDKview(DK_FLAT)
      frmCDBE.GetDecayStraight id, True
      NDS = IIf(tvw.Nodes.Count > 0, tvw.Nodes.Count - 1, 0)
      write_param pfile, "NDS", siteIdx, j, 0, 0, 0, 0, 0, NOT_APP, NOT_APP, NDS
      If NDS > 0 Then
        ' the first node is the parent
        Set snode = tvw.Nodes(2)
        k = 0
        Do While Not snode Is Nothing
          k = k + 1
          frmCDBE.SplitNodeTag snode.tag, rowid, casid1, cName
          write_sparam pfile, FSCASID, siteIdx, j, k, 0, 0, 0, 0, NOT_APP, NOT_APP, casid1
          write_sparam pfile, FSCName, siteIdx, j, k, 0, 0, 0, 0, NOT_APP, NOT_APP, cName
          
          If k = 1 Then
            tblDecay.Seek "=", id
            If Not tblDecay.NoMatch Then
              If Not IsNull(tblDecay!branch1) Then
                If Len(Trim$(tblDecay!branch1)) > 0 Then
                  write_param pfile, "FSFRACTION", siteIdx, j, k, 0, 0, 0, 0, "fraction", "fraction", val(tblDecay!fraction1)
                End If
              End If
              If Not IsNull(tblDecay!branch2) Then
                If Len(Trim$(tblDecay!branch2)) > 0 Then
                  Set cnode = tvwSel.Nodes(cas & Trim$(tblDecay!branch2))
                  
                  frmCDBE.SplitNodeTag cnode.tag, rowid, casid2, cName
                  write_sparam pfile, "SSCASID", siteIdx, j, k, 0, 0, 0, 0, NOT_APP, NOT_APP, casid2 ' Trim$(tblDecay!branch2)
                  write_sparam pfile, "SSCName", siteIdx, j, k, 0, 0, 0, 0, NOT_APP, NOT_APP, cName ' Trim$(tblDecay!branch2)
                  write_param pfile, "SSFRACTION", siteIdx, j, k, 0, 0, 0, 0, "fraction", "fraction", val(tblDecay!fraction2)
                End If
              End If
            End If
          End If
          Set snode = snode.next
        Loop
      End If
      Set nodx = nodx.next
    Loop

    j = 0
    k = 0
    Set nodx = root
    Do While Not nodx Is Nothing
      j = j + 1
      frmCDBE.SplitNodeTag nodx.tag, rowid, id, cName
      Write_Contams_Properties pfile, j, 0, nodx.tag

      Set tvw = frmCDBE.tvwDKview(DK_FLAT)
      frmCDBE.GetDecayStraight id, True
      NDS = IIf(tvw.Nodes.Count > 0, tvw.Nodes.Count - 1, 0)
      If NDS > 0 Then
        Set snode = tvw.Nodes(2)
        k = 0
        Do While Not snode Is Nothing
          k = k + 1
          Write_Contams_Properties pfile, j, k, snode.tag
          Set snode = snode.next
        Loop
      End If
      Set nodx = nodx.next
    Loop
    
    Set nodx = root
    Do While Not nodx Is Nothing
      frmCDBE.SplitNodeTag nodx.tag, rowid, id, cName
      nodx.text = IIf(tvwSel.tag = CASID, id, cName)
      Set nodx = nodx.next
    Loop

    close_parm pfile
  End If
  Write_Contams_16 = True

Write_Contams_Exit:
On Error Resume Next
  rsd.Close
  rsp.Close
  refEnd
  Exit Function

Write_Contams_Err:
  Select Case Err
    Case 3021
      msg = "Constituent " & cont(j).name & " does not exist in the database.  "
      msg = msg & "Remove from constituent list before continuing."
      MsgBox msg
    Case Else
      MsgBox Error
  End Select
  Write_Contams_16 = False
  Resume Write_Contams_Exit
End Function

Function GetListIndex(list As Control, text As String) As Long
Dim i As Long
  
  GetListIndex = -1
  For i = 0 To list.ListCount - 1
    If list.list(i) = text Then
      GetListIndex = i
      Exit Function
    End If
  Next
End Function

Public Function NotEmpty(value) As Boolean
  NotEmpty = False
  If Not IsNull(value) Then
    If Len(value) > 0 Then
      NotEmpty = True
    End If
  End If
End Function

Function ValidInput(pName As Variant, txtval As Variant) As Boolean
Dim minOk As Boolean
Dim maxOk As Boolean
Dim hasMin As Boolean
Dim hasMax As Boolean

  ValidInput = True ' default
  
  tblParam.Seek "=", pName
  If Not tblParam.NoMatch Then
    hasMin = NotEmpty(tblParam!Min)
    hasMax = NotEmpty(tblParam!Max)
    Select Case tblParam!Type
      Case "char":
        If hasMin Then
          If Len(txtval) > tblParam!Min Then
            ValidInput = False
          End If
        End If
      Case "int", "float":
        If IsNumeric(txtval) Then
          If hasMin Then
            minOk = val(txtval) >= val(tblParam!Min)
          Else
            minOk = True
          End If
          If hasMax Then
            maxOk = val(txtval) <= val(tblParam!Max)
          Else
            maxOk = True
          End If
          ValidInput = minOk And maxOk
        Else
          ValidInput = False
        End If
      Case Else:
    End Select
  End If
End Function

Public Sub SetRangeDisplay(stb As StatusBar, KTYPE As Long)
Dim range As String
Dim hasMin As Boolean
Dim hasMax As Boolean

  If Len(HelpAnchor) > 0 Then stb.Panels(2) = "(" & HelpAnchor & ")"
  range = ""
  tblParam.Seek "=", HelpAnchor
  If Not tblParam.NoMatch Then
    hasMin = NotEmpty(tblParam!Min)
    hasMax = NotEmpty(tblParam!Max)
    Select Case tblParam!Type
      Case "list":
        range = "Select one from: " & tblParam!Min
      Case "char":
        If Not IsNull(tblParam!Min) Then
          range = " 1 to " & tblParam!Min & " characters"
        End If
      Case "int", "float":
          If hasMin Then
            If hasMax Then
              range = tblParam!Min & " to " & tblParam!Max
            Else
              range = "Minimum " & tblParam!Min
            End If
          Else
            If hasMax Then range = "Maximum " & tblParam!Max
          End If
      Case Else:
    End Select
    range = range + " "
    If Len(range) > 0 Then
      If KTYPE = 0 Then
        If tblParam!ChemUnits = "Not Defined" Then
          stb.Panels(2).text = "(" & HelpAnchor & ") " & tblParam!ChemUnits
        Else
          stb.Panels(2).text = "(" & HelpAnchor & ") Range: " & range & tblParam!ChemUnits
        End If
      Else
        If tblParam!RadUnits = "Not Defined" Then
          stb.Panels(2).text = "(" & HelpAnchor & ") " & tblParam!RadUnits
        Else
          stb.Panels(2).text = "(" & HelpAnchor & ") Range: " & range & tblParam!RadUnits
        End If
      End If
    End If
  End If
End Sub

Public Function CheckDatabase(tdb As Database) As Boolean
Dim ttable As TableDef

On Error GoTo FG
  Set ttable = tdb.TableDefs("Category")
  Set ttable = tdb.TableDefs("Param")
  Set ttable = tdb.TableDefs("Classification")
  Set ttable = tdb.TableDefs("Chemicals")
  Set ttable = tdb.TableDefs("Ref")
  
FG:
  If Err.Number = 0 Then
    CheckDatabase = True
  Else
    CheckDatabase = False
  End If
End Function

Function GoodFileName(fileName As String) As Boolean

On Error Resume Next
  
  GoodFileName = False
  If Len(fileName) = 0 Then Exit Function
  If ".mdb" <> Right$(fileName, 4) Then Exit Function
  GoodFileName = True
End Function

Function GoodFileXML(fileName As String) As Boolean

On Error Resume Next
  
  GoodFileXML = False
  If Len(fileName) = 0 Then Exit Function
  If ".xml" <> Right$(fileName, 4) Then Exit Function
  GoodFileXML = True
End Function

Function GetFileName(Method As Long) As String

On Error Resume Next
  GetFileName = BLANK
  If cdl.InitDir = BLANK Then cdl.InitDir = App.Path
  cdl.CancelError = True
  cdl.Filter = "Constituent Database(*.mdb)|*.mdb"
  cdl.FilterIndex = 1
  cdl.DefaultExt = "mdb"
  cdl.Flags = cdlOFNHideReadOnly Or cdlOFNNoChangeDir Or cdlOFNExtensionDifferent Or cdlOFNNoLongNames Or cdlOFNPathMustExist Or cdlOFNNoReadOnlyReturn
  
  Select Case Method
  Case DB_OPENXML
    cdl.DialogTitle = "Open EFS XML Constituent Database"
    cdl.fileName = "*.xml"
    cdl.Flags = cdl.Flags Or cdlOFNFileMustExist
    cdl.ShowOpen
  Case DB_OPEN
    cdl.DialogTitle = "Open Existing Constituent Database"
    cdl.fileName = "*.mdb"
    cdl.Flags = cdl.Flags Or cdlOFNFileMustExist
    cdl.ShowOpen
  Case DB_NEW  'create new des and new dbase file
    cdl.DialogTitle = "Create New Constituent Database"
    cdl.fileName = BLANK
    cdl.Flags = cdl.Flags Or cdlOFNOverwritePrompt
    cdl.ShowOpen
    If Err.Number = 0 Then
      If Len(Dir$(cdl.fileName)) > 0 Then
        If vbNo = MsgBox("The file (" & cdl.fileName & ") already exists!" & Chr$(10) & "Do want to replace the existing file?", vbExclamation Or vbYesNo, "File Path\Name Warning") Then
          cdl.fileName = BLANK
        Else
          Kill cdl.fileName
        End If
      End If
    End If
  Case DB_SAVE_AS  'create new des file
    cdl.DialogTitle = "Save As Consitutent Database"
    cdl.fileName = "*.mdb"
    cdl.Flags = cdl.Flags Or cdlOFNOverwritePrompt
    cdl.ShowSave
  End Select
  
  If Err.Number <> 0 Then Exit Function
  GetFileName = cdl.fileName
End Function

Public Function CopyDatabase(fileName As String, NewFileName As String) As Boolean
Dim msg As String
  
On Error Resume Next
  CopyDatabase = False
  If Len(Dir$(NewFileName)) > 0 Then Kill NewFileName
  Err.Clear
  DBEngine.CompactDatabase fileName, NewFileName
  If Err.Number <> 0 Then
    ' When you attempt to open or compact a corrupted database, a run-time error usually occurs.
    ' In some situations, however, a corrupted database may not be detected, and no error occurs.
    ' It's a good idea to provide your users with a way to use the RepairDatabase method in your
    ' application if their database behaves unpredictably.
    msg = Error & vbCrLf
    msg = msg & "This error occurred while compacting/copying database " & fileName & "."
    msg = msg & vbCrLf & "The database may be corrupted or in use."
    msg = msg & vbCrLf & "An attempt will be made to repair the database."
    MsgBox msg
    Err.Clear
    DBEngine.RepairDatabase fileName
    ' try again
    If Err.Number <> 0 Then
      msg = Error & vbCrLf
      msg = msg & "This error occurred while attempting to repair database " & fileName & "."
      MsgBox msg
      Err.Clear
      Exit Function
    Else
      If Len(Dir$(NewFileName)) > 0 Then Kill NewFileName
      Err.Clear
      DBEngine.CompactDatabase fileName, NewFileName
      If Err.Number <> 0 Then
        msg = Error & vbCrLf
        msg = "Still unable to compact/copy the database."
        MsgBox msg
        Err.Clear
        Exit Function
      End If
    End If
  End If
  CopyDatabase = True
End Function

Public Function NewDatabase(fileName As String) As Boolean
Dim i As Long
Dim dbpath As String
Dim msgstring As String
Dim dbI As Database
Dim dbO As Database
Dim tdfO As TableDef
 
On Error GoTo newdb_error
  
  NewDatabase = False
  
  'delete the file if one exisits
  If Len(Dir$(fileName)) > 0 Then Kill fileName
  frm.StatusBar1.Panels(1) = "Creating " & fileName
  
  'check to see that FUI.mdb exists and has the right table definitions
  dbpath = Space$(256)
  GetPrivateProfileString "App Path", "FUI", App.Path, dbpath, 256, FRAMES_INI
  dbpath = StripTerminator(dbpath)
  dbpath = dbpath & "\fui.mdb"
  Set dbI = ws.OpenDatabase(dbpath, False, False)
  If CheckDatabase(dbI) = False Then
    msgstring = "Invalid FUI.MDB file!" + vbCrLf + "Please contact PNNL"
    MsgBox msgstring, vbExclamation + vbOKOnly, "Database Template Error"
    frm.StatusBar1.Panels(1) = ""
    Exit Function
  End If
  dbI.Close
  Set dbI = Nothing
  
  ' copy fui.mdb to new database
  FileCopy dbpath, fileName
  Set dbO = ws.OpenDatabase(fileName, False, False)
  ws.BeginTrans
  For i = dbO.TableDefs.Count - 1 To 0 Step -1
    Set tdfO = dbO.TableDefs(i)
    If 0 = (tdfO.Attributes And dbSystemObject) Then
      Select Case tdfO.name
        Case "Category", "Param", "Classification":
        Case Else
          dbO.Execute "DELETE * FROM [" & tdfO.name & "]"
      End Select
    End If
  Next
  ws.CommitTrans
  dbO.Close
  Set dbO = Nothing
  
  frm.StatusBar1.Panels(1) = "Successfully created " & fileName
  NewDatabase = True
  
GoTo newdb_exit
  
newdb_error:
 MsgBox Error
 Resume Next
 
newdb_exit:
  Beep
End Function

Sub CreateDes(fileName As String)
Dim fin As csv
Dim fout As csv
Dim temp As String
  
  If MsgBox("Would you like to create a corresponding FRAMES description file", vbYesNo) = vbNo Then Exit Sub
  
  temp = SplitPath(fileName, SP_DIR) & SplitPath(fileName, SP_TITLE) & ".des"
  If open_csv(fin, App.Path & "\CDBEDes.template", F_READ) Then
     If open_csv(fout, temp, F_WRITE) Then
       fout.putbuff = fin.getbuff
       put_line fout
       get_line fin
       fin.getbuff = Replace(fin.getbuff, "[a]", SplitPath(fileName, SP_TITLE) & " FRAMES Constituent database")
       fin.getbuff = Replace(fin.getbuff, "[b]", "contsel.exe " & fileName)
       fout.putbuff = fin.getbuff
       put_line fout
       Do While Not EOCF(fin)
         get_line fin
         fout.putbuff = fin.getbuff
         put_line fout
       Loop
       close_csv fout
     End If
     close_csv fin
  End If
End Sub

Public Function CreateAttribute(attrid As String) As Boolean
Dim tdf As TableDef
  
  ' add fields for criteria values to CriteriaData table
  '   the unexplained error response to adding fields to the table
  '   was handled by interacting with the table in the LoadData
  '   function which adds a temporary field.  The temporary field
  '   is then deleted on OK from frmPropWiz
  
On Error Resume Next
  DB.TableDefs.Refresh
  Set tdf = DB.TableDefs("CriteriaData")
  If Err.Number <> 0 Then
    Exit Function
  End If
  
On Error GoTo ErrorHandler
  
  If Len(attrid) > 0 Then
    tdf.Fields.Append tdf.CreateField(attrid, dbText)
    tdf.Fields.Append tdf.CreateField("Ref" & attrid, dbInteger)
    DB.TableDefs.Refresh
  Else
On Error Resume Next
    tdf.Fields.Delete "Temp"
    Err.Clear
    tdf.Fields.Append tdf.CreateField("Temp", dbText)
  End If
  DB.TableDefs.Refresh
  CreateAttribute = True
  
ErrorHandler:
  If Err.Number <> 0 Then
    CreateAttribute = False
  End If
End Function
