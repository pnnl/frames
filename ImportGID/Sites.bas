Attribute VB_Name = "SiteTypes"
Option Explicit
Option Compare Text

Type srcId
  src() As String
End Type

Public parmlist() As parmrec
Global ConList As Collection
 

Function FindGidMarker(pfile As parmfile, fname As String, marker As String) As Long
Dim i As Long
Dim pname As String
Dim numrec As Long
Dim buffer As String
Dim header As Boolean

  If open_parm(pfile, fname, F_READ) Then
    pname = BLANK
    While marker <> pname And Not EOCF(pfile.file)
      pname = get_val(pfile.file)
      numrec = val(get_val(pfile.file))
      header = (pfile.file.leng = 0 Or pfile.file.getbuff = ",")
      If pname <> marker Then
        If header Then
          For i = 1 To numrec
            Line Input #pfile.file.fnum, buffer
          Next
        End If
        get_line pfile.file
      End If
    Wend
  End If
  If (pname = marker) Then FindGidMarker = numrec Else FindGidMarker = 0
End Function

Function LoadGIDData(pfile As parmfile, fname As String, marker As String) As Long
Dim i As Long
Dim rc As Long
Dim recs As Long
Dim msg As String

  recs = FindGidMarker(pfile, fname, marker) ' Version 1.2+
  
  ReDim parmlist(recs)
  
  i = 1
  rc = 1
  get_line pfile.file
  While i <= recs And rc = 1 And Not EOCF(pfile.file)
      rc = read_parmrec(pfile, parmlist(i))
    If rc <> 0 Then i = i + 1
  Wend
  If i < recs Then
    msg = "Actual count (" & i & ") for FUI record block "
    msg = msg & Chr(10) & " does not agree with marker count (" & recs & ")"
    MsgBox msg
    ReDim Preserve parmlist(i)
  End If
  close_parm pfile
End Function

Sub Read_Long(pname As String, idx1&, idx2&, idx3&, idx4&, idx5&, idx6&, pval&)
Dim rc As Boolean
Dim value As String

  rc = ReadGidData(pname, idx1, idx2, idx3, idx4, idx5, idx6, value)
  If rc And IsNumeric(value) Then
    pval = CInt(value)
  Else
    pval = 0
  End If
End Sub

Sub Read_Single(pname As String, idx1&, idx2&, idx3&, idx4&, idx5&, idx6&, pval!)
Dim rc As Boolean
Dim value As String

  rc = ReadGidData(pname, idx1, idx2, idx3, idx4, idx5, idx6, value)
  If rc And IsNumeric(value) Then
    pval = CSng(value)
  Else
    pval = 0
  End If
End Sub

Sub Read_String(pname As String, idx1&, idx2&, idx3&, idx4&, idx5&, idx6&, pval As String)
Dim rc As Boolean
Dim value As String

  rc = ReadGidData(pname, idx1, idx2, idx3, idx4, idx5, idx6, value)
  If rc Then pval = value Else pval = BLANK
End Sub

Function ReadGidData(pname As String, idx1&, idx2&, idx3&, idx4&, idx5&, idx6&, pval As String) As Boolean
Dim i As Long
  
  ReadGidData = False
  For i = 1 To UBound(parmlist)
    If pname = parmlist(i).pname Then
      If idx1 = parmlist(i).idx1 And idx2 = parmlist(i).idx2 And idx3 = parmlist(i).idx3 _
        And idx4 = parmlist(i).idx4 And idx5 = parmlist(i).idx5 And idx6 = parmlist(i).idx6 Then
        pval = parmlist(i).pval
        ReadGidData = True
        Exit Function
      End If
    End If
  Next
End Function

Sub Sites_Read(fname As String)
Dim i As Long, j As Long, k As Long, file As Long, rc As Long
Dim ii As Integer
Dim jj As Integer
Dim mx As Long
Dim temp As parmrec
Dim pfile As parmfile
Dim mname As String, id As String, vers As Single
Dim gc As Long, srcNum As Long, parent As String
Dim cont As String
Dim FuiId As Long
Dim p As ParmCls
Dim ConNum As Long
Dim ConName As String
Dim ConParm As Collection
  
  rc = LoadGIDData(pfile, fname, FUI)
  lastId = 0
  Read_Single "Version", 0, 0, 0, 0, 0, 0, vers
  Read_Long "Sites", 0, 0, 0, 0, 0, 0, NumSites
  For i = 0 To NumSites - 1
    Read_String "SiteName", i + 1, 0, 0, 0, 0, 0, Sites(i).Name
    Sites(i).NumGlyphs = 0
  Next
  
  Set ConList = New Collection
  For i = 1 To NumSites
    Read_String "ConName", i, 1, 0, 0, 0, 0, ConName
    rc = FindGidMarker(pfile, fname, ConName) ' Version 1.2+
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
      ConList.Add ConParm, ConName
    End If
    close_parm pfile
  Next
  
  ' get connectivity
'''  If vers < 1.2 Then
'''    Sites_Read_FUI
'''  Else
    Sites_Read_CSM fname
'''  End If
  
  ReDim parmlist(0)
End Sub

Sub Sites_Read_CSM(fname As String)
  Dim i As Integer, j As Integer, k As Long, rc As Long
  Dim pfile As parmfile, src As Long, srcId As String, Name As String
  Dim Prefix As String, gc As Integer, map() As Integer
  Dim mx As Long
  
  ' 4/01 - Viewers are now be accessed from the module popupmenu so they are
  '        excluded from the glyphs and connections - BLH
  
  rc = LoadGIDData(pfile, fname, CSM)
  Prefix = "Mod"
  For i = 0 To NumSites - 1
    gc = 0
    Read_Long "NumMod", i + 1, 0, 0, 0, 0, 0, Sites(i).NumGlyphs
    ReDim map(Sites(i).NumGlyphs)
    For j = 0 To Sites(i).NumGlyphs - 1
      map(j) = -1
      Read_String Prefix & "Id", i + 1, j + 1, 0, 0, 0, 0, Name
      If "vwr" <> Left(Name, 3) Then
        map(j) = gc
        With Sites(i).Glyphs(gc)
          Read_String Prefix & "Id", i + 1, j + 1, 0, 0, 0, 0, .Name
          Read_String Prefix & "Label", i + 1, j + 1, 0, 0, 0, 0, .Label
          Read_String Prefix & "Model", i + 1, j + 1, 0, 0, 0, 0, .model
          Read_Single Prefix & "LocX", i + 1, j + 1, 0, 0, 0, 0, .x
          Read_Single Prefix & "LocY", i + 1, j + 1, 0, 0, 0, 0, .y
          Read_Single Prefix & "LocZ", i + 1, j + 1, 0, 0, 0, 0, .Z
          Read_Single Prefix & "ScrX", i + 1, j + 1, 0, 0, 0, 0, .sx
          Read_Single Prefix & "ScrY", i + 1, j + 1, 0, 0, 0, 0, .sy
          Read_Long Prefix & "State", i + 1, j + 1, 0, 0, 0, 0, .state
'          .grpIdx = getgrpIdxByPrefix(Left(.Name, 3))
'          .modIdx = getmodIdxByName(.Model)
'          If .modIdx < 0 Then
'            .State = NO_MODULE
'            If .grpIdx < 0 Then
'              Load frmMapGroup
'              frmMapGroup.PrepMapGroup .Name, .Model, .label
'              frmMapGroup.Show vbModal
'              .grpIdx = frmgrpIdx
'              .Name = Group(.grpIdx).Prefix & CStr(Right(.Name, Len(.Name) - 3))
'              .Model = ""
'            End If
'          Else
'            If .grpIdx <> Module(.modIdx).grpIdx Then
'              .grpIdx = Module(.modIdx).grpIdx
'            End If
'          End If
        End With
        gc = gc + 1
      End If
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
  Next i
End Sub

