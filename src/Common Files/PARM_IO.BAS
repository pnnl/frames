Attribute VB_Name = "PARM_IO"
Option Explicit
Option Compare Text

#If IO = 0 Then
  Public Type parmrec
    pName As String
    idx1 As Long
    idx2 As Long
    idx3 As Long
    idx4 As Long
    idx5 As Long
    idx6 As Long
    ref As Long
    uunit As String
    cunit As String
    pval As String
  End Type
  
  Public Type parmfile
    pos As Long
    file As csv
    find_pattern As String
    pList() As parmrec
    pLookup As Collection
  End Type
#End If

Function FindSection(pfile As parmfile, marker As String) As Long
Dim i As Long
Dim pName As String
Dim numrec As Long
Dim buffer As String
Dim header As Boolean

    pName = ""
    reset_csv pfile.file
    While marker <> pName And Not EOCF(pfile.file)
      pName = get_val(pfile.file)
      numrec = Val(get_val(pfile.file))
      header = (pfile.file.leng = 0 Or pfile.file.getbuff = ",")
      If pName <> marker Then
        If header Then
          For i = 1 To numrec
            Line Input #pfile.file.fnum, buffer
          Next
        End If
        get_line pfile.file
      End If
    Wend
    If (pName = marker) Then
      FindSection = numrec
    Else
      FindSection = 0
    End If
End Function

Function LookupKey(pName As String, idx1 As Long, idx2 As Long, idx3 As Long, idx4 As Long, idx5 As Long, idx6 As Long) As String
  Dim pkey As String
  pkey = LCase(pName)
  pkey = pkey & "." & idx1
  pkey = pkey & "." & idx2
  pkey = pkey & "." & idx3
  pkey = pkey & "." & idx4
  pkey = pkey & "." & idx5
  pkey = pkey & "." & idx6
  LookupKey = pkey
End Function
Function LoadSection(pfile As parmfile, marker As String) As Long
Dim i As Long
Dim rc As Long
Dim recs As Long
Dim msg As String
Dim p As parmrec
Dim pkey As String

     
    recs = FindSection(pfile, marker) ' Version 1.2+
    LoadSection = recs
    Set pfile.pLookup = New Collection
    ReDim pfile.pList(recs) As parmrec
    
    i = 1
    rc = 1
    get_line pfile.file
    While i <= recs And rc = 1 And Not EOCF(pfile.file)
      rc = read_parmrec(pfile, p)
      pkey = LookupKey(p.pName, p.idx1, p.idx2, p.idx3, p.idx4, p.idx5, p.idx6)
      pfile.pLookup.Add i, pkey
      pfile.pList(i) = p
      If rc <> 0 Then i = i + 1
    Wend
    If i < recs Then
      msg = "Actual count (" & i & ") for FUI record block "
      msg = msg & Chr$(10) & " does not agree with marker count (" & recs & ")"
      MsgBox msg
      ReDim Preserve pfile.pList(i)
    End If
End Function

Function ReadStr(pfile As parmfile, rval As String, pName As String, Optional idx1& = 0, Optional idx2& = 0, Optional idx3& = 0, Optional idx4& = 0, Optional idx5& = 0, Optional idx6& = 0) As Boolean
Dim pkey As String
  
  rval = ""
  ReadStr = False
  pkey = LookupKey(pName, idx1, idx2, idx3, idx4, idx5, idx6)
  On Error Resume Next
  rval = pfile.pList(pfile.pLookup(pkey)).pval
  ReadStr = Not Err.Number
  Err.Clear
End Function

Function ReadLng(pfile As parmfile, rval As Long, pName As String, Optional idx1& = 0, Optional idx2& = 0, Optional idx3& = 0, Optional idx4& = 0, Optional idx5& = 0, Optional idx6& = 0) As Boolean
Dim pkey As String

  rval = 0
  ReadLng = False
  pkey = LookupKey(pName, idx1, idx2, idx3, idx4, idx5, idx6)
  On Error Resume Next
  rval = CLng(pfile.pList(pfile.pLookup(pkey)).pval)
  ReadLng = Not Err.Number
  Err.Clear
End Function

Function ReadDbl(pfile As parmfile, rval As Double, pName As String, Optional idx1& = 0, Optional idx2& = 0, Optional idx3& = 0, Optional idx4& = 0, Optional idx5& = 0, Optional idx6& = 0) As Boolean
Dim pkey As String
  
  rval = 0#
  ReadDbl = False
  pkey = LookupKey(pName, idx1, idx2, idx3, idx4, idx5, idx6)
  On Error Resume Next
  rval = CDbl(pfile.pList(pfile.pLookup(pkey)).pval)
  ReadDbl = Not Err.Number
  Err.Clear
End Function

Function ReadTF(pfile As parmfile, rval As Boolean, pName As String, Optional idx1& = 0, Optional idx2& = 0, Optional idx3& = 0, Optional idx4& = 0, Optional idx5& = 0, Optional idx6& = 0) As Boolean
Dim pkey As String
  
  rval = 0#
  ReadTF = False
  pkey = LookupKey(pName, idx1, idx2, idx3, idx4, idx5, idx6)
  On Error Resume Next
  If Val(pfile.pList(pfile.pLookup(pkey)).pval) > 0 Then
   rval = True
  Else
   rval = False
  End If
  ReadTF = Not Err.Number
  Err.Clear
End Function
  
Sub close_parm(pfile As parmfile)
  close_csv pfile.file
  pfile.find_pattern = ""
End Sub

Function find_frst(pfile As parmfile, parm As parmrec, Match As String) As Boolean
' This function reads a parameter file sequentially looking for "pName"
' in the "Name" field of a parameter record.  When a Match is found a 1
' is returned and the values stored in "parm".  If no .prm file a found mesaage is
' displayed and the function returns a 0.
' If the pName is not found the function returns a 0

  Dim temp As parmrec
  On Error GoTo ffopenerror
  pfile.find_pattern = Trim(Match)
  reset_csv pfile.file
  Do
    If read_parmrec(pfile, temp) Then
      If temp.pName = pfile.find_pattern Then
        find_frst = True
        parm = temp
        Exit Function
      End If
      If EOCF(pfile.file) Then Exit Do
    End If
  Loop
  find_frst = False
  Exit Function

ffopenerror:
  MsgBox Error() & " find_frst"
  find_frst = False
  Exit Function
End Function

Function find_next(pfile As parmfile, parm As parmrec) As Boolean
' This function reads a parameter file sequentially looking for the next
' occurrance of "pName" in the "Name" field of a parameter record.  When a Match
' is found a 1' is returned and the values stored in "parm".  If no .prm file a message is
' displayed and the function returns a 0.
' If the pName is not found the function returns a 0
  
  Dim temp As parmrec
  On Error GoTo fnopenerror
  Do
    If read_parmrec(pfile, temp) Then
      If temp.pName = pfile.find_pattern Then
        find_next = True
        parm = temp
        Exit Function
      End If
      If EOCF(pfile.file) Then Exit Do
    End If
  Loop
  find_next = False
  Exit Function

fnopenerror:
  MsgBox Error() & " find_next"
  find_next = False
  Exit Function
End Function

Function open_parm(pfile As parmfile, fName As String, fmode As Long) As Boolean
  open_parm = open_csv(pfile.file, fName, fmode)
  pfile.find_pattern = ""
End Function

Function read_parmrec(pfile As parmfile, temp As parmrec) As Long
  temp.pName = get_val(pfile.file)
  temp.idx1 = Val(get_val(pfile.file))
  temp.idx2 = Val(get_val(pfile.file))
  temp.idx3 = Val(get_val(pfile.file))
  temp.idx4 = Val(get_val(pfile.file))
  temp.idx5 = Val(get_val(pfile.file))
  temp.idx6 = Val(get_val(pfile.file))
  temp.ref = Val(get_val(pfile.file))
  temp.uunit = get_val(pfile.file)
  temp.cunit = get_val(pfile.file)
  temp.pval = get_val(pfile.file)
  pfile.pos = pfile.file.pos
  get_line pfile.file
  read_parmrec = 1
  If temp.pName = "" Then read_parmrec = 0
End Function

Sub set_parm(parm As parmrec, pName As String, i1 As Long, i2 As Long, i3 As Long, i4 As Long, i5 As Long, i6 As Long, ref As Long, uunit As String, cunit As String, pval As String)
  parm.pName = pName
  parm.idx1 = i1
  parm.idx2 = i2
  parm.idx3 = i3
  parm.idx4 = i4
  parm.idx5 = i5
  parm.idx6 = i6
  parm.ref = ref
  parm.uunit = uunit
  parm.cunit = cunit
  parm.pval = pval
End Sub

Sub write_sparam(pfile As parmfile, pName As String, i1&, i2&, i3&, i4&, i5&, i6&, ref&, uunit As String, cunit As String, pval As String)
Dim parm As parmrec
  
  set_parm parm, pName, i1, i2, i3, i4, i5, i6, ref, uunit, cunit, CStr(pval)
  write_sparmrec pfile, parm
End Sub

Sub write_param(pfile As parmfile, pName As String, i1&, i2&, i3&, i4&, i5&, i6&, ref&, uunit As String, cunit As String, pval)
Dim parm As parmrec
  
  set_parm parm, pName, i1, i2, i3, i4, i5, i6, ref, uunit, cunit, CStr(pval)
  write_parmrec pfile, parm
End Sub

Sub write_sparmrec(pfile As parmfile, temp As parmrec)
  put_val pfile.file, temp.pName, ""
  put_val pfile.file, temp.idx1, ""
  put_val pfile.file, temp.idx2, ""
  put_val pfile.file, temp.idx3, ""
  put_val pfile.file, temp.idx4, ""
  put_val pfile.file, temp.idx5, ""
  put_val pfile.file, temp.idx6, ""
  put_val pfile.file, temp.ref, ""
  put_val pfile.file, temp.uunit, ""
  put_val pfile.file, temp.cunit, ""
  put_sval pfile.file, temp.pval, ""
  put_line pfile.file
End Sub

Sub write_parmrec(pfile As parmfile, temp As parmrec)
  put_val pfile.file, temp.pName, ""
  put_val pfile.file, temp.idx1, ""
  put_val pfile.file, temp.idx2, ""
  put_val pfile.file, temp.idx3, ""
  put_val pfile.file, temp.idx4, ""
  put_val pfile.file, temp.idx5, ""
  put_val pfile.file, temp.idx6, ""
  put_val pfile.file, temp.ref, ""
  put_val pfile.file, temp.uunit, ""
  put_val pfile.file, temp.cunit, ""
  put_val pfile.file, temp.pval, ""
  put_line pfile.file
End Sub

Sub Put_Transactions(ByVal marker As String, ByVal inpfile As String, pfileout As parmfile)
Dim rc As Long
Dim pos1 As Long
Dim pos2 As Long
Dim numrec As Long
Dim buffer As String
Dim pfilein As parmfile

On Error GoTo Put_Transactions_error
  
  ' insert new records this marker
  rc = open_parm(pfilein, inpfile, F_READ)
  
  ' put marker
  pos1 = Seek(pfileout.file.fnum)
  put_val pfileout.file, marker, ""
  put_val pfileout.file, Format(numrec, "0000000000"), ""
  put_line pfileout.file
  
  ' put transactions
  numrec = 0
  reset_csv pfilein.file
  buffer = pfilein.file.getbuff & Chr$(13) & Chr$(10)
  
  While Not EOF(pfilein.file.fnum)
    numrec = numrec + 1
    Put pfileout.file.fnum, , buffer
    get_line pfilein.file
    buffer = pfilein.file.getbuff & Chr$(13) & Chr$(10)
  Wend
  
  If pfilein.file.getbuff <> "" Then
    numrec = numrec + 1
    Put pfileout.file.fnum, , buffer
  End If
  close_parm pfilein
  
  pos2 = Seek(pfileout.file.fnum)
  Seek pfileout.file.fnum, pos1
  put_val pfileout.file, marker, ""
  put_val pfileout.file, Format(numrec, "0000000000"), ""
  put_line pfileout.file
  Seek pfileout.file.fnum, pos2
  
  Exit Sub
Put_Transactions_error:
  MsgBox "Put_Transactions " + Err.Description
  Resume Next
End Sub

