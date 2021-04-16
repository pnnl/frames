Attribute VB_Name = "PARM_IO2"
Option Explicit
Option Compare Text

Global readErr As String
Global parmList() As parmrec
Global parmIdx As Long
 
Global Const BLANK = ""

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
      numrec = Val(get_val(pfile.file))
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
  
  ReDim parmList(recs)
  
  i = 1
  rc = 1
  get_line pfile.file
  While i <= recs And rc = 1 And Not EOCF(pfile.file)
    rc = read_parmrec(pfile, parmList(i))
    If rc <> 0 Then i = i + 1
  Wend
  If i < recs Then
    msg = "Actual count (" & i & ") for FUI record block "
    msg = msg & Chr(10) & " does not agree with marker count (" & recs & ")"
    MsgBox msg
    ReDim Preserve parmList(i)
  End If
  readErr = BLANK
  close_parm pfile
  LoadGIDData = i - 1
End Function

Sub Read_Long(pname As String, idx1&, idx2&, idx3&, idx4&, idx5&, idx6&, pval&)
Dim rc As Boolean
Dim value As String

  rc = ReadGidData(pname, idx1, idx2, idx3, idx4, idx5, idx6, value)
  If rc And IsNumeric(value) Then
    pval = CInt(value)
  Else
    pval = 0
    readErr = readErr & "Error reading long -> " & pname & "[" & CStr(idx1) & "," & CStr(idx2) & "," & CStr(idx3) _
                   & "," & CStr(idx4) & "," & CStr(idx5) & "," & CStr(idx6) & "]" & vbCrLf
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
    readErr = readErr & "Error reading single -> " & pname & "[" & CStr(idx1) & "," & CStr(idx2) & "," & CStr(idx3) _
                   & "," & CStr(idx4) & "," & CStr(idx5) & "," & CStr(idx6) & "]" & vbCrLf
  End If
End Sub

Sub Read_String(pname As String, idx1&, idx2&, idx3&, idx4&, idx5&, idx6&, pval As String)
Dim rc As Boolean
Dim value As String

  rc = ReadGidData(pname, idx1, idx2, idx3, idx4, idx5, idx6, value)
  If rc Then
    pval = value
  Else
    pval = BLANK
    readErr = readErr & "Error reading string -> " & pname & "[" & CStr(idx1) & "," & CStr(idx2) & "," & CStr(idx3) _
                   & "," & CStr(idx4) & "," & CStr(idx5) & "," & CStr(idx6) & "]" & vbCrLf
  End If
End Sub

Function ReadGidData(pname As String, idx1&, idx2&, idx3&, idx4&, idx5&, idx6&, pval As String) As Boolean
Dim i As Long
  parmIdx = -1
  ReadGidData = False
  For i = 1 To UBound(parmList)
    If pname = parmList(i).pname Then
      If idx1 = parmList(i).idx1 And idx2 = parmList(i).idx2 And idx3 = parmList(i).idx3 _
        And idx4 = parmList(i).idx4 And idx5 = parmList(i).idx5 And idx6 = parmList(i).idx6 Then
        pval = parmList(i).pval
        parmIdx = i
        ReadGidData = True
        Exit Function
      End If
    End If
  Next
End Function

