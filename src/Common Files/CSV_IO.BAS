Attribute VB_Name = "CSV_IO"
Option Explicit
Option Compare Text
'Comma separated value file handler
'csv file handle data type definition

Public Const SP_DRIVE = 0
Public Const SP_DIR = 1
Public Const SP_DIRR = 4
Public Const SP_TITLE = 2
Public Const SP_EXT = 3

Public Const F_WRITE = 1
Public Const F_READ = 2
Public Const F_APPEND = 3

#If IO = 0 Then
  Public Type csv
    pos As Long
    fnum As Long
    fmode As Long
    fName As String
    leng As Long
    getbuff As String
    putbuff As String
    separator As String
  End Type
#End If

Function ReplaceExt(Name As String, ext As String) As String
  ReplaceExt = SplitPath(Name, SP_DIR) & SplitPath(Name, SP_TITLE) & "." & ext
End Function

Function SplitPath(Path As String, item As Integer) As String
Dim i As Integer
Dim dot As Integer
Dim slash As Integer
Dim colon As Integer
Dim colonnotfound As Boolean
Dim slashnotfound As Boolean
Dim dotnotfound As Boolean

  SplitPath = ""
  If Path = "" Then Exit Function
  dotnotfound = True
  slashnotfound = True
  colonnotfound = True
  i = Len(Path) + 1
  dot = i
  slash = 0
  colon = 0
  'start form the end and find first dot
  While i > 0
    If Mid$(Path, i, 1) = "." And dotnotfound And slashnotfound Then
      dot = i
      dotnotfound = False
    ElseIf Mid$(Path, i, 1) = "\" And slashnotfound Then
      slash = i
      slashnotfound = False
    ElseIf Mid$(Path, i, 1) = ":" And colonnotfound Then
      colon = i
      colonnotfound = False
    End If
    i = i - 1
  Wend
  
  Select Case item
    Case SP_DRIVE:  SplitPath = Left$(Path, colon)                             'drive up to slash (c: | "")
    Case SP_DIR:    SplitPath = Left$(Path, slash)                             'drive to slash inclusive (c:\dir1\ | \dir1\dir2\ | dir\ | "")
    Case SP_DIRR:   SplitPath = Left$(Path, slash - 1)                         'drive to slash exclusive (c:\dir1 | \dir1\dir2 | dir | "")
    Case SP_TITLE:  SplitPath = Mid$(Path, slash + 1, dot - slash - 1)         'slash to dot exclusive (.my.firstfile | myfirstfile | "")
    Case SP_EXT:    SplitPath = Right$(Path, Len(Path) - dot + 1)              'dot to end inclusive (.txt | .myextension | "")
  End Select
End Function

Function EOCF(fle As csv) As Boolean
  EOCF = True
  If fle.fnum = 0 Then Exit Function
  If LOF(fle.fnum) <= Seek(fle.fnum) And EOL(fle) Then
    EOCF = True
  Else
    EOCF = False
  End If
End Function

Function EOL(fle As csv) As Boolean
  EOL = (fle.leng = 0 Or fle.fnum = 0)
End Function

Sub close_csv(fle As csv)
  If fle.fnum = 0 Then Exit Sub
  Close fle.fnum
  fle.fnum = 0
End Sub

Sub get_line(fle As csv)
  fle.leng = 0
  fle.getbuff = ""
  If fle.fnum = 0 Then Exit Sub
  If LOF(fle.fnum) > Seek(fle.fnum) Then
    Line Input #fle.fnum, fle.getbuff
    fle.getbuff = Trim$(fle.getbuff)
    fle.leng = Len(fle.getbuff)
  End If
End Sub

Function get_val_multiline(fle As csv) As String
Dim pos As Long
Dim pos1 As Long
  
  If fle.fnum > 0 And fle.leng > 0 Then
    pos = InStr(fle.getbuff, fle.separator)
    pos1 = InStr(fle.getbuff, """")
    If pos = 0 And pos1 = 0 Then
      get_val_multiline = fle.getbuff
      fle.getbuff = ""
    Else
      If pos > 0 And pos1 = 0 Then
        get_val_multiline = Trim$(Left$(fle.getbuff, pos - 1))
        fle.getbuff = Trim$(Right$(fle.getbuff, Len(fle.getbuff) - pos))
      Else
        If pos = 0 And pos1 > 0 Then
          fle.getbuff = Trim$(Right$(fle.getbuff, Len(fle.getbuff) - pos1))
          pos = InStr(fle.getbuff, """")
          If pos > 0 Then
            get_val_multiline = Trim$(Left$(fle.getbuff, pos - 1))
            fle.getbuff = ""
          Else
            get_val_multiline = fle.getbuff
            Do
              get_line fle
              pos1 = InStr(fle.getbuff, """")
              If pos1 > 0 Then
                get_val_multiline = get_val_multiline + Left$(fle.getbuff, pos1 - 1)
                fle.getbuff = Right$(fle.getbuff, Len(fle.getbuff) - pos1)
                Exit Do
              Else
    ' THIS IS THE SOLE DIFFERENCE FROM get_val - required for
    ' reading the .DES multi-line description
                get_val_multiline = get_val_multiline + vbCrLf + fle.getbuff
              End If
              If LOF(fle.fnum) <= Seek(fle.fnum) Then Exit Do
            Loop
          End If
        Else
          If pos < pos1 Then
            get_val_multiline = Trim$(Left$(fle.getbuff, pos - 1))
            fle.getbuff = Trim$(Right$(fle.getbuff, Len(fle.getbuff) - pos))
          Else
            fle.getbuff = Trim$(Right$(fle.getbuff, Len(fle.getbuff) - pos1))
            pos = InStr(fle.getbuff, """")
            If pos > 0 Then
              get_val_multiline = Trim$(Left$(fle.getbuff, pos - 1))
              pos1 = InStr(pos, fle.getbuff, fle.separator)
              fle.getbuff = Trim$(Right$(fle.getbuff, Len(fle.getbuff) - pos1))
            Else
              get_val_multiline = fle.getbuff
              Do
                get_line fle
                pos1 = InStr(fle.getbuff, """")
                If pos1 > 0 Then
                  get_val_multiline = get_val_multiline + Left$(fle.getbuff, pos1 - 1)
                  fle.getbuff = Right$(fle.getbuff, Len(fle.getbuff) - pos1)
                  Exit Do
                Else
                  get_val_multiline = get_val_multiline + fle.getbuff
                End If
                If LOF(fle.fnum) <= Seek(fle.fnum) Then Exit Do
              Loop
            End If
          End If
        End If
      End If
    End If
    fle.leng = Len(fle.getbuff)
  End If
End Function
Function get_val(fle As csv) As String
Dim pos As Long
Dim pos1 As Long
  
  If fle.fnum > 0 And fle.leng > 0 Then
    pos = InStr(fle.getbuff, fle.separator)
    pos1 = InStr(fle.getbuff, """")
    If pos = 0 And pos1 = 0 Then
      get_val = fle.getbuff
      fle.getbuff = ""
    Else
      If pos > 0 And pos1 = 0 Then
        get_val = Trim$(Left$(fle.getbuff, pos - 1))
        fle.getbuff = Trim$(Right$(fle.getbuff, Len(fle.getbuff) - pos))
      Else
        If pos = 0 And pos1 > 0 Then
          fle.getbuff = Trim$(Right$(fle.getbuff, Len(fle.getbuff) - pos1))
          pos = InStr(fle.getbuff, """")
          If pos > 0 Then
            get_val = Trim$(Left$(fle.getbuff, pos - 1))
            fle.getbuff = ""
          Else
            get_val = fle.getbuff
            Do
              get_line fle
              pos1 = InStr(fle.getbuff, """")
              If pos1 > 0 Then
                get_val = get_val + Left$(fle.getbuff, pos1 - 1)
                fle.getbuff = Right$(fle.getbuff, Len(fle.getbuff) - pos1)
                Exit Do
              Else
                get_val = get_val + fle.getbuff
              End If
              If LOF(fle.fnum) <= Seek(fle.fnum) Then Exit Do
            Loop
          End If
        Else
          If pos < pos1 Then
            get_val = Trim$(Left$(fle.getbuff, pos - 1))
            fle.getbuff = Trim$(Right$(fle.getbuff, Len(fle.getbuff) - pos))
          Else
            fle.getbuff = Trim$(Right$(fle.getbuff, Len(fle.getbuff) - pos1))
            pos = InStr(fle.getbuff, """")
            If pos > 0 Then
              get_val = Trim$(Left$(fle.getbuff, pos - 1))
              pos1 = InStr(pos, fle.getbuff, fle.separator)
              If pos1 = 0 Then pos1 = pos
              fle.getbuff = Trim$(Right$(fle.getbuff, Len(fle.getbuff) - pos1))
            Else
              get_val = fle.getbuff
              Do
                get_line fle
                pos1 = InStr(fle.getbuff, """")
                If pos1 > 0 Then
                  get_val = get_val + Left$(fle.getbuff, pos1 - 1)
                  fle.getbuff = Right$(fle.getbuff, Len(fle.getbuff) - pos1)
                  Exit Do
                Else
                  get_val = get_val + fle.getbuff
                End If
                If LOF(fle.fnum) <= Seek(fle.fnum) Then Exit Do
              Loop
            End If
          End If
        End If
      End If
    End If
    fle.leng = Len(fle.getbuff)
  End If
End Function

Function open_csv(fle As csv, fName As String, fmode As Long, Optional showerror As Boolean = True) As Boolean
  If fName = "" Then Exit Function
  open_csv = True
  fle.fName = fName
  fle.fnum = FreeFile
  fle.separator = ","
  On Error Resume Next
  If fmode = F_WRITE Or fmode = F_APPEND Then
    fle.fmode = F_WRITE
    If fmode = F_WRITE Then If Dir$(fle.fName) <> "" Then Kill fle.fName
    Open fle.fName For Binary Access Read Write As fle.fnum
    If (Err.Number = 0) Then get_line fle
  Else
    fle.fmode = F_READ
    Open fle.fName For Input Access Read As fle.fnum
    If (Err.Number = 0) Then get_line fle
  End If
  If (Err.Number <> 0) Then
    If showerror Then MsgBox Error() & " open_csv " & fle.fName
    open_csv = False
    fle.fmode = 0
    fle.fnum = 0
    fle.leng = 0
    fle.fName = ""
  End If
End Function

Sub put_line(fle As csv)
  If fle.fnum > 0 Then
    fle.putbuff = fle.putbuff & Chr$(13) & Chr$(10)
    Put fle.fnum, , fle.putbuff
    fle.putbuff = ""
  End If
End Sub

Sub put_sval(fle As csv, var As Variant, Optional formt As Variant)
Dim delimeter As String
  
  If fle.fnum <> 0 And fle.fmode = F_WRITE Then
    If Len(fle.putbuff) = 0 Then
      delimeter = ""
    Else
      delimeter = fle.separator
    End If
  End If
  fle.putbuff = fle.putbuff & delimeter & """" & var & """"
End Sub

Sub put_val(fle As csv, var As Variant, Optional formt As Variant)
Dim delimeter As String
  
  If fle.fnum <> 0 And fle.fmode = F_WRITE Then
    If Len(fle.putbuff) = 0 Then
      delimeter = ""
    Else
      delimeter = fle.separator
    End If
    If IsNumeric(var) Then
  '   fle.putbuff = fle.putbuff & delimeter & Format$(var, formt)
      fle.putbuff = fle.putbuff & delimeter & CStr(var)
      Else
        If var = "true" Or var = "false" Then
          fle.putbuff = fle.putbuff & delimeter & var
        Else
          fle.putbuff = fle.putbuff & delimeter & """" & var & """"
        End If
      End If
  End If
End Sub

Sub reset_csv(fle As csv)
  If fle.fnum <> 0 Then
    Seek fle.fnum, 1
    get_line fle
  End If
End Sub

Sub set_separator(fle As csv, sep As String)
  If fle.fnum <> 0 And Len(sep) = 1 Then
    fle.separator = sep
  End If
End Sub

Function StripTerminator(ByVal strString As String) As String
  Dim intZeroPos As Integer
  intZeroPos = InStr(strString, Chr$(0))
  If intZeroPos > 0 Then
    StripTerminator = Left$(strString, intZeroPos - 1)
  Else
    StripTerminator = strString
  End If
End Function

