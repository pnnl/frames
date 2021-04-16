Attribute VB_Name = "KD_IO"
Type kdrec
  cas As String
  factor(9) As Double
End Type

Public KD_loaded As Long
Dim KD_count As Long
Dim KD_table() As kdrec

Function get_kd(cas As String, idx As Long) As Double
 Dim i As Long
 get_kd = -1
  If KD_loaded And idx > 0 Then
    For i = 1 To KD_count
      If StrComp(cas, KD_table(i).cas) = 0 Then
        get_kd = KD_table(i).factor(idx)
        Exit Function
      End If
    Next
  End If
End Function

Sub load_kd()
  Dim fle As csv
  Dim temp As kdrec
  Dim fname As String
  
  fname = Space$(256)
  GetPrivateProfileString "App Path", "FUI", App.Path, fname, 256, FRAMES_INI
  fname = Trim(StripTerminator(fname)) & "\kd_data.csv"
  
  KD_count = 0
  If open_csv(fle, fname, 2) Then
    Do Until EOCF(fle)
      If read_kdrec(fle, temp) Then
        KD_count = KD_count + 1
        ReDim Preserve KD_table(KD_count) As kdrec
        KD_table(KD_count) = temp
      End If
    Loop
    close_csv fle
    If KD_count > 0 Then
      KD_loaded = True
    Else
      MsgBox "Error in file:" & UCase(fname) & Chr(10) & "Known KD's not used.", 64, "KD Lookup"
      KD_loaded = False
    End If
  Else
    MsgBox "Unable to locate or open:" & UCase(fname) & Chr(10) & "Known KD's not used.", 64, "KD  Lookup"
    KD_loaded = False
  End If
End Sub

Function read_kdrec(fle As csv, temp As kdrec) As Long
  temp.cas = get_val(fle)
  For i = 1 To 9
    temp.factor(i) = Val(get_val(fle))
  Next
  get_line fle
  read_kdrec = True
  If temp.cas = "" Then read_kdrec = False
End Function

Sub write_kdrec(fle As csv, temp As kdrec)
  Dim i As Long
  put_val fle, temp.cas, ""
  For i = 1 To 9
    put_val fle, temp.factor(i), ""
  Next
  put_line fle
End Sub

