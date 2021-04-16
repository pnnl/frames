Attribute VB_Name = "Gather"
Option Explicit

Sub loadprm()
  Dim done As Integer
  Dim i As Long
  Dim j As Long
  Dim k As Long
  Dim fle As parmfile
  Dim temp As parmrec
  Dim fnum As Long
  Dim filename As String
  Dim storage As String
  Dim putline As String
  Dim header As String
  Dim numlayers As Long
  Dim infile As csv
  Dim values As Variant
 
  On Error Resume Next
  'get the number of layers and
  'the name of the outputfile
  If open_parm(fle, fuiname, 2) Then
    Do Until EOCF(fle.file)
      If read_parmrec(fle, temp) Then
        Select Case temp.pname
          Case modName
            For i = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                Select Case temp.pname
                Case "HelpParamOutFile"
                  filename = temp.pval
                Case "HelpD10NumLayers"
                  numlayers = Val(temp.pval)
                End Select
              End If
            Next
            Exit Do
          Case Else
            For i = 1 To temp.idx1
              get_line fle.file
            Next
        End Select
      End If
    Loop
    close_parm fle
  Else
    PutError "Can't find or open file " & fuiname
    End
  End If
  
  
  'Get results and place them in putline
  j = 0
  done = 0
  putline = ""
  fnum = FreeFile
  open_csv infile, filename, F_READ
  Do Until EOCF(infile) Or done = 2
    If InStr(1, infile.getbuff, "AVERAGE ANNUAL TOTALS") > 0 Then
      get_line infile
      done = done + 1
      Do Until InStr(1, infile.getbuff, "********************") > 0
        storage = get_val(infile)
        values = Split(storage, " ")
        If storage <> "" Then
          Select Case values(0)
            Case "PRECIPITATION", "RUNOFF", "EVAPOTRANSPIRATION", "CHANGE"
              j = j + 1
              putline = putline & storage & ", "
            Case "LATERAL"
              j = j + 1
              get_line infile
              infile.getbuff = Replace(infile.getbuff, "  ", " ")
              putline = putline & Replace(storage, "COLLECTED", "COLLECTED " + infile.getbuff) & ", "
            Case "PERCOLATION/LEAKAGE"
              j = j + 1
              get_line infile
              infile.getbuff = Replace(infile.getbuff, "  ", " ")
              putline = putline & Replace(storage, "THROUGH", "THROUGH " + infile.getbuff) & ", "
            Case "AVERAGE"
              j = j + 1
              get_line infile
              infile.getbuff = Replace(infile.getbuff, "  ", " ")
              putline = putline & Replace(storage, "TOP", "TOP " + infile.getbuff) & ", , , "
          End Select
        End If
        get_line infile
      Loop
    End If
    If InStr(1, infile.getbuff, "FINAL WATER STORAGE") Then
      done = done + 1
      get_line infile
      get_line infile
      get_line infile
      For k = 1 To numlayers
        get_line infile
        putline = putline & "Final water storage layer " & infile.getbuff & ", "
        get_line infile
      Next
    End If
    get_line infile
  Loop
  Close #fnum
  
  ' format results into comma seperated
  For k = 1 To 5
    putline = Replace(putline, "   ", "  ")
  Next
  putline = Replace(putline, "(", "")
  putline = Replace(putline, ")", "")
  putline = Replace(putline, "  ", ",")
  putline = Replace(putline, ",,", ",")


  ' find out if this the first iteration
  ' if filesize < 1 then iteration is 1
  fnum = FreeFile
  filename = Replace(RunName, "~senso", "(TMP).SUF")
  Open filename For Input As fnum
  i = LOF(fnum)
  Close #fnum
  
  ' kill old file if iteration = 1
  filename = filename + ".SUF"
  fnum = FreeFile
  If i < 1 Then
    If Dir(filename) <> "" Then
      Kill filename
    End If
  End If
  Open filename For Append As fnum
  
  ' write header line if first iteration
  If i < 1 Then
    For k = 1 To j
      header = header + "Label,(in.),Std Dev(in.),(ft^3),(%),"
    Next
    For k = 1 To numlayers
      header = header + "Label, (in.), (vol/vol),"
    Next
    Print #fnum, header
  End If
  
  Print #fnum, putline
  Close #fnum
End Sub

Sub Main()
  StartModule Nothing, App.title, 5
  If InStr(1, RunName, "~senso") Then loadprm
  EndModule
End Sub


