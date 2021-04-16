Option Explicit

Declare Function GetModuleUsage% Lib "Kernel" (ByVal hModule%)
Global ParmFileName$    'Path and Name of parameter file
Global OutFileName$     'Path and Name of output file
Global SfcFileName$     'Path and Name of hourly sfc data file
Global lat              'Latitude of Station
Global lon              'longitude of station
Global timez            'Number of hours from GMT
Global wndhgt           'Height of wind measurements
Global z0meas           'Surface Roughness at Meas. site
Global z0appl           'Surface Roughness at Application site
Global doTD$            'Flag on whether using TD3240 data
Global td32file$        'Path and Name of TD3240 data
Global tdform$          'Format that TD3240 data is in

Sub CheckFile (infile$, ibad)
    Dim inp$, pth$, fname$, ext$
    

    inp$ = infile$

    ibad = 1
    
    Call GetNamePath(inp$, pth$, fname$, ext$)

    'Check if filename is greater than 8 char. or less than 1
    'Check if ext is greater than 3 char.
    If Len(fname$) > 8 Or Len(fname$) < 1 Then
        ibad = 0
    ElseIf Len(ext$) > 3 Then
        ibad = 0
    End If



End Sub

Sub ChkParm (nwfile$)
    Dim t$, chk$
    Dim nn%
    
    nn% = FreeFile
    Open nwfile$ For Input As nn%
        OutFileName$ = NEXTTOKEN$(nn%, t$)
        SfcFileName$ = NEXTTOKEN$(nn%, t$)
        chk$ = NEXTTOKEN$(nn%, t$)
        lat = Val(NEXTTOKEN$(nn%, t$))
        lon = Val(NEXTTOKEN$(nn%, t$))
        timez = Val(NEXTTOKEN$(nn%, t$))
        z0meas = Val(NEXTTOKEN$(nn%, t$))
        z0appl = Val(NEXTTOKEN$(nn%, t$))
        wndhgt = Val(NEXTTOKEN$(nn%, t$))
        doTD$ = NEXTTOKEN$(nn%, t$)
        If doTD$ = "T" Then
            td32file$ = NEXTTOKEN$(nn%, t$)
            tdform$ = NEXTTOKEN$(nn%, t$)
        End If
    Close #nn%

End Sub

Sub GetNamePath (nitem$, path$, FilName$, ext$)
    Dim nwitem$, tmp$, tstr$
    Dim leng, Flag, i, len1
    
    nwitem$ = NEXTTOKEN(-1, nitem$)
    leng = Len(nwitem$)
    tmp$ = ""
    path$ = ""
    FilName$ = ""
    ext$ = ""
    Flag = 0
    For i = leng To 1 Step -1
      tstr$ = Mid$(nwitem$, i, 1)
      If tstr$ = "." Then
        ext$ = tmp$
        tmp$ = ""
      Else
        tmp$ = tstr$ + tmp$
      End If
      If tstr$ = "\" And Flag <> 0 Then
         path$ = tstr$ + path$
      ElseIf tstr$ = "\" And Flag = 0 Then
         len1 = Len(tmp$)
         FilName$ = Mid$(tmp$, 2, len1)
         Flag = 1
      End If
    Next i
    'FilName$ = Mid$(nwitem$, 1, leng - 1)
   '****Debug
    'msg$ = "The Filename is " + FilName$
    'msg$ = msg$ + Chr$(13) + Chr$(10)
    'msg$ = msg$ + "The path is " + path$ + " with extenstion " + ext$
    'ms = MsgBox(msg$)

End Sub

Sub ShowStatus (s$)
    Dim de
    'shows text in the status window
    On Local Error Resume Next
    If StatusForm.Visible = 0 Then
        StatusForm.Show
    End If
    If StatusForm.WindowState = 1 Then
        StatusForm.WindowState = 0
    End If
    StatusForm.StatusText.Text = s$
    StatusForm.StatusText.SelStart = Len(s$)
    StatusForm.Refresh
    StatusForm.SetFocus
    de = DoEvents()
End Sub

Sub StatusTitle (t$)
    If t$ = "" Then
        StatusForm.Caption = "ERROR Message"
    Else
        StatusForm.Caption = "ERROR Message " + t$
    End If
End Sub

Sub WRITEERR (infile$)
    Dim l$, ln$
    Dim nn%

    nn% = FreeFile
    Open infile$ For Input As nn%
        Do While Not EOF(nn%)
            Line Input #nn%, ln$
            l$ = l$ + ln$ + Chr$(13) + Chr$(10)
        Loop

    Close nn%
            
    Call StatusTitle("")
    Call ShowStatus(l$)
End Sub

Sub WriteRSFile (nwfile$)
    Dim l$, wrt$
    Dim nn%

    nn% = FreeFile
    Open nwfile$ For Output As nn%
        l$ = LTrim$(RTrim$(OutFileName$))
        Print #nn%, l$
        l$ = LTrim$(RTrim$(SfcFileName$))
        Print #nn%, l$
        wrt$ = "CD144"
        l$ = LTrim$(RTrim$(wrt$))
        Print #nn%, l$
        l$ = LTrim$(RTrim$(lat)) + "," + LTrim$(RTrim$(lon))
        Print #nn%, l$
        l$ = LTrim$(RTrim$(timez))
        Print #nn%, l$
        l$ = LTrim$(RTrim$(z0meas))
        Print #nn%, l$
        l$ = LTrim$(RTrim$(z0appl))
        Print #nn%, l$
        l$ = LTrim$(RTrim$(wndhgt))
        Print #nn%, l$
        l$ = LTrim$(RTrim$(doTD$))
        Print #nn%, l$
        If doTD$ = "T" Then
            l$ = LTrim$(RTrim$(td32file$))
            Print #nn%, l$
            l$ = LTrim$(RTrim$(tdform$))
            Print #nn%, l$
        End If
    Close #nn%
End Sub

