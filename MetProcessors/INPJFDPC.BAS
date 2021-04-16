Option Explicit

Declare Function GetModuleUsage% Lib "Kernel" (ByVal hModule%)
Global ParmFileName$    'Path and name of saved info file
Global OutFileName$     'Path and Name of output file
Global JfdFileName$     'Path and Name of jfd data file
Global lat          'Latitude of Station
Global wndhgt       'Height of wind measurements
Global z0meas       'Surface Roughness at Meas. site
Global z0appl       'Surface Roughness at Application site
Global hrjfd        'Number of hours in JFD
Global numstab      'Number of Stability Classes
Global numwnddir    'Number of Wind Direction Classes
Global numwndspd    'Number of Wind Speed Classes
Global midval(8)    'Midpoint Values for Wind Speed Classes
Global MaxTemp(12)  'Average Maximum Temperature
Global MinTemp(12)  'Average Minimum Temperature
Global DoPrcp$      'Flag on whether to include prcp
Global LgtRain(5)   'Probabilty of Light Rain
Global ModRain(5)   'Probability of Mod/Hvy Rain
Global LgtSnow(5)   'Probability of Light snow
Global ModSnow(5)   'Probability of mod/hvy snow

Sub CheckFile (infile$, ibad)
    Dim inp$
    Dim pth$, fname$, ext$

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

Sub ChkParm (inp$)
    Dim t$
    Dim nn%, i

    nn% = FreeFile
    Open inp$ For Input As nn%
        OutFileName$ = NEXTTOKEN$(nn%, t$)
        JfdFileName$ = NEXTTOKEN$(nn%, t$)
        lat = Val(NEXTTOKEN$(nn%, t$))
        wndhgt = Val(NEXTTOKEN$(nn%, t$))
        z0meas = Val(NEXTTOKEN$(nn%, t$))
        z0appl = Val(NEXTTOKEN$(nn%, t$))
        numstab = Val(NEXTTOKEN$(nn%, t$))
        numwnddir = Val(NEXTTOKEN$(nn%, t$))
        numwndspd = Val(NEXTTOKEN$(nn%, t$))
        For i = 1 To numwndspd
            midval(i) = Val(NEXTTOKEN$(nn%, t$))
        Next i
        For i = 1 To 12
            MaxTemp(i) = Val(NEXTTOKEN$(nn%, t$))
        Next i
        For i = 1 To 12
            MinTemp(i) = Val(NEXTTOKEN$(nn%, t$))
        Next i
        DoPrcp$ = NEXTTOKEN$(nn%, t$)
        If DoPrcp$ = "T" Then
            For i = 1 To 5
                LgtRain(i) = Val(NEXTTOKEN$(nn%, t$))
            Next i
            For i = 1 To 5
                ModRain(i) = Val(NEXTTOKEN$(nn%, t$))
            Next i
            For i = 1 To 5
                LgtSnow(i) = Val(NEXTTOKEN$(nn%, t$))
            Next i
            For i = 1 To 5
                ModSnow(i) = Val(NEXTTOKEN$(nn%, t$))
            Next i
        
        Else
            For i = 1 To 5
                LgtRain(i) = 0
                ModRain(i) = 0
                LgtSnow(i) = 0
                LgtRain(i) = 0
            Next i
        End If
        hrjfd = Val(NEXTTOKEN$(nn%, t$))

    Close nn%

End Sub

Sub GetNamePath (nitem$, path$, FilName$, ext$)
    Dim nwitem$, tmp$, tstr$
    Dim leng, flag, i, len1
    
    nwitem$ = NEXTTOKEN(-1, nitem$)
    leng = Len(nwitem$)
    tmp$ = ""
    path$ = ""
    FilName$ = ""
    ext$ = ""
    flag = 0
    For i = leng To 1 Step -1
      tstr$ = Mid$(nwitem$, i, 1)
      If tstr$ = "." Then
        ext$ = tmp$
        tmp$ = ""
      Else
        tmp$ = tstr$ + tmp$
      End If
      If tstr$ = "\" And flag <> 0 Then
         path$ = tstr$ + path$
      ElseIf tstr$ = "\" And flag = 0 Then
         len1 = Len(tmp$)
         FilName$ = Mid$(tmp$, 2, len1)
         flag = 1
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
    Dim l$, l1$, l2$, l3$
    Dim nn%, i

    nn% = FreeFile
    Open nwfile$ For Output As nn%
        l$ = LTrim$(RTrim$(OutFileName$))
        Print #nn%, l$
        l$ = LTrim$(RTrim$(JfdFileName$))
        Print #nn%, l$
        l$ = LTrim$(RTrim$(lat))
        Print #nn%, l$
        l$ = LTrim$(RTrim$(wndhgt))
        Print #nn%, l$
        l$ = LTrim$(RTrim$(z0meas))
        Print #nn%, l$
        l$ = LTrim$(RTrim$(z0appl))
        Print #nn%, l$
        l$ = LTrim$(RTrim$(numstab))
        Print #nn%, l$
        l$ = LTrim$(RTrim$(numwnddir))
        Print #nn%, l$
        l$ = LTrim$(RTrim$(numwndspd))
        Print #nn%, l$
        l$ = ""
        For i = 1 To numwndspd - 1
            l$ = l$ + LTrim$(RTrim$(midval(i))) + ","
        Next i
        l$ = l$ + LTrim$(RTrim$(midval(numwndspd)))
        Print #nn%, l$
        l$ = ""
        l1$ = ""
        For i = 1 To 12
            l$ = l$ + LTrim$(RTrim$(MaxTemp(i))) + ","
            l1$ = l1$ + LTrim$(RTrim$(MinTemp(i))) + ","
        Next i

        Print #nn%, l$
        Print #nn%, l1$

        l$ = LTrim$(RTrim$(DoPrcp$))
        Print #nn%, l$

        If DoPrcp$ = "T" Then

            l$ = ""
            l1$ = ""
            l2$ = ""
            l3$ = ""
            
            For i = 1 To 5
                
                l$ = l$ + LTrim$(RTrim$(LgtRain(i))) + ","
                l1$ = l1$ + LTrim$(RTrim$(ModRain(i))) + ","
                l2$ = l2$ + LTrim$(RTrim$(LgtSnow(i))) + ","
                l3$ = l3$ + LTrim$(RTrim$(ModSnow(i))) + ","
            Next i
            Print #nn%, l$
            Print #nn%, l1$
            Print #nn%, l2$
            Print #nn%, l3$

        End If
        l$ = LTrim$(RTrim$(hrjfd))
        Print #nn%, l$

    Close #nn%
End Sub

