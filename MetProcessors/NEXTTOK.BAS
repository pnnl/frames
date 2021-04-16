
Function FndFirst (a$, b$)
'
'Returns the index of the first character from b$ that
'is found in a$
'
'A value of 0 is returned if none of the characters in b$
'are found in a$.
'
'Steven A. Stage, 12/9/94
'

    ff = 0
    ll = Len(b$)
    For ib = 1 To ll
        ii = InStr(a$, Mid$(b$, ib, 1))
        If ii <= 0 Then
            'noop
        ElseIf ff <= 0 Then
            ff = ii
        Else
            If ii < ff Then
                ff = ii
            End If
        End If
    Next ib

    FndFirst = ff

End Function

Function NextLine$ (nn%, l$)

'Free field input routine.
'
'Removes the next non-blank line from l$ and returns it.
'When l$ is empty, a new line is read from unit nn%.
'
'If nn%<=0 then no data will be read.
'When l$ is exhausted and nn% is at an EOF then t$="" is returned.
'
'Anything following an apostrophe (!) is considered to be
'a comment and is ignored.
'
    
    t$ = ""

TrimLine:

    'remove comments

    ii = InStr(l$, "!")
    If ii > 0 Then l$ = Left$(l$, ii - 1)
    l$ = LTrim$(RTrim$(l$))

    If l$ = "" Then

        If nn% <= 0 Then GoTo ExitNextLine2
        
        If EOF(nn%) Then GoTo ExitNextLine

        Line Input #nn%, l$
        GoTo TrimLine

    End If

ExitNextLine:

    If l$ = "" Then GoTo TrimLine

ExitNextLine2:

    NextLine$ = l$

End Function

Function NextToken$ (nn%, l$)

'Free field input routine.
'
'Removes the next token from l$ and returns it.
'When l$ is empty, a new line is read from unit nn%.
'
'If nn%<=0 then no data will be read.
'When l$ is exhausted and nn% is at an EOF then t$="" is returned.
'
'Anything following an apostrophe (!) is considered to be
'a comment and is ignored.
'
'Tokens can be separated by spaces or by commas.
'
'A special type of token is allowed:
'
'   This special token starts with a slash, "/" and contains
'   a pair of parenthesis "(" and ")"
'   This is returned as a single token even if there are spaces
'   or commas between the parenthesis.
'
'   Special tokens may be spread across multiple lines of input.
'
    
    t$ = ""

TrimString:

    'remove comments

    ii = InStr(l$, "!")
    If ii > 0 Then l$ = Left$(l$, ii - 1)
    l$ = LTrim$(RTrim$(l$))

    If l$ = "" Then

        If nn% <= 0 Then GoTo ExitNextToken
'        tmp$ = "file num = " + Str$(nn%)
'        tmp$ = tmp$ + "Name of input file is " + InpFileName$
'        ms = MsgBox(tmp$)

        If EOF(nn%) Then GoTo ExitNextToken

        Line Input #nn%, l$
        GoTo TrimString

    End If

    ispace = InStr(l$, " ")

    'set string containing special characters that can mark the end
    'of the token
    Special$ = " ," & Chr$(13) & Chr$(10)
    
    'Find location of first special character
    isc = FndFirst(l$, Special$)
    If isc <= 0 Then isc = Len(l$) + 1

    'check for special token starting with / and containing ()
    ii = InStr(l$, "/")
    jj = InStr(l$, "(")
    If Not (ii = 1 And jj > 0 And jj < ispace) Then
        IsSpecialToken = 0
    Else
        'The start of a special token has been found
        'make sure that we also have the end
        nParens = 1
        DoneBalance = 0
        kk = jj + 1
        While Not DoneBalance
            Select Case Mid$(l$, kk, 1)
            Case "("
                nParens = nParens + 1
            Case ")"
                nParens = nParens - 1
            End Select
            If nParens = 0 Then
                DoneBalance = -1
                IsSpecialToken = -1
            ElseIf kk >= Len(l$) Then
                If nn% <= 0 Then
                    DoneBalance = -1
                    IsSpecialToken = 0
                ElseIf EOF(nn%) Then
                    DoneBalance = -1
                    IsSpecialToken = 0
                Else
                    DoneBalance = 0
                    Line Input #nn%, ll$
                    l$ = l$ + " " + ll$
                End If
            End If
            kk = kk + 1
        Wend
        
    End If

    If IsSpecialToken = 0 Then

        'this is a normal token
        t$ = Left(l$, isc - 1)
        l$ = Mid$(l$, isc)
        l$ = LTrim$(l$)
        
    Else

        'this is a special token
        t$ = Left(l$, kk)
        l$ = Mid$(l$, kk + 1)
        l$ = LTrim$(l$)
    
    End If
    
    'Remove special characters from start of strings
    isc = FndFirst(l$, Special$)
    While isc = 1 And l$ <> ""
        l$ = Mid$(l$, 2)
        isc = FndFirst(l$, Special$)
    Wend
          
    isc = FndFirst(t$, Special$)
    While isc = 1 And t$ <> ""
        t$ = Mid$(t$, 2)
        isc = FndFirst(t$, Special$)
    Wend
          
ExitNextToken:

    If t$ = "" And l$ <> "" Then GoTo TrimString

    NextToken$ = t$

End Function

