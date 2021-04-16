Attribute VB_Name = "VBCommon"
Option Explicit


'=============================================================================
'Purpose: Display a VB message box and receive a response from the user.
'Returns: Response value as a coded integer (e.g., vbYes), or just "OK".
'=============================================================================
' This function adds additional message flag(s) to make sure the message is not
' hidden behind other windows belonging to this or other applications (EXEs).
'=============================================================================
'Also writes the same text to an error file if the optional 2nd arg. (iErrorFile) is a positive number.
'
Public Function ifxMsgBox(ByVal stPrompt As String, _
                    Optional iErrorFile As Integer = -1, _
                    Optional ByVal iButtons As Long = 0, _
                    Optional ByVal stTitle As String = vbNullString) As Integer
    Dim stT As String
    On Error Resume Next
    
    stT = stTitle
    If (stT = vbNullString) Then stT = App.Title
'    If (iErrFile > 0) Then
'        Print #iErrFile, stPrompt & vbCrLf & "================================================"
'    End If
    Call PutError(stPrompt)
    
    ifxMsgBox = MsgBox(stPrompt, iButtons Or vbSystemModal, stT)  '***RAB: Include vbSystemModal
'ifxMsgBox = MsgBox(stPrompt, iButtons + vbSystemModal + VbMsgBoxSetForeground, stT)  ' Don't need VbMsgBoxSetForeground??
    Exit Function

End Function

'**-----------------------------------------------------------------**
'** Display the Default Mouse Pointer (Cursor).                     **
'**-----------------------------------------------------------------**
'
Public Sub TwCursorGo()

    Screen.MousePointer = vbNormal
    
End Sub

'**-----------------------------------------------------------------**
'** Display the Hourglass Mouse Pointer (Cursor).                   **
'**-----------------------------------------------------------------**
'
Public Sub TwCursorWait()

    Screen.MousePointer = vbHourglass

End Sub

Public Function iNextParseToken(strWorking As String, strToken As String, ByVal strDelim As String) As Integer
' strWorking contains a text string that contains zero or more substrings delimited by the leftmost character in strDelim.
' The remaining comments are written as if the delimiter character is "@".  The code will substitute the actual delimiter.
' This function parses out the next substring (exclusive of delimiter characters).
'   and puts it in strToken.  A null substring will result if strWorking begins with "@@",
'   or if strWorking = "" or "@".
' If strWorking begins with "@", strToken will contain all succeeding chars. up to but not including
'   the next "@" or the end of strWorking, whichever comes first.
' If strWorking does not begin with "@", then strToken will start with the first char. of strWorking.
' Function return value is the number of chars. in strToken
' Function value = -1 means there were no more tokens left in strWorking.
' Function value = 0 means "null value", e.g. there were two delimiters in succession with no chars. in between)
' Function value = -999 means an error occurred.
' On return, strWorking contains what is left of itself on entering, after the leading "@", if any,
'    and strToken, if any, have been removed.
'
Dim iPos As Integer, iPos2 As Integer
Dim stMyDelim As String
Const sDQ As String = """"    ' Double-quote character
Dim jLen As Long

On Error GoTo ParseError

'Get delimiter char.
stMyDelim = Left$(strDelim, 1)

If (strWorking = "" Or strWorking = stMyDelim) Then
    strToken = ""
    strWorking = ""
    iNextParseToken = -1
Else
    iPos = InStr(strWorking, stMyDelim)
    If (iPos = 1) Then  ' 1st char is delimiter char., strip it from the front of strWorking
        strWorking = Mid$(strWorking, 2)
        iPos = InStr(strWorking, stMyDelim)
    End If
    If (iPos = 0) Then
        strToken = Trim$(strWorking)
        strWorking = ""
    Else
        ' Return all chars. from pos. 1 to char. just prior to first instance of delimiter char.
        strToken = Trim$(Mid(strWorking, 1, iPos - 1))
        strWorking = Trim$(Mid(strWorking, iPos + 1))  '08/01/03 RAB: Omit leading delimiter char. in returned strWorking
    End If
    'Now, if the delimiter is not the character ", check to see if the string begins and ends with "
    'If so, strip off both beginning and ending double-quote characters
    If (stMyDelim <> sDQ) Then
        If ((Left$(strToken, 1) = sDQ) And (Right$(strToken, 1) = sDQ)) Then
            jLen = Len(strToken)
            strToken = Mid$(strToken, 2, jLen - 2)
        End If
    End If
    iNextParseToken = Len(strToken)
End If
Exit Function

ParseError:
    ifxMsgBox "Error parsing the following string: " & vbCrLf & strWorking
    iNextParseToken = -999
    Exit Function
    
End Function

Public Function CheckFileExtension(stFilePath As String, ByVal stDesiredExt As String, stReturn As String) As Boolean

' Check the input filepath string for a file extension (presence of at least one "." after the last "\")
'
' Input/Output:
'   stFilePath:  A string that purportedly contains a valid file pathname.  See below for possible modification to stFilePath.
' Input only:
'   stDesiredExt:  A file extension (without the leading ".") to be appended to stFilePath if it has no extension.
' Output only:
'   stReturn contains the original file pathname with the ".<extension>", if there was one, stripped off.
'
'Function value returned is True if all is OK;  False if an error (such as a file path name ending in "\") was found.
'

Dim k As Integer
Dim kLen As Integer
Dim j As Integer
Dim jPos As Integer
Dim jSave As Integer

    CheckFileExtension = False
'First, look for the last "\" in the file path string
    kLen = Len(stFilePath)
    j = 0
    k = 1
    While (j <= kLen)
        k = InStr(j + 1, stFilePath, "\")
        If (k > 0) Then
          j = k
        Else 'reached end of string: j now points to the position of the last "\" in the string, or 0 if none was found.
          jPos = j
          j = kLen + 1
        End If
    Wend
    If (jPos = kLen) Then
        'ifxMsgBox "The HIF file name cannot end on a directory separator character ""\""" & vbCrLf _
                    & "Please enter a file name after the directory path."
        Exit Function
    Else
        j = jPos
        jSave = j
        k = 1
        While (j <= kLen)
            k = InStr(j + 1, stFilePath, ".")
            If (k > 0) Then
              j = k
            Else 'reached end of string: j now points to the position of the last "." in the string, or 0 if none was found.
              jPos = j
              j = kLen + 1
            End If
        Wend
    End If
    If (jPos = jSave) Then
        'No "." was found after last "\":  Assume the file path did not include a file extension, so add the default ext.
        stReturn = stFilePath
        stFilePath = stFilePath & "." & stDesiredExt
    ElseIf (jPos = kLen) Then
    ' Last character in pathname was a "."; append the passed-in extension without adding another "."
        stReturn = Left$(stFilePath, jPos - 1)
        stFilePath = stFilePath & stDesiredExt
    Else
        'Found a "." (the last one) after the "\"; Set stReturn to the left part of stFilePath up to but not including the "."
        stReturn = Left$(stFilePath, jPos - 1)
    End If
    CheckFileExtension = True
    Exit Function

End Function

Public Function IsPositiveNumeric(ByVal stN As String) As Boolean
'Return True if the string is numeric and > 0
'Return false if the string is non-numeric or <= 0
'
    IsPositiveNumeric = False
    If (IsNumeric(stN)) Then
        If (CDbl(stN) > 0#) Then
            IsPositiveNumeric = True
        End If
    End If
    Exit Function

End Function

Public Function IsNonNegativeNumeric(ByVal stN As String) As Boolean
'Return True if the string is numeric and >= 0
'Return false if the string is non-numeric or < 0
'
    IsNonNegativeNumeric = False
    If (IsNumeric(stN)) Then
        If (CDbl(stN) >= 0#) Then
            IsNonNegativeNumeric = True
        End If
    End If
    Exit Function

End Function


Public Function IsENumeric(ByVal stN As String) As Boolean
'Return True if the string is numeric (check for E format, e.g '1.234E-07')
'Return False if the string is non-numeric

Dim stToken As String
Dim stWork As String
Dim iCharCount As Integer

    IsENumeric = False
    stWork = stN
    If (InStr(1, stN, "E")) Then
'Check to see if both the characteristic and exponent parts are numeric
        iCharCount = iNextParseToken(stWork, stToken, "E")
        If (IsNumeric(stToken) And IsNumeric(stWork)) Then IsENumeric = True
    ElseIf (InStr(1, stN, "e")) Then
'Check to see if both the characteristic and exponent parts are numeric
        iCharCount = iNextParseToken(stWork, stToken, "e")
        If (IsNumeric(stToken) And IsNumeric(stWork)) Then IsENumeric = True
    Else
        IsENumeric = IsNumeric(stN)
    End If
    Exit Function
    
End Function

Public Function DirectAddressSort(ByVal nOrig As Long, ByVal nMax As Long, lOriginal() As Long, lReturn() As Long) As Boolean
'Direct copy (each value is also its (new) address) of a scrambled list of Long integers ...
' ... known to contain a set of unique integer (Long) numbers within a range of 1 to nMax.
'  e.g., a list of nOrig unique unordered pointers to record ids, ...
'  ... where the minimum possible value is 1 and the maximum possible value is nMax.
' The scrambled input list is in lOriginal;  the resulting "address = value" list (with possible zeros) is returned in lReturn.
' Caller must ensure that lReturn is declared as undimensioned (Dim lReturn() as Long).  This routine does: ReDim lReturn(nMax).

' RABurnett 08/11/03

    Dim j As Long
    
    On Error GoTo Fail_Exit
    DirectAddressSort = False
    ReDim lReturn(nMax)  'This should clear out the array (set all elements to zero).
'First fill the output array with zeros by redimensioning it at
    For j = 1 To nOrig
        lReturn(lOriginal(j)) = lOriginal(j)
    Next j
    DirectAddressSort = True
    Exit Function
    
Fail_Exit:
    Exit Function
    
End Function

Public Function Sort123(ByVal nOrig As Long, iOriginal() As Integer, _
                        nValMax As Long, _
                        lPos() As Long, lLastDup() As Long) As Boolean
'Special sort routine for arrays containing sequential 2-byte integer (1, 2, 3, ..., nValMax), where nValMax <= 32767 (or ???)
'Args. and return values are the same as in S_BSORT_I2_Index (cf.)
'
    Dim nNextOver As Long  'Points to the first element of the next available overflow partition.
    Const OverflowMarker As Long = -999  'Last entry in each bin is an overflow marker, initialized at -999
                'When the overflow marker is reached, branch to next available overflow partition, and set marker to -nNextStart
                'Where nNextStart points to the first array element of the overflow partition for this bin.
    Dim lBinSize As Long    'Bin size for base bins and overflow bins:  Compute as (nOrig / (nValMax-2)) + 2
                            
'Running index for bins representing iOriginal array values 2 thru nValMax-1 (pointer to most recently filled element of mP for this bin)
    Dim mX() As Long  'Note:  mX(1) is unused (see array nCT)
    
    Dim mP() As Long 'Partitioned array to receive the position pointers for the intermediate array values (1 < n < nValMax)
    Dim nUpperBoundMp As Long  'Current upper bound of array mP
                                'Will need to ReDim Preserve mP(nUpperBoundMp + lBinSize) whenever an overflow segment is needed
    
'Counts for integer values 1 thru (nValMax - 1)
' nct(1) serves as both a count and an index, so we doesn't need a bin in mP for values of 1
    Dim nct() As Long

'Starting element of home bin for each of the possible integer values:  2  thru  (nValMax-1)
    Dim mStart() As Long   'Note that again, element 1 is unused since we don't need a bin for values of 1
    
'Backward index pointer (from end of lPos array backwards) for values of nUnique (max. value in original array)
    Dim mXnValMax As Long  'Most recently filled lPos entry for max. orig. array value.
                           'We can compute the count of occurrences of nValMax as:  nOrig - mXnValMax + 1
                           'So, we don't need a bin in mP for values of nValMax, or a counter for the number of occurrences.
    Dim j As Long
    Dim k As Long
    Dim kk As Long
    Dim jj As Long
    Dim nNextStart As Long  'Holds the mP array position of the first cell in the next overflow partition
    Dim iValue As Integer
    Dim nCount As Long

    On Error GoTo Fail_Exit

    Sort123 = False

    If (nValMax <= 1) Then
      lLastDup(1) = nOrig
      Exit Function
    End If
    
    ReDim mX(nValMax - 1)  'Element 1 of array mX is unused.  We will not need to ReDim this array again.
    ReDim mStart(nValMax - 1)  'Element 1 is unused.  We won't need to ReDim again.
    ReDim nct(nValMax - 1)  ' Element 1 IS used.  Won't need to ReDim again.
    
'Simplify if nValMax = 2
  If (nValMax = 2) Then
  
    nCount = 0
    mXnValMax = nOrig + 1
    
    For j = 1 To nOrig
      iValue = iOriginal(j)
      If (iValue = 1) Then
          nCount = nCount + 1
          lPos(nCount) = j
      ElseIf (iValue = 2) Then
          mXnValMax = mXnValMax - 1
          lPos(mXnValMax) = j
      Else
          GoTo Fail_Exit
      End If
      
    Next j
    If (mXnValMax <> nCount + 1) Then GoTo Fail_Exit
    lLastDup(1) = nCount
    lLastDup(2) = nOrig
    
  Else
'Initialization
    lBinSize = 2 + (nOrig / (nValMax - 2))
    nUpperBoundMp = lBinSize * (nValMax - 2)
    ReDim mP(nUpperBoundMp)
    
    mXnValMax = nOrig + 1   ' We will subtract 1 each time BEFORE using the variable as an index to store a position pointer
    nNextStart = 1
    For j = 1 To nValMax - 1
        nct(j) = 0
        If (j > 1) Then
          mStart(j) = nNextStart
          mX(j) = nNextStart - 1  'Running index ready to advance to first cell in this bin
          nNextStart = nNextStart + lBinSize
          mP(nNextStart - 1) = OverflowMarker   'Put overflow link marker in the last cell in this bin
        End If
    Next j
    If (nNextStart <> nUpperBoundMp + 1) Then
        ifxMsgBox "nNextStart calculation error in Sort123"
        GoTo Fail_Exit
    End If
    Debug.Print "PAUSE1 IN SORT123"
'Now we're ready to step through the original array and throw pointers into the proper bin
    For j = 1 To nOrig
      iValue = iOriginal(j)
      If (iValue < 1 Or iValue > nValMax) Then GoTo Fail_Exit  'Value is outside the advertised range of 1 to nValMax
      Select Case iValue
        Case 1
          nct(1) = nct(1) + 1
          lPos(nct(1)) = j
        Case nValMax
          mXnValMax = mXnValMax - 1
          lPos(mXnValMax) = j
        Case Else
          nct(iValue) = nct(iValue) + 1
          mX(iValue) = mX(iValue) + 1
          If (mP(mX(iValue)) = OverflowMarker) Then
            nUpperBoundMp = nUpperBoundMp + lBinSize
            ReDim Preserve mP(nUpperBoundMp)
            mP(mX(iValue)) = -nNextStart  'Reset overflow marker to point to beginning of next overflow partition
            mX(iValue) = nNextStart
            mP(mX(iValue)) = j
            nNextStart = nNextStart + lBinSize
          Else
            mP(mX(iValue)) = j   '***Added this line on 08/11/03 !!! ***
          End If
          
      End Select
      
    Next j
    
    'Now collect the contents of the bins in their proper order
    lLastDup(1) = nct(1)
    nCount = nct(1)
    k = nct(1) + 1
    For j = 2 To nValMax - 1
        nCount = nCount + nct(j)
        lLastDup(j) = nCount
        kk = mStart(j)
        For jj = 1 To nct(j)
          If (mP(kk) < 0) Then
            kk = -mP(kk)  'Overflow link pointer
          End If
          lPos(k) = mP(kk)
          k = k + 1
          kk = kk + 1
        Next jj
    Next j
    lLastDup(nValMax) = nCount + nOrig - mXnValMax + 1
    If (lLastDup(nValMax) <> nOrig) Then
        ifxMsgBox "Running count for lLastDup on max. value (" & lLastDup(nValMax) & "didn't match total array count (" & CStr(nOrig) & ")"
        GoTo Fail_Exit
    End If
    If (k <> mXnValMax) Then
        ifxMsgBox "Sort Bin n-1 failed to meet sort bin n"
        GoTo Fail_Exit
    End If
  End If
'**DONE**
  Sort123 = True
  Exit Function
  
Fail_Exit:
    ifxMsgBox "SORT123 error:  j=" & CStr(j) & ";  iValue=" & CStr(iValue)
    Debug.Print "PAUSE"
  Exit Function
    
End Function

Public Function S_BSORT_I2_Index(ByVal n As Long, iOriginal() As Integer, _
                        nUnique As Long, _
                        lPos() As Long, lLastDup() As Long) As Boolean
'===========================================================================
'Purpose:
'  This routine sorts Integer array iOriginal() into a local array ioArray().
'  It leaves iOriginal unaltered.
'
'  It returns a Long array lPos() containing pointers to positions in
'  iOriginal such that iOriginal can be traversed in ascending order.
'
'  It also returns a "last duplicate" pointer array lLastDup().  lLastDup(k)
'  is a pointer to the entry in lPos that references the last (if duplicated)
'  or only occurrence of the kth smallest unique iOriginal array value, so that
'  the positions of multiple occurrences of a value in the original array can
'  be easily found and retrieved.  For example, if the sort yields an internal
'  sorted array (ioArray) containing the values {1,1,1,5,8,8,9,9,9,9,9} (with
'  lPos containing pointers to the corresponding iOriginal elements), then
'  lLastDup would be returned as {3,4,6,11}.  And lPos(1:3) would point to the
'  positions in iOriginal that contained the three occurrences of the smallest
'  unique element (1 in this example); lPos(4) points to the only occurrence
'  of the second smallest unique element (5 in this example); lPos(5:6) would
'  point to the two occurrences of the third smallest unique element (8); etc.
'
'  Sorting Method: Binary Sort
'
'Input Parameters:
'  n:  Total number of elements in unsorted array iOriginal.  Pointer array
'      lPos passed by caller should also be dimensioned to n or greater.
'  iOriginal():  Integer array (typically containing index pointers to
'      a finite set of categorical text values or other categorical values).
'
'Input/Output:
'  nUnique:  Number of unique (possibly duplicated) entries in iOriginal (if
'     known).  The array lLastDup passed by the caller should be dimensioned
'     to nUnique or greater.  If the number of unique entries is not known
'     by caller, caller should pass a value of zero (0) in nUnique, in which
'     case this function will calculate and return the proper value in nUnique.
'
'Output Parameters:
'  lPos():  Long array holding pointers to elements of iOriginal in sorted order.
'  lLastDup():  Long array holding pointers to elements of lPos that identify
'      multiple (1 or more) occurrences of each unique original array element.
'   (See description of "last duplicate" pointer array in Purpose section above.)
'===========================================================================
'  History:
'    08/25/88, JRW. Modified BSORT_I4 to S_BSORT_R4 (4-byte Real numbers).
'    09/11/94, TRD. Stole from IBS and converted to VB for use in FEMIS.
'    08/01/03, RAB. Converted to sort 2-byte Integer numbers;
'                Leaves original array intact;
'                Returns "last duplicate" pointer array (see above)
'                  in addition to the sort order pointer array lPos;
'                Changed some arguments and local variables from Integer
'                  to Long to accommodate array lengths > 32767.
'---------------------------------------------------------------------------
Dim ioarray() As Integer  'Working copy of iOriginal, ends up in sorted order.
Dim i As Long
Dim j As Long
Dim k As Long
Dim l As Long
Dim m As Long
Dim lmi As Long
Dim jmk As Long
Dim jmi As Long
Dim nm1 As Long
Dim ip1 As Long
Dim mmid As Long
Dim imed As Long
Dim ITT As Long
Dim iFirst As Integer
ReDim iu(1 To 36) As Long
ReDim il(1 To 36) As Long

Dim hold As Integer
Dim amed As Integer
Dim tt As Integer
'----------------------M A I N   P R O G R A M------------------------------

    On Error GoTo Fail_Exit
    
    S_BSORT_I2_Index = False
    
'**     check the input arguments for errors
        If (n <= 1) Then
            ifxMsgBox "BSORT_I2: Attempted to sort an array of length " & CStr(n)
            GoTo Fail_Exit
        End If
        
        ReDim ioarray(1 To n) As Integer

        For i = 1 To n
           lPos(i) = i
           ioarray(i) = iOriginal(i)  'Copy to local working array
        Next

        hold = ioarray(1)
        For i = 2 To n
           If (ioarray(i) <> hold) Then GoTo 90
        Next
        GoTo Success_Exit
90:

'**     check to see if the input vector is already sorted

        nm1 = n - 1
        For i = 1 To nm1
           ip1 = i + 1
        If (ioarray(i) <= ioarray(ip1)) Then GoTo 200
        GoTo 250
200:    Next
        GoTo Success_Exit
250:    m = 1
        i = 1
        j = n
305:    If (i >= j) Then GoTo 370
310:    k = i
        mmid = (i + j) / 2
        amed = ioarray(mmid)
        imed = lPos(mmid)
        If (ioarray(i) <= amed) Then GoTo 320
        ioarray(mmid) = ioarray(i)
        lPos(mmid) = lPos(i)
        ioarray(i) = amed
        lPos(i) = imed
        amed = ioarray(mmid)
        imed = lPos(mmid)
320:    l = j
        If (ioarray(j) >= amed) Then GoTo 340
        ioarray(mmid) = ioarray(j)
        lPos(mmid) = lPos(j)
        ioarray(j) = amed
        lPos(j) = imed
        amed = ioarray(mmid)
        imed = lPos(mmid)
        If (ioarray(i) <= amed) Then GoTo 340
        ioarray(mmid) = ioarray(i)
        lPos(mmid) = lPos(i)
        ioarray(i) = amed
        lPos(i) = imed
        amed = ioarray(mmid)
        imed = lPos(mmid)
        GoTo 340
330:    ioarray(l) = ioarray(k)
        lPos(l) = lPos(k)
        ioarray(k) = tt
        lPos(k) = ITT
340:    l = l - 1
        If (ioarray(l) > amed) Then GoTo 340
        tt = ioarray(l)
        ITT = lPos(l)
350:    k = k + 1
        If (ioarray(k) < amed) Then GoTo 350
        If (k <= l) Then GoTo 330
        lmi = l - i
        jmk = j - k
        If (lmi <= jmk) Then GoTo 360
        il(m) = i
        iu(m) = l
        i = k
        m = m + 1
        GoTo 380
360:    il(m) = k
        iu(m) = j
        j = l
        m = m + 1
        GoTo 380
370:    m = m - 1
        If (m = 0) Then GoTo Success_Exit
        i = il(m)
        j = iu(m)
380:    jmi = j - i
        If (jmi >= 11) Then GoTo 310
        If (i = 1) Then GoTo 305
        i = i - 1
390:    i = i + 1
        If (i = j) Then GoTo 370
        amed = ioarray(i + 1)
        imed = lPos(i + 1)
        If (ioarray(i) <= amed) Then GoTo 390
        k = i
395:    ioarray(k + 1) = ioarray(k)
        lPos(k + 1) = lPos(k)
        k = k - 1
        If (amed < ioarray(k)) Then GoTo 395
        ioarray(k + 1) = amed
        lPos(k + 1) = imed
        GoTo 390
        
Success_Exit:
' ioArray(1:n) now contains the sorted values.  Scan sequentially through
'   ioArray and populate the passed lLastDup array.
    iFirst = ioarray(1)
    k = 1
    For i = 2 To n
        If (ioarray(i) <> iFirst) Then
            iFirst = ioarray(i)  'Beginning of new set of duplicated values
            lLastDup(k) = i - 1
            k = k + 1
        End If
    Next i

    lLastDup(k) = n
    If (nUnique <= 0) Then
        nUnique = k   'nUnique was not known or passed by caller; return it.
    ElseIf (k <> nUnique) Then
' Number of unique array values does not agree with passed value of nUnique
      ifxMsgBox "BSORT_I2: Actual number of unique array values (" & CStr(k) & ")" _
        & vbCrLf & "does not agree with expected number (" & CStr(nUnique) & ")"
      GoTo Fail_Exit
    End If
        
    S_BSORT_I2_Index = True
    Exit Function

Fail_Exit:
    Exit Function
    
End Function


Public Sub S_BSORT_R8(n As Long, _
                      ioarray() As Double, _
                      iPos() As Long)
'===========================================================================
'  Purpose:
'    This routine sorts array "ioarray" into itself.  It also keeps the
'    index of the position in the original array for each cell in "ipos".
'    N is currently limited to 137,000,000.
'    Sorting Method: Binary Sort
'
'  Input Parameters:
'    N                 I*4   # of elements in the array
'
'  Input/Output Parameters':
'    IOARRAY(*)        R*8   Entry - unsorted array
'                            Exit  - sorted array
'
'  Output Parameters:
'    IPOS(*)           I*4   Original location of sorted elements
'===========================================================================
'  History:
'    08/25/88, JRW. Modified BSORT_I4;
'    09/11/94, TRD. Stole from IBS and converted to VB;
'---------------------------------------------------------------------------
Dim i As Long
Dim j As Long
Dim k As Long
Dim l As Long
Dim m As Long
Dim lmi As Long
Dim jmk As Long
Dim jmi As Long
Dim nm1 As Long
Dim ip1 As Long
Dim mmid As Long
Dim imed As Long
Dim ITT As Long
ReDim iu(1 To 36) As Long
ReDim il(1 To 36) As Long

Dim hold As Double
Dim amed As Double
Dim tt As Double
'----------------------M A I N   P R O G R A M------------------------------

'**     check the input arguments for errors
        If (n <= 1) Then Exit Sub

        For i = 1 To n
           iPos(i) = i
        Next

        hold = ioarray(1)
        For i = 2 To n
           If (ioarray(i) <> hold) Then GoTo 908
        Next
        Exit Sub
908:

'**     check to see if the input vector is already sorted

        nm1 = n - 1
        For i = 1 To nm1
           ip1 = i + 1
        If (ioarray(i) <= ioarray(ip1)) Then GoTo 2008
        GoTo 2508
2008:    Next
        Exit Sub
2508:    m = 1
        i = 1
        j = n
3058:    If (i >= j) Then GoTo 3708
3108:    k = i
        mmid = (i + j) / 2
        amed = ioarray(mmid)
        imed = iPos(mmid)
        If (ioarray(i) <= amed) Then GoTo 3208
        ioarray(mmid) = ioarray(i)
        iPos(mmid) = iPos(i)
        ioarray(i) = amed
        iPos(i) = imed
        amed = ioarray(mmid)
        imed = iPos(mmid)
3208:    l = j
        If (ioarray(j) >= amed) Then GoTo 3408
        ioarray(mmid) = ioarray(j)
        iPos(mmid) = iPos(j)
        ioarray(j) = amed
        iPos(j) = imed
        amed = ioarray(mmid)
        imed = iPos(mmid)
        If (ioarray(i) <= amed) Then GoTo 3408
        ioarray(mmid) = ioarray(i)
        iPos(mmid) = iPos(i)
        ioarray(i) = amed
        iPos(i) = imed
        amed = ioarray(mmid)
        imed = iPos(mmid)
        GoTo 3408
3308:    ioarray(l) = ioarray(k)
        iPos(l) = iPos(k)
        ioarray(k) = tt
        iPos(k) = ITT
3408:    l = l - 1
        If (ioarray(l) > amed) Then GoTo 3408
        tt = ioarray(l)
        ITT = iPos(l)
3508:    k = k + 1
        If (ioarray(k) < amed) Then GoTo 3508
        If (k <= l) Then GoTo 3308
        lmi = l - i
        jmk = j - k
        If (lmi <= jmk) Then GoTo 3608
        il(m) = i
        iu(m) = l
        i = k
        m = m + 1
        GoTo 3808
3608:    il(m) = k
        iu(m) = j
        j = l
        m = m + 1
        GoTo 3808
3708:    m = m - 1
        If (m = 0) Then Exit Sub
        i = il(m)
        j = iu(m)
3808:    jmi = j - i
        If (jmi >= 11) Then GoTo 3108
        If (i = 1) Then GoTo 3058
        i = i - 1
3908:    i = i + 1
        If (i = j) Then GoTo 3708
        amed = ioarray(i + 1)
        imed = iPos(i + 1)
        If (ioarray(i) <= amed) Then GoTo 3908
        k = i
3958:    ioarray(k + 1) = ioarray(k)
        iPos(k + 1) = iPos(k)
        k = k - 1
        If (amed < ioarray(k)) Then GoTo 3958
        ioarray(k + 1) = amed
        iPos(k + 1) = imed
        GoTo 3908
End Sub

Public Function Join(ByVal n1 As Long, List1() As Long, ByVal n2 As Long, List2() As Long, ByVal nMax As Long, _
                     nReturn As Long, ListReturn() As Long, Optional ByVal bExist As Boolean = False) As Boolean
'
'"Relational Join" of two Long integer lists (arrays):
'Return the intersection of two Long integer arrays (only those values that are in both arrays).
'Assumptions:
'  (1) There are no duplicates in either input list.
'  (2) The return list is dimensioned by the caller to be at least as long as the longer input array.
'
'Inputs:
'     n1:  Number of elements in List 1
'  List1:  Long array containing List 1
'     n2:  Number of elements in List 2
'  List2:  Long array containing List 2
'   nMax:  The maximum number that List 2 can contain (also assumes the minimum number List2 can contain is 1).
' bExist:  (optional) -- If bExist is present and True, quit when you have found one (1) item that is in both List1 and List2.
'
'Outputs:
'  nReturn:  The number of elements in the joined (intersected) list.
'  ListReturn:  The returned array containing the joined list.
'  ListReturn can point to the same array as List1, but not List2.

    Dim j As Long
    Dim k As Long
    Dim LTemp As Long
    Dim BigList() As Long  'DirectAddressSort will ReDim Biglist(nMax)
    
    On Error GoTo JoinError
    Join = False
    nReturn = 0
'Sort list 2 to make the joining go faster
    If (Not DirectAddressSort(n2, nMax, List2(), BigList())) Then GoTo JoinError
        For j = 1 To n1
'***** Old method ***
'      LTemp = List1(j)
'      k = 1
'      While (k <= n2)
'        If (LTemp = List2(k)) Then
'            nReturn = nReturn + 1
'            ListReturn(nReturn) = List1(j)
'            If (bExist) Then GoTo JoinSuccess   'Exit as soon as at least one match is found
'            k = n2  'Break the "While" loop
'        End If
'        k = k + 1
'      Wend
          If (BigList(List1(j)) = List1(j)) Then
              nReturn = nReturn + 1
              ListReturn(nReturn) = List1(j)
              If (bExist) Then GoTo JoinSuccess  'Exit as soon as at least one match is found
          End If
              
        Next j
    
JoinSuccess:
    Join = True
    
JoinError:
    Exit Function
    
End Function

Public Function Linearize(ByVal iNumDims As Integer, lLength() As Long, lIndex() As Long) As Long
'Given a "virtual" array with multiple dimensions, ...
' ... convert a multi-indexed array reference to a single index into a linearized equivalent single-dimensioned vector.
'Inputs:  iNumDims -- number of dimensions in the multi-dimensional virtual array
'         lLength() -- holds the dimension lengths for each of the iNumDims dimensions
'         lIndex() -- holds the indexes, one for each dimension, that defines an element in the multi-dim. array
'Function value returns a single index pointing to the equivalent element in a linearized single-dimensioned array.
'
    Dim lBlock As Long
    Dim LTemp As Long
    Dim k As Integer
    
    On Error GoTo Fail_Exit
    Linearize = 0  'General error return code
    If (iNumDims <= 1) Then
        If (iNumDims = 1) Then Linearize = lIndex(1)
        GoTo Fail_Exit
    End If
'The number of dimensions is > 1
    lBlock = 1
    LTemp = 1   'The item itself
    For k = iNumDims To 1 Step -1
        If (lIndex(k) <= 0 Or lLength(k) <= 0 Or lIndex(k) > lLength(k)) Then
            Linearize = -k   'Error code for problem with kth dimension
            GoTo Fail_Exit
        End If
        LTemp = LTemp + lBlock * (lIndex(k) - 1)
        lBlock = lBlock * lLength(k)
    Next k
    Linearize = LTemp
'All done

Fail_Exit:
    Exit Function
    
End Function
