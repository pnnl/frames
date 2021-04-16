Attribute VB_Name = "PopViewLib"
Option Explicit

Public POPStatus As Boolean

Public Const stNA As String = "N.A."  ' Not Applicable (first value for cancer organ, rad dose organ, and progeny indep. var. groups)
Public Const stComma As String = ","
Public Const stRead As String = "Error reading or processing"
Public Const stParse As String = "Error parsing"
Public Const stNotNumber As String = "Error: Expected a positive numeric value in"
Public Const stNotNumeric As String = "Error: Expected a numeric value in"
Public Const stMissing As String = "Error: Expected a non-null value in"
Public Const stDataSetTypeError As String = "Expected either ""acute"" or ""chronic"" in"
Public Const stInsert As String = "Error inserting "
Const iMaxPopLocs As Integer = 321  '**Max. number of population locations (cities, towns) in a population site file
Const iMaxPopAgeGroups As Integer = 5  '**Max. number of population age groups in a population site file
Const iMaxPRCombos  As Integer = 1000  '**Max. total no. of Pathway / Route combos (used to dimension the PR arrays)
Public mstPopSiteName As String
Public mlNumPopAgeGroups As Long  'Actual umber of age groups defined for this population site
Public mlNumPopLocs As Long   ' Actual number of locations (cities, towns) in this population site
Public mstPopAgeGroupName(1 To iMaxPopAgeGroups) As String
Public miPopAgeGroupLower(1 To iMaxPopAgeGroups) As Integer
Public miPopAgeGroupUpper(1 To iMaxPopAgeGroups) As Integer
Public mstPopAgeGroupUnits(1 To iMaxPopAgeGroups) As String
Public mstPopLocName(1 To iMaxPopLocs) As String
Public mdPopLocArea(1 To iMaxPopLocs) As Double
Public mstPopLocAreaUnits(1 To iMaxPopLocs) As String
Public mdPopLocX(1 To iMaxPopLocs) As Double    ' x-coordinate (longitudinal) of population location
Public mdPopLocY(1 To iMaxPopLocs) As Double    ' y-coordinate (latitudinal) of population location
Public mstPopLocCoordUnits(1 To iMaxPopLocs) As String   ' Coordinate (x,y) units -- assume the same units for both x and y
Public mlPopCountAG1(1 To iMaxPopLocs) As Long
Public mlPopCountAG2(1 To iMaxPopLocs) As Long
Public mlPopCountAG3(1 To iMaxPopLocs) As Long
Public mlPopCountAG4(1 To iMaxPopLocs) As Long
Public mlPopCountAG5(1 To iMaxPopLocs) As Long
Public mlLocNearest(1 To iMaxPopLocs) As Long

Public Function FindNearest(count As Long, x() As Double, y() As Double)
  'A subroutine to go through one list of coordinates and the nearest in the given set
  Dim i As Integer, j As Integer
  Dim dist(1 To iMaxPopLocs) As Double ' Distance from i'th mdPopLocX
  Dim d As Double
  If (mlNumPopLocs > 1) Then
    i = 1
    For j = 1 To count
      d = (x(j) - mdPopLocX(i)) ^ 2 + (y(j) - mdPopLocY(i)) ^ 2
      mlLocNearest(i) = j
      dist(j) = d
    Next j
  End If
  For i = 2 To mlNumPopLocs
    For j = 1 To count
      d = (x(j) - mdPopLocX(i)) ^ 2 + (y(j) - mdPopLocY(i)) ^ 2
      If (d < dist(j)) Then
        mlLocNearest(i) = j
        dist(j) = d
      End If
    Next j
  Next i
End Function

Public Function ReadPopFile(filename As String) As String
' Karl Castleton split this code out from the Go button of RAB code
'**Open and read the Population file whose full path name is contained in the filename String.
    Dim iPopFile As Integer     ' POP file number
    Dim j As Integer
    Dim k As Integer
    Dim kk As Integer
    Dim stLine As String
    Dim stWork As String
    Dim stToken As String
    Dim stLastLine As String
    Dim stErr As String
    Dim pFileOpen As Boolean
    
  pFileOpen = False
  POPStatus = False
  If (Len(filename) = 0) Then
    ifxMsgBox "Please enter a pathname for the Population file."
    Exit Function
  End If
  If (Not CheckFileExtension(filename, "POP", stWork)) Then
    ifxMsgBox "Please correct the Population file pathname.  The file pathname must not end with a ""\""."
    Exit Function
  End If
'***Now open and read the population file***
    iPopFile = FreeFile
    stLine = "(No lines read from file)"
    stErr = "Error attempting to get Population data from file " & filename
    Open filename For Input As #iPopFile
    pFileOpen = True
    
    stErr = "Error: Unexpected End-of-File in Population file. "
    If (EOF(iPopFile)) Then GoTo GoError
    stErr = "Error attempting to read first line of Population file."
    Line Input #iPopFile, stLine
    stWork = stLine
    j = iNextParseToken(stWork, mstPopSiteName, ",")
    stErr = stMissing & " site name in Population file. "
    If (j <= 0) Then GoTo GoError
    
    j = iNextParseToken(stWork, stToken, ",")
    stErr = stMissing & " age group count in Population file " & vbCrLf & "Line: " & stLine
    If (j <= 0) Then GoTo GoError
    stErr = stNotNumber & " age group count in Population file " & vbCrLf & "Line: " & stLine
    If (Not IsPositiveNumeric(stToken)) Then GoTo GoError
    mlNumPopAgeGroups = CLng(stToken)
    stErr = "Number of population age groups must be " & iMaxPopAgeGroups & " or less."
    If (mlNumPopAgeGroups > iMaxPopAgeGroups) Then GoTo GoError
    
    j = iNextParseToken(stWork, stToken, ",")
    stErr = stMissing & " location count in Population file " & vbCrLf & "Line: " & stLine
    If (j <= 0) Then GoTo GoError
    stErr = stNotNumber & " location count in Population file " & vbCrLf & "Line: " & stLine
    If (Not IsPositiveNumeric(stToken)) Then GoTo GoError
    mlNumPopLocs = CLng(stToken)
    stErr = "Number of population locations must be " & iMaxPopLocs & " or less."
    If (mlNumPopLocs > iMaxPopLocs) Then GoTo GoError
    
  For k = 1 To mlNumPopAgeGroups
    stErr = "Unexpected End-of-File in Population file. "
    If (EOF(iPopFile)) Then GoTo GoError
    stErr = "Error reading Population file" & vbCrLf & "Previous line: " & stLine
    Line Input #iPopFile, stLine
    stWork = stLine
    j = iNextParseToken(stWork, mstPopAgeGroupName(k), ",")
    stErr = stMissing & " age group name in Population file " & vbCrLf & "Line: " & stLine
    If (j <= 0) Then GoTo GoError
    
    j = iNextParseToken(stWork, stToken, ",")
    stErr = stMissing & " age group lower age in Population file " & vbCrLf & "Line: " & stLine
    If (j <= 0) Then GoTo GoError
    stErr = stNotNumber & " age group lower age in Population file " & vbCrLf & "Line: " & stLine
    If (Not IsNonNegativeNumeric(stToken)) Then GoTo GoError  ' 0 is allowable
    miPopAgeGroupLower(k) = CLng(stToken)
    
    j = iNextParseToken(stWork, stToken, ",")
    stErr = stMissing & " age group upper age in Population file " & vbCrLf & "Line: " & stLine
    If (j <= 0) Then GoTo GoError
'Allow zero for upper age of range also:  Perhaps "infant" is defined as 0 to 0 (i.e., children < 1 year old).
    stErr = stNotNumber & " age group upper age in Population file " & vbCrLf & "Line: " & stLine
    If (Not IsNonNegativeNumeric(stToken)) Then GoTo GoError
    miPopAgeGroupUpper(k) = CLng(stToken)
    stErr = "Error in population file: Upper age of age group is less than lower age."
    If (miPopAgeGroupUpper(k) < miPopAgeGroupLower(k)) Then GoTo GoError
    
    j = iNextParseToken(stWork, mstPopAgeGroupUnits(k), ",")
    stErr = stMissing & " age units in Pop. data file: " & vbCrLf & "Line: " & stLine
    If (j <= 0) Then GoTo GoError
    
  Next k
  
  For k = 1 To mlNumPopLocs
    stErr = "Error: Unexpected End-of-File in Population file. "
    If (EOF(iPopFile)) Then GoTo GoError
    stErr = "Error reading Population file" & vbCrLf & "Previous line: " & stLine
    Line Input #iPopFile, stLine
    stWork = stLine
    j = iNextParseToken(stWork, mstPopLocName(k), ",")
    stErr = stMissing & " location name in Population file " & vbCrLf & "Line: " & stLine
    If (j <= 0) Then GoTo GoError

    j = iNextParseToken(stWork, stToken, ",")
    stErr = stMissing & " area in Population file " & vbCrLf & "Line: " & stLine
    If (j <= 0) Then GoTo GoError
    stErr = stNotNumber & " area in Population file " & vbCrLf & "Line: " & stLine
    If (Not IsPositiveNumeric(stToken)) Then GoTo GoError  ' 0 is allowable
    mdPopLocArea(k) = CLng(stToken)
    
    j = iNextParseToken(stWork, mstPopLocAreaUnits(k), ",")
    stErr = stMissing & " area units in Population file " & vbCrLf & "Line: " & stLine
    If (j <= 0) Then GoTo GoError
    
    j = iNextParseToken(stWork, stToken, ",")
    stErr = stMissing & " x-coordinate in Population file " & vbCrLf & "Line: " & stLine
    If (j <= 0) Then GoTo GoError
    stErr = stNotNumber & " x-coordinate in Population file " & vbCrLf & "Line: " & stLine
    If (Not IsPositiveNumeric(stToken)) Then GoTo GoError  ' 0 is allowable
    mdPopLocX(k) = CDbl(stToken)
    
    j = iNextParseToken(stWork, mstPopLocCoordUnits(k), ",")
    stErr = stMissing & " coordinate units in Population file " & vbCrLf & "Line: " & stLine
    If (j <= 0) Then GoTo GoError
   
    j = iNextParseToken(stWork, stToken, ",")
    stErr = stMissing & " y-coordinate in Population file " & vbCrLf & "Line: " & stLine
    If (j <= 0) Then GoTo GoError
    stErr = stNotNumber & " y-coordinate in Population file " & vbCrLf & "Line: " & stLine
    If (Not IsPositiveNumeric(stToken)) Then GoTo GoError  ' 0 is allowable
    mdPopLocY(k) = CDbl(stToken)
    
'Skip the next token: y-coordinate units (we will assume the x- and y-coordinate units should be the same).
    
    'Read the line with the population counts for each age group for this location
    stErr = "Error: Unexpected End-of-File in Population file. "
    If (EOF(iPopFile)) Then GoTo GoError
    stErr = "Error reading Population file" & vbCrLf & "Previous line: " & stLine
    Line Input #iPopFile, stLine
    stWork = stLine
    
    For kk = 1 To mlNumPopAgeGroups
        j = iNextParseToken(stWork, stToken, ",")
        stErr = stMissing & " population count in Population file " & vbCrLf & "Line: " & stLine
        If (j <= 0) Then GoTo GoError
        stErr = stNotNumber & " population count in Population file " & vbCrLf & "Line: " & stLine
        If (Not IsNonNegativeNumeric(stToken)) Then GoTo GoError  ' 0 is allowable (very small town?)
        Select Case kk
        Case 1
          mlPopCountAG1(k) = CLng(stToken)
        Case 2
          mlPopCountAG2(k) = CLng(stToken)
        Case 3
          mlPopCountAG3(k) = CLng(stToken)
        Case 4
          mlPopCountAG4(k) = CLng(stToken)
        Case 5
          mlPopCountAG5(k) = CLng(stToken)
    
        End Select
    
    Next kk
    
  Next k
  
    Close iPopFile
    ReadPopFile = "Success"
    POPStatus = True
    Exit Function
GoError:
  ReadPopFile = stErr
    On Error Resume Next
    If (pFileOpen) Then
        Close iPopFile
    End If
  ifxMsgBox stErr
End Function

