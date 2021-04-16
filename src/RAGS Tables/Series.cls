VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Series"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
Option Compare Text
'local variable(s) to hold property value(s)
Public num As Long 'local copy
Public numSeries As Long
Private times() As Double
Private values() As Double
Public tvalue As Double
Public tvaluect As Long
Public key As String

Public Function GetTime(i As Long) As Double
  GetTime = times(i)
End Function

Public Function GetValue(i As Long) As Double
  GetValue = values(i)
End Function

Public Sub Reset()
  num = -1
  numSeries = -1
  ReDim times(0)
  ReDim values(0)
  tvalue = -99#:  tvaluect = -1
  key = ""
End Sub

Private Sub Class_Initialize()
  num = -1
  numSeries = -1
  ReDim times(0)
  ReDim values(0)
  tvalue = -99#:  tvaluect = -1
  key = ""
End Sub

Public Sub AddInSeries(x As Series)
  Dim tnum As Long
  Dim ttimes() As Double
  Dim tvalues() As Double
  x.GetSeries tnum, ttimes, tvalues
  If tnum > 0 Then AddIn tnum, ttimes, tvalues
End Sub


Public Sub AddIn(inum As Long, itime() As Double, ivalue() As Double)
  Dim tnum As Long
  Dim ttimes() As Double
  Dim tvalues() As Double
  Dim ns As Long
  
  If inum <= 0 Then Exit Sub
  
  If num <= 0 Then
    SetSeries inum, itime, ivalue
    numSeries = 1
    Exit Sub
  End If
  
  ns = numSeries
  If ns <= 0 Then ns = 1
  
  tnum = num + inum + 4
  ReDim ttimes(0 To tnum - 1)
  ReDim tvalues(0 To tnum - 1)
  
  AddSeries num, times(0), values(0), inum, itime(0), ivalue(0), tnum, ttimes(0), tvalues(0)
  SetSeries tnum, ttimes, tvalues
  
  numSeries = ns + 1
End Sub

Public Sub ComputeAnswer()
  Dim i As Long, ctz As Long
  Dim zeroes As Long
  Dim rprob() As Double
  Dim sprob() As Double
  Dim svalue() As Double
  Dim idx() As Long
  Dim row As Long
  Dim sortv() As Double
  Dim minval As Double, maxval As Double
  Dim tstart As Long, tend As Long
  Dim status As Integer
  Dim mean As Double, mn As Double, mx As Double, med As Double, sd As Double
  Dim n As Long
  
  On Error GoTo ErrorHandler
  
  tvalue = -99#
  tvaluect = 0
  If 0 < num Then
    ReDim rprob(101)
    zeroes = IIf(mfDeMin = "All", 1, 0)
    If measureOfInterest = mct Or measureOfInterest = mRME Then
      tvalue = values(0)
      tvaluect = num
    Else
      ReDim sprob(0 To 100)
      ReDim svalue(0 To 100)
      
      status = GetProbability(zeroes, 100, num, times(0), values(0), minval, maxval, tstart, tend, sprob(0), svalue(0))
      
      If status = 1 Then
        ' less than 2 applicable points
        ctz = 0
        For i = 0 To num - 1
          If (zeroes Or ((Not zeroes) And values(i) > 0#)) Then ctz = ctz + 1
        Next i
        If ctz > 0 And (measureOfInterest <> mPE) Then
          For i = 0 To num
            If (zeroes Or ((Not zeroes) And values(i) > 0#)) Then
              tvalue = values(i)
              tvaluect = 1
              Exit For
            End If
          Next i
        End If
      End If
      If status = 0 Then
        tvalue = 0#
        tvaluect = 0
        ReDim idx(LBound(times) To UBound(times))
        ReDim sortv(LBound(values) To UBound(values))
        For i = tstart To tend
          If (zeroes Or ((Not zeroes) And svalue(i) > 0#)) Then
            idx(tvaluect) = tvaluect + 1
            sortv(tvaluect) = values(i)
            tvaluect = tvaluect + 1
          End If
        Next i
        If tvaluect > 0 Then
          ReDim Preserve idx(0 To tvaluect - 1)
          ReDim Preserve sortv(0 To tvaluect - 1)
          
          ' Random gpf's seem to point to statistical functions, especially
          ' when the series values are all zero.  Look for this condition as possible,
          ' and skip the stat function if so.
          ' Suggested statistical functions enclosed in try/catch blocks in dll also.
          ctz = 0
          For i = 0 To tvaluect - 1
            ' if both >= and <=, then must be equal...  not pretty but seems to work
            If sortv(i) >= 0# And sortv(i) <= 0# Then ctz = ctz + 1
          Next i
          If ctz = tvaluect Then
            mean = 0#
            mn = 0#
            mx = 0#
            med = 0#
            sd = 0#
          Else
            StatSort tvaluect, idx(0), sortv(0), mean, mn, mx, med, sd
          End If
       
           Select Case measureOfInterest
             Case mMIN:
               tvalue = mn
             Case mMAX:
               tvalue = mx
             Case mMID:
               If 1 = tvaluect Mod 2 Then
                tvalue = sortv(1 + (tvaluect \ 2))
               Else
'''                tvalue = (sortv(tvaluect \ 2) + sortv((tvaluect \ 2) + 1)) / 2
                
                n = (tvaluect \ 2) - 1
                If n >= 0 And n < tvaluect - 1 Then
                  tvalue = (sortv(n) + sortv(n + 1)) / 2
                End If
               End If
             Case mMED:
                tvalue = med
             Case mMN:
                tvalue = 0
                For i = 0 To tvaluect - 1
                  tvalue = tvalue + sortv(i)
                Next i
                tvalue = mean
             Case mPE: '  tvalue = RAGS.GetProbability(mfThreshold, sprob, svalue)
'''                Dim d1 As Double, d2 As Double, r As Double
'''                For i = LBound(sprob) To UBound(sprob) - 1
'''                  If sprob(i) >= mfThreshold And sprob(i + 1) <= mfThreshold Then
'''                    d1 = Abs(sprob(i + 1) - sprob(i))
'''                    d2 = Abs(mfThreshold - sprob(i))
'''                    If (d1 = 0#) Then
'''                      r = 0#
'''                    Else
'''                      r = d2 / d1
'''                    End If
'''                    tvalue = svalue(i) + r * (svalue(i + 1) - svalue(i))
'''                    Exit For
'''                  End If
'''                Next i
            ' the 95th percentile is equivalent to exceeding a value 5% of the time
            ' svalue is ordered from 0=100% to 100=
                tvalue = svalue(mfThreshold)
                
           End Select
         End If
      End If
    End If
  End If
ErrorHandler:
  If Err.Number <> 0 Then
    ''' MsgBox Err.Description & vbCrLf & "status: " & CStr(status), vbOKOnly, "Series.ComputeAnswer"
    PutLog "ERROR in Series.ComputeAnswer: " & Err.Description
    tvalue = -99#
    tvaluect = 0
  End If
End Sub
Public Function PutValue(ByRef wso As Object, ByVal inrow As Long, ByVal incol As Long)
  If tvaluect > 0 And tvalue >= 0 Then
     xlsSetCell wso, incol, inrow, tvalue, SCFMT
  End If
End Function

Public Function SetCell(ByRef wso As Object, ByVal inrow As Long, col$, lastrow$, Optional Unit$ = "") As Long
  Dim row As Long
  
  On Error GoTo ErrorHandler
  row = inrow
  If tvaluect > 0 And tvalue >= 0 Then
     xlsSetCell wso, wso.range(col).Column, row, tvalue, SCFMT
     If Unit <> "" Then xlsSetCell wso, wso.range(col).Column + 1, row, Unit
     row = row + 1: CheckRow wso, row + 1, lastrow
  End If
ErrorHandler:
  If Err.Number <> 0 Then
    MsgBox Err.Description, vbOKOnly, "Series.SetCell"
  End If
  SetCell = row
End Function

Public Sub SetSeries(n As Long, rtimes() As Double, rvalues() As Double)
  Dim lb As Long
  lb = LBound(rtimes)
  Class_Initialize
  If n > 0 Then
    num = n
    ReDim times(0 To num - 1)
    ReDim values(0 To num - 1)
    Dim i As Long
    For i = 0 To num - 1
      times(i) = rtimes(lb + i)
      values(i) = rvalues(lb + i)
    Next i
    ComputeAnswer
  End If
End Sub

Public Sub GetSeries(ByRef n As Long, ByRef rtimes() As Double, rvalues() As Double)
  ReDim rtimes(0)
  ReDim rvalues(0)
  n = num
  If num <= 0 Then Exit Sub
  ReDim rtimes(n)
  ReDim rvalues(n)
  Dim i As Long
  For i = 0 To n - 1
    rtimes(i) = times(i)
    rvalues(i) = values(i)
  Next i
End Sub

Public Function SummaryValue() As Double
  SummaryValue = -1#
  If tvaluect > 0 And tvalue >= 0 Then SummaryValue = tvalue
End Function
