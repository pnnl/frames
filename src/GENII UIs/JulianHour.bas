Attribute VB_Name = "JulianHour"
Option Explicit
Option Compare Text

Public Function CheckLeap(year As Long) As Boolean
  If (year Mod 4 = 0) And ((year Mod 100 <> 0) Or (year Mod 400 = 0)) Then
    CheckLeap = True
  Else
    CheckLeap = False
  End If
End Function

Public Function Date2JulHours(mn As Long, dy As Long, yr As Long, hr As Long) As Long
  Dim Numdays As Long
  Dim feb As Long
  
  If CheckLeap(yr) Then
    feb = 29
  Else
    feb = 28
  End If
  Numdays = dy - 1
  If mn > 1 Then Numdays = Numdays + 31
  If mn > 2 Then Numdays = Numdays + feb
  If mn > 3 Then Numdays = Numdays + 31
  If mn > 4 Then Numdays = Numdays + 30
  If mn > 5 Then Numdays = Numdays + 31
  If mn > 6 Then Numdays = Numdays + 30
  If mn > 7 Then Numdays = Numdays + 31
  If mn > 8 Then Numdays = Numdays + 31
  If mn > 9 Then Numdays = Numdays + 30
  If mn > 10 Then Numdays = Numdays + 31
  If mn > 11 Then Numdays = Numdays + 30
  Date2JulHours = (Numdays * 24) + hr
End Function

Public Function Use(jhour As Long, days As Long, month As Long)
  If days * 24 < jhour Then
    Use = jhour - (days * 24)
    month = month + 1
  Else
    Use = jhour
  End If
End Function

Public Sub JulHours2Date(jhr As Long, yr As Long, mn As Long, dy As Long, hr As Long)
  Dim feb As Long
  
  If CheckLeap(yr) Then
    feb = 29
  Else
    feb = 28
  End If
  
  mn = 1
  hr = jhr
  hr = Use(hr, 31, mn)
  hr = Use(hr, feb, mn)
  hr = Use(hr, 31, mn)
  hr = Use(hr, 30, mn)
  hr = Use(hr, 31, mn)
  hr = Use(hr, 30, mn)
  hr = Use(hr, 31, mn)
  hr = Use(hr, 31, mn)
  hr = Use(hr, 30, mn)
  hr = Use(hr, 31, mn)
  hr = Use(hr, 30, mn)
  dy = 1
  While hr > 24
    dy = dy + 1
    hr = hr - 24
  Wend
End Sub

