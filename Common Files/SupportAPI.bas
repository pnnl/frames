Attribute VB_Name = "SupportAPI"
Option Explicit
Option Compare Text


Public Const MAXFIELD = 4096
Public Type tINDEX
  i1 As Long
  i2 As Long
  i3 As Long
  i4 As Long
  i5 As Long
  i6 As Long
  i7 As Long
  i8 As Long
  i9 As Long
  i10 As Long
  i11 As Long
  i12 As Long
End Type
Public retStr As String * MAXFIELD

Public Function SetIdx(Optional i1 As Long = 0, Optional i2 As Long = 0, _
                        Optional i3 As Long = 0, Optional i4 As Long = 0, _
                        Optional i5 As Long = 0, Optional i6 As Long = 0, _
                        Optional i7 As Long = 0, Optional i8 As Long = 0, _
                        Optional i9 As Long = 0, Optional i10 As Long = 0, _
                        Optional i11 As Long = 0, Optional i12 As Long = 0) As tINDEX
  Dim index As tINDEX
  index.i1 = i1
  index.i2 = i2
  index.i3 = i3
  index.i4 = i4
  index.i5 = i5
  index.i6 = i6
  index.i7 = i7
  index.i8 = i8
  index.i9 = i9
  index.i10 = i10
  index.i11 = i11
  index.i12 = i12
  SetIdx = index
End Function

Public Function SetIdxPos(pos As Long, val As Long, ByRef ndx As tINDEX) As tINDEX
  Select Case pos
    Case 1: ndx.i1 = val
    Case 2: ndx.i2 = val
    Case 3: ndx.i3 = val
    Case 4: ndx.i4 = val
    Case 5: ndx.i5 = val
    Case 6: ndx.i6 = val
    Case 7: ndx.i7 = val
    Case 8: ndx.i8 = val
    Case 9: ndx.i9 = val
    Case 10: ndx.i10 = val
    Case 11: ndx.i11 = val
    Case 12: ndx.i12 = val
  End Select
  SetIdxPos = ndx
End Function

Public Function GetIdxPos(pos As Long, ByRef ndx As tINDEX) As Long
  Dim val As Long
  Select Case pos
    Case 1: val = ndx.i1
    Case 2: val = ndx.i2
    Case 3: val = ndx.i3
    Case 4: val = ndx.i4
    Case 5: val = ndx.i5
    Case 6: val = ndx.i6
    Case 7: val = ndx.i7
    Case 8: val = ndx.i8
    Case 9: val = ndx.i9
    Case 10: val = ndx.i10
    Case 11: val = ndx.i11
    Case 12: val = ndx.i12
  End Select
  GetIdxPos = val
End Function
