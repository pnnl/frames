VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Organisms"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Public TRVs As Collection
Public Id As String
Public idx As Long

Public Sub WriteGIDEntries()
Dim i As Integer
Dim myTRV As TRV
        
  set_parm parm, "uspecies", 0, idx, 0, 0, 0, 0, 0, "N/A", "N/A", Id
  write_parmrec fle, parm
  For i = 1 To TRVs.Count
    Set myTRV = TRVs(i)
    If myTRV.idx > 0 Then
      myTRV.WriteTRVs (idx)
    End If
  Next
End Sub

Private Sub Class_Initialize()
  Set TRVs = New Collection
End Sub

