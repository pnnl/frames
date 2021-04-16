VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "LifeForms"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit

Public Doses As Collection
Public Locs As New Location
Public id As String
Public idx As Long

Public Sub WriteGIDEntries()
Dim i As Integer
Dim mydose As dose
Dim myloc As Location
        
  set_parm parm, "species", idx, 0, 0, 0, 0, 0, 0, "N/A", "N/A", id
  write_parmrec fle, parm
  For i = 1 To Doses.Count
    Set mydose = Doses(i)
    If mydose.idx > 0 Then
      If mydose.WriteAcute(idx) Then
        PutError "Invalid acute exposure time for organism " + life(wName, idx) + ", constituent " + chem(wName, mydose.idx)
      End If
      If mydose.WriteCCC(idx) Then
        PutError "Invalid CCC for organism " + life(wName, idx) + ", constituent " + chem(wName, mydose.idx)
      End If
      If mydose.WriteDoseCurve(idx) Then
        PutError "Invalid dose curve sequence for organism " + life(wName, idx) + ", constituent " + chem(wName, mydose.idx)
      End If
      If Not mydose.CurvesOK() Then
        PutError "Dose curve lower or crosses CCC for organism " + life(wName, idx) + ", constituent " + chem(wName, mydose.idx)
      End If
    End If
  Next
  If Locs.WriteLocFreq(idx) Then
    PutError "No location information for organism " + life(wName, idx) + ", constituent " + chem(wName, mydose.idx)
  End If
End Sub

Private Sub Class_Initialize()
  Set Doses = New Collection
End Sub
