Attribute VB_Name = "DataTypes"
Option Explicit
Option Compare Text

Global parm As parmrec
Global fle As parmfile

Public Const wID = 1
Public Const wName = 2
Public Const wType = 3
Public Const wQual = 4

Global numlife As Long
Global life() As String
Global numloc As Long
Global loc() As String
Global numchem As Long
Global chem() As String


Global loccnt As Integer
Global scfcnt As Integer
Global ecocnt As Integer
Global twicnt As Integer

Global loelcheck As Boolean
Global noelcheck As Boolean
Global loedcheck As Boolean
Global noedcheck As Boolean
Global usercheck As Boolean


'Terrestrial TRV index
Global ssl As New MyCollection
Global cct As New MyCollection

'Aquatic TRV index
Global lc As New MyCollection
Global ld As New MyCollection
Global ec As New MyCollection
Global ed As New MyCollection
Global part As New MyCollection
Global ldpart As New MyCollection
Global ndpart As New MyCollection
Global effect As New MyCollection
Global ldeffect As New MyCollection
Global ndeffect As New MyCollection

Global LifeForm As New Collection
Global Organism As New Collection

Public Declare Function GetPrivateProfileString _
                                      Lib "kernel32" Alias "GetPrivateProfileStringA" _
                                       (ByVal lpApplicationName As String, _
                                        ByVal lpKeyName As Any, _
                                        ByVal lpDefault As String, _
                                        ByVal lpReturnedString As String, _
                                        ByVal nSize As Long, _
                                        ByVal lpFileName As String) As Long

Function ReadIniLong(AppName As String, KeyName As String, Default As Long, value As Long) As Boolean
Dim i As Long
Dim pszVal As String

  value = Default
  ReadIniLong = False
  
  pszVal = String(255, 0)
  i = GetPrivateProfileString(AppName, KeyName, "", pszVal, Len(pszVal), App.Path + "\\WeapUI.ini")
  If i > 0 Then
    ReadIniLong = True
    value = Val((Left$(pszVal, i)))
  End If
End Function

Function ReadIniString(AppName As String, KeyName As String, Default As String, value As String) As Boolean
Dim i As Long
Dim pszVal As String

  value = Default
  ReadIniString = False
  
  pszVal = String(255, 0)
  i = GetPrivateProfileString(AppName, KeyName, "", pszVal, Len(pszVal), App.Path + "\\WeapUI.ini")
  If i > 0 Then
    ReadIniString = True
    value = Left$(pszVal, i)
  End If
End Function

Public Function GetTypedIdIndex(id As String, kind As String) As Long
Dim i As Long
Dim j As Long
  
  j = 0
  GetTypedIdIndex = 0
  For i = 1 To numloc
    If loc(wType, i) = kind Then
      j = j + 1
      If id = loc(wID, i) Then
        GetTypedIdIndex = j
        Exit Function
      End If
    End If
  Next
End Function

Sub SetSpreadCombo(ss As fpSpread, col As Integer, Row As Integer, value() As String, num As Long, kind As String)
  Dim lst As String
  Dim i As Long
  Dim j As Long
  lst = "None"
  ss.Row = Row
  ss.col = col
  ss.Action = 26                     'clear the box
  ss.CellType = 8
  j = 1
  For i = 1 To num
    If value(wType, i) = kind Then
      lst = lst + Chr(9) + "Location" + CStr(j) + " (" + value(wID, i) + ")"
      j = j + 1
    End If
  Next
  ss.TypeComboBoxList = lst
  ss.TypeComboBoxCurSel = 0
End Sub

