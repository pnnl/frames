Attribute VB_Name = "EditDes"
Option Explicit
Option Compare Text

Type filetype
  Name As String
  ident As String
  min As Integer
  max As Integer
End Type

Type SectionName
  names() As String
  labels() As String
  cnt As Integer
End Type
Public sverror As Boolean

Global FRAMES_INI As String
 
Global names() As String
Global types() As String
Global ids() As String
Global fileaddcheck As Boolean
Global modaddcheck As Boolean
Global modaddtype As String
Global modaddname As String
Global modaddicon As String
Global tempextension As String
Global tempqualifier As String

Public Function CheckExistingTypes(modtyp As String, box As ComboBox) As Boolean
  Dim i As Integer
  For i = 0 To box.ListCount
    If modtyp = box.list(i) Then
      CheckExistingTypes = False
      Exit Function
    End If
  Next
  CheckExistingTypes = True
End Function

Public Function CheckID(Id As String, tempids() As String) As Boolean
  Dim i As Integer
  CheckID = True
  For i = 0 To UBound(tempids) - 1
    If tempids(i) = Id Then
      CheckID = False
      Exit Function
    End If
  Next
End Function

Public Function GenerateID(Name As String, tempids() As String) As String
  Dim i As Integer
  Dim j As Integer
  Dim tempid As String
  Dim tempchar As String
  tempid = Mid(Name, 1, 3)
  j = 4
  While (Not CheckID(tempid, tempids)) And (j < Len(Name))
    tempid = Mid(Name, 1, 1)
    tempid = tempid + Mid(Name, 2, 1)
    tempid = tempid + Mid(Name, j, 1)
    j = j + 1
  Wend
  GenerateID = tempid
End Function

Public Function ExistsCheck(tempstring As String, stringarray() As String) As Boolean
  Dim i As Integer
  ExistsCheck = False
  For i = 0 To UBound(stringarray) - 1
    If tempstring = stringarray(i) Then
      ExistsCheck = True
      Exit Function
    End If
  Next
End Function

Public Sub AddModelInfo(typebox As ComboBox, namebox As ComboBox)
  Dim Name As String
  Dim i As Integer
  Dim errmsg As String
  Dim Id As String
  
  Name = namebox.Text
  If typebox.Text = "Viewer" Then
    Id = "vwr"
  Else
    Id = GenerateID(Name, ids)
  End If
  ReDim Preserve types(UBound(types) + 1) As String
  types(UBound(types) - 1) = typebox.Text
  If ExistsCheck(namebox.Text, names) Then
    errmsg = "Module Name already exists" + Chr(13) + Chr(10)
    errmsg = errmsg + "Please select an existing name or enter a unique module name"
    MsgBox errmsg, vbExclamation + vbOKOnly, "Error"
  Else
    ReDim Preserve names(UBound(names) + 1) As String
    ReDim Preserve ids(UBound(ids) + 1) As String
    names(UBound(names) - 1) = namebox.Text
    ids(UBound(ids) - 1) = Id
  End If
End Sub
