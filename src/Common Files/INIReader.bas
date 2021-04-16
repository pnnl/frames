Attribute VB_Name = "INIReader"
Option Compare Text
Declare Function GetPrivateProfileString Lib "kernel32" Alias "GetPrivateProfileStringA" (ByVal lpApplicationName As String, ByVal lpKeyName As Any, ByVal lpDefault As String, ByVal lpReturnedString As String, ByVal nSize As Long, ByVal lpFileName As String) As Long

Global modtypes() As String
Global modnames() As String
Global modids() As String
Global modicons() As String

'File types and qualifiers
Global extensions() As String
Global qualifiers() As String

Public Sub ParseFileInfo(filestring As String, extension As String, qualifier As String)
  Dim i As Integer
  Dim ext As String
  Dim qual As String
  Dim tempstring As String
  Dim tempchar As String
  
  ext = ""
  qual = ""
  For i = 1 To Len(filestring) + 1
    tempchar = Mid(filestring, i, 1)
    If (tempchar = ",") Or (i = Len(filestring) + 1) Then
      If ext = "" Then
        ext = tempstring
      Else
        qual = tempstring
      End If
      tempstring = ""
    Else
      tempstring = tempstring + tempchar
    End If
  Next
  extension = ext
  qualifier = qual
End Sub

Public Sub ParseTypeInfo(delimiter As String, typestring As String, modtype As String, ModName As String, modid As String, modicon As String)
  Dim i As Integer
  Dim types As String
  Dim Name As String
  Dim Id As String
  Dim icon As String
  Dim tempstring As String
  Dim tempchar As String
  types = ""
  Name = ""
  Id = ""
  icon = ""
  For i = 1 To Len(typestring) + 1
    tempchar = Mid(typestring, i, 1)
    If (tempchar = delimiter) Or (i = Len(typestring) + 1) Then
      If types = "" Then
        types = tempstring
      ElseIf Name = "" Then
        Name = tempstring
      ElseIf Id = "" Then
        Id = tempstring
      Else
        icon = tempstring
      End If
      tempstring = ""
    Else
      tempstring = tempstring + tempchar
    End If
  Next
  modtype = types
  ModName = Name
  modid = Id
  modicon = icon
End Sub

Public Function ReadINI() As Boolean
  Dim check As Long
  Dim keyname As String
  Dim tempstring As String
  Dim modcnt As Integer
  Dim filecnt As Integer
  Dim returned As String
  Dim i As Integer
  
  Const gstrNULL = ""
  ReadINI = False
  On Error GoTo READERROR
  
  returned = Space$(256)
  check = GetPrivateProfileString("ModuleTypeInfo", "Count", gstrNULL, returned, 256, FRAMES_INI)
  If check = 0 Then Exit Function
  returned = RTrim$(StripTerminator(returned))
  modcnt = Val(returned)
  ReDim modtypes(modcnt) As String
  ReDim modnames(modcnt) As String
  ReDim modids(modcnt) As String
  ReDim modicons(modcnt) As String
  For i = 1 To modcnt
    tempstring = i
    keyname = "ModInfo" + tempstring
    returned = Space$(256)
    check = GetPrivateProfileString("ModuleTypeInfo", keyname, gstrNULL, returned, 256, FRAMES_INI)
    If check = 0 Then Exit Function
    returned = RTrim$(StripTerminator(returned))
    ParseTypeInfo ",", returned, modtypes(i - 1), modnames(i - 1), modids(i - 1), modicons(i - 1)
  Next
  
  returned = Space$(256)
  check = GetPrivateProfileString("FileTypeInfo", "Count", gstrNULL, returned, 256, FRAMES_INI)
  If check = 0 Then Exit Function
  returned = RTrim$(StripTerminator(returned))
  filecnt = Val(returned)
  ReDim extensions(filecnt) As String
  ReDim qualifiers(filecnt) As String
  For i = 1 To filecnt
    tempstring = i
    keyname = "FileInfo" + tempstring
    returned = Space$(256)
    check = GetPrivateProfileString("FileTypeInfo", keyname, gstrNULL, returned, 256, FRAMES_INI)
    If check = 0 Then Exit Function
    returned = RTrim$(StripTerminator(returned))
    ParseFileInfo returned, extensions(i - 1), qualifiers(i - 1)
  Next
  
  ReadINI = True
READERROR:
End Function
