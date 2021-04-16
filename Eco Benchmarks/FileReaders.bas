Attribute VB_Name = "FileReaders"
Option Explicit
Option Compare Text

Type GIDRec
    name As String
    idxs(6) As Long
    ref As String
    usrUnit As String
    defUnit As String
    valu As String
End Type

Global parms(8) As String

Private Const MAX_PATH As Long = 260

' Variables for login.frm
Global LoginSucceeded As Boolean
Global UsrName As String
Global pssword As String

' Variables for addkey.frm
Global AddKeySucceeded As Boolean
Global KeyId As String
Global KeyName As String

Global numvars As Integer
Global numtbls As Integer
Global vars As New Collection

Public Function ClearVariables() As Boolean
Dim i As Integer

  For i = vars.count To 1 Step -1
    vars.Item(i).ClearCollections
  Next
End Function

Public Function ReadDeclaration(infile As csv) As String
Dim i As Integer
Dim j As Integer
Dim numIdxs As Integer
Dim tempvar As New Variable

  ReadDeclaration = ""
  'read the varaiable name
  tempvar.name = get_val(infile)
  
  'read the dimensions needed to access a single value
  numIdxs = Val(get_val(infile))
  
  If tempvar.name = "~Table" Then
  '~Table varaiable has been declared
    numtbls = numtbls + 1
    tempvar.name = tempvar.name + CStr(numtbls)
    tempvar.filled = False
    tempvar.datatype = ""
    tempvar.min = ""
    tempvar.max = ""
    tempvar.unit = ""
    tempvar.label = ""
    'add table attributes
    For i = 1 To numIdxs
      tempvar.indices.Add get_val(infile), CStr(i)
    Next
    For i = 1 To tempvar.indices.count
      For j = 1 To tempvar.indices.count
        If Not i = j Then vars.Item(tempvar.indices.Item(i)).siblings.Add tempvar.indices.Item(j)
      Next
    Next
  Else
  'Varaiable has been declared
    numvars = numvars + 1
    tempvar.filled = False
    tempvar.datatype = get_val(infile)
    tempvar.min = get_val(infile)
    tempvar.max = get_val(infile)
    tempvar.unit = get_val(infile)
    tempvar.label = get_val(infile)
    'add varaiable indices
    For i = 1 To numIdxs
      tempvar.indices.Add get_val(infile), CStr(i)
    Next
  End If
  'read rest of the line
  get_line infile
  vars.Add tempvar, tempvar.name
  ReadDeclaration = tempvar.name
End Function

Public Function ReadDictionary(fileName As String) As Boolean
Dim i As Integer
Dim dic As csv
Dim numlines As Integer
Dim tempname As String
  
  ReadDictionary = False
  On Error GoTo READERROR
  If Not open_csv(dic, fileName, 2) Then Exit Function
  numvars = 0
  numtbls = 0
  numlines = Val(get_val(dic))
  get_line dic                                      'read the rest of line
  get_line dic                                      'read column header line
  For i = 3 To numlines
    If ReadDeclaration(dic) = "" Then GoTo READERROR
  Next
  ReadDictionary = True
READERROR:
End Function

Public Function FindGidSection(infile As csv, secname As String) As Integer
Dim i As Integer
Dim pname As String
Dim numrec As Integer
Dim buffer As String
      
  While secname <> pname And Not EOCF(infile)
    pname = get_val(infile)
    numrec = Val(get_val(infile))
    If pname <> secname Then
      If infile.leng = 0 Or infile.getbuff = "," Then
        For i = 1 To numrec
          Line Input #infile.fnum, buffer
        Next
      End If
    End If
    get_line infile
  Wend
  If secname = pname Then
    FindGidSection = numrec
  Else
    FindGidSection = 0
  End If
End Function

Public Function ReadModLabel(gidfilename As String, idx1 As Long, idx2 As Long, name As String) As String
Dim i As Integer
Dim linecount As Integer
Dim UsrName As String
Dim gidcheck As Boolean
Dim infile As csv
Dim tempRec As GIDRec
  
  ReadModLabel = ""
  gidcheck = open_csv(infile, gidfilename, 2)
  If Not gidcheck Then Exit Function
  
  linecount = FindGidSection(infile, "FUI")
  If linecount = 0 Then Exit Function
  
  On Error GoTo READERROR
  For i = 1 To linecount
    gidcheck = ReadGIDRec(infile, tempRec)
    If gidcheck And tempRec.idxs(0) = idx1 And tempRec.idxs(1) = idx2 And Left(tempRec.name, 3) = Left(name, 3) Then
      Select Case Right(tempRec.name, Len(tempRec.name) - 3)
        Case "Label":       UsrName = tempRec.valu
        Case "DesPath":     DesName = tempRec.valu
      End Select
    End If
  Next
  close_csv infile
  ReadModLabel = UsrName + " (" + ModName + ")"
READERROR:
End Function

Public Function ReadGidChemicals(gidfilename As String, box1 As ListBox, box2 As ListBox) As Boolean
Dim i As Integer
Dim count As Integer
Dim infile As csv
Dim tempRec As GIDRec
  
  ReadGidChemicals = False
  If Not open_csv(infile, gidfilename, F_READ) Then Exit Function
  count = FindGidSection(infile, "FUI")
  If count = 0 Then Exit Function
  For i = 1 To count
    If ReadGIDRec(infile, tempRec) Then
      If tempRec.idxs(0) = SiteIdx Then
        Select Case tempRec.name
        Case "FSCASID"
          If tempRec.idxs(2) = 0 Then box1.AddItem tempRec.valu, tempRec.idxs(1) - 1
        Case "FSCNAME"
          If tempRec.idxs(2) = 0 Then box2.AddItem tempRec.valu, tempRec.idxs(1) - 1
        Case "CLKTYPE"
          If tempRec.idxs(2) = 0 Then
            If tempRec.valu = 1 Then
              box1.RemoveItem tempRec.idxs(1) - 1
              box2.RemoveItem tempRec.idxs(1) - 1
            End If
          End If
        End Select
      End If
    Else
      'place error handling code here
    End If
  Next
  close_csv infile
  ReadGidChemicals = True
End Function

Public Sub WriteGIDRec(fle As csv, name As String, _
                       i1 As Integer, i2 As Integer, i3 As Integer, _
                       i4 As Integer, i5 As Integer, i6 As Integer, _
                       ref As Integer, usrUnit As String, defUnit As String, valu As String)
    put_val fle, name
    put_val fle, i1
    put_val fle, i2
    put_val fle, i3
    put_val fle, i4
    put_val fle, i5
    put_val fle, i6
    put_val fle, ref
    put_val fle, usrUnit
    put_val fle, defUnit
    put_val fle, valu
    put_line fle
End Sub

Public Function ReadGIDRec(infile As csv, tempRec As GIDRec) As Boolean
Dim i As Integer
  
On Error GoTo READERROR
  ReadGIDRec = False
  tempRec.name = get_val(infile)
  For i = 0 To 5
    tempRec.idxs(i) = CLng(get_val(infile))
  Next
  tempRec.ref = get_val(infile)
  tempRec.usrUnit = get_val(infile)
  tempRec.defUnit = get_val(infile)
  tempRec.valu = get_val(infile)
  get_line infile
  ReadGIDRec = True
READERROR:
End Function

Public Function ReadGID(gidfilename As String, secname As String) As Boolean
Dim count As Integer
Dim infile As csv
Dim i As Integer
Dim j As Integer
Dim k As Integer
Dim key As String
Dim tempRec As GIDRec
Dim var As Variable
Dim akaChem() As Long
Dim newChemIdx() As Long
  
  ReadGID = False
  If open_csv(infile, gidfilename, 2) Then
    count = FindGidSection(infile, secname)
    ReDim newChemIdx(1) As Long
    ClearVariables
    For i = 1 To count
      If ReadGIDRec(infile, tempRec) Then
        Select Case tempRec.name
        Case "EcoCasAlias":       'backwards compatiablity, renamed to fit with dic
          tempRec.name = "akaChemId"
        Case "EcoChemAlias":      'backwards compatiablity, renamed to fit with dic
          tempRec.name = "akaChemName"
        Case "ChemId"
          If UBound(newChemIdx) < tempRec.idxs(0) Then ReDim Preserve newChemIdx(tempRec.idxs(0)) As Long
          newChemIdx(tempRec.idxs(0)) = SearchListBox(tempRec.valu, EcoEdit.ChemSelect(0)) + 1
        End Select
        
        On Error Resume Next
        Err.Clear
        Set var = vars.Item(tempRec.name)
        If Err.Number = 0 Then
         'create key
          key = ""
          'get chemid index position
          k = SearchCollection(var.indices, "ChemId")
          If k = -1 Then k = SearchCollection(var.indices, "numChems")
          For j = 0 To 5
            'adjust for chemical index changes, those missing = -1
            'this is only ok because we write and read the ids first
            If (j + 1) = k Then
              If newChemIdx(tempRec.idxs(j)) = -1 Then Exit For
              tempRec.idxs(j) = newChemIdx(tempRec.idxs(j))
            End If
            key = key + CStr(tempRec.idxs(j)) + ","
          Next
          'add variable values
          If j = 6 Then
            var.filled = True
            var.values.Add tempRec.valu, key
            var.keys.Add key, key
            var.usrUnit.Add tempRec.usrUnit, key
            var.defUnit.Add tempRec.defUnit, key
            var.references.Add tempRec.ref, key
          End If
        Else
          Select Case tempRec.name
            Case "EcoCasId":         'backwards compatiablity, duplicate to chemid
            Case "EcoChemName":      'backwards compatiablity, duplicate to chemname
            Case Else
              MsgBox "Variable not found: " & tempRec.name
              Err.Clear
          End Select
        End If
        On Error GoTo 0
      End If
    Next
    close_csv infile
    ReadGID = True
  End If
End Function

Public Function GetNToken(n As Integer, original As String) As String
    Dim Token As String
    Dim tempchar As String
    Dim tempstring As String
    Dim i As Integer
    Dim j As Integer
    Dim quotecnt As Integer
    Dim del As String
    del = Chr(13)
    Token = ""
    tempstring = original
    j = 1
    For i = 1 To n
        Token = ""
        tempchar = Mid(tempstring, j, 1)
        While (Not tempchar = Chr(13)) And (Not tempchar = Chr(10)) And (j < Len(tempstring))
            Token = Token + tempchar
            j = j + 1
            tempchar = Mid(tempstring, j, 1)
        Wend
        j = j + 2
    Next
    GetNToken = Token
End Function

Public Function SearchNoDashBox(value As String, box As ComboBox) As Long
Dim i As Long
  
  SearchNoDashBox = -1
  For i = 1 To box.ListCount
    If Replace(box.List(i - 1), "-", "") = value Then
      SearchNoDashBox = i - 1
      Exit Function
    End If
  Next
End Function

Public Function SearchComboBox(value As String, box As ComboBox) As Long
Dim i As Long
  
  SearchComboBox = -1
  For i = 1 To box.ListCount
    If box.List(i - 1) = value Then
      SearchComboBox = i - 1
      Exit Function
    End If
  Next
End Function

Public Function SearchListBox(value As String, box As ListBox) As Long
Dim i As Long
  
  SearchListBox = -1
  For i = 1 To box.ListCount
    If box.List(i - 1) = value Then
      SearchListBox = i - 1
      Exit Function
    End If
  Next
End Function

Public Function SearchCollection(things As Collection, value As String) As Integer
Dim i As Integer
  
  SearchCollection = -1
  For i = 1 To things.count
    If things.Item(i) = value Then
      SearchCollection = i
      Exit Function
    End If
  Next
End Function

Public Function SearchForKey(things As Collection, key As String) As Boolean
Dim thing As Variant
  
On Error GoTo KEYNOTFOUND
  SearchForKey = False
  thing = things.Item(key)
  SearchForKey = True
KEYNOTFOUND:
End Function
  
Public Function SearchforNumKey(things As Collection) As String
Dim i As Integer
  
  SearchforNumKey = ""
  For i = 1 To things.count
    If InStr(1, things.Item(i), "num") <> 0 Then
      SearchforNumKey = things.Item(i)
      Exit Function
    End If
  Next
End Function
