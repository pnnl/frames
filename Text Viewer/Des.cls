VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "DesCls"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
Option Compare Text

Const VERSTRING = "MODULE VERSION"
Const DESCSTRING = "MODULE DESCRIPTION"
Const LIMITSTRING = "MODULE REFERENCES"
Const CONNECTSTRING = "VALID CONNECTIONS"
Const INPUTCONNSTRING = "Valid Input Reads"
Const OUTPUTCONNSTRING = "Valid Output Writes"
Const SYSSTRING = "SYSTEM REQUIREMENTS"
Const OSSTRING = "Operating System:     "
Const PROCSTRING = "Processor:            "
Const RAMSTRING = "RAM Memory:           "
Const DISKSTRING = "Disk Space:           "
Const POCSTRING = "POINT OF CONTACT"
Const COMPSTRING = "Company Name:         "
Const MODLRSTRING = "Contact Name:         "
Const ADDSTRING = "Mailing Address:      "
Const CITYSTRING = "City:                 "
Const CNTRYSTRING = "Country:              "
Const STSTRING = "State:                "
Const ZIPSTRING = "Zip Code:             "
Const PHONESTRING = "Telephone Number:     "
Const FAXSTRING = "Fax Number:           "
Const EMAILSTRING = "Email Address:        "
Const URLSTRING = "URL Address:          "

Private versindex As Long
Private descindex As Long
Private limitindex As Long
Private osindex As Long
Private procindex As Long
Private ramindex As Long
Private diskindex As Long
Private sysindex As Long
Private compindex As Long
Private modlrindex As Long
Private addindex As Long
Private cityindex As Long
Private stindex As Long
Private zipindex As Long
Private phoneindex As Long
Private faxindex As Long
Private emailindex As Long
Private urlindex As Long
Private pocindex As Long
Private cntryindex As Long
Private connectindex As Long
Private inputindex As Long
Private outputindex As Long

Private ModNm As String
Private vers As String
Private Desc As String
Private Limit As String
Private Proc As String
Private RAM As String
Private Disk As String
Private Comp As String
Private Modlr As String
Private Street As String
Private City As String
Private State As String
Private Zip As String
Private Country As String
Private Phone As String
Private Fax As String
Private EMail As String
Private url As String
Private Deskey As String
Private loaded As Boolean

Private mvarVariables As Collection
'local variable(s) to hold property value(s)
Private mvarreads As Collection 'local copy
Private mvarwrites As Collection 'local copy
Private mvarDesPath As String

Public Property Get Key() As String
  Key = DesPath
End Property

Public Property Let Key(ByVal vData As String)
  DesPath = vData
End Property

Public Property Get Variables() As Collection
  Set Variables = mvarVariables
End Property

Private Property Set Variables(ByVal vData As Collection)
  Set mvarVariables = vData
End Property

Public Property Get DesPath() As String
  DesPath = mvarDesPath
End Property

Public Property Let DesPath(ByVal vData As String)
  mvarDesPath = vData
End Property

Public Property Set writes(ByVal vData As Collection)
  Set mvarwrites = vData
End Property

Public Property Get writes() As Collection
  Set writes = mvarwrites
End Property

Public Property Set reads(ByVal vData As Collection)
  Set mvarreads = vData
End Property

Public Property Get reads() As Collection
  Set reads = mvarreads
End Property

Public Function LoadDES() As Boolean
  Dim filein As csv
  Dim temp As String
  Dim desccheck As String
  Dim i As Long
  Dim errormsg As String
  
    loaded = False
    If DesPath = "" Or "" = Dir$(DesPath) Then Exit Function
  
    LoadDES = False
    If open_csv(filein, DesPath, F_READ) = False Then
        errormsg = "Unable to open file " + DesPath
        MsgBox errormsg, vbOKOnly + vbSystemModal + vbExclamation, "Error"
        Exit Function
    End If
    
    temp = get_val(filein)
    If Not temp = "mf" Then
        errormsg = "Unable to read file" + Chr(13) + Chr(10)
        errormsg = errormsg + "non-compatible file type"
        MsgBox errormsg, vbOKOnly + vbSystemModal + vbExclamation, "Error"
        Exit Function
    End If
    temp = get_val(filein)  ' version
    get_line filein
    temp = get_val(filein)  ' model type info not needed for print
    ModNm = get_val(filein) ' models name given by the developer
                            ' ui and model executables and icon info ignored
    get_line filein
    temp = get_val(filein)
    If temp = "" Then
        close_csv filein
        Exit Function
    End If
    If Asc(temp) < 48 Or Asc(temp) > 57 Then
        close_csv filein
        desccheck = GetDescInfo(filein)
        If open_csv(filein, DesPath, F_READ) = False Then
            errormsg = "Unable to open file " + DesPath
            MsgBox errormsg, vbOKOnly + vbSystemModal + vbExclamation, "Error"
            Screen.MousePointer = vbDefault
            Exit Function
        End If
        temp = get_val(filein)
        temp = get_val(filein)
        get_line filein
        temp = get_val(filein)
        temp = get_val(filein)
        temp = get_val(filein)
        temp = get_val(filein)
        get_line filein
        temp = get_val(filein)
    End If
    GetReads filein, get_val(filein)
    GetWrites filein
    ReadVars filein
  ' ReadGis
    close_csv filein
    Screen.MousePointer = vbDefault
    LoadDES = True
    
    loaded = True
OpenCancel:

End Function

Public Function GetDescString(fname As String) As String
Dim tempbuff As String
Dim f5 As Long
  
  f5 = FreeFile
  Open fname For Input As #f5
  Line Input #f5, tempbuff
  Line Input #f5, tempbuff
  tempbuff = Input(1, #f5)
  GetDescString = tempbuff
  tempbuff = Input(1, #f5)
  GetDescString = GetDescString + tempbuff
  While Not tempbuff = Chr(34)
    tempbuff = Input(1, #f5)
    If tempbuff = Chr(13) Then
      tempbuff = Input(1, #f5)
      tempbuff = Chr(13) + Chr(10)
    End If
    GetDescString = GetDescString + tempbuff
  Wend
  GetDescString = GetDescString + Chr(34)
  Close #f5
End Function

Private Function GetDescInfo(filein As csv) As String
    Dim temp As String
    Dim phonetemp As String
    Dim tempstate As String
    Dim tempint As Long
    Dim faxtemp As String
     close_csv filein
     temp = GetDescString(filein.fname)
     GetIndices temp
     vers = GetDataString(temp, VERSTRING, False)
     Desc = GetDataString(temp, DESCSTRING, True)
     Limit = GetDataString(temp, LIMITSTRING, True)
     Proc = GetDataString(temp, PROCSTRING, False)
     RAM = GetDataString(temp, RAMSTRING, False)
     Disk = GetDataString(temp, DISKSTRING, False)
     Comp = GetDataString(temp, COMPSTRING, False)
     Modlr = GetDataString(temp, MODLRSTRING, False)
     Street = GetDataString(temp, ADDSTRING, False)
     City = GetDataString(temp, CITYSTRING, False)
     State = GetDataString(temp, STSTRING, False)
     Zip = GetDataString(temp, ZIPSTRING, False)
     Country = GetDataString(temp, CNTRYSTRING, False)
     Phone = GetDataString(temp, PHONESTRING, False)
     Fax = GetDataString(temp, FAXSTRING, False)
     EMail = GetDataString(temp, EMAILSTRING, False)
     url = GetDataString(temp, URLSTRING, False)
     If (vers = "") And (Desc = "") And (Limit = "") _
      And (Proc = "") And (RAM = "") And (Disk = "") _
      And (Comp = "") And (Modlr = "") And (Street = "") _
      And (City = "") And (State = "") And (Zip = "") _
      And (Country = "") And (Phone = "") And (Fax = "") _
      And (EMail = "") And (url = "") Then
        Desc = temp
     End If
     GetDescInfo = ""
End Function

Private Sub GetReads(filein As csv, checkstring As String)
  Dim i As Long, j As Long, readcnt As Long, filecnt As Long
  Dim col As Collection
  Dim ftype As FileType
  
    Set reads = New Collection
    
    If checkstring = "" Then
        get_line filein
        readcnt = Val(get_val(filein))
    Else
        readcnt = Val(checkstring)
    End If
    get_line filein
    
    For j = 1 To readcnt
      Set col = New Collection
      filecnt = get_val(filein)
      For i = 1 To filecnt
        Set ftype = New FileType
        ftype.Init get_val(filein), get_val(filein), get_val(filein), get_val(filein)
        col.Add ftype, CStr(i)
      Next
      reads.Add col, CStr(j)
      get_line filein
    Next
End Sub

Private Sub GetWrites(filein As csv)
  Dim i As Long, writecnt As Long
  Dim ftype As FileType
  
    Set writes = New Collection
  
    writecnt = Val(get_val(filein))
    get_line filein
    For i = 1 To writecnt
      Set ftype = New FileType
      ftype.Init get_val(filein), get_val(filein), "0", "0"
      get_line filein
      writes.Add ftype, CStr(i)
    Next
End Sub

Private Sub ReadVars(filein As csv)
  Dim lct As Long, vCt As Long, m As Long
  Dim tmpvar As New vars
  Dim errorindex As Long
  Dim linecnt As Long
  Dim tmpdes As New DesVars
  Dim temp2 As String
  Dim errormsg As String
  Dim temp As String
  Dim lastvar As vars
  
  
  Set tmpvar.Labels = New Collection
  Set Variables = New Collection
    
    linecnt = 1
    temp = get_val(filein)
    If Not temp = "" Then
        If Val(temp) = 0 Then
            temp = get_val(filein)
            get_line filein
            linecnt = linecnt + 1
            Exit Sub
        Else
          linecnt = Val(temp)
        End If
    End If
'   temp = get_val(filein)
    get_line filein
'   linecnt = linecnt + 1
'   While Not EOCF(filein)
    While lct < linecnt ' Not EOCF(filein)
      lct = lct + 1
        temp = get_val(filein)
        temp2 = get_val(filein)
        Set tmpvar = New vars
        If Not (temp = "Label" Or temp = "Variable") And InStr(1, temp2, "cue", 1) = 0 Then
            errorindex = 0
            tmpvar.Name = temp
            tmpvar.Stoch = temp2
            tmpvar.Units = get_val(filein)
            temp = get_val(filein)
            If temp = "Min" Then
                tmpvar.Min = get_val(filein)
                temp = get_val(filein)
                If temp = "Max" Then
                    tmpvar.Max = get_val(filein)
                Else
                    tmpvar.Max = ""
                End If
            Else
                tmpvar.Min = ""
                temp = get_val(filein)
                If temp = "Max" Then
                    tmpvar.Max = get_val(filein)
                Else
                    tmpvar.Max = ""
                End If
            End If
            tmpvar.Desc = get_val(filein)
            temp = get_val(filein)
            On Error GoTo ERROR_HANDLER
            Variables.Add tmpvar, tmpvar.Name
            Set lastvar = tmpvar
            vCt = Val(temp)
            For m = 1 To vCt
              get_line filein
              Set tmpdes = New DesVars
'             tmpdes.des =
              tmpdes.varFlag = get_val(filein)
              tmpdes.Name = get_val(filein)
              tmpdes.Index1 = get_val(filein)
              tmpdes.Index2 = get_val(filein)
              tmpdes.Index3 = get_val(filein)
              tmpdes.Index4 = get_val(filein)
              tmpdes.Index5 = get_val(filein)
              tmpdes.Index6 = get_val(filein)
              Variables.item(tmpvar.Name).Labels.Add tmpdes, CStr(m) ' tmpdes.name ' + lastparen.Key
            Next m
'           get_line filein
        End If
      get_line filein
    Wend
    Exit Sub
ERROR_HANDLER:
    If errorindex = 0 Then
        errormsg = "Unable to add variable " + tmpvar.Name
        errormsg = errormsg + Chr(13) + Chr(10) + "Variable is a duplicate"
        errormsg = errormsg + Chr(13) + Chr(10) + "Line #"
        errormsg = errormsg + CStr(linecnt)
        errormsg = errormsg + " in the variable section of the description file"
        MsgBox errormsg, vbExclamation + vbOKOnly + vbSystemModal, "Error"
    Else
        errormsg = "Unable to add variable cue " + tmpdes.Name
        errormsg = errormsg + Chr(13) + Chr(10) + "Variable cue is a duplicate"
        errormsg = errormsg + Chr(13) + Chr(10) + "Line #"
        errormsg = errormsg + CStr(linecnt)
        errormsg = errormsg + " in the variable section of the description file"
        MsgBox errormsg, vbExclamation + vbOKOnly + vbSystemModal, "Error"
    End If
    Resume Next
End Sub

Public Sub GetCueIndices(Index As Long, tempdes As DesVars, cuecnt As Long, tempvar As vars)
    If Index > 0 Then
        Set tempdes = New DesVars
        cuecnt = cuecnt + 1
        tempdes.Name = "cue " + str(cuecnt)
        tempdes.varFlag = "Label"
        tempvar.Labels.Add tempdes, tempdes.Name + tempvar.Name
    End If
End Sub

Public Sub CheckCueNumber(tempdes As DesVars, tempvar As vars)
  Dim tmpdes As New DesVars, j As Long
    If tempvar.Labels.Count > Variables.item(tempvar.Name).Labels.Count Then
        For j = Variables.item(tempvar.Name).Labels.Count + 1 To tempvar.Labels.Count
            Set tmpdes = New DesVars
            tmpdes.Name = "cue " + str(j)
            Variables.item(tempvar.Name).Labels.Add tempdes, tempdes.Name + tempvar.Name
        Next
    End If
End Sub

Public Sub GetIndices(tempstring As String)
    Dim header As String
    versindex = InStr(1, tempstring, VERSTRING, vbTextCompare)
    descindex = InStr(1, tempstring, DESCSTRING, vbTextCompare)
    limitindex = InStr(1, tempstring, LIMITSTRING, vbTextCompare)
    sysindex = InStr(1, tempstring, SYSSTRING, vbTextCompare)
    osindex = InStr(1, tempstring, OSSTRING, vbTextCompare)
    procindex = InStr(1, tempstring, PROCSTRING, vbTextCompare)
    ramindex = InStr(1, tempstring, RAMSTRING, vbTextCompare)
    diskindex = InStr(1, tempstring, DISKSTRING, vbTextCompare)
    pocindex = InStr(1, tempstring, POCSTRING, vbTextCompare)
    compindex = InStr(1, tempstring, COMPSTRING, vbTextCompare)
    modlrindex = InStr(1, tempstring, MODLRSTRING, vbTextCompare)
    addindex = InStr(1, tempstring, ADDSTRING, vbTextCompare)
    cityindex = InStr(1, tempstring, CITYSTRING, vbTextCompare)
    stindex = InStr(1, tempstring, STSTRING, vbTextCompare)
    cntryindex = InStr(1, tempstring, CNTRYSTRING, vbTextCompare)
    zipindex = InStr(1, tempstring, ZIPSTRING, vbTextCompare)
    phoneindex = InStr(1, tempstring, PHONESTRING, vbTextCompare)
    faxindex = InStr(1, tempstring, FAXSTRING, vbTextCompare)
    emailindex = InStr(1, tempstring, EMAILSTRING, vbTextCompare)
    urlindex = InStr(1, tempstring, URLSTRING, vbTextCompare)
    connectindex = InStr(1, tempstring, CONNECTSTRING, vbTextCompare)
    inputindex = InStr(1, tempstring, INPUTCONNSTRING, vbTextCompare)
    outputindex = InStr(1, tempstring, OUTPUTCONNSTRING, vbTextCompare)
End Sub

Public Function GetHeadLen(header As String) As Long
    Select Case header
        Case VERSTRING
            GetHeadLen = Len(VERSTRING)
        Case DESCSTRING
            GetHeadLen = Len(DESCSTRING)
        Case LIMITSTRING
            GetHeadLen = Len(LIMITSTRING)
        Case SYSSTRING
            GetHeadLen = Len(SYSSTRING)
        Case OSSTRING
            GetHeadLen = Len(OSSTRING)
        Case PROCSTRING
            GetHeadLen = Len(PROCSTRING)
        Case RAMSTRING
            GetHeadLen = Len(RAMSTRING)
        Case DISKSTRING
            GetHeadLen = Len(DISKSTRING)
        Case POCSTRING
            GetHeadLen = Len(POCSTRING)
        Case COMPSTRING
            GetHeadLen = Len(COMPSTRING)
        Case MODLRSTRING
            GetHeadLen = Len(MODLRSTRING)
        Case ADDSTRING
            GetHeadLen = Len(ADDSTRING)
        Case CITYSTRING
            GetHeadLen = Len(CITYSTRING)
        Case STSTRING
            GetHeadLen = Len(STSTRING)
        Case ZIPSTRING
            GetHeadLen = Len(ZIPSTRING)
        Case PHONESTRING
            GetHeadLen = Len(PHONESTRING)
        Case FAXSTRING
            GetHeadLen = Len(FAXSTRING)
        Case EMAILSTRING
            GetHeadLen = Len(EMAILSTRING)
        Case URLSTRING
            GetHeadLen = Len(URLSTRING)
        Case CNTRYSTRING
            GetHeadLen = Len(CNTRYSTRING)
        Case CONNECTSTRING
            GetHeadLen = Len(CONNECTSTRING)
        Case INPUTCONNSTRING
            GetHeadLen = Len(INPUTCONNSTRING)
        Case OUTPUTCONNSTRING
            GetHeadLen = Len(OUTPUTCONNSTRING)
        Case Else
            GetHeadLen = 0
    End Select
End Function

Public Function GetNxtIndex(previndex As Long, header As String, inputstring As String) As Long
    Dim start As Long
    start = previndex + GetHeadLen(header)
    If Not InStr(start, inputstring, VERSTRING, 0) = 0 Then
        GetNxtIndex = InStr(start, inputstring, VERSTRING, 0)
        Exit Function
    End If
    If Not InStr(start, inputstring, DESCSTRING, 0) = 0 Then
        GetNxtIndex = InStr(start, inputstring, DESCSTRING, 0)
        Exit Function
    End If
    If Not InStr(start, inputstring, LIMITSTRING, 0) = 0 Then
        GetNxtIndex = InStr(start, inputstring, LIMITSTRING, 0)
        Exit Function
    End If
    If Not InStr(start, inputstring, CONNECTSTRING, 0) = 0 Then
        GetNxtIndex = InStr(start, inputstring, CONNECTSTRING, 0)
        Exit Function
    End If
    If Not InStr(start, inputstring, INPUTCONNSTRING, 0) = 0 Then
        GetNxtIndex = InStr(start, inputstring, INPUTCONNSTRING, 0)
        Exit Function
    End If
    If Not InStr(start, inputstring, OUTPUTCONNSTRING, 0) = 0 Then
        GetNxtIndex = InStr(start, inputstring, OUTPUTCONNSTRING, 0)
        Exit Function
    End If
    If Not InStr(start, inputstring, SYSSTRING, 0) = 0 Then
        GetNxtIndex = InStr(start, inputstring, SYSSTRING, 0)
        Exit Function
    End If
    If Not InStr(start, inputstring, OSSTRING, 0) = 0 Then
        GetNxtIndex = InStr(start, inputstring, OSSTRING, 0)
        Exit Function
    End If
    If Not InStr(start, inputstring, PROCSTRING, 0) = 0 Then
        GetNxtIndex = InStr(start, inputstring, PROCSTRING, 0)
        Exit Function
    End If
    If Not InStr(start, inputstring, RAMSTRING, 0) = 0 Then
        GetNxtIndex = InStr(start, inputstring, RAMSTRING, 0)
        Exit Function
    End If
    If Not InStr(start, inputstring, DISKSTRING, 0) = 0 Then
        GetNxtIndex = InStr(start, inputstring, DISKSTRING, 0)
        Exit Function
    End If
    If Not InStr(start, inputstring, POCSTRING, 0) = 0 Then
        GetNxtIndex = InStr(start, inputstring, POCSTRING, 0)
        Exit Function
    End If
    If Not InStr(start, inputstring, COMPSTRING, 0) = 0 Then
        GetNxtIndex = InStr(start, inputstring, COMPSTRING, 0)
        Exit Function
    End If
    If Not InStr(start, inputstring, MODLRSTRING, 0) = 0 Then
        GetNxtIndex = InStr(start, inputstring, MODLRSTRING, 0)
        Exit Function
    End If
    If Not InStr(start, inputstring, ADDSTRING, 0) = 0 Then
        GetNxtIndex = InStr(start, inputstring, ADDSTRING, 0)
        Exit Function
    End If
    If Not InStr(start, inputstring, CITYSTRING, 0) = 0 Then
        GetNxtIndex = InStr(start, inputstring, CITYSTRING, 0)
        Exit Function
    End If
    If Not InStr(start, inputstring, STSTRING, 0) = 0 Then
        GetNxtIndex = InStr(start, inputstring, STSTRING, 0)
        Exit Function
    End If
    If Not InStr(start, inputstring, ZIPSTRING, 0) = 0 Then
        GetNxtIndex = InStr(start, inputstring, ZIPSTRING, 0)
        Exit Function
    End If
    If Not InStr(start, inputstring, CNTRYSTRING, 0) = 0 Then
        GetNxtIndex = InStr(start, inputstring, CNTRYSTRING, 0)
        Exit Function
    End If
    If Not InStr(start, inputstring, PHONESTRING, 0) = 0 Then
        GetNxtIndex = InStr(start, inputstring, PHONESTRING, 0)
        Exit Function
    End If
    If Not InStr(start, inputstring, FAXSTRING, 0) = 0 Then
        GetNxtIndex = InStr(start, inputstring, FAXSTRING, 0)
        Exit Function
    End If
    If Not InStr(start, inputstring, EMAILSTRING, 0) = 0 Then
        GetNxtIndex = InStr(start, inputstring, EMAILSTRING, 0)
        Exit Function
    End If
    If Not InStr(start, inputstring, URLSTRING, 0) = 0 Then
        GetNxtIndex = InStr(start, inputstring, URLSTRING, 0)
        Exit Function
    End If
    GetNxtIndex = Len(inputstring) + 1
End Function

Public Function GetDataString(inputstring As String, header As String, desccheck As Boolean) As String
    Dim s1 As Long
    Dim e1 As Long
    Dim s2 As Long
    Dim tabcnt As Long
    Dim tempstring As String
    Dim i As Long
    tempstring = ""
    s1 = GetHeadIndex(header)
    If s1 = 0 Then
        GetDataString = ""
        Exit Function
    End If
    e1 = s1 + GetHeadLen(header)
    s2 = GetNxtIndex(s1, header, inputstring)
    If header = VERSTRING Then
        s2 = s2 - 4
    ElseIf header = DESCSTRING Then
        s2 = s2 - 4
    ElseIf header = LIMITSTRING Then
        's2 = s2 - 4
        s2 = s2 - 2
    ElseIf header = CONNECTSTRING Then
        s2 = s2 - 4
    ElseIf header = INPUTCONNSTRING Then
        s2 = s2 - 4
    ElseIf header = OUTPUTCONNSTRING Then
        s2 = s2 - 4
    End If
    If desccheck Then
        For i = e1 + 2 To s2 - 1
            tempstring = tempstring + Mid(inputstring, i, 1)
        Next
    Else
        For i = e1 To s2 - 1
            If Not (Mid(inputstring, i, 1) = Chr(13) Or Mid(inputstring, i, 1) = Chr(10) Or Mid(inputstring, i, 1) = Chr(34)) Then
                tempstring = tempstring + Mid(inputstring, i, 1)
            End If
        Next
    End If
    GetDataString = tempstring
End Function

Public Function GetHeadIndex(header As String) As Long
    Select Case header
        Case VERSTRING
            GetHeadIndex = versindex
        Case DESCSTRING
            GetHeadIndex = descindex
        Case LIMITSTRING
            GetHeadIndex = limitindex
        Case SYSSTRING
            GetHeadIndex = sysindex
        Case OSSTRING
            GetHeadIndex = osindex
        Case PROCSTRING
            GetHeadIndex = procindex
        Case RAMSTRING
            GetHeadIndex = ramindex
        Case DISKSTRING
            GetHeadIndex = diskindex
        Case POCSTRING
            GetHeadIndex = pocindex
        Case COMPSTRING
            GetHeadIndex = compindex
        Case MODLRSTRING
            GetHeadIndex = modlrindex
        Case ADDSTRING
            GetHeadIndex = addindex
        Case CITYSTRING
            GetHeadIndex = cityindex
        Case STSTRING
            GetHeadIndex = stindex
        Case ZIPSTRING
            GetHeadIndex = zipindex
        Case PHONESTRING
            GetHeadIndex = phoneindex
        Case FAXSTRING
            GetHeadIndex = faxindex
        Case EMAILSTRING
            GetHeadIndex = emailindex
        Case URLSTRING
            GetHeadIndex = urlindex
        Case CNTRYSTRING
            GetHeadIndex = cntryindex
        Case CONNECTSTRING
            GetHeadIndex = connectindex
        Case INPUTCONNSTRING
            GetHeadIndex = inputindex
        Case OUTPUTCONNSTRING
            GetHeadIndex = outputindex
        Case Else
            GetHeadIndex = 0
    End Select
End Function

Public Sub ParseArguments(cmdline As String, executable As String, arguments As String)
    Dim i As Integer
    Dim separator As Integer
    separator = InStr(cmdline, " ")
    If Not separator = 0 Then
        executable = Mid(cmdline, 1, separator - 1)
        arguments = Mid(cmdline, separator + 1, Len(cmdline) - separator)
    Else
        executable = cmdline
        arguments = ""
    End If
End Sub

Public Sub DisplayModuleDescription(rtb As RichTextBox, vfui As FuiCls)
Dim tdesc As String
Dim fout As String
Dim f2 As Long

  If Not loaded Then
    If Not LoadDES() Then Exit Sub
  End If
  
  f2 = FreeFile
  fout = vfui.RunName + ".lst"
  Open fout For Output As #f2

  Print #f2, ""
  Print #f2, String(80, "*")
  Print #f2, "* Model:   " + ModNm
  Print #f2, "* Version:  " + vers
  Print #f2, "* Description:  "
              
  tdesc = Desc
  While 0 < Len(tdesc)
    If 0 = InStr(tdesc, vbCrLf) Then
      Print #f2, "* " & String(5, " ") & tdesc
      tdesc = ""
    Else
      Print #f2, "* " & String(5, " ") & Left(tdesc, InStr(tdesc, vbCrLf) - 1)
      tdesc = Mid$(tdesc, InStr(tdesc, vbCrLf) + 2)
    End If
  Wend
  Print #f2, String(80, "*")
  Close #f2
  rtb.FileName = vfui.RunName & ".lst"
  Kill fout
  Err.Clear
End Sub

Public Function GetDesVariables() As Collection
  If Not loaded Then
    If Not LoadDES() Then
      Set GetDesVariables = Nothing
      Exit Function
    End If
  End If
  Set GetDesVariables = Variables
End Function

Private Sub Class_Initialize()
  Set mvarVariables = Nothing
  Set mvarreads = Nothing
  Set mvarwrites = Nothing
End Sub
