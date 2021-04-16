VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "FuiCls"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
Option Compare Text

'local variable(s) to hold property value(s)

'Private mvarDesName As String        'the path and name of the run
Private mvarFuiName As String        'the path and name of the run
Private mvarRunName As String        'the name of the parameter file
Private mvarsiteIdx As Long          'the index to siteIdx
Private mvarmodIdx As Long           'the index to module
Private mvarModName As String        'the name of the module glyphIdx
Private mvarargv0 As String
Private mvarraw As Boolean

Private mvarcData As Collection 'local copy
Private mvarcDes As Collection 'local copy
Private mvarcMod As Collection 'local copy

Friend Property Let raw(ByVal vData As String)
  mvarraw = vData
End Property

Friend Property Get raw() As String
  raw = mvarraw
End Property

Friend Property Let argv0(ByVal vData As String)
  mvarargv0 = vData
End Property

Friend Property Get argv0() As String
  argv0 = mvarargv0
End Property

Friend Property Let FUIName(ByVal vData As String)
  mvarFuiName = vData
End Property

Friend Property Get FUIName() As String
  FUIName = mvarFuiName
End Property

Friend Property Let RunName(ByVal vData As String)
  mvarRunName = vData
End Property

Friend Property Get RunName() As String
  RunName = mvarRunName
End Property

Friend Property Let siteIdx(ByVal vData As Long)
  mvarsiteIdx = vData
End Property

Friend Property Get siteIdx() As Long
  siteIdx = mvarsiteIdx
End Property

Friend Property Let modIdx(ByVal vData As Long)
  mvarmodIdx = vData
End Property

Friend Property Get modIdx() As Long
  modIdx = mvarmodIdx
End Property

Friend Property Let ModName(ByVal vData As String)
  mvarModName = vData
End Property

Friend Property Get ModName() As String
  ModName = mvarModName
End Property

Private Property Set cmod(ByVal vData As Collection)
  Set mvarcMod = vData
End Property

Public Property Get cmod() As Collection
  Set cmod = mvarcMod
End Property

Public Property Set cdes(ByVal vData As Collection)
  Set mvarcDes = cData
End Property

Public Property Get cdes() As Collection
  Set cdes = mvarcDes
End Property

Public Property Set cData(ByVal vData As Collection)
  Set mvarcData = vData
End Property

Public Property Get cData() As Collection
  Set cData = mvarcData
End Property

Private Sub Class_Initialize()
  Set mvarcData = New Collection
  Set mvarcDes = New Collection
  Set mvarcMod = New Collection
End Sub

Public Sub GetSource(source As String, sourcelabel As String, stype As String, sourcedes As String)
Dim pfilein As parmfile
Dim parm As parmrec
Dim marker As String
Dim fpos As Long, numrec As Long, i As Long, c1 As Integer, c2 As Integer
Dim found As Boolean
Dim rc As Boolean

' source = ""
' sourcelabel = ""
' stype = ""
' sourcedes = ""
  
  rc = open_parm(pfilein, FUIName & ".gid", F_READ)
  
    Do While Not found
      If read_parmrec(pfilein, parm) Then
        numrec = parm.idx1
        fpos = Seek(pfilein.file.fnum)
        If "CSM" = parm.pname Then
          found = True
          Exit Do
        Else
          For i = 1 To numrec
            get_line pfilein.file
          Next
        End If
      End If
    Loop
    
  If "vwr" <> Left(ModName, 3) Then
  
    source = ModName
    stype = argv0
    found = True
    
  Else
    
    If found Then
      Seek #pfilein.file.fnum, fpos
      found = False
      For i = 1 To numrec
        If read_parmrec(pfilein, parm) Then
          If parm.pname = "ModId" And parm.pval = ModName Then
            c1 = parm.idx1
            c2 = parm.idx2
            found = True
            Exit For
          End If
        End If
      Next
    End If
    
    If found Then
      found = False
      source = ""
      stype = ""
      Seek #pfilein.file.fnum, fpos
      For i = 1 To numrec
        If read_parmrec(pfilein, parm) Then
          If parm.pname = "ModSrcId" And c1 = parm.idx1 And c2 = parm.idx2 And 1 = parm.idx3 Then
            source = parm.pval
          End If
          If parm.pname = "ModSrcType" And c1 = parm.idx1 And c2 = parm.idx2 And 1 = parm.idx3 Then
            stype = parm.pval
          End If
  '       If sourcelabel <> "" And found Then Exit For  ???
          If source <> "" And stype <> "" Then
            found = True
            Exit For
          End If
        End If
      Next
    End If
  
  End If
  
  If found Then
    c1 = -1: c2 = -1
    Seek #pfilein.file.fnum, fpos
    found = False
    For i = 1 To numrec
      If read_parmrec(pfilein, parm) Then
        If parm.pname = "ModId" And parm.pval = source Then
          c1 = parm.idx1
          c2 = parm.idx2
          found = True
        End If
        If parm.pname = "ModLabel" And parm.idx1 = c1 And parm.idx2 = c2 Then
          sourcelabel = parm.pval
        End If
        If parm.pname = "ModDesPath" And parm.idx1 = c1 And parm.idx2 = c2 Then
          sourcedes = parm.pval
        End If
        If found And sourcelabel <> "" And sourcedes <> "" Then Exit For
      End If
    Next
  End If
  
  close_parm pfilein
  
  If Not found Then
    MsgBox "Source not found for " & ModName
    End

  End If
End Sub

Public Sub DisplayModuleDescription(ModName As String, rtb As RichTextBox, vfui As FuiCls)
Dim vmod As ModCls
  
  Set vmod = vfui.GetModule(ModName, vfui)
  vmod.DisplayModuleDescription rtb, vfui
End Sub

Public Sub DisplayFileContents(rtb As RichTextBox, vfui As FuiCls, prtref As Boolean)
  Dim i As Long
  Dim f1 As Long
  Dim f2 As Long
  Dim done As Boolean
  Dim vmod As ModCls
  Dim fin As String
  Dim fout As String
  Dim num As Long
  Dim Module As String
  Dim Line As String
  
  On Error Resume Next
  
  fin = vfui.FUIName & "." & vfui.argv0
  fout = vfui.RunName + ".lst"
  done = False
  If vfui.raw = False And "gid" = vfui.argv0 Then
    Set vmod = vfui.GetModule(vfui.ModName, vfui)
    done = vmod.DisplayFileContents(fin, fout, rtb, vfui, prtref)
  Else
    f1 = FreeFile
    Open fin For Input As #f1
    f2 = FreeFile
    Open fout For Output As #f2
    While (Not EOF(f1)) And (Not done)
      Input #f1, Module, num
      If Module = ModName Then done = True
      For i = 1 To num
        Line Input #f1, Line
        If done Then Print #f2, Line
      Next
    Wend
    Close #f1
    Close #f2
  End If
  
  If done Then
    rtb.Text = ""
    rtb.LoadFile fout
    rtb.SelStart = 0
    rtb.SelRTF = String(80, "*") + vbCrLf + _
        "* File:    " + fin + vbCrLf + _
        "* Section: " + ModName + vbCrLf + _
        "* Date:    " + CStr(Format(Date, "General Date")) + " " + CStr(Time()) + vbCrLf + _
        String(80, "*") + vbCrLf + vbCrLf
    Kill fout
  Else
    MsgBox "Unable to display file contents!", vbExclamation
  End If
  Err.Clear
End Sub

Public Sub AddModule(vmod As ModCls)
Dim i As Long, v As ModCls
  For i = 1 To cmod.Count
    Set v = cmod.item(i)
    If v.Key = vmod.Key Then
      Exit Sub
    End If
  Next i
  cmod.Add vmod, vmod.Key
End Sub

Public Sub AddDes(vdes As DesCls)
Dim i As Long, v As DesCls
  For i = 1 To cdes.Count
    Set v = cdes.item(i)
    If v.Key = vdes.Key Then Exit Sub
  Next i
  cdes.Add vdes, vdes.Key
End Sub

Public Function GetDes(vkey As String) As DesCls
Dim vdes As DesCls
  For Each vdes In cdes
    If vdes.Key = vkey Then
      Set GetDes = vdes
      Exit Function
    End If
  Next
  Set vdes = New DesCls
  vdes.Key = vkey
  AddDes vdes
  Set GetDes = vdes
End Function

Public Function GetModule(vkey As String, vfui As FuiCls) As ModCls
Dim vmod As ModCls
  For Each vmod In cmod
    If vmod.Key = vkey Then
      Set GetModule = vmod
      Exit Function
    End If
  Next
  Set vmod = New ModCls
  vmod.Init vfui
  vmod.Key = vkey
  AddModule vmod
  Set GetModule = vmod
End Function

Public Function GetDesVariables(vkey As String) As Collection
  Set GetDesVariables = cdes(vkey).GetDesVariables()
End Function

Public Function GetData(vkey As String) As GidCls
Dim vData As GidCls
  For Each vData In cData
    If vData.Key = vkey Then
      Set GetData = vData
      Exit Function
    End If
  Next
  Set vData = New GidCls
  vData.GetData vkey, siteIdx, FUIName & ".gid"
  cData.Add vData, vkey
  Set GetData = vData
End Function

Friend Function Lookup(ModName As String, pname As String, i1&, i2&, i3&, i4&, i5&, i6&) As String
Dim pval As String
Dim gc1 As GidCls, gc2 As GidCls, gc3 As GidCls

  Set gc1 = GetData("cache")
  If gc1.Lookup(pname, i1, i2, i3, i4, i5, i6, pval) Then
    Lookup = pval
    Exit Function
  End If
  
  Set gc2 = GetData(ModName)
  If gc2.Lookup(pname, i1, i2, i3, i4, i5, i6, pval) Then
    Lookup = pval
    Exit Function
  End If
  
  Set gc3 = GetData("FUI")
  If gc3.Lookup(pname, i1, i2, i3, i4, i5, i6, pval) Then
    gc1.AddParm pname, i1, i2, i3, i4, i5, i6, pval
    Lookup = pval
    Exit Function
  End If
  
  gc1.AddParm pname, i1, i2, i3, i4, i5, i6, ""
  Lookup = ""

End Function

Friend Sub Init(FUI As String, run As String, siteIdx&, modx&, mname As String, v0 As String)
  FUIName = FUI
  RunName = run
  siteIdx = siteIdx
  modIdx = modx
  ModName = mname
  argv0 = v0
End Sub

