VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "GidCls"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
Option Compare Text

Private vCt As Long
Private mvarKey As String
Private vData() As parmrec

Public Property Get Key() As String
  Key = mvarKey
End Property

Public Property Let Key(vData As String)
  mvarKey = vData
End Property

Public Function LoadFUIData(SiteIndex As Long, fin As String) As Boolean
Dim m As Long
Dim cnt As Long
Dim parm As parmrec
Dim filein As parmfile

  If vCt = 0 Then
    ReDim vData(vCt)
    vCt = 0
    If open_parm(filein, fin, F_READ) Then
      Do Until EOCF(filein.file)
        If read_parmrec(filein, parm) Then
          Select Case parm.pname
            Case "FUI"
              cnt = parm.idx1
              For m = 1 To cnt
                If read_parmrec(filein, parm) Then
                  If parm.idx1 = SiteIndex Then
                    vCt = vCt + 1
                    If vCt > UBound(vData) Then
                      ReDim Preserve vData(vCt + 25)
                    End If
                    vData(vCt) = parm
                  End If
                End If
              Next m
              ReDim Preserve vData(vCt)
              Exit Do
          Case Else
            For m = 1 To parm.idx1
              get_line filein.file
            Next
          End Select
        End If
      Loop
      close_parm filein
    End If
  End If
  LoadFUIData = (vCt > 0)
End Function

Public Function LoadGIDData(SiteIndex&, ModName As String, fin As String) As Boolean
Dim m As Long
Dim parm As parmrec
Dim filein As parmfile

  LoadGIDData = False

  ReDim vData(0)
  vCt = 0

  If open_parm(filein, fin, F_READ) Then
    Do Until EOCF(filein.file)
      If read_parmrec(filein, parm) Then
        Select Case parm.pname
          Case ModName
            For m = 1 To parm.idx1
              If read_parmrec(filein, parm) Then
'               If parm.idx1 = SiteIndex Then
                  vCt = vCt + 1
                  If vCt > UBound(vData) Then
                    ReDim Preserve vData(vCt + 25)
                  End If
                  vData(vCt) = parm
'               End If
              End If
            Next m
            ReDim Preserve vData(vCt)
            Exit Do
          Case Else
            For m = 1 To parm.idx1
              get_line filein.file
            Next
        End Select
      End If
    Loop
    close_parm filein
  Else
'   put_val errfile, "Can't find or open file " & FUIName
'   put_line errfile
'   close_csv errfile
    close_parm filein
  End If
  LoadGIDData = (0 < vCt)
End Function

Public Function FormatGIDData(fout As String, vars As Collection, vfui As FuiCls, Printref As Boolean) As Boolean
  Dim temp As String
  Dim i As Long
  Dim numrec As Long
  Dim fpos As Long
  Dim f2 As Long
  Dim refabbr As String
  Dim refdes As String
  
  On Error Resume Next
  
  Load Reference
  f2 = FreeFile
  Open fout For Output As #f2
  For i = 1 To vCt
    With vData(i)
      Err.Clear
      Print #f2, vars(.pname).FormatGID(.idx1, .idx2, .idx3, .idx4, .idx5, .idx6, .ref, .uunit, .cunit, .pval, vfui)
      If Err <> 0 Then
        temp = .pname & "  (" & .idx1 & "," & .idx2 & "," & .idx3 & "," & .idx4 & "," & .idx5 & "," & .idx6 & "):  "
        If IsNumeric(.pval) Then
          temp = temp & convert(.cunit, .uunit, CDbl(.pval)) & " " & .uunit
        Else
          temp = temp & .pval
        End If
        Print #f2, temp
      End If
      If Printref And .ref > 0 Then
        Reference.get_ref .ref, refabbr, refdes
        Print #f2, "Reference for " & .pname
        Print #f2, Tab; "Reference ID #" & CStr(.ref)
        Print #f2, Tab; "Reference: " & refabbr
        Print #f2, Tab; "Reference Description: " & refdes
      End If
    End With
  Next i
  Close #f2
  Unload Reference
  
  Err.Clear
  FormatGIDData = True
End Function

Public Function GetData(vkey As String, siteIdx As Long, fin As String) As Object
  Key = vkey
  Select Case vkey:
    Case "FUI":
      LoadFUIData siteIdx, fin
    Case "CSM":
    Case "cache":
    Case Else:
      LoadGIDData siteIdx, vkey, fin
  End Select
End Function

Friend Function Lookup(Name As String, i1&, i2&, i3&, i4&, i5&, i6&, ByRef pval As String) As Boolean
Dim m As Long
  Lookup = False
  For m = 1 To vCt
    If Name = vData(m).pname And _
       i1 = vData(m).idx1 And i2 = vData(m).idx2 And i3 = vData(m).idx3 And _
       i4 = vData(m).idx4 And i5 = vData(m).idx5 And i6 = vData(m).idx6 Then
       pval = vData(m).pval
       Lookup = True
       Exit Function
    End If
  Next
End Function

Public Sub AddParm(Name As String, i1&, i2&, i3&, i4&, i5&, i6&, pval As String)
Dim p1 As parmrec

  p1.pname = Name
  p1.idx1 = i1
  p1.idx2 = i2
  p1.idx3 = i3
  p1.idx4 = i4
  p1.idx5 = i5
  p1.idx6 = i6
  p1.pval = pval
  
  vCt = vCt + 1
  ReDim Preserve vData(vCt)
  vData(vCt) = p1
End Sub

Private Sub Class_Initialize()
  vCt = 0
  ReDim vData(0)
End Sub
