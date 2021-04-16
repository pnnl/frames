Attribute VB_Name = "ImportBAS"
Option Explicit
Option Compare Text

Public Type param
  pname As String
  dim As Long
  uunit As String
  cunit As String
  pval As String
End Type

Public curGlyph As Glyph

Public Const PASS = 0
Public Const FAIL = -1
'Public SimPath As String        'the path to the simulation files
Public SimSet As String
Public lst As ListBox
Public model As String
Public rtb As RichTextBox

Public colModules As Collection
Public colVars() As param
Public Const ICON_XY = 125 ' 65  ' pixels
Public ActiveScope As Long
Public ModIcon As Collection


Public DomainIconPath As String
Public ClassIconPath As String
Public GroupIconPath As String
Public SubGrpIconPath As String

Public Const delimiter = "££"

Public Function DisplayError(ByVal code As Long, ByVal API_CALL As String, Optional ByVal display As Boolean = False) As Long
Dim errstr As String
Dim errmsg As String

  DisplayError = code
  If code < 0 Then
  '  If code = -1010 Or code = -1001 Then Exit Function
    ReadError PID, code, errstr
'    errstr = Replace(errstr, vbTab, "")
    errmsg = "API Call : " + API_CALL + vbCrLf
    errmsg = errmsg + "Error Code : " + CStr(code) + vbCrLf
    errmsg = errmsg + "Error Description : " + errstr
    Debug.Print errmsg
'    If display Then MsgBox errmsg, vbExclamation + vbOKOnly, "Import DES File Error"
    If display Then AddText errmsg
  Else
  End If
End Function

Public Sub ReduceVariables()
  Dim i As Long, j As Long, idx As Long
  Dim vCt As Long
  Dim found As Boolean
  On Error Resume Next
  
  ReDim colVars(UBound(parmlist))
  
  For i = 1 To UBound(parmlist)
    idx = 0
    If parmlist(i).idx1 > 0 Then idx = 1
    If parmlist(i).idx2 > 0 Then idx = 2
    If parmlist(i).idx3 > 0 Then idx = 3
    If parmlist(i).idx4 > 0 Then idx = 4
    If parmlist(i).idx5 > 0 Then idx = 5
    If parmlist(i).idx6 > 0 Then idx = 6
    found = False
    For j = 1 To vCt
      If colVars(j).pname = parmlist(i).pname Then
        found = True
        Exit For
      End If
    Next
    If Not found Then
      vCt = vCt + 1
      colVars(vCt).pname = parmlist(i).pname
      colVars(vCt).dim = idx
      colVars(vCt).cunit = parmlist(i).cunit
      colVars(vCt).uunit = parmlist(i).uunit
      colVars(vCt).pval = parmlist(i).pval

    Else
      If idx > colVars(j).dim Then colVars(j).dim = idx
    End If
  Next i
  ReDim Preserve colVars(vCt)
End Sub

Public Sub AddText(txt As String, Optional EOL As Boolean = True)
  rtb.Text = rtb.Text & txt & IIf(EOL, vbCrLf, "")
End Sub

Public Sub SetText(txt As String, Optional EOL As Boolean = True)
  rtb.Text = txt & IIf(EOL, vbCrLf, "")
End Sub

