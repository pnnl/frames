VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ParmCls"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Private Type prec
  pname As String
  idx1 As Long
  idx2 As Long
  idx3 As Long
  idx4 As Long
  idx5 As Long
  idx6 As Long
  ref As Long
  uunit As String
  cunit As String
  pval As String
End Type

Private parm As prec

Sub setparm(pname As String, i1 As Long, i2 As Long, i3 As Long, i4 As Long, i5 As Long, i6 As Long, ref As Long, uunit As String, cunit As String, pval As String)
  parm.pname = pname
  parm.idx1 = i1
  parm.idx2 = i2
  parm.idx3 = i3
  parm.idx4 = i4
  parm.idx5 = i5
  parm.idx6 = i6
  parm.ref = ref
  parm.uunit = uunit
  parm.cunit = cunit
  parm.pval = pval
End Sub

Sub getparm(pname As String, i1 As Long, i2 As Long, i3 As Long, i4 As Long, i5 As Long, i6 As Long, ref As Long, uunit As String, cunit As String, pval As String)
  pname = parm.pname
  i1 = parm.idx1
  i2 = parm.idx2
  i3 = parm.idx3
  i4 = parm.idx4
  i5 = parm.idx5
  i6 = parm.idx6
  ref = parm.ref
  uunit = parm.uunit
  cunit = parm.cunit
  pval = parm.pval
End Sub
