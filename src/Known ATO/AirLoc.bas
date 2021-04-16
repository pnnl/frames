Attribute VB_Name = "DataTypes"
Option Explicit
Option Compare Text
  
Type series
  uunit As String
  cunit As String
  cnt As Long
  valu() As Double
End Type

Type progstruct
  cas As String
  ref As Long
  use As Long
  gas As Long              'gas checkbox
  p1 As Long               'particle 1 checkbox
  p2 As Long               'particle 2 checkbox
  p3 As Long               'particle 3 checkbox
  air As Long              'air concentration checkbox
  ext As Long              'external dose checkbox
  dep As Long              'deposition checkbox
  wet As Boolean           'wet radio button
  dry As Boolean           'dry radio button
  tot As Boolean           'total radio button
  cnt As Long
  out(15) As series
End Type

Type constituent
  cas As String
  ref As Long
  use As Long
  gas As Long              'gas checkbox
  p1 As Long               'particle 1 checkbox
  p2 As Long               'particle 2 checkbox
  p3 As Long               'particle 3 checkbox
  air As Long              'air concentration checkbox
  ext As Long              'external dose checkbox
  dep As Long              'deposition checkbox
  wet As Boolean           'wet radio button
  dry As Boolean           'dry radio button
  tot As Boolean           'total radio button
  cnt As Long
  out(15) As series
  numprog As Long          'number of progeny
  prog() As progstruct     'progeny array
End Type

Type location
  name As String           'model name
  lbl As String
  x As Double
  y As Double
  z As Double
  use As Long
  numparent As Long
  con() As constituent
End Type

Type progeny
  name As String            'progeny name
  cas As String             'progeny id
  clk As Long
  clc As Long
'  ref As Long
  uunit As String
  cunit As String
  tsdcnt As Long
  tsd() As Double
End Type

Type parent
  name As String            'constituent name
  cas As String             'constituent id
  clk As Long
  clc As Long
  numprog As Long           'number of progeny
  prog() As progeny         'progeny array
  uunit As String
  cunit As String
  tsdcnt As Long
  tsd() As Double
End Type

Type progenys
  cas As String
  uunit As String
  cunit As String
  tsdcnt As Long
  tsd() As Double
End Type

Type parents
  cas As String             'constituent id
  numprog As Long           'number of progeny
  prog() As progenys        'progeny array
  uunit As String
  cunit As String
  tsdcnt As Long
  tsd() As Double
End Type

Type expstruct
  expname As String         'model name
  expsrc As String          'model source
  explbl As String
  x As Double
  y As Double
  z As Double
  use As Boolean
End Type

Global sitename As String
Global mode As String
Global loadng As Boolean
Global prevtime As Long
Global numexp As Long         'number of exposure
Global exploc() As expstruct
Global numcon As Long
Global con() As parent        'number of parents
Global numloc As Long
Global loc() As location
Global c1 As Long
Global c2 As Long
Global c3 As Long
Global d1 As Long
Global d2 As Long
Global d3 As Long
Global settype As Boolean

Sub WriteOutputs(fle As parmfile, parm As parmrec, con As constituent, i As Long, j As Long, k As Long)
Dim m As Long
Dim n As Long

  set_parm parm, "outcnt", i, j, k, 0, 0, 0, con.ref, "N/A", "N/A", CStr(con.cnt)
  write_parmrec fle, parm
  set_parm parm, "gas", i, j, k, 0, 0, 0, 0, "N/A", "N/A", CStr(con.gas)
  write_parmrec fle, parm
  set_parm parm, "particle1", i, j, k, 0, 0, 0, 0, "N/A", "N/A", CStr(con.p1)
  write_parmrec fle, parm
  set_parm parm, "particle2", i, j, k, 0, 0, 0, 0, "N/A", "N/A", CStr(con.p2)
  write_parmrec fle, parm
  set_parm parm, "particle3", i, j, k, 0, 0, 0, 0, "N/A", "N/A", CStr(con.p3)
  write_parmrec fle, parm
  set_parm parm, "air", i, j, k, 0, 0, 0, 0, "N/A", "N/A", CStr(con.air)
  write_parmrec fle, parm
  set_parm parm, "ext", i, j, k, 0, 0, 0, 0, "N/A", "N/A", CStr(con.ext)
  write_parmrec fle, parm
  set_parm parm, "dep", i, j, k, 0, 0, 0, 0, "N/A", "N/A", CStr(con.dep)
  write_parmrec fle, parm
  set_parm parm, "depwet", i, j, k, 0, 0, 0, 0, "N/A", "N/A", CStr(con.wet)
  write_parmrec fle, parm
  set_parm parm, "depdry", i, j, k, 0, 0, 0, 0, "N/A", "N/A", CStr(con.dry)
  write_parmrec fle, parm
  set_parm parm, "deptot", i, j, k, 0, 0, 0, 0, "N/A", "N/A", CStr(con.tot)
  write_parmrec fle, parm
  For m = 1 To 15
    For n = 1 To con.out(m).cnt
      set_parm parm, "values", i, j, k, m, n, 0, 0, con.out(m).uunit, con.out(m).cunit, convert(con.out(m).uunit, con.out(m).cunit, con.out(m).valu(n))
      write_parmrec fle, parm
    Next
  Next
End Sub

Sub WriteProgOutputs(fle As parmfile, parm As parmrec, con As progstruct, i As Long, j As Long, k As Long)
Dim m As Long
Dim n As Long

  set_parm parm, "outcnt", i, j, k, 0, 0, 0, con.ref, "N/A", "N/A", CStr(con.cnt)
  write_parmrec fle, parm
  set_parm parm, "gas", i, j, k, 0, 0, 0, 0, "N/A", "N/A", CStr(con.gas)
  write_parmrec fle, parm
  set_parm parm, "particle1", i, j, k, 0, 0, 0, 0, "N/A", "N/A", CStr(con.p1)
  write_parmrec fle, parm
  set_parm parm, "particle2", i, j, k, 0, 0, 0, 0, "N/A", "N/A", CStr(con.p2)
  write_parmrec fle, parm
  set_parm parm, "particle3", i, j, k, 0, 0, 0, 0, "N/A", "N/A", CStr(con.p3)
  write_parmrec fle, parm
  set_parm parm, "air", i, j, k, 0, 0, 0, 0, "N/A", "N/A", CStr(con.air)
  write_parmrec fle, parm
  set_parm parm, "ext", i, j, k, 0, 0, 0, 0, "N/A", "N/A", CStr(con.ext)
  write_parmrec fle, parm
  set_parm parm, "dep", i, j, k, 0, 0, 0, 0, "N/A", "N/A", CStr(con.dep)
  write_parmrec fle, parm
  set_parm parm, "depwet", i, j, k, 0, 0, 0, 0, "N/A", "N/A", CStr(con.wet)
  write_parmrec fle, parm
  set_parm parm, "depdry", i, j, k, 0, 0, 0, 0, "N/A", "N/A", CStr(con.dry)
  write_parmrec fle, parm
  set_parm parm, "deptot", i, j, k, 0, 0, 0, 0, "N/A", "N/A", CStr(con.tot)
  write_parmrec fle, parm
  For m = 1 To 15
    For n = 1 To con.out(m).cnt
      set_parm parm, "values", i, j, k, m, n, 0, 0, con.out(m).uunit, con.out(m).cunit, convert(con.out(m).uunit, con.out(m).cunit, con.out(m).valu(n))
      write_parmrec fle, parm
    Next
  Next
End Sub

Sub SetOutputs(con As constituent)
Dim j As Long
  
  con.ref = 0
  con.gas = 0
  con.p1 = 1
  con.p2 = 0
  con.p3 = 0
  con.air = 1
  con.ext = 0
  con.dep = 1
  con.wet = False
  con.dry = False
  con.tot = True
  If con.numprog > 0 Then
    For j = 1 To con.numprog
      If con.prog(j).use > 0 Then
        con.prog(j).gas = 0
        con.prog(j).p1 = 1
        con.prog(j).p2 = 0
        con.prog(j).p3 = 0
        con.prog(j).air = 1
        con.prog(j).ext = 0
        con.prog(j).dep = 1
        con.prog(j).wet = False
        con.prog(j).dry = False
        con.prog(j).tot = True
      End If
    Next
  End If
End Sub

Sub SetProgOutputs(prog As progstruct)
  prog.ref = 0
  prog.gas = 0
  prog.p1 = 1
  prog.p2 = 0
  prog.p3 = 0
  prog.air = 1
  prog.ext = 0
  prog.dep = 1
  prog.wet = False
  prog.dry = False
  prog.tot = True
End Sub

Sub SetTimeUnits(uunit As String, cunit As String)
  If settype Then
    uunit = "hr"
    cunit = "hr"
  Else
    uunit = "yr"
    cunit = "yr"
  End If
End Sub

Sub SetUnits(out() As series, kind As Long)
Dim i As Long
  If kind = 1 Then
    out(1).cunit = "Sv"                                      'ext dose column
    If out(1).uunit = "" Then out(1).uunit = "Sv"
    For i = 2 To 11                                        'dep columns
      out(i).cunit = "Bq/m^2/yr"
      If out(i).uunit = "" Or out(i).uunit = "Bq/m^2" Then out(i).uunit = "Bq/m^2/yr"
    Next
    For i = 12 To 15                                       'conc columns
      out(i).cunit = "Bq/m^3"
      If out(i).uunit = "" Then out(i).uunit = "Bq/m^3"
    Next
  Else
    out(1).cunit = ""                                        'ext dose column
    If out(1).uunit = "" Then out(1).uunit = ""
    For i = 2 To 11                                        'dep columns
      out(i).cunit = "kg/m^2/yr"
      If out(i).uunit = "" Or out(i).uunit = "kg/m^2" Then out(i).uunit = "kg/m^2/yr"
    Next
    For i = 12 To 15                                       'conc columns
      out(i).cunit = "kg/m^3"
      If out(i).uunit = "" Then out(i).uunit = "kg/m^3"
    Next
  End If
End Sub
