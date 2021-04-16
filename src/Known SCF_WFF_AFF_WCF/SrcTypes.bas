Attribute VB_Name = "DataTypes"
Option Explicit
Option Compare Text

Global haveflux As Boolean
Global moist As String          'the moisture type of the output
Global dtset As String          'the data set type
Global outname As String        'name of the output
Global mVal As Boolean

Type fluxrec
  time As Double
  flux(4) As Double
End Type

Type progflux
  ref As Long
  name As String
  cas As String
  use As Boolean
  irIdx As Long
  unit(5) As String
  nts As Long
  series() As fluxrec
End Type

Type parentflux
  ref As Long
  cas As String
  name As String
  use As Boolean
  irIdx As Long
  unit(5) As String
  nts As Long
  series() As fluxrec
  numprog As Long
  prog() As progflux
End Type
 
Type progeny
  name As String
  cas As String
  kind As Long
  grp As Long
End Type

Type parent
  name As String
  cas As String
  kind As Long
  grp As Long
  numprog As Long
  prog() As progeny
End Type

Type location
  media As String
  dataset As String
  locname As String 'for backward compatibilty
  loctype As String 'for backward compatibilty
  use As Boolean
  idx As Long
  numcon As Long
  con() As parentflux
  pnt(6) As String
  unt(6) As String
  val(6) As String
  ref(6) As Long
  x As Double
  Y As Double
  Z As Double
End Type

Type snk
  lbl As String
  name As String
  type As String
  qual As String
  x As Double
  Y As Double
  Z As Double
End Type

Global numsnk As Long
Global loc() As snk
Global numsink As Long
Global sink() As location
Global f_numcon As Long
Global con() As parent
Global irGrp() As Double

Sub Make_WFF()
Dim i As Long, j As Long, k As Long
Dim l As Long, m As Long, n As Long
Dim connum As Long
Dim msg As String
Dim cnt As Long
Dim file As csv
Dim vMult As Variant
Dim irMult As Double

  cnt = 0
  For i = 1 To numsink
    If sink(i).use And Left(sink(i).dataset, 3) = "wff" Then
      cnt = cnt + 1
    End If
  Next
  If cnt = 0 Then Exit Sub
  
  If open_csv(file, RunName & ".wff", 1) Then
    PutHeader file
    put_val file, cnt
    put_line file
    For i = 1 To numsink
      If sink(i).use And Left(sink(i).dataset, 3) = "wff" Then
        put_val file, "all"
        put_val file, sink(i).media
        For j = 1 To 4
          put_val file, convert(sink(i).unt(j), sink(i).pnt(j), val(sink(i).val(j)))
          put_val file, sink(i).pnt(j)
        Next
        put_val file, f_numcon
        put_line file
        put_val file, "yr"
        put_val file, "m^3/yr"
        put_val file, sink(i).con(0).nts
        put_line file
        For j = 1 To sink(i).con(0).nts
          put_val file, convert(sink(i).con(0).unit(1), "yr", sink(i).con(0).series(j).time)
          put_val file, convert(sink(i).con(0).unit(2), "m^3/yr", sink(i).con(0).series(j).flux(1))
          put_line file
        Next
        For j = 1 To f_numcon
          For k = 1 To sink(i).numcon
            If sink(i).con(k).cas = con(j).cas Then
              If sink(i).con(k).nts > 0 Then
                put_val file, con(j).name
                put_sval file, con(j).cas
                put_val file, "yr"
                put_val file, sink(i).con(k).unit(0)
                put_val file, sink(i).con(k).nts
                put_val file, IIf(sink(i).dataset = "wff:Surface Water", 2, 1)
                If Known.prog.Checked Then
                  put_val file, sink(i).con(k).numprog
                Else
                  put_val file, 0
                End If
                put_line file
                vMult = 1
                If sink(i).con(k).irIdx > 0 Then Known.vaSpread3.GetText 2, sink(i).con(k).irIdx, vMult
                irMult = 1#
                If mVal Then irMult = val(Known.txt(8).Text) * val(vMult)
                For l = 1 To sink(i).con(k).nts
                  put_val file, convert(sink(i).con(k).unit(1), "yr", sink(i).con(k).series(l).time)
                  put_val file, convert(sink(i).con(k).unit(2), sink(i).con(k).unit(0), sink(i).con(k).series(l).flux(1) * irMult)
                  If sink(i).dataset = "wff:Surface Water" Then
                    put_val file, convert(sink(i).con(k).unit(3), sink(i).con(k).unit(0), sink(i).con(k).series(l).flux(2) * irMult)
                  End If
                  put_line file
                Next
              Else
                put_val file, con(j).name
                put_sval file, con(j).cas
                put_val file, "yr"
                put_val file, sink(i).con(k).unit(0)
                put_val file, 2
                put_val file, IIf(sink(i).dataset = "wff:Surface Water", 2, 1)
                If Known.prog.Checked Then
                  put_val file, sink(i).con(k).numprog
                Else
                  put_val file, 0
                End If
                put_line file
                put_val file, 0#
                put_val file, 0#
                If sink(i).dataset = "wff:Surface Water" Then put_val file, 0#
                put_line file
                put_val file, 1#
                put_val file, 0#
                If sink(i).dataset = "wff:Surface Water" Then put_val file, 0#
                put_line file
              End If
              If Known.prog.Checked Then
                For l = 1 To con(j).numprog
                  For m = 1 To sink(i).con(k).numprog
                    If sink(i).con(k).prog(m).cas = con(j).prog(l).cas Then
                      If sink(i).con(k).prog(m).nts > 0 Then
                        put_val file, con(j).prog(l).name
                        put_sval file, con(j).prog(l).cas
                        put_val file, "yr"
                        put_val file, sink(i).con(k).prog(m).unit(0)
                        put_val file, sink(i).con(k).prog(m).nts
                        put_val file, IIf(sink(i).dataset = "wff:Surface Water", 2, 1)
                        put_val file, con(j).name
                        put_val file, con(j).cas
                        put_line file
                        vMult = 1
                        If sink(i).con(k).irIdx > 0 Then Known.vaSpread3.GetText 2, sink(i).con(k).prog(m).irIdx, vMult
                        irMult = 1#
                        If mVal Then irMult = val(Known.txt(8).Text) * val(vMult)
                        For n = 1 To sink(i).con(k).prog(m).nts
                          put_val file, convert(sink(i).con(k).prog(m).unit(1), "yr", sink(i).con(k).prog(m).series(n).time)
                          put_val file, convert(sink(i).con(k).prog(m).unit(2), sink(i).con(k).prog(m).unit(0), sink(i).con(k).prog(m).series(n).flux(1) * irMult)
                          If sink(i).dataset = "wff:Surface Water" Then
                            put_val file, convert(sink(i).con(k).prog(m).unit(3), sink(i).con(k).prog(m).unit(0), sink(i).con(k).prog(m).series(n).flux(2) * irMult)
                          End If
                          put_line file
                        Next
                      Else
                        put_val file, con(j).prog(l).name
                        put_sval file, con(j).prog(l).cas
                        put_val file, "yr"
                        put_val file, sink(i).con(k).prog(m).unit(0)
                        put_val file, 2
                        put_val file, IIf(sink(i).dataset = "wff:Surface Water", 2, 1)
                        put_val file, con(j).name
                        put_val file, con(j).cas
                        put_line file
                        put_val file, 0#
                        put_val file, 0#
                        If sink(i).dataset = "wff:Surface Water" Then put_val file, 0#
                        put_line file
                        put_val file, 1#
                        put_val file, 0#
                        If sink(i).dataset = "wff:Surface Water" Then put_val file, 0#
                        put_line file
                      End If
                    End If
                  Next
                Next
              End If
            End If
          Next
        Next
      End If
    Next
    close_csv file
  Else
    PutError "Unable to create water flux file" & RunName & ".wff"
  End If
End Sub

Sub Make_AFF()
Dim i As Long, j As Long, k As Long
Dim l As Long, m As Long, n As Long
Dim connum As Long
Dim Ok(3) As Boolean
Dim file As csv
Dim vMult As Variant
Dim irMult As Double
  
  'sets the index to the aff dataset
  For i = 1 To numsink
    If sink(i).use And Left(sink(i).dataset, 3) = "aff" Then Exit For
  Next
  If i > numsink Then Exit Sub
  
  If open_csv(file, RunName & ".aff", 1) Then
    PutHeader file
    put_val file, 1
    put_line file
    put_val file, "all"
    put_line file
    put_val file, sink(i).media
    put_line file
    For j = 1 To 6
      If sink(i).media = "Area" And j > 1 And j < 5 Then
        put_val file, 0
      Else
        put_val file, convert(sink(i).unt(j), sink(i).pnt(j), val(sink(i).val(j)))
      End If
      put_val file, sink(i).pnt(j)
      put_line file
    Next
    k = 0
    For j = 0 To 3
      If FluxTypes.SSCheck1(j).Enabled And FluxTypes.SSCheck1(j).Value Then
        k = k + 1
        Ok(j) = True
      End If
    Next
    put_val file, k
    put_line file
    k = 0
    For j = 0 To 3
      If Ok(j) Then
        put_val file, FluxTypes.SSCheck1(j).Tag
        put_val file, convert(FluxTypes.unit(k + 1).Text, FluxTypes.unit(k + 1).Tag, val(FluxTypes.txt(k + 1).Text))
        put_val file, FluxTypes.unit(k + 1).Tag
        put_val file, convert(FluxTypes.unit(k).Text, FluxTypes.unit(k).Tag, val(FluxTypes.txt(k).Text))
        put_val file, FluxTypes.unit(k).Tag
        file.putbuff = file.putbuff + ","
        put_line file
      End If
      k = k + 2
    Next
    put_val file, f_numcon
    put_line file
    
    For j = 1 To f_numcon
      For k = 1 To sink(i).numcon
        If sink(i).con(k).cas = con(j).cas Then
          If sink(i).con(k).nts > 0 Then
            put_val file, con(j).name
            put_sval file, con(j).cas
            put_val file, "yr"
            put_val file, sink(i).con(k).unit(0)
            put_val file, sink(i).con(k).nts
            If Known.prog.Checked Then
              put_val file, sink(i).con(k).numprog
            Else
              put_val file, 0
            End If
            put_line file
            vMult = 1
            If sink(i).con(k).irIdx > 0 Then Known.vaSpread3.GetText 2, sink(i).con(k).irIdx, vMult
            irMult = 1#
            If mVal Then irMult = val(Known.txt(8).Text) * val(vMult)
            For l = 1 To sink(i).con(k).nts
              put_val file, convert(sink(i).con(k).unit(1), "yr", sink(i).con(k).series(l).time)
              If Ok(0) Then put_val file, convert(sink(i).con(k).unit(2), sink(i).con(k).unit(0), sink(i).con(k).series(l).flux(1) * irMult)
              If Ok(1) Then put_val file, convert(sink(i).con(k).unit(3), sink(i).con(k).unit(0), sink(i).con(k).series(l).flux(2) * irMult)
              If Ok(2) Then put_val file, convert(sink(i).con(k).unit(4), sink(i).con(k).unit(0), sink(i).con(k).series(l).flux(3) * irMult)
              If Ok(3) Then put_val file, convert(sink(i).con(k).unit(5), sink(i).con(k).unit(0), sink(i).con(k).series(l).flux(4) * irMult)
              put_line file
            Next
          Else
            put_val file, con(j).name
            put_sval file, con(j).cas
            put_val file, "yr"
            put_val file, sink(i).con(k).unit(0)
            put_val file, 2
            If Known.prog.Checked Then
              put_val file, sink(i).con(k).numprog
            Else
              put_val file, 0
            End If
            put_line file
            put_val file, 0#
            If Ok(0) Then put_val file, 0#
            If Ok(1) Then put_val file, 0#
            If Ok(2) Then put_val file, 0#
            If Ok(3) Then put_val file, 0#
            put_line file
            put_val file, 1#
            If Ok(0) Then put_val file, 0#
            If Ok(1) Then put_val file, 0#
            If Ok(2) Then put_val file, 0#
            If Ok(3) Then put_val file, 0#
            put_line file
          End If
          If Known.prog.Checked Then
            For l = 1 To con(j).numprog
              For m = 1 To sink(i).con(k).numprog
                If sink(i).con(k).prog(m).cas = con(j).prog(l).cas Then
                  If sink(i).con(k).prog(m).nts > 0 Then
                    put_val file, con(j).prog(l).name
                    put_sval file, con(j).prog(l).cas
                    put_val file, "yr"
                    put_val file, sink(i).con(k).prog(m).unit(0)
                    put_val file, sink(i).con(k).prog(m).nts
                    put_val file, con(j).name
                    put_val file, con(j).cas
                    put_line file
                    vMult = 1
                    If sink(i).con(k).irIdx > 0 Then Known.vaSpread3.GetText 2, sink(i).con(k).prog(m).irIdx, vMult
                    irMult = 1#
                    If mVal Then irMult = val(Known.txt(8).Text) * val(vMult)
                    For n = 1 To sink(i).con(k).prog(m).nts
                      put_val file, convert(sink(i).con(k).prog(m).unit(1), "yr", sink(i).con(k).prog(m).series(n).time)
                      If Ok(0) Then put_val file, convert(sink(i).con(k).prog(m).unit(2), sink(i).con(k).prog(m).unit(0), sink(i).con(k).prog(m).series(n).flux(1) * irMult)
                      If Ok(1) Then put_val file, convert(sink(i).con(k).prog(m).unit(3), sink(i).con(k).prog(m).unit(0), sink(i).con(k).prog(m).series(n).flux(2) * irMult)
                      If Ok(2) Then put_val file, convert(sink(i).con(k).prog(m).unit(4), sink(i).con(k).prog(m).unit(0), sink(i).con(k).prog(m).series(n).flux(3) * irMult)
                      If Ok(3) Then put_val file, convert(sink(i).con(k).prog(m).unit(5), sink(i).con(k).prog(m).unit(0), sink(i).con(k).prog(m).series(n).flux(4) * irMult)
                      put_line file
                    Next
                  Else
                    put_val file, con(j).prog(l).name
                    put_sval file, con(j).prog(l).cas
                    put_val file, "yr"
                    put_val file, sink(i).con(k).prog(m).unit(0)
                    put_val file, 2
                    put_val file, con(j).name
                    put_val file, con(j).cas
                    put_line file
                    put_val file, 0#
                    If Ok(0) Then put_val file, 0#
                    If Ok(1) Then put_val file, 0#
                    If Ok(2) Then put_val file, 0#
                    If Ok(3) Then put_val file, 0#
                    put_line file
                    put_val file, 1#
                    If Ok(0) Then put_val file, 0#
                    If Ok(1) Then put_val file, 0#
                    If Ok(2) Then put_val file, 0#
                    If Ok(3) Then put_val file, 0#
                    put_line file
                  End If
                End If
              Next
            Next
          End If
        End If
      Next
    Next
    close_csv file
  Else
    PutError "Unable to create air flux file" & RunName & ".aff"
  End If
End Sub

Sub Make_CF(typ As String)
Dim i As Long, j As Long, k As Long
Dim l As Long, m As Long, n As Long
Dim connum As Long
Dim file As csv
Dim cnt As Long
Dim vMult As Variant
Dim irMult As Double
  
  cnt = 0
  For i = 1 To numsink
    If sink(i).use And Left(sink(i).dataset, 3) = typ Then
      cnt = cnt + 1
    End If
  Next
  If cnt = 0 Then Exit Sub
  
  If open_csv(file, RunName & "." & typ, 1) Then
    PutHeader file
    put_val file, cnt
    put_line file
    For i = 1 To numsink
      If sink(i).use And Left(sink(i).dataset, 3) = typ Then
        put_val file, "all"
        put_val file, sink(i).media
        
        If typ = "scf" Then
          For j = 1 To 3
            put_val file, convert(sink(i).unt(j), sink(i).pnt(j), val(sink(i).val(j)))
            put_val file, sink(i).pnt(j)
          Next
        End If
        
        connum = 0
        For j = 1 To sink(i).numcon
          If sink(i).con(j).nts > 0 Then connum = connum + 1
        Next
        put_val file, connum
        
        'place xy and z here
        
        put_val file, loc(0).x * 1000
        put_val file, "m"
        put_val file, loc(0).Y * 1000
        put_val file, "m"
        put_val file, loc(0).Z * 1000
        put_val file, "m"
        put_line file
        
        For j = 1 To f_numcon
          For k = 1 To sink(i).numcon
            If sink(i).con(k).cas = con(j).cas Then
              If sink(i).con(k).nts > 0 Then
                put_val file, con(j).name
                put_sval file, con(j).cas
                put_val file, "yr"
                put_val file, sink(i).con(k).unit(0)
                put_val file, sink(i).con(k).nts
                If Known.prog.Checked Then
                  connum = 0
                  For l = 1 To sink(i).con(k).numprog
                    If sink(i).con(k).prog(l).nts > 0 Then connum = connum + 1
                  Next
                  put_val file, connum
                Else
                  put_val file, 0
                End If
                put_line file
                vMult = 1
                If sink(i).con(k).irIdx > 0 Then Known.vaSpread3.GetText 2, sink(i).con(k).irIdx, vMult
                irMult = 1#
                If mVal Then irMult = val(Known.txt(8).Text) * val(vMult)
                For l = 1 To sink(i).con(k).nts
                  put_val file, convert(sink(i).con(k).unit(1), "yr", sink(i).con(k).series(l).time)
                  put_val file, convert(sink(i).con(k).unit(2), sink(i).con(k).unit(0), sink(i).con(k).series(l).flux(1) * irMult)
                  put_line file
                Next
                If Known.prog.Checked Then
                  For l = 1 To con(j).numprog
                    For m = 1 To sink(i).con(k).numprog
                      If sink(i).con(k).prog(m).cas = con(j).prog(l).cas Then
                        If sink(i).con(k).prog(m).nts > 0 Then
                          put_val file, con(j).prog(l).name
                          put_sval file, con(j).prog(l).cas
                          put_val file, "yr"
                          put_val file, sink(i).con(k).prog(m).unit(0)
                          put_val file, sink(i).con(k).prog(m).nts
                          put_val file, con(j).name
                          put_val file, con(j).cas
                          put_line file
                          vMult = 1
                          If sink(i).con(k).irIdx > 0 Then Known.vaSpread3.GetText 2, sink(i).con(k).prog(m).irIdx, vMult
                          irMult = 1#
                          If mVal Then irMult = val(Known.txt(8).Text) * val(vMult)
                          For n = 1 To sink(i).con(k).prog(m).nts
                            put_val file, convert(sink(i).con(k).prog(m).unit(1), "yr", sink(i).con(k).prog(m).series(n).time)
                            put_val file, convert(sink(i).con(k).prog(m).unit(2), sink(i).con(k).prog(m).unit(0), sink(i).con(k).prog(m).series(n).flux(1) * irMult)
                            put_line file
                          Next
                        End If
                      End If
                    Next
                  Next
                End If
              End If
            End If
          Next
        Next
      End If
    Next
    close_csv file
  Else
    PutError "Unable to create concentration file" & RunName & "." & typ
  End If
End Sub
