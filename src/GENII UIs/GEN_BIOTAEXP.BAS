Attribute VB_Name = "DataTypes"
Option Explicit
Option Compare Text

Global fle As parmfile
Global ui As New Collection
Global gn_s As New Selects
Global gn_l() As New Logical
Global gn_t() As New Float
Global gn_i() As New Whole
Global loadng As Boolean
Global rfs As Object
Global ctext3 As String
Global ctext4 As String
Global acute As Boolean
Global f_numcon As Long, con() As contam
Global m_numcon As Long, kd() As kdparm
Global lsize As Long
Global tsize As Long
Global haveglyph As Boolean
Global haveswater As Boolean
Global havegwater As Boolean
Dim temp As parmrec
Dim numloc As Long

Type d_kdparm
  cas As String
  idx As Long
  kd As Double
  lr As Double
  uunt As String
  luunt As String
  ref As Long
End Type

Type kdparm
  cas As String
  idx As Long
  kd As Double
  lr As Double
  uunt As String
  luunt As String
  ref As Long
  numprog As Long
  progeny() As d_kdparm
End Type

Type daughter
  Name As String
  cas As String
  kind As Long
  kd As Double
End Type

Type contam
  Name As String
  cas As String
  kind As Long
  kd As Double
  numprog As Long
  progeny() As daughter
End Type

Type src
  lbl As String
  Name As String
  type As String
  qual As String
End Type

Public loc() As src

Sub updategauge()
  Loading.Gauge1.Value = Loading.Gauge1.Value + 1
  Loading.Label1 = "Loading ... " + Format(Loading.Gauge1.Value / Loading.Gauge1.Max * 100, "###") + "%"
  Loading.Refresh
End Sub

Private Sub addcon()
  If temp.idx2 > f_numcon Then
    f_numcon = temp.idx2
    ReDim Preserve con(f_numcon) As contam
  End If
  If temp.idx3 > con(temp.idx2).numprog Then
    con(temp.idx2).numprog = temp.idx3
    ReDim Preserve con(temp.idx2).progeny(temp.idx3) As daughter
  End If
End Sub

Private Sub addkd()
  If temp.idx1 > m_numcon Then
    m_numcon = temp.idx1
    ReDim Preserve kd(temp.idx1) As kdparm
  End If
  If temp.idx2 > kd(temp.idx1).numprog Then
    kd(temp.idx1).numprog = temp.idx2
    ReDim Preserve kd(temp.idx1).progeny(temp.idx2) As d_kdparm
  End If
End Sub

Private Sub SizeLocal()
  If temp.idx3 > numloc Then
    numloc = temp.idx3
    ReDim Preserve loc(numloc) As src
  End If
End Sub

Sub loadprm()
  Dim i As Long
  Dim j As Long
  Dim k As Long
  Dim m As Long
  Dim mx As Long
  Dim fle As parmfile
  
  For i = 1 To 3
    gn_l(i).Value = False
    gen_exp.SSCheck1_Click gn_l(i).index, False
  Next
  gen_exp.SSTab1(0).TabVisible(1) = False
  If open_parm(fle, FUIName, 2) Then
    Do Until EOCF(fle.file)
      If read_parmrec(fle, temp) Then
        Select Case temp.pName
          Case "fui"
            For m = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                If temp.idx1 = siteIdx Then
                  Select Case temp.pName
                    Case "clkd"
                      addcon
                      If temp.idx3 = 0 Then
                        con(temp.idx2).kd = Val(temp.pval)
                      Else
                        con(temp.idx2).progeny(temp.idx3).kd = Val(temp.pval)
                      End If
                    Case "clktype"
                      addcon
                      If temp.idx3 = 0 Then
                        con(temp.idx2).kind = CInt(temp.pval)
                      Else
                        con(temp.idx2).progeny(temp.idx3).kind = CInt(temp.pval)
                      End If
                    Case "fscname"
                      addcon
                      If temp.idx3 = 0 Then
                        con(temp.idx2).Name = temp.pval
                      Else
                        con(temp.idx2).progeny(temp.idx3).Name = temp.pval
                      End If
                    Case "fscasid"
                      addcon
                      If temp.idx3 = 0 Then
                        con(temp.idx2).cas = temp.pval
                      Else
                        con(temp.idx2).progeny(temp.idx3).cas = temp.pval
                      End If
                  End Select
                End If
              End If
            Next
            
          Case "csm"
            mx = -1
            For i = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                'assumes modname will always occur before sink variables
                If temp.pName = "modid" And temp.pval = modName Then
                  mx = temp.idx2
                End If
                If mx > -1 And temp.idx2 = mx And temp.idx1 = siteIdx Then
                  Select Case temp.pName
'                    Case "moddespath"
'                      DesName = temp.pval
                    Case "modsrcid"
                      SizeLocal
                      loc(temp.idx3).Name = temp.pval
                    Case "modsrclabel"
                      SizeLocal
                      loc(temp.idx3).lbl = temp.pval
                    Case "modsrctype"
                      SizeLocal
                      loc(temp.idx3).type = temp.pval
                    Case "modsrcqual"
                      SizeLocal
                      loc(temp.idx3).qual = temp.pval
                  End Select
                End If
              End If
            Next
            For i = 1 To numloc
              Select Case loc(i).qual
                Case "Surface Water"
                  gen_exp.SSTab1(0).TabVisible(1) = True
                  haveswater = True
              End Select
            Next
          
          Case modName
            haveglyph = True
            Loading.Gauge1.Max = Val(temp.idx1)
            Loading.Gauge1.Value = 0
            For m = 1 To temp.idx1
              updategauge
              If read_parmrec(fle, temp) Then
                For i = 0 To tsize
                  If gn_t(i).Name = temp.pName And gn_t(i).idx(0) = temp.idx1 Then
                    gn_t(i).Text = convert(temp.cunit, temp.uunit, Val(temp.pval))
                    gn_t(i).pUnit = temp.cunit
                    gn_t(i).Unit = temp.uunit
                    gn_t(i).refnum = temp.ref
                  End If
                Next
                If Not acute Then
                  For i = 0 To 2
                    If gn_i(i).Name = temp.pName And gn_i(i).idx(0) = temp.idx1 Then
                      gn_i(i).Text = convert(temp.cunit, temp.uunit, Val(temp.pval))
                      gn_i(i).pUnit = temp.cunit
                      gn_i(i).Unit = temp.uunit
                      gn_i(i).refnum = temp.ref
                    End If
                  Next
                End If
                For i = 0 To lsize
                  If i = 32 Then i = 36
                  If gn_l(i).Name = temp.pName And gn_l(i).idx(0) = temp.idx1 Then
                    If temp.pval = "True" Then
                       gn_l(i).Value = True
                       gen_exp.SSCheck1_Click gn_l(i).index, True
                    Else
                       gn_l(i).Value = False
                       gen_exp.SSCheck1_Click gn_l(i).index, False
                    End If
                    gn_l(i).refnum = temp.ref
                  End If
                Next
                If gn_s.Name = temp.pName And gn_s.idx(0) = temp.idx1 Then
                  gn_s.pval = Val(temp.pval)
                  gn_s.refnum = temp.ref
                End If
                Select Case temp.pName
                  Case "casid"
                    addkd
                    If temp.idx2 = 0 Then
                      kd(temp.idx1).cas = temp.pval
                    Else
                      kd(temp.idx1).progeny(temp.idx2).cas = temp.pval
                    End If
                  Case "soilkd"
                    addkd
                    If temp.idx2 = 0 Then
                      kd(temp.idx1).idx = 0
                      kd(temp.idx1).kd = CDbl(convert(temp.cunit, temp.uunit, Val(temp.pval)))
                      kd(temp.idx1).uunt = temp.uunit
                      kd(temp.idx1).ref = temp.ref
                    Else
                      kd(temp.idx1).progeny(temp.idx2).idx = 0
                      kd(temp.idx1).progeny(temp.idx2).kd = CDbl(convert(temp.cunit, temp.uunit, Val(temp.pval)))
                      kd(temp.idx1).progeny(temp.idx2).uunt = temp.uunit
                      kd(temp.idx1).progeny(temp.idx2).ref = temp.ref
                    End If
                  Case "leachr"
                    addkd
                    If temp.idx2 = 0 Then
                       kd(temp.idx1).idx = 0
                       kd(temp.idx1).lr = CDbl(temp.pval)
                       kd(temp.idx1).luunt = temp.uunit
                       kd(temp.idx1).ref = temp.ref
                     Else
                       kd(temp.idx1).progeny(temp.idx2).idx = 0
                       kd(temp.idx1).progeny(temp.idx2).lr = CDbl(convert(temp.cunit, temp.uunit, Val(temp.pval)))
                       kd(temp.idx1).progeny(temp.idx2).luunt = temp.uunit
                       kd(temp.idx1).progeny(temp.idx2).ref = temp.ref
                    End If
                End Select
              End If
            Next
          Case Else
            For m = 1 To temp.idx1
              get_line fle.file
            Next
        End Select
      End If
    Loop
    close_parm fle
    
    
    setup
    
'resolve contaminate differences          if contaminate no longer exists its index is 0
    For i = 1 To f_numcon
      For j = 1 To m_numcon
        If kd(j).cas = con(i).cas And con(i).kind = 1 Then
          kd(j).idx = i
          Exit For
       End If
      Next
      If j > m_numcon And con(i).kind = 1 Then
        m_numcon = m_numcon + 1
        ReDim Preserve kd(m_numcon) As kdparm
        kd(m_numcon).cas = con(i).cas
        kd(m_numcon).idx = i
        kd(m_numcon).uunt = "ml/g"
        kd(m_numcon).luunt = "1/yr"
        kd(m_numcon).ref = 0
        kd(m_numcon).kd = con(i).kd
      End If
    Next
    For i = 1 To m_numcon
      If kd(i).idx > 0 Then gen_exp.kd1.AddItem con(kd(i).idx).Name
    Next

'resolve progeny differences         if progeny no longer exists its index is 0
    For i = 1 To m_numcon
      If kd(i).idx <> 0 Then
        For j = 1 To con(kd(i).idx).numprog
          For k = 1 To kd(i).numprog
            If kd(i).progeny(k).cas = con(kd(i).idx).progeny(j).cas Then
              kd(i).progeny(k).idx = j
              Exit For
            End If
          Next
          If k > kd(i).numprog Then
            kd(i).numprog = kd(i).numprog + 1
            ReDim Preserve kd(i).progeny(kd(i).numprog) As d_kdparm
            kd(i).progeny(kd(i).numprog).cas = con(kd(i).idx).progeny(j).cas
            kd(i).progeny(kd(i).numprog).idx = j
            kd(i).progeny(kd(i).numprog).uunt = "ml/g"
            kd(i).progeny(kd(i).numprog).luunt = "1/yr"
            kd(i).progeny(kd(i).numprog).ref = 0
            kd(i).progeny(kd(i).numprog).kd = con(kd(i).idx).progeny(j).kd
          End If
        Next
      End If
    Next
    
  Else
    put_val errfile, "Can't find or open file " & FUIName
    put_line errfile
    close_csv errfile
    End
  End If
  
End Sub

Sub kdchange(tx As String, oldtx As String)
  Dim i As Long
  For i = 1 To m_numcon
    If con(kd(i).idx).Name = oldtx Then
       kd(i).kd = gen_exp.txt(91).Text
       kd(i).uunt = gen_exp.unt(91).Text
       kd(i).ref = Val(gen_exp.bref(16).Tag)
       Exit For
    End If
  Next
  For i = 0 To m_numcon
    If con(kd(i).idx).Name = tx Then
      set_unit gen_exp.unt(91), kd(i).uunt
      gen_exp.txt(91).Text = kd(i).kd
      gen_exp.bref(16).Tag = kd(i).ref
      gen_exp.bref(16).Caption = "Ref: " & kd(i).ref
      gn_t(90).Text = kd(i).kd
      gn_t(90).Change
      Exit For
    End If
  Next
End Sub

Sub d_kdchange(tx As String, oldtx As String)
  Dim i As Long
  Dim j As Long
  On Error Resume Next
  For i = 1 To m_numcon
    If con(kd(i).idx).Name = ctext3 Then
      For j = 1 To kd(i).numprog
        If con(kd(i).idx).progeny(kd(i).progeny(j).idx).Name = oldtx Then
          kd(i).progeny(j).kd = CDbl(gen_exp.txt(92).Text)
          kd(i).progeny(j).uunt = gen_exp.unt(92).Text
          kd(i).progeny(j).ref = Val(gen_exp.bref(17).Tag)
          Exit For
        End If
      Next
      Exit For
    End If
  Next
  For i = 0 To m_numcon
    If con(kd(i).idx).Name = gen_exp.kd1.Text Then
      For j = 1 To kd(i).numprog
        If con(kd(i).idx).progeny(kd(i).progeny(j).idx).Name = tx Then
          set_unit gen_exp.unt(92), kd(i).progeny(j).uunt
          gen_exp.txt(92).Text = kd(i).progeny(j).kd
          gen_exp.bref(17).Caption = "Ref: " & kd(i).progeny(j).ref
          gen_exp.bref(17).Tag = kd(i).progeny(j).ref
          gn_t(91).Text = kd(i).progeny(j).kd
          gn_t(91).Change
          Exit For
        End If
      Next
      Exit For
    End If
  Next
End Sub

Sub lrchange(tx As String, oldtx As String)
  Dim i As Long
  For i = 1 To m_numcon
    If con(kd(i).idx).Name = oldtx Then
       kd(i).lr = gen_exp.txt(91).Text
       kd(i).luunt = gen_exp.unt(91).Text
       kd(i).ref = Val(gen_exp.bref(16).Tag)
       Exit For
    End If
  Next
  For i = 0 To m_numcon
    If con(kd(i).idx).Name = tx Then
      set_unit gen_exp.unt(91), kd(i).luunt
      gen_exp.txt(91).Text = kd(i).lr
      gen_exp.bref(16).Caption = "Ref: " & kd(i).ref
      gen_exp.bref(16).Tag = kd(i).ref
      gn_t(92).Text = kd(i).lr
      gn_t(92).Change
      Exit For
    End If
  Next
End Sub

Sub d_lrchange(tx As String, oldtx As String)
  Dim i As Long
  Dim j As Long
  On Error Resume Next
  For i = 1 To m_numcon
    If con(kd(i).idx).Name = ctext3 Then
      For j = 1 To kd(i).numprog
        If con(kd(i).idx).progeny(kd(i).progeny(j).idx).Name = oldtx Then
          kd(i).progeny(j).lr = CDbl(gen_exp.txt(92).Text)
          kd(i).progeny(j).luunt = gen_exp.unt(92).Text
          kd(i).progeny(j).ref = Val(gen_exp.bref(17).Tag)
          Exit For
        End If
      Next
      Exit For
    End If
  Next
  For i = 0 To m_numcon
    If con(kd(i).idx).Name = gen_exp.kd1.Text Then
      For j = 1 To kd(i).numprog
        If con(kd(i).idx).progeny(kd(i).progeny(j).idx).Name = tx Then
          set_unit gen_exp.unt(92), kd(i).progeny(j).luunt
          gen_exp.txt(92).Text = kd(i).progeny(j).lr
          gen_exp.bref(17).Caption = "Ref: " & kd(i).progeny(j).ref
          gen_exp.bref(17).Tag = kd(i).progeny(j).ref
          gn_t(93).Text = kd(i).progeny(j).lr
          gn_t(93).Change
          Exit For
        End If
      Next
      Exit For
    End If
  Next
End Sub
    
Sub setup()
Dim i As Long
  If Not haveglyph Then
    gen_exp.cb(15).ListIndex = 0
  End If
  If Not haveswater Then
    For i = 1 To 3
      If i = 2 Then i = 3
      gn_l(i).Value = False
      gen_exp.SSCheck1_Click gn_l(i).index, False
      gn_l(i).Enabled = False
    Next
  End If
End Sub

Sub loaddef(kind As Long)
  Dim i As Long
  Dim m As Long
  Dim fName As String
  Dim fle As parmfile
  
  Select Case kind
    Case 1
    fName = App.Path + "\gndflbxm.def"
    Case 2
    fName = App.Path + "\gndflbxa.def"
    Case 3
    fName = App.Path + "\gndflbud.def"
    Case Else: Exit Sub
  End Select
  
  loadng = True
  If open_parm(fle, fName, 2) Then
    Do Until EOCF(fle.file)
      If read_parmrec(fle, temp) Then
        Loading.Gauge1.Max = Val(temp.idx1)
        Loading.Gauge1.Value = 0
        For m = 1 To temp.idx1
          updategauge
          If read_parmrec(fle, temp) Then
            For i = 0 To tsize
              If gn_t(i).Name = temp.pName And gn_t(i).idx(0) = temp.idx1 Then
                gn_t(i).Text = convert(temp.cunit, temp.uunit, Val(temp.pval))
                gn_t(i).pUnit = temp.cunit
                gn_t(i).Unit = temp.uunit
                gn_t(i).refnum = temp.ref
              End If
            Next
            If Not acute Then
              For i = 0 To 2
                If gn_i(i).Name = temp.pName And gn_i(i).idx(0) = temp.idx1 Then
                  gn_i(i).Text = convert(temp.cunit, temp.uunit, Val(temp.pval))
                  gn_i(i).pUnit = temp.cunit
                  gn_i(i).Unit = temp.uunit
                  gn_i(i).refnum = temp.ref
                End If
              Next
            End If
            For i = 0 To lsize
              If i = 32 Then i = 36
              If gn_l(i).Name = temp.pName And gn_l(i).idx(0) = temp.idx1 Then
                If temp.pval = "True" Then
                   gn_l(i).Value = True
                Else
                   gn_l(i).Value = False
                   gen_exp.SSCheck1_Click gn_l(i).index, False
                End If
                gn_l(i).refnum = temp.ref
              End If
            Next
            If gn_s.Name = temp.pName And gn_s.idx(0) = temp.idx1 Then
              gn_s.pval = Val(temp.pval)
              gn_s.refnum = temp.ref
            End If
          End If
        Next
      End If
    Loop
    close_parm fle
    setup
    loadng = False

  Else
    put_val errfile, "Can't find or open file " & FUIName
    put_line errfile
    close_csv errfile
    End
  End If
  
End Sub

