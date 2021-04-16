Attribute VB_Name = "Shared"
Option Explicit
Option Compare Text

Type desc
  Name As String
  idx As Long
  Kind As Boolean
End Type

Type Confluxs
    Name As String
    id As String
    Flux() As Double
    Chainindex As Long
End Type

Type Confluxset
    Parent As Confluxs
    NumProg As Long
    Prog() As Confluxs
End Type

Type Fluxset
    Kind As Long
    Radii As Double
    Dens As Double
End Type

Type AFFSet
    Name As String
    numflux As Long
    Flux() As Fluxset
    Conflux() As Confluxset
    Numconc As Long
End Type

Type AFF
    numsets As Long
    AFFSet() As AFFSet
    SrcIndex As Long
End Type

Type NucVarbs
    Name As String
    id As String
    indx As Long
    HalfLife As Double
    ParID1 As Long
    BrFct1 As Double
    ParID2 As Long
    BrFct2 As Double
End Type

Type Chain
    Parent As NucVarbs
    NumProg As Long
    Prog() As NucVarbs
End Type

Type Source
'all
    Name As String
    Kind As Long
    Area_Hgt As Double
    PlmFlg As Long
    St_Rad As Double
    St_Flow As Double
    ExTemp As Double

  #If PUFF Then
    X_val As Double
    Y_val As Double
    SigR As Double
    SigZ As Double
  #End If

  #If PLUME Then
    Rad As Double
    Ang As Double
    DispFlg As Long
    IscFlg As Long
    IscWakeFlg As Long
    LowBoundFlg As Long
    BIDFlg As Long
    HghWndFlg As Long
    LowWndFlg As Long
    BldHgt(36) As Double
    BldWdth(36) As Double
    BldArea(36) As Double
  #End If
End Type

Type RS_vals
'all
    RTX(2) As Double
    SigParm As Long
    WindMin As Double
    ChgMod As Double
'acute
    St_Year As Long
    St_Month As Long
    St_Day As Long
    St_Hour As Long
    
  #If PLUME Then
    RecNum As Long
    MinWind As Double
    Numrad As Long
    Radius() As Double
    CalmFlag As Long
    CalmDist(36) As Double
    SectorFlag As Long
    SiteBoundary(16) As Double
    MethodFlag As Long
    UserSector As Long
  #End If

  #If PUFF Then
    NumX As Long
    NumY As Long
    NodeDist As Double
    MaxPufRad As Double
    NumPufHr As Long
    SigRCoef As Double
    PufTimeStep As Long
    PufMergFlag As Long
    PufMergDist As Double
    TracRegFac As Double
    MinPufConc As Double
  #End If
End Type

Type LstVals
    MetFile As String
    CShnFile As String
    RSFile As String
    OutFile As String
    NucDataFile As String
    DomFile As String
End Type

Type F_Source
    Name As String
    Xval As Double
    Yval As Double
End Type

#If PUFF Then
  Type GridVals
      GridFlag As Long
      GridVals(50, 50) As Double
  End Type
  Global z0Grid As GridVals
#End If

Global NumChain As Long
Global ChainData() As Chain
Global NumAFF As Long
Global AFFData() As AFF
Global NumSrc As Long
Global SourceData() As Source
Global RSData As RS_vals
Global LstData As LstVals
Global doe As Double

Global Const PcitoCi As Double = 3.1688E-20 'Convert PCi/yr to Ci/s

Sub ChainsWrite()
    Dim fName As String
    Dim fnum As Long
    Dim mtext As String
    Dim ichn As Long, isrc As Long, iprg As Long
    Dim outstr As String, outrel As String, outmass As String

    fName = LstData.NucDataFile
    fnum = FreeFile
    
    Open fName For Output As fnum
    
    Print #fnum, NumChain
    
    For ichn = 1 To NumChain
        Print #fnum, Trim(ChainData(ichn).NumProg + 1)
        outstr = "'" + ChainData(ichn).Parent.Name + "'" + ","
        outstr = outstr + "'" + ChainData(ichn).Parent.id + "',"
        outstr = outstr + Trim(ChainData(ichn).Parent.HalfLife) + ","
        outstr = outstr + Trim(ChainData(ichn).Parent.indx) + ","
        outstr = outstr + Trim(ChainData(ichn).Parent.ParID1) + ","
        outstr = outstr + Trim(ChainData(ichn).Parent.BrFct1) + ","
        outstr = outstr + Trim(ChainData(ichn).Parent.ParID2) + ","
        outstr = outstr + Trim(ChainData(ichn).Parent.BrFct2) + ","
        outstr = outstr + "0"
        Print #fnum, outstr
        For isrc = 1 To NumSrc
            GetRelMass ichn, isrc, outrel, outmass
            Print #fnum, outrel
            Print #fnum, outmass
        Next isrc
        For iprg = 1 To ChainData(ichn).NumProg
            outstr = "'" + ChainData(ichn).Prog(iprg).Name + "',"
            outstr = outstr + "'" + ChainData(ichn).Prog(iprg).id + "',"
            outstr = outstr + Trim(ChainData(ichn).Prog(iprg).HalfLife) + ","
            outstr = outstr + Trim(ChainData(ichn).Prog(iprg).indx) + ","
            outstr = outstr + Trim(ChainData(ichn).Prog(iprg).ParID1) + ","
            outstr = outstr + Trim(ChainData(ichn).Prog(iprg).BrFct1) + ","
            outstr = outstr + Trim(ChainData(ichn).Prog(iprg).ParID2) + ","
            outstr = outstr + Trim(ChainData(ichn).Prog(iprg).BrFct2)
            Print #fnum, outstr
            For isrc = 1 To NumSrc
                GetNucRel ichn, isrc, iprg, outrel
                Print #fnum, outrel
            Next isrc
        Next iprg
    Next ichn
    
    Close fnum
End Sub

Sub GetAFFStuff(reltype As String)
  Dim affile As csv
  Dim af_name As String
  Dim fName As String
  Dim indx As Long
  Dim wrgname As Boolean
  Dim i As Long
  Dim srcindx As Long
  Dim mtext As String
  
  fName = LTrim(RTrim(argv(0))) & ".AFF"
  If open_csv(affile, fName, 2) Then
    Do Until EOCF(affile)
      af_name = get_val(affile)
      indx = Val(get_val(affile))
      wrgname = True
      For i = 1 To NumSrc
        If af_name = SourceData(i).Name Then
            srcindx = i
            wrgname = False
            Exit For
        End If
      Next i
      If wrgname Then
        For i = 1 To indx
            get_line affile
        Next i
      Else
        'find number of lines of header and skip
        get_line affile
        indx = Val(get_val(affile))
        For i = 1 To indx
            get_line affile
        Next i
      
        NumAFF = NumAFF + 1
        ReDim Preserve AFFData(NumAFF) As AFF
        AFFData(NumAFF).SrcIndex = srcindx
        get_line affile
        AFFData(NumAFF).numsets = Val(get_val(affile))
      
        ReDim Preserve AFFData(NumAFF).AFFSet(AFFData(NumAFF).numsets) As AFFSet
      
        For i = 1 To AFFData(NumAFF).numsets
          If i = 1 Then
            AFFReadSet AFFData(NumAFF).AFFSet(i), affile, SourceData(srcindx), reltype
          Else
            AFFReadAround affile
          End If
        Next i
      
      End If
      If Not EOCF(affile) Then
        get_line affile
      End If
    
    Loop
    close_csv affile
  Else
    mtext = "Unable to open or find file " + affile.fName
    put_val errfile, mtext
    put_line errfile
    close_csv errfile
    MsgBox mtext
    End
  End If
  
End Sub

Sub GetNucRel(chnindx As Long, srcindx As Long, prgindx As Long, outstr As String)
    Dim isrc As Long, icn As Long
    Dim iprg As Long, iflx As Long
    Dim totrel As Double
    Dim sumpar As Double
    Dim gasfrac As Double
    Dim sumgas As Double
    Dim ng_fract As Double
    Dim rg_fract As Double
    Dim part_fract As Double
    Dim nameid As String
               

'the following line was revised from "0" to bypass a chain-decay branching issue BAN 2 Nov 05
    outstr = "0.00E+00,0.00,0.00,1.00"
    For isrc = 1 To NumAFF
        If AFFData(isrc).SrcIndex = srcindx Then
            For icn = 1 To AFFData(isrc).AFFSet(1).Numconc
                If AFFData(isrc).AFFSet(1).Conflux(icn).Parent.Chainindex = chnindx Then
                    For iprg = 1 To AFFData(isrc).AFFSet(1).Conflux(icn).NumProg
'                    If AFFData(isrc).AFFSet(1).Conflux(icn).Prog(iprg).Chainindex = prgindx Then
'                            totrel = 0
'                            For iflx = 1 To AFFData(isrc).AFFSet(1).numflux
'                                totrel = totrel + AFFData(isrc).AFFSet(1).Conflux(icn).Prog(iprg).Flux(iflx) * PcitoCi
'                            Next iflx
'                            outstr = Format(totrel, "Scientific")
'                        End If
                    If AFFData(isrc).AFFSet(1).Conflux(icn).Prog(iprg).Chainindex = prgindx Then
                            totrel = 0
                            sumpar = 0
                            sumgas = 0
                            For iflx = 1 To AFFData(isrc).AFFSet(1).numflux
                                totrel = totrel + AFFData(isrc).AFFSet(1).Conflux(icn).Prog(iprg).Flux(iflx) * PcitoCi
                                If AFFData(isrc).AFFSet(1).Flux(iflx).Kind = 1 Then
                                  sumpar = sumpar + AFFData(isrc).AFFSet(1).Conflux(icn).Prog(iprg).Flux(iflx) * PcitoCi
                                Else
'                                  gasfrac = AFFData(isrc).AFFSet(1).Flux(iflx).Dens
                                  gasfrac = AFFData(isrc).AFFSet(1).Flux(iflx).Radii
                                  sumgas = sumgas + AFFData(isrc).AFFSet(1).Conflux(icn).Prog(iprg).Flux(iflx) * PcitoCi
                                End If
                            Next iflx
                            If totrel > 0 Then
                              ng_fract = sumgas * gasfrac / totrel
                              rg_fract = sumgas * (1 - gasfrac) / totrel
                              part_fract = sumpar / totrel
                              nameid = ChainData(icn).Prog(iprg).id
                              If (InStr(nameid, "EL") > 0 Or InStr(nameid, "Ar") > 0 Or InStr(nameid, "Kr") > 0 Or InStr(nameid, "Xe") > 0 Or InStr(nameid, "Rn") > 0) Then
                                ng_fract = 1
                                rg_fract = 0
                                part_fract = 0
                              End If
                            Else
                              ng_fract = 0
                              rg_fract = 0
                              part_fract = 1
                            End If
                            outstr = Format(totrel, "Scientific") + "," + Format(ng_fract, "#0.00") + "," + Format(rg_fract, "#0.00") + "," + Format(part_fract, "#0.00")
                        End If
                    Next iprg
                End If
            Next icn
        End If
    Next isrc

End Sub

Sub GetPartData(srcindx As Long, out1 As String, out2 As String, out3 As String, out4 As String)
    Dim i As Long
    Dim j As Long
    Dim numbins As Long
    
    out1 = ""
    out2 = ""
    out3 = ""
    out4 = ""
    For i = 1 To NumAFF
        If AFFData(i).SrcIndex = srcindx Then
            numbins = 0
            For j = 1 To AFFData(i).AFFSet(1).numflux
                If AFFData(i).AFFSet(1).Flux(j).Kind = 1 Then
                    numbins = numbins + 1
                    If numbins <= 8 Then
                        out3 = out3 + Format(AFFData(i).AFFSet(1).Flux(j).Radii, "0.0") + ","
                        out4 = out4 + Format(AFFData(i).AFFSet(1).Flux(j).Dens, "0.0") + ","
                    End If
                End If
            Next j
            If numbins > 0 Then
                out1 = "T"
                If numbins > 8 Then numbins = 8
                out2 = CStr(numbins)
            Else
                out1 = "F"
            End If
        End If
    Next i
End Sub

Sub GetRelMass(chnindx As Long, srcindx As Long, out1 As String, out2 As String)
    Dim isrc As Long
    Dim icn As Long
    Dim i As Long
    Dim iflx As Long
    Dim parrel() As Double
    Dim totpar As Long
    Dim gasfrac As Double
    Dim totrel As Double
    Dim sumpar As Double
    Dim sumgas As Double
    Dim ng_fract As Double
    Dim rg_fract As Double
    Dim part_fract As Double
    Dim nameid As String

    out1 = "0"
    out2 = "1"
    
    For isrc = 1 To NumAFF
        If AFFData(isrc).SrcIndex = srcindx Then
            For icn = 1 To AFFData(isrc).AFFSet(1).Numconc
                If AFFData(isrc).AFFSet(1).Conflux(icn).Parent.Chainindex = chnindx Then
                    totrel = 0
                    totpar = 0
                    sumpar = 0
                    sumgas = 0
                    For iflx = 1 To AFFData(isrc).AFFSet(1).numflux
                        totrel = totrel + AFFData(isrc).AFFSet(1).Conflux(icn).Parent.Flux(iflx) * PcitoCi
                        If AFFData(isrc).AFFSet(1).Flux(iflx).Kind = 1 Then
                          totpar = totpar + 1
                          ReDim Preserve parrel(totpar) As Double
                          parrel(totpar) = AFFData(isrc).AFFSet(1).Conflux(icn).Parent.Flux(iflx) * PcitoCi
                          sumpar = sumpar + parrel(totpar)
                        Else
'                          gasfrac = AFFData(isrc).AFFSet(1).Flux(iflx).Dens
                          gasfrac = AFFData(isrc).AFFSet(1).Flux(iflx).Radii
                          sumgas = sumgas + AFFData(isrc).AFFSet(1).Conflux(icn).Parent.Flux(iflx) * PcitoCi
                        End If
                    Next iflx
                    If totrel > 0 Then
                      ng_fract = (sumgas * gasfrac) / totrel
                      rg_fract = (sumgas * (1 - gasfrac)) / totrel
                      part_fract = sumpar / totrel
                      nameid = ChainData(icn).Parent.id
                      If (InStr(nameid, "EL") > 0 Or InStr(nameid, "Ar") > 0 Or InStr(nameid, "Kr") > 0 Or InStr(nameid, "Xe") > 0 Or InStr(nameid, "Rn") > 0) Then
                        ng_fract = 1
                        rg_fract = 0
                        part_fract = 0
                      End If
                    Else
                      ng_fract = 0
                      rg_fract = 0
                      part_fract = 1
                      
                    End If
                    out1 = Format(totrel, "Scientific") + "," + Format(ng_fract, "#0.00") + "," + Format(rg_fract, "#0.00") + "," + Format(part_fract, "#0.00")
                    out2 = ""
                    For i = 1 To totpar
                      If sumpar > 0 Then
                        out2 = out2 + Format((parrel(i) / sumpar), "#0.00") + ","
                      Else
                        out2 = out2 + " 1.00,"
                      End If
                    Next i
                End If
            Next icn
        End If
    Next isrc
    
End Sub

Sub AFFReadAround(fle As csv)
    Dim i As Long, j As Long, k As Long
    Dim indx As Long, indx1 As Long
    Dim indx2 As Long, indx3 As Long
    Dim dum As String
    
    For i = 1 To 8
        get_line fle
    Next i
    
    get_line fle
    indx = Val(get_val(fle))
    For i = 1 To indx
        get_line fle
    Next i
    
    get_line fle
    indx1 = Val(get_val(fle))
    For i = 1 To indx1
        get_line fle
        dum = get_val(fle)
        dum = get_val(fle)
        dum = get_val(fle)
        dum = get_val(fle)
        indx2 = Val(get_val(fle))
        indx3 = Val(get_val(fle))
        For j = 1 To indx2
            get_line fle
        Next j
        For k = 1 To indx3
            get_line fle
            dum = get_val(fle)
            dum = get_val(fle)
            dum = get_val(fle)
            dum = get_val(fle)
            indx2 = Val(get_val(fle))
            For j = 1 To indx2
                get_line fle
            Next j
        Next k
    Next i
        
End Sub

Sub AFFReadConFluxSet(tempcset As Confluxset, numflux As Long, fle As csv, reltype As String)

Dim dum As String
Dim addline As Boolean
Dim i As Long
Dim numtimes As Long
Dim prvtime As Double
Dim curtime As Double
Dim prvflux() As Double
Dim curflux() As Double
Dim etime As Double
Dim stime As Double
Dim iflx As Long, iprg As Long

  get_line fle
  tempcset.Parent.Name = get_val(fle)
  tempcset.Parent.id = get_val(fle)
  tempcset.Parent.Chainindex = ChainIndexGet(tempcset.Parent.id)
  dum = get_val(fle)
  dum = get_val(fle)
  numtimes = Val(get_val(fle))
  tempcset.NumProg = Val(get_val(fle))
  ReDim tempcset.Prog(tempcset.NumProg) As Confluxs
  ReDim tempcset.Parent.Flux(numflux) As Double
  ReDim prvflux(numflux) As Double
  ReDim curflux(numflux) As Double

'This section revised by BAN 14 Sept 2005 because Christian was a dope

  For i = 1 To numtimes
    get_line fle
'    prvtime = curtime
    curtime = Val(get_val(fle))
    If i = numtimes Then etime = curtime
    If i = 1 Then
      stime = curtime
      For iflx = 1 To numflux
        curflux(iflx) = Val(get_val(fle))
        'added
        prvflux(iflx) = curflux(iflx)
        '
      Next
    Else
      For iflx = 1 To numflux
        curflux(iflx) = Val(get_val(fle))
       ' If reltype = "acute" Then
       '  tempcset.Parent.Flux(iflx) = tempcset.Parent.Flux(iflx) + curflux(iflx)
       ' Else
       'here is part of the problem!!
          'tempcset.Parent.Flux(iflx) = tempcset.Parent.Flux(iflx) + (curflux(iflx) + prvflux(iflx) / 2) * (curtime - prvtime)
          tempcset.Parent.Flux(iflx) = tempcset.Parent.Flux(iflx) + ((curflux(iflx) + prvflux(iflx)) / 2) * (curtime - prvtime)
       ' End If
        prvflux(iflx) = curflux(iflx)
      Next iflx
    End If
    'added
    prvtime = curtime
    '
  Next i
  For iflx = 1 To numflux
    'I don't know why this would ever occur
    'If (etime - stime) > 0 Then
      If reltype = "acute" Then
      'Christian's units require that this have a year's worth of hours added for acute
      'The ACUTE output is a RATE for a hard-wired 1 hour
        tempcset.Parent.Flux(iflx) = tempcset.Parent.Flux(iflx) * 8760#
      Else
        tempcset.Parent.Flux(iflx) = tempcset.Parent.Flux(iflx) / (etime - stime)
      End If
    'Else
    '  tempcset.Parent.Flux(iflx) = curflux(iflx)
    'End If
  Next iflx
  If tempcset.NumProg > 0 Then
    For iprg = 1 To tempcset.NumProg
      get_line fle
      tempcset.Prog(iprg).Name = get_val(fle)
      tempcset.Prog(iprg).id = get_val(fle)
      tempcset.Prog(iprg).Chainindex = ChainNucIndexGet(tempcset.Prog(iprg).id, tempcset.Parent.Chainindex)
      dum = get_val(fle)
      dum = get_val(fle)
      numtimes = Val(get_val(fle))
      ReDim Preserve tempcset.Prog(iprg).Flux(numflux) As Double
         For iflx = 1 To numflux
          prvflux(iflx) = 0#
         Next iflx
      'added
      prvtime = 0
      '
      For i = 1 To numtimes
        get_line fle
      '  prvtime = curtime
        curtime = Val(get_val(fle))
        If i = numtimes Then etime = curtime
        If i = 1 Then
          stime = curtime
          For iflx = 1 To numflux
            curflux(iflx) = Val(get_val(fle))
       'added
            prvflux(iflx) = curflux(iflx)
        '
          Next
        Else
          For iflx = 1 To numflux
            curflux(iflx) = Val(get_val(fle))
            'If reltype = "acute" Then
            '  tempcset.Prog(iprg).Flux(iflx) = tempcset.Prog(iprg).Flux(iflx) + curflux(iflx)
            'Else
              tempcset.Prog(iprg).Flux(iflx) = tempcset.Prog(iprg).Flux(iflx) + (curflux(iflx) + prvflux(iflx) / 2) * (curtime - prvtime)
            'End If
            prvflux(iflx) = curflux(iflx)
          Next
        End If
        'added
        prvtime = curtime
        '
      Next i
      For iflx = 1 To numflux
        'If (etime - stime) > 0 Then
          If reltype = "acute" Then
          'Time unit added to correspond to RATE for 1 hour hard-wired
            tempcset.Prog(iprg).Flux(iflx) = tempcset.Prog(iprg).Flux(iflx) * 8766#
          Else
            tempcset.Prog(iprg).Flux(iflx) = tempcset.Prog(iprg).Flux(iflx) / (etime - stime)
          End If
        'Else
        '  tempcset.Prog(iprg).Flux(iflx) = curflux(iflx)
        'End If
      Next iflx
    Next iprg
  End If
End Sub

Sub AFFReadFluxSet(tempflux As Fluxset, fle As csv)
    Dim chktype As String
    Dim dum As String
    
    get_line fle
    chktype = get_val(fle)
    If InStr(1, chktype, "Gas 1", 1) > 0 Then
        tempflux.Kind = 0
    Else
        tempflux.Kind = 1
    End If
    tempflux.Radii = Val(get_val(fle))
    dum = get_val(fle)
    tempflux.Dens = Val(get_val(fle))
End Sub

Sub AFFReadSet(tempset As AFFSet, fle As csv, insource As Source, reltype As String)
    Dim chktype As String
    Dim chkarea As Double
    Dim i As Long
    
    get_line fle
    tempset.Name = get_val(fle)
    get_line fle
    chktype = get_val(fle)
    If chktype = "point" Then
        insource.Kind = 0
    Else
        insource.Kind = 1
    End If
    
    get_line fle
    chkarea = Val(get_val(fle))
    If insource.Kind = 0 Then
    'the following line was corrected by BAN on 6 March 2003
        insource.St_Rad = Sqr((chkarea) / (4 * Atn(1)))
        get_line fle
        insource.Area_Hgt = Val(get_val(fle))
        get_line fle
        get_line fle
        insource.St_Flow = Val(get_val(fle)) * chkarea
        get_line fle
        insource.ExTemp = Val(get_val(fle))
        get_line fle
    Else
        insource.Area_Hgt = chkarea
        For i = 1 To 5
            get_line fle
        Next i
    End If
    
    get_line fle
    tempset.numflux = Val(get_val(fle))
    ReDim tempset.Flux(tempset.numflux) As Fluxset
    For i = 1 To tempset.numflux
        AFFReadFluxSet tempset.Flux(i), fle
    Next i
    get_line fle
    tempset.Numconc = Val(get_val(fle))
    ReDim tempset.Conflux(tempset.Numconc) As Confluxset
    
    For i = 1 To tempset.Numconc
        AFFReadConFluxSet tempset.Conflux(i), tempset.numflux, fle, reltype
    Next i
End Sub

Function ChainIndexGet(id As String) As Long
    Dim i As Long

    ChainIndexGet = 0
    For i = 1 To NumChain
        If ChainData(i).Parent.id = id Then
            ChainIndexGet = i
            Exit For
        End If
    Next i
End Function

Function ChainNucIndexGet(id As String, parindx As Long) As Long
    Dim i As Long
        
    ChainNucIndexGet = 0
    If parindx = 0 Or parindx > NumChain Then
        Exit Function
    End If
    
    For i = 1 To ChainData(parindx).NumProg
        If ChainData(parindx).Prog(i).id = id Then
            ChainNucIndexGet = i
            Exit For
        End If
    Next i
    
End Function


Sub ChainSet(newchain As Chain, addchain As Chain)
    Dim i As Long

    addchain.Parent.Name = newchain.Parent.Name
    addchain.Parent.id = newchain.Parent.id
    addchain.Parent.indx = newchain.Parent.indx
    addchain.Parent.HalfLife = newchain.Parent.HalfLife
    addchain.Parent.ParID1 = newchain.Parent.ParID1
    addchain.Parent.BrFct1 = newchain.Parent.BrFct1
    addchain.Parent.ParID2 = newchain.Parent.ParID2
    addchain.Parent.BrFct2 = newchain.Parent.BrFct2
    addchain.NumProg = newchain.NumProg
    ReDim addchain.Prog(addchain.NumProg) As NucVarbs
    If addchain.NumProg > 0 Then
        For i = 1 To addchain.NumProg
            addchain.Prog(i).Name = newchain.Prog(i).Name
            addchain.Prog(i).id = newchain.Prog(i).id
            addchain.Prog(i).indx = newchain.Prog(i).indx
            addchain.Prog(i).HalfLife = newchain.Prog(i).HalfLife
            addchain.Prog(i).ParID1 = newchain.Prog(i).ParID1
            addchain.Prog(i).BrFct1 = newchain.Prog(i).BrFct1
            addchain.Prog(i).ParID2 = newchain.Prog(i).ParID2
            addchain.Prog(i).BrFct2 = newchain.Prog(i).BrFct2
        Next i
    End If
    
   'Delete all progeny information from newchain
   newchain.NumProg = 0
   ReDim newchain.Prog(newchain.NumProg)
End Sub

Sub GetNucInfo(nucid As String, temp As Chain, found As Boolean, etext As String)
    Dim fName As String
    Dim fnum As Long
    Dim chkid As String
    Dim exdaug As Long
    Dim imdaug As Long
    Dim i As Long
    Dim inpstr As String
    
    etext = ""
    found = False
    If nucid = "" Then Exit Sub
    fName = App.Path + "\rmdlib.dat"
    On Error Resume Next
    fnum = FreeFile
    
    Open fName For Input Access Read As fnum
    If Err.Number <> 0 Then
        etext = "Error opening " + fName + Chr(10) + "Error is " + Error()
        Exit Sub
    End If
    
    For i = 1 To 2
        Line Input #fnum, inpstr
    Next i
    
    Do
        If EOF(fnum) Then
            Exit Do
        End If
        
        Line Input #fnum, inpstr
        chkid = LTrim$(RTrim$(Mid(inpstr, 1, 10)))
        exdaug = Abs(Val(Mid(inpstr, 12, 1)))
        imdaug = Val(Mid(inpstr, 15, 1))
        If nucid = chkid Then
            temp.Parent.Name = SetNucName(nucid)
            temp.Parent.id = nucid
            Line Input #fnum, inpstr
            temp.Parent.HalfLife = Val(Mid(inpstr, 30, 8))
            temp.Parent.indx = Val(Mid(inpstr, 39, 1))
            temp.Parent.ParID1 = Val(Mid(inpstr, 41, 1))
            temp.Parent.BrFct1 = Val(Mid(inpstr, 43, 6))
            temp.Parent.ParID2 = Val(Mid(inpstr, 50, 1))
            temp.Parent.BrFct2 = Val(Mid(inpstr, 52, 6))
            If imdaug > 0 Then
                For i = 1 To imdaug
                    Line Input #fnum, inpstr
                Next i
            End If
            
            If exdaug > 1 Then
                temp.NumProg = exdaug - 1
                ReDim temp.Prog(temp.NumProg) As NucVarbs

                For i = 1 To exdaug - 1
                    Line Input #fnum, inpstr
                    temp.Prog(i).id = LTrim$(RTrim$(Mid(inpstr, 20, 10)))
                    temp.Prog(i).Name = SetNucName(temp.Prog(i).id)
                    temp.Prog(i).HalfLife = Val(Mid(inpstr, 30, 8))
                    temp.Prog(i).indx = Val(Mid(inpstr, 39, 1))
                    temp.Prog(i).ParID1 = Val(Mid(inpstr, 41, 1))
                    temp.Prog(i).BrFct1 = Val(Mid(inpstr, 43, 6))
                    temp.Prog(i).ParID2 = Val(Mid(inpstr, 50, 1))
                    temp.Prog(i).BrFct2 = Val(Mid(inpstr, 52, 6))
                Next i
            End If
            found = True
            Exit Do
        ElseIf chkid = "end" Then
            Exit Do
        Else
            For i = 1 To exdaug + imdaug
                Line Input #fnum, inpstr
            Next i
        End If
    Loop
 
End Sub

Function SetNucName(id As String) As String
  Dim chkchar As String
  Dim chkchar1 As String
  Dim chkchar2 As String
  
  chkchar = Mid(id, 1, 1)
  chkchar1 = Mid(id, 2, 1)
  chkchar2 = Mid(id, 3, 1)
  SetNucName = "Unknown"
  Select Case (chkchar)
    Case "a":      Select Case (chkchar1)
        Case "c":    SetNucName = "Actinium-" + Mid(id, 3)
        Case "g":    SetNucName = "Silver-" + Mid(id, 3)
        Case "l":    SetNucName = "Aluminum-" + Mid(id, 3)
        Case "m":    SetNucName = "Americium-" + Mid(id, 3)
        Case "r":    SetNucName = "Argon-" + Mid(id, 3)
        Case "s":    SetNucName = "Arsenic-" + Mid(id, 3)
        Case "t":    SetNucName = "Astatine-" + Mid(id, 3)
        Case "u":    SetNucName = "Gold-" + Mid(id, 3)
      End Select
    Case "b":      Select Case (chkchar1)
        Case "a":    SetNucName = "Barium-" + Mid(id, 3)
        Case "e":    SetNucName = "Beryllium-" + Mid(id, 3)
        Case "i":    SetNucName = "Bismuth-" + Mid(id, 3)
        Case "k":    SetNucName = "Berkelium-" + Mid(id, 3)
        Case "r":    SetNucName = "Bromine-" + Mid(id, 3)
        Case Else
          If InStr(1, "bcdfghjlmniopqstuvwxyz", chkchar1, 1) = 0 Then SetNucName = "Boron-" + Mid(id, 2)
      End Select
    Case "c":      Select Case (chkchar1)
        Case "a":    SetNucName = "Calcium-" + Mid(id, 3)
        Case "d":    SetNucName = "Cadmium-" + Mid(id, 3)
        Case "e":    SetNucName = "Cerium-" + Mid(id, 3)
        Case "f":    SetNucName = "Californium-" + Mid(id, 3)
        Case "l":    SetNucName = "Chlorine-" + Mid(id, 3)
        Case "m":    SetNucName = "Curium-" + Mid(id, 3)
        Case "r":    SetNucName = "Chromium-" + Mid(id, 3)
        Case "o":    SetNucName = "Cobalt-" + Mid(id, 3)
        Case "s":    SetNucName = "Cesium-" + Mid(id, 3)
        Case "u":    SetNucName = "Copper-" + Mid(id, 3)
        Case Else
          If InStr(1, "bcghijknpqtvwxyz", chkchar1, 1) = 0 Then SetNucName = "Carbon-" + Mid(id, 2)
      End Select
    Case "d":      Select Case (chkchar1)
        Case "y":    SetNucName = "Dysprosium-" + Mid(id, 3)
      End Select
    Case "e":      Select Case (chkchar1)
        Case "s":    SetNucName = "Einsteinium-" + Mid(id, 3)
        Case "r":    SetNucName = "Erbium-" + Mid(id, 3)
        Case "u":    SetNucName = "Europium-" + Mid(id, 3)
      End Select
    Case "f":      Select Case (chkchar1)
        Case "m":    SetNucName = "Fermium-" + Mid(id, 3)
        Case "r":    SetNucName = "Francium-" + Mid(id, 3)
        Case "e":    SetNucName = "Iron-" + Mid(id, 3)
        Case Else
          If InStr(1, "abcdfghijklnopqstuvwxyz", chkchar1, 1) = 0 Then SetNucName = "Fluorine-" + Mid(id, 2)
      End Select
    Case "g":      Select Case (chkchar1)
        Case "d":    SetNucName = "Gadolinium-" + Mid(id, 3)
        Case "a":    SetNucName = "Gallium-" + Mid(id, 3)
        Case "e":    SetNucName = "Germanium-" + Mid(id, 3)
      End Select
    Case "h":      Select Case (chkchar1)
        Case "f":    SetNucName = "Hafnium-" + Mid(id, 3)
        Case "e":    SetNucName = "Helium-" + Mid(id, 3)
        Case "o":    SetNucName = "Holmium-" + Mid(id, 3)
        Case "g":    SetNucName = "Mercury-" + Mid(id, 3)
        Case Else
          If InStr(1, "abcdhijklmnpqrstuvwxyz", chkchar1, 1) = 0 Then SetNucName = "Hydrogen-" + Mid(id, 2)
      End Select
    Case "i":      Select Case (chkchar1)
        Case "n":    SetNucName = "Indium-" + Mid(id, 3)
        Case "r":    SetNucName = "Iridium-" + Mid(id, 3)
        Case Else
          If InStr(1, "abcdefghijklmopqstuvwxyz", chkchar1, 1) = 0 Then SetNucName = "Iodine-" + Mid(id, 2)
      End Select
    Case "j"
    
    Case "k":      Select Case (chkchar1)
        Case "r":    SetNucName = "Krypton-" + Mid(id, 3)
        Case Else
          If InStr(1, "abcdefghijklmnopqstuvwxyz", chkchar1, 1) = 0 Then SetNucName = "Potassium-" + Mid(id, 2)
      End Select
    Case "l":      Select Case (chkchar1)
        Case "a":    SetNucName = "Lanthanum-" + Mid(id, 3)
        Case "r":    SetNucName = "Lawrencium-" + Mid(id, 3)
        Case "i":    SetNucName = "Lithium-" + Mid(id, 3)
        Case "u":    SetNucName = "Lutetium-" + Mid(id, 3)
      End Select
    Case "m":      Select Case (chkchar1)
        Case "g":    SetNucName = "Magnesium-" + Mid(id, 3)
        Case "n":    SetNucName = "Manganese-" + Mid(id, 3)
        Case "d":    SetNucName = "Mendelevium-" + Mid(id, 3)
        Case "o":    SetNucName = "Molybdenum-" + Mid(id, 3)
      End Select
    Case "n":      Select Case (chkchar1)
        Case "d":    SetNucName = "Neodymium-" + Mid(id, 3)
        Case "e":    SetNucName = "Neon-" + Mid(id, 3)
        Case "p":    SetNucName = "Neptumium-" + Mid(id, 3)
        Case "i":    SetNucName = "Nickel-" + Mid(id, 3)
        Case "b":    SetNucName = "Niobium-" + Mid(id, 3)
        Case "o":    SetNucName = "Nobelium-" + Mid(id, 3)
        Case "a":    SetNucName = "Sodium-" + Mid(id, 3)
        Case Else
          If InStr(1, "cfghjklmnqrstuvwxyz", chkchar1, 1) = 0 Then SetNucName = "Nitrogen-" + Mid(id, 2)
      End Select
    Case "o":      Select Case (chkchar1)
        Case "s":    SetNucName = "Osmium-" + Mid(id, 3)
        Case "b":    SetNucName = "OBT"
        Case Else
          If InStr(1, "acdefghijklmnopqrtuvwxyz", chkchar1, 1) = 0 Then SetNucName = "Oxygen-" + Mid(id, 2)
      End Select
    Case "p":      Select Case (chkchar1)
        Case "b":    SetNucName = "Lead-" + Mid(id, 3)
        Case "d":    SetNucName = "Palladium-" + Mid(id, 3)
        Case "t":    SetNucName = "Platinum-" + Mid(id, 3)
        Case "u":    SetNucName = "Plutonium-" + Mid(id, 3)
        Case "o":    SetNucName = "Polonium-" + Mid(id, 3)
        Case "r":    SetNucName = "Praseodymium-" + Mid(id, 3)
        Case "m":    SetNucName = "Promethium-" + Mid(id, 3)
        Case "a":    SetNucName = "Protactinium-" + Mid(id, 3)
        Case Else
          If InStr(1, "cefghijklnpqsvwxyz", chkchar1, 1) = 0 Then SetNucName = "Phosphorus-" + Mid(id, 2)
      End Select
    Case "q"
    
    Case "r":      Select Case (chkchar1)
        Case "a":    SetNucName = "Radium-" + Mid(id, 3)
        Case "n":    SetNucName = "Radon-" + Mid(id, 3)
        Case "e":    SetNucName = "Rhenium-" + Mid(id, 3)
        Case "h":    SetNucName = "Rhodium-" + Mid(id, 3)
        Case "b":    SetNucName = "Rubidium-" + Mid(id, 3)
        Case "u":    SetNucName = "Ruthenium-" + Mid(id, 3)
      End Select
    Case "s":      Select Case (chkchar1)
        Case "b":    SetNucName = "Antimony-" + Mid(id, 3)
        Case "m":    SetNucName = "Samarium-" + Mid(id, 3)
        Case "c":    SetNucName = "Scandium-" + Mid(id, 3)
        Case "e":    SetNucName = "Selenium-" + Mid(id, 3)
        Case "i":    SetNucName = "Silicon-" + Mid(id, 3)
        Case "r":    SetNucName = "Strontium-" + Mid(id, 3)
        Case "n":    SetNucName = "Tin-" + Mid(id, 3)
        Case Else
          If InStr(1, "adfghjklopqstuvwxyz", chkchar1, 1) = 0 Then SetNucName = "Sulfur-" + Mid(id, 2)
      End Select
    Case "t":      Select Case (chkchar1)
        Case "a":    SetNucName = "Tantalum-" + Mid(id, 3)
        Case "c":    SetNucName = "Technetium-" + Mid(id, 3)
        Case "e":    SetNucName = "Tellurium-" + Mid(id, 3)
        Case "b":    SetNucName = "Terbium-" + Mid(id, 3)
        Case "l":    SetNucName = "Thallium-" + Mid(id, 3)
        Case "h":    SetNucName = "Thorium-" + Mid(id, 3)
        Case "m":    SetNucName = "Thulium-" + Mid(id, 3)
        Case "i":    SetNucName = "Titanium-" + Mid(id, 3)
      End Select
    Case "u":      Select Case (chkchar1)
        Case "n"
          Select Case (chkchar2)
            Case "e":    SetNucName = "Unnilennium-" + Mid(id, 4)
            Case "h":    SetNucName = "Unnilhexium-" + Mid(id, 4)
            Case "o":    SetNucName = "Unniloctium-" + Mid(id, 4)
            Case "p":    SetNucName = "Unnilpentium-" + Mid(id, 4)
            Case "q":    SetNucName = "Unnilquadium-" + Mid(id, 4)
            Case "s":    SetNucName = "Unnilseptium-" + Mid(id, 4)
          End Select
        Case "u"
          Select Case (chkchar2)
            Case "b":    SetNucName = "Ununbium-" + Mid(id, 4)
            Case "n":    SetNucName = "Ununnilium-" + Mid(id, 4)
            Case "u":    SetNucName = "Unununium-" + Mid(id, 4)
          End Select
        Case Else
          If InStr(1, "abcdefghijklmopqrstvwxyz", chkchar1, 1) = 0 Then SetNucName = "Uranium-" + Mid(id, 2)
      End Select
    Case "v"
      If InStr(1, "abcdefghijklmnopqrstuvwxyz", chkchar1, 1) = 0 Then SetNucName = "Vanadium-" + Mid(id, 2)
    Case "w"
      If InStr(1, "abcdefghijklmnopqrstuvwxyz", chkchar1, 1) = 0 Then SetNucName = "Tungsten-" + Mid(id, 2)
    Case "x"
      If chkchar1 = "e" Then SetNucName = "Xenon-" + Mid(id, 3)
    Case "y":      Select Case (chkchar1)
        Case "b":    SetNucName = "Ytterbium-" + Mid(id, 3)
        Case Else
          If InStr(1, "acdefghijklmnopqrstuvwxyz", chkchar1, 1) = 0 Then SetNucName = "Yttrium-" + Mid(id, 2)
      End Select
    Case "z":      Select Case (chkchar1)
        Case "r":    SetNucName = "Zirconium-" + Mid(id, 3)
        Case "n":    SetNucName = "Zinc-" + Mid(id, 3)
      End Select
  End Select

End Function

Sub updategauge()
  Loading.Gauge1.Value = Loading.Gauge1.Value + 1
  Loading.Label1 = "Loading ... " + Format(Loading.Gauge1.Value / Loading.Gauge1.Max * 100, "###") + "%"
  Loading.Refresh
End Sub

Sub XY_TO_RADANG(x As Double, y As Double, Rad As Double, Ang As Double)
    Dim pi, degtorad As Double
    
    pi = 4 * Atn(1)
    degtorad = pi / 180
    
    Rad = Sqr(x * x + y * y)

    If x = 0 And y = 0 Then
        
        Ang = 0
    ElseIf y = 0 Then
        If x > 0 Then
            
            Ang = 90
        Else
            Ang = 270
        End If
    ElseIf y > 0 Then
        If x >= 0 Then
            Ang = Atn(x / y) / degtorad
        Else
            Ang = 360 + Atn(x / y) / degtorad
        End If
    Else
        Ang = 180 + Atn(x / y) / degtorad
    End If

End Sub

Sub SizeSourceData(size As Long)
  If size > NumSrc Then
    NumSrc = size
    ReDim Preserve SourceData(NumSrc) As Source
  End If
End Sub

Sub GetGIDStuff()
  Dim temp As parmrec
  Dim fle As parmfile
  Dim m As Long
  Dim etext As String
  Dim tempchain As Chain
  Dim found As Boolean
  Dim f_srcnum As Long
  Dim f_src() As F_Source
  Dim nosrc As Boolean
  Dim i As Long
  Dim j As Long
  Dim mtext As String
  Dim airxval As Double
  Dim airyval As Double
  Dim jhour As Long
  
  doe = False
  If open_parm(fle, FUIName, 2) Then
    Do Until EOCF(fle.file)
      If read_parmrec(fle, temp) Then
        Select Case temp.pName
          Case "fui"
            Loading.Label1.Caption = "Loading FUI Values"
            Loading.Gauge1.Max = Val(temp.idx1)
            Loading.Gauge1.Value = 0
            For m = 1 To temp.idx1
              updategauge
              If read_parmrec(fle, temp) Then
                If temp.idx1 = siteIdx Then
                  Select Case temp.pName
                    Case "airx"
                      If temp.idx2 = modIdx Then
                        airxval = Val(temp.pval)
                      End If
                    Case "airy"
                      If temp.idx2 = modIdx Then
                        airyval = Val(temp.pval)
                      End If
                    Case "fscasid"
                      If temp.idx3 = 0 Then
                        GetNucInfo temp.pval, tempchain, found, etext
                        If etext <> "" Then
                          put_val errfile, etext
                          put_line errfile
                          close_csv errfile
                          MsgBox etext
                          End
                        End If
                        If found Then
                          NumChain = NumChain + 1
                          ReDim Preserve ChainData(NumChain) As Chain
                          ChainSet tempchain, ChainData(NumChain)
                        End If
                      End If
                                       
                    Case "srcnum"
                      f_srcnum = Val(temp.pval)
                      ReDim Preserve f_src(f_srcnum) As F_Source
                    Case "srcname"
                      If temp.idx2 > f_srcnum Then
                        f_srcnum = temp.idx2
                        ReDim Preserve f_src(f_srcnum) As F_Source
                      End If
                      f_src(temp.idx2).Name = temp.pval
                    Case "srcx"
                      If temp.idx2 > f_srcnum Then
                        f_srcnum = temp.idx2
                        ReDim Preserve f_src(f_srcnum) As F_Source
                      End If
                      f_src(temp.idx2).Xval = Val(temp.pval)
                    Case "srcy"
                      If temp.idx2 > f_srcnum Then
                        f_srcnum = temp.idx2
                        ReDim Preserve f_src(f_srcnum) As F_Source
                      End If
                      f_src(temp.idx2).Yval = Val(temp.pval)
                    Case "usrnum"
                      f_srcnum = Val(temp.pval)
                      ReDim Preserve f_src(f_srcnum) As F_Source
                    Case "usrname"
                      If temp.idx2 > f_srcnum Then
                        f_srcnum = temp.idx2
                        ReDim Preserve f_src(f_srcnum) As F_Source
                      End If
                      f_src(temp.idx2).Name = temp.pval
                    Case "usrx"
                      If temp.idx2 > f_srcnum Then
                        f_srcnum = temp.idx2
                        ReDim Preserve f_src(f_srcnum) As F_Source
                      End If
                      f_src(temp.idx2).Xval = Val(temp.pval)
                    Case "usry"
                      If temp.idx2 > f_srcnum Then
                        f_srcnum = temp.idx2
                        ReDim Preserve f_src(f_srcnum) As F_Source
                      End If
                      f_src(temp.idx2).Yval = Val(temp.pval)
                  End Select
                End If
              End If
            Next m
          Case modName
            Loading.Label1.Caption = "Loading Model Values"
            Loading.Gauge1.Max = Val(temp.idx1)
            Loading.Gauge1.Value = Loading.Gauge1.Max - Val(temp.idx1)
            For m = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                updategauge
                Select Case temp.pName
                 'source data
                  Case "arsrcnumb"
                    NumSrc = Val(temp.pval)
                    ReDim Preserve SourceData(NumSrc) As Source
                  Case "arsrcname"
                    SizeSourceData temp.idx3
                    SourceData(temp.idx3).Name = temp.pval
                  Case "arsrcdoriseflag"
                    SizeSourceData temp.idx3
                    SourceData(temp.idx3).PlmFlg = Val(temp.pval)
                  Case "armetfile"
                    LstData.MetFile = temp.pval
                  Case "arcldshnlib"
                    LstData.CShnFile = temp.pval

                 'RS data
                  Case "artransresist"
                    RSData.RTX(temp.idx3) = Val(temp.pval)
                  Case "arsigparm"
                    RSData.SigParm = Val(temp.pval)
                  Case "arminrisespd"
                    RSData.WindMin = Val(temp.pval)
                  Case "arminsigyshift"
                    RSData.ChgMod = Val(temp.pval)
#If PLUME Then
                 'source data
                  Case "arsrcdodispflag"
                    SizeSourceData temp.idx3
                    SourceData(temp.idx3).DispFlg = Val(temp.pval)
                  Case "ariscflag"
                    SizeSourceData temp.idx3
                    SourceData(temp.idx3).IscFlg = Val(temp.pval)
                  Case "ariscwakeflag"
                    SizeSourceData temp.idx3
                    SourceData(temp.idx3).IscWakeFlg = Val(temp.pval)
                  Case "arlowboundflag"
                    SizeSourceData temp.idx3
                    SourceData(temp.idx3).LowBoundFlg = Val(temp.pval)
                  Case "arbidflag"
                    SizeSourceData temp.idx3
                    SourceData(temp.idx3).BIDFlg = Val(temp.pval)
                  Case "arhghwndcorflag"
                    SizeSourceData temp.idx3
                    SourceData(temp.idx3).HghWndFlg = Val(temp.pval)
                  Case "arlowwndcorflag"
                    SizeSourceData temp.idx3
                    SourceData(temp.idx3).LowWndFlg = Val(temp.pval)
                  Case "arbldhgt"
                    SizeSourceData temp.idx3
                    SourceData(temp.idx3).BldHgt(temp.idx4) = Val(temp.pval)
                  Case "arbldwdth"
                    SizeSourceData temp.idx3
                    SourceData(temp.idx3).BldWdth(temp.idx4) = Val(temp.pval)
                  Case "arbldarea"
                    SizeSourceData temp.idx3
                    SourceData(temp.idx3).BldArea(temp.idx4) = Val(temp.pval)
                  
                 'RS data
                  Case "arminwind"
                    RSData.MinWind = Val(temp.pval)
                  Case "arradval"
                    RSData.Numrad = RSData.Numrad + 1
                    ReDim Preserve RSData.Radius(RSData.Numrad) As Double
                    RSData.Radius(RSData.Numrad) = Val(temp.pval)
                  Case "arnumrecring"
                    RSData.RecNum = Val(temp.pval)
                  Case "arcalmdistflag"
                    RSData.CalmFlag = Val(temp.pval)
                  Case "arcalmdist"
                    RSData.CalmDist(temp.idx3) = Val(temp.pval)
                  Case "arsectorflag"
                    RSData.SectorFlag = Val(temp.pval)

#End If

#If PUFF Then
                 'source data
                  Case "arsrcsigr"
                    SizeSourceData temp.idx3
                    SourceData(temp.idx3).SigR = Val(temp.pval)
                  Case "arsrcsigz"
                    SizeSourceData temp.idx3
                    SourceData(temp.idx3).SigZ = Val(temp.pval)

                 'RS data
                  Case "arrecpnumx"
                    RSData.NumX = Val(temp.pval)
                  Case "arrecpnumy"
                    RSData.NumY = Val(temp.pval)
                  Case "arpuffmaxrad"
                    RSData.MaxPufRad = Val(temp.pval)
                  Case "arpuffnumhr"
                    RSData.NumPufHr = Val(temp.pval)
                  Case "arpuffsigrcoef"
                    RSData.SigRCoef = Val(temp.pval)
                  Case "arpufftimestep"
                    RSData.PufTimeStep = Val(temp.pval)
                  Case "arpuffmergeflag"
                    RSData.PufMergFlag = Val(temp.pval)
                  Case "arpuffmergedist"
                    RSData.PufMergDist = Val(temp.pval)
                  Case "artrackregfact"
                    RSData.TracRegFac = Val(temp.pval)
                  Case "arminconcpuff"
                    RSData.MinPufConc = Val(temp.pval)
                  Case "arrecpdist"
                    RSData.NodeDist = Val(temp.pval)
                 
                 'zgrid data
                  Case "ardogridflag"
                    z0Grid.GridFlag = Val(temp.pval)
                  Case "arz0gridval"
                    z0Grid.GridVals(temp.idx3, temp.idx4) = Val(temp.pval)
#End If

#If ACUTE Then
                  Case "jhour"
                    jhour = Val(temp.pval)
                  Case "arstarthour"
                    RSData.St_Hour = Val(temp.pval)
                  Case "arstartmonth"
                    RSData.St_Month = Val(temp.pval)
                  Case "arstartday"
                    RSData.St_Day = Val(temp.pval)
                  Case "arstartyear"
                    RSData.St_Year = Val(temp.pval)
                  Case "methodflag"
                    RSData.MethodFlag = Val(temp.pval)
                    doe = True
                  Case "usersector"
                    RSData.UserSector = Val(temp.pval)
                    doe = True
                  Case "siteboundary"
                    RSData.SiteBoundary(temp.idx3) = Val(temp.pval)
                    doe = True
#End If

                End Select
              End If
            Next m
          Case Else
            For m = 1 To temp.idx1
              get_line fle.file
            Next m
        End Select
        
      End If
    Loop
    close_parm fle

#If ACUTE Then
    JulHours2Date jhour, RSData.St_Year, RSData.St_Month, RSData.St_Day, RSData.St_Hour
#End If

    'Set source data for data in fui section
    For i = 1 To NumSrc
      nosrc = False
      For j = 1 To f_srcnum
        If SourceData(i).Name = f_src(j).Name Then
          nosrc = True
          
'convert to m from km
#If PLUME Then
          XY_TO_RADANG (f_src(j).Xval - airxval) * 1000, (f_src(j).Yval - airyval) * 1000, SourceData(i).Rad, SourceData(i).Ang
#End If

#If PUFF Then
          SourceData(i).X_val = (f_src(j).Xval - airxval) * 1000
          SourceData(i).Y_val = (f_src(j).Yval - airyval) * 1000
#End If
          
          Exit For
        End If
        If nosrc Then
          mtext = "Source Name in FUI section and Air section do not match..rerun Air UI"
          put_val errfile, mtext
          put_line errfile
          close_csv errfile
          MsgBox mtext
          End
        End If
      Next j
    Next i
  Else
    mtext = "Can't find or open file " + FUIName
    put_val errfile, mtext
    put_line errfile
    close_csv errfile
    End
  End If
End Sub

Sub LSTWrite()
  Dim fnum As Long
  Dim fName As String
  Dim mtext As String
  
  On Error Resume Next
  
  fnum = FreeFile
  fName = RunName + ".LST"
  Open fName For Output As fnum
  If Err.Number <> 0 Then
    mtext = "Error writting to listing file " + fName + Chr(10) + "Error is " + Error()
    put_val errfile, mtext
    put_line errfile
    close_csv errfile
    MsgBox mtext
    End
  End If
  
  Print #fnum, LstData.RSFile
  Print #fnum, LstData.NucDataFile

#If PUFF Then
  Print #fnum, LstData.DomFile
#End If

  Print #fnum, LstData.CShnFile
  Print #fnum, LstData.MetFile
  Print #fnum, LstData.OutFile
  Close fnum
End Sub

Sub RSWrite()
  Dim fnum As Long
  Dim mtext As String
  Dim outstr As String, outstr1 As String
  Dim outstr2 As String, outstr3 As String
  Dim i As Long, irec As Long

  On Error Resume Next
  
  fnum = FreeFile
  Open LstData.RSFile For Output As fnum
  If Err.Number <> 0 Then
    mtext = "Error writting to listing file " + LstData.RSFile + Chr(10) + "Error is " + Error()
    put_val errfile, mtext
    put_line errfile
    close_csv errfile
    MsgBox mtext
    End
  End If
  
  Print #fnum, Format(RSData.WindMin, "0.0")
  Print #fnum, Format(RSData.ChgMod, "0.0")

#If PLUME Then
  Print #fnum, Format(RSData.MinWind, "0.00")
  Print #fnum, CStr(RSData.Numrad)
  outstr = ""
  For i = 1 To RSData.Numrad
    outstr = outstr + Format(RSData.Radius(i), "0.0") + ","
  Next i
  Print #fnum, outstr
  Print #fnum, CStr(RSData.RecNum)
#End If

  outstr = Format(RSData.RTX(1), "0.0") + "," + Format(RSData.RTX(2), "0.0")
  Print #fnum, outstr
  Print #fnum, CStr(RSData.SigParm)
  
#If PUFF Then
  outstr = CStr(RSData.NumX) + "," + CStr(RSData.NumY) + "," + Format(RSData.NodeDist * 1000, "0.0")
  Print #fnum, outstr
  
  Print #fnum, Format(RSData.MaxPufRad, "0.0")
  Print #fnum, Format(RSData.NumPufHr, "0.0")
  Print #fnum, Format(RSData.SigRCoef, "0.0")
  Print #fnum, Format(RSData.PufTimeStep, "0.0")
  If RSData.PufMergFlag = 0 Then
      Print #fnum, "F"
      Print #fnum, CStr(0)
  Else
      Print #fnum, "T"
      Print #fnum, LTrim(RTrim(RSData.PufMergDist))
  End If
  Print #fnum, Format(RSData.TracRegFac, "0.0")
  Print #fnum, Format(RSData.MinPufConc, "0.0")
#End If
  
#If PLUME Then
  If RSData.CalmFlag = 0 Then
    Print #fnum, "F"
  Else
    Print #fnum, "T"
    outstr = ""
    For i = 1 To RSData.RecNum
      outstr = outstr + Format(RSData.CalmDist(i), "0.0") + ","
    Next i
    Print #fnum, outstr
  End If
#End If

  Print #fnum, LTrim(RTrim(NumSrc))
  For i = 1 To NumSrc
    outstr = ""
    outstr1 = ""
    outstr2 = ""
    outstr3 = ""
    outstr = CStr(SourceData(i).Kind) + ","

#If PUFF Then
    outstr = outstr + Format(SourceData(i).X_val, "0.0") + ","
    outstr = outstr + Format(SourceData(i).Y_val, "0.0") + ","
#End If

#If PLUME Then
    outstr = outstr + Format(SourceData(i).Rad, "0.0") + ","
    outstr = outstr + Format(SourceData(i).Ang, "0.0") + ","
#End If

    outstr = outstr + Format(SourceData(i).Area_Hgt, "0.0") + ","
    If SourceData(i).Kind = 0 Then
      If SourceData(i).PlmFlg = 1 Then
        outstr = outstr + "T" + ","
        outstr = outstr + Format(SourceData(i).St_Rad, "0.0") + ","
        outstr = outstr + Format(SourceData(i).St_Flow, "0.0") + ","
        outstr = outstr + Format(SourceData(i).ExTemp, "0.0")
      Else
        outstr = outstr + "F" + ","
        outstr = outstr + "0,0,0"
      End If

#If PUFF Then
      outstr = outstr + "," + Format(SourceData(i).SigR, "0.0") + ","
      outstr = outstr + Format(SourceData(i).SigZ, "0.0")
      Print #fnum, outstr
    Else
      MsgBox "Invalid Source " + SourceData(i).Name + " for Puff Model - Can only handle Point Sources."
      End
    End If
#End If
    

#If PLUME Then
      If SourceData(i).DispFlg = 1 Then
        If SourceData(i).IscFlg = 1 Then
          If SourceData(i).IscWakeFlg = 1 Then
            outstr1 = "T" + ","
            outstr2 = ""
            outstr3 = ""
            For irec = 1 To RSData.RecNum
              outstr2 = outstr2 + Format(SourceData(i).BldHgt(irec), "0.0") + ","
              outstr3 = outstr3 + Format(SourceData(i).BldWdth(irec), "0.0") + ","
            Next irec
          Else
            outstr1 = "F" + ","
          End If
          If SourceData(i).LowBoundFlg = 1 Then
            outstr1 = outstr1 + "T" + ","
          Else
            outstr1 = outstr1 + "F" + ","
          End If
          If SourceData(i).BIDFlg = 1 Then
            outstr1 = outstr1 + "T" + ","
          Else
            outstr1 = outstr1 + "F" + ","
          End If
          outstr1 = outstr1 + "F,F"
        Else
          outstr1 = "F,F,F,"
          If SourceData(i).HghWndFlg = 1 Then
            outstr1 = outstr1 + "T" + ","
            outstr2 = ""
            outstr3 = ""
            For irec = 1 To RSData.RecNum
              outstr2 = outstr2 + Format(SourceData(i).BldArea(irec), "0.0") + ","
            Next irec
          Else
            outstr1 = outstr1 + "F" + ","
          End If
          If SourceData(i).LowWndFlg = 1 Then
            outstr1 = outstr1 + "T"
          Else
            outstr1 = outstr1 + "F"
          End If
        End If
      Else
      
        outstr1 = "F,F,F,F,F"
      End If
    Else
      outstr = outstr + "F,0,0,0"
      outstr1 = "F,F,F,F,F"
    End If
    Print #fnum, outstr
    Print #fnum, outstr1
    If outstr2 <> "" Then
      Print #fnum, outstr2
    End If
    If outstr3 <> "" Then
      Print #fnum, outstr3
    End If
#End If

    GetPartData i, outstr, outstr1, outstr2, outstr3
    
    Print #fnum, outstr
    If outstr = "T" Then
      Print #fnum, outstr1
      Print #fnum, outstr2
      Print #fnum, outstr3
    End If
  Next i

#If PLUME Then
  If RSData.SectorFlag = 1 Then
    Print #fnum, "T"
  Else
    Print #fnum, "F"
  End If
#End If
  
#If ACUTE Then
  outstr = CStr(RSData.St_Year) + ","
  outstr = outstr + CStr(RSData.St_Month) + ","
  outstr = outstr + CStr(RSData.St_Day) + ","
  outstr = outstr + CStr(RSData.St_Hour)
  Print #fnum, outstr
  
  If doe Then
      Print #fnum, RSData.MethodFlag
      outstr = ""
      For i = 1 To 16
        outstr = outstr + Format(RSData.SiteBoundary(i), "0.0") + ","
      Next i
      Print #fnum, outstr
      Print #fnum, RSData.UserSector
  End If
#End If

  Close fnum
End Sub

#If PUFF Then
Sub WriteDOM()
  Dim i As Long
  Dim j As Long
  Dim fnum As Long
  Dim mtext As String
  Dim outstr As String
  
  If z0Grid.GridFlag = 0 Then
    Exit Sub
  End If
  
  fnum = FreeFile
  Open LstData.DomFile For Output As fnum
  If Err.Number <> 0 Then
    mtext = "Error writting to listing file " + LstData.DomFile + Chr(10) + "Error is " + Error()
    put_val errfile, mtext
    put_line errfile
    close_csv errfile
    MsgBox mtext
    End
  End If
  For j = 1 To RSData.NumY
    outstr = ""
    For i = 1 To RSData.NumX
      outstr = outstr + LTrim(RTrim(z0Grid.GridVals(i, j))) + ","
    Next i
    Print #fnum, outstr
  Next j
  Close fnum
End Sub
#End If

