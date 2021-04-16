Attribute VB_Name = "Params"
Option Explicit
Option Compare Text

Dim dryVolume As Single

Global Modified As Boolean
Global ponded As Boolean
Global halfIndex As Integer

Global Const OPT_OFF = 0
Global Const OPT_REL = 1
Global Const OPT_FLX = 2
Global Const OPT_COMP = 3
Global Const OPT_EXT = 4
Global Const NOT_APP = "n/a"
Global Const GWHALF = 0
Global Const SLHALF = 1
Global Const SWHALF = 2

Dim temp As parmrec

'Global errfile As csv
'Global errflag As Boolean
'Global argv() As String
'Global argc As Integer
'Global FUIName As String        'the path and name of the run
'Global RunName As String        'the name of the parameter file
'Global RefName As String        'the name of the reference file
'Global SiteIdx As Integer      'the index to site model parms
Global GlyphIndex As Integer
Global GlyphName As String
Global SourceMedia As String

' references to structure elements
Global Const E_NAME = 1
Global Const E_SCREEN = 2
Global Const E_SEQ = 3
Global Const E_DTYPE = 4
Global Const E_CATEGORY = 5
Global Const E_INTLUNIT = 6
Global Const E_USERUNIT = 7
Global Const E_PTYPE = 8
Global Const E_ORIDE = 9
Global Const E_DEFVAL = 10
Global Const E_SSROW = 12
Global Const E_INTLVAL = 13
Global Const E_USERVAL = 14
Global Const E_ANNO = 15
Global Const E_RANGE = 16
Global Const E_SUBTYPE = 17
Global Const E_MODIFIED = 18
Global Const E_DB = 19
Global Const E_ALTID = 20
'Global Const E_UDF = 21
Global Const E_MIN = 22
Global Const E_MAX = 23
'Global Const E_INTLVAL_NOTNA = 24  ' distinguish between UNDEF_NA and valid value
Global Const E_SSCOL = 25
Global Const E_STATEVAR = 26
Global Const E_SPR = 27
Global Const E_DURATION = 28
Global Const E_ACTIVE = 29
Global Const E_INVALID = 30
Global Const E_REF = 31

Const MAX_PROGENY = 10
Const MAX_CAS_REF = 10

Type AllContamStruct
  cas As String
  name As String
  sol As String ' Double
  koc As String ' Double
  ghalf(2) As String
  kd As String ' Double
  type As Integer
  mw As Double
  num As Integer
  ctr(MAX_CAS_REF, 2) As Integer
End Type

Type ProgenyStruct
  cas As String
  name As String
  sol As String
  ghalf(2) As String
  koc As String ' Double
  kd As String ' Double
  type As Integer
  mw As Double
End Type

Type ContamStruct
  cas As String
  name As String
  sol As String
  ghalf(2) As String
  koc As String ' Double
  kd As String ' Double
  type As Integer
  mw As Double
  nds As Integer
  prog(MAX_PROGENY) As ProgenyStruct
End Type

Type ParamStruct
  pname As String
  siteIdx As Integer
  c1 As Integer
  c2 As Integer
  c3 As Integer
  c4 As Integer
  c5 As Integer
  ref As Integer
  value As String       ' intl units
  intlval As String
  intlUnit As String
  userVal As String
  userunit As String
  range       As String ' user units
  
  SubType     As String
  ssRow       As Integer
  ssCol       As Integer
  sprIndex    As Integer

' For User Define Contaminant Parameters...
' ContamId    As String
  Modified    As Integer
' udf         As Integer  ' from udf table

  duration    As Single   ' years
  fActive     As Integer
  invalid       As Boolean
End Type

Dim vptParam() As ParamStruct
Public vptCont() As ContamStruct
Public vptAllCont() As AllContamStruct
Public IsSecondarySource As Boolean

Function IsInputComplete(nTab%, pname$, msg$) As Boolean
Dim i As Integer
Dim np As Integer
Dim desc As String
Dim complete As Boolean
Dim valid As Boolean
Dim npath As Integer
Dim paramId As String
Dim ndx As Integer

  complete = True
  
  If nTab < 0 Then
    For i = 1 To 5
      Select Case i
        Case 1: paramId = "STINF_OP"
        Case 2: paramId = "STOVL_OP"
        Case 3: paramId = "STSUS_OP"
        Case 4: paramId = "STVOL_OP"
        Case 5: paramId = "STSRC_OP"
      End Select
      If GetParamElem(ndx, paramId, 0, 0, E_USERVAL, np) Then npath = npath + np
    Next
    If npath = 0 Then
      IsInputComplete = False
      msg = "At least one pathway must be active."
      Exit Function
    End If
  End If
  
  For i = 0 To UBound(ssTabCtl)
    If nTab < 0 Or i = nTab Then
      If IsTabEnabled(i) Then
        ssTabCtl(i).ReDraw = False
        For np = 1 To UBound(vptParam)
          If vptParam(np).sprIndex = i Then
           valid = True
            If vptParam(np).fActive Then
              valid = vptParam(np).userVal <> "" And Not vptParam(np).invalid
              If valid Then
                valid = CheckRange(vptParam(np).pname, vptParam(np).userVal, vptParam(np).range)
              End If
              If Not valid Then
                msg = msg & "Missing or invalid " & vptParam(np).pname & " value on " & frmMain.SSTab1.TabCaption(i) & " tab" & vbCrLf
              End If
            End If
            SetInputValid ssTabCtl(vptParam(np).sprIndex), vptParam(np).ssCol, vptParam(np).ssRow, np, valid
            If complete And Not valid Then
              pname = vptParam(np).pname
            End If
            complete = complete And valid
'           If vptParam(np).fActive And (vptParam(np).userVal = "" Or vptParam(np).invalid) Then
'             SetInputValid ssTabCtl(vptParam(np).sprIndex), vptParam(np).ssCol, vptParam(np).ssRow, np, False
'             If complete Then pname = vptParam(np).pname
'             complete = False
'           Else
'             SetInputValid ssTabCtl(vptParam(np).sprIndex), vptParam(np).ssCol, vptParam(np).ssRow, np, True
'           End If
          End If
        Next
        ssTabCtl(i).ReDraw = True
      End If
    End If
  Next
  IsInputComplete = complete
End Function


Function AddParamStruct(paramId$, c1%, c2%, c3%) As Integer
Dim ndx As Integer, ip As Integer
  ndx = 1 + UBound(vptParam)
  ReDim Preserve vptParam(ndx)
  vptParam(ndx).pname = paramId
  vptParam(ndx).c1 = c1
  vptParam(ndx).c2 = c2
  vptParam(ndx).c3 = c3
  vptParam(ndx).intlUnit = GetParamInfoElem(ip, paramId, E_INTLUNIT)
  vptParam(ndx).userunit = vptParam(ndx).intlUnit
  AddParamStruct = ndx
End Function

Function GetParamIndex(paramId$, c1%, c2%, c3%) As Integer
Dim i As Integer
Dim ndx As Integer

  ndx = 0
  For i = 1 To UBound(vptParam)
    If vptParam(i).pname = paramId And _
       vptParam(i).c1 = c1 And _
       vptParam(i).c2 = c2 And _
       vptParam(i).c3 = c3 Then
         ndx = i
         Exit For
    End If
  Next
  GetParamIndex = ndx
End Function
Function GetParamStruct(pStruct As ParamStruct) As Integer
Dim ndx As Integer
  ndx = GetParamIndex(pStruct.pname, pStruct.c1, pStruct.c2, pStruct.c3)
  pStruct = vptParam(ndx)
  GetParamStruct = ndx
End Function

Function IsParamIndex(Index%, paramId$, c1%, c2%, c3%) As Integer
  IsParamIndex = vptParam(Index).pname = paramId And _
    vptParam(Index).c1 = c1 And _
    vptParam(Index).c2 = c2 And _
    vptParam(Index).c3 = c3
End Function

Sub SaveParameters()
  Dim i As Integer, j As Integer, k As Integer, L As Integer, M As Integer
  Dim prm As String
  Dim prefix As String
  Dim fle As parmfile
  Dim sval As Boolean
  Dim numcon As Integer
  Dim np As Integer
  Dim UserFormat As String
  UserFormat = CVTFormat
  
  Dim nrec As Integer
  
  On Error Resume Next
  If "" <> Dir(RunName + ".gid") Then Kill (RunName + ".gid")
  
  If open_parm(fle, RunName + ".gid", 1) Then
    
    UpdatePathwayControlParameters
    
    ' save contaminant list
    temp.idx1 = siteIdx
    temp.idx2 = 0
    temp.idx3 = 0
    temp.idx4 = 0
    temp.idx5 = 0
    temp.idx6 = 0
    temp.ref = 0
    temp.uunit = ""
    temp.cunit = ""
    
    set_parm temp, "CVTFormat", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CVTFormat
    write_parmrec fle, temp
    CVTFormat = "General Number"

    temp.pname = "WARNED"
    temp.pval = True
    write_parmrec fle, temp
    
    temp.pname = "STCASID"
    For i = 1 To UBound(vptAllCont)
      temp.idx1 = siteIdx
      temp.idx2 = vptAllCont(i).ctr(1, 1)
      temp.idx3 = vptAllCont(i).ctr(1, 2)
      temp.pval = vptAllCont(i).cas
      write_parmrec fle, temp
    Next
     
    SaveSoilParameters fle
   
    For i = 0 To UBound(ssTabCtl)
    If IsTabEnabled(i) Then
      For np = 1 To UBound(vptParam)
        If vptParam(np).sprIndex = i Then
          If vptParam(np).userVal <> "" And Not vptParam(np).invalid Then
            temp.pname = vptParam(np).pname
            temp.idx1 = siteIdx
            temp.idx2 = vptParam(np).c1
            temp.idx3 = vptParam(np).c2
            temp.idx4 = vptParam(np).c3
            temp.idx5 = vptParam(np).c4
            temp.idx6 = vptParam(np).c5
            temp.ref = vptParam(np).ref
            temp.uunit = vptParam(np).userunit
            temp.cunit = vptParam(np).intlUnit
            If IsNumeric(vptParam(np).userVal) Then
              temp.pval = Val(vptParam(np).value)
              temp.pval = Val(ConvertUOM(vptParam(np).userVal, "", temp.uunit, temp.cunit)) ' 8/97 BLH
            Else
              temp.pval = vptParam(np).userVal
            End If
            write_parmrec fle, temp
            If temp.pname = "STSOL" _
              Or 0 <> InStr(temp.pname, "STKD") _
              Or temp.pname = "STGHALF" Then
'             Or 0 <> InStr(temp.pname, "STSOURCE") Then
              UpdateProgenyParameters fle, temp
            End If
            nrec = nrec + 1
          End If
        End If
      Next
    End If
    Next
    
    close_parm fle
    CVTFormat = UserFormat
    SetFormat frmMain
  Else
    put_val errfile, "Can't open file " & RunName + ".gid"
    put_line errfile
    close_csv errfile
  End If

End Sub

Sub SaveSoilParameters(prmFile As parmfile)
  Dim sand As String
  Dim silt As String
  Dim clay As String
  Dim omc As String
  Dim ph As String
  Dim iron As String
  Dim sum As Double
  Dim i As Integer
  
  GetParamElem i, "KDSAND", 0, 0, E_USERVAL, sand
  GetParamElem i, "KDSILT", 0, 0, E_USERVAL, silt
  GetParamElem i, "KDCLAY", 0, 0, E_USERVAL, clay
  GetParamElem i, "KDOMC", 0, 0, E_USERVAL, omc
  GetParamElem i, "KDIRON", 0, 0, E_USERVAL, iron
  GetParamElem i, "KDPH", 0, 0, E_USERVAL, ph
  
  sum = CDbl(0 & sand) + CDbl(0 & silt) + CDbl(0 & clay) + CDbl(0 & omc) + CDbl(0 & iron)
  
  If sum > 99.9 And sum < 100.1 Then
    set_parm temp, "KDSAND", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", sand
    write_parmrec prmFile, temp
    
    set_parm temp, "KDSILT", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", silt
    write_parmrec prmFile, temp
    
    set_parm temp, "KDCLAY", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", clay
    write_parmrec prmFile, temp
    
    set_parm temp, "KDOMC", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", omc
    write_parmrec prmFile, temp
    
    set_parm temp, "KDIRON", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", iron
    write_parmrec prmFile, temp
    
  End If
  
  If CDbl(0 & ph) >= 1 And CDbl(0 & ph) <= 14 Then
    set_parm temp, "KDPH", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", ph
    write_parmrec prmFile, temp
    
  End If

End Sub

Function SetParamStruct(pStruct As ParamStruct) As Integer
Dim ndx As Integer
  ndx = GetParamIndex(pStruct.pname, pStruct.c1, pStruct.c2, pStruct.c3)
  If ndx = 0 Then
    ndx = AddParamStruct(pStruct.pname, pStruct.c1, pStruct.c2, pStruct.c3)
  End If
  vptParam(ndx).ref = pStruct.ref
  vptParam(ndx).value = pStruct.value
  vptParam(ndx).intlval = pStruct.intlval
  vptParam(ndx).intlUnit = pStruct.intlUnit
  vptParam(ndx).userVal = pStruct.userVal
  vptParam(ndx).userunit = pStruct.userunit
  vptParam(ndx).SubType = pStruct.SubType
  vptParam(ndx).ssRow = pStruct.ssRow
  vptParam(ndx).ssCol = pStruct.ssCol
  vptParam(ndx).sprIndex = pStruct.sprIndex
  vptParam(ndx).fActive = pStruct.fActive
  SetParamStruct = ndx
End Function

Sub SetParamElem(Index%, paramId$, c1%, c2%, Elem%, value)
  SetParamElem3 Index, paramId, c1, c2, 0, Elem, value
End Sub

Function GetParamElem(Index%, paramId$, c1%, c2%, Elem%, value As Variant) As Boolean
  GetParamElem = GetParamElem3(Index, paramId, c1, c2, 0, Elem, value)
End Function

Sub InitParamStruct()
  ReDim vptParam(0)
End Sub


Sub LoadParameters()
  Dim i As Integer, j As Integer, k As Integer, L As Integer, M As Integer
  'Dim prm As String
  Dim prefix As String
  Dim fle As parmfile
  Dim sval As Boolean
  Dim numcon As Integer
  Dim np As Integer
  Dim nrec As Integer
  Dim pvalue As String
  Dim nds As Integer
  Dim n As Long
  Dim remap() As Integer
  Dim sol As String
  Dim prm As parmrec
  Dim CsmIndex As Long
  Dim lrec As Long
  Dim mepas As Boolean
  
  mepas = False
  If open_parm(fle, FUIName, 2) Then
    LoadSection fle, "CSM"
    find_frst fle, prm, "ModId"
    Do While prm.pval <> GlyphName
      find_next fle, prm
    Loop
    CsmIndex = prm.idx2
    ReadLng fle, lrec, "ModSrcNum", siteIdx, CsmIndex
    For n = 1 To lrec
      ReadStr fle, prm.pval, "ModSrcType", siteIdx, CsmIndex, n
      If prm.pval = "ato" Or prm.pval = "wff" Then
        IsSecondarySource = True
        Exit For
      End If
    Next n
    close_parm fle
  End If

  
  If open_parm(fle, FUIName, 2) Then
    Do Until EOCF(fle.file)
      If read_parmrec(fle, temp) Then
        Select Case temp.pname
          Case "fui"
            ReDim vptCont(0)
            nrec = temp.idx1
            For M = 1 To nrec
              If read_parmrec(fle, temp) Then
                If temp.idx1 = siteIdx Then
                  Select Case temp.pname
                    Case "numcon"
                      numcon = Val(temp.pval)
                      ReDim Preserve vptCont(numcon)
                    Case "clktype", "clsol", "fscasid", "fscname", "nds", "clkoc", "clkd", "clwm", _
                         "clghalf", "clshalf", "clwhalf"
                      numcon = Val(temp.idx2)
                      If numcon > UBound(vptCont) Then ReDim Preserve vptCont(numcon)
                      Select Case temp.pname
                        Case "clktype"
                          If temp.idx3 = 0 Then
                            vptCont(numcon).type = Val(temp.pval)
                          Else
                            vptCont(numcon).prog(temp.idx3).type = Val(temp.pval)
                          End If
                        Case "clwm"
                          If temp.idx3 = 0 Then
                            vptCont(numcon).mw = Val(temp.pval)
                          Else
                            vptCont(numcon).prog(temp.idx3).mw = Val(temp.pval)
                          End If
                        Case "clsol"
                          If temp.idx3 = 0 Then
                            vptCont(numcon).sol = temp.pval
                          Else
                            vptCont(numcon).prog(temp.idx3).sol = temp.pval
                          End If
                        Case "clghalf", "clwhalf", "clshalf"
                          setHalfTime numcon, temp
                        Case "clkoc"
                          If temp.idx3 = 0 Then
                            vptCont(numcon).koc = temp.pval
                          Else
                            vptCont(numcon).prog(temp.idx3).koc = temp.pval
                          End If
                        Case "clkd"
                          If temp.idx3 = 0 Then
                            vptCont(numcon).kd = temp.pval
                          Else
                            vptCont(numcon).prog(temp.idx3).kd = temp.pval
                          End If
                        Case "fscasid"
                          If temp.idx3 = 0 Then
                            vptCont(numcon).cas = temp.pval
                          Else
                            vptCont(numcon).prog(temp.idx3).cas = temp.pval
                          End If
                        Case "fscname"
                          If temp.idx3 = 0 Then
                            vptCont(numcon).name = temp.pval
                          Else
                            vptCont(numcon).prog(temp.idx3).name = temp.pval
                          End If
                        Case "nds"
                          If temp.idx3 = 0 Then
                            vptCont(numcon).nds = Val(temp.pval)
                          End If
                      End Select
                  End Select
                End If
              End If
            Next
            ' construct vptAllCont
            ReDim vptAllCont(UBound(vptCont))
            For M = 1 To UBound(vptCont)
              vptAllCont(M).cas = vptCont(M).cas
              vptAllCont(M).name = vptCont(M).name
              vptAllCont(M).sol = vptCont(M).sol
              For i = GWHALF To SWHALF
                vptAllCont(M).ghalf(i) = vptCont(M).ghalf(i)
              Next
              vptAllCont(M).koc = vptCont(M).koc
              vptAllCont(M).kd = vptCont(M).kd
              vptAllCont(M).type = vptCont(M).type
              vptAllCont(M).num = 1
              vptAllCont(M).ctr(vptAllCont(M).num, 1) = M
            Next
            For M = 1 To UBound(vptCont)
              For nds = 1 To vptCont(M).nds
                For n = 1 To UBound(vptAllCont)
                  If vptAllCont(n).cas = vptCont(M).prog(nds).cas Then Exit For
                Next
                If n > UBound(vptAllCont) Then
                  ReDim Preserve vptAllCont(n)
                  vptAllCont(n).cas = vptCont(M).prog(nds).cas
                  vptAllCont(n).name = "* " & vptCont(M).prog(nds).name
                  vptAllCont(n).sol = vptCont(M).prog(nds).sol
                  vptAllCont(n).koc = vptCont(M).prog(nds).koc
                  For i = GWHALF To SWHALF
                    vptAllCont(n).ghalf(i) = vptCont(M).prog(nds).ghalf(i)
                  Next
                  vptAllCont(n).kd = vptCont(M).prog(nds).kd
                  vptAllCont(n).type = vptCont(M).prog(nds).type
                End If
                vptAllCont(n).num = vptAllCont(n).num + 1
                vptAllCont(n).ctr(vptAllCont(n).num, 1) = M
                vptAllCont(n).ctr(vptAllCont(n).num, 2) = nds
              Next
            Next
          Case LCase(GlyphName)
            nrec = temp.idx1
            ReDim vptParam(nrec)
            ReDim remap(0)
            For M = 1 To nrec
              If read_parmrec(fle, temp) Then
                If temp.pname = "CVTFormat" Then
                  CVTFormat = "General Number" 'CVTFormat = temp.pval
                  mepas = True
                End If
                ' handle unit change for existing gid files
                If temp.pname = "STCLOUD" Then
                  If temp.uunit = "tenths" Then
                    temp.uunit = "fraction"
                    temp.cunit = "fraction"
                    temp.pval = CStr(0.1 * Val(temp.pval))
                  End If
                End If
                If temp.uunit = "m/s" Then temp.uunit = "m/sec"
                If temp.cunit = "m/s" Then temp.cunit = "m/sec"
                vptParam(M).pname = temp.pname
                vptParam(M).siteIdx = temp.idx1
                vptParam(M).c1 = temp.idx2
                vptParam(M).c2 = temp.idx3
                vptParam(M).c3 = temp.idx4
                vptParam(M).c4 = temp.idx5
                vptParam(M).c5 = temp.idx6
                vptParam(M).ref = temp.ref
                If (temp.uunit <> NOT_APP) Then vptParam(M).userunit = temp.uunit
                If (temp.cunit <> NOT_APP) Then vptParam(M).intlUnit = temp.cunit
                vptParam(M).value = temp.pval
                vptParam(M).intlval = temp.pval
                vptParam(M).userVal = ConvertUOM(temp.pval, "", temp.cunit, temp.uunit)
                If vptParam(M).userVal = "" And ("CSM" <> vptParam(M).pname) And mepas Then
                  MsgBox "Error: UserValue is empty " & vptParam(M).pname
                End If
                vptParam(M).sprIndex = -1
              End If
            Next
          Case Else
            nrec = temp.idx1
            For M = 1 To nrec
              get_line fle.file
            Next
        End Select
      End If
    Loop
    close_parm fle
    SetFormat frmMain
    
  Else
    put_val errfile, "Can't find or open file " & FUIName
    put_line errfile
    close_csv errfile
    End
  End If

End Sub


Public Function EnableParam(paramId$) As Boolean
Dim enable As Integer
Dim ndx As Integer
Dim pszValue As String
Dim sRc As Integer

  Select Case paramId
    Case "STATYPE"
'     enable = OpValue("STVOL_OP", "flux") Or OpValue("STSUS_OP", "flux")
      enable = OpValue("STVOL_OP", OPT_FLX) Or OpValue("STSUS_OP", OPT_FLX)
    Case "STRELHT", "STSTRUCHT", "STEXVEL", "STEXTEMP"
      sRc = GetParamElem(ndx, "STATYPE", 0, 0, E_USERVAL, pszValue)
      enable = 0 <> InStr(pszValue, "point")
    Case "STROADS"
      sRc = GetParamElem(ndx, "STROADS", 0, 0, E_USERVAL, pszValue)
      enable = (pszValue <> "none")
  End Select
  EnableParam = enable
End Function

Sub UpdatePathwayControlParameters()
Dim i As Integer
Dim paramId As String
Dim rc As Integer
Dim ndx As Integer
Dim intval As String
Dim ip As Integer
Dim category As String
Dim strval As String
Dim nc As Integer
Dim maxtime As String
Dim params() As String
Dim numcon As Integer, c1 As Integer, c2 As Integer
Dim media As String
Dim optVal As Integer
Dim ctr(3) As Integer

    rc = GetParamElem(ndx, "STMEDIA", 0, 0, E_USERVAL, media)
    rc = GetParamElem(ndx, "STMAXTIME", 0, 0, E_USERVAL, maxtime)

    rc = GetParamElem(ndx, "STREMSITE", 0, 0, E_USERVAL, intval)
    If (Not rc) Or (BOOL_NO = intval) Then
      For i = 1 To 5
        Select Case i
          Case 1: paramId = "STCLEAN"
          Case 2: paramId = "STTOTPOR"
          Case 3: paramId = "STAIRSPC"
          Case 4: paramId = "STSURFVOL"
          Case 5: paramId = "STGLASS"
        End Select
        If Not (0 < InStr(media, "surface") And (paramId = "STTOTPOR" Or paramId = "STAIRSPC")) Then
          SetPathwayParam ndx, paramId + "_NUM", 0, 0, 0, "1", 1
          SetPathwayParam ndx, paramId + "_TIM", 1, 0, 0, maxtime, 1
        End If
        If paramId = "STSURFVOL" Or paramId = "STGLASS" Then
          SetPathwayParam ndx, paramId, 1, 0, 0, "0", 1
        End If
      Next
    End If

    ' update site pathway parameters
    For i = 1 To 5
      Select Case i
        Case 1: paramId = "STINF_OP"
        Case 2: paramId = "STOVL_OP"
        Case 3: paramId = "STSUS_OP"
        Case 4: paramId = "STVOL_OP"
        Case 5: paramId = "STSRC_OP"
      End Select
      rc = GetParamElem(ndx, paramId, 0, 0, E_USERVAL, intval)
      optVal = Val(intval)
      category = GetParamInfoElem(ip, paramId, E_CATEGORY)
      strval = ListStructText(category, CStr(intval))
      
      ctr(1) = 0
      ctr(2) = 0
      ctr(3) = 0
      
      Select Case i
        Case 1: paramId = "STINF_OP"
          If InStr(strval, "compute") Then
            SetPathwayParam2 ndx, "STDARCY", ctr, 1, 0, maxtime, 0
          ElseIf (0 = optVal) Then 'off
            SetPathwayParam2 ndx, "STDARCY", ctr, 1, 2, maxtime, 0
          End If
        Case 2: paramId = "STOVL_OP"
          If InStr(media, "surface") Then
            SetPathwayParam2 ndx, "STWATR", ctr, 1, 2, maxtime, 0
          Else
            If InStr(strval, "compute") Then
              SetPathwayParam2 ndx, "STWATR", ctr, 1, 0, maxtime, 0
            ElseIf (0 = optVal) Then ' off
              SetPathwayParam2 ndx, "STWATR", ctr, 1, 2, maxtime, 0
            End If
          End If
        Case 3: paramId = "STSUS_OP"
          If InStr(strval, "compute") Then
           SetPathwayParam2 ndx, "STWIND", ctr, 1, 0, maxtime, 0
          ElseIf InStr(strval, "off") Then
            SetPathwayParam2 ndx, "STWIND", ctr, 1, 2, maxtime, 0
          End If
        Case 4: paramId = "STVOL_OP"
'           SetPathwayParam2 ndx, "STWIND", ctr, 1, 0, maxtime, 0
        
        Case 5: paramId = "STSRC_OP"
'           SetPathwayParam2 ndx, "STWIND", ctr, 1, 0, maxtime, 0
      End Select
    Next
    
    ' update contaminant pathway parameters
    For i = 1 To 5
      numcon = UBound(vptCont)
      Select Case i
        Case 1: paramId = "STINF_OP"
        Case 2: paramId = "STOVL_OP"
        Case 3: paramId = "STSUS_OP"
        Case 4: paramId = "STVOL_OP"
        Case 5: paramId = "STSRC_OP": numcon = UBound(vptAllCont)
      End Select
      rc = GetParamElem(ndx, paramId, 0, 0, E_USERVAL, intval)
      optVal = Val(intval)
      category = GetParamInfoElem(ip, paramId, E_CATEGORY)
      strval = ListStructText(category, CStr(intval))
      For nc = 1 To numcon
        c1 = vptAllCont(nc).ctr(1, 1)
        c2 = vptAllCont(nc).ctr(1, 2)
        
        ctr(1) = c1
        ctr(2) = c2
        ctr(3) = 0
        Select Case i
          Case 1: paramId = "STINF_OP"
            If InStr(strval, "compute") Or (optVal = OPT_REL) Then
              SetPathwayParam2 ndx, "STLEACH", ctr, 3, 0, maxtime, 0
            ElseIf (optVal = 0) Then ' off
              SetPathwayParam2 ndx, "STLEACH", ctr, 3, 2, maxtime, 0
            End If
          Case 2: paramId = "STOVL_OP"
            If InStr(strval, "compute") Or (optVal = OPT_REL) Then
              SetPathwayParam2 ndx, "STOVL", ctr, 3, 0, maxtime, 0
            ElseIf (optVal = 0) Then 'off
              SetPathwayParam2 ndx, "STOVL", ctr, 3, 2, maxtime, 0
            End If
          Case 3: paramId = "STSUS_OP"
            If InStr(strval, "compute") Or (optVal = OPT_REL) Then
              SetPathwayParam2 ndx, "STSUSP", ctr, 3, 0, maxtime, 0
              SetPathwayParam ndx, "STSUSP_NUM", c1, c2, 0, "0", 0
            ElseIf (optVal = 0) Then ' off
              SetPathwayParam2 ndx, "STSUSP", ctr, 3, 2, maxtime, 0
            End If
          Case 4: paramId = "STVOL_OP"
            If InStr(strval, "compute") Or (optVal = OPT_REL) Then
              SetPathwayParam2 ndx, "STVOLAT", ctr, 3, 0, maxtime, 0
            ElseIf (optVal = 0) Then ' off
              SetPathwayParam2 ndx, "STVOLAT", ctr, 3, 2, maxtime, 0
            End If
          Case 5: paramId = "STSRC_OP"
            If (optVal = OPT_OFF Or optVal = OPT_EXT) Then ' off or external/secondary source
             SetPathwayParam2 ndx, "STSOURCE", ctr, 3, 2, maxtime, 0
            End If
        End Select
      Next nc
    Next i

End Sub

Sub SetPathwayParam2(ndx%, paramId$, ctr() As Integer, cp%, count%, maxtime, sprIndex%)
Dim pId As String

  ctr(cp) = 0
  pId = paramId & "_NUM"
  SetParamElem3 ndx, pId, ctr(1), ctr(2), ctr(3), E_USERVAL, count
  SetParamElem3 ndx, pId, ctr(1), ctr(2), ctr(3), E_SPR, sprIndex
  SetParamElem3 ndx, pId, ctr(1), ctr(2), ctr(3), E_INVALID, False
  If count = 0 Then Exit Sub
  
  ctr(cp) = 1
  pId = paramId
  SetParamElem3 ndx, pId, ctr(1), ctr(2), ctr(3), E_USERVAL, 0
  SetParamElem3 ndx, pId, ctr(1), ctr(2), ctr(3), E_SPR, sprIndex
  SetParamElem3 ndx, pId, ctr(1), ctr(2), ctr(3), E_INVALID, False
  pId = paramId & "_TIM"
  SetParamElem3 ndx, pId, ctr(1), ctr(2), ctr(3), E_USERVAL, 0
  SetParamElem3 ndx, pId, ctr(1), ctr(2), ctr(3), E_SPR, sprIndex
  SetParamElem3 ndx, pId, ctr(1), ctr(2), ctr(3), E_INVALID, False
  
  ctr(cp) = 2
  pId = paramId
  SetParamElem3 ndx, pId, ctr(1), ctr(2), ctr(3), E_USERVAL, 0
  SetParamElem3 ndx, pId, ctr(1), ctr(2), ctr(3), E_SPR, sprIndex
  SetParamElem3 ndx, pId, ctr(1), ctr(2), ctr(3), E_INVALID, False
  pId = paramId & "_TIM"
  SetParamElem3 ndx, pId, ctr(1), ctr(2), ctr(3), E_USERVAL, maxtime
  SetParamElem3 ndx, pId, ctr(1), ctr(2), ctr(3), E_SPR, sprIndex
  SetParamElem3 ndx, pId, ctr(1), ctr(2), ctr(3), E_INVALID, False
End Sub

Sub SetPathwayParam(ndx%, paramId$, c1%, c2%, c3%, value, sprIndex%)
  SetParamElem3 ndx, paramId, c1, c2, c3, E_USERVAL, CStr(value)
  SetParamElem3 ndx, paramId, c1, c2, c3, E_SPR, sprIndex
End Sub

Sub ResetParamsSprIndex(sprIndex%)
Dim i As Integer
  For i = 1 To UBound(vptParam)
    If vptParam(i).sprIndex = sprIndex Then
      vptParam(i).sprIndex = -1
      vptParam(i).fActive = False
    End If
  Next
End Sub

Sub ActivateSubtypes(sprIndex%, pId$, c1%, c2%, enable%)
Dim i As Integer, rc As Integer
Dim sRow As Integer, sCol As Integer
Dim intlval As String, userVal As String
Dim intlUnit As String, userunit As String
Dim ssVar

  For i = 1 To UBound(vptParam)
    If vptParam(i).SubType = pId And vptParam(i).c1 = c1 And vptParam(i).c2 = c2 Then
      sRow = vptParam(i).ssRow
      ssTabCtl(sprIndex).Row = sRow
      ssTabCtl(sprIndex).col = SS_VAL
      ssTabCtl(sprIndex).Lock = Not enable
      ssTabCtl(sprIndex).col = SS_UOM
      If ssTabCtl(sprIndex).CellType = SS_CELL_TYPE_COMBOBOX Then
        ssTabCtl(sprIndex).Lock = Not enable
        If enable Then
          ssTabCtl(sprIndex).BackColor = QBColor(15) ' white
        Else
          ssTabCtl(sprIndex).BackColor = LOCK_BACK_COLOR
        End If
      End If
      If enable Then
        vptParam(i).sprIndex = sprIndex
        intlval = vptParam(i).value
        intlUnit = vptParam(i).intlUnit
        userunit = intlUnit
        If ssTabCtl(sprIndex).GetText(SS_UOM, sRow, ssVar) Then userunit = ssVar
        userVal = ConvertUOM(intlval, "", intlUnit, userunit)
      Else
        vptParam(i).sprIndex = -1
        userVal = ""
      End If
      vptParam(i).fActive = enable
      ssTabCtl(sprIndex).SetText SS_VAL, sRow, CStr(userVal)
    End If
  Next
End Sub


Function GetParamElem3(Index%, paramId$, c1%, c2%, c3%, Elem%, value As Variant) As Boolean
Dim userunit As String
Dim category As String
Dim intlUnit As String
Dim ip As Integer

  On Error GoTo GetParamElem3_Error

  If Index > 0 And Index <= UBound(vptParam) Then
    If Not IsParamIndex(Index, paramId, c1, c2, c3) Then
      Index = GetParamIndex(paramId, c1, c2, c3)
    End If
  Else
    Index = GetParamIndex(paramId, c1, c2, c3)
  End If
  If Index <= 0 Then
    GetParamElem3 = False
  Else
    GetParamElem3 = True
    Select Case Elem
      Case E_USERVAL ' E_INTLVAL
'       If "" = vptParam(Index).value Then
        If "" = vptParam(Index).userVal Then
          value = ""
          GetParamElem3 = False
        Else
          Select Case VarType(value)
            Case vbString
              If Index > 0 Then value = vptParam(Index).userVal Else value = "0"
            Case Else
              If Index > 0 Then ' And Not Undefined Then
                value = vptParam(Index).userVal
              Else
                value = "0"
              End If
          End Select
        End If
'     Case E_USERVAL
'       If Index > 0 Then
'         userUnit = vptParam(Index).userUnit
'         intlUnit = GetParamInfoElem(ip, ParamId, E_INTLUNIT)
'         category = GetParamInfoElem(ip, ParamId, E_CATEGORY)
'         value = ConvertUOM(vptParam(Index).value, category, intlUnit, userUnit)
'       Else
'         value = "0"
'       End If
'       If Index > 0 Then value = vptParam(Index).userVal Else value = "0"
      Case E_INTLVAL
        value = ConvertUOM(vptParam(Index).userVal, "", vptParam(Index).userunit, vptParam(Index).intlUnit)
      Case E_INTLUNIT
        value = vptParam(Index).intlUnit
        If value = "" Then
          value = GetParamInfoElem(ip, paramId, E_INTLUNIT)
        End If
      Case E_USERUNIT
        value = vptParam(Index).userunit
      Case E_RANGE
        value = vptParam(Index).range
      Case E_SUBTYPE
        value = vptParam(Index).SubType
      Case E_SSCOL
        value = vptParam(Index).ssCol
      Case E_SSROW
        value = vptParam(Index).ssRow
      Case E_SPR
        value = vptParam(Index).sprIndex
      Case E_DURATION
        value = vptParam(Index).duration
      Case E_ACTIVE
        value = vptParam(Index).fActive
      Case E_INVALID
        value = vptParam(Index).invalid
      Case E_REF
        value = Val(vptParam(Index).ref)
      Case Else
        GetParamElem3 = False
    End Select
  End If
GetParamElem3_Exit:
  Exit Function
GetParamElem3_Error:
  GetParamElem3 = False
  Resume GetParamElem3_Exit
End Function

Sub SetParamElem3(Index%, paramId$, c1%, c2%, c3%, Elem%, value)
  
  If Index > 0 And Index <= UBound(vptParam) Then
    If Not IsParamIndex(Index, paramId, c1, c2, c3) Then
      Index = GetParamIndex(paramId, c1, c2, c3)
    End If
  Else
    Index = GetParamIndex(paramId, c1, c2, c3)
  End If
  If Index <= 0 Then
    Index = AddParamStruct(paramId, c1, c2, c3)
  End If

  Select Case Elem
'   Case E_SEQ
'     vptParam(Index).count = Value
'   Case E_INTLVAL
'     If vptParam(Index).value <> CStr(value) Then
'       vptParam(Index).value = value
'       vptParam(Index).Modified = True
'     End If
'     vptParam(Index).intlVal = vptParam(Index).value
'     vptParam(Index).userVal = _
'       ConvertUOM(vptParam(Index).intlVal, _
'         vptParam(Index).intlUnit, vptParam(Index).userUnit)
    Case E_USERVAL
      If vptParam(Index).userVal <> CStr(value) Then
        vptParam(Index).userVal = value
        vptParam(Index).Modified = True
      End If
'     vptParam(Index).intlVal = vptParam(Index).value
'     vptParam(Index).userVal = _
'       ConvertUOM(vptParam(Index).intlVal, _
'         vptParam(Index).intlUnit, vptParam(Index).userUnit)
    Case E_INTLUNIT
      vptParam(Index).intlUnit = value
    Case E_USERUNIT
      If vptParam(Index).userunit <> CStr(value) Then
        vptParam(Index).userunit = value
        vptParam(Index).Modified = True
      End If
    Case E_RANGE
    
    
      vptParam(Index).range = value
    Case E_SUBTYPE
      vptParam(Index).SubType = value
    Case E_SSROW
      vptParam(Index).ssRow = value
    Case E_SPR
      vptParam(Index).sprIndex = value
    Case E_SSCOL
      vptParam(Index).ssCol = value
    Case E_DURATION
      vptParam(Index).duration = value
    Case E_ACTIVE
      vptParam(Index).fActive = value
    Case E_MODIFIED
      vptParam(Index).Modified = value
    Case E_INVALID
      vptParam(Index).invalid = value
    Case E_REF
      vptParam(Index).ref = value
  End Select
End Sub

Sub UpdateProgenyParameters(fle As parmfile, temp As parmrec)
Dim i As Integer, j As Integer

' temp.pname = vptParam(np).pname
' temp.idx1 = SiteIdx
' temp.idx2 = vptParam(np).c1
' temp.idx3 = vptParam(np).c2
' temp.idx4 = vptParam(np).c3

  For i = 1 To UBound(vptAllCont)
    If temp.idx2 = vptAllCont(i).ctr(1, 1) And temp.idx3 = vptAllCont(i).ctr(1, 2) Then
      If vptAllCont(i).num > 1 Then
        For j = 2 To vptAllCont(i).num
          temp.idx2 = vptAllCont(i).ctr(j, 1)
          temp.idx3 = vptAllCont(i).ctr(j, 2)
          write_parmrec fle, temp
        Next
      End If
    End If
  Next
End Sub

Sub AdjustContamParams()
Dim i As Integer, j As Integer, k As Integer, nc As Integer
Dim prm As String
Dim map() As Integer
Dim str As String
  'resolve contaminate differences
  ReDim map(10, 0)
  
  For i = 1 To UBound(vptParam)
    prm = vptParam(i).pname
    If prm = "STCASID" Then
      If vptParam(i).c1 > UBound(map, 2) Then ReDim Preserve map(10, vptParam(i).c1)
      For j = 1 To UBound(vptAllCont)
        If vptAllCont(j).cas = vptParam(i).value Then
          map(vptParam(i).c2, vptParam(i).c1) = j
          Exit For
        End If
      Next
    ElseIf prm = "STSOL" Or prm = "STVOLRAT" _
        Or prm = "STKD" Or prm = "STKD_NUM" Or prm = "STKD_TIM" _
        Or prm = "STLEACH" Or prm = "STLEACH_NUM" Or prm = "STLEACH_TIM" _
        Or prm = "STOVL" Or prm = "STOVL_NUM" Or prm = "STOVL_TIM" _
        Or prm = "STSUSP" Or prm = "STSUSP_NUM" Or prm = "STSUSP_TIM" _
        Or prm = "STVOLAT" Or prm = "STVOLAT_NUM" Or prm = "STVOLAT_TIM" _
        Or prm = "STSOURCE" Or prm = "STSOURCE_NUM" Or prm = "STSOURCE_TIM" _
        Or prm = "STGHALF" _
        Or prm = "STINVEN" Or prm = "STCONC" Or prm = "STCONCTYPE" Then
        If vptParam(i).c1 <= UBound(map, 2) Then
          j = map(vptParam(i).c2, vptParam(i).c1)
          vptParam(i).c1 = vptAllCont(j).ctr(1, 1)
          vptParam(i).c2 = vptAllCont(j).ctr(1, 2)
        End If
        ' BLH 03/09/2001  -- this is a bug fix for units in the gid file
        '   "g/yr or Ci/yr" that cause problems for SUM^3
        If InStr(vptParam(i).userunit, " or ") Then
          If vptParam(i).userunit = "g or Ci" Then
            str = IIf(1 = vptAllCont(j).type, "Ci", "g")
          Else
            str = IIf(1 = vptAllCont(j).type, "Ci/yr", "g/yr")
          End If
          vptParam(i).userunit = str
          vptParam(i).intlUnit = str
        End If
    End If
    
  Next

End Sub

Sub SetInputValid(spr As Control, col, Row, ndx%, valid As Boolean)
  spr.Row = Row
  spr.col = col
  If Not valid Then
    spr.BackColor = ERR_RED
  Else
    If spr.CellType <> SS_CELL_TYPE_COMBOBOX Then spr.BackColor = INP_GRN
  End If
  vptParam(ndx).invalid = Not valid
End Sub

Sub SetParamElemSprAttr(ndx%, pId$, c1%, c2%, c3%, sprIndex%, col%, Row%, active As Boolean)
  SetParamElem3 ndx, pId, c1, c2, c3, E_SPR, sprIndex
  SetParamElem3 ndx, pId, c1, c2, c3, E_SSCOL, col
  SetParamElem3 ndx, pId, c1, c2, c3, E_SSROW, Row
  SetParamElem3 ndx, pId, c1, c2, c3, E_ACTIVE, active
End Sub

Sub setDryVolume()
Dim idx As Integer
Dim pvalue As String

  dryVolume = 0#
  If GetParamElem(idx, "STTHICK", 0, 0, E_INTLVAL, pvalue) Then dryVolume = Val(pvalue)
  If GetParamElem(idx, "STLENGTH", 0, 0, E_INTLVAL, pvalue) Then dryVolume = dryVolume * Val(pvalue)
  If GetParamElem(idx, "STWIDTH", 0, 0, E_INTLVAL, pvalue) Then dryVolume = dryVolume * Val(pvalue)
  If GetParamElem(idx, "STZBULKD", 0, 0, E_INTLVAL, pvalue) Then dryVolume = dryVolume * Val(pvalue)
  
End Sub

Sub setHalfTime(numcon As Integer, temp As parmrec)
  Dim ghalf As String, idx As Integer
  Select Case temp.pname
    Case "CLGHALF":  idx = GWHALF
    Case "CLSHALF":  idx = SLHALF
    Case "CLWHALF":  idx = SWHALF
  End Select
  If 0 = Val(temp.pval) Then ghalf = "" Else ghalf = temp.pval
  If temp.idx3 = 0 Then
    vptCont(numcon).ghalf(idx) = ghalf
  Else
    vptCont(numcon).prog(temp.idx3).ghalf(idx) = ghalf
  End If
End Sub

