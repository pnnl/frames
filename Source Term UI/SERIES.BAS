Attribute VB_Name = "Series"
Option Explicit

' Constants for Kd value lookup
Global Const SELECT_LOOKUP = 0
Global Const DATABASE_LOOKUP = 1
Global Const EQUATION_LOOKUP = 2
Global Const TABLE_LOOKUP = 3



Function getEstKdDesc(kdEstimateType%) As String
  ' Fail safe return value
  getEstKdDesc = ""
  Select Case kdEstimateType
    Case DATABASE_LOOKUP  ' Database value
      getEstKdDesc = "Database Value"
    Case EQUATION_LOOKUP  ' Equation Estimator value
      getEstKdDesc = "Estimated Value"
    Case TABLE_LOOKUP  ' Lookup Table value
      getEstKdDesc = "Lookup Table Value"
  End Select

End Function

Function getKd(conName$, kdEstimateType%, Optional progName$ = "")
  Dim i As Integer
  Dim sand As String
  Dim silt As String
  Dim clay As String
  Dim omc As String
  
  ' Fail safe return value
  getKd = -1
  Select Case kdEstimateType
    Case DATABASE_LOOKUP  ' Database value
      getKd = Kd_DatabaseValue(conName, progName)
    Case EQUATION_LOOKUP  ' Equation Estimator value
      GetParamElem i, "KDSAND", 0, 0, E_USERVAL, sand
      GetParamElem i, "KDSILT", 0, 0, E_USERVAL, silt
      GetParamElem i, "KDCLAY", 0, 0, E_USERVAL, clay
      GetParamElem i, "KDOMC", 0, 0, E_USERVAL, omc
      
      getKd = Kd_EquationValue(conName, omc, clay, silt, sand, progName)
    Case TABLE_LOOKUP  ' Lookup Table value
      getKd = Kd_LookupTableValue(conName, progName)
  End Select
End Function

Function SprChangeToContamSeries(sprIndex%, col%, Row%) As Integer
Dim sRc As Integer
Dim sSeq As Integer
Dim paramId As Integer
Dim sInt As Integer
Dim sRow As Integer
Dim nextId As Integer
Dim numRates As Integer
Dim i As Integer
Dim maxtime As Single
Dim CurrRate As Single
Dim CurrVal As String
Dim prevNum As Integer
Dim RateTime As String
Dim RateVal As String
Dim tottime As Single
Dim sCol As Integer
Dim pszValue As String
Dim varValue As Variant
Dim pszCategory As String
Dim userunit As String
Dim ValidInput As Boolean
Dim c1 As Integer
Dim c2 As Integer
Dim c3 As Integer
Dim CurrTime As Single
Dim PrevTime As Single
Dim NextTime As Single
Dim pIdVal As String
Dim pIdNum As String
Dim pIdTim As String
Dim np As Integer
Dim pNum As Integer
Dim pvalue As String
Dim pTime As String
Dim pRef As Integer
Dim var As Variant
Dim intlUnit As String
Dim spr As fpSpread
Dim range As String
Dim ip As Integer
Dim origVal As String


  Set spr = ssTabCtl(sprIndex)
  spr.Row = Row
  spr.col = col
  spr.ReDraw = False

  If spr.GetText(SS_PID, Row, var) Then pIdVal = var
  If spr.GetText(SS_C1, Row, var) Then c1 = CInt(var)
  If spr.GetText(SS_C2, Row, var) Then c2 = CInt(var)
  If spr.GetText(SS_C3, Row, var) Then c3 = CInt(var)
  
  pIdNum = pIdVal + "_NUM"
  pIdTim = pIdVal + "_TIM"

  sRc = GetParamElem(sInt, "STMAXTIME", 0, 0, E_USERVAL, maxtime)
  
  pszValue = ""
  Select Case spr.CellType
    Case SS_CELL_TYPE_COMBOBOX
      Select Case sprIndex
        Case TAB_KD
          If spr.TypeComboBoxCurSel > 0 Then
            If c2 > 0 Then
              pszValue = getKd(vptCont(c1).name, spr.TypeComboBoxCurSel, vptCont(c1).prog(c2).name)
            Else
              pszValue = getKd(vptCont(c1).name, spr.TypeComboBoxCurSel)
            End If
          End If
        Case Else
          If spr.GetText(col, Row, var) Then pszValue = var
      End Select
    Case Else
      If spr.GetText(col, Row, var) Then pszValue = var
  End Select

    Select Case col
      Case SS_KD_EST
        If Val(pszValue) >= 0 Then
          sRc = GetParamElem3(sInt, pIdNum, c1, c2, 0, E_USERVAL, numRates)
          For i = 1 To numRates
            SetParamElem3 sInt, pIdVal, c1, c2, i, E_USERVAL, pszValue
            spr.SetText SS_KD, Row + i - 1, pszValue
          Next i
        End If
      Case SS_KD_NUM
        sRc = GetParamElem3(sInt, pIdNum, c1, 0, 0, E_RANGE, range)
        sRc = GetParamElem3(sInt, pIdNum, c1, c2, 0, E_USERVAL, prevNum)
        
        ValidInput = ValidateType(VAL_INT, pIdNum, spr, False)
        If ValidInput And pszValue <> "" And range <> "" Then
          ValidInput = CheckRange(pIdNum, pszValue, range)
        End If
        If Not ValidInput Then
          pszValue = CStr(prevNum)
          spr.SetText col, Row, CStr(pszValue)
          ValidInput = ValidateType(VAL_INT, pIdNum, spr, False)
          If ValidInput And pszValue <> "" Then
            ValidInput = CheckRange(pIdNum, pszValue, range)
          End If
        End If
        SetInputValid spr, col, Row, sInt, ValidInput
        If Not ValidInput Then Exit Function

        numRates = CInt(pszValue)

        sRow = Row

'       sRc = GetParamElem3(sInt, pIdVal, c1, c2, c3, E_USERVAL, CurrRate)
        spr.Row = sRow + 1
        For i = 2 To prevNum
          spr.Action = SS_ACTION_DELETE_ROW
          spr.MaxRows = spr.MaxRows - 1
          If i > numRates Then
            SetParamElem3 sInt, pIdTim, c1, c2, i, E_USERVAL, "" ' 0#
            SetParamElemSprAttr sInt, pIdTim, c1, c2, i, 0, 0, 0, False
            
            SetParamElem3 sInt, pIdVal, c1, c2, i, E_USERVAL, "" ' "n/a" ' UNDEF_NA
            SetParamElemSprAttr sInt, pIdVal, c1, c2, i, 0, 0, 0, False
          End If
        Next
        
        SetParamElem3 sInt, pIdNum, c1, c2, 0, E_USERVAL, numRates
        
        If numRates > 1 Then
          If Row = spr.MaxRows Then
            spr.MaxRows = spr.MaxRows + (numRates - 1)
          Else
            For i = 2 To numRates
              spr.MaxRows = spr.MaxRows + 1
              spr.Action = SS_ACTION_INSERT_ROW
            Next
          End If
        End If
          
        tottime = 0#

        For i = 1 To numRates
          sRow = Row + (i - 1)
'           RateVal = CStr(CurrRate)
            RateVal = "0"
'           If "STKD" = pIdVal Then RateVal = "0" Else RateVal = ""
            If i <= prevNum Then
              sRc = GetParamElem3(sInt, pIdVal, c1, c2, i, E_USERVAL, RateVal)
            End If
            If i = 1 Then RateTime = "0"
            If i = numRates Then RateTime = maxtime
            If i > 1 And i < numRates Then RateTime = ""

            spr.Row = sRow
            For sCol = 0 To spr.MaxCols
              spr.col = sCol
              Select Case sCol
                Case SS_PID:
                  spr.SetText sCol, sRow, CStr(pIdVal)
                Case SS_C1:
                  spr.SetText sCol, sRow, CStr(c1)
                Case SS_C2:
                  spr.SetText sCol, sRow, CStr(c2)
                Case SS_C3:
                  spr.SetText sCol, sRow, CStr(i)
                Case SS_ROW:
                  spr.SetText sCol, sRow, CStr(i)
                Case SS_KD_NUM:
                  If sRow = 1 Then spr.SetText sCol, sRow, CStr(numRates)
                  spr.Lock = (i > 1) ' False
                Case SS_KD:
                  spr.SetText sCol, sRow, CStr(RateVal)
                  spr.Lock = False
                  SetInputValid spr, sCol, sRow, sInt, IsNumeric(RateVal)
                Case SS_KD_TIM:
                  spr.SetText sCol, sRow, CStr(RateTime)
'                 If "STKD" = pIdVal Then
                    spr.Lock = ("STKD" = pIdVal And (i = 1 Or i = numRates))
'                 Else
'                   spr.Lock = False
'                 End If
                  SetInputValid spr, sCol, sRow, sInt, IsNumeric(RateTime)
                Case SS_KD_REF:
                  sRc = GetParamElem3(sInt, pIdVal, c1, c2, i, E_REF, pRef)
                  spr.SetText sCol, sRow, pRef
              End Select
            Next
            
            If i = 1 Then
              SetParamElemSprAttr sInt, pIdNum, c1, c2, 0, sprIndex, SS_KD_NUM, sRow, True
              SetParamElem3 sInt, pIdNum, c1, c2, 0, E_RANGE, GetParamInfoElem(ip, pIdNum, E_RANGE)
              SetParamElem3 sInt, pIdNum, c1, c2, 0, E_INVALID, False
              SetParamElem3 sInt, pIdNum, c1, c2, 0, E_USERVAL, numRates
            End If
            SetParamElemSprAttr sInt, pIdVal, c1, c2, i, sprIndex, SS_KD, sRow, True
            SetParamElem3 sInt, pIdVal, c1, c2, i, E_RANGE, GetParamInfoElem(ip, pIdVal, E_RANGE)
            SetParamElem3 sInt, pIdVal, c1, c2, i, E_INVALID, False
            SetParamElem3 sInt, pIdVal, c1, c2, i, E_USERVAL, RateVal
            
            SetParamElemSprAttr sInt, pIdTim, c1, c2, i, sprIndex, SS_KD_TIM, sRow, True
            SetParamElem3 sInt, pIdTim, c1, c2, i, E_RANGE, GetParamInfoElem(ip, pIdTim, E_RANGE)
            SetParamElem3 sInt, pIdTim, c1, c2, i, E_INVALID, False
            SetParamElem3 sInt, pIdTim, c1, c2, i, E_USERVAL, RateTime
          Next
          
      Case SS_KD_TIM
        ' change in time
        If spr.GetText(col, Row, var) Then pszValue = var
        If spr.GetText(SS_ROW, Row, var) And IsNumeric(var) Then sRow = var
        sRc = GetParamElem3(sInt, pIdTim, c1, c2, c3, E_RANGE, range)

        ValidInput = ValidateType(VAL_FLOAT, pIdTim, spr, False)
        If ValidInput And pszValue <> "" Then
          ValidInput = CheckRange(pIdVal, pszValue, range)
        End If
        
        ' check that time is > previous time and < next time
        If ValidInput Then
          GetParamElem3 sInt, pIdNum, c1, c2, 0, E_USERVAL, numRates
          range = "x"
          If c3 = 1 Then
            range = "0 <= x"
          Else
            If GetParamElem3(sInt, pIdTim, c1, c2, c3 - 1, E_USERVAL, var) Then
              If IsNumeric(var) Then range = var & " < " & range
            End If
          End If
          If c3 < numRates Then
            If GetParamElem3(sInt, pIdTim, c1, c2, c3 + 1, E_USERVAL, var) Then
              If IsNumeric(var) Then range = range & " < " & var
            End If
          End If
          If range <> "x" Then
            ValidInput = CheckRange(pIdTim, pszValue, range)
          End If
        End If

        If Not ValidInput Then
'         sRc = GetParamElem3(sInt, pIdTim, c1, c2, c3, E_USERVAL, pszValue)
          pszValue = " "
          spr.SetText col, Row, CStr(pszValue)
        End If
        SetParamElem3 sInt, pIdTim, c1, c2, c3, E_USERVAL, pszValue
        SetInputValid spr, col, Row, sInt, ValidInput

      Case SS_KD
        If spr.GetText(col, Row, var) Then pszValue = var
        sRc = GetParamElem3(sInt, pIdVal, c1, c2, c3, E_RANGE, range)

        ValidInput = ValidateType(VAL_FLOAT, pIdVal, spr, False)
        If ValidInput And pszValue <> "" Then
          ValidInput = CheckRange(pIdVal, pszValue, range)
        End If
        
        If Not ValidInput Then
'         sRc = GetParamElem3(sInt, pIdVal, c1, c2, c3, E_USERVAL, pszValue)
          pszValue = " "
          spr.SetText col, Row, CStr(pszValue)
        End If
        SetParamElem3 sInt, pIdVal, c1, c2, c3, E_USERVAL, pszValue
        SetInputValid spr, col, Row, sInt, ValidInput

    End Select

  spr.ReDraw = True

End Function
Sub WriteContamSeriesToSpr(sprIndex%, mode$, params() As String)
Dim spr As fpSpread
Dim sInt As Integer
Dim maxtime As String
Dim ncont As Integer
Dim kdval As String
Dim kdtime As String
Dim temp As Double
Dim kdlist As String
Dim ndx As Integer
Dim i As Integer
Dim lRow As Integer
Dim nRow As Integer
Dim sRc As Integer
Dim sCol As Integer
Dim ip As Integer
Dim pIdVal As String
Dim pIdNum As String
Dim pIdTim As String
Dim np As Integer
Dim pNum As Integer
Dim pvalue As String
Dim pTime As String
Dim pRef As Integer
Dim idx As Integer
Dim c1 As Integer, c2 As Integer, c3 As Integer
Dim numcon As Integer
Dim str As String
Dim sgl As Single
Dim msg As String
Dim ctype As Integer
Dim sand As Double
Dim silt As Double
Dim clay As Double
Dim omc As Double

  If mode = "KD" Then
    ReDim params(1)
    params(1) = "STKD"
    
    Load frmSoilTri ' activate estimate parameters
    GetParamElem i, "KDSAND", 0, 0, E_USERVAL, sand
    GetParamElem i, "KDSILT", 0, 0, E_USERVAL, silt
    GetParamElem i, "KDCLAY", 0, 0, E_USERVAL, clay
    GetParamElem i, "KDOMC", 0, 0, E_USERVAL, omc
  End If

  Set spr = ssTabCtl(sprIndex)
  
  ssParamsInit spr

  spr.MaxRows = 50
  
  spr.Row = -1
  spr.col = -1
  spr.BackColor = QBColor(15)
  spr.CellType = 1
  spr.Lock = True
  spr.LockBackColor = LIGHT_GREY
  
  spr.col = SS_KD_EST
  spr.ColHidden = False
  
  If mode = "KD" Then
    spr.ColWidth(SS_KD_EST) = 23
    spr.ColWidth(SS_KD_CONT) = spr.ColWidth(SS_KD_CONT) - spr.ColWidth(SS_KD_EST) + 2
    spr.ColWidth(SS_KD_NUM) = 5
    spr.ColWidth(SS_KD_TIM) = 5
    spr.ColWidth(SS_KD) = 10
    spr.ColWidth(SS_KD_REF) = 4
    
    spr.SetText SS_KD_EST, 0, "Estimate"
    spr.SetText SS_KD_NUM, 0, "Count"
    spr.SetText SS_KD, 0, "Value"
    spr.SetText SS_KD_TIM, 0, "Time" '  (years)"
  Else
    spr.ColWidth(SS_KD_EST) = 11
    spr.ColWidth(SS_KD_CONT) = spr.ColWidth(SS_KD_CONT) - spr.ColWidth(SS_KD_EST) - 2
    spr.ColWidth(SS_KD_NUM) = 8
    spr.ColWidth(SS_KD_TIM) = 8
    spr.ColWidth(SS_KD) = 10
    spr.ColWidth(SS_KD_REF) = 6
    spr.RowHeight(0) = 3 * spr.RowHeight(1)
    
    spr.SetText SS_KD_EST, 0, "Unit"
    spr.SetText SS_KD_CONT, 0, "Description"
    spr.SetText SS_KD_NUM, 0, "# of " & Chr(10) & "Measure -ments"
    spr.SetText SS_KD, 0, "Value"
    spr.SetText SS_KD_TIM, 0, "Year of " & Chr(10) & "Measure -ment"
  End If

  spr.col = SS_KD_REF
  spr.ColHidden = False
  spr.CellType = SS_CELL_TYPE_STATIC_TEXT
  spr.BackColor = LIGHT_GREY
  spr.TypeHAlign = 2 ' center
  spr.VisibleCols = SS_KD_REF
  spr.SetText SS_KD_REF, 0, "Ref."

  
  ResetParamsSprIndex sprIndex
  
  sInt = GetParamElem(ndx, "STMAXTIME", 0, 0, E_USERVAL, maxtime)

  lRow = 1
  
  For np = 1 To UBound(params)
  
    If lRow > spr.MaxRows Then spr.MaxRows = 50 + spr.MaxRows
    
    Select Case mode
      Case "KD"
        numcon = UBound(vptAllCont)
        pIdVal = params(np)
      Case "FLUX"
        numcon = UBound(vptCont)
        Select Case params(np)
          Case "STINF_OP": pIdVal = "STLEACH"
          Case "STOVL_OP": pIdVal = "STOVL"
          Case "STSUS_OP": pIdVal = "STSUSP"
          Case "STVOL_OP": pIdVal = "STVOLAT"
          Case "STSRC_OP": pIdVal = "STSOURCE" '  numcon = UBound(vptAllCont)
        End Select
    End Select
    
    
    pIdNum = pIdVal + "_NUM"
    pIdTim = pIdVal + "_TIM"
    
    SetParamInfoElem ip, pIdTim, E_RANGE, "0 <= x <= " & maxtime
    
    str = GetParamInfoElem(ip, pIdVal, E_SCREEN)
    str = str & " -- " & pIdVal
    spr.SetText SS_KD_CONT, lRow, CStr(str)
    If mode = "KD" Then spr.SetText SS_KD, lRow, GetParamInfoElem(ip, pIdVal, E_INTLUNIT)
      
    
'   spr.SetText SS_KD, lRow, GetParamInfoElem(ip, pIdVal, E_INTLUNIT) ' 3/99 BLH'
'   spr.SetText SS_KD_TIM, lRow, "years"    ' 3/99 BLH
    
    lRow = lRow + 1
    
    For ncont = 1 To numcon
    
      c1 = vptAllCont(ncont).ctr(1, 1)
      c2 = vptAllCont(ncont).ctr(1, 2)
      
      If mode = "KD" Then
        str = "ml/g"  ' 7/99 BLH
      Else
        If 1 = vptAllCont(ncont).type Then str = "Ci/yr" Else str = "g/yr"
      End If
      SetParamInfoElem ip, pIdVal, E_INTLUNIT, str
      SetParamInfoElem ip, pIdVal, E_USERUNIT, str
      
      pvalue = "0"
      sRc = GetParamElem(ndx, pIdNum, c1, c2, E_USERVAL, pNum)
      If Not (sRc And pNum > 0) Then
        pNum = 2
        pTime = maxtime
        SetParamElem3 sInt, pIdNum, c1, c2, 0, E_USERVAL, pNum
        SetParamElem3 sInt, pIdVal, c1, c2, 1, E_USERVAL, 0#
        SetParamElem3 sInt, pIdTim, c1, c2, 1, E_USERVAL, 0#
        SetParamElem3 sInt, pIdVal, c1, c2, pNum, E_SSROW, lRow
        SetParamElem3 sInt, pIdVal, c1, c2, 2, E_USERVAL, 0#
        SetParamElem3 sInt, pIdTim, c1, c2, 2, E_USERVAL, pTime
      End If
      SetParamElem3 sInt, pIdNum, c1, c2, 0, E_SPR, sprIndex
      
'     sRc = GetParamElem3(ndx, pIdTim, c1, c2, pNum, E_USERVAL, pTime)
'     If maxtime <> pTime Then
'       pNum = 1
'       SetParamElem3 sInt, pIdNum, c1, c2, 0, E_USERVAL, pNum
'       SetParamElem3 sInt, pIdTim, c1, c2, pNum, E_USERVAL, maxtime
'     End If
      
      spr.Row = lRow
      For nRow = 1 To pNum
        If Not GetParamElem3(ndx, pIdTim, c1, c2, nRow, E_USERVAL, pTime) Then pTime = ""
        If Not GetParamElem3(ndx, pIdVal, c1, c2, nRow, E_USERVAL, pvalue) Then pvalue = ""
        If IsNumeric(pTime) Then
          If Val(pTime) > Val(maxtime) Then pTime = ""
        End If
        If nRow = pNum Then pTime = maxtime

        SetParamElem3 ndx, pIdTim, c1, c2, nRow, E_USERVAL, pTime
        SetParamElem3 ndx, pIdTim, c1, c2, nRow, E_INVALID, Not IsNumeric(pTime)
        SetParamElem3 ndx, pIdVal, c1, c2, nRow, E_USERVAL, pvalue
        SetParamElem3 ndx, pIdVal, c1, c2, nRow, E_INVALID, Not IsNumeric(pvalue)
       
        spr.Row = lRow
        
        ' update parameter struct with vaSpread ino
        If nRow = 1 Then
          SetParamElemSprAttr sInt, pIdNum, c1, c2, 0, sprIndex, SS_KD_NUM, lRow, True
          SetParamElem3 sInt, pIdNum, c1, c2, 0, E_RANGE, GetParamInfoElem(ip, pIdNum, E_RANGE)
        End If
        SetParamElemSprAttr sInt, pIdVal, c1, c2, nRow, sprIndex, SS_KD, lRow, True
        SetParamElem3 sInt, pIdVal, c1, c2, nRow, E_RANGE, GetParamInfoElem(ip, pIdVal, E_RANGE)

        SetParamElemSprAttr sInt, pIdTim, c1, c2, nRow, sprIndex, SS_KD_TIM, lRow, True
        SetParamElem3 sInt, pIdTim, c1, c2, nRow, E_RANGE, GetParamInfoElem(ip, pIdTim, E_RANGE)

        
        ' populate vaSpread
        spr.SetText SS_PID, lRow, CStr(pIdVal)
        spr.SetText SS_C1, lRow, CStr(c1)
        spr.SetText SS_C2, lRow, CStr(c2)
        spr.SetText SS_C3, lRow, CStr(nRow)
        
            
        spr.col = SS_KD_CONT
        If nRow = 1 Then
          spr.SetText SS_KD_CONT, lRow, Space(5) + CStr(vptAllCont(ncont).name)

        End If
        
        spr.col = SS_KD_EST
        If nRow = 1 Then
          If mode = "KD" Then
            kdlist = " Select Adsorption Coeff."
            If c2 > 0 Then
              temp = Kd_DatabaseValue(vptAllCont(c1).name, vptAllCont(ncont).name)
            Else
              temp = Kd_DatabaseValue(vptAllCont(ncont).name)
            End If
            If temp < 0 Then
                kdlist = kdlist & Chr(9) & "N/A - " & getEstKdDesc(DATABASE_LOOKUP)
            Else
                kdlist = kdlist & Chr(9) & temp & " - " & getEstKdDesc(DATABASE_LOOKUP)
            End If
            
           If c2 > 0 Then
              temp = Kd_EquationValue(vptAllCont(c1).name, CStr(omc), CStr(clay), CStr(silt), CStr(sand), vptAllCont(ncont).name)
            Else
              temp = Kd_EquationValue(vptAllCont(ncont).name, CStr(omc), CStr(clay), CStr(silt), CStr(sand))
            End If
            If temp < 0 Then
                kdlist = kdlist & Chr(9) & "N/A - " & getEstKdDesc(EQUATION_LOOKUP)
            Else
                kdlist = kdlist & Chr(9) & temp & " - " & getEstKdDesc(EQUATION_LOOKUP)
            End If
            
            If c2 > 0 Then
              temp = Kd_LookupTableValue(vptAllCont(c1).name, vptAllCont(ncont).name)
            Else
              temp = Kd_LookupTableValue(vptAllCont(ncont).name)
            End If
            If temp < 0 Then
                kdlist = kdlist & Chr(9) & "N/A - " & getEstKdDesc(TABLE_LOOKUP)
            Else
                kdlist = kdlist & Chr(9) & temp & " - " & getEstKdDesc(TABLE_LOOKUP)
            End If
            
            spr.CellType = SS_CELL_TYPE_COMBOBOX
            spr.TypeComboBoxList = kdlist
            spr.TypeComboBoxCurSel = 0
            spr.Lock = False
          Else
            spr.CellType = SS_CELL_TYPE_EDIT
            spr.Lock = True
            Dim ulist As String, uct As Integer
            If Not GetParamElem3(ndx, pIdVal, c1, c2, nRow, E_USERUNIT, str) Then
              str = GetParamInfoElem(ip, pIdVal, E_INTLUNIT)
            End If
'           BLH 03/09/2001 - after correcting "g/yr or Ci/yr" units in the gid file
'                            (see AdjustContamParams) the rad units produced a conversion list.
'                            I disallowed this because there is no unit change handler implemented for
'                            this particular spreadsheet format (dynamic time series).
'           If str <> "" Then
'             uct = get_conversion_list(str, ulist)
'             If uct > 1 Then
'               spr.CellType = 8
'               spr.TypeComboBoxList = ulist
'               spr.TypeComboBoxCurSel = 0
'               spr.Lock = False
'             Else
                spr.SetText SS_KD_EST, lRow, CStr(str)
'             End If
'           End If
          End If
        End If
        
        spr.col = SS_KD_NUM
        If nRow = 1 Then spr.SetText SS_KD_NUM, lRow, CStr(pNum)
        spr.Lock = (nRow > 1) ' False
        
        spr.col = SS_KD
        spr.SetText SS_KD, lRow, convert("", "", CDbl(pvalue))
        spr.Lock = False
        
        spr.col = SS_KD_TIM
        spr.SetText SS_KD_TIM, lRow, CStr(pTime)
        If (mode = "KD") Then
          spr.Lock = (nRow = 1 Or nRow = pNum)
        Else
          spr.Lock = False
        End If
        
        spr.col = SS_KD_REF
        sRc = GetParamElem(ndx, pIdTim, pNum, 0, E_REF, pRef)
        spr.SetText SS_KD_REF, lRow, pRef

        
        lRow = lRow + 1
        
        If lRow > spr.MaxRows Then spr.MaxRows = 50 + spr.MaxRows

      Next nRow
    Next ncont
    lRow = lRow + 1
  Next np
  spr.MaxRows = lRow - 2
  
  IsInputComplete sprIndex, pIdVal, msg  ' check the data

End Sub
Sub WriteSeriesToSpr(sprIndex%, params() As String)
Dim spr As fpSpread
Dim sInt As Integer
Dim maxtime As String
Dim ncont As Integer
Dim kdval As String
Dim kdtime As String
Dim ndx As Integer
Dim i As Integer
Dim lRow As Integer
Dim nRow As Integer
Dim sRc As Integer
Dim sCol As Integer
Dim ip As Integer
Dim pIdVal As String
Dim pIdNum As String
Dim pIdTim As String
Dim np As Integer
Dim pNum As Integer
Dim pvalue As String
Dim pTime As String
Dim pRef As Integer
Dim str As String
Dim msg As String
Dim ulist As String, uct As Integer


  Set spr = ssTabCtl(sprIndex)
  
  ssParamsInit spr
  ResetParamsSprIndex sprIndex
  
' spr.MaxRows = UBound(params) * (3 + UBound(vptCont))
  spr.MaxRows = 50
  
  spr.Row = -1
  spr.col = -1
  spr.CellType = 1
  spr.BackColor = QBColor(15)
  spr.Lock = True
  spr.LockBackColor = LIGHT_GREY
  
' spr.ColWidth(SS_KD) = 10
  spr.ColWidth(SS_KD_CONT) = spr.ColWidth(SS_KD_CONT) - 2
  spr.ColWidth(SS_KD_NUM) = 8
  spr.ColWidth(SS_KD_TIM) = 8
  spr.ColWidth(SS_KD) = 10
  spr.RowHeight(0) = 3 * spr.RowHeight(1)
  spr.SetText SS_KD_CONT, 0, "Description"
  spr.SetText SS_KD_NUM, 0, "# of " & Chr(10) & "Measure- ments"
  spr.SetText SS_KD, 0, "Value"
  spr.SetText SS_KD_TIM, 0, "Year of " & Chr(10) & "Measure- ment"
  spr.SetText SS_KD_REF, 0, "Ref."
  
  spr.col = SS_KD_EST
  spr.ColHidden = False
  spr.ColWidth(SS_KD_EST) = 11
  spr.ColWidth(SS_DESC) = spr.ColWidth(SS_DESC) - spr.ColWidth(SS_KD_EST)
  spr.SetText SS_KD_EST, 0, "Unit"
  
  spr.col = SS_KD_REF
  spr.ColHidden = False
  spr.ColWidth(SS_KD_REF) = 6
  spr.CellType = SS_CELL_TYPE_STATIC_TEXT
  spr.BackColor = LIGHT_GREY
  spr.TypeHAlign = 2 ' center
  spr.VisibleCols = SS_KD_REF
  sInt = GetParamElem(ndx, "STMAXTIME", 0, 0, E_USERVAL, maxtime)

  lRow = 1
  
  For np = 1 To UBound(params)
  
    If lRow > spr.MaxRows Then spr.MaxRows = 50 + spr.MaxRows
      
  
    Select Case params(np)
      Case "STINF_OP": pIdVal = "STDARCY"
      Case "STOVL_OP": pIdVal = "STWATR"
      Case "STSUS_OP": pIdVal = "STWIND"
      Case "STAIRSPC": pIdVal = params(np)
      Case "STTOTPOR": pIdVal = params(np)
    End Select
  
    pIdNum = pIdVal + "_NUM"
    pIdTim = pIdVal + "_TIM"
    
    SetParamInfoElem ip, pIdTim, E_RANGE, "0 <= x <= " & maxtime
   
      pvalue = 0
      sRc = GetParamElem(ndx, pIdNum, 0, 0, E_USERVAL, pNum)
      If Not (sRc And pNum > 0) Then
        pNum = 2
        pTime = maxtime
        SetParamElem sInt, pIdNum, 0, 0, E_USERVAL, pNum
        SetParamElem sInt, pIdVal, 1, 0, E_USERVAL, 0#
        SetParamElem sInt, pIdTim, 1, 0, E_USERVAL, 0#
'       SetParamElem sInt, pIdVal, 1, 0, E_SSROW, lRow
        SetParamElem sInt, pIdVal, 2, 0, E_USERVAL, 0#
        SetParamElem sInt, pIdTim, 2, 0, E_USERVAL, pTime
'       SetParamElem sInt, pIdVal, 2, 0, E_SSROW, lRow + 1
      End If
      spr.Row = lRow
      SetParamElem sInt, pIdNum, 0, 0, E_SPR, sprIndex
      
      sRc = GetParamElem(ndx, pIdTim, pNum, 0, E_USERVAL, pTime)
'     If pTime <> maxtime Then
'       pNum = 1
'       SetParamElem sInt, pIdNum, 0, 0, E_USERVAL, pNum
'       SetParamElem sInt, pIdTim, 1, 0, E_USERVAL, maxtime
'     End If

    str = GetParamInfoElem(ip, pIdVal, E_SCREEN)
    str = str & " -- " & pIdVal
'   str = str & " (" & GetParamInfoElem(ip, pIdVal, E_INTLUNIT) & ")"
    spr.SetText SS_KD_CONT, lRow, CStr(str)

      lRow = lRow + 1
      For nRow = 1 To pNum
    
        If Not GetParamElem(ndx, pIdTim, nRow, 0, E_USERVAL, pTime) Then pTime = ""
        If Not GetParamElem(ndx, pIdVal, nRow, 0, E_USERVAL, pvalue) Then pvalue = ""
        If IsNumeric(pTime) Then
          If Val(pTime) > Val(maxtime) Then pTime = ""
        End If
        If nRow = pNum Then pTime = maxtime
        If nRow = 1 Then pTime = 0
        
        ' update parameter struct with vaSpread ino
        SetParamElem ndx, pIdTim, nRow, 0, E_USERVAL, pTime
        SetParamElem ndx, pIdTim, nRow, 0, E_INVALID, Not IsNumeric(pTime)
        SetParamElem ndx, pIdVal, nRow, 0, E_USERVAL, pvalue
        SetParamElem ndx, pIdVal, nRow, 0, E_INVALID, Not IsNumeric(pvalue)
        
        If nRow = 1 Then
          SetParamElemSprAttr ndx, pIdNum, 0, 0, 0, sprIndex, SS_KD_NUM, lRow, True
          SetParamElem3 ndx, pIdNum, 0, 0, 0, E_RANGE, GetParamInfoElem(ip, pIdNum, E_RANGE)
        End If
        SetParamElemSprAttr ndx, pIdVal, nRow, 0, 0, sprIndex, SS_KD, lRow, True
        SetParamElem3 ndx, pIdVal, nRow, 0, 0, E_RANGE, GetParamInfoElem(ip, pIdVal, E_RANGE)
        
        SetParamElemSprAttr ndx, pIdTim, nRow, 0, 0, sprIndex, SS_KD_TIM, lRow, True
        SetParamElem3 ndx, pIdTim, nRow, 0, 0, E_RANGE, GetParamInfoElem(ip, pIdTim, E_RANGE)

        spr.Row = lRow
    
        For sCol = 0 To spr.MaxCols
          spr.col = sCol
          Select Case sCol
          
            Case SS_PID:
              spr.SetText sCol, lRow, CStr(pIdVal)
            Case SS_C1:
              spr.SetText sCol, lRow, CStr(nRow)
            Case SS_C2:
              spr.SetText sCol, lRow, CStr(0)
            Case SS_ROW:
              spr.SetText sCol, lRow, CStr(nRow)
            Case SS_KD_CONT:
'             If nRow = 1 Then
'               str = GetParamInfoElem(ip, pIdVal, E_INTLUNIT)
'               str = GetParamInfoElem(ip, pIdVal, E_SCREEN)
'               str = str & " -- " & pIdVal
'               str = str & " (" & GetParamInfoElem(ip, pIdVal, E_INTLUNIT) & ")"
'               spr.SetText sCol, lRow, CStr(str)
'             End If
            Case SS_KD_EST:
              If nRow = 1 Then
                If Not GetParamElem(ndx, pIdVal, nRow, 0, E_USERUNIT, str) Then
                  str = GetParamInfoElem(ip, pIdVal, E_USERUNIT)
                End If
'               spr.SetText sCol, lRow, CStr(str)
                
                If str <> "" Then
                  uct = get_conversion_list(str, ulist)
                  If uct > 1 Then
                    spr.CellType = 8 ' combobox
                    spr.TypeComboBoxList = ulist
                    spr.TypeComboBoxCurSel = 0
                    spr.Lock = False
                  Else
                    spr.SetText SS_KD_EST, lRow, CStr(str)
                  End If
                End If

              End If
            Case SS_KD_NUM:
              If nRow = 1 Then spr.SetText sCol, lRow, CStr(pNum)
              spr.Lock = (nRow > 1) ' False
            Case SS_KD:
              spr.SetText sCol, lRow, CStr(pvalue)
              spr.Lock = False
            Case SS_KD_TIM:
              spr.SetText sCol, lRow, CStr(pTime)
              spr.Lock = ((nRow = pNum) Or (nRow = 1))
            Case SS_KD_REF:
              sRc = GetParamElem(ndx, pIdTim, pNum, 0, E_REF, pRef)
              spr.SetText sCol, lRow, pRef
          End Select
        Next sCol
        lRow = lRow + 1
      Next nRow
    lRow = lRow + 1
  Next np
  spr.MaxRows = lRow - 2
  IsInputComplete sprIndex, pIdVal, msg  ' check the data
End Sub
Sub WriteContamParamsToSpr(sprIndex%)
Dim spr As fpSpread
Dim sInt As Integer
Dim ncont As Integer
Dim lRow As Integer
Dim nRow As Integer
Dim sRc As Integer
Dim pIdVal As String
Dim ip As Integer, np As Integer
Dim pvalue As String
Dim params() As String
Dim c1 As Integer, c2 As Integer
Dim pInfo As ParamInfoStruct
Dim pStruct As ParamStruct
Dim ulist As String
Dim uct As Integer
Dim idx As Integer
Dim intlUnit As String
Dim numcon As Integer
Dim range As String
Dim msg As String
Dim novol As Boolean
Dim dryVolume As Single

  On Error GoTo WriteContamParamsToSpr_Error
  
  ReDim params(4)
  params(1) = "STSOL"
  params(2) = "STINVEN"
  params(3) = "STGHALF"
  params(4) = "STVOLRAT"
  
  Set spr = ssTabCtl(sprIndex)
  
  ErrorTag = "ssParamsInit"
  ssParamsInit spr
  
  spr.MaxRows = 50
  
  spr.col = SS_CONC
  spr.ColHidden = False
  idx = spr.ColWidth(SS_UOM)
  spr.ColWidth(SS_UOM) = spr.ColWidth(SS_CONC) ' uom is now default size (8)
  spr.ColWidth(SS_CONC) = idx
  spr.ColWidth(SS_DESC) = spr.ColWidth(SS_DESC) - spr.ColWidth(SS_UOM)
  
  spr.MaxRows = UBound(params) * (3 + UBound(vptAllCont))
  
  spr.Row = -1
  spr.col = -1
  spr.BackColor = QBColor(15)
  spr.CellType = 1
  spr.Lock = True
  spr.LockBackColor = LIGHT_GREY
  
  spr.col = SS_REF
  spr.CellType = SS_CELL_TYPE_STATIC_TEXT
  spr.BackColor = LIGHT_GREY
  spr.TypeHAlign = 2 ' center
  
  spr.SetText SS_DESC, 0, "Description"
  spr.SetText SS_VAL, 0, "Value"
  spr.SetText SS_UOM, 0, "Unit"
  spr.SetText SS_REF, 0, "Ref."

  lRow = 1
  
  For np = 1 To UBound(params)
    pIdVal = params(np)
    
    ErrorTag = pIdVal

    If pIdVal = "STVOLRAT" Then novol = OpValue("STVOL_OP", OPT_OFF)
    
    ip = GetParamInfoStruct(pIdVal, pInfo)
    
    pvalue = GetParamInfoElem(ip, pIdVal, E_SCREEN)
    pvalue = pvalue & " -- " & pIdVal
    spr.SetText SS_KD_CONT, lRow, CStr(pvalue)
'   If pIdVal = "STINVEN" Then
'     spr.SetText SS_CONC, lRow, CStr("(conc g/g,Ci/g)")
'   End If
    lRow = lRow + 1
    
    If (pIdVal = "STSOL" Or pIdVal = "STGHALF") Then
      numcon = UBound(vptAllCont)
    Else
      numcon = UBound(vptCont)
    End If
      
    
    For ncont = 1 To numcon
    
      ErrorTag = pIdVal & " " & ncont
      
      c1 = ncont
      c2 = 0
      
      c1 = vptAllCont(ncont).ctr(1, 1)
      c2 = vptAllCont(ncont).ctr(1, 2)
      
      If pIdVal = "STINVEN" Then
        If 1 = vptCont(ncont).type Then intlUnit = "Ci" Else intlUnit = "g"
        SetParamInfoElem ip, pIdVal, E_INTLUNIT, intlUnit
      End If
     
      If Not GetParamElem(idx, pIdVal, c1, c2, E_USERVAL, pvalue) Then
        Select Case pIdVal
          Case "STGHALF": pvalue = vptAllCont(ncont).ghalf(halfIndex)
          Case "STSOL": pvalue = vptAllCont(ncont).sol
          Case "STVOLRAT":  If novol Then pvalue = "0.0" Else pvalue = "1.0"
          Case "STINVEN": pvalue = "" ' pvalue = "0.0"
        End Select
        SetParamElem idx, pIdVal, c1, c2, E_USERVAL, pvalue
      End If
      
      pStruct.pname = pInfo.pszName
      pStruct.c1 = c1
      pStruct.c2 = c2
      idx = GetParamStruct(pStruct)
      
      If pIdVal = "STINVEN" Then
        pStruct.intlUnit = intlUnit
        If InStr(pStruct.userunit, "gr") Then pStruct.userunit = intlUnit
        SetParamElem idx, pIdVal, c1, c2, E_INTLUNIT, pStruct.intlUnit
        SetParamElem idx, pIdVal, c1, c2, E_USERUNIT, pStruct.userunit
      End If

      spr.Row = lRow

      spr.SetText SS_PID, lRow, CStr(pIdVal)
      spr.SetText SS_C1, lRow, CStr(c1)
      spr.SetText SS_C2, lRow, CStr(c2)
      spr.SetText SS_ROW, lRow, CStr(nRow)
      spr.SetText SS_DESC, lRow, Space(5) + CStr(vptAllCont(ncont).name)
      
      If pIdVal = "STINVEN" Then
'       ErrorTag = pIdVal & " " & nCont & " getconcentration"
'       spr.SetText SS_CONC, lRow, convert("", "", CDbl(getConcentration(idx)))
        spr.col = SS_CONC
        spr.CellType = SS_CELL_TYPE_BUTTON
        spr.TypeButtonType = SS_CELL_BUTTON_NORMAL
        spr.TypeButtonText = "Worksheet"
        spr.TypeButtonColor = INP_GRN
        spr.TypeButtonColor = &HC0E0FF
      End If
      
    
      spr.col = SS_VAL
      spr.SetText SS_VAL, lRow, CStr(pvalue)
      spr.Lock = False
      If pIdVal = "STVOLRAT" And novol Then spr.Lock = True
'     If pIdVal = "STGHALF" And vptAllCont(ncont).type = 1 Then spr.Lock = True
'     If pIdVal = "STINVEN" Then spr.Lock = True
      If pIdVal = "STINVEN" Then spr.CellType = 5
      
      ErrorTag = pIdVal & " " & ncont & " unit"
      spr.col = SS_UOM
      spr.Lock = True
      If ListCategory(pInfo.pszCategory) Then
        spr.CellType = SS_CELL_TYPE_COMBOBOX
        WriteComboToSSv2 spr, pInfo.pszCategory, pStruct.value
      Else
        ErrorTag = pIdVal & " " & ncont & " convert"
        If IsNumeric(pStruct.userVal) Then
          spr.SetText SS_VAL, lRow, convert("", "", CDbl(pStruct.userVal))
        End If
        
        If pStruct.intlUnit <> "" Then
          uct = get_conversion_list(pStruct.userunit, ulist)
          If uct > 1 Then
            spr.CellType = 8 ' combobox
            spr.TypeComboBoxList = ulist
            spr.TypeComboBoxCurSel = 0
            spr.Lock = False
          Else
            spr.SetText SS_UOM, lRow, CStr(pStruct.userunit)
          End If
        End If
      End If
      ErrorTag = pIdVal & " " & ncont & " range"
      If pIdVal = "STINVEN" Then spr.Lock = True
      
      range = pInfo.range
      If range = "" Then sRc = GetParamElem(ip, pIdVal, c1, c2, E_RANGE, range)
      If range <> "" Then
        range = ConvertRange(range, "", pStruct.intlUnit, pStruct.userunit)
      End If
      SetParamElem idx, pIdVal, c1, c2, E_RANGE, range

      SetParamElemSprAttr sInt, pIdVal, c1, c2, 0, sprIndex, SS_VAL, lRow, True
      SetParamElem3 sInt, pIdVal, c1, c2, 0, E_RANGE, range
      
      spr.SetText SS_REF, lRow, pStruct.ref

      lRow = lRow + 1
    Next ncont
   lRow = lRow + 1
  Next np
  spr.MaxRows = lRow - 2
  
  IsInputComplete sprIndex, pIdVal, msg  ' check the data
  
  Exit Sub
WriteContamParamsToSpr_Error:
  MsgBox "WriteContamParamsToSpr::" & ErrorTag & " " & Error()
  Resume Next
End Sub
Function SprChangeToSeries(sprIndex%, col%, Row%) As Integer
Dim sRc As Integer
Dim sSeq As Integer
Dim paramId As Integer
Dim sInt As Integer
Dim sRow As Integer
Dim nextId As Integer
Dim numRates As Integer
Dim i As Integer
Dim maxtime As Single
Dim CurrRate As Single
Dim CurrVal As String
Dim prevNum As Integer
Dim RateTime As String
Dim RateVal As String
Dim tottime As Single
Dim sCol As Integer
Dim pszValue As String
Dim varValue As Variant
Dim pszCategory As String
Dim userunit As String
Dim ValidInput As Boolean
Dim c1 As Integer
Dim c2 As Integer
Dim pIdVal As String
Dim pIdNum As String
Dim pIdTim As String
Dim np As Integer
Dim pNum As Integer
Dim pvalue As String
Dim pTime As String
Dim pRef As Integer
Dim CurrTime As Single
Dim PrevTime As Single
Dim NextTime As Single
Dim var As Variant
Dim intlUnit As String
Dim ip As Integer
Dim range As String
Dim c3 As Integer

Dim spr As fpSpread

  Set spr = ssTabCtl(sprIndex)
  
  spr.Row = Row
  spr.col = col
  spr.ReDraw = False
  
  If spr.GetText(SS_PID, Row, var) Then pIdVal = var
  pIdNum = pIdVal + "_NUM"
  pIdTim = pIdVal + "_TIM"
  
    If spr.GetText(SS_C1, Row, var) Then c1 = var
    c2 = 0
    c3 = 0
    
    sRc = GetParamElem(sInt, "STMAXTIME", 0, 0, E_USERVAL, maxtime)
    
    pszValue = ""
    If spr.GetText(col, Row, var) Then pszValue = var
    
    Select Case col
      Case SS_KD_EST
        sRc = GetParamElem(sInt, pIdNum, 0, 0, E_USERVAL, numRates)
        For i = 1 To numRates
          SetParamElem sInt, pIdVal, i, 0, E_USERUNIT, pszValue
        Next i

      Case SS_KD_NUM
        
        sRc = GetParamElem(sInt, pIdNum, 0, 0, E_USERVAL, prevNum)
        sRc = GetParamElem3(sInt, pIdNum, 0, 0, 0, E_RANGE, range)

        ValidInput = ValidateType(VAL_INT, pIdNum, spr, False)
        If ValidInput And pszValue <> "" Then
          ValidInput = CheckRange(pIdNum, pszValue, range)
        End If
        
        ' restore previous data
        If Not ValidInput Then
          pszValue = CStr(prevNum)
          spr.SetText col, Row, CStr(pszValue)
          ValidInput = ValidateType(VAL_INT, pIdNum, spr, False)
          If ValidInput And pszValue <> "" Then
            ValidInput = CheckRange(pIdNum, pszValue, range)
          End If
        End If
          
        SetInputValid spr, col, Row, sInt, ValidInput
        If Not ValidInput Then Exit Function

        numRates = CInt(pszValue)

        sRow = Row

        sRc = GetParamElem(sInt, pIdVal, 1, 0, E_USERVAL, CurrRate)
        sRc = GetParamElem(sInt, pIdVal, 1, 0, E_USERUNIT, userunit)
        spr.Row = sRow + 1
        For i = 2 To prevNum
          spr.Action = SS_ACTION_DELETE_ROW
          spr.MaxRows = spr.MaxRows - 1
          If i > numRates Then
            SetParamElem sInt, pIdTim, i, 0, E_USERVAL, 0#
            SetParamElem sInt, pIdVal, i, 0, E_USERVAL, "" ' "n/a" ' UNDEF_NA
            SetParamElemSprAttr sInt, pIdTim, i, 0, 0, 0, 0, 0, False
            SetParamElemSprAttr sInt, pIdVal, i, 0, 0, 0, 0, 0, False
          End If
        Next
        
        If numRates > 1 Then
          If Row = spr.MaxRows Then
            spr.MaxRows = spr.MaxRows + (numRates - 1)
          Else
            For i = 2 To numRates
              spr.MaxRows = spr.MaxRows + 1
              spr.Action = SS_ACTION_INSERT_ROW
            Next
          End If
        End If
          
        tottime = 0#

        For i = 1 To numRates
          sRow = Row + (i - 1)
            RateVal = "0.0"
            RateTime = ""
            If i <= prevNum Then
              sRc = GetParamElem(sInt, pIdVal, i, 0, E_USERVAL, RateVal)
'             sRc = GetParamElem(sInt, pIdTim, i, 0, E_USERVAL, RateTime)
            End If
            If Not IsNumeric(RateVal) Then RateVal = "0.0"
            If (i = numRates) Then
              RateTime = maxtime
            Else
              If IsNumeric(RateTime) Then
                If CSng(RateTime) > maxtime Then RateTime = CStr(maxtime)
              End If
            End If
            If i = 1 Then RateTime = "0"
            If i = numRates Then RateTime = maxtime
            If i > 1 And i < numRates Then RateTime = ""
            
            SetParamElem sInt, pIdVal, i, 0, E_USERUNIT, userunit
            SetParamElem sInt, pIdVal, i, 0, E_USERVAL, RateVal
            SetParamElem sInt, pIdTim, i, 0, E_USERVAL, RateTime
  
            spr.Row = sRow
            For sCol = 0 To spr.MaxCols
              spr.col = sCol
              Select Case sCol
                Case SS_PID:
                  spr.SetText sCol, sRow, CStr(pIdVal)
                Case SS_C1:
                  spr.SetText sCol, sRow, CStr(i)
                Case SS_C2:
                  spr.SetText sCol, sRow, CStr(0)
                Case SS_ROW:
                  spr.SetText sCol, sRow, CStr(i)
                
                Case SS_KD_NUM:
                  If sRow = 1 Then spr.SetText sCol, sRow, CStr(numRates)
                  spr.Lock = (i > 1)
                Case SS_KD:
                  spr.SetText sCol, sRow, CStr(RateVal)
                  SetInputValid spr, sCol, sRow, sInt, IsNumeric(RateVal)
                  spr.Lock = False
                Case SS_KD_TIM:
                  spr.SetText sCol, sRow, CStr(RateTime)
                  SetInputValid spr, sCol, sRow, sInt, IsNumeric(RateTime)
                  spr.Lock = ((i = numRates) Or (i = 1))
                Case SS_KD_REF:
                  sRc = GetParamElem(sInt, pIdVal, i, 0, E_REF, pRef)
                  spr.SetText sCol, sRow, pRef
              End Select
            Next
            
            If i = 1 Then
              SetParamElem sInt, pIdNum, 0, 0, E_USERVAL, numRates
              SetParamElem sInt, pIdNum, 0, 0, E_RANGE, GetParamInfoElem(ip, pIdNum, E_RANGE)
              SetParamElemSprAttr sInt, pIdNum, 0, 0, 0, sprIndex, SS_KD_NUM, sRow, True
            End If
            SetParamElem sInt, pIdVal, i, 0, E_USERVAL, RateVal
            SetParamElem sInt, pIdVal, i, 0, E_RANGE, GetParamInfoElem(ip, pIdVal, E_RANGE)
            SetParamElemSprAttr sInt, pIdVal, i, 0, 0, sprIndex, SS_KD, sRow, True
            
            SetParamElem sInt, pIdTim, i, 0, E_USERVAL, RateTime
            SetParamElem sInt, pIdTim, i, 0, E_RANGE, GetParamInfoElem(ip, pIdTim, E_RANGE)
            SetParamElemSprAttr sInt, pIdTim, i, 0, 0, sprIndex, SS_KD_TIM, sRow, True
          Next
          
      Case SS_KD_TIM
        ' change in time
        If spr.GetText(col, Row, var) Then pszValue = var
        If spr.GetText(7, Row, var) And IsNumeric(var) Then sRow = var
        sRc = GetParamElem3(sInt, pIdTim, c1, c2, c3, E_RANGE, range)

        ValidInput = ValidateType(VAL_FLOAT, pIdTim, spr, False)
        If ValidInput And pszValue <> "" Then
          ValidInput = CheckRange(pIdTim, pszValue, range)
        End If
        
        ' check that time is > previous time and < next time
        If ValidInput Then
          GetParamElem sInt, pIdNum, 0, 0, E_USERVAL, numRates
          range = "x"
          If c1 = 1 Then
            range = "0 <= x"
          Else
            If GetParamElem(sInt, pIdTim, c1 - 1, 0, E_USERVAL, var) Then
              If IsNumeric(var) Then range = var & " < " & range
            End If
          End If
          If c1 < numRates Then
            If GetParamElem(sInt, pIdTim, c1 + 1, 0, E_USERVAL, var) Then
              If IsNumeric(var) Then range = range & " < " & var
            End If
          End If
          If range <> "x" Then
            ValidInput = CheckRange(pIdTim, pszValue, range)
          End If
        End If
        If Not ValidInput Then
'         sRc = GetParamElem(sInt, pIdTim, c1, 0, E_USERVAL, pszValue)
          pszValue = " "
          spr.SetText col, Row, CStr(pszValue)
        End If
        SetParamElem sInt, pIdTim, c1, 0, E_USERVAL, pszValue
        SetInputValid spr, col, Row, sInt, ValidInput

      Case SS_KD
        If spr.GetText(col, Row, var) Then pszValue = var
        If spr.GetText(7, Row, var) And IsNumeric(var) Then sRow = var
        sRc = GetParamElem3(sInt, pIdVal, c1, c2, c3, E_RANGE, range)
  
        ValidInput = ValidateType(VAL_FLOAT, pIdVal, spr, False)
        If ValidInput And pszValue <> "" Then
          ValidInput = CheckRange(pIdVal, pszValue, range)
        End If
        
        If Not ValidInput Then
'         sRc = GetParamElem(sInt, pIdVal, c1, c2, E_USERVAL, pszValue)
          pszValue = " "
          spr.SetText col, Row, CStr(pszValue)
        End If
        SetParamElem sInt, pIdVal, c1, c2, E_USERVAL, pszValue
        SetInputValid spr, col, Row, sInt, ValidInput
    End Select
    
  spr.ReDraw = True
End Function

