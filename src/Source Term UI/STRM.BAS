Attribute VB_Name = "SSFunctions"
Option Explicit
Option Compare Text

Public ErrorTag As String
Public KnownFlux As Boolean
Public ExecMode As Integer

Global parSelect As ParamStruct

Global Const KNOWN_ONLY = 1
Global Const COMPUTE_ONLY = 2
Global Const COMBINED = 3

Global Const BOOL_NO = "no"
Global Const BOOL_YES = "yes"

Global Const FIXED = 1

' constants sent to ValidateType
Global Const VAL_INT = 0
Global Const VAL_FLOAT = 1
Global Const VAL_DATE = 2

Global Const SS_DESC = 1
Global Const SS_CONC = 2
Global Const SS_VAL = 3
Global Const SS_UOM = 4
Global Const SS_REF = 5

Global Const SS_KD_CONT = 1
Global Const SS_KD_EST = 2
Global Const SS_KD_NUM = 3
Global Const SS_KD = 4
Global Const SS_KD_TIM = 5
Global Const SS_KD_REF = 6

Global Const SS_PID = 7
Global Const SS_C1 = 8
Global Const SS_C2 = 9
Global Const SS_C3 = 10
Global Const SS_ROW = 11
Global Const SS_RNG = 12
Global Const SS_MAX_COLS = 13

Global Const TAB_OPTIONS = 0
Global Const TAB_SITE = 1
Global Const TAB_OVL = 2
Global Const TAB_SUS = 3
Global Const TAB_HYD = 4
Global Const TAB_CLIM = 5
Global Const TAB_KD = 6
Global Const TAB_CONT = 7
Global Const TAB_REL = 8
Global Const TAB_FLUX = 9

Global Const LIGHT_GREY = "&HC0C0C0"
Global Const LOCK_BACK_COLOR = LIGHT_GREY
Global Const DARK_GREY = "&H808080"
Global Const WATER_COLOR = &HFF0000
Global Const SOIL_COLOR = &H4080
Global Const AIR_COLOR = &HC0C000
Global Const SS_SHADOWCOLOR = &H808000
Global Const SELECT_COLOR = &HFF0000
Global Const BLACK = &H0&
Global Const RED = &HFF&
Global Const GREEN = &HFF00&
Global Const BLUE = &HFF0000
Global Const WHITE = &HFFFFFF

Global Const SS_TYP_SINGLE = 1
Global Const SS_TYP_MATRIX = 2

' constants for soil composition
Global Const fSAND = 1
Global Const fSILT = 2
Global Const fCLAY = 3
Global Const fOMC = 4
Global Const fIRON = 5
Global Const fPH = 6

Global ssTabCtl() As fpSpread
Global TabEnabled() As Boolean

Sub SetTabEnabled(ByVal nTab%, ByVal enable As Boolean)
  TabEnabled(nTab) = enable
  frmMain.SSTab1.TabEnabled(nTab) = enable
End Sub
Function IsTabEnabled(ByVal nTab%) As Boolean
  IsTabEnabled = TabEnabled(nTab)
End Function


Function ConvertUOM(pszValue$, category$, intlUnit$, userunit$) As String
  
  If intlUnit <> userunit And IsNumeric(pszValue) Then
    ConvertUOM = convert(intlUnit, userunit, CDbl(pszValue))
  Else
    ConvertUOM = pszValue
  End If
End Function

Function cstrg(value$) As String
  cstrg = value
  If (0 < InStr(value, ".")) Then cstrg = Format(Val(value$), "0.0##E+00")
End Function

Function SplitRange(pszRange, item() As String, fNumeric() As Integer) As Integer
Dim sPos As Integer, sNext As Integer, sCount As Integer
Dim i As Integer

  sPos = 1
  sNext = InStr(sPos, pszRange, " ")
  sCount = 1
  Do While sNext > 0
    item(sCount) = Mid(pszRange, sPos, sNext - sPos)
    sPos = sNext + 1
    sNext = InStr(sPos, pszRange, " ")
    If "" <> item(sCount) Then
      sCount = sCount + 1
    End If
  Loop
  'put the last item in
  item(sCount) = Mid(pszRange, sPos)
  For i = 1 To sCount
    fNumeric(i) = IsNumeric(item(i))
  Next

  SplitRange = sCount
End Function

Function SprChangeToParamStruct(ss As fpSpread, col As Long, Row As Long) As Integer
Dim paramId As String
Dim sRc As Integer, ndx As Integer
Dim pszCategory As String
Dim pszValue As String, pszUnits As String, pszRange As String
Dim pszIntlValue As String, pszIntlUnit As String
Dim varValue As Variant
Dim DataType As String
Dim ssRow As Integer
Dim ChangeMade As Integer
Dim ssType As Integer
Dim ValidInput As Boolean
Dim idx3 As Integer
Dim idx4 As Integer
Dim ip As Integer

  If (Not vfProcessUserInput) _
    Or ss.CellType = SS_CELL_TYPE_CHECKBOX Then Exit Function
  
  ChangeMade = ss.ChangeMade
  ssType = ss.GetItemData()
  
  If ssType = SS_TYP_MATRIX Then
    ss.col = col
    ss.Row = ss.MaxRows: paramId = ss.Text
    ss.Row = 1: pszUnits = ss.Text
    ss.Row = Row: varValue = ss.Text
    idx3 = ss.GetRowItemData(Row)
    idx4 = 0
  Else
    ss.col = SS_PID: ss.Row = Row: paramId = ss.Text
    ss.col = SS_VAL: varValue = ss.Text
    ss.col = SS_UOM: pszUnits = ss.Text
    ss.col = SS_C1: If IsNumeric(ss.value) Then idx3 = ss.value
    ss.col = SS_C2: If IsNumeric(ss.value) Then idx4 = ss.value
  End If
  
  ss.Row = Row
  ss.col = col

  pszCategory = GetParamInfoElem(ip, paramId, E_CATEGORY)
  DataType = GetParamInfoElem(ip, paramId, E_DTYPE)
  
  sRc = GetParamElem(ndx, paramId, idx3, idx4, E_INTLUNIT, pszIntlUnit)
  sRc = GetParamElem(ndx, paramId, idx3, idx4, E_RANGE, pszRange)

  ValidInput = True

  If ssType <> SS_TYP_MATRIX Then ' SS_TYP_SINGLE

    Select Case col
      Case SS_VAL
        pszValue = varValue
        If ListCategory(pszCategory) Then
          pszValue = ListStructValue(pszCategory, CStr(varValue))
'         SetParamElem ndx, ParamId, idx3, idx4, E_INTLVAL, pszValue
          SetParamElem ndx, paramId, idx3, idx4, E_USERVAL, pszValue
        Else
          Select Case LCase$(DataType)
            Case "int", "integer"
              ValidInput = ValidateType(VAL_INT, paramId, ss, False)
              If ValidInput And pszValue <> "" And pszRange <> "" Then
                If Not CheckRange(paramId, pszValue, pszRange) Then ValidInput = False
              End If
            Case "bool"
              If pszValue = "1" Then pszValue = BOOL_YES Else pszValue = BOOL_NO
            Case "real", "float"
              ValidInput = ValidateType(VAL_FLOAT, paramId, ss, False)
              If ValidInput And pszValue <> "" And pszRange <> "" Then
                ValidInput = CheckRange(paramId, pszValue, pszRange)
              End If
          End Select
            
          If ValidInput Then
            SetParamElem ndx, paramId, idx3, idx4, E_USERVAL, pszValue
            SetParamElem ndx, paramId, idx3, idx4, E_INVALID, False
            SetInputValid ss, col, Row, ndx, True
          Else
            sRc = GetParamElem(ndx, paramId, idx3, idx4, E_USERVAL, pszValue)
            ss.SetText col, Row, CStr(pszValue)
          End If
        End If
        
      Case SS_UOM  ' UNITS
        SetParamElem ndx, paramId, idx3, idx4, E_USERUNIT, pszUnits
        pszRange = GetParamInfoElem(ip, paramId, E_RANGE) ' internal units
        pszRange = ConvertRange(pszRange, "", pszIntlUnit, pszUnits)
        SetParamElem ndx, paramId, idx3, idx4, E_RANGE, pszRange
        ' rewrite the range display for this parameter
        sRc = GetParamElem(ndx, paramId, idx3, idx4, E_SPR, ndx)
        vaSpreadClick ndx, col, Row, False
        ' validate the existing text
    End Select

  Else  ' SS_TYP_MATRIX

    Select Case Row
      Case 1 ' units
        pszRange = GetParamInfoElem(ip, paramId, E_RANGE)
        pszIntlUnit = GetParamInfoElem(ip, paramId, E_INTLUNIT)
        pszRange = ConvertRange(pszRange, "", pszIntlUnit, pszUnits)
        For idx3 = 1 To 12
          SetParamElem ndx, paramId, idx3, 0, E_USERUNIT, pszUnits
          SetParamElem ndx, paramId, idx3, 0, E_RANGE, pszRange
        Next
      Case Else
        pszValue = varValue
        Select Case LCase$(DataType)
          Case "int", "integer"
            ValidInput = ValidateType(VAL_INT, paramId, ss, False)
            If ValidInput And pszValue <> "" And pszRange <> "" Then
              ValidInput = CheckRange(paramId, pszValue, pszRange)
            End If
          Case "bool"
            If pszValue = "1" Then pszValue = BOOL_YES Else pszValue = BOOL_NO
          Case "real", "float"
            ValidInput = ValidateType(VAL_FLOAT, paramId, ss, False)
            If ValidInput And pszValue <> "" And pszRange <> "" Then
              ValidInput = CheckRange(paramId, pszValue, pszRange)
            End If
        End Select
            
        ' update structure
        SetInputValid ss, col, Row, ndx, ValidInput
        If ValidInput Then
          SetParamElem ndx, paramId, idx3, 0, E_USERUNIT, pszUnits
          SetParamElem ndx, paramId, idx3, 0, E_USERVAL, pszValue
          SetParamElem ndx, paramId, idx3, 0, E_INVALID, False
        Else
          GetParamElem ndx, paramId, idx3, 0, E_USERVAL, pszValue
          ss.SetText col, Row, CStr(pszValue)
        End If
    End Select
  End If
  SprChangeToParamStruct = ValidInput
End Function

Sub RewriteParamToSpr(paramId$, idx3%, idx4%)
Dim Row As Integer
Dim range As String
Dim intlUnit As String
Dim sRc As Integer
Dim ndx As Integer
Dim sprIndex As Integer
Dim sActive As Boolean
Dim col As Integer
Dim userunit As String
Dim category As String
Dim pszValue As String
Dim pszUserValue As String

  sRc = GetParamElem(ndx, paramId, idx3, idx4, E_SPR, sprIndex)
  sRc = GetParamElem(ndx, paramId, idx3, idx4, E_SSCOL, col)
  sRc = GetParamElem(ndx, paramId, idx3, idx4, E_SSROW, Row)
  sRc = GetParamElem(ndx, paramId, idx3, idx4, E_INTLUNIT, intlUnit)
  sRc = GetParamElem(ndx, paramId, idx3, idx4, E_ACTIVE, sActive)

  If SS_TYP_MATRIX = ssTabCtl(sprIndex).GetItemData() Then
    sRc = GetParamElem(ndx, paramId, idx3, idx4, E_USERVAL, pszValue)
    If Not ssTabCtl(sprIndex).CellType = SS_CELL_TYPE_CHECKBOX Then
      ssTabCtl(sprIndex).SetText col, Row, CStr(pszValue)
    End If
    ssTabCtl(sprIndex).Row = Row: ssTabCtl(sprIndex).col = col
    ssTabCtl(sprIndex).Action = 0 ' active cell
  Else
    sRc = GetParamElem(ndx, paramId, idx3, idx4, E_RANGE, range)

    ssTabCtl(sprIndex).Row = Row: ssTabCtl(sprIndex).col = 2
    WriteOneParamToSpr sprIndex, Row, paramId, idx3, idx4, range, intlUnit, "", sActive
  End If

End Sub

Function ConvertRange(pszRange$, pszCategory$, pszInUnit$, pszOutUnit$) As String

Dim op1 As String, op2 As String
Dim num1 As Variant, num2 As Variant, num3 As Variant
Dim D As Integer
Dim sCount As Integer, sStart As Integer
Static item(5) As String
Static fNumeric(5) As Integer
Dim rgszTmp() As String
Dim fValid As Integer
Dim i As Integer, sCt As Integer
Dim sSpace As Integer
Dim pszOldMin As String, pszOldMax As String
Dim pszNewMin As String, pszNewMax As String
Dim pszNewRange As String, pszRng As String
Dim sPos As Integer, sNext As Integer

Const SP = " "

  If (pszRange = "") Then
    ConvertRange = pszRange
    Exit Function
  End If
  
  sCount = SplitRange(pszRange, item(), fNumeric())
  
  If Not (sCount = 3 Or sCount = 5) Then
    MsgBox "Error in ConvertRange: " & pszRange
    ConvertRange = " "
    Exit Function
  End If
   
  If sCount = 3 And fNumeric(1) Then
    pszOldMin = CStr(item(1))
    pszNewMin = cstrg(ConvertUOM(pszOldMin, pszCategory, pszInUnit, pszOutUnit))
    pszNewRange = pszNewMin & SP & item(2) & SP & item(3)
  ElseIf sCount = 3 And fNumeric(3) Then
    pszOldMax = CStr(item(3))
    pszNewMax = cstrg(ConvertUOM(pszOldMax, pszCategory, pszInUnit, pszOutUnit))
    pszNewRange = item(1) & SP & item(2) & SP & pszNewMax
  Else
    pszOldMin = CStr(item(1))
    pszNewMin = cstrg(ConvertUOM(pszOldMin, pszCategory, pszInUnit, pszOutUnit))
    
    pszOldMax = CStr(item(5))
    pszNewMax = cstrg(ConvertUOM(pszOldMax, pszCategory, pszInUnit, pszOutUnit))
    If IsNumeric(pszNewMin) Then
      pszNewRange = pszNewMin & SP & item(2) & SP & item(3)
    End If
    If IsNumeric(pszNewMax) Then
      If pszNewRange = "" Then pszNewRange = item(3)
      pszNewRange = pszNewRange & SP & item(4) & SP & pszNewMax
    End If
  End If
  
  ConvertRange = pszNewRange
End Function

Function ValidateType(ByVal sType As Variant, paramId$, ThisControl As Control, ByVal fEmptyValid%) As Integer
Dim fRetVal As Integer, ip  As Integer, D As Integer
Dim pszValue As String, pszLabel As String, pszMsg As String
Dim sId As Integer

  pszValue = ThisControl.Text
  fRetVal = True
  
  pszLabel = GetParamInfoElem(ip, paramId, E_SCREEN) ' vptPInfo(ip).pszScreen
  
  If pszValue = "" And Not fEmptyValid Then
    fRetVal = False
  ElseIf pszValue = "" And fEmptyValid Then
    fRetVal = True
  ElseIf (sType = VAL_INT Or sType = VAL_FLOAT) And Not IsNumeric(pszValue) Then
    fRetVal = False
  End If
  pszMsg = "Please enter a value for " & pszLabel
  
  If fRetVal And sType = VAL_INT Then
    D = InStr(1, pszValue, ".")
    If D > 0 Then
    fRetVal = False
    pszMsg = "Please enter an integer for " & pszLabel
    End If
  End If
  
  If fRetVal And sType = VAL_DATE Then
    If Not IsDate(pszValue) Then
      fRetVal = False
      pszMsg = "Please enter a date for " & pszLabel
    End If
  End If
  
  If Not fRetVal Then
    MsgBox pszMsg, vbOKOnly + vbExclamation
    If TypeOf ThisControl Is TextBox Or TypeOf ThisControl Is ComboBox Then
    ThisControl.SetFocus
    End If
  End If
     
  ValidateType = fRetVal
  
End Function


Sub vaSpreadClick(Index As Integer, col As Long, Row As Long, click As Boolean)
Dim paramId As String
Dim value, dbl As Double
Dim sRc As Integer
Dim var As Variant
Dim enable As Boolean
Dim c1 As Integer, c2 As Integer, c3 As Integer
Dim range As String
Dim ndx As Integer

  parSelect.pname = ""
  parSelect.sprIndex = Index
  parSelect.ssRow = Row
  parSelect.ssCol = SS_REF
  
  frmMain.panRight.Caption = ""
  If Row <= 0 Or (Not vfProcessUserInput) Then Exit Sub
  Select Case Index
    Case TAB_CONT
      If click Then
        ssTabCtl(Index).col = col
        ssTabCtl(Index).Row = Row
        If ssTabCtl(Index).CellType = SS_CELL_TYPE_BUTTON Then
          frmConc.Show 1
          WriteContamParamsToSpr TAB_CONT
        End If
      End If
      If Not ssTabCtl(Index).GetText(SS_PID, Row, var) Then Exit Sub
      paramId = var
      If ssTabCtl(Index).GetText(SS_C1, Row, var) Then c1 = CInt(var)
      If ssTabCtl(Index).GetText(SS_C2, Row, var) Then c2 = CInt(var)
    
    Case TAB_REL
      If Not ssTabCtl(Index).GetText(SS_PID, Row, var) Then Exit Sub
      paramId = var
      If col = SS_KD_NUM Then paramId = paramId & "_NUM"
      If col = SS_KD_TIM Then paramId = paramId & "_TIM"
      
      If Not col = SS_KD_NUM Then
        If ssTabCtl(Index).GetText(SS_C1, Row, var) Then c1 = var
      End If
      c2 = 0
      c3 = 0
      parSelect.ssCol = SS_KD_REF

    Case TAB_KD, TAB_FLUX ' TAB_CONT
      If Not ssTabCtl(Index).GetText(SS_PID, Row, var) Then Exit Sub
      paramId = var
      If col = SS_KD_NUM Then paramId = paramId & "_NUM"
      If col = SS_KD_TIM Then paramId = paramId & "_TIM"
      
      If ssTabCtl(Index).GetText(SS_C1, Row, var) Then c1 = CInt(var)
      If ssTabCtl(Index).GetText(SS_C2, Row, var) Then c2 = CInt(var)
      If Not col = SS_KD_NUM Then
        If ssTabCtl(Index).GetText(SS_C3, Row, var) Then c3 = CInt(var)
      End If
      parSelect.ssCol = SS_KD_REF

    Case Else
      If SS_TYP_MATRIX = ssTabCtl(Index).GetItemData() Then
        ssTabCtl(Index).col = col
        ssTabCtl(Index).Row = ssTabCtl(Index).MaxRows
        paramId = ssTabCtl(Index).Text
        c1 = ssTabCtl(Index).GetRowItemData(Row)
        c2 = 0
        
        parSelect.ssRow = ssTabCtl(Index).MaxRows - 1
        parSelect.ssCol = col
      Else
        ssTabCtl(Index).col = SS_PID: ssTabCtl(Index).Row = Row: paramId = ssTabCtl(Index).Text
        ssTabCtl(Index).col = SS_C1: If IsNumeric(ssTabCtl(Index).value) Then c1 = ssTabCtl(Index).value
        ssTabCtl(Index).col = SS_C2: If IsNumeric(ssTabCtl(Index).value) Then c2 = ssTabCtl(Index).value
      End If
  End Select
  
  If GetParamElem3(ndx, paramId, c1, c2, c3, E_RANGE, range) Then
    If range <> "" Then frmMain.panRight.Caption = Space(2) & "Range: " & range
  End If

  ' save parameter keys for help and references
  parSelect.pname = paramId
  parSelect.c1 = c1
  parSelect.c2 = c2
  parSelect.c3 = c3
  GetParamElem3 ndx, paramId, c1, c2, c3, E_REF, parSelect.ref

End Sub


Sub CommentRow(sprIndex%, Row%, comment)

  ssTabCtl(sprIndex).Row = Row
  ssTabCtl(sprIndex).col = 1
  ssTabCtl(sprIndex).ForeColor = QBColor(1) ' blue
  ssTabCtl(sprIndex).SetText 1, Row, comment
  Row = Row + 1
End Sub




Sub WriteMatrixToSpr(sprIndex%, ssCol%, count%, paramId$, pszParam$, pszRange$, pszRangeUnit$, pszSubType$, sActive%)
Dim i As Integer, idx As Integer, fActive As Integer, ip As Integer
Dim category As String, pszUserValue As String
Dim intlUnit  As String, userunit As String, pszValue As String
Dim ssRow As Integer
Dim sprRow As Integer
Dim range As String
Dim FormType As String
Dim FormTitle As String
Dim sSeq As Integer
Dim spr As fpSpread
Dim uct As Integer
Dim ulist As String
Dim invalid As Boolean
Dim rangeUnit As String
Dim pRef As Integer

  Set spr = ssTabCtl(sprIndex)
  spr.ColWidth(1) = 10
  ssRow = 0
  spr.Row = ssRow

  If sActive = -1 Then fActive = True Else fActive = False
  
' category = GetParamInfoElem(ip, paramId, E_CATEGORY)
  intlUnit = GetParamInfoElem(ip, paramId, E_INTLUNIT)
  If Not GetParamElem(idx, paramId, 1, 0, E_USERUNIT, userunit) _
    Or (category <> "" And userunit = "") Then
    userunit = GetParamInfoElem(ip, paramId, E_USERUNIT)
  End If
  
  range = GetParamInfoElem(ip, paramId, E_RANGE)
  If range = "" Then GetParamElem idx, paramId, 1, 0, E_RANGE, range
  If range <> "" Then
    rangeUnit = GetParamInfoElem(ip, paramId, E_INTLUNIT)
    range = ConvertRange(range, "", rangeUnit, userunit)
  End If
  
  spr.Row = 0: spr.col = -1: spr.TypeHAlign = SS_CELL_H_ALIGN_LEFT
  For sSeq = 1 To count
    spr.SetText 1, sSeq + 3, Format$(CStr(sSeq) & "/01/01", "mmmm")
  Next

  spr.SetText 1, ssRow, "Param"
  spr.SetText ssCol, ssRow, CStr(pszParam)

  spr.SetText 1, ssRow + 1, "Unit"
  spr.Row = ssRow + 1
  spr.col = ssCol
     If intlUnit <> "" Then
    uct = get_conversion_list(userunit, ulist)
    If uct > 1 Then
      spr.CellType = 8 ' combobox
      spr.TypeComboBoxList = ulist
      spr.TypeComboBoxCurSel = 0
      spr.Lock = False
    Else
      spr.SetText ssCol, ssRow + 1, CStr(userunit)
    End If
  End If
    
  spr.SetText ssCol, ssRow + 2, CStr(paramId)

  spr.col = ssCol
  ssRow = ssRow + 3
  For sSeq = 1 To count
    
    ssRow = ssRow + 1
    spr.Row = ssRow

    spr.SetRowItemData ssRow, sSeq
    SetParamElemSprAttr idx, paramId, sSeq, 0, 0, sprIndex, ssCol, ssRow, True
    SetParamElem idx, paramId, sSeq, 0, E_RANGE, range
    SetParamElem idx, paramId, sSeq, 0, E_USERUNIT, userunit

    If Not GetParamElem(idx, paramId, sSeq, 0, E_USERVAL, pszValue) Then
      SetParamElem idx, paramId, sSeq, 0, E_USERVAL, ""
      pszValue = ""
    End If
'   i = GetParamElem(idx, paramId, sSeq, 0, E_USERUNIT, userUnit)
    i = GetParamElem(idx, paramId, sSeq, 0, E_INVALID, invalid)
    i = GetParamElem(idx, paramId, sSeq, 0, E_REF, pRef)
    If ListCategory(category) Then
      spr.CellType = SS_CELL_TYPE_COMBOBOX
      WriteComboToSSv2 spr, category, pszValue
    Else
      Select Case GetParamInfoElem(ip, paramId, E_DTYPE)
        Case "bool"
          spr.CellType = SS_CELL_TYPE_CHECKBOX
          spr.TypeCheckCenter = True
          If pszValue = "yes" Then
            spr.value = 1
          Else
            spr.value = 0
          End If
        Case "real"
          spr.CellType = SS_CELL_TYPE_EDIT
        Case "int"
          spr.CellType = SS_CELL_TYPE_INTEGER
          spr.TypeHAlign = SS_CELL_H_ALIGN_LEFT
        Case "char"
          spr.CellType = SS_CELL_TYPE_EDIT
        Case Else
          spr.CellType = SS_CELL_TYPE_EDIT
      End Select
      If Not spr.CellType = SS_CELL_TYPE_CHECKBOX Then
        spr.SetText ssCol, ssRow, CStr(pszValue)
      End If
    End If
    spr.Row = ssRow
    spr.col = ssCol
    If fActive Then spr.Lock = False
    If sActive = FIXED Then spr.Lock = True
    If spr.Lock And spr.CellType = SS_CELL_TYPE_CHECKBOX Then
      spr.BackColor = LOCK_BACK_COLOR
    End If
    If Not spr.Lock Then spr.BackColor = WHITE
    
    SetInputValid spr, ssCol, ssRow, idx, (pszValue <> "" And Not invalid)

  Next

  If spr.MaxRows <> ssRow + 1 Then
    spr.MaxRows = ssRow + 3
    spr.Row = spr.MaxRows
    spr.RowHidden = True
  End If
  
  spr.SetText 1, spr.MaxRows - 1, "Reference"
  spr.SetText ssCol, spr.MaxRows - 1, pRef
  spr.SetText ssCol, spr.MaxRows, CStr(paramId)
  spr.VisibleRows = spr.MaxRows - 1
End Sub

Sub WriteOneParamToSpr(sprIndex%, Row%, paramId$, c1%, c2%, range$, rangeUnit$, SubType$, sActive As Boolean)
Dim ndx As Integer
Dim i As Integer, idx As Integer, fActive As Integer, ip As Integer
Dim pszCategory As String, pszUserValue As String
Dim pszIntlUnit As String, pszUserUnit As String, pszValue As String
Dim DataType As String
Dim DefValue As String
Dim Status As Integer
Dim ScreenName As String
Dim ssCol As Integer
Dim spr As fpSpread
Dim pInfo As ParamInfoStruct
Dim pStruct As ParamStruct
Dim ulist As String
Dim uct As Integer

  Set spr = ssTabCtl(sprIndex)

  ip = GetParamInfoStruct(paramId, pInfo)
  
  pStruct.pname = pInfo.pszName
  pStruct.c1 = c1
  pStruct.c2 = c2
  
  idx = GetParamStruct(pStruct)
  If 0 = idx Then
    pStruct.pname = pInfo.pszName
    pStruct.c1 = c1
    pStruct.c2 = c2
    pStruct.intlUnit = pInfo.pszIntlUnit
    pStruct.userunit = pInfo.pszIntlUnit
    pStruct.value = pInfo.pszDefValue
    pStruct.userVal = pInfo.pszDefValue
    pStruct.intlval = pInfo.pszDefValue
    idx = SetParamStruct(pStruct)
  End If
  
  pStruct.ssRow = Row
  pStruct.ssCol = SS_VAL
  pStruct.sprIndex = sprIndex
  pStruct.SubType = SubType
  pStruct.fActive = sActive
  idx = SetParamStruct(pStruct)
  
  spr.Row = Row
  spr.col = -1
  spr.Lock = True
  
  spr.SetRowItemData Row, Row
  spr.SetColItemData ssCol, SS_VAL
  spr.SetText SS_PID, Row, CStr(paramId)
  spr.SetText SS_C1, Row, CStr(pStruct.c1)
  spr.SetText SS_C2, Row, CStr(pStruct.c2)
  spr.SetText SS_DESC, Row, CStr(pInfo.pszScreen) + " -- " + paramId
  
  spr.col = SS_VAL
  spr.Lock = Not sActive
  If ListCategory(pInfo.pszCategory) Then
    spr.CellType = SS_CELL_TYPE_COMBOBOX
    WriteComboToSSv2 spr, pInfo.pszCategory, pStruct.userVal
    SetParamElem idx, paramId, c1, c2, E_USERVAL, pStruct.userVal
  Else
    spr.SetText SS_VAL, Row, CStr(pStruct.userVal)
    
    SetInputValid spr, SS_VAL, Row, idx, _
      Not (pStruct.fActive And (pStruct.userVal = "" Or pStruct.invalid))
    
    If pInfo.pszIntlUnit <> "" Then
      uct = get_conversion_list(pInfo.pszIntlUnit, ulist)
      If uct > 1 Then
        spr.col = SS_UOM
        spr.CellType = 8 ' combobox
        spr.TypeComboBoxList = ulist

'       spr.TypeComboBoxCurSel = 0
'       this takes care of changes in unit categories between gid and pinfo
'         ex: change in stssol from 'g/cm^3' to 'percent'
        spr.Text = pStruct.userunit
        If (spr.Text = "") Then
          spr.TypeComboBoxCurSel = 0
          SetParamElem idx, paramId, 0, 0, E_USERUNIT, spr.Text
          SetParamElem idx, paramId, 0, 0, E_INTLUNIT, pInfo.pszIntlUnit
        End If
        
        spr.Lock = Not sActive
      Else
        spr.SetText SS_UOM, Row, CStr(pStruct.userunit)
      End If
    End If
  End If
  spr.SetText SS_REF, Row, pStruct.ref
  
  spr.col = SS_RNG
  range = pInfo.range
  If range = "" Then Status = GetParamElem(idx, paramId, c1, c2, E_RANGE, range)
  If range <> "" Then
    rangeUnit = pStruct.intlUnit
    range = ConvertRange(range, pszCategory, rangeUnit, pStruct.userunit)
  End If
  SetParamElem idx, paramId, c1, c2, E_RANGE, range
  
  Row = Row + 1
End Sub

Sub WriteComboToSSv2(ss As Control, pszCategory$, pszValue$)
Dim pszList As String, sIndex As Integer, sCt  As Integer, i As Integer
Dim catValue() As String
Dim catText() As String

  pszList = ""
  sIndex = 0
  
  If GetListCategoryValues(pszCategory, catValue(), catText()) Then
    For i = 1 To UBound(catValue)
      If 0 < Len(pszList) Then pszList = pszList & Chr(9)
      pszList = pszList & catText(i)
      If pszValue = catValue(i) Then
        sIndex = i - 1
      End If
    Next
  End If

  ss.TypeComboBoxList = pszList
  ss.TypeComboBoxIndex = sIndex
  ss.TypeComboBoxCurSel = sIndex
  pszValue = ListStructValue(pszCategory, ss.Text)
End Sub

Function CheckRange(vParam As Variant, value As Variant, pszRange As String) As Integer
' pszRange must be of one of the following forms:
'     num1 op x op num2
'     num1 op x
'     x op num2
' where op is < or <=
' There must be one and only one space between these entities
Dim op1 As String, op2 As String
Dim num1 As Variant, num2 As Variant
Dim D As Integer
Dim sCount As Integer
Static item(5) As String
Static fNumeric(5) As Integer
Dim fValid As Integer
Dim r1 As Double, r3 As Double, r5 As Double
Dim pszParam As String
Dim sStart As Integer, i As Integer
Dim sSpace As Integer, pszMsg As String, pszTmp As String, pszRng As String
Dim rgszTmp() As String, sCt As Integer

  CheckRange = True
  
  If pszRange = "" Then Exit Function

  pszRng = pszRange

  Select Case VarType(vParam)
    Case vbInteger
      pszParam = GetParamInfoElem(i, CInt(vParam), E_SCREEN)
    Case vbString
      pszParam = CStr(vParam)
  End Select

  sCount = SplitRange(pszRange, item(), fNumeric())

  If Not (sCount = 3 Or sCount = 5) Then
    MsgBox "Invalid range detected in CheckRange", vbOKOnly + vbCritical
    CheckRange = False
    Exit Function
  End If
   
  fValid = True
  
  ' check validity of first range
  If Not ((item(2) = "<" Or item(2) = "<=") And (fNumeric(1) Or fNumeric(3))) Then
    MsgBox "Invalid range detected in CheckRange", vbOKOnly + vbCritical
    CheckRange = False
    Exit Function
  End If
  
  If fNumeric(1) Then  ' replace the "x"
    item(3) = CDbl(value)
  Else
    item(1) = CDbl(value)
  End If
  
  r1 = CDbl(item(1))
  r3 = CDbl(item(3))
  If item(2) = "<" Then
    fValid = (r1 < r3)
  Else
    fValid = (r1 <= r3)
  End If
  
  ' check validity of second range
  If sCount = 5 And fValid Then
    If Not ((item(4) = "<" Or item(4) = "<=") And fNumeric(5)) Then
    MsgBox "Invalid range detected in CheckRange", vbOKOnly + vbCritical
    CheckRange = False
    Exit Function
    End If
  
    If Not fNumeric(5) Then item(5) = CDbl(value) ' replace "x"
  
    r3 = CDbl(item(3))
    r5 = CDbl(item(5))
    If item(4) = "<" Then
    fValid = (r3 < r5)
    Else
    fValid = (r3 <= r5)
    End If
  End If
  
  If Not fValid Then
  '  pszMsg = "Please enter " & pszParam & " in the range " & pszRange
  '  MsgBox pszMsg, vbOKOnly + vbExclamation
    Beep
  End If
  
  CheckRange = fValid

End Function


Sub vaSpreadChange(Index As Integer, col As Long, Row As Long)
Dim paramId
Dim value
Dim sRc As Integer
Dim var As Variant
Dim enable As Boolean

  sRc = ssTabCtl(Index).GetText(SS_PID, Row, paramId)
  sRc = ssTabCtl(Index).GetText(SS_VAL, Row, value)
  Select Case Index
    Case TAB_REL
      sRc = SprChangeToSeries(Index, CInt(col), CInt(Row))
    Case TAB_KD, TAB_FLUX ' TAB_CONT
       sRc = SprChangeToContamSeries(Index, CInt(col), CInt(Row))
    Case Else
      sRc = SprChangeToParamStruct(ssTabCtl(Index), col, Row)
      CalcTabInputDependencies CStr(paramId)
  End Select
End Sub


Sub ssParamsInit(spr As Control)
Dim i As Integer
  spr.CursorType = SS_CURSOR_TYPE_DEFAULT
  spr.CursorStyle = SS_CURSOR_STYLE_ARROW

  spr.UserResize = False
  
  spr.EditEnterAction = SS_CELL_EDITMODE_EXIT_DOWN
  
  spr.UnitType = 2
  spr.Width = 9030

  spr.UnitType = 1
  spr.MaxCols = SS_MAX_COLS
  spr.MaxRows = 500
  spr.Row = -1
  spr.col = -1
  spr.Action = SS_ACTION_CLEAR_TEXT
  spr.CellType = SS_CELL_TYPE_EDIT
  spr.BackColor = WHITE ' LIGHT_GREY
  
  spr.DisplayRowHeaders = False
  spr.VisibleRows = 20
  spr.VisibleCols = SS_REF
  spr.GridSolid = False
  spr.GridColor = QBColor(0)

  spr.ColWidth(SS_DESC) = 43 ' 40 ' 38
  spr.ColWidth(SS_VAL) = 12
  spr.ColWidth(SS_UOM) = 12
  spr.ColWidth(SS_REF) = 6 ' 9 ' 8
  spr.AutoSize = True
  
  spr.SetText SS_DESC, 0, "Description"
  spr.SetText SS_VAL, 0, "Value"
  spr.SetText SS_UOM, 0, "Unit"
  spr.SetText SS_REF, 0, "Ref." ' "Reference"

  spr.Row = -1
  For i = 0 To SS_MAX_COLS
    spr.col = i
    Select Case i
      Case SS_C1, SS_C2
        spr.CellType = SS_CELL_TYPE_INTEGER
      Case SS_DESC, SS_PID, SS_RNG
        spr.CellType = SS_CELL_TYPE_STATIC_TEXT
        spr.TypeTextWordWrap = True
        spr.BackColor = LIGHT_GREY
      Case SS_VAL, SS_UOM
        spr.CellType = SS_CELL_TYPE_EDIT
        spr.LockBackColor = LIGHT_GREY
        spr.Lock = True
      Case SS_KD_EST
        spr.ColHidden = True
      Case SS_REF
        spr.CellType = SS_CELL_TYPE_STATIC_TEXT
        spr.BackColor = LIGHT_GREY
        spr.TypeHAlign = 2 ' center
      Case Else
        spr.CellType = SS_CELL_TYPE_STATIC_TEXT
        spr.BackColor = LIGHT_GREY
    End Select
    If i > SS_REF Then spr.ColHidden = True
  Next
  spr.Protect = True

End Sub

Sub ConstructOverlandSS(sprIndex%)
Dim rc As Integer, ndx As Integer, intval As String, strval As String
Dim ip As Integer, opval As String
Dim category As String
Dim spr As fpSpread
Dim sRow As Integer
Dim ovl As Boolean, ovlknown As Boolean
Dim vol As Double
Dim optVal As Integer
Dim msg As String
  
  rc = GetParamElem(ndx, "STOVL_OP", 0, 0, E_USERVAL, intval)
  optVal = Val(intval)
  category = GetParamInfoElem(ip, "STOVL_OP", E_CATEGORY)
  opval = ListStructText(category, intval)
' ovl = InStr(opval, "compute")
' ovlknown = 0 <> InStr(opval, "flux") Or 0 <> InStr(opval, "erosion") ' not discharge
' ovlknown = 0 <> InStr(opval, "known")
  ovl = (optVal = OPT_COMP)
  ovlknown = (optVal = OPT_REL Or optVal = OPT_FLX)
  
  rc = GetParamElem(ndx, "STPOND", 0, 0, E_USERVAL, intval)
  ponded = (intval = BOOL_YES)
  
' Overland input only for computed
' ovl = ovl Or ovlknown Or ponded
  SetTabEnabled TAB_OVL, ovl Or ovlknown
  
  ResetParamsSprIndex sprIndex
  
  If Not (ovl Or ovlknown) Then Exit Sub
  
  Set spr = ssTabCtl(sprIndex)
  spr.SetItemData SS_TYP_SINGLE

  ssParamsInit spr
          
  sRow = 1
    
' If InStr(opval, "flux") Then
  If (optVal = OPT_FLX) Then
    WriteOneParamToSpr sprIndex, sRow, "STSEDLOD", 1, 0, "0 <= x", "", "", True
  End If
  
' If InStr(opval, "known") Then ' include 'known flow rate' BLH 9/97
  If (optVal = OPT_REL Or optVal = OPT_FLX) Then
    If ponded Then
      If GetVolume(vol, "m") Then SetParamInfoElem ndx, "STOVLFLO", E_RANGE, "0 <= x <= " & convert("", "", vol)
    End If
    WriteOneParamToSpr sprIndex, sRow, "STOVLFLO", 1, 0, "0 <= x", "", "", True
    If ponded Then
      WriteOneParamToSpr sprIndex, sRow, "STOVLFLX", 0, 0, "0 <= x <= 1", "", "", False
    End If
  End If
' If InStr(opval, "compute") Then
  If (optVal = OPT_COMP) Then
    WriteOneParamToSpr sprIndex, sRow, "STSTORMI", 0, 0, "0 <= x", "", "", True
    WriteOneParamToSpr sprIndex, sRow, "STPRECIP", 0, 0, "0 <= x", "", "", True
    WriteOneParamToSpr sprIndex, sRow, "STSLOPE", 0, 0, "0 <= x", "", "", True
    WriteOneParamToSpr sprIndex, sRow, "STKFACTR", 0, 0, ".02 <= x <= 1.0", "", "", True
    WriteOneParamToSpr sprIndex, sRow, "STCFACTR", 0, 0, ".01 <= x <= 1.5", "", "", True
    WriteOneParamToSpr sprIndex, sRow, "STPFACTR", 0, 0, ".01 <= x <= 1.5", "", "", True
    WriteOneParamToSpr sprIndex, sRow, "STSLENGTH", 0, 0, "0 <= x", "", "", True
'   If Not OpValue("STSUS_OP", "compute") Then
    If Not OpValue("STSUS_OP", OPT_COMP) Then
      WriteOneParamToSpr sprIndex, sRow, "STSBULKD", 0, 0, "0 <= x", "", "", True
    End If

  End If
  spr.MaxRows = sRow - 1
  
    
  IsInputComplete sprIndex, strval, msg  ' check the data

  spr.ReDraw = True
  
End Sub
Sub ConstructSuspensionSS(sprIndex%)
Dim spr As fpSpread
Dim sRow As Integer
Dim enable As Boolean
Dim rc As Integer, intval As String, strval As String
Dim ndx As Integer
Dim msg As String

  Set spr = ssTabCtl(sprIndex)
  spr.SetItemData SS_TYP_SINGLE
          
  ssParamsInit spr
  ResetParamsSprIndex sprIndex

  spr.ColWidth(SS_VAL) = spr.ColWidth(SS_VAL) + 2
  spr.ColWidth(SS_UOM) = spr.ColWidth(SS_UOM) - 2
  
  sRow = 1
  If EnableParam("STRELHT") Then
    WriteOneParamToSpr sprIndex, sRow, "STRELHT", 0, 0, "0 <= x", "", "", True
    WriteOneParamToSpr sprIndex, sRow, "STSTRUCHT", 0, 0, "0 <= x", "", "", True
    WriteOneParamToSpr sprIndex, sRow, "STEXVEL", 0, 0, "0 <= x", "", "", True
    WriteOneParamToSpr sprIndex, sRow, "STEXTEMP", 0, 0, "0 <= x", "", "", True
  End If
' If OpValue("STSUS_OP", "compute") Then
  If OpValue("STSUS_OP", OPT_COMP) Then
    WriteOneParamToSpr sprIndex, sRow, "STSBULKD", 0, 0, "0 <= x", "", "", True
    WriteOneParamToSpr sprIndex, sRow, "STCORRSC", 0, 0, "0 <= x", "", "", True
    WriteOneParamToSpr sprIndex, sRow, "STLOCSUR", 0, 0, "0 <= x", "", "", True
    WriteOneParamToSpr sprIndex, sRow, "STVEGFR", 0, 0, "0 <= x", "", "", True
    WriteOneParamToSpr sprIndex, sRow, "STCRUST", 0, 0, "0 <= x", "", "", True
    WriteOneParamToSpr sprIndex, sRow, "STNUMDIS", 0, 0, "0 <= x", "", "", True
    WriteOneParamToSpr sprIndex, sRow, "STMAXWIND", 0, 0, "0 <= x", "", "", True
    WriteOneParamToSpr sprIndex, sRow, "STPEI", 0, 0, "", "0 <= x", "", True
    WriteOneParamToSpr sprIndex, sRow, "STSDISTB", 0, 0, "", "", "", True
    
    enable = EnableParam("STROADS")
'   If OpValue("STINF_OP", "off") And OpValue("STOVL_OP", "off") Then
    If OpValue("STINF_OP", OPT_OFF) And OpValue("STOVL_OP", OPT_OFF) Then
      WriteOneParamToSpr sprIndex, sRow, "STNUMPRCP", 0, 0, "0 <= x", "", "ROADS_ANY", enable
    End If
    
    sRow = sRow + 1
    WriteOneParamToSpr sprIndex, sRow, "STROADS", 0, 0, "", "", "", True
    
    enable = EnableParam("STROADS")
  ' hide until source term implementation is correct
    spr.SetText 1, sRow, "     Paved Roadways"
    sRow = sRow + 1
''''''     silt_massload     = f_read("stsiltl",2,0,0,0,0,0);
''''''     vehicle_weight    = f_read("stvweigh",2,0,0,0,0,0);
''''''     num_rain_days     = f_read("stnumprcp",0,0,0,0,0,0);
''''''     dist_travel       = f_read("strtdist",2,0,0,0,0,0);
''''''     num_vehicle_day   = f_read("strtnum",2,0,0,0,0,0);
    
    WriteOneParamToSpr sprIndex, sRow, "STRTDIST", 2, 0, "0 <= x", "", "ROADS_PAVE", enable
    WriteOneParamToSpr sprIndex, sRow, "STVWEIGH", 2, 0, "0 <= x", "", "ROADS_PAVE", enable
    WriteOneParamToSpr sprIndex, sRow, "STRTNUM", 2, 0, "0 <= x", "", "ROADS_PAVE", enable
    WriteOneParamToSpr sprIndex, sRow, "STSILTL", 2, 0, "0 <= x", "", "ROADS_PAVE", enable

    spr.SetText 1, sRow, "     Unpaved Roadways"
    sRow = sRow + 1
''''''     silt_percent      = f_read("stsilt",1,0,0,0,0,0);
''''''     vehicle_speed     = f_read("stvspeed",1,0,0,0,0,0);
''''''     vehicle_weight    = f_read("stvweigh",1,0,0,0,0,0);
''''''     num_rain_days     = f_read("stnumprcp",0,0,0,0,0,0);
''''''     dist_travel       = f_read("strtdist",1,0,0,0,0,0);
''''''     num_vehicle_day   = f_read("strtnum",1,0,0,0,0,0);
''''''     unpav_dry_moist   = f_read("stdrysco",1,0,0,0,0,0);
    
    WriteOneParamToSpr sprIndex, sRow, "STRTDIST", 1, 0, "0 <= x", "", "ROADS_DIRT", enable
    WriteOneParamToSpr sprIndex, sRow, "STVSPEED", 1, 0, "0 <= x", "", "ROADS_DIRT", enable
    WriteOneParamToSpr sprIndex, sRow, "STVWEIGH", 1, 0, "0 <= x", "", "ROADS_DIRT", enable
    WriteOneParamToSpr sprIndex, sRow, "STRTNUM", 1, 0, "0 <= x", "", "ROADS_DIRT", enable
    WriteOneParamToSpr sprIndex, sRow, "STSILT", 1, 0, "", "", "ROADS_DIRT", enable
    WriteOneParamToSpr sprIndex, sRow, "STDRYSCO", 1, 0, "", "", "ROADS_DIRT", enable
    
    CalcTabInputDependencies "STROADS"

  End If

  spr.MaxRows = sRow - 1
  IsInputComplete sprIndex, strval, msg  ' check the data
  spr.ReDraw = True

End Sub
Sub ConstructWeatherStationSS(sprIndex%)
Dim spr As fpSpread
Dim sRow As Integer
Dim sRc As Integer
Dim ndx As Integer
Dim value As String
Dim inf As Integer
Dim msg As String

    Set spr = ssTabCtl(sprIndex)
    spr.SetItemData SS_TYP_SINGLE
    
    ssParamsInit spr
    sRow = 1
    
    sRc = GetParamElem(ndx, "STINF_OP", 0, 0, E_USERVAL, value)
    inf = (3 = CInt(value)) ' InStr(value, "compute")
    
'   frmMain.SSTab1.TabCaption(sprIndex) = "Infiltration"
    
    WriteOneParamToSpr sprIndex, sRow, "STLCDELEV", 0, 0, "0 <= x", "", "", True
    WriteOneParamToSpr sprIndex, sRow, "STLAT", 0, 0, "0 <= x", "", "", True
    WriteOneParamToSpr sprIndex, sRow, "STELEV", 0, 0, "0 <= x", "", "", True
    sRow = sRow + 1
    WriteOneParamToSpr sprIndex, sRow, "STSCSCN", 0, 0, "0 <= x", "", "", True
    WriteOneParamToSpr sprIndex, sRow, "STAVAILW", 0, 0, "0 <= x", "", "", True
    WriteOneParamToSpr sprIndex, sRow, "STNUMPRCP", 0, 0, "0 <= x", "", "", True
    
    spr.MaxRows = sRow - 1
    
    IsInputComplete sprIndex, value, msg

    spr.ReDraw = True
End Sub
Sub ConstructMonthlyClimateSS(sprIndex%)
Dim spr As fpSpread
Dim sRow As Integer
Dim j As Integer, pId As String
Dim msg As String

  Set spr = ssTabCtl(sprIndex)
          
'    Form1.SSTab1.TabCaption(sprIndex) = "Monthly Climatology"

  spr.SetItemData SS_TYP_MATRIX
  
  spr.EditEnterAction = SS_CELL_EDITMODE_EXIT_DOWN
' spr.VisibleRows = 17
  spr.MaxRows = 50
  spr.VisibleRows = spr.MaxRows
  spr.Row = -1
  spr.col = -1
  spr.CellType = SS_CELL_TYPE_STATIC_TEXT
  spr.AllowCellOverflow = False ' True
  spr.ProcessTab = True
  spr.MaxCols = 8
  spr.VisibleCols = 8
  spr.AutoSize = True
  spr.GridSolid = False
  spr.GridColor = BLACK
  spr.GridShowVert = True
  spr.GridShowHoriz = True
  spr.EditModeReplace = True
  spr.ScrollBars = SS_SCROLLBAR_V_ONLY
  spr.ScrollBarExtMode = True
  spr.Row = -1
  For j = 1 To spr.MaxCols
    spr.ColWidth(j) = 9
    spr.col = j
    Select Case j
      Case 1
        spr.CellType = SS_CELL_TYPE_STATIC_TEXT
        spr.TypeTextWordWrap = True
        spr.BackColor = LIGHT_GREY
        spr.ForeColor = BLACK
      Case Else
        spr.CellType = SS_CELL_TYPE_EDIT
        spr.LockBackColor = LIGHT_GREY
        spr.LockForeColor = BLACK
        spr.Lock = True
    End Select
  Next
  sRow = 1
  WriteMatrixToSpr sprIndex, 2, 12, "STTEMP", "temp", "-50 <= x <= 50", "C", "", True
  WriteMatrixToSpr sprIndex, 3, 12, "STMPRECIP", "precip", "0 <= x", "cm", "", True
  WriteMatrixToSpr sprIndex, 6, 12, "STMNUMPRE", "precip days", "0 <= x", "", "", True
  WriteMatrixToSpr sprIndex, 4, 12, "STWINDV", "windsp", "0 <= x", "m/s", "", True
  WriteMatrixToSpr sprIndex, 5, 12, "STCLOUD", "cloudy", "0 <= x <= 1", "fraction", "", True
  WriteMatrixToSpr sprIndex, 8, 12, "STRHMAX", "max humid", "0 <= x <= 100", "%", "", True
  WriteMatrixToSpr sprIndex, 7, 12, "STRHMIN", "min humid", "0 <= x <= 100", "%", "", True

  spr.SetItemData SS_TYP_MATRIX
  
  IsInputComplete sprIndex, pId, msg ' check the data


  spr.ReDraw = True


End Sub
Sub ConstructReleaseSiteSS(sprIndex%)
Dim spr As fpSpread
Dim sRow As Integer
Dim sRc As Integer
Dim wstfrm As Integer
Dim ndx As Integer
Dim intlval As String
Dim enable As Integer
Dim media As String
Dim pId As String
Dim msg As String
  
  Set spr = ssTabCtl(sprIndex)
  spr.SetItemData SS_TYP_SINGLE

  spr.ReDraw = False
                       
  ssParamsInit spr
  ResetParamsSprIndex sprIndex
    
  sRow = 1
  spr.SetItemData SS_TYP_SINGLE
    
  ponded = False
  wstfrm = False

  If GetParamElem(ndx, "STPOND", 0, 0, E_USERVAL, intlval) Then
    ponded = (intlval = BOOL_YES)
  End If
  If GetParamElem(ndx, "STREMSITE", 0, 0, E_USERVAL, intlval) Then
    wstfrm = (intlval = BOOL_YES)
  End If
  sRc = GetParamElem(ndx, "STMEDIA", 0, 0, E_USERVAL, media)
  
  'SpreadSetText spr, SS_DESC, sRow, "Waste Zone Characteristics"
  'sRow = sRow + 1
  
  If Not ponded Then
    WriteOneParamToSpr sprIndex, sRow, "STCLEAN", 1, 0, "0 <= x", "", "STPOND", True
  Else
    SetPathwayParam ndx, "STCLEAN", 1, 0, 0, "0", sprIndex
  End If
  WriteOneParamToSpr sprIndex, sRow, "STTHICK", 0, 0, "0 <= x", "", "", True
  WriteOneParamToSpr sprIndex, sRow, "STLENGTH", 0, 0, "0 <= x", "", "", True
  WriteOneParamToSpr sprIndex, sRow, "STWIDTH", 0, 0, "0 <= x", "", "", True
' WriteOneParamToSpr sprIndex, sRow, "STMIXDEP", 0, 0, "", "", "", True

  If Not ponded Then
    sRow = sRow + 1
    WriteOneParamToSpr sprIndex, sRow, "STZBULKD", 0, 0, "0 <= x", "", "", True '
  End If
  
  Select Case media
    Case "surface water"
      WriteOneParamToSpr sprIndex, sRow, "STSSOL", 0, 0, "", "", "STPOND", ponded
    Case "aquifer", "soil"
      WriteOneParamToSpr sprIndex, sRow, "STTOTPOR", 1, 0, "0 <= x", "", "STREMSITE", Not wstfrm
      Select Case media
        Case "aquifer"
          SetRange "STEFFPOR"
          SetParamElem ndx, "STAIRSPC", 1, 0, E_USERVAL, "0"
          WriteOneParamToSpr sprIndex, sRow, "STEFFPOR", 0, 0, "", "", "", True
        Case "soil"
          WriteOneParamToSpr sprIndex, sRow, "STMOISTC", 0, 0, "0 <= x", "", "", True
          WriteOneParamToSpr sprIndex, sRow, "STAIRSPC", 1, 0, "0 <= x", "", "STREMSITE", False
      End Select
      CalcTabInputDependencies "STTOTPOR"
  End Select
  
' sRow = sRow + 1
' SpreadSetText spr, SS_DESC, sRow, "Site Characteristics"
  sRow = sRow + 1
  WriteOneParamToSpr sprIndex, sRow, "STAVTEMP", 0, 0, "0 <= x", "", "", True
' Aug03 - conditional STWINDHT
  If OpValue("STINF_OP", OPT_COMP) Or OpValue("STOVL_OP", OPT_COMP) _
        Or (Not (OpValue("STSUS_OP", OPT_OFF) And OpValue("STOVL_OP", OPT_OFF))) Then
    WriteOneParamToSpr sprIndex, sRow, "STWINDHT", 0, 0, "0 <= x", "", "", True
  End If
  If OpValue("STSUS_OP", OPT_COMP) Then
    WriteOneParamToSpr sprIndex, sRow, "STAVWINDV", 0, 0, "0 <= x", "", "", True ' suspension only
  End If
  
  spr.MaxRows = sRow - 1
  
    IsInputComplete sprIndex, pId, msg ' check the data

  spr.ReDraw = True
End Sub
Sub EvaluatePathwayCompute()
Dim i As Integer
Dim rc As Integer
Dim ndx As Integer
Dim intval As String
Dim category As String
Dim ip As Integer
Dim strval As String
Dim doop As Integer
Dim compute As Integer
Dim paramId As String
Dim params() As String

Dim inf As Integer
Dim ovl As Integer
Dim sus As Integer
Dim vol As Integer
Dim sRc As Integer
Dim ovlknown As Integer
Dim ovlFlow As Integer
  On Error GoTo EvaluatePathwayCompute_Error

  ' look at _OP parameters
  For i = 1 To 5
    Select Case i
      Case 1: paramId = "STINF_OP"
      Case 2: paramId = "STOVL_OP"
      Case 3: paramId = "STSUS_OP"
      Case 4: paramId = "STVOL_OP"
      Case 5: paramId = "STSRC_OP"
    End Select
'   doop = OpValue(paramId, "compute")
    doop = OpValue(paramId, OPT_COMP)
    Select Case paramId
      Case "STINF_OP": inf = doop
      Case "STOVL_OP": ovl = doop
        ovlknown = OpValue(paramId, OPT_FLX) Or OpValue(paramId, OPT_REL) ' OpValue(paramId, "known")
        ovlFlow = OpValue(paramId, OPT_REL) ' OpValue(paramId, "discharge")
      Case "STSUS_OP":
        sus = doop
        sRc = GetParamElem(ndx, "STATYPE", 0, 0, E_USERVAL, intval)
        sus = sus Or intval = "point"
      Case "STVOL_OP": vol = doop
      Case "STSRC_OP": sRc = doop
    End Select
  Next i
  
  ErrorTag = "ConstructReleaseSiteSS"
  ConstructReleaseSiteSS TAB_SITE
  
  ErrorTag = "ConstructWeatherStationSS"
  SetTabEnabled TAB_HYD, inf Or ovl
  If (inf Or ovl) Then ConstructWeatherStationSS TAB_HYD
  
  ErrorTag = "ConstructMonthlyClimateSS"
  SetTabEnabled TAB_CLIM, inf Or ovl
  If (inf Or ovl) Then ConstructMonthlyClimateSS TAB_CLIM
  
  ErrorTag = "ConstructOverlandSS"
  SetTabEnabled TAB_OVL, ovl
  If (ovl Or ovlknown) Then ConstructOverlandSS TAB_OVL
  
  ErrorTag = "ConstructSuspensionSS"
  SetTabEnabled TAB_SUS, sus
  If sus Then ConstructSuspensionSS TAB_SUS
  
  ErrorTag = "WriteContamParamsToSpr"
  SetTabEnabled TAB_CONT, True
  WriteContamParamsToSpr TAB_CONT
  
  ErrorTag = "WriteContamSeriesToSpr"
  SetTabEnabled TAB_KD, True
  WriteContamSeriesToSpr TAB_KD, "KD", params()
  
  Exit Sub
EvaluatePathwayCompute_Error:
  MsgBox "EvaluatePathwayCompute::" & ErrorTag & " " & Error()
  Resume Next
  
End Sub

Sub EvaluatePathwayFlux()
Dim i As Integer
Dim rc As Integer
Dim intval As Integer
Dim category As String
Dim strval As String
Dim flux As Boolean
Dim paramId As String
Dim ndx As Integer
Dim ip As Integer
Dim params() As String
Dim np As Integer

  On Error GoTo EvaluatePathwayFlux_Error
  ErrorTag = ""

  ReDim params(5)
  For i = 1 To 5
    Select Case i
      Case 1: paramId = "STINF_OP"
      Case 2: paramId = "STOVL_OP"
      Case 3: paramId = "STSUS_OP"
      Case 4: paramId = "STVOL_OP"
      Case 5: paramId = "STSRC_OP"
    End Select
    rc = GetParamElem(ndx, paramId, 0, 0, E_USERVAL, intval)
    category = GetParamInfoElem(ip, paramId, E_CATEGORY)
    strval = ListStructText(category, CStr(intval))
'   If OpValue(paramId, "flux") Then
    If OpValue(paramId, OPT_FLX) Then
      np = np + 1
      params(np) = paramId
    End If
  Next i
  
  ReDim Preserve params(np)
  
' For i = 1 To np
'   If "STOVL_OP" = params(i) Then
'     ErrorTag = "ConstructOverlandSS"
'     ConstructOverlandSS TAB_OVL
'     Exit For
'   End If
' Next
  
  ErrorTag = "WriteContamSeriesToSpr"
  SetTabEnabled TAB_FLUX, (np > 0)
  If (np > 0) Then WriteContamSeriesToSpr TAB_FLUX, "FLUX", params()
  
  
  
  Exit Sub
EvaluatePathwayFlux_Error:
  MsgBox "EvaluatePathwayFlux::" & ErrorTag & " " & Error()
  Resume Next
  
End Sub
Sub EvaluatePathwayRelease()
Dim i As Integer
Dim rc As Integer
Dim intval As Integer
Dim category As String
Dim strval As String
Dim release As Boolean
Dim paramId As String
Dim ndx As Integer
Dim ip As Integer
Dim params() As String
Dim np As Integer
Dim optVal As Integer
  On Error GoTo EvaluatePathwayRelease_Error
  ErrorTag = ""

  ReDim params(5)

  For i = 1 To 3
    Select Case i
      Case 1: paramId = "STINF_OP"
      Case 2: paramId = "STOVL_OP"
      Case 3: paramId = "STSUS_OP"
    End Select
    rc = GetParamElem(ndx, paramId, 0, 0, E_USERVAL, intval)
    optVal = Val(intval)
    If Not (ponded And paramId = "STOVL_OP") Then
      If optVal = OPT_REL Or optVal = OPT_FLX Then
        np = np + 1
        params(np) = paramId
      End If
    End If
  Next i
  
  ReDim Preserve params(np)
  
  For i = 1 To np
    If "STOVL_OP" = params(i) Then
      ErrorTag = "ConstructOverlandSS"
      ConstructOverlandSS TAB_OVL
      Exit For
    End If
  Next

  
  ErrorTag = "WriteSeriesToSpr"
  SetTabEnabled TAB_REL, np > 0
  If (np > 0) Then WriteSeriesToSpr TAB_REL, params()
  
  Exit Sub
EvaluatePathwayRelease_Error:
  MsgBox "EvaluatePathwayRelease::" & ErrorTag & " " & Error()
  Resume Next

End Sub

Sub CalcTabInputDependencies(paramId$)
Dim i As Integer
Dim j As Integer
Dim rSum As Single
Dim sPar As Integer
Dim rval As Single
Dim sRc As Integer
Dim ndx As Integer
Dim Row As Long
Dim pszValue As String
Dim sAttr As Integer
Dim col As Integer
Dim count As Integer
Dim rMin As Single
Dim rMax As Single
Dim rMid As Single
Dim wwDepth As Single
Dim wwWidth As Single
Dim wwDisch As Single
Dim lRow As Integer
Dim spr As fpSpread
Dim sprIndex As Integer
Dim dist As Double, ldisp  As Double, tdisp As Double, vdisp As Double
Dim dodirt As Boolean, dopave As Boolean, enable As Integer
Dim intlval As String, params() As String
Dim intlUnit As String
Dim userunit As String
Dim range As String
Dim userVal As String
Dim vol As Double
Dim var As String
Dim flow As Single
Dim valid As Boolean
Dim tmpStr As String
Dim max As String
Dim ip As Integer
Dim msg As String

  On Error GoTo CalcTabInputDependencies_Error
  ErrorTag = ""
  
  Select Case paramId
    Case "STINF_OP", "STOVL_OP", "STSUS_OP", "STVOL_OP", "STSRC_OP"
      
      enable = OpValue("STVOL_OP", OPT_FLX) Or OpValue("STSUS_OP", OPT_FLX)
      
      SetParamElem ndx, "STATYPE", 0, 0, E_ACTIVE, enable ' blh 11/97 False ' BLH 8/97
      If Not enable Then SetParamElem ndx, "STATYPE", 0, 0, E_USERVAL, "area"
      
      If paramId = "STVOL_OP" Then SetContaminantVolRatio
      
      EvaluatePathwayCompute
      EvaluatePathwayFlux
      EvaluatePathwayRelease
      
      If paramId = "STOVL_OP" Then
        If GetParamElem(ndx, "STMEDIA", 0, 0, E_USERVAL, intlval) Then
          If InStr(intlval, "surface") Then
            ConstructReleaseSiteSS 1
          End If
        End If
      End If

      IsInputComplete -1, paramId, msg
      
    Case "STMEDIA"
      SetMediaOptions
      CalcTabInputDependencies "STOVL_OP"
      ConstructReleaseSiteSS 1
    
    Case "STATYPE"
      enable = OpValue("STSUS_OP", OPT_COMP)
      sRc = GetParamElem(ndx, "STATYPE", 0, 0, E_USERVAL, intlval)
      enable = enable Or intlval = "point"
      SetTabEnabled TAB_SUS, enable
      If intlval = "point" Then
        frmMain.SSTab1.TabCaption(TAB_SUS) = "Stack"
      Else
        frmMain.SSTab1.TabCaption(TAB_SUS) = "Suspension"
      End If
      If enable Then ConstructSuspensionSS TAB_SUS

    Case "STROADS"
      sRc = GetParamElem(ndx, "STROADS", 0, 0, E_USERVAL, intlval)
      dodirt = False: dopave = False
      Select Case intlval
        Case "none"
        Case "dirt": dodirt = True
        Case "paved": dopave = True
        Case "both": dodirt = True: dopave = True
      End Select
      ActivateSubtypes TAB_SUS, "ROADS_PAVE", 2, 0, CInt(dopave)
      ActivateSubtypes TAB_SUS, "ROADS_DIRT", 1, 0, CInt(dodirt)
      ActivateSubtypes TAB_SUS, "ROADS_ANY", 0, 0, (dodirt Or dopave)
      
      IsInputComplete TAB_SUS, intlval, msg
      
    Case "STMAXTIME"
    
      WriteContamSeriesToSpr TAB_KD, "KD", params()
      EvaluatePathwayFlux
      EvaluatePathwayRelease
      
    Case "STTOTPOR"
  
      sRc = GetParamElem(ndx, "STTOTPOR", 1, 0, E_USERVAL, userVal)
      sRc = GetParamElem(ndx, "STTOTPOR", 1, 0, E_USERUNIT, userunit)
      intlUnit = GetParamInfoElem(ndx, "STEFFPOR", E_INTLUNIT)
      max = ConvertUOM(userVal, "", userunit, intlUnit)
      SetParamInfoElem ip, "STEFFPOR", E_RANGE, "0 <= x <= " & max  ' intl unit
      
      sRc = GetParamElem(ndx, "STEFFPOR", 0, 0, E_USERUNIT, userunit)
      range = "0 <= x <= " & ConvertUOM(max, "", intlUnit, userunit)
      SetParamElem ndx, "STEFFPOR", 0, 0, E_RANGE, range            ' user unit
      
'     GetParamElem ndx, "STEFFPOR", 0, 0, E_SPR, sprIndex
'     GetParamElem ndx, "STEFFPOR", 0, 0, E_SSROW, row
'     vaSpreadClick sprIndex, SS_VAL, row

      sRc = GetParamElem(ndx, "STEFFPOR", 0, 0, E_USERVAL, userVal)
      If IsNumeric(userVal) Then
        valid = CheckRange("STEFFPOR", userVal, range)
        sRc = GetParamElem(ndx, "STEFFPOR", 0, 0, E_SSROW, Row)
        sRc = GetParamElem(ndx, "STEFFPOR", 0, 0, E_SSCOL, col)
        SetInputValid ssTabCtl(1), col, Row, ndx, valid
      End If

      SetParamInfoElem ip, "STMOISTC", E_RANGE, "0 <= x <= " & max
      sRc = GetParamElem(ndx, "STMOISTC", 0, 0, E_USERUNIT, userunit)
      range = "0 <= x <= " & ConvertUOM(max, "", intlUnit, userunit)
      SetParamElem ndx, "STMOISTC", 0, 0, E_RANGE, range
      
'     GetParamElem ndx, "STMOISTC", 0, 0, E_SPR, sprIndex
'     GetParamElem ndx, "STMOISTC", 0, 0, E_SSROW, row
'     vaSpreadClick sprIndex, SS_VAL, row
      
      
      SetParamElem ndx, "STMOISTC", 0, 0, E_RANGE, range
      sRc = GetParamElem(ndx, "STMOISTC", 0, 0, E_USERVAL, userVal)
      If IsNumeric(userVal) Then
        valid = CheckRange("STMOISTC", userVal, range)
        sRc = GetParamElem(ndx, "STMOISTC", 0, 0, E_SSROW, Row)
        sRc = GetParamElem(ndx, "STMOISTC", 0, 0, E_SSCOL, col)
        SetInputValid ssTabCtl(1), col, Row, ndx, valid
      End If
      ComputeAirSpace
      
    Case "STMOISTC"
      ComputeAirSpace
      
    Case "STEFFPOR"
'     sRc = GetParamElem(ndx, "STEFFPOR", 0, 0, E_USERVAL, userVal)
'     range = userVal & " <= x <= 1"
'     SetParamElem ndx, "STTOTPOR", 1, 0, E_RANGE, range
'     sRc = GetParamElem(ndx, "STTOTPOR", 1, 0, E_USERVAL, userVal)
'     If IsNumeric(userVal) Then
'       If Not CheckRange("STTOTPOR", userVal, range) Then
'         sRc = GetParamElem(ndx, "STTOTPOR", 1, 0, E_SSROW, row)
'         sRc = GetParamElem(ndx, "STTOTPOR", 1, 0, E_SSCOL, col)
'         SetInputValid ssTabCtl(1), col, row, ndx, False
'       End If
'     End If
    Case "STTHICK", "STWIDTH", "STLENGTH"
      setDryVolume
      If ponded Then
        If GetParamElem(ndx, "STOVLFLO", 1, 0, E_USERVAL, userVal) Then
          If GetVolume(vol, "m") Then
            range = "0 <= x <= " & vol
            SetParamElem ndx, "STOVLFLO", 1, 0, E_RANGE, range
            valid = CheckRange("STOVLFLO", userVal, range)
            sRc = GetParamElem(ndx, "STOVLFLO", 1, 0, E_SSROW, Row)
            sRc = GetParamElem(ndx, "STOVLFLO", 1, 0, E_SSCOL, col)
            SetInputValid ssTabCtl(TAB_OVL), col, Row, ndx, valid
          End If
        End If
      End If
    Case "STZBULKD"
      setDryVolume
    Case "STOVLFLO"
      If ponded Then
        If GetParamElem(ndx, "STOVLFLO", 1, 0, E_USERVAL, userVal) Then
          GetParamElem ndx, "STOVLFLO", 1, 0, E_USERUNIT, userunit
          flow = ConvertUOM(userVal, "", userunit, "m^3/yr")
          If GetVolume(vol, "m") Then
            If vol > 0 Then SetParamElem ndx, "STOVLFLX", 0, 0, E_USERVAL, (flow / vol)
            GetParamElem ndx, "STOVLFLX", 0, 0, E_SSROW, Row
            ssTabCtl(TAB_OVL).SetText SS_VAL, Row, convert("", "", flow / vol)
          End If
        End If
      End If
    Case "STINVEN"
  End Select
  
  Exit Sub
CalcTabInputDependencies_Error:
  MsgBox "CalcTabInputDependencies::" & paramId & ": " & Error()
  Resume Next
End Sub

Function OpValue(paramId$, optid%) As Boolean
Dim rc As Integer, ndx As Integer, ip As Integer
Dim intval As String, category As String, opval As String

  rc = GetParamElem(ndx, paramId, 0, 0, E_USERVAL, intval)
  category = GetParamInfoElem(ip, paramId, E_CATEGORY)
  opval = ListStructText(category, intval)
' OpValue = 0 <> InStr(opval, substr)
  OpValue = (optid = Val(intval))
End Function

Sub SetMediaOptions()
Dim sRc As Integer, ndx As Integer, i As Integer, CT As Integer
Dim Row As Integer
Dim enable As Boolean
Dim media As String, pId As String
Dim category As String, userVal As String

      sRc = GetParamElem(ndx, "STMEDIA", 0, 0, E_USERVAL, media)
      
      SetPathwayModeOptions media
      
      For i = 1 To 5
        Select Case i
          Case 1: pId = "STINF_OP"
          Case 2: pId = "STOVL_OP"
          Case 3: pId = "STSUS_OP"
          Case 4: pId = "STVOL_OP"
          Case 5: pId = "STSRC_OP"
        End Select
        category = GetParamInfoElem(ndx, pId, E_CATEGORY)
        CT = GetListCategoryCount(category)
        enable = 1 < CT
        SetParamElem ndx, pId, 0, 0, E_ACTIVE, enable
        
        userVal = "0"
        If enable Then
          If GetParamElem(ndx, pId, 0, 0, E_USERVAL, userVal) Then
            If "" = ListStructText(category, userVal) Then userVal = "0"
          End If
        End If
        SetParamElem ndx, pId, 0, 0, E_USERVAL, userVal
        RewriteParamToSpr pId, 0, 0
      Next

      If InStr(media, "aquifer") Then
        SetParamInfoElem ndx, "STDARCY", E_SCREEN, "darcy velocity"
        SetParamInfoElem ndx, "STDARCY_NUM", E_SCREEN, "number of darcy velocity measurements"
        SetParamInfoElem ndx, "STDARCY_TIM", E_SCREEN, "time of darcy velocity measurements"
      Else
        SetParamInfoElem ndx, "STDARCY", E_SCREEN, "darcy infiltration rate"
        SetParamInfoElem ndx, "STDARCY_NUM", E_SCREEN, "number of darcy infiltration rates"
        SetParamInfoElem ndx, "STDARCY_TIM", E_SCREEN, "time of darcy infiltration rate"
      End If
      
'     BLH 8/18/97 enable overland options for pond
      If InStr(media, "surface") Then
        SetParamElem ndx, "STPOND", 0, 0, E_USERVAL, BOOL_YES
        ConstructReleaseSiteSS 1
      Else
        SetParamElem ndx, "STPOND", 0, 0, E_USERVAL, BOOL_NO
      End If
End Sub

Sub ConstructOptionsSS(sprIndex%)
Dim spr As fpSpread
Dim sRow As Integer
Dim pId As String
Dim msg As String
Dim ndx As Integer


  Set spr = ssTabCtl(sprIndex)
  spr.ReDraw = False
  ssParamsInit spr
  
  spr.ColWidth(SS_DESC) = 37 ' 40 ' 38
  spr.ColWidth(SS_VAL) = 22 ' 16
  spr.ColWidth(SS_UOM) = 8
  spr.ColWidth(SS_REF) = 6 ' 8

  sRow = 1
  
  WriteOneParamToSpr sprIndex, sRow, "STMEDIA", 0, 0, "", "", "", True

  sRow = sRow + 1
  WriteOneParamToSpr sprIndex, sRow, "STINF_OP", 0, 0, "", "", "", True
  WriteOneParamToSpr sprIndex, sRow, "STOVL_OP", 0, 0, "", "", "", True
  WriteOneParamToSpr sprIndex, sRow, "STSUS_OP", 0, 0, "", "", "", True
  WriteOneParamToSpr sprIndex, sRow, "STVOL_OP", 0, 0, "", "", "", True
  WriteOneParamToSpr sprIndex, sRow, "STSRC_OP", 0, 0, "", "", "", True
  
  SetParamElem ndx, "STATYPE", 0, 0, E_USERVAL, "area" ' default
  SetParamElemSprAttr ndx, "STATYPE", 0, 0, 0, sprIndex, 0, 0, True

  
  sRow = sRow + 1
  
' There is a potential problem with the time step in the CSTRM.
' Any time step other than 1 yr gives bogus results.
  SetParamElem ndx, "STDELTA_T", 0, 0, E_USERVAL, 1
  WriteOneParamToSpr sprIndex, sRow, "STDELTA_T", 0, 0, "", "", "", False

  WriteOneParamToSpr sprIndex, sRow, "STMAXTIME", 0, 0, "", "", "", True
  WriteOneParamToSpr sprIndex, sRow, "STMINWST", 0, 0, "", "", "", True
  
  spr.MaxRows = sRow - 1
  
  IsInputComplete sprIndex, pId, msg ' check the data

  spr.ReDraw = True
  

End Sub

Function GetVolume(vol As Double, intlUnit$) As Boolean
Dim ndx As Integer
Dim var As String
Dim userunit As String

  GetVolume = False
  If GetParamElem(ndx, "STTHICK", 0, 0, E_USERVAL, var) Then
    If IsNumeric(var) Then
      GetParamElem ndx, "STTHICK", 0, 0, E_USERUNIT, userunit
      vol = Val(ConvertUOM(var, "", userunit, intlUnit))
      If GetParamElem(ndx, "STWIDTH", 0, 0, E_USERVAL, var) Then
        If IsNumeric(var) Then
          GetParamElem ndx, "STWIDTH", 0, 0, E_USERUNIT, userunit
          vol = vol * Val(ConvertUOM(var, "", userunit, intlUnit))
          If GetParamElem(ndx, "STLENGTH", 0, 0, E_USERVAL, var) Then
            If IsNumeric(var) Then
              GetParamElem ndx, "STLENGTH", 0, 0, E_USERUNIT, userunit
              vol = vol * Val(ConvertUOM(var, "", userunit, intlUnit))
              GetVolume = True
            End If
          End If
        End If
      End If
    End If
  End If
End Function

Sub ComputeAirSpace()
Dim sprIndex As Integer
Dim sRc As Integer
Dim intlval As String
Dim userVal As String
Dim userunit As String
Dim intlUnit As String
Dim airspc As Single
Dim moistc As Single
Dim totpor As Single
Dim Row As Long
Dim col As Long
Dim ndx As Integer
Dim valid As Boolean
Dim invalid As Boolean

  If Not GetParamElem(ndx, "STMOISTC", 0, 0, E_INVALID, invalid) Then Exit Sub
  If invalid Then Exit Sub
  If Not GetParamElem(ndx, "STTOTPOR", 1, 0, E_INVALID, invalid) Then Exit Sub
  If invalid Then Exit Sub

  If Not GetParamElem(ndx, "STAIRSPC", 1, 0, E_SSROW, Row) Then Exit Sub
  sRc = GetParamElem(ndx, "STAIRSPC", 1, 0, E_INTLUNIT, intlUnit)
  
  If Not GetParamElem(ndx, "STMOISTC", 0, 0, E_USERVAL, userVal) Then Exit Sub
  sRc = GetParamElem(ndx, "STMOISTC", 0, 0, E_USERUNIT, userunit)
  moistc = ConvertUOM(userVal, "", userunit, intlUnit)
  
  If Not GetParamElem(ndx, "STTOTPOR", 1, 0, E_USERVAL, userVal) Then Exit Sub
  sRc = GetParamElem(ndx, "STTOTPOR", 1, 0, E_USERUNIT, userunit)
  totpor = ConvertUOM(userVal, "", userunit, intlUnit)
  
  airspc = totpor - moistc
  
  sRc = GetParamElem(ndx, "STAIRSPC", 1, 0, E_USERUNIT, userunit)
  userVal = ConvertUOM(CStr(airspc), "", intlUnit, userunit)
  sRc = GetParamElem(ndx, "STAIRSPC", 1, 0, E_SPR, sprIndex)
  If (sprIndex <> -1) Then ssTabCtl(sprIndex).SetText SS_VAL, Row, CStr(userVal)
  SetParamElem ndx, "STAIRSPC", 1, 0, E_USERVAL, userVal
End Sub

Function Kd_DatabaseValue(name$, Optional progenyName$ = "") As Double
  Dim i As Long
  Dim j As Long
  
  ' Fail safe return value
  Kd_DatabaseValue = -1
  
  If Len(progenyName) > 0 Then
    If InStr(progenyName, "*") = 1 Then
      j = Progeny_Index(name, Mid(progenyName, 3), i)
    Else
      j = Progeny_Index(name, progenyName, i)
    End If
    If j >= 0 Then
      Kd_DatabaseValue = Val(vptCont(i).prog(j).kd)
    End If
    Exit Function
  End If
  
  i = Kd_Index(name)
  If i > 0 Then
    Kd_DatabaseValue = Val(vptCont(i).kd)
  End If
  
End Function

Function Kd_SoilEquationCalc(koc#, organic#, clay#, silt#, sand#) As Double
  Kd_SoilEquationCalc = 0.0001 * koc * (57.735 * organic + 2# * clay + 0.4 * silt + 0.005 * sand)
End Function

Function Kd_EquationValue(name$, organic$, clay$, silt$, sand$, Optional progenyName$ = "") As Double
  Dim i As Long
  Dim j As Long
  
  ' Fail safe return value
  Kd_EquationValue = -1
  
  If Kdn_Index(False) > 0 Then
    If Len(progenyName) > 0 Then
      If InStr(progenyName, "*") = 1 Then
        j = Progeny_Index(name, Mid(progenyName, 3), i)
      Else
        j = Progeny_Index(name, progenyName, i)
      End If
      If j >= 0 Then
        Kd_EquationValue = Kd_SoilEquationCalc(Val(vptCont(i).prog(j).koc), Val(organic), Val(clay), Val(silt), Val(sand))
      End If
      Exit Function
    End If
    
    i = Kd_Index(name)
    If i > 0 Then
        Kd_EquationValue = Kd_SoilEquationCalc(Val(vptCont(i).koc), Val(organic), Val(clay), Val(silt), Val(sand))
    End If
    
  End If

End Function

Function Kd_LookupTableValue(name$, Optional progenyName$ = "") As Double
  Dim i As Long
  Dim j As Long
  
  ' Fail safe return value
  Kd_LookupTableValue = -1
  
  If Len(progenyName) > 0 Then
    If InStr(progenyName, "*") = 1 Then
      j = Progeny_Index(name, Mid(progenyName, 3), i)
    Else
      j = Progeny_Index(name, progenyName, i)
    End If
    If j >= 0 Then
      Kd_LookupTableValue = get_kd(vptCont(i).prog(j).cas, Kdn_Index())
    End If
    Exit Function
  End If
  
  i = Kd_Index(name)
  If i > 0 Then
    Kd_LookupTableValue = get_kd(vptCont(i).cas, Kdn_Index())
  End If

End Function

Function Kdn_Index(Optional IncludepH As Boolean = True) As Integer
  Dim i As Integer
  Dim sum As Double
  Dim sand As Double
  Dim silt As Double
  Dim clay As Double
  Dim omc As Double
  Dim ph As Double
  Dim iron As Double
  
  ' Fail safe return value
  Kdn_Index = -1
  
  If Not KD_loaded Then load_kd

  GetParamElem i, "KDSAND", 0, 0, E_USERVAL, sand
  GetParamElem i, "KDSILT", 0, 0, E_USERVAL, silt
  GetParamElem i, "KDCLAY", 0, 0, E_USERVAL, clay
  GetParamElem i, "KDOMC", 0, 0, E_USERVAL, omc
  GetParamElem i, "KDPH", 0, 0, E_USERVAL, ph
  GetParamElem i, "KDIRON", 0, 0, E_USERVAL, iron
  
  sum = sand + silt + clay + omc + iron
  If sum > 100.1 Or sum < 99.9 Then
    Exit Function
  ElseIf IncludepH And (ph < 1 Or ph > 14) Then
    Exit Function
  Else
    sum = clay + omc + iron
    If ph >= 9# Then
      If sum >= 30# Then
        Kdn_Index = 3
      Else
        If sum >= 10# Then
          Kdn_Index = 2
        Else
          Kdn_Index = 1
        End If
      End If
    Else
      If ph > 5# Then
        If sum >= 30# Then
          Kdn_Index = 6
        Else
          If sum >= 10# Then
            Kdn_Index = 5
          Else
            Kdn_Index = 4
          End If
        End If
      Else
        If sum >= 30# Then
          Kdn_Index = 9
        Else
          If sum >= 10# Then
            Kdn_Index = 8
          Else
            Kdn_Index = 7
          End If
        End If
      End If
    End If
  End If

End Function

Function Kd_Index(con_name$) As Long
  Dim i As Long
  
  For i = 1 To UBound(vptCont)
    If vptCont(i).name = con_name Then Exit For
  Next i
  
  If i > UBound(vptCont) Then i = -1
  
  Kd_Index = i
  
End Function

Function Progeny_Index(con_name$, prog_name$, ByRef con_index&) As Long
  Dim i As Long
  Dim j As Long
  
  j = -1
  i = Kd_Index(con_name)
  
  If i >= 0 Then
    For j = 1 To vptCont(i).nds
      If vptCont(i).prog(j).name = prog_name Then Exit For
    Next j
    
    If j > vptCont(i).nds Then j = -1
  End If
  
  If j >= 0 Then con_index = i
  Progeny_Index = j

End Function

Sub LoadSoilParams()
  Dim i As Integer
  Dim ndx As Integer
  Dim paramId As String
  Dim var
  
  vfProcessUserInput = False
  
  If GetParamElem(i, "KDCLASS", 0, 0, E_USERVAL, var) Then
    frmSoilTri.lstSoils.ListIndex = var
    SetParamElem ndx, "KDCLASS", 0, 0, E_SPR, TAB_KD
  Else
    frmSoilTri.lstSoils.ListIndex = -1
  End If
  For i = fSAND To fPH
    frmSoilTri.txt(i) = ""
    Select Case i
      Case fSAND: paramId = "KDSAND"
      Case fSILT: paramId = "KDSILT"
      Case fCLAY: paramId = "KDCLAY"
      Case fOMC: paramId = "KDOMC"
      Case fIRON: paramId = "KDIRON"
      Case fPH: paramId = "KDPH"
    End Select
    If GetParamElem(ndx, paramId, 0, 0, E_USERVAL, var) Then
      frmSoilTri.txt(i) = var
      SetParamElem ndx, paramId, 0, 0, E_SPR, TAB_KD
    End If
  Next
  
  vfProcessUserInput = True
End Sub

Sub SetContaminantVolRatio()
Dim intlval As String
Dim ncont As Integer, c1 As Integer, c2 As Integer, idx As Integer

  If OpValue("STVOL_OP", OPT_OFF) Then
    intlval = "0.0"
  Else
    intlval = "1.0"
  End If
  For ncont = 1 To UBound(vptCont)
    c1 = vptAllCont(ncont).ctr(1, 1)
    c2 = vptAllCont(ncont).ctr(1, 2)
    SetParamElem idx, "STVOLRAT", c1, c2, E_USERVAL, intlval
  Next

End Sub

Sub SetRange(ByVal paramId$)
Dim sRc As Integer, ndx As Integer, ip As Integer
Dim intlval As String, intlUnit As String
Dim userVal As String, userunit As String

  Select Case paramId
    Case "STEFFPOR"
      If GetParamElem(ndx, "STTOTPOR", 1, 0, E_USERVAL, userVal) Then
        GetParamElem ndx, "STTOTPOR", 1, 0, E_USERUNIT, userunit
        intlUnit = GetParamInfoElem(ip, "STEFFPOR", E_INTLUNIT)
        intlval = ConvertUOM(userVal, "", userunit, intlUnit)
        SetParamInfoElem ndx, "STEFFPOR", E_RANGE, "0 <= x <= " & intlval
        Exit Sub
      End If
  End Select
End Sub

Public Sub SetSoilParams(soilclass%, sand$, silt$, clay$, omc$, iron$, ph$)
  Dim i As Integer
  Dim kdn As Long
  Dim kdlist As String
  Dim var
  Dim c1 As Integer, c2 As Integer
  Dim r As Long
  Dim temp As Double
  Dim spr As Control
  
  SetParamElem i, "KDCLASS", 0, 0, E_USERVAL, soilclass
  SetParamElem i, "KDSAND", 0, 0, E_USERVAL, sand
  SetParamElem i, "KDSILT", 0, 0, E_USERVAL, silt
  SetParamElem i, "KDCLAY", 0, 0, E_USERVAL, clay
  SetParamElem i, "KDOMC", 0, 0, E_USERVAL, omc
  SetParamElem i, "KDIRON", 0, 0, E_USERVAL, iron
  SetParamElem i, "KDPH", 0, 0, E_USERVAL, ph
  
  kdn = Kdn_Index()
  
  Set spr = ssTabCtl(TAB_KD)
  spr.ReDraw = False
  
  For r = 1 To spr.MaxRows
    spr.Row = r
    spr.col = SS_KD_EST
    If spr.CellType = SS_CELL_TYPE_COMBOBOX Then
      If spr.GetText(SS_C1, r, var) Then c1 = Val(var)
      If spr.GetText(SS_C2, r, var) Then c2 = Val(var)
      kdlist = " Select Adsorption Coeff."
      If c2 > 0 Then
        temp = Kd_DatabaseValue(vptCont(c1).name, vptCont(c1).prog(c2).name)
      Else
        temp = Kd_DatabaseValue(vptCont(c1).name)
      End If
      If temp < 0 Then
          kdlist = kdlist & Chr(9) & "N/A - " & getEstKdDesc(DATABASE_LOOKUP)
      Else
          kdlist = kdlist & Chr(9) & temp & " - " & getEstKdDesc(DATABASE_LOOKUP)
      End If
      
      If c2 > 0 Then
        temp = Kd_EquationValue(vptCont(c1).name, CStr(omc), CStr(clay), CStr(silt), CStr(sand), vptCont(c1).prog(c2).name)
      Else
        temp = Kd_EquationValue(vptCont(c1).name, CStr(omc), CStr(clay), CStr(silt), CStr(sand))
      End If
      If temp < 0 Then
          kdlist = kdlist & Chr(9) & "N/A - " & getEstKdDesc(EQUATION_LOOKUP)
      Else
          kdlist = kdlist & Chr(9) & temp & " - " & getEstKdDesc(EQUATION_LOOKUP)
      End If
      
      If c2 > 0 Then
        temp = Kd_LookupTableValue(vptCont(c1).name, vptCont(c1).prog(c2).name)
      Else
        temp = Kd_LookupTableValue(vptCont(c1).name)
      End If
      If temp < 0 Then
          kdlist = kdlist & Chr(9) & "N/A - " & getEstKdDesc(TABLE_LOOKUP)
      Else
          kdlist = kdlist & Chr(9) & temp & " - " & getEstKdDesc(TABLE_LOOKUP)
      End If
      
      'spr.CellType = SS_CELL_TYPE_COMBOBOX
      spr.TypeComboBoxList = kdlist
      spr.TypeComboBoxCurSel = 0
      spr.Lock = False
    End If
  Next
  
  spr.ReDraw = True
  Set spr = Nothing
  
  'Exposure.KdListUpdate
  'Exposure.ProgenyListUpdate

End Sub

