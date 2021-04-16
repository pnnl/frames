VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Object = "{F856EC8B-F03C-4515-BDC6-64CBD617566A}#7.0#0"; "FPSPR70.ocx"
Begin VB.Form Main 
   Caption         =   "Form1"
   ClientHeight    =   8400
   ClientLeft      =   60
   ClientTop       =   456
   ClientWidth     =   8172
   LinkTopic       =   "Form1"
   ScaleHeight     =   8400
   ScaleWidth      =   8172
   StartUpPosition =   2  'CenterScreen
   Begin VB.TextBox txtFileName 
      Height          =   285
      Left            =   2055
      TabIndex        =   7
      Top             =   6735
      Width           =   5070
   End
   Begin VB.CommandButton cmdGetURL 
      Height          =   330
      Left            =   7320
      Picture         =   "Messenger2.frx":0000
      Style           =   1  'Graphical
      TabIndex        =   6
      ToolTipText     =   "Select a document"
      Top             =   6720
      Width           =   435
   End
   Begin VB.ComboBox Combo1 
      Height          =   288
      Left            =   2160
      Style           =   2  'Dropdown List
      TabIndex        =   4
      Top             =   360
      Width           =   2772
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   120
      Top             =   6720
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.TextBox txtReachBack 
      Height          =   315
      Left            =   2100
      TabIndex        =   3
      Text            =   "Text1"
      Top             =   6135
      Width           =   5112
   End
   Begin VB.CommandButton cmdPost 
      Caption         =   "Post Events"
      Height          =   480
      Left            =   5595
      TabIndex        =   1
      Top             =   7425
      Width           =   1530
   End
   Begin FPSpreadADO.fpSpread fpSpread1 
      Height          =   4992
      Left            =   696
      TabIndex        =   0
      Top             =   840
      Width           =   7092
      _Version        =   458752
      _ExtentX        =   12509
      _ExtentY        =   8805
      _StockProps     =   64
      BackColorStyle  =   1
      ColHeaderDisplay=   0
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   7.8
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      GrayAreaBackColor=   14737632
      GridSolid       =   0   'False
      MaxCols         =   1
      MaxRows         =   20
      RowHeaderDisplay=   0
      SpreadDesigner  =   "Messenger2.frx":014A
      UnitType        =   2
   End
   Begin VB.Label lblconfig 
      AutoSize        =   -1  'True
      Caption         =   "Graphical View:"
      Height          =   195
      Left            =   675
      TabIndex        =   8
      Top             =   6765
      Width           =   1110
   End
   Begin VB.Label Label1 
      AutoSize        =   -1  'True
      Caption         =   "Time Index"
      Height          =   192
      Left            =   720
      TabIndex        =   5
      Top             =   360
      Width           =   1272
   End
   Begin VB.Label Reach 
      AutoSize        =   -1  'True
      Caption         =   "Reachback:"
      Height          =   195
      Left            =   675
      TabIndex        =   2
      Top             =   6210
      Width           =   885
   End
End
Attribute VB_Name = "Main"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Dim PID As Long
Dim stMod As String
Dim hLibModule As Long

Dim argc As Long             'command line argument count
Dim argv() ''As String         'command line argument array

Dim hRadList As Long
Dim hChemList As Long
Dim hEMCriteria As Long
Dim hFileType As Long

Dim reachback As String
Dim dLat As Double
Dim dLong As Double

Dim hasFlux As Boolean
Dim hasConc As Boolean
Dim hasExtent As Boolean
Dim hasEmission As Boolean

Dim loading As Boolean
Dim ArgArray()
Dim colDatum As New Collection

Private Sub Combo1_Click()
  If Not loading Then showDatum
End Sub

Private Sub txtFileName_Change()
  cmdPost.Enabled = "" <> Dir$(txtFileName.Text)
End Sub

Function GetCommandLine(Optional MaxArgs)
   'Declare variables.
   Dim C, CmdLine, CmdLnLen, InArg, i, NumArgs, InQuote
   'See if MaxArgs was provided.
   If IsMissing(MaxArgs) Then MaxArgs = 10
   'Make array of the correct size.
   ReDim ArgArray(MaxArgs)
   NumArgs = 0: InArg = False
   'Get command line arguments.
   CmdLine = Command$
   CmdLnLen = Len(CmdLine)
   'Go thru command line one character
   'at a time.
   InQuote = False
   InArg = False
   
   For i = 1 To CmdLnLen
      C = Mid(CmdLine, i, 1)
      'Test for space or tab.
      If (C <> " " And C <> vbTab Or InQuote) Then
         'Neither space nor tab.
         'Test if already in argument.
         If (C = """") Then
            InQuote = Not InQuote
            C = ""
         End If
         If Not InArg Then
         'New argument begins.
         'Test for too many arguments.
            If NumArgs = MaxArgs Then Exit For
            NumArgs = NumArgs + 1
            InArg = True
         End If
         'Concatenate character to current argument.
         ArgArray(NumArgs) = ArgArray(NumArgs) & C
      Else
         'Found a space or tab.
         'Set InArg flag to False.
         InArg = False
      End If
   Next i
   'Resize array just enough to hold arguments.
   ReDim Preserve ArgArray(NumArgs)
   'Return Array in Function name.
   GetCommandLine = ArgArray()
End Function

Private Sub cmdGetURL_Click()
  On Error Resume Next
  With CommonDialog1
    .CancelError = True
    .Flags = cdlOFNHideReadOnly
    .ShowOpen
    If Err.Number = 0 Then
      txtFileName = .filename
    End If
  End With
  Err.Clear
End Sub

Private Sub cmdPost_Click()
Dim obj, var
Dim col As Long
Dim row As Long
Dim msg As String
Dim post As Boolean
Dim status As Boolean
Dim stPoint As String
Dim stUserName As String
Dim stClassName As String
Dim stSubclassName As String
Dim stNotes As String
Dim filename As String
Dim stAttrName1 As String, stAttrValue1 As String
Dim stAttrName2 As String, stAttrValue2 As String
Dim stAttrName3 As String, stAttrValue3 As String
Dim stAttrName4 As String, stAttrValue4 As String
Dim stAttrName5 As String, stAttrValue5 As String

Dim objEA As New KP_API.clsEarthAlert

' copy reachback xls file to reachback location
  filename = Dir(argv(argc - 3) & "\V2Temp\~tmp~*.xls")
  While filename <> ""
    FileCopy argv(argc - 3) & "\V2Temp\" & filename, txtReachBack.Text & "\" & filename
    If Err.Number <> 0 Then
      MsgBox "Failed to copy reachback file " & filename
    End If
    filename = Dir()
  Wend
  
  For col = 1 To fpSpread1.DataColCnt
    row = fpSpread1.DataRowCnt
    fpSpread1.row = row
    fpSpread1.col = col
    post = fpSpread1.value
    If post Then
      msg = ""
      For row = 1 To fpSpread1.DataRowCnt - 1
        fpSpread1.GetText col, row, var
        If hasEmission Or hasConc Then
          Select Case row
          Case 1: stUserName = var
          Case 2: stPoint = var
          Case 3: stClassName = var
          Case 4: stSubclassName = var
          Case 5: stNotes = var
          Case 6: stAttrName1 = var
          Case 7: stAttrValue1 = var
          Case 8: stAttrName2 = var
          Case 9: stAttrValue2 = var
          Case 10: stAttrName3 = var
          Case 11: stAttrValue3 = var
          Case 12: stAttrName4 = var
          Case 13: stAttrValue4 = var
          Case 14: stAttrName5 = var
          Case 15: stAttrValue5 = var
          Case 16: dLat = var
          Case 17: dLong = var
          End Select
          msg = msg & var & vbCrLf
        End If
        
        If hasExtent Then
        
        End If
      Next row
      
      If hasEmission Or hasConc Then
        status = objEA.SetConditionalPointData _
            (stUserName, stPoint, stClassName, stSubclassName, stNotes, _
             stAttrName1, stAttrValue1, stAttrName2, stAttrValue2, stAttrName3, stAttrValue3, _
             stAttrName4, stAttrValue4, stAttrName5, stAttrValue5, dLat, dLong, "")
      End If
    End If
  Next col
End Sub

Private Sub Form_Load()
Dim AppPath
Dim lngCount As Long
Dim strFileName As String

  argv = GetCommandLine()
  argc = UBound(argv)
  
  If argc < 4 Then
    MsgBox "Invalid arguments passed to module" & Chr(10) & Command & Chr(10) & "Contact PNNL", 16, "Usage error!"
    End
  End If

  strFileName = String(255, 0)
  lngCount = GetModuleFileName(App.hInstance, strFileName, 255)
  strFileName = Left(strFileName, lngCount)
  If UCase(Right(strFileName, 7)) = "VB6.EXE" Then
    AppPath = "C:\program files\FramesV2"
    hLibModule = LoadLibrary(AppPath & "\systemio.dll")
  End If
  
  PID = 0
  PID = ModuleDevOpen(argv(argc - 2), argv(argc - 1), argv(argc))
  If (PID < 0) Then
    MsgBox "Invalid PID: " & PID
    End
  End If
  
  Call InitSpreadsheet
  Call stuff
End Sub

Private Sub Form_Unload(cancel As Integer)
  If PID <> 0 Then
    F2ClearErrors PID
    ModuleDevClose PID, 0
  End If
  If 0 <> hLibModule Then FreeLibrary hLibModule
End Sub

Private Sub stuff()
  Dim md As String

  stMod = argv(argc)
  hRadList = IconGetInputDataSet(PID, stMod, "RadList", 1)
  hChemList = IconGetInputDataSet(PID, stMod, "ChemList", 1)
  hEMCriteria = IconGetInputDataSet(PID, stMod, "EMCriteria", 1)
  hFileType = IconGetInputDataSet(PID, stMod, "EMConfigFile", 1)
  reachback = DataSetReadString1(PID, hEMCriteria, "Reachback", "", 1)
  txtReachBack = reachback
  
  md = argv(argc - 4)
  If md = "/wsen" Then
    hasConc = True
    Me.Caption = "SurfaceWater Sensor Notification"
    Call WCFSensorNotification
  ElseIf md = "/wsrc" Then
    hasConc = True
    Me.Caption = "SurfaceWater Source Notification"
    If hFileType Then
      ReadEMConfig
    Else
      Call WCFSourceNotification
    End If
  ElseIf md = "/asen" Then
    hasEmission = True
    Me.Caption = "Air Sensor Notification"
    Call AFFSensorNotification
  End If
  
  showDatum
End Sub

Private Sub InitSpreadsheet()
Dim i As Long
Dim cw
Dim tw
Dim ht As Long
Dim wid As Long
Dim cwid As Long
Dim swid As Single
Dim fwidth

  fpSpread1.SetText 0, 0, "Parameter"
  fpSpread1.SetText 1, 0, "Value"
  fpSpread1.SetText 0, 1, "ValidFemisUserName"
  fpSpread1.SetText 0, 2, "PointName"
  fpSpread1.SetText 0, 3, "ClassName"
  fpSpread1.SetText 0, 4, "SubclassName"
  fpSpread1.SetText 0, 5, "Notes"
  fpSpread1.SetText 0, 6, "AttrName1"
  fpSpread1.SetText 0, 7, "AttrValue1"
  fpSpread1.SetText 0, 8, "AttrName2"
  fpSpread1.SetText 0, 9, "AttrValue2"
  fpSpread1.SetText 0, 10, "AttrName3"
  fpSpread1.SetText 0, 11, "AttrValue3"
  fpSpread1.SetText 0, 12, "AttrName4"
  fpSpread1.SetText 0, 13, "AttrValue4"
  fpSpread1.SetText 0, 14, "AttrName5"
  fpSpread1.SetText 0, 15, "AttrValue5"
  fpSpread1.SetText 0, 16, "Lat"
  fpSpread1.SetText 0, 17, "Long"
  fpSpread1.SetText 0, 18, "Post ?"
  fpSpread1.MaxRows = 18
  fpSpread1.MaxCols = 500
  
  fpSpread1.row = -1
  fpSpread1.col = -1
  fpSpread1.CellType = CellTypeEdit
  
  fpSpread1.row = 1
  fpSpread1.CellType = CellTypeStaticText
  
  fpSpread1.row = 18
  fpSpread1.CellType = CellTypeCheckBox
  
  fpSpread1.TypeMaxEditLen = 512
  fpSpread1.SelectBlockOptions = 0
  fpSpread1.col = 0
  
  fwidth = fpSpread1.MaxTextColWidth(0)
  fpSpread1.ColWidth(0) = fwidth * 4

  cw = fpSpread1.MaxTextColWidth(0) + 5
  fpSpread1.ColWidth(0) = cw
  fpSpread1.ColWidthToTwips cw, cwid
  tw = cwid
  
  fpSpread1.GetClientArea wid, ht
  If wid > tw Then
    fpSpread1.TwipsToColWidth wid - (tw - cwid), swid
    fpSpread1.ColWidth(1) = swid
  Else
    tw = 0
    fpSpread1.ColWidthToTwips fpSpread1.ColWidth(1), cwid
    fpSpread1.TwipsToColWidth (wid - cwid) / (fpSpread1.MaxCols - 1), swid
    For i = 2 To fpSpread1.MaxCols
      fpSpread1.ColWidth(i) = swid
      tw = tw + swid
    Next i
    fpSpread1.ColWidthToTwips tw, cwid
    fpSpread1.ColWidthToTwips swid, cwid
    fpSpread1.TwipsToColWidth wid - (cwid * (fpSpread1.MaxCols - 1)), swid
    fpSpread1.ColWidth(1) = CLng(swid)
  End If
End Sub

Private Sub showDatum()
  Dim obj, var
  Dim col As Long
  Dim row As Long
  Dim cnt As Long
  
  fpSpread1.MaxCols = colDatum.Count
  cnt = 1
  Label1.BackColor = vbWhite
  For col = 1 To colDatum.Count
    row = 0
    obj = colDatum(col)
    If obj(14) = Combo1.Text Then
      If obj(3) = "Above Threshold" Then Label1.BackColor = vbRed
      For Each var In obj
        row = row + 1
        fpSpread1.SetText cnt, row, var
      Next var
      row = row + 1
      fpSpread1.row = row
      fpSpread1.col = cnt
      fpSpread1.value = 1
      fpSpread1.TypeCheckText = cnt & " of a possible " & colDatum.Count
      fpSpread1.ColWidth(cnt) = fpSpread1.ColWidth(1)
      cnt = cnt + 1
    End If
  Next col
  fpSpread1.MaxCols = cnt
End Sub

Public Function ReadEMConfig() As Boolean
'This reads a text file based on the config file info and calls the dll
'and adds all the records in the txt comma delimited file.
Dim i As Long
Dim cnt As Long
Dim feature As String
Dim sItems
Dim varevent

  ReadEMConfig = False
  cnt = DataSetDimensionCount(PID, hFileType, "Text", SetIdx())
  If cnt > 0 Then
    Combo1.Clear
    loading = True
    For i = 2 To cnt
      feature = DataSetReadString1(PID, hFileType, "Text", "", i)
      sItems = Split(feature, ",")
      Combo1.AddItem sItems(12)
      If sItems(2) = "Potential_Source" Then
        varevent = Array(sItems(0), _
          sItems(1), _
          sItems(2), sItems(3), _
          sItems(4), _
          sItems(13), sItems(14), _
          sItems(5), sItems(6), _
          sItems(7), sItems(8), _
          sItems(9), sItems(10), _
          sItems(11), sItems(12), _
          sItems(15), sItems(16))
        colDatum.Add varevent
      ElseIf sItems(2) = "Water_Sensor" Then
        varevent = Array(sItems(0), _
          sItems(1), _
          sItems(2), sItems(3), _
          sItems(4), _
          sItems(5), sItems(6), _
          sItems(7), sItems(8), _
          sItems(9), sItems(10), _
          sItems(11), sItems(12), _
          sItems(13), sItems(14), _
          sItems(15), sItems(16))
        colDatum.Add varevent
      End If
    Next
    loading = False
    Combo1.ListIndex = 0
    ReadEMConfig = True
  End If
End Function

Public Sub WCFSensorNotification()
  Dim i As Long, j As Long, k As Long, t As Long
  Dim cnt As Long
  Dim ct As Long
  Dim ctRad As Long
  Dim ctChem As Long
  Dim ctTime As Long
  Dim ctFeature As Long
  Dim prefix As String
  Dim unit As String
  Dim chemical As String
  Dim feature As String
  Dim threshold As Double
  Dim above As Boolean
  Dim varevent
  Dim hList As Long
  Dim hPts As Long
  Dim hConc As Long
  Dim hRadSWConc As Long
  Dim hChemSWConc As Long
  Dim values()
  Dim times()
  Dim rback As String
  Dim filename As String
  
  hPts = IconGetInputDataSet(PID, stMod, "SurfaceWaterPoints", 1)
  ctFeature = DataSetDimensionCount(PID, hPts, "Feature", SetIdx())
  ctChem = DataSetDimensionCount(PID, hChemList, "CASID", SetIdx())
  ctRad = DataSetDimensionCount(PID, hRadList, "CASID", SetIdx())
  
  On Error Resume Next
  hChemSWConc = IconGetInputDataSet(PID, stMod, "ChemSurfaceWaterDissolvedConc", 1)
  If Err.Number > 0 Then hChemSWConc = 0
  hRadSWConc = IconGetInputDataSet(PID, stMod, "RadSurfaceWaterDissolvedConc", 1)
  If Err.Number > 0 Then hRadSWConc = 0
  
' create reachback xls file link to reachback location
  filename = Dir(argv(argc - 3) & "\V2Temp\~tmp~*.xls")
  While filename <> ""
    rback = rback + txtReachBack.Text & "\" & filename + ";"
    filename = Dir()
  Wend
  
  
  On Error GoTo 0
  
  Combo1.Clear
  loading = True
  For i = 1 To ctFeature
    feature = DataSetReadString1(PID, hPts, "Feature", "", i)
    dLat = DataSetReadReal3(PID, hPts, "FeaturePts", "m", i, 1, 1)
    dLong = DataSetReadReal3(PID, hPts, "FeaturePts", "m", i, 2, 1)
    
    If (InStr(feature, "S01") > 0) Then
      dLat = 45.917069264294: dLong = -119.3779585293
    ElseIf (InStr(feature, "S02") > 0) Then
      dLat = 45.918437986474: dLong = -119.3683992576
    ElseIf (InStr(feature, "S03") > 0) Then
      dLat = 45.920055454462: dLong = -119.3589596835
    ElseIf (InStr(feature, "S04") > 0) Then
      dLat = 45.92403022403: dLong = -119.3495249787
    ElseIf (InStr(feature, "S05") > 0) Then
      dLat = 45.926850155793: dLong = -119.3388679356
    ElseIf (InStr(feature, "S06") > 0) Then
      dLat = 45.92238794523: dLong = -119.3753988708
    ElseIf (InStr(feature, "S07") > 0) Then
      dLat = 45.924077408304: dLong = -119.3659876892
    ElseIf (InStr(feature, "S08") > 0) Then
      dLat = 45.926523403184: dLong = -119.357824253
    ElseIf (InStr(feature, "S09") > 0) Then
      dLat = 45.92992044261: dLong = -119.3462770035
    ElseIf (InStr(feature, "S10") > 0) Then
      dLat = 45.931671574347: dLong = -119.3363735263
    ElseIf (InStr(feature, "S11") > 0) Then
      dLat = 45.929114381408: dLong = -119.3302541662
    End If
    
    For t = 0 To 1
      If t = 0 Then
        hList = hChemList
        hConc = hChemSWConc
        ct = ctChem
        prefix = "Chem"
        unit = "mg/L"
      Else
        hList = hRadList
        hConc = hRadSWConc
        ct = ctRad
        prefix = "Rad"
        unit = "Bq/L"
      End If
    
      For j = 1 To ct
        chemical = DataSetReadString1(PID, hList, "Name", "", j)
        threshold = DataSetReadReal2(PID, hEMCriteria, prefix + "ImpactThreshold", unit, j, 1)
        ctTime = DataSetDimensionCount(PID, hConc, "TimePts", SetIdx(i, j))
        ReDim values(ctTime)
        ReDim times(ctTime)
        
        For k = 1 To ctTime
          times(k) = DataSetReadReal3(PID, hConc, "TimePts", "yr", i, j, k)
          values(k) = DataSetReadReal3(PID, hConc, "Conc", unit, i, j, k)
          
          above = values(k) > threshold
          
          On Error Resume Next
          Combo1.Text = CStr(times(k))
          If Err.Number > 0 Then Combo1.AddItem CStr(times(k))
          On Error GoTo 0
          
          ' create event record per source notification format
          varevent = Array("millard", _
              feature, _
              "Water_Sensor", _
              IIf(above, "Above Threshold", "Active_Normal"), _
              IIf(above, rback, ""), _
              "Threshold_Limit", IIf(above, "TRUE", "FALSE"), _
              "Chemical", chemical, _
              "Concentration", CStr(values(k)), _
              "Concent_Units", "mg/L", _
              "Exp_Release_Time", times(k), _
              CStr(dLat), CStr(dLong))
            
          colDatum.Add varevent
        Next k
      Next j
    Next t
  Next i
  loading = False
  Combo1.ListIndex = 0
End Sub

Public Sub WCFSourceNotification()
  Dim i As Long, j As Long, k As Long, t As Long
  Dim cnt As Long
  Dim ct As Long
  Dim ctRad As Long
  Dim ctChem As Long
  Dim ctTime As Long
  Dim ctFeature As Long
  Dim prefix As String
  Dim unit As String
  Dim chemical As String
  Dim feature As String
  Dim threshold As Double
  Dim above As Boolean
  Dim varevent
  Dim hList As Long
  Dim hPts As Long
  Dim hConc As Long
  Dim hRadSWConc As Long
  Dim hChemSWConc As Long
  Dim values()
  Dim times()
  Dim rback As String
  Dim filename As String
  
  hPts = IconGetInputDataSet(PID, stMod, "SurfaceWaterPoints", 1)
  ctFeature = DataSetDimensionCount(PID, hPts, "Feature", SetIdx())
  ctChem = DataSetDimensionCount(PID, hChemList, "CASID", SetIdx())
  ctRad = DataSetDimensionCount(PID, hRadList, "CASID", SetIdx())
  
  On Error Resume Next
  hChemSWConc = IconGetInputDataSet(PID, stMod, "ChemSurfaceWaterDissolvedConc", 1)
  If Err.Number > 0 Then hChemSWConc = 0
  hRadSWConc = IconGetInputDataSet(PID, stMod, "RadSurfaceWaterDissolvedConc", 1)
  If Err.Number > 0 Then hRadSWConc = 0
  
' create reachback xls file link to reachback location
  filename = Dir(argv(argc - 3) & "\V2Temp\~tmp~*.xls")
  While filename <> ""
    rback = rback + txtReachBack.Text & "\" & filename + ";"
    filename = Dir()
  Wend
  On Error GoTo 0
  
  Combo1.Clear
  loading = True
  For i = 1 To ctFeature
    feature = DataSetReadString1(PID, hPts, "Feature", "", i)
    dLat = DataSetReadReal3(PID, hPts, "FeaturePts", "m", i, 1, 1)
    dLong = DataSetReadReal3(PID, hPts, "FeaturePts", "m", i, 2, 1)
    
    If (InStr(feature, "RS1") > 0) Then
      dLat = 45.920527552332:   dLong = -119.3578912459:
    ElseIf (InStr(feature, "RS2") > 0) Then dLat = 45.921498059719:   dLong = -119.3557863827:
    ElseIf (InStr(feature, "RS3") > 0) Then dLat = 45.922400852661:   dLong = -119.3539081452:
    ElseIf (InStr(feature, "RS4") > 0) Then dLat = 45.92347055669:    dLong = -119.3513533598:
    ElseIf (InStr(feature, "RS5") > 0) Then dLat = 45.923657828875:   dLong = -119.3588583707:
    ElseIf (InStr(feature, "RS6") > 0) Then dLat = 45.92469175997:    dLong = -119.3554250877:
    End If
    
    For t = 0 To 1
      If t = 0 Then
        hList = hChemList
        hConc = hChemSWConc
        ct = ctChem
        prefix = "Chem"
        unit = "mg/L"
      Else
        hList = hRadList
        hConc = hRadSWConc
        ct = ctRad
        prefix = "Rad"
        unit = "Bq/L"
      End If
    
      For j = 1 To ct
        chemical = DataSetReadString1(PID, hList, "Name", "", j)
        threshold = DataSetReadReal2(PID, hEMCriteria, prefix + "ImpactThreshold", unit, j, 1)
        ctTime = DataSetDimensionCount(PID, hConc, "TimePts", SetIdx(i, j))
        ReDim values(ctTime)
        ReDim times(ctTime)
        
        For k = 1 To ctTime
          times(k) = DataSetReadReal3(PID, hConc, "TimePts", "yr", i, j, k)
          values(k) = DataSetReadReal3(PID, hConc, "Conc", unit, i, j, k)
          
          above = values(k) > threshold
          
          On Error Resume Next
          Combo1.Text = CStr(times(k))
          If Err.Number > 0 Then Combo1.AddItem CStr(times(k))
          On Error GoTo 0
          
          ' create event record per source notification format
          varevent = Array("millard", _
              feature, _
              "Potential_Source", _
              "Potential_Source", _
              IIf(above, rback & "; " & txtReachBack & "\Source\eplume_RS3.avi", ""), _
              "", "", _
              "Chemical", chemical, _
              "Concentration", CStr(values(k)), _
              "Concent_Units", unit, _
              "Exp_Release_Time", times(k), _
              CStr(dLat), CStr(dLong))
            
          colDatum.Add varevent
        Next k
      Next j
    Next t
  Next i
  loading = False
  Combo1.ListIndex = 0
End Sub

Public Sub AFFSensorNotification()
' air emissions - convert flux to concentratiom
  Dim i As Long, j As Long, k As Long, t As Long, l As Long
  Dim cnt As Long
  Dim ct As Long
  Dim ctRad As Long
  Dim ctChem As Long
  Dim ctTime As Long
  Dim ctFeature As Long
  Dim prefix As String
  Dim unit As String
  Dim chemical As String
  Dim feature As String
  Dim inv As Double
  Dim tot0 As Double
  Dim tot1 As Double
  Dim varevent
  Dim hList As Long
  Dim hSus As Long
  Dim hPts As Long
  Dim hConc As Long
  Dim hEmission As Long
  Dim threshold
  Dim hChemAirEmission
  Dim hRadAirEmission
  Dim times()
  Dim ctLiquids As Long
  Dim ctGases As Long
  Dim ctSolids As Long
  Dim filename As String
  Dim rback As String
  Dim sunit As String
  
  
  hSus = IconGetInputDataSet(PID, stMod, "SuspendedLiquidsDef", 1)
  ctLiquids = DataSetDimensionCount(PID, hSus, "Name", SetIdx())
  hSus = IconGetInputDataSet(PID, stMod, "SuspendedGasesDef", 1)
  ctGases = DataSetDimensionCount(PID, hSus, "Name", SetIdx())
  hSus = IconGetInputDataSet(PID, stMod, "SuspendedSolidsDef", 1)
  ctSolids = DataSetDimensionCount(PID, hSus, "Name", SetIdx())

  hPts = IconGetInputDataSet(PID, stMod, "AirPolygons", 1)
  ctFeature = DataSetDimensionCount(PID, hPts, "Feature", SetIdx())
  ctChem = DataSetDimensionCount(PID, hChemList, "CASID", SetIdx())
  ctRad = DataSetDimensionCount(PID, hRadList, "CASID", SetIdx())

  On Error Resume Next
  hChemAirEmission = IconGetInputDataSet(PID, stMod, "ChemAirEmission", 1)
  If Err.Number > 0 Then hChemAirEmission = 0
  hRadAirEmission = IconGetInputDataSet(PID, stMod, "RadAirEmission", 1)
  If Err.Number > 0 Then hRadAirEmission = 0

' create reachback xls file link to reachback location
  filename = Dir(argv(argc - 3) & "\V2Temp\~tmp~*.xls")
  While filename <> ""
    rback = rback + txtReachBack.Text & "\" & filename + ";"
    filename = Dir()
  Wend
  
  On Error GoTo 0

  Combo1.Clear
  loading = True
  
'  Dim midx As Integer
'  midx = DataSetLookUp(PID, iGeo, "PointID", stMod, SetIdx(0))
'  dLong = DataSetReadReal2(PID, iGeo, "Coordinates", "m", midx, 1)
'  dLat = DataSetReadReal2(PID, iGeo, "Coordinates", "m", midx, 2)
  
  For i = 1 To ctFeature
    feature = DataSetReadString1(PID, hPts, "Feature", "", i)
    dLong = DataSetReadReal3(PID, hPts, "FeaturePts", "m", i, 1, 1)
    dLat = DataSetReadReal3(PID, hPts, "FeaturePts", "m", i, 2, 1)
    For t = 0 To 1
      If t = 0 Then
        hList = hChemList
        hEmission = hChemAirEmission
        ct = ctChem
        prefix = "Chem"
        unit = "g/yr"
        sunit = "g"
      Else
        hList = hRadList
        hEmission = hRadAirEmission
        ct = ctRad
        prefix = "Rad"
        unit = "Bq/yr"
        sunit = "Bq"
      End If

      For j = 1 To ctChem
        chemical = DataSetReadString1(PID, hList, "Name", "", j)
        ctTime = DataSetDimensionCount(PID, hEmission, "TimePts", SetIdx(i, j))

        inv = 0
        ReDim times(ctTime)
        ReDim values(ctTime)
        For k = 1 To ctTime
          times(k) = DataSetReadReal3(PID, hEmission, "TimePts", "yr", i, j, k)
          values(k) = 0
          For l = 1 To ctLiquids
            values(k) = values(k) + DataSetReadReal4(PID, hEmission, "LiquidEmission", unit, i, j, l, k)
          Next l
          For l = 1 To ctSolids
            values(k) = values(k) + DataSetReadReal4(PID, hEmission, "SolidEmission", unit, i, j, l, k)
          Next l
          For l = 1 To ctGases
            values(k) = values(k) + DataSetReadReal4(PID, hEmission, "GasEmission", unit, i, j, l, k)
          Next l

          If k > 1 Then
            inv = inv + (values(k - 1) + values(k)) / 2 * (times(k) - times(k - 1))
          Else
            inv = values(k)
          End If

          If inv > 0 Then
            On Error Resume Next
            Combo1.Text = CStr(times(k))
            If Err.Number > 0 Then Combo1.AddItem CStr(times(k))
            On Error GoTo 0
            
            ' create event record per source notification format
            varevent = Array("millard", _
                stMod, _
                "Potential_Source", _
                "Estimated", _
                rback, _
                "", "", _
                "Chemical", chemical, _
                "Concentration", CStr(inv), _
                "Concent_Units", sunit, _
                "Exp_Release_Time", times(k), _
                CStr(dLat), CStr(dLong))
  
            colDatum.Add varevent
          End If
        Next k
      Next j
    Next t
  Next i
  loading = False
  Combo1.ListIndex = 0
End Sub
