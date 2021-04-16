VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Object = "{F856EC8B-F03C-4515-BDC6-64CBD617566A}#7.0#0"; "FPSPR70.ocx"
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "threed32.ocx"
Begin VB.Form hifSum 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Summary of Risks/Hazard/Dose"
   ClientHeight    =   7200
   ClientLeft      =   45
   ClientTop       =   615
   ClientWidth     =   9600
   Icon            =   "hifSum.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   7200
   ScaleWidth      =   9600
   StartUpPosition =   2  'CenterScreen
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   0
      Top             =   0
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.Frame Frame1 
      Height          =   7050
      Left            =   120
      TabIndex        =   0
      Top             =   -12
      Width           =   9360
      Begin VB.ComboBox cboName 
         Height          =   315
         Left            =   1470
         Style           =   2  'Dropdown List
         TabIndex        =   8
         Top             =   315
         Width           =   3000
      End
      Begin VB.ComboBox cboOrgCancer 
         Height          =   315
         Left            =   6090
         Style           =   2  'Dropdown List
         TabIndex        =   7
         Top             =   720
         Width           =   3000
      End
      Begin VB.ComboBox cboConst 
         Height          =   315
         Left            =   1455
         Style           =   2  'Dropdown List
         TabIndex        =   6
         Top             =   1560
         Width           =   3000
      End
      Begin VB.ComboBox cboExpPt 
         Height          =   315
         Left            =   1470
         Style           =   2  'Dropdown List
         TabIndex        =   5
         Top             =   720
         Width           =   3000
      End
      Begin VB.ComboBox cboTime 
         Height          =   315
         ItemData        =   "hifSum.frx":030A
         Left            =   6090
         List            =   "hifSum.frx":030C
         Style           =   2  'Dropdown List
         TabIndex        =   4
         Top             =   315
         Width           =   3000
      End
      Begin VB.ComboBox cboAge 
         Height          =   315
         Left            =   1470
         Style           =   2  'Dropdown List
         TabIndex        =   3
         Top             =   1140
         Width           =   3000
      End
      Begin VB.ComboBox cboOrgDose 
         Height          =   315
         Left            =   6090
         Style           =   2  'Dropdown List
         TabIndex        =   2
         Top             =   1140
         Width           =   3000
      End
      Begin Threed.SSCheck SSCheck1 
         Height          =   255
         Left            =   210
         TabIndex        =   1
         Top             =   2100
         Width           =   1575
         _Version        =   65536
         _ExtentX        =   2778
         _ExtentY        =   450
         _StockProps     =   78
         Caption         =   "Show Totals Only"
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
      End
      Begin FPSpreadADO.fpSpread vaSpread1 
         Height          =   3432
         Left            =   240
         TabIndex        =   9
         Top             =   2412
         Width           =   8892
         _Version        =   458752
         _ExtentX        =   15684
         _ExtentY        =   6054
         _StockProps     =   64
         ArrowsExitEditMode=   -1  'True
         AutoCalc        =   0   'False
         BackColorStyle  =   1
         ColHeaderDisplay=   0
         DAutoSizeCols   =   1
         DisplayColHeaders=   0   'False
         DisplayRowHeaders=   0   'False
         EditEnterAction =   5
         EditModeReplace =   -1  'True
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         FormulaSync     =   0   'False
         MaxCols         =   100
         RowHeaderDisplay=   0
         RowsFrozen      =   4
         ScrollBarExtMode=   -1  'True
         ScrollBars      =   2
         ScrollBarShowMax=   0   'False
         ShadowColor     =   14737632
         SpreadDesigner  =   "hifSum.frx":030E
         VisibleCols     =   100
         VisibleRows     =   15
      End
      Begin VB.Label Label1 
         AutoSize        =   -1  'True
         Caption         =   "Dataset"
         Height          =   195
         Left            =   210
         TabIndex        =   17
         Top             =   405
         Width           =   1200
         WordWrap        =   -1  'True
      End
      Begin VB.Label Label3 
         AutoSize        =   -1  'True
         Caption         =   "Location"
         Height          =   195
         Left            =   210
         TabIndex        =   16
         Top             =   825
         Width           =   1200
         WordWrap        =   -1  'True
      End
      Begin VB.Label Label4 
         AutoSize        =   -1  'True
         Caption         =   "Constituent"
         Height          =   195
         Left            =   210
         TabIndex        =   15
         Top             =   1665
         Width           =   795
      End
      Begin VB.Label lblOrgCancer 
         Caption         =   "Cancer organ"
         Height          =   195
         Left            =   4710
         TabIndex        =   14
         Top             =   840
         Width           =   1200
         WordWrap        =   -1  'True
      End
      Begin VB.Label Label9 
         Caption         =   "Time Point (yr)"
         Height          =   195
         Left            =   4710
         TabIndex        =   13
         Top             =   435
         Width           =   1200
      End
      Begin VB.Label lblDuration 
         Caption         =   "Exposure Duration: "
         Height          =   255
         Left            =   4725
         TabIndex        =   12
         Top             =   1665
         Width           =   2295
      End
      Begin VB.Label lblOrgDose 
         Caption         =   "Dose organ"
         Height          =   195
         Left            =   4725
         TabIndex        =   11
         Top             =   1245
         Width           =   1200
         WordWrap        =   -1  'True
      End
      Begin VB.Label Label5 
         AutoSize        =   -1  'True
         Caption         =   "Age Group"
         Height          =   195
         Left            =   210
         TabIndex        =   10
         Top             =   1245
         Width           =   765
      End
   End
   Begin VB.Menu mnuPrint 
      Caption         =   "Print"
   End
   Begin VB.Menu mnuSave 
      Caption         =   "Save"
   End
   Begin VB.Menu help 
      Caption         =   "&Help"
      Begin VB.Menu mnuShow 
         Caption         =   "Show Messages"
      End
      Begin VB.Menu howto 
         Caption         =   "How to ..."
      End
      Begin VB.Menu about 
         Caption         =   "&About"
      End
   End
End
Attribute VB_Name = "hifSum"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Private SprWid As Long

Private Sub about_Click()
  frmAbout.picIcon = frmAbout.ImageList1.ListImages(1).Picture
  frmAbout.Show 0
End Sub

Private Sub howto_Click()
  GetHelp
End Sub

Private Sub mnuPrint_Click()
  vaSpread1.PrintUseDataMax = True
  vaSpread1.PrintGrid = False
  vaSpread1.PrintColHeaders = False
  vaSpread1.PrintRowHeaders = False
  vaSpread1.PrintBorder = False
  On Error GoTo PRINTERROR
  vaSpread1.Action = 13
  On Error GoTo 0
PRINTERROR:
End Sub

Private Sub mnuSave_Click()
Dim savecheck As Boolean
  savecheck = True
  On Error GoTo CANCELSAVE
  CommonDialog1.ShowSave
  On Error GoTo 0
  savecheck = vaSpread1.SaveTabFile(CommonDialog1.fileName)
  If Not savecheck Then MsgBox "Unable to save table to " + CommonDialog1.fileName, vbApplicationModal + vbExclamation + vbOKOnly, "Save Error"
CANCELSAVE:
End Sub

Private Sub Form_Unload(Cancel As Integer)
  hifClose
  EndModule
End Sub

Private Sub Form_Load()
Dim i As Long

  SprWid = vaSpread1.width
  hifLoadDatasets
  If AnError Or numDS = 0 Then Unload Me
  cboName.Clear
  For i = 0 To numDS - 1
    cboName.AddItem ds(i).locName & ":" & ds(i).locType
  Next
  updateConstituentList
  cboUpdateConstituent cboConst, True
  cboName.ListIndex = 0
  Set frm = Me
  SetHelpFile App.Path + "\Summary.htm"
  ReadIniString "Options", "ShowViewerMessages", "True", ShowMsg
  If ShowMsg = "true" Then mnuShow.Checked = True
End Sub

Private Sub cboName_Click()
Dim dsIdx As Long

  dsIdx = cboName.ListIndex
  cboUpdateExpPT dsIdx, cboExpPt
  cboUpdateAgeGroup dsIdx, cboAge
  cboUpdateCancerOrgans dsIdx, cboOrgCancer
  cboUpdateDoseOrgans dsIdx, cboOrgDose
  cboAge_Click
End Sub

Private Sub cboExpPt_Click()
  If IgnoreEvents Then Exit Sub
  If cboTime.ListCount > 0 And cboTime.ListIndex > -1 Then cboTime_Click
End Sub

Private Sub cboAge_Click()
  If IgnoreEvents Then Exit Sub
  cboConst_Click
End Sub

Private Sub cboConst_Click()
Dim i As Long
Dim j As Long
Dim ktype As Long
Dim m As Double
Dim n As Double
Dim dsIdx As Long
Dim ageIdx As Long
  
  If IgnoreEvents Then Exit Sub
  dsIdx = cboName.ListIndex
  ageIdx = cboAge.ListIndex
  GetRoutesAndPaths dsIdx, ageIdx, cboConst.Text
  i = GetAllContIdx(cboConst.Text)
  
  If i < 1 Then  ' must find a constituent with time points
    If cboConst.Text = ALLCHEMS Then ktype = 0
    If cboConst.Text = ALLRADS Then ktype = 1
    i = 0
    m = 0
    For i = 1 To UBound(allCAS)
      If allCAS(i).ktype = ktype Then
        If i = 0 Then
          j = i
          m = hifGetTimeCount(dsIdx, ageIdx, allCAS(i).cas)
        Else
          n = hifGetTimeCount(dsIdx, ageIdx, allCAS(i).cas)
          If n > m Then
            m = n
            j = i
          End If
        End If
      End If
    Next
    i = j
  Else
     m = hifGetTimeCount(dsIdx, ageIdx, allCAS(i).cas)
  End If
  
  If m = 0 Then
    MsgBox "No times available for summary!"
  Else
    cboUpdateStartTimes dsIdx, ageIdx, allCAS(i).pcas(0), cboTime
  End If
  
  SetExposureRoutesAndPathways vaSpread1
  IgnoreEvents = True
  If cboOrgCancer.ListCount > 0 Then cboOrgCancer.ListIndex = 0
  If cboOrgDose.ListCount > 0 Then cboOrgDose.ListIndex = 0
  IgnoreEvents = False
  If cboTime.ListCount > 0 And cboTime.ListIndex > -1 Then cboTime_Click
End Sub

Private Sub cboTime_Click()
Dim dsIdx As Long
Dim ageIdx As Long
Dim ptIdx As Long
Dim timeIdx As Long
Dim orgIdx As Long
Dim name As String
Dim i As Long
Dim m As Long
Dim numTime As Long
Dim ktype As Long
Dim row As Long
Dim col As Long
Dim rte As String
Dim Path As String
Dim measure As String
Dim Unit As String
Dim msg As String

Dim risk As Double
Dim var As Variant
Dim rid As Long
Dim tot() As Double
Dim gtot() As Double
Dim xv As Double
Dim yv As Double
Dim xvtemp As Double
Dim yvtemp As Double

  If IgnoreEvents Then Exit Sub
  If 0 = UBound(uExposure) Then Exit Sub
  MousePointer = vbHourglass
  dsIdx = cboName.ListIndex
  ageIdx = cboAge.ListIndex
  ptIdx = cboExpPt.ListIndex
  timeIdx = cboTime.ListIndex
  name = cboConst.Text
  msg = ""
  
  ReDim tot(vaSpread1.MaxCols)
  ReDim gtot(vaSpread1.MaxCols)
  For row = vaSpread1.DataRowCnt To 5 Step -1
    rid = vaSpread1.GetRowItemData(row)
    Select Case rid
      Case 0:
        ReDim tot(vaSpread1.MaxCols)
      Case -1:
        For col = 2 To vaSpread1.MaxCols
          vaSpread1.SetText col, row, Format(tot(col), "0.0##E+00") + Space(2)
        Next col
        ReDim tot(vaSpread1.MaxCols)
      Case -2:
        For col = 2 To vaSpread1.MaxCols
          vaSpread1.SetText col, row, Format(gtot(col), "0.0##E+00") + Space(2)
        Next col
      Case Is > 0:
        rte = uExposure(rid).Route
        Path = uExposure(rid).Path
        For col = 2 To vaSpread1.MaxCols
          risk = 0
          vaSpread1.GetText col, 1, var
          measure = var
          vaSpread1.GetText col, 2, var
          Unit = var
          Select Case measure
            Case "hi":
              orgIdx = 0
            Case "risk"
              orgIdx = cboOrgCancer.ListIndex
              vaSpread1.SetText col, 3, "(" + cboOrgCancer + ")"
            Case "dose":
              orgIdx = cboOrgDose.ListIndex
              vaSpread1.SetText col, 3, "(" + cboOrgDose + ")"
          End Select
          If name = ALLCHEMS Then ktype = 0
          If name = ALLRADS Then ktype = 1
          Select Case name
          Case ALLCHEMS, ALLRADS
            For i = 1 To UBound(allCAS)
              If allCAS(i).ktype = ktype Then
                numTime = hifLoadTimeSeries(dsIdx, ptIdx, ageIdx, orgIdx, allCAS(i).cas, allCAS(i).pcas(0), Path, rte, measure, Unit)
                If numTime > 0 Then
                  hifGetTimeAndValue dsIdx, timeIdx, xv, yv
                  xvtemp = val(cboTime.Text)
                  If xv = xvtemp Then
                    yvtemp = yv
                  Else
                    yvtemp = 0
                    For m = 0 To numTime - 1
                      hifGetTimeAndValue dsIdx, m, xv, yv
                      If xv = xvtemp Then
                        yvtemp = yv
                        Exit For
                      End If
                    Next
                  End If
                Else
                  yvtemp = 0
                End If
                If yvtemp = 0 Then
                  If msg = "" Then
                    msg = allCAS(i).name & ", " & Path & ", " & rte & ", " & measure & ", " & Unit
                  Else
                    msg = msg & vbCrLf & allCAS(i).name & ", " & Path & ", " & rte & ", " & measure & ", " & Unit
                  End If
                End If
                risk = risk + yvtemp
              End If
            Next
          Case Else
            i = GetAllContIdx(name)
            If i > -1 Then
              numTime = hifLoadTimeSeries(dsIdx, ptIdx, ageIdx, orgIdx, allCAS(i).cas, allCAS(i).pcas(0), Path, rte, measure, Unit)
              If numTime > 0 Then
                hifGetTimeAndValue dsIdx, timeIdx, xv, yv
              Else
                yv = 0
              End If
              risk = risk + yv
            End If
          End Select
          
          vaSpread1.SetText col, row, Format(risk, "0.0##E+00") + Space(2)
          tot(col) = tot(col) + risk
          gtot(col) = gtot(col) + risk
        Next
    End Select
  Next
  
  If msg <> "" Then
    Load frmMsg
    If ShowMsg = "True" Then
      frmMsg.lblMsg.caption = "Impacts information at the specified time point was not available for "
      frmMsg.txtMsg.Text = msg
      frmMsg.Show vbModal
    End If
    Unload frmMsg
'    MsgBox "Impacts information at the specified time point was not available for " & msg
  End If
  
  lblDuration = "Exposure duration: " + CStr(hifGetDuration(dsIdx)) + " yr"
  setheader vaSpread1
  MousePointer = vbNormal
End Sub

Private Sub cboOrgCancer_Click()
  If IgnoreEvents Then Exit Sub
  If cboTime.ListCount > 0 And cboTime.ListIndex > -1 Then cboTime_Click
End Sub

Private Sub cboOrgDose_Click()
  If IgnoreEvents Then Exit Sub
  If cboTime.ListCount > 0 And cboTime.ListIndex > -1 Then cboTime_Click
End Sub

Private Sub mnuShow_Click()
  mnuShow.Checked = Not mnuShow.Checked
  ShowMsg = mnuShow.Checked
  WriteIniString "Options", "ShowViewerMessages", ShowMsg
End Sub

Private Sub SSCheck1_Click(value As Integer)
Dim r As Long
Dim rid As Long

  For r = 7 To vaSpread1.MaxRows
    vaSpread1.row = r
    rid = vaSpread1.GetRowItemData(r)
    If rid >= 0 Then vaSpread1.RowHidden = (SSCheck1.value)
  Next r
End Sub
  
Private Sub setheader(spr As fpSpread)
  spr.SetText 1, 1, "Exposure Route and Pathway"
  spr.SetText 1, 2, cboConst.Text & " summation for " & cboName.Text
  spr.SetText 1, 3, "at location " & cboExpPt.Text & " for ages " & cboAge.Text & " at time " & cboTime.Text
End Sub

Private Sub SetExposureRoutesAndPathways(spr As fpSpread)
Dim i As Long
Dim j As Long
Dim rid As Long
Dim rte As String
Dim pw As String
Dim Unit As String
Dim row As Long
Dim col As Long
  
  spr.ReDraw = False
  spr.AutoSize = False
  spr.width = SprWid
  'unhide rows
  For i = 1 To spr.MaxRows
    spr.row = i
    spr.SetRowItemData i, 0
    spr.RowHidden = False
  Next
  'clear the spreadsheet
  spr.MaxCols = 10
  spr.MaxRows = 500
  spr.row = 1
  spr.row2 = spr.MaxRows
  spr.col = 1
  spr.col2 = spr.MaxCols
  spr.BlockMode = True
  spr.Action = 3 ' clear
  spr.BlockMode = False
  setheader spr
  If 0 = UBound(uExposure) Then
    spr.SetText 1, 5, Space(2) + "TOTAL"
    spr.SetText 2, 5, NOT_AVAILABLE
    spr.MaxCols = 2
    spr.MaxRows = 5
    CompleteSpreadsheetLayout spr
    Exit Sub
  End If
  'set unit labels
  lblOrgDose.Visible = False
  cboOrgDose.Visible = False
  lblOrgCancer.Visible = False
  cboOrgCancer.Visible = False
  col = 1
  For i = 1 To UBound(uExposure)
    If 0 = InStr(Unit, uExposure(i).measure + uExposure(i).Unit) Then
      col = col + 1
      spr.TypeHAlign = 2
      spr.SetText col, 1, uExposure(i).measure
      spr.SetText col, 2, uExposure(i).Unit
      
      Select Case uExposure(i).measure
        Case "risk":
          lblOrgCancer.Visible = True
          cboOrgCancer.Visible = True
          spr.SetText col, 3, "(" + cboOrgCancer + ")"
        Case "dose":
          lblOrgDose.Visible = True
          cboOrgDose.Visible = True
          spr.SetText col, 3, "(" + cboOrgDose + ")"
      End Select
      Unit = Unit + uExposure(i).measure + uExposure(i).Unit
    End If
  Next i
  
  row = 5
  spr.SetText 1, row, Space(2) + "TOTAL"
  spr.SetRowItemData row, -2
  
  For i = 1 To UBound(uExposure)
    If 0 = InStr(rte, uExposure(i).Route) Then
      pw = ""
      row = row + 2
      spr.SetText 1, row, Space(2) + uExposure(i).Route + " (total)"
      spr.SetRowItemData row, -1
      For j = 1 To UBound(uExposure)
        If uExposure(i).Route = uExposure(j).Route And 0 = InStr(pw, uExposure(j).Path) Then
          row = row + 1
          spr.SetText 1, row, Space(7) + uExposure(j).Path
          spr.SetRowItemData row, j
          pw = pw + uExposure(j).Path
        End If
      Next j
      rte = rte + uExposure(i).Route
    End If
  Next i
  spr.MaxCols = col
  spr.MaxRows = row
  
  spr.row = 5
  spr.row2 = spr.MaxRows
  spr.col = 2
  spr.col2 = spr.MaxCols
  spr.BlockMode = True
  spr.TypeHAlign = IIf(0 < UBound(uExposure), 1, 2)
  spr.BlockMode = False
  For i = 7 To vaSpread1.MaxRows
    vaSpread1.row = i
    rid = vaSpread1.GetRowItemData(i)
    If rid >= 0 Then vaSpread1.RowHidden = (SSCheck1.value)
  Next i
  CompleteSpreadsheetLayout spr

End Sub

Sub CompleteSpreadsheetLayout(spr As fpSpread)
Dim i As Long
Dim cwid As Long
Dim wid As Long
Dim ht As Long
Dim swid As Single
Dim cw
Dim tw
  
  spr.row = 1
  For i = spr.MaxCols To 1 Step -1
    spr.col = i
    cw = spr.MaxTextColWidth(i) + 5
    spr.ColWidth(i) = cw
    spr.ColWidthToTwips cw, cwid
    tw = tw + cwid
  Next i
  spr.GetClientArea wid, ht
  If wid > tw Then
    spr.TwipsToColWidth wid - (tw - cwid), swid
    spr.ColWidth(1) = swid
  Else
    tw = 0
    spr.ColWidthToTwips spr.ColWidth(1), cwid
    spr.TwipsToColWidth (wid - cwid) / (spr.MaxCols - 1), swid
    For i = 2 To spr.MaxCols
      spr.ColWidth(i) = swid
      tw = tw + swid
    Next i
    spr.ColWidthToTwips tw, cwid
    spr.ColWidthToTwips swid, cwid
    spr.TwipsToColWidth wid - (cwid * (spr.MaxCols - 1)), swid
    spr.ColWidth(1) = CLng(swid)
  End If
  spr.rowHeight(1) = spr.MaxTextRowHeight(1)
  spr.row = 1
  spr.row2 = 3
  spr.col = 1
  spr.col2 = spr.MaxCols
  spr.BlockMode = True
  spr.TypeHAlign = 2  ' center
  spr.BackColor = &HC0FFFF    ' vbYellow
  spr.row = 5
  spr.row2 = 5
  spr.FontBold = True
  spr.BlockMode = False
  spr.AutoSize = True
  spr.ReDraw = True
End Sub
