VERSION 5.00
Object = "{FE0065C0-1B7B-11CF-9D53-00AA003C9CB6}#1.1#0"; "comct232.ocx"
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Object = "{F856EC8B-F03C-4515-BDC6-64CBD617566A}#7.0#0"; "FPSPR70.ocx"
Begin VB.Form hqfSum 
   Caption         =   "Form1"
   ClientHeight    =   7275
   ClientLeft      =   165
   ClientTop       =   735
   ClientWidth     =   10230
   Icon            =   "hqfSum.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   7275
   ScaleWidth      =   10230
   StartUpPosition =   3  'Windows Default
   Begin VB.Frame Frame2 
      Height          =   7140
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   10092
      Begin VB.TextBox txtCount 
         Alignment       =   2  'Center
         Enabled         =   0   'False
         Height          =   285
         Left            =   2415
         TabIndex        =   1
         Text            =   "0"
         Top             =   420
         Width           =   420
      End
      Begin ComCtl2.UpDown UpDown1 
         Height          =   285
         Left            =   2835
         TabIndex        =   2
         Top             =   420
         Width           =   240
         _ExtentX        =   423
         _ExtentY        =   503
         _Version        =   327681
         Value           =   1
         OrigLeft        =   3720
         OrigTop         =   120
         OrigRight       =   3960
         OrigBottom      =   495
         Max             =   25
         Min             =   1
         SyncBuddy       =   -1  'True
         BuddyProperty   =   0
         Enabled         =   -1  'True
      End
      Begin FPSpreadADO.fpSpread vaSpread1 
         Height          =   6000
         Left            =   240
         TabIndex        =   3
         Top             =   912
         Width           =   8820
         _Version        =   458752
         _ExtentX        =   15558
         _ExtentY        =   10583
         _StockProps     =   64
         ArrowsExitEditMode=   -1  'True
         AutoCalc        =   0   'False
         BackColorStyle  =   1
         ColHeaderDisplay=   0
         EditEnterAction =   5
         EditModeReplace =   -1  'True
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         FormulaSync     =   0   'False
         MaxCols         =   8
         ScrollBarExtMode=   -1  'True
         ScrollBarShowMax=   0   'False
         SpreadDesigner  =   "hqfSum.frx":030A
      End
      Begin VB.Label Label3 
         AutoSize        =   -1  'True
         Caption         =   "Number of criteria (max 25):"
         Height          =   195
         Left            =   210
         TabIndex        =   4
         Top             =   420
         Width           =   1935
      End
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   0
      Top             =   0
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
      CancelError     =   -1  'True
      DefaultExt      =   ".txt"
      DialogTitle     =   "Save Table to File"
      Filter          =   "Tab Delimited Text File (*.txt) | *.txt"
   End
   Begin VB.Menu mnuPrint 
      Caption         =   "Print"
   End
   Begin VB.Menu mnusave 
      Caption         =   "Save"
   End
   Begin VB.Menu help 
      Caption         =   "&Help"
      Begin VB.Menu howto 
         Caption         =   "How to..."
      End
      Begin VB.Menu about 
         Caption         =   "&About"
      End
   End
End
Attribute VB_Name = "hqfSum"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Private IgnoreEvents As Boolean

Private Sub about_Click()
  frmAbout.picIcon = frmAbout.ImageList1.ListImages(1).Picture
  frmAbout.Show 0
End Sub

Private Sub Form_Load()
  On Error GoTo ErrorHandler
  IgnoreEvents = True
  
  ' miscellaneous initializations
  Set activeForm = Me
  Set activeSpread = vaSpread1
  
  txtCount = 1
  Set frm = Me
  caption = " - Time Series"
  SetHelpFile App.Path + "\Graphical.htm"

  IgnoreEvents = False
  
  Select Case pdcfType
    Case "hqf1"
      caption = "Terrestrial Ecological Effects and Hazard Quotient" & caption
      hqfLoadDatasets
      If AnError Or numDS = 0 Then Unload Me
      InitSelect 1, 6
      vaSpread1_Change 2, CBO_OR
    Case "hqf2"
      caption = "Aquatic Ecological Effects and Hazard Quotient" & caption
      hqfLoadDatasets
      If AnError Or numDS = 0 Then Unload Me
      InitSelect 1, 8
      vaSpread1_Change 2, CBO_DS
    Case Else:
      PutError "Unknown file type -> " & pdcfType
  End Select

ErrorHandler:
  If Err.Number <> 0 Then
    PutError Err.Description & " from frmSeries_Load"
    Unload Me
  End If
  Form_Resize
End Sub

Private Sub Form_Unload(Cancel As Integer)
  On Error Resume Next
  Select Case pdcfType
    Case "hqf1", "hqf2":    exfClose
  End Select

  EndModule
End Sub

Private Sub Form_Resize()
  If IgnoreEvents Then Exit Sub
  If Me.ScaleWidth = 0 Or Me.ScaleHeight = 0 Then Exit Sub
  If Frame2.Visible Then
    If 50 < Me.ScaleHeight Then
      Frame2.Height = Me.ScaleHeight
    End If
    If 50 < Me.ScaleWidth - 2 * Frame2.Left Then
      Frame2.width = Me.ScaleWidth - 2 * Frame2.Left
    End If
    If 50 < Frame2.Height - 1.2 * vaSpread1.Top Then
      vaSpread1.Height = Frame2.Height - 1.2 * vaSpread1.Top
    End If
    If 50 < Frame2.width - 2 * vaSpread1.Left Then
      vaSpread1.width = Frame2.width - 2 * vaSpread1.Left
    End If
  End If
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

Private Sub UpdateSpread(ByVal col As Long, ByVal row As Long, change As Boolean)
  Dim toprow As Long
  Dim retcode As Boolean
  
  On Error GoTo ErrorHandler
  
  If Not vaSpread1.Enabled Then Exit Sub
  toprow = vaSpread1.toprow
  activeForm.MousePointer = vbHourglass
  vaSpread1.ReDraw = False
  vaSpread1.Enabled = False
  
  If change Then
    Select Case pdcfType
      'Sheetchange calls must change the
      'series xValues, yvalues and set labels
      Case "hqf1": retcode = hqfSheetChange(col, row)
      Case "hqf2": retcode = hqf2SheetChange(col, row)
    End Select
  End If
  
  If toprow <= vaSpread1.DataRowCnt Then vaSpread1.toprow = toprow
  vaSpread1.Enabled = True
  vaSpread1.ReDraw = True
  activeForm.MousePointer = vbDefault

ErrorHandler:
  If Err.Number <> 0 Then ReportError task, "UpadteSpread"
End Sub

Private Sub vaSpread1_Change(ByVal col As Long, ByVal row As Long)
  If Not vaSpread1.Enabled Then Exit Sub
  MousePointer = vbHourglass
  UpdateSpread col, row, True
  MousePointer = vbNormal
End Sub

Private Sub txtCount_Change()
Dim c As Long
Dim width As Long
  
  If Not activeSpread.Enabled Then Exit Sub
  Select Case pdcfType
  Case "hqf1":   width = 6
  Case "hqf2":   width = 10
  End Select
  
  numCol = activeSpread.MaxCols / width
  activeSpread.MaxCols = width * txtCount
  If numCol < txtCount Then
    For c = numCol + 1 To txtCount
      InitSelect c, width
      vaSpread1_Change (c * width) - width + 2, 1
    Next
  End If
  numCol = txtCount
End Sub

Sub InitSelect(col As Long, width As Long)
  Dim i As Long
  Dim c1 As Long
  Dim c2 As Long
  Dim cbolist As String
  
  On Error GoTo ErrorHandler:
  
  Select Case pdcfType
    Case "hqf1":
      CBO_OR = 1
      CBO_CHM = 2
      CBO_JRS = 3
      CBO_TME = 4
      numSel = CBO_TME
    Case "hqf2":
      CBO_DS = 1
      CBO_OR = 2
      CBO_CHM = 3
      CBO_TME = 4
      numSel = CBO_TME
  End Select
  
  'Set col1 and col2
  c2 = (width * col) - (width - 2)
  c1 = c2 - 1
  'Set row labels
  If CBO_DS > 0 Then activeSpread.SetText c1, CBO_DS, "Location"
  If CBO_OR > 0 Then activeSpread.SetText c1, CBO_OR, "Organism"
  If CBO_CHM > 0 Then activeSpread.SetText c1, CBO_CHM, "Constituent"
  If CBO_JRS > 0 Then activeSpread.SetText c1, CBO_JRS, "Jurisdiction"
  If CBO_TME > 0 Then activeSpread.SetText c1, CBO_TME, "Time Point"
  
  'Set column labels
  activeSpread.SetText c1, 0, "Criteria"
  activeSpread.SetText c2, 0, "Selection #" & col
  'Set cells to selection boxes
  activeSpread.col = c2
  For i = 1 To numSel
    activeSpread.row = i
    activeSpread.CellType = 8
  Next
  'set scroll lock on series
  activeSpread.RowsFrozen = numSel + 2
  'update the allCAS constituent list
  updateConstituentList
  
  Select Case pdcfType
    Case "hqf1":
      updateOrganisms c2
    Case "hqf2":
      cbolist = ""
      For i = 0 To numDS - 1
        cbolist = cbolist + IIf(cbolist <> "", Chr(9), "") + _
                            IIf(ds(i).locName <> "all", ds(i).locName, "") + _
                            IIf(ds(i).locName <> "" And ds(i).locName <> "all", ":", "") + ds(i).locType
      Next
      SetSelList CBO_DS, c2, cbolist
  End Select
  'Set column widths
  activeSpread.ColWidth(c1) = activeSpread.MaxTextColWidth(c1)
  activeSpread.ColWidth(c2) = activeSpread.MaxTextColWidth(c2)
  

    ' If none of the cells are in a span, create a new span
  activeSpread.BlockMode = False
  For i = 0 To numSel
    activeSpread.AddCellSpan c2, i, c2 + width, 1
  Next
    
ErrorHandler:
  If Err.Number <> 0 Then PutError Err.Description & " in InitSelect"
End Sub

Private Sub Updown1_Downclick()
  If Not vaSpread1.Enabled Then Exit Sub
  txtCount.Text = UpDown1.value
End Sub

Private Sub Updown1_Upclick()
  If Not vaSpread1.Enabled Then Exit Sub
  txtCount.Text = UpDown1.value
End Sub

