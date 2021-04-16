VERSION 5.00
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "threed32.ocx"
Begin VB.Form main 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Framework Chart Viewer"
   ClientHeight    =   1725
   ClientLeft      =   4050
   ClientTop       =   3225
   ClientWidth     =   4710
   Icon            =   "xlsChart.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   1725
   ScaleWidth      =   4710
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin Threed.SSPanel SSPanel1 
      Height          =   1380
      Left            =   135
      TabIndex        =   0
      Top             =   180
      Width           =   4470
      _Version        =   65536
      _ExtentX        =   7885
      _ExtentY        =   2434
      _StockProps     =   15
      BackColor       =   12632256
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
   End
End
Attribute VB_Name = "main"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text
Public hLibModule As Long
Public ProcessEvents As Boolean
Public savefile As Boolean

Private Sub Form_Load()
Dim ok As Boolean
Dim strFileName As String
Dim lngCount As Long
Dim i As Long
  
  On Error GoTo ErrorHandler
  Close
  strFileName = String(255, 0)
  lngCount = GetModuleFileName(App.hInstance, strFileName, 255)
  strFileName = Left(strFileName, lngCount)
  
  If Right(strFileName, 7) = "VB6.EXE" Then
    hLibModule = LoadLibrary(App.Path & "\frames.dll")
  End If
  
  ok = False
  savefile = False
  MousePointer = vbHourglass
  GetArguments
  If argv(0) <> "PE" Then
    viewType = "VWR"
    pdcfType = argv(0)
    If argv(1) = "save" Then savefile = True
  Else
    viewType = "PE"
    pdcfType = argv(1)
    If argv(2) = "save" Then savefile = True
  End If
  StartModule Me, "Graphical Viewer", argc
  FUIName = Left(FUIName, InStr(FUIName, ".gid") - 1)
  pdcfName = FUIName & "." & Left(pdcfType, 3)
  
  GetModLabelAndDes
  SSPanel1.caption = "Viewing File: " & pdcfName & Chr(10) & "Section: " & modName
  Visible = True
  
  Select Case viewType
    Case "VWR"
      Select Case pdcfType
        Case "suf":     ok = sufChart(pdcfName, modName)
        Case "suf2":    ok = sufChart2(pdcfName, modName)
        Case "exf":     ok = exfChart(pdcfName, modName)
        Case "hqf1"
          Me.Hide
          hqfSum.Show
        Case "hqf2"
          Me.Hide
          hqfSum.Show
        Case "scf", "aff", "ato", "wff", "wcf", "bbf", "epf", "rif", "hif", "hqf", "sufwcf", "sufscf", "mrkwcf", "mrkscf"
          If pdcfType = "sufwcf" Then
            For i = 1 To UBound(modSrcId)
              If modSrc(i) = "wcf" Then
                modName = modSrcId(i)
                modLabel = modSrcLbl(i)
              End If
              If modSrc(i) = "suf" Then
                sufName = modSrcId(i)
              End If
            Next
          End If
          If pdcfType = "sufscf" Then
            For i = 1 To UBound(modSrcId)
              If modSrc(i) = "scf" Then
                modName = modSrcId(i)
                modLabel = modSrcLbl(i)
              End If
              If modSrc(i) = "suf" Then
                sufName = modSrcId(i)
              End If
            Next
          End If
          Me.Hide
          frmSeries.Show
        Case "hif1"
          pdcfType = Left(pdcfType, 3)
          Me.Hide
          hifORIA.caption = "Health Impacts - Summary at One Time Point - Option 1"
          hifORIA.Show
          hifORIA.optTable(0).value = True
        Case "hif2"
          pdcfType = Left(pdcfType, 3)
          Me.Hide
          hifORIA.caption = "Health Impacts - Summary at One Time Point - Option 2"
          hifORIA.Show
          hifORIA.optTable(1).value = True
        Case "hif3"
          pdcfType = Left(pdcfType, 3)
          Me.Hide
          hifORIA.caption = "Health Impacts - Summary at One Time Point - Option 3"
          hifORIA.Show
          hifORIA.optTable(2).value = True
        Case "hif4"
          pdcfType = Left(pdcfType, 3)
          Me.Hide
          hifORIA.caption = "Maximum Individual Dose/Risk/Hazard"
          hifORIA.Show
          hifORIA.optTable(3).value = True
        Case "hif5"
          pdcfType = Left(pdcfType, 3)
          Me.Hide
          hifSum.Show
      End Select
    Case "PE"
      Select Case pdcfType
        Case "scf", "aff", "ato", "wff", "wcf", "bbf", "epf", "rif", "hif", "hqf"
          Me.Hide
          frmSeries.Show
      End Select
  End Select
  
  
ErrorHandler:
  If Err.Number <> 0 Then
    ReportError task, "main form load"
    Close
    End
  End If
End Sub

Private Sub Form_Unload(Cancel As Integer)
  
  If savefile Then
    On Error Resume Next
    wbo.DisplayAlerts = False
    wso.Saveas FUIName & "." & modName & ".xls", 1
    wbo.DisplayAlerts = True
  End If
  
  Set wbo = Nothing
  Set cho = Nothing
  If 0 <> hLibModule Then FreeLibrary hLibModule
 
  EndModule
End Sub
